%% Author: skruger
%% Created: Dec 2, 2010
%% Description: TODO: Add description to proxy_read_response
-module(proxy_read_response).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([start/2,send_headers/2,read_response/2,read_chunked_response/2,get_next/1]).

-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state,{parent,sock,headers,buff,size,bytes_sent,request,logdebug,encoding}).

-define(LOGDEBUG(D,CMD) , if D -> CMD ; true -> ok end ).

%%% This module takes ownership of the response stream and receives headers and a response body.  
%%% It emits a message containing headers then a series of messages containing the response body.
%%% It will report a size when possible and always emit a final message indicating that the transfer is complete.

% Messages
%
% {response_header,#header_block{},ResponseSize}
% ResponseSize = int() | chunked
%
% {response_data,Data}
% Data = binary()
%
% {end_response_data,ByteLength}
% ByteLength = int()

%%
%% API Functions
%%

start(Sock,Request) ->
	ParentPid = self(),
%% 	?DEBUG_MSG("~p started by: ~p~n",[?MODULE,ParentPid]),
	try
		Hdr = header_parse:decode_headers(Sock),
		case gen_fsm:start(?MODULE,[#state{parent=ParentPid,sock=Sock,headers=Hdr,buff=Hdr#header_block.body,request=Request}],[]) of
			{ok,RPid} ->
				gen_socket:controlling_process(Sock,RPid),
				{?MODULE,RPid};
			Err ->
				Err
		end
	catch
		_:CErr ->
			?ERROR_MSG("Error in ~p:start(): ~p~n~p~n",[?MODULE,CErr,Request]),
			CErr
	end.

get_next({?MODULE,Pid}) ->
%% 	?DEBUG_MSG("Requesting next: ~p~n",[self()]),
	gen_fsm:send_event(Pid,check_data).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------

init([State]) ->
	erlang:monitor(process,State#state.parent),
%% 	?DEBUG_MSG("Staring: ~p~n",[(State#state.request)#header_block.request]),
	
	gen_fsm:send_event(self(),run),
	{NextState,State1} =
	case proxylib:method_has_data(State#state.request,State#state.headers) of
		true ->
			Dict = dict:from_list((State#state.headers)#header_block.headers),
			case dict:find('Transfer-Encoding',Dict) of
				{ok,"chunked"} ->
					{send_headers,State#state{size=chunked,bytes_sent=0}};
				NoChunk ->
					case dict:find('Connection',Dict) of
						{ok,"close"} ->
							{send_headers,State#state{size=close,bytes_sent=0}};
						NoConn ->
							case dict:find('Content-Length',Dict) of
								{ok,LenStr} ->
									{Len,_} = string:to_integer(LenStr),
									{send_headers,State#state{size=Len,bytes_sent=0}};
								NoLen ->
									?ERROR_MSG("No valid size headers received from server!  (Non compliant HTTP host)~n~p~nchunked encoding: ~p~nConnection: close: ~p~nContent-length: ~p~n~p~n",
											   [(State#state.request)#header_block.rstr,NoChunk,NoConn,NoLen,(State#state.headers)#header_block.headers]),
									Len = size(State#state.buff),
									Hdr = (State#state.headers)#header_block{headers= (State#state.headers)#header_block.headers++[{'Connection',"close"}]},
									{send_headers,State#state{headers=Hdr,size=close,bytes_sent=0}}
%% 									{stop,error}
							end
					end	
			end;
		_ ->
			{send_headers,State#state{size=0,bytes_sent=0}}
	end,
	Dict0 = dict:from_list((State1#state.headers)#header_block.headers),
	HBlock0 = (State1#state.headers),
	{Encoding,Headers} =
		case dict:find('Content-Encoding',Dict0) of
			{ok,ContentEnc} when (ContentEnc == "gzip") or (ContentEnc == "x-gzip") ->
				case State1#state.size of
					Vary when (Vary == close) or (Vary == chunked )->
						GZHdr = proxylib:remove_headers(['Content-Encoding','Content-Length'],HBlock0#header_block.headers),
						{gzip,GZHdr};
					ResLen when is_integer(ResLen) and ((HBlock0#header_block.response)#response_rec.protocol == {1,1}) ->
 						GZHdr = proxylib:remove_headers(['Content-Encoding','Content-Length','Connection','Transfer-Encoding'],HBlock0#header_block.headers),
						{gzip,GZHdr++[{'Transfer-Encoding',"chunked"}]};
					ResLen when is_integer(ResLen) and ((HBlock0#header_block.response)#response_rec.protocol == {1,0}) ->
 						GZHdr = proxylib:remove_headers(['Content-Encoding','Content-Length','Connection','Transfer-Encoding'],HBlock0#header_block.headers),
						{gzip,GZHdr++[{'Connection',"close"}]}
				end;
			{ok,Other} ->
				?WARN_MSG("Unsupported content-encoding: ~p~n",[Other]),
				{plain,HBlock0#header_block.headers};
			_EncInfo ->
%% 				?DEBUG_MSG("No encoding. ~p~n",[EncInfo]),
				{plain,HBlock0#header_block.headers}
		end,
%% 	?DEBUG_MSG("Started with: ~p~n~nEnded with:~p~n",[HBlock0#header_block.headers,Headers]),
	HBlock = HBlock0#header_block{headers = Headers},
	StateOut = State1#state{encoding=Encoding,headers=HBlock},
	case NextState of
		error ->
			{error,StateOut};
		_ ->
			{ok,NextState,StateOut}
	end.

send_headers(run,State) ->
%% 	?DEBUG_MSG("Sent headers to ~p~n",[State#state.parent]),
	Protocol = ((State#state.headers)#header_block.response)#response_rec.protocol,
	ResponseSize = case State#state.encoding of
			   gzip when (Protocol == {1,0}) or (State#state.size == close) -> close;
			   gzip when (Protocol == {1,1}) -> chunked;
			   _ -> State#state.size
		   end,
	State#state.parent ! {response_header,State#state.headers,ResponseSize},
	case State#state.size of
		chunked ->
 			?LOGDEBUG(State#state.logdebug,?DEBUG_MSG("Starting in chunked mode.",[])),
			{next_state,read_chunked_response,State};
		close ->
			{next_state,read_response,State};
		0 ->
			{next_state,read_response,State};
		Size when Size > 0 -> 
%% 			?DEBUG_MSG("Starting with known size: ~p~n",[Size]),
			{next_state,read_response,State};
		Other ->
			?ERROR_MSG("Invalid size: ~p",[Other]),
			{stop,normal,State}
	end.
		

read_response(check_data,State) when State#state.bytes_sent >= State#state.size ->
	State#state.parent ! {end_response_data,State#state.bytes_sent},
	gen_socket:controlling_process(State#state.sock,State#state.parent),
	{stop,normal,State};
read_response(check_data,State) when State#state.buff == <<>> ->
%% 	?DEBUG_MSG("read_response() with no data requesting more: ~p ~p~n",[State#state.bytes_sent,State#state.size]),
	case gen_socket:recv(State#state.sock,0) of
		{ok,Data} ->
			send_response_data(State,Data),
			Sent = trunc(bit_size(Data)/8) + State#state.bytes_sent,
			{next_state,read_response,State#state{bytes_sent=Sent}};
		{error,closed} ->
			State#state.parent ! {end_response_data,State#state.bytes_sent},
			{stop,normal,State};
		Err ->
 			?ERROR_MSG("Receive error: ~p~n",[Err]),
			State#state.parent ! {end_response_data,State#state.bytes_sent},
			{stop,normal,State}
	end;
read_response(check_data,State) ->
%% 	?DEBUG_MSG("Sending existing data.~n",[]),
	send_response_data(State,State#state.buff),
	Sent = trunc(bit_size(State#state.buff)/8) + State#state.bytes_sent,
	{next_state,read_response,State#state{buff = <<>>,bytes_sent=Sent}};
read_response({gen_socket,_,<<Data>>},State) ->
	gen_fsm:send_event(self(),check_data),
	<<OldBuff/binary>> = State#state.buff,
	{next_state,read_response,State#state{buff = <<OldBuff/binary,Data/binary>>}}.


read_chunked_response(check_data,State) when State#state.buff == <<>> ->
	case gen_socket:recv(State#state.sock,0) of
		{ok,Data} ->
			gen_fsm:send_event(self(),check_data),
			{next_state,read_chunked_response,State#state{buff=Data}};
		Err ->
			?ERROR_MSG("Receive error: ~p~n",[Err]),
			{stop,normal,State}
	end;
read_chunked_response(check_data,State) ->
 	?LOGDEBUG(State#state.logdebug,?DEBUG_MSG("read_chunked_response(check_data,) with buff: ~p~n",[State#state.buff])),
	case proxylib:find_binary_pattern(State#state.buff,<<"\r\n">>) of
		nomatch ->
			case gen_socket:recv(State#state.sock,0) of
				{ok,Data} ->
 					?LOGDEBUG(State#state.logdebug,?DEBUG_MSG("Received more after \"nomatch\"~n",[])),
					gen_fsm:send_event(self(),check_data),
					Buff = State#state.buff,
					{next_state,read_chunked_response,State#state{buff= <<Buff/binary,Data/binary>>}};
				Err ->
					?ERROR_MSG("Receive error: ~p~n",[Err]),
					{stop,normal,State}
			end;
		0 ->
			<<"\r\n",Rest/binary>> = State#state.buff,
			gen_fsm:send_event(self(),check_data),
			{next_state,read_chunked_response,State#state{buff=Rest}};
		Idx when Idx < 10 ->
			<<LenStr:Idx/binary,"\r\n",Chunk/binary>> = State#state.buff,
			Len = erlang:list_to_integer(string:strip(binary_to_list(LenStr),both,($ ) ),16),
%% 			?DEBUG_MSG("Index found: ~p ~p(hex) ~p",[Idx,LenStr,Len]),
			case trunc(bit_size(Chunk)/8) of
				_ when Len == 0 ->
					% Last chunk
					State#state.parent ! {end_response_data,State#state.bytes_sent},
					gen_socket:controlling_process(State#state.sock,State#state.parent),
					{stop,normal,State};
				CLen when CLen >= Len ->
					% complete chunk
					<<SendChunk:Len/binary,R/binary>> = Chunk,
					BytesSent = CLen + State#state.bytes_sent,
					send_response_data(State,SendChunk),
 					?LOGDEBUG(State#state.logdebug,?DEBUG_MSG("Got chunk:~n~p~n(~p)~n Leftover:~n~p~n",[SendChunk,trunc(bit_size(SendChunk)/8),R])),
					{next_state,read_chunked_response,State#state{buff=R,bytes_sent=BytesSent}};
				_ ->
					case gen_socket:recv(State#state.sock,0) of
						{ok,Data} ->
%% 							?DEBUG_MSG("Received more after \"nomatch\"~n",[]),
							gen_fsm:send_event(self(),check_data),
							Buff = State#state.buff,
							{next_state,read_chunked_response,State#state{buff= <<Buff/binary,Data/binary>>}};
						Err ->
							?ERROR_MSG("Receive error: ~p~n",[Err]),
							{stop,normal,State}
					end
			end;
		Idx ->
			?ERROR_MSG("Idx value too big or other error (~p).  Can't be a valid chunk!~n",[Idx]),
			{stop,normal,State}
	end;
read_chunked_response({gen_socket,_,<<Data>>},State) ->
	gen_fsm:send_event(self(),check_data),
	<<OldBuff/binary>> = State#state.buff,
	{next_state,read_chunked_response,State#state{buff = <<OldBuff/binary,Data/binary>>}}.
		

send_response_data(State,Data) ->
	case State#state.encoding of
		gzip ->
%% 			?DEBUG_MSG("Sending gzip_response_data~n",[]),
			State#state.parent ! {gzip_response_data,Data};
		_ ->
			State#state.parent ! {response_data,Data}
	end.
	
%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info({gen_socket,_,_}=Data, StateName, StateData) ->
	?DEBUG_MSG("Got data: ~p~n",[Data]),
	gen_fsm:send_event(self(),Data),
    {next_state, StateName, StateData};
handle_info({'DOWN',_,process,_Pid,_},_StateName,State) ->
%% 	?DEBUG_MSG("Stopping ~p because proxy_pass went away.~n",[?MODULE]),
	{stop,normal,State};
handle_info(Other,StateName,StateData) ->
	?INFO_MSG("Got unexpected info in state: ~p~n~p~n",[StateName,Other]),
	{next_state,StateName,StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
