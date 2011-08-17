%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 6, 2010
%%% -------------------------------------------------------------------
-module(proxy_read_request).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([get_next/1, start/1,stop/1]).


%% gen_fsm callbacks
-export([send_headers/2,read_request/2]).
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state,{parent,sock,headers,buff,size,bytes_sent,logdebug}).

% Messages
%
% {request_header,#header_block{},requestSize}
% requestSize = int() | 0
%
% {request_data,Data}
% Data = binary()
%
% {end_request_data,ByteLength}
% ByteLength = int() | 0


%% ====================================================================
%% External functions
%% ====================================================================

start(Sock) ->
	ParentPid = self(),
%% 	?DEBUG_MSG("~p started by: ~p~n",[?MODULE,ParentPid]),
%% 	Hdrx = header_parse:decode_headers(Sock),
%% 	Hdrx = header_parse:get_headers(Sock,request),
%% 	?ERROR_MSG("Headers: ~n~p~n",[Hdrx]).

	%% 	Hdr = header_parse:get_headers(Sock,request),
	Hdr = header_parse:decode_headers(Sock),
	case (Hdr#header_block.request)#request_rec.method of
		"CONNECT" ->
			ParentPid ! {request_header,Hdr,0},
			http_connect;
		_ ->
			case gen_fsm:start(?MODULE,[#state{parent=ParentPid,sock=Sock,headers=Hdr,buff=Hdr#header_block.body}],[]) of
				{ok,RPid} ->
					gen_socket:controlling_process(Sock,RPid),
					gen_fsm:send_event(RPid,run),
					{?MODULE,RPid};
				Err ->
					Err
			end
	end.

get_next({?MODULE,Pid}) ->
%% 	?DEBUG_MSG("Requesting next: ~p~n",[self()]),
	gen_fsm:send_event(Pid,check_data).

stop({?MODULE,Pid}) ->
	gen_fsm:sync_send_all_state_event(Pid,stop).

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
%% 	?DEBUG_MSG("Staring: ~p~n",[(State#state.request)#header_block.request]),
	Dict = dict:from_list((State#state.headers)#header_block.headers),
	Expect = case dict:find("Expect",Dict) of
				 {ok,V} ->
					 list_to_atom(V);
				 _ -> undefined
			 end,
	Headers = (State#state.headers)#header_block{expect = Expect},
	case dict:find('Content-Length',Dict) of
		{ok,LenStr} ->
			{Len,_} = string:to_integer(LenStr),
			{ok,send_headers,State#state{size=Len,bytes_sent=0,headers=Headers}};
		_ ->
			{ok,send_headers,State#state{size=0,bytes_sent=0,headers=Headers}}
	end.

send_headers(run,State) ->
	HdrData = {request_header,State#state.headers,State#state.size},
	State#state.parent ! HdrData,
%% 	?DEBUG_MSG("Sent headers to ~p~n~p~n",[State#state.parent,HdrData]),
	case State#state.size of
		Size when Size >= 0 ->
			{next_state,read_request,State};
		Other ->
			?ERROR_MSG("Invalid size: ~p",[Other]),
			gen_socket:controlling_process(State#state.sock,State#state.parent),
			{stop,normal,State}
	end.

read_request(check_data,State) when State#state.bytes_sent >= State#state.size ->
	State#state.parent ! {end_request_data,State#state.bytes_sent},
	gen_socket:controlling_process(State#state.sock,State#state.parent),
	{stop,normal,State};
read_request(check_data,State) when State#state.buff == <<>> ->
%% 	?DEBUG_MSG("read_request() with no data requesting more: ~p ~p~n",[State#state.bytes_sent,State#state.size]),
	case gen_socket:recv(State#state.sock,0) of
		{ok,Data} ->
			State#state.parent ! {request_data,Data},
			Sent = trunc(bit_size(Data)/8) + State#state.bytes_sent,
			{next_state,read_request,State#state{bytes_sent=Sent}};
		Err ->
			?ERROR_MSG("Receive error: ~p~n",[Err]),
			{stop,normal,State}
	end;
read_request(check_data,State) ->
%% 	?DEBUG_MSG("Sending existing data.~n",[]),
	State#state.parent ! {request_data,State#state.buff},
	Sent = trunc(bit_size(State#state.buff)/8) + State#state.bytes_sent,
	{next_state,read_request,State#state{buff = <<>>,bytes_sent=Sent}};
read_request({gen_socket,_,<<Data>>},State) ->
	gen_fsm:send_event(self(),check_data),
	<<OldBuff/binary>> = State#state.buff,
	{next_state,read_request,State#state{buff = <<OldBuff/binary,Data/binary>>}}.



%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
state_name(_Event, StateData) ->
    {next_state, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(_Event, _From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(stop,_StateName,State) ->
	{stop,normal,State};
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
handle_sync_event(stop,_From,StateName,State) ->
	gen_socket:controlling_process(State#state.sock,State#state.parent),
	gen_fsm:send_all_state_event(self(),stop),
	{reply,ok,StateName,State};
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
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

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

