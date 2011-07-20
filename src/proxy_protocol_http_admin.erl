%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Jul 18, 2011
%%% -------------------------------------------------------------------
-module(proxy_protocol_http_admin).

-behaviour(proxy_protocol).
-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([handle_protocol/1]).

%% gen_fsm callbacks
-export([init/1, start_request/2, parse_request/2,send_response/2,handle_request/2,
	 handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {listener,client_sock,request,headers,auth,body_length,body}).

%% ====================================================================
%% External functions
%% ====================================================================

handle_protocol(PListener) ->
	CSock = PListener#proxy_listener.client_sock,
	{ok,Pid} = gen_fsm:start(?MODULE,#state{listener=PListener,client_sock=CSock},[]),
	gen_socket:controlling_process(CSock, Pid),
	gen_fsm:send_event(Pid,read),
	ok.

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
init(State) ->
    {ok, start_request, State}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
start_request(read, #state{client_sock=CSock}=StateData) ->
	case gen_socket:recv(CSock,0,2000) of
		{ok,Data} ->
			gen_fsm:send_event(self(),{data,Data}),
			{next_state, parse_request, StateData};
		Err ->
			?ERROR_MSG("Error in start_request(): ~p~n",[Err]),
			{stop,normal,StateData}
	end.

parse_request({data,Data},StateData) ->
	case erlang:decode_packet(http,Data,[]) of
		{ok,Packet,Rest} ->
			gen_fsm:send_event(self(),{header,Rest}),
			{next_state, parse_request, StateData#state{request=Packet,headers=[],body_length=0,body= <<>>}};
		Err ->
			?ERROR_MSG("Error in start_request(): ~p~n",[Err]),
			{stop,normal,StateData}
	end;
parse_request({header,Data},#state{headers=HeaderList}=StateData) ->
	case erlang:decode_packet(httph,Data,[]) of
		{ok,http_eoh,Rest} ->
			gen_fsm:send_event(self(),{body,Rest}),
			{next_state,parse_request,StateData};
		{ok,{http_header,_,'Authorization',_,"Basic "++AuthInfo},Rest} ->
			UserPassBin = base64:decode(AuthInfo),
			[User,Pass|_] = string:tokens(binary_to_list(UserPassBin),":"),
			gen_fsm:send_event(self(),{header,Rest}),
			{next_state, parse_request,StateData#state{auth={User,Pass}}};
		{ok,{http_header,_,'Content-Length',_,StrLen},Rest} ->
			gen_fsm:send_event(self(),{header,Rest}),
			{next_state, parse_request,StateData#state{body_length=list_to_integer(StrLen)}};
		{ok,{http_header,_,Header,_,Value},Rest} ->
%% 			?ERROR_MSG("Got header: ~p ~p~n",[Header,Value]),
			gen_fsm:send_event(self(),{header,Rest}),
			NewHeaders = [{Header,Value}|HeaderList],
			{next_state, parse_request, StateData#state{headers=NewHeaders}};
		Err ->
			?ERROR_MSG("Error in start_request(): ~p~n",[Err]),
			{stop,normal,StateData}
	end;
parse_request({body,_Data},#state{body_length=0}=State) ->
	gen_fsm:send_event(self(),run),
	{next_state,handle_request,State};
parse_request({body,Data},#state{body_length=Length,client_sock=CSock}=State) ->
	case iolist_size(Data) of
		Size when Size < Length ->
			case gen_socket:recv(CSock,0,2000) of
				{ok,Data2} ->
					gen_fsm:send_event(self(),{body,<<Data/binary,Data2/binary>>}),
					{next_state,parse_request,State};
				Err ->
					gen_fsm:send_event(self(),{500,"Error",[],io_lib:format("Error reading full request:<br/>~n~p",[Err])}),
					{next_state,send_response,State}
				end;
		_Size ->
			gen_fsm:send_event(self(),run),
			{next_state,handle_request,State#state{body=Data}}
	end.

handle_request(run,#state{auth=Auth}=State) ->
	case Auth of
		{User,Pass} ->
			gen_fsm:send_event(self(),{200,"OK",[{'Content-Type',"text/html"}],["This is some output<br/>",User,"<br/>",Pass,"\n",State#state.body,"\n"]});
		_ ->
			gen_fsm:send_event(self(),{401,"OK",[{'WWW-Authenticate',"basic"}],["This is some output with auth required.<br/>\n",State#state.body,"\n"]})
	end,
	{next_state,send_response,State}.

send_response({Status,StatusText,Headers,Body},#state{request={http_request,_,_,{V1,V2}}}=StateData) ->
	BodySize = integer_to_list(iolist_size(Body)),
	GHdr = [{'Server',"Surrogate http_admin"},{'Content-Length',BodySize}],
	Response =
	io_lib:format("HTTP/~p.~p ~p ~s~n",[V1,V2,Status,StatusText])++
		[io_lib:format("~s: ~s\r\n",[atom_to_list(H),S]) || {H,S} <- Headers++GHdr]++
		["\r\n",Body],
	gen_socket:send(StateData#state.client_sock,Response),
	{stop,normal,StateData}.
	

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

