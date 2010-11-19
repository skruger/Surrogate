%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 5, 2010
%%% -------------------------------------------------------------------
-module(proxy_socks45_listener).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("filterproxy.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([accept/2,socks_init/2,socks_request/2,socks4_request/2,http_proxy/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {listen_args,recv_buff,client_sock,userinfo}).

%% ====================================================================
%% External functions
%% ====================================================================


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
init(Args) ->
	gen_fsm:send_event(self(),wait),
    {ok, accept, #state{listen_args=Args}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
accept(wait, StateData) ->
	case gen_tcp:accept((StateData#state.listen_args)#proxy_listener.listen_sock) of
		{ok,Sock} ->
			gen_server:cast((StateData#state.listen_args)#proxy_listener.parent_pid,{child_accepted,self()}),
			gen_fsm:send_event(self(),recv_version),
			inet:setopts(Sock,[{active,false}]),
%% 			io:format("Accepted ~p~n",[Sock]),
			{next_state,socks_init,StateData#state{client_sock=Sock}};
		{error,timeout} ->
			io:format("~p: Accept timeout, retrying.~n",[?MODULE]),
			gen_fsm:send_event(self(),wait),
			{next_state,accept,StateData};
		Err ->
			io:format("~p: Accept error: ~p~n",[?MODULE,Err]),
			gen_server:cast((StateData#state.listen_args)#proxy_listener.parent_pid,{child_accepted,self()}),
			{stop,normal,StateData}
	end.

socks_init(recv_version,StateData) ->
	case gen_tcp:recv(StateData#state.client_sock,0) of
		{ok,<<5:8/integer,_MethodCt:8/integer,Methods/binary>>} ->
			gen_fsm:send_event(self(),{select_method,Methods}),
			{next_state,socks_init,StateData};
		{ok,<<4:8,_/binary>>=Socks4init} ->
%% 			io:format("SOCKS 4 not supported.~n~p~n",[Socks4init]),
			gen_fsm:send_event(self(),{request,Socks4init}),
			{next_state,socks4_request,StateData};
		{ok,<<Ver:8/integer,_:8/integer,Methods/binary>>} ->
			io:format("Unsupported socks version: ~p~n~p~n",[Ver,Methods]),
			{stop,normal,StateData};
		Err ->
			io:format("~p receive error: ~p~n",[?MODULE,Err]),
			{stop,normal,StateData}
	end;
socks_init({select_method,<<>>},StateData) ->
	io:format("No supported SOCKS 5 authentication methods.  Stopping.~n"),
	gen_tcp:send(StateData#state.client_sock,<<5:8/integer,255:8/integer>>),
	gen_tcp:close(StateData#state.client_sock),
	{stop,normal,StateData};
socks_init({select_method,<<0:8/integer,_R/binary>>},State) ->
	gen_tcp:send(State#state.client_sock,<<5:8/integer,0:8/integer>>),
	gen_fsm:send_event(self(),recv_request),
	{next_state,socks_request,State#state{userinfo={socks5_user,anonymous}}};
socks_init({select_method,<<M:8/integer,R/binary>>},State) ->
	io:format("skipping unsupported SOCKS 5 authentication method: ~p~n",[M]),
	gen_fsm:send_event(self(),{select_method,R}),
	{next_state,socks_init,State}.

socks_request(recv_request,State) ->
	case gen_tcp:recv(State#state.client_sock,0) of
		%% Connect command
		{ok,<<5:8/integer,1:8/integer,_Reserved:8/integer,AddrInfo/binary>> = _Pkt} ->
			case decode_address(AddrInfo) of
				{ok,_Host,80} ->
					gen_fsm:send_event(self(),{connect,5}),
					gen_tcp:send(State#state.client_sock,<<5:8/integer,0:8/integer,0:8/integer,AddrInfo/binary>>),
					{next_state,http_proxy,State};
				{ok,Host,Port} ->
					io:format("Socks CONNECT to ~p:~p~n",[Host,Port]),
					case gen_tcp:connect(Host,Port,[binary,{active,false}]) of
						{ok,ServerSock} ->
							gen_tcp:send(State#state.client_sock,<<5:8/integer,0:8/integer,0:8/integer,AddrInfo/binary>>),
							proxy_connect:socks5_connect(State#state.client_sock,ServerSock),
							{stop,normal,State};
						{error,econnrefused} ->
							io:format("~p Connection refused: ~p:~p~n",[?MODULE,Host,Port]),
							gen_tcp:send(State#state.client_sock,<<5:8/integer,5:8/integer,0:8/integer,AddrInfo/binary>>),
							{stop,normal,State};
						{error,ehostunreach} ->
							io:format("~p Host unreachable: ~p:~p~n",[?MODULE,Host,Port]),
							gen_tcp:send(State#state.client_sock,<<5:8/integer,4:8/integer,0:8/integer,AddrInfo/binary>>),
							{stop,normal,State};
						
						Err ->
							io:format("~p General connection error: ~p ~p:~p~n",[?MODULE,Err,Host,Port]),
							gen_tcp:send(State#state.client_sock,<<5:8/integer,1:8/integer,0:8/integer,AddrInfo/binary>>),
							{stop,normal,State}
					end;
				Err ->
					io:format("~p Error: (~p) Invalid host info: ~p",[?MODULE,Err,AddrInfo]),
					%% Respond with "Address type not supported"
					gen_tcp:send(State#state.client_sock,<<5:8/integer,8:8/integer,0:8/integer,AddrInfo/binary>>),
					{stop,normal,State}
			end;
		{ok,<<5:8/integer,Cmd:8/integer,_:8/integer,Rest/binary>> = Pkt} ->
			io:format("~p unsupported command: ~p~n~p~n",[?MODULE,Cmd,Pkt]),
			gen_tcp:send(State#state.client_sock,<<5:8/integer,7:8/integer,0:8/integer,Rest/binary>>),
			gen_tcp:close(State#state.client_sock),
			{stop,normal,State};
		Err ->
			io:format("~p unexpected error while waiting for request: ~p~n",[?MODULE,Err]),
			gen_tcp:close(State#state.client_sock),
			{stop,normal,State}
	end.
			
		
%% Socks 4 CONNECT request
socks4_request({request,<<4:8/integer,1:8/integer,Port:16/integer,A:8,B:8,C:8,D:8,Rest/binary>>},State) ->
	Host = {A,B,C,D},
%% 	io:format("Trying to connect SOCKS 4 (~p:~p)~n",[Host,Port]),
	UserSize = trunc(bit_size(Rest)/8)-1,
	if UserSize > 0 ->
		   <<User:UserSize/binary,_/binary>> = Rest,
		   ProxyUser = binary_to_list(User),
%% 		   io:format("SOCKS 4 User: ~p~n",[User]),
		   ok;
	   true ->
		   ProxyUser = "nouser",
		   ok
	end,
	if
		Port == 80 ->
			gen_tcp:send(State#state.client_sock,<<0:8/integer,90:8/integer,Port:16/integer,A:8,B:8,C:8,D:8>>),
			gen_fsm:send_event(self(),{connect,4}),
			{next_state,http_proxy,State#state{userinfo={socks4_user,ProxyUser}}};
		true ->
			case gen_tcp:connect(Host,Port,[binary,{active,false}]) of
				{ok,ServerSock} ->
					gen_tcp:send(State#state.client_sock,<<0:8/integer,90:8/integer,Port:16/integer,A:8,B:8,C:8,D:8>>),
					proxy_connect:socks5_connect(State#state.client_sock,ServerSock),
					{stop,normal,State};
				Err ->
					io:format("~p General connection error (socks4): ~p ~p:~p~n",[?MODULE,Err,Host,Port]),
					gen_tcp:send(State#state.client_sock,<<0:8/integer,91:8/integer,Port:16/integer,A:8,B:8,C:8,D:8>>),
					{stop,normal,State}
			
			end
	end;
socks4_request({request,Req},State) ->
	io:format("Unsupported SOCKS 4 Request: ~p~n",[Req]),
	<<_:2/binary,AddrInfo:6/binary,_/binary>> = Req,
	gen_tcp:send(State#state.client_sock,<<0:8/integer,91:8/integer,AddrInfo>>),
	{stop,normal,State}.
	

http_proxy({connect,Ver},State) ->
	Sock = State#state.client_sock,
%% 	io:format("Connect SOCKS ~p to http through filter.~n",[Ver]),
	{ok,Parse} = header_parse:start_link(),
	ProxyPass = header_parse:receive_headers(Parse,Sock),
	{ok,Pid} = proxy_pass:start(ProxyPass#proxy_pass{userinfo=State#state.userinfo}),
	case proxylib:parse_request(ProxyPass#proxy_pass.request) of
		#request_rec{method="CONNECT"} ->
			proxy_connect:http_connect(ProxyPass#proxy_pass{client_sock=Sock,proxy_type={socks,Ver}}),
%% 			gen_tcp:close(Sock),
			io:format("Request: ~p~n",[ProxyPass#proxy_pass.request]),
			{stop,normal,State};
		_ ->
			gen_tcp:controlling_process(Sock,Pid),
			gen_fsm:send_event(Pid,{socket,Sock}),
			{stop,normal,State}
	end.

%%     {next_state, state_name, StateData}.

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

decode_address(<<1:8/integer,A:8,B:8,C:8,D:8,Port:16/integer>>) ->
	{ok,{A,B,C,D},Port};
decode_address(<<3:8/integer,Len:8/integer,Rest/binary>>) ->
	<<Addr:Len/binary,Port:16/integer>> = Rest,
	{ok,Addr,Port};
decode_address(AddrInfo) ->
	io:format("~p unsupported address type: ~p~n",AddrInfo),
	{error,badaddr}.
	
