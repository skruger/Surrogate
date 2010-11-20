%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Oct 30, 2010
%%% -------------------------------------------------------------------
-module(proxy_pass).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("filterproxy.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([proxy_start/2,proxy_auth/2,proxy_connect/2,client_send/2,server_recv/2,server_start_recv/2]).

%% -define(LOG(N,P),lists:flatten(io_lib:format(~p)

%% -record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start(Args) when Args#proxy_pass.csock_mod == undefined ->
	start(Args#proxy_pass{csock_mod=gen_tcp});
start(Args) ->
	gen_fsm:start_link(?MODULE,Args,[{debug,[log]}]).

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
%% init({balance,Pool}) ->
	
init(Args) ->
    {ok, proxy_start, Args}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
proxy_start({socket,CSock},State) ->
	case proxyconf:get(proxy_auth,false) of
		false ->
			io:format("No auth.~n"),
			gen_fsm:send_event(self(),start),
			{next_state,proxy_connect,State#proxy_pass{client_sock=CSock}};
		_AuthCfg when State#proxy_pass.userinfo =/= undefined ->
			io:format("Already had auth: ~p~n",[State#proxy_pass.userinfo]),
			gen_fsm:send_event(self(),start),
			{next_state,proxy_connect,State#proxy_pass{client_sock=CSock}};
		AuthCfg ->
			gen_fsm:send_event(self(),{check_auth,AuthCfg}),
			{next_state,proxy_auth,State#proxy_pass{client_sock=CSock}}
	end;
proxy_start({reverse_proxy,CSock,{host,Host,Port}=_Addr}=_L,State) ->
%% 	io:format("proxy_start() ~p~n",[L]),
	case gen_tcp:connect(Host,Port,[binary,inet,{active,false}],60000) of
		{ok,SSock} ->
			gen_fsm:send_event(self(),{headers,State#proxy_pass.headers}),
			{next_state,client_send,State#proxy_pass{client_sock=CSock,server_sock=SSock}};
		Err ->
			io:format("~p connect() error: ~p~n",[?MODULE,Err]),
			{stop,normal,State}
	end;
proxy_start({balance,Pool,Port,Sock}=_L,State) ->
%% 	io:format("Starting balancer: ~p~n",[L]),
	case balancer:next(Pool) of
		{IP,OPort} ->
			gen_fsm:send_event(self(),{reverse_proxy,Sock,{host,IP,OPort}}),
			{next_state,proxy_start,State};
		{_,_,_,_} = IP ->
			gen_fsm:send_event(self(),{reverse_proxy,Sock,{host,IP,Port}}),
			{next_state,proxy_start,State};
		_ ->
			{stop,normal,State}
	end.

proxy_auth({check_auth,_AuthCfg},State) ->
%% 	io:format("Check auth: ~p~n",[AuthInfo]),
	Dict = proxylib:header2dict(State#proxy_pass.headers),
	case dict:find("proxy-authorization",Dict) of
		{ok,"Basic "++AuthStr} ->
			Auth2 = binary_to_list(base64:decode(AuthStr)),
			case string:chr(Auth2,$:) of
				0 ->
					gen_fsm:send_event(self(),send_challenge),
					{next_state,proxy_auth,State};
				Idx ->
					User = string:substr(Auth2,1,Idx-1),
					Pass = string:substr(Auth2,Idx+1),
					
					case proxy_auth:check_user(User,Pass) of
						{ok,UserInfo} ->
							io:format("User: ~p, Pass: ~p ok~n",[User,Pass]),
							gen_fsm:send_event(self(),start),
							{next_state,proxy_connect,State#proxy_pass{userinfo=UserInfo}};
						Err ->
							io:format("Authentication error for ~p: ~p (~p)~n",[User,Err,Pass]),
							gen_fsm:send_event(self(),send_challenge),
							{next_state,proxy_auth,State}
					end
			end;
		Err ->
			io:format("No auth: ~p~n",[Err]),
			gen_fsm:send_event(self(),send_challenge),
			{next_state,proxy_auth,State}
	end;
proxy_auth(send_challenge,State) ->
	io:format("Sending auth challenge~n"),
	AuthReq = "HTTP/1.1 407 Proxy Auth\r\nProxy-Authenticate: Basic realm=\"FastProxy2\"\r\n\r\n",
	(State#proxy_pass.csock_mod):send(State#proxy_pass.client_sock,AuthReq),
	(State#proxy_pass.csock_mod):close(State#proxy_pass.client_sock),
	{stop,normal,State}.


proxy_connect(start,State) ->
	case proxylib:parse_request(State#proxy_pass.request) of
		#request_rec{method="CONNECT"} ->
			io:format("Post authentication CONNECT~n"),
			proxy_connect:http_connect(State),
			{stop,normal,State};
		_ ->
			gen_fsm:send_event(self(),open_socket),
			{next_state,proxy_connect,State}
	end;
proxy_connect(open_socket,State) ->
	Dict = proxylib:header2dict(State#proxy_pass.headers),
%% 	ReqRec = proxylib:parse_request(State#proxy_pass.request),
	case dict:find("host",Dict) of
		{ok,HostStr} ->
			{host,Host,Port} = proxylib:parse_host(HostStr,80),
			case gen_tcp:connect(Host,Port,[binary,inet,{active,false}],20000) of
				{ok,SSock} ->
					gen_fsm:send_event(self(),{headers,State#proxy_pass.headers}),
					{next_state,client_send,State#proxy_pass{server_sock=SSock}};
				Err ->
					io:format("~p connect() error: ~p~n",[?MODULE,Err]),
					{stop,normal,State}
			end;
		_Err ->
			io:format("~p didn't receive a host header: ~p~n",[?MODULE,dict:to_list(Dict)]),
			{stop, normal,State}
	end.


							

client_send({headers,Hdr},State) ->
	HBlock = proxylib:combine_headers(Hdr),
	Req = proxylib:parse_request(State#proxy_pass.request),
	RequestText = lists:flatten(io_lib:format("~s ~s ~s\r\n~s",[Req#request_rec.method,Req#request_rec.path,"HTTP/1.0",HBlock])),
	gen_tcp:send(State#proxy_pass.server_sock,RequestText),
	Dict = proxylib:header2dict(State#proxy_pass.headers),
	case dict:find("content-length",Dict) of
		{ok,Length} ->
			{Len,_} = string:to_integer(Length),
			gen_fsm:send_event(self(),{request_body,Len}),
			{next_state,client_send,State};
		_ ->
			gen_fsm:send_event(self(),response),
			{next_state,server_start_recv,State}
	end;		
%% 	io:format("Request: ~n~p~n====~n",[RequestText]),
client_send({request_body,Len},State) when Len < 1 ->
%% 	io:format("Request body sent.  Len=~p~n",[Len]),
	gen_fsm:send_event(self(),response),
	{next_state,server_start_recv,State};
client_send({request_body,Len},State) when State#proxy_pass.recv_buff /= <<>> ->
	gen_tcp:send(State#proxy_pass.server_sock,State#proxy_pass.recv_buff),
	gen_fsm:send_event(self(),{request_body,Len-trunc(bit_size(State#proxy_pass.recv_buff)/8)}),
%% 	io:format("Sending extra request text: ~n~p~n",[State#proxy_pass.recv_buff]),
	{next_state,client_send,State#proxy_pass{recv_buff = <<>>}};
client_send({request_body,Len},State) ->
%% 	io:format("Waiting for extra request text with ~p left.~n",[Len]),
	case (State#proxy_pass.csock_mod):recv(State#proxy_pass.client_sock,0,500) of
		{ok,Packet} ->
			gen_tcp:send(State#proxy_pass.server_sock,Packet),
			gen_fsm:send_event(self(),{request_body,Len-trunc(bit_size(Packet)/8)}),
			{next_state,client_send,State};
		{error,Reason} ->
			io:format("Error reading client socket: ~p~n",[Reason]),
			gen_fsm:send_event(self(),response),
			{next_state,server_start_recv,State}
	end.

server_start_recv(response,State) ->
	{ok,Parse} = header_parse:start_link(),
	SvrHeader = header_parse:receive_headers(Parse,State#proxy_pass.server_sock),
	ResponseHeaders = [[SvrHeader#proxy_pass.request|"\r\n"]|proxylib:combine_headers(SvrHeader#proxy_pass.headers)],
	(State#proxy_pass.csock_mod):send(State#proxy_pass.client_sock,ResponseHeaders),
	case proxylib:method_has_data(State#proxy_pass.request,SvrHeader) of
		true ->
			Dict = proxylib:header2dict(SvrHeader#proxy_pass.headers),
			case dict:find("content-length",Dict) of
				{ok,Length} ->
					{Len,_} = string:to_integer(Length),
					if 
						SvrHeader#proxy_pass.recv_buff /= <<>> ->
%% 							io:format("Sending extra for response to: ~p~n",[State#proxy_pass.request]),
							(State#proxy_pass.csock_mod):send(State#proxy_pass.client_sock,SvrHeader#proxy_pass.recv_buff),
							gen_fsm:send_event(self(),{response,Len-trunc(bit_size(SvrHeader#proxy_pass.recv_buff)/8)});
						true ->
							gen_fsm:send_event(self(),{response,Len})
					end,
					{next_state,server_recv,State};
				_Err ->
%% 					io:format("No content-length header:~n~p~n~p~n~p~n",[State#proxy_pass.request,SvrHeader#proxy_pass.request,SvrHeader#proxy_pass.headers]),
					gen_fsm:send_event(self(),response),
					(State#proxy_pass.csock_mod):send(State#proxy_pass.client_sock,SvrHeader#proxy_pass.recv_buff),
					{next_state,server_recv,State}
				end;
		false ->
%% 			io:format("No content expected:~n~p~n~p~n",[State#proxy_pass.request,SvrHeader]),
			gen_fsm:send_event(self(),{response,0}),
			{next_state,server_recv,State}
	end.

server_recv({response,Len},State) when Len < 1->
%% 	io:format("Response ended with Len=~p~n",[Len]),
	gen_tcp:close(State#proxy_pass.server_sock),
	(State#proxy_pass.csock_mod):close(State#proxy_pass.client_sock),
	{stop,normal,State};
server_recv({response,Len},State) ->
	case gen_tcp:recv(State#proxy_pass.server_sock,0,300000) of
		{ok,Packet} ->
			(State#proxy_pass.csock_mod):send(State#proxy_pass.client_sock,Packet),
			gen_fsm:send_event(self(),{response,Len-trunc(bit_size(Packet)/8)}),
			{next_state,server_recv,State};
		{error,closed} ->
%% 			io:format("Socket closed: ~p~n",[State#proxy_pass.request]),
			(State#proxy_pass.csock_mod):close(State#proxy_pass.client_sock),
			{stop,normal,State};
		{error,Reason} ->
			io:format("Recv ended: ~p~n",[Reason]),
			(State#proxy_pass.csock_mod):close(State#proxy_pass.client_sock),
			{stop,normal,State}
	end;
%% send_event(self(),response) is used for connections that do not implement content-length headers
%% The headers have already been recieved and the data should be flowing.  A gen_tcp:recv() timeout
%% will kill the connection.  This is normal with HTTP/1.0 responses sending content of unknown length.
server_recv(response,State) ->
	case gen_tcp:recv(State#proxy_pass.server_sock,0,5000) of
		{ok,Packet} ->
			(State#proxy_pass.csock_mod):send(State#proxy_pass.client_sock,Packet),
			gen_fsm:send_event(self(),response),
			{next_state,server_recv,State};
		{error,closed} ->
			(State#proxy_pass.csock_mod):close(State#proxy_pass.client_sock),
			{stop,normal,State};
		{error,Reason} ->
			io:format("Recv ended: ~p~n",[Reason]),
			{stop,normal,State}
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

