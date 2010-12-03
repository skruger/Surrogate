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

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([proxy_start/2,proxy_client_read/2,proxy_auth/2,proxy_connect/2,client_send/2,server_recv_11/2,proxy_error/2]).

%% -define(LOG(N,P),lists:flatten(io_lib:format(~p)

%% -record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
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
	gen_fsm:send_event(self(),get_headers),
	{next_state,proxy_client_read,State#proxy_pass{client_sock=CSock}};
proxy_start({reverse_proxy,CSock,{host,Host,Port}=_Addr}=_L,State) ->
%% 	io:format("proxy_start() ~p~n",[L]),
	case gen_tcp:connect(Host,Port,[binary,inet,{active,false}],60000) of
		{ok,SSock0} ->
			{ok,SSock} = gen_socket:create(SSock0,gen_tcp),
			gen_fsm:send_event(self(),{headers,(State#proxy_pass.request)#header_block.headers}),
			{next_state,client_send,State#proxy_pass{client_sock=CSock,server_sock=SSock}};
		Err ->
			?INFO_MSG("reverse_proxy connect() error: ~p~n",[Err]),
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

proxy_client_read(get_headers,State0) ->
	ReqHdr = header_parse:get_headers(State0#proxy_pass.client_sock,request),
	State = State0#proxy_pass{request=ReqHdr,proxy_type=(ReqHdr#header_block.request)#request_rec.proxytype},
%% 	case proxyconf:get(proxy_auth,false) of
	case proplists:get_value(proxy_auth,State#proxy_pass.config,false) of
		false ->
%% 			io:format("No auth.~n"),
			gen_fsm:send_event(self(),start),
			{next_state,proxy_connect,State};
		_AuthCfg when State#proxy_pass.userinfo =/= undefined ->
%% 			io:format("Already had auth: ~p~n",[State#proxy_pass.userinfo]),
			gen_fsm:send_event(self(),start),
			{next_state,proxy_connect,State};
		AuthCfg ->
			gen_fsm:send_event(self(),{check_auth,AuthCfg}),
			{next_state,proxy_auth,State}
	end.

proxy_auth({check_auth,_AuthCfg},State) ->
%% 	?DEBUG_MSG("Check auth: ~p~n",[AuthCfg]),
	Dict = proxylib:header2dict((State#proxy_pass.request)#header_block.headers),
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
%% 							io:format("User: ~p, Pass: ~p ok~n",[User,Pass]),
							gen_fsm:send_event(self(),start),
							{next_state,proxy_connect,State#proxy_pass{userinfo=UserInfo}};
						Err ->
							?ERROR_MSG("Authentication error for ~p: ~p (~p)~n",[User,Err,Pass]),
							gen_fsm:send_event(self(),send_challenge),
							{next_state,proxy_auth,State}
					end
			end;
		_Err ->
%% 			io:format("No auth: ~p~n",[Err]),
			gen_fsm:send_event(self(),send_challenge),
			{next_state,proxy_auth,State}
	end;
proxy_auth(send_challenge,State) ->
%% 	io:format("Sending auth challenge~n"),
	AuthReq = "HTTP/1.1 407 Proxy Auth\r\nProxy-Authenticate: Basic realm=\"FastProxy2\"\r\nConnection: close\r\n\r\n",
	?ACCESS_LOG(407,(State#proxy_pass.request)#header_block.rstr,"nouser","Proxy authorization request."),
	gen_socket:send(State#proxy_pass.client_sock,AuthReq),
	gen_socket:close(State#proxy_pass.client_sock),
	{stop,normal,State}.


proxy_connect(start,State) ->
	case (State#proxy_pass.request)#header_block.request of
		#request_rec{method="CONNECT"} ->
%% 			io:format("Post authentication CONNECT~n"),
			case proxy_connect:http_connect(State) of
				ok ->
					?ACCESS_LOG(200,(State#proxy_pass.request)#header_block.rstr,State#proxy_pass.userinfo,"Connection Established"),
					{stop,normal,State};
				_ ->
					%% send_event() should be done by proxy_connect:http_connect()
					{next_state,proxy_error,State}
			end;
		_ ->
			gen_fsm:send_event(self(),open_socket),
			{next_state,proxy_connect,State}
	end;
proxy_connect(open_socket,State) ->
	Dict = proxylib:header2dict((State#proxy_pass.request)#header_block.headers),
	case dict:find("host",Dict) of
		{ok,HostStr} ->
			{host,Host,Port} = proxylib:parse_host(HostStr,80),
			FList = proplists:get_value(proxy_filters,State#proxy_pass.config,[]),
			case filter_check:host(FList,Host,State#proxy_pass.userinfo) of
				deny ->
					EMsg = io_lib:format("Deny by rule for host: ~p~n<br/>Hdr: ~p~n",[Host,(State#proxy_pass.request)#header_block.headers]),
					gen_fsm:send_event(self(),{error,403,"Forbidden",lists:flatten(EMsg)}),
					{next_state,proxy_error,State};
				_Ok ->
					case gen_tcp:connect(Host,Port,[binary,inet,{active,false}],20000) of
						{ok,SSock0} ->
							{ok,SSock} = gen_socket:create(SSock0,gen_tcp),
							gen_fsm:send_event(self(),{headers,(State#proxy_pass.request)#header_block.headers}),
							{next_state,client_send,State#proxy_pass{server_sock=SSock}};
						
						{error,ErrStat} = Err ->
							?DEBUG_MSG("~p connect() error: ~p~n~p~n",[?MODULE,Err,HostStr]),
							gen_fsm:send_event(self(),{error,503,lists:flatten(io_lib:format("Error connecting to server: ~p ~p",[HostStr,ErrStat]))}),
							{next_state,proxy_error,State}
					end
			end;
		_Err ->
			?INFO_MSG("Didn't receive a host header: ~p~n",[dict:to_list(Dict)]),
			gen_fsm:send_event(self(),{error,503,lists:flatten(io_lib:format("No host header received from client",[]))}),
			{next_state,proxy_error,State}
	end.


							

client_send({headers,Hdr},State) ->
	HBlock = proxylib:combine_headers(Hdr),
	Req = (State#proxy_pass.request)#header_block.request,
%% 	RequestText = lists:flatten(io_lib:format("~s ~s ~s\r\n~s",[Req#request_rec.method,Req#request_rec.path,"HTTP/1.0",HBlock])),
	RequestText = lists:flatten(io_lib:format("~s ~s ~s\r\n~s",[Req#request_rec.method,Req#request_rec.path,Req#request_rec.protocol,HBlock])),
	gen_socket:send(State#proxy_pass.server_sock,RequestText),
	Dict = proxylib:header2dict((State#proxy_pass.request)#header_block.headers),
	case dict:find("content-length",Dict) of
		{ok,Length} ->
			{Len,_} = string:to_integer(Length),
			gen_fsm:send_event(self(),{request_body,Len}),
			{next_state,client_send,State};
		_ ->
			gen_fsm:send_event(self(),response),
			{next_state,server_recv_11,State}
	end;		
client_send({request_body,Len},State) when Len < 1 ->
	gen_fsm:send_event(self(),response),
	{next_state,server_recv_11,State};
client_send({request_body,Len},State) when (State#proxy_pass.request)#header_block.body /= <<>> ->
	gen_socket:send(State#proxy_pass.server_sock,(State#proxy_pass.request)#header_block.body),
	gen_fsm:send_event(self(),{request_body,Len-trunc(bit_size((State#proxy_pass.request)#header_block.body)/8)}),
	NewReq = (State#proxy_pass.request)#header_block{body = <<>>},
	{next_state,client_send,State#proxy_pass{request =  NewReq} };
client_send({request_body,Len},State) ->
 	case gen_socket:recv(State#proxy_pass.client_sock,0,500) of
		{ok,Packet} ->
			gen_socket:send(State#proxy_pass.server_sock,Packet),
			gen_fsm:send_event(self(),{request_body,Len-trunc(bit_size(Packet)/8)}),
			{next_state,client_send,State};
		{error,Reason} ->
			?INFO_MSG("Error reading client socket: ~p~n",[Reason]),
			gen_fsm:send_event(self(),response),
			{next_state,server_recv_11,State}
	end.


server_recv_11(response,State) ->
%% 	?DEBUG_MSG("Staring proxy_read_response (~p)~n",[self()]),
	case proxy_read_response:start(State#proxy_pass.server_sock,State#proxy_pass.request) of
		{proxy_read_response,_} = RDrv ->
			{next_state,server_recv_11,State#proxy_pass{response_driver=RDrv}};
		Err ->
			?ERROR_MSG("Error starting proxy_read_response: ~p~n",[Err]),
			EMsg = io_lib:format("Internal proxy error: ~p",[Err]),
			gen_fsm:send_event(self(),{error,503,lists:flatten(EMsg)}),
			{next_state,proxy_error,State}
	end;
server_recv_11({response_header,ResHdr,ResponseSize},State) ->
%% 	?DEBUG_MSG("Got response_header (~p).~n",[self()]),
	ResponseHeaders = [[ResHdr#header_block.rstr|"\r\n"]|proxylib:combine_headers(ResHdr#header_block.headers)],
	gen_socket:send(State#proxy_pass.client_sock,ResponseHeaders),
	proxy_read_response:get_next(State#proxy_pass.response_driver),
	{next_state,server_recv_11,State#proxy_pass{response=ResHdr,response_bytes_left=ResponseSize}};
server_recv_11({response_data,Data},State) when State#proxy_pass.response_bytes_left == chunked ->
%% 	?DEBUG_MSG("Got chunked {response_data,_}, send out as a chunk.~p~n",[self()]),
	DataLen = list_to_binary(erlang:integer_to_list(trunc(bit_size(Data)/8),16)),
	DataOut = <<DataLen/binary,"\r\n",Data/binary,"\r\n">>,
	gen_socket:send(State#proxy_pass.client_sock,DataOut),
	proxy_read_response:get_next(State#proxy_pass.response_driver),
	{next_state,server_recv_11,State};
server_recv_11({end_response_data,_Size},State) when State#proxy_pass.response_bytes_left == chunked ->
	gen_socket:send(State#proxy_pass.client_sock,<<"0\r\n\r\n">>),
%% 	?DEBUG_MSG("Done, Stopping ~p after ~p bytes in chunked mode.~n",[self(),Size]),
	gen_socket:close(State#proxy_pass.server_sock),
	gen_socket:close(State#proxy_pass.client_sock),
	{stop,normal,State};
server_recv_11({response_data,Data},State) ->
%% 	?DEBUG_MSG("Got {response_data,_}. ~p~n",[self()]),
	gen_socket:send(State#proxy_pass.client_sock,Data),
	proxy_read_response:get_next(State#proxy_pass.response_driver),
	{next_state,server_recv_11,State};
server_recv_11({end_response_data,_Size},State) ->
%% 	?DEBUG_MSG("Done, Stopping ~p after ~p bytes~n",[self(),Size]),
	gen_socket:close(State#proxy_pass.server_sock),
	gen_socket:close(State#proxy_pass.client_sock),
	{stop,normal,State}.


proxy_error({error,Code},State) ->
	?DEBUG_MSG("Proxy error: ~p~n",[Code]),
	case Code of
		_ ->
			gen_fsm:send_event(self(),{error,Code,"General Proxy Failure"}),
			{next_state,proxy_error,State}
	end;
proxy_error({error,Code,Desc},State) ->
	proxy_error({error,Code,"Proxy Error",Desc},State);
proxy_error({error,Code,RText,Desc},State) ->
	?DEBUG_MSG("Proxy error: ~p ~p~n",[Code,Desc]),
	ICode = integer_to_list(Code),
	Err = "HTTP/1.0 "++ICode++" "++RText++"\r\nContent-type: text/html\r\nConnection: close\r\n\r\n",
	EResponse = lists:flatten(io_lib:format("~s<h3>~s - ~s</h3>",[Err,ICode,Desc])),
	?ACCESS_LOG(Code,(State#proxy_pass.request)#header_block.rstr,State#proxy_pass.userinfo,Desc),
	gen_socket:send(State#proxy_pass.client_sock,EResponse),
	gen_socket:close(State#proxy_pass.client_sock),
	{stop,normal,State}.

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
% {response_header,#header_block{},ResponseSize}
% ResponseSize = int() | chunked
%
% {response_data,Data}
% Data = binary()
%
% {end_response_data,ByteLength}
handle_info({response_header,_,_}=I,StateName,StateData) ->
%% 	?DEBUG_MSG("Got response header in state ~p~n",[StateName]),
	gen_fsm:send_event(self(),I),
	{next_state, StateName, StateData};
handle_info({response_data,_}=I,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: {response_data,_} in state ~p~n",[StateName]),
	gen_fsm:send_event(self(),I),
	{next_state, StateName, StateData};
handle_info({end_response_data,_}=I,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: ~p~n",[I]),
	gen_fsm:send_event(self(),I),
	{next_state, StateName, StateData};
handle_info(Info, StateName, StateData) ->
	?ERROR_MSG("Unmatched info: ~p~n",[Info]),
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

