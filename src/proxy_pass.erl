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

-export([proxy_start/2,client_send_11/2,server_recv_11/2,proxy_error/2,proxy_finish/2]).

%% ,proxy_auth/2,proxy_client_read/2,proxy_connect/2,client_send/2

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
	Filters = proplists:get_value(stream_filters,Args#proxy_pass.config,[]),
	FilterRef = filter_stream:init_filter_list(Filters),
    {ok, proxy_start, Args#proxy_pass{filters=FilterRef,keepalive=0}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
proxy_start({socket,CSock},State) ->
	gen_fsm:send_event(self(),request),
%% 	?DEBUG_MSG("peername: ~p~n",[gen_socket:peername(CSock)]),
	{next_state,client_send_11,State#proxy_pass{client_sock=CSock}};
proxy_start({reverse_proxy,CSock,{host,_Host,_Port}=Addr}=_L,State) ->
	gen_fsm:send_event(self(),request),
	{next_state,client_send_11,State#proxy_pass{client_sock=CSock,reverse_proxy_host=Addr}};
proxy_start({error,_,_,_}=Err,State) ->
	gen_fsm:send_event(self(),Err),
	{next_state,proxy_error,State}.

%% proxy_auth({check_auth,_AuthCfg},State) ->
%% %% 	?DEBUG_MSG("Check auth: ~p~n",[AuthCfg]),
%% 	Dict = proxylib:header2dict((State#proxy_pass.request)#header_block.headers),
%% 	case dict:find("proxy-authorization",Dict) of
%% 		{ok,"Basic "++AuthStr} ->
%% 			Auth2 = binary_to_list(base64:decode(AuthStr)),
%% 			case string:chr(Auth2,$:) of
%% 				0 ->
%% 					gen_fsm:send_event(self(),send_challenge),
%% 					{next_state,proxy_auth,State};
%% 				Idx ->
%% 					User = string:substr(Auth2,1,Idx-1),
%% 					Pass = string:substr(Auth2,Idx+1),
%% 					
%% 					case proxy_auth:check_user(User,Pass) of
%% 						{ok,UserInfo} ->
%% %% 							io:format("User: ~p, Pass: ~p ok~n",[User,Pass]),
%% 							gen_fsm:send_event(self(),start),
%% %% 							Hdr = proxylib:replace_header("proxy-authorization","Proxy-Authorization: none",(State#proxy_pass.request)#header_block.headers),
%% %% 							Request = (State#proxy_pass.request)#header_block{headers=Hdr},
%% 							{next_state,proxy_connect,State#proxy_pass{userinfo=UserInfo}};
%% 						Err ->
%% 							?ERROR_MSG("Authentication error for ~p: ~p (~p)~n",[User,Err,Pass]),
%% 							gen_fsm:send_event(self(),send_challenge),
%% 							{next_state,proxy_auth,State}
%% 					end
%% 			end;
%% 		_Err ->
%% %% 			io:format("No auth: ~p~n",[Err]),
%% 			gen_fsm:send_event(self(),send_challenge),
%% 			{next_state,proxy_auth,State}
%% 	end;
%% proxy_auth(send_challenge,State) ->
%% %% 	io:format("Sending auth challenge~n"),
%% 	AuthReq = "HTTP/1.1 407 Proxy Auth\r\nProxy-Authenticate: Basic realm=\"FastProxy2\"\r\nConnection: close\r\n\r\n",
%% 	?ACCESS_LOG(407,(State#proxy_pass.request)#header_block.rstr,"nouser","Proxy authorization request."),
%% 	gen_socket:send(State#proxy_pass.client_sock,AuthReq),
%% 	gen_socket:close(State#proxy_pass.client_sock),
%% 	{stop,normal,State}.

client_send_11(request,State) ->
	try
		case proxy_read_request:start(State#proxy_pass.client_sock) of
			{proxy_read_request,_} = RDrv ->
				{next_state,client_send_11,State#proxy_pass{request_driver=RDrv}};
			Err ->
				?ERROR_MSG("Error starting proxy_read_request: ~p~n",[Err]),
				EMsg = io_lib:format("Internal proxy error: ~p",[Err]),
				gen_fsm:send_event(self(),{error,503,lists:flatten(EMsg)}),
				{next_state,proxy_error,State}
		end
 	catch
 		_:{error,closed} -> {stop,normal,State};
 		_:ErrCatch ->
 			?ERROR_MSG("Error receiving headers: ~p (Keepalive: ~p)~n",[ErrCatch,State#proxy_pass.keepalive]),
 			{stop,normal,State}
	end;
client_send_11({request_header,ReqHdr,_RequestSize}=_R,State0) ->
	State = State0#proxy_pass{request=ReqHdr},
%% 	?DEBUG_MSG("request_header: ~p~n",[R]),
	case ReqHdr#header_block.request of
		#request_rec{method="CONNECT"} ->
			proxy_read_request:stop(State#proxy_pass.request_driver),
			gen_socket:info(State#proxy_pass.client_sock),
%% 			?DEBUG_MSG("Connect: ~p~n",[ReqHdr]),
			case proxy_connect:http_connect(State) of
				ok ->
					?ACCESS_LOG(200,(State#proxy_pass.request)#header_block.rstr,State#proxy_pass.userinfo,"Connection Established"),
					{stop,normal,State};
				_ ->
					%% send_event() should be done by proxy_connect:http_connect()
					{next_state,proxy_error,State}
			end;
		_ ->
			gen_fsm:send_event(self(),connect_server),
			{next_state,client_send_11,State}
	end;
client_send_11(connect_server,State) ->
	Dict = proxylib:header2dict((State#proxy_pass.request)#header_block.headers),
	ReqHdr = State#proxy_pass.request,
	Via = io_lib:format("Via: ~s ~s (Surrogate)",[((State#proxy_pass.request)#header_block.request)#request_rec.protocol,net_adm:localhost()]),
	Hdr0 = proxylib:append_header(Via, ReqHdr#header_block.headers),
	Hdr = proxylib:remove_headers(["accept-encoding","keep-alive","proxy-connection","proxy-authorization"],Hdr0),
	RequestHeaders = [[ReqHdr#header_block.rstr|"\r\n"]|proxylib:combine_headers(Hdr)],
	ConnHost = 
	case State#proxy_pass.reverse_proxy_host of
		{host,_Host,_Port} = H -> H;
		_ ->
			case dict:find("host",Dict) of
				{ok,HostStr} ->
					proxylib:parse_host(HostStr,80);
				EHost -> EHost
			end

	end,
	case ConnHost of
		{host,Host,Port} ->
			case gen_tcp:connect(Host,Port,[binary,inet,{active,false}],20000) of
				{ok,SSock0} ->
					{ok,SSock} = gen_socket:create(SSock0,gen_tcp),
					gen_socket:send(SSock,RequestHeaders),
					proxy_read_request:get_next(State#proxy_pass.request_driver),
					{next_state,client_send_11,State#proxy_pass{server_sock=SSock}};
				ErrConn ->
					?ERROR_MSG("Could not Connect to backend server: ~p ~p~n",[ConnHost,ErrConn]),
					EMsg = io_lib:format("Internal proxy error: ~p",[ErrConn]),
					gen_fsm:send_event(self(),{error,503,lists:flatten(EMsg)}),
					{next_state,proxy_error,State}
			end;
		Err ->
			?ERROR_MSG("Could not determine backend server: ~p~n",[Err]),
			EMsg = io_lib:format("Internal proxy error: ~p",[Err]),
			gen_fsm:send_event(self(),{error,503,lists:flatten(EMsg)}),
			{next_state,proxy_error,State}
	end;
client_send_11({request_data,Data},State) ->
	gen_socket:send(State#proxy_pass.server_sock,Data),
	{next_state,client_send_11,State};
client_send_11({end_request_data,_Size},State) ->
	gen_fsm:send_event(self(),response),
	{next_state,server_recv_11,State};
client_send_11(Extra,State) ->
	?ERROR_MSG("Extra data in client_send_11: ~p~n",[Extra]),
	{stop,normal,State}.
	

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
	?ACCESS_LOG(200,(State#proxy_pass.request)#header_block.rstr,State#proxy_pass.userinfo,ResHdr#header_block.rstr),
%% 	?DEBUG_MSG("Got response_header (~p).~n",[self()]),
	ResponseHeaders = [[ResHdr#header_block.rstr|"\r\n"]|proxylib:combine_headers(ResHdr#header_block.headers)],
	gen_socket:send(State#proxy_pass.client_sock,ResponseHeaders),
	proxy_read_response:get_next(State#proxy_pass.response_driver),
	{next_state,server_recv_11,State#proxy_pass{response=ResHdr,response_bytes_left=ResponseSize}};
%% response_bytes_left == close when connection: close set for http/1.0 force chunk encoding
server_recv_11({response_data,Data},State) when State#proxy_pass.response_bytes_left == close ->
	gen_socket:send(State#proxy_pass.client_sock,Data),
	proxy_read_response:get_next(State#proxy_pass.response_driver),
	{next_state,server_recv_11,State};
server_recv_11({end_response_data,_Size},State) when State#proxy_pass.response_bytes_left == close ->
	gen_socket:close(State#proxy_pass.server_sock),
	gen_socket:close(State#proxy_pass.client_sock),
	{stop,normal,State};
server_recv_11({response_data,Data},State) when State#proxy_pass.response_bytes_left == chunked ->
%% 	?DEBUG_MSG("Got chunked {response_data,_}, send out as a chunk.~p~n",[self()]),
	DataLen = list_to_binary(erlang:integer_to_list(trunc(bit_size(Data)/8),16)),
	DataOut = <<DataLen/binary,"\r\n",Data/binary,"\r\n">>,
	gen_socket:send(State#proxy_pass.client_sock,DataOut),
	proxy_read_response:get_next(State#proxy_pass.response_driver),
	{next_state,server_recv_11,State};
server_recv_11({end_response_data,_Size},State) when State#proxy_pass.response_bytes_left == chunked ->
	gen_socket:send(State#proxy_pass.client_sock,<<"0\r\n\r\n">>),
	gen_fsm:send_event(self(),next),
	{next_state,proxy_finish,State};
server_recv_11({response_data,Data},State) ->
%% 	?DEBUG_MSG("Got {response_data,_}. ~p~n",[self()]),
	gen_socket:send(State#proxy_pass.client_sock,Data),
	proxy_read_response:get_next(State#proxy_pass.response_driver),
	{next_state,server_recv_11,State};
server_recv_11({end_response_data,_Size},State) ->
%% 	?DEBUG_MSG("Done, Stopping ~p after ~p bytes~n",[self(),Size]),
%% 	gen_socket:close(State#proxy_pass.server_sock),
%% 	gen_socket:close(State#proxy_pass.client_sock),
%% 	{stop,normal,State}.
	gen_fsm:send_event(self(),next),
	{next_state,proxy_finish,State}.
	


proxy_finish(next,State) ->
	Dict = proxylib:header2dict((State#proxy_pass.request)#header_block.headers),
%% 	gen_socket:close(State#proxy_pass.server_sock),
%% 	gen_socket:close(State#proxy_pass.client_sock),
%% 	{stop,normal,State}.
	case dict:find("proxy-connection",Dict) of
		{ok,"keep-alive"} when State#proxy_pass.keepalive >= 10 ->
			?DEBUG_MSG("Proxy-Connection: keep-alive Closing (~p)~n",[State#proxy_pass.keepalive]),
			gen_socket:close(State#proxy_pass.server_sock),
			gen_socket:close(State#proxy_pass.client_sock),
			{stop,normal,State};
		{ok,"keep-alive"} ->
%% 			?DEBUG_MSG("Proxy-Connection: keep-alive (~p)~n",[State#proxy_pass.keepalive]),
			gen_fsm:send_event(self(),request),
			gen_socket:close(State#proxy_pass.server_sock),
			{next_state,client_send_11,State#proxy_pass{server_sock=undefined,keepalive=State#proxy_pass.keepalive+1}};
		{ok,"close"} ->
			gen_socket:close(State#proxy_pass.server_sock),
			gen_socket:close(State#proxy_pass.client_sock),
			{stop,normal,State};
		error ->
			gen_socket:close(State#proxy_pass.server_sock),
			gen_socket:close(State#proxy_pass.client_sock),
			{stop,normal,State};
		Conn ->
			?DEBUG_MSG("Closing with unknown value \"Connection: ~p\"~n",[Conn]),
			gen_socket:close(State#proxy_pass.server_sock),
			gen_socket:close(State#proxy_pass.client_sock),
			{stop,normal,State}
	end.
			
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
handle_info({request_header,_,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Got request header in state ~p~n",[StateName]),
	case filter_stream:process_hooks(request,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I -> gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({request_data,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: {request_data,_} in state ~p~n",[StateName]),
	case filter_stream:process_hooks(request,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I -> gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({end_request_data,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: ~p~n",[I]),
	case filter_stream:process_hooks(request,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I -> gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({response_header,_,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Got response header in state ~p~n",[StateName]),
	case filter_stream:process_hooks(request,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I -> gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({response_data,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: {response_data,_} in state ~p~n",[StateName]),
	case filter_stream:process_hooks(request,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I -> gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({end_response_data,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: ~p~n",[I]),
	case filter_stream:process_hooks(request,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I -> gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({filter_delay,Data},StateName,StateData) ->
	gen_fsm:send_event(self(),Data),
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

