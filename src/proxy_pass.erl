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
-export([start/1,start/2,start_remote/2]).

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
	case proplists:get_value(worker_pool,Args#proxy_pass.config,'NONE') of
		'NONE' ->
			do_start(Args);
		WorkerPool ->
			start(worker_pool:next(WorkerPool),Args)
	end.

start(Node,Args) when Node == node() ->
%% 	?WARN_MSG("Attempting remote start to self (~p).~n",[Node]),
	do_start(Args);
start(Node,Args) ->
	case net_adm:ping(Node) of
		pong ->
			spawn(Node,?MODULE,start_remote,[self(),Args]),
%% 			?DEBUG_MSG("Starting remote ~p on ~p.~n",[?MODULE,Node]),
			receive
				Ret -> Ret
			after 5000 -> 
					?ERROR_MSG("Timeout passing traffic to worker node (~p).  Starting locally.~n",[Node]),
					do_start(Args)
			end;
		pang ->
			?CRITICAL("Could not find node: ~p.  Starting locally.~n",[Node]),
			do_start(Args)
	end.

do_start(Args) ->
	gen_fsm:start_link(?MODULE,Args,[{debug,[log]}]).


start_remote(Parent,Args) ->
	Ret = do_start(Args),
	erlang:send(Parent,Ret),
%% 	?DEBUG_MSG("~p started on node ~p.~n",[?MODULE,node()]),
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
%% init({balance,Pool}) ->
	
init(Args) ->
	Filters = proplists:get_value(stream_filters,Args#proxy_pass.config,[]),
	FilterRef = filter_stream:init_filter_list(Filters),
	filter_stream:process_hooks(info,{proxy_pass_config,Args#proxy_pass.config},FilterRef),
    {ok, proxy_start, Args#proxy_pass{filters=FilterRef,keepalive=0,gzbuff= <<>> }}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
proxy_start({socket,CSock},State) ->
	gen_fsm:send_event(self(),request),
%% 	?DEBUG_MSG("peername: ~p~n",[gen_socket:peername(CSock)]),
	{ok,Peer} = gen_socket:peername(CSock),
	filter_stream:process_hooks(request,{request_peer,Peer},State#proxy_pass.filters),
	{next_state,client_send_11,State#proxy_pass{client_sock=CSock}};
proxy_start({reverse_proxy,CSock,{host,_Host,_Port}=Addr}=_L,State) ->
%% 	?DEBUG_MSG("Starting reverse proxy for: ~p~n",[Addr]),
	{ok,Peer} = gen_socket:peername(CSock),
	filter_stream:process_hooks(request,{request_peer,Peer},State#proxy_pass.filters),
	gen_fsm:send_event(self(),request),
	{next_state,client_send_11,State#proxy_pass{client_sock=CSock,reverse_proxy_host=Addr}};
proxy_start({error,_,_,_}=Err,State) ->
	gen_fsm:send_event(self(),Err),
	{next_state,proxy_error,State}.

client_send_11(request,State) ->
	try
		case proxy_read_request:start(State#proxy_pass.client_sock) of
			http_connect ->
				{next_state,client_send_11,State};
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
		_:{killed,_} -> {stop,normal,State};
 		_:ErrCatch ->
 			?ERROR_MSG("Error receiving headers: ~p (Keepalive: ~p)~n",[ErrCatch,State#proxy_pass.keepalive]),
 			{stop,normal,State}
	end;
client_send_11({request_header,#header_block{expect='100-continue'}=_ReqHdr,_RequestSize}=_R,State) ->
	?ERROR_MSG("Error: Expect: 100-continue is unsupported~n",[]),
	gen_fsm:send_event(self(),{error,417,"Expectation Failed",""}),
	{next_state,proxy_error,State};
client_send_11({request_header,ReqHdr,_RequestSize}=_R,State0) ->
	State = State0#proxy_pass{request=ReqHdr},
%% 	?DEBUG_MSG("request_header: ~p~n",[R]),
	case ReqHdr#header_block.request of
		#request_rec{method="CONNECT"} ->
			case proxy_connect:http_connect(State) of
				ok ->
					?ACCESS_LOG(200,(State#proxy_pass.request)#header_block.rstr,State#proxy_pass.userinfo,"Connection Established"),
					{stop,normal,State};
				_ ->
					%% send_event() should be done by proxy_connect:http_connect()
					{next_state,proxy_error,State}
			end;
		_ ->
			Dict = proxylib:header2dict((State#proxy_pass.request)#header_block.headers),
			ReqHdr = State#proxy_pass.request,
			Via = io_lib:format("Via: ~s ~s (Surrogate ~p)",[((State#proxy_pass.request)#header_block.request)#request_rec.protocol,net_adm:localhost(),node()]),
			Hdr0 = proxylib:remove_headers(["keep-alive","proxy-connection","proxy-authorization","accept-encoding"],ReqHdr#header_block.headers),
			Hdr1 = 
			case proplists:get_value(enable_gzip,State#proxy_pass.config,false) of
				false -> Hdr0;
				_EnableGzip ->
					proxylib:append_header("Accept-encoding: gzip",Hdr0)
			end,
			Hdr = proxylib:append_header(Via, Hdr1),
			Req=ReqHdr#header_block.request,
			ReqStr = lists:flatten(io_lib:format("~s ~s ~s",[Req#request_rec.method,Req#request_rec.path,Req#request_rec.protocol])),
			RequestHeaders = lists:flatten([[ReqStr|"\r\n"]|proxylib:combine_headers(Hdr)]),
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
				{host,Host0,Port} ->
					try
						{Host,InetVer} = 
							case proplists:get_value(inet6,State#proxy_pass.config,false) of 
								true ->
									case proxylib:inet_getaddr(Host0) of
										{ip,Addr} -> {Addr,proxylib:inet_version(Addr)};
										Addr -> {Addr,proxylib:inet_version(Addr)}
									end;
								false ->
									{Host0,inet}
							end,
						case gen_tcp:connect(Host,Port,[binary,InetVer,{active,false}],20000) of
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
						end
					catch
						_:CErr ->
							?ERROR_MSG("Error doing gen_tcp:connect(~p,~p): ~p (ip: ~p)~n",[Host0,Port,CErr,proxylib:inet_getaddr(Host0)]),
							CEMsg = io_lib:format("Internal proxy error connecting to host: ~p",[CErr]),
							gen_fsm:send_event(self(),{error,503,lists:flatten(CEMsg)}),
							{next_state,proxy_error,State}
					end;
				Err ->
					?ERROR_MSG("Could not determine backend server: ~p~n",[Err]),
					EMsg = io_lib:format("Internal proxy error: ~p",[Err]),
					gen_fsm:send_event(self(),{error,503,lists:flatten(EMsg)}),
					{next_state,proxy_error,State}
			end
	end;
client_send_11({request_data,Data},State) ->
	gen_socket:send(State#proxy_pass.server_sock,Data),
	proxy_read_request:get_next(State#proxy_pass.request_driver),
	{next_state,client_send_11,State};
client_send_11({end_request_data,_Size},State) ->
	gen_fsm:send_event(self(),response),
	{next_state,server_recv_11,State};
client_send_11({request_filter_response,Data},State) ->
	case State#proxy_pass.request_driver of
		{Mod,_} ->
			Mod:stop(State#proxy_pass.request_driver);
		_ -> ok
	end,
	gen_socket:send(State#proxy_pass.client_sock,Data),
	gen_socket:close(State#proxy_pass.client_sock),
	{stop,normal,State};
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
	RHdr = ResHdr#header_block.headers,
	ResponseHeaders = [[ResHdr#header_block.rstr|"\r\n"]|proxylib:combine_headers(RHdr)],
	gen_socket:send(State#proxy_pass.client_sock,ResponseHeaders),
%% 	?DEBUG_MSG("Headers sent to client. (~p)",[ResponseSize]),
	case ResponseSize of
		0 ->
			gen_fsm:send_event(self(),next),
			{next_state,proxy_finish,State};
		_ ->
			proxy_read_response:get_next(State#proxy_pass.response_driver),
			{next_state,server_recv_11,State#proxy_pass{response=ResHdr,response_bytes_left=ResponseSize}}
	end;
%% response_bytes_left == close when connection: close set for http/1.0 force chunk encoding
server_recv_11({response_data,Data},State) when State#proxy_pass.response_bytes_left == close ->
	gen_socket:send(State#proxy_pass.client_sock,Data),
	proxy_read_response:get_next(State#proxy_pass.response_driver),
	{next_state,server_recv_11,State};
server_recv_11({end_response_data,_Size},State) when State#proxy_pass.response_bytes_left == close ->
%% 	?DEBUG_MSG("Connection closed: ~p (~p bytes)~n",[self(),Size]),
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
%% 	?DEBUG_MSG("Chunk end: ~p~n",[self()]),
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
%% 		{ok,"keep-alive"} when State#proxy_pass.keepalive >= 10 ->
%% 			?DEBUG_MSG("Proxy-Connection: keep-alive Closing (~p)~n",[State#proxy_pass.keepalive]),
%% 			gen_socket:close(State#proxy_pass.server_sock),
%% 			gen_socket:close(State#proxy_pass.client_sock),
%% 			{stop,normal,State};
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
	end;
proxy_finish(Msg,State) ->
	?WARN_MSG("Got unexpected extra message in proxy_finish state: ~p~n",[Msg]),
	{next_state,proxy_finish,State}.
			
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
	{stop,normal,State};
proxy_error(Msg,State) ->
	?WARN_MSG("Received unexpected event in proxy_error state: ~p~n",[Msg]),
	{next_state,proxy_error,State}.

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
	case filter_stream:process_hooks(response,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I ->
%% 			?DEBUG_MSG("Got response header in state ~p~n~p~n",[StateName,I]),
			gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({gzip_response_data,GzData0},StateName,State) ->
	GzBuff = State#proxy_pass.gzbuff,
	GzData = <<GzBuff/binary,GzData0/binary>>,
	case
		try
			zlib:gunzip(GzData)
		catch
			_:Err ->
				Err
		end of
		Data when is_binary(Data) ->
%% 			?DEBUG_MSG("gzip_response_data OK! (~p bytes)~n",[trunc(bit_size(Data)/8)]),
			self() ! {response_data,Data},
			{next_state,StateName,State#proxy_pass{gzbuff = <<>>}};
		_Other ->
%% 			?DEBUG_MSG("gzip_response_data error: ~p~n",[Other]),
			proxy_read_response:get_next(State#proxy_pass.response_driver),
			{next_state,StateName,State#proxy_pass{gzbuff = GzData}}
	end;

handle_info({response_data,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: {response_data,_} in state ~p~n",[StateName]),
	case filter_stream:process_hooks(response,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I -> gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({end_response_data,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: ~p~n",[I]),
	case filter_stream:process_hooks(response,Dat,StateData#proxy_pass.filters) of
		delay -> ok;
		I -> gen_fsm:send_event(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({filter_delay,Data},StateName,StateData) ->
	gen_fsm:send_event(self(),Data),
	{next_state, StateName, StateData};
handle_info({userinfo,UInfo},StateName,State) ->
%% 	?DEBUG_MSG("Got userinfo: ~p~n",[UInfo]),
	filter_stream:process_hooks(request,UInfo,State#proxy_pass.filters),
	{next_state,StateName,State#proxy_pass{userinfo=UInfo}};
handle_info({request_filter_response,_}=CErr,StateName,State) ->
	gen_fsm:send_event(self(),CErr),
	{next_state,StateName,State};
handle_info(get_request_data,StateName,State) ->
	proxy_read_request:get_next(State#proxy_pass.request_driver),
	{next_state,StateName,State};
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

