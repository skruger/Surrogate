%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Oct 30, 2010
%%% -------------------------------------------------------------------
-module(proxy_client).

-behaviour(gen_statem).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1,setproxyaddr/2,addproxyaddr/2,start_socket_processing/2]).

%% gen_statem callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4, callback_mode/0]).

-export([proxy_start/2,client_request/2,http_connect/2,websocket_bridge/2,client_response/3,proxy_error/2,proxy_finish/2]).

%% ,proxy_auth/2,proxy_client_read/2,proxy_connect/2,client_send/2

%% -define(LOG(N,P),lists:flatten(io_lib:format(~p)

%% ====================================================================
%% External functions
%% ====================================================================


start(Args) ->
	gen_statem:start_link(?MODULE,Args,[{debug,[log]}]).

%% Clears and replaces the proxy address list
setproxyaddr(Pid,HostDef) ->
	gen_statem:cast(Pid,{setproxyaddr,HostDef}).

%% Appends to the proxy address list
addproxyaddr(Pid,HostDef) ->
	gen_statem:cast(Pid,{addproxyaddr,HostDef}).


start_socket_processing(Pid,{socket, Sock}) ->
	gen_statem:cast(Pid,{socket,Sock}).

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
	pg2:create(?MODULE),
	pg2:join(?MODULE,self()),
	Filters = proplists:get_value(stream_filters,Args#proxy_txn.config,[]),
	FilterRef = filter_stream:init_filter_list(Filters),
  {ok, proxy_start, Args#proxy_txn{filters=FilterRef,keepalive=0,proxy_client_pid=self(),reverse_proxy_host=[]}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
proxy_start({socket,CSock},State) ->
	gen_statem:cast(self(),request),
%% 	?DEBUG_MSG("peername: ~p~n",[gen_socket:peername(CSock)]),
	case gen_socket:peername(CSock) of
		{ok,Peer} ->
%% 			filter_stream:process_hooks(request,{request_peer,Peer},State#proxy_txn.filters,State),
			{next_state,client_request,State#proxy_txn{client_sock=CSock,request_peer=Peer}};
		Error ->
			?ERROR_MSG("Error getting peername!  No request_peer will be set:~p~n~p~n",[Error,State]),
			{next_state,client_request,State#proxy_txn{client_sock=CSock}}
	end.

client_request(request,State) ->
	try
		case proxy_read_request:start(State#proxy_txn.client_sock) of
			http_connect ->
				{next_state,http_connect,State};
			{proxy_read_request,_} = RDrv ->
				{next_state,client_request,State#proxy_txn{request_driver=RDrv}};
			Err ->
				?ERROR_MSG("Error starting proxy_read_request: ~p~n",[Err]),
				gen_statem:cast(self(),proxylib:proxy_error(503,Err)),
				{next_state,proxy_error,State}
		end
 	catch
 		_:{error,closed} -> {stop,normal,State};
		_:{killed,_} -> {stop,normal,State};
 		_:ErrCatch ->
 			?ERROR_MSG("Error receiving headers: ~p (Keepalive: ~p)~n~p~n",[ErrCatch,State#proxy_txn.keepalive,erlang:get_stacktrace()]),
 			{stop,normal,State}
	end;

client_request({request_header,#header_block{expect='100-continue'}=_ReqHdr,_RequestSize}=_R,State) ->
	?ERROR_MSG("Error: Expect: 100-continue is unsupported~n",[]),
	gen_statem:cast(self(),{error,417,"Expectation Failed",""}),
	{next_state,proxy_error,State};

client_request({request_header,ReqHdr,_RequestSize}=_R,State0) ->

%% 	?DEBUG_MSG("request_header: ~p~n",[R]),
%% 			Via = io_lib:format("Via: ~s ~s (Surrogate ~p)",[((State#proxy_txn.request)#header_block.request)#request_rec.protocol,net_adm:localhost(),node()]),
  %% Move via headers to filter_headers
  {HopHeaders, Hdr0} = proxylib:split_hop_headers(ReqHdr#header_block.headers),
  State = State0#proxy_txn{request=ReqHdr,client_hop_headers=HopHeaders, end2end_req_headers=Hdr0},
  case proxy_server:start_link(State) of
    {ok, SrvPid} ->
      {next_state,client_request, State#proxy_txn{proxy_server_pid=SrvPid}};
    Err ->
      ?INFO_MSG("Could not start proxy_server instance: ~p~n",[Err]),
      ErrMsg = io_lib:format("~p", [Err]),
      gen_statem:cast(self(),proxylib:proxy_error(503,lists:flatten(ErrMsg))),
      {next_state, proxy_error, State}
  end;

client_request({request_data,Data},State) ->
  gen_statem:call(State#proxy_txn.proxy_server_pid, {request_data,Data}),
	proxy_read_request:get_next(State#proxy_txn.request_driver),
	{next_state,client_request,State};

client_request({end_request_data,_Size},State) ->
  gen_statem:call(State#proxy_txn.proxy_server_pid, {end_request_data,_Size}),
  {next_state,client_response,State};

client_request({request_filter_response,Data},State) ->
	case State#proxy_txn.request_driver of
		{Mod,_} ->
			Mod:stop(State#proxy_txn.request_driver);
		_ -> ok
	end,
	gen_socket:send(State#proxy_txn.client_sock,Data),
	gen_socket:close(State#proxy_txn.client_sock),
	{stop,normal,State};

client_request(Extra,State) ->
	?ERROR_MSG("Unknown event in client_request: ~p~nTerminating...",[Extra]),
	{stop,normal,State}.

http_connect({request_header, ReqHdr, _Size}, State0) ->
  State = State0#proxy_txn{request=ReqHdr},
  case proxy_connect:http_connect(State) of
    ok ->
      ?ACCESS_LOG(200,(State#proxy_txn.request)#header_block.rstr,State#proxy_txn.userinfo,"Connection Established"),
      {stop,normal,State};
    _ ->
      %% send_event() should be done by proxy_connect:http_connect()
      {next_state,proxy_error,State}
  end.

websocket_bridge({bridge, ServerSock}, State) ->
  proxy_connect:bridge_client_server(State#proxy_txn.client_sock,ServerSock),
  {stop, normal, State}.


client_response({response_header,ResHdr,ResponseSize}, _From, State) ->
	?ACCESS_LOG(200,(State#proxy_txn.request)#header_block.rstr,State#proxy_txn.userinfo,ResHdr#header_block.rstr),
	#response_rec{code=Code,protocol={VerMajor,VerMinor},text=StatusStr} = ResHdr#header_block.response,
	ResponseStr = io_lib:format("HTTP/~p.~p ~p ~s\r\n",[VerMajor,VerMinor,Code,StatusStr]),
  {_SvrHop, SvrResp0} = proxylib:split_hop_headers(ResHdr#header_block.headers),
  DynamicHeader =
  case ResponseSize of
    close -> [{'Connection', "close"}];
    chunked -> [{'Transfer-Encoding', "chunked"}];
    _ -> []
  end,
  SvrResp = [{'Connection',"close"}|SvrResp0]++DynamicHeader,
	ResHeaders = [ResponseStr,proxylib:build_header_list(SvrResp)],
	ResponseHeaders = iolist_to_binary(ResHeaders),
%% 	?ERROR_MSG("ResponseHeaders:~n~p~n",[ResponseHeaders]),
	gen_socket:send(State#proxy_txn.client_sock,ResponseHeaders),
	case {ResponseSize,Code} of
		{_,101} ->
%% 			?ERROR_MSG("Upgrade to websocket~n",[]),
			{reply, websocket_bridge, websocket_bridge, State};
		{0,_} ->
			gen_statem:cast(self(),next),
			{reply, stop, proxy_finish,State};
		_ ->
%% 			proxy_read_response:get_next(State#proxy_txn.response_driver),
%%       ?ERROR_MSG("ResponseSize=~p~n",[ResponseSize]),
			{reply, ok, client_response,State#proxy_txn{response=ResHdr,response_bytes_left=ResponseSize}}
	end;
%% response_bytes_left == close when connection: close set for http/1.0 force chunk encoding
client_response({response_data,Data}, _From, State) when State#proxy_txn.response_bytes_left == close ->
	gen_socket:send(State#proxy_txn.client_sock,Data),
%% 	proxy_read_response:get_next(State#proxy_txn.response_driver),
	{reply, get_next,client_response,State};
client_response({end_response_data,_Size}, _From, State) when State#proxy_txn.response_bytes_left == close ->
%% 	?DEBUG_MSG("Connection closed: ~p (~p bytes)~n",[self(),Size]),
%% 	gen_socket:close(State#proxy_txn.server_sock),
	{stop,normal,stop,State};
client_response({response_data,<<>>}, _From, State) when State#proxy_txn.response_bytes_left == chunked ->
  ?ERROR_MSG("Supressing empty chunk.~n",[]),
  {reply, get_next, client_response, State};
client_response({response_data,Data}, _From, State) when State#proxy_txn.response_bytes_left == chunked ->
%% 	?ERROR_MSG("Got chunked {response_data,~p}, send out as a chunk.~p~n",[Data, self()]),
	DataLen = list_to_binary(erlang:integer_to_list(trunc(bit_size(Data)/8),16)),
	DataOut = <<DataLen/binary,"\r\n",Data/binary,"\r\n">>,
	gen_socket:send(State#proxy_txn.client_sock,DataOut),
%% 	proxy_read_response:get_next(State#proxy_txn.response_driver),
	{reply, get_next,client_response,State};
client_response({end_response_data,_Size}, _From, State) when State#proxy_txn.response_bytes_left == chunked ->
%% 	?DEBUG_MSG("Chunk end: ~p~n",[self()]),
	gen_socket:send(State#proxy_txn.client_sock,<<"0\r\n\r\n">>),
	gen_statem:cast(self(),next),
	{reply, stop, proxy_finish,State};
client_response({response_data,Data}, _From, State) ->
%% 	?ERROR_MSG("Got {response_data,_}. ~p~n",[self()]),
	gen_socket:send(State#proxy_txn.client_sock,Data),
%% 	proxy_read_response:get_next(State#proxy_txn.response_driver),
	{reply, get_next, client_response, State};
client_response({end_response_data,_Size}, _From, State) ->
%% 	?DEBUG_MSG("Done, Stopping ~p after ~p bytes~n",[self(),Size]),
%% 	gen_socket:close(State#proxy_txn.server_sock),
%% 	gen_socket:close(State#proxy_txn.client_sock),
%% 	{stop,normal,State}.
	gen_statem:cast(self(),next),
	{reply, stop,proxy_finish,State}.
	


proxy_finish(next,State) ->
	Dict = dict:from_list((State#proxy_txn.request)#header_block.headers),
%% 	gen_socket:close(State#proxy_txn.server_sock),
%% 	gen_socket:close(State#proxy_txn.client_sock),
%% 	{stop,normal,State}.
	case dict:find('Proxy-Connection',Dict) of
		{ok,ProxyConn} ->
			case string:to_lower(ProxyConn) of
%% 				"keep-alive" when State#proxy_txn.keepalive >= 10 ->
%% 					?DEBUG_MSG("Proxy-Connection: keep-alive Closing (~p)~n",[State#proxy_txn.keepalive]),
%% 					gen_socket:close(State#proxy_txn.server_sock),
%% 					gen_socket:close(State#proxy_txn.client_sock),
%% 					{stop,normal,State};
				"keep-alive" ->
					gen_statem:cast(self(),request),
%% 					gen_socket:close(State#proxy_txn.server_sock),
					{next_state,client_request,State#proxy_txn{server_sock=undefined,keepalive=State#proxy_txn.keepalive+1}};
				"close" ->
%% 					gen_socket:close(State#proxy_txn.server_sock),
					gen_socket:close(State#proxy_txn.client_sock),
					{stop,normal,State};
				Conn ->
					?DEBUG_MSG("Closing with unknown value \"Connection: ~p\"~n",[Conn]),
%% 					gen_socket:close(State#proxy_txn.server_sock),
					gen_socket:close(State#proxy_txn.client_sock),
					{stop,normal,State}
			end;
		error ->
%% 			gen_socket:close(State#proxy_txn.server_sock),
			gen_socket:close(State#proxy_txn.client_sock),
			{stop,normal,State}
	end;
proxy_finish(Msg,State) ->
	?WARN_MSG("Got unexpected extra message in proxy_finish state: ~p~n",[Msg]),
	{next_state,proxy_finish,State}.
			
proxy_error({error,Code},State) ->
	?DEBUG_MSG("Proxy error: ~p~n",[Code]),
	case Code of
		_ ->
			gen_statem:cast(self(),{error,Code,"General Proxy Failure"}),
			{next_state,proxy_error,State}
	end;
proxy_error({error,Code,Desc},State) ->
	proxy_error({error,Code,"Proxy Error",Desc},State);
proxy_error({error,Code,RText,Desc},State) ->
	?DEBUG_MSG("Proxy error: ~p ~p~n",[Code,Desc]),
	ICode = integer_to_list(Code),
	Err = "HTTP/1.0 "++ICode++" "++RText++"\r\nContent-type: text/html\r\nConnection: close\r\n\r\n",
	EResponse = lists:flatten(io_lib:format("~s<h3>~s - ~s</h3>",[Err,ICode,Desc])),
	?ACCESS_LOG(Code,(State#proxy_txn.request)#header_block.rstr,State#proxy_txn.userinfo,Desc),
	gen_socket:send(State#proxy_txn.client_sock,EResponse),
	gen_socket:close(State#proxy_txn.client_sock),
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
handle_event({setproxyaddr,TargetList}, StateName,StateData) ->
	{next_state,StateName,StateData#proxy_txn{reverse_proxy_host=TargetList}};
handle_event({addproxyaddr,TargetList}, StateName,StateData) ->
	ExistingHosts = StateData#proxy_txn.reverse_proxy_host,
	{next_state,StateName,StateData#proxy_txn{reverse_proxy_host=ExistingHosts++TargetList}};
handle_event(Event, StateName, StateData) ->
	?INFO_MSG("~p:handle_event() received unknown event: ~p~n",[?MODULE,Event]),
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
%% 	?DEBUG_MSG("Processing filters: ~p~n",[StateData#proxy_txn.filters]),
	case filter_stream:process_hooks(request,Dat,StateData#proxy_txn.filters,StateData) of
		delay -> ok;
		I -> gen_statem:cast(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({request_data,_}=Dat,StateName,StateData) ->
%% 	?ERROR_MSG("Sending event: {request_data,_} in state ~p~n",[StateName]),
	case filter_stream:process_hooks(request,Dat,StateData#proxy_txn.filters,StateData) of
		delay -> ok;
		I -> gen_statem:cast(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({end_request_data,_}=Dat,StateName,StateData) ->
	case filter_stream:process_hooks(request,Dat,StateData#proxy_txn.filters,StateData) of
		delay -> ok;
		I ->
%%       ?DEBUG_MSG("Sending event: ~p~n",[I]),
      gen_statem:cast(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({response_header,_,_}=Dat,StateName,StateData) ->
	case filter_stream:process_hooks(response,Dat,StateData#proxy_txn.filters,StateData) of
		delay -> ok;
		I ->
%% 			?DEBUG_MSG("Got response header in state ~p~n~p~n",[StateName,I]),
			gen_statem:cast(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({gzip_response_data_error,_UnzipErr},_StateName,StateData) ->
	{stop,gzip_error,StateData};
handle_info({response_data,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: {response_data,_} in state ~p~n",[StateName]),
	case filter_stream:process_hooks(response,Dat,StateData#proxy_txn.filters,StateData) of
		delay -> ok;
		I -> gen_statem:cast(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({end_response_data,_}=Dat,StateName,StateData) ->
%% 	?DEBUG_MSG("Sending event: ~p~n",[I]),
	case filter_stream:process_hooks(response,Dat,StateData#proxy_txn.filters,StateData) of
		delay -> ok;
		I -> gen_statem:cast(self(),I)
	end,
	{next_state, StateName, StateData};
handle_info({filter_delay,Data},StateName,StateData) ->
	gen_statem:cast(self(),Data),
	{next_state, StateName, StateData};
handle_info({userinfo,UInfo},StateName,State) ->
%% 	?DEBUG_MSG("Got userinfo: ~p~n",[UInfo]),
	filter_stream:process_hooks(request,UInfo,State#proxy_txn.filters,State),
	{next_state,StateName,State#proxy_txn{userinfo=UInfo}};
handle_info({request_filter_response,_}=CErr,StateName,State) ->
	gen_statem:cast(self(),CErr),
	{next_state,StateName,State};
handle_info(get_request_data,StateName,State) ->
	proxy_read_request:get_next(State#proxy_txn.request_driver),
	{next_state,StateName,State};
%% handle_info(get_response_data,StateName,State) ->
%% 	proxy_read_response:get_next(State#proxy_txn.response_driver),
%% 	{next_state,StateName,State};
handle_info({proxy_error, Code, Text}, _StateName, StateData) ->
  gen_statem:cast(self(), {error, Code, Text}),
  {next_state, proxy_error, StateData};
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

callback_mode() ->
    state_functions.
