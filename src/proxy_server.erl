%% Copyright
-module(proxy_server).
-author("skruger").

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm
-export([init/1, handle_event/3,
  handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([server_response/2,server_request/3]) .

-include("surrogate.hrl").

%% API
start_link(ProxyTxn) ->
  gen_fsm:start_link(?MODULE, ProxyTxn, []).

%% gen_fsm callbacks
%% -record(state, {}).

init(#proxy_txn{end2end_req_headers=Hdr0, request=ReqHdr}=ProxyTxn) ->
  ListenerName = proplists:get_value(surrogate_listener_name,ProxyTxn#proxy_txn.config),
  surrogate_stats:add_counter(ListenerName,requests,1),
  Hdr1 =
    case proplists:get_value(enable_gzip,ProxyTxn#proxy_txn.config,false) of
      false -> Hdr0;
      _EnableGzip ->
        Hdr0++[{'Accept-Encoding',"gzip"}]
    end,
  Hdr = Hdr1,
  Req=ReqHdr#header_block.request,
  {ProtoMajor,ProtoMinor} = Req#request_rec.protocol,
  ReqStr = io_lib:format("~s ~s HTTP/~p.~p\r\n",[Req#request_rec.method,Req#request_rec.path,ProtoMajor,ProtoMinor]),
  ReqHeaders = [ReqStr,proxylib:build_header_list(Hdr)],
  RequestHeaders = iolist_to_binary(ReqHeaders),
%% 			?ERROR_MSG("RequestHeaders:~n~p~n",[RequestHeaders]),
  case proxy_protocol:tcp_connect(ProxyTxn#proxy_txn.reverse_proxy_host) of
    {ok,SSock} ->
      gen_socket:set_listener(SSock,ListenerName),
      gen_socket:set_direction(SSock,back),
      gen_socket:send(SSock,RequestHeaders),
      proxy_read_request:get_next(ProxyTxn#proxy_txn.request_driver),
%%       ?ERROR_MSG("~p:init(~p) ok", [?MODULE, ProxyTxn]),
      {ok,server_request,ProxyTxn#proxy_txn{server_sock=SSock}};
    {error,ErrMsg} ->
      ?INFO_MSG("Could not connect to reverse proxy host: ~p~n",[ProxyTxn#proxy_txn.reverse_proxy_host]),
      gen_fsm:send_event(ProxyTxn#proxy_txn.proxy_client_pid,{error,503,lists:flatten(ErrMsg)}),
      {stop, normal}
  end.

server_request({request_data, Data}, _From, State) ->
  gen_socket:send(State#proxy_txn.server_sock, Data),
  {reply, ok, server_request, State};

server_request({end_request_data, _Size}, _From, State) ->
  gen_fsm:send_event(self(), start_response),
%%   ?ERROR_MSG("~p transitioning to server_response.~n",[?MODULE]),
  {reply, ok, server_response, State}.

server_response(start_response, State) ->
%%   ?ERROR_MSG("~p start_response~n",[?MODULE]),
  case proxy_read_response:start(State#proxy_txn.server_sock, State#proxy_txn.request) of
    {proxy_read_response, _} = RDrv ->
      {next_state,server_response,State#proxy_txn{response_driver=RDrv}};
    Err ->
      ?ERROR_MSG("Error starting proxy_read_response: ~p~n",[Err]),
      EMsg = io_lib:format("Internal proxy error: ~p",[Err]),
      gen_fsm:send_event(self(),{error,503,lists:flatten(EMsg)}),
      {stop,normal, State}
  end;
server_response({response_header,ResHdr,Size}, State) ->
%%   ?ERROR_MSG("~p response_header~n~p~n",[?MODULE,ResHdr]),
  case gen_fsm:sync_send_event(State#proxy_txn.proxy_client_pid, {response_header, ResHdr, Size}) of
    websocket_bridge ->
      ProxyClient = State#proxy_txn.proxy_client_pid,
      ServerSock = State#proxy_txn.server_sock,
      gen_socket:controlling_process(ServerSock, ProxyClient),
      gen_fsm:send_event(ProxyClient, {bridge, ServerSock}),
      {stop, normal, State};
    stop ->
      gen_socket:close(State#proxy_txn.server_sock),
      {stop, normal, State};
    ok ->
      proxy_read_response:get_next(State#proxy_txn.response_driver),
      {next_state, server_response, State}
  end;

server_response({response_data, Data}, State) ->
  case gen_fsm:sync_send_event(State#proxy_txn.proxy_client_pid, {response_data, Data}) of
    get_next ->
      proxy_read_response:get_next(State#proxy_txn.response_driver),
      {next_state, server_response, State};
    stop ->
      case proxy_read_response:stop(State#proxy_txn.response_driver) of
        {ok, Sock} ->
          gen_socket:close(Sock);
        _ -> ok
      end,
      {stop, normal, State}
  end;
server_response({end_response_data, _Size}=ERD, State) ->
  case gen_fsm:sync_send_event(State#proxy_txn.proxy_client_pid, ERD) of
    stop ->
      {stop, normal, State}
  end.


handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info({response_header,_,_} = ResHdr, StateName, State) ->
  gen_fsm:send_event(self(),ResHdr),
  {next_state, StateName, State};
handle_info({response_data,_} = ResData, StateName, State) ->
  gen_fsm:send_event(self(),ResData),
  {next_state, StateName, State};
handle_info({end_response_data,_} = ResData, StateName, State) ->
  gen_fsm:send_event(self(),ResData),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

