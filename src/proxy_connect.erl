%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 5, 2010
%%% -------------------------------------------------------------------
-module(proxy_connect).

%% -behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([client_loop/2,server_loop/2,http_connect/1,bridge_client_server/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {client_sock,server_sock}).

%% ====================================================================
%% External functions
%% ====================================================================


http_connect(ProxyPass) ->
  Mode = proplists:get_value(http_connect,ProxyPass#proxy_txn.config, terminate),
  Req = (ProxyPass#proxy_txn.request)#header_block.request,
  Host = Req#request_rec.host,
  Port = Req#request_rec.port,
  http_connect(ProxyPass, Host, Port, Mode).


%% http_connect(ProxyPass, Host, Port, spoof) ->


http_connect(ProxyPass, Host, Port, terminate) ->
	Req = (ProxyPass#proxy_txn.request)#header_block.request,
	Host = Req#request_rec.host,
	Port = Req#request_rec.port,
	?ERROR_MSG("HTTP CONNECT ~s:~p~n", [Host, Port]),
	case gen_tcp:connect(Host,Port,[binary,inet,{active,false}]) of
		{ok,SvrSock0} ->
%% 			?DEBUG_MSG("http_connect() ~p connected:~p~n",[self(),SvrSock0]),
			{ok,SvrSock} = gen_socket:create(SvrSock0,gen_tcp),
			ServerPid = spawn(?MODULE,server_loop,[SvrSock,undefined]),
			ClientPid = spawn(?MODULE,client_loop,[ProxyPass#proxy_txn.client_sock,undefined]),
			gen_socket:send(ProxyPass#proxy_txn.client_sock,<<"HTTP/1.1 200 Connection Established\r\nConnection: keep-alive\r\n\r\n">>),
			gen_socket:controlling_process(SvrSock,ServerPid),
			gen_socket:controlling_process(ProxyPass#proxy_txn.client_sock,ClientPid),
			ServerPid ! {client,ClientPid},
			ClientPid ! {server,ServerPid},
			ok;
		{error,ErrStat} = Err ->
			gen_fsm:send_event(self(),{error,503,lists:flatten(io_lib:format("Error connecting to server: ~p",[ErrStat]))}),
			Err
	end;
http_connect(ProxyPass, Mode) ->
  ?ERROR_MSG("Invalid http_connect mode: ~p~n", [Mode]),
  http_connect(ProxyPass, terminate).

bridge_client_server(ClientSock,ServerSock) ->
	ServerPid = spawn(?MODULE,server_loop,[ServerSock,undefined]),
	ClientPid = spawn(?MODULE,client_loop,[ClientSock,undefined]),
	gen_socket:controlling_process(ServerSock,ServerPid),
	gen_socket:controlling_process(ClientSock,ClientPid),
	ServerPid ! {client,ClientPid},
	ClientPid ! {server,ServerPid},
	ok.
	
  

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

client_loop(ClientSock,ServerPid) when ServerPid == undefined ->
	receive
		{server,Pid} ->
			gen_socket:setopts(ClientSock,[{active,once}]),
			client_loop(ClientSock,Pid);
		Other ->
			?ERROR_MSG("client_loop() bad receive: ~p~n",[Other]),
			client_loop(ClientSock,ServerPid)
	end;
client_loop(Sock,SvrPid) ->
	receive
		{gen_socket,Sock,Data} ->
			gen_socket:setopts(Sock,[{active,once}]),
			SvrPid ! {client_tcp,Data},
			client_loop(Sock,SvrPid);
		{server_tcp,Data} ->
			gen_socket:send(Sock,Data),
			client_loop(Sock,SvrPid);
		{gen_socket_closed,Sock} ->
			SvrPid ! client_closed,
			ok;
		server_closed ->
			gen_socket:close(Sock),
			ok;
		{gen_socket_error,Sock,Reason} ->
			?DEBUG_MSG("Socket closed: ~p~n",[Reason]),
			SvrPid ! client_closed;
		Other ->
			?ERROR_MSG("Unexpected data in client_loop(): ~p~n",[Other]),
			client_loop(Sock,SvrPid)
	end.
		
server_loop(ServerSock,ClientPid) when ClientPid == undefined ->
	receive
		{client,Pid} ->
			gen_socket:setopts(ServerSock,[{active,once}]),
			server_loop(ServerSock,Pid);
		Other ->
			?ERROR_MSG("server_loop() bad receive: ~p~n",[Other]),
			server_loop(ServerSock,ClientPid)
	end;
server_loop(Sock,CliPid) ->
	receive
		{gen_socket,Sock,Data} ->
			gen_socket:setopts(Sock,[{active,once}]),
			CliPid ! {server_tcp,Data},
			server_loop(Sock,CliPid);
		{client_tcp,Data} ->
			gen_socket:send(Sock,Data),
			server_loop(Sock,CliPid);
		{gen_socket_closed,Sock} ->
			CliPid ! server_closed,
			ok;
		client_closed ->
			gen_socket:close(Sock),
			ok;
		{gen_socket_error,Sock,Reason} ->
			?DEBUG_MSG("Server socket closed: ~p~n",[Reason]),
			CliPid ! server_closed;
		Other ->
			?ERROR_MSG("Unexpected data in server_loop(): ~p~n",[Other]),
			server_loop(Sock,CliPid)
	end.

