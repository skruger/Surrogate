%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 1, 2010
%%% -------------------------------------------------------------------
-module(gen_socket).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([create/2,destroy/1,send/2,recv/2,recv/3,setopts/2,getopts/2,close/1,controlling_process/2,peername/1,info/1]).

-export([set_listener/2,set_direction/2,upgrade_ssl/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket,type,controlling_process,controlling_process_mon,listener,tx_bytes,rx_bytes}).

%% ====================================================================
%% External functions
%% ====================================================================

set_listener({?MODULE,Sock},Listener) ->
	gen_server:call(Sock,{set_listener,Listener}).

set_direction({?MODULE,Sock},front) ->
	gen_server:call(Sock,{set_direction,front_tx_bytes,front_rx_bytes});
set_direction({?MODULE,Sock},back) ->
	gen_server:call(Sock,{set_direction,back_tx_bytes,back_rx_bytes}).


create(Socket,Type)  -> %% when (Type == gen_tcp) or (Type == ssl)
%% 	?DEBUG_MSG("gen_socket:create(~p,~p)~n",[Socket,Type]),
	case gen_server:start(?MODULE,[Socket,Type,self()],[]) of
		{ok,Pid} ->
			ok = Type:controlling_process(Socket,Pid),
			GenSock = {?MODULE,Pid},
			{ok,GenSock};
		Err ->
			Err
	end.

destroy({?MODULE,Sock}) ->
	gen_server:call(Sock,{destroy,self()}).

controlling_process({?MODULE,Sock},Pid) ->
	gen_server:call(Sock,{controlling_process,Pid}).

setopts({?MODULE,Sock},Options) ->
	gen_server:call(Sock,{setopts,Options}).

getopts({?MODULE,Sock},OptName) ->
	gen_server:call(Sock,{getopts,OptName}).

send({?MODULE,Sock},Packet) ->
	gen_server:call(Sock,{send,Packet}).

recv(Sock,Length) ->
	recv(Sock,Length,infinity).

recv({?MODULE,Sock},Length,Timeout) ->
	gen_server:call(Sock,{recv,Length,Timeout},Timeout).

close({?MODULE,Sock}) ->
	try
	
		gen_server:call(Sock,{close})
	catch
		_:{R,_} when (R == noproc) or (R == normal) ->
%% 			?DEBUG_MSG("Close error ~p already gone returning ok.~n",[?MODULE]),
			ok;
		_:Err ->
			?WARN_MSG("~p:close() error: ~p~n",[?MODULE,Err]),
			{error,Err}
	end.


peername({?MODULE,Sock}) ->
	gen_server:call(Sock,{peername}).

info({?MODULE,Sock}) ->
	gen_server:call(Sock,info).

upgrade_ssl({?MODULE, Sock}, Opts) ->
  gen_server:call(Sock, {upgrade_ssl, Opts}).

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
init([Socket,Type,Pid]) ->
	Mon = erlang:monitor(process,Pid),
	{ok, #state{socket=Socket,type=Type,controlling_process=Pid,controlling_process_mon=Mon}}.

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
handle_call({upgrade_ssl, Opts}, _From, State) when State#state.type == gen_tcp ->
  case ssl:ssl_accept(State#state.socket, Opts,10000) of
    {ok, SSLSocket} ->
      {reply, ok, State#state{type=ssl, socket=SSLSocket}};
    {error, Err} ->
      ?ERROR_MSG("Error upgrading socket to SSL: ~p~n", [Err]),
      {reply, {error, Err}, State}
  end;
handle_call({upgrade_ssl, Opts}, _From, State) when State#state.type == ssl ->
  {reply, {error, already_ssl}, State};
handle_call({destroy,Pid},From,State) when State#state.type == gen_tcp ->
	erlang:demonitor(State#state.controlling_process_mon),
	ok = gen_tcp:controlling_process(State#state.socket,Pid),
	Reply = {ok,State#state.type,State#state.socket},
	gen_server:reply(From,Reply),
	{stop,normal,State};
handle_call(info,From,State) ->
	?INFO_MSG("Socket info requested by ~p~n~p~n",[From,State]),
	{reply,State,State};
handle_call({controlling_process,Pid},_From,State) when is_pid(Pid) ->
	erlang:demonitor(State#state.controlling_process_mon),
	Mon = erlang:monitor(process,Pid),
	{reply,ok,State#state{controlling_process=Pid,controlling_process_mon=Mon}};
%% handle_call(R,From,State) when State#state.controlling_process /= From ->
%% 	?ERROR_MSG("Error, request made by non-controlling_process (~p)~n~p~n~p~n",[From,State,R]),
%% 	{reply,{error,eperm},State};
handle_call({setopts,Opt},_From,State) when State#state.type == ssl ->
	R = ssl:setopts(State#state.socket,Opt),
	{reply,R,State};
handle_call({getopts,Opt},_From,State) when State#state.type == ssl ->
	R = ssl:getopts(State#state.socket,Opt),
	{reply,R,State};
handle_call({send,Packet},_From,State) when State#state.type == ssl ->
	R = ssl:send(State#state.socket,Packet),
	DataLen = if is_binary(Packet) -> byte_size(Packet); true -> catch byte_size(iolist_to_binary(Packet)) end,
	surrogate_stats:add_counter(State#state.listener,State#state.tx_bytes,DataLen),
	{reply,R,State};
handle_call({recv,Len,Timeout},_From,State) when State#state.type == ssl ->
	R = ssl:recv(State#state.socket,Len,Timeout),
	case R of
		{ok,Data} ->
			DataLen = byte_size(Data),
			surrogate_stats:add_counter(State#state.listener,State#state.rx_bytes,DataLen);
		_ -> ok
	end,
	{reply,R,State};
handle_call({peername},_From,State) when State#state.type == ssl ->
	R = ssl:peername(State#state.socket),
	{reply,R,State};
handle_call({close},_From,State) when State#state.type == ssl ->
	R = ssl:close(State#state.socket),
	gen_server:cast(self(),stop),
	{reply,R,State};
handle_call({setopts,Opt},_From,State) when State#state.type == gen_tcp ->
	R = inet:setopts(State#state.socket,Opt),
	{reply,R,State};
handle_call({getopts,Opt},_From,State) when State#state.type == gen_tcp ->
	R = inet:getopts(State#state.socket,Opt),
	{reply,R,State};
handle_call({send,Packet},_From,State) when State#state.type == gen_tcp ->
	R = gen_tcp:send(State#state.socket,Packet),
	DataLen = if is_binary(Packet) -> byte_size(Packet); true -> catch byte_size(iolist_to_binary(Packet)) end,
	surrogate_stats:add_counter(State#state.listener,State#state.tx_bytes,DataLen),
	{reply,R,State};
handle_call({recv,Len,Timeout},_From,State) when State#state.type == gen_tcp ->
	R = gen_tcp:recv(State#state.socket,Len,Timeout),
	case R of
		{ok,Data} ->
			DataLen = byte_size(Data),
			surrogate_stats:add_counter(State#state.listener,State#state.rx_bytes,DataLen);
		_ -> ok
	end,
	{reply,R,State};
handle_call({peername},_From,State) when State#state.type == gen_tcp ->
	R = inet:peername(State#state.socket),
	{reply,R,State};
handle_call({close},_From,State) when State#state.type == gen_tcp ->
	try
	R = gen_tcp:close(State#state.socket),
%%  	?DEBUG_MSG("Close ~p~n",[R]),
%% 	gen_server:cast(self(),stop),
	{reply,R,State}
	catch
		_:Err ->
			?ERROR_MSG("Error closing socket: ~p~n",[Err]),
			{reply,{error,Err}}
	end;
handle_call({set_listener,Listen},_From,State) ->
%% 	?ERROR_MSG("Setting listener: ~p~n",[Listen]),
	{reply,ok,State#state{listener=Listen}};
handle_call({set_direction,Tx,Rx},_From,State) ->
%% 	?ERROR_MSG("Setting tx: ~p, rx: ~p~n",[Tx,Rx]),
	{reply,ok,State#state{tx_bytes=Tx,rx_bytes=Rx}}.


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop,State) ->
	{stop,normal,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({T,_Socket,Data},State) when (T == ssl) or (T == tcp) ->
	DataLen = byte_size(Data),
	surrogate_stats:add_counter(State#state.listener,State#state.rx_bytes,DataLen),
	P = {?MODULE,{?MODULE,self()},Data},
	State#state.controlling_process ! P,
	{noreply,State};
handle_info({T,_Socket},State) when (T==tcp_closed) or (T == ssl_closed) ->
%% 	?DEBUG_MSG("~p: exiting.~n",[T]),
	State#state.controlling_process ! {gen_socket_closed,{?MODULE,self()}},
	{stop,normal,State};
handle_info({T,_Socket,Reason}=P,State) when (T==tcp_error) or (T == ssl_error) ->
	State#state.controlling_process ! {gen_socket_error,{?MODULE,self()},Reason},
	?WARN_MSG("~p closing: ~p exiting.~n",[?MODULE,P]),
	{noreply,State};
handle_info({'DOWN',_,process,Pid,_Reason},State) when State#state.controlling_process == Pid ->
%% 	?DEBUG_MSG("~p owner (~p) exited.  Closing ~p~n",[?MODULE,Pid,self()]),
	{stop,normal,State};
handle_info(Info, State) ->
	?INFO_MSG("Unexpected info in ~p: ~p~n~p~n",[?MODULE,Info,State]),
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

