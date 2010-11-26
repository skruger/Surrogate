%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 5, 2010
%%% -------------------------------------------------------------------
-module(proxy_socks45).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

-define(LISTENERS, 1).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listener,num_listeners,listeners}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(PropList) ->
	gen_server:start_link({local,?MODULE},?MODULE,[PropList],[]).


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
init([PropList]) ->
   	?DEBUG_MSG("~p starting.~n",[?MODULE]),
	Port = proplists:get_value(listen,PropList,1080),
    case gen_tcp:listen(Port,[inet,binary,{active,true},{reuseaddr,true}]) of
		{ok,ListenSock} ->
			Listener = #proxy_listener{listen_sock = ListenSock,parent_pid=self()},
			gen_server:cast(self(),check_listeners),
			{ok, #state{listener=Listener,num_listeners=?LISTENERS,listeners=[]}};
		Err ->
			?CRITICAL("~p could not start listen(): ~p",[?MODULE,Err]),
			{stop,error}
	end.


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
handle_cast(check_listeners,State)->
	case lists:flatlength(State#state.listeners) of
		Num when Num < State#state.num_listeners ->
			gen_server:cast(self(),startchild);
		_ ->
			true
	end,
	{noreply,State};
handle_cast(startchild,State) ->
%% 	?FQDEBUG("Starting child...~n"),
	gen_server:cast(self(),check_listeners),
	case gen_fsm:start_link(proxy_socks45_listener,State#state.listener,[]) of
		{ok,Pid} ->
			erlang:monitor(process,Pid),
			erlang:register(list_to_atom(lists:append("socks_",pid_to_list(Pid))),Pid),
%% 			?FQDEBUG("Started child ~p~n",[Pid]),
			{noreply,State#state{listeners = [Pid|State#state.listeners]}};
		Err ->
			?ERROR_MSG("Error starting child: ~p~n",[Err]),
			{noreply,State}
	end;
handle_cast({child_accepted,Pid},State) ->
	gen_server:cast(self(),check_listeners),
	{noreply,State#state{listeners = lists:delete(Pid,State#state.listeners)}};

handle_cast(Msg, State) ->
	?DEBUG_MSG("~p recieved unknown cast: ~p~n",[?MODULE,Msg]),
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'DOWN',_,process,Pid,normal},State) ->
	gen_server:cast(self(),{child_accepted,Pid}),
	{noreply,State};
handle_info(Info, State) ->
	?DEBUG_MSG("~p recieved unknown info: ~p~n",[?MODULE,Info]),
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

