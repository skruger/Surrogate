%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Oct 30, 2010
%%% -------------------------------------------------------------------
-module(proxy_transparent_listener).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {listen_args,sock,headers,request,recv_buff}).

%% ====================================================================
%% External functions
%% ====================================================================


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
init(Args) ->
    Self = self(),
	F = fun() -> do_accept(Self,Args#proxy_listener.listen_sock) end,
	spawn_link(F),
    {ok, #state{listen_args =Args,headers=[],request=none,recv_buff=[]}}.


do_accept(Parent,Listen) ->
	case gen_tcp:accept(Listen,300000) of
		{ok,Sock0} ->
			case gen_socket:create(Sock0,gen_tcp) of
				{ok,Sock} ->
					gen_socket:controlling_process(Sock,Parent),
					gen_server:cast(Parent,{accept,{ok,Sock}});
				Err1 ->
					?ERROR_MSG("error creating gen_socket: ~p~n",[Err1]),
					gen_server:cast(Parent,{accept,Err1})
			end;
		Err ->
			gen_server:cast(Parent,{accept,Err})
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
handle_call(Request, _From, State) ->
    Reply = ok,
	?DEBUG_MSG("~p: Unknown call: ~p~n",[?MODULE,Request]),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({accept,{ok,Sock}},State) ->
	gen_server:cast((State#state.listen_args)#proxy_listener.parent_pid,{child_accepted,self()}),
	{ok,Pid} = proxy_pass:start(#proxy_pass{config=(State#state.listen_args)#proxy_listener.config}),
	gen_socket:controlling_process(Sock,Pid),
	gen_fsm:send_event(Pid,{socket,Sock}),
	{stop,normal,State};
handle_cast({accept,{error,timeout}},State) ->
	gen_server:cast((State#state.listen_args)#proxy_listener.parent_pid,{child_accepted,self()}),
	{stop,normal,State};
handle_cast({accept,Other},State) ->
	?ERROR_MSG("Error in accept: ~p~n",[Other]),
	gen_server:cast((State#state.listen_args)#proxy_listener.parent_pid,{child_accepted,self()}),
	{stop,normal,State};

handle_cast(Msg, State) ->
	?DEBUG_MSG("~p: Unknown cast: ~p~n",[?MODULE,Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
	?DEBUG_MSG("~p: Unknown info: ~p~n",[?MODULE,Info]),
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

