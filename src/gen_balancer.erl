%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 25, 2010
%%% -------------------------------------------------------------------
-module(gen_balancer).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").



%% --------------------------------------------------------------------
%% External exports
-export([start_link/3,start/2,is_alive/1,next/2,set_host_state/3,get_host_healthcheckers/1,behaviour_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -record(state, {balancer_mod,balancer_state}).

%% ====================================================================
%% External functions
%% ====================================================================
behaviour_info(callbacks) ->
	[{init,1}, {next,2}];
behaviour_info(_) ->
	undefined.

start_link(Name,Mod,Args) ->
	?DEBUG_MSG("~p ~p ~p~n",[Name,?MODULE,[Mod,Args]]),
	case gen_server:start_link({local,Name},?MODULE,[Mod,Args],[]) of
		{ok,Pid} = OK ->
			case Name of
				{_,Proc} when is_atom(Proc) ->
					register(Proc,Pid);
				_ -> 
					?WARN_MSG("Could not register process name in gen_balancer.  Invalid name returned from proxylib:get_pool_process()~n~p~n",[Name]),
					ok
			end,
			OK;
		Err ->
			Err
	end.

start(Mod,Args) ->
	gen_server:start(Mod,[Mod,Args],[]).

is_alive(Pool) ->
	case whereis(proxylib:get_pool_process(Pool)) of
		Pid when is_pid(Pid) ->
			true;
		_ -> false
	end.

next(Pool,ClientInfo) ->
	Ref = proxylib:get_pool_process(Pool),
	gen_server:call(Ref,{get_next,ClientInfo}).

set_host_state(Pool,Host,Status) ->
	Ref = proxylib:get_pool_process(Pool),
	gen_server:cast(Ref,{set_host_state,Host,Status,self()}).

get_host_healthcheckers(Pool) ->
	Ref = proxylib:get_pool_process(Pool),
	gen_server:call(Ref,get_host_healthcheckers).

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
init([Mod,Args]) ->
	?DEBUG_MSG("Starting: ~p ~p~n",[Mod,Args]),
	Hosts = proplists:get_value(hosts,Args,[]),
	Pool = dict:from_list([{H,unknown} || H <- Hosts]),
	State = #gen_balancer_state{pool=Pool,active_pool=[],healthcheckers=dict:new()},
	gen_server:cast(self(),update_active_pool),
	case apply(Mod,init,[Args]) of
		{ok,LState} ->
			{ok,State#gen_balancer_state{balancer_mod=Mod,local_state=LState}};
		{ok,LState,Timeout} ->
			{ok,State#gen_balancer_state{balancer_mod=Mod,local_state=LState},Timeout};
		Err ->
			Err
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
handle_call({get_next,ClientInfo}, _From, State) ->
	apply(State#gen_balancer_state.balancer_mod,next,[ClientInfo,State]);
%% 	Reply = apply(State#gen_balancer_state.balancer_mod,next,[ClientInfo,State]),
%%     {reply, Reply, State};
handle_call(get_host_healthcheckers,_From,State) ->
	{reply,dict:to_list(State#gen_balancer_state.healthcheckers),State};
handle_call(Err,_From,State) ->
	?ERROR_MSG("Got unknown call: ~p~n",[Err]),
	{reply,ok,State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({set_host_state,Host,Status,WhoUpdated},State) ->
	gen_server:cast(self(),{update_healthcheckers,Host,WhoUpdated}),
	case dict:fetch(Host,State#gen_balancer_state.pool) of
		Status ->
			{noreply,State}; %% Host status unchanged
		_ ->
			NewPool = dict:store(Host,Status,State#gen_balancer_state.pool),
			gen_server:cast(self(),update_active_pool),
			{noreply,State#gen_balancer_state{pool=NewPool}}
	end;
handle_cast(update_active_pool,State) ->
	ActivePool = get_active_hosts(dict:to_list(State#gen_balancer_state.pool),[]),
	?DEBUG_MSG("Active pool: ~p~n",[ActivePool]),
	{noreply,State#gen_balancer_state{active_pool=ActivePool}};
handle_cast({update_healthcheckers,Host,Pid},State) ->
	Checkers = dict:store(Host,Pid,State#gen_balancer_state.healthcheckers),
	{noreply,State#gen_balancer_state{healthcheckers=Checkers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

get_active_hosts([],Acc) ->
	lists:reverse(Acc);
get_active_hosts([{_H,down}|R],Acc) ->
	get_active_hosts(R,Acc);
get_active_hosts([{H,_}|R],Acc) ->
	get_active_hosts(R,[H|Acc]).
	

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

