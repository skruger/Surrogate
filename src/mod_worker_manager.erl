%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Jan 3, 2011
%%% -------------------------------------------------------------------
-module(mod_worker_manager).

-behaviour(gen_server).
-behaviour(proxy_mod).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1,proxy_mod_start/1,proxy_mod_stop/1]).

-export([add_pool/1,remove_pool/1,list_pools/0,get_name/1,register_node/2,next_worker/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% -record(state, {pool,nodes}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(PoolName) ->
	global:trans({?MODULE,self()},
				 fun() ->
						 PName = get_name(PoolName),
						 case global:whereis_name(PName) of
							 undefined ->
								 ?WARN_MSG("~p not running.  Starting pool ~p.",[?MODULE,PoolName]),
								 gen_server:start_link({global,PName},?MODULE,PoolName,[]);
							 Pid ->
								 erlang:link(Pid),
								 ?WARN_MSG("~p already running for pool ~p.",[?MODULE,PoolName]),
								 {ok,Pid}
						 end end).

proxy_mod_start(_) ->
	mnesia:create_table(worker_pool,[{attributes,record_info(fields,worker_pool)}]),
	mnesia:change_table_copy_type(worker_pool,node(),disc_copies),
	SpecList =
		[{get_name(PoolName),{?MODULE,start_link,[PoolName]},
		  permanent,5000,worker,[]} || PoolName <- list_pools() ],
%% 	Spec = {mod_worker_manager,{mod_worker_manager,start_link,[]},permanent,5000,worker,[]},
	lists:foreach(fun(Spec) ->
						  case supervisor:start_child(surrogate_sup,Spec) of
							  {error,_} = SupErr ->
								  ?CRITICAL("Error starting mod_worker_manager with config: ~p~nError: ~p~n",[Spec,SupErr]);
							  _ -> ok
						  end end,SpecList),
	ok.

proxy_mod_stop(_) ->
	
	ok.

%% ====================================================================
%% Server functions
%% ====================================================================

get_name(Pool) ->
	list_to_atom("worker_pool_"++atom_to_list(Pool)).

add_pool(Pool) ->
	F = fun() ->
				case mnesia:read(worker_pool,Pool) of
					[] ->
						?INFO_MSG("Adding pool: ~p~n",[Pool]),
						mnesia:write(#worker_pool{pool=Pool,nodes=[]});
					_ ->
						ok
				end
		end,
	mnesia:transaction(F).

remove_pool(Pool) ->
	F = fun() -> mnesia:delete({worker_pool,Pool}) end,
	mnesia:transaction(F).
	
list_pools() ->
	F = fun() ->
				Match = #worker_pool{pool='$1',_='_'},
				mnesia:select(worker_pool,[{Match,[],['$1']}])
		end,
	case mnesia:transaction(F) of
		{atomic,R} -> R;
		{aborted,Err} -> {error,Err}
	end.

register_node(Pool,Node) ->
	gen_server:call({global,get_name(Pool)},{register_node,Node}).

next_worker(Pool) ->
	gen_server:call({global,get_name(Pool)},{next_worker}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(PoolName) ->
	self() ! check_workers,
	F1 = fun() ->
				 [P|_] = mnesia:read(worker_pool,PoolName),
				 P
		 end,
	case mnesia:transaction(F1) of
		{atomic,State0} ->
			State = State0#worker_pool{idx=0},
			mnesia:transaction(fun() -> mnesia:write(State) end),
			{ok,State};
		Err ->
			{error,Err}
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
handle_call({register_node,Node},_From,State) ->
%% 	?ERROR_MSG("Registering node with ~p: ~p~n",[?MODULE,Node]),
	case lists:any(fun(E) -> E == Node end,State#worker_pool.nodes) of
		true ->
			{reply,ok,State};
		_ ->
			Rec = State#worker_pool{nodes = [Node|State#worker_pool.nodes]},
			mnesia:transaction(fun() -> mnesia:write(Rec) end),
			{reply,ok,Rec}
	end;
handle_call({next_worker},_From,State) when State#worker_pool.nodes == [] ->
	{reply,{error,noworkers},State};
handle_call({next_worker},_From,#worker_pool{nodes=Nodes,idx=Idx}=State) ->
	NewIdx = (Idx rem length(Nodes)) +1,
	Node = lists:nth(NewIdx,Nodes),
	{reply,{ok,Node},State#worker_pool{idx=NewIdx}};
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
handle_cast({check_workers,[]},State) ->
%% 	?ERROR_MSG("Done checking workers...~p~n",[State#worker_pool.nodes]),
	erlang:send_after(5000,self(),check_workers),
	{noreply,State};
handle_cast({check_workers,[Node|R]},State) ->
	gen_server:cast(self(),{check_workers,R}),
%% 	?WARN_MSG("Checking worker ~p~n",[Node]),
	Worker = {mod_worker:get_name(State#worker_pool.pool),Node},
	case catch gen_server:call(Worker,ping,1000) of
		pong ->
			{noreply, State};
		_ ->
			?WARN_MSG("Removing worker ~p from pool ~p.",[Node,State#worker_pool.pool]),
			Rec = State#worker_pool{nodes=lists:filter(fun(N) -> N /= Node end,State#worker_pool.nodes)},
			mnesia:transaction(fun() -> mnesia:write(Rec) end),
			{noreply,Rec}
	end;
	
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(check_workers,State) ->
%% 	?ERROR_MSG("State: ~p~n",[State]),
	gen_server:cast(self(),{check_workers,State#worker_pool.nodes}),
	{noreply,State};
handle_info({nodedown,Node},State) ->  %% Received after monitor_node()
	?WARN_MSG("Received nodedown: ~p~n",[Node]),
	
	{noreply,State};
handle_info(Info, State) ->
	?DEBUG_MSG("Got unexpected info: ~p~n",[Info]),
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

