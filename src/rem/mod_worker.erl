-module(mod_worker).

-behavior(proxy_mod).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([proxy_mod_start/1,proxy_mod_stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_name/1,start_link/1]).

-record(state,{name}).
%%
%% API Functions
%%

proxy_mod_start(Conf) ->
	case proplists:get_value(manager_nodes,Conf,none) of
		none ->
			ok;
		Nodes when is_list(Nodes) ->
			?INFO_MSG("Adding extra_db_nodes: ~p~n",[Nodes]),
			mnesia:change_config(extra_db_nodes,Nodes);
		Node when is_atom(Node) ->
			?INFO_MSG("Adding extra_db_nodes: ~p~n",[Node]),
			mnesia:change_config(extra_db_nodes,[Node])
	end,
	Pools = proplists:get_value(pools,Conf,[]),
	SpecList =
		[{get_name(PoolName),{?MODULE,start_link,[PoolName]},
		  permanent,5000,worker,[]} || PoolName <- Pools ],
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

get_name(PoolName) ->
	list_to_atom("worker_pool_node_"++atom_to_list(PoolName)).

start_link(Pool) ->
	PName = get_name(Pool),
	gen_server:start_link({local,PName},?MODULE,Pool,[]).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(PoolName) ->
	self() ! reregister,
	{ok, #state{name=PoolName}}.

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
handle_call(ping,_From,State) ->
	{reply,pong,State};
handle_call(stop,From,State) ->
	gen_server:reply(From,stop),
	{stop,normal,State};
handle_call(state,_From,State) ->
	{reply,State,State};
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
handle_info(reregister,State) ->
	mod_worker_manager:register_node(State#state.name,node()),
%% 	?WARN_MSG("Trying to reregister node: ~p~n",[node()]),
	erlang:send_after(10000,self(),reregister),
	{noreply,State};
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


