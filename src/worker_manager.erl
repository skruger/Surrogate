%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Jan 3, 2011
%%% -------------------------------------------------------------------
-module(worker_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,list_active_nodes/0]).

-export([add_pool/1,add_pool_node/2,remove_pool_node/2,remove_node/1,remove_pool/1,list_pools/0,list_pool_nodes/1,list_node_pools/1,set_pool_node_state/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% ====================================================================
%% Server functions
%% ====================================================================

add_pool(Pool) ->
	F = fun() ->
				case mnesia:read(worker_pool,Pool) of
					[] ->
						?INFO_MSG("Adding pool: ~p~n",[Pool]),
						mnesia:write(#worker_pool{pool=Pool,active=true});
					_ ->
						ok
				end
		end,
	mnesia:transaction(F).

add_pool_node(Pool,Node) ->
	try
		case net_adm:ping(Node) of
			pong ->
				ok;
			pang ->
				throw(pang)
		end,
		F = fun() ->
					case mnesia:read(worker_pool,Pool) of
						[] ->
							mnesia:abort(nopool);
						_ ->
							ok
					end,
					case mnesia:read(worker_node,Node) of
						[] ->
							?INFO_MSG("Adding node: ~p~n",[Node]),
							mnesia:write(#worker_node{node=Node,active=true});
						_ ->
							ok
					end,
					case mnesia:read(worker_node_pool,{Pool,Node}) of
						[] ->
							worker_pool:refresh_nodes(Pool),
							mnesia:write(#worker_node_pool{pool_node={Pool,Node},active=true});
						_ ->
							ok
					end
			end,
		mnesia:transaction(F)
	catch
		_:OErr ->
			{error,OErr}
	end.

remove_pool_node(Pool,Node) ->
	F = fun() ->
				worker_pool:refresh_nodes(Pool),
				mnesia:delete({worker_node_pool,{Pool,Node}})
		end,
	mnesia:transaction(F).

remove_node(Node) ->
	F = fun() ->
				mnesia:delete({worker_node,Node})
		end,
	case list_node_pools(Node) of
		[] ->
			mnesia:transaction(F);
		Err ->
			{error,Err}
	end.

remove_pool(Pool) ->
	F = fun() -> mnesia:delete({worker_pool,Pool}) end,
	case list_pool_nodes(Pool) of
		[] ->
			mnesia:transaction(F);
		Err ->
			{error,Err}
	end.

list_pool_nodes(Pool) ->
	F = fun() ->
				Match = #worker_node_pool{pool_node={Pool,'$1'},_='_'},
				mnesia:select(worker_node_pool,[{Match,[],['$1']}])
		end,
	case mnesia:transaction(F) of
		{atomic,R} -> R;
		{aborted,Err} -> {error,Err}
	end.

list_node_pools(Node) ->
	F = fun() ->
				Match = #worker_node_pool{pool_node={'$1',Node},_='_'},
				mnesia:select(worker_node_pool,[{Match,[],['$1']}])
		end,
	case mnesia:transaction(F) of
		{atomic,R} -> R;
		{aborted,Err} -> {error,Err}
	end.

			
list_pools() ->
	F = fun() ->
				Match = #worker_pool{active=true,pool='$1',_='_'},
				mnesia:select(worker_pool,[{Match,[],['$1']}])
		end,
	case mnesia:transaction(F) of
		{atomic,R} -> R;
		{aborted,Err} -> {error,Err}
	end.

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	mnesia:create_table(worker_pool,[{attributes,record_info(fields,worker_pool)}]),
	mnesia:create_table(worker_node,[{attributes,record_info(fields,worker_node)}]),
	mnesia:create_table(worker_node_pool,[{attributes,record_info(fields,worker_node_pool)}]),
	mnesia:change_table_copy_type(worker_pool,node(),disc_copies),
	mnesia:change_table_copy_type(worker_node,node(),disc_copies),
	mnesia:change_table_copy_type(worker_node_pool,node(),disc_copies),
	check_nodes(),
	erlang:send_after(10000,self(),check_workers),
	UpdatePool = fun(Pool) -> 
						 PName = worker_pool:processname(Pool),
						 
						 supervisor:delete_child(surrogate_sup,PName),
						 R =
						 supervisor:start_child(surrogate_sup,
												{PName,{worker_pool,start_pool,[Pool,roundrobin]},
												 permanent,5000,worker,[]}),
						 ?DEBUG_MSG("Trying to start: ~p -> ~p~n",[PName,R]),
						 ok
				 end,
	lists:foreach(fun(Pool2) ->
						  spawn(fun() -> UpdatePool(Pool2) end)
				  end,list_pools()),
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
handle_info(check_workers,State) ->
	erlang:send_after(10000,self(),check_workers),
	check_nodes(),
	{noreply,State};
handle_info({nodedown,Node},State) ->
	?WARN_MSG("Received nodedown: ~p~n",[Node]),
	check_nodes(),
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

list_active_nodes() ->
	F = fun() ->
				Match = #worker_node{active=true,node='$1',_='_'},
				mnesia:select(worker_node,[{Match,[],['$1']}])
		end,
	mnesia:transaction(F).

check_nodes() ->
	lists:foreach(fun(X) ->
						  case net_adm:ping(X#worker_node.node) of
							  pong ->
								  gen_server:cast(self(),{monitor_node,X}),
								  case lists:member(X#worker_node.node,mnesia:system_info(running_db_nodes)) of
									  true ->
										  enable_node(X);
									  false ->
										  ?INFO_MSG("Node ~p is alive, but mnesia is not functioning properly.~nStart mnesia on the node and run: ~nmnesia:change_config(extra_db_nodes,['~p']). to repair.~n",
													[X#worker_node.node,node()]),
										  disable_node(X)
								  end;
							  pang ->
								  disable_node(X)
						  end
				  end,
				  ets:tab2list(worker_node)).

enable_node(X) ->
	if X#worker_node.active ->
		ok;
		true ->
			?INFO_MSG("Activating previously inactive node: ~p~n",[X#worker_node.node]),
			erlang:monitor_node(X#worker_node.node,true),
			set_pool_node_state(X#worker_node.node,true),
			lists:foreach(fun(P) -> worker_pool:refresh_nodes(P) end,
						  list_node_pools(X#worker_node.node)),
			mnesia:transaction(fun(N) ->
									   mnesia:write(N)
							   end,[X#worker_node{active=true}])
	end.

disable_node(X) ->
	if X#worker_node.active ->
			?INFO_MSG("Deactivating previously active node: ~p~n",[X#worker_node.node]),
			set_pool_node_state(X#worker_node.node,false),
			lists:foreach(fun(P) -> worker_pool:refresh_nodes(P) end,
						  list_node_pools(X#worker_node.node)),
			mnesia:transaction(fun(N) ->
									   mnesia:write(N)
							   end,[X#worker_node{active=false}]);
		true ->
			ok
	end.

set_pool_node_state(Node,Active) ->
	Pools = list_node_pools(Node),
%% 	?DEBUG_MSG("set_pool_node_state(~p,~p)~nPools: ~p~n",[Node,Active,Pools]),
	F = fun() ->
				lists:foreach(fun(Pool) ->
									  [Rec|_] = mnesia:read(worker_node_pool,{Pool,Node}),
									  Rec1 = Rec#worker_node_pool{active=Active},
									  mnesia:write(Rec1)
							  end,Pools)
		end,
	mnesia:transaction(F).
									  
	