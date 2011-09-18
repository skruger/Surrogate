-module(mod_balance).

-behaviour(proxy_mod).
%%
%% Include files
%%
-include("surrogate.hrl").

-define(SUP,mod_balance_sup).
%%
%% Exported Functions
%%
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([proxy_mod_start/1,proxy_mod_stop/1]).

-export([add_balancer/2,get_balancers/0,balancer_add_host/2,balancer_remove_host/2]).
-export([balancer_add_check/2,balancer_remove_check/2,balancer_set_enable/2,balancer_refresh_config/1]).


-record(state,{config}).
%%
%% API Functions
%%

proxy_mod_start(Conf) ->
	mnesia:create_table(cluster_balancer,[{attributes,record_info(fields,cluster_balancer)}]),
	mnesia:change_table_copy_type(cluster_balancer,node(),disc_copies),
	mnesia:add_table_copy(cluster_listener,node(),disc_copies),
	Spec = {?SUP,{supervisor,start_link,[{local,?SUP},?MODULE,{supervisor,Conf}]},
			permanent,2000,supervisor,[?MODULE]},
	supervisor:start_child(surrogate_sup,Spec),
 	Spec2 = {?MODULE,{gen_server,start_link,[{local,?MODULE},?MODULE,{gen_server,Conf},[]]},
			permanent,2000,supervisor,[?MODULE]},
	supervisor:start_child(surrogate_sup,Spec2),
	?ERROR_MSG("Starting ~p~n",[?MODULE]),
	ok.

proxy_mod_stop(_) ->
	ok.

get_poolname(PName) ->
	list_to_atom("balance_"++atom_to_list(PName)++"_sup").

start_balancers([Bal|R],Type) ->
	case Bal of
		{PName,BalMod,Conf} ->
			PoolName = get_poolname(PName), 
			Spec = {PoolName,{balance_sup,start_link,[PName,BalMod,Conf]},
					permanent,2000,supervisor,[]},
			case supervisor:start_child(?SUP,Spec) of
				{error,_} = SupErr ->
					?CRITICAL("Error starting pool with ~p config: ~p~n~p~n",[Type,Bal,SupErr]);
				_ -> ok
						
			end;
		_ ->
			?ERROR_MSG("Invalid balanceer definition (~p): ~p~n",[Type,Bal]),
			ok
	end,
	start_balancers(R,Type);
start_balancers(_R,_Type) ->
	ok.

balancer_specs(Balancers) ->
	lists:map(fun({PName,BalMod,Conf}) ->
						  PoolName = get_poolname(PName), 
						  Spec = {PoolName,{balance_sup,start_link,[PName,BalMod,Conf]},
								  permanent,2000,supervisor,[]},
						  Spec end,Balancers).

add_balancer(Name,Module) when is_list(Name) ->
	add_balancer(list_to_atom(Name),Module);
add_balancer(Name,Module) when is_list(Module) ->
	case code:where_is_file(Module++".beam") of
		non_existing ->
			{error,bad_module};
		_ ->
			add_balancer(Name,list_to_atom(Module))
	end;
add_balancer(Name,Module) ->
	case code:ensure_loaded(Module) of
		{module,_} ->
			Bal = #cluster_balancer{name=Name,enabled=false,balance_module=Module,hosts=[],checks=[],config=[]},
			F1 = fun() ->
				 		mnesia:write(Bal) end,
			mnesia:transaction(F1);
		Err ->
			Err
	end.

balancer_enable(Name) ->
	balancer_set_enable(Name,true).

balancer_disable(Name) ->
	balancer_set_enable(Name,false).

balancer_set_enable(Name,Enable) when is_list(Name) ->
	balancer_set_enable(list_to_atom(Name),Enable);
balancer_set_enable(Name,Enable) ->
	F1 = fun() ->
				 case mnesia:read(cluster_balancer,Name) of
					 [#cluster_balancer{}=Bal|_] ->
						 mnesia:write(Bal#cluster_balancer{enabled=Enable});
					 _ ->
						 mnesia:abort(no_balancer)
				 end end,
	mnesia:transaction(F1).

balancer_add_host(Name,Host) when is_list(Name) ->
	balancer_add_host(list_to_atom(Name),Host);
balancer_add_host(Name,Host) when is_list(Host) ->
	balancer_add_host(Name,proxylib:inet_parse(Host));
balancer_add_host(Name,Host) ->
	F1 = fun() ->
				 case mnesia:read(cluster_balancer,Name) of
					 [#cluster_balancer{hosts=Hosts}=Bal|_] ->
						 mnesia:write(Bal#cluster_balancer{hosts=[Host|Hosts]});
					 _ ->
						 mnesia:abort(no_balancer)
				 end end,
	mnesia:transaction(F1).

balancer_remove_host(Name,Host) when is_list(Name) ->
	balancer_remove_host(list_to_atom(Name),Host);
balancer_remove_host(Name,Host) when is_list(Host) ->
	balancer_remove_host(Name,proxylib:inet_parse(Host));
balancer_remove_host(Name,Host) ->
	F1 = fun() ->
				 case mnesia:read(cluster_balancer,Name) of
					 [#cluster_balancer{hosts=Hosts}=Bal|_] ->
						 mnesia:write(Bal#cluster_balancer{hosts=lists:delete(Host,Hosts)});
					 _ ->
						 mnesia:abort(no_balancer)
				 end end,
	mnesia:transaction(F1).

balancer_add_check(Name,Check) when is_list(Name) ->
	balancer_add_check(list_to_atom(Name),Check);
balancer_add_check(Name,Check) ->
	F1 = fun() ->
				 case mnesia:read(cluster_balancer,Name) of
					 [#cluster_balancer{checks=Checks}=Bal|_] ->
						 mnesia:write(Bal#cluster_balancer{checks=[Check|Checks]});
					 _ ->
						 mnesia:abort(no_balancer)
				 end end,
	mnesia:transaction(F1).

balancer_remove_check(Name,Check) when is_list(Name) ->
	balancer_remove_check(list_to_atom(Name),Check);
balancer_remove_check(Name,Check) ->
	F1 = fun() ->
				 case mnesia:read(cluster_balancer,Name) of
					 [#cluster_balancer{checks=Checks}=Bal|_] ->
						 mnesia:write(Bal#cluster_balancer{checks=lists:delete(Check,Checks)});
					 _ ->
						 mnesia:abort(no_balancer)
				 end end,
	mnesia:transaction(F1).

balancer_refresh_config(Name) when is_list(Name) ->
	balancer_refresh_config(list_to_atom(Name));
balancer_refresh_config(Name) ->
	gen_server:call(mod_balance,{refresh_balancer,Name}).

get_balancers() ->
	Balancers = [mnesia:dirty_read(cluster_balancer,Name) || Name <- mnesia:dirty_all_keys(cluster_balancer)],
	[ {Name,Bal,Conf++[{hosts,Hosts},{checks,Checks}]}
	 || [#cluster_balancer{name=Name,enabled=true,balance_module=Bal,hosts=Hosts,checks=Checks,config=Conf}|_] 
			<- Balancers].

init({supervisor,_Conf}) ->
	{ok,{{one_for_one,5,50},[]}};
init({gen_server,Conf}) ->
	self() ! check_config_timer,
	{ok,#state{config=Conf}}.

handle_call({refresh_balancer,Name},_From,State) ->
	PoolName = get_poolname(Name),
	supervisor:terminate_child(?SUP, PoolName),
	supervisor:delete_child(?SUP, PoolName),
	gen_server:cast(self(),check_config),
	{reply,ok,State};
handle_call(_Req,_From,State) ->
	{reply,error,State}.

handle_cast(check_config,State) ->
	SupChildren = supervisor:which_children(mod_balance_sup),
%% 	CurrentChildren = [Child || {Child,_,_,_} <- SupChildren],
	NewChildrenList = balancer_specs(State#state.config)++balancer_specs(get_balancers()),
	lists:foreach(fun({Child,_,_,_,_,_}=Spec) ->
						  case lists:keyfind(Child,1,SupChildren) of
							  false ->
								  ?ERROR_MSG("Start child: ~p~n",[Spec]),
								  supervisor:start_child(?SUP,Spec);
							  _ ->
								  ok
						  end end,NewChildrenList),
	lists:foreach(fun({Child,_,_,_}) ->
						  case lists:keyfind(Child,1,NewChildrenList) of
							  false ->
								  ?ERROR_MSG("Stop child: ~p~n",[Child]),
								  supervisor:terminate_child(?SUP, Child),
								  supervisor:delete_child(?SUP,Child),
								  ok;
							  _ ->
								  ok
						  end end,SupChildren),
%% 	?ERROR_MSG("Current: ~n~p~nNew:~n~p~n",[CurrentChildren,NewChildrenList]),
	{noreply,State};
handle_cast(_Msg,State) ->
	{noreply,State}.

handle_info(check_config_timer,State) ->
	gen_server:cast(self(),check_config),
	erlang:send_after(10000,self(),check_config_timer),
	{noreply,State};
handle_info(_Info,State) ->
	{noreply,State}.

terminate(_Reason,_State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Local Functions
%%

