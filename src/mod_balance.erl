-module(mod_balance).

-behaviour(proxy_mod).
%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([proxy_mod_start/1,proxy_mod_stop/1]).

%%
%% API Functions
%%
proxy_mod_start([Bal|R]) ->
	case Bal of
		{PName,BalMod,Conf} ->
			PoolName = list_to_atom("balance_"++atom_to_list(PName)++"_sup"),
%% 			PoolProcName = proxylib:get_pool_process(PName),
			Spec = {PoolName,{balance_sup,start_link,[PName,BalMod,Conf]},
					permanent,2000,supervisor,[]},
			case supervisor:start_child(surrogate_sup,Spec) of
				{error,_} = SupErr ->
					?CRITICAL("Error starting pool with config: ~p~n~p~n",[Bal,SupErr]);
				_ -> ok
						
			end;
		_ ->
			?ERROR_MSG("Invalid balanceer definition: ~p~n",[Bal]),
			ok
	end,
	proxy_mod_start(R);
proxy_mod_start(_) ->
	ok.

proxy_mod_stop(_) ->
	ok.


%%
%% Local Functions
%%

