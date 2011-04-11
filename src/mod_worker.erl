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
	end.

proxy_mod_stop(_) ->
	ok.

%%
%% Local Functions
%%

