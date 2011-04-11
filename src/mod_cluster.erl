-module(mod_cluster).

-behaviour(proxy_mod).
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([proxy_mod_start/1,proxy_mod_stop/1]).

%%
%% API Functions
%%

proxy_mod_start(Conf) ->
	application:load(cluster_supervisor),
	application:set_env(cluster_supervisor,cluster_config,Conf),
	application:start(cluster_supervisor).

proxy_mod_stop(_Conf) ->
	application:stop(cluster_supervisor).


%%
%% Local Functions
%%

