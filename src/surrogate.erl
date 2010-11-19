
-module(surrogate).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0,stop/0,reload/0]).

start() ->
	application:start(?MODULE).

stop() ->
	application:stop(?MODULE).

reload() ->
	stop(),
	start().




%%
%% API Functions
%%



%%
%% Local Functions
%%

