
-module(surrogate_mnesia).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([delete_all/0,init/0]).

%%
%% API Functions
%%

init() ->
	[
	mnesia:create_table(filter_host_list,[{attributes,record_info(fields,filter_host_list)}]),
 	mnesia:create_table(filter_url_list,[{type,bag},{attributes,record_info(fields,filter_url_list)}]),
	mnesia:create_table(proxy_userinfo,[{attributes,record_info(fields,proxy_userinfo)}])
	].
%% 	ok.

delete_all() ->
	[
	 mnesia:delete_table(filter_host_list),
	 mnesia:delete_table(filter_url_list),
	 mnesia:delete_table(proxy_userinfo)
	].
	
%%
%% Local Functions
%%

