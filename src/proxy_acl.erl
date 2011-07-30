%% Author: skruger
%% Created: Jul 29, 2011
%% Description: TODO: Add description to proxy_acl
-module(proxy_acl).

%%
%% Include files
%%
-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([set_permission/2,clear_list/1,clear_all_permissions/0]).
-export([get_permission/2]).

%%
%% API Functions
%%

clear_all_permissions() ->
	mnesia:clear_table(proxy_acl).

set_permission(List,User) ->
	mnesia:dirty_write(#proxy_acl{list=List,user=User}).

clear_list(List) ->
	mnesia:dirty_delete(proxy_acl,List).

get_permission(List,User) ->
	case lists:filter(fun(#proxy_acl{user=U}) when U == User -> true;
						 (_) -> false end, mnesia:dirty_read(proxy_acl,List)) of
		[] ->
			false;
		R when length(R) > 0 -> true
	end.


%%
%% Local Functions
%%

