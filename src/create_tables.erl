-module(create_tables).

-export([init_tables/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

init_tables() ->
    mnesia:create_table(proxy_userinfo, [{attributes, record_info(fields, proxy_userinfo)}]),
    mnesia:create_table(filter_host_list, [{attributes, record_info(fields, filter_host_list)}]),
    mnesia:create_table(filter_url_list, [{attributes, record_info(fields, filter_url_list)}]).
