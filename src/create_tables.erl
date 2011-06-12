-module(create_tables).

-export([init_tables/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

init_tables() ->
    mnesia:create_table(proxy_userinfo, [{attributes, record_info(fields, proxy_userinfo)}]).
    