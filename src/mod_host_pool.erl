%% Author: skruger
%% Created: Jul 20, 2011
%% Description: TODO: Add description to mod_host_pool
-module(mod_host_pool).

%%
%% Include files
%%
-include("surrogate.hrl").

%%
%% Exported Functions
%%

-behaviour(proxy_mod).
-behaviour(filter_stream).

-export([start_instance/0,process_hook/4,proxy_mod_start/1,proxy_mod_stop/1]).
-export([set_host_pool/2,get_pool_by_host/1]).

-record(?MODULE,{host,pool}).

%%
%% API Functions
%%

proxy_mod_start(_Conf) ->
	mnesia:create_table(?MODULE,[{attributes,record_info(fields,?MODULE)}]),
	mnesia:change_table_copy_type(?MODULE,node(),disc_copies),
	ok.

proxy_mod_stop(_Conf) ->
	ok.

start_instance() ->
	{?MODULE,?MODULE}.

process_hook(_Pid,request,{request_header,Hdr,_Size}=HBlock,PPC) ->
	HDict = proxylib:header2dict(Hdr#header_block.headers),
	case dict:find("host",HDict) of
		{ok,HostStr} ->
			{host,Host,Port} = proxylib:parse_host(HostStr,80),
			case get_pool_by_host(Host) of
				{ok,Pool} ->
					proxy_pass:setproxyaddr(PPC#proxy_pass.proxy_pass_pid,[{pool,Pool,Port,3}]);
				_ ->
					ok
			end;
		_ -> ok
	end,
	HBlock;
process_hook(_Pid,_Mode,Data,_PPC) ->
	Data.

get_pool_by_host(Host) ->
	case mnesia:dirty_read(?MODULE,Host) of
		[#?MODULE{pool=Pool}|_] ->
			case gen_balancer:is_alive(Pool) of
				true ->
					{ok,Pool};
				_ -> {error,invalid_pool}
			end;
		_ ->
			{error,nopool}
	end.

set_host_pool(Host,Pool) ->
	F = fun() ->
				mnesia:write(#?MODULE{host=Host,pool=Pool}) end,
	mnesia:transaction(F).

%%
%% Local Functions
%%

