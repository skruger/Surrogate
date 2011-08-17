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

%% -export([http_api/2]).

-record(?MODULE,{host,pool}).

%%
%% API Functions
%%

proxy_mod_start(_Conf) ->
	mnesia:create_table(?MODULE,[{attributes,record_info(fields,?MODULE)}]),
	mnesia:change_table_copy_type(?MODULE,node(),disc_copies),
%% 	proxy_protocol_http_admin:register_module(?MODULE,?MODULE,http_api),
	ok.

proxy_mod_stop(_Conf) ->
%% 	proxy_protocol_http_admin:unregister_module(?MODULE),
	ok.

start_instance() ->
	{?MODULE,?MODULE}.

process_hook(_Pid,request,{request_header,Hdr,_Size}=HBlock,PPC) ->
	HDict = dict:from_list(Hdr#header_block.headers),
	case dict:find('Host',HDict) of
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

%% http_api([],Request) when Request#http_admin.has_auth ->
%% 	?ERROR_MSG("Request: ~p~n",[Request]),
%% 	"Host to pool mapping module.".
%% 	{404,[{"Content-type","text/plain"}],io_lib:format("Path: ~p not found",[Path])}.

%%
%% Local Functions
%%

