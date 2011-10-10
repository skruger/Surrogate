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
-export([set_host_pool/2,set_host_pool/3,get_pool_by_host/1]).

-export([http_api/3]).

-record(?MODULE,{host,pool,port}).

%%
%% API Functions
%%

proxy_mod_start(_Conf) ->
	mnesia:create_table(?MODULE,[{attributes,record_info(fields,?MODULE)}]),
	mnesia:change_table_copy_type(?MODULE,node(),disc_copies),
	mnesia:add_table_copy(?MODULE,node(),disc_copies),
	proxy_protocol_http_admin:register_module(?MODULE,?MODULE,http_api),
	ok.

proxy_mod_stop(_Conf) ->
	proxy_protocol_http_admin:unregister_module(?MODULE),
	ok.

start_instance() ->
	{?MODULE,?MODULE}.

process_hook(_Pid,request,{request_header,Hdr,_Size}=HBlock,PPC) ->
	Opts = proplists:get_value(mod_host_pool,PPC#proxy_pass.config,[]),
	RouteHeader = proplists:get_value(route_header,Opts,'Host'),
	HDict = dict:from_list(Hdr#header_block.headers),
	case dict:find(RouteHeader,HDict) of
		{ok,HostStr} ->
			{host,Host,Port} = proxylib:parse_host(HostStr,80),
			case get_pool_by_host(Host) of
				{ok,Pool} ->
					proxy_pass:setproxyaddr(PPC#proxy_pass.proxy_pass_pid,[{pool,Pool,Port,3}]);
				{ok,Pool,PortOverride} ->
					proxy_pass:setproxyaddr(PPC#proxy_pass.proxy_pass_pid,[{pool,Pool,PortOverride,3}]);
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
		[#?MODULE{pool=Pool,port=Port}|_] ->
			case gen_balancer:is_alive(Pool) of
				true when is_integer(Port) ->
					{ok,Pool,Port};
				true ->
					{ok,Pool};
				_ -> {error,{stopped,Pool}}
			end;
		_ ->
			{error,nopool}
	end.

set_host_pool(Host,Pool) ->
	set_host_pool(Host,Pool,undefined).

set_host_pool(Host,Pool,Port) ->
	F = fun() ->
				mnesia:write(#?MODULE{host=Host,pool=Pool,port=Port}) end,
	mnesia:transaction(F).

http_api(["vhost",Host],#http_admin{method='GET',has_auth=Auth}=_Request,_Cfg) when Auth == true ->
	case get_pool_by_host(Host) of
		{ok,Pool,Port} ->
			Json = {struct,[{"status",<<"ok">>},{"pool",list_to_binary(atom_to_list(Pool))},
					{"port",list_to_binary(integer_to_list(Port))}]},
			{200,[],iolist_to_binary(mjson:encode(Json))};
		{ok,Pool} ->
			Json = {struct,[{"status",<<"ok">>},{"pool",list_to_binary(atom_to_list(Pool))}]},
			{200,[],iolist_to_binary(mjson:encode(Json))};
		{error,{stopped,Pool}} ->
			Json = {struct,[{"status",<<"warning">>},{"warning",<<"pool_not_running">>},
							{"pool",list_to_binary(atom_to_list(Pool))}]},
			{200,[],iolist_to_binary(mjson:encode(Json))};
		{error,_ErrReason} ->
			Json = {struct,[{"status",<<"error">>},{"error",<<"pool_not_found">>}]},
			{404,[],iolist_to_binary(mjson:encode(Json))}
	end;
http_api(["vhost",Host],#http_admin{method='DELETE',has_auth=Auth}=_Request,_Cfg) when Auth == true ->
	mnesia:dirty_delete(?MODULE,Host),
	Json = {struct,[{"status",<<"ok">>}]},
	{200,[],iolist_to_binary(mjson:encode(Json))};
http_api(["vhost",Host,PoolStr],#http_admin{method='POST',has_auth=Auth}=_Request,_Cfg) when Auth == true ->
	Pool = list_to_atom(PoolStr),
	case gen_balancer:is_alive(Pool) of
		true ->
			set_host_pool(Host,Pool),
			Json = {struct,[{"status",<<"ok">>},{"pool",list_to_binary(PoolStr)}]},
			{200,[],iolist_to_binary(mjson:encode(Json))};
		_ ->
			Json = {struct,[{"status",<<"error">>},{"error",<<"pool_not_found">>}]},
			{404,[],iolist_to_binary(mjson:encode(Json))}
	end;
http_api(["vhost",Host,PoolStr,PortStr],#http_admin{method='POST',has_auth=Auth}=_Request,_Cfg) when Auth == true ->
	Pool = list_to_atom(PoolStr),
	Port = list_to_integer(PortStr),
	case gen_balancer:is_alive(Pool) of
		true ->
			set_host_pool(Host,Pool,Port),
			Json = {struct,[{"status",<<"ok">>},{"pool",list_to_binary(PoolStr)},
					{"port",list_to_binary(PortStr)}]},
			{200,[],iolist_to_binary(mjson:encode(Json))};
		_ ->
			Json = {struct,[{"status",<<"error">>},{"error",<<"pool_not_found">>}]},
			{404,[],iolist_to_binary(mjson:encode(Json))}
	end;
http_api(_,Req,_Cfg) when Req#http_admin.has_auth == true ->
	{404,[],<<"Not found">>};
http_api(_,_,_Cfg) ->
	{401,["WWW-Authorize","Basic realm=mod_host_pool"],<<"Authorization required">>}.

%% http_api([],Request) when Request#http_admin.has_auth ->
%% 	?ERROR_MSG("Request: ~p~n",[Request]),
%% 	"Host to pool mapping module.".
%% 	{404,[{"Content-type","text/plain"}],io_lib:format("Path: ~p not found",[Path])}.

%%
%% Local Functions
%%

