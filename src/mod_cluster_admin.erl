%% Author: skruger
%% Created: Aug 5, 2011
%% Description: TODO: Add description to mod_cluster_admin
-module(mod_cluster_admin).

%%
%% Include files
%%
-include("surrogate.hrl").

%%
%% Exported Functions
%%

-behaviour(proxy_mod).

-export([proxy_mod_start/1,proxy_mod_stop/1]).
-export([http_api/3]).

%%
%% API Functions
%%

proxy_mod_start(_Opts) ->
	?ERROR_MSG("Staring ~p~n",[?MODULE]),
	proxy_protocol_http_admin:register_module(?MODULE,?MODULE,http_api).

proxy_mod_stop(_Opts) ->
	proxy_protocol_http_admin:unregister_module(?MODULE).


http_api(["vip",IPStr],#http_admin{method='POST',has_auth=Auth},_Conf) when Auth == true ->
	try
		Vip = {ip,proxylib:inet_parse(IPStr)},
		cluster_vip_manager:add_vip(Vip),
		cluster_vip_manager:disable_vip(Vip),
		StatusMsg = io_lib:format("Posted: ~p~n",[Vip]),
		JsonOut = {struct,[{"status",<<"ok">>},{"error",<<"none">>},{"status_msg",iolist_to_binary(StatusMsg)}]},
		{200,[],iolist_to_binary(mochijson2:encode(JsonOut))}
	catch
		_:{ErrName,ErrTup} when is_tuple(ErrTup) ->
			JsonOutErr = {struct,[{"status",<<"error">>},{"error_type",list_to_binary(atom_to_list(ErrName))},
								  {"error",iolist_to_binary(io_lib:format("~p",[ErrTup]))}]},
			{200,[],iolist_to_binary(mochijson2:encode(JsonOutErr))};
		_:{ErrName,ErrBin} when is_list(ErrBin) ; is_binary(ErrBin) ->
			?ERROR_MSG("~p / ~p",[ErrName,ErrBin]),
			JsonOutErr = {struct,[{"status",<<"error">>},{"error_type",list_to_binary(atom_to_list(ErrName))},
								  {"error",iolist_to_binary(ErrBin)}]},
			{200,[],iolist_to_binary(mochijson2:encode(JsonOutErr))};

		_:Error ->
			?ERROR_MSG("Update error (~p): ~p",[self(),Error]),
			EStr = io_lib:format("Update error (~p): ~p",[self(),Error]),
			JsonOutErr = {struct,[{"status",<<"error">>},{"error",iolist_to_binary(EStr)}]},
			{200,[],iolist_to_binary(mochijson2:encode(JsonOutErr))}
	end;
http_api(["vip",IP],#http_admin{method='DELETE',has_auth=Auth},_Conf) when Auth == true ->
	Vip = {ip,proxylib:inet_parse(IP)},
	cluster_vip_manager:disable_vip(Vip),
	StatusMsg = "Disabled VIP",
	JsonOut = {struct,[{"status",<<"ok">>},{"error",<<"none">>},{"status_msg",iolist_to_binary(StatusMsg)}]},
	{200,[],iolist_to_binary(mochijson2:encode(JsonOut))};
http_api(["vip",IP],#http_admin{method='PUT',has_auth=Auth,body=JsonIn},_Conf) when Auth == true ->
	try
		Vip = {ip,proxylib:inet_parse(IP)},
		{Status,JsonOut} =
		case mochijson2:decode(JsonIn) of
			{struct,JsonList} ->
				case proplists:get_value(<<"action">>,JsonList,<<"none">>) of
					<<"enable">> ->
						cluster_vip_manager:enable_vip(Vip),
						{200,[{"status",<<"ok">>},{"error",<<"none">>}]};
					<<"disable">> ->
						cluster_vip_manager:disable_vip(Vip),
						{200,[{"status",<<"ok">>},{"error",<<"none">>}]};
					BadAction ->
						{500,[{"status",<<"error">>},{"error",<<"bad_action">>},{"status_msg",iolist_to_binary(["Invalid action: ",BadAction])}]}
				end;
			BadJson ->
				?ERROR_MSG("Bad json in POST ~p/vip/~s~n~p~n",[?MODULE,IP,BadJson]),
				{500,[{"status",<<"error">>},{"error",<<"bad_action">>},{"status_msg",iolist_to_binary(["Invalid json."])}]}
		end,
		{Status,[{"Content-type","application/json"}],iolist_to_binary(mochijson2:encode({struct,JsonOut}))}
	catch
		_:VipErr ->
			?ERROR_MSG("Error modifying vip: ~p~n",[VipErr]),
			{500,[],<<"Internal Server Error">>}
	end;
http_api(["vip"],#http_admin{has_auth=Auth},_Conf) when Auth == true ->
	try
		VipList0 = cluster_vip_manager:get_vip_list(),
		VipList = lists:map(fun({IP,Stat,Nodes}) ->
									{list_to_binary(proxylib:format_inet(IP)),
									 list_to_binary(atom_to_list(Stat)),
									 [list_to_binary(atom_to_list(H)) || H <- Nodes]};
							   (_) -> error end,VipList0),
		Vips = [{struct,[{"address",IP},{"status",Status},{"nodes",Nodes}]} || {IP,Status,Nodes} <- VipList],
		?ERROR_MSG("VipList: ~p~nVips: ~p~n",[VipList,Vips]),
		JsonOut = {struct,[{"items",lists:sort(Vips)}]},
		{200,[],iolist_to_binary(mochijson2:encode(JsonOut))}
	catch
		_:VipErr ->
			?ERROR_MSG("Error getting vip list: ~p~n",[VipErr]),
			{500,[],<<"Internal Server Error">>}
	end;
http_api(["listener","delete",Name],#http_admin{body=Json,has_auth=Auth}=Request,_Conf) when Auth == true ->
	R = mnesia:dirty_delete(cluster_listener,list_to_atom(Name)),
	?ERROR_MSG("Delete result: ~p~n",[R]),
	JsonOut = {struct,[{"status",<<"ok">>},{"error",<<"none">>},{"status_msg",<<"Listener deleted.">>}]},
	{200,[],iolist_to_binary(mochijson2:encode(JsonOut))};
http_api(["listener",Name],#http_admin{body=Json,has_auth=Auth}=Request,_Conf) when Auth == true ->
	try
		case mochijson2:decode(Json) of
			{struct,Args} ->
				Listener0 = listener_json_record(Args,#cluster_listener{}),
				NewName = listener_name(Listener0),
				StatusMsg = 
				case mnesia:dirty_read(cluster_listener,NewName) of
					[#cluster_listener{supervisor=Sup,sup_process_name=SupProc}|_] ->
						mnesia:dirty_write(Listener0#cluster_listener{supervisor=Sup,sup_process_name=SupProc}),
						io_lib:format("Replaced entry for ~p",[NewName]);
					_ ->
						mnesia:dirty_write(Listener0#cluster_listener{name=NewName}),
						io_lib:format("Added new entry for ~p",[NewName])
				end,
				JsonOut = {struct,[{"status",<<"ok">>},{"error",<<"none">>},{"status_msg",iolist_to_binary(StatusMsg)}]},
				{200,[],iolist_to_binary(mochijson2:encode(JsonOut))};
			Other ->
				?ERROR_MSG("Unexpected decode result: ~n~p~n",[Other]),
				erlang:error({json_error,io_lib:format("Json decode error: ~p",[Other])})
		end
	catch
		_:{ErrName,ErrBin} ->
			JsonOutErr = {struct,[{"status",<<"error">>},{"error_type",list_to_binary(atom_to_list(ErrName))},
								  {"error",iolist_to_binary(ErrBin)}]},
			{200,[],iolist_to_binary(mochijson2:encode(JsonOutErr))};
		_:Error ->
			?ERROR_MSG("Update error (~p): ~p",[self(),Error]),
			EStr = io_lib:format("Update error (~p): ~p",[self(),Error]),
			JsonOutErr = {struct,[{"status",<<"error">>},{"error",iolist_to_binary(EStr)}]},
			{200,[],iolist_to_binary(mochijson2:encode(JsonOutErr))}
	end;
http_api(["listeners.json"],Request,_Conf) when Request#http_admin.has_auth == true ->
	RawListeners = 
	lists:flatmap(fun(K) ->
						  mnesia:dirty_read(cluster_listener,K) end,
				  mnesia:dirty_all_keys(cluster_listener)),
	JsonListeners = {struct,[{"items",[listener_to_json(L) || L <- RawListeners]}]},
	{200,[{"Content-Type","text/plain"}],iolist_to_binary(mochijson2:encode(JsonListeners))};
http_api(["listeners.json"],_Request,_Conf) ->
	{200,[{"Content-Type","text/plain"}],iolist_to_binary(mochijson2:encode({struct,[{"items",[]}]}))};
http_api(["postecho"],Request,_Conf) ->
	{200,[],iolist_to_binary(Request#http_admin.body)};
http_api(Path,Request,_Conf) when Request#http_admin.has_auth == true ->
	Err = io_lib:format("Not found in ~p: ~p~n",[?MODULE,Path]),
	{404,[{'Content-type',"text/plain"}],iolist_to_binary(Err)};
http_api(Path,Request,_Conf) ->
	?ERROR_MSG("Authorization required: ~p~n~p~n",[Path,Request]),
	{401,[{"WWW-Authenticate","Basic realm=\"mod_cluster_admin\""}],iolist_to_binary("Authorization required")}.

%%
%% Local Functions
%%

listener_name(#cluster_listener{ip=IP,port=Port}) ->
	IPList = proxylib:format_inet(IP),
	NameStr = lists:flatten(io_lib:format("~s_~p",[IPList,Port])),
	list_to_atom(NameStr).

listener_json_record([],Rec) ->
	Rec;
listener_json_record([{<<"name">>,Name0}|R],Rec) ->
	Name = list_to_atom(binary_to_list(Name0)),
	listener_json_record(R,Rec#cluster_listener{name=Name});
listener_json_record([{<<"listen_type">>,Type0}|R],Rec) when (Type0 == <<"listen_plain">>) or (Type0 == <<"listen_ssl">>) ->
	Type = list_to_atom(binary_to_list(Type0)),
	listener_json_record(R,Rec#cluster_listener{type=Type});
listener_json_record([{<<"listen_type">>,Type0}|R],Rec) ->
	?ERROR_MSG("Invalid listen_type: ~p~n",[binary_to_list(Type0)]),
	erlang:error({listen_type_error,iolist_to_binary(io_lib:format("Invalid listen_type: ~p",[binary_to_list(Type0)]))}),
	listener_json_record(R,Rec);
listener_json_record([{<<"listen_port">>,Port0}|R],Rec) ->
	Port = list_to_integer(binary_to_list(Port0)),
	listener_json_record(R,Rec#cluster_listener{port=Port});
listener_json_record([{<<"listen_address">>,Addr0}|R],Rec) ->
	Addr = binary_to_list(Addr0),
	case catch proxylib:inet_parse(proxylib:inet_getaddr(Addr)) of
		{_,_,_,_} = IP ->
			listener_json_record(R,Rec#cluster_listener{ip={ip,IP}});
		{_,_,_,_,_,_,_,_} = IP ->
			listener_json_record(R,Rec#cluster_listener{ip={ip,IP}});
		BadIP ->
			?ERROR_MSG("Invalid IP Format: ~p~n~p~n",[Addr,BadIP]),
			erlang:error({invalid_ip_error,iolist_to_binary(io_lib:format("Invalid IP format: ~p",[Addr]))}),
			listener_json_record(R,Rec)
	end;
listener_json_record([{<<"config_list">>,Conf0}|R],Rec) ->
	Conf = binary_to_list(Conf0)++".",
	case erl_scan:string(Conf) of
		{ok,Tokens,_} ->
			case erl_parse:parse_term(Tokens) of
				{ok,Term} when is_list(Term) ->
					listener_json_record(R,Rec#cluster_listener{options=Term});
				ErrTerm ->
					?ERROR_MSG("No terms found in conf: ~p~n~p~n",[ErrTerm,Tokens]),
					erlang:error({config_list,io_lib:format("Error in config_list: ~p",[ErrTerm])}),
					listener_json_record(R,Rec)
			end;
		ErrTok ->
			?ERROR_MSG("No tokens found in Conf: ~p~n~p~n",[ErrTok,Conf]),
			erlang:error({config_list,io_lib:format("Error in config_list: ~p",[ErrTok])}),
			listener_json_record(R,Rec)
	end;
listener_json_record([Other|R],Rec) ->
	?ERROR_MSG("Unexpected value in json record: ~p~n",[Other]),
	listener_json_record(R,Rec).

listener_to_json(#cluster_listener{ip={ip,IP},port=Port,type=Type,options=ConfigList,name=Name}) ->
	NameBin = list_to_binary(atom_to_list(Name)),
	IPBin = list_to_binary(proxylib:format_inet(IP)),
	PortBin = list_to_binary(integer_to_list(Port)),
	TypeBin = list_to_binary(atom_to_list(Type)),
	ConfBin = iolist_to_binary(io_lib:format("~p",[ConfigList])),
	{struct,[{"name",NameBin},{"listen_type",TypeBin},{"listen_address",IPBin},{"listen_port",PortBin},{"config_list",ConfBin}]}.
	
%% {name: "Type", field: "listen_type", width: "65px"},
%% 		            {name: "Address",field: "listen_address",width: "150px"},
%% 		            {name: "Port",field: "listen_port",width: "60px"},
%% 		            {name: "Config",field: "config_list",width: "512px"}]
