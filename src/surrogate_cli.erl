%% Author: skruger
%% Created: Oct 13, 2011
%% Description: TODO: Add description to surrogate_cli
-module(surrogate_cli).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([exec_cmds/1]).

%%
%% API Functions
%%

exec_cmds(["user"|R]) ->
	user_cmd(R);
exec_cmds(["vip"|R]) ->
	vip_cmd(R);
exec_cmds(["info"|_]) ->
	io:format("Node: ~p~nNodes: ~p~n",[node(),nodes()]);
exec_cmds([Cmd|Args]) ->
	io:format("Unknown command:~p with args:~n~p~n~n",[Cmd,Args]),
	usage().


vip_cmd(["list"|_]) ->
	case cluster_vip_manager:get_vip_list() of
		List when is_list(List) ->
			FormatStr = "~40s | ~8s | ~p~n",
			io:format(FormatStr,["Address","Status",'Node List']),
			lists:foreach(fun({IP,En,Nodes}) -> io:format(FormatStr,[proxylib:format_inet(IP),atom_to_list(En),Nodes]) end, List);
		Err ->
			io:format("Error list vip: ~p~n",[Err])
	end;
vip_cmd(["disable",VipStr|_]) ->
	IPAddr = proxylib:inet_parse(VipStr),
	Result =  cluster_vip_manager:disable_vip({ip,IPAddr}),
	io:format("~p~n",[Result]);
vip_cmd(["enable",VipStr|_]) ->
	IPAddr = proxylib:inet_parse(VipStr),
	Result =  cluster_vip_manager:enable_vip({ip,IPAddr}),
	io:format("~p~n",[Result]);
vip_cmd(["setnodes",VipStr|NodeStrList]) ->
	NodeList = [list_to_atom(N) || N <- NodeStrList],
	IPAddr = proxylib:inet_parse(VipStr),
	io:format("Setting nodes for ~p~n~p~n",[IPAddr,NodeList]),
	Result =  cluster_vip_manager:set_hostnodes({ip,IPAddr},NodeList),
	io:format("~p~n",[Result]);
vip_cmd([Action|_Args]) ->
	io:format("vip command has no action ~p~n~n",[Action]),
	usage().

user_cmd(["add",Username,Pass|_]) ->
	case proxy_auth:add_user(Username,Pass) of
		{atomic,ok} ->
			io:format("User added: ~p~n",[Username]);
		Err ->
			io:format("Error adding user: ~p~n",[Err])
	end;
user_cmd(["delete",Username|_]) ->
	case proxy_auth:delete_user(Username) of
		{atomic,ok} ->
			io:format("User ~p deleted.~n",[Username]);
		Err ->
			io:format("Error deleting user: ~p~n",[Err])
	end;
user_cmd(["list"|_]) ->
	case proxy_auth:list_users() of
		List when is_list(List) ->
			lists:foreach(fun(User) -> io:format("~s~n",[User]) end,List);
		Err ->
			io:format("Error adding user: ~p~n",[Err])
	end;
user_cmd([Action|_Args]) ->
	io:format("user command has no action ~p~n~n",[Action]),
	usage().
	


%%
%% Local Functions
%%

usage() ->
	io:format("Surrogate cli usage..~n",[]).
