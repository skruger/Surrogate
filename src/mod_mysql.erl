-module(mod_mysql).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([proxy_mod_start/1,proxy_mod_stop/1]).

-behaviour(proxy_mod).

%%
%% API Functions
%%

%%%  mod_mysql configuration:
%% SqlConnSpecs = [SqlConnSpec|_]
%% SqlConnSpec = {ConnName,Host, Port, User, Password, Database}

proxy_mod_start([Conn|R]) ->
	case Conn of
		{ConnName,Host, Port, User, Password, Database} ->
			ChildName = list_to_atom("mysqlconn_"++atom_to_list(ConnName)),
			Spec = {ChildName,{mysql,start_link,[ConnName, Host, Port, User, Password, Database]},
					permanent,2000,worker,[]},
			case supervisor:start_child(surrogate_sup,Spec) of
				{error,_} = SupErr ->
					?CRITICAL("Error starting mysql with config: ~p~n~p~n",[Conn,SupErr]);
				_ -> ok
						
			end;
		_ ->
			?ERROR_MSG("Invalid mod_mysql connection specification: ~p~n",[Conn]),
			error
	end,
	proxy_mod_start(R);
proxy_mod_start(_) ->
	ok.

proxy_mod_stop(_Conf) ->
	ok.



%%
%% Local Functions
%%

