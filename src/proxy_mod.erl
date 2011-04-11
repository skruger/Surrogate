-module(proxy_mod).

%%
%% Include files
%%
-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([behaviour_info/1,start_proxy_modules/1,stop_proxy_modules/1]).

%%
%% API Functions
%%

behaviour_info(callbacks) ->
	[{proxy_mod_start,1},{proxy_mod_stop,1}];
behaviour_info(_) ->
	undefined.


start_proxy_modules([MSpec|R]) ->
	case MSpec of
		{F,Conf} ->
			try
				F:proxy_mod_start(Conf)
			catch
				_:Error ->
					?ERROR_MSG("Error starting global filter instance: ~p~n~p:proxy_mod_start(~p)~n",[Error,F,Conf]),
					error
			end;
		_ ->
			?ERROR_MSG("Invalid module spec given to ~p:start_proxy_modules(): ~p~n",[?MODULE,MSpec]),
			error
	end,
	start_proxy_modules(R);
start_proxy_modules(_) -> ok.

stop_proxy_modules([MSpec|R]) ->
	case MSpec of
		{F,Conf} ->
			try
				F:proxy_mod_stop(Conf)
			catch
				_:Error ->
					?ERROR_MSG("Error stopping global filter instance: ~p~n~p:proxy_mod_stop(~p)~n",[Error,F,Conf]),
					error
			end;
		_ ->
			?ERROR_MSG("Invalid module spec given to ~p:stop_proxy_modules(): ~p~n",[?MODULE,MSpec]),
			error
	end,
	stop_proxy_modules(R);
stop_proxy_modules(_) -> ok.



%%
%% Local Functions
%%

