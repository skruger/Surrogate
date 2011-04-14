%file_comment
-module(surrogate_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1,
	 prep_stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([run/0,mnesia_init/0,run_test/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
run_test() ->
 	appmon:start(),
	run().

run() ->
	surrogate_log:restart_errorlog(),
	case init:get_argument(appmon) of
		{ok,_} ->
			appmon:start();
		_ ->
			true
	end,
	case init:get_argument(pidfile) of
		{ok,[[File|_]|_]} ->
			case filelib:is_file(File) of
				true ->
					io:format("Pid file already exists.  Exiting!~n",[]),
					error_logger:error_msg("Pid file already exists.  Exiting!~n",[]),
					halt(1),
					true;
				false ->
					Pid = os:getpid(),
					file:write_file(File,Pid)
			end;
		_ ->
			ok
	end,
	application:start(sasl),
	application:start(inets),
	application:start(mnesia),
%%     create_tables:init_tables(),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cluster_supervisor),
	application:load(surrogate),
	case application:start(surrogate) of
		ok ->
			?DEBUG_MSG("~p started.~n",[?MODULE]),
			ok;
		Err ->
			error_logger:error_msg("Surrogate was unable to start: ~p~n",[Err]),
			Err
	end.

mnesia_init() ->
	mnesia:create_schema([node()]).

%% ====================================================================!
%% External functions
%% ====================================================================!
%% --------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% --------------------------------------------------------------------
start(_Type, StartArgs) ->
	error_logger:info_msg("~p starting.~n",[?MODULE]),
    case surrogate_sup:start_link(StartArgs) of
	{ok, Pid} ->
		error_logger:info_msg("~p supervisor started: ~p~n",[?MODULE,Pid]),
	    {ok, Pid};
	Error ->
		error_logger:error_msg("~p Error: ~p~n",[?MODULE,Error]),
	    Error
    end.


prep_stop(State) ->
	%%proplists:get_value(modules,State#state.config_terms,[]),
	Filters = proxyconf:get(modules,[]),
	spawn(proxy_mod,stop_proxy_modules,[Filters]),
	?INFO_MSG("Shutting down modules.~n",[]),
	State.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

