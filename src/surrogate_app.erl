%file_comment
-module(surrogate_app).

-behaviour(application).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Behavioural exports
%% --------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
        ]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([run/0,mnesia_init/0]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
run() ->
	appmon:start(),
	application:start(sasl),
	application:start(mnesia),
    create_tables:init_tables(),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:load(surrogate),
	application:start(surrogate).

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

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop(_State) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

