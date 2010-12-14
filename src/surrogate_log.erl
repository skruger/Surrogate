%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 24, 2010
%%% -------------------------------------------------------------------
-module(surrogate_log).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,start_link/1,start_link/2,append/3,access/4,log_level/1,get_accesslog/0,get_errorlog/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {errorlog,accesslog,log_level}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).
start_link(LogFile) ->
	gen_server:start_link({local,?MODULE},?MODULE,[LogFile],[]).
start_link(ErrorLog,AccessLog) ->
	gen_server:start_link({local,?MODULE},?MODULE,[ErrorLog,AccessLog],[]).


append(Level,Mod,Message) ->
	gen_server:cast(?MODULE,{log,Level,Mod,Message}).

access(Code,Url,User,Extra) ->
	gen_server:cast(?MODULE,{access_log,Code,Url,User,Extra}).

log_level(Lvl) ->
	gen_server:cast(?MODULE,{log_level,Lvl}).

get_accesslog() ->
	case init:get_argument(accesslog) of
		{ok,[[AccessLog|_]|_]} ->
			string:strip(AccessLog,both,$");
		_ ->
			none
	end.
get_errorlog() ->
	case init:get_argument(errorlog) of
		{ok,[[ErrorLog|_]|_]} ->
			string:strip(ErrorLog,both,$");
		_ ->
			none
	end.

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	try
	ErrLog = case get_errorlog() of
				 none ->
					 undefined;
				 EFileName ->
					 case file:open(EFileName,[write,append,delayed_write]) of
						 {ok,EFile} ->
							 EFile;
						 EErr ->
							 error_logger:error_msg("Could not open error log file ~p (~p)~n",[EFileName,EErr]),
							 undefined
					 end
			 end,
	AccessLog = case get_accesslog() of
					none -> undefined;
					AFileName ->
						case file:open(AFileName,[write,append,delayed_write]) of
							{ok,AFile} ->
								AFile;
							AErr ->
								error_logger:error_msg("Could not open access log file ~p (~p)~n",[AFileName,AErr]),
								undefined
						end
				end,
	{ok,#state{errorlog=ErrLog,accesslog=AccessLog,log_level=5}}
	catch
		_:Err ->
			io:format("Error starting ~p: ~p~n",[?MODULE,Err]),
			error_logger:error_msg("Error starting ~p: ~p~n",[?MODULE,Err])
	end;
init([FileName]) ->
	case file:open(FileName,[write,append]) of
		{ok,File} ->
			{ok,#state{errorlog=File,log_level=5}};
		Err ->
			{stop,Err}
	end;
init([ErrorFile,AccessLog|_]) ->
	case file:open(ErrorFile,[write,append]) of
		{ok,File} ->
			case file:open(AccessLog,[write,append]) of
				{ok,Access} ->
					{ok,#state{errorlog=File,accesslog=Access,log_level=5}};
				Err ->
					?CRITICAL("Could not open access log file: ~p.~n~p~n",[AccessLog,Err]),
					{ok,#state{errorlog=File,log_level=5}}
			end;
		Err ->
			error_logger:error_msg("Could not open error log file: ~p.~n",[ErrorFile]),
			{stop,Err}
	end.


%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({log,Level,Mod,Message},State) when (State#state.log_level >= Level) and (State#state.errorlog /= undefined) ->
	Msg = lists:flatten(io_lib:format("===== ~s ~s =====~n~p~n~s~n",[log_level_text(Level),timestamp(),Mod,Message])),
	file:write(State#state.errorlog,Msg),
	{noreply,State};
handle_cast({access_log,Code,Url,User,Extra},State) when State#state.accesslog /= undefined ->
	Log = lists:flatten(io_lib:format("~s ~p ~s ~s - ~s~n",[timestamp(),Code,Url,format_extra(Extra),format_user(User)])),
	file:write(State#state.accesslog,Log),
	{noreply,State};
handle_cast({log_level,Lvl},State) ->
	?CRITICAL("Setting log level to ~p",[Lvl]),
	{noreply,State#state{log_level=Lvl}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

format_user(undefined) ->
	"None: no user";
format_user(User) when is_record(User,proxy_userinfo) ->
	"HTTP: "++User#proxy_userinfo.username;
format_user({socks4_user,User}) ->
	"SOCKS4: "++User;
format_user({socks5_user,User}) when is_atom(User)->
	"SOCKS5: "++lists:flatten(io_lib:format("~p",[User]));
format_user({socks5_user,User}) when is_list(User)->
	"SOCKS5: "++User;
format_user(User) when is_list(User) ->
	"Other: "++User;
format_user(User) ->
	?ERROR_MSG("Invalid user information passed to format_user(): ~p~n",[User]),
	"Bad user info see error log".

format_extra(Extra) when is_list(Extra) ->
	Extra;
format_extra(Extra) ->
	lists:flatten(io_lib:format("~p",[Extra])).

timestamp() ->
	httpd_util:rfc1123_date(calendar:now_to_local_time(now())).

log_level_text(0) ->
	"CRITICAL";
log_level_text(1) ->
	"ERROR";
log_level_text(2) ->
	"WARNING";
log_level_text(3) ->
	"INFO";
log_level_text(4) ->
	"DEBUG";
log_level_text(5) ->
	"DEBUG+".
