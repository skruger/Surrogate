%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 11, 2010
%%% -------------------------------------------------------------------
-module(proxyconf).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1,start_link/0,get_proxyconfig/0]).

-export([reload/0,get/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {config_terms}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	start_link(get_proxyconfig()).

start_link(Config) ->
	case file:consult(Config) of
		{ok,Terms} ->
			gen_server:start_link({local,?MODULE},?MODULE,#state{config_terms=Terms},[]);
		Err ->
			?CRITICAL("~p:start_link(~p) failed with file:consult() error: ~p~n",[?MODULE,Config,Err]),
			Err
	end.

get(Prop,Def) ->
	gen_server:call(?MODULE,{property,Prop,Def}).

reload() ->
	gen_server:cast(?MODULE,reload).

get_proxyconfig() ->
	case init:get_argument(proxyconfig) of
		{ok,[[Cfg|_]|_]} ->
			string:strip(Cfg,both,$");
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
init(State) ->
	case proplists:get_value(mysql_conn,State#state.config_terms,false) of
		{Host, Port, User, Password, Database} = MysqlConf ->
			F = fun() ->
						Spec = {mysql,{mysql,start_link,[mysql, Host, Port, User, Password, Database]},
								permanent,2000,worker,[]},
						case supervisor:start_child(surrogate_sup,Spec) of
							{error,_} = SupErr ->
								?CRITICAL("Error starting mysql with config: ~p~n~p~n",[MysqlConf,SupErr]);
							_ -> ok
						end
				end,
			spawn(F);
		Err ->
			?CRITICAL("Invalid {mysql_conn,{Host, Port, User, Password, Database}} configuration option.~n~p~n",[Err])
	end,
	%% Start filters as specified in proxy.conf
	Filters = proplists:get_value(start_filters,State#state.config_terms,[]),
	StartFilter = fun(F1) ->
						  Spec = F1:filter_childspec(),
						  case supervisor:start_child(surrogate_sup,Spec) of
							  {error,_} = FilterErr ->
								  ?CRITICAL("Got error ~p when starting ~p with spec:~n~p~n",[FilterErr,F1,Spec]);
							  _ -> ?DEBUG_MSG("~p started: ~p~n",[F1,Spec])
						  end
				  end,
	RunFilter = fun() -> lists:foreach(StartFilter,Filters) end,
	spawn(RunFilter),
	LogLevel = proplists:get_value(log_level,State#state.config_terms,5),
	surrogate_log:log_level(LogLevel),
    ?DEBUG_MSG("~p init with ~p~n",[?MODULE,State]),
	{ok, State#state{}}.

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
handle_call({property,Key,Default},_From,State) ->
	Ret = proplists:get_value(Key,State#state.config_terms,Default),
	{reply,Ret,State};
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
handle_cast(reload,State) ->
	{stop,reload,State};
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

