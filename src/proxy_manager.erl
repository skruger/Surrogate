%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 16, 2010
%%% -------------------------------------------------------------------
-module(proxy_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pid}).

%% ====================================================================
%% External functions
%% ====================================================================

start(MgrSpec,Name) ->
 	gen_server:start_link({local,Name},?MODULE,[MgrSpec],[]).

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
init([MgrSpec]) ->
	process_flag(trap_exit, true),
	case start_manager(MgrSpec) of
		{ok,Pid} ->
			erlang:monitor(process,Pid),
			?DEBUG_MSG("Started with pid ~p~n",[Pid]),
			{ok, #state{pid=Pid}};
		{error,{already_started,Pid}} ->
			erlang:monitor(process,Pid),
			?WARN_MSG("Already started with pid ~p~n",[Pid]),
			{ok, #state{pid=Pid}};
		Err ->
			?ERROR_MSG("Error starting ~p: ~p~n",[?MODULE,Err]),
			{stop,error}
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'DOWN',_,process,Pid,Reason},State) when State#state.pid == Pid ->
	?WARN_MSG("inets httpd server died unexpectedly: ~p~nClosing ~p so supervisor can restart.~n",[Reason,?MODULE]),
	{stop,Reason,State};
handle_info(Info, State) ->
	?DEBUG_MSG("Got info: ~p~n",[Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->
	R = inets:stop(httpd,State#state.pid),
	?DEBUG_MSG("inets:stop returned: ~p~n",[R]),
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


start_manager({http_management_api,{ip,IP0},Port,Proplist}) ->
	IP = proxylib:inet_parse(IP0),
	BindStr = lists:flatten(io_lib:format("~s:~p",[proxylib:format_inet(IP),Port])),
	SRoot = "/tmp/http-"++BindStr,
	SDocRoot = SRoot++"/htdocs",
	file:make_dir(SRoot),
	file:make_dir(SDocRoot),
	DefaultProps = [{port,Port},{bind_address,IP},{server_root,SRoot},{document_root,SDocRoot},
					{server_name,net_adm:localhost()},{erl_script_alias,{"/rpc",[surrogate_api]}},
					{modules,[mod_esi,mod_alias]},{error_log,"error.log"}],
	AllProps = update_http_manager_properties(Proplist,DefaultProps),
	?DEBUG_MSG("inets:start(httpd,~p)~n",[AllProps]),
	case inets:start(httpd,AllProps) of
		{ok,Pid} = R ->
			link(Pid),
			R;
		Err ->
			Err
	end.
		
	

update_http_manager_properties([],Props) -> Props;
update_http_manager_properties([P|R],Props) ->
	case P of
		{server_admin,_} = Adm ->
			update_http_manager_properties(R,[Adm|Props]);
		{server_name,_Name} = SN ->
			Prop1 = 
				case proplists:is_defined(server_name,Props) of
					true ->
						proplists:delete(server_name,Props);
					_ ->
						Props
				end,
			update_http_manager_properties(R,[SN|Prop1]);
		_ ->
			?WARN_MSG("Invalid http_management_api property: ~p~n",[P]),
			update_http_manager_properties(R,Props)
	end.

