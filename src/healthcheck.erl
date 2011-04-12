%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Apr 12, 2011
%%% -------------------------------------------------------------------
-module(healthcheck).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([behaviour_info/1]).
-export([start_checks/3,monitor_parent/1,report_status/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {pool,host,conf,checks}).
-record(check_state,{name,args,status}).

%% ====================================================================
%% External functions
%% ====================================================================

behaviour_info(callbacks) ->
	[{start_check,3}];
behaviour_info(_) ->
	undefined.

%%%  Health check definition.
%%start_check(Parent,Host,Conf) ->

start_checks(Pool,Host,Conf) ->
	gen_server:start_link(?MODULE,{Pool,Host,Conf},[]).

report_status({?MODULE,Pid},Status) ->
	gen_server:cast(Pid,{update_status,self(),Status}).

monitor_parent({?MODULE,Pid}) ->
	erlang:monitor(process,Pid).

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
init({Pool,Host,Conf}) ->
	Name = list_to_atom("cm_"++erlang:pid_to_list(self())),
	register(Name,self()),
	lists:foreach(fun(Check) ->
						  gen_server:cast(self(),{start_check,Check})
				  end,Conf),
    {ok, #state{pool=Pool,host=Host,conf=Conf,checks=dict:new()}}.

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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({start_check,{Check,Args}},State) ->
	Owner = {?MODULE,self()},
	try
		case Check:start_check(Owner,State#state.host,Args) of
			{ok,Pid} ->
				erlang:monitor(process,Pid),
				CheckState = #check_state{name=Check,args=Args,status=unknown},
				{noreply,State#state{checks=dict:store(Pid,CheckState,State#state.checks)}}
		end
	catch
		_:Err ->
			?ERROR_MSG("Could not start check: ~p~n~p:start_check(~p,~p,~p)~n",[Err,Check,Owner,State#state.host,Args]),
			{noreply,State}
	end;
handle_cast({update_status,Pid,Status},State) ->
	CheckState0 = dict:fetch(Pid,State#state.checks),
	CheckState = CheckState0#check_state{status=Status},
	Checks = dict:store(Pid,CheckState,State#state.checks),
%% 	?DEBUG_MSG("State updated: ~p ~p~n",[State#state.host,CheckState]),
	gen_server:cast(self(),check_host_state),
	{noreply,State#state{checks=Checks}};
handle_cast(check_host_state,State) ->
	IsDown = 
		lists:any(fun(CheckState) ->
						  CheckState#check_state.status == down
				  end,[X || {_Pid,X} <- dict:to_list(State#state.checks)]),
	if
		IsDown ->
			gen_balancer:set_host_state(State#state.pool,State#state.host,down);
		true ->
			gen_balancer:set_host_state(State#state.pool, State#state.host, up)
	end,
%% 	?DEBUG_MSG("IsDown: ~p~n~p~n",[IsDown,State]),
	{noreply, State};
handle_cast(Msg, State) ->
	?DEBUG_MSG("Unknown cast: ~p~n",[Msg]),
	{noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'DOWN',_Ref,process,_Pid,Reason},State) ->
	{stop,Reason,State};
handle_info(Info, State) ->
	?DEBUG_MSG("Unknown info: ~p~nState: ~p~n",[Info,State]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


