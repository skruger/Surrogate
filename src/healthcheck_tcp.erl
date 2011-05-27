%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Apr 12, 2011
%%% -------------------------------------------------------------------
-module(healthcheck_tcp).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_check/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {parent,host,conf,interval}).

%% ====================================================================
%% External functions
%% ====================================================================

start_check(Parent,Host,Conf) ->
	gen_server:start(?MODULE,{Parent,Host,Conf},[]).

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
init({Parent,Host,Conf}) ->
	TMSec = proplists:get_value(interval,Conf,30)*1000,
	healthcheck:monitor_parent(Parent),
	erlang:send_after(100,self(),check),
	{ok, #state{parent=Parent,host=Host,conf=Conf,interval=TMSec}}.

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
handle_cast(check,State) ->
	Host = State#state.host,
	Port = proplists:get_value(port,State#state.conf,80),
	case gen_tcp:connect(Host,Port,[]) of
		{ok,Sock} ->
			healthcheck:report_status(State#state.parent, up),
			gen_tcp:close(Sock);
		_Err ->
			healthcheck:report_status(State#state.parent, down)
	end,
	{noreply,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(check,State) ->
	gen_server:cast(self(),check),
	erlang:send_after(State#state.interval,self(),check),
	{noreply,State};
handle_info({'DOWN',_Ref,process,_Pid,_Reason}=Down,State) ->
	?WARN_MSG("Heathcheck parent went away: ~p~n~p~n",[Down,State]),
	{stop,normal,State};
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

