%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 17, 2010
%%% -------------------------------------------------------------------
-module(balancer).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_pool/2,next/1,stop/1,state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {mode,hostlist,host_count,lasthost}).

%% ====================================================================
%% External functions
%% ====================================================================

start_pool(Poolname,Mode) ->
	Pools = proxyconf:get(balance_pools,[]),
	PoolProps = proplists:get_value(Poolname,Pools),
	case proplists:get_value(hosts,PoolProps,[]) of
		[] ->
			{error,nomembers};
		PoolMembers ->
			Pname = proxylib:get_pool_process(Poolname),
			gen_server:start_link(Pname,?MODULE,[PoolMembers,Mode],[])
	end.
	
next(Pool) ->
	gen_server:call(proxylib:get_pool_process(Pool),next).
stop(Pool) ->
	gen_server:call(proxylib:get_pool_process(Pool),stop).

state(Pool) ->
	gen_server:call(proxylib:get_pool_process(Pool),state).


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
init([PoolMembers,Mode]) ->
	io:format("members: ~p~n",[PoolMembers]),
    {ok, #state{mode=Mode,hostlist=PoolMembers,lasthost=0,host_count=length(PoolMembers)}}.

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
handle_call(next,_From,State) when State#state.mode == roundrobin ->
	NextHost = (State#state.lasthost + 1) rem State#state.host_count,
%% 	io:format("~p returning host: ~p~n",[?MODULE,NextHost+1]),
	Host = lists:nth(NextHost+1,State#state.hostlist),
	{reply,Host,State#state{lasthost=NextHost}};
handle_call(stop,From,State) ->
	gen_server:reply(From,stop),
	{stop,normal,State};
handle_call(state,_From,State) ->
	{reply,State,State};
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
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
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

