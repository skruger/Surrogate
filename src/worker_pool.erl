%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 17, 2010
%%% -------------------------------------------------------------------
-module(worker_pool).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_pool/2,next/1,stop/1,state/1,refresh_nodes/1,processname/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name,mode,hostlist,host_count,lasthost}).


%% ====================================================================
%% External functions
%% ====================================================================

	
start_pool(Poolname,Mode) ->
	case mnesia:dirty_read(worker_pool,Poolname) of
		[] ->
			{error,nopool};
		[#worker_pool{}|_] ->
			Pname = processname(Poolname),
			gen_server:start_link({local,Pname},?MODULE,[Mode,Poolname],[])
	end.
	
next('NONE') ->
	node();
next(Pool) ->
	try
		gen_server:call(processname(Pool),next)
	catch
		_:Err ->
			?ERROR_MSG("Error calling ~p:next() for pool ~p: ~p~n",[?MODULE,Pool,Err]),
			node()
	end.

stop(Pool) ->
	gen_server:call(processname(Pool),stop).

state(Pool) ->
	gen_server:call(processname(Pool),state).

refresh_nodes(Pool) ->
	gen_server:cast(processname(Pool),refresh_nodes).


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
init([Mode,PoolName]) ->
	gen_server:cast(self(),refresh_nodes),
	{ok, #state{mode=Mode,hostlist=[],lasthost=0,host_count=0,name=PoolName}}.

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
handle_cast(refresh_nodes,State) ->
	NewNodes = 
	lists:filter(fun(Node) ->
						 case mnesia:dirty_read(worker_node_pool,{State#state.name,Node}) of
							 [#worker_node_pool{active=true}|_] ->
								 true;
							 [#worker_node_pool{active=false}|_] ->
								 false;
							 Bad ->
								 ?DEBUG_MSG("refresh_nodes got non-matching value: ~p~n",[Bad]),
								 false
						 end end,
				 mod_worker_manager:list_pool_nodes(State#state.name)),
	{noreply,State#state{hostlist=NewNodes,host_count=length(NewNodes)}};
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

processname(PoolName) ->
	list_to_atom("worker_pool_"++atom_to_list(PoolName)).
