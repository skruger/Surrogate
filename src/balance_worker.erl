%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 17, 2010
%%% -------------------------------------------------------------------
-module(balance_worker).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([mnesia_init/0,start_pool/2,next/1,stop/1,state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {mode,hostlist,host_count,lasthost}).

-record(worker_pools,{name}).

%% ====================================================================
%% External functions
%% ====================================================================

mnesia_init() ->
	mnesia:create_table(worker_pools,[{attributes,record_info(fields,worker_pools)}]),
	mnesia:change_table_copy_type(worker_pools,node(),disc_copies).

	
start_pool(Poolname,Mode) ->
	Pools = proxyconf:get(balance_pools,[]),
	PoolProps = proplists:get_value(Poolname,Pools),
	case proplists:get_value(hosts,PoolProps,[]) of
		[] ->
			{error,nomembers};
		PoolMembers ->
			Pname = poolprocessname(Poolname),
			gen_server:start_link(Pname,?MODULE,[PoolMembers,Mode],[])
	end.
	
next(Pool) ->
	gen_server:call(poolprocessname(Pool),next).
stop(Pool) ->
	gen_server:call(poolprocessname(Pool),stop).

state(Pool) ->
	gen_server:call(poolprocessname(Pool),state).


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
	?INFO_MSG("members: ~p~n",[PoolMembers]),
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

poolprocessname(PoolName) ->
	{global,list_to_atom("worker_pool_"++atom_to_list(PoolName))}.
