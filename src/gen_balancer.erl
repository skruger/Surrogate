%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 25, 2010
%%% -------------------------------------------------------------------
-module(gen_balancer).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").



%% --------------------------------------------------------------------
%% External exports
-export([start_link/3,start/2, next/2,behaviour_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -record(state, {balancer_mod,balancer_state}).

%% ====================================================================
%% External functions
%% ====================================================================
behaviour_info(callbacks) ->
	[{init,1}, {next,2}];
behaviour_info(_) ->
	undefined.

start_link(Name,Mod,Args) ->
	?DEBUG_MSG("~p ~p ~p~n",[Name,?MODULE,[Mod,Args]]),
	gen_server:start_link(Name,?MODULE,[Mod,Args],[]).

start(Mod,Args) ->
	gen_server:start(Mod,[Mod,Args],[]).

next(Pool,ClientInfo) ->
	Ref = proxylib:get_pool_process(Pool),
	gen_server:call(Ref,{get_next,ClientInfo}).


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
init([Mod,Args]) ->
	?DEBUG_MSG("Starting: ~p ~p~n",[Mod,Args]),
	State = init_state(Args),
	case apply(Mod,init,[Args]) of
		{ok,LState} ->
			{ok,State#gen_balancer_state{balancer_mod=Mod,local_state=LState}};
		{ok,LState,Timeout} ->
			{ok,State#gen_balancer_state{balancer_mod=Mod,local_state=LState},Timeout};
		Err ->
			Err
	end.
    
init_state(Args) ->
	init_state(Args,#gen_balancer_state{pool=[],active_pool=[]}).

init_state([],State) ->
	State;
init_state([A|R],State) ->
	case A of
		{host,Host} ->
			Pool = [Host|(State#gen_balancer_state.pool)],
			?DEBUG_MSG("Host added: ~p~n",[Host]),
			init_state(R,State#gen_balancer_state{pool=Pool,active_pool=Pool});
%% 		{hosts,HList} ->
%% 			init_state(R,State#gen_balancer_state{pool=HList,active_pool=HList});
		Unk ->
			?WARN_MSG("Invalid balancer option: ~p~n",[Unk]),
			init_state(R,State)
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
handle_call({get_next,ClientInfo}, _From, State) ->
	apply(State#gen_balancer_state.balancer_mod,next,[ClientInfo,State]);
%% 	Reply = apply(State#gen_balancer_state.balancer_mod,next,[ClientInfo,State]),
%%     {reply, Reply, State};
handle_call(Err,_From,State) ->
	?ERROR_MSG("Got unknown call: ~p~n",[Err]),
	{reply,ok,State}.

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

