
-module(balance_round_robin).

%%
%% Include files
%%
-include("surrogate.hrl").

-behaviour(gen_balancer).
%%
%% Exported Functions
%%
-export([next/2, init/1]).

-record(state_rr,{offset}).

%%
%% API Functions
%%

init(_Args) ->
	{ok,#state_rr{offset=0}}.

next(_ClientInfo,State) ->
	LocalState = State#gen_balancer_state.local_state,
	NewOffset = (LocalState#state_rr.offset + 1) rem length(State#gen_balancer_state.active_pool),
%% 	?DEBUG_MSG("Offset: ~p~n",[NewOffset]),
	Reply = lists:nth(NewOffset+1,State#gen_balancer_state.active_pool),
	{reply,Reply,State#gen_balancer_state{local_state=LocalState#state_rr{offset=NewOffset}}}.

%%
%% Local Functions
%%

