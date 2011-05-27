%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 17, 2010
%%% -------------------------------------------------------------------
-module(balance_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/3]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 init/1
        ]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Pool,BalanceMod,Conf) ->
	SupName = list_to_atom("balance_"++atom_to_list(Pool)++"_sup"),
	supervisor:start_link({local,SupName},?MODULE,{Pool,BalanceMod,Conf}).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init({Pool,BalanceMod,Conf}) ->
	CName = proxylib:get_pool_process(Pool),
	Child0 = {balancer,{gen_balancer,start_link,[CName,BalanceMod,Conf]},permanent,2000,worker,[]},
	HealthChecks = lists:map(fun(H) ->
									 {H,{healthcheck,start_checks,[Pool,H,proplists:get_value(checks,Conf,[])]},permanent,2000,worker,[]}
							 end, proplists:get_value(hosts,Conf,[])),
	%% TODO:  Add health check processes here 
	Children = [Child0|HealthChecks],
	{ok,{{one_for_one,5,10}, Children}}.



%% health_checks([],Acc) ->
%% 	Acc;
%% health_checks([H|R],Acc) ->
%% 	case H of
%% 		{host,Addr} ->
%% 			CkName = list_to_atom("healthcheck_"++integer_to_list(length(Acc))),
%% 			Child = {CkName,{},permanent,2000,worker,[]},
%% 			ok;
%% 		_ ->
%% 			health_checks(R,Acc)
%% 	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

