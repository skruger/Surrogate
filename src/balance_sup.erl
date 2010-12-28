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
-export([start_link/0,get_spec/0,pool_children/2]).

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

start_link() ->
	supervisor:start_link({local,?MODULE},?MODULE,[]).

get_spec() ->
	case proxyconf:get(balance_pools,[]) of
		[] ->
			[];
		_ ->
			[{balance_sup,
			  {balance_sup,start_link,[]},
			  permanent,
			  10000,
			  worker,
			  []}]
	end.


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	Pools = proxyconf:get(balance_pools,[]),
    Children = pool_children(Pools,[]),
	{ok,{{one_for_all,0,1}, Children}}.


pool_children([],C) ->
	C;
pool_children([{Pool,PoolMod,PoolProps}|R],C) ->
	CName = proxylib:get_pool_process(Pool),
	Child = {CName,{gen_balancer,start_link,[CName,PoolMod,PoolProps]},
			 permanent,2000,worker,[]},
	?DEBUG_MSG("Add child: ~p~n",[Child]),
	pool_children(R,[Child|C]).

%% 
%% pool_children([],C) ->
%% 	C;
%% pool_children([{Pool,PoolProps}|R],C) ->
%% 	CName = proxylib:get_pool_process(Pool),
%% 	Mode = proplists:get_value(mode,PoolProps,roundrobin),
%% 	case proplists:get_value(hosts,PoolProps,[]) of
%% 		[] ->
%% 			pool_children(R,C);
%% 		_Hosts ->
%% 			Child = {CName,{balancer,start_pool,[Pool,Mode]},
%% 					 permanent,2000,worker,[]},
%% 			pool_children(R,[Child|C])
%% 	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

