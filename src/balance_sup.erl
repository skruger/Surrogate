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

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0,get_spec/0]).

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
    PChildren = pool_children(Pools,[]),
	Listen = proxyconf:get(listeners,[]),
	BChildren = balance_children(Listen,[]),
	Children = PChildren++BChildren,
%% 	io:format("Staring ~p~n~p~n",[?MODULE,Children]),
	{ok,{{one_for_all,0,1}, Children}}.

balance_children([],C) ->
	C;
balance_children([B|R],C) ->
%% 	Id = list_to_atom("balance_"++integer_to_list(length(C)+1)),
	Spec = {B,{balance_http,start_link,[B]},
			permanent,
			2000, worker,[]},
	balance_children(R,[Spec|C]).

pool_children([],C) ->
	C;
pool_children([{Pool,PoolProps}|R],C) ->
	CName = proxylib:get_pool_process(Pool),
	Mode = proplists:get_value(mode,PoolProps,roundrobin),
	case proplists:get_value(hosts,PoolProps,[]) of
		[] ->
			pool_children(R,C);
		_Hosts ->
			Child = {CName,{balancer,start_pool,[Pool,Mode]},
					 permanent,2000,worker,[]},
			pool_children(R,[Child|C])
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

