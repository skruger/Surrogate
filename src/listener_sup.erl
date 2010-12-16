%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Oct 30, 2010
%%% -------------------------------------------------------------------
-module(listener_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).

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

%% start_link(Args) ->
%% 	?DEBUG_MSG("Starting ~p~n",[?MODULE]),
%% 	supervisor:start_link({local,?MODULE},?MODULE,Args).

start_link() ->
	?DEBUG_MSG("Starting ~p~n",[?MODULE]),
	supervisor:start_link({local,?MODULE},?MODULE,[]).


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
	ListenSpec = proxyconf:get(listeners,[]),
	?DEBUG_MSG("Got listeners: ~n~p~n",[ListenSpec]),
	{ok,{{one_for_one,15,5},
		 listen_childspec(ListenSpec,[])
		}}.

%% init(ConfName) ->
%% 	CSpecs = proxy_childspecs(ConfName),
%%     ?DEBUG_MSG("~p supervisor init using config name: ~p.~n~p~n",[?MODULE,ConfName,CSpecs]),
%% 	{ok,{{one_for_one,15,5}, 
%% 		 CSpecs
%% 		 }}.

listen_childspec([],Acc) ->
	Acc;
listen_childspec([L|R],Acc) ->
	SpecList = make_childspec(L),
	listen_childspec(R,Acc++SpecList).
	
make_childspec(L) ->
	case L of
		{proxy_transparent,{ip,{A1,A2,A3,A4}},Port,_} = S ->
			Name = list_to_atom(lists:flatten(io_lib:format("~p_~p.~p.~p.~p:~p",[proxy_transparent,A1,A2,A3,A4,Port]))),
			Spec = {Name,{proxy_transparent,start_link,[S]},
					permanent, 10000,worker,[]},
			[Spec];
		{proxy_socks45,{ip,{A1,A2,A3,A4}},Port,_} = S ->
			Name = list_to_atom(lists:flatten(io_lib:format("~p_~p.~p.~p.~p:~p",[proxy_socks45,A1,A2,A3,A4,Port]))),
			Spec = {Name,{proxy_socks45,start_link,[S]},
					permanent,10000,worker,[]},
			[Spec];
		{Bal,{ip,{A1,A2,A3,A4}},Port,_} = S when Bal == balance_http ->
			Name = list_to_atom(lists:flatten(io_lib:format("~p_~p.~p.~p.~p:~p",[Bal,A1,A2,A3,A4,Port]))),
			Spec = {Name,{balance_http,start_link,[S]},
					permanent, 2000,worker,[]},
			[Spec];
		{Bal,{ip,{A1,A2,A3,A4}},Port,_,_,_} = S when Bal == balance_https ->
			Name = list_to_atom(lists:flatten(io_lib:format("~p_~p.~p.~p.~p:~p",[Bal,A1,A2,A3,A4,Port]))),
			Spec = {Name,{balance_http,start_link,[S]},
					permanent, 2000,worker,[]},
			[Spec];
		{http_management_api,RawBind,Port,_Proplist} = S ->
			BindStr = case RawBind of
						  {ip,{A1,A2,A3,A4}} ->
							  lists:flatten(io_lib:format("~p.~p.~p.~p:~p",[A1,A2,A3,A4,Port]));
						  A when is_list(A) ->
							  lists:flatten(io_lib:format("~s:~p",[A,Port]));
						  _ ->
							  ?CRITICAL("Invalid Bind address format: ~p~n",[RawBind]),
							  error
					  end,
			Name = list_to_atom("management_api_"++BindStr),
			Spec = {Name,{proxy_manager,start,[S]},
					permanent,30000,worker,[]},
			[Spec];
		Undef ->
			?ERROR_MSG("Unsupported listen spec:~n~p~n",[Undef]),
			[]
	end.



%% 
%% proxy_childspecs(ConfName) ->
%% 	Conf = proxyconf:get(ConfName,[]),
%% 	proxy_childspec_list(Conf,[]).
%% 
%% proxy_childspec_list([],Acc) ->
%% 	Acc;
%% proxy_childspec_list([{CName,Props}|R],Acc) ->
%% 	Spec = {CName,
%% 			{CName,start_link,[Props]},
%% 			permanent,
%% 			10000,
%% 			worker,
%% 			[]},
%% 	proxy_childspec_list(R,[Spec|Acc]).

%% ====================================================================
%% Internal functions
%% ====================================================================

