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
-export([start_link/0,make_childspec/1,ip_listener_list/2,ip_sup_name/1]).

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
	ListenPropList = ip_listener_list(ListenSpec,[]),
	?DEBUG_MSG("Got listeners: ~n~p~n",[ListenPropList]),
	LCSpecs = lists:map(fun(X) -> ip_listener_childspec(X,ListenPropList) end,proplists:get_keys(ListenPropList)),
	Children = lists:flatten([listen_childspec(proplists:get_all_values({ip,{0,0,0,0}},ListenPropList),[])|LCSpecs]),
	{ok,{{one_for_one,15,5},
		 Children
		}};

init({{ip,_IP},Listeners}) ->
	{ok,{{one_for_one,15,5},
		 listen_childspec(Listeners,[])
		}};

init(Args) ->
	?DEBUG_MSG("Unknown arguments to init: ~p~n",[Args]),
	ignore.

%% init(ConfName) ->
%% 	CSpecs = proxy_childspecs(ConfName),
%%     ?DEBUG_MSG("~p supervisor init using config name: ~p.~n~p~n",[?MODULE,ConfName,CSpecs]),
%% 	{ok,{{one_for_one,15,5}, 
%% 		 CSpecs
%% 		 }}.

ip_listener_childspec({ip,{0,0,0,0}},_) -> [];
ip_listener_childspec({ip,_}=Key,ListenPropList) ->
	ListenSpecs = proplists:get_all_values(Key,ListenPropList),
	SupName = ip_sup_name(Key),
	{SupName,{supervisor,start_link,[{local,SupName},?MODULE,{Key,ListenSpecs}]},
	 permanent,10000,supervisor,[]}.
		
		
	
ip_sup_name({ip,IP}) ->
	list_to_atom(lists:flatten(io_lib:format("listener_~p_~p_~p_~p_sup",tuple_to_list(IP)))).

ip_listener_list([],Acc) -> Acc;
ip_listener_list([L|R],Acc) ->
	case tuple_to_list(L) of
%% 		[_,{ip,{0,0,0,0}}|_] ->
%% 			ip_listener_list(R,Acc);
		[_,{ip,_}=IP,_Port|_] ->
			ip_listener_list(R,[{IP,L}|Acc]);
		_ ->
			ip_listener_list(R,Acc)
	end.

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
		{rest_rpc,{ip,{A1,A2,A3,A4}},Port,_Opts} = S ->
			Name = list_to_atom(lists:flatten(io_lib:format("rest_rpc_~p.~p.~p.~p:~p",[A1,A2,A3,A4,Port]))),
			Spec = {Name,{rest_rpc,start_link,[S]},
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

%% ====================================================================
%% Internal functions
%% ====================================================================

