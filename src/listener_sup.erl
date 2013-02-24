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
-export([start_link/0,make_childspec/1,ip_listener_list/2,ip_sup_name/1,ip_listener_childspec/1]).

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
	ListenPropList0 = ip_listener_list(ListenSpec,[]),
	?DEBUG_MSG("Got listeners: ~n~p~n",[ListenPropList0]),
	ListenPropList = ip_listener_filter_valid(ListenPropList0),
	?DEBUG_MSG("Using listeners: ~n~p~n",[ListenPropList]),
	LCSpecs = lists:map(fun(X) -> ip_listener_childspec(X,ListenPropList) end,proplists:get_keys(ListenPropList)),
	?DEBUG_MSG("Listener childspecs: ~n~p~n",[LCSpecs]),
	Children = lists:flatten([listen_childspec(proplists:get_all_values({ip,{0,0,0,0,0,0,0,0}},ListenPropList),[])++
								  listen_childspec(proplists:get_all_values({ip,{0,0,0,0}},ListenPropList),[])++
								  LCSpecs]),
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
ip_listener_childspec({ip,{0,0,0,0,0,0,0,0}},_) -> [];
ip_listener_childspec({ip,_}=Key,ListenPropList) ->
	ListenSpecs = proplists:get_all_values(Key,ListenPropList),
	SupName = ip_sup_name(Key),
	[{SupName,{supervisor,start_link,[{local,SupName},?MODULE,{Key,ListenSpecs}]},
	 permanent,10000,supervisor,[]}].
		
ip_listener_childspec({ip,_}= IP) ->
	SupName = ip_sup_name(IP),
	{SupName,{supervisor,start_link,[{local,SupName},?MODULE,{IP,[]}]},
	 permanent,10000,supervisor,[]}.
	
ip_sup_name({ip,IP}) ->
	list_to_atom(lists:flatten(io_lib:format("listener_~s_sup",[proxylib:format_inet(IP)]))).

ip_listener_list([],Acc) -> Acc;
ip_listener_list([L|R],Acc) ->
	case tuple_to_list(L) of
		[_,{ip,_}=IP0,_Port|_] ->
			IP = {ip,proxylib:inet_parse(IP0)},
			ip_listener_list(R,[{IP,L}|Acc]);
		_ ->
			ip_listener_list(R,Acc)
	end.

ip_listener_filter_valid(Listeners) ->
%% 	IPList0 = [ IP || {network_interfaces,_Host,_Dev,IP,_Alias} <- cluster_network_manager:get_interface_proplist()],
	IPList = [{ip,{0,0,0,0}},{ip,{0,0,0,0,0,0,0,0}}], %% |IPList0
	lists:filter(fun({IP,_}) ->
						 lists:member(IP,IPList)
				 end,Listeners).

listen_childspec([],Acc) ->
	Acc;
listen_childspec([L|R],Acc) ->
	SpecList = make_childspec(L),
	listen_childspec(R,Acc++SpecList).
	
make_childspec(L) ->
	try
		?DEBUG_MSG("make_childspec(~p)~n",[L]),
		case L of
			{listen_plain,{ip,IP0},Port,_} = S ->
				IP = proxylib:inet_parse(IP0),
				Name = list_to_atom(lists:flatten(io_lib:format("~p_~s:~p",[listen_plain,proxylib:format_inet(IP),Port]))),
				Spec = {Name,{proxy_listener,start_link,[S]},
						permanent, 2000,worker,[]},
				[Spec];
			{listen_ssl,{ip,IP0},Port,_} = S ->
				IP = proxylib:inet_parse(IP0),
				Name = list_to_atom(lists:flatten(io_lib:format("~p_~s:~p",[listen_ssl,proxylib:format_inet(IP),Port]))),
				Spec = {Name,{proxy_listener,start_link,[S]},
						permanent, 2000,worker,[]},
				[Spec];
			Undef ->
				?ERROR_MSG("Unsupported listen spec:~n~p~n",[Undef]),
				[]
		end
	catch
		_:CErr ->
			?ERROR_MSG("Error processing listen spec:~n~p~n~p",[CErr,L]),
			[]
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

