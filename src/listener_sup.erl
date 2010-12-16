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
	case L of
		{proxy_transparent,{ip,{A1,A2,A3,A4}},Port,_} = S ->
			Name = list_to_atom(lists:flatten(io_lib:format("~p_~p.~p.~p.~p:~p",[proxy_transparent,A1,A2,A3,A4,Port]))),
			Spec = {Name,{proxy_transparent,start_link,[S]},
					permanent, 10000,worker,[]},
			listen_childspec(R,[Spec|Acc]);
		{proxy_socks45,{ip,{A1,A2,A3,A4}},Port,_} = S ->
			Name = list_to_atom(lists:flatten(io_lib:format("~p_~p.~p.~p.~p:~p",[proxy_socks45,A1,A2,A3,A4,Port]))),
			Spec = {Name,{proxy_socks45,start_link,[S]},
					permanent,10000,worker,[]},
			listen_childspec(R,[Spec|Acc]);
		{Bal,{ip,{A1,A2,A3,A4}},Port,_} = S when Bal == balance_http ->
			Name = list_to_atom(lists:flatten(io_lib:format("~p_~p.~p.~p.~p:~p",[Bal,A1,A2,A3,A4,Port]))),
			Spec = {Name,{balance_http,start_link,[S]},
					permanent, 2000,worker,[]},
			listen_childspec(R,[Spec|Acc]);
		{Bal,{ip,{A1,A2,A3,A4}},Port,_,_,_} = S when Bal == balance_https ->
			Name = list_to_atom(lists:flatten(io_lib:format("~p_~p.~p.~p.~p:~p",[Bal,A1,A2,A3,A4,Port]))),
			Spec = {Name,{balance_http,start_link,[S]},
					permanent, 2000,worker,[]},
			listen_childspec(R,[Spec|Acc]);
		{http_management_api,RawBind,Port,Proplist} ->
%% 			{ok,HTTPD} = inets:start(httpd,[{port,8888},{bind_address,{0,0,0,0}},{server_name,"shaunkruger.com"},{server_root,"/tmp"},{document_root,"/tmp/docs"}]).
			{Bind,BindStr} = case RawBind of
								 {ip,{A1,A2,A3,A4}=B} ->
									 {B,lists:flatten(io_lib:format("~p.~p.~p.~p:~p",[A1,A2,A3,A4,Port]))};
								 A when is_list(A) ->
									 {A,lists:flatten(io_lib:format("~s:~p",[A,Port]))};
								 _ ->
									 ?CRITICAL("Invalid Bind address format: ~p~n",[RawBind]),
									 error
							 end,
			SRoot = "/tmp/http-"++BindStr,
			SDocRoot = SRoot++"/htdocs",
			file:make_dir(SRoot),
			file:make_dir(SDocRoot),
			Args = [httpd,Proplist++[{port,Port},{bind_address,Bind},{server_root,SRoot},{document_root,SDocRoot},{server_name,net_adm:localhost()}]],
			Name = list_to_atom("management_api_"++BindStr),
			Spec = {Name,{inets,start,Args},
					permanent,2000,worker,[]},
			listen_childspec(R,[Spec|Acc]);
		Undef ->
			?ERROR_MSG("Unsupported listen spec:~n~p~n",[Undef]),
			listen_childspec(R,Acc)
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

