%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 16, 2011
%%% -------------------------------------------------------------------
-module(filter_host_rewrite).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([process_hook/4, start_instance/0]).

-behaviour(filter_stream).

-import(proxylib,[format_inet/1]).

-record(rewrite_host,{headers,config}).


start_instance() ->
	{?MODULE,?MODULE}.

process_hook(_Pid,request,{request_header,ReqHdr,RequestSize},PPC) ->
	Cfg = PPC#proxy_pass.config,
	HBlock0 = ReqHdr#header_block.headers,
	RewriteHosts = proplists:get_all_values(?MODULE,Cfg),
%% 	?ERROR_MSG("Got rewrite hosts: ~p~n~p~n",[RewriteHosts,RawProps]),
	#rewrite_host{headers=HBlock1} =
					 lists:foldl(fun rewrite_hosts/2,
								 #rewrite_host{headers=HBlock0,config=PPC},
								 RewriteHosts),
	{request_header,ReqHdr#header_block{headers=HBlock1},RequestSize};
process_hook(_Pid,response,{response_header,ResHdr,ResponseSize}=Data,_PPC) ->
	HBlock0 = ResHdr#header_block.headers,
	case {get({?MODULE,rewrite}),proplists:get_value('Content-Type',HBlock0,none)} of
		{undefined,_Type} ->
%% 			?ERROR_MSG("Do nothing (host not rewritten)~nContent type: ~p~n",[Type]),
			ok;
		{_,"text/html"++_Rest} when ResponseSize /= close ->
			?ERROR_MSG("Can't rewrite responses that are fixed length!  Enable filter_force_chunk. (~p)~n",
					   [ResponseSize]),
			erase({?MODULE,rewrite});
		{_,"text/html"++_Rest} ->
%% 			?ERROR_MSG("Rewriting response:~n~p~nRequestSize:~p~n",[Data,ResponseSize]),
			ok;
		{_,Other} ->
			?ERROR_MSG("Not rewriting content type: ~p~n~p~n",[Other,HBlock0]),
			erase({?MODULE,rewrite})
	end,
	Data;
process_hook(_Pid,response,{response_data,Data},_PPC) ->
	case get({?MODULE,rewrite}) of
		undefined ->
			{response_data,Data};
		{OldHost,NewHost} ->
			NewData = iolist_to_binary(re:replace(Data,NewHost,OldHost,[global])),
%% 			?ERROR_MSG("Rewrite~nfrom: ~p~nto: ~p~ndata:~n~p~nnewdata:~n~p~n",
%% 					   [NewHost,OldHost,Data,NewData]),
			{response_data,NewData}
	end;
process_hook(_Ref,_Type,Data,_PPC) ->
	Data.

%% returns: {ok,Newheaders} | false
replace_host(Headers,Match,Replace) ->
	Dict = dict:from_list(Headers),
	case dict:find('Host',Dict) of
		{ok,Host} ->
			case re:replace(Host,Match,Replace) of
				Host ->
					false;
				NewHostIo ->
					NewHost = binary_to_list(iolist_to_binary(NewHostIo)),
					put({?MODULE,rewrite},{Host,NewHost}),
					%% Disable cache control headers when rewriting.
					Dict1 = dict:erase('Last-Modified',Dict),
					Dict2 = dict:erase('Etag',Dict1),
					NewDict = dict:store('Host',NewHost,Dict2),
					{ok,dict:to_list(NewDict)}
			end;
		HostErr ->
			?ERROR_MSG("~p: Host header not found!~n~p~n",[?MODULE,HostErr]),
			false
	end.

rewrite_hosts({replace,Match,Replace},#rewrite_host{headers=HBlock}=Acc) ->
	case replace_host(HBlock,Match,Replace) of
		{ok,Headers} ->
			Acc#rewrite_host{headers=Headers};
		false ->
			Acc
	end;
rewrite_hosts({replace,Match,Replace,RedirectSpec}=_RSpec,
			  #rewrite_host{headers=HBlock,config=PPC}=Acc) ->
	case replace_host(HBlock,Match,Replace) of
		{ok,Headers} ->
			{host,_Host,_Port} = TargetHost = proxylib:parse_host(RedirectSpec,80),
			TargetList = proxy_protocol:resolve_target_list(TargetHost,PPC#proxy_pass.config),
%% 		 	?ERROR_MSG("~p TargetList: ~p~n",[?MODULE,TargetList]),
			proxy_pass:setproxyaddr(PPC#proxy_pass.proxy_pass_pid,TargetList),
			Acc#rewrite_host{headers=Headers};
		false ->
			Acc
	end;
rewrite_hosts(Opt,Acc) ->
	?ERROR_MSG("Unrecognized rewrite host option in ~p.~n~p~n",[?MODULE,Opt]),
	Acc.

