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
process_hook(_Pid,response,{response_header,ResHdr,ResponseSize}=_Data,_PPC) ->
	HBlock0 = ResHdr#header_block.headers,
	case {get({?MODULE,rewrite}),proplists:get_value('Content-Type',HBlock0,none)} of
		{undefined,_Type} ->
%% 			?ERROR_MSG("Do nothing (host not rewritten)~nContent type: ~p~n",[Type]),
			ok;
		{_,"text/html"++_Rest} when ResponseSize /= close,
									ResponseSize /= chunked ->
			?ERROR_MSG("Can't rewrite responses that are fixed length!  "
						"Enable filter_force_chunk. (~p)~n",
					   [ResponseSize]),
			erase({?MODULE,rewrite});
		{_,"text/"++_Rest} -> ok;
		{_,_Other} ->
%% 			?ERROR_MSG("Not rewriting content type: ~p~n~p~n",[Other,HBlock0]),
			erase({?MODULE,rewrite})
	end,
	HBlock1 = lists:filter(fun({'Etag',_}) -> false;
							  ({'Last-Modified',_}) -> false;
							  ({'Expires',_}) -> false;
							  (_) -> true end, HBlock0),
	HBlock2 = [{'X-Surrogate-Host-Rewrite',"true"}|HBlock1],
	Dict = dict:from_list(HBlock2),
	HBlock3 =
		case dict:find('Location',Dict) of
			{ok,Location} ->
				case get({?MODULE,rewrite}) of
					undefined ->
						HBlock2;
					{OldHost,NewHost} ->
						LocIoBin = re:replace(Location,NewHost,OldHost,[global]),
						NewLocationBin = iolist_to_binary(LocIoBin),
						NewLocation = binary_to_list(NewLocationBin),
						?ERROR_MSG("Redirect rewrite:~n"
									"Location: ~p~nNewLocation: ~p~n",
								   [Location,NewLocation]),
						HBlockNoLoc = lists:filter(fun({'Location',_}) -> false;
													 (_) -> true end,HBlock2),
						[{'Location',NewLocation}|HBlockNoLoc]
				end;
			_ -> HBlock2
		end,
%% 	?ERROR_MSG("HBlock3: ~p~n",[HBlock3]),
	{response_header,ResHdr#header_block{headers=HBlock3},ResponseSize};
process_hook(_Pid,response,{response_data,Data},PPC) ->
	case get({?MODULE,rewrite}) of
		undefined ->
			{response_data,Data};
		{_OldHost,_NewHost} ->
			case get({?MODULE,response_data}) of
				undefined ->
					put({?MODULE,response_data},Data);
				ExistingData ->
					put({?MODULE,response_data},[ExistingData,Data])
			end,
			PPC#proxy_pass.proxy_pass_pid ! get_response_data,
			delay
	end;
process_hook(_Pid,response,{end_response_data,_} = EndMsg,PPC) ->
	case get({?MODULE,response_data}) of
		undefined ->
			EndMsg;
		DataList ->
%% 			?ERROR_MSG("Ending response:~n~p~n",[DataList]),
			OrigResponse = iolist_to_binary(DataList),
			NewResponse =
			case get({?MODULE,rewrite}) of
				undefined ->
					OrigResponse;
				{OldHost,NewHost} ->
%% 					?ERROR_MSG("Rewrite~nfrom: ~p~nto: ~p~n",
%% 							   [NewHost,OldHost]),
					iolist_to_binary(re:replace(OrigResponse,NewHost,OldHost,[global]))
			end,
			PPC#proxy_pass.proxy_pass_pid ! {filter_delay,{response_data,NewResponse}},
			PPC#proxy_pass.proxy_pass_pid ! {filter_delay,EndMsg},
			erase({?MODULE,rewrite}),
			erase({?MODULE,response_data}),
			delay
	end;
process_hook(_Ref,_Type,Data,_PPC) ->
	Data.

%% returns: {ok,Newheaders} | false
replace_host(Headers,Match,Replace) ->
	case proplists:get_value('Host',Headers,error) of
		error ->
			?ERROR_MSG("~p: Host header not found!~n~p~n",[?MODULE,Headers]),
			false;
		{ok,Host} ->
			case re:replace(Host,Match,Replace) of
				Host ->
					false;
				NewHostIo ->
					NewHost = binary_to_list(iolist_to_binary(NewHostIo)),
					put({?MODULE,rewrite},{Host,NewHost}),
					%% Disable cache control headers when rewriting.
					Headers1 =
					lists:filter(fun({'Last-Modified',_}) -> false;
									({'Etag',_}) -> false;
									({'Host',_}) -> false;
									(_) -> true end, Headers),
					{ok,[{'Host',NewHost}|Headers1]}
			end
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
			TargetList = proxy_protocol:resolve_target_list(TargetHost,
															PPC#proxy_pass.config),
%% 		 	?ERROR_MSG("~p TargetList: ~p~n",[?MODULE,TargetList]),
			proxy_pass:setproxyaddr(PPC#proxy_pass.proxy_pass_pid,TargetList),
			Acc#rewrite_host{headers=Headers};
		false ->
			Acc
	end;
rewrite_hosts(Opt,Acc) ->
	?ERROR_MSG("Unrecognized rewrite host option in ~p.~n~p~n",[?MODULE,Opt]),
	Acc.

