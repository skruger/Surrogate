
-module(filter_headers).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([process_hook/4, start_instance/0]).

-behaviour(filter_stream).

-import(proxylib,[format_inet/1]).

-record(rewrite_host,{headers,config}).

%%

start_instance() ->
	{?MODULE,?MODULE}.
	

process_hook(_Pid,request,{request_header,ReqHdr,RequestSize},PPC) ->
	Cfg = PPC#proxy_pass.config,
	RawProps = proplists:get_value(?MODULE,Cfg,[]),
	{_Props,AddHdrs,RemHdrs} = parse_props(RawProps,[],[],[]),
	HBlock0 = ReqHdr#header_block.headers,
	HBlock1 =
		case proplists:get_value(xforwardfor,Cfg,false) of
			false -> HBlock0;
			_ ->
				try
					{PeerAddr,_} = PPC#proxy_pass.request_peer,
					HBlock0++[{'X-Forwarded-For',format_inet(PeerAddr)}]
%% 					proxylib:append_header("X-Forwarded-For: "++format_inet(PeerAddr),HBlock0)
				catch _:_ -> HBlock0 end
		  end,
	HBlock2 = proxylib:remove_headers(RemHdrs,HBlock1),
	HBlock3 = proxylib:append_headers(AddHdrs,HBlock2),
	ReqRec = ReqHdr#header_block.request,
	{VerMajor,VerMinor} = ReqRec#request_rec.protocol,
	HBlock4 = 
		case proplists:get_value(via,Cfg,true) of
			true ->
				{ok,Hostname} = inet:gethostname(),
				HBlock3++[{'Via',io_lib:format("HTTP/~p.~p ~s",[VerMajor,VerMinor,Hostname])}];
			HostStr when is_list(HostStr) ->
				HBlock3++[{'Via',io_lib:format("HTTP/~p.~p ~s",[VerMajor,VerMinor,HostStr])}];
			_ ->
				HBlock3
		end,
	RewriteHosts = proplists:get_all_values(rewrite_host,RawProps),
%% 	?ERROR_MSG("Got rewrite hosts: ~p~n~p~n",[RewriteHosts,RawProps]),
	#rewrite_host{headers=HBlock5} =
					 lists:foldl(fun rewrite_hosts/2,
								 #rewrite_host{headers=HBlock4,config=PPC},
								 RewriteHosts),
	{request_header,ReqHdr#header_block{headers=HBlock5},RequestSize};
process_hook(_Ref,_Type,Data,_PPC) ->
	Data.

rewrite_hosts({replace,Match,Replace},#rewrite_host{headers=HBlock}=Acc) ->
	Dict = dict:from_list(HBlock),
	case dict:find('Host',Dict) of
		{ok,Host} ->
			case re:replace(Host,Match,Replace) of
				Host ->
					Acc;
				NewHost ->
					NewDict = dict:store('Host',NewHost,Dict),
					Acc#rewrite_host{headers=dict:to_list(NewDict)}
			end;
		HostErr ->
			?ERROR_MSG("~p: Host header not found!~n~p~n",[?MODULE,HostErr]),
			Acc
	end;
rewrite_hosts({replace,Match,Replace,RedirectSpec}=RSpec,#rewrite_host{headers=HBlock,config=PPC}=Acc) ->
	Dict = dict:from_list(HBlock),
	case dict:find('Host',Dict) of
		{ok,Host} ->
			case re:replace(Host,Match,Replace) of
				Host ->
					Acc;
				NewHost ->
					NewDict = dict:store('Host',NewHost,Dict),
					{host,_Host,_Port} = TargetHost = proxylib:parse_host(RedirectSpec,80),
					TargetList = proxy_protocol:resolve_target_list(TargetHost,PPC#proxy_pass.config),
%% 		 			?ERROR_MSG("~p TargetList: ~p~n",[?MODULE,TargetList]),
					proxy_pass:setproxyaddr(PPC#proxy_pass.proxy_pass_pid,TargetList),
					Acc#rewrite_host{headers=dict:to_list(NewDict)}
			end;
		HostErr ->
			?ERROR_MSG("~p: Host header not found!~n~p~n",[?MODULE,HostErr]),
			Acc
	end;
rewrite_hosts(Opt,Acc) ->
	?ERROR_MSG("Unrecognized rewrite host option in ~p.~n~p~n",[?MODULE,Opt]),
	Acc.

parse_props([],PropAcc,AddAcc,RemAcc) ->
	{PropAcc,AddAcc,RemAcc};
parse_props([{add,Hdr}|R],PropAcc,AddAcc,RemAcc) ->
	parse_props(R,PropAcc,[Hdr|AddAcc],RemAcc);
parse_props([{remove,Hdr0}|R],PropAcc,AddAcc,RemAcc) ->
	Hdr = string:to_lower(Hdr0),
	parse_props(R,PropAcc,AddAcc,[Hdr|RemAcc]);
parse_props([V|R],PropAcc,AddAcc,RemAcc) ->
	parse_props(R,[V|PropAcc],AddAcc,RemAcc).
