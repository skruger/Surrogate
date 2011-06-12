
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
					proxylib:append_header("X-Forwarded-For: "++format_inet(PeerAddr),HBlock0)
				catch _:_ -> HBlock0 end
		  end,
	HBlock2 = proxylib:remove_headers(RemHdrs,HBlock1),
	HBlock3 = proxylib:append_headers(AddHdrs,HBlock2),
	{request_header,ReqHdr#header_block{headers=HBlock3},RequestSize};
process_hook(_Ref,_Type,Data,_PPC) ->
	Data.

parse_props([],PropAcc,AddAcc,RemAcc) ->
	{PropAcc,AddAcc,RemAcc};
parse_props([{add,Hdr}|R],PropAcc,AddAcc,RemAcc) ->
	parse_props(R,PropAcc,[Hdr|AddAcc],RemAcc);
parse_props([{remove,Hdr0}|R],PropAcc,AddAcc,RemAcc) ->
	Hdr = string:to_lower(Hdr0),
	parse_props(R,PropAcc,AddAcc,[Hdr|RemAcc]);
parse_props([V|R],PropAcc,AddAcc,RemAcc) ->
	parse_props(R,[V|PropAcc],AddAcc,RemAcc).
