%% file_comment
-module(proxylib).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([header2dict/1,parse_host/2,parse_request/1,parse_response/1,combine_headers/1,build_header_list/1,split_headers/1,find_binary_pattern/2,method_has_data/2]).

%% -export([send/2,setopts/2]).

-export([proxy_error/2]).

-export([get_pool_process/1,remove_headers/2,remove_header/2,append_header/2,append_headers/2,replace_header/3,timestamp/0]).
-export([inet_parse/1,format_inet/1,inet_version/1,inet_getaddr/1,inet_getaddr/2,uri_unescape/1,string_to_term/1]).

-export([split_hop_headers/1]).

%% -export([re/1]).
%%
%% API Functions
%%

proxy_error(Code ,Err) ->
  {error, Code, lists:flatten(io_lib:format("Internal proxy error: ~p",[Err]))}.

header2dict(Hdr) ->
	?ERROR_MSG("header2dict() is deprecated!~n~p~n",[erlang:get_stacktrace()]),
	header2dict(Hdr,[]).

header2dict([],List)->
	dict:from_list(List);
header2dict([Hdr|R],Acc)->
	case string:str(Hdr,":") of
		Idx ->
			Key = string:sub_string(Hdr,1,Idx-1),
			Val = string:strip(string:sub_string(Hdr,Idx+1),left,$ ),
			header2dict(R,[{string:to_lower(Key),Val}|Acc])
	end.

split_hop_headers(HdrList) ->
  lists:partition(fun({Hdr,_}) -> lists:member(Hdr, ?HOP_HEADERS) end, HdrList).




remove_headers([],Hdr) ->
	Hdr;
remove_headers([H|R],Hdr) ->
	remove_headers(R,remove_header(H,Hdr)).

remove_header(Name,Hdr) ->
	lists:filter(fun({N,_}) when N == Name -> false;
					(_) -> true end,Hdr).

replace_header(Name,NewHdr,HdrBlock) ->
	replace_header(Name,NewHdr,HdrBlock,[]).
replace_header(_,_,[],Acc) ->
	lists:reverse(Acc);
replace_header(Name,NewHdr,[Hdr|R],Acc) ->
	Idx = string:str(Hdr,":"),
	case string:to_lower(string:sub_string(Hdr,1,Idx-1)) of
		Name ->
			lists:reverse(Acc)++[NewHdr|R];
		_ ->
			replace_header(Name,NewHdr,R,[Hdr|Acc])
	end.

append_headers([],Block) ->
	Block;
append_headers([H|R],Block) ->
	append_headers(R,append_header(H,Block)).

append_header(Header,Block) ->
	Block++[Header].

parse_host(Host,DefPort) ->
	case string:str(Host,":") of
		0 ->
			{host,Host,DefPort};
		Idx ->
			{Port,_} = string:to_integer(string:sub_string(Host,Idx+1)),
			{host,string:sub_string(Host,1,Idx-1),Port}
	end.

build_header_list(Headers) ->
	build_header_list(Headers,[]).

build_header_list([],Acc) ->
	[lists:reverse(Acc),"\r\n"];
build_header_list([{Hdr,Val}|R],Acc) when is_atom(Hdr) ->
	build_header_list([{atom_to_list(Hdr),Val}|R],Acc);
build_header_list([{Hdr,Val}|R],Acc) ->
	HdrLine = [Hdr,": ",Val,"\r\n"],
	build_header_list(R,[HdrLine|Acc]);
build_header_list([Hdr|R],Acc) ->
	?WARN_MSG("Bad header: ~p~n",Hdr),
	build_header_list(R,Acc).

combine_headers(Headers) ->
	combine_headers(lists:reverse(Headers),[]).
combine_headers([],Acc) ->
	lists:flatten([Acc|"\r\n"]);
combine_headers([H|R],Acc) ->
	combine_headers(R,[[H|"\r\n"]|Acc]).

split_headers(Headers) ->
	split_headers(Headers,[],[]).
split_headers([],Req,Acc) ->
	{ok,Req,lists:reverse(Acc)};
split_headers(Header,[],Acc) ->
	case string:str(Header,"\r\n") of
		0 ->
			{error,no_request};
		Idx ->
			Req = string:sub_string(Header,1,Idx-1),
			Rest = string:sub_string(Header,Idx+2),
			split_headers(Rest,Req,Acc)
	end;
split_headers(Header,Req,Acc) ->
	case string:str(Header,"\r\n") of
		0 ->
			split_headers([],Req,[Header|Acc]);
		Idx ->
			Hdr = string:sub_string(Header,1,Idx-1),
			Rest = string:sub_string(Header,Idx+2),
			split_headers(Rest,Req,[Hdr|Acc])
	end.

parse_request(Req) ->
	Proxy = "(\\w*)\\s*(([[:graph:]])*)\\s*(.*)",
%% 	HTTPProxy = "(\\w*)\\s*([[:alpha:]]+\:\\/\\/[[:alpha:]\\.]+)(\\/([[:graph:]])*)\\s*(.*)",
	case re:run(Req,Proxy,[{capture,all,list}]) of
		{match,[_,Method,FullPath,_,Proto]} ->
%%  			io:format("Method: ~p~nFullPath: ~p~nProto: ~p~n~n",[Method,FullPath,Proto]),
			HTTPPortProxy = "([[:alpha:]]*:\\/\\/[[:alnum:]-\\.]+):([[:digit:]]+)(\\/.*)",
			case re:run(FullPath,HTTPPortProxy,[{capture,all,list}]) of
				{match,[_,Host,PortStr,Path]} ->
%% 					?DEBUG_MSG("Method: ~p~nHost: ~p~nPort: ~p~nPath: ~p~nProto: ~p~n~n",[Method,Host,PortStr,Path,Proto]),
					{Port,_} = string:to_integer(PortStr),
					#request_rec{method=Method,host=Host,path=Path,port=Port,protocol=Proto,proxytype=http_proxy};
				nomatch ->
					HTTPProxy = "([[:alpha:]]*:\\/\\/[[:alnum:]-\\.]+)(.*)",
					case re:run(FullPath,HTTPProxy,[{capture,all,list}]) of
						{match,[_,Host,Path]} ->
							#request_rec{method=Method,host=Host,path=Path,port=80,protocol=Proto,proxytype=http_proxy};
						nomatch ->
		%% 					io:format("Method: ~p~nPath: ~p~nProto: ~p~n~n",[Method,FullPath,Proto]),
							#request_rec{method=Method,path=FullPath,protocol=Proto,port=80,proxytype=transparent_proxy}
					end
			end;
%% 			{http_proxy,Method,Host,Path,Proto};
		nomatch ->
			{error,badmatch}
	end.

parse_response(Res) ->
	Pat = "([[:alnum:]\\.\\/]+)\\s*([[:digit:]]+)\\s*(.*)",
	case re:run(Res,Pat,[{capture,all,list}]) of
		{match,[_,Proto,CodeT,Text]} ->
			{Code,_} = string:to_integer(CodeT),
			#response_rec{protocol=Proto,code=Code,text=Text};
		Err ->
			?WARN_MSG("parse_response() Invalid response format: ~p~n~p~n",[Res,Err])
	end.

method_has_data(Req,Res) ->
	case Req#header_block.request of
		#request_rec{method='HEAD'} ->
			false;
		_ ->
%% 			?DEBUG_MSG("method_has_data(): Res=~p~n",[Res]),
			case Res#header_block.response of
				#response_rec{code=204} ->
					false;
				#response_rec{code=304} ->
					false;
				#response_rec{code=Code} when (Code < 200) and (Code > 99) ->
					false;
				_ ->
					true
			end
	end.

format_inet({ip,IP}) ->
	format_inet(IP);
format_inet({_,_,_,_}=IP) ->
	lists:flatten(io_lib:format("~p.~p.~p.~p",erlang:tuple_to_list(IP)));
format_inet({_,_,_,_,_,_,_,_}=IP) ->
	collapsev6(lists:flatten(io_lib:format("~.16B:~.16B:~.16B:~.16B:~.16B:~.16B:~.16B:~.16B",erlang:tuple_to_list(IP))),[]).

collapsev6([],Acc) ->
	lists:reverse(Acc);
collapsev6([$0,$:|R],[]) ->  % detect 0 in first group
	collapsev62([$:|R],":");
collapsev6([$:,$0,$:|R],Acc) ->
	collapsev62([$:|R],":"++Acc);
collapsev6([C|R],Acc) ->
	collapsev6(R,[C|Acc]).
collapsev62([$:,$0,$:|R],Acc) ->
	collapsev62([$:|R],Acc);
collapsev62([$:,$0],Acc) -> % detect 0 in last group
	collapsev6([],[$:|Acc]);
collapsev62(R,Acc) ->
	collapsev6([],lists:reverse(R)++Acc).

inet_version({ip,IP}) ->
	inet_version(IP);
inet_version({_,_,_,_}) ->
	inet;
inet_version({_,_,_,_,_,_,_,_}) ->
	inet6.

inet_parse({ip,IP}) ->
	inet_parse(IP);
inet_parse({_,_,_,_,_,_,_,_}=IP) ->
	IP;
inet_parse({_,_,_,_} = IP) ->
	IP;
inet_parse(IPStr) when is_list(IPStr) ->
	case inet_parse:address(IPStr) of
		{ok,IP} ->
			IP;
		_ ->
			?CRITICAL("Invalid IP format: ~p~n",[IPStr]),
			throw(invalid_ip)
	end.

inet_getaddr(Host) ->
	inet_getaddr(Host,inet).

inet_getaddr(Host,inet) ->
	case inet:getaddr(Host,inet) of
		{ok,IP} -> {ip,IP};
		{error,_} -> Host
	end;
inet_getaddr(Host,inet6) ->
	case inet:getaddr(Host,inet6) of
		{ok,IP} ->
			{ip,IP};
		{error,_Reason} ->
%% 			?DEBUG_MSG("No IPv6: ~p ~p~n",[Host,Reason]),
			inet_getaddr(Host,inet)
	end.

find_binary_pattern(Subject,Pat) ->
	find_binary_pattern(Subject,Pat,0).

find_binary_pattern(Subject,Pat,_) when bit_size(Subject) < bit_size(Pat) ->
	nomatch;
find_binary_pattern(Subject,Pat,Pos) ->
	PatSize = trunc(bit_size(Pat)/8),
%% 	io:format("Subject: ~p~nPat: ~p~nPatsize: ~p~n",[Subject,Pat,PatSize]),
	<<SubTest:PatSize/binary,_/binary>> = Subject,
	if
		SubTest == Pat ->
			Pos;
		true ->
			<<_:1/binary,NewSub/binary>> = Subject,
			find_binary_pattern(NewSub,Pat,Pos+1)
	end.

timestamp() ->
	httpd_util:rfc1123_date(calendar:now_to_local_time(now())).


get_pool_process(PoolName) ->
	list_to_atom("balancer_pool_"++atom_to_list(PoolName)).

uri_unescape(Str) ->
	uri_unescape2(Str,[]).

uri_unescape2([],Acc) ->
	lists:reverse(Acc);
uri_unescape2([$% ,H1,H2|R],Acc) ->
	case catch (list_to_integer([H1])*16+list_to_integer([H2])) of  %% list_to_integer(List,Size) not supported in older versions
		C when is_integer(C) ->
			uri_unescape2(R,[C|Acc]);
		_ ->
			uri_unescape2(R,[H2,H1,$%|Acc])
	end;
uri_unescape2([C|R],Acc) ->
	uri_unescape2(R,[C|Acc]).

string_to_term(TermStr) ->
	case erl_scan:string(TermStr) of
		{ok,Tokens,_} ->
			case erl_parse:parse_term(Tokens) of
				{ok,Term} ->
					Term;
				Error ->
					erlang:throw({parse_error,Error})
			end;
		ScanError ->
			erlang:throw({scan_parse_error,ScanError})
	end.
