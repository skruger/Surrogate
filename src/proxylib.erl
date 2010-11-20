%% file_comment
-module(proxylib).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([header2dict/1,parse_host/2,parse_request/1,parse_response/1,parse_connect/1,combine_headers/1,split_headers/1,find_binary_pattern/2,method_has_data/2]).

-export([send/2,setopts/2]).

-export([get_pool_process/1]).

%% -export([re/1]).
%%
%% API Functions
%%

header2dict(Hdr) ->
	header2dict(Hdr,[]).

header2dict([],List)->
	dict:from_list(List);
header2dict([Hdr|R],Acc)->
	case string:str(Hdr,":") of
		Idx ->
			Key = string:sub_string(Hdr,1,Idx-1),
			Val = string:sub_string(Hdr,Idx+2),
			header2dict(R,[{string:to_lower(Key),Val}|Acc])
	end.

parse_host(Host,DefPort) ->
	case string:str(Host,":") of
		0 ->
			{host,Host,DefPort};
		Idx ->
			{Port,_} = string:to_integer(string:sub_string(Host,Idx+1)),
			{host,string:sub_string(Host,1,Idx-1),Port}
	end.

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
	
parse_connect(Req) ->
	case parse_request(Req) of
		#request_rec{method="CONNECT",path=HostPort} = _ReqRec ->
			case re:run(HostPort,"(.*):(.*)",[{capture,all,list}]) of
				{match,[_,Host,PortStr]} ->
					{Port,_} = string:to_integer(PortStr),
					{ok,Host,Port}
			end;
		Err ->
			Err
	end.

parse_request(Req) ->
	Proxy = "(\\w*)\\s*(([[:graph:]])*)\\s*(.*)",
%% 	HTTPProxy = "(\\w*)\\s*([[:alpha:]]+\:\\/\\/[[:alpha:]\\.]+)(\\/([[:graph:]])*)\\s*(.*)",
	case re:run(Req,Proxy,[{capture,all,list}]) of
		{match,[_,Method,FullPath,_,Proto]} ->
%%  			io:format("Method: ~p~nFullPath: ~p~nProto: ~p~n~n",[Method,FullPath,Proto]),
			HTTPPortProxy = "([[:alpha:]]*:\\/\\/[[:alnum:]-\\.]+):([[:digit:]]+)(.*)",
			case re:run(FullPath,HTTPPortProxy,[{capture,all,list}]) of
				{match,[_,Host,PortStr,Path]} ->
%% 					io:format("Match: ~p~n",[Array]),
					io:format("Method: ~p~nHost: ~p~nPort: ~p~nPath: ~p~nProto: ~p~n~n",[Method,Host,PortStr,Path,Proto]),
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
			io:format("Invalid response format: ~p~n~p~n",[Res,Err])
	end.

method_has_data(Request,ResHdr) ->
	case parse_request(Request) of
		#request_rec{method="HEAD"} ->
			false;
		_ ->
			case parse_response(ResHdr#proxy_pass.request) of
				#response_rec{code=204} ->
					false;
				#response_rec{code=304} ->
					false;
				#response_rec{code=Code} when (Code < 200) and (Code > 99) ->
					flase;
				_ ->
					true
			end
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


get_pool_process(PoolName) ->
	{global,list_to_atom("balancer_pool_"++atom_to_list(PoolName))}.

			%% 	string:str(binary_to_list(Subject),binary_to_list(Pat)).

send({sslsocket,_,_} = Sock,Data) ->
	ssl:send(Sock,Data);
send(Sock,Data) ->
	gen_tcp:send(Sock,Data).

setopts({sslsocket,_,_}=Sock,Opts) ->
	ssl:setopts(Sock,Opts);
setopts(Sock,Opts) ->
	inets:setopts(Sock,Opts).

%% proxylib:find_binary_pattern(<<"this is a string with a pattern in it">>,<<"with">>).

%%
%% Local Functions
%%

%% re(Pat) ->
%% 	[re:run("http://l1.yimg.com/a/i/crsl/10q4/frog_flames_56x44.jpg",Pat,[{capture,all,list}]),
%% 	 re:run("/a/i/crsl/10q4/frog_flames_56x44.jpg",Pat,[{capture,all,list}])].
	 

%% proxylib:re("(\\w*)\\s*((\\w|:|\\/|\\.)*).*").
