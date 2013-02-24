%% Author: skruger
%% Created: Jul 20, 2011
%% Description: TODO: Add description to mod_auth
-module(filter_auth).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-behaviour(filter_stream).

-export([start_instance/0,process_hook/4]).

%%
%% API Functions
%%

start_instance() ->
	{?MODULE,?MODULE}.

process_hook(_,request,{request_header,ReqHdr,_RequestSize}=Req,#proxy_txn{config=PConf}=_PPC) ->
%% 	?ERROR_MSG("ProxyPass: ~p~n",[PConf]),
	Conf = proplists:get_value(?MODULE,PConf,[]),
	Dict = dict:from_list(ReqHdr#header_block.headers),
	{Mode,AuthHdr} = case proplists:get_value(mode,Conf,proxy) of
				  proxy -> {proxy,'Proxy-Authorization'};
				  _ -> {www,'Authorization'}
			  end,
	AuthRequestResponse = {request_filter_response,proxy_auth_request('basic',Mode,"Surrogate")},
	case dict:find(AuthHdr,Dict) of
		{ok,"Basic "++AuthStr} ->
			Auth2 = binary_to_list(base64:decode(AuthStr)),
%% 			?ERROR_MSG("Userinfo: ~p~n",[Auth2]),
			case string:chr(Auth2,$:) of
				0 ->
					AuthRequestResponse;
				Idx ->
					User = string:substr(Auth2,1,Idx-1),
					Pass = string:substr(Auth2,Idx+1),
					case proxy_auth:check_user(User,Pass) of
						{ok,UserInfo} ->
							self() ! {userinfo,UserInfo},
							Req;
						Err ->
							?ERROR_MSG("Authentication error for ~p: ~p (~p)~n",[User,Err,Pass]),
							AuthRequestResponse
					end
			end;
		_NoAuth ->
			AuthRequestResponse
	end;
process_hook(_Pid,_Mode,Data,_PPC) ->
	Data.


%%
%% Local Functions
%%

proxy_auth_request('basic',proxy,Realm) ->
	Out = io_lib:format("HTTP/1.1 407 Auth required\r\nProxy-Authenticate: Basic realm=~p\r\nConnection: close\r\n\r\n",[Realm]),
	iolist_to_binary(Out);
proxy_auth_request('basic',www,Realm) ->
	Out = io_lib:format("HTTP/1.1 401 Auth required\r\nWWW-Authenticate: Basic realm=~p\r\nConnection: close\r\n\r\n",[Realm]),
	iolist_to_binary(Out);
proxy_auth_request(_,_,_Realm) ->
	none.

