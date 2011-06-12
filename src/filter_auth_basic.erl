
-module(filter_auth_basic).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([process_hook/4, start_instance/0]).

-behaviour(filter_stream).


%%
%% API Functions
%%

start_instance() ->
	{?MODULE,?MODULE}.

process_hook(_,request,{request_header,ReqHdr,_RequestSize}=Req,_PPC) ->
	AuthRequestResponse = {request_filter_response,<<"HTTP/1.1 407 Auth required\r\nProxy-Authenticate: Basic realm=\"Surrogate\"\r\nConnection: close\r\n\r\n">>},
	Dict = proxylib:header2dict(ReqHdr#header_block.headers),
	case dict:find("proxy-authorization",Dict) of
		{ok,"Basic "++AuthStr} ->
			Auth2 = binary_to_list(base64:decode(AuthStr)),
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
process_hook(_,_,Data,_PPC) ->
	Data.



%%
%% Local Functions
%%

