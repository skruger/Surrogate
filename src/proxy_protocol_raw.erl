%% Author: skruger
%% Created: Jul 16, 2011
%% Description: TODO: Add description to proxy_protocol_raw
-module(proxy_protocol_raw).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%

-behaviour(proxy_protocol).

-export([handle_protocol/1]).

%%
%% API Functions
%%

handle_protocol(PListener) ->
	ClientSock = PListener#proxy_listener.client_sock,
	Target = proxy_protocol:get_proxy_target(PListener),
	TargetList = proxy_protocol:resolve_target_list(Target,PListener#proxy_listener.proplist),
	{ok,ServerSock} = proxy_protocol:tcp_connect(TargetList),
	
 	ServerPid = spawn(proxy_connect,server_loop,[ServerSock,undefined]),
	ClientPid = spawn(proxy_connect,client_loop,[ClientSock,undefined]),
	gen_socket:controlling_process(ServerSock,ServerPid),
	gen_socket:controlling_process(ClientSock,ClientPid),
	ServerPid ! {client,ClientPid},
	ClientPid ! {server,ServerPid},
	ok.

