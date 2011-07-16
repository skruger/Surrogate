%% Author: skruger
%% Created: Jul 11, 2011
%% Description: TODO: Add description to proxy_protocol_http
-module(proxy_protocol_http).

%%
%% Include files
%%

-include("surrogate.hrl").

-behaviour(proxy_protocol).

%%
%% Exported Functions
%%
-export([handle_protocol/1]).

%%
%% API Functions
%%

handle_protocol(State) ->
	Sock = State#proxy_listener.client_sock,
	ProxyPass = #proxy_pass{proxy_type=transparent_proxy,config=State#proxy_listener.proplist},
	{ok,Pid} = proxy_pass:start(ProxyPass),
	gen_socket:controlling_process(Sock,Pid),
	Target = proxy_protocol:get_proxy_target(State),
%% 	?ERROR_MSG("Target: ~p~n",[Target]),
	proxy_pass:setproxyaddr(Pid,Target),
	gen_fsm:send_event(Pid,{socket,Sock}).

