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
	Port = proplists:get_value(backend_port,State#proxy_listener.proplist,State#proxy_listener.listen_port),
	case proplists:get_value(proxy_host,State#proxy_listener.proplist,undefined) of
		undefined ->
			case proplists:get_value(pool,State#proxy_listener.proplist,undefined) of
				undefined ->
					ok;
				Pool ->
					Retries = proplists:get_value(pool_retries,State#proxy_listener.proplist,3),
					proxy_pass:setproxypool(Pid, Pool, Port, Retries)
			end;
		{_,_,_,_} = Addr ->
			proxy_pass:setproxyaddr(Pid,Addr,Port);
		{Addr,BPort} ->
			proxy_pass:setproxyaddr(Pid,Addr,BPort);
		_ ->
			ok
	end,
	gen_fsm:send_event(Pid,{socket,Sock}).

