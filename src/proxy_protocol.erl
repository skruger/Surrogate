%% Author: skruger
%% Created: Jul 11, 2011
%% Description: TODO: Add description to proxy_protocol
-module(proxy_protocol).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([get_module/1,behaviour_info/1,get_proxy_target/1]).

%%
%% API Functions
%%

behaviour_info(callbacks) ->
	[{handle_protocol,1}];
behaviour_info(_) ->
	undefined.

get_module(Name) ->
	list_to_atom("proxy_protocol_"++atom_to_list(Name)).

get_proxy_target(State) ->
	Port = proplists:get_value(backend_port,State#proxy_listener.proplist,State#proxy_listener.listen_port),
	case proplists:get_value(proxy_host,State#proxy_listener.proplist,undefined) of
		undefined ->
			case proplists:get_value(pool,State#proxy_listener.proplist,undefined) of
				undefined ->
					none;
				Pool ->
					Retries = proplists:get_value(pool_retries,State#proxy_listener.proplist,3),
					{pool,Pool,Port,Retries}
			end;
		{_,_,_,_} = Addr ->
			{host,Addr,Port};
		{Addr,BPort} ->
			{host,Addr,BPort};
		_ ->
			none
	end.

%%
%% Local Functions
%%

