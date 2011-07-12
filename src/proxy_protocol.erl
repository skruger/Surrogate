%% Author: skruger
%% Created: Jul 11, 2011
%% Description: TODO: Add description to proxy_protocol
-module(proxy_protocol).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_module/1,behaviour_info/1]).

%%
%% API Functions
%%

behaviour_info(callbacks) ->
	[{handle_protocol,1}];
behaviour_info(_) ->
	undefined.

get_module(Name) ->
	list_to_atom("proxy_protocol_"++atom_to_list(Name)).


%%
%% Local Functions
%%

