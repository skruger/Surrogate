%% Author: skruger
%% Created: Dec 3, 2010
%% Description: TODO: Add description to filter_force_chunk
-module(filter_force_chunk).

%%
%% Include files
%%
-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([process_hook/3, start_instance/0]).

-behaviour(filter_stream).
%%
%% API Functions
%%

start_instance() -> {?MODULE,?MODULE}.

process_hook(_,response,{response_header,_,chunked}=Data) ->
	Data;
process_hook(_,response,{response_header,Hdr,_Length}) ->
	case (Hdr#header_block.response)#response_rec.protocol of
		"HTTP/1.1" ->
			NewHeaders = Hdr#header_block.headers ++ ["Transfer-Encoding: chunked"],
			{response_header,Hdr#header_block{headers=NewHeaders},chunked};
		"HTTP/1.0" ->
			NewHeaders = Hdr#header_block.headers ++ ["Connection: close"],
			{response_header,Hdr#header_block{headers=NewHeaders},close}
	end;
process_hook(_,_,Data) ->
	Data.

%%
%% Local Functions
%%

