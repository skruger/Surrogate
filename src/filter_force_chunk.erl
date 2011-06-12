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
-export([process_hook/4, start_instance/0]).

-behaviour(filter_stream).
%%
%% API Functions
%%

start_instance() -> {?MODULE,?MODULE}.

process_hook(_,response,{response_header,_,chunked}=Data,_PPC) ->
	Data;
process_hook(_,response,{response_header,_,close}=Data,_PPC) ->
	Data;
process_hook(_,response,{response_header,_,0}=Header,_PPC) ->
	Header;
process_hook(_,response,{response_header,Hdr,_Length}=Header,_PPC) ->
	Dict = proxylib:header2dict(Hdr#header_block.headers),
	case dict:find("content-length",Dict) of
		{ok,_} ->
			case (Hdr#header_block.response)#response_rec.protocol of
				"HTTP/1.1" ->
					NewHeaders = proxylib:replace_header("content-length","Transfer-Encoding: chunked",Hdr#header_block.headers),
					{response_header,Hdr#header_block{headers=NewHeaders},chunked};
				"HTTP/1.0" ->
					NewHeaders0 = proxylib:remove_headers(["connection","content-length"],Hdr#header_block.headers),
					NewHeaders2 = proxylib:append_header("Connection: close",NewHeaders0),
					{response_header,Hdr#header_block{headers=NewHeaders2},close}
			end;
		_ -> 
			Header
	end;
process_hook(_,_Method,Data,_PPC) ->
	Data.

%%
%% Local Functions
%%

