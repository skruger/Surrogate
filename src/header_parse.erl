%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Oct 31, 2010
%%% -------------------------------------------------------------------
-module(header_parse).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------

-export([get_headers/2]).

%% ====================================================================
%% External functions
%% ====================================================================

get_headers(Sock,Type) ->
	read_header_block(<<>>,Sock,Type).

read_header_block(<<HdrData/binary>>,Sock,Type) ->
	case proxylib:find_binary_pattern(<<HdrData/binary>>,<<"\r\n\r\n">>) of
		nomatch ->
			case gen_socket:recv(Sock,0) of
				{ok,<<Dat/binary>>} ->
					read_header_block(<<HdrData/binary,Dat/binary>>,Sock,Type);
				Err ->
					
					throw(Err)
			end;
		Idx ->
			<<Hdr:Idx/binary,"\r\n\r\n",Body/binary>> = HdrData,
			{ok,Request,HdrList} = proxylib:split_headers(binary_to_list(Hdr)),
			case Type of
				request ->
					Req = proxylib:parse_request(Request),
					#header_block{headers=HdrList,body=Body,request=Req,rstr=Request};
				response ->
					Res = proxylib:parse_response(Request),
					#header_block{headers=HdrList,body=Body,response=Res,rstr=Request};
				BadType ->
					?ERROR_MSG("get_headers() failed with invalid type: ~p~n",[BadType]),
					throw(nomatch)
			end
	end.
			
			

