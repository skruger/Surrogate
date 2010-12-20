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

-export([get_headers/2,read_header_block/3]).

%% ====================================================================
%% External functions
%% ====================================================================

get_headers(Sock,Type) ->
	read_header_block(<<>>,Sock,Type).

read_header_block(<<HdrData/binary>>,Sock,Type) ->
	case proxylib:find_binary_pattern(<<HdrData/binary>>,<<"\r\n\r\n">>) of
		nomatch when Type == request ->
			Timeout = if bit_size(HdrData) > 0 ->
							 3000;
						 true -> infinity
					  end,
			case gen_socket:recv(Sock,0,Timeout) of
				{ok,<<Dat/binary>>} ->
					read_header_block(<<HdrData/binary,Dat/binary>>,Sock,Type);
				{error,closed} = Err ->
					throw(Err);
				Err ->
					?ERROR_MSG("Error receiving request headers: ~p~n",[Err]),
					throw(Err)
			end;
		nomatch ->
			case gen_socket:recv(Sock,0) of
				{ok,<<Dat/binary>>} ->
					read_header_block(<<HdrData/binary,Dat/binary>>,Sock,Type);
				Err ->
					
					throw(Err)
			end;
		Idx ->
			<<Hdr:Idx/binary,"\r\n\r\n",Body/binary>> = HdrData,
			case proxylib:split_headers(binary_to_list(Hdr)) of
				{ok,Request,HdrList} ->
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
					end;
				{error,SplitErr} ->
					?WARN_MSG("Error in split_headers: ~p~n~p~n",[SplitErr,binary_to_list(Hdr)]),
					throw(SplitErr)
			end
	end.
			
			

