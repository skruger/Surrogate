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
-export([decode_headers/1,read_decode_block/3]).

%% ====================================================================
%% External functions
%% ====================================================================

decode_headers(Sock) ->
	read_decode_block(<<>>,Sock,#header_block{headers=[],body= <<>>}).

read_decode_block(HdrData,Sock,#header_block{headers=HdrList}=Acc) ->
	{Mode,Timeout} = 
		if (Acc#header_block.request == undefined) and (Acc#header_block.response == undefined) ->
			   {http,infinity};
		   true -> {httph,2000}
		end,
	case catch erlang:decode_packet(Mode,HdrData,[]) of
		{ok,http_eoh,Rest} ->
%% 			?ERROR_MSG("4: ~p~n",[Acc]),
			URL = format_request(Acc),
			Acc#header_block{body=Rest,headers=lists:reverse(HdrList),rstr=URL};
		{ok,{http_request,Method,URI,Ver}=RawReq,Rest} ->
%% 			-record(request_rec,{proxytype,method,path,protocol,host,state,port}).
			Req = 
			case URI of
				{abs_path,Path0} ->
					#request_rec{proxytype=transparent_proxy,method=Method,path=Path0,protocol=Ver};
				{absoluteURI,Protocol,Host,Port0,Path0} ->
					Port =
						if is_integer(Port0) -> Port0; 
						   true -> 80 
						end,
					#request_rec{proxytype=Protocol,method=Method,path=Path0,protocol=Ver,host=Host,port=Port};
				{scheme,Host,PortStr} ->
					#request_rec{proxytype=connect,method=Method,protocol=Ver,host=Host,port=list_to_integer(PortStr),path=""};
				Other ->
					?ERROR_MSG("Got other ~p from raw request:~n~p~n",[Other,RawReq]),
					throw(other_error)
			end,
			read_decode_block(Rest,Sock,#header_block{rawhead=RawReq,request=Req});
		{ok,{http_response,Ver,Code,ResponseString}=RawRes,Rest} ->
			Res = #response_rec{protocol=Ver,code=Code,text=ResponseString},
			read_decode_block(Rest,Sock,#header_block{rawhead=RawRes,response=Res});
		{ok,{http_header,_Int,HdrName,_,HdrVal},Rest} ->
			Hdr = {HdrName,HdrVal},
			NewHdr = 
			case HdrList of
				undefined -> [Hdr];
				_ -> [Hdr|HdrList]
			end,
			read_decode_block(Rest,Sock,Acc#header_block{headers=NewHdr});
		_ ->
			case gen_socket:recv(Sock,0,Timeout) of
				{ok,More} ->
					read_decode_block(<<HdrData/binary,More/binary>>,Sock,Acc);
				{error,closed} = Err ->
					throw(Err);
				Err ->
					?ERROR_MSG("Error decoding headers: ~p~n",[Err]),
					throw(Err)
			end
	end.

format_request(#header_block{request=#request_rec{method=Method,path=Path,host=Host,port=Port}}) ->
	Req = io_lib:format("~s http://~s:~p~s",[Method,Host,Port,Path]),
	Bin = iolist_to_binary(Req),
	?ERROR_MSG("String: ~p~n",[Bin]),
	binary_to_list(Bin);
format_request(_) -> "Response request".

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
			
			

