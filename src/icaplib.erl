
-module(icaplib).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([http_request_headers/2,body_chunks/2,encapsulated_sections/1,read_response/1]).

%%
%% API Functions
%%

http_request_headers(ReqRec,Headers) ->
	HttpReq = lists:flatten(io_lib:format("~s ~s ~s\r\n~s",
										  [ReqRec#request_rec.method,ReqRec#request_rec.path,ReqRec#request_rec.protocol,
										   proxylib:combine_headers(Headers)])),
	list_to_binary(HttpReq).

body_chunks(<<>>,<<>>) -> <<>>;
body_chunks(Data,Acc) ->
	Chunksize = 1024,
	case trunc(bit_size(Data)/8) of
		0 ->
			<<Acc/binary,"0\r\n\r\n">>;
		Int when Int > Chunksize ->
			<<Chunk:Chunksize/binary,R/binary>> = Data,
			CLen = list_to_binary(erlang:integer_to_list(Chunksize,16)),
			body_chunks(R,<<Acc/binary,CLen/binary,"\r\n",Chunk/binary,"\r\n">>);
		Int ->
			CLen = list_to_binary(erlang:integer_to_list(Int,16)),
			body_chunks(<<>>,<<Acc/binary,CLen/binary,"\r\n",Data/binary,"\r\n">>)
	end.

read_response(Sock) ->
	Hdr = header_parse:get_headers(Sock,response),
	Dict = proxylib:header2dict(Hdr#header_block.headers),
	case dict:find("encapsulated",Dict) of
		{ok,Encline} ->
			EncSec = encapsulated_sections(Encline),
			SecList = get_sections(EncSec,Sock,Hdr#header_block.body,[]),
			Data = decode_chunks(SecList#icap_block.buff,Sock,<<>>),
			SecList#icap_block{header=Hdr,bodytype=get_body_type(EncSec,undefined),data=Data,buff = <<>>};
		Err ->
			?ERROR_MSG("No encapsulated: header! ~p~n",[Err]),
			Err
	end.

decode_chunks(<<"0\r\n\r\n">>,_,Acc) ->
	Acc;
decode_chunks(Buff,Sock,Acc) ->
	case proxylib:find_binary_pattern(Buff, <<"\r\n">>) of
		nomatch ->
			<<>>;
		0 ->
			?DEBUG_MSG("Strip leading.~n",[]),
			<<"\r\n",R/binary>> = Buff,
			decode_chunks(R,Sock,Acc);
		Idx ->
			<<LenBin:Idx/binary,"\r\n",Chunk/binary>> = Buff,
			Len = erlang:list_to_integer(binary_to_list(LenBin),16),
			?DEBUG_MSG("Idx: ~p ~p ~p~n",[Idx,LenBin,Len]),
			case trunc(bit_size(Chunk)/8) of
				CSize when CSize >= Len ->
					<<CData:Len/binary,R/binary>> = Chunk,
					decode_chunks(R,Sock,<<Acc/binary,CData/binary>>);
				CSize ->
					?DEBUG_MSG("Trying to get more: ~p ~p",[CSize,Len]),
					{ok,NewData} = gen_socket:recv(Sock,0),
					decode_chunks(<<Buff/binary,NewData/binary>>,Sock,Acc)
			end
	end.
					

encapsulated_sections(EncLine) ->
	SecList = lists:map(fun(X) -> string:strip(X,both,$ ) end,string:tokens(EncLine,",")),
	encapsulated_sections(SecList,[]).
	
encapsulated_sections([],Prop) -> lists:reverse(Prop);
encapsulated_sections([Enc|R],Prop) ->
	EncPat = "([[:alpha:]-]+)=([[:digit:]]+)",
	case re:run(Enc,EncPat,[{capture,all,list}]) of
		{match,[_,Key,Val0]} ->
			{Val,_} = string:to_integer(Val0),
			encapsulated_sections(R,[{list_to_atom(Key),Val}|Prop]);
		_Other ->
			lists:reverse(Prop)
	end.

get_sections([{_,End}],_Sock,Buff,Acc) ->
	<<_:End/binary,Rest/binary>> = Buff,
	#icap_block{sections=Acc,buff=Rest};
get_sections([{Sec,Start}|[{_NextSec,End}|_]=R]=L,Sock,Buff,Acc) ->
	?DEBUG_MSG("Size: ~p~nStart: ~p~nEnd: ~p~n",[trunc(bit_size(Buff)/8),Start,End]),
	case trunc(bit_size(Buff)/8) of
		BLen when BLen >= End ->
			SecLen = End - Start,
			<<_:Start/binary,SecData:SecLen/binary,_/binary>> = Buff,
			get_sections(R,Sock,Buff,[{Sec,SecData}|Acc]);
		_ ->
			?DEBUG_MSG("Trying to get more.",[]),
			{ok,NewBuff} = gen_socket:recv(Sock,0),
			get_sections(L,Sock,<<Buff/binary,NewBuff/binary>>,Acc)
	end.
	
get_body_type([],Old) ->
	Old;
get_body_type([{T,_}|R],Old) ->
	Old1 = body_type(T,Old),
	get_body_type(R,Old1).

body_type('null-body',_R) -> nullbody;
body_type('res-body',_R) -> response;
body_type('req-body',_R) -> request;
body_type(_,R) -> R.

%%
%% Local Functions
%%

