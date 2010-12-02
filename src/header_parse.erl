%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Oct 31, 2010
%%% -------------------------------------------------------------------
-module(header_parse).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,get_headers/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock,request,headers,recv_buff,requestor,sock_owner,sock_closed}).

%% ====================================================================
%% External functions
%% ====================================================================

%% receive_headers(Pid,Sock) ->
%% 	gen_socket:controlling_process(Sock,Pid),
%% 	gen_server:call(Pid,{receive_headers,Sock,self()},600000).

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
			
			

%% ====================================================================
%% Server functions
%% ====================================================================

start_link() ->
	gen_server:start_link(?MODULE,[],[]).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{headers= [] ,recv_buff= <<>>}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%% handle_call({receive_ssl_headers,Sock,SockOwner},From,State) when State#state.requestor == undefined ->
%% 	inet:setopts(Sock,[{active,once}]),
%% 	{noreply,State#state{sock=Sock,requestor=From,sock_owner=SockOwner,csock_mod=ssl}};
handle_call({receive_headers,Sock,SockOwner},From,State) when State#state.requestor == undefined ->
%% 	?DEBUG_MSG("receive_headers(~p),~p,~p~n",[self(),Sock,SockOwner]),
	gen_socket:setopts(Sock,[{active,once}]),
	{noreply,State#state{sock=Sock,requestor=From,sock_owner=SockOwner}};
handle_call({receive_headers,_Sock},_From,State) ->
	{reply,{error,busy},State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
%% handle_cast(parse_headers,State) ->
%% %% 	io:format("~p recv_buff: ~p~n",[?MODULE,State#state.recv_buff]),
%% 	case proxylib:find_binary_pattern(State#state.recv_buff,<<"\r\n\r\n">>) of
%% 		nomatch ->
%% 			gen_socket:setopts(State#state.sock,[{active,once}]),
%% 			{noreply,State};
%% 		Idx ->
%% 			<<Hdr:Idx/binary,"\r\n\r\n",Body/binary>> = State#state.recv_buff,
%% %% 			?DEBUG_MSG("Header: ~p~n",[Hdr]),
%% 			{ok,Request,HdrList} = proxylib:split_headers(binary_to_list(Hdr)),
%% 			gen_socket:controlling_process(State#state.sock,State#state.sock_owner),
%% 			Re = #proxy_pass{headers=HdrList,recv_buff=Body,request=Request,sock_closed=State#state.sock_closed},
%% 			gen_server:reply(State#state.requestor,Re),
%% 			gen_server:cast(self(),exit),
%% 			{noreply,State}
%% 	end;
handle_cast(exit,State) ->
	{stop,normal,State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({gen_socket,_Sock,<<Msg/binary>>},State) ->
	gen_server:cast(self(),parse_headers),
	<<RecvBuff/binary>> = State#state.recv_buff,
	{noreply, State#state{recv_buff = <<RecvBuff/binary,Msg/binary>>}};
handle_info({gen_socket_closed,_},State) ->
	{noreply, State#state{sock_closed=true}};
handle_info(Info, State) ->
	?INFO_MSG("Got unexpected info: ~p~n",[Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

