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

-include("filterproxy.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([receive_headers/2,start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock,request,headers,recv_buff,requestor,sock_owner}).

%% ====================================================================
%% External functions
%% ====================================================================

receive_headers(Pid,Sock) ->
	gen_tcp:controlling_process(Sock,Pid),
	gen_server:call(Pid,{receive_headers,Sock,self()},600000).

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
handle_call({receive_headers,Sock,SockOwner},From,State) when State#state.requestor == undefined ->
	inet:setopts(Sock,[{active,once}]),
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
handle_cast(parse_headers,State) ->
%% 	io:format("~p recv_buff: ~p~n",[?MODULE,State#state.recv_buff]),
	case proxylib:find_binary_pattern(State#state.recv_buff,<<"\r\n\r\n">>) of
		nomatch ->
			inet:setopts(State#state.sock,[{active,once}]),
			{noreply,State};
		Idx ->
			<<Hdr:Idx/binary,"\r\n\r\n",Body/binary>> = State#state.recv_buff,
			{ok,Request,HdrList} = proxylib:split_headers(binary_to_list(Hdr)),
			gen_tcp:controlling_process(State#state.sock,State#state.sock_owner),
			Re = #proxy_pass{headers=HdrList,recv_buff=Body,request=Request},
			gen_server:reply(State#state.requestor,Re),
			gen_server:cast(self(),exit),
			{noreply,State}
	end;
handle_cast(exit,State) ->
	{stop,normal,State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tcp,_Sock,<<Msg/binary>>},State) ->
	gen_server:cast(self(),parse_headers),
	<<RecvBuff/binary>> = State#state.recv_buff,
	{noreply, State#state{recv_buff = <<RecvBuff/binary,Msg/binary>>}};
handle_info(_Info, State) ->
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

