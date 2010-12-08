
-module(filter_xforwardfor).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([process_hook/3, start_instance/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(filter_stream).
-behaviour(gen_server).

-record(state,{proxy_pass,peer_addr,peer_port}).
%%
%% API Functions
%%

start_instance() ->
	{ok,Pid} = gen_server:start_link(?MODULE,[self()],[]),
	{?MODULE,Pid}.


process_hook(Pid,request,{request_peer,_} = Peer) ->
	gen_server:call(Pid,Peer);
process_hook(Pid,request,{request_header,ReqHdr,RequestSize}) ->
	{peerinfo,Addr,_,_} = gen_server:call(Pid,get_peer),
	HBlock = proxylib:append_header("X-Forwarded-For: "++Addr,ReqHdr#header_block.headers),
	{request_header,ReqHdr#header_block{headers=HBlock},RequestSize};
process_hook(_Ref,_Type,Data) ->
	Data.


init([ProxyPassPid]) ->
	%% monitor is critical for cleaning up later.  Without monitor and handle_info() exiting later this
	%% process could be orphaned forever.
	erlang:monitor(process,ProxyPassPid),
	{ok,#state{proxy_pass=ProxyPassPid}}.

handle_call({request_peer,{Addr,Port}}=Peer,_From,State) ->
%% 	?DEBUG_MSG("Storing peer: ~p~n",[Peer]),
	{reply,Peer,State#state{peer_addr=Addr,peer_port=Port}};
handle_call(get_peer,_From,State) ->
	PInfo = {peerinfo,format_ip(State#state.peer_addr),State#state.peer_addr,State#state.peer_port},
	{reply,PInfo,State};
handle_call(_Msg,_From,State) ->
	{reply,ok,State}.



format_ip({A,B,C,D}) ->
	lists:flatten(io_lib:format("~p.~p.~p.~p",[A,B,C,D])).


%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({'DOWN',_,process,_Pid,_},State) ->
	{stop,normal,State};
handle_info(Info, State) ->
	?DEBUG_MSG("Unknown info: ~p~n",[Info]),
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
