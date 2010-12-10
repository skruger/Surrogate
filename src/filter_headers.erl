
-module(filter_headers).

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

-record(state,{proxy_pass,peer_addr,peer_port,config,add_headers,remove_headers}).
%%
%% API Functions


%%

start_instance() ->
	{ok,Pid} = gen_server:start_link(?MODULE,[self()],[]),
	{?MODULE,Pid}.

process_hook(Pid,info,{proxy_pass_config,_}=R) ->
	gen_server:call(Pid,R),
	R;
process_hook(Pid,_,{request_peer,_} = Peer) ->
%% 	?DEBUG_MSG("Got peer: ~p~n",[Peer]),
	gen_server:call(Pid,Peer),
	Peer;
process_hook(Pid,request,{request_header,ReqHdr,RequestSize}) ->
%% 	{peerinfo,Addr,_,_} = gen_server:call(Pid,get_peer),
%% 	?DEBUG_MSG("Addr: ~p~n",[Addr]),
%% 	HBlock = proxylib:append_header("X-Forwarded-For: "++Addr,ReqHdr#header_block.headers),
	HBlock = gen_server:call(Pid,{headermod,ReqHdr#header_block.headers}),
	{request_header,ReqHdr#header_block{headers=HBlock},RequestSize};
process_hook(_Ref,_Type,Data) ->
	Data.


init([ProxyPassPid]) ->
	%% monitor is critical for cleaning up later.  Without monitor and handle_info() exiting later this
	%% process could be orphaned forever.
	erlang:monitor(process,ProxyPassPid),
	{ok,#state{proxy_pass=ProxyPassPid}}.

handle_call({proxy_pass_config,Cfg},_From,State) ->
	RawProps = proplists:get_value(?MODULE,Cfg,[]),
	{Props,AddHdrs,RemHdrs} = parse_props(RawProps,[],[],[]),
	{reply,{proxy_pass_config,Cfg},State#state{config=Props,add_headers=AddHdrs,remove_headers=RemHdrs}};
handle_call({headermod,HBlock0},_From,State) ->
	HBlock1 =
		case proplists:get_value(xforwardfor,State#state.config,false) of
			false -> HBlock0;
			_ ->
				Addr = format_ip(State#state.peer_addr),
				proxylib:append_header("X-Forwarded-For: "++Addr,HBlock0)
		  end,
	HBlock2 = proxylib:remove_headers(State#state.remove_headers,HBlock1),
	HBlock3 = proxylib:append_headers(State#state.add_headers,HBlock2),
	{reply,HBlock3,State};
handle_call({request_peer,{Addr,Port}}=Peer,_From,State) ->
%% 	?DEBUG_MSG("Storing peer: ~p~n",[Peer]),
	{reply,Peer,State#state{peer_addr=Addr,peer_port=Port}};
handle_call(get_peer,_From,State) ->
	PInfo = {peerinfo,format_ip(State#state.peer_addr),State#state.peer_addr,State#state.peer_port},
	{reply,PInfo,State};
handle_call(_Msg,_From,State) ->
	{reply,ok,State}.


format_ip({A,B,C,D}) ->
	lists:flatten(io_lib:format("~p.~p.~p.~p",[A,B,C,D]));
format_ip(Info) ->
	?WARN_MSG("~p Bad IP format: ~p~nReturning \"0.0.0.0\"",[self(),Info]),
	"0.0.0.0".
	


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

parse_props([],PropAcc,AddAcc,RemAcc) ->
	{PropAcc,AddAcc,RemAcc};
parse_props([{add,Hdr}|R],PropAcc,AddAcc,RemAcc) ->
	parse_props(R,PropAcc,[Hdr|AddAcc],RemAcc);
parse_props([{remove,Hdr0}|R],PropAcc,AddAcc,RemAcc) ->
	Hdr = string:to_lower(Hdr0),
	parse_props(R,PropAcc,AddAcc,[Hdr|RemAcc]);
parse_props([V|R],PropAcc,AddAcc,RemAcc) ->
	parse_props(R,[V|PropAcc],AddAcc,RemAcc).
