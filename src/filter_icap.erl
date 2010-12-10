%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 9, 2010
%%% -------------------------------------------------------------------
-module(filter_icap).

-behaviour(gen_server).
-behaviour(filter_stream).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_instance/0,process_hook/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {proxy_pass,request_hdr,request_data}).

%% ====================================================================
%% External functions
%% ====================================================================

start_instance() ->
	ProxyPass = self(),
	{ok,Pid} = gen_server:start(?MODULE,[ProxyPass],[]),
	{?MODULE,Pid}.

process_hook(_,request,{request_header,#header_block{request=#request_rec{method="CONNECT"}},_}=Data) ->
%% 	?DEBUG_MSG("Do nothing with connect method.~n",[]),
	Data;
process_hook(Pid,request,{request_header,_Hdr,_Size}=HBlock) ->
	gen_server:call(Pid,{request_hdr,HBlock}),
	delay;
process_hook(Pid,request,{request_data,Data}) ->
	gen_server:call(Pid,{request_data,Data}),
	delay;
process_hook(Pid,request,{end_request_data,_Size}=End) ->
%% 	?DEBUG_MSG("process_hook(~p) ~p~n",[End,?MODULE]),
	gen_server:call(Pid,End),
	delay;
process_hook(_Pid,_Mode,Data) ->
%% 	?DEBUG_MSG("~p:process_hook(~p,~p)~n",[?MODULE,Mode,Data]),
	Data.

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([ProxyPassPid]) ->
	erlang:monitor(process,ProxyPassPid),
    {ok, #state{proxy_pass=ProxyPassPid,request_data = <<>>}}.

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
handle_call({request_hdr,Hdr},_From,State) ->
	State#state.proxy_pass ! get_request_data,
	{reply,ok,State#state{request_hdr=Hdr}};
handle_call({request_data,Data},_From,State) ->
	<<OldData/binary>> = State#state.request_data,
	State#state.proxy_pass ! get_request_data,
	{reply,ok,State#state{request_data= <<OldData/binary,Data/binary>>}};
handle_call({end_request_data,_Size},_From,State) ->
%% 	?DEBUG_MSG("end_request received.  Sending data.~n",[]),
	State#state.proxy_pass ! {filter_delay,State#state.request_hdr},
	State#state.proxy_pass ! {filter_delay,{request_data,State#state.request_data}},
	State#state.proxy_pass ! {filter_delay,{end_request_data,trunc(bit_size(State#state.request_data)/8)}},
	
	{request_header,ReqHdr,_} = State#state.request_hdr,
 	Req=ReqHdr#header_block.request,
 	ReqStr = lists:flatten(io_lib:format("~s ~s ~s",[Req#request_rec.method,Req#request_rec.path,Req#request_rec.protocol])),
 	RequestHeaders = lists:flatten([[ReqStr|"\r\n"]|proxylib:combine_headers(ReqHdr#header_block.headers)]),
	?DEBUG_MSG("Header block: ~n~p~n",[RequestHeaders]),
 			
	{reply,ok,State#state{request_hdr=undefined,request_data = <<>>}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
%% 	?DEBUG_MSG("Stopping ~p because proxy_pass went away.~n",[?MODULE]),
	{stop,normal,State};
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

