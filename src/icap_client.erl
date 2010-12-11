%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 11, 2010
%%% -------------------------------------------------------------------
-module(icap_client).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1,stop/1]).

-export([request_mod/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {url,host,sock}).

%% ====================================================================
%% External functions
%% ====================================================================

start(IcapUrl) ->
	IcapReg = "([[:alpha:]]*):\\/\\/([[:alnum:]-\\.\\:]+)(/.*)",
	case re:run(IcapUrl,IcapReg,[{capture,all,list}]) of
		{match,[_,_Proto,Host,_Path]} ->
			State = #state{url=IcapUrl,host=Host},
			case gen_server:start(?MODULE,[State],[]) of
				{ok,Pid} ->
					{ok,{?MODULE,Pid}};
				Err ->
					Err
			end;
		MErr ->
			?ERROR_MSG("~p failed to start.  Bad icap URL: ~p~n~p~n",[?MODULE,IcapUrl,MErr]),
			{error,badurl}
	end.
					
stop({?MODULE,Pid}) ->
	gen_server:cast(Pid,shutdown).

%% ====================================================================
%% Server functions
%% ====================================================================

request_mod({?MODULE,Icap},Header,Body0) ->
	HTTPReq = icaplib:http_request_headers(Header#header_block.request, Header#header_block.headers),
	Body = icaplib:body_chunks(Body0, <<>>),
%% 	<<"ok">>.
	gen_server:call(Icap,{reqmod,HTTPReq,Body}).
	

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([State]) ->
	case string:chr(State#state.host,$:) of
		Idx when is_integer(Idx) ->
			Host = string:substr(State#state.host,1,Idx-1),
			{Port,_} = string:to_integer(string:substr(State#state.host,Idx+1)),
			?DEBUG_MSG("Host: ~p~nPort: ~p~n",[Host,Port]),
			case gen_tcp:connect(Host,Port,[binary,inet,{active,false}],20000) of
				{ok,Sock0} ->
					{ok,Sock} = gen_socket:create(Sock0,gen_tcp),
					{ok, State#state{sock=Sock}};
				TcpErr ->
					?ERROR_MSG("Could not connect to icap server: ~p ~p~n",[State#state.host,TcpErr]),
					{stop,normal}
			end;
		Err ->
			?ERROR_MSG("~p failed to find correct host address: ~p~n",[?MODULE,Err]),
			{stop,normal}
	end.
	
    

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
handle_call({reqmod,Req,Body},_From,State) ->
	ReqSize = trunc(bit_size(Req)/8),
	EncBody =
		case Body of
			<<>> ->
				lists:flatten(io_lib:format("null-body=~p",[ReqSize]));
			_ ->
				lists:flatten(io_lib:format("req-body=~p",[ReqSize]))
		end,
	IRHdr = io_lib:format("REQMOD ~s ICAP/1.0\r\nHost: ~s\r\nDate: ~s\r\nEncapsulated: req-hdr=0, ~s\r\n\r\n",
				  		  [State#state.url,State#state.host,proxylib:timestamp(),EncBody]),
	IcapReq0 = list_to_binary(lists:flatten(IRHdr)),
	IcapReq = <<IcapReq0/binary,Req/binary,Body/binary>>,
	gen_socket:send(State#state.sock,IcapReq),
	Res = icaplib:read_response(State#state.sock),
	{reply,Res,State};
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
handle_cast(shutdown,State) ->
	?DEBUG_MSG("Stopping.~n",[]),
	{stop,normal,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
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

