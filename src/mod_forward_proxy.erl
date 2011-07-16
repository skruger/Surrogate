%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Jun 4, 2011
%%% -------------------------------------------------------------------
-module(mod_forward_proxy).

-behaviour(gen_server).
-behaviour(filter_stream).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-export([proxy_mod_start/1,proxy_mod_stop/1]).

-export([start_instance/0,process_hook/4]).

-record(state, {conf}).

%% ====================================================================
%% External functions
%% ====================================================================

proxy_mod_start(Conf) ->
	supervisor:start_child(surrogate_sup,{?MODULE,{?MODULE,start_link,[Conf]},permanent,1000,worker,[?MODULE]}),
	ok.

proxy_mod_stop(_Conf) ->
	try
		supervisor:terminate_child(surrogate_sup, ?MODULE),
		supervisor:delete_child(surrogate_sup, ?MODULE)
	catch _:_ -> ok	end,
	ok.

start_link(Conf) ->
	gen_server:start_link({local,?MODULE},?MODULE,Conf,[]).

%% ====================================================================
%% Server functions
%% ====================================================================

start_instance() ->
	{?MODULE,?MODULE}.

process_hook(_Pid,request,{request_header,Hdr,_Size}=HBlock,PPC) ->
	HDict = proxylib:header2dict(Hdr#header_block.headers),
	case dict:find("host",HDict) of
		{ok,HostStr} ->
			{host,_Host,_Port} = TargetHost = proxylib:parse_host(HostStr,80),
			TargetList = proxy_protocol:resolve_target_list(TargetHost,PPC#proxy_pass.config),
%% 			?ERROR_MSG("TargetList: ~p~n",[TargetList]),
			proxy_pass:setproxyaddr(PPC#proxy_pass.proxy_pass_pid,TargetList);
		_ -> ok
	end,
	HBlock;
process_hook(_Pid,_Mode,Data,_PPC) ->
	Data.


  
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Conf) ->
    {ok, #state{conf=Conf}}.

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

