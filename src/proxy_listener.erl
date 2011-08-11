%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 18, 2010
%%% -------------------------------------------------------------------
-module(proxy_listener).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([listen_master/2,accept_plain/2,accept_ssl/2,start_link/1]).

%% ,http_balance/2
%% gen_fsm callbacks
-export([init/1,  handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% -record(state, {}).

-record(socket_state,{type,listener,num_listeners,listeners,listen_port,proplist}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link(Args) ->
	gen_fsm:start_link(?MODULE,Args,[{debug,[log]}]).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init({listen_plain,{ip,IP0},Port,Props}=L) ->
	erlang:send_after(20000,self(),child_check_timer),
	?INFO_MSG("~p PLAIN listening: ~p~n",[?MODULE,L]),
	ProtocolType = proplists:get_value(protocol,Props,undefined_listener_protocol),
	ListenName = lists:flatten(io_lib:format("plain_~p_~p_~p",[proxylib:inet_parse(IP0),Port,ProtocolType])), 
 	erlang:register(list_to_atom(ListenName),self()),
	Listeners = proplists:get_value(num_listeners,Props,1),
	Bind = {ip,proxylib:inet_parse(IP0)},
	Opts =
		case proxylib:inet_version(Bind) of
			inet ->
				[Bind,binary,{active,false},inet,{reuseaddr,true}];
			inet6 ->
				[Bind,binary,{active,false},inet6,{reuseaddr,true}]
		end,
	?INFO_MSG("~p PLAIN listening: ~p~ngen_tcp:listen(~p,~p)~n",[?MODULE,L,Port,Opts]),
	case gen_tcp:listen(Port,Opts) of
		{ok,Listen} ->
			gen_fsm:send_event(self(),check_listeners),
			{ok, listen_master, #socket_state{type=plain,listener=Listen,num_listeners=Listeners,listeners=[],listen_port=Port,proplist=Props}};
		Err ->
			error_logger:error_msg("Error on listen() ~p~nSleeping before retry.~n",[Err]),
			timer:sleep(5000),
			case gen_tcp:listen(Port,Opts++[{reuseaddr,true}]) of
				{ok,Listen} ->
					gen_fsm:send_event(self(),check_listeners),
					{ok, listen_master, #socket_state{type=plain,listener=Listen,num_listeners=Listeners,listeners=[],listen_port=Port,proplist=Props}};
				Err2 ->
					?ERROR_MSG("~p could not start with args ~p~nError: ~p~n",[?MODULE,L,Err2]),
					{stop,error}
			end
	end;
init({plain,Listen,Port,Props,Parent}=_L) ->
	{ok,accept_plain,#proxy_listener{type=plain,client_sock=Listen,listen_port=Port,proplist=Props,parent_pid=Parent}};

init({listen_ssl,{ip,IP0},Port,Props}=L)->
	erlang:send_after(20000,self(),child_check_timer),
	ProtocolType = proplists:get_value(protocol,Props,undefined_listener_protocol),
	ListenName = lists:flatten(io_lib:format("ssl_~p_~p_~p",[proxylib:inet_parse(IP0),Port,ProtocolType])), 
 	erlang:register(list_to_atom(ListenName),self()),
	Listeners = proplists:get_value(num_listeners,Props,1),
	ssl:start(),
	?INFO_MSG("~p SSL listening: ~p~n",[?MODULE,L]),
 	KeyFile = proplists:get_value(keyfile,Props),
 	CertFile = proplists:get_value(certfile,Props),
	Bind = {ip,proxylib:inet_parse(IP0)},
	Opts = 
		case proxylib:inet_version(Bind) of
			inet -> [{certfile,CertFile},{keyfile,KeyFile},Bind,binary,{active,false},{depth,2}];
			inet6 -> [inet6,{certfile,CertFile},{keyfile,KeyFile},Bind,binary,{active,false},{ssl_imp,new},{depth,2}] %% {ssl_imp, new} set for R13B04
		end,
	case catch ssl:listen(Port,Opts) of
		{ok,Listen} ->
			gen_fsm:send_event(self(),check_listeners),
			{ok,listen_master,#socket_state{type=ssl,listener=Listen,num_listeners=Listeners,listeners=[],listen_port=Port,proplist=Props}};
		Err ->
 			error_logger:error_msg("Error on ssl listen() ~p~nSleeping before retry.~n",[Err]),
			timer:sleep(5000),
			case catch gen_tcp:listen(Port,Opts++[{reuseaddr,true}]) of
				{ok,Listen} ->
					gen_fsm:send_event(self(),check_listeners),
					{ok,listen_master,#socket_state{type=ssl,listener=Listen,num_listeners=Listeners,listeners=[],listen_port=Port,proplist=Props}};
				Err2 ->
					?ERROR_MSG("~p could not start ssl listener with args ~p~nOptions: ~p~nError: ~p~n",[?MODULE,L,Opts,Err2]),
					{stop,Err2}
			end
	end;
init({ssl,_Listen,Port,Props,Parent}=_L) ->
	{ok,accept_ssl,#proxy_listener{type=ssl,listen_port=Port,proplist=Props,parent_pid=Parent}}.


%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
listen_master(check_listeners,State)->
	Listeners = lists:filter(fun(P) -> is_pid(P) and is_process_alive(P) end,State#socket_state.listeners),
	case length(Listeners) of
		Num when Num < State#socket_state.num_listeners ->
			gen_fsm:send_event(self(),startchild);
		_ ->
			true
	end,
	{next_state,listen_master,State#socket_state{listeners=Listeners}};
listen_master(startchild,State) ->
%% 	io:format("~p starting child...~n",[?MODULE]),
	gen_fsm:send_event(self(),check_listeners),
	case gen_fsm:start_link(?MODULE,{State#socket_state.type,State#socket_state.listener,State#socket_state.listen_port,State#socket_state.proplist,self()},[]) of
		{ok,Pid} ->
			erlang:monitor(process,Pid),
%% 			erlang:register(list_to_atom(lists:append(atom_to_list(State#socket_state.type),pid_to_list(Pid))),Pid),
			gen_fsm:send_event(Pid,{wait,State#socket_state.listener}),
%% 			?FQDEBUG("Started child ~p~n",[Pid]),
			{next_state,listen_master,State#socket_state{listeners = [Pid|State#socket_state.listeners]}};
		Err ->
			?ERROR_MSG("Error starting child: ~p~n",[Err]),
			{next_state,listen_master,State}
	end;
listen_master({child_accepted,Pid},State) ->
	gen_fsm:send_event(self(),check_listeners),
	{next_state,listen_master,State#socket_state{listeners = lists:delete(Pid,State#socket_state.listeners)}};
listen_master(child_check_timer,State) ->
	gen_fsm:send_event(self(),check_listeners),
	erlang:send_after(2000,self(),child_check_timer),
	{next_state,listen_master,State}.


accept_plain({wait,_ListenSock},State) ->
	case gen_tcp:accept(State#proxy_listener.client_sock) of
		{ok,Sock0} ->
			{ok,Sock} = gen_socket:create(Sock0,gen_tcp),
			gen_fsm:send_event(State#proxy_listener.parent_pid,{child_accepted,self()}),
			gen_fsm:send_event(self(),get_headers),
			proxy_protocol:handle_protocol(State#proxy_listener{client_sock=Sock}),
			{stop,normal,State};
		{error,timeout} ->
			?INFO_MSG("~p: Accept timeout, retrying.~n",[?MODULE]),
			gen_fsm:send_event(self(),wait),
			{next_state,accept,State};
		Err ->
			?ERROR_MSG("~p: Accept error: ~p~n",[?MODULE,Err]),
			gen_server:cast({State#proxy_listener.parent_pid,self()}),
			{stop,normal,State}
	end.

accept_ssl({wait,ListenSock},State) ->
%% 	io:format("~p accept_ssl(),~p",[?MODULE,State]),
	case ssl:transport_accept(ListenSock) of
		{ok,SSLSock} ->
%% 			?ERROR_MSG("ssl transport accepted . ~p~n",[SSLSock]),
			case catch ssl:ssl_accept(SSLSock) of
				ok ->
%% 					?ERROR_MSG("ssl_accept completed.~n",[]),
					{ok,Sock} = gen_socket:create(SSLSock,ssl),
					gen_fsm:send_event(State#proxy_listener.parent_pid,{child_accepted,self()}),
					gen_fsm:send_event(self(),get_headers),
					proxy_protocol:handle_protocol(State#proxy_listener{client_sock=Sock}),
					{stop,normal,State};
				{'EXIT',Err} ->
					?ERROR_MSG("Error in ssl_accept:~n~p~n",[Err]),
					gen_fsm:send_event(State#proxy_listener.parent_pid,{child_accepted,self()}),
					{stop,normal,State};
				SSLErr ->
					?ERROR_MSG("SSL Error: ~p~n",[SSLErr]),
					gen_fsm:send_event(State#proxy_listener.parent_pid,{child_accepted,self()}),
					{stop,normal,State}
			end;
		{error,timeout} ->
			?INFO_MSG("~p: Accept timeout, retrying.~n",[?MODULE]),
			gen_fsm:send_event(self(),wait),
			{next_state,accept,State};
		Err ->
			?ERROR_MSG("~p: Accept error: ~p~n",[?MODULE,Err]),
			gen_fsm:send_event(State#proxy_listener.parent_pid,{child_accepted,self()}),
			{stop,normal,State}
	end.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

