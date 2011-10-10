%% Author: skruger
%% Created: Jul 11, 2011
%% Description: TODO: Add description to proxy_protocol
-module(proxy_protocol).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([get_module/1,behaviour_info/1,get_proxy_target/1,resolve_addr/2,tcp_connect/1,resolve_target_list/2]).
-export([handle_protocol/1,handle_protocol2/1]).

%%
%% API Functions
%%

behaviour_info(callbacks) ->
	[{handle_protocol,1}];
behaviour_info(_) ->
	undefined.

%% Get execution into separate process so that crashes don't propagate back to listener through process links.
handle_protocol(State) ->
	Pid = spawn(?MODULE,handle_protocol2,[State]),
	gen_socket:controlling_process(State#proxy_listener.client_sock,Pid),
	Pid ! run_handle_protocol, % Sync to avoid race.
	ok.

%% wait for 'run' before starting.  handle_protocol must set the gen_socket 
%% controlling_process before anything in the handle_protocol2 process executes.
handle_protocol2(State) ->
	receive run_handle_protocol -> ok end,
	Personality = proplists:get_value(protocol,State#proxy_listener.proplist),
	Mod = proxy_protocol:get_module(Personality),
	apply(Mod,handle_protocol,[State]).
	
get_module(Name) ->
	list_to_atom("proxy_protocol_"++atom_to_list(Name)).

get_proxy_target(State) ->
	Port = proplists:get_value(backend_port,State#proxy_listener.proplist,State#proxy_listener.listen_port),
	case proplists:get_value(proxy_host,State#proxy_listener.proplist,undefined) of
		undefined ->
			case proplists:get_value(pool,State#proxy_listener.proplist,undefined) of
				undefined ->
					none;
				Pool ->
					Retries = proplists:get_value(pool_retries,State#proxy_listener.proplist,3),
					{pool,Pool,Port,Retries}
			end;
		{ssl,Addr,BPort,SSLConf} ->
			{host_ssl,Addr,BPort,SSLConf};
		{Addr,BPort} ->
			{host,Addr,BPort};
		{_,_,_,_} = Addr ->
			{host,Addr,Port};
		{_,_,_,_,_,_,_,_} = Addr6 ->
			{host,Addr6,Port};
		_ ->
			none
	end.

resolve_target_list({pool,_Pool,_Port,_Retries}=PoolDef,_ListenConf) ->
	[PoolDef];
resolve_target_list({host,Host,Port},ListenConf) ->
	[{host,IP,Port} || IP <- proxy_protocol:resolve_addr(Host,ListenConf)];
resolve_target_list({host_ssl,Host,Port,SSLConf},ListenConf) ->
	[{host_ssl,IP,Port,SSLConf} || IP <- proxy_protocol:resolve_addr(Host,ListenConf)];
resolve_target_list(_,_Conf) ->
	[].
	
resolve_addr({ip,Addr},PPC) ->
%% 	?ERROR_MSG("Strip {ip,_}~n",[]),
	resolve_addr(Addr,PPC);
resolve_addr({_,_,_,_}=Addr,_PPC) ->
	[Addr];
resolve_addr({_,_,_,_,_,_,_,_}=Addr,_PPC) ->
	[Addr];
resolve_addr(HostStr,Conf) ->
	case proplists:get_value(inet6,Conf,false) of
		true ->
			case proxylib:inet_getaddr(HostStr,inet6) of
				{ip,{_,_,_,_,_,_,_,_}=Addr6} ->
					case proxylib:inet_getaddr(HostStr,inet) of
						{ip,Addr4} ->
							[Addr6,Addr4];
						_ ->
							[Addr6]
					end;
				{ip,Addr4} ->
					[Addr4];
				Noaddr -> 
					[{error,enohost,Noaddr}]
			end;
		false ->
			case proxylib:inet_getaddr(HostStr,inet) of
				{ip,Addr4} ->
					[Addr4];
				Noaddr ->
					[{error,enohost,Noaddr}]
			end
	end.

tcp_connect([]) ->
	{error,"No servers to connect to."};
tcp_connect([{pool,_Pool,_Port,0}|R]) ->
	tcp_connect(R);
tcp_connect([{pool,Pool,Port,Retries}|R]) ->
%% 	?ERROR_MSG("Getting host from pool ~p with ~p retries.~n",[Pool,Retries]),
	case gen_balancer:next(Pool,#client_info{}) of
		{_,_,_,_} = Addr ->
			tcp_connect([{host,Addr,Port},{pool,Pool,Port,Retries-1}|R]);
		{Addr,Port2} ->
			tcp_connect([{host,Addr,Port2},{pool,Pool,Port,Retries-1}|R]);
		_ -> tcp_connect(R)
	end;
tcp_connect([{host,{error,Err,Host},_Port}]) ->
	EMsg = io_lib:format("Internal proxy error: ~p connecting to ~p",[Err,Host]),
	{error,EMsg};
tcp_connect([{host,{error,Err,Host},_Port}|R]) ->
	?DEBUG_MSG("Error ~p connecting to host ~p~n",[Err,Host]),
	tcp_connect(R);
tcp_connect([{host_ssl,Host,Port,SSLConf}=ConnHost|R]) ->
	InetVer = proxylib:inet_version(Host),
	Timeout = 
	case R of
		[] -> 30000;
		_ -> 3000
	end,
	case ssl:connect(Host,Port,[binary,InetVer,{active,false}]++SSLConf,Timeout) of
		{ok,SSock0} ->
			gen_socket:create(SSock0,ssl);
		ErrConn when R == [] ->
			?ERROR_MSG("Could not Connect to server: ~p ~p~n",[ConnHost,ErrConn]),
			EMsg = io_lib:format("Internal proxy error: ~p",[ErrConn]),
			{error,EMsg};
		Err ->
			?DEBUG_MSG("tcp_connect SSL error: ~p ~p~nContinuing with next host in list.~n",[Host,Err]),
			tcp_connect(R)
	end;
tcp_connect([{host,Host,Port}=ConnHost|R]) ->
	InetVer = proxylib:inet_version(Host),
	Timeout = 
	case R of
		[] -> 30000;
		_ -> 3000
	end,
	case gen_tcp:connect(Host,Port,[binary,InetVer,{active,false}],Timeout) of
		{ok,SSock0} ->
			gen_socket:create(SSock0,gen_tcp);
		ErrConn when R == [] ->
			?ERROR_MSG("Could not Connect to server: ~p ~p~n",[ConnHost,ErrConn]),
			EMsg = io_lib:format("Internal proxy error: ~p",[ErrConn]),
			{error,EMsg};
		Err ->
			?DEBUG_MSG("tcp_connect error: ~p ~p~nContinuing with next host in list.~n",[Host,Err]),
			tcp_connect(R)
	end.
