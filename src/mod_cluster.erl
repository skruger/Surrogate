-module(mod_cluster).

-behaviour(proxy_mod).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([proxy_mod_start/1,proxy_mod_stop/1]).

-export([vip_state/3,listener_name/2,add_listener/1]).

-record(cluster_listener,{name,ip,port,type,options,supervisor,sup_process_name}).

%%
%% API Functions
%%

listener_name({ip,IP},Port) ->
	listener_name(IP,Port);
listener_name(IP,Port) ->
	list_to_atom(lists:flatten(io_lib:format("~s_~p",[proxylib:format_inet(IP),Port]))).

proxy_mod_start(Conf) ->
	mnesia:create_table(cluster_listener,[{attributes,record_info(fields,cluster_listener)}]),
	mnesia:add_table_index(cluster_listener,ip),
	mnesia:change_table_copy_type(cluster_listener,node(),disc_copies),
	
	application:load(cluster_supervisor),
	application:set_env(cluster_supervisor,cluster_config,Conf),
	application:start(cluster_supervisor),
	cluster_supervisor_callback:add(vip_state, ?MODULE, Conf).

proxy_mod_stop(_Conf) ->
	application:stop(cluster_supervisor).


add_listener({Type,IP,Port,Opts}) ->
	LName = listener_name(IP,Port),
	Listener = #cluster_listener{name=LName,ip=IP,port=Port,type=Type,options=Opts},
	F1 = fun() ->
				 mnesia:write(Listener) end,
	mnesia:transaction(F1).

%% mod_cluster:add_listener({proxy_http,{ip,{192,168,19,199}},8087,[inet,{proxy_host,{{ip,{192,168,19,58}},80}},{proxy_filters,[filter_host]},{filter_headers,[{add,"X-modified-by: Surrogate"}]},{proxy_auth,true}]}).
%% {stream_filters,[filter_auth_basic]},

% State:
%% {down,Node,Vip}
%% {up,Node,Vip}
%% {change_node,OldNode,NewNode,Vip}
%% up and down state change notifications call the callback module existing on the host that had the Vip.
%% change_node notifications may go to nodes other than this one.

vip_state({up,_Node,_Vip},IP,_Extra) ->
	Listeners = mnesia:dirty_index_read(cluster_listener,IP,ip),
	error_logger:info_msg("Starting vip: ~p~n~p~n",[IP,Listeners]),
	[stop_listener(L) || L <- Listeners],
	F1 = fun(LRec) ->
				 #cluster_listener{name=_LName,ip=IP,port=Port,type=Type,options=Opts} = LRec,
				 L1 = {Type,IP,Port,Opts},
				 CSpec = listener_sup:make_childspec(L1),
				 Sup = {listener_sup,node()},
				 ProcNames = [element(1,C) || C <- CSpec],
				 [supervisor:start_child(listener_sup,Child) || Child <- CSpec],
				 mnesia:write(LRec#cluster_listener{supervisor=Sup,sup_process_name=ProcNames}),

			ok end,
	[mnesia:transaction(F1,[L]) || L <- Listeners];
vip_state({down,_Node,_Vip},IP,_Extra) ->
	Listeners = mnesia:dirty_index_read(cluster_listener,IP,ip),
	error_logger:info_msg("Starting vip: ~p~n~p~n",[IP,Listeners]),
	[stop_listener(L) || L <- Listeners];
vip_state(State,Vip,Extra) ->
	error_logger:info_msg("Received vip state for vip ~p: ~p~nExtra:~p~n", [Vip,State,Extra]),
	ok.

%%
%% Local Functions
%%

stop_listener(#cluster_listener{supervisor=Sup,sup_process_name=PName}) when Sup /= undefined, is_list(PName) ->
	lists:foreach(fun(Proc) ->
						  supervisor:terminate_child(Sup,Proc),
						  supervisor:delete_child(Sup,Proc) end,PName),
	ok;
stop_listener(_L) ->
%% 	error_logger:info_msg("Nothing to stop: ~p~n",[L]),
	ok.
