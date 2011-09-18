-module(mod_cluster).

-behaviour(gen_server).
-behaviour(proxy_mod).

%%
%% Include files
%%
-include("surrogate.hrl").
%%
%% Exported Functions
%%
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/1]).

-export([proxy_mod_start/1,proxy_mod_stop/1]).

-export([vip_state/3,listener_name/2,add_listener/1,delete_listener/1,start_vips/1]).
-export([get_listeners/0,get_listener_details/1]).

-record(state,{opts}).

%%
%% API Functions
%%

listener_name({ip,IP},Port) ->
	listener_name(IP,Port);
listener_name(IP,Port) ->
	list_to_atom(lists:flatten(io_lib:format("~s_~p",[proxylib:format_inet(IP),Port]))).

proxy_mod_start(Conf) ->
	R = supervisor:start_child(surrogate_sup,{?MODULE,{?MODULE,start_link,[Conf]},permanent,1000,worker,[?MODULE]}),
	error_logger:error_msg("~p returned ~p~n~n~n~n",[?MODULE,R]),
	mnesia:create_table(cluster_listener,[{attributes,record_info(fields,cluster_listener)}]),
	mnesia:add_table_index(cluster_listener,ip),
	mnesia:change_table_copy_type(cluster_listener,node(),disc_copies),
	mnesia:add_table_copy(cluster_listener,node(),disc_copies),
	case proplists:get_value(start_cluster_supervisor,Conf,true) of
		false ->
			ok;
		_ ->
			application:load(cluster_supervisor),
			application:set_env(cluster_supervisor,cluster_config,Conf),
			application:start(cluster_supervisor),
			cluster_supervisor_callback:add(vip_state, ?MODULE, Conf)
	end,
	ok.

proxy_mod_stop(_Conf) ->
	try
		supervisor:terminate_child(surrogate_sup, ?MODULE),
		supervisor:delete_child(surrogate_sup, ?MODULE)
	catch _:_ -> ok end,
	application:stop(cluster_supervisor).

add_listener({Type,IP,Port,Opts}) ->
	LName = listener_name(IP,Port),
	Listener = #cluster_listener{name=LName,ip=IP,port=Port,type=Type,options=Opts},
	F1 = fun() ->
				 mnesia:write(Listener) end,
	mnesia:transaction(F1).

delete_listener(LName) ->
	mnesia:transaction(fun() -> mnesia:delete({cluster_listener,LName}) end).

start_link(Opts) ->
	gen_server:start_link({local,?MODULE},?MODULE,Opts,[]).

init(Opts) ->
	error_logger:info_msg("~p starting up.",[?MODULE]),
	{ok,#state{opts=Opts}}.

handle_call({enable_vip,IP},_From,State) ->
	Listeners = mnesia:dirty_index_read(cluster_listener,IP,ip),
	error_logger:info_msg("Starting vip: ~p~n~p~n",[IP,Listeners]),
	_Stop = [stop_listener(L) || L <- Listeners],
	[self() ! {start_listener,IP,L} || L <- Listeners],
	{reply,ok,State};
handle_call(_Msg,_From,State) ->
	{reply,ok,State}.

handle_cast(_Msg,State) ->
	{noreply,State}.

handle_info({start_listener,IP,#cluster_listener{name=_LName,ip=IP,port=Port,type=Type,options=Opts}=_Listener},State) ->
	L = {Type,IP,Port,Opts},
	?INFO_MSG("Starting listener: ~p~n",[L]),
	CSpec = listener_sup:make_childspec(L),
	[ self() ! {start_child,IP,L,C} || C <- CSpec],
	{noreply,State};
handle_info({start_child,_IP,_L,C}=StartChild,State) ->
	ChildName = element(1,C),
	?INFO_MSG("Starting child: ~p~n",[ChildName]),
	case supervisor:start_child(listener_sup,C) of
		{ok,_,_} ->
			?INFO_MSG("Child started: ~p~n",[ChildName]);
		{ok,_} ->
			?INFO_MSG("Child started: ~p~n",[ChildName]);
		Err ->
			?INFO_MSG("Child ~p not started.  Retrying...~n~p~n",[ChildName,Err]),
			erlang:send_after(5000,self(),StartChild)
	end,
	{noreply,State};
handle_info(_Info,State) ->
	{noreply,State}.

terminate(_Reason,_State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% State:
%% {down,Node,Vip}
%% {up,Node,Vip}
%% {change_node,OldNode,NewNode,Vip}
%% up and down state change notifications call the callback module existing on the host that had the Vip.
%% change_node notifications may go to nodes other than this one.

vip_state({up,_Node,_Vip},IP,_Extra) ->
	gen_server:call(?MODULE,{enable_vip,IP});
%% 	spawn(?MODULE,start_vips,[IP]);
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

start_vips(IP) ->
	erlang:send_after(10000,self(),run),
	receive	run -> ok after 10000 -> ok end,
	Listeners = mnesia:dirty_index_read(cluster_listener,IP,ip),
	error_logger:info_msg("Starting vip: ~p~n~p~n",[IP,Listeners]),
	Stop = [stop_listener(L) || L <- Listeners],
	Start = [start_listener(L) || L <- Listeners],
	error_logger:info_msg("Tried stopping vip ~p before starting again:~n~p~nStarted vip ~p:~n~p~n",[IP,Stop,IP,Start]).

start_listener(#cluster_listener{name=_LName,ip=IP,port=Port,type=Type,options=Opts} = LRec) ->
	L1 = {Type,IP,Port,Opts},
	CSpec = listener_sup:make_childspec(L1),
	ProcNames = start_listener_children(CSpec,[]),
	Sup = {listener_sup,node()},
	mnesia:transaction(fun() -> mnesia:write(LRec#cluster_listener{supervisor=Sup,sup_process_name=ProcNames}) end).
	
start_listener_children([],Acc) ->
	Acc;
start_listener_children([C|R],Acc) ->
	ProcName = element(1,C),
	case supervisor:start_child(listener_sup,C) of
		{ok,_,_} ->
			start_listener_children(R,[ProcName|Acc]);
		{ok,_} ->
			start_listener_children(R,[ProcName|Acc]);
		Err ->
			error_logger:error_msg("Error starting listener child: ~n~p~n~p~n",[Err,C]),
			start_listener_children(R,Acc)
	end.
	

stop_listener(#cluster_listener{supervisor=Sup,sup_process_name=PName}) when Sup /= undefined, is_list(PName) ->
	lists:foreach(fun(Proc) ->
						  try
							  supervisor:terminate_child(Sup,Proc),
							  supervisor:delete_child(Sup,Proc) 
						  catch _:Err -> Err end end,PName),
	ok;
stop_listener(_L) ->
%% 	error_logger:info_msg("Nothing to stop: ~p~n",[L]),
	ok.

get_listeners() ->
	mnesia:dirty_all_keys(cluster_listener).

get_listener_details(L) ->
	case mnesia:dirty_read(cluster_listener,L) of
		[R|_] -> R;
		R -> R
	end.
		
