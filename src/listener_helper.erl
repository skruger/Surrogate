%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 9, 2011
%%% -------------------------------------------------------------------
-module(listener_helper).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([listener_name/2,add_listener/1,delete_listener/1]).
-export([get_listeners/0,get_listener_details/1]).

-record(state, {running_vips}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

add_listener({Type,IP,Port,Opts}) ->
	LName = listener_name(IP,Port),
	Listener = #cluster_listener{name=LName,ip=IP,port=Port,type=Type,options=Opts},
	F1 = fun() ->
				 mnesia:write(Listener) end,
	mnesia:transaction(F1).

delete_listener(LName) ->
	mnesia:transaction(fun() -> mnesia:delete({cluster_listener,LName}) end).

%% start_vips(IP) ->
%% 	erlang:send_after(10000,self(),run),
%% 	receive	run -> ok after 10000 -> ok end,
%% 	Listeners = mnesia:dirty_index_read(cluster_listener,IP,ip),
%% 	error_logger:info_msg("Starting vip: ~p~n~p~n",[IP,Listeners]),
%% 	Stop = [stop_listener(L) || L <- Listeners],
%% 	Start = [start_listener(L) || L <- Listeners],
%% 	error_logger:info_msg("Tried stopping vip ~p before starting again:~n~p~nStarted vip ~p:~n~p~n",[IP,Stop,IP,Start]).

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
init([]) ->
	mnesia:create_table(cluster_listener,[{attributes,record_info(fields,cluster_listener)}]),
	mnesia:add_table_index(cluster_listener,ip),
	mnesia:change_table_copy_type(cluster_listener,node(),disc_copies),
	mnesia:add_table_copy(cluster_listener,node(),disc_copies),
    {ok, #state{running_vips=[]}}.

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
handle_cast({check_missing_vips,IPList},State) ->
	{noreply, State};
handle_cast({check_vip,IP},State) ->
	SupName = listener_sup:ip_sup_name(IP),
	ListenerProcs = lists:filter(fun({SupName,_,_,_}) -> true; (_) -> false end,
	gen_server:cast(self(),{check_vip_listeners,IP}),
								 supervisor:which_children(listener_sup)),
	case lists:member(SupName,[Sup || {Sup,_,_,_} <- ListenerProcs]) of
		true ->
			{noreply, State};
		false ->
			CSpec = listener_sup:ip_listener_childspec(IP),
			case supervisor:start_child(listener_sup,CSpec) of
				{ok,_Child} ->
					{noreply, State#state{running_vips = [IP|State#state.running_vips]}};
				_ ->
					{noreply, State}
			end
	end;
handle_cast({check_vip_listeners,IP},State) ->
	SupName = listener_sup:ip_sup_name(IP),
	%% Make sure all cluster_listener entries are started
	%% filter listeners under SupName leaving any that are not in cluster_listener
	%% Stop leftover listeners.
%% 	Listeners = mnesia:dirty_index_read(cluster_listener,IP,ip),
%% 	lists:foreach(fun(#cluster_listener{ip=IP,port=Port}) ->
%% 						  Name = listener_name(IP,Port),
%% 						  
%% 	ListnerNames = [listener_name(IP,Port) || #cluster_listener{ip=IP,port=Port} <- Listners],
	
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(check_vips,State) ->
	IPList = [IP || {IP,_,_} <- cluster_vip_manager:get_available_local_vips()],
	[gen_server:cast(self(),{check_vip,IP}) || IP <- IPList],
	gen_server:cast(self(),{check_missing_vips,IPList}),
	{noreply,State};
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

listener_name({ip,IP},Port) ->
	listener_name(IP,Port);
listener_name(IP,Port) ->
	list_to_atom(lists:flatten(io_lib:format("~s_~p",[proxylib:format_inet(IP),Port]))).


