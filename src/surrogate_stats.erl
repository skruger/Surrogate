%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Oct 22, 2011
%%% -------------------------------------------------------------------
-module(surrogate_stats).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([add_counter/3]).
-export([http_api/3]).

%% gen_server callbacks
-export([start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(surrogate_stats,{listener_counter,value}).

-record(state, {statdict}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

add_counter(Listener,Counter,Count) when Listener == undefined; Counter == undefined; not is_integer(Count) ->
	ok;  %% Do nothing if any inputs are invalid.
add_counter(Listener,Counter,Count) ->
	gen_server:cast(?MODULE,{add_counter,Listener,Counter,Count}).

http_api(["stats.json"],#http_admin{method='GET'} = Request,_Conf)  when Request#http_admin.has_auth == true ->
	StatsList =
	lists:map(fun(Key) ->
					  ?ERROR_MSG("Key: ~p~n",[Key]),
					  [#surrogate_stats{listener_counter={Listen,Ctr},value=Val}|_] = mnesia:dirty_read(surrogate_stats,Key),
					  ?ERROR_MSG("Ok",[]),
					  {struct,[{"listener",list_to_binary(atom_to_list(Listen))},
							   {"counter",list_to_binary(atom_to_list(Ctr))},
							   {"value",list_to_binary(integer_to_list(Val))}]}
			  end,mnesia:dirty_all_keys(surrogate_stats)),
	{200,[],iolist_to_binary(mjson:encode(lists:sort(StatsList)))};
http_api(Path,Request,_Conf) when Request#http_admin.has_auth == true ->
	Err = io_lib:format("Not found in ~p: ~p~n",[?MODULE,Path]),
	{404,[{'Content-type',"text/plain"}],iolist_to_binary(Err)};
http_api(Path,Request,_Conf) ->
	?ERROR_MSG("Authorization required: ~p~n~p~n",[Path,Request]),
	{401,[{"WWW-Authenticate","Basic realm=\"mod_cluster_admin\""}],iolist_to_binary("Authorization required")}.


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
%% 	erlang:start_timer(1000,self(),commit),
	mnesia:create_table(surrogate_stats,[{attributes,record_info(fields,surrogate_stats)}]),
	mnesia:change_table_copy_type(surrogate_stats,node(),disc_copies),
	mnesia:add_table_copy(surrogate_stats,node(),disc_copies),
	proxy_protocol_http_admin:register_module(?MODULE,?MODULE,http_api),
	self() ! commit,
	{ok, #state{statdict=dict:new()}}.

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
handle_cast({add_counter,Listener,Counter,Count},State) ->
%% 	?ERROR_MSG("Increment counter: ~p ~p ~p~n",[Listener,Counter,Count]),
	K = {Listener,Counter},
	case dict:find(K,State#state.statdict) of
		{ok,OldCount} ->
			Dict = dict:store(K,OldCount+Count,State#state.statdict),
			{noreply,State#state{statdict=Dict}};
		_ ->
			Dict = dict:store(K,Count,State#state.statdict),
			{noreply,State#state{statdict=Dict}}
	end;
handle_cast(commit,State) ->
	{noreply,State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(commit,State) ->
	NewDict = 
	case dict:size(State#state.statdict) of
		Size when Size > 0 ->
			Counters = dict:to_list(State#state.statdict),
			F1 = fun({LCt,Val}) ->
						 case mnesia:read(surrogate_stats,LCt,write) of
							 [#surrogate_stats{value=OldVal}=Rec] ->
								 mnesia:write(Rec#surrogate_stats{value=OldVal+Val});
							 _ ->
								 mnesia:write(#surrogate_stats{listener_counter=LCt,value=Val})
						 end end,
			F2 = fun() -> lists:foreach(F1,Counters) end,
			mnesia:transaction(F2),
			?ERROR_MSG("Committing: ~p~n",[Counters]),
			dict:new();
		_ ->
			State#state.statdict
	end,
	erlang:send_after(2500,self(),commit),
	{noreply,State#state{statdict=NewDict}};
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
