%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Dec 16, 2010
%%% -------------------------------------------------------------------
-module(surrogate_api_cmd).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,register/2,exec/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {commands}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% ====================================================================
%% Server functions
%% ====================================================================

register(Name,Cmd) when is_atom(Name) ->
	?MODULE:register(atom_to_list(Name),Cmd);
register(Name,Cmd) ->
	gen_server:call(?MODULE,{register,Name,Cmd}).

exec(Name,Args) ->
	case gen_server:call(?MODULE,{get_command,Name}) of
		{ok,#api_command{module=Mod,function=Fun}} ->
%% 			?DEBUG_MSG("apply(~p,~p,~p)~n",[Mod,Fun,Args]),
			erlang:apply(Mod,Fun,[Args]);
		Err ->
			Err
	end.

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{commands=dict:new()}}.

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
handle_call({register,Name,Cmd},_From,State) ->
	?DEBUG_MSG("Register ~p ~p~n",[Name,Cmd]),
	{reply,ok,State#state{commands=dict:store(Name, Cmd, State#state.commands)}};
handle_call({get_command,Name},_From,State) ->
	case dict:find(Name,State#state.commands) of
		{ok,Cmd} ->
			{reply,{ok,Cmd},State};
		Err ->
			?WARN_MSG("Unknown api command: ~p~n~p~n",[Name,Err]),
			{reply,{error,invalid},State}
	end;
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

