%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 13, 2010
%%% -------------------------------------------------------------------
-module(proxy_auth).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").
-include("mysql.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0,check_user/2,is_user/1,add_user/2,list_users/0,delete_user/1]).

-export([apicmd/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {mode}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

apicmd(["add_user",User,Pass]) ->
	add_user(User,Pass);
apicmd(Args) ->
	?DEBUG_MSG("Got unexpected args: ~p~n",[Args]),
	{error,invalid}.
	


%% ====================================================================
%% Server functions
%% ====================================================================

%% check_user("skruger","test") ->
%% 	ok;
check_user(User,Pass) ->
	gen_server:call(?MODULE,{auth,User,Pass}).
list_users() ->
	gen_server:call(?MODULE,list_users).
is_user(Uname) ->
	gen_server:call(?MODULE,{is_user,Uname}).
add_user(User,Pass) ->
	gen_server:call(?MODULE,{add_user,User,Pass}).
delete_user(User) ->
	gen_server:call(?MODULE,{delete_user,User}).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	surrogate_api_cmd:register("proxy_auth_cmd",#api_command{module=?MODULE,function=apicmd}),
	Mode = proxyconf:get(auth_mode,mnesia),
    {ok, #state{mode=Mode}}.

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
handle_call(list_users,_From,State) when State#state.mode == mnesia ->
%% 	Txn = fun() ->
%%  				  MatchHead = #filter_url_list{host=Host,path='$1',rule='$2'},
%% 				  mnesia:select(filter_url_list,[{MatchHead,[{'==','$1',Path}],['$2']}])
%% 	end,
	
	F = fun() ->
				Match = #proxy_userinfo{username='$1',_='_'},
				mnesia:select(proxy_userinfo,[{Match,[],['$1']}])
		end,
	case mnesia:transaction(F) of
		{atomic,Ret} when is_list(Ret) ->
			{reply,Ret,State};
		Ret ->
			{reply,{error,Ret},State}
	end;
handle_call({is_user,UserName},_From,State) when State#state.mode == mnesia ->
	F = fun() ->
				mnesia:read(proxy_userinfo,string:to_lower(UserName))
  		end,
	Ret = mnesia:transaction(F),
	{reply,Ret,State};
handle_call({auth,User,Pass},_From,State) when State#state.mode == mnesia ->
	CPass = crypto:md5(Pass),
	F = fun() ->
				mnesia:read(proxy_userinfo,string:to_lower(User))
  		end,
	case mnesia:transaction(F) of
		{atomic,[#proxy_userinfo{password=CPass}=UserInfo]} ->
			{reply,{ok,UserInfo#proxy_userinfo{password=undefined}},State};
		_ ->
			{reply,false,State}
	end;
handle_call({add_user,User,Pass},_From,State) when State#state.mode == mnesia ->
	F1 = fun() ->
				 mnesia:write(#proxy_userinfo{username=string:to_lower(User),password=crypto:md5(Pass)})
		 end,
	Ret = mnesia:transaction(F1),
	{reply,Ret,State};
handle_call({delete_user,User},_From,State) when State#state.mode == mnesia ->
	F = fun() ->
				mnesia:delete({proxy_userinfo,User})
		end,
	Ret = mnesia:transaction(F),
	{reply,Ret,State};
handle_call({auth,User0,Pass},_From,State) when State#state.mode == mysql ->
	User = string:to_lower(User0),
	case mysql:fetch(mysql,"select name from user_auth where name='"++User++"' and password=md5('"++Pass++"')") of
		{data,#mysql_result{rows=[]}} ->
			{reply,false,State};
		{data,#mysql_result{rows=[[Name|_]|_]}} ->
			{reply,{ok,#proxy_userinfo{username=binary_to_list(Name)}},State}
	end;
handle_call({add_user,User0,Pass},_From,State) when State#state.mode == mysql ->
	User = string:to_lower(User0),
	case mysql:fetch(mysql,"replace into user_auth (name,password) values ('"++User++"',md5('"++Pass++"'))") of
		{updated,_} ->
			{reply,ok,State};
		Err ->
			{reply,{error,Err},State}
	end;
handle_call({is_user,UserName},_From,State) when State#state.mode == mysql ->
	User = string:to_lower(UserName),
	case mysql:fetch(mysql,"select name from user_auth where name='"++User++"'") of
		{data,#mysql_result{rows=[]}} ->
			{reply,false,State};
		{data,#mysql_result{rows=[[Name|_]|_]}} ->
			{reply,{ok,#proxy_userinfo{username=binary_to_list(Name)}},State}
	end;
handle_call(list_users,_From,State) when State#state.mode == mysql ->
	case mysql:fetch(mysql,"select name from user_auth order by name") of
		{data,#mysql_result{rows=Names}} ->
			N = lists:map(fun(X) ->
								  [Y] = X,
								  binary_to_list(Y) end,Names),
			{reply,N,State};
		Err ->
			{reply,Err,State}
	end;
handle_call(Request, _From, State) ->
	?ERROR_MSG("Invalid auth request for mode ~p~n~p~n",[State#state.mode,Request]),
    Reply = {error,invalid},
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

