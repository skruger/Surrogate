%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 13, 2010
%%% -------------------------------------------------------------------
-module(proxy_auth).

-behaviour(gen_server).
-behaviour(proxy_mod).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").
-include("mysql.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1,proxy_mod_start/1,proxy_mod_stop/1]).

-export([check_user/2,is_user/1,add_user/2,list_users/0,delete_user/1]).
-export([check_user/3,is_user/2,add_user/3,list_users/1,delete_user/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {mode}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link(Conf) ->
	gen_server:start_link({local,?MODULE},?MODULE,Conf,[]).

proxy_mod_start(Conf) ->
	Spec = {proxy_auth,{proxy_auth,start_link,[Conf]},
			permanent,10000,worker,[]},
	case supervisor:start_child(surrogate_sup,Spec) of
		{error,_} = Autherr ->
			?CRITICAL("Starting auth failed: ~p~n~p~n",[Autherr,Spec]);
		_ -> ?DEBUG_MSG("Auth process started in proxy_mod_start().~n",[])
	end.
	
proxy_mod_stop(_Conf) ->
	ok.

%% ====================================================================
%% Server functions
%% ====================================================================

%% check_user("skruger","test") ->
%% 	ok;
check_user(User,Pass) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{auth,User,Pass}).
list_users() ->
	gen_server:call(pg2:get_closest_pid(?MODULE),list_users).
is_user(Uname) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{is_user,Uname}).
add_user(User,Pass) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{add_user,User,Pass}).
delete_user(User) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{delete_user,User}).

check_user(Mode,User,Pass) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{Mode,auth,User,Pass}).
list_users(Mode) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{Mode,list_users}).
is_user(Mode,Uname) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{Mode,is_user,Uname}).
add_user(Mode,User,Pass) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{Mode,add_user,User,Pass}).
delete_user(Mode,User) ->
	gen_server:call(pg2:get_closest_pid(?MODULE),{Mode,delete_user,User}).


%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Conf) ->
	pg2:create(?MODULE),
	pg2:join(?MODULE,self()),
	Mode = proplists:get_value(default_auth,Conf,mnesia),
	case proxyconf:get(mode,worker) of
		master ->
			mnesia:create_table(proxy_userinfo, [{attributes, record_info(fields, proxy_userinfo)}]),
			mnesia:change_table_copy_type(proxy_userinfo,node(),disc_copies);
		_ ->
			ok
	end,
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
handle_call({mnesia,list_users},_From,State) ->
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
handle_call({mnesia,is_user,UserName},_From,State) ->
	F = fun() ->
				mnesia:read(proxy_userinfo,string:to_lower(UserName))
  		end,
	Ret = mnesia:transaction(F),
	{reply,Ret,State};
handle_call({mnesia,auth,User,Pass},_From,State) ->
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
handle_call({mnesia,add_user,User,Pass},_From,State) ->
	F1 = fun() ->
				 mnesia:write(#proxy_userinfo{username=string:to_lower(User),password=crypto:md5(Pass)})
		 end,
	Ret = mnesia:transaction(F1),
	{reply,Ret,State};
handle_call({mneisa,delete_user,User},_From,State) ->
	F = fun() ->
				mnesia:delete({proxy_userinfo,User})
		end,
	Ret = mnesia:transaction(F),
	{reply,Ret,State};
handle_call({{mysql,Conn},auth,User0,Pass},_From,State) ->
	User = string:to_lower(User0),
	case mysql:fetch(Conn,"select name from user_auth where name='"++User++"' and password=md5('"++Pass++"')") of
		{data,#mysql_result{rows=[]}} ->
			{reply,false,State};
		{data,#mysql_result{rows=[[Name|_]|_]}} ->
			{reply,{ok,#proxy_userinfo{username=binary_to_list(Name)}},State}
	end;
handle_call({{mysql,Conn},add_user,User0,Pass},_From,State) ->
	User = string:to_lower(User0),
	case mysql:fetch(Conn,"replace into user_auth (name,password) values ('"++User++"',md5('"++Pass++"'))") of
		{updated,_} ->
			{reply,ok,State};
		Err ->
			{reply,{error,Err},State}
	end;
handle_call({{mysql,Conn},delete_user,User},_From,State) ->
	mysql:fetch(Conn,"DELETE FROM user_auth where name='"++User++"'"),
	{reply,ok,State};
handle_call({{mysql,Conn},is_user,UserName},_From,State) ->
	User = string:to_lower(UserName),
	case mysql:fetch(Conn,"select name from user_auth where name='"++User++"'") of
		{data,#mysql_result{rows=[]}} ->
			{reply,false,State};
		{data,#mysql_result{rows=[[Name|_]|_]}} ->
			{reply,{ok,#proxy_userinfo{username=binary_to_list(Name)}},State}
	end;
handle_call({{mysql,Conn},list_users},_From,State) ->
	case mysql:fetch(Conn,"select name from user_auth order by name") of
		{data,#mysql_result{rows=Names}} ->
			N = lists:map(fun(X) ->
								  [Y] = X,
								  binary_to_list(Y) end,Names),
			{reply,N,State};
		Err ->
			{reply,Err,State}
	end;
handle_call({auth,User,Pass},From,State) ->
	handle_call({State#state.mode,auth,User,Pass},From,State);
handle_call(list_users,From,State) ->
	handle_call({State#state.mode,list_users},From,State);
handle_call({is_user,Uname},From,State) ->
	handle_call({State#state.mode,is_user,Uname},From,State);
handle_call({add_user,User,Pass},From,State) ->
	handle_call({State#state.mode,add_user,User,Pass},From,State);
handle_call({delete_user,User},From,State) ->
	handle_call({State#state.mode,delete_user,User},From,State);
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

