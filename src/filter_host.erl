%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 11, 2010
%%% -------------------------------------------------------------------
-module(filter_host).

-behaviour(gen_server).
-behaviour(filter_check).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("filterproxy.hrl").


%% --------------------------------------------------------------------
%% External exports
-export([filter_start/0,filter_host/2,filter_url/2,test/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {config,deny_hosts}).


%% ====================================================================
%% External functions
%% ====================================================================

filter_start() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

filter_host(Host,User) ->
	gen_server:call(?MODULE,{host,Host,User}).

filter_url(Url,User) ->
	gen_server:call(?MODULE,{url,Url,User}).
			
	
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
	Props = proxyconf:get(filter_host,[]),
	gen_server:cast(self(),load_deny_hosts),
	{ok, #state{config=Props,deny_hosts=[]}}.


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
handle_call({host,Host,User},From,State) ->
	gen_server:cast(self(),{check_host,Host,From,User}),
	{noreply,State};
handle_call({url,Url,User},From,State) ->
	case string:str(Url,"/") of
		Idx when Idx < 2 ->
			{reply,ok,State};
		Idx ->
			Host = string:substr(Url,1,Idx-1),
			Path = string:substr(Url,Idx),
			gen_server:cast(self(),{check_url,Host,Path,From,User}),
			{noreply,State}
	end.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({check_host,Host,From,User},State) ->
	Fun = fun() ->
				  mnesia:read(filter_host_list,Host)
		  end,
	case mnesia:transaction(Fun) of
		{atomic,[]} ->
			case string:str(Host,".") of
				0 ->
					gen_server:reply(From,ok),
					{noreply,State};
				Idx ->
					NHost = string:substr(Host,Idx+1),
					gen_server:cast(self(),{check_host,NHost,From,User}),
					{noreply,State}
			end;
		{atomic,[Rec|_]} ->
			gen_server:reply(From,Rec#filter_host_list.rule),
			{noreply,State}
	end;
handle_cast({check_url,Host,Path,From,User},State) ->
	Txn = fun() ->
				  mnesia:read(filter_url_list,Host)
		  end,
	case mnesia:transaction(Txn) of
		{atomic,[]} ->
			case string:str(Host,".") of
  				0 ->
					gen_server:reply(From,ok),
					{noreply,State};
				Idx ->
					NHost = string:substr(Host,Idx+1),
					gen_server:cast(self(),{check_url,NHost,Path,From,User}),
					{noreply,State}
			end;
		{atomic,_UrlList} ->
%% 			io:format("Got URL List: ~p~n",[UrlList]),
			gen_server:cast(self(),{check_url_path,Host,Path,From,User}),
			{noreply,State}
	end;
handle_cast({check_url_path,Host,Path,From,User},State) ->
%% 	io:format("~p~n",[Req]),
	Txn = fun() ->
 				  MatchHead = #filter_url_list{host=Host,path='$1',rule='$2'},
				  mnesia:select(filter_url_list,[{MatchHead,[{'==','$1',Path}],['$2']}])
	end,
	case mnesia:transaction(Txn) of
		{atomic,[]} ->
			case string:rstr(Path,"/") of
				0 ->
					gen_server:reply(From,ok),
					{noreply,State};
				Idx ->
					NewPath = string:substr(Path,1,Idx-1),
					gen_server:cast(self(),{check_url_path,Host,NewPath,From,User}),
					{noreply,State}
			end;
		{atomic,[Rule|_]} ->
			gen_server:reply(From,Rule),
			{noreply,State}
	end;
handle_cast(load_deny_hosts,State) ->
	case proplists:get_value(deny_hosts,State#state.config,none) of
		none ->
			ok;
		FileName ->
			case file:read_file(FileName) of
				{ok,FileContents} ->
					F = fun() ->
								mnesia:clear_table(filter_host_list),
								split_lines(binary_to_list(FileContents))
						end,
					Res = mnesia:transaction(F),
					io:format("load_deny_hosts: ~p~n",[Res]);
				Error ->
					io:format("~p could not open file: ~p (~p)~n",[?MODULE,FileName,Error])
			end
	end,
	{noreply,State};
handle_cast(load_deny_urls,State) ->
	io:format("Loading urls.~n"),
	case proplists:get_value(deny_urls,State#state.config,none) of
		none ->
			ok;
		FileName ->
			load_urls(FileName,deny)
	end,
	{noreply,State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(state,State) ->
	io:format("~p state:~n~p~n",[?MODULE,State]),
	{noreply,State}.

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
%% filter_host_deny_hosts 
  
split_lines(File) ->
%% 	io:format("~p,~p~n",[File]),
	case string:str(File,"\n") of
		0 ->
			mnesia:write(#filter_host_list{host=File,rule=deny}),
			ok;
		Idx ->
 			Name = string:substr(File,1,Idx-1),
			mnesia:write(#filter_host_list{host=Name,rule=deny}),
			Rest = string:substr(File,Idx+1),
			split_lines(Rest)
	end.

load_urls(FileName,Rule) ->
	case file:read_file(FileName) of
		{ok,FileContents} ->
			mnesia:clear_table(filter_url_list),
			load_url_lines(binary_to_list(FileContents),Rule);
		Error ->
			io:format("~p could not open file: ~p (~p)~n",[?MODULE,FileName,Error])
	end.

load_url_lines(FileData,Rule) ->
	InsertFun = fun(Line) ->
						Idx = string:str(Line,"/"),
						Host = string:substr(Line,1,Idx-1),
						Path = string:strip(string:substr(Line,Idx),right,$/),
						NewRule = #filter_url_list{host=Host,path=Path,rule=Rule},
						mnesia:dirty_write(NewRule),
						io:format("Inserting line: ~p as ~p~n",[Line,NewRule]),
						ok
				end,
	case string:str(FileData,"\n") of
		0 ->
			InsertFun(FileData);
		Idx ->
			LDat = string:substr(FileData,1,Idx-1),
			InsertFun(LDat),
			Rest = string:substr(FileData,Idx+1),
			load_url_lines(Rest,Rule)
	end.

test(Name,Path) ->
	Fun = fun() ->
				  MatchHead = #filter_url_list{host=Name,path='$1',rule='$2'},
				  mnesia:select(filter_url_list,[{MatchHead,[{'==','$1',Path}],['$2']}])
%% 				  io:format("Got Sel=~p~n",[Sel]),
%% 				  Guard = {'==','$1',Name}
%% 				  MatchHead = #filter_host_list{host='$1',_='_'},
%% 				  Sel = mnesia:select(filter_host_list,[{MatchHead,[],['$1']}]),
%% 				  mnesia:read(filter_url_list,Name)
%% 				  Sel
%% 				  lists:map(fun(S) -> [R] = mnesia:read(filter_url_list,S),R end,Sel)
		  end,
	case mnesia:transaction(Fun) of
%% 		{atomic,[]} ->
%% 			ok;
%% 		{atomic,[Rec|_]} ->
%% 			Rec#filter_host_list.rule;
		Other ->
			Other
	end.
