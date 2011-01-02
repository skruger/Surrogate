
-module(surrogate_mnesia).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([delete_all/0,remove_disc_node/1]).

%%
%% API Functions
%%

remove_disc_node(Node) ->
	BakFile = "/tmp/backup-"++os:getpid()++".bak",
	?WARN_MSG("Backing up mnesia to ~p before removing disc node.",[BakFile]),
	case mnesia:backup(BakFile) of
		ok ->
			DiscNodes = disc_nodes(),
			Tables = mnesia:system_info(tables),
			Self = self(),
			try
			
				lists:foreach(fun(X) ->
									  case proxylib:rapply(X,mnesia,stop,[]) of
										  {error,timeout} ->
											  ?ERROR_MSG("Timeout stopping node ~p~n",[X]),
											  throw(stop_timeout);
										  Res ->
											  Res
									  end end,
							  DiscNodes),
				case mnesia:delete_schema(DiscNodes) of
					ok -> ok;
					{error,Reason} ->
						?ERROR_MSG("Could not delete schema on all mnesia nodes: ~p~n",[Reason]),
						throw(delete_error)
				end,
				NewDiscNodes = lists:delete(Node,DiscNodes),
				case mnesia:create_schema(NewDiscNodes) of
					ok -> ok;
					{error,CReason} ->
						?ERROR_MSG("Could not create schema on DB nodes (~p): ~p~nBackup retained at ~p~nMnesia is currently broken!~n",
								   [NewDiscNodes,CReason,BakFile]),
						throw(create_error)
				end,
				lists:foreach(fun(X) ->
									  case proxylib:rapply(X,mnesia,start,[]) of
										  {error,timeout} ->
											  ?ERROR_MSG("Timeout starting node ~p~nMnesia was not yet restored from backup: ~p~n",[X,BakFile]),
											  throw(start_timeout);
										  Res -> Res
									  end end,
											  
							  NewDiscNodes),
				case mnesia:restore("/tmp/mnesia.bak",[{recreate_tables,Tables}]) of
					{atomic,Tabs} ->
						?WARN_MSG("Restored tables from backup: ~p~n",[Tabs]),
						ok;
					{aborted,AReason} ->
						?ERROR_MSG("Error restoring backup after removing disc node: ~p~n",[AReason]),
						throw(restore_error)
				end
			catch
				_:RErr ->
					{error,RErr}
			end;
		BErr ->
			BErr
	end.
			
								  

disc_nodes() ->
	mnesia:table_info(schema,disc_copies).


add_discless_node(Node) ->
	mnesia:change_config(extra_db_nodes,[Node]).


delete_all() ->
	[
	 mnesia:delete_table(filter_host_list),
	 mnesia:delete_table(filter_url_list),
	 mnesia:delete_table(proxy_userinfo)
	].
	



%%
%% Local Functions
%%

