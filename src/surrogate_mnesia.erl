
-module(surrogate_mnesia).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([delete_all/0,remove_disc_node/1,stop_remove_disc_node/1,add_disc_node/2,tables/0,backup_tables/1,disc_nodes/0,add_discless_node/1]).

%%
%% API Functions
%%

tables() ->
	lists:delete(schema,mnesia:system_info(tables)).

backup_tables(T) ->
	TableAttributes = 
		lists:map(fun (X) ->
						   [X,[{attributes,mnesia:table_info(X,attributes)}]] end,T),
	TableData =
		lists:map(fun (X) ->
						   {X,ets:tab2list(X)}
						   end,T),
	 
		
	[{tables,TableAttributes},{tabledata,TableData}].

add_disc_node(Node,Props) ->
	add_discless_node(Node),
	mnesia:add_table_copy(schema,Node,disc_copies),
	mnesia:change_table_copy_type(schema,Node,disc_copies),
	lists:foreach(fun(X) ->
						  mnesia:add_table_copy(X,Node,disc_copies)
				  end,
				  proplists:get_value(disc_copies,Props,[])),
	lists:foreach(fun(X) ->
						  mnesia:add_table_copy(X,Node,ram_copies)
				  end,
				  proplists:get_value(ram_copies,Props,[])).
	

remove_disc_node(Node) ->
	Tables = tables(),
	RM = lists:map(fun(X) -> {X,mnesia:del_table_copy(X,Node)} end,Tables),
	case rpc:call(Node,mnesia,stop,[],5000) of
		{badrpc,Err} ->
			{error,Err};
		_ ->
			[mnesia:del_table_copy(schema,Node)|RM]
	end.
		

stop_remove_disc_node(Node) ->
	BakFile = "/tmp/mnesia-"++os:getpid()++".bak",
	?WARN_MSG("Backing up mnesia to ~p before removing disc node.",[BakFile]),
	DiscNodes = disc_nodes(),
	Tables = tables(),
	try
		lists:foreach(fun(X) -> mnesia:del_table_copy(X,Node) end,
					  Tables),
		case mnesia:backup(BakFile) of
			ok -> ok;
			BErr ->
				?ERROR_MSG("Backup error in remove_disc_node(): ~p~n",[BErr]),
				throw(backup_error)
		end,
		lists:foreach(fun(X) ->
							  case rpc:call(X,mnesia,stop,[],5000) of
								  {badrpc,timeout} ->
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
				?WARN_MSG("Could not create schema on DB nodes (~p): ~p~nBackup retained at ~p~nMnesia may break on the next step!~n",
						   [NewDiscNodes,CReason,BakFile]),
				ok
		end,
		lists:foreach(fun(X) ->
							  case rpc:call(X,mnesia,start,[],5000) of
								  {badrpc,timeout} ->
									  ?ERROR_MSG("Timeout starting node ~p~nMnesia was not yet restored from backup: ~p~n",[X,BakFile]),
									  throw(start_timeout);
								  Res -> Res
							  end end,
					  NewDiscNodes),
		case mnesia:restore(BakFile,[{recreate_tables,Tables}]) of
			{atomic,_Tables1} -> ok;
			{error,IFReason} ->
				?CRITICAL("Could not load from backup: ~p~n",[IFReason]),
				throw(norestore)
		end
	catch
		_:RErr ->
			{error,RErr}
	end.
			
								  

disc_nodes() ->
	mnesia:table_info(schema,disc_copies).


add_discless_node(Node) ->
	case lists:member(Node,mnesia:system_info(db_nodes)) of
		false ->
%% 			NodeList = [Node|mnesia:system_info(extra_db_nodes)],
			?INFO_MSG("Adding extra_db_node ~p ~n",[Node]),
			rpc:call(Node,mnesia,stop,[]),
			rpc:call(Node,mnesia,delete_schema,[[Node]]),
			rpc:call(Node,mnesia,start,[]),
			mnesia:change_config(extra_db_nodes,[Node]);
		_ ->
			{error, already_node}
	end.


delete_all() ->
	[
	 mnesia:delete_table(filter_host_list),
	 mnesia:delete_table(filter_url_list),
	 mnesia:delete_table(proxy_userinfo)
	].
	



%%
%% Local Functions
%%

