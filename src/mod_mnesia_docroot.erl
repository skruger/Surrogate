%% Author: skruger
%% Created: Jul 30, 2011
%% Description: TODO: Add description to mod_mnesia_docroot
-module(mod_mnesia_docroot).

%%
%% Include files
%%

-include("surrogate.hrl").
-include_lib("stdlib/include/zip.hrl").

-behaviour(proxy_mod).

%%
%% Exported Functions
%%
-export([proxy_mod_start/1,proxy_mod_stop/1]).

-export([http_api/3]).
-export([create_docroot/1,create_local_docroot/1,load_docroot/2]).

%%
%% API Functions
%%

proxy_mod_start(_Opts) ->
	proxy_protocol_http_admin:register_module(?MODULE,?MODULE,http_api),
	ok.

proxy_mod_stop(_Opts) ->
	proxy_protocol_http_admin:unregister_module(?MODULE),
	ok.

http_api(_Path,_Request,_Conf) ->
	{404,[],<<"Not found">>}.


%%
%% Local Functions
%%


create_docroot(Name) ->
	mnesia:create_table(Name,[{attributes,record_info(fields,surrogate_docroot)},
							  {record_name,surrogate_docroot},{disc_only_copies,[node()]}]),
	case mnesia:table_info(Name,record_name) of
		surrogate_docroot ->
			catch mnesia:add_table_copy(Name,node(),disc_only_copies),
			ok;
		_Err ->
			?ERROR_MSG("Table ~p must have already existed!~n~p~n",[mnesia:table_info(Name,all)]),
			ok
	end.

create_local_docroot(Name) ->
	mnesia:create_table(Name,[{attributes,record_info(fields,surrogate_docroot)},
							  {record_name,surrogate_docroot},{disc_only_copies,[node()]},
							  {local_content,true}]),
	case mnesia:table_info(Name,record_name) of
		surrogate_docroot ->
			catch mnesia:add_table_copy(Name,node(),disc_only_copies),
			ok;
		_Err ->
			?ERROR_MSG("Table ~p must have already existed!~n~p~n",[mnesia:table_info(Name,all)]),
			ok
	end.

load_docroot(DocRoot,Data) ->
	case mnesia:table_info(DocRoot,record_name) of
		surrogate_docroot ->
			load_docroot2(DocRoot,Data);
		Rec ->
			?ERROR_MSG("load_docroot(~p,~p) failed!~nTable had wrong record_name: ~p~n",[DocRoot,Data,Rec]),
			{error,{badrec,Rec}}
	end.

load_docroot2(DocRoot,{zip,ZipFile}) ->
	case zip:zip_open(ZipFile,[memory]) of
		{ok,ZHandle} ->
			case zip:zip_list_dir(ZHandle) of
				{ok,ZipList} ->
					mnesia:clear_table(DocRoot),
					load_zip_docroot(DocRoot,ZHandle,ZipList);
				ListErr ->
					?ERROR_MSG("Couldn't zip_list_dir()~n~p~n",[ListErr]),
					ok
			end,
			zip:zip_close(ZHandle);
		ZipErr ->
			?ERROR_MSG("Couldn't open zip file for import.~n~p~n~p~n",[ZipFile,ZipErr]),
			ZipErr
	end.
			
										  
load_zip_docroot(_DocRoot,_ZHandle,[]) ->
	ok;
load_zip_docroot(DocRoot,ZHandle,[#zip_file{name=FName}|R]) ->
	case zip:zip_get(FName,ZHandle) of
		{ok,{_File,Binary}} ->
			Path = string:tokens(FName,"/"),
			mnesia:dirty_write(DocRoot,#surrogate_docroot{path=Path,filedata=Binary}),
			ok
	end,
	load_zip_docroot(DocRoot,ZHandle,R);
load_zip_docroot(DocRoot,ZHandle,[_|R]) ->
	load_zip_docroot(DocRoot,ZHandle,R).