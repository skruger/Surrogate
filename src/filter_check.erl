%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Nov 7, 2010
%%% -------------------------------------------------------------------
-module(filter_check).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([url/3,host/3]).

-export([behaviour_info/1]).

%% ====================================================================
%% External functions
%% ====================================================================

behaviour_info(callbacks) ->
	[{filter_url,2},{filter_host,2},{filter_start,0}];
behaviour_info(_) ->
	undefined.

%% ====================================================================
%% Server functions
%% ====================================================================

url(FilterName,Url,User) ->
%% 	io:format("~p checking rules for ~p ~p~n",[FilterName,Url,User]),
	try
		FilterName:filter_url(Url,User)
	catch
		exit:ExitErr ->
			?ERROR_MSG("~p:filter_url(~p,~p)~nCaught exit: ~p~n",[FilterName,Url,User,ExitErr]),
			ok;
		error:Error ->
			?ERROR_MSG("~p:filter_url(~p,~p)~nCaught error: ~p~n",[FilterName,Url,User,Error]),
			ok
	end.
	
host(FilterName,Host,User) ->
%% 	io:format("~p checking rules for ~p ~p~n",[FilterName,Host,User]),
	try
		FilterName:filter_host(Host,User)
	catch
		exit:ExitErr ->
			?ERROR_MSG("~p:filter_host(~p,~p)~nCaught exit: ~p~n",[FilterName,Host,User,ExitErr]),
			ok;
		error:Error ->
			?ERROR_MSG("~p:filter_host(~p,~p)~nCaught error: ~p~n",[FilterName,Host,User,Error]),
			ok
	end.
	

