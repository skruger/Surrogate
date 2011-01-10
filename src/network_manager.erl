%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Jan 9, 2011
%%% -------------------------------------------------------------------
-module(network_manager).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("surrogate.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([get_interfaces/0,get_interface_addr/1,ip_list_to_tuple/1,get_interface_proplist/1,get_interface_proplist/0,discover_node_interfaces/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-record(network_interfaces,{node,interface,address}).

%% ====================================================================
%% External functions
%% ====================================================================

get_interfaces() ->
	string:tokens(os:cmd("ifconfig -a -s |egrep \"(^eth|^bond)\" | cut -f 1 -d ' ' "),"\n").

get_interface_addr(Iface) ->
	Cmd = "ifconfig "++Iface++" |sed -n 's/.*inet *addr:\\([0-9\\.]*\\).*/\\1/p'",
%% 	io:format("~p~n",[Cmd]),
	string:strip(os:cmd(Cmd),right,$\n).

ip_list_to_tuple(Ip) ->
	try
		DigitList = 
			lists:map(fun(D) ->
							  list_to_integer(D)
					  end,string:tokens(Ip,".")),
		case DigitList of
			List when is_list(List) and (length(List) == 4) ->
				{ip,list_to_tuple(List)};
			_ ->
				{error,badformat}
		end
	catch
		_:Err ->
			{error,Err}
	end.

get_interface_proplist() ->
	get_interface_proplist(get_interfaces()).

get_interface_proplist(Interfaces) ->
	lists:map(fun(Iface) ->
					  {list_to_atom(Iface),ip_list_to_tuple(get_interface_addr(Iface))}
			  end,Interfaces).

is_alias_interface(Iface) ->
	case string:str(Iface,":") of
		0 ->
			false;
		_ ->
			true
	end.

discover_node_interfaces(Node) ->
	rpc:call(Node,?MODULE,get_interface_proplist,[]).

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
    {ok, #state{}}.

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
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

