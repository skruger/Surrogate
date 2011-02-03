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
-export([get_interfaces/0,get_interface_addr/1,ip_list_to_tuple/1,get_interface_proplist/1,get_interface_proplist/0,discover_node_interfaces/1,find_alias_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

-record(network_interfaces,{node,interface,address,alias}).

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
					  #network_interfaces{node=node(),interface=list_to_atom(Iface),
										  address=ip_list_to_tuple(get_interface_addr(Iface)),
										  alias=is_alias_interface(Iface)}
			  end,Interfaces).

is_alias_interface(Iface) ->
	case string:str(Iface,":") of
		0 ->
			false;
		_ ->
			true
	end.

discover_node_interfaces(Node) when Node == node() ->
	get_interface_proplist();
discover_node_interfaces(Node) ->
	try
		rpc:call(Node,?MODULE,get_interface_proplist,[])
	catch
		_:Err ->
			?ERROR_MSG("Rpc error when discovering node interfaces.  ~p~n",[Err]),
			[]
	end.

find_alias_node(Address) ->
	Interfaces = 
		lists:flatten(
		  lists:map(fun(N) ->
							try
								discover_node_interfaces(N)
							catch
								_:Err ->
									?ERROR_MSG("Error discovering interfaces on node: ~p~n~p~n",[N,Err]),
									[]
							end
					end,[node()|nodes()])),
	?DEBUG_MSG("Interfaces: ~p~n",[Interfaces]),
	lists:filter(fun(IntInfo) ->
						 case IntInfo of
							 #network_interfaces{address=Address} ->
								 true;
							 _ -> false
						 end end,Interfaces).
	

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

