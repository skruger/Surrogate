%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Jul 18, 2011
%%% -------------------------------------------------------------------
-module(proxy_protocol_http_admin).

-behaviour(proxy_protocol).
-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([handle_protocol/1]).

%% gen_fsm callbacks
-export([init/1, start_request/2, parse_request/2,send_response/2,handle_request/2,
	 handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([register_module/3,unregister_module/1,get_modules/0]).
-export([handle_root/2]).

-record(state, {listener,client_sock,request,headers,auth,body_length,body}).



%% ====================================================================
%% External functions
%% ====================================================================

handle_protocol(PListener) ->
	CSock = PListener#proxy_listener.client_sock,
	{ok,Pid} = gen_fsm:start(?MODULE,#state{listener=PListener,client_sock=CSock},[]),
	gen_socket:controlling_process(CSock, Pid),
	gen_fsm:send_event(Pid,read),
	ok.

register_module(ModName,Mod,Fun) when is_atom(ModName) ->
%% 	-record(http_admin_modules,{path,module,function}).
	mnesia:dirty_write(#http_admin_module{path=["module",atom_to_list(ModName)],module=Mod,function=Fun});
register_module(Path,Mod,Fun) ->
	mnesia:dirty_write(#http_admin_module{path=Path,module=Mod,function=Fun}).

unregister_module(ModName) when is_atom(ModName) ->
	mnesia:dirty_delete({http_admin_module,["module",atom_to_list(ModName)]});
unregister_module(Path) ->
	mnesia:dirty_delete({http_admin_module,Path}).

get_modules() ->
	Keys = mnesia:dirty_all_keys(http_admin_module),
	Modules = 
	lists:flatten([mnesia:dirty_read(http_admin_module,K) || K <- Keys]),
	PMods = 
	[{length(Path),{Path,{Mod,Fun}}} || #http_admin_module{path=Path,module=Mod,function=Fun} <- Modules],
	{_,Result} = lists:unzip(lists:reverse(lists:sort(PMods))),
	Result.
	

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init(State) ->
    {ok, start_request, State}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
start_request(read, #state{client_sock=CSock}=StateData) ->
	case gen_socket:recv(CSock,0,10000) of
		{ok,Data} ->
			gen_fsm:send_event(self(),{data,Data}),
			{next_state, parse_request, StateData};
		Err ->
			?ERROR_MSG("Error in start_request(): ~p~n",[Err]),
			{stop,normal,StateData}
	end.

parse_request({data,Data},StateData) ->
	case erlang:decode_packet(http,Data,[]) of
		{ok,Packet,Rest} ->
			HttpAdminR = parse_packet(Packet),
			gen_fsm:send_event(self(),{header,Rest}),
			{next_state, parse_request, StateData#state{request=HttpAdminR,headers=[],body_length=0,body= <<>>}};
		Err ->
			?ERROR_MSG("Error in start_request(): ~p~n",[Err]),
			{stop,normal,StateData}
	end;
parse_request({header,Data},#state{request=Req,headers=HeaderList}=StateData) ->
	case erlang:decode_packet(httph,Data,[]) of
		{ok,http_eoh,Rest} ->
			FinalRequest = Req#http_admin{headers=HeaderList},
			gen_fsm:send_event(self(),{body,Rest}),
			{next_state,parse_request,StateData#state{request=FinalRequest}};
		{ok,{http_header,_,'Authorization',_,"Basic "++AuthInfo},Rest} ->
			UserPassBin = base64:decode(AuthInfo),
			[User,Pass|_] = string:tokens(binary_to_list(UserPassBin),":"),
			UserInfo = proxy_auth:check_user(User,Pass),
			HasAuth = case UserInfo of {ok,_} -> true; _ -> false end,
			?ERROR_MSG("Check user: ~p~n",[UserInfo]),
			gen_fsm:send_event(self(),{header,Rest}),
			{next_state, parse_request,StateData#state{request=Req#http_admin{auth={User,Pass},has_auth=HasAuth}}};
		{ok,{http_header,_,'Content-Length',_,StrLen},Rest} ->
			gen_fsm:send_event(self(),{header,Rest}),
			{next_state, parse_request,StateData#state{body_length=list_to_integer(StrLen)}};
		{ok,{http_header,_,Header,_,Value},Rest} ->
%% 			?ERROR_MSG("Got header: ~p ~p~n",[Header,Value]),
			gen_fsm:send_event(self(),{header,Rest}),
			NewHeaders = [{Header,Value}|HeaderList],
			{next_state, parse_request, StateData#state{headers=NewHeaders}};
		Err ->
			?ERROR_MSG("Error in start_request(): ~p~n",[Err]),
			{stop,normal,StateData}
	end;
parse_request({body,_Data},#state{body_length=0}=State) ->
	gen_fsm:send_event(self(),run),
	{next_state,handle_request,State};
parse_request({body,Data},#state{body_length=Length,client_sock=CSock,request=Req0}=State) ->
	case iolist_size(Data) of
		Size when Size < Length ->
			case gen_socket:recv(CSock,0,10000) of
				{ok,Data2} ->
					gen_fsm:send_event(self(),{body,<<Data/binary,Data2/binary>>}),
					{next_state,parse_request,State};
				Err ->
					gen_fsm:send_event(self(),{500,[],io_lib:format("Error reading full request:<br/>~n~p",[Err])}),
					{next_state,send_response,State}
				end;
		_Size ->
			Req = Req0#http_admin{body=Data},
			gen_fsm:send_event(self(),run),
			{next_state,handle_request,State#state{request=Req}}
	end.

handle_request(run,#state{request=Request}=State) ->
	RequestHandlers = proxy_protocol_http_admin:get_modules() ++ 
						  [{[],{?MODULE,handle_root}}],
%% 	?ERROR_MSG("Request: ~p~nHandlers: ~p~n",[Request,RequestHandlers]),
	Result = route_request(RequestHandlers,Request),
	gen_fsm:send_event(self(),Result),
	{next_state,send_response,State}.

send_response({Status,Headers,Body},#state{request=#http_admin{version={V1,V2}}}=StateData) ->
	BodySize = integer_to_list(iolist_size(Body)),
	GHdr = [{'Server',"Surrogate http_admin"},{'Content-Length',BodySize}],
	Response =
	io_lib:format("HTTP/~p.~p ~p ~s~n",[V1,V2,Status,http_status(Status)])++
		[io_lib:format("~s: ~s\r\n",[any_to_list(H),S]) || {H,S} <- Headers++GHdr]++
		["\r\n",Body],
	gen_socket:send(StateData#state.client_sock,Response),
	gen_socket:close(StateData#state.client_sock),
	{stop,normal,StateData}.
	

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

route_request([],_Request) ->
	{404,[],<<"404 Not found.">>};
route_request([{PathPrefix,{Module,Fun}}|R], #http_admin{path=Path}=Request) ->
	case (lists:prefix(PathPrefix,Path) or (PathPrefix == Path)) of
		true ->
			LocalPath = lists:nthtail(length(PathPrefix), Path),
			try
				
				case apply(Module,Fun,[LocalPath,Request]) of
					{Res,Hdr,Data} ->
						{Res,Hdr,iolist_to_binary(Data)};
					Data when is_list(Data) or is_binary(Data) ->
						{200,[],iolist_to_binary(Data)}
				end
			catch
				_:Err ->
					case erlang:get_stacktrace() of
						%% If modules did not have a path handler (matching function clause) return 404 error for it.
						[{Module,Fun,_}|_]  when Err == function_clause ->
							{404,[],<<"Page not found.">>};
						_ ->
							?ERROR_MSG("Error in route_request(): Pid=~p~nError: ~p~nFunction: ~p~nPath: ~p~nStack: ~p~n",
										[self(),Err,{Module,Fun,[LocalPath,Request]},PathPrefix,erlang:get_stacktrace()]),
							{500,[],iolist_to_binary(io_lib:format("Internal server error.  Pid=~p",[self()]))}
					end
			end;
		false ->
			route_request(R,Request)
	end.
			
handle_root(["output"],Request) ->
	{200,[{'Content-Type',"text/html"}],iolist_to_binary(["This is some output<br/>\n",Request#http_admin.body,"\n"])};
handle_root(_,_) ->
	{404,[],<<"404 Not found.">>}.


parse_packet({http_request,Method,{abs_path,PathStr},Ver}=Packet) ->
	?ERROR_MSG("Header: ~p~n",[Packet]),
	{ReqStr,GetArgs} = 
	case string:chr(PathStr,$?) of
		ArgIdx when ArgIdx > 0 ->
			PathStr2 = string:substr(PathStr,1,ArgIdx-1),
			{string:tokens(PathStr2,[$/]),string:substr(PathStr,ArgIdx+1)};
		_ -> 
			{string:tokens(PathStr,[$/]),[]}
	end,
	FormData = parse_formdata(GetArgs),
	?ERROR_MSG("ReqStr: ~p~nGetArgs: ~p~nFormData: ~p~n",[ReqStr,GetArgs,FormData]),
	#http_admin{method=Method,path=ReqStr,version=Ver,args=FormData,body= <<>>}.

parse_formdata(GetArgs) ->
	parse_formdata(string:tokens(GetArgs,[$&]),[]).

parse_formdata([],Acc) ->
	lists:reverse(Acc);
parse_formdata([Arg|R],Acc) ->
	Pair =
	case string:chr(Arg,$=) of
		Idx when Idx > 0 ->
			{string:substr(Arg,1,Idx-1),string:substr(Arg,Idx+1)};
		_ ->
			{Arg,none}
	end,
	?ERROR_MSG("Arg: ~p~n",[Arg]),
	parse_formdata(R,[Pair|Acc]).

any_to_list(Atom) when is_atom(Atom) ->
	atom_to_list(Atom);
any_to_list(Bin) when is_binary(Bin) ->
	binary_to_list(Bin);
any_to_list(List) ->
	List.

http_status(200) -> "OK";
http_status(400) -> "Error";
http_status(500) -> "Internal Server Error";
http_status(Status) -> lists:flatten(io_lib:format("Status ~p not mapped to a string.",[Status])).
