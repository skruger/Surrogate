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
-export([handle_root/3]).

-record(state, {listener,client_sock,request,headers,auth,body_length,body,listen_config}).



%% ====================================================================
%% External functions
%% ====================================================================

handle_protocol(PListener) ->
	CSock = PListener#proxy_listener.client_sock,
	{ok,Pid} = gen_fsm:start(?MODULE,#state{listener=PListener,client_sock=CSock,listen_config=PListener#proxy_listener.proplist},[]),
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
	case catch gen_socket:recv(CSock,0,10000) of
		{ok,Data} ->
			gen_fsm:send_event(self(),{data,Data}),
			{next_state, parse_request, StateData};
		{error,closed} ->
			{stop,normal,StateData};
		Err ->
			?ERROR_MSG("Error in ~p:start_request(): ~p~n",[?MODULE,Err]),
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
			{Auth,HasAuth} = 
			case proxy_auth:check_user(User,Pass) of
				{ok,#proxy_userinfo{username=ProxyUser}=UInfo} ->
					{{User,Pass,UInfo},proxy_acl:get_permission(http_admin,ProxyUser)};
				_ ->
					{{User,Pass,undefined},false}
			end,
%% 			?ERROR_MSG("Check user: ~p~n",[Auth]),
			gen_fsm:send_event(self(),{header,Rest}),
			{next_state, parse_request,StateData#state{request=Req#http_admin{auth=Auth,has_auth=HasAuth}}};
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
			Vars = parse_formdata(binary_to_list(Data)),
			Req = Req0#http_admin{args=Vars,body=Data},
			
			gen_fsm:send_event(self(),run),
			{next_state,handle_request,State#state{request=Req}}
	end.

handle_request(run,#state{request=Request,listen_config=Config}=State) ->
	RequestHandlers = proxy_protocol_http_admin:get_modules() ++ 
						  [{[],{?MODULE,handle_root}}],
%% 	?ERROR_MSG("Request: ~p~nHandlers: ~p~n",[Request,RequestHandlers]),
	Result = route_request(RequestHandlers,Request,Config),
	gen_fsm:send_event(self(),Result),
	{next_state,send_response,State}.

send_response({Status,Headers0,Body},#state{request=#http_admin{version={V1,V2},path=ReqPath}}=StateData) ->
	Headers1 = 
	case has_header("content-type",Headers0) of
		true ->
			Headers0;
		_ ->
			MimeType = get_mime_type(string:join(ReqPath,"/")),
%% 			?ERROR_MSG("Loaded mimetype: ~p~n",[MimeType]),
			Headers0++MimeType
	end,
	Headers =
	case has_header("connection",Headers1) of
		true ->
			Headers1;
		_ ->
			Headers1++[{"Connection","close"}]
	end,
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

route_request([],_Request,_Conf) ->
	{404,[],<<"404 Not found.">>};
route_request([{PathPrefix,{Module,Fun}}|R], #http_admin{path=Path}=Request,Config) ->
	case (lists:prefix(PathPrefix,Path) or (PathPrefix == Path)) of
		true ->
			LocalPath = lists:nthtail(length(PathPrefix), Path),
			try
				
				case apply(Module,Fun,[LocalPath,Request,Config]) of
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
			route_request(R,Request,Config)
	end.
			
%%handle_root() with zip support:
% Take config option for root as [{zip,"file1.zip"},{dir,"/path/to/docroot"},{mnesia,'Mneisa_Table_Name'}]
% Implement favicon through special -define() in surrogate_favicon.hrl
% Setup system for assigning mime type based on file extension?

handle_root(["login"],#http_admin{headers=Hdr,has_auth=Auth},_Config) when Auth == true ->
	Ref = proplists:get_value('Referer',Hdr,"/"),
	{302,[{"Location",Ref}],<<>>};
handle_root(["login"],#http_admin{headers=Hdr},_Config) ->
	Ref = proplists:get_value('Referer',Hdr,"/"),
	Args = iolist_to_binary([Ref]),
	{401,[{"WWW-Authenticate","Basic realm=Surrogate admin"}],Args};
handle_root(["logout"],#http_admin{headers=Hdr},_Config) ->
	Ref = proplists:get_value('Referer',Hdr,"/"),
	HTML = io_lib:format("<html><head><script>window.location = ~p;</script></head><body><a href=~p>~s</a></body></html>",
						 [Ref,Ref,"Back to main page."]),
	{401,[{"Content-Type","text/html"}],iolist_to_binary(HTML)};
handle_root(Path,Request,Config) ->
	case proplists:get_all_values(docroot,Config) of
		[] ->
			{404,[],<<"No docroot configured.">>};
		Roots when is_list(Roots) ->
			case handle_docroot(Roots,Path,Request) of
				{404,_,_} ->
					handle_docroot(Roots,Path++["index.html"],Request);  %% If nothing is found try again with /index.html
				DocRootResponse ->
					DocRootResponse
			end;
		Other ->
			{404,[],iolist_to_binary(io_lib:format("Not found in: ~p~n",[Other]))}
	end.

handle_docroot([],_Path,_Request) ->
	{404,[],<<"404 Not found.">>};
handle_docroot([{dir,DocRoot}|R],ReqPath,Request) ->
	RFile = DocRoot ++ string:join(ReqPath,"/"),
%% 	?ERROR_MSG("File: ~p~n",[filename:extension(RFile)]),
	case file:read_file_info(RFile) of
		{ok,FileInfo} ->
%%  			?ERROR_MSG("Opening file: ~p~n~p~n",[RFile,FileInfo]),
			case file:read_file(RFile) of
				{ok,Binary} ->
					{200,[],Binary};
				{error,eisdir} ->
					handle_docroot(R,ReqPath,Request);
				OpenErr ->
					{500,[],iolist_to_binary(io_lib:format("Error opening file ~p~n",[OpenErr]))}
			end;
		_Other ->
			handle_docroot(R,ReqPath,Request)
	end;
handle_docroot([{zip,ZipFile}|R],ReqPath,Request) ->
	case zip:zip_open(ZipFile,[memory]) of
		{ok,Handle} ->
			File = string:join(ReqPath,"/"),
			case zip:zip_get(File,Handle) of
				{ok,{_File,Binary}} ->
					zip:zip_close(Handle),
					{200,[],Binary};
				{error,file_not_found} ->
					handle_docroot(R,ReqPath,Request);
				GetErr ->
					zip:zip_close(Handle),
					?ERROR_MSG("Error retrieving file from zip:~n~p~n~p~n~p~n",[ZipFile,File,GetErr]),
					handle_docroot(R,ReqPath,Request)
			end;
		OpenErr ->
			?ERROR_MSG("Error opeinging docroot zip file: ~n~p~n~p~n",[ZipFile,OpenErr]),
			handle_docroot(R,ReqPath,Request)
	end;
handle_docroot([{mnesia,Table}|R],ReqPath,Request) ->
	case mnesia:dirty_read(Table,ReqPath) of
		[#surrogate_docroot{filedata=Data}|_] ->
			{200,[],Data};
		_ ->
			handle_docroot(R,ReqPath,Request)
	end;
handle_docroot([DocRoot|R],ReqPath,Request) ->
	?ERROR_MSG("Unknown docroot type: ~p~n",[DocRoot]),
	handle_docroot(R,ReqPath,Request).
	

get_mime_type(FileName) ->
	Ext = filename:extension(FileName),
	get_mime_type2(Ext).

get_mime_type2(".html") -> [{"Content-Type","text/html"}];
get_mime_type2(".conf") -> [{"Content-Type","text/plain"}];
get_mime_type2(".js") -> [{"Content-Type","application/javascript"}];
get_mime_type2(".json") -> [{"Content-Type","application/json"}];
get_mime_type2(".xml") -> [{"Content-Type","application/xml"}];
get_mime_type2(_) ->
	[].
	
parse_packet({http_request,Method,{abs_path,PathStr},Ver}=Packet) ->
%% 	?ERROR_MSG("Header: ~p~n",[Packet]),
	{ReqStr0,GetArgs} = 
	case string:chr(PathStr,$?) of
		ArgIdx when ArgIdx > 0 ->
			PathStr2 = string:substr(PathStr,1,ArgIdx-1),
			{string:tokens(PathStr2,[$/]),string:substr(PathStr,ArgIdx+1)};
		_ -> 
			{string:tokens(PathStr,[$/]),[]}
	end,
	ReqStr = [proxylib:uri_unescape(PathPart) || PathPart <- ReqStr0 ],
	FormData = parse_formdata(GetArgs),
%% 	?ERROR_MSG("ReqStr: ~p~nGetArgs: ~p~nFormData: ~p~n",[ReqStr,GetArgs,FormData]),
	#http_admin{method=Method,path=ReqStr,version=Ver,args=FormData,body= <<>>}.

parse_formdata(GetArgs) ->
	parse_formdata(string:tokens(GetArgs,[$&]),[]).

parse_formdata([],Acc) ->
	lists:reverse(Acc);
parse_formdata([Arg|R],Acc) ->
	Pair =
	case string:chr(Arg,$=) of
		Idx when Idx > 0 ->
			{string:substr(Arg,1,Idx-1),proxylib:uri_unescape(string:substr(Arg,Idx+1))};
		_ ->
			{Arg,none}
	end,
	?ERROR_MSG("Arg: ~p~n",[Arg]),
	parse_formdata(R,[Pair|Acc]).

has_header(HeaderStr,HeaderList0) ->
	HeaderList = [string:to_lower(any_to_list(H)) || {H,_} <- HeaderList0],
	case lists:filter(fun(X) when X == HeaderStr -> true; (_) -> false end,HeaderList) of
		[] -> false;
		_ -> true
	end.

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
