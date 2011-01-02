
-module(surrogate_api).
 
%%
%% Include files
%%
-include("surrogate.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%%
%% Exported Functions
%%
-export([call/3,xmlcmd/3,json/3,json2proplist/1]).

%%
%% API Functions
%%

call(Sess,Env,Input) ->
	Form = "<form method='POST' ><input type='text' name='test'/><input type='submit'/></form>",
	Info = io_lib:format("Env: ~p~nInput: ~p~n",[Env,Input]),
	mod_esi:deliver(Sess,lists:flatten(["Content-type: text/html\r\n\r\n<pre>",Info,"</pre>",Form])).

xmlcmd(Sess,Env,Input) ->
	%% Use Env to access headers.
	%% Use Input to get post data or extra path elements and get variables.
	try
		case xmerl_scan:string(Input) of
			{Parse,_} ->
				?DEBUG_MSG("Got command: ~p~n",[Parse]),
				AuthInfo = xget_auth(xfind_element(auth,Parse#xmlElement.content,[])),
				{command,CmdName,CmdArg} = E = xget_command(xfind_element(command,Parse#xmlElement.content,[])),
				CmdRet = surrogate_api_cmd:exec(CmdName,CmdArg),
				Info = io_lib:format("Env: ~p~nInput: ~p~n~p~n~p~n~p~n",[Env,Input,AuthInfo,E,CmdRet]),
				?DEBUG_MSG("Info: ~p~n",Info),
				mod_esi:deliver(Sess,lists:flatten(["Content-type: text/plain\r\n\r\n",CmdRet]))
		end
	catch
		_:Err ->
			Info2 = io_lib:format("Env: ~p~nInput: ~p~n~nError: ~p~n~p~n",[Env,Input,Err,erlang:get_stacktrace()]),
			mod_esi:deliver(Sess,lists:flatten(["Content-type: text/html\r\n\r\n<pre>",Info2,"</pre>"]))
	end.


json(Sess,Env,Input) ->
	%% Use Env to access headers.
	%% Use Input to get post data or extra path elements and get variables.
	try
		case mochijson2:decode(Input) of
			Parse ->
				ReqInfo = method_auth_info(Parse,[]),
				case proplists:get_value(command,ReqInfo,false) of
					Command ->
						CmdRet = 
							case surrogate_api_cmd:exec(Command,Input) of
								R when is_list(R)->
									R;
								R when is_binary(R) ->
									binary_to_list(R);
								{error,_} = CmdErr ->
									Msg = lists:flatten(io_lib:format("surrogate_api_cmd:exec() error: ~p",[CmdErr])),
									iolist_to_binary(mochijson2:encode({struct,[{status,<<"error">>},{message,list_to_binary(Msg)}]}));
								R ->
									io_lib:format("~p",[R])
							end,
						?DEBUG_MSG("Env: ~p~nInput: ~p~n~p~n~p~n~p~n",[Env,Input,Parse,CmdRet,json2proplist(Parse)]),
						mod_esi:deliver(Sess,lists:flatten(["Content-type: text/plain\r\n\r\n",CmdRet]))
				end
		end
	catch
		_:Err ->
			Info2 = io_lib:format("Error~nEnv: ~p~nInput: ~p~n~nError: ~p~n~p~n",[Env,Input,Err,erlang:get_stacktrace()]),
			mod_esi:deliver(Sess,lists:flatten(["Content-type: text/html\r\n\r\n<pre>",Info2,"</pre>"]))
	end.

%%
%% Local Functions
%%

method_auth_info({struct,[]},Props) -> Props;
method_auth_info({struct,[A|R]},Props) ->
	case A of
		{<<"execmodule">>,BCmd} ->
			Cmd = binary_to_list(BCmd),
			?DEBUG_MSG("Got command: ~p~n",[Cmd]),
			method_auth_info({struct,R},[{command,Cmd}| Props]);
		{<<"auth">>,AuthStruct} ->
			method_auth_info({struct,R},parse_json_auth(AuthStruct,[]) ++ Props);
		_O ->
%% 			?DEBUG_MSG("Unknown result: ~p~n",[O]),
			method_auth_info({struct,R}, Props)
	end.
	
parse_json_auth({struct,[]},Props) -> Props;
parse_json_auth({struct,[A|R]},Props) ->
	case A of
		{<<"username">>,BUser} ->
			parse_json_auth({struct,R},[{username,binary_to_list(BUser)}|Props]);
		{<<"password">>,BPass} ->
			parse_json_auth({struct,R},[{password,binary_to_list(BPass)}|Props]);
		_O ->
%% 			?DEBUG_MSG("Unknown auth field: ~p~n",[O]),
			parse_json_auth({struct,R},Props)
	end.

json2proplist(A) when is_list(A) -> json2proplist(mochijson2:decode(A),[]);
json2proplist(A) -> json2proplist(A,[]).
json2proplist({struct,[]},Props) -> Props;
json2proplist({struct,[A|R]},Props) ->
	case A of 
		{BKey,Value} when is_binary(Value) ->
			Key = list_to_atom(binary_to_list(BKey)),
			json2proplist({struct,R},[{Key,binary_to_list(Value)}|Props]);
		{BKey,{struct,_} = Value} ->
			Key = list_to_atom(binary_to_list(BKey)),
			Value2 = {Key,json2proplist(Value)},
			json2proplist({struct,R},[Value2|Props]);
		{BKey,Value} when is_list(Value) ->
			Key = list_to_atom(binary_to_list(BKey)),
			Value2 = {Key,lists:map(fun(X) -> 
											case X of 
												X when is_binary(X) -> binary_to_list(X);
												{NBKey,{struct,_}=NVal} ->
													NKey = list_to_atom(binary_to_list(NBKey)),
													{NKey,json2proplist(NVal)};
												{NBKey,NVal} when is_binary(NVal) ->
													NKey = list_to_atom(binary_to_list(NBKey)),
													{NKey,binary_to_list(NVal)}
											end end, Value)},
			json2proplist({struct,R},[Value2|Props]);
		{_,_} ->
			?DEBUG_MSG("Unexpected type in json2proplist: ~p~n",[A]),
			json2proplist({struct,R},Props)
	end.
		
xget_auth([AElem|_]) ->
	xget_auth(AElem);
xget_auth(AElem) ->
	{auth,
	 xfind_attribute(name,AElem#xmlElement.attributes),
	 xfind_attribute(pass,AElem#xmlElement.attributes)}.

xget_command([C|_]) ->
	{command,
	 xfind_attribute(name,C#xmlElement.attributes),
	 xget_args(C#xmlElement.content,[])}.

xget_args([],Acc) -> lists:reverse(Acc);
xget_args([A|R],Acc) ->
	case A of
		#xmlElement{name=arg,content=[#xmlText{value=Arg}|_]} when is_list(Arg) ->
			xget_args(R,[Arg|Acc]);
		_ ->
			xget_args(R,Acc)
	end.

xfind_attribute(_Name,[]) -> {error,notfound};
xfind_attribute(Name,[A|R]) ->
	case A of
		#xmlAttribute{name=Name,value=Value} ->
			Value;
		_ ->
			xfind_attribute(Name,R)
	end.

xfind_element(_Elem,[],Acc) -> lists:reverse(Acc);
xfind_element(Elem,[E|R],Acc) ->
	case E of
		#xmlElement{name=Elem} ->
			xfind_element(Elem,R,[E|Acc]);
		_ ->
			xfind_element(Elem,R,Acc)
	end.
