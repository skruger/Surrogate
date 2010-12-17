
-module(surrogate_api).

%%
%% Include files
%%
-include("surrogate.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%%
%% Exported Functions
%%
-export([call/3,xmlcmd/3]).

%%
%% API Functions
%%

call(Sess,Env,Input) ->
	Form = "<form method='POST' action='/rpc/surrogate_api/xmlcmd' target='test'><input type='text' name='test'/><input type='submit'/></form>",
	Info = io_lib:format("Env: ~p~nInput: ~p~n",[Env,Input]),
	mod_esi:deliver(Sess,lists:flatten(["Content-type: text/html\r\n\r\n<pre>",Info,"</pre>",Form])).

xmlcmd(Sess,Env,Input) ->
	%% Use Env to access headers.
	%% Use Input to get post data or extra path elements and get variables.
	try
		case xmerl_scan:string(Input) of
			{Parse,_} ->
				?DEBUG_MSG("Got command: ~p~n",[Parse]),
				AuthInfo = get_auth(find_element(auth,Parse#xmlElement.content,[])),
				{command,CmdName,CmdArg} = E = get_command(find_element(command,Parse#xmlElement.content,[])),
				CmdRet = surrogate_api_cmd:exec(CmdName,CmdArg),
				Info = io_lib:format("Env: ~p~nInput: ~p~n~p~n~p~n~p~n",[Env,Input,AuthInfo,E,CmdRet]),
				mod_esi:deliver(Sess,lists:flatten(["Content-type: text/plain\r\n\r\n",Info]))
		end
	catch
		_:Err ->
			Info2 = io_lib:format("Env: ~p~nInput: ~p~n~nError: ~p~n~p~n",[Env,Input,Err,erlang:get_stacktrace()]),
			mod_esi:deliver(Sess,lists:flatten(["Content-type: text/html\r\n\r\n<pre>",Info2,"</pre>"]))
	end.


%%
%% Local Functions
%%

get_auth([AElem|_]) ->
	get_auth(AElem);
get_auth(AElem) ->
	{auth,
	 find_attribute(name,AElem#xmlElement.attributes),
	 find_attribute(pass,AElem#xmlElement.attributes)}.

get_command([C|_]) ->
	{command,
	 find_attribute(name,C#xmlElement.attributes),
	 get_args(C#xmlElement.content,[])}.

get_args([],Acc) -> lists:reverse(Acc);
get_args([A|R],Acc) ->
	case A of
		#xmlElement{name=arg,content=[#xmlText{value=Arg}|_]} when is_list(Arg) ->
			get_args(R,[Arg|Acc]);
		_ ->
			get_args(R,Acc)
	end.

find_attribute(_Name,[]) -> {error,notfound};
find_attribute(Name,[A|R]) ->
	case A of
		#xmlAttribute{name=Name,value=Value} ->
			Value;
		_ ->
			find_attribute(Name,R)
	end.

find_element(_Elem,[],Acc) -> lists:reverse(Acc);
find_element(Elem,[E|R],Acc) ->
	case E of
		#xmlElement{name=Elem} ->
			find_element(Elem,R,[E|Acc]);
		_ ->
			find_element(Elem,R,Acc)
	end.
