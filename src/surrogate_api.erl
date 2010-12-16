
-module(surrogate_api).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([call/3]).

%%
%% API Functions
%%

call(Sess,Env,Input) ->
	Form = "<form method='POST'><input type='text' name='test'/><input type='submit'/></form>",
	Info = io_lib:format("Env: ~p~nInput: ~p~n",[Env,Input]),
	mod_esi:deliver(Sess,lists:flatten(["Content-type: text/html\r\n\r\n<pre>",Info,"</pre>",Form])).

%%
%% Local Functions
%%

