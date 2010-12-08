%% Author: skruger
%% Created: Dec 3, 2010
%% Description: TODO: Add description to filter_stream
-module(filter_stream).

%%
%% Include files
%%

-include("surrogate.hrl").

%%
%% Exported Functions
%%
-export([process_hooks/3, init_filter_list/1, behaviour_info/1]).

%%
%% API Functions
%%

behaviour_info(callbacks) ->
	[{start_instance,0},{process_hook,3}];
behaviour_info(_) ->
	undefined.


init_filter_list(Filters) ->
	init_filter_list(Filters,[]).

init_filter_list([],Acc) ->
	Acc;
init_filter_list([F|R],Acc) ->
	case
		try 
%% 			?DEBUG_MSG("Initializing ~p~n",[F]),
			F:start_instance()
		catch
			_:Error ->
				?ERROR_MSG("Error starting instance of ~p: ~p~n",[F,Error]),
				error
		end of
		error ->
			init_filter_list(R,Acc);
		{_,_} = Ref ->
			init_filter_list(R,[Ref|Acc]);
		Err ->
			?ERROR_MSG("Invalid filter reference: ~p~nShould be {modname,ProcRef} where ProcRef is pid() or registered name.",[Err]),
			init_filter_list(R,Acc)
	end.

%% valid hooks:
%% request_header -> Data = #header_block
%% request_body -> Data = binary() -- Does not support changing length of request_body
%% response -> Data = {response_header,#header_block,ResponseSize} | {response_data,binary()} | {end_response_data,ByteLength} | delay
%% ResponseSize = int() | chunked
%% ByteLength = int()
%% if delay is returned proxy_pass must receive the data as {filer_delay,Data}

process_hooks(Hook,Data,Filters) ->
%% 	?DEBUG_MSG("Process hooks: ~p~n",[Filters]),
	process_hooks(Hook,Data,Filters,Data).

process_hooks(_,Data,[],_) ->
	Data;
process_hooks(Hook,Data,[{FMod,FPid}=F|R],OrigData) ->
%% 	?DEBUG_MSG("Process hook: ~p~n",[F]),
	case 
		try
%% 			?DEBUG_MSG("Processing hook: ~p~n",[F]),
			FMod:process_hook(FPid,Hook,Data)
		catch
			_:Error ->
				?ERROR_MSG("Error processing hook ~p: ~p~n",[F,Error]),
				error
		end of
		error ->
			% alternative to error is stopping all processing and returning OrigData
			?DEBUG_MSG("Error? ~p~n",[FMod]),
			process_hooks(Hook,Data,R,OrigData);
		delay -> delay;
		RetData ->
%% 			?DEBUG_MSG("~p Got RetData: ~p~n",[FMod,RetData]),
%% 			case RetData of
%% 				{response_data,_} ->
%%  					ok;
%% 				_ ->
%% 					?DEBUG_MSG("~p Got RetData: ~p~n",[FMod,RetData])
%% 			end,
			process_hooks(Hook,RetData,R,OrigData)
	end.

%%
%% Local Functions
%%

