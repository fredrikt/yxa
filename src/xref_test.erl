%% Code for testing during development. Finds misspelled function
%% names and incorrect arity usage. It requiers that the modules
%% tested, have already been debug compiled - use erlc with the
%% +debug_info flag, e.g. :
%%
%% erlc +debug_info xxxx.erl
%%
%% This code assumes that the current directory (when calling
%% xref_test:run) is the build folder (the directory where the .beam
%% files are).
%% This code has been tested on my (hsten) local machine, with a
%%
%% > cd src
%% > erlc -W +debug_info *.erl
%% > erl
%% 1> xref_test:run().
%%
%% in the source directory, but should also work in the build
%% directory using the yxa Makefile to compile it.
%%--------------------------------------------------------------------

-module(xref_test).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 run/0,
	 run_shell/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     run xref on yxa code, to look for bugs
%% @end
%%--------------------------------------------------------------------
run() ->

    Xref = foobar,

    %% stop any old xref process
    try xref:stop(Xref)
    catch
	throw: _ -> ok;
	  error: _ -> ok;
	  exit: _ -> ok
    end,
    %% start new "empty" xref process
    xref:start(Xref, {xref_mode, functions}),

    %% add path to OTP modules - they should not be detected as unkown modules
    OTP = code:get_path(),
    xref:set_library_path(Xref, OTP, [{verbose, true}]),


    AddOptions = [
		  {builtins, false},
		  {recurse, false},
		  {verbose, true},
		  {warnings, true}
		 ],

    %% tell xref where to look for modules to check
    Res = xref:add_directory(Xref, ".", [{recurse, true}]),
    io:format("add_directory: ~n~p~n", [Res]),



    %% determine which properties to check with xref
    Analysis = [
		undefined_function_calls,
		undefined_functions,
		locals_not_used,

		%% this lists lots of functions - some are exported
		%% behaviour callbacks, others are unused functions intended
		%% for future use (to expose a useful interface to the module)
		%% and some are probably callback functions not related to
		%% behaviours.

		%% exports_not_used,
		deprecated_function_calls,
		deprecated_functions

		%% {deprecated_function_calls, DeprFlag},
		%% {deprecated_functions, DeprFlag},
		%% {call, FuncSpec},
		%% {use, FuncSpec},
		%% {module_call, ModSpec},
		%% {module_use, ModSpec},
		%% {application_call, AppSpec},
		%% {application_use, AppSpec},
		%% {release_call, RelSpec},
		%% {release_use, RelSpec}
	       ],

    %% format analysis results
    Options = [{verbose, true}],
    F = fun(AnalysisKind) ->
		ARes = filter(xref:analyze(Xref, AnalysisKind, Options), AnalysisKind),
		io:format("~n----------------------------------------------------"),
		io:format("~n- ANALYSIS       ~p", [AnalysisKind]),
		io:format("~n----------------------------------------------------"),
		io:format("~n~p~n", [ARes])
	end,
    lists:foreach(F, Analysis).

run_shell() ->
    run(),
    erlang:halt(0).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Res, AnalysisKind) -> [term()]
%%
%%            Res          = term() "return value of xref:analyze"
%%            AnalysisKind = atom() "the xref:analyze kind"
%%
%% @doc     remove certain xref:analyze output that only appears to be
%%          wrong
%% @end
%%--------------------------------------------------------------------
%% filter out the all calls to local:xxxx/yyy
filter(Res, undefined_function_calls) ->
    {ok, L} = Res,
    F = fun(E, Acc) ->
		case E of
		    {_, {local,_,_}} -> Acc;
		    _ -> [E | Acc]
		end
	end,
    lists:reverse(lists:foldl(F, [], L));

%% filter out the all calls to local:xxxx/yyy
filter(Res, undefined_functions) ->
    {ok, L} = Res,
    F = fun(E, Acc) ->
		case E of
		    {local,_,_} -> Acc;
		    _ -> [E | Acc]
		end
	end,
    lists:reverse(lists:foldl(F, [], L));

filter(Res, _AnalysisKind) ->
    Res.
