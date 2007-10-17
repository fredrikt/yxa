%%%-------------------------------------------------------------------
%%% File    : yxa_config_util.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      YXA config subsystem utility functions.
%%%
%%% @since    29 Nov 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(yxa_config_util).

-export([startup_log/4
	]).


%%--------------------------------------------------------------------
%% @spec    (Source, Level, Fmt, Args) -> ok
%%
%%            Source = atom() "typically module name"
%%            Level  = debug | normal | error
%%            Fmt    = string()
%%            Args   = [term()]
%%
%% @doc     Output debug messages to console if OS environment
%%          variable YXA_STARTUP_DEBUG is set. Necessary to debug
%%          errors when YXA applications are starting up, but logging
%%          subsystem isn't running yet.
%% @end
%%--------------------------------------------------------------------
startup_log(Source, Level, Fmt, Args) when is_atom(Source), is_atom(Level), is_list(Fmt), is_list(Args) ->
    case os:getenv("YXA_STARTUP_DEBUG") of
	Env when is_list(Env) ->
	    io:format("~p ~p; ~p; ", [self(), Level, Source]),
	    io:format(Fmt, Args),
	    io:format("~n");
	false ->
	    ok
    end,
    ok.
