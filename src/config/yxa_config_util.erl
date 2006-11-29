%%%-------------------------------------------------------------------
%%% File    : yxa_config_util.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: YXA config subsystem utility functions.
%%%
%%% Created : 29 Nov 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(yxa_config_util).

-export([startup_log/4
	]).


%%--------------------------------------------------------------------
%% Function: startup_log(Source, Level, Fmt, Args)
%%           Source = atom(), typically module name
%%           Level  = atom(), debug | normal | error
%%           Fmt    = string()
%%           Args   = list() of term()
%% Descrip.: Output debug messages to console if OS environment
%%           variable YXA_STARTUP_DEBUG is set. Necessary to debug
%%           errors when YXA applications are starting up, but logging
%%           subsystem isn't running yet.
%% Returns : ok
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
