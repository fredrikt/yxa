%%%-------------------------------------------------------------------
%%% File    : yxa_ctl.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Yxa command line control module.
%%%
%%% Created : 28 May 2005 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(yxa_ctl).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/0]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 status/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(EXIT_OK, 0).
-define(EXIT_ERROR, 1).
-define(EXIT_NODEDOWN, 2).
-define(EXIT_USAGE, 3).

%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: start()
%% Descrip.: Fetch the command line arguments and start processing.
%% Returns : does not return, does erlang:halt().
%%--------------------------------------------------------------------
start() ->
    case init:get_plain_arguments() of
	[NodeStr | Args] ->
	    Node = list_to_atom(NodeStr),
	    try process(Node, Args) of
		ok ->
		    erlang:halt(?EXIT_OK);
		error ->
		    erlang:halt(?EXIT_ERROR);
		{badrpc, nodedown} ->
		    io:format("Error: Node ~p not responding~n", [Node]),
		    erlang:halt(?EXIT_NODEDOWN);
		Unknown ->
		    io:format("Yxa_ctl RPC returned unknown result : ~p~n", [Unknown]),
		    erlang:halt(?EXIT_ERROR)
	    catch
		error: Y ->
		    ST = erlang:get_stacktrace(),
		    io:format("Yxa_ctl failed : error ~p ~p~n", [Y, ST]),
		    erlang:halt(?EXIT_ERROR);
		X: Y ->
		    io:format("Yxa_ctl failed : ~p ~p~n", [X, Y]),
		    erlang:halt(?EXIT_ERROR)
	    end;
	_ ->
	    io:format("Invalid arguments~n"),
	    erlang:halt(?EXIT_USAGE)
    end.

%%--------------------------------------------------------------------
%% Function: process(Node, [Action])
%%           Node   = atom()
%%           Action = string(), "status" | "stop" | "restart" |
%%				"reload"
%% Descrip.: Do something to Node.
%% Returns : ok | result of rpc:call(...)
%%--------------------------------------------------------------------
process(Node, ["status"]) ->
    case rpc:call(Node, yxa_ctl, status, []) of
	{ok, Starttime} when is_integer(Starttime) ->
	    io:format("Node ~p status :~n", [Node]),
	    {ok, Output} = format_status(Starttime),
	    io:put_chars(Output),
	    ok;
	Res ->
	    Res
    end;
process(Node, ["stop"]) ->
    case rpc:call(Node, sipserver, stop, []) of
	ok ->
	    io:format("Node ~p stopped~n", [Node]),
	    ok;
	Res ->
	    Res
    end;
%%process(Node, ["info"]) ->
process(Node, ["reload"]) ->
    case rpc:call(Node, yxa_config, reload, []) of
	ok ->
	    io:format("Node ~p configuration reloaded~n", [Node]),
	    ok;
	{error, Where, E} when is_atom(Where), is_list(E) ->
	    io:format("Failed reloading configuration on node ~p.~n"
		      "Phase : ~p~n"
		      "Error : ~s~n"
		      "~n", [Node, Where, E]),
	    error;
	Res ->
	    Res
    end;
process(Node, ["restart"]) ->
    case rpc:call(Node, sipserver, restart, []) of
	ok ->
	    io:format("Node ~p restarted~n", [Node]),
	    ok;
	Res ->
	    Res
    end;

process(_Node, [Cmd]) ->
    io:format("Invalid command ~p~n", [Cmd]),
    error.


%%====================================================================
%% Internal exported functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: status()
%% Descrip.: Check status of running SIP server
%% Returns : {ok, Output}
%%           Output = string()
%%--------------------------------------------------------------------
status() ->
    [{starttime, Starttime}] = ets:lookup(yxa_statistics, starttime),
    {ok, Starttime}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: format_status()
%% Descrip.: Given the start-time returned from status(), construct
%%           information about start-time and current uptime and
%%           format it for printing.
%% Returns : {ok, Output}
%%           Output = string()
%%--------------------------------------------------------------------
format_status(Starttime) ->    
    %% get running time in days, hours, minutes, seconds
    Daystime = calendar:seconds_to_daystime(util:timestamp() - Starttime),

    Output = "Started : " ++ util:sec_to_date(Starttime) ++ "\n"
	"Uptime  : " ++ fmt_daystime(Daystime) ++ "\n",

    {ok, lists:flatten(Output)}.

fmt_daystime({0, {H, M, S}}) ->
    io_lib:format("~p hour(s), ~p minute(s), ~p second(s)", [H, M, S]);
fmt_daystime({D, {H, M, S}}) ->
    io_lib:format("~p day(s), ~s", [D, fmt_daystime({0, {H, M, S}})]).

