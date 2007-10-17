%%%-------------------------------------------------------------------
%%% File    : yxa_ctl.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      YXA command line control module.
%%%
%%% @since    28 May 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
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
	 status/0,
	 info/1
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").

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
%% @spec    () -> term() "does not return - does erlang:halt()."
%%
%% @doc     Fetch the command line arguments and start processing.
%% @end
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
		    io:format("Error: Node ~p not responding,~n"
			      "       verify that you have the same Erlang cookie and SSL dist settings~n",
			      [Node]),
		    erlang:halt(?EXIT_NODEDOWN);
		Unknown ->
		    io:format("YXA_ctl RPC returned unknown result : ~p~n", [Unknown]),
		    erlang:halt(?EXIT_ERROR)
	    catch
		error: Y ->
		    ST = erlang:get_stacktrace(),
		    io:format("YXA_ctl failed : error ~p ~p~n", [Y, ST]),
		    erlang:halt(?EXIT_ERROR);
		X: Y ->
		    io:format("YXA_ctl failed : ~p ~p~n", [X, Y]),
		    erlang:halt(?EXIT_ERROR)
	    end;
	_ ->
	    io:format("Invalid arguments~n"),
	    erlang:halt(?EXIT_USAGE)
    end.

%%--------------------------------------------------------------------
%% @spec    (Node, [Action]) -> ok | term() "result of rpc:call(...)"
%%
%%            Node   = atom()
%%            Action = string() "\"status\" | \"stop\" | \"restart\" | \"reload\""
%%
%% @doc     Do something to Node.
%% @end
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
process(Node, ["info" | Args]) ->
    case rpc:call(Node, yxa_ctl, info, [Args]) of
	{ok, Info} ->
	    io:format("Node ~p info :~n~n", [Node]),
	    {ok, Output} = format_info(Info),
	    io:put_chars(Output),
	    ok;
	Res ->
	    Res
    end;
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
%% @spec    () ->
%%            {ok, Output}
%%
%%            Output = string()
%%
%% @doc     Check status of running SIP server
%% @end
%%--------------------------------------------------------------------
status() ->
    [{starttime, Starttime}] = ets:lookup(yxa_statistics, starttime),
    {ok, Starttime}.


%%--------------------------------------------------------------------
%% @spec    (Flags) ->
%%            {ok, Output}
%%
%%            Flags = [string()] "\"all\""
%%
%%            Output = [{Topic, Indent, Width, [{Key, Value}]}]
%%
%% @doc     Check status of running SIP server
%% @end
%%--------------------------------------------------------------------
info([]) ->
    [{starttime, Starttime1}] = ets:lookup(yxa_statistics, starttime),
    Starttime = util:sec_to_date(Starttime1),
    GeneralInfo =
	[{"General information :", 2, 15,
	  [{"Version",		version:get_version()},
	   {"Long version",	version:get_long_version()},
	   {"Starttime",	Starttime}
	  ]}
	 ],
    {ok, GeneralInfo};

info(["all"]) ->
    {ok, GeneralInfo} = info([]),

    {ok,
     GeneralInfo ++ [blank] ++
     info_transport() ++
     info_tcp_connections() ++
     info_transactions()
    }.

info_transport() ->
    %% Transport layer information
    YXASipsocketInfo = lists:sort(ets:tab2list(yxa_sipsocket_info)),
    ListenInfo = lists:map(fun({_Pid, H}) when is_record(H, yxa_sipsocket_info_e) ->
				   Val = lists:concat([H#yxa_sipsocket_info_e.proto, ":",
						       H#yxa_sipsocket_info_e.addr, ":",
						       H#yxa_sipsocket_info_e.port]),
				   {"Listening on", Val}
			   end, YXASipsocketInfo),

    [{"Transportlayer information :", 2, 15,
      ListenInfo},
     blank
    ].

info_tcp_connections() ->
    {ok, Connections} = tcp_dispatcher:get_socketlist(),
    MonitorFmtd1 = socketlist:monitor_format(Connections),
    %% remove listening-on lines
    MonitorFmtd2 =
	lists:foldl(fun("Listening" ++ _, Acc) ->
			    Acc;
		       (H, Acc) ->
			    [H | Acc]
		    end, [], MonitorFmtd1),
    MonitorFmtd = lists:reverse(MonitorFmtd2),
    [{"Cached TCP connections :", 2, 15,
      case length(MonitorFmtd) of
	   0 -> [{"Connections", "none at the moment"}];
	   N -> [{"Connections", integer_to_list(N)}] ++
		    lists:map(fun(H) ->
				      {"    " ++ H}
			      end, MonitorFmtd)
		end
     },
     blank
    ].

info_transactions() ->
    Transactions = transactionstatelist:get_all_entries(),
    T_Info = transactionstatelist:monitor_format(Transactions),

    [{"Transcationlayer information :", 2, 15,
      case length(T_Info) of
	  0 -> [{"Transactions", "none at the moment"}];
	  N -> [{"Transcations", integer_to_list(N)}] ++
		   lists:map(fun(H) ->
				     {"    " ++ H}
			     end, T_Info)
      end
     },
     blank
    ].


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Starttime) ->
%%            {ok, Output}
%%
%%            Starttime = integer() "absolute timestamp"
%%
%%            Output = string()
%%
%% @doc     Given the start-time returned from status(), construct
%%          information about start-time and current uptime and
%%          format it for printing.
%% @end
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


format_info(Args) ->
    format_info(Args, []).

format_info([{Subject, Indent, Width, Elems} | T], Res) when is_list(Subject), is_integer(Indent),
							     is_integer(Width), is_list(Elems) ->
    Fmt = lists:concat(["~", Indent + Width, ".",
			"", Width, "s"
			" : "
			"~s~n"
		       ]),
    Body = lists:map(fun({LHS, RHS}) ->
			     io_lib:format(Fmt, [LHS, RHS]);
			({Single}) when is_list(Single) ->
			     Single ++ "\n"
		     end, Elems),
    This = lists:concat([Subject, "\n", Body]),
    format_info(T, [This | Res]);
format_info([blank | T], Res) ->
    format_info(T, ["\n" | Res]);
format_info([], Res) ->
    {ok, lists:reverse(Res)}.
