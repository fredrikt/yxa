%%%-------------------------------------------------------------------
%%% File    : yxa_proc.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      YXA process related utility functions.
%%%
%%% @since    31 Mar 2010 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(yxa_proc).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 safe_spawn/3,

	 safe_is_process_alive/1,
	 safe_signal/3,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
-include("siprecords.hrl").

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
%% @spec    (Module, Function, Arguments) ->
%%            Pid
%%
%%            Module    = atom()
%%            Function  = atom()
%%            Arguments = list()
%%
%%            Pid = pid() "spawned process pid"
%%
%% @doc     Run Module:Function(Arguments), in a separate process. Log
%%          errors, but otherwise just ignore them. Relies on Erlang
%%          process links elsewhere to 'fix' errors.
%% @end
%%--------------------------------------------------------------------
safe_spawn(Module, Function, Arguments) ->
    proc_lib:spawn(fun() -> safe_spawn_child(Module, Function, Arguments) end).

safe_spawn_child(Module, Function, Arguments) ->
    try apply(Module, Function, Arguments) of
	Res -> Res
    catch
	exit: E ->
	    logger:log(error, "=ERROR REPORT==== from ~p:~p :~n~p", [Module, Function, E]),
	    erlang:exit(E);
	throw:
	  {siperror, Status, Reason} ->
	    logger:log(error, "Spawned function ~p:~p generated a SIP-error (ignoring) : ~p ~s",
		       [Module, Function, Status, Reason]),
	    throw({siperror, Status, Reason});
	  {siperror, Status, Reason, _} ->
	    logger:log(error, "Spawned function ~p:~p generated a SIP-error (ignoring) : ~p ~s",
		       [Module, Function, Status, Reason]),
	    throw({siperror, Status, Reason})
    end.

%%--------------------------------------------------------------------
%% @spec    (Process) ->
%%            {Alive, ProcessPid}
%%
%%            Process = pid() | atom()
%%
%%            Alive      = true | false
%%            ProcessPid = pid()
%%
%% @doc     determine if the process Process is running
%% @end
%%--------------------------------------------------------------------
safe_is_process_alive(Pid) when is_pid(Pid) ->
    {is_process_alive(Pid), Pid};
safe_is_process_alive(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
	Pid when is_pid(Pid) ->
	    case is_process_alive(Pid) of
		true ->
		    {true, Pid};
		false ->
		    {false, Pid}
	    end;
	E ->
	    {false, E}
    end.

%%--------------------------------------------------------------------
%% @spec    (LogTag, PidIn, Message) -> ok | error
%%
%%            LogTag  = string() "log prefix when we fail"
%%            PidIn   = pid() | atom()
%%            Message = string()
%%
%% @doc     Check if a process is alive before sending it a signal.
%% @end
%%--------------------------------------------------------------------
safe_signal(LogTag, PidIn, Message) ->
    case yxa_proc:safe_is_process_alive(PidIn) of
	{true, Pid} ->
	    Pid ! Message,
	    ok;
	{false, Pid} when is_list(LogTag) ->
	    logger:log(error, LogTag ++ "Can't send signal ~p to pid '~p' (~p) - not alive or not pid",
		       [Message, PidIn, Pid]),
	    error;
	{false, _} ->
	    error
    end.

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
-ifdef( YXA_NO_UNITTEST ).
test() ->
    {error, "Unit test code disabled at compile time"}.

-else.

test() ->
    %% test safe_is_process_alive(PidOrName)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "safe_is_process_alive/1 - 0"),
    %% get a reference to a dead process
    DeadPid = spawn(fun() -> ok end),

    %% use erlang:monitor() to find out when pid is dead (it might be dead already)
    MonitorRef = erlang:monitor(process, DeadPid),
    receive
	{'DOWN', MonitorRef, process, DeadPid, _Reason} -> ok
    after
	1000 ->
	    throw({error, "test: safe_is_process_alive/1: the process I spawned did not exit!"})
    end,

    autotest:mark(?LINE, "safe_is_process_alive/1 - 1"),
    %% myself as input, should definately be alive
    MyPid = self(),
    {true, MyPid} = safe_is_process_alive(MyPid),

    autotest:mark(?LINE, "safe_is_process_alive/1 - 2"),
    %% atom input (valid) of a process that should really be alive
    {true, LoggerPid} = safe_is_process_alive(logger),
    true = is_pid(LoggerPid),

    autotest:mark(?LINE, "safe_is_process_alive/1 - 5"),
    %% test dead process
    {false, DeadPid} = safe_is_process_alive(DeadPid),

    autotest:mark(?LINE, "safe_is_process_alive/1 - 6"),
    %% test non-existing registered name
    {false, undefined} = safe_is_process_alive(util_autotest_does_not_exist),


    %% test safe_signal(LogTag, PidOrName, Message)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "safe_is_process_alive/1 - 1.1"),
    %% send message to ourselves
    SafeSignalRef = make_ref(),
    ok = safe_signal("foo", self(), {SafeSignalRef, "test"}),

    autotest:mark(?LINE, "safe_is_process_alive/1 - 1.2"),
    %% check that we got the message
    receive
	{SafeSignalRef, "test"} ->
	    ok
    after
	0 ->
	    throw({error, "test: safe_is_process_alive/1: did not get signal I sent to myself"})
    end,

    autotest:mark(?LINE, "safe_is_process_alive/1 - 1.2"),
    %% dead pid
    error = safe_signal("foo", DeadPid, {SafeSignalRef, "test with dead recipient"}),

    autotest:mark(?LINE, "safe_is_process_alive/1 - 1.2"),
    %% for 100% coverage
    error = safe_signal(none, DeadPid, {SafeSignalRef, "test with dead recipient"}),

    ok.

-endif.
