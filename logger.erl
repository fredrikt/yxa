-module(logger).
-export([start/0, start/1, recvloop/3, log/2, log/3]).

start() ->
    ApplicationName = case application:get_application() of
	{ok, Name} ->
	    Name;
        _ ->
	    erlang:fault("Application name undefined")
    end,
    start(sipserver:get_env(logger_logbasename, ApplicationName)).

start(Basename) ->
    {ok, DebugIoDevice} = file:open(lists:concat([Basename, ".debug"]), [append]),
    {ok, NormalIoDevice} = file:open(lists:concat([Basename, ".log"]), [append]),
    {ok, ErrorIoDevice} = file:open(lists:concat([Basename, ".error"]), [append]),
    Pid = spawn(logger, recvloop, [DebugIoDevice, NormalIoDevice, ErrorIoDevice]),
    register(logger, Pid).

recvloop(DebugIoDevice, NormalIoDevice, ErrorIoDevice) ->
    receive
	{log, Level, Format, Arguments, Pid} ->
	    DebugLevel = debug_level(debug),
	    NormalLevel = debug_level(normal),
	    ErrorLevel = debug_level(error),
	    NumLevel = debug_level(Level),
	    if 
		NumLevel >= ErrorLevel ->
		    log_to_stdout(Level, Format, Arguments, Pid),
		    log_to_device(ErrorIoDevice, Level, Format, Arguments, Pid);
		true -> none
	    end,
	    if
		NumLevel >= NormalLevel ->
		    log_to_device(NormalIoDevice, Level, Format, Arguments, Pid),
		    log_to_stdout(Level, Format, Arguments, Pid);
		true -> none
	    end,
	    if
		NumLevel >= DebugLevel ->
		    log_to_device(DebugIoDevice, Level, Format, Arguments, Pid);
		true -> none
	    end,
	    recvloop(DebugIoDevice, NormalIoDevice, ErrorIoDevice)
    end.

debug_level(debug) -> 10;
debug_level(normal) -> 20;
debug_level(error) -> 30.

do_log(Level, Format, Arguments) ->
    logger ! {log, Level, Format, Arguments, self()},
    ok.

log(Level, Format) ->
    log(Level, Format, []).

log(Level, Format, Arguments) ->
    case catch do_log(Level, Format, Arguments) of
	ok ->
	    ok;
	Foo ->
	    io:format("cannot log ~p:~n", [Format]),
	    io:format(Format, Arguments),
	    io:format("log error: ~p~n", [Foo]),
	    error
    end.

log_to_device(IoDevice, Level, Format, Arguments, Pid) ->
    io:format(IoDevice, "~s ", [util:sec_to_date(util:timestamp())]),
    io:format(IoDevice, "~p~p:", [Level, Pid]),
    io:format(IoDevice, Format, Arguments),
    io:format(IoDevice, "~n", []).

log_to_stdout(Level, Format, Arguments, Pid) ->
    io:format("~s ~p ", [util:sec_to_date(util:timestamp()), Pid]),
    io:format(Format, Arguments),
    io:format("~n", []).
