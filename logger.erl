-module(logger).
-export([start/0, start/1, recvloop/3, log/2, log/3, quit/1]).

start() ->
    ApplicationName = case application:get_application() of
	{ok, Name} ->
	    Name;
        _ ->
	    erlang:fault("Application name undefined")
    end,
    start(sipserver:get_env(logger_logbasename, ApplicationName)).

start(Basename) ->
    DebugIoDevice = safe_open(lists:concat([Basename, ".debug"]), [append]),
    NormalIoDevice = safe_open(lists:concat([Basename, ".log"]), [append]),
    ErrorIoDevice = safe_open(lists:concat([Basename, ".error"]), [append]),
    Pid = spawn(logger, recvloop, [DebugIoDevice, NormalIoDevice, ErrorIoDevice]),
    register(logger, Pid).

safe_open(Filename, Args) ->
    case file:open(Filename, Args) of
	{ok, FD} ->
	    FD;
	{error, E} ->
	    erlang:fault("Error opening logfile", [Filename, Args])
    end.

recvloop(DebugIoDevice, NormalIoDevice, ErrorIoDevice) ->
    receive
	{log, Level, TimeStamp, Format, Arguments, Pid} ->
	    DebugLevel = debug_level(debug),
	    NormalLevel = debug_level(normal),
	    ErrorLevel = debug_level(error),
	    NumLevel = debug_level(Level),
	    if 
		NumLevel >= ErrorLevel ->
		    log_to_stdout(Level, TimeStamp, Format, Arguments, Pid),
		    log_to_device(ErrorIoDevice, Level, TimeStamp, Format, Arguments, Pid);
		true -> none
	    end,
	    if
		NumLevel >= NormalLevel ->
		    log_to_device(NormalIoDevice, Level, TimeStamp, Format, Arguments, Pid),
		    % Make sure we don't log errors twice to stdout
		    if
			NumLevel =< NormalLevel ->
			    log_to_stdout(Level, TimeStamp, Format, Arguments, Pid);
			true -> none
		    end;
		true -> none
	    end,
	    if
		NumLevel >= DebugLevel ->
		    log_to_device(DebugIoDevice, Level, TimeStamp, Format, Arguments, Pid);
		true -> none
	    end,
	    recvloop(DebugIoDevice, NormalIoDevice, ErrorIoDevice);
	{quit, Pid} ->
	     file:close(DebugIoDevice),
	     file:close(NormalIoDevice),
	     file:close(ErrorIoDevice),
	     Pid ! {logger_quit}
    end.

debug_level(debug) -> 10;
debug_level(normal) -> 20;
debug_level(error) -> 30.

do_log(Level, Format, Arguments) when atom(Level), list(Format), list(Arguments) ->
    {Megasec, Sec, USec} = now(),
    USecStr = string:substr(integer_to_list(USec), 1, 3),
    DateTime = util:sec_to_date(Megasec * 1000000 + Sec),
    logger ! {log, Level, lists:concat([DateTime, ".", USecStr]), Format, Arguments, self()},
    ok.

log(Level, Format) when atom(Level), list(Format) ->
    log(Level, Format, []);
log(Foo, Bar) ->
    log(error, "PROGRAMMING ERROR: Incorrect use of logger:log(), arguments :~n~p ~p", [Foo, Bar]).

log(Level, Format, Arguments) when atom(Level), list(Format), list(Arguments) ->
    case catch do_log(Level, Format, Arguments) of
	ok ->
	    ok;
	{'EXIT', E} ->
	    io:format("cannot log ~p:~n", [Format]),
	    io:format("log error: ~p~n", [E]),
	    ok
    end.

log_to_device(IoDevice, Level, TimeStamp, Format, Arguments, Pid) ->
    io:format(IoDevice, "~s ~p~p:", [TimeStamp, Level, Pid]),
    case catch io:format(IoDevice, Format, Arguments) of
	{'EXIT', E} ->
	    io:format(IoDevice, "LOG FORMATTING ERROR ~p, Format : ~p~n", [E, Format]);
	_ ->
	    true
    end,
    io:format(IoDevice, "~n", []).

log_to_stdout(Level, TimeStamp, Format, Arguments, Pid) ->
    io:format("~s ~p ", [TimeStamp, Pid]),
    case catch io:format(Format, Arguments) of
	{'EXIT', E} ->
	    io:format("LOG FORMATTING ERROR ~p, Format : ~p~n", [E, Format]);
	_ ->
	    true
    end,
    io:format("~n", []).

quit(Msg) ->
    case Msg of
	none -> true;
	[] -> true;
	_ ->
	    log(normal, Msg)
    end,
    logger ! {quit, self()},
    receive
	{logger_quit} ->
	    ok
    after
	1000 ->
	    timeout
    end.
