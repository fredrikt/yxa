-module(logger).
-export([start/1, recvloop/1, log/2, log/3]).

start(Filename) ->
    {ok, IoDevice} = file:open(Filename, [append]),
    Pid = spawn(logger, recvloop, [IoDevice]),
    register(logger, Pid).

recvloop(IoDevice) ->
    receive
	{log, Level, Format, Arguments, Pid} ->
	    case Level of
		debug ->
		    io:format(IoDevice, "~s ",
                    [util:sec_to_date(util:timestamp())]),
		    io:format(IoDevice, "~p~p:", [Level, Pid]),
		    io:format(IoDevice, Format, Arguments),
		    io:format(IoDevice, "~n", []);
		normal ->
		    io:format("~s ",
                    [util:sec_to_date(util:timestamp())]),
		    io:format(Format, Arguments),
		    io:format("~n", []),
		    io:format(IoDevice, "~s ",
                    [util:sec_to_date(util:timestamp())]),
		    io:format(IoDevice, "~p~p:", [Level, Pid]),
		    io:format(IoDevice, Format, Arguments),
		    io:format(IoDevice, "~n", [])
	    end,
	    recvloop(IoDevice)
    end.

log(Level, Format) ->
    log(Level, Format, []).

log(Level, Format, Arguments) ->
    logger ! {log, Level, Format, Arguments, self()}.
