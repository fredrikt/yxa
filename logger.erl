%%%-------------------------------------------------------------------
%%% File    : logger.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Description : Logger is the process that writes our log messages
%%% to disk.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(logger).

%%-compile(export_all).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start_link/1, log/2, log/3, quit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {debug_iodev, debug_fn, normal_iodev, normal_fn, error_iodev, error_fn}).

%% needed for file:read_file_info
-include_lib("kernel/include/file.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    ApplicationName = case application:get_application() of
			  {ok, Name} ->
			      Name;
			  _ ->
			      erlang:fault("Application name undefined")
		      end,
    start_link(sipserver:get_env(logger_logbasename, ApplicationName)).

start_link(AppName) ->
    gen_server:start_link({local, logger}, ?MODULE, [AppName], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Basename]) ->
    %% Check if we should rotate log files when they reach a upper
    %% limit in size (bytes). Defaults to 250 MB.
    case sipserver:get_env(max_logfile_size, 250 * 1024 * 1024) of
	none -> ok;
	Size ->
	    %% Set up a timer to do the size checking every 60 seconds
	    {ok, T} = timer:send_interval(60 * 1000, logger, {check_logfile_size, Size})
    end,
    %% Construct log filenames and open/create them
    DebugFn = lists:concat([Basename, ".debug"]),
    NormalFn = lists:concat([Basename, ".log"]),
    ErrorFn = lists:concat([Basename, ".error"]),
    {ok, DebugIoDevice} = safe_open(DebugFn, [append]),
    {ok, NormalIoDevice} = safe_open(NormalFn, [append]),
    {ok, ErrorIoDevice} = safe_open(ErrorFn, [append]),
    {ok, #state{debug_iodev=DebugIoDevice, debug_fn=DebugFn,
		normal_iodev=NormalIoDevice, normal_fn=NormalFn,
		error_iodev=ErrorIoDevice, error_fn=ErrorFn}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({rotate_logs}, From, State) ->
    %% Create rotation filename suffix from current time
    case create_filename_time_suffix() of
	error ->
	    {reply, {error, "Could not create suitable suffix"}, State};
	Suffix when list(Suffix) ->
	    {Res, NewState} = rotate([debug, normal, error], Suffix, State),
	    {reply, Res, NewState}
    end;

handle_call({rotate_logs, Suffix}, From, State) ->
    {Res, NewState} = rotate([debug, normal, error], Suffix, State),
    {reply, Res, NewState};

handle_call({rotate_logs, Suffix, Logs}, From, State) when list(Logs) ->
    {Res, NewState} = rotate(Logs, Suffix, State),
    {reply, Res, NewState};

handle_call({quit}, From, State) ->
    {stop, normal, {ok}, State};

handle_call(Request, From, State) ->
    {reply, {error, "unknown gen_server call in logger"}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({log, Level, TimeStamp, Format, Arguments, Pid}, State) ->
    DebugLevel = debug_level(debug),
    NormalLevel = debug_level(normal),
    ErrorLevel = debug_level(error),
    NumLevel = debug_level(Level),
    if
	NumLevel >= ErrorLevel ->
	    log_to_stdout(Level, TimeStamp, Format, Arguments, Pid),
	    log_to_device(State#state.error_iodev, Level, TimeStamp, Format, Arguments, Pid);
	true -> none
    end,
    if
	NumLevel >= NormalLevel ->
	    log_to_device(State#state.normal_iodev, Level, TimeStamp, Format, Arguments, Pid),
	    %% Make sure we don't log errors twice to stdout
	    if
		NumLevel =< NormalLevel ->
		    log_to_stdout(Level, TimeStamp, Format, Arguments, Pid);
		true -> none
	    end;
	true -> none
    end,
    if
	NumLevel >= DebugLevel ->
	    log_to_device(State#state.debug_iodev, Level, TimeStamp, Format, Arguments, Pid);
	true -> none
    end,
    {noreply, State};

handle_cast(Unknown, State) ->
    logger:log(error, "Logger: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({check_logfile_size, Size}, State) ->
    %% If not configured not to, we get a check_logfile_size signal
    %% every now and then. Check if any of our logfiles are over the
    %% limit and if so, rotate it.
    case needs_rotating([debug, normal, error], Size, State) of
	[] ->
	    {noreply, State};
	L ->
	    %% Create rotation filename suffix from current time
	    case create_filename_time_suffix() of
	        error ->
		    {noreply, State};
	        Suffix when list(Suffix) ->
		    {_, NewState} = rotate(L, Suffix, State),
		    {noreply, NewState}
	    end
    end;

handle_info(Info, State) ->
    logger:log(error, "Logger: Received unknown signal :~n~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    file:close(State#state.error_iodev),
    file:close(State#state.normal_iodev),
    file:close(State#state.debug_iodev),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

safe_open(Filename, Args) ->
    case file:open(Filename, Args) of
	{ok, FD} ->
	    {ok, FD};
	{error, E} ->
	    erlang:fault("Error opening logfile", [Filename, Args])
    end.

debug_level(debug) -> 10;
debug_level(normal) -> 20;
debug_level(error) -> 30.

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

%%--------------------------------------------------------------------
%% Function: create_filename_time_suffix/0
%% Description: Return a date string (european style) suitable for
%%              appending to a filename when rotating it.
%% Returns: String |
%%          error
%%--------------------------------------------------------------------
create_filename_time_suffix() ->
    {Megasec, Sec, USec} = now(),
    DateTime = util:sec_to_date(Megasec * 1000000 + Sec),
    case string:tokens(DateTime, " ") of
	[Date, Time] ->
	    Suffix = "-" ++ Date ++ "_" ++ Time,
	    Suffix;
	_ ->
	    logger:log(error, "Logger: Failed creating a log file rotation suffix"),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: needs_rotating/2
%% Description: Check a list of {level, Fn} log files to see if any of
%%              them are larger than Size bytes. Return a list of all
%%              'level' atoms whose logfiles exceeds the limit.
%% Returns: []          |
%%          ListOfAtoms
%%--------------------------------------------------------------------
needs_rotating(In, Size, State) when record(State, state) ->
    needs_rotating2(In, Size, State, []).

needs_rotating2([], Size, State, Res) ->
    lists:reverse(Res);
needs_rotating2([H | T], Size, State, Res) when record(State, state) ->
    Fn = level2filename(H, State),
    case file:read_file_info(Fn) of
	{ok, FileInfo} when record(FileInfo, file_info), FileInfo#file_info.size > Size ->
	    %% file_info record returned, and size is larger than Size
	    logger:log(debug, "Logger: Rotating '~p' logfile ~p since it is larger than ~p bytes (~p)",
		       [H, Fn, Size, FileInfo#file_info.size]),
	    needs_rotating2(T, Size, State, [H | Res]);
	_ ->
	    %% Either error returned from read_file_info, or file size is within limits.
	    %% We don't check (care) which one...
	    needs_rotating2(T, Size, State, Res)
    end.

rotate([], Suffix, State) when record(State, state) ->
    {ok, State};
rotate([Level | T], Suffix, State) when record(State, state) ->
    Fn = level2filename(Level, State),
    case rotate_file(Fn, Suffix) of
	{ok, NewIoDev} ->
	    logger:log(debug, "Rotated '~p' logfile with suffix ~p", [Level, Suffix]),
	    rotate(T, Suffix, update_iodev(Level, NewIoDev, State));
	{error, E} ->
	    logger:log(error, "Failed rotating '~p' logfile with suffix ~p : ~p", [Level, Suffix, E]),
	    Str = lists:concat(["Failed rotating '", Level, "' logfile"]),
	    {{error, Str}, State}
    end.

update_iodev(debug, N, State) when record(State, state) ->
    State#state{debug_iodev=N};
update_iodev(normal, N, State) when record(State, state) ->
    State#state{normal_iodev=N};
update_iodev(error, N, State) when record(State, state) ->
    State#state{error_iodev=N}.

level2filename(debug, State) when record(State, state) ->
    State#state.debug_fn;
level2filename(normal, State) when record(State, state) ->
    State#state.normal_fn;
level2filename(error, State) when record(State, state) ->
    State#state.error_fn.

rotate_file(Filename, Suffix) ->
    %% Open new file before we try to rename old one to make sure
    %% there isn't any permission problems etc. with us creating a new
    %% logfile
    %% XXX is there a mktemp() available in Erlang?
    TmpFile = Filename ++ Suffix ++ ".rotate",
    case catch safe_open(TmpFile, [append]) of
        {ok, NewIoDev} ->
	    %% Try to rename the old logfile
	    case file:rename(Filename, Filename ++ Suffix) of
		ok ->
		    %% Rename temporary file to the right filename
		    %% XXX Can we trap errors here?
		    file:rename(TmpFile, Filename),
		    {ok, NewIoDev};
		_ ->
		    file:close(NewIoDev),
		    %% XXX unlink TmpFile or not? It would be a race...
		    error
	    end;
	_ ->
	    error
    end.

%% do_log is executed in caller pid (by log/2 or log/3), not in
%% persistent logger process.
do_log(Level, Format, Arguments) when atom(Level), list(Format), list(Arguments) ->
    {Megasec, Sec, USec} = now(),
    USecStr = string:substr(integer_to_list(USec), 1, 3),
    DateTime = util:sec_to_date(Megasec * 1000000 + Sec),
    LogTS = lists:concat([DateTime, ".", USecStr]),
    gen_server:cast(logger, {log, Level, LogTS, Format, Arguments, self()}),
    ok.


%%--------------------------------------------------------------------
%%% Interface functions
%%--------------------------------------------------------------------

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

quit(Msg) ->
    case Msg of
	none -> true;
	[] -> true;
	_ ->
	    log(normal, Msg)
    end,
    case catch gen_server:cast(logger, {quit}, 1000) of
	{ok} ->
	    ok;
        _ ->
	    {error, "logger error"}
    end.
