%%%-------------------------------------------------------------------
%%% File    : logger.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Logger is the process that writes our log messages
%%%           to disk (and erlang shell).
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%
%%% Note    : Erlang/OTP has it's own log module - disk_log, that
%%%           among other things is able to do log rotation. People
%%%           having used it is not all happy though.
%%%-------------------------------------------------------------------
-module(logger).

%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 start_link/1,
	 log/2,
	 log/3,
	 log_iolist/2,
	 quit/1,
	 enable/1,
	 disable/1
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
%% needed for file:read_file_info
-include_lib("kernel/include/file.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%% @type state() = #state{}.
%%                 used to keep track of the files, used by the log functions,
%%                 one file type of file is keept for each type of log (debug,
%%                 normal, error)
-record(state, {
	  debug_iodev,		% term(), file descriptor
	  debug_fn,		% string(), file name
	  debug_enabled,	% bool()

	  normal_iodev,		% term(), file descriptor
	  normal_fn,		% string(), file name
	  normal_enabled,	% bool()

	  error_iodev,		% term(), file descriptor
	  error_fn,		% string(), file name
	  error_enabled,	% bool()

	  console_enabled	% bool()
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
%% benchmarking shows no real speed improvement by delayed_write,
%% just annoyance
-define(FILE_OPTS, [append, raw, binary]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     Start the 'logger' server, with a default logbase.
%% @see      start_link/1.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    LogBase =
	case yxa_config:get_env(logger_logbasename) of
	    {ok, LogBase1} -> LogBase1;
	    none ->
		case application:get_application() of
		    {ok, Name} when is_atom(Name) ->
			AppName = atom_to_list(Name),
			case yxa_config:get_env(logger_logdir) of
			    {ok, LogDir} ->
				filename:join(LogDir, AppName);
			    none ->
				AppName
			end;
		    _ ->
			io:format("ERROR: Can't start logger: Application name undefined~n"),
			erlang:error("Application name undefined")
		end
	end,
    start_link(LogBase).

%%--------------------------------------------------------------------
%% @spec    (LogBase) -> term()
%%
%%            LogBase = string() "base name of logfiles to use"
%%
%% @doc     Start the 'logger' server start_link/0 uses the current
%%          application name, together with any configured
%%          logger_logdir as LogBase. The logger is only registered
%%          locally (on the current node)
%% @end
%%--------------------------------------------------------------------
start_link(LogBase) when is_list(LogBase) ->
    gen_server:start_link({local, logger}, ?MODULE, [LogBase], []).


%%--------------------------------------------------------------------
%% @spec    (Level, Format) -> ok
%%
%%            Level  = normal | debug | error
%%            Format = string() "a io:format format string"
%%
%% @equiv    log(Level, Format, [])
%% @end
%%--------------------------------------------------------------------
log(Level, Format) when is_atom(Level), is_list(Format) ->
    log(Level, Format, []).

%%--------------------------------------------------------------------
%% @spec    (Level, Format, Arguments) -> ok
%%
%%            Level     = normal | debug | error
%%            Format    = string() "a io:format format string"
%%            Arguments = list() "a io:format argument list"
%%
%% @doc     Log a log entry.
%% @end
%%--------------------------------------------------------------------
log(Level, Format, Arguments) when is_atom(Level), is_list(Format),
				   is_list(Arguments) ->
    do_log(Level, Format, Arguments).

%%--------------------------------------------------------------------
%% @spec    (Level, IOlist) -> ok
%%
%%            Level  = normal | debug | error
%%            IOlist = io_list()
%%
%% @doc     Log a log entry, without formatting the data in any way
%%          (except adding the timestamp-pid-level prefix).
%% @end
%%--------------------------------------------------------------------
log_iolist(Level, IOlist) when is_atom(Level) ->
    LogTS = get_ts(now()),
    L = atom_to_list(Level),
    P = pid_to_list(self()),
    gen_server:cast(logger, {log, Level, [LogTS, 32, L, P, $:, IOlist]}),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Msg) ->
%%            ok | {error, Reason}
%%
%%            Msg = none | [] | term()
%%
%%            Reason = string()
%%
%% @doc     Terminate the logger process. If Msg is term(), it will be
%%          sent to the log before quitting "log(normal, Msg)".
%% @end
%%--------------------------------------------------------------------
quit(Msg) ->
    case Msg of
	none -> true;
	[] -> true;
	_ ->
	    log(normal, Msg)
    end,
    %% XXX why isn't gen_server:call/2 with default timeout (5000ms) used ?
    case catch gen_server:call(logger, {quit}, 1000) of
	{ok} ->
	    ok;
	%% timeout
        _ ->
	    {error, "logger error"}
    end.

%%--------------------------------------------------------------------
%% @spec    (Which) ->
%%            {ok, Status}    |
%%            {error, Reason}
%%
%%            Which = Id | [Id]
%%            Id    = all | debug | normal | error | console
%%
%%            Status  = [{Output, Enabled}]
%%            Output  = debug | normal | error | console
%%            Enabled = bool()
%%            Reason  = term()
%%
%% @doc     Enables logging on one or more outputs.
%% @end
%%--------------------------------------------------------------------
enable(Status) when is_list(Status) ->
    enable_disable_list(Status, true);
enable(Which) when is_atom(Which) ->
    gen_server:call(logger, {update_logger_status, [{Which, true}]}).

%%--------------------------------------------------------------------
%% @spec    (Which) ->
%%            {ok, Status}    |
%%            {error, Reason}
%%
%%            Which = Id | [Id]
%%            Id    = all | debug | normal | error | console
%%
%%            Status  = [{Output, Enabled}]
%%            Output  = debug | normal | error | console
%%            Enabled = bool()
%%            Reason  = term()
%%
%% @doc     Disables logging on one or more outputs.
%% @end
%%--------------------------------------------------------------------
disable(Status) when is_list(Status) ->
    enable_disable_list(Status, false);
disable(Which) when is_atom(Which) ->
    gen_server:call(logger, {update_logger_status, [{Which, false}]}).

%% part of enable/1 and disable/1
enable_disable_list(Status, Val) ->
    %% turn Status [foo, bar] into [{foo, Val}, {bar, Val}]
    L = [{Which, Val} || Which <- Status, is_atom(Which)],
    case length(L) == length(Status) of
	true ->
	    gen_server:call(logger, {update_logger_status, L});
	false ->
	    {error, non_atom_in_list}
    end.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ([Basename]) -> {ok, State}
%%
%%            Basename = string() "log filename base"
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([Basename]) ->
    %% Set up a timer to check if log files needs rotating every 60 seconds
    {ok, _T} = timer:send_interval(60 * 1000, logger, check_logfile_size),

    %% Construct log filenames and open/create them
    DebugFn = lists:concat([Basename, ".debug"]),
    NormalFn = lists:concat([Basename, ".log"]),
    ErrorFn = lists:concat([Basename, ".error"]),
    {DebugIoDevice, NormalIoDevice, ErrorIoDevice} =
	try
	    begin
		{ok, D_Dev} = safe_open(DebugFn, ?FILE_OPTS),
		{ok, N_Dev} = safe_open(NormalFn, ?FILE_OPTS),
		{ok, E_Dev} = safe_open(ErrorFn, ?FILE_OPTS),
		{D_Dev, N_Dev, E_Dev}
	    end
	catch
	    X:Y ->
		io:format("ERROR: Failed opening logfiles : ~p ~p~n", [X, Y])
	end,

    NewState1 =
	#state{debug_iodev	= DebugIoDevice,
	       debug_fn		= DebugFn,

	       normal_iodev	= NormalIoDevice,
	       normal_fn	= NormalFn,

	       error_iodev	= ErrorIoDevice,
	       error_fn		= ErrorFn
	      },

    {ok, Status} = yxa_config:get_env(logger_status),
    update_logger_status(Status, NewState1).


%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({rotate_logs}, From, State) ->
%%            {reply, Reply, State}
%%
%%            Reply  = ok | {error, Reason}
%%            Reason = string()
%%
%% @doc     Rotate all logs with a default suffix. Note : This code is
%%          not invoked from anywhere. Intended for future use when
%%          rotate_log may be triggered from other sources than the
%%          timer, but can of course be used manually (from the erl
%%          shell) on a running system.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({rotate_logs}, _From, State) ->
    %% Create rotation filename suffix from current time
    Suffix = create_filename_time_suffix(),
    {Res, NewState} = rotate([debug, normal, error], Suffix, State),
    {reply, Res, NewState};

%%--------------------------------------------------------------------
%% @spec    ({rotate_logs, Suffix}, From, State) ->
%%            {reply, Reply, NewState}
%%
%%            Reply    = ok | {error, Reason}
%%            NewState = #state{}
%%
%% @doc     Rotate all logs with a specific Suffix.
%% @see      rotate/3.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({rotate_logs, Suffix}, _From, State) ->
    {Res, NewState} = rotate([debug, normal, error], Suffix, State),
    {reply, Res, NewState};

%%--------------------------------------------------------------------
%% @spec    ({rotate_logs, Suffix, Logs}, From, State) ->
%%            {reply, Reply, NewState}
%%
%%            Reply    = ok | {error, Reason}
%%            NewState = #state{}
%%
%% @doc     Rotate the outputs indicated in Logs with a specific
%%          Suffix.
%% @see      rotate/3.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({rotate_logs, Suffix, Logs}, _From, State) when list(Logs) ->
    {Res, NewState} = rotate(Logs, Suffix, State),
    {reply, Res, NewState};

%%--------------------------------------------------------------------
%% @spec    ({quit}, From, State) -> {stop, normal, {ok}, State}
%%
%% @doc     Stop the logger.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({quit}, _From, State) ->
    {stop, normal, {ok}, State};

%%--------------------------------------------------------------------
%% @spec    ({update_logger_status, Status}, From, State) ->
%%            {reply, Reply, NewState}
%%
%%            Reply    = {ok, Result} | {error, Reason}
%%            Result   = [{Level, Status}]
%%            Level    = debug | normal | error | console
%%            Status   = true | false
%%            Reason   = term()
%%            NewState = #state{}
%%
%% @doc     Enable or disable different outputs.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({update_logger_status, Status}, _From, State) when is_list(Status) ->
    case update_logger_status(Status, State) of
	{ok, NewState} ->
	    Result = [{debug,   NewState#state.debug_enabled},
		      {normal,  NewState#state.normal_enabled},
		      {error,   NewState#state.error_enabled},
		      {console, NewState#state.console_enabled}
		     ],
	    {reply, {ok, Result}, NewState};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;

%%--------------------------------------------------------------------
%% @spec    (Unknown, From, State) ->
%%            {reply, {error, Reason}, State}
%%
%%            Reason = string()
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "Logger: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "unknown gen_server call in logger"}, State}.


%%--------------------------------------------------------------------
%% @spec    handle_cast(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({log, Level, Data}, State) -> {noreply, State}
%%
%%            Level = debug | normal | error
%%            Data  = binary() "what we should write to the log file"
%%
%% @doc     Write a log message to one or more of our log files (and
%%          to stdout if Level /= debug). Having this as a cast()
%%          makes the log/2 and log/3 calls non-blocking.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({log, Level, Data}, State) when is_atom(Level), is_binary(Data); is_list(Data) ->
    case Level of
	error ->
	    log_to_device(State#state.error_enabled, State#state.error_iodev, Data),
	    log_to_device(State#state.normal_enabled, State#state.normal_iodev, Data),
	    log_to_device(State#state.debug_enabled, State#state.debug_iodev, Data),
	    log_to_stdout(State#state.console_enabled, Data);
	normal ->
	    log_to_device(State#state.normal_enabled, State#state.normal_iodev, Data),
	    log_to_device(State#state.debug_enabled, State#state.debug_iodev, Data),
	    log_to_stdout(State#state.console_enabled, Data);
	debug ->
	    log_to_device(State#state.debug_enabled, State#state.debug_iodev, Data)
    end,
    {noreply, State};

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Unknown cast.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "Logger: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @spec    handle_info(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling all non call/cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (check_logfile_size, State) -> {noreply, NewState}
%%
%% @doc     Periodically gets invoked by timer. Check if any of our
%%          logfiles are greater than our configured limit, if so -
%%          call rotate() on them.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(check_logfile_size, State) ->
    %% If not configured not to, we get a check_logfile_size signal
    %% every now and then. Check if any of our logfiles are over the
    %% limit and if so, rotate it.
    case yxa_config:get_env(max_logfile_size) of
	{ok, 0} ->
	    {noreply, State};
	{ok, Size} ->
	    case needs_rotating([debug, normal, error], Size, State) of
		[] ->
		    {noreply, State};
		L ->
		    %% Create rotation filename suffix from current time
		    Suffix = create_filename_time_suffix(),
		    {_, NewState} = rotate(L, Suffix, State),
		    {noreply, NewState}
	    end
    end;

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Unknown info.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(Unknown, State) ->
    logger:log(error, "Logger: Received unknown signal :~n~p", [Unknown]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    file:close(State#state.error_iodev),
    file:close(State#state.normal_iodev),
    file:close(State#state.debug_iodev),
    Reason.

%%--------------------------------------------------------------------
%% @spec    (_OldVsn, State, _Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% XXX is it ok, to call erlang:fault/2 if file can't be opened ?
safe_open(Filename, Args) when is_list(Filename) ->
    case file:open(Filename, Args) of
	{ok, FD} ->
	    {ok, FD};
	{error, E} ->
	    EStr = file:format_error(E),
	    Msg = io_lib:format("Error opening logfile '~s' : ~s (~p)", [Filename, EStr, E]),
	    erlang:fault(lists:flatten(Msg), [Filename, Args])
    end.

%%--------------------------------------------------------------------
%% @spec    (Enabled, IoDevice, Data) -> ok | term()
%%
%%            Enabled  = true | false
%%            IoDevice = term() "where to send io:format() output (a file descriptor)"
%%            Data     = binary()
%%
%% @doc     Log a message to an IO device (file). Note : catch is used
%%          to ensure that formating error in io:format calls are
%%          logged - they may otherwise simply crash the logger
%%          without feedback, if the logger is run without a erlang
%%          shell to look at
%% @end
%%--------------------------------------------------------------------
log_to_device(false, _IoDevice, _Data) ->
    ok;
log_to_device(true, IoDevice, Data) ->
    file:write(IoDevice, Data).

%%--------------------------------------------------------------------
%% @spec    (Enabled, Data) -> ok | term()
%%
%%            Enabled = true | false
%%            Data    = binary()
%%
%% @doc     Log a message to console. Note : catch is used to ensure
%%          that formating error in io:format calls are logged - they
%%          may otherwise simply crash the logger without feedback,
%%          if the logger is run without a erlang shell to look at
%% @end
%%--------------------------------------------------------------------
log_to_stdout(false, _Data) ->
    ok;
log_to_stdout(true, Data) ->
    try
	io:put_chars(Data)
    catch
	exit: {ebadf, _} ->
	    %% When our supervisor shuts down it's children, we will loose
	    %% our ability to log to stdout before we are shut down, when
	    %% we are logging other processes shutdowns and the logger
	    %% process will in fact crash, diverting attention from the
	    %% real reason for shutdown. We therefor need to ignore 'ebadf'
	    %% errors :(
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    () -> string()
%%
%% @doc     Return a date string (european style) suitable for
%%          appending to a filename when rotating it.
%% @end
%%--------------------------------------------------------------------
create_filename_time_suffix() ->
    {Megasec, Sec, _USec} = now(),
    DateTime = util:sec_to_date(Megasec * 1000000 + Sec),
    [Date, Time] = string:tokens(DateTime, " "),
    Suffix = "-" ++ Date ++ "_" ++ Time,
    Suffix.

%%--------------------------------------------------------------------
%% @spec    (In, Size, State) -> [atom()]
%%
%%            In    = [debug | normal | error]
%%            Size  = integer() "max size before rotating"
%%            State = #state{}
%%
%% @doc     Check a list of log files to see if any of them are larger
%%          than Size bytes. Return a list of all 'level' atoms whose
%%          logfiles exceeds the limit.
%% @end
%%--------------------------------------------------------------------
needs_rotating(In, Size, State) when record(State, state) ->
    Fun = fun(H, Acc) ->
		  Fn = level2filename(H, State),
		  case file:read_file_info(Fn) of
		      {ok, FileInfo} when FileInfo#file_info.size > Size ->
			  %% file_info record returned, and size is larger than Size
			  logger:log(debug, "Logger: Rotating '~p' logfile ~p since it is larger than ~p bytes (~p)",
				     [H, Fn, Size, FileInfo#file_info.size]),
			  [H | Acc];
		      _ ->
			  %% Either error returned from read_file_info, or file size is within limits.
			  %% We don't check (care) which one... check next file in list.
			  Acc
		  end
	  end,
    lists:foldl(Fun, [], In).


%% return {ok, State} | {{error, Str}, State}
%% stop processing after the first error encountered
%% XXX process all log files, even if some fail?
rotate([], _Suffix, State) when record(State, state) ->
    {ok, State};
rotate([Level | T], Suffix, State) when record(State, state) ->
    Fn = level2filename(Level, State),
    case rotate_file(Fn, Suffix) of
 	{ok, NewIoDev} ->
 	    logger:log(debug, "Rotated '~p' logfile with suffix ~p", [Level, Suffix]),
	    NewState = case Level of
			   debug ->
			       file:close(State#state.debug_iodev),
			       State#state{debug_iodev = NewIoDev};
			   normal ->
			       file:close(State#state.normal_iodev),
			       State#state{normal_iodev = NewIoDev};
			   error ->
			       file:close(State#state.error_iodev),
			       State#state{error_iodev = NewIoDev}
		       end,
 	    rotate(T, Suffix, NewState);
 	{error, E} ->
 	    logger:log(error, "Failed rotating '~p' logfile with suffix ~p : ~p", [Level, Suffix, E]),
 	    Str = lists:concat(["Failed rotating '", Level, "' logfile"]),
 	    {{error, Str}, State}
    end.

level2filename(debug, State) when record(State, state) ->
    State#state.debug_fn;
level2filename(normal, State) when record(State, state) ->
    State#state.normal_fn;
level2filename(error, State) when record(State, state) ->
    State#state.error_fn.

rotate_file(Filename, Suffix) ->
    %% Open new file before we try to rename old one to make sure
    %% there isn't any permission problems etc. with us creating a new
    %% logfile.
    %% A successfull usage will create a new empty "Filename" file,
    %% and rename the old log as "Filename ++ Suffix". Suffix is a date marker.
    %% XXX is there a mktemp() available in Erlang?
    TmpFile = Filename ++ Suffix ++ ".rotate",
    case catch safe_open(TmpFile, ?FILE_OPTS) of
        {ok, NewIoDev} ->
	    %% Try to rename the old logfile
	    case file:rename(Filename, Filename ++ Suffix) of
		ok ->
		    %% Rename temporary file to the right filename.
		    %% XXX Can we trap errors here?
		    file:rename(TmpFile, Filename),
		    {ok, NewIoDev};
		_ ->
		    file:close(NewIoDev),
		    %% XXX unlink TmpFile or not? It would be a race...
		    Filename2 = lists:flatten(Filename ++ Suffix),
                    E = io_lib:format("Failed renaming file ~p to ~p", [Filename, Filename2]),
		    {error, lists:flatten(E)}
	    end;
	_ ->
	    E = io_lib:format("Failed opening temporary file ~p in append-mode", [TmpFile]),
	    {error, lists:flatten(E)}
    end.

%% do_log is executed in caller pid (by log/2 or log/3), not in
%% persistent logger process.
do_log(Level, Format, Arguments) when is_atom(Level), is_list(Format), is_list(Arguments) ->
    %%				      Level == debug; Level == normal; Level == error ->
    LogTS = get_ts(now()),
    Data = format_msg(LogTS, Level, self(), Format, Arguments),
    gen_server:cast(logger, {log, Level, Data}),
    ok.

%% get timestamp
get_ts({Megasec, Sec, USec}) ->
    USecStr = format_usec(USec, 3),
    DateTime = util:sec_to_date(Megasec * 1000000 + Sec),
    list_to_binary([DateTime, $., USecStr]).

%% Precision = integer(),range = 1 - 6
format_usec(Usec, Precision) ->
    Str = integer_to_list(Usec),
    FullStr = string:right(Str, 6, $0), % pad left side with 0, to full 6 char length
    string:substr(FullStr, 1 , Precision).

%%--------------------------------------------------------------------
%% @spec    (TimeStamp, Level, Pid, Format, Arguments) ->
%%            LogMsg
%%
%%            TimeStamp = binary()
%%            Level     = atom()
%%            Pid       = pid()
%%            Format    = string()
%%            Arguments = term()
%%
%%            LogMsg = binary()
%%
%% @doc     Do a guarded io_lib:format() on Format and Arguments and
%%          produce a binary that we send to the logger process.
%% @end
%%--------------------------------------------------------------------
format_msg(TimeStamp, Level, Pid, Format, Arguments) ->
    %% make binary out of Format and Arguments
    Msg = case catch io_lib:format(Format, Arguments) of
	      {'EXIT', E} ->
		  Tmp = io_lib:format("LOG FORMATTING ERROR ~p, Format : ~p", [E, Format]),
		  list_to_binary(Tmp);
	      Res when is_list(Res) ->
		  list_to_binary(Res)
	  end,
    Tag = io_lib:format("~p~p:", [Level, Pid]),
    %% The 10 is the trailing linefeed
    list_to_binary([TimeStamp, 32, Tag, Msg, 10]).

%%--------------------------------------------------------------------
%% @spec    (Status, State) ->
%%            {ok, NewState}  |
%%            {error, Reason}
%%
%%            Status  = [{Type, Enabled}]
%%            Type    = all | debug | normal | error | console
%%            Enabled = bool()
%%            State   = #state{}
%%
%%            NewState = #state{}
%%            Reason   = term()
%%
%% @doc     Update what logging facilities are enabled/disabled.
%% @end
%%--------------------------------------------------------------------
update_logger_status(Status, State) ->
    case update_logger_status2(Status, State) of
	NewState when is_record(NewState, state) ->
	    {ok, NewState};
	E ->
	    E
    end.

update_logger_status2([{all, V} | T], State) when is_boolean(V) ->
    update_logger_status2(T, State#state{debug_enabled   = V,
					 normal_enabled  = V,
					 error_enabled   = V,
					 console_enabled = V
					});

update_logger_status2([{debug, V} | T], State) when is_boolean(V) ->
    update_logger_status2(T, State#state{debug_enabled = V});

update_logger_status2([{normal, V} | T], State) when is_boolean(V) ->
    update_logger_status2(T, State#state{normal_enabled = V});

update_logger_status2([{error, V} | T], State) when is_boolean(V) ->
    update_logger_status2(T, State#state{error_enabled = V});

update_logger_status2([{console, V} | T], State) when is_boolean(V) ->
    update_logger_status2(T, State#state{console_enabled = V});

update_logger_status2([H | _T], _State) ->
    {error, {invalid, H}};

update_logger_status2([], State) ->
    State.
