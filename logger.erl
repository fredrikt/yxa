%%%-------------------------------------------------------------------
%%% File    : logger.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: Logger is the process that writes our log messages
%%%           to disk (and erlang shell).
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%
%%% Note: binary data is much faster to write to disk (~ x100 times)
%%%       and may be preferable if this module is used with higher 
%%%       loads. 
%%% Note: with the current implementation - the logger being 
%%%       registered as 'logger' there can be only one logger per 
%%%       node. Running the same application on several nodes both 
%%%       wich have the same file:get_cwd/0 will result in both 
%%%       using the same log files UNLESS they have a different
%%%       configured logger_logbasename.
%%% Note: erlang/OTP has it's own log module - disk_log, that among 
%%%       other things is able to do log rotation
%%% 
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
	 quit/1
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

%% used to keep track of the files, used by the log functions, one 
%% file type of file is keept for each type of log (debug, normal, 
%% error)
-record(state, {
	  debug_iodev,  % file descriptor
	  debug_fn,     % file name
	  normal_iodev, % file descriptor
	  normal_fn,    % file name
	  error_iodev,  % file descriptor
	  error_fn      % file name
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link(AppName)
%%           start_link()
%% Descrip.: start the server. 
%%           start_link/0 uses the current application as AppName.
%%           The logger is only registered localy (on the current 
%%           node)
%% Returns : gen_server:start_link/4
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


%%--------------------------------------------------------------------
%% Function: log(Level, Format, Arguments) 
%%           Level = normal | debug | error
%%           Format = string(), a io:format format string
%%           Arguments = list(), a io:format argument list
%% Descrip.: log a log entry
%% Returns : ok
%%--------------------------------------------------------------------
log(Level, Format) when atom(Level), list(Format) ->
    log(Level, Format, []).

log(Level, Format, Arguments) when atom(Level), list(Format), list(Arguments) ->
    do_log(Level, Format, Arguments).

%%--------------------------------------------------------------------
%% Function: quit(Msg)
%%           Msg = none | [] | term()
%% Descrip.: terminate the logger process. if Msg is term(), it will 
%%           be sent to the log before quiting "log(normal, Msg)"
%% Returns : ok | {error, "logger error"}
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

%%====================================================================
%% Behaviour functions
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
    DefaultSize = 250 * 1024 * 1024,
    case sipserver:get_env(max_logfile_size, DefaultSize) of
	none -> 
	    %% max_logfile_size = none, don't rotate logs
	    ok;
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
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages.
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%%           Msg = {rotate_logs}               | 
%%                 {rotate_logs, Suffix}       |
%%                 {rotate_logs, Suffix, Logs} |
%%                 {quit}                           quit logger
%%           Suffix = string(), log date suffix to use, 
%%                              default = current time
%%           Logs = atom(), logs to update, default = all logs
%% Descrip.: Handling call messages
%% Returns : {reply, Reply, State}
%%           Reply  = ok | {error, Reason}
%%           Reason = string()
%%
%% Note    : This code is not invoked from anywhere. Intended for
%%           future use when rotate_log may be triggered from other
%%           sources than the timer, but can of course be used
%%           manually (from the erl shell) on a running system.
%%--------------------------------------------------------------------
handle_call({rotate_logs}, From, State) ->
    %% Create rotation filename suffix from current time
    Suffix = create_filename_time_suffix(),
    {Res, NewState} = rotate([debug, normal, error], Suffix, State),
    {reply, Res, NewState};

handle_call({rotate_logs, Suffix}, From, State) ->
    {Res, NewState} = rotate([debug, normal, error], Suffix, State),
    {reply, Res, NewState};

handle_call({rotate_logs, Suffix, Logs}, From, State) when list(Logs) ->
    {Res, NewState} = rotate(Logs, Suffix, State),
    {reply, Res, NewState};

handle_call({quit}, From, State) ->
    {stop, normal, {ok}, State};

handle_call(Unknown, From, State) ->
    logger:log(error, "Logger: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "unknown gen_server call in logger"}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%%           Msg = {log, Level, TimeStamp, Format, Arguments, Pid}
%%           Level     = debug | normal | error
%%           TimeStamp = string()
%%           Format    = string(), io_lib:format() format
%%           Arguments = list() of term(), io_lib:format() arguments
%%           Pid       = pid(), log message originators pid
%% Descrip.: Write a log message to one or more of our log files (and
%%           to stdout if Level /= debug). Having this as a cast()
%%           makes the log/2 and log/3 calls asynchronous.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_cast({log, Level, TimeStamp, Format, Arguments, Pid}, State) ->
    case Level of
	error ->
	    log_to_device(State#state.error_iodev, Level, TimeStamp, Format, Arguments, Pid),
	    log_to_device(State#state.normal_iodev, Level, TimeStamp, Format, Arguments, Pid),
	    log_to_device(State#state.debug_iodev, Level, TimeStamp, Format, Arguments, Pid), 
	    log_to_stdout(Level, TimeStamp, Format, Arguments, Pid);
	normal ->
	    log_to_device(State#state.normal_iodev, Level, TimeStamp, Format, Arguments, Pid),
	    log_to_device(State#state.debug_iodev, Level, TimeStamp, Format, Arguments, Pid),
	    log_to_stdout(Level, TimeStamp, Format, Arguments, Pid);
	debug ->
	    log_to_device(State#state.debug_iodev, Level, TimeStamp, Format, Arguments, Pid)
    end,
    {noreply, State};

handle_cast(Unknown, State) ->
    logger:log(error, "Logger: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info({check_logfile_size, Size}, State)
%% Descrip.: Periodically gets invoked by timer. Check if any of our
%%           logfiles are greater than our configured limit, if so -
%%           call rotate() on them.
%% Returns : {noreply, NewState}
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
	    Suffix = create_filename_time_suffix(),
	    {_, NewState} = rotate(L, Suffix, State),
	    {noreply, NewState}
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

%%====================================================================
%% Internal functions
%%====================================================================

%% XXX is it ok, to call erlang:fault/2 if file can't be opened ?
safe_open(Filename, Args) ->
    case file:open(Filename, Args) of
	{ok, FD} ->
	    {ok, FD};
	{error, E} ->
	    erlang:fault("Error opening logfile", [Filename, Args])
    end.

%%--------------------------------------------------------------------
%% Function: log_to_device(IoDevice, Level, TimeStamp, Format, Arguments, Pid)
%%           log_to_stdout(Level, TimeStamp, Format, Arguments, Pid)
%%           Level, TimeStamp, Pid = used to identify the message
%%           Format, Arguments = used by io:format
%%           IoDevice = where to send io:format out put (a file)
%% Descrip.: log a message
%% Returns : -
%% Note    : catch is used to ensure that formating error in io:format
%%           calls are logged - they may otherwise simply crash the 
%%           logger with out feedback, if the logger is run without
%%           a erlang shell to look at
%%--------------------------------------------------------------------
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
%% Descrip.: Return a date string (european style) suitable for
%%           appending to a filename when rotating it.
%% Returns : string() 
%%--------------------------------------------------------------------
create_filename_time_suffix() ->
    {Megasec, Sec, USec} = now(),
    DateTime = util:sec_to_date(Megasec * 1000000 + Sec),
    [Date, Time] = string:tokens(DateTime, " "),
    Suffix = "-" ++ Date ++ "_" ++ Time,
    Suffix.

%%--------------------------------------------------------------------
%% Function: needs_rotating(In, Size, State)
%%           In = list() of atom(), level atoms - debug, normal, error
%%           Size = integer(), max size before rotating
%%           State = gen_server handle_xxx function state
%% Descrip.: Check a list of log files to see if any of
%%           them are larger than Size bytes. Return a list of all
%%           'level' atoms whose logfiles exceeds the limit.
%% Returns : list() of atom()
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
		    %% Either error returned from read_file_info, or file size is within limits.
		    %% We don't check (care) which one... check next file in list.
		    %%
		    %% XXX is this a good idea ? the files should not be unreadable ! 
		    %% should we throw some kind of error ?
		    %% at least it is true that we don't know if the files need rotating.
		    _ ->
			Acc
		end
	end,
    lists:foldl(Fun, [], In).


%% return {ok, State} | {{error, Str}, State}
%% stop processing after the first error encountered 
%% XXX it would be better to process all log files, even if some fail
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
    %% logfile.
    %% A successfull usage will create a new empty "Filename" file, 
    %% and rename the old log as "Filename ++ Suffix". Suffix is a date marker.
    %% XXX is there a mktemp() available in Erlang?
    TmpFile = Filename ++ Suffix ++ ".rotate",
    case catch safe_open(TmpFile, [append]) of
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
		    {error, E}
	    end;
	_ ->
	    E = io_lib:format("Failed opening temporary file ~p in append-mode", [TmpFile]),
	    {error, E}
    end.

%% do_log is executed in caller pid (by log/2 or log/3), not in
%% persistent logger process.
do_log(Level, Format, Arguments) when atom(Level), list(Format), list(Arguments) ->
    {Megasec, Sec, USec} = now(),
    USecStr = format_usec(USec, 3),
    DateTime = util:sec_to_date(Megasec * 1000000 + Sec),
    LogTS = lists:concat([DateTime, ".", USecStr]),
    gen_server:cast(logger, {log, Level, LogTS, Format, Arguments, self()}),
    ok.


%% Precision = integer(),range = 1 - 6
format_usec(Usec, Precision) ->
    Str = integer_to_list(Usec),
    FullStr = string:right(Str, 6, $0), % pad left side with 0, to full 6 char length
    string:substr(FullStr, 1 , Precision).



