%%%-------------------------------------------------------------------
%%% File    : logger.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Description : Logger is the process that writes our log messages
%%% to disk.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(logger).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start_link/1, log/2, log/3, quit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {debug_iodev, normal_iodev, error_iodev}).

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
    DebugIoDevice = safe_open(lists:concat([Basename, ".debug"]), [append]),
    NormalIoDevice = safe_open(lists:concat([Basename, ".log"]), [append]),
    ErrorIoDevice = safe_open(lists:concat([Basename, ".error"]), [append]),
    {ok, #state{debug_iodev=DebugIoDevice, normal_iodev=NormalIoDevice, error_iodev=ErrorIoDevice}}.

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
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
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
	    FD;
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

%% Executed in caller pid, not in persistent logger process.
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
