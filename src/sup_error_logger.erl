%%%-------------------------------------------------------------------
%%% File    : sup_error_logger.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Make sure we don't miss error information because our
%%            logger isn't started yet.
%%%
%%% @since     5 Oct 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sup_error_logger).

-behaviour(gen_event).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/0]).

%%--------------------------------------------------------------------
%% Internal exports - gen_event callbacks
%%--------------------------------------------------------------------
-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {parent,
		stack
	       }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, sup_error_logger).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () ->
%%            ok | Error
%%
%%            Error = term() "error_logger:add_report_handler/2 result"
%%
%% @doc     Tell error_logger to add this handler.
%% @end
%%--------------------------------------------------------------------
start() ->
    error_logger:add_report_handler(?MODULE, {self()}).

%%====================================================================
%% Behaviour functions - gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({Parent}) -> {ok, State}
%%
%% @doc     Initialize this event handler. Called by the event manager
%%          that has been told to add this event handler.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({Parent}) ->
    {ok, #state{parent = Parent,
		stack = []
	       }}.

%%--------------------------------------------------------------------
%% @spec    handle_event(Event, State) ->
%%            {ok, State}                                |
%%            {swap_handler, Args1, State1, Mod2, Args2} |
%%            remove_handler
%%
%% @doc     This function gets called when the event manager receives
%%          an event sent using gen_event:notify/2 (or sync_notify).
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({info_report, SomePid, Event}, State) -> {ok, NewState}
%%
%%            Event  = {Parent, progress, Args}
%%            Parent = pid() "matching our parents pid from State"
%%            Args   = [{Key, Value}]
%%
%% @doc     Handle informational reports from the error_logger. We
%%          maintain a list of the last five process reports we see
%%          from our parent supervisor, to help narrow down where
%%          failures occur, if any.
%% @end
%%--------------------------------------------------------------------
handle_event({info_report, _SomePid, {Parent, progress, Args}}, #state{parent = Parent} = State) ->
    %% save the last five info_report from our top level supervisor
    case Args of
	[{supervisor, {local, sipserver_sup}},
	 {started, ChildInfo}
	] ->
	    Descr = describe_started_child(ChildInfo),
	    NewStack =
		case State#state.stack of
		    [A, B, C, D | _] ->
			[Descr, A, B, C, D];
		    _ ->
			%% stack is not yet more than four entrys
			[Descr | State#state.stack]
		end,
	    {ok, State#state{stack = NewStack}};
	_ ->
	    {ok, State}
    end;

%%--------------------------------------------------------------------
%% @spec    ({error_report, SomePid, ReportArgs}, State) ->
%%            {ok, State}
%%
%% @doc     Try to make sure the user sees all error information when
%%          errors occur. First print it to stdout and then, if the
%%          logger process is running, log it using the logger too.
%% @end
%%--------------------------------------------------------------------
handle_event({error_report, _SomePid, _ReportArgs} = Report, State) ->
    ReportStr = lists:flatten( io_lib:format("~p", [Report]) ),
    io:format("Supervisor error:~n~s~nLast five successfully started subsystems (oldest last) :~n~p~n~n",
	      [ReportStr, State#state.stack]),
    case util:safe_is_process_alive(logger) of
	{true, _Logger} ->
	    logger:log(error, "Supervisor error :~n~s~n"
		       "Last five successfully started subsystems (oldest last) :~n~p",
		       [ReportStr, State#state.stack]);
	_ ->
	    ok
    end,
    {ok, State};

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {ok, State}
%%
%% @doc     Ignore any other events.
%% @end
%%--------------------------------------------------------------------
handle_event(_Unknown, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec    (Request, State) ->
%%            {ok, Reply, State}                                |
%%            {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%            {remove_handler, Reply}
%%
%% @doc     This gets called when the event manager receives a request
%%          sent using gen_event:call/3,4.
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) ->
%%            {ok, Reply, State}
%%
%%            Reply = {error, bad_request}
%%
%% @doc     This event handler does not handle calls.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(_Unknown, State) ->
    {ok, {error, bad_request}, State}.

%%--------------------------------------------------------------------
%% @spec    (Info, State) ->
%%            {ok, State}                                |
%%            {swap_handler, Args1, State1, Mod2, Args2} |
%%            remove_handler
%%
%% @doc     This function is called when the event manager receives
%%          any other message than an event or a synchronous request
%%          (or a system message).
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {ok, State}
%%
%% @doc     This event handler does not handle info.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(_Unknown, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> void()
%%
%% @doc     Called when this event handler is deleted from the event
%%          manager. Clean up.
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (OldVsn, State, Extra) -> {ok, NewState}
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

%%--------------------------------------------------------------------
%% @spec    (ChildInfo) ->
%%            Descr
%%
%%            ChildInfo = [tuple()]
%%
%%            Descr = string()
%%
%% @doc     Make a string containing name and pid of started processes
%%          from data we get in info_reports.
%% @end
%%--------------------------------------------------------------------
describe_started_child(ChildInfo) when is_list(ChildInfo) ->
    ChildName =
	case lists:keysearch(name, 1, ChildInfo) of
	    {value, {name, ChildName1}} ->
		ChildName1;
	    false ->
		"no name"
	end,

    ChildPid =
	case lists:keysearch(pid, 1, ChildInfo) of
	    {value, {pid, ChildPid1}} ->
		pid_to_list(ChildPid1);
	    false ->
		"no pid"
	end,
    lists:flatten( lists:concat(["pid=", ChildPid,
				 "name=", ChildName])).

