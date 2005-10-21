%%%-------------------------------------------------------------------
%%% File    : sup_error_logger.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Make sure we don't miss error information because our
%%            logger isn't started yet.
%%%
%%% Created :  5 Oct 2005 by Fredrik Thulin <ft@it.su.se>
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
%% Function: start()
%% Descrip.: Tell error_logger to add this handler.
%% Returns : {ok, Pid}       |
%%           {error, Reason}
%%           Pid    = pid()
%%           Reason = string()
%%--------------------------------------------------------------------
start() ->
    error_logger:add_report_handler(?MODULE, [self()]),
    receive
	{sup_error_logger, init, Pid} ->
	    {ok, Pid}
    after
	1000 ->
	    io:format("ERROR: Supervisor error handler failed to start~n~n"),
	    {error, "Supervisor error handler failed to start"}
    end.
    
%%====================================================================
%% Behaviour functions - gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Descrip.: Initialize this event handler. Called by the event
%%           manager that has been told to add this event handler.
%% Returns : {ok, State}
%%--------------------------------------------------------------------
init([Parent]) ->
    Parent ! {sup_error_logger, init, self()},
    {ok, #state{parent = Parent,
		stack = []
	       }}.

%%--------------------------------------------------------------------
%% Function: handle_event(Event, State)
%% Descrip.: This function gets called when the event manager receives
%%           an event sent using gen_event:notify/2 (or sync_notify).
%% Returns : {ok, State}                                |
%%           {swap_handler, Args1, State1, Mod2, Args2} |
%%           remove_handler
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_event({info_report, ...}, State)
%% Descrip.: Handle informational reports from the error_logger. We
%%           maintain a list of the last five process reports we see
%%           from our parent supervisor, to help narrow down where
%%           failures occur, if any.
%% Returns : {ok, NewState}
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
%% Function: handle_event({error_report, ...}, State)
%% Descrip.: Try to make sure the user sees all error information when
%%           errors occur. First print it to stdout and then, if the
%%           logger process is running, log it using the logger too.
%% Returns : {ok, State}
%%--------------------------------------------------------------------
handle_event({error_report, _SomePid, ReportArgs} = Report, State) ->
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
%% Function: handle_event(_, State)
%% Descrip.: Ignore any other events.
%% Returns : {ok, State}
%%--------------------------------------------------------------------
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, State)
%% Descrip.: This gets called when the event manager receives a
%%           request sent using gen_event:call/3,4.
%% Returns : {ok, Reply, State}                                |
%%           {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%           {remove_handler, Reply}
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_call(_, State)
%% Descrip.: This event handler does not handle calls.
%% Returns : {ok, Reply, State}                                |
%%           Reply = {error, bad_request}
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    {ok, {error, bad_request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State)
%% Descrip.: This function is called when the event manager receives
%%           any other message than an event or a synchronous request
%%           (or a system message).
%% Returns : {ok, State}                                |
%%           {swap_handler, Args1, State1, Mod2, Args2} |
%%           remove_handler
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info(_, State)
%% Descrip.: This event handler does not handle info.
%% Returns : {ok, State}
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Called when this event handler is deleted from the event
%%           manager. Clean up.
%% Returns : void()
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: describe_started_child(ChildInfo)
%%           ChildInfo = list() of tuple()
%% Descrip.: Make a string containing name and pid of started
%%           processes from data we get in info_reports.
%% Returns : Descr = string()
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

