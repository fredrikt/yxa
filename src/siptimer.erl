%%%-------------------------------------------------------------------
%%% File     : siptimer.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Module to manage timers, for use in client and server
%%%            transactions. Make it easier to cancel timers and
%%%            revive them when we get provisional responses etc.
%%% @since    17 Jun 2003 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(siptimer).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 add_timer/4,
	 revive_timer/3,
	 reset_timers/2,
	 get_timer/2,
	 get_timers_appsignal_matching/2,
	 cancel_all_timers/1,
	 cancel_timers/2,
	 cancel_timers_with_appsignal/2,
	 debugfriendly/1,
	 timeout2str/1,
	 extract/2,
	 empty/0,
	 get_length/1,

	 test/0,
	 test_get_appsignals/1
	]).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type siptimerlist() = #siptimerlist{}.
%%                        no description
-record(siptimerlist, {
	  list		%% list() of siptimer record()
	 }).
%% @type siptimer() = #siptimer{}.
%%                    no description
-record(siptimer, {
	  ref,		%% ref(), unique reference
	  timer,	%% term(), timer reference
	  timeout,	%% integer(), timeout in milliseconds
	  description,	%% string(), description of timer
	  starttime,	%% integer(), start time in util:timestamp() format
	  appsignal	%% term(), application signal this timer represents
	 }).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Timeout, Description, AppSignal, TimerList) ->
%%            NewTimerList
%%
%%            Timeout     = integer() "new timeout in milliseconds"
%%            Description = string()
%%            AppSignal   = term()
%%            TimerList   = #siptimerlist{}
%%
%%            NewTimerList = #siptimerlist{}
%%
%% @doc     Create a new timer and add it to TimerList.
%% @end
%%--------------------------------------------------------------------
add_timer(0, _Description, _AppSignal, TimerList) when is_record(TimerList, siptimerlist) ->
    TimerList;
add_timer(Timeout, Description, AppSignal, TimerList) when is_record(TimerList, siptimerlist) ->
    Ref = make_ref(),
    {ok, NewTimerRef} = timer:send_after(Timeout, {siptimer, Ref, Description}),
    TimeoutStr = timeout2str(Timeout),
    logger:log(debug, "Siptimer: Set up timer ~p:~p, ~s seconds", [Ref, Description, TimeoutStr]),
    NewSipTimer = #siptimer{ref = Ref,
			    timer = NewTimerRef,
			    timeout = Timeout,
			    description = Description,
			    starttime = util:timestamp(),
			    appsignal = AppSignal
			   },
    make_siptimerlist( lists:append(TimerList#siptimerlist.list, [NewSipTimer]) ).


%%--------------------------------------------------------------------
%% @spec    () ->
%%            NewTimerList
%%
%%            NewTimerList = #siptimerlist{}
%%
%% @doc     Create an empty siptimerlist.
%% @end
%%--------------------------------------------------------------------
empty() ->
    make_siptimerlist([]).

%%--------------------------------------------------------------------
%% @spec    (Timers, TimerList) ->
%%            NewTimerList
%%
%%            Timers    = [#siptimer{}]
%%            TimerList = #siptimerlist{}
%%
%%            NewTimerList = #siptimerlist{}
%%
%% @doc     Reset all timers listed in Timers to their original
%%          timeout value.
%% @end
%%--------------------------------------------------------------------
reset_timers([H | T], TimerList) when is_record(H, siptimer), is_record(TimerList, siptimerlist) ->
    Ref = H#siptimer.ref,
    case get_timer(Ref, TimerList) of
        none ->
	    Description = H#siptimer.description,
	    logger:log(error, "Siptimer: Can't reset timer ~p:~p not found in list",
		       [Ref, Description]),
	    logger:log(debug, "Siptimer: Timerlist where timer ~p:~p was not found :~n~p",
		       [Ref, Description, debugfriendly(TimerList)]),
	    reset_timers(T, TimerList);
        ThisTimer ->
            Description = ThisTimer#siptimer.description,
	    case timer:cancel(ThisTimer#siptimer.timer) of
		{ok, cancel} -> ok;
		{error, Reason} ->
		    logger:log(error, "Siptimer: Warning: Reset timer ~p:~p - cancel of old timer failed : ~p",
			       [Ref, Description, Reason])
	    end,
	    remove_queued_result({siptimer, Ref, Description}),
            Timeout = ThisTimer#siptimer.timeout,
	    OldStarttime = ThisTimer#siptimer.starttime,
	    SecondsLeft = (OldStarttime + (Timeout div 1000)) - util:timestamp(),
	    logger:log(debug, "Siptimer: Timer ~p:~p was about to fire in ~p seconds, resetting with new timeout, "
		       "~s seconds", [Ref, Description, SecondsLeft, timeout2str(Timeout)]),
	    {ok, NewTimerRef} = timer:send_after(Timeout, {siptimer, Ref, Description}),
	    NewTimerList = update_timer(ThisTimer#siptimer{timer=NewTimerRef}, TimerList),
	    reset_timers(T, NewTimerList)
    end;
reset_timers([], TimerList) when is_record(TimerList, siptimerlist) ->
    TimerList.

%%--------------------------------------------------------------------
%% @spec    (SipTimer, NewTimeout, TimerList) ->
%%            NewTimerList
%%
%%            SipTimer   = #siptimer{}
%%            NewTimeout = integer() "new timeout in milliseconds"
%%            TimerList  = #siptimerlist{}
%%
%%            NewTimerList = #siptimerlist{}
%%
%% @doc     Revive a sip timer. This means we start a new timer with
%%          the same parameters as the old one, but with a new
%%          timeout value. Don't revive a timer that hasn't fired or
%%          you will have two timers that might fire!
%% @end
%%--------------------------------------------------------------------
revive_timer(SipTimer, NewTimeout, TimerList) when is_record(SipTimer, siptimer), is_integer(NewTimeout),
						   is_record(TimerList, siptimerlist) ->
    Ref = SipTimer#siptimer.ref,
    case get_timer(Ref, TimerList) of
        none ->
	    Description = SipTimer#siptimer.description,
	    logger:log(error, "Siptimer: Can't revive timer ~p:~p not found in list :~n~p~n",
		       [Ref, Description, TimerList]),
	    TimerList;
        ThisTimer ->
            Description = ThisTimer#siptimer.description,
	    logger:log(debug, "Siptimer: Reviving timer ~p:~p with new timeout, ~s seconds",
		       [Ref, Description, timeout2str(NewTimeout)]),
	    {ok, NewTimerRef} = timer:send_after(NewTimeout, {siptimer, Ref, Description}),
	    NewTimer = ThisTimer#siptimer{timeout = NewTimeout,
					  timer = NewTimerRef
					 },
	    update_timer(NewTimer, TimerList)
    end.

%%--------------------------------------------------------------------
%% @spec    (SipTimer, TimerList) ->
%%            NewTimerList
%%
%%            SipTimer  = #siptimer{}
%%            TimerList = #siptimerlist{}
%%
%%            NewTimerList = #siptimerlist{}
%%
%% @doc     Locate a timer in TimerList whose reference matches the
%%          reference of SipTimer. Return TimerList with the matching
%%          timer replaced by SipTimer.
%% @end
%%--------------------------------------------------------------------
update_timer(SipTimer, TList) when is_record(SipTimer, siptimer), is_record(TList, siptimerlist) ->
    Ref = SipTimer#siptimer.ref,
    make_siptimerlist(update_timer2(Ref, SipTimer, TList#siptimerlist.list, TList)).

update_timer2(Ref, NewT, [H | T], _TList) when is_record(H, siptimer), H#siptimer.ref == Ref ->
    [NewT | T];
update_timer2(Ref, NewT, [H | T], TList) when is_record(H, siptimer) ->
    lists:append([H], update_timer2(Ref, NewT, T, TList)).

%%--------------------------------------------------------------------
%% @spec    (Ref, TimerList) -> #siptimer{} | none
%%
%%            Ref       = ref() "unique reference"
%%            TimerList = #siptimerlist{}
%%
%% @doc     Locate a timer in TimerList whose reference matches Ref.
%% @end
%%--------------------------------------------------------------------
get_timer(Ref, TimerList) when is_record(TimerList, siptimerlist) ->
    get_timer2(Ref, TimerList#siptimerlist.list).

get_timer2(_Ref, []) ->
    none;
get_timer2(Ref, [H | _T]) when is_record(H, siptimer), H#siptimer.ref == Ref ->
    H;
get_timer2(Ref, [H | T]) when is_record(H, siptimer) ->
    get_timer2(Ref, T).

%%--------------------------------------------------------------------
%% @spec    (AppSignal, TimerList) -> [#siptimer{}]
%%
%%            AppSignal = term()
%%            TimerList = #siptimerlist{}
%%
%% @doc     Locate all timers in TimerList which have their
%%          'appsignal' element matching AppSignal.
%% @end
%%--------------------------------------------------------------------
get_timers_appsignal_matching(AppSignal, TimerList) when is_record(TimerList, siptimerlist) ->
    get_timers_appsignal_matching2(AppSignal, TimerList#siptimerlist.list, []).

get_timers_appsignal_matching2(Value, [#siptimer{appsignal = Value}=H | T], Res) ->
    get_timers_appsignal_matching2(Value, T, [H | Res]);
get_timers_appsignal_matching2(Value, [H | T], Res) when is_record(H, siptimer) ->
    get_timers_appsignal_matching2(Value, T, Res);
get_timers_appsignal_matching2(_Value, [], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% @spec    (Values, SipTimer) -> [term()]
%%
%%            Values    = [ref        |
%%                        timeout     |
%%                        description |
%%                        starttime   |
%%                        appsignal]
%%            TimerList = #siptimerlist{}
%%
%% @doc     Extract elements from a siptimer record().
%% @end
%%--------------------------------------------------------------------
extract(Values, SipTimer) when is_record(SipTimer, siptimer) ->
    extract(Values, SipTimer, []).

extract([], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    lists:reverse(Res);
extract([ref | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, [SipTimer#siptimer.ref | Res]);
extract([timeout | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, [SipTimer#siptimer.timeout | Res]);
extract([description | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, [SipTimer#siptimer.description | Res]);
extract([starttime | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, [SipTimer#siptimer.starttime | Res]);
extract([appsignal | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, [SipTimer#siptimer.appsignal | Res]).

%%--------------------------------------------------------------------
%% @spec    (Timers, TimerList) ->
%%            NewTimerList
%%
%%            Timers    = [#siptimer{}]
%%            TimerList = #siptimerlist{} | none
%%
%%            NewTimerList = #siptimerlist{}
%%
%% @doc     Cancel a siptimer, return a new TimerList without the
%%          cancelled siptimer.
%% @end
%%--------------------------------------------------------------------
cancel_timers([H | T], TimerList) when is_record(H, siptimer), is_record(TimerList, siptimerlist) ->
    Timer = H#siptimer.timer,
    Ref = H#siptimer.ref,
    Descr = H#siptimer.description,
    logger:log(debug, "Siptimer: Cancelling timer ~p:~p", [Ref, Descr]),
    case timer:cancel(Timer) of
	{ok, cancel} -> ok;
	{error, Reason} ->
	    logger:log(error, "Siptimer: Warning: Cancel timer ~p:~p failed : ~p",
		       [Ref, Descr, Reason])
    end,
    remove_queued_result({siptimer, Ref, Descr}),
    NewTimerList = make_siptimerlist( del_ref(Ref, TimerList#siptimerlist.list) ),
    cancel_timers(T, NewTimerList);
cancel_timers([], TimerList) when is_record(TimerList, siptimerlist) ->
    TimerList.

%%--------------------------------------------------------------------
%% @spec    (TimerList) ->
%%            EmptyList
%%
%%            TimerList = #siptimerlist{} | none
%%
%%            EmptyList = #siptimerlist{}
%%
%% @doc     Cancel all siptimers in TimerList.
%% @end
%%--------------------------------------------------------------------
cancel_all_timers(TimerList) when is_record(TimerList, siptimerlist) ->
    EmptyList = empty(),
    logger:log(debug, "Siptimer: Cancelling all timers :"),
    cancel_timers(TimerList#siptimerlist.list, EmptyList),
    EmptyList.

%%--------------------------------------------------------------------
%% @spec    (AppSignal, TimerList) ->
%%            NewTimerList
%%
%%            AppSignal = term() "appsignal to match on"
%%            TimerList = #siptimerlist{} | none
%%
%%            NewTimerList = #siptimerlist{}
%%
%% @doc     Cancel all siptimers in TimerList that has an appsignal
%%          matching AppSignal.
%% @end
%%--------------------------------------------------------------------
cancel_timers_with_appsignal(AppSignal, TimerList) when is_record(TimerList, siptimerlist) ->
    case get_timers_appsignal_matching(AppSignal, TimerList) of
	[] ->
	    %%    logger:log(debug, "Siptimer: No timers with AppSignal ~p found in TimerList :~n~p",
	    %%	       [AppSignal, debugfriendly(TimerList)]),
	    TimerList;
	CancelTimers when is_list(CancelTimers) ->
	    logger:log(debug, "Siptimer: Cancelling all timers with AppSignal ~p :", [AppSignal]),
	    cancel_timers(CancelTimers, TimerList)
    end.

%%--------------------------------------------------------------------
%% @spec    (TimerList) -> [string()]
%%
%%            TimerList = #siptimerlist{} | none
%%
%% @doc     Format all timers in TimerList into strings suitable for
%%          debug logging.
%% @end
%%--------------------------------------------------------------------
debugfriendly(TimerList) when is_record(TimerList, siptimerlist) ->
    debugfriendly2(TimerList#siptimerlist.list, []).

debugfriendly2([H | T], Res) when is_record(H, siptimer) ->
    RefStr = io_lib:format("~p", [H#siptimer.ref]),
    Descr = H#siptimer.description,
    SignalStr = io_lib:format("~p", [H#siptimer.appsignal]),
    Str = lists:flatten( lists:concat([RefStr, ":", Descr, " -> ", SignalStr]) ),
    debugfriendly2(T, [Str | Res]);
debugfriendly2([], Res) ->
    lists:reverse(Res).

%%--------------------------------------------------------------------
%% @spec    (Timeout) ->
%%            TStr
%%
%%            Timeout = integer() "milliseconds"
%%
%%            TStr = string()
%%
%% @doc     Give string second representation of Timeout.
%% @end
%%--------------------------------------------------------------------
timeout2str(500) ->
    "0.5";
timeout2str(Timeout) ->
    integer_to_list(Timeout div 1000).

%%--------------------------------------------------------------------
%% @spec    (TimerList) ->
%%            Len
%%
%%            TimerList = #siptimerlist{}
%%
%%            Len = integer()
%%
%% @doc     Return the length of the list of siptimers contained in
%%          the siptimerlist record().
%% @end
%%--------------------------------------------------------------------
get_length(TimerList) when is_record(TimerList, siptimerlist) ->
    length(TimerList#siptimerlist.list).


%%====================================================================
%% Internal functions
%%====================================================================

make_siptimerlist([]) ->
    #siptimerlist{list=[]};
make_siptimerlist(In) ->
    #siptimerlist{list=make_siptimerlist2(In, [])}.

%% make_siptimerlist2/2 - part of make_siptimerlist/1.
%% Make sure all elements are siptimer record().
%% Returns : list() of siptimer record()
make_siptimerlist2([H | T], Res) when is_record(H, siptimer) ->
    make_siptimerlist2(T, [H | Res]);
make_siptimerlist2([], Res) ->
    lists:reverse(Res).

%% Receive all messages matching Signal from this processes mailbox. When there
%% are no more signals matching Signal, return ok. This is to remove any signals
%% from a timer that we have in queue when we cancel the timer.
remove_queued_result(Signal) ->
    receive
	Signal ->
	    remove_queued_result(Signal)
    after 0 ->
	    ok
    end.

%% part of cancel_timer/2
del_ref(_Ref, []) ->
    [];
del_ref(Ref, [H | T]) when is_record(H, siptimer), H#siptimer.ref == Ref ->
    del_ref(Ref, T);
del_ref(Ref, [H | T]) when is_record(H, siptimer) ->
    [H | del_ref(Ref, T)].


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (TimerList) ->
%%            AppSignals
%%
%%            AppSignals = [term()]
%%
%% @doc     Exported function for use in unit testings of modules
%%          using siptimer timers. Returns just the appsignals from
%%          the timers in TimerList, in fixed order of which timers
%%          would fire first.
%% @end
%%--------------------------------------------------------------------
test_get_appsignals(TimerList) when is_record(TimerList, siptimerlist) ->
    Sorted = lists:sort(fun test_get_appsignals_sort/2, TimerList#siptimerlist.list),
    lists:map(fun(Timer) ->
		      Timer#siptimer.appsignal
	      end, Sorted).

test_get_appsignals_sort(A, B) when is_record(A, siptimer), is_record(B, siptimer) ->
    At = A#siptimer.starttime + (A#siptimer.timeout / 1000),
    Bt = B#siptimer.starttime + (B#siptimer.timeout / 1000),
    (Bt >= At).

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    %% empty()
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "emtpy/0 - 1"),
    #siptimerlist{list=[]} = EmptyList = empty(),


    %% add_timer(Timeout, Description, AppSignal, TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add_timer/4 - 1"),
    %% Test with zero timeout, no timer will be created
    EmptyList = add_timer(0, "foo", none, EmptyList),

    autotest:mark(?LINE, "add_timer/4 - 2"),
    %% Add a timer with a high timeout so that we have time to cancel it again
    AddTimerL1 = add_timer(100 * 1000, "test: add_timer/4 timer 1", {test_siptimer, 1}, EmptyList),

    autotest:mark(?LINE, "add_timer/4 - 2"),
    %% Add another timer with a high timeout so that we have time to cancel it again
    AddTimerL2 = add_timer(200 * 1000, "test: add_timer/4 timer 2", {test_siptimer, 2}, AddTimerL1),


    %% get_length(TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_length/1 - 1"),
    0 = get_length(EmptyList),

    autotest:mark(?LINE, "get_length/1 - 2"),
    1 = get_length(AddTimerL1),

    autotest:mark(?LINE, "get_length/1 - 3"),
    2 = get_length(AddTimerL2),


    %% get_timers_appsignal_matching(AppSignal, TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_timers_appsignal_matching/2 - 1"),
    %% Look for timer that does not exist
    [] = get_timers_appsignal_matching(none, AddTimerL2),

    autotest:mark(?LINE, "get_timers_appsignal_matching/2 - 2"),
    %% Get timer 1
    [GTAM_T1] = get_timers_appsignal_matching({test_siptimer, 1}, AddTimerL2),
    %% verify results
    #siptimer{appsignal = {test_siptimer, 1}} = GTAM_T1,

    autotest:mark(?LINE, "get_timers_appsignal_matching/2 - 3"),
    %% Get timer 2
    [GTAM_T2] = get_timers_appsignal_matching({test_siptimer, 2}, AddTimerL2),
    %% verify results
    #siptimer{appsignal = {test_siptimer, 2}} = GTAM_T2,


    %% get_timer(Ref, TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_timer/2 - 1"),
    none = get_timer(make_ref(), AddTimerL2),

    autotest:mark(?LINE, "get_timer/2 - 2"),
    GTAM_T2_Ref = GTAM_T2#siptimer.ref,
    GTAM_T2 = get_timer(GTAM_T2_Ref, AddTimerL2),


    %% extract(Values, SipTimer)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "extract/2 - 1"),
    {GTAM_T2_Timeout, GTAM_T2_Desc, GTAM_T2_Start, GTAM_T2_AppS} =
	{GTAM_T2#siptimer.timeout, GTAM_T2#siptimer.description,
	 GTAM_T2#siptimer.starttime, GTAM_T2#siptimer.appsignal},

    [GTAM_T2_Ref, GTAM_T2_Timeout, GTAM_T2_Desc, GTAM_T2_Start, GTAM_T2_AppS] =
	extract([ref, timeout, description, starttime, appsignal], GTAM_T2),


    %% cancel_timers(Timers, TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "cancel_timers/2 - 1"),
    %% Cancel second timer
    AddTimerL1 = cancel_timers([GTAM_T2], AddTimerL2),

    autotest:mark(?LINE, "cancel_timers/2 - 2"),
    %% Cancel first timer
    EmptyList = cancel_timers([GTAM_T1], AddTimerL1),

    autotest:mark(?LINE, "cancel_timers/2 - 3"),
    %% Cancel first timer in list, so that second timer remains
    #siptimerlist{list = [GTAM_T2]} = cancel_timers([GTAM_T1], AddTimerL2),


    %% cancel_timers_with_appsignal(AppSignal, TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "cancel_timers_with_appsignal/2 - 1"),
    AddTimerL2 = cancel_timers_with_appsignal("no match", AddTimerL2),

    autotest:mark(?LINE, "cancel_timers_with_appsignal/2 - 2"),
    AddTimerL1 = cancel_timers_with_appsignal({test_siptimer, 2}, AddTimerL2),

    autotest:mark(?LINE, "cancel_timers_with_appsignal/2 - 3"),
    #siptimerlist{list = [GTAM_T2]} = cancel_timers_with_appsignal({test_siptimer, 1}, AddTimerL2),


    %% cancel_all_timers(TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "cancel_all_timers/1 - 1"),
    EmptyList = cancel_all_timers(EmptyList),

    autotest:mark(?LINE, "cancel_all_timers/1 - 2"),
    EmptyList = cancel_all_timers(AddTimerL2),


    %% revive_timer(SipTimer, NewTimeout, TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "revive_timer/3 - 1"),
    %% Revive timer that does not exist in list
    AddTimerL1 = revive_timer(GTAM_T2, 100 * 1000, AddTimerL1),

    autotest:mark(?LINE, "revive_timer/3 - 2"),
    %% Set up two new timers
    ReviveTimerL1 = add_timer(100 * 1000, "test: revive_timer/3 T1", {test_siptimer, "revive timer 1"}, EmptyList),
    ReviveTimerL2 = add_timer(100 * 1000, "test: revive_timer/3 T2", {test_siptimer, "revive timer 2"}, ReviveTimerL1),
    [ReviveT_T1] = get_timers_appsignal_matching({test_siptimer, "revive timer 1"}, ReviveTimerL2),
    [ReviveT_T2] = get_timers_appsignal_matching({test_siptimer, "revive timer 2"}, ReviveTimerL2),

    autotest:mark(?LINE, "revive_timer/3 - 3.1"),
    %% revive first timer in list (ReviveT_T1)
    ReviveTimerL3 = revive_timer(ReviveT_T1, 120 * 1000, ReviveTimerL2),

    autotest:mark(?LINE, "revive_timer/3 - 3.2"),
    %% verify results (verify that second timer was unchanged, and extract first timer as ReviveT_T1_1)
    #siptimerlist{list = [#siptimer{} = ReviveT_T1_1, ReviveT_T2]} = ReviveTimerL3,

    autotest:mark(?LINE, "revive_timer/3 - 3.3"),
    %% verify that the first timer in the list (ReviveT_T1) really was updated
    true = test_timer_was_updated(ReviveT_T1, ReviveT_T1_1),

    autotest:mark(?LINE, "revive_timer/3 - 4.1"),
    %% revive second timer in list (ReviveT_T2)
    ReviveTimerL4 = revive_timer(ReviveT_T2, 120 * 1000, ReviveTimerL3),

    autotest:mark(?LINE, "revive_timer/3 - 4.2"),
    %% verify results (verify that first timer was unchanged, and extract second timer as ReviveT_T2_1)
    #siptimerlist{list = [ReviveT_T1_1, #siptimer{} = ReviveT_T2_1]} = ReviveTimerL4,

    autotest:mark(?LINE, "revive_timer/3 - 4.3"),
    %% verify that the second timer in the list (ReviveT_T2) really was updated
    true = test_timer_was_updated(ReviveT_T2, ReviveT_T2_1),

    autotest:mark(?LINE, "revive_timer/3 - 5"),
    %% clean up
    EmptyList = cancel_all_timers(ReviveTimerL1),
    EmptyList = cancel_all_timers(ReviveTimerL2),
    EmptyList = cancel_all_timers(ReviveTimerL3),
    EmptyList = cancel_all_timers(ReviveTimerL4),


    %% reset_timers(Timers, TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "reset_timers/2 - 0"),
    %% Set up two new timers
    ResetTimersL1 = add_timer(100 * 1000, "test: reset_timers/2 T1", {test_siptimer, "reset timer 1"}, EmptyList),
    ResetTimersL2 = add_timer(100 * 1000, "test: reset_timers/2 T2", {test_siptimer, "reset timer 2"}, ResetTimersL1),
    [ResetT_T1] = get_timers_appsignal_matching({test_siptimer, "reset timer 1"}, ResetTimersL2),
    [ResetT_T2] = get_timers_appsignal_matching({test_siptimer, "reset timer 2"}, ResetTimersL2),

    autotest:mark(?LINE, "reset_timers/2 - 1"),
    %% Reset timer that does not exist in list
    ResetTimersL2 = reset_timers([#siptimer{ref = make_ref()}], ResetTimersL2),

    autotest:mark(?LINE, "reset_timers/2 - 2.1"),
    %% reset first timer in list (ResetT_T1)
    ResetTimersL3 = reset_timers([ResetT_T1], ResetTimersL2),

    autotest:mark(?LINE, "reset_timers/2 - 2.2"),
    %% verify results (verify that second timer was unchanged, and extract first timer as ResetT_T1_1)
    #siptimerlist{list = [#siptimer{} = ResetT_T1_1, ResetT_T2]} = ResetTimersL3,

    autotest:mark(?LINE, "reset_timers/2 - 2.3"),
    %% verify that the first timer in the list (ResetT_T1) really was updated
    test_timer_was_updated(ResetT_T1, ResetT_T1_1),

    autotest:mark(?LINE, "reset_timers/2 - 3.1"),
    %% reset second timer in list (ResetT_T2)
    ResetTimersL4 = reset_timers([ResetT_T2], ResetTimersL3),

    autotest:mark(?LINE, "reset_timers/2 - 3.2"),
    %% verify results (verify that first timer was unchanged, and extract second timer as ResetT_T2_1)
    #siptimerlist{list = [ResetT_T1_1, #siptimer{} = ResetT_T2_1]} = ResetTimersL4,

    autotest:mark(?LINE, "reset_timers/2 - 4.3"),
    %% verify that the second timer in the list (ResetT_T2) really was updated
    test_timer_was_updated(ResetT_T2, ResetT_T2_1),

    autotest:mark(?LINE, "reset_timers/2 - 5.1"),
    %% reset both timers (in the wrong order), and also one timer that does not exist
    ResetTimersL5 = reset_timers([ResetT_T2_1, #siptimer{ref = make_ref()}, ResetT_T1_1], ResetTimersL4),

    autotest:mark(?LINE, "reset_timers/2 - 5.2"),
    %% verify results
    #siptimerlist{list = [#siptimer{} = ResetT_T1_2, #siptimer{} = ResetT_T2_2]} = ResetTimersL5,

    autotest:mark(?LINE, "reset_timers/2 - 5.3"),
    %% verify that the first timer in the list (ResetT_T2_1) really was updated
    test_timer_was_updated(ResetT_T2_1, ResetT_T2_2),

    autotest:mark(?LINE, "reset_timers/2 - 5.4"),
    %% verify that the second timer in the list (ResetT_T1_1) really was updated
    true = test_timer_was_updated(ResetT_T1_1, ResetT_T1_2),

    autotest:mark(?LINE, "reset_timers/2 - 6"),
    %% clean up
    EmptyList = cancel_all_timers(ResetTimersL5),


    %% timeout2str(Timeout)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "timeout2str/1 - 1"),
    "0.5" = timeout2str(500),

    autotest:mark(?LINE, "timeout2str/1 - 2"),
    "1" = timeout2str(1000),

    autotest:mark(?LINE, "timeout2str/1 - 3"),
    "2" = timeout2str(2222),


    %% debugfriendly(TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "debugfriendly/1 - 1"),
    [] = debugfriendly(EmptyList),

    autotest:mark(?LINE, "debugfriendly/1 - 2"),
    2 = length( debugfriendly(AddTimerL2) ),


    %% test_get_appsignals(TimerList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "test_get_appsignals/1 - 1"),
    TestGetAppsignals_TL1 = #siptimerlist{list = [
						  #siptimer{appsignal = second,
							    starttime = 101,
							    timeout   = 10
							   },
						  #siptimer{appsignal = first,
							    starttime = 100,
							    timeout   = 500
							   },
						  #siptimer{appsignal = third,
							    starttime = 100,
							    timeout   = 1500
							   }
						 ]},
    [first, second, third] = test_get_appsignals(TestGetAppsignals_TL1),

    autotest:mark(?LINE, "test_get_appsignals/1 - 1"),
    %% test with same timeout
    TestGetAppsignals_TL2 = #siptimerlist{list = [
						  #siptimer{appsignal = first,
							    starttime = 100,
							    timeout   = 1000
							   },
						  #siptimer{appsignal = second,
							    starttime = 100,
							    timeout   = 1000
							   }
						 ]},
    [first, second] = test_get_appsignals(TestGetAppsignals_TL2),


    %% test siptimer operations
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "siptimer operations - 0"),
    TimerOp_L1 = add_timer(1, "timer that will fire but be cancelled", {test_siptimer, cancel_me}, EmptyList),
    TimerOp_L2 = add_timer(2, "timer that will fire", {test_siptimer, find_me, 1}, TimerOp_L1),
    TimerOp_L3 = add_timer(100 * 1000, "timer that will be cancelled", {test_siptimer, cancel_me}, TimerOp_L2),
    TimerOp_L4 = add_timer(100 * 1000, "timer that never will fire", {test_siptimer, never_seen}, TimerOp_L3),
    TimerOp_L5 = add_timer(1, "timer that will fire but be cancelled #2", {test_siptimer, cancel_me}, TimerOp_L4),
    [TimerOp_T1, TimerOp_T2, _, _, TimerOp_T5] = TimerOp_L5#siptimerlist.list,

    autotest:mark(?LINE, "siptimer operations - 1"),
    %% wait for the timer that will fire (appsignal '{test_siptimer, find_me, 1}')
    TimerOp_T2_Ref = TimerOp_T2#siptimer.ref,
    receive
	{siptimer, TimerOp_T2_Ref, "timer that will fire"} -> ok
    after 1000 ->
	    erlang:fault({error, "siptimer that should fire in 2 ms did not fire in 1 s"})
    end,

    autotest:mark(?LINE, "siptimer operations - 2"),
    %% check that the timer that will fire but be cancelled has fired now
    TimerOp_T1_Ref = TimerOp_T1#siptimer.ref,
    receive
	{siptimer, TimerOp_T1_Ref, "timer that will fire but be cancelled"} = TimerOp_Msg1 ->
	    %% re-send this signal to ourselves
	    self() ! TimerOp_Msg1
    after 100 ->
	    erlang:fault({error, "siptimer that should fire in 1 ms did not fire in > 100 ms"})
    end,

    autotest:mark(?LINE, "siptimer operations - 3.1"),
    %% cancel timers
    TimerOp_L6 = cancel_timers_with_appsignal({test_siptimer, cancel_me}, TimerOp_L5),

    autotest:mark(?LINE, "siptimer operations - 3.2"),
    %% verify that the cancelling of timers remove pending fired signals from our mailbox
    TimerOp_T5_Ref = TimerOp_T5#siptimer.ref,
    receive
	{siptimer, TimerOp_T1_Ref, _} ->
	    erlang:fault({error, "cancelled timers signal not removed from process mailbox"});
	{siptimer, TimerOp_T5_Ref, _} ->
	    erlang:fault({error, "cancelled timers signal #2 not removed from process mailbox"})
    after 10 ->
	    ok
    end,

    autotest:mark(?LINE, "siptimer operations - 4.1"),
    %% clean up
    cancel_all_timers(TimerOp_L6),


    %% final verification of operations
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "siptimer test cleanups - 1"),
    %% verify that we don't have any siptimer signals in our mailbox here
    receive
	{siptimer, UnknownRef, UnknownDesc} ->
	    erlang:fault({error, "Unknown siptimer has fired", [UnknownRef, UnknownDesc]})
    after 0 ->
	    ok
    end,

    ok.


%% test_timer_was_updated/2 - Check that NewT is the same siptimer as OldT, but with a
%% new 'timer' element.
%% Returns : true | {error, Reason, [OldT, NewT]}
test_timer_was_updated(OldT, NewT) when is_record(OldT, siptimer), is_record(NewT, siptimer) ->
    Ref_eq   = (NewT#siptimer.ref         == OldT#siptimer.ref),
    Desc_eq  = (NewT#siptimer.description == OldT#siptimer.description),
    AppS_eq  = (NewT#siptimer.appsignal   == OldT#siptimer.appsignal),
    Timer_eq = (NewT#siptimer.timer       == OldT#siptimer.timer),
    %% we don't check timeout since we might reset/revive a timer to the same timeout
    %% we can't check starttime since it would be a race condition (same second as first time or the next)

    if
	Ref_eq == false ->
	    {error, siptimer_ref_does_not_match, [OldT, NewT]};
	Desc_eq == false ->
	    {error, siptimer_description_does_not_match, [OldT, NewT]};
	AppS_eq == false ->
	    {error, siptimer_appsignal_does_not_match, [OldT, NewT]};
	Timer_eq == true ->
	    {error, siptimer_timer_not_updated, [OldT, NewT]};
	true ->
	    true
    end.
