%%%-------------------------------------------------------------------
%%% File     : siptimer.erl
%%% Author   : Fredrik <ft@it.su.se>
%%% Descrip. : Module to manage timers, for use in client and server
%%%            transactions. Make it easier to cancel timers and
%%%            revive them when we get provisional responses etc.
%%% Created  : 17 Jun 2003 by Fredrik Thulin <ft@it.su.se>
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
	 cancel_timer/2,
	 cancel_all_timers/1,
	 cancel_timers/2,
	 cancel_timers_with_appsignal/2,
	 stop_timers/1,
	 debugfriendly/1,
	 timeout2str/1,
	 extract/2,
	 empty/0,
	 get_length/1
	]).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(siptimerlist, {list}).
-record(siptimer, {ref, timer, timeout, description, starttime, appsignal}).

%%====================================================================
%% External functions
%%====================================================================
add_timer(0, _, _, TimerList) when is_record(TimerList, siptimerlist) ->
    TimerList;
add_timer(Timeout, Description, AppSignal, TimerList) when is_record(TimerList, siptimerlist) ->
    Ref = make_ref(),
    {ok, NewTimerRef} = timer:send_after(Timeout, {siptimer, Ref, Description}),
    TimeoutStr = timeout2str(Timeout),
    logger:log(debug, "Siptimer: Set up timer ~p:~p, ~s seconds", [Ref, Description, TimeoutStr]),
    NewSipTimer = #siptimer{ref=Ref, timer=NewTimerRef, timeout=Timeout, description=Description, starttime=util:timestamp(), appsignal=AppSignal},
    make_siptimerlist(lists:append(TimerList#siptimerlist.list, [NewSipTimer])).

empty() ->
    make_siptimerlist([]).

reset_timers(ResetList, TimerList) when is_record(ResetList, siptimerlist), is_record(TimerList, siptimerlist) ->
    reset_timers2(ResetList#siptimerlist.list, TimerList).

reset_timers2([], TimerList) ->
    TimerList;
reset_timers2([H | _T], TimerList) when is_record(H, siptimer), is_record(TimerList, siptimerlist) ->
    Ref = H#siptimer.ref,
    case get_timer(Ref, TimerList) of
        none ->
	    Description = H#siptimer.description,
	    logger:log(error, "Siptimer: Can't reset timer ~p:~p gone from list :~n~p~n", [Ref, Description, TimerList]),
	    TimerList;
        ThisTimer ->
            Description = ThisTimer#siptimer.description,
	    timer:cancel(ThisTimer#siptimer.timer),
	    remove_queued_result({siptimer, Ref, Description}),
            Timeout = ThisTimer#siptimer.timeout,
	    OldStarttime = ThisTimer#siptimer.starttime,
	    SecondsLeft = (OldStarttime + (Timeout div 1000)) - util:timestamp(),
	    logger:log(debug, "Siptimer: Timer ~p:~p was about to fire in ~p seconds, resetting with new timeout, ~s seconds",
			[Ref, Description, SecondsLeft, timeout2str(Timeout)]),
	    {ok, NewTimerRef} = timer:send_after(Timeout, {siptimer, Ref, Description}),
	    update_timer(ThisTimer#siptimer{timer=NewTimerRef}, TimerList)
    end.

revive_timer(SipTimer, NewTimeout, TimerList) when is_record(SipTimer, siptimer), is_record(TimerList, siptimerlist) ->
    Ref = SipTimer#siptimer.ref,
    case get_timer(Ref, TimerList) of
        none ->
	    Description = SipTimer#siptimer.description,
	    logger:log(error, "Siptimer: Can't revive timer ~p:~p gone from list :~n~p~n", [Ref, Description, TimerList]),
	    TimerList;
        ThisTimer ->
            Description = ThisTimer#siptimer.description,
	    logger:log(debug, "Siptimer: Reviving timer ~p:~p with new timeout, ~s seconds", [Ref, Description, timeout2str(NewTimeout)]),
	    {ok, NewTimerRef} = timer:send_after(NewTimeout, {siptimer, Ref, Description}),
	    update_timer(ThisTimer#siptimer{timeout=NewTimeout,timer=NewTimerRef}, TimerList)
    end.
	
update_timer(SipTimer, TList) when is_record(SipTimer, siptimer), is_record(TList, siptimerlist) ->
    Ref = SipTimer#siptimer.ref,
    make_siptimerlist(update_timer2(Ref, SipTimer, TList#siptimerlist.list, TList)).
    
update_timer2(Ref, _, [], TList) ->
    logger:log(error, "Siptimer: Asked to update a timer, but I can't find it"),
    logger:log(debug, "Siptimer: Asked to update a timer with ref=~p, but I can't find it in list :~n~p",
		[Ref, debugfriendly(TList)]),
    [];
update_timer2(Ref, NewT, [H | T], _TList) when is_record(H, siptimer), H#siptimer.ref == Ref ->
    [NewT | T];
update_timer2(Ref, NewT, [H | T], TList) when is_record(H, siptimer) ->
    lists:append([H], update_timer2(Ref, NewT, T, TList)).


get_timer(Ref, TimerList) when is_record(TimerList, siptimerlist) ->
    get_timer2(Ref, TimerList#siptimerlist.list).

get_timer2(_Ref, []) ->
    none;
get_timer2(Ref, [H | _T]) when is_record(H, siptimer), H#siptimer.ref == Ref ->
    H;
get_timer2(Ref, [H | T]) when is_record(H, siptimer) ->
    get_timer2(Ref, T).


get_timers_appsignal_matching(AppSignal, TimerList) when is_record(TimerList, siptimerlist) ->
    make_siptimerlist(get_timers_something_matching2(appsignal, AppSignal, TimerList#siptimerlist.list, [])).


extract(Values, SipTimer) when is_record(SipTimer, siptimer) ->
    extract(Values, SipTimer, []).

extract([], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    Res;
extract([ref | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, lists:append(Res, [SipTimer#siptimer.ref]));
extract([timeout | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, lists:append(Res, [SipTimer#siptimer.timeout]));
extract([description | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, lists:append(Res, [SipTimer#siptimer.description]));
extract([starttime | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, lists:append(Res, [SipTimer#siptimer.starttime]));
extract([appsignal | T], SipTimer, Res) when is_record(SipTimer, siptimer) ->
    extract(T, SipTimer, lists:append(Res, [SipTimer#siptimer.appsignal])).

cancel_timer(SipTimer, TimerList) when is_record(SipTimer, siptimer), is_record(TimerList, siptimerlist) ->
    Timer = SipTimer#siptimer.timer,
    Ref = SipTimer#siptimer.ref,
    Descr = SipTimer#siptimer.description,
    logger:log(debug, "Siptimer: Cancelling timer ~p:~p", [Ref, Descr]),
    timer:cancel(Timer),
    remove_queued_result({siptimer, Ref, Descr}),
    make_siptimerlist(del_ref(Ref, TimerList#siptimerlist.list)).

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

del_ref(_Ref, []) ->
    [];
del_ref(Ref, [H | T]) when is_record(H, siptimer), H#siptimer.ref == Ref ->
    del_ref(Ref, T);
del_ref(Ref, [H | T]) when is_record(H, siptimer) ->
    [H | del_ref(Ref, T)].

stop_timers(TimerList) when is_record(TimerList, siptimerlist) ->
    ok = stop_timers2(TimerList#siptimerlist.list).

stop_timers2([]) ->
    ok;
stop_timers2([SipTimer | Rest]) ->
    Timer = SipTimer#siptimer.timer,
    Ref = SipTimer#siptimer.ref,
    Descr = SipTimer#siptimer.description,
    logger:log(debug, "Siptimer: Stopping timer ~p:~p", [Ref, Descr]),
    timer:cancel(Timer),
    remove_queued_result({siptimer, Ref, Descr}),
    stop_timers2(Rest).

cancel_timers(CancelTimers, TimerList) when is_record(CancelTimers, siptimerlist), is_record(TimerList, siptimerlist) ->
    cancel_timers(CancelTimers#siptimerlist.list, TimerList);

cancel_timers([], TimerList) when is_record(TimerList, siptimerlist) ->
    TimerList;
cancel_timers([H | T], TimerList) when is_record(H, siptimer), is_record(TimerList, siptimerlist) ->
    NewTList = cancel_timer(H, TimerList),
    cancel_timers(T, NewTList).

cancel_all_timers([]) ->
    logger:log(debug, "Siptimer: Asked to cancel all timers but there are no timers to cancel"),
    empty();
cancel_all_timers(TimerList) when is_record(TimerList, siptimerlist) ->
    logger:log(debug, "Siptimer: Cancelling all timers :"),
    lists:map(fun (SipTimer) ->
		      Timer = SipTimer#siptimer.timer,
		      Ref = SipTimer#siptimer.ref,
		      Descr = SipTimer#siptimer.description,
		      logger:log(debug, "Siptimer:    ~p:~p", [Ref, Descr]),
		      timer:cancel(Timer),
		      remove_queued_result({siptimer, Ref, Descr})
		end, TimerList#siptimerlist.list),
    empty().

cancel_timers_with_appsignal(AppSignal, TimerList) when is_record(TimerList, siptimerlist) ->
    case get_timers_appsignal_matching(AppSignal, TimerList) of
	Foo when is_record(Foo, siptimerlist), Foo#siptimerlist.list == [] ->
	%%    logger:log(debug, "Siptimer: No timers with AppSignal ~p found in TimerList :~n~p",
	%%	       [AppSignal, debugfriendly(TimerList)]),
	    TimerList;
	CancelTimers when is_record(CancelTimers, siptimerlist) ->
	    logger:log(debug, "Siptimer: Cancelling all timers with AppSignal ~p :", [AppSignal]),
	    cancel_timers(CancelTimers, TimerList)
    end.

debugfriendly(TimerList) when is_record(TimerList, siptimerlist) ->
    debugfriendly(TimerList#siptimerlist.list);
debugfriendly([]) ->
    [];
debugfriendly([H | Rest]) when is_record(H, siptimer) ->
    RefStr = io_lib:format("~p", [H#siptimer.ref]),
    Descr = H#siptimer.description,
    SignalStr = io_lib:format("~p", [H#siptimer.appsignal]),
    Str = lists:concat([RefStr, ":", Descr, " -> ", SignalStr]),
    lists:append([lists:flatten(Str)], debugfriendly(Rest)).


timeout2str(500) ->
    "0.5";
timeout2str(Timeout) ->
    integer_to_list(Timeout div 1000).

get_length(TimerList) when is_record(TimerList, siptimerlist) ->
    length(TimerList#siptimerlist.list).


%%====================================================================
%% Internal functions
%%====================================================================

make_siptimerlist([]) ->
    #siptimerlist{list=[]};
make_siptimerlist(In) ->
    #siptimerlist{list=make_siptimerlist2(In, [])}.

make_siptimerlist2([], Res) ->
    Res;
make_siptimerlist2([H | T], Res) when is_record(H, siptimer) ->
    make_siptimerlist2(T, lists:append(Res, [H]));
make_siptimerlist2(In, Res) ->
    erlang:fault("Trying to make siptimerlist of something else than siptimers!", [In, Res]).

get_timers_something_matching2(_Key, _Value, [], Res) ->
    Res;
get_timers_something_matching2(appsignal, Value, [H | T], Res) when is_record(H, siptimer), H#siptimer.appsignal == Value ->
    get_timers_something_matching2(appsignal, Value, T, lists:append(Res, [H]));
get_timers_something_matching2(Key, Value, [H | T], Res) when is_record(H, siptimer) ->
    get_timers_something_matching2(Key, Value, T, Res).    
