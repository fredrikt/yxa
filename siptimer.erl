-module(siptimer).
-export([add_timer/6, revive_timer/3, get_timer/2, get_timers_appid_matching/2,
	 get_timers_appsignal_matching/2, extract_appid/1,
	 extract_appsignal/1, extract_timeout/1, cancel_timer/2, cancel_all_timers/1,
	 cancel_timer_seq/2, cancel_timers/2, get_timers_seqs/2, extract_seqno/1,
	 extract_description/1, extract_starttime/1, 
	 cancel_timers_with_appid/2, stop_timers/1, debugfriendly/1]).

add_timer(_, 0, _, _, _, TimerList) ->
    TimerList;
add_timer(TimerSeq, Timeout, Description, AppSignal, AppId, TimerList) ->
    {ok, NewTimerRef} = timer:send_after(Timeout, {siptimer, TimerSeq, Description}),
    logger:log(debug, "Siptimer: set up timer ~p:~p, ~p seconds", [TimerSeq, Description, Timeout / 1000]),
    lists:append(TimerList, [{TimerSeq, NewTimerRef, Timeout, Description, util:timestamp(), AppSignal, AppId}]).

revive_timer(Timer, NewTimeout, TimerList) ->
    {TimerSeq, OldTimerRef, OldTimeout, Description, OldStartTime, AppSignal, AppId} = Timer,
    case get_timer(TimerSeq, TimerList) of
        none ->
	    logger:log(error, "Siptimer: Can't revive timer ~p:~p gone from list :~n~p~n", [TimerSeq, Description, TimerList]),
	    TimerList;
        _ ->
	    logger:log(debug, "Siptimer: Reviving timer ~p:~p with new timeout, ~p seconds", [TimerSeq, Description, NewTimeout / 1000]),
	    {ok, NewTimerRef} = timer:send_after(NewTimeout, {siptimer, TimerSeq, Description}),
	    lists:keyreplace(TimerSeq, 1, TimerList, {TimerSeq, NewTimerRef, NewTimeout, Description, util:timestamp(), AppSignal, AppId})
    end.
	
get_timer(TimerSeq, TimerList) ->
    case lists:keysearch(TimerSeq, 1, TimerList) of
	false ->
	    none;
	{value, Timer} ->
	    Timer
    end.

get_timers_appid_matching(AppId, []) ->
    [];
get_timers_appid_matching(AppId, [{TimerSeq, TimerRef, Timeout, Description, StartTime, AppSignal, AppId} | Rest]) ->
    lists:append([{TimerSeq, TimerRef, Timeout, Description, StartTime, AppSignal, AppId}], get_timers_appid_matching(AppId, Rest));
get_timers_appid_matching(AppId, [NoMatch | Rest]) ->
    get_timers_appid_matching(AppId, Rest).

get_timers_appsignal_matching(AppSignal, []) ->
    [];
get_timers_appsignal_matching(AppSignal, [{TimerSeq, TimerRef, Timeout, Description, StartTime, AppSignal, AppId} | Rest]) ->
    lists:append([{TimerSeq, TimerRef, Timeout, Description, StartTime, AppSignal, AppId}], get_timers_appsignal_matching(AppSignal, Rest));
get_timers_appsignal_matching(AppSignal, [NoMatch | Rest]) ->
    get_timers_appsignal_matching(AppSignal, Rest).

get_timers_seqs([], TimerList) ->
    [];
get_timers_seqs([TSeq|Rest], TimerList) ->
    [get_timer(TSeq, TimerList) | get_timers_seqs(Rest, TimerList)];
get_timers_seqs([TSeq], TimerList) ->
    [get_timer(TSeq, TimerList)].

extract_seqno({Seq, _, _, _, _, _, _}) ->
    Seq.
extract_timeout({_, _, Timeout, _, _, _, _}) ->
    Timeout.
extract_description({_, _, _, Descr, _, _, _}) ->
    Descr.
extract_starttime({_, _, _, _, StartTime, _, _}) ->
    StartTime.
extract_appsignal({_, _, _, _, _, AppSignal, _}) ->
    AppSignal.
extract_appid({_, _, _, _, _, _, AppId}) ->
    AppId.

cancel_timer_seq(TimerSeq, TimerList) ->
    case get_timer(TimerSeq, TimerList) of
	none ->
	    false;
	{TimerSeq, TRef, _, Descr, _, _, _} ->
	    logger:log(debug, "Siptimer: Cancelling timer ~p:~p", [TimerSeq, Descr]),
	    timer:cancel(TRef)
    end,
    lists:keydelete(TimerSeq, 1, TimerList).

cancel_timer([], TimerList) ->
    logger:log(error, "Siptimer: cancel_timer() called with empty Timer!"),
    false;
cancel_timer(Timer, []) ->
    logger:log(error, "Siptimer: cancel_timer() called with empty TimerList!"),
    false;
cancel_timer(Timer, TimerList) when list(TimerList) ->
    {TimerSeq, TRef, _, Descr, _, _, _} = Timer,
    logger:log(debug, "Siptimer: Cancelling timer ~p:~p", [TimerSeq, Descr]),
    timer:cancel(TRef),
    lists:keydelete(TimerSeq, 1, TimerList).

stop_timers([]) ->
    logger:log(debug, "Siptimer: Asked to stop timers but none were supplied"),
    [];
stop_timers([Timer | Rest]) ->
    {TimerSeq, TRef, _, Descr, _, _, _} = Timer,
    logger:log(debug, "Siptimer: Stopping timer ~p:~p", [TimerSeq, Descr]),
    timer:cancel(TRef),
    stop_timers(Rest).

cancel_timers(TimerList, []) ->
    logger:log(debug, "Siptimer: Asked to cancel timers but none were supplied"),
    [];
cancel_timers(TimerList, [Timer]) ->
    cancel_timer(Timer, TimerList);
cancel_timers(TimerList, [Timer | Rest]) ->
    NewTList = cancel_timer(Timer, TimerList),
    cancel_timers(NewTList, Rest).

cancel_all_timers([]) ->
    logger:log(debug, "Siptimer: Asked to cancel all timers but there are no timers to cancel"),
    [];
cancel_all_timers(TimerList) ->
    logger:log(debug, "Siptimer: Cancelling all timers :"),
    lists:map(fun (Timer) ->
    		      {TSeq, TRef, _, Descr, _, _, _} = Timer,
		      logger:log(debug, "Siptimer:   ~p:~p", [TSeq, Descr]),
		      timer:cancel(TRef)
		end, TimerList),
    [].

cancel_timers_with_appid(AppId, TimerList) when list(TimerList) ->
    case get_timers_appid_matching(AppId, TimerList) of
	[] ->
	    logger:log(debug, "Siptimer: No timers with AppId ~p found in TimerList :~n~p", [AppId, debugfriendly(TimerList)]),
	    TimerList;
    CancelTimers ->
	    logger:log(debug, "Siptimer: Cancelling all timers with AppId ~p :", [AppId]),
	    cancel_timers(TimerList, CancelTimers)
    end.

debugfriendly([]) ->
    [];
debugfriendly([{TSeq, TRef, Timeout, Descr, Starttime, AppSignal, AppId} | Rest]) ->
    lists:append([{TSeq, Descr}], debugfriendly(Rest)).

