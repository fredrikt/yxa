%% Notes: the parser is assumed to supply sensible time_switch__cond_x
%%        parameters:
%%      * dtstart < dtend
%%      - this is checked in xml_parse.erl, see README for more
%%        details
%%      * valid date_time, date and time values
%%      - this is also checked in xml_parse.erl
%%      * time values are assumed to not contain leap seconds
%%      - this is checked in xml_parse.erl and interpret_time.erl
%%        (for local_usec/0)
%%--------------------------------------------------------------------
%%
%%        _calendar.erl/erlang.erl handling of leap seconds_
%%
%% There are a number of time handling functions (BIFs) in the
%% erlang.erl module that take and return date-time tuples as input
%% and output as seen below. These are the functions used by
%% calendar.erl to get time data as well:
%%
%% Function                                Return type
%% --------                                -----------
%% erlang:localtime/0                   -> {{Year, Month, Day}, {Hour, Minute, Second}}
%% erlang:localtime_to_universaltime/1  -> {{Year, Month, Day}, {Hour, Minute, Second}} (UTC)
%% erlang:localtime_to_universaltime/2  -> {{Year, Month, Day}, {Hour, Minute, Second}} (UTC)
%% erlang:universaltime_to_localtime/1  -> {{Year, Month, Day}, {Hour, Minute, Second}}
%% erlang:universaltime/0               -> {{Year, Month, Day}, {Hour, Minute, Second}} (UTC)
%%
%% There is also erlang:now() which returns second information:
%% erlang:now/0                         -> {MegaSeconds, Seconds, MicroSeconds}
%%
%% Looking at erlang calendar.erl and related C source as well as
%% the documentation that says that a day is = 86400s, it appears
%% that erlang/OTP doesn't report or handle leap seconds - they are
%% treated as unix/posix time i.e. they are assumed to not exist and
%% any occurring leap seconds are reported as the previous second
%% (second = 59), this means that the seconds are "wall clock" seconds
%% rather than the number of seconds since some time X.
%%--------------------------------------------------------------------
%% One reason for why leap seconds are bad as input (in scripts),
%% besides not being supported in the byseconds attribute:
%%
%% Presume that dtstart could be set to a leap second date-time (e.g
%% 1972-06-30 23:59:60), duration = 5 hours, that should reoccur
%% daily, with a count = 100 - how can the occurrences be
%% pre-calculated ? there is no way to predict when leap seconds
%% should be inserted ! - the earth slowdown is somewhat random
%%
%%--------------------------------------------------------------------
%% Usage of "Freq" and "Interval":
%%
%% * the two parameters above are used as a step length to determine
%%   when the next time range will occur, with "dtstart" as base
%%   offset
%% * the length of a time range is = "duration" |
%%   "dtend" - "dtstart" + 1 second (so that the dtend second is
%%   included)
%% * there are some cases when a time range will contain non-legal
%%   values (some date-time points - seconds, will not exist), for
%%   example leap days, transitions to DST or the 31:th in some
%%   months.
%%   These time ranges will usually be shrunk to fit the actual range
%%   i.e. all legal date-time points (seconds) will still be checked
%%   against, the "time" conditions in a "time-switch".
%% * this means that some ranges like 2:00:00 - 3:00:00 (assuming
%%   swedish transition to DST) can end up with degenerated ranges,
%%   in this case the range 3:00:00 - 3:00:00
%% * the transition from DST to regular time is somewhat special, as
%%   floating times will report the same time twice. A time range
%%   [T1,T2] occurring during this transition period
%%   (UTC 0:00:00 - 2:00:00, set floating back 1 hour at UTC 1:00:00)
%%   has a TX(DST) and a TX(no DST) time value, that map to different
%%   UTC values, but to the same floating time value.
%%   To ensure that the range [T1,T2] is unbroken i.e. only occurs
%%   once, the range will be interpreted as [T1(DST),T2(no DST)].
%%   Note, that this will increase the length of the time range.
%%
%%--------------------------------------------------------------------
%% Example of ending DST:
%% The example below assumes that clock is moved back to 2:00:00 when
%% 2:59:59 should become 3:00:00 (swedish DST example) - there are
%% two cases:
%%
%% 1) a time interval ending in the 2:00:00 - 2:59:59 interval
%% 2) and one starting in that interval
%%
%% IntervalA = [... , 2:20:00]
%%
%% Wallclock  2:00:00 - 2:20:00 (still DST)
%% UTC        1:00:00 - 1:20:00
%%
%% Wallclock  2:20:01 - 2:59:59 (still DST) - not in IntervalA
%% UTC        1:20:01 - 1:59:59
%%
%% Wallclock  2:00:00 - 2:20:00 (no DST)
%% UTC        2:00:00 - 2:20:00
%%
%% IntervalB = [2:40:00 , ...]
%%
%% Wallclock  2:40:00 - 2:59:59 (still DST)
%% UTC        1:40:00 - 1:59:59
%%
%% Wallclock  2:00:00 - 2:39:39 (no DST) - not in IntervalB
%% UTC        2:00:00 - 2:39:39
%%
%% Wallclock  2:40:00 - ... (no DST)
%% UTC        2:40:00 - ...
%%
%% This time-switch implementation treats these kind of
%% discontinuous ranges as the shortest possible range containing
%% both the discontinuous ranges.
%% case 1):
%% Wallclock ... 2:00:00 (DST = true) - 2:20:00 (DST = false)
%% UTC       ... 1:00:00 - 2:20:00
%%
%% case 2):
%% Wallclock 2:40:00 (DST = true) - 2:40:00 ... (DST = false)
%% UTC       1:40:00 - 2:40:00 ...
%%
%%--------------------------------------------------------------------

%% XXX a number of data-time and "count" handling operations are
%%     O(N) (N = current_time - dtstart). Which should be possible to
%%     make O(1) (e.g. removing used time_ranges entries from
%%     time_switch__cond_x records) or at least improve by a
%%     noticeable constant. calendar.erl has a number of internal
%%     functions that can probably be adapted for this purpose.
%% XXX are Timezones handled properly, so that they work if they
%%     where supported ? They are currently passed along in function
%%     calls in a somewhat haphazard fashion
%%--------------------------------------------------------------------

-module(interpret_time).

%% -compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 in_time_range/2,
	 in_time_range_test/3,
	 get_count_ranges_5/2,
	 get_count_ranges_7/2,
	 get_count_ranges_8/2,
	 safe_local_time_to_universal_time_dst/1,
	 sort_byparam/1,
	 get_lowest_level/2,
	 level_type/1,
	 is_bysetpos_usable/1,

	 test/0,

	 %% used in external test module interpret_time_test.erl
	 local_usec/0,
	 in_usec_range/3,
	 in_time_range_5/3,
	 in_time_range/3,
	 get_start_level/2,
	 get_level_params/2,
	 get_next_level/3,
	 match_param/4,
	 is_reoccurrence/2,
	 create_startpoints/3,
	 create_startpoints/4,
	 get_next_process_level/3,
	 byxxx_match/2,
	 get_set/2,
	 nth/2,
	 get_bysetpos/2,
	 is_start_time_valid_bysetpos/2,
	 is_current_time_valid_bysetpos/2
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([

        ]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("cpl.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% @time_spec() = integer() | {pseudo_usec, Time}. Time spec.


%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    (Timezone, TimeSwitchCond) -> true | false
%%
%%            Timezone       = term()
%%            TimeSwitchCond = term()
%%
%% @doc     determine if the time period specified by the
%%          TimeSwitchCond is currently occurring Note : local_usec()
%%          uses calendar:local_time(), which relies on the OS to
%%          return a proper DateTime with correct DST
%% @end
%%--------------------------------------------------------------------

in_time_range(Timezone, TimeSwitchCond) ->
    Current = local_usec(),
    in_time_range(Timezone, TimeSwitchCond, Current).

%% function useful in test_backend to allow test cases to pass a custom time
in_time_range_test(Timezone, TimeSwitchCond, Current) ->
    in_time_range(Timezone, TimeSwitchCond, Current).

%% time attribute combinations
%% dtstart + dtend    -> single range
%% dtstart + duration -> single range

in_time_range(Timezone, TimeSwitchCond, Current) when record(TimeSwitchCond, time_switch__cond_2) ->
    DTStart = time_switch:get_dtstart(TimeSwitchCond),
    Start = ts_datetime:datetime_to_usec(start, Timezone, DTStart),
    End = case time_switch:get_dtend_duration(TimeSwitchCond) of
	      {dtend, Time} ->
		  ts_datetime:datetime_to_usec(stop, Timezone, Time);
	      {duration, Duration} ->
		  ts_datetime:datetime_to_usec(stop, Timezone,
					       ts_datetime:add_datetime_and_duration(DTStart, Duration))
	  end,
    in_usec_range(Start, End, Current);

%%--------------------------------------------------------------------
%% dtstart is used as offset for the reoccurring <interval>*<freq> periods
%% duration is used to determine the end of a reoccurring period
%%
%% dtstart + duration + freq -> every <freq> timeperiod there is a <duration> long period
%% dtstart + duration + freq + interval ->
%%    every <interval>*<freq> timeperiod there is a <duration> long period (there is no until/count so never stop)
%% dtstart + duration + freq + interval + until->
%%    every <interval>*<freq> timeperiod there is a <duration> long period until current time > <until>
%% dtstart + duration + freq + interval + count->
%%    every <interval>*<freq> timeperiod range is a <duration> long period, only the first <count> periods are checked

in_time_range(Timezone, TimeSwitchCond, Current) when record(TimeSwitchCond, time_switch__cond_5) ->
    in_time_range(Timezone, TimeSwitchCond, Current, fun in_time_range_5/3);

%%--------------------------------------------------------------------
%% bysecond        - in min
%% byminute        - in hour
%% byhour          - in day
%% byday           - in week
%% byday      -/+N - nth day in month or year
%% byday         N - every day N in month or year
%% bymonthday N/-N - nth day for start or end of month
%% byyearday  N/-N - nth day for start or end of year
%% byweekno   N/-N - nth day for start or end of year, wkst determines start of week
%% bymonth         - month of year

in_time_range(Timezone, TimeSwitchCond, Current) when record(TimeSwitchCond, time_switch__cond_7) ->
    in_time_range(Timezone, TimeSwitchCond, Current, fun in_time_range_7/3);

%%--------------------------------------------------------------------
in_time_range(Timezone, TimeSwitchCond, Current) when record(TimeSwitchCond, time_switch__cond_8) ->
    case in_time_range(Timezone, TimeSwitchCond, Current, fun in_time_range_7/3) of
	%% Current is part of set used by "bysetpos"
	%% now check that Current is a date-time selected by a "bysetpos" index
	true ->
	    is_current_time_valid_bysetpos(TimeSwitchCond, Current);
	false ->
	    false
    end.


%% in_time_range/3 help function
%% determine which type of processing the TimeSwitchCond needs
in_time_range(Timezone, TimeSwitchCond, Current, InTimeRangeFun) ->
    Start = ts_datetime:datetime_to_usec(start, Timezone, time_switch:get_dtstart(TimeSwitchCond)),
    case time_switch:get_until_count(TimeSwitchCond) of
	%% check for reocurrence intervals in a limited span of time - [Start, UUntil] range
	{until, Time} ->
	    UUntil = case Time of
			 Time when is_record(Time, date_time) ->
			     ts_datetime:datetime_to_usec(stop, Timezone, Time);
			 {_Year, _Month, _Day} ->
			     %% set H, M and S to second before Year, Month, Day + 1
			     H = 23,
			     M = 59,
			     S = 59,
			     ts_datetime:datetime_to_usec(stop, Timezone,
							  #date_time{date = Time, time = {H,M,S}, type = floating})
		     end,

	    %% check if Current is in the [Start, UUntil] interval - the interval where reoccurrences can occur
	    case in_usec_range(Start, UUntil, Current) of
		true ->
		    InTimeRangeFun(Current, Timezone, TimeSwitchCond);
		false ->
		    false
	    end;
	%% reoccurrence starts at Start and goes on forever
	repeat_forever ->
	    InTimeRangeFun(Current, Timezone, TimeSwitchCond);
	%% reoccurrence starts at Start and stops after occurring Count times
	{count, _Count} ->
	    count_time_range(Current, Timezone, TimeSwitchCond)
    end.

%%--------------------------------------------------------------------
%% @spec    (Timezone, TimeSwitchCond) ->
%%            [{Start, Stop}]
%%
%%            Timezone       = term() "currently unused"
%%            TimeSwitchCond = #time_switch__cond_5{}
%%
%%            Start = time_spec()
%%            Stop  = time_spec()
%%
%% @doc     get all ranges specified by the "count" attribute in a
%%          "time-switch"
%% @end
%%--------------------------------------------------------------------
get_count_ranges_5(Timezone, TimeSwitchCond) when is_record(TimeSwitchCond, time_switch__cond_5) ->
    Start = time_switch:get_dtstart(TimeSwitchCond),
    Duration = ts_duration:duration(TimeSwitchCond),
    {count, Count} = time_switch:get_until_count(TimeSwitchCond),
    Ranges = get_count_range_5(Timezone, Count, 0, Start, Duration, TimeSwitchCond, []),
    Ranges.

get_count_range_5(_Timezone, 0, _Iter, _Start, _Duration, _TimeSwitchCond, Ranges) ->
    lists:reverse(Ranges);

get_count_range_5(Timezone, Count, Iter, Start, Duration, TimeSwitchCond, Ranges) ->
    S = add_freq_step(TimeSwitchCond, Start, Iter),

    %% note: valid_range(...) is only needed when dealing with functions that can
    %%       create non-legal dates, like unsafe_add_xxx(...)
    case valid_range(Timezone, S, Duration) of
	no_range ->
	    get_count_range_5(Timezone, Count, Iter + 1, Start, Duration, TimeSwitchCond, Ranges);
	Range ->
	    get_count_range_5(Timezone, Count - 1, Iter + 1, Start, Duration, TimeSwitchCond, [Range | Ranges])
    end.

%%--------------------------------------------------------------------
%% @spec    (Timezone, TimeSwitchCond) ->
%%            [{Start, Stop}]
%%
%%            Timezone       = term() "currently unused"
%%            TimeSwitchCond = #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%
%%            Start = time_spec()
%%            Stop  = time_spec()
%%
%% @doc     get all ranges specified by the "count" attribute in a
%%          "time-switch"
%% @end
%%--------------------------------------------------------------------
get_count_ranges_7(Timezone, TimeSwitchCond) when is_record(TimeSwitchCond, time_switch__cond_7);
						  is_record(TimeSwitchCond, time_switch__cond_8) ->
    Duration = ts_duration:duration(TimeSwitchCond),
    TabId = count_create(),
    StartPoints = generate_counts(TimeSwitchCond, TabId),
    count_delete(TabId),
    %% io:format("StartPoints = ~p~n",[StartPoints]), %DDD
    %% this call will never remove entries as validity has already been checked
    %% it's only used to create the {Start, Stop} return tuples
    [valid_range(Timezone, S, Duration) || S <- StartPoints].

%% return: list() of date_time record()
generate_counts(TimeSwitchCond, TabId) ->
    Freq = time_switch:get_freq(TimeSwitchCond),
    ByParams = sort_byparam(time_switch:get_by_values(TimeSwitchCond)),
    LowestLevel = get_lowest_level(Freq, ByParams),
    {count, Count} = time_switch:get_until_count(TimeSwitchCond),
    {Year,_,_} = (time_switch:get_dtstart(TimeSwitchCond))#date_time.date,
    %% io:format("Freq ~p, ByParams ~p, LowestLevel ~p, Year ~p~n",[Freq,ByParams,LowestLevel,Year]), %DDD
    generate_counts(yearly, LowestLevel, TimeSwitchCond, ByParams, Year, Count, TabId).

%%--------------------------------------------------------------------
%% @spec    (Timezone, TimeSwitchCond) ->
%%            [{Start, Stop}]
%%
%%            Timezone       = term() "currently unused"
%%            TimeSwitchCond = #time_switch__cond_8{}
%%
%%            Start = time_spec()
%%            Stop  = time_spec()
%%
%% @doc     get all ranges specified by the "count" attribute in a
%%          "time-switch"
%% @end
%%--------------------------------------------------------------------
get_count_ranges_8(Timezone, TimeSwitchCond) when is_record(TimeSwitchCond, time_switch__cond_8) ->
    get_count_ranges_7(Timezone, TimeSwitchCond).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Handling bysetpos:
%%
%% http://www.imc.org/ietf-calendar/mail-archive discusses the
%% handling of BYSETPOS (in iCalendar), see the threads
%% "Can we fix BYSETPOS" (2001-06-08 +) ,"CPL: Summary of what it is"
%% (2001-08-08 +) and "BYSETPOS: Fresh start" (2001-07-26 +).
%%
%% The basic consensus appears to be that bysetpos=N, selects the
%% Nth entry, from start or end of a set.
%% The set consists of all start points inside the range of a "freq"
%% reoccurrence, e.g. if:
%% dtstart="2000-01-01 00:00:00", freq="monthly" and interval="4"
%%
%% implies that the start point sets are found in the ranges:
%% [yyyy-01-01 00:00:00, yyyy-01-31 23:59:59]
%% [yyyy-05-01 00:00:00, yyyy-05-31 23:59:59]
%% [yyyy-09-01 00:00:00, yyyy-09-30 23:59:59]
%%
%% the byxxx parameters in a "time" tag will expand or constrain the
%% freq based sets - byxxx > freq constrains and byxxx < freq extends
%% a set into subsets.
%% There are several issues with the bysetpos specification in
%% RFC 2445/3880:
%% * bysetpos range is limited to [-366,-1] and [1,366], so large sets
%%   can't be fully accessed - it has been proposed (see the mailing
%%   list) that the range should be extended to 31622401
%%   (366*24*60*60 + 1 leap second)
%% * it is possible to define very large sets; freq="yearly",
%%   byyearday="1,...366", byhour="0,...23", byminute="0,...59" and
%%   bysecond="0,...59" - it has been proposed (see the mailing
%%   list) to limit bysetpos to _date_ values, but bysetpos has valid
%%   uses for _time_ values as well:
%%   FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYHOUR=12,13;BYSETPOS=-1,-2
%%   (get hour 12 and 13 on last workday in month)
%% * CPL requires that "is current time = nth bysetpos ?" can be
%%   determined in O(1) time, for efficient handling of phone calls.
%%   O(N) is acceptable if it can be ensured that N is small.
%%
%% The primary use for "bysetpos", the only use for which "time" tags
%% can't be rewritten with other "time" tag attributes, is to select a
%% start point from a set that can vary in size, like days in month
%% below:
%% freq="month", byday="MO,TU,WE,TH,FR" and bysetpos="-1" ("the last
%% work day of the month"). byday, bymonthday, byweekno and byyearday
%% have the possibility to create such sets.
%%
%% Note: none of the proposed fixes / clarifications / corrections
%%       discussed in the mailing list appear to have been added to
%%       the RFCs (- hsten 2005-03-16).
%%
%% possible solutions:
%% * create size limited (366) set with the count handling functions
%%   for time_switch__cond_7 records. Place no other limitations on
%%   bysetpos usage in "time" tag.
%% - hard / impossible to verify that set will never become > 366
%%   elements. If a set is truncated to 366 this will mean
%%   that bysetpos="-1" (or "1") may not refer to the last (first)
%%   element
%% * constrain the combination of byxxx and bysetpos values:
%% o only allow bymonth - byday (date values)
%% o only allow certain byxxx-freq combinations e.g. byday - days in
%%   month (freq=monthly), byyearday - days in year (freq=yearly), ...
%% o ...


%%--------------------------------------------------------------------
%% A constrained O(1) bysetpos implementation:
%% any choice of freq and byxxx is valid as long as set size will be
%% =< 366, it is sufficient to check freq against the lowest ranked
%% byxxx parameter (or freq in the degenerated case) to determine if
%% freq and byxxx parameters can be combined.
%%
%% Lowest Level - ranking
%% is from highest to lowest
%% rank
%% (freq)	(byxxx)         suitable set range (valid freq)
%% - - - - - - - - - - - -
%% yearly                       - [1]
%% monthly                      - [1]
%% 		bymonth		yearly
%% weekly                       weekly
%% daily                        daily
%% 		byweekno        yearly, monthly, weekly, daily
%% 		byyearday       yearly, monthly, weekly, daily
%% 		bymonthday      yearly, monthly, weekly, daily
%% 		byday           yearly, monthly, weekly, daily
%% hourly                       hourly
%% 		byhour          hourly
%% minutely                     minutely
%% 		byminute        minutely
%% secondly                     secondly
%% 		bysecond        secondly
%%
%% [1]: can't be lowest level, rank is > any byxxx rank
%%--------------------------------------------------------------------
is_bysetpos_usable(TimeSwitchCond) ->
    Freq = time_switch:get_freq(TimeSwitchCond),
    ByParams = time_switch:get_by_values(TimeSwitchCond),
    LowestLevel = get_lowest_level(Freq, ByParams),
    case {Freq, LowestLevel} of
	{yearly, bymonth} -> true;
	{yearly, byweekno} -> true;
	{yearly, byyearday} -> true;
	{yearly, bymonthday} -> true;
    	{yearly, byday} -> true;

	{monthly, byweekno} -> true;
	{monthly, byyearday} -> true;
	{monthly, bymonthday} -> true;
	{monthly, byday} -> true;

	{weekly, weekly} -> true;
	{weekly, byweekno} -> true;
	{weekly, byyearday} -> true;
	{weekly, bymonthday} -> true;
	{weekly, byday} -> true;

	{daily, daily} -> true;
	{daily, byweekno} -> true;
	{daily, byyearday} -> true;
	{daily, bymonthday} -> true;
	{daily, byday} -> true;

	{hourly, hourly} -> true;
	{hourly, byhour} -> true;

	{minutely, minutely} -> true;
	{minutely, byminute} -> true;

	{secondly, secondly} -> true;
	{secondly, bysecond} -> true;

	_ -> false
    end.

%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond, Current) -> true | false
%%
%%            TimeSwitchCond = #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%            Current        = integer() "gregorian seconds (UTC), system utc time from functions like local_usec/0, which will always be valid"
%%
%% @doc     determine if Current is a time that is part of the
%%          [StartPoint, StartPoint + Duration] range where
%%          StartPoint is a start point selected by the "bysetpos"
%%          parameter (there may be several start points to check).
%%          Note : Current is used to determine which (freq)
%%          reoccurrence should be used to create the start point
%%          set, from which bysetpos selects start points
%% @end
%%--------------------------------------------------------------------
is_current_time_valid_bysetpos(TimeSwitchCond, Current) ->
    Timezone = dummy,
    Time = gsec_to_datetime(Current, floating, Timezone),
    %% io:format("Time ~p~n",[Time]), %DDD
    %% examine the possible startpoints in the current reoccurrence
    case get_bysetpos(TimeSwitchCond, Time) of
	[] ->
	    %% io:format("1.~n"), %DDD
	    false;
	%% length =< number of bysetpos indexes
	StartTimes ->
	    %% io:format("2.~n"), %DDD
	    %% io:format("StartTimes ~p~n",[StartTimes]), %DDD
	    Timezone = dummy,
	    Duration = ts_duration:duration(TimeSwitchCond),
	    F = fun(S) ->
			%% io:format("S ~p~n",[S]), %DDD
			%% note: valid_range/3 is used for it's range creation functionality,
			%% StartTimes shouldn't contain date_time records with invalid dates
			case valid_range(Timezone, S, Duration) of
			    no_range ->
				%% io:format("3.~n"), %DDD
				false;
			    {US, UE} ->
				%% io:format("4.~n"), %DDD
				in_usec_range(US, UE, Current)
			end
		end,
	    lists:any(F, StartTimes)
    end.

%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond, StartTime) -> true | false
%%
%%            TimeSwitchCond = #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%            StartTime      = #date_time{} "dates are assumed to be valid dates (start points)"
%%
%% @doc     takes a start time generated by "count" preprocessing and
%%          determines if such a date-time is also a valid occurrence
%%          of bysetpos. This will only work properly if both "count"
%%          handling and is_start_time_valid_bysetpos/2 use the same
%%          TimeSwitchCond. Note : Current is used to determine which
%%          (freq) reoccurrence should be used to create the start
%%          point set, from which bysetpos selects start points Note
%%          : the handling of bysetpos for "count" preprocessing is
%%          currently O(N*S), N = number of counts, S = size of
%%          bysetpos set (`=<' 366)
%% @end
%%--------------------------------------------------------------------
is_start_time_valid_bysetpos(TimeSwitchCond, StartTime) ->
    case get_bysetpos(TimeSwitchCond, StartTime) of
	[] ->
	    false;
	StartTimes ->
	    %% StartTime and StartTimes are both members of the freq
	    %% reoccurrence set, so StartTime is valid if StartTime is
	    %% a member of the StartTimes set
	    F = fun(E) -> E == StartTime end,
	    lists:any(F, StartTimes)
    end.


%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond, Time) -> [#date_time{}]
%%
%%            TimeSwitchCond = #time_switch__cond_8{}
%%            Time           = #date_time{} "valid date is assumed"
%%
%% @doc     get all start points specified by "bysetpos" in certain
%%          freq reoccurrence, specified by Time
%% @end
%%--------------------------------------------------------------------
get_bysetpos(TimeSwitchCond, Time) ->
    StartPoints = get_set(Time, TimeSwitchCond),
    %% io:format("StartPoints ~p~n",[StartPoints]), %DDD
    F = fun(N, Acc) ->
		case nth(StartPoints,N) of
		    '#not_found' ->
			Acc;
		    E ->
			[E | Acc]
		end
	end,
    R = lists:foldl(F, [], time_switch:get_bysetpos(TimeSwitchCond)),
    %% remove duplicats, different N / -M may refere to the same date-time
    lists:usort(R).

%%--------------------------------------------------------------------
%% @spec    (DT, TimeSwitchCond) -> [#date_time{}]
%%
%%            DT             = #date_time{}
%%            TimeSwitchCond = #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%
%% @doc     get the freq reoccurrence set of start points, this is
%%          achived by jumping into the middle of generate_counts/7
%%          based on freq (supplied by TimeSwitchCond), DT determines
%%          which freq reoccurrence that is examined. Note : this
%%          function is somewhat ugly, as it may skips several levels
%%          in generate_counts(...) to get the proper branch and by
%%          accessing the start point accumulation (ets) table
%%          directly, all which requires extra setup and clean up
%%          Note :
%% @end
%%--------------------------------------------------------------------
get_set(DT, TimeSwitchCond) ->
    Freq = time_switch:get_freq(TimeSwitchCond),
    Time = dt_to_partial_init(DT, Freq), %% always at least initiated with year
    MaxCount = get_set,
    TabId = count_create(),
    ByParams = time_switch:get_by_values(TimeSwitchCond),
    LowestLevel = get_lowest_level(Freq, ByParams),

    %% check if freq < than all byxxx, if so there will only be one possible start point
    case LowestLevel == Freq of
	true ->
	    DtStart = time_switch:get_dtstart(TimeSwitchCond),
	    [add_dtstart_modifiers(Time, DtStart)];
	false ->
	    %% enter generate_counts at the level below Freq (as all generator
	    %% related initiation has already been done)
	    Level = get_next_level(Freq),
	    FreqRank = get_rank(Freq),
	    CurrentByParams = [Param || Param = {ByType, _Val} <- ByParams,
					get_rank(ByType) < FreqRank],
	    %%      io:format("get_set: Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,"
	    %% 	       "~nCurrentByParams ~p~nTime ~p,~nMaxCount ~p, TabId ~p~n",
	    %% 	       [Level, LowestLevel, TimeSwitchCond, CurrentByParams, Time, MaxCount, TabId]), %DDD

	    generate_counts(Level, LowestLevel, TimeSwitchCond, CurrentByParams, Time, MaxCount, TabId),
	    StartPoints = count_get_all(TabId),
	    count_delete(TabId),
	    StartPoints
    end.

%% create a partialy initiated date-time used in generate_counts(...)
%% dt_to_partial_init(DT, Freq)
dt_to_partial_init(DT, yearly) ->
    {Y,_M,_D} = DT#date_time.date,
    DT#date_time{date = {Y,undefined,undefined}, time = {undefined,undefined,undefined}};
dt_to_partial_init(DT, monthly) ->
    {Y,M,_D} = DT#date_time.date,
    DT#date_time{date = {Y,M,undefined}, time = {undefined,undefined,undefined}};
%% weekly = daily (with Interval * 7)
dt_to_partial_init(DT, weekly) ->
    {Y,M,D} = DT#date_time.date,
    DT#date_time{date = {Y,M,D}, time = {undefined,undefined,undefined}};
dt_to_partial_init(DT, daily) ->
    {Y,M,D} = DT#date_time.date,
    DT#date_time{date = {Y,M,D}, time = {undefined,undefined,undefined}};
dt_to_partial_init(DT, hourly) ->
    {H,_M,_S} = DT#date_time.time,
    DT#date_time{time = {H,undefined,undefined}};
dt_to_partial_init(DT, minutely) ->
    {H,M,_S} = DT#date_time.time,
    DT#date_time{time = {H,M,undefined}};
dt_to_partial_init(DT, secondly) ->
    {H,M,S} = DT#date_time.time,
    DT#date_time{time = {H,M,S}}.


%%--------------------------------------------------------------------
%% @spec    (L,N) ->
%%            term() | '#not_found'
%%            "'#not_found' if N refers to position beyond the elements in L"
%%
%%            L = [term()]
%%            N = integer() "N > 0 or N < 0"
%%
%% @doc     get elemenet N in list L. N = 1 is the index of the first
%%          element, N = 2 the second and so on. Negative indexes are
%%          count from the end rather than the front of L.
%% @end
%%--------------------------------------------------------------------
nth(L,N) when N > 0 ->
    nth2(L,N);
nth(L,N) when N < 0 ->
    nth2(lists:reverse(L),-N).

nth2([], _N) -> '#not_found';
nth2([E | _], 1) -> E;
nth2([_|List], N) ->
    nth2(List, N-1).


%%--------------------------------------------------------------------
%% generate_count_ranges_7 works in a similar fashion as
%% "Processing time_switch__cond_7", the generators are used to
%% create a search tree of start points. Filters and interval+dtstart
%% values for generators, are used to determine which tree branches
%% need to be examined.
%% The search tree is scanned depth first - this ensures that the
%% start points are found in order.
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @spec    (Level, LowestLevel, TimeSwitchCond, ByParams, TimeYear,
%%          MaxCount, TabId) -> [#date_time{}]
%%
%%            Level          = atom() "the current byxxx / freq level,"
%%            LowestLevel    = atom() "the lowest byxxx / freq level supplied in TimeSwitchCond - the level to stop the tree search at"
%%            TimeSwitchCond = #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%            ByParams       = term() "by_values from TimeSwitchCond, stores the currently unprocessed ones  ones byxxx less than or equal to Level "
%%            TimeYear       = Time | Year
%%            Time           = #date_time{} "partialy initiated time value - a path through the search tree"
%%            Year           = #date_time{} "partialy initiated time value - a path through the search tree"
%%            MaxCount       = integer() | get_set "number of start points to retrieve from time dtstart or get_set to retrieve a search tree  branch - used by get_set/2"
%%            TabId          = term() "from count_create/0"
%%
%% @doc     get the MaxCount first start point occurrences in
%%          TimeSwitchCond Note : the code is also reused to
%%          implement get_set/2 - this adds some extra complexity
%%          (conditional checks on MaxCount) see get_set/2 for more
%%          details
%% @end
%%--------------------------------------------------------------------

%% YEARLY - - - - - - - - - - - - - - - - - -
%% note: The Time (Year) argument is non-standard in that it is used to pass the current year to search,
%%       rather than a partially initialized, date_time record(), used to build start points from.
%%       This is used to so that Year can be incremented indefinitely
generate_counts(yearly = Level, LowestLevel, TimeSwitchCond, ByParams, Year, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Year ~p, MaxCount ~p~n",
    %% [Level, LowestLevel, TimeSwitchCond, ByParams, Year, MaxCount]), %DDD

    {StartYear,_,_} = (time_switch:get_dtstart(TimeSwitchCond))#date_time.date,
    {ok, LookAHead} = yxa_config:get_env(cpl_time_switch_count_max_lookahead),
    %%
    case ((Year - StartYear) > LookAHead) and (is_integer(MaxCount)) of
	true ->
	    throw({error, failed_to_get_count_no_of_start_points_during_lookahead_time});
	false ->
	    {NextLevel, _NextProcessLevel, _NextLevelByParams} = get_next_level(Level, ByParams, TimeSwitchCond),
	    %% io:format("NextLevel ~p~n",[NextLevel]), %% DDD
	    Time = #date_time{date = {Year, undefined, undefined},
			      time = {undefined, undefined, undefined}, type = floating},
	    Fun = fun(FNewTime) ->
			  generate_counts(NextLevel, LowestLevel, TimeSwitchCond, ByParams,
					  FNewTime, MaxCount, TabId) end,
	    try
		begin
		    %% look for start points in current year
		    limited_df(Fun, [Time], MaxCount, TimeSwitchCond),

		    %% io:format("tab: ~p~n",[ets:tab2list(TabId)]), %%DDD

		    case MaxCount of
			%% get_set/2 never retrieves more that one year
			%% this is a special check needed only to deal with
			%% the special Year handling in this clause
			get_set ->
			    ok;
			%% CurrentCount < MaxCount
			%% generate a new start year
			_ when is_integer(MaxCount) ->
			    NextYear = case time_switch:get_freq(TimeSwitchCond) of
					   yearly -> Year + time_switch:get_interval(TimeSwitchCond);
					   _ -> Year + 1
				       end,
			    generate_counts(yearly, LowestLevel, TimeSwitchCond, ByParams, NextYear,
					    MaxCount, TabId)
		    end
		end
	    catch
		throw: {finished, count_reached} ->
		    %% got MaxCount startpoints
		    count_get_all(TabId)
	    end
    end;

%% BYMONTH - - - - - - - - - - - - - - - - - -
generate_counts(bymonth = Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    {_Y,M,_D} = Time#date_time.date,
    Match = M,
    F = fun({bymonth, Val}) ->
		Val == Match
	end,
    count_filter(F, Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId);


%% BYWEEKNO - - - - - - - - - - - - - - - - - -
generate_counts(byweekno = Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    %% extra work is needed to determine if any of the days in the range [Time, Time + Duration]
    %% are part of the weeks specified by ByParams - this is done by figuring out which
    %% weeks are part of the [Time, Time + Duration] range
    {Year,_M,_D} = Date = Time#date_time.date,
    Duration = ts_duration:duration(TimeSwitchCond),
    TempStart = Time#date_time{time = {0,0,0}}, % supply values for undefined fields
    EndDate = (ts_datetime:add_datetime_and_duration(TempStart, Duration))#date_time.date,
    Wkst = time_switch:get_wkst(TimeSwitchCond),
    {_, StartWeekno} = ts_date:date_to_weekno(Date, Wkst),
    {_, EndWeekno} = ts_date:date_to_weekno(EndDate, Wkst),
    Weeks = lists:seq(StartWeekno, EndWeekno),

    F = fun({byweekno, Val}) ->
		WeekNo = ts_date:get_week_no(Year, Wkst, Val),
		lists:member(WeekNo, Weeks) % if WeekNo = nth_week_does_not_exits -> false
	end,
    count_filter(F, Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId);

%% BYYEARDAY - - - - - - - - - - - - - - - - - -
generate_counts(byyearday = Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    {Year,_M,_D} = Date = Time#date_time.date,
    DayNo = ts_date:date_to_dayno(Date),
    F = fun({byyearday, Val}) ->
		DayNo == ts_date:normalize_yearday(Year, Val)
	end,
    count_filter(F, Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId);

%% BYMONTHDAY - - - - - - - - - - - - - - - - - -
generate_counts(bymonthday = Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    {Year,Month,MDay} = Time#date_time.date,
    F = fun({bymonthday, Val}) ->
		MDay == ts_date:normalize_monthday(Year, Month, Val)
	end,
    count_filter(F, Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId);


%% BYDAY - - - - - - - - - - - - - - - - - -
generate_counts(byday = Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    {Year,Month,_MDay} = Date = Time#date_time.date,
    TimeWeekDay = ts_date:date_to_weekday(Date),
    F = fun({byday, Val}) ->
		case Val of
		    {all, WeekDay} ->
			WeekDay == TimeWeekDay;
		    {N, WeekDay} ->
			ts_date:nth_byday_in_month(Year,Month,N,WeekDay) == Date
		end
	end,
    count_filter(F, Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId);


%% BYHOUR - - - - - - - - - - - - - - - - - -
generate_counts(byhour = Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    {Hour,_Min,_Sec} = Time#date_time.time,
    F = fun({byhour, Val}) ->
		Val == Hour
	end,
    count_filter(F, Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId);


%% BYMINUTE - - - - - - - - - - - - - - - - - -
generate_counts(byminute = Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    {_Hour,Min,_Sec} = Time#date_time.time,
    F = fun({byminute, Val}) ->
		Val == Min
	end,
    count_filter(F, Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId);


%% BYSECOND - - - - - - - - - - - - - - - - - -
%% generate_counts(bysecond = Level, _LowestLevel, _TimeSwitchCond, ByParams,
%% Time, _MaxCount, _CurrentCount, _TimeAcc) ->
generate_counts(bysecond = Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    {_Hour,_Min,Sec} = Time#date_time.time,
    {CurrentLevelParams, _LowerLevelParams} = get_level_params(Level, ByParams),
    F = fun({bysecond, Val}) ->
		Val == Sec
	end,
    case valid_level_byparam(F, CurrentLevelParams) of
	true ->
	    %% dummy is supplied as fun as Continue Level == LowestLevel in this clause
	    is_gen_done(TimeSwitchCond, Time, Level, LowestLevel, MaxCount, TabId, dummy);
	false ->
	    ok
    end;

%% GENERATORS - - - - - - - - - - - - - - - - - -
%% Level = monthly | weekly | daily | hourly | minutely | secondly
generate_counts(Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
    %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    %% get valid times (search branches)
    NewTimes = get_new_times(Level, TimeSwitchCond, Time),
    %% io:format("NewTimes ~p", [NewTimes]), %DDD
    is_gen_done(TimeSwitchCond, NewTimes, Level, LowestLevel, MaxCount, TabId,
		fun() ->
			%% ByParams    :  are only processed by byxxx - so there is no need to update them
			%% CurrentCount:
			%% TimeAcc     :
			{NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
			Fun = fun(FNewTime) ->
				      generate_counts(NextLevel, LowestLevel, TimeSwitchCond, ByParams, FNewTime,
						      MaxCount, TabId) end,
			limited_df(Fun, NewTimes, MaxCount, TimeSwitchCond)
		end).



%% check if time range [Start, Start + Duration] is valid
%% and that Start >= dtstart (first possible start point occurrence)
is_valid(Start, TimeSwitchCond) ->
    Timezone = dummy,
    Duration = ts_duration:duration(TimeSwitchCond),

    DtStart = time_switch:get_dtstart(TimeSwitchCond),
    case ts_datetime:le_datetime(Timezone, DtStart, Start) of
	true ->
	    case valid_range(Timezone, Start, Duration) of
		no_range -> false;
		_Range -> true
	    end;
	false ->
	    false
    end.

%% TimeOrTimesList = Time | list() of date_time record()
%% Time            = date_time record() | removed_by_filter
%% Check if generate_counts(...) should terminate or if it should continue by calling Continute()
is_gen_done(TimeSwitchCond, TimeOrTimesList, Level, LowestLevel, MaxCount, TabId, Continue) ->
    DtStart = time_switch:get_dtstart(TimeSwitchCond),
    %% no more generator/filter to process - add startpoints
    case LowestLevel == Level of
 	true ->
	    case level_type(Level) of
		generator ->
		    Times = [add_dtstart_modifiers(S, DtStart) || S <- TimeOrTimesList],
		    StartTimeList = [add_dtstart_modifiers(Time, DtStart)
				     || Time <- Times, is_valid(Time, TimeSwitchCond)],
		    [count_add(TabId, MaxCount, TimeSwitchCond, ST) || ST <- StartTimeList];
		filter ->
		    Time = add_dtstart_modifiers(TimeOrTimesList, DtStart),
		    case is_valid(Time, TimeSwitchCond) of
			true -> count_add(TabId, MaxCount, TimeSwitchCond, Time);
			false -> ok
		    end
	    end;
	false ->
	    Continue()
    end.


%% process a byxxx filter
count_filter(MatchFilter, Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount, TabId) ->
    %% io:format(">> Filter: Level ~p, LowestLevel ~p,~nTimeSwitchCond ~p,~nByParams ~p, Time ~p, MaxCount ~p~n",
 	      %% [Level, LowestLevel, TimeSwitchCond, ByParams, Time, MaxCount]), %DDD
    {CurrentLevelParams, _LowerLevelParams} = get_level_params(Level, ByParams),
    case valid_level_byparam(MatchFilter, CurrentLevelParams) of
	true ->
	    %% io:format("MatchFilter -> true~n"), %DDD
	    is_gen_done(TimeSwitchCond, Time, Level, LowestLevel, MaxCount, TabId,
			fun() ->
				{NextLevel, _NextProcessLevel, NextLevelByParams} =
				    get_next_level(Level, ByParams, TimeSwitchCond),
				generate_counts(NextLevel, LowestLevel, TimeSwitchCond, NextLevelByParams, Time,
						MaxCount, TabId)
			end);
	false ->
	    %% io:format("MatchFilter -> false~n"), %DDD
	    ok

    end.

%%--------------------------------------------------------------------
%% @spec    (MatchFilter, CurrentLevelParams) -> true | false
%%
%%            MatchFilter        = function() "fun(ByParam) - returns true | false test a certain value of a certain level against a single ByParam of the same level "
%%            ByParam            = term() "a time_switch__cond_N.by_values element (where N is 7|8) "
%%            CurrentLevelParams = [ByParam] "should be of the same level as the value tested by MatchFilter  "
%%
%% @doc     check that all (if any) byxxx values for a certain xxx
%%          level match a start time candidate tested with
%%          MatchFilter
%% @end
%%--------------------------------------------------------------------
valid_level_byparam(_MatchFilter, []) ->
    true;
valid_level_byparam(MatchFilter, CurrentLevelParams) ->
    lists:any(MatchFilter, CurrentLevelParams).


%%--------------------------------------------------------------------
%% @spec    (Level, BaseTime, Vals) -> [#date_time{}]
%%
%%            Level    = atom() "generator used to generate Vals"
%%            BaseTime = #date_time{} "acts as"
%%            Vals     = [integer()] "time value; year - second"
%%
%% @doc     get_new_times/3 support function create a list of new
%%          times based on BaseTime, by initializing the time value
%%          related to Level (monthly sets month in #date_time.date
%%          ...)
%% @end
%%--------------------------------------------------------------------
new_times(monthly, BaseTime, Vals) ->
    [begin {Y,_M,D} = BaseTime#date_time.date,
	   BaseTime#date_time{date = {Y,Val,D}}
     end || Val <- Vals];
%% same as daily
new_times(weekly, BaseTime, Vals) ->
    [begin {Y,M,_D} = BaseTime#date_time.date,
	   BaseTime#date_time{date = {Y,M,Val}}
     end || Val <- Vals];
new_times(daily, BaseTime, Vals) ->
    [begin {Y,M,_D} = BaseTime#date_time.date,
	   BaseTime#date_time{date = {Y,M,Val}}
     end || Val <- Vals];
new_times(hourly, BaseTime, Vals) ->
    [begin {_H,M,S} = BaseTime#date_time.time,
	   BaseTime#date_time{time = {Val,M,S}}
     end || Val <- Vals];
new_times(minutely, BaseTime, Vals) ->
    [begin {H,_M,S} = BaseTime#date_time.time,
	   BaseTime#date_time{time = {H,Val,S}}
     end || Val <- Vals];
new_times(secondly, BaseTime, Vals) ->
    [begin {H,M,_S} = BaseTime#date_time.time,
	   BaseTime#date_time{time = {H,M,Val}}
     end || Val <- Vals].

%%--------------------------------------------------------------------
%% @spec    (Level, ST, Time) -> true | false
%%
%%            Level = atom() "generator type"
%%            ST    = #date_time{} "partialy initialized start point that needs to be tested to see if it is  part of its 'parent' Time "
%%            Time  = #date_time{} "partialy initialized time created by traversing 'start point' tree with generate_counts(...)"
%%
%% @doc     determine if ST is a star point that is part of the Time
%%          search branch
%% @end
%%--------------------------------------------------------------------
step_in_time(monthly, ST, Time) ->
    {Y1,_,_} = Time#date_time.date,
    {Y2,_,_} = ST#date_time.date,
    Y1 == Y2;
step_in_time(weekly, ST, Time) ->
    {Y1,M1,_} = Time#date_time.date,
    {Y2,M2,_} = ST#date_time.date,
    (Y1 == Y2) and (M1 == M2);
step_in_time(daily, ST, Time) ->
    {Y1,M1,_} = Time#date_time.date,
    {Y2,M2,_} = ST#date_time.date,
    (Y1 == Y2) and (M1 == M2);
step_in_time(hourly, ST, Time) ->
    {Y1,M1,D1} = Time#date_time.date,
    {Y2,M2,D2} = ST#date_time.date,
    (Y1 == Y2) and (M1 == M2) and (D1 == D2);
step_in_time(minutely, ST, Time) ->
    D1 = Time#date_time.date,
    D2 = ST#date_time.date,
    {H1,_,_} = Time#date_time.time,
    {H2,_,_} = ST#date_time.time,
    (D1 == D2) and (H1 == H2);
step_in_time(secondly, ST, Time) ->
    D1 = Time#date_time.date,
    D2 = ST#date_time.date,
    {H1,M1,_} = Time#date_time.time,
    {H2,M2,_} = ST#date_time.time,
    (D1 == D2) and (H1 == H2) and (M1 == M2).

%%--------------------------------------------------------------------
%% @spec    (Level, Diff, Interval) -> integer()
%%
%%            Level    = weekly | term()
%%            Diff     = integer() "distance to dtstart"
%%            Interval = integer()
%%
%% @doc     determine step length (in Level type units) to first
%%          possible occurence of a start point, after Diff amount of
%%          time after dtstart
%% @end
%%--------------------------------------------------------------------
full_step(weekly, Diff, Interval) ->
    IntervalSteps = Diff div (Interval * 7), % in no. of week * Interval steps
    case Diff rem (Interval * 7) of
	0 -> IntervalSteps * Interval * 7;
	_ -> (IntervalSteps + 1) * Interval * 7
    end;

full_step(_, Diff, Interval) ->
    IntervalSteps = Diff div Interval,
    case Diff rem Interval of
	0 -> IntervalSteps * Interval;
	_ -> (IntervalSteps + 1) * Interval
    end.

%--------------------------------------------------------------------
%% @spec    (Level, DtStart, Time) -> {FirstTime, integer()}
%%
%%            Level   = monthly  |
%%                      weekly   |
%%                      daily    |
%%                      hourly   |
%%                      minutely |
%%                      secondly
%%            DtStart = term()
%%            Time    = #date_time{}
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
get_diff(monthly, DtStart, Time) ->
    Timezone = dummy,
    {Year,_,_} = Time#date_time.date,
    FirstTime = #date_time{date = {Year,1,1}, time = {0,0,0}, type = floating},
    Diff = ts_datetime:diff_datetime(Timezone, DtStart, FirstTime, monthly),
    {FirstTime, Diff};
get_diff(weekly, DtStart, Time) ->
    Timezone = dummy,
    {Year,Month,_} = Time#date_time.date,
    FirstTime = #date_time{date = {Year,Month,1}, time = {0,0,0}, type = floating},
    Diff = ts_datetime:diff_datetime(Timezone, DtStart, FirstTime, daily),
    {FirstTime, Diff};
get_diff(daily, DtStart, Time) ->
    Timezone = dummy,
    {Year,Month,_} = Time#date_time.date,
    FirstTime = #date_time{date = {Year,Month,1}, time = {0,0,0}, type = floating},
    Diff = ts_datetime:diff_datetime(Timezone, DtStart, FirstTime, daily),
    {FirstTime, Diff};
get_diff(hourly, DtStart, Time) ->
    Timezone = dummy,
    Date = Time#date_time.date,
    FirstTime = #date_time{date = Date, time = {0,0,0}, type = floating},
    Diff = ts_datetime:diff_datetime(Timezone, DtStart, FirstTime, hourly),
    {FirstTime, Diff};
get_diff(minutely, DtStart, Time) ->
    Timezone = dummy,
    Date = Time#date_time.date,
    {Hour,_,_} = Time#date_time.time,
    FirstTime = #date_time{date = Date, time = {Hour,0,0}, type = floating},
    Diff = ts_datetime:diff_datetime(Timezone, DtStart, FirstTime, minutely),
    {FirstTime, Diff};
get_diff(secondly, DtStart, Time) ->
    Timezone = dummy,
    Date = Time#date_time.date,
    {Hour,Minute,_} = Time#date_time.time,
    FirstTime = #date_time{date = Date, time = {Hour,Minute,0}, type = floating},
    Diff = ts_datetime:diff_datetime(Timezone, DtStart, FirstTime, secondly),
    {FirstTime, Diff}.

%%--------------------------------------------------------------------
%% @spec    (Level, DtStart, Steps) -> #date_time{}
%%
%%            Level   = monthly  |
%%                      weekly   |
%%                      daily    |
%%                      hourly   |
%%                      minutely |
%%                      secondly
%%            DtStart = #date_time{}
%%            Steps   = integer()
%%
%% @doc     add Level * Steps amount of time to dtstart
%% @end
%%--------------------------------------------------------------------
add_steps(monthly, DtStart, Steps) ->
    %% is_gen_done(...) calls is_valid(...) before adding a starting
    %% point to the start point table, so invalid start points will
    %% never be part of the "count" ranges generated by
    %% get_count_ranges_7/2
    ts_datetime:unsafe_add_months(DtStart, Steps);
add_steps(weekly, DtStart, Steps) ->
    ts_datetime:add_days(DtStart, Steps);
add_steps(daily, DtStart, Steps) ->
    ts_datetime:add_days(DtStart, Steps);
add_steps(hourly, DtStart, Steps) ->
    ts_datetime:add_hours(DtStart, Steps);
add_steps(minutely, DtStart, Steps) ->
    ts_datetime:add_minutes(DtStart, Steps);
add_steps(secondly, DtStart, Steps) ->
    ts_datetime:add_seconds(DtStart, Steps).

%%--------------------------------------------------------------------
%% @spec    (Level, DtStart) -> #date_time{} "(partially initialized)"
%%
%%            Level   = monthly  |
%%                      weekly   |
%%                      daily    |
%%                      hourly   |
%%                      minutely |
%%                      secondly
%%            DtStart = #date_time{}
%%
%% @doc     return a partially initialized date-time value based on
%%          dtstart, where time fields (D,H,M,S) that are less than
%%          Level are left uninitialized (set to undefined)
%% @end
%%--------------------------------------------------------------------
dtstart_based_time(Level, DtStart) ->
    U = undefined,
    case Level of
	monthly ->
	    {Year,Month,_} = DtStart#date_time.date,
	    #date_time{date = {Year,Month,U}, time = {U,U,U}, type = floating};
	weekly ->
	    {Year,Month,Day} = DtStart#date_time.date,
	    #date_time{date = {Year,Month,Day}, time = {U,U,U}, type = floating};
	daily ->
	    {Year,Month,Day} = DtStart#date_time.date,
	    #date_time{date = {Year,Month,Day}, time = {U,U,U}, type = floating};
	hourly ->
	    {Year,Month,Day} = DtStart#date_time.date,
	    {Hour,_,_} = DtStart#date_time.time,
	    #date_time{date = {Year,Month,Day}, time = {Hour,U,U}, type = floating};
	minutely ->
	    {Year,Month,Day} = DtStart#date_time.date,
	    {Hour,Minute,_} = DtStart#date_time.time,
	    #date_time{date = {Year,Month,Day}, time = {Hour,Minute,U}, type = floating};
	secondly ->
	    {Year,Month,Day} = DtStart#date_time.date,
	    {Hour,Minute,Second} = DtStart#date_time.time,
	    #date_time{date = {Year,Month,Day}, time = {Hour,Minute,Second}, type = floating}
    end.

%%--------------------------------------------------------------------
%% @spec    (Level, TimeSwitchCond, Time) ->
%%            [#date_time{}] "(partially initiated)"
%%
%%            Level          = monthly  |
%%                             weekly   |
%%                             daily    |
%%                             hourly   |
%%                             minutely |
%%                             secondly
%%            TimeSwitchCond = #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%            Time           = #date_time{} "search tree position in generate_counts(...) "
%%
%% @doc     takes Time and creates N date_time records with an
%%          additional time unit (month, day, hour, minute or second)
%%          set. Level specifies this. Note : get_new_times/3 is
%%          expected to be used to initialize time units in order
%%          (month -> second)
%% @end
%%--------------------------------------------------------------------
get_new_times(Level, TimeSwitchCond, Time) ->
    {Year,Month,_} = Time#date_time.date, %% current gen. year
    DtStart = time_switch:get_dtstart(TimeSwitchCond),
    Freq = time_switch:get_freq(TimeSwitchCond),

    %% io:format("Time ~p~n", [Time]), %%DDD

    case Freq == Level of
	true ->
	    %% get first start point in the search branch specified by
	    %% the partialy initialized Time value
	    Interval = time_switch:get_interval(TimeSwitchCond),
	    {FirstTime, Diff} = get_diff(Level, DtStart, Time),
	    Timezone = dummy,
	    ST = case ts_datetime:le_datetime(Timezone, FirstTime, DtStart) of
		     true ->
			 %% io:format("1.~n"), %DDD
			 dtstart_based_time(Level, DtStart);
		     false ->
			 %% io:format("2.~n"), %DDD
			 FullSteps = full_step(Level, Diff, Interval),
			 %% io:format("FullSteps ~p~n", [FullSteps]), %DDD
			 add_steps(Level, DtStart, FullSteps)
		 end,

	    %% io:format("FirstTime ~p,~n Interval ~p, Diff ~p, ST ~p~n", [FirstTime, Interval, Diff, ST]), %DDD

	    %% check that the start point occurs in the time
	    %% period of the search branched specified by Time
	    Vals = case step_in_time(Level, ST, Time) of
		       true ->
			   %% freq == generator, limit supplied start times to valid;
			   %% dtstart + nth_step * interval ones
			   case Level of
			       monthly ->
				   {_,StartMonth,_} = ST#date_time.date,
				   lists:seq(StartMonth, 12, Interval);
			       weekly ->
				   {SYear,SMonth,StartDay} = ST#date_time.date,
				   lists:seq(StartDay, calendar:last_day_of_the_month(SYear, SMonth), Interval * 7);
			       daily ->
				   {SYear,SMonth,StartDay} = ST#date_time.date,
				   lists:seq(StartDay, calendar:last_day_of_the_month(SYear, SMonth), Interval);
			       hourly ->
				   {StartHour,_,_} = ST#date_time.time,
				   lists:seq(StartHour, 23, Interval);
			       minutely ->
				   {_,StartMinute,_} = ST#date_time.time,
				   lists:seq(StartMinute, 59, Interval);
			       secondly ->
				   {_,_,StartSecond} = ST#date_time.time,
				   lists:seq(StartSecond, 59, Interval)
			   end;
		       false ->
			   []
		   end,
	    %% io:format("level = freq: Freq ~p, Level ~p, Vals ~p~n",[Freq, Level, Vals]), %DDD
	    new_times(Level, Time, Vals);

	%% current level /= freq (generator defined by "freq" in "time" tag),
	%% so supply all possible generator values
	false ->
	    Vals = case Level of
		       monthly ->
			   lists:seq(1,12);
		       weekly ->
			   %% freq /= weekly so let generate_counts(daily = Level, ...)
			   %% generate month days
			   {_,_,Val} = Time#date_time.date,
			   [Val];
		       daily ->
			   %% check if days of month have already been supplied by generator = weekly
			   case Freq == weekly of
			       true ->
				   {_,_,Val} = Time#date_time.date,
				   [Val];
			       false ->
				   lists:seq(1, calendar:last_day_of_the_month(Year, Month))
			   end;
		       hourly ->
			   lists:seq(0, 23);
		       minutely ->
			   lists:seq(0, 59);
		       secondly ->
			   lists:seq(0, 59)
		   end,
	    %% io:format("level /= freq: Freq ~p, Level ~p, Vals ~p~n",[Freq, Level, Vals]), %DDD
	    %% check if return is = [Time]
	    new_times(Level, Time, Vals)
    end.

%%--------------------------------------------------------------------
%% @spec    (...) -> ...
%%
%% @doc     create, delete, add element, retrieve elements (in
%%          insertion order) and get table size. count_xxx implements
%%          a external table, used to accumulate count start points.
%%          Data is store as {CountNo, DateTimeRec} pairs in a ets
%%          table
%% @end
%%--------------------------------------------------------------------

%% @clear

%% return: table id
count_create() ->
    %% ets:new(count_res, [ordered_set, {keypos, 1}, protected]). %DDD
    ets:new(count_res, [set, {keypos, 1}, protected]).

count_delete(TabId) ->
    ets:delete(TabId).

count_get_all(TabId) ->
    lists:sort([E || {_CountNo,E} <- ets:tab2list(TabId)]).

%% only add if elements if there are counts remaining
count_add(TabId, MaxCount, TimeSwitchCond, Val) ->
%%     io:format("count_add: MaxCount ~p, TimeSwitchCond~p,~nVal ~p~n",[MaxCount, TimeSwitchCond, Val]), %DDD
%%     io:format("ets tab: TabId ~p ~p~n",[TabId, ets:tab2list(TabId)]), %DDD

    CurrentCount = current_count(TabId),
%%    io:format("CurrentCount ~p~n",[CurrentCount]), %DDD
    case MaxCount of
	%% used by get_set/2 to retrieve a full search tree branch
	get_set ->
	    add_element(CurrentCount, TabId, Val);
	%% get MaxCount elements
	_ when is_integer(MaxCount) ->
	    if
		CurrentCount < MaxCount ->
		    %% don't insert date-times before dtstart (start of count based range)
		    DtStart = time_switch:get_dtstart(TimeSwitchCond),
		    case range_clip(DtStart, [Val]) of
			[] ->
			    ok;
			_ ->
			    %% if time_switch__cond_N contains bysetpos, check that the
			    %% bysetpos condition holds
			    case is_record(TimeSwitchCond, time_switch__cond_8) of
				true ->
				    case is_start_time_valid_bysetpos(TimeSwitchCond, Val) of
					true ->
					    add_element_check_finish(CurrentCount, MaxCount, TabId, Val);
					false ->
					    ok
				    end;
				false ->
				    add_element_check_finish(CurrentCount, MaxCount, TabId, Val)
			    end
		    end;
		true ->
		    %% this should never happen
		    throw({error, trying_to_add_more_than_maxcount_elements})
	    end
    end.

%% add element to table
%% return: NextCount
add_element(CurrentCount, TabId, Val) ->
    NextCount = CurrentCount + 1,
    ets:insert(TabId, {NextCount, Val}),
    NextCount.

%% throw() if a sufficient number of elements NextCount have been found
check_finish(NextCount, MaxCount) ->
    case NextCount == MaxCount of
	true ->
	    %% no more start points are needed, exit back to
	    %% last generate_counts(yearly = Level, ...)
	    throw({finished, count_reached});
	false ->
	    ok
    end.

add_element_check_finish(CurrentCount, MaxCount, TabId, Val) ->
    NextCount = add_element(CurrentCount, TabId, Val),
    check_finish(NextCount, MaxCount).



%% get table size - no. of accumulated entries
current_count(TabId) ->
    ets:info(TabId, size).

%%--------------------------------------------------------------------
%% @spec    (Fun, Dests, MaxCount, TimeSwitchCond) -> ok
%%
%%            Fun            = fun() "the next generate_counts(...) call"
%%            Dests          = [#date_time{}] "the search branches to explore "
%%            MaxCount       = integer() "The number of matches required"
%%            TimeSwitchCond = #time_switch__cond_n{} "the time-switch (time tag) conditions"
%%
%% @doc     limited depth first search - do depth first scan on Dests
%%          and only return the first MaxCount matches
%% @end
%%--------------------------------------------------------------------
limited_df(Fun, Dests, MaxCount, TimeSwitchCond) ->
    limited_df2(Fun, Dests, MaxCount, TimeSwitchCond).

limited_df2(_Fun, [], _MaxCount, _TimeSwitchCond) ->
    ok;

%% call generate_counts(...) on all partial start times
limited_df2(Fun, [Dest | RDests], MaxCount, TimeSwitchCond) ->
    DtStart = time_switch:get_dtstart(TimeSwitchCond),
    %% check if branch needs to be examined for start points
    case range_clip(DtStart, [Dest]) of
	[] ->
	    ok;
	_ ->
	    Fun(Dest)
    end,
    limited_df2(Fun, RDests, MaxCount, TimeSwitchCond).


%%--------------------------------------------------------------------
%% @spec    (DtStart, Times) -> [#date_time{}]
%%
%%            DtStart = #date_time{}
%%            Times   = [#date_time{}] "partialy initiated"
%%            Level   = atom() "current filter/generator level, used to determine how much of the Times elements  date-time to use in checks against Start"
%%
%% @doc     only keep elements in Times that are >= DtStart
%% @end
%%--------------------------------------------------------------------
range_clip(DtStart, Times) ->
    %% io:format("range_clip: DtStart ~p, Times ~p~n", [DtStart, Times]), %DDD
    %% set undefined fields to valid values - DtStart fields are used
    %% as this will ensure that only the defined fields will affect
    %% the '>=' comparision
    Filter = fun(Time) ->
		     Timezone = dummy,
		     DT = add_dtstart_modifiers(Time, DtStart),
		     ts_datetime:ge_datetime(Timezone, DT, DtStart)
	     end,
    [Time || Time <- Times, Filter(Time)].


%%--------------------------------------------------------------------
%% @spec    (Current, Timezone, TimeSwitchCond) -> true | false
%%
%%            Current        = integer() "gregorian seconds in utc"
%%            Timezone       = term() "currently unused"
%%            TimeSwitchCond = term() "#time_switch__cond_N{} (N >= 5)"
%%
%% @doc     determine if Current falls inside any of the time ranges
%%          in #time_switch__cond_N.time_ranges Note : O(N), N =
%%          number of count matches stored in
%%          #time_switch__cond_N.time_ranges
%% @end
%%--------------------------------------------------------------------
count_time_range(Current, _Timezone, TimeSwitchCond) ->
    TimeRanges = time_switch:get_time_ranges(TimeSwitchCond),
    F = fun({US, UE}) ->
		in_usec_range(US, UE, Current)
	end,
    lists:any(F, TimeRanges).

%%--------------------------------------------------------------------
%% @spec    (Timezone, Start, Duration) ->
%%            {Start, Stop} | no_range
%%
%%            Timezone = term() "currently unused"
%%            Start    = #date_time{}
%%            Duration = #duration{}
%%
%%            Start = time_spec()
%%            Stop  = time_spec()
%%
%% @doc     transform a date-time start point and duration to a
%%          gregorian second start and stop range. 'no_range' is
%%          returned if Start is a invalid date or if the [Start,
%%          Start + Duration] range (in floating time) is entirely
%%          inside a non-existing DST transition period Note : while
%%          RFC 3880 does not explicitly say what to do with invalid
%%          dates, the _non-normative_ time-switch resolving
%%          algorithm in RFC 3880, Appendix A, page 51-52, chooses to
%%          do a reject. This has been done in (the current) version
%%          of this CPL implementation, this has the advantage of
%%          making the code simpler and time-switch handling easy to
%%          understand.
%% @end
%%--------------------------------------------------------------------
valid_range(Timezone, Start, Duration) ->
    StartDate = Start#date_time.date,
    case calendar:valid_date(StartDate) of
	true ->
	    Stop = ts_datetime:add_datetime_and_duration(Start, Duration),

	    %% convert the [Start,Stop] range to UTC seconds
	    US = ts_datetime:datetime_to_usec(start, Timezone, Start),
	    UE = ts_datetime:datetime_to_usec(stop, Timezone, Stop),
	    case {Start#date_time.type, Stop#date_time.type} of
		{utc, utc} ->
		     {US, UE};
		{floating, floating} ->
		    %% floating time values have a non-existing
		    %% time period during transition to DST, this check
		    %% ensures that the US - UE range isn't covered by the
		    %% non-existent transition period (a partial overlap is ok)
		     case {US,UE} of
			 {{pseudo_usec, _}, {pseudo_usec, _}} ->
			     no_range;
			 _ ->
			     {US, UE}
		     end
	    end;
	false ->
	    no_range
    end.


%%--------------------------------------------------------------------
%% @spec    (Current, Timezone, TimeSwitchCond) -> true | false
%%
%%            Current        = integer() "gregorian seconds in utc"
%%            Timezone       = term() "currently unused"
%%            TimeSwitchCond = #time_switch__cond_5{}
%%
%% @doc     determine if current time Current, falls inside a time
%%          period specified by TimeSwitchCond
%% @end
%%--------------------------------------------------------------------
in_time_range_5(Current, Timezone, TimeSwitchCond) ->
    Start = time_switch:get_dtstart(TimeSwitchCond),
    Duration = ts_duration:duration(TimeSwitchCond),
    Interval = time_switch:get_interval(TimeSwitchCond),

    %% only check if Current is part of the time periods defined by TimeSwitchCond i.e.
    %% if Current >= dtstart in TimeSwitchCond, this ensures that no unneeded work is
    %% done - there's no need to do the checks before the dtstart time occurs
    case lt_usec(Current, ts_datetime:datetime_to_usec(start, Timezone, Start)) of
	true -> false;
	false ->
	    %% ensure that both Current and Start use the same #date_time.type
	    CurrentDateTime = gsec_to_datetime(Current, Start#date_time.type, Timezone),
	    Freq = time_switch:get_freq(TimeSwitchCond),
	    Diff = ts_datetime:diff_datetime(Timezone, CurrentDateTime, Start, Freq),

	    %% determine how many occurrence have occurred (0 = the first one and so on ...)
	    %% note: some may not actually have occurred due to DST and leap days
	    NextFreqNo = Diff div Interval,

	    %% get start time for the current occurence
	    S = add_freq_step(TimeSwitchCond, Start, NextFreqNo),

	    %% check range S-E (E = S + Duration), ensure that S and E are real dates.
	    %% DST is handled by datetime_to_usec(...) and in_usec_range(...)
	    %% note: valid_range(...) is only needed when dealing with functions that can
	    %%       create non-legal dates, like unsafe_add_xxx(...)
	    case valid_range(Timezone, S, Duration) of
		no_range -> false;
		{US, UE} -> in_usec_range(US, UE, Current)
	    end
    end.

%% convert gregorian seconds (utc) to date_time record() of Type = utc | floating
%% Note: Timezone is currently unsupported, and are only used if Type = floating
gsec_to_datetime(GSec, Type, _Timezone) ->
    {Date, Time} = case Type of
		       utc ->
			   calendar:gregorian_seconds_to_datetime(GSec);
		       floating ->
			   UDT = calendar:gregorian_seconds_to_datetime(GSec),
			   calendar:universal_time_to_local_time(UDT)
		   end,
    #date_time{date = Date, time = Time, type = Type}.

%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond, DateTime, Steps) -> #date_time{}
%%
%%            TimeSwitchCond = #time_switch__cond_5{} |
%%                             #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%            DateTime       = #date_time{}
%%            Steps          = integer()
%%
%% @doc     do DateTime + (Steps * Freq Interval in TimeSwitchCond)
%%          Note : non-valid dates (29/2 in non-leap year) can be
%%          created by this function, valid_range(...) or similar
%%          function may be needed handle this output properly
%% @end
%%--------------------------------------------------------------------
add_freq_step(TimeSwitchCond, DateTime, Steps) ->
    Freq = time_switch:get_freq(TimeSwitchCond),
    Interval = time_switch:get_interval(TimeSwitchCond),
    case Freq of
	secondly ->
	    ts_datetime:add_seconds(DateTime, Steps * Interval);
	minutely ->
	    ts_datetime:add_minutes(DateTime, Steps * Interval);
	hourly ->
	    ts_datetime:add_hours(DateTime, Steps * Interval);
	daily ->
	    ts_datetime:add_days(DateTime, Steps * Interval);
	weekly ->
	    ts_datetime:add_weeks(DateTime, Steps * Interval);
	monthly ->
	    ts_datetime:unsafe_add_months(DateTime, Steps * Interval);
	yearly ->
	    ts_datetime:unsafe_add_years(DateTime, Steps * Interval)
    end.

%%--------------------------------------------------------------------
%% @spec    (DateTime) ->
%%            term() "as calendar:local_time_to_universal_time_dst(DateTime)"
%%
%% @doc     see calendar:local_time_to_universal_time_dst(DateTime)
%%          A patch to handle a bug when a illegal date occurs when
%%          changing to DST (daylight saving time) i.e. when the
%%          clock is moved forward a hour during spring.
%%          Setup: MacOSX 10.3.6 / Erlang/OTP R10B-1 Error: ``` >
%%          catch
%%          calendar:local_time_to_universal_time_dst({{2004,3,28},{2,0,1}}).
%%          {'EXIT',{badarg,[{erlang,universaltime_to_localtime,
%%          [{{1969,12,31},{23,59,59}}]},
%%          {calendar,local_time_to_universal_time_dst,1},
%%          {erl_eval,do_apply,5}, {erl_eval,expr,5},
%%          {shell,exprs,6}, {shell,eval_loop,3}]}} '''
%% @end
%%--------------------------------------------------------------------
safe_local_time_to_universal_time_dst(LDateTime) ->
    try
	calendar:local_time_to_universal_time_dst(LDateTime)
    catch
	%% runtime error
	error: _ -> []
    end.

%%--------------------------------------------------------------------
%% @spec    () -> integer() "gregorian seconds in UTC format"
%%
%% @doc     convert local (current) time to a universal format
%%          (gregorian seconds in UTC format) Note : local time
%%          reported is based on what the OS supplies / is set to
%% @end
%%--------------------------------------------------------------------
local_usec() ->
    LDateTime = bound_leap_second(calendar:local_time()),
    UDateTime2 = safe_local_time_to_universal_time_dst(LDateTime),
    UDateTime = case UDateTime2 of
		    %% "For a local DateTime during the period that is skipped when
		    %%  switching to daylight saving time, there is no corresponding UTC
		    %%  since the local time is illegal - it has never happened". - OTP R10B-1 doc
		    [] ->
			%% local time should never be able to generate a illegal date time value
			throw({error, unexpected_local_date_time_value});
		    %% "For a local DateTime during the period that is repeated when
		    %%  switching from daylight saving time, there are two corresponding
		    %%  UTCs. One for the first instance of the period when daylight
		    %%  saving time is still active, and one for the second instance." - OTP R10B-1 doc
		    [_DstDateTimeUTC, DateTimeUTC] ->
			DateTimeUTC;
		    %% "For all other local times there is only one corresponding UTC." - OTP R10B-1 doc
		    [DateTimeUTC] ->
			DateTimeUTC
		end,
    calendar:datetime_to_gregorian_seconds(UDateTime).

%% this function is used to ensure that all date-time values retrieved
%% from erlang/OTP time and calendar functions have second values in the range 0-59,
%% this can lead to the time being of by 1 second.
bound_leap_second({Date, {H,M,S}}) ->
    NewS = if
	       S == 60 ->
		   59;
	       S >= 0, S =< 59 ->
		   S
	   end,
    {Date, {H,M,NewS}}.


%%--------------------------------------------------------------------
%% @spec    (Start, End, Current) -> true | false
%%
%%            Start   = time_spec()
%%            End     = time_spec()
%%            Current = integer() "time in UTC gregorian seconds"
%%
%% @doc     determine if Start `=<' Current `=<' End Start, End and
%%          Current are retrieved with local_usec/0 or
%%          datetime_to_usec/2 (these functions MUST be used)
%% @end
%%--------------------------------------------------------------------
in_usec_range(Start, End, Current) ->
    case {Start, End} of
	%% both start and end are inside the illegal range - there can be no match
	{{pseudo_usec, Time}, {pseudo_usec, Time}} -> false;
	%% range ends in illegal range - "Time - 1" is the last legal value before the illegal range,
	%% this works because there is no 1 hour jump in the UTC seconds value
	{Start, {pseudo_usec, Time}} -> (Start =< Current) and (Current =< (Time - 1));
	%% range start in a illegal range, Time is the first legal value as supplied by datetime_to_usec/2
	{{pseudo_usec, Time} , End} -> (Time =< Current) and (Current =< End);
	%% the regular nice case
	{Start, End} -> (Start =< Current) and (Current =< End)
    end.

%%--------------------------------------------------------------------
%% @spec    (V1, V2) -> true | false
%%
%%            V1 = time_spec()
%%            V2 = time_spec()
%%
%% @doc     determine if V1 `<' V2. local_usec/0 or datetime_to_usec/2
%%          MUST be used to construct V1 and V2.
%% @end
%%--------------------------------------------------------------------
lt_usec(V1, V2) ->
    case {V1, V2} of
	{{pseudo_usec, Time1}, {pseudo_usec, Time2}} -> Time1 < Time2;
	{V1, {pseudo_usec, Time}} -> V1 < Time;
	%% Time-1 moves Time back to last legal second - the one before change to DST
	{{pseudo_usec, Time} , V2} -> (Time-1) < V2;
	%% the regular nice case
	{V1, V2} -> V1 < V2
    end.

%%--------------------------------------------------------------------
%% @spec    (ByParams) -> [term()] "byxxx elements"
%%
%%            ByParams = [term()] "byxxx elements (see #time_switch__cond_N.by_values)"
%%
%% @doc     sort list of byxxx parameters in order as specified by RFC
%%          3880 chapter 4.4 page 18
%% @end
%%--------------------------------------------------------------------
sort_byparam(ByParams) ->
    lists:sort(fun lt_byparam/2, ByParams).

param_order(bymonth) -> 1;
param_order(byweekno) -> 2;
param_order(byyearday) -> 3;
param_order(bymonthday) -> 4;
param_order(byday) -> 5;
param_order(byhour) -> 6;
param_order(byminute) -> 7;
param_order(bysecond) -> 8.

lt_byparam(P1, P2) ->
    {Param1, Val1} = P1,
    {Param2, Val2} = P2,
    {param_order(Param1), Val1} < {param_order(Param2), Val2}.

%%--------------------------------------------------------------------
%% @spec    (Current, Timezone, TimeSwitchCond) -> true | false
%%
%%            Current        = integer() "gregorian seconds in utc"
%%            Timezone       = term() "currently unused"
%%            TimeSwitchCond = #time_switch__cond_7{}
%%
%% @doc     determine if current time Current, falls inside a time
%%          period specified by TimeSwitchCond
%% @end
%%--------------------------------------------------------------------
in_time_range_7(Current, Timezone, TimeSwitchCond) ->
    Start = time_switch:get_dtstart(TimeSwitchCond),

    %% only check if Current is part of the time periods defined by TimeSwitchCond i.e.
    %% if Current >= dtstart in TimeSwitchCond, this ensures that no unneeded work is
    %% done - there's no need to do the checks before the dtstart time occurs
    case lt_usec(Current, ts_datetime:datetime_to_usec(start, Timezone, Start)) of
	true -> false;
	false ->
	    %% ensure that both Current and Start use the same #date_time.type
	    CurrentDateTime = gsec_to_datetime(Current, Start#date_time.type, Timezone),
	    byxxx_match(TimeSwitchCond, CurrentDateTime)
    end.

%%--------------------------------------------------------------------
%% Processing time_switch__cond_7:
%%
%% the basic idea is to view the conditions as generators of possible
%% start date-times. There are two types of conditions that need
%% processing:
%%
%% generator	filter
%% (freq)	(byxxx)
%% 				order of processing
%% yearly			|
%% monthly			|
%% 		bymonth		v
%% weekly
%% daily
%% 		byweekno
%% 		byyearday
%% 		bymonthday
%% 		byday
%% hourly
%% 		byhour
%% minutely
%% 		byminute
%% secondly
%% 		bysecond
%%
%% * process attributes in order, as listed above
%% * there is always _one_ and only one generator
%% * generators create all elements in the current time unit (that are
%%   part of the previous time unit e.g. minutely -> minute
%%   1,2,3,...,59 of hour X). Current time is used as a filter to
%%   select only one of these generated items when checking if a
%%   time-switch condition occurs
%% * filters check that the current time matches one of the supplied
%%   byxxx values
%% * if a filter/generator isn't supplied for one of the levels, use
%%   the current time value if current level (rank) >= freq, otherwise
%%   use dtstart date-time values
%% * duration is assumed to be non-overlapping, see README and
%%   ts_duration.erl for more details of how to check for this
%% * stop level processing when the the last level filter/generator
%%   supplied has been reached. This level is not processed like the
%%   others, instead create all the possible ranges [start_point_N,
%%   start_point_N + duration] and check if the current time is
%%   inside any of them - the ranges are needed as the start_point
%%   time unit may not have the same value as the current time
%% * any still undefined time units (hours, minutes, seconds, ...)
%%   needed to define start_points, are taken from dtstart
%%
%% example:
%%
%% dtstart = 2000-01-01 00:00:00
%% freq = yearly, interval = 2, duration = 3h (max length)
%% bymonth = 1, byday = 8,12,20, byhour = 8,12,15
%%
%% current time = 2002-01-12 13:00:00
%%
%% year, month and day match ->
%%
%% ranges use dtstart supplied minute and seconds (xx:00:00)
%%
%% [08:00:00, 10:59:59]
%% [12:00:00, 14:59:59] (this matches the 13:00:00 time)
%% [15:00:00, 17:59:59]
%%--------------------------------------------------------------------
%% Processing "count":
%%
%% * scan the generated tree depth first, until count matches have
%%   been found
%% * skip any occurrences before dtstart and after until
%%
%% Processing "bysetpos":
%%
%% * bysetpos is similar to "count" but only the Nth count is used for
%%   matching.
%% * how to convert negative N to positive: N = set size + N - 1
%%   (when N is =< -1)
%% * -Nth requires the occurrence set to be finite
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond, CurrentDateTime) -> true | false
%%
%%            TimeSwitchCond  = term() "time_switch_cond_N (N >= 7)"
%%            CurrentDateTime = #date_time{}
%%
%% @doc     determine if CurrentDateTime is part of any of the
%%          intervals specified by the time-switch
%% @end
%%--------------------------------------------------------------------
byxxx_match(TimeSwitchCond, CurrentDateTime) ->
    Freq = time_switch:get_freq(TimeSwitchCond),
    ByParams = sort_byparam(time_switch:get_by_values(TimeSwitchCond)),
    StartLevel = get_start_level(Freq, ByParams),
    LowestLevel = get_lowest_level(Freq, ByParams),

%%     io:format("byxxx_match/2: Freq ~p, ByParams ~p, StartLevel ~p, LowestLevel ~p~n",
%%   	      [Freq,ByParams,StartLevel, LowestLevel]), %DDD

    byxxx_match(yearly, StartLevel, LowestLevel, #start_time{}, ByParams, CurrentDateTime, TimeSwitchCond).


%% find the first time-switch supplied Level
get_start_level(Freq, [{Level, _} | _]) ->
    FRank = get_rank(Freq),
    ByRank = get_rank(Level),
    case FRank > ByRank of
 	true -> Freq;
 	false -> Level
    end.

%% determine which level that needs to be range checked
get_lowest_level(Freq, ByParams) ->
    [{ByLevel, _} | _] = lists:reverse(ByParams),
    FRank = get_rank(Freq),
    ByRank = get_rank(ByLevel),
    case FRank < ByRank of
 	true -> Freq;
 	false -> ByLevel
    end.

%% return: generator | filter
level_type(Level) ->
      case Level of
	yearly -> generator;
	monthly	-> generator;
	bymonth	-> filter;
	weekly -> generator;
	byweekno -> filter;
	daily -> generator;
	byyearday -> filter;
	bymonthday -> filter;
	byday -> filter;
	hourly -> generator;
	byhour -> filter;
	minutely -> generator;
	byminute -> filter;
	secondly -> generator;
	bysecond -> filter
    end.

%% determine which processing order Level is
get_rank(Level) ->
    case Level of
	yearly -> 15;
	monthly	-> 14;
	bymonth	-> 13;
	weekly -> 12;
	daily -> 11;
	byweekno -> 10;
	byyearday -> 9;
	bymonthday -> 8;
	byday -> 7;
	hourly -> 6;
	byhour -> 5;
	minutely -> 4;
	byminute -> 3;
	secondly -> 2;
	bysecond -> 1
    end.

rank_to_level(Rank) ->
    case Rank of
	15 -> yearly;
	14 -> monthly;
	13 -> bymonth;
	12 -> weekly;
	11 -> daily;
	10 -> byweekno;
	9 -> byyearday;
	8 -> bymonthday;
	7 -> byday;
	6 -> hourly;
	5 -> byhour;
	4 -> minutely;
	3 -> byminute;
	2 -> secondly;
	1 -> bysecond
    end.

%% get next level
get_next_level(CurrentLevel) ->
    CRank = get_rank(CurrentLevel),
    rank_to_level(CRank - 1).

%%--------------------------------------------------------------------
%% @spec    (CurrentLevel, ByParams, TimeSwitchCond) ->
%%            {NextLevel, NextProcessLevel, NextLevelByParams}
%%
%%            CurrentLevel   = atom()
%%            ByParams       = [{Level, Val}] "ordered on Level, contains all tuples where  Level is less than or equal to CurrentLevel"
%%            TimeSwitchCond = term() "#time_switch__cond_n{} (n >= 7)"
%%
%%            NextLevel         = term() "the next supplied level, this may be either a filter or a generator  (\"freq\") "
%%            NextProcessLevel  = term() "next filter / generator not yet processed in ByParams"
%%            NextLevelByParams = term() "ByParams stripped of the initial elements that match CurrentLevel"
%%
%% @doc     get the next supplied level and it's ByParams where Level
%%          `=<' NextLevel Note : CurrentLevel must be `>' bysecond
%% @end
%%--------------------------------------------------------------------
get_next_level(CurrentLevel, ByParams, TimeSwitchCond) ->
    Res = case get_rank(CurrentLevel) > get_rank(bysecond) of
	      false ->
		  throw({error, current_level_is_lowest_level});
	      true ->
		  NextLevel = get_next_level(CurrentLevel),
		  {_, NextLevelByParams} = get_level_params(CurrentLevel, ByParams),
		  NextProcessLevel = get_next_process_level(CurrentLevel, ByParams, TimeSwitchCond),
		  {NextLevel, NextProcessLevel, NextLevelByParams}
	  end,

    %% io:format("CurrentLevel ~p, Res ~p~n", [CurrentLevel, Res]), %DDD
    Res.


%% determine which filter / generator needs to be checked next
get_next_process_level(Level, ByParams, TimeSwitchCond) ->
    Freq = time_switch:get_freq(TimeSwitchCond),
    TopRank = get_rank(Level),
    Levels = lists:usort([get_rank(Freq)] ++ [get_rank(Byxxx) || {Byxxx,_} <- ByParams]),
    F = fun(L,Acc) ->
		case L < TopRank of
		    true -> [L | Acc];
		    false -> Acc
		end
	end,
    PLevels = lists:foldl(F, [], Levels),
    %% io:format("PLevels ~p~n", [PLevels]), %DDD
    [PLevel | _] = PLevels,
    rank_to_level(PLevel).

%%--------------------------------------------------------------------
%% @spec    (Level, ByParams) ->
%%            {LevelParams, LowerLevelParams}
%%
%%            Level    = term() "the level of a ByParams elements {Level, Val} tuple"
%%            ByParams = term() "time_switch__cond_x.by_values sorted on Level"
%%
%%            LevelParams      = [{Level, Val}]
%%            LowerLevelParams = [{Level, Val}]
%%
%% @doc     get the {Level, Val} entries from ByParam (the N first
%%          elements), also return the remaining lower level elements
%%          Note : Level must be the highest level in ByParams
%% @end
%%--------------------------------------------------------------------
get_level_params(Level, ByParams) ->
    LevelParams = [{LN, ValN} || {LN,ValN} <- ByParams, LN == Level],
    %% this will allways be the lower level params, as Level - the ones begin striped are
    %% allways the highest ones (occure first in ByParams)
    LowerLevelParams = [{LN, ValN} || {LN,ValN} <- ByParams, LN /= Level],
    {LevelParams, LowerLevelParams}.

%%--------------------------------------------------------------------

%% process a byxxx filter and then call the next byxxx_match(...) step
filter_byxxx(NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
	     CurrentDateTime, TimeSwitchCond) ->
%%      io:format("filter_byxx: NextLevel ~p~n, MatchVal ~p~n, Level ~p~n, LowestLevel ~p~n,"
%% 	       "StartTime ~p~n, ByParams ~p~n, CurrentDateTime~p~n, TimeSwitchCond ~p~n",
%% 	       [NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
%% 	       CurrentDateTime, TimeSwitchCond]), %%DDD
    {LevelParams, _LowerParams} = get_level_params(Level, ByParams),
    Match = case LevelParams of
		[] ->
		    true;
		_ ->
		    Wkst = time_switch:get_wkst(TimeSwitchCond),
		    F = fun(Param) ->
				match_param(Param, MatchVal, StartTime, Wkst)
				%% Val == MatchVal
			end,
		    lists:any(F, LevelParams)
	    end,
    case Match of
	false ->
%%	    io:format("filter_byxxx: Level ~p, CurrentDateTime ~p~n",[Level, CurrentDateTime]), %DDD
 	    false;
	true ->
	    {_NextLevel, NextProcessLevel, NextLevelByParams} = get_next_level(Level, ByParams, TimeSwitchCond),
%% 	    io:format("filter_byxxx: NextLevel        ~p~n",[NextLevel]), %DDD
%% 	    io:format("filter_byxxx: NextProcessLevel ~p~n",[NextProcessLevel]), %DDD
	    byxxx_match(NextLevel, NextProcessLevel, LowestLevel, StartTime, NextLevelByParams, CurrentDateTime,
			TimeSwitchCond)
    end.


%% determine if a value in a byxxx parameter is = the current time (MatchVal)
%% return: true | false
match_param({bysecond, Val}, MatchVal, _StartTime, _Wkst) ->
    Val == MatchVal;
match_param({byminute, Val}, MatchVal, _StartTime, _Wkst) ->
    Val == MatchVal;
match_param({byhour, Val}, MatchVal, _StartTime, _Wkst) ->
    Val == MatchVal;
match_param({byday, Val}, MatchVal, StartTime, _Wkst) ->
    Year = StartTime#start_time.year,
    Month = StartTime#start_time.month,
    Day = StartTime#start_time.monthday,
    case Val of
	{all, WeekDay} ->
	    WeekDay == MatchVal;
	{N, WeekDay} ->
	    %% get current date
	    Date = {Year,Month,Day},
	    %% compare date of Nth WeekDay with current date
	    ts_date:nth_byday_in_month(Year,Month,N,WeekDay) == Date
    end;
match_param({bymonthday, Val}, MatchVal, StartTime, _Wkst) ->
    Year = StartTime#start_time.year,
    Month = StartTime#start_time.month,
    Day = ts_date:normalize_monthday(Year, Month, Val),
    Day == MatchVal;
match_param({byyearday, Val}, MatchVal, StartTime, _Wkst) ->
    Year = StartTime#start_time.year,
    Day = ts_date:normalize_yearday(Year, Val),
    Day == MatchVal;
match_param({byweekno, Val}, MatchVal, StartTime, Wkst) ->
    Year = StartTime#start_time.year,
    WeekNo = ts_date:get_week_no(Year, Wkst, Val),
    {Year, WeekNo} == MatchVal;
match_param({bymonth, Val}, MatchVal, _StartTime, _Wkst) ->
    Val == MatchVal.

%% process a freq element, checks that the time unit for freq is a interval start point
%% e.g. dtstart = 2000-01-01 00:00:00, freq = monthly, interval = 2 -> month must be = 1,3,5,7,9 or 11
is_reoccurrence(TimeSwitchCond, CurrentDateTime, PrepareNextByxxxMatchCall) ->
    case is_reoccurrence(TimeSwitchCond, CurrentDateTime) of
	true ->
	    %% io:format("is_reoccurrence - true~n"), %DDD
	    PrepareNextByxxxMatchCall();
	false ->
	    %% io:format("is_reoccurrence - false~n"), %DDD
	    false
    end.

is_reoccurrence(TimeSwitchCond, DateTime) ->
    Timezone = dummy,
    DtStart = time_switch:get_dtstart(TimeSwitchCond),
    Freq = time_switch:get_freq(TimeSwitchCond),
    Interval = time_switch:get_interval(TimeSwitchCond),
    Diff = ts_datetime:diff_datetime(Timezone, DateTime, DtStart, Freq),
%%     io:format("DtStart ~p, Freq ~p, Interval ~p, Diff ~p, Diff rem Interval ~p~n])", %DDD
%%  	      [DtStart, Freq, Interval, Diff, Diff rem Interval]),
    case Diff rem Interval of
	0 ->
	    true;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (Level, CurrentLevel, LowestLevel, StartTime, ByParams,
%%          CurrentDateTime, TimeSwitchCond) -> true | false
%%
%%            Level           = term() "acts as a generator/filter index used to send the current call on to the next check (start with yearly)"
%%            NextLevel       = term() "next byxxx / freq to process (set to next byxxx/freq still remaining)"
%%            LowestLevel     = term() "the last filter/generator to process "
%%            StartTime       = term() "accumulator of date and time values, used as offset when applying dtstart modifiers when reaching LowestLevel where rang checking is needed "
%%            ByParams        = term() "the remaining unprocessed byxxx values ( must be sorted with  sort_byparam(ByParams) )"
%%            CurrentDateTime = term() "the time to check against TimeSwitchCond"
%%            TimeSwitchCond  = term() "a time_switch__cond_n (N >= 7) record"
%%
%% @doc     determine if CurrentDateTime is part of any time interval
%%          defined by ByParams (from TimeSwitchCond)
%% @end
%%--------------------------------------------------------------------

%% BASE CASE - - - - - - - - - - - - - - - - - -
%% reached lowest supplied level
byxxx_match(LowestLevel, LowestLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %%    io:format("byxxx_match base case:~n"),
    %%     io:format("LowestLevel ~p~n, StartTime ~p~n, ByParams ~p~n, CurrentDateTime ~p~n, TimeSwitchCond ~p~n",
    %%    	      [LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond]), % DDD
    BaseTime = add_dtstart_modifiers(StartTime, time_switch:get_dtstart(TimeSwitchCond)),
    case level_type(LowestLevel) of
	filter ->
	    StartPoints = create_startpoints(BaseTime, ByParams, LowestLevel, time_switch:get_wkst(TimeSwitchCond)),
 	    %% io:format("byxxx_match/7 filter:~nStartPoints ~p~n", [StartPoints]), %DDD
	    check_intervals(StartPoints, TimeSwitchCond, CurrentDateTime);
	%% preceded by byxxx condition - there must be byxxx in a time_switch__cond_N (N >= 7) and
       	generator ->
	    case is_reoccurrence(TimeSwitchCond, BaseTime) of
		true ->
		    StartPoints = create_startpoints(BaseTime, LowestLevel, TimeSwitchCond),
 		    %% io:format("byxxx_match/7 generator:~nStartPoints ~p~n", [StartPoints]), %DDD
		    check_intervals(StartPoints, TimeSwitchCond, CurrentDateTime);
		false ->
		    false
	    end
    end;

%% YEARLY - - - - - - - - - - - - - - - - - -
%% yearly value supplied, check that current time matches
byxxx_match(yearly, yearly, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< yearly - StartTime: ~p~n",[StartTime]), %DDD
%%     io:format("byxxx_match - yearly: "
%% 	      "LowestLevel ~p~nStartTime ~p~nByParams ~p~nCurrentDateTime ~p~nTimeSwitchCond ~p~n", % DDD
%% 	      [LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond]),

    PrepareNextByxxxMatchCall =
 	fun() ->
 		{Y,_M,_D} = CurrentDateTime#date_time.date,
 		NewStartTime = StartTime#start_time{year = Y},
 		{NextLevel, NextProcessLevel, NextLevelByParams} = get_next_level(yearly, ByParams, TimeSwitchCond),
 		byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, NextLevelByParams,
			    CurrentDateTime, TimeSwitchCond)
 	end,
    is_reoccurrence(TimeSwitchCond, CurrentDateTime, PrepareNextByxxxMatchCall);

%% no yearly value supplied
byxxx_match(yearly = Level, NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< yearly - StartTime: ~p~n",[StartTime]), %DDD
%%     io:format("byxxx_match - yearly: "
%% 	      "LowestLevel ~p~nStartTime ~p~nByParams ~p~nCurrentDateTime ~p~nTimeSwitchCond ~p~n", % DDD
%% 	      [LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond]),

    DefaultDateTime = get_default_datetime(Level, TimeSwitchCond, CurrentDateTime),
    {Year,_M,_D} = DefaultDateTime#date_time.date,
    NewStartTime = StartTime#start_time{year = Year},
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, ByParams, CurrentDateTime, TimeSwitchCond);

%% MONTHLY - - - - - - - - - - - - - - - - - -
%% monthly value supplied, check that current time matches
byxxx_match(monthly, monthly, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< monthly - StartTime: ~p~n",[StartTime]), %DDD
    PrepareNextByxxxMatchCall =
	fun() ->
		{_Y,M,_D} = CurrentDateTime#date_time.date,
		NewStartTime = StartTime#start_time{month = M},
		{NextLevel, NextProcessLevel, NextLevelByParams} = get_next_level(monthly, ByParams, TimeSwitchCond),
 		byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, NextLevelByParams,
			    CurrentDateTime, TimeSwitchCond)
	end,
    is_reoccurrence(TimeSwitchCond, CurrentDateTime, PrepareNextByxxxMatchCall);

%% no monthly value supplied
byxxx_match(monthly = Level, NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< monthly - StartTime: ~p~n",[StartTime]), %DDD
    DefaultDateTime = get_default_datetime(Level, TimeSwitchCond, CurrentDateTime),
    {_Y,M,_D} = DefaultDateTime#date_time.date,
    NewStartTime = StartTime#start_time{month = M},
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, ByParams, CurrentDateTime, TimeSwitchCond);

%% BYMONTH - - - - - - - - - - - - - - - - - -
%% filter on month
byxxx_match(bymonth = Level, _NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< bymonth - StartTime: ~p~n",[StartTime]), %DDD
    MatchVal = StartTime#start_time.month,
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    filter_byxxx(NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
		CurrentDateTime, TimeSwitchCond);

%% WEEKLY - - - - - - - - - - - - - - - - - -
%% weekly value supplied, check that current time matches
byxxx_match(weekly, weekly, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< weekly - StartTime: ~p~n",[StartTime]), %DDD
    PrepareNextByxxxMatchCall =
	fun() ->
		Wkst = time_switch:get_wkst(TimeSwitchCond),
		YearWeek = ts_date:date_to_weekno(CurrentDateTime#date_time.date, Wkst),
		{_Y,_M,D} = Date = CurrentDateTime#date_time.date,
		YearDay = ts_date:date_to_dayno(Date),
		MonthDay = D,
		WeekDay = ts_date:dayno_to_daytype(calendar:day_of_the_week(Date)),
		NewStartTime = StartTime#start_time{weekno = YearWeek, yearday = YearDay,
						    monthday = MonthDay, weekday = WeekDay},
		{NextLevel, NextProcessLevel, NextLevelByParams} = get_next_level(weekly, ByParams, TimeSwitchCond),
		byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, NextLevelByParams,
			    CurrentDateTime, TimeSwitchCond)
	end,
    is_reoccurrence(TimeSwitchCond, CurrentDateTime, PrepareNextByxxxMatchCall);

%% no weekly value supplied
byxxx_match(weekly = Level, NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< weekly - StartTime: ~p~n",[StartTime]), %DDD
    DefaultDateTime = get_default_datetime(Level, TimeSwitchCond, CurrentDateTime),
    Wkst = time_switch:get_wkst(TimeSwitchCond),
    %% Note: Year may be a different year than in StartTime
    YearWeek = ts_date:date_to_weekno(DefaultDateTime#date_time.date, Wkst),
    {_Y,_M,D} = Date = CurrentDateTime#date_time.date,
    YearDay = ts_date:date_to_dayno(Date),
    MonthDay = D,
    WeekDay = ts_date:dayno_to_daytype(calendar:day_of_the_week(Date)),
    NewStartTime = StartTime#start_time{weekno = YearWeek, yearday = YearDay,
					monthday = MonthDay, weekday = WeekDay},
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, ByParams, CurrentDateTime, TimeSwitchCond);

%% DAILY - - - - - - - - - - - - - - - - - -
%% daily value supplied, check that current time matches
byxxx_match(daily, daily, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< daily - StartTime: ~p~n",[StartTime]), %DDD
    PrepareNextByxxxMatchCall =
	fun() ->
 		{NextLevel, NextProcessLevel, NextLevelByParams} = get_next_level(daily, ByParams, TimeSwitchCond),
		byxxx_match(NextLevel, NextProcessLevel, LowestLevel, StartTime, NextLevelByParams,
			    CurrentDateTime, TimeSwitchCond)
	end,
    is_reoccurrence(TimeSwitchCond, CurrentDateTime, PrepareNextByxxxMatchCall);

%% no daily value supplied
byxxx_match(daily = Level, NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< daily - StartTime: ~p~n",[StartTime]), %DDD
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    byxxx_match(NextLevel, NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond);

%% BYWEEKNO - - - - - - - - - - - - - - - - - -
%% filter on weekno
byxxx_match(byweekno = Level, _NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< byweekno - StartTime: ~p~n",[StartTime]), %DDD
    MatchVal = StartTime#start_time.weekno,
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    filter_byxxx(NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
		 CurrentDateTime, TimeSwitchCond);

%% BYYEARDAY - - - - - - - - - - - - - - - - - -
%% filter on byyearday
byxxx_match(byyearday = Level, _NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    MatchVal = StartTime#start_time.yearday,
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    filter_byxxx(NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
		 CurrentDateTime, TimeSwitchCond);

%% BYMONTHDAY - - - - - - - - - - - - - - - - - -
byxxx_match(bymonthday = Level, _NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime,
	    TimeSwitchCond) ->
    %% io:format("<< bymonthday - StartTime: ~p~n",[StartTime]), %DDD
    MatchVal = StartTime#start_time.monthday,
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    filter_byxxx(NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
		 CurrentDateTime, TimeSwitchCond);

%% BYDAY - - - - - - - - - - - - - - - - - -
byxxx_match(byday = Level, _NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< byday - StartTime: ~p~n",[StartTime]), %DDD
    MatchVal = StartTime#start_time.weekday,
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    filter_byxxx(NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
		 CurrentDateTime, TimeSwitchCond);

%% HOURLY - - - - - - - - - - - - - - - - - -
%% hourly value supplied, check that current time matches
byxxx_match(hourly, hourly, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< hourly - StartTime: ~p~n",[StartTime]), %DDD
    PrepareNextByxxxMatchCall =
	fun() ->
		{H,_M,_S}  = CurrentDateTime#date_time.time,
		NewStartTime = StartTime#start_time{hour = H},
		{NextLevel, NextProcessLevel, NextLevelByParams} = get_next_level(hourly, ByParams, TimeSwitchCond),
		byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, NextLevelByParams,
			    CurrentDateTime, TimeSwitchCond)
	end,
    is_reoccurrence(TimeSwitchCond, CurrentDateTime, PrepareNextByxxxMatchCall);

%% no hourly value supplied
byxxx_match(hourly = Level, NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< hourly - StartTime: ~p~n",[StartTime]), %DDD
    DefaultDateTime = get_default_datetime(Level, TimeSwitchCond, CurrentDateTime),
    {H,_M,_S} = DefaultDateTime#date_time.time,
    NewStartTime = StartTime#start_time{hour = H},
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, ByParams, CurrentDateTime, TimeSwitchCond);

%% BYHOUR - - - - - - - - - - - - - - - - - -
byxxx_match(byhour = Level, _NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< byhour - StartTime: ~p~n",[StartTime]), %DDD
    MatchVal = StartTime#start_time.hour,
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    filter_byxxx(NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
		 CurrentDateTime, TimeSwitchCond);

%% MINUTELY - - - - - - - - - - - - - - - - - -
%% minutely value supplied, check that current time matches
byxxx_match(minutely, minutely, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< minutely - StartTime: ~p~n",[StartTime]), %DDD
    PrepareNextByxxxMatchCall =
	fun() ->
		{_H,M,_S}  = CurrentDateTime#date_time.time,
		NewStartTime = StartTime#start_time{minute = M},
		{NextLevel, NextProcessLevel, NextLevelByParams} = get_next_level(minutely, ByParams, TimeSwitchCond),
		byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, NextLevelByParams,
			    CurrentDateTime, TimeSwitchCond)
	end,
    is_reoccurrence(TimeSwitchCond, CurrentDateTime, PrepareNextByxxxMatchCall);

%% no minutely value supplied
byxxx_match(minutely = Level, NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< minutely - StartTime: ~p~n",[StartTime]), %DDD
    DefaultDateTime = get_default_datetime(Level, TimeSwitchCond, CurrentDateTime),
    {_H,M,_S} = DefaultDateTime#date_time.time,
    NewStartTime = StartTime#start_time{minute = M},
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, ByParams, CurrentDateTime, TimeSwitchCond);

%% BYMINUTE - - - - - - - - - - - - - - - - - -
byxxx_match(byminute = Level, _NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< byminute - StartTime: ~p~n",[StartTime]), %DDD
    MatchVal = StartTime#start_time.minute,
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    filter_byxxx(NextLevel, MatchVal, Level, LowestLevel, StartTime, ByParams,
		 CurrentDateTime, TimeSwitchCond);

%% SECONDLY - - - - - - - - - - - - - - - - - -
%% secondly value supplied, check that current time matches
 byxxx_match(secondly, secondly, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< secondly - StartTime: ~p~n",[StartTime]), %DDD
    PrepareNextByxxxMatchCall =
 	fun() ->
 		{_H,_M,S}  = CurrentDateTime#date_time.time,
 		NewStartTime = StartTime#start_time{second = S},
		{NextLevel, NextProcessLevel, NextLevelByParams} = get_next_level(secondly, ByParams, TimeSwitchCond),
		byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, NextLevelByParams,
			    CurrentDateTime, TimeSwitchCond)
 	end,
     is_reoccurrence(TimeSwitchCond, CurrentDateTime, PrepareNextByxxxMatchCall);

%% no secondly value supplied
byxxx_match(secondly = Level, NextProcessLevel, LowestLevel, StartTime, ByParams, CurrentDateTime, TimeSwitchCond) ->
    %% io:format("<< secondly - StartTime: ~p~n",[StartTime]), %DDD
    DefaultDateTime = get_default_datetime(Level, TimeSwitchCond, CurrentDateTime),
    {_H,_M,S} = DefaultDateTime#date_time.time,
    NewStartTime = StartTime#start_time{second = S},
    {NextLevel, _, _} = get_next_level(Level, ByParams, TimeSwitchCond),
    byxxx_match(NextLevel, NextProcessLevel, LowestLevel, NewStartTime, ByParams, CurrentDateTime, TimeSwitchCond);

%% BYSECOND - - - - - - - - - - - - - - - - - -
byxxx_match(bysecond, _NextLevel, _LowestLevel, _StartTime, _ByParams, _CurrentDateTime, _TimeSwitchCond) ->
    throw({error, bysecond_processed_as_a_non_base_case}).



%% return Default if Val = undefined, otherwise return Val
default(Val, Default) ->
    case Val of
	undefined -> Default;
	_ -> Val
    end.

%% convert start_time record() StartTime to a #date_time record and
%% replace any 'undefined' values with DtStart supplied values.
add_dtstart_modifiers(StartTime, DtStart) when is_record(StartTime, start_time) ->
    #start_time{
		       year = Year,
		       month = Month,
		       monthday = Day,
		       hour = Hour,
		       minute = Minute,
		       second = Second
		      } = StartTime,
    #date_time{date = {DtYear, DtMonth, DtDay}, time = {DtHour, DtMinute, DtSecond}} = DtStart,
    #date_time{date = {default(Year,DtYear), default(Month,DtMonth), default(Day, DtDay)},
	       time = {default(Hour,DtHour), default(Minute,DtMinute), default(Second, DtSecond)},
	       type = floating};

%% used with generate_counts(...) to set any unspecified values to dtstart values
add_dtstart_modifiers(StartTime, DtStart) when is_record(StartTime, date_time) ->
    #date_time{date = {Year, Month, Day}, time = {Hour, Minute, Second}} = StartTime,
    #date_time{date = {DtYear, DtMonth, DtDay}, time = {DtHour, DtMinute, DtSecond}} = DtStart,
    #date_time{date = {default(Year,DtYear), default(Month,DtMonth), default(Day, DtDay)},
	       time = {default(Hour,DtHour), default(Minute,DtMinute), default(Second, DtSecond)},
	       type = floating}.


%%--------------------------------------------------------------------
%% @spec    (Start, Freq, TimeSwitchCond) -> [#date_time{}]
%%
%%            Start          = #date_time{}
%%            ByType         = yearly   |
%%                             monthly  |
%%                             weekly   |
%%                             daily    |
%%                             hourly   |
%%                             minutely |
%%                             secondly
%%            TimeSwitchCond = term() "time_switch__cond_N (N >= 7)"
%%
%% @doc     get the starting points in the range specified by Start
%%          according to Freq and interval in time-switch Note :
%%          invalid dates may be created as well as date-time values
%%          beyond "until" - this should not be a problem as
%%          valid_range/3 in check_intervals/3 will handle the
%%          invalid dates and as "dtstart" `=<' current time `=<'
%%          "until" is check previously in_time_range/4 there should
%%          be no chance for date-times beyond "until" to match
%% @end
%%--------------------------------------------------------------------
create_startpoints(Start, yearly, _TimeSwitchCond) ->
    [Start];

create_startpoints(Start, monthly, TimeSwitchCond) ->
    #date_time{date = {Y,_M,_D}} = Start,
    RangeStart = #date_time{date = {Y,1,1}, time = {0,0,0}, type = floating},
    RangeEnd = #date_time{date = {Y,12,calendar:last_day_of_the_month(Y, 12)}, time = {23,59,59},
			  type = floating},
    create_startpoints_in_range(RangeStart, RangeEnd, TimeSwitchCond, Start);

create_startpoints(Start, weekly, TimeSwitchCond) ->
    #date_time{date = {Y,M,_D}} = Start,
    RangeStart = #date_time{date = {Y,M,1}, time = {0,0,0}, type = floating},
    RangeEnd = #date_time{date = {Y,M,calendar:last_day_of_the_month(Y, M)}, time = {23,59,59},
			  type = floating},
    create_startpoints_in_range(RangeStart, RangeEnd, TimeSwitchCond, Start);

create_startpoints(Start, daily, TimeSwitchCond) ->
    #date_time{date = {Y,M,_D}} = Start,
    RangeStart = #date_time{date = {Y,M,1}, time = {0,0,0}, type = floating},
    RangeEnd = #date_time{date = {Y,M,calendar:last_day_of_the_month(Y, M)}, time = {23,59,59},
			  type = floating},
    create_startpoints_in_range(RangeStart, RangeEnd, TimeSwitchCond, Start);

create_startpoints(Start, hourly, TimeSwitchCond) ->
    #date_time{date = Date} = Start,
    RangeStart = #date_time{date = Date, time = {0,0,0}, type = floating},
    RangeEnd = #date_time{date = Date, time = {23,59,59}, type = floating},
    create_startpoints_in_range(RangeStart, RangeEnd, TimeSwitchCond, Start);

create_startpoints(Start, minutely, TimeSwitchCond) ->
    #date_time{date = Date, time = {H,_M,_S}} = Start,
    RangeStart = #date_time{date = Date, time = {H,0,0}, type = floating},
    RangeEnd = #date_time{date = Date, time = {H,59,59}, type = floating},
    create_startpoints_in_range(RangeStart, RangeEnd, TimeSwitchCond, Start);

create_startpoints(Start, secondly, TimeSwitchCond) ->
    #date_time{date = Date, time = {H,M,_S}} = Start,
    RangeStart = #date_time{date = Date, time = {H,M,0}, type = floating},
    RangeEnd = #date_time{date = Date, time = {H,M,59}, type = floating},
    create_startpoints_in_range(RangeStart, RangeEnd, TimeSwitchCond, Start).

%% return all reoccurrence starpoints in range [RangeStart, RangeEnd]
create_startpoints_in_range(RangeStart, RangeEnd, TimeSwitchCond, _Start) ->
    DtStart = time_switch:get_dtstart(TimeSwitchCond),
    Interval = time_switch:get_interval(TimeSwitchCond),
    %% ensure that both Current and Start use the same #date_time.type
    Timezone = dummy,
    StartDateTime = RangeStart,
    EndDateTime = RangeEnd,
    Freq = time_switch:get_freq(TimeSwitchCond),
    StartDiff = ts_datetime:diff_datetime(Timezone, StartDateTime, DtStart, Freq),
    EndDiff = ts_datetime:diff_datetime(Timezone, EndDateTime, DtStart, Freq),
    %% determine how many occurence have occured (0 = the first one and so on ...)
    %% note: some may not actualy have occured due to DST and leap days
    StartFreqNo = StartDiff div Interval,
    EndFreqNo = EndDiff div Interval,
    FreqNos = lists:seq(StartFreqNo, EndFreqNo),
    StartPoints = [add_freq_step(TimeSwitchCond, DtStart, FreqNo) || FreqNo <- FreqNos],
    StartPoints.

%%--------------------------------------------------------------------
%% @spec    (Start, ByxxxVals, ByType, Wkst) -> [#date_time{}]
%%
%%            Start     = #date_time{}
%%            ByxxxVals = [{ByType, Val}]
%%            ByType    = bysecond   |
%%                        byminute   |
%%                        byhour     |
%%                        byday      |
%%                        bymonthday |
%%                        byyearday  |
%%                        byweekno   |
%%                        bymonth
%%            Wkst      = mo | tu | we | th | fr | sa | su "(weekday) first working day of the week"
%%
%% @doc     create all the time (starting) points defined by ByxxxVals
%%          - ByType associated with the time Start
%% @end
%%--------------------------------------------------------------------
create_startpoints(Start, ByxxxVals, bymonth, _Wkst) ->
    [begin
	 {Y,_M,D} = Start#date_time.date,
	 Start#date_time{date = {Y,Val,D}}
     end || {_, Val} <- ByxxxVals];

%% this functions doesn't try to constrain the generated week starting
%% points to weeks in the month M
create_startpoints(Start, ByxxxVals, byweekno, Wkst) ->
    {Y,_M,_D} = Start#date_time.date,
    F = fun({_,Val}, Acc) ->
		WeekNo = ts_date:get_week_no(Y, Wkst, Val),
		case WeekNo of
		    nth_week_does_not_exits ->
			Acc;
		    _ ->
			Date = ts_date:weekno_to_date(Y, Wkst, WeekNo),
			[Start#date_time{date = Date} | Acc]
		end
	end,
    lists:foldl(F, [], ByxxxVals);

%% this functions doesn't try to constrain the generated week starting
%% points to weeks in the month M
create_startpoints(Start, ByxxxVals, byyearday, _Wkst) ->
    {Y,_M,_D} = Start#date_time.date,
    F = fun({_,Val}, Acc) ->
	      case ts_date:byyearday_to_date(Y,Val) of
		  nth_day_does_not_exist ->
		      Acc;
		  Date ->
		      [Start#date_time{date = Date} | Acc]
	      end
      end,
    lists:foldl(F, [], ByxxxVals);

create_startpoints(Start, ByxxxVals, bymonthday, _Wkst) ->
    {Y,M,_D} = Start#date_time.date,
    F = fun({_,Val}, Acc) ->
		Day = ts_date:normalize_monthday(Y, M, Val),
		Date = {Y,M,Day},
		case calendar:valid_date(Date) of
		    true -> [Start#date_time{date = Date} | Acc];
		    false -> Acc
		end
	end,
    lists:foldl(F, [], ByxxxVals);

create_startpoints(Start, ByxxxVals, byday, _Wkst) ->
    {Y,M,_D} = Start#date_time.date,
    %% find all days in month that are ByxxxVals weekdays
    F = fun({_ByType, {all, DayType}}, Acc) ->
		AllDays = ts_date:all_byday_in_month(Y,M,DayType),
		[Start#date_time{date = Date} || Date <- AllDays] ++ Acc;
	   ({_ByType, {Nth, DayType}}, Acc) ->
		case ts_date:nth_byday_in_month(Y, M, Nth, DayType) of
		    nth_day_does_not_exist ->
			Acc;
		    Date ->
			[Start#date_time{date = Date} | Acc]
		end
	end,
    %% remove possible duplicates ( e.g. ByxxxVals = [{byday,{1,mo}}, {byday,{all,mo}}] )
    lists:usort(lists:foldl(F, [], ByxxxVals));

create_startpoints(Start, ByxxxVals, byhour, _Wkst) ->
    [begin
	 {_H,M,S} = Start#date_time.time,
	 Start#date_time{time = {Val,M,S}}
     end || {_, Val} <- ByxxxVals];

create_startpoints(Start, ByxxxVals, byminute, _Wkst) ->
    [begin
	 {H,_M,S} = Start#date_time.time,
	 Start#date_time{time = {H,Val,S}}
     end || {_, Val} <- ByxxxVals];

create_startpoints(Start, ByxxxVals, bysecond, _Wkst) ->
    [begin
	 {H,M,_S} = Start#date_time.time,
	 Start#date_time{time = {H,M,Val}}
     end || {_, Val} <- ByxxxVals].


%% check all the [StartPoint, StartPoint + Duration] intervals to see
%% if CurrentDateTime is part of any of them
check_intervals(StartPoints, TimeSwitchCond, CurrentDateTime) ->

%%     io:format("StartPoints ~p~nTimeSwitchCond ~p~nCurrentDateTime ~p~n", % DDD
%% 	      [StartPoints, TimeSwitchCond, CurrentDateTime]),

    Duration = ts_duration:duration(TimeSwitchCond),

%%     io:format("Duration ~p~n",[Duration]), %DDD

    Timezone = dummy,
    Current = ts_datetime:datetime_to_usec(start, Timezone, CurrentDateTime),

%%    io:format("Current ~p~n",[Current]), %DDD

    F = fun(StartPoint) ->
		case valid_range(Timezone, StartPoint, Duration) of
		    no_range -> false;
		    {US, UE} -> in_usec_range(US, UE, Current)
		end
	end,
    lists:any(F, StartPoints).

%%--------------------------------------------------------------------
%% @spec    (Level, Freq) -> true | false
%%
%%            Level = atom()
%%            Freq  = atom() "time_switch__cond_x.freq"
%%
%% @doc     is Level `<' freq,
%% @end
%%--------------------------------------------------------------------
level_lt_freq(Level, Freq) ->
    get_rank(Level) < get_rank(Freq).


%% retrieve date-time information from current time when Level >= Freq
%% and otherwise from dtstart
%% return date_time record()
get_default_datetime(Level, TimeSwitchCond, CurrentDateTime) ->
    case level_lt_freq(Level, time_switch:get_freq(TimeSwitchCond)) of
	true -> time_switch:get_dtstart(TimeSwitchCond);
	false -> CurrentDateTime
    end.

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    interpret_time_test:test(),

    ok.


