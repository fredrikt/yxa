%%%-------------------------------------------------------------------
%%% File    : ts_duration.erl
%%% @author   Håkan Stenholm <hsten@it.su.se>
%%% @doc      Functions to used to handle "duration" values in
%%%           time-switch
%%%
%%% @since    21 Feb 2005 by Håkan Stenholm <hsten@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
%%--------------------------------------------------------------------

-module(ts_duration).

%% -behaviour().

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 duration/1,
	 duration_to_seconds/1,
	 valid_duration/1,
	 sec_to_duration/1,
	 sub_second/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("cpl.hrl").

%% -include_lib("").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond) -> #duration{}
%%
%%            TimeSwitchCond = #time_switch__cond_2{} |
%%                             #time_switch__cond_5{} |
%%                             #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%
%% @doc     calculate duration based on time-switch parameters, if
%%          "duration" is not supplied, "dtstend - dtstart" is used
%% @end
%%--------------------------------------------------------------------
duration(TimeSwitchCond) ->
    {Start, Duration} = {time_switch:get_dtstart(TimeSwitchCond), time_switch:get_dtend_duration(TimeSwitchCond)},
    case Duration of
	{dtend, DateTime} ->
	    Timezone = dummy,
	    Seconds = ts_datetime:diff_datetime(Timezone, DateTime, Start, secondly) + 1,
	    #duration{seconds = Seconds};
	{duration, DurationRec} ->
	    DurationRec
    end.


%%--------------------------------------------------------------------
%% @spec    (Duration) -> integer()
%%
%%            Duration = #duration{}
%%
%% @doc     convert duration record to time in seconds
%% @end
%%--------------------------------------------------------------------
duration_to_seconds(Duration) when is_record(Duration, duration) ->
    #duration{
	  weeks = W,   % integer()
	  days = D,    % integer()
	  hours = H,   % integer()
	  minutes = M, % integer()
	  seconds = S  % integer()
	 } = Duration,
    S + (M * ?SecInMin) + (H * ?SecInHour) + (D * ?SecInDay) + (W * ?SecInWeek).


%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond) -> true | false
%%
%%            TimeSwitchCond = #time_switch__cond_2{} |
%%                             #time_switch__cond_5{} |
%%                             #time_switch__cond_7{} |
%%                             #time_switch__cond_8{}
%%
%% @doc     determine if duration of a reoccurrence will always be
%%          short enough so that no overlap can occur between
%%          reoccurrences Note : time_switch__cond_2 (which doesn't
%%          do reoccurrences) always returns 'true' Note :
%%          valid_duration/1 checks that TimeSwitchCond will be valid
%%          from dtstart until the end of time. This means that some
%%          duration values that may have been valid in the range
%%          [dtstart, until] are rejected as invalid.
%% @end
%%--------------------------------------------------------------------
valid_duration(TimeSwitchCond) when is_record(TimeSwitchCond, time_switch__cond_2) ->
    true;

valid_duration(TimeSwitchCond) when is_record(TimeSwitchCond, time_switch__cond_5) ->
    valid_5(TimeSwitchCond);

valid_duration(TimeSwitchCond) when is_record(TimeSwitchCond, time_switch__cond_7);
 				    is_record(TimeSwitchCond, time_switch__cond_8) ->
    Freq = time_switch:get_freq(TimeSwitchCond),
    ByParams = interpret_time:sort_byparam(time_switch:get_by_values(TimeSwitchCond)),
    LowestLevel = interpret_time:get_lowest_level(Freq, ByParams),
    case interpret_time:level_type(LowestLevel) of
 	filter ->
 	    Vals = [ValN || {LN,ValN} <- ByParams, LN == LowestLevel],
 	    MinReoccurSec = min_reoccur_sec(Vals, LowestLevel),
 	    DurationSec = duration_to_seconds(duration(TimeSwitchCond)),
	    DurationSec =< MinReoccurSec;
 	generator ->
 	    valid_5(TimeSwitchCond)
    end.

%%--------------------------------------------------------------------
%% check that the duration of the reoccurring interval is =< than the interval ("freq" * "interval")
valid_5(TimeSwitchCond) ->
    FSecs = freq_secs(TimeSwitchCond),
    DurationSec = duration_to_seconds(duration(TimeSwitchCond)),
    DurationSec =< FSecs.

freq_secs(TimeSwitchCond) ->
    Freq = time_switch:get_freq(TimeSwitchCond),
    Interval = time_switch:get_interval(TimeSwitchCond),
    case Freq of
	yearly ->
	    Interval * ?SecInYear;
	monthly ->
	    %% XXX ? could it be made larger if Interval > 1 (all month are not 28 days long) ?
	    Interval * ?SecInDay * 28;
	weekly ->
	    Interval * ?SecInWeek;
	daily ->
	    Interval * ?SecInDay;
	hourly ->
	    Interval * ?SecInHour;
	minutely ->
	    Interval * ?SecInMin;
	 secondly ->
	    Interval * 1
    end.

%%--------------------------------------------------------------------
%% return longest reoccurrence period (in seconds) found between Vals
%% note: all Vals values here, are converted into positive values with
%%       proper ordering and duplicate elements are removed (so that
%%       no duration = 0 occur)
%%
min_reoccur_sec(Vals, Type) when Type == bymonth;
				 Type == byhour;
				 Type == byminute;
				 Type == bysecond ->
    [FirstV | _] = Vals,
    MaxDur = 367, % value larger than possible in Vals
    BestDur = min_reoccur_sec(FirstV, lists:sort(Vals), Type, MaxDur),
    {_,StepSec} = by_end_step(Type),
    BestDur * StepSec;

%%
min_reoccur_sec(Vals, Type) when Type == bymonthday;
				 Type == byyearday;
				 Type == byweekno ->
    MaxDur = 367, % value larger than possible in Vals
    {RangeEnds, StepSec} = by_end_step(Type),
    BestDur = lists:min(lists:map(fun(RangeEnd) ->
				    %% convert -N values to N
				    %% use usort to remove possible duplicats
				    Vals2 = lists:usort([end_count_to_front_count(N,RangeEnd) || N <- Vals]),
				    [FirstV | _] = Vals2,
				    min_reoccur_sec(FirstV, Vals2, Type, MaxDur, RangeEnd)
			    end,
			    RangeEnds)),
    BestDur * StepSec;

%%
min_reoccur_sec(Vals, Type) when Type == byday ->
    MaxDur = 367, % value larger than possible in Vals
    {_, StepSec} = by_end_step(Type),
    MonthDaySets = byday_testsets(),
    NumByVals = [{P, ts_date:day_type_to_no(DayName)} || {P, DayName} <- Vals],
    BestDur = lists:min(lists:map(fun(MonthDays) ->
				    %% convert -N/N/all values to day numbers (bymonthday)
				    %% use usort to remove possible duplicats
				    Vals2 = lists:usort(match_byday(NumByVals, MonthDays)),
				    FirstV = first_day_in_next_month(MonthDays, NumByVals),
				    min_reoccur_sec(FirstV, Vals2, Type, MaxDur, length(MonthDays))
			    end,
			    MonthDaySets)),
    BestDur * StepSec.

%%--------------------------------------------------------------------

first_day_in_next_month(MonthDays, NumByVals) ->
    RL = lists:reverse(MonthDays),
    [{_, LastWeekDayNo} | _] = RL,
    MonthLength = length(MonthDays),
    NextMonthLengths = case MonthLength of
			   28 -> [31];
			   29 -> [31];
			   30 -> [31];
			   31 -> [28,29,30,31]
		       end,
    NextMonthSet = [monthdays(Len, next_weekday(LastWeekDayNo)) || Len <- NextMonthLengths],
    FirstDay = lists:min(lists:append([first_match_byday(NumByVals, NextMonth) || NextMonth <- NextMonthSet])),
    FirstDay.

%% ByVals = {byday, Val}
%%          Val = list() of {N, Day}
%%          Day = integer() range = [1,7] (= [mo-su])
%%          N   = -1 or less | 1 or greater | all
%% MonthDays = list() of {MonthDayNo, Day}
%%
%% return: [] (may occur for length 28-30 months) | [FirstMonthDay]
first_match_byday(ByDayVals, MonthDays) ->
    case match_byday(ByDayVals, MonthDays) of
	[FirstMonthDayNo | _] ->
	    [FirstMonthDayNo];
	[] ->
	    []
    end.

%%--------------------------------------------------------------------

next_weekday(CurrentDay) ->
    case CurrentDay of
	7 -> 1;
	_ -> CurrentDay + 1
    end.

%%--------------------------------------------------------------------
%% bymonth | byday (if no N modifiers are used) | byhour | byminute | bysecond
%% this is the simple version of min_reoccur_sec(...) where all Vals
%% have fixed ranges (e.g. bymonth = 1-12 ...)
min_reoccur_sec(FirstV, [V], Type, BestDur) ->
    {RangeEnd,_} = by_end_step(Type),
    Diff = RangeEnd - V,
    Dur = Diff + FirstV,
    lists:min([Dur, BestDur]);
min_reoccur_sec(FirstV, [V1, V2 | R], Type, BestDur) ->
    Dur = V2 - V1,
    min_reoccur_sec(FirstV, [V2 | R], Type, lists:min([Dur, BestDur])).

%% bymonthday | byyearday | byweekno
%% base case, may also occur if monthday/yearday/week didn't fit in month/year of length RangeEnd
min_reoccur_sec(_FirstV, [], _Type, BestDur, _RangeEnd) ->
    BestDur;
%% drop monthday/yearday/week which doesn't fit in month/year of length RangeEnd
%% this means that there may be no valid values in Vals for certain RangeEnd lengths
%% e.g. the 31st monthday when RangeEnd = 30
min_reoccur_sec(FirstV, [V | R], Type, BestDur, RangeEnd) when V > RangeEnd; V < 1 ->
    min_reoccur_sec(FirstV, R, Type, BestDur, RangeEnd);
%% last element in Vals found
min_reoccur_sec(FirstV, [V], _Type, BestDur, RangeEnd) ->
    %% first occurrence in next month/year is assumed to fit in next month/year
    Diff = RangeEnd - V,
    Dur = Diff + FirstV,
    lists:min([BestDur, Dur]);
%% compare two elements in Vals
min_reoccur_sec(FirstV, [V1, V2 | R], Type, BestDur, RangeEnd) ->
    Dur = V2 - V1,
    min_reoccur_sec(FirstV, [V2 | R], Type, lists:min([Dur, BestDur]), RangeEnd).


%% return: {RangeEnd, StepSec},
%%         RangeEnd = max value for month | weekday | hour ...
%%         StepSec  = number of seconds in a time unit of Type
by_end_step(Type) ->
    case Type of
	bymonthday ->
	    {[28,29,30,31], ?SecInDay};
	byyearday ->
	    {[365,366], ?SecInDay};
	byweekno ->
	    {[52,53], ?SecInDay * 7};
	bymonth	->
	    {12, ?SecInDay * 28};
	%% named days in month
	byday ->
	    {[28,29,30,31], ?SecInDay};
	    %% {7, ?SecInDay};
 	byhour ->
	    {23, ?SecInHour};
 	byminute ->
	    {59, ?SecInMin};
 	bysecond ->
	    {59, 1}
    end.


%% convert -N count to +N index
end_count_to_front_count(N,_CountLength) when N > 0 ->
    N;
end_count_to_front_count(N,CountLength) when N < 0 ->
    Nabs = - N,
    CountLength - (Nabs-1).


%% return the possible lists of {DayNo,WeekDay} tuples
byday_testsets() ->
    [monthdays(MLen, WeekStart) || MLen <- [28,29,30,31], WeekStart <- [1,2,3,4,5,6,7]].


%% ByVals = {byday, Val}
%%          Val = list() of {N, Day}
%%          Day = integer() range = [1,7] (= [mo-su])
%%          N   = -1 or less | 1 or greater | all
%% MonthDays = list() of {MonthDayNo, Day}
%%
%% get list() of dayno in month for a month MonthDays (generated with monthdays/2)
%% return: list() of integer()
%%
match_byday(ByDayVals, MonthDays) ->
    lists:usort(match_byday(ByDayVals, MonthDays, lists:reverse(MonthDays), length(MonthDays))).

%% get monthdays (as positiv indexes)
match_byday([], _MonthDays, _RevMonthDays, _MLength) ->
    [];

match_byday([{all,WeekDay} | R], MonthDays, RevMonthDays, MLength) ->
    DayNos = [No || {No,Day} <- MonthDays, Day == WeekDay],
    DayNos ++ match_byday(R, MonthDays, RevMonthDays, MLength);

match_byday([{N,WeekDay} | R], MonthDays, RevMonthDays, MLength) when N > 0 ->
    case day_no_of_nth_day(MonthDays, WeekDay, N) of
	no_day -> match_byday(R, MonthDays, RevMonthDays, MLength);
	DayNo -> [DayNo | match_byday(R, MonthDays, RevMonthDays, MLength)]
    end;

match_byday([{N,WeekDay} | R], MonthDays, RevMonthDays, MLength) when N < 0 ->
    N2 = -N,
    case day_no_of_nth_day(RevMonthDays, WeekDay, N2) of
	no_day -> match_byday(R, MonthDays, RevMonthDays, MLength);
	DayNo -> [DayNo | match_byday(R, MonthDays, RevMonthDays, MLength)]
    end.

%% day_no_of_nth_day(MonthDays, Day, Nth)
%% return: integer() | no_day
day_no_of_nth_day([], _Day, _Nth) ->
    no_day;
day_no_of_nth_day([{DayNo,Day} | _R], Day, 1) ->
    DayNo;
day_no_of_nth_day([{_,Day} | R], Day, Nth) ->
    day_no_of_nth_day(R, Day, Nth-1);
day_no_of_nth_day([{_,_} | R], Day, Nth) ->
    day_no_of_nth_day(R, Day, Nth).

monthdays(MonthLength, StartWeekDayNo) ->
    monthdays(1, MonthLength, StartWeekDayNo).

monthdays(MonthLength, MonthLength, WeekDayNo) ->
    [{MonthLength, WeekDayNo}];
monthdays(CurrentDay, MonthLength, WeekDayNo) ->
    NextWeekDay = next_weekday(WeekDayNo),
    [{CurrentDay, WeekDayNo} | monthdays(CurrentDay+1, MonthLength, NextWeekDay)].

%%--------------------------------------------------------------------
%% @spec    (Sec) -> #duration{}
%%
%%            Sec = integer()
%%
%% @doc     convert Sec to weeks, days, ... seconds - start with
%%          setting as big a time units as possible e.g. 3750 seconds
%%          -> hour = 1, minute = 2, second = 30
%% @end
%%--------------------------------------------------------------------
sec_to_duration(Sec) when Sec >= 0 ->
    Weeks = Sec div ?SecInWeek,
    R1 = Sec - (Weeks * ?SecInWeek),
    Days = R1 div ?SecInDay,
    R2 = R1 - (Days * ?SecInDay),
    Hours = R2 div ?SecInHour,
    R3 = R2 - (Hours * ?SecInHour),
    Minutes = R3 div ?SecInMin,
    R4 = R3 - (Minutes * ?SecInMin),
    Seconds = R4,

    #duration{
		weeks = Weeks,
		days = Days,
		hours = Hours,
		minutes = Minutes,
		seconds = Seconds
	       }.

%%--------------------------------------------------------------------
%% @spec    (Duration) -> #duration{}
%%
%%            Duration = #duration{}
%%
%% @doc     do Duration - 1 second Note : Duration must be >= 1 second
%% @end
%%--------------------------------------------------------------------
sub_second(Duration) when is_record(Duration, duration) ->
    Sec = duration_to_seconds(Duration),
    sec_to_duration(Sec - 1).

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
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------

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

    test1(),

    test2(),

    test3(),

    test4(),

    %% valid_duration(TimeSwitchCond)
    %%--------------------------------------------------------------------
    %% time_switch__cond_2
    %%----------------------------
    %% no reoccurrence
    autotest:mark(?LINE, "valid_duration/1 - 1"),
    TimeSwitchCond1 = #time_switch__cond_2{
      dtstart = #date_time{date = {2005,1,1}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}}
     },
    true = valid_duration(TimeSwitchCond1),

    %% time_switch__cond_5
    %%----------------------------
    %% freq base reoccurrence - duration < reoccurrence
    autotest:mark(?LINE, "valid_duration/1 - 2"),
    TimeSwitchCond2 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,1}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = daily,
      interval = 1,
      until_count = repeat_forever
     },
    true = valid_duration(TimeSwitchCond2),

    %% freq base reoccurrence - duration > reoccurrence
    autotest:mark(?LINE, "valid_duration/1 - 2"),
    TimeSwitchCond3 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,1}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = hourly,
      interval = 1,
      until_count = repeat_forever
     },
    false = valid_duration(TimeSwitchCond3),

   %% freq base reoccurrence - duration == reoccurrence
    autotest:mark(?LINE, "valid_duration/1 - 4"),
    TimeSwitchCond4 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,1}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = hourly,
      interval = 8,
      until_count = repeat_forever
     },
    true = valid_duration(TimeSwitchCond4),

    %% freq base reoccurrence - duration > reoccurrence by 1 second
    autotest:mark(?LINE, "valid_duration/1 - 5"),
    TimeSwitchCond5 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,1}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, seconds = 1}},
      freq = hourly,
      interval = 8,
      until_count = repeat_forever
     },
    false = valid_duration(TimeSwitchCond5),

    %% time_switch__cond_7/8
    %%----------------------------

    %% freq base reoccurrence - duration == reoccurrence
    autotest:mark(?LINE, "valid_duration/1 - 6"),
    TimeSwitchCond6 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 2}},
      freq = daily,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byhour,2}, {byhour,4}, {byhour,23}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond6),

    %% freq base reoccurrence - duration == reoccurrence (use dtend instead of duration)
    autotest:mark(?LINE, "valid_duration/1 - 7"),
    TimeSwitchCond7 =
	TimeSwitchCond6#time_switch__cond_7{
	  dtend_duration = {dtend, #date_time{date = {2005,1,1}, time = {0,2,0}, type = floating}}
	 },
    true = valid_duration(TimeSwitchCond7),

    %% freq base reoccurrence - duration == reoccurrence
    %% [M/25, M+1/2] => days = 25,26,27,28,1 => Duration =< 5 days
    autotest:mark(?LINE, "valid_duration/1 - 8"),
    TimeSwitchCond8 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 5}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonthday,2}, {bymonthday,15}, {bymonthday,25}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond8),

    %% freq base reoccurrence - duration > reoccurrence by 1 second
    autotest:mark(?LINE, "valid_duration/1 - 9"),
    TimeSwitchCond9 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 5, seconds = 1}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonthday,2}, {bymonthday,15}, {bymonthday,25}],
      wkst = mo
     },
    false = valid_duration(TimeSwitchCond9),

    %% freq base reoccurrence - duration == reoccurrence, when _single_ bymonthday = 31
    autotest:mark(?LINE, "valid_duration/1 - 10"),
    TimeSwitchCond10 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 31}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonthday,31}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond10),

    %% -N positions
    %%----------------------------
    %% freq base reoccurrence - days = 5,10,[24,25,26,27] => best duration = 5 (day 5 - 10)
    autotest:mark(?LINE, "valid_duration/1 - 11"),
    TimeSwitchCond11 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 5}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonthday,5}, {bymonthday,-5}, {bymonthday,10}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond11),

    %% freq base reoccurrence - days = 5,[19,20,21,22],[24,25,26,27] =>
    %% best duration = 5 (day 19-24, 20-25, ...)
    autotest:mark(?LINE, "valid_duration/1 - 12"),
    TimeSwitchCond12 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 5}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonthday,5}, {bymonthday,-5}, {bymonthday,-10}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond12),


    %% byday N/-N/all
    %%----------------------------
    %% freq base reoccurrence - all (duration =< 2 days [sa,mo] is the shortest range)
    autotest:mark(?LINE, "valid_duration/1 - 13"),
    TimeSwitchCond13 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 2}},
      freq = daily,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday,{all,mo}}, {byday,{all,we}}, {byday,{all,sa}}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond13),

    %% freq base reoccurrence - N, Shortest range = [M/27 (4th sa), M+1/1 (1st mo)] days = 27,28
    autotest:mark(?LINE, "valid_duration/1 - 14"),
    TimeSwitchCond14 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 2}},
      freq = daily,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday,{1,mo}}, {byday,{2,we}}, {byday,{4,sa}}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond14),

    %% freq base reoccurrence - -N (duration =< 2, 18(-2we) 21(-2sa) 23(-1mo) month length = 28/29)
    autotest:mark(?LINE, "valid_duration/1 - 15"),
    TimeSwitchCond15 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 2}},
      freq = daily,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday,{-1,mo}}, {byday,{-2,we}}, {byday,{-2,sa}}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond15),

    %% freq base reoccurrence - -N (duration 1 second to long)
    autotest:mark(?LINE, "valid_duration/1 - 16"),
    TimeSwitchCond16 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 2, seconds = 1}},
      freq = daily,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday,{-1,mo}}, {byday,{-2,we}}, {byday,{-2,sa}}],
      wkst = mo
     },
    false = valid_duration(TimeSwitchCond16),

    %% freq base reoccurrence - N/-N/all (duration =< 2, range [th,sa] => days = th,fr)
    autotest:mark(?LINE, "valid_duration/1 - 17"),
    TimeSwitchCond17 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 2}},
      freq = daily,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday,{1,mo}}, {byday,{all,th}}, {byday,{-1,sa}}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond17),

    %% xml_parse_test.erl
    %%----------------------------
    %% test 7a :
    %%  <time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\"
    %%          byday=\"MO,TU,WE,TH,FR\">
    autotest:mark(?LINE, "valid_duration/1 - 18"),
    TimeSwitchCond18 = #time_switch__cond_7{
      dtstart = #date_time{date = {2000,7,3}, time = {9,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = weekly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday,{all,mo}}, {byday,{all,tu}}, {byday,{all,we}}, {byday,{all,th}},
		   {byday,{all,fr}}, {byday,{all,sa}}, {byday,{all,su}}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond18),

    %% maximum min_diff is 48 hours (mo -> we), monday range = [mo 9:00, we 8:59]
    %% interval > 1
    autotest:mark(?LINE, "valid_duration/1 - 19"),
    TimeSwitchCond19 = #time_switch__cond_7{
      dtstart = #date_time{date = {2000,7,3}, time = {9,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 48}},
      freq = weekly,
      interval = 2,
      until_count = repeat_forever,
      by_values = [{byday,{all,mo}}, {byday,{all,we}}, {byday,{all,fr}}],
      wkst = mo
     },
    true = valid_duration(TimeSwitchCond19),

    autotest:mark(?LINE, "valid_duration/1 - 20"),
    TimeSwitchCond20 = TimeSwitchCond19#time_switch__cond_7{
			 dtend_duration = {duration, #duration{hours = 48, seconds = 1}}
			},
    false = valid_duration(TimeSwitchCond20),

    ok.


test1() ->
    %% duration_to_seconds(Duration)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "duration_to_seconds/1 - 1"),
    D_a1 = #duration{weeks = 1, days = 1, hours = 1, minutes = 1, seconds = 1},
    S_a1 = ?SecInWeek + ?SecInDay + ?SecInHour + ?SecInMin + 1,
    S_a1 = duration_to_seconds(D_a1),

    autotest:mark(?LINE, "duration_to_seconds/1 - 2"),
    D_a2 = #duration{weeks = 0, days = 0, hours = 0, minutes = 0, seconds = 0},
    S_a2 = 0,
    S_a2 = duration_to_seconds(D_a2),

    autotest:mark(?LINE, "duration_to_seconds/1 - 3"),
    D_a3 = #duration{weeks = 3, days = 4, hours = 5, minutes = 6, seconds = 7},
    S_a3 = (3 * ?SecInWeek) + (4 * ?SecInDay) + (5 * ?SecInHour) + (6 * ?SecInMin) + 7,
    S_a3 = duration_to_seconds(D_a3),
    ok.

test2() ->
    %% sec_to_duration(Sec)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "sec_to_duration/1 - 1"),
    D_b1 = #duration{weeks = 1, days = 1, hours = 1, minutes = 1, seconds = 1},
    S_b1 = ?SecInWeek + ?SecInDay + ?SecInHour + ?SecInMin + 1,
    D_b1 = sec_to_duration(S_b1),

    autotest:mark(?LINE, "sec_to_duration/1 - 2"),
    D_b2 = #duration{weeks = 0, days = 0, hours = 0, minutes = 0, seconds = 0},
    S_b2 = 0,
    D_b2 = sec_to_duration(S_b2),

    autotest:mark(?LINE, "sec_to_duration/1 - 3"),
    D_b3 = #duration{weeks = 3, days = 4, hours = 5, minutes = 6, seconds = 7},
    S_b3 = (3 * ?SecInWeek) + (4 * ?SecInDay) + (5 * ?SecInHour) + (6 * ?SecInMin) + 7,
    D_b3 = sec_to_duration(S_b3),
    ok.

test3() ->
    %% duration_to_seconds(Duration), sec_to_duration(Sec)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "duration_to_seconds/1, sec_to_duration/1 - 1"),
    D1 = #duration{weeks = 0, days = 0, hours = 0, minutes = 0, seconds = 0},
    D1 = sec_to_duration(duration_to_seconds(D1)),

    autotest:mark(?LINE, "duration_to_seconds/1, sec_to_duration/1 - 2"),
    D2 = #duration{weeks = 1, days = 0, hours = 0, minutes = 0, seconds = 0},
    D2 = sec_to_duration(duration_to_seconds(D2)),

    autotest:mark(?LINE, "duration_to_seconds/1, sec_to_duration/1 - 3"),
    D3 = #duration{weeks = 0, days = 1, hours = 0, minutes = 0, seconds = 0},
    D3 = sec_to_duration(duration_to_seconds(D3)),

    autotest:mark(?LINE, "duration_to_seconds/1, sec_to_duration/1 - 4"),
    D4 = #duration{weeks = 0, days = 0, hours = 1, minutes = 0, seconds = 0},
    D4 = sec_to_duration(duration_to_seconds(D4)),

    autotest:mark(?LINE, "duration_to_seconds/1, sec_to_duration/1 - 5"),
    D5 = #duration{weeks = 0, days = 0, hours = 0, minutes = 1, seconds = 0},
    D5 = sec_to_duration(duration_to_seconds(D5)),

    autotest:mark(?LINE, "duration_to_seconds/1, sec_to_duration/1 - 6"),
    D6 = #duration{weeks = 0, days = 0, hours = 0, minutes = 0, seconds = 1},
    D6 = sec_to_duration(duration_to_seconds(D6)),

    autotest:mark(?LINE, "duration_to_seconds/1, sec_to_duration/1 - 6"),
    D7 = #duration{weeks = 1, days = 1, hours = 1, minutes = 1, seconds = 1},
    D7 = sec_to_duration(duration_to_seconds(D7)),
    ok.

test4() ->
    %% sub_second(Duration)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "sub_second/1 - 1"),
    D_c1a = #duration{weeks = 1, days = 1, hours = 1, minutes = 1, seconds = 1},
    D_c1b = #duration{weeks = 1, days = 1, hours = 1, minutes = 1, seconds = 0},
    D_c1b = sub_second(D_c1a),

    autotest:mark(?LINE, "sub_second/1 - 2"),
    D_c2a = #duration{weeks = 3, days = 4, hours = 5, minutes = 6, seconds = 7},
    D_c2b = #duration{weeks = 3, days = 4, hours = 5, minutes = 6, seconds = 6},
    D_c2b = sub_second(D_c2a),

    autotest:mark(?LINE, "sub_second/1 - 3"),
    D_c3a = #duration{weeks = 3, days = 4, hours = 5, minutes = 6, seconds = 0},
    D_c3b = #duration{weeks = 3, days = 4, hours = 5, minutes = 5, seconds = 59},
    D_c3b = sub_second(D_c3a),

    autotest:mark(?LINE, "sub_second/1 - 4"),
    D_c4a = #duration{weeks = 3, days = 0, hours = 0, minutes = 0, seconds = 0},
    D_c4b = #duration{weeks = 2, days = 6, hours = 23, minutes = 59, seconds = 59},
    D_c4b = sub_second(D_c4a),
    ok.

