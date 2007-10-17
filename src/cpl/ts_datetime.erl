%% This module contains functions used manipulate date_time records
%%--------------------------------------------------------------------

-module(ts_datetime).

%% -behaviour().

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 datetime_to_usec/3,
	 dtstart_lt_dtend/3,
	 add_datetime_and_duration/2,
	 add_seconds/2,
	 add_minutes/2,
	 add_hours/2,
	 add_days/2,
	 add_weeks/2,
	 unsafe_add_months/2,
	 unsafe_add_years/2,
	 diff_datetime/4,
	 sub_days/2,
	 datetime_to_type/3,
	 gt_datetime/3,
	 le_datetime/3,
	 ge_datetime/3,

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
%% @spec    (Type, Timezone, DateTime) ->
%%            integer() | {pseudo_usec, Time}
%%
%%            Type     = start | stop
%%            Timezone = term() "currently unsupported"
%%            DateTime = #date_time{} "(must be a valid date-time, but may be a value skipped during change to DST)"
%%
%%            Time = integer()
%%
%% @doc     converts date_time time to a universal format (gregorian
%%          seconds in UTC format), Type is ONLY used to determine
%%          which time to use when DateTime ends a DST period - use
%%          'start' if the DateTime is the first time in a range and
%%          'end' if it is the end of a time range Note :
%%          {pseudo_usec, Time} is returned when a non-existent time
%%          in a date-time is supplied, this occurs only when a
%%          floating time is changed to DST, e.g. a 02:xx:xx value is
%%          set to 03:00:00 (assuming DST change occurs at 02:00:00).
%%          Note : "If neither "tzid" nor "tzurl" [Timezone] are
%%          present, all non-UTC times within this time switch should
%%          be interpreted as being "floating" times, i.e., that they
%%          are specified in the local time-zone of the CPL server."
%%          - RFC 3880 chapter 4.4 p15 * tzid and tzurl are currently
%%          unsupported
%% @end
%%--------------------------------------------------------------------

%% Timezone is ignored when #date_time.type = UTC, UTC never uses
%% time-zone or daylight saving time information - if it did, it
%% wouldn't be a universal format - which it is
datetime_to_usec(_Type, _Timezone, #date_time{date = Date, time = Time, type = utc}) ->
    DateTime = {Date, Time},
    calendar:datetime_to_gregorian_seconds(DateTime);

%% XXX floating time is currently not adjusted with the selected
%% time-zone and applicable daylight saving - the servers local time-zone
%% and daylight saving is used
datetime_to_usec(Type, _Timezone, #date_time{date = Date, time = Time, type = floating}) ->
    DateTime = {Date, Time},
    UDateTime2 = interpret_time:safe_local_time_to_universal_time_dst(DateTime),
    case UDateTime2 of
	%% "For a local DateTime during the period that is skipped when
	%%  switching to daylight saving time, there is no corresponding UTC
	%%  since the local time is illegal - it has never happened." - OTP R10B-1 doc
	[] ->
	    %% time moved forward 1 hour due to DST, e.g. in a 02:00:00 to 03:00:00 change
	    %% any 02:xx:xx value should be set to 03:00:00 - the hour is skipped
	    {H,_M,_S} = Time,
	    DstStartTime = {H,0,0},
	    DstStartDateTime = #date_time{date = Date, time = DstStartTime, type = floating},
	    DstEndDateTime = add_hours(DstStartDateTime, 1),
	    #date_time{date = D, time = T} = DstEndDateTime,
	    [UDateTime] = interpret_time:safe_local_time_to_universal_time_dst({D,T}),
	    USec = calendar:datetime_to_gregorian_seconds(UDateTime),
	    {pseudo_usec, USec};

	%% "For a local DateTime during the period that is repeated when
	%%  switching from daylight saving time, there are two corresponding
	%%  UTCs. One for the first instance of the period when daylight
	%%  saving time is still active, and one for the second instance." - OTP R10B-1 doc
	[DstDateTimeUTC, DateTimeUTC] ->
	    case Type of
		start ->
		    calendar:datetime_to_gregorian_seconds(DstDateTimeUTC);
		stop ->
		    calendar:datetime_to_gregorian_seconds(DateTimeUTC)
	    end;
	%% "For all other local times there is only one corresponding UTC." - OTP R10B-1 doc
	[DateTimeUTC] ->
	    calendar:datetime_to_gregorian_seconds(DateTimeUTC)
    end.

%%--------------------------------------------------------------------

%% @spec    (Timezone, S, E) -> true | false
%%
%%            Timezone = term() "currently unsupported"
%%            S        = #date_time{}
%%            E        = #date_time{}
%%
%% @doc     ensure that dtstart `<' dtend in a time_switch__cond_x
%%          record(). E and S are assumed to have valid date and time
%%          values, they may only be invalid due to transition to DST
%%          Note : see cpl/README about date-time format limitations,
%%          in regard to floating date-time values without time-zone
%%          settings.
%% @end
%%--------------------------------------------------------------------
dtstart_lt_dtend(Timezone, S, E) when is_record(S, date_time), is_record(E, date_time) ->
    %% convert time to gregorian (utc) seconds
    Start = ts_datetime:datetime_to_usec(start, Timezone, S),
    End = ts_datetime:datetime_to_usec(stop, Timezone, E),

    case {Start, End} of
	{{pseudo_usec, Time1}, {pseudo_usec, Time2}} -> Time1 < Time2;
	{Start, {pseudo_usec, Time2}} -> Start < Time2;
	%% Time-1 moves Time back to last legal second - the one before change to DST
	{{pseudo_usec, Time1} , End} -> (Time1-1) < End;
	%% the regular nice case
	{Start, End} -> Start < End
    end.

%%--------------------------------------------------------------------
%% @spec    (DateTime, Duration) -> #date_time{}
%%
%%            DateTime = #date_time{}
%%            Duration = #duration{}
%%
%% @doc     addition is done in a "natural" way, e.g.:
%%          DateTime = 2005-01-01 00:00:00 and Duration = 1 day ->
%%          range = [2005-01-01 00:00:00, 2005-01-01 23:59:59]
%%          i.e. the range consists of 60 * 60 * 24 seconds (1 day) -
%%          leap seconds and DST are ignored, so that the range end
%%          will always be the same, independent of the actual number
%%          of passed seconds. Note that each second counts as a
%%          range of Duration = 1 second, rather than a point in
%%          time, this implies:
%%          DateTime = 2005-01-01 00:00:00 and Duration = 1 second ->
%%          range = [2005-01-01 00:00:00, 2005-01-01 00:00:00]
%%          Note : duration record() don't handle years and month so
%%          the date part of a date can never become illegal, but the
%%          time part may become so due to change to DST
%% @end
%%--------------------------------------------------------------------
add_datetime_and_duration(DateTime, Duration) ->
    Duration2 = ts_duration:sub_second(Duration),

    DT1 = add_seconds(DateTime, Duration2#duration.seconds),
    DT2 = add_minutes(DT1, Duration2#duration.minutes),
    DT3 = add_hours(DT2, Duration2#duration.hours),
    DT4 = add_days(DT3, Duration2#duration.days),
    DT5 = add_weeks(DT4, Duration2#duration.weeks),
    DT5.

%%--------------------------------------------------------------------
%% @spec    (DateTime, xxxTime) -> #date_time{}
%%
%%            DateTime = #date_time{}
%%            xxxTime  = integer()
%%            xxx      = seconds | minutes | hours | days | weeks
%%
%% @doc     add xxxTime number of xxx time units to DateTime
%% @end
%%--------------------------------------------------------------------

%% @clear

add_seconds(#date_time{time = Time} = DT, Sec) ->
    {H, M, PossibleLeapSec} = Time,
    %% ignore possible leap seconds
    S = case PossibleLeapSec of
	    60 -> 59;
	    _ -> PossibleLeapSec
	end,
    NewSec = (S + Sec) rem 60,
    ExtraMin = (S + Sec) div 60,
    add_minutes(DT#date_time{time = {H,M,NewSec}}, ExtraMin).

add_minutes(#date_time{time = Time} = DT, Min) ->
    {H,M,S} = Time,
    NewMin = (M + Min) rem 60,
    ExtraH = (M + Min) div 60,
    add_hours(DT#date_time{time = {H,NewMin,S}}, ExtraH).

add_hours(#date_time{time = Time} = DT, Hour) ->
    {H,M,S} = Time,
    NewH = (H + Hour) rem 24,
    ExtraD = (H + Hour) div 24,
    add_days(DT#date_time{time = {NewH,M,S}}, ExtraD).

add_days(#date_time{date = Date} = DT, Day) ->
    {Y,M,D} = Date,
    NoOfDays = calendar:last_day_of_the_month(Y, M),
    RemainingDaysInMonth = NoOfDays - D,

    case (RemainingDaysInMonth >= Day) of
	true ->
	    DT#date_time{date = {Y,M,D+Day}};
	false ->
	    %% some days in Day don't fit in the current month
	    case M of
		12 ->
		    %% last month in year, move both year and month forward by 1
		    %% consume RemainingDaysInMonth + 1 days through this action
		    add_days(DT#date_time{date = {Y+1, 1, 1}}, Day - (RemainingDaysInMonth + 1));
		_ ->
		    %% not last month in year, move only month forward by 1
		    %% consume RemainingDaysInMonth + 1 days through this action
		    add_days(DT#date_time{date = {Y, M+1, 1}}, Day - (RemainingDaysInMonth + 1))
	    end
    end.

add_weeks(DT, Week) ->
    add_days(DT, Week * 7).

%%--------------------------------------------------------------------
%% @spec    (DT, Month) ->
%%            #date_time{} "values may be illegal i.e. 2005-03-31 + 1 month -> 2005-04-31"
%%
%%            DT    = #date_time{}
%%            Month = integer() "number of months"
%%
%% @doc     needed for adding longer periods
%% @end
%%--------------------------------------------------------------------
unsafe_add_months(#date_time{date = Date} = DT, Month) ->
    {Y, M, D} = Date,
    NewM = (M + Month) rem 12,
    ExtraY = (M + Month) div 12,
    NewDate = {Y + ExtraY, NewM, D},
    DT#date_time{date = NewDate}.

%%--------------------------------------------------------------------
%% @spec    (DT, Year) ->
%%            #date_time{} "values may be illegal i.e. 2005-03-31 + 1 month -> 2005-04-31"
%%
%%            DT   = #date_time{}
%%            Year = integer() "number of years"
%%
%% @doc     needed for adding longer periods
%% @end
%%--------------------------------------------------------------------
unsafe_add_years(#date_time{date = Date} = DT, Year) ->
    {Y, M, D} = Date,
    NewDate = {Y+Year, M, D},
    DT#date_time{date = NewDate}.

%%--------------------------------------------------------------------
%% @spec    (Timezone, DT1, DT2, Freq) -> integer()
%%
%%            Timezone = term() "currently unused"
%%            DT1      = #date_time{}
%%            DT2      = #date_time{}
%%            Freq     = term() "see #time_switch__cond_8{} comments in cpl.hrl"
%%
%% @doc     get difference between DT1 and DT2 ~ abs(DT1 - DT2), for a
%%          certain time unit (supplied by Freq) Note : worst ordo =
%%          days_in_range/2 ordo
%% @end
%%--------------------------------------------------------------------
diff_datetime(Timezone, DT1v, DT2v, Freq) ->
    %% don't convert datetime formats if they allready match
    {DT1, DT2} = case DT1v#date_time.type == DT2v#date_time.type of
		     true -> {DT1v, DT2v};
		     false ->
			 {datetime_to_type(DT1v, utc, Timezone),
			  datetime_to_type(DT2v, utc, Timezone)
			 }
		 end,
    case gt_datetime(Timezone, DT1, DT2) of
	true ->
	    diff_datetime2(Timezone, DT1, DT2, Freq);
	false ->
	    diff_datetime2(Timezone, DT2, DT1, Freq)
    end.


%% Note    : assumes that DT1 >= DT2
%% Note    : worst ordo = days_in_range/2 ordo
%%
%% XXX this code relies heavily on days_in_range/2 which is a O(N)
%%     (N = abs(DT1 - DT2) time interval) operation - can it be made
%%     linear ? - yes see internals of calendar.erl
%%--------------------------------------------------------------------
diff_datetime2(_Timezone, DT1, DT2, Freq) ->
    {Year1, Month1, Day1} = DT1#date_time.date,
    {Hour1, Minute1, Second1} = DT1#date_time.time,
    {Year2, Month2, Day2} = DT2#date_time.date,
    {Hour2, Minute2, Second2} = DT2#date_time.time,

    case Freq of
	yearly ->
	    Year1 - Year2;
	monthly ->
	    diff_monthly(Year2, Month2, Year1, Month1);
	daily ->
	    ts_date:days_in_range({Year2,Month2,Day2}, {Year1,Month1,Day1}) - 1;
	weekly ->
	    %% diff_weeks(Date1,Date2),
	    (ts_date:days_in_range({Year2,Month2,Day2}, {Year1,Month1,Day1}) - 1) div 7;
	hourly ->
	    diff_hourly(Year2, Month2, Day2, Hour2, Year1, Month1, Day1, Hour1);
	minutely ->
	    diff_minutely(Year2, Month2, Day2, Hour2, Minute2, Year1, Month1, Day1, Hour1, Minute1);
	secondly ->
	    diff_secondly(Year2, Month2, Day2, Hour2, Minute2, Second2,
			  Year1, Month1, Day1, Hour1, Minute1, Second1)
    end.


diff_monthly(Year2, Month2, Year1, Month1) ->
    YearDiff = Year1 - Year2,
    case YearDiff of
	0 ->
	    Month1 - Month2;
	_ ->
	    ((YearDiff - 1) * 12) + (12 - Month2) + Month1
    end.

diff_hourly(Year2, Month2, Day2, Hour2, Year1, Month1, Day1, Hour1) ->
    DayDiff = ts_date:days_in_range({Year2,Month2,Day2}, {Year1,Month1,Day1}) - 1,
    case DayDiff of
	0 ->
	    Hour1 - Hour2;
	_ ->
	    ((DayDiff - 1) * 24) + (24 - Hour2) + Hour1
    end.

diff_minutely(Year2, Month2, Day2, Hour2, Minute2, Year1, Month1, Day1, Hour1, Minute1) ->
    HourDiff = diff_hourly(Year2, Month2, Day2, Hour2, Year1, Month1, Day1, Hour1),
    case HourDiff of
	0 ->
	    Minute1 - Minute2;
	_ ->
	    ((HourDiff -1) * 60) + (60 - Minute2) + Minute1
    end.

diff_secondly(Year2, Month2, Day2, Hour2, Minute2, Second2,
	      Year1, Month1, Day1, Hour1, Minute1, Second1) ->
    MinuteDiff = diff_minutely(Year2, Month2, Day2, Hour2, Minute2, Year1, Month1, Day1, Hour1, Minute1),
    case MinuteDiff of
	0 ->
	    Second1 - Second2;
	_ ->
	    ((MinuteDiff -1) * 60) + (60 - Second2) + Second1
    end.

%% subtract N days from DateTime, N >= 0
sub_days(DateTime, Days) ->
    {Y,M,D} = DateTime#date_time.date,
    DRes = D - Days,
    case DRes >= 1 of
	true ->
	    DateTime#date_time{date = {Y,M,DRes}};
	false ->
	    {NewY, NewM} = case M > 1 of
			       true -> {Y, M - 1};
			       false -> {Y - 1, 12}
			   end,
	    NewD = calendar:last_day_of_the_month(NewY, NewM),
	    sub_days(DateTime#date_time{date = {NewY,NewM,NewD}}, Days - D)
    end.

%%--------------------------------------------------------------------
%% @spec    (DateTime, Type, Timezone) -> #date_time{}
%%
%%            DateTime = #date_time{}
%%            Type     = utc | floating
%%            Timezone = term() "currently unsupported"
%%
%% @doc     Convert DateTime between various time representations
%%          Timezone is currently unsupported and should when
%%          supported, only be used when Type = floating Note : This
%%          code expects DateTime to contain a legal date and time
%%          value, so no skipped - transition to DST hour or
%%          non-existing leap days
%% @end
%%--------------------------------------------------------------------
datetime_to_type(DateTime, Type, _Timezone) ->
    #date_time{date = Date, time = Time, type = CurrentType} = DateTime,
    case {CurrentType, Type} of
	{utc, utc} ->
	    DateTime;
	{floating, utc} ->
	    case interpret_time:safe_local_time_to_universal_time_dst({Date,Time}) of
		[{D,T}] ->
		    #date_time{date = D, time = T, type = utc};
		[_DTDst, DTnoDST] ->
		    {D,T} = DTnoDST,
		    #date_time{date = D, time = T, type = utc}
	    end;
	{utc, floating} ->
	    {D,T} = calendar:universal_time_to_local_time({Date, Time}),
	    #date_time{date = D, time = T, type = floating};
	{floating, floating} ->
	    DateTime
    end.

%%--------------------------------------------------------------------
%% @spec    (Timezone, DT1, DT2) -> true | false
%%
%%            Timezone = term() "currently not supported"
%%            DT1      = #date_time{}
%%            DT2      = #date_time{}
%%
%% @doc     determine if DT1 > DT2
%% @end
%%--------------------------------------------------------------------
gt_datetime(_Timezone, DT1, DT2) ->
    %% ordering is from most to least significant date-time value
    %% so this comparison will work as expected
    {DT1#date_time.date, DT1#date_time.time} > {DT2#date_time.date, DT2#date_time.time}.

%%--------------------------------------------------------------------
%% @spec    (Timezone, DT1, DT2) -> true | false
%%
%%            Timezone = term() "currently not supported"
%%            DT1      = #date_time{}
%%            DT2      = #date_time{}
%%
%% @doc     determine if DT1 `=<' DT2
%% @end
%%--------------------------------------------------------------------
le_datetime(_Timezone, DT1, DT2) ->
    %% ordering is from most to least significant date-time value
    %% so this comparison will work as expected
    {DT1#date_time.date, DT1#date_time.time} =< {DT2#date_time.date, DT2#date_time.time}.

%%--------------------------------------------------------------------
%% @spec    (Timezone, DT1, DT2) -> true | false
%%
%%            Timezone = term() "currently not supported"
%%            DT1      = #date_time{}
%%            DT2      = #date_time{}
%%
%% @doc     determine if DT1 `>=' DT2
%% @end
%%--------------------------------------------------------------------
ge_datetime(_Timezone, DT1, DT2) ->
    %% ordering is from most to least significant date-time value
    %% so this comparison will work as expected
    {DT1#date_time.date, DT1#date_time.time} >= {DT2#date_time.date, DT2#date_time.time}.

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

    %% datetime_to_type(DateTime, Type, Timezone)
    %%--------------------------------------------------------------------
    %% utc -> utc
    autotest:mark(?LINE, "datetime_to_type/3 - 1"),
    DTTDateTime1 = #date_time{date = {2005,5,15}, time = {12,0,0}, type = utc},
    DTTDateTime1 = datetime_to_type(DTTDateTime1, utc, dummy),

    %% floating -> floating
    autotest:mark(?LINE, "datetime_to_type/3 - 2"),
    DTTDateTime2 = #date_time{date = {2005,5,15}, time = {12,0,0}, type = floating},
    DTTDateTime2 = datetime_to_type(DTTDateTime2, floating, dummy),

    %% floating (no DST) -> utc
    autotest:mark(?LINE, "datetime_to_type/3 - 3"),
    DTTDateTime3a = #date_time{date = {2005,1,15}, time = {11 + cpl_test_util:timezone_offset(),0,0}, type = floating},
    DTTDateTime3b = #date_time{date = {2005,1,15}, time = {11,0,0}, type = utc},
    DTTDateTime3b = datetime_to_type(DTTDateTime3a, utc, dummy),

    %% utc -> floating (no DST)
    autotest:mark(?LINE, "datetime_to_type/3 - 4"),
    DTTDateTime4a = #date_time{date = {2005,1,15}, time = {12,0,0}, type = utc},
    DTTDateTime4b = #date_time{date = {2005,1,15}, time = {12 + cpl_test_util:timezone_offset(),0,0}, type = floating},
    DTTDateTime4b = datetime_to_type(DTTDateTime4a, floating, dummy),

    %% floating (DST -> no DST transition) -> utc
    autotest:mark(?LINE, "datetime_to_type/3 - 5 - WARNING: test assumes that local = EU"),
    DTTDateTime5a = #date_time{date = {2005,10,30}, time = {1 + cpl_test_util:timezone_offset(),59,59},
			       type = floating},
    DTTDateTime5b = #date_time{date = {2005,10,30}, time = {1,59,59}, type = utc},
    DTTDateTime5b = datetime_to_type(DTTDateTime5a, utc, dummy),

    %% floating (DST) -> utc
    autotest:mark(?LINE, "datetime_to_type/3 - 6 - WARNING: test assumes that local = EU"),
    DTTDateTime6a = #date_time{date = {2005,6,30}, time = {2 + cpl_test_util:timezone_offset(),0,0}, type = floating},
    DTTDateTime6b = #date_time{date = {2005,6,30}, time = {1,0,0}, type = utc},
    DTTDateTime6b = datetime_to_type(DTTDateTime6a, utc, dummy),


    ok.

%% add_xxxx/0
%%--------------------------------------------------------------------
%% Test add_xxxx/0, xxx = second - weeks (not months or years).
%% each test checks simple add where value doesn't overflow
%% and a case where it overflows into the next higher element
%% (sec -> minute ... etc)
test1() ->
    autotest:mark(?LINE, "add_seconds/2 - 1"),
    DT1 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
    R1 = add_seconds(DT1, 5),
    #date_time{date = {2005,1,1}, time = {0,0,5}, type = utc} = R1,

    autotest:mark(?LINE, "add_seconds/2 - 2"),
    DT2 = #date_time{date = {2005,1,1}, time = {0,0,5}, type = utc},
    R2 = add_seconds(DT2, 55),
    #date_time{date = {2005,1,1}, time = {0,1,0}, type = utc} = R2,

    autotest:mark(?LINE, "add_minutes/2 - 1"),
    DT3 = #date_time{date = {2005,1,1}, time = {0,1,0}, type = utc},
    R3 = add_minutes(DT3, 1),
    #date_time{date = {2005,1,1}, time = {0,2,0}, type = utc} = R3,

    autotest:mark(?LINE, "add_minutes/2 - 2"),
    DT4 = #date_time{date = {2005,1,1}, time = {0,2,0}, type = utc},
    R4 = add_minutes(DT4, 58),
    #date_time{date = {2005,1,1}, time = {1,0,0}, type = utc} = R4,

    autotest:mark(?LINE, "add_hours/2 - 1"),
    DT5 = #date_time{date = {2005,1,1}, time = {1,0,0}, type = utc},
    R5 = add_hours(DT5, 1),
    #date_time{date = {2005,1,1}, time = {2,0,0}, type = utc} = R5,

    autotest:mark(?LINE, "add_hours/2 - 2"),
    DT6 = #date_time{date = {2005,1,1}, time = {2,0,0}, type = utc},
    R6 = add_hours(DT6, 22),
    #date_time{date = {2005,1,2}, time = {0,0,0}, type = utc} = R6,

    autotest:mark(?LINE, "add_days/2 - 1"),
    DT7 = #date_time{date = {2005,1,2}, time = {0,0,0}, type = utc},
    R7 = add_days(DT7, 1),
    #date_time{date = {2005,1,3}, time = {0,0,0}, type = utc} = R7,

    autotest:mark(?LINE, "add_days/2 - 2"),
    DT8 = #date_time{date = {2005,1,3}, time = {0,0,0}, type = utc},
    R8 = add_days(DT8, 29),
    #date_time{date = {2005,2,1}, time = {0,0,0}, type = utc} = R8,

    autotest:mark(?LINE, "add_weeks/2 - 1"),
    DT9 = #date_time{date = {2005,1,2}, time = {0,0,0}, type = utc},
    R9 = add_weeks(DT9, 1),
    #date_time{date = {2005,1,9}, time = {0,0,0}, type = utc} = R9,

    autotest:mark(?LINE, "add_weeks/2 - 2"),
    DT10 = #date_time{date = {2005,1,9}, time = {0,0,0}, type = utc},
    R10 = add_weeks(DT10, 4),
    #date_time{date = {2005,2,6}, time = {0,0,0}, type = utc} = R10,

    ok.

%% add_datetime_and_duration/2
%%--------------------------------------------------------------------
%% similar to test1 but tests multi level overflow as well as
%% adding several different time units at once
test2() ->
    %% overflow to new year
    autotest:mark(?LINE, "add_datetime_and_duration/2 - 1"),
    DT1 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
    Dur1 = #duration{
      weeks = 53
     },
    R1 = add_datetime_and_duration(DT1, Dur1),
    %% #date_time{date = {2006, 1, 7}, time = {0,0,0}, type = utc} = R1,
    #date_time{date = {2006, 1, 6}, time = {23,59,59}, type = utc} = R1,

    %% overflow to new year
    autotest:mark(?LINE, "add_datetime_and_duration/2 - 2"),
    DT2 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
    Dur2 = #duration{
      %% days = 365
      days = 366
     },
    R2 = add_datetime_and_duration(DT2, Dur2),
    %% #date_time{date = {2006, 1, 1}, time = {0,0,0}, type = utc} = R2,
    #date_time{date = {2006, 1, 1}, time = {23,59,59}, type = utc} = R2,

    %% overflow to new day
    autotest:mark(?LINE, "add_datetime_and_duration/2 - 3"),
    DT3 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
    Dur3 = #duration{
      hours = 100
     },
    R3 = add_datetime_and_duration(DT3, Dur3),
    %% #date_time{date = {2005,1,5}, time = {4,0,0}, type = utc} = R3,
    #date_time{date = {2005,1,5}, time = {3,59,59}, type = utc} = R3,

    %% overflow to new month
    autotest:mark(?LINE, "add_datetime_and_duration/2 - 4"),
    DT4 = #date_time{date = {2005,1,31}, time = {0,0,0}, type = utc},
    Dur4 = #duration{
      minutes = (60 * 24 * 2) + 2
     },
    R4 = add_datetime_and_duration(DT4, Dur4),
    %% #date_time{date = {2005,2,2}, time = {0,2,0}, type = utc} = R4,
    #date_time{date = {2005,2,2}, time = {0,1,59}, type = utc} = R4,

    %% overflow to new year
    autotest:mark(?LINE, "add_datetime_and_duration/2 - 5"),
    DT5 = #date_time{date = {2005,12,31}, time = {0,0,0}, type = utc},
    Dur5 = #duration{
      seconds = (60 * 60 * 24) + (60 * 60 * 12) + (60 * 5) + 30
     },
    R5 = add_datetime_and_duration(DT5, Dur5),
    %% #date_time{date = {2006,1,1}, time = {12,5,30}, type = utc} = R5,
    #date_time{date = {2006,1,1}, time = {12,5,29}, type = utc} = R5,

    %% add multiple values

    %% no overflows
    autotest:mark(?LINE, "add_datetime_and_duration/2 - 6"),
    DT6 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
    Dur6 = #duration{
      weeks = 1,
      days = 1,
      hours = 1,
      minutes = 1,
      seconds = 1
     },
    R6 = add_datetime_and_duration(DT6, Dur6),
    %% #date_time{date = {2005, 1, 9}, time = {1,1,1}, type = utc} = R6,
    #date_time{date = {2005, 1, 9}, time = {1,1,0}, type = utc} = R6,

    %% owerflow
    autotest:mark(?LINE, "add_datetime_and_duration/2 - 7"),
    DT7 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
    Dur7 = #duration{
      weeks = 10,
      days = 100,
      hours = 100,
      minutes = 100,
      seconds = 100
     },
    R7 = add_datetime_and_duration(DT7, Dur7),
    %% #date_time{date = {2005,6,24}, time = {5,41,40}, type = utc} = R7,
    #date_time{date = {2005,6,24}, time = {5,41,39}, type = utc} = R7,

    %% some overflow and some other date-time start values
    autotest:mark(?LINE, "add_datetime_and_duration/2 - 8"),
    DT8 = #date_time{date = {2005,4,24}, time = {15,3,29}, type = utc},
    Dur8 = #duration{
      weeks = 3,
      days = 5,
      hours = 7,
      minutes = 360,
      seconds = 13
     },
    R8 = add_datetime_and_duration(DT8, Dur8),
    %% #date_time{date = {2005,5,21}, time = {4,3,42}, type = utc} = R8,
    #date_time{date = {2005,5,21}, time = {4,3,41}, type = utc} = R8,

    ok.


%% datetime_to_usec/2
%%--------------------------------------------------------------------
%% Note: DST switching MUST be conformant to EU standard (selected
%%       time in test is based on when change over is done)

test3() ->
    %% DST offset (in hours)
    DST = 1,

    %% no DST in effect
    autotest:mark(?LINE, "datetime_to_usec/2 - 1 - WARNING: test assumes that local = EU"),
    U1 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 27},
						   time = {12, 0, 0}, type = utc}),
    U2 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 27},
						   time = {12 + cpl_test_util:timezone_offset(), 0, 0},
						   type = floating}),
    U1 = U2,

    %% DST in effect
    autotest:mark(?LINE, "datetime_to_usec/2 - 2 - WARNING: test assumes that local = EU"),
    U3 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						   time = {12, 0, 0}, type = utc}),
    U4 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						   time = {11 + DST + cpl_test_util:timezone_offset(), 0, 0},
						   type = floating}),
    U3 /= U4,

    %% DST in effect
    autotest:mark(?LINE, "datetime_to_usec/2 - 3 - WARNING: test assumes that local = EU"),
    U5 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						   time = {12, 0, 0}, type = utc}),
    U6 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						   time = {12 + DST + cpl_test_util:timezone_offset(), 0, 0},
						   type = floating}),
    U5 = U6,

    %% changing to DST

    %% switch to DST at 1:00:00 UTC
    autotest:mark(?LINE, "datetime_to_usec/2 - 4 - WARNING: test assumes that local = EU"),
    R7 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						   time = {1 + DST + cpl_test_util:timezone_offset(), 0, 0},
						   type = floating}),
    Time7 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						      time = {1, 0, 0}, type = utc}),
    R7 = Time7,

    %% end of non-existent time interval, last second of it - 2:59:59 if
    %% local = sweden (illegal range = 2:00:00 - 2:59:59)
    autotest:mark(?LINE, "datetime_to_usec/2 - 5 - WARNING: test assumes that local = EU"),
    R8 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						   time = {1 + cpl_test_util:timezone_offset(), 59, 59},
						   type = floating}),
    {pseudo_usec, T8} = R8,
    Time8 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						      time = {1, 0, 0}, type = utc}),
    T8 = Time8,

    %% first datetime after change to DST
    autotest:mark(?LINE, "datetime_to_usec/2 - 6 - WARNING: test assumes that local = EU"),
    R9 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						   time = {1 + DST + cpl_test_util:timezone_offset(), 0, 1},
						   type = floating}),
    Time9 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						      time = {1, 0, 1}, type = utc}),
    R9 = Time9,

    %% start of non-existent time interval, first second of it - 2:00:00 if
    %% local = sweden (illegal range = 2:00:00 - 2:59:59)
    autotest:mark(?LINE, "datetime_to_usec/2 - 7 - WARNING: test assumes that local = EU"),
    R10 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						    time = {1 + cpl_test_util:timezone_offset(), 0, 0},
						    type = floating}),
    {pseudo_usec, T10} = R10,
    Time10 = datetime_to_usec(start, dummy, #date_time{date = {2004, 3, 28},
						       time = {1, 0, 0}, type = utc}),
    T10 = Time10,


    %% changing back to non DST

    %% DST = false, first non DST modified time
    autotest:mark(?LINE, "datetime_to_usec/2 - 8 - WARNING: test assumes that local = EU"),
    R11 = datetime_to_usec(stop, dummy, #date_time{date = {2005, 10, 30},
						   time = {1 + cpl_test_util:timezone_offset(), 0, 0},
						   type = floating}),
    Time11 = datetime_to_usec(start, dummy, #date_time{date = {2005, 10, 30},
						       time = {1, 0, 0}, type = utc}),
    R11 = Time11,

    %% DST = false, last possible duplicate time value
    autotest:mark(?LINE, "datetime_to_usec/2 - 9 - WARNING: test assumes that local = EU"),
    R12 = datetime_to_usec(stop, dummy, #date_time{date = {2005, 10, 30},
						   time = {1 + cpl_test_util:timezone_offset(), 59, 59},
						   type = floating}),
    Time12 = datetime_to_usec(start, dummy, #date_time{date = {2005, 10, 30},
						       time = {1, 59, 59}, type = utc}),
    R12 = Time12,

    %% DST = true, first duplicate time value
    autotest:mark(?LINE, "datetime_to_usec/2 - 10 - WARNING: test assumes that local = EU"),
    R13 = datetime_to_usec(start, dummy, #date_time{date = {2005, 10, 30},
						    time = {0 + DST + cpl_test_util:timezone_offset(), 0, 0},
						    type = floating}),
    Time13 = datetime_to_usec(start, dummy, #date_time{date = {2005, 10, 30},
						       time = {0, 0, 0}, type = utc}),
    R13 = Time13,

    %% first occurence (while DST = true) of last duplicat value
    autotest:mark(?LINE, "datetime_to_usec/2 - 11 - WARNING: test assumes that local = EU"),
    R14 = datetime_to_usec(start, dummy, #date_time{date = {2005, 10, 30},
						    time = {0 + DST + cpl_test_util:timezone_offset(), 59, 59},
						    type = floating}),
    Time14 = datetime_to_usec(start, dummy, #date_time{date = {2005, 10, 30},
						       time = {0, 59, 59}, type = utc}),
    R14 = Time14,

    ok.


%% diff_datetime/4
%%--------------------------------------------------------------------
test4() ->
    Timezone = dummy,

    autotest:mark(?LINE, "diff_datetime/4 - 1"),
    DT1a = #date_time{date = {2006, 1, 1}, time = {0, 0, 0}, type = utc},
    DT1b = #date_time{date = {2005, 1, 1}, time = {0, 0, 0}, type = utc},
    1 = diff_datetime(Timezone, DT1a, DT1b, yearly),

    autotest:mark(?LINE, "diff_datetime/4 - 2"),
    DT2a = #date_time{date = {2005, 1, 1}, time = {0, 0, 0}, type = utc},
    DT2b = #date_time{date = {2005, 1, 1}, time = {0, 0, 0}, type = utc},
    0 = diff_datetime(Timezone, DT2a, DT2b, yearly),

    autotest:mark(?LINE, "diff_datetime/4 - 3"),
    DT3a = #date_time{date = {2005, 1, 1}, time = {0, 0, 0}, type = utc},
    DT3b = #date_time{date = {2005, 2, 1}, time = {0, 0, 0}, type = utc},
    31 = diff_datetime(Timezone, DT3a, DT3b, daily),

    autotest:mark(?LINE, "diff_datetime/4 - 4"),
    DT4a = #date_time{date = {2005, 1, 10}, time = {0, 0, 0}, type = utc},
    DT4b = #date_time{date = {2005, 2, 5}, time = {0, 0, 0}, type = utc},
    26 = diff_datetime(Timezone, DT4a, DT4b, daily),

    autotest:mark(?LINE, "diff_datetime/4 - 5"),
    DT5a = #date_time{date = {2005, 1, 1}, time = {0, 0, 0}, type = utc},
    DT5b = #date_time{date = {2005, 4, 1}, time = {0, 0, 0}, type = utc},
    3 = diff_datetime(Timezone, DT5a, DT5b, monthly),

    autotest:mark(?LINE, "diff_datetime/4 - 6"),
    DT6a = #date_time{date = {2005, 1, 10}, time = {0, 0, 0}, type = utc},
    DT6b = #date_time{date = {2005, 1, 5}, time = {0, 0, 0}, type = utc},
    0 = diff_datetime(Timezone, DT6a, DT6b, monthly),

    autotest:mark(?LINE, "diff_datetime/4 - 7"),
    DT7a = #date_time{date = {2005, 1, 1}, time = {0, 0, 0}, type = utc},
    DT7b = #date_time{date = {2005, 4, 10}, time = {0, 0, 0}, type = utc},
    14 = diff_datetime(Timezone, DT7a, DT7b, weekly),

    autotest:mark(?LINE, "diff_datetime/4 - 8"),
    DT8a = #date_time{date = {2005, 1, 10}, time = {0, 0, 0}, type = utc},
    DT8b = #date_time{date = {2005, 2, 5}, time = {0, 0, 0}, type = utc},
    3 = diff_datetime(Timezone, DT8a, DT8b, weekly),

    autotest:mark(?LINE, "diff_datetime/4 - 9"),
    DT9a = #date_time{date = {2003, 5, 1}, time = {0, 0, 0}, type = utc},
    DT9b = #date_time{date = {2005, 4, 1}, time = {0, 0, 0}, type = utc},
    23 = diff_datetime(Timezone, DT9a, DT9b, monthly),

    autotest:mark(?LINE, "diff_datetime/4 - 10"),
    DT10a = #date_time{date = {2005, 5, 1}, time = {10, 0, 0}, type = utc},
    DT10b = #date_time{date = {2005, 5, 1}, time = {17, 0, 0}, type = utc},
    7 = diff_datetime(Timezone, DT10a, DT10b, hourly),

    autotest:mark(?LINE, "diff_datetime/4 - 11"),
    DT11a = #date_time{date = {2005, 1, 4}, time = {10, 0, 0}, type = utc},
    DT11b = #date_time{date = {2005, 1, 5}, time = {17, 0, 0}, type = utc},
    31 = diff_datetime(Timezone, DT11a, DT11b, hourly),

    autotest:mark(?LINE, "diff_datetime/4 - 12"),
    DT12a = #date_time{date = {2005, 1, 4}, time = {10, 0, 0}, type = utc},
    DT12b = #date_time{date = {2005, 1, 6}, time = {3, 0, 0}, type = utc},
    41 = diff_datetime(Timezone, DT12a, DT12b, hourly),

    autotest:mark(?LINE, "diff_datetime/4 - 13"),
    DT13a = #date_time{date = {2005, 1, 4}, time = {10, 0, 0}, type = utc},
    DT13b = #date_time{date = {2005, 3, 6}, time = {3, 0, 0}, type = utc},
    1457  = diff_datetime(Timezone, DT13a, DT13b, hourly),

    autotest:mark(?LINE, "diff_datetime/4 - 14"),
    DT14a = #date_time{date = {2005, 12, 31}, time = {10, 0, 0}, type = utc},
    DT14b = #date_time{date = {2006, 1, 1}, time = {3, 0, 0}, type = utc},
    17 = diff_datetime(Timezone, DT14a, DT14b, hourly),

    autotest:mark(?LINE, "diff_datetime/4 - 15"),
    DT15a = #date_time{date = {2004, 12, 31}, time = {10, 0, 0}, type = utc},
    DT15b = #date_time{date = {2006, 1, 1}, time = {3, 0, 0}, type = utc},
    8777 = diff_datetime(Timezone, DT15a, DT15b, hourly),

    autotest:mark(?LINE, "diff_datetime/4 - 16"),
    DT16a = #date_time{date = {2004, 12, 31}, time = {10, 5, 0}, type = utc},
    DT16b = #date_time{date = {2006, 1, 1}, time = {3, 45, 0}, type = utc},
    526660 = diff_datetime(Timezone, DT16a, DT16b, minutely),

    autotest:mark(?LINE, "diff_datetime/4 - 17"),
    DT17a = #date_time{date = {2004, 12, 31}, time = {10, 5, 30}, type = utc},
    DT17b = #date_time{date = {2006, 1, 1}, time = {3, 45, 42}, type = utc},
    31599612 = diff_datetime(Timezone, DT17a, DT17b, secondly),

    ok.


