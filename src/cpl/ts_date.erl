%% This module contains various functions that manipulate dates
%% or parts of dates - year, month, day as well as weeks and year
%% days, week days and the like.
%%--------------------------------------------------------------------

-module(ts_date).

%% -behaviour().

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 diff_weekly/3,
	 gregorian_weekno/2,
	 date_to_weekno/2,
	 days_in_range/2,
	 nth_bymonthday/3,
	 normalize_monthday/3,
	 byyearday_to_date/2,
	 normalize_yearday/2,
	 nth_byday_in_month/4,
	 all_byday_in_month/3,
	 day_type_to_no/1,
	 dayno_to_daytype/1,
	 date_to_weekday/1,
	 nth_byday_in_year/3,
	 is_leap_year/1,
	 days_in_year/1,
	 m_days/2,
	 dayno_to_date/2,
	 date_to_dayno/1,
	 dayno_to_month/2,
	 weeks_in_year/2,
	 get_week_no/3,
	 weekno_to_date/3,

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
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
%% assumes that DT1 >= DT2
diff_weekly(Date1, Date2, Wkst) ->
    G1 = gregorian_weekno(Date1, Wkst),
    G2 = gregorian_weekno(Date2, Wkst),
    G1 - G2.

%% return: weekno since 0000-01-01
%% 0000-01-01 is a saturday (day = 6)
%% note: weekno is only defined for dates that follow after the
%%       first occurrence of Wkst (or are the first Wkst day)
gregorian_weekno(Date, Wkst) ->
    %% get start of week no. 1
    WeekStart = case day_type_to_no(Wkst) of
		    1 -> {0,1,3};
		    2 -> {0,1,4};
		    3 -> {0,1,5};
		    4 -> {0,1,6};
		    5 -> {0,1,7};
		    6 -> {0,1,1};
		    7 -> {0,1,2}
		end,
    DaysBeforeFirstWeek = calendar:date_to_gregorian_days(WeekStart),
    Days = calendar:date_to_gregorian_days(Date),
    WeekNo = ((Days - DaysBeforeFirstWeek) div 7) + 1,
    WeekNo.

%%--------------------------------------------------------------------
%% @spec    (Date, Wkst) ->
%%            {Year, WeekNo}
%%
%%            DateTime = {Year,Month,Day}
%%            Year     = integer()
%%            Month    = integer()
%%            Day      = integer()
%%            Wkst     = mo | tu | we | th | fr | sa | su "(weekday) first working day of the week"
%%
%%            Year   = integer()
%%            WeekNo = integer()
%%
%% @doc     determine which week DateTime belongs to Note : see RFC
%%          3880 chapter 4.4 page 17 for details - this code uses
%%          Wkst as the starting date of the week. Count weeks as
%%          belonging to the year where the week has most of it's
%%          days.
%% @end
%%--------------------------------------------------------------------
date_to_weekno({Year,_Month,_Day} = Date, Wkst) ->
    %% find first occurence of week = 1, start date
    {_,_,FirstWkst} = nth_byday_in_year(Year,1,Wkst),

    LastWeekStart = {Year, 12, 31 - (last_week_length(Year, Wkst) - 1)},
    if
	%% first week
	Date < {Year,1,FirstWkst} ->
	    case (FirstWkst-1) >= 4 of
		true -> {Year,1};
		false -> {Year-1, weeks_in_year(Year-1, Wkst)}
	    end;
	%% last week
	Date >= LastWeekStart ->
	    LastWeekLength = last_week_length(Year, Wkst),
	    case LastWeekLength >= 4 of
		true -> {Year, weeks_in_year(Year, Wkst)};
		false -> {Year+1, 1}
	    end;
	%% any other week
	true ->
	    %% determine if first week of year is week = 1
	    FirstWeekNo = case (FirstWkst-1) >= 4 of
			      true -> 1;
			      false -> 0
			  end,
	    DaysInRange = days_in_range({Year,1,FirstWkst}, Date),
	    %% check so that started week are included in week count
	    WeekNo = case (DaysInRange rem 7) of
			 0 ->
			     FirstWeekNo + (DaysInRange div 7);
			 _ ->
			     FirstWeekNo + (DaysInRange div 7) + 1
		     end,
	    {Year, WeekNo}
    end.

%%--------------------------------------------------------------------
%% @spec    (Start, End) -> integer()
%%
%%            Start = {Y,M,D} | {Y,M}
%%            End   = {Y,M,D} | {Y,M}
%%            Y     = integer() "year"
%%            M     = integer() "month"
%%            D     = integer() "day"
%%
%% @doc     determine the number of days in the (inclusive) time range
%%          Start-End, where Start `=<' End Note : CPU cost = O(N), N
%%          = length of time range in months
%% @end
%%--------------------------------------------------------------------
days_in_range({Y1,M1,D1},{Y2,M2,D2}) ->
    %% days_in_range({Y1,M1}, {Y2,M2}) counts all days in the range
    %% "-(D1-1)" - remove days before date Y1-M1-D1
    %% "- last_day_of_the_month(Y2,M2) + D2" - correct day count in last month
    days_in_range({Y1,M1}, {Y2,M2})
	- (D1-1)
	+ D2 -calendar:last_day_of_the_month(Y2, M2);

days_in_range({YStart, MStart}, {YEnd, MEnd}) ->
    days_in_range({YStart, MStart}, {YEnd, MEnd}, 0).

days_in_range({Y, M}, {Y, M}, DayCount) ->
    calendar:last_day_of_the_month(Y, M) + DayCount;

days_in_range({YStart, MStart}, {YEnd, MEnd}, DayCount) ->
    NoOfDays = calendar:last_day_of_the_month(YStart, MStart),
    case MStart of
	12 ->
	    NextM = 1,
	    NextY = YStart + 1,
	    days_in_range({NextY, NextM}, {YEnd, MEnd}, DayCount + NoOfDays);
	_ ->
	    NextM = MStart + 1,
	    NextY = YStart,
	    days_in_range({NextY, NextM}, {YEnd, MEnd}, DayCount + NoOfDays)
    end.

%%--------------------------------------------------------------------
%% @spec    (Year, Month, N) ->
%%            {Year,Month,Day} | nth_day_does_not_exist
%%
%%            Year  = integer()
%%            Month = integer()
%%            N     = integer() "N >= 1 or N =< -1"
%%
%% @doc     find date of Nth day in selected Year-Month. XXX unused.
%%          -1 = last day, -2 = day before last day ...
%% @end
%%--------------------------------------------------------------------
nth_bymonthday(Year,Month,N) when N >= 1 ->
    MonthLength = calendar:last_day_of_the_month(Year, Month),
    NthDay = normalize_monthday(Year, Month, N),
    case NthDay =< MonthLength of
	true -> {Year,Month,NthDay};
	false -> nth_day_does_not_exist
    end;
nth_bymonthday(Year,Month,N) when N =< -1 ->
    NthDay = normalize_monthday(Year, Month, N),
    case NthDay >= 1 of
	true -> {Year,Month,NthDay};
	false -> nth_day_does_not_exist
    end.

%%--------------------------------------------------------------------
%% @spec    (Year,Month,N) ->
%%            DayNo
%%
%%            Year  = integer()
%%            Month = integer()
%%            N     = integer() "N >= 1 or N =< -1"
%%
%%            DayNo = integer() ">= 1 (monthday) if N is valid < 1 if monthday is invalid "
%%
%% @doc     find date of Nth day in selected Year-Month -1 = last day,
%%          -2 = day before last day ...
%% @end
%%--------------------------------------------------------------------
normalize_monthday(_Year, _Month, N) when N >= 1 ->
    N;
normalize_monthday(Year, Month, N) when N =< -1 ->
    Nabs = - N,
    MonthLength = calendar:last_day_of_the_month(Year, Month),
    MonthLength - (Nabs-1).

%%--------------------------------------------------------------------
%% @spec    (Year,Month,N,DayType) ->
%%            {Year,Month,Day} | nth_day_does_not_exist
%%
%%            Year    = integer()
%%            Month   = integer()
%%            N       = integer() "N >= 1 or N =< -1"
%%            DayType = mo | tu | we | th | fr | sa | su "(weekday)"
%%
%% @doc     get date of Nth occurrence of day DayType in month
%% @end
%%--------------------------------------------------------------------
nth_byday_in_month(Year,Month,N,DayType) when N >= 1 ->
    NoOfDays = calendar:last_day_of_the_month(Year, Month),
    FirstMatchPos = get_first_occurence_of_weekday(Year, Month, DayType),
    NthDay = FirstMatchPos + ((N-1) * 7),
    case NthDay =< NoOfDays of
	true -> {Year,Month,NthDay};
	false -> nth_day_does_not_exist
    end;
nth_byday_in_month(Year,Month,N,DayType) when N =< -1 ->
    Nabs = - N,
    LastMatchPos = get_last_occurence_of_weekday(Year, Month, DayType),
    NthDay = LastMatchPos - ((Nabs-1) * 7),
    case NthDay >= 1 of
	true -> {Year,Month,NthDay};
	false -> nth_day_does_not_exist
    end.


get_first_occurence_of_weekday(Year, Month, DayType) ->
    DayTypeNo = day_type_to_no(DayType),
    get_first_occurence_of_weekday(Year, Month, DayTypeNo, 1).

get_first_occurence_of_weekday(Year, Month, DayTypeNo, N) ->
    case calendar:day_of_the_week(Year, Month, N) of
	DayTypeNo -> N;
	_ -> get_first_occurence_of_weekday(Year, Month, DayTypeNo, N+1)
    end.

get_last_occurence_of_weekday(Year, Month, DayType) ->
    DayTypeNo = day_type_to_no(DayType),
    NoOfDays = calendar:last_day_of_the_month(Year, Month),
    get_last_occurence_of_weekday(Year, Month, DayTypeNo, NoOfDays).

get_last_occurence_of_weekday(Year, Month, DayTypeNo, N) ->
    case calendar:day_of_the_week(Year, Month, N) of
	DayTypeNo -> N;
	_ -> get_last_occurence_of_weekday(Year, Month, DayTypeNo, N-1)
    end.

%%--------------------------------------------------------------------
%% @spec    (Year,Month,DayType) -> [{Year,Month,Day}]
%%
%%            Year    = integer()
%%            Month   = integer()
%%            DayType = mo | tu | we | th | fr | sa | su "(weekday)"
%%
%% @doc     get all occurrences of DayType in the indicated month
%% @end
%%--------------------------------------------------------------------
all_byday_in_month(Year,Month,DayType) ->
    N = 1,
    all_byday_in_month(Year,Month,DayType,N, []).

all_byday_in_month(Year,Month,DayType,N,Acc) ->
    Date = nth_byday_in_month(Year,Month,N,DayType),
    case Date of
	nth_day_does_not_exist ->
	    lists:reverse(Acc);
	_ ->
	    all_byday_in_month(Year,Month,DayType,N+1, [Date | Acc])
    end.

%%--------------------------------------------------------------------
%% @spec    (DayType) -> integer()
%%
%%            DayType = mo | tu | we | th | fr | sa | su "(weekday)"
%%
%% @doc     maps DayType codes to numerical weekday numbers (mo -> 1,
%%          tu -> 2 ....)
%% @end
%%--------------------------------------------------------------------
day_type_to_no(mo) -> 1;
day_type_to_no(tu) -> 2;
day_type_to_no(we) -> 3;
day_type_to_no(th) -> 4;
day_type_to_no(fr) -> 5;
day_type_to_no(sa) -> 6;
day_type_to_no(su) -> 7.

dayno_to_daytype(1) -> mo;
dayno_to_daytype(2) -> tu;
dayno_to_daytype(3) -> we;
dayno_to_daytype(4) -> th;
dayno_to_daytype(5) -> fr;
dayno_to_daytype(6) -> sa;
dayno_to_daytype(7) -> su.

%%--------------------------------------------------------------------
%% @spec    (Date) -> mo | tu | we | th | fr | sa | su
%%
%%            Date  = {Year, Month, Day}
%%            Year  = integer()
%%            Month = integer()
%%            Day   = integer()
%%
%% @doc     determine weekday of Date
%% @end
%%--------------------------------------------------------------------
date_to_weekday(Date) ->
    dayno_to_daytype(calendar:day_of_the_week(Date)).


%%--------------------------------------------------------------------
%% @spec    (Year,N,DayType) ->
%%            {Year,Month,Day} | nth_day_does_not_exist
%%
%%            Year    = integer()
%%            N       = integer() "N >= 1 or N =< -1"
%%            DayType = mo | tu | we | th | fr | sa | su "(weekday)"
%%
%% @doc     get date of Nth occurrence of weekday DayType in year Year
%% @end
%%--------------------------------------------------------------------
nth_byday_in_year(Year,N,DayType) when N >= 1 ->
    FirstMatchPos = get_first_occurence_of_weekday(Year, DayType),
    NthDay = FirstMatchPos + ((N-1) * 7),
    dayno_to_date(Year, NthDay);
nth_byday_in_year(Year,N,DayType) when N =< -1 ->
    Nabs = - N,
    LastMatchPos = get_last_occurence_of_weekday(Year, DayType),
    NthDay = LastMatchPos - ((Nabs-1) * 7),
    dayno_to_date(Year, NthDay).

get_first_occurence_of_weekday(Year, DayType) ->
    get_first_occurence_of_weekday(Year, 1, DayType).

get_last_occurence_of_weekday(Year, DayType) ->
    m_days(Year, 11) + get_last_occurence_of_weekday(Year, 12, DayType).

%%--------------------------------------------------------------------
%% @spec    (Year) -> true | false
%%
%% @doc     determine if Year a leap year
%% @end
%%--------------------------------------------------------------------
is_leap_year(Year) ->
    %% Feb 29 is a leap day so check if it is a valid year
    calendar:valid_date(Year, 2, 29).

%%--------------------------------------------------------------------
%% @spec    (Year) -> 365 | 366
%%
%% @doc     return number of days in year Year
%% @end
%%--------------------------------------------------------------------
days_in_year(Year) ->
    case calendar:is_leap_year(Year) of
	true -> 366;
	false -> 365
    end.

%%--------------------------------------------------------------------
%% @spec    (Year, Month) -> integer()
%%
%% @doc     return the number of days in month range [1,Month] in year
%%          Year
%% @end
%%--------------------------------------------------------------------
m_days(_Year, 0) -> 0; %% guard case for get_month/2
m_days(_Year, 1) -> 31;
m_days(Year, 2) -> feb(Year) + 31;
m_days(Year, 3) -> feb(Year) + 62;
m_days(Year, 4) -> feb(Year) + 92;
m_days(Year, 5) -> feb(Year) + 123;
m_days(Year, 6) -> feb(Year) + 153;
m_days(Year, 7) -> feb(Year) + 184;
m_days(Year, 8) -> feb(Year) + 215;
m_days(Year, 9) -> feb(Year) + 245;
m_days(Year, 10) -> feb(Year) + 276;
m_days(Year, 11) -> feb(Year) + 306;
m_days(Year, 12) -> feb(Year) + 337.

feb(Year) ->
    case is_leap_year(Year) of
	true -> 29;
	false -> 28
    end.

%%--------------------------------------------------------------------
%% @spec    (Year, NthDay) ->
%%            {Year2,Month2,Day2} | nth_day_does_not_exist
%%
%%            Year   = integer()
%%            Nthday = integer() "1-365 (366 if Year is a leap year)"
%%
%%            Year2  = integer()
%%            Month2 = integer()
%%            Day2   = integer()
%%
%% @doc     determine which month the NthDay occurs (in year Year)
%% @end
%%--------------------------------------------------------------------
dayno_to_date(Year, NthDay) ->
    DaysInYear = days_in_year(Year),
    case (DaysInYear >= NthDay) and (NthDay >= 1) of
	true ->
	    Month = dayno_to_month(Year, NthDay),
	    PrevMonthDays = m_days(Year,Month-1),
	    {Year, Month, NthDay - PrevMonthDays};
	false ->
	    nth_day_does_not_exist
    end.

%%--------------------------------------------------------------------
%% @spec    (Date) -> integer()
%%
%%            Date  = {Year,Month,Day}
%%            Year  = integer()
%%            Month = integer()
%%            Day   = integer()
%%
%% @doc     convert date to day count
%% @end
%%--------------------------------------------------------------------
date_to_dayno({Year,Month,Day}) ->
    m_days(Year, Month-1) + Day.


%%--------------------------------------------------------------------
%% @spec    (Year, NthDay) -> integer()
%%
%%            Year   = integer()
%%            Nthday = integer() "1-365 (366 if Year is a leap year)"
%%
%% @doc     determine which month the NthDay occurs (in year Year)
%% @end
%%--------------------------------------------------------------------
dayno_to_month(Year, NthDay) ->
    DaysInYear = days_in_year(Year),
    case (DaysInYear >= NthDay) and (NthDay >= 1) of
	true ->
	    StartMonth = (NthDay div 31) + 1,
	    get_month(Year, StartMonth, NthDay)
    end.

get_month(Year, CurrentMonth, NthDay) ->
    %% check if there are to few days in month range [1,CurrentMonth]
    case m_days(Year, CurrentMonth) < NthDay of
	true -> get_month(Year, CurrentMonth + 1, NthDay);
	%% sufficient number of days in month range [1,CurrentMonth],
	%% check that range [1,CurrentMonth-1] isn't sufficent
	%% i.e. that CurrentMonth should be used
	false -> case m_days(Year, CurrentMonth - 1) < NthDay of
		     true -> CurrentMonth;
		     false -> get_month(Year,CurrentMonth -1, NthDay)
		 end
    end.

%%--------------------------------------------------------------------
%% @spec    (Year, Wkst) -> integer()
%%
%%            Year = integer()
%%            Wkst = mo | tu | we | th | fr | sa | su "(weekday) first working day of the week"
%%
%% @doc     determine how many weeks belong to the year Year, some may
%%          overlap into the next and previous year - ISO 8601 counts
%%          weeks as belonging to the year where the week has most of
%%          it's days.
%% @end
%%--------------------------------------------------------------------
weeks_in_year(Year, Wkst) ->
    {_,_,FirstWkst} = nth_byday_in_year(Year,1,Wkst),
    %% check if first partial week belongs to the current year Year
    InitialWeek = case (FirstWkst-1) >= 4 of
		      true -> 1;
		      false -> 0
		  end,
    %% check if major part of last partial week (4+ days) belongs to current year Year
    LastWeekLength = last_week_length(Year, Wkst),
    LastWeek = case LastWeekLength >= 4 of
		   true -> 1;
		   false -> 0
	       end,

    DaysInYear = days_in_year(Year),
    FullWeeks = (DaysInYear - (FirstWkst-1) - LastWeekLength) div 7,

    %% NoOfWeeks
    FullWeeks + InitialWeek + LastWeek.

last_week_length(Year, Wkst) ->
    LastDay = 31,
    WkstNo = day_type_to_no(Wkst),
    last_week_length(Year, WkstNo, LastDay, 1).

last_week_length(Year, WkstNo, LastDay, Count) ->
    case calendar:day_of_the_week(Year, 12, LastDay) of
	WkstNo -> Count;
	_ -> last_week_length(Year, WkstNo, LastDay-1, Count+1)
    end.

%%--------------------------------------------------------------------
%% @spec    (Year, Wkst, WeekNo) ->
%%            integer() | nth_week_does_not_exits
%%
%%            Year   = integer()
%%            Wkst   = mo | tu | we | th | fr | sa | su "(weekday) first working day of the week"
%%            WeekNo = integer() ">= 1 or =< -1"
%%
%% @doc     map WeekNo to the week in Year to the regular nonnegative
%%          week no. representation WeekNo
%% @end
%%--------------------------------------------------------------------
get_week_no(Year, Wkst, WeekNo) when WeekNo >= 1 ->
    WeeksInYear = weeks_in_year(Year, Wkst),
    case (WeekNo >= 1) and (WeekNo =< WeeksInYear) of
	true -> WeekNo;
	false -> nth_week_does_not_exits
    end;
get_week_no(Year, Wkst, WeekNo) when WeekNo =< -1 ->
    WeeksInYear = weeks_in_year(Year, Wkst),
    WeekNo2 = WeeksInYear + WeekNo + 1,
    case (WeekNo2 >= 1) and (WeekNo2 =< WeeksInYear) of
	true ->
	    WeekNo2;
	false ->
	    nth_week_does_not_exits
    end.

%%--------------------------------------------------------------------
%% @spec    (Year, Wkst, WeekNo) ->
%%            {Year,Month,Day} | week_does_not_exist
%%
%%            Year   = integer() "current year"
%%            Wkst   = mo | tu | we | th | fr | sa | su "(weekday) first working day of the week"
%%            WeekNo = integer() "no. of week thats part of Year"
%%
%% @doc     return date of first day in week WeekNo
%% @end
%%--------------------------------------------------------------------
weekno_to_date(Year, Wkst, WeekNo) ->
    %% find first occurence of week = 1, start date
    {_,_,FirstWkst} = nth_byday_in_year(Year,1,Wkst),

    WeeksInYear = weeks_in_year(Year, Wkst),
    case (WeekNo >= 1) and (WeekNo =< WeeksInYear) of
	false ->
	    week_does_not_exist;
	true ->
	    FirstWeekLength = FirstWkst - 1,
	    StartOffset = case FirstWeekLength of
			      0 -> 1;
			      Val when Val < 4 -> FirstWkst;
			      Val when Val >= 4 -> FirstWkst - 7
			  end,
	    DayCount = StartOffset + ((WeekNo-1) * 7),
	    DaysInYear = days_in_year(Year),
	    case DayCount of
		_ when DayCount < 1 ->
		    dayno_to_date(Year-1, days_in_year(Year-1) - (-StartOffset));
		_ when DayCount > DaysInYear ->
		    dayno_to_date(Year+1, DayCount - days_in_year(Year));
		_ ->
		    dayno_to_date(Year, DayCount)
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Year,N) -> {Year,Month,Day} | nth_day_does_not_exist
%%
%%            Year = integer()
%%            N    = integer() "N >= 1 or N =< -1"
%%
%% @doc     get date of Nth day. -1 = last day, -2 = day before last
%%          day ...
%% @end
%%--------------------------------------------------------------------
byyearday_to_date(Year,N) when N >= 1 ->
    NthDay = normalize_yearday(Year, N),
    dayno_to_date(Year, NthDay);
byyearday_to_date(Year,N) when N =< -1 ->
    NthDay = normalize_yearday(Year, N),
    dayno_to_date(Year, NthDay).

%%--------------------------------------------------------------------
%% @spec    (Year, N) ->
%%            YearDay
%%
%%            Year  = integer()
%%            Month = integer()
%%            N     = integer() "N >= 1 or N =< -1"
%%
%%            YearDay = integer() ">= 1 (yearday) if N is valid < 1 if yearday is invalid "
%%
%% @doc     find day number of Nth day in selected Year. -1 = last
%%          day, -2 = day before last day ...
%% @end
%%--------------------------------------------------------------------
normalize_yearday(_Year, N)  when N >= 1 ->
    N;
normalize_yearday(Year, N)  when N =< -1 ->
    Nabs = - N,
    YearLength = days_in_year(Year),
    YearLength - (Nabs -1).


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
%% @doc     autotest
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->


    %% gregorian_weekno(Date, Wkst)
    %%--------------------------------------------------------------------
    %% first week (when Wkst = mo)
    autotest:mark(?LINE, "gregorian_weekno/2 - 1"),
    1  = gregorian_weekno({0,1,3}, mo),

    %% check that week boundaries are honored
    autotest:mark(?LINE, "gregorian_weekno/2 - 2"),
    W = gregorian_weekno({2005,5,2}, mo), %% mo
    W = gregorian_weekno({2005,5,3}, mo),
    W = gregorian_weekno({2005,5,4}, mo),
    W = gregorian_weekno({2005,5,5}, mo),
    W = gregorian_weekno({2005,5,6}, mo),
    W = gregorian_weekno({2005,5,7}, mo),
    W = gregorian_weekno({2005,5,8}, mo), %% su
    PW = W - 1,
    PW = gregorian_weekno({2005,5,1}, mo), % prev week
    NW = W + 1,
    NW = gregorian_weekno({2005,5,9}, mo), % next week

    %% first week (when Wkst = mo)
    autotest:mark(?LINE, "gregorian_weekno/2 - 3"),
    2  = gregorian_weekno({0,1,10}, mo),

    %% check that week boundaries are honored - when Wkst = su
    autotest:mark(?LINE, "gregorian_weekno/2 - 4"),
    W2 = gregorian_weekno({1997,1,12}, su), %% su
    W2 = gregorian_weekno({1997,1,13}, su),
    W2 = gregorian_weekno({1997,1,14}, su),
    W2 = gregorian_weekno({1997,1,15}, su),
    W2 = gregorian_weekno({1997,1,16}, su),
    W2 = gregorian_weekno({1997,1,17}, su),
    W2 = gregorian_weekno({1997,1,18}, su), %% sa
    PW2 = W2 - 1,
    PW2 = gregorian_weekno({1997,1,11}, su), % prev week
    NW2 = W2 + 1,
    NW2 = gregorian_weekno({1997,1,19}, su), % next week


    %% diff_weekly(Date1, Date2, Wkst)
    %%--------------------------------------------------------------------
    %% same week - dates are start and end of week
    autotest:mark(?LINE, "diff_weekly/3 - 1"),
    0 = diff_weekly({2005,2,20}, {2005,2,14}, mo),

    %% check week end boundary
    autotest:mark(?LINE, "diff_weekly/3 - 2"),
    1 = diff_weekly({2005,2,21}, {2005,2,14}, mo),

    %% check week start boundary
    autotest:mark(?LINE, "diff_weekly/3 - 3"),
    1 = diff_weekly({2005,2,20}, {2005,2,13}, mo),

    %% check both end and start boundary
    autotest:mark(?LINE, "diff_weekly/3 - 4"),
    2 = diff_weekly({2005,2,21}, {2005,2,13}, mo),


    %% date_to_weekno(DateTime, Wkst)
    %%--------------------------------------------------------------------
    %% first week not part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 1"),
    {2005, 40} = date_to_weekno({2005,10,5}, mo),

    %% first week part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 2"),
    {2003, 40} = date_to_weekno({2003,10,5}, mo),

    %% test handling of first (partial) week in year
    %% first week part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 3"),
    {2003, 1} = date_to_weekno({2003,1,1}, mo),
    %% first week not part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 4"),
    {2004, 53} = date_to_weekno({2005,1,1}, mo),

    %% test handling of last (partial) week in year
    %% last week not part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 5"),
    {2004, 1} = date_to_weekno({2003,12,31}, mo),
    %% last week part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 6"),
    {2004, 53} = date_to_weekno({2004,12,31}, mo),

    %% same tests with sunday instead of monday as first day of week

    %% first week not part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 7"),
    {2005, 40} = date_to_weekno({2005,10,5}, su),

    %% first week part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 8"),
    {2003, 41} = date_to_weekno({2003,10,5}, su),

    %% test handling of first (partial) week in year
    %% first week part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 9"),
    {2003, 1} = date_to_weekno({2003,1,1}, su),
    %% first week not part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 10"),
    {2004, 52} = date_to_weekno({2005,1,1}, su),

    %% test handling of last (partial) week in year
    %% last week not part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 11"),
    {2003, 53} = date_to_weekno({2003,12,31}, su),
    %% last week part of year
    autotest:mark(?LINE, "date_to_weekno/2 - 12"),
    {2004, 52} = date_to_weekno({2004,12,31}, su),

    %%--------------------------------------------------------------------

    %% last of first days belonging to week in prev. year
    autotest:mark(?LINE, "date_to_weekno/2 - 13"),
    {2004,53} = date_to_weekno({2005, 1, 2}, mo),

    %% first day, in first week of current  year
    autotest:mark(?LINE, "date_to_weekno/2 - 14"),
    {2005,1} = date_to_weekno({2005, 1, 3}, mo),

    %% last day (and week) of current year
    autotest:mark(?LINE, "date_to_weekno/2 - 15"),
    {2005,52} = date_to_weekno({2005, 12, 31}, mo),

    %% week in middle of year
    autotest:mark(?LINE, "date_to_weekno/2 - 16"),
    {2005,17} = date_to_weekno({2005, 4, 25}, mo),


    %% yearday = 1, week = 1
    autotest:mark(?LINE, "date_to_weekno/2 - 17"),
    {2004,1} = date_to_weekno({2004, 1, 1}, mo),

    %% first day in last week of year
    autotest:mark(?LINE, "date_to_weekno/2 - 18"),
    {2004,53} = date_to_weekno({2004, 12, 27}, mo),

    %% week in middle of year
    autotest:mark(?LINE, "date_to_weekno/2 - 19"),
    {2004,40} = date_to_weekno({2004, 10, 1}, mo),

    %% last day of first week in year (last day of week = 1)
    autotest:mark(?LINE, "date_to_weekno/2 - 20"),
    {2003,1} = date_to_weekno({2003, 1, 5}, mo),

    %% first day of last week belonging to next year
    autotest:mark(?LINE, "date_to_weekno/2 - 21"),
    {2004,1} = date_to_weekno({2003, 12, 29}, mo),

    %% last day of last week belonging to current year
    autotest:mark(?LINE, "date_to_weekno/2 - 22"),
    {2003,52} = date_to_weekno({2003, 12, 28}, mo),


    %% days_in_range/2
    %%--------------------------------------------------------------------
    %% single month
    autotest:mark(?LINE, "days_in_range/2 - 1"),
    31 = days_in_range({2005,1}, {2005,1}),

    %% 2 months
    autotest:mark(?LINE, "days_in_range/2 - 2"),
    59 = days_in_range({2005,1}, {2005,2}),

    %% year boundary
    autotest:mark(?LINE, "days_in_range/2 - 3"),
    62 = days_in_range({2005,12}, {2006,1}),

    %% several months
    autotest:mark(?LINE, "days_in_range/2 - 4"),
    122 = days_in_range({2005,4}, {2005,7}),

    %% several years
    autotest:mark(?LINE, "days_in_range/2 - 5"),
    852 = days_in_range({2004,4}, {2006,7}),

    %% single month - Y-M-D format
    autotest:mark(?LINE, "days_in_range/2 - 6"),
    9 = days_in_range({2005,1,7}, {2005,1,15}),

    %% single month (start to end) - Y-M-D format
    autotest:mark(?LINE, "days_in_range/2 - 7"),
    31 = days_in_range({2005,1,1}, {2005,1,31}),

    %% 2 month - Y-M-D format (D1 > D2)
    autotest:mark(?LINE, "days_in_range/2 - 8"),
    17 = days_in_range({2005,1,25}, {2005,2,10}),

    %% 2 month - Y-M-D format (D1 < D2)
    autotest:mark(?LINE, "days_in_range/2 - 9"),
    42 = days_in_range({2005,1,5}, {2005,2,15}),


    %% nth_bymonthday/3
    %%--------------------------------------------------------------------
    %% day in middle of month
    autotest:mark(?LINE, "nth_bymonthday/3 - 1"),
    {2005,4,10} = nth_bymonthday(2005, 4, 10),

    %% first day in month
    autotest:mark(?LINE, "nth_bymonthday/3 - 2"),
    {2005,4,1} = nth_bymonthday(2005, 4, 1),

    %% last day in month
    autotest:mark(?LINE, "nth_bymonthday/3 - 3"),
    {2005,4,30} = nth_bymonthday(2005, 4, 30),

    %% non-existent day in month
    autotest:mark(?LINE, "nth_bymonthday/3 - 4"),
    nth_day_does_not_exist = nth_bymonthday(2005, 4, 31),

    %% count days from end of month
    autotest:mark(?LINE, "nth_bymonthday/3 - 5"),
    {2005,4,21} = nth_bymonthday(2005, 4, -10),

    %% count days from end of month - last day
    autotest:mark(?LINE, "nth_bymonthday/3 - 6"),
    {2005,4,30} = nth_bymonthday(2005, 4, -1),

    %% count days from end of month - first day
    autotest:mark(?LINE, "nth_bymonthday/3 - 7"),
    {2005,4,1} = nth_bymonthday(2005, 4, -30),

    %% count days from end of month - non-existing day
    autotest:mark(?LINE, "nth_bymonthday/3 - 8"),
    nth_day_does_not_exist = nth_bymonthday(2005, 4, -31),


    %% nth_byday_in_month(Year,Month,N,DayType)
    %%--------------------------------------------------------------------
    %% 1st tuesday
    autotest:mark(?LINE, "nth_byday_in_month/4 - 1"),
    {2005, 4, 5} = nth_byday_in_month(2005, 4, 1, tu),

    %% 2nd tuesday
    autotest:mark(?LINE, "nth_byday_in_month/4 - 2"),
    {2005, 4, 12} = nth_byday_in_month(2005, 4, 2, tu),

    %% 3rd tuesday
    autotest:mark(?LINE, "nth_byday_in_month/4 - 3"),
    {2005, 4, 19} = nth_byday_in_month(2005, 4, 3, tu),

    %% 4th tuesday
    autotest:mark(?LINE, "nth_byday_in_month/4 - 4"),
    {2005, 4, 26} = nth_byday_in_month(2005, 4, 4, tu),

    %% no more tuesday in month
    autotest:mark(?LINE, "nth_byday_in_month/4 - 5"),
    nth_day_does_not_exist = nth_byday_in_month(2005, 4, 5, tu),

    %% 1st tuesday from end
    autotest:mark(?LINE, "nth_byday_in_month/4 - 6"),
    {2005, 4, 26} = nth_byday_in_month(2005, 4, -1, tu),

    %% 2nd tuesday from end
    autotest:mark(?LINE, "nth_byday_in_month/4 - 7"),
    {2005, 4, 19} = nth_byday_in_month(2005, 4, -2, tu),

    %% 3rd tuesday from end
    autotest:mark(?LINE, "nth_byday_in_month/4 - 8"),
    {2005, 4, 12} = nth_byday_in_month(2005, 4, -3, tu),

    %% 4th tuesday from end
    autotest:mark(?LINE, "nth_byday_in_month/4 - 9"),
    {2005, 4, 5} = nth_byday_in_month(2005, 4, -4, tu),

    %% no more tuesday in month
    autotest:mark(?LINE, "nth_byday_in_month/4 - 10"),
    nth_day_does_not_exist = nth_byday_in_month(2005, 4, -5, tu),


    %% all_byday_in_month/3
    %%--------------------------------------------------------------------
    %% tuesdays in april
    autotest:mark(?LINE, "all_byday_in_month/3 - 1"),
    [{2005,4,5}, {2005,4,12}, {2005,4,19}, {2005,4,26}] = all_byday_in_month(2005, 4, tu),

    %% sundays in december
    autotest:mark(?LINE, "all_byday_in_month/3 - 2"),
    [{2005,12,4}, {2005,12,11}, {2005,12,18}, {2005,12,25}]  = all_byday_in_month(2005, 12, su),

    %% weekday first day of month
    autotest:mark(?LINE, "all_byday_in_month/3 - 3"),
    [{2005,8,1}, {2005,8,8}, {2005,8,15}, {2005,8,22}, {2005,8,29}] = all_byday_in_month(2005, 8, mo),

    %% weekday last day of month
    autotest:mark(?LINE, "all_byday_in_month/3 - 4"),
    [{2005,8,3}, {2005,8,10}, {2005,8,17}, {2005,8,24}, {2005,8,31}] = all_byday_in_month(2005, 8, we),


    %% dayno_to_month(Year,NthDay)
    %%--------------------------------------------------------------------
    %% last day (non-leap year)
    autotest:mark(?LINE, "dayno_to_month/2 - 1"),
    12 = dayno_to_month(2005,365),

    %% first day (non-leap year)
    autotest:mark(?LINE, "dayno_to_month/2 - 2"),
    1 = dayno_to_month(2005,1),

    %% test a few random days
    %% 2005-06-09 (non-leap year)
    autotest:mark(?LINE, "dayno_to_month/2 - 3"),
    6 = dayno_to_month(2005,160),
    %% 2005-09-14 (non-leap year)
    9 = dayno_to_month(2005,257),
    %% 2005-02-28 (non-leap year)
    2 = dayno_to_month(2005,59),

    %% check that leap year contains
    %% last day (leap year)
    autotest:mark(?LINE, "dayno_to_month/2 - 4"),
    12 = dayno_to_month(2004,366),

    %% first day (leap year)
    autotest:mark(?LINE, "dayno_to_month/2 - 5"),
    1 = dayno_to_month(2004,1),

    %% test month transition
    %% 2004-11-01 (leap year)
    autotest:mark(?LINE, "dayno_to_month/2 - 6"),
    11 = dayno_to_month(2004,306),
    %% 2004-10-31 (leap year)
    10 = dayno_to_month(2004,305),

    %% test that leap day is in right month
    %% 2004-02-29 (leap year)
    autotest:mark(?LINE, "dayno_to_month/2 - 7"),
    2 = dayno_to_month(2004,60),

    %% dayno_to_date(Year, NthDay)
    %%--------------------------------------------------------------------
    %% leap day in leap year
    autotest:mark(?LINE, "dayno_to_date/2 - 1"),
    {2004, 2, 29} = dayno_to_date(2004,60),

    %% first day of year
    autotest:mark(?LINE, "dayno_to_date/2 - 2"),
    {2004, 1, 1} = dayno_to_date(2004,1),

    %% last day of non-leap year
    autotest:mark(?LINE, "dayno_to_date/2 - 3"),
    {2005, 12, 31} = dayno_to_date(2005,365),

    %% last day of leap year
    autotest:mark(?LINE, "dayno_to_date/2 - 4"),
    {2004, 12, 31} = dayno_to_date(2004,366),

    %% day in middle of month, in leap year, after occurrence of leap day
    autotest:mark(?LINE, "dayno_to_date/2 - 5"),
    {2004, 8, 26} = dayno_to_date(2004,239),

    %% day in middle of month, in non-leap year
    autotest:mark(?LINE, "dayno_to_date/2 - 6"),
    {2005, 8, 27} = dayno_to_date(2005,239),

    %% first day of month
    autotest:mark(?LINE, "dayno_to_date/2 - 7"),
    {2005, 4, 1} = dayno_to_date(2005,91),

    %% last day of month
    autotest:mark(?LINE, "dayno_to_date/2 - 8"),
    {2005, 4, 30} = dayno_to_date(2005,120),


    %% date_to_dayno(Date)
    %%--------------------------------------------------------------------
    %% leap day in leap year
    autotest:mark(?LINE, "date_to_dayno/2 - 1"),
    60 = date_to_dayno({2004, 2, 29}),

    %% first day of year
    autotest:mark(?LINE, "date_to_dayno/2 - 2"),
    1 = date_to_dayno({2004,1,1}),

    %% last day of non-leap year
    autotest:mark(?LINE, "date_to_dayno/2 - 3"),
    365 = date_to_dayno({2005,12,31}),

    %% last day of leap year
    autotest:mark(?LINE, "date_to_dayno/2 - 4"),
    366 = date_to_dayno({2004,12,31}),

    %% day in middle of month, in leap year, after occurrence of leap day
    autotest:mark(?LINE, "date_to_dayno/2 - 5"),
    239 = date_to_dayno({2004,8,26}),

    %% day in middle of month, in non-leap year
    autotest:mark(?LINE, "date_to_dayno/2 - 6"),
    239 = date_to_dayno({2005,8,27}),

    %% first day of month
    autotest:mark(?LINE, "date_to_dayno/2 - 7"),
    91 = date_to_dayno({2005,4,1}),

    %% last day of month
    autotest:mark(?LINE, "date_to_dayno/2 - 8"),
    120 = date_to_dayno({2005,4,30}),


    %% nth_byday_in_year(Year,N,DayType)
    %%--------------------------------------------------------------------
    %% first monday in year
    autotest:mark(?LINE, "nth_byday_in_year/3 - 1"),
    {2004,1,5} = nth_byday_in_year(2004,1,mo),

    %% last monday in year
    autotest:mark(?LINE, "nth_byday_in_year/3 - 2"),
    {2004,12,27} = nth_byday_in_year(2004,-1,mo),

    %% 10:th monday in year
    autotest:mark(?LINE, "nth_byday_in_year/3 - 3"),
    {2004,3,4} = nth_byday_in_year(2004,10,th),

    %% 10.th monday from end of year
    autotest:mark(?LINE, "nth_byday_in_year/3 - 4"),
    {2004,10,28} = nth_byday_in_year(2004,-10,th),


    %% weeks_in_year(Year, Wkst)
    %%--------------------------------------------------------------------
    %% first and last week part of year
    autotest:mark(?LINE, "weeks_in_year/1 - 1"),
    53 = weeks_in_year(2004, mo),

    %% first week part of year
    autotest:mark(?LINE, "weeks_in_year/1 - 2"),
    52 = weeks_in_year(2003, mo),

    %% last week part of year
    autotest:mark(?LINE, "weeks_in_year/1 - 3"),
    52 = weeks_in_year(2005, mo),

    %% sunday as first day of week

    %% last week part of year
    autotest:mark(?LINE, "weeks_in_year/1 - 4"),
    52 = weeks_in_year(2004, su),

    %% first week part of year
    autotest:mark(?LINE, "weeks_in_year/1 - 5"),
    53 = weeks_in_year(2003, su),

    %% last week part of year
    autotest:mark(?LINE, "weeks_in_year/1 - 6"),
    52 = weeks_in_year(2005, su),


    %% get_week_no(Year, Wkst, WeekNo)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "get_week_no/3 - 1"),
    52 = get_week_no(2005, mo, -1),

    autotest:mark(?LINE, "get_week_no/3 - 2"),
    53 = get_week_no(2004, mo, -1),

    autotest:mark(?LINE, "get_week_no/3 - 3"),
    1 = get_week_no(2005, mo, -52),

    autotest:mark(?LINE, "get_week_no/3 - 4"),
    1 = get_week_no(2004, mo, -53),

    autotest:mark(?LINE, "get_week_no/3 - 5"),
    nth_week_does_not_exits = get_week_no(2005, mo, -53),

    autotest:mark(?LINE, "get_week_no/3 - 6"),
    44 = get_week_no(2004, mo, -10),

    autotest:mark(?LINE, "get_week_no/3 - 7"),
    43 = get_week_no(2005, mo, 43),

    autotest:mark(?LINE, "get_week_no/3 - 8"),
    nth_week_does_not_exits = get_week_no(2005, mo, 53),


    %% weekno_to_date(Year, Wkst, WeekNo)
    %%--------------------------------------------------------------------
    %% first (partial) week in year
    autotest:mark(?LINE, "weekno_to_date/3 - 1"),
    {2003,12,29} = weekno_to_date(2004, mo, 1),

    %% first full week in year
    autotest:mark(?LINE, "weekno_to_date/3 - 2"),
    {2004,1,5} = weekno_to_date(2004, mo, 2),

    %% last week in year
    autotest:mark(?LINE, "weekno_to_date/3 - 3"),
    {2004,12,27} = weekno_to_date(2004, mo, 53),

    %% next to last week in year
    autotest:mark(?LINE, "weekno_to_date/3 - 4"),
    {2004,12,20} = weekno_to_date(2004, mo, 52),

    %% week in middle of year
    autotest:mark(?LINE, "weekno_to_date/3 - 5"),
    {2004,7,12} = weekno_to_date(2004, mo, 29),

    %% last week in year (last days are part of week 1 in next year
    autotest:mark(?LINE, "weekno_to_date/3 - 6"),
    {2003,12,22} = weekno_to_date(2003, mo, 52),

    %% week in middle of year
    autotest:mark(?LINE, "weekno_to_date/3 - 7"),
    {2003,7,14} = weekno_to_date(2003, mo, 29),

    %% first week, first days are part of week 53 in previous year
    autotest:mark(?LINE, "weekno_to_date/3 - 8"),
    {2005,1,3} = weekno_to_date(2005, mo, 1),

    %% week in middle of year
    autotest:mark(?LINE, "weekno_to_date/3 - 9"),
    {2005,7,18} = weekno_to_date(2005, mo, 29),

    %% --------------
    %% Wkst = su
    %% first week of year
    autotest:mark(?LINE, "weekno_to_date/3 - 10"),
    {1996,12,29} = weekno_to_date(1997, su, 1),

    %% last week of year
    autotest:mark(?LINE, "weekno_to_date/3 - 11"),
    {1997,12,28} = weekno_to_date(1997, su, 53),

    %% week in middle of year
    autotest:mark(?LINE, "weekno_to_date/3 - 12"),
    {1997,1,12} = weekno_to_date(1997, su, 3),


    %% byyearday_to_date(Year,N)
    %%--------------------------------------------------------------------
    %% day N to date (in leap year)
    autotest:mark(?LINE, "byyearday_to_date/3 - 1"),
    {2004,4,9} = byyearday_to_date(2004, 100),

    %% day -N to date (in leap year)
    autotest:mark(?LINE, "byyearday_to_date/3 - 2"),
    {2004,9,23} = byyearday_to_date(2004, -100),



    ok.
