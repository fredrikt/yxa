%%%-------------------------------------------------------------------
%%% File    : interpret_time_test.erl
%%% @author   Håkan Stenholm <hsten@it.su.se>
%%% @doc      Test cases for interpret_time.
%%%
%%% The follow DST policy is assumed for the test cases written:
%%%
%%% DST - Daylight Saving Time (EU countries):
%%% * change to DST is done on last sunday of march
%%% * change back from DST is done on last sunday of october
%%% * change to DST: move clock forward e.g. 01:00 -> 02:00 (UTC time)
%%% * back from DST: move clock back 2:00 -> 1:00 (in EU at 01:00 UTC
%%%   i.e. when DST adjusted wall clock time is = 02:00 + time-zone
%%%   modifier)
%%%
%%% Test cases written that rely on this DST policy, print a
%%% "WARNING: test assumes that local = EU" message.
%%% When the DST change is done differs from country to country (if
%%% DST is used at all) e.g the US changes based on the local time
%%% rather than a common UTC time point, which differs from the EU
%%% where all countries change at the same "real" time rather than the
%%% same wall clock time.
%%%
%%% @since    17 Dec 2004 by Håkan Stenholm <hsten@it.su.se>
%%% @end
%%% @hidden
%%%-------------------------------------------------------------------
-module(interpret_time_test).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 test/0
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


    %% local_usec/0
    %%--------------------------------------------------------------------
    %% test that nothing crashes, checking the return value is somewhat complicated
    %% by the fact that it is always different
    autotest:mark(?LINE, "local_usec/0 - 1"),
    Res = interpret_time:local_usec(),
    case Res of
	Res when is_integer(Res) -> ok;
	_ -> throw({error, local_usec_did_not_return_integer})
    end,

    %% in_usec_range/3
    %%--------------------------------------------------------------------
    %% test regular range
    autotest:mark(?LINE, "in_usec_range/3 - 1"),
    true = interpret_time:in_usec_range(1, 3, 1),
    true = interpret_time:in_usec_range(1, 3, 2),
    true = interpret_time:in_usec_range(1, 3, 3),

    %% single element range
    autotest:mark(?LINE, "in_usec_range/3 - 2"),
    true = interpret_time:in_usec_range(1, 1, 1),

    %% outside range
    autotest:mark(?LINE, "in_usec_range/3 - 3"),
    false = interpret_time:in_usec_range(1, 3, 5),

    %% range contains start and/or end that is a non-legal time
    autotest:mark(?LINE, "in_usec_range/3 - 4"),
    true = interpret_time:in_usec_range(1, {pseudo_usec, 3}, 2),
    true = interpret_time:in_usec_range({pseudo_usec, 1}, 3, 2),
    false = interpret_time:in_usec_range({pseudo_usec, 1}, {pseudo_usec, 3}, 2),

    %%--------------------------------------------------------------------
    test5(),

    %%--------------------------------------------------------------------
    test6(),

    %% in_time_range_test/3 (test version of in_time_range/3)
    %%--------------------------------------------------------------------
    Cond1 = #time_switch__cond_2{
      dtstart = #date_time{date = {2005,1,3}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}}
     },
    Timezone1 = dummy,

    %%
    autotest:mark(?LINE, "in_time_range_test/3 - 1"),
    Current1a = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,3}, time = {12,0,0}, type = floating}),
    true = interpret_time:in_time_range_test(Timezone1, Cond1, Current1a),

    %%
    Cond2 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,3}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = weekly,
      interval = 3,
      until_count = {until, #date_time{date = {2005,3,3}, time = {16,40,00}, type = floating}}
     },
    Timezone2 = dummy,

    %%
    autotest:mark(?LINE, "in_time_range_test/3 - 2"),
    Current2a = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,24}, time = {12,0,0}, type = floating}),
    true = interpret_time:in_time_range_test(Timezone2, Cond2, Current2a),

    %%--------------------------------------------------------------------
    test7(),

    %%--------------------------------------------------------------------
    test8(),

    %%--------------------------------------------------------------------
    test9(),
    test10(),
    test11(),
    test12(),
    test13(),
    test14(),
    test15(),
    test16(),
    test17(),
    test18(),
    test19_1(),
    test19_2(),
    test19_3(),
    test19_4(),
    test19_5(),
    test19_6(),
    test19_7(),
    test19_8(),
    test19_9(),
    test20(),

    %% count handling with time_switch__cond_7
    %%--------------------------------------------------------------------
    test21_1(),
    test21_2(),
    test21_3(),
    test21_4(),
    test21_5(),
    test21_6(),
    test21_7(),
    test21_8(),
    test21_9(),
    test21_10(),
    test21_11(),
    test21_12(),
    test21_13(),
    test21_14(),
    test21_15(),
    test21_16(),

    %% test bysetpos handling
    %%--------------------------------------------------------------------
    test22a(),
    test22b(),
    test22c(),
    test22d(),
    test22e(),
    test22f(),
    test23(),
    test24a(),
    test24b(),
    test25(),
    test26a(),
    test26b(),
    test27(),
    test28(),


    ok.


%% in_time_range_test/3
%%--------------------------------------------------------------------
%% CurrentX = is used to supply a "fake" local_usec() value
test5() ->
    %% test dtend attribute
    autotest:mark(?LINE, "in_time_range_test/3 - 1"),
    Cond1 = #time_switch__cond_2{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
      dtend_duration = {dtend, #date_time{date = {2005,1,1}, time = {0,30,45}, type = utc}}
     },
    Current1 = ts_datetime:datetime_to_usec(start, dummy,
					       #date_time{date = {2005,1,1}, time = {0,12,58}, type = utc}),
    Timezone1 = dummy,
    true = interpret_time:in_time_range_test(Timezone1, Cond1, Current1),

    %% test duration attribute
    autotest:mark(?LINE, "in_time_range_test/3 - 2"),
    Cond2 = #time_switch__cond_2{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
      dtend_duration = {duration, #duration{minutes = 30, seconds = 45}}
     },
    Current2 = ts_datetime:datetime_to_usec(start, dummy,
					       #date_time{date = {2005,1,1}, time = {0,12,58}, type = utc}),
    Timezone2 = dummy,
    true = interpret_time:in_time_range_test(Timezone2, Cond2, Current2),

    %% current = time before range
    autotest:mark(?LINE, "in_time_range_test/3 - 3"),
    Cond3 = #time_switch__cond_2{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = utc},
      dtend_duration = {duration, #duration{minutes = 30, seconds = 45}}
     },
    Current3 = ts_datetime:datetime_to_usec(start, dummy,
					       #date_time{date = {2004,12,30}, time = {0,12,58}, type = utc}),
    Timezone3 = dummy,
    false = interpret_time:in_time_range_test(Timezone3, Cond3, Current3),

    %% transition to DST
     DST = 1,

    %% non-existent time range
    autotest:mark(?LINE, "in_time_range_test/3 - 4 WARNING: test assumes that local = EU"),
    Cond4 = #time_switch__cond_2{
      dtstart = #date_time{date = {2004,3,28}, time = {1 + cpl_test_util:timezone_offset(),30,0}, type = floating},
      dtend_duration = {dtend, #date_time{date = {2004,3,28},
					  time = {1 + cpl_test_util:timezone_offset(),45,00}, type = floating}}
     },
    Current4 = ts_datetime:datetime_to_usec(start, dummy,
					       #date_time{date = {2005,1,1}, time = {0,12,58}, type = utc}),
    Timezone4 = dummy,
    false = interpret_time:in_time_range_test(Timezone4, Cond4, Current4),

    %% end point of range in non-existing time range
    autotest:mark(?LINE, "in_time_range_test/3 - 5 WARNING: test assumes that local = EU"),
    Cond5 = #time_switch__cond_2{
      dtstart = #date_time{date = {2004,3,28}, time = {0 + cpl_test_util:timezone_offset(),0,0}, type = floating},
      dtend_duration = {duration, #duration{minutes = 60, seconds = 10}}
      %% (time = 1 + cpl_test_util:timezone_offset(), 0, 10)
     },
    %% first second of the DST period
    Current5a = ts_datetime:datetime_to_usec(start, dummy, #date_time{date = {2004,3,28},
							 time = {1 + DST + cpl_test_util:timezone_offset(), 0, 0},
							 type = floating}),
    Timezone5a = dummy,
    false = interpret_time:in_time_range_test(Timezone5a, Cond5, Current5a),
    %% last legal time before jump
    Current5b = ts_datetime:datetime_to_usec(start, dummy, #date_time{date = {2004,3,28},
							 time = {0 + cpl_test_util:timezone_offset(), 59, 59},
							 type = floating}),
    Timezone5b = dummy,
    true = interpret_time:in_time_range_test(Timezone5b, Cond5, Current5b),

    %% start of range in non-existing time range
    autotest:mark(?LINE, "in_time_range_test/3 - 6 WARNING: test assumes that local = EU"),
    Cond6 = #time_switch__cond_2{
      dtstart = #date_time{date = {2004,3,28}, time = {1 + cpl_test_util:timezone_offset() ,30,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 1}}
     },
    %% first second beyond range
    Current6a = ts_datetime:datetime_to_usec(start, dummy, #date_time{date = {2004,3,28},
							 time = {1 + cpl_test_util:timezone_offset() + DST,30,00},
							 type = floating}),
    Timezone6a = dummy,
    false = interpret_time:in_time_range_test(Timezone6a, Cond6, Current6a),
    %% last second still inside range
    Current6b = ts_datetime:datetime_to_usec(start, dummy, #date_time{date = {2004,3,28},
							 time = {1 + cpl_test_util:timezone_offset() + DST,29,59},
							 type = floating}),
    Timezone6b = dummy,
    true = interpret_time:in_time_range_test(Timezone6b, Cond6, Current6b),

    %% end DST

    %% check length of duplicate range
    autotest:mark(?LINE, "in_time_range_test/3 - 7 WARNING: test assumes that local = EU"),
    Cond7 = #time_switch__cond_2{
      dtstart = #date_time{date = {2005, 10, 30}, time = {1 + cpl_test_util:timezone_offset() ,0,0}, type = floating},
      dtend_duration = {dtend, #date_time{date = {2005, 10, 30},
					  time = {1 + cpl_test_util:timezone_offset() ,59,59}, type = floating}}
     },
    %% first second in duplicate range
    Current7a = ts_datetime:datetime_to_usec(start, dummy, #date_time{date = {2005, 10, 30},
							 time = {0,0,0},
							 type = utc}),
    Timezone7a = dummy,
    true = interpret_time:in_time_range_test(Timezone7a, Cond7, Current7a),
    %% last second in duplicate range
    Current7b = ts_datetime:datetime_to_usec(stop, dummy, #date_time{date = {2005, 10, 30},
							 time = {1,59,59},
							 type = utc}),
    Timezone7b = dummy,
    true = interpret_time:in_time_range_test(Timezone7b, Cond7, Current7b),
    %% first second befor duplicate range
    Current7c = ts_datetime:datetime_to_usec(start, dummy, #date_time{date = {2005, 10, 29},
							 time = {23,59,59},
							 type = utc}),
    Timezone7c = dummy,
    false = interpret_time:in_time_range_test(Timezone7c, Cond7, Current7c),
    %% first second after duplicate range
    Current7d = ts_datetime:datetime_to_usec(stop, dummy, #date_time{date = {2005, 10, 30},
							 time = {2,0,0},
							 type = utc}),
    Timezone7d = dummy,
    false = interpret_time:in_time_range_test(Timezone7d, Cond7, Current7d),

    ok.

%% in_time_range_5/3
%%--------------------------------------------------------------------
test6() ->
    Cond1 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 10}},
      freq = daily,
      interval = 2,
      until_count = repeat_forever
     },
    Timezone1 = dummy,

    %% current matches first second in first occurrence of interval
    autotest:mark(?LINE, "in_time_range_5/3 - 1"),
    Current1a = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,1}, time = {0,0,0}, type = floating}),
    true = interpret_time:in_time_range_5(Current1a, Timezone1, Cond1),

    %% current doesn't match first occurrence of interval (its the first second after the firts occurence)
    autotest:mark(?LINE, "in_time_range_5/3 - 2"),
    Current1b = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,1}, time = {10,0,1}, type = floating}),
    false = interpret_time:in_time_range_5(Current1b, Timezone1, Cond1),

    %% current matches in 3rd occurrence of interval
    autotest:mark(?LINE, "in_time_range_5/3 - 3"),
    Current1c = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,5}, time = {5,5,23}, type = floating}),
    true = interpret_time:in_time_range_5(Current1c, Timezone1, Cond1),

    %% current doesn't matches day of interval reoccurrence
    autotest:mark(?LINE, "in_time_range_5/3 - 4"),
    Current1d = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,4}, time = {5,5,23}, type = floating}),
    false = interpret_time:in_time_range_5(Current1d, Timezone1, Cond1),

    %% current matches day but not reoccurrence interval
    autotest:mark(?LINE, "in_time_range_5/3 - 5"),
    Current1e = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,5}, time = {23,0,0}, type = floating}),
    false = interpret_time:in_time_range_5(Current1e, Timezone1, Cond1),

    %% test with dtend (rather than duration) - time interval is of the same length

    Cond2 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {dtend, #date_time{date = {2005,1,1}, time = {10,0,0}, type = floating}},
      freq = daily,
      interval = 2,
      until_count = repeat_forever
     },
    Timezone2 = dummy,

    %% current matches first second in first occurrence of interval
    autotest:mark(?LINE, "in_time_range_5/3 - 6"),
    Current2a = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,1}, time = {0,0,0}, type = floating}),
    true = interpret_time:in_time_range_5(Current2a, Timezone2, Cond2),

    %% current doesn't match first occurrence of interval (its the first second after the firts occurence)
    autotest:mark(?LINE, "in_time_range_5/3 - 7"),
    Current2b = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,1}, time = {10,0,1}, type = floating}),
    false = interpret_time:in_time_range_5(Current2b, Timezone2, Cond2),

    %% current matches in 3rd occurrence of interval
    autotest:mark(?LINE, "in_time_range_5/3 - 8"),
    Current2c = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,5}, time = {5,5,23}, type = floating}),
    true = interpret_time:in_time_range_5(Current2c, Timezone2, Cond2),

    %% current doesn't matches day of interval reoccurrence
    autotest:mark(?LINE, "in_time_range_5/3 - 9"),
    Current2d = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,4}, time = {5,5,23}, type = floating}),
    false = interpret_time:in_time_range_5(Current2d, Timezone2, Cond2),

    %% current matches day but not reoccurrence interval
    autotest:mark(?LINE, "in_time_range_5/3 - 10"),
    Current2e = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,5}, time = {23,0,0}, type = floating}),
    false = interpret_time:in_time_range_5(Current2e, Timezone2, Cond2),

    %% current < dtstart (by 1 second)
    autotest:mark(?LINE, "in_time_range_5/3 - 10"),
    Current2f = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,4}, time = {23,59,59}, type = floating}),
    false = interpret_time:in_time_range_5(Current2f, Timezone2, Cond2),

    ok.

%% get_count_ranges_5/2
%%--------------------------------------------------------------------
test7() ->
    Timezone = dummy,

    %% test single count - reoccurrence without "holes"
    autotest:mark(?LINE, "get_count__range_5/2 - 1"),
    Cond1 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,3}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = weekly,
      interval = 3,
      until_count = {count, 1}},

    S1 = ts_datetime:datetime_to_usec(start, Timezone,
					 #date_time{date = {2005,1,3}, time = {8,0,0}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
					 #date_time{date = {2005,1,3}, time = {16,39,59}, type = floating}),
    Res1 = interpret_time:get_count_ranges_5(Timezone, Cond1),
    %% io:format("Res1 = ~p~n",[Res1]),
    [{S1,E1}] = Res1,

    %% test multiple count - reoccurrence without "holes"
    autotest:mark(?LINE, "get_count__range_5/2 - 2"),
    Cond2 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,3}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = weekly,
      interval = 3,
      until_count = {count, 3}},

    S2a = ts_datetime:datetime_to_usec(start, Timezone,
					  #date_time{date = {2005,1,3}, time = {8,0,0}, type = floating}),
    E2a = ts_datetime:datetime_to_usec(stop, Timezone,
					  #date_time{date = {2005,1,3}, time = {16,39,59}, type = floating}),
    S2b = ts_datetime:datetime_to_usec(start, Timezone,
					  #date_time{date = {2005,1,24}, time = {8,0,0}, type = floating}),
    E2b = ts_datetime:datetime_to_usec(stop, Timezone,
					  #date_time{date = {2005,1,24}, time = {16,39,59}, type = floating}),
    S2c = ts_datetime:datetime_to_usec(start, Timezone,
					  #date_time{date = {2005,2,14}, time = {8,0,0}, type = floating}),
    E2c = ts_datetime:datetime_to_usec(stop, Timezone,
					  #date_time{date = {2005,2,14}, time = {16,39,59}, type = floating}),
    Res2 = interpret_time:get_count_ranges_5(Timezone, Cond2),
    %% io:format("Res2 = ~p~n",[Res2]),
    [{S2a,E2a}, {S2b,E2b}, {S2c,E2c}] = Res2,

    %% test multiple count - reoccurrence with "holes"
    autotest:mark(?LINE, "get_count__range_5/2 - 3"),
    Cond3 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,31}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = monthly,
      interval = 1,
      until_count = {count, 3}},

    S3a = ts_datetime:datetime_to_usec(start, Timezone,
					  #date_time{date = {2005,1,31}, time = {8,0,0}, type = floating}),
    E3a = ts_datetime:datetime_to_usec(stop, Timezone,
					  #date_time{date = {2005,1,31}, time = {16,39,59}, type = floating}),
    S3b = ts_datetime:datetime_to_usec(start, Timezone,
					  #date_time{date = {2005,3,31}, time = {8,0,0}, type = floating}),
    E3b = ts_datetime:datetime_to_usec(stop, Timezone,
					  #date_time{date = {2005,3,31}, time = {16,39,59}, type = floating}),
    S3c = ts_datetime:datetime_to_usec(start, Timezone,
					  #date_time{date = {2005,5,31}, time = {8,0,0}, type = floating}),
    E3c = ts_datetime:datetime_to_usec(stop, Timezone,
					  #date_time{date = {2005,5,31}, time = {16,39,59}, type = floating}),
    Res3 = interpret_time:get_count_ranges_5(Timezone, Cond3),
    %% io:format("Res3 = ~p~n",
    %% 	      [[{calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(E1)),
    %% 		calendar:universal_time_to_local_time(calendar:gregorian_seconds_to_datetime(E2))} ||
    %% 		   {E1,E2} <- Res3]]),
    [{S3a,E3a}, {S3b,E3b}, {S3c,E3c}] = Res3,

    %% test multiple count - reoccurrence with DST "hole"
    autotest:mark(?LINE, "get_count__range_5/2 - 4 - WARNING: test assumes that local = EU"),
    Cond4 = #time_switch__cond_5{
      dtstart = #date_time{date = {2004,3,28}, time = {0 + cpl_test_util:timezone_offset(),20,0}, type = floating},
      dtend_duration = {duration, #duration{minutes = 20, seconds = 1}},
      freq = minutely,
      interval = 40,
      until_count = {count, 2}},

    S4a = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {2004,3,28},
								   time = {0 + cpl_test_util:timezone_offset(),20,0},
								   type = floating}),
    E4a = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {2004,3,28},
								  time = {0 + cpl_test_util:timezone_offset(),40,00},
								  type = floating}),
    %% a degenerated interval UTC 2:00:00 - 2:00:00 as only E4b is a "real" value in the S4b - E4b range
    S4b = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {2004,3,28},
								   time = {1 + cpl_test_util:timezone_offset(),40,0},
								   type = floating}),
    E4b = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {2004,3,28},
								  time = {2 + cpl_test_util:timezone_offset(),0,0},
								  type = floating}),
    Res4 = interpret_time:get_count_ranges_5(Timezone, Cond4),
    %% io:format("Res4 = ~p~n", [Res4]),
    [{S4a,E4a}, {S4b,E4b}] = Res4,

    %% test multiple count - when ending DST
    autotest:mark(?LINE, "get_count__range_5/2 - 5 - WARNING: test assumes that local = EU"),
    Cond5 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,10,30}, time = {1 + cpl_test_util:timezone_offset(),0,0}, type = floating},
      dtend_duration = {duration, #duration{minutes = 20, seconds = 1}},
      freq = minutely,
      interval = 40,
      until_count = {count, 2}},

    S5a = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {2005,10,30},
						       time = {0,0,0}, type = utc}),
    E5a = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {2005,10,30},
						      time = {1,20,0}, type = utc}),
    S5b = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {2005,10,30},
						       time = {0,40,0}, type = utc}),
    E5b = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {2005,10,30},
						      time = {2,0,0}, type = utc}),
    Res5 = interpret_time:get_count_ranges_5(Timezone, Cond5),
    %% io:format("Res5 = ~p~n", [Res5]),
    [{S5a,E5a}, {S5b,E5b}] = Res5,

    %% test multiple count - missing leap day
    autotest:mark(?LINE, "get_count__range_5/2 - 6"),

    Cond6 = #time_switch__cond_5{
      dtstart = #date_time{date = {1997,2,28}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = daily,
      interval = 1,
      until_count = {count, 3}},

    S6a = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1997,2,28},
						       time = {8,0,0}, type = floating}),
    E6a = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1997,2,28},
						      time = {16,39,59}, type = floating}),
    S6b = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1997,3,1},
						       time = {8,0,0}, type = floating}),
    E6b = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1997,3,1},
						      time = {16,39,59}, type = floating}),
    S6c = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1997,3,2},
						       time = {8,0,0}, type = floating}),
    E6c = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1997,3,2},
						      time = {16,39,59}, type = floating}),

    Res6 = interpret_time:get_count_ranges_5(Timezone, Cond6),
    %% io:format("Res6 = ~p~n", [Res6]),
    [{S6a,E6a}, {S6b,E6b}, {S6c,E6c}] = Res6,

    %% test multiple count - leap day (1996-02-29 leap day)
    autotest:mark(?LINE, "get_count__range_5/2 - 7"),
    Cond7 = #time_switch__cond_5{
      dtstart = #date_time{date = {1996,2,28}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = daily,
      interval = 1,
      until_count = {count, 3}},

    S7a = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1996,2,28},
						       time = {8,0,0}, type = floating}),
    E7a = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1996,2,28},
						      time = {16,39,59}, type = floating}),
    S7b = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1996,2,29},
						       time = {8,0,0}, type = floating}),
    E7b = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1996,2,29},
						      time = {16,39,59}, type = floating}),
    S7c = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1996,3,1},
						       time = {8,0,0}, type = floating}),
    E7c = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1996,3,1},
						      time = {16,39,59}, type = floating}),

    Res7 = interpret_time:get_count_ranges_5(Timezone, Cond7),
    %% io:format("Res7 = ~p~n", [Res7]),
    [{S7a,E7a}, {S7b,E7b}, {S7c,E7c}] = Res7,

    ok.

%% in_time_range/3 (with time_switch__cond_5 records)
%%--------------------------------------------------------------------
test8() ->
    Timezone = dummy,

    %% test repeat forever
    Cond1 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {dtend, #date_time{date = {2005,1,1}, time = {10,0,0}, type = floating}},
      freq = daily,
      interval = 2,
      until_count = repeat_forever
     },

    autotest:mark(?LINE, "in_time_range/3 - 1a"),
    Current1a = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,7}, time = {5,5,23}, type = floating}),
    true = interpret_time:in_time_range(Timezone, Cond1, Current1a),
    autotest:mark(?LINE, "in_time_range/3 - 1b"),
    Current1b = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,7}, time = {10,0,1}, type = floating}),
    false = interpret_time:in_time_range(Timezone, Cond1, Current1b),


    %% test count
    S2a = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1996,2,28},
						       time = {8,0,0}, type = floating}),
    E2a = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1996,2,28},
						      time = {16,39,59}, type = floating}),
    S2b = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1996,2,29},
						       time = {8,0,0}, type = floating}),
    E2b = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1996,2,29},
						      time = {16,39,59}, type = floating}),
    S2c = ts_datetime:datetime_to_usec(start, Timezone, #date_time{date = {1996,3,1},
						       time = {8,0,0}, type = floating}),
    E2c = ts_datetime:datetime_to_usec(stop, Timezone, #date_time{date = {1996,3,1},
						      time = {16,39,59}, type = floating}),
    Cond2 = #time_switch__cond_5{
      dtstart = #date_time{date = {1996,2,28}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = daily,
      interval = 1,
      until_count = {count, 3},
      time_ranges = [{S2a,E2a}, {S2b,E2b}, {S2c,E2c}]
     },
    autotest:mark(?LINE, "in_time_range/3 - 2a"),
    Current2a = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {1996,2,29}, time = {15,5,23}, type = floating}),
    true = interpret_time:in_time_range(Timezone, Cond2, Current2a),
    autotest:mark(?LINE, "in_time_range/3 - 2b"),
    Current2b = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {1996,2,29}, time = {16,40,0}, type = floating}),
    false = interpret_time:in_time_range(Timezone, Cond2, Current2b),

    %% test until
    Cond3 = #time_switch__cond_5{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {dtend, #date_time{date = {2005,1,1}, time = {10,0,0}, type = floating}},
      freq = daily,
      interval = 2,
      until_count = {until, #date_time{date = {2005,1,7}, time = {5,40,00}, type = floating}}
     },

    autotest:mark(?LINE, "in_time_range/3 - 3a"),
    Current3a = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,7}, time = {6,5,23}, type = floating}),
    false = interpret_time:in_time_range(Timezone, Cond3, Current3a),
    autotest:mark(?LINE, "in_time_range/3 - 3b"),
    Current3b = ts_datetime:datetime_to_usec(start, dummy,
						#date_time{date = {2005,1,7}, time = {5,40,0}, type = floating}),
    true = interpret_time:in_time_range(Timezone, Cond3, Current3b),

    ok.


test9() ->
    %% sort_byparam(ByParams)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "sort_byparam/1 - 1"),
    ByParams = [{byday, {-1,mo}},
		{byday, {all,tu}},
		{byday, {2,we}},
		{byday, {all,th}},
		{byday, {all,fr}},
		{bymonth, 2},
		{bymonth, 1},
		{bysecond, 34}],
    ByParamsSorted = [
		      {bymonth, 1},
		      {bymonth, 2},
		      {byday, {-1,mo}},
		      {byday, {2,we}},
		      {byday, {all,fr}},
		      {byday, {all,th}},
		      {byday, {all,tu}},
		      {bysecond, 34}
		     ],
    ByParamsSorted = interpret_time:sort_byparam(ByParams),
    ok.


test10() ->
    %% get_start_level(Freq, ByParams)
    %%--------------------------------------------------------------------
    %% freq > byxxx
    autotest:mark(?LINE, "get_start_level/2 - 1"),
    yearly = interpret_time:get_start_level(yearly, [{bymonth, 1},{bymonth, 2},{byyearday, 31}]),

    %% freq < byxxx
    autotest:mark(?LINE, "get_start_level/2 - 2"),
    bymonth = interpret_time:get_start_level(secondly, [{bymonth, 1},{bymonth, 2},{byyearday, 31}]),
    ok.


test11() ->
    %% get_lowest_level(Freq, ByParams)
    %%--------------------------------------------------------------------
    %% freq > byxxx
    autotest:mark(?LINE, "get_lowest_level/2 - 1"),
    byyearday = interpret_time:get_lowest_level(yearly, [{bymonth, 1},{bymonth, 2},{byyearday, 31}]),

    %% freq < byxxx
    autotest:mark(?LINE, "get_lowest_level/2 - 2"),
    secondly = interpret_time:get_lowest_level(secondly, [{bymonth, 1},{bymonth, 2},{byyearday, 31}]),
    ok.


test12() ->
    %% get_level_params(Level, ByParams)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_level_params/2 - 1"),
    {[{bymonth, 1},{bymonth, 2}], [{byyearday, 31}]} =
	interpret_time:get_level_params(bymonth, [{bymonth, 1},{bymonth, 2},{byyearday, 31}]),
    ok.


test13() ->
    %% get_next_level(CurrentLevel, ByParams)
    %%--------------------------------------------------------------------
    %% freq trailed and preceded by byxxx attributes
    %% test a sequence of get_next_level calls

    %% remove bymonth
    autotest:mark(?LINE, "get_next_level/2 - 1"),
    {weekly, byyearday, [{byyearday, 31}]} =
	interpret_time:get_next_level(bymonth, [{bymonth, 1},{bymonth, 2},{byyearday, 31}],
				      #time_switch__cond_7{freq = secondly}),
    %% no ByParam
    autotest:mark(?LINE, "get_next_level/2 - 2"),
    {daily, byyearday, [{byyearday, 31}]} =
        interpret_time:get_next_level(weekly, [{byyearday, 31}], #time_switch__cond_7{freq = secondly}),
    %% no ByParam
    autotest:mark(?LINE, "get_next_level/2 - 3"),
    {byweekno, byyearday, [{byyearday, 31}]} =
 	interpret_time:get_next_level(daily, [{byyearday, 31}], #time_switch__cond_7{freq = secondly}),
    %% next level will process ByParam - NextLevel = NextProcessLevel
    autotest:mark(?LINE, "get_next_level/2 - 4"),
    {byyearday, byyearday, [{byyearday, 31}]} =
 	interpret_time:get_next_level(byweekno, [{byyearday, 31}],#time_switch__cond_7{freq = secondly}),
    %% remove byyearday
    autotest:mark(?LINE, "get_next_level/2 - 5"),
     {bymonthday, secondly, []} =
 	interpret_time:get_next_level(byyearday, [{byyearday, 31}], #time_switch__cond_7{freq = secondly}),
    %% handle empty ByParams list
    autotest:mark(?LINE, "get_next_level/2 - 6"),
    {byday, secondly, []} =
 	interpret_time:get_next_level(bymonthday, [], #time_switch__cond_7{freq = secondly}),

    %%
    autotest:mark(?LINE, "get_next_level/2 - 7"),
    {secondly, secondly, [{bysecond, 2}]} =
 	interpret_time:get_next_level(byminute, [{byminute, 1}, {bysecond, 2}],
				      #time_switch__cond_7{freq = secondly}),

    %% ----- test with "freq" greater, smaller and equal to current level
    %% bymonth < yearly
    autotest:mark(?LINE, "get_next_level/2 - 8"),
    {weekly, byyearday, [{byyearday, 31}]} =
	interpret_time:get_next_level(bymonth, [{bymonth, 1},{bymonth, 2},{byyearday, 31}],
				      #time_switch__cond_7{freq = yearly}),
    %% weekly < monthly
    autotest:mark(?LINE, "get_next_level/2 - 9"),
    {daily, byyearday, [{byyearday, 31}]} =
 	interpret_time:get_next_level(weekly, [{byyearday, 31}], #time_switch__cond_7{freq = monthly}),
    %% daily = daily
    autotest:mark(?LINE, "get_next_level/2 - 10"),
    {byweekno, byyearday, [{byyearday, 31}]} =
 	interpret_time:get_next_level(daily, [{byyearday, 31}], #time_switch__cond_7{freq = daily}),
    %% byweekno < weekly
    autotest:mark(?LINE, "get_next_level/2 - 11"),
    {byyearday, byyearday, [{byyearday, 31}]} =
 	interpret_time:get_next_level(byweekno, [{byyearday, 31}],#time_switch__cond_7{freq = weekly}),
    %% byyearday > hourly
    autotest:mark(?LINE, "get_next_level/2 - 12"),
     {bymonthday, hourly, []} =
 	interpret_time:get_next_level(byyearday, [{byyearday, 31}], #time_switch__cond_7{freq = hourly}),
    %% bymonthday > minutely
    autotest:mark(?LINE, "get_next_level/2 - 13"),
    {byday, minutely, []} =
 	interpret_time:get_next_level(bymonthday, [], #time_switch__cond_7{freq = minutely}),

    ok.


test14() ->
    %% match_param({ByType, Val}, MatchVal, StartTime, Wkst)
    %%--------------------------------------------------------------------
    %% focus is on testing byday (other byxxx should work as their tests
    %% have already been tested)
    StartTime = #start_time{
      year = 2005,
      month = 3,
      weekno = {2005,11},   % {Year, WeekNo}
      yearday = 73,
      monthday = 14,
      weekday = mo,
      hour = 12,
      minute = 30,
      second = 30
     },

    autotest:mark(?LINE, "match_param/4 - 1"),
    true = interpret_time:match_param({bysecond, 30}, 30, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 2"),
    true = interpret_time:match_param({byminute, 30}, 30, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 3"),
    true = interpret_time:match_param({byhour, 12}, 12, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 4"),
    true = interpret_time:match_param({bymonthday, 14}, 14, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 5"),
    true = interpret_time:match_param({bymonthday, -18}, 14, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 6"),
    true = interpret_time:match_param({byyearday, 73}, 73, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 7"),
    true = interpret_time:match_param({byyearday, -293}, 73, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 8"),
    true = interpret_time:match_param({byweekno, 11}, {2005,11}, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 9"),
    true = interpret_time:match_param({byweekno, -42}, {2005,11}, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 10"),
    true = interpret_time:match_param({bymonth, 3}, 3, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 11"),
    true = interpret_time:match_param({byday, {all,mo}}, mo, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 12"),
    true = interpret_time:match_param({byday, {2,mo}}, mo, StartTime, mo),
    autotest:mark(?LINE, "match_param/4 - 13"),
    true = interpret_time:match_param({byday, {-3,mo}}, mo, StartTime, mo),
    ok.

test15() ->
    %% is_reoccurrence(TimeSwitchCond, DateTime)
    %%--------------------------------------------------------------------
    TimeSwitchCond = #time_switch__cond_7{
     	  dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
	  dtend_duration = {duration, #duration{days = 5}},
	  freq = monthly,
	  interval = 3,
	  until_count = repeat_forever,
	  by_values = dummy,
    	  wkst = mo
     },
    %% check dtstart start point
    autotest:mark(?LINE, "is_reoccurrence/1 - 1"),
    true = interpret_time:is_reoccurrence(TimeSwitchCond,
					  #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating}),
    autotest:mark(?LINE, "is_reoccurrence/1 - 2"),
    true = interpret_time:is_reoccurrence(TimeSwitchCond,
					  #date_time{date = {2005,4,1}, time = {0,0,0}, type = floating}),
    %% month in next year
    autotest:mark(?LINE, "is_reoccurrence/1 - 3"),
    true = interpret_time:is_reoccurrence(TimeSwitchCond,
					  #date_time{date = {2006,1,1}, time = {0,0,0}, type = floating}),
    %% check for failure
    autotest:mark(?LINE, "is_reoccurrence/1 - 4"),
    false = interpret_time:is_reoccurrence(TimeSwitchCond,
					   #date_time{date = {2005,2,1}, time = {0,0,0}, type = floating}),
    ok.


test16() ->
    %% create_startpoints(Start, Freq, Wkst)
    %%--------------------------------------------------------------------
    CSTimeSwitchCond1 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 5}},
      freq = yearly,
      interval = 3,
      until_count = repeat_forever,
      by_values = dummy,
      wkst = mo
     },

    %% yearly
    autotest:mark(?LINE, "create_startpoints/3 - 1"),
    CSStart1 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    [CSStart1] = interpret_time:create_startpoints(CSStart1, yearly, CSTimeSwitchCond1),

    %% monthly
    autotest:mark(?LINE, "create_startpoints/3 - 2"),
    CSTimeSwitchCond2 = CSTimeSwitchCond1#time_switch__cond_7{freq = monthly},
    CSStart2 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    CSMatch2 = [CSStart2#date_time{date = {2005,1,1}},
		CSStart2#date_time{date = {2005,4,1}},
		CSStart2#date_time{date = {2005,7,1}},
		CSStart2#date_time{date = {2005,10,1}}
	       ],
    CSMatch2 = interpret_time:create_startpoints(CSStart2, monthly, CSTimeSwitchCond2),

    %% weekly
    autotest:mark(?LINE, "create_startpoints/3 - 3"),
    CSTimeSwitchCond3 = CSTimeSwitchCond1#time_switch__cond_7{freq = weekly},
    CSStart3 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    CSMatch3 = [
		CSStart3#date_time{date = {2005,4,16}},
 		CSStart3#date_time{date = {2005,5,7}},
 		CSStart3#date_time{date = {2005,5,28}}
 	       ],
    CSMatch3 = interpret_time:create_startpoints(CSStart3, weekly, CSTimeSwitchCond3),

    %% daily
    autotest:mark(?LINE, "create_startpoints/3 - 4"),
    CSTimeSwitchCond4 = CSTimeSwitchCond1#time_switch__cond_7{freq = daily},
    CSStart4 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    CSMatch4 = [
		CSStart4#date_time{date = {2005,5,1}},
		CSStart4#date_time{date = {2005,5,4}},
		CSStart4#date_time{date = {2005,5,7}},
		CSStart4#date_time{date = {2005,5,10}},
		CSStart4#date_time{date = {2005,5,13}},
		CSStart4#date_time{date = {2005,5,16}},
		CSStart4#date_time{date = {2005,5,19}},
		CSStart4#date_time{date = {2005,5,22}},
		CSStart4#date_time{date = {2005,5,25}},
		CSStart4#date_time{date = {2005,5,28}},
		CSStart4#date_time{date = {2005,5,31}}
	       ],
    CSMatch4 = interpret_time:create_startpoints(CSStart4, daily, CSTimeSwitchCond4),

    %% hourly
    autotest:mark(?LINE, "create_startpoints/3 - 5"),
    CSTimeSwitchCond5 = CSTimeSwitchCond1#time_switch__cond_7{freq = hourly},
    CSStart5 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    CSMatch5 = [
		CSStart5#date_time{date = {2005,5,1}, time = {0,0,0}},
		CSStart5#date_time{date = {2005,5,1}, time = {3,0,0}},
		CSStart5#date_time{date = {2005,5,1}, time = {6,0,0}},
		CSStart5#date_time{date = {2005,5,1}, time = {9,0,0}},
		CSStart5#date_time{date = {2005,5,1}, time = {12,0,0}},
		CSStart5#date_time{date = {2005,5,1}, time = {15,0,0}},
		CSStart5#date_time{date = {2005,5,1}, time = {18,0,0}},
		CSStart5#date_time{date = {2005,5,1}, time = {21,0,0}}
	       ],
    CSMatch5 = interpret_time:create_startpoints(CSStart5, hourly, CSTimeSwitchCond5),

    %% minutely - test with minute offset in dtstart
    autotest:mark(?LINE, "create_startpoints/3 - 6"),
    CSTimeSwitchCond6 =
	CSTimeSwitchCond1#time_switch__cond_7{dtstart = #date_time{date = {2005,5,1},
								   time = {0,2,0},
								   type = floating},
					      freq = minutely,
					      interval = 9},
    CSStart6 = #date_time{date = {2005,5,1}, time = {12,32,0}, type = floating},
    CSMatch6 = [
		CSStart6#date_time{date = {2005,5,1}, time = {11,53,0}},
		CSStart6#date_time{date = {2005,5,1}, time = {12,2,0}},
		CSStart6#date_time{date = {2005,5,1}, time = {12,11,0}},
		CSStart6#date_time{date = {2005,5,1}, time = {12,20,0}},
		CSStart6#date_time{date = {2005,5,1}, time = {12,29,0}},
		CSStart6#date_time{date = {2005,5,1}, time = {12,38,0}},
		CSStart6#date_time{date = {2005,5,1}, time = {12,47,0}},
		CSStart6#date_time{date = {2005,5,1}, time = {12,56,0}}
	       ],
    CSMatch6 = interpret_time:create_startpoints(CSStart6, minutely, CSTimeSwitchCond6),

    %% secondly - test with second offset in dtstart
    autotest:mark(?LINE, "create_startpoints/3 - 7"),
    CSTimeSwitchCond7 =
	CSTimeSwitchCond1#time_switch__cond_7{dtstart = #date_time{date = {2005,5,1},
								   time = {0,2,3},
								   type = floating},
					      freq = secondly,
					      interval = 9},
    CSStart7 = #date_time{date = {2005,5,1}, time = {12,32,30}, type = floating},
    CSMatch7 = [
		CSStart7#date_time{date = {2005,5,1}, time = {12,31,54}},
		CSStart7#date_time{date = {2005,5,1}, time = {12,32,3}},
		CSStart7#date_time{date = {2005,5,1}, time = {12,32,12}},
		CSStart7#date_time{date = {2005,5,1}, time = {12,32,21}},
		CSStart7#date_time{date = {2005,5,1}, time = {12,32,30}},
		CSStart7#date_time{date = {2005,5,1}, time = {12,32,39}},
		CSStart7#date_time{date = {2005,5,1}, time = {12,32,48}},
		CSStart7#date_time{date = {2005,5,1}, time = {12,32,57}}
	       ],
    CSMatch7 = interpret_time:create_startpoints(CSStart7, secondly, CSTimeSwitchCond7),
    ok.

test17() ->
    %% create_startpoints(Start, ByxxxVals, ByType, Wkst)
    %%--------------------------------------------------------------------
    %% bymonth
    autotest:mark(?LINE, "create_startpoints/4 - 1"),
    CS2Start1 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
    CS2ByxxxVals1 = [{bymonth,3}, {bymonth,4}, {bymonth,11}],
    CS2Match1 = [
		 CS2Start1#date_time{date = {2005,3,1}},
		 CS2Start1#date_time{date = {2005,4,1}},
		 CS2Start1#date_time{date = {2005,11,1}}
		],
    CS2Match1 = interpret_time:create_startpoints(CS2Start1, CS2ByxxxVals1, bymonth, mo),

    %% byweekno
    autotest:mark(?LINE, "create_startpoints/4 - 2"),
    CS2Start2 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    CS2ByxxxVals2 = [{byweekno,4}, {byweekno,17}, {byweekno,22}, {byweekno, -33}],
    CS2Match2 = lists:sort([
			    CS2Start2#date_time{date = {2005,1,24}},
			    CS2Start2#date_time{date = {2005,4,25}},
			    CS2Start2#date_time{date = {2005,5,30}},
			    CS2Start2#date_time{date = {2005,5,16}} % week -33/20
			   ]),
    CS2Match2 = lists:sort(interpret_time:create_startpoints(CS2Start2, CS2ByxxxVals2, byweekno, mo)),

    %% byyearday
    autotest:mark(?LINE, "create_startpoints/4 - 3"),
    CS2Start3 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    CS2ByxxxVals3 = [{byyearday,1},
		     {byyearday,-365},
		     {byyearday,-366}, % doesn't exist (not a leap year)
		     {byyearday,366}, % doesn't exist (not a leap year)
		     {byyearday,100},
		     {byyearday,-1}],
    CS2Match3 = lists:sort([
			    CS2Start3#date_time{date = {2005,1,1}},
			    CS2Start3#date_time{date = {2005,1,1}},
			    CS2Start3#date_time{date = {2005,4,10}},
			    CS2Start3#date_time{date = {2005,12,31}}
			   ]),
    CS2Match3 = lists:sort(interpret_time:create_startpoints(CS2Start3, CS2ByxxxVals3, byyearday, mo)),

    %% bymonthday
    autotest:mark(?LINE, "create_startpoints/4 - 4"),
    CS2Start4 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    CS2ByxxxVals4 = [{bymonthday,1},
		     {bymonthday,-32}, % doesn't exist
		     {bymonthday,-31},
		     {bymonthday,32}, % doesn't exist
		     {bymonthday,31},
		     {bymonthday,-10}],
    CS2Match4 = lists:sort([
			    CS2Start4#date_time{date = {2005,5,1}},
			    CS2Start4#date_time{date = {2005,5,1}},
			    CS2Start4#date_time{date = {2005,5,31}},
			    CS2Start4#date_time{date = {2005,5,22}}
			   ]),
    CS2Match4 = lists:sort(interpret_time:create_startpoints(CS2Start4, CS2ByxxxVals4, bymonthday, mo)),

    %% byday
    autotest:mark(?LINE, "create_startpoints/4 - 5"),
    CS2Start5 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    CS2ByxxxVals5 = [{byday,{all,mo}},
		     {byday,{1,tu}},
		     {byday,{3,tu}},
		     {byday,{-2,fr}},
		     {byday,{-5,fr}} % doesn't exit
		    ],
    CS2Match5 = lists:sort([
			    %% mo
			    CS2Start5#date_time{date = {2005,5,2}},
			    CS2Start5#date_time{date = {2005,5,9}},
			    CS2Start5#date_time{date = {2005,5,16}},
			    CS2Start5#date_time{date = {2005,5,23}},
			    CS2Start5#date_time{date = {2005,5,30}},
			    %% tu
			    CS2Start5#date_time{date = {2005,5,3}},
			    CS2Start5#date_time{date = {2005,5,17}},
			    %% fr
			    CS2Start5#date_time{date = {2005,5,20}}
			   ]),
    CS2Match5 = lists:sort(interpret_time:create_startpoints(CS2Start5, CS2ByxxxVals5, byday, mo)),

    %% byhour
    autotest:mark(?LINE, "create_startpoints/4 - 6"),
    CS2Start6 = #date_time{date = {2005,5,1}, time = {1,1,1}, type = floating},
    CS2ByxxxVals6 = [{byhour,1}, {byhour,3}, {byhour,23}],
    CS2Match6 = lists:sort([
			    CS2Start6#date_time{date = {2005,5,1}, time = {1,1,1}},
			    CS2Start6#date_time{date = {2005,5,1}, time = {3,1,1}},
			    CS2Start6#date_time{date = {2005,5,1}, time = {23,1,1}}
			   ]),
    CS2Match6 = lists:sort(interpret_time:create_startpoints(CS2Start6, CS2ByxxxVals6, byhour, mo)),

    %% byminute
    autotest:mark(?LINE, "create_startpoints/4 - 7"),
    CS2Start7 = #date_time{date = {2005,5,1}, time = {1,1,1}, type = floating},
    CS2ByxxxVals7 = [{byminute,1}, {byminute,3}, {byminute,23}],
    CS2Match7 = lists:sort([
			    CS2Start7#date_time{date = {2005,5,1}, time = {1,1,1}},
			    CS2Start7#date_time{date = {2005,5,1}, time = {1,3,1}},
			    CS2Start7#date_time{date = {2005,5,1}, time = {1,23,1}}
			   ]),
    CS2Match7 = lists:sort(interpret_time:create_startpoints(CS2Start7, CS2ByxxxVals7, byminute, mo)),

    %% bysecond
    autotest:mark(?LINE, "create_startpoints/4 - 8"),
    CS2Start8 = #date_time{date = {2005,5,1}, time = {1,1,1}, type = floating},
    CS2ByxxxVals8 = [{bysecond,1}, {bysecond,3}, {bysecond,23}],
    CS2Match8 = lists:sort([
			    CS2Start8#date_time{date = {2005,5,1}, time = {1,1,1}},
			    CS2Start8#date_time{date = {2005,5,1}, time = {1,1,3}},
			    CS2Start8#date_time{date = {2005,5,1}, time = {1,1,23}}
			   ]),
    CS2Match8 = lists:sort(interpret_time:create_startpoints(CS2Start8, CS2ByxxxVals8, bysecond, mo)),
    ok.


test18() ->
    %% get_next_process_level(Level, ByParams, TimeSwitchCond)
    %%--------------------------------------------------------------------
    %% generator comes last
    ByParams1 =
	[{bymonth, N} || N <- lists:seq(1,12)] ++
	[{byweekno, N} || N <- lists:seq(1,53)] ++
	[{byyearday, N} || N <- lists:seq(1,366)] ++
	[{bymonthday, N} || N <- lists:seq(1,31)] ++
	[{byday,{all,mo}}, {byday,{all,tu}}, {byday,{all,we}}, {byday,{all,th}}, {byday,{all,fr}},
	{byday,{all,sa}}, {byday,{all,su}}] ++
	[{byhour, N} || N <- lists:seq(0,23)] ++
	[{byminute, N} || N <- lists:seq(0,59)] ++
	%% all odd seconds
	[{bysecond, N} || N <- lists:seq(0,59), (N rem 2) == 1],

    TSC1 = #time_switch__cond_7{freq = secondly},

    autotest:mark(?LINE, "get_next_process_level/3 - 1.1"),
    bymonth = interpret_time:get_next_process_level(yearly, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.2"),
    bymonth = interpret_time:get_next_process_level(monthly, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.3"),
    byweekno = interpret_time:get_next_process_level(weekly, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.4"),
    byweekno = interpret_time:get_next_process_level(daily, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.5"),
    byhour = interpret_time:get_next_process_level(hourly, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.6"),
    byminute = interpret_time:get_next_process_level(minutely, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.7"),
    bysecond = interpret_time:get_next_process_level(secondly, ByParams1, TSC1),

    autotest:mark(?LINE, "get_next_process_level/3 - 1.8"),
    byweekno = interpret_time:get_next_process_level(bymonth, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.9"),
    byyearday = interpret_time:get_next_process_level(byweekno, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.10"),
    bymonthday = interpret_time:get_next_process_level(byyearday, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.11"),
    byday = interpret_time:get_next_process_level(bymonthday, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.12"),
    byhour = interpret_time:get_next_process_level(byday, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.13"),
    byminute = interpret_time:get_next_process_level(byhour, ByParams1, TSC1),
    autotest:mark(?LINE, "get_next_process_level/3 - 1.14"),
    secondly = interpret_time:get_next_process_level(byminute, ByParams1, TSC1),  %% <---

    %% generator comes first
     ByParams2 =
	[{bymonth, N} || N <- lists:seq(1,12)] ++
	[{byweekno, N} || N <- lists:seq(1,53)] ++
	[{byyearday, N} || N <- lists:seq(1,366)] ++
	[{bymonthday, N} || N <- lists:seq(1,31)] ++
	[{byday,{all,mo}}, {byday,{all,tu}}, {byday,{all,we}}, {byday,{all,th}}, {byday,{all,fr}},
	{byday,{all,sa}}, {byday,{all,su}}] ++
	[{byhour, N} || N <- lists:seq(0,23)] ++
	[{byminute, N} || N <- lists:seq(0,59)] ++
	%% all odd seconds
	[{bysecond, N} || N <- lists:seq(0,59), (N rem 2) == 1],

    TSC2 = #time_switch__cond_7{freq = yearly},

    autotest:mark(?LINE, "get_next_process_level/3 - 2.1"),
    bymonth = interpret_time:get_next_process_level(yearly, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.2"),
    bymonth = interpret_time:get_next_process_level(monthly, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.3"),
    byweekno = interpret_time:get_next_process_level(weekly, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.4"),
    byweekno = interpret_time:get_next_process_level(daily, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.5"),
    byhour = interpret_time:get_next_process_level(hourly, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.6"),
    byminute = interpret_time:get_next_process_level(minutely, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.7"),
    bysecond = interpret_time:get_next_process_level(secondly, ByParams2, TSC2),

    autotest:mark(?LINE, "get_next_process_level/3 - 2.8"),
    byweekno = interpret_time:get_next_process_level(bymonth, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.9"),
    byyearday = interpret_time:get_next_process_level(byweekno, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.10"),
    bymonthday = interpret_time:get_next_process_level(byyearday, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.11"),
    byday = interpret_time:get_next_process_level(bymonthday, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.12"),
    byhour = interpret_time:get_next_process_level(byday, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.13"),
    byminute = interpret_time:get_next_process_level(byhour, ByParams2, TSC2),
    autotest:mark(?LINE, "get_next_process_level/3 - 2.14"),
    bysecond = interpret_time:get_next_process_level(byminute, ByParams2, TSC2),  %% <---

    %% freq in middle of byxxx parameters
    ByParams3 =
	[{bymonth, N} || N <- lists:seq(1,12)] ++
	[{byweekno, N} || N <- lists:seq(1,53)] ++
	[{byyearday, N} || N <- lists:seq(1,366)] ++
	[{bymonthday, N} || N <- lists:seq(1,31)] ++
	[{byday,{all,mo}}, {byday,{all,tu}}, {byday,{all,we}}, {byday,{all,th}}, {byday,{all,fr}},
	 {byday,{all,sa}}, {byday,{all,su}}] ++
	[{byhour, N} || N <- lists:seq(0,23)] ++
	[{byminute, N} || N <- lists:seq(0,59)] ++
	%% all odd seconds
	[{bysecond, N} || N <- lists:seq(0,59), (N rem 2) == 1],

    TSC3 = #time_switch__cond_7{
      freq = daily
     },

    autotest:mark(?LINE, "get_next_process_level/3 - 3.1"),
    bymonth = interpret_time:get_next_process_level(yearly, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.2"),
    bymonth = interpret_time:get_next_process_level(monthly, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.3"),
    daily = interpret_time:get_next_process_level(weekly, ByParams3, TSC3), %% <---
    autotest:mark(?LINE, "get_next_process_level/3 - 3.4"),
    byweekno = interpret_time:get_next_process_level(daily, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.5"),
    byhour = interpret_time:get_next_process_level(hourly, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.6"),
    byminute = interpret_time:get_next_process_level(minutely, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.7"),
    bysecond = interpret_time:get_next_process_level(secondly, ByParams3, TSC3),

    autotest:mark(?LINE, "get_next_process_level/3 - 3.8"),
    daily = interpret_time:get_next_process_level(bymonth, ByParams3, TSC3), %% <---
    autotest:mark(?LINE, "get_next_process_level/3 - 3.9"),
    byyearday = interpret_time:get_next_process_level(byweekno, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.10"),
    bymonthday = interpret_time:get_next_process_level(byyearday, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.11"),
    byday = interpret_time:get_next_process_level(bymonthday, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.12"),
    byhour = interpret_time:get_next_process_level(byday, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.13"),
    byminute = interpret_time:get_next_process_level(byhour, ByParams3, TSC3),
    autotest:mark(?LINE, "get_next_process_level/3 - 3.14"),
    bysecond = interpret_time:get_next_process_level(byminute, ByParams3, TSC3),


    %%
    ByParams4 = [{bymonth, 1}],
    TSC4 = #time_switch__cond_7{
      freq = secondly
     },
    autotest:mark(?LINE, "get_next_process_level/3 - 4.1"),
    bymonth = interpret_time:get_next_process_level(yearly, ByParams4, TSC4),
    autotest:mark(?LINE, "get_next_process_level/3 - 4.2"),
    secondly = interpret_time:get_next_process_level(bymonth, ByParams4, TSC4),
    ok.


test19_1() ->
    %% byxxx_match(TimeSwitchCond, CurrentDateTime)
    %%--------------------------------------------------------------------
    %% weekend in certain months every third year
    BMTimeSwitchCond1 = #time_switch__cond_7{
      dtstart = #date_time{date = {2002,4,10}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 1}},
      freq = yearly,
      interval = 3,
      until_count = repeat_forever,
      by_values = [{bymonth,1}, {bymonth,2}, {bymonth,4}, {bymonth,8}, {byday, {all,su}}, {byday, {all,sa}}],
      wkst = mo
     },
    %% check that duration is valid
    true = ts_duration:valid_duration(BMTimeSwitchCond1),

    autotest:mark(?LINE, "byxxx_match/2 - 1a"),
    %% second year, month = 4, day = 10 time = 00:00:00 in [00:00:00, 00:59:59] range and day = weekend
    BMCurrentDateTime1a = #date_time{date = {2005,4,10}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond1, BMCurrentDateTime1a),

    autotest:mark(?LINE, "byxxx_match/2 - 1b"),
    %% the second after a matching time range [2005-04-10 00:00:00, 2005-04-10 00:59:59]
    BMCurrentDateTime1b = #date_time{date = {2005,4,10}, time = {1,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond1, BMCurrentDateTime1b),

    autotest:mark(?LINE, "byxxx_match/2 - 1c"),
    %% the last second of a matching time range [2005-04-10 00:00:00, 2005-04-10 00:59:59]
    BMCurrentDateTime1c = #date_time{date = {2005,4,10}, time = {0,59,59}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond1, BMCurrentDateTime1c),
    ok.

test19_2() ->
    %%----------------------
    %% a work week match pattern
    BMTimeSwitchCond2 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {8,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8, minutes = 40}},
      freq = daily,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday,{all,mo}}, {byday,{all,tu}}, {byday,{all,we}}, {byday,{all,th}}, {byday,{all,fr}}],
      wkst = mo
     },
    %% check that duration is valid
    true = ts_duration:valid_duration(BMTimeSwitchCond2),

    %% first second
    autotest:mark(?LINE, "byxxx_match/2 - 2a"),
    BMCurrentDateTime2a = #date_time{date = {2005,4,11}, time = {8,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond2, BMCurrentDateTime2a),

    %% last second
    autotest:mark(?LINE, "byxxx_match/2 - 2b"),
    BMCurrentDateTime2b = #date_time{date = {2005,4,11}, time = {16,39,59}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond2, BMCurrentDateTime2b),

    %% in middle of legal range
    autotest:mark(?LINE, "byxxx_match/2 - 2c"),
    BMCurrentDateTime2c = #date_time{date = {2005,4,11}, time = {10,10,10}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond2, BMCurrentDateTime2c),

    %% fail - day = su
    autotest:mark(?LINE, "byxxx_match/2 - 2d"),
    BMCurrentDateTime2d = #date_time{date = {2005,4,10}, time = {10,10,10}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond2, BMCurrentDateTime2d),
    ok.

test19_3() ->
    %%----------------------
    %% supply as many byxxx values as possible - each odd second is a valid range
    ByxxxVals3 =
	[{bymonth, N} || N <- lists:seq(1,12)] ++
	[{byweekno, N} || N <- lists:seq(1,53)] ++
	[{byyearday, N} || N <- lists:seq(1,366)] ++
	[{bymonthday, N} || N <- lists:seq(1,31)] ++
	[{byday,{all,mo}}, {byday,{all,tu}}, {byday,{all,we}}, {byday,{all,th}}, {byday,{all,fr}},
	{byday,{all,sa}}, {byday,{all,su}}] ++
	[{byhour, N} || N <- lists:seq(0,23)] ++
	[{byminute, N} || N <- lists:seq(0,59)] ++
	%% all odd seconds
	[{bysecond, N} || N <- lists:seq(0,59), (N rem 2) == 1],

    %% io:format("ByxxxVals3 ~p~n",[ByxxxVals3]),

    BMTimeSwitchCond3 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = secondly,
      interval = 1,
      until_count = repeat_forever,
      by_values = ByxxxVals3,
      wkst = mo
     },
    %% check that duration is valid
    true = ts_duration:valid_duration(BMTimeSwitchCond3),

    %% even second - fail
    autotest:mark(?LINE, "byxxx_match/2 - 3a"),
    BMCurrentDateTime3a = #date_time{date = {2005,1,3}, time = {0,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond3, BMCurrentDateTime3a),

    %% odd second
    autotest:mark(?LINE, "byxxx_match/2 - 3b"),
    BMCurrentDateTime3b = #date_time{date = {2005,1,3}, time = {0,0,1}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond3, BMCurrentDateTime3b),
    ok.

test19_4() ->
    %%----------------------
    %% Current week is part of previous year
    ByxxxVals4 =
	%% all odd seconds
	[{bysecond, N} || N <- lists:seq(0,59), (N rem 2) == 1],

    BMTimeSwitchCond4 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = secondly,
      interval = 1,
      until_count = repeat_forever,
      by_values = ByxxxVals4,
      wkst = mo
     },
    %% check that duration is valid
    true = ts_duration:valid_duration(BMTimeSwitchCond4),

    autotest:mark(?LINE, "byxxx_match/2 - 4"),
    BMCurrentDateTime4 = #date_time{date = {2005,1,1}, time = {12,0,1}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond4, BMCurrentDateTime4),
    ok.

test19_5() ->
    %%----------------------
    %% byxxx > freq
    BMTimeSwitchCond5 = #time_switch__cond_7{
      dtstart = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = secondly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonth, 1}],
      wkst = mo
     },
    %% check that duration is valid
    true = ts_duration:valid_duration(BMTimeSwitchCond5),

    autotest:mark(?LINE, "byxxx_match/2 - 5a"),
    BMCurrentDateTime5a = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond5, BMCurrentDateTime5a),

    autotest:mark(?LINE, "byxxx_match/2 - 5b"),
    BMCurrentDateTime5b = #date_time{date = {2005,1,31}, time = {23,59,59}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond5, BMCurrentDateTime5b),

    %% first second after range
    autotest:mark(?LINE, "byxxx_match/2 - 5c"),
    BMCurrentDateTime5c = #date_time{date = {2005,2,1}, time = {0,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond5, BMCurrentDateTime5c),

    %% first second before range
    autotest:mark(?LINE, "byxxx_match/2 - 5d"),
    BMCurrentDateTime5d = #date_time{date = {2004,12,31}, time = {23,59,59}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond5, BMCurrentDateTime5d),
    ok.

test19_6() ->
    %%----------------------
    %% test with all freq values (secondly - yearly) and lowest byxxx - bysecond
    BMTimeSwitchCond6 = #time_switch__cond_7{
      dtstart = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = dummy,
      interval = 2,
      until_count = repeat_forever,
      by_values = dummy,
      wkst = mo
     },

    BMTimeSwitchCond6a = BMTimeSwitchCond6#time_switch__cond_7{freq = secondly, by_values = [{bysecond, 0}]},
    BMTimeSwitchCond6b = BMTimeSwitchCond6#time_switch__cond_7{freq = minutely, by_values = [{bysecond, 0}]},
    BMTimeSwitchCond6c = BMTimeSwitchCond6#time_switch__cond_7{freq = hourly, by_values = [{bysecond, 0}]},
    BMTimeSwitchCond6d = BMTimeSwitchCond6#time_switch__cond_7{freq = daily, by_values = [{bysecond, 0}]},
    BMTimeSwitchCond6e = BMTimeSwitchCond6#time_switch__cond_7{freq = weekly, by_values = [{bysecond, 0}]},
    BMTimeSwitchCond6f = BMTimeSwitchCond6#time_switch__cond_7{freq = monthly, by_values = [{bysecond, 0}]},
    BMTimeSwitchCond6g = BMTimeSwitchCond6#time_switch__cond_7{freq = yearly, by_values = [{bysecond, 0}]},

    %% secondly
    autotest:mark(?LINE, "byxxx_match/2 - 6a"),
    true = ts_duration:valid_duration(BMTimeSwitchCond6a),
    BMCurrentDateTime6a_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6a, BMCurrentDateTime6a_1),
    BMCurrentDateTime6a_2 = #date_time{date = {2004,10,10}, time = {8,34,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6a, BMCurrentDateTime6a_2),

    %% minutely
    autotest:mark(?LINE, "byxxx_match/2 - 6b"),
    true = ts_duration:valid_duration(BMTimeSwitchCond6b),
    BMCurrentDateTime6b_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6b, BMCurrentDateTime6b_1),
    BMCurrentDateTime6b_2 = #date_time{date = {2004,10,10}, time = {8,34,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6b, BMCurrentDateTime6b_2),
    BMCurrentDateTime6b_3 = #date_time{date = {2004,10,10}, time = {8,34,13}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond6b, BMCurrentDateTime6b_3),

    %% hourly
    autotest:mark(?LINE, "byxxx_match/2 - 6c"),
    true = ts_duration:valid_duration(BMTimeSwitchCond6c),
    BMCurrentDateTime6c_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6c, BMCurrentDateTime6c_1),
    BMCurrentDateTime6c_2 = #date_time{date = {2004,10,10}, time = {8,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6c, BMCurrentDateTime6c_2),

    %% daily
    autotest:mark(?LINE, "byxxx_match/2 - 6d"),
    true = ts_duration:valid_duration(BMTimeSwitchCond6d),
    BMCurrentDateTime6d_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6d, BMCurrentDateTime6d_1),
    BMCurrentDateTime6d_2 = #date_time{date = {2004,1,3}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6d, BMCurrentDateTime6d_2),

    %% weekly
    autotest:mark(?LINE, "byxxx_match/2 - 6e"),
    true = ts_duration:valid_duration(BMTimeSwitchCond6e),
    BMCurrentDateTime6e_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6e, BMCurrentDateTime6e_1),
    BMCurrentDateTime6e_2 = #date_time{date = {2004,1,15}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6e, BMCurrentDateTime6e_2),
    BMCurrentDateTime6e_3 = #date_time{date = {2004,1,14}, time = {0,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond6e, BMCurrentDateTime6e_3),

    %% monthly
    autotest:mark(?LINE, "byxxx_match/2 - 6f"),
    true = ts_duration:valid_duration(BMTimeSwitchCond6f),
    BMCurrentDateTime6f_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6f, BMCurrentDateTime6f_1),
    BMCurrentDateTime6f_2 = #date_time{date = {2004,3,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6f, BMCurrentDateTime6f_2),
    BMCurrentDateTime6f_3 = #date_time{date = {2004,6,1}, time = {0,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond6f, BMCurrentDateTime6f_3),

    %% yearly
    autotest:mark(?LINE, "byxxx_match/2 - 6g"),
    true = ts_duration:valid_duration(BMTimeSwitchCond6g),
    BMCurrentDateTime6g_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6g, BMCurrentDateTime6g_1),
    BMCurrentDateTime6g_2 = #date_time{date = {2006,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond6g, BMCurrentDateTime6g_2),
    BMCurrentDateTime6g_3 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond6g, BMCurrentDateTime6g_3),
    ok.

test19_7() ->
    %%----------------------
    %% only allow odd time units
    %% byweekno, byyearday and byday are not used as they are hard to combine with regular date-time values
    ByxxxVals7 =
	[{bymonth, N} || N <- lists:seq(1,12), (N rem 2) == 1] ++
	[{bymonthday, N} || N <- lists:seq(1,31), (N rem 2) == 1] ++
	[{byhour, N} || N <- lists:seq(0,23), (N rem 2) == 1] ++
	[{byminute, N} || N <- lists:seq(0,59), (N rem 2) == 1] ++
	[{bysecond, N} || N <- lists:seq(0,59), (N rem 2) == 1],

    %% io:format("ByxxxVals7 ~p~n",[ByxxxVals7]),

    BMTimeSwitchCond7 = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = secondly,
      interval = 1,
      until_count = repeat_forever,
      by_values = ByxxxVals7,
      wkst = mo
     },
    %% check that duration is valid
    true = ts_duration:valid_duration(BMTimeSwitchCond7),

    %% even day - fail
    autotest:mark(?LINE, "byxxx_match/2 - 7a"),
    BMCurrentDateTime7a = #date_time{date = {2005,1,2}, time = {1,1,1}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond7, BMCurrentDateTime7a),

    %% odd time units
    autotest:mark(?LINE, "byxxx_match/2 - 7b"),
    BMCurrentDateTime7b = #date_time{date = {2005,1,3}, time = {1,1,1}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond7, BMCurrentDateTime7b),

    %% odd time units in the far future
    %% unix time - "maximum representable time is 2038-01-19T03:14:07Z" - wikipedia
    %% bugs occur after this
    autotest:mark(?LINE, "byxxx_match/2 - 7c"),
    BMCurrentDateTime7c = #date_time{date = {2037,11,29}, time = {11,51,49}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond7, BMCurrentDateTime7c),
    ok.

test19_8() ->
    %%----------------------
    %% test with all freq values (secondly - yearly) and highest byxxx (bymonth)
    BMTimeSwitchCond8 = #time_switch__cond_7{
      dtstart = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = dummy,
      interval = 2,
      until_count = repeat_forever,
      by_values = dummy,
      wkst = mo
     },

    BMTimeSwitchCond8a = BMTimeSwitchCond8#time_switch__cond_7{freq = secondly, by_values = [{bymonth, 1}]},
    BMTimeSwitchCond8b = BMTimeSwitchCond8#time_switch__cond_7{freq = minutely, by_values = [{bymonth, 1}]},
    BMTimeSwitchCond8c = BMTimeSwitchCond8#time_switch__cond_7{freq = hourly, by_values = [{bymonth, 1}]},
    BMTimeSwitchCond8d = BMTimeSwitchCond8#time_switch__cond_7{freq = daily, by_values = [{bymonth, 1}]},
    BMTimeSwitchCond8e = BMTimeSwitchCond8#time_switch__cond_7{freq = weekly, by_values = [{bymonth, 1}]},
    BMTimeSwitchCond8f = BMTimeSwitchCond8#time_switch__cond_7{freq = monthly, by_values = [{bymonth, 1}]},
    BMTimeSwitchCond8g = BMTimeSwitchCond8#time_switch__cond_7{freq = yearly, by_values = [{bymonth, 1}]},

    %% secondly
    autotest:mark(?LINE, "byxxx_match/2 - 8a"),
    true = ts_duration:valid_duration(BMTimeSwitchCond8a),
    BMCurrentDateTime8a_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8a, BMCurrentDateTime8a_1),
    BMCurrentDateTime8a_2 = #date_time{date = {2005,1,10}, time = {8,34,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8a, BMCurrentDateTime8a_2),
    %% odd second
    BMCurrentDateTime8a_3 = #date_time{date = {2005,1,10}, time = {8,34,1}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond8a, BMCurrentDateTime8a_3),
    %% not month = 1
    BMCurrentDateTime8a_4 = #date_time{date = {2005,2,10}, time = {8,34,2}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond8a, BMCurrentDateTime8a_4),

    %% minutely
    autotest:mark(?LINE, "byxxx_match/2 - 8b"),
    true = ts_duration:valid_duration(BMTimeSwitchCond8b),
    BMCurrentDateTime8b_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8b, BMCurrentDateTime8b_1),
    BMCurrentDateTime8b_2 = #date_time{date = {2004,1,10}, time = {8,34,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8b, BMCurrentDateTime8b_2),
    %% fail because second /= 0
    BMCurrentDateTime8b_3 = #date_time{date = {2004,1,10}, time = {8,34,13}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond8b, BMCurrentDateTime8b_3),

    %% hourly
    autotest:mark(?LINE, "byxxx_match/2 - 8c"),
    true = ts_duration:valid_duration(BMTimeSwitchCond8c),
    BMCurrentDateTime8c_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8c, BMCurrentDateTime8c_1),
    BMCurrentDateTime8c_2 = #date_time{date = {2004,1,10}, time = {8,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8c, BMCurrentDateTime8c_2),

    %% daily
    autotest:mark(?LINE, "byxxx_match/2 - 8d"),
    true = ts_duration:valid_duration(BMTimeSwitchCond8d),
    BMCurrentDateTime8d_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8d, BMCurrentDateTime8d_1),
    BMCurrentDateTime8d_2 = #date_time{date = {2004,1,3}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8d, BMCurrentDateTime8d_2),

    %% weekly
    autotest:mark(?LINE, "byxxx_match/2 - 8e"),
    true = ts_duration:valid_duration(BMTimeSwitchCond8e),
    BMCurrentDateTime8e_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8e, BMCurrentDateTime8e_1),
    BMCurrentDateTime8e_2 = #date_time{date = {2004,1,15}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8e, BMCurrentDateTime8e_2),
    BMCurrentDateTime8e_3 = #date_time{date = {2004,1,14}, time = {0,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond8e, BMCurrentDateTime8e_3),

    %% monthly
    autotest:mark(?LINE, "byxxx_match/2 - 8f"),
    true = ts_duration:valid_duration(BMTimeSwitchCond8f),
    BMCurrentDateTime8f_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8f, BMCurrentDateTime8f_1),
    BMCurrentDateTime8f_2 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8f, BMCurrentDateTime8f_2),
    BMCurrentDateTime8f_3 = #date_time{date = {2004,5,1}, time = {0,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond8f, BMCurrentDateTime8f_3),

    %% yearly
    autotest:mark(?LINE, "byxxx_match/2 - 8g"),
    true = ts_duration:valid_duration(BMTimeSwitchCond8g),
    BMCurrentDateTime8g_1 = #date_time{date = {2004,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8g, BMCurrentDateTime8g_1),
    BMCurrentDateTime8g_2 = #date_time{date = {2006,1,1}, time = {0,0,0}, type = floating},
    true = interpret_time:byxxx_match(BMTimeSwitchCond8g, BMCurrentDateTime8g_2),
    BMCurrentDateTime8g_3 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
    false = interpret_time:byxxx_match(BMTimeSwitchCond8g, BMCurrentDateTime8g_3),

    ok.

test19_9() ->
    %%----------------------
    %% test various durations
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,2}, time = {0,0,0}, type = floating},
      dtend_duration = dummy,
      freq = dummy,
      interval = 2,
      until_count = repeat_forever,
      by_values = dummy,
      wkst = mo
     },

    %% XXX dtstart and byxxx missmatch - fix

    TimeSwitchCond1 = TimeSwitchCond#time_switch__cond_7{freq = daily,
							 by_values = [{byday, {-1,fr}}],
							 dtend_duration = {duration, #duration{seconds = 10}}
							},
    TimeSwitchCond2 = TimeSwitchCond#time_switch__cond_7{freq = daily,
							 by_values = [{byday, {-1,fr}}],
							 dtend_duration = {duration, #duration{minutes = 10}}
							},
    TimeSwitchCond3 = TimeSwitchCond#time_switch__cond_7{freq = daily,
							 by_values = [{byday, {-1,fr}}],
							 dtend_duration = {duration, #duration{hours = 10}}
							},
    TimeSwitchCond4 = TimeSwitchCond#time_switch__cond_7{freq = weekly,
							 by_values = [{bymonth, 3}],
							 dtend_duration = {duration, #duration{days = 5}}
							},
    TimeSwitchCond5 = TimeSwitchCond#time_switch__cond_7{freq = monthly,
							 by_values = [{bymonth, 3}],
							 dtend_duration = {duration, #duration{weeks = 3}}
							},

    %%
    autotest:mark(?LINE, "byxxx_match/2 - 9.1"),
    true = ts_duration:valid_duration(TimeSwitchCond1),
    CurrentDateTime1 = #date_time{date = {2005,1,28}, time = {0,0,9}, type = floating},
    true = interpret_time:byxxx_match(TimeSwitchCond1, CurrentDateTime1),
    %%
    autotest:mark(?LINE, "byxxx_match/2 - 9.2"),
    true = ts_duration:valid_duration(TimeSwitchCond2),
    CurrentDateTime2 = #date_time{date = {2005,1,28}, time = {0,9,59}, type = floating},
    true = interpret_time:byxxx_match(TimeSwitchCond2, CurrentDateTime2),
    %%
    autotest:mark(?LINE, "byxxx_match/2 - 9.3"),
    true = ts_duration:valid_duration(TimeSwitchCond3),
    CurrentDateTime3 = #date_time{date = {2005,1,28}, time = {9,59,59}, type = floating},
    true = interpret_time:byxxx_match(TimeSwitchCond3, CurrentDateTime3),

    %% 2005-03-13 is start of current interval
    autotest:mark(?LINE, "byxxx_match/2 - 9.4"),
    true = ts_duration:valid_duration(TimeSwitchCond4),
    CurrentDateTime4 = #date_time{date = {2005,3,17}, time = {23,59,59}, type = floating},
    true = interpret_time:byxxx_match(TimeSwitchCond4, CurrentDateTime4),

    %% 2005-03-02 is start of current interval
    autotest:mark(?LINE, "byxxx_match/2 - 9.5"),
    true = ts_duration:valid_duration(TimeSwitchCond5),
    CurrentDateTime5 = #date_time{date = {2005,3,22}, time = {23,59,59}, type = floating},
    true = interpret_time:byxxx_match(TimeSwitchCond5, CurrentDateTime5),

    ok.


%% test with dtend and until as utc based date-time values
%% also acts as a test of in_time_range_7(Current, Timezone, TimeSwitchCond)
%%--------------------------------------------------------------------
test20() ->
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2004,1,1}, time = {cpl_test_util:timezone_offset() + 0,0,0}, type = floating},
      %% duration = 1 hour + 1 second
      dtend_duration = {dtend, #date_time{date = {2004,1,1}, time = {1,0,0}, type = utc}},
      freq = daily,
      interval = 2,
      until_count = {until, #date_time{date = {2004,1,5}, time = {0,30,0}, type = utc}},
      by_values = [{bymonth, 1}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),
    Timezone = dummy,

    %% first match (at dtstart)
    autotest:mark(?LINE, "in_time_range_test/2 - 1a"),
    Current1 = ts_datetime:datetime_to_usec(start, Timezone,
					    #date_time{date = {2004,1,1},
						       time = {cpl_test_util:timezone_offset(),0,0},
						       type = floating}),
    true = interpret_time:in_time_range_test(Timezone, TimeSwitchCond, Current1),

    %% last matching second
    autotest:mark(?LINE, "in_time_range_test/2 - 1b"),
    Current2 = ts_datetime:datetime_to_usec(start, Timezone,
					    #date_time{date = {2004,1,3},
						       time = {cpl_test_util:timezone_offset() + 1,0,0},
						       type = floating}),
    true = interpret_time:in_time_range_test(Timezone, TimeSwitchCond, Current2),

    %% first second after a interval
    autotest:mark(?LINE, "in_time_range_test/2 - 1c"),
    Current3 = ts_datetime:datetime_to_usec(start, Timezone,
					    #date_time{date = {2004,1,3},
						       time = {cpl_test_util:timezone_offset() + 1,0,1},
						       type = floating}),
    false = interpret_time:in_time_range_test(Timezone, TimeSwitchCond, Current3),

    %% first second after "until"
    autotest:mark(?LINE, "in_time_range_test/2 - 1d"),
    Current4 = ts_datetime:datetime_to_usec(start, Timezone,
					    #date_time{date = {2004,1,5},
						       time = {cpl_test_util:timezone_offset() + 0,30,1},
						       type = floating}),
    false = interpret_time:in_time_range_test(Timezone, TimeSwitchCond, Current4),


    ok.


%% get_count_ranges_7/2
%%--------------------------------------------------------------------
%% tests that generate_counts(yearly = Level, ...) can check a
%% infinite series of years - if insufficient number of matches are
%% found in the currently processed years
test21_1() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 1"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,2}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{days = 1}},
      freq = monthly,
      interval = 2,
      until_count = {count, 3},
      by_values = [{bymonth, 1}, {bymonth, 3}, {bymonth, 4}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,0,0}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,2}, time = {23,59,59}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,3,2}, time = {0,0,0}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,3,2}, time = {23,59,59}, type = floating}),
    S3 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2006,1,2}, time = {0,0,0}, type = floating}),
    E3 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2006,1,2}, time = {23,59,59}, type = floating}),
    [{S1,E1}, {S2,E2}, {S3,E3}] = Res,

    ok.

%% test bysecond handling (that all levels are traversed properly)
test21_2() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 2"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,2}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = monthly,
      interval = 2,
      until_count = {count, 4},
      by_values = [{bysecond, 1}, {bysecond, 3}, {bysecond, 4}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,0,1}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,0,1}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,0,3}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,0,3}, type = floating}),
    S3 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,0,4}, type = floating}),
    E3 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,0,4}, type = floating}),
    S4 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,1,1}, type = floating}),
    E4 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,2}, time = {0,1,1}, type = floating}),
    [{S1,E1}, {S2,E2}, {S3,E3}, {S4,E4}] = Res,

    ok.

%% --------------- Test remaining byxxx parameters (filters) byweekno - byminute
%% byweekno
test21_3() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 3"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,2}, time = {12,12,12}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = daily,
      interval = 7,
      until_count = {count, 2},
      by_values = [{byweekno, 2}, {byweekno, -2}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,16}, time = {12,12,12}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,16}, time = {12,12,12}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,12,25}, time = {12,12,12}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,12,25}, time = {12,12,12}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% byyearday  - also tests handling of the case where Time in get_new_times(Level, TimeSwitchCond, Time)
%%              may have start points < dtstart
test21_4() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 4"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,2}, time = {12,12,12}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = daily,
      interval = 1,
      until_count = {count, 2},
      by_values = [{byyearday, 2}, {byyearday, -2}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,2}, time = {12,12,12}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,2}, time = {12,12,12}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,12,30}, time = {12,12,12}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,12,30}, time = {12,12,12}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% bymonthday
test21_5() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 5"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,2}, time = {12,12,12}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = daily,
      interval = 1,
      until_count = {count, 2},
      by_values = [{bymonthday, 2}, {bymonthday, -2}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,2}, time = {12,12,12}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,2}, time = {12,12,12}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,1,30}, time = {12,12,12}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,1,30}, time = {12,12,12}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% byday
test21_6() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 6"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,9,10}, time = {12,12,12}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = daily,
      interval = 1,
      until_count = {count, 4},
      %% 2nd mo = 12, -2nd fr = 23, we = 14, 21, 28
      by_values = [{byday, {2, mo}}, {byday, {-2, fr}}, {byday, {all, we}}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,9,12}, time = {12,12,12}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,9,12}, time = {12,12,12}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,9,14}, time = {12,12,12}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,9,14}, time = {12,12,12}, type = floating}),
    S3 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,9,21}, time = {12,12,12}, type = floating}),
    E3 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,9,21}, time = {12,12,12}, type = floating}),
    S4 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,9,23}, time = {12,12,12}, type = floating}),
    E4 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,9,23}, time = {12,12,12}, type = floating}),
    [{S1,E1}, {S2,E2}, {S3,E3}, {S4,E4}] = Res,

    ok.

%% byhour - also test month overlap
test21_7() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 7"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,4,30}, time = {13,12,12}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = daily,
      interval = 3,
      until_count = {count, 2},
      by_values = [{byhour, 20}, {byhour, 12}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,4,30}, time = {20,12,12}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,4,30}, time = {20,12,12}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2005,5,3}, time = {12,12,12}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
				      #date_time{date = {2005,5,3}, time = {12,12,12}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.


%% byminute - start points start in different month than dtstart
test21_8() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 8"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,4,30}, time = {23,21,1}, type = floating},
      dtend_duration = {duration, #duration{seconds = 1}},
      freq = daily,
      interval = 3,
      until_count = {count, 2},
      by_values = [{byminute, 20}, {byminute, 12}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
 				      #date_time{date = {2005,5,3}, time = {0,12,1}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
 				      #date_time{date = {2005,5,3}, time = {0,12,1}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
 				      #date_time{date = {2005,5,3}, time = {0,20,1}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
 				      #date_time{date = {2005,5,3}, time = {0,20,1}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% --------------- test generators (freq) values
%% yearly
test21_9() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 9"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2003,4,28}, time = {22,12,1}, type = floating},
      dtend_duration = {duration, #duration{weeks = 15}},
      freq = yearly,
      interval = 3,
      until_count = {count, 2},
      by_values = [{bymonth, 2}, {bymonth, 10}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2003,10,28}, time = {22,12,1}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2004,2,10}, time = {22,12,0}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
  				      #date_time{date = {2006,2,28}, time = {22,12,1}, type = floating}),
    %% transition to DST occurred, wall clock time is 1 hour ahead of real (non-DST) time
    %% so
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2006,6,13}, time = {22,12,0}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% monthly - also test year end overlap
test21_10() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 10"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2003,12,8}, time = {22,12,1}, type = floating},
      dtend_duration = {duration, #duration{hours = 15}},
      freq = monthly,
      interval = 3,
      until_count = {count, 2},
      by_values = [{bymonthday, 2}, {bymonthday, 10}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2003,12,10}, time = {22,12,1}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2003,12,11}, time = {13,12,0}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
  				      #date_time{date = {2004,3,2}, time = {22,12,1}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2004,3,3}, time = {13,12,0}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% weekly - use several kinds of byxxx parameters
test21_11() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 11a"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2003,12,9}, time = {22,12,1}, type = floating},
      dtend_duration = {duration, #duration{hours = 15}},
      freq = weekly,
      interval = 3,
      until_count = {count, 2},
      by_values = [{byday, {2, tu}}, {byday, {-1, tu}}, {bymonthday, 9}, {bymonthday, 30}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2003,12,9}, time = {22,12,1}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2003,12,10}, time = {13,12,0}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
  				      #date_time{date = {2003,12,30}, time = {22,12,1}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2003,12,31}, time = {13,12,0}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    %% daily test (with interval = 3 * 7)
    autotest:mark(?LINE, "get_count_ranges_7/2 - 11b"),
    TimeSwitchCond2 = TimeSwitchCond#time_switch__cond_7{freq = daily, interval = 21},
    true = ts_duration:valid_duration(TimeSwitchCond2),

    Res2 = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond2),
    [{S1,E1}, {S2,E2}] = Res2,

    ok.

%% daily - use several kinds of byxxx parameters
test21_12() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 12"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2003,12,9}, time = {22,12,1}, type = floating},
      dtend_duration = {duration, #duration{hours = 15}},
      freq = daily,
      interval = 3,
      until_count = {count, 2},
      by_values = [{byday, {2, tu}}, {byday, {-3, mo}}, {bymonthday, 9}, {bymonthday, 15}, {byhour, 22},
		  {byyearday, 343}, {byyearday, 349}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2003,12,9}, time = {22,12,1}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2003,12,10}, time = {13,12,0}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
  				      #date_time{date = {2003,12,15}, time = {22,12,1}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2003,12,16}, time = {13,12,0}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% hourly
test21_13() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 13"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2004,12,31}, time = {22,12,1}, type = floating},
      dtend_duration = {duration, #duration{hours = 2}},
      freq = hourly,
      interval = 3,
      until_count = {count, 2},
      by_values = [{bymonthday, 31}, {bymonthday, 5}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2004,12,31}, time = {22,12,1}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2005,1,1}, time = {0,12,0}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
  				      #date_time{date = {2005,1,5}, time = {1,12,1}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2005,1,5}, time = {3,12,0}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% minutely
test21_14() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 14"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2004,12,31}, time = {23,59,1}, type = floating},
      dtend_duration = {duration, #duration{minutes = 2}},
      freq = minutely,
      interval = 3,
      until_count = {count, 2},
      by_values = [{bymonthday, 31}, {bymonthday, 5}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2004,12,31}, time = {23,59,1}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2005,1,1}, time = {0,1,0}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
  				      #date_time{date = {2005,1,5}, time = {0,2,1}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2005,1,5}, time = {0,4,0}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% secondly
test21_15() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 14"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2004,12,31}, time = {23,59,59}, type = floating},
      dtend_duration = {duration, #duration{seconds = 2}},
      freq = secondly,
      interval = 3,
      until_count = {count, 2},
      by_values = [{bymonthday, 31}, {bymonthday, 5}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    Res = interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond),
    %% io:format("Res = ~p~n", [Res]),
    S1 = ts_datetime:datetime_to_usec(start, Timezone,
				      #date_time{date = {2004,12,31}, time = {23,59,59}, type = floating}),
    E1 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating}),
    S2 = ts_datetime:datetime_to_usec(start, Timezone,
  				      #date_time{date = {2005,1,5}, time = {0,0,2}, type = floating}),
    E2 = ts_datetime:datetime_to_usec(stop, Timezone,
  				      #date_time{date = {2005,1,5}, time = {0,0,3}, type = floating}),
    [{S1,E1}, {S2,E2}] = Res,

    ok.

%% verify count start point search, is limited by 'time_switch_count_max_lookahead' (20 years)
test21_16() ->
    autotest:mark(?LINE, "get_count_ranges_7/2 - 14"),
    Timezone = dummy,
    TimeSwitchCond = #time_switch__cond_7{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = daily,
      interval = 1,
      until_count = {count, 6},
      by_values = [{bymonth, 2}, {bymonthday, 29}],
      wkst = mo
     },
    true = ts_duration:valid_duration(TimeSwitchCond),

    %% leap years in range [2005,2025] are; 2008, 2012, 2016, 2020 and 2024 so there can't be 6 matches
    autotest:fail(fun() -> interpret_time:get_count_ranges_7(Timezone, TimeSwitchCond) end),

    ok.

%%--------------------------------------------------------------------
%% test bysetpos
%%--------------------------------------------------------------------

%% get_set/2
%% as generate_counts(...) should handle time_switch__cond_8 properly
%% there should be no need to test exhaustively with different freq
%% byxxx parameters
%%--------------------------------------------------------------------
%% test monthly generation
test22a() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonthday, 2}, {bymonthday, 4}, {bymonthday, 8}, {bymonthday, 16}, {bymonthday, 31}],
      wkst = mo,
      bysetpos = [1,-1]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    %% date 2005-01-dd hh:mm:ss
    autotest:mark(?LINE, "get_set/2 - 1"),
    DT1 = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
    Res1 = interpret_time:get_set(DT1, TimeSwitchCond),
    [#date_time{date = {2005,1,2}, time = {0,0,0}, type = floating},
     #date_time{date = {2005,1,4}, time = {0,0,0}, type = floating},
     #date_time{date = {2005,1,8}, time = {0,0,0}, type = floating},
     #date_time{date = {2005,1,16}, time = {0,0,0}, type = floating},
     #date_time{date = {2005,1,31}, time = {0,0,0}, type = floating}
    ] = Res1,

    %% date 2005-11-dd hh:mm:ss (no monthday = 31)
    autotest:mark(?LINE, "get_set/2 - 2"),
    DT2 = #date_time{date = {2005,11,1}, time = {0,0,0}, type = floating},
    Res2 = interpret_time:get_set(DT2, TimeSwitchCond),
    [#date_time{date = {2005,11,2}, time = {0,0,0}, type = floating},
     #date_time{date = {2005,11,4}, time = {0,0,0}, type = floating},
     #date_time{date = {2005,11,8}, time = {0,0,0}, type = floating},
     #date_time{date = {2005,11,16}, time = {0,0,0}, type = floating}
    ] = Res2,

    ok.

%% test monthly generation
%% test use of dtstart to initiate start point time units
%% test with the most complex byxxx parameter byday
test22b() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,1}, time = {5,5,5}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday, {1, mo}}, {byday, {all, fr}}, {byday, {-2, we}}],
      wkst = mo,
      bysetpos = [1,-1]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    %% date 2005-04-dd hh:mm:ss
    autotest:mark(?LINE, "get_set/2 - 3"),
    DT1 = #date_time{date = {2005,4,23}, time = {10,10,10}, type = floating},
    Res1 = interpret_time:get_set(DT1, TimeSwitchCond),
    M1 = lists:sort([#date_time{date = {2005,4,4}, time = {5,5,5}, type = floating},  % mo
		     #date_time{date = {2005,4,1}, time = {5,5,5}, type = floating},  % fr
		     #date_time{date = {2005,4,8}, time = {5,5,5}, type = floating},  % fr
		     #date_time{date = {2005,4,15}, time = {5,5,5}, type = floating}, % fr
		     #date_time{date = {2005,4,22}, time = {5,5,5}, type = floating}, % fr
		     #date_time{date = {2005,4,29}, time = {5,5,5}, type = floating}, % fr
		     #date_time{date = {2005,4,20}, time = {5,5,5}, type = floating}  % we
    ]),
    M1 = lists:sort(Res1),

    ok.

%% check yearly generation
test22c() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,1}, time = {5,5,5}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = yearly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonthday, 31}],
      wkst = mo,
      bysetpos = [1,-1]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    %% date 2005-04-dd hh:mm:ss
    autotest:mark(?LINE, "get_set/2 - 4"),
    DT1 = #date_time{date = {2005,4,23}, time = {10,10,10}, type = floating},
    Res1 = interpret_time:get_set(DT1, TimeSwitchCond),
    M1 = lists:sort([#date_time{date = {2005,1,31}, time = {5,5,5}, type = floating},
		     #date_time{date = {2005,3,31}, time = {5,5,5}, type = floating},
		     #date_time{date = {2005,5,31}, time = {5,5,5}, type = floating},
		     #date_time{date = {2005,7,31}, time = {5,5,5}, type = floating},
		     #date_time{date = {2005,8,31}, time = {5,5,5}, type = floating},
		     #date_time{date = {2005,10,31}, time = {5,5,5}, type = floating},
		     #date_time{date = {2005,12,31}, time = {5,5,5}, type = floating}
    ]),
    M1 = lists:sort(Res1),

    ok.

%% check that byxxx parameters > freq are ignored
test22d() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,1}, time = {5,5,5}, type = floating},
      dtend_duration = {duration, #duration{minutes = 8}},
      freq = minutely,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonth, 12}, {byweekno, 52}, {byhour, 2}, % skip these
		   {byminute, 5}, {byminute, 55}],
      wkst = mo,
      bysetpos = [1,-1]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    %% date 2005-04-23 10:05:ss - this use verifies that bymonth, byweekno and byhour are ignored
    autotest:mark(?LINE, "get_set/2 - 5"),
    DT1 = #date_time{date = {2005,4,23}, time = {10,5,10}, type = floating},
    Res1 = interpret_time:get_set(DT1, TimeSwitchCond),
    M1 = lists:sort([
		     #date_time{date = {2005,4,23}, time = {10,5,5}, type = floating}
		    ]),
    M1 = lists:sort(Res1),

    %% date 2005-04-23 10:55:ss - this use verifies that bymonth, byweekno and byhour are ignored
    autotest:mark(?LINE, "get_set/2 - 6"),
    DT2 = #date_time{date = {2005,4,23}, time = {10,55,10}, type = floating},
    Res2 = interpret_time:get_set(DT2, TimeSwitchCond),
    M2 = lists:sort([
		     #date_time{date = {2005,4,23}, time = {10,55,5}, type = floating}
		    ]),
    M2 = lists:sort(Res2),

    ok.

%% test freq = secondly, border case test
%% test freq is lowest level
test22e() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,1}, time = {5,5,5}, type = floating},
      dtend_duration = {duration, #duration{minutes = 8}},
      freq = secondly,
      interval = 1,
      until_count = repeat_forever,
      by_values = dummy,
      wkst = mo,
      bysetpos = [1,-1]
     },

    %% bysecond lowest level, return date 2005-04-23 10:05:42 - second supplied by bysecond
    autotest:mark(?LINE, "get_set/2 - 7"),
    DT1 = #date_time{date = {2005,4,23}, time = {10,5,42}, type = floating},
    TimeSwitchCond1 = TimeSwitchCond#time_switch__cond_8{
			by_values = [{bymonth, 12}, {byweekno, 52}, {byhour, 2}, % skip these
				     {byminute, 5}, {byminute, 55},              % skip these
				     {bysecond, 42}]
		       },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond1),
    Res1 = interpret_time:get_set(DT1, TimeSwitchCond1),
    M1 = lists:sort([#date_time{date = {2005,4,23}, time = {10,5,42}, type = floating}]),
    M1 = lists:sort(Res1),

    %% bysecond lowest level, DT supplies other second than expected by bysecond filter
    autotest:mark(?LINE, "get_set/2 - 8"),
    DT2 = #date_time{date = {2005,4,23}, time = {10,5,43}, type = floating},
    TimeSwitchCond2 = TimeSwitchCond#time_switch__cond_8{
			by_values = [{bymonth, 12}, {byweekno, 52}, {byhour, 2}, % skip these
				     {byminute, 5}, {byminute, 55},              % skip these
				     {bysecond, 42}]
		       },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond2),
    Res2 = interpret_time:get_set(DT2, TimeSwitchCond2),
    M2 = lists:sort([]),
    M2 = lists:sort(Res2),

    %% freq = lowest level, return 2005-04-23 10:05:59 - even seconds are supplied by DT
    autotest:mark(?LINE, "get_set/2 - 9"),
    DT3 = #date_time{date = {2005,4,23}, time = {10,5,59}, type = floating},
    TimeSwitchCond3 = TimeSwitchCond#time_switch__cond_8{
			by_values = [{bymonth, 12}, {byweekno, 52}, {byhour, 2}, % skip these
				     {byminute, 5}, {byminute, 55}              % skip these
				    ]
		       },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond3),
    Res3 = interpret_time:get_set(DT3, TimeSwitchCond3),
    M3 = lists:sort([#date_time{date = {2005,4,23}, time = {10,5,59}, type = floating}]),
    M3 = lists:sort(Res3),

    ok.

%% test dtstart handling
test22f() ->
   TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,15}, time = {5,5,5}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday, {1, mo}}, {byday, {all, fr}}, {byday, {-2, we}}],
      wkst = mo,
      bysetpos = [1,-1]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    %% date 2005-01-dd hh:mm:ss, skips 2005-01-03 (mo), 01-07 (fr) and 01-14 (fr) as they come before dtstart
    autotest:mark(?LINE, "get_set/2 - 10"),
    DT1 = #date_time{date = {2005,1,23}, time = {10,10,10}, type = floating},
    Res1 = interpret_time:get_set(DT1, TimeSwitchCond),
    M1 = lists:sort([#date_time{date = {2005,1,21}, time = {5,5,5}, type = floating}, % fr
		     #date_time{date = {2005,1,28}, time = {5,5,5}, type = floating}, % fr
		     #date_time{date = {2005,1,19}, time = {5,5,5}, type = floating}  % we
    ]),
    M1 = lists:sort(Res1),

    ok.


%% nth/2
%%--------------------------------------------------------------------
test23() ->
    %% empty list
    autotest:mark(?LINE, "nth/2 - 1"),
    '#not_found' = interpret_time:nth([], 5),
    %% N to big
    autotest:mark(?LINE, "nth/2 - 2"),
    '#not_found' = interpret_time:nth([1,2,3,4], 5),
    %% N = last element
    autotest:mark(?LINE, "nth/2 - 3"),
    5 = interpret_time:nth([1,2,3,4,5], 5),
    %% -N, get first element
    autotest:mark(?LINE, "nth/2 - 4"),
    1 = interpret_time:nth([1,2,3,4,5], -5),
    %% get from middle of list
    autotest:mark(?LINE, "nth/2 - 5"),
    2 = interpret_time:nth([1,2,3,4],2),
    3 = interpret_time:nth([1,2,3,4],-2),
    %% -N, to big
    autotest:mark(?LINE, "nth/2 - 6"),
    '#not_found' = interpret_time:nth([1,2,3,4,5], -6),

    ok.

%% get_bysetpos/2
%%--------------------------------------------------------------------
test24a() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = dummy,
      wkst = mo,
      bysetpos = dummy
     },

    %% get first and last
    autotest:mark(?LINE, "get_bysetpos/2 - 1"),
    TimeSwitchCond1 = TimeSwitchCond#time_switch__cond_8{
			by_values = [{bymonthday, 2}, {bymonthday, 4}, {bymonthday, 8},
				     {bymonthday, 16}, {bymonthday, 31}],
			bysetpos = [1,-1]},
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond1),
    Time1 = #date_time{date = {2005,1,2}, time = {7,59,59}, type = floating},
    Res1 = [#date_time{date = {2005,1,2}, time = {0,0,0}, type = floating},
	    #date_time{date = {2005,1,31}, time = {0,0,0}, type = floating}],
    Res1 = interpret_time:get_bysetpos(TimeSwitchCond1, Time1),

    %% both bysetpos refer to the same date-time
    autotest:mark(?LINE, "get_bysetpos/2 - 2"),
    TimeSwitchCond2 = TimeSwitchCond#time_switch__cond_8{
			by_values = [{bymonthday, 2}, {bymonthday, 4},
				     {bymonthday, 16}, {bymonthday, 31}],
			bysetpos = [2,-3]},
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond2),
    Time2 = #date_time{date = {2005,1,2}, time = {7,59,59}, type = floating},
    Res2 = [#date_time{date = {2005,1,4}, time = {0,0,0}, type = floating}],
    Res2 = interpret_time:get_bysetpos(TimeSwitchCond2, Time2),

    %% two bysetpos refer to the same date-time and two refer to non-existing bysetpos indexes (5,-5)
    autotest:mark(?LINE, "get_bysetpos/2 - 3"),
    TimeSwitchCond3 = TimeSwitchCond#time_switch__cond_8{
			by_values = [{bymonthday, 2}, {bymonthday, 4},
				     {bymonthday, 16}, {bymonthday, 31}],
			bysetpos = [2,-3,5,-5]},
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond3),
    Time3 = #date_time{date = {2005,1,2}, time = {7,59,59}, type = floating},
    Res3 = [#date_time{date = {2005,1,4}, time = {0,0,0}, type = floating}],
    Res3 = interpret_time:get_bysetpos(TimeSwitchCond3, Time3),

    ok.

%% dedug test: runs full_step(weekly, Diff, Interval)
test24b() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,7}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = weekly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{byday, {all, fr}}],
      wkst = mo,
      bysetpos = [1,-1]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    autotest:mark(?LINE, "get_bysetpos/2 - 4"),
    Time1 = #date_time{date = {2005,5,13}, time = {11,11,11}, type = floating},
    Res1 = [#date_time{date = {2005,5,13}, time = {0,0,0}, type = floating}],
    Res1 = interpret_time:get_bysetpos(TimeSwitchCond, Time1),

    autotest:mark(?LINE, "get_bysetpos/2 - 5"),
    Time2 = #date_time{date = {2005,5,20}, time = {11,11,11}, type = floating},
    Res2 = [#date_time{date = {2005,5,20}, time = {0,0,0}, type = floating}],
    Res2 = interpret_time:get_bysetpos(TimeSwitchCond, Time2),

    ok.

%% is_start_time_valid_bysetpos/2
%%--------------------------------------------------------------------
test25() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,1}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = monthly,
      interval = 1,
      until_count = repeat_forever,
      by_values = [{bymonthday, 2}, {bymonthday, 4}, {bymonthday, 8},
		   {bymonthday, 16}, {bymonthday, 31}],
      wkst = mo,
      bysetpos = [1,-2,-1]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    %% N = 1 / -5
    autotest:mark(?LINE, "is_start_time_valid_bysetpos/2 - 1"),
    StartTime1 = #date_time{date = {2005,5,2}, time = {0,0,0}, type = floating},
    true = interpret_time:is_start_time_valid_bysetpos(TimeSwitchCond, StartTime1),

    %% N = 2 / -4, fail
    autotest:mark(?LINE, "is_start_time_valid_bysetpos/2 - 2"),
    StartTime2 = #date_time{date = {2005,5,4}, time = {0,0,0}, type = floating},
    false = interpret_time:is_start_time_valid_bysetpos(TimeSwitchCond, StartTime2),

    %% N = 3 / -3, fail
    autotest:mark(?LINE, "is_start_time_valid_bysetpos/2 - 3"),
    StartTime3 = #date_time{date = {2005,5,8}, time = {0,0,0}, type = floating},
    false = interpret_time:is_start_time_valid_bysetpos(TimeSwitchCond, StartTime3),

    %% N = 4 / -2
    autotest:mark(?LINE, "is_start_time_valid_bysetpos/2 - 4"),
    StartTime4 = #date_time{date = {2005,5,16}, time = {0,0,0}, type = floating},
    true = interpret_time:is_start_time_valid_bysetpos(TimeSwitchCond, StartTime4),

    %% = 5 / -1
    autotest:mark(?LINE, "is_start_time_valid_bysetpos/2 - 5"),
    StartTime5 = #date_time{date = {2005,5,31}, time = {0,0,0}, type = floating},
    true = interpret_time:is_start_time_valid_bysetpos(TimeSwitchCond, StartTime5),

    %% a few invalid month days - not part of bymonthday
    autotest:mark(?LINE, "is_start_time_valid_bysetpos/2 - 6"),
    StartTime6 = #date_time{date = {2005,5,30}, time = {0,0,0}, type = floating},
    false = interpret_time:is_start_time_valid_bysetpos(TimeSwitchCond, StartTime6),

    autotest:mark(?LINE, "is_start_time_valid_bysetpos/2 - 7"),
    StartTime7 = #date_time{date = {2005,5,1}, time = {0,0,0}, type = floating},
    false = interpret_time:is_start_time_valid_bysetpos(TimeSwitchCond, StartTime7),

    autotest:mark(?LINE, "is_start_time_valid_bysetpos/2 - 8"),
    StartTime8 = #date_time{date = {2005,5,10}, time = {0,0,0}, type = floating},
    false = interpret_time:is_start_time_valid_bysetpos(TimeSwitchCond, StartTime8),

    ok.


%% is_current_time_valid_bysetpos/2
%%--------------------------------------------------------------------
test26a() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,7}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = monthly,
      interval = 7,
      until_count = repeat_forever,
      by_values = [{byday, {all, fr}}],
      wkst = mo,
      bysetpos = [2,-2]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    Timezone = dummy,

    %% N = 2 / -3, first valid second
    autotest:mark(?LINE, "is_current_time_valid_bysetpos/2 - 1"),
    C1 = #date_time{date = {2005,5,13}, time = {0,0,0}, type = floating},
    Current1 = ts_datetime:datetime_to_usec(start, Timezone, C1),
    true = interpret_time:is_current_time_valid_bysetpos(TimeSwitchCond, Current1),

    %% N = 3 / -2, last valid second
    autotest:mark(?LINE, "is_current_time_valid_bysetpos/2 - 2"),
    C2 = #date_time{date = {2005,5,20}, time = {7,59,59}, type = floating},
    Current2 = ts_datetime:datetime_to_usec(start, Timezone, C2),
    true = interpret_time:is_current_time_valid_bysetpos(TimeSwitchCond, Current2),

    %% N = 2 / -3, the second after the valid time range
    autotest:mark(?LINE, "is_current_time_valid_bysetpos/2 - 3"),
    C3 = #date_time{date = {2005,5,13}, time = {8,0,0}, type = floating},
    Current3 = ts_datetime:datetime_to_usec(start, Timezone, C3),
    false = interpret_time:is_current_time_valid_bysetpos(TimeSwitchCond, Current3),

    ok.

%% test freq = weekly
%% 2005-05-13 and 2005-05-27 are valid bysetpos dates in 2005-05
test26b() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,7}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = weekly,
      interval = 2,
      until_count = repeat_forever,
      by_values = [{byday, {all, fr}}],
      wkst = mo,
      bysetpos = [1,-1]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    Timezone = dummy,

    %% N = 2 / -1
    autotest:mark(?LINE, "is_current_time_valid_bysetpos/2 - 4"),
    C1 = #date_time{date = {2005,5,27}, time = {0,0,0}, type = floating},
    Current1 = ts_datetime:datetime_to_usec(start, Timezone, C1),
    true = interpret_time:is_current_time_valid_bysetpos(TimeSwitchCond, Current1),

    %% N = 1 / -2
    autotest:mark(?LINE, "is_current_time_valid_bysetpos/2 - 5"),
    C2 = #date_time{date = {2005,5,13}, time = {7,59,59}, type = floating},
    Current2 = ts_datetime:datetime_to_usec(start, Timezone, C2),
    true = interpret_time:is_current_time_valid_bysetpos(TimeSwitchCond, Current2),

    ok.


%% in_time_range(Timezone, TimeSwitchCond, Current)
%% when TimeSwitchCond = time_switch__cond_8 record()
%%--------------------------------------------------------------------
test27() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,1,9}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = monthly,
      interval = 2,
      until_count = repeat_forever,
      by_values = [{byday, {all, su}}],
      wkst = mo,
      bysetpos = [2,-2]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),

    Timezone = dummy,

    %% 2005-05-dd, dd = 1 | 8 | 15 | 22 | 29

    autotest:mark(?LINE, "in_time_range/3 - 1"),
    C1 = #date_time{date = {2005,5,8}, time = {0,0,0}, type = floating},
    Current1 = ts_datetime:datetime_to_usec(start, Timezone, C1),
    true = interpret_time:in_time_range(Timezone, TimeSwitchCond, Current1),

    autotest:mark(?LINE, "in_time_range/3 - 2"),
    C2 = #date_time{date = {2005,5,22}, time = {7,59,59}, type = floating},
    Current2 = ts_datetime:datetime_to_usec(start, Timezone, C2),
    true = interpret_time:in_time_range(Timezone, TimeSwitchCond, Current2),

    autotest:mark(?LINE, "in_time_range/3 - 3"),
    %% valid sunday not part of bysetpos selected set
    C3 = #date_time{date = {2005,5,23}, time = {6,0,0}, type = floating},
    Current3 = ts_datetime:datetime_to_usec(start, Timezone, C3),
    false = interpret_time:in_time_range(Timezone, TimeSwitchCond, Current3),

    autotest:mark(?LINE, "in_time_range/3 - 4"),
    %% day not part of set defined by byxxx, freq and interval rules
    C4 = #date_time{date = {2005,5,7}, time = {6,0,0}, type = floating},
    Current4 = ts_datetime:datetime_to_usec(start, Timezone, C4),
    false = interpret_time:in_time_range(Timezone, TimeSwitchCond, Current4),

    ok.

%% get_count_ranges_8(Timezone, TimeSwitchCond)
%% when TimeSwitchCond = time_switch__cond_8 record()
%%--------------------------------------------------------------------
test28() ->
    TimeSwitchCond = #time_switch__cond_8{
      dtstart = #date_time{date = {2005,4,15}, time = {0,0,0}, type = floating},
      dtend_duration = {duration, #duration{hours = 8}},
      freq = monthly,
      interval = 1,
      until_count = {count, 3},
      by_values = [{byday, {all, mo}}, {byday, {all, tu}}, {byday, {all, we}},
		   {byday, {all, th}}, {byday, {all, fr}}],
      wkst = mo,
      bysetpos = [-3]
     },
    true = interpret_time:is_bysetpos_usable(TimeSwitchCond),
    Timezone = dummy,

    autotest:mark(?LINE, "get_count_ranges_8/2 - 1"),
    %% get 3:rd last workday from end of month each month
    C1 = #date_time{date = {2005,4,27}, time = {0,0,0}, type = floating},
    C2 = #date_time{date = {2005,4,27}, time = {7,59,59}, type = floating},
    C3 = #date_time{date = {2005,5,27}, time = {0,0,0}, type = floating},
    C4 = #date_time{date = {2005,5,27}, time = {7,59,59}, type = floating},
    C5 = #date_time{date = {2005,6,28}, time = {0,0,0}, type = floating},
    C6 = #date_time{date = {2005,6,28}, time = {7,59,59}, type = floating},

    Ranges = [
	      {ts_datetime:datetime_to_usec(start, Timezone, C1),
	       ts_datetime:datetime_to_usec(stop, Timezone, C2)},
	      {ts_datetime:datetime_to_usec(start, Timezone, C3),
	       ts_datetime:datetime_to_usec(stop, Timezone, C4)},
	      {ts_datetime:datetime_to_usec(start, Timezone, C5),
	       ts_datetime:datetime_to_usec(stop, Timezone, C6)}
	     ],

    Res = interpret_time:get_count_ranges_8(Timezone, TimeSwitchCond),
    Ranges = Res,

    ok.




