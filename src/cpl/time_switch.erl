%% This module contains operations to access the various kinds of 
%% time_switch__cond_N (N = 2 | 5 | 7 | 8) record fields in a 
%% convenient manner - the records could be view as a chain of 
%% inherited classes, from N = 2 (as a base class) to N = 8 
%% (last child in inheritance chain)
%%--------------------------------------------------------------------

-module(time_switch).

%% -behaviour().

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 get_dtstart/1,
	 get_dtend_duration/1,
	 get_freq/1,
	 get_interval/1,
	 get_until_count/1,
	 get_by_values/1,
	 get_wkst/1,
	 get_bysetpos/1,
	 get_time_ranges/1
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
%% Function: get_xxx(TimeSwitchCond) 
%% Descrip.: get field xxx in a time_switch__cond_x record()
%% Returns : TimeSwitchCond#time_switch__cond_x.xxx content
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% Function: get_dtstart(TimeSwitchCond) 
%% Descrip.: get dtstart from a time_switch__cond_{2,5,7,8} record
%% Returns : term()
%%--------------------------------------------------------------------
get_dtstart(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_2) ->
	    TimeSwitchCond#time_switch__cond_2.dtstart;
	_ when is_record(TimeSwitchCond, time_switch__cond_5) ->
	    TimeSwitchCond#time_switch__cond_5.dtstart;
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.dtstart;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.dtstart
    end.

%%--------------------------------------------------------------------
%% Function: get_dtend_duration(TimeSwitchCond) 
%% Descrip.: get dtend_duration from a time_switch__cond_{2,5,7,8} record
%% Returns : term()
%%--------------------------------------------------------------------
get_dtend_duration(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_2) ->
	    TimeSwitchCond#time_switch__cond_2.dtend_duration;
	_ when is_record(TimeSwitchCond, time_switch__cond_5) ->
	    TimeSwitchCond#time_switch__cond_5.dtend_duration;
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.dtend_duration;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.dtend_duration
    end.

%%--------------------------------------------------------------------
%% Function: get_freq(TimeSwitchCond) 
%% Descrip.: get freq from a time_switch__cond_{5,7,8} record
%% Returns : term()
%%--------------------------------------------------------------------
get_freq(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_5) ->
	    TimeSwitchCond#time_switch__cond_5.freq;
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.freq;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.freq
    end.

%%--------------------------------------------------------------------
%% Function: get_interval(TimeSwitchCond) 
%% Descrip.: get interval from a time_switch__cond_{5,7,8} record
%% Returns : term()
%%--------------------------------------------------------------------
get_interval(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_5) ->
	    TimeSwitchCond#time_switch__cond_5.interval;
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.interval;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.interval
    end.

%%--------------------------------------------------------------------
%% Function: get_until_count(TimeSwitchCond) 
%% Descrip.: get until_count from a time_switch__cond_{5,7,8} record
%% Returns : term()
%%--------------------------------------------------------------------
get_until_count(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_5) ->
	    TimeSwitchCond#time_switch__cond_5.until_count;
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.until_count;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.until_count
    end.

%%--------------------------------------------------------------------
%% Function: get_until_count(TimeSwitchCond) 
%% Descrip.: get by_values from a time_switch__cond_{7,8} record
%% Returns : term()
%%--------------------------------------------------------------------
get_by_values(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.by_values;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.by_values
    end.
	    
%%--------------------------------------------------------------------
%% Function: get_wkst(TimeSwitchCond) 
%% Descrip.: get wkst from a time_switch__cond_{7,8} record
%% Returns : term()
%%--------------------------------------------------------------------
get_wkst(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.wkst;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.wkst
    end.

%%--------------------------------------------------------------------
%% Function: get_bysetpos(TimeSwitchCond) 
%% Descrip.: get bysetpos from a time_switch__cond_8 record
%% Returns : term()
%%--------------------------------------------------------------------
get_bysetpos(TimeSwitchCond) -> 
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.bysetpos
    end.

%%--------------------------------------------------------------------
%% Function: get_time_ranges(TimeSwitchCond) 
%% Descrip.: get time_ranges from a time_switch__cond_{5,7} record
%% Returns : term()
%%--------------------------------------------------------------------
get_time_ranges(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_5) ->
	    TimeSwitchCond#time_switch__cond_5.time_ranges;
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.time_ranges;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.time_ranges
    end.

%%====================================================================
%% Behaviour functions
%%====================================================================



%%====================================================================
%% Internal functions
%%====================================================================



%%====================================================================
%% Test functions
%%====================================================================
