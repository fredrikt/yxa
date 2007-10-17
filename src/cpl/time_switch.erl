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
%% @spec    (TimeSwitchCond) ->
%%            TimeSwitchCond#time_switch__cond_x.xxx content
%%
%% @doc     get field xxx in a time_switch__cond_x record()
%% @end
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get dtstart from a time_switch__cond_{2,5,7,8} record
%% @end
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
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get dtend_duration from a time_switch__cond_{2,5,7,8}
%%          record
%% @end
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
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get freq from a time_switch__cond_{5,7,8} record
%% @end
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
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get interval from a time_switch__cond_{5,7,8} record
%% @end
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
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get until_count from a time_switch__cond_{5,7,8} record
%% @end
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
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get by_values from a time_switch__cond_{7,8} record
%% @end
%%--------------------------------------------------------------------
get_by_values(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.by_values;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.by_values
    end.

%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get wkst from a time_switch__cond_{7,8} record
%% @end
%%--------------------------------------------------------------------
get_wkst(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_7) ->
	    TimeSwitchCond#time_switch__cond_7.wkst;
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.wkst
    end.

%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get bysetpos from a time_switch__cond_8 record
%% @end
%%--------------------------------------------------------------------
get_bysetpos(TimeSwitchCond) ->
    case TimeSwitchCond of
	_ when is_record(TimeSwitchCond, time_switch__cond_8) ->
	    TimeSwitchCond#time_switch__cond_8.bysetpos
    end.

%%--------------------------------------------------------------------
%% @spec    (TimeSwitchCond) -> term()
%%
%% @doc     get time_ranges from a time_switch__cond_{5,7} record
%% @end
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



