-module(util).
-export([timestamp/0, sec_to_date/1, isnumeric/1]).

timestamp() ->
    {Megasec, Sec, _} = now(),
    Megasec * 1000000 + Sec.

sec_to_date(never) ->
    "never";
sec_to_date(Seconds) when integer(Seconds) ->
    Nowtime = {Seconds div 1000000, Seconds rem 1000000, 0},
    lists:flatten(localtime_to_string(calendar:now_to_local_time(Nowtime))).

localtime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
		  [Year, Month, Day, Hour, Minute, Second]).

isnumeric(Number) ->
    case regexp:first_match(Number, "^[0-9]+$") of
	{match, _, _} ->
	    true;
	_ ->
	    false
    end.
