-module(util).
-export([timestamp/0, sec_to_date/1, isnumeric/1, regexp_rewrite/2, casecompare/2, casegrep/2, join/2, concat/2]).

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

digit(Digit) when integer(Digit), Digit >= $0, Digit =< $9 ->
    Digit - $0;
digit(Digit) ->
    error.

apply_rewrite([], List) ->
    [];
apply_rewrite([$\\, $\\ | Rest], List) ->
    [$\\ | apply_rewrite(Rest, List)];
apply_rewrite([$\\, C | Rest], List) ->
    case digit(C) of
	error ->
	    [C | apply_rewrite(Rest, List)];
	0 ->
	    [C | apply_rewrite(Rest, List)];	    
	Number ->
	    lists:nth(Number, List) ++ apply_rewrite(Rest, List)
    end;
apply_rewrite([C | Rest], List) ->
    [C | apply_rewrite(Rest, List)].

regexp_rewrite(Input, []) ->
    nomatch;

regexp_rewrite(Input, [{Regexp, Rewrite} | Rest]) ->
    case group_regexp:groups(Input, Regexp) of
	{match, List} ->
	    apply_rewrite(Rewrite, List);
	nomatch ->
	    regexp_rewrite(Input, Rest);
	{error, Error} ->
	    logger:log(normal, "Error in regexp ~p: ~p", [Regexp, Error]),
	    []
    end.

casecompare(String1, String2) ->
    S1 = httpd_util:to_lower(String1),
    case httpd_util:to_lower(String2) of
	S1 ->
	    true;
	_ ->
	    false
    end.

casegrep(String1,[]) ->
	nomatch;

casegrep(String1,[String2 | Rest]) ->
	case casecompare(String1,String2) of
		true -> 
			true;
		_ ->
			casegrep(String1,Rest)
	end.
			 
join([], Separator) ->
    [];
join([A], Separator) ->
    A;
join([String | Rest], Separator) ->
    String ++ Separator ++ join(Rest, Separator).

concat([], Separator) ->
    [];
concat([A | B], Separator) ->
    A ++ Separator ++ concat(B, Separator).
