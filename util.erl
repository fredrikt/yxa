-module(util).
-export([timestamp/0, sec_to_date/1, isnumeric/1, regexp_rewrite/2, casecompare/2, casegrep/2, join/2, concat/2,
	 safe_is_process_alive/1, safe_signal/3, remove_v6_brackets/1]).

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

isnumeric(Number) when list(Number) ->
    case regexp:first_match(Number, "^[0-9]+$") of
	{match, _, _} ->
	    true;
	_ ->
	    false
    end;
isnumeric(_) ->
    false.

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

casecompare(none, none) ->
    true;
casecompare(none, String) ->
    false;
casecompare(String, none) ->
    false;
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

safe_is_process_alive(Pid) when pid(Pid) ->
    {is_process_alive(Pid), Pid};
safe_is_process_alive(Name) when atom(Name) ->
    case erlang:whereis(Name) of
	Pid when pid(Pid) ->
	    case is_process_alive(Pid) of
		true ->
		    {true, Pid};
		_ ->
		    {false, Pid}
	    end;
	Pid ->
	    {false, Pid}
    end;	
safe_is_process_alive(_) ->
    false.

safe_signal(LogTag, PidIn, Message) ->
    case util:safe_is_process_alive(PidIn) of
	{true, Pid} ->
	    Pid ! Message,
	    ok;
	{false, Pid} when list(LogTag) ->
	    logger:log(error, LogTag ++ "Can't send signal ~p to pid ~p - not alive or not pid", [Message, Pid]),
	    error;
	{false, Pid} ->
	    error
    end.

remove_v6_brackets([$[ | H]) ->
    %% Might be IPv6 formatted address, check if it ends with "]"
    case string:substr(H, length(H), 1) of
	"]" ->
	    A = string:substr(H, 1, length(H) - 1),
	    case inet_parse:ipv6_address(A) of
		{ok, _} ->
		    A;
		_ ->
		    %% what was between [ and ] was not a valid v6 address
		    "[" ++ A ++ "]"
	    end;
	U ->
	    %% No ending "]"
	    "[" ++ H
    end;
remove_v6_brackets(H) ->
    H.
