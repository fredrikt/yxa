%%%-------------------------------------------------------------------
%%% File    : util.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Utility functions focused on string manipulation and
%%%           similar.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%--------------------------------------------------------------------

-module(util).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 timestamp/0,
	 sec_to_date/1,
	 isnumeric/1,
	 regexp_rewrite/2,
	 casecompare/2,
	 casegrep/2,
	 join/2,
	 concat/2,
	 remove_v6_brackets/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

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
%% @spec    () -> integer()
%%
%% @doc     Number of seconds elapsed since start point used by now()
%% @end
%%--------------------------------------------------------------------
timestamp() ->
    {Megasec, Sec, _} = os:timestamp(),
    Megasec * 1000000 + Sec.

%%--------------------------------------------------------------------
%% @spec    (Seconds) -> string()
%%
%%            Seconds = integer() "seconds since start point used by now()"
%%
%% @doc     Takes a Seconds value (for example the result of
%%          timestamp/0) and returns a nicely formatted date-time
%%          string ("yyyy-mm-dd hh:mm:ss").
%% @end
%%--------------------------------------------------------------------
sec_to_date(never) ->
    "never";
sec_to_date(Seconds) when is_integer(Seconds) ->
    Nowtime = {Seconds div 1000000, Seconds rem 1000000, 0},
    lists:flatten(localtime_to_string(calendar:now_to_local_time(Nowtime))).

localtime_to_string({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
		  [Year, Month, Day, Hour, Minute, Second]).

%%--------------------------------------------------------------------
%% @spec    (Number) -> true | false
%%
%%            Number = string()
%%
%% @doc     Determine if Number is a string containing only numbers.
%%          Length of Number must be >= 1. Note : Using the BIF
%%          list_to_integer() is probably cheaper.
%% @end
%%--------------------------------------------------------------------
isnumeric(Number) when is_list(Number) ->
    Pattern = "^[0-9]+\$",
    match == re:run(Number, Pattern, [{capture, none}]);
isnumeric(_) ->
    false.

%%--------------------------------------------------------------------
%% @spec    (Input, RegexpList) -> string() | nomatch
%%
%%            Input      = string()
%%            RegexpList = [{Regexp, Rewrite}]
%%            Regexp     = string() "e.g. \"foo(.+)\""
%%            Rewrite    = string() "e.g. \"\\1@example.com\""
%%
%% @doc     Do regexp substitution. If Input is "foobar" and the
%%          regexp tuple is {"foo(.+)", "\\1@example.com"} this
%%          function will return "bar@example.com".
%% @end
%%--------------------------------------------------------------------
regexp_rewrite(_Input, []) ->
    nomatch;

regexp_rewrite(Input, [{Regexp, Rewrite} | Rest]) ->
    %% XXX would probably be better to use something like
    %%   re:replace(Input, Regexp, Rewrite, [{return, list}])
    %% instead of having our own replace-\1-with-something-else, but that
    %% is an backwards incompatible change.
    case re:run(Input, Regexp, [{capture, all, list}]) of
	{match, [_ | List]} ->
	    apply_rewrite(Rewrite, List);
	nomatch ->
	    regexp_rewrite(Input, Rest)
    end.

%%--------------------------------------------------------------------
%% @spec    (Str1, Str2) -> true | false
%%
%%            Str1 = string() | none
%%            Str2 = string() | none
%%
%% @doc     return true if Str1 and Str2 are either 'none' or the same
%%          string (ignore case)
%% @end
%%--------------------------------------------------------------------
casecompare(none, none) ->
    true;
casecompare(none, _String) ->
    false;
casecompare(_String, none) ->
    false;
casecompare(String1, String2) ->
    S1 = string:to_lower(String1),
    case string:to_lower(String2) of
	S1 ->
	    true;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (Str, StrList) -> true | false
%%
%%            Str     = string() | none
%%            StrList = [string()]
%%
%% @doc     determine if Str is a member of StrList
%% @end
%%--------------------------------------------------------------------
casegrep(_String1, []) ->
    false;
casegrep(String1, [String2 | Rest]) ->
    case casecompare(String1, String2) of
	true ->
	    true;
	false ->
	    casegrep(String1, Rest)
    end.

%%--------------------------------------------------------------------
%% @spec    (Strings, Separator) -> string()
%%
%%            Strings   = [string()]
%%            Separator = string()
%%
%% @doc     Create a single string from the Strings entries. Separator
%%          is placed between all entries (but not after the last
%%          one).
%% @end
%%--------------------------------------------------------------------
%% XXX badly chosen function name - I mostly associate this name with
%% functions like lists:append - hsten
join([], _Separator) ->
    [];
join([H | T], Separator) ->
    H ++ lists:concat([Separator ++ X || X <- T]).

%%--------------------------------------------------------------------
%% @spec    (Strings, Separator) -> list()
%%
%%            Strings   = [string()]
%%            Separator = string()
%%
%% @doc     create a single string from the Strings entries, each
%%          entry ends with the Separator.
%% @end
%%--------------------------------------------------------------------
%% XXX badly chosen function name - I mostly associate this name with
%% functions that add a element to a list - hsten
%%
%% XXX Function seems to be unused, and kind of useless (?).
concat([], _Separator) ->
    [];
concat([A | B], Separator) ->
    A ++ Separator ++ concat(B, Separator).

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            In | V6addr
%%
%%            In = string()
%%
%%            V6Addr = string()
%%
%% @doc     If In is "[IPv6Address]" then return "IPv6Address".
%%          Otherwise, return whatever In was.
%% @end
%%--------------------------------------------------------------------
remove_v6_brackets([$[ | Rest] = In) ->
    %% Might be IPv6 formatted address, check if it ends with "]"
    case string:substr(Rest, length(Rest), 1) of
	"]" ->
	    A = string:substr(Rest, 1, length(Rest) - 1),
	    case inet_parse:ipv6_address(A) of
		{ok, _} ->
		    A;
		_ ->
		    %% what was between [ and ] was not a valid v6 address
		    In
	    end;
	_ ->
	    %% No ending "]"
	    In
    end;
remove_v6_brackets(In) ->
    In.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Rewrite, List) ->
%%            Res
%%
%%            Res = string() |
%%            []
%%
%% @doc     Do regexp group substitution. Part of regexp_rewrite.
%%          Easiest to explain with examples :
%%          Rewrite = "\\1@foo", List = ["first"] -> "first@foo"
%%          Rewrite = "abc\\1def\\2", List = ["123", "456"] ->
%%          "abc123def456" Note : Function is not tail recursive.
%% @end
%%--------------------------------------------------------------------
apply_rewrite([], _List) ->
    [];
apply_rewrite([$\\, $\\ | Rest], List) ->
    %% Double backslash - not a group.
    [$\\ | apply_rewrite(Rest, List)];
apply_rewrite([$\\, C | Rest], List) ->
    case digit(C) of
	error ->
	    %% Backslash not followed by digit, not a group.
	    %% XXX shouldn't we preserve the backslash as well as C?
	    [C | apply_rewrite(Rest, List)];
	0 ->
	    %% Backslash followed by the digit zero, not a group.
	    %% XXX shouldn't we preserve the backslash as well as C?
	    [C | apply_rewrite(Rest, List)];
	Number ->
	    %% Backslash followed by a digit between 1 and 9.
	    %% Substitute with that element from List.
	    %% XXX we crash with a slightly cryptic error if List does
	    %% not have enough elements ({function_clause, ...})!
	    lists:nth(Number, List) ++ apply_rewrite(Rest, List)
    end;
apply_rewrite([C | Rest], List) ->
    %% Not a backslash. Check next character.
    [C | apply_rewrite(Rest, List)].

%% return integer value (0-9) of the chars $0-$9
%% return error if Digit is a non-numerical char (or other type)
digit(Digit) when is_integer(Digit), Digit >= $0, Digit =< $9 ->
    Digit - $0;
digit(_Digit) ->
    error.


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
-ifdef( YXA_NO_UNITTEST ).
test() ->
    {error, "Unit test code disabled at compile time"}.

-else.

test() ->

    %% test timestamp()
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "timestamp/0 - 1"),
    %% we can't test more than that this function returns an integer...
    Timestamp1 = timestamp(),
    true = is_integer(Timestamp1),

    autotest:mark(?LINE, "timestamp/0 - 2"),
    %% ... and that the integer returned is at least 1.1 billion
    true = (Timestamp1 >= 1100000000),

    %% test sec_to_date(Seconds)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "sec_to_date/1 - 1"),
    "2005-01-31 14:06:07" = sec_to_date(1107176767),

    autotest:mark(?LINE, "sec_to_date/1 - 2"),
    "never" = sec_to_date(never),

    %% test isnumeric(Number)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "isnumeric/1 - 1"),
    %% small number
    true = isnumeric("123"),

    autotest:mark(?LINE, "isnumeric/1 - 2"),
    %% big number
    true = isnumeric(string:copies("123", 10)),

    autotest:mark(?LINE, "isnumeric/1 - 3"),
    %% not only number
    false = isnumeric(" 123"),

    autotest:mark(?LINE, "isnumeric/1 - 4"),
    %% not number at all
    false = isnumeric("X"),

    autotest:mark(?LINE, "isnumeric/1 - 5"),
    %% not even a string
    false = isnumeric({1,2,3}),

    autotest:mark(?LINE, "isnumeric/1 - 6"),
    %% empty string is not numeric
    false = isnumeric(""),


    %% test apply_rewrite(Rewrite, List)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "apply_rewrite/2 - 1"),
    %% single group to replace
    "first@foo" = apply_rewrite("\\1@foo", ["first"]),

    autotest:mark(?LINE, "apply_rewrite/2 - 2"),
    %% two groups
    "abc123def456" = apply_rewrite("abc\\1def\\2", ["123", "456"]),

    autotest:mark(?LINE, "apply_rewrite/2 - 3"),
    %% double backslash check
    %% XXX is this really correct? We loose our backslashes - ft
    "a\\1bc123" = apply_rewrite("a\\\\1bc\\1", ["123"]),

    autotest:mark(?LINE, "apply_rewrite/2 - 4"),
    %% Two groups, only one used in rewrite. Such a List might not be
    %% possible for regexp_rewrite to call us with, but we currently
    %% don't check that.
    "foobar" = apply_rewrite("\\1bar", ["foo", "456"]),

    autotest:mark(?LINE, "apply_rewrite/2 - 5"),
    %% Too few elements in List - we crash on that.
    {'EXIT', {function_clause, _}} = (catch apply_rewrite("\\1bar", [])),

    autotest:mark(?LINE, "apply_rewrite/2 - 6"),
    %% backslashes not followed by digits between 1 and 9 (one '0' and one 'c')
    %% XXX is this really correct? We loose our backslashes - ft
    "a0bc123" = apply_rewrite("a\\0b\\c\\1", ["123"]),


    %% test regexp_rewrite(Input, RegexpList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "regexp_rewrite/2 - 1"),
    %% Single regexp
    "foobar" = regexp_rewrite("123foobar456", [{".*foo(...)", "foo\\1"}]),

    autotest:mark(?LINE, "regexp_rewrite/2 - 2"),
    %% More than one regexp
    "foobar" = regexp_rewrite("123foobar456", [{"nomatch", "abc123"},
					       {".*foo(...)", "foo\\1"}]),

    autotest:mark(?LINE, "regexp_rewrite/2 - 3"),
    %% No matching regexp
    nomatch = regexp_rewrite("123f00bar456", [{"nomatch", "abc123"},
					      {".*foo(...)", "foo\\1"}]),

    %% test casecompare(Str1, Str2)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "casecompare/2 - 1"),
    true = casecompare("abc", "ABC"),

    autotest:mark(?LINE, "casecompare/2 - 2"),
    true = casecompare("", ""),

    autotest:mark(?LINE, "casecompare/2 - 3"),
    true = casecompare(none, none),

    autotest:mark(?LINE, "casecompare/2 - 4"),
    false = casecompare("zzz", "zzzz"),

    autotest:mark(?LINE, "casecompare/2 - 5"),
    false = casecompare("zzz", none),

    autotest:mark(?LINE, "casecompare/2 - 6"),
    false = casecompare(none, "A"),


    %% test casegrep(String, List)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "casegrep/2 - 1"),
    true = casegrep("test", ["test"]),

    autotest:mark(?LINE, "casegrep/2 - 2"),
    true = casegrep("test", ["foo", "TEST"]),

    autotest:mark(?LINE, "casegrep/2 - 3"),
    true = casegrep("teSt", [none, "test"]),

    autotest:mark(?LINE, "casegrep/2 - 4"),
    true = casegrep(none, ["test", none]),

    autotest:mark(?LINE, "casegrep/2 - 5"),
    false = casegrep(none, ["test", "foo"]),

    autotest:mark(?LINE, "casegrep/2 - 6"),
    false = casegrep("bar", ["test", "foo"]),


    %% test join(Strings, Separator)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "join/2 - 1"),
    %% straight forward
    "hi world" = join(["hi", "world"], " "),

    autotest:mark(?LINE, "join/2 - 2"),
    %% a single element
    "hi" = join(["hi"], " "),

    autotest:mark(?LINE, "join/2 - 3"),
    %% empty separator
    "hiworld" = join(["hi", "world"], ""),

    autotest:mark(?LINE, "join/2 - 4"),
    %% empty Strings
    [] = join([], "neverseen"),


    %% test concat(Strings, Separator)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "concat/2 - 1"),
    %% straight forward
    "hi world " = concat(["hi", "world"], " "),

    autotest:mark(?LINE, "concat/2 - 2"),
    %% a single element
    "hi " = concat(["hi"], " "),

    autotest:mark(?LINE, "concat/2 - 3"),
    %% empty separator
    "hiworld" = concat(["hi", "world"], ""),

    autotest:mark(?LINE, "concat/2 - 4"),
    %% empty Strings
    [] = concat([], "neverseen"),


    %% test remove_v6_brackets(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "remove_v6_brackets/1 - 1"),
    "[non-IPv6-address]" = remove_v6_brackets("[non-IPv6-address]"),

    autotest:mark(?LINE, "remove_v6_brackets/1 - 2"),
    "2001::abc" = remove_v6_brackets("[2001::abc]"),

    autotest:mark(?LINE, "remove_v6_brackets/1 - 3"),
    "[2001::" = remove_v6_brackets("[2001::"),

    autotest:mark(?LINE, "remove_v6_brackets/1 - 4"),
    "2001::]" = remove_v6_brackets("2001::]"),

    ok.

-endif.
