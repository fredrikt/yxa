%%%-------------------------------------------------------------------
%%% File    : util.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: Utility functions.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%--------------------------------------------------------------------

-module(util).

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
	 safe_is_process_alive/1,
	 safe_signal/3,
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
%% Function: timestamp()
%% Descrip.: Number of seconds elapsed since start point used by now()
%% Returns : integer()
%%--------------------------------------------------------------------
timestamp() ->
    {Megasec, Sec, _} = now(),
    Megasec * 1000000 + Sec.

%%--------------------------------------------------------------------
%% Function: sec_to_date(Seconds)
%%           Seconds = integer(), seconds since start point used by
%%                     now()
%% Descrip.: Takes a Seconds value (e.g. from timestamp/0 above) and
%%           returns a nicely formatted date-time string.
%% Returns : string(), "yyyy-mm-dd hh:mm:ss"
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
%% Function: isnumeric(Number)
%%           Number = string()
%% Descrip.: Determine if Number is a string containing only numbers.
%%           Length of Number must be >= 1.
%% Returns : true | false
%% Note    : Using the BIF list_to_integer() is probably cheaper.
%%--------------------------------------------------------------------
isnumeric(Number) when is_list(Number) ->
    Pattern = "^[0-9]+\$",
    case regexp:first_match(Number, Pattern) of
	{match, _, _} ->
	    true;
	_ ->
	    false
    end;
isnumeric(_) ->
    false.

%%--------------------------------------------------------------------
%% Function: regexp_rewrite(Input, RegexpList)
%%           Input      = string()
%%           RegexpList = list() of {Regexp, Rewrite} tuple()
%%              Regexp  = string(), e.g. "foo(.+)"
%%              Rewrite = string(), e.g. "\\1@example.com"
%% Descrip.: Do regexp substitution. If Input is "foobar" and the
%%           regexp tuple is {"foo(.+)", "\\1@example.com"} this
%%           function will return "bar@example.com".
%% Returns : Res = string() |
%%           nomatch
%%--------------------------------------------------------------------
regexp_rewrite(_Input, []) ->
    nomatch;

regexp_rewrite(Input, [{Regexp, Rewrite} | Rest]) ->
    case group_regexp:groups(Input, Regexp) of
	{match, List} ->
	    %% If Input was "foobar" and Regexp was "foo(.+)" then List will be ["bar"].
	    %% If Input was "foobar" and Regexp was "foo.+" then List will be [].
	    %% If Input was "foobar" and Regexp was "(fo.)(.+)" then List will be ["foo", "bar"].
	    apply_rewrite(Rewrite, List);
	nomatch ->
	    regexp_rewrite(Input, Rest)
    end.

%%--------------------------------------------------------------------
%% Function: casecompare(Str1, Str2)
%%           Str1, Str2 = string() | none
%% Descrip.: return true if Str1 and Str2 are either 'none' or the
%%           same string (ignore case)
%% Returns : true | false
%%--------------------------------------------------------------------
casecompare(none, none) ->
    true;
casecompare(none, _String) ->
    false;
casecompare(_String, none) ->
    false;
casecompare(String1, String2) ->
    S1 = httpd_util:to_lower(String1),
    case httpd_util:to_lower(String2) of
	S1 ->
	    true;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: casegrep(Str, StrList)
%%           Str = string() | none
%%           StrList = list of string()
%% Descrip.: determine if Str is a memeber of StrList
%% Returns : true | false
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
%% Function: join(Strings, Separator)
%%           Strings = list of string()
%%           Separator = string()
%% Descrip.: create a single string from the Strings entries,
%%           Separator is placed between all entries (but not after
%%           the last one)
%% Returns : string()
%% XXX badly chosen function name - I mostly associate this name with
%% functions like lists:append - hsten
%%--------------------------------------------------------------------
join([], _Separator) ->
    [];
join([A], _Separator) ->
    A;
join([String | Rest], Separator) ->
    String ++ Separator ++ join(Rest, Separator).

%%--------------------------------------------------------------------
%% Function: concat(Strings, Separator)
%%           Strings = list of string()
%%           Separator = string()
%% Descrip.: create a single string from the Strings entries, each
%%           entry ends with the Separator.
%% Returns : list()
%%
%% XXX badly chosen function name - I mostly associate this name with
%% functions that add a element to a list - hsten
%%
%% XXX Function seems to be unused, and kind of useless (?).
%%--------------------------------------------------------------------
concat([], _Separator) ->
    [];
concat([A | B], Separator) ->
    A ++ Separator ++ concat(B, Separator).

%%--------------------------------------------------------------------
%% Function: safe_is_process_alive(Process)
%%           Process = pid() | atom()
%% Descrip.: determine if the process Process is running
%% Returns : {Alive, ProcessPid}
%%           Alive = true | false
%%           ProcessPid = pid()
%%--------------------------------------------------------------------
safe_is_process_alive(Pid) when is_pid(Pid) ->
    {is_process_alive(Pid), Pid};
safe_is_process_alive(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
	Pid when is_pid(Pid) ->
	    case is_process_alive(Pid) of
		true ->
		    {true, Pid};
		false ->
		    {false, Pid}
	    end;
	E ->
	    {false, E}
    end.

%%--------------------------------------------------------------------
%% Function: safe_signal(LogTag, PidIn, Message)
%%           LogTag = string() - application name (???)
%%           PidIn = pid() | atom()
%%           Message = string()
%% Descrip.: Check if a process is alive before sending it a signal.
%% Returns : ok | error
%%--------------------------------------------------------------------
safe_signal(LogTag, PidIn, Message) ->
    case util:safe_is_process_alive(PidIn) of
	{true, Pid} ->
	    Pid ! Message,
	    ok;
	{false, Pid} when is_list(LogTag) ->
	    logger:log(error, LogTag ++ "Can't send signal ~p to pid '~p' (~p) - not alive or not pid",
		       [Message, PidIn, Pid]),
	    error;
	{false, _} ->
	    error
    end.

%%--------------------------------------------------------------------
%% Function: remove_v6_brackets(In)
%%           In = string()
%% Descrip.: If In is "[IPv6Address]" then return "IPv6Address".
%%           Otherwise, return whatever In was.
%% Returns : Addr = string(), IPv6 address |
%%           In
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
%% Function: apply_rewrite(Rewrite, List)
%% Descrip.: Do regexp group substitution. Part of regexp_rewrite.
%%           Easiest to explain with examples :
%%
%%           Rewrite = "\\1@foo", List = ["first"] -> "first@foo"
%%           Rewrite = "abc\\1def\\2", List = ["123", "456"] ->
%%                        "abc123def456"
%% Returns : Res = string() |
%%           []
%% Note    : Function is not tail recursive.
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
	    [C | apply_rewrite(Rest, List)];
	0 ->
	    %% Backslash followed by the digit zero, not a group.
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
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%%--------------------------------------------------------------------
test() ->

    %% test timestamp()
    %%--------------------------------------------------------------------
    io:format("test: timestamp/0 - 1~n"),
    %% we can't test more than that this function returns an integer...
    Timestamp1 = timestamp(),
    true = is_integer(Timestamp1),

    io:format("test: timestamp/0 - 2~n"),
    %% ... and that the integer returned is at least 1.1 billion
    true = (Timestamp1 >= 1100000000),    

    %% test sec_to_date(Seconds)
    %%--------------------------------------------------------------------
    io:format("test: sec_to_date/1 - 1~n"),
    "2005-01-31 14:06:07" = sec_to_date(1107176767),


    %% test isnumeric(Number)
    %%--------------------------------------------------------------------
    io:format("test: isnumeric/1 - 1~n"),
    %% small number
    true = isnumeric("123"),

    io:format("test: isnumeric/1 - 2~n"),
    %% big number
    true = isnumeric(string:copies("123", 10)),

    io:format("test: isnumeric/1 - 3~n"),
    %% not only number
    false = isnumeric(" 123"),

    io:format("test: isnumeric/1 - 4~n"),
    %% not number at all
    false = isnumeric("X"),

    io:format("test: isnumeric/1 - 5~n"),
    %% not even a string
    false = isnumeric({1,2,3}),

    
    %% test apply_rewrite(Rewrite, List)
    %%--------------------------------------------------------------------
    io:format("test: apply_rewrite/2 - 1~n"),
    %% single group to replace
    "first@foo" = apply_rewrite("\\1@foo", ["first"]),

    io:format("test: apply_rewrite/2 - 2~n"),
    %% two groups
    "abc123def456" = apply_rewrite("abc\\1def\\2", ["123", "456"]),

    io:format("test: apply_rewrite/2 - 3~n"),
    %% double backslash check
    "a\\1bc123" = apply_rewrite("a\\\\1bc\\1", ["123"]),

    io:format("test: apply_rewrite/2 - 4~n"),
    %% Two groups, only one used in rewrite. Such a List might not be
    %% possible for regexp_rewrite to call us with, but we currently
    %% don't check that.
    "foobar" = apply_rewrite("\\1bar", ["foo", "456"]),

    io:format("test: apply_rewrite/2 - 5~n"),
    %% Too few elements in List - we crash on that.
    {'EXIT', {function_clause, _}} = (catch apply_rewrite("\\1bar", [])),


    %% test regexp_rewrite(Input, RegexpList)
    %%--------------------------------------------------------------------
    io:format("test: regexp_rewrite/2 - 1~n"),
    %% Single regexp
    "foobar" = regexp_rewrite("123foobar456", [{".*foo(...)", "foo\\1"}]),

    io:format("test: regexp_rewrite/2 - 2~n"),
    %% More than one regexp
    "foobar" = regexp_rewrite("123foobar456", [{"nomatch", "abc123"},
					       {".*foo(...)", "foo\\1"}]),

    io:format("test: regexp_rewrite/2 - 3~n"),
    %% No matching regexp
    nomatch = regexp_rewrite("123f00bar456", [{"nomatch", "abc123"},
					      {".*foo(...)", "foo\\1"}]),

    %% test casecompare(Str1, Str2)
    %%--------------------------------------------------------------------
    io:format("test: casecompare/2 - 1~n"),
    true = casecompare("abc", "ABC"),

    io:format("test: casecompare/2 - 2~n"),
    true = casecompare("", ""),

    io:format("test: casecompare/2 - 3~n"),
    true = casecompare(none, none),

    io:format("test: casecompare/2 - 4~n"),
    false = casecompare("zzz", "zzzz"),

    io:format("test: casecompare/2 - 5~n"),
    false = casecompare("zzz", none),

    io:format("test: casecompare/2 - 6~n"),
    false = casecompare(none, "A"),


    %% test casegrep(String, List)
    %%--------------------------------------------------------------------
    io:format("test: casegrep/2 - 1~n"),
    true = casegrep("test", ["test"]),

    io:format("test: casegrep/2 - 2~n"),
    true = casegrep("test", ["foo", "TEST"]),

    io:format("test: casegrep/2 - 3~n"),
    true = casegrep("teSt", [none, "test"]),

    io:format("test: casegrep/2 - 4~n"),
    true = casegrep(none, ["test", none]),

    io:format("test: casegrep/2 - 5~n"),
    false = casegrep(none, ["test", "foo"]),

    io:format("test: casegrep/2 - 6~n"),
    false = casegrep("bar", ["test", "foo"]),


    %% test join(Strings, Separator)
    %%--------------------------------------------------------------------
    io:format("test: join/2 - 1~n"),
    %% straight forward
    "hi world" = join(["hi", "world"], " "),

    io:format("test: join/2 - 2~n"),
    %% a single element
    "hi" = join(["hi"], " "),

    io:format("test: join/2 - 3~n"),
    %% empty separator
    "hiworld" = join(["hi", "world"], ""),

    io:format("test: join/2 - 3~n"),
    %% empty Strings
    [] = join([], "neverseen"),


    %% test concat(Strings, Separator)
    %%--------------------------------------------------------------------
    io:format("test: concat/2 - 1~n"),
    %% straight forward
    "hi world " = concat(["hi", "world"], " "),

    io:format("test: concat/2 - 2~n"),
    %% a single element
    "hi " = concat(["hi"], " "),

    io:format("test: concat/2 - 3~n"),
    %% empty separator
    "hiworld" = concat(["hi", "world"], ""),

    io:format("test: concat/2 - 4~n"),
    %% empty Strings
    [] = concat([], "neverseen"),


    %% test safe_is_process_alive(PidOrName)
    %%--------------------------------------------------------------------
    io:format("test: safe_is_process_alive/1 - 1~n"),
    %% myself as input, should definately be alive
    MyPid = self(),
    {true, MyPid} = safe_is_process_alive(MyPid),

    io:format("test: safe_is_process_alive/1 - 2~n"),
    %% atom input (valid) of a process that should really be alive
    {true, LoggerPid} = safe_is_process_alive(logger),
    true = is_pid(LoggerPid),

    io:format("test: safe_is_process_alive/1 - 3~n"),
    %% list input (invalid)
    {'EXIT', {function_clause, _}} = (catch safe_is_process_alive("foo")),

    io:format("test: safe_is_process_alive/1 - 4~n"),
    %% reference input (invalid)
    {'EXIT', {function_clause, _}} = (catch safe_is_process_alive(make_ref())),


    %% test safe_signal(LogTag, PidOrName, Message)
    %%--------------------------------------------------------------------
    io:format("test: safe_is_process_alive/1 - 1.1~n"),
    %% send message to ourselves
    SafeSignalRef = make_ref(),
    ok = safe_signal("foo", self(), {SafeSignalRef, "test"}),

    io:format("test: safe_is_process_alive/1 - 1.2~n"),
    %% check that we got the message
    receive
	{SafeSignalRef, "test"} ->
	    ok
    after
	0 ->
	    throw({error, "did not get signal I sent to myself"})
    end,

    
    %% test remove_v6_brackets(In)
    %%--------------------------------------------------------------------
    io:format("test: remove_v6_brackets/1 - 1~n"),
    "[non-IPv6-address]" = remove_v6_brackets("[non-IPv6-address]"),

    io:format("test: remove_v6_brackets/1 - 2~n"),
    "2001::abc" = remove_v6_brackets("[2001::abc]"),

    io:format("test: remove_v6_brackets/1 - 3~n"),
    "[2001::" = remove_v6_brackets("[2001::"),

    io:format("test: remove_v6_brackets/1 - 4~n"),
    "2001::]" = remove_v6_brackets("2001::]"),

    ok.
