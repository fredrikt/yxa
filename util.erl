%%
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
	 remove_v6_brackets/1
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
%% Descrip.: number of seconds elapsed since base point of now()
%% Returns : integer()
%%--------------------------------------------------------------------
timestamp() ->
    {Megasec, Sec, _} = now(),
    Megasec * 1000000 + Sec.

%%--------------------------------------------------------------------
%% Function: sec_to_date(Seconds)
%%           Seconds = integer(), seconds since start point of now()
%% Descrip.: takes a Seconds value (e.g. from timestamp) and returns a
%%           nicely formatted string
%% Returns : "yyyy-mm-dd hh:mm:ss" formated string
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
%% Descrip.: determine if Number is a string containing only numbers,
%%           length must be >= 1
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
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
regexp_rewrite(_Input, []) ->
    nomatch;

regexp_rewrite(Input, [{Regexp, Rewrite} | Rest]) ->
    case group_regexp:groups(Input, Regexp) of
	{match, List} ->
	    apply_rewrite(Rewrite, List);
	nomatch ->
	    regexp_rewrite(Input, Rest)
    end.

%% Function:
%% Descrip.: ???
%% Returns :
apply_rewrite([], _List) ->
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

%% return integer value (0-9) of the chars $0-$9
%% return error if Digit is a non-numerical char (or other type)
digit(Digit) when is_integer(Digit), Digit >= $0, Digit =< $9 ->
    Digit - $0;
digit(_Digit) ->
    error.


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
	_ ->
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
%% XXX badly chosen function name - I mostly associate this name with
%% functions that add a element to a list - hsten
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
		_ ->
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
%% Function: remove_v6_brackets(H)
%%           H = string()
%% Descrip.: ???
%% Returns : ip6_address() | string()
%%--------------------------------------------------------------------
remove_v6_brackets([$[ | R] = H) ->
    %% Might be IPv6 formatted address, check if it ends with "]"
    case string:substr(R, length(R), 1) of
	"]" ->
	    A = string:substr(R, 1, length(R) - 1),
	    case inet_parse:ipv6_address(A) of
		{ok, _} ->
		    A;
		_ ->
		    %% what was between [ and ] was not a valid v6 address
		    H
	    end;
	_ ->
	    %% No ending "]"
	    H
    end;
remove_v6_brackets(H) ->
    H.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
