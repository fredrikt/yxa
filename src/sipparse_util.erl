% This module implements some of the parsing rules needed for
%% the BNF defined in RFC 3261 (chapter 25 page 218)
%%
%% Note: The functions in this module are assumed to conform STRICTLY
%% to the BNF, if not otherwise stated.
%%--------------------------------------------------------------------

-module(sipparse_util).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 split_fields/2,
	 split_quoted_string/1,
	 parse_host/1,
	 parse_hostport/1,
	 is_alpha/1,
	 is_digit/1,
	 is_alphanum/1,
	 is_token/1,
	 is_IPv4address/1,
	 is_IPv6reference/1,
	 is_digit/1,
	 is_hostname/1,
	 str_to_float/1,
	 str_to_qval/1,
	 is_qval/1,
	 strip/3,

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
%% Function: split_fields(FieldStr, Sep)
%%           FieldStr = string()
%%           Sep      = char()
%% Descrip.: split FieldStr into two parts where Sep is encountered
%% Returns : {First, Second} | {First} | throw()
%%
%% Example, if Sep = @ then:
%% "foo@bar"  = {"foo", "bar"}
%% "foo"      = {"foo"}
%% "foo@"     = throw()
%% "@bar"     = throw()
%% "foo@@bar" = throw()
%% "@"        = throw()
%% ""         = {""}          XXX is this ok ? nothing currently
%%                            depends on this behaviour.
%%--------------------------------------------------------------------
split_fields(FieldStr, Sep) ->
    split_fields(FieldStr, Sep, []).

%% nothing before Sep
split_fields([Sep | _FieldStrRest], Sep, []) ->
    throw({error, no_first_part});
%% nothing after Sep
split_fields([Sep], Sep, _FirstSegmentAcc) ->
    throw({error, no_second_part});
%% more than one Sep char() found
split_fields([Sep, Sep | _FieldStrRest], Sep, _FirstSegmentAcc) ->
    throw({error, more_than_one_separator});

%% no separator found
split_fields([], _Sep, FirstSegmentAcc) ->
    {lists:reverse(FirstSegmentAcc)};

%% separator found
split_fields([Sep | FieldStrRest], Sep, FirstSegmentAcc) ->
    {lists:reverse(FirstSegmentAcc), FieldStrRest};
%% still looking for separator
split_fields([Char | FieldStrRest], Sep, FirstSegmentAcc) ->
    split_fields(FieldStrRest, Sep, [Char | FirstSegmentAcc]).

%%--------------------------------------------------------------------
%% Function: split_quoted_string(In)
%%           In = string()
%% Descrip.: Parse out a quoted string from In and return the quoted
%%           part as First and the rest as Second.
%% Returns : {ok, First, Second} | throw()
%%           First = Second = string()
%%--------------------------------------------------------------------
split_quoted_string([34 | Rest]) ->	%% 34 is '"'
    split_quoted_string2(Rest, false, []);
split_quoted_string(In) ->
    erlang:error(does_not_start_with_quote, [In]).

split_quoted_string2([92 | T], _Escape=false, Res) ->	%% 92 is '\'
    %% escape-char, and Escape was false
    split_quoted_string2(T, true, [92 | Res]);	%% 92 is '\'
split_quoted_string2([34 | T], _Escape=false, Res) ->	%% 34 is '"'
    %% found non-escaped quote, we are finished
    {ok, lists:reverse(Res), T};
split_quoted_string2([H | T], _Escape, Res) ->
    %% not escape-char, and not end-quote
    split_quoted_string2(T, false, [H | Res]);
split_quoted_string2([], Escape, Res) ->
    erlang:error(no_end_quote, [Escape, Res]).


%%--------------------------------------------------------------------
%% Function: parse_host(Host)
%%           Host = string()
%% the host rule alows for a varity of formats:
%%
%% hostname
%% ipv4address
%% ipv6reference
%%
%% Descrip.: return Host if it is wellformed, a exception is
%%           thrown if the Host is malformed
%% Returns : Host | throw() if parse fails
%%           Host = string()
%%           throw() = {error, Reason} | {'EXIT',Reason}
%%--------------------------------------------------------------------
parse_host(Host) ->
    {Host, none} = parse_hostport(Host),
    Host.


%%--------------------------------------------------------------------
%% Function: parse_hostport(HostPort)
%%           HostPort = string()
%% the host:port section of the sip url can be formated in a varity of
%% ways:
%%
%% hostname:port
%% hostname
%% ipv4address:port
%% ipv4address
%% [ipv6reference]:port
%% ipv6reference
%%
%% Descrip.: splits the string into it's two parts, a exception is
%%           thrown if the Host or Port is malformed
%% Returns : {Host, Port} | throw() if parse fails
%%           Host = string()
%%           Port = integer() | none
%%           throw() = {error, Reason} | {'EXIT', Reason}
%%--------------------------------------------------------------------
parse_hostport(HostPort) ->
    H = hd(HostPort),
    case H of
	91 ->	%% 91 is $[
	    %% can only be an ipv6 reference
	    {ok, Host, Port} = parse_ipv6hostport(HostPort),
	    {Host, Port};
	_ ->
	    %% IPv4 and domain names require a complete parse attempt to be distinguished
	    %% properly from each other, as they may both start with numbers in H
	    case is_digit(H) of
		true ->
		    %% probably a ipv4 address
		    case catch parse_ipv4hostport(HostPort) of
			{ok, Host, Port} when is_list(Host), is_integer(Port); Port == none ->
			    {Host, Port};
			_ ->
			    %% was not IPv4 address, try to parse as IPv6 address without
			    %% brackets, and then as hostname
			    {ok, Host, Port} = parse_hostport_v6_or_hostname(HostPort),
			    {Host, Port}
		    end;
		false ->
		    %% must be a hostname, or an IPv6 address without brackets
		    {ok, Host, Port} = parse_hostport_v6_or_hostname(HostPort),
		    {Host, Port}
	    end
    end.

parse_hostport_v6_or_hostname(In) ->
    case catch parse_ipv6hostport(In) of
	{ok, Host, Port} ->
	    {ok, Host, Port};
	_ ->
	    parse_domain_hostport(In)
    end.

parse_ipv6hostport([91 | IPv6Hostport]) ->	%% 91 is $[
    {IPv6Ref, Port} = get_ipv6ref_and_port(IPv6Hostport),
    %% throw if not ip6 ref
    is_IPv6reference_throw(IPv6Ref),
    %% XXX return without brackets?
    {ok, "[" ++ IPv6Ref ++ "]", Port};
parse_ipv6hostport(In) ->
    %% throw if not ip6 ref
    is_IPv6reference_throw(In),
    %% XXX return without brackets?
    {ok, "[" ++ In ++ "]", none}.

get_ipv6ref_and_port(IPv6Hostport) ->
    case catch sipparse_util:split_fields(IPv6Hostport, 93) of	%% 93 is $]
	{error, no_second_part} ->
	    %% a hack to match when no port is included in IPv6Hostport
	    %% this will occur because split_fields/2 treats trailing
	    %% separators as a error (which it should)
	    %% remove trailing "]"
	    [93 | R] = lists:reverse(IPv6Hostport),	%% 93 is $]
	    IPv6Ref = lists:reverse(R),
	    {IPv6Ref, none};
	{error, R} ->
	    %% other errors
	    throw({error, R});
	{Host, PortRest} ->
	    %% a style "[...]:Port"
	    [$: | Port] = PortRest,
	    {Host, list_to_integer(Port)}
    end.


parse_ipv4hostport(HostPort) ->
    case sipparse_util:split_fields(HostPort, $:) of
	{Host, Port} ->
	    %% throw if not a IPv4 address string, or not a numeric port
	    {ok, is_IPv4address_throw(Host), list_to_integer(Port)};
	{Host} ->
	    %% throw if not a IPv4 address string
	    {ok, is_IPv4address_throw(Host), none}
    end.


parse_domain_hostport(HostPort) ->
    case sipparse_util:split_fields(HostPort, $:) of
	{Host, Port} ->
	    %% throw if not a hostname
	    is_hostname_throw(Host),
	    {ok, rm_trailing_dot_from_hostname(Host), list_to_integer(Port)};
	{Host} ->
	    %% throw if not a hostname
	    is_hostname_throw(Host),
	    {ok, rm_trailing_dot_from_hostname(Host), none}
    end.

rm_trailing_dot_from_hostname(H) ->
    RH = lists:reverse(H),
    case RH of
	[$. | R] -> lists:reverse(R);
	_ -> H
    end.

%%--------------------------------------------------------------------
%% BNF checking functions, based on RFC3261 chapter 25
%%--------------------------------------------------------------------
is_alpha(Char) ->
    if
	(Char >= $A) and (Char =< $Z) ->
	    true;
	(Char >= $a) and (Char =< $z) ->
	    true;
	true ->
	    false
    end.

is_digit(Char) ->
    if
	(Char >= $0) and (Char =< $9) ->
	    true;
	true ->
	    false
    end.

is_alphanum(Char) ->
    is_alpha(Char) or is_digit(Char).

%% return: true | false
is_token(Str) ->
    F = fun
	    ($-, Acc) -> Acc;
	    ($., Acc) -> Acc;
	    ($!, Acc) -> Acc;
	    ($%, Acc) -> Acc;
	    ($*, Acc) -> Acc;
	    ($_, Acc) -> Acc;
	    ($+, Acc) -> Acc;
	    ($`, Acc) -> Acc;
	    ($\', Acc) -> Acc;
	    ($~, Acc) -> Acc;
	    (Char, Acc) ->
		Acc and is_alphanum(Char)
	end,
    lists:foldl(F,true,Str).


%%--------------------------------------------------------------------
%% Function: is_hostname(Str)
%%           Str = string(), the host string
%% Descrip.: parse host string, throw an exception if it doesn't
%%           conform to the format specified in RFC3261 chapter 25.1
%%           p218
%% Returns : true | throw()
%% Note    : this function only matches symbolic hostnames
%%
%% These rules are the same:
%%
%% is_digit = [0-9]
%% is_alpha = [A-Za-z]
%% is_alphanum = [A-Za-z0-9]
%%--------------------------------------------------------------------
is_hostname(Host) ->
    HostL = length(Host),
    %% pattern based on the BNF grammer for the <hostname> rule
    Pattern =
	"(("
	  "([A-Za-z0-9])|"
	    "([A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])"
	")\\."
	")*"
	"("
	"([A-Za-z])|"
	  "([A-Za-z][A-Za-z0-9\\-]*[A-Za-z0-9])"
	")"
	"\\.?",

    R = regexp:first_match(Host, Pattern),
    %% throw exception if whole string doesn't match
    case R of
	{match, 1, HostL} -> true;
	_ -> false
    end.

is_hostname_throw(Host) ->
    case is_hostname(Host) of
	true ->
	    true;
	false ->
	    erlang:error(invalid_hostname, [Host])
    end.

%%--------------------------------------------------------------------
%% Function: is_IPv4address(Str)
%%           Str = string(), the host string
%% Descrip.: parse host string, throw an exception if it doesn't
%%           conform to the format specified in RFC3261 chapter 25.1
%%           page 218
%% Returns : Str | throw()
%% Note    : this function only matches IPv4 hostnames
%%--------------------------------------------------------------------
is_IPv4address(Host) ->
    case inet_parse:ipv4_address(Host) of
	{ok, _IPv4AsTuple} -> true;
	{error, _Reason} -> false
    end.

is_IPv4address_throw(Host) ->
    case is_IPv4address(Host) of
	true -> Host;
	false -> erlang:error(not_an_IPv4_address, [Host])
    end.



%%--------------------------------------------------------------------
%% Function: is_IPv6reference(IPv6Str)
%%           IPv6Str = string(), the host string
%% Descrip.: parse host string, throw an exception if it doesn't
%%           conform to the format specified in RFC3261 chapter 25.1
%%           page 218
%% Returns : true | false
%% Note    : this function only matches IPv6 hostnames
%%--------------------------------------------------------------------
is_IPv6reference(IPv6Str) ->
    case inet_parse:ipv6_address(IPv6Str) of
	{ok, _IPv6AsTuple} -> true;
	{error, _Reason} -> false
    end.

%% Returns : Str | throw()
is_IPv6reference_throw(IPv6Str) ->
    case is_IPv6reference(IPv6Str) of
	true -> IPv6Str;
	false -> erlang:error(not_an_IPv6_reference, [IPv6Str])
    end.


%%--------------------------------------------------------------------
%% Function: str_to_float(Str)
%% Descrip.: convert Str containing float() or integer() value, to a
%%           float().
%% Returns : float() |
%%           throw() (if Str can't be interpreted as a float)
%% Note    : user need to strip any preceding or trailing spaces (or
%%           other chars themselves)            XXX q_value(...) in contact.erl uses similar code
%%                                              XXX this should be in a utility module
%%--------------------------------------------------------------------
str_to_float(Str) ->
    case catch list_to_float(Str) of
	{'EXIT', _} ->
	    case catch list_to_integer(Str) of
		{'EXIT', _} ->
		    throw({error, string_not_float_or_integer});
		Int ->
		    float(Int)
	    end;
        Float ->
	    Float
    end.

%%--------------------------------------------------------------------
%% Function: str_to_qval(Str)
%% Descrip.: parse a qvalue string
%%           qvalue  = ( "0" [ "." 0*3DIGIT ] )
%%                   / ( "1" [ "." 0*3("0") ] )    - RFC 3261
%% Returns : float()
%%--------------------------------------------------------------------
%% extra clauses to ensure proper qvalue formating
str_to_qval("0") -> 0.0;
str_to_qval("1") -> 1.0;
str_to_qval("0.") -> 0.0;
str_to_qval("1.") -> 1.0;
str_to_qval("0." ++ _R = Str) -> str_to_qval2(Str);
str_to_qval("1." ++ _R = Str) -> str_to_qval2(Str);
str_to_qval(_) -> throw({error, malformed_beging_of_qvalue}).

str_to_qval2(Str) ->
    QVal = str_to_float(Str),
    case (QVal >= 0.0) and (QVal =< 1.0) of
	true -> StrLen = length(Str),
		case (StrLen >= 2) and (StrLen =< 5) of
		    true ->
			QVal;
		    false -> throw({error, wrong_number_of_chars_in_q_value})
		end;
	false ->
	    throw({error, q_value_out_of_range})
    end.

%%--------------------------------------------------------------------
%% Function: is_qval(Str)
%% Descrip.: parse a qvalue string
%%           qvalue  = ( "0" [ "." 0*3DIGIT ] )
%%                   / ( "1" [ "." 0*3("0") ] )    - RFC 3261
%% Returns : true | false
%%--------------------------------------------------------------------
is_qval(Str) ->
    try begin
	    str_to_qval(Str),
	    true
	end
    catch
	throw: _ -> false
    end.

%%--------------------------------------------------------------------
%% Function: strip(Str, Direction, StripChars)
%% Descrip.: works like string:strip/3 but can strip several types of
%%           char() at once
%% Returns : NewString = string()
%%--------------------------------------------------------------------
strip(Str, left, StripChars) ->
    strip_preceding(Str, StripChars);

strip(Str, right, StripChars) ->
    RStr = lists:reverse(Str),
    StrippedStr = strip_preceding(RStr, StripChars),
    lists:reverse(StrippedStr);

strip(Str, both, StripChars) ->
    LeftStrippedStr = strip(Str, left, StripChars),
    strip(LeftStrippedStr, right, StripChars).


%% strip_preceding(Str, CharList)
%%
strip_preceding("", _CharList) ->
    "";
strip_preceding([C | R] = Str, CharList) ->
    case lists:member(C, CharList) of
	true ->
	    strip_preceding(R, CharList);
	false ->
	    Str
    end.

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

%% only looks for exceptions from throw()
fail(Fun) ->
    try Fun() of
	_  -> throw({error, no_exception_thrown_by_test})
    catch
	_ -> ok %% catch user throw()
    end.

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->
    %% split_fields/2
    %%--------------------------------------------------------------------
    %% test regular case
    autotest:mark(?LINE, "split_fields/2 - 1"),
    {"foo", "bar"} = sipparse_util:split_fields("foo@bar", $@),
    %% test missplaced Separator
    autotest:mark(?LINE, "split_fields/2 - 2"),
    case catch sipparse_util:split_fields("@bar", $@) of
	{error, _} -> ok;
	_ -> throw({error, test_case_failed})
    end,
    %% test missplaced Separator
    autotest:mark(?LINE, "split_fields/2 - 3"),
    case catch sipparse_util:split_fields("foo@", $@) of
	{error, _} -> ok;
	_ -> throw({error, test_case_failed})
    end,
    %% test without Separator
    autotest:mark(?LINE, "split_fields/2 - 4"),
    {"foo"} = sipparse_util:split_fields("foo", $@),
    %% test with to many separators
    autotest:mark(?LINE, "split_fields/2 - 5"),
    case catch sipparse_util:split_fields("foo@@bar", $@) of
	{error, _} -> ok;
	_ -> throw({error, test_case_failed})
    end,
    %% test with only separator
    autotest:mark(?LINE, "split_fields/2 - 6"),
    case catch sipparse_util:split_fields("@", $@) of
	{error, _} -> ok;
	_ -> throw({error, test_case_failed})
    end,


    %% split_quoted_string/1
    %%--------------------------------------------------------------------
    %% regular test case
    autotest:mark(?LINE, "split_quoted_string/1 - 1"),
    {ok, "Foo Bar", " baz"} = split_quoted_string("\"Foo Bar\" baz"),

    %% nothing more except the quoted string
    autotest:mark(?LINE, "split_quoted_string/1 - 2"),
    {ok, "Foo Bar", ""} = split_quoted_string("\"Foo Bar\""),

    %% nested quotes
    autotest:mark(?LINE, "split_quoted_string/1 - 3"),
    {ok, "Foo Bar \\\"Baz\\\"", " x"} = split_quoted_string("\"Foo Bar \\\"Baz\\\"\" x"),

    %% nothing in between quotes
    autotest:mark(?LINE, "split_quoted_string/1 - 4"),
    {ok, "", " X"} = split_quoted_string("\"\" X"),

    %% does not start with quote
    autotest:mark(?LINE, "split_quoted_string/1 - 5"),
    try split_quoted_string("foo \"bar\"") of
	SQS_Res5 -> throw({error, test_case_failed, SQS_Res5})
    catch
	error: does_not_start_with_quote -> ok
    end,

    %% no ending quote
    %% do this second one as a 'case catch' instead of try/catch to avoid
    %% triggering a compiler bug in R10B-4
    autotest:mark(?LINE, "split_quoted_string/1 - 6"),
    case catch split_quoted_string("\"foo ") of
	{'EXIT', {no_end_quote, _}} -> ok;
	SQS_Res6 -> throw({error, test_case_failed, SQS_Res6})
    end,


    %% parse_hostport/1
    %%--------------------------------------------------------------------
    %% regular test case
    autotest:mark(?LINE, "parse_hostport/1 - 1"),
    {"foo.bar", 42} = sipparse_util:parse_hostport("foo.bar:42"),
    %% test trailing "." in host name
    autotest:mark(?LINE, "parse_hostport/1 - 2"),
    {"foo", 42} = sipparse_util:parse_hostport("foo.:42"),
    %% trailing "." and no port
    autotest:mark(?LINE, "parse_hostport/1 - 3"),
    {"foo", none} = sipparse_util:parse_hostport("foo."),
    %% IPv4 host and port
    autotest:mark(?LINE, "parse_hostport/1 - 4"),
    {"1.1.1.1", 42} = sipparse_util:parse_hostport("1.1.1.1:42"),
    %% IPv4 host and no port
    autotest:mark(?LINE, "parse_hostport/1 - 5"),
    {"1.1.1.1", none} = sipparse_util:parse_hostport("1.1.1.1"),
    %% IPv6 host and port
    autotest:mark(?LINE, "parse_hostport/1 - 6"),
    {"[1:2:3:4:5:6:7:8]", 42} = sipparse_util:parse_hostport("[1:2:3:4:5:6:7:8]:42"),
    %% IPv6 host and no port
    autotest:mark(?LINE, "parse_hostport/1 - 7"),
    {"[1:2:3:4:5:6:7:8]", none} = sipparse_util:parse_hostport("[1:2:3:4:5:6:7:8]"),
    %% IPv6 address not starting with digit
    autotest:mark(?LINE, "parse_hostport/1 - 8"),
    {"[ab:CD:3::5:6:7:8]", none} = sipparse_util:parse_hostport("[ab:CD:3::5:6:7:8]"),
    %% IPv6 host and no port, no brackets
    autotest:mark(?LINE, "parse_hostport/1 - 9"),
    {"[1:2:3:4:5:6:7:8]", none} = sipparse_util:parse_hostport("1:2:3:4:5:6:7:8"),
    %% IPv6 host and no port, no brackets
    autotest:mark(?LINE, "parse_hostport/1 - 10"),
    {"[abcd:2:3:4:5:6:7:8]", none} = sipparse_util:parse_hostport("abcd:2:3:4:5:6:7:8"),
    %% IPv6 host and no port, no brackets
    autotest:mark(?LINE, "parse_hostport/1 - 11"),
    {"[1:2::8]", none} = sipparse_util:parse_hostport("1:2::8"),
    %% IPv6 host and no port, no brackets
    autotest:mark(?LINE, "parse_hostport/1 - 12"),
    {"[abcd:2::6:7:8]", none} = sipparse_util:parse_hostport("abcd:2::6:7:8"),
    %% IPv6 host and no port, no brackets
    autotest:mark(?LINE, "parse_hostport/1 - 13"),
    {"[abcd:2::6:BCDE:8]", none} = sipparse_util:parse_hostport("abcd:2::6:BCDE:8"),
    %% test bad port value
    autotest:mark(?LINE, "parse_hostport/1 - 8"),
    case catch sipparse_util:parse_hostport("1.1.1.1:1A1") of
	{'EXIT', _} -> ok;
	{error, _} -> ok;
	_ -> throw({error, test_failed})
    end,
    %% test out of range IPv4 number
    autotest:mark(?LINE, "parse_hostport/1 - 9"),
    case catch sipparse_util:parse_hostport("1.1.300.1") of
	{'EXIT', _} -> ok;
	{error, _} -> ok;
	_ -> throw({error, test_failed})
    end,
    %% test bad domain name
    autotest:mark(?LINE, "parse_hostport/1 - 10"),
    case catch sipparse_util:parse_hostport("foo_bar.com") of
	{'EXIT', _} -> ok;
	{error, _} -> ok;
	_ ->
	    throw({error, test_failed})
    end,
    %% test out of range IPv6 number
    %% XXX I'm somewhat unsure about the format and range of IPv6
    autotest:mark(?LINE, "parse_hostport/1 - 11"),
    case catch sipparse_util:parse_hostport("[1:2:30000:4:5:6:7:8]") of
	{'EXIT', _} -> ok;
	{error, _} -> ok;
	_ ->
	    throw({error, test_failed})
    end,

    %% str_to_float/1
    %%--------------------------------------------------------------------
    %%
    autotest:mark(?LINE, "str_to_float/1  - 1"),
    1.0 = str_to_float("1.0"),
    autotest:mark(?LINE, "str_to_float/1  - 2"),
    1.0 = str_to_float("1"),
    autotest:mark(?LINE, "str_to_float/1  - 3"),
    1.0 = str_to_float("0001.0000"),
    autotest:mark(?LINE, "str_to_float/1  - 4"),
    fail(fun() -> str_to_float("foo") end),

    %% str_to_qval/1
    %%--------------------------------------------------------------------
    %% int = 1
    autotest:mark(?LINE, "str_to_qval/1  - 1"),
    1.0 = str_to_qval("1"),

    %% int = 0
    autotest:mark(?LINE, "str_to_qval/1  - 2"),
    0.0 = str_to_qval("0"),

    %% max float value
    autotest:mark(?LINE, "str_to_qval/1  - 3"),
    1.0 = str_to_qval("1.000"),

    %% min float value
    autotest:mark(?LINE, "str_to_qval/1  - 4"),
    0.0 = str_to_qval("0.000"),

    %% value in 0-1 range
    autotest:mark(?LINE, "str_to_qval/1  - 5"),
    0.567 = str_to_qval("0.567"),

    %% to large int
    autotest:mark(?LINE, "str_to_qval/1  - 6"),
    fail(fun() -> str_to_qval("3") end),

    %% to many chars
    autotest:mark(?LINE, "str_to_qval/1  - 7"),
    fail(fun() -> str_to_qval("0.1234") end),

    %% float out of range
    autotest:mark(?LINE, "str_to_qval/1  - 8"),
    fail(fun() -> str_to_qval("1.001") end),

    %% missing 0 before .00
    autotest:mark(?LINE, "str_to_qval/1  - 9"),
    fail(fun() -> str_to_qval(".00") end),

    %% to many 0 before .0
    autotest:mark(?LINE, "str_to_qval/1  - 10"),
    fail(fun() -> str_to_qval("00.0") end),

    %% parse "0."
    autotest:mark(?LINE, "str_to_qval/1  - 11"),
    0.0 = str_to_qval("0."),

    %% parse "1."
    autotest:mark(?LINE, "str_to_qval/1  - 12"),
    1.0 = str_to_qval("1."),

    %% is_qval/1
    %%--------------------------------------------------------------------
    %%
    %% int = 1
    autotest:mark(?LINE, "is_qval/1  - 1"),
    true = is_qval("1"),

    %% int = 0
    autotest:mark(?LINE, "is_qval/1  - 2"),
    true = is_qval("0"),

    %% max float value
    autotest:mark(?LINE, "is_qval/1  - 3"),
    true = is_qval("1.000"),

    %% min float value
    autotest:mark(?LINE, "is_qval/1  - 4"),
    true = is_qval("0.000"),

    %% value in 0-1 range
    autotest:mark(?LINE, "is_qval/1  - 5"),
    true = is_qval("0.567"),

    %% to large int
    autotest:mark(?LINE, "is_qval/1  - 6"),
    false = is_qval("3"),

    %% to many chars
    autotest:mark(?LINE, "is_qval/1  - 7"),
    false = is_qval("0.1234"),

    %% float out of range
    autotest:mark(?LINE, "is_qval/1  - 8"),
    false = is_qval("1.001"),

    %% missing 0 before .00
    autotest:mark(?LINE, "is_qval/1  - 9"),
    false = is_qval(".00"),

    %% to many 0 before .0
    autotest:mark(?LINE, "is_qval/1  - 10"),
    false = is_qval("00.0"),

    %% parse "0."
    autotest:mark(?LINE, "str_to_qval/1  - 11"),
    true = is_qval("0."),

    %% parse "1."
    autotest:mark(?LINE, "str_to_qval/1  - 12"),
    true = is_qval("1."),

    %% strip
    %%--------------------------------------------------------------------
    %% test left strip
    autotest:mark(?LINE, "strip/3 - 1"),
    "abc" = strip("+-+-+-+-abc", left, "-+"),

    %% test left strip without strip char
    autotest:mark(?LINE, "strip/3 - 2"),
    "abc" = strip("abc", left, "-+"),

    %% test left strip on empty string
    autotest:mark(?LINE, "strip/3 - 3"),
    "" = strip("", left, "-+"),

    %% test left strip with no strip chars
    autotest:mark(?LINE, "strip/3 - 4"),
    "abc" = strip("abc", left, ""),

    %% test left strip with strip char matching chars inside string to strip
    autotest:mark(?LINE, "strip/3 - 5"),
    "a+-+-+-+-abc" = strip("a+-+-+-+-abc", left, "-+"),

    %% test left strip with strip char matching chars on right end
    autotest:mark(?LINE, "strip/3 - 6"),
    "abc++++" = strip("abc++++", left, "-+"),

    %% --------------------
    %% test right strip
    autotest:mark(?LINE, "strip/3 - 7"),
    "abc" = strip("abc+-+-+-+-", right, "-+"),

    %% test right strip without strip char
    autotest:mark(?LINE, "strip/3 - 8"),
    "abc" = strip("abc", right, "-+"),

    %% test right strip on empty string
    autotest:mark(?LINE, "strip/3 - 9"),
    "" = strip("", right, "-+"),

    %% test right strip with no strip chars
    autotest:mark(?LINE, "strip/3 - 10"),
    "abc" = strip("abc", right, ""),

    %% test right strip with strip char matching chars inside string to strip
    autotest:mark(?LINE, "strip/3 - 11"),
    "a+-+-+-+-abc" = strip("a+-+-+-+-abc", right, "-+"),

    %% test right strip with strip char matching chars on left end
    autotest:mark(?LINE, "strip/3 - 12"),
    "++++abc" = strip("++++abc", right, "-+"),

    %% --------------------
    %% test both strip

    %% test strip from left and right
    autotest:mark(?LINE, "strip/3 - 13"),
    "abc" = strip("+-+++abc-+---++", both, "-+"),

    %% test when strip chars are inside string
    autotest:mark(?LINE, "strip/3 - 14"),
    "abc++++abc" = strip("abc++++abc", both, "-+"),

    %% test with empty string
    autotest:mark(?LINE, "strip/3 - 15"),
    "" = strip("", both, "-+"),

    %% test with no strip chars
    autotest:mark(?LINE, "strip/3 - 16"),
    "+abc-" = strip("+abc-", both, ""),


    ok.


