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
    is_IPv6reference(IPv6Ref),
    %% XXX return without brackets?
    {ok, "[" ++ IPv6Ref ++ "]", Port};
parse_ipv6hostport(In) ->
    %% throw if not ip6 ref
    is_IPv6reference(In),
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
	    {ok, is_IPv4address(Host), list_to_integer(Port)};
	{Host} ->
	    %% throw if not a IPv4 address string
	    {ok, is_IPv4address(Host), none}
    end.


parse_domain_hostport(HostPort) ->
    case sipparse_util:split_fields(HostPort, $:) of
	{Host, Port} ->
	    %% throw if not a hostname
	    is_hostname(Host),
	    {ok, rm_trailing_dot_from_hostname(Host), list_to_integer(Port)};
	{Host} ->
	    %% throw if not a hostname
	    is_hostname(Host),
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
%% Descrip.: parse host string, throw a exception if it doesn't
%%           conform to the format specified in RFC3261 chapter 25.1
%%           p218
%% Returns : Str | throw()
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
	"((([A-Za-z0-9])|([A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9]))\\.)*"
	"(([A-Za-z])|([A-Za-z][A-Za-z0-9\\-]*[A-Za-z0-9]))\\.?",

    R = regexp:first_match(Host, Pattern),
    %% throw exception if whole string doesn't match
    {match, 1, HostL} = R,
    Host.


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
	{ok, _IPv4AsTuple} -> Host;
	{error, _Reason} -> throw({error, not_a_IPv4_address})
    end.

%%--------------------------------------------------------------------
%% Function: is_IPv6refernce(Str)
%%           Str = string(), the host string
%% Descrip.: parse host string, throw an exception if it doesn't
%%           conform to the format specified in RFC3261 chapter 25.1
%%           page 218
%% Returns : Str | throw()
%% Note    : this function only matches IPv6 hostnames
%%--------------------------------------------------------------------
is_IPv6reference(IPv6Str) ->
    case inet_parse:ipv6_address(IPv6Str) of
	{ok, _IPv6AsTuple} -> IPv6Str;
	{error, _Reason} -> throw({error, not_an_IPv6_reference})
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

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->
    %% split_fields/2
    %%--------------------------------------------------------------------
    %% test regular case
    io:format("test: split_fields/2 - 1~n"),
    {"foo", "bar"} = sipparse_util:split_fields("foo@bar", $@),
    %% test missplaced Separator
    io:format("test: split_fields/2 - 2~n"),
    case catch sipparse_util:split_fields("@bar", $@) of
	{error, _} -> ok;
	_ -> throw({error, test_case_failed})
    end,
    %% test missplaced Separator
    io:format("test: split_fields/2 - 3~n"),
    case catch sipparse_util:split_fields("foo@", $@) of
	{error, _} -> ok;
	_ -> throw({error, test_case_failed})
    end,
    %% test without Separator
    io:format("test: split_fields/2 - 4~n"),
    {"foo"} = sipparse_util:split_fields("foo", $@),
    %% test with to many separators
    io:format("test: split_fields/2 - 5~n"),
    case catch sipparse_util:split_fields("foo@@bar", $@) of
	{error, _} -> ok;
	_ -> throw({error, test_case_failed})
    end,
    %% test with only separator
    io:format("test: split_fields/2 - 6~n"),
    case catch sipparse_util:split_fields("@", $@) of
	{error, _} -> ok;
	_ -> throw({error, test_case_failed})
    end,

    %% parse_hostport/1
    %%--------------------------------------------------------------------
    %% regular test case
    io:format("test: parse_hostport/1 - 1~n"),
    {"foo.bar", 42} = sipparse_util:parse_hostport("foo.bar:42"),
    %% test trailing "." in host name
    io:format("test: parse_hostport/1 - 2~n"),
    {"foo", 42} = sipparse_util:parse_hostport("foo.:42"),
    %% trailing "." and no port
    io:format("test: parse_hostport/1 - 3~n"),
    {"foo", none} = sipparse_util:parse_hostport("foo."),
    %% IPv4 host and port
    io:format("test: parse_hostport/1 - 4~n"),
    {"1.1.1.1", 42} = sipparse_util:parse_hostport("1.1.1.1:42"),
    %% IPv4 host and no port
    io:format("test: parse_hostport/1 - 5~n"),
    {"1.1.1.1", none} = sipparse_util:parse_hostport("1.1.1.1"),
    %% IPv6 host and port
    io:format("test: parse_hostport/1 - 6~n"),
    {"[1:2:3:4:5:6:7:8]", 42} = sipparse_util:parse_hostport("[1:2:3:4:5:6:7:8]:42"),
    %% IPv6 host and no port
    io:format("test: parse_hostport/1 - 7~n"),
    {"[1:2:3:4:5:6:7:8]", none} = sipparse_util:parse_hostport("[1:2:3:4:5:6:7:8]"),
    %% IPv6 address not starting with digit
    io:format("test: parse_hostport/1 - 8~n"),
    {"[ab:CD:3::5:6:7:8]", none} = sipparse_util:parse_hostport("[ab:CD:3::5:6:7:8]"),
    %% IPv6 host and no port, no brackets
    io:format("test: parse_hostport/1 - 9~n"),
    {"[1:2:3:4:5:6:7:8]", none} = sipparse_util:parse_hostport("1:2:3:4:5:6:7:8"),
    %% IPv6 host and no port, no brackets
    io:format("test: parse_hostport/1 - 10~n"),
    {"[abcd:2:3:4:5:6:7:8]", none} = sipparse_util:parse_hostport("abcd:2:3:4:5:6:7:8"),
    %% IPv6 host and no port, no brackets
    io:format("test: parse_hostport/1 - 11~n"),
    {"[1:2::8]", none} = sipparse_util:parse_hostport("1:2::8"),
    %% IPv6 host and no port, no brackets
    io:format("test: parse_hostport/1 - 12~n"),
    {"[abcd:2::6:7:8]", none} = sipparse_util:parse_hostport("abcd:2::6:7:8"),
    %% IPv6 host and no port, no brackets
    io:format("test: parse_hostport/1 - 13~n"),
    {"[abcd:2::6:BCDE:8]", none} = sipparse_util:parse_hostport("abcd:2::6:BCDE:8"),
    %% test bad port value
    io:format("test: parse_hostport/1 - 8~n"),
    case catch sipparse_util:parse_hostport("1.1.1.1:1A1") of
	{'EXIT', _} -> ok;
	{error, _} -> ok;
	_ -> throw({error, test_failed})
    end,
    %% test out of range IPv4 number
    io:format("test: parse_hostport/1 - 9~n"),
    case catch sipparse_util:parse_hostport("1.1.300.1") of
	{'EXIT', _} -> ok;
	{error, _} -> ok;
	_ -> throw({error, test_failed})
    end,
    %% test bad domain name
    io:format("test: parse_hostport/1 - 10~n"),
    case catch sipparse_util:parse_hostport("foo_bar.com") of
	{'EXIT', _} -> ok;
	{error, _} -> ok;
	_ ->
	    throw({error, test_failed})
    end,
    %% test out of range IPv6 number
    %% XXX I'm somewhat unsure about the format and range of IPv6
    io:format("test: parse_hostport/1 - 11~n"),
    case catch sipparse_util:parse_hostport("[1:2:30000:4:5:6:7:8]") of
	{'EXIT', _} -> ok;
	{error, _} -> ok;
	_ ->
	    throw({error, test_failed})
    end,

    ok.

