-module(dnsutil).
-export([siplookup/1, enumlookup/1]).

-include("inet_dns.hrl").

srvlookup(Name) ->
    case inet_res:nslookup(Name, in, srv) of
	{ok, Rec} ->
	    ParseSRV = fun(Entry) ->
			       Entry#dns_rr.data
			  end,
	    lists:map(ParseSRV, Rec#dns_rec.anlist);
	{error, nxdomain} ->
	    []
    end.

siplookup(Domain) ->
    case srvlookup("_sip._udp." ++ Domain) of
	[] ->
	    none;
	[{_, _, Port, Host} | _] ->
	    {Host, Port}
    end.

number2enum([], Domain) ->
    Domain;

number2enum([C | Rest], Domain) ->
    [C, $. | number2enum(Rest, Domain)].

fixplus("^+" ++ Regexp) ->
    logger:log(debug,"applyregexp: E.164 subscriber has used an invalid regexp, working around ^+ escape-problem"),
    "^\\+" ++ Regexp;
fixplus(Regexp) ->
    Regexp.

applyregexp(Number, []) ->
    none;
applyregexp(Number, [Regexp | Rest]) ->
    logger:log(debug, "applyregexp: IN ~p ~p~n", [Number, Regexp]),
    Rewrite = case string:tokens(Regexp, "!") of
		  [Lhs, Rhs] ->
		      NLhs = fixplus(Lhs),
		      Res = util:regexp_rewrite(Number, [{NLhs, Rhs}]),
		      logger:log(debug, "applyregexp: OUT ~p~n", [Res]),
		      Res;
		  _ ->
		      nomatch
	      end,
    case Rewrite of
	nomatch ->
	    applyregexp(Number, Rest);
	_ ->
	    Rewrite
    end.

enumalldomains(Number, []) ->
    [];

enumalldomains(Number, [Domain | Rest]) ->
    naptrlookup(number2enum(lists:reverse(Number), Domain)) ++
	enumalldomains(Number, Rest).

chooseenum([], Type) ->
    [];
chooseenum([Elem | Rest], Type) ->
    case Elem of
	{Order, Preference, "u", Type, Regexp, ""} ->
	    [{Order, Preference, Regexp} | chooseenum(Rest, Type)];
	_ ->
	    chooseenum(Rest, Type)
    end.

sortenum({Order1, Preference1, Regexp1}, {Order2, Preference2, Regexp2}) ->
    if
	Order1 < Order2 ->
	    true;
	true ->
	    false
    end.

enumregexp([]) ->
    [];
enumregexp([{Order1, Preference1, Regexp1} | Rest]) ->
    [Regexp1 | enumregexp(Rest)].

enumlookup(none) ->
    none;
enumlookup("+" ++ Number) ->
    L1 = enumalldomains(Number, ["e164.sunet.se", "e164.arpa"]),
    L2 = chooseenum(L1, "SIP+E2U"),
    L3 = lists:sort(fun sortenum/2, L2),
    L4 = enumregexp(L3),
    applyregexp("+" ++ Number, L4);
enumlookup(Number) ->
    none.

naptrlookup(Name) ->
    logger:log(debug, "naptrlookup: ~p~n", [Name]),
    case inet_res:nslookup(Name, in, 35) of
	{ok, Rec} ->
	    ParseNAPTR = fun(Entry) ->
				 parsenaptr(Entry#dns_rr.data)
			 end,
	    lists:map(ParseNAPTR, Rec#dns_rec.anlist);
	{error, nxdomain} ->
	    []
    end.

b2i(Binary) ->
    [I] = binary_to_list(Binary),
    I.

parsenaptr(Record) ->
    Binary = list_to_binary(Record),
    {Order, Rest1} = split_binary(Binary, 2),
    {Preference, Rest2} = split_binary(Rest1, 2),
    {Flagslength, Rest3} = split_binary(Rest2, 1),
    {Flags, Rest4} = split_binary(Rest3, b2i(Flagslength)),
    {Serviceslength, Rest5} = split_binary(Rest4, 1),
    {Services, Rest6} = split_binary(Rest5, b2i(Serviceslength)),
    {Regexplength, Rest7} = split_binary(Rest6, 1),
    {Regexp, Rest8} = split_binary(Rest7, b2i(Regexplength)),
    {Replacementlength, Rest9} = split_binary(Rest8, 1),
    {Replacement, Rest10} = split_binary(Rest9, b2i(Replacementlength)),
    [O1, O2] = binary_to_list(Order),
    [P1, P2] = binary_to_list(Preference),
    {O1 * 256 + O2,
     P1 * 256 + P2,
     binary_to_list(Flags),
     httpd_util:to_upper(binary_to_list(Services)),
     binary_to_list(Regexp),
     binary_to_list(Replacement)}.
