-module(dnsutil).
-export([siplookup/1, enumlookup/1, get_ip_port/2]).

-include("inet_dns.hrl").

srvlookup(Name) ->
    case inet_res:nslookup(Name, in, srv) of
	{ok, Rec} ->
	    ParseSRV = fun(Entry) ->
			       Entry#dns_rr.data
			  end,
	    lists:map(ParseSRV, Rec#dns_rec.anlist);
	{error, What} ->
	    logger:log(debug, "dns resolver: Error ~p when resolving ~p IN SRV", [What, Name]),
	    {error, What}
    end.

siplookup([]) ->
    none;
siplookup(Domain) ->
    case regexp:first_match(Domain, "^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$") of
	{match, _, _} ->
	    logger:log(debug, "dns resolver: ~p is an IP address, not performing SRV lookup", [Domain]),
	    none;
	_ ->
	    case srvlookup("_sip._udp." ++ Domain) of
		[] ->
		    none;
		{error, What} ->
		    {error, What};
		[{_, _, Port, Host} | _] ->
		    {Host, Port}
	    end
    end.

get_ip_port(Host, Port) ->
    Res = inet:getaddr(Host, inet),
    case Res of
	{error, What} ->
	    logger:log(debug, "dns resolver: Error ~p when resolving ~p inet", [What, Host]),
	    {error, What};
	{ok, IP} ->
	    {IP, Port}
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
    logger:log(debug, "Resolver: applyregexp: IN ~p ~p", [Number, Regexp]),
    Rewrite = case string:tokens(Regexp, "!") of
		  [Lhs, Rhs] ->
		      NLhs = fixplus(Lhs),
		      Res = util:regexp_rewrite(Number, [{NLhs, Rhs}]),
		      logger:log(debug, "Resolver: applyregexp: OUT ~p", [Res]),
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
    case sipserver:get_env(enum_domainlist, none) of
	none ->
	    logger:log(debug, "Resolver: Not performing ENUM lookup on ~p since enum_domainlist is empty", ["+" ++ Number]),
	    none;
	DomainList ->
	    logger:log(debug, "Resolver: ENUM query for ~p in ~p", ["+" ++ Number, DomainList]),
	    L1 = enumalldomains(Number, DomainList),
	    L2 = chooseenum(L1, "SIP+E2U"),
	    L3 = lists:sort(fun sortenum/2, L2),
	    L4 = enumregexp(L3),
	    applyregexp("+" ++ Number, L4)
    end;
enumlookup(Foo) ->
    logger:log(error, "Resolver: ENUM lookup on non-E.164 number (~p)", [Foo]),
    none.

isnaptr(Entry) ->
    if
	Entry#dns_rr.type == ?T_NAPTR ->
	    true;
	true ->
	    false
    end.

naptrlookup(Name) ->
    case inet_res:nslookup(Name, in, ?T_NAPTR) of
	{ok, Rec} ->
	    ParseNAPTR = fun(Entry) ->
				 parsenaptr(Entry#dns_rr.data)
			 end,
	    NAPTRs = lists:map(ParseNAPTR, lists:filter(fun isnaptr/1,
					       Rec#dns_rec.anlist)),
	    logger:log(debug, "Resolver: naptrlookup: ~p -> found", [Name]),
	    NAPTRs;
	{error, E} ->
	    logger:log(debug, "Resolver: naptrlookup: ~p -> error ~p", [Name, E]),
	    []
    end.

parsenaptr(Record) ->
    Binary = list_to_binary(Record),
    <<Order:16,
    Preference:16,
    Flagslength:8,
    Rest/binary>> = Binary,
    <<Flags:Flagslength/binary-unit:8,
    Serviceslength:8,
    Rest2/binary>> = Rest,
    <<Services:Serviceslength/binary-unit:8,
    Regexplength:8,
    Rest3/binary>> = Rest2,
    <<Regexp:Regexplength/binary-unit:8,
    Replacementlength:8,
    Rest4/binary>> = Rest3,
    <<Replacement:Replacementlength/binary-unit:8>> = Rest4,
    {Order,
     Preference,
     binary_to_list(Flags),
     httpd_util:to_upper(binary_to_list(Services)),
     binary_to_list(Regexp),
     binary_to_list(Replacement)}.
