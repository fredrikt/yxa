-module(dnsutil).
-export([siplookup/1, enumlookup/1, get_ip_port/2]).

-include("inet_dns.hrl").

srvlookup(Proto, Name) ->
    case inet_res:nslookup(Name, in, srv) of
	{ok, Rec} ->
	    ParseSRV = fun(Entry) ->
			       {Proto, Entry#dns_rr.data}
			  end,
	    lists:map(ParseSRV, Rec#dns_rec.anlist);
	{error, What} ->
	    logger:log(debug, "dns resolver: Error ~p when resolving ~p IN SRV", [What, Name]),
	    {error, What}
    end.

siplookup([]) ->
    none;
siplookup(Domain) ->
    TCP = srvlookup(tcp, "_sip._tcp." ++ Domain),
    UDP = srvlookup(udp, "_sip._udp." ++ Domain),
    combine_srvresults(lists:sort(fun sortsrv/2, lists:flatten([TCP, UDP]))).

combine_srvresults(List) ->
    combine_srvresults(List, [], []).

combine_srvresults([], [], Errors) ->
    Errors;
combine_srvresults([], Res, _) ->
    Res;
combine_srvresults([{error, What} | T], Res, Errors) ->
    combine_srvresults(T, Res, lists:append(Errors, [{error, What}]));
combine_srvresults([{Proto, {_, _, Port, Host}} | T], Res, Errors) ->
    combine_srvresults(T, lists:append(Res, [{Proto, Host, Port}]), Errors).

sortsrv({_, {Order1, Preference1, Port1, Host1}}, {_, {Order2, Preference2, Port2, Host2}}) ->
    if
	Order1 < Order2 ->
	    true;
	Order1 == Order2 ->
	    if
		Preference1 < Preference2 ->
		    true;
		Preference1 == Preference2 ->
		    %% This string sorting is just to make the process deterministic
		    %% so that a stateless proxy always chooses the same destination
		    Str1 = Host1 ++ ":" ++ integer_to_list(Port1),
		    Str2 = Host2 ++ ":" ++ integer_to_list(Port2),
		    case lists:min([Str1, Str2]) of
			Str1 ->
			    true;
			_ ->
			    false
		    end;
		true ->
		    false
	    end;
	true ->
	    false
    end;
%% SRV record always beats error
sortsrv({SipProto, {Order, Preference, Port, Host}}, {error, _}) ->
    true;
%% SRV record always beats error
sortsrv({error, _}, {SipProto, {Order, Preference, Port, Host}}) ->
    false;
%% NXDOMAIN always beats other errors
sortsrv({error, nxdomain}, {error, _}) ->
    true;
%% make errors come last
sortsrv({error, _}, {error, _}) ->
    false.

get_ip_port(Host, Port) when list(Port) ->
    get_ip_port(Host, list_to_integer(Port));
get_ip_port(Host, Port) when integer(Port) ; Port == none ->
    V6List = case sipserver:get_env(enable_v6, false) of
		 true ->
		     %% XXX inet:getaddr with proto inet6 is not documented, check that it is supported.
		     Res6 = inet:getaddr(Host, inet6),
		     case Res6 of
			 {error, What6} ->
			     logger:log(debug, "dns resolver: Error ~p when resolving ~p inet6", [What6, Host]),
			     [];
			 {ok, {0, 0, 0, 0, 0, 65535, _, _}} ->
			     %% IPv4-mapped address, ignore
			     %% XXX optimize so that we don't query again for v4 address below?
			     [];
			 {ok, V6Addr} ->
			     [{siphost:makeip(V6Addr), V6Addr, Port}]
		     end;
		 _ ->
		     %% IPv6 is disabled
		     []
	     end,
    Res = inet:getaddr(Host, inet),
    case Res of
	{error, What4} ->
	    logger:log(debug, "dns resolver: Error ~p when resolving ~p inet", [What4, Host]),
	    case V6List of
		[] ->
		    {error, What4};
		_ ->
		    V6List
	    end;
	{ok, {A1,A2,A3,A4}} ->
	    V4Addr = {A1,A2,A3,A4},
	    V4List = [{siphost:makeip(V4Addr), V4Addr, Port}],
	    lists:append(V6List, V4List)
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
	    % SIP+E2U is RFC2916 and E2U+SIP is 2916bis
	    L2 = lists:append(chooseenum(L1, "SIP+E2U"), chooseenum(L1, "E2U+SIP")),
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
