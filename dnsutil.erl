-module(dnsutil).
-export([siplookup/1, enumlookup/1, enumlookup/2, get_ip_port/2]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/src/inet_dns.hrl").

%% For debugging
%%-compile(export_all).

%% These records is entirely internal to this module
-record(srventry, {proto, dnsrrdata}).
-record(naptrrecord, {order, preference, flags, services, regexp, replacement}).

%% The DNS NAPTR RR type code. Other RR type codes are defined in OTP inet_dns.hrl.
-define(T_NAPTR, 35).

%%--------------------------------------------------------------------
%% Function: srvlookup/2
%% Description: Resolve a SRV record (indicated by Name) and return a
%%              list of srventry records. The DNS RR data in them is
%%              of the format {Order, Pref, Port, Host}.
%% Returns: SRVList         |
%%          {error, Reason}
%%--------------------------------------------------------------------
srvlookup(Proto, Name) when list(Name) ->
    case inet_res:nslookup(Name, in, srv) of
	{ok, Rec} ->
	    ParseSRV = fun(Entry) ->
			       #srventry{proto=Proto, dnsrrdata=Entry#dns_rr.data}
		       end,
	    lists:map(ParseSRV, Rec#dns_rec.anlist);
	{error, What} ->
	    logger:log(debug, "dns resolver: Error ~p when resolving ~p IN SRV", [What, Name]),
	    {error, What}
    end.


%%--------------------------------------------------------------------
%% Function: siplookup/1
%% Description: Look up where to send SIP requests for a domain name.
%%              Ultimately return a list of {Proto, Host, Port} and/or
%%              {error, E} entrys. Proto is tcp, udp or tls. Never
%%              tcp6, udp6 or tls6 since we do not resolve Host into
%%              an address.
%% Returns: ProtoHostPortList
%%--------------------------------------------------------------------
siplookup([]) ->
    none;
siplookup(Domain) ->
    case siplookup_naptr(Domain) of
	nomatch ->
	    %% No domain NAPTR records found. Perform RFC2543 backwards compatible
	    %% SRV lookup of the domain.
	    logger:log(debug, "Resolver: Domain ~p has no NAPTR records, perform RFC2543 backwards " ++
		       "compatible lookup for SIP SRV records under that domain name", [Domain]),
	    TCP = srvlookup(tcp, "_sip._tcp." ++ Domain),
	    UDP = srvlookup(udp, "_sip._udp." ++ Domain),
	    TLS = srvlookup(tls, "_sips._tcp." ++ Domain),
	    R = combine_srvresults(lists:sort(fun sortsrv/2, lists:flatten([TCP, UDP, TLS]))),
	    logger:log(debug, "Resolver: Result of RFC2543 compatible SIP SRV query for domain ~p :~n~p",
		       [Domain, R]),
	    R;
	Res ->
	    %% siplookup_naptr does it's own sorting
	    combine_srvresults(Res)
    end.

%%--------------------------------------------------------------------
%% Function: siplookup_naptr/1
%% Description: Look for NAPTR records for a domain telling us where
%%              to look for SRV records for the very same domain.
%% Returns: SRVList |
%%          nomatch
%%--------------------------------------------------------------------
siplookup_naptr(Domain) when list(Domain) ->
    case naptrlookup(Domain) of
	[] ->
	    nomatch;
	L1 ->
	    L2 = lists:append([filter_naptr(L1, "s", "SIP+D2U"), filter_naptr(L1, "s", "SIP+D2T"),
			      filter_naptr(L1, "s","SIPS+D2T")]),
	    L3 = lists:sort(fun sortenum/2, L2),
	    %% Ok, we now have a preference sorted list of NAPTR results. Look for
	    %% SRV records matching each of them. Sort SRV records only per NAPTR -
	    %% the NAPTR order of preference supersedes the SRV ordering.
	    naptr_to_srvlist(Domain, L3)
    end.


%%--------------------------------------------------------------------
%% Function: siplookup_naptr/1
%% Descrip.: Take a list of sorted NAPTR records and try to turn
%%           them into SRV records.
%% Returns : SRVList |
%%           nomatch
%%--------------------------------------------------------------------
naptr_to_srvlist(Domain, In) ->
    naptr_to_srvlist2(Domain, In, []).

naptr_to_srvlist2(Domain, [], Res) ->
    logger:log(debug, "dns resolver: Domain ~p NAPTR (+SRV) lookup result :~n~p",
	       [Domain, debugfriendly_srventry(Res)]),
    Res;
naptr_to_srvlist2(Domain, [H | Rest], Res) when record(H, naptrrecord) ->
    Proto = case H#naptrrecord.services of
		"SIP+D2U"  ++ _ -> udp;
		"SIP+D2T"  ++ _ -> tcp;
		"SIPS+D2T" ++ _ -> tls
	    end,
    %% Regexp should be empty for domain NAPTR - RFC3263 #4.1. The
    %% interesting thing is the replacement.
    This = srvlookup(Proto, H#naptrrecord.replacement),
    Sorted = lists:sort(fun sortsrv/2, This),
    naptr_to_srvlist2(Domain, Rest, lists:flatten([Res, Sorted])).

debugfriendly_srventry(In) when is_list(In) ->
    debugfriendly_srventry2(In, []).

debugfriendly_srventry2([], Res) ->
    lists:reverse(Res);
debugfriendly_srventry2([H|T], Res) when is_record(H, srventry) ->
    {P, W, Port, Host} = H#srventry.dnsrrdata,
    This = lists:flatten(lists:concat([H#srventry.proto, ", priority=", P, ", weight=", W,
				       " ", Host, ":", Port])),
    debugfriendly_srventry2(T, [This | Res]).

%%--------------------------------------------------------------------
%% Function: combine_srvresults(In)
%%           In = list() of srventry record() | {error, R} tuples
%% Descrip.: Walk through a list of srventry records or {error, R}
%%           tuples and turn the srventry records into
%%           {Proto, Host, Port} tuples.
%% Returns : ProtoHostPortList
%%           ProtoHostPortList = list() of {Proto, Host, Port}
%%           Proto = tcp | udp | tls
%%           Host  = string()
%%           Port  = integer()
%%--------------------------------------------------------------------
combine_srvresults(In) ->
    combine_srvresults(In, [], []).

combine_srvresults([], [], Errors) ->
    Errors;
combine_srvresults([], Res, _) ->
    Res;
combine_srvresults([{error, What} | T], Res, Errors) ->
    combine_srvresults(T, Res, lists:append(Errors, [{error, What}]));
combine_srvresults([H | T], Res, Errors) when record(H, srventry) ->
    {_, _, Port, Host} = H#srventry.dnsrrdata,
    combine_srvresults(T, lists:append(Res, [{H#srventry.proto, Host, Port}]), Errors).


%%--------------------------------------------------------------------
%% Function: sortsrv(A, B)
%%           A = srventry record()
%%           B = srventry record()
%% Descrip.: Walk through a list of srventry records or {error, R}
%%           tuples and sort them according to order and secondly by
%%           weight. Sort errors last, but put nxdomain errors first
%%           of the errors.
%% Returns : list() of srventry record()
%% Note    : XXX weight should be proportional somehow, not just
%%           sorted
%%--------------------------------------------------------------------
sortsrv(A, B) when record(A, srventry), record(B, srventry) ->
    {Order1, Weight1, _Port1, _Host1} = A#srventry.dnsrrdata,
    {Order2, Weight2, _Port2, _Host2} = B#srventry.dnsrrdata,
    if
	Order1 < Order2 ->
	    true;
	Order1 == Order2 ->
	    %% XXX weight should be proportional somehow, not just sorted
	    if
		Weight1 > Weight2 ->
		    true;
		Weight1 == Weight2 ->
		    true;
		true ->
		    false
	    end;
	true ->
	    false
    end;
%% SRV record always beats error
sortsrv(A, {error, _}) when record(A, srventry) ->
    true;
%% SRV record always beats error
sortsrv({error, _}, B) when record(B, srventry)->
    false;
%% NXDOMAIN always beats other errors
sortsrv({error, nxdomain}, {error, _}) ->
    true;
%% make errors come last
sortsrv({error, _}, {error, _}) ->
    false.


%%--------------------------------------------------------------------
%% Function: get_ip_port/2
%% Description: Simply look up the hostname supplied and return a list
%%              of IP addresses and the port. Port may be a list when
%%              supplied to us, but we will turn it into an integer.
%%              Port may also be 'none'.
%% Returns: HostPortList
%%--------------------------------------------------------------------
get_ip_port(Host, Port) when list(Port) ->
    get_ip_port(Host, list_to_integer(Port));
get_ip_port(Host, Port) when integer(Port) ; Port == none ->
    V6List = case sipserver:get_env(enable_v6, true) of
		 true ->
		     %% XXX inet:gethostbyname with proto inet6 is not documented, and not supported.
		     case get_ip_port2(inet6, Host, Port) of
			 {error, _} ->
			     %% Ignore v6 errors (they are logged in get_ip_port2 though)
			     [];
			 Res ->
			     Res
		     end;
		 _ ->
		     %% IPv6 is disabled
		     []
	     end,
    case get_ip_port2(inet, Host, Port) of
	{error, What4} ->
	    logger:log(debug, "Resolver: Error ~p when resolving ~p inet", [What4, Host]),
	    case V6List of
		[] ->
		    %% v6 list is empty (disabled or failed) and v4 failed
		    {error, What4};
		_ ->
		    V6List
	    end;
	V4List when list(V4List) ->
	    lists:append(V6List, V4List)
    end.

get_ip_port2(Family, Host, Port) when Family == inet ; Family == inet6 ->
    %% XXX inet:gethostbyname with proto inet6 is not documented, and not supported.
    case inet:gethostbyname(Host, Family) of
	{error, What} ->
	    logger:log(debug, "Resolver: Error ~p when resolving ~p ~p", [What, Host, Family]),
	    {error, What};
	{ok, HostEnt} when record(HostEnt, hostent) ->
	    Res = lists:map(fun(Addr) ->
				    case Addr of
					{0, 0, 0, 0, 0, 65535, _, _} ->
					    %% IPv4 mapped IPv6 address, ignore - we resolve
					    %% IPv4 addresses separately
					    [];
					_ ->
					    {Family, siphost:makeip(Addr), Port}
				    end
			    end, HostEnt#hostent.h_addr_list),
	    lists:flatten(Res)
    end.


%%--------------------------------------------------------------------
%% Function: number2enum
%% Description: Insert dots between all digits in the input. The input
%%              is the original number in reverse.
%% Returns: ENUMstr
%%--------------------------------------------------------------------
number2enum([], Domain) ->
    Domain;

number2enum([C | Rest], Domain) ->
    [C, $. | number2enum(Rest, Domain)].


%%--------------------------------------------------------------------
%% Function: fixplus/1
%% Description: ENUM users frequently put regexps starting with ^+ in
%%              their NAPTR records (because of examples in RFC2916).
%%              Erlang (R9C-0) regexp module can't handle those (it
%%              loops forever) so we turn them into ^\\+ which works.
%% Returns: NewRegexp
%%--------------------------------------------------------------------
fixplus("^+" ++ Regexp) ->
    logger:log(debug,"Resolver: applyregexp: E.164 subscriber has used an invalid regexp, working around ^+ escape-problem"),
    "^\\+" ++ Regexp;
fixplus(Regexp) ->
    Regexp.


%%--------------------------------------------------------------------
%% Function: applyregexp/2
%% Description: Try a list of regexps together with our indata and
%%              return the rewrite result of the first one which could
%%              be rewritten, or 'none'.
%% Returns: ResultStr |
%%          none
%%--------------------------------------------------------------------
applyregexp(_Number, []) ->
    none;
applyregexp(Number, [Regexp | Rest]) ->
    logger:log(debug, "Resolver: applyregexp: IN ~p ~p", [Number, Regexp]),
    %% XXX handle other tokens than !
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


%%--------------------------------------------------------------------
%% Function: enumalldomains/2
%% Description: Perform ENUM NAPTR lookup of Number under a set of
%%              domains and result a list of naptrrecord records.
%% Returns: NAPTRList
%%--------------------------------------------------------------------
enumalldomains(Number, Domains) ->
    enumalldomains2(Number, Domains, []).

enumalldomains2(_Number, [], Res) ->
    Res;
enumalldomains2(Number, [Domain | Rest], Res) ->
    ENUMdomain = number2enum(lists:reverse(Number), Domain),
    This = naptrlookup(ENUMdomain),
    enumalldomains2(Number, Rest, lists:append(Res, This)).


%%--------------------------------------------------------------------
%% Function: chooseenum/2
%% Description: Pick out all NAPTR records we recognize as ENUM NAPTR
%%              records from a list of naptrrecord records.
%% Returns: ENUMNAPTRList
%%--------------------------------------------------------------------
chooseenum([], _Type) ->
    [];
chooseenum([Elem | Rest], Type) when record(Elem, naptrrecord), Elem#naptrrecord.flags == "u", Elem#naptrrecord.replacement == "" ->
    Services = Elem#naptrrecord.services,
    case Services of
	Type ->
	    %% exact match of Type
	    [Elem | chooseenum(Rest, Type)];
	_ ->
	    %% Does not match Type, see if Services begins with Type followed by a colon
	    TypeColon = Type ++ ":",
	    case string:substr(Elem#naptrrecord.services, 1, length(Type) + 1) of
		TypeColon ->
		    %% Type matches, but there is also a subtype (which we ignore)
		    [Elem | chooseenum(Rest, Type)];
		_ ->
		    %% Services matches neither Type nor Type:
		    chooseenum(Rest, Type)
	    end
    end;
chooseenum([H | Rest], Type) when record(H, naptrrecord), H#naptrrecord.flags /= "u" ->
    logger:log(debug, "Resolver: Skipping ENUM NAPTR because flags is not 'u' : ~p", [H]),
    chooseenum(Rest, Type);
chooseenum([H | Rest], Type) when record(H, naptrrecord), H#naptrrecord.replacement /= "" ->
    logger:log(debug, "Resolver: Skipping ENUM NAPTR because replacement is not empty : ~p", [H]),
    chooseenum(Rest, Type).


%%--------------------------------------------------------------------
%% Function: sortenum/2
%% Description: lists:sort function for ENUM naptrrecord records.
%%              Return true if A has lower order, or if the order
%%              is the same, if A has the lowest preference.
%% Returns: true  |
%%          false
%%--------------------------------------------------------------------
sortenum(A, B) when record(A, naptrrecord), record(B, naptrrecord), A#naptrrecord.order < B#naptrrecord.order ->
    true;
sortenum(A, B) when record(A, naptrrecord), record(B, naptrrecord), A#naptrrecord.preference < B#naptrrecord.preference ->
    true;
sortenum(A, B) when record(A, naptrrecord), record(B, naptrrecord) ->
    false.


%%--------------------------------------------------------------------
%% Function: naptr_regexp/1
%% Description: Return the regexps from a list of naptrrecords.
%% Returns: RegexpList
%%--------------------------------------------------------------------
naptr_regexp([]) ->
    [];
naptr_regexp([H | T]) when record(H, naptrrecord) ->
    [H#naptrrecord.regexp | naptr_regexp(T)].


%%--------------------------------------------------------------------
%% Function: enumlookup/1
%% Description: Do ENUM (RFC2916 and 2916bis) lookup on a E.164 number
%%              in the default configured list of ENUM domains. Uses
%%              enumlookup/2 below.
%% Returns:
%%--------------------------------------------------------------------
enumlookup(none) ->
    none;
enumlookup("+" ++ Number) ->
    case sipserver:get_env(enum_domainlist, none) of
	none ->
	    logger:log(debug, "Resolver: Not performing ENUM lookup on ~p since enum_domainlist is empty", ["+" ++ Number]),
	    none;
	DomainList ->
	    enumlookup("+" ++ Number, DomainList)
    end;
enumlookup(Foo) ->
    logger:log(error, "Resolver: ENUM lookup on non-E.164 number (~p)", [Foo]),
    none.

%%--------------------------------------------------------------------
%% Function: enumlookup/2
%% Description: Do ENUM (RFC2916 and 2916bis) lookup on a E.164 number
%%              in a set of domains. Returns the result of ENUM regexp
%%              rewrite as a string.
%% Returns:
%%--------------------------------------------------------------------
enumlookup("+" ++ Number, DomainList) ->
    %% XXX remove dashes and spaces etc. from Number
    logger:log(debug, "Resolver: ENUM query for ~p in ~p", ["+" ++ Number, DomainList]),
    L1 = enumalldomains(Number, DomainList),
    %% SIP+E2U is RFC2916 and E2U+SIP is 2916bis
    L2 = lists:append([chooseenum(L1, "SIP+E2U"), chooseenum(L1, "E2U+SIP")]),
    L3 = lists:sort(fun sortenum/2, L2),
    L4 = naptr_regexp(L3),
    applyregexp("+" ++ Number, L4).


%%--------------------------------------------------------------------
%% Function: isnaptr/1
%% Description: Evaluate if a dns_rr is of type NAPTR or not. To be
%%              used with lists:filter.
%% Returns: true  |
%%          false
%%--------------------------------------------------------------------
isnaptr(Entry) when record(Entry, dns_rr) ->
    if
	Entry#dns_rr.type == ?T_NAPTR ->
	    true;
	true ->
	    false
    end;
isnaptr(_) ->
    false.


%%--------------------------------------------------------------------
%% Function: naptrlookup/1
%% Description: Look up and return all NAPTR records for a given DNS
%%              domain name.
%% Returns: NAPTRList
%%--------------------------------------------------------------------
naptrlookup(Name) ->
    case inet_res:nslookup(Name, in, ?T_NAPTR) of
	{ok, Rec} ->
	    ParseNAPTR = fun(Entry) ->
				 parsenaptr(Entry#dns_rr.data)
			 end,
	    NAPTRs = lists:map(ParseNAPTR, lists:filter(fun isnaptr/1,
							Rec#dns_rec.anlist)),
	    logger:log(debug, "Resolver: naptrlookup: ~p -> found ~p NAPTR record(s)",
		       [Name, length(NAPTRs)]),
	    NAPTRs;
	{error, E} ->
	    logger:log(debug, "Resolver: naptrlookup: ~p -> error ~p", [Name, E]),
	    []
    end.


%%--------------------------------------------------------------------
%% Function: parsenaptr/1
%% Description: Takes the result of inet_res:nslookup and parses it
%%              into a naptrrecord.
%% Returns: NAPTRrecord
%%--------------------------------------------------------------------
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
    %% What we have in Replacementlength is the length of the first
    %% (and perhaps only) element of the replacement. It is like a
    %% linked list of elements - let parsenaptr_replacement parse it
    Replacement = parsenaptr_replacement(Replacementlength, Rest4, []),
    #naptrrecord{order=Order, preference=Preference,
		 flags=binary_to_list(Flags),
		 services=httpd_util:to_upper(binary_to_list(Services)),
		 regexp=binary_to_list(Regexp),
		 replacement=Replacement
		}.

%%--------------------------------------------------------------------
%% Function: parsenaptr_replacement/1
%% Description: Part of parsenaptr/1. Parse the replacement part of
%%              the inet:nslookup result into a string.
%% Returns: ReplacementString
%%--------------------------------------------------------------------
parsenaptr_replacement(0, _, Res) ->
    Res;
parsenaptr_replacement(Len, In, Res) when integer(Len), binary(In) ->
    %% Get next element (part of label), if the whole replacement is "_sip._tcp.example.com",
    %% then this element will be "_sip", "_tcp", "example" and "com". Concatenate all those
    %% parts with dots in between.
    <<Elem:Len/binary-unit:8,
     NextLen:8,
     Rest/binary>> = In,
    NewRes = case Res of
		 [] -> binary_to_list(Elem);
		 _ -> lists:flatten([Res, $., binary_to_list(Elem)])
	     end,
    parsenaptr_replacement(NextLen, Rest, NewRes).


%%--------------------------------------------------------------------
%% Function: filter_naptr/3
%% Description: Pick out all NAPTR records we recognize as domain
%%              NAPTR records from a list of naptrrecord records.
%% Returns: DomainNAPTRList
%%--------------------------------------------------------------------
filter_naptr([], _Flag, _Service) ->
    [];
filter_naptr([Elem | Rest], Flag, Service) when record(Elem, naptrrecord), Elem#naptrrecord.flags == Flag ->
    case Elem#naptrrecord.services of
	Service ->
	    %% exact match of Service
	    [Elem | filter_naptr(Rest, Flag, Service)];
	_ ->
	    filter_naptr(Rest, Flag, Service)
    end;
filter_naptr([H | Rest], Flag, Service) when record(H, naptrrecord), H#naptrrecord.flags /= Flag ->
    logger:log(debug, "Resolver: Skipping domain NAPTR because flags is not '~s' : ~p", [Flag, H]),
    filter_naptr(Rest, Flag, Service);
filter_naptr([H | Rest], Flag, Service) when record(H, naptrrecord), H#naptrrecord.replacement /= "" ->
    logger:log(debug, "Resolver: Skipping domain NAPTR because replacement is not empty : ~p", [H]),
    filter_naptr(Rest, Flag, Service).
