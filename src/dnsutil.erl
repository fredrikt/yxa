%%%-------------------------------------------------------------------
%%% File    : dnsutil.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: DNS resolving utility functions. Mainly used from the
%%%           sipdst module. Functions to resolve domain SRV- or
%%%           NAPTR records, and to do ENUM resolving.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(dnsutil).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 siplookup/1,
	 enumlookup/1,
	 enumlookup/2,
	 get_ip_port/2,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/src/inet_dns.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%% These records are entirely internal to this module
-record(srventry, {
	  proto,
	  dnsrrdata
	 }).
-record(naptrrecord, {
	  order,
	  preference,
	  flags,
	  services,
	  regexp,
	  replacement
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%% The DNS NAPTR RR type code. Other RR type codes are defined in OTP inet_dns.hrl.
-define(T_NAPTR, 35).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: siplookup(Domain)
%%           Domain = string()
%% Descrip.: Look up where to send SIP requests for a domain name.
%%           Ultimately return a list of {Proto, Host, Port} and/or
%%           {error, E} entrys. Proto is tcp, udp or tls. Never
%%           tcp6, udp6 or tls6 since we do not resolve Host into
%%           an address.
%% Returns : SRVList         |
%%           {error, Reason}
%%           SRVList = list() of sipdns_srv record()
%%           Reason  = atom(), nxdomain | ...
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
%% Function: enumlookup(In)
%%           In = none | string(), E.164 number (string starting with
%%                "+" followed by only digits)
%% Descrip.: Do ENUM (RFC2916 and 2916bis) lookup on a E.164 number
%%           in the default configured list of ENUM domains. Uses
%%           enumlookup/2 below.
%% Returns : Result |
%%           none
%%           Result = string(), result of ENUM NAPTR regexp rewrite
%%--------------------------------------------------------------------
enumlookup(none) ->
    none;
enumlookup("+" ++ Number) ->
    case yxa_config:get_env(enum_domainlist) of
	{ok, []} ->
	    logger:log(debug, "Resolver: Not performing ENUM lookup on ~p since enum_domainlist is empty",
		       ["+" ++ Number]),
	    none;
	{ok, DomainList} ->
	    enumlookup("+" ++ Number, DomainList)
    end;
enumlookup(Foo) ->
    logger:log(error, "Resolver: ENUM lookup on non-E.164 number (~p)", [Foo]),
    none.

%%--------------------------------------------------------------------
%% Function: enumlookup(Number, DomainList)
%%           Number     = string(), E.164 number (starting with "+")
%%           DomainList = list() of string(), ENUM domains to look in
%% Descrip.: Do ENUM (RFC2916 and 2916bis) lookup on a E.164 number
%%           in a set of domains. Returns a single value which is the
%%           result of ENUM regexp rewrite of the best NAPTR found
%%           as a string.
%% Returns : Result |
%%           none
%%           Result = string(), result of ENUM NAPTR regexp rewrite
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
%% Function: get_ip_port(Host, Port)
%%           Host = string(), hostname or IP address
%%           Port = integer() | none, port number
%% Descrip.: Simply look up the hostname supplied and return a list
%%           of IP addresses and the port. Port may be a list when
%%           supplied to us, but we will turn it into an integer.
%%           Port may also be 'none'.
%% Returns : AddrList        |
%%           {error, Reason}
%%           AddrList = list() of sipdns_hostport record()
%%--------------------------------------------------------------------
get_ip_port(Host, Port) when is_integer(Port) ; Port == none ->
    V6List = case yxa_config:get_env(enable_v6) of
		 {ok, true} ->
		     case get_ip_port2(inet6, Host, Port) of
			 {error, _} ->
			     %% Ignore v6 errors (they are logged in get_ip_port2 though)
			     [];
			 Res when is_list(Res) ->
			     Res
		     end;
		 {ok, false} ->
		     %% IPv6 is disabled
		     []
	     end,
    case get_ip_port2(inet, Host, Port) of
	{error, What4} ->
	    case V6List of
		[] ->
		    %% v6 list is empty (disabled or failed) and v4 failed
		    {error, What4};
		_ ->
		    V6List
	    end;
	V4List when is_list(V4List) ->
	    lists:append(V6List, V4List)
    end.

%% part of get_ip_port/2.
%% Returns : AddrList        |
%%           {error, Reason}
%%           AddrList = list() of sipdns_hostport record()
get_ip_port2(Family, Host, Port) when Family == inet ; Family == inet6 ->
    %% XXX inet:gethostbyname with proto inet6 is not documented, and not supported.
    case inet:gethostbyname(Host, Family) of
	{error, What} ->
	    logger:log(debug, "Resolver: Error ~p when resolving ~p ~p", [What, Host, Family]),
	    {error, What};
	{ok, HostEnt} when is_record(HostEnt, hostent) ->
	    HFam = HostEnt#hostent.h_addrtype,
	    Res = lists:map(fun(Addr) ->
				    case {Addr, HFam} of
					{{0, 0, 0, 0, 0, 65535, _, _}, _} ->
					    %% IPv4 mapped IPv6 address, ignore - we resolve
					    %% IPv4 addresses separately
					    [];
					{_Addr, Family} ->
					    #sipdns_hostport{family=Family, addr=siphost:makeip(Addr),
							     port=Port};
					{_Addr, _OtherFamily} ->
					    %% HFam is not the same as Family, ignore (if we
					    %% call gethostbyname() on an IPv4 address, but
					    %% family 'inet6' it returns an IPv4 hostent)
					    []
				    end
			    end, HostEnt#hostent.h_addr_list),
	    %% remove empty lists (ignored addresses) from the result
	    lists:flatten(Res)
    end.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: siplookup_naptr(Domain)
%% Descrip.: Look for NAPTR records for a domain telling us where
%%           to look for SRV records for the very same domain.
%% Returns : SRVList |
%%           nomatch
%%           SRVList = list() of srventry record()
%%--------------------------------------------------------------------
siplookup_naptr(Domain) when is_list(Domain) ->
    case naptrlookup(Domain) of
	[] ->
	    nomatch;
	L1 ->
	    %% RFC3263 #4.1 (Selecting a Transport Protocol) "... This specification
	    %% defines D2U for UDP, D2T for TCP, and D2S for SCTP."
	    L2 = lists:append([filter_naptr(L1, "s", "SIP+D2U"), filter_naptr(L1, "s", "SIP+D2T"),
			       filter_naptr(L1, "s","SIPS+D2T")]),
	    L3 = lists:sort(fun sortenum/2, L2),
	    %% Ok, we now have a preference sorted list of NAPTR results. Look for
	    %% SRV records matching each of them. Sort SRV records only per NAPTR -
	    %% the NAPTR order of preference supersedes the SRV ordering.
	    Res = naptr_to_srvlist(L3),
	    logger:log(debug, "dns resolver: Domain ~p NAPTR (+SRV) lookup result :~n~p",
		       [Domain, debugfriendly_srventry(Res)]),
	    Res
    end.


%%--------------------------------------------------------------------
%% Function: naptr_to_srvlist(In)
%%           In = list() of naptrrecord record()
%% Descrip.: Take a list of sorted NAPTR records and try to turn
%%           them into SRV records.
%% Returns : SRVList |
%%           nomatch
%%           SRVList = list() of srventry record()
%%--------------------------------------------------------------------
naptr_to_srvlist(In) ->
    naptr_to_srvlist2(In, []).

%%
%% Empty regexp
%%
naptr_to_srvlist2([#naptrrecord{regexp=""}=H | T], Res) ->
    Proto = case H#naptrrecord.services of
		"SIP+D2U"  ++ _ -> udp;
		"SIP+D2T"  ++ _ -> tcp;
		"SIPS+D2T" ++ _ -> tls
	    end,
    %% Regexp should be empty for domain NAPTR - RFC3263 #4.1. The
    %% interesting thing is the replacement.
    This = srvlookup(Proto, H#naptrrecord.replacement),
    Sorted = lists:sort(fun sortsrv/2, This),
    naptr_to_srvlist2(T, lists:flatten([Res, Sorted]));
%%
%% Non-empty regexp, not a domain NAPTR
%%
naptr_to_srvlist2([H | T], Res) when is_record(H, naptrrecord) ->
    naptr_to_srvlist2(T, Res);
naptr_to_srvlist2([], Res) ->
    Res.

%%--------------------------------------------------------------------
%% Function: debugfriendly_srventry(In)
%%           In = list() of srventry record()
%% Descrip.: Take a list of srventry records and return them in a
%%           format suitable for logging with ~p.
%% Returns : Data = list() of string()
%%--------------------------------------------------------------------
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
%% Returns : SRVList         |
%%           {error, Reason}
%%           SRVList = list() of sipdns_srv record()
%%           Reason  = atom(), nxdomain | ...
%%--------------------------------------------------------------------
combine_srvresults(In) ->
    combine_srvresults(In, [], []).

combine_srvresults([], [], []) ->
    %% No more input, no result and no errors - return nxdomain
    {error, nxdomain};
combine_srvresults([], [], Errors) ->
    [FirstError | _] = lists:reverse(Errors),
    FirstError;
combine_srvresults([], Res, _) ->
    lists:reverse(Res);
combine_srvresults([{error, What} | T], Res, Errors) ->
    combine_srvresults(T, Res, [{error, What} | Errors]);
combine_srvresults([H | T], Res, Errors) when is_record(H, srventry) ->
    {_Order, _Weight, Port, Host} = H#srventry.dnsrrdata,
    This = #sipdns_srv{proto=H#srventry.proto, host=Host, port=Port},
    combine_srvresults(T, [This | Res], Errors).


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
sortsrv(A, B) when is_record(A, srventry), is_record(B, srventry) ->
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
sortsrv(A, {error, _}) when is_record(A, srventry) ->
    true;
%% SRV record always beats error
sortsrv({error, _}, B) when is_record(B, srventry)->
    false;
%% NXDOMAIN always beats other errors
sortsrv({error, nxdomain}, {error, _}) ->
    true;
%% make errors come last
sortsrv({error, _}, {error, _}) ->
    false.


%%--------------------------------------------------------------------
%% Function: number2enum(Number, Domain)
%%           Number = string(), reversed E.164 number without the "+"
%%           Domain = string(), ENUM domain to append
%% Descrip.: Insert dots between all digits in the input. The input is
%%           the original number in reverse.
%% Returns : ENUMdomain = string()
%%--------------------------------------------------------------------
number2enum([], Domain) ->
    Domain;

number2enum([C | Rest], Domain) ->
    [C, $. | number2enum(Rest, Domain)].


%%--------------------------------------------------------------------
%% Function: fixplus(In)
%%           In = string()
%% Descrip.: ENUM users frequently put regexps starting with ^+ in
%%           their NAPTR records (because of examples in RFC2916).
%%           Erlang (R9C-0) regexp module can't handle those (it
%%           loops forever) so we turn them into ^\\+ which works.
%% Returns : NewRegexp = string()
%%--------------------------------------------------------------------
fixplus("^+" ++ Regexp) ->
    logger:log(debug,"Resolver: applyregexp: E.164 subscriber has used an invalid regexp, "
	       "working around ^+ escape-problem"),
    "^\\+" ++ Regexp;
fixplus(Regexp) ->
    Regexp.


%%--------------------------------------------------------------------
%% Function: applyregexp(Number, RegexpList)
%%           Number = string(), input
%%           Regexp = string(), regexp formatted as "regexp!rewrite".
%% Descrip.: Try a list of regexps together with our indata and
%%           return the rewrite result of the first one which could
%%           be rewritten, or 'none'.
%% Returns : Out = string() |
%%           none
%% Note    : We need to handle other tokens than "!" !!! XXX !!!
%% XXX an artefact of using string:tokens() is that multiple tokens
%% are treated as one, instead of being rejected!
%%--------------------------------------------------------------------
applyregexp(_Number, []) ->
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


%%--------------------------------------------------------------------
%% Function: enumalldomains(Number, Domains)
%%           Number  = string(), E.164 number _without_ the "+"
%%           Domains = list() of string(), ENUM domains
%% Descrip.: Perform ENUM NAPTR lookup of Number under a set of
%%           domains and result a list of naptrrecord records.
%% Returns : NAPTRList = list() of naptrrecord record()
%%--------------------------------------------------------------------
enumalldomains(Number, Domains) ->
    enumalldomains2(lists:reverse(Number), Domains, []).

enumalldomains2(_RNumber, [], Res) ->
    Res;
enumalldomains2(RNumber, [Domain | Rest], Res) ->
    ENUMdomain = number2enum(RNumber, Domain),
    This = naptrlookup(ENUMdomain),
    enumalldomains2(RNumber, Rest, lists:append(Res, This)).


%%--------------------------------------------------------------------
%% Function: chooseenum(NAPTRList, Type)
%%           NAPTRList = list() of naptrrecord record()
%%           Type      = string(), "SIP+E2U" or "E2U+SIP" or ...
%% Descrip.: Pick out all NAPTR records with a Enumservice matching
%%           the specified Type from a list of naptrrecord records.
%% Returns : ENUMNAPTRList = list() of naptrrecord record()
%% Note    : RFC3761 2.4.2 (Services Parameters) defines services as
%%               service-field = "E2U" 1*(servicespec)
%%               servicespec   = "+" enumservice
%%           and says "...followed by 1 or more or more Enumservices
%%           ... Each Enumservice is indicated by an initial '+'
%%           character.". XXX we don't handle entrys with more than
%%           one Enumservice.
%%--------------------------------------------------------------------
chooseenum([], _Type) ->
    [];
chooseenum([#naptrrecord{flags=Flags, replacement=Replacement}=Elem | Rest], Type) when Flags == "u",
											Replacement == "" ->
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
chooseenum([H | Rest], Type) when is_record(H, naptrrecord), H#naptrrecord.flags /= "u" ->
    logger:log(debug, "Resolver: Skipping ENUM NAPTR because flags is not 'u' : ~p", [H]),
    chooseenum(Rest, Type);
chooseenum([H | Rest], Type) when is_record(H, naptrrecord), H#naptrrecord.replacement /= "" ->
    logger:log(debug, "Resolver: Skipping ENUM NAPTR because replacement is not empty : ~p", [H]),
    chooseenum(Rest, Type).


%%--------------------------------------------------------------------
%% Function: sortenum(A, B)
%%           A, B = naptrrecord record()
%% Descrip.: lists:sort function for ENUM naptrrecord records. Return
%%           true if A has lower order. If the order is the same,
%%           return 'true' if A has the lowest preference.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
sortenum(#naptrrecord{order=AOrder}, #naptrrecord{order=BOrder}) when AOrder < BOrder ->
    true;
sortenum(#naptrrecord{order=Order, preference=APref}, #naptrrecord{order=Order, preference=BPref}) when APref < BPref ->
    %% Same order, A's preference is lower than B's
    true;
sortenum(A, B) when is_record(A, naptrrecord), is_record(B, naptrrecord) ->
    false.


%%--------------------------------------------------------------------
%% Function: naptr_regexp(In)
%%           In = list() of naptrrecord record()
%% Descrip.: Return the regexps from a list of naptrrecords.
%% Returns : RegexpList = list() of string()
%%--------------------------------------------------------------------
naptr_regexp([]) ->
    [];
naptr_regexp([H | T]) when is_record(H, naptrrecord) ->
    [H#naptrrecord.regexp | naptr_regexp(T)].


%%--------------------------------------------------------------------
%% Function: naptrlookup(Name)
%%           Name = string(), complete DNS label
%% Descrip.: Look up and return all NAPTR records for a given DNS
%%           domain name.
%% Returns : NAPTRList = list() of naptrrecord record()
%%--------------------------------------------------------------------
naptrlookup(Name) ->
    case inet_res:nslookup(Name, in, ?T_NAPTR) of
	{ok, Rec} ->
	    NAPTRs = parse_naptr_answer(Rec#dns_rec.anlist),
	    logger:log(debug, "Resolver: naptrlookup: ~p -> found ~p NAPTR record(s)",
		       [Name, length(NAPTRs)]),
	    NAPTRs;
	{error, E} ->
	    logger:log(debug, "Resolver: naptrlookup: ~p -> error ~p", [Name, E]),
	    []
    end.


%%--------------------------------------------------------------------
%% Function: parse_naptr_answer(DNSRRList)
%%           DNSRRList = list() of dns_rr record()
%% Descrip.: Look for NAPTR RRs in DNSRRList, and call parsenaptr on
%%           those.
%% Returns : NAPTRList = list() of naptrrecord record()
%%--------------------------------------------------------------------
parse_naptr_answer(DNSRRList) when is_list(DNSRRList) ->
    parse_naptr_answer2(DNSRRList, []).

%% NAPTR record
parse_naptr_answer2([#dns_rr{type=?T_NAPTR, data=Data} | T], Res) when is_list(Data) ->
    This = parsenaptr(list_to_binary(Data)),
    parse_naptr_answer2(T, [This | Res]);
%% non-NAPTR record
parse_naptr_answer2([H | T], Res) when is_record(H, dns_rr) ->
    parse_naptr_answer2(T, Res);
parse_naptr_answer2([], Res) ->
    Res.


%%--------------------------------------------------------------------
%% Function: parsenaptr(Binary)
%%           Binary = binary(), dns_rr record() 'data' element conv-
%%                    erted to binary
%% Descrip.: Takes some DNS RR data and parses it into a naptrrecord.
%% Returns : NAPTRrecord = naptrrecord record()
%% Note    : XXX could be rewritten to not create sub-binaries.
%%--------------------------------------------------------------------
parsenaptr(Binary) when is_binary(Binary) ->
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
%% Function: parsenaptr_replacement(Len, In, [])
%%           Len = integer(), length of replacement
%%           In  = binary(), the raw DNS data
%% Descrip.: Part of parsenaptr/1. Parse the replacement part of the
%%           inet:nslookup result into a string.
%% Returns : Replacement = string()
%% Note    : XXX could be rewritten to not create sub-binaries.
%%--------------------------------------------------------------------
parsenaptr_replacement(0, _, Res) ->
    Res;
parsenaptr_replacement(Len, In, Res) when is_integer(Len), is_binary(In) ->
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
%% Function: filter_naptr(NAPTRs, Flag, Service)
%%           NAPTRs  = list() of naptrrecord record()
%%           Flag    = string(), currently always "s"
%%           Service = string(), "SIP+D2U" | "SIP+D2T" | "SIPS+D2T"
%% Descrip.: Pick out all NAPTR records we recognize as domain NAPTR
%%           records from a list of naptrrecord records.
%% Returns : DomainNAPTRs = list() of naptrrecord record()
%% Note    : This function is quite similar to chooseenum(). Maybe
%%           they can be merged...
%%--------------------------------------------------------------------
filter_naptr(In, Flag, Service) when is_list(In), is_list(Flag), is_list(Service) ->
    filter_naptr2(In, Flag, Service, []).

filter_naptr2([], _Flag, _Service, Res) ->
    lists:reverse(Res);
filter_naptr2([#naptrrecord{flags=Flag, services=Service}=H | T], Flag, Service, Res) ->
    %% Flags and Services match (exact match on Services)
    filter_naptr2(T, Flag, Service, [H | Res]);
filter_naptr2([H | T], Flag, Service, Res) when is_record(H, naptrrecord) ->
    filter_naptr2(T, Flag, Service, Res).


%%--------------------------------------------------------------------
%% Function: srvlookup(Proto, Name)
%%           Proto = tcp | udp | tls
%%           Name  = string(), the DNS label
%% Descrip.: Resolve a SRV record (indicated by Name) and return a
%%           list of srventry records. The DNS RR data in them is
%%           of the format {Order, Pref, Port, Host}.
%% Returns : SRVList         |
%%           {error, Reason}
%%           SRVList = list() of srventry record()
%%--------------------------------------------------------------------
srvlookup(Proto, Name) when is_list(Name) ->
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


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%% Note    : There are a bunch of functions we don't test yet since
%%           they have side effects (does resolving).
%%--------------------------------------------------------------------
test() ->
    %% test siplookup(Domain)

    %% test enumlookup(E164Number)

    %% test enumlookup(E164Number, DomainList)

    %% test get_ip_port(Host, Port)
    %% test get_ip_port2(Family, Host, Port)

    %% test siplookup_naptr(Domain)

    %% test naptr_to_srvlist(In)

    %% test naptr_to_srvlist2(In, Res)

    %% test debugfriendly_srventry(In)


    %% test combine_srvresults(In)
    %%--------------------------------------------------------------------
    CombineSRV_1 = #srventry{dnsrrdata = {0, 21, 5060, "example.org"}, proto=tcp},
    CombineSRV_2 = #srventry{dnsrrdata = {0, 21, 5061, "example.net"}, proto=tls},
    CombineSRV_3 = {error, undefined},
    
    io:format("test: combine_srvresults/1 - 1~n"),
    %% remove error when there are also valid results
    [#sipdns_srv{proto=tcp, host="example.org", port=5060},
     #sipdns_srv{proto=tls, host="example.net", port=5061}] =
	combine_srvresults([CombineSRV_1, CombineSRV_2, CombineSRV_3]),
    
    io:format("test: combine_srvresults/1 - 2~n"),
    %% only error present
    {error, undefined} = combine_srvresults([{error, undefined}]),
    
    io:format("test: combine_srvresults/1 - 3~n"),
    %% neither valid entrys or errors
    {error, nxdomain} = combine_srvresults([]),


    %% test sortsrv(A, B)
    %%--------------------------------------------------------------------
    io:format("test: sortsrv/2 - 1~n"),
    %% sort only valid results

    %% for 'order', high is better. for 'preference', lower is better.
    SortSRV_1 = #srventry{dnsrrdata = {0, 21, 1, "example.org"}},
    SortSRV_2 = #srventry{dnsrrdata = {0, 20, 2, "example.org"}},
    SortSRV_3 = #srventry{dnsrrdata = {1, 10, 3, "example.org"}},
    SortSRV_4 = #srventry{dnsrrdata = {2, 10, 4, "example.org"}},

    SortSRV_L1 = [SortSRV_1, SortSRV_2, SortSRV_3, SortSRV_4],

    %% test that all permutations of SortSRV_L1 run through sortsrv/2
    %% results in the sorted list again
    lists:foldl(fun(H, Num) ->
			%%io:format("test: sortsrv/2 - 1.~p~n", [Num]),
			SortSRV_L1 = lists:sort(fun sortsrv/2, H),
			Num + 1
		end, 1, test_permutations(SortSRV_L1)),

    io:format("test: sortsrv/2 - 2~n"),
    %% sort some errors together with valid results
    SortSRV_E1 = {error, nxdomain},
    SortSRV_E2 = {error, undefined},

    SortSRV_L2 = [SortSRV_1, SortSRV_2, SortSRV_E1, SortSRV_E2],

    %% test that all permutations of SortSRV_L1 run through sortsrv/2
    %% results in the sorted list again
    lists:foldl(fun(H, Num) ->
			%%io:format("test: sortsrv/2 - 2.~p~n", [Num]),
			SortSRV_L2 = lists:sort(fun sortsrv/2, H),
			Num + 1
		end, 1, test_permutations(SortSRV_L2)),

    io:format("test: sortsrv/2 - 3~n"),
    %% nxdomain should beat other errors
    [SortSRV_E1, SortSRV_E2] = lists:sort(fun sortsrv/2, [SortSRV_E2, SortSRV_E1]),


    %% test number2enum([], Domain)

    %% test number2enum([C | Rest], Domain)

    %% test fixplus(Regexp)
    %%--------------------------------------------------------------------
    io:format("test: fixplus/1 - 1~n"),
    %% incorrect regexp (can't have more than one start of string (^ - circumflex) )
    "^\\+foo" = fixplus("^+foo"),

    io:format("test: fixplus/1 - 2~n"),
    "other" = fixplus("other"),

    %% test applyregexp(Number, RegexpList)
    %%--------------------------------------------------------------------
    io:format("test: applyregexp/1 - 1~n"),
    "bar" = applyregexp("Xfoobar", ["!.+foo(...)$!\\1"]),
    
    io:format("test: applyregexp/1 - 2~n"),
    %% XXX this is a potential problem - the regexp "foo..." does not match "Xfoobar" - shouldn't it?
    none = applyregexp("Xfoobar", ["!foo...$!foo"]),

    io:format("test: applyregexp/1 - 3~n"),
    %% multiple regexps, none matching
    none = applyregexp("ABC", ["!foo!foo", "!bar!123"]),

    io:format("test: applyregexp/1 - 4~n"),
    %% multiple regexps, the first one without our token ($!)
    "123" = applyregexp("bar", ["/foo/foo", "!bar!123"]),

    io:format("test: applyregexp/1 - 4~n"),
    %% XXX an artefact of using string:tokens() is that multiple tokens are treated as one,
    %% so this test should really NOT match
    "123" = applyregexp("bar", ["!!!!bar!!!123"]),

    %% test enumalldomains(Number, Domains)

    %% test chooseenum(In, Type)
    %%--------------------------------------------------------------------
    ChooseENUM_1 = #naptrrecord{flags="u", replacement="", services="E2U+sip"},
    ChooseENUM_2 = #naptrrecord{flags="u", replacement="", services="E2U+msg"},
    ChooseENUM_3 = #naptrrecord{flags="u", replacement="", services="E2U+sip:sub"},	%% sub-type
    ChooseENUM_4 = #naptrrecord{flags="s", replacement="", services="E2U+msg"},		%% flag not "u"
    ChooseENUM_5 = #naptrrecord{flags="u", replacement="X", services="E2U+msg"},	%% replacement not ""
    
    ChooseENUM_L1 = [ChooseENUM_1, ChooseENUM_2, ChooseENUM_3, ChooseENUM_4, ChooseENUM_5],

    io:format("test: chooseenum/2 - 1~n"),
    %% single match
    [ChooseENUM_2] = chooseenum(ChooseENUM_L1, "E2U+msg"),

    io:format("test: chooseenum/2 - 2~n"),
    %% more than one match, one matches even though it has a sub-type (":sub")
    [ChooseENUM_1, ChooseENUM_3] = chooseenum(ChooseENUM_L1, "E2U+sip"),
    
    io:format("test: chooseenum/2 - 3 (disabled)~n"),
%    %% more than one match, verify that we compare case insensitively
%    %% XXX should we do case sensitive or not? Read the RFC!
%    [ChooseENUM_1, ChooseENUM_3] = chooseenum(ChooseENUM_L1, "e2u+SIP"),
    
    io:format("test: chooseenum/2 - 4~n"),
    %% no match
    [] = chooseenum(ChooseENUM_L1, "E2U+foo"),


    %% test naptr_regexp(NAPTRList)
    %%--------------------------------------------------------------------
    io:format("test: naptr_regexp/1 - 1~n"),
    [1] = naptr_regexp([#naptrrecord{regexp=1}]),

    io:format("test: naptr_regexp/1 - 2~n"),
    [1, 2] = naptr_regexp([#naptrrecord{regexp=1}, #naptrrecord{regexp=2}]),

    %% test parsenaptr(Binary)
    %%--------------------------------------------------------------------
    io:format("test: parsenaptr/1 - 1~n"),
    %% domain NAPTR
    NAPTR1 = <<20:16,		%% order
	      0:16,		%% preference
	      1, "s",		%% flags
	      7, "SIP+D2U",	%% services
	      0,		%% regexp
	      %% replacement, length indicator followed by that number of
	      %% bytes until length indicator is 0
	      4, "_sip", 4, "_udp", 3, "sip", 2, "su", 2, "se", 0
	      >>,
    NAPTR1_R = #naptrrecord{order = 20,
			    preference = 0,
			    flags = "s",
			    services = "SIP+D2U",
			    regexp = [],
			    replacement = "_sip._udp.sip.su.se"},
    NAPTR1_R = parsenaptr(NAPTR1),

    io:format("test: parsenaptr/1 - 2~n"),
    %% ENUM NAPTR
    NAPTR2 = <<100:16,		%% order
	      10:16,		%% preference
	      1, "u",		%% flags
	      7, "E2U+sip",	%% services
	      22, "!^.*$!sip:ft@it.su.se!",	%% regexp
	      0			%% replacement
	      >>,
    NAPTR2_R = #naptrrecord{order = 100,
			    preference = 10,
			    flags = "u",
			    services = "E2U+SIP",
			    regexp = "!^.*$!sip:ft@it.su.se!",
			    replacement = []},
    NAPTR2_R = parsenaptr(NAPTR2),

    %% test parse_naptr_answer(DNSRRList)
    %%--------------------------------------------------------------------
    io:format("test: parse_naptr_answer/1 - 1~n"),
    %% we should get only the NAPTR records back (in list() of naptrrecord record())
    NAPTRList1 = [#dns_rr{type=?T_NS, data=[]},
		  #dns_rr{type=?T_NAPTR, data=binary_to_list(NAPTR2)},
		  #dns_rr{type=?T_A, data=[]},
		  #dns_rr{type=?T_NAPTR, data=binary_to_list(NAPTR1)}],

    [NAPTR1_R, NAPTR2_R] = parse_naptr_answer(NAPTRList1),


    %% test sortenum(A, B)
    %%--------------------------------------------------------------------
    SortENUM_O1 = #naptrrecord{order=0, preference=0, flags=1},
    SortENUM_O2 = #naptrrecord{order=0, preference=1, flags=2},
    SortENUM_O3 = #naptrrecord{order=1, preference=0, flags=3},
    SortENUM_O4 = #naptrrecord{order=1, preference=1, flags=4},

    SortENUM_L1 = [SortENUM_O1, SortENUM_O2, SortENUM_O3, SortENUM_O4],

    io:format("test: sortenum/2 - 1~n"),
    %% test that all permutations of SortENUM_L1 run through sortenum/2
    %% results in the sorted list again
    lists:foldl(fun(H, Num) ->
			%%io:format("test: sortenum/2 - ~p~n", [Num]),
			SortENUM_L1 = lists:sort(fun sortenum/2, H),
			Num + 1
		end, 1, test_permutations(SortENUM_L1)),

    %% test filter_naptr(NAPTRs, Flag, Service)
    %%--------------------------------------------------------------------
    FilterNAPTR_1 = #naptrrecord{flags="u", replacement="", services="E2U+sip"},
    FilterNAPTR_2 = #naptrrecord{flags="u", replacement="", services="E2U+msg"},
    FilterNAPTR_3 = #naptrrecord{flags="u", replacement="", services="E2U+sip:sub"},	%% sub-type
    FilterNAPTR_4 = #naptrrecord{flags="s", replacement="", services="E2U+msg"},
    FilterNAPTR_5 = #naptrrecord{flags="u", replacement="X", services="E2U+msg"},
    
    FilterNAPTR_L1 = [FilterNAPTR_1, FilterNAPTR_2, FilterNAPTR_3, FilterNAPTR_4, FilterNAPTR_5],

    io:format("test: filter_naptr/3 - 1~n"),
    %% single match
    [FilterNAPTR_4] = filter_naptr(FilterNAPTR_L1, "s", "E2U+msg"),

    io:format("test: filter_naptr/3 - 2~n"),
    %% more than one match, but one has a sub-type (":sub") and will be ignored
    %% XXX is this correct? Does the RFC3761 say that domain NAPTRs can't have sub-type?
    [FilterNAPTR_1] = filter_naptr(FilterNAPTR_L1, "u", "E2U+sip"),
     
    io:format("test: filter_naptr/3 - 3 (disabled)~n"),
%    %% one match, verify that we compare case insensitively
%    %% XXX should we do case sensitive or not? Read the RFC!
%    [FilterNAPTR_1] = filter_naptr(FilterNAPTR_L1, "e2u+SIP"),

    io:format("test: filter_naptr/3 - 4~n"),
    %% no match
    [] = filter_naptr(FilterNAPTR_L1, "zz", "E2U+foo"),


    %% test srvlookup(Proto, Name)

    ok.

%% Neat function from Joe Armstrongs thesis (page 58). Creates all
%% possible permutations of the input list :
%% L = "abc" -> ["abc","acb","bac","bca","cab","cba"]
test_permutations([]) -> [[]];
test_permutations(L) ->
    [[H|T] || H <- L, T <- test_permutations(L--[H])].
