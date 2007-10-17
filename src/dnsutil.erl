%%%-------------------------------------------------------------------
%%% File    : dnsutil.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      DNS resolving utility functions. Mainly used from the
%%%           sipdst module. Functions to resolve domain SRV- or
%%%           NAPTR records, and to do ENUM resolving.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
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

%% @type srventry() = #srventry{}.
%%                    These records are entirely internal to this module
-record(srventry, {
	  proto,	%% inet or inet6 ? XXX
	  order,	%% SRV RR order
	  weight,	%% SRV RR weight
	  host,		%% SRV RR host
	  port		%% SRV RR port
	 }).
%% @type naptrrecord() = #naptrrecord{}.
%%                       no description
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
%% @spec    (Domain) ->
%%            SRVList         |
%%            {error, Reason}
%%
%%            Domain = string()
%%
%%            SRVList = [#sipdns_srv{}]
%%            Reason  = nxdomain | atom()
%%
%% @doc     Look up where to send SIP requests for a domain name.
%%          Ultimately return a list of {Proto, Host, Port} and/or
%%          {error, E} entrys. Proto is tcp, udp or tls. Never tcp6,
%%          udp6 or tls6 since we do not resolve Host into an
%%          address.
%% @end
%%--------------------------------------------------------------------
siplookup([]) ->
    {error, invalid_domainname};
siplookup(Domain) ->
    case siplookup_naptr(Domain) of
	nomatch ->
	    %% No domain NAPTR records found. Perform RFC2543 backwards compatible
	    %% SRV lookup of the domain.
	    logger:log(debug, "Resolver: Domain ~p has no NAPTR records, perform RFC2543 backwards "
		       "compatible lookup for SIP SRV records under that domain name", [Domain]),
	    TCP = srvlookup(tcp, "_sip._tcp." ++ Domain),
	    UDP = srvlookup(udp, "_sip._udp." ++ Domain),
	    TLS = srvlookup(tls, "_sips._tcp." ++ Domain),

	    All = lists:flatten([TCP, UDP, TLS]),
	    Sorted = sort_srvlist(All),
	    R = combine_srvresults(Sorted),
	    logger:log(debug, "Resolver: Result of RFC2543 compatible SIP SRV query for domain ~p :~n~p",
		       [Domain, R]),
	    R;
	Res ->
	    %% siplookup_naptr does it's own sorting
	    combine_srvresults(Res)
    end.

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Result |
%%            none
%%
%%            In = none | string() "E.164 number (string starting with \"+\" followed by only digits)"
%%
%%            Result = string() "result of ENUM NAPTR regexp rewrite"
%%
%% @doc     Do ENUM (RFC2916 and 2916bis) lookup on a E.164 number in
%%          the default configured list of ENUM domains. Uses
%%          enumlookup/2 below.
%% @end
%%--------------------------------------------------------------------
enumlookup(none) ->
    none;
enumlookup("+" ++ Number) ->
    case yxa_config:get_env(enum_domainlist) of
	{ok, DomainList} ->
	    enumlookup("+" ++ Number, DomainList);
	none ->
	    logger:log(debug, "Resolver: Not performing ENUM lookup on ~p since enum_domainlist is not set",
		       ["+" ++ Number]),
	    none
    end;
enumlookup(Foo) ->
    logger:log(error, "Resolver: ENUM lookup on non-E.164 number (~p)", [Foo]),
    none.

%%--------------------------------------------------------------------
%% @spec    (Number, DomainList) ->
%%            Result |
%%            none
%%
%%            Number     = string() "E.164 number (starting with \"+\")"
%%            DomainList = [string()] "ENUM domains to look in"
%%
%%            Result = string() "result of ENUM NAPTR regexp rewrite"
%%
%% @doc     Do ENUM (RFC2916 and 2916bis) lookup on a E.164 number in
%%          a set of domains. Returns a single value which is the
%%          result of ENUM regexp rewrite of the best NAPTR found as
%%          a string.
%% @end
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
%% @spec    (Host, Port) ->
%%            AddrList        |
%%            {error, Reason}
%%
%%            Host = string() "hostname or IP address"
%%            Port = integer() | none "port number"
%%
%%            AddrList = [#sipdns_hostport{}]
%%
%% @doc     Simply look up the hostname supplied and return a list of
%%          #sipdns_hostport records with the IP addresses the Host
%%          resolved to, together with the IP address as supplied to
%%          this function.
%% @end
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
	    V6List ++ V4List
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
					    #sipdns_hostport{family = Family,
							     addr   = siphost:makeip(Addr),
							     port   = Port
							    };
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
%% @spec    (Domain) ->
%%            SRVList |
%%            nomatch
%%
%%            SRVList = [#srventry{}]
%%
%% @doc     Look for NAPTR records for a domain telling us where to
%%          look for SRV records for the very same domain.
%% @end
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
%% @spec    (In) ->
%%            SRVList |
%%            nomatch
%%
%%            In = [#naptrrecord{}]
%%
%%            SRVList = [#srventry{}]
%%
%% @doc     Take a list of sorted NAPTR records and try to turn them
%%          into SRV records.
%% @end
%%--------------------------------------------------------------------
naptr_to_srvlist(In) ->
    naptr_to_srvlist2(In, []).

%%
%% Empty regexp
%%
naptr_to_srvlist2([#naptrrecord{regexp = ""} = H | T], Res) ->
    Proto = case H#naptrrecord.services of
		"SIP+D2U"  ++ _ -> udp;
		"SIP+D2T"  ++ _ -> tcp;
		"SIPS+D2T" ++ _ -> tls
	    end,
    %% Regexp should be empty for domain NAPTR - RFC3263 #4.1. The interesting thing is
    %% the replacement. (We make sure the regexp is empty in the function declaration)
    case srvlookup(Proto, H#naptrrecord.replacement) of
	This when is_list(This) ->
	    Sorted = sort_srvlist(This),
	    naptr_to_srvlist2(T, lists:flatten([Res, Sorted]));
	{error, Reason} ->
	    logger:log(debug, "Resolver: Lookup of SRV records for ~p failed : ~p",
		       [H#naptrrecord.replacement, Reason]),
	    naptr_to_srvlist2(T, Res)
    end;
%%
%% Non-empty regexp, or not a domain NAPTR - skip
%%
naptr_to_srvlist2([H | T], Res) when is_record(H, naptrrecord) ->
    naptr_to_srvlist2(T, Res);
naptr_to_srvlist2([], Res) ->
    Res.

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Data
%%
%%            In = [#srventry{}]
%%
%%            Data = [string()]
%%
%% @doc     Take a list of srventry records and return them in a
%%          format suitable for logging with ~p.
%% @end
%%--------------------------------------------------------------------
debugfriendly_srventry(In) when is_list(In) ->
    debugfriendly_srventry2(In, []).

debugfriendly_srventry2([], Res) ->
    lists:reverse(Res);
debugfriendly_srventry2([H|T], Res) when is_record(H, srventry) ->
    #srventry{order  = O,
	      weight = W,
	      port   = Port,
	      host   = Host
	     } = H,
    This = lists:flatten(lists:concat([H#srventry.proto, ", order=", O, ", weight=", W,
				       " ", Host, ":", Port])),
    debugfriendly_srventry2(T, [This | Res]).

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            SRVList         |
%%            {error, Reason}
%%
%%            In = [#srventry{}] | {error, R}
%%
%%            SRVList = [#sipdns_srv{}]
%%            Reason  = nxdomain | atom()
%%
%% @doc     Walk through a list of srventry records or {error, R}
%%          tuples and turn the srventry records into {Proto, Host,
%%          Port} tuples.
%% @end
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
    This = #sipdns_srv{proto = H#srventry.proto,
		       host  = H#srventry.host,
		       port  = H#srventry.port
		      },
    combine_srvresults(T, [This | Res], Errors).

%%--------------------------------------------------------------------
%% @spec    (In) -> [#srventry{}]
%%
%%            In = [#srventry{}] | {error, R}
%%
%% @doc     Walk through a list of srventry records or {error, R}
%%          tuples and sort them according to order and secondly by
%%          weight. Sort errors last, but put nxdomain errors first
%%          of the errors. The weight sorting is proportional for
%%          entrys of the same order, not fixed.
%% @end
%%--------------------------------------------------------------------
sort_srvlist(In) when is_list(In) ->
    Sorted = lists:sort(fun sortsrv/2, In),
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3),
    make_weight_proportional(Sorted).

%% part of sort_svrlist, a lists:sort function for SRV records
sortsrv(A, B) when is_record(A, srventry), is_record(B, srventry) ->
    #srventry{order = Order1, weight = Weight1} = A,
    #srventry{order = Order2, weight = Weight2} = B,

    if
	Order1 < Order2 ->
	    true;
	Order1 == Order2 ->
	    %% Weight is really a proportional thing, but we sort on it for the same
	    %% order here, to make sortsrv/2 deterministic (for test cases)
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
%% @spec    (In) ->
%%            [Res]
%%
%%            In = [#srventry{}] | {error, R}
%%
%%            Res = #srventry{} | {error, R}
%%
%% @doc     First, separate out all entrys with the same priority.
%%          Invoke order_proportional_weight once per priority.
%% @end
%%--------------------------------------------------------------------
make_weight_proportional(In) when is_list(In) ->
    make_weight_proportional2(In, undefined, [], []).

make_weight_proportional2([#srventry{order = Order} | _] = In, undefined, SRVs, Res) ->
    %% first entry, no previous order
    make_weight_proportional2(In, Order, SRVs, Res);
make_weight_proportional2([#srventry{order = Order} = H | T], Order, SRVs, Res) ->
    %% same order, append to SRVs and recurse
    make_weight_proportional2(T, Order, [H | SRVs], Res);
make_weight_proportional2([H | T], _OtherOrder, SRVs, Res) ->
    %% Something else than another entry with the same Order encountered. Make the
    %% proportional thing and append SRVs (plus H) to Res
    OrderedSRVs = order_proportional_weight(SRVs),
    case is_record(H, srventry) of
	true ->
	    %% another SRV, with different Order - we should recurse
	    make_weight_proportional2(T, H#srventry.order, [H], [OrderedSRVs | Res]);
	false ->
	    %% not another SRV, append to Res and we are done - flatten before returning
	    lists:flatten([[OrderedSRVs | Res], [H | T]])
    end;
make_weight_proportional2([], _Order, SRVs, Res) ->
    %% End of input encountered. Do the  proportional thing and we are done
    OrderedSRVs = order_proportional_weight(SRVs),
    %% flatten before returning
    lists:append([OrderedSRVs | Res]).

%%--------------------------------------------------------------------
%% @spec    (In) -> [#srventry{}]
%%
%%            In = [#srventry{}]
%%
%% @doc     Use random to sort the entrys in In in a proportional
%%          fashion. If three entrys have priority 45, 45 and 10, the
%%          probability of one of the 45 percent entrys to be
%%          returned first is 90 % (one of the two, not the first or
%%          second one individually) and the probability of the last
%%          entry being returned first is ten percent.
%% @end
%%--------------------------------------------------------------------
order_proportional_weight(In) when is_list(In) ->
    WeightSum = lists:foldl(fun(E, Acc) ->
				    Acc + E#srventry.weight
			    end, 0, In),
    order_proportional_weight2(In, WeightSum, []).

order_proportional_weight2([], _Sum, Res) ->
    lists:reverse(Res);
order_proportional_weight2(In, Sum, Res) ->
    WinnerNum = random:uniform(Sum + 1) - 1,
    Winner = get_winner(In, WinnerNum),
    Rest = In -- [Winner],
    NewSum = Sum - Winner#srventry.weight,
    order_proportional_weight2(Rest, NewSum, [Winner | Res]).

get_winner([#srventry{weight = ThisWeight} = H | _], WinnerNum) when WinnerNum =< ThisWeight ->
    %% We found our winner
    H;
get_winner([#srventry{weight = ThisWeight} | T], WinnerNum) ->
    get_winner(T, WinnerNum - ThisWeight).


%%--------------------------------------------------------------------
%% @spec    (Number, Domain) ->
%%            ENUMdomain
%%
%%            Number = string() "reversed E.164 number without the \"+\""
%%            Domain = string() "ENUM domain to append"
%%
%%            ENUMdomain = string()
%%
%% @doc     Insert dots between all digits in the input. The input is
%%          the original number in reverse.
%% @end
%%--------------------------------------------------------------------
number2enum([], Domain) ->
    Domain;

number2enum([C | Rest], Domain) ->
    [C, $. | number2enum(Rest, Domain)].


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            NewRegexp
%%
%%            In = string()
%%
%%            NewRegexp = string()
%%
%% @doc     ENUM users frequently put regexps starting with ^+ in
%%          their NAPTR records (because of examples in RFC2916).
%%          Erlang (R9C-0) regexp module can't handle those (it loops
%%          forever) so we turn them into ^\\+ which works.
%% @end
%%--------------------------------------------------------------------
fixplus("^+" ++ Regexp) ->
    logger:log(debug,"Resolver: applyregexp: E.164 subscriber has used an invalid regexp, "
	       "working around ^+ escape-problem"),
    "^\\+" ++ Regexp;
fixplus(Regexp) ->
    Regexp.


%%--------------------------------------------------------------------
%% @spec    (Number, RegexpList) ->
%%            Out
%%
%%            Number = string() "input"
%%            Regexp = string() "regexp formatted as \"regexp!rewrite\"."
%%
%%            Out = string() |
%%            none
%%
%% @doc     Try a list of regexps together with our indata and return
%%          the rewrite result of the first one which could be
%%          rewritten, or 'none'. Note : We need to handle other
%%          tokens than "!" !!! XXX !!! XXX an artefact of using
%%          string:tokens() is that multiple tokens are treated as
%%          one, instead of being rejected!
%% @end
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
%% @spec    (Number, Domains) ->
%%            NAPTRList
%%
%%            Number  = string() "E.164 number _without_ the \"+\""
%%            Domains = [string()] "ENUM domains"
%%
%%            NAPTRList = [#naptrrecord{}]
%%
%% @doc     Perform ENUM NAPTR lookup of Number under a set of domains
%%          and result a list of naptrrecord records.
%% @end
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
%% @spec    (NAPTRList, Type) ->
%%            ENUMNAPTRList
%%
%%            NAPTRList = [#naptrrecord{}]
%%            Type      = string() "\"SIP+E2U\" or \"E2U+SIP\" or ..."
%%
%%            ENUMNAPTRList = [#naptrrecord{}]
%%
%% @doc     Pick out all NAPTR records with a Enumservice matching the
%%          specified Type from a list of naptrrecord records. Note :
%%          RFC3761 2.4.2 (Services Parameters) defines services as
%%          service-field = "E2U" 1*(servicespec) servicespec = "+"
%%          enumservice and says "...followed by 1 or more or more
%%          Enumservices ... Each Enumservice is indicated by an
%%          initial '+' character.". XXX we don't handle entrys with
%%          more than one Enumservice.
%% @end
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
%% @spec    (A, B) -> true  | false
%%
%%            A = #naptrrecord{}
%%            B = #naptrrecord{}
%%
%% @doc     lists:sort function for ENUM naptrrecord records. Return
%%          true if A has lower order. If the order is the same,
%%          return 'true' if A has the lowest preference.
%% @end
%%--------------------------------------------------------------------
sortenum(#naptrrecord{order=AOrder}, #naptrrecord{order=BOrder}) when AOrder < BOrder ->
    true;
sortenum(#naptrrecord{order=Order, preference=APref}, #naptrrecord{order=Order, preference=BPref}) when APref < BPref ->
    %% Same order, A's preference is lower than B's
    true;
sortenum(A, B) when is_record(A, naptrrecord), is_record(B, naptrrecord) ->
    false.


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            RegexpList
%%
%%            In = [#naptrrecord{}]
%%
%%            RegexpList = [string()]
%%
%% @doc     Return the regexps from a list of naptrrecords.
%% @end
%%--------------------------------------------------------------------
naptr_regexp([]) ->
    [];
naptr_regexp([H | T]) when is_record(H, naptrrecord) ->
    [H#naptrrecord.regexp | naptr_regexp(T)].


%%--------------------------------------------------------------------
%% @spec    (Name) ->
%%            NAPTRList
%%
%%            Name = string() "complete DNS label"
%%
%%            NAPTRList = [#naptrrecord{}]
%%
%% @doc     Look up and return all NAPTR records for a given DNS
%%          domain name.
%% @end
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
%% @spec    (DNSRRList) ->
%%            NAPTRList
%%
%%            DNSRRList = [#dns_rr{}]
%%
%%            NAPTRList = [#naptrrecord{}]
%%
%% @doc     Look for NAPTR RRs in DNSRRList, and call parsenaptr on
%%          those.
%% @end
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
%% @spec    (Binary) ->
%%            NAPTRrecord
%%
%%            Binary = binary() "#dns_rr{} 'data' element conv- erted to binary"
%%
%%            NAPTRrecord = #naptrrecord{}
%%
%% @doc     Takes some DNS RR data and parses it into a naptrrecord.
%%          Note : XXX could be rewritten to not create sub-binaries.
%% @end
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
%% @spec    (Len, In, []) ->
%%            Replacement
%%
%%            Len = integer() "length of replacement"
%%            In  = binary() "the raw DNS data"
%%
%%            Replacement = string()
%%
%% @doc     Part of parsenaptr/1. Parse the replacement part of the
%%          inet:nslookup result into a string. Note : XXX could be
%%          rewritten to not create sub-binaries.
%% @end
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
%% @spec    (NAPTRs, Flag, Service) ->
%%            DomainNAPTRs
%%
%%            NAPTRs  = [#naptrrecord{}]
%%            Flag    = string() "currently always \"s\""
%%            Service = string() "\"SIP+D2U\" | \"SIP+D2T\" | \"SIPS+D2T\""
%%
%%            DomainNAPTRs = [#naptrrecord{}]
%%
%% @doc     Pick out all NAPTR records we recognize as domain NAPTR
%%          records from a list of naptrrecord records. Note : This
%%          function is quite similar to chooseenum(). Maybe they can
%%          be merged...
%% @end
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
%% @spec    (Proto, Name) ->
%%            SRVList         |
%%            {error, Reason}
%%
%%            Proto = tcp | udp | tls
%%            Name  = string() "the DNS label"
%%
%%            SRVList = [#srventry{}]
%%
%% @doc     Resolve a SRV record (indicated by Name) and return a list
%%          of srventry records. The DNS RR data in them is of the
%%          format {Order, Pref, Port, Host}.
%% @end
%%--------------------------------------------------------------------
srvlookup(Proto, Name) when is_list(Name) ->
    case inet_res:nslookup(Name, in, srv) of
	{ok, Rec} ->
	    ParseSRV = fun(Entry) ->
			       {Order, Weight, Port, Host} = Entry#dns_rr.data,

			       #srventry{proto  = Proto,
					 order       = Order,
					 weight      = Weight,
					 port        = Port,
					 host	     = Host
					}
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
%% @spec    () -> ok
%%
%% @doc     autotest callback Note : There are a bunch of functions we
%%          don't test yet since they have side effects (does
%%          resolving).
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    %% test siplookup(Domain)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "siplookup/1 - 1"),
    %% test invalid input domain
    {error, invalid_domainname} = siplookup([]),


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
    CombineSRV_1 = #srventry{order = 0, weight = 21, port = 5060, host = "example.org", proto = tcp},
    CombineSRV_2 = #srventry{order = 0, weight = 21, port = 5061, host = "example.net", proto = tls},
    CombineSRV_3 = {error, undefined},

    autotest:mark(?LINE, "combine_srvresults/1 - 1"),
    %% remove error when there are also valid results
    [#sipdns_srv{proto=tcp, host="example.org", port=5060},
     #sipdns_srv{proto=tls, host="example.net", port=5061}] =
	combine_srvresults([CombineSRV_1, CombineSRV_2, CombineSRV_3]),

    autotest:mark(?LINE, "combine_srvresults/1 - 2"),
    %% only error present
    {error, undefined} = combine_srvresults([{error, undefined}]),

    autotest:mark(?LINE, "combine_srvresults/1 - 3"),
    %% neither valid entrys or errors
    {error, nxdomain} = combine_srvresults([]),


    %% test sortsrv(A, B)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "sortsrv/2 - 1"),
    %% sort only valid results

    %% for 'order', high is better. for 'preference', lower is better.
    SortSRV_1 = #srventry{order = 0, weight = 21, port = 1, host = "example.org"},
    SortSRV_2 = #srventry{order = 0, weight = 20, port = 2, host = "example.org"},
    SortSRV_3 = #srventry{order = 1, weight = 10, port = 3, host = "example.org"},
    SortSRV_4 = #srventry{order = 2, weight = 10, port = 4, host = "example.org"},

    SortSRV_L1 = [SortSRV_1, SortSRV_2, SortSRV_3, SortSRV_4],

    %% test that all permutations of SortSRV_L1 run through sortsrv/2
    %% results in the sorted list again
    lists:foldl(fun(H, Num) ->
			%%autotest:mark(?LINE, "sortsrv/2 - 1.~p", [Num]),
			SortSRV_L1 = lists:sort(fun sortsrv/2, H),
			Num + 1
		end, 1, test_permutations(SortSRV_L1)),

    autotest:mark(?LINE, "sortsrv/2 - 2"),
    %% sort some errors together with valid results
    SortSRV_E1 = {error, nxdomain},
    SortSRV_E2 = {error, undefined},

    SortSRV_L2 = [SortSRV_1, SortSRV_2, SortSRV_E1, SortSRV_E2],

    %% test that all permutations of SortSRV_L1 run through sortsrv/2
    %% results in the sorted list again
    lists:foldl(fun(H, Num) ->
			%%autotest:mark(?LINE, "sortsrv/2 - 2.~p", [Num]),
			SortSRV_L2 = lists:sort(fun sortsrv/2, H),
			Num + 1
		end, 1, test_permutations(SortSRV_L2)),

    autotest:mark(?LINE, "sortsrv/2 - 3"),
    %% nxdomain should beat other errors
    [SortSRV_E1, SortSRV_E2] = lists:sort(fun sortsrv/2, [SortSRV_E2, SortSRV_E1]),


    %% test number2enum([], Domain)

    %% test number2enum([C | Rest], Domain)

    %% test fixplus(Regexp)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "fixplus/1 - 1"),
    %% incorrect regexp (can't have more than one start of string (^ - circumflex) )
    "^\\+foo" = fixplus("^+foo"),

    autotest:mark(?LINE, "fixplus/1 - 2"),
    "other" = fixplus("other"),

    %% test applyregexp(Number, RegexpList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "applyregexp/1 - 1"),
    "bar" = applyregexp("Xfoobar", ["!.+foo(...)$!\\1"]),

    autotest:mark(?LINE, "applyregexp/1 - 2"),
    %% XXX this is a potential problem - the regexp "foo..." does not match "Xfoobar" - shouldn't it?
    none = applyregexp("Xfoobar", ["!foo...$!foo"]),

    autotest:mark(?LINE, "applyregexp/1 - 3"),
    %% multiple regexps, none matching
    none = applyregexp("ABC", ["!foo!foo", "!bar!123"]),

    autotest:mark(?LINE, "applyregexp/1 - 4"),
    %% multiple regexps, the first one without our token ($!)
    "123" = applyregexp("bar", ["/foo/foo", "!bar!123"]),

    autotest:mark(?LINE, "applyregexp/1 - 4"),
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

    autotest:mark(?LINE, "chooseenum/2 - 1"),
    %% single match
    [ChooseENUM_2] = chooseenum(ChooseENUM_L1, "E2U+msg"),

    autotest:mark(?LINE, "chooseenum/2 - 2"),
    %% more than one match, one matches even though it has a sub-type (":sub")
    [ChooseENUM_1, ChooseENUM_3] = chooseenum(ChooseENUM_L1, "E2U+sip"),

    autotest:mark(?LINE, "chooseenum/2 - 3 (disabled)"),
%    %% more than one match, verify that we compare case insensitively
%    %% XXX should we do case sensitive or not? Read the RFC!
%    [ChooseENUM_1, ChooseENUM_3] = chooseenum(ChooseENUM_L1, "e2u+SIP"),

    autotest:mark(?LINE, "chooseenum/2 - 4"),
    %% no match
    [] = chooseenum(ChooseENUM_L1, "E2U+foo"),


    %% test naptr_regexp(NAPTRList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "naptr_regexp/1 - 1"),
    [1] = naptr_regexp([#naptrrecord{regexp=1}]),

    autotest:mark(?LINE, "naptr_regexp/1 - 2"),
    [1, 2] = naptr_regexp([#naptrrecord{regexp=1}, #naptrrecord{regexp=2}]),

    %% test parsenaptr(Binary)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "parsenaptr/1 - 1"),
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

    autotest:mark(?LINE, "parsenaptr/1 - 2"),
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
    autotest:mark(?LINE, "parse_naptr_answer/1 - 1"),
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

    autotest:mark(?LINE, "sortenum/2 - 1"),
    %% test that all permutations of SortENUM_L1 run through sortenum/2
    %% results in the sorted list again
    lists:foldl(fun(H, Num) ->
			%%autotest:mark(?LINE, "sortenum/2 - ~p", [Num]),
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

    autotest:mark(?LINE, "filter_naptr/3 - 1"),
    %% single match
    [FilterNAPTR_4] = filter_naptr(FilterNAPTR_L1, "s", "E2U+msg"),

    autotest:mark(?LINE, "filter_naptr/3 - 2"),
    %% more than one match, but one has a sub-type (":sub") and will be ignored
    %% XXX is this correct? Does the RFC3761 say that domain NAPTRs can't have sub-type?
    [FilterNAPTR_1] = filter_naptr(FilterNAPTR_L1, "u", "E2U+sip"),

    autotest:mark(?LINE, "filter_naptr/3 - 3 (disabled)"),
%    %% one match, verify that we compare case insensitively
%    %% XXX should we do case sensitive or not? Read the RFC!
%    [FilterNAPTR_1] = filter_naptr(FilterNAPTR_L1, "e2u+SIP"),

    autotest:mark(?LINE, "filter_naptr/3 - 4"),
    %% no match
    [] = filter_naptr(FilterNAPTR_L1, "zz", "E2U+foo"),


    %% test srvlookup(Proto, Name)


    %% make_weight_proportional(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "make_weight_proportional/1 - 0"),
    MWP_E1 = #srventry{order  = 0, weight = 25, port   = 1},
    MWP_E2 = #srventry{order  = 0, weight = 25, port   = 2},
    MWP_E3 = #srventry{order  = 0, weight = 25, port   = 3},
    MWP_E4 = #srventry{order  = 0, weight = 25, port   = 4},

    MWP_L1 = [MWP_E1, MWP_E2, MWP_E3, MWP_E4],

    {MWP_T1, MWP_T2, MWP_T3} = erlang:now(),
    random:seed(MWP_T1, MWP_T2, MWP_T3),

    MWP_Res1 = [make_weight_proportional(MWP_L1) || _ <- lists:seq(1, 1000)],

    autotest:mark(?LINE, "make_weight_proportional/1 - 1"),
    MWP_E1_Count = test_mwp_count(MWP_Res1, MWP_E1, 0),
    MWP_E2_Count = test_mwp_count(MWP_Res1, MWP_E2, 0),
    MWP_E3_Count = test_mwp_count(MWP_Res1, MWP_E3, 0),
    MWP_E4_Count = test_mwp_count(MWP_Res1, MWP_E4, 0),

    io:format("SRV sorting distribution : ~5s ~5s ~5s ~5s~n", ["A", "B", "C", "D"]),
    io:format("                           ~.5w ~.5w ~.5w ~.5w~n", [MWP_E1_Count, MWP_E2_Count,
								   MWP_E3_Count, MWP_E4_Count]),

    MWP_1_Lower = 150,
    MWP_1_Upper = 350,

    case (MWP_E1_Count < MWP_1_Lower) orelse (MWP_E1_Count > MWP_1_Upper) orelse
	 (MWP_E2_Count < MWP_1_Lower) orelse (MWP_E2_Count > MWP_1_Upper) orelse
	 (MWP_E3_Count < MWP_1_Lower) orelse (MWP_E3_Count > MWP_1_Upper) orelse
	 (MWP_E4_Count < MWP_1_Lower) orelse (MWP_E3_Count > MWP_1_Upper) of
	true ->
	    Msg = io_lib:format("SRV proportional weighting failure, value ~p/~p/~p/~p out of bounds (~p..~p)",
				[MWP_E1_Count, MWP_E2_Count, MWP_E3_Count, MWP_E4_Count, MWP_1_Lower, MWP_1_Upper]),
	    throw({error, lists:flatten(Msg)});
	false ->
	    ok
    end,

    autotest:mark(?LINE, "make_weight_proportional/1 - 2"),
    %% test with error tuple (those are always last in the input to make_weight_proportional/1)
    MWP_L2 = [MWP_E1, MWP_E2, {error, nxdomain}],
    [#srventry{}, #srventry{}, {error, nxdomain}] = make_weight_proportional(MWP_L2),

    autotest:mark(?LINE, "make_weight_proportional/1 - 3"),
    %% test with only error tuple
    MWP_L3 = [{error, nxdomain}],
    MWP_L3 = make_weight_proportional(MWP_L3),

    ok.

test_mwp_count([[H | _] | T], H, Count) ->
    test_mwp_count(T, H, Count + 1);
test_mwp_count([_ | T], H, Count) ->
    test_mwp_count(T, H, Count);
test_mwp_count([], _H, Count) ->
    Count.

%% Neat function from Joe Armstrongs thesis (page 58). Creates all
%% possible permutations of the input list :
%% L = "abc" -> ["abc","acb","bac","bca","cab","cba"]
test_permutations([]) -> [[]];
test_permutations(L) ->
    [[H|T] || H <- L, T <- test_permutations(L--[H])].
