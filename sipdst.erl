%%%-------------------------------------------------------------------
%%% File    : sipdst.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Functions to resolve URL's or Via headers into sipdst
%%%           records.
%%% Created : 15 Apr 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipdst).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 url_to_dstlist/3,
	 get_response_destination/1,
	 dst2str/1,
	 debugfriendly/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: url_to_dstlist(URL, ApproxMsgSize, ReqURI)
%%           URL           = sipurl record(), the destination we
%%                           should resolve
%%           ApproxMsgSize = integer()
%%           ReqURI        = sipurl record(), the Request-URI to use
%%                           when sending a request to this transport-
%%                           layer destination
%% Descrip.: Make a list of sipdst records from an URL. We need the
%%           approximate message size to determine if we can use
%%           UDP or have to do TCP only.
%% Returns : list() of sipdst record() | {error, Reason}
%%--------------------------------------------------------------------
url_to_dstlist(URL, ApproxMsgSize, ReqURI) when is_record(URL, sipurl), is_integer(ApproxMsgSize),
						is_record(ReqURI, sipurl) ->
    Host = URL#sipurl.host,
    %% Check if URL host is either IPv4 or IPv6 address. For IPv6, host must not have surrounding brackets!
    %% Note: inet_parse:address/1 is not a supported Erlang/OTP function
    case inet_parse:address(Host) of
	{ok, _IPtuple} ->
	    Port = sipurl:get_port(URL),
	    logger:log(debug, "url_to_dstlist: ~p is an IP address, not performing domain NAPTR/SRV lookup", [Host]),
	    Proto = get_proto_from_parameters(URL),
	    case address_to_address_and_proto(Host, Proto) of
		{error, E} ->
		    logger:log(debug, "Warning: Could not make a destination of ~p:~p (~p)",
			       [Host, Port, E]),
		    {error, "Coult not make destination out of URL"};
		{ok, UseAddr, UseProto} ->
		    UsePort = sipsocket:default_port(UseProto, Port),
		    %% We don't fill in sipdst.ssl_hostname here since we are extremely unlikely
		    %% to encounter a SSL certificate for an IP address anyways.
		    [#sipdst{proto=UseProto, addr=UseAddr, port=UsePort, uri=ReqURI}]
	    end;
	_ ->
	    url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
    end.

%% part of url_to_dstlist/3 - determine protocol to use from sipurl records parameters
%% Returns : tcp | udp | tls
get_proto_from_parameters(URL) when is_record(URL, sipurl) ->
    %% Find requested transport in URL parameters and lowercase it
    case url_param:find(URL#sipurl.param_pairs, "transport") of
	[Transport] ->
	    case httpd_util:to_lower(Transport) of
		"tcp" -> tcp;
		"udp" -> udp;
		"tls" -> tls;
		Unknown ->
		    %% RFC3263 4.1 says we SHOULD use UDP for sip: and TCP for sips: when target is IP
		    %% and no transport is indicated in the parameters
		    case URL#sipurl.proto of
			"sips" ->
			    logger:log(debug, "url_to_dstlist: transport protocol ~p not recognized,"
				       " defaulting to TLS for SIPS URL", [Unknown]),
			    tls;
			_ ->
			    logger:log(debug, "url_to_dstlist: transport protocol ~p not recognized,"
				       " defaulting to UDP for non-SIPS URL", [Unknown]),
			    udp
		    end
	    end;
	_ ->
	    %% RFC3263 #4.1 (Selecting a Transport Protocol) "Otherwise, if no transport
	    %% protocol is specified, but the TARGET is a numeric IP address, the client
	    %% SHOULD use UDP for a SIP URI, and TCP for a SIPS URI
	    case (URL#sipurl.proto == "sips") of
		true -> tls;
		false -> udp
	    end
    end.

%%--------------------------------------------------------------------
%% Function: get_response_destination(TopVia)
%%           TopVia = via record()
%% Descrip.: Turn the top Via header from a response into a sipdst
%%           record with the protocol, host and port the response
%%           should be sent to.
%% Returns : sipdst record() |
%%           error
%%--------------------------------------------------------------------
get_response_destination(TopVia) when is_record(TopVia, via) ->
    case get_response_host_proto(TopVia) of
        {ok, Host, Proto} ->
            {ViaPort, Parameters} = {TopVia#via.port, TopVia#via.param},
	    ParamDict = sipheader:param_to_dict(Parameters),
	    Port = case dict:find("rport", ParamDict) of
                       {ok, []} ->
                           %% This must be an error response generated before the rport fix-up. Ignore rport.
                           sipsocket:default_port(Proto, ViaPort);
		       {ok, Rport} ->
                           list_to_integer(Rport);
                       _ ->
                           sipsocket:default_port(Proto, ViaPort)
                   end,
	    #sipdst{proto=Proto, addr=Host, port=Port};
	_ ->
	    error
    end.

%%--------------------------------------------------------------------
%% Function: dst2str(Dst)
%%           Dst = sipdst record()
%% Descrip.: Turn a sipdst into something printable (for debugging)
%% Returns : DstString, Dst as string()
%%--------------------------------------------------------------------

%%
%% URI present
%%
dst2str(Dst) when is_record(Dst, sipdst), Dst#sipdst.uri /= undefined ->
    lists:flatten(io_lib:format("~p:~s:~p (~s)",
				[Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port,
				 sipurl:print(Dst#sipdst.uri)]));

%%
%% No URI, for example Response sipdst record
dst2str(Dst) when is_record(Dst, sipdst) ->
    lists:flatten(io_lib:format("~p:~s:~p", [Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port])).

debugfriendly(Dst) when is_record(Dst, sipdst) ->
    debugfriendly2([Dst], []);
debugfriendly(L) ->
    debugfriendly2(L, []).

debugfriendly2([], Res) ->
    lists:reverse(Res);
debugfriendly2([H|T], Res) when is_record(H, sipdst) ->
    Str = dst2str(H),
    debugfriendly2(T, [Str | Res]).


%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
%%           URL           = sipurl record(), destination
%%           ApproxMsgSize = integer()
%%           ReqURI        = sipurl record(), original Request-URI
%% Descrip.: Called from url_to_dstlist/1 when the Host part of the
%%           URI was not an IP address
%% Returns : DstList         |
%%           {error, Reason}
%%           DstList = list() of sipdst record()
%%--------------------------------------------------------------------

%%
%% URL port specified
%% XXX this guard makes assumptions about an element inside the sipurl record - we'd better not.
%%
url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
  when is_record(URL, sipurl), is_integer(ApproxMsgSize), is_integer(URL#sipurl.port), is_record(ReqURI, sipurl) ->
    %% RFC3263 #4.1 (Selecting a Transport Protocol) "Similarly, if no transport protocol is specified,
    %% and the TARGET is not numeric, but an explicit port is provided, the client SHOULD use UDP for a
    %% SIP URI, and TCP for a SIPS URI"
    case (URL#sipurl.proto == "sips") of
	true ->
	    logger:log(debug, "Resolver: Port was explicitly supplied and URL protocol is SIPS - only try TLS"),
	    host_port_to_dstlist(tls, URL#sipurl.host, sipurl:get_port(URL), ReqURI, URL#sipurl.host);
	false ->
	    UDP = host_port_to_dstlist(udp, URL#sipurl.host, sipurl:get_port(URL), ReqURI, URL#sipurl.host),
	    case ApproxMsgSize > 1200 of
		true ->
		    %% RFC3263 #4.1 "... use UDP for SIP URI ... However, another transport, such as TCP,
		    %% MAY be used if the guidelines of SIP mandate it for this particular request.
		    %% That is the case, for example, for requests that exceed the path MTU."
		    logger:log(debug, "Resolver: Port was explicitly supplied, and size of message is > 1200."
			       " Try TCP and then UDP."),
		    TCP = change_sipdst_protocol(tcp, UDP),
		    combine_host_portres([TCP, UDP]);
		false ->
		    %% RFC3263 #4.1 "... an explicit port is provided, the client SHOULD use UDP for a SIP URI,
		    %% and TCP for a SIPS URI.  This is because UDP is the only mandatory transport in RFC 2543,
		    %% and thus the only one guaranteed to be interoperable for a SIP URI."
		    logger:log(debug, "Resolver: Port was explicitly supplied, and size of message is <= 1200."
			       " Try UDP only."),
		    UDP
	    end
    end;

%%
%% URL port NOT specified, do SRV lookup on host from URL
%% XXX this guard makes assumptions about an element inside the sipurl record - we'd better not.
%%
url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
  when is_record(URL, sipurl), is_integer(ApproxMsgSize), is_record(ReqURI, sipurl), URL#sipurl.port == none ->
    %% RFC3263 #4.1 "Otherwise, if no transport protocol or port is specified, and the target is not
    %% a numeric IP address, the client SHOULD perform a NAPTR query for the domain in the URI.".
    %%
    %% XXX check for transport here!
    case dnsutil:siplookup(URL#sipurl.host) of
	{error, nxdomain} ->
	    %% A SRV-lookup of the Host part of the URL returned NXDOMAIN, this is
	    %% not an error and we will now try to resolve the Host-part directly
	    %% (look for A or AAAA record)
	    UDP = host_port_to_dstlist(udp, URL#sipurl.host, sipurl:get_port(URL), ReqURI, URL#sipurl.host),
	    case ApproxMsgSize > 1200 of
		true ->
		    logger:log(debug, "Warning: ~p has no NAPTR/SRV records in DNS, and the message size "
			       "is > 1200 bytes. Resolving hostname and trying TCP and then UDP.",
			       [URL#sipurl.host]),
		    TCP = change_sipdst_protocol(tcp, UDP),
		    combine_host_portres([TCP, UDP]);
		false ->
		    logger:log(debug, "Warning: ~p has no NAPTR/SRV records in DNS, and the message size "
			       "is =< 1200 bytes. Resolving hostname and defaulting to UDP (only).",
			       [URL#sipurl.host]),
		    UDP
	    end;
	{error, What} ->
	    {error, What};
	DstList when is_list(DstList) ->
	    %% For SIPS URI, we MUST remove any destination that is not TLS or known to be
	    %% protected through some TLS equivalent mechanism (like IPsec)
	    DstList2 = case (URL#sipurl.proto == "sips") of
			   true ->
			       remove_non_tls_destinations(DstList);
			   false ->
			       DstList
		       end,
	    DstList3 = case yxa_config:get_env(tls_disable_client) of
			   {ok, true} ->
			       remove_tls_destinations(DstList2);
			   {ok, false} ->
			       DstList2
		       end,
	    format_siplookup_result(sipurl:get_port(URL), ReqURI, URL#sipurl.host, DstList3)
    end.

%%--------------------------------------------------------------------
%% Function: change_sipdst_protocol(Proto, DstList)
%%           Proto   = atom(), tcp | udp | tls
%%           DstList = list() of sipdst record() |
%%                     {error, Reason} tuple()
%% Descrip.: Go through a list of sipdst records. For each entry,
%%           change the records 'proto' element into Proto or, if the
%%           old protocol was a v6-protocol, into the v6 variant of
%%           Proto.
%% Returns : list() of Entry | {error, Reason}
%%--------------------------------------------------------------------
change_sipdst_protocol(Proto, {error, Reason}) when is_atom(Proto) ->
    {error, Reason};
change_sipdst_protocol(Proto, In) when is_atom(Proto), is_list(In) ->
    Proto6 = case Proto of
		 tcp -> tcp6;
		 udp -> udp6;
		 tls -> tls6
	     end,
    change_sipdst_protocol2(Proto, Proto6, In, []).

%%
%% OldProto is v4, change to Proto
%%
change_sipdst_protocol2(Proto, Proto6, [#sipdst{proto=OldProto}=H | T], Res)
  when is_record(H, sipdst), OldProto == tcp; OldProto == udp; OldProto == tls ->
    This = H#sipdst{proto=Proto},
    change_sipdst_protocol2(Proto, Proto6, T, [This | Res]);
%%
%% OldProto is v6, change to Proto6
%%
change_sipdst_protocol2(Proto, Proto6, [#sipdst{proto=OldProto}=H | T], Res)
  when is_record(H, sipdst), OldProto == tcp6; OldProto == udp6; OldProto == tls6 ->
    This = H#sipdst{proto=Proto6},
    change_sipdst_protocol2(Proto, Proto6, T, [This | Res]);
%%
%% No more entrys
%%
change_sipdst_protocol2(_Proto, _Proto6, [], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% Function: combine_host_portres(In)
%%           In = list() of sipdst record() | {error, Reason} tuple()
%% Descrip.: Weed out the sipdst records (if any) from In, preserving
%%           order. If there are only {error, Reason} tuples, return
%%           the first one.
%% Returns : list() of sipdst record() |
%%           {error, Reason}
%%           Reason = term()
%%--------------------------------------------------------------------
combine_host_portres(In) when is_list(In) ->
    combine_host_portres2(lists:flatten(In), [], []).

combine_host_portres2([{error, _Reason}=H | T], Res, ERes) ->
    %% error, put in ERes
    combine_host_portres2(T, Res, [H | ERes]);
combine_host_portres2([H | T], Res, ERes) when is_record(H, sipdst) ->
    %% non-error, put in Res
    combine_host_portres2(T, [H | Res], ERes);
combine_host_portres2([], [], ERes) ->
    %% no more input, and nothing in res - return the first error
    [FirstError | _] = lists:reverse(ERes),
    FirstError;
combine_host_portres2([], Res, _ERes) ->
    %% no more input, and something in Res (we know that since the above
    %% function declaration would have matched on Res = [].
    lists:reverse(Res).

%%--------------------------------------------------------------------
%% Function: remove_tls_destinations(SRVList)
%%           SRVList = list() of sipdns_srv record()
%% Descrip.: Remove all records having proto 'tls' | tls6 from In, and
%%           return a new list() of sipdns_srv record().
%% Returns : DstList = list() of sipdns_srv record().
%%--------------------------------------------------------------------
remove_tls_destinations(SRVList) ->
    remove_tls_destinations2(SRVList, []).

remove_tls_destinations2([], Res) ->
    lists:reverse(Res);
remove_tls_destinations2([#sipdns_srv{proto=Proto}=H | T], Res) when Proto /= tls, Proto /= tls6 ->
    remove_tls_destinations2(T, [H | Res]);
remove_tls_destinations2([H | T], Res) when is_record(H, sipdns_srv) ->
    %% Proto is tls or tls6
    {Proto, Host, Port} = {H#sipdns_srv.proto, H#sipdns_srv.host, H#sipdns_srv.port},
    logger:log(debug, "Resolver: Removing TLS destination ~p:~s:~p from result set since "
	       "experimental TLS is not enabled", [Proto, Host, Port]),
    remove_tls_destinations2(T, Res).


%%--------------------------------------------------------------------
%% Function: remove_non_tls_destinations(In)
%%           In = list() of sipdns_srv record()
%% Descrip.: Remove all records NOT having proto 'tls' | tls6 from In,
%%           and return a new list() of sipdns_srv record().
%% Returns : DstList = list() of sipdns_srv record()
%%--------------------------------------------------------------------
remove_non_tls_destinations(In) ->
    case remove_non_tls_destinations2(In, 0, []) of
	{ok, 0, _Res} ->
	    In;
	{ok, RemoveCount, Res} ->
	    logger:log(debug, "Resolver: Removed ~p non-TLS destinations", [RemoveCount]),
	    Res
    end.

remove_non_tls_destinations2([], RCount, Res) ->
    {ok, RCount, lists:reverse(Res)};
remove_non_tls_destinations2([#sipdns_srv{proto=Proto}=H | T], RCount, Res) when Proto == tls; Proto == tls6 ->
    remove_non_tls_destinations2(T, RCount, [H | Res]);
remove_non_tls_destinations2([H | T], RCount, Res) when is_record(H, sipdns_srv) ->
    %% Proto is NOT tls or tls6, check if we should consider it a secure destination anyways
    %% (for example, it might be protected by IPsec)
    {Proto, Host, Port} = {H#sipdns_srv.proto, H#sipdns_srv.host, H#sipdns_srv.port},
    case local:is_tls_equivalent(Proto, Host, Port) of
	true ->
	    %% host:port is protected by some TLS equivalent mechanism
	    remove_non_tls_destinations2(T, RCount, [H | Res]);
	X when X == false; X == undefined ->
	    remove_non_tls_destinations2(T, RCount + 1, Res)
    end.

%%--------------------------------------------------------------------
%% Function: host_port_to_dstlist(Proto, InHost, InPort, URI, SSLHost)
%%           Proto   = atom(), tcp | udp | tls
%%           Host    = string()
%%           Port    = integer() | none
%%           URI     = sipurl record() to put in the created sipdst
%%                     records
%%           SSLHost = string() | undefined, SSL hostname to expect
%% Descrip.: Resolves a hostname and returns a list of sipdst
%%           records of the protocol requested.
%%           InPort should either be an integer, or the atom 'none'
%%           to use the default port for the protocol.
%% Returns : DstList         |
%%           {error, Reason}
%%           DstList = list() of sipdst record()
%%--------------------------------------------------------------------
host_port_to_dstlist(Proto, Host, Port, URI, SSLHost) when is_integer(Port) ; Port == none,
							   is_record(URI, sipurl),
							   is_list(SSLHost) ; SSLHost == undefined ->
    case dnsutil:get_ip_port(Host, Port) of
	{error, What} ->
	    {error, What};
	L when is_list(L) ->
	    %% L is a list of sipdns_hostport record()
	    make_sipdst_from_hostport(Proto, URI, SSLHost, L)
    end.

%%--------------------------------------------------------------------
%% Function: make_sipdst_from_hostport(Proto, URI, SSLHost, In)
%%           Proto   = atom(), tcp | udp | tls
%%           URI     = sipurl record(), URI to stick into the
%%                     resulting sipdst records
%%           SSLHost = string() | undefined, SSL hostname to expect
%%           In      = list() of sipdns_hostport record() - typically
%%                     the result of a call to dnsutil:get_ip_port()
%% Descrip.: Turns the result of a dnsutil:get_ip_port() into a list
%%           of sipdst records. get_ip_port() return a list of tuples
%%           like {Inet, Addr, Port} where IP is a string ("10.0.0.1",
%%           "[2001:6b0:5:987::1]"). Inet is 'inet' or 'inet6' and
%%           we need to know to convert Proto as necessary. The order
%%           of the input tuples is preserved in the resulting list.
%% Returns : DstList | {error, Reason}
%%           DstList = list() of sipdst record()
%%           Reason  = string()
%%--------------------------------------------------------------------
make_sipdst_from_hostport(Proto, URI, SSLHost, In) when Proto == tcp; Proto == udp; Proto == tls,
							is_record(URI, sipurl), is_list(SSLHost), is_list(In) ->
    make_sipdst_from_hostport2(Proto, URI, SSLHost, In, []).

%%
%% sipdns_hostport.family == inet
%%
make_sipdst_from_hostport2(Proto, URI, SSLHost, [#sipdns_hostport{family=inet}=H | T], Res) ->
    UsePort = sipsocket:default_port(Proto, H#sipdns_hostport.port),
    This = #sipdst{proto=Proto,
		   addr=H#sipdns_hostport.addr,
		   port=UsePort,
		   uri=URI,
		   ssl_hostname=SSLHost},
    make_sipdst_from_hostport2(Proto, URI, SSLHost, T, [This | Res]);
%%
%% sipdns_hostport.family == inet6
%%
make_sipdst_from_hostport2(Proto, URI, SSLHost, [#sipdns_hostport{family=inet6}=H | T], Res) ->
    %% inet6 family, must turn Proto into IPv6 variant
    UseProto = case Proto of
		   tcp -> tcp6;
		   udp -> udp6;
		   tls -> tls6
	       end,
    UsePort = sipsocket:default_port(UseProto, H#sipdns_hostport.port),
    This = #sipdst{proto=UseProto,
		   addr=H#sipdns_hostport.addr,
		   port=UsePort,
		   uri=URI,
		   ssl_hostname=SSLHost},
    make_sipdst_from_hostport2(Proto, URI, SSLHost, T, [This | Res]);
%%
%% No more input
%%
make_sipdst_from_hostport2(_Proto, _URI, _SSLHost, [], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% Function: format_siplookup_result(InPort, ReqURI, SSLHost, DstList)
%%           InPort        = integer() | none
%%           ReqURI        = sipurl record()
%%           SSLHost       = string() | undefined
%%           DstList       = list() of {Proto, Host, Port} tuple()
%% Descrip.: Turns the result of a dnsutil:siplookup() into a list
%%           of sipdst records. The ordering is preserved.
%% Returns : DstList, list() of sipdst record()
%%--------------------------------------------------------------------
format_siplookup_result(InPort, ReqURI, SSLHost, DstList)
  when is_integer(InPort) ; InPort == none, is_record(ReqURI, sipurl), is_list(SSLHost) ; SSLHost == undefined,
       is_list(DstList) ->
    format_siplookup_result2(InPort, ReqURI, SSLHost, DstList, []).

format_siplookup_result2(_InPort, _ReqURI, _SSLHost, [], Res) ->
    Res;
format_siplookup_result2(InPort, ReqURI, SSLHost, [H | T], Res) when is_record(H, sipdns_srv) ->
    {Proto, Host, Port} = {H#sipdns_srv.proto, H#sipdns_srv.host, H#sipdns_srv.port},
    %% If InPort is 'none', then use the port from DNS. Otherwise, use InPort.
    %% This is to handle if we for example receive a request with a Request-URI of
    %% sip.example.net:5070, and sip.example.net has SRV-records saying port 5060. In
    %% that case, we should still send our request to port 5070.
    UsePort = case InPort of
		  _ when is_integer(InPort) -> InPort;
		  none -> Port
	      end,
    if
	UsePort /= Port ->
	    logger:log(debug, "Warning: ~p is specified to use port ~p in DNS,"
		       " but I'm going to use the supplied port ~p instead",
		       [Host, Port, UsePort]);
	true -> true
    end,
    %% XXX what if host_port_to_dstlist for this hostname returns a sipdst-record with a
    %% different address (because of DNS round-robin, or DNS TTL reaching zero) the second
    %% time we query for the very same hostname returned in a siplookup result set?
    DstList = case host_port_to_dstlist(Proto, Host, UsePort, ReqURI, SSLHost) of
		  {error, Reason} ->
		      logger:log(error, "Warning: Could not make DstList out of ~p:~p:~p : ~p",
				 [Proto, Host, UsePort, Reason]),
		      [];
		  L when is_list(L) ->
		      L
	      end,
    format_siplookup_result2(InPort, ReqURI, SSLHost, T, lists:append(Res, DstList)).


%%--------------------------------------------------------------------
%% Function: get_response_host_port(TopVia)
%%           TopVia = via record()
%% Descrip.: Argument is the top Via header in a response, this
%%           function extracts the destination and protocol we
%%           should use.
%% Returns : {ok, Address, Proto} |
%%           error
%%           Address = string() that might be IPv4 address (from
%%                     received=), IPv6 address (from received=), or
%%                     whatever was in the host part of the Via.
%%           Proto   = atom(), tcp | udp | tcp6 | udp6 | tls | tls6
%%--------------------------------------------------------------------
get_response_host_proto(TopVia) when is_record(TopVia, via) ->
    {Protocol, Host, Parameters} = {TopVia#via.proto, TopVia#via.host, TopVia#via.param},
    ParamDict = sipheader:param_to_dict(Parameters),
    Proto = sipsocket:viastr2proto(Protocol),
    case dict:find("received", ParamDict) of
	{ok, Received} ->
	    case address_to_address_and_proto(Received, Proto) of
		{error, E1} ->
		    logger:log(debug, "Warning: Malformed received= parameter (~p) : ~p", [Received, E1]),
		    %% received= parameter not usable, try host part of Via instead
		    %% XXX try to resolve host part of Via if necessary
		    case address_to_address_and_proto(Host, Proto) of
			{error, E2} ->
			    logger:log(debug, "Warning: Invalid host part of Via (~p) : ~p", [Host, E2]),
			    logger:log(error, "Failed getting a response destination out of Via : ~p", [TopVia]),
			    error;
			{ok, Address1, Proto1} ->
			    {ok, Address1, Proto1}
		    end;
		{ok, Address1, Proto1} ->
		    {ok, Address1, Proto1}
	    end;
	_ ->
	    %% There was no received= parameter. Do the same checks but on the Via
	    %% hostname (which is then almost certainly an IP-address).
	    case address_to_address_and_proto(Host, Proto) of
		{error, E1} ->
		    logger:log(debug, "Warning: No received= and invalid host part of Via (~p) : ~p", [Host, E1]),
		    logger:log(error, "Failed getting a response destination out of Via : ~p", [TopVia]),
		    error;
		{ok, Address1, Proto1} ->
		    {ok, Address1, Proto1}
	    end
    end.


%%--------------------------------------------------------------------
%% Function: address_to_address_and_proto(Addr, DefaultProto)
%%           Addr = term(), something (probably a string()) that is
%%                  parseable by inet_parse:ipv{4,6}_address() (should
%%                  be an IPv4 or IPv6 address, not a hostname!)
%%           DefaultProto = atom(), tcp | udp | tls
%% Descrip.: When looking at Via headers, we often have a protocol
%%           from the SIP/2.0/FOO but we need to look at the
%%           address to determine if our sipdst proto should be
%%           foo or foo6. This function does that.
%% Returns : {ok, Address, Proto} |
%%           {error, Reason}
%%           Address = term(), parsed version of Addr
%%           Proto   = atom(), tcp | udp | tcp6 | udp6 | tls | tls6
%%           Reason  = string()
%%--------------------------------------------------------------------
address_to_address_and_proto(Addr, DefaultProto) when DefaultProto == tcp; DefaultProto == udp; DefaultProto == tls ->
    case inet_parse:ipv4_address(Addr) of
	{ok, _IPtuple} ->
	    {ok, Addr, DefaultProto};
	_ ->
	    case yxa_config:get_env(enable_v6) of
		{ok, true} ->
		    %% Check if it matches IPv6 address syntax
		    case inet_parse:ipv6_address(util:remove_v6_brackets(Addr)) of
			{ok, _IPtuple} ->
			    Proto6 = case DefaultProto of
					 tcp -> tcp6;
					 udp -> udp6;
					 tls -> tls6
				     end,
			    {ok, Addr, Proto6};
			_ ->
			    {error, "not an IPv4 or IPv6 address"}
		    end;
		{ok, false} ->
		    {error, "not an IPv4 address"}
	    end
    end.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%%--------------------------------------------------------------------
test() ->

    %% test remove_tls_destinations(SRVList)
    %%--------------------------------------------------------------------
    TCP4 = #sipdns_srv{proto=tcp, host="192.0.2.1", port=5060},
    UDP4 = #sipdns_srv{proto=udp, host="192.0.2.2", port=5060},
    TLS4 = #sipdns_srv{proto=tls, host="192.0.2.3", port=5061},
    TCP6 = #sipdns_srv{proto=tcp6, host="[2001:6b0:5:987::1]", port=none},
    UDP6 = #sipdns_srv{proto=udp6, host="[2001:6b0:5:987::2]", port=none},
    TLS6 = #sipdns_srv{proto=tls6, host="[2001:6b0:5:987::3]", port=none},

    io:format("test: remove_tls_destinations/2 - 1~n"),
    [TCP4, UDP4] = remove_tls_destinations([TCP4, UDP4]),

    io:format("test: remove_tls_destinations/2 - 2~n"),
    [UDP4] = remove_tls_destinations([UDP4]),

    io:format("test: remove_tls_destinations/2 - 3~n"),
    [TCP4] = remove_tls_destinations([TCP4, TLS4]),

    io:format("test: remove_tls_destinations/2 - 4~n"),
    [] = remove_tls_destinations([TLS4]),

    io:format("test: remove_tls_destinations/2 - 5~n"),
    [] = remove_tls_destinations([TLS6]),

    io:format("test: remove_tls_destinations/2 - 6~n"),
    [] = remove_tls_destinations([TLS6, TLS4, TLS6, TLS4]),

    io:format("test: remove_tls_destinations/2 - 7~n"),
    [TCP6, UDP6] = remove_tls_destinations([TLS6, TCP6, TLS4, UDP6, TLS4]),


    %% test remove_non_tls_destinations(SRVList)
    %%--------------------------------------------------------------------
    io:format("test: remove_non_tls_destinations/2 - 1~n"),
    %% test normal case
    [TLS4, TLS6] = remove_non_tls_destinations([TLS4, TCP6, TLS6, UDP6]),


    %% test make_sipdst_from_hostport(Proto, URI, SSLHost, In)
    %%--------------------------------------------------------------------
    URL = sipurl:parse("sip:ft@example.org:1234"),

    io:format("test: make_sipdst_from_hostport/3 - 1~n"),
    %% simple case, tcp and no supplied port in the tuple
    HostPort1 = #sipdns_hostport{family=inet, addr="address", port=none},
    Dst1 = #sipdst{proto=tcp, addr="address", port=5060, uri=URL, ssl_hostname="ssl1"},
    [Dst1] = make_sipdst_from_hostport(tcp, URL, "ssl1", [HostPort1]),

    io:format("test: make_sipdst_from_hostport/3 - 2~n"),
    %% tcp 'upped' to tcp6 since tuple protocol is inet6. port from tuple used.
    HostPort2 = #sipdns_hostport{family=inet6, addr="address", port=5070},
    Dst2 = #sipdst{proto=tcp6, addr="address", port=5070, uri=URL, ssl_hostname=undefined},
    [Dst2] = make_sipdst_from_hostport(tcp, URL, undefined, [HostPort2]),

    io:format("test: make_sipdst_from_hostport/3 - 3~n"),
    %% mixed
    Dst2_2 = Dst2#sipdst{ssl_hostname="ssl1"},
    [Dst1, Dst2_2] =
	make_sipdst_from_hostport(tcp, URL, "ssl1", [HostPort1, HostPort2]),


    %% test get_response_host_proto(TopVia)
    %% NOTE: We can currently only test with IPv4 addresses since IPv6 is
    %% off by default, so when the tests are run enable_v6 might not be
    %% 'true'.
    %%--------------------------------------------------------------------
    io:format("test: get_response_host_proto/1 - 1~n"),
    %% straight forward, no received= parameter
    TopVia1 = #via{proto="SIP/2.0/TCP", host="192.0.2.1", param=[]},
    {ok, "192.0.2.1", tcp} = get_response_host_proto(TopVia1),

    io:format("test: get_response_host_proto/1 - 2~n"),
    %% straight forward, address in received= parameter
    TopVia2 = #via{proto="SIP/2.0/TCP", host="phone.example.org", param=["received=192.0.2.1"]},
    {ok, "192.0.2.1", tcp} = get_response_host_proto(TopVia2),

    io:format("test: get_response_host_proto/1 - 3~n"),
    %% error, hostname in Via host and no received= parameter
    TopVia3 = #via{proto="SIP/2.0/TCP", host="phone.example.org", param=[]},
    error = get_response_host_proto(TopVia3),

    io:format("test: get_response_host_proto/1 - 4~n"),
    %% invalid received= parameter, but luckily valid IP address in Via host
    TopVia4 = #via{proto="SIP/2.0/TLS", host="192.0.2.1", param=["received=X"]},
    {ok, "192.0.2.1", tls} = get_response_host_proto(TopVia4),


    %% test format_siplookup_result(InPort, ReqURI, SSLHost, DstList)
    %%--------------------------------------------------------------------
    io:format("test: format_siplookup_result/4 - 1~n"),
    %% InPort 'none', use the one from DNS
    SRV3 = #sipdns_srv{proto=tcp, host="192.0.2.1", port=1234},
    Dst3 = #sipdst{proto=tcp, addr="192.0.2.1", port=1234, uri=URL},
    [Dst3] = format_siplookup_result(none, URL, undefined, [SRV3]),

    io:format("test: format_siplookup_result/4 - 2~n"),
    %% InPort 2345, overrides the one in DNS (1234 in Tuple3)
    Dst4 = #sipdst{proto=tcp, addr="192.0.2.1", port=2345, uri=URL},
    [Dst4] = format_siplookup_result(2345, URL, undefined, [SRV3]),

    io:format("test: format_siplookup_result/4 - 3~n"),
    SRV5 = #sipdns_srv{proto=tcp, host="192.0.2.2", port=5065},
    Dst5 = #sipdst{proto=tcp, addr="192.0.2.2", port=5065, uri=URL},
    %% more than one tuple in
    [Dst3, Dst5] = format_siplookup_result(none, URL, undefined, [SRV3, SRV5]),


    %% test combine_host_portres(In)
    %%--------------------------------------------------------------------
    io:format("test: combine_host_portres/1 - 1~n"),
    %% test with only a single error
    {error, 1} = combine_host_portres([{error, 1}]),

    io:format("test: combine_host_portres/1 - 2~n"),
    %% test with only errors
    {error, 1} = combine_host_portres([{error, 1}, {error, 2}]),

    io:format("test: combine_host_portres/1 - 3~n"),
    %% test with errors, and one valid sipdst
    [#sipdst{proto=1}] = combine_host_portres([{error, 1}, {error, 2}, #sipdst{proto=1}]),

    io:format("test: combine_host_portres/1 - 4~n"),
    %% test with errors, and two valid sipdst's
    [#sipdst{proto=1}, #sipdst{proto=2}] =
	combine_host_portres([#sipdst{proto=1}, {error, 1}, {error, 2}, #sipdst{proto=2}]),

    io:format("test: combine_host_portres/1 - 5~n"),
    %% test with three valid sipdst's only
    [#sipdst{proto=1}, #sipdst{proto=2}, #sipdst{proto=3}] =
	combine_host_portres([[#sipdst{proto=1}, #sipdst{proto=2}], [#sipdst{proto=3}]]),


    %% test get_proto_from_parameters(URL)
    %%--------------------------------------------------------------------
    io:format("test: get_proto_from_parameters/1 - 1~n"),
    %% test that we default to UDP
    udp = get_proto_from_parameters( sipurl:parse("sip:ft@example.org") ),

    io:format("test: get_proto_from_parameters/1 - 2~n"),
    %% test UDP protocol specified
    udp = get_proto_from_parameters( sipurl:parse("sip:ft@example.org;transport=UDP") ),

    io:format("test: get_proto_from_parameters/1 - 3~n"),
    %% test TCP protocol specified, and strange casing
    tcp = get_proto_from_parameters( sipurl:parse("sip:ft@example.org;transport=tCp") ),

    io:format("test: get_proto_from_parameters/1 - 4~n"),
    %% test unknown transport parameter and SIPS URL
    tls = get_proto_from_parameters( sipurl:parse("sips:ft@example.org;transport=foobar") ),

    io:format("test: get_proto_from_parameters/1 - 4~n"),
    %% test unknown transport parameter and non-SIPS URL
    udp = get_proto_from_parameters( sipurl:parse("sip:ft@example.org;transport=foobar") ),


    %% test change_sipdst_protocol(Proto, DstList)
    %%--------------------------------------------------------------------
    CSP_TCP = #sipdst{proto=tcp},
    CSP_UDP = #sipdst{proto=udp},
    CSP_TLS = #sipdst{proto=tls},

    CSP_TCP6 = #sipdst{proto=tcp6},
    CSP_UDP6 = #sipdst{proto=udp6},
    CSP_TLS6 = #sipdst{proto=tls6},

    io:format("test: change_sipdst_protocol/2 - 1~n"),
    %% test no change
    [CSP_TCP] = change_sipdst_protocol(tcp, [CSP_TCP]),

    io:format("test: change_sipdst_protocol/2 - 2~n"),
    %% test no change with mixed v4/v6 and error
    [CSP_TCP, CSP_TCP6] = change_sipdst_protocol(tcp, [CSP_TCP, CSP_TCP6]),

    io:format("test: change_sipdst_protocol/2 - 3.1~n"),
    %% test simple change (to tcp)
    [CSP_TCP, CSP_TCP, CSP_TCP, CSP_TCP6, CSP_TCP6, CSP_TCP6] =
	change_sipdst_protocol(tcp, [CSP_UDP, CSP_TLS, CSP_TCP, CSP_TCP6, CSP_UDP6, CSP_TLS6]),

    io:format("test: change_sipdst_protocol/2 - 3.2~n"),
    %% test simple change (to udp)
    [CSP_UDP, CSP_UDP, CSP_UDP, CSP_UDP6, CSP_UDP6, CSP_UDP6] =
	change_sipdst_protocol(udp, [CSP_UDP, CSP_TLS, CSP_TCP, CSP_TCP6, CSP_UDP6, CSP_TLS6]),

    io:format("test: change_sipdst_protocol/2 - 3.3~n"),
    %% test simple change (to tls)
    [CSP_TLS, CSP_TLS, CSP_TLS, CSP_TLS6, CSP_TLS6, CSP_TLS6] =
	change_sipdst_protocol(tls, [CSP_UDP, CSP_TLS, CSP_TCP, CSP_TCP6, CSP_UDP6, CSP_TLS6]),

    io:format("test: change_sipdst_protocol/2 - 4~n"),
    %% test simple v6-to-v6 change
    [CSP_TCP6] = change_sipdst_protocol(tcp, [CSP_UDP6]),

    io:format("test: change_sipdst_protocol/2 - 5~n"),
    %% test multiple v6-to-v6 change
    [CSP_TCP6, CSP_TCP6, CSP_TCP6] = change_sipdst_protocol(tcp, [CSP_UDP6, CSP_TCP6, CSP_TLS6]),

    io:format("test: change_sipdst_protocol/2 - 6~n"),
    %% test error tuple
    {error, undefined} = change_sipdst_protocol(tcp, {error, undefined}),

    ok.
