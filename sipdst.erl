%%%-------------------------------------------------------------------
%%% File    : sipdst.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Functions to resolve URL's or Via headers into sipdst
%%%           records.
%%% Created : 15 Apr 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipdst).

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
%%           ReqURI        = sipurl record(), the original
%%                           Request-URI of the request
%% Descrip.: Make a list of sipdst records from an URL. We need the
%%           approximate message size to determine if we can use
%%           UDP or have to do TCP only.
%% Returns : list() of sipdst record() | {error, Reason}
%%--------------------------------------------------------------------
url_to_dstlist(URL, ApproxMsgSize, ReqURI) when is_record(URL, sipurl), is_integer(ApproxMsgSize), is_record(ReqURI, sipurl) ->
    {Host, Port, Parameters} = {URL#sipurl.host, sipurl:get_port(URL), URL#sipurl.param},
    %% Check if Host is either IPv4 or IPv6 address
    %% XXX module (and function) not listed in erlang documentation but
    %% included in Erlang/OTP source
    case inet_parse:address(Host) of
	{ok, _} ->
	    logger:log(debug, "url_to_dstlist: ~p is an IP address, not performing domain NAPTR/SRV lookup", [Host]),
	    ParamDict = sipheader:param_to_dict(Parameters),
	    %% Find requested transport from Parameters and lowercase it
	    TransportStr = case dict:find("transport", ParamDict) of
			       {ok, V} ->
				   httpd_util:to_lower(V);
			       _ ->
				   none
			   end,
	    Proto = case TransportStr of
			none -> udp;
			"tcp" -> tcp;
			"udp" -> udp;
			Unknown ->
			    %% RFC3263 4.1 says we SHOULD use UDP for sip: and TCP for sips: when target is IP
			    %% and no transport is indicated in the parameters
			    case URL#sipurl.proto of
				"sips" ->
				    logger:log(debug, "url_to_dstlist: transport protocol ~p not recognized,"
					       " defaulting to TCP for sips: URL", [Unknown]),
				    tcp;
				_ ->
				    logger:log(debug, "url_to_dstlist: transport protocol ~p not recognized,"
					       " defaulting to UDP for non-sips: URL", [Unknown]),
				    udp
			    end
		    end,
	    case address_to_address_and_proto(Host, Proto) of
		{error, E} ->
		    logger:log(debug, "Warning: Could not make a destination of ~p:~p (~p)",
			       [Host, Port, E]),
		    {error, "Coult not make destination out of URL"};
		{UseAddr, UseProto} ->
		    UsePort = siprequest:default_port(UseProto, Port),
		    [#sipdst{proto=UseProto, addr=UseAddr, port=UsePort, uri=ReqURI}]
	    end;
	_ ->
	    url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
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
        {Host, Proto} ->
            {ViaPort, Parameters} = {TopVia#via.port, TopVia#via.param},
	    ParamDict = sipheader:param_to_dict(Parameters),
	    Port = case dict:find("rport", ParamDict) of
                       {ok, []} ->
                           %% This must be an error response generated before the rport fix-up. Ignore rport.
                           siprequest:default_port(Proto, ViaPort);
		       {ok, Rport} ->
                           list_to_integer(Rport);
                       _ ->
                           siprequest:default_port(Proto, ViaPort)
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
    debugfriendly2(T, [Str | Res]);
debugfriendly2([H|T], Res) ->
    Str = io_lib:format("INVALID sipdst: ~p", [H]),
    debugfriendly2(T, [Str | Res]).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
%%           URL           = sipurl record(), destination
%%           ApproxMsgSize = integer()
%%           ReqURI        = sipurl record(), original Request-URI
%% Descrip.: Called from url_to_dstlist/1 when the Host part of the
%%              URI was not an IP address
%% Returns : DstList         |
%%           {error, Reason}
%%           DstList = list() of sipdst record()
%%--------------------------------------------------------------------

%%
%% URL port specified
%%
url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
  when is_record(URL, sipurl), is_integer(ApproxMsgSize), URL#sipurl.port /= none, is_record(ReqURI, sipurl) ->
    case (URL#sipurl.proto == tls) or (URL#sipurl.proto == tls6) of
	true ->
	    TLS = host_port_to_dstlist(tcp, URL#sipurl.host, sipurl:get_port(URL), ReqURI),
	    logger:log(debug, "Resolver: Port was explicitly supplied and URL protocol is TLS - only try TCP"),
	    TLS;
	false ->
	    TCP = host_port_to_dstlist(tcp, URL#sipurl.host, sipurl:get_port(URL), ReqURI),
	    UDP = host_port_to_dstlist(udp, URL#sipurl.host, sipurl:get_port(URL), ReqURI),
	    case ApproxMsgSize > 1200 of
		true ->
		    logger:log(debug, "Resolver: Port was explicitly supplied, and size of message is > 1200."
			       " Try TCP and then UDP."),
		    combine_host_portres([TCP, UDP]);
		false ->
		    %% RFC3263 4.1 says we SHOULD use UDP as default in this case, since UDP was the only thing
		    %% mandated by RFC2543. Sucks.
		    %% XXX why do we use and mention TCP here ?
		    logger:log(debug, "Resolver: Port was explicitly supplied, and size of message is <= 1200."
			       " Try UDP and then TCP."),
		    lists:append(UDP, TCP)
	    end
    end;

%%
%% URL port NOT specified, do SRV lookup on host from URL
%%
url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI) when is_record(URL, sipurl), is_integer(ApproxMsgSize),
						       is_record(ReqURI, sipurl) ->
    case dnsutil:siplookup(URL#sipurl.host) of
	[{error, nxdomain} | _] ->
	    %% A SRV-lookup of the Host part of the URL returned NXDOMAIN, this is
	    %% not an error and we will now try to resolve the Host-part directly
	    %% (look for A or AAAA record)
	    host_port_to_dstlist(udp, URL#sipurl.host, sipurl:get_port(URL), ReqURI);
	[{error, What} | _] ->
	    %% If first element returned from siplookup is an error then they all are.
	    {error, What};
	none ->
	    TCP = host_port_to_dstlist(tcp, URL#sipurl.host, sipurl:get_port(URL), ReqURI),
	    UDP = host_port_to_dstlist(udp, URL#sipurl.host, sipurl:get_port(URL), ReqURI),
	    logger:log(debug, "Warning: ~p has no SRV records in DNS, defaulting to TCP and then UDP",
		       [URL#sipurl.host]),
	    case ApproxMsgSize > 1200 of
		true ->
		    logger:log(debug, "Warning: ~p has no SRV records in DNS, and the message size" ++
			       "is > 1200 bytes. Resolving hostname and trying TCP and then UDP.",
			       [URL#sipurl.host]),
		    combine_host_portres([TCP, UDP]);
		false ->
		    logger:log(debug, "Warning: ~p has no SRV records in DNS. Resolving hostname " ++
			       "and defaulting to UDP (only).", [URL#sipurl.host]),
		    UDP
	    end;
	DstList when is_list(DstList) ->
	    DstList2 = case sipserver:get_env(enable_experimental_tls, false) of
			   false ->
			       remove_tls_destinations(DstList);
			   true ->
			       DstList
		       end,
	    format_siplookup_result(sipurl:get_port(URL), ReqURI, DstList2)
    end.

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
    combine_host_portres2(In, [], []).

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
%% Function: remove_tls_destinations(In)
%%           In = list() of {Proto, Host, Port} tuples
%% Descrip.: Remove all tuples having Proto 'tls' | tls6 from In, and
%%           return a new list() of tuples.
%% Returns : DstList = list() of {Proto, Host, Port} tuples
%%--------------------------------------------------------------------
remove_tls_destinations(In) ->
    remove_tls_destinations2(In, []).

remove_tls_destinations2([], Res) ->
    lists:reverse(Res);
remove_tls_destinations2([{Proto, Host, Port} | T], Res) when Proto /= tls, Proto /= tls6 ->
    remove_tls_destinations2(T, [{Proto, Host, Port} | Res]);
remove_tls_destinations2([{Proto, Host, Port} | T], Res) ->
    %% Proto is tls or tls6
    logger:log(debug, "Resolver: Removing TLS destination ~p:~s:~p from result set since "
	       "experimental TLS is not enabled", [Proto, Host, Port]),
    remove_tls_destinations2(T, Res).

%%--------------------------------------------------------------------
%% Function: host_port_to_dstlist(Proto, InHost, InPort, URI)
%%           Proto  = atom(), tcp | udp | tls
%%           InHost = string()
%%           InPort = integer()
%%           URI    = sipurl record() to put in the created sipdst
%%                    records
%% Descrip.: Resolves a hostname and returns a list of sipdst
%%           records of the protocol requested.
%%           InPort should either be an integer, or the atom 'none'
%%           to use the default port for the protocol.
%% Returns : DstList         |
%%           {error, Reason}
%%           DstList = list() of sipdst record()
%%--------------------------------------------------------------------
host_port_to_dstlist(Proto, InHost, InPort, URI) when is_integer(InPort) ; InPort == none,
						      is_record(URI, sipurl) ->
    case dnsutil:get_ip_port(InHost, InPort) of
	{error, What} ->
	    {error, What};
	L when is_list(L) ->
	    %% L is a list of tuples like {Inet, Addr, Port}. Addr is the
	    %% IP address (be it v4 or v6) as a string. Inet is 'inet' or 'inet6'.
	    DstList = make_sipdst_from_hostport(Proto, URI, L),
	    DstList
    end.

%%--------------------------------------------------------------------
%% Function: make_sipdst_from_hostport(Proto, URI, L)
%%           Proto = atom(), tcp | udp | tls
%%           URI   = sipurl record(), URI to stick into the resulting
%%                   sipdst records
%%           L     = list() of {Proto, Addr, Port} tuples - typically
%%                   the result of a call to dnsutil:get_ip_port()
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
make_sipdst_from_hostport(Proto, URI, L) when Proto == tcp; Proto == udp; Proto == tls,
					      is_record(URI, sipurl), is_list(L) ->
    make_sipdst_from_hostport2(Proto, URI, L, []).

make_sipdst_from_hostport2(_Proto, _URI, [], Res) ->
    lists:reverse(Res);
make_sipdst_from_hostport2(Proto, URI, [{inet, Addr, Port} | T], Res) ->
    UsePort = siprequest:default_port(Proto, Port),
    Dst = #sipdst{proto=Proto, addr=Addr, port=UsePort, uri=URI},
    make_sipdst_from_hostport2(Proto, URI, T, [Dst | Res]);
make_sipdst_from_hostport2(Proto, URI, [{inet6, Addr, Port} | T], Res) ->
    NewProto = case Proto of
		   tcp -> tcp6;
		   udp -> udp6;
		   tls -> tls6
	       end,
    UsePort = siprequest:default_port(NewProto, Port),
    Dst = #sipdst{proto=NewProto, addr=Addr, port=UsePort, uri=URI},
    make_sipdst_from_hostport2(Proto, URI, T,[Dst | Res]).


%%--------------------------------------------------------------------
%% Function: format_siplookup_result(InPort, ReqURI, ApproxMsgSize,
%%                                   In)
%%           InPort        = integer() | none
%%           ReqURI        = sipurl record()
%%           DstList       = list() of {Proto, Host, Port} tuple()
%% Descrip.: Turns the result of a dnsutil:siplookup() into a list
%%           of sipdst records. The ordering is preserved.
%% Returns : DstList, list() of sipdst record()
%%--------------------------------------------------------------------
format_siplookup_result(InPort, ReqURI, In) when is_integer(InPort) ; InPort == none,
						 is_record(ReqURI, sipurl), is_list(In) ->
    format_siplookup_result2(InPort, ReqURI, In, []).

format_siplookup_result2(_InPort, _ReqURI, [], Res) ->
    Res;
format_siplookup_result2(InPort, ReqURI, [{Proto, Host, Port} | T], Res) when is_integer(Port) ->
    %% If InPort is none, then use the port from DNS. Otherwise, use InPort.
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
    DstList = case host_port_to_dstlist(Proto, Host, UsePort, ReqURI) of
		  {error, Reason} ->
		      logger:log(error, "Warning: Could not make DstList out of ~p:~p:~p : ~p",
				 [Proto, Host, UsePort, Reason]),
		      [];
		  L when is_list(L) ->
		      L
	      end,
    format_siplookup_result2(InPort, ReqURI, T, lists:append(Res, DstList));
format_siplookup_result2(InPort, ReqURI, [{error, _What} | T], Res) ->
    format_siplookup_result2(InPort, ReqURI, T, Res).


%%--------------------------------------------------------------------
%% Function: get_response_host_port(TopVia)
%%           TopVia = via record()
%% Descrip.: Argument is the top Via header in a response, this
%%           function extracts the destination and protocol we
%%           should use.
%% Returns : {Address, Proto} |
%%           error
%%           Address = string() that might be IPv4 address (from
%%                     received=), IPv6 address (from received=), or
%%                     whatever was in the host part of the Via.
%%           Proto   = atom(), tcp | udp | tcp6 | udp6 | tls | tls6
%%--------------------------------------------------------------------
get_response_host_proto(TopVia) when is_record(TopVia, via) ->
    {Protocol, Host, Parameters} = {TopVia#via.proto, TopVia#via.host, TopVia#via.param},
    ParamDict = sipheader:param_to_dict(Parameters),
    Proto = sipsocket:viaproto2proto(Protocol),
    case dict:find("received", ParamDict) of
	{ok, Received} ->
	    case address_to_address_and_proto(Received, Proto) of
		{error, E1} ->
		    logger:log(debug, "Warning: Malformed received= parameter (~p) : ~p", [E1, Received]),
		    %% received= parameter not usable, try host part of Via instead
		    %% XXX try to resolve host part of Via if necessary
		    case address_to_address_and_proto(Host, Proto) of
			{error, _E2} ->
			    logger:log(debug, "Warning: Invalid host part of Via (~p) : ~p", [E1, Host]),
			    logger:log(error, "Failed getting a response destination out of Via : ~p", [TopVia]),
			    error;
			R ->
			    R
		    end;
		R ->
		    R
	    end;
	_Res ->
	    %% There was no received= parameter. Do the same checks but on the Via
	    %% hostname (which is then almost certainly an IP-address).
	    case address_to_address_and_proto(Host, Proto) of
		{error, E3} ->
		    logger:log(debug, "Warning: No received= and invalid host part of Via (~p) : ~p", [E3, Host]),
		    logger:log(error, "Failed getting a response destination out of Via : ~p", [TopVia]),
		    error;
		R ->
		    R
	    end
    end.


%%--------------------------------------------------------------------
%% Function: address_to_address_and_proto(Addr, DefaultProto)
%%           Addr = term(), something (probably a string()) that is
%%                  parseable by inet_parse:ipv{4,6}_address()
%%           DefaultProto = atom(), tcp | udp | tls
%% Descrip.: When looking at Via headers, we often have a protocol
%%           from the SIP/2.0/FOO but we need to look at the
%%           address to determine if our sipdst proto should be
%%           foo or foo6. This function does that.
%% Returns : {Addr, Proto} | {error, Reason}
%%           Address = term(), parsed version of Addr
%%           Proto   = atom(), tcp | udp | tcp6 | udp6 | tls | tls6
%%           Reason  = string()
%%--------------------------------------------------------------------
address_to_address_and_proto(Addr, DefaultProto) when DefaultProto == tcp; DefaultProto == udp; DefaultProto == tls ->
    case inet_parse:ipv4_address(Addr) of
	{ok, _} ->
	    {Addr, DefaultProto};
	_ ->
	    case sipserver:get_env(enable_v6, false) of
		true ->
		    %% Check if it matches IPv6 address syntax
		    case inet_parse:ipv6_address(util:remove_v6_brackets(Addr)) of
			{ok, _} ->
			    Proto6 = case DefaultProto of
					 tcp -> tcp6;
					 udp -> udp6;
					 tls -> tls6
				     end,
			    {Addr, Proto6};
			_ ->
			    {error, "not an IPv4 or IPv6 address"}
		    end;
		false ->
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

    %% test remove_tls_destinations/2
    %%--------------------------------------------------------------------
    TCP4 = {tcp, "192.0.2.1", 5060},
    UDP4 = {udp, "192.0.2.2", 5060},
    TLS4 = {tls, "192.0.2.3", 5061},
    TCP6 = {tcp6, "[2001:6b0:5:987::1]", none},
    UDP6 = {udp6, "[2001:6b0:5:987::2]", none},
    TLS6 = {tls6, "[2001:6b0:5:987::3]", none},

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


    %% test make_sipdst_from_hostport/3
    %%--------------------------------------------------------------------
    URL = sipurl:parse("sip:ft@example.org:1234"),

    io:format("test: make_sipdst_from_hostport/3 - 1~n"),
    %% simple case, tcp and no supplied port in the tuple
    Tuple1 = {inet, "address", none},
    Dst1 = #sipdst{proto=tcp, addr="address", port=5060, uri=URL},
    [Dst1] = make_sipdst_from_hostport(tcp, URL, [Tuple1]),

    io:format("test: make_sipdst_from_hostport/3 - 2~n"),
    %% tcp 'upped' to tcp6 since tuple protocol is inet6. port from tuple used.
    Tuple2 = {inet6, "address", 5070},
    Dst2 = #sipdst{proto=tcp6, addr="address", port=5070, uri=URL},
    [Dst2] = make_sipdst_from_hostport(tcp, URL, [Tuple2]),

    io:format("test: make_sipdst_from_hostport/3 - 3~n"),
    %% mixed
    [Dst1, Dst2] = make_sipdst_from_hostport(tcp, URL, [Tuple1, Tuple2]),

    %% test get_response_host_proto/1
    %% NOTE: We can currently only test with IPv4 addresses since IPv6 is
    %% off by default, so when the tests are run enable_v6 might not be
    %% 'true'.
    %%--------------------------------------------------------------------

    io:format("test: get_response_host_proto/1 - 1~n"),
    %% straight forward, no received= parameter
    TopVia1 = #via{proto="SIP/2.0/TCP", host="192.0.2.1", param=[]},
    {"192.0.2.1", tcp} = get_response_host_proto(TopVia1),

    io:format("test: get_response_host_proto/1 - 2~n"),
    %% straight forward, address in received= parameter
    TopVia2 = #via{proto="SIP/2.0/TCP", host="phone.example.org", param=["received=192.0.2.1"]},
    {"192.0.2.1", tcp} = get_response_host_proto(TopVia2),

    io:format("test: get_response_host_proto/1 - 3~n"),
    %% error, hostname in Via host and no received= parameter
    TopVia3 = #via{proto="SIP/2.0/TCP", host="phone.example.org", param=[]},
    error = get_response_host_proto(TopVia3),

    io:format("test: get_response_host_proto/1 - 4~n"),
    %% invalid received= parameter, but luckily valid IP address in Via host
    TopVia4 = #via{proto="SIP/2.0/TLS", host="192.0.2.1", param=["received=X"]},
    {"192.0.2.1", tls} = get_response_host_proto(TopVia4),

    %% test format_siplookup_result/3
    %%--------------------------------------------------------------------

    io:format("test: format_siplookup_result/3 - 1~n"),
    %% InPort 'none', use the one from DNS
    Tuple3 = {tcp, "192.0.2.1", 1234},
    Dst3 = #sipdst{proto=tcp, addr="192.0.2.1", port=1234, uri=URL},
    [Dst3] = format_siplookup_result(none, URL, [Tuple3]),

    io:format("test: format_siplookup_result/3 - 2~n"),
    %% InPort 2345, overrides the one in DNS (1234 in Tuple3)
    Dst4 = #sipdst{proto=tcp, addr="192.0.2.1", port=2345, uri=URL},
    [Dst4] = format_siplookup_result(2345, URL, [Tuple3]),

    io:format("test: format_siplookup_result/3 - 3~n"),
    Tuple5 = {tcp, "192.0.2.2", 5065},
    Dst5 = #sipdst{proto=tcp, addr="192.0.2.2", port=5065, uri=URL},
    %% more than one tuple in
    [Dst3, Dst5] = format_siplookup_result(none, URL, [Tuple3, Tuple5]),

    %% test combine_host_portres/1
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
	combine_host_portres([#sipdst{proto=1}, #sipdst{proto=2}, #sipdst{proto=3}]),

    ok.
