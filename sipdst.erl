%%% File    : sipdst.erl
%%% Author  : Fredrik Thulin <ft@t.su.se>
%%% Description : Functions to resolve URL's or Via headers into
%%%               sipdst records.
%%% Created : 15 Apr 2004 by Fredrik Thulin <ft@it.su.se>

-module(sipdst).

-export([url_to_dstlist/3, get_response_destination/1, dst2str/1,
	 debugfriendly/1]).

-include("siprecords.hrl").
-include("sipsocket.hrl").


%%--------------------------------------------------------------------
%% Function: url_to_dstlist/3
%% Description: Make a list of sipdst records from an URL. We need the
%%              approximate message size to determine if we can use
%%              UDP or have to do TCP only.
%% Returns: ListOfDsts | {error, Reason}
%%--------------------------------------------------------------------
url_to_dstlist(URL, ApproxMsgSize, ReqURI) when record(URL, sipurl), integer(ApproxMsgSize) ->
    {Host, Port, Parameters} = {URL#sipurl.host, URL#sipurl.port, URL#sipurl.param},
    %% Check if Host is either IPv4 or IPv6 address
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
				    logger:log(debug, "url_to_dstlist: transport protocol ~p not recognized, defaulting to TCP for sips: URL", [Unknown]),
				    tcp;
				_ ->
				    logger:log(debug, "url_to_dstlist: transport protocol ~p not recognized, defaulting to UDP for non-sips: URL", [Unknown]),
				    udp
			    end
		    end,
	    case address_to_address_and_proto(Host, Proto) of
		{error, E} ->
		    logger:log(debug, "Warning: Could not make a destination of ~p:~p (~p)",
			       [Host, Port, E]),
		    {error, "Coult not make destination out of URL"};
		{UseAddr, UseProto} ->
		    UsePort = list_to_integer(siprequest:default_port(UseProto, Port)),
		    [#sipdst{proto=UseProto, addr=UseAddr, port=UsePort, uri=ReqURI}]
	    end;
	_ ->
	    url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
    end.

%%--------------------------------------------------------------------
%% Function: get_response_destination/1
%% Description: Turn the top Via header from a response into a sipdst
%%              record with the protocol, host and port the response
%%              should be sent to.
%% Returns: Dst |
%%          error
%%--------------------------------------------------------------------
get_response_destination(TopVia) when record(TopVia, via) ->
    case get_response_host_proto(TopVia) of
        {Host, Proto} ->
            {ViaPort, Parameters} = {TopVia#via.port, TopVia#via.param},
	    ParamDict = sipheader:param_to_dict(Parameters),
	    Port = case dict:find("rport", ParamDict) of
                       {ok, []} ->
                           %% This must be an error response generated before the rport fix-up. Ignore rport.
                           list_to_integer(siprequest:default_port(Proto, ViaPort));
		       {ok, Rport} ->
                           list_to_integer(Rport);
                       _ ->
                           list_to_integer(siprequest:default_port(Proto, ViaPort))
                   end,
	    #sipdst{proto=Proto, addr=Host, port=Port};
	_ ->
	    error
    end.

%%--------------------------------------------------------------------
%% Function: dst2str/1
%% Description: Turn a sipdst into something printable (for debugging)
%% Returns: DstString
%%--------------------------------------------------------------------

%%
%% URI present
%%
dst2str(Dst) when record(Dst, sipdst), Dst#sipdst.uri /= undefined ->
    lists:flatten(io_lib:format("~p:~s:~p (~s)", [Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port,
						  sipurl:print(Dst#sipdst.uri)]));

%%
%% No URI, for example Response sipdst record
dst2str(Dst) when record(Dst, sipdst) ->
    lists:flatten(io_lib:format("~p:~s:~p", [Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port])).

debugfriendly(Dst) when record(Dst, sipdst) ->
    debugfriendly2([Dst], []);
debugfriendly(L) ->
    debugfriendly2(L, []).

debugfriendly2([], Res) ->
    lists:reverse(Res);
debugfriendly2([H|T], Res) when record(H, sipdst) ->
    Str = dst2str(H),
    debugfriendly2(T, [Str | Res]);
debugfriendly2([H|T], Res) ->
    Str = io_lib:format("INVALID sipdst: ~p", [H]),
    debugfriendly2(T, [Str | Res]).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: url_to_dstlist_not_ip/3
%% Description: Called from url_to_dstlist/1 when the Host part of the
%%              URI was not an IP address
%% Returns: ListOfDsts | {error, Reason}
%%--------------------------------------------------------------------

%%
%% URL port specified
%%
url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI) when record(URL, sipurl), integer(ApproxMsgSize), URL#sipurl.port /= none ->
    case URL#sipurl.proto of
	_ when URL#sipurl.proto == tls ; URL#sipurl.proto == tls6 ->
	    TLS = host_port_to_dstlist(tcp, URL#sipurl.host, URL#sipurl.port, ReqURI),
	    logger:log(debug, "Resolver: Port was explicitly supplied and URL protocol is TLS - only try TCP"),
	    TLS;
	_ ->
	    TCP = host_port_to_dstlist(tcp, URL#sipurl.host, URL#sipurl.port, ReqURI),
	    UDP = host_port_to_dstlist(udp, URL#sipurl.host, URL#sipurl.port, ReqURI),
	    case ApproxMsgSize of
		_ when ApproxMsgSize > 1200 ->
		    logger:log(debug, "Resolver: Port was explicitly supplied, and size of message is > 1200. Try TCP and then UDP."),
		    lists:append(TCP, UDP);
		_ ->
		    %% RFC3263 4.1 says we SHOULD use UDP as default in this case, since UDP was the only thing
		    %% mandated by RFC2543. Sucks.
		    logger:log(debug, "Resolver: Port was explicitly supplied, and size of message is <= 1200. Try UDP and then TCP."),
		    lists:append(UDP, TCP)
	    end
    end;

%%
%% URL port NOT specified, do SRV lookup on host from URL
%%
url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI) when record(URL, sipurl), integer(ApproxMsgSize) ->
    case dnsutil:siplookup(URL#sipurl.host) of
	[{error, nxdomain} | _] ->
	    %% A SRV-lookup of the Host part of the URL returned NXDOMAIN, this is
	    %% not an error and we will now try to resolve the Host-part directly
	    %% (look for A or AAAA record)
	    host_port_to_dstlist(udp, URL#sipurl.host, URL#sipurl.port, ReqURI);
	[{error, What} | _] ->
	    %% If first element returned from siplookup is an error then they all are.
	    {error, What};
	none ->
	    TCP = host_port_to_dstlist(tcp, URL#sipurl.host, URL#sipurl.port, ReqURI),
	    UDP = host_port_to_dstlist(udp, URL#sipurl.host, URL#sipurl.port, ReqURI),
	    logger:log(debug, "Warning: ~p has no SRV records in DNS, defaulting to TCP and then UDP",
		       [URL#sipurl.host]),
	    if
		ApproxMsgSize > 1200 ->
		    logger:log(debug, "Warning: ~p has no SRV records in DNS, and the message size" ++
			       "is > 1200 bytes. Resolving hostname and trying TCP and then UDP.",
			       [URL#sipurl.host]),
		    lists:append(TCP, UDP);
		true ->
		    logger:log(debug, "Warning: ~p has no SRV records in DNS. Resolving hostname " ++
			       "and defaulting to UDP (only).", [URL#sipurl.host]),
		    UDP
	    end;
	DstList ->
	    format_siplookup_result(URL#sipurl.port, ReqURI, ApproxMsgSize, DstList)
    end.

%%--------------------------------------------------------------------
%% Function: host_port_to_dstlist/4
%% Description: Resolves a hostname and returns a list of sipdst
%%              records of the protocol requested.
%%              Inport should either be an integer, or the atom 'none'
%%              to use the default port for the protocol.
%% Returns: ListOfDsts | {error, Reason}
%%--------------------------------------------------------------------
host_port_to_dstlist(Proto, InHost, PortStr, URI) when list(PortStr) ->
    host_port_to_dstlist(Proto, InHost, list_to_integer(PortStr), URI);
host_port_to_dstlist(Proto, InHost, InPort, URI) when integer(InPort) ; InPort == none ->
    case dnsutil:get_ip_port(InHost, InPort) of
	{error, What} ->
	    {error, What};
	L when list(L) ->
	    %% L is a list of tuples like {v4, Addr, Port}. Addr is the
	    %% IP address (be it v4 or v6) as a string.
	    DstList = make_sipdst_from_hostport(Proto, URI, L)
    end.

%%--------------------------------------------------------------------
%% Function: make_sipdst_from_hostport/3
%% Description: Turns the result of a dnsutil:get_ip_port() into a
%% list of sipdst records. get_ip_port() return a list of tuples like
%%    {IP, Addr, Port}
%% where IP is a string ("10.0.0.1", "[2001:6b0:5:987::1]") and Addr
%% is the same IP but in tuple representation. This allows us to easily
%% determine if it is a IPv4 or IPv6 address, and to convert Proto
%% as necessary.
%% Returns: ListOfDsts | {error, Reason}
%%--------------------------------------------------------------------
make_sipdst_from_hostport(Proto, URI, L) ->
    make_sipdst_from_hostport2(Proto, URI, L, []).

make_sipdst_from_hostport2(Proto, URI, [], Res) ->
    Res;
make_sipdst_from_hostport2(Proto, URI, [{inet, Addr, Port} | T], Res) ->
    UsePort = list_to_integer(siprequest:default_port(Proto, Port)),
    Dst = #sipdst{proto=Proto, addr=Addr, port=UsePort, uri=URI},
    make_sipdst_from_hostport2(Proto, URI, T, lists:append(Res, [Dst]));
make_sipdst_from_hostport2(Proto, URI, [{inet6, Addr, Port} | T], Res) ->
    NewProto = case Proto of
		   tcp -> tcp6;
		   udp -> udp6;
		   tls -> tls6
	       end,
    UsePort = list_to_integer(siprequest:default_port(NewProto, Port)),
    Dst = #sipdst{proto=NewProto, addr=Addr, port=UsePort, uri=URI},
    make_sipdst_from_hostport2(Proto, URI, T, lists:append(Res, [Dst])).


%%--------------------------------------------------------------------
%% Function: format_siplookup_result/4
%% Description: Turns the result of a dnsutil:siplookup() into a list
%% of sipdst records.
%% Returns: ListOfDsts      |
%%          {error, Reason}
%%--------------------------------------------------------------------
format_siplookup_result(PortStr, ReqURI, ApproxMsgSize, DstList) when list(PortStr) ->
    format_siplookup_result(list_to_integer(PortStr), ReqURI, ApproxMsgSize, DstList);
format_siplookup_result(InPort, ReqURI, ApproxMsgSize, DstList) when integer(InPort) ; InPort == none ->
    format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, DstList, []).

format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, [], Res) ->
    Res;
format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, [{Proto, Host, Port} | T], Res) when integer(Port) ->
    %% If InPort is none, then use the port from DNS. Otherwise, use InPort.
    %% This is to handle if we for example receive a request with a Request-URI of
    %% sip.example.net:5070, and sip.example.net has SRV-records saying port 5060. In
    %% that case, we should still send our request to port 5070.
    UsePort = case InPort of
		  _ when integer(InPort) -> InPort;
		  none -> Port
	      end,
    if
	UsePort /= Port ->
	    logger:log(debug, "Warning: ~p is specified to use port ~p in DNS, but I'm going to use the supplied port ~p instead",
		       [Host, Port, UsePort]);
	true -> true
    end,
    DstList = case host_port_to_dstlist(Proto, Host, UsePort, ReqURI) of
		  {error, _} ->
		      [];
		  L when list(L) ->
		      L
	      end,
    format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, T, lists:append(Res, DstList));
format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, [{error, What} | T], Res) ->
    format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, T, Res);
format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, [H | T], Res) ->
    logger:log(debug, "Warning: Skipping unrecognized element when formatting siplookup results : ~p", [H]),
    format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, T, Res).


%%--------------------------------------------------------------------
%% Function: get_response_host_port/1
%% Description: Argument is the top Via header in a response, this
%%              function extracts the destination and protocol we
%%              should use.
%% Returns: {Address, Proto} | error
%%
%%   Address is a string that might be IPv4 address (from received=),
%%   IPv6 address (from received=), or whatever was in the host part
%%   of the Via.
%%
%%   Proto is an atom, tcp|udp|tcp6|udp6|tls|tls6.
%%--------------------------------------------------------------------
get_response_host_proto(TopVia) when record(TopVia, via) ->
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
			{error, E2} ->
			    logger:log(debug, "Warning: Invalid host part of Via (~p) : ~p", [E1, Host]),
			    logger:log(error, "Failed getting a response destination out of Via : ~p", [TopVia]),
			    error;
			R ->
			    R
		    end;
		R ->
		    R
	    end;
	Res ->
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
%% Function: address_to_address_and_proto/1
%% Description: When looking at Via headers, we often have a protocol
%%              from the SIP/2.0/FOO but we need to look at the
%%              address to determine if our sipdst proto should be
%%              foo or foo6. This function does that.
%% Returns: {Address, Proto} | {error, Reason}
%%   Proto is an atom, tcp|udp|tcp6|udp6|tls|tls6.
%%--------------------------------------------------------------------
address_to_address_and_proto(Addr, DefaultProto) ->
    case inet_parse:ipv4_address(Addr) of
	{ok, _} ->
	    %% XXX assure DefaultProto was v4 type?
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

