-module(siprequest).
-export([send_redirect/3, send_auth_req/4, send_proxyauth_req/4,
	 send_proxy_request/4, send_answer/3, send_notavail/2,
	 send_notfound/2, send_proxy_response/5, send_result/5,
	 send_result/6, make_answerheader/1, add_record_route/2,
	 add_record_route/4, myhostname/0,
	 default_port/1, default_port/2,
	 get_loop_cookie/3, generate_branch/0, url_to_dstlist/3,
	 make_response/7, rewrite_route/2, make_base64_md5_token/1,
	 host_port_to_dstlist/4]).

-include("sipsocket.hrl").

send_response(Socket, Code, Text, Header, Body) ->
    case sipheader:topvia(Header) of
	none ->
	    logger:log(error, "Can't send response ~p ~s, no Via left.",
		       [Code, Text]),
	    {senderror, "malformed response"};
	error ->
	    logger:log(error, "Failed getting top Via out of malformed response ~p ~s",
		       [Code, Text]),
	    {senderror, "malformed response"};
	TopVia ->
	    send_response_to(Socket, Code, Text, TopVia, Header, Body)
    end.

send_response_to(DefaultSocket, Code, Text, TopVia, HeaderIn, Body) ->
    Line1 = "SIP/2.0 " ++ integer_to_list(Code) ++ " " ++ Text,
    Header = fix_content_length(HeaderIn, Body),
    Message = lists:flatten(Line1 ++ "\r\n" ++ sipheader:build_header(Header) ++ "\r\n" ++ Body),
    {SendToHost, SendProto} = get_response_destination(TopVia),
    {_, {_, DestPort}, Parameters} = TopVia,
    ParamDict = sipheader:param_to_dict(Parameters),
    Port = case dict:find("rport", ParamDict) of
	       {ok, []} ->
		   %% This must be an error response generated before the rport fix-up. Ignore rport.
		   list_to_integer(default_port(SendProto, DestPort));
	       {ok, Rport} ->
		   list_to_integer(Rport);
	       _ ->
		   list_to_integer(default_port(SendProto, DestPort))
	   end,
    case get_response_socket(DefaultSocket, SendProto, SendToHost, Port) of
	SendSocket when record(SendSocket, sipsocket) ->
	    CPid = SendSocket#sipsocket.pid,
	    logger:log(debug, "send response(top Via: ~s, send to=~p:~s:~p (using ~p)) :~n~s~n",
		       [sipheader:via_print([TopVia]), SendProto, SendToHost, Port, CPid, Message]),
	    case sipsocket:send(SendSocket, SendProto, SendToHost, Port, Message) of
		ok ->
		    ok;
		{error, E} ->
		    logger:log(error, "Failed sending response to ~s:~p using socket ~p, error ~p",
			       [SendToHost, Port, SendSocket, E]),
		    {senderror, E}
	    end;
	{error, E} ->
	    logger:log(error, "Failed to get socket to send response to ~s:~p, error ~p, response :~n~s~n",
		       [SendToHost, Port, E, Message]),
	    {senderror, "Could not get socket"}
    end.

%%--------------------------------------------------------------------
%% Function: get_response_destination/1
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
get_response_destination(TopVia) ->
    {Protocol, {Host, _}, Parameters} = TopVia,
    ParamDict = sipheader:param_to_dict(Parameters),
    Proto = sipsocket:viaproto2proto(Protocol),
    case dict:find("received", ParamDict) of
	{ok, Received} ->
	    case address_to_address_and_proto(Received, Proto) of
		{error, E1} ->
		    logger:log(debug, "Warning: Malformed received= parameter (~p) : ~p", [E1, Received]),
		    %% received= parameter not usable, try host part of Via instead
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

get_response_socket(DefaultSocket, SendProto, SendToHost, Port) when integer(Port) ->
    case sipsocket:is_good_socket(DefaultSocket) of
	true ->
	    case DefaultSocket#sipsocket.proto of
		SendProto ->
		    logger:log(debug, "Siprequest: Using default socket ~p", [DefaultSocket]),
		    DefaultSocket;
		_ ->
		    logger:log(error, "Siprequest: Default socket provided for sending response is protocol ~p, " ++
			       "response should be sent over ~p", [DefaultSocket#sipsocket.proto, SendProto]),
		    {error, "Supplied socket has wrong protocol"}
	    end;
	_ ->
	    logger:log(debug, "Siprequest: No good socket (~p) provided to send_response_to() - asking transport layer for a ~p socket",
		       [DefaultSocket, SendProto]),
	    SocketModule = sipsocket:proto2module(SendProto),
	    case sipsocket:get_socket(SocketModule, SendProto, SendToHost, Port) of
		{error, E1} ->
		    {error, E1};
		none ->
		    {error, "no socket provided and get_socket() returned 'none'"};
		S when record(S, sipsocket) ->
		    logger:log(debug, "Siprequest: Extra debug: Get socket ~p ~p ~p ~p returned socket ~p",
			       [SocketModule, SendProto, SendToHost, Port, S]),
		    S
	    end
    end.
    
%%--------------------------------------------------------------------
%% Function: address_to_address_and_proto/1
%% Description: When looking at Via headers, we often have a protocol
%%              from the SIP/2.0/UDP but we need to look at the
%%              address to determine if our sipdst proto should be
%%              udp or udp6. This function does that.
%% Returns: {Address, Proto} | {error, Reason}
%%   Proto is an atom, tcp|udp|tcp6|udp6|tls|tls6.
%%--------------------------------------------------------------------
address_to_address_and_proto(Addr, DefaultProto) ->
    case regexp:first_match(Addr, "^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+\$") of
	{match, _, _} ->
	    %% XXX assure DefaultProto was v4 type?
	    {Addr, DefaultProto};
	_ ->
	    case sipserver:get_env(enable_v6, false) of
		true ->
		    %% Check if it matches IPv6 address syntax
		    case regexp:first_match(Addr, "^\\[[0-9a-fA-F:]+\\]\$") of
			{match, _, _} ->
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

fix_content_length(Header, Body) ->
    keylist:set("Content-Length", [integer_to_list(length(Body))], Header).

default_port(P) ->
    logger:log(debug, "WARNING: Assuming protocol in default_port"),
    default_port(udp, P).

default_port(Proto, none) when Proto == udp; Proto == udp6; Proto == tcp; Proto == tcp6 ->
    "5060";
default_port(Proto, none) when Proto == tls; Proto == tls6->
    "5061";
default_port(_, Port) when integer(Port) ->
    integer_to_list(Port);
default_port(_, Port) ->
    Port.

%%--------------------------------------------------------------------
%% Function: url_to_dstlist/3
%% Description: Make a list of sipdst records from an URL. We need the
%%              approximate message size to determine if we can use
%%              UDP or have to do TCP only.
%% Returns: ListOfDsts | {error, Reason}
%%--------------------------------------------------------------------
url_to_dstlist(URL, ApproxMsgSize, ReqURI) ->
    {User, Pass, Host, Port, Parameters} = URL,
    %% Check if Host is either IPv4 or IPv6 address
    case regexp:first_match(Host, "^([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)|(\\[[0-9a-fA-F:]+\\])\$") of
	{match, _, _} ->
	    logger:log(debug, "dns resolver: ~p is an IP address, not performing SRV lookup", [Host]),
	    %% RFC3263 4.1 says we SHOULD use UDP for sip: and TCP for sips: when target is IP
	    %% and no transport is indicated in the parameters
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
			    logger:log(debug, "url_to_dstlist: transport protocol ~p not recognized, defaulting to UDP", [Unknown]),
			    udp
		    end,
	    case address_to_address_and_proto(Host, Proto) of
		{error, E} ->
		    logger:log(debug, "Warning: Could not make a destination of ~p:~p (~p)",
			       [Host, Port, E]),
		    {error, "Coult not make destination out of URL"};
		{UseAddr, UseProto} ->
		    UsePort = default_port(UseProto, Port),
		    [#sipdst{proto=UseProto, addr=UseAddr, port=UsePort, uri=ReqURI}]
	    end;
	_ ->
	    url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI)
    end.

%%--------------------------------------------------------------------
%% Function: url_to_dstlist_not_ip/3
%% Description: Called from url_to_dstlist/1 when the Host part of the
%% URI was not an IP address
%% Returns: ListOfDsts | {error, Reason}
%%--------------------------------------------------------------------
url_to_dstlist_not_ip(URL, ApproxMsgSize, ReqURI) ->
    {User, Pass, InHost, InPort, Parameters} = URL,
    case dnsutil:siplookup(InHost) of
	[{error, nxdomain} | _] ->
	    %% A SRV-lookup of the Host part of the URL returned NXDOMAIN, this is
	    %% not an error and we will now try to resolve the Host-part directly
	    %% (look for A or AAAA record)
	    host_port_to_dstlist(InHost, InPort, ApproxMsgSize, ReqURI);
	[{error, What} | _] ->
	    %% If first element returned from siplookup is an error then they all are.
	    {error, What};
	none ->
	    host_port_to_dstlist(InHost, InPort, ApproxMsgSize, ReqURI);
	DstList ->
	    format_siplookup_result(InPort, ReqURI, ApproxMsgSize, DstList)
    end.

%%--------------------------------------------------------------------
%% Function: host_port_to_dstlist/4
%% Description: Resolves a hostname and returns a list of sipdst
%%              records. Needs approximate message size to determine
%%              if UDP protocol is applicable or should be excluded.
%%              Inport should either be an integer, or the atom 'none'
%%              to use the default port for the different protocols
%%              the resolving might turn up.
%% Returns: ListOfDsts | {error, Reason}
%%--------------------------------------------------------------------
host_port_to_dstlist(InHost, PortStr, ApproxMsgSize, URI) when list(PortStr) ->
    host_port_to_dstlist(InHost, list_to_integer(PortStr), ApproxMsgSize, URI);
host_port_to_dstlist(InHost, InPort, ApproxMsgSize, URI) when integer(InPort) ; InPort == none ->
    %% RFC3263 4.1 says we SHOULD use UDP if port is explicitly provided
    %% except if other things (such as packet > 1300 bytes) suggests use
    %% of other transport
    case dnsutil:get_ip_port(InHost, InPort) of
	{error, What} ->
	    {error, What};
	L when list(L) ->
	    %% L is a list of tuples like {IP, Addr, Port}. IP is string,
	    %% Addr is another tuple (get_ip_port uses inet:getaddr()).
	    TCP = make_sipdst_from_hostport(tcp, URI, L),
	    UDP = make_sipdst_from_hostport(udp, URI, L),
	    case InPort of
		none ->
		    logger:log(debug, "Warning: ~p has no SRV records in DNS, defaulting to TCP and then UDP",
			       [InHost]),
		    lists:append(TCP, UDP);
		_ ->
		    if
			ApproxMsgSize > 1200 ->
			    logger:log(debug, "Warning: ~p has no SRV records in DNS, and an explicit port was given (~p) " ++
				       "but the message size is > 1200 bytes. Resolving hostname and trying TCP and then UDP.",
				       [InHost]),
			    lists:append(TCP, UDP);
			true ->
			    logger:log(debug, "Warning: ~p has no SRV records in DNS, but an explicit port was given (~p). " ++
				       "Resolving hostname and defaulting to UDP.", [InHost, InPort]),
			    UDP
		    end
	    end
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
make_sipdst_from_hostport2(Proto, URI, [{Addr, {A1, A2, A3, A4}, Port} | T], Res) ->
    UsePort = list_to_integer(default_port(Proto, Port)),
    Dst = #sipdst{proto=Proto, addr=Addr, port=UsePort, uri=URI},
    make_sipdst_from_hostport2(Proto, URI, T, lists:append(Res, [Dst]));
make_sipdst_from_hostport2(Proto, URI, [{Addr, {A1, A2, A3, A4, A5, A6, A7, A8}, Port} | T], Res) ->
    NewProto = case Proto of
		   tcp -> tcp6;
		   udp -> udp6;
		   tls -> tls6
	       end,
    UsePort = list_to_integer(default_port(NewProto, Port)),
    Dst = #sipdst{proto=NewProto, addr=Addr, port=UsePort, uri=URI},
    make_sipdst_from_hostport2(Proto, URI, T, lists:append(Res, [Dst])).


%%--------------------------------------------------------------------
%% Function: format_siplookup_result/4
%% Description: Turns the result of a dnsutil:siplookup() into a list
%% of sipdst records.
%% Returns: ListOfDsts | {error, Reason}
%%--------------------------------------------------------------------
format_siplookup_result(PortStr, ReqURI, ApproxMsgSize, DstList) when list(PortStr) ->
    format_siplookup_result(list_to_integer(PortStr), ReqURI, ApproxMsgSize, DstList);
format_siplookup_result(InPort, ReqURI, ApproxMsgSize, DstList) when integer(InPort) ->
    format_siplookup_result2(InPort, ReqURI, ApproxMsgSize, DstList, []);
format_siplookup_result(none, ReqURI, ApproxMsgSize, DstList) ->
    format_siplookup_result2(none, ReqURI, ApproxMsgSize, DstList, []).

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
    DstList = case host_port_to_dstlist(Host, UsePort, ApproxMsgSize, ReqURI) of
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

rewrite_route(Header, URI) ->
    Route = sipheader:contact(keylist:fetch("Route", Header)),
    case Route of
        [{_, NewDest} | NewRoute1] ->
	    {NewRoute, NewURI} = case is_loose_router({none, NewDest}) of
				     true ->
					 {NewRoute1, URI};
				     false ->
					 logger:log(debug, "Routing: Destination ~p is a strict (RFC2543) router, appending final destination URI ~p to Route header",
						    [sipurl:print(NewDest), sipurl:print(URI)]),
					 NewRoute2 = lists:append(NewRoute1, [{none, URI}]),
					 {NewRoute2, NewDest}
				 end,
	    logger:log(debug, "Routing: New destination is ~p, new Request-URI is ~p", [sipurl:print(NewDest), sipurl:print(NewURI)]),
	    case NewRoute of
		[] ->
		    {keylist:delete("Route", Header), NewDest, NewURI};
		_ ->
		    {keylist:set("Route", sipheader:contact_print(NewRoute),
				 Header), NewDest, NewURI}
	    end;
	[] ->
	    {Header, URI, URI}
    end.

is_loose_router(Route) ->
    case dict:find("lr", sipheader:contact_params(Route)) of
	{ok, E} ->
	    true;
	_ ->
	    false
    end.

send_proxy_request(SrvTHandler, Request, DstURI, Parameters) ->
    {Method, OrigURI, Header, Body} = Request,
    NewHeader1 = proxy_check_maxforwards(Header),
    check_valid_proxy_request(Method, NewHeader1),
    {NewHeader2, NewDest, NewURI} = rewrite_route(NewHeader1, DstURI),
    Line1 = Method ++ " " ++ sipurl:print(NewURI) ++ " SIP/2.0",
    NewHeader = fix_content_length(NewHeader2, Body),
    ApproxMsgSize = length(Line1 ++ "\r\n" ++ sipheader:build_header(NewHeader) ++ "\r\n" ++ Body),
    {ViaParameters, Dst} = separate_parameters(Parameters),
    case get_dst(NewDest, ApproxMsgSize, Dst) of
	{error, nxdomain} ->
	    logger:log(normal, "Could not resolve destination ~p (NXDOMAIN)", [NewDest]),
	    {sendresponse, 604, "Does Not Exist Anywhere"};
	{error, What} ->
	    logger:log(normal, "Could not resolve destination ~p (~p)", [NewDest, What]),
	    {sendresponse, 500, "Could not resolve destination"};
	DstList when list (DstList) ->
	    logger:log(debug, "Siprequest: Destination list for ~s is :~n~p",
		       [sipurl:print(NewDest), DstList]),
	    NewRequest = {Method, NewDest, NewHeader, Body},
	    TransactionId = sipheader:get_server_transaction_id(NewRequest),
	    send_to_available_dst(sipurl:print(NewDest), DstList, TransactionId, Line1,
	    			  NewRequest, ViaParameters, OrigURI, SrvTHandler)
    end.

separate_parameters(In) ->
    separate_parameters(In, [], []).

separate_parameters([], Via, Dst) ->
    {Via, Dst};
separate_parameters([{dst, D} | T], Via, Dst) when record(D, sipdst) ->
    separate_parameters(T, Via, lists:append(Dst, [D]));
separate_parameters([{dst, E} | T], Via, Dst) ->
    erlang:fault("illegal dst parameter, not sipdst", [E]);
separate_parameters([H | T], Via, Dst) ->
    separate_parameters(T, lists:append(Via, [H]), Dst).

get_dst(URI, ApproxMsgSize, []) ->
    url_to_dstlist(URI, ApproxMsgSize, undefined);
get_dst(_, _, DstList) ->
    DstList.

send_to_available_dst(DestStr, [], TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler) ->
    {Method, URI, Header, Body} = Request,
    Message = Line1 ++ "\r\n" ++ sipheader:build_header(Header) ++ "\r\n" ++ Body,
    logger:log(debug, "Failed sending request (my Via not added) :~n~s", [Message]),
    {senderror, "failed"};
send_to_available_dst(DestStr, [Dst | T], TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler) when record(Dst, sipdst) ->
    {Method, URI, Header, Body} = Request,
    IP = Dst#sipdst.addr,
    Port = Dst#sipdst.port,
    Proto = Dst#sipdst.proto,
    SocketModule = sipsocket:proto2module(Proto),
    case sipsocket:get_socket(SocketModule, Proto, IP, Port) of
	{error, What} ->
	    logger:log(debug, "Failed to get ~p socket (~s:~p) for ~s : ~p", [Proto, IP, Port, DestStr, What]),
	    %% try next
	    send_to_available_dst(DestStr, T, TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler);
	SipSocket when record(SipSocket, sipsocket) ->
	    NewHeader1 = proxy_add_via(Header, Method, OrigURI, Parameters, Proto, SrvTHandler),
	    Message = lists:flatten(Line1 ++ "\r\n" ++ sipheader:build_header(NewHeader1) ++ "\r\n" ++ Body),
	    case sipsocket:send(SipSocket, Proto, IP, Port, Message) of
		ok ->
		    logger:log(debug, "sent request(~p, sent to=~p:~s:~p) :~n~s~n",
			       [DestStr, Proto, IP, Port, Message]),
		    TopVia = sipheader:topvia(NewHeader1),
		    UsedBranch = lists:flatten(sipheader:get_via_branch_full(TopVia)),
		    {ok, SipSocket, UsedBranch};
		{error, E} ->
		    logger:log(debug, "Failed sending message to ~p:~s:~p, error ~p",
			       [Proto, IP, Port, E]),
		    send_to_available_dst(DestStr, T, TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler)
	    end
    end;
send_to_available_dst(DestStr, [Dst | T], TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler) ->
    logger:log(error, "Siprequest: send_to_available_dst called with illegal dst ~p", [Dst]),
    send_to_available_dst(DestStr, T, TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler).

proxy_check_maxforwards(Header) ->
    MaxForwards =
	case keylist:fetch("Max-Forwards", Header) of
	    [M] ->
		lists:min([sipserver:get_env(max_max_forwards, 255), list_to_integer(M) - 1]);
	    [] ->
		sipserver:get_env(default_max_forwards, 70)
	end,
    logger:log(debug, "Max-Forwards is ~p", [MaxForwards]),
    if
	MaxForwards < 1 ->
	    logger:log(normal, "Not proxying request with Max-Forwards < 1"),
	    throw({siperror, 483, "Too Many Hops"});
	true ->
	    keylist:set("Max-Forwards", [integer_to_list(MaxForwards)], Header)
    end.

proxy_add_via(Header, Method, OrigURI, Parameters, Proto, SrvTHandler) ->
    ViaHostname = myhostname(),
    LoopCookie = case sipserver:get_env(detect_loops, true) of
		     true ->
			 get_loop_cookie(Header, OrigURI, Proto);
		     false ->
			 none
		 end,
    ParamDict = sipheader:param_to_dict(Parameters),
    ViaParameters1 = case dict:find("branch", ParamDict) of
			 error ->
			     case stateless_generate_branch(OrigURI, Header) of
				 error ->
				     Parameters;
				 Branch ->
				     %% In order to find the correct "server transaction" (ie. TCP socket) to use
				     %% when sending future responses to this request back upstreams, we need to
				     %% associate the stateless branch we generated with the socket the request
				     %% we are now proxying arrived on. If it is a TCP socket.
				     %% We don't check for errors since sockets can vanish but we can route
				     %% responses using the information in Via too.
				     case Method of
					 "ACK" -> true;	%% ACK is part of INVITE transaction or does not get responded to
					 _ ->
					     transactionlayer:store_stateless_response_branch(SrvTHandler, Branch, Method)
				     end,
				     NewBranch = case LoopCookie of
						     none ->
							 logger:log(debug, "Siprequest: Added statelessly generated branch ~p to request",
								    [Branch]),
							 Branch;
						     _ ->
							 NewBranch1 = Branch ++ "-o" ++ LoopCookie,
							 logger:log(debug, "Siprequest: Added statelessly generated branch (plus loop cookie) ~p to request",
								    [NewBranch1]),
							 NewBranch1
						 end,
				     Param2 = dict:store("branch", NewBranch, ParamDict),
				     sipheader:dict_to_param(Param2)
			     end;
			 {ok, Branch} when LoopCookie /= none ->
			     %% Check if there already is a loop cookie in this branch. Necessary to not
			     %% get the wrong branch in constructed ACK of non-2xx response to INVITE.
			     FlatBranch = lists:flatten(Branch),
			     case string:rstr(FlatBranch, "-o") of
				 0 ->
				     logger:log(debug, "Siprequest: Added loop cookie ~p to branch of request",
						[LoopCookie]),
				     Param2 = dict:append("branch", "-o" ++ LoopCookie, ParamDict),
				     sipheader:dict_to_param(Param2);
				 Index when integer(Index) ->
				     %% There is already a loop cookie in this branch, don't change it. Necessary to not
				     %% get the wrong branch in constructed ACK of non-2xx response to INVITE.
				     logger:log(debug, "Siprequets: NOT adding generated loop cookie ~p to branch that already " ++
						"contains a loop cookie : ~p", [LoopCookie, FlatBranch]),
				     Parameters
			     end;
			 {ok, Branch} ->
			     %% Request has a branch, and loop detection is off (or get_loop_cookie() returned 'none').
			     Parameters
		     end,
    ViaParameters = case sipserver:get_env(request_rport, false) of
			true ->
			    lists:append(ViaParameters1, ["rport"]);
			_ ->
			    ViaParameters1
		    end,
    ViaPort = sipserver:get_listenport(Proto),
    MyVia = sipheader:via_print([{sipsocket:proto2viastr(Proto),
				  {ViaHostname, ViaPort}, ViaParameters}]),
    keylist:prepend({"Via", MyVia}, Header).

stateless_generate_branch(OrigURI, Header) ->
    %% generate a branch for this request in a way that makes sure that we generate
    %% the same branch for a retransmission of this very same request. RFC3261 #16.11
    TopVia = sipheader:topvia(Header),
    case TopVia of
	{_, {ViaHostname, ViaPort}, Parameters} ->
	    case sipheader:get_via_branch(TopVia) of
		"z9hG4bK" ++ RestOfBranch ->
		    In = lists:flatten(lists:concat([node(), "-rbranch-", RestOfBranch])),
		    "z9hG4bK-yxa-" ++ make_base64_md5_token(In);
		_ ->
						% No branch, or non-RFC3261 branch
		    OrigURIstr = sipurl:print(OrigURI),
		    FromTag = sipheader:get_tag(keylist:fetch("From", Header)),
		    ToTag = sipheader:get_tag(keylist:fetch("To", Header)),
		    CallId = keylist:fetch("Call-Id", Header),
		    {CSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
		    In = lists:flatten(lists:concat([node(), "-uri-", OrigURIstr, "-ftag-", FromTag, "-totag-", ToTag, 
						     "-callid-", CallId, "-cseqnum-", CSeqNum])),
		    "z9hG4bK-yxa-" ++ make_base64_md5_token(In)
	    end;
	_ ->
	    logger:log(error, "Can't generate stateless branch for this request, it has no top Via!"),
	    error
    end.

%% Make md5 of input, base64 of that and RFC3261 token of the result
make_base64_md5_token(In) ->
    Out = string:strip(httpd_util:encode_base64(binary_to_list(erlang:md5(In))), right, $=),
    make_3261_token(Out).

%% RFC 3261 chapter 25 BNF notation of token :
%%      token       =  1*(alphanum / "-" / "." / "!" / "%" / "*"
%%                           / "_" / "+" / "" / "'" / "~" )
make_3261_token([]) ->
    [];
make_3261_token([H | T]) when H >= $a, H =< $z ->
    [H|make_3261_token(T)];
make_3261_token([H | T]) when H >= $A, H =< $Z ->
    [H|make_3261_token(T)];
make_3261_token([H | T]) when H >= $0, H =< $9 ->
    [H|make_3261_token(T)];
make_3261_token([H | T]) when H == $-; H == $.; H == $!; H == $%;
H == $*; H == $_; H == $+; H == $`; H == $'; H == $~ ->
    [H|make_3261_token(T)];
make_3261_token([H | T]) ->
    [$_|make_3261_token(T)].

generate_branch() ->
    {Megasec, Sec, Microsec} = now(),
    %% We don't need port here since erlang guarantees that Microsecond is never the
    %% same on one node.
    In = lists:concat([node(), Megasec * 1000000 + Sec, 8, $., Microsec]),
    Out = make_base64_md5_token(In),
    "z9hG4bK-yxa-" ++ Out.

make_answerheader(Header) ->
    RecordRoute = keylist:fetch("Record-Route", Header),
    NewHeader1 = keylist:delete("Record-Route", Header),
    NewHeader2 = case RecordRoute of
		     [] ->
			 NewHeader1;	
		     _ ->
			 keylist:set("Route", RecordRoute, NewHeader1)
		 end.

get_loop_cookie(Header, OrigURI, Proto) ->
    %% Generate a loop detection cookie, RFC3261 16.6 #8
    OrigURIstr = sipurl:print(OrigURI),
    FromTag = sipheader:get_tag(keylist:fetch("From", Header)),
    ToTag = sipheader:get_tag(keylist:fetch("To", Header)),
    CallId = keylist:fetch("Call-Id", Header),
    {CSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    ProxyReq = keylist:fetch("Proxy-Require", Header),
    %% We must remove the response part from Proxy-Authorization because it includes the method
    %% and thus CANCEL does not match INVITE. Contradictingly but implicitly from RFC3261 16.6 #8.
    ProxyAuth = proxyauth_without_response(Header),
    Route = keylist:fetch("Route", Header),
    {TopViaHost, TopViaPort} = case sipheader:topvia(Header) of
				   {TopViaProto, {TopViaHost1, TopViaPort1}, _} ->
				       P = sipsocket:viaproto2proto(TopViaProto),
				       {TopViaHost1, default_port(P, TopViaPort1)};
				   _ ->
				       {myhostname(), sipserver:get_listenport(Proto)}
			       end,
    TopViaSentBy = sipurl:print_hostport(TopViaHost, TopViaPort),
    In = lists:flatten(lists:concat([OrigURIstr, "-ftag-", FromTag, "-totag-", ToTag, 
				     "-callid-", CallId, "-cseqnum-", CSeqNum,
				     "-preq-", ProxyReq, "-pauth-", ProxyAuth,
				     "-route-", Route, "-topvia-", TopViaSentBy])),
    Out = make_base64_md5_token(In),
    logger:log(debug, "Siprequest: Created loop cookie ~p from input :~n~p", [Out, In]),
    Out.    

proxyauth_without_response(Header) ->
    case keylist:fetch("Proxy-Authorization", Header) of
	[] -> none;
	ProxyAuth ->
	    AuthDict = sipheader:auth(ProxyAuth),
	    NewDict1 = dict:erase("response", AuthDict),
	    NewDict2 = dict:erase("nonce", NewDict1),
	    NewDict3 = dict:erase("cnonce", NewDict2),
	    NewDict4 = dict:erase("opaque", NewDict3),
	    sipheader:dict_to_param(NewDict4)
    end.

check_valid_proxy_request("ACK", _) ->
    true;
check_valid_proxy_request("CANCEL", _) ->
    true;    
check_valid_proxy_request(Method, Header) ->
    ProxyRequire = keylist:fetch("Proxy-Require", Header),
    case ProxyRequire of
	[] ->
	    true;
	_ ->
	    logger:log(normal, "Proxy Request check: The client requires unsupported extension(s) ~p", [ProxyRequire]),
	    throw({siperror, 420, "Bad Extension", [{"Unsupported", ProxyRequire}]})
    end.

myhostname() ->
    case sipserver:get_env(myhostnames, []) of
	[] ->
	    siphost:myip();
	Hostnames ->
	    lists:nth(1, Hostnames)
    end.

add_record_route(Proto, Hostname, Port, Header) ->
    Param1 = ["maddr=" ++ siphost:myip(), "lr=true"],
    Param2 = case Proto of
		 none -> Param1;
		 _ ->
		     case Proto of
			 _ when Proto == tcp; Proto == tcp6 ->
			     lists:append(Param1, ["transport=TCP"]);
			 _ ->
			     Param1
		     end
	     end,
    Route = "<" ++ sipurl:print({none, none, Hostname, Port, Param2}) ++ ">",
    [CRoute] = sipheader:contact([Route]),
    case sipheader:contact(keylist:fetch("Record-Route", Header)) of
	[CRoute | _] ->
	    Header;
	_ ->
	    keylist:prepend({"Record-Route", [Route]}, Header)
    end.
add_record_route(Header, Origin) when record(Origin, siporigin) ->
    Port = sipserver:get_listenport(Origin#siporigin.proto),
    add_record_route(Origin#siporigin.proto, myhostname(), Port, Header).

standardcopy(Header, ExtraHeaders) ->
    keylist:appendlist(keylist:copy(Header,
				    ["Via", "From", "To",
				     "Call-ID", "CSeq"]),
		       ExtraHeaders).

send_auth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"WWW-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    send_response(Socket, 401, "Authentication Required",
		  standardcopy(Header, ExtraHeaders),
		  "").

send_proxyauth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"Proxy-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    send_response(Socket, 407, "Proxy Authentication Required",
		  standardcopy(Header, ExtraHeaders),
		  "").

send_redirect(Location, Header, Socket) ->
    Contact = [{none, Location}],
    ExtraHeaders = [{"Contact",
		     sipheader:contact_print(Contact)}],
    send_response(Socket, 302, "Moved Temporarily",
		  standardcopy(Header, ExtraHeaders),
		  "").


send_notfound(Header, Socket) ->
    send_response(Socket, 404, "Not found",
		  standardcopy(Header, []),
		  "").

send_notavail(Header, Socket) ->
    %% XXX CHECK THIS! SUSPECT IT SHOULD BE ["180"]
    ExtraHeaders = [{"Retry-After", "180"}],
    send_response(Socket, 480, "Temporarily unavailable",
		  standardcopy(Header, ExtraHeaders),
		  "").

send_answer(Header, Socket, Body) ->
    %% XXX CHECK THIS! SUSPECT IT SHOULD BE ["application/sdp"] AND [integer_to_list(length(Body))]
    ExtraHeaders = [{"Content-Type", "application/sdp"},
		    {"Content-Length",
		     integer_to_list(length(Body))}],
    send_response(Socket, 200, "OK",
		  standardcopy(Header, ExtraHeaders),
		  Body).

send_result(Header, Socket, Body, Code, Description) ->
    send_response(Socket, Code, Description,
		  standardcopy(Header, []),
		  Body).

send_result(Header, Socket, Body, Code, Description, ExtraHeaders) ->
    send_response(Socket, Code, Description,
		  standardcopy(Header, ExtraHeaders),
		  Body).

send_proxy_response(Socket, Status, Reason, Header, Body) ->
    case sipheader:via(keylist:fetch("Via", Header)) of
	[Self] ->
	    logger:log(error, "Can't proxy response ~p ~s because it contains just one or less Via and that should be mine!",
		       [Status, Reason]),
	    {error, invalid_Via};
	[Self | Via] ->
	    %% Remove Via matching me (XXX should check that it does)
	    NewHeader = keylist:set("Via", sipheader:via_print(Via), Header),
	    %% Now look for the correct "server transaction" to use when sending this response upstreams
	    [NextVia | _] = Via,
	    send_response(Socket, Status, Reason, NewHeader, Body)
    end.


make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SipSocket, Request) when record(SipSocket, sipsocket) ->
    make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SipSocket#sipsocket.proto, Request);
make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, Proto, Request) ->
    {Method, URI, ReqHeader, _} = Request,
    AnswerHeader1 = keylist:appendlist(keylist:copy(ReqHeader, ["Via", "From", "To", "Call-ID", "CSeq",
								"Record-Route", "Timestamp", "Content-Type"]),
				       ExtraHeaders),
    %% PlaceHolderVia is an EXTRA Via with our hostname. We could do without this if
    %% we sent it with send_response() instead of send_proxy_response() but it is easier
    %% to just add an extra Via that will be stripped by send_proxy_response() and don't
    %% have to make a difference in how we send out responses.
    PlaceHolderVia = sipheader:via_print([{sipsocket:proto2viastr(Proto),
					   {siprequest:myhostname(), sipserver:get_listenport(Proto)},
					   ViaParameters}]),
    Via = sipheader:via(keylist:fetch("Via", ReqHeader)),
    AnswerHeader2 = keylist:prepend({"Via", PlaceHolderVia}, AnswerHeader1),
    AnswerHeader3 = siprequest:make_answerheader(AnswerHeader2),
    %% If there is a body, calculate Content-Length, otherwise remove the Content-Type we copied above
    AnswerHeader4 = case Body of
			"" -> keylist:delete("Content-Type", AnswerHeader3);
			_ -> keylist:set("Content-Length", [integer_to_list(length(Body))], AnswerHeader3)
		    end,
    %% If this is not a 100 Trying response, remove the Timestamp we copied above.
    %% The preservation of Timestamp headers into 100 Trying response is mandated by RFC 3261 8.2.6.1
    AnswerHeader5 = case Status of
			100 -> AnswerHeader4;
			_ -> keylist:delete("Timestamp", AnswerHeader4)
		    end,
    {Status, Reason, AnswerHeader5, Body}.
