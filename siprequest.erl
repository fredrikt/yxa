-module(siprequest).
-export([send_redirect/3, send_auth_req/4, send_proxyauth_req/4,
	 send_proxy_request/4, send_answer/3, send_notavail/2,
	 send_notfound/2, send_proxy_response/5, send_result/5,
	 send_result/6, make_answerheader/1, add_record_route/1,
	 add_record_route/3, myhostname/0, default_port/1,
	 get_loop_cookie/2, generate_branch/0, url_to_dstlist/2,
	 make_response/7, rewrite_route/2, make_base64_md5_token/1]).

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

send_response_to(DefaultSocket, Code, Text, Dest, HeaderIn, Body) ->
    Line1 = "SIP/2.0 " ++ integer_to_list(Code) ++ " " ++ Text,
    Header = fix_content_length(HeaderIn, Body),
    Message = Line1 ++ "\r\n" ++ sipheader:build_header(Header) ++ "\r\n" ++ Body,
    {Protocol, {Host, DestPort}, Parameters} = Dest,
    ParamDict = sipheader:param_to_dict(Parameters),
    SendToHost = case dict:find("received", ParamDict) of
	{ok, Received} ->
	    case regexp:first_match(Received, "^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$") of
		{match, _, _} ->
		    Received;
		_ ->
		    logger:log(debug, "Malformed received= parameter (not an IP address): ~p", [Received]),
		    Host
	    end;
	Res ->
	    Host
    end,
    {Port, PortInt} = case dict:find("rport", ParamDict) of
	{ok, []} ->
	    % This must be an error response generated before the rport fix-up. Ignore rport.
	    {DestPort, list_to_integer(default_port(DestPort))};
	{ok, Rport} ->
	    {Rport, list_to_integer(Rport)};
	_ ->
	    {DestPort, list_to_integer(default_port(DestPort))}
    end,
    SendSocket = case sipsocket:is_good_socket(DefaultSocket) of
	true ->
	    DefaultSocket;
	_ ->
	    logger:log(debug, "Siprequest: No good socket (~p) provided to send_response_to() - asking transport layer",
			[DefaultSocket]),
	    case sipsocket:get_socket(sipsocket:via2sipsocketprotocol(Protocol), SendToHost,
					   PortInt) of
		{error, E1} ->
		    {error, E1};
		none ->
		    {error, "no socket provided and get_socket() returned 'none'"};
		S when record(S, sipsocket) ->
		    S
	    end
    end,
    case SendSocket of
	SipSocket when record(SipSocket, sipsocket) ->
	    CPid = SipSocket#sipsocket.pid,
	    logger:log(debug, "send response(top Via: ~s, send to=~s:~s:~p (using ~p)) :~n~s~n",
    			[sipheader:via_print([Dest]), sipsocket:sipproto2str(SipSocket), SendToHost, PortInt, CPid, Message]),
	    case sipsocket:send(SendSocket, SendToHost, PortInt, Message) of
		ok ->
		    ok;
		{error, E} ->
		    logger:log(error, "Failed sending response to ~s:~p using socket ~p, error ~p",
				[SendToHost, PortInt, SendSocket, E]),
		    {senderror, E}
	    end;
	{error, E} ->
	    logger:log(error, "Failed to get socket to send response to ~s:~p, error ~p, response :~n~s~n",
			[SendToHost, PortInt, E, Message]),
	    {senderror, "Could not get socket"}
    end.

fix_content_length(Header, Body) ->
    keylist:set("Content-Length", [integer_to_list(length(Body))], Header).

default_port(none) ->
    "5060";
default_port(Port) when integer(Port) ->
    integer_to_list(Port);
default_port(Port) ->
    Port.

url_to_dstlist(URL, ApproxMsgSize) ->
    {User, Pass, Host, Port, Parameters} = URL,
    case regexp:first_match(Host, "^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$") of
	{match, _, _} ->
	    logger:log(debug, "dns resolver: ~p is an IP address, not performing SRV lookup", [Host]),
	    % RFC3263 4.1 says we SHOULD use UDP for sip: and TCP for sips: when target is IP
	    % and no transport is indicated in the parameters
	    ParamDict = sipheader:param_to_dict(Parameters),
	    Transport = case dict:find("transport", ParamDict) of
		error ->
		    [{sipsocket_udp, Host, default_port(Port)}];
		{ok, "tcp"} ->
	    	    [{sipsocket_tcp, Host, default_port(Port)}];
	    	{ok, Proto} ->
		    logger:log(debug, "url_to_dstlist: transport protocol ~p not recognized, defaulting to UDP", [Proto]),
		    [{sipsocket_udp, Host, default_port(Port)}]
	    end;
	_ ->
	    url_to_dstlist_not_ip(URL, ApproxMsgSize)
    end.

url_to_dstlist_not_ip(URL, ApproxMsgSize) ->
    URL = {User, Pass, InHost, InPort, Parameters} = URL,
    case dnsutil:siplookup(InHost) of
	[{error, nxdomain} | _] ->
	    host_port_to_dstlist(InHost, InPort, ApproxMsgSize);
	[{error, What} | _] ->
	    % If first element returned from siplookup is an error then they all are.
	    {error, What};
	none ->
	    host_port_to_dstlist(InHost, InPort, ApproxMsgSize);
	DstList ->
	    format_dst_list(InHost, InPort, DstList)
    end.

host_port_to_dstlist(InHost, InPort, ApproxMsgSize) ->
    % RFC3263 4.1 says we SHOULD use UDP if port is explicitly provided
    % except if other things (such as packet > 1300 bytes) suggests use
    % of other transport
    case dnsutil:get_ip_port(InHost, default_port(InPort)) of
	{error, What} ->
	    {error, What};
	{IP, DNSport} ->
	    case InPort of
		none ->
		    logger:log(debug, "Warning: ~p has no SRV records in DNS, defaulting to TCP and then UDP port ~p",
				[IP, DNSport]),
		    [{sipsocket_tcp, IP, DNSport}, {sipsocket_udp, IP, DNSport}];
		_ ->
		    if
			ApproxMsgSize > 1200 ->
			    logger:log(debug, "Warning: ~p has no SRV records in DNS, and an explicit port was given but the message size is > 1200 bytes. Try TCP and then UDP port ~p",
					[IP, DNSport]),
			    [{sipsocket_tcp, IP, DNSport}, {sipsocket_udp, IP, DNSport}];
			true ->
			    logger:log(debug, "Warning: ~p has no SRV records in DNS, but an explicit port was given. Defaulting to UDP port ~p",
					[IP, DNSport]),
			    [{sipsocket_udp, IP, DNSport}]
		    end
	    end
    end.

format_dst_list(InHost, InPort, DstList) ->
    format_dst_list(InHost, InPort, DstList, []).

format_dst_list(InHost, InPort, [], Res) ->
    Res;
format_dst_list(InHost, InPort, [{SipProto, Host, Port} | T], Res) ->
    UsePort = case InPort of
	none ->
	    integer_to_list(Port);
	PortInt when integer(InPort) ->
	    integer_to_list(InPort);
	PortStr when list(InPort) ->
	    InPort
    end,
    DNSPortList = integer_to_list(Port),
    if
	UsePort /= DNSPortList ->
	    logger:log(debug, "Warning: ~p is specified to use port ~s in DNS, but I'm going to use the supplied port ~s instead",
	    		[InHost, DNSPortList, UsePort]);		
	true -> true
    end,
    {OutHost, OutPort} = dnsutil:get_ip_port(Host, UsePort),
    format_dst_list(InHost, InPort, T, lists:append(Res, [{SipProto, OutHost, OutPort}]));
format_dst_list(InHost, InPort, [{error, What} | T], Res) ->
    format_dst_list(InHost, InPort, T, Res);
format_dst_list(InHost, InPort, [H | T], Res) ->
    logger:log(debug, "Warning: Unrecognized element returned from dnsutil:siplookup() on host ~p : ~p",
		[InHost, H]),
    format_dst_list(InHost, InPort, T, Res).

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
separate_parameters([{dst, D} | T], Via, Dst) ->
    separate_parameters(T, Via, lists:append(Dst, [D]));
separate_parameters([H | T], Via, Dst) ->
    separate_parameters(T, lists:append(Via, [H]), Dst).
    
get_dst(URI, ApproxMsgSize, []) ->
   url_to_dstlist(URI, ApproxMsgSize);
get_dst(_, _, DstList) ->
    DstList.

send_to_available_dst(DestStr, [], TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler) ->
    {Method, URI, Header, Body} = Request,
    Message = Line1 ++ "\r\n" ++ sipheader:build_header(Header) ++ "\r\n" ++ Body,
    logger:log(debug, "Failed sending request (my Via not added) :~n~s", [Message]),
    {senderror, "failed"};
send_to_available_dst(DestStr, [Dst | T], TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler) ->
    {Method, URI, Header, Body} = Request,
    {SipProto, IP, Port} = Dst,
    PortInt = list_to_integer(Port),
    case sipsocket:get_socket(SipProto, IP, PortInt) of
	{error, What} ->
	    logger:log(debug, "Failed to get ~p socket (~s:~p) for ~s : ~p", [SipProto, IP, PortInt, DestStr, What]),
	    % try next
	    send_to_available_dst(DestStr, T, TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler);
	SipSocket when record(SipSocket, sipsocket) ->
	    NewHeader1 = proxy_add_via(Header, Method, OrigURI, Parameters, SipProto, SrvTHandler),
	    Message = Line1 ++ "\r\n" ++ sipheader:build_header(NewHeader1) ++ "\r\n" ++ Body,
	    case sipsocket:send(SipSocket, IP, PortInt, Message) of
		ok ->
		    logger:log(debug, "sent request(~p, sent to=~s:~s:~p) :~n~s~n",
				[DestStr, sipsocket:sipproto2str(SipSocket), IP, PortInt, Message]),
		    {ok, SipSocket};
		{error, E} ->
		    logger:log(debug, "Failed sending message to ~s:~s:~p, error ~p",
				[sipsocket:sipproto2str(SipSocket), IP, PortInt, E]),
		    send_to_available_dst(DestStr, T, TransactionId, Line1, Request, Parameters, OrigURI, SrvTHandler)
	    end
    end.

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

proxy_add_via(Header, Method, OrigURI, Parameters, SocketProto, SrvTHandler) ->
    ViaHostname = myhostname(),
    LoopCookie = case sipserver:get_env(detect_loops, true) of
	true ->
	    get_loop_cookie(Header, OrigURI);
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
		    % In order to find the correct "server transaction" (ie. TCP socket) to use
		    % when sending future responses to this request back upstreams, we need to
		    % associate the stateless branch we generated with the socket the request
		    % we are now proxying arrived on. If it is a TCP socket.
		    % We don't check for errors since sockets can vanish but we can route
		    % responses using the information in Via too.
		    case Method of
			"ACK" -> true;	% ACK is part of INVITE transaction or does not get responded to
			_ ->
			    transactionlayer:store_stateless_response_branch(SrvTHandler, Branch, Method)
		    end,
		    % Check if there already is a loop cookie in this branch. Necessary to not
		    % get the wrong branch in constructed ACK of non-2xx response to INVITE.
		    NewBranch = case string:rstr(Branch, "-o") of
			0 ->
			    case LoopCookie of
				none -> Branch;
				_ ->
				    NewBranch1 = Branch ++ "-o" ++ LoopCookie,
				    logger:log(debug, "Siprequest: Added statelessly generated branch ~p to request",
						[NewBranch1]),
				    NewBranch1
			    end;
			Index when integer(Index) ->
			    logger:log(debug, "Siprequets: NOT adding generated loop cookie ~p to branch that already " ++
					"contains a loop cookie : ~p", [LoopCookie, Branch]),
			    Branch
		    end,
		    Param2 = dict:store("branch", NewBranch, ParamDict),
	    	    sipheader:dict_to_param(Param2)
	    end;
	{ok, Branch} when LoopCookie /= none ->
	    Param2 = case string:rstr(Branch, "-o") of
		0 ->
		    logger:log(debug, "Siprequest: Added loop cookie ~p to branch of request",
					[LoopCookie]),
		    dict:append("branch", "-o" ++ LoopCookie, ParamDict);
		Index when integer(Index) ->
		    % There is already a loop cookie in this branch, don't change it. Necessary to not
		    % get the wrong branch in constructed ACK of non-2xx response to INVITE.
		    logger:log(debug, "Siprequets: NOT adding generated loop cookie ~p to branch that already " ++
				"contains a loop cookie : ~p", [LoopCookie, Branch]),
		    ParamDict
	    end,		   
	    sipheader:dict_to_param(Param2);
	{ok, Branch} ->
	    % Request has a branch, and loop detection is off (or get_loop_cookie() returned 'none').
	    Parameters
    end,
    ViaPort = default_port(sipserver:get_env(listenport, none)),
    ViaParameters = case sipserver:get_env(request_rport, false) of
	true ->
	    lists:append(ViaParameters1, ["rport"]);
	_ ->
	    ViaParameters1
    end,
    MyVia = sipheader:via_print([{sipsocket:sipproto2viastr(SocketProto),
				 {ViaHostname, ViaPort}, ViaParameters}]),
    keylist:prepend({"Via", MyVia}, Header).

stateless_generate_branch(OrigURI, Header) ->
    % generate a branch for this request in a way that makes sure that we generate
    % the same branch for a retransmission of this very same request. RFC3261 #16.11
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

% Make md5 of input, base64 of that and RFC3261 token of the result
make_base64_md5_token(In) ->
    Out = string:strip(httpd_util:encode_base64(binary_to_list(erlang:md5(In))), right, $=),
    make_3261_token(Out).

% RFC 3261 chapter 25 BNF notation of token :
%      token       =  1*(alphanum / "-" / "." / "!" / "%" / "*"
%                           / "_" / "+" / "" / "'" / "~" )
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
    % We don't need port here since erlang guarantees that Microsecond is never the
    % same on one node.
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

get_loop_cookie(Header, OrigURI) ->
    % Generate a loop detection cookie, RFC3261 16.6 #8
    OrigURIstr = sipurl:print(OrigURI),
    FromTag = sipheader:get_tag(keylist:fetch("From", Header)),
    ToTag = sipheader:get_tag(keylist:fetch("To", Header)),
    CallId = keylist:fetch("Call-Id", Header),
    {CSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    ProxyReq = keylist:fetch("Proxy-Require", Header),
    % We must remove the response part from Proxy-Authorization because it includes the method
    % and thus CANCEL does not match INVITE. Contradictingly but implicitly from RFC3261 16.6 #8.
    ProxyAuth = proxyauth_without_response(Header),
    Route = keylist:fetch("Route", Header),
    {TopViaHost, TopViaPort} = case sipheader:topvia(Header) of
	{_, {TopViaHost1, TopViaPort1}, _} -> {TopViaHost1, default_port(TopViaPort1)};
	_ -> {myhostname(), sipserver:get_env(listenport, none)}
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

add_record_route(Hostname, Port, Header) ->
    Route = "<" ++ sipurl:print({none, none, Hostname, default_port(Port),
				["maddr=" ++ siphost:myip(), "lr=true"]}) ++ ">",
    [CRoute] = sipheader:contact([Route]),
    case sipheader:contact(keylist:fetch("Record-Route", Header)) of
	[CRoute | _] ->
	    Header;
	_ ->
	    keylist:prepend({"Record-Route", [Route]}, Header)
    end.
add_record_route(Header) ->
    add_record_route(myhostname(), sipserver:get_env(listenport, none), Header).

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
    % XXX CHECK THIS! SUSPECT IT SHOULD BE ["180"]
    ExtraHeaders = [{"Retry-After", "180"}],
    send_response(Socket, 480, "Temporarily unavailable",
		  standardcopy(Header, ExtraHeaders),
		  "").

send_answer(Header, Socket, Body) ->
    % XXX CHECK THIS! SUSPECT IT SHOULD BE ["application/sdp"] AND [integer_to_list(length(Body))]
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
	    % Remove Via matching me (XXX should check that it does)
	    NewHeader = keylist:set("Via", sipheader:via_print(Via), Header),
	    % Now look for the correct "server transaction" to use when sending this response upstreams
	    [NextVia | _] = Via,
	    send_response(Socket, Status, Reason, NewHeader, Body)
    end.


make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, {SocketProto, _, _}, Request) ->
    make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SocketProto, Request);
make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SocketProto, Request) ->
    {Method, URI, ReqHeader, _} = Request,
    AnswerHeader1 = keylist:appendlist(keylist:copy(ReqHeader, ["Via", "From", "To", "Call-ID", "CSeq",
						"Record-Route", "Timestamp", "Content-Type"]),
					ExtraHeaders),
    % PlaceHolderVia is an EXTRA Via with our hostname. We could do without this if
    % we sent it with send_response() instead of send_proxy_response() but it is easier
    % to just add an extra Via that will be stripped by send_proxy_response() and don't
    % have to make a difference in how we send out responses.
    PlaceHolderVia = sipheader:via_print([{sipsocket:sipproto2viastr(SocketProto),
				     {siprequest:myhostname(),
				      siprequest:default_port(sipserver:get_env(listenport, none))},
				     ViaParameters}]),
    Via = sipheader:via(keylist:fetch("Via", ReqHeader)),
    AnswerHeader2 = keylist:prepend({"Via", PlaceHolderVia}, AnswerHeader1),
    AnswerHeader3 = siprequest:make_answerheader(AnswerHeader2),
    % If there is a body, calculate Content-Length, otherwise remove the Content-Type we copied above
    AnswerHeader4 = case Body of
	"" -> keylist:delete("Content-Type", AnswerHeader3);
	_ -> keylist:set("Content-Length", [integer_to_list(length(Body))], AnswerHeader3)
    end,
    % If this is not a 100 Trying response, remove the Timestamp we copied above.
    % The preservation of Timestamp headers into 100 Trying response is mandated by RFC 3261 8.2.6.1
    AnswerHeader5 = case Status of
	100 -> AnswerHeader4;
	_ -> keylist:delete("Timestamp", AnswerHeader4)
    end,
    {Status, Reason, AnswerHeader5, Body}.
