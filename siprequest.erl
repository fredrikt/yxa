-module(siprequest).
-export([send_redirect/3, send_auth_req/4, send_proxyauth_req/4,
	 send_proxy_request/4, send_answer/3, send_notavail/2,
	 send_notfound/2, send_proxy_response/2, send_result/5,
	 send_result/6, make_answerheader/1, add_record_route/2,
	 add_record_route/4, myhostname/0, create_via/2,
	 default_port/2,
	 get_loop_cookie/3, generate_branch/0,
	 make_response/7, process_route_header/2, check_proxy_request/1,
	 make_base64_md5_token/1]).

-include("sipsocket.hrl").
-include("siprecords.hrl").

send_response(Socket, Response) when record(Response, response) ->
    {Status, Reason, Header} = {Response#response.status, Response#response.reason,
				Response#response.header},
    case sipheader:topvia(Header) of
	none ->
	    logger:log(error, "Can't send response ~p ~s, no Via left.",
		       [Status, Reason]),
	    {senderror, "malformed response"};
	error ->
	    logger:log(error, "Failed getting top Via out of malformed response ~p ~s",
		       [Status, Reason]),
	    {senderror, "malformed response"};
	TopVia when record(TopVia, via) ->
	    send_response_to(Socket, Response, TopVia)
    end.

send_response_to(DefaultSocket, Response, TopVia) when record(Response, response), record(TopVia, via) ->
    {Status, Reason, HeaderIn, Body} = {Response#response.status, Response#response.reason,
					Response#response.header, Response#response.body},
    Line1 = "SIP/2.0 " ++ integer_to_list(Status) ++ " " ++ Reason,
    Header = fix_content_length(HeaderIn, Body),
    Message = lists:flatten(Line1 ++ "\r\n" ++ sipheader:build_header(Header) ++ "\r\n" ++ Body),
    case sipdst:get_response_destination(TopVia) of
	Dst when record(Dst, sipdst) ->
	    case get_response_socket(DefaultSocket, Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port) of
		SendSocket when record(SendSocket, sipsocket) ->
		    CPid = SendSocket#sipsocket.pid,
		    logger:log(debug, "send response(top Via: ~s, send to=~s (using ~p)) :~n~s~n",
			       [sipheader:via_print([TopVia]), sipdst:dst2str(Dst), CPid, Message]),
		    case sipsocket:send(SendSocket, Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port, Message) of
			ok ->
			    ok;
			{error, E} ->
			    logger:log(error, "Failed sending response to ~s using socket ~p, error ~p",
				       [sipdst:dst2str(Dst), SendSocket, E]),
			    {senderror, E}
		    end;
		{error, E} ->
		    logger:log(error, "Failed to get socket to send response to ~s, error ~p, response :~n~s~n",
			       [sipdst:dst2str(Dst), E, Message]),
		    {senderror, "Could not get socket"}
	    end;
	_ ->
	    {senderror, "Failed finding destination"}
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
    
fix_content_length(Header, Body) ->
    keylist:set("Content-Length", [integer_to_list(length(Body))], Header).

default_port(Proto, none) when Proto == udp; Proto == udp6; Proto == tcp; Proto == tcp6; Proto == "sip" ->
    "5060";
default_port(Proto, none) when Proto == tls; Proto == tls6 ; Proto == "sips" ->
    "5061";
default_port(_, Port) when integer(Port) ->
    integer_to_list(Port);
default_port(_, Port) ->
    Port.

%%--------------------------------------------------------------------
%% Function: process_route_header/2
%% Description: Looks at the Route header. If it exists, we return a
%%              new destination URL for this request, and if there was
%%              no loose router flag in the new destination we append
%%              the original Request-URI to the list of routes, to
%%              traverse strict (RFC2543) proxys.
%% Returns: {ok, NewHeader, NewDestURI, NewRequestURI}  |
%%          nomatch
%%--------------------------------------------------------------------
process_route_header(Header, URI) when record(URI, sipurl) ->
    Route = sipheader:contact(keylist:fetch("Route", Header)),
    case Route of
        [{_, FirstRoute} | NewRoute1] ->
	    {NewHeader, NewURI} = case is_loose_router({none, FirstRoute}) of
				      true ->
					  {set_route(NewRoute1, Header), URI};
				      false ->
					  logger:log(debug, "Routing: First Route ~p is a strict (RFC2543) router, appending original " ++
						     "destination URI ~p to Route header", [sipurl:print(FirstRoute), sipurl:print(URI)]),
					  NewRoute2 = lists:append(NewRoute1, [{none, URI}]),
					  {set_route(NewRoute2, Header), FirstRoute}
				  end,
	    logger:log(debug, "Routing: New destination is ~p, new Request-URI is ~p", [sipurl:print(FirstRoute), sipurl:print(NewURI)]),
	    {ok, NewHeader, FirstRoute, NewURI};
	[] ->
	    nomatch
    end.

set_route([], Header) ->
    keylist:delete("Route", Header);
set_route(Route, Header) ->
    keylist:set("Route", sipheader:contact_print(Route), Header).

is_loose_router(Route) ->
    case dict:find("lr", sipheader:contact_params(Route)) of
	{ok, E} ->
	    true;
	_ ->
	    false
    end.

%% Function: check_proxy_request/3
%% Description: Prepares a request for proxying. Checks Max-Forwards,
%%              etc. Not guaranteed to return, might
%%              throw a siperror if the request should not be proxied.
%% Returns: {ok, NewHeader, ApproxMsgSize}
%%--------------------------------------------------------------------
check_proxy_request(Request) when record(Request, request) ->
    NewHeader1 = proxy_check_maxforwards(Request#request.header),
    check_valid_proxy_request(Request#request.method, NewHeader1),
    NewHeader2 = fix_content_length(NewHeader1, Request#request.body),
    %% The size is _approximate_. The exact size cannot be calculated here
    %% since it is dependent on the right Request-URI and length of the final
    %% Via header we insert, not the 92 bytes + hostname _estimate_.
    Line1 = Request#request.method ++ " " ++ sipurl:print(Request#request.uri) ++ " SIP/2.0",
    ViaLen = 92 + length(siprequest:myhostname()),
    ApproxMsgSize = length(Line1 ++ "\r\n" ++ sipheader:build_header(NewHeader2) ++ "\r\n" ++ Request#request.body) + ViaLen,
    {ok, NewHeader2, ApproxMsgSize}.


send_proxy_request(SrvTHandler, Request, Dst, ViaParameters) when record(Dst, sipdst) ->
    send_proxy_request(SrvTHandler, Request, [Dst], ViaParameters);

send_proxy_request(SrvTHandler, Request, [Dst | DstT], ViaParameters) when record(Request, request), record(Dst, sipdst) ->
    {Method, OrigURI, Header, Body} = {Request#request.method, Request#request.uri,
				       Request#request.header, Request#request.body},
    {ok, NewHeader1, ApproxMsgSize} = check_proxy_request(Request),
    DstList = [Dst | DstT],
    logger:log(debug, "Siprequest (transport layer) : Destination list for request is (~p entrys) :~n~p",
	       [length(DstList), sipdst:debugfriendly(DstList)]),
    NewRequest = Request#request{header=NewHeader1},
    send_to_available_dst(DstList, NewRequest, ViaParameters, SrvTHandler);

send_proxy_request(SrvTHandler, Request, URI, ViaParameters) when record(URI, sipurl) ->
    {ok, _, ApproxMsgSize} = check_proxy_request(Request),
    case process_route_header(Request#request.header, URI) of
	nomatch ->
	    case sipdst:url_to_dstlist(URI, ApproxMsgSize, URI) of
		DstList when list(DstList) ->
		    send_proxy_request(SrvTHandler, Request, DstList, ViaParameters);
		Unknown ->
		    logger:log(error, "Siprequest (transport layer) : Failed resolving URI ~s : ~p", [sipurl:print(URI), Unknown]),
		    {error, "Failed resolving destination"}
	    end;
	{ok, NewHeader, DstURI, ReqURI} when record(DstURI, sipurl), record(ReqURI, sipurl) ->
	    logger:log(debug, "Siprequest (transport layer) : Routing request as per the Route header, Destination ~p, Request-URI ~p",
		       [sipurl:print(DstURI), sipurl:print(ReqURI)]),
	    case sipdst:url_to_dstlist(DstURI, ApproxMsgSize, ReqURI) of
		DstList when list(DstList) ->
		    send_proxy_request(SrvTHandler, Request, DstList, ViaParameters);
		Unknown ->
		    logger:log(error, "Siprequest (transport layer) : Failed resolving URI (from Route header) ~s : ~p", [sipurl:print(URI), Unknown]),
		    {error, "Failed resolving destination"}
	    end
    end.

send_to_available_dst([], Request, ViaParam, SrvTHandler) when record(Request, request) ->
    Line1 = Request#request.method ++ " " ++ sipurl:print(Request#request.uri) ++ " SIP/2.0",
    lists:flatten(Message = Line1 ++ "\r\n" ++ sipheader:build_header(Request#request.header) ++ "\r\n" ++ Request#request.body),
    logger:log(debug, "Siprequest (transport layer) : Failed sending request (my Via not added, original URI shown) :~n~s", [Message]),
    {senderror, "failed"};
send_to_available_dst([Dst | DstT], Request, ViaParam, SrvTHandler) when record(Dst, sipdst), record(Request, request) ->
    IP = Dst#sipdst.addr,
    Port = Dst#sipdst.port,
    Proto = Dst#sipdst.proto,
    SocketModule = sipsocket:proto2module(Proto),
    DestStr = sipdst:dst2str(Dst),
    case sipsocket:get_socket(SocketModule, Proto, IP, Port) of
	{error, What} ->
	    logger:log(debug, "Siprequest (transport layer) : Failed to get ~p socket for ~s : ~p", [Proto, DestStr, What]),
	    %% try next
	    send_to_available_dst(DstT, Request, ViaParam, SrvTHandler);
	SipSocket when record(SipSocket, sipsocket) ->
	    {Method, OrigURI, Header, Body} = {Request#request.method, Request#request.uri,
					   Request#request.header, Request#request.body},
	    NewHeader1 = proxy_add_via(Header, Method, OrigURI, ViaParam, Proto, SrvTHandler),
	    Line1 = Method ++ " " ++ sipurl:print(Dst#sipdst.uri) ++ " SIP/2.0",
	    Message = lists:flatten(Line1 ++ "\r\n" ++ sipheader:build_header(NewHeader1) ++ "\r\n" ++ Body),
	    case sipsocket:send(SipSocket, Proto, IP, Port, Message) of
		ok ->
		    logger:log(debug, "Siprequest (transport layer) : sent request(dst=~s) :~n~s~n",
			       [DestStr, Message]),
		    TopVia = sipheader:topvia(NewHeader1),
		    UsedBranch = lists:flatten(sipheader:get_via_branch_full(TopVia)),
		    {ok, SipSocket, UsedBranch};
		{error, E} ->
		    logger:log(debug, "Siprequest (transport layer) : Failed sending message to ~p:~s:~p, error ~p",
			       [Proto, IP, Port, E]),
		    send_to_available_dst(DstT, Request, ViaParam, SrvTHandler)
	    end
    end;
send_to_available_dst([Dst | DstT], Request, ViaParam, SrvTHandler) ->
    logger:log(error, "Siprequest (transport layer) : send_to_available_dst called with illegal dst ~p", [Dst]),
    send_to_available_dst(DstT, Request, ViaParam, SrvTHandler).


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

%%--------------------------------------------------------------------
%% Function: proxy_add_via(Header, Method, OrigURI, Parameters, Proto,
%%                         SrvTHandler)
%%           Header      = keylist record()
%%           Method      = string()
%%           OrigURI     = sipurl record()
%%           Parameters  = string()
%%           Proto       = ???
%%           SrvTHandler = ???
%% Descrip.: Generate a Via header for this proxy and add it to Header
%% Returns : NewHeader, keylist record()
%%--------------------------------------------------------------------
proxy_add_via(Header, Method, OrigURI, Parameters, Proto, SrvTHandler) ->
    LoopCookie = case sipserver:get_env(detect_loops, true) of
		     true ->
			 get_loop_cookie(Header, OrigURI, Proto);
		     false ->
			 none
		 end,
    ParamDict = sipheader:param_to_dict(Parameters),
    ViaParameters1 = case dict:find("branch", ParamDict) of
			 error ->
			     add_stateless_generated_branch(Header, Method, OrigURI, LoopCookie, Parameters, SrvTHandler);
			 {ok, Branch} ->
			     add_loopcookie_to_branch(LoopCookie, Branch, Parameters, ParamDict)
		     end,
    ViaParameters = case sipserver:get_env(request_rport, false) of
			true ->
			    lists:append(ViaParameters1, ["rport"]);
			_ ->
			    ViaParameters1
		    end,
    V = create_via(Proto, ViaParameters),
    MyVia = sipheader:via_print([V]),
    keylist:prepend({"Via", MyVia}, Header).

%%--------------------------------------------------------------------
%% Function: add_loopcookie_to_branch(LoopCookie, Branch, Parameters,
%%                                    ParamDict)
%%           LoopCookie  = string() | none
%%           Branch      = string()
%%           Parameters  = list() of string()
%%           ParamDict   = dict()
%% Descrip.: If LoopCookie is not 'none', add it to the Branch if
%%           Branch does not already contain a loop cookie. Return
%%           a new Parameters construct.
%% Returns : NewParameters, list() of string()
%%--------------------------------------------------------------------
add_loopcookie_to_branch(none, _Branch, Parameters, _ParamDict) ->
    %% Request has a branch, and loop detection is off (or get_loop_cookie() returned 'none').
    Parameters;
add_loopcookie_to_branch(LoopCookie, Branch, Parameters, ParamDict) ->
    %% Check if there already is a loop cookie in this branch. Necessary to not
    %% get the wrong branch in constructed ACK of non-2xx response to INVITE.
    FlatBranch = lists:flatten(Branch),
    case string:rstr(FlatBranch, "-o") of
	0 ->
	    logger:log(debug, "Siprequest (transport layer) : Added loop cookie ~p to branch of request",
		       [LoopCookie]),
	    Param2 = dict:append("branch", "-o" ++ LoopCookie, ParamDict),
	    sipheader:dict_to_param(Param2);
	Index when integer(Index) ->
	    %% There is already a loop cookie in this branch, don't change it. Necessary to not
	    %% get the wrong branch in constructed ACK of non-2xx response to INVITE.
	    logger:log(debug, "Siprequest (transport layer) : NOT adding generated loop cookie ~p to branch " ++
		       "that already contains a loop cookie : ~p", [LoopCookie, FlatBranch]),
	    Parameters
    end.

%%--------------------------------------------------------------------
%% Function: add_stateless_generated_branch(Header, Method, OrigURI,
%%                                          LoopCookie, Parameters,
%%                                          SrvTHandler)
%%           Header      = keylist record()
%%           Method      = string()
%%           OrigURI     = sipurl record()
%%           LoopCookie  = string() | none
%%           Parameters  = list() of string()
%%           SrvTHandler = term(), (thandler record())
%% Descrip.: If LoopCookie is not 'none', add it to the Branch if
%%           Branch does not already contain a loop cookie. Return
%%           a new Parameters construct.
%% Returns : NewParameters, list() of string()
%%--------------------------------------------------------------------
add_stateless_generated_branch(Header, Method, OrigURI, LoopCookie, Parameters, SrvTHandler)
  when is_record(Header, keylist), is_list(Method), is_record(OrigURI, sipurl), 
       is_list(LoopCookie), is_list(Parameters) ->
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
	    NewBranch =
		case LoopCookie of
		    none ->
			logger:log(debug, "Siprequest (transport layer) : Added statelessly "
				   "generated branch ~p to request", [Branch]),
			Branch;
		    _ ->
			NewBranch1 = Branch ++ "-o" ++ LoopCookie,
			logger:log(debug, "Siprequest (transport layer) : Added statelessly "
				   "generated branch (plus loop cookie) ~p to request", [NewBranch1]),
			NewBranch1
		end,
	    ParamDict = sipheader:param_to_dict(Parameters),
	    Param2 = dict:store("branch", NewBranch, ParamDict),
	    sipheader:dict_to_param(Param2)
    end.

stateless_generate_branch(OrigURI, Header) ->
    %% generate a branch for this request in a way that makes sure that we generate
    %% the same branch for a retransmission of this very same request. RFC3261 #16.11
    case sipheader:topvia(Header) of
	TopVia when record(TopVia, via) ->
	    case sipheader:get_via_branch(TopVia) of
		"z9hG4bK" ++ RestOfBranch ->
		    In = lists:flatten(lists:concat([node(), "-rbranch-", RestOfBranch])),
		    "z9hG4bK-yxa-" ++ make_base64_md5_token(In);
		_ ->
		    %% No branch, or non-RFC3261 branch
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
	    logger:log(error, "Siprequest (transport layer) : Can't generate stateless branch for this request, it has no top Via!"),
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
				   TopVia when record(TopVia, via) ->
				       P = sipsocket:viaproto2proto(TopVia#via.proto),
				       {TopVia#via.host, default_port(P, TopVia#via.port)};
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

%% Function: create_via/2
%% Description: Create a Via for this proxy, given the protocol.
%% Returns: Via
%%--------------------------------------------------------------------
create_via(Proto, Parameters) ->
    Hostname = siprequest:myhostname(),
    Port = sipserver:get_listenport(Proto),
    ViaProto = sipsocket:proto2viastr(Proto),
    #via{proto=ViaProto, host=Hostname, port=Port, param=Parameters}.

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
    Route = "<" ++ sipurl:print(#sipurl{user=none, pass=none, host=Hostname, port=Port, param=Param2}) ++ ">",
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
    Response = #response{status=401, reason="Authentication Required",
			 header=standardcopy(Header, ExtraHeaders), body=""},
    send_response(Socket, Response).

send_proxyauth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"Proxy-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    Response = #response{status=407, reason="Proxy Authentication Required",
                         header=standardcopy(Header, ExtraHeaders), body=""},
    send_response(Socket, Response).

send_redirect(Location, Header, Socket) when record(Location, sipurl) ->
    Contact = [{none, Location}],
    ExtraHeaders = [{"Contact",
		     sipheader:contact_print(Contact)}],
    Response = #response{status=302, reason="Moved Temporarily",
			 header=standardcopy(Header, ExtraHeaders), body=""},
    send_response(Socket, Response).

send_notfound(Header, Socket) ->
    Response = #response{status=404, reason="Not found",
			 header=standardcopy(Header, []), body=""},
    send_response(Socket, Response).

send_notavail(Header, Socket) ->
    %% XXX CHECK THIS! SUSPECT IT SHOULD BE ["180"]
    ExtraHeaders = [{"Retry-After", "180"}],
    Response = #response{status=480, reason="Temporarily unavailable",
                         header=standardcopy(Header, ExtraHeaders), body=""},
    send_response(Socket, Response).

send_answer(Header, Socket, Body) ->
    %% XXX CHECK THIS! SUSPECT IT SHOULD BE ["application/sdp"] AND [integer_to_list(length(Body))]
    ExtraHeaders = [{"Content-Type", "application/sdp"},
		    {"Content-Length",
		     integer_to_list(length(Body))}],
    Response = #response{status=200, reason="OK",
			 header=standardcopy(Header, ExtraHeaders), body=""},
    send_response(Socket, Response).

send_result(RequestHeader, Socket, Body, Status, Reason) ->
    Response = #response{status=Status, reason=Reason,
			 header=standardcopy(RequestHeader, []), body=Body},
    send_response(Socket, Response).

send_result(RequestHeader, Socket, Body, Status, Reason, ExtraHeaders) ->
    Response = #response{status=Status, reason=Reason,
			 header=standardcopy(RequestHeader, ExtraHeaders), body=Body},
    send_response(Socket, Response).

send_proxy_response(Socket, Response) when record(Response, response) ->
    case sipheader:via(keylist:fetch("Via", Response#response.header)) of
	[Self] ->
	    logger:log(error, "Can't proxy response ~p ~s because it contains just one or less Via and that should be mine!",
		       [Response#response.status, Response#response.reason]),
	    {error, invalid_Via};
	[Self | Via] ->
	    %% Remove Via matching me (XXX should check that it does)
	    NewHeader = keylist:set("Via", sipheader:via_print(Via), Response#response.header),
	    %% Now look for the correct "server transaction" to use when sending this response upstreams
	    [NextVia | _] = Via,
	    NewResponse = Response#response{header=NewHeader},
	    send_response(Socket, NewResponse)
    end.

make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SipSocket, Request) when record(SipSocket, sipsocket) ->
    make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SipSocket#sipsocket.proto, Request);
make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, Proto, Request) when record(Request, request) ->
    ReqHeader = Request#request.header,
    AnswerHeader1 = keylist:appendlist(keylist:copy(ReqHeader, ["Via", "From", "To", "Call-ID", "CSeq",
								"Record-Route", "Timestamp", "Content-Type"]),
				       ExtraHeaders),
    %% PlaceHolderVia is an EXTRA Via with our hostname. We could do without this if
    %% we sent it with send_response() instead of send_proxy_response() but it is easier
    %% to just add an extra Via that will be stripped by send_proxy_response() and don't
    %% have to make a difference in how we send out responses.
    V = create_via(Proto, ViaParameters),
    PlaceHolderVia = sipheader:via_print([V]),
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
    #response{status=Status, reason=Reason, header=AnswerHeader5, body=Body}.
