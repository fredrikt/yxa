%%%-------------------------------------------------------------------
%%% File    : siprequest.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: Various functions related to SIP requests.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(siprequest).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 make_answerheader/1,
	 add_record_route/2,
	 add_record_route/4,
	 myhostname/0,
	 create_via/2,
	 default_port/2,
	 get_loop_cookie/3,
	 generate_branch/0,
	 make_response/7,
	 process_route_header/2,
	 check_proxy_request/1,
	 make_base64_md5_token/1,
	 stateless_route_proxy_request/1,
	 fix_content_length/2,
	 binary_make_message/3,
	 proxy_add_via/4,
	 standardcopy/2,
	 get_approximate_msgsize/1,
	 set_request_body/2,
	 set_response_body/2,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Send-response help functions. Not really transport layer but still
%% kind of mis-placed.
%%--------------------------------------------------------------------
-export([
	 send_redirect/3,
	 send_auth_req/4,
	 send_proxyauth_req/4,
	 send_answer/3,
	 send_notavail/2,
	 send_notfound/2
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: fix_content_length(Header, Body)
%%           Header = keylist record()
%%           Body   = string()
%% Descrip.: Before sending out requests/responses, make sure the
%%           Content-Length is correct (don't trust the length
%%           computed by some previous hop). This is partly for
%%           backwards compliance with RFC2543 UAC's who were not
%%           required to include a Content-Length.
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
fix_content_length(Header, Body) ->
    keylist:set("Content-Length", [integer_to_list(length(Body))], Header).

%%--------------------------------------------------------------------
%% Function: default_port(Proto, Port)
%%           Proto = atom(), udp | udp6 | tcp | tcp6 | tls | tcp6 |
%%                   string(), "sip" | "sips"
%%           Port  = integer() | none
%% Descrip.: Yucky function returning a "default port number" as a
%%           string, based on input Proto and Port.
%% Returns : PortString = string()
%%--------------------------------------------------------------------
default_port(Proto, none) when Proto == udp; Proto == udp6; Proto == tcp; Proto == tcp6; Proto == "sip" ->
    5060;
default_port(Proto, none) when Proto == tls; Proto == tls6 ; Proto == "sips" ->
    5061;
default_port(_, Port) when is_integer(Port) ->
    Port.

%%--------------------------------------------------------------------
%% Function: process_route_header(Header, URI)
%%           Header = keylist record()
%%           URI    = sipurl record()
%% Descrip.: Looks at the Route header. If it exists, we return a new
%%           destination URL for this request, and if there was no
%%           loose router flag in the new destination we append the
%%           original Request-URI to the list of routes, to traverse
%%           strict (RFC2543) proxys.
%% Returns : {ok, NewHeader, NewDestURI, NewRequestURI}  |
%%           nomatch, there was no Route: header
%%           NewHeader     = keylist record()
%%           NewDestURI    = sipurl record(), this is what you should
%%                                            use as destination
%%           NewRequestURI = sipurl record(), this is what you should
%%                                            use as request URI
%%--------------------------------------------------------------------
process_route_header(Header, URI) when is_record(Header, keylist), is_record(URI, sipurl) ->
    Route = sipheader:route(Header),
    case Route of
        [#contact{urlstr = FirstRouteStr} | T] ->
	    FirstRoute = sipurl:parse(FirstRouteStr),
	    {NewHeader, NewURI} =
		case is_loose_router(FirstRoute) of
		    true ->
			{set_route(T, Header), URI};
		    false ->
			logger:log(debug,
				   "Routing: First Route ~p is a strict (RFC2543) router, appending original " ++
				   "destination URI ~p to Route header",
				   [FirstRouteStr, sipurl:print(URI)]),
			NewRoute2 = lists:append(T, [contact:new(none, URI, [])]),
			{set_route(NewRoute2, Header), FirstRoute}
		end,
	    logger:log(debug, "Routing: New destination is ~p, new Request-URI is ~p",
		       [FirstRouteStr, sipurl:print(NewURI)]),
	    {ok, NewHeader, FirstRoute, NewURI};
	[] ->
	    nomatch
    end;
process_route_header(H, U) ->
    erlang:fault("process_route_header called with illegal arguments", [H, U]).

%%--------------------------------------------------------------------
%% Function: set_route(Route, Header)
%%           Route = list() of contact record() | []
%% Descrip.: If Route is an empty list ([]), delete any existing
%%           Route: headers from Header. Otherwise, set the Route:
%%           header(s) in Header to Route.
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
set_route([], Header) ->
    keylist:delete('route', Header);
set_route(Route, Header) ->
    keylist:set("Route", sipheader:contact_print(Route), Header).

%%--------------------------------------------------------------------
%% Function: is_loose_router(Route)
%%           Route = sipurl record()
%% Descrip.: Look for a loose router indicator ("lr") in the
%%           contact-parameters of Route.
%% Returns : true | false
%%--------------------------------------------------------------------
is_loose_router(Route) when is_record(Route, sipurl) ->
    case url_param:find(Route#sipurl.param_pairs, "lr") of
	[_] ->
	     true;
	[] ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: stateless_route_proxy_request(Request)
%%           Request = request record()
%% Descrip.: Inspect a request that some Yxa application has found it
%%           should proxy statelessly. Figure out a single sipdst
%%           destination for this request (sorry, stateless SIP does
%%           not handle multiple destinations) and return that
%%           destination plus a new request (the headers might change
%%           if we have popped a Route header).
%% Returns : {ok, Dst, NewRequest} |
%%           throw {siperror, ...}
%%           Dst        = sipdst record()
%%           NewRequest = request record()
%%--------------------------------------------------------------------
stateless_route_proxy_request(Request) when is_record(Request, request) ->
    {ok, NewHeader, ApproxMsgSize} = check_proxy_request(Request),
    case stateless_route_proxy_request2(Request#request{header=NewHeader}, ApproxMsgSize) of
	{ok, [Dst], NewRequest} ->
	    {ok, Dst, NewRequest};
	{ok, [Dst | Rest], NewRequest} ->
	    logger:log(debug, "Warning: request being forwarded statelessly had more than one destination, "
		       "ignoring all but the first one :~n~p", [sipdst:debugfriendly([Dst | Rest])]),
	    {ok, Dst, NewRequest};
	_ ->
	    {error, "stateless_route_proxy_request() failed"}
    end.

%% part of stateless_route_proxy_request/1
stateless_route_proxy_request2(Request, ApproxMsgSize) ->
    {Header, URI} = {Request#request.header, Request#request.uri},
    case process_route_header(Header, URI) of
	nomatch ->
	    case sipdst:url_to_dstlist(URI, ApproxMsgSize, URI) of
		DstList when is_list(DstList) ->
		    {ok, DstList, Request};
		Unknown ->
		    logger:log(error, "Siprequest (transport layer) : Failed resolving URI ~s : ~p",
			       [sipurl:print(URI), Unknown]),
		    {error, "Failed resolving destination"}
	    end;
	{ok, NewHeader1, DstURI, ReqURI} when is_record(DstURI, sipurl), is_record(ReqURI, sipurl) ->
	    logger:log(debug, "Siprequest (transport layer) : Routing request as per the Route header,"
		       " Destination ~p, Request-URI ~p",
		       [sipurl:print(DstURI), sipurl:print(ReqURI)]),
	    case sipdst:url_to_dstlist(DstURI, ApproxMsgSize, ReqURI) of
		DstList when is_list(DstList) ->
		    {ok, DstList, Request#request{header=NewHeader1}};
		Unknown ->
		    logger:log(error, "Siprequest (transport layer) : Failed resolving URI "
			       "(from Route header) ~s : ~p", [sipurl:print(URI), Unknown]),
		    {error, "Failed resolving destination"}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: check_proxy_request(Request)
%%           Request = request record()
%% Descrip.: Prepares a request for proxying. Checks Max-Forwards,
%%           etc. Not guaranteed to return, might throw a siperror if
%%           the request should not be proxied.
%% Returns : {ok, NewHeader, ApproxMsgSize} |
%%           throw {siperror, ...}
%%--------------------------------------------------------------------
check_proxy_request(Request) when is_record(Request, request) ->
    NewHeader1 = proxy_check_maxforwards(Request#request.header),
    check_valid_proxy_request(Request#request.method, NewHeader1),
    NewHeader2 = fix_content_length(NewHeader1, Request#request.body),
    ApproxMsgSize = get_approximate_msgsize(Request#request{header=NewHeader2}),
    {ok, NewHeader2, ApproxMsgSize}.

%%--------------------------------------------------------------------
%% Function: get_approximate_msgsize(Request)
%%           Request = request record()
%% Descrip.: Approximate how big a request will be when we send it.
%%           We must know this when determining if it is OK to send
%%           it to a UDP destination or not.
%% Returns : Size = integer()
%%--------------------------------------------------------------------
get_approximate_msgsize(Request) when is_record(Request, request) ->
    BinLine1 = list_to_binary([Request#request.method, " ", sipurl:print(Request#request.uri), " SIP/2.0"]),
    BinMsg = binary_make_message(BinLine1, Request#request.header, list_to_binary(Request#request.body)),
    ViaLen = 92 + length(siprequest:myhostname()),
    %% The size is _approximate_. The exact size cannot be calculated here
    %% since it is dependent on the right Request-URI and length of the final
    %% Via header we insert, not the 92 bytes + hostname _estimate_.
    ApproxMsgSize = size(BinMsg) + ViaLen,
    ApproxMsgSize.

%%--------------------------------------------------------------------
%% Function: proxy_check_maxforwards(Header)
%%           Header = keylist record()
%% Descrip.: Check Max-Forwards in Header to make sure it is not less
%%           than one when we have subtracted one (1) from it. Return
%%           a new Header with the new Max-Forwards.
%% Returns : NewHeader |
%%           throw({siperror, ...})
%%           NewHeader = keylist record()
%%--------------------------------------------------------------------
proxy_check_maxforwards(Header) ->
    MaxForwards =
	case keylist:fetch('max-forwards', Header) of
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
%% Function: proxy_add_via(Header, Method, OrigURI, Parameters, Proto)
%%           Header      = keylist record()
%%           OrigURI     = sipurl record()
%%           Parameters  = string()
%%           Proto       = ???
%% Descrip.: Generate a Via header for this proxy and add it to Header
%% Returns : NewHeader, keylist record()
%%--------------------------------------------------------------------
proxy_add_via(Header, OrigURI, Parameters, Proto) ->
    LoopCookie = case sipserver:get_env(detect_loops, true) of
		     true ->
			 get_loop_cookie(Header, OrigURI, Proto);
		     false ->
			 none
		 end,
    ParamDict = sipheader:param_to_dict(Parameters),
    ViaParameters1 = case dict:find("branch", ParamDict) of
			 error ->
			     add_stateless_generated_branch(Header, OrigURI, LoopCookie, Parameters);
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
	    Param2 = dict:store("branch", lists:concat([FlatBranch, "-o", LoopCookie]), ParamDict),
	    sipheader:dict_to_param(Param2);
	Index when is_integer(Index) ->
	    %% There is already a loop cookie in this branch, don't change it. Necessary to not
	    %% get the wrong branch in constructed ACK of non-2xx response to INVITE.
	    logger:log(debug, "Siprequest (transport layer) : NOT adding generated loop cookie ~p to branch " ++
		       "that already contains a loop cookie : ~p", [LoopCookie, FlatBranch]),
	    Parameters
    end.

%%--------------------------------------------------------------------
%% Function: add_stateless_generated_branch(Header, OrigURI,
%%                                          LoopCookie, Parameters)
%%           Header      = keylist record()
%%           OrigURI     = sipurl record()
%%           LoopCookie  = string() | none
%%           Parameters  = list() of string()
%% Descrip.: If LoopCookie is not 'none', add it to the Branch if
%%           Branch does not already contain a loop cookie. Return
%%           a new Parameters construct.
%% Returns : NewParameters, list() of string()
%%--------------------------------------------------------------------
add_stateless_generated_branch(Header, OrigURI, LoopCookie, Parameters)
  when is_record(Header, keylist), is_record(OrigURI, sipurl), is_list(LoopCookie), is_list(Parameters) ->
    case stateless_generate_branch(OrigURI, Header) of
	error ->
	    logger:log(error, "Siprequest: Failed adding stateless branch to request, "
		       "throwing a '500 Server Internal Error'"),
	    throw({siperror, 500, "Server Internal Error"});
	{ok, Branch} ->
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

%%--------------------------------------------------------------------
%% Function: stateless_generate_branch(OrigURI, Header)
%%           OrigURI = sipurl record(), the original requests URI
%%           Header  = keylist record()
%% Descrip.: Generate a branch suitable for stateless proxying of a
%%           request (meaning that we make sure that we generate the
%%           very same branch for a retransmission of the very same
%%           request). This is specified in RFC3261 #16.11.
%% Returns : {ok, Branch} |
%%           error
%%           Branch = string()
%%--------------------------------------------------------------------
stateless_generate_branch(OrigURI, Header) ->
    case sipheader:topvia(Header) of
	TopVia when is_record(TopVia, via) ->
	    case sipheader:get_via_branch(TopVia) of
		"z9hG4bK" ++ RestOfBranch ->
		    %% The previous hop has put a RFC3261 branch in it's Via. Use that,
		    %% togehter with our nodename as branch.
		    In = lists:flatten(lists:concat([node(), "-rbranch-", RestOfBranch])),
		    {ok, "z9hG4bK-yxa-" ++ make_base64_md5_token(In)};
		_ ->
		    %% No branch, or non-RFC3261 branch
		    OrigURIstr = sipurl:print(OrigURI),
		    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
		    ToTag = sipheader:get_tag(keylist:fetch('to', Header)),
		    CallId = keylist:fetch('call-id', Header),
		    {CSeqNum, _} = sipheader:cseq(keylist:fetch('cseq', Header)),
		    In = lists:flatten(lists:concat([node(), "-uri-", OrigURIstr, "-ftag-", FromTag, "-totag-", ToTag,
						     "-callid-", CallId, "-cseqnum-", CSeqNum])),
		    {ok, "z9hG4bK-yxa-" ++ make_base64_md5_token(In)}
	    end;
	_ ->
	    logger:log(error, "Siprequest (transport layer) : Can't generate stateless branch for this request, it has no top Via!"),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: make_base64_md5_token(In)
%%           In = term(), indata - anything accepted by erlang:md5()
%% Descrip.: Make md5 of input, base64 of that and RFC3261 token of
%%           the result.
%% Returns : Result of make_3261_token()
%%--------------------------------------------------------------------
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
H == $*; H == $_; H == $+; H == $`; H == $\'; H == $~ ->
    [H|make_3261_token(T)];
make_3261_token([_H | T]) ->
    [$_|make_3261_token(T)].

%%--------------------------------------------------------------------
%% Function: generate_branch()
%% Descrip.: Generate a branch that is 'unique across space and time'.
%% Returns : Branch = string()
%%--------------------------------------------------------------------
generate_branch() ->
    {Megasec, Sec, Microsec} = now(),
    %% We don't need port here since erlang guarantees that Microsecond is never the
    %% same on one node.
    In = lists:concat([node(), Megasec * 1000000 + Sec, 8, $., Microsec]),
    Out = make_base64_md5_token(In),
    "z9hG4bK-yxa-" ++ Out.

%%--------------------------------------------------------------------
%% Function: make_answerheader(Header)
%% Descrip.: Turn Request-Route header from request, into Route header
%%           to include in a response (answer).
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
make_answerheader(Header) ->
    RecordRoute = keylist:fetch('record-route', Header),
    NewHeader1 = keylist:delete('Record-Route', Header),
    NewHeader2 = case RecordRoute of
		     [] ->
			 NewHeader1;
		     _ ->
			 keylist:set("Route", RecordRoute, NewHeader1)
		 end,
    NewHeader2.

%%--------------------------------------------------------------------
%% Function: get_loop_cookie(Header, OrigURI, Proto)
%%           Header  = keylist record()
%%           OrigURI = sipurl record(), original requests URI
%%           Proto   = term(), the protocol the request was received
%%           over
%% Descrip.: Generate a loop detection cookie, RFC3261 16.6 #8.
%% Returns : Cookie = string()
%%--------------------------------------------------------------------
get_loop_cookie(Header, OrigURI, Proto) ->
    OrigURIstr = sipurl:print(OrigURI),
    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
    ToTag = sipheader:get_tag(keylist:fetch('to', Header)),
    CallId = keylist:fetch('call-id', Header),
    {CSeqNum, _} = sipheader:cseq(Header),
    ProxyReq = keylist:fetch('proxy-require', Header),
    %% We must remove the response part from Proxy-Authorization because it changes with the method
    %% and thus CANCEL does not match INVITE. Contradictingly but implicitly from RFC3261 16.6 #8.
    ProxyAuth = proxyauth_without_response(Header),
    Route = keylist:fetch('route', Header),
    {TopViaHost, TopViaPort} = case sipheader:topvia(Header) of
				   TopVia when is_record(TopVia, via) ->
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

%%--------------------------------------------------------------------
%% Function: proxyauth_without_response(Header)
%%           Header  = keylist record()
%% Descrip.: Helper-function for get_loop_cookie(). Remove parts of
%%           Proxy-Authorization header that change based on method.
%% Returns : Result = list() of string() ???
%%--------------------------------------------------------------------
proxyauth_without_response(Header) ->
    case keylist:fetch('proxy-authorization', Header) of
	[] -> none;
	ProxyAuth ->
	    AuthDict = sipheader:auth(ProxyAuth),
	    NewDict1 = dict:erase("response", AuthDict),
	    NewDict2 = dict:erase("nonce", NewDict1),
	    NewDict3 = dict:erase("cnonce", NewDict2),
	    NewDict4 = dict:erase("opaque", NewDict3),
	    sipheader:dict_to_param(NewDict4)
    end.

%%--------------------------------------------------------------------
%% Function: check_valid_proxy_request(Method, Header)
%%           Method = string()
%%           Header = keylist record()
%% Descrip.: Check if a request has any Proxy-Require header that
%%           includes an extension that we don't support. Always
%%           return true on ACK and CANCEL - we can never reject
%%           those.
%% Returns : true | false
%%--------------------------------------------------------------------
check_valid_proxy_request("ACK", _) ->
    true;
check_valid_proxy_request("CANCEL", _) ->
    true;
check_valid_proxy_request(_Method, Header) ->
    ProxyRequire = keylist:fetch('proxy-require', Header),
    case ProxyRequire of
	[] ->
	    true;
	_ ->
	    logger:log(normal, "Proxy Request check: The client requires unsupported extension(s) ~p", [ProxyRequire]),
	    throw({siperror, 420, "Bad Extension", [{"Unsupported", ProxyRequire}]})
    end.

%%--------------------------------------------------------------------
%% Function: myhostname()
%% Descrip.: Get my hostname (the first one in the list, or my IP
%%           address if I have no list of configured hostnames).
%% Returns : Hostname = string()
%%--------------------------------------------------------------------
myhostname() ->
    case sipserver:get_env(myhostnames, []) of
	[] ->
	    siphost:myip();
	Hostnames ->
	    lists:nth(1, Hostnames)
    end.

%%--------------------------------------------------------------------
%% Function: create_via(Proto, Parameters)
%%           Proto      = term()
%%           Parameters = list() of string()
%% Descrip.: Create a Via for this proxy, given the protocol.
%% Returns : Via = via record()
%%--------------------------------------------------------------------
create_via(Proto, Parameters) ->
    Hostname = siprequest:myhostname(),
    Port = sipserver:get_listenport(Proto),
    ViaProto = sipsocket:proto2viastr(Proto),
    #via{proto=ViaProto, host=Hostname, port=Port, param=Parameters}.

%%--------------------------------------------------------------------
%% Function: add_record_route(Proto, Hostname, Port, Header)
%%           Proto      = term()
%%           Hostname   = string()
%%           Port       = integer() | none ???
%%           Header     = keylist record()
%% Descrip.: Prepend a Record-Route header for Proto:Host:Port to the
%%           Header keylist.
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
add_record_route(Proto, Hostname, Port, Header) ->
    Param1 = ["maddr=" ++ siphost:myip(), "lr=true"],
    Param2 = if
		 Proto == tcp; Proto == tcp6 ->
		     lists:append(Param1, ["transport=TCP"]);
		 true ->
		     Param1
	     end,
    RouteStr = sipurl:print(sipurl:new([{host, Hostname}, {port, Port}, {param, Param2}])),
    %% Check if our Record-Route (CRoute) is already present as the first entry of
    %% the Record-Route set in Header.
    AlreadyPresent =
	case sipheader:record_route(Header) of
	    [FirstRoute | _] when is_record(FirstRoute, contact) ->
		%% XXX use sipurl:url_is_equal/2?
		case FirstRoute#contact.urlstr of
		    RouteStr -> true;
		    _ -> false
		end;
	    _ ->
		%% No Record-Route in header
		false
	end,
    case AlreadyPresent of
	true ->
	    logger:log(debug, "Siprequest: NOT adding Record-Route since the first Record-Route matches mine"),
	    Header;
	false ->
	    [Route] = sipheader:contact_print([ contact:new(none, RouteStr, []) ]),
	    keylist:prepend({"Record-Route", [Route]}, Header)
    end.
%%--------------------------------------------------------------------
%% Function: add_record_route(Header, Origin)
%%           Header = keylist record()
%%           Origin = siporigin record()
%% Descrip.: Prepend a Record-Route header for this proxy, based on
%%           the Origin record, to Header.
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
add_record_route(Header, Origin) when is_record(Origin, siporigin) ->
    Port = sipserver:get_listenport(Origin#siporigin.proto),
    add_record_route(Origin#siporigin.proto, myhostname(), Port, Header).

%%--------------------------------------------------------------------
%% Function: standardcopy(Header, ExtraHeaders)
%%           Header = keylist record()
%%           ExtraHeaders = keylist record()
%% Descrip.: Copy the headers that are required for all responses
%%           (Via, From, To, Call-Id, CSeq) plus any extra requested
%%           headers from a source Header keylist and create a new
%%           keylist based on these.
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
standardcopy(Header, ExtraHeaders) ->
    keylist:appendlist(keylist:copy(Header,
				    [via, from, to, 'call-id', cseq]),
		       ExtraHeaders).

send_auth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"WWW-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    Response1 = #response{status=401, reason="Authentication Required",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

send_proxyauth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"Proxy-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    Response1 = #response{status=407, reason="Proxy Authentication Required",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

send_redirect(Location, Header, Socket) when is_record(Location, sipurl) ->
    Contact = contact:new(none, Location, []),
    ExtraHeaders = [{"Contact",
		     sipheader:contact_print([Contact])}],
    Response1 = #response{status=302, reason="Moved Temporarily",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

send_notfound(Header, Socket) ->
    Response1 = #response{status=404, reason="Not found",
			  header=standardcopy(Header, [])},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

send_notavail(Header, Socket) ->
    ExtraHeaders = [{"Retry-After", ["180"]}],
    Response1 = #response{status=480, reason="Temporarily unavailable",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

send_answer(Header, Socket, Body) ->
    %% Remember to add a linefeed (\n) to the end of Body that you pass to this function
    ExtraHeaders = [{"Content-Type", ["application/sdp"]}],
    Response1 = #response{status=200, reason="OK",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, Body),
    transportlayer:send_response(Socket, Response).

%%--------------------------------------------------------------------
%% Function: make_response(Status, Reason, Body, ExtraHeaders,
%%                         ViaParameters, SipSocket, Request)
%%           Status    = integer(), SIP status code
%%           Reason    = string(), SIP reason phrase
%%           Body      = string()
%%           ExtraHeaders = keylist record()
%%           ViaParameters = list() of string()
%%           SipSocket = sipsocket record() | atom(), protocol
%%                       (tcp | tcp6 | tls | tls6 | udp | udp6)
%%           Request   = request record()
%% Descrip.: Create a response given a request.
%% Returns : Response = response record()
%%--------------------------------------------------------------------
make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SipSocket, Request)
  when is_record(SipSocket, sipsocket) ->
    make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SipSocket#sipsocket.proto, Request);
make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, Proto, Request)
  when is_record(Request, request) ->
    ReqHeader = Request#request.header,
    AnswerHeader1 = keylist:appendlist(keylist:copy(ReqHeader, [via, from, to, 'call-id', cseq,
								'record-route', "Timestamp", 'content-type']),
				       ExtraHeaders),
    %% PlaceHolderVia is an EXTRA Via with our hostname. We could do without this if
    %% we sent it with send_response() instead of send_proxy_response() but it is easier
    %% to just add an extra Via that will be stripped by send_proxy_response() and don't
    %% have to make a difference in how we send out responses.
    V = create_via(Proto, ViaParameters),
    PlaceHolderVia = sipheader:via_print([V]),
    AnswerHeader2 = keylist:prepend({"Via", PlaceHolderVia}, AnswerHeader1),
    AnswerHeader3 = siprequest:make_answerheader(AnswerHeader2),
    %% If there is a body, calculate Content-Length, otherwise remove the Content-Type we copied above
    AnswerHeader4 = case Body of
			"" -> keylist:delete('content-type', AnswerHeader3);
			_ -> keylist:set("Content-Length", [integer_to_list(length(Body))], AnswerHeader3)
		    end,
    %% If this is not a 100 Trying response, remove the Timestamp we copied above.
    %% The preservation of Timestamp headers into 100 Trying response is mandated by RFC 3261 8.2.6.1
    AnswerHeader5 = case Status of
			100 -> AnswerHeader4;
			_ -> keylist:delete("Timestamp", AnswerHeader4)
		    end,
    set_response_body(#response{status=Status, reason=Reason, header=AnswerHeader5}, Body).

binary_make_message(BinLine1, Header, BinBody) when is_binary(BinLine1), is_record(Header, keylist),
						    is_binary(BinBody) ->
    BinHeaders = sipheader:build_header_binary(Header),
    concat_binary([BinLine1, 13, 10, BinHeaders, 13, 10, BinBody]).

%%--------------------------------------------------------------------
%% Function: set_request_body(Request, Body)
%%           Request = request record()
%%           Body    = list() or binary()
%% Descrip.: Set the body of a request record.
%% Returns : NewRequest = request record()
%%--------------------------------------------------------------------
set_request_body(Request, Body) when is_record(Request, request), is_binary(Body) ->
    set_request_body(Request, binary_to_list(Body));
set_request_body(Request, Body) when is_record(Request, request), is_list(Body) ->
    ILen = length(Body),
    Len = integer_to_list(ILen),
    NewHeader = keylist:set("Content-Length", [Len], Request#request.header),
    Request#request{header=NewHeader, body=Body}.

%%--------------------------------------------------------------------
%% Function: set_response_body(Request, Body)
%%           Request = request record()
%%           Body    = list() or binary()
%% Descrip.: Set the body of a request record, and calculate
%%           Content-Length.
%% Returns : NewRequest = request record()
%%--------------------------------------------------------------------
set_response_body(Response, Body) when is_record(Response, response), is_binary(Body) ->
    set_response_body(Response, binary_to_list(Body));
set_response_body(Response, Body) when is_record(Response, response), is_list(Body) ->
    ILen = length(Body),
    Len = integer_to_list(ILen),
    NewHeader = keylist:set("Content-Length", [Len], Response#response.header),
    Response#response{header=NewHeader, body=Body}.

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%%--------------------------------------------------------------------
test() ->
    %% myhostname()
    %%--------------------------------------------------------------------
    %% test that we get a list back from myhostname()
    io:format("test: myhostname/0 - 1~n"),
    MyHostname = myhostname(),
    true = is_list(MyHostname),

    %% test that generate_branch gives us a RFC3261 branch
    io:format("test: generate_branch/0 - 1~n"),
    Branch = generate_branch(),
    "z9hG4bK-yxa-" = string:substr(Branch, 1, 12),

    %% test that our make_3261_token produces the expected results
    io:format("test: make_3261_token/1 - 1~n"),
    "abcdeXYZ-.!%*_+`'~123_" = make_3261_token("abcdeXYZ-.!%*_+`'~123@"),

    io:format("test: make_base64_md5_token/1 - 1~n"),
    "Wd_CG1j2CfuAv0NWdJLxTQ" = make_base64_md5_token("abcdeXYZ123-.!%*_+`'~123@"),


    %% default_port(Proto, PortIn)
    %%--------------------------------------------------------------------

    %% udp/udp6/tcp/tcp6/"sip"
    io:format("test: default_port/2 - 1~n"),
    5060 = default_port(udp, none),
    io:format("test: default_port/2 - 2~n"),
    5060 = default_port(udp6, none),
    io:format("test: default_port/2 - 3~n"),
    5060 = default_port(tcp, none),
    io:format("test: default_port/2 - 4~n"),
    5060 = default_port(tcp6, none),
    io:format("test: default_port/2 - 5~n"),
    5060 = default_port("sip", none),

    %% tls/tls6/"sips"
    io:format("test: default_port/2 - 6~n"),
    5061 = default_port(tls, none),
    io:format("test: default_port/2 - 7~n"),
    5061 = default_port(tls6, none),
    io:format("test: default_port/2 - 8~n"),
    5061 = default_port("sips", none),

    %% none of the above, port specified
    io:format("test: default_port/2 - 9~n"),
    1234 = default_port(whatever, 1234),

    %% none of the above, port NOT specified - expect crash
    io:format("test: default_port/2 - 11~n"),
    {'EXIT', {function_clause, _}} = (catch default_port("foo", none)),


    %% create_via(Proto, Parameters)
    %%--------------------------------------------------------------------

    SipLPort  = sipserver:get_listenport(tcp),
    SipsLPort = sipserver:get_listenport(tls),

    %% tcp
    io:format("test: create_via/2 - 1~n"),
    #via{proto="SIP/2.0/TCP", host=MyHostname, port=SipLPort, param=["foo=bar"]} =
	create_via(tcp, ["foo=bar"]),

    %% tcp6
    io:format("test: create_via/2 - 2~n"),
    #via{proto="SIP/2.0/TCP", host=MyHostname, port=SipLPort, param=[]} =
	create_via(tcp6, []),

    %% udp
    io:format("test: create_via/2 - 3~n"),
    #via{proto="SIP/2.0/UDP", host=MyHostname, port=SipLPort, param=[]} =
	create_via(udp, []),

    %% udp6
    io:format("test: create_via/2 - 4~n"),
    #via{proto="SIP/2.0/UDP", host=MyHostname, port=SipLPort, param=[]} =
	create_via(udp6, []),

    %% tls
    io:format("test: create_via/2 - 5~n"),
    #via{proto="SIP/2.0/TLS", host=MyHostname, port=SipsLPort, param=[]} =
	create_via(tls, []),

    %% tls6
    io:format("test: create_via/2 - 6~n"),
    #via{proto="SIP/2.0/TLS", host=MyHostname, port=SipsLPort, param=[]} =
	create_via(tls6, []),

    %% foo protocol - expect crash
    io:format("test: create_via/2 - 7~n"),
    {'EXIT', {function_clause, _}} = (catch create_via(foo, [])),


    %% add_record_route(Header, SipOrigin)
    %% and implicitly add_record_route/4
    %%--------------------------------------------------------------------

    io:format("test: add_record_route/2 - 1~n"),
    EmptyHeader = keylist:from_list([]),
    MyIP = siphost:myip(),

    RRHeader1 = add_record_route(EmptyHeader, #siporigin{proto=tcp}),

    %% check that there is now a single Record-Route in RRHeader1
    io:format("test: add_record_route/2 - 2~n"),
    [RRoute1] = sipheader:record_route(RRHeader1),

    %% check the contents in the single Record-Route
    %% contact.display_name
    io:format("test: add_record_route/2 - 3~n"),
    none = RRoute1#contact.display_name,

    %% contact.urlstr
    io:format("test: add_record_route/2 - 4.1~n"),
    #sipurl{proto="sip", host=MyHostname, port=SipLPort, param_pairs=RRParam1} =
	sipurl:parse(RRoute1#contact.urlstr),

    %% contact.urlstr sipurl parameters
    io:format("test: add_record_route/2 - 4.2~n"),
    [MyIP] = url_param:find(RRParam1, "maddr"),
    ["true"] = url_param:find(RRParam1, "lr"),
    ["tcp"] = url_param:find(RRParam1, "transport"),

    %% contact.contact_param
    io:format("test: add_record_route/2 - 5~n"),
    EmptyContactParam = contact_param:to_norm([]),
    EmptyContactParam = RRoute1#contact.contact_param,

    %% udp protocol
    io:format("test: add_record_route/2 - 6.1~n"),
    RRHeader2 = add_record_route(RRHeader1, #siporigin{proto=udp}),

    %% get our RRoute2 and also check that RRoute1 is still there
    io:format("test: add_record_route/2 - 6.2~n"),
    [RRoute2, RRoute1] = sipheader:record_route(RRHeader2),

    %% contact.urlstr
    io:format("test: add_record_route/2 - 6.3~n"),
    #sipurl{proto="sip", host=MyHostname, port=SipLPort, param_pairs=RRParam2} =
	sipurl:parse(RRoute2#contact.urlstr),

    %% contact.urlstr sipurl parameters
    io:format("test: add_record_route/2 - 6.4~n"),
    [MyIP] = url_param:find(RRParam2, "maddr"),
    ["true"] = url_param:find(RRParam2, "lr"),
    [] = url_param:find(RRParam2, "transport"),

    %% check that add_record_route/2 does not add duplicate Record-Route
    io:format("test: add_record_route/2 - 7~n"),
    RRHeader3 = add_record_route(RRHeader2, #siporigin{proto=udp}),

    io:format("test: add_record_route/2 - 8~n"),
    [RRoute2, RRoute1] = sipheader:record_route(RRHeader3),


    %% build request header
    %%--------------------------------------------------------------------

    io:format("test: build request header - 1~n"),
    ReqHeader = keylist:from_list([
				   {"Via",	["SIP/2.0/TLS 130.237.90.1:111",
						 "SIP/2.0/TCP 2001:6b0:5:987::1"]},
				   {"From",	["<sip:test@it.su.se>;tag=f-123"]},
				   {"To",	["<sip:test@it.su.se>;tag=t-123"]},
				   {"CSeq",	["4711 INVITE"]},
				   {"Call-ID",	["abc123@test"]},
				   {"Record-Route", ["<sip:p1:1111>", "<sip:p2:2222>"]},
				   {"Timestamp", ["1234"]},
				   {"Content-Type", ["application/regression-test"]},
				   {"Foo",	["should not get copied to responses"]},
				   {"s",	["test subject short form"]}
				  ]),

    %% make_answerheader(RequestHeader)
    %%--------------------------------------------------------------------

    %% check that our Record-Route was turned into a Route
    io:format("test: make_answerheader/1 - 1~n"),
    AHeader1 = make_answerheader(ReqHeader),
    ["<sip:p1:1111>", "<sip:p2:2222>"] = keylist:fetch('route', AHeader1),


    %% get_loop_cookie(ReqHeader, URI, Proto
    %%--------------------------------------------------------------------

    io:format("test: get_loop_cookie/3 - 1~n"),
    "JYJPsr4IqjymexLGUB58yg" = get_loop_cookie(ReqHeader, sipurl:parse("sip:test@example.org"), tcp),


    %% make_response(Status, Reason, Body, ExtraHeaders, Parameters,
    %%		     Proto, Request)
    %%--------------------------------------------------------------------

    io:format("test: make_response/7 - 1.1~n"),
    Response1 = make_response(100, "Trying", "test", [], [], tcp, #request{header=ReqHeader}),

    %% basic response record check
    io:format("test: make_response/7 - 1.2~n"),
    #response{status=100, reason="Trying", body="test"} = Response1,

    %% Timestamp still present in 100 Trying check
    io:format("test: make_response/7 - 1.3~n"),
    ["1234"] = keylist:fetch("Timestamp", Response1#response.header),

    %% correct Content-Length added
    io:format("test: make_response/7 - 1.4~n"),
    ["4"] = keylist:fetch('content-length', Response1#response.header),

    %% Foo header not copied
    io:format("test: make_response/7 - 1.5~n"),
    [] = keylist:fetch("Foo", Response1#response.header),

    %% test make_response/7
    io:format("test: make_response/7 - 2.1~n"),
    Response2 = make_response(486, "_BUSY_", "", [{"Extra-Header", ["test"]}], ["test=true"],
			      tls6, #request{header=ReqHeader}),

    %% basic response record check
    io:format("test: make_response/7 - 2.2~n"),
    #response{status=486, reason="_BUSY_", body=""} = Response2,

    %% Timestamp NOT present in 486 Busy Here
    io:format("test: make_response/7 - 2.3~n"),
    [] = keylist:fetch("Timestamp", Response2#response.header),

    %% Content-Type NOT present in 486 Busy Here with empty body
    io:format("test: make_response/7 - 2.4~n"),
    [] = keylist:fetch("Content-Type", Response2#response.header),

    %% Extra-Header supplied added correctly
    io:format("test: make_response/7 - 2.5~n"),
    ["test"] = keylist:fetch("Extra-Header", Response2#response.header),

    %% Record-Route turned into Route
    io:format("test: make_response/7 - 2.6~n"),
    ["<sip:p1:1111>", "<sip:p2:2222>"] = keylist:fetch('route', Response2#response.header),

    %% check_proxy_request(Request)
    %%--------------------------------------------------------------------

    io:format("test: check_proxy_request/1 - 1~n"),
    ReqHeader2 = keylist:set("Content-Length", ["900"], ReqHeader),
    Req2 = #request{method="MESSAGE", uri=sipurl:parse("sip:test@example.org"), header=ReqHeader2, body="foo"},

    io:format("test: check_proxy_request/1 - 2~n"),
    {ok, ReqHeader2_2, ApproxMsgSize2_2} = check_proxy_request(Req2),

    %% check that Content-Length has been properly set in ReqHeader2_2
    io:format("test: check_proxy_request/1 - 3~n"),
    ["3"] = keylist:fetch('content-length', ReqHeader2_2),

    %% check that requests get a default Max-Forwards if they lack it
    io:format("test: check_proxy_request/1 - 4~n"),
    ["70"] = keylist:fetch('max-forwards', ReqHeader2_2),

    %% check that the approximate message size is reasonable
    io:format("test: check_proxy_request/1 - 5~n"),
    %% can't do exact match here since size depends on variables like hostname
    %% or IP address as string which is not the same for everyone
    case ApproxMsgSize2_2 of
	_ when ApproxMsgSize2_2 >= 500, ApproxMsgSize2_2 =< 600 ->
	    ok;
	_ ->
	    erlang:fault("approximate message size not within bounds", [ApproxMsgSize2_2])
    end,

    %% check that we don't accept unknown Proxy-Require
    io:format("test: check_proxy_request/1 - 6~n"),
    ReqHeader3 = keylist:set("Proxy-Require", ["CantBeSupported"], ReqHeader2),
    Req3 = Req2#request{method="INVITE", header=ReqHeader3, body=""},
    Unsupported = [{"Unsupported",["CantBeSupported"]}],
    {siperror, 420, _, Unsupported} = (catch check_proxy_request(Req3)),

    %% check that we do accept Max-Forwards 2
    io:format("test: check_proxy_request/1 - 7~n"),
    ReqHeader4 = keylist:set("Max-Forwards", ["2"], ReqHeader),
    Req4 = Req3#request{header=ReqHeader4},
    {ok, _, _} = check_proxy_request(Req4),

    %% check that we don't accept Max-Forwards 1
    io:format("test: check_proxy_request/1 - 8~n"),
    ReqHeader5 = keylist:set("Max-Forwards", ["1"], ReqHeader),
    Req5 = Req3#request{header=ReqHeader5},
    {siperror, 483, _} = (catch check_proxy_request(Req5)),


    %% process_route_header(Header, URI)
    %%--------------------------------------------------------------------

    io:format("test: process_route_header/2 - 1~n"),
    InURI = sipurl:parse("sip:in@example.org"),
    [InURIasContact] = contact:parse(["<sip:in@example.org>"]),
    StrictRouter = "<sip:strict-router.example.org>",
    [StrictRouterContact] = contact:parse(["<sip:strict-router.example.org>"]),
    StrictRouterURL = sipurl:parse("sip:strict-router.example.org"),
    LooseRouter = "<sip:loose-router.example.org;lr=true>",
    [LooseRouterContact] = contact:parse(["<sip:loose-router.example.org;lr=true>"]),
    LooseRouterURL = sipurl:parse("sip:loose-router.example.org;lr=true"),

    %% Test strict router traversal
    io:format("test: process_route_header/2 - 2.1~n"),
    PRHeader1 = keylist:from_list([{"Route", [StrictRouter, LooseRouter]}]),
    {ok, PRHeader1_out, StrictRouterURL, StrictRouterURL} =
	process_route_header(PRHeader1, InURI),

    io:format("test: process_route_header/2 - 2.2~n"),
    %% check the Route headers in the new headers returned
    [LooseRouterContact, InURIasContact] = sipheader:route(PRHeader1_out),

    %% Test strict router traversal with no other Route-header
    io:format("test: process_route_header/2 - 3.1~n"),
    PRHeader2 = keylist:from_list([{"Route", [StrictRouter]}]),
    {ok, PRHeader2_out, StrictRouterURL, StrictRouterURL} =
	process_route_header(PRHeader2, InURI),

    io:format("test: process_route_header/2 - 3.2~n"),
    %% check the Route headers in the new headers returned
    [InURIasContact] = sipheader:route(PRHeader2_out),

    %% Test loose router, with one more Route
    io:format("test: process_route_header/2 - 4.1~n"),
    PRHeader3 = keylist:from_list([{"Route", [LooseRouter, StrictRouter]}]),
    {ok, PRHeader3_out, LooseRouterURL, InURI} =
	process_route_header(PRHeader3, InURI),

    io:format("test: process_route_header/2 - 4.2~n"),
    %% check the Route headers in the new headers returned
    [StrictRouterContact] = sipheader:route(PRHeader3_out),

    %% Test loose router alone
    io:format("test: process_route_header/2 - 5.1~n"),
    PRHeader4 = keylist:from_list([{"Route", [LooseRouter]}]),
    {ok, PRHeader4_out, LooseRouterURL, InURI} =
	process_route_header(PRHeader4, InURI),

    [] = sipheader:route(PRHeader4_out),

    %% check empty header, no Route present
    io:format("test: process_route_header/2 - 6~n"),
    nomatch = process_route_header(keylist:from_list([]), InURI),


    %% create_via(Proto, Parameters)
    %%--------------------------------------------------------------------

    io:format("test: create_via/1 - 1.1~n"),
    TLSBasicVia = create_via(tls, []),

    io:format("test: create_via/1 - 1.2~n"),
    #via{proto="SIP/2.0/TLS", host=MyHostname, port=SipsLPort} = TLSBasicVia,


    %% proxy_add_via(Header, URI, Parameters, Proto)
    %%--------------------------------------------------------------------

    io:format("test: proxy_add_via/1 - 1.1~n"),
    PAVheaderIn1 = keylist:delete("Via", ReqHeader),

    %% test proxy_add_via/1 with no present Via header
    io:format("test: proxy_add_via/1 - 1.2~n"),
    PAVheader1 = proxy_add_via(PAVheaderIn1, InURI,
			       ["branch=z9hG4bK-test"], tcp6),
    TopVia1 = sipheader:topvia(PAVheader1),

    %% get branch from added Via, branch computated depends on your hostname
    %% so we can't check for a static one
    io:format("test: proxy_add_via/1 - 1.3~n"),
    Branch1 = sipheader:get_via_branch_full(TopVia1),

    %% basic check the top via that was added
    io:format("test: proxy_add_via/1 - 1.4~n"),
    #via{proto="SIP/2.0/TCP", host=MyHostname, port=SipLPort,
	 param=["branch="  ++ Branch1]} = TopVia1,

    io:format("test: proxy_add_via/1 - 2.1~n"),
    PAVheaderIn2 = keylist:set("Via", sipheader:via_print([TLSBasicVia]), ReqHeader),

    %% test proxy_add_via/1 with an existing Via header
    io:format("test: proxy_add_via/1 - 2.2~n"),
    PAVheader2 = proxy_add_via(PAVheaderIn2, InURI,
			       ["branch=z9hG4bK-test"], tcp6),

    %% check the two via's that should be present in PAVheader2
    io:format("test: proxy_add_via/1 - 2.3~n"),
    Branch2 = sipheader:get_via_branch_full(sipheader:topvia(PAVheader2)),
    TopVia1_NewLoopCookie = TopVia1#via{param=["branch=" ++ Branch2]},
    [TopVia1_NewLoopCookie, TLSBasicVia] = sipheader:via(PAVheader2),

    ok.
