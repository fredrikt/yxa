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
fix_content_length(Header, Body) when is_record(Header, keylist), is_list(Body) ->
    keylist:set("Content-Length", [integer_to_list(length(Body))], Header);
fix_content_length(Header, Body) when is_record(Header, keylist), is_binary(Body) ->
    keylist:set("Content-Length", [integer_to_list(size(Body))], Header).

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
    end.

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
	    logger:log(debug, "Siprequest (transport layer) : Warning: request being forwarded "
		       "statelessly had more than one destination, ignoring all but the first "
		       "one :~n~p", [sipdst:debugfriendly([Dst | Rest])]),
	    {ok, Dst, NewRequest};
	{error, Reason} ->
	    logger:log(error, "Siprequest (transport layer) : stateless_route_proxy_request2/2 "
		       "returned error : ~p", [Reason]),
	    {error, Reason}
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
    NewHeader3 = add_record_route(Request#request.uri, NewHeader2),
    ApproxMsgSize = get_approximate_msgsize(Request#request{header=NewHeader3}),
    {ok, NewHeader3, ApproxMsgSize}.

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
    BinMsg = binary_make_message(BinLine1, Request#request.header, Request#request.body),
    ViaLen = 92 + length(myhostname()),
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
		Mnum = list_to_integer(M),
		{ok, Max} = yxa_config:get_env(max_max_forwards),
		%% decrease the old value by one, but assure it is not greater than Max
		lists:min([Max, Mnum - 1]);
	    [] ->
		{ok, MF} = yxa_config:get_env(default_max_forwards),
		MF
	end,
    if
	MaxForwards < 1 ->
	    logger:log(debug, "Siprequest: Not proxying request with Max-Forwards < 1"),
	    throw({siperror, 483, "Too Many Hops"});
	true ->
	    keylist:set("Max-Forwards", [integer_to_list(MaxForwards)], Header)
    end.

%%--------------------------------------------------------------------
%% Function: proxy_add_via(Header, Method, OrigURI, Parameters, Proto)
%%           Header      = keylist record()
%%           OrigURI     = sipurl record()
%%           Parameters  = string()
%%           Proto       = atom(), tcp|tcp6|udp|udp6|tls|tls6
%% Descrip.: Generate a Via header for this proxy and add it to Header
%% Returns : NewHeader, keylist record()
%%
%% XXX rework to only calculate LoopCookie if we are actually going to
%% use it.
%%--------------------------------------------------------------------
proxy_add_via(Header, OrigURI, Parameters, Proto) when is_record(Header, keylist), is_record(OrigURI, sipurl),
						       is_list(Parameters), is_atom(Proto) ->
    ParamDict = sipheader:param_to_dict(Parameters),

    %% Make sure we have a 'branch' parameter, and that it contains a loop cookie
    %% (already present, or added here) unless we are configured to not do loop detection
    ViaParameters1 =
	case dict:find("branch", ParamDict) of
	    error ->
		LoopCookie = proxy_add_via_get_loopcookie(Header, OrigURI, Proto),
		add_stateless_generated_branch(Header, OrigURI, LoopCookie, Parameters);
	    {ok, Branch1} ->
		Branch = lists:flatten(Branch1),
		case branch_contains_loopcookie(Branch) of
		    true ->
			logger:log(debug, "Siprequest (transport layer) : NOT adding loop cookie to branch "
				   "that already contains a loop cookie : ~p", [Branch]),
			Parameters;
		    false ->
			LoopCookie = proxy_add_via_get_loopcookie(Header, OrigURI, Proto),
			add_loopcookie_to_branch(LoopCookie, Branch, Parameters, ParamDict)
		end
	end,

    %% Add 'rport' parameter to Via headers of outgoing request, if configured to
    ViaParameters =
	case yxa_config:get_env(request_rport) of
	    {ok, true} ->
		lists:append(ViaParameters1, ["rport"]);
	    {ok, false} ->
		ViaParameters1
	end,

    V = create_via(Proto, ViaParameters),
    MyVia = sipheader:via_print([V]),
    keylist:prepend({"Via", MyVia}, Header).

%% part of proxy_add_via/4. Returns : LoopCookie = string() | none
proxy_add_via_get_loopcookie(Header, OrigURI, Proto) ->
    case yxa_config:get_env(detect_loops) of
	{ok, true} ->
	    get_loop_cookie(Header, OrigURI, Proto);
	{ok, false} ->
	    none
    end.

%% part of proxy_add_via/4.
%% Check if there already is a loop cookie in this branch. Necessary to not
%% get the wrong branch in constructed ACK of non-2xx response to INVITE, and
%% CANCEL requests we generate.
%% Returns : true | false
branch_contains_loopcookie(Branch) when is_list(Branch) ->
    case Branch of
	"z9hG4bK-yxa-" ++ RestOfBranch ->
	    case string:rstr(RestOfBranch, "-o") of
		0 ->
		    false;
		Index when is_integer(Index) ->
		    true
	    end;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: add_loopcookie_to_branch(LoopCookie, Branch, Parameters,
%%                                    ParamDict)
%%           LoopCookie  = string() | none
%%           Branch      = string()
%%           Parameters  = list() of string()
%%           ParamDict   = dict()
%% Descrip.: If LoopCookie is not 'none', add it to Branch. Return
%%           a new Parameters construct.
%% Returns : NewParameters, list() of string()
%%--------------------------------------------------------------------
add_loopcookie_to_branch(none, _Branch, Parameters, _ParamDict) ->
    %% Request has a branch, and loop detection is off
    Parameters;
add_loopcookie_to_branch(LoopCookie, Branch, _Parameters, ParamDict) ->
    NewBranch = lists:concat([Branch, "-o", LoopCookie]),
    Param2 = dict:store("branch", NewBranch, ParamDict),
    sipheader:dict_to_param(Param2).


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
add_stateless_generated_branch(Header, OrigURI, LoopCookie, Parameters) ->
    add_stateless_generated_branch2(Header, OrigURI, LoopCookie, Parameters, node()).

add_stateless_generated_branch2(Header, OrigURI, LoopCookie, Parameters, Node)
  when is_record(Header, keylist), is_record(OrigURI, sipurl), is_list(LoopCookie); LoopCookie == none,
       is_list(Parameters) ->
    case stateless_generate_branch(OrigURI, Header, Node) of
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
%% Function: stateless_generate_branch(OrigURI, Header, Node)
%%           OrigURI = sipurl record(), the original requests URI
%%           Header  = keylist record()
%%           Node    = atom(), name of node (to make this code
%%                                           testable)
%% Descrip.: Generate a branch suitable for stateless proxying of a
%%           request (meaning that we make sure that we generate the
%%           very same branch for a retransmission of the very same
%%           request). This is specified in RFC3261 #16.11.
%% Returns : {ok, Branch} |
%%           error
%%           Branch = string()
%%--------------------------------------------------------------------
stateless_generate_branch(OrigURI, Header, Nodename) ->
    case sipheader:topvia(Header) of
	TopVia when is_record(TopVia, via) ->
	    case sipheader:get_via_branch(TopVia) of
		"z9hG4bK" ++ RestOfBranch ->
		    %% The previous hop has put a RFC3261 branch in it's Via. Use that,
		    %% togehter with our nodename as branch.
		    In = lists:concat([Nodename, "-rbranch-", RestOfBranch]),
		    {ok, "z9hG4bK-yxa-" ++ make_base64_md5_token(In)};
		_ ->
		    %% No branch, or non-RFC3261 branch. XXX RFC32611 #16.11 suggests
		    %% to include the Top-Via in the hash we generte here. We don't,
		    %% but perhaps there was a reason. It was a long time since we were
		    %% stateless, so I won't change that now - ft 2005-03-22. Revisit
		    %% this question if/when we have a use case.
		    OrigURIstr = sipurl:print(OrigURI),
		    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
		    ToTag = sipheader:get_tag(keylist:fetch('to', Header)),
		    CallId = keylist:fetch('call-id', Header),
		    {CSeqNum, _} = sipheader:cseq(keylist:fetch('cseq', Header)),
		    In = lists:concat([Nodename, "-uri-", OrigURIstr, "-ftag-", FromTag, "-totag-", ToTag,
				       "-callid-", CallId, "-cseqnum-", CSeqNum]),
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
    MD5 = binary_to_list(erlang:md5(In)),
    %% remove the trailing == from the result of encode_base64()
    Out = string:strip(httpd_util:encode_base64(MD5), right, $=),
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
%% Descrip.: Turn Request-Route header from a response into Route-
%%           header to include in another request in the same dialog.
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
make_answerheader(Header) ->
    case keylist:fetch('record-route', Header) of
	[] ->
	    Header;
	RecordRoute ->
	    NewHeader = keylist:set("Route", RecordRoute, Header),
	    keylist:delete('record-route', NewHeader)
    end.

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
				       P = sipsocket:viastr2proto(TopVia#via.proto),
				       {TopVia#via.host, sipsocket:default_port(P, TopVia#via.port)};
				   _ ->
				       {myhostname(), sipsocket:get_listenport(Proto)}
			       end,
    TopViaSentBy = sipurl:print_hostport(TopViaHost, TopViaPort),
    In = lists:concat([OrigURIstr, "-ftag-", FromTag, "-totag-", ToTag,
		       "-callid-", CallId, "-cseqnum-", CSeqNum,
		       "-preq-", ProxyReq, "-pauth-", ProxyAuth,
		       "-route-", Route, "-topvia-", TopViaSentBy]),
    Out = make_base64_md5_token(In),
    logger:log(debug, "Siprequest: Created loop cookie ~p from input :~n'~s'", [Out, In]),
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
    case yxa_config:get_env(myhostnames) of
	{ok, [FirstHostname | _]} when is_list(FirstHostname) ->
	    FirstHostname;
	none ->
	    siphost:myip()
    end.

%%--------------------------------------------------------------------
%% Function: create_via(Proto, Parameters)
%%           Proto      = term()
%%           Parameters = list() of string()
%% Descrip.: Create a Via for this proxy, given the protocol.
%% Returns : Via = via record()
%%--------------------------------------------------------------------
create_via(Proto, Parameters) ->
    Hostname = myhostname(),
    Port = sipsocket:get_listenport(Proto),
    ViaProto = sipsocket:proto2viastr(Proto),
    #via{proto=ViaProto, host=Hostname, port=Port, param=Parameters}.

%%--------------------------------------------------------------------
%% Function: add_record_route(Proto, Hostname, Port, Header)
%%           Proto      = "sips" | "sip" | string()
%%           Hostname   = string()
%%           Port       = integer() | none
%%           Header     = keylist record()
%% Descrip.: Prepend a Record-Route header for Proto:Host:Port to the
%%           Header keylist.
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
add_record_route(Proto, Hostname, Port, Header) when is_list(Proto), is_list(Hostname), is_integer(Port);
						     Port == none ->
    Param1 = ["maddr=" ++ siphost:myip(), "lr=true"],
    RR_Elems =
	case Port of
	    Port when is_integer(Port) ->
		%% Set port unless explicitly told not to. This is to minimize the chances
		%% of problems with wrong port number in NAPTR/SRV records for MyHostname
		%% etc. (ACK of 200 Ok response to INVITE is particularly vulnerable to this
		%% kind of problem since it is always forwarded completely without state).
		[{proto, Proto}, {host, Hostname}, {port, Port}, {param, Param1}];
	    none ->
		%% It was requested that we don't include a port number.
		[{proto, Proto}, {host, Hostname}, {param, Param1}]
	end,
    RouteStr = sipurl:print( sipurl:new(RR_Elems) ),

    %% Check if our Record-Route (RouteStr) is already present as the first entry of
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
%% Function: add_record_route(URL, Header)
%%           URL    = sipurl record(), original Request-URI
%%           Header = keylist record()
%% Descrip.: Prepend a Record-Route header for this proxy, based on
%%           the original Request-URI, to Header.
%% Returns : NewHeader = keylist record()
%%--------------------------------------------------------------------
add_record_route(URL, Header) when is_record(URL, sipurl) ->
    {Port, Proto} =
	case URL#sipurl.proto of
	    "sips" ->
		{sipsocket:get_listenport(tls), "sips"};
	    _ ->
		%% Create sip: Record-Route for everything except SIPS-URIs
		{sipsocket:get_listenport(udp), "sip"}
	end,
    add_record_route(Proto, myhostname(), Port, Header).

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
%%           Body      = binary() | string()
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
    %% Extract proto from SipSocket
    make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, SipSocket#sipsocket.proto, Request);
make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, Proto, Request) when is_list(Body) ->
    %% Turn list body into binary
    BinBody = list_to_binary(Body),
    make_response(Status, Reason, BinBody, ExtraHeaders, ViaParameters, Proto, Request);
make_response(Status, Reason, Body, ExtraHeaders, ViaParameters, Proto, Request)
  when is_integer(Status), is_list(Reason), is_binary(Body), is_list(ExtraHeaders),
       is_list(ViaParameters), is_atom(Proto), is_record(Request, request) ->
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
    AnswerHeader3 = keylist:prepend({"Via", PlaceHolderVia}, AnswerHeader1),
    %% If there is no body, remove the Content-Type we copied above
    AnswerHeader4 = case size(Body) of
			0 -> keylist:delete('content-type', AnswerHeader3);
			_ -> AnswerHeader3
		    end,
    %% If this is not a 100 Trying response, remove the Timestamp we copied above.
    %% The preservation of Timestamp headers into 100 Trying response is mandated by RFC 3261 8.2.6.1
    AnswerHeader5 = case Status of
			100 -> AnswerHeader4;
			_ -> keylist:delete("Timestamp", AnswerHeader4)
		    end,
    set_response_body(#response{status=Status, reason=Reason, header=AnswerHeader5}, Body).

%%--------------------------------------------------------------------
%% Function: binary_make_message(BinLine1, Header, BinBody)
%%           BinLine1 = binary(), the top line of the request/response
%%           Header   = keylist record()
%%           BinBody  = binary(), body of request/response
%% Descrip.: Create a binary representing the request/response, ready
%%           to be sent out on the wire.
%% Returns : Msg = binary()
%%--------------------------------------------------------------------
binary_make_message(BinLine1, Header, BinBody) when is_binary(BinLine1), is_record(Header, keylist),
						    is_binary(BinBody) ->
    BinHeaders = sipheader:build_header_binary(Header),
    list_to_binary([BinLine1, 13, 10, BinHeaders, 13, 10, BinBody]).

%%--------------------------------------------------------------------
%% Function: set_request_body(Request, Body)
%%           Request = request record()
%%           Body    = list() or binary()
%% Descrip.: Set the body of a request record.
%% Returns : NewRequest = request record()
%%--------------------------------------------------------------------
set_request_body(Request, Body) when is_record(Request, request), is_binary(Body) ->
    ILen = size(Body),
    Len = integer_to_list(ILen),
    NewHeader = keylist:set("Content-Length", [Len], Request#request.header),
    Request#request{header=NewHeader, body=Body};
set_request_body(Request, Body) when is_record(Request, request), is_list(Body) ->
    set_request_body(Request, list_to_binary(Body)).

%%--------------------------------------------------------------------
%% Function: set_response_body(Request, Body)
%%           Request = request record()
%%           Body    = list() or binary()
%% Descrip.: Set the body of a request record, and calculate
%%           Content-Length.
%% Returns : NewRequest = request record()
%%--------------------------------------------------------------------
set_response_body(Response, Body) when is_record(Response, response), is_binary(Body) ->
    ILen = size(Body),
    Len = integer_to_list(ILen),
    NewHeader = keylist:set("Content-Length", [Len], Response#response.header),
    Response#response{header=NewHeader, body=Body};
set_response_body(Response, Body) when is_record(Response, response), is_list(Body) ->
    set_response_body(Response, list_to_binary(Body)).


%%====================================================================
%% Test functions
%%====================================================================

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


    %% generate_branch()
    %%--------------------------------------------------------------------
    %% test that generate_branch gives us a RFC3261 branch
    io:format("test: generate_branch/0 - 1~n"),
    Branch = generate_branch(),
    "z9hG4bK-yxa-" = string:substr(Branch, 1, 12),


    %% make_3261_token(In)
    %%--------------------------------------------------------------------
    %% test that our make_3261_token produces the expected results
    io:format("test: make_3261_token/1 - 1~n"),
    "abcdeXYZ-.!%*_+`'~123_" = make_3261_token("abcdeXYZ-.!%*_+`'~123@"),

    io:format("test: make_base64_md5_token/1 - 1~n"),
    "Wd_CG1j2CfuAv0NWdJLxTQ" = make_base64_md5_token("abcdeXYZ123-.!%*_+`'~123@"),


    %% create_via(Proto, Parameters)
    %%--------------------------------------------------------------------
    SipLPort  = sipsocket:get_listenport(tcp),
    SipsLPort = sipsocket:get_listenport(tls),

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


    %% add_record_route(URL, Header)
    %% and implicitly add_record_route/4
    %%--------------------------------------------------------------------
    io:format("test: add_record_route/2 - 1~n"),
    EmptyHeader = keylist:from_list([]),
    MyIP = siphost:myip(),

    RRHeader1 = add_record_route(sipurl:parse("sip:ft@one.example.org;transport=tcp"), EmptyHeader),

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
    %% we should never set transport
    [] = url_param:find(RRParam1, "transport"),

    %% contact.contact_param
    io:format("test: add_record_route/2 - 5~n"),
    EmptyContactParam = contact_param:to_norm([]),
    EmptyContactParam = RRoute1#contact.contact_param,

    %% Already existing RR, and test SIPS protocol as well
    io:format("test: add_record_route/2 - 6.1~n"),
    RRHeader2 = add_record_route(sipurl:parse("sips:ft@two.example.org;transport=tls"), RRHeader1),
    %% get our RRoute2 and also check that RRoute1 is still there
    io:format("test: add_record_route/2 - 6.2~n"),
    [RRoute2, RRoute1] = sipheader:record_route(RRHeader2),

    %% contact.urlstr
    io:format("test: add_record_route/2 - 6.3~n"),
    #sipurl{proto="sips", host=MyHostname, port=SipsLPort, param_pairs=RRParam2} =
	sipurl:parse(RRoute2#contact.urlstr),

    %% contact.urlstr sipurl parameters
    io:format("test: add_record_route/2 - 6.4~n"),
    [MyIP] = url_param:find(RRParam2, "maddr"),
    ["true"] = url_param:find(RRParam2, "lr"),
    %% transport=tls is deprecated
    [] = url_param:find(RRParam2, "transport"),

    %% check that add_record_route/2 does not add duplicate Record-Route
    io:format("test: add_record_route/2 - 7~n"),
    RRHeader3 = add_record_route(sipurl:parse("sips:ft@two.example.org;transport=tls"), RRHeader2),

    io:format("test: add_record_route/2 - 8~n"),
    [RRoute2, RRoute1] = sipheader:record_route(RRHeader3),

    io:format("test: add_record_route/2 - 9.1~n"),
    %% check that we don't care about unknown transport=
    RRHeader9 = add_record_route(sipurl:parse("sip:ft@foo.example.org;transport=foo"), EmptyHeader),

    io:format("test: add_record_route/2 - 9.2~n"),
    [RRoute9] = sipheader:record_route(RRHeader9),
    RRoute9_URL = sipurl:parse(RRoute9#contact.urlstr),
    %% we should never set transport
    [] = url_param:find(RRoute9_URL#sipurl.param_pairs, "transport"),


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

    io:format("test: make_answerheader/1 - 2~n"),
    %% Check that the Record-Route was deleted
    [] = keylist:fetch('record-route', AHeader1),

    io:format("test: make_answerheader/1 - 3~n"),
    %% Test without a Record-Route header
    AHeader2 = keylist:delete('record-route', ReqHeader),
    AHeader2 = make_answerheader(AHeader2),


    %% get_loop_cookie(ReqHeader, URI, Proto
    %%--------------------------------------------------------------------
    io:format("test: get_loop_cookie/3 - 1~n"),
    "JYJPsr4IqjymexLGUB58yg" = get_loop_cookie(ReqHeader, sipurl:parse("sip:test@example.org"), tcp),


    %% make_response(Status, Reason, Body, ExtraHeaders, Parameters, Proto, Request)
    %%--------------------------------------------------------------------
    io:format("test: make_response/7 - 1.1~n"),
    Response1 = make_response(100, "Trying", "test", [], [], tcp, #request{header=ReqHeader}),

    %% basic response record check
    io:format("test: make_response/7 - 1.2~n"),
    #response{status=100, reason="Trying", body = <<"test">>} = Response1,

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
    Response2 = make_response(486, "_BUSY_", <<>>, [{"Extra-Header", ["test"]}], ["test=true"],
			      tls6, #request{header=ReqHeader}),

    %% basic response record check
    io:format("test: make_response/7 - 2.2~n"),
    #response{status=486, reason="_BUSY_", body = <<>>} = Response2,

    %% Timestamp NOT present in 486 Busy Here
    io:format("test: make_response/7 - 2.3~n"),
    [] = keylist:fetch("Timestamp", Response2#response.header),

    %% Content-Type NOT present in 486 Busy Here with empty body
    io:format("test: make_response/7 - 2.4~n"),
    [] = keylist:fetch("Content-Type", Response2#response.header),

    %% Extra-Header supplied added correctly
    io:format("test: make_response/7 - 2.5~n"),
    ["test"] = keylist:fetch("Extra-Header", Response2#response.header),

    %% Make sure Record-Route is NOT turned into Route (we used to do that, but it was wrong)
    io:format("test: make_response/7 - 2.6.1~n"),
    ["<sip:p1:1111>", "<sip:p2:2222>"] = keylist:fetch('record-route', Response2#response.header),

    %% (make sure Route is still empty)
    io:format("test: make_response/7 - 2.6.2~n"),
    [] = keylist:fetch('route', Response2#response.header),


    %% check_valid_proxy_request(Method, Header)
    %%--------------------------------------------------------------------
    CVPR_Header = keylist:from_list([{"Proxy-Require", ["fooextension"]}]),

    io:format("test: check_valid_proxy_request/2 - 1~n"),
    %% verify that we don't reject ACK even if there is a Proxy-Require in there
    true = check_valid_proxy_request("ACK", CVPR_Header),

    io:format("test: check_valid_proxy_request/2 - 2~n"),
    %% verify that we don't reject CANCEL even if there is a Proxy-Require in there
    true = check_valid_proxy_request("CANCEL", CVPR_Header),

    io:format("test: check_valid_proxy_request/2 - 3~n"),
    %% verify that we reject everything else that has a Proxy-Require (since we don't
    %% support any extensions)
    {siperror, 420, "Bad Extension", _EH} = (catch check_valid_proxy_request("OPTIONS", CVPR_Header)),


    %% check_proxy_request(Request)
    %%--------------------------------------------------------------------
    io:format("test: check_proxy_request/1 - 1~n"),
    ReqHeader2 = keylist:set("Content-Length", ["900"], ReqHeader),
    Req2 = #request{method="MESSAGE", uri=sipurl:parse("sip:test@example.org"), header=ReqHeader2, body = <<"foo">>},

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
    RecordRouteLen = length(lists:flatten( keylist:fetch('record-route', ReqHeader2_2) )),
    case (ApproxMsgSize2_2 >= 500 + RecordRouteLen) and (ApproxMsgSize2_2 =< 600 + RecordRouteLen) of
	true ->
	    ok;
	false ->
	    erlang:fault("approximate message size not within bounds", [ApproxMsgSize2_2])
    end,

    %% check that we don't accept unknown Proxy-Require
    io:format("test: check_proxy_request/1 - 6~n"),
    ReqHeader3 = keylist:set("Proxy-Require", ["CantBeSupported"], ReqHeader2),
    Req3 = Req2#request{method="INVITE", header=ReqHeader3, body = <<>>},
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


    %% branch_contains_loopcookie(Branch)
    %%--------------------------------------------------------------------
    io:format("test: branch_contains_loopcookie/1 - 1~n"),
    %% contains Yxa loop cookie
    true = branch_contains_loopcookie("z9hG4bK-yxa-foo-oLOOPCOOKIE"),

    io:format("test: branch_contains_loopcookie/1 - 2~n"),
    %% contains -oSOMETHING, but does not begin with "z9hG4bK-yxa"
    false = branch_contains_loopcookie("z9hG4bK-foo-oLOOPCOOKIE"),

    io:format("test: branch_contains_loopcookie/1 - 3~n"),
    %% begins with "z9hG4bK-yxa", but has no loop cookie
    false = branch_contains_loopcookie("z9hG4bK-yxa-foo"),


    %% stateless_generate_branch(OrigURI, Header, Nodename)
    %%--------------------------------------------------------------------
    io:format("test: stateless_generate_branch/3 - 0~n"),
    SGB_URL1 = sipurl:parse("sip:ft@example.org"),

    io:format("test: stateless_generate_branch/3 - 1~n"),
    %% test normal case
    {ok, "z9hG4bK-yxa-S1Xsdm0n23TBxJnAMqP3ww"} =
	stateless_generate_branch(SGB_URL1, ReqHeader, "test-nodename"),

    io:format("test: stateless_generate_branch/3 - 2~n"),
    SGB_Header2 = keylist:set("Via", ["SIP/2.0/TCP example.org;branch=z9hG4bK-test"], ReqHeader),
    {ok, "z9hG4bK-yxa-WGvVbdiXU1kKvQYwWKmgcg"} =
	stateless_generate_branch(SGB_URL1, SGB_Header2, "test-nodename"),

    io:format("test: stateless_generate_branch/3 - 3~n"),
    %% Test without Via header
    error = stateless_generate_branch(SGB_URL1, keylist:delete('via', ReqHeader), "test-nodename"),


    %% add_loopcookie_to_branch(LoopCookie, Branch, Parameters, ParamDict)
    %%--------------------------------------------------------------------
    io:format("test: add_loopcookie_to_branch/4 - 1~n"),
    %% No loop cookie, transparently returns Parameters
    ["foo=Bar"] = add_loopcookie_to_branch(none, "foo", ["foo=Bar"], sipheader:param_to_dict([])),

    io:format("test: add_loopcookie_to_branch/4 - 2~n"),
    %% Normal case
    ["branch=x-oLOOP"] = add_loopcookie_to_branch("LOOP", "x", ["branch=x"],
						  sipheader:param_to_dict(["branch=x"])),


    %% add_stateless_generated_branch2(Header, OrigURI, LoopCookie, Parameters, Node)
    %%--------------------------------------------------------------------
    io:format("test: add_stateless_generated_branch2/5 - 1~n"),
    {siperror, 500, _} =
	(catch add_stateless_generated_branch2( keylist:delete('via', ReqHeader),
					        sipurl:parse("sip:ft@example.org"),
					        "LOOPCOOKIE", ["branch=x"], 'test-nodename'
					       )),

    io:format("test: add_stateless_generated_branch2/5 - 2~n"),
    ASGB_Header1 = keylist:set("Via", ["SIP/2.0/TCP example.org;branch=z9hG4bK-test"], ReqHeader),
    ["branch=z9hG4bK-yxa-WGvVbdiXU1kKvQYwWKmgcg-oLOOPCOOKIE"] =
	add_stateless_generated_branch2( ASGB_Header1,
					 sipurl:parse("sip:ft@example.org"),
					 "LOOPCOOKIE", [], 'test-nodename'
					),

    io:format("test: add_stateless_generated_branch2/5 - 3~n"),
    ASGB_Header1 = keylist:set("Via", ["SIP/2.0/TCP example.org;branch=z9hG4bK-test"], ReqHeader),
    ["branch=z9hG4bK-yxa-WGvVbdiXU1kKvQYwWKmgcg"] =
	add_stateless_generated_branch2( ASGB_Header1,
					 sipurl:parse("sip:ft@example.org"),
					 none, [], 'test-nodename'
					),


    %% proxy_add_via(Header, URI, Parameters, Proto)
    %%--------------------------------------------------------------------
    io:format("test: proxy_add_via/1 - 0~n"),
    PAV_Header = keylist:delete("Via", ReqHeader),

    %% test proxy_add_via/1 with no present Via header
    io:format("test: proxy_add_via/1 - 1.2~n"),
    PAVheader1 = proxy_add_via(PAV_Header, InURI,
			       ["branch=z9hG4bK-test"], tcp6),
    PAV_TopVia1 = sipheader:topvia(PAVheader1),

    %% get branch from added Via, branch computated depends on your hostname
    %% so we can't check for a static one
    io:format("test: proxy_add_via/1 - 1.3~n"),
    Branch1 = sipheader:get_via_branch_full(PAV_TopVia1),

    %% basic check the top via that was added
    io:format("test: proxy_add_via/1 - 1.4~n"),
    #via{proto="SIP/2.0/TCP", host=MyHostname, port=SipLPort,
	 param=["branch=" ++ Branch1]} = PAV_TopVia1,


    io:format("test: proxy_add_via/1 - 2.1~n"),
    PAV_HeaderIn2 = keylist:set("Via", sipheader:via_print([TLSBasicVia]), PAV_Header),

    %% test proxy_add_via/1 with an existing Via header
    io:format("test: proxy_add_via/1 - 2.2~n"),
    PAV_Header2 = proxy_add_via(PAV_HeaderIn2, InURI,
				["branch=z9hG4bK-test"], tcp6),

    %% check the two via's that should be present in PAVheader2
    io:format("test: proxy_add_via/1 - 2.3~n"),
    Branch2 = sipheader:get_via_branch_full(sipheader:topvia(PAV_Header2)),
    PAV_TopVia1_NewLoopCookie = PAV_TopVia1#via{param=["branch=" ++ Branch2]},
    [PAV_TopVia1_NewLoopCookie, TLSBasicVia] = sipheader:via(PAV_Header2),


    %% test with a branch that already contains a loop cookie
    io:format("test: proxy_add_via/1 - 3.1~n"),
    PAV_Header3 = proxy_add_via(PAV_Header, InURI,
				["branch=z9hG4bK-yxa-foo-oLOOPCOOKIE"], tcp6),
    PAV_TopVia3 = sipheader:topvia(PAV_Header3),

    %% get branch from added Via, branch computated depends on your hostname
    %% so we can't check for a static one
    io:format("test: proxy_add_via/1 - 3.2~n"),
    "z9hG4bK-yxa-foo-oLOOPCOOKIE" = sipheader:get_via_branch_full(PAV_TopVia3),


    io:format("test: proxy_add_via/1 - 4.1~n"),
    PAV_HeaderIn4 = keylist:set("Via", sipheader:via_print([TLSBasicVia]), PAV_Header),

    io:format("test: proxy_add_via/1 - 4.2~n"),
    %% test without a supplied branch - a stateless branch should be added
    PAV_Header4 = proxy_add_via(PAV_HeaderIn4, InURI, [], udp),
    PAV_TopVia4 = sipheader:topvia(PAV_Header4),

    %% get branch from added Via, branch computated depends on your hostname
    %% so we can't check for a static one
    io:format("test: proxy_add_via/1 - 4.3~n"),
    "z9hG4bK-yxa-" ++ PAV_Branch4 = sipheader:get_via_branch_full(PAV_TopVia4),

    %% Make sure there is a loop cookie in the stateless branch that was added
    io:format("test: proxy_add_via/1 - 4.4~n"),
    true = branch_contains_loopcookie("z9hG4bK-yxa-" ++ PAV_Branch4),


    %% test proxy_check_maxforwards(Header)
    %%--------------------------------------------------------------------
    io:format("test: proxy_check_maxforwards/1 - 1~n"),
    %% check that we add the default specified in RFC3261 to header that has no Max-Forwards
    MaxFwd_H1 = proxy_check_maxforwards(keylist:from_list([])),
    ["70"] = keylist:fetch('max-forwards', MaxFwd_H1),

    io:format("test: proxy_check_maxforwards/1 - 2~n"),
    %% check that we decrease Max-Forwards by one (normal case)
    MaxFwd_H2 = proxy_check_maxforwards(MaxFwd_H1),
    ["69"] = keylist:fetch('max-forwards', MaxFwd_H2),

    io:format("test: proxy_check_maxforwards/1 - 3~n"),
    %% check that we don't allow overly large Max-Forwards
    MaxFwd_H3_in = keylist:set("Max-Forwards", ["500"], MaxFwd_H1),
    MaxFwd_H3 = proxy_check_maxforwards(MaxFwd_H3_in),
    ["255"] = keylist:fetch('max-forwards', MaxFwd_H3),

    io:format("test: proxy_check_maxforwards/1 - 4~n"),
    %% check that we don't refuse Max-Forwards: 2
    MaxFwd_H4_in = keylist:set("Max-Forwards", ["2"], MaxFwd_H1),
    MaxFwd_H4 = proxy_check_maxforwards(MaxFwd_H4_in),
    ["1"] = keylist:fetch('max-forwards', MaxFwd_H4),

    io:format("test: proxy_check_maxforwards/1 - 5~n"),
    %% check that we don't reach 0
    {siperror, 483, _} = (catch proxy_check_maxforwards(MaxFwd_H4)),

    io:format("test: proxy_check_maxforwards/1 - 6~n"),
    %% check that we don't allow 0
    MaxFwd_H6 = keylist:set("Max-Forwards", ["0"], MaxFwd_H1),
    {siperror, 483, _} = (catch proxy_check_maxforwards(MaxFwd_H6)),

    io:format("test: proxy_check_maxforwards/1 - 7~n"),
    %% check that we don't allow negative numbers
    MaxFwd_H7 = keylist:set("Max-Forwards", ["-1"], MaxFwd_H1),
    {siperror, 483, _} = (catch proxy_check_maxforwards(MaxFwd_H7)),


    %% standardcopy(Header, ExtraHeaders)
    %%--------------------------------------------------------------------
    io:format("test: standardcopy/2 - 1.1~n"),
    SC_H1 = standardcopy(ReqHeader, []),

    %% verify results
    io:format("test: standardcopy/2 - 1.2~n"),
    ["SIP/2.0/TLS 130.237.90.1:111", "SIP/2.0/TCP 2001:6b0:5:987::1"] = keylist:fetch('via', SC_H1),
    ["<sip:test@it.su.se>;tag=f-123"] = keylist:fetch('from', SC_H1),
    ["<sip:test@it.su.se>;tag=t-123"] = keylist:fetch('to', SC_H1),
    ["abc123@test"] = keylist:fetch('call-id', SC_H1),
    ["4711 INVITE"] = keylist:fetch('cseq', SC_H1),

    io:format("test: standardcopy/2 - 1.3~n"),
    SC_H1_1 = keylist:delete('via',	SC_H1),
    SC_H1_2 = keylist:delete('from',	SC_H1_1),
    SC_H1_3 = keylist:delete('to',	SC_H1_2),
    SC_H1_4 = keylist:delete('call-id',	SC_H1_3),
    SC_H1_5 = keylist:delete('cseq',	SC_H1_4),
    %% verify that no more headers than the ones we expected were copied
    SC_H1_5 = keylist:from_list([]),

    io:format("test: standardcopy/2 - 2.1~n"),
    %% test standardcopy with one extra header
    SC_H2_ExtraHeader = [{"Subject", keylist:fetch("subject", ReqHeader)}],
    SC_H2 = standardcopy(ReqHeader, SC_H2_ExtraHeader),

    %% verify results
    io:format("test: standardcopy/2 - 2.2~n"),
    ["test subject short form"] = keylist:fetch("subject", SC_H2),

    io:format("test: standardcopy/2 - 2.3~n"),
    SC_H2_1 = keylist:delete("subject", SC_H2),
    %% verify that no more headers than the ones we expected were copied
    SC_H2_1 = SC_H1,


    %% set_request_body(Request, Body)
    %%--------------------------------------------------------------------
    SReqB_Header1 = keylist:from_list([{"Content-Length", ["1"]}]),
    SReqB_Header2 = keylist:from_list([]),

    io:format("test: set_request_body/2 - 1.1~n"),
    %% set a binary body
    #request{header=SReqB_Header1_1, body = <<"test">>} =
	set_request_body(#request{header = SReqB_Header1, body = <<>>}, <<"test">>),

    io:format("test: set_request_body/2 - 1.2~n"),
    %% verify that Content-Length was set correctly
    ["4"] = keylist:fetch('content-length', SReqB_Header1_1),

    io:format("test: set_request_body/2 - 2.1~n"),
    %% set a list body - list bodies are kept for backwards compatibility
    #request{header=SReqB_Header2_1, body = <<"test">>} =
	set_request_body(#request{header = SReqB_Header2, body = <<>>}, "test"),

    io:format("test: set_request_body/2 - 2.2~n"),
    %% verify that Content-Length was set correctly
    ["4"] = keylist:fetch('content-length', SReqB_Header2_1),

    io:format("test: set_request_body/2 - 3.1~n"),
    %% delete body
    #request{header=SReqB_Header3_1, body = <<>>} =
	set_request_body(#request{header = SReqB_Header1, body = <<"test">>}, <<>>),

    io:format("test: set_request_body/2 - 2.2~n"),
    %% verify that Content-Length was set correctly
    ["0"] = keylist:fetch('content-length', SReqB_Header3_1),


    %% set_response_body(Request, Body)
    %%--------------------------------------------------------------------
    SResB_Header1 = keylist:from_list([{"Content-Length", ["1"]}]),
    SResB_Header2 = keylist:from_list([]),

    io:format("test: set_response_body/2 - 1.1~n"),
    %% set a binary body
    #response{header=SResB_Header1_1, body = <<"test">>} =
	set_response_body(#response{header = SResB_Header1, body = <<>>}, <<"test">>),

    io:format("test: set_response_body/2 - 1.2~n"),
    %% verify that Content-Length was set correctly
    ["4"] = keylist:fetch('content-length', SResB_Header1_1),

    io:format("test: set_response_body/2 - 2.1~n"),
    %% set a list body - list bodies are kept for backwards compatibility
    #response{header=SResB_Header2_1, body = <<"test">>} =
	set_response_body(#response{header = SResB_Header2, body = <<>>}, "test"),

    io:format("test: set_response_body/2 - 2.2~n"),
    %% verify that Content-Length was set correctly
    ["4"] = keylist:fetch('content-length', SResB_Header2_1),

    io:format("test: set_response_body/2 - 3.1~n"),
    %% delete body
    #response{header=SResB_Header3_1, body = <<>>} =
	set_response_body(#response{header = SResB_Header1, body = <<"test">>}, <<>>),

    io:format("test: set_response_body/2 - 2.2~n"),
    %% verify that Content-Length was set correctly
    ["0"] = keylist:fetch('content-length', SResB_Header3_1),


    %% fix_content_length(Header, Body)
    %%--------------------------------------------------------------------
    io:format("test: fix_content_length/2 - 1~n"),
    %% binary body, no Content-Length header
    ["4"] = keylist:fetch('content-length', fix_content_length(
					      keylist:from_list([]),
					      <<"test">>)
			 ),

    io:format("test: fix_content_length/2 - 2~n"),
    %% list body, incorrect Content-Length header
    ["4"] = keylist:fetch('content-length', fix_content_length(
					      keylist:from_list([{"Content-Length", ["600"]}]),
					      "test")
			 ),

    io:format("test: fix_content_length/2 - 3~n"),
    %% delete body, incorrect Content-Length header
    ["0"] = keylist:fetch('content-length', fix_content_length(
					      keylist:from_list([{"Content-Length", ["600"]}]),
					      <<>>)
					     ),


    %% proxyauth_without_response(Header)
    %%--------------------------------------------------------------------
    io:format("test: proxyauth_without_response/1 - 1~n"),
    PAWR_Str1 =
	"Digest username=\"ft\", realm=\"su.se\", uri=\"sip:example.org\", "
	"response=\"test-response\", nonce=\"test-nonce\", opaque=\"test-opaque\", "
	"algorithm=md5",

    ["algorithm=md5", "realm=su.se", "uri=sip:example.org", "username=ft"] =
	lists:sort( proxyauth_without_response(
		      keylist:from_list([{"Proxy-Authorization", [PAWR_Str1]}])
		     )),

    io:format("test: proxyauth_without_response/1 - 2~n"),
    none = proxyauth_without_response( keylist:from_list([]) ),

    ok.
