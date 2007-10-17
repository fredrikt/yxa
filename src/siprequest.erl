%%%-------------------------------------------------------------------
%%% File    : siprequest.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Various functions related to SIP requests.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
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
	 construct_record_route/1,
	 construct_record_route/3,
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
	 send_notfound/2,
	 request_to_me/3
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
%% @spec    (Header, Body) ->
%%            NewHeader
%%
%%            Header = #keylist{}
%%            Body   = string()
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Before sending out requests/responses, make sure the
%%          Content-Length is correct (don't trust the length
%%          computed by some previous hop). This is partly for
%%          backwards compliance with RFC2543 UAC's who were not
%%          required to include a Content-Length.
%% @end
%%--------------------------------------------------------------------
fix_content_length(Header, Body) when is_record(Header, keylist), is_list(Body) ->
    keylist:set("Content-Length", [integer_to_list(length(Body))], Header);
fix_content_length(Header, Body) when is_record(Header, keylist), is_binary(Body) ->
    keylist:set("Content-Length", [integer_to_list(size(Body))], Header).

%%--------------------------------------------------------------------
%% @spec    (Header, URI) ->
%%            {ok, NewHeader, NewDestURI, NewRequestURI}  |
%%            nomatch
%%
%%            Header = #keylist{}
%%            URI    = #sipurl{}
%%
%%            NewHeader     = #keylist{}
%%            NewDestURI    = #sipurl{} "this is what you should use as destination"
%%            NewRequestURI = #sipurl{} "this is what you should use as request URI"
%%
%% @doc     Looks at the Route header. If it exists, we return a new
%%          destination URL for this request, and if there was no
%%          loose router flag in the new destination we append the
%%          original Request-URI to the list of routes, to traverse
%%          strict (RFC2543) proxys.
%% @end
%%--------------------------------------------------------------------
process_route_header(Header, URI) when is_record(Header, keylist), is_record(URI, sipurl) ->
    Route = sipheader:route(Header),
    case Route of
        [#contact{urlstr = FirstRouteStr} | T] ->
	    FirstRoute = sipurl:parse(FirstRouteStr),
	    {NewHeader, NewURI} =
		case is_loose_router(FirstRoute) of
		    true ->
			{Header, URI};
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
%% @spec    (Route, Header) ->
%%            NewHeader
%%
%%            Route = [#contact{}] | []
%%
%%            NewHeader = #keylist{}
%%
%% @doc     If Route is an empty list ([]), delete any existing Route:
%%          headers from Header. Otherwise, set the Route: header(s)
%%          in Header to Route.
%% @end
%%--------------------------------------------------------------------
set_route([], Header) ->
    keylist:delete('route', Header);
set_route(Route, Header) ->
    keylist:set("Route", sipheader:contact_print(Route), Header).

%%--------------------------------------------------------------------
%% @spec    (Route) -> true | false
%%
%%            Route = #sipurl{}
%%
%% @doc     Look for a loose router indicator ("lr") in the
%%          contact-parameters of Route.
%% @end
%%--------------------------------------------------------------------
is_loose_router(Route) when is_record(Route, sipurl) ->
    case url_param:find(Route#sipurl.param_pairs, "lr") of
	[_] ->
	     true;
	[] ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            {ok, Dst, NewRequest} 
%%
%%            Request = #request{}
%%
%%            Dst        = #sipdst{}
%%            NewRequest = #request{}
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Inspect a request that some YXA application has found it
%%          should proxy statelessly. Figure out a single sipdst
%%          destination for this request (sorry, stateless SIP does
%%          not handle multiple destinations) and return that
%%          destination plus a new request (the headers might change
%%          if we have popped a Route header).
%% @end
%%--------------------------------------------------------------------
stateless_route_proxy_request(Request) when is_record(Request, request) ->
    {ok, NewHeader, ApproxMsgSize} = check_proxy_request(Request),
    case stateless_route_proxy_request2(Request#request{header=NewHeader}, ApproxMsgSize) of
	{ok, [Dst], NewRequest} ->
	    {ok, Dst, NewRequest};
	{ok, [Dst | Rest] = DstL, NewRequest} ->
	    IsAck = (Request#request.method == "ACK"),
	    {ok, BendRFC} = yxa_config:get_env(stateless_send_ack_with_backup_plan),
	    if
		IsAck, BendRFC ->
		    %% This is an ACK (probably of an 2xx response to INVITE), and we are not
		    %% configured to be foolishly following the RFC3261 which says we should
		    %% route stateless requests to one and only one destination. The INVITE
		    %% could very well have been sent to the second destination in a destination
		    %% list, because the primary destination was not responding. We can at least
		    %% try to do the same for the ACK (will work for TCP, but fail for UDP).
		    {ok, DstL, NewRequest};
		true ->
		    logger:log(debug, "Siprequest (transport layer) : Warning: request being forwarded "
			       "statelessly had more than one destination, ignoring all but the first "
			       "one :~n~p", [sipdst:debugfriendly([Dst | Rest])]),
		    {ok, Dst, NewRequest}
	    end;
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
%% @spec    (Request) -> {ok, NewHeader, ApproxMsgSize}
%%
%%            Request = #request{}
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Prepares a request for proxying. Checks Max-Forwards, etc.
%%          Not guaranteed to return, might throw a siperror if the
%%          request should not be proxied.
%% @end
%%--------------------------------------------------------------------
check_proxy_request(Request) when is_record(Request, request) ->
    NewHeader1 = proxy_check_maxforwards(Request#request.header),
    check_valid_proxy_request(Request#request.method, NewHeader1),
    NewHeader2 = fix_content_length(NewHeader1, Request#request.body),
    NewHeader3 =
	case yxa_config:get_env(record_route) of
	    {ok, true} -> add_record_route(Request#request.uri, NewHeader2);
	    {ok, false} -> NewHeader2
	end,
    ApproxMsgSize = get_approximate_msgsize(Request#request{header=NewHeader3}),
    {ok, NewHeader3, ApproxMsgSize}.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            Size
%%
%%            Request = #request{}
%%
%%            Size = integer()
%%
%% @doc     Approximate how big a request will be when we send it. We
%%          must know this when determining if it is OK to send it to
%%          a UDP destination or not.
%% @end
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
%% @spec    (Header) ->
%%            NewHeader 
%%
%%            Header = #keylist{}
%%
%%            NewHeader = #keylist{}
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Check Max-Forwards in Header to make sure it is not less
%%          than one when we have subtracted one (1) from it. Return
%%          a new Header with the new Max-Forwards.
%% @end
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
%% @spec    (Header, OrigURI, Parameters, Proto) ->
%%            NewHeader
%%
%%            Header     = #keylist{}
%%            OrigURI    = #sipurl{}
%%            Parameters = string()
%%            Proto      = tcp | tcp6 | udp | udp6 | tls | tls6
%%
%%            NewHeader =  #keylist{}
%%
%% @doc     Generate a Via header for this proxy and add it to Header
%% @end
%%--------------------------------------------------------------------
%% XXX rework to only calculate LoopCookie if we are actually going to use it.
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
%% @spec    (LoopCookie, Branch, Parameters, ParamDict) ->
%%            NewParameters
%%
%%            LoopCookie = string() | none
%%            Branch     = string()
%%            Parameters = [string()]
%%            ParamDict  = dict()
%%
%%            NewParameters = [string()]
%%
%% @doc     If LoopCookie is not 'none', add it to Branch. Return a
%%          new Parameters construct.
%% @end
%%--------------------------------------------------------------------
add_loopcookie_to_branch(none, _Branch, Parameters, _ParamDict) ->
    %% Request has a branch, and loop detection is off
    Parameters;
add_loopcookie_to_branch(LoopCookie, Branch, _Parameters, ParamDict) ->
    NewBranch = lists:concat([Branch, "-o", LoopCookie]),
    Param2 = dict:store("branch", NewBranch, ParamDict),
    sipheader:dict_to_param(Param2).


%%--------------------------------------------------------------------
%% @spec    (Header, OrigURI, LoopCookie, Parameters) ->
%%            NewParameters
%%
%%            Header     = #keylist{}
%%            OrigURI    = #sipurl{}
%%            LoopCookie = string() | none
%%            Parameters = [string()]
%%
%%            NewParameters = [string()]
%%
%% @doc     If LoopCookie is not 'none', add it to the Branch if
%%          Branch does not already contain a loop cookie. Return a
%%          new Parameters construct.
%% @end
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
%% @spec    (OrigURI, Header, Node) ->
%%            {ok, Branch} |
%%            error
%%
%%            OrigURI = #sipurl{} "the original requests URI"
%%            Header  = #keylist{}
%%            Node    = atom() "name of node (to make this code testable)"
%%
%%            Branch = string()
%%
%% @doc     Generate a branch suitable for stateless proxying of a
%%          request (meaning that we make sure that we generate the
%%          very same branch for a retransmission of the very same
%%          request). This is specified in RFC3261 #16.11.
%% @end
%%--------------------------------------------------------------------
stateless_generate_branch(OrigURI, Header, Nodename) ->
    case sipheader:topvia(Header) of
	TopVia when is_record(TopVia, via) ->
	    case sipheader:get_via_branch(TopVia) of
		"z9hG4bK" ++ RestOfBranch ->
		    %% The previous hop has put a RFC3261 branch in it's Via. Use that,
		    %% together with our nodename as branch.
		    In = lists:concat([Nodename, "-rbranch-", RestOfBranch]),
		    {ok, "z9hG4bK-yxa-" ++ make_base64_md5_token(In)};
		_ ->
		    %% No branch, or non-RFC3261 branch. XXX RFC32611 #16.11 suggests
		    %% to include the Top-Via in the hash we generate here. We don't,
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
%% @spec    (In) -> term() "Result of make_3261_token()"
%%
%%            In = term() "indata - anything accepted by erlang:md5()"
%%
%% @doc     Make md5 of input, base64 of that and RFC3261 token of the
%%          result. Tokens are case-insensitive, so we lowercase the
%%          ones we generate here to make them easier to read.
%% @end
%%--------------------------------------------------------------------
make_base64_md5_token(In) ->
    MD5 = binary_to_list(erlang:md5(In)),
    %% remove the trailing == from the result of encode_base64()
    Out = string:strip(httpd_util:encode_base64(MD5), right, $=),
    make_3261_token(Out).

%% RFC 3261 chapter 25 BNF notation of token :
%%      token       =  1*(alphanum / "-" / "." / "!" / "%" / "*" / "_" / "+" / "" / "'" / "~" )
make_3261_token([]) ->
    [];
make_3261_token([H | T]) when H >= $a, H =< $z ->
    [H | make_3261_token(T)];
make_3261_token([H | T]) when H >= $A, H =< $Z ->
    %% tokens are case-insensitive, lowercase the ones we generate
    [H + ($a - $A) | make_3261_token(T)];
make_3261_token([H | T]) when H >= $0, H =< $9 ->
    [H | make_3261_token(T)];
make_3261_token([H | T]) when H == $-; H == $.; H == $!; H == $%;
H == $*; H == $_; H == $+; H == $`; H == $\'; H == $~ ->
    [H | make_3261_token(T)];
make_3261_token([_H | T]) ->
    [$_ | make_3261_token(T)].

%%--------------------------------------------------------------------
%% @spec    () ->
%%            Branch
%%
%%            Branch = string()
%%
%% @doc     Generate a branch that is 'unique across space and time'.
%% @end
%%--------------------------------------------------------------------
generate_branch() ->
    {Megasec, Sec, Microsec} = now(),
    %% We don't need port here since erlang guarantees that Microsecond is never the
    %% same on one node.
    In = lists:concat([node(), Megasec * 1000000 + Sec, 8, $., Microsec]),
    Out = make_base64_md5_token(In),
    "z9hG4bK-yxa-" ++ Out.

%%--------------------------------------------------------------------
%% @spec    (Header) ->
%%            NewHeader
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Turn Request-Route header from a response into Route-
%%          header to include in another request in the same dialog.
%% @end
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
%% @spec    (Header, OrigURI, Proto) ->
%%            Cookie
%%
%%            Header  = #keylist{}
%%            OrigURI = #sipurl{} "original requests URI"
%%            Proto   = term() "the protocol the request was received over"
%%
%%            Cookie = string()
%%
%% @doc     Generate a loop detection cookie, RFC3261 16.6 #8.
%% @end
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
%% @spec    (Header) -> [string()] | none
%%
%%            Header = #keylist{}
%%
%% @doc     Helper-function for get_loop_cookie(). Remove parts of
%%          Proxy-Authorization header that change based on method.
%% @end
%%--------------------------------------------------------------------
proxyauth_without_response(Header) ->
    case keylist:fetch('proxy-authorization', Header) of
	[] -> none;
	ProxyAuths ->
	    lists:foldl(fun(This, Acc) ->
				AuthDict = sipheader:auth(This),
				NewDict1 = dict:erase("response", AuthDict),
				NewDict2 = dict:erase("nonce", NewDict1),
				NewDict3 = dict:erase("cnonce", NewDict2),
				NewDict4 = dict:erase("opaque", NewDict3),
				Acc ++ sipheader:dict_to_param(NewDict4)
			end, [], ProxyAuths)
    end.

%%--------------------------------------------------------------------
%% @spec    (Method, Header) -> true | false
%%
%%            Method = string()
%%            Header = #keylist{}
%%
%% @doc     Check if a request has any Proxy-Require header that
%%          includes an extension that we don't support. Always
%%          return true on ACK and CANCEL - we can never reject
%%          those.
%% @end
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
%% @spec    () ->
%%            Hostname
%%
%%            Hostname = string()
%%
%% @doc     Get my hostname (the first one in the list, or my IP
%%          address if I have no list of configured hostnames).
%% @end
%%--------------------------------------------------------------------
myhostname() ->
    case yxa_config:get_env(myhostnames) of
	{ok, [FirstHostname | _]} when is_list(FirstHostname) ->
	    FirstHostname;
	none ->
	    siphost:myip()
    end.

%%--------------------------------------------------------------------
%% @spec    (Proto, Parameters) ->
%%            Via
%%
%%            Proto      = term()
%%            Parameters = [string()]
%%
%%            Via = #via{}
%%
%% @doc     Create a Via for this proxy, given the protocol.
%% @end
%%--------------------------------------------------------------------
create_via(Proto, Parameters) ->
    Hostname = myhostname(),
    Port = sipsocket:get_listenport(Proto),
    ViaProto = sipsocket:proto2viastr(Proto),
    #via{proto=ViaProto, host=Hostname, port=Port, param=Parameters}.

%%--------------------------------------------------------------------
%% @spec    (URL, Header) ->
%%            NewHeader
%%
%%            URL    = #sipurl{} "original Request-URI"
%%            Header = #keylist{}
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Prepend a Record-Route header for this proxy, based on the
%%          original Request-URI, to Header.
%% @end
%%--------------------------------------------------------------------
add_record_route(URL, Header) when is_record(URL, sipurl) ->
    add_record_route(URL#sipurl.proto, undefined, undefined, Header).

%%--------------------------------------------------------------------
%% @spec    (Proto, Hostname, Port, Header) ->
%%            NewHeader
%%
%%            Proto    = string() "\"sips\" or \"sip\""
%%            Hostname = undefined | string()
%%            Port     = undefined | none | integer()
%%            Header   = #keylist{}
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Prepend a Record-Route header for Proto:Host:Port to the
%%          Header keylist.
%% @end
%%--------------------------------------------------------------------
add_record_route(Proto, Hostname, Port, Header) when is_list(Proto), is_list(Hostname); Hostname == undefined,
						     is_integer(Port); Port == none; Port == undefined ->
    RouteStr = construct_record_route(Proto, Hostname, Port),

    %% Check if our Record-Route (RouteStr) is already present as the first entry of
    %% the Record-Route set in Header.
    AlreadyPresent =
	case keylist:fetch('record-route', Header) of
	    [RouteStr | _] ->
		%% We only do byte-by-byte matching since the purpose of this check is to
		%% make sure that this function isn't used twice on a message - not to make sure
		%% that there is no Record-Route that would resolve to this proxy/UA in there.
		true;
	    _ ->
		%% No Record-Route in header, or not matching ours
		false
	end,
    case AlreadyPresent of
	true ->
	    logger:log(debug, "Siprequest: NOT adding Record-Route since the first Record-Route matches mine"),
	    Header;
	false ->
	    keylist:prepend({"Record-Route", [RouteStr]}, Header)
    end.

%%--------------------------------------------------------------------
%% @spec    (Proto) ->
%%            RRStr
%%
%%            Proto = string() "\"sips\" or \"sip\""
%%
%%            RRStr = string() "URL enclosed in \"<>\""
%%
%% @equiv    construct_record_route(Proto, undefined, undefined)
%% @end
%%--------------------------------------------------------------------
construct_record_route(Proto) when is_list(Proto) ->
    construct_record_route(Proto, undefined, undefined).

%%--------------------------------------------------------------------
%% @spec    (Proto, Hostname, Port) ->
%%            RRStr
%%
%%            Proto    = string() "\"sips\" or \"sip\""
%%            Hostname = string() | undefined
%%            Port     = integer() | undefined
%%
%%            RRStr = string() "URL enclosed in \"<>\""
%%
%% @doc     Construct a Record-Route header value for this proxy.
%% @end
%%--------------------------------------------------------------------
construct_record_route(Proto, undefined, undefined) when is_list(Proto) ->
    {RRProto, MyPort} =
	case Proto of
	    "sips" ->
		{"sips", sipsocket:get_listenport(tls)};
	    _ ->
		%% Create sip: Record-Route for everything except SIPS-URIs
		{"sip", sipsocket:get_listenport(udp)}
	end,
    URLport = sipsocket:default_port(Proto, none),
    %% If we are listening on the default port for the protocol in URL, we don't
    %% need to set a port in the Record-Route. If not however, we need to set the
    %% port because otherwise Cisco 79xx phones (version 7.5) will put port 5060
    %% in the Route headers!
    Port = case (URLport == MyPort) of
	       true ->
		   none;
	       false ->
		   MyPort
	   end,
    construct_record_route(RRProto, myhostname(), Port);

%% Returns : RRStr = string(), URL enclosed in "<>"
construct_record_route(Proto, Hostname, Port) ->
    URL =
	case yxa_config:get_env(record_route_url) of
	    {ok, RRURL} ->
		case {Proto, RRURL#sipurl.proto} of
		    {"sips", "sip"} ->
			%% Proto is SIPS, but RRURL's Proto is SIP. Upgrade SIP to SIPS.
			sipurl:set([{proto, Proto}], RRURL);
		    _ ->
			RRURL
		end;
	    none ->
		%% we use 'lr=true' instead of just 'lr' since some SIP stacks are known
		%% to require that parameters are of the format 'key=value'.
		Param1 = ["lr=true"],
		case Port of
		    none ->
			%% It was requested that we don't include a port number.
			sipurl:new([{proto, Proto}, {host, Hostname}, {param, Param1}]);
		    _ when is_integer(Port) ->
			sipurl:new([{proto, Proto}, {host, Hostname}, {port, Port}, {param, Param1}])
		end
	end,
    contact:print(contact:new(URL)).

%%--------------------------------------------------------------------
%% @spec    (Header, ExtraHeaders) ->
%%            NewHeader
%%
%%            Header       = #keylist{}
%%            ExtraHeaders = #keylist{}
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Copy the headers that are required for all responses (Via,
%%          From, To, Call-Id, CSeq) plus any extra requested headers
%%          from a source Header keylist and create a new keylist
%%          based on these.
%% @end
%%--------------------------------------------------------------------
standardcopy(Header, ExtraHeaders) ->
    keylist:appendlist(keylist:copy(Header,
				    [via, from, to, 'call-id', cseq]),
		       ExtraHeaders).

%%--------------------------------------------------------------------
%% @spec    (Header, Socket, Auth, Stale) -> term() "send result"
%%
%%            Header = #keylist{}
%%            Socket = #sipsocket{} | none
%%            Auth   = {Realm, Nonce, Opaque}
%%            Realm  = string()
%%            Nonce  = string()
%%            Opaque = string()
%%            Stale  = true | false
%%
%% @doc     Send a '401 Authentication Required' response.
%% @end
%%--------------------------------------------------------------------
send_auth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"WWW-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    Response1 = #response{status=401, reason="Authentication Required",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

%%--------------------------------------------------------------------
%% @spec    (Header, Socket, Auth, Stale) -> term() "send result"
%%
%%            Header = #keylist{}
%%            Socket = #sipsocket{} | none
%%            Auth   = {Realm, Nonce, Opaque}
%%            Realm  = string()
%%            Nonce  = string()
%%            Opaque = string()
%%            Stale  = true | false
%%
%% @doc     Send a '407 Proxy Authentication Required' response.
%% @end
%%--------------------------------------------------------------------
send_proxyauth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"Proxy-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    Response1 = #response{status=407, reason="Proxy Authentication Required",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

%%--------------------------------------------------------------------
%% @spec    (Location, Header, Socket) -> term() "send result"
%%
%%            Location = #sipurl{} | string()
%%            Header   = #keylist{}
%%            Socket   = #sipsocket{} | none
%%
%% @doc     Send a '302 Moved Temporarily' response.
%% @end
%%--------------------------------------------------------------------
send_redirect(Location, Header, Socket) when is_record(Location, sipurl) ->
    Contact = contact:new(none, Location, []),
    ExtraHeaders = [{"Contact",
		     sipheader:contact_print([Contact])}],
    Response1 = #response{status=302, reason="Moved Temporarily",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

%%--------------------------------------------------------------------
%% @spec    (Header, Socket) -> term() "send result"
%%
%%            Header = #keylist{}
%%            Socket = #sipsocket{} | none
%%
%% @doc     Send a '404 Not Found' response.
%% @end
%%--------------------------------------------------------------------
send_notfound(Header, Socket) ->
    Response1 = #response{status=404, reason="Not found",
			  header=standardcopy(Header, [])},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

%%--------------------------------------------------------------------
%% @spec    (Header, Socket) -> term() "send result"
%%
%%            Header = #keylist{}
%%            Socket = #sipsocket{} | none
%%
%% @doc     Send a '480 Temporarily Unavailable' response with a
%%          Retry-After: 180 header.
%% @end
%%--------------------------------------------------------------------
send_notavail(Header, Socket) ->
    ExtraHeaders = [{"Retry-After", ["180"]}],
    Response1 = #response{status=480, reason="Temporarily unavailable",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, <<>>),
    transportlayer:send_response(Socket, Response).

%%--------------------------------------------------------------------
%% @spec    (Header, Socket, Body) -> term() "send result"
%%
%%            Header = #keylist{}
%%            Socket = #sipsocket{} | none
%%            Body   = string() | binary()
%%
%% @doc     Produce a standard '200 OK' response with Content-Type set
%%          to 'application/sdp' (so Body better be an SDP).
%% @end
%%--------------------------------------------------------------------
send_answer(Header, Socket, Body) ->
    %% Remember to add a linefeed (\n) to the end of Body that you pass to this function
    ExtraHeaders = [{"Content-Type", ["application/sdp"]}],
    Response1 = #response{status=200, reason="OK",
			  header=standardcopy(Header, ExtraHeaders)},
    Response = set_response_body(Response1, Body),
    transportlayer:send_response(Socket, Response).

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, ExtraHeaders) ->
%%            term() "result of transactionlayer:send_response_handler/4"
%%
%%            Request      = #request{}
%%            YxaCtx       = #yxa_ctx{}
%%            ExtraHeaders = [{Key, Value}]
%%
%% @doc     Produce a standard 200 OK response to an OPTIONS request
%%          an YXA application received.
%% @end
%%--------------------------------------------------------------------
request_to_me(#request{method = "OPTIONS"} = Request, YxaCtx, ExtraHeaders) when is_record(YxaCtx, yxa_ctx),
										 is_list(ExtraHeaders) ->
    #yxa_ctx{thandler   = THandler,
	     app_logtag = LogTag
	    } = YxaCtx,

    {ok, AppName} = yxa_config:get_env(yxa_appmodule),
    logger:log(normal, "~s: ~p: 'OPTIONS ~s' (to me) -> 200 OK",
	       [LogTag, AppName, sipurl:print(Request#request.uri)]),

    %% XXX The OPTIONS response SHOULD include Accept, Accept-Encoding, Accept-Language, and
    %% Supported headers. RFC 3261 section 11.
    ExtraHeaders1 =
	case lists:keysearch("Supported", 1, ExtraHeaders) of
	    {value, {"Supported", _}} ->
		%% If we are given a Supported header, we don't modify it
		ExtraHeaders;
	    false ->
		This =
		    case yxa_config:get_env(stun_demuxing_on_sip_ports) of
			{ok, true}  -> [{"Supported", ["sip-stun"]}];
			{ok, false} -> []
		    end,
		ExtraHeaders ++ This
	end,

    transactionlayer:send_response_handler(THandler, 200, "OK", ExtraHeaders1);

request_to_me(Request, YxaCtx, ExtraHeaders) when is_record(Request, request), is_record(YxaCtx, yxa_ctx),
						     is_list(ExtraHeaders) ->
    #yxa_ctx{thandler   = THandler,
	     app_logtag = LogTag
	    } = YxaCtx,

    {ok, AppName} = yxa_config:get_env(yxa_appmodule),

    logger:log(normal, "~s: ~p: '~s ~s' (to me) -> 481 Call/Transaction Does Not Exist",
	       [LogTag, AppName, Request#request.method, sipurl:print(Request#request.uri)]),
    transactionlayer:send_response_handler(THandler, 481, "Call/Transaction Does Not Exist").

%%--------------------------------------------------------------------
%% @spec    (Status, Reason, Body, ExtraHeaders, ViaParameters,
%%          SipSocket, Request) ->
%%            Response
%%
%%            Status        = integer() "SIP status code"
%%            Reason        = string() "SIP reason phrase"
%%            Body          = binary() | string()
%%            ExtraHeaders  = #keylist{}
%%            ViaParameters = [string()]
%%            SipSocket     = #sipsocket{}  |
%%                            protocol (tcp |
%%                            tcp6          |
%%                            tls           |
%%                            tls6          |
%%                            udp           |
%%                            udp6)
%%            Request       = #request{}
%%
%%            Response = #response{}
%%
%% @doc     Create a response given a request.
%% @end
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

    CopyHeaders1 = [via, from, to, 'call-id', cseq, 'record-route', timestamp, 'content-type'],
    %% If there is no body, don't copy Content-Type
    CopyHeaders2 =
	case Body of
	    <<>> -> CopyHeaders1 -- ['content-type'];
	    _ -> CopyHeaders1
	end,
    %% If this is not a 100 Trying response, don't copy Timestamp
    %% The preservation of Timestamp headers into 100 Trying response is mandated by RFC 3261 8.2.6.1
    CopyHeaders =
	case Status of
	    100 -> CopyHeaders2;
	    _ ->   CopyHeaders2 -- ['timestamp']
	end,

    ExtraHeaders1 =
	case yxa_config:get_env(include_server_info_in_responses) of
	    {ok, true} ->
		case lists:keysearch("Server", 1, ExtraHeaders) of
		    {value, _} -> ExtraHeaders;
		    false ->
			{ok, App} = yxa_config:get_env(yxa_appmodule),
			ServerStr = io_lib:format("YXA ~p at ~s", [App, siprequest:myhostname()]),
			ExtraHeaders ++ [{"Server", [ServerStr]}]
		end;
	    {ok, false} ->
		ExtraHeaders
	end,

    %% First copy the selected headers from the reqest
    AnswerHeader1 = keylist:copy(Request#request.header, CopyHeaders),
    %% ... then add any extra headers we might want to add to the response
    AnswerHeader2 = keylist:appendlist(AnswerHeader1, ExtraHeaders1),

    %% PlaceHolderVia is an EXTRA Via with our hostname. We could do without this if
    %% we sent it with send_response() instead of send_proxy_response() but it is easier
    %% to just add an extra Via that will be stripped by send_proxy_response() and don't
    %% have to make a difference in how we send out responses.
    V = create_via(Proto, ViaParameters),
    PlaceHolderVia = sipheader:via_print([V]),
    AnswerHeader3 = keylist:prepend({"Via", PlaceHolderVia}, AnswerHeader2),

    Response1 = #response{status = Status,
			  reason = Reason,
			  header = AnswerHeader3
			 },

    set_response_body(Response1, Body).

%%--------------------------------------------------------------------
%% @spec    (BinLine1, Header, BinBody) ->
%%            Msg
%%
%%            BinLine1 = binary() "the top line of the request/response"
%%            Header   = #keylist{}
%%            BinBody  = binary() "body of request/response"
%%
%%            Msg = binary()
%%
%% @doc     Create a binary representing the request/response, ready
%%          to be sent out on the wire.
%% @end
%%--------------------------------------------------------------------
binary_make_message(BinLine1, Header, BinBody) when is_binary(BinLine1), is_record(Header, keylist),
						    is_binary(BinBody) ->
    BinHeaders = sipheader:build_header_binary(Header),
    list_to_binary([BinLine1, 13, 10, BinHeaders, 13, 10, BinBody]).

%%--------------------------------------------------------------------
%% @spec    (Request, Body) ->
%%            NewRequest
%%
%%            Request = #request{}
%%            Body    = string() | binary()
%%
%%            NewRequest = #request{}
%%
%% @doc     Set the body of a request record.
%% @end
%%--------------------------------------------------------------------
set_request_body(Request, Body) when is_record(Request, request), is_binary(Body) ->
    ILen = size(Body),
    Len = integer_to_list(ILen),
    NewHeader = keylist:set("Content-Length", [Len], Request#request.header),
    Request#request{header=NewHeader, body=Body};
set_request_body(Request, Body) when is_record(Request, request), is_list(Body) ->
    set_request_body(Request, list_to_binary(Body)).

%%--------------------------------------------------------------------
%% @spec    (Request, Body) ->
%%            NewResponse
%%
%%            Request = #request{}
%%            Body    = string() | binary()
%%
%%            NewResponse = #response{}
%%
%% @doc     Set the body of a request record, and calculate
%%          Content-Length.
%% @end
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
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    %% myhostname()
    %%--------------------------------------------------------------------
    %% test that we get a list back from myhostname()
    autotest:mark(?LINE, "myhostname/0 - 1"),
    MyHostname = myhostname(),
    true = is_list(MyHostname),


    %% generate_branch()
    %%--------------------------------------------------------------------
    %% test that generate_branch gives us a RFC3261 branch
    autotest:mark(?LINE, "generate_branch/0 - 1"),
    Branch = generate_branch(),
    "z9hG4bK-yxa-" = string:substr(Branch, 1, 12),


    %% make_3261_token(In)
    %%--------------------------------------------------------------------
    %% test that our make_3261_token produces the expected results
    autotest:mark(?LINE, "make_3261_token/1 - 1"),
    "abcdexyz-.!%*_+`'~123_" = make_3261_token("abcdeXYZ-.!%*_+`'~123@"),

    autotest:mark(?LINE, "make_base64_md5_token/1 - 1"),
    "wd_cg1j2cfuav0nwdjlxtq" = make_base64_md5_token("abcdeXYZ123-.!%*_+`'~123@"),


    %% create_via(Proto, Parameters)
    %%--------------------------------------------------------------------
    SipLPort  = sipsocket:get_listenport(tcp),
    SipsLPort = sipsocket:get_listenport(tls),

    %% tcp
    autotest:mark(?LINE, "create_via/2 - 1"),
    #via{proto="SIP/2.0/TCP", host=MyHostname, port=SipLPort, param=["foo=bar"]} =
	create_via(tcp, ["foo=bar"]),

    %% tcp6
    autotest:mark(?LINE, "create_via/2 - 2"),
    #via{proto="SIP/2.0/TCP", host=MyHostname, port=SipLPort, param=[]} =
	create_via(tcp6, []),

    %% udp
    autotest:mark(?LINE, "create_via/2 - 3"),
    #via{proto="SIP/2.0/UDP", host=MyHostname, port=SipLPort, param=[]} =
	create_via(udp, []),

    %% udp6
    autotest:mark(?LINE, "create_via/2 - 4"),
    #via{proto="SIP/2.0/UDP", host=MyHostname, port=SipLPort, param=[]} =
	create_via(udp6, []),

    %% tls
    autotest:mark(?LINE, "create_via/2 - 5"),
    #via{proto="SIP/2.0/TLS", host=MyHostname, port=SipsLPort, param=[]} =
	create_via(tls, []),

    %% tls6
    autotest:mark(?LINE, "create_via/2 - 6"),
    #via{proto="SIP/2.0/TLS", host=MyHostname, port=SipsLPort, param=[]} =
	create_via(tls6, []),

    %% foo protocol - expect crash
    autotest:mark(?LINE, "create_via/2 - 7"),
    {'EXIT', {function_clause, _}} = (catch create_via(foo, [])),


    %% add_record_route(URL, Header)
    %% and implicitly add_record_route/4
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add_record_route/2 - 1"),
    EmptyHeader = keylist:from_list([]),

    RRHeader1 = add_record_route(sipurl:parse("sip:ft@one.example.org;transport=tcp"), EmptyHeader),

    %% check that there is now a single Record-Route in RRHeader1
    autotest:mark(?LINE, "add_record_route/2 - 2"),
    [RRoute1] = sipheader:record_route(RRHeader1),

    %% check the contents in the single Record-Route
    %% contact.display_name
    autotest:mark(?LINE, "add_record_route/2 - 3"),
    none = RRoute1#contact.display_name,

    %% contact.urlstr
    autotest:mark(?LINE, "add_record_route/2 - 4.1"),
    #sipurl{proto="sip", host=MyHostname, port=none, param_pairs=RRParam1} =
	sipurl:parse(RRoute1#contact.urlstr),

    %% contact.urlstr sipurl parameters
    autotest:mark(?LINE, "add_record_route/2 - 4.2"),
    ["true"] = url_param:find(RRParam1, "lr"),
    %% we should never set transport
    [] = url_param:find(RRParam1, "transport"),

    %% contact.contact_param
    autotest:mark(?LINE, "add_record_route/2 - 5"),
    EmptyContactParam = contact_param:to_norm([]),
    EmptyContactParam = RRoute1#contact.contact_param,

    %% Already existing RR, and test SIPS protocol as well
    autotest:mark(?LINE, "add_record_route/2 - 6.1"),
    RRHeader2 = add_record_route(sipurl:parse("sips:ft@two.example.org;transport=tls"), RRHeader1),
    %% get our RRoute2 and also check that RRoute1 is still there
    autotest:mark(?LINE, "add_record_route/2 - 6.2"),
    [RRoute2, RRoute1] = sipheader:record_route(RRHeader2),

    %% contact.urlstr
    autotest:mark(?LINE, "add_record_route/2 - 6.3"),
    #sipurl{proto="sips", host=MyHostname, port=none, param_pairs=RRParam2} =
	sipurl:parse(RRoute2#contact.urlstr),

    %% contact.urlstr sipurl parameters
    autotest:mark(?LINE, "add_record_route/2 - 6.4"),
    ["true"] = url_param:find(RRParam2, "lr"),
    %% transport=tls is deprecated
    [] = url_param:find(RRParam2, "transport"),

    %% check that add_record_route/2 does not add duplicate Record-Route
    autotest:mark(?LINE, "add_record_route/2 - 7"),
    RRHeader3 = add_record_route(sipurl:parse("sips:ft@two.example.org;transport=tls"), RRHeader2),

    autotest:mark(?LINE, "add_record_route/2 - 8"),
    [RRoute2, RRoute1] = sipheader:record_route(RRHeader3),

    autotest:mark(?LINE, "add_record_route/2 - 9.1"),
    %% check that we don't care about unknown transport=
    RRHeader9 = add_record_route(sipurl:parse("sip:ft@foo.example.org;transport=foo"), EmptyHeader),

    autotest:mark(?LINE, "add_record_route/2 - 9.2"),
    [RRoute9] = sipheader:record_route(RRHeader9),
    RRoute9_URL = sipurl:parse(RRoute9#contact.urlstr),
    %% we should never set transport
    [] = url_param:find(RRoute9_URL#sipurl.param_pairs, "transport"),


    %% build request header
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "build request header - 1"),
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
    autotest:mark(?LINE, "make_answerheader/1 - 1"),
    AHeader1 = make_answerheader(ReqHeader),
    ["<sip:p1:1111>", "<sip:p2:2222>"] = keylist:fetch('route', AHeader1),

    autotest:mark(?LINE, "make_answerheader/1 - 2"),
    %% Check that the Record-Route was deleted
    [] = keylist:fetch('record-route', AHeader1),

    autotest:mark(?LINE, "make_answerheader/1 - 3"),
    %% Test without a Record-Route header
    AHeader2 = keylist:delete('record-route', ReqHeader),
    AHeader2 = make_answerheader(AHeader2),


    %% get_loop_cookie(ReqHeader, URI, Proto
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_loop_cookie/3 - 1"),
    "jyjpsr4iqjymexlgub58yg" = get_loop_cookie(ReqHeader, sipurl:parse("sip:test@example.org"), tcp),


    %% make_response(Status, Reason, Body, ExtraHeaders, Parameters, Proto, Request)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "make_response/7 - 1.1"),
    Response1 = make_response(100, "Trying", "test", [], [], tcp, #request{header=ReqHeader}),

    %% basic response record check
    autotest:mark(?LINE, "make_response/7 - 1.2"),
    #response{status=100, reason="Trying", body = <<"test">>} = Response1,

    %% Timestamp still present in 100 Trying check
    autotest:mark(?LINE, "make_response/7 - 1.3"),
    ["1234"] = keylist:fetch(timestamp, Response1#response.header),

    %% correct Content-Length added
    autotest:mark(?LINE, "make_response/7 - 1.4"),
    ["4"] = keylist:fetch('content-length', Response1#response.header),

    %% Foo header not copied
    autotest:mark(?LINE, "make_response/7 - 1.5"),
    [] = keylist:fetch("Foo", Response1#response.header),

    %% test make_response/7
    autotest:mark(?LINE, "make_response/7 - 2.1"),
    Response2_ExtraHeaders = [{"Extra-Header", ["test"]},
			      {"Server", ["unit test"]}
			     ],
    Response2 = make_response(486, "_BUSY_", <<>>, Response2_ExtraHeaders, ["test=true"],
			      tls6, #request{header=ReqHeader}),

    %% basic response record check
    autotest:mark(?LINE, "make_response/7 - 2.2"),
    #response{status=486, reason="_BUSY_", body = <<>>} = Response2,

    %% Timestamp NOT present in 486 Busy Here
    autotest:mark(?LINE, "make_response/7 - 2.3"),
    [] = keylist:fetch(timestamp, Response2#response.header),

    %% Content-Type NOT present in 486 Busy Here with empty body
    autotest:mark(?LINE, "make_response/7 - 2.4"),
    [] = keylist:fetch("Content-Type", Response2#response.header),

    %% Extra-Header supplied added correctly
    autotest:mark(?LINE, "make_response/7 - 2.5"),
    ["test"] = keylist:fetch("Extra-Header", Response2#response.header),

    %% Make sure Record-Route is NOT turned into Route (we used to do that, but it was wrong)
    autotest:mark(?LINE, "make_response/7 - 2.6.1"),
    ["<sip:p1:1111>", "<sip:p2:2222>"] = keylist:fetch('record-route', Response2#response.header),

    %% (make sure Route is still empty)
    autotest:mark(?LINE, "make_response/7 - 2.6.2"),
    [] = keylist:fetch('route', Response2#response.header),

    %% Supplied Server header preventing a Server: header from being generated
    autotest:mark(?LINE, "make_response/7 - 2.7"),
    ["unit test"] = keylist:fetch("Server", Response2#response.header),

    autotest:mark(?LINE, "make_response/7 - 3.1"),
    %% test with Server: header generation disabled, and protocol provided as a sipsocket record
    ok = yxa_test_config:init(unittest, [{include_server_info_in_responses, false}]),
    Response3_ExtraHeaders = [],
    Response3 = make_response(486, "_BUSY_", <<>>, Response3_ExtraHeaders, ["test=true"],
			      #sipsocket{proto = yxa_test}, #request{header = ReqHeader}),

    %% basic response record check
    autotest:mark(?LINE, "make_response/7 - 3.2"),
    #response{status = 486, reason = "_BUSY_", body = <<>>} = Response3,

    %% check that we have no Server: tag
    autotest:mark(?LINE, "make_response/7 - 3.3"),
    [] = keylist:fetch(server, Response3#response.header),

    %% clean up our special test config
    yxa_test_config:stop(),

    %% check_valid_proxy_request(Method, Header)
    %%--------------------------------------------------------------------
    CVPR_Header = keylist:from_list([{"Proxy-Require", ["fooextension"]}]),

    autotest:mark(?LINE, "check_valid_proxy_request/2 - 1"),
    %% verify that we don't reject ACK even if there is a Proxy-Require in there
    true = check_valid_proxy_request("ACK", CVPR_Header),

    autotest:mark(?LINE, "check_valid_proxy_request/2 - 2"),
    %% verify that we don't reject CANCEL even if there is a Proxy-Require in there
    true = check_valid_proxy_request("CANCEL", CVPR_Header),

    autotest:mark(?LINE, "check_valid_proxy_request/2 - 3"),
    %% verify that we reject everything else that has a Proxy-Require (since we don't
    %% support any extensions)
    {siperror, 420, "Bad Extension", _EH} = (catch check_valid_proxy_request("OPTIONS", CVPR_Header)),


    %% check_proxy_request(Request)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_proxy_request/1 - 1"),
    ReqHeader2 = keylist:set("Content-Length", ["900"], ReqHeader),
    Req2 = #request{method="MESSAGE", uri=sipurl:parse("sip:test@example.org"), header=ReqHeader2, body = <<"foo">>},

    autotest:mark(?LINE, "check_proxy_request/1 - 2"),
    {ok, ReqHeader2_2, ApproxMsgSize2_2} = check_proxy_request(Req2),

    %% check that Content-Length has been properly set in ReqHeader2_2
    autotest:mark(?LINE, "check_proxy_request/1 - 3"),
    ["3"] = keylist:fetch('content-length', ReqHeader2_2),

    %% check that requests get a default Max-Forwards if they lack it
    autotest:mark(?LINE, "check_proxy_request/1 - 4"),
    ["70"] = keylist:fetch('max-forwards', ReqHeader2_2),

    %% check that the approximate message size is reasonable
    autotest:mark(?LINE, "check_proxy_request/1 - 5"),
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
    autotest:mark(?LINE, "check_proxy_request/1 - 6"),
    ReqHeader3 = keylist:set("Proxy-Require", ["CantBeSupported"], ReqHeader2),
    Req3 = Req2#request{method="INVITE", header=ReqHeader3, body = <<>>},
    Unsupported = [{"Unsupported",["CantBeSupported"]}],
    {siperror, 420, _, Unsupported} = (catch check_proxy_request(Req3)),

    %% check that we do accept Max-Forwards 2
    autotest:mark(?LINE, "check_proxy_request/1 - 7"),
    ReqHeader4 = keylist:set("Max-Forwards", ["2"], ReqHeader),
    Req4 = Req3#request{header=ReqHeader4},
    {ok, _, _} = check_proxy_request(Req4),

    %% check that we don't accept Max-Forwards 1
    autotest:mark(?LINE, "check_proxy_request/1 - 8"),
    ReqHeader5 = keylist:set("Max-Forwards", ["1"], ReqHeader),
    Req5 = Req3#request{header=ReqHeader5},
    {siperror, 483, _} = (catch check_proxy_request(Req5)),


    %% process_route_header(Header, URI)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_route_header/2 - 1"),
    InURI = sipurl:parse("sip:in@example.org"),
    [InURIasContact] = contact:parse(["<sip:in@example.org>"]),
    StrictRouter = "<sip:strict-router.example.org>",
    [StrictRouterContact] = contact:parse(["<sip:strict-router.example.org>"]),
    StrictRouterURL = sipurl:parse("sip:strict-router.example.org"),
    LooseRouter = "<sip:loose-router.example.org;lr=true>",
    [LooseRouterContact] = contact:parse(["<sip:loose-router.example.org;lr=true>"]),
    LooseRouterURL = sipurl:parse("sip:loose-router.example.org;lr=true"),

    %% Test strict router traversal
    autotest:mark(?LINE, "process_route_header/2 - 2.1"),
    PRHeader1 = keylist:from_list([{"Route", [StrictRouter, LooseRouter]}]),
    {ok, PRHeader1_out, StrictRouterURL, StrictRouterURL} =
	process_route_header(PRHeader1, InURI),

    autotest:mark(?LINE, "process_route_header/2 - 2.2"),
    %% check the Route headers in the new headers returned
    [LooseRouterContact, InURIasContact] = sipheader:route(PRHeader1_out),

    %% Test strict router traversal with no other Route-header
    autotest:mark(?LINE, "process_route_header/2 - 3.1"),
    PRHeader2 = keylist:from_list([{"Route", [StrictRouter]}]),
    {ok, PRHeader2_out, StrictRouterURL, StrictRouterURL} =
	process_route_header(PRHeader2, InURI),

    autotest:mark(?LINE, "process_route_header/2 - 3.2"),
    %% check the Route headers in the new headers returned
    [InURIasContact] = sipheader:route(PRHeader2_out),

    %% Test loose router, with one more Route
    autotest:mark(?LINE, "process_route_header/2 - 4.1"),
    PRHeader3 = keylist:from_list([{"Route", [LooseRouter, StrictRouter]}]),
    {ok, PRHeader3_out, LooseRouterURL, InURI} =
	process_route_header(PRHeader3, InURI),

    autotest:mark(?LINE, "process_route_header/2 - 4.2"),
    %% check the Route headers in the new headers returned
    [LooseRouterContact, StrictRouterContact] = sipheader:route(PRHeader3_out),

    %% Test loose router alone
    autotest:mark(?LINE, "process_route_header/2 - 5.1"),
    PRHeader4 = keylist:from_list([{"Route", [LooseRouter]}]),
    {ok, PRHeader4_out, LooseRouterURL, InURI} =
	process_route_header(PRHeader4, InURI),

    [LooseRouterContact] = sipheader:route(PRHeader4_out),

    %% check empty header, no Route present
    autotest:mark(?LINE, "process_route_header/2 - 6"),
    nomatch = process_route_header(keylist:from_list([]), InURI),


    %% create_via(Proto, Parameters)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "create_via/1 - 1.1"),
    TLSBasicVia = create_via(tls, []),

    autotest:mark(?LINE, "create_via/1 - 1.2"),
    #via{proto="SIP/2.0/TLS", host=MyHostname, port=SipsLPort} = TLSBasicVia,


    %% branch_contains_loopcookie(Branch)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "branch_contains_loopcookie/1 - 1"),
    %% contains YXA loop cookie
    true = branch_contains_loopcookie("z9hG4bK-yxa-foo-oLOOPCOOKIE"),

    autotest:mark(?LINE, "branch_contains_loopcookie/1 - 2"),
    %% contains -oSOMETHING, but does not begin with "z9hG4bK-yxa"
    false = branch_contains_loopcookie("z9hG4bK-foo-oLOOPCOOKIE"),

    autotest:mark(?LINE, "branch_contains_loopcookie/1 - 3"),
    %% begins with "z9hG4bK-yxa", but has no loop cookie
    false = branch_contains_loopcookie("z9hG4bK-yxa-foo"),


    %% stateless_generate_branch(OrigURI, Header, Nodename)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "stateless_generate_branch/3 - 0"),
    SGB_URL1 = sipurl:parse("sip:ft@example.org"),

    autotest:mark(?LINE, "stateless_generate_branch/3 - 1"),
    %% test normal case
    {ok, "z9hG4bK-yxa-s1xsdm0n23tbxjnamqp3ww"} =
	stateless_generate_branch(SGB_URL1, ReqHeader, "test-nodename"),

    autotest:mark(?LINE, "stateless_generate_branch/3 - 2"),
    SGB_Header2 = keylist:set("Via", ["SIP/2.0/TCP example.org;branch=z9hG4bK-test"], ReqHeader),
    {ok, "z9hG4bK-yxa-wgvvbdixu1kkvqywwkmgcg"} =
	stateless_generate_branch(SGB_URL1, SGB_Header2, "test-nodename"),

    autotest:mark(?LINE, "stateless_generate_branch/3 - 3"),
    %% Test without Via header
    error = stateless_generate_branch(SGB_URL1, keylist:delete('via', ReqHeader), "test-nodename"),


    %% add_loopcookie_to_branch(LoopCookie, Branch, Parameters, ParamDict)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add_loopcookie_to_branch/4 - 1"),
    %% No loop cookie, transparently returns Parameters
    ["foo=Bar"] = add_loopcookie_to_branch(none, "foo", ["foo=Bar"], sipheader:param_to_dict([])),

    autotest:mark(?LINE, "add_loopcookie_to_branch/4 - 2"),
    %% Normal case
    ["branch=x-oLOOP"] = add_loopcookie_to_branch("LOOP", "x", ["branch=x"],
						  sipheader:param_to_dict(["branch=x"])),


    %% add_stateless_generated_branch2(Header, OrigURI, LoopCookie, Parameters, Node)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add_stateless_generated_branch2/5 - 1"),
    {siperror, 500, _} =
	(catch add_stateless_generated_branch2( keylist:delete('via', ReqHeader),
					        sipurl:parse("sip:ft@example.org"),
					        "LOOPCOOKIE", ["branch=x"], 'test-nodename'
					       )),

    autotest:mark(?LINE, "add_stateless_generated_branch2/5 - 2"),
    ASGB_Header1 = keylist:set("Via", ["SIP/2.0/TCP example.org;branch=z9hG4bK-test"], ReqHeader),
    ["branch=z9hG4bK-yxa-wgvvbdixu1kkvqywwkmgcg-oLOOPCOOKIE"] =
	add_stateless_generated_branch2( ASGB_Header1,
					 sipurl:parse("sip:ft@example.org"),
					 "LOOPCOOKIE", [], 'test-nodename'
					),

    autotest:mark(?LINE, "add_stateless_generated_branch2/5 - 3"),
    ASGB_Header1 = keylist:set("Via", ["SIP/2.0/TCP example.org;branch=z9hG4bK-test"], ReqHeader),
    ["branch=z9hG4bK-yxa-wgvvbdixu1kkvqywwkmgcg"] =
	add_stateless_generated_branch2( ASGB_Header1,
					 sipurl:parse("sip:ft@example.org"),
					 none, [], 'test-nodename'
					),


    %% proxy_add_via(Header, URI, Parameters, Proto)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "proxy_add_via/1 - 0"),
    PAV_Header = keylist:delete("Via", ReqHeader),

    %% test proxy_add_via/1 with no present Via header
    autotest:mark(?LINE, "proxy_add_via/1 - 1.2"),
    PAVheader1 = proxy_add_via(PAV_Header, InURI,
			       ["branch=z9hG4bK-test"], tcp6),
    PAV_TopVia1 = sipheader:topvia(PAVheader1),

    %% get branch from added Via, branch computated depends on your hostname
    %% so we can't check for a static one
    autotest:mark(?LINE, "proxy_add_via/1 - 1.3"),
    Branch1 = sipheader:get_via_branch_full(PAV_TopVia1),

    %% basic check the top via that was added
    autotest:mark(?LINE, "proxy_add_via/1 - 1.4"),
    #via{proto="SIP/2.0/TCP", host=MyHostname, port=SipLPort,
	 param=["branch=" ++ Branch1]} = PAV_TopVia1,


    autotest:mark(?LINE, "proxy_add_via/1 - 2.1"),
    PAV_HeaderIn2 = keylist:set("Via", sipheader:via_print([TLSBasicVia]), PAV_Header),

    %% test proxy_add_via/1 with an existing Via header
    autotest:mark(?LINE, "proxy_add_via/1 - 2.2"),
    PAV_Header2 = proxy_add_via(PAV_HeaderIn2, InURI,
				["branch=z9hG4bK-test"], tcp6),

    %% check the two via's that should be present in PAVheader2
    autotest:mark(?LINE, "proxy_add_via/1 - 2.3"),
    Branch2 = sipheader:get_via_branch_full(sipheader:topvia(PAV_Header2)),
    PAV_TopVia1_NewLoopCookie = PAV_TopVia1#via{param=["branch=" ++ Branch2]},
    [PAV_TopVia1_NewLoopCookie, TLSBasicVia] = sipheader:via(PAV_Header2),


    %% test with a branch that already contains a loop cookie
    autotest:mark(?LINE, "proxy_add_via/1 - 3.1"),
    PAV_Header3 = proxy_add_via(PAV_Header, InURI,
				["branch=z9hG4bK-yxa-foo-oLOOPCOOKIE"], tcp6),
    PAV_TopVia3 = sipheader:topvia(PAV_Header3),

    %% get branch from added Via, branch computated depends on your hostname
    %% so we can't check for a static one
    autotest:mark(?LINE, "proxy_add_via/1 - 3.2"),
    "z9hG4bK-yxa-foo-oLOOPCOOKIE" = sipheader:get_via_branch_full(PAV_TopVia3),


    autotest:mark(?LINE, "proxy_add_via/1 - 4.1"),
    PAV_HeaderIn4 = keylist:set("Via", sipheader:via_print([TLSBasicVia]), PAV_Header),

    autotest:mark(?LINE, "proxy_add_via/1 - 4.2"),
    %% test without a supplied branch - a stateless branch should be added
    PAV_Header4 = proxy_add_via(PAV_HeaderIn4, InURI, [], udp),
    PAV_TopVia4 = sipheader:topvia(PAV_Header4),

    %% get branch from added Via, branch computated depends on your hostname
    %% so we can't check for a static one
    autotest:mark(?LINE, "proxy_add_via/1 - 4.3"),
    "z9hG4bK-yxa-" ++ PAV_Branch4 = sipheader:get_via_branch_full(PAV_TopVia4),

    %% Make sure there is a loop cookie in the stateless branch that was added
    autotest:mark(?LINE, "proxy_add_via/1 - 4.4"),
    true = branch_contains_loopcookie("z9hG4bK-yxa-" ++ PAV_Branch4),


    %% test proxy_check_maxforwards(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "proxy_check_maxforwards/1 - 1"),
    %% check that we add the default specified in RFC3261 to header that has no Max-Forwards
    MaxFwd_H1 = proxy_check_maxforwards(keylist:from_list([])),
    ["70"] = keylist:fetch('max-forwards', MaxFwd_H1),

    autotest:mark(?LINE, "proxy_check_maxforwards/1 - 2"),
    %% check that we decrease Max-Forwards by one (normal case)
    MaxFwd_H2 = proxy_check_maxforwards(MaxFwd_H1),
    ["69"] = keylist:fetch('max-forwards', MaxFwd_H2),

    autotest:mark(?LINE, "proxy_check_maxforwards/1 - 3"),
    %% check that we don't allow overly large Max-Forwards
    MaxFwd_H3_in = keylist:set("Max-Forwards", ["500"], MaxFwd_H1),
    MaxFwd_H3 = proxy_check_maxforwards(MaxFwd_H3_in),
    ["255"] = keylist:fetch('max-forwards', MaxFwd_H3),

    autotest:mark(?LINE, "proxy_check_maxforwards/1 - 4"),
    %% check that we don't refuse Max-Forwards: 2
    MaxFwd_H4_in = keylist:set("Max-Forwards", ["2"], MaxFwd_H1),
    MaxFwd_H4 = proxy_check_maxforwards(MaxFwd_H4_in),
    ["1"] = keylist:fetch('max-forwards', MaxFwd_H4),

    autotest:mark(?LINE, "proxy_check_maxforwards/1 - 5"),
    %% check that we don't reach 0
    {siperror, 483, _} = (catch proxy_check_maxforwards(MaxFwd_H4)),

    autotest:mark(?LINE, "proxy_check_maxforwards/1 - 6"),
    %% check that we don't allow 0
    MaxFwd_H6 = keylist:set("Max-Forwards", ["0"], MaxFwd_H1),
    {siperror, 483, _} = (catch proxy_check_maxforwards(MaxFwd_H6)),

    autotest:mark(?LINE, "proxy_check_maxforwards/1 - 7"),
    %% check that we don't allow negative numbers
    MaxFwd_H7 = keylist:set("Max-Forwards", ["-1"], MaxFwd_H1),
    {siperror, 483, _} = (catch proxy_check_maxforwards(MaxFwd_H7)),


    %% standardcopy(Header, ExtraHeaders)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "standardcopy/2 - 1.1"),
    SC_H1 = standardcopy(ReqHeader, []),

    %% verify results
    autotest:mark(?LINE, "standardcopy/2 - 1.2"),
    ["SIP/2.0/TLS 130.237.90.1:111", "SIP/2.0/TCP 2001:6b0:5:987::1"] = keylist:fetch('via', SC_H1),
    ["<sip:test@it.su.se>;tag=f-123"] = keylist:fetch('from', SC_H1),
    ["<sip:test@it.su.se>;tag=t-123"] = keylist:fetch('to', SC_H1),
    ["abc123@test"] = keylist:fetch('call-id', SC_H1),
    ["4711 INVITE"] = keylist:fetch('cseq', SC_H1),

    autotest:mark(?LINE, "standardcopy/2 - 1.3"),
    SC_H1_1 = keylist:delete('via',	SC_H1),
    SC_H1_2 = keylist:delete('from',	SC_H1_1),
    SC_H1_3 = keylist:delete('to',	SC_H1_2),
    SC_H1_4 = keylist:delete('call-id',	SC_H1_3),
    SC_H1_5 = keylist:delete('cseq',	SC_H1_4),
    %% verify that no more headers than the ones we expected were copied
    SC_H1_5 = keylist:from_list([]),

    autotest:mark(?LINE, "standardcopy/2 - 2.1"),
    %% test standardcopy with one extra header
    SC_H2_ExtraHeader = [{"Subject", keylist:fetch("subject", ReqHeader)}],
    SC_H2 = standardcopy(ReqHeader, SC_H2_ExtraHeader),

    %% verify results
    autotest:mark(?LINE, "standardcopy/2 - 2.2"),
    ["test subject short form"] = keylist:fetch("subject", SC_H2),

    autotest:mark(?LINE, "standardcopy/2 - 2.3"),
    SC_H2_1 = keylist:delete("subject", SC_H2),
    %% verify that no more headers than the ones we expected were copied
    SC_H2_1 = SC_H1,


    %% set_request_body(Request, Body)
    %%--------------------------------------------------------------------
    SReqB_Header1 = keylist:from_list([{"Content-Length", ["1"]}]),
    SReqB_Header2 = keylist:from_list([]),

    autotest:mark(?LINE, "set_request_body/2 - 1.1"),
    %% set a binary body
    #request{header=SReqB_Header1_1, body = <<"test">>} =
	set_request_body(#request{header = SReqB_Header1, body = <<>>}, <<"test">>),

    autotest:mark(?LINE, "set_request_body/2 - 1.2"),
    %% verify that Content-Length was set correctly
    ["4"] = keylist:fetch('content-length', SReqB_Header1_1),

    autotest:mark(?LINE, "set_request_body/2 - 2.1"),
    %% set a list body - list bodies are kept for backwards compatibility
    #request{header=SReqB_Header2_1, body = <<"test">>} =
	set_request_body(#request{header = SReqB_Header2, body = <<>>}, "test"),

    autotest:mark(?LINE, "set_request_body/2 - 2.2"),
    %% verify that Content-Length was set correctly
    ["4"] = keylist:fetch('content-length', SReqB_Header2_1),

    autotest:mark(?LINE, "set_request_body/2 - 3.1"),
    %% delete body
    #request{header=SReqB_Header3_1, body = <<>>} =
	set_request_body(#request{header = SReqB_Header1, body = <<"test">>}, <<>>),

    autotest:mark(?LINE, "set_request_body/2 - 2.2"),
    %% verify that Content-Length was set correctly
    ["0"] = keylist:fetch('content-length', SReqB_Header3_1),


    %% set_response_body(Request, Body)
    %%--------------------------------------------------------------------
    SResB_Header1 = keylist:from_list([{"Content-Length", ["1"]}]),
    SResB_Header2 = keylist:from_list([]),

    autotest:mark(?LINE, "set_response_body/2 - 1.1"),
    %% set a binary body
    #response{header=SResB_Header1_1, body = <<"test">>} =
	set_response_body(#response{header = SResB_Header1, body = <<>>}, <<"test">>),

    autotest:mark(?LINE, "set_response_body/2 - 1.2"),
    %% verify that Content-Length was set correctly
    ["4"] = keylist:fetch('content-length', SResB_Header1_1),

    autotest:mark(?LINE, "set_response_body/2 - 2.1"),
    %% set a list body - list bodies are kept for backwards compatibility
    #response{header=SResB_Header2_1, body = <<"test">>} =
	set_response_body(#response{header = SResB_Header2, body = <<>>}, "test"),

    autotest:mark(?LINE, "set_response_body/2 - 2.2"),
    %% verify that Content-Length was set correctly
    ["4"] = keylist:fetch('content-length', SResB_Header2_1),

    autotest:mark(?LINE, "set_response_body/2 - 3.1"),
    %% delete body
    #response{header=SResB_Header3_1, body = <<>>} =
	set_response_body(#response{header = SResB_Header1, body = <<"test">>}, <<>>),

    autotest:mark(?LINE, "set_response_body/2 - 2.2"),
    %% verify that Content-Length was set correctly
    ["0"] = keylist:fetch('content-length', SResB_Header3_1),


    %% fix_content_length(Header, Body)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "fix_content_length/2 - 1"),
    %% binary body, no Content-Length header
    ["4"] = keylist:fetch('content-length', fix_content_length(
					      keylist:from_list([]),
					      <<"test">>)
			 ),

    autotest:mark(?LINE, "fix_content_length/2 - 2"),
    %% list body, incorrect Content-Length header
    ["4"] = keylist:fetch('content-length', fix_content_length(
					      keylist:from_list([{"Content-Length", ["600"]}]),
					      "test")
			 ),

    autotest:mark(?LINE, "fix_content_length/2 - 3"),
    %% delete body, incorrect Content-Length header
    ["0"] = keylist:fetch('content-length', fix_content_length(
					      keylist:from_list([{"Content-Length", ["600"]}]),
					      <<>>)
					     ),


    %% proxyauth_without_response(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "proxyauth_without_response/1 - 1"),
    PAWR_Str1 =
	"Digest username=\"ft\", realm=\"su.se\", uri=\"sip:example.org\", "
	"response=\"test-response\", nonce=\"test-nonce\", opaque=\"test-opaque\", "
	"algorithm=md5",

    ["algorithm=md5", "realm=su.se", "uri=sip:example.org", "username=ft"] =
	lists:sort( proxyauth_without_response(
		      keylist:from_list([{"Proxy-Authorization", [PAWR_Str1]}])
		     )),

    autotest:mark(?LINE, "proxyauth_without_response/1 - 2"),
    none = proxyauth_without_response( keylist:from_list([]) ),

    ok.
