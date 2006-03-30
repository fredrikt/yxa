-module(outgoingproxy).

%%--------------------------------------------------------------------
%%% Standard Yxa SIP-application callback functions
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/3,
	 response/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").


%%====================================================================
%% Behaviour functions
%% Standard Yxa SIP-application callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init()
%% Descrip.: Yxa applications must export an init/0 function.
%% Returns : [Tables, Mode, SupData]
%%           Tables  = list() of atom(), remote mnesia tables the Yxa
%%                     startup sequence should make sure are available
%%           Mode    = stateful
%%           SupData = {append, SupSpec} |
%%                     none
%%           SupSpec = OTP supervisor child specification. Extra
%%                     processes this application want the
%%                     sipserver_sup to start and maintain.
%%--------------------------------------------------------------------
init() ->
    [[user, numbers, phone], stateful, none].

%%--------------------------------------------------------------------
%% Function: request(Request, Origin, LogStr)
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogStr  = string() describing request
%% Descrip.: Yxa applications must export an request/3 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------


%%
%% REGISTER
%%
request(#request{method="REGISTER"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case siplocation:process_register_request(Request, THandler, LogTag, LogStr, outgoingproxy) of
	not_homedomain ->
	    case yxa_config:get_env(allow_foreign_registers) of
		{ok, true} ->
		    do_foreign_register(Request, Origin);
		{ok, false} ->
		    transactionlayer:send_response_handler(THandler, 403, "Domain not handled by this proxy")
	    end;
	_ ->
	    true
    end,
    ok;

%%
%% ACK
%%
request(#request{method="ACK"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    logger:log(normal, "outgoingproxy: ~s -> Forwarding ACK received in core statelessly",
	       [LogStr]),
    transportlayer:stateless_proxy_request("outgoingproxy", Request),
    ok;

%%
%% Anything but REGISTER and ACK
%%
request(Request, Origin, _LogStr) when is_record(Request, request), is_record(Origin, siporigin) ->
    do_request(Request, Origin).

%% Note: All the RFC quotes in this function are from RFC3327 #5.2 (Procedures at Intermediate Proxies)
do_foreign_register(Request, Origin) ->
    #request{uri    = URI,
	     header = Header
	    } = Request,

    %% We do Path (RFC3327) if the UA said it supports it, or if we are configured to always do Path
    %% for foreign registrations

    DoPath =
	case sipheader:is_supported("path", Header) of
	    true -> {ok, true};
	    false ->
		case yxa_config:get_env(always_do_path_for_foreign_registers) of
		    {ok, true} ->
			%% "Intermediate proxies SHOULD NOT add a Path header field to a request
			%% unless the UA has indicated support for this extension with a
			%% Supported header field value."
			logger:log(debug, "outgoingproxy: Notice: REGISTER request to foreign domain (~p), "
				   "using Path even though UA did not say it supports it", [URI#sipurl.host]),
			true;
		    {ok, false} ->
			false
		end
	end,

    case DoPath of
	true ->
	    RRStr = siprequest:construct_record_route(URI#sipurl.proto),
	    NewHeader1 = keylist:prepend({"Path", [RRStr]}, Header),
	    NewHeader =
		case sipheader:is_required("path", NewHeader1) of
		    true ->
			NewHeader1;
		    false ->
			%% "If the UA has indicated support and
			%% the proxy requires the registrar to support the Path extension, then
			%% the proxy SHOULD insert a Requires header field value for this
			%% extension."
			%% We do require it, since the only point of using an outgoingproxy is that you
			%% must get all incoming requests through an existing network flow.
			keylist:append({"Require", ["path"]}, NewHeader1)
		end,
	    do_request(Request#request{header = NewHeader}, Origin);
	false ->
	    %% "If the UA has not indicated support for the extension and
	    %% the proxy requires support for it in the registrar, the proxy SHOULD
	    %% reject the request with a 421 response indicating a requirement for
	    %% the extension."
	    THandler = transactionlayer:get_handler_for_request(Request),
	    transactionlayer:send_response_handler(THandler, 421, "Extension Required",
						  [{"Require", ["path"]}])
    end.
	

%%--------------------------------------------------------------------
%% Function: response(Response, Origin, LogStr)
%%           Request = response record()
%%           Origin  = siporigin record()
%%           LogStr  = string(), description of response
%% Descrip.: Yxa applications must export an response/3 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------
response(Response, Origin, LogStr) when is_record(Response, response), is_record(Origin, siporigin) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(normal, "outgoingproxy: Response to ~s: '~p ~s', no matching transaction - proxying statelessly",
	       [LogStr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

do_request(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    {Method, URI} = {Request#request.method, Request#request.uri},

    %% outgoingproxys need to add Record-Route header to make sure in-dialog requests go
    %% through the proxy.

    Location = route_request(Request),
    logger:log(debug, "outgoingproxy: Location: ~p", [Location]),
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case Location of
	none ->
	    logger:log(normal, "~s: outgoingproxy: 403 Forbidden", [LogTag]),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden");

	{response, Status, Reason} ->
	    logger:log(normal, "~s: outgoingproxy: Response '~p ~s'", [LogTag, Status, Reason]),
	    transactionlayer:send_response_handler(THandler, Status, Reason);

	{forward, FwdURL} when is_record(FwdURL, sipurl), FwdURL#sipurl.user == none, FwdURL#sipurl.pass == none ->
	    logger:log(normal, "~s: outgoingproxy: Forward ~s ~s to ~s",
		       [LogTag, Method, sipurl:print(URI), sipurl:print(FwdURL)]),
	    ApproxMsgSize = siprequest:get_approximate_msgsize(Request#request{uri=FwdURL}),
	    case sipdst:url_to_dstlist(FwdURL, ApproxMsgSize, URI) of
		{error, nxdomain} ->
		    logger:log(debug, "outgoingproxy: Failed resolving FwdURL : NXDOMAIN"
			       " (responding '604 Does Not Exist Anywhere')"),
                    transactionlayer:send_response_handler(THandler, 604, "Does Not Exist Anywhere"),
                    error;
		{error, What} ->
		    logger:log(normal, "outgoingproxy: Failed resolving FwdURL : ~p", [What]),
                    transactionlayer:send_response_handler(THandler, 500, "Failed resolving forward destination"),
                    error;
		DstList when is_list(DstList) ->
		    proxy_request(THandler, Request, DstList)
	    end;
	{proxy, Loc} when is_record(Loc, sipurl) ->
	    logger:log(normal, "~s: outgoingproxy: Proxy ~s -> ~s", [LogTag, Method, sipurl:print(Loc)]),
	    proxy_request(THandler, Request, Loc);
	{relay, Loc} ->
	    relay_request(THandler, Request, Loc, Origin, LogTag);
	me ->
	    request_to_me(THandler, Request, LogTag);
	_ ->
	    logger:log(error, "~s: outgoingproxy: Invalid Location ~p", [LogTag, Location]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.

%%--------------------------------------------------------------------
%% Function: route_request(Request)
%%           Request = request record()
%% Descrip.:
%% Returns : {error, Status}            |
%%           {response, Status, Reason} |
%%           {proxy, Location}          |
%%           {relay, Location}          |
%%           {forward, Location}        |
%%           me                         |
%%           none
%%--------------------------------------------------------------------
route_request(Request) when is_record(Request, request) ->
    case keylist:fetch('route', Request#request.header) of
	[] ->
	    URI = Request#request.uri,
	    case local:is_request_to_this_proxy(Request) of
		true ->
		    case url_param:find(URI#sipurl.param_pairs, "addr") of
			[Addr] ->
			    %% XXX we should verify that this really is a location stored
			    %% in the location database and not an attempt to relay SIP
			    %% requests through this proxy
			    logger:log(normal, "outgoingproxy: Decoded real destination ~p", [Addr]),
			    {proxy, sipurl:parse(Addr)};
			[] ->
			    me
		    end;
		false ->
		    case local:get_user_with_contact(URI) of
			none ->
			    case yxa_config:get_env(sipproxy) of
				{ok, DefaultProxy} when is_record(DefaultProxy, sipurl) ->
				    {forward, DefaultProxy};
				none ->
				    none
			    end;
			SIPuser when is_list(SIPuser) ->
			    logger:log(debug, "outgoingproxy: Request destination ~p is a registered "
				       "contact of user ~p - proxying", [URI, SIPuser]),
			    {proxy, URI}
		    end
	    end;
	_Route ->
	    %% Request has Route header
	    logger:log(debug, "Routing: Request has Route header, following Route."),
	    {relay, route}
    end.

%%--------------------------------------------------------------------
%% Function: request_to_me(THandler, Request, LogTag)
%%           THandler = term(), server transaction handle
%%           Request  = request record()
%%           LogTag   = string()
%% Descrip.: Request is meant for this proxy, if it is OPTIONS we
%%           respond 200 Ok, otherwise we respond 481 Call/
%%           transaction does not exist.
%% Returns : Does not matter.
%%--------------------------------------------------------------------
request_to_me(THandler, Request, LogTag) when is_record(Request, request), Request#request.method == "OPTIONS" ->
    logger:log(normal, "~s: incomingproxy: OPTIONS to me -> 200 OK", [LogTag]),
    %% XXX The OPTIONS response SHOULD include Accept, Accept-Encoding, Accept-Language, and 
    %% Supported headers. RFC 3261 section 11.
    transactionlayer:send_response_handler(THandler, 200, "OK");

request_to_me(THandler, Request, LogTag) when is_record(Request, request) ->
    logger:log(normal, "~s: incomingproxy: non-OPTIONS request to me -> 481 Call/Transaction Does Not Exist",
	       [LogTag]),
    transactionlayer:send_response_handler(THandler, 481, "Call/Transaction Does Not Exist").

%%--------------------------------------------------------------------
%% Function: get_branch_from_handler(TH)
%%           TH = term(), server transaction handle
%% Descrip.: Get branch from server transaction handler and then
%%           remove the -UAS suffix. The result is used as a tag
%%           when logging actions.
%% Returns : Branch
%%--------------------------------------------------------------------
get_branch_from_handler(TH) ->
    CallBranch = transactionlayer:get_branch_from_handler(TH),
    case string:rstr(CallBranch, "-UAS") of
	0 ->
	    CallBranch;
	Index when is_integer(Index) ->
	    BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
    end.


%%--------------------------------------------------------------------
%% Function: proxy_request(THandler, Request, DstList)
%%           THandler = term(), server transaction handle
%%           Request = request record()
%%           DstList = list() of sipdst record() | sipurl record() |
%%                     route
%% Descrip.: Proxy a request somewhere without authentication.
%% Returns : Does not matter
%%--------------------------------------------------------------------
proxy_request(THandler, Request, DstList) when is_record(Request, request) ->
    sippipe:start(THandler, none, Request, DstList, 900).

%%--------------------------------------------------------------------
%% Function: relay_request(THandler, Request, Dst, Origin, LogTag)
%%           THandler = term(), server transaction handle
%%           Request  = request record()
%%           Dst      = sipdst record() | sipurl record() | route
%%           Origin   = siporigin record()
%%           LogTag   = string(), prefix for logging
%% Descrip.: Relay request to remote host. If there is not valid
%%           credentials present in the request, challenge user
%%           unless local policy says not to. Never challenge
%%           CANCEL or BYE since they can't be resubmitted and
%%           therefor cannot be challenged.
%% Returns : Does not matter
%%--------------------------------------------------------------------

%%
%% CANCEL or BYE
%%
relay_request(THandler, #request{method=Method}=Request, Dst, _Origin, LogTag)
  when Method == "CANCEL"; Method == "BYE" ->
    logger:log(normal, "~s: outgoingproxy: Relay ~s ~s (unauthenticated)",
	       [LogTag, Request#request.method, sipurl:print(Request#request.uri)]),
    sippipe:start(THandler, none, Request, Dst, 900);

%%
%% Anything but CANCEL or BYE
%%
relay_request(THandler, Request, Dst, Origin, LogTag) when is_record(Request, request) ->
    {Method, Header} = {Request#request.method, Request#request.header},
    case sipauth:get_user_verified_proxy(Header, Method) of
	{authenticated, User} ->
	    logger:log(debug, "Relay: User ~p is authenticated", [User]),
	    logger:log(normal, "~s: outgoingproxy: Relay ~s (authenticated)", [LogTag, relay_dst2str(Dst)]),
	    sippipe:start(THandler, none, Request, Dst, 900);
	{stale, User} ->
	    case local:outgoingproxy_challenge_before_relay(Origin, Request, Dst) of
		false ->
		    logger:log(debug, "Relay: STALE authentication (user ~p), but local policy says we "
			       "should not challenge", [User]),
		    sippipe:start(THandler, none, Request, Dst, 900);
		true ->
		    logger:log(debug, "Relay: STALE authentication, sending challenge"),
		    logger:log(normal, "~s: outgoingproxy: Relay ~s -> STALE authentication (user ~p) ->"
			       " 407 Proxy Authentication Required",
			       [LogTag, relay_dst2str(Dst), User]),
		    transactionlayer:send_challenge(THandler, proxy, true, none)
	    end;
	false ->
            case local:outgoingproxy_challenge_before_relay(Origin, Request, Dst) of
                false ->
                    logger:log(debug, "Relay: Failed authentication, but local policy says we should not challenge"),
                    sippipe:start(THandler, none, Request, Dst, 900);
                true ->
		    logger:log(debug, "Relay: Failed authentication, sending challenge"),
		    logger:log(normal, "~s: outgoingproxy: Relay ~s -> 407 Proxy Authorization Required",
			       [LogTag, relay_dst2str(Dst)]),
		    transactionlayer:send_challenge(THandler, proxy, false, none)
	    end
    end.

relay_dst2str(URI) when is_record(URI, sipurl) ->
    sipurl:print(URI);
relay_dst2str(route) ->
    "according to Route header";
relay_dst2str(_) ->
    "unknown dst".
