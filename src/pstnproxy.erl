%%%-------------------------------------------------------------------
%%% File    : pstnproxy.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: An SIP application level 'firewall' to defend a PSTN
%%%           gateway from misuse and to make sure users are
%%%           authorized to make calls to different 'classes' of
%%%           numbers (configured through regular expressions). Also
%%%           perform ENUM lookups on requests _from_ the PSTN gw.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(pstnproxy).

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
    [[user, numbers], stateful, none].


%%--------------------------------------------------------------------
%% Function: request(Request, Origin, LogStr)
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogStr  = string(), description of response
%% Descrip.: Yxa applications must export an request/3 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------

%%
%% ACK
%%
request(#request{method="ACK"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    %% ACK requests that end up here could not be matched to a server transaction,
    %% most probably they are ACK to 2xx of INVITE (or we have crashed) - proxy
    %% statelessly.
    logger:log(normal, "pstnproxy: ~s -> Forwarding ACK received in core statelessly",
	       [LogStr]),
    transportlayer:stateless_proxy_request("incomingproxy", Request),
    ok;

%%
%% Anything except ACK
%%
request(Request, Origin, _LogStr) when is_record(Request, request), is_record(Origin, siporigin) ->
    Method = Request#request.method,
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case route_request(Request, Origin, THandler, LogTag) of
	pstn ->
	    logger:log(debug, "~s: pstnproxy: Route request to PSTN gateway", [LogTag]),
	    {ok, AllowedMethods} = yxa_config:get_env(allowed_request_methods),
	    case lists:member(Method, AllowedMethods) of
		true ->
		    request_to_pstn(Request, Origin, THandler, LogTag);
		_ ->
		    logger:log(normal, "~s: pstnproxy: Method ~s not allowed for PSTN destination",
			       [LogTag, Method]),
		    ExtraHeaders = [{"Allow", AllowedMethods}],
		    transactionlayer:send_response_handler(THandler, 405, "Method Not Allowed", ExtraHeaders)
	    end;
	sip ->
	    logger:log(normal, "~s: pstnproxy: Route request to SIP server", [LogTag]),
	    request_to_sip(Request, THandler);
	me ->
	    request_to_me(THandler, Request, LogTag);
	drop ->
	    ok
    end,
    ok.


%%--------------------------------------------------------------------
%% Function: response(Response, Origin, LogStr)
%%           Response = response record()
%%           Origin   = siporigin record()
%%           LogStr   = string(), description of response
%% Descrip.: Yxa applications must export an response/3 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------
response(Response, Origin, LogStr) when is_record(Response, response), is_record(Origin, siporigin) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(normal, "pstnproxy: Response to ~s: ~p ~s, no matching transaction - proxying statelessly",
	       [LogStr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response),
    ok.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: route_request(FromIP, ToHost, THandler, LogTag)
%%           FromIP   = string()
%%           ToHost   = string(), Host part of Request-URI
%%           THandler = term(), server transaction handle
%%           LogTag   = string(), prefix for logging
%% Descrip.: Determines if we should route this request to one of
%%           our PSTN gateways or not.
%% Returns : pstn |
%%           sip  |
%%           me   |
%%           drop
%%--------------------------------------------------------------------
route_request(Request, Origin, THandler, LogTag) when is_record(Request, request), is_record(Origin, siporigin) ->
    case local:is_request_to_this_proxy(Request) of
	true ->
	    me;
	false ->
	    route_request2(Origin#siporigin.addr, Request, THandler, LogTag)
    end.

route_request2(FromIP, Request, THandler, LogTag) ->
    ToHost = (Request#request.uri)#sipurl.host,
    case is_pstngateway(FromIP) of
	true ->
	    logger:log(debug, "Routing: Source IP ~s is PSTN gateway, route to SIP proxy", [FromIP]),
	    sip;
	false ->
	    IsLocalHostname = is_localhostname(ToHost),
	    IsPstnGatewayHostname = is_pstngateway(ToHost),
	    FirstRouteIsPstnGateway = first_route_is_pstngateway(Request#request.header),
	    if
		IsLocalHostname ->
		    logger:log(debug, "Routing: Source IP ~s is not PSTN gateway and ~p is me, route to PSTN gateway",
			       [FromIP, ToHost]),
		    pstn;
		IsPstnGatewayHostname  ->
		    logger:log(debug, "Routing: Source IP ~s is not PSTN gateway and ~p is me, route to PSTN gateway",
			       [FromIP, ToHost]),
		    pstn;
		FirstRouteIsPstnGateway ->
		    logger:log(debug, "Routing: First Route-header is PSTN gateway, route to PSTN gateway"),
		    pstn;
		true ->
		    logger:log(normal, "~s: pstnproxy: Denied request from ~s to ~s - not my problem",
			       [LogTag, FromIP, ToHost]),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden"),
		    drop
	    end
    end.


%%--------------------------------------------------------------------
%% Function: is_localhostname(Hostname)
%%           Hostname = string()
%% Descrip.: Check if given hostname matches one of ours, or one of
%%           our IP addresses.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
is_localhostname(Hostname) ->
    LChost = http_util:to_lower(Hostname),
    {ok, MyHostnames} = yxa_config:get_env(myhostnames),
    case lists:member(LChost, MyHostnames) of
	true ->
	    true;
	false ->
	    lists:member(LChost, siphost:myip_list())
    end.


%%--------------------------------------------------------------------
%% Function: is_pstngateway(Hostname)
%%           Hostname = string()
%% Descrip.: Check if given hostname matches one of our PSTN
%%           gateways hostnames.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
is_pstngateway(Hostname) ->
    LChost = http_util:to_lower(Hostname),
    {ok, MyHostnames} = yxa_config:get_env(pstngatewaynames),
    lists:member(LChost, MyHostnames).


%%--------------------------------------------------------------------
%% Function: first_route_is_pstngateway(Header)
%%           Header = keylist record()
%% Descrip.: Check if there is a Route-header in Header, and if it is
%%           then check if the first Route matches one of our PSTN
%%           gateways hostnames.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
first_route_is_pstngateway(Header) ->
    case keylist:fetch('route', Header) of
	[FirstRoute | _] ->
	    [FirstRouteParsed] = contact:parse([FirstRoute]),
	    RouteURL = sipurl:parse(FirstRouteParsed#contact.urlstr),
	    is_pstngateway(RouteURL#sipurl.host);
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: request_to_sip(Request, THandler)
%%           Request  = request record()
%%           THandler = term(), server transaction handle
%% Descrip.: It has been determined that we should send this
%%           request to a VoIP destination (i.e. anything else than
%%           one of our PSTN gateways). If the host part of the
%%           request URI matches this proxy, then do an ENUM lookup
%%           on the user part of the request URI, if it looks like
%%           a phone number.
%% Returns : Does not matter
%%--------------------------------------------------------------------
request_to_sip(Request, THandler) when is_record(Request, request) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    Dest = case is_localhostname(URI#sipurl.host) of
	       true ->
		   %% Hostname matches me
		   determine_sip_location(URI);
	       false ->
		   {proxy, URI}
	   end,
    case Dest of
	{proxy, NewURI} when is_record(NewURI, sipurl) ->
	    NewHeader = add_caller_identity_for_sip(Method, Request#request.header),
	    NewRequest = Request#request{header=NewHeader},
	    proxy_request(THandler, NewRequest, restore_sips_proto(URI, NewURI));
	{reply, Status, Reason} ->
	    transactionlayer:send_response_handler(THandler, Status, Reason);
	{reply, Status, Reason, ExtraHeaders} ->
	    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders);
	none ->
	    none
    end.

%%--------------------------------------------------------------------
%% Function: determine_sip_location(URI)
%%           URI = sipurl record()
%% Descrip.: Find out how to act on a request from PSTN to SIP.
%% Returns : {reply, Status, Reason}               |
%%           {reply, Status, Reason, ExtraHeaders} |
%%           {proxy, NewURI}                       |
%%           none
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           ExtraHeaders = list() of {Key, Value} tuple()
%%           NewURI = sipurl record()
%%--------------------------------------------------------------------
determine_sip_location(URI) when is_record(URI, sipurl) ->
    User = URI#sipurl.user,
    logger:log(debug, "pstnproxy: Performing ENUM lookup on ~p", [User]),
    %% XXX handle {proxy, Loc} return from lookupenum? Should never happen in pstnproxy...
    case local:lookupenum(User) of
	{relay, Loc} when is_record(Loc, sipurl) ->
	    case yxa_config:get_env(pstnproxy_redirect_on_enum) of
		{ok, true} ->
		    %% Redirect caller to the destination we found in ENUM instead of
		    %% proxying the request.
		    Contact = contact:new(none, Loc, []),
		    ExtraHeaders = [{"Contact", sipheader:contact_print([Contact])}],
		    {reply, 302, "Moved Temporarily", ExtraHeaders};
		{ok, false} ->
		    {proxy, Loc}
	    end;
	none ->
	    case yxa_config:get_env(sipproxy) of
		{ok, ProxyURL1} when is_record(ProxyURL1, sipurl) ->
		    %% Use configured sipproxy but exchange user with user from Request-URI
		    ProxyURL = sipurl:set([{user, User},
					   {pass, none}
					  ], ProxyURL1),
		    {proxy, ProxyURL};
		none ->
		    %% No ENUM and no SIP-proxy configured, return failure so that the PBX
		    %% on the other side of the gateway can fall back to PSTN or something
		    {ok, Status} = yxa_config:get_env(pstnproxy_no_sip_dst_code),
		    {reply, Status, "No destination found for number " ++ User}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: request_to_pstn(Request, Origin, THandler, LogTag)
%%           Request  = request record()
%%           Origin   = siporigin record()
%%           THandler = term(), server transaction handle
%%           LogTag   = string(), prefix for logging
%% Descrip.: It has been determined that we should send this
%%           request to one of our PSTN gateways. Figure out which
%%           one and make it so.
%% Returns : Does not matter
%%--------------------------------------------------------------------
request_to_pstn(Request, Origin, THandler, LogTag) when is_record(Request, request), is_record(Origin, siporigin) ->
    URI = Request#request.uri,
    {DstNumber, ToHost} = {URI#sipurl.user, URI#sipurl.host},

    Decision =
	case is_localhostname(ToHost) of
	    true ->
		%% XXX check port too, not just hostname? Maybe not since pstnproxy by definition
		%% should be running alone on a host.
		case local:lookuppstn(DstNumber) of
		    {proxy, Dst} when is_record(Dst, sipurl) ->
			{relay, Dst, Request};
		    {relay, Dst} when is_record(Dst, sipurl) ->
			{relay, Dst, Request};
		    none ->
			request_to_pstn_non_e164(DstNumber, Request, Origin, THandler)
		end;
	    false ->
		{relay, URI, Request}
	end,

    case Decision of
	{relay, DstURI, NewRequest} when is_record(DstURI, sipurl), is_record(NewRequest, request) ->
	    relay_request_to_pstn(THandler, NewRequest, restore_sips_proto(URI, DstURI), DstNumber, LogTag);
	ignore ->
	    ok
    end.

%%--------------------------------------------------------------------
%% Function: request_to_pstn_non_e164(DstNumber, Request, Origin,
%%                                    THandler)
%%           DstNumber = string(), PSTN phone number
%%           Request   = request record()
%%           Origin    = siporigin record()
%%           THandler  = term(), server transaction handle
%% Descrip.: Part of request_to_pstn/4. Determine what to do with a
%%           request which was not recognized by local:lookuppstn/1.
%% Returns : Relay | ignore
%%           Relay = {relay, DstURI, NewRequest}
%%--------------------------------------------------------------------
request_to_pstn_non_e164(DstNumber, Request, Origin, THandler) ->
    case local:pstnproxy_route_pstn_not_e164(DstNumber, Request, Origin, THandler) of
	undefined ->
	    %% Route to default PSTN gateway
	    case yxa_config:get_env(default_pstngateway) of
		{ok, GwURL} when is_record(GwURL, sipurl) ->
		    logger:log(debug, "pstnproxy: Routing request to default PSTN gateway ~p", [sipurl:print(GwURL)]),
		    NewURI = sipurl:set([{user, DstNumber},
					 {pass, none}
					], GwURL),
		    {relay, NewURI, Request};
		none ->
		    logger:log(debug, "pstnproxy: Found no destination for request to PSTN, number ~p. "
			       "Answering '404 Not Found'.", [DstNumber]),
		    transactionlayer:send_response_handler(THandler, 404, "Not Found"),
		    ignore
	    end;
	nomatch ->
	    logger:log(debug, "pstnproxy: local:pstnproxy_route_pstn_not_e164(...) says there is no "
		       "destination for request to PSTN, number ~p. Answering '404 Not Found'.", [DstNumber]),
	    transactionlayer:send_response_handler(THandler, 404, "Not Found"),
	    ignore;
	Res ->
	    Res
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
request_to_me(THandler, #request{method="OPTIONS"}=Request, LogTag) when is_record(Request, request) ->
    logger:log(normal, "~s: pstnproxy: OPTIONS to me -> 200 OK", [LogTag]),
    logger:log(debug, "XXX The OPTIONS response SHOULD include Accept, Accept-Encoding,"
	       " Accept-Language, and Supported headers. RFC 3261 section 11"),
    transactionlayer:send_response_handler(THandler, 200, "OK");

request_to_me(THandler, Request, LogTag) when is_record(Request, request) ->
    logger:log(normal, "~s: pstnproxy: non-OPTIONS request to me -> 481 Call/Transaction Does Not Exist",
	       [LogTag]),
    transactionlayer:send_response_handler(THandler, 481, "Call/Transaction Does Not Exist").


%%--------------------------------------------------------------------
%% Function: add_caller_identity_for_pstn(Method, Headers, URI,
%%                                        Gateway)
%%           Method  = string()
%%           Headers = keylist record()
%%           URI     = sipurl record(), Request URI of outgoing req.
%%           User    = string(), SIP authentication username
%%           Gateway = string()
%% Descrip.: If configured to, add Remote-Party-Id information
%%           about caller to this request before it is sent to a
%%           PSTN gateway. Useful to get proper caller-id.
%% Returns : NewHeader, keylist record()
%%--------------------------------------------------------------------
add_caller_identity_for_pstn("INVITE", Header, URI, User, Gateway) when is_record(Header, keylist),
									is_record(URI, sipurl),
									is_list(User) ->
    case yxa_config:get_env(remote_party_id) of
	{ok, true} ->
	    case local:get_remote_party_number(User, Header, URI, Gateway) of
		{ok, RPI, Number} when is_record(RPI, contact), is_list(Number) ->
		    RemotePartyId = contact:print(RPI),
		    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
		    NewHeader1 = keylist:set("Remote-Party-Id", [RemotePartyId], Header),
		    logger:log(debug, "P-Preferred-Identity: ~s", ["tel:" ++ Number]),
		    keylist:set("P-Preferred-Identity", ["tel:" ++ Number], NewHeader1);
		none ->
		    %% Add RPI information saying to not show any caller id, in case the From:
		    %% contains something the gateway interprets as a phone number when it shouldn't
		    logger:log(debug, "Remote-Party-Id: Blocking Caller-Id for third party user "
			       "or user without number to avoid incorrect/spoofed A-number in PSTN"),
		    Parameters = [{"party", "calling"}, {"screen", "yes"}, {"privacy", "on"}],
		    RPURI = sipurl:new([{host, siprequest:myhostname()}, {param, []}]),
		    RPI = contact:print( contact:new("Anonymous", RPURI, Parameters) ),
		    keylist:set("Remote-Party-Id", [RPI], Header)
	    end;
	{ok, false} ->
	    Header
    end;
add_caller_identity_for_pstn(_Method, Header, _URI, _User, _Gateway) when is_record(Header, keylist) ->
    %% non-INVITE request, don't add Remote-Party-Id
    Header.


%%--------------------------------------------------------------------
%% Function: add_caller_identity_for_sip(Method, Header)
%%           Method  = string()
%%           Header = keylist record()
%% Descrip.: If configured to, add Remote-Party-Id information
%%           about caller to this request received from one of our
%%           PSTN gateways. Useful to get the name of the person
%%           calling in your phones display - if you have the name
%%           available in some database.
%% Returns : NewHeader, keylist record()
%%--------------------------------------------------------------------
add_caller_identity_for_sip("INVITE", Header) ->
    case yxa_config:get_env(remote_party_id) of
	{ok, true} ->
	    {_, FromURL} = sipheader:from(Header),
	    case local:get_remote_party_name(FromURL#sipurl.user, FromURL) of
		{ok, DisplayName} when is_list(DisplayName) ->
		    Parameters = [{"party", "calling"}, {"screen", "yes"}, {"privacy", "off"}],
		    RPI = contact:new(DisplayName, FromURL, Parameters),
		    RemotePartyId = contact:print(RPI),
		    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
		    keylist:set("Remote-Party-Id", [RemotePartyId], Header);
		none ->
		    Header
	    end;
	{ok, false} ->
	    Header
    end;
add_caller_identity_for_sip(_Method, Header) ->
    %% non-INVITE request, don't add Remote-Party-Id
    Header.


%%--------------------------------------------------------------------
%% Function: get_branch_from_handler(TH)
%%           TH = term(), server transaction handle
%% Descrip.: Get branch from server transaction handler and then
%%           remove the -UAS suffix. The result is used as a tag
%%           when logging actions.
%% Returns : Branch, string()
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
%% Function: proxy_request(THandler, Request, DstURI)
%%           THandler = term(), server transcation handle
%%           Request  = request record()
%%           DstURI   = sipurl record()
%% Descrip.: Proxy a request somewhere without authentication. This is
%%           typically a request _from_ one of our PSTN gateways.
%% Returns : void(), Does not matter
%%--------------------------------------------------------------------
proxy_request(THandler, Request, DstURI) when is_record(Request, request),
					      is_record(DstURI, sipurl) ->
    case keylist:fetch('route', Request#request.header) of
	[] ->
	    sippipe:start(THandler, none, Request, DstURI, 900);
	Route ->
	    %% XXX this is a configurable option only because in SU's setup it
	    %% might break calls through the gateway when PRACKs it sends gets
	    %% challenged elsewhere. Don't set this to true!
	    case yxa_config:get_env(pstnproxy_ignore_route_header_bad_idea) of
		{ok, true} ->
		    logger:log(debug, "Warning: Routing of request according to "
			       "Route header disabled  : ~p - BAD IDEA", [Route]),
		    sippipe:start(THandler, none, Request, DstURI, 900);
		{ok, false} ->
		    sippipe:start(THandler, none, Request, route, 900)
	    end
    end.


%%--------------------------------------------------------------------
%% Function: relay_request_to_pstn(THandler, Request, DstURI,
%%                                 DstNumber, LogTag)
%%           THandler  = term(), server transaction handle
%%           Request   = request record()
%%           DstURI    = sipurl record()
%%           DstNumber = string(), destination number in unspecified
%%                       format (as entered, not E.164)
%%           LogTag    = string(), prefix for logging
%% Descrip.: Relay request to one of our PSTN gateways. If there is
%%           not valid credentials present in the request,
%%           challenge user unless the number is in a class which
%%           does not require authentication. Never challenge
%%           CANCEL or BYE since they can't be resubmitted and
%%           therefor cannot be challenged.
%% Returns : void(), Does not matter
%%--------------------------------------------------------------------

%%
%% CANCEL or BYE
%%
relay_request_to_pstn(THandler, #request{method=Method}=Request, DstURI, DstNumber, LogTag) when Method == "CANCEL";
												 Method == "BYE" ->
    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) (unauthenticated)",
	       [LogTag, Request#request.method, DstNumber, sipurl:print(DstURI)]),
    relay_request_to_pstn_isauth(THandler, Request, DstURI);

%%
%% Anything except CANCEL or BYE
%%
relay_request_to_pstn(THandler, Request, DstURI, DstNumber, LogTag) when is_record(Request, request) ->
    {Method, Header} = {Request#request.method, Request#request.header},
    {_, FromURI} = sipheader:from(Header),
    {ok, Classdefs} = yxa_config:get_env(classdefs),
    logger:log(debug, "~s: pstnproxy: Relay ~s to PSTN ~s (~s)", [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
    case sipauth:pstn_call_check_auth(Method, Header, FromURI, DstNumber, Classdefs) of
	{true, User, Class} ->
	    logger:log(debug, "Auth: User ~p is allowed to call dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (authenticated, user ~p, class ~p) (~s)",
		       [LogTag, Method, DstNumber, User, Class, sipurl:print(DstURI)]),
	    %% Now that we know which user this request is authorized as, add Caller-ID information to it
	    PSTNgateway = DstURI#sipurl.host,
	    NewHeader = add_caller_identity_for_pstn(Method, Header, DstURI, User, PSTNgateway),

	    relay_request_to_pstn_isauth(THandler, Request#request{header = NewHeader}, DstURI);
	{stale, User, Class} ->
	    logger:log(debug, "Auth: User ~p must authenticate (stale) for dst ~s (class ~s)",
		       [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) -> STALE authentication, sending challenge",
		       [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
	    transactionlayer:send_challenge(THandler, proxy, true, none);
	{false, none, Class} ->
	    logger:log(debug, "Auth: Need authentication for dst ~s (class ~s)", [DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) -> needs authentication, sending challenge",
		       [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
	    transactionlayer:send_challenge(THandler, proxy, false, none);
	{false, User, Class} ->
	    logger:log(debug, "Auth: User ~p not allowed dst ~s in class unknown - answer 403 Forbidden",
		       [User, DstNumber]),
	    logger:log(normal, "~s: pstnproxy: User ~p not allowed relay ~s to PSTN ~s class ~p -> 403 Forbidden",
		       [LogTag, User, Method, DstNumber, Class]),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden")
    end.

relay_request_to_pstn_isauth(THandler, Request, DstURI) ->
    case keylist:fetch('route', Request#request.header) of
	[] ->
	    sippipe:start(THandler, none, Request, DstURI, 900);
	_Route ->
	    sippipe:start(THandler, none, Request, route, 900)
    end.

%%--------------------------------------------------------------------
%% Function: restore_sips_proto(OldURL, NewURL)
%%           OldURL = sipurl()
%%           NewURL = sipurl()
%% Descrip.: Whenever we have constructed a brand new URL to use as a
%%           destination, we can use this function to easily make sure
%%           we didn't downgrade SIPS to SIP (which RFC3261 forbids).
%% Returns : URL = sipurl record()
%%--------------------------------------------------------------------
restore_sips_proto(OldURL, NewURL) when is_record(OldURL, sipurl), is_record(NewURL, sipurl) ->
    case {OldURL#sipurl.proto, NewURL#sipurl.proto} of
	{"sips", "sip"} ->
	    sipurl:set([{proto, "sips"}], NewURL);
	_ ->
	    NewURL
    end.
