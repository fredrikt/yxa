-module(pstnproxy).

-export([init/0, request/6, response/6]).

init() ->
    [[fun request/6, fun response/6], [user, numbers], stateful, none].


localhostname(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(myhostnames)).

pstngateway(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(pstngatewaynames)).

routeRequestToPSTN(FromIP, ToHost, THandler, LogTag) ->
    case pstngateway(FromIP) of
	true ->
	    logger:log(debug, "Routing: Source IP ~s is PSTN gateway, route to SIP proxy", [FromIP]),
	    false;		
	_ ->
	    IsLocalHostname = localhostname(ToHost),
	    IsPstnGatewayHostname = pstngateway(ToHost),
	    if
		IsLocalHostname == true ->
		    logger:log(debug, "Routing: Source IP ~s is not PSTN gateway and ~p is me, route to PSTN gateway",
		    		[FromIP, ToHost]),
		    true;
		IsPstnGatewayHostname == true ->
		    logger:log(debug, "Routing: Source IP ~s is not PSTN gateway and ~p is me, route to PSTN gateway",
				[FromIP, ToHost]),
		    true;
		true ->
		    logger:log(normal, "~s: pstnproxy: Denied request from ~s to ~s - not my problem", [LogTag, FromIP, ToHost]),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden"),
		    nomatch
	    end
    end.

% ACK requests that end up here could not be matched to a server transaction,
% most probably they are ACK to 2xx of INVITE (or we have crashed) - proxy
% statelessly.
request(Method, URI, Header, Body, Socket, FromIP) when Method == "ACK" ->
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    logger:log(normal, "pstnproxy: ~s -> Forwarding ACK received in core statelessly",
    		[LogStr]),
    transportlayer:send_proxy_request(none, {Method, URI, Header, Body}, URI, []);

request(Method, URL, Header, Body, Socket, FromIP) ->
    do_request(Method, URL, Header, Body, Socket, FromIP).

do_request(Method, URI, Header, Body, Socket, FromIP) ->
    {User, Pass, Host, Port, Parameters} = URI,
    Request = {Method, URI, Header, Body},
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case routeRequestToPSTN(FromIP, Host, THandler, LogTag) of
	true ->
	    logger:log(debug, "~s: pstnproxy: Route request to PSTN gateway", [LogTag]),
	    AllowedMethods = ["INVITE", "ACK", "PRACK", "CANCEL", "BYE", "OPTIONS"],
	    case lists:member(Method, AllowedMethods) of
		true ->
		    toPSTNrequest(Method, URI, Header, Body, Socket, LogTag);
		_ ->
		    logger:log(normal, "~s: pstnproxy: Method ~s not allowed for PSTN destination",
				[LogTag, Method]),
		    ExtraHeaders = [{"Allow", AllowedMethods}],
		    transactionlayer:send_response_handler(THandler, 405, "Method Not Allowed", ExtraHeaders)
	    end;
	false ->
	    logger:log(normal, "~s: pstnproxy: Route request to SIP server", [LogTag]),
	    toSIPrequest(Method, URI, Header, Body, Socket);
	_ ->
	    true
    end.

toSIPrequest(Method, URI, Header, Body, Socket) ->
    {User, Pass, Host, Port, Parameters} = URI,
    Request = {Method, URI, Header, Body},
    NewLocation = case localhostname(Host) of
	true ->
	    logger:log(debug, "Performing ENUM lookup on ~p", [User]),
	    case local:lookupenum(User) of
		{relay, Loc} ->
		    Loc;
		_ ->
		    Proxy = sipserver:get_env(sipproxy, none),
		    case Proxy of
			none ->
			    % No ENUM and no SIP-proxy configured, return failure so
			    % that the PBX on the other side of the gateway can try
			    % PSTN or something
			    %
			    % XXX choose something else than 503 since some clients
			    % probably mark this gateway as defunct for some time.
			    % RFC3261 #16.7 :
			    % In other words, forwarding a 503 means that the proxy knows it
			    % cannot service any requests, not just the one for the Request-
			    % URI in the request which generated the 503.
			    transactionlayer:send_response_request(Request, 503, "Service Unavailable"),
			    none;
			_ ->
			    {User, none, Proxy, none, []}
		    end
	    end;
	_ ->
	    URI
    end,
    case NewLocation of
	none ->
	    none;
	_ ->
	    Newheaders = case sipserver:get_env(record_route, true) of
		true -> siprequest:add_record_route(Header);
	        false -> Header
	    end,
	    {_, _, DstHost, _, _} = NewLocation,
	    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
	    Newheaders2 = add_caller_identity_for_sip(Method, Newheaders, FromURI),
	    THandler = transactionlayer:get_handler_for_request(Request),
	    NewRequest = {Method, URI, Newheaders2, Body},
	    proxy_request(THandler, NewRequest, NewLocation, [])
    end.

toPSTNrequest(Method, URI, Header, Body, Socket, LogTag) ->
    Request = {Method, URI, Header, Body},
    {DstNumber, _, ToHost, ToPort, _} = URI,
    {NewURI, NewHeaders1} = case localhostname(ToHost) of
	true ->
	    case local:lookuppstn(DstNumber) of
		{proxy, Dst} ->
		    {Dst, Header};	
		{relay, Dst} ->
		    {Dst, Header};
		_ ->
		    %% Route to default PSTN gateway
		    PSTNgateway1 = lists:nth(1, sipserver:get_env(pstngatewaynames)),
		    logger:log(debug, "pstnproxy: Routing request to default PSTN gateway ~p", [PSTNgateway1]),
		    NewURI2 = {DstNumber, none, PSTNgateway1, none, []},
		    {NewURI2, Header}
	    end;
	_ ->
	    %AddRoute = sipheader:contact_print([{none, {none, none, ToHost, ToPort, ["lr=true"]}}]),
	    %NewHeader1 = keylist:prepend({"Route", AddRoute}, Header),
	    {URI, Header}
    end,
    {_, _, PSTNgateway, _, _} = NewURI,
    NewHeaders2 = case sipserver:get_env(record_route, true) of
	true -> siprequest:add_record_route(NewHeaders1);
        false -> NewHeaders1
    end,
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    NewHeaders3 = add_caller_identity_for_pstn(Method, NewHeaders2, FromURI, PSTNgateway),
    NewRequest = {Method, URI, NewHeaders3, Body},
    THandler = transactionlayer:get_handler_for_request(Request),
    relay_request_to_pstn(THandler, NewRequest, NewURI, DstNumber, LogTag).
    
proxy_request(THandler, Request, DstURL, Parameters) ->
    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, DstURL, none, Parameters, 900]).

relay_request_to_pstn(THandler, {Method, OrigURI, Header, Body}, DstURI, DstNumber, LogTag) when Method == "CANCEL"; Method == "BYE" ->
    Request = {Method, OrigURI, Header, Body},
    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) (unauthenticated)",
		[LogTag, Method, DstNumber, sipurl:print(DstURI)]),
    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, DstURI, none, [], 900]);

relay_request_to_pstn(THandler, Request, DstURI, DstNumber, LogTag) ->
    {Method, URI, Header, Body} = Request,
    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
    Classdefs = sipserver:get_env(classdefs, [{"", unknown}]),
    logger:log(debug, "~s: pstnproxy: Relay ~s to PSTN ~s (~s)", [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
    case sipauth:pstn_call_check_auth(Method, Header, sipurl:print(FromURI), DstNumber, Classdefs) of
	{true, User, Class} ->
	    logger:log(debug, "Auth: User ~p is allowed to call dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (authenticated, class ~p) (~s)",
		       [LogTag, Method, DstNumber, Class, sipurl:print(DstURI)]),
	    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, DstURI, none, [], 900]);
	{stale, User, Class} ->
	    logger:log(debug, "Auth: User ~p must authenticate (stale) for dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) -> STALE authentication, sending challenge",
		       [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
	    transactionlayer:send_challenge(THandler, proxy, true, none);
	{false, User, Class} ->
	    logger:log(debug, "Auth: User ~p must authenticate for dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) -> needs authentication, sending challenge",
		       [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
	    transactionlayer:send_challenge(THandler, proxy, false, none);
	Unknown ->
	    logger:log(error, "Auth: Unknown result from sipauth:pstn_is_allowed_call() :~n~p", [Unknown]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.

response(Status, Reason, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    logger:log(normal, "Response to ~s: ~p ~s, no matching transaction - proxying statelessly", [LogStr, Status, Reason]),
    Response = {Status, Reason, Header, Body},
    transportlayer:send_proxy_response(none, Response).

add_caller_identity_for_pstn("INVITE", Headers, URI, Gateway) ->
    case sipserver:get_env(remote_party_id, false) of
	true ->
	    case local:get_remote_party_number(URI, Gateway) of
		Number when list(Number) ->
		    {User, _, Host, Port, _} = URI,
		    RemotePartyId = sipurl:print({Number, none, Host, Port, ["party=calling", "screen=yes", "privacy=off"]}),
		    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
		    NewHeaders1 = keylist:set("Remote-Party-Id", [RemotePartyId], Headers),
		    logger:log(debug, "P-Preferred-Identity: ~s", ["tel:" ++ Number]),
		    keylist:set("P-Preferred-Identity", ["tel:" ++ Number], NewHeaders1);
		_ ->
		    Headers
	    end;
	_ ->
	    Headers
    end;
add_caller_identity_for_pstn(_, Headers, _, _) ->
    Headers.
    
add_caller_identity_for_sip("INVITE", Headers, URI) ->
    case sipserver:get_env(remote_party_id, false) of
	true ->
	    {User, Pass, Host, Port, Parameters} = URI,
	    case local:get_remote_party_name(User, URI) of
		DisplayName when list(DisplayName) ->
		    [RPI] = sipheader:contact_print([{DisplayName, URI}]),
		    RemotePartyId = RPI ++ ";party=calling;screen=yes;privacy=off",
		    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
		    keylist:set("Remote-Party-Id", [RemotePartyId], Headers);
		_ ->
		    Headers
	    end;
	_ ->
	    Headers
    end;
add_caller_identity_for_sip(_, Headers, _) ->
    Headers.

get_branch_from_handler(TH) ->
    CallBranch = transactionlayer:get_branch_from_handler(TH),
    case string:rstr(CallBranch, "-UAS") of
	0 ->
	    CallBranch;
	Index when integer(Index) ->
	    BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
    end.
