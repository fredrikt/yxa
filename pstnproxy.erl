-module(pstnproxy).
-export([start/2]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, [user, numbers],
				   stateless]),
    {ok, Pid}.

init() ->
    true.

localhostname(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(myhostnames)).

pstngateway(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(pstngatewaynames)).

routeRequestToPSTN(FromIP, ToHost) ->
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
		    logger:log(debug, "Routing: Denied request from ~s to ~s - not my problem",[FromIP, ToHost]),
		    nomatch
	    end
    end.

request("ACK", URL, Header, Body, Socket, FromIP) ->
    do_request("ACK", URL, Header, Body, Socket, FromIP);

request(Method, URL, Header, Body, Socket, FromIP) ->
    do_request(Method, URL, Header, Body, Socket, FromIP).

do_request(Method, URI, Header, Body, Socket, FromIP) ->
    {User, Pass, Host, Port, Parameters} = URI,
    Request = {Method, URI, Header, Body},
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    case routeRequestToPSTN(FromIP, Host) of
	true ->
	    logger:log(debug, "~s -> PSTN gateway", [LogStr]),
	    case Method of
		"INVITE" ->
		    toPSTNrequest(Method, URI, Header, Body, Socket, LogStr);
		"ACK" ->
		    toPSTNrequest(Method, URI, Header, Body, Socket, LogStr);
		"PRACK" ->
		    toPSTNrequest(Method, URI, Header, Body, Socket, LogStr);
		"CANCEL" ->
		    toPSTNrequest(Method, URI, Header, Body, Socket, LogStr);
		"BYE" ->
		    toPSTNrequest(Method, URI, Header, Body, Socket, LogStr);
		"OPTIONS" ->
		    toPSTNrequest(Method, URI, Header, Body, Socket, LogStr);
		_ ->
		    logger:log(normal, "~s -> PSTN denied, 405 Method Not Allowed", [LogStr]),
		    ExtraHeaders = [{"Allow", ["INVITE", "ACK", "PRACK", "CANCEL", "BYE", "OPTIONS"]}],
		    transactionlayer:send_response_request(Request, 405, "Method Not Allowed", ExtraHeaders)
	    end;
	false ->
	    logger:log(normal, "~s -> SIP server", [LogStr]),
	    toSIPrequest(Method, URI, Header, Body, Socket);
	_ ->
	    logger:log(normal, "~s -> 403 Forbidden", [LogStr]),
	    transactionlayer:send_response_request(Request, 403, "Forbidden")
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

proxy_request(THandler, Request, DstURL, Parameters) ->
    case siprequest:send_proxy_request(THandler, Request, DstURL, Parameters) of
	{sendresponse, Status, Reason} ->
	    {Method, URI, _, _} = Request,
	    logger:log(error, "Transport layer failed sending ~s ~s to ~s, asked us to respond ~p ~s",
			[Method, sipurl:print(URI), sipurl:print(DstURL), Status, Reason]),
	    transactionlayer:send_response_request(Request, Status, Reason);
	{ok, _} ->
	    true;
	Unknown ->
	    logger:log(error, "Transport layer returned unknown result, responding 500 Server Internal Error"),
	    logger:log(debug, "ransport layer returned unknown result :~n~p", [Unknown]),
	    transactionlayer:send_response_request(Request, 500, "Server Internal Error")
    end.

toPSTNrequest(Method, URI, Header, Body, Socket, LogStr) ->
    Request = {Method, URI, Header, Body},
    {DstNumber, _, ToHost, ToPort, _} = URI,
    {NewURI, NewHeaders1} = case localhostname(ToHost) of
	true ->
	    case local:lookuppstn(DstNumber) of
		{proxy, Dst} ->
		    {Dst, Header};	
		_ ->
		    % Route to default PSTN gateway
		    PSTNgateway1 = lists:nth(1, sipserver:get_env(pstngatewaynames)),
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
    relay_request_to_pstn(THandler, NewRequest, NewURI, DstNumber, LogStr).
    
relay_request_to_pstn(THandler, {"ACK", URI, Header, Body}, NewURI, DstNumber, LogStr) ->
    Request = {"ACK", URI, Header, Body},
    logger:log(normal, "~s -> PSTN ~s (~s)", [LogStr, DstNumber, sipurl:print(URI)]),
    siprequest:send_proxy_request(THandler, Request, NewURI, []);

relay_request_to_pstn(THandler, {"CANCEL", URI, Header, Body}, NewURI, DstNumber, LogStr) ->
    Request = {"CANCEL", URI, Header, Body},
    logger:log(normal, "~s -> PSTN ~s (~s)", [LogStr, DstNumber, sipurl:print(URI)]),
    siprequest:send_proxy_request(THandler, Request, NewURI, []);

relay_request_to_pstn(THandler, Request, NewURI, DstNumber, LogStr) ->
    {Method, URI, Header, Body} = Request,
    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
    Classdefs = sipserver:get_env(classdefs, [{"", unknown}]),
    case sipauth:pstn_call_check_auth(Method, Header, sipurl:print(FromURI), DstNumber, Classdefs) of
	{true, User, Class} ->
	    logger:log(debug, "Auth: User ~p is allowed to call dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s -> PSTN ~s (~s)", [LogStr, DstNumber, sipurl:print(NewURI)]),
	    siprequest:send_proxy_request(THandler, Request, NewURI, []);
	{stale, User, Class} ->
	    logger:log(debug, "Auth: User ~p must authenticate for dst ~s (class ~s)", [User, DstNumber, Class]),
	    ExtraHeaders = [{"Proxy-Authenticate", sipheader:auth_print(sipauth:get_challenge(), true)}],
	    transactionlayer:send_response_request(Request, 407, "Proxy Authentication Required", ExtraHeaders);
	{false, User, Class} ->
	    logger:log(debug, "Auth: User ~p must authenticate for dst ~s (class ~s)", [User, DstNumber, Class]),
	    ExtraHeaders = [{"Proxy-Authenticate", sipheader:auth_print(sipauth:get_challenge(), false)}],
	    transactionlayer:send_response_request(Request, 407, "Proxy Authentication Required", ExtraHeaders);
	Unknown ->
	    logger:log(error, "Auth: Unknown result from sipauth:pstn_is_allowed_call() :~n~p", [Unknown]),
	    transactionlayer:send_response_request(Request, 500, "Server Internal Error")
    end.

response(Status, Reason, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    Response = {Status, Reason, Header, Body},
    case transactionlayer:get_server_handler_for_stateless_response(Response) of
	{error, E} ->
	    logger:log(error, "Failed getting server transaction for stateless response: ~p", [E]),
	    logger:log(normal, "Response to ~s: ~p ~s, failed fetching state - proxying", [LogStr, Status, Reason]),
	    siprequest:send_proxy_response(none, Status, Reason, Header, Body);
	none ->
	    logger:log(normal, "Response to ~s: ~p ~s, found no state - proxying", [LogStr, Status, Reason]),
	    siprequest:send_proxy_response(none, Status, Reason, Header, Body);
	TH ->
	    logger:log(debug, "Response to ~s: ~p ~s, server transaction ~p", [LogStr, Status, Reason, TH]),
	    transactionlayer:send_proxy_response_handler(TH, Response)
    end.


add_caller_identity_for_pstn("INVITE", Headers, URI, Gateway) ->
    case sipserver:get_env(remote_party_id, false) of
	true ->
	    case local:get_remote_party_number(URI, Gateway) of
		none ->
		    Headers;
		Number ->
		    {User, _, Host, Port, _} = URI,
		    RemotePartyId = sipurl:print({Number, none, Host, Port, ["party=calling", "screen=yes", "privacy=off"]}),
		    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
		    NewHeaders1 = keylist:set("Remote-Party-Id", [RemotePartyId], Headers),
		    logger:log(debug, "P-Preferred-Identity: ~s", ["tel:" ++ Number]),
		    keylist:set("P-Preferred-Identity", ["tel:" ++ Number], NewHeaders1)
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
		none ->
		    Headers;
		DisplayName ->
		    [RPI] = sipheader:contact_print([{DisplayName, URI}]),
		    RemotePartyId = RPI ++ ";party=calling;screen=yes;privacy=off",
		    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
		    keylist:set("Remote-Party-Id", [RemotePartyId], Headers)
	    end;
	_ ->
	    Headers
    end;
add_caller_identity_for_sip(_, Headers, _) ->
    Headers.
