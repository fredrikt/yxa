-module(pstnproxy).

%% Standard Yxa SIP-application exports
-export([init/0, request/3, response/3]).

-include("siprecords.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%%% Standard Yxa SIP-application exported functions
%%--------------------------------------------------------------------


%% Function: init/0
%% Description: Yxa applications must export an init/0 function.
%% Returns: See XXX
%%--------------------------------------------------------------------
init() ->
    [[user, numbers], stateful, none].


%% Function: request/3
%% Description: Yxa applications must export an request/3 function.
%% Returns: See XXX
%%--------------------------------------------------------------------

%%
%% ACK
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "ACK" ->
    %% ACK requests that end up here could not be matched to a server transaction,
    %% most probably they are ACK to 2xx of INVITE (or we have crashed) - proxy
    %% statelessly.
    logger:log(normal, "pstnproxy: ~s -> Forwarding ACK received in core statelessly",
	       [LogStr]),
    transportlayer:send_proxy_request(none, Request, Request#request.uri, []);

%%
%% Anything except ACK
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    {User, Pass, Host, Port, Parameters} = URI,
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case routeRequestToPSTN(Origin#siporigin.addr, Host, THandler, LogTag) of
	true ->
	    logger:log(debug, "~s: pstnproxy: Route request to PSTN gateway", [LogTag]),
	    AllowedMethods = ["INVITE", "ACK", "PRACK", "CANCEL", "BYE", "OPTIONS"],
	    case lists:member(Method, AllowedMethods) of
		true ->
		    toPSTNrequest(Request, Origin, THandler, LogTag);
		_ ->
		    logger:log(normal, "~s: pstnproxy: Method ~s not allowed for PSTN destination",
			       [LogTag, Method]),
		    ExtraHeaders = [{"Allow", AllowedMethods}],
		    transactionlayer:send_response_handler(THandler, 405, "Method Not Allowed", ExtraHeaders)
	    end;
	false ->
	    logger:log(normal, "~s: pstnproxy: Route request to SIP server", [LogTag]),
	    toSIPrequest(Request, Origin, THandler);
	_ ->
	    true
    end.


%% Function: response/3
%% Description: Yxa applications must export an request/3 function.
%% Returns: See XXX
%%--------------------------------------------------------------------
response(Response, Origin, LogStr) when record(Response, response), record(Origin, siporigin) ->
    {Status, Reason, Header, Body} = {Response#response.status, Response#response.reason, Response#response.header, Response#response.body},
    logger:log(normal, "Response to ~s: ~p ~s, no matching transaction - proxying statelessly", [LogStr, Status, Reason]),
    Response = {Status, Reason, Header, Body},
    transportlayer:send_proxy_response(none, Response).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%% Function: routeRequestToPSTN/1
%% Description: Determines if we should route this request to one of
%%              our PSTN gateways or not.
%% Returns: true  |
%%          false
%%--------------------------------------------------------------------
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


%% Function: localhostname/1
%% Description: Check if given hostname matches one of ours.
%% Returns: true  |
%%          false
%%--------------------------------------------------------------------
localhostname(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(myhostnames)).


%% Function: pstngateway/1
%% Description: Check if given hostname matches one of our PSTN
%%              gateways hostnames.
%% Returns: true  |
%%          false
%%--------------------------------------------------------------------
pstngateway(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(pstngatewaynames)).


%% Function: toSIPrequest/3
%% Description: It has been determined that we should send this
%%              request to a VoIP destination (i.e. anything else than
%%              one of our PSTN gateways). If the host part of the
%%              request URI matches this proxy, then do an ENUM lookup
%%              on the user part of the request URI, if it looks like
%%              a phone number.
%% Returns: Does not matter
%%--------------------------------------------------------------------
toSIPrequest(Request, Origin, THandler) when record(Request, request), record(Origin, siporigin) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    {User, Pass, Host, Port, Parameters} = URI,
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
					  %% No ENUM and no SIP-proxy configured, return failure so
					  %% that the PBX on the other side of the gateway can try
					  %% PSTN or something
					  %%
					  %% XXX choose something else than 503 since some clients
					  %% probably mark this gateway as defunct for some time.
					  %% RFC3261 #16.7 :
					  %% In other words, forwarding a 503 means that the proxy knows it
					  %% cannot service any requests, not just the one for the Request-
					  %% URI in the request which generated the 503.
					  transactionlayer:send_response_handler(THandler, 503, "Service Unavailable"),
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
	    NewHeader1 = case sipserver:get_env(record_route, true) of
			     true -> siprequest:add_record_route(Request#request.header, Origin);
			     false -> Request#request.header
			 end,
	    {_, _, DstHost, _, _} = NewLocation,
	    {_, FromURI} = sipheader:to(keylist:fetch("From", NewHeader1)),
	    NewHeader = add_caller_identity_for_sip(Method, NewHeader1, FromURI),
	    NewRequest = Request#request{header=NewHeader},
	    proxy_request(THandler, NewRequest, NewLocation, [])
    end.


%% Function: toPSTNrequest/3
%% Description: It has been determined that we should send this
%%              request to one of our PSTN gateways. Figure out which
%%              one and make it so.
%% Returns: Does not matter
%%--------------------------------------------------------------------
toPSTNrequest(Request, Origin, THandler, LogTag) when record(Request, request), record(Origin, siporigin) ->
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
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
				    %%AddRoute = sipheader:contact_print([{none, {none, none, ToHost, ToPort, ["lr=true"]}}]),
				    %%NewHeader1 = keylist:prepend({"Route", AddRoute}, Header),
				    {URI, Header}
			    end,
    {_, _, PSTNgateway, _, _} = NewURI,
    NewHeaders2 = case sipserver:get_env(record_route, true) of
		      true -> siprequest:add_record_route(NewHeaders1, Origin);
		      false -> NewHeaders1
		  end,
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    NewHeaders3 = add_caller_identity_for_pstn(Method, NewHeaders2, FromURI, PSTNgateway),
    NewRequest = Request#request{header=NewHeaders3},
    THandler = transactionlayer:get_handler_for_request(Request),
    relay_request_to_pstn(THandler, NewRequest, NewURI, DstNumber, LogTag).


%% Function: add_caller_identity_for_pstn/4
%% Description: If configured to, add Remote-Party-Id information
%%              about caller to this request before it is sent to a
%%              PSTN gateway. Useful to get proper caller-id.
%% Returns: New headers
%%--------------------------------------------------------------------
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
    %% non-INVITE request, don't add Remote-Party-Id
    Headers.


%% Function: add_caller_identity_for_sip/4
%% Description: If configured to, add Remote-Party-Id information
%%              about caller to this request received from one of our
%%              PSTN gateways. Useful to get the name of the person
%%              calling in your phones display - if you have the name
%%              available in some database.
%% Returns: New headers
%%--------------------------------------------------------------------
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
    %% non-INVITE request, don't add Remote-Party-Id
    Headers.


%% Function: get_branch_from_handler/1
%% Description: Get branch from server transaction handler and then
%%              remove the -UAS suffix. The result is used as a tag
%%              when logging actions.
%% Returns: Branch
%%--------------------------------------------------------------------
get_branch_from_handler(TH) ->
    CallBranch = transactionlayer:get_branch_from_handler(TH),
    case string:rstr(CallBranch, "-UAS") of
	0 ->
	    CallBranch;
	Index when integer(Index) ->
	    BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
    end.


%% Function: proxy_request/4
%% Description: Proxy a request somewhere without authentication.
%% Returns: Does not matter
%%--------------------------------------------------------------------
proxy_request(THandler, Request, DstURL, Parameters) when record(Request, request) ->
    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, DstURL, Parameters, 900]).


%% Function: relay_request/5
%% Description: Relay request to one of our PSTN gateways. If there is
%%              not valid credentials present in the request,
%%              challenge user unless the number is in a class which
%%              does not require authentication. Never challenge
%%              CANCEL or BYE since they can't be resubmitted and
%%              therefor cannot be challenged.
%% Returns: Does not matter
%%--------------------------------------------------------------------

%%
%% CANCEL or BYE
%%
relay_request_to_pstn(THandler, Request, DstURI, DstNumber, LogTag) when record(Request, request), Request#request.method == "CANCEL"; Request#request.method == "BYE" ->
    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) (unauthenticated)",
	       [LogTag, Request#request.method, DstNumber, sipurl:print(DstURI)]),
    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, DstURI, [], 900]);

%%
%% Anything except CANCEL or BYE
%%
relay_request_to_pstn(THandler, Request, DstURI, DstNumber, LogTag) when record(Request, request) ->
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
    Classdefs = sipserver:get_env(classdefs, [{"", unknown}]),
    logger:log(debug, "~s: pstnproxy: Relay ~s to PSTN ~s (~s)", [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
    case sipauth:pstn_call_check_auth(Method, Header, sipurl:print(FromURI), DstNumber, Classdefs) of
	{true, User, Class} ->
	    logger:log(debug, "Auth: User ~p is allowed to call dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (authenticated, class ~p) (~s)",
		       [LogTag, Method, DstNumber, Class, sipurl:print(DstURI)]),
	    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, DstURI, [], 900]);
	{stale, User, Class} ->
	    logger:log(debug, "Auth: User ~p must authenticate (stale) for dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) -> STALE authentication, sending challenge",
		       [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
	    transactionlayer:send_challenge(THandler, proxy, true, none);
	{false, none, Class} ->
	    logger:log(debug, "Auth: Need authentication for dst ~s (class ~s)", [DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) -> needs authentication, sending challenge",
		       [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
	    transactionlayer:send_challenge(THandler, proxy, false, none);
	{false, User, Class} ->
	    logger:log(debug, "Auth: User ~p not allowed dst ~s in class unknown - answer 403 Forbidden", [User, DstNumber]),
	    logger:log(normal, "~s: pstnproxy: User ~p not allowed relay ~s to PSTN ~s class ~p -> 403 Forbidden",
		       [LogTag, User, Method, DstNumber, Class]),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden");
	Unknown ->
	    logger:log(error, "Auth: Unknown result from sipauth:pstn_is_allowed_call() :~n~p", [Unknown]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.
