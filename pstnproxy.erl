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
%%           Mode    = stateful (or 'stateless' but DON'T use that).
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
    transportlayer:send_proxy_request(none, Request, Request#request.uri, []),
    ok;

%%
%% Anything except ACK
%%
request(Request, Origin, _LogStr) when record(Request, request), record(Origin, siporigin) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case routeRequestToPSTN(Origin#siporigin.addr, URI#sipurl.host, THandler, LogTag) of
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
	    toSIPrequest(Request, Origin, THandler)
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
    logger:log(normal, "pstnproxy: Response to ~s: ~p ~s, no matching transaction - proxying statelessly", [LogStr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response),
    ok.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: routeRequestToPSTN(FromIP, ToHost, THandler, LogTag)
%%           FromIP   = string()
%%           ToHost   = string(), Host part of Request-URI
%%           THandler = term(), server transaction handle
%%           LogTag   = string(), prefix for logging
%% Descrip.: Determines if we should route this request to one of
%%           our PSTN gateways or not.
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


%%--------------------------------------------------------------------
%% Function: localhostname(Hostname)
%%           Hostname = string()
%% Descrip.: Check if given hostname matches one of ours.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
localhostname(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(myhostnames)).


%%--------------------------------------------------------------------
%% Function: pstngateway(Hostname)
%%           Hostname = string()
%% Descrip.: Check if given hostname matches one of our PSTN
%%           gateways hostnames.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
pstngateway(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(pstngatewaynames)).


%%--------------------------------------------------------------------
%% Function: toSIPrequest(Request, Origin, THandler)
%%           Request  = request record()
%%           Origin   = siporigin record()
%%           THandler = term(), server transaction handle
%% Descrip.: It has been determined that we should send this
%%           request to a VoIP destination (i.e. anything else than
%%           one of our PSTN gateways). If the host part of the
%%           request URI matches this proxy, then do an ENUM lookup
%%           on the user part of the request URI, if it looks like
%%           a phone number.
%% Returns : Does not matter
%%--------------------------------------------------------------------
toSIPrequest(Request, Origin, THandler) when record(Request, request), record(Origin, siporigin) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    NewLocation = case localhostname(URI#sipurl.host) of
		      true ->
			  %% Hostname matches me
			  User = URI#sipurl.user,
			  logger:log(debug, "pstnproxy: Performing ENUM lookup on ~p", [User]),
			  case local:lookupenum(User) of
			      {relay, Loc} ->
				  Loc;
			      _ ->
				  %% Parse configured sipproxy but exchange user with user from Request-URI
				  case sipurl:parse_url_with_default_protocol("sip", sipserver:get_env(sipproxy, none)) of
				      ProxyURL when record(ProxyURL, sipurl) ->
					  ProxyURL#sipurl{user=User, pass=none};
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
					  none
				  end
			  end;
		      _ ->
			  URI
		  end,
    case NewLocation of
	none ->
	    none;
	_ when record(NewLocation, sipurl) ->
	    NewHeader1 = case sipserver:get_env(record_route, true) of
			     true -> siprequest:add_record_route(Request#request.header, Origin);
			     false -> Request#request.header
			 end,
	    {_, FromURI} = sipheader:from(NewHeader1),
	    NewHeader = add_caller_identity_for_sip(Method, NewHeader1, FromURI),
	    NewRequest = Request#request{header=NewHeader},
	    proxy_request(THandler, NewRequest, NewLocation)
    end.


%%--------------------------------------------------------------------
%% Function: toPSTNrequest(Request, Origin, THandler, LogTag)
%%           Request  = request record()
%%           Origin   = siporigin record()
%%           THandler = term(), server transaction handle
%%           LogTag   = string(), prefix for logging
%% Descrip.: It has been determined that we should send this
%%           request to one of our PSTN gateways. Figure out which
%%           one and make it so.
%% Returns : Does not matter
%%--------------------------------------------------------------------
toPSTNrequest(Request, Origin, THandler, LogTag) when record(Request, request), record(Origin, siporigin) ->
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    {DstNumber, ToHost} = {URI#sipurl.user, URI#sipurl.host},
    %% XXX check port to, not just hostname? Maybe not since pstnproxy by definition
    %% should be running alone on a host.
    {NewURI, NewHeaders1} =
	case localhostname(ToHost) of
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
			case sipurl:parse_url_with_default_protocol("sip", PSTNgateway1) of
			    GwURL when record(GwURL, sipurl) ->
				NewURI2 = GwURL#sipurl{user=DstNumber, pass=none},
				{NewURI2, Header};
			    _ ->
				logger:log(error, "pstnproxy: Failed parsing configuration value 'pstngatewaynames' (~p)",
					   [PSTNgateway1]),
				{URI, Header}
			end
		end;
	    _ ->
		{URI, Header}
	end,
    PSTNgateway = NewURI#sipurl.host,
    NewHeaders2 = case sipserver:get_env(record_route, true) of
		      true -> siprequest:add_record_route(NewHeaders1, Origin);
		      false -> NewHeaders1
		  end,
    {_, FromURI} = sipheader:from(Header),
    NewHeaders3 = add_caller_identity_for_pstn(Method, NewHeaders2, FromURI, PSTNgateway),
    NewRequest = Request#request{header=NewHeaders3},
    THandler = transactionlayer:get_handler_for_request(Request),
    relay_request_to_pstn(THandler, NewRequest, NewURI, DstNumber, LogTag).


%%--------------------------------------------------------------------
%% Function: add_caller_identity_for_pstn(Method, Headers, URI,
%%                                        Gateway)
%%           Method  = string()
%%           Headers = keylist record()
%%           URI     = sipurl record()
%%           Gateway = string()
%% Descrip.: If configured to, add Remote-Party-Id information
%%           about caller to this request before it is sent to a
%%           PSTN gateway. Useful to get proper caller-id.
%% Returns : NewHeader, keylist record()
%%--------------------------------------------------------------------
add_caller_identity_for_pstn("INVITE", Headers, URI, Gateway) when is_record(URI, sipurl) ->
    case sipserver:get_env(remote_party_id, false) of
	true ->
	    case local:get_remote_party_number(URI, Gateway) of
		{ok, Number} when is_list(Number) ->
		    Parameters = [{"party", "calling"}, {"screen", "yes"}, {"privacy", "off"}],
		    RPURI = sipurl:set([{user, Number}, {pass, none}, {param, []}], URI),
		    RPI = contact:new(none, RPURI, Parameters),
		    RemotePartyId = contact:print(RPI),
		    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
		    NewHeaders1 = keylist:set("Remote-Party-Id", [RemotePartyId], Headers),
		    logger:log(debug, "P-Preferred-Identity: ~s", ["tel:" ++ Number]),
		    keylist:set("P-Preferred-Identity", ["tel:" ++ Number], NewHeaders1);
		none ->
		    Headers
	    end;
	_ ->
	    Headers
    end;
add_caller_identity_for_pstn(_Method, Headers, _URI, _Gateway) when is_record(Headers, keylist) ->
    %% non-INVITE request, don't add Remote-Party-Id
    Headers.


%%--------------------------------------------------------------------
%% Function: add_caller_identity_for_sip(Method, Headers, URI)
%%           Method  = string()
%%           Headers = keylist record()
%%           URI     = sipurl record()
%% Descrip.: If configured to, add Remote-Party-Id information
%%           about caller to this request received from one of our
%%           PSTN gateways. Useful to get the name of the person
%%           calling in your phones display - if you have the name
%%           available in some database.
%% Returns : NewHeader, keylist record()
%%--------------------------------------------------------------------
add_caller_identity_for_sip("INVITE", Headers, URI) when is_record(URI, sipurl) ->
    case sipserver:get_env(remote_party_id, false) of
	true ->
	    case local:get_remote_party_name(URI#sipurl.user, URI) of
		{ok, DisplayName} when is_list(DisplayName) ->
		    Parameters = [{"party", "calling"}, {"screen", "yes"}, {"privacy", "off"}],
		    RPI = contact:new(DisplayName, URI, Parameters),
		    RemotePartyId = contact:print(RPI),
		    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
		    keylist:set("Remote-Party-Id", [RemotePartyId], Headers);
		none ->
		    Headers
	    end;
	_ ->
	    Headers
    end;
add_caller_identity_for_sip(_, Headers, _) ->
    %% non-INVITE request, don't add Remote-Party-Id
    Headers.


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
	Index when integer(Index) ->
	    BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
    end.


%%--------------------------------------------------------------------
%% Function: proxy_request(THandler, Request, DstURI)
%%           THandler = term(), server transcation handle
%%           Request  = request record()
%%           DstURI   = sipurl record()
%% Descrip.: Proxy a request somewhere without authentication.
%% Returns : Does not matter
%%--------------------------------------------------------------------
proxy_request(THandler, Request, DstURI) when is_record(Request, request),
					      is_record(DstURI, sipurl) ->
    sippipe:start(THandler, none, Request, DstURI, 900).


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
%% Returns : Does not matter
%%--------------------------------------------------------------------

%%
%% CANCEL or BYE
%%
relay_request_to_pstn(THandler, #request{method=Method}=Request, DstURI, DstNumber, LogTag) when Method == "CANCEL";
												 Method == "BYE" ->
    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (~s) (unauthenticated)",
	       [LogTag, Request#request.method, DstNumber, sipurl:print(DstURI)]),
    sippipe:start(THandler, none, Request, DstURI, 900);

%%
%% Anything except CANCEL or BYE
%%
relay_request_to_pstn(THandler, Request, DstURI, DstNumber, LogTag) when record(Request, request) ->
    {Method, Header} = {Request#request.method, Request#request.header},
    {_, FromURI} = sipheader:from(Header),
    Classdefs = sipserver:get_env(classdefs, [{"", unknown}]),
    logger:log(debug, "~s: pstnproxy: Relay ~s to PSTN ~s (~s)", [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
    case sipauth:pstn_call_check_auth(Method, Header, FromURI, DstNumber, Classdefs) of
	{true, User, Class} ->
	    logger:log(debug, "Auth: User ~p is allowed to call dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (authenticated, class ~p) (~s)",
		       [LogTag, Method, DstNumber, Class, sipurl:print(DstURI)]),
	    sippipe:start(THandler, none, Request, DstURI, 900);
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
