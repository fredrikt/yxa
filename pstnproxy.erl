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
	    AllowedMethods = ["INVITE", "ACK", "PRACK", "CANCEL", "BYE", "OPTIONS"],
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
	    request_to_sip(Request, Origin, THandler);
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
    logger:log(normal, "pstnproxy: Response to ~s: ~p ~s, no matching transaction - proxying statelessly", [LogStr, Status, Reason]),
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
%% Returns: pstn |
%%          sip  |
%%          me   |
%%          drop
%%--------------------------------------------------------------------
route_request(Request, Origin, THandler, LogTag) when is_record(Request, request), is_record(Origin, siporigin) ->
    case local:is_request_to_this_proxy(Request) of
	true ->
	    me;
	false ->
	    URI = Request#request.uri,
	    route_request2(Origin#siporigin.addr, URI#sipurl.host, THandler, LogTag)
    end.

route_request2(FromIP, ToHost, THandler, LogTag) ->
    case pstngateway(FromIP) of
	true ->
	    logger:log(debug, "Routing: Source IP ~s is PSTN gateway, route to SIP proxy", [FromIP]),
	    sip;
	_ ->
	    IsLocalHostname = localhostname(ToHost),
	    IsPstnGatewayHostname = pstngateway(ToHost),
	    if
		IsLocalHostname == true ->
		    logger:log(debug, "Routing: Source IP ~s is not PSTN gateway and ~p is me, route to PSTN gateway",
			       [FromIP, ToHost]),
		    pstn;
		IsPstnGatewayHostname == true ->
		    logger:log(debug, "Routing: Source IP ~s is not PSTN gateway and ~p is me, route to PSTN gateway",
			       [FromIP, ToHost]),
		    pstn;
		true ->
		    logger:log(normal, "~s: pstnproxy: Denied request from ~s to ~s - not my problem", [LogTag, FromIP, ToHost]),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden"),
		    drop
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
%% Function: request_to_sip(Request, Origin, THandler)
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
request_to_sip(Request, Origin, THandler) when is_record(Request, request), is_record(Origin, siporigin) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    Dest = case localhostname(URI#sipurl.host) of
	       true ->
		   %% Hostname matches me
		   determine_sip_location(URI);
	       false ->
		   {proxy, URI}
	   end,
    case Dest of
	{proxy, NewURI} when is_record(NewURI, sipurl) ->
	    NewHeader1 = case sipserver:get_env(record_route, true) of
			     true -> siprequest:add_record_route(Request#request.header, Origin);
			     false -> Request#request.header
			 end,
	    {_, FromURI} = sipheader:from(NewHeader1),
	    NewHeader = add_caller_identity_for_sip(Method, NewHeader1, FromURI),
	    NewRequest = Request#request{header=NewHeader},
	    proxy_request(THandler, NewRequest, NewURI);
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
	    case sipserver:get_env(pstnproxy_redirect_on_enum, false) of
		true ->
		    %% Redirect caller to the destination we found in ENUM instead of
		    %% proxying the request.
		    Contact = contact:new(none, Loc, []),
		    ExtraHeaders = [{"Contact", sipheader:contact_print([Contact])}],
		    {reply, 302, "Moved Temporarily", ExtraHeaders};
		false ->
		    {proxy, Loc}
	    end;
	none ->
	    case get_sipproxy() of
		ProxyURL when is_record(ProxyURL, sipurl) ->
		    %% Use configured sipproxy but exchange user with user from Request-URI
		    {proxy, ProxyURL#sipurl{user=User, pass=none}};
		none ->
		    %% No ENUM and no (valid) SIP-proxy configured, return failure so
		    %% that the PBX on the other side of the gateway can fall back to
		    %% PSTN or something
		    Status = sipserver:get_env(pstnproxy_no_sip_dst_code, 480),
		    {reply, Status, "No destination found for number " ++ User}
	    end
    end.

%% part of determine_sip_location/1
get_sipproxy() ->
    case sipserver:get_env(sipproxy, none) of
	none ->
	    none;
	DefaultProxy ->
	    sipurl:parse_url_with_default_protocol("sip", DefaultProxy)
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
    {URI, Header} = {Request#request.uri, Request#request.header},
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
			    GwURL when is_record(GwURL, sipurl) ->
				NewURI2 = GwURL#sipurl{user=DstNumber, pass=none},
				{NewURI2, Header};
			    _ ->
				logger:log(error, "pstnproxy: Failed parsing configuration value "
					   "'pstngatewaynames' (~p)", [PSTNgateway1]),
				{URI, Header}
			end
		end;
	    _ ->
		{URI, Header}
	end,
    NewHeaders2 = case sipserver:get_env(record_route, true) of
		      true -> siprequest:add_record_route(NewHeaders1, Origin);
		      false -> NewHeaders1
		  end,
    NewRequest = Request#request{header=NewHeaders2},
    THandler = transactionlayer:get_handler_for_request(Request),
    relay_request_to_pstn(THandler, NewRequest, NewURI, DstNumber, LogTag).

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
    case sipserver:get_env(remote_party_id, false) of
	true ->
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
	false ->
	    Header
    end;
add_caller_identity_for_pstn(_Method, Header, _URI, _User, _Gateway) when is_record(Header, keylist) ->
    %% non-INVITE request, don't add Remote-Party-Id
    Header.


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
%% Returns : void(), Does not matter
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
relay_request_to_pstn(THandler, Request, DstURI, DstNumber, LogTag) when is_record(Request, request) ->
    {Method, Header} = {Request#request.method, Request#request.header},
    {_, FromURI} = sipheader:from(Header),
    Classdefs = sipserver:get_env(classdefs, [{"", unknown}]),
    logger:log(debug, "~s: pstnproxy: Relay ~s to PSTN ~s (~s)", [LogTag, Method, DstNumber, sipurl:print(DstURI)]),
    case sipauth:pstn_call_check_auth(Method, Header, FromURI, DstNumber, Classdefs) of
	{true, User, Class} ->
	    logger:log(debug, "Auth: User ~p is allowed to call dst ~s (class ~s)", [User, DstNumber, Class]),
	    logger:log(normal, "~s: pstnproxy: Relay ~s to PSTN ~s (authenticated, user ~p, class ~p) (~s)",
		       [LogTag, Method, DstNumber, User, Class, sipurl:print(DstURI)]),
	    %% Now that we know which user this request is authorized as, add Caller-ID information to it
	    PSTNgateway = DstURI#sipurl.host,
	    NewHeader = add_caller_identity_for_pstn(Method, Header, DstURI, User, PSTNgateway),
	    sippipe:start(THandler, none, Request#request{header=NewHeader}, DstURI, 900);
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
