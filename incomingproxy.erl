%%%-------------------------------------------------------------------
%%% File    : incomingproxy.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: Server handling registrar functions and routing of SIP-
%%%           requests/responses.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(incomingproxy).

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
    Registrar = {registrar, {registrar, start_link, []}, permanent, 2000, worker, [registrar]},
    [none, stateful, {append, [Registrar]}].

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
%% XXX REGISTER request processing is done slightly out of order
%% (step 2) compared to the RFC (RFC 3261 chapter 10.3) specification.
%% This could theoreticaly result in unexpected, but legal, error
%% responses.
request(#request{method="REGISTER"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    register_request(Request, Origin, LogStr),
    ok;

%%
%% ACK
%%
request(#request{method="ACK"}=Request, Origin, LogStr) when is_record(Origin, siporigin) ->
    logger:log(normal, "incomingproxy: ~s -> Forwarding ACK received in core statelessly",
	       [LogStr]),
    transportlayer:stateless_proxy_request("incomingproxy", Request),
    ok;

%%
%% CANCEL or BYE
%%
%% These requests cannot be challenged. CANCEL because it can't be resubmitted (RFC3261 #22.1),
%% and ACK because it is illegal to send responses to ACK. Bypass check of authorized From: address.
request(#request{method=Method}=Request, Origin, _LogStr) when is_record(Origin, siporigin),
							       Method == "CANCEL"; Method == "BYE" ->
    do_request(Request, Origin),
    ok;

%%
%% Request other than REGISTER, ACK, CANCEL or BYE
%%
request(Request, Origin, LogStr) when is_record(Request, request), is_record(Origin, siporigin) ->
    {_, FromURI} = sipheader:from(Request#request.header),
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    %% Check if the From: address matches our homedomains, and if so
    %% call verify_homedomain_user() to make sure the user is
    %% authorized and authenticated to use this From: address
    case local:homedomain(FromURI#sipurl.host) of
	true ->
	    case verify_homedomain_user(Request, LogTag) of
		true ->
		    do_request(Request, Origin);
		false ->
		    logger:log(normal, "~s: incomingproxy: Not authorized to use this From: -> 403 Forbidden",
			       [LogTag]),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden");
		drop ->
		    ok;
		Unknown ->
		    logger:log(error, "~s: Unknown result from verify_homedomain_user() :~n~p",
			       [LogStr, Unknown]),
		    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
	    end;
	_ ->
	    do_request(Request, Origin)
    end,
    ok.

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
    logger:log(normal, "incomingproxy: Response to ~s: '~p ~s', no matching transaction - proxying statelessly",
	       [LogStr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: register_request(Request, Origin, LogStr)
%%           Request = response record()
%%           Origin  = siporigin record()
%%           LogStr  = string(), description of request
%% Descrip.: Process a received REGISTER. First check if it is for
%%           one of our domains (homedomain), then check that it
%%           contains proper authentication and that the authenticated
%%           user is allowed to register using this address-of-record.
%%           Finally, let siplocation:process_register_isauth()
%%           process all the Contact headers and update the location
%%           database.
%% Returns : Does not matter.
%%--------------------------------------------------------------------
register_request(Request, Origin, LogStr) when is_record(Request, request), is_record(Origin, siporigin) ->
    URL = Request#request.uri,
    logger:log(debug, "incomingproxy: REGISTER ~p", [sipurl:print(URL)]),
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 1
    %% check if this registrar handles the domain the request wants to register for
    case local:homedomain(URL#sipurl.host) of
	true ->
	    register_require_supported(Request, Origin, LogStr, THandler, LogTag);
	_ ->
	    %% act as proxy and forward message to other domain
	    logger:log(debug, "incomingproxy: REGISTER for non-homedomain ~p", [URL#sipurl.host]),
	    do_request(Request, Origin)
    end.

%% part of register_request/3
register_require_supported(Request, Origin, LogStr, THandler, LogTag) ->
    Header = Request#request.header,
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 2
    case is_valid_register_request(Header) of
	true ->
	    register_authenticate(Request, Origin, LogStr, THandler, LogTag);
	{siperror, Status, Reason, ExtraHeaders} ->
	    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders)
    end.

%% part of register_request/3
register_authenticate(Request, _Origin, LogStr, THandler, LogTag) ->
    {URL, Header} =
	{Request#request.uri, Request#request.header},
    logger:log(debug, "incomingproxy: ~s -> processing", [LogStr]),
    %% delete any present Record-Route header (RFC3261, #10.3)
    NewHeader = keylist:delete("Record-Route", Header),
    {_, ToURL} = sipheader:to(Header),
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 3, step 4 and step 5
    %% authenticate UAC
    case local:can_register(NewHeader, ToURL) of
	{{true, _}, SIPuser} ->
	    Contacts = sipheader:contact(Header),
	    logger:log(debug, "Register: Contact(s) ~p", [sipheader:contact_print(Contacts)]),
	    logger:log(debug, "~s: Registering contacts for SIP user ~p", [LogTag, SIPuser]),
	    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 6, step 7 and step 8
	    case catch siplocation:process_register_isauth(LogTag ++ ": incomingproxy",
							   NewHeader, SIPuser, Contacts) of
		{ok, {Status, Reason, ExtraHeaders}} ->
		    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders);
		{siperror, Status, Reason} ->
		    transactionlayer:send_response_handler(THandler, Status, Reason);
		{siperror, Status, Reason, ExtraHeaders} ->
		    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders);
		{'EXIT', Reason} ->
		    logger:log(error, "=ERROR REPORT==== siplocation:process_register_isauth() failed :~n~p~n",
			       [Reason]),
		    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error");
		_ ->
		    true
	    end;
	{stale, _} ->
	    logger:log(normal, "~s -> Authentication is STALE, sending new challenge", [LogStr]),
	    transactionlayer:send_challenge(THandler, www, true, none);
	{{false, eperm}, SipUser} when SipUser /= none ->
	    logger:log(normal, "~s: incomingproxy: SipUser ~p NOT ALLOWED to REGISTER address ~s",
		       [LogTag, SipUser, sipurl:print(ToURL)]),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden");
	{{false, nomatch}, SipUser} when SipUser /= none ->
	    logger:log(normal, "~s: incomingproxy: SipUser ~p tried to REGISTER invalid address ~s",
		       [LogTag, SipUser, sipurl:print(ToURL)]),
	    transactionlayer:send_response_handler(THandler, 404, "Not Found");
	{false, none} ->
	    Prio = case keylist:fetch('authorization', Header) of
		       [] -> debug;
		       _ -> normal
		   end,
	    %% XXX send new challenge (current behavior) or send 403 Forbidden when authentication fails?
	    logger:log(Prio, "~s -> Authentication FAILED, sending challenge", [LogStr]),
	    transactionlayer:send_challenge(THandler, www, false, 3);
	Unknown ->
	    logger:log(error, "Register: Unknown result from local:can_register() URL ~p :~n~p",
		       [sipurl:print(URL), Unknown]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.


%%--------------------------------------------------------------------
%% Function: is_valid_register_request(Header)
%%           Header = keylist record()
%% Descrip.: looks for unsupported extensions.
%% Returns : true | SipError
%%--------------------------------------------------------------------
is_valid_register_request(Header) ->
    Require = keylist:fetch('require', Header),
    case Require of
	[] ->
	    true;
	_ ->
	    %% we support no extensions, so all are unsupported
	    logger:log(normal, "Request check: The client requires unsupported extension(s) ~p", [Require]),
	    {siperror, 420, "Bad Extension", [{"Unsupported", Require}]}
    end.


%%--------------------------------------------------------------------
%% Function: verify_homedomain_user(Request, LogTag)
%%           Request = request record()
%%           LogTag  = string(), prefix for logging
%% Descrip.: If a request has a From: matching our homedomains,
%%           this function is called to make sure the user really
%%           is who it says it is, and not someone else forging
%%           our users identity.
%% Returns:  true | false | drop
%%--------------------------------------------------------------------
verify_homedomain_user(Request, LogTag) when is_record(Request, request) ->
    Method = Request#request.method,
    Header = Request#request.header,
    case sipserver:get_env(always_verify_homedomain_user, true) of
	true ->
	    {_, FromURI} = sipheader:from(Header),
	    %% Request has a From: address matching one of my domains.
	    %% Verify sending user.
	    case local:get_user_verified_proxy(Header, Method) of
		{authenticated, SIPUser} ->
		    case local:can_use_address(SIPUser, FromURI) of
			true ->
			    logger:log(debug, "Request: User ~p is allowed to use From: address ~p",
				       [SIPUser, sipurl:print(FromURI)]),
			    true;
			false ->
			    logger:log(error, "Authenticated user ~p may NOT use address ~p",
				       [SIPUser, sipurl:print(FromURI)]),
			    false
		    end;
		stale ->
		    logger:log(normal, "~s: incomingproxy: From: address requires authentication (stale)", [LogTag]),
		    transactionlayer:send_challenge_request(Request, proxy, true, none),
		    drop;
		false ->
		    case keylist:fetch('proxy-authenticate', Header) of
			[] ->
			    logger:log(normal, "~s: incomingproxy: From: address requires authentication", [LogTag]),
			    transactionlayer:send_challenge_request(Request, proxy, false, none),
			    drop;
			_ ->
			    false
		    end;
		Unknown ->
		    logger:log(error, "request: Unknown result from local:get_user_verified_proxy() :~n~p", [Unknown]),
		    transactionlayer:send_response_request(Request, 500, "Server Internal Error"),
		    drop
	    end;
	_ ->
	    true
    end.

%%--------------------------------------------------------------------
%% Function: do_request(RequestIn, Origin)
%%           RequestIn = request record()
%%           Origin = siporigin record()
%% Descrip.: Calls route_request() to determine what to do with a
%%           request, and then takes whatever action we are
%%           supposed to.
%% Returns : Does not matter
%%--------------------------------------------------------------------
do_request(RequestIn, Origin) when is_record(RequestIn, request), is_record(Origin, siporigin) ->
    {Method, URI} = {RequestIn#request.method, RequestIn#request.uri},
    logger:log(debug, "~s ~s~n", [Method, sipurl:print(URI)]),
    Header = case sipserver:get_env(record_route, false) of
		 true -> siprequest:add_record_route(RequestIn#request.header, Origin);
		 false -> RequestIn#request.header
	     end,
    Request = RequestIn#request{header = Header},
    Location = route_request(Request),
    logger:log(debug, "Location: ~p", [Location]),
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case Location of
	none ->
	    logger:log(normal, "~s: incomingproxy: 404 Not found", [LogTag]),
	    transactionlayer:send_response_handler(THandler, 404, "Not Found");
	{error, Errorcode} ->
	    logger:log(normal, "~s: incomingproxy: Error ~p", [LogTag, Errorcode]),
	    transactionlayer:send_response_handler(THandler, Errorcode, "Unknown code");
	{response, Status, Reason} ->
	    logger:log(normal, "~s: incomingproxy: Response ~p ~s", [LogTag, Status, Reason]),
	    transactionlayer:send_response_handler(THandler, Status, Reason);
	{proxy, Loc} when is_record(Loc, sipurl) ->
	    logger:log(normal, "~s: incomingproxy: Proxy ~s -> ~s", [LogTag, Method, sipurl:print(Loc)]),
	    proxy_request(THandler, Request, Loc);
	{redirect, Loc} when is_record(Loc, sipurl) ->
	    logger:log(normal, "~s: incomingproxy: Redirect ~s", [LogTag, sipurl:print(Loc)]),
	    Contact = [contact:new(none, Loc, [])],
	    ExtraHeaders = [{"Contact", sipheader:contact_print(Contact)}],
	    transactionlayer:send_response_handler(THandler, 302, "Moved Temporarily", ExtraHeaders);
	{relay, Loc} ->
	    relay_request(THandler, Request, Loc, Origin, LogTag);
	{forward, FwdURL} when is_record(FwdURL, sipurl), FwdURL#sipurl.user == none, FwdURL#sipurl.pass == none ->
	    logger:log(normal, "~s: incomingproxy: Forward ~s ~s to ~s",
		       [LogTag, Method, sipurl:print(URI), sipurl:print(FwdURL)]),
	    ApproxMsgSize = siprequest:get_approximate_msgsize(Request#request{uri=FwdURL}),
	    case sipdst:url_to_dstlist(FwdURL, ApproxMsgSize, URI) of
		{error, nxdomain} ->
		    logger:log(debug, "incomingproxy: Failed resolving FwdURL : NXDOMAIN"
			       " (responding '604 Does Not Exist Anywhere')"),
                    transactionlayer:send_response_handler(THandler, 604, "Does Not Exist Anywhere"),
                    error;
		{error, What} ->
		    logger:log(normal, "incomingproxy: Failed resolving FwdURL : ~p", [What]),
                    transactionlayer:send_response_handler(THandler, 500, "Failed resolving forward destination"),
                    error;
		DstList when is_list(DstList) ->
		    proxy_request(THandler, Request, DstList)
	    end;
	{me} ->
	    request_to_me(THandler, Request, LogTag);
	_ ->
	    logger:log(error, "~s: incomingproxy: Invalid Location ~p", [LogTag, Location]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.

%%--------------------------------------------------------------------
%% Function: route_request(Request)
%%           Request = request record()
%% Descrip.: Check if a request is destined for this proxy, a local
%%           domain or a remote domain. In case of a local domain,
%%           we call request_to_homedomain(), and in case of a
%%           remote domain we call request_to_remote(). If these
%%           functions return 'nomatch' we call lookupdefault().
%% Returns : {error, Status}            |
%%           {response, Status, Reason} |
%%           {proxy, Location}          |
%%           {relay, Location}          |
%%           {forward, Location}        |
%%           {me}                       |
%%           none
%%--------------------------------------------------------------------
route_request(Request) when is_record(Request, request) ->
    URL = Request#request.uri,
    case keylist:fetch('route', Request#request.header) of
	[] ->
	    Loc1 = case local:homedomain(URL#sipurl.host) of
		       true ->
			   case local:is_request_to_this_proxy(Request) of
			       true ->
				   {me};
			       _ ->
				   request_to_homedomain(URL)
			   end;
		       _ ->
			   request_to_remote(URL)
		   end,
	    case Loc1 of
		nomatch ->
		    logger:log(debug, "Routing: No match - trying default route"),
		    local:lookupdefault(URL);
		_ ->
		    Loc1
	    end;
	_ ->
	    %% Request has Route header
	    logger:log(debug, "Routing: Request has Route header, following Route."),
	    {relay, route}
    end.

%%--------------------------------------------------------------------
%% Function: request_to_homedomain(URL)
%%           URL = sipurl record()
%% Descrip.: Find out where to route this request which is for one
%%           of our homedomains.
%% Returns : {error, Status}              |
%%           {response, Status, Reason}   |
%%           {proxy, Location}            |
%%           {relay, Location}            |
%%           {forward, Proto, Host, Port} |
%%           none
%%--------------------------------------------------------------------
request_to_homedomain(URL) when is_record(URL, sipurl) ->
    request_to_homedomain(URL, init).

request_to_homedomain(URL, Recursing) when is_record(URL, sipurl) ->
    logger:log(debug, "Routing: Request to homedomain, URI ~p", [sipurl:print(URL)]),

    Loc1 = local:lookupuser(URL),
    logger:log(debug, "Routing: lookupuser on ~p -> ~p", [sipurl:print(URL), Loc1]),

    case Loc1 of
	none ->
	    logger:log(debug, "Routing: ~s is one of our users, returning Temporarily Unavailable",
		       [sipurl:print(URL)]),
	    {response, 480, "Users location currently unknown"};
	nomatch ->
	    request_to_homedomain_not_sipuser(URL, Recursing);
	Loc1 ->
	    Loc1
    end.

%%--------------------------------------------------------------------
%% Function: request_to_homedomain_not_sipuser/2
%% Descrip.: Second part of request_to_homedomain/1. The request
%%           is not for one of our SIP-users, call
%%           local:lookup_homedomain_url() and if that does not
%%           result in something usefull then see if this is
%%           something we can interpret as a phone number.
%% Returns : {error, Status}            |
%%           {response, Status, Reason} |
%%           {proxy, Location}          |
%%           {relay, Location}          |
%%           {forward, Location}        |
%%           none
%%--------------------------------------------------------------------
request_to_homedomain_not_sipuser(URL, loop) when is_record(URL, sipurl) ->
    none;
request_to_homedomain_not_sipuser(URL, init) when is_record(URL, sipurl) ->

    Loc1 = local:lookup_homedomain_url(URL),
    logger:log(debug, "Routing: local:lookup_homedomain_url on ~s -> ~p", [sipurl:print(URL), Loc1]),

    case Loc1 of
	none ->
	    %% local:lookuppotn() returns 'none' if argument is not numeric,
	    %% so we don't have to check that...
	    Res1 = local:lookuppotn(URL#sipurl.user),
	    logger:log(debug, "Routing: lookuppotn on ~s -> ~p", [URL#sipurl.user, Res1]),
	    Res1;
	{proxy, NewURL} ->
	    logger:log(debug, "Routing: request_to_homedomain_not_sipuser: Calling request_to_homedomain on "
		       "result of local:lookup_homedomain_url (local URL ~s)",
		       [sipurl:print(NewURL)]),
	    request_to_homedomain(NewURL, loop);
	{relay, Dst} ->
	    logger:log(debug, "Routing: request_to_homedomain_not_sipuser: Turning relay into proxy, original "
		       "request was to a local domain"),
	    {proxy, Dst};
	_ ->
	    Loc1
    end.

%%--------------------------------------------------------------------
%% Function: request_to_remote(URL)
%%           URL = sipurl record()
%% Descrip.: Find out where to route this request which is for a
%%           remote domain.
%% Returns : {error, Status}            |
%%           {response, Status, Reason} |
%%           {proxy, Location}          |
%%           {relay, Location}          |
%%           {forward, Location}        |
%%           none
%%--------------------------------------------------------------------
request_to_remote(URL) when is_record(URL, sipurl) ->
    case local:lookup_remote_url(URL) of
	none ->
	    case local:get_user_with_contact(URL) of
		none ->
		    logger:log(debug, "Routing: ~p is not a local domain, relaying", [URL#sipurl.host]),
		    {relay, URL};
		SIPuser ->
		    logger:log(debug, "Routing: ~p is not a local domain,"
			       " but it is a registered location of SIPuser ~p. Proxying.",
			       [sipurl:print(URL), SIPuser]),
		    {proxy, URL}
	    end;
	Location ->
	    logger:log(debug, "Routing: local:lookup_remote_url() ~s -> ~p", [sipurl:print(URL), Location]),
	    Location
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
    logger:log(debug, "XXX The OPTIONS response SHOULD include Accept, Accept-Encoding,"
	       " Accept-Language, and Supported headers. RFC 3261 section 11"),
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
    logger:log(normal, "~s: incomingproxy: Relay ~s ~s (unauthenticated)",
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
	    logger:log(normal, "~s: incomingproxy: Relay ~s (authenticated)", [LogTag, relay_dst2str(Dst)]),
	    sippipe:start(THandler, none, Request, Dst, 900);
	stale ->
	    case local:incomingproxy_challenge_before_relay(Origin, Request, Dst) of
		false ->
		    logger:log(debug, "Relay: STALE authentication, but local policy says we should not challenge"),
		    sippipe:start(THandler, none, Request, Dst, 900);
		_ ->
		    logger:log(debug, "Relay: STALE authentication, sending challenge"),
		    logger:log(normal, "~s: incomingproxy: Relay ~s -> STALE authentication ->"
			       " 407 Proxy Authentication Required",
			       [LogTag, relay_dst2str(Dst)]),
		    transactionlayer:send_challenge(THandler, proxy, true, none)
	    end;
	false ->
            case local:incomingproxy_challenge_before_relay(Origin, Request, Dst) of
                false ->
                    logger:log(debug, "Relay: Failed authentication, but local policy says we should not challenge"),
                    sippipe:start(THandler, none, Request, Dst, 900);
                _ ->
		    logger:log(debug, "Relay: Failed authentication, sending challenge"),
		    logger:log(normal, "~s: incomingproxy: Relay ~s -> 407 Proxy Authorization Required",
			       [LogTag, relay_dst2str(Dst)]),
		    transactionlayer:send_challenge(THandler, proxy, false, none)
	    end;
	Unknown ->
	    logger:log(error, "relay_request: Unknown result from sipauth:get_user_verified_proxy() :~n~p", [Unknown]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.

relay_dst2str(URI) when is_record(URI, sipurl) ->
    sipurl:print(URI);
relay_dst2str(route) ->
    "according to Route header";
relay_dst2str(_) ->
    "unknown dst".
