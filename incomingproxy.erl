-module(incomingproxy).

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
    Registrar = {registrar, {registrar, start_link, []}, permanent, 2000, worker, [registrar]},
    [none, stateful, {append, [Registrar]}].


%% Function: request/3
%% Description: Yxa applications must export an request/3 function.
%% Returns: See XXX
%%--------------------------------------------------------------------

%%
%% REGISTER
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "REGISTER" ->
    {Method, URL, Header, Body} = {Request#request.method, Request#request.uri, Request#request.header, Request#request.body},
    logger:log(debug, "REGISTER ~p", [sipurl:print(URL)]),
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case local:homedomain(URL#sipurl.host) of
	true ->
	    logger:log(debug, "~s -> processing", [LogStr]),
	    % delete any present Record-Route header (RFC3261, #10.3)
	    NewHeader = keylist:delete("Record-Route", Header),
	    {_, ToURL} = sipheader:to(keylist:fetch("To", Header)),
	    case local:can_register(NewHeader, sipurl:print(ToURL)) of
		{{true, _}, SIPuser} ->
		    Contacts = sipheader:contact(keylist:fetch("Contact", Header)),
		    logger:log(debug, "Register: Contact(s) ~p", [sipheader:contact_print(Contacts)]),
		    logger:log(debug, "~s: Registering contacts for SIP user ~p", [LogTag, SIPuser]),
		    case catch siplocation:process_register_isauth(LogTag ++ ": incomingproxy", NewHeader, SIPuser, Contacts) of
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
		    Prio = case keylist:fetch("Authorization", Header) of
			[] -> debug;
			_ -> normal
		    end,
		    % XXX send new challenge (current behavior) or send 403 Forbidden when authentication fails?
		    logger:log(Prio, "~s -> Authentication FAILED, sending challenge", [LogStr]),
		    transactionlayer:send_challenge(THandler, www, false, 3);
		Unknown ->
		    logger:log(error, "Register: Unknown result from local:can_register() URL ~p :~n~p",
		    		[sipurl:print(URL), Unknown]),
		    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
	    end;
	_ ->
	    logger:log(debug, "REGISTER for non-homedomain ~p", [URL#sipurl.host]),
	    do_request({"REGISTER", URL, Header, Body}, Origin)
    end;

%%
%% ACK
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "ACK" ->
    logger:log(normal, "incomingproxy: ~s -> Forwarding ACK received in core statelessly",
    		[LogStr]),
    transportlayer:send_proxy_request(none, Request, Request#request.uri, []);

%%
%% CANCEL or BYE
%%
%% These requests cannot be challenged, since they can't be resubmitted. Bypass
%% check of authorized From: address
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "CANCEL"; Request#request.method == "BYE" ->
    do_request(Request, Origin);

%%
%% Request other than REGISTER, ACK, CANCEL or BYE
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin) ->
    {_, FromURI} = sipheader:from(keylist:fetch("From", Request#request.header)),
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
		    logger:log(normal, "~s: incomingproxy: Not authorized to use this From: -> 403 Forbidden", [LogTag]),
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
    end.


%% Function: response/3
%% Description: Yxa applications must export an response/3 function.
%% Returns: See XXX
%%--------------------------------------------------------------------
response(Response, Origin, LogStr) when record(Response, response), record(Origin, siporigin) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(normal, "Response to ~s: ~p ~s, no matching transaction - proxying statelessly", [LogStr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%% Function: verify_homedomain_user/2
%% Description: If a request has a From: matching our homedomains,
%%              this function is called to make sure the user really
%%              is who it says it is, and not someone else forging
%%              our users identity.
%% Returns: true  |
%%          false |
%%          drop
%%--------------------------------------------------------------------
verify_homedomain_user(Request, LogTag) when record(Request, request) ->
    Method = Request#request.method,
    Header = Request#request.header,
    case sipserver:get_env(always_verify_homedomain_user, true) of
	true ->
	    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
	    % Request has a From: address matching one of my domains.
	    % Verify sending user.
	    case local:get_user_verified_proxy(Header, Method) of
		{authenticated, SIPUser} ->
		    case local:can_use_address(SIPUser, sipurl:print(FromURI)) of
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
		    case keylist:fetch("Proxy-Authenticate", Header) of
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


%% Function: do_request/2
%% Description: Calls route_request() to determine what to do with a
%%              request, and then takes whatever action we are
%%              supposed to.
%% Returns: Does not matter
%%--------------------------------------------------------------------
do_request(RequestIn, Origin) when record(RequestIn, request), record(Origin, siporigin) ->
    {Method, URI} = {RequestIn#request.method, RequestIn#request.uri},
    logger:log(debug, "~s ~s~n",
	       [Method, sipurl:print(URI)]),
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
	{proxy, Loc} when record(Loc, sipurl) ->
	    logger:log(normal, "~s: incomingproxy: Proxy ~s -> ~s", [LogTag, Method, sipurl:print(Loc)]),
	    proxy_request(THandler, Request, Loc);
	{redirect, Loc} when record(Loc, sipurl) ->
	    logger:log(normal, "~s: incomingproxy: Redirect ~s", [LogTag, sipurl:print(Loc)]),
	    Contact = [{none, Loc}],
	    ExtraHeaders = [{"Contact", sipheader:contact_print(Contact)}],
	    transactionlayer:send_response_handler(THandler, 302, "Moved Temporarily", ExtraHeaders);
	{relay, Loc} ->
	    relay_request(THandler, Request, Loc, Origin, LogTag);
	{forward, FwdURL} when record(FwdURL, sipurl), FwdURL#sipurl.user == none, FwdURL#sipurl.pass == none ->
	    logger:log(normal, "~s: incomingproxy: Forward ~s ~s to ~s",
			[LogTag, Method, sipurl:print(URI), sipurl:print(FwdURL)]),
	    {ok, _, ApproxMsgSize} = siprequest:check_proxy_request(Request),
	    case siprequest:url_to_dstlist(FwdURL, ApproxMsgSize, URI) of
		{error, nxdomain} ->
		    logger:log(debug, "incomingproxy: Failed resolving FwdURL : NXDOMAIN (responding '604 Does Not Exist Anywhere')"),
                    transactionlayer:send_response_handler(THandler, 604, "Does Not Exist Anywhere"),
                    error;
		{error, What} ->
		    logger:log(normal, "incomingproxy: Failed resolving FwdURL : ~p", [What]),
                    transactionlayer:send_response_handler(THandler, 500, "Failed resolving forward destination"),
                    error;
		DstList when list(DstList) ->
		    proxy_request(THandler, Request, DstList)
	    end;
	{me} ->
	    request_to_me(THandler, Request, LogTag);
	_ ->
	    logger:log(error, "~s: incomingproxy: Invalid Location ~p", [LogTag, Location]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.


%% Function: route_request/1
%% Description: Check if a request is destined for this proxy, a local
%%              domain or a remote domain. In case of a local domain,
%%              we call request_to_homedomain(), and in case of a
%%              remote domain we call request_to_remote(). If these
%%              functions return 'nomatch' we call lookupdefault().
%% Returns: {error, Status}            |
%%          {response, Status, Reason} |
%%          {proxy, Location}          |
%%          {relay, Location}          |
%%          {forward, Location}        |
%%          {me}                       |
%%          none
%%--------------------------------------------------------------------
route_request(Request) when record(Request, request) ->
    {Method, URL, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    case keylist:fetch("Route", Request#request.header) of
	[] ->
	    Loc1 = case local:homedomain(URL#sipurl.host) of
		       true ->
			   case is_request_to_me(Method, URL, Header) of
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


%% Function: is_request_to_me/3
%% Description: Check if a request is destined for this proxy. Not
%%              for a domain handled by this proxy, but for this
%%              proxy itself.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
is_request_to_me(_, URL, Header) when record(URL, sipurl), URL#sipurl.user == none ->
    true;
is_request_to_me("OPTIONS", URL, Header) when record(URL, sipurl) ->
    % RFC3261 # 11 says a proxy that receives an OPTIONS request with a Max-Forwards less than one
    % MAY treat it as a request to the proxy.
    MaxForwards =
	case keylist:fetch("Max-Forwards", Header) of
	    [M] ->
		lists:min([255, list_to_integer(M) - 1]);
	    [] ->
		70
	end,
    if
	MaxForwards < 1 ->
	    logger:log(debug, "Routing: Request is OPTIONS and Max-Forwards < 1, treating it as a request to me."),
	    true;
	true ->
	    false
    end;
is_request_to_me(_, URL, _) when record(URL, sipurl) ->
    false.


%% Function: request_to_homedomain/1
%% Description: Find out where to route this request which is for one
%%              of our homedomains.
%% Returns: {error, Status}              |
%%          {response, Status, Reason}   |
%%          {proxy, Location}            |
%%          {relay, Location}            |
%%          {forward, Proto, Host, Port} |
%%          none
%%--------------------------------------------------------------------
request_to_homedomain(URL) when record(URL, sipurl) ->
    request_to_homedomain(URL, init).

request_to_homedomain(URL, Recursing) when record(URL, sipurl) ->
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


%% Function: request_to_homedomain_not_sipuser/2
%% Description: Second part of request_to_homedomain/1. The request
%%              is not for one of our SIP-users, call
%%              local:lookup_homedomain_url() and if that does not
%%              result in something usefull then see if this is
%%              something we can interpret as a phone number.
%% Returns: {error, Status}            |
%%          {response, Status, Reason} |
%%          {proxy, Location}          |
%%          {relay, Location}          |
%%          {forward, Location}        |
%%          none
%%--------------------------------------------------------------------
request_to_homedomain_not_sipuser(URL, loop) when record(URL, sipurl) ->
    none;
request_to_homedomain_not_sipuser(URL, init) when record(URL, sipurl) ->

    Loc1 = local:lookup_homedomain_url(URL),
    logger:log(debug, "Routing: local:lookup_homedomain_url on ~s -> ~p", [sipurl:print(URL), Loc1]),

    case Loc1 of
	none ->
	    % local:lookuppotn() returns 'none' if argument is not numeric,
	    % so we don't have to check that...
	    Res1 = local:lookuppotn(URL#sipurl.user),
	    logger:log(debug, "Routing: lookuppotn on ~s -> ~p", [URL#sipurl.user, Res1]),
	    Res1;
	{proxy, NewURL} ->
	    logger:log(debug, "Routing: request_to_homedomain_not_sipuser: Calling request_to_homedomain on " ++
		       "result of local:lookup_homedomain_url (local URL ~s)",
	    		[sipurl:print(NewURL)]),
	    request_to_homedomain(NewURL, loop);
	{relay, Dst} ->
	    logger:log(debug, "Routing: request_to_homedomain_not_sipuser: Turning relay into proxy, original " ++
		       "request was to a local domain"),
	    {proxy, Dst};
	_ ->
	    Loc1
    end.


%% Function: request_to_remote/1
%% Description: Find out where to route this request which is for a
%%              remote domain.
%% Returns: {error, Status}            |
%%          {response, Status, Reason} |
%%          {proxy, Location}          |
%%          {relay, Location}          |
%%          {forward, Location}        |
%%          none
%%--------------------------------------------------------------------
request_to_remote(URL) when record(URL, sipurl) ->
    case local:lookup_remote_url(URL) of
	none ->
	    case local:get_user_with_contact(URL) of
		none ->
		    logger:log(debug, "Routing: ~p is not a local domain, relaying", [URL#sipurl.host]),
		    {relay, URL};
		SIPuser ->
		    logger:log(debug, "Routing: ~p is not a local domain, but it is a registered location of SIPuser ~p. Proxying.",
			       [sipurl:print(URL), SIPuser]),
		    {proxy, URL}
	    end;		
	Location ->
	    logger:log(debug, "Routing: local:lookup_remote_url() ~s -> ~p", [sipurl:print(URL), Location]),
	    Location
    end.


%% Function: request_to_me/3
%% Description: Request is meant for this proxy, if it is OPTIONS we
%%              respond 200 Ok, otherwise we respond 481 Call/
%%              transaction does not exist.
%% Returns: Does not matter.
%%--------------------------------------------------------------------
request_to_me(THandler, Request, LogTag) when record(Request, request), Request#request.method == "OPTIONS" ->
    logger:log(normal, "~s: incomingproxy: OPTIONS to me -> 200 OK", [LogTag]),
    logger:log(debug, "XXX The OPTIONS response SHOULD include Accept, Accept-Encoding, Accept-Language, and Supported headers. RFC 3261 section 11"),
    transactionlayer:send_response_handler(THandler, 200, "OK");

request_to_me(THandler, Request, LogTag) when record(Request, request) ->
    logger:log(normal, "~s: incomingproxy: non-OPTIONS request to me -> 481 Call/Transaction Does Not Exist", [LogTag]),
    transactionlayer:send_response_handler(THandler, 481, "Call/Transaction Does Not Exist").


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


%% Function: proxy_request/3
%% Description: Proxy a request somewhere without authentication.
%% Returns: Does not matter
%%--------------------------------------------------------------------
proxy_request(THandler, Request, DstList) when record(Request, request) ->
    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, DstList, 900]).


%% Function: relay_request/5
%% Description: Relay request to remote host. If there is not valid
%%              credentials present in the request, challenge user
%%              unless local policy says not to. Never challenge
%%              CANCEL or BYE since they can't be resubmitted and
%%              therefor cannot be challenged.
%% Returns: Does not matter
%%--------------------------------------------------------------------

%%
%% CANCEL or BYE
%%
relay_request(THandler, Request, Dst, Origin, LogTag) when record(Request, request), Request#request.method == "CANCEL"; Request#request.method == "BYE" ->
    logger:log(normal, "~s: incomingproxy: Relay ~s ~s (unauthenticated)",
	       [LogTag, Request#request.method, sipurl:print(Request#request.uri)]),
    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, Dst, 900]);

%%
%% Anything but CANCEL or BYE
%%
relay_request(THandler, Request, Dst, Origin, LogTag) when record(Request, request) ->
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    case sipauth:get_user_verified_proxy(Header, Method) of
	{authenticated, User} ->
	    logger:log(debug, "Relay: User ~p is authenticated", [User]),
	    logger:log(normal, "~s: incomingproxy: Relay ~s (authenticated)", [LogTag, relay_dst2str(Dst)]),
	    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, Dst, 900]);
	stale ->
	    case local:incomingproxy_challenge_before_relay(Origin, Request, Dst) of
		false ->
		    logger:log(debug, "Relay: STALE authentication, but local policy says we should not challenge"),
		    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, Dst, 900]);
		_ ->
		    logger:log(debug, "Relay: STALE authentication, sending challenge"),
		    logger:log(normal, "~s: incomingproxy: Relay ~s -> STALE authentication -> 407 Proxy Authentication Required",
			       [LogTag, relay_dst2str(Dst)]),
		    transactionlayer:send_challenge(THandler, proxy, true, none)
	    end;
	false ->
            case local:incomingproxy_challenge_before_relay(Origin, Request, Dst) of
                false ->
                    logger:log(debug, "Relay: Failed authentication, but local policy says we should not challenge"),
                    sipserver:safe_spawn(sippipe, start, [THandler, none, Request, Dst, 900]);
                _ ->
		    logger:log(debug, "Relay: Failed authentication, sending challenge"),
		    logger:log(normal, "~s: incomingproxy: Relay ~s -> 407 Proxy Authorization Required", [LogTag, relay_dst2str(Dst)]),
		    transactionlayer:send_challenge(THandler, proxy, false, none)
	    end;
	Unknown ->
	    logger:log(error, "relay_request: Unknown result from sipauth:get_user_verified_proxy() :~n~p", [Unknown]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.

relay_dst2str(URI) when record(URI, sipurl) ->
    sipurl:print(URI);
relay_dst2str(route) ->
    "according to Route header";
relay_dst2str(_) ->
    "unknown dst".
