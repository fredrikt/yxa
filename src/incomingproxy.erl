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
    Tables = [user, numbers, phone, cpl_script_graph, regexproute],
    [Tables, stateful, {append, [Registrar]}].

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
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branchbase_from_handler(THandler),
    case siplocation:process_register_request(Request, THandler, LogTag, LogStr, incomingproxy) of
	not_homedomain ->
	    do_request(Request, Origin, THandler, LogTag);
	_ ->
	    true
    end,
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
    LogTag = get_branchbase_from_handler(THandler),
    %% Check if the From: address matches our homedomains, and if so
    %% call verify_homedomain_user() to make sure the user is
    %% authorized and authenticated to use this From: address
    case local:homedomain(FromURI#sipurl.host) of
	true ->
	    case verify_homedomain_user(Request, LogTag, Origin, LogStr) of
		true ->
		    do_request(Request, Origin, THandler, LogTag);
		false ->
		    logger:log(normal, "~s: incomingproxy: Not authorized to use this From: -> 403 Forbidden",
			       [LogTag]),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden");
		drop ->
		    ok
	    end;
	_ ->
	    do_request(Request, Origin, THandler, LogTag)
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
%% Function: verify_homedomain_user(Request, LogTag)
%%           Request = request record()
%%           LogTag  = string(), prefix for logging
%% Descrip.: If a request has a From: matching our homedomains,
%%           this function is called to make sure the user really
%%           is who it says it is, and not someone else forging
%%           our users identity.
%% Returns : true | false | drop
%%--------------------------------------------------------------------
verify_homedomain_user(Request, LogTag, Origin, LogStr) when is_record(Request, request), is_list(LogTag),
							     is_record(Origin, siporigin), is_list(LogStr) ->
    case yxa_config:get_env(always_verify_homedomain_user) of
	{ok, true} ->
	    {Method, Header} = {Request#request.method, Request#request.header},
	    {_, FromURI} = sipheader:from(Header),
	    %% Request has a From: address matching one of my domains.
	    %% Verify sending user.
	    case local:get_user_verified_proxy(Header, Method) of
		{authenticated, SIPUser} ->
		    case local:can_use_address(SIPUser, FromURI) of
			true ->
			    logger:log(debug, "Request: User ~p is allowed to use From: address ~p",
				       [SIPUser, sipurl:print(FromURI)]),
			    %% Generate a request_info event with some information about this request
			    L = [{from_user, SIPUser}],
			    event_handler:request_info(normal, LogTag, L),
			    true;
			false ->
			    logger:log(error, "Authenticated user ~p may NOT use address ~p",
				       [SIPUser, sipurl:print(FromURI)]),
			    false
		    end;
		{stale, SIPuser} ->
		    logger:log(normal, "~s: incomingproxy: From: address requires authentication (stale, user ~p)",
			       [LogTag, SIPuser]),
		    transactionlayer:send_challenge_request(Request, proxy, true, none),
		    drop;
		false ->
		    case keylist:fetch('proxy-authenticate', Header) of
			[] ->
			    logger:log(normal, "~s: incomingproxy: From: address requires authentication", [LogTag]),
			    transactionlayer:send_challenge_request(Request, proxy, false, none),
			    drop;
			_ ->
			    OStr = sipserver:origin2str(Origin),
			    Msg = io_lib:format("Request from ~s failed authentication : ~s", [OStr, LogStr]),
			    event_handler:generic_event(normal, auth, LogTag, Msg),
			    false
		    end
	    end;
	{ok, false} ->
	    true
    end.

%%--------------------------------------------------------------------
%% Function: do_request(RequestIn, Origin)
%%           do_request(RequestIn, Origin, THandler, LogTag)
%%           RequestIn = request record()
%%           Origin    = siporigin record()
%%           THandler  = term(), server transaction handle
%%           LogTag    = string() | undefined
%% Descrip.: Calls route_request() to determine what to do with a
%%           request, and then takes whatever action we are
%%           supposed to.
%% Returns : Does not matter
%%--------------------------------------------------------------------
do_request(Request, Origin) ->
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branchbase_from_handler(THandler),
    do_request(Request, Origin, THandler, LogTag).
do_request(Request, Origin, THandler, LogTag) when is_record(Request, request), is_record(Origin, siporigin),
						   is_list(LogTag) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    logger:log(debug, "incomingproxy: Processing request ~s ~s~n", [Method, sipurl:print(URI)]),
    Location = route_request(Request, Origin, LogTag),
    logger:log(debug, "incomingproxy: Location: ~p", [Location]),
    case Location of
	none ->
	    logger:log(normal, "~s: incomingproxy: 404 Not found", [LogTag]),
	    transactionlayer:send_response_handler(THandler, 404, "Not Found");
	{error, Errorcode} ->
	    logger:log(normal, "~s: incomingproxy: Error ~p", [LogTag, Errorcode]),
	    transactionlayer:send_response_handler(THandler, Errorcode, "Unexplained error");
	{response, Status, Reason} ->
	    logger:log(normal, "~s: incomingproxy: Response '~p ~s'", [LogTag, Status, Reason]),
	    transactionlayer:send_response_handler(THandler, Status, Reason);
	{proxy, Loc} when is_record(Loc, sipurl) ->
	    logger:log(normal, "~s: incomingproxy: Proxy ~s -> ~s", [LogTag, Method, sipurl:print(Loc)]),
	    proxy_request(THandler, Request, Loc);
	{proxy, [H | _] = DstList} when is_record(H, sipdst) ->
	    logger:log(normal, "~s: incomingproxy: Proxy ~s to list of destinations", [LogTag, Method]),
	    logger:log(debug, "~s: incomingproxy: Proxy ~s -> ~s", [LogTag, Method, DstList]),
	    proxy_request(THandler, Request, DstList);
	{proxy, route} ->
	    logger:log(normal, "~s: incomingproxy: Proxy ~s according to Route header", [LogTag, Method]),
	    proxy_request(THandler, Request, route);
	{redirect, Loc} when is_record(Loc, sipurl) ->
	    logger:log(normal, "~s: incomingproxy: Redirect ~s", [LogTag, sipurl:print(Loc)]),
	    Contact = [contact:new(none, Loc, [])],
	    ExtraHeaders = [{"Contact", sipheader:contact_print(Contact)}],
	    transactionlayer:send_response_handler(THandler, 302, "Moved Temporarily", ExtraHeaders);
	{relay, Loc} when is_record(Loc, sipurl); Loc == route; is_list(Loc) ->
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
%% Function: route_request(Request, Origin, LogTag)
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogTag  = string()
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
route_request(Request, Origin, LogTag) when is_record(Request, request), is_list(LogTag) ->
    case keylist:fetch('route', Request#request.header) of
	[] ->
	    URL = Request#request.uri,
	    Loc1 = case local:homedomain(URL#sipurl.host) of
		       true ->
			   case local:is_request_to_this_proxy(Request) of
			       true ->
				   {me};
			       _ ->
				   request_to_homedomain(Request, Origin, LogTag)
			   end;
		       _ ->
			   request_to_remote(Request, Origin)
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
%% Function: request_to_homedomain(Request, Origin, LogTag)
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogTag  = string()
%% Descrip.: Find out where to route this request which is for one
%%           of our homedomains.
%% Returns : {error, Status}              |
%%           {response, Status, Reason}   |
%%           {proxy, Location}            |
%%           {relay, Location}            |
%%           {forward, Proto, Host, Port} |
%%           none
%%--------------------------------------------------------------------
request_to_homedomain(Request, Origin, LogTag) when is_record(Request, request), is_record(Origin, siporigin),
						    is_list(LogTag) ->
    request_to_homedomain(Request, Origin, LogTag, init).

request_to_homedomain(Request, Origin, LogTag, Recursing) when is_record(Request, request), is_record(Origin, siporigin),
							       is_list(LogTag) ->
    URL = Request#request.uri,
    URLstr = sipurl:print(URL),
    logger:log(debug, "Routing: Request to homedomain, URI ~p", [URLstr]),

    case local:lookupuser(URL) of
	nomatch ->
	    request_to_homedomain_not_sipuser(Request, Origin, LogTag, Recursing);
        {ok, Users, none} ->
	    logger:log(debug, "Routing: I currently have no locations for user(s) ~p, "
		       "in the location database, answering '480 Temporarily Unavailable'",
		       [Users]),
	    {response, 480, "Users location currently unknown"};
	{ok, Users, Res} when is_list(Users) ->
	    request_to_homedomain_log_result(URLstr, Res),

	    %% Generate a request_info event with some information about this request
	    L = [{to_users, Users}],
	    event_handler:request_info(normal, LogTag, L),

	    Res
    end.

request_to_homedomain_log_result(URLstr, {A, U}) when is_atom(A), is_record(U, sipurl) ->
    logger:log(debug, "Routing: lookupuser on ~p -> ~p to ~p", [URLstr, A, sipurl:print(U)]);
request_to_homedomain_log_result(URLstr, {forward, Proto, Host, Port}) ->
    logger:log(debug, "Routing: lookupuser on ~p -> forward to ~p:~s:~p", [URLstr, Proto, Host, Port]);
request_to_homedomain_log_result(URLstr, Res) ->
    logger:log(debug, "Routing: lookupuser on ~p -> ~p", [URLstr, Res]).

%%--------------------------------------------------------------------
%% Function: request_to_homedomain_not_sipuser(Request, Origin,
%%                                             LogTag, Recursing)
%%           Request   = request record()
%%           Origin    = siporigin record()
%%           LogTag    = string()
%%           Recursing = init | loop, have we recursed already?
%% Descrip.: Second part of request_to_homedomain/1. The request
%%           is not for one of our SIP-users, call
%%           local:lookup_homedomain_request() and if that does not
%%           result in something usefull then see if this is something
%%           we can interpret as a phone number.
%% Returns : {error, Status}            |
%%           {response, Status, Reason} |
%%           {proxy, Location}          |
%%           {relay, Location}          |
%%           {forward, Location}        |
%%           none
%%--------------------------------------------------------------------
request_to_homedomain_not_sipuser(_Request, _Origin, _LogTag, loop) ->
    none;
request_to_homedomain_not_sipuser(Request, Origin, LogTag, init)
  when is_record(Request, request), is_record(Origin, siporigin), is_list(LogTag) ->
    URL = Request#request.uri,

    Loc1 = local:lookup_homedomain_request(Request, Origin),
    logger:log(debug, "Routing: local:lookup_homedomain_request on ~s -> ~p", [sipurl:print(URL), Loc1]),

    case Loc1 of
	none ->
	    %% local:lookuppotn() returns 'none' if argument is not numeric,
	    %% so we don't have to check that...
	    Res1 = local:lookuppotn(URL#sipurl.user),
	    logger:log(debug, "Routing: lookuppotn on ~s -> ~p", [URL#sipurl.user, Res1]),
	    Res1;
	{proxy, NewURL} when is_record(NewURL, sipurl) ->
	    logger:log(debug, "Routing: request_to_homedomain_not_sipuser: Calling request_to_homedomain on "
		       "result of local:lookup_homedomain_request (local URL ~s)",
		       [sipurl:print(NewURL)]),
	    request_to_homedomain(Request#request{uri = URL}, Origin, LogTag, loop);
	{relay, Dst} ->
	    logger:log(debug, "Routing: request_to_homedomain_not_sipuser: Turning relay into proxy, original "
		       "request was to a local domain"),
	    {proxy, Dst};
	_ ->
	    Loc1
    end.

%%--------------------------------------------------------------------
%% Function: request_to_remote(Request, Origin)
%%           Request   = request record()
%%           Origin    = siporigin record()
%% Descrip.: Find out where to route this request which is for a
%%           remote domain.
%% Returns : {error, Status}            |
%%           {response, Status, Reason} |
%%           {proxy, Location}          |
%%           {relay, Location}          |
%%           {forward, Location}        |
%%           none
%%--------------------------------------------------------------------
request_to_remote(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    URL = Request#request.uri,
    case local:lookup_remote_request(Request, Origin) of
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
	    logger:log(debug, "Routing: local:lookup_remote_request() ~s -> ~p", [sipurl:print(URL), Location]),
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
    %% XXX The OPTIONS response SHOULD include Accept, Accept-Encoding, Accept-Language, and
    %% Supported headers. RFC 3261 section 11.
    transactionlayer:send_response_handler(THandler, 200, "OK");

request_to_me(THandler, Request, LogTag) when is_record(Request, request) ->
    logger:log(normal, "~s: incomingproxy: non-OPTIONS request to me -> 481 Call/Transaction Does Not Exist",
	       [LogTag]),
    transactionlayer:send_response_handler(THandler, 481, "Call/Transaction Does Not Exist").

%%--------------------------------------------------------------------
%% Function: get_branchbase_from_handler(TH)
%%           TH = term(), server transaction handle
%% Descrip.: Get branch from server transaction handler and then
%%           remove the -UAS suffix. The result is used as a tag
%%           when logging actions.
%% Returns : BranchBase = string()
%%--------------------------------------------------------------------
get_branchbase_from_handler(TH) ->
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
%%           Request  = request record()
%%           DstList  = list() of sipdst record() | sipurl record() |
%%                      route
%% Descrip.: Proxy a request somewhere without authentication.
%% Returns : Does not matter
%%--------------------------------------------------------------------
proxy_request(THandler, Request, Dst) when is_record(Request, request) ->
    sippipe:start(THandler, none, Request, Dst, 900).

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
    case local:get_user_verified_proxy(Header, Method) of
	{authenticated, User} ->
	    logger:log(debug, "Relay: User ~p is authenticated", [User]),
	    logger:log(normal, "~s: incomingproxy: Relay ~s (authenticated)", [LogTag, relay_dst2str(Dst)]),
	    sippipe:start(THandler, none, Request, Dst, 900);
	{stale, User} ->
	    case local:incomingproxy_challenge_before_relay(Origin, Request, Dst) of
		false ->
		    logger:log(debug, "Relay: STALE authentication (user ~p), but local policy says we "
			       "should not challenge", [User]),
		    sippipe:start(THandler, none, Request, Dst, 900);
		true ->
		    logger:log(debug, "Relay: STALE authentication, sending challenge"),
		    logger:log(normal, "~s: incomingproxy: Relay ~s -> STALE authentication (user ~p) ->"
			       " 407 Proxy Authentication Required",
			       [LogTag, relay_dst2str(Dst), User]),
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
	    end
    end.

relay_dst2str(URI) when is_record(URI, sipurl) ->
    sipurl:print(URI);
relay_dst2str(route) ->
    "according to Route header";
relay_dst2str(_) ->
    "unknown dst".
