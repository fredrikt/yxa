-module(incomingproxy).
-export([start/2]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, none]),
    {ok, Pid}.

init() ->
    timer:apply_interval(60000, siplocation, remove_expired_phones, []).

route_request(Request) ->
    {Method, URL, Header, Body} = Request,
    {User, Pass, Host, Port, Parameters} = URL,
    Loc1 = case local:homedomain(Host) of
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
    end.

is_request_to_me(_, {none, _, _, _, _}, Header) ->
    true;
is_request_to_me("OPTIONS", URL, Header) ->
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
is_request_to_me(_, _, _) ->
    false.

request("REGISTER", URL, Header, Body, Socket, FromIP) ->
    {User, Pass, Host, Port, Parameters} = URL,
    logger:log(debug, "REGISTER ~p", [sipurl:print(URL)]),
    case local:homedomain(Host) of
	true ->
	    LogStr = sipserver:make_logstr({request, "REGISTER", URL, Header, Body}, FromIP),
	    logger:log(debug, "~s -> processing", [LogStr]),
	    % delete any present Record-Route header (RFC3261, #10.3)
	    NewHeader = keylist:delete("Record-Route", Header),
	    {_, ToURL} = sipheader:to(keylist:fetch("To", Header)),
	    case local:can_register(NewHeader, sipurl:print(ToURL)) of
		{true, SIPuser} ->
		    Contacts = sipheader:contact(keylist:fetch("Contact", Header)),
		    logger:log(debug, "Register: Contact(s) ~p", [sipheader:contact_print(Contacts)]),
		    logger:log(debug, "~s -> Registering contacts for SIPuser ~p", [LogStr, SIPuser]),
		    siplocation:process_register_isauth(NewHeader, Socket, SIPuser, Contacts);
		{stale, _} ->
		    logger:log(normal, "~s -> Authentication is STALE, sending new challenge", [LogStr]),
		    siprequest:send_auth_req(NewHeader, Socket, sipauth:get_challenge(), true);
		{false, _} ->
		    Prio = case keylist:fetch("Authorization", Header) of
			[] -> debug;
			_ -> normal
		    end,
		    logger:log(Prio, "~s -> Authentication FAILED, sending challenge", [LogStr]),
		    siprequest:send_auth_req(NewHeader, Socket, sipauth:get_challenge(), false);
		Unknown ->
		    logger:log(error, "Register: Unknown result from local:can_register() URL ~p :~n~p",
		    		[sipurl:print(URL), Unknown]),
		    throw({siperror, 500, "Server Internal Error"})
	    end;
	_ ->
	    logger:log(debug, "REGISTER for non-homedomain ~p", [Host]),
	    do_request({"REGISTER", URL, Header, Body}, Socket, FromIP)
    end;

request("ACK", URL, Header, Body, Socket, FromIP) ->
    logger:log(debug, "ACK ~p", [sipurl:print(URL)]),
    ToTag = sipheader:get_tag(keylist:fetch("To", Header)),
    case ToTag of
	"z9hG4bK-yxa-" ++ Foo ->
	    case lists:suffix(siprequest:myhostname() ++ "-drop-ACK", Foo) of
		true ->
		    logger:log(debug, "Dropping ACK of my challenge"),
		    LogStr = sipserver:make_logstr({request, "ACK", URL, Header, Body}, FromIP),
		    logger:log(debug, "~s -> dropped", [LogStr]);
		false ->
		    do_request({"ACK", URL, Header, Body}, Socket, FromIP)	
	    end;
	_ ->
	    do_request({"ACK", URL, Header, Body}, Socket, FromIP)
    end;

request("CANCEL", URL, Header, Body, Socket, FromIP) ->
    do_request({"CANCEL", URL, Header, Body}, Socket, FromIP);

% Here we have a request that is not CANCEL or ACK (or REGISTER).
% Check if the From: address matches our homedomains, and if so
% call verify_homedomain_user() to make sure the user is
% authorized and authenticated to use this From: address
request(Method, URL, Header, Body, Socket, FromIP) ->
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    {User, Pass, Host, Port, Parameters} = FromURI,
    case local:homedomain(Host) of
	true ->
	    LogStr = sipserver:make_logstr({request, Method, URL, Header, Body}, FromIP),
	    Request = {Method, URL, Header, Body},
	    case verify_homedomain_user(Socket, Request, LogStr) of
		true ->
		    do_request({Method, URL, Header, Body}, Socket, FromIP);
		false ->
		    logger:log(normal, "~s -> 403 Forbidden", [LogStr]),
		    siprequest:send_result(Header, Socket, "", 403, "Forbidden")
	    end;
	_ ->
	    do_request({Method, URL, Header, Body}, Socket, FromIP)
    end.

verify_homedomain_user(Socket, Request, LogStr) ->
    {Method, URI, Header, Body} = Request,
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
		    logger:log(debug, "Request from homedomain user: STALE authentication, sending challenge"),
		    siprequest:send_proxyauth_req(dropack_to(Header), Socket, sipauth:get_challenge(), true);
		false ->
		    Prio = case keylist:fetch("Proxy-Authenticate", Header) of
			[] -> debug;
			_ -> normal
		    end,
		    logger:log(Prio, "~s -> Request from unauthorized homedomain user, sending challenge",
		    		[LogStr]),
		    siprequest:send_proxyauth_req(dropack_to(Header), Socket, sipauth:get_challenge(), false);
		Unknown ->
		    logger:log(error, "request: Unknown result from local:get_user_verified_proxy() :~n~p", [Unknown]),
		    throw({siperror, 500, "Server Internal Error"})
	    end;
	_ ->
	    true
    end.

do_request(Request, Socket, FromIP) ->
    {Method, URL, OrigHeader, Body} = Request,
    logger:log(debug, "~s ~s~n",
	       [Method, sipurl:print(URL)]),
    Header = case sipserver:get_env(record_route, false) of
	true -> siprequest:add_record_route(OrigHeader);
	false -> OrigHeader
    end,
    Location = route_request({Method, URL, Header, Body}),
    logger:log(debug, "Location: ~p", [Location]),
    LogStr = sipserver:make_logstr({request, Method, URL, Header, Body}, FromIP),
    case Location of
	none ->
	    logger:log(normal, "~s -> Not found", [LogStr]),
	    siprequest:send_notfound(Header, Socket);
	{error, Errorcode} ->
	    logger:log(normal, "~s -> Error ~p", [LogStr, Errorcode]),
	    siprequest:send_result(Header, Socket, "", Errorcode, "Unknown code");
	{response, Returncode, Text} ->
	    logger:log(normal, "~s -> Response ~p ~s", [LogStr, Returncode, Text]),
	    siprequest:send_result(Header, Socket, "", Returncode, Text);
	{proxy, Loc} ->
	    logger:log(normal, "~s -> Proxy ~s", [LogStr, sipurl:print(Loc)]),
	    siprequest:send_proxy_request(Socket, Request, Loc, []);
	{redirect, Loc} ->
	    logger:log(normal, "~s -> Redirect ~s", [LogStr, sipurl:print(Loc)]),
	    siprequest:send_redirect(Loc, Header, Socket);
	{relay, Loc} ->
	    logger:log(normal, "~s -> Relay ~s", [LogStr, sipurl:print(Loc)]),
	    relay_request(Socket, Request, Loc, LogStr);
	{forward, Host, Port} ->
	    logger:log(normal, "~s -> Forward to ~p", [LogStr, sipurl:print_hostport(Host, Port)]),
	    AddRoute = sipheader:contact_print([{none, {none, none, Host, Port, ["lr=true"]}}]),
	    NewHeader = keylist:prepend({"Route", AddRoute}, Header),
	    NewRequest = {Method, URL, NewHeader, Body},
	    siprequest:send_proxy_request(Socket, NewRequest, URL, []);
	{me} ->
	    request_to_me(Socket, Request, LogStr);
	_ ->
	    logger:log(error, "~s -> Invalid Location ~p", [LogStr, Location]),
	    siprequest:send_result(Header, Socket, "", 500, "Server Internal Error")
    end.

relay_request(Socket, {"ACK", OrigURI, Header, Body}, URI, LogStr) ->
    Request = {"ACK", OrigURI, Header, Body},
    logger:log(normal, "~s -> Relay ~s", [LogStr, sipurl:print(URI)]),
    siprequest:send_proxy_request(Socket, Request, URI, []);

relay_request(Socket, {"CANCEL", OrigURI, Header, Body}, URI, LogStr) ->
    Request = {"CANCEL", OrigURI, Header, Body},
    logger:log(normal, "~s -> Relay ~s", [LogStr, sipurl:print(URI)]),
    siprequest:send_proxy_request(Socket, Request, URI, []);

relay_request(Socket, {"BYE", OrigURI, Header, Body}, URI, LogStr) ->
    Request = {"BYE", OrigURI, Header, Body},
    logger:log(normal, "~s -> Relay ~s", [LogStr, sipurl:print(URI)]),
    siprequest:send_proxy_request(Socket, Request, URI, []);

relay_request(Socket, Request, DstURI, LogStr) ->
    {Method, URI, Header, Body} = Request,
    case sipauth:get_user_verified_proxy(Header, Method) of
	{authenticated, User} ->
	    logger:log(debug, "Relay: User ~p is authenticated", [User]),
	    logger:log(normal, "~s -> Relay ~s", [LogStr, sipurl:print(DstURI)]),
	    siprequest:send_proxy_request(Socket, Request, DstURI, []);
	stale ->
	    logger:log(debug, "Relay: STALE authentication, sending challenge"),
	    siprequest:send_proxyauth_req(dropack_to(Header), Socket, sipauth:get_challenge(), true);
	false ->
	    logger:log(debug, "Relay: Sending challenge"),
	    logger:log(normal, "~s -> 407 Proxy Authorization Required", [LogStr]),
	    siprequest:send_proxyauth_req(dropack_to(Header), Socket, sipauth:get_challenge(), false);
	Unknown ->
	    logger:log(error, "relay_request: Unknown result from sipauth:get_user_verified_proxy() :~n~p", [Unknown]),
	    throw({siperror, 500, "Server Internal Error"})
    end.

response(Status, Reason, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    logger:log(normal, "Response to ~s: ~p ~s", [LogStr, Status, Reason]),
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).

request_to_homedomain(URL) ->
    request_to_homedomain(URL, init).

request_to_homedomain(URL, Recursing) ->
    {User, Pass, Host, Port, Parameters} = URL,
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

request_to_homedomain_not_sipuser(URL, loop) ->
    none;
request_to_homedomain_not_sipuser(URL, init) ->
    {User, Pass, Host, Port, Parameters} = URL,

    Loc1 = local:lookup_homedomain_url(URL),
    logger:log(debug, "Routing: local:lookup_homedomain_url on ~s -> ~p", [sipurl:print(URL), Loc1]),

    case Loc1 of
	none ->
	    % local:lookuppotn() returns 'none' if argument is not numeric,
	    % so we don't have to check that...
	    Res1 = local:lookuppotn(User),
	    logger:log(debug, "Routing: lookuppotn on ~s -> ~p", [User, Res1]),
	    Res1;
	{proxy, NewURL} ->
	    logger:log(debug, "Routing: request_to_homedomain_not_sipuser: Calling request_to_homedomain on result of local:lookup_homedomain_url (local URL ~s)",
	    		[sipurl:print(NewURL)]),
	    request_to_homedomain(NewURL, loop);
	{relay, Dst} ->
	    logger:log(debug, "Routing: request_to_homedomain_not_sipuser: Turning relay into proxy, original request was to a local domain"),
	    {proxy, Dst};
	_ ->
	    Loc1
    end.

request_to_me(Socket, {"OPTIONS", URI, Header, Body}, LogStr) ->
    logger:log(normal, "~s (to me) -> 200 OK", [LogStr]),
    logger:log(debug, "XXX The OPTIONS response SHOULD include Accept, Accept-Encoding, Accept-Language, and Supported headers. RFC 3261 section 11"),
    siprequest:send_result(Header, Socket, "", 200, "OK");

request_to_me(Socket, Request, LogStr) ->
    {Method, URI, Header, Body} = Request,
    logger:log(debug, "Routing: Dropping non-OPTIONS request to me - I have no state"),
    logger:log(normal, "~s -> 481 Call/Transaction Does Not Exist", [LogStr]),
    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist").

request_to_remote(URL) ->
    case local:lookup_remote_url(URL) of
	none ->
	    case local:get_user_with_contact(URL) of
		none ->
		    {_, _, Host, _, _} = URL,
		    logger:log(debug, "Routing: ~p is not a local domain, relaying", [Host]),
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

% Set a To-tag that includes a magic cookie to recognize the ACK of one of our
% challenges without having to keep state, except if there already is a to-tag
dropack_to(Header) ->
    To = keylist:fetch("To", Header),
    case sipheader:get_tag(To) of
	none ->
	    {DistplayName, ToURI} = sipheader:to(To),
	    MyHostname = siprequest:myhostname(),
	    NewTo = lists:concat([sipheader:to_print({DistplayName, ToURI}), ";tag=",
	    			siprequest:generate_branch(), "-", MyHostname, "-drop-ACK"]),
	    keylist:set("To", [NewTo], Header);
	_ ->
	    Header
    end.
