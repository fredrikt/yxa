-module(incomingproxy).
-export([start/2, remove_expired_phones/0]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, none, true]),
    {ok, Pid}.

init() ->
%    phone:create(),
%    database_regexproute:create(),
    timer:apply_interval(60000, ?MODULE, remove_expired_phones, []).

route_request(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Loc1 = case local:homedomain(Host) of
	true ->
	    request_to_homedomain(URL);
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

request("REGISTER", URL, Header, Body, Socket, FromIP) ->
    {User, Pass, Host, Port, Parameters} = URL,
    logger:log(debug, "REGISTER ~p", [sipurl:print(URL)]),
    case local:homedomain(Host) of
	true ->
	    LogStr = sipserver:make_logstr({request, "REGISTER", URL, Header, Body}, FromIP),
	    logger:log(debug, "~s -> processing", [LogStr]),
	    % delete any present Record-Route header (RFC3261, #10.3)
	    NewHeader = keylist:delete("Record-Route", Header),
	    Contacts = sipheader:contact(keylist:fetch("Contact", Header)),
	    logger:log(debug, "Register: Contact(s) ~p", [sipheader:contact_print(Contacts)]),
	    {_, ToURL} = sipheader:to(keylist:fetch("To", Header)),
	    ToKey = local:sipuser(ToURL),
	    case sipauth:can_register(NewHeader, ToKey) of
		{true, Numberlist} ->
		    if
			Numberlist /= [] ->
			    logger:log(debug, "~s -> Registering user ~p (numbers: ~p)", [LogStr, ToKey, Numberlist]);
			true ->
			    logger:log(debug, "~s -> Registering user ~p", [LogStr, ToKey])
		    end,
		    siprequest:process_register_isauth(NewHeader, Socket, ToKey, Numberlist, Contacts);
		{stale, _} ->
		    logger:log(normal, "~s -> Authentication is STALE, sending new challenge", [LogStr]),
		    siprequest:send_auth_req(NewHeader, Socket, sipauth:get_challenge(), true);
		{false, _} ->
		    logger:log(normal, "~s -> Authentication FAILED, sending challenge", [LogStr]),
		    siprequest:send_auth_req(NewHeader, Socket, sipauth:get_challenge(), false)
	    end;
	_ ->
	    logger:log(debug, "REGISTER for non-homedomain ~p", [Host]),
	    do_request("REGISTER", URL, Header, Body, Socket, FromIP)
    end;

request(Method, URL, Header, Body, Socket, FromIP) ->
    do_request(Method, URL, Header, Body, Socket, FromIP).

do_request(Method, URL, OrigHeader, Body, Socket, FromIP) ->
    logger:log(debug, "~s ~s~n",
	       [Method, sipurl:print(URL)]),
    Header = case sipserver:get_env(record_route, false) of
	true -> siprequest:add_record_route(OrigHeader);
	false -> OrigHeader
    end,
    Location = route_request(URL),
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
	    siprequest:send_proxy_request(Header, Socket, {Method, Loc, Body, []});
	{redirect, Loc} ->
	    logger:log(normal, "~s -> Redirect ~s", [LogStr, sipurl:print(Loc)]),
	    siprequest:send_redirect(Loc, Header, Socket);
	{relay, Loc} ->
	    logger:log(normal, "~s -> Relay ~s", [LogStr, sipurl:print(Loc)]),
	    sipauth:check_and_send_relay(Header, Socket, {siprequest, send_proxy_request}, {Method, Loc, Body, []}, Method);
	{forward, Host, Port} ->
	    logger:log(normal, "~s -> Forward to ~p", [LogStr, sipurl:print_hostport(Host, Port)]),
	    [AddRoute] = sipheader:contact_print([{none, {none, none, Host, Port, ["lr=true"]}}]),
	    NewHeader = keylist:prepend({"Route", AddRoute}, Header),
	    siprequest:send_proxy_request(NewHeader, Socket, {Method, URL, Body, []});
	_ ->
	    logger:log(error, "~s -> Invalid Location ~p", [LogStr, Location]),
	    siprequest:send_result(Header, Socket, "", 500, "Internal Server Error")
    end.

response(Status, Reason, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    logger:log(normal, "Response to ~s: ~p ~s", [LogStr, Status, Reason]),
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).

request_to_homedomain(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Key = local:sipuser(URL),
    logger:log(debug, "Routing: Request to homedomain, sipuser ~p", [Key]),

    Loc1 = local:lookupuser(URL),
    logger:log(debug, "Routing: lookupuser on ~p -> ~p", [sipurl:print(URL), Loc1]),

    case Loc1 of
	none ->
	    case local:isours(URL) of
		true ->
		    logger:log(debug, "Routing: ~p is one of our users, returning Temporarily Unavailable", [User]),
		    {response, 480, "Users location currently unknown"};
		false ->
		    request_to_homedomain_not_sipuser(URL)
	    end;
	Loc1 ->
	    Loc1
    end.

request_to_homedomain_not_sipuser(URL) ->
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
	_ ->
	    Loc1
    end.

request_to_remote(URL) ->
    case local:lookup_remote_url(URL) of
	none ->
	    {_, _, Host, _, _} = URL,
	    logger:log(debug, "Routing: ~p is not a local domain, relaying", [Host]),
	    {relay, URL};
	Location ->
	    logger:log(debug, "Routing: local:lookup_remote_url() ~s -> ~p", [sipurl:print(URL), Location]),
	    Location
    end.

remove_expired_phones() ->
    {atomic, Expired} = phone:expired_phones(),
    remove_phones(Expired).

remove_phones([]) ->
    true;

remove_phones([Phone | Rest]) ->
    {Number, Location, Class} = Phone,
    logger:log(normal, "Expire: User ~p contact ~p has expired", [Number, sipheader:contact_print([{none, Location}])]),
    phone:delete_phone(Number, Class, Location),
    remove_phones(Rest).
