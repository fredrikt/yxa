-module(incomingproxy).
-export([start/0, start/2, process/2, remove_expired_phones/0]).

start(normal, Args) ->
    Pid = spawn(incomingproxy, start, []),
    {ok, Pid}.

start() ->
    phone:init(),
    phone:create(),
    logger:start("sipd.log"),
    {ok, Socket} = gen_udp:open(5060, [{reuseaddr, true}]),
    timer:apply_interval(60000, incomingproxy, remove_expired_phones, []),
    logger:log(normal, "proxy started"),
    recvloop(Socket).

recvloop(Socket) ->
    receive
	{udp, Socket, IP, InPortNo, Packet} ->
	    spawn(incomingproxy, process, [Packet, Socket]),
	    recvloop(Socket)
    end.

process(Packet, Socket) ->
    case sippacket:parse(Packet) of
	{request, Method, URL, Header, Body} ->
	    request(Method, URL, Header, Body, Socket);
	{response, Status, Reason, Header, Body} ->
	    response(Status, Reason, Header, Body, Socket)
    end.

lookuproute(User) ->
    case phone:get_phone(User) of
	{atomic, []} ->
	    none;
	{atomic, Locations} ->
	    {Location, _, _, _} = siprequest:location_prio(Locations),
	    {proxy, Location}
    end.

lookupmail(User) ->
    case directory:lookupmail(User ++ "@kth.se") of
	none ->
	    none;
	Phone ->
	    case lookuproute(Phone) of
		none ->
		    lookupdefault(Phone);
		Loc ->
		    Loc
	    end
    end.

lookupimplicit(User) ->
    none.

isnumeric(Number) ->
    case catch list_to_integer(Number) of
	Num when integer(Num) ->
	    true;
	_ ->
	    false
    end.

globalrewrite("0000" ++ Number) ->
    "+" ++ Number;
globalrewrite("000" ++ Number) ->
    "+46" ++ Number;
globalrewrite("00" ++ Number) ->
    "+468" ++ Number;
globalrewrite("0" ++ Number) ->
    none;
globalrewrite(Number) ->
    "+468790" ++ Number.

lookupdefault(User) ->
    case isnumeric(User) of
	true ->
	    case dnsutil:enumlookup(globalrewrite(User)) of
		none ->
		    {proxy, {User, none, "sip-pstn.kth.se", none, []}};
		URL ->
		    {redirect, sipurl:parse(URL)}
	    end;
	false ->
	    none
    end.

lookupphone(User) ->
    Loc1 = lookuproute(User),
    Loc2 = case Loc1 of
	       none ->
		   lookupmail(User);
	       Loc1 ->
		   Loc1
	   end,
    Loc3 = case Loc2 of
	       none ->
		   lookupimplicit(User);
	       Loc2 ->
		   Loc2
	   end,
    case Loc3 of
	none ->
	    lookupdefault(User);
	Loc3 ->
	    Loc3
    end.

request("REGISTER", URL, Header, Body, Socket) ->
    logger:log(debug, "REGISTER"),
    To = sipheader:to(keylist:fetch("To", Header)),
    Contact = sipheader:contact(keylist:fetch("Contact", Header)),
    {_, {Phone, _, _, _, _}} = To,
    [{_, Location}] = Contact,
    case sipauth:can_register(Header, Phone) of
	true ->
	    siprequest:process_register_isauth(Header, Socket, {Phone, Location});
	stale ->
	    siprequest:send_auth_req(Header, Socket, sipauth:get_challenge(), true);
	false ->
	    siprequest:send_auth_req(Header, Socket, sipauth:get_challenge(), false)
    end;

request(Method, {User, Pass, "kth.se", Port, Parameters}, Header, Body, Socket) ->
    logger:log(normal, "~s ~s~n",
	       [Method, sipurl:print({User, Pass, "kth.se", Port, Parameters})]),
    Location = lookupphone(User),
    logger:log(debug, "Location: ~p", [Location]),
    case Location of
	none ->
	    logger:log(normal, "Not found"),
	    siprequest:send_notfound(Header, Socket);
	{proxy, Loc} ->
	    logger:log(normal, "Proxy ~s", [sipurl:print(Loc)]),
	    siprequest:send_proxy_request(Header, Socket, {Method, Loc, Body});
	{redirect, Loc} ->
	    logger:log(normal, "Redirect ~s", [sipurl:print(Loc)]),
	    siprequest:send_redirect(Loc, Header, Socket)
    end;

request(Method, URL, Header, Body, Socket) ->
%    logger:log(normal, "Redirect ~s", [sipurl:print(URL)]),
    siprequest:send_redirect(URL, Header, Socket).

response(Status, Reason, Header, Body, Socket) ->
    logger:log(normal, "Response ~p ~s", [Status, Reason]),
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).

remove_expired_phones() ->
    {atomic, Expired} = phone:expired_phones(),
    remove_phones(Expired).

remove_phones([]) ->
    true;

remove_phones([Phone | Rest]) ->
    logger:log(debug, "phoneexpire:remove ~p", [Phone]),
    phone:delete_record(Phone),
    remove_phones(Rest).
