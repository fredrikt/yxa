-module(incomingproxy).
-export([start/2, remove_expired_phones/0]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/5,
				   fun response/5, none, true]),
    {ok, Pid}.

init() ->
    phone:create(),
    timer:apply_interval(60000, ?MODULE, remove_expired_phones, []).

lookuproute(User) ->
    case phone:get_phone(User) of
	{atomic, []} ->
	    none;
	{atomic, Locations} ->
	    {Location, _, _, _} = siprequest:location_prio(Locations),
	    {proxy, Location}
    end.

lookupmail(User) ->
    case directory:lookupmail(User) of
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
    case util:isnumeric(User) of
	true ->
	    case dnsutil:enumlookup(globalrewrite(User)) of
		none ->
		    {proxy, {User, none, sipserver:get_env(defaultroute), none, []}};
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
    case Loc2 of
	none ->
	    lookupdefault(User);
	Loc2 ->
	    Loc2
    end.

request("REGISTER", URL, Header, Body, Socket) ->
    logger:log(debug, "REGISTER"),
    To = sipheader:to(keylist:fetch("To", Header)),
    Contact = sipheader:contact(keylist:fetch("Contact", Header)),
    {_, {Phone, _, _, _, _}} = To,
    [{_, Location}] = Contact,
    case sipauth:can_register(Header, Phone) of
	{true, Numberlist} ->
	    logger:log(debug, "numberlist: ~p", [Numberlist]),
	    Auxphones = Numberlist,
	    siprequest:process_register_isauth(Header, Socket, Phone, Auxphones, Location);
	{stale, _} ->
	    siprequest:send_auth_req(Header, Socket, sipauth:get_challenge(), true);
	{false, _} ->
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
