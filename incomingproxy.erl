-module(incomingproxy).
-export([start/2, remove_expired_phones/0]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, none, true]),
    {ok, Pid}.

init() ->
    phone:create(),
    database_regexproute:create(),
    timer:apply_interval(60000, ?MODULE, remove_expired_phones, []).

route_request(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    case lookup:homedomain(Host) of
	true ->
	    request_to_homedomain(URL);
	_ ->
	    request_to_remote(URL)
    end.

lookupmail(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Loc1 = local:lookup_homedomain_url(URL),
    logger:log(debug, "Routing: lookupmail ~p @ ~p -> ~p", [User, Host, Loc1]),
    case Loc1 of
	none ->
	    none;
	Loc1 ->
	    case lookup:lookupuser(Loc1) of
		none ->
		    lookupdefault(Loc1);
		Loc2 ->
		    Loc2
	    end
    end.

enumlookup(Number) ->
    case util:regexp_rewrite(Number, sipserver:get_env(internal_to_e164, [])) of
	nomatch ->
	    none;
	E164 ->
	    Res = dnsutil:enumlookup(E164),
	    logger:log(normal, "ENUM: ~p -> ~p", [E164, Res]),
	    Res
    end.

lookupdefault(User) ->
    case util:isnumeric(User) of
	true ->
	    case enumlookup(User) of
		none ->
		    logger:log(debug, "Routing: Proxying to ~p @ ~p (my defaultroute)~n", [User, sipserver:get_env(defaultroute, "")]),
		    {proxy, {User, none, sipserver:get_env(defaultroute), none, []}};
		URL ->
		    logger:log(debug, "Routing: ENUM lookup resulted in ~p~n", [URL]),
		    {relay, sipurl:parse(URL)}
	    end;
	false ->
	    logger:log(debug, "Routing: could not route call"),
	    none
    end.

request("REGISTER", URL, Header, Body, Socket, FromIP) ->
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

request(Method, URL, Header, Body, Socket, FromIP) ->
    logger:log(debug, "~s ~s~n",
	       [Method, sipurl:print(URL)]),
    Location = route_request(URL),
    logger:log(debug, "Location: ~p", [Location]),
    case Location of
	none ->
	    logger:log(normal, "~s ~s -> Not found", [Method, sipurl:print(URL)]),
	    siprequest:send_notfound(Header, Socket);
	{error, Errorcode} ->
	    logger:log(normal, "~s ~s Error ~p", [Method, sipurl:print(URL), Errorcode]),
	    siprequest:send_result(Header, Socket, "", Errorcode, "Unknown code");
	{response, Returncode, Text} ->
	    logger:log(normal, "~s ~s -> Response ~p ~s", [Method, sipurl:print(URL), Returncode, Text]),
	    siprequest:send_result(Header, Socket, "", Returncode, Text);
	{proxy, Loc} ->
	    logger:log(normal, "~s ~s -> Proxy ~s", [Method, sipurl:print(URL), sipurl:print(Loc)]),
	    siprequest:send_proxy_request(Header, Socket, {Method, Loc, Body, []});
	{redirect, Loc} ->
	    logger:log(normal, "~s ~s -> Redirect ~s", [Method, sipurl:print(URL), sipurl:print(Loc)]),
	    siprequest:send_redirect(Loc, Header, Socket);
	{relay, Loc} ->
	    logger:log(normal, "~s ~s -> Relay ~s", [Method, sipurl:print(URL), sipurl:print(Loc)]),
	    sipauth:check_and_send_relay(Header, Socket, {siprequest, send_proxy_request}, {Method, Loc, Body, []}, Method);
	_ ->
	    logger:log(debug, "Don't know how to process Location ~p", [Location]),
	    siprequest:send_result(Header, Socket, "", 500, "Internal Server Error")
    end.

response(Status, Reason, Header, Body, Socket, FromIP) ->
    logger:log(normal, "Response ~p ~s", [Status, Reason]),
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).

request_to_homedomain(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Key = local:sipuser(URL),
    logger:log(debug, "Routing: ~p is a local domain", [Host]),
    Loc1 = lookup:lookupuser(Key),
    logger:log(debug, "Routing: lookuproute on ~p -> ~p", [Key, Loc1]),
    Loc2 = case Loc1 of
	       none ->
		   lookupmail(URL);
	       Loc1 ->
		   Loc1
	   end,
    case Loc2 of
	none ->
	    case lookup:isours(Key) of
		true ->
		    logger:log(debug, "Routing: ~p is one of our users, returning 480 Users location currently unknown", [Key]),
		    {response, 480, "Users location currently unknown"};
		false ->
		    logger:log(debug, "Routing: ~p isn't one of our users - routing towards default", [Key]),
		    lookupdefault(Key)
	    end;
	Loc2 ->
	    Loc2
    end.

request_to_remote(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    logger:log(debug, "Routing: ~p is not a local domain, relaying", [Host]),
    {relay, URL}.

remove_expired_phones() ->
    {atomic, Expired} = phone:expired_phones(),
    remove_phones(Expired).

remove_phones([]) ->
    true;

remove_phones([Phone | Rest]) ->
    logger:log(debug, "phoneexpire:remove ~p", [Phone]),
    phone:delete_record(Phone),
    remove_phones(Rest).
