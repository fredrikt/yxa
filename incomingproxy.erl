-module(incomingproxy).
-export([start/2, remove_expired_phones/0]).

-include("database_regexproute.hrl").

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/5,
				   fun response/5, none, true]),
    {ok, Pid}.

init() ->
    phone:create(),
    database_regexproute:create(),
    timer:apply_interval(60000, ?MODULE, remove_expired_phones, []).

lookupregexproute(User) ->
    {atomic, Routes} = database_regexproute:list(),
    Sortedroutes = lists:sort(fun (Elem1, Elem2) -> 
				      Prio1 = lists:keysearch(priority, 1, Elem1#regexproute.flags),
				      Prio2 = lists:keysearch(priority, 1, Elem2#regexproute.flags),
				      case {Prio1, Prio2} of
					  {_, false} ->
					      true;
					  {false, _} ->
					      false;
					  {{value, {priority,P1}}, {value, {priority,P2}}} when P1 >= P2 ->
					      true;
					  _ ->
					      false
				      end
			      end, Routes),
    Rules = lists:map(fun(R) ->
			      {R#regexproute.regexp, sipurl:print(R#regexproute.address)}
		      end, Sortedroutes),
    case util:regexp_rewrite(User, Rules) of
	nomatch ->
	    none;
	Result ->
	    {proxy, sipurl:parse(Result)}
    end.

lookuproute(User) ->
    case phone:get_phone(User) of
	{atomic, []} ->
	    Loc = lookupregexproute(User),
	    logger:log(debug, "Routing: Phone-lookup of ~p -> ~p", [User, Loc]),
	    Loc;
	{atomic, Locations} ->
	    {Location, _, _, _} = siprequest:location_prio(Locations),
	    logger:log(debug, "Routing: Phone-lookup of ~s -> ~p", [User, Location]),
	    case Location of
		{error, Errorcode} ->
		    {error, Errorcode};
		_ ->
		    {proxy, Location}
	    end
    end.

lookupmail(User, Host) ->
    Loc1 = directory:lookupmail(User ++ "@" ++ Host),
    logger:log(debug, "Routing: lookupmail ~p @ ~p -> ~p", [User, Host, Loc1]),
    case Loc1 of
	none ->
	    none;
	Loc1 ->
	    case lookuproute(Loc1) of
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
	    dnsutil:enumlookup(E164)
    end.

lookupdefault(User) ->
    case util:isnumeric(User) of
	true ->
	    case enumlookup(User) of
		none ->
		    logger:log(debug, "Routing: Proxying to ~p @ ~p (my defaultroute)~n", [User, sipserver:get_env(defaultroute, "")]),
		    {proxy, {User, none, sipserver:get_env(defaultroute), "5060", []}};
		URL ->
		    logger:log(debug, "Routing: ENUM lookup resulted in ~p~n", [URL]),
		    {relay, sipurl:parse(URL)}
	    end;
	false ->
	    logger:log(debug, "Routing: could not route call"),
	    none
    end.

homedomain(Domain) ->
    util:casegrep(Domain, sipserver:get_env(homedomain)).

isours(Number) ->
    case phone:get_users_for_number(Number) of
	{atomic, []} ->
	    false;
	{atomic, _} ->
	    true;
	{aborted, _} ->
	    false
    end.

lookupphone(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    case homedomain(Host) of
	true ->
	    Loc1 = lookuproute(User),
	    Loc2 = case Loc1 of
		       none ->
			   lookupmail(User, Host);
		       Loc1 ->
			   Loc1
		   end,
	    case Loc2 of
		none ->
		    case isours(User) of
			true ->
			    none;
			false ->
			    lookupdefault(User)
		    end;
		Loc2 ->
		    Loc2
	    end;
	_ ->
	    {relay, URL}
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

request(Method, URL, Header, Body, Socket) ->
    logger:log(normal, "~s ~s~n",
	       [Method, sipurl:print(URL)]),
    Location = lookupphone(URL),
    logger:log(debug, "Location: ~p", [Location]),
    case Location of
	none ->
	    logger:log(normal, "Not found"),
	    siprequest:send_notfound(Header, Socket);
	{error, Errorcode} ->
	    logger:log(normal, "Error ~p", [Errorcode]),
	    siprequest:send_result(Header, Socket, "", Errorcode, "Unknown code");
	{proxy, Loc} ->
	    logger:log(normal, "Proxy ~s", [sipurl:print(Loc)]),
	    siprequest:send_proxy_request(Header, Socket, {Method, Loc, Body});
	{redirect, Loc} ->
	    logger:log(normal, "Redirect ~s", [sipurl:print(Loc)]),
	    siprequest:send_redirect(Loc, Header, Socket);
	{relay, Loc} ->
	    logger:log(normal, "Relay ~s", [sipurl:print(Loc)]),
	    sipauth:check_and_send_relay(Header, Socket, {siprequest, send_proxy_request}, {Method, Loc, Body}, Method)
    end.

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
