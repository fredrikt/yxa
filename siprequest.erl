-module(siprequest).
-export([send_redirect/3, process_register_isauth/5,
	 send_auth_req/4, send_proxyauth_req/4,
	 send_proxy_request/3, send_proxy_request/4, send_answer/3,
	 send_notavail/2, send_notfound/2, send_proxy_response/5,
	 send_result/5, send_result/6, make_answerheader/1,
	 locations_to_contacts/1]).

send_response(Socket, Code, Text, Header, Body) ->
    Via = sipheader:via(keylist:fetch("Via", Header)),
    [Dest | _] = Via,
    logger:log(debug, "send to ~p", [Dest]),
    send_response_to(Socket, Code, Text, Dest, Header, Body).

send_response_to(Socket, Code, Text, Dest, Header, Body) ->
    Line1 = "SIP/2.0 " ++ integer_to_list(Code) ++ " " ++ Text,
    Message = Line1 ++ "\r\n" ++ sipheader:build_header(Header) ++ "\r\n" ++ Body,
    logger:log(debug, "send response(~p):~n~s~n", [Dest, Message]),
    {Protocol, {Host, Port}, Parameters} = Dest,
    ok = gen_udp:send(Socket, Host, list_to_integer(default_port(Port)), Message).

default_port(none) ->
    "5060";
default_port(Port) ->
    Port.

url_to_hostport({User, Pass, InHost, InPort, Parameters}) ->
    case dnsutil:siplookup(InHost) of
	{error, nxdomain} ->
	    dnsutil:get_ip_port(InHost, default_port(InPort));
	{error, What} ->
	    {error, What};
	{Host, Port} ->
	    dnsutil:get_ip_port(Host, integer_to_list(Port));
	none ->
	    dnsutil:get_ip_port(InHost, default_port(InPort))
    end.

rewrite_route(Header, Dest) ->
    Route = sipheader:contact(keylist:fetch("Route", Header)),
    case Route of
	[{_, Newdest} | Newroute] ->
	    logger:log(debug, "Routing: New destination is ~p", [Newdest]),
	    case Newroute of
		[] ->
		    {keylist:delete("Route", Header),
		     Newdest};
		Newroute ->
		    {keylist:set("Route",
				 sipheader:contact_print(Newroute),
				 Header),
		     Newdest}
	    end;
	[] ->
	    {Header, Dest}
    end.

make_answerheader(Header) ->
    RecordRoute = keylist:fetch("Record-Route", Header),
    NewHeader1 = keylist:delete("Record-Route", Header),
    NewHeader2 = case RecordRoute of
	[] ->
	    NewHeader1;	
	_ ->
	    keylist:set("Route", RecordRoute, NewHeader1)
    end.

send_proxy_request(Header, Socket, {Action, ReqURI, Body, Parameters}, Dest) ->
    MaxForwards =
	case keylist:fetch("Max-Forwards", Header) of
	    [M] ->
		lists:min([255, list_to_integer(M) - 1]);
	    [] ->
		70
	end,
    logger:log(debug, "Max-Forwards is ~p", [MaxForwards]),
    if
	MaxForwards < 1 ->
	    logger:log(normal, "Not proxying request with Max-Forwards < 1"),
	    throw({siperror, 483, "Too Many Hops"});
	true ->
	    true
    end,
    check_valid_proxy_request(Action, Header),
    Line1 = Action ++ " " ++ sipurl:print(ReqURI) ++ " SIP/2.0",
    [Viaadd] = sipheader:via_print([{"SIP/2.0/UDP",
				     {siphost:myip(),
				      integer_to_list(sipserver:get_env(listenport, 5060))},
				     Parameters}]),
    Keylist2 = keylist:prepend({"Via", Viaadd}, Header),
    {Keylist3, Newdest} = rewrite_route(Keylist2, Dest),
    Keylist4 = keylist:set("Max-Forwards", [integer_to_list(MaxForwards)], Keylist3),
    Message = Line1 ++ "\r\n" ++ sipheader:build_header(Keylist4) ++ "\r\n" ++ Body,
    case url_to_hostport(Newdest) of
	{error, nxdomain} ->
	    logger:log(normal, "Could not resolve destination ~p (NXDOMAIN)", [Newdest]),
	    siprequest:send_result(Header, Socket, "", 604, "Does Not Exist Anywhere");
	{error, What} ->
	    logger:log(normal, "Could not resolve destination ~p (~p)", [Newdest, What]),
	    siprequest:send_result(Header, Socket, "", 500, "Could not resolve destination");
	{Host, Port} ->
	    logger:log(debug, "send request(~p,~p:~p):~n~s~n", [Newdest, Host, Port, Message]),
	    ok = gen_udp:send(Socket, Host, list_to_integer(Port), Message)
    end.

send_proxy_request(Header, Socket, {Action, Dest, Body, Parameters}) ->
    send_proxy_request(Header, Socket, {Action, Dest, Body, Parameters}, Dest).

check_valid_proxy_request("ACK", _) ->
    true;
check_valid_proxy_request("CANCEL", _) ->
    true;    
check_valid_proxy_request(Method, Header) ->
    ProxyRequire = keylist:fetch("Proxy-Require", Header),
    case ProxyRequire of
	[] ->
	    true;
	_ ->
	    logger:log(normal, "Proxy Request check: The client requires unsupported extension(s) ~p", [ProxyRequire]),
	    throw({siperror, 420, "Bad Extension", [{"Unsupported", ProxyRequire}]})
    end.

register_contact(Phone, Location, Priority, Header) ->
    {_, Contact} = Location,
    Expire = parse_register_expire(Header, Contact),
    NewContact = remove_expires_parameter(Contact),
    logger:log(normal, "REGISTER ~s at ~s (priority ~p, expire in ~p)",
		[Phone, sipurl:print(NewContact), Priority, Expire]),
    phone:insert_purge_phone(Phone, [{priority, Priority}],
			     dynamic,
			     Expire + util:timestamp(),
			     NewContact).

remove_expires_parameter(Contact) ->
    {User, _, Host, Port, _} = Contact,
    Param1 = sipheader:contact_params(Contact),
    Param2 = dict:erase("expires", Param1),
    Parameters = sipheader:dict_to_param(Param2),
    {User, none, Host, Port, Parameters}.

unregister_contact(Phone, Contact, Priority) ->
    logger:log(normal, "UN-REGISTER ~s at ~s (priority ~p)",
		[Phone, sipheader:contact_print([{none, Contact}]), Priority]),
    phone:insert_purge_phone(Phone, [{priority, Priority}],
			     dynamic,
			     util:timestamp(),
			     Contact).

process_register_isauth(Header, Socket, Phone, Auxphones, Contacts) ->
    % XXX RFC3261 says to store Call-ID and CSeq and to check these on
    % registers replacing erlier bindings (10.3 #7)

    check_valid_register_request(Header),

    % try to find a wildcard to process
    case process_register_wildcard_isauth(Header, Socket, Phone, Auxphones, Contacts) of
	none ->
	    % no wildcard, loop through all Contacts, and for all Contacts
	    % loop through all Auxphones
	    lists:map(fun (Location) ->
			register_contact(Phone, Location, 100, Header),

			lists:map(fun (Auxphone) ->
				register_contact(Auxphone, Location, 50, Header)
			    end, Auxphones)
		      
		      end, Contacts);
	_ ->
	    none
    end,
    
    FetchedContactList = fetch_contacts(Phone),
    
    send_response(Socket, 200, "OK",
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)},
		   {"Contact", FetchedContactList}], "").

check_valid_register_request(Header) ->
    Require = keylist:fetch("Require", Header),
    case Require of
	[] ->
	    true;
	_ ->
	    logger:log(normal, "Request check: The client requires unsupported extension(s) ~p", [Require]),
	    throw({siperror, 420, "Bad Extension", [{"Unsupported", Require}]})
    end.

fetch_contacts(Phone) ->
    case phone:get_phone(Phone) of
	{atomic, Locations} ->
	    locations_to_contacts(Locations);
	_ ->
	    none
    end.

locations_to_contacts([]) ->
    [];
locations_to_contacts([{Location, Flags, Class, Expire}]) ->
    [print_contact(Location, Expire)];
locations_to_contacts([{Location, Flags, Class, Expire} | Rest]) ->
    [print_contact(Location, Expire), locations_to_contacts(Rest)].

print_contact(Location, Expire) ->
    {User, _, Host, Port, Parameters} = Location,
    % make sure we don't end up with a negative Expires
    NewExpire = lists:max([0, Expire - util:timestamp()]),
    Contact = {none, {User, none, Host, Port, lists:append(Parameters, ["expires=" ++ integer_to_list(NewExpire)])}},
    sipheader:contact_print([Contact]).

process_register_wildcard_isauth(Header, Socket, Phone, Auxphones, Contacts) ->
    case is_valid_wildcard_request(Header, Contacts) of
	true ->
	    logger:log(debug, "Register: Processing valid wildcard un-register"),
	    case phone:get_phone(Phone) of
		{atomic, Entrys} ->
		    % loop through all Entrys and unregister them
		    lists:map(fun (Entry) ->
			{Location, Flags, Class, Expire} = Entry,
			Prio = case lists:keysearch(priority, 1, Flags) of
			    {value, {priority, P}} -> P;
			    _ -> 100
			end,
			unregister_contact(Phone, Location, Prio)
		      end, Entrys);
		_ ->
		    none
	    end;
	_ ->
	    none
    end.

is_valid_wildcard_request(Header, [{_, {wildcard, Parameters}}]) ->
    case keylist:fetch("Expires", Header) of
	["0"] ->
	    % A single wildcard contact with an Expires header of 0, now just check that
	    % Parameters does not contain any expire value except possibly zero
	    case dict:find("expires", sipheader:contact_params({none, {wildcard, Parameters}})) of
		{ok, E} ->
		    case E of
			"0" ->
			    true;
			_ ->
			    %logger:log(debug, "Register: Wildcard with non-zero contact expires parameter (~p), invalid IMO", [Parameters]),
			    throw({siperror, 400, "Wildcard with non-zero contact expires parameter"})
		    end;
		_ ->
		    true
	    end;
	_ ->
	    throw({siperror, 400, "Wildcard without 'Expires: 0', invalid (RFC3261 10.2.2)"})
    end;
is_valid_wildcard_request(Header, Contacts) ->
    % More than one Contacts (or just one non-wildcard), make sure there
    % are no wildcards since that would be invalid
    case wildcard_grep(Contacts) of
	true ->
	    %logger:log(debug, "Register: Wildcard present but not alone, invalid (RFC3261 10.3 #6)"),
	    throw({siperror, 400, "Wildcard present but not alone, invalid (RFC3261 10.3 #6)"});
	_ ->
	    none
    end.

wildcard_grep([]) ->
    nomatch;
wildcard_grep([{_, {wildcard, Parameters}} | Rest]) ->
    true;
wildcard_grep([Foo | Rest]) ->
    wildcard_grep(Rest).
    
parse_register_expire(Header, Contact) ->
    MaxRegisterTime = sipserver:get_env(max_register_time, 43200),
    case dict:find("expires", sipheader:contact_params(Contact)) of
	{ok, E1} ->
	    % Contact parameters has an expire value, use that
	    lists:min([MaxRegisterTime, list_to_integer(E1)]);
	error ->
	    case keylist:fetch("Expires", Header) of
		[E2] ->
		    % Request has an Expires header, use that
		    lists:min([MaxRegisterTime, list_to_integer(E2)]);
		[] ->
		    % Default expire
		    3600
	    end
    end.

send_auth_req(Header, Socket, Auth, Stale) ->
    send_response(Socket, 401, "Authentication Required",
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)},
		   {"WWW-Authenticate", sipheader:auth_print(Auth, Stale)}], "").

send_proxyauth_req(Header, Socket, Auth, Stale) ->
    send_response(Socket, 407, "Proxy Authentication Required",
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)},
		   {"Proxy-Authenticate", sipheader:auth_print(Auth, Stale)}], "").

send_redirect(Location, Header, Socket) ->
    Contact = [{none, Location}],
    send_response(Socket, 302, "Moved Temporarily",
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)},
		   {"Contact", sipheader:contact_print(Contact)}], "").

send_notfound(Header, Socket) ->
    send_response(Socket, 404, "Not found",
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)}], "").

send_notavail(Header, Socket) ->
    send_response(Socket, 480, "Temporarily unavailable",
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)},
		   {"Retry-After", ["180"]}], "").

send_answer(Header, Socket, Body) ->
    send_response(Socket, 200, "OK",
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)},
		   {"Content-Type", ["application/sdp"]},
		   {"Content-Length", [integer_to_list(length(Body))]}], Body).

send_result(Header, Socket, Body, Code, Description) ->
    send_response(Socket, Code, Description,
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)}], Body).

send_result(Header, Socket, Body, Code, Description, ExtraHeaders) ->
    send_response(Socket, Code, Description,
		  [{"via", keylist:fetch("Via", Header)},
		   {"From", keylist:fetch("From", Header)},
		   {"To", keylist:fetch("To", Header)},
		   {"Call-ID", keylist:fetch("Call-ID", Header)},
		   {"CSeq", keylist:fetch("CSeq", Header)} | ExtraHeaders], Body).

send_proxy_response(Socket, Status, Reason, Header, Body) ->
    [Self | Via] = sipheader:via(keylist:fetch("Via", Header)),
    Keylist = keylist:set("Via", sipheader:via_print(Via),
			  Header),
    send_response(Socket, Status, Reason,
		  Keylist, Body).
