-module(siprequest).
-export([send_redirect/3, process_register_isauth/5,
	 send_auth_req/4, send_proxyauth_req/4,
	 send_proxy_request/3, send_answer/3,
	 send_notavail/2, send_notfound/2, send_proxy_response/5,
	 send_result/5, send_result/6, make_answerheader/1,
	 locations_to_contacts/1, add_record_route/1, 
	 add_record_route/3, myhostname/0, default_port/1]).

send_response(Socket, Code, Text, Header, Body) ->
    Via = sipheader:via(keylist:fetch("Via", Header)),
    [Dest | _] = Via,
    send_response_to(Socket, Code, Text, Dest, Header, Body).

send_response_to(Socket, Code, Text, Dest, Header, Body) ->
    Line1 = "SIP/2.0 " ++ integer_to_list(Code) ++ " " ++ Text,
    Message = Line1 ++ "\r\n" ++ sipheader:build_header(Header) ++ "\r\n" ++ Body,
    {Protocol, {Host, Port}, Parameters} = Dest,
    PortInt = list_to_integer(default_port(Port)),
    SendToHost = case dict:find("received", sipheader:param_to_dict(Parameters)) of
	{ok, Received} ->
	    case regexp:first_match(Received, "^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$") of
		{match, _, _} ->
		    Received;
		_ ->
		    logger:log(debug, "Malformed received= parameter (not an IP address): ~p", [Received]),
		    Host
	    end;
	Res ->
	    Host
    end,
    logger:log(debug, "send response(~p, send to=~p:~p) :~n~s~n", [Dest, SendToHost, default_port(Port), Message]),
    case gen_udp:send(Socket, SendToHost, PortInt, Message) of
	ok ->
	    ok;
	{error, E} ->
	    logger:log(error, "Failed sending response to ~p:~p, error ~p",
			[SendToHost, PortInt, E]),
	    {senderror, E}
    end.

default_port(none) ->
    "5060";
default_port(Port) when integer(Port) ->
    integer_to_list(Port);
default_port(Port) ->
    Port.

url_to_hostport({User, Pass, InHost, InPort, Parameters}) ->
    case dnsutil:siplookup(InHost) of
	{error, nxdomain} ->
	    dnsutil:get_ip_port(InHost, default_port(InPort));
	{error, What} ->
	    {error, What};
	{Host, Port} ->
	%    InPortInt = list_to_integer(default_port(InPort)),
	%    if
	%	Port /= InPortInt ->
	%	    logger:log(debug, "Warning: When looking up ~p in DNS, port ~p became port ~p",
	%	    		[InHost, InPortInt, Port]);		
	%	true -> true
	%    end,
	    UsePort = case InPort of
		none ->
		    integer_to_list(Port);
		PortInt when integer(InPort) ->
		    integer_to_list(InPort);
		PortStr when list(InPort) ->
		    InPort
	    end,
	    DNSPortList = integer_to_list(Port),
	    if
		UsePort /= DNSPortList ->
		    logger:log(debug, "Warning: ~p is specified to use port ~s in DNS, but I'm going to use the supplied port ~s instead",
		    		[InHost, DNSPortList, UsePort]);		
		true -> true
	    end,
	    dnsutil:get_ip_port(Host, UsePort);
	none ->
	    dnsutil:get_ip_port(InHost, default_port(InPort))
    end.

rewrite_route(Header, URI) ->
    Route = sipheader:contact(keylist:fetch("Route", Header)),
    case Route of
        [{_, NewDest} | NewRoute1] ->
	    case route_matches_me({none, NewDest}) of
		true ->
		    logger:log(debug, "Routing: First Route ~p matches me, removing it.", [sipheader:contact_print([{none, NewDest}])]),
		    case NewRoute1 of
			[] ->
			    rewrite_route(keylist:delete("Route", Header), URI);
			_ ->
			    rewrite_route(keylist:set("Route", sipheader:contact_print(NewRoute1), Header), URI)
		    end;
		_ ->
		    {NewRoute, NewURI} = case is_loose_router({none, NewDest}) of
			true ->
			    {NewRoute1, URI};
			false ->
			    logger:log(debug, "Routing: Destination ~p is a strict (RFC2543) router, appending final destination URI ~p to Route header",
					[sipurl:print(NewDest), sipurl:print(URI)]),
			    NewRoute2 = lists:append(NewRoute1, [{none, URI}]),
			    {NewRoute2, NewDest}
		    end,
		    logger:log(debug, "Routing: New destination is ~p, new Request-URI is ~p", [sipurl:print(NewDest), sipurl:print(NewURI)]),
		    case NewRoute of
			[] ->
			    {keylist:delete("Route", Header), NewDest, NewURI};
			_ ->
			    {keylist:set("Route", sipheader:contact_print(NewRoute),
			     Header), NewDest, NewURI}
		    end
	    end;
	[] ->
	    {Header, URI, URI}
    end.

route_matches_me(Route) ->
    {_, {_, _, Host, RoutePort, _}} = Route,
    Port = default_port(RoutePort),
    HostnameList = lists:append(sipserver:get_env(myhostnames, []), [siphost:myip()]),
    HostnameIsMyHostname = util:casegrep(Host, HostnameList),
    MyPort = default_port(sipserver:get_env(listenport, none)),
    if
	Port /= MyPort -> false;
	HostnameIsMyHostname /= true -> false;
	true ->	true
    end.	   

is_loose_router(Route) ->
    case dict:find("lr", sipheader:contact_params(Route)) of
	{ok, E} ->
	    true;
	_ ->
	    false
    end.	    

send_proxy_request(Header, Socket, {Method, ReqURI, Body, Parameters}) ->
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
    check_valid_proxy_request(Method, Header),
    ViaHostname = myhostname(),
    [Viaadd] = sipheader:via_print([{"SIP/2.0/UDP",
				     {ViaHostname,
				      default_port(sipserver:get_env(listenport, none))},
				     Parameters}]),
    Keylist2 = keylist:prepend({"Via", Viaadd}, Header),
    {Keylist3, NewDest, NewURI} = rewrite_route(Keylist2, ReqURI),
    Keylist4 = keylist:set("Max-Forwards", [integer_to_list(MaxForwards)], Keylist3),
    Line1 = Method ++ " " ++ sipurl:print(NewURI) ++ " SIP/2.0",
    Message = Line1 ++ "\r\n" ++ sipheader:build_header(Keylist4) ++ "\r\n" ++ Body,
    case url_to_hostport(NewDest) of
	{error, nxdomain} ->
	    logger:log(normal, "Could not resolve destination ~p (NXDOMAIN)", [NewDest]),
	    siprequest:send_result(Header, Socket, "", 604, "Does Not Exist Anywhere");
	{error, What} ->
	    logger:log(normal, "Could not resolve destination ~p (~p)", [NewDest, What]),
	    siprequest:send_result(Header, Socket, "", 500, "Could not resolve destination");
	{Host, Port} ->
	    PortInt = list_to_integer(Port),
	    logger:log(debug, "send request(~p, send to=~p:~p) :~n~s~n", [NewDest, Host, PortInt, Message]),
	    case gen_udp:send(Socket, Host, PortInt, Message) of
		ok ->
		    ok;
		{error, E} ->
		    logger:log(error, "Failed sending request to ~p:~p, error ~p", [Host, PortInt, E]),
		    {error, E}
	    end
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

myhostname() ->
    case sipserver:get_env(myhostnames, []) of
	[] ->
	    siphost:myip();
	Hostnames ->
	    lists:nth(1, Hostnames)
    end.

add_record_route(Hostname, Port, Header) ->
    Route = "<" ++ sipurl:print({none, none, Hostname, default_port(Port),
				["maddr=" ++ siphost:myip(), "lr=true"]}) ++ ">",
    [CRoute] = sipheader:contact([Route]),
    case sipheader:contact(keylist:fetch("Record-Route", Header)) of
	[CRoute | _] ->
	    Header;
	_ ->
	    keylist:prepend({"Record-Route", Route}, Header)
    end.
add_record_route(Header) ->
    add_record_route(myhostname(), sipserver:get_env(listenport, none), Header).

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
    Param1 = sipheader:contact_params({none, Contact}),
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
    
    send_response(Socket, 200, "OK",
		  keylist:set("Contact", fetch_contacts(Phone),
			      keylist:copy(Header, ["Via", "From", "To",
						    "Call-ID", "CSeq"])),
		  "").

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
locations_to_contacts([{Location, Flags, Class, Expire} | Rest]) ->
    lists:append(print_contact(Location, Expire), locations_to_contacts(Rest)).

print_contact(Location, Expire) ->
    [C] = sipheader:contact_print([{none, Location}]),
    case Expire of
	never ->
	    logger:log(debug, "Not adding permanent contact ~p to REGISTER response", [C]),
	    [];
	_ ->
	    % make sure we don't end up with a negative Expires
	    NewExpire = lists:max([0, Expire - util:timestamp()]),
	    [C ++ ";expires=" ++ integer_to_list(NewExpire)]
    end.

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
    case dict:find("expires", sipheader:contact_params({none, Contact})) of
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

standardcopy(Header, ExtraHeaders) ->
    keylist:appendlist(keylist:copy(Header,
				    ["Via", "From", "To",
				     "Call-ID", "CSeq"]),
		       ExtraHeaders).

send_auth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"WWW-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    send_response(Socket, 401, "Authentication Required",
		  standardcopy(Header, ExtraHeaders),
		  "").

send_proxyauth_req(Header, Socket, Auth, Stale) ->
    ExtraHeaders = [{"Proxy-Authenticate",
		     sipheader:auth_print(Auth, Stale)}],
    send_response(Socket, 407, "Proxy Authentication Required",
		  standardcopy(Header, ExtraHeaders),
		  "").

send_redirect(Location, Header, Socket) ->
    Contact = [{none, Location}],
    ExtraHeaders = [{"Contact",
		     sipheader:contact_print(Contact)}],
    send_response(Socket, 302, "Moved Temporarily",
		  standardcopy(Header, ExtraHeaders),
		  "").
		   

send_notfound(Header, Socket) ->
    send_response(Socket, 404, "Not found",
		  standardcopy(Header, []),
		  "").

send_notavail(Header, Socket) ->
    ExtraHeaders = [{"Retry-After", "180"}],
    send_response(Socket, 480, "Temporarily unavailable",
		  standardcopy(Header, ExtraHeaders),
		  "").

send_answer(Header, Socket, Body) ->
    ExtraHeaders = [{"Content-Type", "application/sdp"},
		    {"Content-Length",
		     integer_to_list(length(Body))}],
    send_response(Socket, 200, "OK",
		  standardcopy(Header, ExtraHeaders),
		  Body).

send_result(Header, Socket, Body, Code, Description) ->
    send_response(Socket, Code, Description,
		  standardcopy(Header, []),
		  Body).

send_result(Header, Socket, Body, Code, Description, ExtraHeaders) ->
    send_response(Socket, Code, Description,
		  standardcopy(Header, ExtraHeaders),
		  Body).

send_proxy_response(Socket, Status, Reason, Header, Body) ->
    [Self | Via] = sipheader:via(keylist:fetch("Via", Header)),
    case Via of
	[] ->
	    logger:log(error, "Can't proxy response ~p ~s because it contains just one or less Via and that should be mine!",
			[Status, Reason]),
	    {error, invalid_Via};
	_ ->
	    Keylist = keylist:set("Via", sipheader:via_print(Via),
				  Header),
	    send_response(Socket, Status, Reason,
			  Keylist, Body)
    end.
