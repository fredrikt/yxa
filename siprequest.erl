-module(siprequest).
-export([send_redirect/3,
	 send_auth_req/4, send_proxyauth_req/4,
	 send_proxy_request/3, send_answer/3,
	 send_notavail/2, send_notfound/2, send_proxy_response/5,
	 send_result/5, send_result/6, make_answerheader/1,
	 add_record_route/1, 
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
	    end;
	[] ->
	    {Header, URI, URI}
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
    Viaadd = sipheader:via_print([{"SIP/2.0/UDP",
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
	    keylist:prepend({"Record-Route", [Route]}, Header)
    end.
add_record_route(Header) ->
    add_record_route(myhostname(), sipserver:get_env(listenport, none), Header).

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
