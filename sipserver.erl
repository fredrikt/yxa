-module(sipserver).
-export([start/5, process/6, get_env/1, get_env/2, make_logstr/2, safe_spawn/2, safe_spawn_child/2]).

start(InitFun, RequestFun, ResponseFun, RemoteMnesiaTables, _LocalTablesP) ->
    mnesia:start(),
    apply(InitFun, []),
    logger:start(),
    case RemoteMnesiaTables of
	none ->
	    logger:log(normal, "proxy started");
	_ ->
	    case mnesia:change_config(extra_db_nodes,
				 sipserver:get_env(databaseservers)) of
		{error, Reason} ->
		    logger:log(error, "Startup: Could not add configured databaseservers: ~p", [mnesia:error_description(Reason)]);
		_ ->
		    true
	    end,
	    {Message, Args} = case mnesia:wait_for_tables(RemoteMnesiaTables, infinity) of
				  ok ->
				      {"proxy started, all tables found", []};
				  {timeout, BadTabList} ->
				      {"proxy started, tables not reachable right now: ~p", BadTabList}
			      end,
	    logger:log(normal, Message, Args)
    end,
    {ok, Socket} = gen_udp:open(sipserver:get_env(listenport, 5060), [{reuseaddr, true}]),
    recvloop(Socket, RequestFun, ResponseFun).

recvloop(Socket, RequestFun, ResponseFun) ->
    receive
	{udp, Socket, IPlist, InPortNo, Packet} ->
	    safe_spawn(fun process/6, [Packet, Socket, IPlist, InPortNo, RequestFun, ResponseFun]),
	    recvloop(Socket, RequestFun, ResponseFun)
    end.

safe_spawn(Function, Arguments) ->
    spawn(?MODULE, safe_spawn_child, [Function, Arguments]).

safe_spawn_child(Function, Arguments) ->
    case catch apply(Function, Arguments) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from ~p :~n~p", [Function, E]),
	    true;
	_ ->
	    true
    end.

internal_error(Header, Socket) ->
    siprequest:send_result(Header, Socket, "", 500, "Server Internal Error").

internal_error(Header, Socket, Code, Text) ->
    siprequest:send_result(Header, Socket, "", Code, Text).

internal_error(Header, Socket, Code, Text, ExtraHeaders) ->
    siprequest:send_result(Header, Socket, "", Code, Text, ExtraHeaders).

process(Packet, Socket, IPlist, InPortNo, RequestFun, ResponseFun) ->
    IP = siphost:makeip(IPlist),
    case parse_packet(Socket, Packet, IP, InPortNo) of
	{{request, Method, URL, Header, Body}, LogStr} ->
%	    logger:log(debug, "~s from ~s:~p", [Method, IP, InPortNo]),
	    case catch apply(RequestFun, [Method, URL, Header, Body, Socket, IP]) of
		{'EXIT', E} ->
		    logger:log(error, "=ERROR REPORT==== from RequestFun~n~p", [E]),
		    internal_error(Header, Socket);
		{siperror, Code, Text} ->
		    logger:log(error, "INVALID request: ~s -> ~p ~s", [LogStr, Code, Text]),
		    internal_error(Header, Socket, Code, Text);
		{siperror, Code, Text, ExtraHeaders} ->
		    logger:log(error, "INVALID request: ~s -> ~p ~s", [LogStr, Code, Text]),
		    internal_error(Header, Socket, Code, Text, ExtraHeaders);
		_ ->
		    true
	    end;
	{{response, Status, Reason, Header, Body}, LogStr} ->
	    apply(ResponseFun, [Status, Reason, Header, Body, Socket, IP]);
	_ ->
	    true
    end.

parse_packet(Socket, Packet, IP, InPortNo) ->
    case catch sippacket:parse(Packet, IP, InPortNo) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from sippacket:parse()~n~p", [E]),
	    logger:log(error, "CRASHED parsing packet [client=~s]", [IP]),
	    false;
	{siperror, Code, Text} ->
	    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE", [IP, Code, Text]),
	    false;
	{siperror, Code, Text, ExtraHeaders} ->
	    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE", [IP, Code, Text]),
	    false;
	Parsed ->
	    % From here on, we can generate responses to the UAC on error
	    case catch process_parsed_packet(Socket, Parsed, IP, InPortNo) of
		{'EXIT', E} ->
		    logger:log(error, "=ERROR REPORT==== from sipserver:process_parsed_packet() :~n~p", [E]),
		    {error};
		{sipparseerror, Header, Code, Text} ->
		    logger:log(error, "INVALID request [client=~s] ~p ~s", [IP, Code, Text]),
		    internal_error(Header, Socket, Code, Text);
		{sipparseerror, Header, Code, Text, ExtraHeaders} ->
		    logger:log(error, "INVALID request [client=~s]: ~s -> ~p ~s", [IP, Code, Text]),
		    internal_error(Header, Socket, Code, Text, ExtraHeaders);
		{siperror, Code, Text} ->
		    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE", [IP, Code, Text]),
		    false;
		{siperror, Code, Text, ExtraHeaders} ->
		    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE", [IP, Code, Text]),
		    false;
		Res ->
		    Res
	    end
    end.

process_parsed_packet(Socket, {request, Method, URI, Header, Body}, IP, InPortNo) ->
    check_packet({request, Method, URI, Header, Body}, IP),
    % Check "sent-by" in top-Via to see if we MUST add a
    % received= parameter (RFC 3261 18.2.1)
    {TopViaProtocol, {TopViaHost, TopViaPort}, TopViaParameters} = topvia(Header),
    NewHeader1 = case TopViaHost of
	IP ->
	    Header;
	_ ->
	    NewParameters = lists:append(TopViaParameters, ["received=" ++ IP]),
	    NewVia = {TopViaProtocol, {TopViaHost, TopViaPort}, NewParameters},
	    logger:log(debug, "Sipserver: TopViaHost ~p does not match IP ~p, appending received=~s parameter", [TopViaHost, IP, IP]),
	    [FirstVia | Via] = sipheader:via(keylist:fetch("Via", Header)),
	    keylist:set("Via", sipheader:via_print(lists:append([NewVia], Via)), Header)
    end,
    {{_, NewURI}, NewHeader} = case received_from_strict_router(URI, NewHeader1) of
	true ->
	    logger:log(debug, "Sipserver: Received request with a Request-URI I (probably) put in a Record-Route. Pop real Request-URI from Route-header."),
	    ReverseRoute = lists:reverse(sipheader:contact(keylist:fetch("Route", NewHeader1))),
	    [NewReqURI | NewReverseRoute] = ReverseRoute,
	    case NewReverseRoute of
                [] ->
                    {NewReqURI, keylist:delete("Route", NewHeader1)};
                _ ->
                    {NewReqURI, keylist:set("Route", sipheader:contact_print(lists:reverse(NewReverseRoute)), NewHeader1)}
            end;
	_ ->
	    {{none, URI}, NewHeader1}
    end,
    NewHeader2 = remove_route_matching_me(NewHeader),
    LogStr = make_logstr({request, Method, NewURI, NewHeader2, Body}, IP),
    {{request, Method, NewURI, NewHeader2, Body}, LogStr};
process_parsed_packet(Socket, {response, Status, Reason, Header, Body}, IP, InPortNo) ->
    check_packet({response, Status, Reason, Header, Body}, IP),
    % Check that top-Via is ours (RFC 3261 18.1.2),
    % silently drop message if it is not.
    ViaHostname = siprequest:myhostname(),
    MyViaNoParam = {"SIP/2.0/UDP", {ViaHostname,
				siprequest:default_port(sipserver:get_env(listenport, none))}, []},
				{Protocol, {Host, Port}, _} = topvia(Header),
    TopViaNoParam = {Protocol, {Host, siprequest:default_port(Port)}, []},
    case TopViaNoParam of
        MyViaNoParam ->
	    LogStr = make_logstr({response, Status, Reason, Header, Body}, IP),
	    {{response, Status, Reason, Header, Body}, LogStr};
	_ ->
	    logger:log(error, "INVALID top-Via in response [client=~s]. Top-Via (~s (without parameters)) does not match mine (~s). Discarding.",
			[IP, sipheader:via_print([TopViaNoParam]), sipheader:via_print([MyViaNoParam])]),
	    {invalid}
    end.

received_from_strict_router(URI, Header) ->
    MyPort = siprequest:default_port(sipserver:get_env(listenport, none)),
    MyIP = siphost:myip(),
    {User, Pass, Host, URIPort, Parameters} = URI,
    HostnameList = lists:append(sipserver:get_env(myhostnames, []), [siphost:myip()]),
    HostnameIsMyHostname = util:casegrep(Host, HostnameList),
    Port = siprequest:default_port(URIPort),
    MAddrMatch = case dict:find("maddr", sipheader:param_to_dict(Parameters)) of
	{ok, MyIP} -> true;
	_ -> false
    end,
    HeaderHasRoute = case keylist:fetch("Route", Header) of
	[] -> false;
	_ -> true
    end,
    if
	HostnameIsMyHostname /= true -> false;
	Port /= MyPort -> false;
	% Some SIP-stacks evidently strip parameters
	%MAddrMatch /= true -> false;
	HeaderHasRoute /= true -> false;
	true -> true
    end.

remove_route_matching_me(Header) ->
    Route = sipheader:contact(keylist:fetch("Route", Header)),
    case Route of
        [{_, FirstRoute} | NewRoute] ->
	    case route_matches_me({none, FirstRoute}) of
		true ->
		    logger:log(debug, "Sipserver: First Route ~p matches me, removing it.",
		    		[sipheader:contact_print([{none, FirstRoute}])]),
		    case NewRoute of
			[] ->
			    keylist:delete("Route", Header);
			_ ->
			    keylist:set("Route", sipheader:contact_print(NewRoute), Header)
		    end;
		_ ->
		    Header
	    end;
	_ ->
	    Header
    end.

route_matches_me(Route) ->
    {_, {_, _, Host, RoutePort, _}} = Route,
    Port = siprequest:default_port(RoutePort),
    HostnameList = lists:append(get_env(myhostnames, []), [siphost:myip()]),
    HostnameIsMyHostname = util:casegrep(Host, HostnameList),
    MyPort = siprequest:default_port(get_env(listenport, none)),
    if
	Port /= MyPort -> false;
	HostnameIsMyHostname /= true -> false;
	true ->	true
    end.	   

topvia(Header) ->
    Via = sipheader:via(keylist:fetch("Via", Header)),
    [TopVia | _] = Via,
    TopVia.

check_packet({request, Method, URI, Header, Body}, IP) ->
    check_supported_uri_scheme(URI, Header),
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    sanity_check_uri("From:", FromURI, Header),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
    sanity_check_uri("To:", ToURI, Header),
    case sipheader:cseq(keylist:fetch("CSeq", Header)) of
	{unparseable, CSeqStr} ->
	    logger:log(error, "INVALID CSeq ~p in packet from ~s", [CSeqStr, IP]),
	    throw({sipparseerror, Header, 400, "Invalid CSeq"});
	{CSeqNum, CSeqMethod} ->
	    case util:isnumeric(CSeqNum) of
		false ->
		    throw({sipparseerror, Header, 400, "CSeq number " ++ CSeqNum ++ " is not an integer"});	
		_ -> true
	    end,
	    if
		CSeqMethod /= Method ->
		    throw({sipparseerror, Header, 400, "CSeq Method " ++ CSeqMethod ++ " does not match request Method " ++ Method});
		true -> true
	    end
    end;
check_packet({response, Status, Reason, Header, Body}, IP) ->
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    sanity_check_uri("From:", FromURI, Header),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
    sanity_check_uri("To:", ToURI, Header).

make_logstr({request, Method, URI, Header, Body}, IP) ->
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
    io_lib:format("~s ~s [client=~s, from=<~s>, to=<~s>]", 
		[Method, sipurl:print(URI), IP, url2str(FromURI), url2str(ToURI)]);
make_logstr({response, Status, Reason, Header, Body}, IP) ->
    {_, CSeqMethod} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
    case keylist:fetch("Warning", Header) of
	[] ->
	    io_lib:format("~s [client=~s, from=<~s>, to=<~s>]", 
			[CSeqMethod, IP, url2str(FromURI), url2str(ToURI)]);
	[Warning] ->
	    io_lib:format("~s [client=~s, from=<~s>, to=<~s>, warning=~p]", 
			[CSeqMethod, IP, url2str(FromURI), url2str(ToURI), Warning])
    end.

url2str({unparseable, _}) ->
    "unparseable";
url2str(URL) ->
    sipurl:print(URL).

sanity_check_uri(Desc, {none, _, _, _, _, _}, Header) ->
    throw({sipparseerror, Header, 400, "No user part in " ++ Desc ++ " URL"});
sanity_check_uri(Desc, {_, _, none, _, _}, Header) ->
    throw({sipparseerror, Header, 400, "No host part in " ++ Desc ++ " URL"});
sanity_check_uri(_, URI, _) ->
    URI.

check_supported_uri_scheme({unparseable, URIstr}, Header) ->
    case string:chr(URIstr, $:) of
	0 ->
	    throw({sipparseerror, Header, 416, "Unsupported URI Scheme"});
	Index ->
	    Scheme = string:substr(URIstr, 1, Index),
	    throw({sipparseerror, Header, 416, "Unsupported URI Scheme (" ++ Scheme ++ ")"})
    end;
check_supported_uri_scheme(URI, _) ->
    true.

get_env(Name) ->
    {ok, Value} = application:get_env(Name),
    Value.

get_env(Name, Default) ->
    case application:get_env(Name) of
	{ok, Value} ->
	    Value;
	undefined ->
	    Default
    end.
