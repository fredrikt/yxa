-module(pstnproxy).
-export([start/2]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, [user, numbers], false]),
    {ok, Pid}.

init() ->
    true.

localhostname(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(myhostnames)).

pstngateway(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(pstngatewaynames)).

routeRequestToPSTN(FromIP, ToHost) ->
    case pstngateway(FromIP) of
	true ->
	    logger:log(debug, "Routing: Source IP ~s is PSTN gateway, route to SIP proxy", [FromIP]),
	    false;		
	_ ->
	    case localhostname(ToHost) of
		true ->
		    logger:log(debug, "Routing: Source IP ~s is not PSTN gateway and ~p is me, route to PSTN gateway", [FromIP, ToHost]),
		    true;
		_ ->
		    case pstngateway(ToHost) of
			true ->
			    logger:log(debug, "Routing: Destination (~p) is PSTN gateway", [ToHost]),
			    true;
			_ ->
			    logger:log(debug, "Routing: Denied request from ~s to ~s - not my problem", [FromIP, ToHost]),
			    nomatch
		    end
	    end
    end.

request(Method, URL, Header, Body, Socket, FromIP) ->
    {User, Pass, Host, Port, Parameters} = URL,
    logger:log(debug, "~s ~s", [Method, sipurl:print(URL)]),
    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
    LogStr = io_lib:format("~s ~s [client=~s, from=<~s>, to=<~s>]", [Method, sipurl:print(URL), FromIP, sipurl:print(FromURI), sipurl:print(ToURI)]),
    case routeRequestToPSTN(FromIP, Host) of
	true ->
	    logger:log(normal, "~s -> PSTN gateway", [LogStr]),
	    case Method of
		"INVITE" ->
		    toPSTNrequest(Method, User, Header, Body, Socket);
		"ACK" ->
		    toPSTNrequest(Method, User, Header, Body, Socket);
		"PRACK" ->
		    toPSTNrequest(Method, User, Header, Body, Socket);
		"CANCEL" ->
		    toPSTNrequest(Method, User, Header, Body, Socket);
		"BYE" ->
		    toPSTNrequest(Method, User, Header, Body, Socket);
		_ ->
		    siprequest:send_result(Header, Socket, "", 501, "Not Implemented")
	    end;
	false ->
	    logger:log(normal, "~s -> SIP server", [LogStr]),
	    toSIPrequest(Method, URL, Header, Body, Socket);
	_ ->
	    siprequest:send_result(Header, Socket, "", 403, "Forbidden")
    end.

toSIPrequest(Method, URL, Header, Body, Socket) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Newlocation = {User, none, sipserver:get_env(sipproxy), none, []},
    case Newlocation of
	none ->
	    none;
	_ ->
	    Route = "<" ++ sipurl:print({User, Pass, lists:nth(1, sipserver:get_env(myhostnames)), Port,
    				["maddr=" ++ siphost:myip()]}) ++ ">",
	    Newheaders = keylist:prepend({"Record-Route", Route}, Header),
	    siprequest:send_proxy_request(Newheaders, Socket, {Method, Newlocation, Body, []})
    end.

toPSTNrequest(Method, Phone, Header, Body, Socket) ->
    Newlocation = {Phone, none, lists:nth(1, sipserver:get_env(pstngatewaynames)), none, []},
    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
    Fromphone = local:sipuser(FromURI),
    Classdefs = sipserver:get_env(classdefs, [{"", unknown}]),
    Route = "<" ++ sipurl:print({Phone, none, lists:nth(1, sipserver:get_env(myhostnames)), none,
				 ["maddr=" ++ siphost:myip()]}) ++ ">",
    Newheaders = keylist:append({"Record-route", Route}, Header),
    sipauth:check_and_send_auth(Header, Newheaders, Socket, Fromphone, Phone,
				{siprequest, send_proxy_request},
				{Method, Newlocation, Body, []},
				Method, Classdefs).

response(Status, Reason, Header, Body, Socket, FromIP) ->
    logger:log(normal, "~p", [Status]),
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).
