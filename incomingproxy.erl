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
    timer:apply_interval(60000, sipd, remove_expired_phones, []),
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

request("REGISTER", URL, Header, Body, Socket) ->
    logger:log(debug, "REGISTER"),
    To = sipheader:to(keylist:fetch("To", Header)),
    Contact = sipheader:contact(keylist:fetch("Contact", Header)),
    {_, {Phone, _, _, _, _}} = To,
    [{_, Location}] = Contact,
    case sipauth:can_register(Header, Phone) of
	true ->
	    siprequest:process_register_isauth(Header, Socket, {Phone, Location});
	_ ->
	    siprequest:send_auth_req(Header, Socket, sipauth:get_challenge())
    end;

request(Method, {User, Pass, "kth.se", Port, Parameters}, Header, Body, Socket) ->
    logger:log(normal, Method),
%    Phone = case directory:lookupmail(User ++ "@kth.se") of
%		none ->
%		    User;
%		Phone2 ->
%		    Phone2
%	    end,
    Phone = User,
    {atomic, Locations} = phone:get_phone(Phone),
    logger:log(debug, "Locations: ~p", [Locations]),
    {Location, _, _, _} = siprequest:location_prio(Locations),
    case Location of
	none ->
	    Newlocation = {Phone, none, "sip-pstn.kth.se", none, []},
	    siprequest:send_proxy_request(Header, Socket,
					  {Method, Newlocation, Body});
	_ ->
	    siprequest:send_proxy_request(Header, Socket, {Method, Location, Body})
    end;

request(Method, URL, Header, Body, Socket) ->
    siprequest:send_notfound(Header, Socket).

response(Status, Reason, Header, Body, Socket) ->
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
