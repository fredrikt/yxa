-module(outgoingproxy).
-export([start/0, start/2, process/2]).

start(normal, Args) ->
    Pid = spawn(outgoingproxy, start, []),
    {ok, Pid}.

start() ->
    logger:start("sipd.log"),
    {ok, Socket} = gen_udp:open(5060, [{reuseaddr, true}]),
    logger:log(normal, "proxy started"),
    recvloop(Socket).

recvloop(Socket) ->
    receive
	{udp, Socket, IP, InPortNo, Packet} ->
	    spawn(outgoingproxy, process, [Packet, Socket]),
	    recvloop(Socket)
    end.

process(Packet, Socket) ->
    case sippacket:parse(Packet) of
	{request, Method, URL, Header, Body} ->
	    request(Method, URL, Header, Body, Socket);
	{response, Status, Reason, Header, Body} ->
	    response(Status, Reason, Header, Body, Socket)
    end.

request("ACK", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    true;

request("REGISTER", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    URL = {none, none, "kth.se", none, []},
    siprequest:send_redirect(URL, Header, Socket);

request(Method, {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    {NewHost, NewPort} = case {User, Host} of
			     {"00" ++ _, "sip-proxy.kth.se"} ->
				 {"sip-pstn.kth.se", none};
			     {_, "sip-proxy.kth.se"} ->
				 {"kth.se", none};
			     {_, _} ->
				 {Host, Port}
			 end,
    URL = {User, Pass, NewHost, NewPort, []},
    siprequest:send_redirect(URL, Header, Socket).

response(Status, Reason, Header, Body, Socket) ->
    true.
