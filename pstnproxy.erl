-module(pstnproxy).
-export([start/0, start/2, process/2]).

server_node() -> 'incomingproxy@granit.e.kth.se'.

start(normal, Args) ->
    Pid = spawn(pstnproxy, start, []),
    {ok, Pid}.

start() ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [server_node()]),
    logger:start("sipd.log"),
    {ok, Socket} = gen_udp:open(5060, [{reuseaddr, true}]),
    logger:log(normal, "proxy started"),
    recvloop(Socket).

recvloop(Socket) ->
    receive
	{udp, Socket, IP, InPortNo, Packet} ->
	    spawn(pstnproxy, process, [Packet, Socket]),
	    recvloop(Socket)
    end.

process(Packet, Socket) ->
    case sippacket:parse(Packet) of
	{request, Method, URL, Header, Body} ->
	    request(Method, URL, Header, Body, Socket);
	{response, Status, Reason, Header, Body} ->
	    response(Status, Reason, Header, Body, Socket)
    end.

classdefs() ->
    [
     {"^0007[12]", pay},
     {"^000900", pay},
     {"^000939", pay},
     {"^000944", pay},
     {"^0007[0346]", mobile},
     {"^00[1-9]", national},
     {"^[46-9]", internal},
     {"^10", internal},
     {"^112", internal},
     {"^00010", mobile},
     {"^000[2-68]", national},
     {"^0001[1-9]", national},
     {"^00077", national},
     {"^0009[125-9]", national},
     {"^00090[1-9]", national},
     {"^00093[0-8]", national},
     {"^00094[0-35-9]", national},
     {"^0000", international}
    ].

request(Method, {User, Pass, "sip-pstn.kth.se", Port, Parameters}, Header, Body, Socket) ->
    logger:log(normal, Method),
    case Method of
	"INVITE" ->
	    request2(Method, User, Header, Body, Socket);
	"ACK" ->
	    request2(Method, User, Header, Body, Socket);
	"CANCEL" ->
	    request2(Method, User, Header, Body, Socket);
	"BYE" ->
	    request2(Method, User, Header, Body, Socket);
	_ ->
	    siprequest:send_result(Header, Socket, "", 501, "Not Implemented")
    end;

request(Method, URL, Header, Body, Socket) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Newlocation = {User, none, "kth.se", none, []},
    siprequest:send_proxy_request(Header, Socket, {Method, Newlocation, Body}).

proxyaddr() ->
    {ok, Addr} = application:get_env(proxyaddr),
    Addr.

request2(Method, Phone, Header, Body, Socket) ->
    Newlocation = {Phone, none, proxyaddr(), "5060", []},
    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
    {Fromphone, _, _, _, _} = FromURI,
    Classdefs = classdefs(),
    sipauth:check_and_send_auth(Header, Socket, Fromphone, Phone,
				{siprequest, send_proxy_request},
				{Method, Newlocation, Body},
				Method, Classdefs).

response(Status, Reason, Header, Body, Socket) ->
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).
