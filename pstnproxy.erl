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
	    none
    end;

request(Method, URL, Header, Body, Socket) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Newlocation = {User, none, "kth.se", none, []},
    siprequest:send_proxy_request(Header, Socket, {Method, Newlocation, Body}).

request2(Method, Phone, Header, Body, Socket) ->
    Newlocation = {Phone, none, sipconfig:proxyaddr(), "5060", []},
    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
    {Fromphone, _, _, _, _} = FromURI,
    sipauth:check_and_send_auth(Header, Socket, Fromphone, Phone,
				{siprequest, send_proxy_request},
				{Method, Newlocation, Body},
				Method).

response(Status, Reason, Header, Body, Socket) ->
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).
