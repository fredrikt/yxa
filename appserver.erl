-module(appserver).
-export([start/0, start/2, process/2]).

start(normal, Args) ->
    Pid = spawn(appserver, start, []),
    {ok, Pid}.

start() ->
    phone:init(),
    database_call:create_call(),
    logger:start("sipd.log"),
    {ok, Socket} = gen_udp:open(5060, [{reuseaddr, true}]),
    logger:log(normal, "proxy started: ~p", [application:get_env(startmsg)]),
    recvloop(Socket).

recvloop(Socket) ->
    receive
	{udp, Socket, IP, InPortNo, Packet} ->
	    spawn(appserver, process, [Packet, Socket]),
	    recvloop(Socket)
    end.

process(Packet, Socket) ->
    case sippacket:parse(Packet) of
	{request, Method, URL, Header, Body} ->
	    request(Method, URL, Header, Body, Socket);
	{response, Status, Reason, Header, Body} ->
	    response(Status, Reason, Header, Body, Socket)
    end.

request("BYE", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [Call]} ->
	    {Origheaders, Pid} = Call,
	    Pid ! {siprequest, bye},
	    siprequest:send_result(Header, Socket, "", 200, "OK")
    end;

request("ACK", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    true;

request("CANCEL", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    true;

request("INVITE", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    case sipanswer:start(Header, Body, bounce, none, none) of
	{ok, Replybody} ->
	    logger:log(debug, "body:~p", [Replybody]),
	    siprequest:send_answer(Header, Socket, Replybody);
	{error, _} ->
	    true
    end.

response(Status, Reason, Header, Body, Socket) ->
    logger:log(normal, "status:~p", [Status]),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [Call]} ->
	    {Origheaders, Pid} = Call,
	    Pid ! {siprequest, status, Status, Header, Socket, Body, Origheaders}
    end.
