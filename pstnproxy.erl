-module(pstnproxy).
-export([start/0, start/2, process/2]).

server_nodes() ->
    {ok, Servers} = application:get_env(databaseservers),
    Servers.

start(normal, Args) ->
    Pid = spawn(pstnproxy, start, []),
    {ok, Pid}.

start() ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, server_nodes()),
    {Message, Args} = case mnesia:wait_for_tables([user], infinity) of
			  ok ->
			      {"proxy started, all tables found~n", []};
			  {timeout, BadTabList} ->
			      {"proxy started, tables not reachable right now: ~p~n", BadTabList}
		      end,
    logger:start(),
    {ok, Socket} = gen_udp:open(5060, [{reuseaddr, true}]),
    logger:log(normal, Message, Args),
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
    case application:get_env(classdefs) of
	{ok, Defs} ->
	    Defs;
	undefined ->
	    [{"", unknown}]
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
