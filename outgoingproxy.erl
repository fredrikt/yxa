-module(outgoingproxy).
-export([start/0, start/2, process/2]).

-include("siprecords.hrl").

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

%%
%% ACK
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "ACK" ->
    true;

%%
%% REGISTER
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "REGISTER" ->
    URL = Request#request.uri,
    NewURL = URL#sipurl{user=none, pass=none, host="kth.se", port=none, param=[]},
    siprequest:send_redirect(NewURL, Request#request.header, Origin#siporigin.sipsocket);

%%
%% Anything but REGISTER and ACK
%%
request(Method, URL, Header, Body, Socket) when record(URL, sipurl) ->
    URL = Request#request.uri,
    {NewHost, NewPort} = case {URL#sipurl.user, URL#sipurl.host} of
			     {"00" ++ _, "sip-proxy.kth.se"} ->
				 {"sip-pstn.kth.se", none};
			     {_, "sip-proxy.kth.se"} ->
				 {"kth.se", none};
			     {_, _} ->
				 {Host, Port}
			 end,
    NewURL = URL#sipurl{host=NewHost, port=NewPort, param=[]},
    siprequest:send_redirect(NewURL, Request#request.header, Origin#siporigin.sipsocket).

response(Response, Origin, LogStr) when record(Response, response), record(Origin, siporigin) ->
    true.
