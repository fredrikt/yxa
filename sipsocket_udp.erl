-module(sipsocket_udp).
-export([start/1, send/4, is_reliable_transport/1, get_socket/2]).

-include("sipsocket.hrl").

start(Port) ->
    UDPsocket = case gen_udp:open(Port, [{reuseaddr, true}]) of
	{ok, S} ->
	    S;
	{error, Reason} ->
	    logger:log(error, "Could not open UDP socket, port ~p : ~s",
			[Port, inet:format_error(Reason)]),
	    erlang:fault("Could not open UDP socket", [Port])
    end,
    register(udp_listener, self()),
    ReceiverPid = sipserver:safe_spawn(fun udp_recvloop_start/1, [UDPsocket]),
    Local = case inet:sockname(UDPsocket) of
	{ok, {IPlist, LocalPort}} ->
	    {siphost:makeip(IPlist), LocalPort};
	{error, E} ->
	    {"0.0.0.0", 0}
    end,
    gen_udp:controlling_process(UDPsocket, ReceiverPid),
    SocketList = socketlist:add(listener, ReceiverPid, Local, {"0.0.0.0", 0}, 0, socketlist:empty()),
    udp_listenerloop(SocketList).

udp_listenerloop(SocketList) ->
    Res = receive
	{get_socket, Pid, Host, Port} ->
	    % sipsocket_udp is not multi-socket, we just have a singe UDP socket and that is 'listener'.
	    case socketlist:get_using_id(listener, SocketList) of
		[] ->
		    logger:log(error, "Sipsocket TCP: Failed fetching socket with id 'listener' from list :~n~p",
				[SocketList]),
		    Pid ! {failed_getting_socket, self(), Host, Port, "UDP socket down"};
		SElem ->
		    [CPid, Local, Remote] = socketlist:extract([pid, local, remote], SElem),
		    SipSocket = #sipsocket{module=sipsocket_udp, pid=CPid, data={Local, Remote}},
		    Pid ! {got_socket, self(), Host, Port, SipSocket}
		end;
	{quit} ->
    	    {quit};
	Unknown ->
	    logger:log(error, "Sipsocket UDP: Received unknown signal in udp_listenerloop : ~n~p", [Unknown])
    end,
    case Res of
	{quit} ->
	    ok;
	_ ->
	    % We don't check for expired records here since UDP sockets does not expire
	    udp_listenerloop(SocketList)
    end.

udp_recvloop_start(Socket) ->
    log_listening(debug, "UDP", Socket),
    udp_recvloop(Socket).
udp_recvloop(Socket) ->
    receive
	{udp, Socket, IPlist, InPortNo, Packet} ->
	    SipSocket = #sipsocket{module=sipsocket_udp, pid=self(), data=none},
	    Origin = {sipsocket:sipproto2str(?MODULE), siphost:makeip(IPlist), InPortNo, SipSocket},
	    TransactionLayer = erlang:whereis(transaction_layer),
	    sipserver:safe_spawn(sipserver, process, [Packet, SipSocket, Origin, TransactionLayer, TransactionLayer]);
	{send, Pid, {SendToHost, PortInt, Message}} ->
	    % Unfortunately there seems to be no way to receive ICMP port unreachables in erlang gen_udp...
	    SendRes = gen_udp:send(Socket, SendToHost, PortInt, Message),
	    Pid ! {send_result, self(), SendRes};
	Unknown ->
	    logger:log(error, "Sipsocket UDP: Received unknown signal in udp_recvloop : ~n~p", [Unknown])
    end,
    udp_recvloop(Socket).


log_listening(LogLevel, Class, Socket) ->
    case inet:sockname(Socket) of
	{ok, {IPlist, Port}} ->
	    IP = siphost:makeip(IPlist),
	    logger:log(LogLevel, "Listening on ~s ~s:~p (socket ~p)", [Class, IP, Port, Socket]),
	    ok;
	{error, E} ->
	    logger:log(error, "Sipsocket UDP: sockname() returned error ~p", [E]),
	    error
    end.

send(SipSocket, SendToHost, PortInt, Message) when record(SipSocket, sipsocket) ->
    Pid = SipSocket#sipsocket.pid,
    case util_safe_is_process_alive(Pid) of
	true ->
	    Pid ! {send, self(), {SendToHost, PortInt, Message}},
	    receive
	        {send_result, Pid, Res} ->
		    Res
	    after
	        1500 ->
		    {error, "UDP socket handler timed out"}
	    end;
	false ->
	    logger:log(debug, "Sipsocket UDP: The UDP socket handler ~p is dead", [Pid]),
	    {error, "UDP socket handler dead"}
    end;
send(InvalidSocket, SendToHost, PortInt, Message) ->
    logger:log(error, "Sipsocket UDP: Could not send message to ~p:~p, invalid socket : ~p",
		[SendToHost, PortInt, InvalidSocket]).

get_socket(Host, Port) when list(Host), integer(Port) ->
    % Need to find our UDP listening socket
    UdpListener = erlang:whereis(udp_listener),
    case util_safe_is_process_alive(UdpListener) of
	true ->
	    UdpListener ! {get_socket, self(), Host, Port},
	    receive
	        {failed_getting_socket, Pid, Host, Port, E} ->
		    {error, E};
	        {got_socket, Pid, Host, Port, Socket} ->
		    Socket
	    after
	        1500 ->
		    {error, "UDP listener timed out"}
	    end;
	false ->
	    {error, "UDP listener dead"}
    end.

is_reliable_transport(_) -> false.

util_safe_is_process_alive(Pid) when pid(Pid) ->
    is_process_alive(Pid);
util_safe_is_process_alive(_) ->
    false.
