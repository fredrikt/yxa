-module(sipsocket_udp).
-export([start/3, send/4, is_reliable_transport/1, get_socket/3, get_response_socket/1,
	 associate_transaction_with_socket/2, store_stateless_response_branch/2]).

start(Port, RequestFun, ResponseFun) ->
    UDPsocket = case gen_udp:open(Port, [{reuseaddr, true}]) of
	{ok, S} ->
	    S;
	{error, Reason} ->
	    logger:log(error, "Could not open UDP socket, port ~p : ~s",
			[Port, inet:format_error(Reason)]),
	    erlang:fault("Could not open UDP socket", [Port])
    end,
    register(udp_listener, self()),
    ReceiverPid = sipserver:safe_spawn(fun udp_recvloop_start/3, [UDPsocket, RequestFun, ResponseFun]),
    Local = case inet:sockname(UDPsocket) of
	{ok, {IPlist, LocalPort}} ->
	    {siphost:makeip(IPlist), LocalPort};
	{error, E} ->
	    {"0.0.0.0", 0}
    end,
    gen_udp:controlling_process(UDPsocket, ReceiverPid),
    SocketList = socketlist:add(listener, ReceiverPid, Local, {"0.0.0.0", 0}, 0, socketlist:empty()),
    udp_listenerloop(SocketList, RequestFun, ResponseFun).

udp_listenerloop(SocketList, RequestFun, ResponseFun) ->
    Res = receive
	{get_socket, Pid, Host, Port, Header} ->
	    case socketlist:get_using_id(listener, SocketList) of
		[] ->
		    logger:log(error, "Sipsocket TCP: Failed fetching socket with id 'listener' from list :~n~p",
				[SocketList]),
		    Pid ! {failed_getting_socket, self(), Host, Port, "UDP socket down"};
		SElem ->
		    [CPid, Local, Remote] = socketlist:extract([pid, local, remote], SElem),
		    ThisSocket = {sipsocket_udp, CPid, {Local, Remote}},
		    Pid ! {got_socket, self(), Host, Port, ThisSocket}
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
	    udp_listenerloop(SocketList, RequestFun, ResponseFun)
    end.

udp_recvloop_start(Socket, RequestFun, ResponseFun) ->
    log_listening(debug, "UDP", Socket),
    udp_recvloop(Socket, RequestFun, ResponseFun).
udp_recvloop(Socket, RequestFun, ResponseFun) ->
    receive
	{udp, Socket, IPlist, InPortNo, Packet} ->
	    ThisSocket = {sipsocket_udp, self(), none},
	    Origin = {sipsocket:sipproto2str(?MODULE), siphost:makeip(IPlist), InPortNo, ThisSocket},
	    sipserver:safe_spawn(sipserver, process, [Packet, ThisSocket, Origin,
						      RequestFun, ResponseFun]);
	{send, Pid, {SendToHost, PortInt, Message}} ->
	    SendRes = gen_udp:send(Socket, SendToHost, PortInt, Message),
	    Pid ! {send_result, self(), SendRes};
	Unknown ->
	    logger:log(error, "Sipsocket UDP: Received unknown signal in udp_recvloop : ~n~p", [Unknown])
    end,
    udp_recvloop(Socket, RequestFun, ResponseFun).


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

send({sipsocket_udp, Pid, _}, SendToHost, PortInt, Message) ->
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

get_socket(SendToHost, PortInt, Header) ->
    % Need to find our UDP listening socket
    UdpListener = erlang:whereis(udp_listener),
    case util_safe_is_process_alive(UdpListener) of
	true ->
	    UdpListener ! {get_socket, self(), SendToHost, PortInt, Header},
	    receive
	        {failed_getting_socket, Pid, SendToHost, PortInt, E} ->
		    {error, E};
	        {got_socket, Pid, SendToHost, PortInt, Socket} ->
		    Socket
	    after
	        1500 ->
		    {error, "UDP listener timed out"}
	    end;
	false ->
	    {error, "UDP listener dead"}
    end.

get_response_socket(_) ->
    none.

associate_transaction_with_socket(TransactionId, Socket) ->
    ok.

store_stateless_response_branch(Branch, Socket) ->
    ok.

is_reliable_transport(_) -> false.

util_safe_is_process_alive(Pid) when pid(Pid) ->
    is_process_alive(Pid);
util_safe_is_process_alive(_) ->
    false.
