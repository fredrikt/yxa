-module(sipsocket_tcp).
-export([start/1, send/4, is_reliable_transport/1, get_socket/2]).

-include("socketlist.hrl").
-include("sipsocket.hrl").

start(Port) when integer(Port) ->
    TCPsocket = case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
	{ok, S} ->
	    S;
	{error, E1} ->
	    logger:log(error, "Could not open TCP socket, port ~p : ~s",
			[Port, inet:format_error(E1)]),
	    erlang:fault("Could not open TCP socket", [Port])
    end,
    Local = case inet:sockname(TCPsocket) of
	{ok, {IPlist, LocalPort}} ->
	    {siphost:makeip(IPlist), LocalPort};
	{error, E2} ->
	    logger:log(error, "Sipsocket TCP: sockname() returned error ~p", [E2]),
	    {"0.0.0.0", 0}
    end,
    register(tcp_listener, self()),
    ReceiverPid = sipserver:safe_spawn(fun tcp_recvloop_start/2, [TCPsocket, Local]),
    SocketList = socketlist:add(listener, ReceiverPid, Local, {"0.0.0.0", 0}, 0, socketlist:empty()),
    tcp_listenerloop(SocketList).

% tcp_listenerloop() is the process that once started all the listening sockets,
% and it maintains a list of all ongoing tcp connections/listeners and creates
% new connections as requested.
tcp_listenerloop(SocketList) ->
    Res = receive

    	{get_socket, Pid, Host, Port} ->
    	    case get_socket_from_list(Host, Port, SocketList) of
		none ->
		    % We must spawn a child process to take care of making a new connection
		    % since we can't risk this thread getting locked up.
		    P = sipserver:safe_spawn(fun connect_to_remote/4, [Pid, self(), Host, Port]);
		{error, E} ->
		    util:safe_signal("TCP listener: ", Pid, {failed_getting_socket, self(), Host, Port, E});
    		GotSocket when record(GotSocket, sipsocket) ->
		    logger:log(debug, "Sipsocket TCP: (Transport layer) Use existing connection to ~s:~p", [Host, Port]),
    		    util:safe_signal("TCP listener: ", Pid, {got_socket, self(), Host, Port, GotSocket})
    	    end,
	    {ok, SocketList};

	{register_incoming_connection, FromPid, SipSocket} when record(SipSocket, sipsocket) ->
	    CPid = SipSocket#sipsocket.pid,
	    {Local, Remote} = SipSocket#sipsocket.data,
	    case socketlist:add({connection_from, Remote}, CPid, Local, Remote, 0, SocketList) of
		{error, E} ->
		    logger:log(error, "TCP listener: Failed adding incoming connection from ~p to socketlist", [Remote]),
		    util:safe_signal("TCP listener: ", FromPid, {failed_registering_incoming_connection, self(), E}),
		    {ok, SocketList};
		NewSocketList1 ->
		    util:safe_signal("TCP listener: ", FromPid, {registered_incoming_connection, self()}),
		    {ok, NewSocketList1}
	    end;

	{add_new_socket, FromPid, SipSocket} when record(SipSocket, sipsocket) ->
	    CPid = SipSocket#sipsocket.pid,
	    {Local, Remote} = SipSocket#sipsocket.data,
	    case socketlist:add({connection_to, Remote}, CPid, Local, Remote, 0, SocketList) of
		{error, E} ->
		    util:safe_signal("TCP listener: ", FromPid, {failed_adding_new_socket, self(), E}),
		    {ok, SocketList};
		NewSocketList1 ->
		    util:safe_signal("TCP listener: ", FromPid, {added_new_socket, self(), SipSocket}),
		    {ok, NewSocketList1}
	    end;
	
	{tcp_connection_closing, Pid} when pid(Pid) ->
	    {ok, socketlist:delete_using_pid(Pid, SocketList)};

	{quit} ->
    	    {quit};

    	Unknown ->
	    logger:log(error, "Sipsocket TCP: Received unknown signal in tcp_listenerloop() : ~p", [Unknown]),
	    {error, SocketList}
    after
	10 * 1000 ->
	    % Wake up every 10 seconds to do garbage collection (delete_expired() below)
	    {ok, SocketList}
    end,
    case Res of
	{quit} ->
	    ok;
	{_, NewSocketList} when record(NewSocketList, socketlist) ->
	    NewSocketList2 = socketlist:delete_expired(NewSocketList),
	    tcp_listenerloop(NewSocketList2);
	UnknownRes ->
	    logger:log(error, "Sipsocket TCP: Unknown result from tcp_listenerloop() : ~p", [UnknownRes]),
	    tcp_listenerloop(SocketList)
    end.

get_socket_from_list(Host, Port, SocketList) ->
    case socketlist:get_using_remote({Host, Port}, SocketList) of
	SListElem when record(SListElem, socketlistelem) ->
	    [CPid, Local, Remote] = socketlist:extract([pid, local, remote], SListElem),
	    logger:log(debug, "Sipsocket TCP: Reusing existing connection to ~s:~p (~p)",
		[Host, Port, CPid]),
	    SipSocket = #sipsocket{module=?MODULE, pid=CPid, data={Local, Remote}},
	    SipSocket;
	_ ->
	    none
    end.

% tcp_recvloop_start() handles one listening socket, accept()s incoming connections
% and start a tcp_connection() for each of these new connections
tcp_recvloop_start(Socket, Local) ->
    {IP, Port} = Local,
    logger:log(debug, "Listening on TCP ~s:~p (socket ~p)", [IP, Port, Socket]),
    tcp_recvloop(Socket, Local).
tcp_recvloop(ListenSocket, Local) ->
    Res = case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    case inet:peername(Socket) of
		{ok, {IPlist, InPortNo}} ->
		    IP = siphost:makeip(IPlist),
		    Remote = {IP, InPortNo},
		    CPid = sipserver:safe_spawn(fun start_tcp_connection_in/3, [Socket, Local, Remote]);
		{error, E} ->
		    logger:log(error, "Sipsocket TCP: Could not get peername after accept() : ~p", [E]),
		    {ok}
	    end;
	{error, closed} ->
	    logger:log(error, "Sipsocket TCP: accept() says the listensocket ~p was closed, no point in me staying alive.", [ListenSocket]),
	    {quit};
	{error, E} ->
	    logger:log(error, "Sipsocket TCP: accept() returned error : ~s (~p)", [inet:format_error(E), E]),
	    timer:sleep(1000),
	    {error}
    end,
    case Res of
	{quit} ->
	    ok;
	_ ->
	    tcp_recvloop(ListenSocket, Local)
    end.

connect_to_remote(RequestorPid, TcpListener, Host, Port) ->
    logger:log(debug, "Sipsocket TCP: No cached connection to remote host ~s:~p, trying to connect",
    		[Host, Port]),
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
	{ok, NewSocket} ->
	    Local = case inet:sockname(NewSocket) of
		{ok, {IPlist, LocalPort}} ->
		    {siphost:makeip(IPlist), LocalPort};
		{error, E1} ->
		    logger:log(error, "Sipsocket TCP: sockname() on new socket returned error ~p", [E1]),
		    {"0.0.0.0", 0}
	    end,
	    Remote = {Host, Port},
	    SipSocket = #sipsocket{module=?MODULE, pid=self(), data={Local, Remote}},
	    case util:safe_is_process_alive(TcpListener) of
		{true, TcpListenerPid} ->
		    TcpListener ! {add_new_socket, self(), SipSocket},
		    receive
			{added_new_socket, TcpListener, SipSocket} ->
			    util:safe_signal("Sipsocket TCP: ", RequestorPid, {got_socket, TcpListener, Host, Port, SipSocket}),
			    start_tcp_connection_out(NewSocket, Local, Remote);
			{failed_adding_new_socket, TcpListener, E2} ->
			    logger:log(error, "Sipsocket TCP: TCP listener ~p failed adding new socket ~p (peer ~s:~p) : ~p",
					[TcpListener, NewSocket, Host, Port, E2]),
			    util:safe_signal("Sipsocket TCP: ", RequestorPid, {failed_getting_socket, TcpListener, Host, Port, E2}),
			    gen_tcp:close(NewSocket),
			    {error, E2}
			after
			    1500 ->
				logger:log(error, "Sipsocket TCP: TCP listener ~p timed out adding new socket ~p (peer ~s:~p)",
						[TcpListener, NewSocket, Host, Port]),
				util:safe_signal("Sipsocket TCP: ", RequestorPid, {failed_getting_socket, TcpListener, Host, Port,
					"Timed out registering new connection with TCP listener"}),
				gen_tcp:close(NewSocket),
				{error, "TCP listener timed out, could not add new socket"}
			end;
		false ->
		    logger:log(error, "Sipsocket TCP: TCP listener ~p dead, could not store new socket ~p (peer ~s:~p)",
				[TcpListener, NewSocket, Host, Port]),
		    util:safe_signal("Sipsocket TCP: ", RequestorPid, {failed_getting_socket, TcpListener, Host, Port,
				"TCP listener dead, failed registering new connection"}),
		    gen_tcp:close(NewSocket),
		    {error, "TCP listener dead, could not store new socket"}
	    end;
	{error, econnrefused} ->
	    %logger:log(debug, "Sipsocket TCP: Connection refused by ~s:~p", [Host, Port]),
	    util:safe_signal("Sipsocket TCP: ", RequestorPid, {failed_getting_socket, TcpListener, Host, Port, "Connection refused"}),
	    {error, econnrefused};
	{error, E} ->
	    logger:log(error, "Sipsocket TCP: Failed connecting to ~s:~p : ~s (~p)", [Host, Port, inet:format_error(E), E]),
	    util:safe_signal("Sipsocket TCP: ", RequestorPid, {failed_getting_socket, TcpListener, Host, Port, inet:format_error(E)}),
	    {error, E}
    end.


% tcp_connection() handles one incoming or outgoing tcp connection. it starts a tcp_receiver()
% for each socket which will send received SIP messages to this process as soon as they are
% completely received.
start_tcp_connection_in(Socket, Local, Remote) ->
    % No receiver, start one
    Receiver = sipserver:safe_spawn(fun tcp_receiver/5, [self(), Socket, Local, Remote, []]),
    {IP, Port} = Remote,
    logger:log(debug, "Sipsocket TCP: Connection from ~s:~p (started receiver ~p)", [IP, Port, Receiver]),
    % Register this connection with the TCP listener process before we proceed
    Action = case util:safe_is_process_alive(tcp_listener) of
	{true, TcpListener} ->
	    SipSocket = #sipsocket{module=sipsocket_tcp, pid=self(), data={Local, Remote}},
	    TcpListener ! {register_incoming_connection, self(), SipSocket},
	    receive
		{registered_incoming_connection, TcpListener} ->
		    {ok};
		{failed_registering_incoming_connection, TcpListener, E} ->
		    logger:log(error, "Sipsocket TCP: Failed registering with TCP listener ~p : ~p",
				[TcpListener, E]),
		    {quit}
	    after
		1500 ->
		    logger:log(error, "Sipsocket TCP: Timed out waiting for acknowledgement of my registration (sent to ~p)",
				[TcpListener]),
		    {quit}
	    end; 
	{false, TcpListener} ->
	    logger:log(error, "Sipsocket TCP: TCP listener dead ~p, no point in me staying alive.", [TcpListener]),
	    {quit}
    end,
    case Action of
	{quit} ->
	    gen_tcp:close(Socket);
	{ok} ->
	    tcp_connection(Socket, Receiver, Local, Remote, util:timestamp())
    end.
    
start_tcp_connection_out(Socket, Local, Remote) ->
    % No receiver, start one
    Receiver = sipserver:safe_spawn(fun tcp_receiver/5, [self(), Socket, Local, Remote, []]),
    {IP, Port} = Remote,
    logger:log(debug, "Sipsocket TCP: Connected to ~s:~p (started receiver ~p)", [IP, Port, Receiver]),
    tcp_connection(Socket, Receiver, Local, Remote, util:timestamp()).

tcp_connection(Socket, Receiver, Local, Remote, StartTime) ->
    Timeout = sipserver:get_env(tcp_connection_idle_timeout, 300),
    {IP, Port} = Remote,
    Res = receive
	{recv, Receiver, Data} ->
	    SipSocket = #sipsocket{module=?MODULE, pid=self(), data={Local, Remote}},
	    Origin = {sipsocket:sipproto2str(?MODULE), IP, Port, SipSocket},
	    TransactionLayer = erlang:whereis(transaction_layer),
	    sipserver:safe_spawn(sipserver, process, [Data, SipSocket, Origin, TransactionLayer, TransactionLayer]);
	{send, Pid, {SendToHost, PortInt, Message}} ->
	    SendRes = gen_tcp:send(Socket, Message),
	    util:safe_signal("Sipsocket TCP: ", Pid, {send_result, self(), SendRes});
	{close} ->
	    Duration = util:timestamp() - StartTime,
	    logger:log(debug, "Sipsocket TCP: Closing connection with ~s:~p (duration: ~p seconds)", [IP, Port, Duration]),
	    gen_tcp:close(Socket),
	    {quit};
	{connection_closed, Receiver} ->
	    Duration = util:timestamp() - StartTime,
	    logger:log(debug, "Sipsocket TCP: Connection with ~s:~p closed by foreign host (duration: ~p seconds)",
			[IP, Port, Duration]),
	    gen_tcp:close(Socket),
	    {quit};
	Unknown ->
	    logger:log(error, "Sipsocket TCP: Received unknown signal in tcp_connection() : ~p", [Unknown]),
	    {error}
    after
	Timeout * 1000 ->
	    logger:log(debug, "Sipsocket TCP: Connection with ~s:~p timed out after ~p seconds, socket handler terminating.",
			[IP, Port, Timeout]),
	    exit(Receiver, "timed out"),
	    gen_tcp:close(Socket),
	    {quit}
    end,
    case Res of
	{quit} ->
	    util:safe_signal("Sipsocket TCP: ", tcp_listener, {tcp_connection_closing, self()}),
	    ok;
	_ ->
	    tcp_connection(Socket, Receiver, Local, Remote, StartTime)
    end.

% tcp_receiver() constantly tries to read from a socket, and when data is available
% it is checked to see if we have a complete SIP message. if we do, we safe_spawn
% a sipserver:process() on the received message.
tcp_receiver(Parent, Socket, Local, Remote, DataIn) ->
    % do_recv() reads data until it finds the CRLFCRLF between header and body. It is possible
    % that it also reads into the body of the message, but as soon as we have the header we
    % use tcp_read_sip_message() instead, which looks at the Content-Length and keeps reading
    % until the message boundry is reached.
    case do_recv(Socket, DataIn) of
	Data when list(Data) ->
	    Rest = case catch sippacket:parse(Data, none) of
		{request, Method, URI, Header, Body} ->
	    	    tcp_read_sip_message(request, Header, Body, Data, Parent, Socket, Local, Remote);
		{response, Status, Reason, Header, Body} ->
	    	    tcp_read_sip_message(response, Header, Body, Data, Parent, Socket, Local, Remote);
		_ ->
		    % Packet did not parse, maybe we have to read some more data first?
		    Data
	    end,
	    case Rest of
		{connection_closed} ->
		    util:safe_signal("Sipsocket TCP: ", Parent, {connection_closed, self()});
		{close} ->
		    util:safe_signal("Sipsocket TCP: ", Parent, {close});
		_ when list(Rest) ->
		    % We received more data than the Content-Length indicated, might be pipelined requests?
		    % Remove any CRLF sequences before (or between) requests, RFC3261 #7.5
		    tcp_receiver(Parent, Socket, Local, Remote, remove_separator(lists:flatten(Rest)));
		_ ->
		    logger:log(error, "Sipsocket TCP: Unknown result from sippacket:parse() or tcp_read_sip_message() :~n~p~n" ++
				      "Closing socket.", [Rest]),
		    util:safe_signal("Sipsocket TCP: ", Parent, {close})
	     end;
	{connection_closed} ->
	    util:safe_signal("Sipsocket TCP: ", Parent, {connection_closed, self()});
	{close} ->
	    util:safe_signal("Sipsocket TCP: ",Parent, {close});
	Unknown ->
	    logger:log(error, "Sipsocket TCP: Dying after unknown result from do_recv() :~n~p", [Unknown]),
	    util:safe_signal("Sipsocket TCP: ",Parent, {close})
    end.

% tcp_read_sip_message() is used to read the remainder of the message when we have the header
% and therefor can use the Content-Length to determine when we are done.
tcp_read_sip_message(MessageType, Header, BodyIn, DataIn, Parent, Socket, Local, Remote) ->
    {IP, Port} = Remote,
    case get_content_length(Header) of
	CLen when integer(CLen) ->
	    Remaining = CLen - length(BodyIn),
	    case tcp_read_more_data(Socket, Remaining, DataIn) of
		Data when list(Data) ->
		    ThisMessage = string:substr(Data, 1, length(DataIn) + Remaining),
		    Rest = string:substr(Data, 1 + length(DataIn) + Remaining),
		    util:safe_signal("Sipsocket TCP: ",Parent, {recv, self(), ThisMessage}),
		    Rest;
		{connection_closed} ->
		    logger:log(debug, "Sipsocket TCP: Connection closed by foreign host when expecting ~p bytes more data", [CLen]),
		    {connection_closed};
		Res ->
		    logger:log(debug, "Sipsocket TCP: Failed reading ~p bytes more data : ~p", [CLen, Res]),
		    []
	    end;
	_ ->
	    case MessageType of
		request ->
		    logger:log(debug, "Sipsocket TCP: Non-existing or invalid Content-Length in request from ~s:~p, " ++
				      "(socket ~p), discarding message :~n~p", [IP, Port, Socket, DataIn]),
		    logger:log(error, "Sipsocket TCP: Non-existing or invalid Content-Length in request from ~s:~p, " ++
				      "answering 400 Bad Request and closing socket", [IP, Port]),
		    SipSocket = {module=sipsocket_tcp, pid=Parent, data={IP, Port}},
		    transportlayer:send_result(Header, SipSocket, "", 400, "Bad Request",
					   [{"Reason", ["Non-existing or invalid Content-Length"]}]);
		response ->
		    logger:log(debug, "Sipsocket TCP: Non-existing or invalid Content-Length in response from ~s:~p " ++
				      "(socket ~p), discarding message :~n~p", [IP, Port, Socket, DataIn]),
		    logger:log(error, "Sipsocket TCP: Non-existing or invalid Content-Length in response from ~s:~p, " ++
				      "discarding and closing socket.", [IP, Port])
	    end,
	    logger:log(debug, "Sipsocket TCP: Closing socket ~p since there is no way to know we are synchronized with remote end", [Socket]),
	    {close}
    end.

get_content_length(Header) ->
    case keylist:fetch("Content-Length", Header) of
	[CLenStr] ->
	    case util:isnumeric(CLenStr) of
		true ->
		    list_to_integer(CLenStr);
		false ->
		    invalid
	    end;
	_ ->
	    invalid
    end.
    
do_recv(Socket, Bs) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, B} ->
	    SoFar = lists:append(Bs, binary_to_list(B)),
	    % Look for header-body separator
	    case string:str(SoFar, "\r\n\r\n") of
		0 ->
		    % read more data
		    do_recv(Socket, SoFar);
		_ ->
		    SoFar
	    end;
	{error, closed} ->
	    {connection_closed};
	{error, E} ->
	    logger:log(error, "Sipsocket TCP: Error when reading data : ~s (~p)", [inet:format_error(E), E]),
	    {error, E}
    end.

% Read a pre-determined ammount of more data from a socket.
tcp_read_more_data(Socket, Len, DataIn) when Len > 0 -> 
    case gen_tcp:recv(Socket, 0) of
	{ok, B} ->
	    Data = binary_to_list(B),
	    SoFar = lists:append(DataIn, Data),
	    tcp_read_more_data(Socket, Len - length(Data), SoFar);
	{error, closed} ->
	    {connection_closed};
	{error, E} ->
	    logger:log(error, "Sipsocket TCP: Error when reading ~p bytes of data : ~s (~p)", [Len, inet:format_error(E), E]),
	    {error, E}
    end;

tcp_read_more_data(Socket, Len, In) ->
    In.

send(SipSocket, SendToHost, PortInt, Message) when record(SipSocket, sipsocket) ->
    SPid = SipSocket#sipsocket.pid,
    case util:safe_is_process_alive(SPid) of
	{true, Pid} ->
	    Pid ! {send, self(), {SendToHost, PortInt, Message}},
	    receive
	        {send_result, Pid, Res} ->
		    Res
	    after
	        1500 ->
		    {error, "TCP socket handler timed out"}
	    end;
	{false, _} ->
	    {error, "TCP socket handler dead"}
    end.

get_socket(Host, Port) when list(Host), integer(Port) ->
    case util:safe_is_process_alive(tcp_listener) of
	{true, TcpListener} ->
	    TcpListener ! {get_socket, self(), Host, Port},
	    receive
	        {failed_getting_socket, TcpListener, Host, Port, E} ->
		    {error, E};
	        {got_socket, TcpListener, Host, Port, Socket} ->
		    Socket
	    after
	        1500 ->
		    {error, "TCP listener timed out, could not get socket"}
	    end;
	{false, _} ->
	    {error, "TCP listener dead, could not get socket"}
    end.

is_reliable_transport(_) -> true.

% RFC3261 7.5
remove_separator([H|T]) when H == $\r;H == $\n ->
    remove_separator(T);
remove_separator(L) ->
    L.
