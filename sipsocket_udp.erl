%%%-------------------------------------------------------------------
%%% File    : sipsocket_udp.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Transportlayer UDP all-in-one handler. Receive UDP
%%%           datagrams and, if they are not keep alives (or too
%%%           short), spawn sipserver:process(...) on the received
%%%           packets. Send packets using either our IPv4 or our IPv6
%%%           socket.
%%%
%%% Note    : This module should be split in two, one server process
%%%           and one interface module which can be a sipsocket
%%%           behaviour module, like the TCP sipsocket code.
%%%
%%% Note    : Our UDP code does not support multiple interfaces
%%%           correctly. We should get one socket per interface (and
%%%           protocol), to be able to implement RFC3581 (rport)
%%%           correctly.
%%%
%%% Created : 15 Dec 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipsocket_udp).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 send/5,
	 is_reliable_transport/1,
	 get_socket/1
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {socket, socket6, socketlist}).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SOCKETOPTS, [{reuseaddr, true}, binary]).
%% v6 sockets have a default receive buffer size of 1k in Erlang R9C-0
-define(SOCKETOPTSv6, [{reuseaddr, true}, inet6, {buffer, 8 * 1024}]).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/1
%% Descrip.: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, sipsocket_udp}, ?MODULE, [], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([])
%% Descrip.: Initiates the server
%% Returns : {ok, State}    |
%%           {stop, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
init([]) ->
    Port = sipsocket:get_listenport(udp),  %% same for UDP and UDPv6
    start_listening([udp, udp6], Port, #state{socketlist=socketlist:empty()}).

%%--------------------------------------------------------------------
%% Function: start_listening(ProtoList, Port, State)
%%           ProtoList = list() of atom(), udp | udp6
%%           Port      = integer()
%% Descrip.: Begin listening on port Port.
%% Returns : {ok, State}    |
%%           {stop, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
start_listening([], _Port, State) ->
    {ok, State};
start_listening([udp | T], Port, State) when is_integer(Port), is_record(State, state) ->
    case gen_udp:open(Port, ?SOCKETOPTS) of
	{ok, Socket} ->
	    Local = get_localaddr(Socket, "0.0.0.0"),
	    SipSocket = #sipsocket{module=sipsocket_udp, proto=udp, pid=self(), data={Local, none}},
	    NewSocketList = socketlist:add({listener, udp, Port}, self(), udp, Local, none, SipSocket,
					   0, State#state.socketlist),
	    start_listening(T, Port, State#state{socket=Socket, socketlist=NewSocketList});
	{error, Reason} ->
	    logger:log(error, "Could not open UDP socket (options ~p), port ~p : ~s",
		       [?SOCKETOPTS, Port, inet:format_error(Reason)]),
	    {stop, "Could not open UDP socket"}
    end;
start_listening([udp6 | T], Port, State) when is_integer(Port), is_record(State, state) ->
    case yxa_config:get_env(enable_v6) of
	{ok, true} ->
	    case gen_udp:open(Port, ?SOCKETOPTSv6) of
		{ok, Socket} ->
		    Local = get_localaddr(Socket, "[::]"),
		    SipSocket = #sipsocket{module=sipsocket_udp, proto=udp6, pid=self(), data={Local, none}},
		    NewSocketList = socketlist:add({listener, udp6, Port}, self(), udp6, Local, none, SipSocket,
						   0, State#state.socketlist),
		    start_listening(T, Port, State#state{socket6=Socket, socketlist=NewSocketList});
		{error, Reason} ->
		    logger:log(error, "Could not open IPv6 UDP socket (options ~p), port ~p : ~s",
			       [?SOCKETOPTSv6, Port, inet:format_error(Reason)]),
		    {stop, "Could not open IPv6 UDP socket"}
	    end;
	{ok, false} ->
	    start_listening(T, Port, State)
    end.

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: handle_call({get_socket, Proto}, From, State)
%%           Proto = atom(), udp | udp6
%% Descrip.: Get a socket for a certain protocol (Proto).
%%           sipsocket_udp is currently not multi-socket, we just have
%%           a single UDP socket for each protocol and that is
%%           'listener'. Locate and return it.
%% Returns : {reply, Reply, State}
%%           Reply = {ok, SipSocket} |
%%                   {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
handle_call({get_socket, Proto}, _From, State) when is_atom(Proto) ->
    Id = {listener, Proto, sipsocket:get_listenport(Proto)},
    case socketlist:get_using_id(Id, State#state.socketlist) of
	[] ->
	    logger:log(error, "Sipsocket UDP: Failed fetching socket with id '~p' from list :~n~p",
		       [Id, socketlist:debugfriendly(State#state.socketlist)]),
	    {reply, {error, "UDP socket for specified protocol not found"}, State};
	SElem ->
	    [CPid, Local, Remote] = socketlist:extract([pid, local, remote], SElem),
	    SipSocket = #sipsocket{module=sipsocket_udp, proto=Proto, pid=CPid, data={Local, Remote}},
	    {reply, {ok, SipSocket}, State}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call({send, {Host, Port, Message}}, From, State)
%%           Host    = string()
%%           Port    = integer()
%%           Message = term()
%% Descrip.: Send Message to Host:Port.
%% Returns : {reply, Reply, State}
%%           Reply = {send_result, Res}
%%           Res = term()
%% Note    : Unfortunately there seems to be no way to receive ICMP
%%           port unreachables when sending with gen_udp...
%%--------------------------------------------------------------------
%%
%% Host = IPv6 address (e.g. [2001:6b0:5:987::1])
%%
handle_call({send, {"[" ++ HostT, Port, Message}}, _From, State) when is_list(HostT), is_integer(Port) ->
    SendRes =
	case string:rchr(HostT, 93) of	%% 93 is ]
	    0 ->
		{error, "Unknown format of destination"};
	    Index when is_integer(Index) ->
		Addr = string:substr(HostT, 1, Index - 1),
		gen_udp:send(State#state.socket6, Addr, Port, Message)
	end,
    {reply, {send_result, SendRes}, State};
%%
%% Host is not IPv6 reference (inside brackets)
%%
handle_call({send, {Host, Port, Message}}, _From, State) when is_list(Host), is_integer(Port) ->
    SendRes = gen_udp:send(State#state.socket, Host, Port, Message),
    {reply, {send_result, SendRes}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    logger:log(error, "Sipsocket UDP: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info({udp, Socket, IPlist, InPortNo, Packet},
%%                       State)
%%           Socket   = term()
%%           IPlist   = term()
%%           InPortNo = integer()
%%           Packet   = list()
%% Descrip.: Handle data received (as a signal) from one of our
%%           sockets.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_info({udp, Socket, IPlist, InPortNo, Packet}, State) when is_integer(InPortNo) ->
    Sv4 = State#state.socket,
    Sv6 = State#state.socket6,
    Proto = case Socket of
		Sv4 -> udp;
		Sv6 -> udp6;
		_ ->
		    logger:log(error, "Sipsocket UDP: Received gen_server info 'udp' "
			       "from unknown source '~p', ignoring", [Socket])
	end,
    IP = siphost:makeip(IPlist),
    received_packet(Packet, IP, InPortNo, Proto),
    {noreply, State};

handle_info(Info, State) ->
    logger:log(error, "Sipsocket UDP: Received unknown gen_server info : ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
        normal -> true;
        _ -> logger:log(error, "UDP listener terminating : ~p", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Purpose : Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Interface functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: send(SipSocket, Proto, Host, Port, Message)
%%           SipSocket = sipsocket record()
%%           Proto     = atom(), udp | udp6
%%           Host      = string()
%%           Port      = integer()
%%           Message   = term()
%% Descrip.: Send Message to Host:Port. Proto must currently be the
%%           same protocol as is stored in SipSocket.
%% Returns : Result |
%%           {error, Reason}
%%           Result = term(), ultimately the result of send()
%%           Reason = string()
%%--------------------------------------------------------------------
send(SipSocket, Proto, _Host, _Port, _Message)
  when is_record(SipSocket, sipsocket), SipSocket#sipsocket.proto /= Proto ->
    {error, "Protocol mismatch"};
send(SipSocket, Proto, Host, Port, Message) when is_record(SipSocket, sipsocket),
						 is_integer(Port), is_atom(Proto),
						 Proto == udp; Proto == udp6 ->
    Pid = SipSocket#sipsocket.pid,
    case catch gen_server:call(Pid, {send, {Host, Port, Message}}) of
	{send_result, Res} ->
	    Res;
	Unknown ->
	    logger:log(error, "Sipsocket UDP: Unknown send response from socket pid ~p for ~p:~s:~p : ~p",
                       [Pid, Proto, Host, Port, Unknown]),
	    {error, "Unknown sipsocket_udp send result"}
    end.

%%--------------------------------------------------------------------
%% Function: get_socket(Dst)
%%           Dst = sipdst record()
%% Descrip.: Return a socket suitable for communicating with this dst.
%% Returns : SipSocket       |
%%           {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
get_socket(#sipdst{proto=Proto}) when Proto == udp; Proto == udp6 ->
    case catch gen_server:call(sipsocket_udp, {get_socket, Proto}, 1500) of
	{ok, SipSocket} ->
	    SipSocket;
	{error, E} ->
	    logger:log(error, "Sipsocket UDP: Failed fetching socket for protocol ~p : ~p", [Proto, E]),
	    {error, E};
	Unknown ->
	    logger:log(error, "Sipsocket UDP: Unknown get_socket response from sipsocket_udp (protocol ~p) : ~p",
		       [Proto, Unknown]),
	    {error, "sipsocked_udp failed"}
    end.

%%--------------------------------------------------------------------
%% Function: is_reliable_transport(_)
%% Descrip.: No UDP protocol is reliable transport. Return false.
%% Returns : false
%%--------------------------------------------------------------------
is_reliable_transport(_) ->
    false.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: get_localaddr(Socket, DefaultAddr)
%%           Socket      = term(), gen_udp socket
%%           DefaultAddr = list(), default if we fail
%% Descrip.: Return the listening IP and port for a socket.
%% Returns : {IP, Port}
%%           IP   = string()
%%           Port = integer()
%%--------------------------------------------------------------------
get_localaddr(Socket, DefaultAddr) ->
    case inet:sockname(Socket) of
	{ok, {IPlist, LocalPort}} ->
	    IP = siphost:makeip(IPlist),
	    logger:log(debug, "Listening on UDP ~s:~p (socket ~p)", [IP, LocalPort, Socket]),
	    {IP, LocalPort};
	{error, E} ->
	    %% XXX maybe we don't have a good socket after all?
	    logger:log(error, "UDP listener: sockname() on socket ~p returned error ~p", [Socket, E]),
	    {DefaultAddr, 0}
    end.

%%--------------------------------------------------------------------
%% Function: received_packet(Packet, IP, Port, Proto)
%%           Packet = binary(), packet received from network
%%           IP     = string(), source IP address
%%           Port   = integer(), source port
%%           Proto  = atom(), source protocol
%% Descrip.: Check if a received packet is a keepalive or possibly a
%%           SIP message. For the latter case, we spawn
%%           sipserver:process(...) on the packet, after creating the
%%           necessary sipsocket and origin records.
%% Returns : ok
%%--------------------------------------------------------------------
received_packet(<<"\r\n">>, IP, Port, Proto) ->
    logger:log(debug, "Keepalive packet (CRLF) received from ~p:~s:~p", [Proto, IP, Port]),
    ok;
received_packet(Packet, IP, Port, Proto) when is_binary(Packet), size(Packet) =< 20 ->
    %% Too short to be anywhere near a real SIP message
    case is_only_nulls(Packet) of
	true ->
	    logger:log(debug, "Keepalive packet (~p NULLs) received from ~p:~s:~p",
		       [size(Packet), Proto, IP, Port]);
	false ->
	    logger:log(debug, "Packet received from ~p:~s:~p too short to be a SIP message :~n~p~n"
		       "(as list: ~p)", [Proto, IP, Port, Packet, binary_to_list(Packet)])
    end,
    ok;
received_packet(Packet, IP, Port, Proto) when is_binary(Packet), is_list(IP), is_integer(Port), is_atom(Proto) ->
    SipSocket = #sipsocket{module=sipsocket_udp, proto=Proto, pid=self(), data=none},
    Origin = #siporigin{proto=Proto, addr=IP, port=Port,
			receiver=self(), sipsocket=SipSocket},
    sipserver:safe_spawn(sipserver, process, [Packet, Origin, transactionlayer]),
    ok.

%%--------------------------------------------------------------------
%% Function: is_only_nulls(Packet)
%%           Packet = binary(), a packet received from the network
%% Descrip.: Check if a binary packet is only NULL bytes or not.
%% Returns : true | false
%%--------------------------------------------------------------------
is_only_nulls(Packet) when is_binary(Packet) ->
    ZeroList = lists:duplicate(size(Packet), 0),
    (Packet == list_to_binary(ZeroList)).
