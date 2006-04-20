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
	 get_socket/1,
	 get_specific_socket/1,
	 get_raw_socket/1
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
-include("stun.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {inet_socketlist  = [],	%% list() of {Socket, SipSocket} - the first one is the one we send using
		inet6_socketlist = [],	%% list() of {Socket, SipSocket} - the first one is the one we send using
		socketlist
	       }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SOCKETOPTS, [{reuseaddr, true}, binary]).
%% v6 sockets have a default receive buffer size of 1k in Erlang R9C-0
-define(SOCKETOPTSv6, [{reuseaddr, true}, binary, inet6, {buffer, 8 * 1024}]).


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
    IPv4Spec =
	lists:foldl(fun(IP, Acc) ->
			    case inet_parse:ipv4_address(IP) of
				{ok, IPt} ->
				    [{udp, IPt, Port} | Acc];
				_ ->
				    Acc
			    end
		    end, [], lists:reverse(siphost:myip_list())),
    Spec =
	case yxa_config:get_env(enable_v6) of
	    {ok, true} ->
		[{udp6, {0,0,0,0,0,0,0,0}, Port} | IPv4Spec];
	    {ok, false} ->
		IPv4Spec
	end,
    start_listening(Spec, #state{socketlist = socketlist:empty()}).

%%--------------------------------------------------------------------
%% Function: start_listening(Spec, State)
%%           Spec  = list() of {Proto, IP, Port}
%%           Proto = udp | udp6
%%           IP    = {A,B,C,D} | {A,B,C,D,E,F,G,H,I}
%%           Port  = integer()
%% Descrip.: Begin listening on port Port for interface with IP IP.
%% Returns : {ok, State}    |
%%           {stop, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
start_listening([{udp, IP, Port} | T], State) when is_integer(Port), is_record(State, state) ->
    SocketOpts = [{ip, IP} | ?SOCKETOPTS],
    case gen_udp:open(Port, SocketOpts) of
	{ok, Socket} ->
	    Local = get_localaddr(Socket, "0.0.0.0"),
	    SipSocket = #sipsocket{module = sipsocket_udp,
				   proto  = udp,
				   pid    = self(),
				   data   = {Local, none},
				   id     = {udp, erlang:now()}	%% unique ID for Outbound
				  },
	    NewSocketList = socketlist:add({listener, udp, Port}, self(), udp, Local, none, SipSocket,
					   0, State#state.socketlist),
	    {LocalIP, _} = Local,
	    sipsocket:add_listener_info(udp, LocalIP, Port),
	    NewInetSocketL = [{Socket, SipSocket} | State#state.inet_socketlist],
	    start_listening(T, State#state{inet_socketlist = NewInetSocketL,
					   socketlist      = NewSocketList
					  });
	{error, Reason} ->
	    logger:log(error, "Could not open UDP socket (options ~p), port ~p : ~s",
		       [SocketOpts, Port, inet:format_error(Reason)]),
	    {stop, "Could not open UDP socket"}
    end;
start_listening([{udp6, IP, Port} | T], State) when is_integer(Port), is_record(State, state) ->
    SocketOpts = [{ip, IP} | ?SOCKETOPTSv6],
    case gen_udp:open(Port, SocketOpts) of
	{ok, Socket} ->
	    Local = get_localaddr(Socket, "[::]"),
	    SipSocket = #sipsocket{module = sipsocket_udp,
				   proto  = udp6,
				   pid    = self(),
				   data   = {Local, none},
				   id     = {udp6, erlang:now()}	%% unique ID for Outbound
				  },
	    NewSocketList = socketlist:add({listener, udp6, Port}, self(), udp6, Local, none, SipSocket,
					   0, State#state.socketlist),
	    {LocalIP, _} = Local,
	    sipsocket:add_listener_info(udp6, LocalIP, Port),
	    NewInet6SocketL = [{Socket, SipSocket} | State#state.inet6_socketlist],
	    start_listening(T, State#state{inet6_socketlist = NewInet6SocketL,
					   socketlist	    = NewSocketList
					  });
	{error, Reason} ->
	    logger:log(error, "Could not open IPv6 UDP socket (options ~p), port ~p : ~s",
		       [SocketOpts, Port, inet:format_error(Reason)]),
	    {stop, "Could not open IPv6 UDP socket"}
    end;
start_listening([], State) ->
    {ok, State}.


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
%% Descrip.: Get a socket for a certain protocol (Proto). Always
%%           return the first socket for the requested Proto for now.
%% Returns : {reply, Reply, State}
%%           Reply = {ok, SipSocket} |
%%                   {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
handle_call({get_socket, udp}, _From, #state{inet_socketlist = [{_Socket, SipSocket} | _]} = State) ->
    {reply, {ok, SipSocket}, State};

handle_call({get_socket, udp6}, _From, #state{inet6_socketlist = [{_Socket, SipSocket} | _]} = State) ->
    {reply, {ok, SipSocket}, State};

handle_call({get_socket, _Proto}, _From, State) ->
    {reply, {error, "No socket found"}, State};


%%--------------------------------------------------------------------
%% Function: handle_call({get_specific_socket, Id}, From, State)
%%           Id = tuple() ({Proto, Id})
%% Descrip.: Get a specific socket.
%% Returns : {reply, Reply, State}
%%           Reply = {ok, SipSocket} |
%%                   {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
handle_call({get_specific_socket, {udp, _} = Id}, _From, State) ->
    Res = get_sipsocket_id_match(Id, State#state.inet_socketlist),
    {reply, Res, State};

handle_call({get_specific_socket, {udp6, _} = Id}, _From, State) ->
    Res = get_sipsocket_id_match(Id, State#state.inet6_socketlist),
    {reply, Res, State};


%%--------------------------------------------------------------------
%% Function: handle_call({get_raw_socket, Proto}, From, State)
%%           Proto = atom(), udp | udp6
%% Descrip.: Get the raw socket.
%% Returns : {reply, Reply, State}
%%           Reply     = {ok, RawSocket}
%%           RawSocket = term()
%%--------------------------------------------------------------------
handle_call({get_raw_socket, udp}, _From, #state{inet_socketlist = [{Socket, _SipSocket} | _]} = State) ->
    {reply, {ok, Socket}, State};

handle_call({get_raw_socket, udp6}, _From, #state{inet6_socketlist = [{Socket, _SipSocket} | _]} = State) ->
    {reply, {ok, Socket}, State};

handle_call({get_raw_socket, _Proto}, _From, State) ->
    {reply, {error, "No socket found"}, State};


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
		case State#state.inet6_socketlist of
		    [] ->
			{error, no_inet6_socket};
		    [{Socket, _SipSocket} | _] ->
			{ok, AddrT} = inet_parse:ipv6_address(Addr),
			gen_udp:send(Socket, AddrT, Port, Message)
		end
	end,
    {reply, {send_result, SendRes}, State};
%%
%% Host is not IPv6 reference (inside brackets)
%%
handle_call({send, {Host, Port, Message}}, _From, State) when is_list(Host), is_integer(Port) ->
    SendRes = case State#state.inet_socketlist of
		  [] ->
		      {error, no_inet_socket};
		  [{Socket, _SipSocket} | _] ->
		      gen_udp:send(Socket, Host, Port, Message)
	      end,
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
%%           IPtuple  = term(), source IP address
%%           InPortNo = integer()
%%           Packet   = list()
%% Descrip.: Handle data received (as a signal) from one of our
%%           sockets.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_info({udp, Socket, IPtuple, InPortNo, Packet}, State) when is_integer(InPortNo) ->
    {Proto, SipSocket} =
	case lists:keysearch(Socket, 1, State#state.inet_socketlist) of
	    {value, {Socket, SipSocket4}} ->
		{udp, SipSocket4};
	    false ->
		case lists:keysearch(Socket, 1, State#state.inet6_socketlist) of
		    {value, {Socket, SipSocket6}} ->
			{udp6, SipSocket6};
		    false ->
			{nomatch, none}
		end
	end,

    case (Proto == udp orelse Proto == udp6) of
	true ->
	    IP = siphost:makeip(IPtuple),
	    received_packet(Packet, IPtuple, IP, InPortNo, Proto, Socket, SipSocket);
	false ->
	    logger:log(error, "Sipsocket UDP: Received gen_server info 'udp' "
		       "from unknown source '~p', ignoring", [Socket])
    end,

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
get_socket(#sipdst{proto = Proto}) when Proto == udp; Proto == udp6 ->
    case catch gen_server:call(sipsocket_udp, {get_socket, Proto}, 1500) of
	{ok, SipSocket} ->
	    SipSocket;
	{error, E} ->
	    logger:log(error, "Sipsocket UDP: Failed fetching socket for protocol ~p : ~p", [Proto, E]),
	    {error, E}
    end.

%%--------------------------------------------------------------------
%% Function: get_specific_socket(Id)
%%           Id = tuple() ({Proto, Id})
%% Descrip.: Return a specific socket. Used by draft-Outbound implem-
%%           entation to send requests using an existing flow, or not
%%           at all.
%% Returns : SipSocket       |
%%           {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
get_specific_socket({Proto, _} = Id) when Proto == udp; Proto == udp6 ->
    case catch gen_server:call(sipsocket_udp, {get_specific_socket, Id}) of
	{ok, SipSocket} ->
	    SipSocket;
	{error, E} ->
	    %% semi-normal to not find specific sockets
	    logger:log(debug, "Sipsocket UDP: Failed fetching specific socket with id ~p : ~p", [Id, E]),
	    {error, E}
    end.

%%--------------------------------------------------------------------
%% Function: get_raw_socket(Socket)
%%           Socket  = sipsocket record()
%% Descrip.: Get the raw TCP/UDP/TLS socket from the socket handler.
%%           Be careful with what you do with the raw socket - don't
%%           use it for sending/receiving for example. Intended for
%%           use in extractin certificate information of an SSL socket
%%           or similar.
%% Returns : {ok, RawSocket} |
%%           {error, Reason}
%%           RawSocket = term()
%%           Reason    = string()
%%--------------------------------------------------------------------
get_raw_socket(#sipsocket{proto = Proto}) when Proto == udp; Proto == udp6 ->
    case catch gen_server:call(sipsocket_udp, {get_raw_socket, Proto}) of
	{ok, RawSocket} ->
	    {ok, RawSocket};
	{error, E} ->
	    logger:log(error, "Sipsocket UDP: Failed fetching socket for protocol ~p : ~p", [Proto, E]),
	    {error, E}
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
%% Function: received_packet(Packet, IPtuple, IP, Port, Proto,
%%                           Socket, SipSocket)
%%           Packet     = binary(), packet received from network
%%           IPtuple    = term(), source IP address tuple
%%           IP         = string(), source IP address
%%           Port       = integer(), source port
%%           Proto      = atom(), source protocol
%%           Soclet     = term(), socket we received packet on
%%           SipSocket  = sipsocket record()
%% Descrip.: Check if a received packet is a keepalive or possibly a
%%           SIP message. For the latter case, we spawn
%%           sipserver:process(...) on the packet, after creating the
%%           necessary sipsocket and origin records.
%% Returns : ok
%%--------------------------------------------------------------------
received_packet(<<"\r\n">>, _IPtuple, IP, Port, Proto, _Socket, _SipSocket) ->
    logger:log(debug, "Keepalive packet (CRLF) received from ~p:~s:~p", [Proto, IP, Port]),
    ok;
received_packet(Packet, IPtuple, IP, Port, Proto, Socket, _SipSocket) when is_binary(Packet), size(Packet) =< 30 ->
    %% Too short to be anywhere near a real SIP message
    %% STUN packets (or whatever they are) look like "\r111.222.333.444:pppp"
    case is_only_nulls(Packet) of
	true ->
	    logger:log(debug, "Keepalive packet (~p NULLs) received from ~p:~s:~p",
		       [size(Packet), Proto, IP, Port]);
	false ->
	    case Packet of
		<<N:8, _Rest/binary>> when N == 0; N == 1 ->
		    case yxa_config:get_env(stun_demuxing_on_sip_ports) of
			{ok, true} ->
			    stun_process(Packet, Proto, IPtuple, IP, Port, Socket);
			{ok, false} ->
			    logger:log(debug, "Ignoring STUN packet received from ~p:~s:~p",
				       [Proto, IP, Port])
		    end;
		_ ->
		    logger:log(debug, "Packet received from ~p:~s:~p too short to be a SIP message :~n~p~n"
			       "(as list: ~p)", [Proto, IP, Port, Packet, binary_to_list(Packet)])
	    end
    end,
    ok;
received_packet(Packet, IPtuple, IP, Port, Proto, Socket, SipSocket)
  when is_binary(Packet), is_list(IP), is_integer(Port), is_atom(Proto), is_record(SipSocket, sipsocket) ->
    case Packet of
	<<N:8, _Rest/binary>> when N == 0; N == 1 ->
	    case yxa_config:get_env(stun_demuxing_on_sip_ports) of
		{ok, true} ->
		    stun_process(Packet, Proto, IPtuple, IP, Port, Socket);
		{ok, false} ->
		    logger:log(debug, "Ignoring STUN packet received from ~p:~s:~p",
			       [Proto, IP, Port])
	    end;
	_ ->
	    IP = siphost:makeip(IPtuple),
	    Origin = #siporigin{proto		= Proto,
				addr		= IP,
				port		= Port,
				receiver	= self(),
				sipsocket	= SipSocket
			       },
	    sipserver:safe_spawn(sipserver, process, [Packet, Origin, transactionlayer]),
	    ok
    end.

stun_process(Packet, Proto, IPtuple, IP, Port, Socket) ->
    {ok, {LocalIP, LocalPort}} = inet:sockname(Socket),
    SocketOpts =
	case Proto of
	    udp ->
		[{ip, LocalIP} | ?SOCKETOPTS];
	    udp6 ->
		[{ip, LocalIP} | ?SOCKETOPTSv6] -- [{buffer, 8 * 1024}]
	end,
    %% Get a socket with an alternate port number on the same interface that we received
    %% this STUN packet on. STUN has options for getting the server to answer using a
    %% different port (which is unspecified), and different IP address than the request
    %% was received on (to help classify NAT devices between the client and server).
    %% We currently only support the 'use other port' mode - not the 'use other IP'.
    {AltSocket, _AltIP, AltPort} =
	case gen_udp:open(0, SocketOpts) of
	    {ok, AltSocket1} ->
		{ok, {A, P}} = inet:sockname(AltSocket1),
		{AltSocket1, A, P};
	    {error, Reason} ->
		logger:log(error, "Failed acquiring a second socket for alternate-port STUN using socket options "
			   "~p : ~p", [SocketOpts, Reason]),
		{undefined, undefined, undefined}
	end,

    StunEnv =
	#stun_env{proto			= Proto,
		  local_ip		= LocalIP,
		  local_port		= LocalPort,
		  remote_ip		= IPtuple,
		  remote_ip_str		= IP,
		  remote_port		= Port,
		  alt_ip		= undefined,	%% Alternate IP not supported (patches welcome)
		  alt_port		= AltPort
		 },

    case stun:process(Packet, StunEnv) of
	#stun_result{action = send_response, change = Change, response = STUNResponse} ->
	    case Change of
		none ->	gen_udp:send(Socket, IP, Port, STUNResponse);
		port ->	gen_udp:send(AltSocket, IP, Port, STUNResponse);
		ip ->
		    %% This should never happen since we set StunEnv alt_ip to 'undefined', but who knows...
		    %% We don't want the UDP receiving gen_server to crash, so we check for it.
		    logger:log(debug, "STUN module request us to change IP for the response, but that is not "
			       "currently supported - not sending STUN response");
		both ->
		    %% This should never happen since we set StunEnv alt_ip to 'undefined', but who knows...
		    %% We don't want the UDP receiving gen_server to crash, so we check for it.
		    logger:log(debug, "STUN module request us to change both IP and port for the response, "
			       "but changing IP is currently not supported - not sending STUN response")
	    end;
	ignore ->
	    ok;
	{error, not_stun} ->
	    logger:log(debug, "Non-STUN packet received from ~p:~s:~p : ~n~p~n"
		       "(as list: ~p)", [Proto, IP, Port, Packet, binary_to_list(Packet)]);
	{error, Reason2} ->
	    logger:log(debug, "Failed processing STUN packet from ~p:~s:~p : ~p",
		       [Proto, IP, Port, Reason2]),
	    ok
    end,
    gen_udp:close(AltSocket),
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

%%--------------------------------------------------------------------
%% Function: get_sipsocket_id_match(Id, L)
%%           Id = term()
%%           L  = list() of sipsocket record()
%% Descrip.: Look for sipsocket with id matching Id.
%% Returns : {ok, SipSocket} |
%%           {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
get_sipsocket_id_match(Id, [{_Socket, #sipsocket{id = Id} = SipSocket} | _T]) ->
    {ok, SipSocket};
get_sipsocket_id_match(Id, [_H | T]) ->
    get_sipsocket_id_match(Id, T);
get_sipsocket_id_match(_Id, []) ->
    {error, "No socket found"}.
