%%%-------------------------------------------------------------------
%%% File    : tcp_connection.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Handles a single TCP connection - executes send and gets
%%%           complete received SIP messages from a single TCP
%%%           receiver associated with this TCP connection.
%%%
%%% @since    15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
%%%-------------------------------------------------------------------
-module(tcp_connection).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([connect_to/2,
	 connection_from/4,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1,
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
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  socketmodule,
	  socket,
	  receiver,
	  starttime,
	  sipsocket,
	  timeout,
	  on=false
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%% Our socket options
-define(SOCKETOPTS, [binary, {packet, 0}, {active, false}]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (SocketModule, Proto, Socket, HostPort) ->
%%            {ok, Pid} | ignore
%%
%%            SocketModule = gen_tcp | ssl "socket module"
%%            Proto        = tcp | tcp6 | tls | tls6
%%            Socket       = term()
%%            HostPort     = #hp{} "local/remote IP/port info"
%%
%% @doc     Handle an incoming connection. Starts a tcp_connection
%%          gen_server to handle this socket. Note : 'ignore' is
%%          returned if the socket is not acceptable for some reason
%%          (e.g. SSL certificate validation failed)
%% @end
%%--------------------------------------------------------------------
connection_from(SocketModule, Proto, Socket, HostPort) when is_atom(SocketModule), is_atom(Proto),
							    is_record(HostPort, hp) ->
    SSLNames = HostPort#hp.r_ip,
    gen_server:start(?MODULE, {in, SocketModule, Proto, Socket, HostPort, SSLNames}, []).

%%--------------------------------------------------------------------
%% @spec    (Dst, GenServerFrom) ->
%%            {ok, Pid} | Error
%%
%%            Dst           = #sipdst{}
%%            GenServerFrom = term() "send result of connection attempt to this caller using gen_server:reply()."
%%
%%            Error = term() "result of gen_server:start()"
%%
%% @doc     Starts a tcp_connection gen_server and try to connect to a
%%          remote destination. Will eventually send result of
%%          connection attempt to GenServerFrom using
%%          gen_server:reply().
%% @end
%%--------------------------------------------------------------------
connect_to(Dst, GenServerFrom) when is_record(Dst, sipdst) ->
    gen_server:start_link(?MODULE, {connect, Dst, GenServerFrom}, []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    init(Arg) ->
%%            {ok, State}          |
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({connect, Dst, GenServerFrom}) -> {ok, State} | ignore
%%
%%            Dst           = #sipdst{}
%%            GenServerFrom = term() "gen_server:call From - used to reply with the new socket to the process that requested it."
%%
%% @doc     Try to connect to a remote host, and answer GenServerFrom
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({connect, Dst, GenServerFrom}) when is_record(Dst, sipdst) ->
    %% to avoid trying to connect to a destination more than once at the same time,
    %% we insert Dst and our own pid into the ets table transportlayer_tcp_conn_queue.
    QKey = {Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port},
    case ets:insert_new(transportlayer_tcp_conn_queue, {QKey, self()}) of
	true ->
	    %% To not block tcp_dispatcher (who starts these tcp_connections), we
	    %% send ourself a cast and return immediately.
	    gen_server:cast(self(), {connect_to_remote, Dst, GenServerFrom}),
	    {ok, #state{}};
	false ->
	    logger:log(debug, "TCP connection: Not starting parallell connection to ~s",
		       [sipdst:dst2str(Dst)]),
	    case ets:lookup(transportlayer_tcp_conn_queue, QKey) of
		[{QKey, ConnPid}] ->
		    %% Tell ConnPid to do a gen_server:reply() to GenServerFrom
		    %% when it has a connection result
		    logger:log(debug, "TCP connection: Telling ~p to answer for me, and exiting",
			       [ConnPid]),
		    ConnPid ! {also_notify, GenServerFrom},
		    ignore;
		[] ->
		    logger:log(debug, "TCP connection: The other connecting pid seems to have "
			       "completed right now - answering try_again"),
		    gen_server:reply(GenServerFrom, {error, try_again}),
		    ignore
	    end
    end;

%%--------------------------------------------------------------------
%% @spec    ({Direction, SocketModule, Proto, Socket, HostPort,
%%          SSLNames}) ->
%%            {ok, State, Timeout} |
%%            {stop, Reason}
%%
%%            Direction    = in | out
%%            SocketModule = gen_tcp | ssl "the name of the socket module this socket uses"
%%            Proto        = tcp | tcp6 | tls | tls6
%%            Socket       = term()
%%            HP           = #hp{} "local/remote IP/port info"
%%            SSLNames     = [string()] "certificate names to verify a SSL certificate against, or a list containing a single string() with the"
%%            IP of the remote host if this is an inbound non-TLS connection
%%
%% @doc     Initialize this tcp_connection process to handle Socket.
%%          This function is called either directly from start_link,
%%          or in case we are connecting to a remote party, by
%%          handle_call({connect_to_remote, ...) when it has
%%          established a connection that needs to be checked and
%%          initialized.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({Direction, SocketModule, Proto, Socket, HP, SSLNames}) when is_record(HP, hp) ->
    %% First check if socket is acceptable
    Remote = {HP#hp.r_ip, HP#hp.r_port},
    case is_acceptable_socket(Socket, Direction, Proto, Remote, SSLNames) of
	true ->
	    init2([Direction, SocketModule, Proto, Socket, HP]);
	false ->
	    {Res, DirStr} =
		case Direction of
		    in -> {ignore, "from"};
		    out -> {{stop, unacceptable}, "to"}
		end,
	    {IP, Port} = Remote,
	    logger:log(normal, "TCP connection: Closing unacceptable connection ~s ~p:~s:~p",
		       [DirStr, Proto, IP, Port]),
	    SocketModule:close(Socket),
	    ets:delete(transportlayer_tcp_conn_queue, {Proto, IP, Port}),
	    Res
    end.

init2([Direction, SocketModule, Proto, Socket, HP]) when Direction == in; Direction == out ->
    %% create unique ID for Outbound
    Id =  #ob_id{proto	= Proto,
		 id	= erlang:now()
		},
    SipSocket = #sipsocket{module	= sipsocket_tcp,
			   proto	= Proto,
			   pid		= self(),
			   hostport	= HP,
			   id		= Id
			  },
    %% Register this connection with the TCP listener process before we proceed
    case gen_server:call(tcp_dispatcher, {register_sipsocket, Direction, SipSocket}) of
	ok ->
	    {ok, Receiver} = start_tcp_receiver(SocketModule, Socket, SipSocket, Direction),
	    {ok, TimeoutSec} = yxa_config:get_env(tcp_connection_idle_timeout),
	    Timeout = TimeoutSec * 1000,
	    Now = util:timestamp(),
	    State = #state{socketmodule	= SocketModule,
			   socket	= Socket,
			   receiver	= Receiver,
			   starttime	= Now,
			   sipsocket	= SipSocket,
			   timeout	= Timeout,
			   on		= true
			  },
	    {ok, State, Timeout};
	{error, E} ->
	    logger:log(error, "TCP connection: Failed registering with TCP dispatcher : ~p", [E]),
	    SocketModule:close(Socket),
	    {stop, "Failed registering with TCP dispatcher"}
    end.


%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% @spec    ({send, {Host, Port, Message}}, From, State) ->
%%            {reply, Reply, State, Timeout}
%%
%%            Host    = string()
%%            Port    = integer()
%%            Message = list()
%%
%%            Reply   = {send_result, SendRes}
%%            SendRes = term() "result of SocketModule:send()"
%%
%% @doc     Send data on our socket, provided that State#state.on ==
%%          true.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({send, {_Host, _Port, Message}}, _From, State) when State#state.on == true ->
    %% XXX verify that Host:Port matches host and port in State#State.sipsocket!
    SocketModule = State#state.socketmodule,
    SendRes = (catch SocketModule:send(State#state.socket, Message)),
    Reply = {send_result, SendRes},
    {reply, Reply, State, State#state.timeout};

%%--------------------------------------------------------------------
%% @spec    ({get_receiver}, From, State) ->
%%            {reply, Reply, State, Timeout}
%%
%%            Reply  = {ok, Pid}       | {error, Reason}
%%            Pid    = pid()
%%            Reason = string()
%%
%% @doc     Get our receiver processes pid.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({get_receiver}, _From, State) when State#state.on == true ->
    {reply, {ok, State#state.receiver}, State, State#state.timeout};
handle_call({get_receiver}, _From, State) ->
    {reply, {error, "Not started"}, State, State#state.timeout};

%%--------------------------------------------------------------------
%% @spec    (get_raw_socket, From, State) ->
%%            {reply, Reply, State, Timeout}
%%
%%            Reply     = {ok, RawSocket} | {error, Reason}
%%            RawSocket = term()
%%
%% @doc     Get the raw socket we are using.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(get_raw_socket, _From, State) when State#state.on == true ->
    {reply, {ok, State#state.socket}, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% @spec    handle_cast(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% @spec    ({connect_to_remote, Dst, GenServerFrom}, State) ->
%%            {noreply, NewState, Timeout} |
%%            {stop, Reason, State}
%%
%%            Dst           = #sipdst{}
%%            GenServerFrom = term() "gen_server From data"
%%
%%            Reason = normal | string()
%%
%% @doc     Connect to a remote Host on port Port over protocol Proto
%%          provided that State#state.on == false. Send the result of
%%          this operation to GenServerFrom using gen_server:reply().
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({connect_to_remote, #sipdst{proto = Proto, addr = Host, port = Port} = Dst, GenServerFrom},
	    #state{on = false} = State) when (Proto == tcp orelse Proto == tcp6 orelse
					      Proto == tls orelse Proto == tls6), is_list(Host), is_integer(Port) ->

    DstStr = sipdst:dst2str(Dst),
    {ok, ConnectTimeoutSec} = yxa_config:get_env(tcp_connect_timeout),
    ConnectTimeout = ConnectTimeoutSec * 1000,
    {ok, InetModule, SocketModule, Options} = get_settings(Proto),
    Host2 = util:remove_v6_brackets(Host),

    %% Measure how long time it takes to connect to remote hosts
    {TimeSpent, ConnectRes} = timer:tc(SocketModule, connect, [Host2, Port, Options, ConnectTimeout]),
    logger:log(debug, "TCP connection: Extra debug: Time spent connecting to ~s : ~p ms",
	       [DstStr, TimeSpent div 1000]),

    case ConnectRes of
	{ok, NewSocket} ->
	    case SocketModule of
		ssl ->
		    {ok, {Protocol, Cipher}} = ssl:connection_info(NewSocket),
		    logger:log(debug, "Extra debug: TCP connection: SSL socket info for ~p : "
			       "Protocol = ~p, Cipher = ~p", [NewSocket, Protocol, Cipher]);
		_ -> ok
	    end,
	    {L_IP, L_Port} = get_local_ip_port(NewSocket, InetModule, Proto),
	    HP =
		#hp{l_ip   = L_IP,
		    l_port = L_Port,
		    r_ip   = Host,
		    r_port = Port
		   },

	    %% Call init() to get the connection properly initialized before we do our gen_server:reply()
	    case init({out, SocketModule, Proto, NewSocket, HP, Dst#sipdst.ssl_names}) of
		{ok, NewState, Timeout} ->
		    SipSocket = NewState#state.sipsocket,
		    logger:log(debug, "TCP connection: Extra debug: Connected to ~s, socket ~p",
			       [DstStr, SipSocket]),

		    %% This is the answer to a 'gen_server:call(tcp_dispatcher, {get_socket ...)' that
		    %% someone (GenServerFrom) made, but where there were no cached connection available.
		    gen_server:reply(GenServerFrom, {ok, SipSocket}),

		    ets:delete(transportlayer_tcp_conn_queue, {Proto, Host, Port}),

		    {noreply, NewState, Timeout};
		{stop, Reason} ->
		    gen_server:reply(GenServerFrom, {error, Reason}),
		    connect_fail_handle_conn_queue(Proto, Host, Port, {error, Reason}),
		    {stop, normal, State}
	    end;
	{error, econnrefused} ->
	    gen_server:reply(GenServerFrom, {error, "Connection refused"}),
	    connect_fail_handle_conn_queue(Proto, Host, Port, {error, "Connection refused"}),
	    {stop, normal, State};
	{error, ehostunreach} ->
	    %% If we don't get a connection before ConnectTimeout, we end up here
	    gen_server:reply(GenServerFrom, {error, "Host unreachable"}),
	    connect_fail_handle_conn_queue(Proto, Host, Port, {error, "Host unreachable"}),
	    {stop, normal, State};
	{error, E} ->
	    logger:log(error, "TCP connection: Failed connecting to ~s : ~p (~s)",
		       [DstStr, E, inet:format_error(E)]),
	    gen_server:reply(GenServerFrom, {error, "Failed connecting to remote host"}),
	    connect_fail_handle_conn_queue(Proto, Host, Port, {error, E}),
	    {stop, normal, State}
    end;

%%--------------------------------------------------------------------
%% @spec    ({recv_sipmsg, Data}, State) -> {noreply, State, Timeout}
%%
%%            Data = #request{} | #response{}
%%
%% @doc     Our receiver process has received a SIP message on our
%%          socket, invoke sipserver:process() on it.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({recv_sipmsg, Msg}, State) when State#state.on == true ->
    #sipsocket{proto	= Proto,
	       hostport	= #hp{r_ip   = IP,
			      r_port = Port
			     }
	      } = State#state.sipsocket,
    Origin = #siporigin{proto		= Proto,
			addr		= IP,
			port		= Port,
			receiver	= self(),
			sipsocket	= State#state.sipsocket
		       },
    sipserver:safe_spawn(sipserver, process, [Msg, Origin]),
    {noreply, State, State#state.timeout};

%%--------------------------------------------------------------------
%% @spec    ({send_stun_response, STUNresponse}, State) ->
%%            {noreply, State, Timeout}
%%
%%            STUNresponse = iolist() "data to send to peer"
%%
%% @doc
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({send_stun_response, STUNresponse}, State) when State#state.on == true ->
    #sipsocket{proto	= Proto,
	       hostport	= #hp{r_ip   = IP,
			      r_port = Port
			     }
	      } = State#state.sipsocket,
    logger:log(debug, "TCP connection: Extra debug: Sending STUN response to ~p:~s:~p",
	      [Proto, IP, Port]),
    SocketModule = State#state.socketmodule,
    (catch SocketModule:send(State#state.socket, STUNresponse)),
    {noreply, State, State#state.timeout};

%%--------------------------------------------------------------------
%% @spec    ({close, FromPid}, State) -> {stop, normal, NewState}
%%
%%            FromPid = pid()
%%
%% @doc     A request to close this connection.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({close, FromPid}, State) when is_pid(FromPid) ->
    Duration = util:timestamp() - State#state.starttime,
    #sipsocket{proto	= Proto,
	       hostport	= #hp{r_ip   = IP,
			      r_port = Port
			     }
	      } = State#state.sipsocket,
    logger:log(debug, "TCP connection: Closing connection with ~p:~s:~p (duration: ~p seconds)",
	       [Proto, IP, Port, Duration]),
    SocketModule = State#state.socketmodule,
    Receiver = State#state.receiver,
    %% If the receiver tells us to close a SSL socket, the receiver will actually
    %% already have closed the connection (since it is 'controlling process').
    case {SocketModule, Receiver} of
	{ssl, FromPid} ->
	    ok;
	_ ->
	    case catch SocketModule:close(State#state.socket) of
		ok -> ok;
		E ->
		    logger:log(error, "TCP connection: Closing socket ~p (using socketmodule ~p) "
			       "failed : ~p", [SocketModule, E])
	    end
    end,
    {stop, normal, State#state{socket = undefined}};

%%--------------------------------------------------------------------
%% @spec    ({connection_closed, From}, State) ->
%%            {stop, normal, NewState}
%%
%% @doc     Our receiver signals us that it detected that the other
%%          end closed the connection.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({connection_closed, FromPid}, #state{receiver=FromPid}=State) when is_pid(FromPid) ->
    Duration = util:timestamp() - State#state.starttime,
    #sipsocket{proto	= Proto,
	       hostport	= #hp{r_ip   = IP,
			      r_port = Port
			     }
	      } = State#state.sipsocket,
    logger:log(debug, "TCP connection: Connection with ~p:~s:~p closed by foreign host (duration: ~p seconds)",
	       [Proto, IP, Port, Duration]),
    %% Call close() on the socket just to make sure. For SSL, the TCP receiver will have done this for us.
    case State#state.socketmodule of
	ssl -> true;
	SocketModule ->
	    SocketModule:close(State#state.socket)
    end,
    {stop, normal, State#state{socket = undefined}};

handle_cast(Msg, State) ->
    logger:log(error, "TCP connection: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State, State#state.timeout}.


%%--------------------------------------------------------------------
%% @spec    handle_info(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling all non call/cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (timeout, State) -> {stop, normal, NewState}
%%
%% @doc     This connection has neither received nor sent any data in
%%          our configured maximum time period. Make the socket go
%%          away.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) when State#state.on == true ->
    #sipsocket{proto	= Proto,
	       hostport	= #hp{r_ip   = IP,
			      r_port = Port
			     }
	      } = State#state.sipsocket,
    Timeout = State#state.timeout,
    logger:log(debug, "Sipsocket TCP: Connection with ~p:~s:~p timed out after ~p seconds, "
	       "connection handler terminating.", [Proto, IP, Port, Timeout div 1000]),
    case State#state.socketmodule of
	ssl ->
	    %% For SSL, we signal the receiver and let it close the connection
	    %% and terminate quietly
	    State#state.receiver ! {quit_receiver, self()};
	SocketModule ->
	    %% the tcp_receiver will notice that the socket is closed and exit
	    logger:log(debug, "Sipsocket TCP: Closing socket ~p", [State#state.socket]),
	    SocketModule:close(State#state.socket)
    end,
    {stop, normal, State#state{socket = undefined}};

%%--------------------------------------------------------------------
%% @spec    ({also_notify, GenServerFrom}, State) ->
%%            {noreply, State, Timeout::integer()}
%%
%%            GenServerFrom = term()
%%
%% @doc     Do a gen_server:reply with our sipsocket to all processes
%%          that have queued with us instead of starting a parallell
%%          connection attempt to the same destination we have now
%%          connected to.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({also_notify, GenServerFrom}, State) when State#state.on == true ->
    gen_server:reply(GenServerFrom, {ok, State#state.sipsocket}),
    {noreply, State, State#state.timeout};

handle_info(Info, State) ->
    logger:log(debug, "TCP connection: Received unknown gen_server info :~n~p", [Info]),
    {noreply, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    %% if someone is waiting in the connection queue for us when we terminate normally, then
    %% that is an error
    normal;
terminate(Reason, _State) ->
    logger:log(error, "TCP connection: Terminating for some other reason than 'normal' : ~n~p",
	       [Reason]),
    Reason.

%%--------------------------------------------------------------------
%% @spec    (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @spec    (SocketModule, Socket, SipSocket, Dir) -> {ok, Receiver}
%%
%%            SocketModule = gen_tcp | ssl
%%            Socket       = term() "the socket"
%%            SipSocket    = #sipsocket{}
%%            Direction    = in | out
%%
%% @doc     Start a receiver that does blocking read on the socket.
%% @end
%%--------------------------------------------------------------------
start_tcp_receiver(SocketModule, Socket, SipSocket, Dir) when is_atom(SocketModule), is_record(SipSocket, sipsocket),
							      (Dir == in orelse Dir == out) ->
    Receiver = tcp_receiver:start_link(SocketModule, Socket, SipSocket),
    S = case Dir of
	    in -> "Connection from";
	    out -> "Connected to"
	end,
    %% If it is an SSL socket, we must change controlling_process() to the tcp_receiver.
    %% Don't do this here if Dir is 'in' though, because this process is not the
    %% controlling process for those and tcp_listener will change controlling process
    %% to the receiver as soon as we have started it and init() has returned.
    case {SocketModule, Dir} of
	{ssl, out} ->
	    case SocketModule:controlling_process(Socket, Receiver) of
		ok ->
		    logger:log(debug, "TCP connection: Changed controlling process of socket ~p to receiver ~p",
			       [Socket, Receiver]),
		    ok;
		{error, Reason} ->
		    logger:log(error, "TCP connection: Failed changing controlling process of SSL socket ~p "
			       "to receiver ~p : ~p", [Socket, Receiver, Reason]),
		    erlang:error({error, "Failed changing controlling process of SSL socket",
				  {error, Reason}}, [[SocketModule, Socket, SipSocket, Dir]])
	    end;
	_ ->
	    ok
    end,
    #sipsocket{proto	= Proto,
	       hostport	= HP
	      } = SipSocket,
    logger:log(debug, "TCP connection: ~s ~p:~s:~p (socket ~p, started receiver ~p)",
	       [S, Proto, HP#hp.r_ip, HP#hp.r_port, Socket, Receiver]),
    {ok, Receiver}.

%%--------------------------------------------------------------------
%% @spec    (Socket, Dir, Proto, Remote, Names) -> true | false
%%
%%            Socket = term()
%%            Dir    = in | out
%%            Proto  = tls | tcp
%%            Remote = {IP, Port}
%%            IP     = string()
%%            Port   = integer()
%%            Names  = [string()] "list of names for the certificate that the upper layer is willing to accept"
%%
%% @doc     Check if a socket is 'acceptable'. For SSL, this means
%%          verify that the subjectAltName/CN is included in Names.
%% @end
%%--------------------------------------------------------------------
%%
%% SSL socket
%%
is_acceptable_socket(Socket, Dir, Proto, Remote, Names) when (Proto == tls orelse Proto == tls6), is_list(Names) ->
    ssl_util:is_acceptable_ssl_socket(Socket, Dir, Proto, Remote, Names);
%%
%% Non-SSL socket
%%
is_acceptable_socket(Socket, Dir, Proto, Names, Port) ->
    case local:is_acceptable_socket(Socket, Dir, Proto, Names, Port, ?MODULE, undefined) of
	true ->
	    true;
	false ->
	    false;
	undefined ->
	    %% Default accept
	    true
    end.

%%--------------------------------------------------------------------
%% @spec    (Proto) ->
%%            {ok, InetModule, SocketModule, Options}
%%
%%            Proto = tcp | tcp6 | tls | tls6
%%
%%            InetModule   = inet | ssl
%%            SocketModule = gen_tcp | ssl
%%            Options      = list() "socket options"
%%
%% @doc     Get the variable things depending on protocol.
%% @end
%%--------------------------------------------------------------------
get_settings(tcp) ->
    {ok, inet, gen_tcp, ?SOCKETOPTS};
get_settings(tcp6) ->
    {ok, inet, gen_tcp, [inet6 | ?SOCKETOPTS]};
get_settings(tls) ->
    {ok, ssl, ssl, ?SOCKETOPTS ++ get_settings_tls()};
get_settings(tls6) ->
    {ok, ssl, ssl, ?SOCKETOPTS ++ [inet6 | get_settings_tls()]}.

%% part of get_settings/1, get SSL settings
get_settings_tls() ->
    {ok, L1} = yxa_config:get_env(ssl_client_ssloptions),
    case yxa_config:get_env(ssl_client_certfile) of
	{ok, File} ->
	    [{certfile, File} | L1];
	none ->
	    L1
    end.

%%--------------------------------------------------------------------
%% @spec    (Socket, InetModule, Proto) ->
%%            {IP, Port}
%%
%%            Socket     = term() "our new socket"
%%            InetModule = inet | ssl
%%            Proto      = tcp | tcp6 | tls | tls6
%%
%%            IP   = string()
%%            Port = integer()
%%
%% @doc     Get the local address and port of a socket.
%% @end
%%--------------------------------------------------------------------
get_local_ip_port(Socket, InetModule, Proto) ->
    case InetModule:sockname(Socket) of
	{ok, {IPlist, Port}} ->
	    {siphost:makeip(IPlist), Port};
	{error, E1} ->
	    logger:log(error, "TCP connection: ~p:sockname() on new socket returned error ~p",
		       [InetModule, E1]),
	    {get_defaultaddr(Proto), 0}
    end.

%%--------------------------------------------------------------------
%% @spec    (Proto) ->
%%            Addr
%%
%%            Proto = tcp | tcp6
%%
%%            Addr = string()
%%
%% @doc     Get the "any" address.
%% @end
%%--------------------------------------------------------------------
get_defaultaddr(tcp) -> "0.0.0.0";
get_defaultaddr(tcp6) -> "[::]".

%%--------------------------------------------------------------------
%% @spec    (Proto, Host, Port, Reply) -> ok
%%
%%            Proto = tcp | tcp6 | tls | tls6
%%            Host  = string() "IP address"
%%            Port  = integer()
%%            Reply = term() "probably {error, Reason}"
%%
%% @doc     When we have failed connecting to a remote destination, we
%%          remove the entry saying that we are trying from the
%%          connection queue ets table, and then notify any other
%%          processes waiting for a connection to Proto:Host:Port
%%          about our failure.
%% @end
%%--------------------------------------------------------------------
connect_fail_handle_conn_queue(Proto, Host, Port, Reply) ->
    %% we are no longer trying to connect to this destination
    ets:delete(transportlayer_tcp_conn_queue, {Proto, Host, Port}),

    %% tell processes in queue about our failure
    check_also_notify(Reply),
    %% sleep, then check notify queue again to try and handle race here
    %% XXX this needs to be truly fixed!
    timer:sleep(100),
    check_also_notify(Reply),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Reply) -> ok
%%
%%            Reply = term() "but probably {error, Reason}"
%%
%% @doc     Tell any processes that have queued with this one about
%%          the result of our connection attempt. If we have
%%          succeeded, this function will not be called (instead
%%          handle_info({also_notify, ) will deliver good news to
%%          processes in notify queue.
%% @end
%%--------------------------------------------------------------------
check_also_notify(Reply) ->
    receive
	{also_notify, GenServerFrom} ->
	    logger:log(debug, "TCP connection: Telling gen_serverfrom ~p about my failure : ~p",
		       [GenServerFrom, Reply]),
	    gen_server:reply(GenServerFrom, Reply),
	    check_also_notify(Reply)
    after 0 ->
	    ok
    end.



%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    ok.
