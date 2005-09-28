%%%-------------------------------------------------------------------
%%% File    : tcp_connection.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Handles a single TCP connection - executes sen and gets
%%%           complete received SIP messages from a single TCP
%%%           receiver associated with this TCP connection.
%%%
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(tcp_connection).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/3,
	 start_link/6,

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
-include_lib("ssl/include/SSL-PKIX.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  socketmodule,
	  proto,
	  socket,
	  receiver,
	  local,
	  remote,
	  starttime,
	  sipsocket,
	  timeout,
	  on=false
	 }).

-record(ssl_conn_subject, {
	  countryName,
	  organizationName,
	  commonName,
	  description
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
%% Function: start_link(in, SocketModule, Proto, Socket, Local,
%%                      Remote)
%%           SocketModule = atom(), socket module (gen_tcp | ssl)
%%           Proto        = atom(), tcp | tcp6 | tls | tls6
%%           Socket       = term()
%%           Local        = term() ({Host, Port} tuple())
%%           Remote       = term() ({Host, Port} tuple())
%% Descrip.: Incoming connection. Starts a tcp_connection gen_server
%%           to handle this socket.
%% Returns : {ok, Pid} | ignore
%% Note    : 'ignore' is returned if the socket is not acceptable for
%%           some reason (e.g. SSL certificate validation failed)
%%--------------------------------------------------------------------
start_link(in, SocketModule, Proto, Socket, Local, Remote) ->
    {Validation, _RPort} = Remote,
    gen_server:start_link(?MODULE, [in, SocketModule, Proto, Socket, Local, Remote, Validation], []).

%%--------------------------------------------------------------------
%% Function: start_link(connect, Dst, GenServerFrom)
%%           Dst           = sipdst record()
%%           GenServerFrom = term(), send result of connection attempt
%%                           to this caller using gen_server:reply().
%% Descrip.: Starts a tcp_connection gen_server and try to connect to
%%           a remote destination. Will eventually send result of
%%           connection attempt to GenServerFrom using
%%           gen_server:reply().
%% Returns : {ok, Pid} | Error
%%           Error = term(), result of gen_server:start()
%%--------------------------------------------------------------------
start_link(connect, Dst, GenServerFrom) when is_record(Dst, sipdst) ->
    gen_server:start_link(?MODULE, [connect, Dst, GenServerFrom], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(L)
%% Descrip.: Initiates the server
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: init([connect, Dst, GenServerFrom])
%%           Dst           = sipdst record()
%%           GenServerFrom = term(), gen_server:call From - used to
%%                           reply with the new socket to the process
%%                           that requested it.
%% Descrip.: Try to connect to a remote host, and answer GenServerFrom
%% Returns : {ok, State} | ignore
%%--------------------------------------------------------------------
init([connect, Dst, GenServerFrom]) when is_record(Dst, sipdst) ->
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
%% Function: init([Direction, SocketModule, Proto, Socket, Local,
%%                 Remote, SSLNames])
%%           Direction    = in | out
%%           SocketModule = atom(), the name of the socket module
%%                          this socket uses (gen_tcp | ssl)
%%           Proto        = atom(), tcp | tcp6 | tls | tls6
%%           Socket       = term()
%%           Local        = term() ({Host, Port} tuple())
%%           Remote       = term() ({Host, Port} tuple())
%%           Validation   = list() of string(), certificate names to
%%                          verify a SSL certificate against, or a
%%                          list containing a single string() with the
%%			    IP of the remote host if this is an
%%                          inbound non-TLS connection
%% Descrip.: Initialize this tcp_connection process to handle Socket.
%%           This function is called either directly from start_link,
%%           or in case we are connecting to a remote party, by
%%           handle_call({connect_to_remote, ...) when it has
%%           established a connection that needs to be checked and
%%           initialized.
%% Returns : {ok, State, Timeout} |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([Direction, SocketModule, Proto, Socket, Local, Remote, Validation]) ->
    %% First check if socket is acceptable
    case is_acceptable_socket(Socket, Direction, Proto, Remote, Validation) of
	true ->
	    init2([Direction, SocketModule, Proto, Socket, Local, Remote]);
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

init2([Direction, SocketModule, Proto, Socket, Local, Remote]) ->
    SipSocket = #sipsocket{module=sipsocket_tcp, proto=Proto, pid=self(), data={Local, Remote}},
    %% Register this connection with the TCP listener process before we proceed
    case gen_server:call(tcp_dispatcher, {register_sipsocket, Direction, SipSocket}) of
	ok ->
	    {ok, Receiver} = start_tcp_receiver(SocketModule, Socket, SipSocket, Direction),
	    {ok, TimeoutSec} = yxa_config:get_env(tcp_connection_idle_timeout),
	    Timeout = TimeoutSec * 1000,
	    Now = util:timestamp(),
	    State = #state{socketmodule	= SocketModule,
			   proto	= Proto,
			   socket	= Socket,
			   receiver	= Receiver,
			   local	= Local,
			   remote	= Remote,
			   starttime	= Now,
			   sipsocket	= SipSocket,
			   timeout	= Timeout,
			   on		= true
			  },
	    {ok, State, Timeout};
	{error, E} ->
	    logger:log(error, "TCP connection: Failed registering with TCP dispatcher : ~p", [E]),
	    {stop, "Failed registering with TCP dispatcher"}
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
%% Function: handle_call({send, {Host, Port, Message}}, From, State)
%%           Host = string()
%%           Port = integer()
%%           Message = list()
%% Descrip.: Send data on our socket, provided that
%%           State#state.on == true.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply = {send_result, SendRes}
%%           SendRes = term(), result of SocketModule:send()
%%--------------------------------------------------------------------
handle_call({send, {_Host, _Port, Message}}, _From, State) when State#state.on == true ->
    %% XXX verify that Host:Port matches host and port in State#State.sipsocket!
    SocketModule = State#state.socketmodule,
    SendRes = case catch SocketModule:send(State#state.socket, Message) of
		  R -> R
	      end,
    Reply = {send_result, SendRes},
    {reply, Reply, State, State#state.timeout};

%%--------------------------------------------------------------------
%% Function: handle_call({get_receiver}, From, State)
%% Descrip.: Get our receiver processes pid.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply = {ok, Pid}       |
%%                   {error, Reason}
%%           Pid    = pid()
%%           Reason = string()
%%--------------------------------------------------------------------
handle_call({get_receiver}, _From, State) when State#state.on == true ->
    {reply, {ok, State#state.receiver}, State, State#state.timeout};
handle_call({get_receiver}, _From, State) ->
    {reply, {error, "Not started"}, State, State#state.timeout};

%%--------------------------------------------------------------------
%% Function: handle_call(get_raw_socket, From, State)
%% Descrip.: Get the raw socket we are using.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply = {ok, RawSocket} |
%%                   {error, Reason}
%%           RawSocket = term()
%%--------------------------------------------------------------------
handle_call(get_raw_socket, _From, State) when State#state.on == true ->
    {reply, {ok, State#state.socket}, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: handle_cast({connect_to_remote, Dst, GenServerFrom},
%%                       State)
%%           Dst           = sipdst record()
%%           GenServerFrom = term(), gen_server From data
%% Descrip.: Connect to a remote Host on port Port over protocol Proto
%%           provided that State#state.on == false. Send the result of
%%           this operation to GenServerFrom using gen_server:reply().
%% Returns : {noreply, NewState, Timeout} |
%%           {stop, Reason, State}
%%           Reason = normal   |
%%                    string()
%%--------------------------------------------------------------------
handle_cast({connect_to_remote, #sipdst{proto=Proto, addr=Host, port=Port}=Dst, GenServerFrom}, #state{on=false}=State)
  when Proto == tcp; Proto == tcp6; Proto == tls; Proto == tls6, is_list(Host), is_integer(Port) ->

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
	    Local = get_local_ip_port(NewSocket, InetModule, Proto),
	    Remote = {Host, Port},

	    %% Call init() to get the connection properly initialized before we do our gen_server:reply()
	    case init([out, SocketModule, Proto, NewSocket, Local, Remote, Dst#sipdst.ssl_names]) of
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
%% Function: handle_cast({recv_sipmsg, Data}, State)
%%           Data = request record() | response record()
%% Descrip.: Our received process has received a SIP message on our
%%           socket, invoke sipserver:process() on it.
%% Returns : {noreply, State, Timeout}
%%--------------------------------------------------------------------
handle_cast({recv_sipmsg, Msg}, State) when State#state.on == true ->
    {IP, Port} = State#state.remote,
    SipSocket = #sipsocket{module	= sipsocket_tcp,
			   proto	= State#state.proto, pid=self(),
			   data		= {State#state.local, State#state.remote}
			  },
    Origin = #siporigin{proto		= State#state.proto,
			addr		= IP,
			port		= Port,
			receiver	= self(),
			sipsocket	= SipSocket
		       },
    sipserver:safe_spawn(sipserver, process, [Msg, Origin, transactionlayer]),
    {noreply, State, State#state.timeout};

%%--------------------------------------------------------------------
%% Function: handle_cast({close, From}, State)
%% Descrip.: A request to close this connection.
%% Returns : {stop, normal, NewState}
%%--------------------------------------------------------------------
handle_cast({close, FromPid}, State) ->
    %% XXX check that FromPid is someone sensible?
    Duration = util:timestamp() - State#state.starttime,
    {IP, Port} = State#state.remote,
    logger:log(debug, "TCP connection: Closing connection with ~p:~s:~p (duration: ~p seconds)",
	       [State#state.proto, IP, Port, Duration]),
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
    {stop, normal, State#state{socket=undefined}};

%%--------------------------------------------------------------------
%% Function: handle_cast({connection_closed, From}, State)
%% Descrip.: Our receiver signals us that it detected that the other
%%           end closed the connection.
%% Returns : {stop, normal, NewState}
%%--------------------------------------------------------------------
handle_cast({connection_closed, FromPid}, #state{receiver=FromPid}=State) when is_pid(FromPid) ->
    Duration = util:timestamp() - State#state.starttime,
    {IP, Port} = State#state.remote,
    logger:log(debug, "TCP connection: Connection with ~p:~s:~p closed by foreign host (duration: ~p seconds)",
	       [State#state.proto, IP, Port, Duration]),
    %% Call close() on the socket just to make sure. For SSL, the TCP receiver will have done this for us.
    case State#state.socketmodule of
	ssl -> true;
	SocketModule ->
	    SocketModule:close(State#state.socket)
    end,
    {stop, normal, State#state{socket=undefined}};

handle_cast(Msg, State) ->
    logger:log(error, "TCP connection: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State, State#state.timeout}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%% Descrip.: This connection has neither received nor sent any data in
%%           our configured maximum time period. Make the socket go
%%           away.
%% Returns : {stop, normal, NewState}          (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(timeout, State) when State#state.on == true ->
    {IP, Port} = State#state.remote,
    Timeout = State#state.timeout,
    logger:log(debug, "Sipsocket TCP: Connection with ~p:~s:~p timed out after ~p seconds, "
	       "connection handler terminating.", [State#state.proto, IP, Port, Timeout div 1000]),
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
    {stop, normal, State#state{socket=undefined}};

%%--------------------------------------------------------------------
%% Function: handle_info({also_notify, GenServerFrom}, State)
%%           GenServerFrom = term()
%% Descrip.: Do a gen_server:reply with our sipsocket to all processes
%%           that have queued with us instead of starting a parallell
%%           connection attempt to the same destination we have now
%%           connected to.
%% Returns : {noreply, State, ?TIMEOUT}          (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({also_notify, GenServerFrom}, State) when State#state.on == true ->
    gen_server:reply(GenServerFrom, {ok, State#state.sipsocket}),
    {noreply, State, State#state.timeout};

handle_info(Info, State) ->
    logger:log(debug, "TCP connection: Received unknown gen_server info :~n~p", [Info]),
    {noreply, State, State#state.timeout}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
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
%% Function: code_change(OldVsn, State, Extra)
%% Purpose : Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: start_tcp_receiver(SocketModule, Socket, SipSocket, Dir)
%%           SocketModule = atom(), gen_tcp | ssl
%%           Socket       = term(), the socket
%%           SipSocket    = sipsocket record()
%%           Direction    = atom(), in | out
%% Descrip.: Start a receiver that does blocking read on the socket.
%% Returns : {ok, Receiver} |
%%           does not return
%%--------------------------------------------------------------------
start_tcp_receiver(SocketModule, Socket, SipSocket, Dir) when is_atom(SocketModule), is_record(SipSocket, sipsocket),
							      Dir == in; Dir == out ->
    Proto = SipSocket#sipsocket.proto,
    {Local, Remote} = SipSocket#sipsocket.data,
    {IP, Port} = Remote,
    Receiver = tcp_receiver:start_link(SocketModule, Socket, Local, Remote, SipSocket),
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
    logger:log(debug, "TCP connection: ~s ~p:~s:~p (socket ~p, started receiver ~p)",
	       [S, Proto, IP, Port, Socket, Receiver]),
    {ok, Receiver}.

%%--------------------------------------------------------------------
%% Function: is_acceptable_socket(Socket, Dir, Proto, Remote, Names)
%%           Socket = term()
%%           Dir    = atom(), in | out
%%           Proto  = tls | tcp
%%           Remote = {IP, Port} tuple()
%%             IP   = string()
%%             Port = integer()
%%           Names  = list() of string(), list of names for the
%%                    certificate that the upper layer is willing to
%%                    accept
%% Descrip.: Check if a socket is 'acceptable'. For SSL, this means
%%           verify that the subjectAltName/CN is included in Names.
%% Returns : true
%%           false
%%--------------------------------------------------------------------
%%
%% SSL socket
%%
is_acceptable_socket(Socket, Dir, Proto, Remote, Names) when Proto == tls; Proto == tls6, is_list(Names) ->
    {IP, Port} = Remote,
    case get_ssl_peer_info(Socket, Proto, IP, Port) of
	{ok, Subject, AltNames} when is_record(Subject, ssl_conn_subject), is_list(AltNames) ->
	    DirStr = case Dir of
			 in -> "from";
			 out -> "to"
		     end,
	    logger:log(debug, "TCP connection: SSL connection ~s ~s:~p (~s)",
		       [DirStr, IP, Port, Subject#ssl_conn_subject.description]),

	    %% See if Local has an opinion about the validity of this socket...
	    case local:is_acceptable_socket(Socket, Dir, Proto, IP, Port, ?MODULE, Subject) of
		true ->
		    true;
		false ->
		    false;
		undefined ->
		    %% Do our own default checking of the subjectAltName/CN, if this is an outbound
		    %% connection. Per default, we accept any incoming TLS connections as valid,
		    %% although we don't trust those connections in any special way.
		    case Dir of
			in -> true;
			out ->
			    {ok, Reject} = yxa_config:get_env(ssl_check_subject_altname),
			    is_valid_ssl_certname(Names, Subject, AltNames, Reject)
		    end
	    end;
	{error, Reason} ->
	    {ok, RequireClientCert} = yxa_config:get_env(ssl_require_client_has_cert),
	    case {Dir, RequireClientCert} of
		{in, true} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information, "
			       "considering SSL connection from ~s:~p invalid : ~p", [IP, Port, Reason]),
		    false;
		{in, false} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information for connection "
			       "from ~s:~p : ~p (ignoring)", [IP, Port, Reason]),
		    true;
		{out, _RequireClientCert} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information, "
			       "considering SSL connection to ~s:~p invalid : ~p", [IP, Port, Reason]),
		    false
	    end
    end;
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
%% Function: get_ssl_peer_info(Socket, Proto, IP, Port)
%%           Socket = term()
%%           Proto  = tls | tls6
%%           IP     = string()
%%           Port   = integer()
%% Descrip.: Try to get the SSL peer certificate using a socket. If
%%           that fails, we check if it was a client that connected to
%%           us and if clients are required to present a certificate.
%%           Returns either {ok, Subject}, or true/false saying if the
%%           socket should be considered valid or not.
%% Returns : {ok, Subject, AltNames} |
%%           true                    |
%%           false
%%           Subject  = term(), ssl:peercert() subject data
%%           AltNames = list of string(), subjectAltName:s in cert
%%--------------------------------------------------------------------
get_ssl_peer_info(Socket, Proto, IP, Port) when is_atom(Proto), is_list(IP), is_integer(Port) ->
    %% ssl:peercert/3 needs to be wrapped in a try/catch - it fails badly on some certificates
    %% NOTE: the above should not be true anymore since we are not trying to get erlang-decoded
    %% certificates (option 'ssl') anymore, and the problem was with extensions or other
    %% attributes that was not recognized by the erlang-decoding code in the ssl application.
    %% We keep it for unforseen events though.
    PeerCertRes =
	try ssl:peercert(Socket, [pkix]) of
	    PCRes -> PCRes
	catch
	    error: E ->
		ST = erlang:get_stacktrace(),
		logger:log(error, "=ERROR REPORT==== from ssl:peercert/2 on certificate presented by ~p:~s:~p :~n"
			   "~p~nstacktrace : ~p", [Proto, IP, Port, E, ST]),
		{error, "Certificate decoding error"};
	    X: Y ->
		logger:log(error, "=ERROR REPORT==== from ssl:peercert/2 on certificate presented by ~p:~s:~p :~n"
			   "~p ~p", [Proto, IP, Port, X, Y]),
		{error, "Certificate decoding error"}
	end,
    get_ssl_peer_info2(Proto, IP, Port, PeerCertRes).

%% get_ssl_peer_info2/4 - part of get_ssl_peer_info/4, just to make things testable
get_ssl_peer_info2(Proto, IP, Port, PeerCertRes) ->
    case PeerCertRes of
	{ok, PKIXCert} ->
	    %% Now, we need to extract the Subject information, and the subjectAltNames
	    {ok, Subject} = get_ssl_peer_info_subject(PKIXCert),
	    {ok, AltNames} = get_ssl_peer_info_host_altnames(PKIXCert),
	    {ok, Subject, AltNames};
	{error, Reason} ->
	    logger:log(error, "TCP connection: Could not decode certificate presented by ~p:~s:~p : ~p",
		       [Proto, IP, Port, Reason]),
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: get_ssl_peer_info_subject(Cert)
%%           Cert = 'Certificate' record, SSL PKIX parsed
%% Descrip.: Extracts subject information from an SSL PKIX certificate
%% Returns : {ok, Subject} |
%%           error
%%           Subject = ssl_conn_subject record()
%%--------------------------------------------------------------------
get_ssl_peer_info_subject(Cert) when is_record(Cert, 'Certificate') ->
    Subject = (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject,
    case Subject of
	{rdnSequence, AttrList} when is_list(AttrList) ->
	    {ok, C} = ssl_subject_get(ssl_pkix_oid:atom2id(countryName), countryName, AttrList),
	    {ok, O} = ssl_subject_get(ssl_pkix_oid:atom2id(organizationName), organizationName, AttrList),
	    {ok, CN} = ssl_subject_get(ssl_pkix_oid:atom2id(commonName), commonName, AttrList),
	    Descr = lists:append(["C=", C, ", O=", O, ", CN=", CN]),
	    This = #ssl_conn_subject{countryName	= C,
				     organizationName	= O,
				     commonName		= http_util:to_lower(CN),
				     description	= Descr
				    },
	    {ok, This};
	_ ->
	    logger:log(debug, "TCP connection: Don't recognize the format of the SSL subject : ~p",
		       [Subject]),
	    error
    end.

%% ssl_subject_get/3 - extract values out of an PKIX SSL subject
%% Returns : {ok, Str}
ssl_subject_get(Key, countryName, [[#'AttributeTypeAndValue'{type = Key} = H] | T]) ->
    %% copy-pasted from ssl_pkix:transform/1
    {ok, ATAVEnc} = 'PKIX1Explicit88':encode('AttributeTypeAndValue', H),
    {ok, ATAVDec} = 'SSL-PKIX':decode('AttributeTypeAndValue', list_to_binary(ATAVEnc)),
    Value = ATAVDec#'AttributeTypeAndValue'.value,
    case is_list(Value) of
	true ->
	    {ok, Value};
	false ->
	    logger:log(debug, "TCP connection: Could not decode countryName ~p", [H]),
	    ssl_subject_get(Key, countryName, T)
    end;
ssl_subject_get(Key, Name, [[#'AttributeTypeAndValue'{type = Key} = H] | T]) ->
    %% copy-pasted from ssl_pkix:transform/1
    {ok, ATAVEnc} = 'PKIX1Explicit88':encode('AttributeTypeAndValue', H),
    {ok, ATAVDec} = 'SSL-PKIX':decode('AttributeTypeAndValue', list_to_binary(ATAVEnc)),
    case ATAVDec#'AttributeTypeAndValue'.value of
	{printableString, Value} ->
	    {ok, Value};
	_ ->
	    logger:log(debug, "TCP connection: Could not decode ~p ~p", [Name, H]),
	    ssl_subject_get(Key, Name, T)
    end;
ssl_subject_get(Key, Name, [_H | T]) ->
    ssl_subject_get(Key, Name, T);
ssl_subject_get(_Key, _Name, []) ->
    {ok, "\"\""}.


%%--------------------------------------------------------------------
%% Function: get_ssl_peer_info_host_altnames(Cert)
%%           Cert = 'Certificate' record, SSL PKIX parsed
%% Descrip.: Extracts subjectAltName's of type dNSName or iPAddress
%%           from an SSL PKIX certificate.
%% Returns : {ok, AltNames}
%%           AltNames = list() of string()
%%--------------------------------------------------------------------
get_ssl_peer_info_host_altnames(Cert) when is_record(Cert, 'Certificate') ->
    Extensions = (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.extensions,
    AltNameExtensions = get_tbs_extensions(ssl_pkix_oid:atom2id(subjectAltName), Extensions),
    {ok, DNS_altNames} = get_host_altnames('SubjectAltName', AltNameExtensions),
    {ok, DNS_altNames}.

%% get_tbs_extensions/2 - part of get_ssl_peer_info_host_altnames/1
%% Returns : list() of 'Extension' record
get_tbs_extensions(Key, Extensions) ->
    get_tbs_extensions(Key, Extensions, []).

get_tbs_extensions(Key, [#'Extension'{extnID = Key} = H | T], Res) ->
    get_tbs_extensions(Key, T, [H | Res]);
get_tbs_extensions(Key, [H | T], Res) when is_record(H, 'Extension') ->
    get_tbs_extensions(Key, T, Res);
get_tbs_extensions(_Key, [], Res) ->
    lists:reverse(Res).

%% get_dns_altnames(Type, Extensions) - part of get_ssl_peer_info_host_altnames/1
%% Returns : {ok, Strings}
get_host_altnames(Type, Extensions) ->
    get_host_altnames(Type, Extensions, []).

get_host_altnames(Type, [#'Extension'{extnValue = Value} | T], Res) ->
    {ok, Decoded} = 'PKIX1Implicit88':decode(Type, list_to_binary(Value)),
    %% Decoded is a list of tuples, for example :
    %%   [{rfc822Name, "ft@example.org"},
    %%    {dNSName,    "sip.example.org"}]
    This = lists:foldl(fun({dNSName, Name}, Acc) when is_list(Name) ->
			       [http_util:to_lower(Name) | Acc];
			  ({iPAddress, IP}, Acc) when is_list(IP) ->
			       IPstr = siphost:makeip(list_to_tuple(IP)),
			       [IPstr | Acc];
			  (_NonDNSname, Acc) ->
			       Acc
		       end, [], Decoded),
    case This of
	[] ->
	    %% we found no dNSName or iPAddress tuples in Decoded
	    get_host_altnames(Type, T, Res);
	_ ->
	    get_host_altnames(Type, T, Res ++ This)
    end;
get_host_altnames(_Type, [], Res) ->
    {ok, Res}.

%%--------------------------------------------------------------------
%% Function: is_valid_ssl_certname(ValidNames, Subject, AltNames,
%%                                 Reject)
%%           ValidNames = list() of string(), the hostname(s) we want
%%                        to make sure the subjectAltName matches.
%%           Subject    = ssl_conn_subject record()
%%           AltNames   = list() of string()
%%           Reject     = true | false, reject if name does not match
%%                        or just log?
%% Descrip.: Check if a socket is 'acceptable'. For SSL, this means
%%           verify that the subjectAltName/commonName is in the list
%%           of names we expect.
%% Returns : true | false
%%--------------------------------------------------------------------
is_valid_ssl_certname(ValidNames, Subject, AltNames, Reject) when is_list(ValidNames),
								  is_record(Subject, ssl_conn_subject),
								  is_list(AltNames) ->
    CommonName = Subject#ssl_conn_subject.commonName,
    {ok, Matches, MatchingName} = get_matching_altname(ValidNames, CommonName, AltNames),
    case {Matches, Reject} of
	{true, _Reject} ->
	    logger:log(debug, "Transport layer: Verified that subjectAltName/CN (~p) is in list of "
		       "valid names for this certificate (~p)", [MatchingName, ValidNames]),
	    true;
	{false, true} ->
	    logger:log(normal, "Transport layer: Rejecting SSL certificate with commonName ~p and "
		       "subjectAltName ~p since it does not match list of valid names for this connection : ~p",
		       [CommonName, AltNames, ValidNames]),
	    false;
	{false, false} ->
	    logger:log(debug, "Transport layer: Warning: SSL certificate with commonName ~p and "
		       "subjectAltName ~p does not match list of valid names for this connection : ~p",
		       [CommonName, AltNames, ValidNames]),
	    true
    end;
is_valid_ssl_certname(undefined, Subject, AltNames, true) when is_record(Subject, ssl_conn_subject),
							       is_list(AltNames) ->
    logger:log(debug, "Transport layer: Rejecting connection with SSL commonName ~p and subjectAltName ~p "
	       "since list of valid names is undefined", [Subject#ssl_conn_subject.commonName, AltNames]),
    false;
is_valid_ssl_certname(undefined, Subject, AltNames, false) when is_record(Subject, ssl_conn_subject),
								is_list(AltNames) ->
    logger:log(debug, "Transport layer: Warning: Not matching commonName ~p and subjectAltName ~p against "
	       "a list of valid names, since list is undefined",
	       [Subject#ssl_conn_subject.commonName, AltNames]),
    true.

%% part of is_valid_ssl_certname/4
%% Returns : {ok, true, MatchingName} |
%%           {ok, false, undefined}
get_matching_altname(ValidNames, CommonName, [H | T]) when is_list(ValidNames), is_list(CommonName) ->
    case util:casegrep(H, ValidNames) of
	true ->
	    {ok, true, H};
	false ->
	    get_matching_altname(ValidNames, CommonName, T)
    end;
get_matching_altname(ValidNames, CommonName, []) when is_list(ValidNames), is_list(CommonName) ->
    %% no matching subjectAltName, check commonName
    case util:casegrep(CommonName, ValidNames) of
	true ->
	    {ok, true, CommonName};
	false ->
	    {ok, false, undefined}
    end.

%%--------------------------------------------------------------------
%% Function: get_settings(Proto)
%%           Proto = tcp | tcp6 | tls | tls6
%% Descrip.: Get the variable things depending on protocol.
%% Returns : {ok, InetModule, SocketModule, Options}
%%           InetModule   = atom(), inet | ssl
%%           SocketModule = atom(), gen_tcp | ssl
%%           Options      = list(), socket options
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
%% Function: get_local_ip_port(Socket, InetModule, Proto)
%%           Socket     = term(), our new socket
%%           InetModule = inet | ssl
%%           Proto      = tcp | tcp6 | tls | tls6
%% Descrip.: Get the local address and port of a socket.
%% Returns : {IP, Port}
%%           IP   = string()
%%           Port = integer()
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
%% Function: get_defaultaddr(Proto)
%%           Proto = tcp | tcp6
%% Descrip.: Get the "any" address.
%% Returns : Addr = string()
%%--------------------------------------------------------------------
get_defaultaddr(tcp) -> "0.0.0.0";
get_defaultaddr(tcp6) -> "[::]".

%%--------------------------------------------------------------------
%% Function: connect_fail_handle_conn_queue(Proto, Host, Port, Reply)
%%           Proto = tcp | tcp6 | tls | tls6
%%           Host  = string(), IP address
%%           Port  = integer()
%%           Reply = term(), probably {error, Reason}
%% Descrip.: When we have failed connecting to a remote destination,
%%           we remove the entry saying that we are trying from the
%%           connection queue ets table, and then notify any other
%%           processes waiting for a connection to Proto:Host:Port
%%           about our failure.
%% Returns : ok
%%--------------------------------------------------------------------
connect_fail_handle_conn_queue(Proto, Host, Port, Reply) ->
    %% we are no longer trying to connect to this destination
    ets:delete(transportlayer_tcp_conn_queue, {Proto, Host, Port}),

    %% tell processes in queue about our failure
    check_also_notify(Reply),
    %% sleep, then check notify queue again to try and handle race here
    timer:sleep(100),
    check_also_notify(Reply),
    ok.

%%--------------------------------------------------------------------
%% Function: check_also_notify(Reply)
%%           Reply = term(), but probably {error, Reason}
%% Descrip.: Tell any processes that have queued with this one about
%%           the result of our connection attempt. If we have
%%           succeeded, this function will not be called (instead
%%           handle_info({also_notify, ) will deliver good news to
%%           processes in notify queue.
%% Returns : ok
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
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->
    autotest:mark(?LINE, "SSL certificates - 0"),

    %% To test our SSL certificate parsing and validation functions, we need a test certificate.
    %% The binary TestCert1 that we define below is a certificate with Subject information, and
    %% also a number of (Netscape) extensions that causes Erlangs built-in certificate-to-erlang-term
    %% parser to crash (because it does not recognize those old extensions). We want to be liberal
    %% in what we accept at the application layer (the SSL socket driver must have already accepted
    %% the certificate since it passes it to us), so we have our own SSL subject formatting and
    %% extension locating code.
    %%
    %% Certificate:
    %%     Data:
    %%         Version: 3 (0x2)
    %%         Serial Number: 1 (0x1)
    %%         Signature Algorithm: sha1WithRSAEncryption
    %%         Issuer: CN=Yxa test CA
    %%         Validity
    %%             Not Before: Sep 28 07:45:54 2005 GMT
    %%             Not After : Sep 28 07:45:54 2006 GMT
    %%         Subject: C=SE, O=Stockholms universitet, CN=yxa-test-cert1.example.org
    %%         Subject Public Key Info:
    %%             Public Key Algorithm: rsaEncryption
    %%             RSA Public Key: (1024 bit)
    %%                 Modulus (1024 bit):
    %%                     ...
    %%                 Exponent: 65537 (0x10001)
    %%         X509v3 extensions:
    %%             Netscape Cert Type:
    %%                 SSL Client, SSL Server
    %%             X509v3 Key Usage:
    %%                 Digital Signature, Non Repudiation, Key Encipherment
    %%             X509v3 Extended Key Usage:
    %%                 TLS Web Client Authentication, TLS Web Server Authentication
    %%             Netscape CA Revocation Url:
    %%                 http://ca.example.com/crl-v1.crl
    %%             X509v3 Subject Key Identifier:
    %%                 BC:E7:26:71:52:68:1E:C6:13:2D:F1:4C:6D:2A:B6:47:30:7D:7B:D3
    %%             X509v3 Authority Key Identifier:
    %%                 keyid:26:F7:64:BE:4A:5F:75:52:BC:CB:32:89:09:95:45:02:F4:A4:58:2A
    %%                 DirName:/CN=Yxa test CA
    %%                 serial:D7:E9:41:4A:33:5E:7C:E8
    %%
    %%             Authority Information Access:
    %%                 CA Issuers - URI:http://ca.example.com/ca.crt
    %%
    %%             X509v3 CRL Distribution Points:
    %%                 URI:http://ca.example.com/crl-v2.crl
    %%
    %%             X509v3 Certificate Policies:
    %%                 Policy: 1.1.1.1.1
    %%                   CPS: http://ca.example.com/CPS
    %%                   User Notice:
    %%                     Explicit Text: Limited Liability, see http://ca.example.com/CP
    %%
    %%             X509v3 Issuer Alternative Name:
    %%                 email:ca@example.com, URI:http://ca.example.com
    %%             X509v3 Subject Alternative Name:
    %%                 email:test@yxa-test-cert1.example.org,
    %%                 DNS:yxa-test-cert1.example.org,
    %%                 DNS:example.org,
    %%                 DNS:sip.example.org,
    %%                 DNS:localhost,
    %%                 IP Address:192.0.2.78,
    %%                 IP Address:127.0.0.1
    %%     Signature Algorithm: sha1WithRSAEncryption
    %%     ...
    TestCert1_der =
	<<48,130,4,18,48,130,3,188,160,3,2,1,2,2,1,1,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,48,22,49,20,48,18,6,
	 3,85,4,3,19,11,89,120,97,32,116,101,115,116,32,67,65,48,30,23,13,48,53,48,57,50,56,48,55,52,53,53,52,90,23,
	 13,48,54,48,57,50,56,48,55,52,53,53,52,90,48,83,49,11,48,9,6,3,85,4,6,19,2,83,69,49,31,48,29,6,3,85,4,10,19,
	 22,83,116,111,99,107,104,111,108,109,115,32,117,110,105,118,101,114,115,105,116,101,116,49,35,48,33,6,3,85,
	 4,3,19,26,121,120,97,45,116,101,115,116,45,99,101,114,116,49,46,101,120,97,109,112,108,101,46,111,114,103,
	 48,129,159,48,13,6,9,42,134,72,134,247,13,1,1,1,5,0,3,129,141,0,48,129,137,2,129,129,0,222,85,71,54,9,144,
	 139,255,251,249,67,39,37,170,201,90,2,11,146,126,240,112,98,246,86,127,56,0,210,231,29,84,6,170,148,158,162,
	 107,9,10,172,26,148,115,82,95,136,131,28,17,246,57,156,91,56,69,8,117,206,184,171,119,193,64,236,93,227,185,
	 38,73,239,235,190,95,91,113,28,127,146,172,48,204,214,119,20,189,72,207,119,159,134,192,155,102,220,235,116,
	 243,254,88,155,40,116,28,193,36,9,76,179,242,225,201,6,193,164,54,147,209,241,164,185,255,173,161,117,199,
	 234,125,2,3,1,0,1,163,130,2,114,48,130,2,110,48,17,6,9,96,134,72,1,134,248,66,1,1,4,4,3,2,6,192,48,11,6,3,
	 85,29,15,4,4,3,2,5,224,48,29,6,3,85,29,37,4,22,48,20,6,8,43,6,1,5,5,7,3,2,6,8,43,6,1,5,5,7,3,1,48,47,6,9,96,
	 134,72,1,134,248,66,1,4,4,34,22,32,104,116,116,112,58,47,47,99,97,46,101,120,97,109,112,108,101,46,99,111,
	 109,47,99,114,108,45,118,49,46,99,114,108,48,29,6,3,85,29,14,4,22,4,20,188,231,38,113,82,104,30,198,19,45,
	 241,76,109,42,182,71,48,125,123,211,48,70,6,3,85,29,35,4,63,48,61,128,20,38,247,100,190,74,95,117,82,188,203,
	 50,137,9,149,69,2,244,164,88,42,161,26,164,24,48,22,49,20,48,18,6,3,85,4,3,19,11,89,120,97,32,116,101,115,
	 116,32,67,65,130,9,0,215,233,65,74,51,94,124,232,48,56,6,8,43,6,1,5,5,7,1,1,4,44,48,42,48,40,6,8,43,6,1,5,
	 5,7,48,2,134,28,104,116,116,112,58,47,47,99,97,46,101,120,97,109,112,108,101,46,99,111,109,47,99,97,46,99,
	 114,116,48,49,6,3,85,29,31,4,42,48,40,48,38,160,36,160,34,134,32,104,116,116,112,58,47,47,99,97,46,101,120,
	 97,109,112,108,101,46,99,111,109,47,99,114,108,45,118,50,46,99,114,108,48,121,6,3,85,29,32,4,114,48,112,48,
	 110,6,4,41,1,1,1,48,102,48,37,6,8,43,6,1,5,5,7,2,1,22,25,104,116,116,112,58,47,47,99,97,46,101,120,97,109,
	 112,108,101,46,99,111,109,47,67,80,83,48,61,6,8,43,6,1,5,5,7,2,2,48,49,26,47,76,105,109,105,116,101,100,32,
	 76,105,97,98,105,108,105,116,121,44,32,115,101,101,32,104,116,116,112,58,47,47,99,97,46,101,120,97,109,112,
	 108,101,46,99,111,109,47,67,80,48,48,6,3,85,29,18,4,41,48,39,129,14,99,97,64,101,120,97,109,112,108,101,46,
	 99,111,109,134,21,104,116,116,112,58,47,47,99,97,46,101,120,97,109,112,108,101,46,99,111,109,48,123,6,3,85,
	 29,17,4,116,48,114,129,31,116,101,115,116,64,121,120,97,45,116,101,115,116,45,99,101,114,116,49,46,101,120,
	 97,109,112,108,101,46,111,114,103,130,26,121,120,97,45,116,101,115,116,45,99,101,114,116,49,46,101,120,97,
	 109,112,108,101,46,111,114,103,130,11,101,120,97,109,112,108,101,46,111,114,103,130,15,115,105,112,46,101,
	 120,97,109,112,108,101,46,111,114,103,130,9,108,111,99,97,108,104,111,115,116,135,4,192,0,2,78,135,4,127,0,
	 0,1,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,3,65,0,144,103,62,16,155,150,76,155,192,124,95,177,48,81,223,
	 18,241,73,68,30,183,225,22,14,172,193,251,254,99,104,222,249,240,41,28,33,81,4,155,105,29,165,75,214,231,150,
	 100,157,176,135,199,38,70,213,107,101,39,244,183,149,103,223,131,137>>,

    {ok, TestCert1} = ssl_pkix:decode_cert(TestCert1_der, [pkix]),


    %% test get_ssl_peer_info_subject(Cert)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ssl_peer_info_subject/1 - 1.0"),
    {ok, TestCert1_Subject} = get_ssl_peer_info_subject(TestCert1),

    autotest:mark(?LINE, "get_ssl_peer_info_subject/1 - 1.1"),
    %% verify results
    #ssl_conn_subject{countryName	= "SE",
		      organizationName	= "Stockholms universitet",
		      commonName	= "yxa-test-cert1.example.org",
		      description	= "C=SE, O=Stockholms universitet, CN=yxa-test-cert1.example.org"
		     } = TestCert1_Subject,


    %% test get_ssl_peer_info_host_altnames(Cert)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ssl_peer_info_host_altnames/1 - 1.0"),
    {ok, TestCert1_AltNames} = get_ssl_peer_info_host_altnames(TestCert1),

    autotest:mark(?LINE, "get_ssl_peer_info_host_altnames/1 - 1.1"),
    %% verify results
    ["127.0.0.1",
     "192.0.2.78",
     "localhost",
     "sip.example.org",
     "example.org",
     "yxa-test-cert1.example.org"] = TestCert1_AltNames,


    %% test get_ssl_peer_info(Socket, Proto, IP, Port)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ssl_peer_info2/4 - 1"),
    %% test error handling
    {error, "Certificate decoding error"} = get_ssl_peer_info(testing, tls6, "[2001:6b0:5:9876::6789]", 50000),


    %% test get_ssl_peer_info2(Proto, IP, Port, PeerCertRes)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ssl_peer_info2/4 - 1"),
    %% test normal case
    {ok, TestCert1_Subject, TestCert1_AltNames} =
	get_ssl_peer_info2(tls, "192.0.2.11", 50000, {ok, TestCert1}),

    autotest:mark(?LINE, "get_ssl_peer_info2/4 - 2"),
    %% test error handling
    {error, "Testing"} = get_ssl_peer_info2(tls6, "192.0.2.11", 50000, {error, "Testing"}),


    %% test is_valid_ssl_certname(ValidNames, Subject, AltNames, Reject)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 0"),
    IsValidSubject1 = #ssl_conn_subject{commonName = "commonname.example.org"},
    
    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 1"),
    %% test non-matching name #1 (reject: true)
    false = is_valid_ssl_certname(["example.org"], IsValidSubject1, [], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 2"),
    %% test non-matching name #2 (reject: true)
    false = is_valid_ssl_certname(["example.org"], IsValidSubject1, ["altname.testing"], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 3"),
    %% test non-matching name #1 (reject: false)
    true = is_valid_ssl_certname(["example.org"], IsValidSubject1, [], false),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 4"),
    %% test non-matching name #2 (reject: false)
    true = is_valid_ssl_certname(["example.org"], IsValidSubject1, ["altname.testing"], false),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 5"),
    %% test matching commonName
    true = is_valid_ssl_certname(["ComMonName.example.org"], IsValidSubject1, ["example.org", "192.0.2.12"], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 6"),
    %% test matching subjectAltName
    true = is_valid_ssl_certname(["example.org"], IsValidSubject1,
				 ["example.org", "commonName.example.org", "192.0.2.12"], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 7"),
    %% test without list of valid names (reject: true)
    false = is_valid_ssl_certname(undefined, IsValidSubject1, [], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 8"),
    %% test without list of valid names (reject: false)
    true = is_valid_ssl_certname(undefined, IsValidSubject1, [], false),

    ok.
