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
	 start_link/6
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
    case get_ssl_peercert(Socket, Dir, Remote) of
	{ok, Subject} ->

	    {ok, Ident} = get_ssl_identification(Subject),
	    DirStr = case Dir of
			 in -> "from";
			 out -> "to"
		     end,
	    logger:log(debug, "TCP connection: SSL connection ~s ~s:~p (~s)", [DirStr, IP, Port, Ident]),

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
			    is_valid_ssl_certname(Names, Subject, Reject)
		    end
	    end;
	true ->
	    true;
	false ->
	    false
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
%% Function: get_ssl_peercert(Socket, Dir, {IP, Port})
%%           Socket = term()
%%           Dir    = atom(), in | out
%%           Host   = string(), the hostname the upper layer wanted to
%%                    connect to.
%%           Port   = integer()
%% Descrip.: Try to get the SSL peer certificate using a socket. If
%%           that fails, we check if it was a client that connected to
%%           us and if clients are required to present a certificate.
%%           Returns either {ok, Subject}, or true/false saying if the
%%           socket should be considered valid or not.
%% Returns : {ok, Subject} |
%%           true          |
%%           false
%%           Subject = term(), ssl:peercert() subject data
%%--------------------------------------------------------------------
get_ssl_peercert(Socket, Dir, {IP, Port}) when is_atom(Dir), is_list(IP), is_integer(Port) ->
    %% ssl:peercert/3 needs to be wrapped in a try/catch - it fails badly on some certificates
    PeerCertRes =
	try ssl:peercert(Socket, [ssl, subject]) of
	    PCRes -> PCRes
	catch
	    error: E ->
		ST = erlang:get_stacktrace(),
		logger:log(error, "=ERROR REPORT==== from ssl:peercert/2 on certificate from ~s:~p :~n"
			   "~p~nstacktrace : ~p", [IP, Port, E, ST]),
		{error, "Certificate decoding error"};
	      X: Y ->
		logger:log(error, "=ERROR REPORT==== from ssl:peercert/2 on certificate from ~s:~p :~n"
			   "~p ~p", [IP, Port, X, Y]),
		{error, "Certificate decoding error"}
	end,
    case PeerCertRes of
	{ok, Subject} ->
	    {ok, Subject};
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
    end.

%%--------------------------------------------------------------------
%% Function: is_valid_ssl_certname(ValidNames, Dir, Reject)
%%           ValidNames = list() of string(), the hostname(s) we want
%%                        to make sure the subjectAltName matches.
%%           Subject    = term(), SSL subject data
%%           Reject     = true | false, reject if name does not match
%%                        or just log?
%% Descrip.: Check if a socket is 'acceptable'. For SSL, this means
%%           verify that the subjectAltName/commonName is in the list
%%           of names we expect.
%% Returns : true | false
%%--------------------------------------------------------------------
is_valid_ssl_certname(ValidNames, Subject, Reject) when is_list(ValidNames), Reject == true; Reject == false ->
    {ok, AltName} = get_ssl_socket_altname(Subject),
    Matches = util:casegrep(AltName, ValidNames),
    case {Matches, Reject} of
	{true, _Reject} ->
	    logger:log(debug, "Transport layer: Verified that subjectAltName/CN (~p) is in list of "
		       "valid names for this certificate (~p)", [AltName, ValidNames]),
	    true;
	{false, true} ->
	    logger:log(normal, "Transport layer: Rejecting SSL socket since subjectAltName/CN (~p) "
		       "is not in list of valid names for this certificate (~p)", [AltName, ValidNames]),
	    false;
	{false, false} ->
	    logger:log(debug, "Transport layer: Warning: subjectAltName/CN (~p) is not in list of "
		       "valid names for this certificate (~p)",
		       [AltName, ValidNames]),
	    true
    end;
is_valid_ssl_certname(undefined, Subject, true) ->
    {ok, AltName} = get_ssl_socket_altname(Subject),
    logger:log(debug, "Transport layer: Rejecting connection with subjectAltName/CN ~p since list of valid "
	       "names is undefined", [AltName]),
    false;
is_valid_ssl_certname(undefined, Subject, false) ->
    {ok, AltName} = get_ssl_socket_altname(Subject),
    logger:log(debug, "Transport layer: Warning: subjectAltName/CN ~p not matched against list of"
	       "valid names, since list is undefined", [AltName]),
    true.

get_ssl_socket_altname({rndSequence, AttrList}) ->
    %% XXX commonName is not always set to the FQDN. Find a better (more correct)
    %% way to get the subjectAltName
    {ok, {printableString, CN}} = ssl_subject_get(commonName, AttrList),
    {ok, CN}.

get_ssl_identification({rndSequence, AttrList}) ->
    {ok, C} = ssl_subject_get(countryName, AttrList),
    {ok, {printableString, O}} = ssl_subject_get(organizationName, AttrList),
    {ok, {printableString, CN}} = ssl_subject_get(commonName, AttrList),
    {ok, lists:append(["C=", C, ", O=", O, ", CN=", CN])}.

%% XXX 'AttributeTypeAndValue' is really a record defined in ssl_pkix.hrl, but I
%% haven't found a way to include that file.
ssl_subject_get(Key, [[{'AttributeTypeAndValue', Key, Value}] | _T]) ->
    {ok, Value};
ssl_subject_get(Key, [_H | T]) ->
    ssl_subject_get(Key, T);
ssl_subject_get(_Key, []) ->
    none.

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
