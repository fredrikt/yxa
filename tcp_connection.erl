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
%% Function: start_link/6
%% Descrip.: Starts the server
%% Returns : void()
%%--------------------------------------------------------------------
start_link(in, SocketModule, Proto, Socket, Local, Remote) ->
    gen_server:start_link(?MODULE, [in, SocketModule, Proto, Socket, Local, Remote, undefined], []).

start_link(connect, Dst, GenServerFrom) when is_record(Dst, sipdst) ->
    gen_server:start(?MODULE, [connect, Dst, GenServerFrom], []).

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
%% Returns : {ok, State}
%%--------------------------------------------------------------------
init([connect, Dst, GenServerFrom]) when is_record(Dst, sipdst) ->
    %% To not block tcp_dispatcher (who starts these tcp_connections), we
    %% send ourself a cast and return immediately.
    gen_server:cast(self(), {connect_to_remote, Dst, GenServerFrom}),
    {ok, #state{}};

%%--------------------------------------------------------------------
%% Function: init([Direction, SocketModule, Proto, Socket, Local,
%%                Remote, SSLHost])
%%           Direction    = in | out
%%           SocketModule = atom(), the name of the socket module
%%                          this socket uses (gen_tcp | ssl)
%%           Proto        = atom(), tcp | tcp6 | tls | tls6
%%           Socket       = term()
%%           Local        = term() ({Host, Port} tuple())
%%           Remote       = term() ({Host, Port} tuple())
%%           SSLHost      = string() | undefined, hostname to verify
%%                          a SSL certificate against
%% Descrip.: Initialize this tcp_connection process to handle Socket.
%% Returns : {ok, State, Timeout} |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([Direction, SocketModule, Proto, Socket, Local, Remote, SSLHost]) ->
    %% First check if socket is acceptable
    {IP, Port} = Remote,
    case is_acceptable_socket(Socket, Direction, Proto, SSLHost, Port) of
	true ->
	    init2([Direction, SocketModule, Proto, Socket, Local, Remote]);
	false ->
	    logger:log(normal, "TCP connection: Closing unacceptable connection to ~p:~s:~p",
		       [Proto, IP, Port]),
	    SocketModule:close(Socket),
	    case Direction of
		in -> ignore;
		out -> {stop, unacceptable}
	    end
    end.

init2([Direction, SocketModule, Proto, Socket, Local, Remote]) ->
    SipSocket = #sipsocket{module=sipsocket_tcp, proto=Proto, pid=self(), data={Local, Remote}},
    %% Register this connection with the TCP listener process before we proceed
    case gen_server:call(tcp_dispatcher, {register_sipsocket, Direction, SipSocket}) of
	ok ->
	    {ok, Receiver} = start_tcp_receiver(SocketModule, Socket, SipSocket, Direction),
	    Timeout = sipserver:get_env(tcp_connection_idle_timeout, 300) * 1000,
	    Now = util:timestamp(),
	    State = #state{socketmodule=SocketModule, proto=Proto, socket=Socket, receiver=Receiver,
			   local=Local, remote=Remote, starttime=Now, sipsocket=SipSocket, timeout=Timeout,
			   on=true},
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
    {reply, {error, "Not started"}, State, State#state.timeout}.


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
    ConnectTimeout = sipserver:get_env(tcp_connect_timeout, 20) * 1000,
    {ok, InetModule, SocketModule, Options} = get_settings(Proto),
    Host2 = util:remove_v6_brackets(Host),

    %% Measure how long time it takes to connect to remote hosts
    {TimeSpent, ConnectRes} = timer:tc(SocketModule, connect, [Host2, Port, Options, ConnectTimeout]),
    logger:log(debug, "TCP connection: Extra debug: Time spent connecting to ~s : ~p ms",
	       [DstStr, TimeSpent div 1000]),

    case ConnectRes of
	{ok, NewSocket} ->
	    Local = get_local_ip_port(NewSocket, InetModule, Proto),
	    Remote = {Host, Port},

	    %% Call init() to get the connection properly initialized before we do our gen_server:reply()
	    case init([out, SocketModule, Proto, NewSocket, Local, Remote, Dst#sipdst.ssl_hostname]) of
		{ok, NewState, Timeout} ->
		    SipSocket = NewState#state.sipsocket,
		    logger:log(debug, "TCP connection: Extra debug: Connected to ~s, socket ~p",
			       [DstStr, SipSocket]),

		    %% This is the answer to a 'gen_server:call(tcp_dispatcher, {get_socket ...)' that
		    %% someone (GenServerFrom) made, but where there were no cached connection available.
		    gen_server:reply(GenServerFrom, {ok, SipSocket}),

		    {noreply, NewState, Timeout};
		{stop, Reason} ->
		    gen_server:reply(GenServerFrom, {error, Reason}),
		    {stop, normal, State}
	    end;
	{error, econnrefused} ->
	    gen_server:reply(GenServerFrom, {error, "Connection refused"}),
	    {stop, normal, State};
	{error, ehostunreach} ->
	    %% If we don't get a connection before ConnectTimeout, we end up here
	    gen_server:reply(GenServerFrom, {error, "Host unreachable"}),
	    {stop, normal, State};
	{error, E} ->
	    logger:log(error, "TCP connection: Failed connecting to ~s : ~p (~s)",
		       [DstStr, E, inet:format_error(E)]),
	    gen_server:reply(GenServerFrom, {error, "Failed connecting to remote host"}),
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
    SipSocket = #sipsocket{module=sipsocket_tcp, proto=State#state.proto, pid=self(),
			   data={State#state.local, State#state.remote}},
    Origin = #siporigin{proto=State#state.proto, addr=IP, port=Port, receiver=self(),
			sipsocket=SipSocket},
    sipserver:safe_spawn(sipserver, process, [Msg, Origin, transaction_layer]),
    {noreply, State, State#state.timeout};

%%--------------------------------------------------------------------
%% Function: handle_cast({close, From}, State)
%% Descrip.: A request to close this connection.
%% Returns : {stop, normal, NewState}
%%--------------------------------------------------------------------
handle_cast({close, _From}, State) ->
    %% XXX check that From is someone sensible?
    Duration = util:timestamp() - State#state.starttime,
    {IP, Port} = State#state.remote,
    logger:log(debug, "TCP connection: Closing connection with ~p:~s:~p (duration: ~p seconds)",
	       [State#state.proto, IP, Port, Duration]),
    SocketModule = State#state.socketmodule,
    SocketModule:close(State#state.socket),
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
    {noreply, State}.


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
%% Returns : {noreply, State, ?TIMEOUT}          (terminate/2 is called)
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

handle_info(Info, State) ->
    logger:log(debug, "TCP connection: Received unknown gen_server info :~n~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    logger:log(error, "TCP connection: Terminating for some other reason than 'normal' : ~n~p",
	       [Reason]),
    ok.

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
%% Function: is_acceptable_socket(Socket, Dir, Proto, Host, Port)
%%           Socket = term()
%%           Dir    = atom(), in | out
%%           Proto  = tls | tcp
%%           Host   = string(), the hostname the upper layer wanted to
%%                    connect to.
%%           Port   = integer()
%% Descrip.: Check if a socket is 'acceptable'. For SSL, this means
%%           verify that the subjectAltName matches the hostname.
%% Returns : true
%%           false
%%--------------------------------------------------------------------
%%
%% SSL socket
%%
is_acceptable_socket(Socket, Dir, Proto, Host, Port) when Proto == tls; Proto == tls6 ->
    case get_ssl_peercert(Socket, Dir, Host, Port) of
	{ok, Subject} ->

	    {ok, Ident} = get_ssl_identification(Subject),
	    DirStr = case Dir of
			 in -> "from";
			 out -> "to"
		     end,
	    logger:log(debug, "TCP connection: SSL connection ~s ~s:~p (~s)", [DirStr, Host, Port, Ident]),

	    %% See if Local has an opinion about the validity of this socket...
	    case local:is_acceptable_socket(Socket, Dir, Proto, Host, Port, ?MODULE, Subject) of
		true ->
		    true;
		false ->
		    false;
		undefined ->
		    %% Do our own default checking of the subjectAltName, if this is an outbound
		    %% connection. Per default, we accept any incoming TLS connetions as valid,
		    %% although we don't trust those connections in any special way.
		    case Dir of
			in -> true;
			out ->
			    Reject = sipserver:get_env(ssl_check_subject_altname, true),
			    is_valid_ssl_altname(Host, Subject, Reject)
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
is_acceptable_socket(Socket, Dir, Proto, Host, Port) ->
    case local:is_acceptable_socket(Socket, Dir, Proto, Host, Port, ?MODULE, undefined) of
	true ->
	    true;
	false ->
	    false;
	undefined ->
	    %% Default accept
	    true
    end.

%%--------------------------------------------------------------------
%% Function: get_ssl_peercert(Socket, Dir, Proto, Host, Port)
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
get_ssl_peercert(Socket, Dir, Host, Port) ->
    case ssl:peercert(Socket, [ssl, subject]) of
	{ok, Subject} ->
	    {ok, Subject};
	{error, Reason} ->
	    RequireClientCert = sipserver:get_env(ssl_require_client_has_cert, false),
	    case {Dir, RequireClientCert} of
		{in, true} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information, "
			       "considering SSL connection from ~s:~p invalid : ~p", [Host, Port, Reason]),
		    false;
		{in, false} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information for connection from ~s:~p",
			       [Host, Port]),
		    true;
		{out, _RequireClientCert} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information, "
			       "considering SSL connection to ~s:~p invalid : ~p", [Host, Port, Reason]),
		    false
	    end
    end.

%%--------------------------------------------------------------------
%% Function: is_valid_ssl_altname(Host, Dir, Reject)
%%           Host    = string() | undefined, the hostname we want to
%%                     make sure the subjectAltName matches.
%%           Subject = term(), SSL subject data
%%           Reject  = true | false, reject if name does not match or
%%                     just log?
%% Descrip.: Check if a socket is 'acceptable'. For SSL, this means
%%           verify that the subjectAltName matches the hostname.
%% Returns : true | false
%%--------------------------------------------------------------------
is_valid_ssl_altname(undefined, _Subject, _Reject) ->
    true;
is_valid_ssl_altname(Host, Subject, Reject) when Reject == true; Reject == false ->
    {ok, AltName} = get_ssl_socket_altname(Subject),
    case {(AltName == Host), Reject} of
	{true, _Reject} ->
	    logger:log(debug, "TCP connection: Verified that subjectAltName (~p) matches hostname ~p",
		       [AltName, Host]),
	    true;
	{false, true} ->
	    logger:log(normal, "Transport layer: rejecting SSL socket since subjectAltName (~p) "
		       "does not match hostname (~p)", [AltName, Host]),
	    false;
	{false, false} ->
	    logger:log(debug, "Transport layer: Warning: subjectAltName (~p) does not match hostname (~p)",
		       [AltName, Host]),
	    true
    end.

get_ssl_socket_altname({rndSequence, AttrList}) ->
    {ok, {printableString, CN}} = ssl_subject_get(commonName, AttrList),
    {ok, CN}.

get_ssl_identification({rndSequence, AttrList}) ->
    {ok, C} = ssl_subject_get(countryName, AttrList),
    {ok, {printableString, O}} = ssl_subject_get(organizationName, AttrList),
    {ok, {printableString, CN}} = ssl_subject_get(commonName, AttrList),
    {ok, lists:append(["C=", C, ", O=", O, ", CN=", CN])}.

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
    {ok, inet, gen_tcp, get_socketopts(tcp)};
get_settings(tcp6) ->
    {ok, inet, gen_tcp, [inet6 | get_socketopts(tcp)]};
get_settings(tls) ->
    {ok, ssl, ssl, get_socketopts(tls)};
get_settings(tls6) ->
    {ok, ssl, ssl, [inet6 | get_socketopts(tls)]}.

%% part of get_settings/1. Get default socket options plus any configured ones for SSL.
get_socketopts(tcp) ->
    ?SOCKETOPTS;
get_socketopts(tls) ->
    L = sipserver:get_env(ssl_client_ssloptions, []),
    ?SOCKETOPTS ++ L.

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
