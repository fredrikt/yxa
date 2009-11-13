%%%-------------------------------------------------------------------
%%% File    : tcp_listener.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      tcp_listener first does SocketModule:listen() and then
%%%           sits around in a loop that does gen_tcp:accept() and
%%%           spawns one tcp_connection process for each socket.
%%% @since    15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
%%%-------------------------------------------------------------------

-module(tcp_listener).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export([start_link/3]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 start_listening/5
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
	  socketmodule,	%% atom(), gen_tcp | ssl
	  inetmodule,	%% atom(), inet | ssl
	  proto,	%% atom(), tcp | tcp6 | tls | tls6
	  port,		%% integer(), the port this tcp_listener instance listen on
	  socket,	%% term(), our listening socket
	  hostport	%% hp record(), listening socket IP/port
	 }).


%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%% The socket options we use for ordinary TCP sockets
-define(SOCKETOPTS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
%% The ssl module does not support reuseaddr
-define(SSL_SOCKETOPTS, [binary, {packet, 0}, {active, once}, {nodelay, true}]).

%% LINUX_IPPROTO_IPv6 = 41 is located in 
%% /usr/include/linux/in.h from linux-libc-dev
%% /usr/include/netinet/in.h from libc6-dev
-define(LINUX_IPPROTO_IPV6, 41).
%% and LINUX_IPV6_V6ONLY = 26 in
%% * /usr/include/linux/in6.h from linux-libc-dev
%% * /usr/include/bits/in.h from libc6-dev
-define(LINUX_IPV6_V6ONLY, 26).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (IPt, Proto, Port) ->
%%            {ok, Pid} |
%%            ignore
%%
%%            IPt   = {A,B,C,D} | {A,B,C,D,E,F,G,H}
%%            Proto = tcp | tcp6 | tls | tls6
%%            Port  = integer()
%%
%%            Pid = pid() "the process we spawn that does accept()."
%%
%% @doc     Starts the listener.
%% @end
%%--------------------------------------------------------------------
start_link(IPt, Proto, Port) when is_atom(Proto), is_integer(Port) ->
    {ok, InetModule, SocketModule, Options} = get_settings(Proto, IPt),
    case (Proto == tls) or (Proto == tls6) of
	true ->
	    case lists:keysearch(certfile, 1, Options) of
		{value, _} ->
		    Pid = proc_lib:spawn_link(?MODULE, start_listening, [Proto, Port, InetModule, SocketModule, Options]),
		    {ok, Pid};
		false ->
		    logger:log(normal, "NOT starting ~p listener on port ~p, no SSL server certificate specified "
			       "(see README)", [Proto, Port]),
		    ignore
	    end;
	false ->
	    Pid = proc_lib:spawn_link(?MODULE, start_listening, [Proto, Port, InetModule, SocketModule, Options]),
	    {ok, Pid}
    end.

%%--------------------------------------------------------------------
%% @spec    (Proto, Port, InetModule, SocketModule, Options) -> any()
%%
%%            Proto        = tcp | tcp6 | tls | tls6
%%            IP           = string()
%%            Port         = integer()
%%            InetModule   = inet | ssl
%%            SocketModule = gen_tcp | ssl
%%            Options      = term() "socket options"
%%
%% @doc     Starts listening on a port, then enters accept_loop.
%%          NOTE : Does not return.
%% @end
%%--------------------------------------------------------------------
start_listening(Proto, Port, InetModule, SocketModule, Options)
  when is_atom(Proto), is_integer(Port), is_atom(InetModule), is_atom(SocketModule), is_list(Options) ->
    case whereis(tcp_dispatcher) of
	TDisp when is_pid(TDisp) ->
	    true = link(TDisp);
	_ ->
	    logger:log(error, "TCP listener: tcp_dispatcher process not found, can't start listening!"),
	    erlang:error("failed linking to tcp_dispatcher")
    end,
    TCPsocket = case SocketModule:listen(Port, Options) of
		    {ok, S} ->
			S;
		    {error, E1} ->
			logger:log(error, "TCP listener: Could not open socket - module ~p, proto ~p, port ~p : ~p (~s)",
				   [SocketModule, Proto, Port, E1, InetModule:format_error(E1)]),
			logger:log(debug, "TCP socketopts : ~p", [Options]),
			erlang:error({"Could not open socket", {error, E1}},
				    [Proto, Port, InetModule, SocketModule, Options])
		end,
    HP =
	case catch InetModule:sockname(TCPsocket) of
	    {ok, {IPlist, LocalPort}} ->
		#hp{l_ip   = siphost:makeip(IPlist),
		    l_port = LocalPort
		   };
	    {error, E2} ->
		logger:log(error, "TCP listener: ~p:sockname() returned error ~p", [InetModule, E2]),
		#hp{l_ip   = get_defaultaddr(Proto),
		    l_port = Port
		   }
	end,
    State = #state{socketmodule = SocketModule,
		   inetmodule   = InetModule,
		   proto        = Proto,
		   port         = Port,
		   socket       = TCPsocket,
		   hostport     = HP
		  },
    %% Now register with the tcp_dispatcher
    SipSocket = #sipsocket{module	= sipsocket_tcp,
			   proto	= Proto,
			   pid		= self(),
			   hostport	= HP
			  },
    ok = gen_server:call(tcp_dispatcher, {register_sipsocket, listener, SipSocket}),
    sipsocket:add_listener_info(Proto, HP#hp.l_ip, HP#hp.l_port),
    accept_loop_start(State).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (State) -> any()
%%
%%            State = #state{}
%%
%% @doc     Log a small message before starting the real accept_loop.
%%          NOTE : Does not return.
%% @end
%%--------------------------------------------------------------------
accept_loop_start(State) when is_record(State, state) ->
    #hp{l_ip   = IP,
	l_port = Port
       } = State#state.hostport,
    Desc = case State#state.proto of
	       tcp -> "TCP";
	       tcp6 -> "TCP";
	       tls -> "TLS";
	       tls6 -> "TLS"
	   end,
    logger:log(debug, "Listening on ~s ~s:~p (socket ~p)", [Desc, IP, Port, State#state.socket]),
    accept_loop(State).

%%--------------------------------------------------------------------
%% @spec    (State) -> any()
%%
%%            State = #state{}
%%
%% @doc     Waits in accept() until someone connects to the port we
%%          are listening on. When someone does, spawn a
%%          tcp_connection process that handles this connection and
%%          then loop back to ourselves and do accept() again.
%%          NOTE : does not return.
%% @end
%%--------------------------------------------------------------------
accept_loop(State) when is_record(State, state) ->
    SocketModule = State#state.socketmodule,
    InetModule = State#state.inetmodule,
    ListenSocket = State#state.socket,
    %% XXX for SSL sockets, it might be necessary to do accept with a timeout (ssl:accept/2),
    %% to avoid defunct connections from remaining in state CLOSE_WAIT forever (until the
    %% Erlang VM shuts down), according to mail to erlang-questions 2008-01-30 (Gaspar C).
    case my_accept (SocketModule, ListenSocket) of
	{ok, NewSocket} ->
	    case InetModule:peername(NewSocket) of
		{ok, {IPlist, InPortNo}} ->
		    IP = siphost:makeip(IPlist),
		    HP =
			(State#state.hostport)#hp{r_ip = IP,
						  r_port = InPortNo
						 },
		    Proto = State#state.proto,
		    start_tcp_connection(SocketModule, Proto, NewSocket, HP),
		    accept_loop(State);
		{error, E} ->
		    logger:log(error, "TCP listener: Could not get peername after accept() : ~p", [E]),
		    erlang:error({"Could not get peername after accept", {error, E}}, [State])
	    end;
	{error, closed} ->
	    logger:log(error, "TCP listener: accept() says the listensocket ~p was closed, "
		       "no point in me staying alive.", [ListenSocket]),
	    erlang:error("Listening socket closed", [State]);
	{error, esslaccept} when SocketModule == ssl ->
	    %% Don't log SSL error esslaccept with priority 'error' since it is
	    %% what we get on portscans and similar
	    logger:log(debug, "TCP listener: SSL handshake with unidentified peer failed "
		       "(just ignore, usually a portscan or similar)"),
	    accept_loop(State);
	{error, E} when SocketModule == ssl ->
	    logger:log(normal, "TCP listener: SSL accept returned error : (~p) ~s", [E, ssl:format_error(E)]),
	    %% We don't terminate on accept error for SSL sockets, since the error can be that the client
	    %% did not present a client certificate signed by someone we trust, or any other kind of SSL
	    %% error. Terminating the listener on those errors makes it trivial to DoS the whole YXA application
	    %% by just connecting to the SSL port a couple of times.
	    accept_loop(State);
	{error, E} ->
	    logger:log(error, "TCP listener: ~p accept returned error : (~p) ~s", [SocketModule, E, inet:format_error(E)]),
	    %% accept failed for non-SSL socket. This is probably more serious, so we terminate the listener.
	    %% It will be restarted by the transport layer supervisor according to the restart strategy.
            erlang:error({"Accept failed", {error, E}}, [State]);
	Unknown ->
	    %% To keep Dialyzer happy (otherwise complains about this function having no local return)
	    logger:log(error, "TCP listener: ~p accept returned unknown data", [SocketModule]),
	    logger:log(debug, "TCP listener: data returned by ~p accept : ~p", [SocketModule, Unknown]),
	    {error, accept_returned_unknown_data}
    end.

my_accept(gen_tcp, Socket) ->
    gen_tcp:accept(Socket);
my_accept(ssl, Socket) ->
    try ssl:transport_accept(Socket) of
	{ok, NewSocket} ->
	    case ssl:ssl_accept(NewSocket) of
		ok ->
		    {ok, NewSocket};
		E ->
		    E
	    end;
	E ->
	    E
    catch
	  error: undef ->
	    %% try old SSL accept interface
	    ssl:accept(Socket)
    end.

%%--------------------------------------------------------------------
%% @spec    (SocketModule, Proto, Socket, HP) -> ok
%%
%%            SocketModule = gen_tcp | ssl "the name of the sipsocket module this socket uses"
%%            Proto        = tcp | tcp6 | tls | tls6
%%            Socket       = term()
%%            HP           = #hp{} "local/remote IP/port info"
%%
%% @doc     Someone has just connected to our listening socket,
%%          resulting in the connection socket Socket. Start a
%%          tcp_connection process to handle this Socket, and in case
%%          it is an SSL socket, also set controlling process to the
%%          tcp_receiver process that the tcp_connection has started
%%          instead of us (the listening process, which is the
%%          default controlling process). This must be done from here
%%          since the SSL socket handler only allows the current
%%          controlling process to change who is it's controlling
%%          process.
%% @end
%%--------------------------------------------------------------------
%%
%% SSL socket
%%
start_tcp_connection(ssl, Proto, Socket, HP) when is_atom(Proto), is_record(HP, hp) ->
    {ok, {Protocol, Cipher}} = ssl:connection_info(Socket),
    logger:log(debug, "Extra debug: TCP listener : SSL socket info for ~p : "
	       "Protocol = ~p, Cipher = ~p", [Socket, Protocol, Cipher]),
    case tcp_connection:connection_from(ssl, Proto, Socket, HP) of
	{ok, ConnPid} ->
	    {ok, RecvPid} = gen_server:call(ConnPid, {get_receiver}),
	    case ssl:controlling_process(Socket, RecvPid) of
		ok -> ok;
		{error, Reason} ->
		    logger:log(error, "TCP listener: Could not change controlling process of "
			       "SSL socket ~p to ~p : ~p", [Socket, RecvPid, Reason]),
		    erlang:error({"Failed changing controlling process for SSL socket", {error, Reason}},
				 [ssl, Proto, Socket, HP])
	    end;
	{error, Reason} ->
	    logger:log(debug, "TCP listener: Failed starting a tcp_connection handler for socket ~p : ~p",
		       [Socket, Reason]),
	    ok;
	ignore ->
	    %% SSL socket was not acceptable. This is already logged and everything, so just return.
	    ok
    end;
%%
%% Non-SSL socket
%%
start_tcp_connection(SocketModule, Proto, Socket, HP) when is_atom(SocketModule), is_atom(Proto),
							   is_record(HP, hp) ->
    case tcp_connection:connection_from(SocketModule, Proto, Socket, HP) of
	{ok, _ConnPid} ->
	    ok;
	{error, Reason} ->
	    logger:log(debug, "TCP listener: Failed starting a tcp_connection handler for socket ~p : ~p",
		       [Socket, Reason]),
	    ok
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
%% @spec    (Proto, IPtuple) ->
%%            {ok, InetModule, SocketModule, Options}
%%
%%            IPtuple = {A,B,C,D} | {A,B,C,D,E,F,G,H}
%%            Proto   = tcp | tcp6 | tls | tls6
%%
%%            InetModule   = inet | ssl
%%            SocketModule = gen_tcp | ssl
%%            Options      = term() "socket options"
%%
%% @doc     Get the variable things depending on protocol.
%% @end
%%--------------------------------------------------------------------
get_settings(tcp, IPtuple) ->
    This = [inet, {ip, IPtuple}],
    {ok, inet, gen_tcp, This ++ ?SOCKETOPTS};
get_settings(tcp6, IPtuple) ->
    This0 = [inet6, {ip, IPtuple}],
    This = case os:type() of
	       {unix,linux} ->
		   %% set socket options to request an IPv6 socket only on Linux
		   LinuxIPv6only = {raw,  ?LINUX_IPPROTO_IPV6, ?LINUX_IPV6_V6ONLY, <<1:32/native>>},
		   [LinuxIPv6only | This0];
	       _ ->
		   This0
	   end,
    {ok, inet, gen_tcp, This ++ ?SOCKETOPTS};
get_settings(tls, IPtuple) ->
    L = get_settings_tls(),
    This = [{ip, IPtuple} | L],
    {ok, ssl, ssl, This ++ ?SSL_SOCKETOPTS};
get_settings(tls6, IPtuple) ->
    L = get_settings_tls(),
    This = [inet6, {ip, IPtuple}] ++ L,
    {ok, ssl, ssl, This ++ ?SSL_SOCKETOPTS}.

get_settings_tls() ->
    {ok, L1} = yxa_config:get_env(ssl_server_ssloptions),
    case yxa_config:get_env(ssl_server_certfile) of
	{ok, File} ->
	    [{certfile, File} | L1];
	none ->
	    L1
    end.
