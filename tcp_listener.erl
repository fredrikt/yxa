%%%-------------------------------------------------------------------
%%% File    : tcp_listener.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: tcp_listener first does SocketModule:listen() and then
%%%           sits around in a loop that does gen_tcp:accept() and
%%%           spawns one tcp_connection process for each socket.
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------

-module(tcp_listener).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export([start/2]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("sipsocket.hrl").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
	  socketmodule,
	  inetmodule,
	  proto,
	  port,
	  socket,
	  local
	 }).


%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%% The socket options we use for ordinary TCP sockets
-define(SOCKETOPTS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
%% The ssl module does not support reuseaddr
-define(SSL_SOCKETOPTS, [binary, {packet, 0}, {active, once}, {nodelay, true}]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start(Proto, Port)
%%           Proto = atom(), tcp | tcp6 | tls | tls6
%%           Port  = integer()
%% Descrip.: Starts the listener.
%% Returns : {ok, Local, Pid} |
%%           not_started
%%           Local = {IP, Port} tuple()
%%           Pid   = pid(), we spawn a process that does accept() on
%%                   this socket. Pid is that processes pid.
%%--------------------------------------------------------------------
start(Proto, Port) when atom(Proto), integer(Port) ->
    {InetModule, SocketModule, Options} = case Proto of
					      tcp ->
						  {inet, gen_tcp, lists:append(?SOCKETOPTS, [inet])};
					      tcp6 ->
						  {inet, gen_tcp, lists:append(?SOCKETOPTS, [inet6])};
					      tls ->
						  {ssl, ssl, ?SSL_SOCKETOPTS};
					      tls6 ->
						  {ssl, ssl, lists:append(?SSL_SOCKETOPTS, [inet6])}
					  end,
    case Proto of
	_ when Proto == tls; Proto == tls6 ->
	    case sipserver:get_env(ssl_server_certfile, none) of
		none ->
		    logger:log(normal, "NOT starting ~p listener, no SSL server certificate specified (see README)", [Proto]),
		    not_started;
		ClientCert ->
		    NewOptions = lists:append(Options, [{certfile, ClientCert}]),
		    start2(Proto, Port, InetModule, SocketModule, NewOptions)
	    end;
	_ ->
	    start2(Proto, Port, InetModule, SocketModule, Options)
    end.

start2(Proto, Port, InetModule, SocketModule, Options) ->
    TCPsocket = case SocketModule:listen(Port, Options) of
		    {ok, S} ->
			S;
		    {error, E1} ->
			logger:log(error, "TCP listener: Could not open socket - module ~p, proto ~p, port ~p : ~p (~s)",
				   [SocketModule, Proto, Port, E1, InetModule:format_error(E1)]),
			logger:log(debug, "TCP socketopts : ~p", [Options]),
			throw("Could not open socket");
		    Unknown ->
			logger:log(error, "TCP listener: Unknown result from listen()  - module ~p, proto ~p, port ~p : ~p)",
				   [SocketModule, Proto, Port, Unknown]),
			throw("Could not open socket")
		end,
    Local = case catch InetModule:sockname(TCPsocket) of
		{ok, {IPlist, LocalPort}} ->
		    {siphost:makeip(IPlist), LocalPort};
		{error, E2} ->
		    logger:log(error, "TCP listener: ~p:sockname() returned error ~p", [InetModule, E2]),
		    {get_defaultaddr(Proto), 0};
		Unknown2 ->
		    logger:log(error, "TCP listener: ~p:sockname() returned unknown data : ~p", [InetModule, Unknown2]),
		    {get_defaultaddr(Proto), 0}
	    end,
    State = #state{socketmodule=SocketModule, inetmodule=InetModule, proto=Proto, port=Port, socket=TCPsocket, local=Local},
    ListenerPid = sipserver:safe_spawn(fun accept_loop_start/1, [State]),
    {ok, Local, ListenerPid}.

get_defaultaddr(tcp) -> "0.0.0.0";
get_defaultaddr(tcp6) -> "[::]".

accept_loop_start(State) when record(State, state) ->
    {IP, Port} = State#state.local,
    Desc = case State#state.proto of
	       tcp -> "TCP";
	       tcp6 -> "TCP";
	       tls -> "TLS";
	       tls6 -> "TLS"
	   end,
    logger:log(debug, "Listening on ~s ~s:~p (socket ~p)", [Desc, IP, Port, State#state.socket]),
    accept_loop(State).

%%--------------------------------------------------------------------
%% Function: accept_loop(State)
%%           State = state record()
%% Descrip.: Waits in accept() until someone connects to the port we
%%           are listening on. When someone does, spawn a
%%           tcp_connection process that handles this connection and
%%           then loop back to ourselves and do accept() again.
%% Returns : Should never return, but if the listening socket gets
%%           closed for some reason we return the atom() ok.
%%--------------------------------------------------------------------
accept_loop(State) when record(State, state) ->
    SocketModule = State#state.socketmodule,
    InetModule = State#state.inetmodule,
    ListenSocket = State#state.socket,
    Res = case SocketModule:accept(ListenSocket) of
	      {ok, NewSocket} ->
		  case InetModule:peername(NewSocket) of
		      {ok, {IPlist, InPortNo}} ->
			  IP = siphost:makeip(IPlist),
			  Remote = {IP, InPortNo},
			  Local = State#state.local,
			  Proto = State#state.proto,
			  start_tcp_connection(SocketModule, Proto, NewSocket, Local, Remote),
			  ok;
		      {error, E} ->
			  logger:log(error, "TCP listener: Could not get peername after accept() : ~p", [E]),
			  {ok}
		  end;
	      {error, closed} ->
		  logger:log(error, "TCP listener: accept() says the listensocket ~p was closed, no point in me staying alive.", [ListenSocket]),
		  {quit};
	      {error, E} ->
		  logger:log(error, "TCP listener: accept() returned error : ~s (~p)", [inet:format_error(E), E]),
		  timer:sleep(1000),
		  {error}
	  end,
    case Res of
	{quit} ->
	    ok;
	_ ->
	    accept_loop(State)
    end.

%%--------------------------------------------------------------------
%% Function: start_tcp_connection(SocketModule, Proto, Socket, Local,
%%                                Remote)
%%           SocketModule = atom(), the name of the sipsocket module
%%                          this socket uses (gen_tcp | ssl)
%%           Proto  = term() (atom(), tcp | tcp6 | tls | tls6)
%%           Socket = term()
%%           Local  = term() ({Host, Port} tuple())
%%           Remote = term() ({Host, Port} tuple()) 
%% Descrip.: Someone has just connected to our listening socket,
%%           resulting in the connection socket Socket. Start a
%%           tcp_connection process to handle this Socket, and in
%%           case it is an SSL socket, also set controlling process
%%           to the tcp_receiver process that the tcp_connection has
%%           started instead of us (the listening process, which is
%%           the default controlling process).
%%           This must be done from here since the SSL socket handler
%%           only allows the current controlling process to change who
%%           is it's controlling process.
%% Returns : ok    |
%%           error
%%--------------------------------------------------------------------
start_tcp_connection(SocketModule, Proto, Socket, Local, Remote) ->
    case tcp_connection:start_link(in, SocketModule, Proto, Socket, Local, Remote) of
	{ok, ConnPid} ->
	    case SocketModule of
		ssl ->
		    case catch gen_server:call(ConnPid, {get_receiver}) of
			{ok, RecvPid} ->
			    case SocketModule:controlling_process(Socket, RecvPid) of
				ok -> ok;
				{error, Reason} ->
				    logger:log(error, "TCP listener: Could not change controlling process of SSL socket ~p to ~p : ~p",
					       [Socket, RecvPid, Reason]),
				    %% XXX close socket here?
				    error
			    end;
			Unknown ->
			    logger:log(error, "TCP listener: Could not get receiver pid from newly started tcp connection ~p. Returned :~n~p",
				       [ConnPid, Unknown]),
			    %% XXX close socket here?
			    error
		    end;
		_ ->
		    ok
	    end;
	Unknown ->
	    logger:log(error, "TCP listener: Failed starting TCP connection for socket ~p : ~p",
		       [Socket, Unknown]),
	    %% XXX close socket here?
	    error
    end.
