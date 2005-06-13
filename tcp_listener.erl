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

-export([start_link/2]).

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

-record(state, {
	  socketmodule,	%% atom(), gen_tcp | ssl
	  inetmodule,	%% atom(), inet | ssl
	  proto,	%% atom(), tcp | tcp6 | tls | tls6
	  port,		%% integer(), the port this tcp_listener instance listen on
	  socket,	%% term(), our listening socket
	  local		%% tuple(), {LocalIP, LocalPort}
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
%% Function: start_link(Proto, Port)
%%           Proto = atom(), tcp | tcp6 | tls | tls6
%%           Port  = integer()
%% Descrip.: Starts the listener.
%% Returns : {ok, Pid} |
%%           ignore
%%           Pid = pid(), the process we spawn that does accept().
%%--------------------------------------------------------------------
start_link(Proto, Port) when is_atom(Proto), is_integer(Port) ->
    {ok, InetModule, SocketModule, Options} = get_settings(Proto),
    case (Proto == tls) or (Proto == tls6) of
	true ->
	    case sipserver:get_env(ssl_server_certfile, none) of
		none ->
		    logger:log(normal, "NOT starting ~p listener on port ~p, no SSL server certificate specified "
			       "(see README)", [Proto, Port]),
		    ignore;
		CertFile ->
		    NewOptions = lists:append(Options, [{certfile, CertFile}]),
		    Pid = spawn_link(?MODULE, start_listening, [Proto, Port, InetModule, SocketModule, NewOptions]),
		    {ok, Pid}
	    end;
	false ->
	    Pid = spawn_link(?MODULE, start_listening, [Proto, Port, InetModule, SocketModule, Options]),
	    {ok, Pid}
    end.

%%--------------------------------------------------------------------
%% Function: start_listening(Proto, Port, InetModule, SocketModule,
%%                           Options)
%%           Proto = atom(), tcp | tcp6 | tls | tls6
%%           Port  = integer()
%%           InetModule   = atom(), inet | ssl
%%           SocketModule = atom(), gen_tcp | ssl
%%           Options      = term(), socket options
%% Descrip.: Starts listening on a port, then enters accept_loop.
%% Returns : does not return
%%--------------------------------------------------------------------
start_listening(Proto, Port, InetModule, SocketModule, Options)
  when is_atom(Proto), is_integer(Port), is_atom(InetModule), is_atom(SocketModule), is_list(Options) ->
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
    Local = case catch InetModule:sockname(TCPsocket) of
		{ok, {IPlist, LocalPort}} ->
		    {siphost:makeip(IPlist), LocalPort};
		{error, E2} ->
		    logger:log(error, "TCP listener: ~p:sockname() returned error ~p", [InetModule, E2]),
		    {get_defaultaddr(Proto), Port}
	    end,
    State = #state{socketmodule=SocketModule, inetmodule=InetModule, proto=Proto, port=Port, socket=TCPsocket,
		   local=Local},
    %% Now register with the tcp_dispatcher
    Remote = none,
    SipSocket = #sipsocket{module=sipsocket_tcp, proto=Proto, pid=self(), data={Local, Remote}},
    ok = gen_server:call(tcp_dispatcher, {register_sipsocket, listener, SipSocket}),
    accept_loop_start(State).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: accept_loop_start(State)
%%           State = state record()
%% Descrip.: Log a small message before starting the real accept_loop.
%% Returns : does not return
%%--------------------------------------------------------------------
accept_loop_start(State) when is_record(State, state) ->
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
accept_loop(State) when is_record(State, state) ->
    SocketModule = State#state.socketmodule,
    InetModule = State#state.inetmodule,
    ListenSocket = State#state.socket,
    case SocketModule:accept(ListenSocket) of
	{ok, NewSocket} ->
	    case InetModule:peername(NewSocket) of
		{ok, {IPlist, InPortNo}} ->
		    IP = siphost:makeip(IPlist),
		    Remote = {IP, InPortNo},
		    Local = State#state.local,
		    Proto = State#state.proto,
		    start_tcp_connection(SocketModule, Proto, NewSocket, Local, Remote),
		    accept_loop(State);
		{error, E} ->
		    logger:log(error, "TCP listener: Could not get peername after accept() : ~p", [E]),
		    erlang:error({"Could not get peername after accept", {error, E}}, [State])
	    end;
	{error, closed} ->
	    logger:log(error, "TCP listener: accept() says the listensocket ~p was closed, "
		       "no point in me staying alive.", [ListenSocket]),
	    erlang:error("Listening socket closed", [State]);
	{error, E} ->
	    logger:log(error, "TCP listener: accept() returned error : ~s (~p)", [inet:format_error(E), E]),
	    erlang:error({"Accept failed", {error, E}}, [State]);
	Unknown ->
	    %% To keep Dialyzer happy (otherwise complains about this function having no local return)
	    logger:log(error, "TCP listener: ~p:accept() returned unknown data", [SocketModule]),
	    logger:log(debug, "TCP listener: data returned by ~p:accept() : ~p", [SocketModule, Unknown]),
	    {error, accept_returned_unknown_data}
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
%% Returns : ok | throw(...)
%%--------------------------------------------------------------------
%%
%% SSL socket
%%
start_tcp_connection(ssl, Proto, Socket, Local, Remote) ->
    case tcp_connection:start_link(in, ssl, Proto, Socket, Local, Remote) of
	{ok, ConnPid} ->
	    {ok, RecvPid} = gen_server:call(ConnPid, {get_receiver}),
	    case ssl:controlling_process(Socket, RecvPid) of
		ok -> ok;
		{error, Reason} ->
		    logger:log(error, "TCP listener: Could not change controlling process of "
			       "SSL socket ~p to ~p : ~p", [Socket, RecvPid, Reason]),
		    erlang:error({"Failed changing controlling process for SSL socket", {error, Reason}},
				 [ssl, Proto, Socket, Local, Remote])
	    end;
	ignore ->
	    %% SSL socket was not acceptable. This is already logged and everything, so just return.
	    ok
    end;
%%
%% Non-SSL socket
%%
start_tcp_connection(SocketModule, Proto, Socket, Local, Remote) ->
    {ok, _ConnPid} = tcp_connection:start_link(in, SocketModule, Proto, Socket, Local, Remote),
    ok.

%%--------------------------------------------------------------------
%% Function: get_defaultaddr(Proto)
%%           Proto = tcp | tcp6
%% Descrip.: Get the "any" address.
%% Returns : Addr = string()
%%--------------------------------------------------------------------
get_defaultaddr(tcp) -> "0.0.0.0";
get_defaultaddr(tcp6) -> "[::]".

%%--------------------------------------------------------------------
%% Function: get_settings(Proto)
%%           Proto = tcp | tcp6 | tls | tls6
%% Descrip.: Get the variable things depending on protocol.
%% Returns : {ok, InetModule, SocketModule, Options}
%%           InetModule   = atom(), inet | ssl
%%           SocketModule = atom(), gen_tcp | ssl
%%           Options      = term(), socket options
%%--------------------------------------------------------------------
get_settings(tcp) ->
    {ok, inet, gen_tcp, [inet | ?SOCKETOPTS]};
get_settings(tcp6) ->
    {ok, inet, gen_tcp, [inet6 | ?SOCKETOPTS]};
get_settings(tls) ->
    L = sipserver:get_env(ssl_server_ssloptions, []),
    {ok, ssl, ssl, ?SSL_SOCKETOPTS ++ L};
get_settings(tls6) ->
    L = sipserver:get_env(ssl_server_ssloptions, []),
    {ok, ssl, ssl, [inet6 | ?SSL_SOCKETOPTS ++ L]}.
