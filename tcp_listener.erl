%%% File    : tcp_listener.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : tcp_listener first does SocketModule:listen() and then
%%% sits around in a loop that does gen_tcp:accept() and spawns a
%%% tcp_connection for each socket.
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>

-module(tcp_listener).

-include("sipsocket.hrl").

-export([start/2]).

-record(state, {socketmodule, inetmodule, proto, port, socket, local}).

-define(SOCKETOPTS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
%% The ssl module does not support reuseaddr
-define(SSL_SOCKETOPTS, [binary, {packet, 0}, {active, once}, {nodelay, true}]).

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
		    logger:log(normal, "NOT starting ~p listener, no SSL client certificate specified (see README)", [Proto]),
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

accept_loop(State) when record(State, state) ->
    SocketModule = State#state.socketmodule,
    InetModule = State#state.inetmodule,
    ListenSocket = State#state.socket,
    Res = case SocketModule:accept(ListenSocket) of
	      {ok, NewSocket} ->
		  timer:sleep(500),
		  %%case catch SocketModule:peercert(NewSocket) of
		  %%    R ->
		  %%	  logger:log(debug, "Peer certificate :~n~p", [R])
		  %%  end,
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
		  end,
		  receive
		      Msg ->
			  logger:log(normal, "FREDRIK: TCP LISTENER RECEIVED SIGNAL! ~p", [Msg])
		      after 0 ->
			      true
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

get_defaultaddr(tcp) -> "0.0.0.0";
get_defaultaddr(tcp6) -> "[::]".

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
				    error
			    end;
			Unknown ->
			    logger:log(error, "TCP listener: Could not get receiver pid from newly started tcp connection ~p",
				       [ConnPid]),
			    error
		    end;
		_ ->
		    ok
	    end;
	Unknown ->
	    logger:log(error, "TCP listener: Failed starting TCP connection for socket ~p : ~p",
		       [Socket, Unknown]),
	    error
    end.
