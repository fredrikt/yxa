%%% File    : tcp_listener.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : tcp_listener first does SocketModule:listen() and then
%%% sits around in a loop that does gen_tcp:accept() and spawns a
%%% tcp_connection for each socket.
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>

-module(tcp_listener).

-include("sipsocket.hrl").

-export([start/2]).

-record(state, {socketmodule, port, socket, local}).

start(SocketModule, Port) when integer(Port) ->
    TCPsocket = case SocketModule:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
		    {ok, S} ->
			S;
		    {error, E1} ->
			logger:log(error, "TCP listener: Could not open TCP socket - module ~p, port ~p : ~s",
				   [SocketModule, Port, inet:format_error(E1)]),
			erlang:fault("Could not open TCP socket", [SocketModule, Port])
		end,
    Local = case inet:sockname(TCPsocket) of
		{ok, {IPlist, LocalPort}} ->
		    {siphost:makeip(IPlist), LocalPort};
		{error, E2} ->
		    logger:log(error, "TCP listener: sockname() returned error ~p", [E2]),
		    {"0.0.0.0", 0}
	    end,
    State = #state{socketmodule=SocketModule, port=Port, socket=TCPsocket, local=Local},
    ListenerPid = sipserver:safe_spawn(fun accept_loop_start/1, [State]),
    {ok, Local, ListenerPid}.

accept_loop_start(State) when record(State, state) ->
    {IP, Port} = State#state.local,
    logger:log(debug, "Listening on TCP ~s:~p (socket ~p)", [IP, Port, State#state.socket]),
    accept_loop(State).

accept_loop(State) when record(State, state) ->
    SocketModule = State#state.socketmodule,
    ListenSocket = State#state.socket,
    Res = case SocketModule:accept(ListenSocket) of
	      {ok, NewSocket} ->
		  case inet:peername(NewSocket) of
		      {ok, {IPlist, InPortNo}} ->
			  IP = siphost:makeip(IPlist),
			  Remote = {IP, InPortNo},
			  Local = State#state.local,
			  R = tcp_connection:start_link(SocketModule, NewSocket, in, Local, Remote),
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
