%%%-------------------------------------------------------------------
%%% File    : stun_client.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      STUN test client for TCP/TLS. Really just for testing
%%%           the SIP/STUN demuxing server implementation in YXA.
%%%
%%% @since     22 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(stun_client).

-export([send/2,
	 send/4
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("stun.hrl").


%% RFC3489bis-03 #6 (STUN Message Structure)
-define(STUN_MAGIC_COOKIE, 16#2112A442).


%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
send(URLstr, Count) when is_list(URLstr), is_integer(Count) ->
    URL = sipurl:parse(URLstr),
    [Dst | _] = sipdst:url_to_dstlist(URL, 1000, URL),

    #sipdst{proto = Proto,
	    addr  = Host,
	    port  = Port
	   } = Dst,


    send2(Proto, Host, Port, Count).

send(Proto, Host, Port, Count) when is_atom(Proto), is_list(Host), is_integer(Port), is_integer(Count) ->
    send2(Proto, Host, Port, Count).


send2(Proto, Host, Port, Count) when is_atom(Proto), is_list(Host), is_integer(Port), is_integer(Count) ->
    SocketOpts =
	case Proto of
	    tcp6 ->
		[{reuseaddr, true}, {active, true}, binary, inet6];
	    tcp ->
		[{reuseaddr, true}, {active, true}, binary];
	    udp6 ->
		[{reuseaddr, true}, {active, true}, binary];
	    udp ->
		[{reuseaddr, true}, {active, true}, binary]
	end,

    io:format("*** Connecting to ~p:~s:~p...~n", [Proto, Host, Port]),

    {ok, Socket} =
	if
	    Proto == tcp ; Proto == tcp6 ->
		gen_tcp:connect(Host, Port, SocketOpts);
	    Proto == udp ; Proto == udp6 ->
		gen_udp:open(0, SocketOpts)
	end,

    send3(Proto, Socket, Host, Port, Count).

send3(udp, _Socket, _Host, _Port, 0) ->
    ok;
send3(tcp, Socket, _Host, _Port, 0) ->
    ok = gen_tcp:close(Socket);
send3(Proto, Socket, Host, Port, Count) ->
    Timestamp = util:timestamp(),

    STUN_OLD = <<16#0001:16/big-unsigned,
	    0:16/big-unsigned,
	    Timestamp:128/big-unsigned
	    >>,

    STUN = <<16#0001:16/big-unsigned,
	    0:16/big-unsigned,
	    ?STUN_MAGIC_COOKIE:32/big-unsigned,
	    Timestamp:96/big-unsigned
	    >>,

    io:format("*** Sending...~n"),
    case Proto of
	tcp -> gen_tcp:send(Socket, STUN);
	udp -> gen_udp:send(Socket, Host, Port, STUN)
    end,

    receive
	{tcp, Socket, Packet} when Proto == tcp ->
	    io:format("*** Received : ~p~n", [Packet]),
	    io:format("~p~n", [stun:decode_attributes(Packet, 20)]),
	    timer:sleep(1100),
	    send3(Proto, Socket, Host, Port, Count - 1);
	{tcp_closed, Socket} when Proto == tcp ->
	    io:format("*** Socket closed!~n"),
	    error;

	{udp, Socket, _RecvHost, _RecvPort, Packet} when Proto == udp ->
	    io:format("*** Received : ~p~n", [Packet]),
	    io:format("~p~n", [stun:decode_attributes(Packet, 20)]),
	    timer:sleep(1100),
	    send3(Proto, Socket, Host, Port, Count - 1);

	Unknown ->
	    io:format("*** UNKNOWN SIGNAL : ~p~n~n", [Unknown]),
	    send3(Proto, Socket, Host, Port, Count - 1)

    after 1000 ->
	    io:format("*** Timed out!~n"),
	    error
    end.
