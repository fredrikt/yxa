%%%-------------------------------------------------------------------
%%% File    : stun_client.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: STUN test client for TCP/TLS. Really just for testing
%%%           the SIP/STUN demuxing server implementation in YXA.
%%%
%%% Created :  22 Mar 2006 by Fredrik Thulin <ft@it.su.se>
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
		[{reuseaddr, true}, {active, true}, binary]
	end,

    io:format("*** Connecting to ~p:~s:~p...~n", [Proto, Host, Port]),

    {ok, Socket} = gen_tcp:connect(Host, Port, SocketOpts),

    send3(Socket, Count).

send3(Socket, 0) ->
    ok = gen_tcp:close(Socket);
send3(Socket, Count) ->
    Timestamp = util:timestamp(),

    STUN = <<16#0001:16/big-unsigned,
	    0:16/big-unsigned,
	    Timestamp:128/big-unsigned
	    >>,

    io:format("*** Sending...~n"),
    gen_tcp:send(Socket, STUN),
    
    receive
	{tcp, Socket, Packet} ->
	    io:format("*** Received : ~p~n", [Packet]),
	    timer:sleep(1100),
	    send3(Socket, Count - 1);
	{tcp_closed, Socket} ->
	    io:format("*** Socket closed!~n"),
	    error
    after 1000 ->
	    io:format("*** Timed out!~n"),
	    error
    end.
