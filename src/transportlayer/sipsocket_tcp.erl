%%%-------------------------------------------------------------------
%%% File    : sipsocket_tcp.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      TCP/TLS sipsocket module. Interface module to the
%%%           tcp_dispatcher gen_server process that shepherds all
%%%           TCP/TLS connection handler processes.
%%%
%%% @since    15 Dec 2003 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sipsocket_tcp).
%%-compile(export_all).

-behaviour(sipsocket).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0,
	 send/5,
	 is_reliable_transport/1,
	 get_socket/1,
	 get_specific_socket/1,
	 get_raw_socket/1,
	 get_remote_peer/1,
	 close_socket/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("socketlist.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------
%% @type tcp_proto() = tcp | tcp6 | tls | tls6. Transport layer TCP/TLS protocol.


%%====================================================================
%% External functions
%%====================================================================

start_link() ->
    tcp_dispatcher:start_link().

%%--------------------------------------------------------------------
%% @spec    (SipSocket, Proto, Host, Port, Message) ->
%%            SendRes         |
%%            {error, Reason} |
%%            term()
%%
%%            SipSocket = #sipsocket{}
%%            Proto     = tcp_proto()
%%            Host      = string()
%%            Port      = integer()
%%            Message   = term() "I/O list to send"
%%
%%            SendRes = term() "socket module send() result"
%%            Reason  = string()
%%
%% @doc     Send a SIP message. Get the tcp_connection process from
%%          the sipsocket, and request it to send the message.
%%          Returns whatever the socket module (gen_tcp or ssl) send-
%%          function returns. Typically 'ok' or {error, Something}.
%% @end
%%--------------------------------------------------------------------
send(#sipsocket{proto=SProto}, Proto, _Host, Port, _Message) when is_integer(Port), SProto /= Proto ->
    {error, "Protocol mismatch"};
send(SipSocket, _Proto, Host, Port, Message) when is_record(SipSocket, sipsocket), is_integer(Port) ->
    %% Proto matches the one in SipSocket, so it can't be the wrong one when
    %% we extract the connection handler pid from SipSocket.
    SPid = SipSocket#sipsocket.pid,
    Timeout = get_timeout(SipSocket#sipsocket.proto),
    case catch gen_server:call(SPid, {send, {Host, Port, Message}}, Timeout) of
	{send_result, Res} ->
	    Res;
	{'EXIT', {noproc, _}} ->
	    %% catch some more common gen_server errors since the error tuples from
	    %% gen_server calls are _huge_
	    Msg = io_lib:format("sipsocket_tcp failed sending through pid ~p : no such process",
				[SPid]),
	    {error, lists:flatten(Msg)};
	{'EXIT', Reason} ->
	    Msg = io_lib:format("sipsocket_tcp failed sending through pid ~p : ~p",
				[SPid, Reason]),
	    {error, lists:flatten(Msg)}
    end.

%%--------------------------------------------------------------------
%% @spec    (Dst) ->
%%            SipSocket       |
%%            {error, Reason}
%%
%%            Dst = #sipdst{}
%%
%%            SipSocket = #sipsocket{}
%%            Reason    = string()
%%
%% @doc     Get a socket, cached or new, useable to send messages to
%%          this destination.
%% @end
%%--------------------------------------------------------------------
%%
%% Protocol is 'tls' or 'tls6'
%%
get_socket(#sipdst{proto=Proto}=Dst) when Proto == tls; Proto == tls6 ->
    case yxa_config:get_env(tls_disable_client) of
	{ok, true} ->
	    {error, "TLS client disabled"};
	{ok, false} ->
	    get_socket2(Dst, false)
    end;
%%
%% Protocol is 'tcp' or 'tcp6'
%%
get_socket(#sipdst{proto=Proto}=Dst) when Proto == tcp; Proto == tcp6 ->
    get_socket2(Dst, false).

%% get_socket2/2 - part of get_socket/1
get_socket2(Dst, false) ->
    Timeout = get_timeout(Dst#sipdst.proto),
    case catch gen_server:call(tcp_dispatcher, {get_socket, Dst}, Timeout) of
	{error, try_again} ->
	    %% TCP dispatcher started a new tcp_connection to connect to Dst, but
	    %% there was already a connection attempt ongoing. The second tcp_connection
	    %% noticed this, but when it was about to queue with the first
	    %% tcp_connection had finished
	    get_socket2(Dst, true);
	{error, E} ->
	    {error, E};
	{ok, Socket} ->
	    Socket;
	{'EXIT', Reason} ->
	    Msg = io_lib:format("sipsocket_tcp failed fetching ~p socket : ~p",
				[Dst#sipdst.proto, Reason]),
	    {error, lists:flatten(Msg)}
    end;
get_socket2(Dst, true) ->
    Msg = io_lib:format("sipsocket_tcp failed fetching ~p socket (tried twice)",
			[Dst#sipdst.proto]),
    {error, lists:flatten(Msg)}.

%%--------------------------------------------------------------------
%% @spec    (Id) ->
%%            SipSocket       |
%%            {error, Reason}
%%
%%            Id = #ob_id{}
%%
%%            SipSocket = #sipsocket{}
%%            Reason    = string()
%%
%% @doc     Return a specific socket. Used by draft-Outbound implem-
%%          entation to send requests using an existing flow, or not
%%          at all.
%% @end
%%--------------------------------------------------------------------
get_specific_socket(#ob_id{proto = Proto} = Id) when Proto == tcp orelse Proto == tcp6
						     orelse Proto == tls orelse Proto == tls6 ->
    case catch gen_server:call(tcp_dispatcher, {get_specific_socket, Id}) of
	{ok, Socket} ->
	    Socket;
	E ->
	    logger:log(debug, "sipsocket_tcp: Failed fetching specific socket : ~p", [E]),
	    E
    end.

%%--------------------------------------------------------------------
%% @spec    (Socket) ->
%%            {ok, RawSocket} |
%%            {error, Reason}
%%
%%            Socket = #sipsocket{}
%%
%%            RawSocket = term()
%%            Reason    = string()
%%
%% @doc     Get the raw TCP/UDP/TLS socket from the socket handler. Be
%%          careful with what you do with the raw socket - don't use
%%          it for sending/receiving for example. Intended for use in
%%          extractin certificate information of an SSL socket or
%%          similar.
%% @end
%%--------------------------------------------------------------------
get_raw_socket(SipSocket) when is_record(SipSocket, sipsocket) ->
    SPid = SipSocket#sipsocket.pid,
    Timeout = get_timeout(SipSocket#sipsocket.proto),
    case catch gen_server:call(SPid, get_raw_socket, Timeout) of
	{ok, RawSocket} ->
	    {ok, RawSocket};
	{'EXIT', Reason} ->
	    Msg = io_lib:format("sipsocket_tcp failed getting raw socket from pid ~p : ~p",
				[SPid, Reason]),
	    {error, lists:flatten(Msg)}
    end.

%%--------------------------------------------------------------------
%% @spec    (Id) ->
%%            {ok, Proto, IP, Port} |
%%            {error, Reason}
%%
%%            Id = #ob_id{}
%%
%%            Proto  = tcp_proto()
%%            IP     = string()
%%            Port   = integer()
%%            Reason = string()
%%
%% @doc     Get the remote IP and port of a specific socket.
%% @end
%%--------------------------------------------------------------------
get_remote_peer(Id) when is_record(Id, ob_id) ->
    case get_specific_socket(Id) of
	{ok, SipSocket} when is_record(SipSocket, sipsocket) ->
	    #sipsocket{proto	= Proto,
		       hostport = #hp{r_ip   = IP,
				      r_port = Port
				     }
		      } = SipSocket,
	    {ok, Proto, IP, Port};
	E ->
	    E
    end.

%%--------------------------------------------------------------------
%% @spec    (_SipSocket) -> true
%%
%% @doc     Return true. This sipsocket modules transports are
%%          reliable. The meaning of reliable is that they handle
%%          resends automatically, so the transaction layer does not
%%          have to set up timers to resend messages.
%% @end
%%--------------------------------------------------------------------
is_reliable_transport(#sipsocket{proto = Proto}) when Proto == tcp; Proto == tcp6; Proto == tls; Proto == tls6 ->
    true.

%%--------------------------------------------------------------------
%% @spec    (SipSocket) ->
%%            ok              |
%%            {error, Reason}
%%
%%            SipSocket = #sipsocket{}
%%
%%            Reason = string()
%%
%% @doc     Close a socket.
%% @end
%%--------------------------------------------------------------------
close_socket(#sipsocket{proto = Proto, pid = SPid}) when Proto == tcp; Proto == tcp6; Proto == tls; Proto == tls6 ->
    case catch gen_server:cast(SPid, {close, self()}) of
	ok ->
	    ok;
	{'EXIT', {noproc, _}} ->
	    %% catch some more common gen_server errors since the error tuples from
	    %% gen_server call are _huge_
	    Msg = io_lib:format("sipsocket_tcp failed closing socket controlled by ~p : no such process",
				[SPid]),
	    {error, lists:flatten(Msg)};
	{'EXIT', Reason} ->
	    Msg = io_lib:format("sipsocket_tcp failed closing socket controlled by ~p : ~p",
				[SPid, Reason]),
	    {error, lists:flatten(Msg)}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

get_timeout(tcp) -> 1500;
get_timeout(tcp6) -> 1500;
get_timeout(tls) -> 5000;
get_timeout(tls6) -> 5000.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    autotest:mark(?LINE, "tcp connections - 0"),

    %% check if tcp_dispatcher is running, otherwise we need to start it
    StartedDispatcher =
	case erlang:whereis(tcp_dispatcher) of
	    undefined ->
		{ok, _DispatcherPid} = tcp_dispatcher:start_link(),
		true;
	    _DispatcherPid ->
		false
	end,

    LocalhostAddr = {127, 0, 0, 1},

    autotest:mark(?LINE, "outgoing connections - 0"),
    %% start one server process that listens on an available port
    {ok, TestPort} = test_start_listener(LocalhostAddr),

    autotest:mark(?LINE, "outgoing connections - 1.0"),
    %% Start 10 processes that will all try to connect to TestPort. Only the
    %% first one should actually try to connect, but all should succeed.
    TestConnectDst1 = #sipdst{proto = tcp,
			      addr  = siphost:makeip(LocalhostAddr),
			      port  = TestPort
			     },
    TestConnectNumConnects = 10,
    ok = test_start_connecting_processes(TestConnectDst1, TestConnectNumConnects),

    {ok, ConnectResults1} = test_collect_results(TestConnectNumConnects, []),

    autotest:mark(?LINE, "outgoing connections - 1.1"),
    %% make sure all connection attemtps returned in the same thing
    case lists:all(fun(Elem) ->
			   (Elem == hd(ConnectResults1))
		   end, ConnectResults1) of
	true ->
	    ok;
	false ->
	    io:format("Test FAILED, connection results :~n~p~n", [ConnectResults1]),
	    throw("failed, not all connections are the same")
    end,

    autotest:mark(?LINE, "outgoing connections - 1.2"),
    %% make sure they all succeeded (all are the same, checked above)
    #sipsocket{proto = tcp,
	       pid = OutgoingPid1
	      } = hd(ConnectResults1),

    autotest:mark(?LINE, "outgoing connections - 1.3"),
    %% make sure the connection pid is still alive
    true = erlang:is_process_alive(OutgoingPid1),

    autotest:mark(?LINE, "outgoing connections - 1.4"),
    %% now make sure the connection is closed
    erlang:monitor(process, OutgoingPid1),
    gen_server:cast(OutgoingPid1, {close, self()}),
    %% wait until the connection process has terminated
    receive
	{'DOWN', _MonitorRef, process, OutgoingPid1, normal} ->
	    ok
    after
	1000 ->
	    Msg = io_lib:format("Test FAILED, could not get tcp_connection ~p to shut down",
				[OutgoingPid1]),
	    throw(lists:flatten(Msg))
    end,

    autotest:mark(?LINE, "outgoing connections - 2.0"),
    %% test with no server listening, port should be closed now
    ok = test_start_connecting_processes(TestConnectDst1, TestConnectNumConnects),

    {ok, ConnectResults2} = test_collect_results(TestConnectNumConnects, []),

    autotest:mark(?LINE, "outgoing connections - 2.1"),
    %% make sure all connection attemtps returned in the same thing
    case lists:all(fun(Elem) ->
			   (Elem == hd(ConnectResults2))
		   end, ConnectResults2) of
	true ->
	    ok;
	false ->
	    io:format("Test FAILED, connection results :~n~p~n", [ConnectResults2]),
	    throw("failed, not all connections are the same")
    end,

    autotest:mark(?LINE, "outgoing connections - 2.2"),
    %% make sure they all succeeded (all are the same, checked above)
    {error, "Connection refused"} = hd(ConnectResults2),


    autotest:mark(?LINE, "tcp connections - 1"),
    %% stop TCP dispatcher again if we started it
    case StartedDispatcher of
	true ->
	    {'EXIT', {normal, _}} = (catch gen_server:call(tcp_dispatcher, {quit}));
	false ->
	    ok
    end,

    ok.

%% LISTENER PROCESS
test_start_listener(BindAddr) ->
    Parent = self(),
    Pid = spawn(fun() ->
			test_listen_accept_start(Parent, BindAddr)
		end),
    receive
	{listening_on, Pid, Port} when is_integer(Port) ->
	    {ok, Port}
    after 1000 ->
	    {error, "Did not receive message with port number from spawned pid"}
    end.

test_listen_accept_start(Parent, BindAddr) ->
    SocketOpts = [{ip, BindAddr},
		  binary,
		  {packet, 0},
		  {active, false},
		  {reuseaddr, false}
		 ],
    {ok, Socket} = gen_tcp:listen(0, SocketOpts),
    {ok, Port} = inet:port(Socket),
    Parent ! {listening_on, self(), Port},
    test_listen_accept_once(Socket),
    gen_tcp:close(Socket),
    receive
	{Parent, quit} ->
	    ok
    after
	10 * 1000 ->
	    timeout
    end.

test_listen_accept_once(Socket) ->
    {ok, NewSocket} = gen_tcp:accept(Socket),
    NewSocket.

%% OUTGOING CONNECTION PROCESS
test_start_connecting_processes(_Dst, 0) ->
    ok;
test_start_connecting_processes(Dst, N) ->
    Parent = self(),
    spawn(fun() ->
		  test_connect_start(Parent, Dst, N)
	  end),
    test_start_connecting_processes(Dst, N - 1).

test_connect_start(Parent, Dst, MyNum) ->
    Res = sipsocket:get_socket(Dst),
    Parent ! {get_socket_result, self(), MyNum, Res}.

%% RESULT COLLECTER
test_collect_results(0, Res) ->
    {ok, Res};
test_collect_results(N, Res) ->
    receive
	{get_socket_result, _Pid, _Num, CRes} ->
	    test_collect_results(N - 1, [CRes | Res])
    after
	5000 ->
	    timeout
    end.
