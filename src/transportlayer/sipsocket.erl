%%%-------------------------------------------------------------------
%%% File    : sipsocket.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Transport layer processes supervisor, and sipsocket
%%%           interface functions.
%%%
%%% @since    21 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sipsocket).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([send/5,

	 get_socket/1,
	 get_specific_socket/1,
	 get_raw_socket/1,
	 get_remote_peer/1,

	 viastr2proto/1,
	 proto2viastr/1,

	 is_good_socket/1,
	 is_reliable_transport/1,
	 default_port/2,

	 get_listenport/1,
	 get_all_listenports/0,
	 add_listener_info/3,
	 close_socket/1,

	 behaviour_info/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([start_link/0,
	 init/1
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, transportlayer).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------
%% @type proto() = tcp | tcp6 | udp | udp6 | tls | tls6 | yxa_test | yxa_test6.
%%	           Transport layer protocol.


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @spec    (callbacks) -> [tuple()]
%%
%% @doc     Describe all the API functions a module indicating it is a
%%          sipsocket behaviour module must export. List of tuples of
%%          the function names and their arity.
%% @hidden
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{start_link, 0},
     {send, 5},
     {is_reliable_transport, 1},
     {get_socket, 1},
     {get_specific_socket, 1},
     {get_raw_socket, 1},
     {get_remote_peer, 1},
     {close_socket, 1}
    ];
behaviour_info(_Other) ->
    undefined.


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ([]) ->
%%            {ok, {SupFlags, [ChildSpec]}} |
%%            ignore                        |
%%            {error, Reason}
%%
%% @doc     Initialize the supervisor with child specs for one
%%          sipsocket_udp and one tcp_dispatcher worker process.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
    logger:log(debug, "Transport layer supervisor started"),
    ets:new(yxa_sipsocket_info, [public, bag, named_table]),
    ets:new(sipsocket_blacklist:get_blacklist_name(),
	    [public, bag, named_table]),
    UDP = {sipsocket_udp, {sipsocket_udp, start_link, []},
	   permanent, 2000, worker, [sipsocket_udp]},
    TCP = {tcp_dispatcher, {sipsocket_tcp, start_link, []},
	   permanent, 2000, worker, [tcp_dispatcher]},
    TCPlisteners = tcp_dispatcher:get_listenerspecs(),
    BlackList = {sipsocket_blacklist, {sipsocket_blacklist, start_link, []},
		 permanent, 2000, worker, [sipsocket_blacklist]},
    MyList = [UDP, TCP] ++ TCPlisteners ++ [BlackList],
    {ok, {{one_for_one, 5, 60}, MyList}}.


%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Interface functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Socket, Proto, Host, Port, Message) ->
%%            Res
%%
%%            Socket  = #sipsocket{}
%%            Proto   = proto()
%%            Host    = string()
%%            Port    = integer()
%%            Message = term() "I/O list to send"
%%
%%            Res = ok | {error, E} | term()
%%
%% @doc     Locate a sipsocket module through the Socket record(), and
%%          then ask that sipsocket module to send Message to Host,
%%          port Port using protocol Proto. Currently, none of our
%%          sipsocket modules accept different protocol in the Socket
%%          record() and Proto.
%% @end
%%--------------------------------------------------------------------
send(Socket, Proto, Host, Port, Message) when is_record(Socket, sipsocket), is_atom(Proto),
					      is_list(Host), is_integer(Port) ->
    SipSocketM = Socket#sipsocket.module,
    SipSocketM:send(Socket, Proto, Host, Port, Message).

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
%%          Dst using protocol Proto.
%% @end
%%--------------------------------------------------------------------
get_socket(Dst) when is_record(Dst, sipdst) ->
    Module = proto2module(Dst#sipdst.proto),
    Module:get_socket(Dst).


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
%% @doc     Get a specific socket. Don't try to open a new connection
%%          if the requested one does not exist.
%% @end
%%--------------------------------------------------------------------
get_specific_socket(#ob_id{proto = Proto} = Id) when is_atom(Proto) ->
    Module = proto2module(Proto),
    Module:get_specific_socket(Id);
get_specific_socket(Unknown) ->
    logger:log(error, "Sipsocket: Request for specific socket with invalid socket identifier : ~p", [Unknown]),
    {error, "Invalid specific socket identifier"}.

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
get_raw_socket(Socket) when is_record(Socket, sipsocket) ->
    SipSocketM = Socket#sipsocket.module,
    SipSocketM:get_raw_socket(Socket).

%%--------------------------------------------------------------------
%% @spec    (Socket) ->
%%            {ok, Proto, Addr, Port} |
%%            not_applicable
%%
%%            Socket = #sipsocket{}
%%
%%            Proto = proto()
%%            Addr  = string() "IP/IPv6 address of peer"
%%            Port  = integer()
%%
%% @doc     Get informaion about 'who' is on the other side of a
%%          specific socket.
%% @end
%%--------------------------------------------------------------------
get_remote_peer(Socket) when is_record(Socket, sipsocket) ->
    SipSocketM = Socket#sipsocket.module,
    SipSocketM:get_remote_peer(Socket).

%%--------------------------------------------------------------------
%% @spec    (Socket) -> true | false
%%
%% @doc     Call the sipsocket module in specified in Socket and let
%%          it tell the caller if it is a reliable transport or not.
%% @end
%%--------------------------------------------------------------------
is_reliable_transport(#sipsocket{module=Module} = Socket) ->
    Module:is_reliable_transport(Socket).

%%--------------------------------------------------------------------
%% @spec    (Proto) ->
%%            Module
%%
%%            Proto = proto()
%%
%%            Module = atom()
%%
%% @doc     Get the sipsocket module for Proto.
%% @end
%%--------------------------------------------------------------------
proto2module(tcp)  -> sipsocket_tcp;
proto2module(tcp6) -> sipsocket_tcp;
proto2module(tls)  -> sipsocket_tcp;
proto2module(tls6) -> sipsocket_tcp;
proto2module(udp)  -> sipsocket_udp;
proto2module(udp6) -> sipsocket_udp;
proto2module(yxa_test)  -> sipsocket_test;
proto2module(yxa_test6)  -> sipsocket_test.

%%--------------------------------------------------------------------
%% @spec    (Socket) ->
%%            ViaStr
%%
%%            Socket = #sipsocket{}
%%
%%            ViaStr = string()
%%
%% @doc     Get the Via string (e.g. "SIP/2.0/TCP") for a transport
%%          layer protocol.
%% @end
%%--------------------------------------------------------------------
proto2viastr(Socket) when is_record(Socket, sipsocket) ->
    proto2viastr(Socket#sipsocket.proto);

%%--------------------------------------------------------------------
%% @spec    (Proto) ->
%%            ViaStr
%%
%%            Proto = proto()
%%
%%            ViaStr = string()
%%
%% @doc     Get the Via string (e.g. "SIP/2.0/TCP") for a transport
%%          layer protocol.
%% @end
%%--------------------------------------------------------------------
proto2viastr(tcp)  -> "SIP/2.0/TCP";
proto2viastr(tcp6) -> "SIP/2.0/TCP";
proto2viastr(tls)  -> "SIP/2.0/TLS";
proto2viastr(tls6) -> "SIP/2.0/TLS";
proto2viastr(udp)  -> "SIP/2.0/UDP";
proto2viastr(udp6) -> "SIP/2.0/UDP";
proto2viastr(yxa_test) -> "SIP/2.0/YXA-TEST";
proto2viastr(yxa_test6) -> "SIP/2.0/YXA-TEST".

%%--------------------------------------------------------------------
%% @spec    (Str) ->
%%            Proto
%%
%%            Str = string() "\"SIP/2.0/TCP\" or other"
%%
%%            Proto = proto()
%%
%% @doc     Turn a SIP/2.0/TCP or similar string into an atom
%%          corresponding to our internal representation of that
%%          protocol.
%% @end
%%--------------------------------------------------------------------
viastr2proto("SIP/2.0/TCP") -> tcp;
viastr2proto("SIP/2.0/TLS") -> tls;
viastr2proto("SIP/2.0/UDP") -> udp;
viastr2proto("SIP/2.0/YXA-TEST") -> yxa_test.

%%--------------------------------------------------------------------
%% @spec    (Socket) -> true | false
%%
%%            Socket = #sipsocket{}
%%
%% @doc     Check if a socket is still potentially useable. Does not
%%          guarantee that sending using the socket in question will
%%          be successfull, but indicates if it is worth trying to
%%          use the socket at all or not.
%% @end
%%--------------------------------------------------------------------
is_good_socket(Socket) when is_record(Socket, sipsocket) ->
    case util:safe_is_process_alive(Socket#sipsocket.pid) of
	{true, _} ->
	    true;
	_ ->
	    false
    end;
is_good_socket(_) ->
    false.

%%--------------------------------------------------------------------
%% @spec    (Proto) ->
%%            Port
%%
%%            Proto = proto()
%%
%%            Port = integer()
%%
%% @doc     Return the port we would listen on for a Proto, regardless
%%          of if we in fact are listening on the port or not.
%% @end
%%--------------------------------------------------------------------
get_listenport(Proto) when Proto == tls; Proto == tls6 ->
    case yxa_config:get_env(tls_listenport) of
	{ok, P} when is_integer(P) ->
	    P;
	none ->
	    default_port(Proto, none)
    end;
get_listenport(Proto) when Proto == tcp; Proto == tcp6; Proto == udp; Proto == udp6 ->
    case yxa_config:get_env(listenport) of
	{ok, P} when is_integer(P) ->
	    P;
	none ->
	    default_port(Proto, none)
    end;
get_listenport(yxa_test) ->
    case get({sipsocket_test, get_listenport}) of
	undefined ->
	    6050;
	R ->
	    R
    end.

%%--------------------------------------------------------------------
%% @spec    () ->
%%            PortList
%%
%%            PortList = [integer()]
%%
%% @doc     Returns a list of all ports we listen on. In some places,
%%          we need to get a list of all ports which are valid for
%%          this proxy. Notes : Perhaps this problem can't be solved
%%          this easilly - what if we have multiple interfaces and
%%          listen on different ports on them?
%% @end
%%--------------------------------------------------------------------
get_all_listenports() ->
    get_all_listenports2(ets:tab2list(yxa_sipsocket_info), []).

get_all_listenports2([{_Pid, #yxa_sipsocket_info_e{port = Port}} | T], Res) ->
    get_all_listenports2(T, [Port | Res]);
get_all_listenports2([], Res) ->
    lists:usort(Res).

%%--------------------------------------------------------------------
%% @spec    (Proto, Addr, Port) -> ok
%%
%%            Proto = proto()
%%            Addr  = string() "listening IP address"
%%            Port  = integer()
%%
%% @doc     Add an entry to ETS table yxa_sipsocket_info and sweep it
%%          for stale entrys.
%% @end
%%--------------------------------------------------------------------
add_listener_info(Proto, Addr, Port) when is_atom(Proto), is_list(Addr), is_integer(Port) ->
    InfoRecord = #yxa_sipsocket_info_e{proto = Proto,
				       addr  = Addr,
				       port  = Port
				      },
    ets:insert(yxa_sipsocket_info, {self(), InfoRecord}),
    %% Remove any stale records from yxa_sipsocket_info while we are at it
    %% This is really because the ETS table is created by the sipsocket supervisor,
    %% but a supervisor has no hooks that are called when it restarts it's children.
    %% The tcp_dispatcher can clean up for TCP/TLS listeners, but who is going to clean
    %% away dead UDP listeners, or clean away the previous generation of TCP/TLS listeners
    %% if the tcp_dispatcher terminates? A better solution would be welcome.
    lists:map(fun({Pid, Y}) when is_pid(Pid), is_record(Y, yxa_sipsocket_info_e) ->
		      case erlang:is_process_alive(Pid) of
			  true ->
			      ok;
			  false ->
			      ets:delete(yxa_sipsocket_info, Pid),
			      logger:log(debug, "Sipsocket: Removed stale listener from sipsocket info list : "
					 "~p (~p:~s:~p)", [Pid,
							   Y#yxa_sipsocket_info_e.proto,
							   Y#yxa_sipsocket_info_e.addr,
							   Y#yxa_sipsocket_info_e.port])
		      end
		      end, ets:tab2list(yxa_sipsocket_info)),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Proto, Port) ->
%%            Port
%%
%%            Proto    = proto() | ProtoStr
%%            ProtoStr = string() "\"sip\" | \"sips\""
%%            Port     = integer() | none
%%
%%            Port = integer()
%%
%% @doc     Yucky function returning a "default port number" as an
%%          integer, based on input Proto and Port.
%% @end
%%--------------------------------------------------------------------
default_port(Proto, none) when Proto == udp; Proto == udp6; Proto == tcp; Proto == tcp6; Proto == "sip" ->
    5060;
default_port(Proto, none) when Proto == tls; Proto == tls6 ; Proto == "sips" ->
    5061;
default_port(yxa_test, none) ->
    %% This is for tests which use fake sipsocket module to emulate network
    6050;
default_port(_, Port) when is_integer(Port) ->
    Port.

%%--------------------------------------------------------------------
%% @spec    (Socket) ->
%%            ok              |
%%            {error, Reason}
%%
%%            Socket = #sipsocket{}
%%
%%            Reason = not_applicable | term()
%%
%% @doc     Close a socket.
%% @end
%%--------------------------------------------------------------------
close_socket(Socket) when is_record(Socket, sipsocket) ->
    SipSocketM = Socket#sipsocket.module,
    SipSocketM:close_socket(Socket).

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

    %% test get_listenport(Proto)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_listenport/1 - 1"),
    %% test with 'udp'
    5060 = get_listenport(udp),

    autotest:mark(?LINE, "get_listenport/1 - 2"),
    %% test with 'tls6'
    5061 = get_listenport(tls6),

    autotest:mark(?LINE, "get_listenport/1 - 3"),
    %% test with invalid value (string)
    {'EXIT', {function_clause, _}} = (catch get_listenport("invalid")),

    autotest:mark(?LINE, "get_listenport/1 - 4"),
    %% test with invalid value (atom)
    {'EXIT', {function_clause, _}} = (catch get_listenport(none)),


    %% test get_all_listenports()
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_all_listenports/0 - 1"),
    %% simply call function
    ListenPorts1 = get_all_listenports(),

    autotest:mark(?LINE, "get_all_listenports/0 - 2"),
    %% check that all entrys returned are integers
    true = lists:all(fun(H) ->
			     is_integer(H)
		     end, ListenPorts1),

    autotest:mark(?LINE, "get_all_listenports/0 - 3"),
    %% check that we didn't get an empty list
    [_ | _] = ListenPorts1,


    %% default_port(Proto, PortIn)
    %%--------------------------------------------------------------------
    %% udp/udp6/tcp/tcp6/"sip"
    autotest:mark(?LINE, "default_port/2 - 1"),
    5060 = default_port(udp, none),
    autotest:mark(?LINE, "default_port/2 - 2"),
    5060 = default_port(udp6, none),
    autotest:mark(?LINE, "default_port/2 - 3"),
    5060 = default_port(tcp, none),
    autotest:mark(?LINE, "default_port/2 - 4"),
    5060 = default_port(tcp6, none),
    autotest:mark(?LINE, "default_port/2 - 5"),
    5060 = default_port("sip", none),

    %% tls/tls6/"sips"
    autotest:mark(?LINE, "default_port/2 - 6"),
    5061 = default_port(tls, none),
    autotest:mark(?LINE, "default_port/2 - 7"),
    5061 = default_port(tls6, none),
    autotest:mark(?LINE, "default_port/2 - 8"),
    5061 = default_port("sips", none),

    %% none of the above, port specified
    autotest:mark(?LINE, "default_port/2 - 9"),
    1234 = default_port(whatever, 1234),

    %% none of the above, port NOT specified - expect crash
    autotest:mark(?LINE, "default_port/2 - 11"),
    {'EXIT', {function_clause, _}} = (catch default_port("foo", none)),


    ok.
