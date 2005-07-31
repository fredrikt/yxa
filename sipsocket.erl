%%%-------------------------------------------------------------------
%%% File    : sipsocket.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Transport layer processes supervisor, and transport
%%%           layer interface functions.
%%%
%%% Created : 21 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipsocket).

-behaviour(supervisor).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([send/5,
	 is_reliable_transport/1,
	 get_socket/2,
	 viaproto2proto/1,
	 proto2viastr/1,
	 is_good_socket/1,
	 proto2module/1,
	 get_listenport/1,
	 get_all_listenports/0,
	 default_port/2,
	 
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
%% Records
%%--------------------------------------------------------------------


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Descrip.: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Function: behaviour_info(callbacks)
%% Descrip.: Describe all the API functions a module indicating it is
%%           a sipsocket behaviour module must export. List of tuples
%%           of the function names and their arity.
%% Returns : list() of tuple()
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{start, 1},
     {send, 5},
     {is_reliable_transport, 1},
     {get_socket, 1}
    ];
behaviour_info(_Other) ->
    undefined.


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([])
%% Descrip.: Initialize the supervisor with child specs for one
%%           sipsocket_udp and one tcp_dispatcher worker process.
%% Returns : {ok, {SupFlags, [ChildSpec]}} |
%%           ignore                        |
%%           {error, Reason}
%%--------------------------------------------------------------------
init([]) ->
    logger:log(debug, "Transport layer supervisor started"),
    UDP = {sipsocket_udp, {sipsocket_udp, start_link, []},
	   permanent, 2000, worker, [sipsocket_udp]},
    TCP = {tcp_dispatcher, {tcp_dispatcher, start_link, []},
	   permanent, 2000, worker, [tcp_dispatcher]},
    TCPlisteners = tcp_dispatcher:get_listenerspecs(),
    MyList = [UDP, TCP] ++ TCPlisteners,
    {ok, {{one_for_one, 5, 60}, MyList}}.


%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Interface functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: send(Socket, Proto, Host, Port, Message)
%%           Socket  = sipsocket record()
%%           Proto   = atom(), tcp|tcp6|udp|udp6|tls|tls6|...
%%           Host    = string()
%%           Port    = integer()
%%           Message = term(), I/O list to send
%% Descrip.: Locate a sipsocket module through the Socket record(),
%%           and then ask that sipsocket module to send Message to
%%           Host, port Port using protocol Proto. Currently, none of
%%           our sipsocket modules accept different protocol in the
%%           Socket record() and Proto.
%% Returns : Res
%%           Res = term(), result of apply() but typically
%%                         ok | {error, E}
%%--------------------------------------------------------------------
send(Socket, Proto, Host, Port, Message) when is_record(Socket, sipsocket), is_atom(Proto),
					      is_list(Host), is_integer(Port) ->
    SipSocketM = Socket#sipsocket.module,
    SipSocketM:send(Socket, Proto, Host, Port, Message).

%%--------------------------------------------------------------------
%% Function: get_socket(Module, Dst)
%%           Module = atom()
%%           Dst    = sipdst record()
%% Descrip.: Get a socket, cached or new, useable to send messages to
%%           Dst from sipsocket module Module.
%% Returns : Res
%%           Res = term(), result of apply() but typically a
%%                         sipsocket record()
%%--------------------------------------------------------------------
get_socket(Module, Dst) when is_atom(Module), is_record(Dst, sipdst) ->
    Module:get_socket(Dst).

%%--------------------------------------------------------------------
%% Function: is_reliable_transport(Socket)
%% Descrip.: Call the sipsocket module in specified in Socket and let
%%           it tell the caller if it is a reliable transport or not.
%% Returns : Res
%%           Res = term(), result of apply() but typically
%%                         true | false
%%--------------------------------------------------------------------
is_reliable_transport(Socket) when is_record(Socket, sipsocket) ->
    apply(Socket#sipsocket.module, is_reliable_transport, [Socket]).

proto2module(tcp)  -> sipsocket_tcp;
proto2module(tcp6) -> sipsocket_tcp;
proto2module(tls)  -> sipsocket_tcp;
proto2module(tls6) -> spisocket_tcp;
proto2module(udp)  -> sipsocket_udp;
proto2module(udp6) -> sipsocket_udp.

proto2viastr(Socket) when is_record(Socket, sipsocket) ->
    proto2viastr(Socket#sipsocket.proto);
proto2viastr(tcp)  -> "SIP/2.0/TCP";
proto2viastr(tcp6) -> "SIP/2.0/TCP";
proto2viastr(tls)  -> "SIP/2.0/TLS";
proto2viastr(tls6) -> "SIP/2.0/TLS";
proto2viastr(udp)  -> "SIP/2.0/UDP";
proto2viastr(udp6) -> "SIP/2.0/UDP".

viaproto2proto("SIP/2.0/TCP") -> tcp;
viaproto2proto("SIP/2.0/TLS") -> tls;
viaproto2proto("SIP/2.0/UDP") -> udp.

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
%% Function: get_listenport(Proto)
%%           Proto = atom(), tcp | tcp6 | tls | tls6 | udp | udp6
%% Descrip.: Return the port we would listen on for a Proto,
%%           regardless of if we in fact are listening on the port or
%%           not.
%% Returns : Port = integer()
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
%% Function: get_all_listenports()
%% Descrip.: Returns a list of all ports we listen on. In some places,
%%           we need to get a list of all ports which are valid for
%%           this proxy.
%% Returns : PortList = list() of integer()
%% Notes   : Perhaps this problem can't be solved this easilly - what
%%           if we have multiple interfaces and listen on different
%%           ports on them?
%%
%% XXX finish this function! Have to fetch a list of the ports we
%% listen on from the transport layer.
%%--------------------------------------------------------------------
get_all_listenports() ->
    [get_listenport(udp)].


%%--------------------------------------------------------------------
%% Function: default_port(Proto, Port)
%%           Proto = atom(), udp | udp6 | tcp | tcp6 | tls | tls6 |
%%                   string(), "sip" | "sips"
%%           Port  = integer() | none
%% Descrip.: Yucky function returning a "default port number" as an
%%           integer, based on input Proto and Port.
%% Returns : Port = integer()
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


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->

    %% test get_listenport(Proto)
    %%--------------------------------------------------------------------
    io:format("test: get_listenport/1 - 1~n"),
    %% test with 'udp'
    5060 = get_listenport(udp),

    io:format("test: get_listenport/1 - 2~n"),
    %% test with 'tls6'
    5061 = get_listenport(tls6),

    io:format("test: get_listenport/1 - 3~n"),
    %% test with invalid value (string)
    {'EXIT', {function_clause, _}} = (catch get_listenport("invalid")),

    io:format("test: get_listenport/1 - 4~n"),
    %% test with invalid value (atom)
    {'EXIT', {function_clause, _}} = (catch get_listenport(none)),


    %% test get_all_listenports()
    %%--------------------------------------------------------------------
    io:format("test: get_all_listenports/0 - 1~n"),
    %% simply call function
    ListenPorts1 = get_all_listenports(),

    io:format("test: get_all_listenports/0 - 2~n"),
    %% check that all entrys returned are integers
    true = lists:all(fun(H) ->
			     is_integer(H)
		     end, ListenPorts1),

    io:format("test: get_all_listenports/0 - 3~n"),
    %% check that we didn't get an empty list
    [_ | _] = ListenPorts1,


    %% default_port(Proto, PortIn)
    %%--------------------------------------------------------------------
    %% udp/udp6/tcp/tcp6/"sip"
    io:format("test: default_port/2 - 1~n"),
    5060 = default_port(udp, none),
    io:format("test: default_port/2 - 2~n"),
    5060 = default_port(udp6, none),
    io:format("test: default_port/2 - 3~n"),
    5060 = default_port(tcp, none),
    io:format("test: default_port/2 - 4~n"),
    5060 = default_port(tcp6, none),
    io:format("test: default_port/2 - 5~n"),
    5060 = default_port("sip", none),

    %% tls/tls6/"sips"
    io:format("test: default_port/2 - 6~n"),
    5061 = default_port(tls, none),
    io:format("test: default_port/2 - 7~n"),
    5061 = default_port(tls6, none),
    io:format("test: default_port/2 - 8~n"),
    5061 = default_port("sips", none),

    %% none of the above, port specified
    io:format("test: default_port/2 - 9~n"),
    1234 = default_port(whatever, 1234),

    %% none of the above, port NOT specified - expect crash
    io:format("test: default_port/2 - 11~n"),
    {'EXIT', {function_clause, _}} = (catch default_port("foo", none)),


    ok.
