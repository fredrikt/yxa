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
	 
	 behaviour_info/1
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
