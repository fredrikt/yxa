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
%% Include files
%%--------------------------------------------------------------------

-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([send/5,
	 is_reliable_transport/1,
	 get_socket/4,
	 viaproto2proto/1,
	 proto2viastr/1,
	 is_good_socket/1,
	 proto2module/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([start_link/0,
	 init/1]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

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

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([])
%% Descrip.: Initialize the supervisor with child specs for one
%%           sipsocket_udp and one tcp_dispatcher worker process.
%% Returns : {ok,  {SupFlags,  [ChildSpec]}} |
%%           ignore                          |
%%           {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
    logger:log(debug, "Transport layer supervisor started"),
    UDP = {sipsocket_udp, {sipsocket_udp, start_link, []},
	   permanent, 2000, worker, [sipsocket_udp]},
    TCP = {tcp_dispatcher, {tcp_dispatcher, start_link, []},
	   permanent, 2000, worker, [tcp_dispatcher]},
    {ok,{{one_for_one,5,60}, [UDP, TCP]}}.

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Interface functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: send(Socket, Proto, Host, Port, Message)
%%           Socket  = sipsocket record()
%%           Proto   = atom(), (at least tcp|tcp6|udp|udp6|tls)
%%           Host    = string()
%%           Port    = integer()
%%           Message = term()
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
%% Function: get_socket(Module, Proto, Host, Port)
%%           Module = atom()
%%           Proto  = atom(), (at least tcp | tcp6 | udp | udp6 | tls)
%%           Host   = string()
%%           Port   = integer()
%% Descrip.: Get a socket, cached or new, useable to send messages to
%%           Host on port Port using protocol Proto from sipsocket
%%           module Module.
%% Returns : Res
%%           Res = term(), result of apply() but typically a
%%                         sipsocket record()
%%--------------------------------------------------------------------
get_socket(Module, Proto, Host, Port) when is_atom(Module), is_atom(Proto), is_integer(Port) ->
    Module:get_socket(Proto, Host, Port).

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
