%%%-------------------------------------------------------------------
%%% File    : sipsocket.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : Transaction layer processes supervisor.
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
-export([start_link/0]).

-export([send/5, is_reliable_transport/1, get_socket/4,
	 viaproto2proto/1, proto2viastr/1,
	 is_good_socket/1, proto2module/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([init/1]).

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
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
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

send(Socket, Proto, Host, Port, Message) when list(Port) ->
    send(Socket, Proto, Host, list_to_integer(Port), Message);
send(Socket, Proto, Host, Port, Message) when record(Socket, sipsocket), integer(Port) ->
    apply(Socket#sipsocket.module, send, [Socket, Proto, Host, Port, Message]).

get_socket(Module, Proto, Host, Port) when atom(Module), atom(Proto), list(Port) ->
    get_socket(Module, Proto, Host, list_to_integer(Port));
get_socket(Module, Proto, Host, Port) when atom(Module), atom(Proto) ->
    apply(Module, get_socket, [Proto, Host, Port]).

is_reliable_transport(Socket) when record(Socket, sipsocket) ->
    apply(Socket#sipsocket.module, is_reliable_transport, [Socket]).

proto2module(tcp) -> sipsocket_tcp;
proto2module(tcp6) -> sipsocket_tcp;
proto2module(tls) -> sipsocket_tcp;
proto2module(tls6) -> spisocket_tcp;
proto2module(udp) -> sipsocket_udp;
proto2module(udp6) -> sipsocket_udp.
    
proto2viastr(Socket) when record(Socket, sipsocket) ->
    proto2viastr(Socket#sipsocket.proto);
proto2viastr(tcp) ->  "SIP/2.0/TCP";
proto2viastr(tcp6) -> "SIP/2.0/TCP";
proto2viastr(tls) ->  "SIP/2.0/TLS";
proto2viastr(tls6) -> "SIP/2.0/TLS";
proto2viastr(udp) ->  "SIP/2.0/UDP";
proto2viastr(udp6) -> "SIP/2.0/UDP".

viaproto2proto("SIP/2.0/TCP") -> tcp;
viaproto2proto("SIP/2.0/TLS") -> tls;
viaproto2proto("SIP/2.0/UDP") -> udp.

is_good_socket(Socket) when record(Socket, sipsocket) ->
    case util:safe_is_process_alive(Socket#sipsocket.pid) of
	{true, _} ->
	    true;
	_ ->
	    false
    end;
is_good_socket(_) ->
    false.
