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
-export([start_link/1]).

-export([send/4, is_reliable_transport/1, get_socket/3,
	 sipproto2str/1, via2sipsocketprotocol/1, sipproto2viastr/1,
	 is_good_socket/1]).

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
start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([Port]) ->
    logger:log(debug, "Transport layer supervisor started"),
    UDP = {sipsocket_udp, {sipsocket_udp, start_link, [Port]},
	   permanent, 2000, worker, [sipsocket_udp]},
    TCP = {tcp_dispatcher, {tcp_dispatcher, start_link, [Port]},
	   permanent, 2000, worker, [tcp_dispatcher]},
    {ok,{{one_for_one,5,60}, [UDP, TCP]}}.

%%====================================================================
%% Internal functions
%%====================================================================

%%====================================================================
%% Interface functions
%%====================================================================

send(Socket, SendToHost, PortInt, Message) when record(Socket, sipsocket) ->
    apply(Socket#sipsocket.module, send, [Socket, SendToHost, PortInt, Message]).

get_socket(SipProto, Host, Port) ->
    apply(SipProto, get_socket, [Host, Port]).

is_reliable_transport(Socket) when record(Socket, sipsocket) ->
    apply(Socket#sipsocket.module, is_reliable_transport, [Socket]).

sipproto2str(Socket) when record(Socket, sipsocket) ->
    sipproto2str(Socket#sipsocket.module);
sipproto2str(sipsocket_tcp) -> "tcp";
sipproto2str(sipsocket_udp) -> "udp".

sipproto2viastr(Socket) when record(Socket, sipsocket) ->
    sipproto2viastr(Socket#sipsocket.module);
sipproto2viastr(sipsocket_tcp) -> "SIP/2.0/TCP";
sipproto2viastr(sipsocket_udp) -> "SIP/2.0/UDP".

via2sipsocketprotocol(Socket) when record(Socket, sipsocket) ->
    via2sipsocketprotocol(Socket#sipsocket.module);
via2sipsocketprotocol("SIP/2.0/TCP") -> sipsocket_tcp;
via2sipsocketprotocol("SIP/2.0/UDP") -> sipsocket_udp.

is_good_socket(Socket) when record(Socket, sipsocket) ->
    case util:safe_is_process_alive(Socket#sipsocket.pid) of
	{true, _} ->
	    true;
	_ ->
	    false
    end;
is_good_socket(_) ->
    false.
