%%%-------------------------------------------------------------------
%%% File    : sipsocket_tcp.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: TCP/TLS sipsocket module. Interface module to the
%%%           tcp_dispatcher gen_server process that shepherds all
%%%           TCP/TLS connection handler processes.
%%%
%%% Created : 15 Dec 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipsocket_tcp).
%%-compile(export_all).

-behaviour(sipsocket).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/1,
	 send/5,
	 is_reliable_transport/1,
	 get_socket/1
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("socketlist.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------


%%====================================================================
%% External functions
%%====================================================================

start(Port) when is_integer(Port) ->
    tcp_dispatcher:start_link(Port).

%%--------------------------------------------------------------------
%% Function: send(SipSocket, Proto, Host, Port, Message)
%%           SipSocket = sipsocket record()
%%           Proto     = atom(), tcp | tcp6 | tls | tls6
%%           Host      = string()
%%           Port      = integer()
%%           Message   = term(), I/O list to send
%% Descrip.: Send a SIP message. Get the tcp_connection process from
%%           the sipsocket, and request it to send the message.
%% Returns : SendRes         |
%%           {error, Reason}
%%           SendRes = term(), whatever the socket module (gen_tcp or
%%                     ssl) send-function returns. Typically 'ok' or
%%                     {error, _Something}
%%           Reason = string()
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
	{'EXIT', Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: get_socket(Dst)
%%           Dst = sipdst record()
%% Descrip.: Get a socket, cached or new, useable to send messages to
%%           this destination.
%% Returns : SipSocket       |
%%           {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
%%
%% Protocol is 'tls' or 'tls6'
%% 
get_socket(#sipdst{proto=Proto}=Dst) when Proto == tls; Proto == tls6 ->
    case sipserver:get_env(tls_disable_client, false) of
	false ->
	    Timeout = get_timeout(Proto),
	    case catch gen_server:call(tcp_dispatcher, {get_socket, Dst}, Timeout) of
		{error, E} ->
		    {error, E};
		{ok, Socket} ->
		    Socket;
		{'EXIT', Reason} ->
		    {error, Reason}
		end;
	true ->
	    {error, "TLS client disabled"}
    end;
%%
%% Protocol is 'tcp' or 'tcp6'
%% 
get_socket(#sipdst{proto=Proto}=Dst) when Proto == tcp; Proto == tcp6 ->
    Timeout = get_timeout(Dst#sipdst.proto),
    case catch gen_server:call(tcp_dispatcher, {get_socket, Dst}, Timeout) of
	{error, E} ->
	    {error, E};
	{ok, Socket} ->
	    Socket;
	{'EXIT', Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: is_reliable_transport(_SipSocket)
%% Descrip.: Return true. This sipsocket modules transports are
%%           reliable. The meaning of reliable is that they handle
%%           resends automatically, so the transaction layer does not
%%           have to set up timers to resend messages.
%% Returns : true
%%--------------------------------------------------------------------
is_reliable_transport(_) -> true.


%%====================================================================
%% Internal functions
%%====================================================================

get_timeout(tcp) -> 1500;
get_timeout(tcp6) -> 1500;
get_timeout(tls) -> 5000;
get_timeout(tls6) -> 5000.
