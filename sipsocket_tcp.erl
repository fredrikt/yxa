-module(sipsocket_tcp).
-export([start/1, send/4, is_reliable_transport/1, get_socket/2]).

-include("socketlist.hrl").
-include("sipsocket.hrl").

start(Port) when integer(Port) ->
    tcp_dispatcher:start_link(Port).
    
send(SipSocket, SendToHost, PortInt, Message) when record(SipSocket, sipsocket) ->
    SPid = SipSocket#sipsocket.pid,
    case catch gen_server:call(SPid, {send, {SendToHost, PortInt, Message}}, 1500) of
	{send_result, Res} ->
	    Res;
	{'EXIT', Reason} ->
	    {error, Reason}
    end.

get_socket(Host, Port) when list(Host), integer(Port) ->
    case catch gen_server:call(tcp_dispatcher, {get_socket, Host, Port}, 1500) of
	{ok, Socket} ->
	    Socket;
	{error, E} ->
	    {error, E};
	{'EXIT', Reason} ->
            {error, Reason}
    end.

is_reliable_transport(_) ->
    true.
