-module(sipsocket).
-export([start/1, check_alive/2, send/4, is_reliable_transport/1, get_socket/3,
	 sipproto2str/1, via2sipsocketprotocol/1, sipproto2viastr/1,
	 is_good_socket/1]).

-include("sipsocket.hrl").

start(Port) when integer(Port) ->
    UDP = sipserver:safe_spawn(sipsocket_udp, start, [Port]),
    TCP = sipserver:safe_spawn(sipsocket_tcp, start, [Port]),
    [{sipsocket_udp, UDP}, {sipsocket_tcp, TCP}].

check_alive(_, []) ->
    ok;
check_alive(Port, [{Module, Pid} | T]) ->
    case util:safe_is_process_alive(Pid) of
	{false, Pid} ->
	    logger:log(error, "Sipsocket: Module ~p pid ~p not alive, attempting restart.",
			[Module, Pid]),
	    apply(Module, start, [Port]);
	{true, Pid} ->
	    true
    end,
    check_alive(Port, T).

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
