-module(sipsocket).
-export([start/3, reopen_sockets/4, send/4, is_reliable_transport/1, get_socket/4,
	 sipproto2str/1, via2sipsocketprotocol/1, sipproto2viastr/1, get_response_socket/2,
	 associate_transaction_with_socket/2, store_stateless_response_branch/2]).


start(Port, RequestFun, ResponseFun) when integer(Port) ->
    UDP = sipserver:safe_spawn(sipsocket_udp, start, [Port, RequestFun, ResponseFun]),
    TCP = sipserver:safe_spawn(sipsocket_tcp, start, [Port, RequestFun, ResponseFun]),
    [{udp, UDP}, {tcp, TCP}].

reopen_sockets(Sockets, Port, RequestFun, ResponseFun) ->
    % not implemented yet
    timer:sleep(500000),
    reopen_sockets(Sockets, Port, RequestFun, ResponseFun).

send(Socket, SendToHost, PortInt, Message) ->
    {Module, _, _} = Socket,
    apply(Module, send, [Socket, SendToHost, PortInt, Message]).

get_socket(SipProto, Host, Port, Data) ->
    apply(SipProto, get_socket, [Host, Port, Data]).

get_response_socket(SipProto, Data) ->
    apply(SipProto, get_response_socket, [Data]).

associate_transaction_with_socket(Socket, TransactionId) ->
    {Module, _, _} = Socket,
    apply(Module, associate_transaction_with_socket, [TransactionId, Socket]).

store_stateless_response_branch(Socket, Branch) ->
    {Module, _, _} = Socket,
    apply(Module, store_stateless_response_branch, [Branch, Socket]).

is_reliable_transport(Socket) ->
    {Module, _, _} = Socket,
    apply(Module, is_reliable_transport, [Socket]).

sipproto2str(sipsocket_tcp) -> "tcp";
sipproto2str(sipsocket_udp) -> "udp".

sipproto2viastr(sipsocket_tcp) -> "SIP/2.0/TCP";
sipproto2viastr(sipsocket_udp) -> "SIP/2.0/UDP".

via2sipsocketprotocol("SIP/2.0/TCP") -> sipsocket_tcp;
via2sipsocketprotocol("SIP/2.0/UDP") -> sipsocket_udp.

