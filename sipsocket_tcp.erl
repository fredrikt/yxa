-module(sipsocket_tcp).
-export([start/1, send/5, is_reliable_transport/1, get_socket/3]).

-include("socketlist.hrl").
-include("sipsocket.hrl").

start(Port) when integer(Port) ->
    tcp_dispatcher:start_link(Port).

send(SipSocket, Proto, _Host, Port, _Message) when record(SipSocket, sipsocket), integer(Port), SipSocket#sipsocket.proto /= Proto ->
    {error, "Protocol mismatch"};
send(SipSocket, _Proto, Host, Port, Message) when record(SipSocket, sipsocket), integer(Port) ->
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

get_socket(Proto, Host, Port) when atom(Proto), list(Host), integer(Port), Proto == tls; Proto == tls6 ->
    case sipserver:get_env(enable_experimental_tls, false) of
	true ->
	    Timeout = get_timeout(Proto),
	    case catch gen_server:call(tcp_dispatcher, {get_socket, Proto, Host, Port}, Timeout) of
		{error, E} ->
		    {error, E};
		{ok, Socket} ->
		    Socket;
		{'EXIT', Reason} ->
		    {error, Reason}
		end;
	_ ->
	    {error, "TLS (still experimental) is not enabled"}
    end;

get_socket(Proto, Host, Port) when atom(Proto), list(Host), integer(Port) ->
    Timeout = get_timeout(Proto),
    case catch gen_server:call(tcp_dispatcher, {get_socket, Proto, Host, Port}, Timeout) of
	{error, E} ->
	    {error, E};
	{ok, Socket} ->
	    Socket;
	{'EXIT', Reason} ->
            {error, Reason}
    end.

is_reliable_transport(_) -> true.

%% Internal functions

get_timeout(tcp) -> 1500;
get_timeout(tcp6) -> 1500;
get_timeout(tls) -> 5000;
get_timeout(tls6) -> 5000.

