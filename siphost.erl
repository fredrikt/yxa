-module(siphost).
-export([myip/0, myip_list/0, makeip/1]).

makeip({A1, A2, A3, A4}) ->
    integer_to_list(A1) ++ "." ++
	integer_to_list(A2) ++ "." ++
	integer_to_list(A3) ++ "." ++
	integer_to_list(A4);
makeip({A1, A2, A3, A4, A5, A6, A7, A8}) ->
    "[" ++
	ipv6ify(A1) ++ ":" ++
	ipv6ify(A2) ++ ":" ++
	ipv6ify(A3) ++ ":" ++
	ipv6ify(A4) ++ ":" ++
	ipv6ify(A5) ++ ":" ++
	ipv6ify(A6) ++ ":" ++
	ipv6ify(A7) ++ ":" ++
	ipv6ify(A8) ++ "]".

ipv6ify(0) ->
    "0";
ipv6ify(A) ->
    hex:to(A, 4).

myip() ->
    [A | _] = get_iplist(),
    A.

%% XXX make this return all addresses, currently IPv6 addresses
%% are not returned!
myip_list() ->
    get_iplist().

get_if([]) ->
    [];

get_if([A | R]) ->
    {ok, B} = inet:ifget(A, [addr, flags]),
    {value, {flags, Flags}} = lists:keysearch(flags, 1, B),
    case lists:member(loopback,Flags) of
	true ->
	    get_if(R);
	false ->
	    {value, {addr, Addr}} = lists:keysearch(addr, 1, B),
	    [makeip(Addr) | get_if(R)]
    end.

get_iplist() ->
    {ok, If} = inet:getiflist(),
    get_if(If).
