-module(siphost).
-export([myip/0, makeip/1]).

makeip(AddrIn) ->
    {Addr1, Addr2, Addr3, Addr4} = AddrIn,
    integer_to_list(Addr1) ++ "." ++
	integer_to_list(Addr2) ++ "." ++
	integer_to_list(Addr3) ++ "." ++
	integer_to_list(Addr4).


myip() ->
    [A | _] = get_iplist(),
    A.

get_if([]) ->
    [];

get_if([A | R]) ->
    {ok, B} = inet:ifget(A, [addr, flags]),
    case lists:member(loopback,keylist:fetch(flags, B)) of
	true ->
	    get_if(R);
	false ->
	    Addr = makeip(keylist:fetch(addr, B)),
	    [Addr | get_if(R)]
    end.

get_iplist() ->
    {ok, If} = inet:getiflist(),
    get_if(If).
