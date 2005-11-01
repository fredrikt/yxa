-module(siphost).
-export([myip/0, myip_list/0, makeip/1]).

makeip({A1, A2, A3, A4}) ->
    integer_to_list(A1) ++ "." ++
	integer_to_list(A2) ++ "." ++
	integer_to_list(A3) ++ "." ++
	integer_to_list(A4);
makeip({A1, A2, A3, A4, A5, A6, A7, A8}) ->
    A = inet_parse:ntoa({A1, A2, A3, A4, A5, A6, A7, A8}),
    "[" ++ httpd_util:to_lower(A) ++ "]".

myip() ->
    case get_iplist() of
	[A | _] ->
	    A;
	[] ->
	    "127.0.0.1"
    end.

%% XXX make this return all addresses, currently IPv6 addresses
%% are not returned!
myip_list() ->
    case get_iplist() of
	[] ->
	    ["127.0.0.1"];
	L ->
	    L
    end.

get_if(L) ->
    get_if(L, []).

get_if([], Res) ->
    Res;

get_if([H | T], Res) ->
    {ok, B} = inet:ifget(H, [addr, flags]),
    {value, {flags, Flags}} = lists:keysearch(flags, 1, B),
    case usable_if(Flags) of
	false ->
	    %% Ignore unusable interfaces
	    get_if(T, Res);
	true ->
	    case lists:keysearch(addr, 1, B) of
		{value, {addr, Addr}} ->
		    get_if(T, [makeip(Addr) | Res]);
		_ ->
		    %% Interface has no address, might happen on BSD
		    get_if(T, Res)
	    end
    end.

%% Interface must be up and not loopback to be usable.
usable_if(Flags) ->
    not lists:member(loopback, Flags)
	and lists:member(up, Flags).

get_iplist() ->
    {ok, If} = inet:getiflist(),
    get_if(If).
