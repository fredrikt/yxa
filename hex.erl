-module(hex).
-export([to/1, to/2, from/1]).

to(_, 0) ->
    [];
to(Number, N) when integer(Number), N > 0 ->
    lists:append(to(Number div 16, N - 1), [lists:nth(Number rem 16 + 1,
						      [$0, $1, $2, $3,
						       $4, $5, $6, $7,
						       $8, $9, $a, $b,
						       $c, $d, $e, $f])]).

to(Binary) when binary(Binary) ->
    A = binary_to_list(Binary),
    B = lists:map(fun(C) ->
			  to(C, 2)
		  end, A),
    lists:flatten(B).

from([], N) ->
    N;

from([C | String], N) when C >= $0, C =< $9 ->
    M = N * 16 + (C - $0),
    from(String, M);

from([C | String], N) when C >= $a, C =< $f ->
    M = N * 16 + (C - $a + 10),
    from(String, M);

from([C | String], N) when C >= $A, C =< $F ->
    M = N * 16 + (C - $A + 10),
    from(String, M).

from(String) ->
    from(String, 0).
