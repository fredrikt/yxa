-module(siputil).
-export([linefix/1, printvalue/1, concat_strings/1]).

linefix([]) ->
    [];

linefix([13]) ->
    [10];

linefix([10]) ->
    [10];

linefix([A]) ->
    [A];




linefix([13, 10, 13, 10 | C]) ->
    [10, 10 | C];

linefix([10, 10 | C]) ->
    [10, 10 | C];

linefix([13, 13 | C]) ->
    [10, 10 | C];




linefix([13, 10 | C]) ->
    [10 | linefix(C)];

linefix([13, B | C]) ->
    [10 | linefix([B | C])];

linefix([A, 10 | C]) ->
    [A | linefix([10 | C])];

linefix([A, B | C]) ->
    [A | linefix([B | C])].

printvalue([]) ->
    [];

printvalue([A]) ->
    A;

printvalue([A | B]) ->
    A ++ "," ++ printvalue(B).

concat_strings([]) ->
    [];

concat_strings([[] | B]) ->
    concat_strings(B);

concat_strings([A | B]) ->
    A ++ "\r\n" ++ concat_strings(B).
