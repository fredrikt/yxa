-module(siputil).
-export([linefix/1, printvalue/1, concat_strings/1]).

linefix(In) ->
    linefix2(In, []).

%% 13          -> 10
%% 10          -> 10
%% 13 10 13 10 -> 10 10
%% 10 10       -> 10 10
%% 13 13       -> 10 10

%% header-body separator, stop processing (leave body untouched)
linefix2([13, 10, 13, 10 | T], Res) ->
    L1 = [10, 10 | Res],
    L = lists:foldl(fun(H, Acc) ->
			    [H|Acc]
		    end, L1, T),
    lists:reverse(L);
linefix2([10, 10 | T], Res) ->
    L1 = [10, 10 | Res],
    L = lists:foldl(fun(H, Acc) ->
			    [H|Acc]
		    end, L1, T),
    lists:reverse(L);
linefix2([13, 13 | T], Res) ->
    L1 = [10, 10 | Res],
    L = lists:foldl(fun(H, Acc) ->
			    [H|Acc]
		    end, L1, T),
    lists:reverse(L);


linefix2([13, 10 | T], Res) ->
    linefix2(T, [10 | Res]);


linefix2([13 | T], Res)  ->
    linefix2(T, [10 | Res]);

linefix2([10 | T], Res) ->
    linefix2(T, [10 | Res]);


linefix2([H | T], Res) ->
    %% any char
    linefix2(T, [H | Res]);

linefix2([], Res) ->
    %% no input left, return
    lists:reverse(Res).


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
