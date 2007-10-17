-module(siputil).
-export([linefix/1,
	 printvalue/1,
	 concat_strings/1,
	 generate_tag/0
	]).

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

%%--------------------------------------------------------------------
%% @spec    (In) -> [string()]
%%
%%            In = [string()]
%%
%% @doc     Insanely ineffective concatenation of strings, with CRLFs
%%          in between. Please, don't use this function!
%% @end
%%--------------------------------------------------------------------
concat_strings([]) ->
    [];

concat_strings([[] | B]) ->
    concat_strings(B);

concat_strings([A | B]) ->
    A ++ "\r\n" ++ concat_strings(B).

%%--------------------------------------------------------------------
%% @spec    () ->
%%            Tag
%%
%%            Tag = string()
%%
%% @doc     Generate a string that might be used as To: tag in
%%          responses we create. This means it includes at least 32
%%          bits of randomness (specified by RFC3261 #19.3).
%% @end
%%--------------------------------------------------------------------
generate_tag() ->
    %% Erlang guarantees that subsequent calls to now() generate increasing values (on the same node).
    {Megasec, Sec, Microsec} = now(),
    In = lists:concat([node(), Megasec * 1000000 + Sec, 8, $., Microsec]),
    Out1 = siprequest:make_base64_md5_token(In),
    Out = http_util:to_lower(Out1),	%% tags are case-insensitive
    %% don't make the tag longer than it has to be.
    "yxa-" ++ string:substr(Out, 1, 9).
