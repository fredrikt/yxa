
%% this module handles hex data:
%% * hex string to integer
%% * integer to hex string
%% *
%%
%%--------------------------------------------------------------------

-module(hex).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 to/1,
	 to/2,
	 to_hex_string/1,
	 to_int/1,
	 from/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Number, N) -> string()
%%
%%            Number = integer()
%%            N      = integer()
%%
%% @doc     Convert Number to a hex string.
%% @end
%%--------------------------------------------------------------------
to(_, 0) ->
    [];
to(Number, N) when is_integer(Number), N > 0 ->
    lists:append(to(Number div 16, N - 1), [lists:nth(Number rem 16 + 1,
						      [$0, $1, $2, $3,
						       $4, $5, $6, $7,
						       $8, $9, $a, $b,
						       $c, $d, $e, $f])]).

%%--------------------------------------------------------------------
%% @spec    (Binary) -> string()
%%
%%            Binary = binary()
%%
%% @doc     Convert a binary (for example the result of erlang:md5/1)
%%          to a hex string.
%% @end
%%--------------------------------------------------------------------
to(Binary) when is_binary(Binary) ->
    A = binary_to_list(Binary),
    B = lists:map(fun(C) ->
			  to(C, 2)
		  end, A),
    lists:flatten(B).


%%--------------------------------------------------------------------
%% @spec    (Int) ->
%%            string() "containing $0-$F, upper case is used for $A-$F"
%%
%%            Int = integer()
%%
%% @doc     convert Int into string() in hex encoding
%% @end
%%--------------------------------------------------------------------
%% the basic idea of this function is to alway take the least
%% significant (right) 4 bits (hex number) and add as a hex char to
%% the ones already processed in HexString

%% this function is not defined for negative numbers
to_hex_string(Int) when is_integer(Int), Int >= 0 ->
    Mask = 2#1111,
    to_hex_string(Int bsr 4, [to_hex_char(Int band Mask)] ).

to_hex_string(0, HexString) when is_list(HexString) ->
    HexString;
to_hex_string(IntRest, HexString) when is_integer(IntRest), is_list(HexString) ->
    Mask = 2#1111,
    to_hex_string(IntRest bsr 4, [to_hex_char(IntRest band Mask) | HexString] ).

%%--------------------------------------------------------------------
%% @spec    (Int) -> integer() "$0-$F, upper case is used for $A-$F"
%%
%%            Int = integer() "0-15"
%%
%% @doc     convert Int into char() in hex encoding
%% @end
%%--------------------------------------------------------------------
to_hex_char(Int) when (Int >= 0), (Int =< 9) ->
    $0 + Int;
to_hex_char(Int) when (Int >= 10), (Int =< 15) ->
    $A + Int -10.


%%--------------------------------------------------------------------
%% @spec    (String) -> integer()
%%
%%            String = string() "a numeric string of hex values (0-9, A-F and a-f can be used)"
%%
%% @equiv   from(String)
%% @end
%%--------------------------------------------------------------------
to_int(String) ->
    from(String, 0).

%%--------------------------------------------------------------------
%% @spec    (String) -> integer()
%%
%%            String = string() "a numeric string of hex values (0-9, A-F and a-f can be used)"
%%
%% @doc     Convert hex string into an integer.
%% @end
%%--------------------------------------------------------------------
from(String) ->
    from(String, 0).

%% N keeps the current acumulated value, each new char encountered when
%% scaning to the right, is added and N is shifted 4 bits to the left
%% Note: "N * 16" could be replaced by "N bsl 4"

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


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
-ifdef( YXA_NO_UNITTEST ).
test() ->
    {error, "Unit test code disabled at compile time"}.

-else.

test() ->

    %% to(Binary)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "to/1 - 1"),
    "000120406080ff" = to(<<0,1,32,64,96,128,255>>),

    %% to(Number, N)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "to/2 - 1"),
    [] = to("foo", 0),

    %% to_hex_string(Int)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "to_hex_string/1 - 1"),
    "FFFF" = to_hex_string(65535),

    autotest:mark(?LINE, "to_hex_string/1 - 2"),
    "123456789" = to_hex_string(4886718345),

    %% to_int(String)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "to_int/1 - 1"),
    65534 = to_int("fffe"),

    %% from(String)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "from/1 - 1"),
    51966 = from("cafe"),

    autotest:mark(?LINE, "from/1 - 2"),
    51966 = from("CaFE"),

    autotest:mark(?LINE, "from/1 - 3"),
    4886718345 = from("123456789"),

    ok.

-endif.
