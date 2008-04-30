%%%-------------------------------------------------------------------
%%% File    : spocp.erl
%%% @author   Fredrik <ft@it.su.se>
%%% @doc      SPOCP client in Erlang.
%%% @since    26 Sep 2006 by Fredrik <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(spocp).

-export([
	 term_to_sexpr/1,
	 term_to_wireformat/1,

	 test/0
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%====================================================================
%% Exported functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (In) -> string()
%%
%% @doc     Turn an erlang term into a SPOCP s-expression. No error
%%          trapping is done.
%% @end
%%--------------------------------------------------------------------
term_to_sexpr(In) ->
    lists:flatten(term_to_sexpr2(In)).

term_to_sexpr2(List) when is_list(List) ->
    case is_spocp_string(List) of
	true ->  List;
	false -> "(" ++ join([term_to_sexpr2(Elem) || Elem <- List], " ") ++ ")"
    end;
term_to_sexpr2(Tuple) when is_tuple(Tuple) ->
    term_to_sexpr2(tuple_to_list(Tuple));
term_to_sexpr2(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
term_to_sexpr2(Atom) when is_atom(Atom) ->
    atom_to_list(Atom).


%%--------------------------------------------------------------------
%% @spec    (In) -> string()
%%
%% @doc     Turn an erlang term into a SPOCP wire formatted string.
%% @end
%%--------------------------------------------------------------------
term_to_wireformat(In) ->
    lists:flatten(term_to_wireformat2(In)).

term_to_wireformat2(List) when is_list(List) ->
    This =
	case is_spocp_string(List) of
	    true ->  List;
	    false -> "(" ++ [term_to_wireformat2(Elem) || Elem <- List] ++ ")"
    end,
    lv_encode(lists:flatten(This));
term_to_wireformat2(Tuple) when is_tuple(Tuple) ->
    term_to_wireformat2(tuple_to_list(Tuple));
term_to_wireformat2(Integer) when is_integer(Integer) ->
    lv_encode(integer_to_list(Integer));
term_to_wireformat2(Atom) when is_atom(Atom) ->
    lv_encode(atom_to_list(Atom)).


%%====================================================================
%% Internal functions
%%====================================================================


is_spocp_string([H | T]) when H >= $a, H =< $z ->
    is_spocp_string(T);
is_spocp_string([H | T]) when H >= $A, H =< $Z ->
    is_spocp_string(T);
is_spocp_string([H | T]) when H >= $0, H =< $9 ->
    is_spocp_string(T);
is_spocp_string([H | T]) ->
    case lists:member(H, "+_-.:/%") of
	true ->
	    is_spocp_string(T);
	false ->
	    false
    end;
is_spocp_string([]) ->
    true.

join([], _Separator) ->
    [];
join([A], _Separator) ->
    A;
join([String | Rest], Separator) ->
    String ++ Separator ++ join(Rest, Separator).

lv_encode(String) when is_list(String) ->
    lists:concat([length(String), ":", String]).

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
test() ->

    Term1 =
	{sip, [
	       {from, [{uid, "ft"},
		       {realm, "example.net"}
		      ]},
	       {to, [{pstn, "+46701234567"}]}
	      ]
	},
    Term1_SExpr = "(sip ((from ((uid ft) (realm example.net))) (to ((pstn +46701234567)))))",
    Term1_Wire = x,

    %% term_to_sexpr(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "term_to_sexpr/1 - 1"),
    Term1_SExpr = term_to_sexpr(Term1),

    %% term_to_wireformat(In)
    autotest:mark(?LINE, "term_to_wireformat/1 - 1"),
    Term1_Wire = term_to_wireformat(Term1),

    autotest:mark(?LINE, "term_to_wireformat/1 - 1"),
    Term1_Wire = term_to_wireformat(Term1_SExpr),

    ok.
