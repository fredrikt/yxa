-module(bootstrap).
-export([start/1]).

start([AdminPassword]) ->
    io:format("Bootstrapping Yxa on node ~p :~n", [node()]),
    ok = create_schema(node()),
    ok = mnesia:start(),

    io:format("* Creating tables on this Mnesia node (~p)~n", [node()]),
    init_db_module([phone,
		    database_regexproute,
		    database_forward,
		    database_call
		   ], node()),

    io:format("* Updating any pre-existing table definitions~n"),
    ok = table_update:update(),

    io:format("* Creating admin-user~n"),
    phone:insert_user("admin", AdminPassword, [admin], []),

    ok = mnesia:stop(),
    io:format("~nBootstrapping complete.~n~n").

create_schema(Node) ->
    case mnesia:create_schema([Node]) of
	ok ->
	    io:format("* Created Mnesia schema on node ~p~n", [Node]);
	{error, {Node, {already_exists, Node}}} ->
	    io:format("* Mnesia schema already existed on node ~p~n", [Node]);
	E ->
	    io:format("~nFailed creating Mnesia schema on node ~p :~n~p---~nExiting.",
		      [Node, E]),
	    erlang:halt(1)
    end,
    ok.

init_db_module([H | T], Node) ->
    io:format("  + Creating table '~p'~n", [H]),
    H:create([Node]),
    init_db_module(T, Node);
init_db_module([], _Node) ->
    ok.
