-module(bootstrap).
-export([start/1]).

start([AdminPassword]) ->
    io:format("Bootstrapping Yxa on node ~p~n", [node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    io:format("Creating tables on this Mnesia node (~p)~n", [node()]),
    phone:create([node()]),
    database_regexproute:create([node()]),
    database_forward:create([node()]),
    database_call:create([node()]),
    io:format("Creating admin-user~n"),
    phone:insert_user("admin", AdminPassword, [admin], []).
