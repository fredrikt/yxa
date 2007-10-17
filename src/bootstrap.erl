%%%-------------------------------------------------------------------
%%% File    : bootstrap.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Initialize Mnesia databases needed for an YXA
%%%           installation. Run through the 'yxa-bootstrap' shell
%%%           script.
%%%
%%% @since    07 Oct 2003 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(bootstrap).

-export([start/0,
	 replica/1
	]).

-define(DB_MODULES, [phone,
		     database_regexproute,
		     database_forward,
		     database_call,
		     cpl_db,
		     database_gruu,
		     database_eventdata
		    ]).

-define(MNESIA_TABLES, [user,
			numbers,
			phone,
			regexproute,
			forward,
			%% call
			cpl_script_graph,
			gruu,
			eventdata
		       ]).


%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Create a first Mnesia database server at the node where
%%          this is run (through the execution of "yxa-bootstrap").
%% @end
%%--------------------------------------------------------------------
start() ->
    io:format("Bootstrapping YXA on node ~p :~n", [node()]),
    ok = create_schema(node()),
    ok = mnesia:start(),

    io:format("* Creating tables on this Mnesia node (~p)~n", [node()]),
    init_db_module(?DB_MODULES, node()),

    io:format("* Updating any pre-existing table definitions~n"),
    ok = table_update:update(),

    io:format("* Stopping Mnesia~n"),
    stopped = mnesia:stop(),
    io:format("~nBootstrapping complete.~n~n"),
    ok.

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

%%--------------------------------------------------------------------
%% @spec    ([Master]) -> ok
%%
%%            Master = string()
%%
%% @doc     Create a second Mnesia database server at the node where
%%          this is run (through the execution of "yxa-bootstrap").
%% @end
%%--------------------------------------------------------------------
replica([Master]) ->
    MasterNode = list_to_atom(Master),
    io:format("Making YXA on node ~p a Mnesia replica node~n", [node()]),

    io:format("* Starting Mnesia~n"),
    ok = mnesia:start(),

    io:format("* Adding master Mnesia db_node ~p~n", [MasterNode]),
    case mnesia:change_config(extra_db_nodes, [MasterNode]) of
	{ok, []} ->
	    %% If Mnesia on the current node already knew to link up with MasterNode,
	    %% change_config says 'ok' but with an empty list. This is however exactly
	    %% the same thing that happens if we fail to connect to the remote node
	    %% because of an distribution mechanism failure so we need to make sure
	    %% we are online...
	    case lists:member(MasterNode, mnesia:system_info(running_db_nodes)) of
		true ->
		    ok;
		false ->
		    erlang:error("Failed connecting to master node - Erlang distribution problem? "
				 "Are you running SSL on the master node but not on this?")
	    end;
	{ok, [MasterNode]} ->
	    ok;
	{error, E} ->
	    erlang:error(E)
    end,

    AllTables = [schema | ?MNESIA_TABLES],

    io:format("* Waiting for tables : ~w~n", [AllTables]),
    ok = mnesia:wait_for_tables(AllTables, 60 * 1000),

    io:format("* Replicating ~p tables :~n", [length(AllTables)]),
    ok = replicate_tables(AllTables),

    io:format("~nReplica created successfully.~n~n"),
    ok.


replicate_tables([H | T]) ->
    case lists:member(node(), mnesia:table_info(H, disc_copies)) of
	true ->
	    io:format("~25w: Table is already present on disc at node ~p~n", [H, node()]);
	false ->
	    case lists:member(node(), mnesia:table_info(H, ram_copies)) of
		true ->
		    io:format("~25w: Changing table from a ram table to a disc table~n", [H]),
		    {atomic, ok} = mnesia:change_table_copy_type(H, node(), disc_copies);
		false ->
		    io:format("~25w: Copying table to this node~n", [H]),
		    {atomic, ok} = mnesia:add_table_copy(H, node(), disc_copies)
	    end
    end,
    replicate_tables(T);
replicate_tables([]) ->
    ok.
