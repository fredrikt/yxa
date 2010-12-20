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
 
    ok = my_wait_for_tables(?MNESIA_TABLES, 10000),

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
    io:format("  + Creating table(s) using module '~p'~n", [H]),
    case H:create([Node]) of
	{atomic, ok} ->
	    init_db_module(T, Node);
	{aborted, {already_exists, _Table}} ->
	    %% not an error
	    init_db_module(T, Node);
	E ->
	    io:format("~nERROR: ~p:create/1 FAILED : ~p~n~n", [H, E]),
	    erlang:error("failed creating table(s)")
    end;
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
		    io:format("~nERROR: Failed connecting to master node - Erlang distribution problem? "
			      "Are you running SSL on the master node but not on this?~n~n"),
		    erlang:error("connect to master node failed")
	    end;
	{ok, [MasterNode]} ->
	    ok;
	{error, E} ->
	    erlang:error(E)
    end,

    %% basic sanity check of MasterNode
    case check_node_has_tables(MasterNode, [phone, user, numbers, regexproute]) of
	true ->
	    ok;
	false ->
	    io:format("~nERROR: Master node ~p does not have disc_copies of basic YXA tables, "
		      "is it really an YXA db node?~n~n", [MasterNode]),
	    erlang:error("bad master node")
    end,

    AllTables = [schema | ?MNESIA_TABLES],
    %% Only wait for tables present on MasterNode. This makes it possible to upgrade
    %% an YXA installation to a new version with more tables by adding new replicas.
    %% This might not be safe to do though, but having the option is nice. Be warned!
    WaitTables = lists:filter(fun(T) ->
				      check_node_has_tables(MasterNode, [T])
			      end, AllTables),

    ok = my_wait_for_tables(WaitTables, 60 * 1000),

    io:format("* Replicating ~p tables :~n", [length(WaitTables)]),
    ok = replicate_tables(WaitTables),

    io:format("~nReplica created successfully.~n~n"),
    ok.

replicate_tables([H | T]) ->
    case check_node_has_tables(node(), [H]) of
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

%%--------------------------------------------------------------------
%% @spec    (Node, Tables) -> bool()
%%          Node = atom()
%%          Tables = [atom()]
%%
%% @doc     Checks if Node has disc copies of a list of Mnesia tables.
%% @hidden
%% @end
%%--------------------------------------------------------------------
check_node_has_tables(_Node, []) ->
    true;
check_node_has_tables(Node, [H | T]) ->
    L =
	try mnesia:table_info(H, disc_copies) of
	    L1 -> L1
	catch
	    exit:
	      {aborted, {no_exists, H, disc_copies}} ->
		[]
	end,
    case lists:member(Node, L) of
	true ->
	    check_node_has_tables(Node, T);
	false ->
	    false
    end.


%%--------------------------------------------------------------------
%% @spec    (Tables, Timeout) -> ok | timeout | error
%%          Tables = [atom()]
%%          Timeout = integer() "timeout in milliseconds"
%%
%% @doc     Perform mnesia:wait_for_tables/2 on one table at a time,
%%          with pretty-printed output for each table.
%% @hidden
%% @end
%%--------------------------------------------------------------------
my_wait_for_tables(Tables, Timeout) when is_list(Tables), is_integer(Timeout) ->
    io:format("* Waiting for ~p tables : ", [length(Tables)]),
    my_wait_for_tables2(Tables, Timeout, 0).

my_wait_for_tables2([], _Timeout, _Count) ->
    io:format("~n"),
    ok;
my_wait_for_tables2([H | T], Timeout, Count) ->
    case Count of
	0 -> io:format("~p", [H]);
	_ -> io:format(", ~p", [H])
    end,
    case mnesia:wait_for_tables([H], Timeout) of
	ok ->
	    my_wait_for_tables2(T, Timeout, Count + 1);
	{timeout, _Bad_Tab_List} ->
	    io:format(" timeout!~n~n"),
	    timeout;
	{error, Reason} ->
	    io:format(" error ~p~n~n", [Reason]),
	    error
    end.
    
    
