%%--------------------------------------------------------------------
%%% File     : stack_monitor.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Graphical real-time monitor for YXA stacks.
%%% @since    22 Oct 2004 by Fredrik <ft@it.su.se>
%%% @end
%%--------------------------------------------------------------------

-module(stack_monitor).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/0]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("socketlist.hrl").
-include("transactionstatelist.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  gs,
	  win,
	  packer,
	  nodes_lb,
	  refresh_nodes_b,
	  stop_b,
	  quit_b,
	  connections_lb,
	  transactions_lb,
	  current_node_l,
	  current_node
	 }).

%%====================================================================
%% External functions
%%====================================================================

%% Defines for packer placements
%% x axis
-define(LEFTSTART, 1).
-define(LEFTEND, 5).
-define(RIGHTSTART, 7).
-define(RIGHTEND, 7).
%% y axis
-define(TOPROW, 1).
-define(MIDDLEROW, 3).
-define(BOTTOMROW, 5).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
start() ->
    %%process_flag(trap_exit, true),

    GS = gs:start(),
    WH = [{width, 1000}, {height, 500}],

    %% the parent of a top-level window is the gs server
    Win = gs:create(window, GS, [{title, "YXA stack monitor"},
				 {map, true}, {configure, true} | WH]),

    %% Packer
    gs:frame(packer, Win, [{packer_x, [{stretch, 1, 33},
				       {fixed, 10},		%% Spacer
				       {stretch, 1, 33},
				       {fixed, 10},		%% Spacer
				       {stretch, 1, 33},
				       {fixed, 5},		%% Spacer
				       {stretch, 8, 200}
				      ]},
			   {packer_y, [{fixed, 30},
				       {stretch, 3},
				       {fixed, 30},
				       {stretch, 3},
				       {fixed, 30}
				      ]}
			  ]),

    %% "Choose node" label
    gs:label(packer, [{label, {text, "Choose node"}}, {width, 250},
		      {pack_xy, {?LEFTSTART, ?TOPROW}}]),

    %% Node listbox
    NodeLb = gs:listbox(packer, [{doubleclick, true}, {vscroll, right},
				 {pack_x, {?LEFTSTART,?LEFTEND}}, {pack_y, {?TOPROW + 1,4}}]),

    %% Refresh nodes button
    RefreshNodes = gs:button(packer, [{label, {text, "Refresh"}}, {width, 10},
				      {pack_xy, {?LEFTSTART, ?BOTTOMROW}}]),

    %% Stop button
    Stop = gs:button(packer, [{label, {text, "Stop"}}, {enable, false}, {width, 10},
			      {pack_xy, {?LEFTSTART + 2, ?BOTTOMROW}}]),

    %% Quit button
    Quit = gs:button(packer, [{label, {text, "Quit"}}, {width, 10},
			      {pack_xy, {?LEFTEND, ?BOTTOMROW}}]),

    %% "Connections" label
    gs:label(packer, [{label, {text, "Connections"}}, {width, 250},
		      {pack_xy, {?RIGHTSTART, ?TOPROW}}]),

    %% Connections listbox
    ConnectionsLb = gs:listbox(packer, [{vscroll, right},
					{pack_x, {?RIGHTSTART, ?RIGHTEND}},
					{pack_y, {?TOPROW + 1, ?MIDDLEROW - 1}}
				       ]),

    %% "Transactions" label
    gs:label(packer, [{label, {text, "Transactions"}}, {width, 250},
		      {pack_xy, {?RIGHTSTART, ?MIDDLEROW}}]),


    %% Transactions listbox
    TranscationsLb = gs:listbox(packer, [{vscroll, right},
					 {pack_x, {?RIGHTSTART, ?RIGHTEND}},
					 {pack_y, {?MIDDLEROW + 1, ?BOTTOMROW - 1}}
					]),


    %% "Current node" label
    CurrentNodeL = gs:label(packer, [{label, {text, "x"}},
				     {pack_xy, {?RIGHTSTART, ?BOTTOMROW}}]),

    gs:config(packer, WH),
    State1 = #state{gs=GS,
		    win=Win,
		    refresh_nodes_b=RefreshNodes,
		    stop_b=Stop,
		    quit_b=Quit,
		    nodes_lb=NodeLb,
		    connections_lb=ConnectionsLb,
		    transactions_lb=TranscationsLb,
		    current_node_l=CurrentNodeL
		   },
    State2 = refresh_nodes(State1),
    State = clear_current_node(State2),

    %% Refresh nodes listbox every 5 seconds
    timer:send_interval(5 * 1000, self(), refresh_nodes),
    %% Update node data once every second
    timer:send_interval(1 * 1000, self(), update_nodedata),

    loop(State).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
loop(State) when is_record(State, state) ->
    Res9 =
	receive
	    {gs, Id, Event, Data, Args} ->
		case handle_gs(Id, Event, Data, Args, State) of
		    NewState when is_record(NewState, state) -> {ok, NewState};
		    Res -> Res
		end;

	    refresh_nodes ->
		%% Timer event to refresh Nodes listbox
		{ok, refresh_nodes(State)};

	    update_nodedata ->
		%% Timer event to update the displayed data about the
		%% selected node
		NewState1 = refresh_connectionslist(State),
		NewState2 = refresh_transactionslist(NewState1),
		{ok, NewState2};

	    Unknown ->
		io:format("~nUNKNOWN SIGNAL RECEIVED :~n~p~n~n", [Unknown]),
		ok
	end,
    case Res9 of
	quit -> ok;
	{_, NewState9} when is_record(NewState9, state) -> loop(NewState9)
    end.

handle_gs(Id, click, _Data, _Args, State) when is_record(State, state), Id == State#state.refresh_nodes_b ->
    %% Refresh nodes button clicked
    refresh_nodes(State);

handle_gs(Id, click, _Data, _Args, State) when is_record(State, state), Id == State#state.stop_b ->
    %% Stop button clicked
    clear_current_node(State);

handle_gs(Id, click, _Data, _Args, State) when is_record(State, state), Id == State#state.quit_b ->
    %% Quit button clicked
    quit;

handle_gs(Id, doubleclick, _Data, Args, State) when is_record(State, state), Id == State#state.nodes_lb ->
    %% A node was doubleclicked on
    [_, Node, _] = Args,
    %% Enable the stop button
    Stop = State#state.stop_b,
    gs:config(Stop, [{enable, true}]),
    %% Write out the name of the node
    Nstr = io_lib:format("Selected node : ~s", [Node]),
    Nlabel = State#state.current_node_l,
    gs:config(Nlabel, {label, {text, Nstr}}),
    State#state{current_node = list_to_atom(Node)};

%%handle_gs(Id, configure, _Data, [W,H|_], State) when is_record(State, state), Id == State#state.packer ->
handle_gs(_Id, configure, _Data, [W,H|_], State) when is_record(State, state) ->
    gs:config(packer, [{width, W}, {height, H}]), % repack
    State;

handle_gs(Id, destroy, _Data, _Args, State) when is_record(State, state), Id == State#state.win ->
    io:format("YXA stack monitor main window closed, exiting.~n"),
    quit;

handle_gs(Id, Event, Data, Args, State) ->
    io:format("UNKNOWN GS EVENT:~n"
	      "Id    : ~p~n"
	      "Event : ~p~n"
	      "Data  : ~p~n"
	      "Args  : ~p~n"
	      "~n", [Id, Event, Data, Args]),
    State.


%%--------------------------------------------------------------------
%% @spec    (State) ->
%%            NewState
%%
%%            NewState = #state{}
%%
%% @doc     Request to refresh the Nodes listbox.
%% @end
%%--------------------------------------------------------------------
refresh_nodes(State) when is_record(State, state) ->
    NodeLb = State#state.nodes_lb,
    gs:config(NodeLb, [{items, get_nodes()}]),
    State.

%%--------------------------------------------------------------------
%% @spec    () -> result of nodes()
%%
%% @doc     Try to connect to all nodes listed in the users
%%          .hosts.erlang file and return a list of nodes we are
%%          connected to.
%% @end
%%--------------------------------------------------------------------
get_nodes() ->
    %% get hosts
    case net_adm:host_file() of
	{error, _} ->
	    true;
	Hosts when is_list(Hosts) ->
	    Ping = fun(N) -> net_adm:ping(N)
		   end,
	    %% ping all hosts
	    lists:map(Ping, Hosts)
    end,
    nodes().

refresh_connectionslist(State) when is_record(State, state), State#state.current_node /= none ->
    Node = State#state.current_node,
    Lb = State#state.connections_lb,
    {Text9, NewState9} =
	case catch gen_server:call({tcp_dispatcher, Node}, {get_socketlist}) of
	    {ok, Connections} when is_record(Connections, socketlist) ->
		C = io_lib:format("~p entrys in tcp_dispatcher socketlist :",
				  [socketlist:get_length(Connections)]),
		Text = [C, ""] ++ socketlist:monitor_format(Connections),
		{Text, State};
	    U ->
		Ulist = format_unknown_for_listbox(U),
		Text =
		    [io_lib:format("Unknown response from tcp_dispatcher at node ~p :", [Node]),
		     ""] ++ Ulist,
		%% Clear node since we got an error - no point in hammering a node
		%% that can't handle our requests
		NewState = clear_current_node(State),
		{Text, NewState}
	end,
    gs:config(Lb, {items, Text9}),
    NewState9;

refresh_connectionslist(State) when is_record(State, state) ->
    %% No node is selected
    State.

refresh_transactionslist(State) when is_record(State, state), State#state.current_node /= none ->
    Node = State#state.current_node,
    Lb = State#state.transactions_lb,
    {Text9, NewState9} =
	case catch gen_server:call({transactionlayer, Node}, {monitor_get_transactionlist}) of
	    {ok, Transactions} when is_record(Transactions, transactionstatelist) ->
		C = io_lib:format("~p entrys in transactionlayer's list :",
				  [transactionstatelist:get_length(Transactions)]),
		TF = transactionstatelist:monitor_format(Transactions),
		Text = [C, ""] ++ TF,
		{Text, State};
	    U ->
		Ulist = format_unknown_for_listbox(U),
		Text =
		    [io_lib:format("Unknown response from transactionlayer at node ~p :", [Node]),
		     ""] ++ Ulist,
		%% Clear node since we got an error - no point in hammering a node
		%% that can't handle our requests
		NewState = clear_current_node(State),
		{Text, NewState}
	end,
    gs:config(Lb, {items, Text9}),
    NewState9;
refresh_transactionslist(State) when is_record(State, state) ->
    %% No node is selected
    State.

clear_current_node(State) when is_record(State, state) ->
    %% Clear "Connections" listbox
    ConnLb = State#state.connections_lb,
    gs:config(ConnLb, {items, ["No node selected"]}),

    %% Clear "Transactions" listbox
    TranLb = State#state.transactions_lb,
    gs:config(TranLb, {items, ["No node selected"]}),

    %% Disable the stop button
    Stop = State#state.stop_b,
    gs:config(Stop, [{enable, false}]),

    %% Clear "Current node" label
    Nlabel = State#state.current_node_l,
    gs:config(Nlabel, {label, {text, "No node selected"}}),

    State#state{current_node=none}.

format_unknown_for_listbox(U) ->
    %% U is typically an {'EXIT', ...} because something went really wrong -
    %% try to re-format it so that the error is readable in the listbox. This
    %% requires io_lib:format()ting in two steps to get a string that we can
    %% split on newlines
    Ustr1 = io_lib:format("~p", [U]),
    Ustr = lists:flatten(io_lib:format("~s", [Ustr1])),
    Ulist = string:tokens(Ustr, "\n"),
    Ulist.
