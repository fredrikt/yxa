%%%-------------------------------------------------------------------
%%% File    : yxa_monitor.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      YXA application monitor process. Monitors other nodes in
%%%           the network (for logging purposes) and shuts down the
%%%           local node if it gets into serious problems (like Mnesia
%%%           going away for an extended period of time).
%%%
%%% @since    16 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(yxa_monitor).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/2
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%% @type state() = #state{}.
%%                 no description
-record(state, {appmodule,		%% atom(), YXA application module
		mnesia_tables		%% list() of atom(), Mnesia tables we require to be present
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, yxa_monitor).
-define(TIMEOUT, 10 * 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (AppModule, MnesiaTables) -> term()
%%
%%            AppModule    = atom() "YXA application module"
%%            MnesiaTables = [atom()] "Mnesia tables we require to be present"
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(AppModule, MnesiaTables) when is_atom(AppModule), is_list(MnesiaTables) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {AppModule, MnesiaTables}, []).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({AppModule, MnesiaTables}) ->
%%            {ok, State}          |
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%
%%            AppModule    = atom() "YXA application module"
%%            MnesiaTables = [atom()] "Mnesia tables we require to be present"
%%
%% @doc     Initiates the server.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({AppModule, MnesiaTables}) ->
    %% Tell net_kernel to notify us of nodes coming up or going down
    ok = net_kernel:monitor_nodes(true, [{node_type, visible},
					 nodedown_reason]
				 ),
    State = #state{appmodule		= AppModule,
		   mnesia_tables	= MnesiaTables
		  },
    {ok, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({add_mnesia_tables, Tables}, From, State) ->
%%            {reply, ok, NewState, Timeout::integer()}
%%
%%            Tables = [atom()]
%%
%% @doc     Add more tables to the list of Mnesia tables we should
%%          look for.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({add_mnesia_tables, Tables}, _From, State) when is_list(Tables) ->
    NewL = lists:usort(Tables ++ State#state.mnesia_tables),
    {reply, ok, State#state{mnesia_tables = NewL}, ?TIMEOUT};


%%--------------------------------------------------------------------
%% @spec    (Unknown, From, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "YXA monitor: Received unknown gen_server call : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec    handle_cast(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown cast.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "YXA monitor: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% @spec    handle_info(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling all non call/cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({nodeup, Node, InfoList}, State) ->
%%            {noreply, State, Timeout::integer()}
%%
%%            Node     = atom()
%%            InfoList = [{Key, Value}]
%%            Key      = atom()
%%            Value    = term()
%%
%% @doc     Handles a 'node up' message.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({nodeup, Node, InfoList}, State) ->
    %% XXX change to debug level
    logger:log(normal, "YXA monitor: Node up: ~p", [Node]),
    case InfoList of
	[{node_type, visible}] ->
	    ok;
	_ ->
	    logger:log(debug, "YXA monitor: Node ~p info : ~p", [Node, InfoList])
    end,
    {noreply, State, ?TIMEOUT};

%%--------------------------------------------------------------------
%% @spec    ({nodedown, Node, InfoList}, State) ->
%%            {noreply, State, Timeout::integer()}
%%
%%            Node     = atom()
%%            InfoList = [{Key, Value}]
%%            Key      = atom()
%%            Value    = term()
%%
%% @doc     Handles a 'node down' message.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({nodedown, Node, InfoList}, State) ->
    {LogLevel, DownReason} =
	case lists:keysearch(nodedown_reason, 1, InfoList) of
	    {value, {nodedown_reason, Reason}} ->
		nodedown_reason(Reason);
	    false ->
		%% we should always get nodedown_reason - strange
		{normal, "no reason given"}
	end,
    logger:log(LogLevel, "YXA monitor: Node down: ~p (~s)", [Node, DownReason]),
    I2 = InfoList -- [{node_type, visible}],
    logger:log(debug, "YXA monitor: Node ~p info : ~p", [Node, I2]),
    {noreply, State, ?TIMEOUT};


%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Currently does nothing. Should check periodically for
%%          resources we need, like Mnesia tables.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    %%% XXX CHECK FOR MNESIA TABLES HERE
    {noreply, State, ?TIMEOUT};

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown info.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(Unknown, State) ->
    logger:log(error, "YXA monitor: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
        normal -> logger:log(debug, "YXA monitor: terminating normally");
        shutdown -> logger:log(debug, "YXA monitor: shutting down");
        _ -> logger:log(error, "YXA monitor: terminating : ~p", [Reason])
    end,
    Reason.

%%--------------------------------------------------------------------
%% @spec    (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Returns: {LogLevel, Reason}
%%          LogLevel = debug | normal | error
%%          Reason   = string()
nodedown_reason(connection_setup_failed) ->
    {normal, "connection setup failed (different cookie?)"};
nodedown_reason(no_network) ->
    %% XXX perhaps we should shut down this node on this reason?
    {normal, "no network available"};
nodedown_reason(net_kernel_terminated) ->
    %% XXX perhaps we should shut down this node on this reason?
    {normal, "the net_kernel process terminated"};
nodedown_reason(shutdown) ->
    {normal, "unspecified connection shutdown"};
nodedown_reason(connection_closed) ->
    {normal, "the connection was closed"};
nodedown_reason(disconnect) ->
    {debug, "the connection was disconnected (forced from the current node)"};
nodedown_reason(net_tick_timeout) ->
    {normal, "net tick timeout"};
nodedown_reason(send_net_tick_failed) ->
    %% XXX change to debug level? Perhaps not.
    {normal, "failed to send net tick over the connection"};
nodedown_reason(get_status_failed) ->
    {normal, "status information retrieval from the Port holding the connection failed"};
nodedown_reason(Unknown) ->
    {normal, io_lib:format("unknown reason : ~p", [Unknown])}.
