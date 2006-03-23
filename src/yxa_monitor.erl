%%%-------------------------------------------------------------------
%%% File    : yxa_monitor.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: YXA application monitor process. Monitors other nodes in
%%%           the network (for logging purposes) and shuts down the
%%%           local node if it gets into serious problems (like Mnesia
%%%           going away for an extended period of time).
%%%
%%% Created : 16 Mar 2006 by Fredrik Thulin <ft@it.su.se>
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
%% Function: start_link(AppModule, MnesiaTables)
%%           AppModule = atom(), YXA application module
%%           MnesiaTables = list() of atom(), Mnesia tables we require
%%                          to be present
%% Descrip.: Starts the server
%%--------------------------------------------------------------------
start_link(AppModule, MnesiaTables) when is_atom(AppModule), is_list(MnesiaTables) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [AppModule, MnesiaTables], []).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([AppModule, MnesiaTables])
%%           AppModule = atom(), YXA application module
%%           MnesiaTables = list() of atom(), Mnesia tables we require
%%                          to be present
%% Descrip.: Initiates the server.
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([AppModule, MnesiaTables]) ->
    %% Tell net_kernel to notify us of nodes coming up or going down
    ok = net_kernel:monitor_nodes(true, [{node_type, visible},
					 nodedown_reason]
				 ),
    State = #state{appmodule		= AppModule,
		   mnesia_tables	= MnesiaTables
		  },
    {ok, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_call(Msg, _From, State) ->
    logger:log(error, "YXA monitor: Received unknown gen_server call : ~p", [Msg]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast(Msg, State) ->
    logger:log(error, "YXA monitor: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
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
    


handle_info(timeout, State) ->
    %%% XXX CHECK FOR MNESIA TABLES HERE
    {noreply, State, ?TIMEOUT};

handle_info(Unknown, State) ->
    logger:log(error, "YXA monitor: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
        normal -> logger:log(debug, "YXA monitor: terminating normally");
        shutdown -> logger:log(debug, "YXA monitor: shutting down");
        _ -> logger:log(error, "YXA monitor: terminating : ~p", [Reason])
    end,
    Reason.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
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
