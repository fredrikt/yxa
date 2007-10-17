%%%-------------------------------------------------------------------
%%% File    : dialog_server.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Supervised process that all dialog controllers link to,
%%%           which cleans away the dialog state when the dialog
%%%           controller terminates.
%%%
%%% @since    14 Feb 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(dialog_server).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0
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
-record(state, {
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, dialog_server).
-define(TIMEOUT, 61 * 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ([]) ->
%%            {ok, State}          |
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% This is a system process that traps EXIT signals from dialog controllers
    process_flag(trap_exit, true),
    sipdialog:init(),
    State = #state{},
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
%% @spec    (Unknown, From, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "Dialog server: Received unknown gen_server call : ~p", [Unknown]),
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
    logger:log(error, "Dialog server: Received unknown gen_server cast : ~p", [Unknown]),
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
%% @spec    (timeout, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Check for expired dialogs.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    sipdialog:handle_expired_dialogs(?TIMEOUT div 1000),
    {noreply, State, ?TIMEOUT};


%%--------------------------------------------------------------------
%% @spec    ({'EXIT', Pid, Reason}, State) ->
%%            {noreply, State, Timeout::integer()}
%%
%%            Pid    = pid()
%%            Reason = normal | term()
%%
%% @doc     Trap exit signals from socket handlers and act on them.
%%          Log if they exit with an error, and remove them from our
%%          list of existing sockets.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
	normal -> logger:log(debug, "Dialog server: Received normal exit-signal from process ~p", [Pid]);
	shutdown -> logger:log(debug, "Dialog server: Received 'shutdown' exit-signal from process ~p", [Pid]);
	killed -> logger:log(debug, "Dialog server: Received 'killed' exit-signal from process ~p", [Pid]);
	_ -> logger:log(error, "Dialog server: =ERROR REPORT==== Received non-normal exit signal "
			"from process ~p :~n~p", [Pid, Reason])
    end,
    DebugLevel =
	case (Reason == normal orelse Reason == shutdown orelse Reason == killed) of
	    true -> debug;
	    false -> error
	end,
    case sipdialog:delete_using_pid(Pid) of
	{ok, DialogId} ->
	    logger:log(DebugLevel, "Dialog server: Received exit-signal from dialog controller ~p, "
		       "deleted dialog ~p.", [Pid, DialogId]);
	nomatch ->
	    logger:log(DebugLevel, "Dialog server: Received exit-signal from ~p that was not a "
		       "registered dialog controller", [Pid])
    end,
    {noreply, State, ?TIMEOUT};

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown info.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(Unknown, State) ->
    logger:log(error, "Dialog server: Received unknown gen_server info : ~p", [Unknown]),
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
        normal -> logger:log(debug, "Dialog server: terminating normally");
        shutdown -> logger:log(debug, "Dialog server: shutting down");
        _ -> logger:log(error, "Dialog server: terminating : ~p", [Reason])
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
