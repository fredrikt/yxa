%%%-------------------------------------------------------------------
%%% File    : dialog_server.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Supervised process that all dialog controllers link to,
%%%           which cleans away the dialog state when the dialog
%%%           controller terminates.
%%%
%%% Created : 14 Feb 2006 by Fredrik Thulin <ft@it.su.se>
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
%% Function: start_link()
%% Descrip.: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([])
%% Descrip.: Initiates the server
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    %% This is a system process that traps EXIT signals from dialog controllers
    process_flag(trap_exit, true),
    sipdialog:init(),
    State = #state{},
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

%%--------------------------------------------------------------------
%% Function: handle_call(Unknown, From, State)
%% Descrip.: Unknown call.
%% Returns : {noreply, State, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "Dialog server: Received unknown gen_server call : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_cast(Unknown, State)
%% Descrip.: Unknown cast.
%% Returns : {noreply, State, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "Dialog server: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%% Descrip.: Check for expired dialogs.
%% Returns : {noreply, State, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    sipdialog:handle_expired_dialogs(?TIMEOUT div 1000),
    {noreply, State, ?TIMEOUT};


%%--------------------------------------------------------------------
%% Function: handle_info({'EXIT', Pid, Reason}, State)
%%           Pid    = pid()
%%           Reason = normal | term()
%% Descrip.: Trap exit signals from socket handlers and act on them.
%%           Log if they exit with an error, and remove them from our
%%           list of existing sockets.
%% Returns : {noreply, State, ?TIMEOUT}
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
%% Function: handle_info(Unknown, State)
%% Descrip.: Unknown info.
%% Returns : {noreply, State, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_info(Unknown, State) ->
    logger:log(error, "Dialog server: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
        normal -> logger:log(debug, "Dialog server: terminating normally");
        shutdown -> logger:log(debug, "Dialog server: shutting down");
        _ -> logger:log(error, "Dialog server: terminating : ~p", [Reason])
    end,
    Reason.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
