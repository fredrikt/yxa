%%%-------------------------------------------------------------------
%%% File    : event_mnesia_monitor.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Process that subscribes to Mnesia events and notifies
%%%           ongoing subscription handlers about changes.
%%%
%%% Created : 05 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(event_mnesia_monitor).

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
-include("event.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	 }).


%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, event_mnesia_monitor).
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
    timer:apply_interval(?TIMEOUT, database_eventdata, delete_expired, []),
    case subscribe_to_table(eventdata) of
	ok ->
	    logger:log(debug, "SIP Event server Mnesia monitor started"),
	    {ok, #state{}};
	error ->
	    logger:log(error, "SIP Event server Mnesia monitor: Failed subscribing to Mnesia events"),
	    {stop, "Failed subscribing to Mnesia table events"}
    end.
	
subscribe_to_table(Tab) ->
    subscribe_to_table(Tab, 2).

subscribe_to_table(_Tab, 0) ->
    logger:log(debug, "SIP Event server Mnesia monitor: Mnesia subscribe retrys exceeded"),
    error;
subscribe_to_table(Tab, C) when is_integer(C) ->
    case mnesia:subscribe({table, Tab, simple}) of
	{error, {not_active_local, Tab}} ->
	    %% Set up RAM replica on this node, and then try one more time
	    mnesia:add_table_copy(Tab, node(), ram_copies),
	    subscribe_to_table(Tab, C - 1);
	{error, E} ->
	    logger:log(debug, "SIP Event server Mnesia monitor: Mnesia subscribe error : ~p", [E]),
	    error;
	{ok, _Node} ->
	    ok
    end.


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

handle_call(Request, _From, State) ->
    logger:log(error, "SIP Event server Mnesia monitor: Received unknown gen_server call : ~p", [Request]),
    {reply, {error, "Unknown gen_server call", State}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast(Msg, State) ->
    logger:log(error, "SIP Event server Mnesia monitor: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({mnesia_table_event, Event}, State) ->
    case database_eventdata:decode_mnesia_change_event(Event) of
	{ok, PackageS, EventL} when is_list(PackageS), is_list(EventL) ->
	    do_notify(PackageS, EventL);
	error ->
	    logger:log(error, "SIP Event server Mnesia monitor: Could not decode presentitys from mnesia table event"),
	    logger:log(debug, "SIP Event server Mnesia monitor: Event : ~p", [Event]),
	    ok;
	none ->
	    %% an event about something not referring to a change of information
	    ok
	end,
    {noreply, State};

handle_info(Info, State) when is_tuple(Info) ->
    case element(1, Info) of
	mnesia_up -> ok;
	mnesia_down -> ok;
	mnesia_checkpoint_activated -> ok;
	mnesia_checkpoint_deactivated -> ok;
	mnesia_overload -> ok;
	inconsistent_database -> ok;
	mnesia_fatal -> ok;
	mnesia_info -> ok;
	mnesia_error -> ok;
	mnesia_user -> ok;
	_ ->
	    logger:log(error, "SIP Event server Mnesia monitor: Received unknown gen_server info : ~p", [Info])
    end,
    {noreply, State};

handle_info(Info, State) ->
    logger:log(error, "SIP Event server Mnesia monitor: Received unknown gen_server info : ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
	normal -> true;
	_ -> logger:log(error, "SIP Event server Mnesia monitor: terminating : ~p", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: do_notify(PackageS, In)
%%           PackageS = string(), event package name
%%           In       = list() of eventdata_dbe record()
%% Descrip.: Send {notify, Source} signals to all processes registered
%%           as watchers for the presentitys in the eventdata_dbe
%%           records.
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
do_notify(PackageS, [Eventdata | T]) when is_record(Eventdata, eventdata_dbe) ->
    #eventdata_dbe{presentity = Presentity,
		   flags      = Flags
		  } = Eventdata,
    Source =
	case lists:keysearch(source, 1, Flags) of
	    {value, {source, SourceV}} -> SourceV;
	    false -> undefined
	end,
    logger:log(debug, "SIP Event server Mnesia monitor: Detected change of state for "
	       "presentity : ~p (package ~p, source: ~p)", [Presentity, PackageS, Source]),
    NotifyL = notifylist:lookup(Presentity, PackageS),
    logger:log(debug, "SIP Event server Mnesia monitor: Extra debug: Notifying : ~p",
	       [NotifyL]),
    _ = [gen_server:cast(Pid, {notify, Source}) || Pid <- NotifyL],
    do_notify(PackageS, T);
do_notify(_PackageS, []) ->
    ok.
