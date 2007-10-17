%%%-------------------------------------------------------------------
%%% File    : event_mnesia_monitor.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Process that subscribes to Mnesia events and notifies
%%%           ongoing subscription handlers about changes.
%%%
%%% @since    05 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
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
%% @type state() = #state{}.
%%                 no description
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
    timer:apply_interval(?TIMEOUT, database_eventdata, delete_expired, []),
    Tables = [eventdata, phone],
    case subscribe_to_tables(Tables) of
	ok ->
	    logger:log(debug, "SIP Event server Mnesia monitor started"),
	    {ok, #state{}};
	error ->
	    logger:log(error, "SIP Event server Mnesia monitor: Failed subscribing to Mnesia events"),
	    {stop, "Failed subscribing to Mnesia table events"}
    end.

%% part of init/1
subscribe_to_tables([Tab | T]) ->
    case subscribe_to_table(Tab, 2) of
	ok ->
	    subscribe_to_tables(T);
	E ->
	    E
    end;
subscribe_to_tables([]) ->
    ok.

%% part of subscribe_to_tables/1
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
%% @spec    (Unknown, From, State) -> {noreply, State}
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "SIP Event server Mnesia monitor: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "Unknown gen_server call", State}}.

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
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Unknown cast.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "SIP Event server Mnesia monitor: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.

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
%% @spec    (Info, State) -> {noreply, State}
%%
%%            Info  = {mnesia_table_event, Event}
%%            Event = tuple()
%%            State = #state{}
%%
%% @doc     Handle a mnesia_table_event for table 'eventdata'.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({mnesia_table_event, {_Type, Rec, _Tid} = Event}, State) when element(1, Rec) == eventdata ->
    case database_eventdata:decode_mnesia_change_event(Event) of
	{ok, PackageS, EventL} when is_list(PackageS), is_list(EventL) ->
	    do_notify(PackageS, EventL);
	error ->
	    logger:log(error, "SIP Event server Mnesia monitor: Could not decode presentitys from mnesia table "
		       "'eventdata' event"),
	    logger:log(debug, "SIP Event server Mnesia monitor: Event : ~p", [Event]);
	none ->
	    %% an event about something not referring to a change of information
	    ok
	end,
    {noreply, State};

%%--------------------------------------------------------------------
%% @spec    (Info, State) -> {noreply, State}
%%
%%            Info  = {mnesia_table_event, Event}
%%            Event = tuple()
%%            State = #state{}
%%
%% @doc     Handle a mnesia_table_event for table 'phone'.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({mnesia_table_event, {_Type, Rec, _Tid} = Event}, State) when element(1, Rec) == phone ->
    case phone:decode_mnesia_change_event(Event) of
	{ok, Action, User, Location} when is_list(User) ->
	    logger:log(debug, "SIP Event server Mnesia monitor: Detected location db '~p' for SIP user ~p, location ~p",
		       [Action, User, sipurl:print(siplocation:to_url(Location))]),
	    case local:eventserver_locationdb_action(Action, User, Location) of
		undefined ->
		    ok;
		Res when is_list(Res) ->
		    registration_event_actions(Res)
	    end;
	error ->
	    logger:log(error, "SIP Event server Mnesia monitor: Could not decode locations from mnesia table "
		       "'phone' event"),
	    logger:log(debug, "SIP Event server Mnesia monitor: Event : ~p", [Event]);
	none ->
	    %% an event about something not referring to a change of information
	    ok
	end,
    {noreply, State};

%%--------------------------------------------------------------------
%% @spec    (MnesiaInfo, State) -> {noreply, State}
%%
%%            MnesiaInfo = tuple()
%%            State      = #state{}
%%
%% @doc     Handle a mnesia info tuple.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(MnesiaInfo, State) when is_tuple(MnesiaInfo) ->
    case element(1, MnesiaInfo) of
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
	    logger:log(error, "SIP Event server Mnesia monitor: Received unknown gen_server info : ~p", [MnesiaInfo])
    end,
    {noreply, State};

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Unknown info.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(Unknown, State) ->
    logger:log(error, "SIP Event server Mnesia monitor: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
	normal -> true;
	_ -> logger:log(error, "SIP Event server Mnesia monitor: terminating : ~p", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% @spec    (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec    (PackageS, In) -> {ok, NewState}
%%
%%            PackageS = string() "event package name"
%%            In       = [#eventdata_dbe{}]
%%
%% @doc     Send {notify, Source} signals to all processes registered
%%          as watchers for the presentitys in the eventdata_dbe
%%          records.
%% @end
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


%%--------------------------------------------------------------------
%% @spec    (Actions) -> ok
%%
%%            Actions = [tuple()]
%%
%% @doc     Perform any actions that
%%          local:eventserver_locationdb_action/3 said we should when
%%          some user registered.
%% @end
%%--------------------------------------------------------------------
registration_event_actions([{shared_line, Resource, User, URL, Params} | T]) ->
    logger:log(normal, "SIP Event server Mnesia monitor: Starting shared line ~p for user ~p (~s)",
	       [Resource, User, sipurl:print(URL)]),
    active_subscriber:shared_line(Resource, User, URL, Params),
    registration_event_actions(T);
registration_event_actions([]) ->
    ok.
