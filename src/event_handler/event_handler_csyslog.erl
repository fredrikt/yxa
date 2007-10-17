%%%-------------------------------------------------------------------
%%% File    : event_handler_csyslog.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Event handler to log events to syslog using a C port
%%%           driver (syslog_port).
%%%
%%%           The reason to not do this simply by sending syslog UDP
%%%           datagrams is that it would require a syslog server, or
%%%           a local syslog daemon listening on a network socket.
%%%           Sending to a remote syslog server would also mean that
%%%           the messages wouldn't end up in the local syslog files,
%%%           regardless of local syslog configuration. This is not
%%%           good - we should follow the path of least surprise.
%%%
%%%           The reason for not simply writing to /dev/syslog is that
%%%           Erlang refuses to open character devices.
%%%
%%% @since    6 Dec 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(event_handler_csyslog).
%%-compile(export_all).

-behaviour(gen_event).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Internal exports - gen_event callbacks
%%--------------------------------------------------------------------
-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  port
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(DEFAULT_PORTNAME, "syslog_c-port").

%%====================================================================
%% External functions
%%====================================================================


%%====================================================================
%% Behaviour functions - gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Args) -> {ok, State}
%%
%% @doc     Initialize this event handler.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([AppName]) when is_atom(AppName) ->
    init([AppName, ?DEFAULT_PORTNAME]);
init([AppName, PortName]) when is_atom(AppName), is_list(PortName) ->
    %% Look for the syslog_c-port in the directory where this modules BEAM-file resides
    Directorys = [filename:dirname(code:which(?MODULE))] ++
	[code:priv_dir(yxa)],
    case locate_file(Directorys, PortName) of
	{ok, Cmd} ->
	    Port = open_port({spawn, Cmd}, [{packet, 2}, exit_status]),
	    %% syslog_port will openlog() with the first data we send it as identifier
	    Msg = atom_to_list(AppName),
	    true = port_command(Port, Msg),
	    {ok, #state{port=Port}};
	not_found ->
	    logger:log(error, "Event handler csyslog: Could not locate C port binary '~s' "
		       "(searched in directorys : ~p)", [PortName, Directorys]),
	    {error, "C port binary not found"}
    end.

%%--------------------------------------------------------------------
%% @spec    handle_event(Event, State) ->
%%            {ok, State}                                |
%%            {swap_handler, Args1, State1, Mod2, Args2} |
%%            remove_handler
%%
%% @doc     This function gets called when the event manager receives
%%          an event sent using gen_event:notify/2 (or sync_notify).
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({event, Pid, Prio, Class, Id, L}, State) -> {ok, State}
%%
%%            Pid   = pid() "process generating the event"
%%            Prio  = debug | normal | error
%%            Class = call | proxy | atom() "class of message"
%%            Id    = string()
%%            L     = [term()] "the data to be logged"
%%
%% @doc     Log event using our syslog port driver.
%% @end
%%--------------------------------------------------------------------
handle_event({event, Pid, Prio, Class, Id, L}, State) when is_pid(Pid), is_atom(Prio), is_atom(Class),
							   is_list(Id); is_list(L) ->
    PrioChar = case Prio of
		   debug -> $d;
		   normal -> $i;
		   error -> $e;
		   _ -> 46	%% 46 is $.
	       end,
    Msg = io_lib:format("~c" "c=~p; id=~p; ~800p", [PrioChar, Class, Id, L]),
    Port = State#state.port,
    true = port_command(Port, Msg),
    {ok, State};

handle_event(Event, State) ->
    logger:log(error, "Event handler csyslog: Received unknown event : ~p", [Event]),
    {ok, State}.


%%--------------------------------------------------------------------
%% @spec    (Request, State) ->
%%            {ok, Reply, State}                                |
%%            {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%            {remove_handler, Reply}
%%
%% @doc     This gets called when the event manager receives a request
%%          sent using gen_event:call/3,4.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Request, State) ->
    logger:log(error, "Event handler csyslog: Received unknown call : ~p", [Request]),
    Reply = ok,
    {ok, Reply, State}.


%%--------------------------------------------------------------------
%% @spec    (Info, State) ->
%%            {ok, State}                                |
%%            {swap_handler, Args1, State1, Mod2, Args2} |
%%            remove_handler
%%
%% @doc     This function is called when the event manager receives
%%          any other message than an event or a synchronous request
%%          (or a system message).
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {data, Data}}, State) when is_record(State, state), State#state.port == Port ->
    logger:log(error, "Event handler csyslog: Error from port driver : ~p", [Data]),
    remove_handler;

handle_info({Port, {exit_status, Status}}, #state{port = Port}) ->
    logger:log(error, "Event handler csyslog: Port driver ~p exited : ~p", [Port, Status]),
    remove_handler;

handle_info(Unknown, State) ->
    logger:log(error, "Event handler csyslog: Received unknown gen_event info : ~p", [Unknown]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> void()
%%
%% @doc     Called when this event handler is deleted from the event
%%          manager. Clean up.
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    true = port_close(State#state.port),
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


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Path, File) -> {ok, Filename} | not_found
%%
%% @doc     Locate a file fiven a set of directorys.
%% @end
%%--------------------------------------------------------------------
locate_file([H | T], File) ->
    Filename = filename:join(H, File),
    case file:read_file_info(Filename) of
	{ok, _FileInfo} ->
	    {ok, Filename};
	_ ->
	    locate_file(T, File)
    end;
locate_file([], _File) ->
    not_found.

