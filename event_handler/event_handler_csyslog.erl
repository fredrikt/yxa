%%%-------------------------------------------------------------------
%%% File    : event_handler_csyslog.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Event handler to log events to syslog using a C port
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
%%% Created : 6 Dec 2004 by Fredrik Thulin <ft@it.su.se>
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
-record(state, {
	  port
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).


%%====================================================================
%% External functions
%%====================================================================


%%====================================================================
%% Behaviour functions - gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Descrip.: Initialize this event handler.
%% Returns : {ok, State}
%%--------------------------------------------------------------------
init([AppName]) when is_atom(AppName) ->
    %% Look for the syslog_c-port in the directory where this modules BEAM-file resides
    Dir = filename:dirname(code:which(?MODULE)),
    Cmd = filename:join(Dir, "syslog_c-port"),
    Port = open_port({spawn, Cmd}, [{packet, 2}]),
    %% syslog_port will openlog() with the first data we send it as identifier
    Msg = atom_to_list(AppName),
    true = port_command(Port, Msg),
    {ok, #state{port=Port}}.


%%--------------------------------------------------------------------
%% Function: handle_event(Event, State)
%% Descrip.: This function gets called when the event manager receives
%%           an event sent using gen_event:notify/2 (or sync_notify).
%% Returns : {ok, State}                                |
%%           {swap_handler, Args1, State1, Mod2, Args2} |
%%           remove_handler
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_event({event, Prio, Class, Desc, Ctx, Data},
%%                        State)
%%           Prio  = atom(), debug | normal | error
%%           Class = atom(), class of message (call | proxy | ...)
%%           Desc  = atom() | tuple(), message type
%%           Ctx   = term(), context
%%           Data  = term(), the data to be logged
%% Descrip.: Log event using our syslog port driver.
%% Returns : {ok, State}
%%--------------------------------------------------------------------
handle_event({event, Pid, Prio, Class, Id, L}, State) when is_pid(Pid), is_atom(Prio), is_atom(Class),
							   is_list(Id); is_list(L) ->
    PrioChar = case Prio of
		   debug -> $d;
		   normal -> $i;
		   error -> $e;
		   _ -> $.
	       end,
    Msg = io_lib:format("~c" "c=~p; id=~p; ~800p", [PrioChar, Class, Id, L]),
    Port = State#state.port,
    true = port_command(Port, Msg),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_call(Request, State)
%% Descrip.: This gets called when the event manager receives a
%%           request sent using gen_event:call/3,4.
%% Returns : {ok, Reply, State}                                |
%%           {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%           {remove_handler, Reply}
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State)
%% Descrip.: This function is called when the event manager receives
%%           any other message than an event or a synchronous request
%%           (or a system message).
%% Returns : {ok, State}                                |
%%           {swap_handler, Args1, State1, Mod2, Args2} |
%%           remove_handler
%%--------------------------------------------------------------------
handle_info({Port, {data, Data}}, State) when is_record(State, state), State#state.port == Port ->
    logger:log(error, "Event handler csyslog: Error from port driver : ~p", [Data]),
    remove_handler;

handle_info(Unknown, State) ->
    logger:log(error, "Event handler csyslog: Received unknown gen_event info : ~p", [Unknown]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Called when this event handler is deleted from the event
%%           manager. Clean up.
%% Returns : void()
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    true = port_close(State#state.port),
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
