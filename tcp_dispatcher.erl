%%%-------------------------------------------------------------------
%%% File    : tcp_dispatcher.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : TCP dispatcher initially does gen_tcp:listen() and
%%% then keeps track of all existing TCP connections.
%%%
%%% Created : 12 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(tcp_dispatcher).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("socketlist.hrl").
-include("sipsocket.hrl").

-define(TIMEOUT, 10 * 1000).
%%--------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socketlist}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, tcp_dispatcher}, ?MODULE, [Port], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Port])  when integer(Port) ->
    process_flag(trap_exit, true),
    {ok, Local, Listener} = tcp_listener:start(gen_tcp, Port),
    SocketList = socketlist:add(listener, Listener, Local, {"0.0.0.0", 0}, 0, socketlist:empty()),
    {ok, #state{socketlist=SocketList}, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({get_socket, Host, Port}, From, State) ->
    case get_socket_from_list(Host, Port, State#state.socketlist) of
	none ->
	    %% We must spawn a child process to take care of making a new connection
	    %% since we can't risk this thread getting locked up.
	    P = sipserver:safe_spawn(fun connect_to_remote/4, [gen_tcp, From, Host, Port]),
	    {noreply, State, ?TIMEOUT};
	{error, E} ->
	    {reply, {error, E}, State, ?TIMEOUT};
	GotSocket when record(GotSocket, sipsocket) ->
	    logger:log(debug, "Sipsocket TCP: (Transport layer) Use existing connection to ~s:~p", [Host, Port]),
	    {reply, {ok, GotSocket}, State, ?TIMEOUT}
    end;


handle_call({register_sipsocket, Dir, SipSocket}, From, State) when record(SipSocket, sipsocket) ->
    CPid = SipSocket#sipsocket.pid,
    case catch link(CPid) of
	true ->
	    {Local, Remote} = SipSocket#sipsocket.data,
	    Ident = case Dir of
			in ->
			    {connection_from, Remote};
			out ->
			    {connection_to, Remote}
		    end,
	    case socketlist:add(Ident, CPid, Local, Remote, 0, State#state.socketlist) of
		{error, E} ->
		    logger:log(error, "TCP dispatcher: Failed adding ~p to socketlist", [Ident]),
		    {reply, {error, E}, State, ?TIMEOUT};
		NewSocketList1 ->
		    {reply, ok, State#state{socketlist=NewSocketList1}, ?TIMEOUT}
	    end;
	_ ->
	    {reply, {error, "Could not link to pid"}, State, ?TIMEOUT}
    end;

handle_call({quit}, From, State) ->
    {stop, "Asked to quit", State}.


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    logger:log(error, "TCP dispatcher: 'cast' invoked but not handled : ~p", [Msg]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    SocketList1 = socketlist:delete_expired(State#state.socketlist),
    {noreply, State#state{socketlist=SocketList1}, ?TIMEOUT};

handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
	normal -> logger:log(debug, "TCP dispatcher: Received normal exit-signal from process ~p", [Pid]);
	_ -> logger:log(error, "TCP dispatcher: =ERROR REPORT==== Received non-normal exit signal from process ~p :~n~p", [Pid, Reason])
    end,
    NewState = case socketlist:get_using_pid(Pid, State#state.socketlist) of
		   none ->
		       logger:log(debug, "TCP dispatcher: Received exit signal from ~p not in my list. Socketlist is :~n~p",
				  [Pid, socketlist:debugfriendly(State#state.socketlist)]),
		       State;
		   L when record(L, socketlist) ->
		       NewL = socketlist:delete_using_pid(Pid, State#state.socketlist),
		       logger:log(debug, "TCP dispatcher: Deleting ~p entry(s) from socketlist :~n~p~n(new list is ~p entry(s))",
				  [socketlist:get_length(L), socketlist:debugfriendly(L), socketlist:get_length(NewL)]),
		       %%logger:log(debug, "TCP dispatcher: Extra debug: Socketlist is now :~n~p", [socketlist:debugfriendly(NewL)]),
		       State#state{socketlist=NewL}
	       end,
    {noreply, NewState, ?TIMEOUT};

handle_info(Unknown, State) ->
    logger:log(error, "TCP dispatcher: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    case Reason of
        normal -> true;
        _ -> logger:log(error, "TCP dispatcher terminating : ~p", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_socket_from_list(Host, Port, SocketList) ->
    case socketlist:get_using_remote({Host, Port}, SocketList) of
	SListElem when record(SListElem, socketlistelem) ->
	    [CPid, Local, Remote] = socketlist:extract([pid, local, remote], SListElem),
	    logger:log(debug, "Sipsocket TCP: Reusing existing connection to ~s:~p (~p)",
		       [Host, Port, CPid]),
	    SipSocket = #sipsocket{module=sipsocket_tcp, pid=CPid, data={Local, Remote}},
	    SipSocket;
	_ ->
	    none
    end.

%% Helps us to not block the TCP dispatcher when trying to connect to a remote host
%% No-one pays attention to what this function returns, since it is spawned, as long
%% as it does not return something caught by sipserver:safe_spawn().
connect_to_remote(SocketModule, RequestorPid, Host, Port) ->
    logger:log(debug, "Sipsocket TCP: No cached connection to remote host ~s:~p, trying to connect",
	       [Host, Port]),
    case SocketModule:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
	{ok, NewSocket} ->
	    Local = case inet:sockname(NewSocket) of
			{ok, {IPlist, LocalPort}} ->
			    {siphost:makeip(IPlist), LocalPort};
			{error, E1} ->
			    logger:log(error, "Sipsocket TCP: sockname() on new socket returned error ~p", [E1]),
			    {"0.0.0.0", 0}
		    end,
	    Remote = {Host, Port},
	    case tcp_connection:start_link(SocketModule, NewSocket, out, Local, Remote) of
		{ok, Pid} ->
		    SipSocket = #sipsocket{module=sipsocket_tcp, pid=Pid, data={Local, Remote}},
		    gen_server:reply(RequestorPid, {ok, SipSocket});
		{'EXIT', Reason} ->
		    logger:log(error, "Sipsocket TCP: TCP dispatcher failed adding new socket ~p " ++
			       "(connected to remote peer ~s:~p) : ~p", [NewSocket, Host, Port, Reason]),
		    E = "TCP dispatcher failed",
		    gen_server:reply(RequestorPid, {error, E}),
		    SocketModule:close(NewSocket);
		Unknown ->
		    logger:log(error, "Sipsocket TCP: TCP dispatcher failed adding new socket ~p " ++
			       "(connected to remote peer ~s:~p), tcp_connection start_link returned :~n~p",
			       [NewSocket, Host, Port, Unknown]),
		    gen_server:reply(RequestorPid, {error, "Failed starting tcp_connection"}),
		    SocketModule:close(NewSocket)
	    end;
	{error, econnrefused} ->
	    gen_server:reply(RequestorPid, {error, "Connection refused"});
	{error, E} ->
	    logger:log(error, "Sipsocket TCP: Failed connecting to ~s:~p : ~s (~p)", [Host, Port, inet:format_error(E), E]),
	    gen_server:reply(RequestorPid, {error, inet:format_error(E)})
    end,
    %% process spawned to connect to remote host terminates now
    ok.

