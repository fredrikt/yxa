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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([connect_to_remote/5]).

-record(state, {socketlist}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, tcp_dispatcher}, ?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    Port = sipserver:get_listenport(tcp),
    TLSport = sipserver:get_listenport(tls),
    TCPlisteners = [{tcp, Port}, {tcp6, Port}],
    Listeners = case sipserver:get_env(enable_experimental_tls, false) of
		    true ->
			%% XXX add tls6 to this list when there is an Erlang version released
			%% than has a ssl.erl that handles inet6. Current version (R9C-0) treats
			%% inet6 as an invalid gen_tcp option.
			lists:append(TCPlisteners, [{tls, TLSport}]);
		    _ ->
			TCPlisteners
		end,
    SocketList = start_listeners(Listeners),
    {ok, #state{socketlist=SocketList}, ?TIMEOUT}.

start_listeners(L) ->
    start_listeners(L, socketlist:empty()).

start_listeners([], SocketList) ->
    SocketList;
start_listeners([{Proto, Port} | T], SocketList) when atom(Proto), integer(Port), Proto == tcp6; Proto == tls6 ->
    case sipserver:get_env(enable_v6, false) of
	true ->
	    NewSocketList = case catch tcp_listener:start(Proto, Port) of
				{ok, Local6, Listener6} ->
				    SipSocket = #sipsocket{module=sipsocket_tcp, proto=Proto, pid=Listener6, data={Local6, none}},
				    %% Add with timeout of 0 since listening sockets should never be expired
				    socketlist:add({listener, Proto, Port}, Listener6, Local6, none, SipSocket, 0, SocketList);
				not_started ->
				    SocketList;
				{'EXIT', Reason} ->
				    logger:log(error, "Failed starting IPv6 TCP listener (~p) : ~p", [Proto, Reason]),
				    SocketList;
				Unknown ->
				    logger:log(error, "Failed starting IPv6 TCP listener (~p) : ~p", [Proto, Unknown]),
				    SocketList
			    end,
	    start_listeners(T, NewSocketList);
	_ ->
	    start_listeners(T, SocketList)
    end;
start_listeners([{Proto, Port} | T], SocketList) when atom(Proto), integer(Port), Proto == tcp; Proto == tls ->
    NewSocketList = case catch tcp_listener:start(Proto, Port) of
			{ok, Local, Listener} ->
			    SipSocket = #sipsocket{module=sipsocket_tcp, proto=Proto, pid=Listener, data={Local, none}},
			    %% Add with timeout of 0 since listening sockets should never be expired
			    socketlist:add({listener, Proto, Port}, Listener, Local, none, SipSocket, 0, SocketList);
			not_started ->
			    SocketList;
			{'EXIT', Reason} ->
			    logger:log(error, "Failed starting IPv4 TCP listener (~p) : ~p", [Proto, Reason]),
			    SocketList;
			Unknown ->
			    logger:log(error, "Failed starting IPv4 TCP listener (~p) : ~p", [Proto, Unknown]),
			    SocketList
		    end,
    start_listeners(T, NewSocketList).

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
handle_call({get_socket, Proto, Host, Port}, From, State) ->
    case get_socket_from_list(Host, Port, State#state.socketlist) of
	none ->
	    %% We must spawn a child process to take care of making a new connection
	    %% since we can't risk this thread getting locked up.
	    %P = sipserver:safe_spawn(fun connect_to_remote/4, [From, Proto, Host, Port]),
	    %spawn(?MODULE, connect_to_remote, [Proto, Host, Port, From, self()]),
	    tcp_connection:start_link(connect, Proto, Host, Port, From),
	    logger:log(debug, "Sipsocket TCP: No cached '~p' connection to remote host ~s:~p, trying to connect",
		       [Proto, Host, Port]),
	    {noreply, State, ?TIMEOUT};
	{error, E} ->
	    {reply, {error, E}, State, ?TIMEOUT};
	SipSocket when record(SipSocket, sipsocket) ->
	    logger:log(debug, "Sipsocket TCP: Use existing connection to ~s:~p", [Host, Port]),
	    {reply, {ok, SipSocket}, State, ?TIMEOUT}
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
	    case socketlist:add(Ident, CPid, Local, Remote, SipSocket, 0, State#state.socketlist) of
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
        normal -> logger:log(error, "TCP dispatcher terminating normally");
        _ -> logger:log(error, "TCP dispatcher terminating : ~p", [Reason]),
	     timer:sleep(500)
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
	    [CPid, Local, Remote, SipSocket] = socketlist:extract([pid, local, remote, sipsocket], SListElem),
	    logger:log(debug, "Sipsocket TCP: Reusing existing connection to ~s:~p (~p)",
		       [Host, Port, CPid]),
	    SipSocket;
	_ ->
	    none
    end.

%% Unfortunate little process necessary to start connections to remote hosts
%% without blocking tcp_dispatcher. We would not need this process at all if
%% gen_server:start() did not block until init() returns.
connect_to_remote(Proto, Host, Port, GenServerFrom, Parent) ->
    tcp_connection:start(connect, sipsocket_tcp, Proto, Host, Port, GenServerFrom, Parent).
