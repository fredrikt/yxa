%%%-------------------------------------------------------------------
%%% File    : tcp_connection.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : Handles a single TCP connection - executes send
%%% and gets complete received SIP messages from a single TCP receiver
%%% associated with this TCP connection.
%%%
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(tcp_connection).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socketmodule, socket, receiver, local, remote, starttime}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(SocketModule, Socket, Direction, Local, Remote) ->
    gen_server:start_link(?MODULE, [SocketModule, Socket, Direction, Local, Remote], []).

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
init([SocketModule, Socket, Direction, Local, Remote]) ->
    SipSocket = #sipsocket{module=sipsocket_tcp, pid=self(), data={Local, Remote}},
    %% Register this connection with the TCP listener process before we proceed
    case catch gen_server:call(tcp_dispatcher, {register_sipsocket, Direction, SipSocket}, 1500) of
	{registered_sipsocket} ->
	    {IP, Port} = Remote,
	    %% Start a receiver
	    Receiver = tcp_receiver:start(SocketModule, Socket, Local, Remote),
	    S = case Direction of
		    in -> "Connection from";
		    out -> "Connected to"
		end,
	    logger:log(debug, "TCP connection: ~s ~s:~p (started receiver ~p)", [S, IP, Port, Receiver]),
	    Timeout = sipserver:get_env(tcp_connection_idle_timeout, 300) * 1000,
	    Now = util:timestamp(),
	    State = #state{socketmodule=SocketModule, socket=Socket, receiver=Receiver, local=Local, remote=Remote, starttime=Now},
	    {ok, State, Timeout};
	{failed_registering_sipsocket, E} ->
	    logger:log(error, "TCP connection: Failed registering with TCP dispatcher : ~p", [E]),
	    {stop, "Failed registering with TCP dispatcher"};
	{'EXIT', Reason} ->
	    logger:log(error, "TCP connection: Failed registering new connection with TCP dispatcher : ~p",
		       [Reason]),
	    {stop, "Failed registering with TCP dispatcher"}
    end.


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

%% Request to send data on our socket.
handle_call({send, {SendToHost, PortInt, Message}}, From, State) ->
    SendRes = gen_tcp:send(State#state.socket, Message),
    Timeout = sipserver:get_env(tcp_connection_idle_timeout, 300) * 1000,
    Reply = {send_result, SendRes},
    {reply, Reply, State, Timeout}.


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%% Data received by our receiver process, invoke sipserver:process() on it.
handle_cast({recv, Data}, State) ->
    {IP, Port} = State#state.remote,
    SipSocket = #sipsocket{module=sipsocket_tcp, pid=self(), data={State#state.local, State#state.remote}},
    Origin = {sipsocket:sipproto2str(sipsocket_tcp), IP, Port, SipSocket},
    sipserver:safe_spawn(sipserver, process, [Data, SipSocket, Origin, transaction_layer, transaction_layer]),
    Timeout = sipserver:get_env(tcp_connection_idle_timeout, 300) * 1000,
    {noreply, State, Timeout};

%% We are closing the connection.
handle_cast({close, From}, State) ->
    Duration = util:timestamp() - State#state.starttime,
    {IP, Port} = State#state.remote,
    logger:log(debug, "TCP connection: Closing connection with ~s:~p (duration: ~p seconds)", [IP, Port, Duration]),
    gen_tcp:close(State#state.socket),
    {stop, normal, State#state{socket=undefined}};

%% Our receiver signals us that it detected that the other end closed the connection.
handle_cast({connection_closed, From}, State) ->
    Duration = util:timestamp() - State#state.starttime,
    {IP, Port} = State#state.remote,
    logger:log(debug, "TCP connection: Connection with ~s:~p closed by foreign host (duration: ~p seconds)",
	       [IP, Port, Duration]),
    gen_tcp:close(State#state.socket),
    {stop, normal, State#state{socket=undefined}}.


%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {IP, Port} = State#state.remote,
    Timeout = sipserver:get_env(tcp_connection_idle_timeout, 300) * 1000,
    logger:log(debug, "Sipsocket TCP: Connection with ~s:~p timed out after ~p seconds, socket handler terminating.",
	       [IP, Port, Timeout div 1000]),
    exit(State#state.receiver, "timed out"),
    gen_tcp:close(State#state.socket),
    {stop, normal, State#state{socket=undefined}};

handle_info(Info, State) ->
    logger:log(debug, "TCP connection: Received unknown gen_server info :~n~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(normal, State) ->
    ok;
terminate(Reason, State) ->
    logger:log(error, "TCP connection: Terminating for some other reason than 'normal' : ~n~p",
	       [Reason]),
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

