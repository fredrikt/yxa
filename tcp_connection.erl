%%%-------------------------------------------------------------------
%%% File    : tcp_connection.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Handles a single TCP connection - executes sen and gets
%%%           complete received SIP messages from a single TCP
%%%           receiver associated with this TCP connection.
%%%
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(tcp_connection).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/6, start_link/5]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  socketmodule,
	  proto,
	  socket,
	  receiver,
	  local,
	  remote,
	  starttime,
	  sipsocket,
	  timeout,
	  on
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%% Our socket options
-define(SOCKETOPTS, [binary, {packet, 0}, {active, false}]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/6
%% Descrip.: Starts the server
%% Returns : void()
%%--------------------------------------------------------------------
start_link(in, SocketModule, Proto, Socket, Local, Remote) ->
    gen_server:start_link(?MODULE, [in, SocketModule, Proto, Socket, Local, Remote], []).

start_link(connect, Proto, Host, Port, GenServerFrom) ->
    gen_server:start(?MODULE, [connect, Proto, Host, Port, GenServerFrom], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(L)
%% Descrip.: Initiates the server
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: init([connect, Proto, Host, Port, GenServerFrom])
%% Descrip.: Try to connect to a remote host, and answer GenServerFrom
%% Returns : {ok, State}
%%--------------------------------------------------------------------
init([connect, Proto, Host, Port, GenServerFrom]) ->
    %% To not block tcp_dispatcher (who starts these tcp_connections), we
    %% send ourself a cast and return immediately.
    gen_server:cast(self(), {connect_to_remote, Proto, Host, Port, GenServerFrom}),
    {ok, #state{on=false}};

%%--------------------------------------------------------------------
%% Function: init([Direction, SocketModule, Proto, Socket, Local,
%%                Remote])
%%           Direction    = in | out
%%           SocketModule = atom(), the name of the socket module
%%                          this socket uses (gen_tcp | ssl)
%%           Proto        = atom(), tcp | tcp6 | tls | tls6
%%           Socket       = term()
%%           Local        = term() ({Host, Port} tuple())
%%           Remote       = term() ({Host, Port} tuple())
%% Descrip.: Initialize this tcp_connection process to handle Socket.
%% Returns : {ok, State, Timeout} |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([Direction, SocketModule, Proto, Socket, Local, Remote]) ->
    SipSocket = #sipsocket{module=sipsocket_tcp, proto=Proto, pid=self(), data={Local, Remote}},
    %% Register this connection with the TCP listener process before we proceed
    case catch gen_server:call(tcp_dispatcher, {register_sipsocket, Direction, SipSocket}, 1500) of
	ok ->
	    {IP, Port} = Remote,
	    %% Start a receiver
	    Receiver = tcp_receiver:start_link(SocketModule, Socket, Local, Remote, SipSocket),
	    S = case Direction of
		    in -> "Connection from";
		    out -> "Connected to"
		end,
	    %% If it is an SSL socket, we must change controlling_process() to the tcp_receiver.
	    %% Don't do this here if Direction is 'in' though, because this process is not the
	    %% controlling process for those and tcp_listener will change controlling process
	    %% to the receiver as soon as we have started it and init() has returned.
	    case SocketModule of
		ssl when Direction /= in ->
		    case SocketModule:controlling_process(Socket, Receiver) of
			ok ->
			    logger:log(debug, "TCP connection: Changed controlling process of socket ~p to receiver ~p",
				       [Socket, Receiver]),
			    ok;
			{error, Reason} ->
			    logger:log(error, "TCP connection: Failed changing controlling process of SSL socket ~p to receiver ~p : ~p",
				       [Socket, Receiver, Reason]),
			    error;
			_ ->
			    ok
		    end;
		_ ->
		    ok
	    end,
	    logger:log(debug, "TCP connection: ~s ~p:~s:~p (socket ~p, started receiver ~p)", [S, Proto, IP, Port, Socket, Receiver]),
	    Timeout = sipserver:get_env(tcp_connection_idle_timeout, 300) * 1000,
	    Now = util:timestamp(),
	    State = #state{socketmodule=SocketModule, proto=Proto, socket=Socket, receiver=Receiver, local=Local, remote=Remote,
			   starttime=Now, sipsocket=SipSocket, timeout=Timeout, on=true},
	    {ok, State, Timeout};
	{error, E} ->
	    logger:log(error, "TCP connection: Failed registering with TCP dispatcher : ~p", [E]),
	    {stop, "Failed registering with TCP dispatcher"};
	{'EXIT', Reason} ->
	    logger:log(error, "TCP connection: Failed registering new connection with TCP dispatcher : ~p",
		       [Reason]),
		    {stop, "Failed registering with TCP dispatcher"}
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


%%--------------------------------------------------------------------
%% Function: handle_call({send, {Host, Port, Message}}, From, State)
%%           Host = string()
%%           Port = integer()
%%           Message = list()
%% Descrip.: Send data on our socket, provided that
%%           State#state.on == true.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply = {send_result, SendRes}
%%           SendRes = term(), result of SocketModule:send()
%%--------------------------------------------------------------------
handle_call({send, {_Host, _Port, Message}}, _From, State) when State#state.on == true ->
    %% XXX verify that Host:Port matches host and port in State#State.sipsocket!
    SocketModule = State#state.socketmodule,
    SendRes = case catch SocketModule:send(State#state.socket, Message) of
		  R -> R
	      end,
    Reply = {send_result, SendRes},
    {reply, Reply, State, State#state.timeout};

%%--------------------------------------------------------------------
%% Function: handle_call({get_receiver}, From, State)
%% Descrip.: Get our receiver processes pid.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply = {ok, Pid}       |
%%                   {error, Reason}
%%           Pid    = pid()
%%           Reason = string()
%%--------------------------------------------------------------------
handle_call({get_receiver}, _From, State) when State#state.on == true ->
    {reply, {ok, State#state.receiver}, State, State#state.timeout};
handle_call({get_receiver}, _From, State) ->
    {reply, {error, "Not started"}, State, State#state.timeout}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_cast({connect_to_remote, Proto, Host, Port,
%%                       GenServerFrom}, State)
%%           Proto = atom(), tcp | tcp6 | tls | tls6
%%           Host  = string()
%%           Port  = integer()
%%           GenServerFrom = term(), gen_server From data
%% Descrip.: Connect to a remote Host on port Port over protocol Proto
%%           provided that State#state.on == false. Send the result of
%%           this operation to GenServerFrom using gen_server:reply().
%% Returns : {noreply, NewState, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%           Reason = normal   |
%%                    string()
%%--------------------------------------------------------------------
handle_cast({connect_to_remote, Proto, Host, Port, GenServerFrom}, State) when State#state.on == false ->
    ConnectTimeout = sipserver:get_env(tcp_connect_timeout, 20) * 1000,
    {InetModule, SocketModule, Options} = case Proto of
					      tcp ->
						  {inet, gen_tcp, ?SOCKETOPTS};
					      tcp6 ->
						  {inet, gen_tcp, lists:append(?SOCKETOPTS, [inet6])};
					      tls ->
						  {ssl, ssl, ?SOCKETOPTS};
					      tls6 ->
						  {ssl, ssl, lists:append(?SOCKETOPTS, [inet6])}
			      end,
    Host2 = util:remove_v6_brackets(Host),
    {TimeSpent, ConnectRes} = timer:tc(SocketModule, connect, [Host2, Port, Options, ConnectTimeout]),
    logger:log(debug, "TCP connection: Extra debug: Time spent connecting to ~p:~s:~p : ~p ms",
	       [Proto, Host, Port, TimeSpent div 1000]),
    case ConnectRes of
	{ok, NewSocket} ->
	    Local = case InetModule:sockname(NewSocket) of
			{ok, {IPlist, LocalPort}} ->
			    {siphost:makeip(IPlist), LocalPort};
			{error, E1} ->
			    logger:log(error, "TCP connection: ~p:sockname() on new socket returned error ~p", [InetModule, E1]),
			    {get_defaultaddr(Proto), 0}
		    end,
	    Remote = {Host, Port},
	    %% Call init() to get the tcp_receiver started before we do our gen_server:reply()
	    case init([out, SocketModule, Proto, NewSocket, Local, Remote]) of
		{ok, NewState, Timeout} ->
		    SipSocket = NewState#state.sipsocket,
		    logger:log(debug, "TCP connection: Extra debug: Connected to ~p:~s:~p, socket ~p", [Proto, Host, Port, SipSocket]),
		    %% This is the answer to a 'gen_server:call(tcp_dispatcher, {get_socket ...)' that
		    %% someone (GenServerFrom) made, but where there were no cached connection available.
		    gen_server:reply(GenServerFrom, {ok, SipSocket}),
		    {noreply, NewState, Timeout};
		Res ->
		    logger:log(debug, "TCP connection: init() returned unknown data : ~p", [Res]),
		    gen_server:reply(GenServerFrom, {error, "Failed initializing tcp_connection handler"}),
		    {stop, "Failed initializing tcp_connection handler", State}
	    end;
	{error, econnrefused} ->
	    gen_server:reply(GenServerFrom, {error, "Connection refused"}),
	    {stop, normal, State};
	{error, ehostunreach} ->
	    %% If we don't get a connection before ConnectTimeout, we end up here
	    gen_server:reply(GenServerFrom, {error, "Host unreachable"}),
	    {stop, normal, State};
	{error, E} ->
	    logger:log(error, "TCP connection: Failed connecting to ~p:~s:~p : ~p (~s)",
		       [Proto, Host, Port, E, inet:format_error(E)]),
	    gen_server:reply(GenServerFrom, {error, "Failed connecting to remote host"}),
	    {stop, normal, State}
    end;

%%--------------------------------------------------------------------
%% Function: handle_cast({recv_sipmsg, Data}, State)
%%           Data = request record() | response record()
%% Descrip.: Our received process has received a SIP message on our
%%           socket, invoke sipserver:process() on it.
%% Returns : {noreply, State, Timeout}
%%--------------------------------------------------------------------
handle_cast({recv_sipmsg, Msg}, State) when State#state.on == true ->
    {IP, Port} = State#state.remote,
    SipSocket = #sipsocket{module=sipsocket_tcp, proto=State#state.proto, pid=self(),
			   data={State#state.local, State#state.remote}},
    Origin = #siporigin{proto=State#state.proto, addr=IP, port=Port, receiver=self(),
			sipsocket=SipSocket},
    sipserver:safe_spawn(sipserver, process, [Msg, Origin, transaction_layer]),
    {noreply, State, State#state.timeout};

%%--------------------------------------------------------------------
%% Function: handle_cast({close, From}, State)
%% Descrip.: A request to close this connection.
%% Returns : {stop, normal, NewState}
%%--------------------------------------------------------------------
handle_cast({close, _From}, State) ->
    %% XXX check that From is someone sensible?
    Duration = util:timestamp() - State#state.starttime,
    {IP, Port} = State#state.remote,
    logger:log(debug, "TCP connection: Closing connection with ~p:~s:~p (duration: ~p seconds)", 
	       [State#state.proto, IP, Port, Duration]),
    SocketModule = State#state.socketmodule,
    SocketModule:close(State#state.socket),
    {stop, normal, State#state{socket=undefined}};

%%--------------------------------------------------------------------
%% Function: handle_cast({connection_closed, From}, State)
%% Descrip.: Our receiver signals us that it detected that the other
%%           end closed the connection.
%% Returns : {stop, normal, NewState}
%%--------------------------------------------------------------------
%% Our receiver signals us that it detected that the other end closed the connection.
handle_cast({connection_closed, FromPid}, #state{receiver=FromPid}=State) when is_pid(FromPid) ->
    Duration = util:timestamp() - State#state.starttime,
    {IP, Port} = State#state.remote,
    logger:log(debug, "TCP connection: Connection with ~p:~s:~p closed by foreign host (duration: ~p seconds)",
	       [State#state.proto, IP, Port, Duration]),
    %% Call close() on the socket just to make sure. For SSL, the TCP receiver will have done this for us.
    case State#state.socketmodule of
	ssl -> true;
	SocketModule ->
	    SocketModule:close(State#state.socket)
    end,
    {stop, normal, State#state{socket=undefined}};

handle_cast(Msg, State) ->
    logger:log(error, "TCP connection: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%% Descrip.: This connection has neither received nor sent any data in
%%           our configured maximum time period. Make the socket go
%%           away.
%% Returns : {noreply, State, ?TIMEOUT}          (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(timeout, State) when State#state.on == true ->
    {IP, Port} = State#state.remote,
    Timeout = State#state.timeout,
    logger:log(debug, "Sipsocket TCP: Connection with ~p:~s:~p timed out after ~p seconds, connection handler terminating.",
	       [State#state.proto, IP, Port, Timeout div 1000]),
    case State#state.socketmodule of
	ssl ->
	    %% For SSL, we signal the receiver and let it close the connection
	    %% and terminate quietly
	    State#state.receiver ! {quit_receiver, self()},
	    true;
	gen_tcp ->
	    %% the tcp_receiver will notice that the socket is closed and exit
	    logger:log(debug, "Sipsocket TCP: Closing socket ~p", [State#state.socket]),
	    gen_tcp:close(State#state.socket)
    end,
    {stop, normal, State#state{socket=undefined}};

handle_info(Info, State) ->
    logger:log(debug, "TCP connection: Received unknown gen_server info :~n~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    logger:log(error, "TCP connection: Terminating for some other reason than 'normal' : ~n~p",
	       [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

get_defaultaddr(tcp) -> "0.0.0.0";
get_defaultaddr(tcp6) -> "[::]".
