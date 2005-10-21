%%%-------------------------------------------------------------------
%%% File    : tcp_dispatcher.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: TCP dispatcher initially does gen_tcp:listen() and
%%%           then keeps track of all existing TCP connections.
%%%
%%% Created : 12 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(tcp_dispatcher).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0,
	 get_socketlist/0
	]).

%%--------------------------------------------------------------------
%% Transport layer internal exports
%%--------------------------------------------------------------------
-export([
	 get_listenerspecs/0
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

-include("socketlist.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
	  socketlist	%% socketlist record(), our list of ongoing
	                %% TCP (or TLS) connections, plus our listener
                        %% sockets
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%% Our standard wakeup interval - how often we should look for expired
%% connections in our socketlist.
-define(TIMEOUT, 10 * 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link()
%% Descrip.: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, tcp_dispatcher}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: get_socketlist()
%% Descrip.: Get the complete list of sockets from the tcp_dispatcher.
%% Returns : {ok, SocketList}
%%           SocketList = socketlist record()
%%--------------------------------------------------------------------
get_socketlist() ->
    gen_server:call(tcp_dispatcher, {get_socketlist}).

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
    %% This is a system process that traps EXIT signals from TCP/TLS connection handlers
    process_flag(trap_exit, true),
    State = #state{socketlist = socketlist:empty()
		  }, 
    ets:new(transportlayer_tcp_conn_queue, [public, set, named_table]),
    {ok, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: get_listenerspecs()
%% Descrip.: Get an OTP supservisor child specification for all TCP
%%           listeners.
%% Returns : SupSpec
%%           SupSpec = OTP supervisor child specification. Extra
%%                     processes this application want the
%%                     sipserver_sup to start and maintain.
%%--------------------------------------------------------------------
get_listenerspecs() ->
    Port = sipsocket:get_listenport(tcp),
    TLSport = sipsocket:get_listenport(tls),
    TCPlisteners = [{tcp, Port}, {tcp6, Port}],
    Listeners = case yxa_config:get_env(tls_disable_server) of
		    {ok, false} ->
			%% XXX add tls6 to this list when there is an Erlang version released
			%% than has a ssl.erl that handles inet6. Current version (R9C-0) treats
			%% inet6 as an invalid gen_tcp option.
			lists:append(TCPlisteners, [{tls, TLSport}]);
		    {ok, true} ->
			TCPlisteners
		end,
    format_listener_specs(Listeners).

%%--------------------------------------------------------------------
%% Function: format_listener_specs(L)
%%           L     = list() of {Proto, Port} tuple()
%%           Proto = atom()
%%           Port  = integer()
%% Descrip.: Format a OTP supservisor child specification for each
%%           entry in L.
%% Returns : SupSpec
%%           SupSpec = OTP supervisor child specification. Extra
%%                     processes this application want the
%%                     sipserver_sup to start and maintain.
%%--------------------------------------------------------------------
format_listener_specs(L) ->
    format_listener_specs(L, []).

format_listener_specs([], Res) ->
    lists:reverse(Res);
format_listener_specs([{Proto, Port} | T], Res)
  when is_atom(Proto), is_integer(Port), Proto == tcp6; Proto == tls6 ->
    case yxa_config:get_env(enable_v6) of
	{ok, true} ->
	    Id = {listener, Proto, Port},
	    MFA = {tcp_listener, start_link, [Proto, Port]},
	    Spec = {Id, MFA, permanent, brutal_kill, worker, [tcp_listener]},
	    format_listener_specs(T, [Spec | Res]);
	{ok, false} ->
	    format_listener_specs(T, Res)
    end;
format_listener_specs([{Proto, Port} | T], Res)
  when is_atom(Proto), is_integer(Port), Proto == tcp; Proto == tls ->
    Id = {listener, Proto, Port},
    MFA = {tcp_listener, start_link, [Proto, Port]},
    Spec = {Id, MFA, permanent, brutal_kill, worker, [tcp_listener]},
    format_listener_specs(T, [Spec | Res]).


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
%% Function: handle_call({get_socket, Dst}, From, State)
%%           Dst = sipdst record()
%% Descrip.: Look for a cached connection to the destination in Dst.
%%           If one is found, return {reply, ...} with it. If we are
%%           already trying to connect to this destination, queue this
%%           requestor with the existing tcp_connection process. Else,
%%           start a tcp_connection process that tries to connect to
%%           the Proto:Host:Port and will do gen_server:reply(...)
%%           when it either succeeds or fails. We must do it this way
%%           since we can't block the tcp_dispatcher process. There is
%%           a race here where we might end up having more than one
%%           connection to Proto:Host:Port at the same time, but that
%%           should be OK.
%% Returns : {reply, Reply, NewState, ?TIMEOUT} |
%%           {noreply, NewState, ?TIMEOUT}
%%           Reply = {ok, SipSocket} |
%%                   {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
handle_call({get_socket, Dst}, From, State) when is_record(Dst, sipdst) ->
    case get_socket_from_list(Dst, State#state.socketlist) of
	none ->
	    %% We must spawn a tcp_connection process to take care of making this new connection
	    %% since the tcp_dispatcher may not be blocked by time consuming operations. The spawned
	    %% process will do a gen_server:reply(...).
	    case tcp_connection:start_link(connect, Dst, From) of
		{ok, CH} ->
		    logger:log(debug, "TCP dispatcher: No cached connection to remote destination ~s, trying to "
			       "connect (started TCP connection handler ~p)", [sipdst:dst2str(Dst), CH]),
		    {noreply, State, ?TIMEOUT};
		ignore ->
		    {noreply, State, ?TIMEOUT};
		E ->
		    logger:log(error, "TCP dispatcher: Failed starting TCP connection handler for destination "
			       "~s : ~p", [sipdst:dst2str(Dst), E]),
		    {reply, {error, "tcp_connection handler problem"}, State, ?TIMEOUT}
	    end;
	{error, E} ->
	    {reply, {error, E}, State, ?TIMEOUT};
	SipSocket when is_record(SipSocket, sipsocket) ->
	    logger:log(debug, "TCP dispatcher: Using existing connection to ~s", [sipdst:dst2str(Dst)]),
	    {reply, {ok, SipSocket}, State, ?TIMEOUT}
    end;


%%--------------------------------------------------------------------
%% Function: handle_call({register_sipsocket, Type, SipSocket}, From,
%%                       State)
%%           Type = in | out | listener, Direction (or, who initiated
%%                                                  the socket)
%%           SipSocket = sipsocket record()
%% Descrip.: Add a socket to our list. Called by tcp_connection
%%           handlers when they have established a connection (inbound
%%           or outbound).
%% Returns : {reply, Reply, NewState, ?TIMEOUT}
%%           Reply = ok              |
%%                   {error, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
handle_call({register_sipsocket, Type, SipSocket}, _From, State) when is_atom(Type), is_record(SipSocket, sipsocket) ->
    CPid = SipSocket#sipsocket.pid,
    {Local, Remote} = SipSocket#sipsocket.data,
    Proto = SipSocket#sipsocket.proto,
    Ident = case Type of
		listener ->
		    {_IP, Port} = Local,
		    {listener, Proto, Port};
		in ->
		    {from, Proto, Remote};
		out ->
		    {to, Proto, Remote}
	    end,
    %% Socket expiration not implemented. Perhaps not even needed. If you are thinking of
    %% implementing it remember that listening sockets should always have timeout 0.
    Timeout = 0,	
    case socketlist:add(Ident, CPid, Proto, Local, Remote, SipSocket, Timeout, State#state.socketlist) of
	{error, E} ->
	    logger:log(error, "TCP dispatcher: Failed adding ~p to socketlist", [Ident]),
	    {reply, {error, E}, State, ?TIMEOUT};
	NewSocketList1 ->
	    {reply, ok, State#state{socketlist=NewSocketList1}, ?TIMEOUT}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call({get_socketlist}, From, State)
%% Descrip.: The stack monitor is requesting our list of connections.
%% Returns : {reply, {ok, List} State, ?TIMEOUT}
%%           List = socketlist record()
%%--------------------------------------------------------------------
handle_call({get_socketlist}, _From, State) ->
    {reply, {ok, State#state.socketlist}, State, ?TIMEOUT};

handle_call({quit}, _From, State) ->
    logger:log(debug, "TCP dispatcher: Shutting down upon receiving {quit}"),
    {stop, normal, State};

handle_call(Msg, _From, State) ->
    logger:log(error, "TCP dispatcher: Received unknown gen_server call : ~p", [Msg]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast(Msg, State) ->
    logger:log(error, "TCP dispatcher: Received unknown gen_server cast : ~p", [Msg]),
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
%% Descrip.: Wake up and delete expired sockets from our list.
%% Returns : {reply, Reply, NewState, ?TIMEOUT} |
%%           Reply = ok              |
%%                   {error, Reason}
%%           Reason    = string()
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    %% XXX not sure we actually ever add sockets with an expire time set
    SocketList1 = socketlist:delete_expired(State#state.socketlist),
    {noreply, State#state{socketlist=SocketList1}, ?TIMEOUT};

%%--------------------------------------------------------------------
%% Function: handle_info({'EXIT', Pid, Reason}, State)
%%           Pid    = pid()
%%           Reason = normal | term()
%% Descrip.: Trap exit signals from socket handlers and act on them.
%%           Log if they exit with an error, and remove them from our
%%           list of existing sockets.
%% Returns : {noreply, NewState, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
	normal -> logger:log(debug, "TCP dispatcher: Received normal exit-signal from process ~p", [Pid]);
	shutdown -> logger:log(debug, "TCP dispatcher: Received 'shutdown' exit-signal from process ~p", [Pid]);
	killed -> logger:log(debug, "TCP dispatcher: Received 'killed' exit-signal from process ~p", [Pid]);
	_ -> logger:log(error, "TCP dispatcher: =ERROR REPORT==== Received non-normal exit signal "
			"from process ~p :~n~p", [Pid, Reason])
    end,
    %% Remove any entrys for this pid from transportlayer_tcp_conn_queue
    lists:map(fun({QKey, QPid}) when QPid == Pid ->
		      ets:delete(transportlayer_tcp_conn_queue, QKey);
		 ({_DestTuple, _OtherQPid}) ->
		      ok
	      end, ets:tab2list(transportlayer_tcp_conn_queue)),
    NewState = case socketlist:get_using_pid(Pid, State#state.socketlist) of
		   none ->
		       case Reason of
			   normal ->
			       logger:log(debug, "TCP dispatcher: Received normal exit signal "
					  "from ~p not in my list.", [Pid]);
			   _ ->
			       logger:log(error, "TCP dispatcher: Received non-normal exit signal "
					  "from ~p not in my list.", [Pid]),
			       logger:log(debug, "TCP dispatcher: Socketlist is :~n~p",
					  [socketlist:debugfriendly(State#state.socketlist)])
		       end,
		       State;
		   L when is_record(L, socketlist) ->
		       NewL = socketlist:delete_using_pid(Pid, State#state.socketlist),
		       logger:log(debug, "TCP dispatcher: Deleting ~p entry(s) from socketlist :~n~p~n"
				  "(new list is ~p entry(s))", [socketlist:get_length(L),
								socketlist:debugfriendly(L),
								socketlist:get_length(NewL)]),
		       case ets:lookup(yxa_sipsocket_info, Pid) of
			   [{Pid, Y}] when is_record(Y, yxa_sipsocket_info_e) ->
			       logger:log(debug, "TCP dispatcher: Deleted listener ~p (~p:~s:~p) from "
					  "sipsocket info list", [Pid, Y#yxa_sipsocket_info_e.proto,
								  Y#yxa_sipsocket_info_e.addr,
								  Y#yxa_sipsocket_info_e.port]),
			       ets:delete(yxa_sipsocket_info, Pid);
			   [] -> ok
		       end,
		       State#state{socketlist = NewL}
	       end,
    {noreply, NewState, ?TIMEOUT};

handle_info(Unknown, State) ->
    logger:log(error, "TCP dispatcher: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
        normal -> logger:log(debug, "TCP dispatcher: terminating normally");
        shutdown -> logger:log(debug, "TCP dispatcher: shutting down");
        _ -> logger:log(error, "TCP dispatcher: terminating : ~p", [Reason])
    end,
    Reason.

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
%% Function: get_socket_from_list(Dst, SocketList)
%%           Dst        = sipdst record()
%%           SocketList = socketlist record()
%% Descrip.: Look for an entry matching Dst in SocketList
%% Returns : SipSocket |
%%           none
%%           SipSocket = sipsocket record()
%%--------------------------------------------------------------------
get_socket_from_list(#sipdst{proto=Proto}=Dst, SocketList) when Proto == tcp; Proto == tcp6;
								Proto == tls; Proto == tls6 ->
    {Host, Port} = {Dst#sipdst.addr, Dst#sipdst.port},
    case socketlist:get_using_remote(Proto, {Host, Port}, SocketList) of
	SListElem when is_record(SListElem, socketlistelem) ->
	    case (Proto == tls) or (Proto == tls6) of
		true ->
		    %% XXX this is a TLS socket, we must check that the cached connection is valid
		    %% considering the ssl_hostname in Dst!
		    logger:log(debug, "Warning: Reusing cached connection to TLS destination without "
			       "verifying that the certificate is valid for ssl_names ~p!",
			       [Dst#sipdst.ssl_names]);
		false ->
		    ok
	    end,
	    [CPid, SipSocket] = socketlist:extract([pid, sipsocket], SListElem),
	    logger:log(debug, "Sipsocket TCP: Reusing existing connection to ~s (~p)",
		       [sipdst:dst2str(Dst), CPid]),
	    SipSocket;
	_ ->
	    none
    end.
