%%%-------------------------------------------------------------------
%%% File    : tcp_dispatcher.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      TCP dispatcher initially does gen_tcp:listen() and
%%%           then keeps track of all existing TCP connections.
%%%
%%% @since    12 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
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

%% @type state() = #state{}.
%%                 no description
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
%% @spec    () -> term()
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, tcp_dispatcher}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec    () ->
%%            {ok, SocketList}
%%
%%            SocketList = #socketlist{}
%%
%% @doc     Get the complete list of sockets from the tcp_dispatcher.
%% @end
%%--------------------------------------------------------------------
get_socketlist() ->
    gen_server:call(tcp_dispatcher, {get_socketlist}).

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
    %% This is a system process that traps EXIT signals from TCP/TLS connection handlers
    process_flag(trap_exit, true),
    State = #state{socketlist = socketlist:empty()
		  },
    ets:new(transportlayer_tcp_conn_queue, [public, set, named_table]),
    {ok, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec    () ->
%%            SupSpec
%%
%%            SupSpec = term() "OTP supervisor child specification. Extra processes this application want the sipserver_sup to start and maintain."
%%
%% @doc     Get an OTP supservisor child specification for all TCP
%%          listeners.
%% @end
%%--------------------------------------------------------------------
get_listenerspecs() ->
    Port = sipsocket:get_listenport(tcp),
    TLSport = sipsocket:get_listenport(tls),

    {ok, DisableTLS} = yxa_config:get_env(tls_disable_server),

    IPv4Specs =
	lists:foldl(fun(IP, Acc) ->
			    TLS =
				case DisableTLS of
				    true -> [];
				    false -> [{tls, IP, TLSport}]
				end,
			    This = [{tcp, IP, Port} | TLS],
			    This ++ Acc
%%		    end, [], lists:reverse(siphost:myip_list() ++ ["127.0.0.1"])),
		    end, [], lists:reverse(siphost:myip_list())),

    IPv6Specs =
	lists:foldl(fun(IP, Acc) ->
			    TLS =
				case DisableTLS of
				    true -> [];
				    false ->
					%% XXX add tls6 to this list when there is an Erlang version released
					%% than has a ssl.erl that handles inet6. Current version (R9C-0) treats
					%% inet6 as an invalid gen_tcp option.
					%% [{tls6, IP, TLSport}]
					[]
				end,
			    This = [{tcp6, IP, Port} | TLS],
			    This ++ Acc
		    end, [], ["::"]),

    Listeners = IPv4Specs ++ IPv6Specs,

    format_listener_specs(Listeners).

%%--------------------------------------------------------------------
%% @spec    (L) ->
%%            SupSpec
%%
%%            L     = [{Proto, Port}]
%%            Proto = atom()
%%            Port  = integer()
%%
%%            SupSpec = term() "OTP supervisor child specification. Extra processes this application want the sipserver_sup to start and maintain."
%%
%% @doc     Format a OTP supservisor child specification for each
%%          entry in L.
%% @end
%%--------------------------------------------------------------------
format_listener_specs(L) ->
    format_listener_specs(L, []).

format_listener_specs([], Res) ->
    lists:reverse(Res);
format_listener_specs([{Proto, IP, Port} | T], Res)
  when is_atom(Proto), is_list(IP), is_integer(Port), Proto == tcp6; Proto == tls6 ->
    case yxa_config:get_env(enable_v6) of
	{ok, true} ->
	    Id = {listener, Proto, IP, Port},
	    {ok, IPt} = inet_parse:ipv6_address(IP),
	    MFA = {tcp_listener, start_link, [IPt, Proto, Port]},
	    Spec = {Id, MFA, permanent, brutal_kill, worker, [tcp_listener]},
	    format_listener_specs(T, [Spec | Res]);
	{ok, false} ->
	    format_listener_specs(T, Res)
    end;
format_listener_specs([{Proto, IP, Port} | T], Res)
  when is_atom(Proto), is_list(IP), is_integer(Port), Proto == tcp; Proto == tls ->
    Id = {listener, Proto, IP, Port},
    {ok, IPt} = inet_parse:ipv4_address(IP),
    MFA = {tcp_listener, start_link, [IPt, Proto, Port]},
    Spec = {Id, MFA, permanent, brutal_kill, worker, [tcp_listener]},
    format_listener_specs(T, [Spec | Res]).


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
%% @spec    ({get_socket, Dst}, From, State) ->
%%            {reply, Reply, NewState, Timeout::integer()} |
%%            {noreply, NewState, Timeout::integer()}
%%
%%            Dst = #sipdst{}
%%
%%            Reply     = {ok, SipSocket} | {error, Reason}
%%            SipSocket = #sipsocket{}
%%            Reason    = string()
%%
%% @doc     Look for a cached connection to the destination in Dst. If
%%          one is found, return {reply, ...} with it. If we are
%%          already trying to connect to this destination, queue this
%%          requestor with the existing tcp_connection process. Else,
%%          start a tcp_connection process that tries to connect to
%%          the Proto:Host:Port and will do gen_server:reply(...)
%%          when it either succeeds or fails. We must do it this way
%%          since we can't block the tcp_dispatcher process. There is
%%          a race here where we might end up having more than one
%%          connection to Proto:Host:Port at the same time, but that
%%          should be OK.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({get_socket, Dst}, From, State) when is_record(Dst, sipdst) ->
    case get_socket_from_list(Dst, State#state.socketlist) of
	none ->
	    %% We must spawn a tcp_connection process to take care of making this new connection
	    %% since the tcp_dispatcher may not be blocked by time consuming operations. The spawned
	    %% process will do a gen_server:reply(...).
	    case tcp_connection:connect_to(Dst, From) of
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
	    {reply, {ok, SipSocket}, State, ?TIMEOUT}
    end;


%%--------------------------------------------------------------------
%% @spec    ({get_specific_socket, Id}, From, State) ->
%%            {ok, SipSocket} |
%%            {error, Reason}
%%
%%            Id = #ob_id{}
%%
%%            SipSocket = #sipsocket{}
%%            Reason    = string()
%%
%% @doc     Return a specific socket. Used by draft-Outbound implem-
%%          entation to send requests using an existing flow, or not
%%          at all.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({get_specific_socket, Id}, _From, State) when is_record(Id, ob_id) ->
    case get_specific_socket_from_list(Id, State#state.socketlist) of
	none ->
	    {reply, {error, "Specific socket not available"}, State, ?TIMEOUT};
	{error, E} ->
	    {reply, {error, E}, State, ?TIMEOUT};
	SipSocket when is_record(SipSocket, sipsocket) ->
	    {reply, {ok, SipSocket}, State, ?TIMEOUT}
    end;


%%--------------------------------------------------------------------
%% @spec    ({register_sipsocket, Type, SipSocket}, From, State) ->
%%            {reply, Reply, NewState, Timeout::integer()}
%%
%%            Type      = in | out | listener "Direction (or, who initiated the socket)"
%%            SipSocket = #sipsocket{}
%%
%%            Reply  = ok              | {error, Reason}
%%            Reason = string()
%%
%% @doc     Add a socket to our list. Called by tcp_connection
%%          handlers when they have established a connection (inbound
%%          or outbound).
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({register_sipsocket, Type, SipSocket}, _From, State) when is_atom(Type), is_record(SipSocket, sipsocket) ->
    CPid = SipSocket#sipsocket.pid,
    %% Socket expiration not implemented. Perhaps not even needed. If you are thinking of
    %% implementing it remember that listening sockets should always have timeout 0.
    Timeout = 0,
    case link(CPid) of
	true ->
	    case socketlist:add(Type, CPid, SipSocket, Timeout, State#state.socketlist) of
		{ok, NewSocketList1} ->
		    {reply, ok, State#state{socketlist = NewSocketList1}, ?TIMEOUT};
		{error, E} ->
		    {reply, {error, E}, State, ?TIMEOUT}
	    end;
	_ ->
	    logger:log(error, "TCP dispatcher: Failed linking to socket handler (pid ~p)", [CPid]),
	    {reply, {error, "Link failed"}, State, ?TIMEOUT}
    end;

%%--------------------------------------------------------------------
%% @spec    ({get_socketlist}, From, State) ->
%%            {reply, {ok, List} State, Timeout::integer()}
%%
%%            List = #socketlist{}
%%
%% @doc     The stack monitor is requesting our list of connections.
%% @hidden
%% @end
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
%% @spec    (Unknown, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown cast.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "TCP dispatcher: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.


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
%% @spec    (timeout, State) ->
%%            {reply, Reply, NewState, Timeout::integer()} 
%%
%%            Reply  = ok | {error, Reason}
%%            Reason = string()
%%
%% @doc     Wake up and delete expired sockets from our list.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    %% XXX not sure we actually ever add sockets with an expire time set
    SocketList1 = socketlist:delete_expired(State#state.socketlist),
    {noreply, State#state{socketlist=SocketList1}, ?TIMEOUT};

%%--------------------------------------------------------------------
%% @spec    ({'EXIT', Pid, Reason}, State) ->
%%            {noreply, NewState, Timeout::integer()}
%%
%%            Pid    = pid()
%%            Reason = normal | term()
%%
%% @doc     Trap exit signals from socket handlers and act on them.
%%          Log if they exit with an error, and remove them from our
%%          list of existing sockets.
%% @hidden
%% @end
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
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
        normal -> logger:log(debug, "TCP dispatcher: terminating normally");
        shutdown -> logger:log(debug, "TCP dispatcher: shutting down");
        _ -> logger:log(error, "TCP dispatcher: terminating : ~p", [Reason])
    end,
    Reason.

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
%% @spec    (Dst, SocketList) ->
%%            SipSocket |
%%            none
%%
%%            Dst        = #sipdst{}
%%            SocketList = #socketlist{}
%%
%%            SipSocket = #sipsocket{}
%%
%% @doc     Look for an entry matching Dst in SocketList
%% @end
%%--------------------------------------------------------------------
get_socket_from_list(#sipdst{proto = Proto} = Dst, SocketList) when Proto == tcp; Proto == tcp6;
								    Proto == tls; Proto == tls6 ->
    case socketlist:get_using_remote(Proto, Dst#sipdst.addr, Dst#sipdst.port, SocketList) of
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
	none ->
	    none
    end.



get_specific_socket_from_list(Id, SocketList) when is_record(Id, ob_id), is_record(SocketList, socketlist) ->
    case socketlist:get_using_socketid(Id, SocketList) of
	SListElem when is_record(SListElem, socketlistelem) ->
	    [CPid, SipSocket] = socketlist:extract([pid, sipsocket], SListElem),
	    logger:log(debug, "Sipsocket TCP: Reusing (specific) existing connection with id ~p (~p)",
		       [Id, CPid]),
	    SipSocket;
	none ->
	    none
    end.

