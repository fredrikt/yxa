%%%-------------------------------------------------------------------
%%% File    : sipsocket_udp.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Transportlayer UDP all-in-one handler.
%%%
%%% Created : 15 Dec 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipsocket_udp).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0
	 send/5,
	 is_reliable_transport/1,
	 get_socket/3
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {socket, socket6, socketlist}).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SOCKETOPTS, [{reuseaddr, true}]).
%% v6 sockets have a default receive buffer size of 1k in Erlang R9C-0
-define(SOCKETOPTSv6, [{reuseaddr, true}, inet6, {buffer, 8 * 1024}]).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/1
%% Descrip.: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, sipsocket_udp}, ?MODULE, [], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([])
%% Descrip.: Initiates the server
%% Returns : {ok, State}    |
%%           {stop, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
init([]) ->
    Port = sipserver:get_listenport(udp),  %% same for UDP and UDPv6
    start_listening([udp, udp6], Port, #state{socketlist=socketlist:empty()}).

%%--------------------------------------------------------------------
%% Function: start_listening(ProtoList, Port, State)
%%           ProtoList = list() of atom(), udp | udp6
%%           Port      = integer()
%% Descrip.: Begin listening on port Port.
%% Returns : {ok, State}    |
%%           {stop, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
start_listening([], Port, State) ->
    {ok, State};
start_listening([udp | T], Port, State) when integer(Port), record(State, state) ->
    case gen_udp:open(Port, ?SOCKETOPTS) of
	{ok, Socket} ->
	    Local = get_localaddr(Socket, "0.0.0.0"),
	    SipSocket = #sipsocket{module=sipsocket_udp, proto=udp, pid=self(), data={Local, none}},
	    NewSocketList = socketlist:add({listener, udp}, self(), Local, none, SipSocket, 0, State#state.socketlist),
	    start_listening(T, Port, State#state{socket=Socket, socketlist=NewSocketList});
	{error, Reason} ->
	    logger:log(error, "Could not open UDP socket (options ~p), port ~p : ~s",
		       [?SOCKETOPTS, Port, inet:format_error(Reason)]),
	    {stop, "Could not open UDP socket"}
    end;
start_listening([udp6 | T], Port, State) when integer(Port), record(State, state) ->
    case sipserver:get_env(enable_v6, false) of
	true ->
	    case gen_udp:open(Port, ?SOCKETOPTSv6) of
		{ok, Socket} ->
		    Local = get_localaddr(Socket, "[::]"),
		    SipSocket = #sipsocket{module=sipsocket_udp, proto=udp6, pid=self(), data={Local, none}},
		    NewSocketList = socketlist:add({listener, udp6}, self(), Local, none, SipSocket, 0, State#state.socketlist),
		    start_listening(T, Port, State#state{socket6=Socket, socketlist=NewSocketList});
		{error, Reason} ->
		    logger:log(error, "Could not open IPv6 UDP socket (options ~p), port ~p : ~s",
			       [?SOCKETOPTSv6, Port, inet:format_error(Reason)]),
		    {stop, "Could not open IPv6 UDP socket"}
	    end;
	_ ->
	    start_listening(T, Port, State)
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
%% Function: handle_call({get_socket, Proto, Host, Port}, From, State)
%%           Proto = atom(), udp | udp6
%%           Host  = string()
%%           Port  = integer()
%% Descrip.: Get a socket for a certain protocol (Proto).
%%           sipsocket_udp is currently not multi-socket, we just have
%%           a singe UDP socket for each protocol and that is
%%           'listener'. Locate and return it.
%% Returns : {reply, Reply, State}
%%           Reply = {ok, SipSocket} |
%%                   {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
handle_call({get_socket, Proto, _Host, _Port}, From, State) when is_atom(Proto) ->
    %% sipsocket_udp is currently not multi-socket, we just have a singe UDP socket and that is 'listener'
    %% for each Proto.
    Id = {listener, Proto},
    case socketlist:get_using_id(Id, State#state.socketlist) of
	[] ->
	    logger:log(error, "Sipsocket UDP: Failed fetching socket with id '~p' from list :~n~p",
		       [Id, socketlist:debugfriendly(State#state.socketlist)]),
	    {reply, {error, "UDP socket for specified protocol not found"}, State};
	SElem ->
	    [CPid, Local, Remote] = socketlist:extract([pid, local, remote], SElem),
	    SipSocket = #sipsocket{module=sipsocket_udp, proto=Proto, pid=CPid, data={Local, Remote}},
	    {reply, {ok, SipSocket}, State}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call({send, {Host, Port, Message}}, From, State)
%%           Host    = string()
%%           Port    = integer()
%%           Message = term()
%% Descrip.: Send Message to Host:Port.
%% Returns : {reply, Reply, State}
%%           Reply = {send_result, Res}
%%           Res = term()
%%--------------------------------------------------------------------
handle_call({send, {Host, Port, Message}}, From, State) when integer(Port) ->
    %% Unfortunately there seems to be no way to receive ICMP port unreachables when sending with gen_udp...
    SendRes = case Host of
		  "[" ++ Rest ->
		      %% XXX use inet_parse:ipv6_address() ?
		      %% IPv6 address (e.g. [2001:6b0:5:987::1])
		      case string:rchr(Rest, $]) of
			  0 ->
			      {error, "Uknown format of destination"};
			  Index when integer(Index) ->
			      H = string:substr(Rest, 1, Index - 1),
			      gen_udp:send(State#state.socket6, H, Port, Message)
		      end;
		  _ ->
		      gen_udp:send(State#state.socket, Host, Port, Message)
	      end,
    {reply, {send_result, SendRes}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    logger:log(error, "Sipsocket UDP: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info({udp, Socket, IPlist, InPortNo, Packet},
%%                       State)
%%           Socket   = term()
%%           IPlist   = term()
%%           InPortNo = integer()
%%           Packet   = list()
%% Descrip.: Handle data received (as a signal) from one of our
%%           sockets. Spawn sipserver:process() on each message.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_info({udp, Socket, IPlist, InPortNo, Packet}, State) ->
    Sv4 = State#state.socket,
    Sv6 = State#state.socket6,
    case Socket of
	Sv4 ->
	    SipSocket = #sipsocket{module=sipsocket_udp, proto=udp, pid=self(), data=none},
	    Origin = #siporigin{proto=udp, addr=siphost:makeip(IPlist), port=InPortNo, receiver=self(), sipsocket=SipSocket},
	    sipserver:safe_spawn(sipserver, process, [Packet, Origin, transaction_layer]);
	Sv6 ->
	    SipSocket = #sipsocket{module=sipsocket_udp, proto=udp6, pid=self(), data=none},
	    Origin = #siporigin{proto=udp6, addr=siphost:makeip(IPlist), port=InPortNo, receiver=self(), sipsocket=SipSocket},
	    sipserver:safe_spawn(sipserver, process, [Packet, Origin, transaction_layer]);
	_ ->
	    logger:log(error, "Sipsocket UDP: Received gen_server info 'udp' from unknown source '~p', ignoring",
		       [Socket])
    end,
    {noreply, State};

handle_info(Info, State) ->
    logger:log(error, "Sipsocket UDP: Received unknown gen_server info : ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    case Reason of
        normal -> true;
        _ -> logger:log(error, "UDP listener terminating : ~p", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Purpose : Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: get_localaddr(Socket, DefaultAddr)
%% Descrip.: Return the listening IP and port for a socket.
%% Returns : {IP, Port}
%%           IP   = string()
%%           Port = integer()
%%--------------------------------------------------------------------
get_localaddr(Socket, DefaultAddr) ->
    case inet:sockname(Socket) of
	{ok, {IPlist, LocalPort}} ->
	    IP = siphost:makeip(IPlist),
	    logger:log(debug, "Listening on UDP ~s:~p (socket ~p)", [IP, LocalPort, Socket]),
	    {IP, LocalPort};
	{error, E} ->
	    %% XXX maybe we don't have a good socket after all?
	    logger:log(error, "UDP listener: sockname() on socket ~p returned error ~p", [Socket, E]),
	    {DefaultAddr, 0}
    end.


%%--------------------------------------------------------------------
%%% Interface functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: send(SipSocket, Proto, Host, Port, Message)
%%           SipSocket = sipsocket record()
%%           Proto     = atom(), udp | udp6
%%           Host      = string()
%%           Port      = integer()
%%           Message   = term()
%% Descrip.: Send Message to Host:Port. Proto must currently be the
%%           same protocol as is stored in SipSocket.
%% Returns : Result |
%%           {error, Reason}
%%           Result = term(), ultimately the result of send()
%%           Reason = string()
%%--------------------------------------------------------------------
send(SipSocket, Proto, Host, Port, Message) when record(SipSocket, sipsocket), integer(Port), SipSocket#sipsocket.proto /= Proto ->
    {error, "Protocol mismatch"};
send(SipSocket, Proto, Host, Port, Message) when record(SipSocket, sipsocket), integer(Port) ->
    Pid = SipSocket#sipsocket.pid,
    case catch gen_server:call(Pid, {send, {Host, Port, Message}}) of
	{send_result, Res} ->
	    Res;
	Unknown ->
	    logger:log(error, "Sipsocket UDP: Unknown send response from socket pid ~p for ~p:~s:~p : ~p",
                       [Pid, Proto, Host, Port, Unknown]),
	    {error, "Unknown sipsocket_udp send result"}
    end;
send(InvalidSocket, Proto, Host, Port, Message) ->
    logger:log(error, "Sipsocket UDP: Could not send message to ~p:~s:~p, invalid socket : ~p",
		[Proto, Host, Port, InvalidSocket]).

%%--------------------------------------------------------------------
%% Function: get_socket(Proto, Host, Port)
%%           Proto     = atom(), udp | udp6
%%           Host      = string()
%%           Port      = integer()
%% Descrip.: Return a socket suitable for communicating with Host:Port
%%           using protocol Proto.
%% Returns : SipSocket       |
%%           {error, Reason}
%%           SipSocket = sipsocket record()
%%           Reason    = string()
%%--------------------------------------------------------------------
get_socket(Proto, Host, Port) when atom(Proto), list(Host), integer(Port) ->
    case catch gen_server:call(sipsocket_udp, {get_socket, Proto, Host, Port}, 1500) of
	{ok, SipSocket} ->
	    SipSocket;
	{error, E} ->
	    logger:log(error, "Sipsocket UDP: Failed fetching socket for ~s:~p : ~p", [Host, Port, E]),
	    {error, E};
	Unknown ->
	    logger:log(error, "Sipsocket UDP: Unknown get_socket response from sipsocket_udp ~s:~p : ~p",
		       [Host, Port, Unknown]),
	    {error, "sipsocked_udp failed"}
    end.

%%--------------------------------------------------------------------
%% Function: is_reliable_transport(_)
%% Descrip.: No UDP protocol is reliable transport. Return false.
%% Returns : false
%%--------------------------------------------------------------------
is_reliable_transport(_) ->
    false.
