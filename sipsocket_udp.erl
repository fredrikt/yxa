%%%-------------------------------------------------------------------
%%% File    : sipsocket_udp.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : Transportlayer UDP all-in-one handler.
%%%
%%% Created : 15 Dec 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipsocket_udp).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1]).

-export([send/4, is_reliable_transport/1, get_socket/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, socketlist}).

-include("sipsocket.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, sipsocket_udp}, ?MODULE, [Port], []).

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
init([Port]) ->
    case gen_udp:open(Port, [{reuseaddr, true}]) of
	{ok, Socket} ->
	    Local = case inet:sockname(Socket) of
			{ok, {IPlist, LocalPort}} ->
			    IP = siphost:makeip(IPlist),
			    logger:log(debug, "Listening on UDP ~s:~p (socket ~p)", [IP, LocalPort, Socket]),
			    {siphost:makeip(IPlist), LocalPort};
			{error, E} ->
			    logger:log(error, "UDP listener: sockname() on socket ~p returned error ~p", [Socket, E]),
			    {"0.0.0.0", 0}
		    end,
	    SocketList = socketlist:add(listener, self(), Local, {"0.0.0.0", 0}, 0, socketlist:empty()),
	    {ok, #state{socket=Socket, socketlist=SocketList}};

	{error, Reason} ->
	    logger:log(error, "Could not open UDP socket, port ~p : ~s",
		       [Port, inet:format_error(Reason)]),
	    {stop, "Could not open UDP socket"}
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
handle_call({get_socket, Host, Port}, From, State) ->
    %% sipsocket_udp is currently not multi-socket, we just have a singe UDP socket and that is 'listener'.
    case socketlist:get_using_id(listener, State#state.socketlist) of
	[] ->
	    logger:log(error, "Sipsocket UDP: Failed fetching socket with id 'listener' from list :~n~p",
		       [socketlist:debugfriendly(State#state.socketlist)]),
	    {reply, {error, "UDP socket not found"}, State};
	SElem ->
	    [CPid, Local, Remote] = socketlist:extract([pid, local, remote], SElem),
	    SipSocket = #sipsocket{module=sipsocket_udp, pid=CPid, data={Local, Remote}},
	    {reply, {ok, SipSocket}, State}
    end;

handle_call({send, {SendToHost, PortInt, Message}}, From, State) ->
    %% Unfortunately there seems to be no way to receive ICMP port unreachables in erlang gen_udp...
    SendRes = gen_udp:send(State#state.socket, SendToHost, PortInt, Message),
    {reply, {send_result, SendRes}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    logger:log(error, "Sipsocket UDP: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_info({udp, Socket, IPlist, InPortNo, Packet}, State) ->
    SipSocket = #sipsocket{module=sipsocket_udp, pid=self(), data=none},
    Origin = {sipsocket:sipproto2str(sipsocket_udp), siphost:makeip(IPlist), InPortNo, SipSocket},
    sipserver:safe_spawn(sipserver, process, [Packet, SipSocket, Origin, transaction_layer, transaction_layer]),
    {noreply, State};

handle_info(Info, State) ->
    logger:log(error, "Sipsocket UDP: Received unknown gen_server info : ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    case Reason of
        normal -> true;
        _ -> logger:log(error, "UDP listener terminating : ~p", [Reason])
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

%%--------------------------------------------------------------------
%%% Interface functions
%%--------------------------------------------------------------------


send(SipSocket, SendToHost, PortInt, Message) when record(SipSocket, sipsocket) ->
    Pid = SipSocket#sipsocket.pid,
    case catch gen_server:call(Pid, {send, {SendToHost, PortInt, Message}}) of
	{send_result, Res} ->
	    Res;
	Unknown ->
	    logger:log(error, "Sipsocket UDP: Unknown send response from socket pid ~p for ~s:~p : ~p",
                       [Pid, SendToHost, PortInt, Unknown]),
	    {error, "Unknown sipsocket_udp send result"}
    end;
send(InvalidSocket, SendToHost, PortInt, Message) ->
    logger:log(error, "Sipsocket UDP: Could not send message to ~p:~p, invalid socket : ~p",
		[SendToHost, PortInt, InvalidSocket]).

get_socket(Host, Port) when list(Host), integer(Port) ->
    case catch gen_server:call(sipsocket_udp, {get_socket, Host, Port}, 1500) of
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

is_reliable_transport(_) -> false.


