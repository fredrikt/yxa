%%%-------------------------------------------------------------------
%%% File    : tcp_receiver.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: TCP receiver does blocking read on a single socket and
%%%           signals parent when a complete SIP request/response has
%%%           been received.
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------

-module(tcp_receiver).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/5]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([recv_loop/2]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  socketmodule,
	  socket,
	  parent,
	  local,
	  remote,
	  sipsocket,
	  linked=no
	 }).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/6
%% Descrip.: Spawn a tcp_receiver process into it's recv_loop().
%% Returns : void()
%%--------------------------------------------------------------------
start_link(SocketModule, Socket, Local, Remote, SipSocket) ->
    State = #state{socketmodule=SocketModule, socket=Socket, parent=self(), local=Local, remote=Remote, sipsocket=SipSocket},
    Pid = spawn_link(?MODULE, recv_loop, [State, []]),
    Pid.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: recv_loop(State, DataIn)
%%           State  = state record()
%%           DataIn = list()
%% Descrip.: SSL version: Wait for data from our socket. When data is
%%           available we check to see if we have received a complete
%%           SIP message yet. If so, continue process it. Then loop
%%           back to self. If we detect that the socket has been
%%           closed, tell our parent before we break out of the loop
%%           and terminate.
%% Returns : void()
%%--------------------------------------------------------------------
recv_loop(#state{linked=no, socketmodule=ssl}=State, DataIn) when is_list(DataIn) ->
    %% Socket is SSL and this is the first time we enter recv_loop(). Link to sockets pid
    %% and set us up to receive exit signals from our parent and from the sockets pid.
    process_flag(trap_exit, true),
    SocketPid = ssl:pid(State#state.socket),
    true = link(SocketPid),
    recv_loop(State#state{linked=yes}, DataIn);
recv_loop(State, DataIn) when record(State, state), list(DataIn), State#state.socketmodule == ssl ->
    Socket = State#state.socket,
    SocketPid = ssl:pid(State#state.socket),
    %% Must set {active, once} here even if tcp_listener did it right before us -
    %% it seems as if the SSL socket looses this status when controlling process is changed.
    ssl:setopts(State#state.socket, [{active, once}]),
    Parent = State#state.parent,
    Rest = receive
	       {ssl, Socket, B} ->
		   L = binary_to_list(B),
		   logger:log(debug, "TCP receiver: Received ~p bytes of data (SSL), adding it to my buffer which contained ~p bytes of data",
			      [length(L), length(DataIn)]),
		   %% remove_separator() removes any leading CR or LF
		   NewDataIn = lists:append(DataIn, L),
		   SoFar = remove_separator(NewDataIn),
		   %% Look for header-body separator
		   case has_header_body_separator(SoFar) of
		       false ->
			   %% No Header Body separator yet, read more data
			   SoFar;
		       true ->
			   case header_received(SoFar, State) of
			       AdditionalData when is_list(AdditionalData) ->
				   logger:log(debug, "TCP receiver: My buffer now contains ~p bytes of data (SSL)", [length(AdditionalData)]),
				   AdditionalData;
			       AdditionalData2 ->
				   AdditionalData2
			   end
		   end;

	       {ssl_closed, Socket} ->
		   logger:log(debug, "TCP receiver: Extra debug: Socket closed (Socket: ~p, local ~p, remote ~p)",
			      [Socket, State#state.local, State#state.remote]),
		   {connection_closed};

	       {ssl_error, Socket, Reason} ->
		   logger:log(error, "TCP receiver: SSL error from socket ~p : ~p", [Socket, Reason]),
		   DataIn;

	       {quit_receiver, Parent} ->
		   logger:log(debug, "TCP receiver: Closing SSL socket ~p and terminating upon request from my parent",
			      [Socket]),
		   {quit};

	       {'EXIT', SocketPid, Reason} ->
		   logger:log(debug, "TCP receiver: SSL socket ~p terminated, shutting down. Reason was : ~p",
			      [Socket, Reason]),
		   {close};

	       {'EXIT', Parent, Reason} ->
		   logger:log(debug, "TCP receiver: SSL connection handler ~p terminated, shutting down. Reason was : ~p",
			      [Parent, Reason]),
		   {quit};

	       Unknown ->
		   logger:log(error, "TCP receiver: Received unknown signal :~n~p", [Unknown]),
		   DataIn
	   after 10 * 1000 ->
		   DataIn
	   end,
    case Rest of
	{connection_closed} ->
	    gen_server:cast(Parent, {connection_closed, self()});
	{close} ->
	    ssl:close(State#state.socket),	%% Maybe not needed, but better safe than sorry
	    gen_server:cast(Parent, {close, self()});
	{quit} ->
	    ssl:close(State#state.socket),
	    ok;
	_ when is_list(Rest) ->
	    recv_loop(State, Rest)
    end;

%%--------------------------------------------------------------------
%% Function: recv_loop(State, DataIn)
%%           State  = state record()
%%           DataIn = list()
%% Descrip.: Constantly try to read data from our socket. Loop back to
%%           self. If we detect that the socket has been closed, tell
%%           our parent before we break out of the loop and terminate.
%% Returns : void()
%%--------------------------------------------------------------------
recv_loop(State, DataIn) when record(State, state) ->
    Parent = State#state.parent,
    case do_recv(DataIn, State) of
	Data when list(Data) ->
	    recv_loop(State, Data);
	{connection_closed} ->
	    gen_server:cast(Parent, {connection_closed, self()});
	{close} ->
	    gen_server:cast(Parent, {close, self()});
	Unknown ->
	    logger:log(error, "TCP receiver: Dying after unknown result from do_recv() :~n~p", [Unknown]),
	    gen_server:cast(Parent, {close, self()})
    end.

%%--------------------------------------------------------------------
%% Function: do_recv(DataIn, State)
%%           State  = state record()
%%           DataIn = list()
%% Descrip.: Read data from our socket and add it to whatever data was
%%           supplied in DataIn until a SIP message header/body
%%           separator ("\r\n\r\n") is seen. It is possible that we
%%           also get part or a whole body of the message, but as soon
%%           as we have the header we use tcp_read_sip_message()
%%           instead.
%% Returns : DataOut             |
%%           {connection_closed} |
%%           {close}
%%           {error, Reason}
%%           DataOut = list(), whatever we have left in our buffer
%%                     when we have processed all SIP messages in it.
%%                     Will get passed to us as DataIn on the next
%%                     itteration.
%%           Reason = string()
%%--------------------------------------------------------------------
do_recv(DataIn, State) ->
    SocketModule = State#state.socketmodule,
    Socket = State#state.socket,
    case SocketModule:recv(Socket, 0) of
	{ok, B} ->
	    L = binary_to_list(B),
	    logger:log(debug, "TCP receiver: Received ~p bytes of data, adding it to my buffer which contained ~p bytes of data",
		       [length(L), length(DataIn)]),
	    %% remove_separator() removes any leading CR or LF
	    SoFar = remove_separator(lists:append(DataIn, L)),
	    case has_header_body_separator(SoFar) of
		false ->
		    %% No Header Body separator yet, read more data
		    do_recv(SoFar, State);
		true ->
		    case header_received(SoFar, State) of
			Rest when is_list(Rest) ->
			    logger:log(debug, "TCP receiver: My buffer now contains ~p bytes of data", [length(Rest)]),
			    Rest;
			Rest2 ->
			    Rest2
		    end
	    end;
	{error, closed} ->
	    logger:log(debug, "TCP receiver: Extra debug: ~p recv() returned 'closed' in do_recv() (Socket: ~p)",
		       [SocketModule, Socket]),
	    {connection_closed};
	{error, E} ->
	    logger:log(error, "TCP receiver: Error when reading data : ~s (~p)", [inet:format_error(E), E]),
	    {error, E}
    end.

%% Look for header-body separator, return true | false
has_header_body_separator(Data) ->
    case string:str(Data, "\r\n\r\n") of
	0 ->
	    false;
	N when is_integer(N) ->
	    true
    end.

%%--------------------------------------------------------------------
%% Function: header_received(Data, State)
%%           Data  = list()
%%           State = state record()
%% Descrip.: Data contains at least one SIP messages complete header.
%%           Parse it and hand it over to tcp_read_sip_message().
%%           If we aren't given a parseable header as input, we return
%%           a instruction to close this socket.
%%           When tcp_read_sip_message() has received a complete SIP
%%           message (and handled it), it might have received more
%%           data (part or whole of the next request/response sent to
%%           us on this socket). If that is the case we invoke
%%           ourselves again on this data in case it contains a
%%           header/body separator, or we return the extra bytes to
%%           our caller in case it does not.
%% Returns : DataOut             |
%%           {connection_closed} |
%%           {close}
%%           DataOut = list(), whatever we have left in our buffer
%%           when we have processed all SIP messages in it. Will get
%%           passed to us as DataIn on the next itteration.
%%--------------------------------------------------------------------
header_received(Data, State) when list(Data), record(State, state) ->
    Rest = case catch sippacket:parse(Data, none) of
	       Request when record(Request, request) ->
		   tcp_read_sip_message(request, Request#request.header, Request#request.body, Data, State);
	       Response when record(Response, response) ->
		   tcp_read_sip_message(response, Response#response.header, Response#response.body, Data, State);
	       _ ->
		   %% Data did not parse, close connection.
		   Socket = State#state.socket,
		   logger:log(debug, "TCP receiver: Received unparseable data - closing socket (~p) :~n~p",
			      [Socket, Data]),
		   {close}
	   end,
    case Rest of
	{connection_closed} ->
	    {connection_closed};
	{close} ->
	    {close};
	_ when list(Rest) ->
	    %% We received more data than the Content-Length indicated, might be pipelined requests?
	    %% Remove any CRLF sequences before (or between) requests, RFC3261 #7.5
	    NewData = remove_separator(lists:flatten(Rest)),
	    %% Check if there is already a complete request/response in NewData -
	    %% recurse on ourselves until that is not the case
	    case has_header_body_separator(NewData) of
		true  -> header_received(NewData, State);
		false -> NewData
	    end;
	_ ->
	    logger:log(error, "TCP receiver: Unknown result from sippacket:parse() or tcp_read_sip_message() :~n~p~n" ++
		       "Closing socket.", [Rest]),
	    {close}
    end.


%%--------------------------------------------------------------------
%% Function: tcp_read_sip_message(MessageType, Header, BodyIn, DataIn,
%%                                State)
%%           MessageType = request | response
%%           Header      = keylist record()
%%           BodyIn      = list(), whatever part of the body that was
%%                         received before we were invoked
%%           DataIn      = list(), the data we have received yet, in
%%                         unparsed form
%%           State       = state record()
%% Descrip.: A parseable header of a SIP message has been received
%%           (and possibly also parts of or the whole body). Calculate
%%           how much more data (if any) we must read to have the
%%           whole message (through the Content-Length header), and
%%           wait until we have received it all. Then send this SIP
%%           message to our parent.
%% Returns : DataOut             |
%%           {connection_closed} |
%%           {close}
%%           DataOut = list(), whatever we have left in our buffer
%%           when we have one (1) SIP messages in it.
%%--------------------------------------------------------------------
tcp_read_sip_message(MessageType, Header, BodyIn, DataIn, State) when record(State, state) ->
    {IP, Port} = State#state.remote,
    Parent = State#state.parent,
    Socket = State#state.socket,
    case get_content_length(Header) of
	CLen when integer(CLen) ->
	    Remaining = CLen - length(BodyIn),
	    case tcp_read_more_data(State#state.socketmodule, Socket, Remaining, DataIn) of
		Data when list(Data) ->
		    ThisMessage = string:substr(Data, 1, length(DataIn) + Remaining),
		    Rest = string:substr(Data, 1 + length(DataIn) + Remaining),
		    gen_server:cast(Parent, {recv, ThisMessage}),
		    %% We have received more data than just this request/response, like a pipelined request
		    Rest;
		{connection_closed} ->
		    logger:log(debug, "TCP receiver: Connection closed by foreign host when expecting ~p bytes more data", [CLen]),
		    {connection_closed};
		Res ->
		    logger:log(debug, "TCP receiver: Failed reading ~p bytes more data : ~p", [CLen, Res]),
		    []
	    end;
	Unknown ->
	    logger:log(debug, "TCP receiver: Non-integer Content-Length spotted : ~p", [Unknown]),
	    case MessageType of
		request ->
		    logger:log(debug, "TCP receiver: Non-existing or invalid Content-Length in request from ~s:~p, " ++
			       "(socket ~p), discarding message :~n~p", [IP, Port, Socket, DataIn]),
		    logger:log(error, "TCP receiver: Non-existing or invalid Content-Length in request from ~s:~p, " ++
			       "answering 400 Bad Request and closing socket", [IP, Port]),
		    SipSocket = State#state.sipsocket,
		    transportlayer:send_result(Header, SipSocket, "", 400, "Bad Request",
					       [{"Reason", ["Non-existing or invalid Content-Length"]}]);
		response ->
		    logger:log(debug, "TCP receiver: Non-existing or invalid Content-Length in response from ~s:~p " ++
			       "(socket ~p), discarding message :~n~p", [IP, Port, Socket, DataIn]),
		    logger:log(error, "TCP receiver: Non-existing or invalid Content-Length in response from ~s:~p, " ++
			       "discarding and closing socket.", [IP, Port])
	    end,
	    logger:log(debug, "TCP receiver: Closing socket ~p since there is no way to know we are synchronized with remote end", [Socket]),
	    {close}
    end.

%%--------------------------------------------------------------------
%% Function: get_content_length(Header)
%%           Header = keylist record()
%% Descrip.: Get Content-Length and return it as an integer value.
%% Returns : Length  |
%%           invalid
%%           Length = integer()
%%--------------------------------------------------------------------
get_content_length(Header) ->
    case keylist:fetch('content-length', Header) of
	[CLenStr] ->
	    case util:isnumeric(CLenStr) of
		true ->
		    %% Be extra careful and check for numeric value.
		    %% We don't have to worry about partially received Content-Length
		    %% header here since this function doesn't get invoked until
		    %% the header-body separator has been seen.
		    list_to_integer(CLenStr);
		false ->
		    invalid
	    end;
	_ ->
	    invalid
    end.

%%--------------------------------------------------------------------
%% Function: tcp_read_more_data(SocketModule, Socket, Len, DataIn)
%%           SocketModule = atom(), socket module to use
%%           Socket       = term(), the socket
%%           Len          = integer(), how much more data to wait for
%%           DataIn       = list()
%% Descrip.: SSL version: Wait for a pre-determined ammount of more
%%           data from a socket.
%% Returns : DataOut             |
%%           {connection_closed} |
%%           {error, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
tcp_read_more_data(SocketModule, Socket, Len, DataIn) when Len > 0, SocketModule == ssl ->
    receive
	{ssl, Socket, B} ->
	    Data = binary_to_list(B),
	    L = length(Data),
	    logger:log(debug, "TCP receiver: Extra debug: Received ~p bytes of data (~p more expected)",
		       [L, Len - L]),
	    SoFar = lists:append(DataIn, Data),
	    SocketModule:setopts(Socket, [{active, once}]),
	    tcp_read_more_data(SocketModule, Socket, Len - length(Data), SoFar);

	{ssl_closed, Socket} ->
	    logger:log(debug, "TCP receiver: Extra debug: Got message that socket ~p was closed tcp_read_more_data()",
		       [Socket]),
	    {connection_closed};

	{ssl_error, Socket, E} ->
	    logger:log(error, "TCP receiver: Error when reading ~p bytes of data : ~s (~p)",
		       [Len, SocketModule:format_error(E), E]),
	    {error, E}
    end;

%%--------------------------------------------------------------------
%% Function: tcp_read_more_data(SocketModule, Socket, Len, DataIn)
%%           SocketModule = atom(), socket module to use
%%           Socket       = term(), the socket
%%           Len          = integer(), how much more data to read
%%           DataIn       = list()
%% Descrip.: Read a pre-determined ammount of more data from a socket.
%% Returns : DataOut             |
%%           {connection_closed} |
%%           {error, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
tcp_read_more_data(SocketModule, Socket, Len, DataIn) when Len > 0 ->
    case SocketModule:recv(Socket, 0) of
	{ok, B} ->
	    Data = binary_to_list(B),
	    L = length(Data),
	    logger:log(debug, "TCP receiver: Extra debug: Received ~p bytes of data (~p more expected)",
		       [L, Len - L]),
	    SoFar = lists:append(DataIn, Data),
	    tcp_read_more_data(SocketModule, Socket, Len - length(Data), SoFar);
	{error, closed} ->
	    logger:log(debug, "TCP receiver: Extra debug: ~p recv() returned 'closed' in tcp_read_more_data() (Socket: ~p)",
		       [SocketModule, Socket]),
	    {connection_closed};
	{error, E} ->
	    logger:log(error, "TCP receiver: Error when reading ~p bytes of data : ~s (~p)",
		       [Len, inet:format_error(E), E]),
	    {error, E}
    end;

tcp_read_more_data(_SocketModule, _Socket, _Len, In) ->
    In.

%%--------------------------------------------------------------------
%% Function: remove_separator(Data)
%%           Data = list()
%% Descrip.: RFC3261 7.5. Remove linefeeds between messages (often
%%           used as keepalive messages).
%% Returns : DataOut
%%           DataOut = list()
%%--------------------------------------------------------------------
remove_separator([H|T]) when H == $\r;H == $\n ->
    remove_separator(T);
remove_separator(L) ->
    L.
