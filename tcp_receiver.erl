%%% File    : tcp_receiver.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : TCP receiver does blocking read on a single socket
%%% and signals parent when a complete SIP request/response has been
%%% received.
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>

-module(tcp_receiver).

-export([start_link/4, recv_loop/2]).

-include("sipsocket.hrl").
-include("siprecords.hrl").

-record(state, {socketmodule, socket, parent, local, remote}).

start_link(SocketModule, Socket, Local, Remote) ->
    State = #state{socketmodule=SocketModule, socket=Socket, parent=self(), local=Local, remote=Remote},    
    Pid = spawn_link(?MODULE, recv_loop, [State, []]),
    Pid.

%% tcp_receiver() for SSL sockets waits for data from a socket. When data is
%% available, we ...
recv_loop(State, DataIn) when record(State, state), list(DataIn), State#state.socketmodule == ssl ->
    Socket = State#state.socket,
    Rest = receive

	       {ssl, Socket, B} ->
		   L = binary_to_list(B),
		   logger:log(debug, "TCP receiver: Received ~p bytes of data, adding it to my buffer which contained ~p bytes of data X",
			      [length(L), length(DataIn)]),
		   %% remove_separator() removes any leading CR or LF
		   NewDataIn = lists:append(DataIn, L),
		   SoFar = remove_separator(NewDataIn),
		   ssl:setopts(Socket, [{active, once}]),
		   %% Look for header-body separator
		   case string:str(SoFar, "\r\n\r\n") of
		       0 ->
			   %% No Header Body separator yet, read more data
			   SoFar;
		       _ ->
			   AdditionalData = header_received(SoFar, State),
			   AdditionalData
		   end;


	       {ssl_closed, Socket} ->
		   logger:log(debug, "TCP receiver: Extra debug: Socket closed (Socket: ~p, local ~p, remote ~p)",
			      [Socket, State#state.local, State#state.remote]),
		   {connection_closed};

	       {ssl_error, Socket, Reason} ->
		   logger:log(error, "TCP receiver: SSL error from socket ~p : ~p", [Socket, Reason]),
		   DataIn;

	       Unknown ->
		   logger:log(error, "TCP receiver: Received unknown signal :~n~p", [Unknown]),
		   DataIn
	   after 10 * 1000 ->
		   DataIn
	   end,
    case Rest of
	{connection_closed} ->
	    ssl:close(State#state.socket),
	    gen_server:cast(State#state.parent, {connection_closed, self()});
	{close} ->
	    ssl:close(State#state.socket),
	    gen_server:cast(State#state.parent, {close, self()});
	_ when list(Rest) ->
	    recv_loop(State, Rest)
    end;

% tcp_receiver() constantly tries to read from a socket, and when data is available
% it is checked to see if we have a complete SIP message. if we do, we safe_spawn
% a sipserver:process() on the received message.
recv_loop(State, DataIn) when record(State, state) ->
    Parent = State#state.parent,
    %% do_recv() reads data until it finds the CRLFCRLF between header and body. It is possible
    %% that it also reads into the body of the message, but as soon as we have the header we
    %% use tcp_read_sip_message() instead, which looks at the Content-Length and keeps reading
    %% until the message boundry is reached.
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

%% Read data until we encounter the header/body separator "\r\n\r\n"
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
	    %% Look for header-body separator
	    case string:str(SoFar, "\r\n\r\n") of
		0 ->
		    %% No Header Body separator yet, read more data
		    do_recv(SoFar, State);
		_ ->
		    Rest = header_received(SoFar, State),
		    Rest
	    end;
	{error, closed} ->
	    logger:log(debug, "TCP receiver: Extra debug: ~p recv() returned 'closed' in do_recv() (Socket: ~p)",
		       [SocketModule, Socket]),
	    {connection_closed};
	{error, E} ->
	    logger:log(error, "TCP receiver: Error when reading data : ~s (~p)", [inet:format_error(E), E]),
	    {error, E}
    end.

%% We have found the separator between Header and Body. We can now extract the
%% Content-Length header and read until we have the whole message. We migth get
%% more than just this message though, if the sender sends us two requests at
%% once. In that case, we return the extra data back to do_recv().
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
	    NewData;
	_ ->
	    logger:log(error, "TCP receiver: Unknown result from sippacket:parse() or tcp_read_sip_message() :~n~p~n" ++
		       "Closing socket.", [Rest]),
	    {close}
    end.
   

% tcp_read_sip_message() is used to read the remainder of the message when we have the header
% and therefor can use the Content-Length to determine when we are done.
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
	_ ->
	    case MessageType of
		request ->
		    logger:log(debug, "TCP receiver: Non-existing or invalid Content-Length in request from ~s:~p, " ++
			       "(socket ~p), discarding message :~n~p", [IP, Port, Socket, DataIn]),
		    logger:log(error, "TCP receiver: Non-existing or invalid Content-Length in request from ~s:~p, " ++
			       "answering 400 Bad Request and closing socket", [IP, Port]),
		    SipSocket = {module=sipsocket_tcp, pid=Parent, data={IP, Port}},
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

get_content_length(Header) ->
    case keylist:fetch("Content-Length", Header) of
	[CLenStr] ->
	    case util:isnumeric(CLenStr) of
		true ->
		    %% must be careful, it could be a partial content-header
		    %% XXX do we correctly detect that we have received for example
		    %% 'Content-Length: 10' but no LF? Real length might be '101'.
		    list_to_integer(CLenStr);
		false ->
		    invalid
	    end;
	_ ->
	    invalid
    end.
    
% Wait for a pre-determined ammount of more data from an SSL socket.
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

% Read a pre-determined ammount of more data from a socket.
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

tcp_read_more_data(SocketModule, Socket, Len, In) ->
    In.

% RFC3261 7.5
remove_separator([H|T]) when H == $\r;H == $\n ->
    remove_separator(T);
remove_separator(L) ->
    L.
