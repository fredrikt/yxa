%%%-------------------------------------------------------------------
%%% File    : tcp_receiver.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: TCP receiver does blocking read on a single socket and
%%%           signals parent when a complete SIP request/response has
%%%           been received. If we encounter something we can't handle
%%%           we just close the socket. There is no way to know if we
%%%           are in or out of sync with the remote end if we have
%%%           encountered something not perfectly legal.
%%%
%%%           tcp_receiver processes are just the blocking-read shadow
%%%           of tcp_connection processes. We consider the
%%%           tcp_connection process the 'real owner' of the open
%%%           socket, so we never for example close the sockets
%%%           ourselves - instead we signal the tcp_connection process
%%%           (our State#state.parent) to close it, and then we just
%%%           terminate. This is not really true for SSL sockets
%%%           though, since the ssl module should send received data
%%%           to this process we are also the owner of SSL sockets.
%%%
%%%           Signals we send to our parent :
%%%
%%%           {recv_sipmsg, R} - we have received a complete SIP
%%%                              message and parsed it into a request
%%%                              or response record()
%%%           {close, self()}  - close the socket, please
%%%           {connection_closed, self()} - the other end has closed
%%%                                         the socket
%%%
%%%           Signals we honor (SSL sockets only) :
%%%
%%%           {quit_receiver, Parent} - we should close the socket
%%%
%%% Created : 15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------

-module(tcp_receiver).
-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/5,
	 test/0]).

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

-record(recv, {
	  msg_stack=[],		%% First-in-last-out stack of received frames (messages)
	  frame = <<>>,		%% The current frame

	  parsed,		%% undefined | request record() | response record()
	  body_offset,		%% undefined | integer(), the start position of the body in 'frame'
	  body_length,		%% undefined | integer(), the content length of the message
	  bytes_left,		%% undefined | integer(), number of bytes left on this frame -
				%% redundant data (we could always calculate it) but it is easier this way
	  origin_str		%% string(), used when logging receved frames
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(CR, 16#0D).
-define(LF, 16#0A).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/6
%% Descrip.: Spawn a tcp_receiver process into it's recv_loop().
%% Returns : void()
%%--------------------------------------------------------------------
start_link(SocketModule, Socket, Local, Remote, SipSocket) ->
    State = #state{socketmodule=SocketModule, socket=Socket, parent=self(),
		   local=Local, remote=Remote, sipsocket=SipSocket},

    %% Create origin string for logging the source of received frames
    {IP, Port} = State#state.remote,
    Origin = #siporigin{proto=SipSocket#sipsocket.proto, addr=IP, port=Port, receiver=self(), sipsocket=SipSocket},
    OriginStr = sipserver:origin2str(Origin),

    Recv = #recv{origin_str=OriginStr},
    Pid = spawn_link(?MODULE, recv_loop, [State, Recv]),
    Pid.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: recv_loop(State, DataIn)
%%           State  = state record()
%%           DataIn = list()
%% Descrip.: SSL version: Wait for data from our socket. If we receive
%%           (more) data, call handle_received_data(). Loops back to
%%           self until someone closes the connection or some sort of
%%           error occur.
%% Returns : void()
%%--------------------------------------------------------------------
recv_loop(#state{linked=no, socketmodule=ssl}=State, Recv) when is_record(Recv, recv) ->
    %% Socket is SSL and this is the first time we enter recv_loop(). Link to sockets pid
    %% and set us up to receive exit signals from our parent and from the sockets pid.
    process_flag(trap_exit, true),
    SocketPid = ssl:pid(State#state.socket),
    true = link(SocketPid),
    recv_loop(State#state{linked=yes}, Recv);
recv_loop(#state{socketmodule=ssl}=State, Recv) when is_record(Recv, recv) ->
    Socket = State#state.socket,
    SocketPid = ssl:pid(State#state.socket),
    %% Must set {active, once} here even if tcp_listener did it right before us -
    %% it seems as if the SSL socket looses this status when controlling process is changed.
    ssl:setopts(State#state.socket, [{active, once}]),
    Parent = State#state.parent,
    Res =
	receive
	    {ssl, Socket, B} ->
		logger:log(debug, "TCP receiver: Received ~p bytes of data (SSL), my frame buffer "
			   "contained ~p bytes of data", [size(B), size(Recv#recv.frame)]),
		handle_received_data(B, Recv, State);

	    {ssl_closed, Socket} ->
		logger:log(debug, "TCP receiver: Extra debug: Socket closed (Socket: ~p, local ~p, remote ~p)",
			   [Socket, State#state.local, State#state.remote]),
		connection_closed;

	    {ssl_error, Socket, Reason} ->
		logger:log(error, "TCP receiver: SSL error from socket ~p : ~p", [Socket, Reason]),
		Recv;

	    {quit_receiver, Parent} ->
		logger:log(debug, "TCP receiver: Closing SSL socket ~p and terminating upon request from my parent",
			   [Socket]),
		quit;

	    {'EXIT', SocketPid, Reason} ->
		logger:log(debug, "TCP receiver: SSL socket ~p terminated, shutting down. Reason was : ~p",
			   [Socket, Reason]),
		close;

	    {'EXIT', Parent, Reason} ->
		logger:log(debug, "TCP receiver: SSL connection handler ~p terminated, shutting down. Reason was : ~p",
			   [Parent, Reason]),
		quit;

	    Unknown ->
		logger:log(error, "TCP receiver: Received unknown signal :~n~p", [Unknown]),
		Recv
	end,
    case Res of
	connection_closed ->
	    gen_server:cast(Parent, {connection_closed, self()});
	close ->
	    ssl:close(State#state.socket),	%% Maybe not needed, but better safe than sorry
	    gen_server:cast(Parent, {close, self()});
	quit ->
	    ssl:close(State#state.socket),
	    ok;
	NewRecv when is_record(NewRecv, recv) ->
	    recv_loop(State, NewRecv)
    end;

%%--------------------------------------------------------------------
%% Function: recv_loop(State, DataIn)
%%           State  = state record()
%%           DataIn = list()
%% Descrip.: SSL version: Wait for data from our socket. If we receive
%%           (more) data, call handle_received_data(). Loops back to
%%           self until someone closes the connection or some sort of
%%           error occur.
%% Returns : void()
%%--------------------------------------------------------------------
recv_loop(State, Recv) when is_record(State, state), is_record(Recv, recv) ->
    Parent = State#state.parent,
    SocketModule = State#state.socketmodule,
    Socket = State#state.socket,
    Res =
	case SocketModule:recv(Socket, 0) of
	    {ok, B} ->
		logger:log(debug, "TCP receiver: Received ~p bytes of data, my frame buffer contained ~p bytes of data",
			   [size(B), size(Recv#recv.frame)]),
		handle_received_data(B, Recv, State);
	    {error, closed} ->
		logger:log(debug, "TCP receiver: Extra debug: ~p recv() returned 'closed' in do_recv() (Socket: ~p)",
			   [SocketModule, Socket]),
		connection_closed;
	    {error, E} ->
		logger:log(error, "TCP receiver: Error when reading data : ~s (~p)", [inet:format_error(E), E]),
		close
	end,
    case Res of
	NewRecv when is_record(NewRecv, recv) ->
	    recv_loop(State, NewRecv);
	connection_closed ->
	    gen_server:cast(Parent, {connection_closed, self()});
	close ->
	    gen_server:cast(Parent, {close, self()});
	Unknown ->
	    logger:log(error, "TCP receiver: Dying after unknown result from do_recv() :~n~p", [Unknown]),
	    gen_server:cast(Parent, {close, self()})
    end.

%%--------------------------------------------------------------------
%% Function: handle_received_data(Data, Recv, State)
%%           Data  = binary(), the data we have just received
%%           Recv  = recv record(), receive state
%%           State = state record()
%% Descrip.: Handle data just received from the network. Calls
%%           handle_received_data2/2 and then looks at the msg_stack
%%           in the returned recv record() to see if we now have one
%%           or more messages to pass to our parent.
%% Returns : NewRecv = recv record() |
%%           close
%%--------------------------------------------------------------------
handle_received_data(Data, Recv, State) when is_binary(Data), is_record(Recv, recv) ->
    try handle_received_data2(Data, Recv) of
	#recv{msg_stack=[]} = NewRecv ->
	    %% No messages on the stack
	    NewRecv;
	#recv{msg_stack=ReverseMsgs} = NewRecv ->
	    Msgs = lists:reverse(ReverseMsgs),
	    ok = send_messages_to_parent(Msgs, State#state.parent),
	    NewRecv#recv{msg_stack=[]}
    catch
	throw:
	  {error, parse_failed, E} ->
	    {IP, Port} = State#state.remote,
	    Msg = binary_to_list( list_to_binary([Recv#recv.frame, Data]) ),
	    ProtoStr = case State#state.socketmodule of
			   ssl -> "tls";
			   gen_tcp -> "tcp"
		       end,
	    logger:log(error, "TCP receiver: Failed parsing data received from ~s:~s:~p, ",
		       "discarding and closing socket.", [ProtoStr, IP, Port]),
	    logger:log(debug, "TCP receiver: Data : ~n~p~nError : ~p", [Msg, E]),
	    close

    end.

%%--------------------------------------------------------------------
%% Function: handle_received_data2(Data, Recv)
%%           Data = binary(), the data we have just received
%%           Recv = recv record(), receive state
%% Descrip.: Look at received data to see if it should be added to our
%%           current recv.frame, or if it is CRLF's between requests.
%%
%%           These first three instances of handle_received_data2/2 is
%%           just for ignoring CRLF's (and LF's) between requests, as
%%           specified by RFC3261 #7.5.
%% Returns : NewRecv = recv record()
%%--------------------------------------------------------------------
handle_received_data2(<<?CR, ?LF, Rest/binary>>, #recv{frame = <<>>}=Recv) ->
    %% Ignore CRLF received when current recv.frame is empty.
    handle_received_data2(Rest, Recv);

handle_received_data2(<<?LF, Rest/binary>>, #recv{frame = <<>>}=Recv) ->
    %% Ignore LF received when current recv.frame is empty.
    handle_received_data2(Rest, Recv);

handle_received_data2(<<?LF, Rest/binary>>, #recv{frame = <<?CR>>}=Recv) ->
    %% LF received when current recv.frame was a CR - ignore it and clear recv.frame.
    handle_received_data2(Rest, Recv#recv{frame = <<>>});

%%--------------------------------------------------------------------
%% Function: handle_received_data2(Data, Recv)
%%           Data = binary(), the data we have just received
%%           Recv = recv record(), receive state
%% Descrip.: Look at received data to see if we should attempt to
%%           parse message headers now (because we've just spotted the
%%           header-body separator). init_frame() might find that we
%%           have received the complete frame by the way, in which
%%           case it will place the received frame in recv.msg_stack
%%           and start on a new frame.
%% Returns : NewRecv = recv record()
%%--------------------------------------------------------------------
handle_received_data2(Data, #recv{body_offset = undefined}=Recv) when is_binary(Data) ->
    %% No body_offset information - means we haven't parsed the headers yet
    OldLen = size(Recv#recv.frame),
    NewFrame = list_to_binary([Recv#recv.frame, Data]),
    %% Now look for header-body separator in NewFrame - we only have to look from position
    %% (OldLen - 3) since worst case is that we have CRLFCR in the previously received data
    %% and Data begins with LF.
    case has_header_body_separator(NewFrame, OldLen - 3) of
	{true, BodyOffset} ->
	    %% Ok, we found the header-body separator. We might now have a complete message,
	    %% or we might still be waiting for more data to add to the body.
	    init_frame(NewFrame, BodyOffset, Recv);
	false ->
	    %% Header-body separator not found
	    Recv#recv{frame=NewFrame}
    end;

%%--------------------------------------------------------------------
%% Function: handle_received_data2(Data, Recv)
%%           Data = binary(), the data we have just received
%%           Recv = recv record(), receive state
%% Descrip.: recv.body_offset is set, meaning we have recevied all the
%%           headers but (at least previously) not the whole body. Add
%%           the received data to our current frame, and call
%%           decode_frame() to see if we have a complete frame now.
%% Returns : NewRecv = recv record()
%%--------------------------------------------------------------------
handle_received_data2(Data, Recv) when is_binary(Data), is_record(Recv, recv) ->
    %% bytes_left is set, means we have already received the headers and are now just
    %% adding to the body. Note that we might just have received the rest of the frame
    %% we were working on, and parts of (or whole) the next frame.
    NewFrame = list_to_binary([Recv#recv.frame, Data]),
    BL = Recv#recv.bytes_left - size(Data),
    decode_frame(NewFrame, Recv#recv{bytes_left=BL}).


%%--------------------------------------------------------------------
%% Function: init_frame(Frame, BodyOffset, Recv)
%%           Frame      = binary(), our frame with the newly arrived
%%                        data appended
%%           BodyOffset = integer(), where the body starts
%%           Recv       = recv record(), receive state
%% Descrip.: When we see the header-body separator, we let this
%%           function call sippacket:parse() on the data we have this
%%           far, so that we can calculate the total frame length (we
%%           must parse the headers for that - to find the Content-
%%           Length header). This function caches the parse results
%%           in recv.parsed to avoid having to do that again later.
%% Returns : NewRecv = recv record() |
%%           throw(...)
%%--------------------------------------------------------------------
init_frame(Frame, BodyOffset, Recv) when is_binary(Frame), is_integer(BodyOffset), is_record(Recv, recv) ->
    case catch sippacket:parse(Frame, none) of
	Request when is_record(Request, request) ->
	    CL = get_content_length(request, Request#request.header),
	    Left = (BodyOffset + CL) - size(Frame),
	    decode_frame(Frame, Recv#recv{parsed=Request, body_offset=BodyOffset, body_length=CL, bytes_left=Left});
	Response when is_record(Response, response) ->
	    CL = get_content_length(response, Response#response.header),
	    Left = (BodyOffset + CL) - size(Frame),
	    decode_frame(Frame, Recv#recv{parsed=Response, body_offset=BodyOffset, body_length=CL, bytes_left=Left});
	Unknown ->
	    throw({error, parse_failed, Unknown})
    end.

%%--------------------------------------------------------------------
%% Function: decode_frame(Frame, Recv)
%%           Frame = binary(), our frame with the newly arrived data
%%                   appended
%%           Recv  = recv record(), receive state
%% Descrip.: Check bytes_left inside Recv to see if we have now
%%           received enough data, or if we should just set frame in
%%           Recv to Frame. The caller is responsible for updating
%%           bytes_left in Recv. If we have enough data, split out
%%           this frame and push it onto the msg_stack in Recv. Then
%%           check any remaining data to see if there are more data
%%           we can process there at this time.
%% Returns : NewRecv = recv record()
%%--------------------------------------------------------------------
decode_frame(Frame, #recv{bytes_left=BL} = Recv) when is_binary(Frame), is_integer(BL), BL =< 0 ->
    %% Bytes left is zero or less, we have a full frame
    BodyOffset = Recv#recv.body_offset,
    BodyLen = Recv#recv.body_length,
    %% Extract the complete body from Frame, put it into the request/response record
    %% we have already parsed and insert the results first in the msg_stack.
    <<_:BodyOffset/binary, Body:BodyLen/binary, NewFrame/binary>> = Frame,
    %% Don't use siprequest:set_{request,response}_body since it potentially alters the record
    %% in more ways than just setting the body element. We don't want the socket code to alter
    %% what is received.
    Msg =
	if
	    is_record(Recv#recv.parsed, request) ->
		(Recv#recv.parsed)#request{body = Body};
	    is_record(Recv#recv.parsed, response) ->
		(Recv#recv.parsed)#response{body = Body}
	end,
    NewStack = [Msg | Recv#recv.msg_stack],
    OriginStr = Recv#recv.origin_str,
    logger:log_iolist(debug, [<<"Frame received (from ">>, OriginStr, <<") :", 10>>, Frame, 10]),
    %% Check if there is already a header-body separator in NewFrame
    case has_header_body_separator(NewFrame, 0) of
	{true, NewFrameBO} ->
	    init_frame(NewFrame, NewFrameBO, Recv#recv{msg_stack=NewStack});
	false ->
	    %% create new recv record to get correct defaults, then just set msg_stack and frame (and origin_str)
	    #recv{msg_stack=NewStack, frame=NewFrame, origin_str=OriginStr}
    end;
decode_frame(Frame, Recv) when is_binary(Frame), is_record(Recv, recv) ->
    %% bytes_left is > 0, we can't decode a frame right now. Save it in recv.
    Recv#recv{frame=Frame}.

%%--------------------------------------------------------------------
%% Function: has_header_body_separator(Data, Offset)
%%           Data   = binary(), our current frame
%%           Offset = integer(), start offset for search
%% Descrip.: Look for header-body separator (CRLFCRLF or LFLF).
%% Returns : {true, BodyOffset} |
%%           false
%%--------------------------------------------------------------------
%%
has_header_body_separator(Data, Offset) when is_integer(Offset), Offset >= 0 ->
    has_header_body_separator2(Data, Offset);
has_header_body_separator(Data, Offset) when is_integer(Offset) ->
    %% Offset < 0
    has_header_body_separator2(Data, 0).

has_header_body_separator2(Data, Offset) when is_binary(Data), is_integer(Offset) ->
    case Data of
	<<_:Offset/binary, ?CR, ?LF, ?CR, ?LF, _/binary>> ->
	    {true, Offset + 4};
	<<_:Offset/binary, ?LF, ?LF, _/binary>> ->
	    {true, Offset + 2};
	<<_:Offset/binary, _:8, _/binary>> ->
	    has_header_body_separator2(Data, Offset + 1);
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: send_messages_to_parent(MsgList, Parent)
%%           MsgList = list() of request or response record()
%%           Parent  = term(), pid() or atom()
%% Descrip.: Send received frames to parent (tcp_connection pid).
%% Returns : ok
%%--------------------------------------------------------------------
send_messages_to_parent([H | T], Parent) ->
    gen_server:cast(Parent, {recv_sipmsg, H}),
    send_messages_to_parent(T, Parent);
send_messages_to_parent([], _Parent) ->
    ok.

%%--------------------------------------------------------------------
%% Function: get_content_length(Type, Header)
%%           Type   = request | response
%%           Header = keylist record()
%% Descrip.: Get Content-Length and return it as an integer value.
%% Returns : Length = integer() |
%%           throw(...)
%%--------------------------------------------------------------------
get_content_length(Type, Header) ->
    case keylist:fetch('content-length', Header) of
	[CLenStr] ->
	    %% sippacket will fail if Content-Length is bad before we get here.
	    %% if it doesn't - let it crash here.
	    list_to_integer(CLenStr);
	_ ->
	    throw({error, Type, invalid_content_length, Header})
    end.

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%%--------------------------------------------------------------------
test() ->
    %% handle_received_data2/2
    %%--------------------------------------------------------------------
    EmptyRecv = #recv{},

    io:format("test: handle_received_data2/2 - 1~n"),
    %% check that we properly ignore leading CRLF's
    EmptyRecv = handle_received_data2(<<?CR, ?LF>>, EmptyRecv),

    io:format("test: handle_received_data2/2 - 2~n"),
    %% check that we properly ignore leading LF's
    EmptyRecv = handle_received_data2(<<?LF>>, EmptyRecv),

    io:format("test: handle_received_data2/2 - 3~n"),
    %% one CR is stored in recv.frame
    #recv{frame = <<?CR>>} = HRD_2_Recv = handle_received_data2(<<?CR>>, EmptyRecv),

    io:format("test: handle_received_data2/2 - 4~n"),
    %% but when it is followed by a LF recv.frame is emptied
    EmptyRecv = handle_received_data2(<<?LF>>, HRD_2_Recv),

    io:format("test: handle_received_data2/2 - 5~n"),
    %% check that we properly ignore a bundle of leading CRLF's and LF's
    EmptyRecv = handle_received_data2(<<?CR, ?LF, ?LF, ?LF, ?CR, ?LF, ?CR, ?LF, ?LF>>, EmptyRecv),

    io:format("test: handle_received_data2/2 - 7.1~n"),
    HRD_7_Data = <<
		  "INVITE sip:ft@example.org SIP/2.0", ?CR, ?LF,
		  "Foo: bar", ?CR, ?LF,
		  "Content-Length: 0", ?CR, ?LF,
		  ?CR, ?LF
		  >>,
    HRD7_URI = sipurl:parse("sip:ft@example.org"),
    %% receive a complete request at once, result should be like EmptyRecv but with
    %% the request parsed and ready in msg_stack
    HRD_7_Recv = handle_received_data2(HRD_7_Data, EmptyRecv),
    %% verify msg_stack
    [HRD_7_Req] = HRD_7_Recv#recv.msg_stack,
    %% verify that msg_stack was the only thing set in HRD_7_Recv
    EmptyRecv = HRD_7_Recv#recv{msg_stack=[]},

    io:format("test: handle_received_data2/2 - 7.2~n"),
    %% verify results
    true = is_record(HRD_7_Req, request),
    "INVITE" = HRD_7_Req#request.method,
    HRD7_URI = HRD_7_Req#request.uri,
    ["bar"] = keylist:fetch("foo", HRD_7_Req#request.header),
    ["0"] = keylist:fetch('content-length', HRD_7_Req#request.header),
    <<>> = HRD_7_Req#request.body,

    io:format("test: handle_received_data2/2 - 8~n"),
    %% test two requests in the same packet (use the requests from test 7 above)
    HRD_8_Recv = handle_received_data2(list_to_binary([HRD_7_Data, HRD_7_Data]), EmptyRecv),
    %% verify msg_stack
    [HRD_7_Req, HRD_7_Req] = HRD_8_Recv#recv.msg_stack,
    %% verify that msg_stack was the only thing set in HRD_8_Recv
    EmptyRecv = HRD_8_Recv#recv{msg_stack=[]},

    io:format("test: handle_received_data2/2 - 9~n"),
    %% start receiving a request
    HRD_9_Recv = handle_received_data2(<<?LF, ?CR, ?LF, "INVITE sip:ft@example.org ">>, EmptyRecv),
    HRD_9_Recv = EmptyRecv#recv{frame = <<"INVITE sip:ft@example.org ">>},

    io:format("test: handle_received_data2/2 - 10~n"),
    HRD_10_Data = <<"INVITE sip:ft@example.org SIP/2.0", ?CR, ?LF>>,
    %% receive more
    HRD_10_Recv = handle_received_data2(<<"SIP/2.0", ?CR, ?LF>>, HRD_9_Recv),
    HRD_10_Recv = EmptyRecv#recv{frame = HRD_10_Data},

    io:format("test: handle_received_data2/2 - 11~n"),
    HRD_11_Data = <<
		   "Foo: ", ?LF,
		   "  bar", ?CR, ?LF,
		   "Content-Length: 0004", ?CR, ?LF,
		   ?CR
		   >>,
    %% some headers (mixed line endings), and half the header-body separator
    HRD_11_Recv = handle_received_data2(HRD_11_Data, HRD_10_Recv),
    %% quickly verify the results
    HRD_11_Frame = list_to_binary([HRD_10_Data, HRD_11_Data]),
    HRD_11_Frame = HRD_11_Recv#recv.frame,

    io:format("test: handle_received_data2/2 - 12.1~n"),
    %% now receive a single LF which means we now are exactly at the header-body
    %% separator and are just waiting for four bytes of body
    HRD_12_Recv = handle_received_data2(<<?LF>>, HRD_11_Recv),
    io:format("test: handle_received_data2/2 - 12.2~n"),
    %% quickly verify the results (body length)
    4 = HRD_12_Recv#recv.body_length,
    io:format("test: handle_received_data2/2 - 12.3~n"),
    %% quickly verify the results (body offset)
    HRD_12_Right_Body_Offset = size(HRD_11_Frame) + 1,
    HRD_12_Right_Body_Offset = HRD_12_Recv#recv.body_offset,
    io:format("test: handle_received_data2/2 - 12.4~n"),
    4 = HRD_12_Recv#recv.bytes_left,

    io:format("test: handle_received_data2/2 - 13.1~n"),
    %% receive the four body bytes plus some chars from the next request
    HRD_13_Recv = handle_received_data2(<<"testINV">>, HRD_12_Recv),

    io:format("test: handle_received_data2/2 - 13.2~n"),
    %% verify the results
    [HRD_13_Req] = HRD_13_Recv#recv.msg_stack,
    true = is_record(HRD_13_Req, request),

    io:format("test: handle_received_data2/2 - 13.3~n"),
    %% verify headers
    ["bar"] = keylist:fetch("foo", HRD_13_Req#request.header),
    io:format("test: handle_received_data2/2 - 13.4~n"),
    ["0004"] = keylist:fetch('content-length', HRD_13_Req#request.header),

    io:format("test: handle_received_data2/2 - 13.5~n"),
    %% verify body
    <<"test">> = HRD_13_Req#request.body,

    io:format("test: handle_received_data2/2 - 13.6~n"),
    %% verify next frame data
    <<"INV">> = HRD_13_Recv#recv.frame,

    io:format("test: handle_received_data2/2 - 14.1~n"),
    %% Make a data entry with a request, two responses and then the same request again.
    %% We then feed this to handle_received_data2/2 one byte at a time, and then verify
    %% the results
    HRD_14_Req_Data =
	"INVITE sip:ft@example.org SIP/2.0\n"
	"Foo: bar\n"
	"Content-Length: 3\n"
	"\n"
	"one",
    %% Short form of header Content-Length
    HRD_14_Res1_Data =
	"SIP/2.0 100 Trying\r\n"
	"Foo: bar\r\n"
	"l:0\r\n"
	"\r\n",
    %% A bunch of CR/LFs between requests (should be ignored)
    HRD_14_Ignore = "\r\n\r\n\n\n\r\n",
    %% Another request, with a binary body
    HRD_14_Res2_Data =
	"SIP/2.0 404 Not Found\r\n"
	"Foo: bar\r\n"
	"Content-Length: 8\r\n"
	"\r\n" ++
	[1,2,3,4,5,6,7,8],

    HRD_14_Data = HRD_14_Req_Data ++ HRD_14_Res1_Data ++ HRD_14_Ignore ++
	HRD_14_Res2_Data ++ HRD_14_Ignore ++ HRD_14_Req_Data,

    io:format("test: handle_received_data2/2 - 14.2~n"),
    HRD_14_Recv =
	lists:foldl(fun(H, Recv) when is_record(Recv, recv) ->
			    B = list_to_binary([H]),
			    handle_received_data2(B, Recv)
		    end, EmptyRecv, HRD_14_Data),

    io:format("test: handle_received_data2/2 - 14.3~n"),
    %% verify the results (msg_stack and record types)
    [HRD_14_Req, HRD_14_Res1, HRD_14_Res2, HRD_14_Req] = lists:reverse(HRD_14_Recv#recv.msg_stack),
    true = is_record(HRD_14_Req, request),
    true = is_record(HRD_14_Res1, response),
    true = is_record(HRD_14_Res2, response),

    io:format("test: handle_received_data2/2 - 14.4~n"),
    %% verify the results (bodys)
    <<"one">> = HRD_14_Req#request.body,
    <<>> = HRD_14_Res1#response.body,
    <<1,2,3,4,5,6,7,8>> = HRD_14_Res2#response.body,


    %% test invalid content-length

    io:format("test: handle_received_data2/2 - 15~n"),
    %% invalid Content-Length in request
    HRD_15_Data = <<
		   "INVITE sip:ft@example.org SIP/2.0", ?CR, ?LF,
		   "Foo: bar", ?CR, ?LF,
		   "Content-Length: broken", ?CR, ?LF,
		   ?CR, ?LF
		   >>,
    {error, parse_failed, _} =
	(catch handle_received_data2(HRD_15_Data, EmptyRecv)),

    io:format("test: handle_received_data2/2 - 16~n"),
    %% invalid (double) Content-Length in response
    HRD_16_Data = <<
		   "SIP/2.0 100 Trying", ?CR, ?LF,
		   "Foo: bar", ?CR, ?LF,
		   "Content-Length: 1, 2", ?CR, ?LF,
		   ?CR, ?LF
		   >>,
    {error, parse_failed, _} =
	(catch handle_received_data2(HRD_16_Data, EmptyRecv)),

    ok.
