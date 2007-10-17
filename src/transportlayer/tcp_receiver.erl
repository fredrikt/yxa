%%%-------------------------------------------------------------------
%%% File    : tcp_receiver.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      TCP receiver does blocking read on a single socket and
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
%%%           to this process and not our parent we are also the owner
%%%           of SSL sockets.
%%%
%%%           XXX describe STUN demuxing done by this module
%%%
%%%           Signals we send to our parent :
%%%
%%%           {recv_sipmsg, R} - we have received a complete SIP
%%%                              message and parsed it into a request
%%%                              or response record()
%%%
%%%           {send_stun_response, Data} - request to send a STUN
%%%                                        response packet to the peer
%%%
%%%           {close, self()}  - close the socket, please
%%%
%%%           {connection_closed, self()} - the other end has closed
%%%                                         the socket
%%%
%%%           Signals we honor (SSL sockets only) :
%%%
%%%           {quit_receiver, Parent} - we should close the socket
%%%
%%% @since    15 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
%%%-------------------------------------------------------------------

-module(tcp_receiver).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/3,

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
-include("stun.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  socketmodule,		%% atom(), the module used to handle 'socket'
	  socket,		%% term(), the socket we should read from
	  parent,		%% pid() of our parent (tcp_connection)
	  sipsocket,		%% sipsocket record()
	  linked = false	%% bool(), some SSL initialization performed yet?
	 }).

%% @type recv() = #recv{}.
%%                no description
-record(recv, {
	  msg_stack = [],	%% First-in-last-out stack of received frames (messages)
	  frame = <<>>,		%% The current frame

	  parsed,		%% undefined | request record() | response record()
	  body_offset,		%% undefined | integer(), the start position of the body in 'frame'
	  body_length,		%% undefined | integer(), the content length of the message
	  bytes_left,		%% undefined | integer(), number of bytes left on this frame -
				%% redundant data (we could always calculate it) but it is easier this way
	  origin_str,		%% string(), used when logging receved frames
	  stun_env,		%% undefined | stun_env record() if we are de-muxing STUN
	  is_stun = false	%% bool(), are we currently receiving a STUN packet?
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
%% @spec    (SocketModule, Socket, SipSocket) ->
%%            Receiver
%%
%%            SocketModule = ssl | gen_tcp
%%            Socket       = term() "socket to read from"
%%            SipSocket    = #sipsocket{}
%%
%%            Receiver = pid()
%%
%% @doc     Spawn a tcp_receiver process into it's recv_loop().
%% @end
%%--------------------------------------------------------------------
start_link(SocketModule, Socket, SipSocket) when SocketModule == ssl; SocketModule == gen_tcp,
						 is_record(SipSocket, sipsocket) ->
    #sipsocket{proto	= Proto,
	       hostport	= #hp{l_ip	= LocalIP,
			      l_port	= LocalPort,
			      r_ip	= RemoteIP,
			      r_port	= RemotePort
			     }
	      } = SipSocket,
    StunEnv =
	case yxa_config:get_env(stun_demuxing_on_sip_ports) of
	    {ok, true} ->
		{{ok, LocalIPtuple},
		 {ok, RemoteIPtuple}
		} = if
			Proto == tcp; Proto == tls ->
			    {inet_parse:ipv4_address(LocalIP),
			     inet_parse:ipv4_address(RemoteIP)
			    };
			Proto == tcp6; Proto == tls6 ->
			    {inet_parse:ipv6_address(util:remove_v6_brackets(LocalIP)),
			     inet_parse:ipv6_address(util:remove_v6_brackets(RemoteIP))
			    }
		    end,
		#stun_env{proto			= Proto,
			  local_ip		= LocalIPtuple,
			  local_port		= LocalPort,
			  remote_ip		= RemoteIPtuple,
			  remote_ip_str		= RemoteIP,
			  remote_port		= RemotePort,
			  alt_ip		= undefined,	%% Alternate IP not supported (patches welcome)
			  alt_port		= undefined	%% Alternate port not supported for TCP/TLS
			 };
	    {ok, false} ->
		undefined
	end,

    State = #state{socketmodule	= SocketModule,
		   socket	= Socket,
		   parent	= self(),
		   sipsocket	= SipSocket
		  },

    %% Create origin string for logging the source of received frames
    Origin = #siporigin{proto		= Proto,
			addr		= RemoteIP,
			port		= RemotePort,
			receiver	= self(),
			sipsocket	= SipSocket
		       },
    OriginStr = sipserver:origin2str(Origin),

    Recv = #recv{origin_str	= OriginStr,
		 stun_env	= StunEnv
		},
    spawn_link(?MODULE, recv_loop, [State, Recv]).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (State, DataIn) -> void()
%%
%%            State  = #state{}
%%            DataIn = list()
%%
%% @doc     SSL version: Wait for data from our socket. If we receive
%%          (more) data, call handle_received_data(). Loops back to
%%          self until someone closes the connection or some sort of
%%          error occur.
%% @end
%%--------------------------------------------------------------------
recv_loop(#state{linked = false, socketmodule = ssl} = State, Recv) when is_record(Recv, recv) ->
    %% Socket is SSL and this is the first time we enter recv_loop(). Link to sockets pid
    %% and set us up to receive exit signals from our parent and from the sockets pid.
    process_flag(trap_exit, true),
    SocketPid = ssl:pid(State#state.socket),
    true = link(SocketPid),
    recv_loop(State#state{linked = true}, Recv);
recv_loop(#state{socketmodule = ssl} = State, Recv) when is_record(Recv, recv) ->
    #state{socket = Socket,
	   parent = Parent
	  } = State,
    SocketPid = ssl:pid(Socket),
    %% Must set {active, once} here even if tcp_listener did it right before us -
    %% it seems as if the SSL socket looses this status when controlling process is changed.
    ssl:setopts(Socket, [{active, once}]),
    Res =
	receive
	    {ssl, Socket, B} ->
		logger:log(debug, "TCP receiver: Received ~p bytes of data (SSL), my frame buffer "
			   "contained ~p bytes of data", [size(B), size(Recv#recv.frame)]),
		handle_received_data(B, Recv, State);

	    {ssl_closed, Socket} ->
		HP = (State#state.sipsocket)#sipsocket.hostport,
		Local = io_lib:format("~s:~p", [HP#hp.l_ip, HP#hp.l_port]),
		Remote =
		    case HP#hp.r_ip of
			undefined -> "undefined";
			_ ->
			    io_lib:format("~s:~p", [HP#hp.r_ip, HP#hp.r_port])
		    end,
		logger:log(debug, "TCP receiver: Extra debug: Socket closed (Socket: ~p, local ~p, remote ~p)",
			   [Socket, Local, Remote]),
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
	    case catch ssl:close(State#state.socket) of
		ok ->
		    ok;
		E ->
		    logger:log(error, "TCP receiver: Closing SSL socket ~p failed : ~p",
			       [Socket, E])
	    end,
	    gen_server:cast(Parent, {close, self()});
	quit ->
	    ssl:close(Socket),
	    ok;
	NewRecv when is_record(NewRecv, recv) ->
	    recv_loop(State, NewRecv)
    end;

%%--------------------------------------------------------------------
%% @spec    (State, DataIn) -> void()
%%
%%            State  = #state{}
%%            DataIn = list()
%%
%% @doc     SSL version: Wait for data from our socket. If we receive
%%          (more) data, call handle_received_data(). Loops back to
%%          self until someone closes the connection or some sort of
%%          error occur.
%% @end
%%--------------------------------------------------------------------
recv_loop(State, Recv) when is_record(State, state), is_record(Recv, recv) ->
    #state{parent	= Parent,
	   socketmodule	= SocketModule,
	   socket	= Socket
	  } = State,
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
%% @spec    (Data, Recv, State) ->
%%            NewRecv
%%
%%            Data  = binary() "the data we have just received"
%%            Recv  = #recv{} "receive state"
%%            State = #state{}
%%
%%            NewRecv = #recv{} |
%%            close
%%
%% @doc     Handle data just received from the network. Calls
%%          handle_received_data2/2 and then looks at the msg_stack
%%          in the returned recv record() to see if we now have one
%%          or more messages to pass to our parent.
%% @end
%%--------------------------------------------------------------------
handle_received_data(Data, Recv, State) when is_binary(Data), is_record(Recv, recv) ->
    try handle_received_data2(Data, Recv) of
	#recv{msg_stack = []} = NewRecv ->
	    %% No messages on the stack
	    NewRecv;
	#recv{msg_stack = ReverseMsgs} = NewRecv ->
	    Msgs = lists:reverse(ReverseMsgs),
	    ok = process_msg_stack(Msgs, State#state.parent),
	    NewRecv#recv{msg_stack = []}
    catch
	throw:
	  {error, parse_failed, E} ->
	    #sipsocket{hostport = #hp{r_ip   = R_IP,
				      r_port = R_Port
				     }
		      } = State#state.sipsocket,
	    Msg = binary_to_list( list_to_binary([Recv#recv.frame, Data]) ),
	    ProtoStr = case State#state.socketmodule of
			   ssl ->	"tls";
			   gen_tcp ->	"tcp"
		       end,
	    logger:log(error, "TCP receiver: Failed parsing data received from ~s:~s:~p, "
		       "discarding and closing socket.", [ProtoStr, R_IP, R_Port]),
	    logger:log(debug, "TCP receiver: Data : ~n~p~nError : ~p", [Msg, E]),
	    close
    end.


%%--------------------------------------------------------------------
%% @spec    (Data, Recv) ->
%%            NewRecv
%%
%%            Data = binary() "the data we have just received"
%%            Recv = #recv{} "receive state"
%%
%%            NewRecv = #recv{}
%%
%% @doc     Look at received data to see if it should be added to our
%%          current recv.frame, or if it is CRLF's between requests.
%%          These first three instances of handle_received_data2/2 is
%%          just for ignoring CRLF's (and LF's) between requests, as
%%          specified by RFC3261 #7.5.
%% @end
%%--------------------------------------------------------------------
handle_received_data2(<<?CR, ?LF, Rest/binary>>, #recv{frame = <<>>} = Recv) ->
    %% Ignore CRLF received when current recv.frame is empty.
    handle_received_data2(Rest, Recv);

handle_received_data2(<<?LF, Rest/binary>>, #recv{frame = <<>>} = Recv) ->
    %% Ignore LF received when current recv.frame is empty.
    handle_received_data2(Rest, Recv);

handle_received_data2(<<?LF, Rest/binary>>, #recv{frame = <<?CR>>} = Recv) ->
    %% LF received when current recv.frame was a CR - ignore it and clear recv.frame.
    handle_received_data2(Rest, Recv#recv{frame = <<>>});

handle_received_data2(<<N, _/binary>> = Data, #recv{frame = <<>>} = Recv) when N == 0; N == 1 ->
    %% empty frame, received something that is definately not SIP but might be STUN
    handle_received_data2_stun(Data, Recv);

handle_received_data2(Data, #recv{is_stun = true} = Recv) ->
    %% 'pop' stored partial STUN packet from Recv, construct new frame and
    %% call handle_received_data2_stun on it
    NewFrame = list_to_binary([Recv#recv.frame, Data]),
    handle_received_data2_stun(NewFrame, Recv#recv{frame = <<>>});

%%--------------------------------------------------------------------
%% @spec    (Data, Recv) ->
%%            NewRecv
%%
%%            Data = binary() "the data we have just received"
%%            Recv = #recv{} "receive state"
%%
%%            NewRecv = #recv{}
%%
%% @doc     Look at received data to see if we should attempt to parse
%%          message headers now (because we've just spotted the
%%          header-body separator). init_frame() might find that we
%%          have received the complete frame by the way, in which
%%          case it will place the received frame in recv.msg_stack
%%          and start on a new frame.
%% @end
%%--------------------------------------------------------------------
handle_received_data2(Data, #recv{body_offset = undefined, is_stun = false} = Recv) when is_binary(Data) ->
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
	    Recv#recv{frame = NewFrame}
    end;


%%--------------------------------------------------------------------
%% @spec    (Data, Recv) ->
%%            NewRecv
%%
%%            Data = binary() "the data we have just received"
%%            Recv = #recv{} "receive state"
%%
%%            NewRecv = #recv{}
%%
%% @doc     recv.body_offset is set, meaning we have recevied all the
%%          headers but (at least previously) not the whole body. Add
%%          the received data to our current frame, and call
%%          decode_frame() to see if we have a complete frame now.
%% @end
%%--------------------------------------------------------------------
handle_received_data2(Data, #recv{is_stun = false} = Recv) when is_binary(Data), is_record(Recv, recv) ->
    %% bytes_left is set, means we have already received the headers and are now just
    %% adding to the body. Note that we might just have received the rest of the frame
    %% we were working on, and parts of (or whole) the next frame.
    NewFrame = list_to_binary([Recv#recv.frame, Data]),
    BL = Recv#recv.bytes_left - size(Data),
    decode_frame(NewFrame, Recv#recv{bytes_left = BL}).



%%--------------------------------------------------------------------
%% @spec    (Data, Recv) ->
%%            NewRecv
%%
%%            Data = binary() "the data we have just received"
%%            Recv = #recv{} "receive state"
%%
%%            NewRecv = #recv{}
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
handle_received_data2_stun(Frame, #recv{stun_env = undefined, frame = <<>>}) when is_binary(Frame) ->
    logger:log(error, "TCP receiver: Received non-SIP-probably-STUN when not de-muxing, "
	       "throwing parse failed error"),
    throw({error, parse_failed, "STUN demuxing not enabled"});
handle_received_data2_stun(Frame, #recv{stun_env = StunEnv, frame = <<>>} = Recv) when is_binary(Frame) ->
    case stun:process_stream(Frame, StunEnv) of
	{ok, StunResult, FrameRest} ->
	    NewStack = [StunResult | Recv#recv.msg_stack],
	    NewRecv = Recv#recv{msg_stack = NewStack},
	    %% we decoded a STUN packet, FrameRest are leftovers and might be SIP or STUN data
	    %% so we call handle_received_data2/2 on it
	    handle_received_data2(FrameRest, NewRecv);
	{need_more_data, Num} ->
	    Recv#recv{is_stun		= true,
		      frame		= Frame,
		      bytes_left	= Num
		     };
	ignore ->
	    logger:log(error, "TCP receiver: STUN packet decoding failure, "
		       "throwing parse failed error"),
	    logger:log(debug, "TCP receiver: The data that made us fail was : ~p", [Frame]),
	    throw({error, parse_failed, "STUN packet parse error"});
	{error, not_stun} ->
	    logger:log(error, "TCP receiver: What looked like a STUN packet wasn't, "
		       "throwing parse failed error"),
	    logger:log(debug, "TCP receiver: The data that made us fail was : ~p", [Frame]),
	    throw({error, parse_failed, "STUN packet parse error"})
    end.


%%--------------------------------------------------------------------
%% @spec    (Frame, BodyOffset, Recv) ->
%%            NewRecv
%%
%%            Frame      = binary() "our frame with the newly arrived data appended"
%%            BodyOffset = integer() "where the body starts"
%%            Recv       = #recv{} "receive state"
%%
%%            NewRecv = #recv{}
%%
%% @throws  {error, parse_failed, Reason::string()} 
%%
%% @doc     When we see the header-body separator, we let this
%%          function call sippacket:parse() on the data we have this
%%          far, so that we can calculate the total frame length (we
%%          must parse the headers for that - to find the Content-
%%          Length header). This function caches the parse results in
%%          recv.parsed to avoid having to do that again later.
%% @end
%%--------------------------------------------------------------------
init_frame(Frame, BodyOffset, Recv) when is_binary(Frame), is_integer(BodyOffset), is_record(Recv, recv) ->
    case catch sippacket:parse(Frame, none) of
	Request when is_record(Request, request) ->
	    CL = get_content_length(request, Request#request.header),
	    Left = (BodyOffset + CL) - size(Frame),
	    decode_frame(Frame, Recv#recv{parsed	= Request,
					  body_offset	= BodyOffset,
					  body_length	= CL,
					  bytes_left	= Left
					 });
	Response when is_record(Response, response) ->
	    CL = get_content_length(response, Response#response.header),
	    Left = (BodyOffset + CL) - size(Frame),
	    decode_frame(Frame, Recv#recv{parsed	= Response,
					  body_offset	= BodyOffset,
					  body_length	= CL,
					  bytes_left	= Left
					 });
	Unknown ->
	    throw({error, parse_failed, Unknown})
    end.

%%--------------------------------------------------------------------
%% @spec    (Frame, Recv) ->
%%            NewRecv
%%
%%            Frame = binary() "our frame with the newly arrived data appended"
%%            Recv  = #recv{} "receive state"
%%
%%            NewRecv = #recv{}
%%
%% @doc     Check bytes_left inside Recv to see if we have now
%%          received enough data, or if we should just set frame in
%%          Recv to Frame. The caller is responsible for updating
%%          bytes_left in Recv. If we have enough data, split out
%%          this frame and push it onto the msg_stack in Recv. Then
%%          call handle_received_data2 on any remaining bytes.
%% @end
%%--------------------------------------------------------------------
decode_frame(Frame, #recv{bytes_left = BL} = Recv) when is_binary(Frame), is_integer(BL), BL =< 0 ->
    %% Bytes left is zero or less, we have a full frame
    #recv{body_offset	= BodyOffset,
	  body_length	= BodyLen,
	  origin_str	= OriginStr
	 } = Recv,
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
    %% For logging purposes, we now extract this exact frame
    %% XXX if body is binary, or very large, we shouldn't log it
    ThisFrameLen = BodyOffset + BodyLen,
    <<ThisFrame:ThisFrameLen/binary, _/binary>> = Frame,
    logger:log_iolist(debug, [<<"Frame received (from ">>, OriginStr, <<") :", 10>>, ThisFrame, 10]),

    %% create new recv record to get correct defaults, then just set msg_stack and frame (and origin_str)
    StunEnv = Recv#recv.stun_env,
    NewRecv =
	#recv{msg_stack		= NewStack,
	      frame		= <<>>,
	      origin_str	= OriginStr,
	      stun_env		= StunEnv
	     },
    handle_received_data2(NewFrame, NewRecv);
decode_frame(Frame, Recv) when is_binary(Frame), is_record(Recv, recv) ->
    %% bytes_left is > 0, we can't decode a frame right now. Save it in recv.
    Recv#recv{frame = Frame}.

%%--------------------------------------------------------------------
%% @spec    (Data, Offset) ->
%%            {true, BodyOffset} |
%%            false
%%
%%            Data   = binary() "our current frame"
%%            Offset = integer() "start offset for search"
%%
%% @doc     Look for header-body separator (CRLFCRLF or LFLF).
%% @end
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
%% @spec    (MsgList, Parent) -> ok
%%
%%            MsgList = [#request{} | #response{} | #stun_result{}]
%%            Parent  = term() "pid() or atom()"
%%
%% @doc     Send received frames to parent (tcp_connection pid), and
%%          respond to STUN requests.
%% @end
%%--------------------------------------------------------------------
process_msg_stack([H | T], Parent) when is_record(H, stun_result) ->
    case H of
	#stun_result{action = send_response, change = none, response = STUNResponse} ->
	    gen_server:cast(Parent, {send_stun_response, STUNResponse});
	_ ->
	    logger:log(debug, "TCP Receiver: Ignoring incompatible STUN response")
    end,
    process_msg_stack(T, Parent);
process_msg_stack([H | T], Parent) ->
    gen_server:cast(Parent, {recv_sipmsg, H}),
    process_msg_stack(T, Parent);
process_msg_stack([], _Parent) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (Type, Header) ->
%%            Length
%%
%%            Type   = request | response
%%            Header = #keylist{}
%%
%%            Length = integer()
%%
%% @throws  {error, Type, invalid_content_length, Header} 
%%
%% @doc     Get Content-Length and return it as an integer value.
%% @end
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
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    %% handle_received_data2(Data, Recv)
    %%--------------------------------------------------------------------
    EmptyRecv = #recv{},

    autotest:mark(?LINE, "handle_received_data2/2 - 1"),
    %% check that we properly ignore leading CRLF's
    EmptyRecv = handle_received_data2(<<?CR, ?LF>>, EmptyRecv),

    autotest:mark(?LINE, "handle_received_data2/2 - 2"),
    %% check that we properly ignore leading LF's
    EmptyRecv = handle_received_data2(<<?LF>>, EmptyRecv),

    autotest:mark(?LINE, "handle_received_data2/2 - 3"),
    %% one CR is stored in recv.frame
    #recv{frame = <<?CR>>} = HRD_2_Recv = handle_received_data2(<<?CR>>, EmptyRecv),

    autotest:mark(?LINE, "handle_received_data2/2 - 4"),
    %% but when it is followed by a LF recv.frame is emptied
    EmptyRecv = handle_received_data2(<<?LF>>, HRD_2_Recv),

    autotest:mark(?LINE, "handle_received_data2/2 - 5"),
    %% check that we properly ignore a bundle of leading CRLF's and LF's
    EmptyRecv = handle_received_data2(<<?CR, ?LF, ?LF, ?LF, ?CR, ?LF, ?CR, ?LF, ?LF>>, EmptyRecv),

    autotest:mark(?LINE, "handle_received_data2/2 - 7.1"),
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

    autotest:mark(?LINE, "handle_received_data2/2 - 7.2"),
    %% verify results
    true = is_record(HRD_7_Req, request),
    "INVITE" = HRD_7_Req#request.method,
    HRD7_URI = HRD_7_Req#request.uri,
    ["bar"] = keylist:fetch("foo", HRD_7_Req#request.header),
    ["0"] = keylist:fetch('content-length', HRD_7_Req#request.header),
    <<>> = HRD_7_Req#request.body,

    autotest:mark(?LINE, "handle_received_data2/2 - 8"),
    %% test two requests in the same packet (use the requests from test 7 above)
    HRD_8_Recv = handle_received_data2(list_to_binary([HRD_7_Data, HRD_7_Data]), EmptyRecv),
    %% verify msg_stack
    [HRD_7_Req, HRD_7_Req] = HRD_8_Recv#recv.msg_stack,
    %% verify that msg_stack was the only thing set in HRD_8_Recv
    EmptyRecv = HRD_8_Recv#recv{msg_stack=[]},

    autotest:mark(?LINE, "handle_received_data2/2 - 9"),
    %% start receiving a request
    HRD_9_Recv = handle_received_data2(<<?LF, ?CR, ?LF, "INVITE sip:ft@example.org ">>, EmptyRecv),
    HRD_9_Recv = EmptyRecv#recv{frame = <<"INVITE sip:ft@example.org ">>},

    autotest:mark(?LINE, "handle_received_data2/2 - 10"),
    HRD_10_Data = <<"INVITE sip:ft@example.org SIP/2.0", ?CR, ?LF>>,
    %% receive more
    HRD_10_Recv = handle_received_data2(<<"SIP/2.0", ?CR, ?LF>>, HRD_9_Recv),
    HRD_10_Recv = EmptyRecv#recv{frame = HRD_10_Data},

    autotest:mark(?LINE, "handle_received_data2/2 - 11"),
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

    autotest:mark(?LINE, "handle_received_data2/2 - 12.1"),
    %% now receive a single LF which means we now are exactly at the header-body
    %% separator and are just waiting for four bytes of body
    HRD_12_Recv = handle_received_data2(<<?LF>>, HRD_11_Recv),
    autotest:mark(?LINE, "handle_received_data2/2 - 12.2"),
    %% quickly verify the results (body length)
    4 = HRD_12_Recv#recv.body_length,
    autotest:mark(?LINE, "handle_received_data2/2 - 12.3"),
    %% quickly verify the results (body offset)
    HRD_12_Right_Body_Offset = size(HRD_11_Frame) + 1,
    HRD_12_Right_Body_Offset = HRD_12_Recv#recv.body_offset,
    autotest:mark(?LINE, "handle_received_data2/2 - 12.4"),
    4 = HRD_12_Recv#recv.bytes_left,

    autotest:mark(?LINE, "handle_received_data2/2 - 13.1"),
    %% receive the four body bytes plus some chars from the next request
    HRD_13_Recv = handle_received_data2(<<"testINV">>, HRD_12_Recv),

    autotest:mark(?LINE, "handle_received_data2/2 - 13.2"),
    %% verify the results
    [HRD_13_Req] = HRD_13_Recv#recv.msg_stack,
    true = is_record(HRD_13_Req, request),

    autotest:mark(?LINE, "handle_received_data2/2 - 13.3"),
    %% verify headers
    ["bar"] = keylist:fetch("foo", HRD_13_Req#request.header),
    autotest:mark(?LINE, "handle_received_data2/2 - 13.4"),
    ["0004"] = keylist:fetch('content-length', HRD_13_Req#request.header),

    autotest:mark(?LINE, "handle_received_data2/2 - 13.5"),
    %% verify body
    <<"test">> = HRD_13_Req#request.body,

    autotest:mark(?LINE, "handle_received_data2/2 - 13.6"),
    %% verify next frame data
    <<"INV">> = HRD_13_Recv#recv.frame,

    autotest:mark(?LINE, "handle_received_data2/2 - 14.1"),
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

    autotest:mark(?LINE, "handle_received_data2/2 - 14.2"),
    HRD_14_Recv =
	lists:foldl(fun(H, Recv) when is_record(Recv, recv) ->
			    B = list_to_binary([H]),
			    handle_received_data2(B, Recv)
		    end, EmptyRecv, HRD_14_Data),

    autotest:mark(?LINE, "handle_received_data2/2 - 14.3"),
    %% verify the results (msg_stack and record types)
    [HRD_14_Req, HRD_14_Res1, HRD_14_Res2, HRD_14_Req] = lists:reverse(HRD_14_Recv#recv.msg_stack),
    true = is_record(HRD_14_Req, request),
    true = is_record(HRD_14_Res1, response),
    true = is_record(HRD_14_Res2, response),

    autotest:mark(?LINE, "handle_received_data2/2 - 14.4"),
    %% verify the results (bodys)
    <<"one">> = HRD_14_Req#request.body,
    <<>> = HRD_14_Res1#response.body,
    <<1,2,3,4,5,6,7,8>> = HRD_14_Res2#response.body,


    %% test invalid content-length

    autotest:mark(?LINE, "handle_received_data2/2 - 15"),
    %% invalid Content-Length in request
    HRD_15_Data = <<
		   "INVITE sip:ft@example.org SIP/2.0", ?CR, ?LF,
		   "Foo: bar", ?CR, ?LF,
		   "Content-Length: broken", ?CR, ?LF,
		   ?CR, ?LF
		   >>,
    {error, parse_failed, _} =
	(catch handle_received_data2(HRD_15_Data, EmptyRecv)),

    autotest:mark(?LINE, "handle_received_data2/2 - 16"),
    %% invalid (double) Content-Length in response
    HRD_16_Data = <<
		   "SIP/2.0 100 Trying", ?CR, ?LF,
		   "Foo: bar", ?CR, ?LF,
		   "Content-Length: 1, 2", ?CR, ?LF,
		   ?CR, ?LF
		   >>,
    {error, parse_failed, _} =
	(catch handle_received_data2(HRD_16_Data, EmptyRecv)),

    autotest:mark(?LINE, "handle_received_data2/2 - 17"),
    %% test STUN packet (all of it received at once)
    Test_StunEnv =
	#stun_env{proto		= tcp,
		  local_ip	= {192,0,2,190},
		  local_port	= 2233,
		  remote_ip	= {192,0,2,192},
		  remote_ip_str	= "192.0.2.192",
		  remote_port	= 1919,
		  alt_ip	= undefined,
		  alt_port	= undefined
		 },
    Test_StunRecv = #recv{stun_env = Test_StunEnv},

    HRD_17_Data = <<16#0001:16/big-unsigned,	%% STUN binding request
		   0:16/big-unsigned,		%% STUN length of attributes
		   1234:128/big-unsigned>>,	%% STUN transaction ID
    #recv{msg_stack	= [#stun_result{action = send_response}],
	  frame		= <<>>,
	  bytes_left	= undefined,
	  is_stun	= false,
	  stun_env	= Test_StunEnv
	 } = handle_received_data2(HRD_17_Data, Test_StunRecv),

    autotest:mark(?LINE, "handle_received_data2/2 - 18"),
    %% test reception of a STUN packet when STUN demuxing is not enabled
    {error, parse_failed, "STUN demuxing not enabled"} =
	(catch handle_received_data2(<<16#0001:16/big-unsigned>>, EmptyRecv)),

    autotest:mark(?LINE, "handle_received_data2/2 - 19.1"),
    %% test need more data (only received one byte)
    #recv{frame		= <<0>>,
	  is_stun	= true,
	  bytes_left	= 3
	 } = handle_received_data2(<<0>>, Test_StunRecv),

    autotest:mark(?LINE, "handle_received_data2/2 - 19.2"),
    %% test need more data
    #recv{frame		= <<16#0001:16/big-unsigned>>,
	  is_stun	= true,
	  bytes_left	= 2
	 } = handle_received_data2(<<16#0001:16/big-unsigned>>, Test_StunRecv),

    autotest:mark(?LINE, "handle_received_data2/2 - 19.3"),
    %% test need more data, missing 128 bits (16 bytes) of STUN header
    HRD_19_3_Data = <<16#0001:16/big-unsigned,    %% STUN binding request
		     0:16/big-unsigned>>,		%% STUN length of attributes
    #recv{frame		= HRD_19_3_Data,
	  is_stun	= true,
	  bytes_left	= 16
	 } = handle_received_data2(HRD_19_3_Data, Test_StunRecv),


    autotest:mark(?LINE, "handle_received_data2/2 - 19.3"),
    %% test need more data, missing 96 bits (12 bytes) of STUN transaction ID
    HRD_19_4_Data = <<16#0001:16/big-unsigned,		%% STUN binding request
		     0:16/big-unsigned,			%% STUN length of attributes
		     16#2112A442:32/big-unsigned>>,	%% STUN RFC3489bis magic cookie
    #recv{frame		= HRD_19_4_Data,
	  is_stun	= true,
	  bytes_left	= 12
	 } = HRD_19_4_Recv = handle_received_data2(HRD_19_4_Data, Test_StunRecv),

    autotest:mark(?LINE, "handle_received_data2/2 - 20"),
    %% test receiving some more bytes of the last tests STUN packet
    HRD_19_5_Data = list_to_binary([HRD_19_4_Data, 0, 1, 2, 4]),
    #recv{frame		= HRD_19_5_Data,
	  is_stun	= true,
	  bytes_left	= 8
	 } = handle_received_data2(<<0, 1 , 2, 4>>, HRD_19_4_Recv),



    %% handle_received_data(Data, Recv, State)
    %% we can't test much of this function since it is not side-effect free
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "handle_received_data/2 - 1"),
    %% minimal state
    HRD_State1 = #state{socketmodule	= ssl,
			sipsocket	= #sipsocket{hostport = #hp{l_ip = "0.0.0.0",
								    l_port = 0,
								    r_ip = "192.0.2.1",
								    r_port = 5060
								   }
						    },
			parent		= self()
		       },
    close = handle_received_data(<<"broken\n\n">>, EmptyRecv, HRD_State1),

    autotest:mark(?LINE, "handle_received_data/2 - 2"),
    %% no data, only CRLFs
    #recv{frame = <<>>,
	  msg_stack = []
	 } = handle_received_data(<<"\r\n">>, EmptyRecv, HRD_State1),

    autotest:mark(?LINE, "handle_received_data/2 - 3.1"),
    %% test with SIP request, STUN request and SIP response received back to back, PLUS some extra bytes

    HRD_Full_3_Data =
	<<%% CRLF between requests (should be ignored)
	 "\r\n"
	 "INVITE sip:ft@example.org SIP/2.0\n"
	 "Foo: bar\n"
	 "Content-Length: 3\n"
	 "\n"
	 "one"
	 %% A bunch of CR/LFs between requests (should be ignored)
	 "\r\n\r\n\n\n\r\n",
	 %% A complete STUN request
	 16#0001:16/big-unsigned,    %% STUN binding request
	 0:16/big-unsigned,           %% STUN length of attributes
	 1234:128/big-unsigned,
	 %% A bunch of CR/LFs between requests (should be ignored)
	 "\r\n\r\n"
	 %% Short form of header Content-Length
	 "SIP/2.0 100 Trying\r\n"
	 "Foo: bar\r\n"
	 "l:0\r\n"
	 "\r\n"
	 %% A bunch of CR/LFs between requests (should be ignored)
	 "\r\n\r\n\n\n\r\n"
	 "OPTIONS "
	 >>,
    #recv{frame		= <<"OPTIONS ">>,
	  msg_stack	= []
	 } = handle_received_data(HRD_Full_3_Data, Test_StunRecv, HRD_State1),

    autotest:mark(?LINE, "handle_received_data/2 - 3.2"),
    receive
	{'$gen_cast', {recv_sipmsg, #request{method = "INVITE"}}} ->
	    ok;
	HRD_Signal1 ->
	    HRD_Msg1 = io_lib:format("Received unknown signal : ~p", [HRD_Signal1]),
	    throw({error, lists:flatten(HRD_Msg1)})
    after
	0 -> throw({error, "Test did not result in the expected signal"})
    end,

    autotest:mark(?LINE, "handle_received_data/2 - 3.3"),
    receive
	{'$gen_cast', {send_stun_response, [<<_:20/binary>>, _]}} ->
	    ok;
	HRD_Signal2 ->
	    HRD_Msg2 = io_lib:format("Received unknown signal : ~p", [HRD_Signal2]),
	    throw({error, lists:flatten(HRD_Msg2)})
    after
	0 -> throw({error, "Test did not result in the expected signal"})
    end,

    autotest:mark(?LINE, "handle_received_data/2 - 3.4"),
    receive
	{'$gen_cast', {recv_sipmsg, #response{status = 100}}} ->
	    ok;
	HRD_Signal3 ->
	    HRD_Msg3 = io_lib:format("Received unknown signal : ~p", [HRD_Signal3]),
	    throw({error, lists:flatten(HRD_Msg3)})
    after
	0 -> throw({error, "Test did not result in the expected signal"})
    end,

    ok.
