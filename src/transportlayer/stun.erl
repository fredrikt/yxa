%%%-------------------------------------------------------------------
%%% File    : stun.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Incomplete STUN-server. Based on
%%%           draft-ietf-behave-rfc3489bis-03 but only a subset
%%%           currently implemented. You have been warned!
%%%
%%%           This module is written to be totally side-effects free,
%%%           in order to be totally testable. This requires us to
%%%           pass quite a few arguments about the world to the
%%%           process(...) function though, such as our own sockets
%%%           IP address, the alternate sockets IP address and so on.
%%%
%%% @since     9 Feb 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(stun).
%%-compile(export_all).

-export([
	 process/2,
	 process_stream/2,

	 test/0
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("stun.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(stun_request, {type,		%% STUN request type
		       length,		%% integer(), length of payload
		       rfc,		%% rfc3489 | rfc3489bis
		       t_id,		%% transcation id, 96 or 128 bits
		       integrity_c,	%% undefined | integer(), the number of bytes in
		       			%% packet over which we are to do message integrity (for RFC3489bis)
		       attrs,		%% undefined | list() of stun_attr record()
		       env		%% stun_env record()
		       }).

-record(stun_attr, {type,		%% STUN attribute type
		    label,		%% atom(), stored here for debugging/test cases only
		    length,		%% attribute value length
		    value		%% attribute value
		   }).


%% @type stun_error_code() = integer() | {integer(), string()}.
%%           STUN error code (gets default reason phrase) or code and reason phrase.

%% @type stun_error_attr() = [binary()].
%%           Encoded STUN attributes.

%% @type stun_error() = {stun_error, stun_error_code(), stun_error_attr()}.
%%           STUN parsing/handling error. Thrown by various functions on error. Never seen outside this module though.

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER_IDENTIFICATION, "YXA").

-define(STUN_FIXED_HEADER_LENGTH, 20).

%% RFC3489bis-03 #6 (STUN Message Structure)
-define(STUN_MAGIC_COOKIE, 16#2112A442).

%% RFC3489bis-03 #15.1 (STUN Message Type Registry)
-define(STUN_TYPE_BINDING_REQUEST,		16#0001).
-define(STUN_TYPE_BINDING_RESPONSE,		16#0101).
-define(STUN_TYPE_BINDING_ERROR_RESPONSE,	16#0111).
-define(STUN_TYPE_SHARED_SECRET_REQUEST,	16#0002).
-define(STUN_TYPE_SHARED_SECRET_RESPONSE,	16#0102).
-define(STUN_TYPE_SHARED_SECRET_ERROR_RESPONSE,	16#0112).

%% RFC3489bis-03 #15.2 (STUN Attribute Registry)
-define(STUN_ATTRIBUTE_MAPPED_ADDRESS,		16#0001).
-define(STUN_ATTRIBUTE_RESPONSE_ADDRESS,	16#0002).
-define(STUN_ATTRIBUTE_CHANGE_REQUEST,		16#0003).
-define(STUN_ATTRIBUTE_SOURCE_ADDRESS,		16#0004).
-define(STUN_ATTRIBUTE_CHANGED_ADDRESS,		16#0005).
-define(STUN_ATTRIBUTE_USERNAME,		16#0006).
-define(STUN_ATTRIBUTE_PASSWORD,		16#0007).
-define(STUN_ATTRIBUTE_MESSAGE_INTEGRITY,	16#0008).
-define(STUN_ATTRIBUTE_ERROR_CODE,		16#0009).
-define(STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES,	16#000A).
-define(STUN_ATTRIBUTE_REFLECTED_FROM,		16#000B).
-define(STUN_ATTRIBUTE_ALTERNATE_SERVER,	16#000E).
-define(STUN_ATTRIBUTE_REALM,			16#0014).
-define(STUN_ATTRIBUTE_NONCE,			16#0015).
-define(STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS,	16#0020).
-define(STUN_ATTRIBUTE_SERVER,			16#8022).
-define(STUN_ATTRIBUTE_ALTERNATE_SERVER_XXX,	16#8023).	%% duplicate in the draft
-define(STUN_ATTRIBUTE_BINDING_LIFETIME,	16#8024).

%% RFC3489bis-03 #11.1 (MAPPED-ADDRESS)
-define(STUN_ADDRESS_FAMILY_INET,		16#1).
-define(STUN_ADDRESS_FAMILY_INET6,		16#2).



%%--------------------------------------------------------------------
%% @spec    (Packet, Env) ->
%%            StunResult        |
%%            ignore            |
%%            {error, not_stun}
%%
%%            Packet = binary() "the STUN packet received"
%%            Env    = #stun_env{}
%%
%%            StunResult = #stun_result{}
%%
%% @doc     Process a STUN packet received from the network. Relies on
%%          the caller to send the actual response, if one is
%%          returned from this function (in order to be fully
%%          testable).
%% @end
%%--------------------------------------------------------------------
process(<<N:8, _/binary>> = Packet, Env) when is_binary(Packet), is_record(Env, stun_env),
					      N == 0; N == 1 ->
    case Packet of
	<<Type:16/big-unsigned,
	 Length:16/big-unsigned,
	 _:128/big-unsigned,
	 _Rest/binary>> ->
	    SReq1 = #stun_request{env		= Env,
				  type		= Type,
				  length	= Length
				 },
	    SReq = process_get_transaction(Packet, SReq1),
	    received_stun_request(SReq, Packet);
	_ ->
	    {error, not_stun}
    end;
process(_Packet, _Env) ->
    {error, not_stun}.

%%--------------------------------------------------------------------
%% @spec    (Stream, Env) ->
%%            {ok, StunResult, StreamRest}  |
%%            {need_more_data, Num}         |
%%            {error, not_stun}             |
%%            ignore
%%
%%            Stream = binary() "data received from network that is believed to start with a STUN packet"
%%            Env    = #stun_env{}
%%
%%            StunResult = #stun_result{}
%%            Num        = integer()
%%            StreamRest = binary() "the rest of Stream when the first STUN packet has been processed. NOTE: Caller must check if StreamRest starts with another STUN packet!"
%%
%% @doc     Process a binary stream of data received from the network.
%%          Relies on the caller to send the actual response, if one
%%          is returned from this function (in order to be fully
%%          testable).
%% @end
%%--------------------------------------------------------------------
process_stream(<<FirstByte:8, _/binary>> = Stream, Env) when is_binary(Stream), is_record(Env, stun_env),
							     FirstByte == 0; FirstByte == 1 ->
    case Stream of
	<<Type:16/big-unsigned,
	 Length:16/big-unsigned,
	 _/binary>> ->
	    PacketLength = ?STUN_FIXED_HEADER_LENGTH + Length,
	    case (PacketLength - size(Stream)) of
		N when N > 0 ->
		    {need_more_data, N};
		_StreamSize ->
		    SReq1 = #stun_request{env		= Env,
					  type		= Type,
					  length	= Length
					 },
		    SReq = process_get_transaction(Stream, SReq1),
		    {STUNPacket, StreamRest} = split_binary(Stream, PacketLength),
		    case received_stun_request(SReq, STUNPacket) of
			StunResult when is_record(StunResult, stun_result) ->
			    {ok, StunResult, StreamRest};
			ignore ->
			    ignore
		    end
	    end;
	_ when size(Stream) < 4 ->
	    %% four is the minimum ammount of data we need (Type + Length)
	    {need_more_data, 4 - size(Stream)};
	_ ->
	    %% XXX is it possible to get here? Can the binary have at least four bytes
	    %% and NOT match the first clause?
	    {error, not_stun}
    end;
process_stream(_Stream, _Env) ->
    {error, not_stun}.

%% part of process/2 and process_stream/2 - look for magic cookie indicating RFC3489bis
process_get_transaction(Data, SReq) when is_binary(Data), is_record(SReq, stun_request) ->
    case Data of
	<<_TypeLength:4/binary,
	 ?STUN_MAGIC_COOKIE:32/big-unsigned,
	 TransactionId:96/big-unsigned,
	 _/binary>> ->
	    SReq#stun_request{rfc	= rfc3489bis,
			      t_id	= TransactionId
			     };
	<<_TypeLength:4/binary,
	 TransactionId:128/big-unsigned,
	 _/binary>> ->
	    SReq#stun_request{rfc	= rfc3489,
			      t_id	= TransactionId
			     }
    end.

%%--------------------------------------------------------------------
%% @spec    (SReq, Packet) ->
%%            StunResult        |
%%            ignore
%%
%%            SReq   = #stun_request{}
%%            Packet = binary() "the STUN packet received"
%%
%%            StunResult = #stun_result{}
%%
%% @doc     What we received at least held something that could pass
%%          as a STUN header. Wrap our further processing of the
%%          packet in a try/catch to try and ensure that we never
%%          crash or return anything unexpected to the caller of this
%%          module.
%% @end
%%--------------------------------------------------------------------
received_stun_request(SReq, Packet) ->
    try safe_received_stun_request(SReq, Packet) of
	Res when is_record(Res, stun_result) ->
	    Res;
	{error, {not_implemented, Line}} ->
	    logger:log(debug, "STUN: 'Not implemented' signalled from line ~p, answering 300",
		       [Line]),
	    guarded_create_stun_error(SReq, 300, []);
	ignore ->
	    ignore;
	Unknown ->
	    logger:log(debug, "STUN: Unknown result from safe_received_stun_request/2, returning 'ignore' : ~p",
		       [Unknown]),
	    ignore
    catch
	throw:
	  {stun_error, Error, Attributes} ->
	    guarded_create_stun_error(SReq, Error, Attributes);
	error:
	  Y ->
	    ST = erlang:get_stacktrace(),
	    logger:log(error, "STUN: Processing of request FAILED, anwering 500 :~nerror ~p ~p", [Y, ST]),
	    guarded_create_stun_error(SReq, 500, []);
	X:
	  Y ->
	    logger:log(error, "STUN: Processing of request FAILED, anwering 500 : ~p ~p", [X, Y]),
	    guarded_create_stun_error(SReq, 500, [])
    end.

%%--------------------------------------------------------------------
%% @spec    (SReq, Packet) ->
%%            StunResult                       |
%%            ignore                           |
%%            {error, {not_implemented, Line}}
%%
%%            SReq   = #stun_request{}
%%            Packet = binary() "the STUN packet received"
%%
%%            StunResult = #stun_result{}
%%            Line       = integer() "source code line where the 'not implemented' situation occured"
%%
%% @throws  stun_error() 
%%
%% @doc     Do some initial sanity checks of the received packet to
%%          see if it could be wellformed STUN. Check the size of the
%%          payload section against the value encoded in the STUN
%%          header (the first 20 bytes) and so on.
%% @end
%%--------------------------------------------------------------------
safe_received_stun_request(#stun_request{length = Len} = SReq, Packet) ->
    #stun_env{proto = Proto, remote_ip_str = IPstr, remote_port = Port} = SReq#stun_request.env,
    Expected = size(Packet) - ?STUN_FIXED_HEADER_LENGTH,
    case Expected of
	Len ->
	    %%
	    %% Correct length of payload
	    %%
	    case decode_attributes(Packet, ?STUN_FIXED_HEADER_LENGTH) of
		{ok, Attributes, MI_Offset} when is_list(Attributes), is_integer(MI_Offset) ->
		    stun_request_check_integrity(SReq#stun_request{attrs = Attributes}, Packet, MI_Offset);
		{error, Reason} ->
		    logger:log(debug, "Failed parsing attributes in STUN request from ~p:~s:~p : ~p",
			       [Proto, IPstr, Port, Reason]),
		    stun_error(400)
	    end;
	BadLen ->
	    %%
	    %% Incorrect length of payload
	    %%
	    logger:log(normal, "STUN request (type ~p, ~p) with invalid length ~p (real: ~p) from ~p:~s:~p",
		       [SReq#stun_request.type, SReq#stun_request.rfc, SReq#stun_request.length,
			BadLen, Proto, IPstr, Port]),
	    stun_error(400)
    end.

%%--------------------------------------------------------------------
%% @spec    (SReq, Packet, MI_Offset) ->
%%            StunResult                       |
%%            ignore                           |
%%            {error, {not_implemented, Line}}
%%
%%            SReq      = #stun_request{}
%%            Packet    = binary() "the STUN packet received"
%%            MI_Offset = integer() "how many bytes from the start of Packet were there up to the MI-attr?"
%%
%%            StunResult = #stun_result{}
%%            Line       = integer() "source code line where the 'not implemented' situation occured"
%%
%% @throws  stun_error() 
%%
%% @doc     If there was an MESSAGE-INTEGRITY attribute in the Packet,
%%          we go along verifying the integrity of the STUN request.
%% @end
%%--------------------------------------------------------------------
stun_request_check_integrity(#stun_request{rfc = rfc3489bis} = SReq, Packet, MI_Offset) when is_binary(Packet),
											     is_integer(MI_Offset) ->
    Attributes = SReq#stun_request.attrs,
    case get_attribute(?STUN_ATTRIBUTE_MESSAGE_INTEGRITY, Attributes) of
	{ok, _MsgIntegr} ->
	    Get = fun(Type) when is_integer(Type) ->
			  case get_attribute(Type, Attributes) of
			      {ok, MatchL} ->
				  [binary_to_list(E#stun_attr.value) || E <- MatchL];
			      nomatch ->
				  []
			  end
	    end,
	    MsgIntegrity = Get(?STUN_ATTRIBUTE_MESSAGE_INTEGRITY),
	    Realm = Get(?STUN_ATTRIBUTE_REALM),
	    Nonce = Get(?STUN_ATTRIBUTE_NONCE),
	    Username = Get(?STUN_ATTRIBUTE_USERNAME),
	    check_message_integrity(Packet, SReq, MI_Offset, MsgIntegrity, Realm, Nonce, Username);
	nomatch ->
	    case (SReq#stun_request.env)#stun_env.allow_no_msg_integr of
		true ->
		    %% This code path is the likely candidate for a client that implements draft-ietf-sip-outbound-02
		    %% since that draft specifies using binding requests without attributes as NAT keep-alives
		    %% (#7.1. STUN Processing)
		    stun_request_check_mandatory_attributes(SReq);
		false ->
		    %% The server checks the request for a MESSAGE-INTEGRITY attribute.  If
		    %% not present, the server generates an error response with an ERROR-
		    %% CODE attribute and a response code of 401.  That error response MUST
		    %% include a NONCE attribute, containing a nonce that the server wishes
		    %% the client to reflect back in a subsequent request (and therefore
		    %% include in the message integrity computation).  The error response
		    %% MUST include a REALM attribute, containing a realm from which the
		    %% username and password are scoped [8].
		    #stun_env{proto = Proto, remote_ip_str = IPstr, remote_port = Port} = SReq#stun_request.env,
		    logger:log(debug, "STUN request from ~p:~s:~p has no MESSAGE-INTEGRITY attribute",
			       [Proto, IPstr, Port]),
		    %%stun_error(401, [NonceAttr, RealmAttr]),
		    {error, {not_implemented, ?LINE}}
	    end
    end;

stun_request_check_integrity(#stun_request{rfc = rfc3489} = SReq, _Packet, _MI_Offset) ->
    %% XXX check if we allow non-integrity-assured STUN or not (should be configurable)
    stun_request_check_mandatory_attributes(SReq).

%%--------------------------------------------------------------------
%% @spec    (SReq) ->
%%            StunResult                       |
%%            {error, {not_implemented, Line}}
%%
%%            SReq = #stun_request{}
%%
%%            StunResult = #stun_result{}
%%            Line       = integer() "source code line where the 'not implemented' situation occured"
%%
%% @throws  stun_error() 
%%
%% @doc     Check that there are no mandatory attributes (type less
%%          than 0x7ffff) that we don't recognize in the request we
%%          received.
%% @end
%%--------------------------------------------------------------------
stun_request_check_mandatory_attributes(SReq) ->
    %% The server MUST check for any mantadory attributes in the request
    %% (values less than or equal to 0x7fff) which it does not understand.
    %% If it encounters any, the server MUST generate a Binding Error
    %% Response, and it MUST include an ERROR-CODE attribute with a 420
    %% response code.  Any attributes that are known, but are not supposed
    %% to be present in a message (MAPPED-ADDRESS in a request, for example)
    %% MUST be ignored.
    Unknown = [A#stun_attr.type || A <- SReq#stun_request.attrs,
				   A#stun_attr.type =< 16#7fff,
				   A#stun_attr.label == 'unknown_attr'],
    case Unknown of
	[] ->
	    stun_request(SReq);
	AttrList ->
	    ResponseAttrs =
		[create_attribute(?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES, AttrList),
		 create_attribute(?STUN_ATTRIBUTE_SERVER, ?SERVER_IDENTIFICATION)
		],
	    stun_error(420, ResponseAttrs)
    end.

%%--------------------------------------------------------------------
%% @spec    (SReq) ->
%%            StunResult                       |
%%            {error, {not_implemented, Line}}
%%
%%            SReq = #stun_request{}
%%
%%            StunResult = #stun_result{}
%%            Line       = integer() "source code line where the 'not implemented' situation occured"
%%
%% @throws  stun_error() 
%%
%% @doc     We have now performed all checks on the received STUN
%%          request - integrity check has passed and we have checked
%%          that there are no mandatory attributes in there we don't
%%          understand. Now handle the request differently based on
%%          the type of request.
%% @end
%%--------------------------------------------------------------------
stun_request(#stun_request{type = ?STUN_TYPE_BINDING_REQUEST} = SReq) ->
    #stun_env{proto = Proto, remote_ip_str = IPstr, remote_ip = IP, remote_port = Port} = SReq#stun_request.env,
    logger:log(debug, "STUN binding request (~p) from ~p:~s:~p", [SReq#stun_request.rfc, Proto, IPstr, Port]),
    {Status, Attr1} = process_attributes(SReq),
    ResponseAttributes =
	case SReq#stun_request.rfc of
	    rfc3489bis ->
		%% "This attribute MUST always be present in a Binding Response."
		%% RFC3489bis-03 #11.15 (XOR-MAPPED-ADDRESS)
		XORMappedAttr = create_attribute(?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS, SReq),
		[XORMappedAttr | Attr1];
	    rfc3489 ->
		#stun_env{local_ip = LocalIP, local_port = LocalPort} = SReq#stun_request.env,
		%% "For backwards compatibility with RFC3489-compliant STUN servers, if
		%%  the STUN server receives a Binding request without the magic cookie,
		%%  the STUN server MUST include the following attributes in the Binding
		%%  response; otherwise these attribute MUST NOT be included:
		%%     MAPPED-ADDRESS
		%%     SOURCE-ADDRESS"
		%% rfc3489bis-03 #12.2.8 (Server Procedures) (12.2 == Binding Discovery)
		MData = {Proto, IP, Port},
		MappedAttr = create_attribute(?STUN_ATTRIBUTE_MAPPED_ADDRESS, MData),
		SData = {Proto, LocalIP, LocalPort},
		SourceAttr = create_attribute(?STUN_ATTRIBUTE_SOURCE_ADDRESS, SData),
		[MappedAttr, SourceAttr] ++ Attr1
	end,
    create_stun_response(SReq, Status, ResponseAttributes);

stun_request(SReq) when is_record(SReq, stun_request) ->
    #stun_env{proto = Proto, remote_ip_str = IPstr, remote_port = Port} = SReq#stun_request.env,
    logger:log(normal, "Unimplemented STUN request (type ~p, ~p) from ~p:~s:~p",
	       [SReq#stun_request.type, SReq#stun_request.rfc, Proto, IPstr, Port]),
    {error, {not_implemented, ?LINE}}.


%%--------------------------------------------------------------------
%% @spec    (Packet, SReq, MI_Offset, MsgIntegrity, Realm, Nonce,
%%          Username) ->
%%            {error, {not_implemented, Line}} 
%%
%%            Packet       = binary() "STUN request"
%%            SReq         = #stun_request{}
%%            MI_Offset    = integer() "how many bytes from the start of Packet were there up to the MI-attr?"
%%            MsgIntegrity = string() "SHA1 HMAC (or? XXX)"
%%            Realm        = AttrValue
%%            Nonce        = AttrValue
%%            Username     = AttrValue
%%            AttrValue    = nomatch | {ok, [String]}
%%
%%            Line = integer() "source code line where the 'not implemented' situation occured"
%%
%% @throws  stun_error() 
%%
%% @doc     Process the list of parsed attributes in SReq, and put
%%          together attributes for the response where those vary
%%          with the attributes in the request,
%% @end
%%--------------------------------------------------------------------
check_message_integrity(_Packet, SReq, _MI_Offset, MsgIntegrityL, RealmL, NonceL, UsernameL)
  when length(MsgIntegrityL) > 1; length(RealmL) > 1; length(NonceL) > 1; length(UsernameL) > 1 ->
    %% more than one of one or more of the attributes
    #stun_env{proto = Proto, remote_ip_str = IPstr, remote_port = Port} = SReq#stun_request.env,
    logger:log(debug, "STUN request from ~p:~s:~p has more than one of the message integrity attributes : "
	       "MESSAGE-INTEGRITY (~p), REALM (~p), NONCE (~p), USERNAME (~p)",
	       [Proto, IPstr, Port, length(MsgIntegrityL), length(RealmL), length(NonceL), length(UsernameL)]),
    stun_error(400);

check_message_integrity(_Packet, SReq, _MI_Offset, _MsgIntegrity, [], _Nonce, _Username) ->
    #stun_env{proto = Proto, remote_ip_str = IPstr, remote_port = Port} = SReq#stun_request.env,
    %% If the MESSAGE-INTEGRITY attribute was present, the server checks for
    %% the existence of the REALM attribute.  If the attribute is not
    %% present, the server MUST generate an error response.  That error
    %% response MUST include an ERROR-CODE attribute with response code of
    %% 434.  That error response MUST also include a NONCE and a REALM
    %% attribute.
    logger:log(debug, "STUN request from ~p:~s:~p has no REALM attribute",
	       [Proto, IPstr, Port]),
    %%stun_error(434, [NonceAttr, RealmAttr]),
    {error, {not_implemented, ?LINE}};

check_message_integrity(_Packet, SReq, _MI_Offset, _MsgIntegrity, _Realm, [], _Username) ->
    #stun_env{proto = Proto, remote_ip_str = IPstr, remote_port = Port} = SReq#stun_request.env,
    %% If the REALM attribute was present, the server checks for the
    %% existence of the NONCE attribute.  If the NONCE attribute is not
    %% present, the server MUST generate an error response.  That error
    %% response MUST include an ERROR-CODE attribute with a response code of
    %% 435.  That error response MUST include a NONCE attribute and a REALM
    %% attribute.
    logger:log(debug, "STUN request from ~p:~s:~p has no NONCE attribute",
	       [Proto, IPstr, Port]),
    %%stun_error(435, [NonceAttr, RealmAttr]),
    {error, {not_implemented, ?LINE}};

check_message_integrity(_Packet, SReq, _MI_Offset, _MsgIntegrity, _Realm, _Nonce, []) ->
    #stun_env{proto = Proto, remote_ip_str = IPstr, remote_port = Port} = SReq#stun_request.env,
    %% If the NONCE attribute was present, the server checks for the
    %% existence of the USERNAME attribute.  If it was not present, the
    %% server MUST generate an error response.  The error response MUST
    %% include an ERROR-CODE attribute with a response code of 432.  It MUST
    %% include a NONCE attribute and a REALM attribute.
    logger:log(debug, "STUN request from ~p:~s:~p has no USERNAME attribute",
	       [Proto, IPstr, Port]),
    %%stun_error(432, [NonceAttr, RealmAttr]),
    {error, {not_implemented, ?LINE}};

check_message_integrity(Packet, SReq, MI_Offset, MsgIntegrity, [Realm], [Nonce], [Username])
  when is_binary(Packet), is_record(SReq, stun_request), is_integer(MI_Offset), is_list(MsgIntegrity), is_list(Realm),
       is_list(Nonce), is_list(Username) ->
    %% Do the actual message-integrity checking here
    %% stun_request3(SReq, Proto, IP, Port)
    {error, {not_implemented, ?LINE}}.



%%--------------------------------------------------------------------
%% @spec    (SReq) ->
%%            {Change, ReplyAttrs} 
%%
%%            SReq = #stun_request{}
%%
%%            Change     = none | ip | port | both | error
%%            ReplyAttrs = [#stun_attr{}]
%%
%% @throws  stun_error() 
%%
%% @doc     Process the list of parsed attributes in SReq, and put
%%          together attributes for the response where those vary
%%          with the attributes in the request,
%% @end
%%--------------------------------------------------------------------
process_attributes(SReq) when is_record(SReq, stun_request) ->
    Attrs = SReq#stun_request.attrs,
    case get_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, Attrs) of
	{ok, CR} when length(CR) > 1 ->
	    stun_error({400, "Must have 0 or 1 CHANGE-REQUEST attributes"});
	_ ->
	    %% zero or one CHANGE-REQUEST
	    ok
    end,

    process_attributes2(Attrs, SReq, none, []).

process_attributes2([#stun_attr{type = ?STUN_ATTRIBUTE_CHANGE_REQUEST} = H | T], SReq, _Change, Res) ->
    %% look if the client requests us to change IP or port
    <<Flags:32/big-unsigned>> = H#stun_attr.value,
    ChangeIP = (Flags band 4 /= 0),
    ChangePort = (Flags band 2 /= 0),

    #stun_env{proto		= Proto,
	      local_ip		= LocalIP,
	      local_port	= LocalPort,
	      alt_ip		= AltIP,
	      alt_port		= AltPort
	     } = SReq#stun_request.env,

    NewChange =
	if
	    ChangeIP == true, AltIP == undefined ->
		logger:log(debug, "STUN: Client requested that we used an alternate IP address, but I have none. "
			   "Answering 300."),
		stun_error({300, "No alternate IP address available here"});
	    ChangePort == true, AltPort == undefined ->
		logger:log(debug, "STUN: Client requested that we used an alternate port, but I have none. "
			   "Answering 300."),
		stun_error({300, "No alternate port available here"});

	    ChangeIP == true, ChangePort == true ->
		both;
	    ChangeIP == true, ChangePort == false ->
		ip;
	    ChangeIP == false, ChangePort == true ->
		port;
	    true ->
		none
	end,
    NewRes =
	case SReq#stun_request.rfc of
	    rfc3489 ->
		%% Likewise if the STUN server receives a Binding Request containing the
		%% CHANGE-REQUEST attribute without the magic cookie, the STUN server
		%% MUST include the CHANGED-ADDRESS attribute in its Binding Response.
		CData =
		    if
			AltIP /= undefined, AltPort /= undefined ->
			    %% Both altnerate IP address and port supplied.
			    {Proto, AltIP, AltPort};

			AltIP /= undefined, AltPort == undefined ->
			    %% Having an alternate IP address but no alternate port is ridiculous, and it
			    %% probably isn't right to use other IP but same port, but what should we do then?
			    {Proto, AltIP, LocalPort};

			AltIP == undefined, AltPort /= undefined ->
			    %% No alternate IP address (host might only have one IP address), but alternate port
			    {Proto, LocalIP, AltPort};

			AltIP == undefined, AltPort == undefined ->
			    %% No alternate IP address or alternate port. Indicate this to client by
			    %% creating a CHANGED-ADDRESS attribute with the same values the client
			    %% has alread used once.
			    {Proto, LocalIP, LocalPort}
		    end,
		ChAttr = create_attribute(?STUN_ATTRIBUTE_CHANGED_ADDRESS, CData),
		[ChAttr | Res];
	    rfc3489bis ->
		Res
	end,
    process_attributes2(T, SReq, NewChange, NewRes);

%% "Any attributes that are known, but are not supposed
%% to be present in a message (MAPPED-ADDRESS in a request, for example)
%% MUST be ignored." rfc3489bis-03 #9.1.1 (Receive Request Message)

%% ignore all the types not listed as applicable in request in the table at the
%% top of #11 (Attributes)
process_attributes2([#stun_attr{type = Type} | T], SReq, Status, Res) when Type == ?STUN_ATTRIBUTE_MAPPED_ADDRESS;
									   Type == ?STUN_ATTRIBUTE_PASSWORD;
									   Type == ?STUN_ATTRIBUTE_ERROR_CODE;
									   Type == ?STUN_ATTRIBUTE_ALTERNATE_SERVER;
									   Type == ?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES;
									   Type == ?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS;
									   Type == ?STUN_ATTRIBUTE_SERVER;
									   Type == ?STUN_ATTRIBUTE_BINDING_LIFETIME ->
    process_attributes2(T, SReq, Status, Res);

process_attributes2([#stun_attr{type = ?STUN_ATTRIBUTE_MESSAGE_INTEGRITY} | T], SReq, Status, Res) ->
    %% ignore MESSAGE-INTEGRITY attribute here, handled separately (before process_attributes/1)
    process_attributes2(T, SReq, Status, Res);

process_attributes2([#stun_attr{type = Type, label = Label} | _T], _SReq, _Status, _Res) ->
    logger:log(debug, "STUN: Encountered attribute of type ~p (~p) in request, and I don't "
	       "know how to handle it. Answering 300.", [Type, Label]),
    stun_error(300);
process_attributes2([], _SReq, Status, Res) ->
    {Status, lists:reverse(Res)}.


%%--------------------------------------------------------------------
%% @spec    (SReq, Number, Attributes) ->
%%            StunResult | ignore
%%
%%            SReq       = #stun_request{}
%%            Number     = stun_error_code()
%%            Attributes = stun_error_attr()
%%
%%            StunResult = #stun_result{}
%%
%% @doc     Create a STUN error response in a try/catch to hopefully
%%          never crash inside this code, or any code this code
%%          calls. Used from high-level functions that have no other
%%          error protection.
%% @end
%%--------------------------------------------------------------------
guarded_create_stun_error(SReq, Number, Attributes) ->
    try create_stun_error(SReq, Number, Attributes) of
	Res when is_record(Res, stun_result) ->
	    Res;
	Unknown ->
	    logger:log(error, "STUN: FAILED constructing error response ~p, unknown result : ~p",
		       [Number, Unknown]),
	    ignore
    catch
	error:
	  Y ->
	    ST = erlang:get_stacktrace(),
	    logger:log(error, "STUN: FAILED constructing error response ~p : error ~p ~p",
		       [Number, Y, ST]),
	    ignore;
	X: Y ->
	    logger:log(error, "STUN: FAILED constructing error response ~p : ~p ~p",
		       [Number, X, Y]),
	    ignore
    end.

%%--------------------------------------------------------------------
%% @spec    (SReq, Number, Attributes) ->
%%            StunResult
%%
%%            SReq       = #stun_request{}
%%            Number     = stun_error_code()
%%            Attributes = stun_error_attr()
%%
%%            StunResult = #stun_result{}
%%
%% @doc     Create a STUN error response to the request SReq.
%% @end
%%--------------------------------------------------------------------
create_stun_error(SReq, Number, Attributes) when is_record(SReq, stun_request), is_list(Attributes) ->
    %% all STUN error responses MUST have a type whose value is
    %% 0x110 higher than their associated request.
    ErrorAttr = create_attribute(?STUN_ATTRIBUTE_ERROR_CODE, Number),
    Response = create_stun_response_bin(16#110, SReq, [ErrorAttr | Attributes]),
    #stun_result{action		= send_response,
		 type		= error,
		 change		= none,
		 response	= Response
		}.

%%--------------------------------------------------------------------
%% @spec    (SReq, Status, Attributes) ->
%%            StunResult
%%
%%            SReq       = #stun_request{}
%%            Change     = none | ip | port | both
%%            Attributes = [binary()] "encoded attributes"
%%
%%            StunResult = #stun_result{}
%%
%% @doc     Create a STUN response.
%% @end
%%--------------------------------------------------------------------
create_stun_response(SReq, Change, Attributes) when is_record(SReq, stun_request), is_list(Attributes) ->
    %% All STUN success responses MUST have a type whose value is 0x100 higher
    %% than their associated request
    Response = create_stun_response_bin(16#100, SReq, Attributes),
    #stun_result{action		= send_response,
		 type		= ok,
		 change		= Change,
		 response	= Response
		}.

%%--------------------------------------------------------------------
%% @spec    (Increment, SReq, Attributes) ->
%%            [Header | Attrs]
%%
%%            Increment  = integer() "0x100 for successful response and 0x110 for error responses"
%%            SReq       = #stun_request{}
%%            Attributes = [binary()] "encoded attributes"
%%
%%            Header = binary() "the 20 bytes STUN header"
%%            Attrs  = binary()
%%
%% @doc     Encode a STUN response. Returned as a list with two
%%          elements (STUN header and attributes) to make writing
%%          test cases easier [Header | Attrs].
%% @end
%%--------------------------------------------------------------------
create_stun_response_bin(Increment, SReq, Attributes) when is_list(Attributes) ->
    Type = SReq#stun_request.type + Increment,
    TransactionId = SReq#stun_request.t_id,

    FlatAttrs = list_to_binary([Attributes]),
    Length = size(FlatAttrs),

    Header =
	case SReq#stun_request.rfc of
	    rfc3489bis ->
		<<Type:16/big-unsigned,
		 Length:16/big-unsigned,
		 ?STUN_MAGIC_COOKIE:32/big-unsigned,
		 TransactionId:96/big-unsigned>>;
	    rfc3489 ->
		<<Type:16/big-unsigned,
		 Length:16/big-unsigned,
		 TransactionId:128/big-unsigned>>
		    end,
    [Header, FlatAttrs].


%%--------------------------------------------------------------------
%% @spec    (Packet, Offset) ->
%%            {ok, Attributes, MIntegrityOffset} |
%%            {error, bad_attribute}
%%
%%            Packet = binary() "STUN packet received from network"
%%            Offset = integer() "offset to attributes in Packet"
%%
%%            Attributes       = [#stun_attr{}]
%%            MIntegrityOffset = integer() "offset to first MESSAGE-INTEGRITY attribute"
%%
%% @doc     Decode all STUN attributes in Packet. To not create sub-
%%          binarys we work with an offset into one and the same
%%          binary all the time.
%% @end
%%--------------------------------------------------------------------
decode_attributes(Packet, Offset) when is_binary(Packet), is_integer(Offset) ->
    MIntegrityOffset = 0,
    decode_attributes2(Packet, Offset, MIntegrityOffset, []).

decode_attributes2(Packet, Offset, MIO, Res) ->
    case Packet of
	<<_:Offset/binary, AttrType:16/big-unsigned, AttrLen:16/big-unsigned, _/binary>> ->
	    DataOffset = Offset + 2 + 2,
	    %% If this is the first MESSAGE-INTEGRITY attribute (MIO == 0), we set MIO to
	    %% the offset of this attribute minus one (or zero, if that would make it negative)
	    NewMIO =
		if
		    AttrType == ?STUN_ATTRIBUTE_MESSAGE_INTEGRITY, MIO == 0 ->
			lists:max([0, Offset]);
		    true ->
			MIO
		end,

	    case Packet of
		<<_:DataOffset/binary, Value:AttrLen/binary, _/binary>> ->
		    This = #stun_attr{type	= AttrType,
				      label	= attr_to_label(AttrType),
				      length	= AttrLen,
				      value	= Value
				     },
		    decode_attributes2(Packet, DataOffset + AttrLen, NewMIO, [This | Res]);
		_ ->
		    {error, bad_attribute}
	    end;
	<<_:Offset/binary>> ->
	    %% reached end of packet
	    {ok, lists:reverse(Res), MIO};
	_ ->
	    {error, bad_attribute}
    end.


%%--------------------------------------------------------------------
%% @spec    (Type, Attributes) ->
%%            {ok, Matching} |
%%            nomatch
%%
%%            Type       = integer() "STUN attribute number"
%%            Attributes = [#stun_attr{}]
%%
%%            Matching = [#stun_attr{}]
%%
%% @doc     Find all attributes matching Type.
%% @end
%%--------------------------------------------------------------------
get_attribute(Type, Attributes) when is_list(Attributes) ->
    case get_attribute2(Type, Attributes, [], []) of
	{ok, [], _NoMatch} ->
	    nomatch;
	{ok, Match, _NoMatch} ->
	    {ok, Match}
    end.

%%--------------------------------------------------------------------
%% @spec    (Type, Attributes, [], []) ->
%%            {ok, Match, NoMatch}
%%
%%            Type       = integer() "STUN attribute number"
%%            Attributes = [#stun_attr{}]
%%
%%            Match   = [#stun_attr{}]
%%            NoMatch = [#stun_attr{}]
%%
%% @doc     Find all attributes matching Type, and return both the
%%          ones matching and those that don't match in two new lists
%%          of stun_attr records.
%% @end
%%--------------------------------------------------------------------
get_attribute2(Type, [#stun_attr{type = Type} = H | T], Match, NoMatch) ->
    get_attribute2(Type, T, [H | Match], NoMatch);
get_attribute2(Type, [H | T], Match, NoMatch) ->
    get_attribute2(Type, T, Match, [H | NoMatch]);
get_attribute2(_Type, [], Match, NoMatch) ->
    {ok, Match, NoMatch}.


%%--------------------------------------------------------------------
%% @spec
%%    (STUN_ATTRIBUTE_MAPPED_ADDRESS, {Proto, Address, Port}) ->
%%            Attr
%%
%%            Proto   = udp | udp6 | tcp | tcp6 | tls | tls6 | atom()
%%            Address = {A,B,C,D} | {A,B,C,D,E,F,G,H}
%%            Port    = integer()
%%
%%            Attr = binary()
%%
%% @doc     MAPPED-ADDRESS is the IP and port we received a STUN
%%          request from.
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_MAPPED_ADDRESS,	In) ->
    %% 11.1
    mapped_address_encode(?STUN_ATTRIBUTE_MAPPED_ADDRESS, In);

%%create_attribute(?STUN_ATTRIBUTE_RESPONSE_ADDRESS,	_In) ->
%%    %% 11.2
%%    {error, {not_implemented, ?LINE}};

%%--------------------------------------------------------------------
%% @spec
%%    (?STUN_ATTRIBUTE_CHANGED_ADDRESS, {Proto, Address, Port}) ->
%%            Attr
%%
%%            Proto   = udp | udp6 | tcp | tcp6 | tls | tls6 | atom()
%%            Address = {A,B,C,D} | {A,B,C,D,E,F,G,H}
%%            Port    = integer()
%%
%%            Attr = binary()
%%
%% @doc     CHANGED-ADDRESS is the IP and port we _would_ have
%%          responded using if the client had requested it (through a
%%          CHANGE-REQUEST attribute with the appropriate bits set).
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_CHANGED_ADDRESS,	In) ->
    %% 11.3
    mapped_address_encode(?STUN_ATTRIBUTE_CHANGED_ADDRESS, In);

%%--------------------------------------------------------------------
%% @spec
%%    (?STUN_ATTRIBUTE_CHANGE_REQUEST, {ChangeIP, ChangePort}) ->
%%            Attr
%%
%%            ChangeIP   = bool() "client requests us to use an alternate source IP when responing"
%%            ChangePort = bool() "client requests us to use an alternate source port when responing"
%%
%%            Attr = binary()
%%
%% @doc     CHANGE-REQUEST is how the client tells the STUN server to
%%          use an alternate source IP / source port when responding,
%%          or just polls the servers capability to do so.
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST,	In) ->
    %% 11.4
    {ChangeIP, ChangePort} = In,
    Value =
	(if ChangeIP == true -> 4; true -> 0 end) +
	(if ChangePort == true -> 2; true -> 0 end),
    Data = <<Value:32/big-unsigned>>,
    encode_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, Data);

%%--------------------------------------------------------------------
%% @spec
%%    (?STUN_ATTRIBUTE_SOURCE_ADDRESS, {Proto, Address, Port}) ->
%%            Attr
%%
%%            Proto   = udp | udp6 | tcp | tcp6 | tls | tls6 | ...
%%            Address = {A,B,C,D} | {A,B,C,D,E,F,G,H}
%%            Port    = integer()
%%
%%            Attr = binary()
%%
%% @doc     This is the attribute used by the server to tell the
%%          client which IP and port it used to send a STUN response,
%%          in case there is some kind of address translator between
%%          the client and server that obfuscates the true source IP
%%          or port.
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_SOURCE_ADDRESS,	In) ->
    %% 11.5
    mapped_address_encode(?STUN_ATTRIBUTE_SOURCE_ADDRESS, In);

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_USERNAME, Username) ->
%%            Attr
%%
%%            Username = string()
%%
%%            Attr = binary()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_USERNAME,		_In) ->
    %% 11.6
    {error, {not_implemented, ?LINE}};

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_PASSWORD, Password) ->
%%            Attr
%%
%%            Password = string()
%%
%%            Attr = binary()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_PASSWORD,		_In) ->
    %% 11.7
    {error, {not_implemented, ?LINE}};

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_MESSAGE_INTEGRITY, HMAC) ->
%%            Attr
%%
%%            HMAC = string()
%%
%%            Attr = binary()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_MESSAGE_INTEGRITY,	_In) ->
    %% 11.8
    {error, {not_implemented, ?LINE}};

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_ERROR_CODE, {Status, Reason}) ->
%%            Attr
%%
%%            Status = integer()
%%            Reason = string()
%%
%%            Attr = binary()
%%
%% @doc     ERROR-CODE is how a server informs a client about an
%%          non-successfull result of the processing of a request.
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_ERROR_CODE,		In) ->
    %% 11.9
    {Status, Reason} =
	case In of
	    {N, R} when is_integer(N), is_list(R) ->
		{N, R};
	    N when is_integer(N) ->
		{N, error_code_reason(N)}
	end,
    Nxx = Status div 100,
    Rem = Status rem 100,
    Data = [<<0:16, Nxx:8, Rem:8>>,
	    make_even_four_bin(Reason)
	   ],
    encode_attribute(?STUN_ATTRIBUTE_ERROR_CODE, list_to_binary(Data));

%%create_attribute(?STUN_ATTRIBUTE_REFLECTED_FROM,	_In) ->
%%    %% 11.10
%%    {error, {not_implemented, ?LINE}};

%%create_attribute(?STUN_ATTRIBUTE_ALTERNATE_SERVER,	_In) ->
%%    %% 11.11
%%    {error, {not_implemented, ?LINE}};

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_REALM, Realm) ->
%%            Attr
%%
%%            Realm = string()
%%
%%            Attr = binary()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_REALM,			_In) ->
    %% 11.12
    {error, {not_implemented, ?LINE}};

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_NONCE, Nonce) ->
%%            Attr
%%
%%            Nonce = string()
%%
%%            Attr = binary()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_NONCE,			_In) ->
    %% 11.13
    {error, {not_implemented, ?LINE}};

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES, In) ->
%%            Attr
%%
%%            In = [integer()]
%%
%%            Attr = binary()
%%
%% @doc     UNKNOWN-ATTRIBUTES is a list of mandatory attributes the
%%          server does not recognize. Indicates non-conformance in
%%          the server, or updated specifications the client is aware
%%          of but not the server.
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES,	In) ->
    %% 11.14
    Fixed =
	case length(In) rem 2 of
	    0 -> In;
	    1 ->
		%% If the number of unknown attributes is an odd number, one of the
		%% attributes MUST be repeated in the list, so that the total length of
		%% the list is a multiple of 4 bytes.
		H = hd(In),
		[H | In]
	end,
    B = [<<Attr:16/big-unsigned>> || Attr <- Fixed],
    encode_attribute(?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES, list_to_binary(B));

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS, SReq) ->
%%            Attr
%%
%%            SReq = #stun_request{}
%%
%%            Attr = binary()
%%
%% @doc     This is the same as MAPPED-ADDRESS, but the values are XOR
%%          encoded to prevent a NAT searching and replacing it's own
%%          address/port in datagram payloads from interveining with
%%          STUN.
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS,	In) ->
    %% 11.15
    #stun_env{proto = Proto, remote_ip = Address, remote_port = Port} = In#stun_request.env,
    XAddress =
	if
	    Proto == udp; Proto == tcp; Proto == tls ->
		%% IPv4
		{A, B, C, D} = Address,
		%% If the IP address family is IPv4, X-Address is mapped IP address exclusive
		%% or'd with the magic cookie.
		X = ((A bsl 24) + (B bsl 16) + (C bsl 8) + D) bxor ?STUN_MAGIC_COOKIE,
		<<X:32/big-unsigned>>;
	    Proto == udp6; Proto == tcp6; Proto == tls6 ->
		%% IPv6
		T_Id = In#stun_request.t_id,
		<<X:128/big-unsigned>> = <<?STUN_MAGIC_COOKIE:32/big-unsigned, T_Id:96/big-unsigned>>,
		<<A:128/big-unsigned>> = ip_tuple_to_bin(Address),
		%% If the IP address family is IPv6, the X-Address is the mapped IP address
		%% exclusively or'ed with the magic cookie and the 96-bit transaction ID.
		XA = A bxor X,
		<<XA:128/big-unsigned>>
	    end,
    %% X-Port is the mapped port, exclusive or'd with most significant 16
    %% bits of the magic cookie.
    XPort = Port bxor (?STUN_MAGIC_COOKIE bsr 16),

    mapped_address_encode(?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS, {Proto, XAddress, XPort});

%%--------------------------------------------------------------------
%% @spec    (?STUN_ATTRIBUTE_SERVER, In) ->
%%            Attr
%%
%%            In = string()
%%
%%            Attr = binary()
%%
%% @doc     Freeform text description of STUN server.
%% @end
%%--------------------------------------------------------------------
create_attribute(?STUN_ATTRIBUTE_SERVER,		In) ->
    %% 11.16
    encode_attribute(?STUN_ATTRIBUTE_SERVER, make_even_four_bin(In)).

%%create_attribute(?STUN_ATTRIBUTE_ALTERNATE_SERVER_XXX,	_In) ->
%%    %% 11.17
%%    {error, {not_implemented, ?LINE}}; %% duplicate in the draft

%%create_attribute(?STUN_ATTRIBUTE_BINDING_LIFETIME,	_In) ->
%%    %% 11.18
%%    {error, {not_implemented, ?LINE}}.

%%--------------------------------------------------------------------
%% @spec    (Type, {Proto, IP, Port}) ->
%%            Attr
%%
%%            Type    = integer() "STUN attribute number"
%%            Proto   = udp | udp6 | tcp | tcp6 | tls | tls6 | atom()
%%            Address = {A,B,C,D} | {A,B,C,D,E,F,G,H} | binary() "four or 16 bytes (IPv4 or IPv6)"
%%            Port    = integer()
%%
%%            Attr = binary()
%%
%% @doc     Encode IP and port number in the MAPPED-ADDRESS format. A
%%          number of other attributes have semantics equivalent with
%%          MAPPED-ADDRESS, so a helper function makes sense.
%% @end
%%--------------------------------------------------------------------
mapped_address_encode(Type, {Proto, IP, Port}) ->
    Family =
	case Proto of
	    Inet when Inet == udp; Inet == tcp; Inet == tls ->
		?STUN_ADDRESS_FAMILY_INET;
	    Inet6 when Inet6 == udp6; Inet6 == tcp6; Inet6 == tls6 ->
		?STUN_ADDRESS_FAMILY_INET6
	end,
    Address =
	case IP of
	    <<_:4/binary>> ->
		IP;
	    <<_:16/binary>> ->
		IP;
	    _ when is_tuple(IP), size(IP) == 4; size(IP) == 8 ->
		ip_tuple_to_bin(IP)
	end,

    L = [<<0:8, Family:8, Port:16/big-unsigned>>,
	 Address],
    encode_attribute(Type, list_to_binary(L)).


%%--------------------------------------------------------------------
%% @spec    (Type, Value) ->
%%            Attr
%%
%%            Type  = integer() "STUN attribute number"
%%            Value = binary()
%%
%%            Attr = binary()
%%
%% @doc     TLV (type-length-value) encode a STUN attribute.
%% @end
%%--------------------------------------------------------------------
encode_attribute(Type, Value) when is_integer(Type), is_binary(Value) ->
    Len = size(Value),
    list_to_binary([<<Type:16/big-unsigned, Len:16/big-unsigned>>,
		    Value]).

%%--------------------------------------------------------------------
%% @spec    (L) ->
%%            Value
%%
%%            L = string()
%%
%%            Value = binary()
%%
%% @doc     All STUN attributes must be a multiple of four bytes, so
%%          we need to pad strings with spaces at the end.
%% @end
%%--------------------------------------------------------------------
make_even_four_bin(L) when is_list(L) ->
    case length(L) rem 4 of
	 3 -> list_to_binary([L, <<" ">>]);
	 2 -> list_to_binary([L, <<"  ">>]);
	 1 -> list_to_binary([L, <<"   ">>]);
	 0 -> list_to_binary(L)
     end.

%%--------------------------------------------------------------------
%% @spec    (Address) ->
%%            Value
%%
%%            Address = {A,B,C,D} | {A,B,C,D,E,F,G,H}
%%
%%            Value = binary()
%%
%% @doc     Turn an IP address tuple into a network order binary.
%% @end
%%--------------------------------------------------------------------
ip_tuple_to_bin({A1,A2,A3,A4}) ->
    <<A1,A2,A3,A4>>;
ip_tuple_to_bin({A1,A2,A3,A4,A5,A6,A7,A8}) ->
    %% XXX not sure if this is a correct way to encode IPv6 address into
    %% network byte order (big endian)
    <<A1:16/big-unsigned, A2:16/big-unsigned, A3:16/big-unsigned, A4:16/big-unsigned,
     A5:16/big-unsigned, A6:16/big-unsigned, A7:16/big-unsigned, A8:16/big-unsigned>>.


%%--------------------------------------------------------------------
%% @spec    (Code) -> term()
%%
%%            Code = stun_error_code()
%%
%% @throws  stun_error() 
%%
%% @equiv    stun_error(Code, [])
%% @end
%%--------------------------------------------------------------------
stun_error(N) when is_integer(N) ->
    throw({stun_error, N, []});
stun_error({N, Reason}) when is_integer(N), is_list(Reason) ->
    throw({stun_error, {N, Reason}, []}).

%%--------------------------------------------------------------------
%% @spec    (Code, Attrs) -> term()
%%
%%            Code  = stun_error_code()
%%            Attrs = stun_error_attrs()
%%
%% @throws  stun_error() 
%%
%% @doc     Throw a signal that when caught by the upper level error
%%          handling functions will be turned into a STUN ERROR-CODE
%%          attribute.
%% @end
%%--------------------------------------------------------------------
stun_error(N, Attrs) when (is_integer(N) orelse is_tuple(N)), is_list(Attrs) ->
    throw({stun_error, N, Attrs}).

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            Label | 'unknown_attr'
%%
%%            Number = integer()
%%
%%            Label = atom()
%%
%% @doc     Turn a STUN attribute number into an atom label. The label
%%          is mostly used for debugging/test cases, but the return
%%          value is also significant when we check for unknown
%%          attributes - if a mandatory attribute (Number less than
%%          0x7fff) is not recognized by this function then the STUN
%%          request will be rejected.
%% @end
%%--------------------------------------------------------------------
attr_to_label(?STUN_ATTRIBUTE_MAPPED_ADDRESS) ->	'MAPPED-ADDRESS';
%%attr_to_label(?STUN_ATTRIBUTE_RESPONSE_ADDRESS) ->	'RESPONSE-ADDRESS';
attr_to_label(?STUN_ATTRIBUTE_CHANGE_REQUEST) ->	'CHANGE-REQUEST';
attr_to_label(?STUN_ATTRIBUTE_SOURCE_ADDRESS) ->	'SOURCE-ADDRESS';
attr_to_label(?STUN_ATTRIBUTE_CHANGED_ADDRESS) ->	'CHANGED-ADDRESS';
attr_to_label(?STUN_ATTRIBUTE_USERNAME) ->		'USERNAME';
attr_to_label(?STUN_ATTRIBUTE_PASSWORD) ->		'PASSWORD';
attr_to_label(?STUN_ATTRIBUTE_MESSAGE_INTEGRITY) ->	'MESSAGE-INTEGRITY';
attr_to_label(?STUN_ATTRIBUTE_ERROR_CODE) ->		'ERROR-CODE';
attr_to_label(?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES) ->	'UNKNOWN-ATTRIBUTES';
%%attr_to_label(?STUN_ATTRIBUTE_REFLECTED_FROM) ->	'REFLECTED-FROM';
attr_to_label(?STUN_ATTRIBUTE_ALTERNATE_SERVER) ->	'ALTERNATE-SERVER';
attr_to_label(?STUN_ATTRIBUTE_REALM) ->			'REALM';
attr_to_label(?STUN_ATTRIBUTE_NONCE) ->			'NONCE';
attr_to_label(?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS) ->	'XOR-MAPPED-ADDRESS';
attr_to_label(?STUN_ATTRIBUTE_SERVER) ->		'SERVER';
%%attr_to_label(?STUN_ATTRIBUTE_ALTERNATE_SERVER_XXX) ->	'ALTERNATE-SERVER-XXX';	%% duplicate in the draft
%%attr_to_label(?STUN_ATTRIBUTE_BINDING_LIFETIME) ->	'BINDING-LIFETIME';
attr_to_label(_Unknown) ->				 unknown_attr.


%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            Reason
%%
%%            Number = integer() "STUN attribute number"
%%
%%            Reason = string()
%%
%% @doc     Get the default reason phrase for STUN errors.
%% @end
%%--------------------------------------------------------------------
%% Defaults from section rfc3489bis-03 #11.9 (ERROR-CODE)
error_code_reason(300) -> "Try Alternate";
error_code_reason(400) -> "Bad Request";
error_code_reason(401) -> "Unauthorized";
error_code_reason(420) -> "Unknown Attribute";
error_code_reason(430) -> "Stale Credentials";
error_code_reason(431) -> "Integrity Check Failure";
error_code_reason(432) -> "Missing Username";
error_code_reason(433) -> "Use TLS";
error_code_reason(434) -> "Missing Realm";
error_code_reason(435) -> "Missing Nonce";
error_code_reason(436) -> "Unknown Username";
error_code_reason(437) -> "Stale Nonce";
error_code_reason(500) -> "Server Error";
error_code_reason(600) -> "Global Failure";
error_code_reason(N) when is_integer(N) -> "unknown error".





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

    %% decode_attributes(Packet, Offset)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "decode_attributes/2 - 0"),
    DA_Attr1 = #stun_attr{type   = ?STUN_ATTRIBUTE_CHANGE_REQUEST,
			  label  = 'CHANGE-REQUEST',
			  length = 4,
			  value	 = <<0, 0, 0, 0>>
			 },
    DA_Data1 = <<00, 03, 00, 04, 00, 00, 00, 00>>,

    DA_MI_Attr = #stun_attr{type   = ?STUN_ATTRIBUTE_MESSAGE_INTEGRITY,
			    label  = 'MESSAGE-INTEGRITY',
			    length = 0,
			    value  = <<>>
			   },

    DA_MI_Data = <<?STUN_ATTRIBUTE_MESSAGE_INTEGRITY:16/big-unsigned, 0:16>>,

    autotest:mark(?LINE, "decode_attributes/2 - 1"),
    %% test with single attribute
    {ok, [DA_Attr1], 0} = decode_attributes(DA_Data1, 0),

    autotest:mark(?LINE, "decode_attributes/2 - 2"),
    %% test with two attributes
    {ok, [DA_Attr1, DA_Attr1], 0} = decode_attributes(list_to_binary([DA_Data1, DA_Data1]), 0),

    autotest:mark(?LINE, "decode_attributes/2 - 3"),
    %% test with MESSAGE-INTEGRITY
    {ok, [DA_Attr1, DA_MI_Attr], 8} = decode_attributes(list_to_binary([DA_Data1, DA_MI_Data]), 0),

    autotest:mark(?LINE, "decode_attributes/2 - 4"),
    %% test with MESSAGE-INTEGRITY not as the last attribute
    {ok, [DA_Attr1, DA_MI_Attr, DA_Attr1], 8} =
	decode_attributes(list_to_binary([DA_Data1, DA_MI_Data, DA_Data1]), 0),

    autotest:mark(?LINE, "decode_attributes/2 - 5"),
    %% test with more than one MESSAGE-INTEGRITY attribute
    {ok, [DA_Attr1, DA_MI_Attr, DA_Attr1, DA_MI_Attr], 8} =
	decode_attributes(list_to_binary([DA_Data1, DA_MI_Data, DA_Data1, DA_MI_Data]), 0),

    autotest:mark(?LINE, "decode_attributes/2 - 5"),
    %% test with attribute with invalid length
    {error, bad_attribute} = decode_attributes(<<16#78, 16#10, 1, 2, 3, 4>>, 0),

    %% make_even_four_bin(In)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "make_even_four_bin/1 - 1"),
    <<"TEST">> = make_even_four_bin("TEST"),

    autotest:mark(?LINE, "make_even_four_bin/1 - 2"),
    <<"TEST2   ">> = make_even_four_bin("TEST2"),

    autotest:mark(?LINE, "make_even_four_bin/1 - 3"),
    <<"TEST2   ">> = make_even_four_bin("TEST2 "),

    autotest:mark(?LINE, "make_even_four_bin/1 - 4"),
    <<"TEST2   ">> = make_even_four_bin("TEST2  "),

    autotest:mark(?LINE, "make_even_four_bin/1 - 5"),
    <<"TEST2   ">> = make_even_four_bin("TEST2   "),


    %% create_attribute(Type, In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "create_attribute('UNKNOWN-ATTRIBUTES' ...) - 1"),
    %% test with two (even number)
    {ok, [#stun_attr{type   = ?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES,
		     label  = 'UNKNOWN-ATTRIBUTES',
		     length = 4,
		     value  = <<40:16, 64:16>>
		    }
	 ], 0} =
	decode_attributes(
	  create_attribute(?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES, [40, 64]),
	  0),

    autotest:mark(?LINE, "create_attribute('UNKNOWN-ATTRIBUTES' ...) - 2"),
    %% test with three (odd number, requires padding)
    {ok, [#stun_attr{type   = ?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES,
		     label  = 'UNKNOWN-ATTRIBUTES',
		     length = 8,
		     value  = <<20:16, 20:16, 40:16, 64:16>>
		    }
	 ], 0} =
	decode_attributes(
	  create_attribute(?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES, [20, 40, 64]),
	  0),

    autotest:mark(?LINE, "create_attribute('XOR-MAPPED-ADDRESS' ...) - 1"),
    %% test with the example IPv4 address and port from RFC3489bis-03
    {ok, [#stun_attr{type   = ?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS,
		     label  = 'XOR-MAPPED-ADDRESS',
		     length = 8,
		     value  = <<0, ?STUN_ADDRESS_FAMILY_INET,
			       16#34a1:16/big-unsigned,
			       16#e1baa543:32/big-unsigned>>}
	 ], 0} = decode_attributes(
		   create_attribute(?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS,
				    #stun_request{env = #stun_env{proto		= udp,
								  remote_ip	= {192, 168, 1, 1},
								  remote_port	= 5555
								 }}
				   ),
		   0),

    autotest:mark(?LINE, "create_attribute('XOR-MAPPED-ADDRESS' ...) - 2"),
    {ok, CA_XOR_v6_IP} = inet_parse:ipv6_address("2001:6b0:5:987:4:3:2:1"),
    CA_XOR_v6_X_Port = 16#34a1,
    CA_XOR_v6_X_IP = 1431186559674287241132258419264834836,	%% XXX yet to verify that this is correct
    %% test with IPv6 address
    {ok, [#stun_attr{type   = ?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS,
		     label  = 'XOR-MAPPED-ADDRESS',
		     length = 20,
		     value  = <<0, ?STUN_ADDRESS_FAMILY_INET6,
			       CA_XOR_v6_X_Port:16/big-unsigned,
			       CA_XOR_v6_X_IP:128/big-unsigned>>}
	 ], 0} = decode_attributes(
		   create_attribute(?STUN_ATTRIBUTE_XOR_MAPPED_ADDRESS,
				    #stun_request{env = #stun_env{proto		= udp6,
								  remote_ip	= CA_XOR_v6_IP,
								  remote_port	= 5555
								 },
						  t_id = 123456789
						 }
				   ),
		   0),


    %% create_stun_error(SReq, Number, Attributes)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "create_stun_error/3 - 0"),
    %% test sending of error response (420, unknown attributes)
    SER_TransactionId1 = 4711,
    SER_SReq1 = #stun_request{type	= ?STUN_TYPE_BINDING_REQUEST,
			      rfc	= rfc3489bis,
			      t_id	= SER_TransactionId1
			     },
    SER_RAttrs1 = [create_attribute(?STUN_ATTRIBUTE_UNKNOWN_ATTRIBUTES, [78])],

    autotest:mark(?LINE, "create_stun_error/3 - 1.1"),
    #stun_result{action		= send_response,
		 type		= error,
		 change		= none,
		 response	= [SER_Header1, SER_ResultAttrs1]
		} = create_stun_error(SER_SReq1, 420, SER_RAttrs1),

    autotest:mark(?LINE, "create_stun_error/3 - 1.2"),
    %% verify STUN response header
    <<?STUN_TYPE_BINDING_ERROR_RESPONSE:16/big-unsigned,
     _:16/big-unsigned,
     ?STUN_MAGIC_COOKIE:32/big-unsigned,
     SER_TransactionId1:96/big-unsigned
     >> = SER_Header1,

    autotest:mark(?LINE, "create_stun_error/3 - 1.3"),
    %% verify STUN response attributes
    SER_ResultAttrs1 = list_to_binary([create_attribute(?STUN_ATTRIBUTE_ERROR_CODE, 420),
				       SER_RAttrs1]),


    %% process_attributes(SReq)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_attributes/1 - 0"),
    %% test more than one CHANGE-REQUEST attribute
    {ok, [ProcessAttrNone], 0} = decode_attributes(create_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, {false, false}), 0),
    {ok, [ProcessAttrIP], 0}   = decode_attributes(create_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, {true, false}), 0),
    {ok, [ProcessAttrPort], 0} = decode_attributes(create_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, {false, true}), 0),
    {ok, [ProcessAttrBoth], 0} = decode_attributes(create_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, {true, true}), 0),

    ProcessAttr_Proto		= udp,
    ProcessAttr_LocalIP		= {192, 0, 2, 193},
    ProcessAttr_LocalPort	= 5555,
    ProcessAttr_AltIP		= {192, 0, 2, 194},
    ProcessAttr_AltPort		= 6666,

    ProcessAttr_Env =
	#stun_env{proto		= ProcessAttr_Proto,
		  local_ip	= ProcessAttr_LocalIP,
		  local_port	= ProcessAttr_LocalPort,
		  alt_ip	= ProcessAttr_AltIP,
		  alt_port	= ProcessAttr_AltPort
		 },

    autotest:mark(?LINE, "process_attributes/1 - 1"),
    {stun_error, {400, "Must have 0 or 1 CHANGE-REQUEST attributes"}, []} =
	(catch process_attributes(#stun_request{attrs = [ProcessAttrNone, ProcessAttrIP]})),

    autotest:mark(?LINE, "process_attributes/1 - 2"),
    %% test request to change nothing
    ProcessAttr2_Req = #stun_request{attrs = [ProcessAttrNone],
				     rfc   = rfc3489,
				     env   = ProcessAttr_Env#stun_env{alt_ip = undefined,
								      alt_port = undefined
								     }},

    ProcessAttr2_Res = create_attribute(?STUN_ATTRIBUTE_CHANGED_ADDRESS,
					{ProcessAttr_Proto, ProcessAttr_LocalIP, ProcessAttr_LocalPort}
				       ),

    {none, [ProcessAttr2_Res]} = process_attributes(ProcessAttr2_Req),

    autotest:mark(?LINE, "process_attributes/1 - 3.1"),
    %% test request to change IP, alternate IP present but alternate port not
    ProcessAttr3_1_Req = #stun_request{attrs = [ProcessAttrIP],
				       rfc   = rfc3489,
				       env   = ProcessAttr_Env#stun_env{alt_port = undefined}
				      },
    ProcessAttr3_1_Res = create_attribute(?STUN_ATTRIBUTE_CHANGED_ADDRESS,
					  {ProcessAttr_Proto, ProcessAttr_AltIP, ProcessAttr_LocalPort}
					 ),

    {ip, [ProcessAttr3_1_Res]} = process_attributes(ProcessAttr3_1_Req),


    autotest:mark(?LINE, "process_attributes/1 - 3.2"),
    %% test request to change IP, alternate IP NOT present
    ProcessAttr3_2_Req = #stun_request{attrs = [ProcessAttrIP],
				       rfc   = rfc3489,
				       env   = ProcessAttr_Env#stun_env{alt_ip = undefined}
				      },
    {stun_error, {300, "No alternate IP address available here"}, []} =
	(catch process_attributes(ProcessAttr3_2_Req)),


    autotest:mark(?LINE, "process_attributes/1 - 4.1"),
    %% test request to change port, alternate port present but not alternate IP so local IP should be used
    ProcessAttr4_1_Req = #stun_request{attrs = [ProcessAttrPort],
				       rfc   = rfc3489,
				       env   = ProcessAttr_Env#stun_env{alt_ip = undefined}
				      },
    ProcessAttr4_1_Res = create_attribute(?STUN_ATTRIBUTE_CHANGED_ADDRESS,
					  {ProcessAttr_Proto, ProcessAttr_LocalIP, ProcessAttr_AltPort}
					 ),

    {port, [ProcessAttr4_1_Res]} = process_attributes(ProcessAttr4_1_Req),


    autotest:mark(?LINE, "process_attributes/1 - 4.2"),
    %% test request to change port, alternate port NOT present
    ProcessAttr4_2_Req = #stun_request{attrs = [ProcessAttrPort],
				       rfc   = rfc3489,
				       env   = ProcessAttr_Env#stun_env{alt_port = undefined}
				      },
    {stun_error, {300, "No alternate port available here"}, []} =
	(catch process_attributes(ProcessAttr4_2_Req)),


    autotest:mark(?LINE, "process_attributes/1 - 5.1"),
    %% test request to change both IP and port, both available
    ProcessAttr5_1_Req = #stun_request{attrs = [ProcessAttrBoth],
				       rfc   = rfc3489,
				       env   = ProcessAttr_Env
				      },
    ProcessAttr5_1_Res = create_attribute(?STUN_ATTRIBUTE_CHANGED_ADDRESS,
					  {ProcessAttr_Proto, ProcessAttr_AltIP, ProcessAttr_AltPort}
					 ),

    {both, [ProcessAttr5_1_Res]} = process_attributes(ProcessAttr5_1_Req),


    autotest:mark(?LINE, "process_attributes/1 - 5.2"),
    %% test request to change both IP and port, alternate IP NOT present
    ProcessAttr5_2_Req = #stun_request{attrs = [ProcessAttrBoth],
				       rfc   = rfc3489,
				       env   = ProcessAttr_Env#stun_env{alt_ip = undefined}
				      },
    {stun_error, {300, "No alternate IP address available here"}, []} =
	(catch process_attributes(ProcessAttr5_2_Req)),

    autotest:mark(?LINE, "process_attributes/1 - 5.3"),
    %% test request to change both IP and port, alternate port NOT present
    ProcessAttr5_3_Req = #stun_request{attrs = [ProcessAttrBoth],
				       rfc   = rfc3489,
				       env   = ProcessAttr_Env#stun_env{alt_port = undefined}
				      },
    {stun_error, {300, "No alternate port available here"}, []} =
	(catch process_attributes(ProcessAttr5_3_Req)),

    autotest:mark(?LINE, "process_attributes/1 - 6"),
    %% test request to change both IP and port,both available. RFC3489bis client.
    ProcessAttr6_Req = #stun_request{attrs = [ProcessAttrBoth],
				     rfc   = rfc3489bis,
				     env   = ProcessAttr_Env
				    },
    {both, []} = process_attributes(ProcessAttr6_Req),


    autotest:mark(?LINE, "process_attributes/1 - 7"),
    %% verify that we ignore certain attributes in requests
    {ok, [ProcessAttrServer], 0} = decode_attributes(create_attribute(?STUN_ATTRIBUTE_SERVER, "TEST"), 0),
    {ok, [ProcessAttrMIntegr], 0} = decode_attributes(encode_attribute(?STUN_ATTRIBUTE_MESSAGE_INTEGRITY,
								       list_to_binary(lists:seq(0, 64))), 0),

    ProcessAttr7_Req = #stun_request{attrs = [ProcessAttrServer, ProcessAttrNone, ProcessAttrMIntegr],
				     rfc   = rfc3489bis,
				     env   = ProcessAttr_Env
				    },
    {none, []} = process_attributes(ProcessAttr7_Req),

    autotest:mark(?LINE, "process_attributes/1 - 8"),
    %% test with strange attribute, should not happen since we check for unknown attributes first I guess...
    {ok, [ProcessAttrStrange], 0} = decode_attributes(encode_attribute(16#7878, <<"TEST UNKNOWN DATA">>), 0),
    ProcessAttr8_Req = #stun_request{attrs = [ProcessAttrStrange],
				     rfc   = rfc3489,
				     env   = ProcessAttr_Env
				    },
    {stun_error, 300, []} =
	(catch process_attributes(ProcessAttr8_Req)),




    %%
    %% WHOLE SHEBANG TESTS BELOW THIS POINT
    %%

    %% process(Packet, Env)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process/2 - 0"),

    Process_ReqEnv =
	#stun_env{proto		= udp,
		  local_ip	= {192,0,2,190},
		  local_port	= 2233,
		  remote_ip	= {192,0,2,192},
		  remote_ip_str	= "192.0.2.192",
		  remote_port	= 1919,
		  alt_ip	= undefined,
		  alt_port	= 65512
		 },

    {ok, Process_ReqEnv_v6_LocalIP} = inet_parse:ipv6_address("2001:6b0:5:987::abcd"),
    {ok, Process_ReqEnv_v6_AltIP} = inet_parse:ipv6_address("2001:6b0:5:987::bcde"),
    Process_ReqEnv_v6_RemoteIPstr = "2001:6b0:5:987::1234",
    {ok, Process_ReqEnv_v6_RemoteIP} = inet_parse:ipv6_address(Process_ReqEnv_v6_RemoteIPstr),
    Process_ReqEnv_v6 =
	#stun_env{proto		= udp6,
		  local_ip	= Process_ReqEnv_v6_LocalIP,
		  local_port	= 6666,
		  remote_ip	= Process_ReqEnv_v6_RemoteIP,
		  remote_ip_str	= Process_ReqEnv_v6_RemoteIPstr,
		  remote_port	= 2345,
		  alt_ip	= Process_ReqEnv_v6_AltIP,
		  alt_port	= 12
		 },

    autotest:mark(?LINE, "process/2 - 1.0"),

    %% first, set up a typical binding request from a RFC3489 client
    Process_ReqAttr1 = create_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, {false, false}),
    Process_ReqAttr1Size = size(Process_ReqAttr1),
    Process_Req1Transaction = 112233,
    Process_Req1 =
	list_to_binary([
			 <<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
			  Process_ReqAttr1Size:16/big-unsigned,
			  Process_Req1Transaction:128/big-unsigned>>,
			 Process_ReqAttr1]),

    autotest:mark(?LINE, "process/2 - 1.1"),
    %% now process it
    #stun_result{action		= send_response,
		 type		= ok,
		 change		= none,
		 response	= Process_Res1
		} = process(Process_Req1, Process_ReqEnv),

    autotest:mark(?LINE, "process/2 - 1.2"),
    %% verify the header part first
    <<?STUN_TYPE_BINDING_RESPONSE:16/big-unsigned,
     Process_Res1AttrSize:16/big-unsigned,
     Process_Req1Transaction:128/big-unsigned,
     Process_Res1Attr/binary>> = list_to_binary(Process_Res1),

    autotest:mark(?LINE, "process/2 - 1.3"),
    %% verify size of attributes part
    Process_Res1AttrSize = size(Process_Res1Attr),

    %% verify result
    Process_Req1Port = Process_ReqEnv#stun_env.remote_port,
    Process_Req1LocalPort = Process_ReqEnv#stun_env.local_port,
    Process_Req1AltPort = Process_ReqEnv#stun_env.alt_port,
    Process_Res2Match =
	[{'SOURCE-ADDRESS',	<<0:8,
				 ?STUN_ADDRESS_FAMILY_INET:8,
				 Process_Req1LocalPort:16/big-unsigned,
				 192,0,2,190>> },
	 {'MAPPED-ADDRESS',	<<0:8,
				 ?STUN_ADDRESS_FAMILY_INET:8,
				 Process_Req1Port:16/big-unsigned,
				 192,0,2,192>> },
	 {'CHANGED-ADDRESS',	<<0:8,
				 ?STUN_ADDRESS_FAMILY_INET:8,
				 Process_Req1AltPort:16/big-unsigned,
				 192,0,2,190>> }
	],

    test_verify_attributes(Process_Res2Match, Process_Res1Attr, "process/2 - 1.", 4),


    autotest:mark(?LINE, "process/2 - 2.0"),
    %% set up RFC3489 binding request much like the previous test, only this time we
    %% require an alternate IP address
    Process_ReqAttr2 = create_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, {true, false}),
    Process_ReqAttr2Size = size(Process_ReqAttr2),
    Process_Req2 =
	list_to_binary([
			 <<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
			  Process_ReqAttr2Size:16/big-unsigned,
			  Process_Req1Transaction:128/big-unsigned>>,
			Process_ReqAttr2]),

    autotest:mark(?LINE, "process/2 - 2.1"),
    %% now process it, should result in error since we have no alternate IP
    #stun_result{action 	= send_response,
		 type		= error,
		 response 	= [<<_:20/binary>>, Process_Res2Attrs]
		} = process(Process_Req2, Process_ReqEnv),

    %% verify result
    test_verify_attributes([{error, 300, "No alternate IP address available here"}],
			   Process_Res2Attrs, "process/2 - 2.", 2),


    autotest:mark(?LINE, "process/2 - 3.1"),
    %% test not stun
    {error, not_stun} = process(<<10,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0>>, #stun_env{}),

    autotest:mark(?LINE, "process/2 - 3.2"),
    %% test not stun - too short
    {error, not_stun} = process(<<10,2,3,4,5,6,7>>, #stun_env{}),


    autotest:mark(?LINE, "process/2 - 4.1"),
    %% test invalid length
    Process_Req4 = list_to_binary([Process_Req2, <<1>>]),
    #stun_result{action		= send_response,
		 type		= error,
		 response	= [<<?STUN_TYPE_BINDING_ERROR_RESPONSE:16/big-unsigned,
				    _:18/binary>>, Process_Res4Attrs]
		} = process(Process_Req4, Process_ReqEnv),

    %% verify result
    test_verify_attributes([{error, 400, "Bad Request"}],
			   Process_Res4Attrs, "process/2 - 4.", 2),


    autotest:mark(?LINE, "process/2 - 5.0"),
    %% test invalid attribute, but correct length in STUN header
    Process_Req5 =
	list_to_binary([
			 <<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
			  1:16/big-unsigned,
			  Process_Req1Transaction:128/big-unsigned>>,
			<<1>>]),

    autotest:mark(?LINE, "process/2 - 5.1"),
    #stun_result{action		= send_response,
		 type		= error,
		 response	= [<<?STUN_TYPE_BINDING_ERROR_RESPONSE:16/big-unsigned,
				    _:18/binary>>, Process_Res5Attrs]
		} = process(Process_Req5, Process_ReqEnv),

    %% verify result
    test_verify_attributes([{error, 400, "Bad Request"}],
			   Process_Res5Attrs, "process/2 - 5.", 2),


    autotest:mark(?LINE, "process/2 - 6.0"),
    %% test with unimplemented STUN request type
    Process_Req6Transaction = 16#bad0cafe,
    Process_Req6UnknownRequest = 16#0009,
    Process_Req6 = <<Process_Req6UnknownRequest:16/big-unsigned,
		    0:16/big-unsigned,
		    Process_Req6Transaction:128/big-unsigned>>,

    Process_Req6UnknownResponse = Process_Req6UnknownRequest + 16#110,

    autotest:mark(?LINE, "process/2 - 6.1"),
    #stun_result{action		= send_response,
		 type		= error,
		 response	= [<<Process_Req6UnknownResponse:16/big-unsigned,
				    _:16/big-unsigned,
				    Process_Req6Transaction:128/big-unsigned>>,
				   Process_Res6Attrs]
		} = process(Process_Req6, Process_ReqEnv),

    %% verify result
    test_verify_attributes([{error, 300, "Try Alternate"}],
			   Process_Res6Attrs, "process/2 - 6.", 2),


    autotest:mark(?LINE, "process/2 - 7.0"),
    %% test NAT keepalive usage with magic cookie
    Process_Req7Transaction = 16#99999,
    Process_Req7 = <<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
		    0:16/big-unsigned,
		    ?STUN_MAGIC_COOKIE:32/big-unsigned,
		    Process_Req7Transaction:96/big-unsigned>>,

    autotest:mark(?LINE, "process/2 - 7.1"),
    #stun_result{action		= send_response,
		 type		= ok,
		 change		= none,
		 response	= [<<?STUN_TYPE_BINDING_RESPONSE:16/big-unsigned,
				    _:16/big-unsigned,
				    ?STUN_MAGIC_COOKIE:32/big-unsigned,
				    Process_Req7Transaction:96/big-unsigned>>,
				   Process_Res7Attrs]
		} = process(Process_Req7, Process_ReqEnv),

    Process_Res7_XPort = Process_ReqEnv#stun_env.remote_port bxor 16#2112,
    Process_Res7_XAddr = 16#c00002c0 bxor ?STUN_MAGIC_COOKIE,		%% 192.0.2.192 == 0xc00002c0

    Process_Res7Match =
	[{'XOR-MAPPED-ADDRESS',	<<0:8,
				 ?STUN_ADDRESS_FAMILY_INET:8,
				 Process_Res7_XPort:16/big-unsigned,
				 Process_Res7_XAddr:32/big-unsigned>> }
	],

    %% verify response attributes
    test_verify_attributes(Process_Res7Match, Process_Res7Attrs, "process/2 - 7.", 2),


    autotest:mark(?LINE, "process/2 - 8"),
    %% test unknown mandatory attribute, STUN request should be rejected with a 420
    Process_Req8AttrType = 16#7878,		    %% unknown attribute, less than 0x7fff
    Process_Req8Attr = encode_attribute(Process_Req8AttrType, <<>>),
    Process_ReqAttr8Size = size(Process_Req8Attr),
    Process_Req8Transaction = 16#aabbcc,
    Process_Req8 =
	list_to_binary([
			<<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
			 Process_ReqAttr8Size:16/big-unsigned,
			 ?STUN_MAGIC_COOKIE:32/big-unsigned,
			 Process_Req8Transaction:96/big-unsigned>>,
			Process_Req8Attr]),

    autotest:mark(?LINE, "process/2 - 8.1"),
    %% now process it
    #stun_result{action		= send_response,
		 type		= error,
		 change		= none,
		 response	= [<<?STUN_TYPE_BINDING_ERROR_RESPONSE:16/big-unsigned,
				    _:16/big-unsigned,
				    ?STUN_MAGIC_COOKIE:32/big-unsigned,
				    Process_Req8Transaction:96/big-unsigned>>,
				   Process_Res8Attrs]
		} = process(Process_Req8, Process_ReqEnv),

    %% verify response attributes
    Process_Res8Match = [{error, 420, "Unknown Attribute"},
			 {'UNKNOWN-ATTRIBUTES', <<Process_Req8AttrType:16/big-unsigned,
						 Process_Req8AttrType:16/big-unsigned>>},
			 {'SERVER',	make_even_four_bin(?SERVER_IDENTIFICATION)}
			],
    test_verify_attributes(Process_Res8Match, Process_Res8Attrs, "process/2 - 8.", 2),


    autotest:mark(?LINE, "process/2 - 9.0"),
    %% test that we reject requests demanding MESSAGE-INTEGRITY (since we don't support that yet)
    Process_ReqAttr9 = encode_attribute(?STUN_ATTRIBUTE_MESSAGE_INTEGRITY,
					list_to_binary(lists:seq(0, 64))),
    Process_ReqAttr9Size = size(Process_ReqAttr9),
    Process_Req9 =
	list_to_binary([
			 <<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
			  Process_ReqAttr9Size:16/big-unsigned,
			  ?STUN_MAGIC_COOKIE:32/big-unsigned,
			  Process_Req1Transaction:96/big-unsigned>>,
			Process_ReqAttr9]),

    autotest:mark(?LINE, "process/2 - 9.1"),
    %% now process it, should result in 300 error since we haven't implemented MESSAGE-INTEGRITY yet
    #stun_result{action 	= send_response,
		 type		= error,
		 response 	= [<<_:20/binary>>, Process_Res9Attrs]
		} = process(Process_Req9, Process_ReqEnv),

    %% verify result
    test_verify_attributes([{error, 300, "Try Alternate"}],
			   Process_Res9Attrs, "process/2 - 9.", 2),

    autotest:mark(?LINE, "process/2 - 9.4"),
    Process_Req9_4 = <<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
		      0:16/big-unsigned,
		      ?STUN_MAGIC_COOKIE:32/big-unsigned,
		      Process_Req1Transaction:96/big-unsigned>>,
    %% test that we also reject messages without MESSAGE-INTEGRITY if allow_no_msg_integr == false
    #stun_result{action 	= send_response,
		 type		= error,
		 response 	= [<<_:20/binary>>, Process_Res9_4Attrs]
		} = process(Process_Req9_4, Process_ReqEnv#stun_env{allow_no_msg_integr = false}),

    %% verify result
    test_verify_attributes([{error, 300, "Try Alternate"}],
			   Process_Res9_4Attrs, "process/2 - 9.", 5),


    autotest:mark(?LINE, "process/2 - 10.0"),
    %% test with more than one MESSAGE-INTEGRITY attribute
    Process_ReqAttr10 = encode_attribute(?STUN_ATTRIBUTE_MESSAGE_INTEGRITY,
					 list_to_binary(lists:seq(0, 64))),
    Process_ReqAttr10Size = size(Process_ReqAttr10) * 2,
    Process_Req10 =
	list_to_binary([
			 <<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
			  Process_ReqAttr10Size:16/big-unsigned,
			  ?STUN_MAGIC_COOKIE:32/big-unsigned,
			  Process_Req1Transaction:96/big-unsigned>>,
			Process_ReqAttr10, Process_ReqAttr10]),

    autotest:mark(?LINE, "process/2 - 10.1"),
    %% now process it, should result in 400 error
    #stun_result{action 	= send_response,
		 type		= error,
		 response 	= [<<_:20/binary>>, Process_Res10Attrs]
		} = process(Process_Req10, Process_ReqEnv),

    %% verify result
    test_verify_attributes([{error, 400, "Bad Request"}],
			   Process_Res10Attrs, "process/2 - 10.", 2),



    autotest:mark(?LINE, "process/2 - 11.0"),
    %% first, set up a typical IPv6 binding request from a RFC3489 client
    Process_ReqAttr11 = create_attribute(?STUN_ATTRIBUTE_CHANGE_REQUEST, {false, false}),
    Process_ReqAttr11Size = size(Process_ReqAttr11),
    Process_Req11Transaction = 667766776688,
    Process_Req11 =
	list_to_binary([
			 <<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
			  Process_ReqAttr11Size:16/big-unsigned,
			  Process_Req11Transaction:128/big-unsigned>>,
			 Process_ReqAttr11]),

    autotest:mark(?LINE, "process/2 - 11.1"),
    %% now process it
    #stun_result{action		= send_response,
		 type		= ok,
		 change		= none,
		 response	= Process_Res11
		} = process(Process_Req11, Process_ReqEnv_v6),

    autotest:mark(?LINE, "process/2 - 11.2"),
    %% verify the header part first
    <<?STUN_TYPE_BINDING_RESPONSE:16/big-unsigned,
     Process_Res11AttrSize:16/big-unsigned,
     Process_Req11Transaction:128/big-unsigned,
     Process_Res11Attr/binary>> = list_to_binary(Process_Res11),

    autotest:mark(?LINE, "process/2 - 11.3"),
    %% verify size of attributes part
    Process_Res11AttrSize = size(Process_Res11Attr),

    %% verify result
    Process_Req11Port = Process_ReqEnv_v6#stun_env.remote_port,
    Process_Req11LocalPort = Process_ReqEnv_v6#stun_env.local_port,
    Process_Req11AltPort = Process_ReqEnv_v6#stun_env.alt_port,

    Process_Res11Match =
	[{'SOURCE-ADDRESS',	list_to_binary([<<0:8,
						 ?STUN_ADDRESS_FAMILY_INET6:8,
						 Process_Req11LocalPort:16/big-unsigned>>,
						ip_tuple_to_bin(Process_ReqEnv_v6_LocalIP)
					       ])},
	 {'MAPPED-ADDRESS',	list_to_binary([<<0:8,
						 ?STUN_ADDRESS_FAMILY_INET6:8,
						 Process_Req11Port:16/big-unsigned>>,
						ip_tuple_to_bin(Process_ReqEnv_v6_RemoteIP)
					       ])},
	 {'CHANGED-ADDRESS',	list_to_binary([<<0:8,
						 ?STUN_ADDRESS_FAMILY_INET6:8,
						 Process_Req11AltPort:16/big-unsigned>>,
						ip_tuple_to_bin(Process_ReqEnv_v6_AltIP)
					       ])}
	],

    test_verify_attributes(Process_Res11Match, Process_Res11Attr, "process/2 - 11.", 4),


    autotest:mark(?LINE, "process/2 - 12"),
    %% test with truncated packet
    {error, not_stun} = process(<<?STUN_TYPE_BINDING_REQUEST:16/big-unsigned,
				 0:16/big-unsigned>>, Process_ReqEnv),




    %% process_stream(Stream, Env)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_stream/2 - 1"),
    %% test with too short packet (should have 12 bytes of attribute data)
    ProcessS_Req1 = <<1:16/big-unsigned,
		     12:16/big-unsigned,
		     16#0bad0cafe:128/big-unsigned>>,

    {need_more_data, 12} = process_stream(ProcessS_Req1, Process_ReqEnv_v6),


    autotest:mark(?LINE, "process_stream/2 - 2.1"),
    %% test way too short packet (need minimum of four bytes)
    {need_more_data, 3} = process_stream(<<0>>, Process_ReqEnv),

    autotest:mark(?LINE, "process_stream/2 - 2.2"),
    %% test process_stream guard check
    {need_more_data, 3} = process_stream(<<1>>, Process_ReqEnv),



    autotest:mark(?LINE, "process_stream/2 - 3.0"),
    %% test complete packet with unimplemented STUN request type, we are really just trying to test the de-framing
    ProcessS_Req3Transaction = 16#bad00cafe,
    ProcessS_Req3UnknownRequest = 16#0009,

    ProcessS_Req3 = <<ProcessS_Req3UnknownRequest:16/big-unsigned,
		     0:16/big-unsigned,
		     ProcessS_Req3Transaction:128/big-unsigned>>,
    ProcessS_Req3UnknownResponse = ProcessS_Req3UnknownRequest + 16#110,

    autotest:mark(?LINE, "process_stream/2 - 3.1"),
    {ok,
     #stun_result{action	= send_response,
		  type		= error,
		  response	= [<<ProcessS_Req3UnknownResponse:16/big-unsigned,
				    _:16/big-unsigned,
				    ProcessS_Req3Transaction:128/big-unsigned>>,
				   ProcessS_Res3Attrs]
		 },
     <<>>} = process_stream(ProcessS_Req3, Process_ReqEnv),

    %% verify result
    test_verify_attributes([{error, 300, "Try Alternate"}],
			   ProcessS_Res3Attrs, "process_stream/2 - 3.", 2),


    autotest:mark(?LINE, "process_stream/2 - 3.2"),
    %% test same thing with extra data
    {ok,
     #stun_result{action	= send_response,
		  type		= error,
		  response	= [<<ProcessS_Req3UnknownResponse:16/big-unsigned,
				    _:16/big-unsigned,
				    ProcessS_Req3Transaction:128/big-unsigned>>,
				   ProcessS_Res3Attrs]
		 },
     <<"extra">>} = process_stream(list_to_binary([ProcessS_Req3, <<"extra">>]), Process_ReqEnv),

    autotest:mark(?LINE, "process_stream/2 - 4"),
    %% test bogus data
    {error, not_stun} = process_stream(<<"INVITE sip:ft@example.org SIP/2.0\r\n">>, Process_ReqEnv),

    ok.



test_verify_attributes(Match, Attrs, Test, TestNum) when is_list(Match), is_binary(Attrs),
							 is_list(Test), is_integer(TestNum) ->
    autotest:mark(?LINE, Test ++ integer_to_list(TestNum)),
    {ok, ParsedAttrs, _MessageIntegrityBytes} = decode_attributes(Attrs, 0),
    MLen = length(Match),
    case length(ParsedAttrs) of
	MLen -> ok;
	PLen ->
	    Labels = [E#stun_attr.label || E <- ParsedAttrs],
	    Msg = io_lib:format("wrong number of STUN attributes, expected ~p got ~p (~p)",
				[MLen, PLen, Labels]),
	    throw({error, lists:flatten(Msg)})
    end,

    test_verify_errors2(Match, ParsedAttrs, Test, TestNum + 1).

test_verify_errors2([H | T], Attrs, Test, TestNum) ->
    autotest:mark(?LINE, Test ++ integer_to_list(TestNum)),

    case H of
	{error, ErrorNum, Reason} when is_integer(ErrorNum), is_list(Reason) ->
	    ok = test_expect_error_code(ErrorNum, Reason, Attrs);
	{Label, Value} when is_atom(Label), is_binary(Value) ->
	    case test_get_attr_value(Label, Attrs) of
		[Value] ->
		    ok;
		OtherValue ->
		    Msg = io_lib:format("Attribute ~p has wrong value, expected ~p got ~p",
					[Label, Value, OtherValue]),
		    throw({error, lists:flatten(Msg)})
	    end
    end,

    test_verify_errors2(T, Attrs, Test, TestNum + 1);
test_verify_errors2([], _Attrs, _Test, _TestNum) ->
    ok.

test_expect_error_code(Num, Str, Attrs) when is_integer(Num), is_list(Str) ->
    Class = Num div 100,
    Rem = Num rem 100,
    Padded = make_even_four_bin(Str),
    case test_get_attr_value('ERROR-CODE', Attrs) of
	[<<0:16, Class:8, Rem:8, Padded/binary>>] ->
	    ok;
	[<<0:16, Class:8, Rem:8, Reason/binary>>] ->
	    Msg = io_lib:format("test got right STUN error (~p) but wrong reason, expected ~p got ~p",
				[Num, binary_to_list(Padded), binary_to_list(Reason)]),
	    throw({error, lists:flatten(Msg)});
	[<<0:16, OClass:8, ORem:8, Reason/binary>>] ->
	    Msg = io_lib:format("test got the wrong STUN error (~p ~p), expected '~p ~p'",
                                [(OClass * 100) + ORem, binary_to_list(Reason), Num, binary_to_list(Padded)]),
	    throw({error, lists:flatten(Msg)})
    end,
    ok.


test_get_attr_value(Label, Attrs) when is_atom(Label), is_list(Attrs) ->
    [E#stun_attr.value || E <- Attrs, E#stun_attr.label == Label].
