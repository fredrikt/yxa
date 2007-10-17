%%%-------------------------------------------------------------------
%%% File    : transportlayer
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Transport layer functions.
%%%
%%% @since    01 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------

-module(transportlayer).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 send_proxy_request/4,
	 send_proxy_response/2,
	 send_result/5,
	 send_result/6,
	 stateless_proxy_request/2,
	 stateless_proxy_ack/3,

	 report_unreachable/3,
	 report_unreachable/4,
	 remove_blacklisting/1,
	 is_eligible_dst/1,

	 test/0
	]).

%% exports only intended for use by siprequest module
-export([
	 send_response/2
	]).
%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @equiv    sipsocket:start_link()
%% @end
%%--------------------------------------------------------------------
start_link() ->
    sipsocket:start_link().

%%--------------------------------------------------------------------
%% @spec    (Socket, Response) ->
%%            {error, invalid_Via} |
%%            SendResult
%%
%%            Socket   = #sipsocket{} | none
%%            Response = #response{}
%%
%%            SendResult = term() "result of send_response()"
%%
%% @doc     Extract the top Via from Response, and send this response
%%          to that location (destination).
%% @end
%%--------------------------------------------------------------------
send_proxy_response(Socket, Response)
  when is_record(Socket, sipsocket); Socket == none, is_record(Response, response) ->
    case sipheader:via(Response#response.header) of
	[_Self] ->
	    logger:log(error, "Transport layer: Can't proxy response ~p ~s because it contains just one or less Via "
		       "and that should be mine!", [Response#response.status, Response#response.reason]),
	    {error, invalid_Via};
	[_Self | Via] ->
	    %% Remove Via matching me (XXX should check that it does match me)
	    NewHeader = keylist:set("Via", sipheader:via_print(Via), Response#response.header),
	    NewResponse = Response#response{header = NewHeader},
	    send_response(Socket, NewResponse)
    end.

%%--------------------------------------------------------------------
%% @spec    (Socket, Request, Dst, ViaParameters) ->
%%            {error, Reason}             |
%%            {ok, SipSocket, UsedBranch} 
%%
%%            Socket        = #sipsocket{} | none "socket to use for sending this request"
%%            Request       = #request{}
%%            Dst           = #sipdst{}
%%            ViaParameters = [{Key, Value}]
%%
%%            Reason     = timeout | string() | term()
%%            SipSocket  = #sipsocket{} "the socket used"
%%            UsedBranch = string() "the complete branch finally used"
%%
%% @doc     Prepare for proxying a request. The preparation process is
%%          basically to get us a list() of sipdst records and then
%%          calling send_to_available_dst(). Note : To use this
%%          function, you should already have called
%%          check_proxy_request (directly or indirectly).
%% @end
%%--------------------------------------------------------------------
send_proxy_request(Socket, Request, Dst, ViaParameters)
  when is_record(Socket, sipsocket); Socket == none, is_record(Request, request), is_record(Dst, sipdst) ->
    #sipdst{addr  = IP,
	    port  = Port,
	    proto = Proto
	   } = Dst,
    DestStr = sipdst:dst2str(Dst),
    case get_good_socket(Socket, Dst) of
	{error, {timeout, _}} ->
	    logger:log(debug, "Transport layer: Failed to get ~p socket for ~s : gen_server timeout",
		       [Proto, DestStr]),
	    {error, timeout};
	{error, What} ->
	    logger:log(debug, "Transport layer: Failed to get ~p socket for ~s : ~p",
		       [Proto, DestStr, What]),
	    {error, What};
	SipSocket when is_record(SipSocket, sipsocket) ->
	    #request{method = Method,
		     uri    = OrigURI,
		     header = Header,
		     body   = Body
		    } = Request,
	    NewHeader1 = siprequest:proxy_add_via(Header, OrigURI, ViaParameters, Proto),
	    NewHeader2 = siprequest:fix_content_length(NewHeader1, Body),
	    BinLine1 = list_to_binary([Method, " ", sipurl:print(Dst#sipdst.uri), " SIP/2.0"]),
	    BinMsg = siprequest:binary_make_message(BinLine1, NewHeader2, Body),
	    SendRes = sipsocket:send(SipSocket, Proto, IP, Port, BinMsg),
	    case SendRes of
		ok ->
		    %% Log the request we just sent out. Since we have it in binary format already,
		    %% and list operations on big lists are quite expensive, we avoid making it a
		    %% list that would then be made a binary again by the logger, by using the
		    %% special logger:log_iolist() here.
		    LogPrefix1 = <<"Transport layer: sent request(">>,
		    LogPrefix2 = io_lib:format("~p bytes, dst=~s) :", [size(BinMsg), DestStr]),
		    logger:log_iolist(debug, [LogPrefix1, LogPrefix2, 10, BinMsg, 10]),
		    TopVia = sipheader:topvia(NewHeader2),
		    UsedBranch = lists:flatten(sipheader:get_via_branch_full(TopVia)),
		    {ok, SipSocket, UsedBranch};
		{error, E} ->
		    %% Don't log the actual request when we fail, since that would fill up our logs
		    %% with requests failed for one destination (TCP for example) and then succeeding
		    %% for another (UDP for example).
		    logger:log(debug, "Transport layer: Failed sending request (~p bytes) to ~p:~s:~p, error ~p",
			       [size(BinMsg), Proto, IP, Port, E]),
		    {error, E}
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (RequestHeader, Socket, Body, Status, Reason) ->
%%            Res                 |
%%            {senderror, Reason}
%%
%%            RequestHeader = #keylist{} "header of SIP request we should respond to"
%%            Socket        = none | #sipsocket{}
%%            Body          = string() | binary()
%%            Status        = integer() "SIP status code"
%%            Reason        = string() "SIP reason phrase"
%%
%%            Res    = term() "result of send_response_to(...)"
%%            Reason = string()
%%
%% @doc     Create a response (with minimal headers) to a request and
%%          send it.
%% @end
%%--------------------------------------------------------------------
send_result(RequestHeader, Socket, Body, Status, Reason)
  when is_record(RequestHeader, keylist), (is_record(Socket, sipsocket) orelse Socket == none),
       (is_binary(Body) orelse is_list(Body)), is_integer(Status), is_list(Reason) ->
    Response1 = #response{status=Status, reason=Reason,
			  header=siprequest:standardcopy(RequestHeader, [])},
    Response = siprequest:set_response_body(Response1, Body),
    send_response(Socket, Response).

%%--------------------------------------------------------------------
%% @spec    (RequestHeader, Socket, Body, Status, Reason,
%%          ExtraHeaders) ->
%%            Res                 |
%%            {senderror, Reason}
%%
%%            RequestHeader = #keylist{} "header of SIP request we should respond to"
%%            Socket        = none | #sipsocket{}
%%            Body          = string() | binary()
%%            Status        = integer() "SIP status code"
%%            Reason        = string() "SIP reason phrase"
%%            ExtraHeaders  = [{Key, Value}]
%%            Key           = string() "SIP header name"
%%            Value         = [string()] "header content"
%%
%%            Res    = term() "result of send_response_to(...)"
%%            Reason = string()
%%
%% @doc     Create a response (with minimal headers) to a request and
%%          send it. Include one or more extra headers specified by
%%          the caller.
%% @end
%%--------------------------------------------------------------------
send_result(RequestHeader, Socket, Body, Status, Reason, ExtraHeaders)
  when is_record(RequestHeader, keylist), (is_record(Socket, sipsocket) orelse Socket == none),
       (is_binary(Body) orelse is_list(Body)), is_integer(Status), is_list(Reason), is_list(ExtraHeaders) ->
    Response1 = #response{status = Status,
			  reason = Reason,
			  header = siprequest:standardcopy(RequestHeader, ExtraHeaders)
			 },
    Response = siprequest:set_response_body(Response1, Body),
    send_response(Socket, Response).

%%--------------------------------------------------------------------
%% @spec    (LogTag, Request, YxaCtx) ->
%%            Res
%%
%%            LogTag  = string() "only used on error"
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%%            Res    = term()    |
%%            ignore          |
%%            {error, Reason}
%%            Res    = term() "result of transportlayer:send_proxy_request()"
%%            Reason = string()
%%
%% @doc     Proxy a request statelessly. We do that sometimes, for
%%          example ACK 'requests' of to 2xx response to INVITE.
%% @end
%%--------------------------------------------------------------------
stateless_proxy_ack(LogTag, #request{method = "ACK"} = Request, YxaCtx) when is_list(LogTag),
									     is_record(YxaCtx, yxa_ctx) ->
    logger:log(debug, "~s: Checking if Request-URI of ACK received in core is a GRUU",
	       [LogTag]),

    %% Must check if Request-URI was a GRUU
    Res =
	case local:is_gruu_url(Request#request.uri) of
	    {true, GRUU} ->
		case local:lookupuser_gruu(Request#request.uri, GRUU) of
		    {ok, _User, {proxy, NewURL}, _Contact} ->
			Request#request{uri = NewURL};
		    {ok, _User, {proxy, NewURL}} when is_record(NewURL, sipurl) ->
			Request#request{uri = NewURL};
		    {ok, _User, {response, Status1, Reason1}} ->
			{response, Status1, Reason1};
		    {ok, _User, {proxy, {with_path, NewURL, Path}}} ->
			%% RFC3327
			NewHeader = keylist:prepend({"Route", Path}, Request#request.header),
			Request#request{header = NewHeader,
					uri    = NewURL
				       }
		end;
	    false ->
		Request
	end,

    case Res of
	NewRequest when is_record(NewRequest, request) ->
	    logger:log(normal, "~s: ~s -> Forwarding ACK received in core statelessly",
		       [LogTag, YxaCtx#yxa_ctx.logstr]),
	    stateless_proxy_request(LogTag, NewRequest);
	{response, Status, Reason} ->
	    logger:log(normal, "~s: ~s -> Dropping ACK to GRUU '~s' : '~p ~s'",
		       [LogTag, sipurl:print(Request#request.uri), Status, Reason]),
	    ignore
    end.

%%--------------------------------------------------------------------
%% @spec    (LogTag, Request) ->
%%            Res
%%
%%            LogTag  = string() "only used on error"
%%            Request = #request{}
%%
%%            Res    = term() |
%%            {error, Reason}
%%            Res    = term() "result of transportlayer:send_proxy_request()"
%%            Reason = string()
%%
%% @doc     Proxy a request statelessly. We do that sometimes, for
%%          example ACK 'requests' of to 2xx response to INVITE.
%% @end
%%--------------------------------------------------------------------
stateless_proxy_request(LogTag, Request) when is_list(LogTag), is_record(Request, request) ->
    case siprequest:stateless_route_proxy_request(Request) of
	{ok, Dst, NewRequest} when is_record(Dst, sipdst) ->
	    stateless_proxy_request2(LogTag, NewRequest, [Dst]);
	{ok, DstList, NewRequest} when is_list(DstList) ->
	    stateless_proxy_request2(LogTag, NewRequest, DstList);
	{error, Reason} ->
	    logger:log(error, "~s: Could not get destination for stateless request : ~p", [LogTag, Reason]),
	    {error, Reason}
    end.

%% stateless_proxy_request2/3 - part of stateless_proxy_request/2
%% Returns : ok | {error, Reason}
stateless_proxy_request2(LogTag, Request, [Dst | Rest]) when is_record(Dst, sipdst) ->
    case transportlayer:is_eligible_dst(Dst) of
	true ->
	    case send_proxy_request(none, Request, Dst, []) of
		{ok, _SendSocket, _BranchUsed} ->
		    logger:log(normal, "~s: Proxied stateless request '~s ~s' to ~p:~s:~p",
			       [LogTag, Request#request.method, sipurl:print(Request#request.uri),
				Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port]),
		    ok;
		{error, Reason} ->
		    logger:log(normal, "~s: FAILED proxying stateless request '~s ~s' to ~p:~s:~p : ~p",
			       [LogTag, Request#request.method, sipurl:print(Request#request.uri),
				Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port, Reason]),
		    case Rest of
			[] ->
			    {error, Reason};
			_ ->
			    stateless_proxy_request2(LogTag, Request, Rest)
		    end
	    end;
	{false, Reason} ->
	    logger:log(normal, "~s: Not proxying request '~s ~s' to ~p:~s:~p (~s)",
		       [LogTag, Request#request.method, sipurl:print(Request#request.uri),
			Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port, Reason]),
	    stateless_proxy_request2(LogTag, Request, Rest)
    end;
stateless_proxy_request2(LogTag, Request, []) ->
    logger:log(normal, "~s: FAILED proxying stateless request '~s ~s' : All destinations unreachable",
	       [LogTag, Request#request.method, sipurl:print(Request#request.uri)]),
    {error, "No reachable destination"}.

%%--------------------------------------------------------------------
%% @spec    (Socket, Response) ->
%%            Res                 |
%%            {senderror, Reason}
%%
%%            Socket   = #sipsocket{} | none
%%            Response = #response{}
%%
%%            Res    = term() "result of send_response_to(...)"
%%            Reason = string()
%%
%% @doc     Prepare to send a response out on a socket.
%% @end
%%--------------------------------------------------------------------
send_response(Socket, Response) when is_record(Response, response) ->
    case sipheader:topvia(Response#response.header) of
	none ->
	    #response{status = Status,
		      reason = Reason
		     } = Response,
	    logger:log(error, "Transport layer: Can't send response '~p ~s', no Via left.", [Status, Reason]),
	    {senderror, "malformed response"};
	TopVia when is_record(TopVia, via) ->
	    send_response_to(Socket, Response, TopVia)
    end.

%%--------------------------------------------------------------------
%% @spec    (Socket, Response, TopVia) ->
%%            ok                  |
%%            {senderror, Reason}
%%
%%            Socket   = #sipsocket{} | none
%%            Response = #response{}
%%            Via      = #via{}
%%
%%            Reason = string()
%%
%% @doc     Send a response out on a socket. Note : XXX this function
%%          does not handle errors returned from sub-functions in a
%%          very consistent manner. Should be fixed.
%% @end
%%--------------------------------------------------------------------
send_response_to(DefaultSocket, Response, TopVia) when is_record(Response, response), record(TopVia, via) ->
    #response{status = Status,
	      reason = Reason,
	      header = HeaderIn,
	      body   = Body
	     } = Response,
    Header = siprequest:fix_content_length(HeaderIn, Body),
    BinLine1 = list_to_binary(["SIP/2.0 ", integer_to_list(Status), " ", Reason]),
    BinMsg = siprequest:binary_make_message(BinLine1, Header, Body),
    case sipdst:get_response_destination(TopVia) of
	Dst when is_record(Dst, sipdst) ->
	    case get_good_socket(DefaultSocket, Dst) of
		SendSocket when is_record(SendSocket, sipsocket) ->
		    CPid = SendSocket#sipsocket.pid,
		    SendRes = sipsocket:send(SendSocket, Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port, BinMsg),
		    %% Log the response we just sent out. Log after send to minimize latency. Since we have it
		    %% in binary format already, and list operations on big lists are quite expensive, we avoid
		    %% making it a list that would then be made a binary again by the logger, by using the special
		    %% logger:log_iolist() here.
		    LogPrefix = io_lib:format("(~p bytes, sent to=~s (using ~p)) :",
					      [size(BinMsg), sipdst:dst2str(Dst), CPid]),
		    LogPrefixBin = list_to_binary(["Transport layer: send response", LogPrefix]),
		    logger:log_iolist(debug, [LogPrefixBin, 10, BinMsg, 10]),
		    case SendRes of
			ok ->
			    ok;
			{error, E} ->
			    logger:log(error, "Transport layer: Failed sending response to ~s using socket ~p, "
				       "error ~p", [sipdst:dst2str(Dst), SendSocket, E]),
			    {senderror, E}
		    end;
		{error, E} ->
		    logger:log(error, "Transport layer: Failed to get socket to send response to ~s, error ~p",
			       [sipdst:dst2str(Dst), E]),
		    logger:log(debug, "Transport layer: Failed to get socket to send response to ~s, error ~p, "
			       "response :~n~s~n", [sipdst:dst2str(Dst), E, binary_to_list(BinMsg)]),
		    {senderror, "Could not get socket"}
	    end;
	error ->
	    {senderror, "Failed finding destination"}
    end.


%%--------------------------------------------------------------------
%% @spec    (Dst) -> ok
%%
%%            Dst = #sipdst{}
%%
%% @doc     Interface function to sipsocket_blacklist.
%% @end
%%--------------------------------------------------------------------
remove_blacklisting(Dst) ->
    sipsocket_blacklist:remove_blacklisting(Dst).

%%--------------------------------------------------------------------
%% @spec    (Dst, SipSocket, Msg) -> ok
%%
%%            Dst       = #sipdst{} "the unreachable destination"
%%            SipSocket = #sipsocket{} | none "socket that didn't work"
%%            Msg       = string() "reason for blacklisting"
%%
%% @doc     Interface function to sipsocket_blacklist.
%% @end
%%--------------------------------------------------------------------
report_unreachable(#sipdst{proto = yxa_test} = Dst, _SipSocket, Msg) ->
    self() ! {transportlayer, report_unreachable, Dst, lists:flatten(Msg)},
    ok;
report_unreachable(Dst, SipSocket, Msg) ->
    sipsocket_blacklist:report_unreachable(Dst, SipSocket, Msg).

%%--------------------------------------------------------------------
%% @spec    (Dst, SipSocket, Msg, RetryAfter) -> ok
%%
%%            Dst        = #sipdst{} "the unreachable destination"
%%            SipSocket  = #sipsocket{} | none "socket that didn't work"
%%            Msg        = string() "reason for blacklisting"
%%            RetryAfter = undefined | integer() "seconds to blacklist"
%%
%% @doc     Interface function to sipsocket_blacklist.
%% @end
%%--------------------------------------------------------------------
report_unreachable(#sipdst{proto = yxa_test} = Dst, _SipSocket, Msg, RetryAfter) ->
    self() ! {transportlayer, report_unreachable, Dst, lists:flatten(Msg), RetryAfter},
    ok;
report_unreachable(Dst, SipSocket, Msg, RetryAfter) ->
    sipsocket_blacklist:report_unreachable(Dst, SipSocket, Msg, RetryAfter).

%%--------------------------------------------------------------------
%% @spec    (Dst) ->
%%            true | {false, Reason}
%%
%%            Dst = #sipdst{}
%%
%%            Reason = string()
%%
%% @doc     Check if the transport layer thinks a destination is
%%          eligible. Currently we only check if it is blacklisted,
%%          but we could for example check if the protocol is
%%          supported, if we change the resolving functions to
%%          support more types of destinations than the transport
%%          layer.
%% @end
%%--------------------------------------------------------------------
is_eligible_dst(Dst) when is_record(Dst, sipdst) ->
    case sipsocket_blacklist:is_blacklisted(Dst) of
	true ->
	    {false, "blacklisted"};
	false ->
	    true
    end.

%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    (DefaultSocket, Dst) ->
%%            Socket          |
%%            {error, Reason}
%%
%%            DefaultSocket = #sipsocket{} | none
%%            Dst           = #sipdst{}
%%
%%            Socket = #sipsocket{}
%%            Reason = string()
%%
%% @doc     Check if DefaultSocket is a working socket, and is of the
%%          protocol requested (SendProto). If so, return the
%%          DefaultSocket - otherwise go bother the transport layer
%%          and try to get a socket useable to communicate with
%%          SendToHost on port Port using protocol SendProto.
%% @end
%%--------------------------------------------------------------------
%%
%% Default socket provided, check that it is still valid
%%
get_good_socket(#sipsocket{proto = Proto} = DefaultSocket, #sipdst{proto = Proto} = Dst) ->
    case sipsocket:is_good_socket(DefaultSocket) of
	true ->
	    logger:log(debug, "Transport layer: Using default socket ~p", [DefaultSocket]),
	    DefaultSocket;
	false ->
	    logger:log(debug, "Transport layer: No good socket (~p) provided - asking sipsocket "
		       "module for a ~p socket", [DefaultSocket, Proto]),
	    get_good_socket(none, Dst)
    end;
%%
%% Default socket has other protocol than the requested one
%%
get_good_socket(#sipsocket{proto = Proto}, #sipdst{proto = OtherProto}) ->
    %% This happens for example when we receive a request with the wrong protocol in the top Via
    logger:log(debug, "Transport layer: Socket of protocol '~p' requested, different from default socket given (~p)",
	       [Proto, OtherProto]),
    Msg = io_lib:format("Default socket has wrong protocol (~p instead of ~p)", [Proto, OtherProto]),
    {error, lists:flatten(Msg)};
%%
%% No default socket provided, but one is stuffed into Dst
%%
get_good_socket(none, #sipdst{socket = Socket}) when is_record(Socket, sipsocket) ->
    case sipsocket:is_good_socket(Socket) of
	true ->
	    Socket;
	false ->
	    logger:log(debug, "Transport layer: Socket ~p is not valid anymore", [Socket]),
	    {error, "Specific socket not valid anymore"}
    end;

%%
%% No default socket provided, Dst#sipdst.socket is undefined
%%
get_good_socket(none, Dst) when is_record(Dst, sipdst) ->
    case sipsocket:get_socket(Dst) of
	S when is_record(S, sipsocket) ->
	    #sipdst{proto = Proto,
		    addr  = Host,
		    port  = Port
		   } = Dst,
	    logger:log(debug, "Transport layer: Extra debug: Get socket ~p:~p:~p returned socket ~p",
		       [Proto, Host, Port, S]),
	    S;
	{error, Reason} ->
	    {error, Reason};
	none ->
	    {error, "Failed getting a socket"}
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
    MyHostname = siprequest:myhostname(),
    MyPort = sipsocket:default_port(yxa_test, none),
    Me = lists:concat([MyHostname, ":", MyPort]),

    %% send_proxy_response(Socket, Response)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "send_proxy_response/2 - 0"),
    SPResponse_Res1 = #response{status = 100,
				reason = "Trying",
				header = keylist:from_list([{"Via",  ["SIP/2.0/YXA-TEST " ++ Me,
								      "SIP/2.0/YXA-TEST 192.0.2.2"]},
							    {"From", ["sip:ft@example.org;tag=f-tag"]},
							    {"To",   ["sip:ft@example.org;tag=t-tag"]},
							    {"CSeq", ["4711 INVITE"]}
							   ]),
				body   = <<>>
			       },

    autotest:mark(?LINE, "send_proxy_response/2 - 1.1"),
    %% test normal case with a single Via besides mine
    ok = send_proxy_response(none, SPResponse_Res1),

    autotest:mark(?LINE, "send_proxy_response/2 - 1.2"),
    %% now check process mailbox to extract the SIP message 'sent'
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.2", 6050}, SPRes_Msg1} ->
	    %%io:format("SENT :~n~s~n", [binary_to_list(SPRes_Msg1)]),
	    %% parse message that was 'sent'
	    autotest:mark(?LINE, "send_proxy_response/2 - 1.3"),
	    SPResponse_Res1_1 = sippacket:parse(SPRes_Msg1, none),
	    true = is_record(SPResponse_Res1_1, response),

	    autotest:mark(?LINE, "send_proxy_response/2 - 1.4"),
	    #response{status = 100,
		      reason = "Trying",
		      body   = <<>>
		     } = SPResponse_Res1_1,

	    autotest:mark(?LINE, "send_proxy_response/2 - 1.5"),
	    %% check that from, to and cseq are the same
	    lists:map(fun(N) ->
			      New = keylist:fetch(N, SPResponse_Res1_1#response.header),
			      Old = keylist:fetch(N, SPResponse_Res1#response.header),
			      {N, New} = {N, Old}
		      end, ['from', 'to', 'cseq']),

	    autotest:mark(?LINE, "send_proxy_response/2 - 1.6"),
	    %% check that Content-Length have been set correctly
	    ["0"] = keylist:fetch('content-length', SPResponse_Res1_1#response.header),

	    autotest:mark(?LINE, "send_proxy_response/2 - 1.7"),
	    %% check that the top Via header has been removed
	    [#via{proto = "SIP/2.0/YXA-TEST",
		  host  = "192.0.2.2"}] = sipheader:via(SPResponse_Res1_1#response.header),

	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,

    autotest:mark(?LINE, "send_proxy_response/2 - 2.0"),
    %% test with only one Via - should be rejected
    SPResponse_H2 = SPResponse_Res1#response.header,
    SPResponse_H2_1 = keylist:set("Via", ["SIP/2.0/YXA-TEST " ++ Me], SPResponse_H2),
    SPResponse_Res2 = SPResponse_Res1#response{header = SPResponse_H2_1},

    autotest:mark(?LINE, "send_proxy_response/2 - 2.1"),
    {error, invalid_Via} = send_proxy_response(none, SPResponse_Res2),



    %% send_proxy_request(Socket, Request, Dst, ViaParameters)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "send_proxy_request/4 - 0"),
    SPRequest_URI1 = sipurl:parse("sip:test@example.org"),
    SPRequest_Req1 = #request{method = "OPTIONS",
			uri    = SPRequest_URI1,
			header = keylist:from_list([{"Via",  ["SIP/2.0/YXA-TEST sip.example.org"]},
						    {"From", ["sip:ft@example.org;tag=f-tag"]},
						    {"To",   ["sip:ft@example.org"]},
						    {"CSeq", ["1 OPTIONS"]}
						   ]),
			body   = <<"testing">>
		       },
    SPRequest_Dst1 = #sipdst{proto = yxa_test,
		       addr  = "192.0.2.1",
		       port  = 6050,
		       uri   = SPRequest_URI1
		      },

    autotest:mark(?LINE, "send_proxy_request/4 - 1.1"),
    %% test normal case
    {ok, #sipsocket{proto = yxa_test}, "z9hG4bK-yxa-" ++ _} = send_proxy_request(none, SPRequest_Req1, SPRequest_Dst1, []),

    autotest:mark(?LINE, "send_proxy_request/4 - 1.2"),
    %% now check process mailbox to extract the SIP message 'sent'
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.1", 6050}, SPRequest_Msg1} ->
	    %%io:format("SENT :~n~s~n", [binary_to_list(SPRequest_Msg1)]),
	    %% parse message that was 'sent'
	    autotest:mark(?LINE, "send_proxy_request/4 - 1.3"),
	    SPRequest_Req1_1 = sippacket:parse(SPRequest_Msg1, none),
	    true = is_record(SPRequest_Req1_1, request),

	    autotest:mark(?LINE, "send_proxy_request/4 - 1.4"),
	    #request{method = "OPTIONS",
		     uri    = SPRequest_URI1,
		     body   = <<"testing">>
		    } = SPRequest_Req1_1,

	    autotest:mark(?LINE, "send_proxy_request/4 - 1.5"),
	    %% check that from, to and cseq are the same
	    lists:map(fun(N) ->
			      New = keylist:fetch(N, SPRequest_Req1_1#request.header),
			      Old = keylist:fetch(N, SPRequest_Req1#request.header),
			      {N, New} = {N, Old}
		      end, ['from', 'to', 'cseq']),

	    autotest:mark(?LINE, "send_proxy_request/4 - 1.6"),
	    %% check that Content-Length have been set correctly
	    ["7"] = keylist:fetch('content-length', SPRequest_Req1_1#request.header),

	    autotest:mark(?LINE, "send_proxy_request/4 - 1.7"),
	    %% check that there are now two YXA-TEST via headers and that they are in the
	    %% expected order
	    [#via{proto = "SIP/2.0/YXA-TEST",
		  port  = 6050},
	     #via{proto = "SIP/2.0/YXA-TEST",
		 host   = "sip.example.org"}
	    ] = sipheader:via(SPRequest_Req1_1#request.header),

	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,


    autotest:mark(?LINE, "send_proxy_request/4 - 2.1"),
    %% test send error
    SPRequest_2_Res = {error, {test, make_ref()}},
    autotest:store_unit_test_result(?MODULE, {sipsocket_test, send_result}, SPRequest_2_Res),
    SPRequest_2_Res = send_proxy_request(none, SPRequest_Req1, SPRequest_Dst1, []),
    autotest:mark(?LINE, "send_proxy_request/4 - 2.2"),
    %% clean up
    autotest:clear_unit_test_result(?MODULE, {sipsocket_test, send_result}),
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.1", 6050}, _SPRequest_Msg2} ->
	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,


    autotest:mark(?LINE, "send_proxy_request/4 - 3.1"),
    %% test get socket error
    SPRequest_3_Res = {error, {test, make_ref()}},
    autotest:store_unit_test_result(?MODULE, {sipsocket_test, get_socket}, SPRequest_3_Res),
    SPRequest_3_Res = send_proxy_request(none, SPRequest_Req1, SPRequest_Dst1, []),
    autotest:mark(?LINE, "send_proxy_request/4 - 3.2"),
    %% clean up
    autotest:clear_unit_test_result(?MODULE, {sipsocket_test, get_socket}),


    %% send_result(RequestHeader, Socket, Body, Status, Reason)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "send_result/5 - 0"),
    SResult_H1 = keylist:from_list([{"Via",     ["SIP/2.0/YXA-TEST sip.example.org;received=192.0.2.4"]},
				    {"From",    ["sip:ft@example.org;tag=f-tag"]},
				    {"To",      ["sip:ft@example.org"]},
				    {"Call-Id", ["foo-4711"]},
				    {"CSeq",    ["1 OPTIONS"]}
				   ]),

    autotest:mark(?LINE, "send_result/5 - 1.1"),
    %% test normal case
    ok = send_result(SResult_H1, none, <<"response-body">>, 100, "Trying"),

    autotest:mark(?LINE, "send_result/5 - 1.2"),
    %% now check process mailbox to extract the SIP message 'sent'
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.4", 6050}, SPResponse_Msg1} ->
	    %%io:format("SENT :~n~s~n", [binary_to_list(SPResponse_Msg1)]),
	    %% parse message that was 'sent'
	    autotest:mark(?LINE, "send_result/5 - 1.3"),
	    SResult_Res1_1 = sippacket:parse(SPResponse_Msg1, none),
	    true = is_record(SResult_Res1_1, response),

	    autotest:mark(?LINE, "send_result/5 - 1.4"),
	    #response{status = 100,
		      reason = "Trying",
		      body   = <<"response-body">>
		     } = SResult_Res1_1,

	    autotest:mark(?LINE, "send_result/5 - 1.5"),
	    %% check that from, to and cseq are the same
	    lists:map(fun(N) ->
			      New = keylist:fetch(N, SResult_Res1_1#response.header),
			      Old = keylist:fetch(N, SResult_H1),
			      {N, New} = {N, Old}
		      end, ['from', 'to', 'cseq']),

	    autotest:mark(?LINE, "send_result/5 - 1.6"),
	    %% check that Content-Length have been set correctly
	    ["13"] = keylist:fetch('content-length', SResult_Res1_1#response.header),

	    autotest:mark(?LINE, "send_result/5 - 1.7"),
	    %% check the Via header
	    [#via{proto = "SIP/2.0/YXA-TEST",
		  host  = "sip.example.org",
		  port  = none,
		  param = ["received=192.0.2.4"]
		 }
	    ] = sipheader:via(SResult_Res1_1#response.header),

	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,


    %% send_result(RequestHeader, Socket, Body, Status, Reason, ExtraHeaders)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "send_result/6 - 1.1"),
    %% test normal case
    ok = send_result(SResult_H1, none, <<"response-body">>, 100, "Trying",
		     [{"Foo", ["bar", "baz"]}, {"Subject", ["is a test"]}]),

    autotest:mark(?LINE, "send_result/6 - 1.2"),
    %% now check process mailbox to extract the SIP message 'sent'
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.4", 6050}, SPResponse_Msg2} ->
	    %%io:format("SENT :~n~s~n", [binary_to_list(SPResponse_Msg2)]),
	    %% parse message that was 'sent'
	    autotest:mark(?LINE, "send_result/6 - 1.3"),
	    SResult_Res2_1 = sippacket:parse(SPResponse_Msg2, none),

	    autotest:mark(?LINE, "send_result/6 - 1.4"),
	    %% just verify our extra headers, besides that this function is identical
	    %% to the already tested send_result/5
	    ["bar", "baz"] = keylist:fetch("Foo", SResult_Res2_1#response.header),
	    ["is a test"] = keylist:fetch("Subject", SResult_Res2_1#response.header),

	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,


    %% send_response(Socket, Response)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "send_response/2 - 0"),
    SendResponse1 = #response{status = 100,
			      reason = "Trying",
			      header = keylist:from_list([]),
			      body   = <<>>
			     },

    autotest:mark(?LINE, "send_response/2 - 1"),
    %% test only invalid case, other functionality already covered
    {senderror, "malformed response"} = send_response(none, SendResponse1),


    %% send_response_to(DefaultSocket, Response, TopVia)
    %%--------------------------------------------------------------------
    %% test only errors, other functionality already covered

    autotest:mark(?LINE, "send_response_to/3 - 0"),
    SendResponseTo_Response1 = #response{status = 100,
					 reason = "Trying",
					 header = keylist:from_list([]),
					 body   = <<>>
					},
    [SendResponseTo_Via1] = sipheader:via(["SIP/2.0/YXA-TEST 192.0.2.9"]),

    autotest:mark(?LINE, "send_response_to/3 - 1.1"),
    %% test send error
    SendResponseTo_1_Ref = {test, make_ref()},
    autotest:store_unit_test_result(?MODULE, {sipsocket_test, send_result}, {error, SendResponseTo_1_Ref}),
    {senderror, SendResponseTo_1_Ref} = send_response_to(none, SendResponseTo_Response1, SendResponseTo_Via1),
    autotest:mark(?LINE, "send_response_to/3 - 1.2"),
    %% clean up
    autotest:clear_unit_test_result(?MODULE, {sipsocket_test, send_result}),
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.9", 6050}, _SendResponseTo_Msg1} ->
	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,


    autotest:mark(?LINE, "send_response_to/3 - 2.1"),
    %% test get socket error
    autotest:store_unit_test_result(?MODULE, {sipsocket_test, get_socket}, {error, "testing get_socket error"}),
    {senderror, "Could not get socket"} = send_response_to(none, SendResponseTo_Response1, SendResponseTo_Via1),
    autotest:mark(?LINE, "send_response_to/3 - 2.2"),
    %% clean up
    autotest:clear_unit_test_result(?MODULE, {sipsocket_test, get_socket}),


    autotest:mark(?LINE, "send_response_to/3 - 3.0"),
    %% test get destination failure, non-IP Via header without received= information
    [SendResponseTo_Via3] = sipheader:via(["SIP/2.0/YXA-TEST sip.example.org"]),

    autotest:mark(?LINE, "send_response_to/3 - 3.1"),
    {senderror, "Failed finding destination"} = send_response_to(none, SendResponseTo_Response1, SendResponseTo_Via3),


    %% get_good_socket(DefaultSocket, Dst)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_good_socket/2 - 0"),
    GGSocket_Dst1 = #sipdst{proto = yxa_test,
			    addr  = "192.0.2.11",
			    port  = 6050
			   },
    GGSocket_GoodSocket = sipsocket:get_socket(GGSocket_Dst1),
    GGSocket_DeadPid = spawn(fun() -> ok end),
    GGSocket_BadSocket = #sipsocket{proto = yxa_test,
				    pid   = GGSocket_DeadPid
				   },

    %% test normal case
    autotest:mark(?LINE, "get_good_socket/2 - 1"),
    #sipsocket{proto = yxa_test} = get_good_socket(GGSocket_GoodSocket, GGSocket_Dst1),

    %% test invalid socket
    autotest:mark(?LINE, "get_good_socket/2 - 2"),
    #sipsocket{proto = yxa_test} = get_good_socket(GGSocket_BadSocket, GGSocket_Dst1),

    %% test get socket error
    autotest:mark(?LINE, "get_good_socket/2 - 3"),
    autotest:store_unit_test_result(?MODULE, {sipsocket_test, get_socket}, {error, "testing get_socket error"}),
    {error, "testing get_socket error"} = get_good_socket(none, GGSocket_Dst1),
    %% clean up
    autotest:clear_unit_test_result(?MODULE, {sipsocket_test, get_socket}),

    %% test get socket failure
    autotest:mark(?LINE, "get_good_socket/2 - 4"),
    autotest:store_unit_test_result(?MODULE, {sipsocket_test, get_socket}, none),
    {error, "Failed getting a socket"} = get_good_socket(none, GGSocket_Dst1),
    %% clean up
    autotest:clear_unit_test_result(?MODULE, {sipsocket_test, get_socket}),

    ok.
