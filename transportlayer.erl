%%%-------------------------------------------------------------------
%%% File    : transportlayer
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Transport layer functions. Because of legacy reasons,
%%%           the actual code is in siprequest.erl but the sending
%%%           code will be moved here RSN.
%%%
%%% Created : 01 Mar 2004 by Fredrik Thulin <ft@it.su.se>
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
	 stateless_proxy_request/2
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

start_link() ->
    sipsocket:start_link().

%%--------------------------------------------------------------------
%% Function: send_proxy_response(Socket, Response)
%%           Socket   = sipsocket record() | none
%%           Response = response record()
%% Descrip.: Extract the top Via from Response, and send this response
%%           to that location (destination).
%% Returns : {error, invalid_Via} |
%%           SendResult
%%           SendResult = term(), result of send_response()
%%--------------------------------------------------------------------
send_proxy_response(Socket, Response)
  when is_record(Socket, sipsocket); Socket == none, is_record(Response, response) ->
    case sipheader:via(Response#response.header) of
	[_Self] ->
	    logger:log(error,
		       "Can't proxy response ~p ~s because it contains just one or less Via and that should be mine!",
		       [Response#response.status, Response#response.reason]),
	    {error, invalid_Via};
	[_Self | Via] ->
	    %% Remove Via matching me (XXX should check that it does)
	    NewHeader = keylist:set("Via", sipheader:via_print(Via), Response#response.header),
	    %% Now look for the correct "server transaction" to use when sending this response upstreams
	    [_NextVia | _] = Via,
	    NewResponse = Response#response{header=NewHeader},
	    send_response(Socket, NewResponse)
    end.

%%--------------------------------------------------------------------
%% Function: send_proxy_request(Socket, Request, Dst, ViaParameters)
%%           SrvTHandler   = thandler record() | none
%%           Request       = request record()
%%           Dst           = sipdst record()
%%           ViaParameters = list() of {key, value} tuple()
%% Descrip.: Prepare for proxying a request. The preparation process
%%           is basically to get us a list() of sipdst records and
%%           then calling send_to_available_dst().
%% Returns : throw()                     |
%%           {error, Reason}             |
%%           {ok, SipSocket, UsedBranch} |
%%           Reason     = string()
%%           SipSocket  = sipsocket record(), the socket used
%%           UsedBranch = string(), the complete branch finally used
%% Note    : To use this function, you should already have called
%%           check_proxy_request (directly or indirectly).
%%--------------------------------------------------------------------
send_proxy_request(Socket, Request, Dst, ViaParameters)
  when is_record(Socket, sipsocket); Socket == none, is_record(Request, request), is_record(Dst, sipdst) ->
    IP = Dst#sipdst.addr,
    Port = Dst#sipdst.port,
    Proto = Dst#sipdst.proto,
    DestStr = sipdst:dst2str(Dst),
    case get_good_socket(Socket, Proto, IP, Port) of
	{error, What} ->
	    logger:log(debug, "Siprequest (transport layer) : Failed to get ~p socket for ~s : ~p",
		       [Proto, DestStr, What]),
	    {error, What};
	SipSocket when is_record(SipSocket, sipsocket) ->
	    {Method, OrigURI, Header, Body} = {Request#request.method, Request#request.uri,
					       Request#request.header, Request#request.body},
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
		    LogPrefix1 = <<"Siprequest (transport layer) : sent request(">>,
		    LogPrefix2 = io_lib:format("~p bytes, dst=~s) :", [size(BinMsg), DestStr]),
		    logger:log_iolist(debug, [LogPrefix1, LogPrefix2, 10, BinMsg, 10]),
		    TopVia = sipheader:topvia(NewHeader2),
		    UsedBranch = lists:flatten(sipheader:get_via_branch_full(TopVia)),
		    {ok, SipSocket, UsedBranch};
		{error, E} ->
		    %% Don't log the actual request when we fail, since that would fill up our logs
		    %% with requests failed for one destination (TCP for example) and then succeeding
		    %% for another (UDP for example).
		    logger:log(debug, "Siprequest (transport layer) : Failed sending request (~p bytes) to ~p:~s:~p, error ~p",
			       [size(BinMsg), Proto, IP, Port, E]),
		    {error, E}
	    end
    end.

send_result(RequestHeader, Socket, Body, Status, Reason)
  when is_record(RequestHeader, keylist), is_record(Socket, sipsocket); Socket == none,
       is_binary(Body); is_list(Body), is_integer(Status), is_list(Reason) ->
    Response1 = #response{status=Status, reason=Reason,
			  header=siprequest:standardcopy(RequestHeader, [])},
    Response = siprequest:set_response_body(Response1, Body),
    send_response(Socket, Response).

send_result(RequestHeader, Socket, Body, Status, Reason, ExtraHeaders)
  when is_record(RequestHeader, keylist), is_record(Socket, sipsocket); Socket == none,
       is_binary(Body); is_list(Body), is_integer(Status), is_list(Reason), is_record(ExtraHeaders, keylist) ->
    Response1 = #response{status=Status, reason=Reason,
			  header=siprequest:standardcopy(RequestHeader, ExtraHeaders)},
    Response = siprequest:set_response_body(Response1, Body),
    send_response(Socket, Response).

%%--------------------------------------------------------------------
%% Function: stateless_proxy_request(LogTag, Request)
%%           LogTag  = string(), only used on error
%%           Request = request record()
%% Descrip.: Proxy a request statelessly. We do that sometimes, for
%%           example ACK 'requests' of to 2xx response to INVITE.
%% Returns : Res = term() |
%%           {error, Reason}
%%           Res    = result of transportlayer:send_proxy_request()
%%           Reason = string()
%%--------------------------------------------------------------------
stateless_proxy_request(LogTag, Request) when is_list(LogTag), is_record(Request, request) ->
    case siprequest:stateless_route_proxy_request(Request) of
	{ok, Dst, NewRequest} ->
	    transportlayer:send_proxy_request(none, NewRequest, Dst, []);
	{error, Reason} ->
	    logger:log(error, "~s: Could not get destination for stateless request : ~p", [LogTag, Reason]),
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: send_response(Socket, Response)
%%           Socket   = sipsocket record()
%%           Response = response record()
%% Descrip.: Prepare to send a response out on a socket.
%% Returns : Res = term(), result of send_response_to(...) |
%%           {senderror, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
send_response(Socket, Response) when is_record(Response, response) ->
    {Status, Reason, Header} = {Response#response.status, Response#response.reason,
				Response#response.header},
    case sipheader:topvia(Header) of
	none ->
	    logger:log(error, "Transport layer: Can't send response '~p ~s', no Via left.", [Status, Reason]),
	    {senderror, "malformed response"};
	TopVia when is_record(TopVia, via) ->
	    send_response_to(Socket, Response, TopVia)
    end.

%%--------------------------------------------------------------------
%% Function: send_response_to(Socket, Response, TopVia)
%%           Socket   = sipsocket record() | none
%%           Response = response record()
%%           Via      = via record()
%% Descrip.: Send a response out on a socket.
%% Returns : ok                  |
%%           {senderror, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
send_response_to(DefaultSocket, Response, TopVia) when is_record(Response, response), record(TopVia, via) ->
    {Status, Reason, HeaderIn, Body} = {Response#response.status, Response#response.reason,
					Response#response.header, Response#response.body},
    Header = siprequest:fix_content_length(HeaderIn, Body),
    BinLine1 = list_to_binary(["SIP/2.0 ", integer_to_list(Status), " ", Reason]),
    BinMsg = siprequest:binary_make_message(BinLine1, Header, Body),
    case sipdst:get_response_destination(TopVia) of
	Dst when is_record(Dst, sipdst) ->
	    case get_good_socket(DefaultSocket, Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port) of
		SendSocket when is_record(SendSocket, sipsocket) ->
		    CPid = SendSocket#sipsocket.pid,
		    SendRes = sipsocket:send(SendSocket, Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port, BinMsg),
		    %% Log the response we just sent out. Log after send to minimize latency. Since we have it
		    %% in binary format already, and list operations on big lists are quite expensive, we avoid
		    %% making it a list that would then be made a binary again by the logger, by using the special
		    %% logger:log_iolist() here.
		    LogPrefix = io_lib:format("send response(~p bytes, sent to=~s (using ~p)) :",
					      [size(BinMsg), sipdst:dst2str(Dst), CPid]),
		    LogPrefixBin = list_to_binary([LogPrefix]),
		    logger:log_iolist(debug, [LogPrefixBin, 10, BinMsg, 10]),
		    case SendRes of
			ok ->
			    ok;
			{error, E} ->
			    logger:log(error, "Failed sending response to ~s using socket ~p, error ~p",
				       [sipdst:dst2str(Dst), SendSocket, E]),
			    {senderror, E}
		    end;
		{error, E} ->
		    logger:log(error, "Failed to get socket to send response to ~s, error ~p",
			       [sipdst:dst2str(Dst), E]),
		    logger:log(debug, "Failed to get socket to send response to ~s, error ~p, response :~n~s~n",
			       [sipdst:dst2str(Dst), E, binary_to_list(BinMsg)]),
		    {senderror, "Could not get socket"}
	    end;
	_ ->
	    {senderror, "Failed finding destination"}
    end.

%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: get_good_socket(DefaultSocket, SendProto, SendToHost,
%%                           Port)
%%           DefaultSocket = sipsocket record() | none
%%           SendProto     = atom(), tcp|tcp6|tls|tls6|udp|udp6
%%           SendToHost    = string()
%%           Port          = integer()
%% Descrip.: Check if DefaultSocket is a working socket, and is of the
%%           protocol requested (SendProto). If so, return the
%%           DefaultSocket - otherwise go bother the transport layer
%%           and try to get a socket useable to communicate with
%%           SendToHost on port Port using protocol SendProto.
%% Returns : Socket          |
%%           {error, Reason}
%%           Socket = sipsocket record()
%%           Reason = string()
%%--------------------------------------------------------------------
get_good_socket(DefaultSocket, SendProto, SendToHost, Port) when is_integer(Port) ->
    case sipsocket:is_good_socket(DefaultSocket) of
	true ->
	    case DefaultSocket#sipsocket.proto of
		SendProto ->
		    logger:log(debug, "Siprequest: Using default socket ~p", [DefaultSocket]),
		    DefaultSocket;
		_ ->
		    logger:log(error, "Siprequest: Default socket provided for sending message is protocol ~p, "
			       "but message should be sent over ~p", [DefaultSocket#sipsocket.proto, SendProto]),
		    {error, "Supplied socket has wrong protocol"}
	    end;
	_ ->
	    logger:log(debug, "Siprequest: No good socket (~p) provided - asking transport layer for a ~p socket",
		       [DefaultSocket, SendProto]),
	    SocketModule = sipsocket:proto2module(SendProto),
	    case sipsocket:get_socket(SocketModule, SendProto, SendToHost, Port) of
		{error, E1} ->
		    {error, E1};
		none ->
		    {error, "no socket provided and get_socket() returned 'none'"};
		S when is_record(S, sipsocket) ->
		    logger:log(debug, "Siprequest: Extra debug: Get socket ~p ~p ~p ~p returned socket ~p",
			       [SocketModule, SendProto, SendToHost, Port, S]),
		    S
	    end
    end.

