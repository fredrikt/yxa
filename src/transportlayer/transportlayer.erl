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
	 stateless_proxy_request/2,

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
	    logger:log(error, "Transport layer: Can't proxy response ~p ~s because it contains just one or less Via "
		       "and that should be mine!", [Response#response.status, Response#response.reason]),
	    {error, invalid_Via};
	[_Self | Via] ->
	    %% Remove Via matching me (XXX should check that it does match me)
	    NewHeader = keylist:set("Via", sipheader:via_print(Via), Response#response.header),
	    NewResponse = Response#response{header=NewHeader},
	    send_response(Socket, NewResponse)
    end.

%%--------------------------------------------------------------------
%% Function: send_proxy_request(Socket, Request, Dst, ViaParameters)
%%           Socket        = sipsocket record() | none, socket to use
%%                           for sending this request
%%           Request       = request record()
%%           Dst           = sipdst record()
%%           ViaParameters = list() of {key, value} tuple()
%% Descrip.: Prepare for proxying a request. The preparation process
%%           is basically to get us a list() of sipdst records and
%%           then calling send_to_available_dst().
%% Returns : throw()                     |
%%           {error, Reason}             |
%%           {ok, SipSocket, UsedBranch} |
%%           Reason     = timeout | string() | term()
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
	    case send_proxy_request(none, NewRequest, Dst, []) of
		{ok, _SendSocket, _BranchUsed} ->
		    logger:log(normal, "~s: Proxied stateless request '~s ~s' to ~p:~s:~p",
			       [LogTag, NewRequest#request.method, sipurl:print(NewRequest#request.uri),
				Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port]),
		    ok;
		{error, Reason} ->
		    logger:log(normal, "~s: FAILED proxying stateless request '~s ~s' to ~p:~s:~p : ~p",
			       [LogTag, NewRequest#request.method, sipurl:print(NewRequest#request.uri),
				Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port, Reason]),
		    {error, Reason}
	    end;
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
    case sipheader:topvia(Response#response.header) of
	none ->
	    {Status, Reason} = {Response#response.status, Response#response.reason},
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
%% Note    : XXX this function does not handle errors returned from
%%           sub-functions in a very consistent manner. Should be
%%           fixed.
%%--------------------------------------------------------------------
send_response_to(DefaultSocket, Response, TopVia) when is_record(Response, response), record(TopVia, via) ->
    {Status, Reason, HeaderIn, Body} = {Response#response.status, Response#response.reason,
					Response#response.header, Response#response.body},
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

%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: get_good_socket(DefaultSocket, Dst)
%%           DefaultSocket = sipsocket record() | none
%%           Dst           = sipdst record()
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
%%
%% Default socket provided, check that it is still valid
%%
get_good_socket(#sipsocket{proto=Proto}=DefaultSocket, #sipdst{proto=Proto}=Dst) ->
    case sipsocket:is_good_socket(DefaultSocket) of
	true ->
	    logger:log(debug, "Transport layer: Using default socket ~p", [DefaultSocket]),
	    DefaultSocket;
	false ->
	    logger:log(debug, "Transport layer: No good socket (~p) provided - asking transport layer for a ~p socket",
		       [DefaultSocket, Proto]),
	    get_good_socket(none, Dst)
    end;
%%
%% No default socket provided
%%
get_good_socket(none, Dst) when is_record(Dst, sipdst) ->
    case sipsocket:get_socket(Dst) of
	S when is_record(S, sipsocket) ->
	    {Proto, Host, Port} = {Dst#sipdst.proto, Dst#sipdst.addr, Dst#sipdst.port},
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
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->
    MyHostname = siprequest:myhostname(),
    MyPort = sipsocket:default_port(yxa_test, none),
    Me = lists:concat([MyHostname, ":", MyPort]),

    %% send_proxy_response(Socket, Response)
    %%--------------------------------------------------------------------
    io:format("test: send_proxy_response/2 - 0~n"),
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

    io:format("test: send_proxy_response/2 - 1.1~n"),
    %% test normal case with a single Via besides mine
    ok = send_proxy_response(none, SPResponse_Res1),
    
    io:format("test: send_proxy_response/2 - 1.2~n"),
    %% now check process mailbox to extract the SIP message 'sent'
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.2", 6050}, SPRes_Msg1} ->
	    %%io:format("SENT :~n~s~n", [binary_to_list(SPRes_Msg1)]),
	    %% parse message that was 'sent'
	    io:format("test: send_proxy_response/2 - 1.3~n"),
	    SPResponse_Res1_1 = sippacket:parse(SPRes_Msg1, none),
	    true = is_record(SPResponse_Res1_1, response),

	    io:format("test: send_proxy_response/2 - 1.4~n"),
	    #response{status = 100,
		      reason = "Trying",
		      body   = <<>>
		     } = SPResponse_Res1_1,

	    io:format("test: send_proxy_response/2 - 1.5~n"),
	    %% check that from, to and cseq are the same
	    lists:map(fun(N) ->
			      New = keylist:fetch(N, SPResponse_Res1_1#response.header),
			      Old = keylist:fetch(N, SPResponse_Res1#response.header),
			      {N, New} = {N, Old}
		      end, ['from', 'to', 'cseq']),

	    io:format("test: send_proxy_response/2 - 1.6~n"),
	    %% check that Content-Length have been set correctly
	    ["0"] = keylist:fetch('content-length', SPResponse_Res1_1#response.header),
	    
	    io:format("test: send_proxy_response/2 - 1.7~n"),
	    %% check that the top Via header has been removed
	    [#via{proto = "SIP/2.0/YXA-TEST",
		  host  = "192.0.2.2"}] = sipheader:via(SPResponse_Res1_1#response.header),

	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,

    io:format("test: send_proxy_response/2 - 2.0~n"),
    %% test with only one Via - should be rejected
    SPResponse_H2 = SPResponse_Res1#response.header,
    SPResponse_H2_1 = keylist:set("Via", ["SIP/2.0/YXA-TEST " ++ Me], SPResponse_H2),
    SPResponse_Res2 = SPResponse_Res1#response{header = SPResponse_H2_1},

    io:format("test: send_proxy_response/2 - 2.1~n"),
    {error, invalid_Via} = send_proxy_response(none, SPResponse_Res2),
    


    %% send_proxy_request(Socket, Request, Dst, ViaParameters)
    %%--------------------------------------------------------------------
    io:format("test: send_proxy_request/4 - 0~n"),
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

    io:format("test: send_proxy_request/4 - 1.1~n"),
    %% test normal case
    {ok, #sipsocket{proto = yxa_test}, "z9hG4bK-yxa-" ++ _} = send_proxy_request(none, SPRequest_Req1, SPRequest_Dst1, []),

    io:format("test: send_proxy_request/4 - 1.2~n"),
    %% now check process mailbox to extract the SIP message 'sent'
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.1", 6050}, SPRequest_Msg1} ->
	    %%io:format("SENT :~n~s~n", [binary_to_list(SPRequest_Msg1)]),
	    %% parse message that was 'sent'
	    io:format("test: send_proxy_request/4 - 1.3~n"),
	    SPRequest_Req1_1 = sippacket:parse(SPRequest_Msg1, none),
	    true = is_record(SPRequest_Req1_1, request),

	    io:format("test: send_proxy_request/4 - 1.4~n"),
	    #request{method = "OPTIONS",
		     uri    = SPRequest_URI1,
		     body   = <<"testing">>
		    } = SPRequest_Req1_1,

	    io:format("test: send_proxy_request/4 - 1.5~n"),
	    %% check that from, to and cseq are the same
	    lists:map(fun(N) ->
			      New = keylist:fetch(N, SPRequest_Req1_1#request.header),
			      Old = keylist:fetch(N, SPRequest_Req1#request.header),
			      {N, New} = {N, Old}
		      end, ['from', 'to', 'cseq']),

	    io:format("test: send_proxy_request/4 - 1.6~n"),
	    %% check that Content-Length have been set correctly
	    ["7"] = keylist:fetch('content-length', SPRequest_Req1_1#request.header),
	    
	    io:format("test: send_proxy_request/4 - 1.7~n"),
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


    io:format("test: send_proxy_request/4 - 2.1~n"),
    %% test send error
    SPRequest_2_Res = {error, {test, make_ref()}},
    put({sipsocket_test, send_result}, SPRequest_2_Res),
    SPRequest_2_Res = send_proxy_request(none, SPRequest_Req1, SPRequest_Dst1, []),
    io:format("test: send_proxy_request/4 - 2.2~n"),
    %% clean up
    erase({sipsocket_test, send_result}),
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.1", 6050}, _SPRequest_Msg2} ->
	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,


    io:format("test: send_proxy_request/4 - 3.1~n"),
    %% test get socket error
    SPRequest_3_Res = {error, {test, make_ref()}},
    put({sipsocket_test, get_socket}, SPRequest_3_Res),
    SPRequest_3_Res = send_proxy_request(none, SPRequest_Req1, SPRequest_Dst1, []),
    io:format("test: send_proxy_request/4 - 3.2~n"),
    %% clean up
    erase({sipsocket_test, get_socket}),


    %% send_result(RequestHeader, Socket, Body, Status, Reason)
    %%--------------------------------------------------------------------
    io:format("test: send_result/5 - 0~n"),
    SResult_H1 = keylist:from_list([{"Via",     ["SIP/2.0/YXA-TEST sip.example.org;received=192.0.2.4"]},
				    {"From",    ["sip:ft@example.org;tag=f-tag"]},
				    {"To",      ["sip:ft@example.org"]},
				    {"Call-Id", ["foo-4711"]},
				    {"CSeq",    ["1 OPTIONS"]}
				   ]),

    io:format("test: send_result/5 - 1.1~n"),
    %% test normal case
    ok = send_result(SResult_H1, none, <<"response-body">>, 100, "Trying"),

    io:format("test: send_result/5 - 1.2~n"),
    %% now check process mailbox to extract the SIP message 'sent'
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.4", 6050}, SPResponse_Msg1} ->
	    %%io:format("SENT :~n~s~n", [binary_to_list(SPResponse_Msg1)]),
	    %% parse message that was 'sent'
	    io:format("test: send_result/5 - 1.3~n"),
	    SResult_Res1_1 = sippacket:parse(SPResponse_Msg1, none),
	    true = is_record(SResult_Res1_1, response),

	    io:format("test: send_result/5 - 1.4~n"),
	    #response{status = 100,
		      reason = "Trying",
		      body   = <<"response-body">>
		     } = SResult_Res1_1,

	    io:format("test: send_result/5 - 1.5~n"),
	    %% check that from, to and cseq are the same
	    lists:map(fun(N) ->
			      New = keylist:fetch(N, SResult_Res1_1#response.header),
			      Old = keylist:fetch(N, SResult_H1),
			      {N, New} = {N, Old}
		      end, ['from', 'to', 'cseq']),

	    io:format("test: send_result/5 - 1.6~n"),
	    %% check that Content-Length have been set correctly
	    ["13"] = keylist:fetch('content-length', SResult_Res1_1#response.header),
	    
	    io:format("test: send_result/5 - 1.7~n"),
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
    io:format("test: send_result/6 - 1.1~n"),
    %% test normal case
    ok = send_result(SResult_H1, none, <<"response-body">>, 100, "Trying",
		     [{"Foo", ["bar", "baz"]}, {"Subject", ["is a test"]}]),

    io:format("test: send_result/6 - 1.2~n"),
    %% now check process mailbox to extract the SIP message 'sent'
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.4", 6050}, SPResponse_Msg2} ->
	    %%io:format("SENT :~n~s~n", [binary_to_list(SPResponse_Msg2)]),
	    %% parse message that was 'sent'
	    io:format("test: send_result/6 - 1.3~n"),
	    SResult_Res2_1 = sippacket:parse(SPResponse_Msg2, none),

	    io:format("test: send_result/6 - 1.4~n"),
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
    io:format("test: send_response/2 - 0~n"),
    SendResponse1 = #response{status = 100,
			      reason = "Trying",
			      header = keylist:from_list([]),
			      body   = <<>>
			     },
    
    io:format("test: send_response/2 - 1~n"),
    %% test only invalid case, other functionality already covered
    {senderror, "malformed response"} = send_response(none, SendResponse1),


    %% send_response_to(DefaultSocket, Response, TopVia)
    %%--------------------------------------------------------------------
    %% test only errors, other functionality already covered

    io:format("test: send_response_to/3 - 0~n"),
    SendResponseTo_Response1 = #response{status = 100,
					 reason = "Trying",
					 header = keylist:from_list([]),
					 body   = <<>>
					},
    [SendResponseTo_Via1] = sipheader:via(["SIP/2.0/YXA-TEST 192.0.2.9"]),

    io:format("test: send_response_to/3 - 1.1~n"),
    %% test send error
    SendResponseTo_1_Ref = {test, make_ref()},
    put({sipsocket_test, send_result}, {error, SendResponseTo_1_Ref}),
    {senderror, SendResponseTo_1_Ref} = send_response_to(none, SendResponseTo_Response1, SendResponseTo_Via1),
    io:format("test: send_response_to/3 - 1.2~n"),
    %% clean up
    erase({sipsocket_test, send_result}),
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.9", 6050}, _SendResponseTo_Msg1} ->
	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end,


    io:format("test: send_response_to/3 - 2.1~n"),
    %% test get socket error
    put({sipsocket_test, get_socket}, {error, "testing get_socket error"}),
    {senderror, "Could not get socket"} = send_response_to(none, SendResponseTo_Response1, SendResponseTo_Via1),
    io:format("test: send_response_to/3 - 2.2~n"),
    %% clean up
    erase({sipsocket_test, get_socket}),


    io:format("test: send_response_to/3 - 3.0~n"),
    %% test get destination failure, non-IP Via header without received= information
    [SendResponseTo_Via3] = sipheader:via(["SIP/2.0/YXA-TEST sip.example.org"]),

    io:format("test: send_response_to/3 - 3.1~n"),
    {senderror, "Failed finding destination"} = send_response_to(none, SendResponseTo_Response1, SendResponseTo_Via3),


    %% get_good_socket(DefaultSocket, Dst)
    %%--------------------------------------------------------------------
    io:format("test: get_good_socket/2 - 0~n"),
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
    io:format("test: get_good_socket/2 - 1~n"),
    #sipsocket{proto = yxa_test} = get_good_socket(GGSocket_GoodSocket, GGSocket_Dst1),

    %% test invalid socket
    io:format("test: get_good_socket/2 - 2~n"),
    #sipsocket{proto = yxa_test} = get_good_socket(GGSocket_BadSocket, GGSocket_Dst1),

    %% test get socket error
    io:format("test: get_good_socket/2 - 3~n"),
    put({sipsocket_test, get_socket}, {error, "testing get_socket error"}),
    {error, "testing get_socket error"} = get_good_socket(none, GGSocket_Dst1),
    %% clean up
    erase({sipsocket_test, get_socket}),

    %% test get socket failure
    io:format("test: get_good_socket/2 - 4~n"),
    put({sipsocket_test, get_socket}, none),
    {error, "Failed getting a socket"} = get_good_socket(none, GGSocket_Dst1),
    %% clean up
    erase({sipsocket_test, get_socket}),

    ok.
