%%
%%--------------------------------------------------------------------

-module(sipserver).

%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/2,
	 process/3,
	 get_env/1,
	 get_env/2,
	 make_logstr/2,
	 safe_spawn/2,
	 safe_spawn/3,
	 origin2str/2,
	 get_listenport/1,
	 get_all_listenports/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

-export([
	 safe_spawn_child/2,
	 safe_spawn_child/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start(normal, [AppModule])
%%           AppModule = atom(), name of this Yxa application
%% Descrip.: The big start-function for the Yxa stack. Invoke this
%%           function to make the sun go up, and tell it the name of
%%           your Yxa application (AppModule) to have the stack invoke
%%           the correct init/0, request/3 and response/3 methods.
%% Returns : {ok, Sup}              |
%%           does not return at all
%%           Sup = pid of the Yxa OTP supervisor (module
%%                 sipserver_sup)
%%--------------------------------------------------------------------
start(normal, [AppModule]) ->
    catch ssl:start(),
    %% XXX uhm, is this sufficient? better than ssl:s internal seeding at least
    %% (since it uses a constant number)
    ssl:seed([sipserver:get_env(sipauth_password, ""), util:timestamp()]),
    mnesia:start(),
    [RemoteMnesiaTables, Mode, AppSupdata] = apply(AppModule, init, []),
    case sipserver_sup:start_link(AppModule, Mode, AppSupdata) of
	{ok, Supervisor} ->
	    case siphost:myip() of
		"127.0.0.1" ->
		    logger:log(normal, "NOTICE: siphost:myip() returns 127.0.0.1, it is either "
			       "broken on your platform or you have no interfaces (except loopback) up");
		_ ->
		    true
	    end,
	    case RemoteMnesiaTables of
		none ->
		    %% update old database versions
		    table_update:update(),
		    logger:log(normal, "proxy started, supervisor is ~p", [Supervisor]);
		_ ->
		    DbNodes = case sipserver:get_env(databaseservers, none) of
				  none ->
				      logger:log(error, "Startup: This application needs remote tables ~p but you " ++
						 "haven't configured any databaseservers, exiting.",
						 [RemoteMnesiaTables]),
				      logger:quit(none),
				      erlang:fault("No databaseservers configured");
				  Res ->
				      Res
			      end,
		    logger:log(debug, "Mnesia extra db nodes : ~p", [DbNodes]),
		    case mnesia:change_config(extra_db_nodes,
					      sipserver:get_env(databaseservers)) of
			{error, Reason} ->
			    logger:log(error, "Startup: Could not add configured databaseservers: ~p",
				       [mnesia:error_description(Reason)]),
			    logger:quit(none),
			    erlang:fault("Could not add configured databaseservers");
			_ ->
			    true
		    end,
		    {Message, Args} = find_remote_mnesia_tables(RemoteMnesiaTables, Supervisor),
		    logger:log(normal, Message, Args)
	    end,
	    %% Start the transport layer now that we have initialized everything
	    TransportLayer = {transportlayer,
			      {transportlayer, start, []},
			      permanent, 2000, supervisor, [transportlayer]},
	    case supervisor:start_child(Supervisor, TransportLayer) of
		{error, E} ->
		    logger:log(error, "Sipserver: Failed starting the transport layer : ~p", [E]),
		    %% We must sleep a few seconds here, so that the supervisor does not shut down
		    %% the logger process while it is still logging (yes, that was what happened
		    %% with the result being that the logger process crashed and produced a mis-
		    %% guiding error 'ebadf' from io:format()).
		    timer:sleep(3),
		    {error, E};
		{ok, _} ->
		    {ok, Supervisor};
		{ok, _, _} ->
		    {ok, Supervisor}
	    end;
	Unknown ->
	    E = lists:flatten(io_lib:format("Failed starting supervisor : ~p", [Unknown])),
	    {error, E}
    end.

%%--------------------------------------------------------------------
%% Function: find_remote_mnesia_tables(RemoteMnesiaTables, Supervisor)
%%           RemoteMnesiaTables = list() of atom(), names of remote
%%           mnesia tables needed by this Yxa application.
%% Descrip.: Do mnesia:wait_for_tables() for RemoteMnesiaTables, with
%%           a timeout since we (Stockholm university) have had
%%           intermittent problems with mnesia startups. Try a mnesia
%%           stop/start after 30 seconds, and stop trying by using
%%           erlang:fault() after another 30 seconds.
%% Returns : {LogFormat, LogArgs} | does not return at all
%%           LogFormat = string()
%%           LogArgs = list() of term()
%%
%% XXX are the problems asociated with the somewhat random startup
%% order of the nodes/applications? mnesia:start() is asynchronous
%% or is it the usage of ctrl-c to terminate node when debugging
%% which mnesia might perceive as a network error? - hsten
%%--------------------------------------------------------------------
find_remote_mnesia_tables(RemoteMnesiaTables, Supervisor) ->
    logger:log(debug, "Initializing remote Mnesia tables ~p", [RemoteMnesiaTables]),
    find_remote_mnesia_tables1(Supervisor, RemoteMnesiaTables, RemoteMnesiaTables, 0).

find_remote_mnesia_tables1(Supervisor, OrigTableList, RemoteMnesiaTables, Count) ->
    case mnesia:wait_for_tables(RemoteMnesiaTables, 10000) of
	ok ->
	    {"proxy started, all tables found, supervisor is ~p", [Supervisor]};
	{timeout, BadTabList} ->
	    case Count of
		3 ->
		    logger:log(normal, "Attempting a Mnesia restart because I'm still waiting for tables ~p",
			       [BadTabList]),
		    StopRes = mnesia:stop(),
		    StartRes = mnesia:start(),
		    logger:log(debug, "Mnesia stop() -> ~p, start() -> ~p", [StopRes, StartRes]),
		    find_remote_mnesia_tables1(Supervisor, OrigTableList, OrigTableList, Count + 1);
		6 ->
		    logger:log(error, "Could not initiate remote Mnesia tables ~p, exiting.", [BadTabList]),
		    logger:quit(none),
		    erlang:fault("Mnesia table init error", RemoteMnesiaTables);
		_ ->
		    logger:log(debug, "Still waiting for tables ~p", [BadTabList]),
		    find_remote_mnesia_tables1(Supervisor, OrigTableList, BadTabList, Count + 1)
	    end
    end.

%%--------------------------------------------------------------------
%% Function: safe_spawn(Module, Fun)
%%           safe_spawn(Module, Function, Arguments)
%%           Fun = fun() | {Module, Function}
%%           Module, Function = atom() - names of module and function
%%           Arguments = list(), arguments for Function and
%%           Module:Function
%% Descrip.: run Function or Module:Function with Arguments as
%%           arguments, in a separate thread. Return true if no
%%           exception occured.
%% Returns : true  |
%%           error
%%--------------------------------------------------------------------
safe_spawn(Function, Arguments) ->
    spawn(?MODULE, safe_spawn_child, [Function, Arguments]).

safe_spawn(Module, Function, Arguments) ->
    spawn(?MODULE, safe_spawn_child, [Module, Function, Arguments]).


%%
safe_spawn_child(Function, Arguments) ->
    case catch apply(Function, Arguments) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from ~p :~n~p", [Function, E]),
	    error;
	{siperror, Status, Reason} ->
	    logger:log(error, "Spawned function ~p generated a SIP-error (ignoring) : ~p ~s",
		       [Function, Status, Reason]),
	    error;
	{siperror, Status, Reason, _} ->
	    logger:log(error, "Spawned function ~p generated a SIP-error (ignoring) : ~p ~s",
		       [Function, Status, Reason]),
	    error;
	_ ->
	    true
    end.

%%
safe_spawn_child(Module, Function, Arguments) ->
    case catch apply(Module, Function, Arguments) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from ~p:~p :~n~p", [Module, Function, E]),
	    error;
	{siperror, Status, Reason} ->
	    logger:log(error, "Spawned function ~p:~p generated a SIP-error (ignoring) : ~p ~s",
		       [Module, Function, Status, Reason]),
	    error;
	{siperror, Status, Reason, _} ->
	    logger:log(error, "Spawned function ~p:~p generated a SIP-error (ignoring) : ~p ~s",
		       [Module, Function, Status, Reason]),
	    error;
	_ ->
	    true
    end.

%%--------------------------------------------------------------------
%% Function: my_send_result(Request, Socket, Status, Reason,
%%                          ExtraHeaders)
%%           Request = request record()
%%           Socket  = sipsocket record()
%%           Status  = integer(), SIP response code
%%           Reason  = string(), error description
%%           ExtraHeaders = keylist record()
%% Descrip.: In sipserver we do lots of checking in the dark areas of
%%           transport layer, transaction layer or somewhere in
%%           between. When we detect unparseable requests for example,
%%           we generate an error response in sipserver but special
%%           care must be taken so that we do not generate responses
%%           to malformed ACK's. This function checks that.
%% Returns : ok  |
%%           Res
%%           Res = term(), result of transportlayer:send_result()
%%--------------------------------------------------------------------
my_send_result(Request, Socket, Status, Reason, ExtraHeaders) when record(Request, request) ->
    case Request#request.method of
	"ACK" ->
	    %% Empirical evidence says that it is a really bad idea to send responses to ACK
	    %% (since the response may trigger yet another ACK). Although not very clearly,
	    %% RFC3261 section 17 (Transactions) do say that responses to ACK is not permitted :
	    %% ' The client transaction is also responsible for receiving responses
	    %%   and delivering them to the TU, filtering out any response
	    %%   retransmissions or disallowed responses (such as a response to ACK).'
	    logger:log(normal, "Sipserver: Suppressing application error response ~p ~s in response to ACK ~s",
		       [Status, Reason, sipurl:print(Request#request.uri)]);
	_ ->
	    case transactionlayer:send_response_request(Request, Status, Reason, ExtraHeaders) of
		ok -> ok;
		_ ->
		    logger:log(error, "Sipserver: Failed sending caught error ~p ~s (in response to ~s ~s) " ++
			       "using transaction layer - sending directly on the socket we received the request on",
			       [Status, Reason, Request#request.method, sipurl:print(Request#request.uri)]),
		    transportlayer:send_result(Request#request.header, Socket, "", Status, Reason, ExtraHeaders)
	    end
    end.

%%--------------------------------------------------------------------
%% Function: internal_error(Request, Socket)
%%           Request = request record()
%%           Socket  = sipsocket record()
%% Descrip.: Send a 500 Server Internal Error, or some other given
%%           error, in response to a request (Request) received on a
%%           specific socket (Socket).
%% Returns : ok  |
%%           Res
%%           Res = term(), result of transportlayer:send_result()
%%--------------------------------------------------------------------
internal_error(Request, Socket) when record(Request, request), record(Socket, sipsocket) ->
    my_send_result(Request, Socket, 500, "Server Internal Error", []).

internal_error(Request, Socket, Status, Reason) when record(Request, request), record(Socket, sipsocket) ->
    my_send_result(Request, Socket, Status, Reason, []).

internal_error(Request, Socket, Status, Reason, ExtraHeaders) when record(Request, request),
								   record(Socket, sipsocket) ->
    my_send_result(Request, Socket, Status, Reason, ExtraHeaders).

%%--------------------------------------------------------------------
%% Function: process(Packet, Origin, Dst)
%%           Packet = string()
%%           Origin = siporigin record()
%%           Dst = transport_layer | Module
%% Descrip.: Check if something we received from a socket (Packet) is
%%           a valid SIP request/response by calling parse_packet() on
%%           it. Then, use my_apply to either send it on to the
%%           transaction layer, or invoke a modules request/3 or
%%           response/3 function on it - depending on the contents of
%%           Dst.
%% Returns : void(), does not matter.
%%--------------------------------------------------------------------
process(Packet, Origin, Dst) when record(Origin, siporigin) ->
    SipSocket = Origin#siporigin.sipsocket,
    case parse_packet(Packet, Origin) of
	{Request, LogStr} when record(Request, request) ->
	    case catch my_apply(Dst, Request, Origin, LogStr) of
		{'EXIT', E} ->
		    logger:log(error, "=ERROR REPORT==== from SIP message handler/transaction layer ~n~p", [E]),
		    internal_error(Request, SipSocket);
		{siperror, Status, Reason} ->
		    logger:log(error, "FAILED processing request: ~s -> ~p ~s", [LogStr, Status, Reason]),
		    internal_error(Request, SipSocket, Status, Reason);
		{siperror, Status, Reason, ExtraHeaders} ->
		    logger:log(error, "FAILED processing request: ~s -> ~p ~s", [LogStr, Status, Reason]),
		    internal_error(Request, SipSocket, Status, Reason, ExtraHeaders);
		_ ->
		    true
	    end;
	{Response, LogStr} when record(Response, response) ->
	    my_apply(Dst, Response, Origin, LogStr);
	_ ->
	    true
    end.

%%--------------------------------------------------------------------
%% Function: my_apply(Dst, Request, Origin, LogStr)
%%           Dst = transaction_layer | Module, Module is the name of a
%%                 module that exports a request/3 and a response/3
%%                 function
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogStr  = string(), textual description of request
%% Descrip.: If Dst is transaction_layer, gen_server call the
%%           transaction layer and let it decide our next action. If
%%           Dst is the name of a module, apply() that modules
%%           request/3 function.
%% Returns : true        |
%%           SIPerror    |
%%           ApplyResult
%%           SIPerror = {siperror, Status, Reason}
%%             Status = integer()
%%             Reason = string()
%%           ApplyResult = result of apply()
%%--------------------------------------------------------------------
my_apply(transaction_layer, R, Origin, LogStr) when record(R, request);
						    record(R, response), record(Origin, siporigin) ->
    %% Dst is the transaction layer.
    case transactionlayer:from_transportlayer(R, Origin, LogStr) of
	{continue} ->
	    %% terminate silently
	    true;
	{pass_to_core, AppModule} ->
	    %% Dst (the transaction layer presumably) wants us to apply a function with this
	    %% request/response as argument. This is common when the transaction layer has started
	    %% a new server transaction for this request and wants it passed to the core (or TU)
	    %% but can't do it itself because that would block the transactionlayer process.
	    my_apply(AppModule, R, Origin, LogStr);
	_ ->
	    Type = element(1, R),	%% get record type - 'request' or 'response'
	    logger:log(error, "Sipserver: Got no or unknown response from transaction_layer regarding ~p : ~s",
		       [Type, LogStr]),
	    {siperror, 500, "Server Internal Error"}
    end;
my_apply(AppModule, Request, Origin, LogStr) when atom(AppModule), record(Request, request),
						  record(Origin, siporigin) ->
    apply(AppModule, request, [Request, Origin, LogStr]);
my_apply(AppModule, Response, Origin, LogStr) when atom(AppModule), record(Response, response),
						   record(Origin, siporigin) ->
    apply(AppModule, response, [Response, Origin, LogStr]).

%%--------------------------------------------------------------------
%% Function: parse_packet(Packet, Origin)
%%           Packet = string()
%%           Origin = siporigin record()
%% Descrip.: Check if something we received from a socket (Packet) is
%%           a valid SIP request/response
%% Returns : {Msg, LogStr}          |
%%           void(), unspecified
%%           Msg = request record() |
%%                 response record()
%%           LogStr = string(), textua description of request/response
%%--------------------------------------------------------------------
parse_packet(Packet, Origin) when record(Origin, siporigin) ->
    Socket = Origin#siporigin.sipsocket,
    case catch sippacket:parse(Packet, Origin) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from sippacket:parse()~n~p", [E]),
	    logger:log(error, "CRASHED parsing packet [client=~s]", [origin2str(Origin, "unknown")]),
	    false;
	{siperror, Status, Reason} ->
	    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE",
		       [origin2str(Origin, "unknown"), Status, Reason]),
	    false;
	{siperror, Status, Reason, _ExtraHeaders} ->
	    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE",
		       [origin2str(Origin, "unknown"), Status, Reason]),
	    false;
	keepalive ->
	    true;
	Parsed ->
	    %% From here on, we can generate responses to the UAC on error
	    case catch process_parsed_packet(Parsed, Origin) of
		{'EXIT', E} ->
		    logger:log(error, "=ERROR REPORT==== from sipserver:process_parsed_packet() :~n~p", [E]),
		    {error};
		{sipparseerror, request, Header, Status, Reason} ->
		    logger:log(error, "INVALID request [client=~s] ~p ~s",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    parse_do_internal_error(Header, Socket, Status, Reason, []);
		{sipparseerror, request, Header, Status, Reason, ExtraHeaders} ->
		    logger:log(error, "INVALID request [client=~s]: ~s -> ~p ~s",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    parse_do_internal_error(Header, Socket, Status, Reason, ExtraHeaders);
		{sipparseerror, response, _Header, Status, Reason} ->
		    logger:log(error, "INVALID response [client=~s]: ~s -> ~p ~s (dropping)",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    false;
		{sipparseerror, response, _Header, Status, Reason, _ExtraHeaders} ->
		    logger:log(error, "INVALID response [client=~s]: ~s -> ~p ~s (dropping)",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    false;
		{siperror, Status, Reason} ->
		    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    false;
		{siperror, Status, Reason, _ExtraHeaders} ->
		    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    false;
		Res ->
		    Res
	    end
    end.

%%--------------------------------------------------------------------
%% Function: parse_do_internal_error(Header, Socket, Status, Reason,
%%                                   ExtraHeaders)
%%           Header = term(), opaque (keylist record())
%%           Socket = term(), opaque (sipsocket record())
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           ExtraHeaders = term(), opaque (keylist record())
%% Descrip.: Handle errors returned during initial parsing of a
%%           request. These errors occur before the transaction layer
%%           is notified of the requests, so there are never any
%%           server transactions to handle the errors. Just send them.
%% Returns : ok
%%--------------------------------------------------------------------
parse_do_internal_error(Header, Socket, Status, Reason, ExtraHeaders) ->
    {_, Method} = sipheader:cseq(Header),
    case Method of
	"ACK" ->
	    %% Empirical evidence says that it is a really bad idea to send responses to ACK
	    %% (since the response may trigger yet another ACK). Although not very clearly,
	    %% RFC3261 section 17 (Transactions) do say that responses to ACK is not permitted :
	    %% ' The client transaction is also responsible for receiving responses
	    %%   and delivering them to the TU, filtering out any response
	    %%   retransmissions or disallowed responses (such as a response to ACK).'
	    logger:log(normal, "Sipserver: Suppressing parsing error response ~p ~s because CSeq method is ACK",
		       [Status, Reason]);
	_ ->
	    transportlayer:send_result(Header, Socket, "", Status, Reason, ExtraHeaders)
    end,
    ok.

%%--------------------------------------------------------------------
%% Function: process_parsed_packet(Request, Origin)
%%           Request = request record()
%%           Origin  = siporigin record(), information about where
%%                     this Packet was received from
%% Descrip.: Do alot of transport/transaction layer checking/work on
%%           a request or response we have received and previously
%%           concluded was parseable. For example, do RFC3581 handling
%%           of rport parameter on top via, check for loops, check if
%%           we received a request from a strict router etc.
%% Returns : {NewRequest, LogStr}
%%--------------------------------------------------------------------
process_parsed_packet(Request, Origin) when record(Request, request), record(Origin, siporigin) ->
    NewHeader1 = fix_topvia_received(Request#request.header, Origin),
    NewHeader2 = fix_topvia_rport(NewHeader1, Origin),
    check_packet(Request#request{header=NewHeader2}, Origin),
    {NewURI, NewHeader3} =
	case received_from_strict_router(Request#request.uri, NewHeader2) of
	    true ->
		logger:log(debug, "Sipserver: Received request with a"
			   " Request-URI I (probably) put in a Record-Route. "
			   "Pop real Request-URI from Route-header."),
		ReverseRoute = lists:reverse(sipheader:route(NewHeader2)),
		[FirstRoute | NewReverseRoute] = ReverseRoute,
		NewReqURI = sipurl:parse(FirstRoute#contact.urlstr),
		NewH = 
		    case NewReverseRoute of
			[] ->
			    keylist:delete("Route", NewHeader2);
			_ ->
			    keylist:set("Route", sipheader:contact_print(
						   lists:reverse(NewReverseRoute)), NewHeader2)
		end,
		{NewReqURI, NewH};
	    _ ->
		{Request#request.uri, NewHeader2}
	end,
    NewHeader4 = remove_route_matching_me(NewHeader3),
    NewRequest = Request#request{uri=NewURI, header=NewHeader4},
    LogStr = make_logstr(NewRequest, Origin),
    {NewRequest, LogStr};

%%--------------------------------------------------------------------
%% Function: process_parsed_packet(Response, Origin)
%%           Response = response record()
%%           Origin   = siporigin record(), information about where
%%                      this Packet was received from
%% Descrip.: Do alot of transport/transaction layer checking/work on
%%           a request or response we have received and previously
%%           concluded was parseable. For example, do RFC3581 handling
%%           of rport parameter on top via, check for loops, check if
%%           we received a request from a strict router etc.
%% Returns : {NewResponse, LogStr} |
%%           {invalid}
%%--------------------------------------------------------------------
process_parsed_packet(Response, Origin) when record(Response, response), record(Origin, siporigin) ->
    check_packet(Response, Origin),
    TopVia = sipheader:topvia(Response#response.header),
    case check_response_via(Response, Origin, TopVia) of
	ok ->
	    LogStr = make_logstr(Response, Origin),
	    {Response, LogStr};
	error ->
	    %% Silently drop packet
	    {invalid}
    end.

%%--------------------------------------------------------------------
%% Function: check_response_via(Response, Origin, TopVia)
%%           Response = response record()
%%           Origin   = siporigin record(), information about where
%%                      this Packet was received from
%%           TopVia   = via record() | none
%% Descrip.: Check that there actually was a Via header in this
%%           response, and check if it matches us.
%% Returns : ok    |
%%           error
%%--------------------------------------------------------------------
check_response_via(_Response, Origin, none) ->
    logger:log(error, "INVALID top-Via in response [client=~s] (no Via found).",
	       [origin2str(Origin, "unknown")]),
    error;
check_response_via(Response, Origin, TopVia) when is_record(TopVia, via) ->
    %% Check that top-Via is ours (RFC 3261 18.1.2),
    %% silently drop message if it is not.

    %% Create a Via that looks like the one we would have produced if we sent the request
    %% this is an answer to, but don't include parameters since they might have changed
    Proto = Origin#siporigin.proto,
    %% This is what we expect, considering the protocol in Origin (Proto)
    MyViaNoParam = siprequest:create_via(Proto, []),
    %% But we also accept this, which is the same but with the protocol from this response - in
    %% case we sent the request out using TCP but received the response over UDP for example
    SentByMeNoParam = siprequest:create_via(sipsocket:viaproto2proto(TopVia#via.proto), []),
    case sipheader:via_is_equal(TopVia, MyViaNoParam, [proto, host, port]) of
        true ->
	    ok;
	_ ->
	    case sipheader:via_is_equal(TopVia, SentByMeNoParam, [proto, host, port]) of
		true ->
		    %% This can happen if we for example send a request out on a TCP socket, but the
		    %% other end responds over UDP.
		    logger:log(debug, "Sipserver: Warning: received response [client=~s]"
			       " matching me, but different protocol ~p (received on: ~p)",
			       [origin2str(Origin, "unknown"),
				TopVia#via.proto, sipsocket:proto2viastr(Origin#siporigin.proto)]),
		    LogStr = make_logstr(Response, Origin),
		    {Response, LogStr};
		_ ->
		    logger:log(error, "INVALID top-Via in response [client=~s]."
			       " Top-Via (without parameters) (~s) does not match mine (~s). Discarding.",
			       [origin2str(Origin, "unknown"), sipheader:via_print([TopVia#via{param=[]}]),
				sipheader:via_print([MyViaNoParam])]),
		    error
	    end
    end.

%%--------------------------------------------------------------------
%% Function: fix_topvia_received(Header, Origin)
%%           Header = term(), opaque (keylist record())
%%           Origin = siporigin record()
%% Descrip.: Add received= parameter to top Via of a requests Header
%%           if we need to. RFC 3261 #18.2.1.
%% Returns : NewHeader
%%           NewHeader = term(), opaque (a new keylist record())
%%--------------------------------------------------------------------
fix_topvia_received(Header, Origin) when record(Origin, siporigin) ->
    IP = Origin#siporigin.addr,
    %% Check "sent-by" in top-Via to see if we MUST add a
    %% received= parameter (RFC 3261 18.2.1)
    TopVia = sipheader:topvia(Header),
    case TopVia#via.host of
	IP ->
	    Header;
	_ ->
	    ParamDict = sipheader:param_to_dict(TopVia#via.param),
	    NewDict = dict:store("received", IP, ParamDict),
	    NewVia = TopVia#via{param=sipheader:dict_to_param(NewDict)},
	    logger:log(debug, "Sipserver: TopViaHost ~p does not match IP ~p, appending received=~s parameter",
		       [TopVia#via.host, IP, IP]),
	    replace_top_via(NewVia, Header)
    end.

%%--------------------------------------------------------------------
%% Function: fix_topvia_rport(Header, Origin)
%%           Header = term(), opaque (keylist record())
%%           Origin = siporigin record()
%% Descrip.: Implement handling of rport= top Via parameter upon
%%           receiving a request with an 'rport' parameter. RFC3581.
%% Returns : NewHeader
%%           NewHeader = term(), opaque (a new keylist record())
%%--------------------------------------------------------------------
%% XXX this RFC3581 implementation is not 100% finished. RFC3581 Section 4 says we MUST
%% send the responses to this request back from the same IP and port we received the
%% request to. We should be able to solve this when sending responses if we keep a list
%% of requests and sockets even for requests received over UDP too. XXX make it so.
fix_topvia_rport(Header, Origin) when record(Origin, siporigin) ->
    IP = Origin#siporigin.addr,
    Port = Origin#siporigin.port,
    PortStr = integer_to_list(Port),
    TopVia = sipheader:topvia(Header),
    ParamDict = sipheader:param_to_dict(TopVia#via.param),
    case dict:find("rport", ParamDict) of
	error ->
	    Header;
	{ok, []} ->
	    logger:log(debug, "Sipserver: Client requests symmetric response routing, setting rport=~p", [Port]),
	    NewDict1 = dict:store("rport", PortStr, ParamDict),
	    %% RFC3581 Section 4 says we MUST add a received= parameter when client
	    %% requests rport even if the sent-by is set to the IP-address we received
	    %% the request from.
	    NewDict = dict:store("received", IP, NewDict1),
	    NewVia = TopVia#via{param=sipheader:dict_to_param(NewDict)},
	    replace_top_via(NewVia, Header);
	{ok, PortStr} ->
	    logger:log(debug, "Sipserver: Top Via has rport already set to ~p,"
		       " remote party isn't very RFC3581 compliant.",
		       [Port]),
	    Header;
	{ok, RPort} ->
	    logger:log(error, "Sipserver: Received request with rport already"
		       " containing a value (~p)! Overriding with port ~p.",
		       [RPort, Port]),
	    NewDict = dict:store("rport", PortStr, ParamDict),
	    NewVia = TopVia#via{param=sipheader:dict_to_param(NewDict)},
	    replace_top_via(NewVia, Header)
    end.

%% Replace top Via header in a keylist record()
replace_top_via(NewVia, Header) when record(NewVia, via) ->
    [_FirstVia | Via] = sipheader:via(Header),
    keylist:set("Via", sipheader:via_print(lists:append([NewVia], Via)), Header).

%%--------------------------------------------------------------------
%% Function: received_from_strict_router(URI, Header)
%%           URI = sipurl record()
%%           Header = term(), opaque (keylist record())
%% Descrip.: Look at the URI of a request we just received to see
%%           if it is something we (possibly) put in a Record-Route
%%           and this is a request sent from a strict router
%%           (RFC2543 compliant UA).
%% Returns: true |
%%          false
%%--------------------------------------------------------------------
received_from_strict_router(URI, Header) when record(URI, sipurl) ->
    MyPorts = sipserver:get_all_listenports(),
    MyIP = siphost:myip(),
    HostnameList = lists:append(sipserver:get_env(myhostnames, []), siphost:myip_list()),
    %% If the URI has a username in it, it is not something we've put in a Record-Route
    UsernamePresent = case URI#sipurl.user of
			  none -> false;
			  T when list(T) -> true
		      end,
    HostnameIsMyHostname = util:casegrep(URI#sipurl.host, HostnameList),
    %% In theory, we should not treat an absent port number in this Request-URI as
    %% if the default port number was specified in there, but in practice that is
    %% what we have to do since some UAs can remove the port we put into the
    %% Record-Route
    Port = list_to_integer(siprequest:default_port(URI#sipurl.proto, URI#sipurl.port)),
    PortMatches = lists:member(Port, MyPorts),
    MAddrMatch = case dict:find("maddr", sipheader:param_to_dict(URI#sipurl.param)) of
		     {ok, MyIP} -> true;
		     _ -> false
		 end,
    HeaderHasRoute = case keylist:fetch("Route", Header) of
			 [] -> false;
			 _ -> true
		     end,
    if
	UsernamePresent /= false -> false;
	HostnameIsMyHostname /= true -> false;
	PortMatches /= true -> false;
	%% Some SIP-stacks evidently strip parameters
	%%MAddrMatch /= true -> false;
	HeaderHasRoute /= true ->
	    logger:log(debug, "Sipserver: Warning: Request-URI looks like something"
		       " I put in a Record-Route header, but request has no Route!"),
	    false;
	true -> true
    end.

%%--------------------------------------------------------------------
%% Function: remove_route_matching_me(Header)
%%           Header = term(), opaque (keylist record())
%% Descrip.: Look at the first Route header element in Header (if any)
%%           and see if it matches this proxy. If so, remove the first
%%           element and return a new Header.
%% Returns : NewHeader
%%           NewHeader = term(), opaque (new keylist record(), or the
%%                       same as input if no changes were made)
%%--------------------------------------------------------------------
remove_route_matching_me(Header) ->
    Route = sipheader:route(Header),
    case Route of
        [#contact{urlstr = FirstRoute} | NewRoute] ->
	    case route_matches_me(sipurl:parse(FirstRoute)) of
		true ->
		    logger:log(debug, "Sipserver: First Route ~p matches me, removing it.",
			       [ contact:print(contact:new(none, FirstRoute, [])) ]),
		    NewHeader =
			case NewRoute of
			    [] ->
				keylist:delete("Route", Header);
			    _ ->
				keylist:set("Route", sipheader:contact_print(NewRoute), Header)
			end,
		    NewHeader;
		_ ->
		    Header
	    end;
	_ ->
	    Header
    end.

%%--------------------------------------------------------------------
%% Function: route_matches_me(Route)
%%           Route = sipurl record()
%% Descrip.: Helper function for remove_route_matching_me/1. Check if
%%           an URL matches this proxys name and port.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
route_matches_me(Route) when record(Route, sipurl) ->
    MyPorts = sipserver:get_all_listenports(),
    Port = case siprequest:default_port(Route#sipurl.proto, Route#sipurl.port) of
	       I when integer(I) ->
		   I;
	       L when list(L) ->
		   list_to_integer(L)
	   end,
    PortMatches = lists:member(Port, MyPorts),
    HostnameList = lists:append(get_env(myhostnames, []), siphost:myip_list()),
    HostnameMatches = util:casegrep(Route#sipurl.host, HostnameList),
    if
	HostnameMatches /= true -> false;
	PortMatches /= true -> false;
	true ->	true
    end.

%%--------------------------------------------------------------------
%% Function: check_packet(Packet, Origin)
%%           Packet  = request record() | response record()
%%           Origin  = siporigin record(), information about where
%%                     this Packet was received from
%% Descrip.: Sanity check To: and From: in a received request/response
%%           and, if Packet is a request record(), also check sanity
%%           of CSeq and (unless configured not to) check for a
%%           looping request.
%% Returns : true  |
%%           false |
%%           throw(), {sipparseerror, request, Header, Status, Reason}
%%--------------------------------------------------------------------
%%
%% Packet is request record()
%%
check_packet(Request, Origin) when record(Request, request), record(Origin, siporigin) ->
    {Method, Header} = {Request#request.method, Request#request.header},
    check_supported_uri_scheme(Request#request.uri, Header),
    sanity_check_contact(request, "From", Header),
    sanity_check_contact(request, "To", Header),
    case sipheader:cseq(Header) of
	{unparseable, CSeqStr} ->
	    logger:log(error, "INVALID CSeq ~p in packet from ~s", [CSeqStr, origin2str(Origin, "unknown")]),
	    throw({sipparseerror, request, Header, 400, "Invalid CSeq"});
	{CSeqNum, CSeqMethod} ->
	    case util:isnumeric(CSeqNum) of
		false ->
		    throw({sipparseerror, request, Header, 400, "CSeq number " ++
			   CSeqNum ++ " is not an integer"});
		_ -> true
	    end,
	    if
		CSeqMethod /= Method ->
		    throw({sipparseerror, request, Header, 400, "CSeq Method " ++ CSeqMethod ++
			   " does not match request Method " ++ Method});
		true -> true
	    end;
	_ ->
	    logger:log(error, "INVALID CSeq in packet from ~s", [origin2str(Origin, "unknown")]),
	    throw({sipparseerror, request, Header, 400, "Invalid CSeq"})
    end,
    case sipserver:get_env(detect_loops, true) of
	true ->
	    check_for_loop(Header, Request#request.uri, Origin);
	_ ->
	    true
    end;
%%
%% Packet is response record()
%%
check_packet(Response, Origin) when record(Response, response), record(Origin, siporigin) ->
    sanity_check_contact(response, "From", Response#response.header),
    sanity_check_contact(response, "To", Response#response.header).

%%--------------------------------------------------------------------
%% Function: check_for_loop(Header, URI, Origin)
%%           Header = term(), opaque (keylist record())
%%           URI    = term(), opaque (sipurl record())
%%           Origin  = siporigin record(), information about where
%%                     this Packet was received from
%% Descrip.: Inspect Header's Via: record(s) to make sure this is not
%%           a looping request.
%% Returns : true  |
%%           throw(), {sipparseerror, request, Header, Status, Reason}
%%--------------------------------------------------------------------
check_for_loop(Header, URI, Origin) when record(Origin, siporigin) ->
    LoopCookie = siprequest:get_loop_cookie(Header, URI, Origin#siporigin.proto),
    ViaHostname = siprequest:myhostname(),
    ViaPort = sipserver:get_listenport(Origin#siporigin.proto),
    CmpVia = #via{host=ViaHostname, port=ViaPort},

    case via_indicates_loop(LoopCookie, CmpVia, sipheader:via(Header)) of
	true ->
	    logger:log(debug, "Sipserver: Found a loop when inspecting the Via headers, "
		       "throwing SIP-serror '482 Loop Detected'"),
	    throw({sipparseerror, request, Header, 482, "Loop Detected"});
	_ ->
	    true
    end.

%%--------------------------------------------------------------------
%% Function: via_indicates_loop(LoopCookie, CmpVia, ViaList)
%% Descrip.: Helper function for check_for_loop/3. See that function.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
via_indicates_loop(_, _, []) ->
    false;
via_indicates_loop(LoopCookie, CmpVia, [TopVia | Rest]) when record(TopVia, via)->
    case sipheader:via_is_equal(TopVia, CmpVia, [host, port]) of
	true ->
	    %% Via matches me
	    ParamDict = sipheader:param_to_dict(TopVia#via.param),
	    %% Can't use sipheader:get_via_branch() since it strips the loop cookie
	    case dict:find("branch", ParamDict) of
		error ->
		    %% XXX should broken Via perhaps be considered fatal?
		    logger:log(error, "Sipserver: Request has Via that matches me,"
			       " but no branch parameter. Loop checking broken!"),
		    logger:log(debug, "Sipserver: Via ~p matches me, but has no branch parameter."
			       " Loop checking broken!",
			       sipheader:via_print([TopVia])),
		    via_indicates_loop(LoopCookie, CmpVia, Rest);
		{ok, Branch} ->
		    case lists:suffix("-o" ++ LoopCookie, Branch) of
			true ->
			    true;
			_ ->
			    %% Loop cookie does not match, check next (request might have passed
			    %% this proxy more than once, loop can be further back the via trail)
			    via_indicates_loop(LoopCookie, CmpVia, Rest)
		    end
	    end;
	_ ->
	    %% Via doesn't match me, check next.
	    via_indicates_loop(LoopCookie, CmpVia, Rest)
    end.

%%--------------------------------------------------------------------
%% Function: make_logstr(R, Origin)
%%           R      = request record() | response record()
%%           Origin = siporigin record()
%% Descrip.: Create a textual representation of a request/response,
%%           for use in logging.
%% Returns : LogStr
%%           LogStr = string()
%%--------------------------------------------------------------------
make_logstr(Request, Origin) when record(Request, request) ->
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    {_, FromURI} = sipheader:from(Header),
    {_, ToURI} = sipheader:to(Header),
    ClientStr = origin2str(Origin, "unknown"),
    lists:flatten(io_lib:format("~s ~s [client=~s, from=<~s>, to=<~s>]",
				[Method, sipurl:print(URI), ClientStr, url2str(FromURI), url2str(ToURI)]));
make_logstr(Response, Origin) when record(Response, response) ->
    Header = Response#response.header,
    {_, CSeqMethod} = sipheader:cseq(Header),
    {_, FromURI} = sipheader:from(Header),
    {_, ToURI} = sipheader:to(Header),
    ClientStr = origin2str(Origin, "unknown"),
    case keylist:fetch("Warning", Header) of
	[] ->
	    lists:flatten(io_lib:format("~s [client=~s, from=<~s>, to=<~s>]",
					[CSeqMethod, ClientStr, url2str(FromURI), url2str(ToURI)]));
	[Warning] ->
	    lists:flatten(io_lib:format("~s [client=~s, from=<~s>, to=<~s>, warning=~p]",
					[CSeqMethod, ClientStr, url2str(FromURI), url2str(ToURI), Warning]))
    end.

url2str({unparseable, _}) ->
    "unparseable";
url2str(URL) ->
    sipurl:print(URL).

sanity_check_contact(Type, Name, Header) ->
    case keylist:fetch(Name, Header) of
	[Str] ->
	    case sipheader:from([Str]) of
		{_, URI} when record(URI, sipurl) ->
		    sanity_check_uri(Type, Name ++ ":", URI, Header);
		_ ->
		    throw({sipparseerror, Type, Header, 400, "Invalid " ++ Name ++ ": header"})
	    end;
	_ ->
	    %% Header is either missing, or []
	    throw({sipparseerror, Type, Header, 400, "Missing or invalid " ++ Name ++ ": header"})
    end.

sanity_check_uri(Type, Desc, URI, Header)  when record(URI, sipurl), URI#sipurl.host == none ->
    throw({sipparseerror, Type, Header, 400, "No host part in " ++ Desc ++ " URL"});
sanity_check_uri(_, _, URI, _) when record(URI, sipurl) ->
    ok.

check_supported_uri_scheme({unparseable, URIstr}, Header) ->
    case string:chr(URIstr, $:) of
	0 ->
	    throw({sipparseerror, request, Header, 416, "Unsupported URI Scheme"});
	Index ->
	    Scheme = string:substr(URIstr, 1, Index),
	    throw({sipparseerror, request, Header, 416, "Unsupported URI Scheme (" ++ Scheme ++ ")"})
    end;
check_supported_uri_scheme(URI, _) when record(URI, sipurl) ->
    true.

get_env(Name) ->
    {ok, Value} = application:get_env(Name),
    Value.

get_env(Name, Default) ->
    case application:get_env(Name) of
	{ok, Value} ->
	    Value;
	undefined ->
	    Default
    end.

origin2str(Origin, _) when record(Origin, siporigin) ->
    lists:concat([Origin#siporigin.proto, ":", Origin#siporigin.addr, ":", Origin#siporigin.port]);
origin2str(Str, _) when list(Str) ->
    lists:concat([Str]);
origin2str(_F, Default) ->
    Default.

get_listenport(Proto) when Proto == tls; Proto == tls6 ->
    case sipserver:get_env(tls_listenport, none) of
	P when integer(P) ->
	    P;
	none ->
	    L = siprequest:default_port(Proto, none),
	    list_to_integer(L)
    end;
get_listenport(Proto) ->
    case sipserver:get_env(listenport, none) of
	P when integer(P) ->
	    P;
	none ->
	    L = siprequest:default_port(Proto, none),
	    list_to_integer(L)
    end.

%% In some places, we need to get a list of all ports which are valid for this proxy.
get_all_listenports() ->
    %% XXX implement the rest of this. Have to fetch a list of the ports we listen on
    %% from the transport layer.
    [get_listenport(udp)].

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------

