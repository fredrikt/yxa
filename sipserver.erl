%%
%%--------------------------------------------------------------------

-module(sipserver).

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
%% Function: 
%% Descrip.: 
%% Returns : 
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
%% Function: 
%% Descrip.: 
%% Returns : 
%% 
%% ??? are the problems asociated with the somewhat random startup order of the nodes/applications ?
%% mnesia:start() is asyncronos
%% or is it the usage of ctrl-c to terminate node ? which makes mnesia precieve it as a network
%% error ?
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
%% Descrip.: run Function or Module:Function with Argumnets as 
%%           arguments, in a separate thread. Return true if no 
%%           exception occured.
%% Returns : true | error
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
%% Function: 
%% Descrip.: ???
%% Returns : 
%%--------------------------------------------------------------------
send_result(Request, Socket, Status, Reason, ExtraHeaders) when record(Request, request) ->
    case Request#request.method of
	"ACK" ->
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
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
internal_error(Request, Socket) when record(Request, request), record(Socket, sipsocket) ->
    send_result(Request, Socket, 500, "Server Internal Error", []).

internal_error(Request, Socket, Status, Reason) when record(Request, request), record(Socket, sipsocket) ->
    send_result(Request, Socket, Status, Reason, []).

internal_error(Request, Socket, Status, Reason, ExtraHeaders) when record(Request, request), 
								   record(Socket, sipsocket) ->
    send_result(Request, Socket, Status, Reason, ExtraHeaders).

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
%% Dst is either 'transaction_layer' or the name of a module which exports
%% a request and response function
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
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
my_apply(transaction_layer, R, Origin, LogStr) when record(R, request); 
						    record(R, response), record(Origin, siporigin) ->
    %% Dst is the transaction layer.
    case gen_server:call(transaction_layer, {sipmessage, R, Origin, LogStr}, 2000) of
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
	    Type = case R of
		       _ when record(R, request) -> request;
		       _ -> response
		   end,
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
%% Function: 
%% Descrip.: 
%% Returns : 
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
	{siperror, Status, Reason, ExtraHeaders} ->
	    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE",
		       [origin2str(Origin, "unknown"), Status, Reason]),
	    false;
	keepalive ->
	    true;
	Parsed ->
	    %% From here on, we can generate responses to the UAC on error
	    case catch process_parsed_packet(Socket, Parsed, Origin) of
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
		{sipparseerror, response, Header, Status, Reason} ->
		    logger:log(error, "INVALID response [client=~s]: ~s -> ~p ~s (dropping)",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    false;
		{sipparseerror, response, Header, Status, Reason, ExtraHeaders} ->
		    logger:log(error, "INVALID response [client=~s]: ~s -> ~p ~s (dropping)",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    false;
		{siperror, Status, Reason} ->
		    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    false;
		{siperror, Status, Reason, ExtraHeaders} ->
		    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE",
			       [origin2str(Origin, "unknown"), Status, Reason]),
		    false;
		Res ->
		    Res
	    end
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
parse_do_internal_error(Header, Socket, Status, Reason, ExtraHeaders) ->
    %% Handle errors returned during initial parsing of a request. These errors
    %% occur before the transaction layer is notified of the requests, so there
    %% are never any server transactions to handle the errors. Just send them.
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    case Method of
	"ACK" ->
	    logger:log(normal, "Sipserver: Suppressing parsing error response ~p ~s because CSeq method is ACK",
		       [Status, Reason]);
	_ ->
	    transportlayer:send_result(Header, Socket, "", Status, Reason, ExtraHeaders)
    end,
    ok.

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
%%
%% Process parsed Request
%%
process_parsed_packet(Socket, Request, Origin) when record(Request, request), record(Origin, siporigin) ->
    NewHeader1 = fix_topvia_received(Request#request.header, Origin),
    NewHeader2 = fix_topvia_rport(NewHeader1, Origin),
    check_packet(Request#request{header=NewHeader2}, Origin),
    {{_, NewURI}, NewHeader3} = 
	case received_from_strict_router(Request#request.uri, NewHeader2) of
	    true ->
		logger:log(debug, "Sipserver: Received request with a"
			   " Request-URI I (probably) put in a Record-Route. "
			   "Pop real Request-URI from Route-header."),
		ReverseRoute = lists:reverse(sipheader:contact(
					       keylist:fetch("Route", NewHeader2))),
		[NewReqURI | NewReverseRoute] = ReverseRoute,
		case NewReverseRoute of
		    [] ->
			{NewReqURI, keylist:delete("Route", NewHeader2)};
		    _ ->
			{NewReqURI, keylist:set("Route", sipheader:contact_print(
						  lists:reverse(NewReverseRoute)), NewHeader2)}
					end;
				    _ ->
					{{none, Request#request.uri}, NewHeader2}
				end,
    NewHeader4 = remove_route_matching_me(NewHeader3),
    NewRequest = Request#request{uri=NewURI, header=NewHeader4},
    LogStr = make_logstr(NewRequest, Origin),
    {NewRequest, LogStr};

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
%%
%% Process parsed Response
%%
process_parsed_packet(Socket, Response, Origin) when record(Response, response), record(Origin, siporigin) ->
    check_packet(Response, Origin),
    %% Check that top-Via is ours (RFC 3261 18.1.2),
    %% silently drop message if it is not.
    TopVia = sipheader:topvia(Response#response.header),
    %% Create a Via that looks like the one we would have produced if we sent the request
    %% this is an answer to, but don't include parameters since they might have changed
    Proto = Origin#siporigin.proto,
    %% This is what we expect, considering the protocol in Origin (Proto)
    MyViaNoParam = siprequest:create_via(Proto, []),
    %% But we also accept this, which is the same but with the protocol from this response - in
    %% case we sent the request out using TCP but received the response over UDP for example
    SentByMeNoParam = siprequest:create_via(sipsocket:viaproto2proto(TopVia#via.proto), []),
    TopViaProto = sipsocket:viaproto2proto(TopVia#via.proto),
    case sipheader:via_is_equal(TopVia, MyViaNoParam, [proto, host, port]) of
        true ->
	    LogStr = make_logstr(Response, Origin),
	    {Response, LogStr};
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
		    {invalid}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
fix_topvia_received(Header, Origin) when record(Origin, siporigin) ->
    IP = Origin#siporigin.addr,
    InPortNo = Origin#siporigin.port,
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
%% Function: 
%% Descrip.: 
%% Returns : 
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

replace_top_via(NewVia, Header) when record(NewVia, via) ->
    [FirstVia | Via] = sipheader:via(keylist:fetch("Via", Header)),
    keylist:set("Via", sipheader:via_print(lists:append([NewVia], Via)), Header).

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
%% Function: received_from_strict_router/2
%% Description: Look at the URI of a request we just received to see
%%              if it is something we (possibly) put in a Record-Route
%%              and this is a request sent from a strict router
%%              (RFC2543 compliant UA).
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
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
remove_route_matching_me(Header) ->
    Route = sipheader:contact(keylist:fetch("Route", Header)),
    case Route of
        [{_, FirstRoute} | NewRoute] ->
	    case route_matches_me(FirstRoute) of
		true ->
		    logger:log(debug, "Sipserver: First Route ~p matches me, removing it.",
			       [sipheader:contact_print([{none, FirstRoute}])]),
		    case NewRoute of
			[] ->
			    keylist:delete("Route", Header);
			_ ->
			    keylist:set("Route", sipheader:contact_print(NewRoute), Header)
		    end;
		_ ->
		    Header
	    end;
	_ ->
	    Header
    end.

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
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
check_packet(Request, Origin) when record(Request, request), record(Origin, siporigin) ->
    {Method, Header} = {Request#request.method, Request#request.header},
    check_supported_uri_scheme(Request#request.uri, Header),
    sanity_check_contact(request, "From", Header),
    sanity_check_contact(request, "To", Header),
    case sipheader:cseq(keylist:fetch("CSeq", Header)) of
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
	    check_for_loop(Header, Request#request.uri, Request#request.method, Origin);	
	_ ->
	    true
    end;

check_packet(Response, Origin) when record(Response, response), record(Origin, siporigin) ->
    sanity_check_contact(response, "From", Response#response.header),
    sanity_check_contact(response, "To", Response#response.header).

check_for_loop(Header, URI, Method, Origin) when record(Origin, siporigin) ->
    LoopCookie = siprequest:get_loop_cookie(Header, URI, Origin#siporigin.proto),
    ViaHostname = siprequest:myhostname(),
    ViaPort = sipserver:get_listenport(Origin#siporigin.proto),
    CmpVia = #via{host=ViaHostname, port=ViaPort},
    case via_indicates_loop(LoopCookie, CmpVia,
    			    sipheader:via(keylist:fetch("Via", Header))) of
	true ->
	    throw({sipparseerror, request, Header, 482, "Loop Detected"});
	_ ->
	    true
    end.

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

make_logstr(Request, Origin) when record(Request, request) ->
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
    ClientStr = origin2str(Origin, "unknown"),
    lists:flatten(io_lib:format("~s ~s [client=~s, from=<~s>, to=<~s>]", 
				[Method, sipurl:print(URI), ClientStr, url2str(FromURI), url2str(ToURI)]));
make_logstr(Response, Origin) when record(Response, response) ->
    Header = Response#response.header,
    {_, CSeqMethod} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
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
origin2str(F, Default) ->
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

