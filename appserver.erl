-module(appserver).
-export([start/2, start_actions/10]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, [phone], false]),
    {ok, Pid}.

init() ->
    true. %database_call:create_call().

request("INVITE", URI, Header, Body, Socket, FromIP) ->
    % create header suitable for answering the incoming request
    AnswerHeader = siprequest:make_answerheader(Header),
    ContactURI = case sipheader:contact(keylist:fetch("Contact", Header)) of
	[{_, C}] ->
	    C;
	_ ->
	    throw({siperror, 400, "No valid Contact-header in request"})
    end,
    CallBranch = sipproxy:generate_branch() ++ "-UAS",
    % XXX add tag= to To-header for UAS process?
    %CAnswerHeader = tag("To", AnswerHeader),
    CallPid = sipproxy:start_UAS(CallBranch, "INVITE", Socket, ContactURI, 86400, URI, AnswerHeader, Body, FromIP),
    logger:log(debug, "Appserver: Sending 100 Trying -> PID ~p", [CallPid]),
    CallPid ! {sendresponse, 100, "Trying"},
    Key = local:sipuser(URI),
    Actions = get_actions(URI),
    case Actions of
	nomatch ->
	    logger:log(normal, "Appserver: No actions found for ~p, returning 404 Not Found", [Key]),
	    siprequest:send_notfound(AnswerHeader, Socket);
	_ ->
	    logger:log(debug, "Appserver: User ~p actions :~n~p", [Key, Actions]),
	    fork_actions(CallBranch, CallPid, "INVITE", URI, Header, Header, Body, Socket, FromIP, Actions)
    end;

request("ACK", URI, Header, Body, Socket, FromIP) ->
    do_request("ACK", URI, Header, Body, Socket, FromIP);
    
request("CANCEL", URI, Header, Body, Socket, FromIP) ->
    do_request("CANCEL", URI, Header, Body, Socket, FromIP);
    
request("BYE", URI, Header, Body, Socket, FromIP) ->
    do_request("BYE", URI, Header, Body, Socket, FromIP);
    
request(Method, URI, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    logger:log(normal, "~s -> 501 Not Implemented", [LogStr]),
    siprequest:send_result(Header, Socket, "", 501, "Not Implemented").

do_request(Method, URI, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call_type(CallID, appserver) of
	{atomic, [{Origheaders, {Pid, CallBranch}}]} ->
	    logger:log(normal, "~s -> PID ~p", [LogStr, Pid]),
	    Pid ! {siprequest, Method, {URI, Header, Body}};
	{atomic, []} ->
	    logger:log(normal, "~s -> 481 Call/Transaction Does Not Exist", [LogStr]),
	    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist")
    end.


response(Status, Reason, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call_type(CallID, appserver) of
	{atomic, [{Origheaders, {Pid, CallBranch}}]} ->
	    case sipproxy:get_branch(Header) of
		CallBranch ->
		    logger:log(normal, "Response to CallBranch ~s: ~p ~s -> PID ~p", [LogStr, Status, Reason, Pid]),
		    Pid ! {response, CallBranch, {Status, Reason, Header, Body}};
		_ ->
		    logger:log(normal, "Response to ~s: ~p ~s -> PID ~p", [LogStr, Status, Reason, Pid]),
		    Pid ! {response, {Status, Reason, Header, Body}}
	    end;
	{atomic, []} ->
	    logger:log(normal, "Response to ~s: ~p ~s -> Call ~p not found (ignoring)", [LogStr, Status, Reason, CallID])
    end.

get_actions(URI) ->
    Key = local:sipuser(URI),
    case fetch_actions(Key) of
	none ->
	    {User, Pass, Host, Port, Parameters} = URI,
	    case util:isnumeric(User) of
		true ->
		    case fetch_actions(User) of
			none -> nomatch;
			Actions -> lists:append(Actions, [{wait, 40}])
		    end;
		_ ->
		    nomatch
	    end;
	Actions ->
	    lists:append(Actions, [{wait, 40}])
    end.

fetch_actions(Key) ->
    case phone:get_phone(Key) of
    	{atomic, []} ->
	    none;
	{atomic, Locations} ->
	    BestLocations = local:prioritize_locations(Key, Locations),
	    locations_to_actions(BestLocations)
    end.

locations_to_actions([]) ->
    [];
locations_to_actions([{Location, Flags, Class, Expire}]) ->
    [{call, 30, Location}];
locations_to_actions([{Location, Flags, Class, Expire} | Rest]) ->
    Actions = locations_to_actions(Rest),
    lists:append([{call, 30, Location}], Actions).

fork_actions(CallBranch, CallPid, Method, URI, OrigHeader, ReqHeader, Body, Socket, FromIP, Actions) ->
    ForkPid = spawn(?MODULE, start_actions, [CallBranch, self(), Method, URI, OrigHeader, ReqHeader, Body, Socket, FromIP, Actions]),
    logger:log(normal, "FREDRIK: THIS IS GLUE PROCESS ~p. CALLPID ~p FORKPID ~p", [self(), CallPid, ForkPid]),
    process_messages(CallPid, ForkPid, calling).

start_actions(CallBranch, Parent, Method, URI, OrigHeader, ReqHeader, Body, Socket, FromIP, Actions) ->
    % create header suitable for our outgoing requests
    BranchHeader = keylist:delete("Route", ReqHeader),
    [CallID] = keylist:fetch("Call-ID", OrigHeader),
    % parent is the appserver process running around in process_messages()
    case database_call:insert_call_unique(CallID, appserver, OrigHeader, {Parent, CallBranch}) of
	{atomic, ok} ->
	    sipproxy:fork(Method, Parent, URI, ReqHeader, Body, FromIP, Socket, Actions, []),
	    % We don't return from fork() until all Actions are done, and fork() signals Parent
	    % when it is done so we don't need to do anything more here except remove the
	    % call from the database.
	    logger:log(debug, "Appserver glue: fork() done, removing call ~p from database", [CallID]),
	    database_call:delete_call_type(CallID, appserver);
	{aborted, key_exists} ->
	    logger:log(error, "Appserver glue: Could not start call with CallID ~p - key exists", [CallID]),
	    true
    end.

% Receive messages from branch pids and do whatever is necessary with them
process_messages(CallPid, ForkPid, State) ->
    receive
	{siprequest, "CANCEL", {URI, Header, Body}} ->
	    % XXX check that this is a CANCEL of the original INVITE (or whatever)?
	    logger:log(normal, "Appserver glue: Original request cancelled, reply 200 OK and send 'cancel' -> fork pid ~p", [ForkPid]),
	    CallPid ! {forwardresponse, Header, "", 200, "OK"},
	    ForkPid ! {cancel},
	    logger:log(debug, "Appserver glue: Request send of 487 Request Terminated -> call pid ~p", [CallPid]),
	    CallPid ! {sendresponse, 487, "Request Terminated"},
	    process_messages(CallPid, ForkPid, terminated);
	{siprequest, Method, {URI, Header, Body}} ->
	    logger:log(debug, "Appserver glue: Forwarding ~p -> call pid ~p", [Method, CallPid]),
	    CallPid ! {siprequest, Method, {URI, Header, Body}},
	    process_messages(CallPid, ForkPid, State);
	{response, Branch, {Status, Reason, ResponseHeader, ResponseBody}} ->
	    logger:log(debug, "Appserver glue: Received ~p ~s for branch ~p, calling process_messages_response()", [Status, Reason, Branch]),
	    process_messages_response(State, CallPid, ForkPid, Branch, Status, Reason, ResponseHeader, ResponseBody);
	{response, {Status, Reason, Header, Body}} ->
	    %logger:log(debug, "Appserver glue: Forwarding ~p ~s -> fork pid ~p", [Status, Reason, ForkPid]),
	    ForkPid ! {response, {Status, Reason, Header, Body}},
	    process_messages(CallPid, ForkPid, State);    
	{all_terminated} ->
	    logger:log(debug, "Appserver glue: received all_terminated"),
    	    CallPid ! {sendresponse, 486, "Busy Here"},
	    process_messages(CallPid, ForkPid, State);
	{no_more_actions} ->
	    case State of
		terminated ->
		    logger:log(debug, "Appserver glue: received no_more_actions when 'terminated' - ignoring");
		confirmed ->
		    logger:log(debug, "Appserver glue: received no_more_actions when 'completed' - ignoring");
		_ ->
		    logger:log(debug, "Appserver glue: received no_more_actions when in state '~p', responding 480 Temporarily Unavailable to original request",
		    		       [State]),
		    CallPid ! {sendresponse, 480, "Temporarily Unavailable"}
	    end,
	    process_messages(CallPid, ForkPid, State);
	{callhandler_terminating} ->
	    logger:log(debug, "Appserver glue: received callhandler_terminating, exiting");
	{quit} ->
	    logger:log(normal, "Appserver glue: Quitting")
    end.

process_messages_response(State, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status == 100 ->
    logger:log(normal, "Appserver glue: Received 100 when in State '~p'", [State]),
    process_messages(CallPid, ForkPid, State);

process_messages_response(calling, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status =< 199 ->
    % XXX some provisional responses contain RTP data for early media, while some others don't. Do we care?
    CallPid ! {sendresponse, Status, Reason},
    logger:log(normal, "Appserver glue: Sending (re-formatted) ~p ~s and changing state from 'calling' to 'proceeding'", [Status, Reason]),
    process_messages(CallPid, ForkPid, proceeding);
process_messages_response(proceeding, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status =< 199 ->
    logger:log(normal, "Appserver glue: Ignoring provisional response ~p ~s when allready 'proceeding'", [Status, Reason]),
    process_messages(CallPid, ForkPid, proceeding);

process_messages_response(calling, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status =< 299 ->
    logger:log(normal, "Appserver glue: Got successfull final response ~p ~s, send it on and go from 'calling' to 'confirmed' state", [Status, Reason]),
    CallPid ! {forwardresponse, Header, Body, Status, Reason},    
    ForkPid ! {cancel_all_branches_except, [Branch]},
    process_messages(CallPid, ForkPid, confirmed);
process_messages_response(proceeding, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status =< 299 ->
    logger:log(normal, "Appserver glue: Got successfull final response ~p ~s, send it on and go from 'proceeding' to 'confirmed' state", [Status, Reason]),
    CallPid ! {forwardresponse, Header, Body, Status, Reason},    
    ForkPid ! {cancel_all_branches_except, [Branch]},
    process_messages(CallPid, ForkPid, confirmed);
process_messages_response(confirmed, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status =< 299 ->
    logger:log(normal, "Appserver glue: Got successfull final response ~p ~s when allready in 'confirmed' state, forwarding", [Status, Reason]),
    CallPid ! {forwardresponse, Header, Body, Status, Reason},    
    process_messages(CallPid, ForkPid, confirmed);

process_messages_response(State, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status =< 599 ->
    % We ignore these because ForkPid will send us a signal when all branches fail, this is just one branch reporting.
    logger:log(normal, "Appserver glue: Got final response ~p ~s when in state '~p', ignoring", [Status, Reason, State]),
    process_messages(CallPid, ForkPid, State);

process_messages_response(terminated, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status =< 699 ->
    logger:log(normal, "Appserver glue: Got global failure response ~p ~s when allready 'terminated', ignoring", [Status, Reason]),
    process_messages(CallPid, ForkPid, terminated);
process_messages_response(State, CallPid, ForkPid, Branch, Status, Reason, Header, Body) when Status =< 699 ->
    logger:log(normal, "Appserver glue: Got global failure response ~p ~s when in state '~p', forwarding and entering state 'terminated'", [Status, Reason, State]),
    CallPid ! {forwardresponse, Header, Body, Status, Reason},
    process_messages(CallPid, ForkPid, terminated).



