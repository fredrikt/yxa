-module(appserver).
-export([start/2, create_session/6]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, [phone], false]),
    {ok, Pid}.

init() ->
    true. %database_call:create_call().

request("ACK", URI, Header, Body, Socket, FromIP) ->
    do_request("ACK", URI, Header, Body, Socket, FromIP);
request("CANCEL", URI, Header, Body, Socket, FromIP) ->
    do_request("CANCEL", URI, Header, Body, Socket, FromIP);

request(Method, URI, OrigHeader, Body, Socket, FromIP) ->
    Header = case sipserver:get_env(record_route, false) of
	true -> siprequest:add_record_route(OrigHeader);
	false -> OrigHeader
    end,
    case database_call:fetch_call(Header, appserver) of
	nomatch ->
	    create_session(Method, URI, Header, Body, Socket, FromIP);
	{OrigRequest, {Pid, CallBranch, TargetList}} ->
	    {OrigMethod, OrigURI, _, _} = OrigRequest,
	    case URI of
		OrigURI ->
		    case is_process_alive(Pid) of
			true ->
			    case sipproxy:get_branch(Header) of
				none ->
				    logger:log(normal, "appserver: Received ~s ~s matching existing transaction, sending on to GluePid ~p", [Method, sipurl:print(URI), Pid]),
				    safe_signal(Pid, {siprequest, callbranch, {Method, URI, Header, Body}});
				Branch ->
				    logger:log(normal, "appserver: Received ~s ~s matching existing transaction, sending on to GluePid ~p", [Method, sipurl:print(URI), Pid]),
				    safe_signal(Pid, {siprequest, Branch, {Method, URI, Header, Body}})
			    end;		
			false ->
			    logger:log(normal, "appserver: Received ~s ~s matching existing transaction with dead GluePid ~p. Removing transaction from database and starting new.",
			    		[Method, sipurl:print(URI), Pid]),
			    DialogueID = sipheader:dialogueid(Header),
			    delete_call(DialogueID),
			    create_session(Method, URI, Header, Body, Socket, FromIP)
		    end;
		_ ->
		    logger:log(debug, "appserver: Found dialogue, but this requests URI (~s) does not match original requests URI (~s), proxying.",
		    		[sipurl:print(URI), sipurl:print(OrigURI)]),
		    siprequest:send_proxy_request(Header, Socket, {Method, URI, Body, []})
	    end;
	Foo ->
	    logger:log(error, "appserver: Unknown database content returned :~n~p", [Foo])
    end.

create_session("INVITE", URI, Header, Body, Socket, FromIP) ->
    % create header suitable for answering the incoming request
    case keylist:fetch("Route", Header) of
	[] ->
	    BranchBase = sipproxy:generate_branch(),
	    CallBranch = BranchBase ++ "-UAS",
	    OrigRequest = {"INVITE", URI, Header, Body},
	    CallPid = serverbranch:start(CallBranch, Socket, FromIP, self(), OrigRequest),
	    Actions = get_actions(URI),
	    Key = local:sipuser(URI),
	    case Actions of
		nomatch ->
		    logger:log(normal, "Appserver: No actions found for INVITE ~s (SIP user ~p), answering 404 Not Found", [sipurl:print(URI), Key]),
		    fork_actions(BranchBase, CallBranch, CallPid, OrigRequest, Socket, FromIP, {sendresponse, 404, "Not Found"});
		_ ->
		    logger:log(debug, "Appserver: User ~p actions :~n~p", [Key, Actions]),
		    fork_actions(BranchBase, CallBranch, CallPid, OrigRequest, Socket, FromIP, Actions)
	    end;
	_ ->
	    logger:log(debug, "Appserver: Request INVITE ~s has Route header. Forwarding statelessly.", [sipurl:print(URI)]),
	    LogStr = sipserver:make_logstr({request, "INVITE", URI, Header, Body}, FromIP),
	    logger:log(normal, "~s: Forwarding statelessly", [LogStr]),
	    siprequest:send_proxy_request(Header, Socket, {"INVITE", URI, Body, []})
    end;
create_session(Method, URI, Header, Body, Socket, FromIP) ->
    % create header suitable for answering the incoming request
    case keylist:fetch("Route", Header) of
	[] ->
	    BranchBase = sipproxy:generate_branch(),
	    CallBranch = BranchBase ++ "-UAS",
	    OrigRequest = {Method, URI, Header, Body},
	    Actions = get_actions(URI),
	    Key = local:sipuser(URI),
	    case Actions of
		nomatch ->
		    logger:log(normal, "Appserver: No actions found for ~s ~s (SIP user ~p), answering 404 Not Found",
		    		[Method, sipurl:print(URI), Key]),
		    AnswerHeader = siprequest:make_answerheader(Header),
		    siprequest:send_notfound(AnswerHeader, Socket);
		_ ->
		    CallPid = serverbranch:start(CallBranch, Socket, FromIP, self(), OrigRequest),
		    logger:log(debug, "Appserver: User ~p actions :~n~p", [Key, Actions]),
		    fork_actions(BranchBase, CallBranch, CallPid, OrigRequest, Socket, FromIP, Actions)
	    end;
	_ ->
	    logger:log(debug, "Appserver: Request ~s ~s has Route header. Forwarding statelessly.", [Method, sipurl:print(URI)]),
	    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
	    logger:log(normal, "~s: Forwarding statelessly", [LogStr]),
	    siprequest:send_proxy_request(Header, Socket, {Method, URI, Body, []})
    end.

do_request(Method, URI, OrigHeader, Body, Socket, FromIP) ->
    Header = case sipserver:get_env(record_route, false) of
	true -> siprequest:add_record_route(OrigHeader);
	false -> OrigHeader
    end,
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    case database_call:fetch_call(Header, appserver) of
	nomatch ->
	    DialogueID = sipheader:dialogueid(Header),
	    logger:log(normal, "~s Dialogue ~p -> 481 Call/Transaction Does Not Exist", [LogStr, DialogueID]),
	    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist");
	{OrigRequest, {Pid, CallBranch, Targets}} ->
	    {OrigMethod, OrigURI, _, _} = OrigRequest,
	    case URI of
		OrigURI ->
		    Branch = sipproxy:get_branch(Header),
		    case Branch of
			CallBranch ->
			    logger:log(normal, "~s -> glue PID ~p (callbranch)", [LogStr, Pid]),
			    safe_signal(Pid, {siprequest, callbranch, {Method, URI, Header, Body}});
			none ->
			    logger:log(normal, "~s -> glue PID ~p (callbranch, since no branch was found)", [LogStr, Pid]),
			    safe_signal(Pid, {siprequest, callbranch, {Method, URI, Header, Body}});
			_ ->
			    case targetlist:get_target(Branch, Targets) of
				none ->
				    logger:log(debug, "appserver: Could not find target for branch ~p in debugfriendly(Targets) :~n~p",
				    		[Branch, targetlist:debugfriendly(Targets)]),
				    logger:log(normal, "~s -> glue PID ~p (callbranch, since branch ~s is not recognized)", [LogStr, Pid, Branch]),
				    safe_signal(Pid, {siprequest, callbranch, {Method, URI, Header, Body}});
				Target ->
				    BranchPid = targetlist:extract_pid(Target),
				    logger:log(debug, "appserver: Forwarding request ~s ~s to BranchPid ~p", [Method, sipurl:print(URI), BranchPid]),
				    safe_signal(BranchPid, {request, {Method, URI, Header, Body}})
			    end
		    end;
		_ ->
		    logger:log(debug, "appserver: Found dialogue, but this requests URI (~s) does not match original requests URI (~s), proxying.",
		    		[sipurl:print(URI), sipurl:print(OrigURI)]),
		    siprequest:send_proxy_request(Header, Socket, {Method, URI, Body, []})
	    end;
	Foo ->
	    logger:log(error, "appserver: Unknown database content returned :~n~p", [Foo])
    end.

delete_call(DialogueID) ->
    {DbDialogueID, _} = database_call:fetch_dialogue(DialogueID, appserver),
    logger:log(debug, "Appserver: Deleting call ~p from database", [DbDialogueID]),
    database_call:delete_call_type(DbDialogueID, appserver).

response(Status, Reason, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    case database_call:fetch_call(Header, appserver) of
	nomatch ->
	    % RFC 3261 16.7 says we MUST act like a stateless proxy when no
	    % transaction can be found
	    DialogueID = sipheader:dialogueid(Header),
	    logger:log(debug, "Response to ~s: ~p ~s -> Call ~p not found, forwarding statelessly", [LogStr, Status, Reason, DialogueID]),
	    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body);
	{OrigRequest, {GluePid, CallBranch, TargetList}} ->
	    Branch = sipproxy:get_branch(Header),
	    logger:log(normal, "~s: Response to ~s: ~p ~s", [Branch, LogStr, Status, Reason]),
	    case Branch of
		CallBranch ->
		    logger:log(debug, "Response to CallBranch ~s: ~p ~s -> Glue PID ~p", [LogStr, Status, Reason, GluePid]),
		    safe_signal(GluePid, {response, CallBranch, {Status, Reason, Header, Body}});
		_ ->
		    logger:log(debug, "Response to ~s: ~p ~s -> PID ~p", [LogStr, Status, Reason, GluePid]),
		    safe_signal(GluePid, {response, {Status, Reason, Header, Body}})
	    end;
	Foo ->
	    logger:log(error, "appserver: Unknown database content returned :~n~p", [Foo])
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

forward_call_actions({Forwards, Timeout, Localring}, Actions) ->
    Func = fun(Forward) ->
		   {call, 40, {Forward, none, "kth.se", none, []}}
	   end,
    ForwardActions = lists:map(Func, Forwards),
    case Localring of
	true ->
	    lists:append(Actions, [{wait, Timeout} | ForwardActions]);
	_ ->
	    case Timeout of
		0 ->
		    ForwardActions;
		_ ->
		    lists:append(Actions, [{wait, Timeout} | ForwardActions])
	    end
    end.

fetch_actions(Key) ->
    case database_forward:fetch(Key) of
	{atomic, [F]} ->
	    forward_call_actions(F, fetch_phone_actions(Key));
	_ ->
	    fetch_phone_actions(Key)
    end.

fetch_phone_actions(Key) ->
    case phone:get_phone(Key) of
    	{atomic, []} ->
	    none;
	{atomic, Locations} ->
	    BestLocations = local:prioritize_locations(Key, Locations),
	    locations_to_actions(BestLocations)
    end.

locations_to_actions([]) ->
    [];
locations_to_actions([{Location, Flags, Class, Expire} | Rest]) ->
    lists:append([{call, 40, Location}], locations_to_actions(Rest)).

fork_actions(BranchBase, CallBranch, CallPid, Request, Socket, FromIP, Actions) ->
    {Method, URI, OrigHeader, _} = Request,
    DialogueID = sipheader:dialogueid(OrigHeader),
    case database_call:insert_call_unique(DialogueID, appserver, Request, {self(), CallBranch, []}) of
	{atomic, ok} ->
	    {ForkPid, InitialState} = case Actions of
		{sendresponse, Status, Reason} ->
		    safe_signal(CallPid, {sendresponse, Status, Reason}),
		    {none, completed};
		_ ->
		    {sipserver:safe_spawn(fun start_actions/6, [BranchBase, self(), Request, Socket, FromIP, Actions]), calling}
	    end,
	    logger:log(normal, "Appserver: Starting up call ~p, glue process ~p, CallPid ~p, ForkPid ~p, Initial state ~p",
	    		[DialogueID, self(), CallPid, ForkPid, InitialState]),
	    process_messages(Socket, CallPid, ForkPid, InitialState),
	    logger:log(normal, "Appserver glue: Finished with call (~s ~s), exiting.", [Method, sipurl:print(URI)]),
	    delete_call(DialogueID),
	    true;
	{aborted, key_exists} ->
	    logger:log(error, "Appserver glue: Could not start call with dialogue id ~p - key exists, must be duplicate request", [DialogueID]),
	    safe_signal(CallPid, {quit}),
	    false
    end.

start_actions(BranchBase, GluePid, OrigRequest, Socket, FromIP, Actions) ->
    {Method, URI, OrigHeader, _} = OrigRequest,
    Timeout = 32,
    % We don't return from fork() until all Actions are done, and fork() signals GluePid
    % when it is done.
    sipproxy:fork(BranchBase, GluePid, OrigRequest, FromIP, Socket, Actions, Timeout, []),
    DialogueID = sipheader:dialogueid(OrigHeader),
    logger:log(debug, "Appserver glue: fork() of call ~p (~s ~s) done, start_actions() returning", [DialogueID, Method, sipurl:print(URI)]),
    true.

% Receive messages from branch pids and do whatever is necessary with them
process_messages(Socket, none, none, State) -> 
    logger:log(debug, "Appserver glue: Both CallPid and ForkPid are 'none' - terminating when in state ~p", [State]);
process_messages(Socket, CallPid, ForkPid, State) ->
    receive
	{siprequest, callbranch, Request} ->
	    {Method, URI, _, _} = Request,
	    logger:log(debug, "Appserver glue: Forwarding ~s ~s -> CallPid ~p", [Method, sipurl:print(URI), CallPid]),
	    safe_signal(CallPid, {request, Request}),
	    process_messages(Socket, CallPid, ForkPid, State);
	
	{siprequest, Branch, Request} ->
	    {Method, URI, _, _} = Request,
	    logger:log(debug, "Appserver glue: Forwarding ~s ~s -> ForkPid ~p", [Method, sipurl:print(URI), ForkPid]),
	    safe_signal(ForkPid, {siprequest, Request}),
	    process_messages(Socket, CallPid, ForkPid, State);

	
	{response, notarget, Response} ->
	    {Status, Reason, ResponseHeader, ResponseBody} = Response,
	    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", ResponseHeader)),
	    logger:log(debug, "Appserver glue: Received ~p ~s in response to ~p but no target was found, forwarding statelessly",
	    			[Status, Reason, Method]),
	    %safe_signal(CallPid, {forwardresponse, Response}),
	    siprequest:send_proxy_response(Socket, Status, Reason, ResponseHeader, ResponseBody),
	    process_messages(Socket, CallPid, ForkPid, State);

	{response, Branch, Response} ->
	    {Status, Reason, ResponseHeader, ResponseBody} = Response,
	    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", ResponseHeader)),
	    logger:log(debug, "Appserver glue: Received ~p ~s in response to ~p from branch ~p, forwarding to CallPid",
	    			[Status, Reason, Method, Branch]),
	    safe_signal(CallPid, {forwardresponse, Response}),
	    process_messages(Socket, CallPid, ForkPid, State);

	{response, Response} ->
	    {Status, Reason, Header, Body} = Response,
	    logger:log(debug, "Appserver glue: Forwarding ~p ~s -> fork pid ~p", [Status, Reason, ForkPid]),
	    safe_signal(ForkPid, {response, {Status, Reason, Header, Body}}),
	    process_messages(Socket, CallPid, ForkPid, State);    

	{response_timeout, Branch, Response} ->
	    {Status, Reason, _, _} = Response,
	    logger:log(debug, "Appserver glue: Branch ~p sending of response ~p ~s has timed out (my state is ~p). Asking CallPid to quit.",
	    		[Branch, Status, Reason, State]),
	    safe_signal(CallPid, {quit}),
	    process_messages(Socket, CallPid, ForkPid, State);    

	{request_cancelled, CancelledRequest, CancelRequest} ->
	    logger:log(debug, "Appserver glue: Received word that the original request has been cancelled, sending 'cancel_pending' to ForkPid ~p and entering state 'cancelled'",
	    			[ForkPid]),
	    safe_signal(ForkPid, {cancel_pending}),
	    process_messages(Socket, CallPid, ForkPid, cancelled);
	    
	{all_terminated, FinalResponse, Targets} ->
	    NewState = case State of
		completed ->
		    logger:log(debug, "Appserver glue: received all_terminated when 'completed' - ignoring (XXX could this happen?)"),
		    completed;
		terminated ->
		    logger:log(debug, "Appserver glue: received all_terminated when 'terminated' - ignoring"),
		    terminated;
		cancelled ->
		    logger:log(debug, "Appserver glue: received all_terminated when in state '~p' - request was cancelled. Ask CallPid ~p to send 487 Request Terminated and entering state 'completed'",
		    		       [State, CallPid]),
		    safe_signal(CallPid, {sendresponse, 487, "Request Cancelled"}),
		    completed;
		_ ->
		    case FinalResponse of
			{Status, Reason, _, _} ->
			    logger:log(debug, "Appserver glue: received all_terminated when in state '~p' - forwarding the ~p ~s response to CallPid ~p and entering state 'terminated'",
			    		       [State, Status, Reason, CallPid]),
	    		    safe_signal(CallPid, {forwardresponse, FinalResponse});
	    		{Status, Reason} ->
			    logger:log(debug, "Appserver glue: received all_terminated when in state '~p' - asking CallPid ~p to answer ~p ~s and entering state 'terminated'",
			    		       [State, CallPid, Status, Reason]),
	    		    safe_signal(CallPid, {sendresponse, Status, Reason})
	    	    end,
	    	    terminated
	    end,
	    process_messages(Socket, CallPid, ForkPid, NewState);

	{no_more_actions} ->
	    NewState = case State of
		completed ->
		    logger:log(debug, "Appserver glue: received no_more_actions when 'completed' - ignoring"),
		    completed;
		terminated ->
		    logger:log(debug, "Appserver glue: received no_more_actions when 'terminated' - ignoring"),
		    terminated;
		cancelled ->
		    logger:log(debug, "Appserver glue: received no_more_actions when in state '~p' - request was cancelled. Ask CallPid ~p to send 487 Request Terminated and entering state 'completed'",
		    		       [State, CallPid]),
		    safe_signal(CallPid, {sendresponse, 487, "Request Terminated"}),
		    completed;
		_ ->
		    logger:log(debug, "Appserver glue: received no_more_actions when in state '~p', responding 408 Request Timeout to original request",
		    		       [State]),
		    safe_signal(CallPid, {sendresponse, 408, "Request Timeout"}),
		    terminated
	    end,
	    process_messages(Socket, CallPid, ForkPid, NewState);

	{callhandler_terminating, FromPid} ->
	    logger:log(debug, "Appserver glue: received callhandler_terminating from ~p (ForkPid is ~p (CallPid is ~p)) - setting ForkPid to 'none'", [FromPid, ForkPid, CallPid]),
	    process_messages(Socket, CallPid, none, State);
	%    case FromPid of
	%	CallPid ->
	%	    logger:log(debug, "Appserver glue: received callhandler_terminating from ~p - setting ForkPid to 'none'", [FromPid]),
	%	    process_messages(Socket, CallPid, none, State);
	%	_ ->
	%	    logger:log(debug, "Appserver glue: received callhandler_terminating from unknown pid ~p (ForkPid is ~p) - ignoring", [FromPid, ForkPid]),
	%	    process_messages(Socket, CallPid, ForkPid, State)
	%    end;
	    
	{serverbranch_terminating, {Branch, FromPid}} ->
	    logger:log(debug, "Appserver glue: received serverbranch_terminating from ~p (CallPid is ~p (ForkPid is ~p)) - setting CallPid to 'none'", [FromPid, CallPid, ForkPid]),
	    process_messages(Socket, none, ForkPid, State);
	%    case FromPid of
	%	CallPid ->
	%	    logger:log(debug, "Appserver glue: received serverbranch_terminating from ~p - setting CallPid to 'none'", [FromPid]),
	%	    process_messages(Socket, none, ForkPid, State);
	%	_ ->
	%	    logger:log(debug, "Appserver glue: received serverbranch_terminating from unknown pid ~p (CallPid is ~p) - ignoring", [FromPid, CallPid]),
	%	    process_messages(Socket, CallPid, ForkPid, State)
	%    end;
	    
	{quit} ->
	    logger:log(debug, "Appserver glue: Quitting process_messages(Socket, )")
    after
	300 * 1000 ->
	    logger:log(debug, "Appserver glue: Still alive after 5 minutes! CallPid ~p, ForkPid ~p, State ~p", [CallPid, ForkPid, State]),
	    safe_signal(CallPid, {showtransactions}),
	    safe_signal(ForkPid, {showtargets}),
	    NewCallPid = case is_process_alive(CallPid) of
		true ->
		    CallPid;
		false ->
		    logger:log("Appserver glue: CallPid ~p is not any longer alive, garbage collected.", [CallPid]),
		    none
	    end,
	    NewForkPid = case is_process_alive(ForkPid) of
		true ->
		    ForkPid;
		false ->
		    logger:log("Appserver glue: ForkPid ~p is not any longer alive, garbage collected.", [ForkPid]),
		    none
	    end,
	    process_messages(Socket, NewCallPid, NewForkPid, State)
    end.


safe_signal(Pid, Message) ->
    case Pid of
	none ->
	    logger:log(debug, "Appserver: Dropped signal ~p to terminated pid: ~p", [Message, Pid]),
	    true;
	_ ->
	    case is_process_alive(Pid) of
		true ->
		    Pid ! Message;
		false ->
		    logger:log(error, "Appserver: Can't send signal ~p to pid ~p - not alive", [Message, Pid])
	    end
    end.

% XXX Store Route-Set when creating early or completed dialogue
%
% 16.2 :
% 2. URI scheme check
%   
%  If the Request-URI has a URI whose scheme is not understood by the
%  proxy, the proxy SHOULD reject the request with a 416 (Unsupported
%  URI Scheme) response.
%
% Reject requests with too low Max-Forwards before forking
%
% XXX loop detection
                    
