-module(sipproxy).
-export([start/6, process_branch/6]).

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
	    locations_to_actions(BestLocations);
	_ ->
	    none
    end.

locations_to_actions([]) ->
    [];
locations_to_actions([{Location, Flags, Class, Expire}]) ->
    [{call, 30, Location}];
locations_to_actions([{Location, Flags, Class, Expire} | Rest]) ->
    Actions = locations_to_actions(Rest),
    lists:append([{call, 30, Location}], Actions).

start(Method, URI, Header, Body, Socket, FromIP) ->
    % create header suitable for answering the incoming request
    AnswerHeader = siprequest:make_answerheader(Header),
    % create header suitable for our outgoing requests
    ForkHeader = keylist:delete("Route", AnswerHeader),
    logger:log(normal, "Forking: Sending 100 Trying", []),
    siprequest:send_result(AnswerHeader, Socket, "", 100, "Trying"),
    [CallID] = keylist:fetch("Call-ID", AnswerHeader),
    case database_call:insert_call_unique(CallID, proxy, AnswerHeader,
					  self()) of
	{atomic, ok} ->
	    Key = local:sipuser(URI),
	    Actions = get_actions(URI),
	    case Actions of
		nomatch ->
		    logger:log(normal, "Forking: No actions found for ~p, returning 404 Not Found", [Key]),
		    siprequest:send_notfound(AnswerHeader, Socket);
		_ ->
		    logger:log(debug, "Forking: User ~p actions :~n~p", [Key, Actions]),
		    fork(CallID, URI, ForkHeader, Body, FromIP, Socket, Actions, [])
	    end;
	{aborted, key_exists} ->
	    logger:log(debug, "Forking: Could not start call with CallID ~p - key exists", [CallID]),
	    true
    end.

get_branch(Header) ->
    case sipheader:via(keylist:fetch("Via", Header)) of
	[Via | _] ->
	    case dict:find("branch", sipheader:via_params(Via)) of
		error ->
		    none;
		{ok, Branch} ->
		    Branch
	    end;
	_ ->
	    none
    end.

stop_resend(Targets, Header) ->
    Branch = get_branch(Header),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    case find_target(Branch, Targets) of
	{BranchURI, Pid, Status} ->
	    Pid ! {stopresend, Method};
	_ ->
	    logger:log(normal, "Forking: Could not find branch for response to ~p", [Branch, Method]),
	    true
    end.

cancel_targets(Targets) ->
    % send {cancel} to all PIDs in states other than completed
    NewTargets1 = cancel_targets_state(Targets, calling),
    cancel_targets_state(NewTargets1, proceeding).

cancel_targets_state(Targets, State) ->
    % send {cancel} to all PIDs in a specific State
    lists:map(fun ({Branch, BranchURI, Pid, BranchState}) ->
    			case BranchState of
    			    State ->
				Pid ! {cancel},
				{Branch, BranchURI, Pid, completed};
			    _ ->
				{Branch, BranchURI, Pid, BranchState}
			end
	      end, Targets).

cancel_orig_request(Header, Socket, CancelHeader, Targets) ->
    logger:log(normal, "Forking: Replying 200 OK to CANCEL", []),
    siprequest:send_result(CancelHeader, Socket, "", 200, "OK"),
    logger:log(normal, "Forking: Got CANCEL, cancelling all 'calling' and 'proceeding' targets", []),
    %logger:log(debug, "FREDRIK: TARGETS WHEN ORIG-REQUEST CANCEL:~n~p", [Targets]),
    NewTargets = cancel_targets(Targets),
    logger:log(normal, "Forking: Replying 487 Request Terminated to original request", []),
    siprequest:send_result(Header, Socket, "", 487, "Request Terminated"),
    %logger:log(debug, "FREDRIK: TARGETS AFTER ORIG-REQUEST CANCEL:~n~p", [NewTargets]),
    NewTargets.

process_branch(Socket, Timeout, URI, Branch, Header, Body) ->
    logger:log(normal, "~s: Sending INVITE -> ~s", [Branch, sipurl:print(URI)]),
    siprequest:send_proxy_request(Header, Socket,
				  {"INVITE", URI, Body, ["branch=" ++ Branch]}),
    TimerA = 500,
    TimerB = TimerA * 64,
    timer:send_after(TimerA, {timer, timerA, invite}),
    timer:send_after(TimerB, {stopresend, "INVITE"}),
    timer:send_after(Timeout * 1000, {cancel, Timeout}),
    process_branch_loop(Socket, URI, Branch, Header, Body,
			{invite, TimerA * 2}).

process_cancel(Socket, URI, Branch, Header, Body) ->
    {CSeq, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    Newheader = keylist:set("CSeq", [sipheader:cseq_print({CSeq, "CANCEL"})],
			    Header),
    siprequest:send_proxy_request(Newheader, Socket,
				  {"CANCEL", URI, Body, ["branch=" ++ Branch]}),
    TimerA = 500,
    TimerB = TimerA * 64,
    timer:send_after(TimerA, {timer, timerA, cancel}),
    timer:send_after(TimerB, {stopresend, "CANCEL"}),
    process_branch_loop(Socket, URI, Branch, Newheader, Body,
			{cancel, TimerA * 2}).

process_branch_loop(Socket, URI, Branch, Header, Body, {invite, TimerA}) ->
    receive
	{cancel} ->
	    logger:log(normal, "~s: Sending CANCEL -> ~s", [Branch, sipurl:print(URI)]),
	    process_cancel(Socket, URI, Branch, Header, Body);
	{stopresend, "INVITE"} ->
	    logger:log(normal, "~s: Resend(INVITE -> ~s): stopping resend", [Branch, sipurl:print(URI)]),
	    process_branch_loop(Socket, URI, Branch, Header, Body,
				{none});
	{quit} ->
	    logger:log(normal, "~s: Resend(INVITE -> ~s): quitting branch", [Branch, sipurl:print(URI)]),
	    true;
	{timer, timerA, invite} ->
	    logger:log(normal, "~s: Resend(INVITE -> ~s): resending", [Branch, sipurl:print(URI)]),
	    timer:send_after(TimerA, {timer, timerA, invite}),
	    siprequest:send_proxy_request(Header, Socket,
					  {"INVITE", URI, Body,
					   ["branch=" ++ Branch]}),
	    process_branch_loop(Socket, URI, Branch, Header, Body,
				{invite, TimerA * 2})
    end;

process_branch_loop(Socket, URI, Branch, Header, Body, {cancel, TimerA}) ->
    receive
	{stopresend, "CANCEL"} ->
	    logger:log(normal, "~s: Resend(CANCEL -> ~s): stopping (and quitting branch)", [Branch, sipurl:print(URI)]),
	    true;
	{quit} ->
	    logger:log(normal, "~s: Resend(CANCEL -> ~s): quitting branch", [Branch, sipurl:print(URI)]),
	    true;
	{timer, timerA, cancel} ->
	    logger:log(normal, "~s: Resend(CANCEL -> ~s): resending", [Branch, sipurl:print(URI)]),
	    timer:send_after(TimerA, {timer, timerA, cancel}),
	    siprequest:send_proxy_request(Header, Socket,
					  {"CANCEL", URI, Body,
					   ["branch=" ++ Branch]}),
	    process_branch_loop(Socket, URI, Branch, Header, Body,
				{cancel, TimerA * 2})
    end;

process_branch_loop(Socket, URI, Branch, Header, Body, {none}) ->
    receive
	{cancel} ->
	    logger:log(normal, "~s: Sending CANCEL -> ~s", [Branch, sipurl:print(URI)]),
	    process_cancel(Socket, URI, Branch, Header, Body);
	{quit} ->
	    logger:log(normal, "~s: Resend(~s): quitting branch", [Branch, sipurl:print(URI)]),
	    true
    end.

generate_branch() ->
    {Megasec, Sec, Microsec} = now(),
    "z9hG4bK" ++ hex:to(Sec, 8) ++ hex:to(Microsec, 8).

start_branch(Socket, ReqURI, Timeout, URI, Header, Body, FromIP) ->
    Branch = generate_branch(),
    LogStr = sipserver:make_logstr({request, "INVITE", ReqURI, Header, Body}, FromIP),
    logger:log(normal, "~s: (new) ~s -> Fork to ~s", [Branch, LogStr, sipurl:print(URI)]),
    {Branch, URI, spawn(?MODULE, process_branch, [Socket, Timeout, URI, Branch,
					     Header, Body]), calling}.

response_action(Response, calling) when Response == 100 ->
    logger:log(debug, "Decision: Received 100, going from 'calling' to 'proceeding'"),
    {ignore, proceeding, none};
response_action(Response, BranchState) when Response == 100 ->
    logger:log(debug, "Decision: Received 100, always ignoring (current branch state ~p)", [BranchState]),
    {ignore, BranchState, none};

response_action(Response, calling) when Response =< 199 ->
    % This was the first provisional response received, forward it to the originator.
    logger:log(debug, "Decision: Received first 1xx ~p, branch = calling -> send and enter proceeding", [Response]),
    {send, proceeding, none};
response_action(Response, BranchState) when Response =< 199 ->
    % This was NOT the first provisional response, just ignore it.
    logger:log(debug, "Decision: Received 1xx ~p when in state ~p, ignoring", [Response, BranchState]),
    {ignore, BranchState, none};

response_action(Response, calling) when Response =< 299 ->
    % We have a winner! This branch received the first 2xx response.
    logger:log(debug, "Decision: Received 2xx ~p, branch = calling, complete this branch and CANCEL all others", [Response]),
    {send, completed, cancel};
response_action(Response, proceeding) when Response =< 299 ->
    % We have a winner! This branch received the first 2xx response.
    logger:log(debug, "Decision: Received 2xx ~p, branch = proceeding, complete this branch and CANCEL all others", [Response]),
    {send, completed, cancel};
response_action(Response, BranchState) when Response =< 299 ->
    % This is NOT the first 2xx response. Ignore and finish this branch.
    logger:log(debug, "Decision: Received 2xx ~p when in state ~p, ignoring", [Response, BranchState]),
    {ignore, BranchState, none};

response_action(Response, terminated) when Response =< 599 ->
    % This branch was allready terminated, re-send ACK in case it got lost.
    logger:log(debug, "Decision: Received 4xx or 5xx ~p -> when terminated, ACK again", [Response]),
    {ack, terminated, none};
response_action(Response, BranchState) when Response =< 599 ->
    % Ok, this branch is now finished.
    logger:log(debug, "Decision: Received 4xx or 5xx ~p -> ACK and complete this branch (Branch was '~p')", [Response, BranchState]),
    {ack, terminated, none};

response_action(Response, BranchState) when Response =< 699 ->
    % Global failure, ack and finish all branches.
    logger:log(debug, "Decision: Received 6xx ~p -> ACK and CANCEL all (Branch was '~p')", [Response, BranchState]),
    {sendandack, terminated, cancel}.

find_target(Branch, Targets) ->
    case lists:keysearch(Branch, 1, Targets) of
	false ->
	    none;
	{value, {_, BranchURI, Pid, BranchState}} ->
	    {BranchURI, Pid, BranchState}
    end.
    
fork(CallID, ReqURI, Header, Body, FromIP, Socket, Actions, Targets) ->
    case Actions of
	[{call, Timeout, URI} | Rest] ->
	    logger:log(debug, "Forking: calling ~s with timeout ~p",
		      [sipurl:print(URI), Timeout]),
	    Newtargets = [start_branch(Socket, ReqURI, Timeout, URI, Header, Body, FromIP)
			  | Targets],
	    fork(CallID, ReqURI, Header, Body, FromIP, Socket, Rest, Newtargets);
	[{wait, Time} | Rest] ->
	    logger:log(normal, "Forking: waiting ~p seconds", [Time]),
	    case process_wait(CallID, ReqURI, Header, Body, FromIP, Socket, Rest,
			 Targets, false, util:timestamp() + Time) of
		finished ->
		    % XXX set controlpid to none
		    logger:log(normal, "Forking: end processing");
		NewTargets ->
		    fork(CallID, ReqURI, Header, Body, FromIP, Socket, Rest, NewTargets)
	    end;
	[] ->
	    % XXX set controlpid to none
	    case allterminated(Targets) of
		true ->
		    logger:log(normal, "Forking: All targets are terminated, send 486 Busy Here to original request"),
		    siprequest:send_proxy_response(Socket, 486, "Busy here", Header,  "");
		_ ->
		    cancel_targets(Targets)
	    end,
	    logger:log(normal, "Forking: done"),
	    true
    end.

evaluate_response(Targets, Header, Response) ->
    Branch = get_branch(Header),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    case Method of
	"INVITE" ->
	    case find_target(Branch, Targets) of
		none ->
		    logger:log(debug, "process_response() could not find branch ~p in target-list", [Branch]),
		    true;
		{BranchURI, BranchPid, BranchState} ->
		    logger:log(debug, "FREDRIK: TARGETS :~n~p", [Targets]),
		    logger:log(debug, "~s: Rolling the dice, response was ~p and state is ~p", [Branch, Response, BranchState]),
		    {BranchAction, NewBranchState, OtherBranchesAction} =
			response_action(Response, BranchState),
		    if
			NewBranchState /= BranchState ->
			    logger:log(debug, "~s: Changing BranchState from '~p' to '~p', OtherBranchesAction '~p'", 
					[Branch, BranchState, NewBranchState, OtherBranchesAction]);
			true -> none
		    end,
		    {BranchAction, NewBranchState, OtherBranchesAction,
		        lists:keyreplace(Branch, 1, Targets, {Branch, BranchURI, BranchPid, NewBranchState})}
	    end;
	_ ->
	    logger:log(debug, "~s: Ignoring ~p response to ~p (not response to INVITE)", [Branch, Response, Method]),
	    {ignore, none, none, Targets}
    end.

send_ack(Socket, Header, Branch, BranchURI, Status, Reason) ->
    URI = case sipheader:contact(keylist:fetch("Contact", Header)) of
        [] ->
	    BranchURI;
	[{_, Contact}|_] ->
	    Contact
    end,
    logger:log(normal, "~s: send ACK to response ~p ~s (send to ~p)",
	       [Branch, Status, Reason, sipurl:print(URI)]),
    siprequest:send_proxy_request(Header, Socket, {"ACK", URI, "", []}).

process_response(Branch, Status, Reason, Header, Socket, ResponseHeader, ResponseBody, ReqURI, Targets) ->
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", ResponseHeader)),
    logger:log(normal, "~s: Response to ~s: ~p ~s", [Branch, Method, Status, Reason]),
    stop_resend(Targets, ResponseHeader),
    {BranchURI, BranchPid, BranchState} = find_target(Branch, Targets),
    {BranchAction, NewBranchState, OtherBranchesAction, NewTargets1} =
	evaluate_response(Targets, ResponseHeader, Status),
    case BranchAction of
	ignore ->
	    logger:log(debug, "~s: ignoring ~p ~s", [Branch, Status, Reason]);
	ack ->
	    send_ack(Socket, ResponseHeader, Branch, BranchURI, Status, Reason);
	send ->
	    logger:log(normal, "~s: send ~p ~s", [Branch, Status, Reason]),
	    siprequest:send_proxy_response(Socket, Status, Reason,
					   ResponseHeader,
					   ResponseBody);
	sendandack ->
	    send_ack(Socket, ResponseHeader, Branch, BranchURI, Status, Reason),
	    logger:log(normal, "~s: send ~p ~s", [Branch, Status, Reason]),
	    siprequest:send_proxy_response(Socket, Status, Reason,
					   ResponseHeader,
					   ResponseBody);
	quit ->
	    logger:log(debug, "~s: terminating PID ~p", [Branch, BranchPid]),
	    BranchPid ! {quit}
    end,

    case NewBranchState of
	terminated ->
	    logger:log(debug, "~s: sending 'quit' to PID ~p since this branch has terminated", [Branch, BranchPid]),
	    BranchPid ! {quit};
	completed ->
	    logger:log(debug, "~s: sending 'quit' to PID ~p since this branch has completed", [Branch, BranchPid]),
	    BranchPid ! {quit};
	_ ->
	    true
    end,
	    
    case OtherBranchesAction of
        cancel ->
	    cancel_targets(NewTargets1);
	_ ->
	    NewTargets1
    end.

allterminated([]) ->
    true;
allterminated([{_, _, _, terminated} | Rest]) ->
    allterminated(Rest);
allterminated([_ | Rest]) ->
    false.

process_wait(CallID, ReqURI, Header, Body, FromIP, Socket, Actions, Targets, true, AbsTime) ->
    finished;
process_wait(CallID, ReqURI, Header, Body, FromIP, Socket, Actions, Targets, false, AbsTime) ->
    Time = lists:max([AbsTime - util:timestamp(), 0]),
    receive
	{cancel, {CancelStatus, CancelHeader, CancelBody}} ->
	    NewTargets = cancel_orig_request(Header, Socket, CancelHeader, Targets),
	    process_wait(CallID, ReqURI, Header, Body, FromIP, Socket, Actions, 
	    		 NewTargets, allterminated(NewTargets), AbsTime);
	{response, {Status, Reason, ResponseHeader, ResponseBody}} ->
	    Branch = get_branch(ResponseHeader),
	    NewTargets = case Branch of
		none ->
		    Targets;
	    _ ->
		    process_response(Branch, Status, Reason, Header, Socket,
				     ResponseHeader, ResponseBody, ReqURI, Targets)
	    end,
	    process_wait(CallID, ReqURI, Header, Body, FromIP, Socket, Actions,
			 NewTargets, allterminated(NewTargets), AbsTime)
    after
	Time * 1000 ->
	    logger:log(debug, "Forking: This wait's time (~p seconds) is over", [Time]),
	    Targets
    end.
