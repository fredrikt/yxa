-module(sipproxy).
-export([fork/8, generate_branch/0, get_branch/1]).

% This is the start() function of sipproxy. sipproxy can presently
% accomplish 'stateless forking'. It can follow an Actions list and
% fork a request to several Targets.
%
% You start it by calling the fork() method with a set of Actions. Currenly
% supported actions are a list of 'call' and 'wait'. You can mix calls and
% waits freely.

fork(BranchBase, Parent, OrigRequest, FromIP, Socket, Actions, Timeout, Targets) ->
    % XXX detect Max-Forwards: 1 here instead of when sending the requests
    % out since siprequest:send_proxy_request() would send the errors
    % right back to the caller
    {Method, ReqURI, Header, Body} = OrigRequest,
    case Actions of
	[{call, CallTimeout, CallURI} | Rest] ->
	    logger:log(debug, "sipproxy: calling ~s with timeout ~p",
		      [sipurl:print(CallURI), CallTimeout]),
	    Branch = BranchBase ++ "-UAC" ++ integer_to_list(length(Targets) + 1),
	    Request = {Method, CallURI, Header, Body},
	    BranchPid = clientbranch:start(Branch, Socket, FromIP, self(), OrigRequest, Request, CallTimeout),
	    NewTargets = targetlist:add_target(Branch, Request, BranchPid, calling, none, Targets),
	    fork(BranchBase, Parent, OrigRequest, FromIP, Socket, Rest, Timeout, NewTargets);
	[{wait, Time} | Rest] ->
	    logger:log(debug, "sipproxy: waiting ~p seconds", [Time]),
	    case process_wait(Parent, OrigRequest, Socket, Targets,
			      false, util:timestamp() + Time, calling) of
		{discontinue, NewTargets} ->
		    logger:log(debug, "sipproxy: discontinue processing"),
		    fork(BranchBase, Parent, OrigRequest, FromIP, Socket, [], Timeout, NewTargets);
		NewTargets ->
		    fork(BranchBase, Parent, OrigRequest, FromIP, Socket, Rest, Timeout, NewTargets)
	    end;
	[] ->
	    case allterminated(Targets) of
		true ->
		    logger:log(debug, "sipproxy: All targets are terminated, staying alive to handle resends");
		_ ->
		    logger:log(normal, "sipproxy: Reached end of Actions-list for ~s ~s, cancelling pending targets.",
		    		[Method, sipurl:print(ReqURI)]),
		    cancel_pending_targets(Targets)
	    end,
	    Parent ! {no_more_actions},
	    logger:log(debug, "sipproxy: waiting ~p seconds for final responses", [Timeout]),
	    process_wait(Parent, OrigRequest, Socket, Targets,
			 false, util:timestamp() + Timeout, stayalive),
	    Parent ! {callhandler_terminating, self()}
    end.

generate_branch() ->
    {Megasec, Sec, Microsec} = now(),
    "z9hG4bK-yxa-" ++ hex:to(Sec, 8) ++ hex:to(Microsec, 8).

get_branch(Header) ->
    case sipheader:via(keylist:fetch("Via", Header)) of
	[Via | _] ->
	    % XXX check that this is my Via
	    case dict:find("branch", sipheader:via_params(Via)) of
		error ->
		    none;
		{ok, Branch} ->
		    Branch
	    end;
	_ ->
	    none
    end.

cancel_pending_targets(Targets) ->
    % send {cancel} to all PIDs in states other than completed and terminated
    NewTargets1 = cancel_targets_state(Targets, calling),
    cancel_targets_state(NewTargets1, proceeding).

cancel_all_targets(Targets) ->
    % send {cancel} to all PIDs in states other than terminated
    NewTargets1 = cancel_targets_state(Targets, trying),
    NewTargets1 = cancel_targets_state(Targets, calling),
    NewTargets2 = cancel_targets_state(NewTargets1, proceeding),
    cancel_targets_state(NewTargets2, completed).

cancel_targets_state(Targets, State) ->
    % send {cancel} to all PIDs in a specific State, return updated TargetList
    lists:map(fun (ThisTarget) ->
    			BranchState = targetlist:extract_state(ThisTarget),
    			case BranchState of
    			    State ->
    			        Pid = targetlist:extract_pid(ThisTarget),
				Pid ! {cancel, "cancel_targets_state"},
				%NewTarget1 = targetlist:set_state(ThisTarget, completed),
				targetlist:set_endresult(ThisTarget, cancelled);
			    _ ->
				ThisTarget
			end
	      end, Targets).

process_wait(Parent, OrigRequest, Socket, Targets, true, AbsTime, State) ->
    logger:log(debug, "sipproxy: All Targets terminated or completed. Ending process_wait(), returning discontinue. debugfriendly(TargetList) :~n~p",
    		[targetlist:debugfriendly(Targets)]),
    {discontinue, Targets};
process_wait(Parent, OrigRequest, Socket, Targets, false, AbsTime, State) ->
    Time = lists:max([AbsTime - util:timestamp(), 0]),
    ProcessNewTargets = receive
	{cancel_all} ->
	    logger:log(debug, "sipproxy: process_wait() received 'cancel_all', calling cancel_all_targets()"),
	    cancel_all_targets(Targets);
	{cancel_pending} ->
	    logger:log(debug, "sipproxy: process_wait() received 'cancel_pending', calling cancel_pending_targets()"),
	    cancel_pending_targets(Targets);

	{request, Request} ->
	    {Method, URI, Header, Body} = Request,
	    Branch = get_branch(Header),
	    case targetlist:get_target(Branch, Targets) of
		none ->
		    logger:log(error, "sipproxy: Received an ~s for an unknown Target (branch ~p), ignoring", [Method, Branch]);
		ThisTarget ->
		    BranchPid = targetlist:extract_pid(ThisTarget),
		    logger:log(debug, "sipproxy: Forwarding ~s request to branch ~p pid ~p", [Method, Branch, BranchPid]),
		    safe_signal(BranchPid, {request, Request})
	    end,
	    Targets;

	{response, Response} ->
	    {Status, Reason, Header, Body} = Response,
	    {_, ResponseToMethod} = sipheader:cseq(keylist:fetch("CSeq", Header)),
	    Branch = get_branch(Header),
	    case targetlist:get_target(Branch, Targets) of
		none ->
		    logger:log(normal, "sipproxy: Received response ~p ~s to ~s for an unknown Target (branch ~p), sending to parent (Pid ~p) for stateless forwarding.",
		    			[Status, Reason, ResponseToMethod, Branch, Parent]),
		    Parent ! {response, notarget, Response};
		ThisTarget ->
		    case targetlist:extract_state(ThisTarget) of
			terminated ->
			    logger:log(normal, "sipproxy: Response ~p ~s to terminated branch ~p, sending to parent (Pid ~p) for stateless forwarding.",
			    		[Status, Reason, Branch, Parent]),
			    Parent ! {response, notarget, Response};
			_ ->
			    FetchedRequest = targetlist:extract_initialrequest(ThisTarget),
			    {FetchedMethod, FetchedURI, _, _} = FetchedRequest,
			    BranchPid = targetlist:extract_pid(ThisTarget),
			    logger:log(debug, "sipproxy: Forwarding response ~p ~s to ~s, branch ~p to pid ~p (original request ~s ~s)", 
					    [Status, Reason, ResponseToMethod, Branch, BranchPid, FetchedMethod, sipurl:print(FetchedURI)]),
			    safe_signal(BranchPid, {response, Response})
		    end
	    end,
	    Targets;

	{branch_result, Branch, NewBranchState, Response, ResponseToRequest} ->
	    {ResponseToMethod, ResponseToURI, _, _} = ResponseToRequest,
	    {Status, Reason, _, _} = Response,
	    case targetlist:get_target(Branch, Targets) of
		none ->
		    logger:log(error, "sipproxy: Received branch result ~p ~s to ~s ~s from an unknown Target (branch ~p), ignoring.",
		    			[Status, Reason, ResponseToMethod, sipurl:print(ResponseToURI), Branch]),
		    Targets;
		ThisTarget ->
		    NewTarget1 = targetlist:set_state(ThisTarget, NewBranchState),
		    NewTarget2 = targetlist:set_endresult(NewTarget1, Response),	% XXX only do this for final responses?
		    NewTargets = targetlist:update_target(NewTarget2, Targets),
		    logger:log(debug, "sipproxy: Received branch result ~p ~s to ~s ~s from branch ~p. My Targets-list now contain :~n~p",
		    		[Status, Reason, ResponseToMethod, sipurl:print(ResponseToURI), Branch, targetlist:debugfriendly(NewTargets)]),
		    case forward_immediately(ResponseToMethod, Status) of
			true ->
			    logger:log(debug, "sipproxy: Forwarding response ~p ~s (in response to ~s ~s) to my parent (PID ~p) immediately",
			    		[Status, Reason, ResponseToMethod, sipurl:print(ResponseToURI), Parent]),
			    Parent ! {response, Branch, Response};
			_ ->
			    logger:log(debug, "sipproxy: Not forwarding response ~p ~s (in response to ~s ~s) immediately",
			    		[Status, Reason, ResponseToMethod, sipurl:print(ResponseToURI)])
		    end,
		    cancel_pending_if_invite_2xx_or_6xx(ResponseToMethod, Status, NewTargets)
	    end;

	{resend_response, Request} ->
	    {Method, URI, _, _} = Request,
	    logger:log(error, "sipproxy: Dropping request to resend response (to request ~s ~s), such a request should NOT be sent to client transactions.",
	    		[Method, sipurl:print(URI)]),
	    Targets;

	{clientbranch_terminating, {Branch, ClientPid}} ->
	    case targetlist:get_target(Branch, Targets) of
		none ->
		    logger:log(error, "sipproxy: Received 'clientbranch_terminating' (from Pid ~p) from an unknown Target (branch ~p), ignoring.",
		    			[ClientPid, Branch]),
		    Targets;
		ThisTarget ->
		    NewTarget1 = targetlist:set_state(ThisTarget, terminated),
		    UpdatedTargets = targetlist:update_target(NewTarget1, Targets),
		    logger:log(debug, "sipproxy: Received notification that branch ~p PID ~p has terminated. Targetlist :~n~p", 
		    		[Branch, ClientPid, targetlist:debugfriendly(UpdatedTargets)]),
		    UpdatedTargets
	    end;
	    	    	
	{showtargets} ->
	    logger:log(debug, "sipproxy: Received 'showtargets' request, debugfriendly(TargetList) :~n~p",
    			[targetlist:debugfriendly(Targets)]),
    	    Targets;
    	    
	Msg ->
	    logger:log(error, "sipproxy: Received unknown message ~p, ignoring", [Msg]),
	    Targets
    after
	Time * 1000 ->
	    logger:log(debug, "sipproxy: This wait's time (~p seconds) is over", [Time]),
	    {return}
    end,
    case ProcessNewTargets of
	{return} ->
	    Targets;
	_ ->
	    NewState = report_upstreams_statemachine(Parent, ProcessNewTargets, State),
	    TimeLeft = lists:max([AbsTime - util:timestamp(), 0]),
	    AllTerminated = allterminated(ProcessNewTargets),
	    EndProcessing = end_processing(AbsTime, ProcessNewTargets, NewState),
	    logger:log(debug, "sipproxy: State changer, State=~p, NewState=~p, TimeLeft=~p, AllTerminated=~p. EndProcessing = ~p",
	    		[State, NewState, TimeLeft, AllTerminated, EndProcessing]),
	    process_wait(Parent, OrigRequest, Socket, 
	    		 ProcessNewTargets, EndProcessing, AbsTime, NewState)
    end.

allterminated(TargetList) ->
    TargetsCalling = targetlist:get_targets_in_state(calling, TargetList),
    TargetsTrying = targetlist:get_targets_in_state(trying, TargetList),
    TargetsProceeding = targetlist:get_targets_in_state(proceeding, TargetList),
    TargetsCompleted = targetlist:get_targets_in_state(completed, TargetList),
    if
	TargetsCalling /= [] -> false;
	TargetsTrying /= [] -> false;
	TargetsProceeding /= [] -> false;
	%TargetsCompleted /= [] -> false;
	true -> true
    end.

end_processing(AbsTime, _, stayalive) ->
    Now = util:timestamp(),
    if
	Now =< AbsTime -> false;
	true -> true
    end;
end_processing(_, _, completed) ->
    true;
end_processing(_, TargetList, State) ->
    allterminated(TargetList).

report_upstreams_statemachine(Parent, Targets, calling) ->
    case allterminated(Targets) of
	true ->
	    Responses = targetlist:get_responses(Targets),
	    logger:log(debug, "sipproxy: All targets are terminated, evaluating responses :~n~p", [printable_responses(Responses)]),
	    ForwardResponse = case Responses of
		[] ->
		    logger:log(normal, "sipproxy: No responses to choose between, answering 408 Request Timeout"),
		    {408, "Request Timeout"};
		_ ->
		    case make_final_response(Responses) of
			none ->
			    logger:log(normal, "sipproxy: Found no suitable response in list ~p, answering 408 Request Timeout", [printable_responses(Responses)]),
			    {408, "Request Timeout"};
			FinalResponse ->
			    {Status, Reason, _, _} = FinalResponse,
			    logger:log(normal, "sipproxy: Picked response ~p ~s from the list of responses available (~p).",
			    		[Status, Reason, printable_responses(Responses)]),
			    FinalResponse
		    end
	    end,
	    logger:log(debug, "sipproxy: Sending the result to parent Pid ~p.", [Parent]),
	    Parent ! {all_terminated, ForwardResponse, Targets},
	    completed;
	false ->
	    calling
    end;
report_upstreams_statemachine(Parent, Targets, State) ->
    State.

safe_signal(Pid, Message) ->
    case Pid of
	none ->
	    logger:log(debug, "sipproxy: Dropped message to pid 'none' : ~p", [Message]),
	    false;
	_ ->
	    case is_process_alive(Pid) of
		true ->
		    Pid ! Message,
		    true;
		false ->
		    logger:log(debug, "sipproxy: Can't send signal to pid ~p - not alive", [Pid]),
		    false
	    end
    end.

forward_immediately(Method, Status) when Status =< 99 ->
    logger:log(error, "sipproxy: Invalid response ~p to ~s", [Status, Method]),
    false;
forward_immediately("INVITE", Status) when Status =< 299 ->
    true;
forward_immediately(Method, Status) when Status =< 199 ->
    true;
forward_immediately(Method, Status) ->
    false.

cancel_pending_if_invite_2xx_or_6xx("INVITE", Status, Targets) when Status =< 199 ->
    Targets;
cancel_pending_if_invite_2xx_or_6xx("INVITE", Status, Targets) when Status =< 299 ->
    logger:log(debug, "sipproxy: Cancelling pending targets since one INVITE transaction resulted in a 2xx response ~p", [Status]),
    cancel_pending_targets(Targets);
cancel_pending_if_invite_2xx_or_6xx(Method, Status, Targets) when Status =< 599 ->
    Targets;
cancel_pending_if_invite_2xx_or_6xx(Method, Status, Targets) when Status =< 699 ->
    logger:log(debug, "sipproxy: Cancelling pending targets since one branch resulted in a 6xx response ~p", [Status]),
    cancel_pending_targets(Targets).

printable_responses([]) ->
    [];
printable_responses([{Status, Reason, _, _} | Rest]) ->
    lists:append([integer_to_list(Status) ++ " " ++ Reason], printable_responses(Rest));
printable_responses([Response | Rest]) ->
    lists:append([Response], printable_responses(Rest)).

% look for 6xx responses, then pick the lowest but prefer 
% 401, 407, 415, 420 and 484
%
% It is not by accident we look for 2xx here as well, it is because this response-list
% will be used to resend responses to non-INVITE if we receive them retransmitted
make_final_response(Responses) ->
    TwoxxResponses = get_xx_responses(200, Responses),
    ThreexxResponses = get_xx_responses(300, Responses),
    FourxxResponses = get_xx_responses(400, Responses),
    FivexxResponses = get_xx_responses(500, Responses),
    SixxxResponses = get_xx_responses(600, Responses),
    if
	SixxxResponses /= [] ->
	    aggregate_authreqs(lists:nth(1, SixxxResponses), Responses);
	TwoxxResponses /= [] ->
	    aggregate_authreqs(lists:nth(1, TwoxxResponses), Responses);
	ThreexxResponses /= [] ->
	    aggregate_authreqs(lists:nth(1, ThreexxResponses), Responses);
	FourxxResponses /= [] ->
	    BestFourxx = prefer_response([401, 407, 415, 420, 484], FourxxResponses),
	    aggregate_authreqs(BestFourxx, Responses);
	FivexxResponses /= [] ->
	    % XXX RFC 3261 says we SHOULD avoid 503 Service Unavailable!
	    aggregate_authreqs(lists:nth(1, FivexxResponses), Responses);
	true ->
	    logger:log(debug, "Appserver: No response to my liking"),
	    none
    end.	    

aggregate_authreqs(BestResponse, Responses) ->
    {BestStatus, _, _, _} = BestResponse,
    WWWAuth = collect_auth_headers("WWW-Authenticate", Responses),
    ProxyAuth = collect_auth_headers("Proxy-Authenticate", Responses),
    DoAggregate = case BestStatus of
	401 -> true;
	407 -> true;
	_ -> false
    end,
    case DoAggregate of
	true ->
	    logger:log(debug, "Adding aggregated authentication headers to response"),
	    % XXX finish this
	    BestResponse;
	_ ->
	    BestResponse
    end.    

% XXX implement this
collect_auth_headers(Key, Responses) ->
    [].
    

% XXX implement this
prefer_response(PreferenceList, Responses) ->
    lists:nth(1, Responses).

get_xx_responses(Min, Responses) ->
    Max = Min + 99,
    lists:keysort(1, get_range_responses(Min, Max, Responses)).
    
get_range_responses(Min, Max, []) ->
    [];
get_range_responses(Min, Max, [{Status, Reason, Header, Body} | Rest]) ->
    if
	Status < Min -> get_range_responses(Min, Max, Rest);
	Status > Max -> get_range_responses(Min, Max, Rest);
	true ->
	    lists:append([{Status, Reason, Header, Body}], get_range_responses(Min, Max, Rest))
    end.


