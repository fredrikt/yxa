-module(sipproxy).
-export([start/5]).

-include("sipproxy.hrl").
-include("siprecords.hrl").

-record(state, {parent, branchbase, request, actions, targets, endtime, final_response_sent, mystate, approx_msgsize, timeout}).
% mystate is either calling, cancelled, completed or stayalive

% This is the "start" function of sipproxy. sipproxy can fork a request
% according to a list of sipproxy_action elements.
%
% You start it by calling the start() method with a set of Actions. Currenly
% supported actions are a list of 'call' and 'wait'. You can mix calls and
% waits freely.

start(BranchBase, Parent, Request, Actions, Timeout) when record(Request, request) ->
    case siprequest:check_proxy_request(Request) of
	{ok, _, ApproxMsgSize} ->
	    case keylist:fetch("Route", Request#request.header) of
		[] ->
		    State = #state{parent=Parent, branchbase=BranchBase, request=Request, 
				   actions=Actions, timeout=Timeout, targets=targetlist:empty(),
				   mystate=calling, approx_msgsize=ApproxMsgSize,
				   endtime=util:timestamp() + Timeout, final_response_sent=false},
		    fork(State),
		    util:safe_signal("sipproxy :", Parent, {callhandler_terminating, self()}),
		    ok;
		_ ->
		    logger:log(debug, "sipproxy: Can't fork request with Route header"),
		    {error, "Request with Route header could not be forked"}
	    end;
	_ ->
	    logger:log(debug, "sipproxy: check_proxy_request did not say it was OK to proxy request"),
	    {error, "Request could not be forked"}
    end.

%%
%% No actions left
%%
fork(State) when record(State, state), State#state.actions == [] ->
    OrigRequest = State#state.request,
    Targets = State#state.targets,
    {Method, ReqURI} = {OrigRequest#request.method, OrigRequest#request.uri},
    NewTargets = case allterminated(Targets) of
		     true ->
			 logger:log(debug, "sipproxy: All targets are terminated, staying alive to handle resends"),
			 Targets;
		     _ ->
			 logger:log(normal, "sipproxy: Reached end of Actions-list for ~s ~s, cancelling pending targets.",
				    [Method, sipurl:print(ReqURI)]),
			 NewTargets1 = cancel_pending_targets(Targets),
			 NewTargets1
    end,
    util:safe_signal("sipproxy :", State#state.parent, {no_more_actions}),
    logger:log(debug, "sipproxy: waiting ~p seconds for final responses", [State#state.timeout]),
    process_wait(false, State#state{mystate=stayalive, targets=NewTargets, endtime=util:timestamp() + State#state.timeout}),
    ok;
fork(State) when record(State, state) ->
    OrigRequest = State#state.request,
    [HAction | TAction] = State#state.actions,
    {Method, ReqURI} = {OrigRequest#request.method, OrigRequest#request.uri},
    Targets = State#state.targets,
    case HAction#sipproxy_action.action of
	call ->
	    CallURI = HAction#sipproxy_action.requri,
	    CallTimeout = HAction#sipproxy_action.timeout,
	    logger:log(debug, "sipproxy: forking ~s request to ~s with timeout ~p",
		      [Method, sipurl:print(CallURI), CallTimeout]),
	    Branch = State#state.branchbase ++ "-UAC" ++ integer_to_list(targetlist:list_length(Targets) + 1),
	    Request = OrigRequest#request{uri=CallURI},
	    DstList = sipdst:url_to_dstlist(CallURI, State#state.approx_msgsize, CallURI),
	    [FirstDst|_] = DstList,
	    NewTargets = case transactionlayer:start_client_transaction(Request, none, FirstDst, Branch, CallTimeout, self()) of
	    	BranchPid when pid(BranchPid) ->
		    targetlist:add(Branch, Request, BranchPid, calling, CallTimeout, DstList, Targets);
		{error, E} ->
		    logger:log(error, "sipproxy: Failed starting client transaction : ~p", [E]),
		    Targets
	    end,
	    fork(State#state{actions=TAction, targets=NewTargets});
	wait ->
	    Time = HAction#sipproxy_action.timeout,
	    logger:log(debug, "sipproxy: waiting ~p seconds", [Time]),
	    case process_wait(false, State) of
		{discontinue, NewState} ->
		    logger:log(debug, "sipproxy: discontinue processing"),
		    fork(NewState#state{actions=[]});
		NewTargets ->
		    fork(State#state{actions=TAction, targets=NewTargets})
	    end
    end.

cancel_pending_targets(Targets) ->
    % send {cancel} to all PIDs in states other than completed and terminated
    NewTargets1 = cancel_targets_state(Targets, calling),
    cancel_targets_state(NewTargets1, proceeding).

cancel_targets_state(Targets, State) ->
    % send {cancel} to all PIDs in a specific State, return updated TargetList
    TargetsInState = targetlist:get_targets_in_state(State, Targets),
    lists:map(fun (ThisTarget) ->
    			[Pid] = targetlist:extract([pid], ThisTarget),
			gen_server:cast(Pid, {cancel, "cancel_targets_state"})
	      end, TargetsInState),
    NewTargets = mark_cancelled(TargetsInState, Targets),
    NewTargets.

mark_cancelled([], TargetList) ->
    TargetList;
mark_cancelled([H | T], TargetList) ->
    NewTarget = targetlist:set_cancelled(H, true),
    NewTargetList = targetlist:update_target(NewTarget, TargetList),
    mark_cancelled(T, NewTargetList).

process_wait(true, State) when record(State, state) ->
    Targets = State#state.targets,
    logger:log(debug, "sipproxy: All Targets terminated or completed. Ending process_wait(), returning discontinue. debugfriendly(TargetList) :~n~p",
    		[targetlist:debugfriendly(Targets)]),
    {discontinue, State};
process_wait(false, State) when record(State, state) ->
    Time = lists:max([State#state.endtime - util:timestamp(), 0]),
    Targets = State#state.targets,
    {Res, NewState} = receive
	{cancel_pending} ->
	    logger:log(debug, "sipproxy: process_wait() received 'cancel_pending', calling cancel_pending_targets()"),
	    NewTargets = cancel_pending_targets(Targets),
	    % If mystate is 'calling', we need to set it to 'cancelled' to prevent us from
	    % starting new transactions for additional destinations if we were to receive
	    % a 503 response.
	    NewMyState = case State#state.mystate of
		calling -> cancelled;
		S -> S
	    end,
	    NewState1 = State#state{targets=NewTargets, mystate=NewMyState},
	    {ok, NewState1};

	{branch_result, Branch, NewTransactionState, Response} when record(Response, response) ->
	    {Status, Reason} = {Response#response.status, Response#response.reason},
	    NewState1 = case targetlist:get_using_branch(Branch, Targets) of
		none ->
		    logger:log(error, "sipproxy: Received branch result ~p ~s from an unknown Target (branch ~p), ignoring.",
		    			[Status, Reason, Branch]),
		    State;
		ThisTarget ->
		    [ResponseToRequest] = targetlist:extract([request], ThisTarget),
		    {RMethod, RURI} = {ResponseToRequest#request.method, ResponseToRequest#request.uri},
		    NewTarget1 = targetlist:set_state(ThisTarget, NewTransactionState),
		    NewTarget2 = targetlist:set_endresult(NewTarget1, Response),	% XXX only do this for final responses?
		    NewTargets1 = targetlist:update_target(NewTarget2, Targets),
		    NewTargets = try_next_destination(Response, ThisTarget, NewTargets1, State),
		    logger:log(debug, "sipproxy: Received branch result ~p ~s (request: ~s ~s) from branch ~p. My Targets-list now contain :~n~p",
		    		[Status, Reason, RMethod, sipurl:print(RURI), Branch, targetlist:debugfriendly(NewTargets)]),
		    NewState2 = State#state{targets=NewTargets},
		    NewState3 = check_forward_immediately(ResponseToRequest, Response, Branch, NewState2),
		    NewState4 = cancel_pending_if_invite_2xx_or_6xx(RMethod, Status, NewState3),
		    NewState4
	    end,
	    {ok, NewState1};

	{clienttransaction_terminating, {Branch, ClientPid}} ->
	    NewState1 = case targetlist:get_using_branch(Branch, Targets) of
		none ->
		    logger:log(error, "sipproxy: Received 'clienttransaction_terminating' (from Pid ~p) from an unknown Target (branch ~p), ignoring.",
		    			[ClientPid, Branch]),
		    State;
		ThisTarget ->
		    NewTarget1 = targetlist:set_state(ThisTarget, terminated),
		    NewTargets = targetlist:update_target(NewTarget1, Targets),
		    logger:log(debug, "sipproxy: Received notification that branch ~p PID ~p has terminated. Targetlist :~n~p", 
		    		[Branch, ClientPid, targetlist:debugfriendly(NewTargets)]),
		    State#state{targets=NewTargets}
	    end,
	    {ok, NewState1};
	    	    	
	{showtargets} ->
	    logger:log(debug, "sipproxy: Received 'showtargets' request, debugfriendly(TargetList) :~n~p",
    			[targetlist:debugfriendly(State#state.targets)]),
    	    {ok, State};
    	    
	Msg ->
	    logger:log(error, "sipproxy: Received unknown message ~p, ignoring", [Msg]),
	    {error, State}
    after
	Time * 1000 ->
	    logger:log(debug, "sipproxy: This wait's time (~p seconds) is over", [Time]),
	    {quit, State}
    end,
    case Res of
	quit ->
	    State#state.targets;
	_ ->
	    NewState_1 = report_upstreams_statemachine(NewState),
	    TimeLeft = lists:max([NewState_1#state.endtime - util:timestamp(), 0]),
	    AllTerminated = allterminated(NewState_1),
	    EndProcessing = end_processing(NewState_1),
	    logger:log(debug, "sipproxy: State changer, MyState=~p, NewMyState=~p, TimeLeft=~p, FinalResponseSent=~p, AllTerminated=~p, EndProcessing=~p",
	    		[State#state.mystate, NewState_1#state.mystate, TimeLeft, NewState_1#state.final_response_sent, AllTerminated, EndProcessing]),
	    process_wait(EndProcessing, NewState_1)
    end.

try_next_destination(Response, Target, Targets, State) when record(Response, response) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    case Status of
	503 ->
	    case State#state.mystate of
		calling ->
		    case targetlist:extract([dstlist], Target) of
			[[FirstDst]] ->
			    logger:log(debug, "sipproxy: Received ~p ~s, but there are no more destinations to try for this target",
					[Status, Reason]),
			    Targets;
			[[FailedDst | DstList]] ->
			    [Branch] = targetlist:extract([branch], Target),
			    NewBranch = get_next_target_branch(Branch),
			    logger:log(debug, "sipproxy: Received ~p ~s, starting new branch ~p for next destination",
					[Status, Reason, NewBranch]),
			    [Request, Timeout] = targetlist:extract([request, timeout], Target),
			    [FirstDst | _] = DstList,
			    % XXX ideally we should not try to contact the same host over UDP, when
			    % we receive a transport layer error for TCP, if there were any other
			    % equally preferred hosts in the SRV response for a destination.
			    % It makes more sense to try to connect to Proxy B over TCP/UDP than to
			    % try Proxy A over UDP when Proxy A over TCP has just failed.
			    NewTargets = case transactionlayer:start_client_transaction(Request, none, FirstDst, NewBranch, Timeout, self()) of
			    	BranchPid when pid(BranchPid) ->
				    targetlist:add(NewBranch, Request, BranchPid, calling, Timeout, DstList, Targets);
				{error, E} ->
				    logger:log(error, "sipproxy: Failed starting client transaction : ~p", [E]),
				    Targets
			    end,
			    NewTargets
		    end;
		S ->
		    logger:log(debug, "sipproxy: Received ~p ~s but not trying next destination when I'm in state ~p",
			[Status, Reason, S]),
		    Targets
	    end;
	_ ->
	    Targets
    end.

get_next_target_branch(In) ->
    case string:rchr(In, $.) of
	0 ->
	    In ++ ".1";
	Index when integer(Index) ->
	    Rhs = string:substr(In, Index + 1),
	    case util:isnumeric(Rhs) of
		true ->
		    Lhs = string:substr(In, 1, Index),
		    Lhs ++ integer_to_list(list_to_integer(Rhs) + 1);
	    	_ ->
	    	    In ++ ".1"
	    end
    end.
    
allterminated(State) when record(State, state) ->
    allterminated(State#state.targets);
    
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

end_processing(State) when record(State, state), State#state.mystate == stayalive ->
    Now = util:timestamp(),
    EndTime = State#state.endtime,
    if
	Now =< EndTime -> false;
	true -> true
    end;
end_processing(State) when record(State, state), State#state.mystate == completed ->
    true;
end_processing(State) when record(State, state) ->
    allterminated(State).

% We need to forward 2xx responses to INVITE to upstream even if we are 'cancelled'.

%%
%% final_response_sent == false, mystate == calling or cancelled
%%
report_upstreams_statemachine(State) when record(State, state), State#state.final_response_sent == false,
					State#state.mystate == calling; State#state.mystate == cancelled ->
    case allterminated(State) of
	true ->
	    Responses = targetlist:get_responses(State#state.targets),
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
			R503 when record(R503, response), R503#response.status == 503 ->
			    %% RFC3261 16.7 (Choosing the best response) bullet 6 says that if
			    %% we only have a 503, we should send a 500 instead
			    logger:log(normal, "sipproxy: Turning best response '503 ~s' into a '500 Only response to fork was 503'",
				       [R503#response.reason]),
			    {500, "Only response to fork was 503"};
			FinalResponse when record(FinalResponse, response) ->
			    {Status, Reason} = {FinalResponse#response.status, FinalResponse#response.reason},
			    logger:log(debug, "sipproxy: Picked response '~p ~s' from the list of responses available (~p).",
			    		[Status, Reason, printable_responses(Responses)]),
			    FinalResponse
		    end
	    end,
	    Parent = State#state.parent,
	    logger:log(debug, "sipproxy: Sending the result to parent Pid ~p.", [Parent]),
	    util:safe_signal("sipproxy :", Parent, {all_terminated, ForwardResponse}),
	    State#state{mystate=completed, final_response_sent=true};
	false ->
	    State
    end;

%%
%% final_response_sent == true, or mystate is not calling or cancelled
%%
report_upstreams_statemachine(State) when record(State, state) ->
    State.

check_forward_immediately(Request, Response, Branch, State) when record(Request, request), record(Response, response), record(State, state) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    {Status, Reason} = {Response#response.status, Response#response.reason},
    case forward_immediately(Method, Status, State) of
	{true, NewState} ->
	    Parent = NewState#state.parent,
	    logger:log(debug, "sipproxy: Forwarding response '~p ~s' (in response to ~s ~s) to my parent (PID ~p) immediately",
	    		[Status, Reason, Method, sipurl:print(URI), Parent]),
	    %% RFC 3261 16.7 bullet 5 (Check response for forwarding) says we MUST process any
	    %% response chosen for immediate forwarding as described in
	    %% "Aggregate Authorization Header Field Values" through "Record-Route".
	    %% This makes sense if it is an 401 or an 407 that forward_immediately() tells us
	    %% to forward immediately - otherwise this is a no-op.
	    Targets = NewState#state.targets,
	    Responses = targetlist:get_responses(Targets),
	    FwdResponse = aggregate_authreqs(Response, Responses),
	    util:safe_signal("sipproxy :", Parent, {sipproxy_response, self(), Branch, FwdResponse}),
	    NewState;
	_ ->
	    logger:log(debug, "sipproxy: Not forwarding response ~p ~s (in response to ~s ~s) immediately",
	    		[Status, Reason, Method, sipurl:print(URI)]),
	    State
    end.

% A 100 Trying is considered an illegal response here since it is hop-by-hop and should
% be 'filtered' at the transaction layer.
forward_immediately(Method, Status, State) when record(State, state), Status =< 100 ->
    logger:log(error, "sipproxy: Invalid response ~p to ~s", [Status, Method]),
    {false, State};
forward_immediately(Method, Status, State) when record(State, state), Status =< 199 ->
    {true, State};
% 2xx responses to INVITE are always forwarded immediately, regardless of final_response_sent
forward_immediately("INVITE", Status, State) when record(State, state), Status =< 299 ->
    NewState = State#state{mystate=completed, final_response_sent=true},
    {true, NewState};
forward_immediately(Method, Status, State) when record(State, state), State#state.final_response_sent /= true, Status =< 299 ->
    NewState = State#state{mystate=completed, final_response_sent=true},
    {true, NewState};
forward_immediately(Method, Status, State) when record(State, state) ->
    {false, State}.

cancel_pending_if_invite_2xx_or_6xx("INVITE", Status, State) when record(State, state), Status =< 199 ->
    State;
cancel_pending_if_invite_2xx_or_6xx("INVITE", Status, State) when record(State, state), Status =< 299 ->
    logger:log(debug, "sipproxy: Cancelling pending targets since one INVITE transaction resulted in a 2xx response ~p", [Status]),
    NewTargets = cancel_pending_targets(State#state.targets),
    State#state{targets=NewTargets};
cancel_pending_if_invite_2xx_or_6xx(Method, Status, State) when record(State, state), Status =< 599 ->
    State;
cancel_pending_if_invite_2xx_or_6xx(Method, Status, State) when record(State, state), Status =< 699 ->
    logger:log(debug, "sipproxy: Cancelling pending targets since one branch resulted in a 6xx response ~p", [Status]),
    NewTargets = cancel_pending_targets(State#state.targets),
    State#state{targets=NewTargets}.

printable_responses([]) ->
    [];
printable_responses([H | Rest]) when record(H, response) ->
    lists:append([integer_to_list(H#response.status) ++ " " ++ H#response.reason], printable_responses(Rest));
printable_responses([Response | Rest]) ->
    lists:append([Response], printable_responses(Rest)).

% look for 6xx responses, then pick the lowest but prefer
% 401, 407, 415, 420 and 484 if we choose a 4xx and avoid
% 503 if we choose a 5xx.
make_final_response(Responses) ->
    TwoxxResponses = get_xx_responses(200, Responses),
    ThreexxResponses = get_xx_responses(300, Responses),
    FourxxResponses = get_xx_responses(400, Responses),
    FivexxResponses = get_xx_responses(500, Responses),
    SixxxResponses = get_xx_responses(600, Responses),
    if
	SixxxResponses /= [] ->
	    pick_response(6, SixxxResponses);
	TwoxxResponses /= [] ->
	    pick_response(2, TwoxxResponses);
	ThreexxResponses /= [] ->
	    pick_response(3, ThreexxResponses);
	FourxxResponses /= [] ->
	    BestFourxx = pick_response(4, FourxxResponses),
	    aggregate_authreqs(BestFourxx, Responses);
	FivexxResponses /= [] ->
	    pick_response(5, FivexxResponses);
	true ->
	    logger:log(debug, "Appserver: No response to my liking"),
	    none
    end.	    

aggregate_authreqs(BestResponse, Responses) when record(BestResponse, response) ->
    {Status, Reason, Header, Body} = {BestResponse#response.status, BestResponse#response.reason,
				      BestResponse#response.header, BestResponse#response.body},
    case Status of
	407 ->
	    ProxyAuth = collect_auth_headers(Status, "Proxy-Authenticate", Responses),
	    logger:log(debug, "Aggregating ~p Proxy-Authenticate headers into response '407 ~s'",
		       [length(ProxyAuth), Reason]),
	    NewHeader = keylist:set("Proxy-Authenticate", ProxyAuth, Header),
	    BestResponse#response{header=NewHeader};
	401 ->
	    WWWAuth = collect_auth_headers(Status, "WWW-Authenticate", Responses),
	    logger:log(debug, "Aggregating ~p WWW-Authenticate headers into response '401 ~s'",
		       [length(WWWAuth), Reason]),
	    NewHeader = keylist:set("WWW-Authenticate", WWWAuth, Header),
	    BestResponse#response{header=NewHeader};
	_ ->
	    BestResponse
    end.

collect_auth_headers(Status, Key, Responses) ->
    collect_auth_headers2(Status, Key, Responses, []).
    
collect_auth_headers2(Status, Key, [], Res) ->
    Res;
collect_auth_headers2(Status, Key, [H | T], Res) when record(H, response), H#response.status == Status ->
    NewRes = lists:append(Res, keylist:fetch(Key, H#response.header)),
    collect_auth_headers2(Status, Key, T, NewRes);
collect_auth_headers2(Status, Key, [H | T], Res) ->
    collect_auth_headers2(Status, Key, T, Res).

pick_response(4, FourxxResponses) ->
    lists:nth(1, lists:sort(fun pick_4xx_sort/2, FourxxResponses));
pick_response(5, FivexxResponses) ->
    lists:nth(1, lists:sort(fun avoid_503_sort/2, FivexxResponses));
pick_response(Nxx, Responses) ->
    lists:nth(1, Responses).

pick_4xx_sort(A, B) when record(A, response), record(B, response) ->
    Apref = is_4xx_preferred(A),
    Bpref = is_4xx_preferred(B),
    if
	Apref == Bpref ->
	    % A and B is either both in the preferred list, or both NOT in the preferred list.
	    % Pick using numerical sort of response codes.
	    if
		A#response.status =< B#response.status ->
		   true;
		true ->
		   false
	    end;	    
	Apref == true ->
	    true;
	true ->
	    false
    end.
    
is_4xx_preferred(Response) when record(Response, response) ->
    lists:member(Response#response.status, [401, 407, 415, 420, 484]).

avoid_503_sort(A, _) when record(A, response), A#response.status == 503 ->
    false;
avoid_503_sort(A, B) when record(A, response), record(B, response), A#response.status =< B#response.status ->
    true;
avoid_503_sort(_, _) ->
    false.
    
get_xx_responses(Min, Responses) ->
    Max = Min + 99,
    lists:keysort(1, get_range_responses(Min, Max, Responses)).
    
get_range_responses(Min, Max, []) ->
    [];
get_range_responses(Min, Max, [H | T]) when record(H, response) ->
    if
	H#response.status < Min -> get_range_responses(Min, Max, T);
	H#response.status > Max -> get_range_responses(Min, Max, T);
	true ->
	    lists:append([H], get_range_responses(Min, Max, T))
    end.
