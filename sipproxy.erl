-module(sipproxy).
-export([fork/9, process_request/7, process_branch_loop/7, start_UAS/9, generate_branch/0, get_branch/1]).

% This is the start() function of sipproxy. sipproxy can presently
% accomplish 'stateless forking'. It can follow an Actions list and
% fork a request to several Targets.
%
% You start it by calling the start() method with a set of Actions. Currenly
% supported actions are a list of 'call' and 'wait'. You can mix calls and
% waits freely.
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

cancel_targets(Targets) ->
    % send {cancel} to all PIDs in states other than completed
    NewTargets1 = cancel_targets_state(Targets, calling),
    cancel_targets_state(NewTargets1, proceeding).

cancel_targets_state(Targets, State) ->
    % send {cancel} to all PIDs in a specific State
    lists:map(fun ({Branch, BranchURI, Pid, BranchState}) ->
    			case BranchState of
    			    State ->
				Pid ! {cancel, "cancel_targets_state"},
				{Branch, BranchURI, Pid, completed};
			    _ ->
				{Branch, BranchURI, Pid, BranchState}
			end
	      end, Targets).

cancel_targets_except(BranchList, Targets) ->
    lists:map(fun ({Branch, BranchURI, Pid, BranchState}) ->
    			case util:casegrep(Branch, BranchList) of
			    true ->
				{Branch, BranchURI, Pid, BranchState};
    			    _ ->
    			        % Only send cancel if in state 'calling' or 'proceeding'
				case BranchState of
				    calling ->
					Pid ! {cancel, "cancel_targets_except"},
					{Branch, BranchURI, Pid, completed};
				    proceeding ->
					Pid ! {cancel, "cancel_targets_except"},
					{Branch, BranchURI, Pid, completed};
				    _ ->
					{Branch, BranchURI, Pid, BranchState}
				end,
				{Branch, BranchURI, Pid, completed}
			end
	      end, Targets).

% Spawn a new erlang process and make it send something (INVITE, SUBSCRIBE
% etc) through the process_request() function. process_request() will go
% into a process_branch_loop() loop to resend stuff until asked to stop.
start_branch(Socket, Method, ReqURI, Timeout, URI, Header, Body, FromIP, Parent) ->
    Branch = generate_branch(),
    LogStr = sipserver:make_logstr({request, Method, ReqURI, Header, Body}, FromIP),
    Pid = spawn(?MODULE, process_request, [Method, Socket, Timeout, URI, Branch,
					   Header, Body]),
    logger:log(normal, "~s: (new pid ~p, parent ~p) ~s -> Fork to ~s", [Branch, Pid, Parent, LogStr, sipurl:print(URI)]),
    {Branch, URI, Pid, calling}.

% This is the same as start_branch() with the exception that we don't send anything
% until requested to.
start_UAS(Branch, Method, Socket, ReqURI, Timeout, URI, Header, Body, FromIP) ->
    LogStr = sipserver:make_logstr({request, Method, ReqURI, Header, Body}, FromIP),
    Pid = spawn(?MODULE, process_branch_loop, [Socket, ReqURI, Branch, Header, Body, [], {idle, 0, none}]),
    logger:log(normal, "~s -> Started UAS (PID ~p)", [LogStr, Pid]),
    Pid.

stop_timers(_, _, _, []) ->
    true;
stop_timers(Branch, response, Parameters, TimerList) ->
    {Status, Reason} = Parameters,
    logger:log(normal, "~s: Stopping timers - response ~p ~s", [Branch, Status, Reason]),
    lists:map(fun (TRef) ->
		      timer:cancel(TRef)
		end, TimerList);
stop_timers(Branch, request, Parameters, TimerList) ->
    logger:log(normal, "~s: Stopping timers - request ~p", [Branch, Parameters]),
    lists:map(fun (TRef) ->
		      timer:cancel(TRef)
		end, TimerList).

% Send a SIP-message and arrange for it to be resent as per the SIP specification.
% This function is called as a start of a new branch.
% The first send_after() causes the message to be resent,
% the second send_after() will cause us to stop resending this message and
% the third send_after() will start sending CANCELs to this request, if reached.
% Start looping process_branch_loop() for this branch.
process_request(Method, Socket, Timeout, URI, Branch, Header, Body) ->
    logger:log(debug, "~s: process_request() Sending ~s -> ~s", [Branch, Method, sipurl:print(URI)]),
    siprequest:send_proxy_request(Header, Socket,
				  {Method, URI, Body, ["branch=" ++ Branch]}),
    TimerA = 500,
    TimerB = TimerA * 64,
    {ok, ResendTimer} = timer:send_after(TimerA, {timer, timerA, Method, {Header, Body}}),
    {ok, StopResendTimer} = timer:send_after(TimerB, {stopresend}),
    TimerList = case Timeout of
	none ->
	    [ResendTimer, StopResendTimer];
	_ ->
	   {ok, TT} = timer:send_after(Timeout * 1000, {cancel, "process_request " ++ Method ++ " timeout"}),
	   [ResendTimer, StopResendTimer, TT]
    end,
    process_branch_loop(Socket, URI, Branch, Header, Body, TimerList,
			{request, TimerA * 2, Method}).

% This is the main loop for a branch. Receive messages and act on them.
process_branch_loop(Socket, URI, Branch, Header, Body, Timers, {Type, TimerA, Parameters}) ->
    receive
	{cancel, Msg} ->
	    logger:log(debug, "~s: Branch requested to 'cancel' (~s)", [Branch, Msg]),
	    stop_timers(Branch, Type, Parameters, Timers),
	    logger:log(normal, "~s: Sending CANCEL -> ~p", [Branch, sipurl:print(URI)]),
	    process_branch_cancel(Socket, URI, Branch, Header, Body);
	{ack, AckHeader, Status, Reason} ->
	    stop_timers(Branch, Type, Parameters, Timers),
	    logger:log(normal, "~s: Sending ACK -> ~s", [Branch, sipurl:print(URI)]),
	    process_branch_ack(Socket, URI, Branch, Header, "", AckHeader, Status, Reason);

	{forwardresponse, ResponseHeader, ResponseBody, Status, Reason} ->
	    stop_timers(Branch, Type, Parameters, Timers),
	    logger:log(normal, "~s: Forwarding ~p ~s -> ~s", [Branch, Status, Reason, sipurl:print(URI)]),
	    process_forwardresponse(Socket, URI, Branch, Header, Body, ResponseHeader, ResponseBody, Status, Reason);
	{sendresponse, Status, Reason} ->
	    stop_timers(Branch, Type, Parameters, Timers),
	    logger:log(normal, "~s: Sending ~p ~s -> ~s", [Branch, Status, Reason, sipurl:print(URI)]),
	    process_sendresponse(Socket, URI, Branch, Header, "", Status, Reason);

	{timer, timerA, sendresponse, Parameters} ->
	    {ResponseHeader, ResponseBody, Status, Reason} = Parameters,
	    logger:log(normal, "~s: Resend after ~p (response -> ~s): Sending ~p ~s",
	    	       [Branch, TimerA, sipurl:print(URI), Status, Reason]),
	    timer:send_after(TimerA, {timer, timerA, response, Parameters}),
	    siprequest:send_proxy_response(Socket, Status, Reason,
					   ResponseHeader, ResponseBody),
	    process_branch_loop(Socket, URI, Branch, Header, Body, Timers,
				{response, TimerA * 2, Parameters});

	{timer, timerA, Method, Parameters} ->
	    {ReqHeader, ReqBody} = Parameters,
	    logger:log(normal, "~s: Resend after ~p (request -> ~s): resending ~s",
	    	       [Branch, TimerA, sipurl:print(URI), Method]),
	    timer:send_after(TimerA, {timer, timerA, Method, Parameters}),
	    siprequest:send_proxy_request(ReqHeader, Socket,
					  {Method, URI, ReqBody,
					   ["branch=" ++ Branch]}),
	    process_branch_loop(Socket, URI, Branch, Header, Body, Timers,
				{Method, TimerA * 2, Parameters});

	{stopresend} ->
	    stop_timers(Branch, Type, Parameters, Timers),
	    process_branch_loop(Socket, URI, Branch, Header, Body, [], {Type, TimerA, Parameters});
	{quit} ->
	    stop_timers(Branch, Type, Parameters, Timers),
	    logger:log(normal, "~s: handling ~s: quitting branch",
	    	       [Branch, sipurl:print(URI)]),
	    true
    end.

% This branches dialogue should be cancelled.
process_branch_cancel(Socket, URI, Branch, Header, Body) ->
    {CSeq, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    CancelHeader = keylist:set("CSeq", [sipheader:cseq_print({CSeq, "CANCEL"})],
			    Header),
    logger:log(debug, "~s: process_branch_cancel() Sending ~s -> ~s", [Branch, "CANCEL", sipurl:print(URI)]),
    siprequest:send_proxy_request(CancelHeader, Socket,
				  {"CANCEL", URI, Body, ["branch=" ++ Branch]}),
    TimerA = 500,
    TimerB = TimerA * 64,
    {ok, ResendTimer} = timer:send_after(TimerA, {timer, timerA, "CANCEL", {Header, Body}}),
    {ok, StopResendTimer} = timer:send_after(TimerB, {stopresend}),
    {ok, TT} = timer:send_after(64 * 1000, {quit}),
    TimerList = [ResendTimer, StopResendTimer, TT],
    process_branch_loop(Socket, URI, Branch, Header, Body, TimerList,
			{request, TimerA * 2, "CANCEL"}).

% Send an ACK, requires some special processing to figure out where to
% direct it.
process_branch_ack(Socket, URI, Branch, Header, Body, AckHeader, Status, Reason) ->
    {CSeqID, CSeqMethod} = sipheader:cseq(keylist:fetch("CSeq", AckHeader)),
    ExtraHeaders = [{"CSeq", sipheader:cseq_print({CSeqID, "ACK"})}], 
    SendHeader = keylist:appendlist(keylist:copy(Header,
						 ["Via", "From", "To",
						  "Call-ID"]),
				    ExtraHeaders),
    process_request("ACK", Socket, none, URI, Branch, SendHeader, Body).

% This branch should answer something.
process_sendresponse(Socket, URI, Branch, Header, Body, Status, Reason) ->
    logger:log(debug, "~s: process_sendresponse() Sending ~p ~s -> ~s", [Branch, Status, Reason, sipurl:print(URI)]),
    siprequest:send_result(Header, Socket, Body, Status, Reason),
    TimerA = 500,
    TimerB = TimerA * 64,
    {ok, ResendTimer} = timer:send_after(TimerA, {timer, timerA, sendresponse, {Header, Body, Status, Reason}}),
    {ok, StopResendTimer} = timer:send_after(TimerB, {stopresend}),
    % XXX what is the right thing to do when noone answers our responses? send CANCEL?
    process_branch_loop(Socket, URI, Branch, Header, Body,
    			[ResendTimer, StopResendTimer],
			{response, TimerA * 2, {Status, Reason}}).

% This branch should forward an answer.
process_forwardresponse(Socket, URI, Branch, Header, Body, ResHeader, ResBody, Status, Reason) ->
    siprequest:send_proxy_response(Socket, Status, Reason,
				   ResHeader, ResBody),
    process_branch_loop(Socket, URI, Branch, Header, Body,
    			[], {response, 0, {Status, Reason}}).

generate_branch() ->
    {Megasec, Sec, Microsec} = now(),
    "z9hG4bK-yxa-" ++ hex:to(Sec, 8) ++ hex:to(Microsec, 8).

response_action(Response, calling) when Response == 100 ->
    logger:log(debug, "Decision: Received 100, going from 'calling' to 'proceeding'"),
    {ignore, proceeding, none};
response_action(Response, BranchState) when Response == 100 ->
    logger:log(debug, "Decision: Received 100, always ignoring (current branch state ~p)", [BranchState]),
    {ignore, BranchState, none};

response_action(Response, calling) when Response =< 199 ->
    % This was the first provisional response received, forward it to the originator.
    logger:log(debug, "Decision: Received first 1xx ~p, branch = calling -> enter proceeding", [Response]),
    {ignore, proceeding, none};
response_action(Response, BranchState) when Response =< 199 ->
    % This was NOT the first provisional response, just ignore it.
    logger:log(debug, "Decision: Received 1xx ~p when in state ~p, ignoring", [Response, BranchState]),
    {ignore, BranchState, none};

response_action(Response, calling) when Response =< 299 ->
    % We have a winner! This branch received the first 2xx response.
    logger:log(debug, "Decision: Received 2xx ~p, branch = calling, complete this branch", [Response]),
    {ignore, completed, cancel};
response_action(Response, proceeding) when Response =< 299 ->
    % We have a winner! This branch received the first 2xx response.
    logger:log(debug, "Decision: Received 2xx ~p, branch = proceeding, complete this branch", [Response]),
    {ignore, completed, cancel};
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
    logger:log(debug, "Decision: Received 4xx or 5xx ~p -> ACK and change this branches state from '~p' to 'terminated'", [Response, BranchState]),
    {ack, terminated, none};

response_action(Response, BranchState) when Response =< 699 ->
    % Global failure, ack and finish all branches.
    logger:log(debug, "Decision: Received 6xx ~p -> ACK (Branch state was '~p')", [Response, BranchState]),
    {sendandack, terminated, cancel}.

find_target(Branch, Targets) ->
    case lists:keysearch(Branch, 1, Targets) of
	false ->
	    none;
	{value, {_, BranchURI, Pid, BranchState}} ->
	    {BranchURI, Pid, BranchState}
    end.

fork(Method, Parent, ReqURI, Header, Body, FromIP, Socket, Actions, Targets) ->
    case Actions of
	[{call, Timeout, URI} | Rest] ->
	    logger:log(debug, "sipproxy: calling ~s with timeout ~p",
		      [sipurl:print(URI), Timeout]),
	    NewTargets = [start_branch(Socket, Method, ReqURI, Timeout, URI, Header, Body, FromIP, Parent)
			  | Targets],
	    fork(Method, Parent, ReqURI, Header, Body, FromIP, Socket, Rest, NewTargets);
	[{wait, Time} | Rest] ->
	    logger:log(normal, "sipproxy: waiting ~p seconds", [Time]),
	    case process_wait(Parent, ReqURI, Header, Body, Socket, Targets,
			      false, util:timestamp() + Time) of
		{discontinue, NewTargets} ->
		    logger:log(debug, "sipproxy: discontinue processing"),
		    fork(Method, Parent, ReqURI, Header, Body, FromIP, Socket, [], NewTargets);
		NewTargets ->
		    fork(Method, Parent, ReqURI, Header, Body, FromIP, Socket, Rest, NewTargets)
	    end;
	[] ->
	    case allterminated(Targets) of
		true ->
		    logger:log(normal, "sipproxy: All targets are terminated"),
		    Parent ! {all_terminated};
		_ ->
		    logger:log(debug, "sipproxy: Reached end of Actions-list"),
		    cancel_targets(Targets),
		    Parent ! {no_more_actions}
	    end,
	    logger:log(normal, "sipproxy: waiting 32 seconds for final responses"),
	    process_wait(Parent, ReqURI, Header, Body, Socket, Targets,
			 false, util:timestamp() + 32),
	    Parent ! {callhandler_terminating}
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
		    logger:log(debug, "~s: Rolling the dice, response was ~p and state is ~p", [Branch, Response, BranchState]),
		    {BranchAction, NewBranchState, OtherBranchesAction} =
			response_action(Response, BranchState),
		    case NewBranchState of
			BranchState ->
			    % No update of Targets required.
			    {BranchAction, NewBranchState, OtherBranchesAction, Targets};
			_ ->
			    logger:log(debug, "~s: Changing BranchState from '~p' to '~p', OtherBranchesAction '~p'", 
					[Branch, BranchState, NewBranchState, OtherBranchesAction]),
			    NewTargets = lists:keyreplace(Branch, 1, Targets, {Branch, BranchURI, BranchPid, NewBranchState}),
			    logger:log(debug, "sipproxy: NewTargets list :~n~p", [NewTargets]),
			    {BranchAction, NewBranchState, OtherBranchesAction, NewTargets}
		    end
	    end;
	_ ->
	    logger:log(debug, "~s: Ignoring ~p response to ~p (not response to INVITE)", [Branch, Response, Method]),
	    {ignore, none, none, Targets}
    end.

process_response(Branch, Status, Reason, Header, Socket, ResponseHeader, ResponseBody, ReqURI, Targets) ->
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", ResponseHeader)),
    logger:log(normal, "~s: Response to ~s: ~p ~s", [Branch, Method, Status, Reason]),
    {BranchURI, BranchPid, BranchState} = find_target(Branch, Targets),
    BranchPid ! {stopresend},
    {BranchAction, NewBranchState, OtherBranchesAction, NewTargets1} =
	evaluate_response(Targets, ResponseHeader, Status),
    logger:log(debug, "~s: ~p ~s BranchPid ~p, Action ~p", [Branch, Status, Reason, BranchPid, BranchAction]),
    case BranchAction of
	ignore ->
	    true;
	ack ->
	    BranchPid ! {ack, ResponseHeader, Status, Reason};
	quit ->
	    logger:log(debug, "~s: terminating PID ~p", [Branch, BranchPid]),
	    BranchPid ! {quit}
    end,
    NewTargets1.

allterminated([]) ->
    true;
allterminated([{_, _, _, terminated} | Rest]) ->
    allterminated(Rest);
allterminated([_ | Rest]) ->
    false.

process_wait(Parent, ReqURI, Header, Body, Socket, Targets, true, AbsTime) ->
    logger:log(debug, "sipproxy: All Targets terminated. Ending process_wait(), returning discontinue."),
    {discontinue, Targets};
process_wait(Parent, ReqURI, Header, Body, Socket, Targets, false, AbsTime) ->
    Time = lists:max([AbsTime - util:timestamp(), 0]),
    receive
	{cancel} ->
	    logger:log(debug, "sipproxy: process_wait() received 'cancel', calling cancel_targets()"),
	    cancel_targets(Targets);
	{cancel_all_branches_except, BranchList} ->
	    cancel_targets_except(BranchList, Targets);
	{siprequest, "CANCEL", {_, CHeader, _}} ->
	    Branch = get_branch(CHeader),
	    logger:log(error, "sipproxy: A CANCEL? Of one of my branches? Ignoring (Branch: ~p)", [Branch]),
	    process_wait(Parent, ReqURI, Header, Body, Socket, 
	    		 Targets, allterminated(Targets), AbsTime);
	{siprequest, "ACK", {AckURI, AckHeader, AckBody}} ->
	    {_, BranchPid, BranchState} = find_target(get_branch(AckHeader), Targets),
	    BranchPid ! {stopresend},
	    case BranchState of
		terminated ->
		    logger:log(debug, "~s: Received ACK when 'terminated' - that means we can RIP now"),
		    BranchPid ! {quit};
		_ ->
		    logger:log(debug, "~s: Received ACK, stopped any resends but otherwise ignoring")
	    end,
	    process_wait(Parent, ReqURI, Header, Body, Socket, 
	    		 Targets, allterminated(Targets), AbsTime);
	{siprequest, "BYE", {ByeURI, ByeHeader, ByeBody}} ->
	    Branch = get_branch(ByeHeader),
	    logger:log(error, "sipproxy: A BYE? Of one of my branches? Ignoring (Branch: ~p)", [Branch]),
	    process_wait(Parent, ReqURI, Header, Body, Socket, 
	    		 Targets, allterminated(Targets), AbsTime);
	{response, {Status, Reason, ResponseHeader, ResponseBody}} ->
	    Branch = get_branch(ResponseHeader),
	    % Always signal parent when we receive a response. Parent is responsible for
	    % filtering these.
	    logger:log(debug, "sipproxy: Notifying parent ~p of response ~p ~s to branch ~p", [Parent, Status, Reason, Branch]),
    	    Parent ! {response, Branch, {Status, Reason, ResponseHeader, ResponseBody}},
	    NewTargets = case find_target(Branch, Targets) of
		none ->
		    logger:log(error, "sipproxy: No target found for branch ~p response ~p ~s", [Branch, Status, Reason]),
		    Targets;
	    _ ->
		    process_response(Branch, Status, Reason, Header, Socket,
				     ResponseHeader, ResponseBody, ReqURI, Targets)
	    end,
	    process_wait(Parent, ReqURI, Header, Body, Socket,
			 NewTargets, allterminated(NewTargets), AbsTime)
    after
	Time * 1000 ->
	    logger:log(debug, "sipproxy: This wait's time (~p seconds) is over", [Time]),
	    Targets
    end.
