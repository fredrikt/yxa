-module(sipproxy).
-export([start/5, process_branch/6]).

get_actions(URI) ->
    [
     {call, 30, sipurl:parse("sip:000705778022@sip-pstn.kth.se")},
     {call, 30, sipurl:parse("sip:1002@kth.se")},
     {wait, 40}].

start(Method, URI, Header, Body, Socket) ->
    siprequest:send_result(Header, Socket, "", 100, "Trying"),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:insert_call_unique(CallID, proxy, Header,
					  self()) of
	{atomic, ok} ->
	    process(CallID, Header, Body, Socket, get_actions(URI),
		    [], initial);
	{aborted, key_exists} ->
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
    case lists:keysearch(Branch, 1, Targets) of
	false ->
	    true;
	{value, {_, Pid, Status}} ->
	    Pid ! {stopresend, Method}
    end.

cancel_others(Targets, Header) ->
    MyBranch = get_branch(Header),
    logger:log(debug, "Cancel others: my branch is ~s", [MyBranch]),

    lists:map(fun ({Branch, Pid, _}) ->
		      logger:log(debug, "Cancel others: trying branch ~s", [Branch]),
		      case Branch of
			  MyBranch ->
			      logger:log(debug, "Cancel others: was my branch", []),
			      true;
			  _ ->
			      logger:log(debug, "Cancel others: wasn't my branch", []),
			      Pid ! {cancel}
		      end
	      end, Targets).

process_branch(Socket, Timeout, URI, Branch, Header, Body) ->
    logger:log(normal, "Forked INVITE ~p", [URI]),
    siprequest:send_proxy_request(Header, Socket,
				  {"INVITE", URI, Body, ["branch=" ++ Branch]}),
    TimerA = 500,
    TimerB = TimerA * 64,
    timer:send_after(TimerA, {timer, timerA, invite}),
    timer:send_after(TimerB, {stopresend, "INVITE"}),
    timer:send_after(Timeout * 1000, {cancel}),
    process_branch_loop(Socket, URI, Branch, Header, Body,
			{invite, TimerA * 2}).

process_cancel(Socket, URI, Branch, Header, Body) ->
    logger:log(normal, "Forked CANCEL ~p", [URI]),
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
	    logger:log(debug, "Resend(~p): doing cancel", [URI]),
	    process_cancel(Socket, URI, Branch, Header, Body);
	{stopresend, "INVITE"} ->
	    logger:log(debug, "Resend(~p): stopping INVITE", [URI]),
	    process_branch_loop(Socket, URI, Branch, Header, Body,
				{none});
	{quit} ->
	    logger:log(debug, "Resend(~p): quitting", [URI]),
	    true;
	{timer, timerA, invite} ->
	    logger:log(debug, "Resend(~p): resending INVITE", [URI]),
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
	    logger:log(debug, "Resend(~p): stopping CANCEL", [URI]),
	    true;
	{quit} ->
	    logger:log(debug, "Resend(~p): quitting", [URI]),
	    true;
	{timer, timerA, cancel} ->
	    logger:log(debug, "Resend(~p): resending CANCEL", [URI]),
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
	    logger:log(debug, "Resend(~p): doing CANCEL", [URI]),
	    process_cancel(Socket, URI, Branch, Header, Body);
	{quit} ->
	    logger:log(debug, "Resend(~p): quitting", [URI]),
	    true
    end.

generate_branch() ->
    {Megasec, Sec, Microsec} = now(),
    "z9hG4bK" ++ hex:to(Sec, 8) ++ hex:to(Microsec, 8).

start_branch(Socket, Timeout, URI, Header, Body) ->
    Branch = generate_branch(),
    {Branch, spawn(?MODULE, process_branch, [Socket, Timeout, URI, Branch,
					     Header, Body]),
     initial}.

response_action(Response, State, _) when Response == 100 ->
    {ignore, State};
response_action(Response, finished, initial) when Response < 199 ->
    {ignore, finished};
response_action(Response, initial, initial) when Response < 199 ->
    {send, initial};
response_action(Response, _, finished) when Response < 199 ->
    {ignore, finished};
response_action(Response, initial, initial) when Response < 299 ->
    {finish, first};
response_action(Response, initial, finished) when Response < 299 ->
    {ignore, finished};
response_action(Response, finished, initial) when Response < 299 ->
    {ignore, finished};
response_action(Response, finished, finished) when Response < 299 ->
    {ignore, finished};
response_action(Response, first, _) when Response < 299 ->
    {send, finished};
response_action(Response, State, _) when Response < 699 ->
    {ignore, finished}.

process_response(Targets, Header, Response, State) ->
    Branch = get_branch(Header),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    case Method of
	"INVITE" ->
	    case lists:keysearch(Branch, 1, Targets) of
		false ->
		    true;
		{value, {_, Pid, BranchState}} ->
		    {Action, NewBranchState} =
			response_action(Response, BranchState, State),
		    {Action, lists:keyreplace(Branch, 1, Targets,
					      {Branch, Pid, NewBranchState})}
	    end;
	_ ->
	    {ignore, Targets}
    end.

process(CallID, Header, Body, Socket, Actions, Targets, finished) ->
    receive
	{response, {Status, ResponseHeader, ResponseBody}} ->
	    logger:log(debug, "Forking: response ~p", [Status]),
	    stop_resend(Targets, ResponseHeader),
	    {Action, Newtargets} =
		process_response(Targets, ResponseHeader, Status, finished),
	    logger:log(debug, "Forking: action ~p newtargets ~p", [Action, Newtargets]),
	    case {Action, allfinished(Targets)} of
		{ignore, false} ->
		    logger:log(debug, "Forking: ignoring", []),
		    process(CallID, Header, Body, Socket, Actions,
			    Newtargets, finished);
		{ignore, true} ->
		    logger:log(debug, "Forking: giving up", []),
		    siprequest:send_proxy_response(Socket, 404, "Not Found",
						   Header,  ""),
		    process(CallID, Header, Body, Socket, Actions,
			    Newtargets, finished);
		{send, _} ->
		    siprequest:send_proxy_response(Socket, Status, "",
						   ResponseHeader,
						   ResponseBody),
		    process(CallID, Header, Body, Socket, Actions,
			    Newtargets, finished);
		{finish, _} ->
		    siprequest:send_proxy_response(Socket, Status, "",
						   ResponseHeader,
						   ResponseBody),
		    cancel_others(Targets, ResponseHeader),
		    process(CallID, Header, Body, Socket, Actions,
			    Newtargets, finished)
	    end
    end;
process(CallID, Header, Body, Socket, Actions, Targets, State) ->
    case Actions of
	[{call, Timeout, URI} | Rest] ->
	    logger:log(debug, "Forking: calling ~s with timeout ~p",
		      [sipurl:print(URI), Timeout]),
	    Newtargets = [start_branch(Socket, Timeout, URI, Header, Body)
			  | Targets],
	    process(CallID, Header, Body, Socket, Rest, Newtargets, State);
	[{wait, Time} | Rest] ->
	    logger:log(debug, "Forking: waiting ~p seconds", [Time]),
	    process_wait(CallID, Header, Body, Socket, Rest,
			 Targets, State, util:timestamp() + Time);
	[] ->
	    % XXX set controlpid to none
	    logger:log(debug, "Forking: done"),
	    lists:map(fun ({Branch, Pid, _}) ->
			      Pid ! {cancel}
		      end, Targets),
	    siprequest:send_proxy_response(Socket, 404, "Not Found",
					   Header,  ""),
	    true
    end.

allfinished([]) ->
    true;
allfinished([{_, _, finished} | Rest]) ->
    allfinished(Rest);
allfinished([_ | Rest]) ->
    false.

process_wait(CallID, Header, Body, Socket, Actions, Targets, State, AbsTime) ->
    Time = lists:max([AbsTime - util:timestamp(), 0]),
    receive
	{cancel, {CancelStatus, CancelHeader, CancelBody}} ->
	    logger:log(normal, "Got CANCEL", []),
	    lists:map(fun ({Branch, Pid, _}) ->
			      Pid ! {cancel}
		      end, Targets),
	    siprequest:send_result(CancelHeader, Socket, "", 200, "OK"),
	    siprequest:send_result(Header, Socket, "", 487,
				   "Request Terminated"),
	    process(CallID, Header, Body, Socket, Actions, Targets,
		    finished);
	{response, {Status, ResponseHeader, ResponseBody}} ->
	    logger:log(debug, "Forking: response ~p", [Status]),
	    stop_resend(Targets, ResponseHeader),
	    {Action, Newtargets} =
		process_response(Targets, ResponseHeader, Status, State),
	    logger:log(debug, "Forking: action ~p newtargets ~p", [Action, Newtargets]),
	    case {Action, allfinished(Targets)} of
		{ignore, false} ->
		    logger:log(debug, "Forking: ignoring", []),
		    process_wait(CallID, Header, Body, Socket, Actions,
				 Newtargets, State, AbsTime);
		{ignore, true} ->
		    logger:log(debug, "Forking: giving up", []),
		    siprequest:send_proxy_response(Socket, 404, "Not Found",
						   Header,  ""),
		    process_wait(CallID, Header, Body, Socket, Actions,
				 Newtargets, finished, AbsTime);
		{send, _} ->
		    siprequest:send_proxy_response(Socket, Status, "",
						   ResponseHeader,
						   ResponseBody),
		    process_wait(CallID, Header, Body, Socket, Actions,
				 Newtargets, State, AbsTime);
		{finish, _} ->
		    siprequest:send_proxy_response(Socket, Status, "",
						   ResponseHeader,
						   ResponseBody),
		    cancel_others(Targets, ResponseHeader),
		    process_wait(CallID, Header, Body, Socket, Actions,
				 Newtargets, State, AbsTime)
	    end
    after
	Time * 1000 ->
	    process(CallID, Header, Body, Socket, Actions, Targets,
		    State)
    end.
