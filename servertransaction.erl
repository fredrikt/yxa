-module(servertransaction).
-export([start/5]).

-record(state, {branch, logtag, socket, report_to, request, response, sipmethod, sipstate, timerlist, mode, my_to_tag}).

start({"ACK", URI, _, _}, Socket, LogStr, RequestFun, Mode) ->
    % XXX better to start transaction and immediately send 500 Server Internal Error?
    % Will require passing ACK to process_received_ack() _after_ checking for resent request.
    logger:log(error, "Server transaction: NOT starting transaction for ACK request (ACK ~s)",
		[sipurl:print(URI)]),
    error;
start(Request, Socket, LogStr, RequestFun, Mode) ->
    {Method, URI, _, _} = Request,
    Branch = siprequest:generate_branch() ++ "-UAS",
    LogTag = Branch ++ " " ++ Method,
    % We start all transactions with SIP-state trying. Even INVITE. This is not exactly
    % as formulated in the RFC (section 17.2.1), but we proceed from trying to proceeding
    % on the first 100 Trying. We do this because we don't want to mandate every application
    % to be transaction stateful.
    MyToTag = generate_tag(),
    MyState = #state{branch=Branch, logtag=LogTag, socket=Socket, request=Request,
			sipmethod=Method, sipstate=trying, timerlist=siptimer:empty(),
			mode=Mode, my_to_tag=MyToTag},
    TransactionPid = sipserver:safe_spawn(fun spawned/2, [MyState, LogStr]),
    TransactionPid.
    
spawned(State, LogStr) when record(State, state) ->
    {Method, URI, _, _} = State#state.request,
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Server transaction: Started new server transaction for request ~s ~s.",
		[LogTag, Method, sipurl:print(URI)]),
    logger:log(normal, "~s: ~s", [LogTag, LogStr]),
    % RFC3261 17.2.1 says the _transaction layer_ MUST generate a 100 Trying in response
    % to an INVITE unless it _knows_ the TU will generate a response within 200 ms. We
    % can't know that.
    NewStateIn = if
	Method == "INVITE", State#state.mode == stateful ->
	    Response = make_response(100, "Trying", "", [], [], State),
	    {ok, NewStateIn1} = do_response(created, Response, State),
	    NewStateIn1;
	true ->
	    State
    end,
    {NewState, Throw} = case catch apply(fun loop/1, [NewStateIn]) of
    	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from servertransaction:loop() :~n~p", [E]),
	    {State, none};
	{siperror, Status, Reason} ->
	    logger:log(debug, "Server transaction threw an exception (~p ~s)",
			[Status, Reason]),
	    {State, {siperror, Status, Reason}};
	{siperror, Status, Reason, ExtraHeaders} ->
	    logger:log(debug, "Server transaction threw an exception (~p ~s)",
			[Status, Reason]),
	    {State, {siperror, Status, Reason, ExtraHeaders}};
	ReturnedState when record(ReturnedState, state) ->
	    {ReturnedState, none}
    end,
    case util:safe_is_process_alive(NewState#state.report_to) of
	{true, R} ->
	    Branch = NewState#state.branch,
	    R ! {servertransaction_terminating, self()};
	{false, undefined} ->
	    % Don't log when we have never had a report_to
	    true;
	{false, R} ->
	    logger:log(debug, "~s: Server transaction orphaned ('~p' not alive) - can't inform parent that I am terminating now",
			[LogTag, R])
    end,
    transactionlayer:transaction_terminating(self()),
    case Throw of
	none ->
	    ok;
	_ ->
	    throw(Throw)
    end.

loop(State) when record(State, state) ->
    LogTag = State#state.logtag,
    OrigRequest = State#state.request,
    {OrigMethod, OrigURI, _, _} = OrigRequest,
    {Res, NewState} = receive

	{sipmessage, FromPid, request, {"ACK", OrigURI, Header, Body}, Socket, LogStr, ServerPid, MsgRef, RequestFun} ->
	    % Received an ACK with a Request-URI that matches our original one
	    NewState1 = case State#state.mode of
		stateful ->
		    ServerPid ! {continue, MsgRef},
		    process_received_ack(State);
		stateless ->
		    logger:log(debug, "~s: Received ACK ~s when stateless, pass to core.",
				[LogTag, sipurl:print(OrigURI)]),
		    ServerPid ! {pass_to_core, MsgRef, RequestFun},
		    State
	    end,
	    {ok, NewState1};

	{sipmessage, FromPid, request, Request, Socket, LogStr, ServerPid, MsgRef, RequestFun} ->
	    {Method, URI, Header, _} = Request,
	    logger:log(debug, "~s: Server transaction received a request, checking if it is a resend (~s ~s)",
			[LogTag, Method, sipurl:print(URI)]),
	    {CSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
	    {_, _, OrigHeader, _} = State#state.request,
	    {OrigCSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", OrigHeader)),
	    if
		Method == OrigMethod, URI == OrigURI, CSeqNum == OrigCSeqNum ->
		    case State#state.response of
			{Status, Reason, RHeader, RBody} ->
			    logger:log(normal, "~s: Received a retransmission of request (~s ~s), resending response ~p ~s",
					[LogTag, Method, sipurl:print(URI), Status, Reason]),
			    ServerPid ! {continue, MsgRef},
			    transportlayer:send_proxy_response(State#state.socket, State#state.response);
			_ ->
			    case State#state.report_to of
				P when pid(P) ->
				    logger:log(normal, "~s: Received a retransmission of request (~s ~s) but I have no response to resend!",
				    		[LogTag, Method, sipurl:print(URI)]),
				    ServerPid ! {continue, MsgRef};
				_ ->
				    logger:log(debug, "~s: Received a resend of original request (~s ~s) - pass to core",
						[LogTag, Method, sipurl:print(URI)]),
				    % It looks like we are a stateless server transaction. Tell ServerPid
				    % to pass this retransmission on to the core.
				    ServerPid ! {pass_to_core, MsgRef, RequestFun}
			    end
		    end;
		true ->
		    logger:log(normal, "~s: Server transaction: Received request is not particulary alike the first one (~p ~s ~s /= ~p ~s ~s). " ++
				"Dropping it on the floor.",
				[LogTag, CSeqNum, Method, sipurl:print(URI), OrigCSeqNum, OrigMethod, sipurl:print(OrigURI)]),
		    ServerPid ! {continue, MsgRef},
		    true
	    end,
	    {ok, State};

	{create_response, FromPid, Status, Reason, ExtraHeaders} ->
	    Response = make_response(Status, Reason, "", ExtraHeaders, [], State),
	    {ok, NewState1} = do_response(created, Response, State),
	    {ok, NewState1};

	{forwardresponse, Response} ->
	    {ok, NewState1} = do_response(forwarded, Response, State),
	    {ok, NewState1};

	{expired} ->
	    logger:log(debug, "Server transaction: Received signal that I am expired, exiting."),
	    {quit, State};

	{siptimer, TRef, TDesc} ->
	    case siptimer:get_timer(TRef, State#state.timerlist) of
	        none ->
	            logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring. LIST ~n~p", [LogTag, TRef, TDesc, State#state.timerlist]),
	            {error, State};
	        Timer ->
		    case process_timer(Timer, State) of
			NewState1 when record(NewState1, state) ->
			    {ok, NewState1};
			Unknown1 ->
			    logger:log(error, "~s: Server transaction: process_timer() returned unknown result :~n~p",
					[LogTag, Unknown1]),
			    {error, State}
		    end
	    end;

	{cancelled, FromPid} ->
	    % XXX check that FromPid is transaction layer?
	    SipState = State#state.sipstate,
	    case lists:member(SipState, [completed, confirmed, terminated]) of
		false ->
		    ReportTo = case util:safe_is_process_alive(State#state.report_to) of
			{true, R} ->
			    R;
			{false, R} ->
			    logger:log(debug, "~s: Server transaction orphaned ('~p' not alive) - can't inform parent that I have been cancelled",
					[LogTag, R])
		    end,
		    case ReportTo of
			ReportTo when pid(ReportTo) ->
			    logger:log(debug, "~s: Server transaction cancelled, telling parent ~p", [LogTag, ReportTo]),
			    % We don't generate the 487 Request Cancelled here, parent does other stuff first and then
			    % tell us to send that response.
			    ReportTo ! {servertransaction_cancelled, self()},
			    {ok, State};
			_ ->
			    % XXX store this and immediately signal that we have been cancelled if someone calls set_report_to on us?
			    logger:log(debug, "~s: Server transaction cancelled, acting alone. Answering 500 Server Internal Error.", [LogTag]),
			    Response = make_response(500, "Server Internal Error", "", [], [], State),
			    {ok, NewState1} = do_response(created, Response, State),
			    {ok, NewState1}
		    end;
		_ ->
		    logger:log(debug, "~s: Server transaction cancelled when in state ~p - ignoring.",
				[LogTag, SipState]),
		    {ok, State}
	    end;

	{get_branch, FromPid} ->
	    FromPid ! {got_branch, self(), State#state.branch},
	    {ok, State};

	{set_report_to, FromPid} ->
	    NewState1 = case State#state.report_to of
		undefined ->
		    logger:log(debug, "Server transaction adopted by ~p", [FromPid]),
		    FromPid ! {set_report_to, self()},
		    State#state{report_to=FromPid};
		_ ->
		    FromPid ! {failed_setting_report_to, self(), "Already set"},
		    State
	    end,
	    {ok, NewState1};
	    
	{quit} ->
	    {quit, State};

	Unknown ->
	    logger:log(error, "~s: Server transaction: Received unknown message in loop() :~n~p",
			[LogTag, Unknown]),	    
	    {error, State}
    after
	300 * 1000 ->
	    {ZMethod, ZURI, _, _} = State#state.request,
	    case State#state.mode of
		stateful ->
		    logger:log(error, "~s: Server transaction (~s ~s) still alive after 5 minutes!",
				[LogTag, ZMethod, sipurl:print(ZURI)]),
		    {error, State};
		stateless ->
		    logger:log(debug, "~s: Stateless server transaction (~s ~s) terminating without having sent a response after 5 minutes",
				[LogTag, ZMethod, sipurl:print(ZURI)]),
		    {quit, State}
	    end
    end,
    DoQuit = case Res of
	quit -> true;
	_ ->
	    case NewState#state.sipstate of
		terminated -> true;
		_ -> false
	    end
    end,
    case DoQuit of
	true ->
	    RStr = case NewState#state.response of
		{QStatus, QReason, _, _} ->
		    io_lib:format("last sent response was ~p ~s", [QStatus, QReason]);
		undefined ->
		    "no responses sent"
	    end,
	    {QMethod, QURI, _, _} = NewState#state.request,
	    logger:log(debug, "~s: Server transaction (~s ~s) terminating in state '~p', ~s",
			[LogTag, QMethod, sipurl:print(QURI), NewState#state.sipstate, RStr]),
	    NewState;
	_ ->
	    loop(NewState)
    end.

process_timer(Timer, State) when record(State, state) ->
    [TRef, Timeout, Signal, Description] = siptimer:extract([ref, timeout, appsignal, description], Timer),
    LogTag = State#state.logtag,
    {Method, URI, Header, Body} = State#state.request,
    logger:log(debug, "~s: Timer ~p:~p fired", [LogTag, TRef, Description]),
    case Signal of

	{resendresponse} ->
	    case State#state.response of
		{Status, Reason, RHeader, RBody} ->
		    case State#state.sipstate of
			completed ->
			    logger:log(debug, "~s: Resend after ~p (response to ~s ~s): ~p ~s",
			    	       [LogTag, siptimer:timeout2str(Timeout), Method, sipurl:print(URI), Status, Reason]),
			    transportlayer:send_proxy_response(State#state.socket, State#state.response),
			    NewTimeout = case Method of
				"INVITE" ->
				    % Use Timer Value T2 for INVITE responses
				    T2 = sipserver:get_env(timerT2, 4000),
				    lists:min([Timeout * 2, T2]);
				_ ->
				    Timeout * 2
			    end,
			    NewTimerList = siptimer:revive_timer(Timer, NewTimeout, State#state.timerlist),
			    State#state{timerlist=NewTimerList};
			_ ->
			    logger:log(debug, "~s: Ignoring signal to resend response to (~s ~s): ~p ~s since we are in state '~p'",
			    	       [LogTag, Method, sipurl:print(URI), Status, Reason, State]),
			    State
		    end;
		undefined ->
		    logger:log(error, "~s: Resend after ~p (response to ~s ~s, state '~p'): no response sent!",
				[Timeout, Method, sipurl:print(URI), State]),
		    State
	    end;

	{resendresponse_timeout} ->
	    {Status, Reason, _, _} = State#state.response,
	    logger:log(normal, "~s: Sending of ~p ~s (response to ~s ~s) timed out after ~s seconds, terminating transaction.",
		    	       [LogTag, Status, Reason, Method, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
	    % XXX report to TU?
	    enter_sip_state(terminated, State);
	
	{terminate_transaction} ->
	    logger:log(debug, "~s: Received timer signal to terminate transaction", [LogTag]),
	    enter_sip_state(terminated, State);

	_ ->
	    logger:log(error, "~s: Received unknown signal from timer ~p: ~p", [LogTag, TRef, Signal]),
	    State
    end.	

do_response(Created, Response, State) when record(State, state) ->
    LogTag = State#state.logtag,
    {ResponseToMethod, ResponseToURI, _, _} = State#state.request,
    OldSipState = State#state.sipstate,
    {Status, Reason, _, _} = Response,
    NewState1 = go_stateful(Created, ResponseToMethod, Status, LogTag, State),
    {Action, SendReliably, NewSipState} = send_response_statemachine(ResponseToMethod, Status, OldSipState),
    NewState2 = case Action of
	ignore ->
	    NewState1;
	send ->
	    LogLevel = if
		Status >= 200 -> normal;
		true -> debug
	    end,
	    What = case Created of
		created -> "Responding";
		_ -> "Forwarding response"
	    end,
	    logger:log(LogLevel, "~s: ~s ~p ~s",
			[LogTag, What, Status, Reason]),
	    {ok, NewState2_1} = send_response(Response, SendReliably, NewState1),
	    NewState2_1
    end,
    NewState = enter_sip_state(NewSipState, NewState2),
    {ok, NewState}.

go_stateful(created, Method, Status, LogTag, State) when record(State, state), State#state.mode == stateless, Status >= 200 ->
    logger:log(debug, "~s: Going stateful when sending locally created final response ~p response to ~s",
		[LogTag, Status, Method]),
    State#state{mode=stateful};
go_stateful(_, _, _, _, State) ->
    State.

send_response_statemachine(Method, Status, trying) when Status == 100 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'trying' - " ++
		      "doing so (unreliably) and entering state 'proceeding'", [Status, Method]),
    {send, false, proceeding};

send_response_statemachine(Method, Status, State) when Status == 100 ->
    logger:log(debug, "UAS decision: Requested to send 100 Trying to ~s when in state '~p' - " ++
		      "ignoring", [Method, State]),
    {ignore, false, proceeding};

send_response_statemachine(Method, Status, trying) when Status =< 199 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'trying' - " ++
		      "doing so (unreliably) and entering state 'proceeding'", [Status, Method]),
    {send, false, proceeding};

send_response_statemachine(Method, Status, proceeding) when Status =< 199 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'proceeding' - " ++
		      "doing so (unreliably)", [Status, Method]),
    {send, false, proceeding};


send_response_statemachine("INVITE", Status, proceeding) when Status =< 299 ->
    logger:log(debug, "UAS decision: Requested to send 2xx response ~p to INVITE when in state 'proceeding' - " ++
		      "doing so (unreliably) and entering state 'terminated'", [Status]),
    {send, false, terminated};


send_response_statemachine("INVITE", Status, proceeding) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 3xx, 4xx, 5xx or 6xx response ~p to INVITE when in state 'proceeding' - " ++
		      "doing so (reliably) and entering state 'completed'", [Status]),
    {send, true, completed};

send_response_statemachine(Method, Status, trying) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when in state 'trying' - " ++
		      "doing so (unreliably) and entering state 'completed'", [Status, Method]),
    {send, false, completed};

send_response_statemachine(Method, Status, proceeding) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when in state 'proceeding' - " ++
		      "doing so (unreliably) and entering state 'completed'", [Status, Method]),
    {send, false, completed};

send_response_statemachine(Method, Status, completed) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when already in state 'completed' - " ++
		      "ignoring", [Status, Method]),
    {ignore, false, completed};

send_response_statemachine(Method, Status, terminated) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send response ~p to ~s when in state 'terminated' - " ++
		      "ignoring", [Status, Method]),
    {ignore, false, terminated}.


% This branch should answer something.
send_response(Response, SendReliably, State) when record(State, state) ->
    {Status, Reason, RHeader, RBody} = Response,
    Request = State#state.request,
    {Method, URI, _, _} = Request,
    Socket = State#state.socket,
    LogTag = State#state.logtag,
    if
	Method == "INVITE", Status >= 300 ->
	    % Tell transactionlayer pid about this response to INVITE - it needs to know
	    % what To-tag we have used in order to match any ACK:s received for this
	    % response back to this server transaction.
	    ToTag = sipheader:get_tag(keylist:fetch("To", RHeader)),
	    case transactionlayer:store_to_tag(Request, ToTag) of
		ok -> true;
		R ->
		    % XXX abort sending?
		    logger:log(error, "~s: Failed storing To-tag with transactionlayer : ~p", [LogTag, R])
	    end;
	true -> true
    end,
    logger:log(debug, "~s: Sending response to ~s ~s : ~p ~s", 
    		[LogTag, Method, sipurl:print(URI), Status, Reason]),
    transportlayer:send_proxy_response(Socket, Response),
    NewState1 = case SendReliably of
	true ->
	    T1 = sipserver:get_env(timerT1, 500),
	    case Method of
		"INVITE" ->
		    TimerG = T1,
		    TimerH = 64 * T1,
		    GDesc = "resendresponse " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++ sipurl:print(URI) ++ " (Timer G)",
		    HDesc = "stopresend of " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++ sipurl:print(URI) ++ " (Timer H)",
		    NewState2 = add_timer(TimerG, GDesc, {resendresponse}, State),
		    NewState3 = add_timer(TimerH, HDesc, {resendresponse_timeout}, NewState2),
		    NewState3;
		_ ->
		    TimerJ = 64 * T1,
		    JDesc = "terminate server transaction after response " ++ integer_to_list(Status) ++ " " ++ Reason ++ " has been sent to " ++ sipurl:print(URI) ++ " (Timer J)",
		    NewState2 = add_timer(TimerJ, JDesc, {terminate_transaction}, State),
		    NewState2
	    end;
	false ->
	    State
    end,
    NewState = NewState1#state{response=Response},
    {ok, NewState}.

add_timer(Timeout, Description, AppSignal, State) when record(State, state) ->
    NewTimerList = siptimer:add_timer(Timeout, Description, AppSignal, State#state.timerlist),
    State#state{timerlist=NewTimerList}.

enter_sip_state(NewSipState, State) when record(State, state) ->
    LogTag = State#state.logtag,
    OldSipState = State#state.sipstate,
    case NewSipState of
	OldSipState ->
	    State;
	_ ->
	    NewState1 = State#state{sipstate=NewSipState},
	    {ResponseToMethod, ResponseToURI, _, _} = NewState1#state.request,
	    NewState = case NewSipState of
	        completed ->
		    case ResponseToMethod of
			"INVITE" ->
			    NewState1;
			_ ->
			    T1 = sipserver:get_env(timerT1, 500),
			    TimerJ = 64 * T1,
			    logger:log(debug, "~s: Server transaction: Entered state 'completed'. Original request was non-INVITE, " ++
					      "starting Timer J with a timeout of ~s seconds.",
			    		[LogTag, siptimer:timeout2str(TimerJ)]),
			    % Install TimerJ (default 32 seconds) RFC 3261 17.2.2. Until TimerJ fires we
			    % resend our response whenever we receive a request resend.
			    JDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++ " (Timer J)",
			    add_timer(TimerJ, JDesc, {terminate_transaction}, NewState1)
		    end;
		confirmed ->
		    case ResponseToMethod of
			"INVITE" ->
			    TimerI = sipserver:get_env(timerT4, 5000),
			    logger:log(debug, "~s: Entered state 'confirmed'. Original request was an INVITE, starting Timer I with a timeout of ~s seconds.",
			    		[LogTag, siptimer:timeout2str(TimerI)]),
			    % Install TimerI (T4, default 5 seconds) RFC 3261 17.2.1. Until TimerI fires we
			    % absorb any additional ACK requests that might arrive.
			    IDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++ " (Timer I)",
			    add_timer(TimerI, IDesc, {terminate_transaction}, NewState1);
			_ ->
			    logger:log(error, "~s: Entered state 'confirmed'. Original request was NOT an INVITE (it was ~s ~s). How could this be?",
			    		[LogTag, ResponseToMethod, sipurl:print(ResponseToURI)]),
			    NewState1
		    end;
		_ ->
		    logger:log(debug, "~s: Entering state '~p'", [LogTag, NewSipState]),
		    NewState1
	    end,
	    NewState
    end.

process_received_ack(State) when record(State, state) ->
    {Method, URI, _, _} = State#state.request,
    {Status, Reason, _, _} = State#state.response,
    LogTag = State#state.logtag,
    logger:log(normal, "~s: Response ~p ~s to request ~s ~s ACK-ed", [LogTag, Status, Reason, Method, sipurl:print(URI)]),
    case Method of
	"INVITE" ->
	    logger:log(debug, "~s: Received ACK, cancelling resend timers for response ~p ~s (to request ~s ~s) and entering state 'confirmed'",
	    		[LogTag, Status, Reason, Method, sipurl:print(URI)]),
	    TimerList = State#state.timerlist,
	    NewTimerList = siptimer:cancel_timers_with_appsignal({resendresponse}, TimerList),
	    NewState1 = State#state{timerlist=NewTimerList},
	    NewState = enter_sip_state(confirmed, NewState1),
	    NewState;
	_ ->
	    logger:log(debug, "~s: Received ACK to non-INVITE request ~s ~s (response being ACKed is ~p ~s) - ignoring",
			[LogTag, Method, sipurl:print(URI), Status, Reason]),
	    State
    end.

make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State) when record(State, state), Status == 100 ->
    siprequest:make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State#state.socket, State#state.request);

make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State) when record(State, state) ->
    Request = State#state.request,
    {Method, URI, Header, Body} = Request,
    To = keylist:fetch("To", Header),
    Req = case sipheader:get_tag(To) of
	none ->
	    {DistplayName, ToURI} = sipheader:to(To),
	    NewTo = lists:concat([sipheader:to_print({DistplayName, ToURI}), ";tag=",
	    			  State#state.my_to_tag]),
	    NewHeader = keylist:set("To", [NewTo], Header),
	    {Method, URI, NewHeader, Body};
	_ ->
	    Request
    end,
    siprequest:make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State#state.socket, Req).

generate_tag() ->
    % Erlang guarantees that subsequent calls to now() generate increasing values (on the same node).
    {Megasec, Sec, Microsec} = now(),
    In = lists:concat([node(), Megasec * 1000000 + Sec, 8, $., Microsec]),
    Out = siprequest:make_base64_md5_token(In),
    % RFC3261 #19.3 says tags must have at least 32 bits randomness,
    % don't make them longer than they have to be.
    "yxa-" ++ string:substr(Out, 1, 9).
