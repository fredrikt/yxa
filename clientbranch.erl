-module(clientbranch).
-export([start/7]).

% OrigRequest = {Method, ReqURI, Header, Body}

% Spawn a new erlang process ready to handle a client branch. This process can handle multiple transactions.
start(Branch, Socket, FromIP, Parent, OrigRequest, Request, Timeout) ->
    sipserver:safe_spawn(fun start_branch/7, [Branch, Socket, FromIP, Parent, OrigRequest, Request, Timeout]).

start_branch(Branch, Socket, FromIP, Parent, OrigRequest, Request, Timeout) ->
    {OrigReqMethod, OrigReqURI, OrigReqHeader, OrigReqBody} = OrigRequest,
    {Method, URI, _, _} = Request,
    LogStr = sipserver:make_logstr({request, OrigReqMethod, OrigReqURI, OrigReqHeader, OrigReqBody}, FromIP),
    logger:log(normal, "~s: ~s -> Started client transaction ~s ~s (pid ~p, parent ~p, timeout ~p)",
	[Branch, LogStr, Method, sipurl:print(URI), self(), Parent, Timeout]),
    State = case Method of
	"INVITE" -> calling;
	_ -> trying
    end,
    TransactionList = transactionlist:add_transaction(Request, none, State, []),
    TimerData = process_request(Branch, Socket, Timeout, Request, {[], 0}),
    BranchData = {TimerData, TransactionList},
    process_branch_loop(Branch, Socket, Parent, OrigRequest, BranchData),
    ok.

% This is the main loop for a client branch. Receive messages and act on them.
process_branch_loop(Branch, Socket, Parent, OrigRequest, BranchData) ->
    {OrigMethod, OrigURI, OrigHeader, OrigBody} = OrigRequest,
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    NewBranchData = receive

	{cancel, Msg} ->
	    logger:log(debug, "~s: Branch requested to cancel (~s)", [Branch, Msg]),
	    cancel_request(Branch, Socket, OrigRequest, BranchData);
	    

	{request, {"ACK", URI, Header, Body}} ->
	    case transactionlist:get_transaction_using_header(Header, TransactionList) of
		none ->
		    CSeq = sipheader:cseq(keylist:fetch("CSeq", Header)),
		    logger:log(error, "~s: Received ACK of unknown request (CSeq ~p) - ignoring (XXX is this correct?)", [Branch, CSeq]),
		    logger:log(debug, "~s: Current transactions :~n~p", [Branch, transactionlist:debugfriendly(TransactionList)]),
		    BranchData;
		AckToTransaction ->
		    Request = {"ACK", URI, Header, Body},
		    process_received_ack(Branch, Socket, Parent, BranchData, Request, AckToTransaction)
	    end;

	{request, Request} ->
	    {Method, URI, Header, _} = Request,
	    logger:log(error, "~s: Client branch received a request (~s ~s) that was not an ACK - such requests should NOT be passed to the client branch!",
			[Branch, Method, sipurl:print(URI)]),
	    siprequest:send_result(Header, Socket, "", 500, "Internal Server Error"),
	    BranchData;
	
	% We have received a response to one of our requests.
	{response, Response} ->
	    {Status, Reason, Header, Body} = Response,
	    case transactionlist:get_transaction_using_header(Header, TransactionList) of
		none ->
		    CSeq = sipheader:cseq(keylist:fetch("CSeq", Header)),
		    logger:log(error, "~s: Received response ~p ~s to unknown transaction (CSeq ~p) - ignoring", [Branch, Status, Reason, CSeq]),
		    logger:log(debug, "~s: Current transactions :~n~p", [Branch, transactionlist:debugfriendly(TransactionList)]),
		    BranchData;
		Transaction ->
		    logger:log(debug, "~s: Found transaction for response ~p ~s", [Branch, Status, Reason]),
		    process_received_response(Branch, Socket, Parent, BranchData, OrigRequest, Response, Transaction)
	    end;
	    	

	{siptimer, TSeq, TDesc} ->
	    case siptimer:get_timer(TSeq, TimerList) of
	        none ->
	            logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring. TimerList :~n~p",
	            		[Branch, TSeq, TDesc, siptimer:debugfriendly(TimerList)]),
	            BranchData;
	        Timer ->
		    process_timer(Branch, Socket, Parent, OrigRequest, BranchData, Timer)
	    end;


	{quit} ->
	    {quit};

	Msg ->
	    logger:log(error, "~s: Received unknown message ~p", [Branch, Msg]),
	    BranchData
    end,
    OrigReqTransaction = transactionlist:get_transaction_using_header(OrigHeader, TransactionList),
    DoQuit = case NewBranchData of
	{quit} -> true;
	_ ->
	    case transactionlist:extract_state(OrigReqTransaction) of
		terminated -> true;
		_ -> false
	    end
    end,
    case DoQuit of
	true ->
	    Parent ! {clientbranch_terminating, {Branch, self()}},
	    FetchedRequest = transactionlist:extract_request(OrigReqTransaction),
	    {FetchedReqMethod, FetchedReqURI, _, _} = FetchedRequest,
	    logger:log(normal, "~s: ending client branch, transaction ~s ~s",
	    	       [Branch, FetchedReqMethod, sipurl:print(FetchedReqURI)]),
	    true;
	false ->
	    process_branch_loop(Branch, Socket, Parent, OrigRequest, NewBranchData)
    end.

process_timer(Branch, Socket, Parent, OrigRequest, BranchData, Timer) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    {OrigMethod, OrigURI, OrigHeader, OrigBody} = OrigRequest,
    TSeqNo = siptimer:extract_seqno(Timer),
    Timeout = siptimer:extract_timeout(Timer),
    Signal = siptimer:extract_appsignal(Timer),
    Description =  siptimer:extract_description(Timer),
    logger:log(debug, "~s: Timer ~p:~p fired", [Branch, TSeqNo, Description]),
    case Signal of
        {resendrequest, Request} ->
	    {ReqMethod, ReqURI, ReqHeader, ReqBody} = Request,
	    Transaction = transactionlist:get_transaction_using_header(ReqHeader, TransactionList),
	    State = transactionlist:extract_state(Transaction),
	    DoResend = case State of
        	trying ->
        	    true;
        	calling ->
        	    true;
        	_ ->
        	    false
            end,
            case DoResend of
		true ->
		    logger:log(normal, "~s: Resend after ~p (request -> ~s): ~s",
		    	       [Branch, Timeout / 1000, sipurl:print(ReqURI), ReqMethod]),
		    siprequest:send_proxy_request(ReqHeader, Socket,
						  {ReqMethod, ReqURI, ReqBody,
						   ["branch=" ++ Branch]}),
		    NewTimeout = get_request_resend_timeout(ReqMethod, Timeout, State),
		    NewTimerList = siptimer:revive_timer(Timer, NewTimeout, TimerList),
		    {{NewTimerList, TimerSeq}, TransactionList};
		_ ->
		    logger:log(debug, "~s: Ignoring resendrequest of ~s ~s when in state ~p",
		    		[Branch, ReqMethod, sipurl:print(ReqURI), State]),
		    BranchData
	    end;

	{resendrequest_timeout, Request} ->
	    {Method, URI, Header, _} = Request,
	    logger:log(normal, "~s: Sending of ~s ~s timed out after ~p seconds", [Branch, Method, sipurl:print(URI), Timeout / 1000]),
	    Transaction = transactionlist:get_transaction_using_header(Header, TransactionList),
	    case transactionlist:extract_state(Transaction) of
		trying ->
		    fake_request_timeout(Branch, BranchData, Parent, OrigRequest, Request);
		calling ->
		    fake_request_timeout(Branch, BranchData, Parent, OrigRequest, Request);
		proceeding ->
		    case Method of
			"INVITE" ->
			    logger:log(debug, "~s: Sending of original request (INVITE ~s) timed out in state 'proceeding' - cancel original request",
			    		[Branch, sipurl:print(URI)]),
			    cancel_request(Branch, Socket, OrigRequest, BranchData);
			_ ->
			    fake_request_timeout(Branch, BranchData, Parent, OrigRequest, Request)
		    end;
		State ->
		    logger:log(debug, "~s: Sending of original request (~s ~s) timed out in state '~s' - ignoring.",
		    		[Branch, Method, sipurl:print(URI), State]),
		    BranchData
	    end;

	{terminate_transaction, Id} ->
	    logger:log(debug, "~s: Received timer signal to terminate transaction ~p", [Branch, Id]),
	    terminate_transaction_id(Branch, BranchData, Id);
	    
	{invite_timeout, Id} ->
	    Transaction = transactionlist:get_transaction(Id, TransactionList),
	    Request = transactionlist:extract_request(Transaction),
	    {_, URI, _, _} = Request,
	    logger:log(normal, "~s: Request INVITE ~s timeout after ~p seconds", [Branch, sipurl:print(URI), Timeout / 1000]),
	    end_invite(Branch, Socket, Parent, OrigRequest, BranchData, Id);
	    
	{invite_expire, Id} ->
	    end_invite(Branch, Socket, Parent, OrigRequest, BranchData, Id);
	
	{quit} ->
	    logger:log(debug, "~s: Received timer signal to quit", [Branch]),
	    {quit};

	_ ->
	    logger:log(error, "~s: Received unknown signal from timer ~p: ~p", [Branch, TSeqNo, Signal]),
	    BranchData
    end.

% Process a response delivered to us from one of our branches. Make sure
% it is a response to the same kind of request as our original request,
% then run it through the state machine to determine what to do.
process_received_response(Branch, Socket, Parent, BranchData, OrigRequest, Response, Transaction) ->
    ResponseToRequest = transactionlist:extract_request(Transaction),
    TransactionId = transactionlist:extract_id(Transaction),
    OldState = transactionlist:extract_state(Transaction),
    {OrigMethod, OrigURI, OrigHeader, _} = OrigRequest,
    {Status, Reason, ResponseHeader, _} = Response,
    {ResponseToMethod, ResponseToURI, ResponseToHeader, _} = ResponseToRequest,
    ResponseToId = sipheader:cseq(keylist:fetch("CSeq", ResponseToHeader)),
    State = transactionlist:extract_state(Transaction),
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    % Stop resending of request as soon as possible.
    NewTimerList1 = case State of
	trying ->
	    stop_resendrequest_timer(Branch, {resendrequest, ResponseToId}, ResponseToRequest, TimerList);
	calling ->
	    stop_resendrequest_timer(Branch, {resendrequest, ResponseToId}, ResponseToRequest, TimerList);
	_ ->
	    TimerList
    end,
    NewTimerList2 = case ResponseToMethod of
	"INVITE" ->
	    % ACK any 3xx, 4xx, 5xx and 6xx responses if they are responses to an INVITE 
	    % and we are in either 'calling' or 'proceeding' state
	    ack_response_to_invite(Branch, State, Socket, Status, Reason, ResponseHeader, ResponseToRequest),

	    % Cancel/reset INVITE request expire timer when receiving provisional responses
	    update_invite_expire(Branch, Status, {resendrequest, ResponseToId}, {invite_expire, ResponseToId}, NewTimerList1);
	_ ->
	    NewTimerList1
    end,
    NewBranchData1 = {{NewTimerList2, TimerSeq}, TransactionList},
    {BranchAction, NewState} =
	received_response_state_machine(ResponseToMethod, Status, State),
    NewBranchData2 = update_transaction_state(Branch, NewBranchData1, TransactionId, Response, NewState, BranchAction),
    {NewBranchData3, NewBranchAction} = act_on_new_state(Branch, Socket, OrigRequest, NewBranchData2, TransactionId, Response, OldState, NewState, BranchAction),
    perform_branchaction(Branch, Socket, Parent, OrigRequest, NewBranchData3, TransactionId, Response, NewState, NewBranchAction).

update_transaction_state(Branch, BranchData, TransactionId, Response, NewState, BranchAction) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    Transaction = transactionlist:get_transaction(TransactionId, TransactionList),
    State = transactionlist:extract_state(Transaction),
    ResponseToRequest = transactionlist:extract_request(Transaction),
    {Status, Reason, ResponseHeader, _} = Response,
    {ResponseToMethod, ResponseToURI, ResponseToHeader, _} = ResponseToRequest,
    case NewState of
	State ->
	    logger:log(debug, "~s: Evaluated ~p ~s (in response to ~s ~s). Remaining in state '~p', action '~p'.",
	    	[Branch, Status, Reason, ResponseToMethod, sipurl:print(ResponseToURI), State, BranchAction]),
	    % XXX if what we received was a provisional response and a provisional response is what is stored,
	    % replace the stored response with the new one (so that we resend the most recent one)
            %NewTransaction = transactionlist:set_response(Transaction, Response),
            %NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
	    %{TimerData, NewTransactionList};
	    BranchData;
	_ ->
	    logger:log(debug, "~s: Evaluated ~p ~s (in response to ~s ~s). Changing state from '~p' to '~p', action '~p'.",
	    	[Branch, Status, Reason, ResponseToMethod, sipurl:print(ResponseToURI), State, NewState, BranchAction]),
	    NewTransaction1 = transactionlist:set_state(Transaction, NewState),
	    NewTransaction = transactionlist:set_response(NewTransaction1, Response),
            NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
	    {TimerData, NewTransactionList}
    end.

act_on_new_state(Branch, Socket, OrigRequest, BranchData, TransactionId, Response, OldState, NewState, BranchAction) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    Transaction = transactionlist:get_transaction(TransactionId, TransactionList),
    ResponseToRequest = transactionlist:extract_request(Transaction),
    {OrigMethod, OrigURI, OrigHeader, _} = OrigRequest,
    {Status, Reason, ResponseHeader, _} = Response,
    {ResponseToMethod, ResponseToURI, ResponseToHeader, _} = ResponseToRequest,
    ResponseToId = sipheader:cseq(keylist:fetch("CSeq", ResponseToHeader)),
    case NewState of
	OldState ->
	    {BranchData, BranchAction};
	_ ->
	    case NewState of
		proceeding ->
		    case ResponseToMethod of
			"INVITE" ->
			    case transactionlist:extract_cancelled(Transaction) of
				true ->
				    logger:log(debug, "~s: A previously cancelled transaction (~s ~s) entered state 'proceeding' upon receiving a ~p ~s response. CANCEL it!",
				    		[Branch, ResponseToMethod, sipurl:print(ResponseToURI), Status, Reason]),
				    NewBranchData = cancel_request(Branch, Socket, ResponseToRequest, BranchData),
				    {NewBranchData, ignore};
				_ ->
				    {BranchData, BranchAction}
			    end;
			_ ->
			    {BranchData, BranchAction}
		    end;
		completed ->
		    NewTimerList1 = siptimer:cancel_timers_with_appid({resendrequest, ResponseToId}, TimerList),
		    % Install TimerD with a minimum value of 32 seconds (RFC 3261 17.1.1.2)
		    % or TimerK (T4, default 5 seconds) in case of non-INVITE request (17.1.2.2)
		    % to stay alive for a few seconds collecting resends
		    NewTimerList2 = case ResponseToMethod of
			"INVITE" ->
			    TimerD = 32000,
			    DDesc = "terminate client transaction INVITE " ++ sipurl:print(ResponseToURI) ++ " (Timer D)",
			    siptimer:add_timer(TimerSeq + 1, TimerD, DDesc, {terminate_transaction, ResponseToId}, {resendrequest, ResponseToId}, NewTimerList1);
			_ ->
			    TimerK = sipserver:get_env(timerT4, 5000),
			    KDesc = "terminate client transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++ " (Timer K)",
			    siptimer:add_timer(TimerSeq + 1, TimerK, KDesc, {terminate_transaction, ResponseToId}, {timerK}, NewTimerList1)
		    end,
		    NewBranchData = {{NewTimerList2, TimerSeq + 1}, TransactionList},
		    {NewBranchData, BranchAction};
		terminated ->
		    NewTimerList1 = siptimer:cancel_timers_with_appid({resendrequest, ResponseToId}, TimerList),
		    NewBranchData = {{NewTimerList1, TimerSeq}, TransactionList},
		    {NewBranchData, BranchAction};
		_ ->
		    {BranchData, BranchAction}
	    end
    end.

perform_branchaction(Branch, Socket, Parent, OrigRequest, BranchData, TransactionId, Response, NewState, BranchAction) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    Transaction = transactionlist:get_transaction(TransactionId, TransactionList),
    ResponseToRequest = transactionlist:extract_request(Transaction),
    {Status, Reason, ResponseHeader, _} = Response,
    {ResponseToMethod, ResponseToURI, ResponseToHeader, _} = ResponseToRequest,
    State = transactionlist:extract_state(Transaction),
    case BranchAction of
	ignore ->
	    BranchData;
	tell_parent ->
	    case ResponseToMethod of
		"CANCEL" ->
		    logger:log(debug, "~s: Not telling parent about response to a CANCEL",
			    		[Branch]);
		_ ->
		    Parent ! {branch_result, Branch, NewState, Response, ResponseToRequest}
	    end,
	    BranchData
    end.

received_response_state_machine(Method, Status, State) when Status =< 99 ->
    logger:log(debug, "UAC decision: Received INVALID response ~p to ~p when in state '~p', ignoring", [Status, Method, State]),
    {ignore, State};


received_response_state_machine(Method, Status, calling) when Status == 100 ->
    logger:log(debug, "UAC decision: Received 100 in response to ~s, going from 'calling' to 'proceeding'", [Method]),
    {ignore, proceeding};
received_response_state_machine(Method, Status, State) when Status == 100 ->
    logger:log(debug, "UAC decision: Received 100 in response to ~s when in state '~p', ignoring", [Method, State]),
    {ignore, State};


received_response_state_machine("INVITE", Status, calling) when Status =< 199 ->
    logger:log(debug, "UAC decision: Received first 1xx response ~p to INVITE, going from 'calling' to 'proceeding' and telling parent", [Status]),
    {tell_parent, proceeding};
received_response_state_machine("INVITE", Status, proceeding) when Status =< 199 ->
    logger:log(debug, "UAC decision: Received 1xx response ~p to INVITE when in state 'proceeding', telling parent", [Status]),
    {tell_parent, proceeding};

received_response_state_machine(Method, Status, trying) when Status =< 199 ->
    logger:log(debug, "UAC decision: Received first 1xx response ~p to ~s, going from 'trying' to 'proceeding' and telling parent", [Status, Method]),
    {tell_parent, proceeding};
received_response_state_machine(Method, Status, proceeding) when Status =< 199 ->
    logger:log(debug, "UAC decision: Received 1xx response ~p to ~s when in state 'proceeding', ignoring", [Status, Method]),
    {ignore, proceeding};


received_response_state_machine("INVITE", Status, State) when Status =< 299 ->
    % We ALWAYS tell parent about 2xx responses to INVITE. RFC 3261 17.1.1.2 says we
    % MUST enter the 'terminated' state when receiving a 2xx response to INVITE.
    % This causes this client transaction to be terminated immediately, and any
    % resends will then be passed directly to the proxy core.
    logger:log(debug, "UAC decision: Received 2xx response ~p to INVITE when in state '~p',  entering state 'terminated' and telling parent", [Status, State]),
    {tell_parent, terminated};

received_response_state_machine(Method, Status, trying) when Status =< 299 ->
    logger:log(debug, "UAC decision: Received 2xx response ~p to ~s when in state 'trying',  entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status =< 299 ->
    logger:log(debug, "UAC decision: Received 2xx response ~p to ~s when in state 'proceeding',  entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status =< 299 ->
    logger:log(debug, "UAC decision: Received LATE 2xx response ~p to ~s when in state '~p', ignoring", [Status, Method, State]),
    {ignore, State};


received_response_state_machine(Method, Status, trying) when Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state 'trying', entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine("INVITE", Status, calling) when Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to INVITE when in state 'calling', entering state 'completed' and telling parent", [Status]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state 'proceeding', entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state '~p', ignoring", [Status, Method, State]),
    {ignore, State};


received_response_state_machine(Method, Status, trying) when Status =< 699 ->
    % Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to ~s when in state 'trying', entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine("INVITE", Status, calling) when Status =< 699 ->
    % Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to INVITE when in state 'calling', entering state 'completed' and telling parent", [Status]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status =< 699 ->
    % Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to ~s when in state 'proceeding', entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status =< 699 ->
    % If one branch has answered 2xx already, I don't see how it would be correct to undo that
    logger:log(debug, "UAC decision: Received LATE 6xx response ~p to ~s when in state '~p', ignoring", [Status, Method, State]),
    {ignore, State}.


process_received_ack(Branch, Socket, Parent, BranchData, Request, AckToTransaction) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    AckToRequest = transactionlist:extract_request(AckToTransaction),
    {AckToMethod, AckToURI, AckToHeader, _} = AckToRequest,
    case AckToMethod of
	"INVITE" ->
	    Id = sipheader:cseq(keylist:fetch("CSeq", AckToHeader)),
	    logger:log(debug, "~s: Received ACK, cancelling resend timers for request ~s ~s and entering state 'confirmed'",
	    		[Branch, AckToMethod, sipurl:print(AckToURI)]),
	    NewTimerList = siptimer:cancel_timers_with_appid({resendresponse, Id}, TimerList),
	    NewTransaction1 = transactionlist:set_acked(AckToTransaction, true),
	    NewTransaction = transactionlist:set_state(NewTransaction1, confirmed),
	    NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
	    {{NewTimerList, TimerSeq}, NewTransactionList};
	_ ->
	    logger:log(error, "~s: Received ACK to non-INVITE request ~s ~s - ignoring",
			[Branch, AckToMethod, sipurl:print(AckToURI)]),
	    BranchData
    end.
    
% When we receive provisional responses to an INVITE we sent, we have to
% reset TimerC to a value more than 3 minutes
update_invite_expire(Branch, Status, AppId, Signal, TimerList) when Status == 100 ->
    TimerList;
update_invite_expire(Branch, Status, AppId, Signal, TimerList) when Status =< 199 ->
    TransactionTimers = siptimer:get_timers_appid_matching(AppId, TimerList),
    case siptimer:get_timers_appsignal_matching(Signal, TransactionTimers) of
	[] ->
	    logger:log(error, "~s: Received provisional response ~p to INVITE request, but no 'invite_expire' (Timer C) timer found", [Branch, Status]),
	    logger:log(error, "~s: TimerList where I could not find an 'invite_expire' (Timer C) timer :~n~p",
	    		[Branch, siptimer:debugfriendly(TimerList)]),
	    TimerList;
	[InviteExpireTimer] ->
	    siptimer:stop_timers([InviteExpireTimer]),
	    Timeout = siptimer:extract_timeout(InviteExpireTimer),
	    OldStarttime = siptimer:extract_starttime(InviteExpireTimer),
	    SecondsLeft = (OldStarttime + (Timeout / 1000)) - util:timestamp(),
	    logger:log(debug, "~s: Received 1xx response to INVITE, 'invite_expire' (Timer C) was about to fire in ~p seconds. Resetting to ~p seconds.",
	    		[Branch, SecondsLeft, Timeout / 1000]),
	    siptimer:revive_timer(InviteExpireTimer, Timeout, TimerList)
    end;
update_invite_expire(Branch, Status, AppId, Signal, TimerList) ->
    TimerList.

ack_response_to_invite(Branch, State, Socket, Status, Reason, ResponseHeader, {"INVITE", OrigURI, _, _}) when Status =< 299 ->
    logger:log(debug, "~s: Not ACK-ing 1xx or 2xx response to INVITE ~s: ~p ~s",
    		[Branch, sipurl:print(OrigURI), Status, Reason]),
    true;
ack_response_to_invite(Branch, State, Socket, Status, Reason, ResponseHeader, ResponseToRequest) when Status =< 699 ->
    {_, ResponseToURI, _, _} = ResponseToRequest,
    logger:log(debug, "~s: ACK-ing 3xx, 4xx, 5xx or 6xx response ~p ~s to INVITE ~s when in state '~p'",
    		[Branch, Status, Reason, sipurl:print(ResponseToURI), State]),
    process_branch_ack(Branch, Socket, Status, ResponseHeader, ResponseToRequest).

% Send an ACK. We do not set up any timers for retransmission since if this
% ACK is lost we will receive a retransmitted response which will effectively
% cause us to send a new ACK.
process_branch_ack(Branch, Socket, Status, AckHeader, OrigRequest) when Status =< 199 ->
    logger:log(debug, "~s: Not ACK-ing response ~p. XXX shouldn't this be a PRACK?", [Branch, Status]); 
process_branch_ack(Branch, Socket, Status, AckHeader, OrigRequest) when Status =< 699 ->
    {OrigMethod, OrigURI, OrigHeader, OrigBody} = OrigRequest,
    if
	Status =< 299 ->
	    logger:log(debug, "~s: process_branch_ack() Sending ACK of 2xx response ~p -> ~s", [Branch, Status, sipurl:print(OrigURI)]),
	    {CSeq, _} = sipheader:cseq(keylist:fetch("CSeq", OrigHeader));
	true ->
	    logger:log(debug, "~s: process_branch_ack() Sending ACK of non-2xx response ~p -> ~s", [Branch, Status, sipurl:print(OrigURI)]),
	    {CSeq, _} = sipheader:cseq(keylist:fetch("CSeq", AckHeader))
    end,
    % Don't copy any Via headers. send_proxy_request() will add one for this proxy,
    % and that is the only one that should be in an ACK. RFC 3261 17.1.1.3
    % copy() To and then set() it to preserve order...
    SendHeader1 = keylist:copy(OrigHeader, ["From", "To", "Call-ID"]),
    SendHeader2 = keylist:set("To", keylist:fetch("To", AckHeader), SendHeader1),
    SendHeader3 = keylist:set("CSeq", [sipheader:cseq_print({CSeq, "ACK"})], SendHeader2),
    % Copy Route-header from original request, if present (mandated by RFC 3261 17.1.1.3)
    SendHeader = case keylist:fetch("Route", OrigHeader) of
	[] ->
	    SendHeader3;
	Route ->
	    keylist:set("Route", Route, SendHeader3)
    end,
    siprequest:send_proxy_request(SendHeader, Socket,
    				  {"ACK", OrigURI, "", ["branch=" ++ Branch]}).

get_resendrequest_timer(Match, []) ->
    none;
get_resendrequest_timer(Match, [Timer | Rest]) ->
    case siptimer:extract_appsignal(Timer) of
	{Match, _} ->
	    Timer;
	_ ->
	    get_resendrequest_timer(Match, Rest)
    end.
        
stop_resendrequest_timer(Branch, AppId, Request, TimerList) ->
    {Method, URI, _, _} = Request,
    case siptimer:get_timers_appid_matching(AppId, TimerList) of
	[] ->
	    logger:log(error, "~s: Could not find any timers matching AppId ~p in TimerList :~n~p",
			[Branch, AppId, siptimer:debugfriendly(TimerList)]),
	    TimerList;	    
	AppTimers ->
	    NTList1 = case get_resendrequest_timer(resendrequest, AppTimers) of
		none ->
		    TimerList;
		ResendTimer ->
		    logger:log(debug, "~s: Cancelling resendrequest timer for request ~s ~s", [Branch, Method, sipurl:print(URI)]),
		    siptimer:cancel_timer(ResendTimer, TimerList)
	    end,
	    % If it is an INVITE, we stop the resendrequest_timeout as well
	    % since an invite have {invite_expire} (Timer C) that takes
	    % care of ending it if nothing more is received.
	    case Method of
		"INVITE" ->
		    case get_resendrequest_timer(resendrequest_timeout, AppTimers) of
			none ->
			    NTList1;
			ResendTimeoutTimer ->
			    logger:log(debug, "~s: Cancelling resendrequest_timeout timer for request ~s ~s", [Branch, Method, sipurl:print(URI)]),
			    siptimer:cancel_timer(ResendTimeoutTimer, NTList1)
		    end;
		_ ->
		    NTList1
	    end     
    end.

get_request_resend_timeout("INVITE", OldTimeout, State) ->
    OldTimeout * 2;
get_request_resend_timeout(Method, OldTimeout, State) ->
    % Use Timer T2 as maximum for non-INVITE requests when in 'trying' state.
    % The meaning is that we should resend non-INVITE requests at 500ms, 1s, 2s, 4s, 4s, 4s ...
    % or, if we receive a provisional response and go into proceeding state, every 4s.
    T2 = sipserver:get_env(timerT2, 4000),
    case State of
	trying ->
	    lists:min([OldTimeout * 2, T2]);
	proceeding ->
	    T2
    end.
    
cancel_request(Branch, Socket, OrigRequest, BranchData) ->
    {OrigMethod, OrigURI, OrigHeader, OrigBody} = OrigRequest,
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    case transactionlist:get_transaction_using_header(OrigHeader, TransactionList) of
        none ->
	    logger:log(debug, "~s: Asked to cancel, but no request has been sent. Terminating immediately.", [Branch]),
	    {quit};
        Transaction ->
            TransactionId = transactionlist:extract_id(Transaction),
            CancelRequest = transactionlist:extract_request(Transaction),
	    {CancelReqMethod, CancelReqURI, _, _} = CancelRequest,
	    case CancelReqMethod of
		"INVITE" ->
		    NewTransaction = transactionlist:set_cancelled(Transaction, true),
		    NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
		    case transactionlist:extract_state(Transaction) of
			calling ->
			    logger:log(debug, "~s: NOT sending CANCEL of previos request (INVITE ~s) right now since we are in state 'calling'",
			    		[Branch, sipurl:print(CancelReqURI)]),
			    {TimerData, NewTransactionList};
			proceeding ->
			    %NewTimerList = siptimer:cancel_timers_with_appid({resendrequest, TransactionId}, TimerList),
			    NewTimerList = stop_resendrequest_timer(Branch, {resendrequest, TransactionId}, CancelRequest, TimerList),
			    logger:log(debug, "~s: Cancelling previous request: INVITE ~s", [Branch, sipurl:print(CancelReqURI)]),
			    process_branch_cancel(Branch, Socket, CancelRequest, {{NewTimerList, TimerSeq}, NewTransactionList});
			State ->
			    logger:log(debug, "~s: NOT sending CANCEL of previos request (INVITE ~p) since we are in state '~p'",
			    		[Branch, sipurl:print(CancelReqURI), State]),
			    {TimerData, NewTransactionList}
		    end;
		_ ->
		    % RFC 3261 9.1 says a stateful proxy SHOULD NOT send CANCEL of non-INVITE requests
		    logger:log(debug, "~s: Original request was non-INVITE (~s) - not sending CANCEL. Going into 'terminated' state.", [Branch, CancelReqMethod]),
		    %NewTimerList = siptimer:cancel_timers_with_appid({resendrequest, TransactionId}, TimerList),
		    NewTimerList = stop_resendrequest_timer(Branch, {resendrequest, TransactionId}, CancelRequest, TimerList),
		    NewTransaction = transactionlist:set_state(Transaction, terminated),
		    NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
		    {{NewTimerList, TimerSeq}, NewTransactionList}
	    end
    end.

% end_invite() ends an INVITE transaction. This is called when our {call, ...} timeout
% ocurs, or when Timer C fires. Whichever happens first.
end_invite(Branch, Socket, Parent, OrigRequest, BranchData, Id) ->
    {TimerData, TransactionList} = BranchData,
    Transaction = transactionlist:get_transaction(Id, TransactionList),
    Request = transactionlist:extract_request(Transaction),
    {Method, URI, _, _} = Request,
    case transactionlist:extract_state(Transaction) of
	calling ->
	    fake_request_timeout(Branch, BranchData, Parent, OrigRequest, Request);
	proceeding ->
	    logger:log(debug, "~s: Sending of original request (INVITE ~s) timed out in state 'proceeding' - cancel original request",
	    		[Branch, sipurl:print(URI)]),
	    cancel_request(Branch, Socket, OrigRequest, BranchData);
	State ->
	    logger:log(debug, "~s: Sending of original request (INVITE ~s) timed out in state '~s' - ignoring.",
	    		[Branch, sipurl:print(URI), State]),
	    BranchData
    end.

fake_request_timeout(Branch, BranchData, Parent, OrigRequest, Request) ->
    {Method, URI, Header, _} = Request,
    logger:log(debug, "~s: Client transaction (~s ~s) timed out - pretend we got an 408 Request Timeout",
   		[Branch, Method, sipurl:print(URI)]),
    % Create a fake 408 Request Timeout response to store as EndResult for this Target
    % since RFC 3261 16.7 says we MUST act as if such a response was received
    FakeResponse = serverbranch:make_response(408, "Request Timeout", Request),
    Parent ! {branch_result, Branch, terminated, FakeResponse, OrigRequest},
    terminate_transaction_header(Branch, BranchData, Header).

terminate_transaction(Branch, BranchData, Transaction) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    TransactionId = transactionlist:extract_id(Transaction),
    NewTransaction = transactionlist:set_state(Transaction, terminated),
    NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
    NewTimerList = siptimer:cancel_timers_with_appid({resendrequest, TransactionId}, TimerList),
    {{NewTimerList, TimerSeq}, NewTransactionList}.

terminate_transaction_id(Branch, BranchData, TransactionId) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    case transactionlist:get_transaction(TransactionId, TransactionList) of
	none ->
	    logger:log(error, "~s: Asked to terminate unknown transaction with id ~p", [Branch, TransactionId]),
	    logger:log(debug, "~s: TransactionList where id ~p was not found :~n~p",
	    		[Branch, TransactionId, transactionlist:debugfriendly(TransactionList)]),
	    BranchData;
	Transaction ->
	    terminate_transaction(Branch, BranchData, Transaction)
    end.

terminate_transaction_header(Branch, BranchData, Header) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    case transactionlist:get_transaction_using_header(Header, TransactionList) of
	none ->
	    CSeq = sipheader:cseq(keylist:fetch("CSeq", Header)),
	    logger:log(error, "~s: Asked to terminate unknown transaction using headers with CSeq ~p", [Branch, CSeq]),
	    logger:log(debug, "~s: TransactionList searched :~n~p", [Branch, transactionlist:debugfriendly(TransactionList)]),
	    BranchData;
	Transaction ->
	    terminate_transaction(Branch, BranchData, Transaction)
    end.

% Send a SIP-message and arrange for it to be resent as per the SIP specification.
% The first send_after() causes the message to be resent,
% the second send_after() will cause us to stop resending this message and
% the third send_after() will start sending CANCELs to this request, if reached.
% Start looping process_branch_loop() for this branch.
process_request(Branch, Socket, Timeout, {"INVITE", URI, Header, Body}, TimerData) ->
    {TimerList, TimerSeq} = TimerData,
    logger:log(normal, "~s: Sending INVITE -> ~s", [Branch, sipurl:print(URI)]),
    siprequest:send_proxy_request(Header, Socket,
				  {"INVITE", URI, Body, ["branch=" ++ Branch]}),
    T1 = sipserver:get_env(timerT1, 500),
    TimerA = T1,
    TimerB = 64 * T1,
    % RFC 3261 says that Timer C for INVITE requests MUST be greater than 3 minutes
    TimerC = 181 * 1000,
    InviteTimeout = Timeout * 1000,
    ADesc = "resendrequest INVITE to " ++ sipurl:print(URI) ++ " (TimerA)",
    BDesc = "stopresend of INVITE to " ++ sipurl:print(URI) ++ " (TimerB)",
    CDesc = "invite_expire of INVITE to " ++ sipurl:print(URI) ++ " (TimerC)",
    TimeoutDesc = "invite_timeout of INVITE to " ++ sipurl:print(URI) ++ " after " ++ integer_to_list(Timeout) ++ " seconds",
    Id = sipheader:cseq(keylist:fetch("CSeq", Header)),
    Request = {"INVITE", URI, Header, Body},
    NTList1 = siptimer:add_timer(TimerSeq + 1, TimerA, ADesc, {resendrequest, Request}, {resendrequest, Id}, TimerList),
    NTList2 = siptimer:add_timer(TimerSeq + 2, TimerB, BDesc, {resendrequest_timeout, Request}, {resendrequest, Id}, NTList1),
    NTList3 = siptimer:add_timer(TimerSeq + 3, TimerC, CDesc, {invite_expire, Id}, {resendrequest, Id}, NTList2),
    NTList4 = siptimer:add_timer(TimerSeq + 4, InviteTimeout, TimeoutDesc, {invite_timeout, Id}, {resendrequest, Id}, NTList3),
    {NTList4, TimerSeq + 4};
process_request(Branch, Socket, Timeout, Request, TimerData) ->
    {Method, URI, Header, Body} = Request,
    {TimerList, TimerSeq} = TimerData,
    logger:log(normal, "~s: Sending ~s -> ~s", [Branch, Method, sipurl:print(URI)]),
    siprequest:send_proxy_request(Header, Socket,
				  {Method, URI, Body, ["branch=" ++ Branch]}),
    T1 = sipserver:get_env(timerT1, 500),
    TimerE = T1,
    TimerF = 64 * T1,
    EDesc = "resendrequest " ++ Method ++ " to " ++ sipurl:print(URI) ++ " (Timer E)",
    FDesc = "stopresend of " ++ Method ++ " to " ++ sipurl:print(URI) ++ " (Timer F)",
    Id = sipheader:cseq(keylist:fetch("CSeq", Header)),
    NTList1 = siptimer:add_timer(TimerSeq + 1, TimerE, EDesc, {resendrequest, Request}, {resendrequest, Id}, TimerList),
    NTList2 = siptimer:add_timer(TimerSeq + 2, TimerF, FDesc, {resendrequest_timeout, Request}, {resendrequest, Id}, NTList1),
    {NTList2, TimerSeq + 2}.

% This branches dialogue should be cancelled. OrigRequest is the request that should
% be cancelled.
process_branch_cancel(Branch, Socket, OrigRequest, BranchData) ->
    {TimerData, TransactionList} = BranchData,
    {Method, URI, OrigHeader, Body} = OrigRequest,
    logger:log(debug, "~s: process_branch_cancel() Sending CANCEL -> ~s", [Branch, sipurl:print(URI)]),
    Id = sipheader:cseq(keylist:fetch("CSeq", OrigHeader)),
    {CSeq, _} = Id,
    CancelId = {CSeq, "CANCEL"},
    % Delete all Via headers. process_request() uses send_proxy_request() which 
    % will add one for this proxy, and that is the only one that should be in a
    % CANCEL. Set CSeq method to CANCEL and delete all Require and Proxy-Require
    % headers. All this according to RFC 3261 9.1 Client Behaviour.
    CancelHeader1 = keylist:set("CSeq", [sipheader:cseq_print(CancelId)],
			    OrigHeader),
    CancelHeader2 = keylist:delete("Via", CancelHeader1),
    CancelHeader3 = keylist:delete("Require", CancelHeader2),
    CancelHeader4 = keylist:delete("Proxy-Require", CancelHeader3),
    CancelHeader5 = keylist:delete("Content-Type", CancelHeader4),
    CancelHeader6 = keylist:delete("Content-Length", CancelHeader5),
    CancelHeader = keylist:set("Content-Length", ["0"], CancelHeader6),
    CancelRequest = {"CANCEL", URI, CancelHeader, ""},
    {NTList1, NTSeq} = process_request(Branch, Socket, 0, CancelRequest, TimerData),
    NTList2 = siptimer:add_timer(NTSeq + 1, 64 * 1000, "quit after CANCEL", {terminate_transaction, CancelId}, {resendrequest, CancelId}, NTList1),
    NewTransactionList = transactionlist:add_transaction(CancelRequest, none, trying, TransactionList),
    logger:log(debug, "~s: TransactionList with new CANCEL transaction added :~n~p", [Branch, transactionlist:debugfriendly(NewTransactionList)]),
    {{NTList2, NTSeq + 1}, NewTransactionList}.
