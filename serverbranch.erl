-module(serverbranch).
-export([start/5, make_response/3]).

% OrigRequest = {Method, ReqURI, Header, Body}

% Spawn a new erlang process ready to handle a server branch. This process can handle multiple transactions.
start(Branch, Socket, FromIP, Parent, OrigRequest) ->
    {ReqMethod, ReqURI, ReqHeader, ReqBody} = OrigRequest,
    LogStr = sipserver:make_logstr({request, ReqMethod, ReqURI, ReqHeader, ReqBody}, FromIP),
    % RFC 3261 16.2 says a stateful proxy SHOULD NOT generate provisional responses for non-INVITE requests
    {State, Response} = case ReqMethod of
	"INVITE" ->
	    Response1 = make_response(100, "Trying", {ReqMethod, ReqURI, ReqHeader, ""}),
	    process_sendresponse(Branch, Socket, OrigRequest, Response1, {[], 0}, false),
	    {proceeding, Response1};
	_ ->
	    {trying, none}
    end,
    TimerData = {[], 0},
    BranchData = {TimerData, transactionlist:add_transaction(OrigRequest, Response, State, [])},
    Pid = sipserver:safe_spawn(fun process_branch_loop/5, [Branch, Socket, Parent, OrigRequest, BranchData]),
    logger:log(normal, "~s: ~s -> Started server transaction (pid ~p, parent ~p)", [Branch, LogStr, Pid, Parent]),
    Pid.

% This is the main loop for a server branch. Receive messages and act on them.
process_branch_loop(Branch, Socket, Parent, OrigRequest, BranchData) ->
    {OrigMethod, OrigURI, OrigHeader, OrigBody} = OrigRequest,
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    NewBranchData = receive

	{request, {"ACK", URI, Header, Body}} ->
	    case transactionlist:get_transaction_using_header(Header, TransactionList) of
		none ->
		    CSeq = sipheader:cseq(keylist:fetch("CSeq", Header)),
		    logger:log(error, "~s: Received ACK of unknown request (CSeq ~p) - ignoring (XXX is this correct?)", [Branch, CSeq]),
		    BranchData;
		AckToTransaction ->
		    Request = {"ACK", URI, Header, Body},
		    process_received_ack(Branch, Socket, Parent, BranchData, Request, AckToTransaction)
	    end;

	{request, Request} ->
	    {Method, URI, Header, Body} = Request,
	    case transactionlist:get_transaction_using_header(Header, TransactionList) of
		none ->
		    case Method of
			"CANCEL" ->
			    {CSeq, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
			    case transactionlist:get_transaction({CSeq, "INVITE"}, TransactionList) of
				none ->
				    logger:log(error, "~s: Received CANCEL of unknown request (CSeq ~p) - responding 481 Call/Transaction Does Not Exist", [Branch, {CSeq, "INVITE"}]),
				    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist"),
				    BranchData;
				CancelOfTransaction ->
				    Request = {"CANCEL", URI, Header, Body},
				    process_received_cancel(Branch, Socket, Parent, OrigRequest, BranchData, Request, CancelOfTransaction)
			    end;
			_ ->
			    logger:log(error, "~s: Server branch received a request (~s ~s) that was neither a resend, an ACK nor a CANCEL - such requests should NOT be passed to the server branch!",
					[Branch, Method, sipurl:print(URI)]),
			    logger:log(debug, "~s: debugfriendly(TransactionList) :~n~p", [Branch, transactionlist:debugfriendly(TransactionList)]),
			    siprequest:send_result(Header, Socket, "", 500, "Internal Server Error"),
			    BranchData
		    end;
		Transaction ->
		    case transactionlist:extract_response(Transaction) of
			none ->
			    logger:log(debug, "~s: Server branch received a resent request (~s ~s), but I have not sent any response yet so I can't resend a response. Ignoring.",
					[Branch, Method, sipurl:print(URI)]),
			    BranchData;
			MostRecentResponse ->
			    {Status, Reason, ResHeader, ResBody} = MostRecentResponse,
			    logger:log(normal, "~s: Received duplicate request (~s ~s), resending last response: ~p ~s",
					[Branch, Method, sipurl:print(URI), Status, Reason]),
			    siprequest:send_proxy_response(Socket, Status, Reason, ResHeader, ResBody),
			    BranchData
		    end
	    end;	    
	
	{forwardresponse, Response} ->
	    % proxying of a response unreliably (for 1xx and 2xx responses)
	    {Status, Reason, _, _} = Response,
	    send_response(Branch, Socket, OrigRequest, Response, BranchData);
	{sendresponse, Status, Reason} ->
	    % send response created from original request
	    siptimer:cancel_all_timers(TimerList),
	    {Method, URI, Header, _} = OrigRequest,
	    Response = make_response(Status, Reason, {Method, URI, Header, ""}),
	    send_response(Branch, Socket, OrigRequest, Response, BranchData);

	{siptimer, TSeq, TDesc} ->
	    case siptimer:get_timer(TSeq, TimerList) of
	        none ->
	            logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring.", [Branch, TSeq, TDesc]),
	            BranchData;
	        Timer ->
		    process_timer(Branch, Socket, Parent, OrigRequest, BranchData, Timer)
	    end;

	{showtransactions} ->
	    logger:log(debug, "~s: Received 'showtransactions' request, debugfriendly(TransactionList) :~n~p",
    			[Branch, transactionlist:debugfriendly(TransactionList)]),
    	    BranchData;

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
	    logger:log(debug, "~s: Server branch terminating, signalling parent Pid ~p", [Branch, Parent]),
	    Parent ! {serverbranch_terminating, {Branch, self()}},
	    logger:log(normal, "~s: ending server branch, transaction ~s ~s",
	    	       [Branch, OrigMethod, sipurl:print(OrigURI)]),
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

	{resendresponse, {Response, OrigRequest}} ->
	    {Status, Reason, RHeader, RBody} = Response,
	    {OrigReqMethod, OrigReqURI, OrigReqHeader, _} = OrigRequest,
	    Transaction = transactionlist:get_transaction_using_header(OrigReqHeader, TransactionList),
	    State = transactionlist:extract_state(Transaction),
	    case State of
		completed ->
		    logger:log(normal, "~s: Resend after ~p (response to ~s ~s): ~p ~s",
		    	       [Branch, Timeout div 1000, OrigReqMethod, sipurl:print(OrigReqURI), Status, Reason]),
		    siprequest:send_proxy_response(Socket, Status, Reason, RHeader, RBody),
		    NewTimeout = case OrigReqMethod of
			"INVITE" ->
			    % Use Timer Value T2 for INVITE responses
			    T2 = sipserver:get_env(timerT2, 4000),
			    lists:min([Timeout * 2, T2]);
			_ ->
			    Timeout * 2
		    end,
		    NewTimerList = siptimer:revive_timer(Timer, NewTimeout, TimerList),
		    {{NewTimerList, TimerSeq}, TransactionList};
		_ ->
		    logger:log(debug, "~s: Ignoring signal to resend response to (~s ~s): ~p ~s since we are in state '~p'",
		    	       [Branch, OrigReqMethod, sipurl:print(OrigReqURI), Status, Reason, State]),
		    BranchData
	    end;

	{resendresponse_timeout, {Response, OrigRequest}} ->
	    {Status, Reason, _, _} = Response,
	    {OrigReqMethod, OrigReqURI, OrigReqHeader, _} = OrigRequest,
	    logger:log(normal, "~s: Sending of ~p ~s (response to ~s ~s) timed out after ~p seconds",
		    	       [Branch, Status, Reason, OrigReqMethod, sipurl:print(OrigReqURI), Timeout div 1000]),
	    Transaction = transactionlist:get_transaction_using_header(OrigReqHeader, TransactionList),
	    Id = transactionlist:extract_id(Transaction),
	    {quit};

	{quit} ->
	    logger:log(debug, "~s: Received timer signal to quit", [Branch]),
	    {quit};

	_ ->
	    logger:log(error, "~s: Received unknown signal from timer ~p: ~p", [Branch, TSeqNo, Signal]),
	    BranchData
    end.	

send_response(Branch, Socket, OrigRequest, Response, BranchData) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    {Status, Reason, ResHeader, ResBody} = Response,
    case transactionlist:get_transaction_using_response(Response, TransactionList) of
	none ->
	    CSeq = sipheader:cseq(keylist:fetch("CSeq", ResHeader)),
	    logger:log(error, "~s: Asked to send response ~p ~s (CSeq ~p) to a request I have not received. Ignoring.",
	    		[Branch, Status, Reason, CSeq]),
	    BranchData;
	Transaction ->
	    ResponseToRequest = transactionlist:extract_request(Transaction),
	    OldState = transactionlist:extract_state(Transaction),
	    TransactionId = transactionlist:extract_id(Transaction),
	    {ResponseToMethod, ResponseToURI, ResponseToHeader, _} = ResponseToRequest,
	    {Action, SendReliably, NewState} = send_response_statemachine(ResponseToMethod, Status, OldState),

	    NewBranchData = case Action of
		ignore ->
		    BranchData;
		send ->
		    logger:log(normal, "~s: Responding ~p ~s to request ~s ~s", [Branch, Status, Reason, ResponseToMethod, sipurl:print(ResponseToURI)]),
		    NewTimerData = process_sendresponse(Branch, Socket, ResponseToRequest, Response, TimerData, SendReliably),
		    NewTransaction = transactionlist:set_response(Transaction, Response),
		    NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
		    {NewTimerData, NewTransactionList}
	    end,
	    act_on_new_state(Branch, NewBranchData, TransactionId, OldState, NewState)
    end.

act_on_new_state(Branch, BranchData, TransactionId, OldState, NewState) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    Transaction = transactionlist:get_transaction(TransactionId, TransactionList),
    case NewState of
	OldState ->
	    BranchData;
	_ ->
	    NewTransaction = transactionlist:set_state(Transaction, NewState),
	    NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
	    ResponseToRequest = transactionlist:extract_request(NewTransaction),
	    {ResponseToMethod, ResponseToURI, ResponseToHeader, _} = ResponseToRequest,
	    case NewState of
	        completed ->
		    ResponseToId = sipheader:cseq(keylist:fetch("CSeq", ResponseToHeader)),
		    case ResponseToMethod of
			"INVITE" ->
			    {TimerData, NewTransactionList};
			_ ->
			    T1 = sipserver:get_env(timerT1, 500),
			    TimerJ = 64 * T1,
			    logger:log(debug, "~s: Entering state 'completed'. Original request was non-INVITE, starting Timer J with a timeout of ~p seconds.",
			    		[Branch, TimerJ]),
			    % Install TimerJ (default 32 seconds) RFC 3261 17.2.2. Until TimerJ fires we
			    % resend our response whenever we receive a request resend.
			    JDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++ " (Timer J)",
			    NTList1 = siptimer:add_timer(TimerSeq + 1, TimerJ, JDesc, {quit}, {timerJ}, TimerList),
			    {{NTList1, TimerSeq + 1}, NewTransactionList}
		    end;
		confirmed ->
		    ResponseToId = sipheader:cseq(keylist:fetch("CSeq", ResponseToHeader)),
		    NTList1 = siptimer:cancel_timers_with_appid({resendresponse, ResponseToId}, TimerList),
		    case ResponseToMethod of
			"INVITE" ->
			    TimerI = sipserver:get_env(timerT4, 5000),
			    logger:log(debug, "~s: Entering state 'confirmed'. Original request was an INVITE, starting Timer I with a timeout of ~p seconds.",
			    		[Branch, TimerI div 1000]),
			    % Install TimerI (T4, default 5 seconds) RFC 3261 17.2.1. Until TimerI fires we
			    % absorb any additional ACK requests that might arrive.
			    IDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++ " (Timer I)",
			    NTList2 = siptimer:add_timer(TimerSeq + 1, TimerI, IDesc, {quit}, {timerI}, NTList1),
			    {{NTList2, TimerSeq + 1}, NewTransactionList};
			_ ->
			    logger:log(error, "~s: Entered state 'confirmed'. Original request was NOT an INVITE (it was ~s ~s). How could this be?",
			    		[Branch, ResponseToMethod, sipurl:print(ResponseToURI)]),
			    {{NTList1, TimerSeq}, NewTransactionList}
		    end;
		_ ->
		    {TimerData, NewTransactionList}
	    end
    end.

send_response_statemachine(Method, Status, trying) when Status =< 199 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'proceeding' - doing so (unreliably) and entering state 'proceeding'", [Status, Method]),
    {send, false, proceeding};

send_response_statemachine(Method, Status, proceeding) when Status =< 199 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'proceeding' - doing so (unreliably)", [Status, Method]),
    {send, false, proceeding};


send_response_statemachine("INVITE", Status, proceeding) when Status =< 299 ->
    logger:log(debug, "UAS decision: Requested to send 2xx response ~p to INVITE when in state 'proceeding' - doing so (unreliably) and entering state 'terminated'", [Status]),
    {send, false, terminated};


send_response_statemachine("INVITE", Status, proceeding) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 3xx, 4xx, 5xx or 6xx response ~p to INVITE when in state 'proceeding' - doing so (reliably) and entering state 'completed'", [Status]),
    {send, true, completed};

send_response_statemachine(Method, Status, trying) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when in state 'trying' - doing so (unreliably) and entering state 'completed'", [Status, Method]),
    {send, false, completed};

send_response_statemachine(Method, Status, proceeding) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when in state 'proceeding' - doing so (unreliably) and entering state 'completed'", [Status, Method]),
    {send, false, completed};

send_response_statemachine(Method, Status, completed) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when already in state 'completed' - ignoring", [Status, Method]),
    {ignore, false, completed};

send_response_statemachine(Method, Status, terminated) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send response ~p to ~s when in state 'terminated' - ignoring", [Status, Method]),
    {ignore, false, terminated}.


process_received_ack(Branch, Socket, Parent, BranchData, Request, AckToTransaction) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    AckToRequest = transactionlist:extract_request(AckToTransaction),
    AckToResponse = transactionlist:extract_response(AckToTransaction),
    {Status, Reason, _, _} = AckToResponse,
    OldState = transactionlist:extract_state(AckToTransaction),
    Id = transactionlist:extract_id(AckToTransaction),
    {AckToMethod, AckToURI, _, _} = AckToRequest,
    logger:log(normal, "~s: Response ~p ~s to request ~s ~s ACK-ed", [Branch, Status, Reason, AckToMethod, sipurl:print(AckToURI)]),
    case AckToMethod of
	"INVITE" ->
	    logger:log(debug, "~s: Received ACK, cancelling resend timers for response ~p ~s (to request ~s ~s) and entering state 'confirmed'",
	    		[Branch, Status, Reason, AckToMethod, sipurl:print(AckToURI)]),
	    NewTimerList = siptimer:cancel_timers_with_appid({resendresponse, Id}, TimerList),
	    NewTransaction1 = transactionlist:set_acked(AckToTransaction, true),
	    NewTransaction = transactionlist:set_state(NewTransaction1, confirmed),
	    NewTransactionList = transactionlist:update_transaction(NewTransaction, TransactionList),
	    NewBranchData = {{NewTimerList, TimerSeq}, NewTransactionList},
	    act_on_new_state(Branch, NewBranchData, Id, OldState, confirmed);
	_ ->
	    logger:log(debug, "~s: Received ACK to non-INVITE request ~s ~s (response being ACKed is ~p ~s) - ignoring",
			[Branch, AckToMethod, sipurl:print(AckToURI), Status, Reason]),
	    BranchData
    end.
    
process_received_cancel(Branch, Socket, Parent, OrigRequest, BranchData, Request, CancelOfTransaction) ->
    {TimerData, TransactionList} = BranchData,
    {TimerList, TimerSeq} = TimerData,
    {_, URI, Header, Body} = Request,
    CancelOfId = transactionlist:extract_id(CancelOfTransaction),
    CancelOfRequest = transactionlist:extract_request(CancelOfTransaction),
    {CancelOfMethod, CancelOfURI, CancelOfHeader, _} = CancelOfRequest,
    {OrigReqMethod, OrigReqURI, OrigReqHeader, _} = OrigRequest,
    OrigRequestId = sipheader:cseq(keylist:fetch("CSeq", OrigReqHeader)),
    AppTimers = siptimer:get_timers_appid_matching(CancelOfId, TimerList),
    logger:log(debug, "~s: Received CANCEL ~s, cancelling resendresponse timers for request: ~s ~s",
    		[Branch, sipurl:print(URI), CancelOfMethod, sipurl:print(CancelOfURI)]),
    NewTimerList = siptimer:cancel_timers_with_appid({resendresponse, CancelOfId}, TimerList),
    Response = make_response(200, "OK", Request),
    siprequest:send_result(Header, Socket, "", 200, "OK"),
    case CancelOfId of
	OrigRequestId ->
	    logger:log(normal, "~s: Original request (~s ~s) cancelled", [Branch, OrigReqMethod, sipurl:print(OrigReqURI)]),
	    Parent ! {request_cancelled, OrigRequest, {"CANCEL", URI, Header, Body}};
	_ ->
	    logger:log(debug, "~s: Cancel was not of original request - not informing parent", [Branch])
    end,
    NewTransaction = transactionlist:set_cancelled(CancelOfTransaction, true),
    NewTransactionList1 = transactionlist:update_transaction(NewTransaction, TransactionList),
    NewTransactionList = transactionlist:add_transaction({"CANCEL", URI, Header, Body}, Response, completed, NewTransactionList1),
    {{NewTimerList, TimerSeq}, NewTransactionList}.


% This branch should answer something.
process_sendresponse(Branch, Socket, OrigRequest, Response, TimerData, SendReliably) ->
    {TimerList, TimerSeq} = TimerData,
    {Status, Reason, ResHeader, ResBody} = Response,
    {_, OrigURI, OrigHeader, _} = OrigRequest,
    Id = sipheader:cseq(keylist:fetch("CSeq", ResHeader)),
    {_, ResponseToMethod} = Id,
    logger:log(debug, "~s: Sending response to ~s ~s : ~p ~s", 
    		[Branch, ResponseToMethod, sipurl:print(OrigURI), Status, Reason]),
    siprequest:send_proxy_response(Socket, Status, Reason, ResHeader, ResBody),
    NewTimerData = case SendReliably of
	true ->
	    T1 = sipserver:get_env(timerT1, 500),
	    case ResponseToMethod of
		"INVITE" ->
		    TimerG = T1,
		    TimerH = 64 * T1,
		    TDesc1 = "resendresponse " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++ sipurl:print(OrigURI) ++ " (Timer G)",
		    TDesc2 = "stopresend of " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++ sipurl:print(OrigURI) ++ " (Timer H)",
		    NTList1 = siptimer:add_timer(TimerSeq + 1, TimerG, TDesc1, {resendresponse, {Response, OrigRequest}}, {resendresponse, Id}, TimerList),
		    NTList2 = siptimer:add_timer(TimerSeq + 2, TimerH, TDesc2, {resendresponse_timeout, {Response, OrigRequest}}, {resendresponse, Id}, NTList1),
		    {NTList2, TimerSeq + 2};
		_ ->
		    TimerJ = 64 * T1,
		    TDesc1 = "terminate server transaction after response " ++ integer_to_list(Status) ++ " " ++ Reason ++ " has been sent to " ++ sipurl:print(OrigURI) ++ " (Timer J)",
		    NTList1 = siptimer:add_timer(TimerSeq + 1, TimerJ, TDesc1, {quit}, {timerJ}, TimerList),
		    {NTList1, TimerSeq + 1}
	    end;
	false ->
	    TimerData
    end.				

% Create a response that can be passed to process_sendresponse() from a request header.
make_response(Status, Reason, OrigRequest) ->
    {Method, URI, Header, Body} = OrigRequest,
    AnswerHeader1 = keylist:copy(Header, ["Via", "From", "To", "Call-ID", "CSeq",
					  "Record-Route", "Timestamp", "Content-Type"]),
    % PlaceHolderVia is an EXTRA Via with our hostname. We could do without this if
    % we sent it with send_response() instead of send_proxy_response() but it is easier
    % to just add an extra Via that will be stripped by send_proxy_response() and don't
    % have to make a difference in how we send out responses.
    PlaceHolderVia = sipheader:via_print([{"SIP/2.0/UDP",
				     {siprequest:myhostname(),
				      siprequest:default_port(sipserver:get_env(listenport, none))},
				     []}]),
    Via = sipheader:via(keylist:fetch("Via", Header)),
    AnswerHeader2 = keylist:prepend({"Via", PlaceHolderVia}, AnswerHeader1),
    AnswerHeader3 = siprequest:make_answerheader(AnswerHeader2),
    % If there is a body, calculate Content-Length, otherwise remove the Content-Type we copied above
    AnswerHeader4 = case Body of
	"" -> keylist:delete("Content-Type", AnswerHeader3);
	_ -> keylist:set("Content-Length", [integer_to_list(length(Body))], AnswerHeader3)
    end,
    % If this is not a 100 Trying response, remove the Timestamp we copied above.
    % The preservation of Timestamp headers into 100 Trying response is mandated by RFC 3261 8.2.6.1
    AnswerHeader5 = case Status of
	100 -> AnswerHeader4;
	_ -> keylist:delete("Timestamp", AnswerHeader4)
    end,
    {Status, Reason, AnswerHeader5, Body}.
