-module(sippipe).
-export([start/7]).

-record(state, {branch, serverhandler, clienthandler, request, dsturi, dstlist, parameters, timeout}).

start(ServerHandler, ClientPid, Request, DstURI, DstList, Parameters, Timeout) ->
    case transactionlayer:is_good_transaction(ServerHandler) of
	true ->
	    Branch = get_branch_from_handler(ServerHandler) ++ "-UAC",
	    start(Branch, ServerHandler, ClientPid, Request, DstURI, DstList, Parameters, Timeout);
	false ->
	    case transactionlayer:get_handler_for_request(Request) of
		{error, E} ->
		    logger:log(error, "sippipe: Could not start pipe, no valid server transaction " ++
				"supplied (~p) and none found using transaction layer, error : ~p",
				[ServerHandler, E]),
		    error;
		S ->
		    Branch = get_branch_from_handler(S) ++ "-UAC",
		    start(Branch, S, ClientPid, Request, DstURI, DstList, Parameters, Timeout)		    
	    end	
    end.

start(Branch, ServerHandler, ClientPid, Request, DstURI, DstListIn, Parameters, Timeout) when ClientPid == none ->
    {Method, URI, Header, Body} = Request,
    {_, UseDstURI, _} = siprequest:rewrite_route(Header, DstURI),
    DstList = case DstListIn of
	none -> siprequest:url_to_dstlist(UseDstURI, 500); % XXX do better estimation
	_ when list(DstListIn) -> DstListIn
    end,
    [FirstDst|_] = DstList,
    SendRequest = {Method, DstURI, Header, Body},
    case transactionlayer:start_client_transaction(SendRequest, none, FirstDst, Branch, Timeout, self()) of
    	BranchPid when pid(BranchPid) ->
	    start(Branch, ServerHandler, BranchPid, Request, DstURI, DstList, Parameters, Timeout);
	{error, E} ->
	    logger:log(error, "sippipe: Failed starting client transaction : ~p", [E]),
	    error
    end;

start(Branch, ServerHandler, ClientPid, Request, DstURI, DstList, Parameters, Timeout) ->
    case transactionlayer:adopt_server_transaction_handler(ServerHandler) of
	{error, E} ->
	    logger:log(error, "sippipe: Could not adopt server transaction handler ~p : ~p", [ServerHandler, E]),
	    error;
	_ ->
	    State=#state{branch=Branch, serverhandler=ServerHandler, clienthandler=ClientPid,
			 request=Request, dsturi=DstURI, dstlist=DstList, parameters=Parameters,
			 timeout=Timeout},
	    loop(State)
    end.

get_branch_from_handler(TH) ->
    CallBranch = transactionlayer:get_branch_from_handler(TH),
    case string:rstr(CallBranch, "-UAS") of
	0 ->
	    CallBranch;
	Index when integer(Index) ->
	    BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
    end.

loop(State) when record(State, state) ->

    ServerHandlerPid = transactionlayer:get_pid_from_handler(State#state.serverhandler),

    {Res, NewState} = receive

	{servertransaction_cancelled, ServerHandlerPid} ->
	    gen_server:cast(State#state.clienthandler, {cancel, "server transaction cancelled"}),
	    {ok, State};

	{branch_result, Branch, NewTransactionState, Response} ->
	    NewState1 = process_client_transaction_response(Response, State),
	    {ok, NewState1};

	{servertransaction_terminating, _} ->
	    NewState1 = State#state{serverhandler=none},
	    {ok, NewState1};
	    	    	
	{clienttransaction_terminating, {Branch, ClientPid}} ->
	    NewState1 = State#state{clienthandler=none},
	    {ok, NewState1};
	    	    	
	Msg ->
	    logger:log(error, "sippipe: Received unknown message ~p, ignoring", [Msg]),
	    {error, State}
    after
	300 * 1000 ->
	    logger:log(error, "sippipe: Warning: pipe process still alive!~nClient handler : ~p, Server handler : ~p",
			[State#state.clienthandler, State#state.serverhandler]),
	    {error, State}
    end,
    case Res of
	quit ->
	    ok;
	_ ->
	    case all_terminated(State) of
		true ->
		    ok;
		false ->
		    loop(NewState)
	    end
    end.

process_client_transaction_response(Response, State) when record(State, state) ->
    {Status, Reason, _, _} = Response,
    case Status of
	503 ->
	    DstListIn = State#state.dstlist,
	    case DstListIn of
		[FirstDst] ->
		    logger:log(debug, "sippipe: Received ~p ~s, but there are no more destinations " ++
					"to try for this target - telling server transaction to answer 500 No reachable destination",
				[Status, Reason]),
		    % RFC3261 #16.7 bullet 6 says we SHOULD generate a 500 if a 503 is the best we've got
		    transactionlayer:send_response_handler(State#state.serverhandler, 500, "No reachable destination");
		[FailedDst | DstList] ->
		    Branch = State#state.branch,
		    NewBranch = get_next_target_branch(Branch),
		    logger:log(debug, "sippipe: Received ~p ~s, starting new branch ~p for next destination",
				[Status, Reason, NewBranch]),
		    Request = State#state.request,
		    Timeout = State#state.timeout,
		    [FirstDst | _] = DstList,
		    % XXX ideally we should not try to contact the same host over UDP, when
		    % we receive a transport layer error for TCP, if there were any other
		    % equally preferred hosts in the SRV response for a destination.
		    % It makes more sense to try to connect to Proxy B over TCP/UDP than to
		    % try Proxy A over UDP when Proxy A over TCP has just failed.
		    {Method, URI, Header, Body} = Request,
		    DstURI = State#state.dsturi,
		    SendRequest = {Method, DstURI, Header, Body},
		    NewState = case transactionlayer:start_client_transaction(SendRequest, none, FirstDst, NewBranch, Timeout, self()) of
		    	BranchPid when pid(BranchPid) ->
			    State#state{clienthandler=BranchPid, branch=NewBranch, dstlist=DstList};
			{error, E} ->
			    logger:log(error, "sippipe: Failed starting client transaction : ~p", [E]),
			    transactionlayer:send_response_handler(State#state.serverhandler, 500, "Server Internal Error"),
			    State
		    end,
		    NewState
	    end;
	_ ->
	    transactionlayer:send_proxy_response_handler(State#state.serverhandler, Response),
	    State
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
    
all_terminated(State) when record(State, state) ->
    ServerAlive = transactionlayer:is_good_transaction(State#state.serverhandler),
    ClientAlive = case util:safe_is_process_alive(State#state.clienthandler) of
	{true, _} -> true;
	_ -> false
    end,
    if
	ServerAlive == true -> false;
	ClientAlive == true -> false;
	true -> true
    end.
