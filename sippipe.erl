-module(sippipe).
-export([start/6]).

-record(state, {branch, serverhandler, clienthandler, request, dstlist, parameters, timeout}).

-include("sipsocket.hrl").
-include("siprecords.hrl").

start(ServerHandler, ClientPid, Request, DstList, Parameters, Timeout) when record(Request, request) ->
    CompatRequest = {Request#request.method, Request#request.uri, Request#request.header, Request#request.body},
    start(ServerHandler, ClientPid, CompatRequest, DstList, Parameters, Timeout);

start(ServerHandler, ClientPid, Request, {User, Pass, Host, Port, URIParam}, Parameters, Timeout) ->
    %% URI input
    URI = {User, Pass, Host, Port, URIParam},
    Dst = #sipdst{uri=URI},
    logger:log(debug, "sippipe: Made sipdst record out of URI input : ~s", [sipurl:print(URI)]),
    start(ServerHandler, ClientPid, Request, [Dst], Parameters, Timeout);

%% First, we want to get the branch base from an existing ServerHandler
start(ServerHandler, ClientPid, Request, DstList, Parameters, Timeout) when list(DstList) ->
    case transactionlayer:is_good_transaction(ServerHandler) of
	true ->
	    %% Adopt server transaction and then continue
	    case adopt_st_and_get_branch(ServerHandler) of
		error ->
		    transactionlayer:send_response_handler(ServerHandler, 500, "Server Internal Error"),
		    error;
		Branch ->
		    start2(Branch, ServerHandler, ClientPid, Request, DstList, Parameters, Timeout)
	    end;
	false ->
	    %% Either dead transaction handler, or none supplied. Try to find a valid handler
	    %% using the request.
	    case transactionlayer:get_handler_for_request(Request) of
		{error, E} ->
		    logger:log(error, "sippipe: Could not start pipe, no valid server transaction " ++
			       "supplied (~p) and none found using transaction layer, error : ~p",
			       [ServerHandler, E]),
		    error;
		STHandler ->
		    %% Ok, found the server transaction handler. Adopt it and query it for it's branch base.
		    case adopt_st_and_get_branch(STHandler) of
			error ->
			    transactionlayer:send_response_handler(STHandler, 500, "Server Internal Error"),
			    error;
			Branch ->
			    start2(Branch, STHandler, ClientPid, Request, DstList, Parameters, Timeout)
		    end
	    end	
    end.

%% A client pid was supplied to us, go to work
start2(Branch, ServerHandler, ClientPid, Request, DstListIn, Parameters, Timeout) when ClientPid /= none ->
    start3(Branch, ServerHandler, ClientPid, Request, DstListIn, Parameters, Timeout);

%% No client transaction handler is specified, start a client transaction on the first
%% element from the destination list, after making sure it is complete.
start2(Branch, ServerHandler, _, Request, [Dst | DstT], Parameters, Timeout) when record(Dst, sipdst) ->
    DstList = resolve_if_necessary([Dst | DstT]),
    {Method, _, Header, Body} = Request,
    [FirstDst|_] = DstList,
    {_, DstURI, _} = siprequest:rewrite_route(Header, FirstDst#sipdst.uri),
    SendRequest = {Method, DstURI, Header, Body},
    case transactionlayer:start_client_transaction(SendRequest, none, FirstDst, Branch, Timeout, self()) of
    	BranchPid when pid(BranchPid) ->
	    start3(Branch, ServerHandler, BranchPid, Request, DstList, Parameters, Timeout);
	{error, E} ->
	    logger:log(error, "sippipe: Failed starting client transaction : ~p", [E]),
	    error
    end.

%% Do piping between a now existing server and client transaction handler.
start3(Branch, ServerHandler, ClientPid, Request, DstList, Parameters, Timeout) ->
    State=#state{branch=Branch, serverhandler=ServerHandler, clienthandler=ClientPid,
		 request=Request, dstlist=DstList, parameters=Parameters,
		 timeout=Timeout},
    loop(State).

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
    case local:sippipe_received_response(State#state.request, Response, State#state.dstlist) of
	{huntstop, SendStatus, SendReason} ->
	    %% Don't continue searching, respond something
	    transactionlayer:send_response_handler(State#state.serverhandler, SendStatus, SendReason),
	    State;
	{continue, NewDstList} ->
	    %% Continue, possibly with an altered DstList
	    State#state{dstlist=NewDstList};
	_ ->
	    %% Use sippipe defaults
	    process_client_transaction_response2(Response, State)
    end.

process_client_transaction_response2(Response, State) when record(State, state) ->
    {Status, Reason, _, _} = Response,
    case Status of
	503 ->
	    DstListIn = State#state.dstlist,
	    case DstListIn of
		[FirstDst] ->
		    logger:log(debug, "sippipe: Received ~p ~s, but there are no more destinations " ++
			       "to try for this target - telling server transaction to answer 500 No reachable destination",
			       [Status, Reason]),
		    %% RFC3261 #16.7 bullet 6 says we SHOULD generate a 500 if a 503 is the best we've got
		    transactionlayer:send_response_handler(State#state.serverhandler, 500, "No reachable destination"),
		    State;
		[FailedDst | DstList] ->
		    NewBranch = get_next_target_branch(State#state.branch),
		    logger:log(debug, "sippipe: Received '~p ~s', starting new branch ~p for next destination",
			       [Status, Reason, NewBranch]),
		    Request = State#state.request,
		    Timeout = State#state.timeout,
		    NewDstList = resolve_if_necessary(DstList),
		    [FirstDst | _] = NewDstList,
		    %% XXX ideally we should not try to contact the same host over UDP, when
		    %% we receive a transport layer error for TCP, if there were any other
		    %% equally preferred hosts in the SRV response for a destination.
		    %% It makes more sense to try to connect to Proxy B over TCP/UDP than to
		    %% try Proxy A over UDP when Proxy A over TCP has just failed.
		    {Method, _, Header, Body} = Request,
		    {_, DstURI, _} = siprequest:rewrite_route(Header, FirstDst#sipdst.uri),
		    SendRequest = {Method, DstURI, Header, Body},
		    NewState = case transactionlayer:start_client_transaction(SendRequest, none, FirstDst, NewBranch, Timeout, self()) of
				   BranchPid when pid(BranchPid) ->
				       State#state{clienthandler=BranchPid, branch=NewBranch, dstlist=NewDstList};
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

adopt_st_and_get_branch(TH) ->
    case transactionlayer:adopt_server_transaction_handler(TH) of
        {error, E} ->    
            logger:log(error, "sippipe: Could not adopt server transaction handler ~p : ~p", [TH, E]),
	    error;
	_ ->
	    Branch = get_branch_from_handler(TH) ++ "-UAC",
	    Branch
    end.

%% Query a transaction handler for it's branch, and remove the -UAS suffix
%% in order to get the 'branch base'
get_branch_from_handler(TH) ->
    CallBranch = transactionlayer:get_branch_from_handler(TH),
    case string:rstr(CallBranch, "-UAS") of
	0 ->
	    CallBranch;
	Index when integer(Index) ->
	    BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
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

%% Take a list of transport layer destinations as argument, and if the first one
%% does not have all elements filled out then resolve as necessary and return a
%% new list.
resolve_if_necessary([Dst | T]) when record(Dst, sipdst), Dst#sipdst.proto == undefined; Dst#sipdst.addr == undefined; Dst#sipdst.port == undefined ->
    URI = Dst#sipdst.uri,
    %% This is an incomplete sipdst, it should have it's URI set so we resolve the rest from here
    case URI of
	{_, _, _, _, _} ->
	    case siprequest:url_to_dstlist(URI, 500, URI) of    % XXX do better size estimation
		DstList when list(DstList) ->
		    DstList;
		Unknown ->
		    logger:log(error, "sippipe: Failed resolving URI ~s : ~p", [sipurl:print(Dst#sipdst.uri), Unknown]),
		    resolve_if_necessary(T)
	    end;
	InvalidURI ->
	    logger:log(error, "sippipe: Skipping destination with invalid URI : ~p", [InvalidURI]),
	    resolve_if_necessary(T)
    end;
resolve_if_necessary([Dst | T]) when record(Dst, sipdst) ->
    [Dst | T];
resolve_if_necessary([Dst | T]) ->
    logger:log(error, "sippipe: Skipping invalid destination : ~p", [Dst]),
    resolve_if_necessary(T);
resolve_if_necessary(Unknown) ->
    logger:log(error, "sippipe: Unrecognized destination input data : ~p", [Unknown]),
    [].

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
