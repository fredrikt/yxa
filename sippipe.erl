%%% File    : sippipe.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : Try a given set of destinations sequentially
%%%               until we get a final response from one or them,
%%%               or have no destinations left to try.
%%%
%%% Created :  20 Feb 2004 by Fredrik Thulin <ft@it.su.se>

-module(sippipe).
-export([start/5]).

-record(state, {branch, serverhandler, clienthandler, request, dstlist, timeout, starttime, endtime, warntime, approxmsgsize}).

-include("sipsocket.hrl").
-include("siprecords.hrl").

%% Function: start/1
%% Description: Try to be flexible. If we are provided with a URI
%%              as DstList, we resolve it into a list of sipdst
%%              records. If we are given a client transaction handle,
%%              we start out with that one. Otherwise we start a
%%              client transaction and then enter loop/1.
%% Returns: Does not matter
%%--------------------------------------------------------------------

start(ServerHandler, ClientPid, Request, URI, Timeout) when record(Request, request) ->
    case catch guarded_start(ServerHandler, ClientPid, Request, URI, Timeout) of
	{'EXIT', Reason} ->
	    logger:log(error, "=ERROR REPORT==== from sippipe :~n~p", [Reason]),
	    transactionlayer:send_response_handler(ServerHandler, 500, "Server Internal Error");
	Res ->
	    Res
    end.

%%
%% Dst is an URI
%%
guarded_start(ServerHandler, ClientPid, Request, URI, Timeout) when record(URI, sipurl) ->
    %% URI input
    Dst = #sipdst{uri=URI},
    logger:log(debug, "sippipe: Made sipdst record out of URI input : ~s", [sipurl:print(URI)]),
    guarded_start(ServerHandler, ClientPid, Request, [Dst], Timeout);

%%
%% Dst == route
%%
guarded_start(ServerHandler, ClientPid, Request, route, Timeout) ->
    %% Request should have a Route header
    %% URI passed to process_route_header is foo here
    case siprequest:process_route_header(Request#request.header, Request#request.uri) of
	nomatch ->
	    logger:log(error, "sippipe: No destination given, and request has no Route header"),
	    erlang:fault("no destination and no route", [ServerHandler, ClientPid, Request, route, Timeout]);
	{ok, NewHeader, DstURI, ReqURI} when record(DstURI, sipurl), record(ReqURI, sipurl) ->
	    {ok, _, ApproxMsgSize} = siprequest:check_proxy_request(Request),
	    logger:log(debug, "sippipe: Routing request as per the Route header, Destination ~p, Request-URI ~p",
		      [sipurl:print(DstURI), sipurl:print(ReqURI)]),
	    case sipdst:url_to_dstlist(DstURI, ApproxMsgSize, ReqURI) of
		{error, nxdomain} ->
		    logger:log(debug, "sippipe: Failed resolving URI ~s : NXDOMAIN (responding '604 Does Not Exist Anywhere')",
			       [sipurl:print(DstURI)]),
		    transactionlayer:send_response_handler(ServerHandler, 604, "Does Not Exist Anywhere"),
		    error;
		{error, What} ->
		    logger:log(normal, "sippipe: Failed resolving URI ~s : ~p", [sipurl:print(DstURI), What]),
		    transactionlayer:send_response_handler(ServerHandler, 500, "Failed resolving Route destination"),
		    error;
                DstList when list(DstList) ->
                    guarded_start(ServerHandler, ClientPid, Request, DstList, Timeout);
                Unknown ->
                    logger:log(error, "sippipe: Failed resolving URI ~s : ~p", [sipurl:print(DstURI), Unknown]),
		    transactionlayer:send_response_handler(ServerHandler, 500, "Failed resolving Route destination"),
		    error
            end;
	Unknown ->
	    logger:log(error, "sippipe: Unkown result from sipdst:url_to_dstlist : ~p",
		       [Unknown]),
	    transactionlayer:send_response_handler(ServerHandler, 500, "Server Internal Error"),
	    error
    end;

%% First, we want to get the branch base from an existing ServerHandler
guarded_start(ServerHandler, ClientPid, Request, DstList, Timeout) when record(Request, request), list(DstList) ->
    case transactionlayer:is_good_transaction(ServerHandler) of
	true ->
	    %% Adopt server transaction and then continue
	    case adopt_st_and_get_branch(ServerHandler) of
		error ->
		    transactionlayer:send_response_handler(ServerHandler, 500, "Server Internal Error"),
		    error;
		Branch ->
		    guarded_start2(Branch, ServerHandler, ClientPid, Request, DstList, Timeout)
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
			    guarded_start2(Branch, STHandler, ClientPid, Request, DstList, Timeout)
		    end
	    end
    end.

%%
%% ClientPid /= none
%%
guarded_start2(Branch, ServerHandler, ClientPid, Request, DstListIn, Timeout) when record(Request, request), ClientPid /= none ->
    %% A client pid was supplied to us, go to work
    {ok, _, ApproxMsgSize} = siprequest:check_proxy_request(Request),
    final_start(Branch, ServerHandler, ClientPid, Request, DstListIn, Timeout, ApproxMsgSize);

%%
%% ClientPid == none
%%
guarded_start2(Branch, ServerHandler, _, Request, [DstIn | DstInT], Timeout) when record(Request, request), record(DstIn, sipdst) ->
    %% No client transaction handler is specified, start a client transaction on the first
    %% element from the destination list, after making sure it is complete.
    DstListIn = [DstIn | DstInT],
    {ok, _, ApproxMsgSize} = siprequest:check_proxy_request(Request),
    case resolve_if_necessary(DstListIn, ApproxMsgSize) of
	[] ->
	    logger:log(error, "sippipe: Failed starting sippipe, no valid destination(s) found"),
	    transactionlayer:send_response_handler(ServerHandler, 500, "Failed resolving destination"),
	    error;
	[FirstDst | DstT] ->
	    DstList = [FirstDst | DstT],
	    case transactionlayer:start_client_transaction(Request, none, FirstDst, Branch, Timeout, self()) of
		BranchPid when pid(BranchPid) ->
		    final_start(Branch, ServerHandler, BranchPid, Request, DstList, Timeout, ApproxMsgSize);
		{error, E} ->
		    logger:log(error, "sippipe: Failed starting client transaction : ~p", [E]),
		    transactionlayer:send_response_handler(ServerHandler, 500, "Failed resolving destination"),
		    error
	    end
    end.

%% Do piping between a now existing server and client transaction handler.
final_start(Branch, ServerHandler, ClientPid, Request, DstList, Timeout, ApproxMsgSize) when record(Request, request) ->
    StartTime = util:timestamp(),
    State=#state{branch=Branch, serverhandler=ServerHandler, clienthandler=ClientPid,
		 request=Request, dstlist=DstList, approxmsgsize=ApproxMsgSize,
		 timeout=Timeout, endtime = StartTime + Timeout, warntime = StartTime + 300},
    logger:log(debug, "sippipe: All preparations finished, entering pipe loop"),
    loop(State).


%% Function: loop/1
%% Description: Main loop.
%% Returns: Does not matter - does not return until we are finished.
%%--------------------------------------------------------------------
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
			  3 * 1000 ->
			      tick(State, util:timestamp())
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

%% Function: tick/1
%% Description: Check to see if it is time to warn, or perhaps time
%%              to die.
%% Returns: {quit, State} |
%%          {Res, State}
%%--------------------------------------------------------------------

%%
%% Time to quit, send a final response to server transaction and
%% cancel any running client transaction.
%%
tick(State, Now) when record(State, state), Now >= State#state.endtime ->
    case client_transaction_alive(State) of
	true ->
	    gen_server:cast(State#state.clienthandler, {cancel, "sippipe decided to quit (timeout)"});
	_ -> true
    end,
    case transactionlayer:is_good_transaction(State#state.serverhandler) of
	true ->
	    transactionlayer:send_response_handler(State#state.serverhandler, 500, "No reachable destination");
	_ -> ok
    end,
    logger:log(error, "sippipe: Reached end time after ~p seconds", [State#state.timeout]),
    {quit, State#state{clienthandler=none, serverhandler=none, branch=none, dstlist=[]}};

%%
%% Time to warn
%%
tick(State, Now) when record(State, state), Now >= State#state.warntime ->
    logger:log(error, "sippipe: Warning: pipe process still alive!~nClient handler : ~p, Server handler : ~p",
	       [State#state.clienthandler, State#state.serverhandler]),
    {ok, State#state{warntime=Now + 60}};

tick(State, Now) ->
    {tick, State}.


%% Function: process_client_transaction_response/2
%% Description: We have received a response. Check if we should do
%%              something.
%% Returns: New State
%%--------------------------------------------------------------------
process_client_transaction_response(Response, State) when record(Response, response), record(State, state), Response#response.status >= 200 ->
    logger:log(debug, "sippipe: Received final response '~p ~s'", [Response#response.status, Response#response.reason]),
    %% This is a final response. See if local:sippipe_received_response() has an oppinion on what
    %% we should do next.
    case local:sippipe_received_response(State#state.request, Response, State#state.dstlist) of
	{huntstop, SendStatus, SendReason} ->
	    %% Don't continue searching, respond something
	    transactionlayer:send_response_handler(State#state.serverhandler, SendStatus, SendReason),
	    State#state{clienthandler=none, branch=none, dstlist=[]};
	{next, NewDstList} ->
	    %% Continue, possibly with an altered DstList
	    start_next_client_transaction(State#state{dstlist=NewDstList});
	_ ->
	    %% Use sippipe defaults
	    process_client_transaction_response2(Response, State)
    end;
process_client_transaction_response(Response, State) when record(Response, response), record(State, state) ->
    logger:log(debug, "sippipe: Piping non-final response '~p ~s' to server transaction", [Response#response.status, Response#response.reason]),
    transactionlayer:send_proxy_response_handler(State#state.serverhandler, Response),
    State.

process_client_transaction_response2(Response, State) when record(Response, response), record(State, state) ->
    case Response#response.status of
	503 ->
	    start_next_client_transaction(State);
	_ ->
	    logger:log(debug, "sippipe: Piping final response '~p ~s' to server transaction ~p",
		       [Response#response.status, Response#response.reason, State#state.serverhandler]),
	    transactionlayer:send_proxy_response_handler(State#state.serverhandler, Response),
	    State
    end.

%% Function: start_next_client_transaction/1
%% Description: When this function is called, any previous client
%%              transactions will have received a final response.
%%              Start the next client transaction from
%%              State#state.dstlist, or send a 500 response in case
%%              we have no destinations left.
%% Returns: New State
%%--------------------------------------------------------------------
start_next_client_transaction(State) when record(State, state) ->
    DstListIn = State#state.dstlist,
    case DstListIn of
	[FirstDst] ->
	    logger:log(debug, "sippipe: There are no more destinations to try for this target - " ++
		       "telling server transaction to answer 500 No reachable destination"),
	    %% RFC3261 #16.7 bullet 6 says we SHOULD generate a 500 if a 503 is the best we've got
	    transactionlayer:send_response_handler(State#state.serverhandler, 500, "No reachable destination"),
	    State#state{clienthandler=none, branch=none, dstlist=[]};
	[FailedDst | DstList] ->
	    NewBranch = get_next_target_branch(State#state.branch),
	    logger:log(debug, "sippipe: Starting new branch ~p for next destination",
		       [NewBranch]),
	    Request = State#state.request,
	    Timeout = State#state.timeout,
	    ApproxMsgSize = State#state.approxmsgsize,
	    NewDstList = resolve_if_necessary(DstList, ApproxMsgSize),
	    [FirstDst | _] = NewDstList,
	    %% XXX ideally we should not try to contact the same host over UDP, when
	    %% we receive a transport layer error for TCP, if there were any other
	    %% equally preferred hosts in the SRV response for a destination.
	    %% It makes more sense to try to connect to Proxy B over TCP/UDP than to
	    %% try Proxy A over UDP when Proxy A over TCP has just failed.
	    NewState = case transactionlayer:start_client_transaction(Request, none, FirstDst, NewBranch, Timeout, self()) of
			   BranchPid when pid(BranchPid) ->
			       State#state{clienthandler=BranchPid, branch=NewBranch, dstlist=NewDstList};
			   {error, E} ->
			       logger:log(error, "sippipe: Failed starting client transaction : ~p", [E]),
			       transactionlayer:send_response_handler(State#state.serverhandler, 500, "Server Internal Error"),
			       State
		       end,
	    NewState
    end.

%% Function: adopt_st_and_get_branch/1
%% Description: Adopt a server transaction, and get it's branch.
%% Returns: error |
%%          Branch
%%--------------------------------------------------------------------
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

%% Function: get_next_target_branch/1
%% Description: Given the current branch as input, return the next one
%%              to use.
%% Returns: NewBranch
%%--------------------------------------------------------------------
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

%% Function: resolve_if_necessary/2
%% Description: Look at the first element of the input DstList. If it
%%              is a URI instead of a sipdst record, then resolve the
%%              URI into sipdst record(s) and prepend the new
%%              record(s) to the input DstList and return the new list
%% Returns: NewDstList
%%--------------------------------------------------------------------
resolve_if_necessary([], _) ->
    [];
resolve_if_necessary([Dst | T], ApproxMsgSize) when record(Dst, sipdst), Dst#sipdst.proto == undefined; Dst#sipdst.addr == undefined; Dst#sipdst.port == undefined ->
    URI = Dst#sipdst.uri,
    %% This is an incomplete sipdst, it should have it's URI set so we resolve the rest from here
    case URI of
	_ when record(URI, sipurl) ->
	    case sipdst:url_to_dstlist(URI, ApproxMsgSize, URI) of
		DstList when list(DstList) ->
		    DstList;
		Unknown ->
		    logger:log(error, "sippipe: Failed resolving URI ~s : ~p", [sipurl:print(URI), Unknown]),
		    resolve_if_necessary(T, ApproxMsgSize)
	    end;
	InvalidURI ->
	    logger:log(error, "sippipe: Skipping destination with invalid URI : ~p", [InvalidURI]),
	    resolve_if_necessary(T, ApproxMsgSize)
    end;
resolve_if_necessary([Dst | T], _) when record(Dst, sipdst) ->
    [Dst | T];
resolve_if_necessary([Dst | T], AMS) ->
    logger:log(error, "sippipe: Skipping invalid destination : ~p", [Dst]),
    resolve_if_necessary(T, AMS);
resolve_if_necessary(Unknown, _) ->
    logger:log(error, "sippipe: Unrecognized destination input data : ~p", [Unknown]),
    [].

%% Function: all_terminated/1
%% Description: Check if both server transaction and client
%%              transaction are dead. If so, return 'true'.
%% Returns: true  |
%%          false
%%--------------------------------------------------------------------
all_terminated(State) when record(State, state) ->
    ServerAlive = transactionlayer:is_good_transaction(State#state.serverhandler),
    ClientAlive = client_transaction_alive(State),
    if
	ServerAlive == true -> false;
	ClientAlive == true -> false;
	true -> true
    end.

%% Function: client_transaction_alive/1
%% Description: Check if the client transaction is dead.
%% Returns: true  |
%%          false
%%--------------------------------------------------------------------
client_transaction_alive(State) when record(State, state) ->
    case util:safe_is_process_alive(State#state.clienthandler) of
	{true, _} -> true;
	_ -> false
    end.
