%%--------------------------------------------------------------------
%%% File    : sippipe.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Try a given set of destinations sequentially until we
%%%           get a final response from one or them, or have no
%%%           destinations left to try.
%%%
%%% Created :  20 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%--------------------------------------------------------------------
-module(sippipe).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/5
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {branch,
		serverhandler,
		clienthandler,
		request,
		dstlist,
		timeout,
		starttime,
		endtime,
		warntime,
		approxmsgsize
	       }).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start(ServerHandler, ClientPid, Request, Dst, Timeout)
%%           ServerHandler = term(), server transaction handler
%%           ClientPid     = pid() | none, pid of already existing
%%                           client transaction that we should connect
%%                           with the server transaction
%%           Request       = request record(), original request
%%           Dst           = list() of sipdst() record |
%%
%% Descrip.: Try to be flexible. If we are provided with a URI as
%%           DstList, we resolve it into a list of sipdst records.
%%           If we are given a client transaction handle, we start out
%%           with that one. Otherwise we start a client transaction
%%           and then enter loop/1.
%% Returns : Does not matter
%%--------------------------------------------------------------------
start(ServerHandler, ClientPid, Request, Dst, Timeout) when is_record(Request, request) ->
    case catch guarded_start(ServerHandler, ClientPid, Request, Dst, Timeout) of
	{'EXIT', Reason} ->
	    logger:log(error, "=ERROR REPORT==== from sippipe :~n~p", [Reason]),
	    transactionlayer:send_response_handler(ServerHandler, 500, "Server Internal Error"),
	    {error, Reason};
	Res ->
	    Res
    end.

%% First, we want to get the branch base from an existing ServerHandler
guarded_start(ServerHandler, ClientPid, RequestIn, DstIn, Timeout) when is_record(RequestIn, request) ->
    %% call check_proxy_request to get sanity of proxying this request verified (Max-Forwards checked etc)
    {ok, NewHeader1, ApproxMsgSize} = siprequest:check_proxy_request(RequestIn),
    Request1 = RequestIn#request{header=NewHeader1},
    %% now make sure we know some destinations of this request
    {ok, DstList, Request} = start_get_dstlist(ServerHandler, Request1, ApproxMsgSize, DstIn),
    case start_get_servertransaction(ServerHandler, Request) of
	{ok, STHandler, Branch} ->
	    guarded_start2(Branch, STHandler, ClientPid, Request, DstList, Timeout, ApproxMsgSize);
	ok ->
	    ok;
	error ->
	    error
    end.

%%
%% ClientPid /= none
%%
guarded_start2(Branch, ServerHandler, ClientPid, Request, DstListIn, Timeout, ApproxMsgSize)
  when is_record(Request, request), ClientPid /= none ->
    %% A client pid was supplied to us, go to work
    final_start(Branch, ServerHandler, ClientPid, Request, DstListIn, Timeout, ApproxMsgSize);

%%
%% ClientPid == none
%%
guarded_start2(Branch, ServerHandler, none, Request, Dst, Timeout, ApproxMsgSize) when is_record(Request, request) ->
    %% No client transaction handler is specified, start a client transaction on the first
    %% element from the destination list, after making sure it is complete.
    case resolve_if_necessary(Dst, ApproxMsgSize) of
	[] ->
	    logger:log(error, "sippipe: Failed starting sippipe, no valid destination(s) found"),
	    transactionlayer:send_response_handler(ServerHandler, 500, "Failed resolving destination"),
	    error;
	[FirstDst | _] = DstList when is_record(FirstDst, sipdst) ->
	    NewRequest = Request#request{uri=FirstDst#sipdst.uri},
	    case transactionlayer:start_client_transaction(NewRequest, none, FirstDst, Branch, Timeout, self()) of
		BranchPid when is_pid(BranchPid) ->
		    final_start(Branch, ServerHandler, BranchPid, Request, DstList, Timeout, ApproxMsgSize);
		{error, E} ->
		    logger:log(error, "sippipe: Failed starting client transaction : ~p", [E]),
		    transactionlayer:send_response_handler(ServerHandler, 500, "Failed resolving destination"),
		    error
	    end
    end.

%% Do piping between a now existing server and client transaction handler.
final_start(Branch, ServerHandler, ClientPid, Request, [Dst|_]=DstList, Timeout, ApproxMsgSize)
  when is_list(Branch), is_pid(ClientPid), is_record(Request, request), is_record(Dst, sipdst),
       is_integer(Timeout), is_integer(ApproxMsgSize) ->
    StartTime = util:timestamp(),
    State=#state{branch=Branch, serverhandler=ServerHandler, clienthandler=ClientPid,
		 request=Request, dstlist=DstList, approxmsgsize=ApproxMsgSize,
		 timeout=Timeout, endtime = StartTime + Timeout, warntime = StartTime + 300},
    logger:log(debug, "sippipe: All preparations finished, entering pipe loop (~p destinations in my list)",
	       [length(DstList)]),
    loop(State).


%%--------------------------------------------------------------------
%% Function: loop(State)
%% Descrip.: Main loop.
%% Returns : Does not matter - does not return until we are finished.
%%--------------------------------------------------------------------
loop(State) when is_record(State, state) ->

    ClientPid = State#state.clienthandler,
    ServerHandlerPid = transactionlayer:get_pid_from_handler(State#state.serverhandler),

    {Res, NewState} = receive

			  {servertransaction_cancelled, ServerHandlerPid} ->
			      gen_server:cast(State#state.clienthandler, {cancel, "server transaction cancelled"}),
			      {ok, State};

			  {branch_result, _Branch, _NewTransactionState, Response} ->
			      NewState1 = process_client_transaction_response(Response, State),
			      {ok, NewState1};

			  {servertransaction_terminating, ServerHandlerPid} ->
			      NewState1 = State#state{serverhandler=none},
			      {ok, NewState1};

			  {clienttransaction_terminating, {_Branch, ClientPid}} ->
			      NewState1 = State#state{clienthandler=none},
			      {ok, NewState1};

			  {clienttransaction_terminating, {_Branch, _}} ->
			      %% An (at this time) unknown client transaction signals us that it
			      %% has terminated. This is probably one of our previously started
			      %% client transactions that is now finishing - just ignore the signal.
			      {ok, State};

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

%%--------------------------------------------------------------------
%% Function: tick(State, Now)
%%           Now = integer(), current time
%% Descrip.: Check to see if it is time to warn, or perhaps time
%%           to die.
%% Returns : {quit, NewState} |
%%           {Res, NewState}
%%           NewState = state record()
%%           Res      = quit | ok | tick
%%--------------------------------------------------------------------

%%
%% Time to quit, send a final response to server transaction and
%% cancel any running client transaction.
%%
tick(State, Now) when is_record(State, state), Now >= State#state.endtime ->
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
tick(State, Now) when is_record(State, state), Now >= State#state.warntime ->
    logger:log(error, "sippipe: Warning: pipe process still alive!~nClient handler : ~p, Server handler : ~p",
	       [State#state.clienthandler, State#state.serverhandler]),
    {ok, State#state{warntime=Now + 60}};

tick(State, _Now) ->
    {tick, State}.


%%--------------------------------------------------------------------
%% Function: process_client_transaction_response(Response, State)
%%           Response = response record()
%% Descrip.: We have received a response. Check if we should do
%%           something.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_client_transaction_response(#response{status=Status}=Response, State)
  when is_record(Response, response), is_record(State, state), Status >= 200 ->
    logger:log(debug, "sippipe: Received final response '~p ~s'", [Status, Response#response.reason]),
    %% This is a final response. See if local:sippipe_received_response() has an oppinion on what
    %% we should do next.
    case local:sippipe_received_response(State#state.request, Response, State#state.dstlist) of
	{huntstop, SendStatus, SendReason} ->
	    %% Don't continue searching, respond something
	    transactionlayer:send_response_handler(State#state.serverhandler, SendStatus, SendReason),
	    %% XXX cancel client handler?
	    State#state{clienthandler=none, branch=none, dstlist=[]};
	{next, NewDstList} ->
	    %% Continue, possibly with an altered DstList
	    start_next_client_transaction(State#state{dstlist=NewDstList});
	undefined ->
	    %% Use sippipe defaults
	    process_client_transaction_response2(Response, State)
    end;
process_client_transaction_response(Response, State) when is_record(Response, response), is_record(State, state) ->
    logger:log(debug, "sippipe: Piping non-final response '~p ~s' to server transaction",
	       [Response#response.status, Response#response.reason]),
    transactionlayer:send_proxy_response_handler(State#state.serverhandler, Response),
    State.

process_client_transaction_response2(Response, State) when is_record(Response, response), is_record(State, state) ->
    case Response#response.status of
	503 ->
	    start_next_client_transaction(State);
	_ ->
	    logger:log(debug, "sippipe: Piping final response '~p ~s' to server transaction ~p",
		       [Response#response.status, Response#response.reason, State#state.serverhandler]),
	    transactionlayer:send_proxy_response_handler(State#state.serverhandler, Response),
	    State
    end.

%%--------------------------------------------------------------------
%% Function: start_next_client_transaction(State)
%% Descrip.: When this function is called, any previous client
%%           transactions will have received a final response. Start
%%           the next client transaction from State#state.dstlist, or
%%           send a 500 response in case we have no destinations left.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
start_next_client_transaction(State) when is_record(State, state) ->
    DstListIn = State#state.dstlist,
    case DstListIn of
	[_FirstDst] ->
	    logger:log(debug, "sippipe: There are no more destinations to try for this target - " ++
		       "telling server transaction to answer 500 No reachable destination"),
	    %% RFC3261 #16.7 bullet 6 says we SHOULD generate a 500 if a 503 is the best we've got
	    transactionlayer:send_response_handler(State#state.serverhandler, 500, "No reachable destination"),
	    State#state{clienthandler=none, branch=none, dstlist=[]};
	[_FailedDst | DstList] ->
	    NewBranch = get_next_target_branch(State#state.branch),
	    logger:log(debug, "sippipe: Starting new branch ~p for next destination",
		       [NewBranch]),
	    Request = State#state.request,
	    Timeout = State#state.timeout,
	    ApproxMsgSize = State#state.approxmsgsize,
	    NewDstList = resolve_if_necessary(DstList, ApproxMsgSize),
	    [FirstDst | _] = NewDstList,
	    NewRequest = Request#request{uri=FirstDst#sipdst.uri},
	    %% XXX ideally we should not try to contact the same host over UDP, when
	    %% we receive a transport layer error for TCP, if there were any other
	    %% equally preferred hosts in the SRV response for a destination.
	    %% It makes more sense to try to connect to Proxy B over TCP/UDP than to
	    %% try Proxy A over UDP when Proxy A over TCP has just failed.
	    NewState = case transactionlayer:start_client_transaction(NewRequest, none, FirstDst, NewBranch, Timeout, self()) of
			   BranchPid when is_pid(BranchPid) ->
			       State#state{clienthandler=BranchPid, branch=NewBranch, dstlist=NewDstList};
			   {error, E} ->
			       logger:log(error, "sippipe: Failed starting client transaction : ~p", [E]),
			       transactionlayer:send_response_handler(State#state.serverhandler, 500, "Server Internal Error"),
			       State
		       end,
	    NewState
    end.

%%--------------------------------------------------------------------
%% Function: adopt_st_and_get_branch(TH)
%%           TH = term(), server transaction handle
%% Descrip.: Adopt a server transaction, and get it's branch.
%% Returns : Branch    |
%%           error     |
%%           cancelled
%%           Branch = string()
%%--------------------------------------------------------------------
adopt_st_and_get_branch(TH) ->
    case transactionlayer:adopt_server_transaction_handler(TH) of
	{error, cancelled} ->
	    logger:log(normal, "sippipe: Request has allready been cancelled, aborting."),
	    cancelled;
        {error, E} ->
            logger:log(error, "sippipe: Could not adopt server transaction handler ~p : ~p", [TH, E]),
	    error;
	_ ->
	    Branch = get_branch_from_handler(TH) ++ "-UAC",
	    Branch
    end.

%% Part of adopt_st_and_get_branch(). Query a transaction handler for it's branch,
%% and remove the -UAS suffix in order to get the 'branch base'.
get_branch_from_handler(TH) ->
    CallBranch = transactionlayer:get_branch_from_handler(TH),
    case string:rstr(CallBranch, "-UAS") of
	0 ->
	    CallBranch;
	Index when is_integer(Index) ->
	    BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
    end.

%%--------------------------------------------------------------------
%% Function: get_next_target_branch(In)
%%           In = string()
%% Descrip.: Given the current branch as input, return the next one
%%           to use.
%% Returns: NewBranch = string()
%%--------------------------------------------------------------------
get_next_target_branch(In) ->
    case string:rchr(In, $.) of
	0 ->
	    In ++ ".1";
	Index when is_integer(Index) ->
	    Rhs = string:substr(In, Index + 1),
	    case util:isnumeric(Rhs) of
		true ->
		    Lhs = string:substr(In, 1, Index),
		    Lhs ++ integer_to_list(list_to_integer(Rhs) + 1);
	    	_ ->
	    	    In ++ ".1"
	    end
    end.

%%--------------------------------------------------------------------
%% Function: resolve_if_necessary(DstList, ApproxMsgSize)
%%           DstList       = list() of sipdst record()
%%           ApproxMsgSize = integer()
%% Descrip.: Look at the first element of the input DstList. If it is
%%           a URI instead of a sipdst record, then resolve the URI
%%           into sipdst record(s) and prepend the new record(s) to
%%           the input DstList and return the new list
%% Returns : NewDstList = list() of sipdst record()
%%--------------------------------------------------------------------
resolve_if_necessary([], _) ->
    [];
resolve_if_necessary([#sipdst{proto=Proto, addr=Addr, port=Port}=Dst | T], ApproxMsgSize)
  when Proto == undefined, Addr == undefined, Port == undefined ->
    URI = Dst#sipdst.uri,
    %% This is an incomplete sipdst, it should have it's URI set so we resolve the rest from here
    case URI of
	_ when is_record(URI, sipurl) ->
	    case sipdst:url_to_dstlist(URI, ApproxMsgSize, URI) of
		DstList when is_list(DstList) ->
		    DstList;
		Unknown ->
		    logger:log(error, "sippipe: Failed resolving URI ~s : ~p", [sipurl:print(URI), Unknown]),
		    resolve_if_necessary(T, ApproxMsgSize)
	    end;
	InvalidURI ->
	    logger:log(error, "sippipe: Skipping destination with invalid URI : ~p", [InvalidURI]),
	    resolve_if_necessary(T, ApproxMsgSize)
    end;
resolve_if_necessary([Dst | T], _) when is_record(Dst, sipdst) ->
    [Dst | T];
resolve_if_necessary([Dst | T], AMS) ->
    logger:log(error, "sippipe: Skipping invalid destination : ~p", [Dst]),
    resolve_if_necessary(T, AMS);
resolve_if_necessary(Unknown, _) ->
    logger:log(error, "sippipe: Unrecognized destination input data : ~p", [Unknown]),
    [].

%%--------------------------------------------------------------------
%% Function: all_terminated(State)
%% Descrip.: Check if both server transaction and client transaction
%%           are dead. If so, return 'true'.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
all_terminated(State) when is_record(State, state) ->
    ServerAlive = transactionlayer:is_good_transaction(State#state.serverhandler),
    ClientAlive = client_transaction_alive(State),
    if
	ServerAlive == true -> false;
	ClientAlive == true -> false;
	true -> true
    end.

%%--------------------------------------------------------------------
%% Function: client_transaction_alive(State)
%% Descrip.: Check if the client transaction is dead.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
client_transaction_alive(State) when is_record(State, state) ->
    case util:safe_is_process_alive(State#state.clienthandler) of
	{true, _} -> true;
	_ -> false
    end.

%%====================================================================
%% Startup functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: start_get_dstlist(ServerHandler, Request, ApproxMsgSize,
%%                             Dst)
%%           ServerHandler = term(), server transaction handle
%%           Request       = request record()
%%           ApproxMsgSize = integer(), approximate size of formatted
%%                                      request
%%           Dst           = sipurl record() | route |
%%                           list() of sipdst record()
%% Descrip.: Get an initial list of destinations for this request.
%%           This might mean we pop the destination from a Route
%%           header, in which case we return a new request.
%% Returns : {ok, DstList, NewRequest} |
%%           error
%%           DstList    = list of sipdst record()
%%           NewRequest = request record()
%%--------------------------------------------------------------------

%%
%% Dst is an URI
%%
start_get_dstlist(_ServerHandler, Request, _ApproxMsgSize, URI) when is_record(Request, request),
								     is_record(URI, sipurl) ->
    Dst = #sipdst{uri=URI},
    logger:log(debug, "sippipe: Made sipdst record out of URI input : ~s", [sipurl:print(URI)]),
    {ok, [Dst], Request};

%%
%% Dst == route
%%
start_get_dstlist(ServerHandler, Request, ApproxMsgSize, route) when is_record(Request, request),
								     is_integer(ApproxMsgSize) ->
    %% Request should have a Route header
    %% URI passed to process_route_header is foo here
    case siprequest:process_route_header(Request#request.header, Request#request.uri) of
	nomatch ->
	    logger:log(error, "sippipe: No destination given, and request has no Route header"),
	    erlang:fault("no destination and no route", [ServerHandler, Request, route]);
	{ok, NewHeader, DstURI, ReqURI} when is_record(DstURI, sipurl), is_record(ReqURI, sipurl) ->
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
                DstList when is_list(DstList) ->
                    {ok, DstList, Request#request{header=NewHeader}};
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

start_get_dstlist(_ServerHandler, Request, _ApproxMsgSize, [Dst | _] = DstList) when is_record(Request, request),
										     is_record(Dst, sipdst) ->
    {ok, DstList, Request}.

%%--------------------------------------------------------------------
%% Function: start_get_servertransaction(ServerHandler, Request)
%%           ServerHandler = none | term(), server transaction handle
%%           Request       = request record()
%% Descrip.: Assure we have a working server transaction handle.
%%           Either we get one as ServerHandler argument, or we try to
%%           find one using the transaction layer. If the server
%%           transaction has already been cancelled, we return 'ok'.
%% Returns : {ok, STHandler, Branch} |
%%           ok                      |
%%           error
%%           STHandler = term()
%%           Branch    = string()
%%--------------------------------------------------------------------
start_get_servertransaction(none, Request) when is_record(Request, request) ->
    %% No server transaction handler supplied. Try to find a valid handler using the request.
    case transactionlayer:get_handler_for_request(Request) of
	{error, E} ->
	    logger:log(error, "sippipe: Could not start pipe, no valid server transaction "
		       "supplied and none found using transaction layer, error : ~p", [E]),
	    error;
	STHandler ->
	    %% Ok, found the server transaction handler. Adopt it and query it for it's branch base.
	    case adopt_st_and_get_branch(STHandler) of
		error ->
		    transactionlayer:send_response_handler(STHandler, 500, "Server Internal Error"),
		    error;
		cancelled ->
		    ok;
		Branch ->
		    {ok, STHandler, Branch}
	    end
    end;
start_get_servertransaction(ServerHandler, Request) when is_record(Request, request) ->
    case transactionlayer:is_good_transaction(ServerHandler) of
	true ->
	    %% Adopt server transaction and then continue
	    case adopt_st_and_get_branch(ServerHandler) of
		error ->
		    transactionlayer:send_response_handler(ServerHandler, 500, "Server Internal Error"),
		    error;
		cancelled ->
		    ok;
		Branch ->
		    {ok, ServerHandler, Branch}
	    end;
	false ->
	    logger:log(error, "sippipe: Server transaction given to me (~p) is not alive", [ServerHandler]),
	    error
    end.
