%%%-------------------------------------------------------------------
%%% File    : appserver_glue.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Appserver glue between sipproxy and server transaction.
%%% Created : 25 Oct 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(appserver_glue).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/2
	 ]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipproxy.hrl").
-include("siprecords.hrl").
%%-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  branchbase,		%% string(), base of branches we should use
	  callhandler,		%% term(), the server transaction handler
	  request,		%% request record()
	  sipmethod,		%% string(), the method of the request, only use for matching!
	  actions,		%% list() of sipproxy_action record()
	  forkpid,		%% pid() of sipproxy process
	  callhandler_pid,	%% pid() of server transaction handler, only use for matching!
	  cancelled=false,	%% atom(), true | false
	  completed=false	%% atom(), true | false
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
%% Timeout before complaining about still being alive.
-define(TIMEOUT, 300 * 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link(BranchBase, CallHandler, Request, Actions)
%%           Request     = request record()
%%           Actions     = list() of sipproxy_action record()
%% Descrip.: start this appserver_glue gen_server.
%% Returns : gen_server:start_link/4
%%--------------------------------------------------------------------
start(Request, Actions) ->
    gen_server:start(?MODULE, [Request, Actions], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, Actions])
%%           Request     = request record()
%%           Actions     = list() of sipproxy_action record()
%% Descrip.: Initiates the server
%% Returns : {ok, State, Timeout} |
%%           error (parent will throw a siperror (500))
%%--------------------------------------------------------------------
init([Request, Actions]) ->
    case adopt_and_get_branchbase(Request) of
	{ok, CallHandler, BranchBase} ->
	    Method = {Request#request.method},
	    ForkPid = sipserver:safe_spawn(appserver, start_actions, [BranchBase, self(), Request, Actions]),
	    logger:log(normal, "~s: Appserver glue: Forking request, ~p actions",
		       [BranchBase, length(Actions)]),
	    logger:log(debug, "Appserver glue: Forking request, glue process ~p, CallHandler ~p, ForkPid ~p",
		       [self(), CallHandler, ForkPid]),
	    %% We need the pid of the callhandler extracted to do guard matches on it
	    CHPid = transactionlayer:get_pid_from_handler(CallHandler),
	    logger:log(debug, "Appserver glue started"),
	    {ok, #state{branchbase=BranchBase, callhandler=CallHandler, forkpid=ForkPid,
			request=Request, actions=Actions, callhandler_pid=CHPid, sipmethod=Method},
	     ?TIMEOUT};
	Res ->
	    Res
    end.

%%--------------------------------------------------------------------
%% Function: adopt_and_get_branchbase(Request)
%%           Request = request record()
%% Descrip.: As part of initialization, we adopt the server
%%           transaction, and fetch it's generated branch. We then
%%           remove the "-UAS" suffix from the branch so that we can
%%           generate easily correlatable client branches.
%% Returns : {ok, CallHandler, BranchBase} |
%%           error |
%%           ok
%%--------------------------------------------------------------------
adopt_and_get_branchbase(Request) when is_record(Request, request) ->
    URI = Request#request.uri,
    case transactionlayer:adopt_server_transaction(Request) of
	{error, cancelled} ->
	    logger:log(normal, "Appserver: Request '~s ~s' has allready been cancelled, aborting.",
		       [Request#request.method, sipurl:print(URI)]),
	    ok;
	{error, E} ->
	    logger:log(error, "Appserver: Failed to adopt server transaction for request ~s ~s : ~p",
		       [Request#request.method, sipurl:print(URI), E]),
	    error;
	CallHandler ->
	    CallBranch = transactionlayer:get_branch_from_handler(CallHandler),
	    case string:rstr(CallBranch, "-UAS") of
		0 ->
		    logger:log(error, "Appserver: Could not make branch base from branch ~p", [CallBranch]),
		    error;
		Index when integer(Index) ->
		    BranchBase = string:substr(CallBranch, 1, Index - 1),
		    {ok, CallHandler, BranchBase}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages.
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(Msg, From, State) ->
    logger:log(error, "Appserver glue: Received unknown gen_server call from ~p :~n~p",
	       [From, Msg]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    logger:log(error, "Appserver glue: Received unknown gen_server cast :~n~p", [Msg]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info({servertransaction_cancelled, FromPid}, ...
%%           FromPid = pid() (callhandler_pid)
%% Descrip.: Our server transaction (CallHandlerPid) signals that it
%%           has been cancelled. Send {cancel_pending} to our
%%           sipproxy process (ForkPid).
%% Returns : {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({servertransaction_cancelled, FromPid}, #state{callhandler_pid=FromPid}=State) when is_pid(FromPid) ->
    ForkPid = State#state.forkpid,
    logger:log(debug, "Appserver glue: Original request has been cancelled, sending " ++
	       "'cancel_pending' to ForkPid ~p and entering state 'cancelled'",
	       [ForkPid]),
    util:safe_signal("Appserver glue: ", ForkPid, {cancel_pending}),
    NewState1 = State#state{cancelled=true},
    check_quit({noreply, NewState1, ?TIMEOUT}, none);

%%--------------------------------------------------------------------
%% Function: handle_info({servertransaction_terminated, FromPid}, ...
%%           FromPid = pid() (callhandler_pid)
%% Descrip.: Our server transaction (CallHandlerPid) signals that it
%%           has terminated.
%% Returns : {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({servertransaction_terminating, FromPid}, #state{callhandler_pid=FromPid}=State) when is_pid(FromPid) ->
    {CallHandlerPid, ForkPid} = {State#state.callhandler_pid, State#state.forkpid},
    logger:log(debug, "Appserver glue: received servertransaction_terminating from my CallHandlerPid ~p "
	       "(ForkPid is ~p) - setting CallHandler to 'none'", [CallHandlerPid, ForkPid]),
    NewState1 = State#state{callhandler=none},
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_info({sipproxy_response, FromPid, Branch,
%%                       Response}, State)
%%           FromPid  = pid() (forkpid)
%%           Branch   = string()
%%           Response = response record()
%% Descrip.: Our sipproxy has received a response that it was supposed
%%           to forward to the TU (transaction user, us). Check it out
%%           to see what we should do with it (forward it to the
%%           server transaction, or just ignore it - depends on our
%%           sipstate).
%% Returns : {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({sipproxy_response, FromPid, _Branch, Response}, #state{forkpid=FromPid}=State)
  when is_record(Response, response), is_pid(FromPid) ->
    NewState1 =
	case State#state.callhandler of
	    none ->
		{Status, Reason} = {Response#response.status, Response#response.reason},
		logger:log(error, "Appserver glue: Received a response (~p ~s) that I can't forward "
			   "since CallHandler is 'none'", [Status, Reason]),
		State;
	    _ ->
		handle_sipproxy_response(Response, State)
	end,
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_info({sipproxy_all_terminated, FromPid,
%%                        FinalResponse}, State)
%%           FromPid  = pid() (forkpid)
%%           FinalResponse = response record() | {Status, Reason}
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%% Descrip.: Our sipproxy says that now, all it's client transactions
%%           are finished. It hands us either a final response
%%           as a record(), which we might or might not have seen
%%           before (so we check if we should forward it to the server
%%           transaction), or an (error) response as a tuple that it
%%           wants us to tell the server transaction to generate.
%% Returns : {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({sipproxy_all_terminated, FromPid, FinalResponse}, #state{forkpid=FromPid}=State) when is_pid(FromPid) ->
    CallHandler = State#state.callhandler,
    NewState1 =
	case State#state.cancelled of
	    true ->
		logger:log(debug, "Appserver glue: received sipproxy_all_terminated - request was cancelled. "
			   "Ask CallHandler ~p to send 487 Request Terminated and entering state 'completed'",
			   [CallHandler]),
		transactionlayer:send_response_handler(CallHandler, 487, "Request Cancelled"),
		State#state{completed=true};
	    false ->
		case FinalResponse of
		    _ when is_record(FinalResponse, response) ->
			logger:log(debug, "Appserver glue: received sipproxy_all_terminated with a '~p ~s'"
				   "response (completed: ~p)", [FinalResponse#response.status,
								FinalResponse#response.reason,
								State#state.completed]),
			NewState2 = handle_sipproxy_response(FinalResponse, State),
			NewState2;
		    {Status, Reason} when is_integer(Status), is_list(Reason) ->
			logger:log(debug, "Appserver glue: received sipproxy_all_terminated - asking CallHandler "
				   "~p to answer ~p ~s", [CallHandler, Status, Reason]),
			safe_callhandler_send_response(CallHandler, Status, Reason),
			%% XXX check that this is really a final response?
			State#state{completed=true};
		    none ->
			logger:log(error, "Appserver glue: received sipproxy_all_terminated with no final answer"),
			%% XXX generate a 500 Server Internal Error?
			State
		end
	end,
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_info({sipproxy_no_more_actions, FromPid}, State)
%%           FromPid  = pid() (forkpid)
%% Descrip.: Our sipproxy says that it has no more actions to perform.
%%           Check if we have sent a final response by now, and if so
%%           just ignore this signal. If we haven't sent a final
%%           response yet we check if we have been cancelled, or if
%%           we should generate a 408 Request Timeout.
%% Returns : {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({sipproxy_no_more_actions, FromPid}, #state{forkpid=FromPid}=State) when is_pid(FromPid) ->
    CallHandler = State#state.callhandler,
    NewState1 = 
	case State#state.cancelled of
	    true ->
		logger:log(debug, "Appserver glue: received sipproxy_no_more_actions when cancelled. " ++
			   "Ask CallHandler ~p to send 487 Request Terminated", [CallHandler]),
		transactionlayer:send_response_handler(CallHandler, 487, "Request Terminated"),
		State#state{completed=true};
	    false ->
		case State#state.completed of
		    false ->
			Request = State#state.request,
			{Method, URI} = {Request#request.method, Request#request.uri},
			logger:log(debug, "Appserver glue: received sipproxy_no_more_actions when NOT cancelled "
				   "(and not completed), responding 408 Request Timeout to original request ~s ~s",
				   [Method, sipurl:print(URI)]),
			safe_callhandler_send_response(CallHandler, 408, "Request Timeout"),
			State#state{completed=true};
		    true ->
			logger:log(debug, "Appserver glue: received sipproxy_no_more_actions "
				   "when already completed - ignoring"),
			State
		end
	end,
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_info({sipproxy_terminating, FromPid}, State)
%%           FromPid  = pid() (forkpid)
%% Descrip.: Our sipproxy is terminating. If we haven't sent a final
%%           response yet, generate a 500 Server Internal Error.
%% Returns : {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({sipproxy_terminating, FromPid}, #state{forkpid=FromPid}=State) when is_pid(FromPid) ->
    CallHandler = State#state.callhandler,
    {CallHandlerPid, ForkPid} = {State#state.callhandler_pid, State#state.forkpid},
    logger:log(debug, "Appserver glue: received sipproxy_terminating from my ForkPid ~p (CallHandlerPid is ~p) " ++
	       "- setting ForkPid to 'none'", [ForkPid, CallHandlerPid]),
    NewState1 = State#state{forkpid=none},
    NewState2 = 
	case transactionlayer:is_good_transaction(CallHandler) of
	    true ->
		%% Server transaction is still alive, make it send 500 response
		%% if no final response has been sent already
		case NewState1#state.completed of
		    false ->
			logger:log(error, "Appserver glue: No answer to original request, "
				   "answer 500 Server Internal Error"),
			transactionlayer:send_response_handler(CallHandler, 500, "Server Internal Error"),
			NewState1#state{completed=true};
		    true ->
			NewState1
		end;
	    false ->
		NewState1
	end,
    check_quit({noreply, NewState2, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%%           FromPid  = pid() (forkpid)
%% Descrip.: For some reason, we are still alive. Check if we should
%%           just garbage collect our way out of here.
%% Returns : {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%% Notes   : XXX maybe we should log this as an error. This should
%%           always be an indication that _something_ is wrong - even
%%           if it really is something in the transaction layer or
%%           wherever.
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    {CallHandler, ForkPid} = {State#state.callhandler, State#state.forkpid},
    logger:log(debug, "Appserver glue: Still alive after 5 minutes! CallHandler '~p', ForkPid '~p'",
	       [CallHandler, ForkPid]),
    util:safe_signal("Appserver glue: ", ForkPid, {showtargets}),
    NewState1 =
	case garbage_collect_transaction("CallHandler", CallHandler) of
	    none ->
		%% Set both callhandler and callhandler_pid to 'none'
		State#state{callhandler=none, callhandler_pid=none};
	    _ ->
		State
	end,
    NewForkPid = garbage_collect_pid("ForkPid", ForkPid),
    NewState = NewState1#state{forkpid=NewForkPid},
    check_quit({noreply, NewState, ?TIMEOUT});


handle_info(Info, State) ->
    logger:log(error, "Appserver glue: Received unknown signal :~n~p", [Info]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    {Request, BranchBase} = {State#state.request, State#state.branchbase},
    {Method, URI} = {Request#request.method, Request#request.uri},
    case Reason of
	normal ->
	    logger:log(normal, "~s: Appserver glue: Finished with fork (~s ~s), exiting.",
		       [BranchBase, Method, sipurl:print(URI)]);
	_ ->
	    logger:log(error, "~s: Appserver glue: Terminating fork (~s ~s), exiting ABNORMALLY.",
		       [BranchBase, Method, sipurl:print(URI)]),
	    logger:log(debug, "Appserver glue: Abnormally exiting with reason :~n~p", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Purpose : Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: check_quit(Res)
%%           Res = gen_server callback reply
%% Descrip.: Wrapper function checking if both our callhandler and our
%%           forkpid are dead. If so, turn Res into a {stop, ...} but
%%           if Res was a {reply, ...} do the gen_server:reply() first
%% Returns : Res                   |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
check_quit(Res) ->
    check_quit(Res, none).

check_quit(Res, From) ->
    case Res of
	{_, _, State, _} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	{_, _, State} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	{_, State, _} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	{_, State} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	_ ->
	    Res
    end.

check_quit2(Res, From, #state{callhandler=none, forkpid=none}=State) ->
    logger:log(debug, "Appserver glue: Both CallHandler and ForkPid are 'none' - terminating"),
    NewReply = case Res of
		   {reply, Reply, _, _} when From /= none ->
		       gen_server:reply(From, Reply),
		       {stop, normal, State};
		   {reply, Reply, _} when From /= none ->
		       gen_server:reply(From, Reply),
		       {stop, normal, State};
		   {stop, _, _, _}  ->
		       Res;
		   {stop, _, _} ->
		       Res;
		   _ ->
		       {stop, normal, State}
	       end,
    NewReply;
check_quit2(Res, _From, State) when is_record(State, state) ->
    %% Not time to quit yet
    Res.

safe_callhandler_send_response(none, Status, Reason) ->
    logger:log(error, "Appserver glue: CallHandler is gone, can't ask it to send '~p ~s' response", [Status, Reason]),
    error;
safe_callhandler_send_response(TH, Status, Reason) ->
    %% XXX do catch here since we are supposed to be safe?
    transactionlayer:send_response_handler(TH, Status, Reason).

%%--------------------------------------------------------------------
%% Function: handle_sipproxy_response(Response, State)
%%           Response = response record()
%%           State    = state record()
%% Descrip.: Process a response we have received from our sipproxy
%%           process and figure out what to do with it. See if we
%%           should send it on to our parent and record if this was a
%%           final response (set State#state.completed=true if so).
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
%%
%% Method is INVITE and we are already completed
%%
handle_sipproxy_response(Response, #state{sipmethod="INVITE", completed=true}=State) when record(Response, response) ->
    Request = State#state.request,
    URI = Request#request.uri,
    {Status, Reason} = {Response#response.status, Response#response.reason},
    if
	Status >= 200, Status =< 299 ->
	    %% XXX change to debug level
	    logger:log(normal, "Appserver glue: Forwarding 'late' 2xx response, ~p ~s to INVITE ~s statelessly",
		       [Status, Reason, sipurl:print(URI)]),
	    transportlayer:send_proxy_response(none, Response);
	true ->
	    logger:log(error, "Appserver glue: NOT forwarding non-2xx response ~p ~s to INVITE ~s - " ++
		       "a final response has already been forwarded (sipproxy should not do this!)",
		       [Status, Reason, sipurl:print(URI)])
    end,
    State;
%%
%% Method is non-INVITE and we are already completed
%%
handle_sipproxy_response(Response, #state{completed=true}=State) when record(Response, response) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(error, "Appserver glue: NOT forwarding response ~p ~s to non-INVITE request ~s ~s - " ++
	       "a final response has already been forwarded (sipproxy should not do this!)",
	       [Status, Reason, Method, sipurl:print(URI)]),
    State;
%%
%% We are not yet completed
%%
handle_sipproxy_response(Response, State) when record(Response, response), record(State, state) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    {Status, Reason} = {Response#response.status, Response#response.reason},
    CallHandler = State#state.callhandler,
    logger:log(debug, "Appserver glue: Forwarding response '~p ~s' to '~s ~s' to CallHandler ~p",
	       [Status, Reason, Method, sipurl:print(URI), CallHandler]),
    case transactionlayer:is_good_transaction(CallHandler) of
	true ->
	    transactionlayer:send_proxy_response_handler(CallHandler, Response);
	false ->
	    logger:log(error, "Appserver glue: Can't forward response '~p ~s' to bad CallHandler '~p'",
		       [Status, Reason, CallHandler])
    end,
    if
	Status >= 200 ->
	    State#state{completed=true};
	true ->
	    State
    end.

%%--------------------------------------------------------------------
%% Function: garbage_collect_pid(Descr, Pid)
%%           Descr = string(), only used in logging
%%           Pid   = pid() | none
%% Descrip.: Check if a process is still alive, return 'none' if it
%%           isn't.
%% Returns : Pid | none
%%           Pid = pid(), same as input Pid
%%--------------------------------------------------------------------
garbage_collect_pid(_Descr, none) ->
    none;
garbage_collect_pid(Descr, Pid) when is_pid(Pid) ->
    case util:safe_is_process_alive(Pid) of
	{true, _}  ->
	    Pid;
	_ ->
	    logger:log(error, "Appserver glue: ~s ~p is not alive, garbage collected.",
		       [Descr, Pid]),
	    none
    end.

%%--------------------------------------------------------------------
%% Function: garbage_collect_transaction(Descr, TH)
%%           Descr = string(), only used in logging
%%           TH    = term() (thandler record())
%% Descrip.: Check if a transaction handle still refers to an alive
%%           transaction handler process.
%% Returns : Pid | none
%%           Pid = pid(), same as input Pid
%%--------------------------------------------------------------------
garbage_collect_transaction(Descr, TH) ->
    case transactionlayer:is_good_transaction(TH) of
	true ->
	    TH;
	_ ->
	    logger:log(error, "Appserver glue: ~s transaction is not alive, garbage collected.",
		       [Descr]),
	    none
    end.
