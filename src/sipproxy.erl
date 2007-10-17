%%%-------------------------------------------------------------------
%%% File    : sipproxy.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Find a working destination for a request, given a number
%%%           of locations and actions. Through forking or sequential
%%%           'search'.
%%%
%%%           The doings of this module is described in RFC3261
%%%           #16.2 Stateful Proxy
%%%           #16.7 (Response Processing)
%%%
%%%           sipproxy is what is called a proxy core in the
%%%           introduction of #16.2.
%%%
%%%           The process invoking sipproxy:start_link() should be
%%%           prepared to receive the following signals :
%%%
%%%           {sipproxy_response, Pid, Branch, Response}
%%%           {sipproxy_all_terminated, Pid, FinalResponse}
%%%           {sipproxy_terminating, Pid}
%%%
%%%              Response = response record()
%%%              FinalResponse = Response | {Status, Reason} |
%%%                              {Status, Reason, ExtraHeaders}
%%%              Pid      = pid() of sipproxy process
%%%              Branch   = string(), the branch used by the client
%%%                         transaction that received/generated a
%%%                         response
%%%              Status   = integer(), SIP status code
%%%              Reason   = string(), SIP reason phrase
%%%
%%%           If you want to terminate a running sipproxy (for example
%%%           if the server transaction has been cancelled), send it
%%%           an {cancel_pending, ExtraHeaders} signal.
%%%
%%% @since    13 Feb 2003 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sipproxy).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_actions/5,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipproxy.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  parent,			%% pid(), parent process - used to trap EXITs
	  branchbase,			%% string(), a base for us to use when creating client transaction branches.
	  				%% Must be unique!
	  request,			%% request record(), the request we are working on
	  actions,			%% list() of sipproxy_action record() - our list of actions
	  surplus,			%% list() of sipproxy_action record() - suplus actions that match user+instance of
	 				%% one of the records stored in 'actions'
	  targets,			%% targetlist record(), list of all our targets (ongoing or finished).
					%% Targets are client transactions.
	  final_response_sent = false,	%% true | false, have we forwarded a final response yet?
	  mystate = calling,		%% calling | cancelled | completed | stayalive
	  approx_msgsize,		%% integer(), approximate size of requests we send out. Expensive to calculate,
					%% so we remember it.
	  timeout			%% integer(), number of seconds to wait for final response after we reach
	  				%% the end of our list of actions and cancel all our pending targets
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (BranchBase, Parent, OrigRequest, Actions, Surplus) ->
%%            ok | error
%%
%%            BranchBase  = string() "the \"base\" part of the server transactions branch - so that we can get sipproxy to generate intuitive branches for it's corresponding client transactions"
%%            Parent      = pid() "the pid to which sipproxy should report"
%%            OrigRequest = #request{}
%%            Actions     = [#sipproxy_action{}]
%%            Surplus     = [#sipproxy_action{}]
%%
%% @doc     Start the processing, currently forking (in parallell or
%%          sequentially) of Request according to Actions. This
%%          function is typically executed by a spawn in the
%%          appserver glue process.
%% @end
%%--------------------------------------------------------------------
start_actions(BranchBase, Parent, OrigRequest, Actions, Surplus) when is_record(OrigRequest, request) ->
    case start_check_actions(Actions) of
	ok ->
	    {Method, URI} = {OrigRequest#request.method, OrigRequest#request.uri},
	    Timeout = 32,	%% wait at the end of actions-list timeout
	    %% We don't return from sipproxy:start() until all Actions are done, and sipproxy signals Parent
	    %% when it is done.
	    case start(BranchBase, Parent, OrigRequest, Actions, Surplus, Timeout) of
		ok ->
		    logger:log(debug, "sipproxy: fork of request ~s ~s done, start_actions() returning", [Method, sipurl:print(URI)]),
		    ok;
		{error, What} ->
		    logger:log(error, "sipproxy: fork of request ~s ~s failed : ~p", [Method, sipurl:print(URI), What]),
		    error
	    end;
	{error, Reason} ->
	    logger:log(error, "sipproxy: Problems with actions : ~p", [Reason]),
	    logger:log(debug, "sipproxy: Actions : ~p", [Actions]),
	    error
    end.

%%--------------------------------------------------------------------
%% @spec
%%    (BranchBase, Parent, Request, Actions, Surplus, Timeout) ->
%%            ok              |
%%            {error, Reason}
%%
%%            BranchBase = string()
%%            Parent     = pid()
%%            Request    = #request{}
%%            Actions    = [#sipproxy_action{}]
%%            Surplus    = [#sipproxy_action{}]
%%            Timeout    = integer()
%%
%%            Reason = string()
%%
%% @doc     Start the processing, currently forking (in parallell or
%%          sequentially) of Request according to Actions. Note :
%%          Actions is a set of actions to perform. Currently
%%          supported actions are a list of 'call' and 'wait'. You
%%          can mix calls and waits freely.
%% @end
%%--------------------------------------------------------------------
start(BranchBase, Parent, Request, Actions, Surplus, Timeout)
  when is_list(BranchBase), is_pid(Parent), is_record(Request, request), is_list(Actions), is_list(Surplus),
       is_integer(Timeout) ->
    Res = case catch siprequest:check_proxy_request(Request) of
	      {ok, NewHeader, ApproxMsgSize} ->
		  %% sipproxy should never be invoked on a request that contains
		  %% a Route header, but we check just in case someone screws up.
		  case keylist:fetch('route', Request#request.header) of
		      [] ->
			  EndTime = util:timestamp() + Timeout,
			  State = #state{parent		= Parent,
					 branchbase	= BranchBase,
					 timeout	= Timeout,
					 request	= Request#request{header = NewHeader},
					 actions	= Actions,
					 surplus	= Surplus,
					 targets	= targetlist:empty(),
					 approx_msgsize	= ApproxMsgSize
					},
			  %% We need to trap exits so that we can handle client transcations failing, and
			  %% cancel all client transactions if the parent crashes.
			  TrapExit = process_flag(trap_exit, true),
			  fork(EndTime, State),
			  %% restore trapexit in case we are testing
			  true = process_flag(trap_exit, TrapExit),
			  ok;
		      _ ->
			  logger:log(error, "sipproxy: Can't fork request with Route header"),
			  InternalError = {500, "Server Internal Error"},
			  Parent ! {sipproxy_all_terminated, self(), InternalError},
			  {error, "Request with Route header could not be forked"}
		  end;
	      {siperror, Status, Reason} ->
		  %% siprequest:check_proxy_request() did a throw() - something it does if
		  %% for example Max-Forwards is 1. Pass this on to Parent and then return.
		  logger:log(debug, "sipproxy: caught siperror throw() from check_proxy_request : ~p ~s",
			     [Status, Reason]),
		  Parent ! {sipproxy_all_terminated, self(), {Status, Reason}},
		  E = io_lib:format("Request could not be forked (~p ~s)", [Status, Reason]),
		  {error, lists:flatten(E)};
	      {siperror, Status, Reason, ExtraHeaders} ->
		  %% siprequest:check_proxy_request() did a throw() - something it does if
		  %% for example Max-Forwards is 1. Pass this on to Parent and then return.
		  logger:log(debug, "sipproxy: caught siperror throw() from check_proxy_request : ~p ~s, "
			     "extra headers : ~p", [Status, Reason, ExtraHeaders]),
		  Parent ! {sipproxy_all_terminated, self(), {Status, Reason, ExtraHeaders}},
		  E = io_lib:format("Request could not be forked (~p ~s)", [Status, Reason]),
		  {error, lists:flatten(E)}
	  end,
    %% Always signal Parent that we are terminating
    Parent ! {sipproxy_terminating, self()},
    Res.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Actions) ->
%%            ok              |
%%            {error, Reason}
%%
%%            Reason = string()
%%
%% @doc     Check that the actions supplied to our start function
%%          makes sense.
%% @end
%%--------------------------------------------------------------------
start_check_actions(Actions) when is_list(Actions) ->
    case start_check_actions2(Actions, 0, 0) of
	{ok, 0, _Waits} ->
	    {error, "no call action(s)"};
	{ok, _Calls, 0} ->
	    {error, "no wait action(s)"};
	{ok, _Calls, _Waits} ->
	    ok
    end.

start_check_actions2([#sipproxy_action{action = call} | T], Calls, Waits) ->
    start_check_actions2(T, Calls + 1, Waits);
start_check_actions2([#sipproxy_action{action = wait} | T], Calls, Waits) ->
    start_check_actions2(T, Calls, Waits + 1);
start_check_actions2([], Calls, Waits) ->
    {ok, Calls, Waits}.

%%--------------------------------------------------------------------
%% @spec    (EndTime, State) -> ok
%%
%%            EndTime = integer() "absolute timestamp we should end"
%%            State   = #state{}
%%
%% @doc     Main loop. Process actions until there are none left.
%% @end
%%--------------------------------------------------------------------
%%
%% No actions left
%%
fork(_EndTime, State) when is_record(State, state), State#state.actions == [] ->
    OrigRequest = State#state.request,
    Targets = State#state.targets,
    {Method, ReqURI} = {OrigRequest#request.method, OrigRequest#request.uri},
    NewState =
	case allterminated(Targets) of
	    true ->
		logger:log(debug, "sipproxy: All targets are terminated, terminating"),
		State;
	    false ->
		%% Some targets are not 'terminated'. Even though we cancel them here, they just
		%% might result in a 2xx response to INVITE (it's a race condition - their response
		%% might be sent already), so we have to stay alive for a little while to forward
		%% these 2xx responses to our parent.
		logger:log(normal, "sipproxy: Reached end of Actions-list for '~s ~s', cancelling pending targets.",
			   [Method, sipurl:print(ReqURI)]),
		NewTargets = cancel_pending_targets(Targets, []),

		State#state.parent ! {sipproxy_no_more_actions, self()},

		logger:log(debug, "sipproxy: waiting ~p seconds for any final responses", [State#state.timeout]),
		NewEndTime = util:timestamp() + State#state.timeout,
		case process_wait(NewEndTime, State#state{mystate = stayalive,
							  targets = NewTargets}
				 ) of
		    {discontinue, NewState1_1} when is_record(NewState1_1, state) ->
			logger:log(debug, "sipproxy: final responses wait is over (discontinue), exiting."),
			NewState1_1;
		    {timeout, NewState1_1} when is_record(NewState1_1, state) ->
			logger:log(debug, "sipproxy: final responses wait is over (timeout), exiting."),
			case NewState1_1#state.final_response_sent of
			    true ->
				NewState1_1;
			    false ->
				TimeoutResponse = {408, "Request Timeout"},
				NewState1_1#state.parent ! {sipproxy_all_terminated, self(), TimeoutResponse},
				NewState1_1#state{final_response_sent = true}
			end
		    end
	end,

    %% Check that this set of actions actually resulted in a final response. The original list of Actions
    %% could have been empty, or contained only waits or something equally stupid (the stupidity lies in
    %% the hand of the end user with CPL scripts ;) ).
    case NewState#state.final_response_sent of
	false ->
	    logger:log(debug, "sipproxy: No final response sent to parent for this set of actions, "
		       "generating a '500 Server Internal Error'"),
	    InternalError = {500, "Server Internal Error"},
	    NewState#state.parent ! {sipproxy_all_terminated, self(), InternalError};
	_ -> ok
    end,
    ok;
%%
%% One or more actions left
%%
fork(EndTime, State) when is_integer(EndTime), is_record(State, state) ->
    %% Process first action in list
    OrigRequest = State#state.request,
    [HAction | TAction] = State#state.actions,
    Method = OrigRequest#request.method,
    Targets = State#state.targets,
    case HAction#sipproxy_action.action of
	call ->
	    #sipproxy_action{requri   = CallURI,
			     timeout  = CallTimeout,
			     user     = SIPUser,
			     instance = Instance
			    } = HAction,
	    logger:log(debug, "sipproxy: forking ~s request to ~s with timeout ~p",
		       [Method, sipurl:print(CallURI), CallTimeout]),
	    %% Create a new branch for this target (client transaction). BranchBase plus a sequence number is unique.
	    Branch = State#state.branchbase ++ "-UAC" ++ integer_to_list(targetlist:get_length(Targets) + 1),

	    {ok, Request, DstList} = make_new_target_request(OrigRequest, State#state.approx_msgsize, HAction),
	    case DstList of
		[FirstDst | _] = DstList ->
		    NewTargets =
			case local:start_client_transaction(Request, FirstDst, Branch, CallTimeout) of
			    BranchPid when is_pid(BranchPid) ->
				targetlist:add(Branch, Request, BranchPid, calling,
					       CallTimeout, DstList, {SIPUser, Instance}, Targets);
			    {error, E} ->
				logger:log(error, "sipproxy: Failed starting client transaction : ~p", [E]),
				Targets
			end,
		    fork(EndTime, State#state{actions = TAction,
					      targets = NewTargets
					     });
		[] ->
		    logger:log(normal, "~s: Target URI ~s did not resolve to any destinations we could use - ignoring",
			       [Branch, sipurl:print(CallURI)]),
		    fork(EndTime, State#state{actions = TAction});
		{error, Reason} ->
		    logger:log(normal, "~s: Failed resolving URI ~s - ignoring this target (error : ~p)",
			       [Branch, sipurl:print(CallURI), Reason]),
		    fork(EndTime, State#state{actions = TAction})
	    end;
	wait ->
	    Time = HAction#sipproxy_action.timeout,
	    logger:log(debug, "sipproxy: waiting ~p seconds", [Time]),
	    %% XXX we loose our original endtime that is starttime + timeout given to start() here,
	    %% we probably shouldn't. The meaning of the timeout to start() is not clearly specified.
	    NewEndTime = util:timestamp() + Time,
	    case process_wait(NewEndTime, State) of
		{discontinue, NewState} when is_record(NewState, state) ->
		    logger:log(debug, "sipproxy: discontinue processing"),
		    fork(NewEndTime, NewState#state{actions = []});
		{timeout, NewState} when is_record(NewState, state) ->
		    fork(NewEndTime, NewState#state{actions = TAction})
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, ApproxMsgSize, Action) ->
%%            {ok, NewRequest, NewCallURI}
%%
%%            Request       = #request{}
%%            ApproxMsgSize = integer()
%%            Action        = #sipproxy_action{}
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
make_new_target_request(Request, ApproxMsgSize, Action)
  when is_record(Request, request), is_integer(ApproxMsgSize), is_record(Action, sipproxy_action) ->
    #sipproxy_action{requri   = URI,
		     user     = User,
		     path     = Path
		    } = Action,

    %% Create a Route header if there was a RFC3327 path associated with the location db entry
    {NewHeader1, DstURI} =
	case Path of
	    [] ->
		{Request#request.header, URI};
	    [FirstPath | _] = Path ->
		NewHeader1_1 = keylist:prepend({"Route", Path}, Request#request.header),
		[FirstC] = contact:parse([FirstPath]),
		FirstURL = sipurl:parse(FirstC#contact.urlstr),
		{NewHeader1_1, FirstURL}
	end,

    %% Add X-YXA-Peer-Auth header if User is specified and we have configured entry for our peer
    {ok, PeerAuthL} = yxa_config:get_env(x_yxa_peer_auth, []),
    NewHeader =
	case is_list(User) of
	    true ->
		case lists:keysearch(URI#sipurl.host, 1, PeerAuthL) of
		    {value, {_Host, Secret}} ->
			sipauth:add_x_yxa_peer_auth(Request#request.method, URI, NewHeader1, User, Secret);
		    false ->
			NewHeader1
		end;
	    false ->
		NewHeader1
	end,

    DstList = sipdst:url_to_dstlist(DstURI, ApproxMsgSize, URI),

    NewRequest =
	Request#request{uri    = URI,
			header = NewHeader
		       },

    {ok, NewRequest, DstList}.

%%--------------------------------------------------------------------
%% @spec    (Targets, ExtraHeaders) ->
%%            NewTargets
%%
%%            Targets      = #targetlist{}
%%            ExtraHeaders = [{Key, ValueList}]
%%
%%            NewTargets = #targetlist{}
%%
%% @doc     Cancel all targets in state 'calling' or 'proceeding'.
%% @end
%%--------------------------------------------------------------------
cancel_pending_targets(Targets, ExtraHeaders) when is_list(ExtraHeaders) ->
    %% cancel all PIDs in states other than completed and terminated
    NewTargets1 = cancel_targets_state(Targets, calling, ExtraHeaders),
    cancel_targets_state(NewTargets1, proceeding, ExtraHeaders).

%%--------------------------------------------------------------------
%% @spec    (Targets, TargetState, ExtraHeaders) ->
%%            NewTargets
%%
%%            Targets      = #targetlist{}
%%            TargetState  = atom()
%%            ExtraHeaders = [{Key, ValueList}]
%%
%%            NewTargets = #targetlist{}
%%
%% @doc     Cancel all targets in state TargetState.
%% @end
%%--------------------------------------------------------------------
cancel_targets_state(Targets, TargetState, ExtraHeaders) when is_atom(TargetState), is_list(ExtraHeaders) ->
    %% cancel all PIDs in a specific state, return updated TargetList
    TargetsInState = targetlist:get_targets_in_state(TargetState, Targets),
    lists:map(fun(ThisTarget) ->
		      [Pid] = targetlist:extract([pid], ThisTarget),
		      transactionlayer:cancel_client_transaction(Pid, "cancel_targets_state", ExtraHeaders)
	      end, TargetsInState),
    NewTargets = mark_cancelled(TargetsInState, Targets),
    NewTargets.

%%--------------------------------------------------------------------
%% @spec    (Targets, TargetList) ->
%%            NewTargets
%%
%%            Targets    = [term()]
%%            TargetList = #targetlist{}
%%
%%            NewTargets = #targetlist{}
%%
%% @doc     Marks a number of targets (Targets) as cancelled.
%% @end
%%--------------------------------------------------------------------
mark_cancelled([], TargetList) ->
    TargetList;
mark_cancelled([H | T], TargetList) ->
    NewTarget = targetlist:set_cancelled(H, true),
    NewTargetList = targetlist:update_target(NewTarget, TargetList),
    mark_cancelled(T, NewTargetList).

%%--------------------------------------------------------------------
%% @spec    (EndTime, State) ->
%%            {discontinue, NewState} |
%%            {timeout, NewState}
%%
%%            EndTime = integer() "time when we should end this wait"
%%            State   = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Marks a number of targets (Targets) as cancelled.
%% @end
%%--------------------------------------------------------------------
process_wait(EndTime, State) when is_integer(EndTime), is_record(State, state) ->
    Time = lists:max([EndTime - util:timestamp(), 0]),
    {Res, NewState} =
	receive
	    Msg ->
		process_signal(Msg, State)
	after
	    Time * 1000 ->
		logger:log(debug, "sipproxy: This wait's time (~p seconds) is over", [Time]),
		{timeout, State}
	end,
    case Res of
	ok ->
	    AllTerminated = allterminated(NewState),
	    NewState_1 = report_upstreams(AllTerminated, NewState),
	    TimeLeft = lists:max([EndTime - util:timestamp(), 0]),
	    EndProcessing = end_processing(EndTime, NewState_1),
	    logger:log(debug, "sipproxy: Extra debug: State changer, MyState=~p (was: ~p), TimeLeft=~p, "
		       "FinalResponseSent=~p, AllTerminated=~p, EndProcessing=~p",
		       [NewState_1#state.mystate, State#state.mystate, TimeLeft,
			NewState_1#state.final_response_sent, AllTerminated, EndProcessing]),
	    case EndProcessing of
		true ->
		    Targets = NewState_1#state.targets,
		    logger:log(debug, "sipproxy: All Targets terminated or completed. Ending process_wait(), "
			       "returning discontinue. debugfriendly(TargetList) :~n~p",
			       [targetlist:debugfriendly(Targets)]),
		    {discontinue, NewState_1};
		false ->
		    process_wait(EndTime, NewState_1)
	    end;
	timeout ->
	    {timeout, NewState};
	quit ->
	    %% Parent has exited, so we don't need to report any final response upstreams
	    {discontinue, NewState}
    end.

%%--------------------------------------------------------------------
%% @spec    ({cancel_pending}, State) ->
%%            {ok, NewState}
%%
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     We are asked to terminate all pending targets.
%% @end
%%--------------------------------------------------------------------
process_signal({cancel_pending, ExtraHeaders}, State) when is_record(State, state) ->
    Targets = State#state.targets,
    logger:log(debug, "sipproxy: Received 'cancel_pending', calling cancel_pending_targets()"),
    NewTargets = cancel_pending_targets(Targets, ExtraHeaders),
    %% If mystate is 'calling', we need to set it to 'cancelled' to prevent us from
    %% starting new transactions for additional destinations if we were to receive
    %% a 503 response.
    NewMyState = case State#state.mystate of
		     calling -> cancelled;
		     S -> S
		 end,
    NewState = State#state{targets = NewTargets,
			   mystate = NewMyState
			  },
    {ok, NewState};

%%--------------------------------------------------------------------
%% @spec    ({branch_result, ClientPid, Branch, NewTState, Response},
%%          State) ->
%%            {ok, NewState}
%%
%%            ClientPid = pid()
%%            Branch    = string()
%%            NewTState = term() "new client transaction state"
%%            Response  = #response{} | {Status, Reason}
%%            Status    = integer() "SIP status code"
%%            Reason    = string() "SIP reason phrase"
%%            State     = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     One of our client transactions reports something. Either
%%          it has received a response, timed out or been told to
%%          report something by someone.
%% @end
%%--------------------------------------------------------------------
process_signal({branch_result, ClientPid, Branch, NewTState, Response}, State)
  when is_pid(ClientPid), is_record(State, state) ->
    %% serialize the response we receive
    SPResponse =
	case Response of
	    _ when is_record(Response, response) ->
		#sp_response{status  = Response#response.status,
			     reason  = Response#response.reason,
			     header  = Response#response.header,
			     body    = Response#response.body,
			     created = false
			    };
	    {Status, Reason} when is_integer(Status), is_list(Reason) ->
		#sp_response{status  = Status,
			     reason  = Reason,
			     header  = keylist:from_list([]),
			     body    = <<>>,
			     created = true
			    }
	end,
    NewState = process_branch_result(ClientPid, Branch, NewTState, SPResponse, State),
    {ok, NewState};

%%--------------------------------------------------------------------
%% @spec    ({clienttransaction_terminating, ClientPid}, State) ->
%%            {ok, NewState}
%%
%%            ClientPid = pid() "pid of client transaction"
%%            State     = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     One of our client transactions reports that it is
%%          terminating.
%% @end
%%--------------------------------------------------------------------
process_signal({clienttransaction_terminating, ClientPid, Branch}, State) when is_pid(ClientPid),
									       is_record(State, state) ->
    Targets = State#state.targets,
    NewState =
	case targetlist:get_using_branch(Branch, Targets) of
	    none ->
		logger:log(error, "sipproxy: Received 'clienttransaction_terminating' from unknown Target (~p) "
			   "ignoring.", [ClientPid]),
		State;
	    ThisTarget ->
		%% Verify that we got the signal from the right pid. XXX handle wrong pid more gracefully?
		[ClientPid] = targetlist:extract([pid], ThisTarget),
		NewTarget1 = targetlist:set_state(ThisTarget, terminated),
		NewTargets = targetlist:update_target(NewTarget1, Targets),
		logger:log(debug, "sipproxy: Received notification that client transaction with pid ~p has terminated. "
			   "Targetlist :~n~p", [ClientPid, targetlist:debugfriendly(NewTargets)]),
		State#state{targets = NewTargets}
	end,
    {ok, NewState};

%%--------------------------------------------------------------------
%% @spec    ({showtargets}, State) -> {ok, State}
%%
%%            State = #state{}
%%
%% @doc     Someone wants us to log our current set of targets to the
%%          debug log.
%% @end
%%--------------------------------------------------------------------
process_signal({showtargets}, State) when is_record(State, state) ->
    logger:log(debug, "sipproxy: Received 'showtargets' request, debugfriendly(TargetList) :~n~p",
	       [targetlist:debugfriendly(State#state.targets)]),
    {ok, State};

%%--------------------------------------------------------------------
%% @spec    ({'EXIT', Parent, Reason}, State) ->
%%            {quit, State} |
%%            does not return
%%
%%            Parent = pid()
%%            Reason = term()
%%
%% @doc     If Pid matches our parent (State#state.parent), this
%%          function will match. We log the exit reason and then exit
%%          (hard) ourselves. This will cause the EXIT signal to be
%%          propagated to our client branches, which will CANCEL
%%          themselves if they are not already completed.
%% @end
%%--------------------------------------------------------------------
%%
%% final_response_sent == 'true' or mystate /= 'calling'
%%
process_signal({'EXIT', Pid, normal}, #state{parent			= Parent,
					     final_response_sent	= FRS,
					     mystate			= MyState
					    } = State)
  when Pid == Parent, FRS == true; MyState /= calling ->
    %% Parent exited when we are finishing up. This is considered normal, so we just exit too.
    {quit, State};

%%
%% final_response_sent == 'false', or mystate is 'calling'
%%
process_signal({'EXIT', Pid, Reason}, #state{parent = Parent}) when Pid == Parent ->
    %% Our parent has exited on us, either abnormally or before we sent a final response.
    %% Exit straight away since there is no point in us staying alive. The client branches
    %% are linked to this process, and will cancel themselves when we exit, if they need to.
    logger:log(error, "sipproxy: My parent (~p) just exited, so I will too (with error 'sipproxy_parent_died').",
	       [Parent]),
    logger:log(debug, "sipproxy: Parents (~p) exit-reason : ~p", [Parent, Reason]),
    erlang:exit(sipproxy_parent_died);

%%--------------------------------------------------------------------
%% @spec    ({'EXIT', Pid, Reason}, State) -> term()
%%
%%            Pid    = pid()
%%            Reason = term()
%%
%% @doc     Some other process than our parent has exited. Check if it
%%          was one of our client transaction. If it was, determine
%%          what it's final result should be (500 if the client
%%          transaction did not throw a siperror), and store that in
%%          our response context (target list). Note : Ideally, we
%%          should start a CANCEL transaction if it was an invite,
%%          that wasn't completed or terminated, that exited. We
%%          would need to know a few things more than we know today
%%          though - such as what branch the transport layer used for
%%          the INVITE etc.
%% @hidden
%% @end
%%--------------------------------------------------------------------
process_signal({'EXIT', Pid, Reason}, State) when is_record(State, state) ->
    Targets = State#state.targets,
    case targetlist:get_using_pid(Pid, Targets) of
	none ->
	    logger:log(error, "sipproxy: Got EXIT signal from unknown pid ~p, reason: ~p",
		       [Pid, Reason]),
	    {error, State};
	ThisTarget ->
	    NewTarget1 =
		case Reason of
		    normal ->
			logger:log(debug, "sipproxy: Branch with pid ~p exited normally", [Pid]),
			targetlist:set_state(ThisTarget, terminated);
		    {_, {siperror, Status, SipReason}} ->
			logger:log(error, "sipproxy: Branch with pid ~p FAILED : ~p ~s",
				   [Pid, Status, SipReason]),
			SPR = #sp_response{status  = Status,
					   reason  = SipReason,
					   created = true
					  },
			targetlist:set_endresult(ThisTarget, SPR);
		    {_, {siperror, Status, SipReason, ExtraHeaders}} ->
			%% We currently have no means to do anything intelligent with
			%% extra headers here. XXX.
			logger:log(error, "sipproxy: Branch with pid ~p FAILED : ~p ~s"
				   " (discarding extra headers : ~p)",
				   [Pid, Status, SipReason, ExtraHeaders]),
			SPR = #sp_response{status  = Status,
					   reason  = SipReason,
					   created = true
					  },
			targetlist:set_endresult(ThisTarget, SPR);
		    _ ->
			logger:log(error, "sipproxy: Branch with pid ~p exited ABNORMALLY:~n~p",
				   [Pid, Reason]),
			SPR = #sp_response{status  = 500,
					   reason  = "Server Internal Error",
					   created = true
					  },
			targetlist:set_endresult(ThisTarget, SPR)
		end,
	    NewTarget = targetlist:set_state(NewTarget1, terminated),
	    NewTargets = targetlist:update_target(NewTarget, Targets),
	    {ok, State#state{targets = NewTargets}}
    end;

process_signal(Msg, State) when is_record(State, state) ->
    logger:log(error, "sipproxy: Received unknown message ~p, ignoring", [Msg]),
    {error, State}.

%%--------------------------------------------------------------------
%% @spec    (Response, Target, State) ->
%%            NewState
%%
%%            Response = #response{}
%%            Target   = term() "targetlist target"
%%            State    = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Examine a response we have received, and if it is a 503
%%          (we fake receiving 503's when the transport layer
%%          indicates failure), we start a new client transaction for
%%          the next destination in this targets destination list.
%%          The targets destination list is typically a list of
%%          destinations derived from a URI.
%% @end
%%--------------------------------------------------------------------
try_next_destination(Status, Target, State) when is_integer(Status), is_record(State, state) ->
    case {Status, State#state.mystate} of
	{430, calling} ->
	    case targetlist:extract([user_instance], Target) of
		[{User, Instance}] when is_list(User), is_list(Instance) ->
		    SameUserInstance = [E || E <- State#state.surplus,
					     E#sipproxy_action.user == User,
					     E#sipproxy_action.instance == Instance],
		    case SameUserInstance of
			[] ->
			    logger:log(debug, "sipproxy: Received 430 response, but I have no surplus actions for "
				       "user ~p, instance ~p", [User, Instance]),
			    State;
			[NextAction | _] ->
			    NewSurplus = State#state.surplus -- [NextAction],
			    try_next_destination_430(NextAction, Target, {User, Instance},
						     State#state{surplus = NewSurplus})
		    end;
		_ ->
		    State
	    end;
	{430, S} ->
	    case targetlist:extract([user_instance], Target) of
		[{User, Instance}] when is_list(User), is_list(Instance) ->
		    logger:log(debug, "sipproxy: Received response 430 but not trying next destination "
			       "when I'm in state ~p", [S]);
		_ ->
		    ok
	    end,
	    State;
	{503, calling} ->
	    %% The first entry in dstlist is the one we have just finished with,
	    %% not the next one to try.
	    [[_FailedDst | DstList]] = targetlist:extract([dstlist], Target),
	    case get_next_sipdst(DstList) of
		[] ->
		    logger:log(debug, "sipproxy: Received response ~p, but there are no more destinations to try "
			       "for this target", [Status]),
		    State;
		[FirstDst | _] = NewDstList ->
		    [Branch, UserInstance] = targetlist:extract([branch, user_instance], Target),
		    NewBranch = get_next_target_branch(Branch),
		    logger:log(debug, "sipproxy: Received response ~p, starting new branch ~p for next destination ~s",
			       [Status, NewBranch, sipdst:dst2str(FirstDst)]),
		    [Request, Timeout] = targetlist:extract([request, timeout], Target),
		    %% XXX ideally we should not try to contact the same host over UDP, when
		    %% we receive a transport layer error for TCP, if there were any other
		    %% equally preferred hosts in the SRV response for a destination.
		    %% It makes more sense to try to connect to Proxy B over TCP/UDP than to
		    %% try Proxy A over UDP when Proxy A over TCP has just failed.
		    case local:start_client_transaction(Request, FirstDst, NewBranch, Timeout) of
			BranchPid when is_pid(BranchPid) ->
			    NT = targetlist:add(NewBranch, Request, BranchPid, calling, Timeout, NewDstList,
						UserInstance, State#state.targets),
			    State#state{targets = NT};
			{error, E} ->
			    logger:log(error, "sipproxy: Failed starting client transaction : ~p", [E]),
			    State
		    end
	    end;
	{503, S} ->
	    logger:log(debug, "sipproxy: Received response 503 but not trying next destination when I'm in state ~p",
		       [S]),
	    State;
	_ ->
	    State
    end.

try_next_destination_430(NextAction, Target, UserInstance, State) when is_record(NextAction, sipproxy_action) ->
    [Branch, CallTimeout] = targetlist:extract([branch, timeout], Target),
    NewBranch = get_next_target_branch(Branch),
    logger:log(debug, "sipproxy: Received 430 response, starting new branch ~p from surplus supply",
	       [NewBranch]),

    #state{request	  = OrigRequest,
	   approx_msgsize = ApproxMsgSize
	  } = State,
    {ok, Request, DstList} = make_new_target_request(OrigRequest, ApproxMsgSize, NextAction),
    case DstList of
	[FirstDst | _] = DstList ->
	    case local:start_client_transaction(Request, FirstDst, NewBranch, CallTimeout) of
		BranchPid when is_pid(BranchPid) ->
		    NT = targetlist:add(NewBranch, Request, BranchPid, calling,
					CallTimeout, DstList, UserInstance, State#state.targets),
		    State#state{targets = NT};
		{error, E} ->
		    logger:log(error, "sipproxy: Failed starting client transaction : ~p", [E]),
		    State
	    end;
	[] ->
	    logger:log(normal, "~s: Target URI ~s did not resolve to any destinations we could use - ignoring",
		       [NewBranch, sipurl:print(Request#request.uri)]),
	    State;
	{error, Reason} ->
	    logger:log(normal, "~s: Failed resolving URI ~s - ignoring this target (error : ~p)",
		       [NewBranch, sipurl:print(Request#request.uri), Reason]),
	    State
    end.

%%--------------------------------------------------------------------
%% @spec    (DstList) ->
%%            NewDstList
%%
%%            DstList = [#sipdst{}]
%%
%%            NewDstList = [#sipdst{}]
%%
%% @doc     Get the next entry in DstList that the transport layer
%%          considers eligible. This includes checking if the dest-
%%          ination is currently blacklisted or not.
%% @end
%%--------------------------------------------------------------------
get_next_sipdst([H | T] = DstList) ->
    case transportlayer:is_eligible_dst(H) of
	true ->
	    DstList;
	{false, Reason} ->
	    logger:log(debug, "sipproxy: Skipping non-eligible destination (~s) : ~s", [Reason, sipdst:dst2str(H)]),
	    get_next_sipdst(T)
    end;
get_next_sipdst([]) ->
    [].


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Out
%%
%%            In = string()
%%
%%            Out = string()
%%
%% @doc     Create the next branch given the current one. Basically we
%%          increase the number at the end, or add ".1" if there is
%%          no .number at the end.
%% @end
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
%% @spec    (State) -> true | false
%%
%% @equiv    allterminated(State#state.targets)
%% @end
%%--------------------------------------------------------------------
allterminated(State) when is_record(State, state) ->
    allterminated(State#state.targets);

%%--------------------------------------------------------------------
%% @spec    (TargetList) -> true | false
%%
%%            TargetList = #targetlist{}
%%
%% @doc     Determine if all targets are terminated (that is, no
%%          targets are in the states calling, trying or proceeding).
%% @end
%%--------------------------------------------------------------------
allterminated(TargetList) ->
    TargetsCalling = targetlist:get_targets_in_state(calling, TargetList),
    TargetsTrying = targetlist:get_targets_in_state(trying, TargetList),
    TargetsProceeding = targetlist:get_targets_in_state(proceeding, TargetList),
    if
	TargetsCalling /= [] -> false;
	TargetsTrying /= [] -> false;
	TargetsProceeding /= [] -> false;
	true -> true
    end.

%%--------------------------------------------------------------------
%% @spec    (EndTime, State) -> true | false
%%
%%            EndTime = integer() "absulute time we should end"
%%            State   = #state{}
%%
%% @doc     Determine if we should end processing or not (by looking
%%          at the endtime element of our State record).
%% @end
%%--------------------------------------------------------------------
end_processing(EndTime, State) when is_integer(EndTime), is_record(State, state), State#state.mystate == stayalive ->
    Now = util:timestamp(),
    if
	Now =< EndTime -> false;
	true -> true
    end;
end_processing(_EndTime, State) when is_record(State, state), State#state.mystate == completed ->
    true;
end_processing(_EndTime, State) when is_record(State, state) ->
    allterminated(State).

%%--------------------------------------------------------------------
%% @spec    (AllTerminated, State) ->
%%            NewState
%%
%%            AllTerminated = true | false
%%            State         = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     If AllTerminated is true, this function decides what final
%%          response we should report upstreams. Note : We need to
%%          forward 2xx responses to INVITE to upstream even if we
%%          are 'cancelled'.
%% @end
%%--------------------------------------------------------------------
%%
%% AllTerminated == true, final_response_sent == false, mystate == calling or cancelled
%%
report_upstreams(true, #state{final_response_sent = false, mystate = MyState} = State)
  when MyState == calling; MyState == cancelled ->
    Responses = targetlist:get_responses(State#state.targets),
    logger:log(debug, "sipproxy: All targets are terminated, evaluating responses :~n~p",
	       [printable_responses(Responses)]),
    ForwardResponse =
	case Responses of
	    [] ->
		logger:log(normal, "sipproxy: No responses to choose between, answering '408 Request Timeout'"),
		{408, "Request Timeout"};
	    _ when is_list(Responses) ->
		case make_final_response(Responses) of
		    none ->
			logger:log(normal, "sipproxy: Found no suitable response in list ~p, "
				   "answering '500 No answers collected'", [printable_responses(Responses)]),
			{500, "No answers collected"};
		    R503 when is_record(R503, sp_response), R503#sp_response.status == 503 ->
			%% RFC3261 16.7 (Choosing the best response) bullet 6 says that if
			%% we only have a 503, we should send a 500 instead
			logger:log(normal, "sipproxy: Turning best response '503 ~s' into a "
				   "'500 Only response to fork was 503'", [R503#sp_response.reason]),
			{500, "Only response to fork was 503"};
		    #sp_response{created = false} = SP_FR ->
			#sp_response{status = Status,
				     reason = Reason,
				     header = Header,
				     body   = Body
				    } = SP_FR,
			logger:log(debug, "sipproxy: Picked best response '~p ~s' (received)", [Status, Reason]),
			#response{status = Status,
				  reason = Reason,
				  header = Header,
				  body = Body
				 };
		    SP_FR when is_record(SP_FR, sp_response) ->
			#sp_response{status = Status,
				     reason = Reason
				    } = SP_FR,
			logger:log(debug, "sipproxy: Picked best response '~p ~s' (created)", [Status, Reason]),
			{Status, Reason}
		end
	end,
    Parent = State#state.parent,
    logger:log(debug, "sipproxy: Sending the result to parent Pid ~p.", [Parent]),
    %% ForwardResponse is either a response record() or a {Status, Reason} tuple
    util:safe_signal("sipproxy: ", Parent, {sipproxy_all_terminated, self(), ForwardResponse}),
    State#state{mystate		    = completed,
		final_response_sent = true
	       };
%%
%% AllTerminated = false, final_response_sent == true, or mystate is not calling or cancelled
%%
report_upstreams(_AllTerminated, State) when is_record(State, state) ->
    State.

%%--------------------------------------------------------------------
%% @spec    (ClientPid, Branch, NewTState, SPResponse, State) ->
%%            NewState
%%
%%            ClientPid  = pid()
%%            Branch     = string()
%%            NewTState  = term() "new client transaction state"
%%            SPResponse = #sp_response{}
%%            Status     = integer() "SIP status code"
%%            Reason     = string() "SIP reason phrase"
%%            State      = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     A branch has reported a result to us. Check what we need
%%          to do with it, and do it.
%% @end
%%--------------------------------------------------------------------
process_branch_result(ClientPid, Branch, NewTState, SPResponse, State) when is_pid(ClientPid), is_list(Branch),
									    is_record(SPResponse, sp_response),
									    is_record(State, state) ->
    Targets = State#state.targets,
    #sp_response{status = Status,
		 reason = Reason
		} = SPResponse,
    case targetlist:get_using_branch(Branch, Targets) of
	none ->
	    logger:log(error, "sipproxy: Received branch result '~p ~s' from an unknown "
		       "Target (pid ~p, branch ~p), ignoring.", [Status, Reason, ClientPid, Branch]),
	    State;
	ThisTarget ->
	    %% By matching on ClientPid here, we make sure we got the signal from
	    %% the right process. XXX handle wrong pid more gracefully than crashing!
	    [ClientPid, ResponseToRequest, OldTState] = targetlist:extract([pid, request, state], ThisTarget),
	    #request{method = RMethod,
		     uri    = RURI
		    } = ResponseToRequest,
	    logger:log(debug, "sipproxy: Received branch result '~p ~s' from ~p (request: ~s ~s). ",
		       [Status, Reason, ClientPid, RMethod, sipurl:print(RURI)]),
	    NewTarget1 = targetlist:set_state(ThisTarget, NewTState),
	    NewTarget2 = targetlist:set_endresult(NewTarget1, SPResponse),	% XXX only do this for final responses?
	    NewTargets1 = targetlist:update_target(NewTarget2, Targets),
	    NewState1 = State#state{targets = NewTargets1},
	    NewState2 = try_next_destination(Status, ThisTarget, NewState1),
	    case (OldTState == NewTState) of
		true ->
		    ok;	%% No change, don't have to log verbose things
		false ->
		    logger:log(debug, "sipproxy: Extra debug: Target state changed from '~p' to '~p'.~n"
			       "My Targets-list (response context) now contain :~n~p",
			       [OldTState, NewTState, targetlist:debugfriendly(NewState2#state.targets)])
	    end,
	    NewState3 = check_forward_immediately(ResponseToRequest, SPResponse, Branch, NewState2),
	    NewState4 = cancel_pending_if_invite_2xx_or_6xx(RMethod, Status, Reason, NewState3),
	    NewState4
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, SPResponse, Branch, State) ->
%%            NewState
%%
%%            Request    = #request{}
%%            SPResponse = #sp_response{}
%%            Branch     = string()
%%            State      = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     When one of our client transactions receive a response,
%%          this function gets called to see if it is a response that
%%          we are supposed to send to our parent immediately. Such
%%          responses are provisional responses (1xx) except 100, and
%%          2xx responses to INVITE.
%% @end
%%--------------------------------------------------------------------
check_forward_immediately(_Request, #sp_response{status = Status, created = true}, _Branch, _State)
  when Status == 401; Status == 407 ->
    %% We can't create authorization requests in client transactions! Firstly, it
    %% makes no sense - secondly we can't aggregate any authorization headers into
    %% {Status, Reason} tuples. XXX maybe turn this into a 500, 400 or something?
    throw({error, sipproxy_cant_create_auth_response});
check_forward_immediately(Request, SPResponse, Branch, State)
  when is_record(Request, request), is_record(SPResponse, sp_response), is_record(State, state) ->
    #request{method = Method,
	     uri    = URI
	    } = Request,
    #sp_response{status = Status,
		 reason = Reason
		} = SPResponse,
    case forward_immediately(Method, Status, State) of
	{true, NewState} ->
	    Parent = NewState#state.parent,
	    logger:log(debug, "sipproxy: Forwarding response '~p ~s' (in response to ~s ~s) to my parent (pid ~p) "
		       "immediately", [Status, Reason, Method, sipurl:print(URI), Parent]),

	    %% RFC 3261 16.7 bullet 5 (Check response for forwarding) says we MUST process any
	    %% response chosen for immediate forwarding as described in
	    %% "Aggregate Authorization Header Field Values" through "Record-Route".
	    %% This makes sense if it is an 401 or an 407 that forward_immediately() tells us
	    %% to forward immediately - otherwise this is a no-op.
	    Targets = NewState#state.targets,
	    Responses = targetlist:get_responses(Targets),
	    FwdSPR = aggregate_authreqs(SPResponse, Responses),
	    %% Make a response record out of our internal variant sp_response
	    FwdResponse =
		case FwdSPR#sp_response.created of
		    true ->
			{FwdSPR#sp_response.status, FwdSPR#sp_response.reason};
		    false ->
			#response{status = FwdSPR#sp_response.status,
				  reason = FwdSPR#sp_response.reason,
				  header = FwdSPR#sp_response.header,
				  body   = FwdSPR#sp_response.body
				 }
		end,
	    util:safe_signal("sipproxy: ", Parent, {sipproxy_response, self(), Branch, FwdResponse}),
	    NewState;
	false ->
	    logger:log(debug, "sipproxy: Not forwarding response '~p ~s' (in response to ~s ~s) immediately",
		       [Status, Reason, Method, sipurl:print(URI)]),
	    State
    end.

%%--------------------------------------------------------------------
%% @spec    (Method, Status, State) ->
%%            {true, NewState} | false
%%
%%            Method = string()
%%            Status = integer() "SIP status code"
%%            State  = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Return 'true' if this is a response that we are supposed
%%          to send to our parent immediately. Such responses are
%%          provisional responses (1xx) except 100, and 2xx responses
%%          to INVITE.
%% @end
%%--------------------------------------------------------------------
forward_immediately(Method, Status, State) when is_record(State, state), Status == 100 ->
    %% A 100 Trying is considered an illegal response here since it is hop-by-hop and should
    %% be 'filtered' at the transaction layer.
    logger:log(error, "sipproxy: Invalid response ~p to ~s", [Status, Method]),
    false;
forward_immediately(_Method, Status, State) when is_record(State, state), Status >= 101, Status =< 199 ->
    %% Provisional responses (except 100) are always forwarded immediately,
    %% regardless of Method
    {true, State};
forward_immediately("INVITE", Status, State) when is_record(State, state), Status >= 200, Status =< 299 ->
    %% 2xx responses to INVITE are always forwarded immediately, regardless of final_response_sent
    NewState = State#state{mystate = completed, final_response_sent = true},
    {true, NewState};
forward_immediately(_Method, Status, #state{final_response_sent = FRS} = State)
  when FRS /= true, Status >= 200, Status =< 299 ->
    %% 2xx responses to non-INVITE are forwarded immediately unless we have already
    %% forwarded some other final response already (implicitly another 2xx).
    NewState = State#state{mystate = completed, final_response_sent = true},
    {true, NewState};
forward_immediately(_Method, Status, State) when is_record(State, state), Status =< 699 ->
    %% No other final responses are forwarded immediately
    false.

%%--------------------------------------------------------------------
%% @spec    (Method, Status, Reason, State) ->
%%            NewState
%%
%%            Method = string()
%%            Status = integer() "SIP status code"
%%            Reason = string() "SIP reason phrase"
%%            State  = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     If we receive a 2xx response to INVITE, or a 6xx response
%%          to any request we should terminate all pending targets.
%% @end
%%--------------------------------------------------------------------
cancel_pending_if_invite_2xx_or_6xx("INVITE", Status, _Reason, State) when is_record(State, state), Status =< 199 ->
    State;
cancel_pending_if_invite_2xx_or_6xx("INVITE", Status, _Reason, State) when is_record(State, state), Status =< 299 ->
    logger:log(debug, "sipproxy: Cancelling pending targets since one INVITE transaction "
	       "resulted in a 2xx response ~p", [Status]),
    ReasonStr = "SIP; cause=200; text=\"Call completed elsewhere\"",
    ExtraHeaders = [{"Reason", [ReasonStr]}],
    NewTargets = cancel_pending_targets(State#state.targets, ExtraHeaders),
    State#state{targets = NewTargets};
cancel_pending_if_invite_2xx_or_6xx(_Method, Status, _Reason, State) when is_record(State, state), Status =< 599 ->
    %% non-INVITE and not 6xx
    State;
cancel_pending_if_invite_2xx_or_6xx(_Method, Status, Reason, State) when is_record(State, state), Status =< 699 ->
    logger:log(debug, "sipproxy: Cancelling pending targets since one branch resulted in a 6xx response ~p", [Status]),
    ReasonStr = lists:flatten( io_lib:format("SIP; cause=~p; text=~p", [Status, Reason]) ),
    ExtraHeaders = [{"Reason", [ReasonStr]}],
    NewTargets = cancel_pending_targets(State#state.targets, ExtraHeaders),
    State#state{targets = NewTargets}.

%%--------------------------------------------------------------------
%% @spec    (Responses) -> [string()]
%%
%%            Responses = [#sp_response{}]
%%
%% @doc     Format a list of responses for (debug) logging.
%% @end
%%--------------------------------------------------------------------
printable_responses(In) ->
    printable_responses2(In, []).

printable_responses2([], Res) ->
    lists:reverse(Res);
printable_responses2([H | T], Res) when is_record(H, sp_response) ->
    This = lists:concat([H#sp_response.status, " ", H#sp_response.reason]),
    printable_responses2(T, [This | Res]).

%%--------------------------------------------------------------------
%% @spec    (Responses) -> #sp_response{} | none
%%
%%            Responses = [Res]
%%            Res       = #response{} | {Status, Reason}
%%            Status    = integer() "SIP status code"
%%            Reason    = string() "SIP reason phrase"
%%
%% @doc     Determine which response is the best response in a
%%          response context (Responses). Perform any necessary
%%          authentication-header aggregation and return the response
%%          to deliver to our parent. Note : Look for 6xx responses,
%%          then pick the lowest but prefer 401, 407, 415, 420 and
%%          484 if we choose a 4xx and avoid 503 if we choose a 5xx.
%%          This is described in RFC3261 #16.7 (Response Processing),
%%          bullet 6. (Choosing the best response).
%% @end
%%--------------------------------------------------------------------
make_final_response(Responses) ->
    TwoxxResponses = get_xx_responses(200, Responses),
    ThreexxResponses = get_xx_responses(300, Responses),
    FourxxResponses = get_xx_responses(400, Responses),
    FivexxResponses = get_xx_responses(500, Responses),
    SixxxResponses = get_xx_responses(600, Responses),
    if
	SixxxResponses /= [] ->
	    pick_response(6, SixxxResponses);
	TwoxxResponses /= [] ->
	    pick_response(2, TwoxxResponses);
	ThreexxResponses /= [] ->
	    pick_response(3, ThreexxResponses);
	FourxxResponses /= [] ->
	    BestFourxx = pick_response(4, FourxxResponses),
	    aggregate_authreqs(BestFourxx, Responses);
	FivexxResponses /= [] ->
	    pick_response(5, FivexxResponses);
	true ->
	    logger:log(debug, "sipproxy: No response to my liking"),
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (BestResponse, Responses) -> #sp_response{} | none
%%
%%            BestResponse = #sp_response{}
%%            Responses    = [#sp_response{}]
%%
%% @doc     Put authentication headers from all responses in Responses
%%          into the one we have decided to send to our parent -
%%          BestResponse. Note : This is described in RFC3261 #16.7
%%          (Response Processing), bullet 7. (Aggregate Authorization
%%          Header Field Values).
%% @end
%%--------------------------------------------------------------------
%%
%% BestResponse status == 407, Proxy-Authenticate
%%
aggregate_authreqs(#sp_response{status = 407} = BestResponse, Responses) when is_list(Responses) ->
    #sp_response{reason = Reason,
		 header = Header} = BestResponse,
    ProxyAuth = collect_auth_headers(407, 'proxy-authenticate', Responses),
    logger:log(debug, "Aggregating ~p Proxy-Authenticate headers into response '407 ~s'",
	       [length(ProxyAuth), Reason]),
    NewHeader = keylist:set("Proxy-Authenticate", ProxyAuth, Header),
    BestResponse#sp_response{header = NewHeader};
%%
%% BestResponse status == 401, WWW-Authenticate
%%
aggregate_authreqs(#sp_response{status=401}=BestResponse, Responses) when is_list(Responses) ->
    #sp_response{reason = Reason,
		 header = Header} = BestResponse,
    WWWAuth = collect_auth_headers(401, 'www-authenticate', Responses),
    logger:log(debug, "Aggregating ~p WWW-Authenticate headers into response '401 ~s'",
	       [length(WWWAuth), Reason]),
    NewHeader = keylist:set("WWW-Authenticate", WWWAuth, Header),
    BestResponse#sp_response{header = NewHeader};
%%
%% BestResponse status is neither 401 nor 407
%%
aggregate_authreqs(BestResponse, Responses) when is_record(BestResponse, sp_response), is_list(Responses) ->
    BestResponse.

%%--------------------------------------------------------------------
%% @spec    (Status, Key, Responses) ->
%%            HeaderValue
%%
%%            Status    = integer() "SIP status code"
%%            Key       = term() "keylist key of headers to look for"
%%            Responses = [#sp_response{}]
%%
%%            HeaderValue = [string()]
%%
%% @doc     Part of aggregate_authreqs(). Get all headers matched by
%%          Key from all responses in Responses which have status
%%          Status.
%% @end
%%--------------------------------------------------------------------
collect_auth_headers(Status, Key, Responses) ->
    collect_auth_headers2(Status, Key, Responses, []).

collect_auth_headers2(_Status, _Key, [], Res) ->
    Res;
collect_auth_headers2(Status, Key, [#sp_response{status=Status}=H | T], Res) ->
    case keylist:fetch(Key, H#sp_response.header) of
	[] ->
	    collect_auth_headers2(Status, Key, T, Res);
	This ->
	    collect_auth_headers2(Status, Key, T, Res ++ This)
    end;
collect_auth_headers2(Status, Key, [H | T], Res) when is_record(H, sp_response) ->
    collect_auth_headers2(Status, Key, T, Res).

%%--------------------------------------------------------------------
%% @spec    (Nxx, Responses) ->
%%            BestResponse
%%
%%            Nxx       = integer() "4 for 4xx repsonses etc."
%%            Responses = [#sp_response{}]
%%
%%            BestResponse = #response{}
%%
%% @doc     Pick a response from class Nxx. We use some different
%%          sorting routines depending on Nxx.
%% @end
%%--------------------------------------------------------------------
pick_response(4, FourxxResponses) when is_list(FourxxResponses) ->
    hd(lists:sort(fun pick_4xx_sort/2, FourxxResponses));
pick_response(5, FivexxResponses) when is_list(FivexxResponses) ->
    hd(lists:sort(fun avoid_503_sort/2, FivexxResponses));
pick_response(_Nxx, Responses) when is_list(Responses) ->
    hd(Responses).

%% part of pick_response(), 4xx response sorting
pick_4xx_sort(A, B) when is_record(A, sp_response), is_record(B, sp_response) ->
    Apref = is_4xx_preferred(A),
    Bpref = is_4xx_preferred(B),
    if
	Apref == Bpref ->
	    %% A and B is either both in the preferred list, or both NOT in the preferred list.
	    %% Pick using numerical sort of response codes.
	    (A#sp_response.status =< B#sp_response.status);
	Apref == true ->
	    true;
	true ->
	    false
    end.

%% part of pick_response(), 4xx response sorting
is_4xx_preferred(Response) when is_record(Response, sp_response) ->
    lists:member(Response#sp_response.status, [401, 407, 415, 420, 484]).

%% part of pick_response(), 5xx response sorting
avoid_503_sort(#sp_response{status=503}, Bresponse) when is_record(Bresponse, sp_response) ->
    false;
%% part of pick_response(), 5xx response sorting
avoid_503_sort(Aresponse, #sp_response{status=503}) when is_record(Aresponse, sp_response) ->
    true;
avoid_503_sort(#sp_response{status=Astatus}, #sp_response{status=Bstatus}) ->
    (Astatus =< Bstatus).

%%--------------------------------------------------------------------
%% @spec    (Min, Responses) ->
%%            BestResponse
%%
%%            Min       = integer() "number evenly divided by 100"
%%            Responses = [#sp_response{}]
%%
%%            BestResponse = #sp_response{}
%%
%% @doc     Get all responses between Min and Min + 99. sorting
%%          routines depending on Nxx.
%% @end
%%--------------------------------------------------------------------
get_xx_responses(Min, Responses) ->
    Max = Min + 99,
    lists:keysort(1, get_range_responses(Min, Max, Responses, [])).

%% part of get_xx_responses()
get_range_responses(_Min, _Max, [], Res) ->
    lists:reverse(Res);
get_range_responses(Min, Max, [H | T], Res) when is_record(H, sp_response) ->
    if
	H#sp_response.status < Min -> get_range_responses(Min, Max, T, Res);
	H#sp_response.status > Max -> get_range_responses(Min, Max, T, Res);
	true ->
	    get_range_responses(Min, Max, T, [H | Res])
    end.



%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    Self = self(),

    %% test start_check_actions(Actions)
    %%--------------------------------------------------------------------
    CheckAction_Call1 = #sipproxy_action{action = call, timeout = 1},
    CheckAction_Call2 = #sipproxy_action{action = call, timeout = 2},
    CheckAction_Wait1 = #sipproxy_action{action = wait, timeout = 3},
    CheckAction_Wait2 = #sipproxy_action{action = wait, timeout = 4},

    autotest:mark(?LINE, "start_check_actions/1 - 1"),
    %% test normal case
    ok = start_check_actions([CheckAction_Call1, CheckAction_Wait1]),

    autotest:mark(?LINE, "start_check_actions/1 - 2"),
    %% test normal case #2
    ok = start_check_actions([CheckAction_Call1, CheckAction_Wait1,
			      CheckAction_Call2, CheckAction_Wait2]),

    autotest:mark(?LINE, "start_check_actions/1 - 3"),
    %% test only call
    {error, _} = start_check_actions([CheckAction_Call2]),

    autotest:mark(?LINE, "start_check_actions/1 - 4"),
    %% test only wait
    {error, _} = start_check_actions([CheckAction_Wait1, CheckAction_Wait2]),


    %% test start_actions(BranchBase, Parent, OrigRequest, Actions)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "start_actions/4 - 1.1"),
    %% test normal case, but with too low Max-Forwards for a fork to actually happen
    StartAcReq1 = #request{method = "OPTIONS", uri = sipurl:parse("sip:ft@testcase.example.org"),
			   header = keylist:from_list([{"Max-Forwards", ["1"]}]), body = <<>>},
    error = start_actions("testbranchbase", self(), StartAcReq1, [CheckAction_Call1, CheckAction_Wait1], []),

    autotest:mark(?LINE, "start_actions/4 - 1.2"),
    check_we_got_signal({sipproxy_all_terminated, Self, {483, "Too Many Hops"}}),
    check_we_got_signal({sipproxy_terminating, Self}),

    autotest:mark(?LINE, "start_actions/4 - 2"),
    %% test failure case
    error = start_actions("foo", self(), #request{}, [], []),


    %% test start(BranchBase, Parent, Request, Actions, Timeout)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "start/5 - 0"),
    StartRef = make_ref(),
    Me = self(),
    Line5 = ?LINE + 1,
    StartSigCol = spawn_link(fun() -> signal_collector(Me, StartRef, passive, Line5) end),

    autotest:mark(?LINE, "start/5 - 1"),
    %% Test with Route header
    StartReq1 = #request{method = "OPTIONS", uri = sipurl:parse("sip:ft@testcase.example.org"),
			 header = keylist:from_list([{"Route", ["<sip:testcase.example.org>"]}]), body = <<>>},
    {error, "Request with Route header could not be forked"} =
	start("testbranchbase", StartSigCol, StartReq1, [], [], 0),
    StartExpectedSignals1 = [{sipproxy_all_terminated, Self,
			      {500, "Server Internal Error"}},
			     {sipproxy_terminating, Self}],
    poll_signal_collector(StartRef, StartSigCol, StartExpectedSignals1),

    autotest:mark(?LINE, "start/5 - 2.0"),
    %% Test normal case (but only wait action, so a 500 will be generated)
    StartReq2 = #request{method = "OPTIONS", uri = sipurl:parse("sip:ft@testcase.example.org"),
			 header = keylist:from_list([]), body = <<>>},

    autotest:mark(?LINE, "start/5 - 2.1"),
    ok = start("testbranchbase", StartSigCol, StartReq2, [#sipproxy_action{action = wait, timeout = 0}], [], 0),

    autotest:mark(?LINE, "start/5 - 2.2"),
    StartExpectedSignals2 = [{sipproxy_all_terminated, Self,
			      {500, "Server Internal Error"}},
			     {sipproxy_terminating, Self}],
    poll_signal_collector(StartRef, StartSigCol, StartExpectedSignals2),

    StartSigCol ! {quit, self()},



    %% test fork(EndTime, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "fork/2 - 0"),
    %% test that we send a 408 Request Timeout if we reach our timeout and
    %% haven't sent a final response yet. This can happen if one of our
    %% branches has been cancelled but have not received a response yet.
    ForkList0 = targetlist:empty(),
    ForkRequest1 = #request{method = "INVITE", uri = sipurl:parse("sip:ft@example.org"),
			    header = keylist:from_list([]), body = <<>>},
    %% start a signal collector
    ForkRef1 = make_ref(),
    Line_Fork2 = ?LINE,
    ForkSigCollect1 = spawn_link(fun() -> signal_collector(Me, ForkRef1, passive, Line_Fork2 + 1) end),
    ForkList1_1 = targetlist:add("branch1", ForkRequest1, ForkSigCollect1, calling,
			       1, [], none, ForkList0),
    ForkList1 = targetlist:add("branch2", ForkRequest1, ForkSigCollect1, terminated,
			       1, [], none, ForkList1_1),

    autotest:mark(?LINE, "fork/2 - 1.1"),
    ForkState1 = #state{request = ForkRequest1,
			timeout = 0,
			parent  = ForkSigCollect1,
			targets = ForkList1,
			actions = []
		       },
    ok = fork(0, ForkState1),

    autotest:mark(?LINE, "fork/2 - 1.2"),
    ForkCancelCast = {'$gen_cast', {cancel, "cancel_targets_state", []}},
    ForkExpectSignals1 = [ForkCancelCast,
			  {sipproxy_no_more_actions, Self},
			  {sipproxy_all_terminated, Self, {408, "Request Timeout"}}
			 ],
    poll_signal_collector(ForkRef1, ForkSigCollect1, ForkExpectSignals1),

    autotest:mark(?LINE, "fork/2 - 2.1"),
    %% test all terminated
    ForkList2 = targetlist:add("terminatedbranch", ForkRequest1, ForkSigCollect1, terminated,
			       1, [], none, ForkList0),
    ForkState2 = #state{request = ForkRequest1,
			timeout = 0,
			parent  = ForkSigCollect1,
			targets = ForkList2,
			actions = [],
			final_response_sent = true
		       },
    ok = fork(0, ForkState2),

    autotest:mark(?LINE, "fork/2 - 2.2"),
    %% verify that no signals were sent to the signal collector
    poll_signal_collector(ForkRef1, ForkSigCollect1, []),

    autotest:mark(?LINE, "fork/2 - 3.1"),
    %% test generation of 500 Server Internal Error when no final repsonse has been generated,
    %% for whatever reason
    ForkState3 = ForkState2#state{final_response_sent = false},
    ok = fork(0, ForkState3),

    autotest:mark(?LINE, "fork/2 - 3.2"),
    ForkExpectedSignals3 = [{sipproxy_all_terminated, Self, {500, "Server Internal Error"}}],
    poll_signal_collector(ForkRef1, ForkSigCollect1, ForkExpectedSignals3),


    ForkSigCollect1 ! {quit, self()},


    %% test get_next_target_branch(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_next_target_branch - 1"),
    "z9hG4bK-really-unique.1" = get_next_target_branch("z9hG4bK-really-unique"),

    autotest:mark(?LINE, "get_next_target_branch - 2"),
    "z9hG4bK-really-unique.10" = get_next_target_branch("z9hG4bK-really-unique.9"),

    autotest:mark(?LINE, "get_next_target_branch - 3"),
    "z9hG4bK-really-unique.foo.1" = get_next_target_branch("z9hG4bK-really-unique.foo"),


    %% test mark_cancelled(Targets, TargetList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "mark_cancelled/2 - 0"),
    MarkCancelledList0 = targetlist:empty(),
    MarkCancelledList1 = targetlist:add("branch1", #request{}, self(), calling,
					1, [], none, MarkCancelledList0),
    MarkCancelledList2 = targetlist:add("branch2", #request{}, self(), terminated,
					1, [], none, MarkCancelledList1),
    MarkCancelled_T1_1 = targetlist:get_using_branch("branch1", MarkCancelledList2),
    MarkCancelled_T2_1 = targetlist:get_using_branch("branch2", MarkCancelledList2),

    autotest:mark(?LINE, "mark_cancelled/2 - 1.1"),
    MarkCancelledList2_1 = mark_cancelled([MarkCancelled_T1_1, MarkCancelled_T2_1],
					  MarkCancelledList2),

    autotest:mark(?LINE, "mark_cancelled/2 - 1.2"),
    %% verify the results (before)
    ["branch1", false] = targetlist:extract([branch, cancelled], MarkCancelled_T1_1),
    ["branch2", false] = targetlist:extract([branch, cancelled], MarkCancelled_T2_1),

    autotest:mark(?LINE, "mark_cancelled/2 - 1.3"),
    %% verify the results (after)
    MarkCancelled_T1_2 = targetlist:get_using_branch("branch1", MarkCancelledList2_1),
    MarkCancelled_T2_2 = targetlist:get_using_branch("branch2", MarkCancelledList2_1),
    ["branch1", true] = targetlist:extract([branch, cancelled], MarkCancelled_T1_2),
    ["branch2", true] = targetlist:extract([branch, cancelled], MarkCancelled_T2_2),


    %% test allterminated(State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "allterminated/1 - 0"),
    AllTerminatedList0 = targetlist:empty(),
    AllTerminatedList1 = targetlist:add("branch1", #request{}, self(), calling,
					1, [], none, AllTerminatedList0),
    AllTerminatedList2 = targetlist:add("branch2", #request{}, self(), terminated,
					1, [], none, AllTerminatedList1),

    autotest:mark(?LINE, "allterminated/1 - 1"),
    true = allterminated(#state{targets=AllTerminatedList0}),

    autotest:mark(?LINE, "allterminated/1 - 2"),
    true = allterminated(AllTerminatedList0),

    autotest:mark(?LINE, "allterminated/1 - 3"),
    false = allterminated(AllTerminatedList1),

    autotest:mark(?LINE, "allterminated/1 - 4"),
    false = allterminated(AllTerminatedList2),

    autotest:mark(?LINE, "allterminated/1 - 5"),
    AllTerminatedT1_1 = targetlist:get_using_branch("branch1", AllTerminatedList2),
    AllTerminatedT1_2 = targetlist:set_state(AllTerminatedT1_1, terminated),
    AllTerminatedList2_1 = targetlist:update_target(AllTerminatedT1_2, AllTerminatedList2),
    true = allterminated(AllTerminatedList2_1),


    %% test end_processing(EndTime, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "end_processing/2 - 1"),
    false = end_processing(util:timestamp() + 10, #state{mystate = stayalive}),

    autotest:mark(?LINE, "end_processing/2 - 2"),
    true = end_processing(util:timestamp() - 1, #state{mystate = stayalive}),

    autotest:mark(?LINE, "end_processing/2 - 3"),
    true = end_processing(1, #state{mystate = completed}),

    autotest:mark(?LINE, "end_processing/2 - 4"),
    true = end_processing(util:timestamp() + 10, #state{mystate = completed}),

    autotest:mark(?LINE, "end_processing/2 - 5"),
    true = end_processing(util:timestamp() + 10, #state{mystate = completed}),

    autotest:mark(?LINE, "end_processing/2 - 6"),
    EndProcessingList0 = targetlist:empty(),
    EndProcessingList1 = targetlist:add("branch1", #request{}, self(), terminated,
					1, [], none, EndProcessingList0),
    EndProcessingList2 = targetlist:add("branch2", #request{}, self(), calling,
					1, [], none, EndProcessingList1),

    true = end_processing(1, #state{targets = EndProcessingList1}),

    autotest:mark(?LINE, "end_processing/2 - 6"),
    false = end_processing(1, #state{targets = EndProcessingList2}),


    %% test forward_immediately(Method, Status, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "forward_immediately/3 - 1"),
    %% INVITE, =< 100 response - never forwarded
    false = forward_immediately("INVITE", 100, #state{}),

    autotest:mark(?LINE, "forward_immediately/3 - 2"),
    %% INVITE, provisional response
    {true, #state{mystate = calling}} = forward_immediately("INVITE", 101, #state{mystate = calling}),

    autotest:mark(?LINE, "forward_immediately/3 - 3"),
    %% INVITE, provisional response
    {true, #state{mystate = calling}} = forward_immediately("INVITE", 199, #state{mystate = calling}),

    autotest:mark(?LINE, "forward_immediately/3 - 4"),
    %% INVITE, 2xx response always forwarded immediately
    {true, #state{mystate = completed, final_response_sent = true}} =
	forward_immediately("INVITE", 200, #state{mystate = calling, final_response_sent = false}),

    autotest:mark(?LINE, "forward_immediately/3 - 5"),
    %% non-INVITE, response = 2xx and final response not yet sent
    {true, #state{mystate = completed, final_response_sent = true}} =
	forward_immediately("OPTIONS", 200, #state{mystate = calling, final_response_sent = false}),

    autotest:mark(?LINE, "forward_immediately/3 - 6"),
    %% non-INVITE, response =< 299 and final response not yet sent
    {true, #state{mystate = completed, final_response_sent = true}} =
	forward_immediately("OPTIONS", 299, #state{mystate = calling, final_response_sent = false}),

    autotest:mark(?LINE, "forward_immediately/3 - 6"),
    %% non-INVITE or response >= 300, final_response_sent is already 'true'
    false = forward_immediately("OPTIONS", 299, #state{mystate = calling, final_response_sent = true}),


    %% test pick_response(Nxx, Responses)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "pick_response/2 4xx - 0"),
    R400 = #sp_response{status=400},
    %% preferred ones
    R401 = #sp_response{status=401},
    R407 = #sp_response{status=407},
    R415 = #sp_response{status=415},
    R420 = #sp_response{status=420},
    R484 = #sp_response{status=484},
    %% end preferred ones
    R499 = #sp_response{status=499},

    autotest:mark(?LINE, "pick_response/2 4xx - 1"),
    %% test lowest response is returned if no preferred ones
    R400 = pick_response(4, [R499, R400]),

    autotest:mark(?LINE, "pick_response/2 4xx - 2"),
    %% test that preferred responses work
    R401 = pick_response(4, [R499, R401, R400]),
    R407 = pick_response(4, [R499, R407, R400]),
    R415 = pick_response(4, [R499, R415, R400]),
    R420 = pick_response(4, [R499, R420, R400]),
    R484 = pick_response(4, [R499, R484, R400]),

    autotest:mark(?LINE, "pick_response/2 4xx - 3"),
    %% test that equal responses work
    R401 = pick_response(4, [R401, R407, R401]),

    autotest:mark(?LINE, "pick_response/2 4xx - 4"),
    %% test that numerical sorting between preferred responses works as last option
    R401 = pick_response(4, [R400, R401, R407, R415, R420, R484, R499]),

    autotest:mark(?LINE, "pick_response/2 5xx - 0"),
    R500 = #sp_response{status=500},
    R503 = #sp_response{status=503},
    R599 = #sp_response{status=599},

    autotest:mark(?LINE, "pick_response/2 5xx - 1"),
    %% test that we never choose the 503 if there is another response too
    R500 = pick_response(5, [R500, R503]),

    autotest:mark(?LINE, "pick_response/2 5xx - 2"),
    %% test that we never choose the 503 if there is another response too
    R599 = pick_response(5, [R503, R599, R503]),

    autotest:mark(?LINE, "pick_response/2 5xx - 3"),
    %% test that choosing a 503 if we have to works
    R503 = pick_response(5, [R503]),

    autotest:mark(?LINE, "pick_response/2 5xx - 4"),
    %% test 5xx without 503
    R500 = pick_response(5, [R599, R500]),


    %% test aggregate_authreqs(BestResponse, Responses)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "aggregate_authreqs/2 - 0"),
    AuthReq_EmptyH = keylist:from_list([]),
    AuthReq_R400   = #sp_response{status=400, header=AuthReq_EmptyH},
    AuthReq_R401_1 = #sp_response{status=401, header=keylist:set("WWW-Authenticate", ["R401_1"], AuthReq_EmptyH)},
    AuthReq_R401_2 = #sp_response{status=401, header=keylist:set("WWW-Authenticate", ["R401_2"], AuthReq_EmptyH)},
    AuthReq_R401_3 = #sp_response{status=401, header=keylist:set("Proxy-Authenticate", ["broken"], AuthReq_EmptyH)},
    AuthReq_R407_1 = #sp_response{status=407, header=keylist:set("Proxy-Authenticate", ["R407_1"], AuthReq_EmptyH)},
    AuthReq_R407_2 = #sp_response{status=407, header=keylist:set("Proxy-Authenticate", ["R407_2"], AuthReq_EmptyH)},
    AuthReqL = [AuthReq_R400, AuthReq_R401_1, AuthReq_R401_2, AuthReq_R401_3, AuthReq_R407_1, AuthReq_R407_2],

    autotest:mark(?LINE, "aggregate_authreqs/2 - 1.1"),
    %% Aggregate WWW-Authenticate headers, _don't_ copy the erroneous Proxy-Authenticate
    %% header in a 401 response
    Aggr401 = aggregate_authreqs(#sp_response{status=401, header=AuthReq_EmptyH}, AuthReqL),

    autotest:mark(?LINE, "aggregate_authreqs/2 - 1.2"),
    %% verify results
    ["R401_1", "R401_2"] = keylist:fetch('www-authenticate', Aggr401#sp_response.header),
    [] = keylist:fetch('proxy-authenticate', Aggr401#sp_response.header),

    autotest:mark(?LINE, "aggregate_authreqs/2 - 2.1"),
    %% Aggregate Proxy-Authenticate headers
    Aggr407 = aggregate_authreqs(#sp_response{status=407, header=AuthReq_EmptyH}, AuthReqL),

    autotest:mark(?LINE, "aggregate_authreqs/2 - 2.2"),
    %% verify results
    ["R407_1", "R407_2"] = keylist:fetch('proxy-authenticate', Aggr407#sp_response.header),


    %% test make_final_response(Responses)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "make_final_response/1 - 0"),
    FinalRes_R1 = #sp_response{status=100},	%% not a real final response
    FinalRes_R2 = #sp_response{status=200},
    FinalRes_R3 = #sp_response{status=300},
    FinalRes_R4 = #sp_response{status=400},
    FinalRes_R5 = #sp_response{status=599},
    FinalRes_R6 = #sp_response{status=600},

    autotest:mark(?LINE, "make_final_response/1 - 1"),
    %% test pick of 6xx
    FinalRes_R6 = make_final_response([FinalRes_R1, FinalRes_R5, FinalRes_R6, FinalRes_R4, FinalRes_R3, FinalRes_R2]),

    autotest:mark(?LINE, "make_final_response/1 - 2"),
    %% test pick of 2xx
    FinalRes_R2 = make_final_response([FinalRes_R1, FinalRes_R5, FinalRes_R4, FinalRes_R3, FinalRes_R2]),

    autotest:mark(?LINE, "make_final_response/1 - 3"),
    %% test pick of 3xx
    FinalRes_R3 = make_final_response([FinalRes_R1, FinalRes_R5, FinalRes_R3, FinalRes_R4]),

    autotest:mark(?LINE, "make_final_response/1 - 4"),
    %% test pick of 4xx
    FinalRes_R4 = make_final_response([FinalRes_R1, FinalRes_R5, FinalRes_R4]),

    autotest:mark(?LINE, "make_final_response/1 - 5"),
    %% test pick of 5xx
    FinalRes_R5 = make_final_response([FinalRes_R1, FinalRes_R5]),

    autotest:mark(?LINE, "make_final_response/1 - 6"),
    %% test that we don't fall for the 100 response (it is not a final response)
    none = make_final_response([FinalRes_R1]),


    %% test report_upstreams(AllTerminated, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "report_upstreams/2 - 0"),
    UpstreamsTargets1 = targetlist:add("branch1", #request{}, self(), calling,
				       1, [], none, targetlist:empty()),
    UpstreamsTarget_1 = targetlist:get_using_branch("branch1", UpstreamsTargets1),
    UpstreamsState1 = #state{final_response_sent = false,
			     mystate = calling,
			     targets = targetlist:empty(),
			     parent  = self()
			    },

    %%
    %% Test 408 response when no final responses have been received
    %%
    test_report_upstreams("1", UpstreamsState1, {408, "Request Timeout"}),

    %%
    %% Test 500 when there are no final responses
    %%
    %% Create a targetlist with a single 100 response
    UpstreamsTargets2 =
	targetlist:update_target(targetlist:set_endresult(UpstreamsTarget_1, #sp_response{status=100}),
				 UpstreamsTargets1),

    test_report_upstreams("2", UpstreamsState1#state{targets=UpstreamsTargets2}, {500, "No answers collected"}),

    %%
    %% Test 500 when best final response is 503
    %%
    UpstreamsTargets3 =
	targetlist:update_target(targetlist:set_endresult(UpstreamsTarget_1, #sp_response{status=503}),
				 UpstreamsTargets1),

    test_report_upstreams("3", UpstreamsState1#state{targets=UpstreamsTargets3},
			  {500, "Only response to fork was 503"}),

    %%
    %% Test forwarding of created response
    %%
    UpstreamsTargets4 =
	targetlist:update_target(targetlist:set_endresult(UpstreamsTarget_1,
							  #sp_response{created=true, status=499, reason="testing"}),
				 UpstreamsTargets1),

    test_report_upstreams("4", UpstreamsState1#state{targets=UpstreamsTargets4}, {499, "testing"}),

    %%
    %% Test forwarding of received response
    %%
    UpstreamsTargets5 =
	targetlist:update_target(targetlist:set_endresult(UpstreamsTarget_1,
							  #sp_response{created=false, status=499, reason="testing"}),
				 UpstreamsTargets1),

    test_report_upstreams("5", UpstreamsState1#state{targets=UpstreamsTargets5},
			  #response{status=499, reason="testing"}),



    %% test process_branch_result(ClientPid, Branch, NewTState, SPResponse, State)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "process_branch_result/5 - 1"),
    %% test unknown target
    PBR_State1 = #state{targets = targetlist:empty()},
    PBR_SPresponse1 = #sp_response{status  = 599,
				   reason  = "test",
				   created = true
				  },
    PBR_State1 = process_branch_result(self(), "testbranch", test, PBR_SPresponse1, PBR_State1),


    autotest:mark(?LINE, "process_branch_result/5 - 2.1"),
    %% test provisional response
    PBR_Targets2 = targetlist:add("branch1", #request{method = "TEST",
						      uri    = sipurl:parse("sip:ft@192.0.2.222")
						     },
				  self(), calling, 1, [], none, targetlist:empty()),
    PBR_SPresponse2 = #sp_response{status  = 183,
				   reason  = "test",
				   created = false
				  },
    PBR_State2 = #state{final_response_sent = false,
			mystate = calling,
			targets = PBR_Targets2,
			parent  = self(),
			request = #request{method = "TEST",
					   uri    = sipurl:parse("sip:ft@autotest.example.org")
					  }
		       },

    PBR_State2_branchstate = newbranchstate,
    PBR_State2_Res = process_branch_result(self(), "branch1", PBR_State2_branchstate, PBR_SPresponse2, PBR_State2),

    autotest:mark(?LINE, "process_branch_result/5 - 2.2"),
    %% verify that state isn't changed except for the targetlist
    PBR_State2 = PBR_State2_Res#state{targets = PBR_State2#state.targets},

    autotest:mark(?LINE, "process_branch_result/5 - 2.3"),
    %% verify that the target in the targetlist got updated with the new info
    PBR_Target2_1 = targetlist:get_using_branch("branch1", PBR_Targets2),
    %% update branch state and 'final' response
    PBR_Target2_2 = targetlist:set_state(PBR_Target2_1, PBR_State2_branchstate),
    PBR_Target2_3 = targetlist:set_endresult(PBR_Target2_2, PBR_SPresponse2),
    PBR_Target2_3 = targetlist:get_using_branch("branch1", PBR_State2_Res#state.targets),

    autotest:mark(?LINE, "process_branch_result/5 - 2.4"),
    %% check that the 183 was forwarded to parent (us)
    check_we_got_signal({sipproxy_response, Self, "branch1", #response{status = 183, reason = "test"}}),


    %% test check_forward_immediately(Request, SPResponse, Branch, State)
    %%--------------------------------------------------------------------



    %% Spawn pids that collects signals and relays them to us in a way that allows
    %% us to differentiate to which pid the signal was sent

    %% test cancel_pending_if_invite_2xx_or_6xx(Method, Status, Reason, State)
    %%--------------------------------------------------------------------
    CancelPendingRef = make_ref(),
    CancelLine = ?LINE,
    CancelPendingPid1 = spawn_link(fun() -> signal_collector(Me, CancelPendingRef, active, CancelLine + 1) end),
    CancelPendingPid2 = spawn_link(fun() -> signal_collector(Me, CancelPendingRef, active, CancelLine + 2) end),

    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 0"),

    %% one pending and one already completed target
    CancelPendingReq1 = #request{method="INVITE", uri=sipurl:parse("sip:ft@it.su.se")},
    CancelPendingTargets1 = targetlist:add("branch1", CancelPendingReq1, CancelPendingPid1, calling,
					   1, [], none, targetlist:empty()),
    CancelPendingTargets2 = targetlist:add("branch2", CancelPendingReq1, CancelPendingPid2, completed,
					   1, [], none, CancelPendingTargets1),
    CancelPendingState1 = #state{targets = CancelPendingTargets2},

    %%
    %% Test provisional response
    %%
    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 1"),
    %% test provisional response to INVITE, no change expected
    CancelPendingState1 = cancel_pending_if_invite_2xx_or_6xx("INVITE", 180, "Foo", CancelPendingState1),

    %%
    %% Test 2xx response to INVITE
    %%
    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 2.1"),
    CancelPendingState2_out = cancel_pending_if_invite_2xx_or_6xx("INVITE", 200, "Ok", CancelPendingState1),

    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 2.2"),
    %% check results
    true = get_target_value(CancelPendingState2_out, "branch1", cancelled),
    false = get_target_value(CancelPendingState2_out, "branch2", cancelled),
    receive
	{CancelPendingRef, CancelPendingPid1,
	 {'$gen_cast',
	  {cancel, "cancel_targets_state", [{"Reason",
					     ["SIP; cause=200; text=\"Call completed elsewhere\""]}]}
	 }} ->
	    ok;
	{CancelPendingRef, CancelPendingPid2_2, CancelPendingMsg2_2} ->
	    E2_2 = io_lib:format("received unknown signal (from ~p) : ~p",
				 [CancelPendingPid2_2, CancelPendingMsg2_2]),
	    throw({error, lists:flatten(E2_2)})
    after
	1000 ->
	    throw({error, "did not receive the expected cancel signal from the pending target"})
    end,

    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 2.3"),
    %% check that we did NOT cancel the second target, which was already in state 'completed'
    receive
	{CancelPendingRef, CancelPendingPid2,
	 {'$gen_cast', {cancel, "cancel_targets_state", _}}} ->
	    throw({error, "we cancelled more than we should have"});
	{CancelPendingRef, CancelPendingPid2_3, CancelPendingMsg2_3} ->
	    E2_3 = io_lib:format("received unknown signal (from ~p) : ~p",
				 [CancelPendingPid2_3, CancelPendingMsg2_3]),
	    throw({error, lists:flatten(E2_3)})
    after
	0 ->
	    ok
    end,

    %%
    %% Test non-6xx response to non-INVITE
    %%
    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 3"),
    %% test non-6xx response to non-INVITE, no change expected
    CancelPendingState1 = cancel_pending_if_invite_2xx_or_6xx("OPTIONS", 599, "Foo", CancelPendingState1),

    %%
    %% Test 6xx reponse
    %%
    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 4.1"),
    #state{} = cancel_pending_if_invite_2xx_or_6xx("OPTIONS", 603, "Decline", CancelPendingState1),

    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 4.2"),
    %% check results
    receive
	{CancelPendingRef, CancelPendingPid1,
	 {'$gen_cast',
	  {cancel, "cancel_targets_state", [{"Reason",
					     ["SIP; cause=603; text=\"Decline\""]}]}
	 }} ->
	    ok;
	{CancelPendingRef, CancelPendingPid4_2, CancelPendingMsg4_2} ->
	    E4_2 = io_lib:format("received unknown signal (from ~p) : ~p",
				 [CancelPendingPid4_2, CancelPendingMsg4_2]),
	    throw({error, lists:flatten(E4_2)})
    after
	1000 ->
	    throw({error, "did not receive the expected cancel signal from the pending target"})
    end,

    autotest:mark(?LINE, "cancel_pending_if_invite_2xx_or_6xx/4 - 4.3"),
    %% check that we did NOT cancel the second target, which was already in state 'completed'
    receive
	{CancelPendingRef, CancelPendingPid2,
	 {'$gen_cast', {cancel, "cancel_targets_state", _}}} ->
	    throw({error, "we cancelled more than we should have"});
	{CancelPendingRef, CancelPendingPid4_3, CancelPendingMsg4_3} ->
	    E4_3 = io_lib:format("received unknown signal (from ~p) : ~p",
				 [CancelPendingPid4_3, CancelPendingMsg4_3]),
	    throw({error, lists:flatten(E4_3)})
    after
	0 ->
	    ok
    end,

    CancelPendingPid1 ! {quit, self()},
    CancelPendingPid2 ! {quit, self()},


    %% test process_wait(EndTime, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_wait/2 - 0"),
    ProcessWaitState1 = #state{targets = targetlist:empty(),
			       mystate = completed
			      },
    Self ! {showtargets},

    autotest:mark(?LINE, "process_wait/2 - 1"),
    {discontinue, ProcessWaitState1} = process_wait(util:timestamp(), ProcessWaitState1),


    %% test process_signal({cancel_pending, ExtraHeaders}, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_signal/2 {cancel_pending,...} - 0"),
    PSCancelPendingRef = make_ref(),
    PSCancelLine = ?LINE,
    PSSigFun = fun() ->
		       signal_collector(Me, PSCancelPendingRef, passive, PSCancelLine)
	       end,
    PSCancelPendingPid1 = spawn_link(PSSigFun),
    PSCancelPendingPid2 = spawn_link(PSSigFun),

    %% one pending and one already completed target
    PSCancelPendingReq1 = #request{method = "INVITE", uri = sipurl:parse("sip:ft@test.example.org")},
    PSCancelPendingTargets1 = targetlist:add("branch1", PSCancelPendingReq1, PSCancelPendingPid1, calling,
					     1, [], none, targetlist:empty()),
    PSCancelPendingTargets2 = targetlist:add("branch2", PSCancelPendingReq1, PSCancelPendingPid2, completed,
					     1, [], none, PSCancelPendingTargets1),
    PSCancelPendingState1 = #state{targets = PSCancelPendingTargets2, mystate = calling},

    autotest:mark(?LINE, "process_signal/2 {cancel_pending,...} - 1.1"),
    %% test normal case, mystate = calling
    {ok, PSCancelPendingState1_out} = process_signal({cancel_pending, [{"Reason", ["test"]}]}, PSCancelPendingState1),

    autotest:mark(?LINE, "process_signal/2 {cancel_pending,...} - 1.2"),
    %% verify results
    cancelled = PSCancelPendingState1_out#state.mystate,
    true = get_target_value(PSCancelPendingState1_out, "branch1", cancelled),
    false = get_target_value(PSCancelPendingState1_out, "branch2", cancelled),
    autotest:mark(?LINE, "process_signal/2 {cancel_pending,...} - 1.3"),
    poll_signal_collector(PSCancelPendingRef, PSCancelPendingPid1,
			  [{'$gen_cast', {cancel, "cancel_targets_state", [{"Reason", ["test"]}]}}]
			 ),
    autotest:mark(?LINE, "process_signal/2 {cancel_pending,...} - 1.4"),
    poll_signal_collector(PSCancelPendingRef, PSCancelPendingPid2, []),

    autotest:mark(?LINE, "process_signal/2 {cancel_pending,...} - 2.1"),
    %% test normal case, mystate = completed
    PSCancelPendingState2 = #state{targets = PSCancelPendingTargets1, mystate = completed},
    {ok, PSCancelPendingState2_out} = process_signal({cancel_pending, []}, PSCancelPendingState2),

    autotest:mark(?LINE, "process_signal/2 {cancel_pending,...} - 2.2"),
    %% verify results
    completed = PSCancelPendingState2_out#state.mystate,
    poll_signal_collector(PSCancelPendingRef, PSCancelPendingPid1,
			  [{'$gen_cast', {cancel, "cancel_targets_state", []}}]
			 ),
    true = get_target_value(PSCancelPendingState2_out, "branch1", cancelled),

    PSCancelPendingPid1 ! {quit, self()},
    PSCancelPendingPid2 ! {quit, self()},


    %% test process_signal({clienttransaction_terminating, ClientPid, Branch}, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_signal/2 {clienttransaction_terminating,...} - 0"),
    PSClientTermRef = make_ref(),
    PSClientCancelLine = ?LINE,
    PSClientTermPid1 = spawn_link(fun() -> signal_collector(Me, PSClientTermRef, passive, PSClientCancelLine + 1) end),

    PSClientTermReq1 = #request{method = "INVITE", uri = sipurl:parse("sip:ft@test.example.org")},
    PSClientTermTargets1 = targetlist:add("branch1", PSClientTermReq1, PSClientTermPid1, calling,
					     1, [], none, targetlist:empty()),
    PSClientTermState1 = #state{targets = PSClientTermTargets1},

    autotest:mark(?LINE, "process_signal/2 {clienttransaction_terminating,...} - 1.1"),
    %% test normal case
    {ok, PSClientTermState1_out} = process_signal({clienttransaction_terminating, PSClientTermPid1,
						   "branch1"}, PSClientTermState1),

    autotest:mark(?LINE, "process_signal/2 {clienttransaction_terminating,...} - 1.2"),
    %% verify result
    terminated = get_target_value(PSClientTermState1_out, "branch1", state),

    autotest:mark(?LINE, "process_signal/2 {clienttransaction_terminating,...} - 2"),
    %% test with invalid pid, expected to crash
    {'EXIT', {{badmatch, _}, _}} = (catch process_signal({clienttransaction_terminating,
							   self(), "branch1"}, PSClientTermState1)),

    autotest:mark(?LINE, "process_signal/2 {clienttransaction_terminating,...} - 3"),
    %% test with unknown branch, no change expected
    {ok, PSClientTermState1} =
	process_signal({clienttransaction_terminating, self(), "unknownbranch"}, PSClientTermState1),

    %% clean up
    PSClientTermPid1 ! {quit, self()},

    ok.

test_report_upstreams(Num, State, Expected) when is_record(State, state) ->
    UpstreamsMyself = self(),

    autotest:mark(?LINE, "report_upstreams/2 - ~s.1", [Num]),
    #state{mystate=completed, final_response_sent=true} = report_upstreams(true, State),

    autotest:mark(?LINE, "report_upstreams/2 - ~s.2", [Num]),
    %% Check result
    receive
	{sipproxy_all_terminated, UpstreamsMyself, Expected} ->
	    ok
    after 1000 ->
	    throw({error, "test: did not receive the expected response"})
    end.

%% signal collector
signal_collector(Parent, Ref, Mode, Id) ->
    erlang:monitor(process, Parent),
    signal_collector(Parent, Ref, Mode, Id, []).

signal_collector(Parent, Ref, Mode, Id, Buf) ->
    receive
	{quit, Parent} ->
	    ok;
	{flush, Parent} when Mode == passive ->
	    %% send stored messages to parent
	    Parent ! {Ref, self(), flush, lists:reverse(Buf)},
	    signal_collector(Parent, Ref, Mode, Id, []);
	{'DOWN', _MRef, process, Parent, _Info} ->
	    ok;
	Msg ->
	    case Mode of
		active ->
		    %% Relay message to parent, with a reference and our pid as tags
		    Parent ! {Ref, self(), Msg},
		    signal_collector(Parent, Ref, Mode, Id, []);
		passive ->
		    %% store message
		    signal_collector(Parent, Ref, Mode, Id, [Msg | Buf])
	    end
    after 10000 ->
	    Msg = io_lib:format("signal collector (id ~p) timed out", [Id]),
	    erlang:error(lists:flatten(Msg))
    end.

poll_signal_collector(Ref, Pid, Expected) ->
    Pid ! {flush, self()},
    receive
	{Ref, Pid, flush, Signals} ->
	    poll_signal_collector2(Signals, Expected, 0)
    after 1000 ->
	    throw({error, "test: signal collector does not respond"})
    end,
    ok.

poll_signal_collector2([S1 | ST], [E1 | ET], N) when S1 == E1 ->
    poll_signal_collector2(ST, ET, N + 1);
poll_signal_collector2([S1 | _ST], [E1 | _ET], N) ->
    Msg = io_lib:format("test: signal collector signal ~p does not match~n(~p, expected ~p)~n",
			[N, S1, E1]),
    throw({error, lists:flatten(Msg)});
poll_signal_collector2([], [], _N) ->
    ok;
poll_signal_collector2(S, E, _N) ->
    Msg = io_lib:format("test: signal collector signals does not match~n(signals left: ~p, expected :~p)~n",
			[S, E]),
    throw({error, lists:flatten(Msg)}).

check_we_got_signal(Signal) ->
    receive
	Signal ->
	    ok
    after 1000 ->
	    Msg = io_lib:format("error: test did not result in a ~p signal", [Signal]),
	    erlang:error(lists:flatten(Msg))
    end.

get_target_value(State, Branch, Key) when is_record(State, state), is_list(Branch), is_atom(Key) ->
    case targetlist:get_using_branch(Branch, State#state.targets) of
	none ->
	    erlang:error("Could not find target with branch " ++ Branch);
	Target ->
	    [Value] = targetlist:extract([Key], Target),
	    Value
    end.
