%%%-------------------------------------------------------------------
%%% File    : sipproxy.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: Find a working destination for a request, given a number
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
%%%              FinalResponse = Response | {Status, Reason}
%%%              Pid      = pid() of sipproxy process
%%%              Branch   = string(), the branch used by the client
%%%                         transaction that received/generated a
%%%                         response
%%%              Status   = integer(), SIP status code
%%%              Reason   = string(), SIP reason phrase
%%%
%%%           If you want to terminate a running sipproxy (for example
%%%           if the server transaction has been cancelled), send it
%%%           an {cancel_pending} signal.
%%%
%%% Created : 13 Feb 2003 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(sipproxy).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/5,
	 start_actions/4
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipproxy.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  parent,			%% pid(), parent process - used to trap EXITs
	  branchbase,			%% string(), a base for us to use when creating client transaction branches.
					%% Must be unique!
	  request,			%% request record(), the request we are working on
	  actions,			%% list() of sipproxy_action record() - our list of actions
	  targets,			%% targetlist record(), list of all our targets (ongoing or finished).
	  				%% Targets are client transactions.
	  final_response_sent=false,	%% true | false, have we forwarded a final response yet?
	  mystate=calling,		%% calling | cancelled | completed | stayalive
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
%% Function: start_actions(BranchBase, GluePid, OrigRequest, Actions)
%%           BranchBase = string(), the "base" part of the server
%%                        transactions branch - so that we can get
%%                        sipproxy to generate intuitive branches for
%%                        it's corresponding client transactions
%%           GluePid = pid(), the pid to which sipproxy should report
%%           OrigRequest = request record()
%%           Actions = list() of sipproxy_action record()
%% Descrip.: This function is spawned by the appserver glue process
%%           and executes sipproxy:start() in this new thread.
%% Returns : ok | error
%%--------------------------------------------------------------------
start_actions(BranchBase, GluePid, OrigRequest, Actions) when is_record(OrigRequest, request) ->
    {Method, URI} = {OrigRequest#request.method, OrigRequest#request.uri},
    Timeout = 32,	%% wait at the end of actions-list timeout
    %% We don't return from sipproxy:start() until all Actions are done, and sipproxy signals GluePid
    %% when it is done.
    case sipproxy:start(BranchBase, GluePid, OrigRequest, Actions, Timeout) of
	ok ->
	    logger:log(debug, "sipproxy: fork of request ~s ~s done, start_actions() returning", [Method, sipurl:print(URI)]),
	    ok;
	{error, What} ->
	    logger:log(error, "sipproxy: fork of request ~s ~s failed : ~p", [Method, sipurl:print(URI), What]),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: start(BranchBase, Parent, Request, Actions, Timeout)
%%           BranchBase = string()
%%           Parent     = pid()
%%           Request    = request record()
%%           Actions    = list() of sipproxy_action record()
%%           Timeout    = integer()
%% Descrip.: Start the processing, currently forking (in parallell or
%%           sequentially) of Request according to Actions.
%% Returns : ok              |
%%           {error, Reason}
%%           Reason = string()
%% Note    : Actions is a set of actions to perform. Currently
%%           supported actions are a list of 'call' and 'wait'. You
%%           can mix calls and waits freely.
%%--------------------------------------------------------------------
start(BranchBase, Parent, Request, Actions, Timeout)
  when is_list(BranchBase), is_pid(Parent), is_record(Request, request), is_list(Actions), is_integer(Timeout) ->
    %% We need to trap exits so that we can handle client transcations failing, and
    %% cancel all client transactions if the parent crashes.
    process_flag(trap_exit, true),
    Res = case catch siprequest:check_proxy_request(Request) of
	      {ok, NewHeader, ApproxMsgSize} ->
		  %% sipproxy should never be invoked on a request that contains
		  %% a Route header, but we check just in case someone screws up.
		  case keylist:fetch('route', Request#request.header) of
		      [] ->
			  EndTime = util:timestamp() + Timeout,
			  State = #state{parent=Parent, branchbase=BranchBase, timeout=Timeout,
					 request=Request#request{header=NewHeader},
					 actions=Actions, targets=targetlist:empty(),
					 approx_msgsize=ApproxMsgSize},
			  fork(EndTime, State),
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
	      Unknown ->
		  logger:log(debug, "sipproxy: check_proxy_request returned unknown result :~n~p", [Unknown]),
		  InternalError = {500, "Server Internal Error"},
		  Parent ! {sipproxy_all_terminated, self(), InternalError},
		  {error, "Request could not be forked"}
	  end,
    %% Always signal Parent that we are terminating
    util:safe_signal("sipproxy: ", Parent, {sipproxy_terminating, self()}),
    Res.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: fork(State)
%%           State = state record()
%% Descrip.: Main loop. Process actions until there are none left.
%% Returns : ok
%%--------------------------------------------------------------------
fork(EndTime, State) when is_integer(EndTime), is_record(State, state), State#state.actions == [] ->
    %% No actions left
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
		NewTargets = cancel_pending_targets(Targets),

		State#state.parent ! {sipproxy_no_more_actions, self()},

		logger:log(debug, "sipproxy: waiting ~p seconds for any final responses", [State#state.timeout]),
		NewEndTime = util:timestamp() + State#state.timeout,
		NewState1 = case process_wait(false, NewEndTime, State#state{mystate=stayalive, targets=NewTargets}) of
				{discontinue, NewState1_1} when is_record(NewState1_1, state) -> NewState1_1;
				NewState1_1 when is_record(NewState1_1, state) -> NewState1_1
			    end,
		logger:log(debug, "sipproxy: final responses wait is over, exiting."),
		NewState1
	end,

    %% Check that this set of actions actually resulted in a final response. Actions can be empty, or
    %% contain only waits or something equally stupid (the stupidity lies in the hand of the end user
    %% with CPL scripts ;) ).
    case NewState#state.final_response_sent of
	false ->
	    logger:log(debug, "sipproxy: No final response sent to parent for this set of actions, "
		       "generating a '500 Server Internal Error'"),
	    InternalError = {500, "Server Internal Error"},
	    NewState#state.parent ! {sipproxy_all_terminated, self(), InternalError};
	_ -> ok
    end,
    ok;
fork(EndTime, State) when is_integer(EndTime), is_record(State, state) ->
    %% Process first action in list
    OrigRequest = State#state.request,
    [HAction | TAction] = State#state.actions,
    Method = OrigRequest#request.method,
    Targets = State#state.targets,
    case HAction#sipproxy_action.action of
	call ->
	    CallURI = HAction#sipproxy_action.requri,
	    CallTimeout = HAction#sipproxy_action.timeout,
	    logger:log(debug, "sipproxy: forking ~s request to ~s with timeout ~p",
		       [Method, sipurl:print(CallURI), CallTimeout]),
	    %% Create a new branch for this target (client transaction). BranchBase plus a sequence number is unique.
	    Branch = State#state.branchbase ++ "-UAC" ++ integer_to_list(targetlist:get_length(Targets) + 1),
	    Request = OrigRequest#request{uri=CallURI},
	    DstList = sipdst:url_to_dstlist(CallURI, State#state.approx_msgsize, CallURI),
	    [FirstDst|_] = DstList,
	    NewTargets = case transactionlayer:start_client_transaction(Request, none, FirstDst, Branch,
									CallTimeout, self()) of
			     BranchPid when is_pid(BranchPid) ->
				 targetlist:add(Branch, Request, BranchPid, calling, CallTimeout, DstList, Targets);
			     {error, E} ->
				 logger:log(error, "sipproxy: Failed starting client transaction : ~p", [E]),
				 Targets
			 end,
	    fork(EndTime, State#state{actions=TAction, targets=NewTargets});
	wait ->
	    Time = HAction#sipproxy_action.timeout,
	    logger:log(debug, "sipproxy: waiting ~p seconds", [Time]),
	    NewEndTime = util:timestamp() + Time,
	    case process_wait(false, NewEndTime, State) of
		{discontinue, NewState} ->
		    logger:log(debug, "sipproxy: discontinue processing"),
		    fork(NewEndTime, NewState#state{actions=[]});
		NewTargets ->
		    fork(NewEndTime, State#state{actions=TAction, targets=NewTargets})
	    end
    end.

%%--------------------------------------------------------------------
%% Function: cancel_pending_targets(Targets)
%%           Targets = targetlist record()
%% Descrip.: Cancel all targets in state 'proceeding'.
%% Returns : NewTargets = targetlist record()
%%--------------------------------------------------------------------
cancel_pending_targets(Targets) ->
    %% send {cancel} to all PIDs in states other than completed and terminated
    NewTargets1 = cancel_targets_state(Targets, calling),
    cancel_targets_state(NewTargets1, proceeding).

%%--------------------------------------------------------------------
%% Function: cancel_targets_state(Targets, TargetState)
%%           Targets     = targetlist record()
%%           TargetState = atom()
%% Descrip.: Cancel all targets in state TargetState.
%% Returns : NewTargets = targetlist record()
%%--------------------------------------------------------------------
cancel_targets_state(Targets, TargetState) when is_atom(TargetState) ->
    %% send {cancel} to all PIDs in a specific State, return updated TargetList
    TargetsInState = targetlist:get_targets_in_state(TargetState, Targets),
    lists:map(fun(ThisTarget) ->
		      [Pid] = targetlist:extract([pid], ThisTarget),
		      gen_server:cast(Pid, {cancel, "cancel_targets_state", []})
	      end, TargetsInState),
    NewTargets = mark_cancelled(TargetsInState, Targets),
    NewTargets.

%%--------------------------------------------------------------------
%% Function: mark_cancelled(Targets, TargetList)
%%           Targets    = list() of term()
%%           TargetList = targetlist record()
%% Descrip.: Marks a number of targets (Targets) as cancelled.
%% Returns : NewTargets = targetlist record()
%%--------------------------------------------------------------------
mark_cancelled([], TargetList) ->
    TargetList;
mark_cancelled([H | T], TargetList) ->
    NewTarget = targetlist:set_cancelled(H, true),
    NewTargetList = targetlist:update_target(NewTarget, TargetList),
    mark_cancelled(T, NewTargetList).

%%--------------------------------------------------------------------
%% Function: process_wait(EndProcessing, State)
%%           EndProcessing  = atom(), true | false
%%           State          = state record()
%% Descrip.: Marks a number of targets (Targets) as cancelled.
%% Returns : {discontinue, NewState}
%%           NewState = state record()
%%--------------------------------------------------------------------
process_wait(true, _EndTime, State) when is_record(State, state) ->
    Targets = State#state.targets,
    logger:log(debug, "sipproxy: All Targets terminated or completed. Ending process_wait(), returning discontinue. "
	       "debugfriendly(TargetList) :~n~p", [targetlist:debugfriendly(Targets)]),
    {discontinue, State};
process_wait(false, EndTime, State) when is_record(State, state) ->
    Time = lists:max([EndTime - util:timestamp(), 0]),
    {Res, NewState} =
	receive
	    Msg ->
		process_signal(Msg, State)
	after
	    Time * 1000 ->
		logger:log(debug, "sipproxy: This wait's time (~p seconds) is over", [Time]),
		{quit, State}
	end,
    case Res of
	quit ->
	    State#state.targets;
	_ ->
	    AllTerminated = allterminated(NewState),
	    NewState_1 = report_upstreams(AllTerminated, NewState),
	    TimeLeft = lists:max([EndTime - util:timestamp(), 0]),
	    EndProcessing = end_processing(EndTime, NewState_1),
	    logger:log(debug, "sipproxy: State changer, (old) MyState=~p, NewMyState=~p, TimeLeft=~p, "
		       "FinalResponseSent=~p, AllTerminated=~p, EndProcessing=~p",
		       [State#state.mystate, NewState_1#state.mystate, TimeLeft,
			NewState_1#state.final_response_sent, AllTerminated, EndProcessing]),
	    process_wait(EndProcessing, EndTime, NewState_1)
    end.

%%--------------------------------------------------------------------
%% Function: process_signal({cancel_pending}, State)
%%           State          = state record()
%% Descrip.: We are asked to terminate all pending targets.
%% Returns : {ok, NewState}
%%           NewState = state record()
%%--------------------------------------------------------------------
process_signal({cancel_pending}, State) when is_record(State, state) ->
    Targets = State#state.targets,
    logger:log(debug, "sipproxy: Received 'cancel_pending', calling cancel_pending_targets()"),
    NewTargets = cancel_pending_targets(Targets),
    %% If mystate is 'calling', we need to set it to 'cancelled' to prevent us from
    %% starting new transactions for additional destinations if we were to receive
    %% a 503 response.
    NewMyState = case State#state.mystate of
		     calling -> cancelled;
		     S -> S
		 end,
    NewState = State#state{targets=NewTargets, mystate=NewMyState},
    {ok, NewState};

%%--------------------------------------------------------------------
%% Function: process_signal({branch_result, ClientPid, Branch,
%%                           NewTState, Response}, State)
%%           ClientPid = pid()
%%           Branch    = string()
%%           NewTState = term(), new client transaction state
%%           Response  = response record() | {Status, Reason}
%%             Status  = integer(), SIP status code
%%             Reason  = string(), SIP reason phrase
%%           State     = state record()
%% Descrip.: One of our client transactions reports something. Either
%%           it has received a response, timed out or been told to
%%           report something by someone.
%% Returns : {ok, NewState}
%%           NewState = state record()
%%--------------------------------------------------------------------
process_signal({branch_result, ClientPid, Branch, NewTState, Response}, State)
  when is_pid(ClientPid), is_record(State, state) ->
    %% serialize the response we receive
    SPResponse =
	case Response of
	    _ when is_record(Response, response) ->
		#sp_response{status = Response#response.status, reason = Response#response.reason,
			     header = Response#response.header, body = Response#response.body,
			     created = false};
	    {Status, Reason} when is_integer(Status), is_list(Reason) ->
		#sp_response{status = Status, reason = Reason, header = keylist:from_list([]),
			     body = <<>>, created = true}
	end,
    NewState = process_branch_result(ClientPid, Branch, NewTState, SPResponse, State),
    {ok, NewState};

%%--------------------------------------------------------------------
%% Function: process_signal({clienttransaction_terminating,
%%                           ClientPid}, State)
%%           ClientPid = pid(), pid of client transaction
%%           State     = state record()
%% Descrip.: One of our client transactions reports that it is
%%           terminating.
%% Returns : {ok, NewState}
%%           NewState = state record()
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
		State#state{targets=NewTargets}
	end,
    {ok, NewState};

%%--------------------------------------------------------------------
%% Function: process_signal({showtargets}, State)
%%           State     = state record()
%% Descrip.: Someone wants us to log our current set of targets to the
%%           debug log.
%% Returns : {ok, State}
%%--------------------------------------------------------------------
process_signal({showtargets}, State) when is_record(State, state) ->
    logger:log(debug, "sipproxy: Received 'showtargets' request, debugfriendly(TargetList) :~n~p",
	       [targetlist:debugfriendly(State#state.targets)]),
    {ok, State};

%%--------------------------------------------------------------------
%% Function: process_signal({'EXIT', Parent, Reason}, State)
%%           Parent = pid()
%%           Reason = term()
%% Descrip.: If Pid matches our parent (State#state.parent), this
%%           function will match. We log the exit reason and then
%%           exit (hard) ourselves. This will cause the EXIT signal to
%%           be propagated to our client branches, which will CANCEL
%%           themselves if they are not already completed.
%% Returns : {quit, State} |
%%           does not return
%%--------------------------------------------------------------------
%%
%% final_response_sent == 'true' or mystate /= 'calling'
%%
process_signal({'EXIT', Pid, normal}, #state{parent=Parent, final_response_sent=FRS,
					    mystate=MyState}=State)
  when Pid == Parent, FRS == true; MyState /= calling ->
    %% Parent exited when we are finishing up. This is considered normal, so we just exit too.
    {quit, State};

%%
%% final_response_sent == 'false', or mystate is 'calling'
%%
process_signal({'EXIT', Pid, Reason}, #state{parent=Parent}) when Pid == Parent ->
    %% Our parent has exited on us, either abnormally or before we sent a final response.
    %% Exit straight away since there is no point in us staying alive. The client branches
    %% are linked to this process, and will cancel themselves when we exit, if they need to.
    logger:log(error, "sipproxy: My parent just exited, so I will too (with error 'sipproxy_parent_died')."),
    logger:log(debug, "sipproxy: Parents (~p) exit-reason : ~p", [Parent, Reason]),
    erlang:exit(sipproxy_parent_died);

%%--------------------------------------------------------------------
%% Function: handle_info({'EXIT', Pid, Reason}, State)
%%           Pid    = pid()
%%           Reason = term()
%% Descrip.: Some other process than our parent has exited. Check if
%%           it was one of our client transaction. If it was,
%%           determine what it's final result should be (500 if the
%%           client transaction did not throw a siperror), and store
%%           that in our response context (target list).
%% Note    : Ideally, we should start a CANCEL transaction if it was
%%           an invite, that wasn't completed or terminated, that
%%           exited. We would need to know a few things more than we
%%           know today though - such as what branch the transport
%%           layer used for the INVITE etc.
%%--------------------------------------------------------------------
process_signal({'EXIT', Pid, Reason}, State) when is_record(State, state) ->
    Targets = State#state.targets,
    case targetlist:get_using_pid(Pid, Targets) of
	none ->
	    logger:log(error, "sipproxy: Got EXIT signal from unknown pid ~p, reason: ~p",
		       [Pid, Reason]),
	    {error, State};
	ThisTarget ->
	    logger:log(debug, "FREDRIK: SIPPROXY RECEIVED EXIT SIGNAL FROM BRANCH PID ~p", [Pid]),
	    NewTarget1 =
		case Reason of
		    normal ->
			logger:log(debug, "sipproxy: Branch with pid ~p exited normally", [Pid]),
			targetlist:set_state(ThisTarget, terminated);
		    {_, {siperror, Status, SipReason}} ->
			logger:log(error, "sipproxy: Branch with pid ~p FAILED : ~p ~s",
				  [Pid, Status, SipReason]),
			SPR = #sp_response{status=Status, reason=SipReason, created=true},
			targetlist:set_endresult(ThisTarget, SPR);
		    {_, {siperror, Status, SipReason, ExtraHeaders}} ->
			%% We currently have no means to do anything intelligent with
			%% extra headers here. XXX.
			logger:log(error, "sipproxy: Branch with pid ~p FAILED : ~p ~s"
				   " (discarding extra headers : ~p)",
				  [Pid, Status, SipReason, ExtraHeaders]),
			SPR = #sp_response{status=Status, reason=SipReason, created=true},
			targetlist:set_endresult(ThisTarget, SPR);
		    _ ->
			logger:log(error, "sipproxy: Branch with pid ~p exited ABNORMALLY:~n~p",
				   [Pid, Reason]),
			SPR = #sp_response{status=500, reason="Server Internal Error", created=true},
			targetlist:set_endresult(ThisTarget, SPR)
		    end,
	    NewTarget = targetlist:set_state(NewTarget1, terminated),
	    NewTargets = targetlist:update_target(NewTarget, Targets),
	    {ok, State#state{targets=NewTargets}}
    end;

process_signal(Msg, State) when is_record(State, state) ->
    logger:log(error, "sipproxy: Received unknown message ~p, ignoring", [Msg]),
    {error, State}.

%%--------------------------------------------------------------------
%% Function: try_next_destination(Response, Target, Targets, State)
%%           Response = response record()
%%           Target   =
%%           Targets  =
%%           State    = state record()
%% Descrip.: Examine a response we have received, and if it is a 503
%%           (we fake receiving 503's when the transport layer
%%           indicates failure), we start a new client transaction for
%%           the next destination in this targets destination list.
%%           The targets destination list is typically a list of
%%           destinations derived from a URI.
%% Returns : NewTargets = targetlist record()
%%--------------------------------------------------------------------
try_next_destination(Status, Target, Targets, State) when is_integer(Status), is_record(State, state) ->
    case {Status, State#state.mystate} of
	{503, calling} ->
	    %% The first entry in dstlist is the one we have just finished with,
	    %% not the next one to try.
	    case targetlist:extract([dstlist], Target) of
		[[_FirstDst]] ->
		    logger:log(debug, "sipproxy: Received response ~p, but there are no more destinations to try "
			       "for this target", [Status]),
		    Targets;
		[[_FailedDst | DstList]] ->
		    [Branch] = targetlist:extract([branch], Target),
		    NewBranch = get_next_target_branch(Branch),
		    logger:log(debug, "sipproxy: Received response ~p, starting new branch ~p for next destination",
			       [Status, NewBranch]),
		    [Request, Timeout] = targetlist:extract([request, timeout], Target),
		    [FirstDst | _] = DstList,
		    %% XXX ideally we should not try to contact the same host over UDP, when
		    %% we receive a transport layer error for TCP, if there were any other
		    %% equally preferred hosts in the SRV response for a destination.
		    %% It makes more sense to try to connect to Proxy B over TCP/UDP than to
		    %% try Proxy A over UDP when Proxy A over TCP has just failed.
		    NewTargets =
			case transactionlayer:start_client_transaction(Request, none, FirstDst, NewBranch,
								       Timeout, self()) of
			    BranchPid when is_pid(BranchPid) ->
				targetlist:add(NewBranch, Request, BranchPid, calling, Timeout, DstList, Targets);
			    {error, E} ->
				logger:log(error, "sipproxy: Failed starting client transaction : ~p", [E]),
				Targets
			end,
		    NewTargets
	    end;
	{503, S} ->
	    logger:log(debug, "sipproxy: Received response 503 but not trying next destination when I'm in state ~p",
		       [S]),
	    Targets;
	_ ->
	    Targets
    end.

%%--------------------------------------------------------------------
%% Function: get_next_target_branch(In)
%%           In = string()
%% Descrip.: Create the next branch given the current one. Basically
%%           we increase the number at the end, or add ".1" if there
%%           is no .number at the end.
%% Returns : Out = string()
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
%% Function: allterminated(State)
%%           allterminated(TargetList)
%%           State      = state record()
%%           TargetList = targetlist record()
%% Descrip.: Determine if all targets are terminated (that is, no
%%           targets are in the states calling, trying or proceeding).
%% Returns : true | false
%%--------------------------------------------------------------------
allterminated(State) when is_record(State, state) ->
    allterminated(State#state.targets);

allterminated(TargetList) ->
    TargetsCalling = targetlist:get_targets_in_state(calling, TargetList),
    TargetsTrying = targetlist:get_targets_in_state(trying, TargetList),
    TargetsProceeding = targetlist:get_targets_in_state(proceeding, TargetList),
    %%TargetsCompleted = targetlist:get_targets_in_state(completed, TargetList),
    if
	TargetsCalling /= [] -> false;
	TargetsTrying /= [] -> false;
	TargetsProceeding /= [] -> false;
	%%TargetsCompleted /= [] -> false;
	true -> true
    end.

%%--------------------------------------------------------------------
%% Function: end_processing(State)
%%           State = state record()
%% Descrip.: Determine if we should end processing or not (by looking
%%           at the endtime element of our State record).
%% Returns : true | false
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
%% Function: report_upstreams(AllTerminated, State)
%%           AllTerminated = true | false
%%           State         = state record()
%% Descrip.: If AllTerminated is true, this function decides what
%%           final response we should report upstreams.
%% Returns : NewState = state record()
%% Note    : We need to forward 2xx responses to INVITE to upstream
%%           even if we are 'cancelled'.
%%--------------------------------------------------------------------
%%
%% AllTerminated == true, final_response_sent == false, mystate == calling or cancelled
%%
report_upstreams(true, #state{final_response_sent=false, mystate=MyState}=State)
  when MyState == calling; MyState == cancelled ->
    Responses = targetlist:get_responses(State#state.targets),
    logger:log(debug, "sipproxy: All targets are terminated, evaluating responses :~n~p",
	       [printable_responses(Responses)]),
    ForwardResponse =
	case Responses of
	    [] ->
		logger:log(normal, "sipproxy: No responses to choose between, answering 408 Request Timeout"),
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
				   "'500 Only response to fork was 503'", [R503#response.reason]),
			{500, "Only response to fork was 503"};
		    #sp_response{created=false}=SP_FR ->
			{Status, Reason, Header, Body} = {SP_FR#sp_response.status, SP_FR#sp_response.reason,
							  SP_FR#sp_response.header, SP_FR#sp_response.body},
			logger:log(debug, "sipproxy: Picked best response '~p ~s' (received)", [Status, Reason]),
			#response{status=Status, reason=Reason, header=Header, body=Body};
		    SP_FR when is_record(SP_FR, sp_response) ->
			{Status, Reason} = {SP_FR#sp_response.status, SP_FR#sp_response.reason},
			logger:log(debug, "sipproxy: Picked best response '~p ~s' (created)", [Status, Reason]),
			{Status, Reason}
		end
	end,
    Parent = State#state.parent,
    logger:log(debug, "sipproxy: Sending the result to parent Pid ~p.", [Parent]),
    %% ForwardResponse is either a response record() or a {Status, Reason} tuple
    util:safe_signal("sipproxy: ", Parent, {sipproxy_all_terminated, self(), ForwardResponse}),
    State#state{mystate=completed, final_response_sent=true};
%%
%% AllTerminated = false, final_response_sent == true, or mystate is not calling or cancelled
%%
report_upstreams(_AllTerminated, State) when is_record(State, state) ->
    State.

process_branch_result(ClientPid, Branch, NewTState, SPResponse, State) when is_pid(ClientPid), is_list(Branch),
									    is_record(SPResponse, sp_response),
									    is_record(State, state) ->
    Targets = State#state.targets,
    {Status, Reason} = {SPResponse#sp_response.status, SPResponse#sp_response.reason},
    case targetlist:get_using_branch(Branch, Targets) of
	none ->
	    logger:log(error, "sipproxy: Received branch result '~p ~s' from an unknown "
		       "Target (pid ~p, branch ~p), ignoring.", [Status, Reason, ClientPid, Branch]),
	    State;
	ThisTarget ->
	    %% By matching on ClientPid here, we make sure we got the signal from
	    %% the right process. XXX handle wrong pid more gracefully than crashing!
	    [ClientPid, ResponseToRequest, OldTState] = targetlist:extract([pid, request, state], ThisTarget),
	    {RMethod, RURI} = {ResponseToRequest#request.method, ResponseToRequest#request.uri},
	    NewTarget1 = targetlist:set_state(ThisTarget, NewTState),
	    NewTarget2 = targetlist:set_endresult(NewTarget1, SPResponse),	% XXX only do this for final responses?
	    NewTargets1 = targetlist:update_target(NewTarget2, Targets),
	    NewTargets = try_next_destination(Status, ThisTarget, NewTargets1, State),
	    logger:log(debug, "sipproxy: Received branch result '~p ~s' from ~p (request: ~s ~s). ",
		       [Status, Reason, ClientPid, RMethod, sipurl:print(RURI)]),
	    case (OldTState == NewTState) of
		true ->
		    ok;	%% No change, don't have to log verbose things
		false ->
		    logger:log(debug, "sipproxy: Extra debug: Target state changed from '~p' to '~p'.~n"
			       "My Targets-list (response context) now contain :~n~p",
			       [OldTState, NewTState, targetlist:debugfriendly(NewTargets)])
	    end,
	    NewState2 = State#state{targets=NewTargets},
	    NewState3 = check_forward_immediately(ResponseToRequest, SPResponse, Branch, NewState2),
	    NewState4 = cancel_pending_if_invite_2xx_or_6xx(RMethod, Status, NewState3),
	    NewState4
    end.

%%--------------------------------------------------------------------
%% Function: check_forward_immediately(Request, SPResponse, Branch,
%%                                     State)
%%           Request    = request record()
%%           SPResponse = sp_response record()
%%           Branch     = string()
%%           State      = state record()
%% Descrip.: When one of our client transactions receive a response,
%%           this function gets called to see if it is a response that
%%           we are supposed to send to our parent immediately. Such
%%           responses are provisional responses (1xx) except 100, and
%%           2xx responses to INVITE.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
check_forward_immediately(_Request, #sp_response{status=Status, created=true}, _Branch, _State)
  when Status == 401; Status == 407 ->
    %% We can't create authorization requests in client transactions! Firstly, it
    %% makes no sense - secondly we can't aggregate any authorization headers into
    %% {Status, Reason} tuples. XXX maybe turn this into a 500, 400 or something?
    throw({error, sipproxy_cant_create_auth_response});
check_forward_immediately(Request, SPResponse, Branch, State)
  when is_record(Request, request), is_record(SPResponse, sp_response), is_record(State, state) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    {Status, Reason} = {SPResponse#sp_response.status, SPResponse#sp_response.reason},
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
			#response{status = FwdSPR#sp_response.status, reason = FwdSPR#sp_response.reason,
				  header = FwdSPR#sp_response.header, body = FwdSPR#sp_response.body}
		end,
	    util:safe_signal("sipproxy: ", Parent, {sipproxy_response, self(), Branch, FwdResponse}),
	    NewState;
	false ->
	    logger:log(debug, "sipproxy: Not forwarding response '~p ~s' (in response to ~s ~s) immediately",
		       [Status, Reason, Method, sipurl:print(URI)]),
	    State
    end.

%%--------------------------------------------------------------------
%% Function: forward_immediately(Method, Status, State)
%%           Method = string()
%%           Status = integer(), SIP status code
%%           State  = state record()
%% Descrip.: Return 'true' if this is a response that we are supposed
%%           to send to our parent immediately. Such responses are
%%           provisional responses (1xx) except 100, and 2xx responses
%%           to INVITE.
%% Returns : {true, NewState} | false
%%           NewState = state record()
%%--------------------------------------------------------------------
forward_immediately(Method, Status, State) when is_record(State, state), Status =< 100 ->
    %% A 100 Trying is considered an illegal response here since it is hop-by-hop and should
    %% be 'filtered' at the transaction layer.
    logger:log(error, "sipproxy: Invalid response ~p to ~s", [Status, Method]),
    false;
forward_immediately(_Method, Status, State) when is_record(State, state), Status =< 199 ->
    %% Provisional responses (except 100) are always forwarded immediately,
    %% regardless of Method
    {true, State};
forward_immediately("INVITE", Status, State) when is_record(State, state), Status =< 299 ->
    %% 2xx responses to INVITE are always forwarded immediately, regardless of final_response_sent
    NewState = State#state{mystate=completed, final_response_sent=true},
    {true, NewState};
forward_immediately(_Method, Status, State) when is_record(State, state), State#state.final_response_sent /= true, Status =< 299 ->
    %% 2xx responses to non-INVITE are forwarded immediately unless we have already
    %% forwarded some other final response already (implicitly another 2xx).
    NewState = State#state{mystate=completed, final_response_sent=true},
    {true, NewState};
forward_immediately(_Method, _Status, State) when is_record(State, state) ->
    false.

%%--------------------------------------------------------------------
%% Function: cancel_pending_if_invite_2xx_or_6xx(Method, Status,
%%                                               State)
%%           Method = string()
%%           Status = integer(), SIP status code
%%           State  = state record()
%% Descrip.: If we receive a 2xx response to INVITE, or a 6xx response
%%           to any request we should terminate all pending targets.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
cancel_pending_if_invite_2xx_or_6xx("INVITE", Status, State) when is_record(State, state), Status =< 199 ->
    State;
cancel_pending_if_invite_2xx_or_6xx("INVITE", Status, State) when is_record(State, state), Status =< 299 ->
    logger:log(debug, "sipproxy: Cancelling pending targets since one INVITE transaction resulted in a 2xx response ~p", [Status]),
    NewTargets = cancel_pending_targets(State#state.targets),
    State#state{targets=NewTargets};
cancel_pending_if_invite_2xx_or_6xx(_Method, Status, State) when is_record(State, state), Status =< 599 ->
    %% non-INVITE and not 6xx
    State;
cancel_pending_if_invite_2xx_or_6xx(_Method, Status, State) when is_record(State, state), Status =< 699 ->
    logger:log(debug, "sipproxy: Cancelling pending targets since one branch resulted in a 6xx response ~p", [Status]),
    NewTargets = cancel_pending_targets(State#state.targets),
    State#state{targets=NewTargets}.

%%--------------------------------------------------------------------
%% Function: printable_responses(Responses)
%%           Responses = list() of sp_response record()
%% Descrip.: Format a list of responses for (debug) logging.
%% Returns : list() of string()
%%--------------------------------------------------------------------
printable_responses(In) ->
    printable_responses2(In, []).

printable_responses2([], Res) ->
    lists:reverse(Res);
printable_responses2([H | T], Res) when is_record(H, sp_response) ->
    This = lists:concat([H#sp_response.status, " ", H#sp_response.reason]),
    printable_responses2(T, [This | Res]).

%%--------------------------------------------------------------------
%% Function: make_final_response(Responses)
%%           Responses = list() of ( response record() |
%%                                   {Status, Reason} tuple() )
%%              Status = integer(), SIP status code
%%              Reason = string(), SIP reason phrase
%% Descrip.: Determine which response is the best response in a
%%           response context (Responses). Perform any necessary
%%           authentication-header aggregation and return the response
%%           to deliver to our parent.
%% Returns : sp_response record() | none
%%
%% Note    : Look for 6xx responses, then pick the lowest but prefer
%%           401, 407, 415, 420 and 484 if we choose a 4xx and avoid
%%           503 if we choose a 5xx. This is described in RFC3261
%%           #16.7 (Response Processing), bullet 6. (Choosing the best
%%           response).
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
%% Function: aggregate_authreqs(BestResponse, Responses)
%%           BestResponse = sp_response record()
%%           Responses    = list() of sp_response record()
%% Descrip.: Put authentication headers from all responses in
%%           Responses into the one we have decided to send to our
%%           parent - BestResponse.
%% Returns : sp_response record() | none
%%
%% Note    : This is described in RFC3261 #16.7 (Response Processing),
%%           bullet 7. (Aggregate Authorization Header Field Values).
%%--------------------------------------------------------------------
%% BestResponse status == 407, Proxy-Authenticate
aggregate_authreqs(#sp_response{status=407}=BestResponse, Responses) when is_list(Responses) ->
    {Reason, Header} = {BestResponse#sp_response.reason, BestResponse#sp_response.header},
    ProxyAuth = collect_auth_headers(407, 'proxy-authenticate', Responses),
    logger:log(debug, "Aggregating ~p Proxy-Authenticate headers into response '407 ~s'",
	       [length(ProxyAuth), Reason]),
    NewHeader = keylist:set("Proxy-Authenticate", ProxyAuth, Header),
    BestResponse#sp_response{header=NewHeader};
%% BestResponse status == 401, WWW-Authenticate
aggregate_authreqs(#sp_response{status=401}=BestResponse, Responses) when is_list(Responses) ->
    {Reason, Header} = {BestResponse#sp_response.reason, BestResponse#sp_response.header},
    WWWAuth = collect_auth_headers(401, 'www-authenticate', Responses),
    logger:log(debug, "Aggregating ~p WWW-Authenticate headers into response '401 ~s'",
	       [length(WWWAuth), Reason]),
    NewHeader = keylist:set("WWW-Authenticate", WWWAuth, Header),
    BestResponse#sp_response{header=NewHeader};
%% BestResponse status is neither 401 nor 407
aggregate_authreqs(BestResponse, Responses) when is_record(BestResponse, sp_response), is_list(Responses) ->
    BestResponse.

%%--------------------------------------------------------------------
%% Function: collect_auth_headers(Status, Key, Responses)
%%           Status    = integer(), SIP status code
%%           Key       = term(), keylist key of headers to look for
%%           Responses = list() of sp_response record()
%% Descrip.: Part of aggregate_authreqs(). Get all headers matched by
%%           Key from all responses in Responses which have status
%%           Status.
%% Returns : list() of HValue
%%           HValue = term(), header value as returned by
%%                    keylist:fetch()
%%--------------------------------------------------------------------
collect_auth_headers(Status, Key, Responses) ->
    collect_auth_headers2(Status, Key, Responses, []).

collect_auth_headers2(_Status, _Key, [], Res) ->
    lists:reverse(Res);
collect_auth_headers2(Status, Key, [#sp_response{status=Status}=H | T], Res) ->
    This = keylist:fetch(Key, H#sp_response.header),
    collect_auth_headers2(Status, Key, T, [This | Res]);
collect_auth_headers2(Status, Key, [H | T], Res) when is_record(H, sp_response) ->
    collect_auth_headers2(Status, Key, T, Res).

%%--------------------------------------------------------------------
%% Function: pick_response(Nxx, Responses)
%%           Nxx       = integer(), 4 for 4xx repsonses etc.
%%           Responses = list() of sp_response record()
%% Descrip.: Pick a response from class Nxx. We use some different
%%           sorting routines depending on Nxx.
%% Returns : BestResponse = response record()
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
	    if
		A#sp_response.status =< B#sp_response.status ->
		    true;
		true ->
		    false
	    end;
	Apref == true ->
	    true;
	true ->
	    false
    end.

%% part of pick_response(), 4xx response sorting
is_4xx_preferred(Response) when is_record(Response, sp_response) ->
    lists:member(Response#sp_response.status, [401, 407, 415, 420, 484]).

%% part of pick_response(), 5xx response sorting
avoid_503_sort(#sp_response{status=503}, _) ->
    false;
avoid_503_sort(#sp_response{status=Astatus}, #sp_response{status=Bstatus}) when Astatus =< Bstatus ->
    true;
avoid_503_sort(_, _) ->
    false.

%%--------------------------------------------------------------------
%% Function: get_xx_responses(Min, Responses)
%%           Min       = integer(), number evenly divided by 100
%%           Responses = list() of sp_response record()
%% Descrip.: Get all responses between Min and Min + 99.
%%           sorting routines depending on Nxx.
%% Returns : BestResponse = sp_response record()
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
