%%%-------------------------------------------------------------------
%%% File    : clienttransaction.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : Client transaction.
%%%
%%% Created : 06 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(clienttransaction).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {branch, socket_in, logtag, socket, report_to, request, response, sipstate, timerlist, dst, timeout, cancelled, do_cancel}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/6
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Request, SocketIn, Dst, Branch, Timeout, Parent) ->
    gen_server:start_link(?MODULE, [Request, SocketIn, Dst, Branch, Timeout, Parent], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Request, SocketIn, Dst, Branch, Timeout, Parent]) ->
    {Method, URI, _, _} = Request,
    LogTag = lists:flatten(Branch ++ " " ++ Method),
    SipState = case Method of
		   "INVITE" -> calling;
		   _ -> trying
	       end,
    NewState1 = #state{branch=Branch, logtag=LogTag, socket=SocketIn, request=Request,
		       sipstate=SipState, timerlist=siptimer:empty(),
		       dst=Dst, socket_in=SocketIn, timeout=Timeout,
		       cancelled=false, report_to=Parent},
    logger:log(debug, "~s: Started new client transaction for request ~s ~s~n(dst ~p).",
	       [LogTag, Method, sipurl:print(URI), Dst]),
    State = initiate_request(NewState1),
    {ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    LogTag = State#state.logtag,
    logger:log(error, "~s: Client transcation received unknown gen_server call :~n~p",
	       [LogTag, Request]),
    check_quit({reply, {error, "unknown gen_server call", State}}, From).

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({sipmessage, response, Response, ResponseSocket, LogStr}, State) ->
    %% We have received a response to one of our requests.
    LogTag = State#state.logtag,
    {Status, Reason, _, _} = Response,
    LogLevel = if
		   Status >= 200 -> normal;
		   true -> debug
	       end,
    logger:log(LogLevel, "~s: Received response ~p ~s", [LogTag, Status, Reason]),
    NewState = process_received_response(Response, State),
    check_quit({noreply, NewState});

handle_cast({cancel, Msg}, State) ->
    LogTag = State#state.logtag,
    NewState = case State#state.cancelled of
		   true ->
		       logger:log(debug, "~s: Ignoring signal to cancel (~s) when in SIP-state '~p', I am already cancelled.",
				  [LogTag, Msg, State#state.sipstate]),
		       State;
		   _ ->
		       logger:log(debug, "~s: Branch requested to cancel (~s) when in SIP-state '~p'",
				  [LogTag, Msg, State#state.sipstate]),
		       cancel_request(State)
	       end,
    check_quit({noreply, NewState});

handle_cast({quit}, State) ->
    logger:log(debug, "~s: Received signal to quit", [State#state.logtag]),
    check_quit({stop, normal, State}).

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({siptimer, TRef, TDesc}, State) ->
    LogTag = State#state.logtag,
    NewState = case siptimer:get_timer(TRef, State#state.timerlist) of
		   none ->
		       logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring.", [LogTag, TRef, TDesc]),
		       State;
		   Timer ->
		       case process_timer(Timer, State) of
			   NewState1 when record(NewState1, state) ->
			       NewState1;
			   Unknown ->
			       logger:log(error, "~s: Client transaction: process_timer() returned unknown result :~n~p",
					  [LogTag, Unknown]),
			       State
		       end
	       end,
    check_quit({noreply, NewState});

handle_info(Info, State) ->
    logger:log(error, "Client transaction: Received unknown gen_server info :~n~p", [Info]),
    check_quit({noreply, State}).

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    %% XXX what to do on siperror? No meaning in sending that to our peer. Should be
    %% reported to report_to as if we received a 5xx perhaps?
    LogTag = State#state.logtag,
    case Reason of
    	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from clienttransaction :~n~p", [E]);
	{siperror, Status, Reason} ->
	    logger:log(error, "Client transaction threw an exception (~p ~s) - handling of that is not implemented",
		       [Status, Reason]);
	{siperror, Status, Reason, ExtraHeaders} ->
	    logger:log(error, "Client transaction threw an exception (~p ~s) - handling of that is not implemented",
		       [Status, Reason]);
	_ ->
	    SipState = State#state.sipstate,
	    case Reason of
		normal ->
		    logger:log(debug, "~s: Client transaction terminating in state ~p", [LogTag, SipState]);
		_ ->
		    logger:log(error, "~s: Client transaction terminating in state ~p, reason : ~p", [LogTag, SipState, Reason])
	    end,
	    logger:log(debug, "~s: Client transaction terminating in state ~p", [LogTag, SipState]),
	    true
    end,
    case util:safe_is_process_alive(State#state.report_to) of
	{true, R} ->
	    Branch = State#state.branch,
	    R ! {clienttransaction_terminating, {Branch, self()}};
	{false, R} ->
	    logger:log(debug, "~s: Client transaction orphaned ('~p' not alive) - can't inform parent that I am terminating now",
		       [LogTag, R])
    end,
    ok.


%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

check_quit(Res) ->
    check_quit(Res, none).

%% Before returing a cast() reply, check if the SIP-state is terminated and
%% if so return {stop ...}
check_quit(Res, From) ->
    case Res of
	{_, _, State, _} when record(State, state) ->
	    check_quit2(Res, From, State);
	{_, _, State} when record(State, state) ->
	    check_quit2(Res, From, State);
	{_, State, _} when record(State, state) ->
	    check_quit2(Res, From, State);
	{_, State} when record(State, state) ->
	    check_quit2(Res, From, State);
	_ ->
	    Res
    end.

check_quit2(Res, From, State) when record(State, state)  ->
    case State#state.sipstate of
	terminated ->
	    RStr = case State#state.response of
		       {Status, Reason, _, _} ->
			   io_lib:format("last received response was ~p ~s", [Status, Reason]);
		       undefined ->
			   "no responses received"
		   end,
	    {Method, URI, _, _} = State#state.request,
	    LogTag = State#state.logtag,
	    logger:log(debug, "~s: Server transaction (~s ~s) terminating in state '~p', ~s",
		       [LogTag, Method, sipurl:print(URI), State#state.sipstate, RStr]),
	    %% If there was a reply to be sent, we must send that before changing Res into
	    %% a stop.
	    NewReply = case Res of
			   {reply, Reply, _, _} ->
			       send_reply(Reply, From, State),
			       {stop, normal, State};
			   {reply, Reply, _} ->
			       send_reply(Reply, From, State),
			       {stop, normal, State};
			   {stop, _, _, _}  ->
			       Res;
			   {stop, _, _} ->
			       Res;
			   _ ->
			       {stop, normal, State}
		       end,
	    NewReply;
	_ ->
	    Res
    end.

send_reply(none, Reply, State) when record(State, state) ->
    logger:log(error, "~s: Can't send gen_server reply ~p to 'none'", [State#state.logtag, Reply]),
    error;
send_reply(To, Reply, State) when record(State, state) ->
    gen_server:reply(To, Reply).

process_timer(Timer, State) when record(State, state) ->
    [TRef, Timeout, Signal, Description] = siptimer:extract([ref, timeout, appsignal, description], Timer),
    LogTag = State#state.logtag,
    {Method, URI, Header, Body} = State#state.request,
    logger:log(debug, "~s: Timer ~p:~p fired", [LogTag, TRef, Description]),
    case Signal of

        {resendrequest} ->
	    SipState = State#state.sipstate,
            case lists:member(SipState, [trying, calling]) of
		true ->
		    logger:log(debug, "~s: Resend after ~s (request -> ~s): ~s",
		    	       [LogTag, siptimer:timeout2str(Timeout), sipurl:print(URI), Method]),
		    Branch = State#state.branch,
		    transportlayer:send_proxy_request(State#state.socket, State#state.request,
						      URI, ["branch=" ++ Branch, {dst, State#state.dst}]),
		    NewTimeout = get_request_resend_timeout(Method, Timeout, SipState),
		    NewTimerList = siptimer:revive_timer(Timer, NewTimeout, State#state.timerlist),
		    NewState1 = State#state{timerlist=NewTimerList},
		    NewState1;
		_ ->
		    logger:log(debug, "~s: Ignoring resendrequest of ~s ~s when in state ~p",
			       [LogTag, Method, sipurl:print(URI), SipState]),
		    State
	    end;

	{resendrequest_timeout} ->
	    logger:log(normal, "~s: Sending of ~s ~s timed out after ~s seconds",
		       [LogTag, Method, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
	    SipState = State#state.sipstate,
	    case SipState of
		trying ->
		    fake_request_timeout(State);
		calling ->
		    fake_request_timeout(State);
		proceeding ->
		    case Method of
			"INVITE" ->
			    logger:log(debug, "~s: Sending of original request (INVITE ~s) timed out in state " ++
				       "'proceeding' - cancel original request",
				       [LogTag, sipurl:print(URI)]),
			    cancel_request(State);
			_ ->
			    fake_request_timeout(State)
		    end;
		_ ->
		    logger:log(debug, "~s: Sending of original request (~s ~s) timed out in state '~s' - ignoring.",
			       [LogTag, Method, sipurl:print(URI), SipState]),
		    State
	    end;

	{terminate_transaction} ->
	    logger:log(debug, "~s: Received timer signal to terminate client transaction", [LogTag]),
	    terminate_transaction(State);

	{invite_timeout} ->
	    logger:log(debug, "~s: Request INVITE ~s timeout after ~s seconds",
		       [LogTag, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
	    end_invite(State);

	{invite_expire} ->
	    end_invite(State);

	_ ->
	    logger:log(error, "~s: Received unknown signal from timer ~p: ~p", [LogTag, TRef, Signal]),
	    State
    end.

get_request_resend_timeout("INVITE", OldTimeout, State) ->
    OldTimeout * 2;
get_request_resend_timeout(Method, OldTimeout, State) ->
    %% Use Timer T2 as maximum for non-INVITE requests when in 'trying' state.
    %% The meaning is that we should resend non-INVITE requests at 500ms, 1s, 2s, 4s, 4s, 4s ...
    %% or, if we receive a provisional response and go into proceeding state, every 4s.
    T2 = sipserver:get_env(timerT2, 4000),
    case State of
	trying ->
	    lists:min([OldTimeout * 2, T2]);
	proceeding ->
	    T2
    end.

%% Process a response delivered to us from one of our branches. Make sure
%% it is a response to the same kind of request as our original request,
%% then run it through the state machine to determine what to do.
process_received_response(Response, State) when record(State, state) ->
    OldSipState = State#state.sipstate,
    Request = State#state.request,
    {Method, URI, _, _} = Request,
    {Status, Reason, _, _} = Response,
    %% Stop resending of request as soon as possible.
    NewState1 = stop_resendrequest_timer(State),
    NewState2 = case Method of
		    "INVITE" ->
			%% ACK any 3xx, 4xx, 5xx and 6xx responses if they are responses to an INVITE 
			%% and we are in either 'calling' or 'proceeding' state
			NewState2_1 = ack_non_1xx_or_2xx_response_to_invite(Method, Response, NewState1),

			%% Cancel/reset INVITE request expire timer when receiving provisional responses
			NewState2_2 = update_invite_expire(Status, NewState2_1),
			NewState2_2;
		    _ ->
			NewState1
		end,
    {BranchAction, NewSipState} =
	received_response_state_machine(Method, Status, OldSipState),
    NewState3 = update_transaction_state(Response, NewSipState, BranchAction, NewState2),
    {NewState4, NewBranchAction} = act_on_new_sipstate(OldSipState, NewSipState, BranchAction, NewState3),
    NewState = perform_branchaction(NewBranchAction, NewState4),
    NewState.

update_transaction_state(Response, NewSipState, BranchAction, State) when record(State, state) ->
    {Status, Reason, _, _} = Response,
    LogTag = State#state.logtag,
    {Method, URI, _, _} = State#state.request,
    OldSipState = State#state.sipstate,
    case NewSipState of
	OldSipState ->
	    logger:log(debug, "~s: Evaluated ~p ~s (in response to ~s ~s). Remaining in state '~p', action '~p'.",
		       [LogTag, Status, Reason, Method, sipurl:print(URI), OldSipState, BranchAction]),
	    %% Update response so that we report exactly this response to our parent and not any previous
	    %% (for example, report a previous 183 instead of this new one)
	    NewState = State#state{response=Response},
	    NewState;
	_ ->
	    logger:log(debug, "~s: Evaluated ~p ~s (in response to ~s ~s). Changing state from '~p' to '~p', action '~p'.",
		       [LogTag, Status, Reason, Method, sipurl:print(URI), OldSipState, NewSipState, BranchAction]),
	    NewState = State#state{sipstate=NewSipState, response=Response},
	    NewState
    end.

act_on_new_sipstate(OldSipState, NewSipState, BranchAction, State) when record(State, state) ->
    case NewSipState of
	OldSipState ->
	    {State, BranchAction};
	_ ->
	    LogTag = State#state.logtag,
	    Request = State#state.request,
	    {Method, URI, _, _} = Request,
	    case NewSipState of
		proceeding ->
		    case Method of
			"INVITE" ->
			    {Status, Reason, _, _} = State#state.response,
			    case State#state.do_cancel of
				true ->
				    logger:log(debug, "~s: A previously cancelled transaction (INVITE ~s) entered state 'proceeding' " ++
					       "upon receiving a ~p ~s response. CANCEL it!", [LogTag, sipurl:print(URI), Status, Reason]),
				    NewState1 = State#state{do_cancel=false},
				    NewState2 = cancel_request(NewState1),
				    {NewState2, ignore};
				_ ->
				    {State, BranchAction}
			    end;
			_ ->
			    {State, BranchAction}
		    end;
		completed ->
		    TimerList = State#state.timerlist,
		    NewTimerList1 = siptimer:cancel_timers_with_appsignal({resendrequest_timeout}, TimerList),
		    NewState1 = State#state{timerlist=NewTimerList1},
		    %% Install TimerD with a minimum value of 32 seconds (RFC 3261 17.1.1.2)
		    %% or TimerK (T4, default 5 seconds) in case of non-INVITE request (17.1.2.2)
		    %% to stay alive for a few seconds collecting resends
		    NewState2 = case sipsocket:is_reliable_transport(State#state.socket) of
				    true ->
					logger:log(debug, "~s: Transitioning to SIP-state 'terminated' (almost) immediately "
						   "since the transport used for this branch is reliable.",
						   [LogTag]),
					TimerDK = 1,
					DKDesc = "terminate client transaction " ++ sipurl:print(URI) ++ " (Timer D or Timer K)",
					add_timer(TimerDK, DKDesc, {terminate_transaction}, NewState1);
				    _ ->
					case Method of
					    "INVITE" ->
						TimerD = 32 * 1000,
						DDesc = "terminate client transaction INVITE " ++ sipurl:print(URI) ++ " (Timer D)",
						add_timer(TimerD, DDesc, {terminate_transaction}, NewState1);
					    _ ->
						TimerK = sipserver:get_env(timerT4, 5000),
						KDesc = "terminate client transaction " ++ Method ++ " " ++ sipurl:print(URI) ++ " (Timer K)",
						add_timer(TimerK, KDesc, {terminate_transaction}, NewState1)
					end
				end,
		    {NewState2, BranchAction};
		terminated ->
		    TimerList = State#state.timerlist,
		    NewTimerList1 = siptimer:cancel_timers_with_appsignal({resendrequest_timeout}, TimerList),
		    NewState1 = State#state{timerlist=NewTimerList1},
		    {NewState1, BranchAction};
		_ ->
		    {State, BranchAction}
	    end
    end.

perform_branchaction(BranchAction, State) when record(State, state) ->
    LogTag = State#state.logtag,
    Response = State#state.response,
    {Status, Reason, _, _} = Response,
    Request = State#state.request,
    {Method, URI, _, _} = Request,
    case BranchAction of
	ignore ->
	    true;
	tell_parent ->
	    case State#state.report_to of
		R when pid(R) ->
		    case util:safe_is_process_alive(R) of
			{true, _} ->
			    logger:log(debug, "~s: Client transaction telling parent ~p about response ~p ~s",
				       [LogTag, R, Status, Reason]),
			    Branch = State#state.branch,
			    SipState = State#state.sipstate,
			    R ! {branch_result, Branch, SipState, Response};
			_ ->
			    logger:log(error, "~s: Client transaction orphaned ('~p' not alive) - not sending response ~p ~s to anyone",
				       [LogTag, R, Status, Reason])
		    end;
		_ ->
		    logger:log(debug, "~s: Client transaction has no one to report received response (~p ~s) to",
			       [LogTag, Status, Reason])
	    end
    end,
    State.

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

received_response_state_machine(Method, Status, State) when Status =< 199 ->
    logger:log(debug, "UAC decision: Received 1xx response ~p to ~s when in state '~p', ignoring", [Status, Method, State]),
    {ignore, State};

received_response_state_machine("INVITE", Status, State) when Status =< 299 ->
    %% We ALWAYS tell parent about 2xx responses to INVITE. RFC 3261 17.1.1.2 says we
    %% MUST enter the 'terminated' state when receiving a 2xx response to INVITE.
    %% This causes this client transaction to be terminated immediately, and any
    %% resends will then be passed directly to the proxy core.
    logger:log(debug, "UAC decision: Received 2xx response ~p to INVITE when in state '~p', " ++
	       "entering state 'terminated' and telling parent", [Status, State]),
    {tell_parent, terminated};

received_response_state_machine(Method, Status, trying) when Status =< 299 ->
    logger:log(debug, "UAC decision: Received 2xx response ~p to ~s when in state 'trying', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status =< 299 ->
    logger:log(debug, "UAC decision: Received 2xx response ~p to ~s when in state 'proceeding', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status =< 299 ->
    logger:log(debug, "UAC decision: Received LATE 2xx response ~p to ~s when in state '~p', " ++
	       "ignoring", [Status, Method, State]),
    {ignore, State};


received_response_state_machine(Method, Status, trying) when Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state 'trying', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine("INVITE", Status, calling) when Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to INVITE when in state 'calling', " ++
	       "entering state 'completed' and telling parent", [Status]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state 'proceeding', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state '~p', " ++
	       "ignoring", [Status, Method, State]),
    {ignore, State};


received_response_state_machine(Method, Status, trying) when Status =< 699 ->
    %% Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to ~s when in state 'trying', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine("INVITE", Status, calling) when Status =< 699 ->
    %% Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to INVITE when in state 'calling', " ++
	       "entering state 'completed' and telling parent", [Status]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status =< 699 ->
    %% Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to ~s when in state 'proceeding', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status =< 699 ->
    %% If one branch has answered 2xx already, I don't see how it would be correct to undo that
    logger:log(debug, "UAC decision: Received LATE 6xx response ~p to ~s when in state '~p', " ++
	       "ignoring", [Status, Method, State]),
    {ignore, State}.

stop_resendrequest_timer(State) when record(State, state) ->
    LogTag = State#state.logtag,
    {Method, URI, _, _} = State#state.request,
    NTList1 = siptimer:cancel_timers_with_appsignal({resendrequest}, State#state.timerlist),
    %% If it is an INVITE, we stop the resendrequest_timeout as well
    %% since an invite have {invite_expire} (Timer C) that takes
    %% care of ending it if nothing more is received.
    NTList2 = case Method of
		  "INVITE" ->
		      siptimer:cancel_timers_with_appsignal({resendrequest_timeout}, NTList1);
		  _ ->
		      NTList1
	      end,
    NewState = State#state{timerlist=NTList2},
    NewState.

%% Send a SIP-message and arrange for it to be resent as per the SIP specification.
%% TimerA causes the message to be resent,
%% TimerB will cause us to stop resending this message and
%% TimerC will start sending CANCELs to this request, if reached.
initiate_request(State) when record(State, state) ->
    Request = State#state.request,
    {Method, URI, _, _} = Request,
    LogTag = State#state.logtag,
    Branch = State#state.branch,
    Dst = State#state.dst,
    Timeout = State#state.timeout,
    logger:log(normal, "~s: Sending ~s ~s", [LogTag, Method, sipurl:print(URI)]),
    case transportlayer:send_proxy_request(State#state.socket_in, Request, URI, ["branch=" ++ Branch, {dst, Dst}]) of
	{sendresponse, Status, Reason} ->
	    logger:log(error, "~s: Transport layer failed sending ~s ~s to ~p, asked us to respond ~p ~s",
		       [LogTag, Method, sipurl:print(URI), Status, Reason]),
	    init_fake_request_response(Status, Reason, State);
	{ok, SendingSocket} ->
	    T1 = sipserver:get_env(timerT1, 500),
	    TimerA = get_initial_resend_timer(SendingSocket, T1),
	    TimerB = 64 * T1,
	    ADesc = "resendrequest " ++ Method ++ " to " ++ sipurl:print(URI) ++ " (TimerA)",
	    BDesc = "stopresend of " ++ Method ++ " to " ++ sipurl:print(URI) ++ " (TimerB)",
	    NewState1 = add_timer(TimerA, ADesc, {resendrequest}, State),
	    NewState2 = add_timer(TimerB, BDesc, {resendrequest_timeout}, NewState1),
	    NewState5 = case Method of
			    "INVITE" ->
				CDesc = "invite_expire of INVITE to " ++ sipurl:print(URI) ++ " (TimerC)",
				%% RFC 3261 says that Timer C for INVITE requests MUST be greater than 3 minutes
				TimerC = 181 * 1000,
				InviteTimeout = Timeout * 1000,
				TimeoutDesc = "invite_timeout of INVITE to " ++ sipurl:print(URI) ++ " after " ++ integer_to_list(Timeout) ++ " seconds",
				NewState3 = add_timer(TimerC, CDesc, {invite_expire}, NewState2),
				NewState4 = add_timer(InviteTimeout, TimeoutDesc, {invite_timeout}, NewState3);
			    _ ->
				NewState2
			end,
	    NewState6 = NewState5#state{socket=SendingSocket},
	    NewState6;
	_ ->
	    %% transport error, generate 503 Service Unavailable
	    init_fake_request_response(503, "Service Unavailable", State)
    end.

init_fake_request_response(Status, Reason, State) when record(State, state) ->
    NewState1 = fake_request_response(Status, Reason, State),
    %% Since initiate_request() is called from our init() we can't terminate right now. Add a
    %% timer that terminates this client transaction.
    TDesc = lists:concat(["Terminate transaction that never started after fake response ", Status, " ", Reason]),
    NewState = add_timer(1, TDesc, {terminate_transaction}, NewState1),
    NewState.

get_initial_resend_timer(Socket, T1) ->
    case sipsocket:is_reliable_transport(Socket) of
	true -> 0;
	_ -> T1
    end.

add_timer(Timeout, Description, AppSignal, State) when record(State, state) ->
    NewTimerList = siptimer:add_timer(Timeout, Description, AppSignal, State#state.timerlist),
    State#state{timerlist=NewTimerList}.

fake_request_response(Status, Reason, State) when record(State, state) ->
    Request = State#state.request,
    {Method, URI, Header, _} = Request,
    LogTag = State#state.logtag,
    SipProto = case State#state.dst of
		   {P, _, _} ->
		       P;
		   _ ->
		       logger:log(error, "~s: Client transaction dst is malformed, defaulting to UDP SIP protocol for fake response ~p ~s",
				  [LogTag, Status, Reason]),
		       sipsocket_udp
	       end,
    logger:log(normal, "~s: ~s ~s - pretend we got an '~p ~s' (and enter SIP state terminated)",
	       [LogTag, Method, sipurl:print(URI), Status, Reason]),
    FakeResponse = siprequest:make_response(Status, Reason, "", [], [], SipProto, Request),
    case util:safe_is_process_alive(State#state.report_to) of
	{true, R} ->
	    logger:log(debug, "~s: Client transaction forwarding fake response ~p ~s to ~p",
		       [LogTag, Status, Reason, R]),
	    Branch = State#state.branch,
	    R ! {branch_result, Branch, terminated, FakeResponse};
	{false, R} ->
	    logger:log(debug, "~s: Client transaction orphaned ('~p' not alive) - not sending fake response ~p ~s to anyone",
		       [LogTag, R, Status, Reason])
    end,
    NewState1 = State#state{response = FakeResponse},
    NewState = terminate_transaction(NewState1),
    NewState.

fake_request_timeout(State) when record(State, state) ->
    {Method, URI, _, _} = State#state.request,
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Client transaction (~s ~s) timed out - pretend we got an 408 Request Timeout",
	       [LogTag, Method, sipurl:print(URI)]),
    %% Create a fake 408 Request Timeout response to store as EndResult for this Target
    %% since RFC 3261 16.7 says we MUST act as if such a response was received
    NewState = fake_request_response(408, "Request Timeout", State),
    NewState.

%% end_invite() ends an INVITE transaction. This is called when our {call, ...} timeout
%% ocurs, or when Timer C fires. Whichever happens first.
end_invite(State) when record(State, state) ->
    {_, URI, _, _} = State#state.request,
    SipState = State#state.sipstate,
    LogTag = State#state.logtag,
    case SipState of
	calling ->
	    fake_request_timeout(State);
	proceeding ->
	    logger:log(debug, "~s: Sending of original request (INVITE ~s) timed out in state 'proceeding' - cancel original request",
		       [LogTag, sipurl:print(URI)]),
	    cancel_request(State);
	_ ->
	    logger:log(debug, "~s: Sending of original request (INVITE ~s) timed out in state '~s' - ignoring.",
		       [LogTag, sipurl:print(URI), SipState]),
	    State
    end.

terminate_transaction(State) when record(State, state) ->
    NewTimerList = siptimer:cancel_all_timers(State#state.timerlist),
    NewState = State#state{sipstate=terminated, timerlist=NewTimerList},
    NewState.

cancel_request(State) when record(State, state) ->
    {Method, URI, _, _} = State#state.request,
    LogTag = State#state.logtag,
    case Method of
	"INVITE" ->
	    NewState1 = State#state{cancelled=true},
	    logger:log(debug, "~s: Marked transaction as cancelled", [LogTag]),
	    SipState = NewState1#state.sipstate,
	    case SipState of
		calling ->
		    logger:log(debug, "~s: Stopping resend of INVITE ~s, but NOT starting CANCEL client transaction " ++
			       "right now since we are in state 'calling'", [LogTag, sipurl:print(URI)]),
		    NewState2 = stop_resendrequest_timer(NewState1),
		    NewState3 = NewState2#state{do_cancel=true},
		    NewState3;
		proceeding ->
		    logger:log(debug, "~s: Stopping resends of INVITE ~s and starting a CANCEL client transaction",
			       [LogTag, sipurl:print(URI)]),
		    NewState2 = stop_resendrequest_timer(NewState1),
		    NewState3 = start_cancel_transaction(NewState2),
		    NewState3;
		SipState ->
		    logger:log(debug, "~s: NOT starting CANCEL of previous request (INVITE ~p) since we are in state '~p'",
			       [LogTag, sipurl:print(URI), SipState]),
		    NewState1
	    end;
	_ ->
	    %% RFC 3261 9.1 says a stateful proxy SHOULD NOT send CANCEL of non-INVITE requests
	    logger:log(debug, "~s: Original request was non-INVITE (~s) - not sending CANCEL. Going into 'terminated' state.",
		       [LogTag, Method]),
	    NewState1 = terminate_transaction(State),
	    NewState1
    end.

%% Start fire-and-forget CANCEL transaction for this client transaction
start_cancel_transaction(State) when record(State, state) ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI, Header, _} = Request,
    logger:log(debug, "~s: initiate_cancel() Sending CANCEL ~s", [LogTag, sipurl:print(URI)]),
    {CSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    CancelId = {CSeqNum, "CANCEL"},
    %% Delete all Via headers. initiate_request() uses send_proxy_request() which 
    %% will add one for this proxy, and that is the only one that should be in a
    %% CANCEL. Set CSeq method to CANCEL and delete all Require and Proxy-Require
    %% headers. All this according to RFC 3261 9.1 Client Behaviour.
    CancelHeader1 = keylist:set("CSeq", [sipheader:cseq_print(CancelId)],
				Header),
    CancelHeader2 = keylist:delete("Via", CancelHeader1),
    CancelHeader3 = keylist:delete("Require", CancelHeader2),
    CancelHeader4 = keylist:delete("Proxy-Require", CancelHeader3),
    CancelHeader5 = keylist:delete("Content-Type", CancelHeader4),
    CancelHeader6 = keylist:delete("Content-Length", CancelHeader5),
    CancelHeader = keylist:set("Content-Length", ["0"], CancelHeader6),
    CancelRequest = {"CANCEL", URI, CancelHeader, ""},
    T1 = sipserver:get_env(timerT1, 500),
    NewState = add_timer(64 * T1, "quit after CANCEL", {terminate_transaction}, State),
    Socket = State#state.socket,
    Dst = State#state.dst,
    Branch = State#state.branch,
    %% XXX is the 32 * T1 correct for CANCEL?
    case transactionlayer:start_client_transaction(CancelRequest, Socket, Dst, Branch, 32 * T1, none) of
	P when pid(P) ->
	    logger:log(debug, "~s: Started CANCEL client transaction with pid ~p", [LogTag, P]);
	{error, E} ->
	    %% XXX what to do here?
	    logger:log(error, "~s: Failed starting CANCEL client transaction : ~p", [LogTag, E])
    end,
    NewState.

%% When we receive provisional responses to an INVITE we sent, we have to
%% reset TimerC to a value more than 3 minutes
update_invite_expire(Status, State) when record(State, state), Status == 100 ->
    State;
update_invite_expire(Status, State) when record(State, state), Status =< 199 ->
    LogTag = State#state.logtag,
    case siptimer:get_timers_appsignal_matching({invite_expire}, State#state.timerlist) of
	[] ->
	    logger:log(error, "~s: Received provisional response ~p to INVITE request, but no 'invite_expire' (Timer C) timer found",
		       [LogTag, Status]),
	    logger:log(debug, "~s: TimerList where I could not find an 'invite_expire' (Timer C) timer :~n~p",
		       [LogTag, siptimer:debugfriendly(State#state.timerlist)]),
	    State;
	InviteExpireTimerList ->
	    logger:log(debug, "~s: Received 1xx response to INVITE, resetting 'invite_expire' (Timer C)",
		       [LogTag]),
	    NewTimerList = siptimer:reset_timers(InviteExpireTimerList, State#state.timerlist),
	    NewState = State#state{timerlist=NewTimerList},
	    NewState
    end;
update_invite_expire(Status, State) ->
    State.

ack_non_1xx_or_2xx_response_to_invite("INVITE", {Status, Reason, _, _}, State) when record(State, state), Status =< 299 ->
    LogTag = State#state.logtag,
    {_, URI, _, _} = State#state.request,
    logger:log(debug, "~s: Not ACK-ing 1xx or 2xx response to INVITE ~s: ~p ~s",
	       [LogTag, sipurl:print(URI), Status, Reason]),
    State;
ack_non_1xx_or_2xx_response_to_invite("INVITE", {Status, Reason, RHeader, RBody}, State) when record(State, state), Status =< 699 ->
    LogTag = State#state.logtag,
    {_, URI, _, _} = State#state.request,
    Response = {Status, Reason, RHeader, RBody},
    logger:log(debug, "~s: ACK-ing 3xx, 4xx, 5xx or 6xx response ~p ~s to INVITE ~s (in state '~p')",
	       [LogTag, Status, Reason, sipurl:print(URI), State#state.sipstate]),
    generate_ack(Response, State),
    State.

%% Send an ACK. We do not set up any timers for retransmission since if this
%% ACK is lost we will receive a retransmitted response which will effectively
%% cause us to send a new ACK.
generate_ack({Status, Reason, _, _}, State) when record(State, state), Status =< 199 ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Not ACK-ing response ~p ~s. XXX shouldn't this be a PRACK?", [LogTag, Status, Reason]); 
generate_ack({Status, Reason, RHeader, RBody}, State) when record(State, state), Status =< 699 ->
    {Method, URI, Header, Body} = State#state.request,
    LogTag = State#state.logtag,
    Request = State#state.request,
    {_, URI, Header, Body} = Request,
    if
	Status =< 299 ->
	    logger:log(debug, "~s: generate_ack() Sending ACK of 2xx response ~p -> ~s",
		       [LogTag, Status, sipurl:print(URI)]),
	    {CSeq, _} = sipheader:cseq(keylist:fetch("CSeq", Header));
	true ->
	    logger:log(debug, "~s: generate_ack() Sending ACK of non-2xx response ~p -> ~s",
		       [LogTag, Status, sipurl:print(URI)]),
	    {CSeq, _} = sipheader:cseq(keylist:fetch("CSeq", RHeader))
    end,
    %% Don't copy any Via headers. send_proxy_request() will add one for this proxy,
    %% and that is the only one that should be in an ACK. RFC 3261 17.1.1.3
    %% copy() To and then set() it to preserve order...
    SendHeader1 = keylist:copy(Header, ["From", "Call-ID"]),
    %% get To: from the response we are ACKing (to preserve To-tag)
    SendHeader2 = keylist:set("To", keylist:fetch("To", RHeader), SendHeader1),
    SendHeader3 = keylist:set("CSeq", [sipheader:cseq_print({CSeq, "ACK"})], SendHeader2),
    %% Copy Route-header from original request, if present (mandated by RFC 3261 17.1.1.3)
    SendHeader = case keylist:fetch("Route", Header) of
		     [] ->
			 SendHeader3;
		     Route ->
			 keylist:set("Route", Route, SendHeader3)
		 end,
    Socket = State#state.socket,
    Dst = State#state.dst,
    %% Must use branch from response (really from original request sent out, but that
    %% should be what is in the response) to make sure we get the exact same branch in this ACK.
    TopVia = sipheader:topvia(RHeader),
    Branch = sipheader:get_via_branch_full(TopVia),
    ACKRequest = {"ACK", URI, SendHeader, ""},
    transportlayer:send_proxy_request(Socket, ACKRequest, URI, ["branch=" ++ Branch, {dst, Dst}]),
    ok.
