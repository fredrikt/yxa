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
-export([
	 start/6,
	 test/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	  branch,	%% string(), the Via branch paramter for this client transaction
	  socket_in,	%% sipsocket record(), the socket the original request was received on
	  logtag,	%% string(), a prefix for logging
	  socket,	%% sipsocket record(), the socket the transport layer sent out our request on
	  report_to,	%% pid() | none, our parent ('none' in case this is for example an independent CANCEL)
	  request,	%% request record(), the request we are supposed to send
	  response,	%% response record(), the last(???) response we have received()
	  sipstate,	%% atom(), trying|calling|proceeding|completed|terminated
	  timerlist,	%% siptimerlist record(), list of timers managed by siptimer module
	  dst,		%% sipdst record(), the destination for our request
	  timeout,	%% integer(), our definite timeout before ending an INVITE - NB: only applies to INVITE
	  cancelled=false,	%% atom(), true | false - have we cancelled ourselves? that is, sent out a CANCEL
	  do_cancel=false,	%% atom(), true | false - have we been instructed to cancel?
	  tl_branch,	%% string(), the branch the transaction layer actually used when sending our request
	  initialized=no	%% atom(), yes | no - have we finished our initialization process
	 }).

-include("sipsocket.hrl").
-include("siprecords.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start/6
%% Descrip.: Starts the server
%%--------------------------------------------------------------------
start(Request, SocketIn, Dst, Branch, Timeout, Parent) ->
    gen_server:start(?MODULE, [Request, SocketIn, Dst, Branch, Timeout, Parent], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, SocketIn, Dst, Branch, Timeout, Parent])
%%           Request  = request record()
%%           SocketIn = sipsocket record(), the default socket we
%%                      should give to the transport layer for this
%%                      request. XXX remove - don't do stateless.
%%           Dst      = sipdst record(), the destination for this
%%                                       client transaction
%%           Branch   = string()
%%           Timeout  = integer(), timeout for INVITE transactions
%%           Parent   = pid(), the process that initiated this client
%% Descrip.: Initiates a client transaction handler. Set us up for
%%           an immediate timeout signal in which we will try to send
%%           out our request - to not block caller.
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([Request, SocketIn, Dst, Branch, Timeout, Parent])
  when is_record(Request, request), is_record(Dst, sipdst), is_list(Branch),
       is_integer(Timeout), is_pid(Parent); Parent == none ->
    RegBranch = sipheader:remove_loop_cookie(Branch),
    {Method, URI} = {Request#request.method, Request#request.uri},
    Desc = lists:flatten(
	     io_lib:format("~s: ~s ~s (dst ~s)", [RegBranch, Method,
						  sipurl:print(URI), sipdst:dst2str(Dst)])),
    %% Get ourselves into the transaction state list first of all
    case transactionstatelist:add_client_transaction(Method, RegBranch, self(), Desc) of
	{duplicate, _} ->
	    logger:log(error, "Transaction layer: Can't start duplicate client transaction"),
	    {stop, "Client transaction already exists"};
	ok ->
	    %% Link to transaction_layer immediately (so that it removes this transaction
	    %% from the transactionstatelist when we exit).
	    TPid = erlang:whereis(transaction_layer),
	    true = link(TPid),
	    case init2([Request, SocketIn, Dst, Branch, Timeout, Parent]) of
		{ok, State, Timeout} when is_record(State, state) ->
		    {ok, State, Timeout};
		Reply ->
		    Reply
	    end;
	error ->
	    logger:log(error, "Transaction layer: Failed adding client transaction to transaction "
		       "layers list"),
	    {stop, "Transaction layer failed"}
    end.

init2([Request, SocketIn, Dst, Branch, Timeout, Parent])
  when is_record(Request, request), is_record(Dst, sipdst), is_list(Branch),
       is_integer(Timeout), is_pid(Parent); Parent == none ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = lists:flatten(Branch ++ " " ++ Method),
    SipState = case Method of
		   "INVITE" -> calling;
		   _ -> trying
	       end,
    State = #state{branch=Branch, logtag=LogTag, socket=SocketIn, request=Request,
		   sipstate=SipState, timerlist=siptimer:empty(),
		   dst=Dst, socket_in=SocketIn, timeout=Timeout,
		   report_to=Parent},
    logger:log(debug, "~s: Started new client transaction for request ~s ~s~n(dst ~s).",
	       [LogTag, Method, sipurl:print(URI), sipdst:dst2str(Dst)]),
    %% Timeout 0 so that the spawned process immediately gets a timeout signal
    %% which will cause it to send out our first request. We can't do that here since
    %% that might risk to block the caller, and since that could cause the spawn_link
    %% to fail in case the transport layer (for example) does a throw()
    {ok, State, 0}.


%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    LogTag = State#state.logtag,
    logger:log(error, "~s: Client transaction received unknown gen_server call :~n~p",
	       [LogTag, Request]),
    check_quit({reply, {error, "unknown gen_server call", State}}, From).


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_cast({sipmessage, Response, Origin, LogStr},
%%                       State)
%%           Response = response record()
%%           Origin   = siporigin record()
%%           LogStr   = string()
%% Descrip.: We have received a response to our request. Process it.
%% Returns : {noreply, State}          |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({sipmessage, Response, Origin, _LogStr}, State) when is_record(Response, response),
								 is_record(Origin, siporigin) ->
    %% We have received a response to one of our requests.
    LogTag = State#state.logtag,
    LogLevel = if
		   Response#response.status >= 200 -> normal;
		   true -> debug
	       end,
    logger:log(LogLevel, "~s: Received response '~p ~s'", [LogTag, Response#response.status, Response#response.reason]),
    NewState = process_received_response(Response, State),
    check_quit({noreply, NewState});

%%--------------------------------------------------------------------
%% Function: handle_cast({cancel, Msg}, State)
%%           Msg = string()
%% Descrip.: We have been asked to cancel. Do so if we are not already
%%           cancelled. 'Doing so' means start an independent CANCEL
%%           client transaction, and mark ourselves as cancelled.
%% Returns : {noreply, State}          |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({cancel, Msg}, State=#state{cancelled=true}) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Ignoring signal to cancel (~s) when in SIP-state '~p', I am already cancelled.",
	       [LogTag, Msg, State#state.sipstate]),
    check_quit({noreply, State});
handle_cast({cancel, Msg}, State=#state{cancelled=false}) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Branch requested to cancel (~s) when in SIP-state '~p'",
	       [LogTag, Msg, State#state.sipstate]),
    NewState = cancel_request(State),
    check_quit({noreply, NewState});

%%--------------------------------------------------------------------
%% Function: handle_cast({expired}, State)
%% Descrip.: The transaction layer tells us that we are long overdue.
%%           Exit.
%% Returns : {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({expired}, State) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Received signal that I am expired, exiting.", [LogTag]),
    check_quit({stop, "Client transaction expired", State});

%%--------------------------------------------------------------------
%% Function: handle_cast({quit}, State)
%% Descrip.: Asked to quit. Do so.
%% Returns : {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({quit}, State) ->
    logger:log(debug, "~s: Received signal to quit", [State#state.logtag]),
    check_quit({stop, normal, State}).


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info({siptimer, TRef, TDesc}, State)
%% Descrip.: One of our timers (siptimers) have fired. Locate it in
%%           our list of siptimers and then invoke process_timer/2.
%% Returns : {noreply, State}          |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({siptimer, TRef, TDesc}, State) ->
    LogTag = State#state.logtag,
    NewState =
	case siptimer:get_timer(TRef, State#state.timerlist) of
	    none ->
		%% This is not really a serious situation. It is actually a race condition that
		%% shows up from time to time under heavy load - the timer might have fired just
		%% before we deleted it from our siptimer list.
		%%logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring.", [LogTag, TRef, TDesc]),
		logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring. SIP state '~p', Timerlist :~n~p",
			   [LogTag, TRef, TDesc, State#state.sipstate, State#state.timerlist]),
		State;
	    Timer ->
		case process_timer(Timer, State) of
		    NewState1 when is_record(NewState1, state) ->
			NewState1;
		    Unknown ->
			logger:log(error, "~s: Client transaction: process_timer() returned unknown result :~n~p",
				   [LogTag, Unknown]),
			State
		end
	end,
    check_quit({noreply, NewState});

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%% Descrip.: Continuation of init/1 but executing after init/1 has
%%           returned, so the caller (the transaction_layer) is not
%%           blocked. Invoke initiate_request/1 to start trying to
%%           send our request to it's destination.
%% Returns : {noreply, State}          |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(timeout, #state{initialized=no}=State) ->
    {noreply, initiate_request(State#state{initialized=yes})};

handle_info(Info, State) ->
    logger:log(error, "Client transaction: Received unknown gen_server info :~n~p", [Info]),
    check_quit({noreply, State}).

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%%           Reason = term()
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
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
	{siperror, Status, Reason, _ExtraHeaders} ->
	    logger:log(error, "Client transaction threw an exception (~p ~s) - handling of that is not implemented",
		       [Status, Reason]);
	_ ->
	    SipState = State#state.sipstate,
	    case Reason of
		normal ->
		    logger:log(debug, "~s: Client transaction terminating in state ~p", [LogTag, SipState]);
		_ ->
		    logger:log(error, "~s: Client transaction terminating in state ~p, reason : ~p",
			       [LogTag, SipState, Reason])
	    end,
	    true
    end,
    ReportTo = State#state.report_to,
    case util:safe_is_process_alive(ReportTo) of
	{true, ReportTo} ->
	    logger:log(debug, "~s: Informing my parent (~p) that I am terminating now",
		       [LogTag, ReportTo]),
	    Branch = State#state.branch,
	    ReportTo ! {clienttransaction_terminating, {Branch, self()}};
	{false, _} ->
	    logger:log(debug, "~s: Client transaction orphaned ('~p' not alive) - can't "
		       "inform parent that I am terminating now", [LogTag, ReportTo])
    end,
    ok.


%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: check_quit(Res, From)
%%           Res  = term(), gen_server:cast() return value
%%           From = term(), gen_server from-value | none
%% Descrip.: Extract the state record() from Res, and check if it's
%%           sipstate is 'terminated'. If it is then turn Res into a
%%           stop signal, but if Res was {reply, ...} execute a
%%           gen_server:reply() first.
%% Returns : {noreply, State}          |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
check_quit(Res) ->
    check_quit(Res, none).

%% Before returing a cast() reply, check if the SIP-state is terminated and
%% if so return {stop ...}
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

check_quit2(Res, From, #state{sipstate=terminated}=State) ->
    RStr = case State#state.response of
	       Re when is_record(Re, response) ->
		   io_lib:format("last received response was ~p ~s", [Re#response.status, Re#response.reason]);
	       undefined ->
		   "no responses received"
	   end,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Client transaction (~s ~s) terminating in state '~p', ~s",
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
check_quit2(Res, _From, _State) ->
    %% sipstate was not 'terminated'
    Res.

send_reply(none, Reply, State) when is_record(State, state) ->
    logger:log(error, "~s: Can't send gen_server reply ~p to 'none'", [State#state.logtag, Reply]),
    error;
send_reply(To, Reply, State) when is_record(State, state) ->
    gen_server:reply(To, Reply).

%%--------------------------------------------------------------------
%% Function: process_timer(Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: Process fired siptimer events.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer(Timer, State) when is_record(State, state) ->
    [TRef, Signal, Description] = siptimer:extract([ref, appsignal, description], Timer),
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Timer ~p:~p fired", [LogTag, TRef, Description]),
    process_timer2(Signal, Timer, State).

%%--------------------------------------------------------------------
%% Function: process_timer2({resendrequest}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: It is time to resend our requests, if we are still in any
%%           of the SIP states 'trying' or 'calling'.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({resendrequest}, Timer, State) when is_record(State, state) ->
    [Timeout] = siptimer:extract([timeout], Timer),
    {LogTag, SipState} = {State#state.logtag, State#state.sipstate},
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    case lists:member(SipState, [trying, calling]) of
	true ->
	    logger:log(debug, "~s: Resend after ~s (request -> ~s): ~s",
		       [LogTag, siptimer:timeout2str(Timeout), sipurl:print(URI), Method]),
	    Branch = State#state.branch,
	    transportlayer:send_proxy_request(State#state.socket, State#state.request,
					      State#state.dst, ["branch=" ++ Branch]),
	    NewTimeout = get_request_resend_timeout(Method, Timeout, SipState),
	    NewTimerList = siptimer:revive_timer(Timer, NewTimeout, State#state.timerlist),
	    NewState1 = State#state{timerlist=NewTimerList},
	    NewState1;
	_ ->
	    logger:log(debug, "~s: Ignoring resendrequest of ~s ~s when in state ~p",
		       [LogTag, Method, sipurl:print(URI), SipState]),
	    State
    end;

%%--------------------------------------------------------------------
%% Function: process_timer2({resendrequest_timeout}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: We have failed. We have reached the end of our resending
%%           cycle and it is time to give up. Fake receiving a
%%           '408 Request Timeout' or, if we are an INVITE in SIP
%%           state 'proceeding', cancel ourselves.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({resendrequest_timeout}, Timer, State) when is_record(State, state) ->
    [Timeout] = siptimer:extract([timeout], Timer),
    {LogTag, Request} = {State#state.logtag, State#state.request},
    {Method, URI} = {Request#request.method, Request#request.uri},
    logger:log(normal, "~s: Sending of ~s ~s timed out after ~s seconds",
	       [LogTag, Method, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
    case State#state.sipstate of
	trying ->
	    fake_request_timeout(State);
	calling ->
	    fake_request_timeout(State);
	proceeding when Method == "INVITE" ->
	    logger:log(debug, "~s: Sending of original request (INVITE ~s) timed out in state " ++
		       "'proceeding' - cancel original request",
		       [LogTag, sipurl:print(URI)]),
	    cancel_request(State);
	proceeding ->
	    fake_request_timeout(State);
	SipState when SipState == completed; SipState == terminated ->
	    logger:log(debug, "~s: Sending of original request (~s ~s) timed out in state '~s' - ignoring.",
		       [LogTag, Method, sipurl:print(URI), SipState]),
	    State
    end;

%%--------------------------------------------------------------------
%% Function: process_timer2({terminate_transaction}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: It is time to terminate this transaction.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({terminate_transaction}, _Timer, State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Received timer signal to terminate client transaction", [LogTag]),
    terminate_transaction(State);

%%--------------------------------------------------------------------
%% Function: process_timer2({invite_timeout}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: The user specified maximum time to attempt to reach the
%%           called party with this INVITE is up. End this INVITE.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({invite_timeout}, Timer, State) when is_record(State, state) ->
    [Timeout] = siptimer:extract([timeout], Timer),
    LogTag = State#state.logtag,
    Request = State#state.request,
    URI = Request#request.uri,
    logger:log(debug, "~s: Request INVITE ~s timeout after ~s seconds",
	       [LogTag, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
    end_invite(State);

%%--------------------------------------------------------------------
%% Function: process_timer2({invite_expire}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: The extra timeout for INVITE (Timer C) has fired. End
%%           this INVITE transaction.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({invite_expire}, _Timer, State) when is_record(State, state) ->
    end_invite(State);

process_timer2(Signal, Timer, State) when is_record(State, state) ->
    [TRef] = siptimer:extract([ref], Timer),
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received unknown signal from timer ~p: ~p", [LogTag, TRef, Signal]),
    State.

%%--------------------------------------------------------------------
%% Function: get_request_resend_timeout(Method, OldTimeout, State)
%%           Method     = string()
%%           OldTimeout = integer()
%%           State      = atom(), trying | proceeding | ...
%% Descrip.: Figure out how long the next resend timeout for this
%%           particular method and state should be, according to the
%%           RFC3261.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
get_request_resend_timeout("INVITE", OldTimeout, _State) ->
    OldTimeout * 2;
get_request_resend_timeout(_Method, OldTimeout, State) when State == trying; State == proceeding ->
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

%%--------------------------------------------------------------------
%% Function: process_received_response(Response, State)
%%           Response = response record()
%%           State    = state record()
%% Descrip.: Process a response that the transport layer has received
%%           and that has been delivered to us since it matches our
%%           transaction. Run it through the state machine to
%%           determine what to do.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_received_response(Response, State) when is_record(State, state), is_record(Response, response) ->
    OldSipState = State#state.sipstate,
    Request = State#state.request,
    Method = Request#request.method,
    Status = Response#response.status,
    %% Stop resending of request as soon as possible.
    NewState1 = stop_resendrequest_timer(State),
    NewState2 =
	case Method of
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

%%--------------------------------------------------------------------
%% Function: update_transaction_state(Response, NewSipState,
%%                                    BranchAction, State)
%%           Response     = response record()
%%           NewSipState  = atom(), trying | calling | proceeding |
%%                          completed | terminated
%%           BranchAction = ignore | tell_parent
%%           State        = state record()
%% Descrip.: Update State with the received response and new SIP state
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
update_transaction_state(Response, NewSipState, BranchAction, State) when is_record(State, state) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
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
	    logger:log(debug, "~s: Evaluated ~p ~s (in response to ~s ~s). Changing state from '~p' "
		       "to '~p', action '~p'.", [LogTag, Status, Reason, Method, sipurl:print(URI),
						 OldSipState, NewSipState, BranchAction]),
	    NewState = State#state{sipstate=NewSipState, response=Response},
	    NewState
    end.

%%--------------------------------------------------------------------
%% Function: act_on_new_sipstate(S, S, BranchAction, State)
%%           OldSipState  = atom(), trying | calling | proceeding |
%%                                  completed | terminated
%%           NewSipState  = atom(), trying | calling | proceeding |
%%                                  completed | terminated
%%           BranchAction = atom(),
%%           State        = state record()
%% Descrip.: Check if the SIP state has really changed, if so - invoke
%%           act_on_new_sipstate2().
%% Returns : {NewState, NewBranchAction}
%%           NewState        = state record()
%%           NewBranchAction = atom(), ignore | tell_parent
%%--------------------------------------------------------------------
act_on_new_sipstate(OldSipState, NewSipState, BranchAction, State)
  when is_atom(OldSipState), is_atom(NewSipState), is_atom(BranchAction),
       BranchAction == ignore; BranchAction == tell_parent,
       is_record(State, state), OldSipState == NewSipState ->
    %% State has not changed
    {State, BranchAction};
act_on_new_sipstate(OldSipState, NewSipState, BranchAction, State)
  when is_atom(OldSipState), is_atom(NewSipState),  is_atom(BranchAction),
       BranchAction == ignore; BranchAction == tell_parent,
       is_record(State, state) ->
    %% State has changed
    act_on_new_sipstate2(NewSipState, BranchAction, State).

%%--------------------------------------------------------------------
%% Function: act_on_new_sipstate2(proceeding, BranchAction, State)
%%           BranchAction = ignore | tell_parent
%%           State        = state record()
%% Descrip.: New SIP state is 'proceeding'. Check that we are not an
%%           INVITE transaction that has already been cancelled.
%% Returns : {NewState, NewBranchAction}
%%           NewState        = state record()
%%           NewBranchAction = atom(), ignore | tell_parent
%%--------------------------------------------------------------------
act_on_new_sipstate2(proceeding, BranchAction, State) when is_record(State, state) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    case Method of
	"INVITE" ->
	    LogTag = State#state.logtag,
	    Response = State#state.response,
	    {Status, Reason} = {Response#response.status, Response#response.reason},
	    case State#state.do_cancel of
		true ->
		    logger:log(debug, "~s: A previously cancelled transaction (INVITE ~s) "
			       "entered state 'proceeding' upon receiving a '~p ~s' response. "
			       "CANCEL ourselves!", [LogTag, sipurl:print(URI), Status, Reason]),
		    NewState1 = State#state{do_cancel=false},
		    NewState2 = cancel_request(NewState1),
		    {NewState2, ignore};
		_ ->
		    {State, BranchAction}
	    end;
	_ ->
	    {State, BranchAction}
    end;

%%--------------------------------------------------------------------
%% Function: act_on_new_sipstate2(completed, BranchAction, State)
%%           BranchAction = ignore | tell_parent
%%           State        = state record()
%% Descrip.: New SIP state is 'completed'. Set up a timer that will
%%           take us to 'terminated'.
%% Returns : {NewState, BranchAction}
%%           NewState = state record()
%%--------------------------------------------------------------------
act_on_new_sipstate2(completed, BranchAction, State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},

    TimerList = State#state.timerlist,
    NewTimerList1 = siptimer:cancel_timers_with_appsignal({resendrequest_timeout}, TimerList),
    NewState1 = State#state{timerlist=NewTimerList1},
    %% Install TimerD with a minimum value of 32 seconds (RFC 3261 17.1.1.2)
    %% or TimerK (T4, default 5 seconds) in case of non-INVITE request (17.1.2.2)
    %% to stay alive for a few seconds collecting resends
    NewState2 =
	case sipsocket:is_reliable_transport(State#state.socket) of
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


%%--------------------------------------------------------------------
%% Function: act_on_new_sipstate2(terminated, BranchAction, State)
%%           BranchAction = ignore | tell_parent
%%           State        = state record()
%% Descrip.: New SIP state is 'terminated'. Stop all timers before
%%           we terminate.
%% Returns : {NewState, BranchAction}
%%           NewState = state record()
%%--------------------------------------------------------------------
act_on_new_sipstate2(terminated, BranchAction, State) when is_record(State, state) ->
    TimerList = State#state.timerlist,
    NewTimerList1 = siptimer:cancel_timers_with_appsignal({resendrequest_timeout}, TimerList),
    NewState1 = State#state{timerlist=NewTimerList1},
    {NewState1, BranchAction};

%%--------------------------------------------------------------------
%% Function: act_on_new_sipstate2(SipState, BranchAction, State)
%%           SipState  = atom(), trying | calling
%%           BranchAction = ignore | tell_parent
%%           State        = state record()
%% Descrip.: New SIP state is 'trying' or 'calling' - just return.
%% Returns : {State, BranchAction}
%%--------------------------------------------------------------------
act_on_new_sipstate2(SipState, BranchAction, State)
  when is_record(State, state), is_atom(SipState),
       SipState == trying; SipState == calling ->
    {State, BranchAction}.

%%--------------------------------------------------------------------
%% Function: perform_branchaction(Action, State)
%%           Action = ignore | tell_parent
%%           State  = state record()
%% Descrip.: If Action is 'tell_parent', we tell our parent process
%%           about the response we have received using a
%%           {branch_result, ...} signal.
%% Returns : State = state record()
%%--------------------------------------------------------------------
perform_branchaction(ignore, State) when is_record(State, state) ->
    State;
perform_branchaction(tell_parent, State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    Response = State#state.response,
    {Status, Reason} = {Response#response.status, Response#response.reason},
    ReportTo = State#state.report_to,
    case util:safe_is_process_alive(ReportTo) of
	{true, ReportTo} when is_pid(ReportTo) ->
	    logger:log(debug, "~s: Client transaction telling parent ~p about response '~p ~s'",
		       [LogTag, ReportTo, Status, Reason]),
	    Branch = State#state.branch,
	    SipState = State#state.sipstate,
	    ReportTo ! {branch_result, Branch, SipState, Response};
	{false, _} ->
	    %% XXX is this an error? Not for independent CANCEL transactions, but
	    %% maybe for others?
	    LogLevel =
		case (State#state.request)#request.method of
		    "CANCEL" -> debug;
		    _ -> error
		end,
	    logger:log(LogLevel, "~s: Client transaction orphaned ('~p' not alive) - not sending "
		       "response '~p ~s' to anyone", [LogTag, ReportTo, Status, Reason])
    end,
    State.

%%--------------------------------------------------------------------
%% Function: received_response_state_machine(Method, Status, State)
%%           Method = string(), SIP request method
%%           Status = integer(), SIP status code (received response)
%%           State  = atom(), trying | calling | proceeding |
%%                            completed | terminated
%% Descrip.: State machine to decide what to do with a received
%%           response, and what we should set our SIP state to.
%% Returns : {Action, State}
%%           Action = atom(), ignore | tell_parent
%%           State  = trying | calling | proceeding | completed |
%%                    terminated
%%--------------------------------------------------------------------
received_response_state_machine("INVITE", Status, calling) when Status == 100 ->
    logger:log(debug, "UAC decision: Received 100 in response to INVITE, going from 'calling' to 'proceeding'"),
    {ignore, proceeding};
received_response_state_machine(Method, Status, trying) when Status == 100 ->
    logger:log(debug, "UAC decision: Received 100 in response to ~s, going from 'trying' to 'proceeding'", [Method]),
    {ignore, proceeding};
received_response_state_machine("INVITE", Status, State) when Status == 100 ->
    logger:log(debug, "UAC decision: Received 100 in response to INVITE when in state '~p', ignoring", [State]),
    {ignore, State};


received_response_state_machine("INVITE", Status, calling) when Status >= 100, Status =< 199 ->
    logger:log(debug, "UAC decision: Received first 1xx response ~p to INVITE, going from "
	       "'calling' to 'proceeding' and telling parent", [Status]),
    {tell_parent, proceeding};
received_response_state_machine("INVITE", Status, proceeding) when Status >= 100, Status =< 199 ->
    logger:log(debug, "UAC decision: Received 1xx response ~p to INVITE when in state 'proceeding', "
	       "telling parent", [Status]),
    {tell_parent, proceeding};

received_response_state_machine(Method, Status, trying) when Status >= 100, Status =< 199 ->
    logger:log(debug, "UAC decision: Received first 1xx response ~p to ~s, going from 'trying' to "
	       "'proceeding' and telling parent", [Status, Method]),
    {tell_parent, proceeding};

received_response_state_machine(Method, Status, State) when Status >= 100, Status =< 199 ->
    logger:log(debug, "UAC decision: Received 1xx response ~p to ~s when in state '~p', ignoring",
	       [Status, Method, State]),
    {ignore, State};

received_response_state_machine("INVITE", Status, State) when Status >= 200, Status =< 299 ->
    %% We ALWAYS tell parent about 2xx responses to INVITE. RFC 3261 17.1.1.2 says we
    %% MUST enter the 'terminated' state when receiving a 2xx response to INVITE.
    %% This causes this client transaction to be terminated immediately, and any
    %% resends will then be passed directly to the proxy core.
    logger:log(debug, "UAC decision: Received 2xx response ~p to INVITE when in state '~p', " ++
	       "entering state 'terminated' and telling parent", [Status, State]),
    {tell_parent, terminated};

received_response_state_machine(Method, Status, trying) when Status >= 200, Status =< 299 ->
    logger:log(debug, "UAC decision: Received 2xx response ~p to ~s when in state 'trying', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status >= 200, Status =< 299 ->
    logger:log(debug, "UAC decision: Received 2xx response ~p to ~s when in state 'proceeding', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status >= 200, Status =< 299 ->
    logger:log(debug, "UAC decision: Received LATE 2xx response ~p to ~s when in state '~p', " ++
	       "ignoring", [Status, Method, State]),
    {ignore, State};


received_response_state_machine(Method, Status, trying) when Status >= 300, Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state 'trying', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine("INVITE", Status, calling) when Status >= 300, Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to INVITE when in state 'calling', " ++
	       "entering state 'completed' and telling parent", [Status]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status >= 300, Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state 'proceeding', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status >= 300, Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3xx, 4xx or 5xx response ~p to ~s when in state '~p', " ++
	       "ignoring", [Status, Method, State]),
    {ignore, State};


received_response_state_machine(Method, Status, trying) when Status >= 600, Status =< 699 ->
    %% Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to ~s when in state 'trying', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine("INVITE", Status, calling) when Status >= 600, Status =< 699 ->
    %% Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to INVITE when in state 'calling', " ++
	       "entering state 'completed' and telling parent", [Status]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status >= 600, Status =< 699 ->
    %% Global failure, make all branches die.
    logger:log(debug, "UAC decision: Received 6xx response ~p to ~s when in state 'proceeding', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status >= 600, Status =< 699,
							    is_atom(State),
							    State == completed;
							    State == terminated ->
    %% If one branch has answered 2xx already, I don't see how it would be correct to undo that
    logger:log(debug, "UAC decision: Received LATE 6xx response ~p to ~s when in state '~p', " ++
	       "ignoring", [Status, Method, State]),
    {ignore, State}.

%%--------------------------------------------------------------------
%% Function: stop_resendrequest_timer(State)
%%           State = state record()
%% Descrip.: Locate and stop any {resendrequest_timeout} siptimers.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
stop_resendrequest_timer(State) when is_record(State, state) ->
    Request = State#state.request,
    Method = Request#request.method,
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

%%--------------------------------------------------------------------
%% Function: initiate_request(State)
%%           State = state record()
%% Descrip.: Send a SIP request and arrange for it to be resent as per
%%           the SIP specification.
%% Returns : NewState = state record()
%% Notes   : TimerA causes the message to be resent,
%%           TimerB will cause us to stop resending this message and
%%           TimerC will start sending CANCELs to this request, if
%%           reached.
%%--------------------------------------------------------------------
initiate_request(State) when is_record(State, state) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = State#state.logtag,
    Branch = State#state.branch,
    Dst = State#state.dst,
    Timeout = State#state.timeout,
    SocketDstStr = lists:concat([Dst#sipdst.proto, ":", Dst#sipdst.addr, ":", Dst#sipdst.port]),
    logger:log(normal, "~s: Sending ~s ~s (to ~s)", [LogTag, Method, sipurl:print(URI),
						     SocketDstStr]),
    case transportlayer:send_proxy_request(State#state.socket_in, Request, Dst, ["branch=" ++ Branch]) of
	{ok, SendingSocket, TLBranch} ->
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
				TimeoutDesc = "invite_timeout of INVITE to " ++ sipurl:print(URI) ++ " after " ++
				    integer_to_list(Timeout) ++ " seconds",
				NewState3 = add_timer(TimerC, CDesc, {invite_expire}, NewState2),
				NewState4 = add_timer(InviteTimeout, TimeoutDesc, {invite_timeout}, NewState3),
				NewState4;
			    _ ->
				NewState2
			end,
	    NewState6 = NewState5#state{socket=SendingSocket, tl_branch=TLBranch},
	    NewState6;
	_ ->
	    %% transport error, generate 503 Service Unavailable
	    logger:log(normal, "~s: Transport layer failed sending ~s ~s to ~s, pretend we got an "
		       "'503 Service Unavailable'", [LogTag, Method, sipurl:print(URI), SocketDstStr]),
	    init_fake_request_response(503, "Service Unavailable", State)
    end.

%%--------------------------------------------------------------------
%% Function: init_fake_request_response(Status, Reason, State)
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           State  = state record()
%% Descrip.: Fake receiving an error response. This variant is only
%%           used from within initiate_request/1.
%% Returns : NewState = state record()
%% Notes   : XXX this function is probably not needed anymore since we
%%           are no longer calling initiate_request() from init/1.
%%--------------------------------------------------------------------
init_fake_request_response(Status, Reason, State) when is_record(State, state) ->
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

add_timer(Timeout, Description, AppSignal, State) when is_record(State, state) ->
    NewTimerList = siptimer:add_timer(Timeout, Description, AppSignal, State#state.timerlist),
    State#state{timerlist=NewTimerList}.

%%--------------------------------------------------------------------
%% Function: fake_request_response(Status, Reason, State)
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           State  = state record()
%% Descrip.: Fake receiving an error response. This is a common way
%%           to tell upper layer white lies about why we have
%%           terminated. It is specified to be done this way in
%%           RFC3261.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
fake_request_response(Status, Reason, State) when is_record(State, state) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = State#state.logtag,
    Proto = case State#state.dst of
		Dst when is_record(Dst, sipdst) ->
		    Dst#sipdst.proto;
		_ ->
		    logger:log(error, "~s: Client transaction dst is malformed, defaulting to UDP SIP "
			       "protocol for fake response ~p ~s", [LogTag, Status, Reason]),
		    udp
	    end,
    logger:log(debug, "~s: ~s ~s - pretend we got an '~p ~s' (and enter SIP state terminated)",
	       [LogTag, Method, sipurl:print(URI), Status, Reason]),
    FakeResponse = siprequest:make_response(Status, Reason, "", [], [], Proto, Request),
    ReportTo = State#state.report_to,
    case util:safe_is_process_alive(ReportTo) of
	{true, ReportTo} ->
	    logger:log(debug, "~s: Client transaction forwarding fake response '~p ~s' to ~p",
		       [LogTag, Status, Reason, ReportTo]),
	    Branch = State#state.branch,
	    ReportTo ! {branch_result, Branch, terminated, FakeResponse};
	{false, _} ->
	    logger:log(debug, "~s: Client transaction orphaned ('~p' not alive) - not sending "
		       "fake response '~p ~s' to anyone", [LogTag, ReportTo, Status, Reason])
    end,
    NewState1 = State#state{response=FakeResponse},
    NewState = terminate_transaction(NewState1),
    NewState.

%%--------------------------------------------------------------------
%% Function: fake_request_response(Status, Reason, State)
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           State  = state record()
%% Descrip.: Fake receiving an '408 Request Terminated' response. See
%%           comments for fake_request_response/3 above for more info.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
fake_request_timeout(State) when is_record(State, state) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = State#state.logtag,
    logger:log(normal, "~s: Client transaction (~s ~s) timed out - pretend we got a '408 Request Timeout' response",
	       [LogTag, Method, sipurl:print(URI)]),
    %% Create a fake 408 Request Timeout response to store as EndResult for this Target
    %% since RFC 3261 16.7 says we MUST act as if such a response was received
    NewState = fake_request_response(408, "Request Timeout", State),
    NewState.

%%--------------------------------------------------------------------
%% Function: end_invite(State)
%%           State = state record()
%% Descrip.: Ends an INVITE transaction. This is called when our user
%%           specified timeout occurs, or when Timer C fires.
%%           Whichever happens first.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
end_invite(State) when is_record(State, state) ->
    Request = State#state.request,
    URI = Request#request.uri,
    SipState = State#state.sipstate,
    LogTag = State#state.logtag,
    case SipState of
	calling ->
	    fake_request_timeout(State);
	proceeding ->
	    logger:log(debug, "~s: Sending of request (INVITE ~s) timed out in state 'proceeding' - cancel request" ++
		       "(by starting a separate CANCEL transaction)", [LogTag, sipurl:print(URI)]),
	    cancel_request(State);
	_ ->
	    logger:log(debug, "~s: Sending of request (INVITE ~s) timed out in state '~s' - ignoring.",
		       [LogTag, sipurl:print(URI), SipState]),
	    State
    end.

%%--------------------------------------------------------------------
%% Function: terminate_transaction(State)
%%           State = state record()
%% Descrip.: Cancel all siptimers and go into SIP state 'terminated'.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
terminate_transaction(State) when is_record(State, state) ->
    NewTimerList = siptimer:cancel_all_timers(State#state.timerlist),
    NewState = State#state{sipstate=terminated, timerlist=NewTimerList},
    NewState.

%%--------------------------------------------------------------------
%% Function: cancel_request(State)
%%           State = state record()
%% Descrip.: We have been asked to cancel. Cancel is tricky business,
%%           if we are still in state 'calling' we have to just mark
%%           ourselves as cancelled and then wait for a 1xx to arrive
%%           etc. etc. Anyways, initiate cancel in whatever way we
%%           can do it at this time.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
cancel_request(State) when is_record(State, state) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = State#state.logtag,
    NewState1 = State#state{cancelled=true},
    logger:log(debug, "~s: Marked transaction as cancelled", [LogTag]),
    case Method of
	"INVITE" ->
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
		    logger:log(debug, "~s: NOT starting CANCEL transaction for request (INVITE ~s) "
			       "since we are in state '~p'", [LogTag, sipurl:print(URI), SipState]),
		    NewState1
	    end;
	_ ->
	    %% RFC 3261 9.1 says a stateful proxy SHOULD NOT send CANCEL of non-INVITE requests.
	    %% We enter 'completed' state to collect any final responses that might arrive.
	    logger:log(debug, "~s: Request was non-INVITE (~s) - not starting CANCEL transaction. "
		       "Going into 'completed' state.", [LogTag, Method]),
	    %% Terminate any resend request timers, and set up TimerK to fire in default 5 seconds,
	    %% ending the transaction. RFC3261 does not really describe non-INVITE transactions being
	    %% cancelled since they does not match the RFC definition of 'pending' (unless they have
	    %% received provisional responses) but this should be the best way to handle it. Note that
	    %% we don't check if it was reliable transport or not when deciding the timeout for Timer K
	    %% here, since the purpose is to collect any late responses.
	    NewState2 = stop_resendrequest_timer(NewState1),
	    TimerK = sipserver:get_env(timerT4, 5000),
	    KDesc = "terminate client transaction " ++ Method ++ " " ++ sipurl:print(URI) ++ " (Timer K)",
	    NewState3 = add_timer(TimerK, KDesc, {terminate_transaction}, NewState2),
	    NewState3#state{sipstate=completed}
    end.

%%--------------------------------------------------------------------
%% Function: start_cancel_transaction(State)
%%           State = state record()
%% Descrip.: Start fire-and-forget CANCEL transaction for this client
%%           transaction. Note that this is done by starting _another_
%%           client transaction process. We will know when _that_
%%           transaction has finished because _this_ transaction will
%%           then receive a '487 Request Cancelled' response.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
start_cancel_transaction(State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    {URI, Header} = {Request#request.uri, Request#request.header},
    logger:log(debug, "~s: initiate_cancel() Sending CANCEL ~s", [LogTag, sipurl:print(URI)]),
    {CSeqNum, _} = sipheader:cseq(Header),
    CancelId = {CSeqNum, "CANCEL"},
    %% Delete all Via headers. initiate_request() uses send_proxy_request() which
    %% will add one for this proxy, and that is the only one that should be in a
    %% CANCEL. Set CSeq method to CANCEL and delete all Require and Proxy-Require
    %% headers. All this according to RFC 3261 9.1 Client Behaviour.
    CancelHeader1 = keylist:set("CSeq", [sipheader:cseq_print(CancelId)],
				Header),
    CancelHeader2 = keylist:delete('via', CancelHeader1),
    CancelHeader3 = keylist:delete('require', CancelHeader2),
    CancelHeader4 = keylist:delete('proxy-require', CancelHeader3),
    CancelHeader5 = keylist:delete('content-type', CancelHeader4),
    CancelHeader6 = keylist:delete('content-length', CancelHeader5),
    CancelHeader = keylist:set("Content-Length", ["0"], CancelHeader6),
    CancelRequest = siprequest:set_request_body(#request{method="CANCEL", uri=URI, header=CancelHeader}, <<>>),
    T1 = sipserver:get_env(timerT1, 500),
    NewState = add_timer(64 * T1, "quit after CANCEL", {terminate_transaction}, State),
    Socket = State#state.socket,
    Dst = State#state.dst,
    %% Must use branch from original request sent out, so that next hop can match
    %% this ACK to the right transaction. tl_branch is not necessarily the same as branch
    %% since the transport layer can be configured to add a stateless loop cookie.
    Branch = State#state.tl_branch,
    %% XXX is the 32 * T1 correct for CANCEL?
    case transactionlayer:start_client_transaction(CancelRequest, Socket, Dst, Branch, 32 * T1, none) of
	P when is_pid(P) ->
	    logger:log(debug, "~s: Started CANCEL client transaction with pid ~p", [LogTag, P]);
	{error, E} ->
	    %% XXX what to do here?
	    logger:log(error, "~s: Failed starting CANCEL client transaction : ~p", [LogTag, E])
    end,
    NewState.

%%--------------------------------------------------------------------
%% Function: update_invite_expire(Status, State)
%%           State = state record()
%% Descrip.: When we receive provisional responses (except 100) to an
%%           INVITE we've sent, we have to reset TimerC to a value
%%           of more than three minutes (180 seconds). We reset it to
%%           whatever it was first initialized to.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
update_invite_expire(Status, State) when is_record(State, state), Status == 100 ->
    State;
update_invite_expire(Status, State) when is_record(State, state), Status >= 101, Status =< 199 ->
    LogTag = State#state.logtag,
    InviteExpireTimerList = siptimer:get_timers_appsignal_matching({invite_expire}, State#state.timerlist),
    case siptimer:get_length(InviteExpireTimerList) of
	0 ->
	    logger:log(error, "~s: Received provisional response ~p to INVITE request, but no "
		       "'invite_expire' (Timer C) timer found", [LogTag, Status]),
	    logger:log(debug, "~s: TimerList where I could not find an 'invite_expire' (Timer C) timer :~n~p",
		       [LogTag, siptimer:debugfriendly(State#state.timerlist)]),
	    State;
	_ ->
	    logger:log(debug, "~s: Received 1xx response to INVITE, resetting 'invite_expire' (Timer C)",
		       [LogTag]),
	    NewTimerList = siptimer:reset_timers(InviteExpireTimerList, State#state.timerlist),
	    NewState = State#state{timerlist=NewTimerList},
	    NewState
    end;
update_invite_expire(_Status, State) ->
    State.

%%--------------------------------------------------------------------
%% Function: ack_non_1xx_or_2xx_response_to_invite(Method, Response,
%%                                                 State)
%%           Method   = string()
%%           Response = response record()
%%           State    = state record()
%% Descrip.: Send an ACK if this is an INVITE transaction and the
%%           response received was a 3xx, 4xx, 5xx or 6xx.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
%%
%% Status >= 100, =< 299
%%
ack_non_1xx_or_2xx_response_to_invite("INVITE", #response{status=Status}=Response, State)
  when is_record(State, state), Status >= 100, Status =< 299 ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    URI = Request#request.uri,
    logger:log(debug, "~s: Not ACK-ing 1xx or 2xx response to INVITE ~s: ~p ~s",
	       [LogTag, sipurl:print(URI), Status, Response#response.reason]),
    State;
%%
%% Status >= 300, =< 699
%%
ack_non_1xx_or_2xx_response_to_invite("INVITE", #response{status=Status}=Response, State)
  when is_record(State, state), Status >= 300, Status =< 699 ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    URI = Request#request.uri,
    logger:log(debug, "~s: ACK-ing 3xx, 4xx, 5xx or 6xx response ~p ~s to INVITE ~s (in state '~p')",
	       [LogTag, Status, Response#response.reason, sipurl:print(URI), State#state.sipstate]),
    generate_ack(Response, State),
    State.

%%--------------------------------------------------------------------
%% Function: generate_ack(Response, State)
%%           Response = response record()
%%           State    = state record()
%% Descrip.: Send an ACK.
%% Returns : NewState = state record()
%% Note    : We do not set up any timers for retransmission since if
%%           this ACK is lost we will receive a retransmitted response
%%           which will effectively cause us to end up here again.
%% Note    : We will never get called to ACK a 2xx response to INVITE,
%%           as per section 13.2.2.4 (2xx Responses) of RFC3261 that
%%           is handled by the UAC core, NOT the transaction layer.
%%--------------------------------------------------------------------
%%
%% Status >= 100, =< 199
%%
generate_ack(#response{status=Status}=Response, State)
  when is_record(State, state), Status >= 100, Status =< 199 ->
    LogTag = State#state.logtag,
    %% XXX when running in UAC mode (not proxy), we might want to
    %% generate a PRACK here or should that be done by the TU (application)?
    logger:log(debug, "~s: Not ACK-ing 1xx response '~p ~s'.",
	       [LogTag, Status, Response#response.reason]);

%%
%% Status >= 200, =< 699
%%
generate_ack(#response{status=Status}=Response, State)
  when is_record(State, state), Status >= 200, Status =< 699 ->
    Request = State#state.request,
    {URI, Header} = {Request#request.uri, Request#request.header},
    RHeader = Response#response.header,
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Sending ACK of '~p ~s' in response to ~s ~s",
	       [LogTag, Status, Response#response.reason, Request#request.method, sipurl:print(URI)]),
    {CSeq, _} = sipheader:cseq(Header),
    %% Don't copy any Via headers. send_proxy_request() will add one for this proxy,
    %% and that is the only one that should be in an ACK. RFC 3261 17.1.1.3
    %% copy() To and then set() it to preserve order...
    SendHeader1 = keylist:copy(Header, ['from', 'call-id']),
    %% get To: from the response we are ACKing (to preserve To-tag)
    SendHeader2 = keylist:set("To", keylist:fetch('to', RHeader), SendHeader1),
    SendHeader3 = keylist:set("CSeq", [sipheader:cseq_print({CSeq, "ACK"})], SendHeader2),
    %% Copy Route-header from original request, if present (mandated by RFC 3261 17.1.1.3)
    SendHeader = case keylist:fetch('route', Header) of
		     [] ->
			 SendHeader3;
		     Route ->
			 keylist:set("Route", Route, SendHeader3)
		 end,
    Socket = State#state.socket,
    Dst = State#state.dst,
    %% Must use branch from original request sent out, so that next hop can match
    %% this ACK to the right transaction. tl_branch is not necessarily the same as branch
    %% since the transport layer can be configured to add a stateless loop cookie.
    Branch = State#state.tl_branch,
    ACKRequest = siprequest:set_request_body(#request{method="ACK", uri=URI, header=SendHeader}, <<>>),
    transportlayer:send_proxy_request(Socket, ACKRequest, Dst, ["branch=" ++ Branch]),
    ok.

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%% Note    : Not much is tested in this module at the moment because
%%           almost every function includes communication with the
%%           outside world (receiving or sending signals/SIP-messages)
%%           which the current test framework does not allow testing
%%           of.
%%--------------------------------------------------------------------
test() ->
    %% test received_response_state_machine/3
    %%--------------------------------------------------------------------

    %% 17.1.1 INVITE Client Transaction

    io:format("test: received_response_state_machine/3 INVITE - 1~n"),
    %% If the client transaction receives a provisional response while in
    %% the "Calling" state, it transitions to the "Proceeding" state.
    %% ... Furthermore, the provisional response MUST be passed to the TU.
    %% Yxa comment: 100 Trying is just to make us stop resending, not forwarded to TU.
    {ignore, proceeding} = received_response_state_machine("INVITE", 100, calling),
    {tell_parent, proceeding} = received_response_state_machine("INVITE", 199, calling),

    io:format("test: received_response_state_machine/3 INVITE - 2~n"),
    %% Any further provisional responses MUST be passed up to the TU while
    %% in the "Proceeding" state.
    %% Yxa comment: 100 Trying is just to make us stop resending, not forwarded to TU.
    {ignore, proceeding} = received_response_state_machine("INVITE", 100, proceeding),
    {tell_parent, proceeding} = received_response_state_machine("INVITE", 199, proceeding),

    io:format("test: received_response_state_machine/3 INVITE - 3~n"),
    %% When in either the "Calling" or "Proceeding" states, reception of a
    %% response with status code from 300-699 MUST cause the client
    %% transaction to transition to "Completed".  The client transaction
    %% MUST pass the received response up to the TU
    {tell_parent, completed} = received_response_state_machine("INVITE", 300, calling),
    {tell_parent, completed} = received_response_state_machine("INVITE", 400, calling),
    {tell_parent, completed} = received_response_state_machine("INVITE", 500, calling),
    {tell_parent, completed} = received_response_state_machine("INVITE", 600, calling),
    io:format("test: received_response_state_machine/3 INVITE - 3~n"),
    {tell_parent, completed} = received_response_state_machine("INVITE", 300, proceeding),
    {tell_parent, completed} = received_response_state_machine("INVITE", 400, proceeding),
    {tell_parent, completed} = received_response_state_machine("INVITE", 500, proceeding),
    {tell_parent, completed} = received_response_state_machine("INVITE", 600, proceeding),

    io:format("test: received_response_state_machine/3 INVITE - 4~n"),
    %% Any retransmissions of the final response that are received while in
    %% the "Completed" state MUST cause the ACK to be re-passed to the
    %% transport layer for retransmission, but the newly received response
    %% MUST NOT be passed up to the TU.
    {ignore, completed} = received_response_state_machine("INVITE", 300, completed),
    {ignore, completed} = received_response_state_machine("INVITE", 400, completed),
    {ignore, completed} = received_response_state_machine("INVITE", 500, completed),
    {ignore, completed} = received_response_state_machine("INVITE", 600, completed),
    
    io:format("test: received_response_state_machine/3 INVITE - 5~n"),
    %% When in either the "Calling" or "Proceeding" states, reception of a
    %% 2xx response MUST cause the client transaction to enter the
    %% "Terminated" state, and the response MUST be passed up to the TU.
    {tell_parent, terminated} = received_response_state_machine("INVITE", 200, calling),
    {tell_parent, terminated} = received_response_state_machine("INVITE", 200, proceeding),

    %% own conclusions

    io:format("test: received_response_state_machine/3 INVITE - 6~n"),
    {ignore, completed} = received_response_state_machine("INVITE", 100, completed),
    {ignore, completed} = received_response_state_machine("INVITE", 199, completed),

    io:format("test: received_response_state_machine/3 INVITE - 7~n"),
    %% late 2xx response, received after a non-2xx response that made us reach 'completed'
    %% XXX actually, this is really weird - someone sent us two different final responses.
    %% should we really go from 'completed' to 'terminated' or 'ignore' and stay in 'completed'?
    {tell_parent, terminated} = received_response_state_machine("INVITE", 200, completed),
    %% we shouldn't be around to reveive late responses if we are terminated, but...
    {tell_parent, terminated} = received_response_state_machine("INVITE", 200, terminated),

    io:format("test: received_response_state_machine/3 INVITE - 8~n"),
    %% when in 'completed' or 'terminated', ignore any 6xx responses. We can't undo
    %% that someone has already picked up the phone even if another branch generates
    %% a 6xx.
    {ignore, completed} = received_response_state_machine("INVITE", 600, completed),
    {ignore, terminated} = received_response_state_machine("INVITE", 600, terminated),


    %% 17.1.2 Non-INVITE Client Transaction

    io:format("test: received_response_state_machine/3 non-INVITE - 1~n"),
    %% If a provisional response is received while in the "Trying" state, the
    %% response MUST be passed to the TU, and then the client transaction
    %% SHOULD move to the "Proceeding" state.
    %% Yxa comment: 100 Trying is just to make us stop resending, not forwarded to TU.
    {ignore, proceeding} = received_response_state_machine("OPTIONS", 100, trying),
    {tell_parent, proceeding} = received_response_state_machine("OPTIONS", 199, trying),

    io:format("test: received_response_state_machine/3 non-INVITE - 2~n"),
    %% If a final response (status codes 200-699) is received while in the 
    %% "Trying" state, the response MUST be passed to the TU, and the client
    %% transaction MUST transition to the "Completed" state.
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 200, trying),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 300, trying),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 400, trying),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 500, trying),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 600, trying),

    io:format("test: received_response_state_machine/3 non-INVITE - 3~n"),
    %% If a final response (status codes 200-699) is received while in the
    %% "Proceeding" state, the response MUST be passed to the TU, and the
    %% client transaction MUST transition to the "Completed" state.
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 200, proceeding),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 300, proceeding),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 400, proceeding),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 500, proceeding),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 600, proceeding),

    %% own conclusions

    io:format("test: received_response_state_machine/3 non-INVITE - 4~n"),
    %% we are already in the completed state, the TU has already been given a final response
    {ignore, completed} = received_response_state_machine("OPTIONS", 100, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 199, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 200, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 300, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 400, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 500, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 600, completed),

    io:format("test: received_response_state_machine/3 non-INVITE - 5~n"),
    %% this is not the first provisional response we receive, we are already in 'proceeding'
    {ignore, proceeding} = received_response_state_machine("OPTIONS", 100, proceeding),
    {ignore, proceeding} = received_response_state_machine("OPTIONS", 199, proceeding),

    io:format("test: received_response_state_machine/3 - 1~n"),
    %% test too low or too high response numbers, testing with differend method is not needed
    {'EXIT', {function_clause, _}} = (catch received_response_state_machine("INVITE", 99, trying)),
    {'EXIT', {function_clause, _}} = (catch received_response_state_machine("INVITE", -1, trying)),
    {'EXIT', {function_clause, _}} = (catch received_response_state_machine("INVITE", 700, trying)),
    {'EXIT', {function_clause, _}} = (catch received_response_state_machine("INVITE", 32984397, trying)),

    %% get_request_resend_timeout/3
    %%--------------------------------------------------------------------

    io:format("test: get_request_resend_timeout/3 INVITE - 1~n"),
    %% INVITE, always just double
    1000 = get_request_resend_timeout("INVITE", 500, trying),
    2000 = get_request_resend_timeout("INVITE", 1000, trying),
    4000 = get_request_resend_timeout("INVITE", 2000, trying),
    8000 = get_request_resend_timeout("INVITE", 4000, trying),
    16000 = get_request_resend_timeout("INVITE", 8000, trying),
    32000 = get_request_resend_timeout("INVITE", 16000, trying),

    io:format("test: get_request_resend_timeout/3 non-INVITE - 1~n"),
    %% non-INVITE, state 'trying'. Resend at 500ms, 1s, 2s, 4s, 4s, 4s ...
    1000 = get_request_resend_timeout("OPTIONS", 500, trying),
    2000 = get_request_resend_timeout("OPTIONS", 1000, trying),
    4000 = get_request_resend_timeout("OPTIONS", 2000, trying),
    4000 = get_request_resend_timeout("OPTIONS", 4000, trying),

    io:format("test: get_request_resend_timeout/3 non-INVITE - 1~n"),
    %% non-INVITE, state 'proceeding'. Resend every 4s.
    4000 = get_request_resend_timeout("OPTIONS", 500, proceeding),
    4000 = get_request_resend_timeout("OPTIONS", 1000, proceeding),
    4000 = get_request_resend_timeout("OPTIONS", 2000, proceeding),
    4000 = get_request_resend_timeout("OPTIONS", 4000, proceeding),
    
    ok.
