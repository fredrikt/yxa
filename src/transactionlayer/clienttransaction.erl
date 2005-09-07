%%%-------------------------------------------------------------------
%%% File    : clienttransaction.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Client transaction.
%%%
%%%           When you start a client transaction, it will try to send
%%%           the request to the destination you have specified, and
%%%           send responses to the Pid you tell it to (Parent
%%%           argument to start()). Parent should expect to receive
%%%           the following signals :
%%%
%%%           {clienttransaction_terminating, Pid, Branch}
%%%           {branch_result, Pid, Branch, SipState, Response}
%%%
%%%               Pid      = pid(), this client transactions pid
%%%               Branch   = string(), the SIP branch used by this
%%%                          client transaction
%%%               SipState = atom(), current SIP state
%%%               Response = response record() | {Status, Reason} for
%%%                          generated responses
%%%                 Status = integer(), SIP status code
%%%                 Reason = string(), SIP reason phrase
%%%
%%%           Note : You can't assume that you will get a branch_result
%%%                  signal with a final Response and SipState =
%%%                  'terminated'.
%%%
%%%           You can tell the client transaction to terminate
%%%           gracefully by using gen_server:cast() to send it a
%%%
%%%           {cancel, Msg, ExtraHeaders} - Msg is only for logging,
%%%               ExtraHeaders is a list of {Key, ValueList} tuples
%%%               to add to the CANCEL request generated.
%%%
%%% Created : 06 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(clienttransaction).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/6,
	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  branch,	%% string(), the Via branch paramter for this client transaction
	  socket_in,	%% sipsocket record(), the socket the original request was received on
	  logtag,	%% string(), a prefix for logging
	  socket,	%% sipsocket record(), the socket the transport layer sent out our request on
	  parent,	%% pid() of our parent, must be kept to recognize exit messages from it
	  report_to,	%% pid() | none, the pid we should notify of progress ('none' in case this is
	  		%%   for example an independent CANCEL)
	  request,	%% request record(), the request we are supposed to send
	  response,	%% response record(), the last response we have received that caused a sipstate change
	  sipstate,	%% atom(), trying|calling|proceeding|completed|terminated
	  timerlist,	%% siptimerlist record(), list of timers managed by siptimer module
	  dst,		%% sipdst record(), the destination for our request
	  timeout,	%% integer(), our definite timeout before ending an INVITE - NB: only applies to INVITE
	  cancelled=false,	%% atom(), true | false - have we cancelled ourselves? that is, sent out a CANCEL
	  do_cancel=false,	%% atom(), {true, EH} | false - have we been instructed to cancel?
	  tl_branch,		%% string(), the branch the transaction layer actually used when sending our request
	  initialized=no,	%% atom(), yes | no - have we finished our initialization process
	  cancel_pid,		%% undefined | pid(), if we start a CANCEL for ourselves, this is the pid of that transaction
	  final_r_sent=false,	%% atom(), true | false - have we sent a final response to our parent yet?
	  testing = false	%% true | false, are we testing the module?
	 }).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link(Request, SocketIn, Dst, Branch, Timeout,
%%                      ReportTo)
%% Descrip.: Starts the server, see init/1 description for details.
%%--------------------------------------------------------------------
start_link(Request, SocketIn, Dst, Branch, Timeout, ReportTo) ->
    %% It is intentional to call gen_server:start(...) here even though
    %% this function is called start_link. That is because of a 'problem'
    %% with gen_servers in Erlang/OTP (at least R10B-2). If you use
    %% gen_server:start_link(...) to start your gen_server, you won't be
    %% able to trap 'EXIT' signals from the parent process, even if you
    %% set process_flag(trap_exit, true)! We set up a link to this process
    %% in the init/1 callback to achieve the same effect (although with
    %% a bitter taste).
    gen_server:start(?MODULE, [Request, SocketIn, Dst, Branch, Timeout, ReportTo, self()], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, SocketIn, Dst, Branch, Timeout, ReportTo,
%%                 Parent])
%%           Request  = request record()
%%           SocketIn = sipsocket record(), the default socket we
%%                      should give to the transport layer for this
%%                      request. XXX remove - don't do stateless.
%%           Dst      = sipdst record(), the destination for this
%%                                       client transaction
%%           Branch   = string()
%%           Timeout  = integer(), timeout for INVITE transactions
%%           ReportTo = pid() | none, where we should report events
%%           Parent   = pid(), the process that initiated this client
%% Descrip.: Initiates a client transaction handler. Set us up for
%%           an immediate timeout signal in which we will try to send
%%           out our request - to not block caller.
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([Request, SocketIn, Dst, Branch, Timeout, ReportTo, Parent])
  when is_record(Request, request), is_record(Dst, sipdst), is_list(Branch),
       is_integer(Timeout), is_pid(Parent); Parent == none ->
    RegBranch = sipheader:remove_loop_cookie(Branch),
    {Method, URI} = {Request#request.method, Request#request.uri},
    Desc = lists:flatten(
	     io_lib:format("~s: ~s ~s (dst ~s)", [RegBranch, Method,
						  sipurl:print(URI), sipdst:dst2str(Dst)])),
    LogTag = lists:concat([RegBranch, " ", Method]),
    %% Get ourselves into the transaction state list first of all
    case transactionstatelist:add_client_transaction(Method, RegBranch, self(), Desc) of
	{duplicate, _} ->
	    logger:log(error, "Transaction layer: Can't start duplicate client transaction"),
	    {stop, "Client transaction already exists"};
	ok ->
	    %% Link to transactionlayer immediately (so that it removes this transaction
	    %% from the transactionstatelist when we exit).
	    TPid = erlang:whereis(transactionlayer),
	    true = link(TPid),
	    %% Link to parent from init/1 instead of using gen_server:start_link to
	    %% be able to trap EXIT signals from parent process. See comment in start_link
	    %% above for more details.
	    true = link(Parent),
	    %% Trap exit signals so that we can cancel ongoing INVITEs if our parent dies
	    process_flag(trap_exit, true),
	    case init2([Request, SocketIn, Dst, Branch, Timeout, ReportTo, Parent, LogTag]) of
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

init2([Request, SocketIn, Dst, Branch, Timeout, ReportTo, Parent, LogTag])
  when is_record(Request, request), is_record(Dst, sipdst), is_list(Branch),
       is_integer(Timeout), is_pid(Parent); Parent == none ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    SipState = case Method of
		   "INVITE" -> calling;
		   _ -> trying
	       end,
    State = #state{branch=Branch, logtag=LogTag, socket=SocketIn, request=Request,
		   sipstate=SipState, timerlist=siptimer:empty(),
		   dst=Dst, socket_in=SocketIn, timeout=Timeout,
		   parent=Parent, report_to=ReportTo},
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
    check_quit({reply, {error, "unknown gen_server call"}, State}, From).


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
    %% We have received a response to our request.
    LogTag = State#state.logtag,
    LogLevel = if
		   Response#response.status >= 200 -> normal;
		   true -> debug
	       end,
    logger:log(LogLevel, "~s: Received response '~p ~s'", [LogTag, Response#response.status, Response#response.reason]),
    NewState = process_received_response(Response, State),
    check_quit({noreply, NewState});

%%--------------------------------------------------------------------
%% Function: handle_cast({cancel, Msg, ExtraHeaders}, State)
%%           Msg          = string()
%%           ExtraHeaders = list() of {key, value} tuple() with extra
%%                          headers that should be included in the
%%                          CANCEL we send (if we send one).
%% Descrip.: We have been asked to cancel. Do so if we are not already
%%           cancelled. 'Doing so' means start an independent CANCEL
%%           client transaction, and mark ourselves as cancelled.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_cast({cancel, Msg, _ExtraHeaders}, State=#state{cancelled=true}) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Ignoring signal to cancel (~s) when in SIP-state '~p'. I am already cancelled.",
	       [LogTag, Msg, State#state.sipstate]),
    check_quit({noreply, State});
handle_cast({cancel, Msg, ExtraHeaders}, State=#state{cancelled=false}) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Branch requested to cancel (~s) when in SIP-state '~p'",
	       [LogTag, Msg, State#state.sipstate]),
    NewState = cancel_request(State, ExtraHeaders),
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
    check_quit({stop, normal, State});

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Unknown cast.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    LogTag = State#state.logtag,
    logger:log(error, "~s: Client transaction received unknown gen_server cast :~n~p",
	       [LogTag, Msg]),
    check_quit({noreply, State}).


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
		logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring.", [LogTag, TRef, TDesc]),
		logger:log(debug, "~s: Unknown timer (~p:~p) fired! Ignoring. SIP state '~p', Timerlist :~n~p",
			   [LogTag, TRef, TDesc, State#state.sipstate, State#state.timerlist]),
		State;
	    Timer ->
		case process_timer(Timer, State) of
		    NewState1 when is_record(NewState1, state) ->
			NewState1;
		    Unknown ->
			logger:log(error, "~s: Client transaction: process_timer() returned unknown result :~n~p",
				   [LogTag, Unknown]),
			erlang:error("clienttransaction: process_timer/2 returned unknown data")
		end
	end,
    check_quit({noreply, NewState});

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%% Descrip.: Continuation of init/1 but executing after init/1 has
%%           returned, so the caller (the transactionlayer) is not
%%           blocked. Invoke initiate_request/1 to start trying to
%%           send our request to it's destination.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_info(timeout, #state{initialized=no}=State) ->
    check_quit({noreply, initiate_request(State#state{initialized=yes})});

%%--------------------------------------------------------------------
%% Function: handle_info({'EXIT', Pid, Reason}, State)
%%           Pid    = pid()
%%           Reason = normal | term()
%% Descrip.: Handle exit signals from the processes we are linked to.
%%           If we are an INVITE transaction and our parent exits with
%%           anything other than 'normal', we cancel ourselves if we
%%           haven't completed yet.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, #state{parent=Parent}=State) when Pid == Parent ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    Method = Request#request.method,
    NewState1 = case should_cancel_on_parent_exit(Method, State) of
		    true ->
			URI = Request#request.uri,
			logger:log(normal, "~s: My parent (~p) exited, cancelling myself (~s ~p)",
				   [LogTag, Parent, Method, sipurl:print(URI)]),
			cancel_request(State);
		    false ->
			logger:log(debug, "~s: My parent (~p) exited, but this client transaction can't "
				   "(or shouldn't) be cancelled now (my state is ~p, cancelled=~p)",
				   [LogTag, Parent, State#state.sipstate, State#state.cancelled]),
			State
		end,

    %% clear report_to if it was the same as our parent (most oftenly is)
    NewState2 = case State#state.report_to of
		   Parent ->
		       NewState1#state{report_to=none};
		   _ ->
		       NewState1
	       end,

    %% Log reason for parent exit, if it wasn't 'normal'
    case Reason of
	normal -> ok;
	_ ->
	    logger:log(debug, "~s: Parent ~p exit reason : ~p", [LogTag, Parent, Reason])
    end,

    check_quit({noreply, NewState2});

handle_info({'EXIT', Pid, normal}, #state{cancel_pid=CancelPid}=State) when Pid == CancelPid ->
    %% the CANCEL client transaction we had started has now exited, just ignore signal
    check_quit({noreply, State});

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
    LogTag = State#state.logtag,
    ReportTo = State#state.report_to,
    case util:safe_is_process_alive(ReportTo) of
	{true, ReportTo} ->
	    logger:log(debug, "~s: Informing my parent (~p) that I am terminating now",
		       [LogTag, ReportTo]),
	    ReportTo ! {clienttransaction_terminating, self(), State#state.branch};
	{false, _} ->
	    logger:log(debug, "~s: Client transaction orphaned ('~p' not alive) - can't "
		       "inform parent that I am terminating now", [LogTag, ReportTo])
    end,
    Reason.


%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: check_quit(Res)
%%           check_quit(Res, From)
%%           Res  = term(), gen_server:call/cast/info() return value
%%           From = term(), gen_server from-value | none
%% Descrip.: Extract the state record() from Res, and check if it's
%%           sipstate is 'terminated'. If it is then turn Res into a
%%           stop signal, but if Res was {reply, ...} execute a
%%           gen_server:reply() first.
%% Returns : {noreply, State}          |
%%           {stop, Reason, State}            (terminate/2 is called)
%% Note    : Not all variants of gen_server call/cast/info return
%%           values are covered in these functions - only the ones we
%%           actually use!
%%--------------------------------------------------------------------
check_quit(Res) ->
    check_quit(Res, none).

%% Before returing a cast() reply, check if the SIP-state is terminated and
%% if so return {stop ...}
check_quit(Res, From) ->
    case Res of
	{stop, _Reason, State} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	{reply, _Reply, State} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	{noreply, State} when is_record(State, state) ->
	    check_quit2(Res, From, State)
    end.

check_quit2(Res, From, #state{sipstate=terminated}=State) ->
    RStr = case State#state.response of
	       Re when is_record(Re, response) ->
		   io_lib:format("last received response was ~p ~s", [Re#response.status, Re#response.reason]);
	       {Status, Reason} ->
		   lists:concat(["created response ", Status, " ", Reason]);
	       undefined ->
		   "no responses received"
	   end,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Client transaction (~s ~s) terminating in state '~p', ~s",
	       [LogTag, Method, sipurl:print(URI), State#state.sipstate, RStr]),
    %% If there was a reply to be sent, we must send that before changing Res into
    %% a stop. Only include clauses for variants we actually use.
    NewRes = case Res of
		 {reply, Reply, _State} ->
		     send_reply(Reply, From, State),
		     {stop, normal, State};
		 {stop, _Reason, _State} ->
		     Res;
		 _ ->
		     {stop, normal, State}
	     end,
    NewRes;
check_quit2(Res, _From, _State) ->
    %% sipstate was not 'terminated'
    Res.

send_reply(Reply, none, State) when is_record(State, state) ->
    logger:log(error, "~s: Can't send gen_server reply ~p to 'none'", [State#state.logtag, Reply]),
    error;
send_reply(Reply, To, State) when is_record(State, state) ->
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
    case (SipState == trying) or (SipState == calling) of
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
	false ->
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
    WasCancelled = case State#state.do_cancel of
		       {true, _EH} ->
			   logger:log(normal, "~s: Cancelled request '~s ~s' timed out after ~s seconds",
				      [LogTag, Method, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
			   true;
		       false ->
			   logger:log(normal, "~s: Sending of '~s ~s' timed out after ~s seconds",
				      [LogTag, Method, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
			   false
		   end,
    case State#state.sipstate of
	trying ->
	    fake_request_timeout(State);
	calling when WasCancelled == true ->
	    %% This INVITE has been cancelled, but we never received a provisional
	    %% response so we give up now and pretend we received a '487 Request Terminated'
	    fake_request_response(487, "Request Terminated", State);
	calling ->
	    fake_request_timeout(State);
	proceeding when Method == "INVITE" ->
	    %% XXX Should we ever end up here? RFC3261 17.1.1 does not specify any action for
	    %% Timer B for INVITE client transactions except in the 'calling' state.
	    logger:log(debug, "~s: Sending of request 'INVITE ~s' timed out in state 'proceeding' - "
		       "cancel myself", [LogTag, sipurl:print(URI)]),
	    %% XXX should we generate a '408 Request Timeout' for our parent (transaction user),
	    %% similar to what we do when our {invite_timeout} timer fires?
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
    logger:log(debug, "~s: Request 'INVITE ~s' timeout after ~s seconds",
	       [LogTag, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
    NewState1 =
	case State#state.final_r_sent of
	    true ->
		%% belts and straps, don't overwrite the response in State if
		%% we have sent a response already, and don't generate an extra
		%% {branch_result, ...} message for our parent
		State;
	    false ->
		NewState2 = State#state{response = {408, "Request Timeout"}},
		perform_branchaction(tell_parent, NewState2)
	end,
    end_invite(NewState1);

%%--------------------------------------------------------------------
%% Function: process_timer2({invite_expire}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: The extra timeout for INVITE (Timer C) has fired. End
%%           this INVITE transaction. This is unlikely to happen,
%%           because the invite_timeout timeout will most likely be
%%           shorter than Timer C, but RFC3261 stipulates Timer C.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({invite_expire}, _Timer, State) when is_record(State, state) ->
    %% end_invite/1 will fake receiving an 408 Request Timeout if we are in sipstate 'calling',
    %% start a CANCEL transaction if we are in state 'proceeding' or don't do anything if we
    %% are in state 'completed' or 'terminated'
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
    {ok, T2} = yxa_config:get_env(timerT2),
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
    Method = (State#state.request)#request.method,
    Status = Response#response.status,
    %% Stop resending of request as soon as possible.
    NewState1 = stop_resendrequest_timer(State),
    NewState2 =
	case Method of
	    "INVITE" when Status >= 100, Status =< 199 ->
		%% Cancel/reset INVITE request expire timer when receiving provisional responses
		update_invite_expire(Status, NewState1);
	    "INVITE" when Status >= 300, Status =< 699  ->
		%% ACK any 3/4/5/6xx responses if they are responses to an INVITE
		%% and we are in either 'calling' or 'proceeding' state
		ack_response_to_invite(Response, NewState1);
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
	    logger:log(debug, "~s: Evaluated '~p ~s' (in response to ~s ~s). Remaining in state '~p', action '~p'.",
		       [LogTag, Status, Reason, Method, sipurl:print(URI), OldSipState, BranchAction]),
	    %% Update response so that we report exactly this response to our parent and not any previous
	    %% (for example, report a previous 183 instead of this new one)
	    State#state{response=Response};
	_ ->
	    logger:log(debug, "~s: Evaluated '~p ~s' (in response to ~s ~s). Changing state from '~p' "
		       "to '~p', action '~p'.", [LogTag, Status, Reason, Method, sipurl:print(URI),
						 OldSipState, NewSipState, BranchAction]),
	    State#state{sipstate=NewSipState, response=Response}
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
  when is_atom(OldSipState), is_atom(NewSipState), is_atom(BranchAction),
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
	    case State#state.do_cancel of
		{true, ExtraHeaders} ->
		    Response = State#state.response,
		    {Status, Reason} = {Response#response.status, Response#response.reason},
		    logger:log(debug, "~s: A previously cancelled transaction (INVITE ~s) "
			       "entered state 'proceeding' upon receiving a '~p ~s' response. "
			       "CANCEL ourselves!", [State#state.logtag, sipurl:print(URI), Status, Reason]),
		    NewState1 = State#state{do_cancel=false},
		    NewState2 = cancel_request(NewState1, ExtraHeaders),
		    {NewState2, ignore};
		false ->
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
    NewTimerList2 = siptimer:cancel_timers_with_appsignal({invite_timeout}, NewTimerList1),
    NewState1 = State#state{timerlist=NewTimerList2},
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
	    false ->
		case Method of
		    "INVITE" ->
			TimerD = 32 * 1000,
			DDesc = "terminate client transaction INVITE " ++ sipurl:print(URI) ++ " (Timer D)",
			add_timer(TimerD, DDesc, {terminate_transaction}, NewState1);
		    _ ->
			{ok, TimerK} = yxa_config:get_env(timerT4),
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
    {Status, Reason} = case Response of
			   _ when is_record(Response, response) ->
			       {Response#response.status, Response#response.reason};
			   {Status1, Reason1} when is_integer(Status1), is_list(Reason1) ->
			       Response
		       end,
    ReportTo = State#state.report_to,
    case util:safe_is_process_alive(ReportTo) of
	{true, ReportTo} when is_pid(ReportTo) ->
	    IsFinalResponse = (Status >= 200),
	    case (IsFinalResponse == true) and (State#state.final_r_sent == true) of
		true ->
		    logger:log(debug, "~s: Not telling parent ~p about response '~p ~s' since we "
			       "have already sent a final response to our parent",
			       [LogTag, ReportTo, Status, Reason]),
		    State;
		false ->
		    logger:log(debug, "~s: Client transaction telling parent ~p about response '~p ~s'",
			       [LogTag, ReportTo, Status, Reason]),
		    Branch = State#state.branch,
		    SipState = State#state.sipstate,
		    ReportTo ! {branch_result, self(), Branch, SipState, Response},
		    if
			IsFinalResponse, State#state.testing == false ->
			    %% Make event out of final response
			    L = [{dst, sipdst:dst2str(State#state.dst)}],
			    event_handler:uac_result(Branch, Status, Reason, L),
			    State#state{final_r_sent = true};
			IsFinalResponse, State#state.testing == true ->
			    State#state{final_r_sent = true};
			true ->
			    State
		    end
	    end;
	{false, undefined} ->
	    %% ReportTo == undefined, we never had a parent (typically this was a
	    %% fire-and-forget CANCEL transaction)
	    logger:log(debug, "~s: Independent client transaction terminating, final response : '~p ~s'",
		       [LogTag, Status, Reason]),
	    State;
	{false, _} ->
	    logger:log(error, "~s: Client transaction orphaned ('~p' not alive) - not sending "
		       "response '~p ~s' to anyone", [LogTag, ReportTo, Status, Reason]),
	    State
    end.

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
    logger:log(debug, "UAC decision: Received 3/4/5xx response ~p to ~s when in state 'trying', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine("INVITE", Status, calling) when Status >= 300, Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3/4/5xx response ~p to INVITE when in state 'calling', " ++
	       "entering state 'completed' and telling parent", [Status]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, proceeding) when Status >= 300, Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3/4/5xx response ~p to ~s when in state 'proceeding', " ++
	       "entering state 'completed' and telling parent", [Status, Method]),
    {tell_parent, completed};
received_response_state_machine(Method, Status, State) when Status >= 300, Status =< 599 ->
    logger:log(debug, "UAC decision: Received 3/4/5xx response ~p to ~s when in state '~p', " ++
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
%% Descrip.: Locate and stop any {resendrequest} (and possibly
%%           {resendrequest_timeout}) siptimers.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
stop_resendrequest_timer(State) when is_record(State, state) ->
    Request = State#state.request,
    Method = Request#request.method,
    NTList1 = siptimer:cancel_timers_with_appsignal({resendrequest}, State#state.timerlist),

    %% If it is an INVITE and we are not waiting to send out a CANCEL, we stop
    %% the resendrequest_timeout as well since an INVITE has {invite_expire} (Timer C)
    %% that takes care of ending it if nothing more is received.
    Cancelling = case State#state.do_cancel of
		     {true, _EH} -> true;
		     false -> false
		 end,
    NTList2 = case {Method, Cancelling} of
		  {"INVITE", false} ->
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
	    {ok, T1} = yxa_config:get_env(timerT1),
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
	    fake_request_response(503, "Service Unavailable", State)
    end.

get_initial_resend_timer(Socket, T1) ->
    case sipsocket:is_reliable_transport(Socket) of
	true -> 0;
	false -> T1
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
    logger:log(debug, "~s: ~s ~s - pretend we got an '~p ~s' (and enter SIP state terminated)",
	       [LogTag, Method, sipurl:print(URI), Status, Reason]),
    NewState1 = State#state{response = {Status, Reason}},
    NewState2 = terminate_transaction(NewState1),
    perform_branchaction(tell_parent, NewState2).

%%--------------------------------------------------------------------
%% Function: fake_request_timeout(Status, Reason, State)
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           State  = state record()
%% Descrip.: Fake receiving an '408 Request Timeout' response. See
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
%%           specified timeout (timer invite_timeout) occurs, or when
%%           Timer C fires. Whichever happens first.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
end_invite(#state{cancelled=true}=State) ->
    logger:log(debug, "~s: End invite: Not doing anything since request is already cancelled.",
	       [State#state.logtag]),
    State;
end_invite(State) when is_record(State, state) ->
    Request = State#state.request,
    URI = Request#request.uri,
    LogTag = State#state.logtag,
    case State#state.sipstate of
	calling ->
	    fake_request_timeout(State);
	proceeding ->
	    logger:log(debug, "~s: Sending of request (INVITE ~s) timed out in state 'proceeding' - cancel request "
		       "(by starting a separate CANCEL transaction)", [LogTag, sipurl:print(URI)]),
	    cancel_request(State);
	SipState when SipState == completed; SipState == terminated ->
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
%%           cancel_request(State, ExtraHeaders)
%%           State        = state record()
%%           ExtraHeaders = list() of {key, value} tuple() with extra
%%                          headers that should be included in the
%%                          CANCEL we send (if we send one).
%% Descrip.: We have been asked to cancel. Cancel is tricky business,
%%           if we are still in state 'calling' we have to just mark
%%           ourselves as cancelled and then wait for a 1xx to arrive
%%           etc. etc. Anyways, initiate cancel in whatever way we
%%           can do it at this time.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
cancel_request(State) when is_record(State, state) ->
    cancel_request(State, []).

cancel_request(#state{cancelled=true}=State, _ExtraHeaders) ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    logger:log(debug, "~s: NOT starting CANCEL transaction for request (~s ~s) "
	       "since we are already cancelled", [LogTag, Method, sipurl:print(URI)]),
    State;
cancel_request(State, ExtraHeaders) when is_record(State, state), is_list(ExtraHeaders) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = State#state.logtag,
    NewState1 = State#state{cancelled=true},
    logger:log(debug, "~s: Marked transaction as cancelled", [LogTag]),
    case Method of
	"INVITE" ->
	    case NewState1#state.sipstate of
		calling ->
		    logger:log(debug, "~s: Stopping resend of INVITE ~s, but NOT starting CANCEL client transaction "
			       "right now since we are in state 'calling'", [LogTag, sipurl:print(URI)]),
		    NewState2 = NewState1#state{do_cancel={true, ExtraHeaders}},
		    NewState3 = stop_resendrequest_timer(NewState2),
		    NewState3;
		proceeding ->
		    logger:log(debug, "~s: Stopping resends of INVITE ~s and starting a CANCEL client transaction",
			       [LogTag, sipurl:print(URI)]),
		    NewState2 = stop_resendrequest_timer(NewState1),
		    NewState3 = start_cancel_transaction(NewState2),
		    NewState3;
		SipState when SipState == completed; SipState == terminated ->
		    logger:log(debug, "~s: NOT starting CANCEL transaction for request (INVITE ~s) "
			       "since we are in state '~p'", [LogTag, sipurl:print(URI), SipState]),
		    NewState1
	    end;
	_ ->
	    %% RFC 3261 9.1 says a stateful proxy SHOULD NOT send CANCEL of non-INVITE requests.
	    %% We enter 'completed' state to collect any final responses that might arrive.
	    %% XXX what if no final responses arrive? What will be our final response in that case?
	    %% XXX exactly what happens with final responses arrived when we are in state 'completed'?
	    logger:log(debug, "~s: Request was non-INVITE (~s) - not starting CANCEL transaction. "
		       "Going into 'completed' state.", [LogTag, Method]),
	    %% Terminate any resend request timers, and set up TimerK to fire in default 5 seconds,
	    %% ending the transaction. RFC3261 does not really describe non-INVITE transactions being
	    %% cancelled since they does not match the RFC definition of 'pending' (unless they have
	    %% received provisional responses) but this should be the best way to handle it. Note that
	    %% we don't check if it was reliable transport or not when deciding the timeout for Timer K
	    %% here, since the purpose is to collect any late responses.
	    NewState2 = stop_resendrequest_timer(NewState1),
	    {ok, TimerK} = yxa_config:get_env(timerT4),
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
%% Returns : NewState = state record() |
%%           {ok, testing, ...}          this is for unit testing only
%%--------------------------------------------------------------------
start_cancel_transaction(State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    logger:log(debug, "~s: Starting new client transaction: CANCEL ~s",
	       [LogTag, sipurl:print(Request#request.uri)]),
    CancelRequest = create_cancel_request(Request),

    {ok, T1} = yxa_config:get_env(timerT1),
    NewState = add_timer(64 * T1, "quit after CANCEL", {terminate_transaction}, State),
    Socket = State#state.socket,
    Dst = State#state.dst,
    %% Must use branch from original request sent out, so that next hop can match this
    %% CANCEL to the right transaction. tl_branch is not necessarily the same as branch
    %% since the transport layer can be configured to add a stateless loop cookie.
    Branch = State#state.tl_branch,
    %% XXX is the 32 * T1 correct for CANCEL?
    case State#state.testing of
	true ->
	    {ok, testing, NewState#state{cancel_pid = testing},
	     [CancelRequest, Socket, Dst, Branch, 32 * T1, none]};
	false ->
	    case transactionlayer:start_client_transaction(CancelRequest, Socket, Dst, Branch, 32 * T1, none) of
		P when is_pid(P) ->
		    logger:log(debug, "~s: Started CANCEL client transaction with pid ~p", [LogTag, P]),
		    NewState#state{cancel_pid=P};
		{error, E} ->
		    %% XXX what to do here?
		    logger:log(error, "~s: Failed starting CANCEL client transaction : ~p", [LogTag, E]),
		    NewState
	    end
    end.

%% part of start_cancel_transaction/1
create_cancel_request(Request) ->
    {URI, Header} = {Request#request.uri, Request#request.header},
    {CSeqNum, _} = sipheader:cseq(Header),
    CancelId = {CSeqNum, "CANCEL"},
    %% Delete all Via headers. The new client transaction will use send_proxy_request() which
    %% will add one for this proxy, and that is the only one that should be in a CANCEL.
    %% Set CSeq method to CANCEL and delete all Require and Proxy-Require headers. All this
    %% according to RFC3261 #9.1 (Client Behaviour).
    CancelHeader1 = keylist:set("CSeq", [sipheader:cseq_print(CancelId)],
				Header),
    CancelHeader2 = keylist:delete('via', CancelHeader1),
    CancelHeader3 = keylist:delete('require', CancelHeader2),
    CancelHeader4 = keylist:delete('proxy-require', CancelHeader3),
    CancelHeader5 = keylist:delete('content-type', CancelHeader4),
    siprequest:set_request_body(#request{method="CANCEL", uri=URI, header=CancelHeader5}, <<>>).

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
    case siptimer:get_timers_appsignal_matching({invite_expire}, State#state.timerlist) of
	[] ->
	    logger:log(error, "~s: Received 1xx response ~p to INVITE request, but no "
		       "'invite_expire' (Timer C) timer found", [LogTag, Status]),
	    logger:log(debug, "~s: TimerList where I could not find an 'invite_expire' (Timer C) timer :~n~p",
		       [LogTag, siptimer:debugfriendly(State#state.timerlist)]),
	    State;
	InviteExpireTimerList when is_list(InviteExpireTimerList) ->
	    logger:log(debug, "~s: Received 1xx response to INVITE, resetting 'invite_expire' (Timer C)",
		       [LogTag]),
	    NewTimerList = siptimer:reset_timers(InviteExpireTimerList, State#state.timerlist),
	    NewState = State#state{timerlist=NewTimerList},
	    NewState
    end.

%%--------------------------------------------------------------------
%% Function: ack_response_to_invite(Method, Response, State)
%%           Method   = string()
%%           Response = response record()
%%           State    = state record()
%% Descrip.: Send an ACK if this is an INVITE transaction and the
%%           response received was a 3/4/5/6xx.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
ack_response_to_invite(#response{status=Status}=Response, State)
  when is_record(State, state), Status >= 300, Status =< 699 ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    URI = Request#request.uri,
    logger:log(debug, "~s: ACK-ing 3/4/5/6xx response '~p ~s' to INVITE ~s (in state '~p')",
	       [LogTag, Status, Response#response.reason, sipurl:print(URI), State#state.sipstate]),
    generate_ack(Response, State),
    State.

%%--------------------------------------------------------------------
%% Function: generate_ack(Response, State)
%%           Response = response record()
%%           State    = state record()
%% Descrip.: Send an ACK. We only do this to non-2xx final responses
%%           when our original request was an INVITE. This function
%%           will not be called unless this is an INVITE UAC.
%% Returns : NewState = state record()
%% Note    : We do not set up any timers for retransmission since if
%%           this ACK is lost we will receive a retransmitted response
%%           which will effectively cause us to end up here again.
%% Note    : We will never get called to ACK a 2xx response to INVITE,
%%           as per section 13.2.2.4 (2xx Responses) of RFC3261 that
%%           is handled by the UAC core, NOT the transaction layer.
%%--------------------------------------------------------------------
generate_ack(#response{status=Status}=Response, State)
  when is_record(State, state), Status >= 300, Status =< 699 ->
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

%%--------------------------------------------------------------------
%% Function: should_cancel_on_parent_exit(Method, State)
%%           Method = list(), our SIP request method
%%           State  = state record()
%% Descrip.: Check if an abnormal exit of our parent should lead to
%%           us cancelling now.
%% Returns : true | false
%% Note    : Perhaps we shouldn't require our parent to not crash if
%%           report_to is not the same as parent? XXX
%%--------------------------------------------------------------------
should_cancel_on_parent_exit("INVITE", #state{sipstate=SipState, cancelled=false, do_cancel=false})
  when SipState == proceeding; SipState == calling ->
    true;
should_cancel_on_parent_exit(_Method, State) when is_record(State, state) ->
    false.

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
    Test_Request =
	#request{method = "INVITE",
		 uri    = sipurl:parse("sip:user@192.0.2.27"),
		 header = keylist:from_list([{"From",    ["Test <sip:test@example.org>;tag=f-tag"]},
					     {"To",      ["Receiver <sip:recv@example.org>"]},
					     {"CSeq",    ["1 INVITE"]},
					     {"Call-Id", ["truly-random"]}
					    ]),
		 body   = <<"sdp">>
		},

    Test_Me = lists:concat([siprequest:myhostname(), ":", sipsocket:default_port(yxa_test, none)]),
    Test_Response =
	#response{status = 100,
		  reason = "Testing",
		  header = keylist:from_list([{"Via",     ["SIP/2.0/YXA-TEST " ++ Test_Me,
							   "SIP/2.0/YXA-TEST 192.0.2.27"]},
					      {"From",    ["Test <sip:test@example.org>;tag=f-tag"]},
					      {"To",      ["Receiver <sip:recv@example.org>;tag=t-tag"]},
					      {"CSeq",    ["1 INVITE"]},
					      {"Call-Id", ["truly-random"]}
					     ]),
		  body   = <<>>
		 },
    Test_Dst = #sipdst{proto = yxa_test,
		       addr  = "192.0.2.27",
		       port  = 6050,
		       uri   = Test_Request#request.uri
		      },

    %% unknown handle_call
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "unknown gen_server:call() - 1"),
    UnknownCallState = #state{},
    {reply, {error, "unknown gen_server call"}, UnknownCallState} =
	handle_call(undefined, none, UnknownCallState),


    %% handle_cast({sipmessage, Response, Origin, LogStr}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {sipmessage, ...} - 1.1"),
    SipMessage_State1 = #state{request   = Test_Request,
			       sipstate  = calling,
			       timerlist = siptimer:empty()
			      },
    {noreply, SipMessage_State1_out} =
	handle_cast({sipmessage, Test_Response, #siporigin{}, "testing"}, SipMessage_State1),

    autotest:mark(?LINE, "gen_server:cast() {sipmessage, ...} - 1.2"),
    %% verify new state
    proceeding = SipMessage_State1_out#state.sipstate,
    100 = (SipMessage_State1_out#state.response)#response.status,
    "Testing" = (SipMessage_State1_out#state.response)#response.reason,
    SipMessage_State1 = SipMessage_State1_out#state{sipstate = calling,
						    response = undefined
						   },


    %% handle_cast({cancel, Msg, ExtraHeaders}
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {cancel, ...} - 1"),
    %% test already cancelled, no change expected
    Cancel_State1 = #state{cancelled = true},
    {noreply, Cancel_State1} = handle_cast({cancel, "testing", []}, Cancel_State1),

    autotest:mark(?LINE, "gen_server:cast() {cancel, ...} - 2.1"),
    %% test normal case, expect that a cancel transaction won't get started since
    %% we are in SIP-state 'completed'
    Cancel_State2 = #state{cancelled = false,
			   sipstate  = completed,
			   request   = Test_Request
			  },
    {noreply, Cancel_State2_out} = handle_cast({cancel, "testing", []}, Cancel_State2),

    autotest:mark(?LINE, "gen_server:cast() {cancel, ...} - 2.2"),
    %% verify new state
    true = Cancel_State2_out#state.cancelled,
    Cancel_State2 = Cancel_State2_out#state{cancelled = false},


    %% handle_cast({expired}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {expired} - 1"),
    ExpiredState = #state{logtag = "testing"},
    {stop, "Client transaction expired", ExpiredState} = handle_cast({expired}, ExpiredState),


    %% handle_cast({quit}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {quit} - 1"),
    QuitState = #state{logtag = "testing"},
    {stop, normal, QuitState} = handle_cast({quit}, QuitState),


    %% unknown handle_cast
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "unknown gen_server:cast() - 1"),
    UnknownCastState = #state{},
    {noreply, UnknownCastState} = handle_cast(undefined, UnknownCastState),


    %% handle_info({siptimer, TRef, TDesc}, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server signal '{siptimer, ...}' - 0"),
    {ok, TimerTest_Signal, SipTimerTest_Timerlist1} = test_get_timer({terminate_transaction}),

    TimerTest_State = #state{timerlist = SipTimerTest_Timerlist1,
			     request   = Test_Request
			    },

    autotest:mark(?LINE, "gen_server signal '{siptimer, ...}' - 1"),
    %% test normal case, the transaction should terminate
    {stop, normal, #state{sipstate = terminated}} = handle_info(TimerTest_Signal, TimerTest_State),

    autotest:mark(?LINE, "gen_server signal '{siptimer, ...}' - 2"),
    %% test with unknown timer
    {noreply, TimerTest_State} =
	handle_info({siptimer, make_ref(), "Testing unknown timer"}, TimerTest_State),


    %% handle_info(timeout, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server signal 'timeout' - 0"),
    TimeoutSignal_State = #state{logtag       = "testing",
				 branch       = "branch",
				 request      = Test_Request,
				 dst          = Test_Dst,
				 socket_in    = #sipsocket{proto = yxa_test,
							   module = sipsocket_test
							  },
				 timerlist    = siptimer:empty(),
				 report_to    = self(),
				 timeout      = 40,
				 sipstate     = calling,
				 initialized  = no,
				 testing      = true
				},

    autotest:mark(?LINE, "gen_server signal 'timeout' - 1.0"),
    %% test that we terminate immediately if this attempt to send the request fails
    put({sipsocket_test, send_result}, {error, "testing"}),
    {stop, normal, _TimeoutSignal_State_out} = handle_info(timeout, TimeoutSignal_State), 
    erase({sipsocket_test, send_result}),

    %% clear fake-sent-request message from process mailbox
    test_verify_request_was_sent(Test_Request, "gen_server signal 'timeout'", 1, 1),
    


    %% handle_info({'EXIT', Pid, Reason}, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 0"),
    ExitSignal_State = #state{logtag    = "testing",
			      parent    = self(),
			      report_to = self(),
			      request   = Test_Request,
			      timerlist = siptimer:empty(),
			      sipstate  = completed
			     },

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 1.1"),
    %% test normal case
    {noreply, ExitSignal_State1_out} = handle_info({'EXIT', self(), normal}, ExitSignal_State),

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 1.2"),
    %% verify new state
    none = ExitSignal_State1_out#state.report_to,
    ExitSignal_State = ExitSignal_State1_out#state{report_to = self()},

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 2"),
    %% test normal case with non-normal reason
    ExitSignal_State2 = ExitSignal_State#state{report_to = none},
    {noreply, ExitSignal_State2} = handle_info({'EXIT', self(), false}, ExitSignal_State2),

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 3.1"),
    %% test normal case which will result in this transaction 'cancelling'
    ExitSignal_State3 = ExitSignal_State#state{sipstate = calling},
    {noreply, ExitSignal_State3_out} = handle_info({'EXIT', self(), normal}, ExitSignal_State3),

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 3.2"),
    %% verify new state
    none = ExitSignal_State3_out#state.report_to,
    true = ExitSignal_State3_out#state.cancelled,
    {true, _} = ExitSignal_State3_out#state.do_cancel,
    ExitSignal_State3 = ExitSignal_State3_out#state{report_to = self(),
						    cancelled = false,
						    do_cancel = false
						   },

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 4"),
    %% test EXIT from cancel_pid, will be ignored
    ExitSignal_State4 = #state{parent = none,
			       cancel_pid = self()
			      },
    {noreply, ExitSignal_State4} = handle_info({'EXIT', self(), normal}, ExitSignal_State4),


    %% unknown handle_info
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "unknown gen_server signal - 1"),
    UnknownInfoState = #state{},
    {noreply, UnknownInfoState} = handle_info(undefined, UnknownInfoState),


    %% terminate(Reason, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "terminate/2 - 1.1"),
    %% test normal case
    TerminateState1 = #state{report_to = self(),
			     branch = "branch"
			    },
    normal = terminate(normal, TerminateState1),

    autotest:mark(?LINE, "terminate/2 - 1.2"),
    %% verify that report_to (we) got a 'clienttransaction_terminating' signal
    TerminateSelf = self(),
    receive
	{clienttransaction_terminating, TerminateSelf, "branch"} ->
	    ok
    after 0 ->
	    throw("test failed, we never got a clienttransaction_terminating signal")
    end,

    autotest:mark(?LINE, "terminate/2 - 2"),
    %% test with dead report_to
    TerminateDeadPid = spawn(fun() -> ok end),
    erlang:yield(),	%% make sure TerminateDeadPid finishes
    TerminateState2 = #state{report_to = TerminateDeadPid},
    normal = terminate(normal, TerminateState2),


    %% check_quit2(Res, From, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_quit2/3 - 1"),
    %% test Res = {reply, ...} (but From = none)
    CheckQuit_State = #state{logtag   = "testing",
			     request  = Test_Request,
			     response = Test_Response,
			     sipstate = terminated
			    },
    {stop, normal, CheckQuit_State} =
	check_quit2({reply, ok, CheckQuit_State}, none, CheckQuit_State),

    autotest:mark(?LINE, "check_quit2/3 - 2"),
    %% test {Status, Reason} response
    CheckQuit_State2 = CheckQuit_State#state{ response = {400, "Testing"} },
    {stop, normal, CheckQuit_State2} =
	check_quit2({reply, ok, CheckQuit_State2}, none, CheckQuit_State2),


    autotest:mark(?LINE, "check_quit2/3 - 3"),
    %% test Res = {stop, ...}
    {stop, true, CheckQuit_State} = check_quit2({stop, true, CheckQuit_State}, none, CheckQuit_State),


    %% process_timer2({resendrequest}, Timer, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_timer2/3 {resendrequest} - 0"),
    {ok, ResendRequestTimer_Signal, ResendRequestTimer_Timerlist1} =
	test_get_timer({resendrequest}),

    ResendRequestTimer_State =
	#state{logtag    = "testing",
	       socket    = #sipsocket{proto = yxa_test},
	       timerlist = ResendRequestTimer_Timerlist1,
	       request   = Test_Request,
	       sipstate  = calling,
	       dst       = Test_Dst,
	       branch    = "test-branch"
	      },

    autotest:mark(?LINE, "process_timer2/3 {resendrequest} - 1.1"),
    %% test normal case, the transaction should terminate
    {noreply, ResendRequestTimer_State_out} =
	handle_info(ResendRequestTimer_Signal, ResendRequestTimer_State),

    autotest:mark(?LINE, "process_timer2/3 {resendrequest} - 1.2"),
    %% cancel the revived timer
    siptimer:cancel_all_timers(ResendRequestTimer_State_out#state.timerlist),
    %% verify new state
    ResendRequestTimer_State = ResendRequestTimer_State_out#state{timerlist = ResendRequestTimer_Timerlist1},
    [{resendrequest}] = siptimer:test_get_appsignals(ResendRequestTimer_State_out#state.timerlist),

    test_verify_request_was_sent(Test_Request, "process_timer2/3 {resendrequest}", 1, 3),

    autotest:mark(?LINE, "process_timer2/3 {resendrequest} - 2"),
    %% test not in sipstate 'completed', no change expected
    %% not really an error but we should not normally end up there either
    ResendRequestTimer_State2 = ResendRequestTimer_State#state{sipstate = completed},
    {noreply, ResendRequestTimer_State2} =
	handle_info(ResendRequestTimer_Signal, ResendRequestTimer_State2),


    %% process_timer2({resendrequest_timeout}, Timer, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - 0"),
    {ok, ResendRequestTimeout_Signal, ResendRequestTimeout_Timerlist1} =
	test_get_timer({resendrequest_timeout}),

    ResendRequestTimeout_State =
	#state{logtag    = "testing",
	       socket    = #sipsocket{proto = yxa_test},
	       timerlist = ResendRequestTimeout_Timerlist1,
	       request   = Test_Request,
	       sipstate  = calling,
	       report_to = self(),
	       testing   = true,
	       branch    = "branch"
	      },

    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - 1.1"),
    %% test normal case
    {stop, normal, ResendRequestTimeout_State_out} =
	handle_info(ResendRequestTimeout_Signal, ResendRequestTimeout_State),

    test_verify_resendrequest_timeout_result(ResendRequestTimeout_State, ResendRequestTimeout_State_out,
					     terminated,
					     {408, "Request Timeout"},
					     1, 2
					    ),

    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - 2.1"),
    %% test normal case with cancelled request
    ResendRequestTimeout_State2 =
	ResendRequestTimeout_State#state{do_cancel = {true, []},
					 sipstate  = calling
					},
    {stop, normal, ResendRequestTimeout_State_out2} =
	handle_info(ResendRequestTimeout_Signal, ResendRequestTimeout_State2),

    test_verify_resendrequest_timeout_result(ResendRequestTimeout_State2, ResendRequestTimeout_State_out2,
					     terminated,
					     {487, "Request Terminated"},
					     2, 2
					    ),

    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - 3.1"),
    %% test normal case in state 'calling' with cancelled request
    ResendRequestTimeout_State3 =
	ResendRequestTimeout_State#state{do_cancel = false,
					 sipstate  = calling
					},
    {stop, normal, ResendRequestTimeout_State_out3} =
	handle_info(ResendRequestTimeout_Signal, ResendRequestTimeout_State3),

    test_verify_resendrequest_timeout_result(ResendRequestTimeout_State3, ResendRequestTimeout_State_out3,
					     terminated,
					     {408, "Request Timeout"},
					     3, 2
					    ),

    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - 4"),
    %% test normal case in state 'proceeding', cancelled = true to not actually start a CANCEL transaction
    ResendRequestTimeout_State4 =
	ResendRequestTimeout_State#state{do_cancel = false,
					 sipstate  = proceeding,
					 cancelled = true
					},
    {noreply, ResendRequestTimeout_State4} =
	handle_info(ResendRequestTimeout_Signal, ResendRequestTimeout_State4),

    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - 5.1"),
    %% test normal case in case 'proceeding' with non-INVITE request
    ResendRequestTimeout_Request5 = (ResendRequestTimeout_State#state.request)#request{method = "OPTIONS"},
    ResendRequestTimeout_State5 =
	ResendRequestTimeout_State#state{sipstate = proceeding,
					 request = ResendRequestTimeout_Request5
					},
    {stop, normal, ResendRequestTimeout_State_out5} =
	handle_info(ResendRequestTimeout_Signal, ResendRequestTimeout_State5),

    test_verify_resendrequest_timeout_result(ResendRequestTimeout_State5, ResendRequestTimeout_State_out5,
					     terminated,
					     {408, "Request Timeout"},
					     5, 2
					    ),

    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - 6"),
    %% test normal case in state 'proceeding', cancelled = true to not actually start a CANCEL transaction
    ResendRequestTimeout_State6 =
	ResendRequestTimeout_State#state{do_cancel = false,
					 sipstate  = completed
					},
    {noreply, ResendRequestTimeout_State6} =
	handle_info(ResendRequestTimeout_Signal, ResendRequestTimeout_State6),


    %% process_timer2({invite_timeout}, Timer, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_timer2/3 {invite_timeout} - 0"),
    {ok, InviteTimeout_Signal, InviteTimeout_Timerlist1} =
	test_get_timer({invite_timeout}),

    InviteTimeout_State =
	#state{logtag    = "testing",
	       socket    = #sipsocket{proto = yxa_test},
	       timerlist = InviteTimeout_Timerlist1,
	       request   = Test_Request,
	       sipstate  = calling,
	       report_to = self(),
	       testing   = true,
	       branch    = "branch",
	       cancelled = true
	      },

    autotest:mark(?LINE, "process_timer2/3 {invite_timeout} - 1.1"),
    %% test normal case, a 408 Request Timeout response should be generated
    {noreply, InviteTimeout_State_out} =
	handle_info(InviteTimeout_Signal, InviteTimeout_State),

    autotest:mark(?LINE, "process_timer2/3 {invite_timeout} - 1.2"),
    %% verify new state
    {408, "Request Timeout"} = InviteTimeout_State_out#state.response,
    InviteTimeout_State = InviteTimeout_State_out#state{response     = undefined,
							final_r_sent = false
						       },

    autotest:mark(?LINE, "process_timer2/3 {invite_timeout} - 1.3"),
    %% verify that report_to was notified
    Self = self(),
    receive
	{branch_result, Self, "branch", calling, {408, "Request Timeout"}} ->
	    ok
    after 0 ->
	    throw("test failed, report_to was not notified about branch result")
    end,


    %% process_timer2({invite_expire}, Timer, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_timer2/3 {invite_expire} - 0"),
    {ok, InviteExpire_Signal, InviteExpire_Timerlist1} =
	test_get_timer({invite_expire}),

    InviteExpire_State =
	#state{logtag    = "testing",
	       socket    = #sipsocket{proto = yxa_test},
	       timerlist = InviteExpire_Timerlist1,
	       request   = Test_Request,
	       sipstate  = calling,
	       report_to = self(),
	       testing   = true,
	       branch    = "branch",
	       cancelled = false
	      },

    autotest:mark(?LINE, "process_timer2/3 {invite_expire} - 1.1"),
    %% test normal case, a 408 Request Timeout response should be generated
    {stop, normal, InviteExpire_State_out} =
	handle_info(InviteExpire_Signal, InviteExpire_State),

    autotest:mark(?LINE, "process_timer2/3 {invite_expire} - 1.2"),
    %% verify new state
    {408, "Request Timeout"} = InviteExpire_State_out#state.response,
    InviteExpire_State = InviteExpire_State_out#state{response     = undefined,
						      final_r_sent = false,
						      sipstate     = calling,
						      timerlist    = InviteExpire_State#state.timerlist
						     },

    autotest:mark(?LINE, "process_timer2/3 {invite_expire} - 1.2"),
    %% verify that report_to was notified
    Self = self(),
    receive
	{branch_result, Self, "branch", terminated, {408, "Request Timeout"}} ->
	    ok
    after 0 ->
	    throw("test failed, report_to was not notified about branch result")
    end,


    %% process_timer2(Signal, Timer, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_timer2/3 Unknown - 0"),
    {ok, UnknownTimer_Signal, UnknownTimer_Timerlist1} =
	test_get_timer(testing),
    UnknownTimer_State =
 	#state{logtag    = "testing",
	       timerlist = UnknownTimer_Timerlist1
	      },

    autotest:mark(?LINE, "process_timer2/3 Unknown - 1"),
    {noreply, UnknownTimer_State} = handle_info(UnknownTimer_Signal, UnknownTimer_State),


    %% process_received_response(Response, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_received_response/2 - 1.0"),
    put({sipsocket_test, is_reliable_transport}, false),
    PRRes_State1 = #state{logtag    = "testing",
			  socket    = #sipsocket{proto = yxa_test,
						 module = sipsocket_test
						},
			  branch    = "branch",
			  tl_branch = "tl_branch",
			  dst       = Test_Dst,
			  request   = Test_Request,
			  sipstate  = calling,
			  report_to = self(),
			  timerlist = siptimer:empty(),
			  testing   = true
			 },
    PPRes_Response1 = Test_Response#response{status = 400,
					     reason = "Testing"
					    },

    autotest:mark(?LINE, "process_received_response/2 - 1.1"),
    PRRes_State1_out = process_received_response(PPRes_Response1, PRRes_State1),

    autotest:mark(?LINE, "process_received_response/2 - 1.2"),
    %% verify new state
    siptimer:cancel_all_timers(PRRes_State1_out#state.timerlist),
    400 = (PRRes_State1_out#state.response)#response.status,
    "Testing" = (PRRes_State1_out#state.response)#response.reason,
    [{terminate_transaction}] = siptimer:test_get_appsignals(PRRes_State1_out#state.timerlist),
    completed = PRRes_State1_out#state.sipstate,

    %% verify the ACK sent
    {ok, PPRes_Request1_out} = test_verify_request_was_sent(Test_Request#request{method = "ACK", body = <<>>},
							    "process_received_response/2", 1, 3),

    autotest:mark(?LINE, "process_received_response/2 - 1.6"),
    %% verify that From, To and Call-Id was copied verbatim
    lists:map(fun(H) ->
		      OrigVal = keylist:fetch(H, Test_Response#response.header),
		      AckVal = keylist:fetch(H, PPRes_Request1_out#request.header),
		      OrigVal = AckVal
	      end, ['from', 'to', 'call-id']),
    ["1 ACK"] = keylist:fetch('cseq', PPRes_Request1_out#request.header),

    autotest:mark(?LINE, "process_received_response/2 - 1.7"),
    %% verify that report_to was notified
    Self = self(),
    receive
	{branch_result, Self, "branch", completed, #response{status = 400, reason = "Testing"}} ->
	    ok
    after 0 ->
	    throw("test failed, report_to was not notified about branch result")
    end,

    autotest:mark(?LINE, "process_received_response/2 - 1.8"),
    %% verify that the ACK sent had a single Via (ours)
    [PPRes_MyVia] = keylist:fetch('via', PPRes_Request1_out#request.header),

    autotest:mark(?LINE, "process_received_response/2 - 1.9"),
    %% verify that it was our tl_branch (and not branch) that was put in the ACK's Via
    true = (string:str(PPRes_MyVia, PRRes_State1#state.tl_branch) /= 0),

    autotest:mark(?LINE, "process_received_response/2 - 2.0"),
    %% test same thing as previous test, but with Route header and alread in SIP-state 'completed'
    %% (simulating a resent 4xx response after we sent our ACK)
    PPRes_Route2 = ["<sip:route-a.example.com>", "<sip:route-b.example.com>"],
    PPRes_Header2 = keylist:set("Route", PPRes_Route2, Test_Request#request.header),
    PRRes_State2 = #state{logtag    = "testing",
			  socket    = #sipsocket{proto = yxa_test,
						 module = sipsocket_test
						},
			  branch    = "branch",
			  tl_branch = "tl_branch",
			  dst       = Test_Dst,
			  request   = Test_Request#request{header = PPRes_Header2},
			  sipstate  = confirmed,
			  report_to = self(),
			  timerlist = siptimer:empty(),
			  testing   = true
			 },
    PPRes_Response2 = Test_Response#response{status = 401,
					     reason = "Testing"
					    },

    autotest:mark(?LINE, "process_received_response/2 - 2.1"),
    PRRes_State2_out = process_received_response(PPRes_Response2, PRRes_State2),

    autotest:mark(?LINE, "process_received_response/2 - 2.2"),
    %% verify new state
    401 = (PRRes_State2_out#state.response)#response.status,
    "Testing" = (PRRes_State2_out#state.response)#response.reason,
    [] = siptimer:test_get_appsignals(PRRes_State2_out#state.timerlist),
    confirmed = PRRes_State2_out#state.sipstate,

    %% verify the ACK sent
    {ok, PPRes_Request2_out} = test_verify_request_was_sent(Test_Request#request{method = "ACK", body = <<>>},
							    "process_received_response/2", 2, 3),

    autotest:mark(?LINE, "process_received_response/2 - 2.6"),
    %% verify that From, To and Call-Id was copied verbatim
    lists:map(fun(H) ->
		      OrigVal = keylist:fetch(H, Test_Response#response.header),
		      AckVal = keylist:fetch(H, PPRes_Request2_out#request.header),
		      OrigVal = AckVal
	      end, ['from', 'to', 'call-id']),
    ["1 ACK"] = keylist:fetch('cseq', PPRes_Request2_out#request.header),
    PPRes_Route2 = keylist:fetch('route', PPRes_Request2_out#request.header),

    autotest:mark(?LINE, "process_received_response/2 - 2.7"),
    %% verify that report_to was NOT notified (since we were already in SIP-state 'completed')
    receive
	{branch_result, Self, "branch", completed, #response{status = 401, reason = "Testing"}} ->
	    throw("test failed, report_to was notified when it shouldn't have")
    after 0 ->
	    ok
    end,

    autotest:mark(?LINE, "process_received_response/2 - 2.8"),
    %% verify that the ACK sent had a single Via (ours)
    [_PPRes_MyVia2] = keylist:fetch('via', PPRes_Request2_out#request.header),


    autotest:mark(?LINE, "process_received_response/2 - 3.0"),
    PPRes_Response3 = Test_Response#response{status = 101,
					     reason = "Testing"
					    },

    PRRes_State3 = PRRes_State1#state{sipstate = proceeding,
				      response = PPRes_Response3#response{status = 100},
				      request  = Test_Request#request{method = "TESTING"}
				     },

    autotest:mark(?LINE, "process_received_response/2 - 3.1"),
    %% test receiving a non-INVITE response, and at the same time test
    %% update_transaction_state/4 without state change
    PRRes_State3_out = process_received_response(PPRes_Response3, PRRes_State3),

    autotest:mark(?LINE, "process_received_response/2 - 3.2"),
    %% verify new state
    siptimer:cancel_all_timers(PRRes_State3_out#state.timerlist),
    101 = (PRRes_State3_out#state.response)#response.status,
    "Testing" = (PRRes_State3_out#state.response)#response.reason,
    [] = siptimer:test_get_appsignals(PRRes_State3_out#state.timerlist),
    proceeding = PRRes_State3_out#state.sipstate,

    erase({sipsocket_test, is_reliable_transport}),

    %% act_on_new_sipstate2(SipState, BranchAction, State)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "act_on_new_sipstate2/3 proceeding - 1.1"),
    %% test do_cancel set to {true, []}
    ActOnNewSS_State1 = #state{logtag    = "testing",
			       do_cancel = {true, []},
			       cancelled = true,	%% to not actually start a CANCEL
			       socket    = #sipsocket{proto = yxa_test,
						      module = sipsocket_test
						     },
			       branch    = "branch",
			       tl_branch = "tl_branch",
			       dst       = Test_Dst,
			       request   = Test_Request,
			       response  = Test_Response,
			       sipstate  = confirmed,
			       report_to = self(),
			       timerlist = siptimer:empty(),
			       testing   = true
			      },

    {ActOnNewSS_State1_out, ignore} = act_on_new_sipstate2(proceeding, tell_parent, ActOnNewSS_State1),

    autotest:mark(?LINE, "act_on_new_sipstate2/3 proceeding - 1.2"),
    %% verify new state
    false = ActOnNewSS_State1_out#state.do_cancel,

    autotest:mark(?LINE, "act_on_new_sipstate2/3 proceeding - 2"),
    %% test do_cancel set to false
    ActOnNewSS_State2 = ActOnNewSS_State1#state{do_cancel = false},
    {ActOnNewSS_State2, tell_parent} = act_on_new_sipstate2(proceeding, tell_parent, ActOnNewSS_State2),

    autotest:mark(?LINE, "act_on_new_sipstate2/3 proceeding - 3"),
    %% test non-INVITE
    ActOnNewSS_State3 = ActOnNewSS_State1#state{do_cancel = false,
						request = Test_Request#request{method = "OPTIONS"}
					       },
    {ActOnNewSS_State3, tell_parent} = act_on_new_sipstate2(proceeding, tell_parent, ActOnNewSS_State3),


    autotest:mark(?LINE, "act_on_new_sipstate2/3 completed - 4.0"),
    put({sipsocket_test, is_reliable_transport}, true),
    ActOnNewSS_State4 = #state{logtag    = "testing",
			       request   = Test_Request,
			       sipstate  = confirmed,
			       socket    = #sipsocket{proto = yxa_test, module = sipsocket_test},
			       timerlist = siptimer:empty()
			      },

    autotest:mark(?LINE, "act_on_new_sipstate2/3 completed - 4.1"),
    %% test normal case with INVITE and reliable transport
    {ActOnNewSS_State4_out, ignore} = act_on_new_sipstate2(completed, ignore, ActOnNewSS_State4),

    autotest:mark(?LINE, "act_on_new_sipstate2/3 completed - 4.2"),
    %% verify new state
    siptimer:cancel_all_timers(ActOnNewSS_State4_out#state.timerlist),
    [{terminate_transaction}] = siptimer:test_get_appsignals(ActOnNewSS_State4_out#state.timerlist),
    erase({sipsocket_test, is_reliable_transport}),

    autotest:mark(?LINE, "act_on_new_sipstate2/3 completed - 5.1"),
    %% test non-INVITE with unreliable transport
    put({sipsocket_test, is_reliable_transport}, false),
    ActOnNewSS_State5 = ActOnNewSS_State4#state{request = Test_Request#request{method = "OPTIONS"}},
    {ActOnNewSS_State5_out, ignore} = act_on_new_sipstate2(completed, ignore, ActOnNewSS_State5),

    autotest:mark(?LINE, "act_on_new_sipstate2/3 completed - 5.2"),
    %% verify new state
    siptimer:cancel_all_timers(ActOnNewSS_State5_out#state.timerlist),
    [{terminate_transaction}] = siptimer:test_get_appsignals(ActOnNewSS_State5_out#state.timerlist),

    autotest:mark(?LINE, "act_on_new_sipstate2/3 terminated - 6"),
    ActOnNewSS_State6 = #state{timerlist = siptimer:empty()},
    {ActOnNewSS_State6, ignore} = act_on_new_sipstate2(terminated, ignore, ActOnNewSS_State6),

    autotest:mark(?LINE, "act_on_new_sipstate2/3 trying - 7"),
    {#state{}, ignore} = act_on_new_sipstate2(calling, ignore, #state{}),

    autotest:mark(?LINE, "act_on_new_sipstate2/3 calling - 8"),
    {#state{}, ignore} = act_on_new_sipstate2(calling, ignore, #state{}),


    %% perform_branchaction(BranchAction, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "perform_branchaction/2 - 1"),
    %% test final response when parent has already received a final response
    PerformBAction_State1 = #state{logtag       = "testing",
				   request      = Test_Request,
				   response     = {400, "Testing"},
				   final_r_sent = true,
				   report_to    = self(),
				   testing      = true
				  },
    PerformBAction_State1 = perform_branchaction(tell_parent, PerformBAction_State1),

    autotest:mark(?LINE, "perform_branchaction/2 - 2"),
    %% test with undefined report_to
    PerformBAction_State2 = PerformBAction_State1#state{report_to = undefined},
    PerformBAction_State2 = perform_branchaction(tell_parent, PerformBAction_State2),

    autotest:mark(?LINE, "perform_branchaction/2 - 3"),
    %% test with dead report_to
    DeadPid = spawn(fun() -> ok end),
    erlang:yield(),
    PerformBAction_State3 = PerformBAction_State1#state{report_to = DeadPid},
    PerformBAction_State3 = perform_branchaction(tell_parent, PerformBAction_State3),

    autotest:mark(?LINE, "perform_branchaction/2 - 4.1"),
    %% test non-final response
    PerformBAction_State4 = PerformBAction_State1#state{final_r_sent = false,
							branch       = "branch",
							sipstate     = proceeding,
						        response     = {101, "Testing"}
						       },
    PerformBAction_State4 = perform_branchaction(tell_parent, PerformBAction_State4),

    autotest:mark(?LINE, "perform_branchaction/2 - 4.2"),
    %% verify that report_to was notified
    receive
	{branch_result, Self, "branch", proceeding, {101, "Testing"}} ->
	    ok
    after 0 ->
	    throw("test failed, report_to was not notified about branch result")
    end,


    %% received_response_state_machine(Method, Status, State)
    %%--------------------------------------------------------------------

    %% 17.1.1 INVITE Client Transaction

    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 1"),
    %% If the client transaction receives a provisional response while in
    %% the "Calling" state, it transitions to the "Proceeding" state.
    %% ... Furthermore, the provisional response MUST be passed to the TU.
    %% Yxa comment: 100 Trying is just to make us stop resending, not forwarded to TU.
    {ignore, proceeding} = received_response_state_machine("INVITE", 100, calling),
    {tell_parent, proceeding} = received_response_state_machine("INVITE", 199, calling),

    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 2"),
    %% Any further provisional responses MUST be passed up to the TU while
    %% in the "Proceeding" state.
    %% Yxa comment: 100 Trying is just to make us stop resending, not forwarded to TU.
    {ignore, proceeding} = received_response_state_machine("INVITE", 100, proceeding),
    {tell_parent, proceeding} = received_response_state_machine("INVITE", 199, proceeding),

    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 3"),
    %% When in either the "Calling" or "Proceeding" states, reception of a
    %% response with status code from 300-699 MUST cause the client
    %% transaction to transition to "Completed".  The client transaction
    %% MUST pass the received response up to the TU
    {tell_parent, completed} = received_response_state_machine("INVITE", 300, calling),
    {tell_parent, completed} = received_response_state_machine("INVITE", 400, calling),
    {tell_parent, completed} = received_response_state_machine("INVITE", 500, calling),
    {tell_parent, completed} = received_response_state_machine("INVITE", 600, calling),
    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 3"),
    {tell_parent, completed} = received_response_state_machine("INVITE", 300, proceeding),
    {tell_parent, completed} = received_response_state_machine("INVITE", 400, proceeding),
    {tell_parent, completed} = received_response_state_machine("INVITE", 500, proceeding),
    {tell_parent, completed} = received_response_state_machine("INVITE", 600, proceeding),

    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 4"),
    %% Any retransmissions of the final response that are received while in
    %% the "Completed" state MUST cause the ACK to be re-passed to the
    %% transport layer for retransmission, but the newly received response
    %% MUST NOT be passed up to the TU.
    {ignore, completed} = received_response_state_machine("INVITE", 300, completed),
    {ignore, completed} = received_response_state_machine("INVITE", 400, completed),
    {ignore, completed} = received_response_state_machine("INVITE", 500, completed),
    {ignore, completed} = received_response_state_machine("INVITE", 600, completed),

    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 5"),
    %% When in either the "Calling" or "Proceeding" states, reception of a
    %% 2xx response MUST cause the client transaction to enter the
    %% "Terminated" state, and the response MUST be passed up to the TU.
    {tell_parent, terminated} = received_response_state_machine("INVITE", 200, calling),
    {tell_parent, terminated} = received_response_state_machine("INVITE", 200, proceeding),

    %% own conclusions

    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 6"),
    {ignore, completed} = received_response_state_machine("INVITE", 100, completed),
    {ignore, completed} = received_response_state_machine("INVITE", 199, completed),

    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 7"),
    %% late 2xx response, received after a non-2xx response that made us reach 'completed'
    %% XXX actually, this is really weird - someone sent us two different final responses.
    %% should we really go from 'completed' to 'terminated' or 'ignore' and stay in 'completed'?
    {tell_parent, terminated} = received_response_state_machine("INVITE", 200, completed),
    %% we shouldn't be around to reveive late responses if we are terminated, but...
    {tell_parent, terminated} = received_response_state_machine("INVITE", 200, terminated),

    autotest:mark(?LINE, "received_response_state_machine/3 INVITE - 8"),
    %% when in 'completed' or 'terminated', ignore any 6xx responses. We can't undo
    %% that someone has already picked up the phone even if another branch generates
    %% a 6xx.
    {ignore, completed} = received_response_state_machine("INVITE", 600, completed),
    {ignore, terminated} = received_response_state_machine("INVITE", 600, terminated),


    %% 17.1.2 Non-INVITE Client Transaction

    autotest:mark(?LINE, "received_response_state_machine/3 non-INVITE - 1"),
    %% If a provisional response is received while in the "Trying" state, the
    %% response MUST be passed to the TU, and then the client transaction
    %% SHOULD move to the "Proceeding" state.
    %% Yxa comment: 100 Trying is just to make us stop resending, not forwarded to TU.
    {ignore, proceeding} = received_response_state_machine("OPTIONS", 100, trying),
    {tell_parent, proceeding} = received_response_state_machine("OPTIONS", 199, trying),

    autotest:mark(?LINE, "received_response_state_machine/3 non-INVITE - 2"),
    %% If a final response (status codes 200-699) is received while in the
    %% "Trying" state, the response MUST be passed to the TU, and the client
    %% transaction MUST transition to the "Completed" state.
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 200, trying),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 300, trying),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 400, trying),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 500, trying),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 600, trying),

    autotest:mark(?LINE, "received_response_state_machine/3 non-INVITE - 3"),
    %% If a final response (status codes 200-699) is received while in the
    %% "Proceeding" state, the response MUST be passed to the TU, and the
    %% client transaction MUST transition to the "Completed" state.
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 200, proceeding),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 300, proceeding),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 400, proceeding),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 500, proceeding),
    {tell_parent, completed} = received_response_state_machine("OPTIONS", 600, proceeding),

    %% own conclusions

    autotest:mark(?LINE, "received_response_state_machine/3 non-INVITE - 4"),
    %% we are already in the completed state, the TU has already been given a final response
    {ignore, completed} = received_response_state_machine("OPTIONS", 100, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 199, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 200, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 300, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 400, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 500, completed),
    {ignore, completed} = received_response_state_machine("OPTIONS", 600, completed),

    autotest:mark(?LINE, "received_response_state_machine/3 non-INVITE - 5"),
    %% this is not the first provisional response we receive, we are already in 'proceeding'
    {ignore, proceeding} = received_response_state_machine("OPTIONS", 100, proceeding),
    {ignore, proceeding} = received_response_state_machine("OPTIONS", 199, proceeding),

    autotest:mark(?LINE, "received_response_state_machine/3 - 1"),
    %% test too low or too high response numbers, testing with differend method is not needed
    {'EXIT', {function_clause, _}} = (catch received_response_state_machine("INVITE", 99, trying)),
    {'EXIT', {function_clause, _}} = (catch received_response_state_machine("INVITE", -1, trying)),
    {'EXIT', {function_clause, _}} = (catch received_response_state_machine("INVITE", 700, trying)),
    {'EXIT', {function_clause, _}} = (catch received_response_state_machine("INVITE", 32984397, trying)),


    %% get_request_resend_timeout/3
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "get_request_resend_timeout/3 INVITE - 1"),
    %% INVITE, always just double
    1000 = get_request_resend_timeout("INVITE", 500, trying),
    2000 = get_request_resend_timeout("INVITE", 1000, trying),
    4000 = get_request_resend_timeout("INVITE", 2000, trying),
    8000 = get_request_resend_timeout("INVITE", 4000, trying),
    16000 = get_request_resend_timeout("INVITE", 8000, trying),
    32000 = get_request_resend_timeout("INVITE", 16000, trying),

    autotest:mark(?LINE, "get_request_resend_timeout/3 non-INVITE - 1"),
    %% non-INVITE, state 'trying'. Resend at 500ms, 1s, 2s, 4s, 4s, 4s ...
    1000 = get_request_resend_timeout("OPTIONS", 500, trying),
    2000 = get_request_resend_timeout("OPTIONS", 1000, trying),
    4000 = get_request_resend_timeout("OPTIONS", 2000, trying),
    4000 = get_request_resend_timeout("OPTIONS", 4000, trying),

    autotest:mark(?LINE, "get_request_resend_timeout/3 non-INVITE - 1"),
    %% non-INVITE, state 'proceeding'. Resend every 4s.
    4000 = get_request_resend_timeout("OPTIONS", 500, proceeding),
    4000 = get_request_resend_timeout("OPTIONS", 1000, proceeding),
    4000 = get_request_resend_timeout("OPTIONS", 2000, proceeding),
    4000 = get_request_resend_timeout("OPTIONS", 4000, proceeding),


    %% initiate_request(State)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "initiate_request/1 INVITE - 1.0"),
    %% test normal case with INVITE transaction
    InitiateReq_State1 = #state{logtag       = "testing",
				branch       = "branch",
				request      = Test_Request,
				dst	     = Test_Dst,
				socket_in    = #sipsocket{proto = yxa_test,
							  module = sipsocket_test
							 },
				timerlist    = siptimer:empty(),
				report_to    = self(),
				timeout      = 40
			       },

    autotest:mark(?LINE, "initiate_request/1 INVITE - 1.1"),
    InitiateReq_State1_out = initiate_request(InitiateReq_State1),

    autotest:mark(?LINE, "initiate_request/1 INVITE - 1.2"),
    %% verify new state
    siptimer:cancel_all_timers(InitiateReq_State1_out#state.timerlist),
    [{resendrequest}, {resendrequest_timeout},
     {invite_timeout}, {invite_expire}] = siptimer:test_get_appsignals(InitiateReq_State1_out#state.timerlist),
    InitiateReq_State1 = InitiateReq_State1_out#state{
			   timerlist = InitiateReq_State1#state.timerlist,
			   tl_branch = undefined,
			   socket    = undefined
			  },

    test_verify_request_was_sent(InitiateReq_State1#state.request, "initiate_request/1 INVITE", 1, 3),

    autotest:mark(?LINE, "initiate_request/1 non-INVITE - 2.0"),
    %% test normal case with non-INVITE transaction
    InitiateReq_State2 = #state{logtag       = "testing",
				branch       = "branch",
				request      = Test_Request#request{method = "TESTING"},
				dst	     = Test_Dst,
				socket_in    = #sipsocket{proto = yxa_test,
							  module = sipsocket_test
							 },
				timerlist    = siptimer:empty(),
				report_to    = self(),
				timeout      = 40
			       },

    put({sipsocket_test, is_reliable_transport}, true),
    autotest:mark(?LINE, "initiate_request/1 non-INVITE - 2.1"),
    InitiateReq_State2_out = initiate_request(InitiateReq_State2),
    erase({sipsocket_test, is_reliable_transport}),

    autotest:mark(?LINE, "initiate_request/1 non-INVITE - 2.2"),
    %% verify new state
    siptimer:cancel_all_timers(InitiateReq_State2_out#state.timerlist),
    [{resendrequest_timeout}] = siptimer:test_get_appsignals(InitiateReq_State2_out#state.timerlist),
    InitiateReq_State2 = InitiateReq_State2_out#state{
			   timerlist = InitiateReq_State2#state.timerlist,
			   tl_branch = undefined,
			   socket    = undefined
			  },

    test_verify_request_was_sent(InitiateReq_State2#state.request, "initiate_request/1 non-INVITE", 2, 3),


    autotest:mark(?LINE, "initiate_request/1 - 3.0"),
    %% test transport layer send error
    InitiateReq_State3 = #state{logtag       = "testing",
				branch       = "branch",
				request      = Test_Request,
				dst	     = Test_Dst,
				socket_in    = #sipsocket{proto = yxa_test,
							  module = sipsocket_test
							 },
				timerlist    = siptimer:empty(),
				report_to    = self(),
				sipstate     = calling,
				timeout      = 40,
				testing      = true
			       },

    put({sipsocket_test, send_result}, {error, "testing"}),
    autotest:mark(?LINE, "initiate_request/1 - 3.1"),
    InitiateReq_State3_out = initiate_request(InitiateReq_State3),
    erase({sipsocket_test, send_result}),

    autotest:mark(?LINE, "initiate_request/1 - 3.2"),
    %% verify new state
    [] = siptimer:test_get_appsignals(InitiateReq_State3_out#state.timerlist),
    InitiateReq_State3 = InitiateReq_State3_out#state{
			   response     = undefined,
			   sipstate     = calling,
			   final_r_sent = false
			  },
    {503, "Service Unavailable"} = InitiateReq_State3_out#state.response,
    true = InitiateReq_State3_out#state.final_r_sent,
    terminated = InitiateReq_State3_out#state.sipstate,

    test_verify_request_was_sent(InitiateReq_State3#state.request, "initiate_request/1", 3, 3),

    autotest:mark(?LINE, "initiate_request/1 - 3.6"),
    %% verify that report_to was notified
    Self = self(),
    receive
	{branch_result, Self, "branch", terminated, {503, "Service Unavailable"}} ->
	    ok
    after 0 ->
	    throw("test failed, report_to was not notified about branch result")
    end,

    %% end_invite(State)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "end_invite/1 - 1.0"),
    %% test normal case with INVITE in state 'proceeding', CANCEL transaction will be started
    EndInvite_State1 = #state{logtag    = "testing",
			      tl_branch = "tl_branch",
			      request   = Test_Request,
			      dst	= Test_Dst,
			      socket    = #sipsocket{proto = yxa_test,
						     module = sipsocket_test
						    },
			      timerlist = siptimer:empty(),
			      report_to = self(),
			      sipstate  = proceeding,
			      timeout   = 40,
			      testing   = true
			     },

    autotest:mark(?LINE, "end_invite/1 - 1.1"),
    {ok, testing, EndInvite_State1_out, CancelStartArgs} = end_invite(EndInvite_State1),

    autotest:mark(?LINE, "end_invite/1 - 1.2"),
    %% verify new state
    siptimer:cancel_all_timers(EndInvite_State1_out#state.timerlist),
    [{terminate_transaction}] = siptimer:test_get_appsignals(EndInvite_State1_out#state.timerlist),
    testing = EndInvite_State1_out#state.cancel_pid,

    autotest:mark(?LINE, "end_invite/1 - 1.3"),
    %% verify arguments that would have been used to start the CANCEL transaction
    EndInviteSocket1 = EndInvite_State1#state.socket,
    EndInviteDst1 = EndInvite_State1#state.dst,
    [#request{method = "CANCEL"},
     EndInviteSocket1,
     EndInviteDst1,
     "tl_branch",
     _EndInviteTimeout1,
     none] = CancelStartArgs,

    autotest:mark(?LINE, "end_invite/1 - 2"),
    %% test sipstate 'completed', should be ignored
    EndInvite_State2 = #state{request  = Test_Request,
			      logtag   = "testing",
			      sipstate = completed
			     },
    EndInvite_State2 = end_invite(EndInvite_State2),

    autotest:mark(?LINE, "end_invite/1 - 2"),
    %% test sipstate 'terminated', should be ignored
    EndInvite_State3 = #state{request  = Test_Request,
			      logtag   = "testing",
			      sipstate = terminated
			     },
    EndInvite_State3 = end_invite(EndInvite_State3),


    %% cancel_request(State, ExtraHeaders)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "cancel_request/2 - 1.0"),
    CancelRequest_State1_1 = #state{request = Test_Request#request{method = "TESTING"},
				    logtag  = "testing",
				    timerlist = siptimer:empty()
				   },
    CancelRequest_State1 = add_timer(10000, "testing timer", {resendrequest}, CancelRequest_State1_1),

    autotest:mark(?LINE, "cancel_request/2 - 1.1"),
    CancelRequest_State1_out = cancel_request(CancelRequest_State1, []),

    autotest:mark(?LINE, "cancel_request/2 - 1.1"),
    %% verify new state
    siptimer:cancel_all_timers(CancelRequest_State1_out#state.timerlist),
    [{terminate_transaction}] = siptimer:test_get_appsignals(CancelRequest_State1_out#state.timerlist),
    completed = CancelRequest_State1_out#state.sipstate,
    true = CancelRequest_State1_out#state.cancelled,
    CancelRequest_State1 = CancelRequest_State1_out#state{timerlist = CancelRequest_State1#state.timerlist,
							  sipstate  = undefined,
							  cancelled = false
							 },

    %% update_invite_expire(Status, State)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "update_invite_expire/2 - 1.0"),
    UpdInvExp_State1_1 = #state{logtag    = "testing",
				timerlist = siptimer:empty()
			       },
    UpdInvExp_State1 = add_timer(10000, "test timer", {invite_expire}, UpdInvExp_State1_1),

    autotest:mark(?LINE, "update_invite_expire/2 - 1.1"),
    %% test normal case
    UpdInvExp_State1_out = update_invite_expire(101, UpdInvExp_State1),

    autotest:mark(?LINE, "update_invite_expire/2 - 1.2"),
    %% verify new state
    siptimer:cancel_all_timers(UpdInvExp_State1_out#state.timerlist),
    [{invite_expire}] = siptimer:test_get_appsignals(UpdInvExp_State1_out#state.timerlist),
    UpdInvExp_State1 = UpdInvExp_State1_out#state{timerlist = UpdInvExp_State1#state.timerlist},

    autotest:mark(?LINE, "update_invite_expire/2 - 2.0"),
    UpdInvExp_State2 = #state{logtag    = "testing",
			      timerlist = siptimer:empty()
			     },

    autotest:mark(?LINE, "update_invite_expire/2 - 2.1"),
    %% test with no timer found in list
    UpdInvExp_State2 = update_invite_expire(101, UpdInvExp_State2),


    ok.


test_verify_request_was_sent(Request, TestLabel, Major, Minor) ->
    test_verify_request_was_sent(Request, TestLabel, Major, Minor, 0).

test_verify_request_was_sent(Request, TestLabel, Major, Minor, Timeout) when is_record(Request, request),
									     is_list(TestLabel),
									     is_integer(Major), is_integer(Minor),
									     is_integer(Timeout) ->
    autotest:mark(?LINE, "~s - ~p.~p", [TestLabel, Major, Minor]),
    %% check that a request was generated
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.27", 6050}, SipMsg} ->
	    %% parse message that was 'sent'
	    autotest:mark(?LINE, "~s - ~p.~p", [TestLabel, Major, Minor + 1]),
	    ParsedRequest = sippacket:parse(SipMsg, none),
	    true = is_record(ParsedRequest, request),

	    autotest:mark(?LINE, "~s - ~p.~p", [TestLabel, Major, Minor + 2]),
	    %% check that it was the expected response that was resent
	    {Method, URI, Body} = {Request#request.method, Request#request.uri, Request#request.body},

	    #request{method = Method,
		     uri    = URI,
		     body   = Body
		    } = ParsedRequest,
	    {ok, ParsedRequest}
    after Timeout ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end.


test_verify_resendrequest_timeout_result(State, State_out, SipState, Response, Major, Minor) ->
    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - ~p.~p", [Major, Minor]),
    %% verify new state
    SipState = State_out#state.sipstate,
    Response = State_out#state.response,
    State = State_out#state{
	      sipstate     = State#state.sipstate,
	      response     = State#state.response,
	      timerlist    = State#state.timerlist,
	      final_r_sent = State#state.final_r_sent
	     },

    autotest:mark(?LINE, "process_timer2/3 {resendrequest_timeout} - ~p.~p", [Major, Minor + 1]),
    %% verify that report_to was notified
    Self = self(),
    receive
	{branch_result, Self, "branch", SipState, Response} ->
	    ok
    after 0 ->
	    throw("test failed, report_to was not notified about branch result")
    end.

%% set up a timer and let it fire, and then return the signal plus a timerlist
test_get_timer(AppSignal) ->
    Timerlist =
	siptimer:add_timer(1, "Testing testing",
			   AppSignal,
			   siptimer:empty()
			  ),

    Signal =
	receive
	    {siptimer, Ref, "Testing testing"} ->
		{siptimer, Ref, "Testing testing"}
	after 100 ->
		throw("test failed, the timer we set up never fired")
	end,

    {ok, Signal, Timerlist}.
