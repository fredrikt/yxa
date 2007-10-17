%%%-------------------------------------------------------------------
%%% File    : appserver_glue.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Appserver glue between sipproxy and server transaction.
%%%
%%%           During a fork of a request, we typically have the
%%%           following components working together :
%%%
%%%           ```
%%%                    -------
%%%                    | UAS |
%%%                    ---+---
%%%                       |
%%%               ------------------
%%%               | appserver glue |
%%%               ------------------
%%%                       |
%%%               ------------------
%%%               |    sipproxy    |
%%%               ------------------
%%%                 |     |      |
%%%               ----- -----  -----
%%%               |UAC| |UAC|  |UAC|  ...
%%%               ----- -----  -----
%%%
%%%           '''
%%%
%%%           In this module, the UAS is called the `callhandler' and
%%%           the sipproxy is called `forkpid'.
%%%
%%%           This process does the following :
%%%
%%%           1) Makes sure we deliver one, and only one, final
%%%              response to the UAS (with one exception, see below)
%%%
%%%           2) Statelessly forwards any 'late' 200 OK responses to
%%%              INVITE that the sipproxy delivers to us. This is
%%%              necessary because the UAS terminates right after it
%%%              sends the first 200 OK response (if the request was
%%%              an INVITE).
%%%
%%%           3) Tells sipproxy to CANCEL all pending transactions if
%%%              the UAS is cancelled (the transaction layer receives
%%%              a CANCEL that matches the UAS transaction).
%%%
%%%           for CPL processing, things work a bit differently. When
%%%           we process a CPL script, we don't pass final responses
%%%           to the UAS but rather to the CPL engine so that it can
%%%           decide what to do.
%%%
%%%           Signals handled :
%%%
%%%           From UAS :
%%%
%%%              `{servertransaction_cancelled, FromPid, ExtraHeaders}' <br/>
%%%                 the UAS has been cancelled.
%%%
%%%              `{servertransaction_terminating, FromPid}'             <br/>
%%%                 the UAS is terminating right now.
%%%
%%%           From sipproxy :
%%%
%%%              `{sipproxy_response, FromPid, Branch, Response}'       <br/>
%%%                 a response has been received from one of the
%%%                 client transactions, that the sipproxy decided
%%%                 should be forwarded upstreams.
%%%
%%%              `{sipproxy_all_terminated, FromPid, FinalResponse}'    <br/>
%%%                 All the call actions provided to the sipproxy have
%%%                 completed.
%%%
%%%              `{sipproxy_no_more_actions, FromPid}'                  <br/>
%%%                 The sipproxy has cancelled all pending client
%%%                 transactions, and is now just staying alive to
%%%                 forward any 200 OK responses.
%%%
%%%              `{sipproxy_terminating, FromPid}'                      <br/>
%%%                  The sipproxy process is finally terminating.
%%%
%%% @since    25 Oct 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(appserver_glue).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/3,
	 start_link_cpl/6
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

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  branchbase,		%% string(), base of branches we should use
	  callhandler,		%% term(), the server transaction handler
	  request,		%% request record()
	  forkpid,		%% pid() of sipproxy process
	  callhandler_pid,	%% pid() of server transaction handler, only use for matching!
	  cancelled = false,	%% atom(), true | false
	  completed = false,	%% atom(), true | false
	  cpl_pid = none	%% pid(), CPL interpreter backend
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
%% Timeout before dying.
-define(TIMEOUT, 300 * 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Request, Actions, Surplus) -> term()
%%
%%            Request = #request{}
%%            Actions = [#sipproxy_action{}]
%%            Surplus = [#sipproxy_action{}]
%%
%% @doc     Start an appserver_glue gen_server. One is started for
%%          each forked request - acting as glue between a sipproxy
%%          process and a server transaction.
%% @end
%%--------------------------------------------------------------------
start_link(Request, Actions, Surplus) when is_record(Request, request), is_list(Actions),
					   is_list(Surplus) ->
    gen_server:start_link(?MODULE, {Request, Actions, Surplus}, []).

%%--------------------------------------------------------------------
%% @spec    (Parent, BranchBase, CallHandler, Request, Actions,
%%          Surplus) -> term()
%%
%%            Parent      = pid() "CPL interpreter backend process"
%%            BranchBase  = string()
%%            CallHandler = #thandler{}
%%            Request     = #request{}
%%            Actions     = [#sipproxy_action{}]
%%            Surplus     = [#sipproxy_action{}]
%%
%% @doc     Starts a fork as a part of CPL processing. Exported only
%%          for use by the CPL subsystem.
%% @private
%% @end
%%--------------------------------------------------------------------
start_link_cpl(Parent, BranchBase, CallHandler, Request, Actions, Surplus)
  when is_pid(Parent), is_list(BranchBase), is_record(Request, request), is_list(Actions), is_list(Surplus) ->
    gen_server:start_link(?MODULE, {cpl, Parent, BranchBase, CallHandler, Request, Actions, Surplus}, []).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({Request, Actions, Surplus}) ->
%%            {ok, State, Timeout} |
%%            ok                   |
%%            error
%%
%%            Request    = #request{}
%%            Actions    = [#sipproxy_action{}]
%%            Surplus    = [#sipproxy_action{}]
%%            Parent     = pid() "CPL interpreter backend process"
%%            BranchBase = string()
%%
%% @doc     Initiates the server.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({Request, Actions, Surplus}) when is_record(Request, request), is_list(Actions), is_list(Surplus) ->
    case transactionlayer:adopt_st_and_get_branchbase(Request) of
	{ok, CallHandler, BranchBase} ->
	    ForkPid = spawn_link(sipproxy, start_actions, [BranchBase, self(), Request, Actions, Surplus]),
	    logger:log(normal, "~s: Appserver glue: Forking request, ~p actions (~p surplus actions)",
		       [BranchBase, length(Actions), length(Surplus)]),
	    logger:log(debug, "Appserver glue: Forking request, CallHandler (UAS) ~p, ForkPid (sipproxy) ~p",
		       [CallHandler, ForkPid]),
	    %% We need the pid of the callhandler extracted to do guard matches on it
	    CHPid = transactionlayer:get_pid_from_handler(CallHandler),
	    {ok, #state{branchbase	= BranchBase,
			callhandler	= CallHandler,
			forkpid		= ForkPid,
			request		= Request,
			callhandler_pid	= CHPid
		       }, ?TIMEOUT};
	ignore ->
	    ok;
	Res ->
	    logger:log(error, "Appserver glue: Can't start - failed to adopt server transaction : ~p",
		       [Res]),
	    error
    end;

%%--------------------------------------------------------------------
%% @spec    ({cpl, Parent, BranchBase, CallHandler, Request, Actions,
%%          Surplus}) ->
%%            {ok, State, Timeout} |
%%            ok                   |
%%            error
%%
%%            Parent      = pid() "CPL interpreter backend process"
%%            BranchBase  = string()
%%            CallHandler = #thandler{}
%%            Request     = #request{}
%%            Actions     = [#sipproxy_action{}]
%%            Surplus     = [#sipproxy_action{}]
%%
%% @doc     Initiates the server. For CPL proxy actions, we don't
%%          adopt the server transaction and we return everything but
%%          provisional responses and 2xx response to INVITE to the
%%          CPL script for further processing, instead of sending it
%%          to the STHandler like we do for non-CPL processing.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({cpl, Parent, BranchBase, CallHandler, Request, Actions, Surplus}) ->
    ForkPid = spawn_link(sipproxy, start_actions, [BranchBase, self(), Request, Actions, Surplus]),
    logger:log(normal, "~s: Appserver glue: Forking request, ~p actions",
	       [BranchBase, length(Actions)]),
    logger:log(debug, "Appserver glue: Forking request, CPL process ~p, CallHandler ~p, ForkPid ~p",
	       [Parent, CallHandler, ForkPid]),
    %% We need the pid of the callhandler extracted to do guard matches on it
    CHPid = transactionlayer:get_pid_from_handler(CallHandler),
    {ok, #state{branchbase	= BranchBase,
		callhandler	= CallHandler,
		forkpid		= ForkPid,
		request		= Request,
		callhandler_pid	= CHPid,
		cpl_pid		= Parent
	       }, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Unknown, From, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, From, State) ->
    logger:log(error, "Appserver glue: Received unknown gen_server call from ~p :~n~p",
	       [From, Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec    handle_cast(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown cast.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "Appserver glue: Received unknown gen_server cast :~n~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec    handle_info(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling all non call/cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({servertransaction_cancelled, FromPid, EH}, State) ->
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%%            FromPid = pid() "callhandler_pid"
%%            EH      = [{Key, ValueList}] "extra headers to include in the CANCEL requests"
%%
%% @doc     Our server transaction (CallHandlerPid) signals that it
%%          has been cancelled. Send {cancel_pending, EH} to our
%%          sipproxy process (ForkPid).
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({servertransaction_cancelled, FromPid, EH}, #state{callhandler_pid = FromPid} = State)
  when is_pid(FromPid) ->
    ForkPid = State#state.forkpid,
    logger:log(debug, "Appserver glue: Original request has been cancelled, sending " ++
	       "'cancel_pending' to ForkPid ~p and entering state 'cancelled' (answering '487 Request Cancelled')",
	       [ForkPid]),
    %% By not doing util:safe_signal(...), we crash (and return 500) instead of returning 487 if this fails
    ForkPid ! {cancel_pending, EH},
    transactionlayer:send_response_handler(State#state.callhandler, 487, "Request Cancelled"),
    NewState1 = State#state{cancelled = true},
    check_quit({noreply, NewState1, ?TIMEOUT}, none);

%%--------------------------------------------------------------------
%% @spec    ({servertransaction_terminated, FromPid}, State) ->
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%%            FromPid = pid() "callhandler_pid"
%%
%% @doc     Our server transaction (CallHandlerPid) signals that it
%%          has terminated. We have no business when either our
%%          server transaction, or our sipproxy has terminated so we
%%          might as well quit too. This will make sipproxy exit,
%%          which will make the client transactions cancel themselves
%%          if they are not already finished.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({servertransaction_terminating, FromPid}, #state{callhandler_pid = FromPid} = State) when is_pid(FromPid) ->
    #state{callhandler_pid	= CallHandlerPid,
	   forkpid		= ForkPid
	  } = State,
    logger:log(debug, "Appserver glue: received servertransaction_terminating from my CallHandlerPid ~p "
	       "(ForkPid is ~p) - terminating (completed: ~p)", [CallHandlerPid, ForkPid, State#state.completed]),
    check_quit({stop, normal, State});

%%--------------------------------------------------------------------
%% @spec
%%    ({sipproxy_response, FromPid, Branch, Response}, State) ->
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%%            FromPid  = pid() "forkpid"
%%            Branch   = string()
%%            Response = #response{}
%%
%% @doc     Our sipproxy has received a response that it was supposed
%%          to forward to the TU (transaction user, us). Check it out
%%          to see what we should do with it (forward it to the
%%          server transaction, or just ignore it - depends on our
%%          sipstate).
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({sipproxy_response, FromPid, _Branch, Response}, #state{forkpid = FromPid} = State)
  when is_record(Response, response), is_pid(FromPid) ->
    NewState = handle_sipproxy_response(Response, State),
    check_quit({noreply, NewState, ?TIMEOUT});

%%--------------------------------------------------------------------
%% @spec
%%    ({sipproxy_all_terminated, FromPid, FinalResponse}, State) ->
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%%            FromPid       = pid() "forkpid"
%%            FinalResponse = #response{}                    |
%%                            {Status, Reason}               |
%%                            {Status, Reason, ExtraHeaders}
%%            Status        = integer() "SIP status code"
%%            Reason        = string() "SIP reason phrase"
%%
%% @doc     Our sipproxy says that now, all it's client transactions
%%          are finished. It hands us either a final response as a
%%          record(), which we might or might not have seen before
%%          (so we check if we should forward it to the server
%%          transaction), or an (error) response as a tuple that it
%%          wants us to tell the server transaction to generate.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({sipproxy_all_terminated, FromPid, FinalResponse}, #state{forkpid = FromPid} = State) when is_pid(FromPid) ->
    CallHandler = State#state.callhandler,
    NewState1 =
	case State#state.cancelled of
	    true ->
		logger:log(debug, "Appserver glue: received sipproxy_all_terminated - request was "
			   "previously cancelled. Silently enter state 'completed'."),
		State#state{completed = true};
	    false ->
		case FinalResponse of
		    _ when is_record(FinalResponse, response) ->
			logger:log(debug, "Appserver glue: received sipproxy_all_terminated with a '~p ~s' "
				   "response (completed: ~p)", [FinalResponse#response.status,
								FinalResponse#response.reason,
								State#state.completed]),
			handle_sipproxy_response(FinalResponse, State);
		    {Status, Reason} when is_integer(Status), Status >= 200, is_list(Reason) ->
			logger:log(debug, "Appserver glue: received sipproxy_all_terminated - asking CallHandler "
				   "~p to answer '~p ~s'", [CallHandler, Status, Reason]),
			send_final_response(State, Status, Reason);
		    {Status, Reason, ExtraHeaders} when is_integer(Status), Status >= 200, is_list(Reason) ->
			logger:log(debug, "Appserver glue: received sipproxy_all_terminated - asking CallHandler "
				   "~p to answer '~p ~s'", [CallHandler, Status, Reason]),
			send_final_response(State, Status, Reason, ExtraHeaders)
		end
	end,
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% @spec    ({sipproxy_no_more_actions, FromPid}, State) ->
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%%            FromPid = pid() "forkpid"
%%
%% @doc     Our sipproxy says that it has no more actions to perform.
%%          Check if we have sent a final response by now, and if so
%%          just ignore this signal. If we haven't sent a final
%%          response yet we check if we should generate a '408
%%          Request Timeout' response.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({sipproxy_no_more_actions, FromPid}, #state{forkpid = FromPid} = State) when is_pid(FromPid) ->
    NewState1 =
	case State#state.cancelled of
	    true ->
		logger:log(debug, "Appserver glue: received sipproxy_no_more_actions when cancelled. Ignoring."),
		State#state{completed = true};
	    false ->
		case State#state.completed of
		    false ->
			#request{method = Method, uri = URI} = State#state.request,
			logger:log(debug, "Appserver glue: received sipproxy_no_more_actions when NOT cancelled "
				   "(and not completed), responding 408 Request Timeout to original request ~s ~s",
				   [Method, sipurl:print(URI)]),
			send_final_response(State, 408, "Request Timeout");
		    true ->
			logger:log(debug, "Appserver glue: received sipproxy_no_more_actions "
				   "when already completed - ignoring"),
			State
		end
	end,
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% @spec    ({sipproxy_terminating, FromPid}, State) ->
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%%            FromPid = pid() "forkpid"
%%
%% @doc     Our sipproxy is terminating. We might as well terminate
%%          too. If we haven't sent a final response to our server
%%          transaction yet, it will detect us exiting and generate a
%%          500 Server Internal Error.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({sipproxy_terminating, FromPid}, #state{forkpid=FromPid}=State) when is_pid(FromPid) ->
    #state{callhandler_pid	= CallHandlerPid,
	   forkpid		= ForkPid
	  } = State,
    logger:log(debug, "Appserver glue: received sipproxy_terminating from my ForkPid ~p (CallHandlerPid is ~p) "
	       "- exiting normally (completed: ~p).", [ForkPid, CallHandlerPid, State#state.completed]),
    check_quit({stop, normal, State});

%%--------------------------------------------------------------------
%% @spec    (timeout, State) -> {noreply, NewState, Timeout::integer()}
%%
%% @doc     For some reason, we are still alive. Check if we should
%%          just garbage collect our way out of here. Notes : XXX
%%          maybe we should log this as an error. This should always
%%          be an indication that _something_ is wrong - even if it
%%          really is something in the transaction layer or wherever.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    logger:log(error, "Appserver glue: Still alive after 5 minutes! Exiting. CallHandler '~p', ForkPid '~p'",
	       [State#state.callhandler, State#state.forkpid]),
    util:safe_signal("Appserver glue: ", State#state.forkpid, {showtargets}),
    check_quit({stop, "appserver_glue should not live forever", State});

handle_info(Info, State) ->
    logger:log(error, "Appserver glue: Received unknown signal :~n~p", [Info]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    BranchBase = State#state.branchbase,
    #request{method = Method, uri = URI} = State#state.request,
    case Reason of
	normal ->
	    logger:log(normal, "~s: Appserver glue: Finished with fork (~s ~s), exiting.",
		       [BranchBase, Method, sipurl:print(URI)]);
	_ ->
	    logger:log(error, "~s: Appserver glue: Terminating fork (~s ~s), exiting ABNORMALLY.",
		       [BranchBase, Method, sipurl:print(URI)]),
	    logger:log(debug, "Appserver glue: Abnormally exiting with reason :~n~p", [Reason])
    end,
    Reason.

%%--------------------------------------------------------------------
%% @spec    (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec    (Res) -> term()
%%
%% @equiv    check_quit(Res, none)
%% @end
%%--------------------------------------------------------------------
check_quit(Res) ->
    check_quit(Res, none).

%%--------------------------------------------------------------------
%% @spec    (Res, From) ->
%%            Res                   |
%%            {stop, Reason, State}
%%
%%            Res  = term() "gen_server:call/cast/info() return value"
%%            From = term() "gen_server from-value | none"
%%
%% @doc     Wrapper function checking if our server transaction
%%          (callhandler) or our sipproxy (forkpid) is dead. If so,
%%          If so, turn Res into a {stop, ...} but if Res was a
%%          {reply, ...} do the gen_server:reply() first. Note : Not
%%          all variants of gen_server call/cast/info return values
%%          are covered in these functions - only the ones we
%%          actually use!
%% @end
%%--------------------------------------------------------------------
check_quit(Res, From) ->
    case Res of
	{stop, _Reason, State} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	{noreply, State, _Timeout} when is_record(State, state) ->
	    check_quit2(Res, From, State)
    end.

%% part of check_quit/2
check_quit2(Res, From, #state{cpl_pid = CPLpid, forkpid = FP} = State) when CPLpid /= none, FP == none ->
    %% When we are executing a CPL script, we never have a callhandler, so just check forkpid
    logger:log(debug, "Appserver glue: We are executing on behalf of a CPL script and ForkPid is 'none' - terminating"),
    check_quit2_terminate(Res, From, State);

check_quit2(Res, From, #state{callhandler = CH, forkpid = FP} = State) when CH == none; FP == none ->
    logger:log(debug, "Appserver glue: CallHandler (~p) or ForkPid (~p) is 'none' - terminating",
	       [CH, FP]),
    check_quit2_terminate(Res, From, State);

check_quit2(Res, _From, State) when is_record(State, state) ->
    %% Not time to quit yet
    Res.

%% part of check_quit2(), turn Res into a {stop, ...}.
check_quit2_terminate(Res, _From, State) ->
    NewReply = case Res of
		   {noreply, _State, _Timeout} ->
		       {stop, normal, State};
		   {stop, _Reason, _State} ->
		       Res
	       end,
    NewReply.

%%--------------------------------------------------------------------
%% @spec    (State, Status, Reason) -> term()
%%
%%            State  = #state{}
%%            Status = integer() "SIP status code"
%%            Reason = string() "SIP reason phrase"
%%
%% @equiv    send_final_response(State, Status, Reason, [])
%% @end
%%--------------------------------------------------------------------
send_final_response(State, Status, Reason) ->
    send_final_response(State, Status, Reason, []).

%%--------------------------------------------------------------------
%% @spec    (State, Status, Reason, EH) ->
%%            State
%%
%%            State  = #state{}
%%            Status = integer() "SIP status code"
%%            Reason = string() "SIP reason phrase"
%%            EH     = [{Key, Value}] "extra headers"
%%
%%            State = #state{}
%%
%% @doc     We have a final response to deliver. * Non-CPL : Send a
%%          final response to our server transaction (aka.
%%          callhandler). * CPL : Check if it is a 2xx response. If
%%          it is, we send it to the server transaction directly and
%%          then tell the CPL pid about it. If it is not a 2xx
%%          response, just tell the CPL pid about it.
%% @end
%%--------------------------------------------------------------------
send_final_response(#state{cpl_pid = none} = State, Status, Reason, EH) when is_integer(Status), is_list(Reason) ->
    TH = State#state.callhandler,
    transactionlayer:send_response_handler(TH, Status, Reason, EH),
    State#state{completed = true};
send_final_response(State, Status, Reason, EH) when is_record(State, state), is_integer(Status), is_list(Reason) ->
    Response = {Status, Reason},
    if
	Status >= 200, Status =< 299 ->
	    %% We send 2xx responses to the server transaction directly, and just tell CPL about it afterwards
	    transactionlayer:send_response_handler(State#state.callhandler, Status, Reason, EH);
	true -> true
    end,
    State#state.cpl_pid ! {appserver_glue_final_response, self(), Response},
    State#state{completed = true}.

%%--------------------------------------------------------------------
%% @spec    (Response, State) ->
%%            State
%%
%%            Response = #response{}
%%            State    = #state{}
%%
%%            State = #state{}
%%
%% @doc     Process a response we have received from our sipproxy
%%          process and figure out what to do with it. See if we
%%          should send it on to our parent and record if this was a
%%          final response (set State#state.completed=true if so).
%% @end
%%--------------------------------------------------------------------
%%
%% Method is INVITE and we are already completed
%%
handle_sipproxy_response(Response, #state{request = Request, completed = true}=State)
  when is_record(Response, response), is_record(Request, request), Request#request.method == "INVITE" ->
    URI = Request#request.uri,
    #response{status = Status, reason = Reason} = Response,
    if
	Status >= 200, Status =< 299 ->
	    %% XXX change to debug level? Might be good to see in the log though, since
	    %% resends of the 200 Ok means that the ACK is not getting through.
	    logger:log(normal, "Appserver glue: Forwarding 'late' 2xx response, ~p ~s to INVITE ~s statelessly",
		       [Status, Reason, sipurl:print(URI)]),
	    transportlayer:send_proxy_response(none, Response);
	true ->
	    logger:log(error, "Appserver glue: NOT forwarding non-2xx response ~p ~s to INVITE ~s - " ++
		       "a final response has already been forwarded (sipproxy should not do this!)",
		       [Status, Reason, sipurl:print(URI)]),
	    erlang:error(more_than_one_final_response, [Response, State])
    end,
    State;
%%
%% Method is non-INVITE and we are already completed
%%
handle_sipproxy_response(Response, #state{completed = true} = State) when is_record(Response, response) ->
    #request{method = Method, uri = URI} = State#state.request,
    #response{status = Status, reason = Reason} = Response,
    logger:log(error, "Appserver glue: NOT forwarding response ~p ~s to non-INVITE request ~s ~s - " ++
	       "a final response has already been forwarded (sipproxy should not do this!)",
	       [Status, Reason, Method, sipurl:print(URI)]),
    erlang:error(more_than_one_final_response, [Response, State]);

%%
%% We are not yet completed, this is not a provisional response, and we have a CPL-pid in State.
%% Forward response to the CPL-pid, so that it can take whatever action the CPL-script stipulates.
%%
handle_sipproxy_response(#response{status=Status}=Response, #state{cpl_pid=CPLpid}=State)
  when is_pid(CPLpid), Status >= 200 ->
    #request{method = Method, uri = URI} = State#state.request,
    #response{status = Status, reason = Reason} = Response,
    %% We always tell CallHandler about 2xx responses
    if
	Status >= 200, Status =< 299 ->
	    CallHandler = State#state.callhandler,
	    logger:log(debug, "Appserver glue: Forwarding response '~p ~s' to '~s ~s' to CallHandler ~p",
		       [Status, Reason, Method, sipurl:print(URI), CallHandler]),
	    transactionlayer:send_proxy_response_handler(CallHandler, Response);
	true -> true
    end,
    %% Now, tell CPLpid about the final response
    CPLpid = State#state.cpl_pid,
    logger:log(debug, "Appserver glue: Forwarding final response '~p ~s' to '~s ~s' to CPL-pid ~p",
	       [Status, Reason, Method, sipurl:print(URI), CPLpid]),
    CPLpid ! {appserver_glue_final_response, self(), Response},
    State#state{completed = true};
%%
%% We are not yet completed, and State does not contain a CPL-pid, or this is a provisional
%% response so we basically forward the response to our CallHandler, and if if it >= 200 we
%% set 'completed' to 'true'.
%%
handle_sipproxy_response(Response, State) when is_record(Response, response), is_record(State, state) ->
    #request{method = Method, uri = URI} = State#state.request,
    #response{status = Status, reason = Reason} = Response,
    CallHandler = State#state.callhandler,
    logger:log(debug, "Appserver glue: Forwarding response '~p ~s' to '~s ~s' to CallHandler ~p",
	       [Status, Reason, Method, sipurl:print(URI), CallHandler]),
    transactionlayer:send_proxy_response_handler(CallHandler, Response),
    if
	Status >= 200 ->
	    State#state{completed = true};
	true ->
	    State
    end.
