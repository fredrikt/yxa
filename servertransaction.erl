%%%-------------------------------------------------------------------
%%% File    : servertransaction.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : A server transaction gen_server.
%%%
%%% Created : 05 Feb 2004 by Fredrik Thulin <ft@lab08.lab.it.su.se>
%%%-------------------------------------------------------------------
-module(servertransaction).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("siprecords.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	  branch,
	  logtag,
	  socket,
	  report_to,
	  request,
	  response,
	  sipmethod,
	  sipstate,
	  cancelled=false,
	  timerlist,
	  mode,
	  my_to_tag
	 }).

-define(TIMEOUT, 300 * 1000).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Request, Socket, LogStr, AppModule, Mode) ->
    gen_server:start_link(?MODULE, [Request, Socket, LogStr, AppModule, Mode], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, Socket, LogStr, AppModule, Mode])
%%           Request = request record()
%%           Socket = sipsocket record(), socket this request arrived
%%                                        on
%%           LogStr = string(), describes request
%%           AppModule = atom(), Yxa application module name)
%%           Mode = atom(), stateless | stateful
%% Descrip.: Initiates the gen_server
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------

%%
%% ACK - no go
%%
init([Request, _Socket, _LogStr, _AppModule, _Mode]) when record(Request, request), Request#request.method == "ACK" ->
    %% Although a bit vague, RFC3261 section 17 (Transactions) do say that
    %% it is not allowed to send responses to ACK requests, so we simply
    %% deny to start a server transaction here.
    logger:log(error, "Server transaction: NOT starting transaction for ACK request (ACK ~s)",
	       [sipurl:print(Request#request.uri)]),
    {stop, "Not starting server transaction for ACK"};
%%
%% Anything but ACK
%%
init([Request, Socket, LogStr, _AppModule, Mode]) when record(Request, request) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    Branch = siprequest:generate_branch() ++ "-UAS",
    LogTag = Branch ++ " " ++ Method,
    MyToTag = generate_tag(),
    %% LogTag is essentially Branch + Method, LogStr is a string that
    %% describes this request (METHOD URI [client=x, from=y, to=z])
    State = #state{branch=Branch, logtag=LogTag, socket=Socket, request=Request,
		   sipmethod=Method, sipstate=trying, timerlist=siptimer:empty(),
		   mode=Mode, my_to_tag=MyToTag},
    logger:log(debug, "~s: Server transaction: Started new server transaction for request ~s ~s.",
	       [LogTag, Method, sipurl:print(URI)]),
    logger:log(normal, "~s: ~s", [LogTag, LogStr]),
    %% RFC3261 17.2.1 says the _transaction layer_ MUST generate a 100 Trying in response
    %% to an INVITE unless it _knows_ the TU will generate a response within 200 ms. We
    %% can't know that, so we generate a 100 Trying if this application is stateful.
    MyState = if
		  Method == "INVITE", State#state.mode == stateful ->
		      Response = make_response(100, "Trying", "", [], [], State),
		      {ok, NewStateIn1} = do_response(created, Response, State),
		      NewStateIn1;
		  true ->
		      State
	      end,
    {ok, MyState, ?TIMEOUT}.


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

handle_call({get_branch}, From, State) ->
    check_quit({reply, {ok, State#state.branch}, State, ?TIMEOUT}, From);

handle_call({set_report_to, Pid}, From, State) ->
    LogTag = State#state.logtag,
    Reply = case State#state.report_to of
		undefined ->
		    case State#state.cancelled of
			true ->
			    logger:log(debug, "~s: Pid ~p attempted to adopt cancelled server transaction",
				      [LogTag, Pid]),
			    {reply, {error, cancelled}, State, ?TIMEOUT};
			false ->
			    logger:log(debug, "~s: Server transaction adopted by ~p", [LogTag, Pid]),
			    {reply, {ok}, State#state{report_to=Pid}, ?TIMEOUT}
		    end;
		_ ->
		    {reply, {error, "Already set"}, State, ?TIMEOUT}
	    end,
    check_quit(Reply, From);

handle_call(Request, From, State) ->
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received unknown gen_server call from ~p :~n~p",
	       [LogTag, From, Request]),
    Reply = ok,
    check_quit({reply, Reply, State, ?TIMEOUT}, From).

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast({sipmessage, Request, Origin, GenSrvFrom, AppModule}, State) when record(Request, request), record(Origin, siporigin) ->
    LogTag = State#state.logtag,
    OrigRequest = State#state.request,
    {OrigMethod, OrigURI, OrigHeader} = {OrigRequest#request.method, OrigRequest#request.uri, OrigRequest#request.header},
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    logger:log(debug, "~s: Server transaction received a request (~s ~s), checking if it is an ACK or a resend",
	       [LogTag, Method, sipurl:print(URI)]),
    {CSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    {OrigCSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", OrigHeader)),
    Reply = if
		OrigMethod == "INVITE", Method == "ACK", URI == OrigURI ->
		    %% Received an ACK with a Request-URI that matches our original one
		    LogTag = State#state.logtag,
		    NewState = case State#state.mode of
				   stateful ->
				       gen_server:reply(GenSrvFrom, {continue}),
				       process_received_ack(State);
				   stateless ->
				       logger:log(debug, "~s: Received ACK ~s when stateless, pass to core.",
						  [LogTag, sipurl:print(OrigURI)]),
				       gen_server:reply(GenSrvFrom, {pass_to_core, AppModule}),
				       State
			       end,
		    {noreply, NewState, ?TIMEOUT};
		Method == OrigMethod, URI == OrigURI, CSeqNum == OrigCSeqNum ->
		    %% This is a resent request, check if we have a response stored that we can resend
		    case State#state.response of
			Re when record(Re, response) ->
			    logger:log(normal, "~s: Received a retransmission of request (~s ~s), resending response ~p ~s",
				       [LogTag, Method, sipurl:print(URI), Re#response.status, Re#response.reason]),
			    gen_server:reply(GenSrvFrom, {continue}),
			    transportlayer:send_proxy_response(State#state.socket, Re);
			_ ->
			    %% No response stored, check if we are stateless or stateful
			    case State#state.mode of
				stateful ->
				    logger:log(normal, "~s: Received a retransmission of request (~s ~s) but I have no " ++
					       "response to resend!", [LogTag, Method, sipurl:print(URI)]),
				    gen_server:reply(GenSrvFrom, {continue});
				stateless ->
				    logger:log(debug, "~s: Received a resend of original request (~s ~s) - pass to core",
					       [LogTag, Method, sipurl:print(URI)]),
				    %% It looks like we are a stateless server transaction. Tell ServerPid
				    %% to pass this retransmission on to the core.
				    gen_server:reply(GenSrvFrom, {pass_to_core, AppModule})
			    end
		    end,
		    {noreply, State, ?TIMEOUT};
		true ->
		    logger:log(normal, "~s: Server transaction: Received request is not particulary alike the first one " ++
			       "(~p ~s ~s /= ~p ~s ~s). Dropping it on the floor.",
			       [LogTag, CSeqNum, Method, sipurl:print(URI), OrigCSeqNum, OrigMethod, sipurl:print(OrigURI)]),
		    gen_server:reply(GenSrvFrom, {continue}),
		    {noreply, State, ?TIMEOUT}
	    end,
    check_quit(Reply);

handle_cast({create_response, Status, Reason, ExtraHeaders, RBody}, State) ->
    Response = make_response(Status, Reason, RBody, ExtraHeaders, [], State),
    {ok, NewState1} = do_response(created, Response, State),
    check_quit({noreply, NewState1, ?TIMEOUT});

handle_cast({forwardresponse, Response}, State) when record(Response, response) ->
    {ok, NewState1} = do_response(forwarded, Response, State),
    check_quit({noreply, NewState1, ?TIMEOUT});

handle_cast({forwardresponse, {Status, Reason, Header, Body}}, State) ->
    Response = #response{status=Status, reason=Reason, header=Header, body=Body},
    {ok, NewState1} = do_response(forwarded, Response, State),
    check_quit({noreply, NewState1, ?TIMEOUT});

handle_cast({expired}, State) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Received signal that I am expired, exiting.", [LogTag]),
    check_quit({stop, "Server transaction expired", State});

handle_cast({cancelled}, State) ->
    LogTag = State#state.logtag,
    SipState = State#state.sipstate,
    Reply = case lists:member(SipState, [completed, confirmed, terminated]) of
		false ->
		    ReportTo = case util:safe_is_process_alive(State#state.report_to) of
				   {true, R} ->
				       R;
				   {false, R} ->
				       logger:log(debug, "~s: Server transaction orphaned ('~p' not alive) - can't inform parent that I have been cancelled",
						  [LogTag, R])
			       end,
		    case ReportTo of
			_ when pid(ReportTo) ->
			    logger:log(debug, "~s: Server transaction cancelled, telling parent ~p", [LogTag, ReportTo]),
			    %% We don't generate the 487 Request Cancelled here, parent does other stuff first and then
			    %% tell us to send that response.
			    ReportTo ! {servertransaction_cancelled, self()},
			    {noreply, State#state{cancelled=true}, ?TIMEOUT};
			_ ->
			    %% Noone has adopted this server transaction yet (called set_report_to) - cancel the request
			    %% and store that we have been cancelled so that we can notify whoever tries to adopt us
			    %% later on.
			    logger:log(debug, "~s: Server transaction cancelled, acting alone. Answering 487 Request Cancelled.", [LogTag]),
			    Response = make_response(487, "Request Cancelled", "", [], [], State),
			    NewState1 = State#state{cancelled=true},
			    {ok, NewState} = do_response(created, Response, NewState1),
			    {noreply, NewState, ?TIMEOUT}
		    end;
		_ ->
		    logger:log(debug, "~s: Server transaction cancelled when in state ~p - ignoring.",
			       [LogTag, SipState]),
		    {noreply, State, ?TIMEOUT}
	    end,
    check_quit(Reply);

handle_cast({quit}, State) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Got signal to quit right away, doing so (when in state ~p)",
	       [LogTag, State#state.sipstate]),
    check_quit({stop, normal, State});

handle_cast(Msg, State) ->
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received unknown gen_server cast :~n~p",
	       [LogTag, Msg]),
    check_quit({noreply, State, ?TIMEOUT}).

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    Reply = case State#state.mode of
		stateful ->
		    RStr = case State#state.response of
			       Response when record(Response, response) ->
				   io_lib:format("last sent response was ~p ~s", [Response#response.status, Response#response.reason]);
			       undefined ->
				   "no responses sent"
			   end,
		    logger:log(error, "~s: Stateful server transaction (~s ~s) still alive after 5 minutes! State is ~p, ~s",
			       [LogTag, Method, sipurl:print(URI), State#state.sipstate, RStr]),
		    transactionlayer:debug_show_transactions(),
		    {noreply, State, ?TIMEOUT};
		stateless ->
		    logger:log(debug, "~s: Stateless server transaction (~s ~s) terminating without having sent a response after 5 minutes",
			       [LogTag, Method, sipurl:print(URI)]),
		    {stop, "Stateless server transaction handler idle for 5 minutes", State}
	    end,
    check_quit(Reply);

handle_info({siptimer, TRef, TDesc}, State) ->
    LogTag = State#state.logtag,
    Reply = case siptimer:get_timer(TRef, State#state.timerlist) of
		none ->
		    logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring. LIST ~n~p", [LogTag, TRef, TDesc, State#state.timerlist]),
		    {noreply, State, ?TIMEOUT};
		Timer ->
		    case process_timer(Timer, State) of
			NewState when record(NewState, state) ->
			    {noreply, NewState, ?TIMEOUT};
			Unknown ->
			    logger:log(error, "~s: Server transaction: process_timer() returned unknown result :~n~p",
				       [LogTag, Unknown]),
			    {noreply, State, ?TIMEOUT}
		    end
	    end,
    check_quit(Reply);

handle_info(Info, State) ->
    logger:log(error, "Server transaction: Received unknown gen_server info :~n~p",
	       [Info]),
    check_quit({noreply, State, ?TIMEOUT}).

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    LogTag = State#state.logtag,
    case Reason of
        normal -> true;
        _ -> logger:log(error, "~s: Server transaction terminating : ~p", [LogTag, Reason])
    end,
    case util:safe_is_process_alive(State#state.report_to) of
	{true, R} ->
	    R ! {servertransaction_terminating, self()};
	{false, undefined} ->
	    %% Don't log when we have never had a report_to
	    true;
	{false, R} ->
	    logger:log(debug, "~s: Server transaction orphaned ('~p' not alive) - can't inform parent that I am terminating now",
		       [LogTag, R])
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
		       Re when record(Re, response) ->
			   io_lib:format("last sent response was ~p ~s", [Re#response.status, Re#response.reason]);
		       undefined ->
			   "no responses sent"
		   end,
	    Request = State#state.request,
	    {Method, URI} = {Request#request.method, Request#request.uri},
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
send_reply(To, Reply, State) when record(State, state)   ->
    gen_server:reply(To, Reply).

process_timer(Timer, State) when record(State, state)    ->
    [TRef, Timeout, Signal, Description] = siptimer:extract([ref, timeout, appsignal, description], Timer),
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    logger:log(debug, "~s: Timer ~p:~p fired", [LogTag, TRef, Description]),
    case Signal of

	{resendresponse} ->
	    case State#state.response of
		Response when record(Response, response) ->
		    {Status, Reason} = {Response#response.status, Response#response.reason},
		    case State#state.sipstate of
			completed ->
			    logger:log(debug, "~s: Resend after ~p (response to ~s ~s): ~p ~s",
			    	       [LogTag, siptimer:timeout2str(Timeout), Method, sipurl:print(URI), Status, Reason]),
			    transportlayer:send_proxy_response(State#state.socket, State#state.response),
			    NewTimeout = case Method of
					     "INVITE" ->
						 %% Use Timer Value T2 for INVITE responses
						 T2 = sipserver:get_env(timerT2, 4000),
						 lists:min([Timeout * 2, T2]);
					     _ ->
						 Timeout * 2
					 end,
			    NewTimerList = siptimer:revive_timer(Timer, NewTimeout, State#state.timerlist),
			    State#state{timerlist=NewTimerList};
			_ ->
			    logger:log(debug, "~s: Ignoring signal to resend response to (~s ~s): ~p ~s since we are in state '~p'",
			    	       [LogTag, Method, sipurl:print(URI), Status, Reason, State]),
			    State
		    end;
		_ ->
		    logger:log(error, "~s: Resend after ~p (response to ~s ~s, state '~p'): no response sent!",
			       [Timeout, Method, sipurl:print(URI), State]),
		    State
	    end;

	{resendresponse_timeout} ->
	    Response = State#state.response,
	    {Status, Reason} = {Response#response.status, Response#response.reason},
	    logger:log(normal, "~s: Sending of ~p ~s (response to ~s ~s) timed out after ~s seconds, terminating transaction.",
		       [LogTag, Status, Reason, Method, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
	    %% XXX report to TU?
	    enter_sip_state(terminated, State);

	{terminate_transaction} ->
	    logger:log(debug, "~s: Received timer signal to terminate transaction", [LogTag]),
	    enter_sip_state(terminated, State);

	_ ->
	    logger:log(error, "~s: Received unknown signal from timer ~p: ~p", [LogTag, TRef, Signal]),
	    State
    end.

do_response(Created, Response, State) when record(State, state), record(Response, response) ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    {ResponseToMethod, ResponseToURI} = {Request#request.method, Request#request.uri},
    OldSipState = State#state.sipstate,
    {Status, Reason} = {Response#response.status, Response#response.reason},
    NewState1 = go_stateful(Created, ResponseToMethod, Status, LogTag, State),
    NewState = 
	case catch send_response_statemachine(ResponseToMethod, Status, OldSipState) of
	    {Action, SendReliably, NewSipState} when Action == ignore; Action == send ->
		NewState2 = case Action of
				ignore ->
				    NewState1;
				send ->
				    LogLevel = if
						   Status >= 200 -> normal;
						   true -> debug
					       end,
				    What = case Created of
					       created -> "Responding";
					       _ -> "Forwarding response"
					   end,
				    logger:log(LogLevel, "~s: ~s ~p ~s",
					       [LogTag, What, Status, Reason]),
				    {ok, NewState2_1} = send_response(Response, SendReliably, NewState1),
				    NewState2_1
			    end,
		enter_sip_state(NewSipState, NewState2);
	    E ->
		logger:log(error, "~s: State machine does not allow us to send '~p ~s' in response to '~s ~s' when my SIP-state is '~p'",
			   [LogTag, Status, Reason, ResponseToMethod, sipurl:print(ResponseToURI), OldSipState]),
		logger:log(debug, "~s: State machine returned :~n~p", [LogTag, E]),
		NewState1
	end,
    {ok, NewState}.

go_stateful(created, Method, Status, LogTag, State) when record(State, state), State#state.mode == stateless, Status >= 200 ->
    logger:log(debug, "~s: Going stateful when sending locally created final response ~p response to ~s",
	       [LogTag, Status, Method]),
    State#state{mode=stateful};
go_stateful(_, _, _, _, State) ->
    State.

send_response_statemachine(Method, Status, trying) when Status == 100 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'trying' - " ++
	       "doing so (unreliably) and entering state 'proceeding'", [Status, Method]),
    {send, false, proceeding};

send_response_statemachine(Method, Status, State) when Status == 100 ->
    logger:log(debug, "UAS decision: Requested to send 100 Trying to ~s when in state '~p' - " ++
	       "ignoring", [Method, State]),
    {ignore, false, State};

send_response_statemachine(Method, Status, trying) when Status =< 199 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'trying' - " ++
	       "doing so (unreliably) and entering state 'proceeding'", [Status, Method]),
    {send, false, proceeding};

send_response_statemachine(Method, Status, proceeding) when Status =< 199 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'proceeding' - " ++
	       "doing so (unreliably)", [Status, Method]),
    {send, false, proceeding};


send_response_statemachine("INVITE", Status, proceeding) when Status =< 299 ->
    logger:log(debug, "UAS decision: Requested to send 2xx response ~p to INVITE when in state 'proceeding' - " ++
	       "doing so (unreliably) and entering state 'terminated'", [Status]),
    {send, false, terminated};


send_response_statemachine("INVITE", Status, proceeding) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 3xx, 4xx, 5xx or 6xx response ~p to INVITE when in state 'proceeding' - " ++
	       "doing so (reliably) and entering state 'completed'", [Status]),
    {send, true, completed};

send_response_statemachine(Method, Status, trying) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when in state 'trying' - " ++
	       "doing so (unreliably) and entering state 'completed'", [Status, Method]),
    {send, false, completed};

send_response_statemachine(Method, Status, proceeding) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when in state 'proceeding' - " ++
	       "doing so (unreliably) and entering state 'completed'", [Status, Method]),
    {send, false, completed};

send_response_statemachine(Method, Status, completed) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2xx, 3xx, 4xx, 5xx or 6xx response ~p to ~s when already in state 'completed' - " ++
	       "ignoring", [Status, Method]),
    {ignore, false, completed};

send_response_statemachine(Method, Status, terminated) when Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send response ~p to ~s when in state 'terminated' - " ++
	       "ignoring", [Status, Method]),
    {ignore, false, terminated}.


%% This branch should answer something.
send_response(Response, SendReliably, State) when record(Response, response), record(State, state) ->
    {Status, Reason, RHeader} = {Response#response.status, Response#response.reason, Response#response.header},
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    Socket = State#state.socket,
    LogTag = State#state.logtag,
    if
	Method == "INVITE", Status >= 300 ->
	    %% Tell transactionlayer pid about this response to INVITE - it needs to know
	    %% what To-tag we have used in order to match any ACK:s received for this
	    %% response back to this server transaction.
	    ToTag = sipheader:get_tag(keylist:fetch("To", RHeader)),
	    case transactionlayer:store_to_tag(Request, ToTag) of
		ok -> true;
		R ->
		    %% XXX abort sending?
		    logger:log(error, "~s: Failed storing To-tag with transactionlayer : ~p", [LogTag, R])
	    end;
	true -> true
    end,
    logger:log(debug, "~s: Sending response to ~s ~s : ~p ~s", 
	       [LogTag, Method, sipurl:print(URI), Status, Reason]),
    transportlayer:send_proxy_response(Socket, Response),
    NewState1 = case SendReliably of
		    true ->
			T1 = sipserver:get_env(timerT1, 500),
			case Method of
			    "INVITE" ->
				TimerG = T1,
				TimerH = 64 * T1,
				GDesc = "resendresponse " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++ sipurl:print(URI) ++ " (Timer G)",
				HDesc = "stopresend of " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++ sipurl:print(URI) ++ " (Timer H)",
				NewState2 = add_timer(TimerG, GDesc, {resendresponse}, State),
				NewState3 = add_timer(TimerH, HDesc, {resendresponse_timeout}, NewState2),
				NewState3;
			    _ ->
				TimerJ = 64 * T1,
				JDesc = "terminate server transaction after response " ++ integer_to_list(Status) ++ " " ++ Reason ++ " has been sent to " ++ sipurl:print(URI) ++ " (Timer J)",
				NewState2 = add_timer(TimerJ, JDesc, {terminate_transaction}, State),
				NewState2
			end;
		    false ->
			State
		end,
    NewState = NewState1#state{response=Response},
    {ok, NewState}.

add_timer(Timeout, Description, AppSignal, State) when record(State, state) ->
    NewTimerList = siptimer:add_timer(Timeout, Description, AppSignal, State#state.timerlist),
    State#state{timerlist=NewTimerList}.

enter_sip_state(NewSipState, State) when record(State, state) ->
    LogTag = State#state.logtag,
    OldSipState = State#state.sipstate,
    case NewSipState of
	OldSipState ->
	    State;
	_ ->
	    NewState1 = State#state{sipstate=NewSipState},
            Request = State#state.request,
            {ResponseToMethod, ResponseToURI} = {Request#request.method, Request#request.uri},
	    NewState = case NewSipState of
			   completed ->
			       case ResponseToMethod of
				   "INVITE" ->
				       NewState1;
				   _ ->
				       T1 = sipserver:get_env(timerT1, 500),
				       TimerJ = 64 * T1,
				       logger:log(debug, "~s: Server transaction: Entered state 'completed'. Original request was non-INVITE, " ++
						  "starting Timer J with a timeout of ~s seconds.",
						  [LogTag, siptimer:timeout2str(TimerJ)]),
				       %% Install TimerJ (default 32 seconds) RFC 3261 17.2.2. Until TimerJ fires we
				       %% resend our response whenever we receive a request resend.
				       JDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++ " (Timer J)",
				       add_timer(TimerJ, JDesc, {terminate_transaction}, NewState1)
			       end;
			   confirmed ->
			       case ResponseToMethod of
				   "INVITE" ->
				       TimerI = sipserver:get_env(timerT4, 5000),
				       logger:log(debug, "~s: Entered state 'confirmed'. Original request was an INVITE, starting " ++ 
						  "Timer I with a timeout of ~s seconds.",  [LogTag, siptimer:timeout2str(TimerI)]),
				       %% Install TimerI (T4, default 5 seconds) RFC 3261 17.2.1. Until TimerI fires we
				       %% absorb any additional ACK requests that might arrive.
				       IDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++ " (Timer I)",
				       add_timer(TimerI, IDesc, {terminate_transaction}, NewState1);
				   _ ->
				       logger:log(error, "~s: Entered state 'confirmed'. Original request was NOT an INVITE (it was ~s ~s). " ++
						  "How could this be?", [LogTag, ResponseToMethod, sipurl:print(ResponseToURI)]),
				       NewState1
			       end;
			   _ ->
			       logger:log(debug, "~s: Entering state '~p'", [LogTag, NewSipState]),
			       NewState1
		       end,
	    NewState
    end.

process_received_ack(State) when is_record(State, state), is_record(State#state.response, response) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    Response = State#state.response,
    {Status, Reason} = {Response#response.status, Response#response.reason},
    LogTag = State#state.logtag,
    logger:log(normal, "~s: Response ~p ~s to request ~s ~s ACK-ed", [LogTag, Status, Reason, Method, sipurl:print(URI)]),
    case Method of
	"INVITE" ->
	    SipState = State#state.sipstate,
	    case lists:member(SipState, [trying, proceeding]) of
		true ->
		    logger:log(error, "~s: Received ACK when in state '~p' - ignoring", [LogTag, SipState]),
		    State;
		false ->
		    logger:log(debug, "~s: Received ACK, cancelling resend timers for response ~p ~s (to request ~s ~s) and entering state 'confirmed'",
			       [LogTag, Status, Reason, Method, sipurl:print(URI)]),
		    TimerList = State#state.timerlist,
		    NewTimerList = siptimer:cancel_timers_with_appsignal({resendresponse}, TimerList),
		    NewState1 = State#state{timerlist=NewTimerList},
		    NewState = enter_sip_state(confirmed, NewState1),
		    NewState
	    end;
	_ ->
	    logger:log(debug, "~s: Received ACK to non-INVITE request ~s ~s (response being ACKed is ~p ~s) - ignoring",
		       [LogTag, Method, sipurl:print(URI), Status, Reason]),
	    State
    end;
process_received_ack(State) when is_record(State, state) ->
    %% State#state.response is not a response record()
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received an ACK before I sent any responses, ignoring", [LogTag]),
    State.

make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State) when record(State, state), Status == 100 ->
    siprequest:make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State#state.socket, State#state.request);

make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State) when record(State, state) ->
    Request = State#state.request,
    Header = Request#request.header,
    To = keylist:fetch("To", Header),
    Req = case sipheader:get_tag(To) of
	      none ->
		  {DistplayName, ToURI} = sipheader:to(To),
		  NewTo = lists:concat([sipheader:to_print({DistplayName, ToURI}), ";tag=",
					State#state.my_to_tag]),
		  NewHeader = keylist:set("To", [NewTo], Header),
		  Request#request{header=NewHeader};
	      _ ->
		  Request
	  end,
    siprequest:make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State#state.socket, Req).

generate_tag() ->
    %% Erlang guarantees that subsequent calls to now() generate increasing values (on the same node).
    {Megasec, Sec, Microsec} = now(),
    In = lists:concat([node(), Megasec * 1000000 + Sec, 8, $., Microsec]),
    Out = siprequest:make_base64_md5_token(In),
    %% RFC3261 #19.3 says tags must have at least 32 bits randomness,
    %% don't make them longer than they have to be.
    "yxa-" ++ string:substr(Out, 1, 9).
