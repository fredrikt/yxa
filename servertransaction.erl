%%%-------------------------------------------------------------------
%%% File    : servertransaction.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: A server transaction gen_server.
%%%
%%% Created : 05 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%%
%%% Note    : Perhaps we should generate a 500 if we don't get adopted
%%%           in a few seconds from when we start?
%%%-------------------------------------------------------------------
-module(servertransaction).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/3,
	 test/0
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
-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("transactionstatelist.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  branch,		%% string(), our branch identifier
	  logtag,		%% string(), prefix to use when logging
	  socket,		%% sipsocket record(), the socket the request was received on -
	  			%% RFC3261 requires us to send responses using the very same socket
	  report_to,		%% undefined | pid(), to whom we should report if we are cancelled
	  parent,		%% pid(), our parent process - to handle trapped EXITs from it
	  request,		%% request record(), the request we are handling
	  response,		%% undefined | response record(), the last response we sent
	  sipstate,		%% atom(), trying|proceeding|completed|confirmed|terminated
	  cancelled=false,	%% bool(), have we been cancelled?
	  timerlist,		%% siptimerlist record(), our current set of timers
	  my_to_tag		%% string(), To-tag to use if we generate a response (as opposed to
	  			%% if we forward a response)
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(TIMEOUT, 300 * 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link(Request, Socket, LogStr)
%%           start(Request, Socket, LogStr)
%%           Request   = request record()
%%           Socket    = sipsocket record(), the socket this request
%%                                         was received on
%%           LogStr    = string(), description of request
%% Descrip.: Starts the server
%% Returns : gen_server:start_link/4
%%--------------------------------------------------------------------
start_link(Request, Socket, LogStr) ->
    %% It is intentional to call gen_server:start(...) here even though
    %% this function is called start_link. That is because of a 'problem'
    %% with gen_servers in Erlang/OTP (at least R10B-2). If you use
    %% gen_server:start_link(...) to start your gen_server, you won't be
    %% able to trap 'EXIT' signals from the parent process, even if you
    %% set process_flag(trap_exit, true)! We set up a link to this process
    %% in the init/1 callback to achieve the same effect (although with
    %% a bitter taste).
    gen_server:start(?MODULE, [Request, Socket, LogStr, self()], []).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Request, Socket, LogStr])
%%           Request   = request record()
%%           Socket    = sipsocket record(), the socket this request
%%                                         was received on
%%           LogStr    = string(), description of request
%% Descrip.: Initiates the server transaction gen_server
%% Returns : {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------

%%
%% ACK - no go
%%
init([#request{method="ACK"}=Request, _Socket, _LogStr, _Parent]) ->
    %% Although a bit vague, RFC3261 section 17 (Transactions) do say that
    %% it is not allowed to send responses to ACK requests, so we simply
    %% deny to start a server transaction here. The transcation_layer will
    %% never try to start a server transcation for ACK requests anyways.
    logger:log(error, "Server transaction: NOT starting transaction for ACK request (ACK ~s)",
	       [sipurl:print(Request#request.uri)]),
    {stop, "Not starting server transaction for ACK"};

%%
%% Anything but ACK
%%
init([Request, Socket, LogStr, Parent]) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    Branch = siprequest:generate_branch() ++ "-UAS",
    Desc = lists:concat([Branch, ": ", Method, " ", sipurl:print(URI)]),
    %% Get ourselves into the transaction state list first of all
    case transactionstatelist:add_server_transaction(Request, self(), Desc) of
	ok ->
	    %% Link to transaction_layer immediately (so that it removes this
	    %% transaction from the transactionstatelist when we exit/crash).
	    TPid = erlang:whereis(transaction_layer),
	    true = link(TPid),
	    %% Link to parent from init/1 instead of using gen_server:start_link to
	    %% be able to trap EXIT signals from parent process. See comment in start_link
	    %% above for more details.
	    true = link(Parent),
	    process_flag(trap_exit, true),
	    case init2([Request, Socket, LogStr, Branch, Parent]) of
		{ok, State, Timeout} when is_record(State, state) ->
		    {ok, State, Timeout};
		Reply ->
		    Reply
	    end;
	{duplicate, TState} when is_record(TState, transactionstate) ->
	    %% We are the losing party of a race. Notify the winner that there was
	    %% a resend (us) and then exit. XXX implement the notifying, for now
	    %% just log.
	    logger:log(normal, "~s: Early resend, exiting.", [LogStr]),
	    {stop, resend}
    end.

init2([Request, Socket, LogStr, Branch, Parent]) when is_record(Request, request) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = Branch ++ " " ++ Method,
    MyToTag = generate_tag(),

    %% LogTag is essentially Branch + Method, LogStr is a string that
    %% describes this request (METHOD URI [client=x, from=y, to=z])
    State = #state{branch=Branch, logtag=LogTag, socket=Socket, request=Request, sipstate=trying,
		   timerlist=siptimer:empty(), my_to_tag=MyToTag, parent=Parent},

    logger:log(debug, "~s: Started new server transaction for request ~s ~s.",
	       [LogTag, Method, sipurl:print(URI)]),
    logger:log(normal, "~s: ~s", [LogTag, LogStr]),

    %% Create event about new request received
    DId = sipheader:dialogid(Request#request.header),
    event_handler:new_request(Method, URI, Branch, DId),

    %% RFC3261 17.2.1 says the _transaction layer_ MUST generate a 100 Trying in response
    %% to an INVITE unless it _knows_ the TU will generate a response within 200 ms. We
    %% can't know that, so we generate a 100 Trying.
    NewState =
	if
	    Method == "INVITE" ->
		Response = make_response(100, "Trying", <<>>, [], [], State),
		{ok, NewState1} = do_response(created, Response, State),
		NewState1;
	    true ->
		State
	end,

    {ok, NewState, ?TIMEOUT}.


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

%%--------------------------------------------------------------------
%% Function: handle_call({get_branch}, From, State)
%% Descrip.: This is a request to get our generated branch.
%% Returns : {reply, Reply, State, Timeout} |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           Reply  = {ok, Branch}
%%           Branch = string()
%%--------------------------------------------------------------------
handle_call({get_branch}, From, State) ->
    check_quit({reply, {ok, State#state.branch}, State, ?TIMEOUT}, From);

%%--------------------------------------------------------------------
%% Function: handle_call({set_report_to, Pid}, From, State)
%%           Pid = pid()
%% Descrip.: This is a request to set our report_to. This is typically
%%           done by a TU (Transaction User (Yxa application)) that
%%           wants to receive notice if we terminate etc.
%%           We can only report to one process, so this function will
%%           fail if our report_to is already set. It will also fail
%%           if we have already been cancelled.
%% Returns : {reply, Reply, State, Timeout} |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           Reply  = ok               |
%%                    {error, Error}   |
%%                    {ignore, Reason} |
%%           Error  = string()
%%           Reason = cancelled | completed
%% Note    : If we already have a report_to, we will reject the call
%%           even if report_to is the same pid as the call asks us to
%%           set it to. This might be relaxed later, but for now you
%%           should not try to adopt a server transcation twice.
%%--------------------------------------------------------------------
handle_call({set_report_to, Pid}, _From, #state{report_to=RTo}=State) when is_pid(Pid), is_pid(RTo) ->
    {reply, {error, "Server transaction already adopted"}, State, ?TIMEOUT};
handle_call({set_report_to, Pid}, From, #state{report_to=undefined}=State) when is_pid(Pid) ->
    LogTag = State#state.logtag,
    Reply =
	case State#state.cancelled of
	    true ->
		logger:log(debug, "~s: Pid ~p attempted to adopt cancelled server transaction",
			   [LogTag, Pid]),
		{reply, {ignore, cancelled}, State, ?TIMEOUT};
	    false ->
		case lists:member(State#state.sipstate, [trying, proceeding]) of
		    true ->
			logger:log(debug, "~s: Server transaction adopted by ~p", [LogTag, Pid]),
			{reply, ok, State#state{report_to=Pid}, ?TIMEOUT};
		    false ->
			logger:log(debug, "~s: Pid ~p attempted to adopt cancelled server transaction"
				   "already in state '~p'", [LogTag, Pid, State#state.sipstate]),
			{reply, {ignore, completed}, State, ?TIMEOUT}
		end
	end,
    check_quit(Reply, From);

handle_call(Request, From, State) ->
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received unknown gen_server call from ~p :~n~p",
	       [LogTag, From, Request]),
    Reply = {reply, error, State, ?TIMEOUT},
    check_quit(Reply, From).


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_cast({siprequest, Request, Origin}, State)
%%           Request = request record()
%%           Origin  = siporigin record()
%% Descrip.: The transaction layer has received a request that matches
%%           this server transaction. It should be either a resend of
%%           the previous request, or an ACK that matches a non-2xx
%%           response to INVITE that we are sending reliably.
%% Returns : {noreply, NewState, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_cast({siprequest, Request, Origin}, State) when is_record(Request, request),
						       is_record(Origin, siporigin) ->
    {LogTag, OrigRequest} = {State#state.logtag, State#state.request},
    {OrigMethod, OrigURI, OrigHeader} = {OrigRequest#request.method, OrigRequest#request.uri,
					 OrigRequest#request.header},
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    logger:log(debug, "~s: Server transaction received a request (~s ~s), checking if it is an ACK or a resend",
	       [LogTag, Method, sipurl:print(URI)]),
    {CSeqNum, _} = sipheader:cseq(Header),
    {OrigCSeqNum, _} = sipheader:cseq(OrigHeader),
    %% For ACK, use sipurl:url_is_equal() to determine if the Request-URI is equal to
    %% the original one. A resent request's URI should be byte-by-byte comparable to
    %% the original one, so in that case we don't use sipurl:url_is_equal().
    URIisEqual = sipurl:url_is_equal(OrigURI, URI),
    NewState =
	if
	    OrigMethod == "INVITE", Method == "ACK", URIisEqual == true ->
		%% Received an ACK with a Request-URI that matches our original one
		LogTag = State#state.logtag,
		process_received_ack(State);
	    Method == OrigMethod, URI == OrigURI, CSeqNum == OrigCSeqNum ->
		%% This is a resent request, check if we have a response stored that we can resend
		case State#state.response of
		    Re when is_record(Re, response) ->
			{Status, Reason} = {Re#response.status, Re#response.reason},
			logger:log(normal, "~s: Received a retransmission of request (~s ~s), resending "
				   "response '~p ~s'", [LogTag, Method, sipurl:print(URI), Status, Reason]),
			transportlayer:send_proxy_response(State#state.socket, Re);
		    _ ->
			%% No response stored
			logger:log(normal, "~s: Received a retransmission of request (~s ~s) but I "
				   "have no response to resend!", [LogTag, Method, sipurl:print(URI)])
		end,
		State;
	    true ->
		logger:log(normal, "~s: Server transaction: Received request is not particulary alike "
			   "the first one (~p ~s ~s /= ~p ~s ~s). Dropping it on the floor.",
			   [LogTag, CSeqNum, Method, sipurl:print(URI),
			    OrigCSeqNum, OrigMethod, sipurl:print(OrigURI)]),
		State
	end,
    check_quit({noreply, NewState, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_cast({{create_response, Status, Reason,
%%                         ExtraHeaders, RBody}, State)
%%           Status       = integer(), SIP status code
%%           Reason       = string(), SIP reason phrase
%%           ExtraHeaders = list() of {Key, ValueList} typles
%%           RBody        = binary(), body to use in response
%% Descrip.: The TU (transaction user) instructs us to create a
%%           response to our request (and send it).
%% Returns : {noreply, NewState, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_cast({create_response, Status, Reason, ExtraHeaders, RBody}, State)
  when is_integer(Status), is_list(Reason), is_list(ExtraHeaders), is_binary(RBody) ->
    Response = make_response(Status, Reason, RBody, ExtraHeaders, [], State),
    {ok, NewState1} = do_response(created, Response, State),
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_cast({forwardresponse, Response}, State)
%%           Response = response record()
%% Descrip.: The TU (transaction user) instructs us to proxy a
%%           response we have received from some other transaction as
%%           a response to this transaction.
%% Returns : {noreply, NewState, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_cast({forwardresponse, Response}, State) when is_record(Response, response) ->
    {ok, NewState1} = do_response(forwarded, Response, State),
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_cast({expired}, State)
%% Descrip.: The transaction layer tells us that we are overdue and
%%           should exit now.
%% Returns : {stop, Reason, State}
%%           Reason = string()
%%--------------------------------------------------------------------
handle_cast({expired}, State) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Received signal that I am expired, exiting.", [LogTag]),
    check_quit({stop, "Server transaction expired", State});

%%--------------------------------------------------------------------
%% Function: handle_cast({cancelled}, State)
%% Descrip.: The transaction layer tells us that it has received a
%%           CANCEL matching our request. If our report_to is set, we
%%           inform the TU and let it do whatever it wants before
%%           instructing us to send a 487 response. If report_to is
%%           NOT set, we answer 487 right here and remember the fact
%%           that we have been cancelled so that we can inform the TU
%%           when it tries to adopt us.
%% Returns : {noreply, NewState, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_cast({cancelled}, State) ->
    LogTag = State#state.logtag,
    SipState = State#state.sipstate,
    Reply =
	case lists:member(SipState, [trying, proceeding]) of
	    true ->
		ReportTo = State#state.report_to,
		case util:safe_is_process_alive(ReportTo) of
		    {true, ReportTo} ->
			logger:log(debug, "~s: Server transaction cancelled, telling parent ~p",
				   [LogTag, ReportTo]),
			%% We don't generate the 487 Request Cancelled here, parent does other
			%% stuff first and then tell us to send that response.
			%% XXX set up some kind of timer so that we can terminate eventually
			%% even if the TU doesn't get back to us with a final response?
			ReportTo ! {servertransaction_cancelled, self()},
			{noreply, State#state{cancelled=true}, ?TIMEOUT};
		    {false, _} ->
			%% Noone has adopted this server transaction yet (called set_report_to) -
			%% cancel the request and store that we have been cancelled so that we
			%% can notify whoever tries to adopt us later on.
			logger:log(debug, "~s: Server transaction orphaned ('~p' not alive) - can't inform "
				   "parent that I have been cancelled. Answering '487 Request Cancelled'.",
				   [LogTag, ReportTo]),
			Response = make_response(487, "Request Cancelled", <<>>, [], [], State),
			NewState1 = State#state{cancelled=true},
			{ok, NewState} = do_response(created, Response, NewState1),
			{noreply, NewState, ?TIMEOUT}
		end;
	    false ->
		logger:log(debug, "~s: Server transaction cancelled when in state ~p - ignoring.",
			   [LogTag, SipState]),
		{noreply, State, ?TIMEOUT}
	end,
    check_quit(Reply);

%%--------------------------------------------------------------------
%% Function: handle_cast({quit}, State)
%% Descrip.: Someone tells us it is time to quit now.
%% Returns : {stop, normal, State}
%%--------------------------------------------------------------------
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
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%% Descrip.: Timeout received. Check if we have report_to set, and if
%%           so check if that process is still alive. Try to find a
%%           reason to terminate (better than having processes linger
%%           forever).
%% Returns : {noreply, State, ?TIMEOUT} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%           Reason = string()
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    RStr = case State#state.response of
	       Response when is_record(Response, response) ->
		   {Status, Reason} = {Response#response.status, Response#response.reason},
		   io_lib:format("last sent response was '~p ~s'", [Status, Reason]);
	       undefined ->
		   "no responses sent"
	   end,
    logger:log(error, "~s: Stateful server transaction (~s ~s) still alive after 5 minutes! SIP-state is ~p, ~s. "
	       "Answering '500 Server Internal Error'.",
	       [LogTag, Method, sipurl:print(URI), State#state.sipstate, RStr]),
    logger:log(debug, "~s: Internal state dump :~n~p", [LogTag, State]),
    transactionlayer:debug_show_transactions(),

    logger:log(debug, "~s: Sending exit signal to parent (~p) and report_to (~p)",
	       [LogTag, State#state.parent, State#state.report_to]),
    %% Send exit signals to our parent and report_to pids to try and avoid having stale
    %% processes hanging around until the whole system is restarted the next time (might take years!)
    %% It isn't enough to just terminate this server transaction process when we finish after
    %% the 500 error is delivered. If we terminate normally (which we would), then processes linked
    %% to us wouldn't get killed. Use catch to not fall on our face if one of them is dead already.
    Parent = State#state.parent,
    (catch exit(Parent, servertransaction_timed_out)),
    ReportTo = State#state.report_to,
    case ReportTo of
	Parent -> ok;
	_ when is_pid(ReportTo) ->
	    (catch exit(ReportTo, servertransaction_timed_out))
    end,

    %% Generate a 500 Server Internal Error. Maybe we should just exit() since the other end
    %% is unlikely to remember this transaction state by now.
    SendResponse = make_response(500, "Server Internal Error", <<>>, [], [], State),
    {ok, NewState} = do_response(created, SendResponse, State),

    check_quit({noreply, NewState, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_info({siptimer, TRef, TDesc}, State)
%%           TRef  = term(), siptimer reference
%%           TDesc = string(), desciption of timer event
%% Descrip.: One of our siptimers has fired. Find it in our list and
%%           invoke process_timer/2.
%% Returns : {noreply, NewState, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_info({siptimer, TRef, TDesc}, State) ->
    LogTag = State#state.logtag,
    NewState =
	case siptimer:get_timer(TRef, State#state.timerlist) of
	    none ->
		logger:log(error, "~s: Unknown timer (~p:~p) fired! Ignoring. LIST ~n~p",
			   [LogTag, TRef, TDesc, State#state.timerlist]),
		State;
	    Timer ->
		case process_timer(Timer, State) of
		    NewState1 when is_record(NewState1, state) ->
			NewState1;
		    Unknown ->
			%% Don't apply the 'let it crash' philosophy inside servertransaction
			%% until we have some other process that will take care of the sending
			%% of an '500 Server Internal Error' for us.
			logger:log(error, "~s: Server transaction: process_timer() returned unknown result :~n~p",
				   [LogTag, Unknown]),
			State
		end
	end,
    check_quit({noreply, NewState, ?TIMEOUT});

%%--------------------------------------------------------------------
%% Function: handle_info({'EXIT', Parent, Reason}, State)
%%           Parent = pid()
%%           Reason = term()
%% Descrip.: Handle trapped EXIT signal from Parent. If we haven't
%%           sent a final response yet, we will generate a
%%           '500 Server Internal Error'.
%% Returns : {noreply, NewState, ?TIMEOUT}
%%--------------------------------------------------------------------

%%
%% normal exit, SipState = completed | confirmed | terminated
%%
handle_info({'EXIT', Pid, normal}, #state{parent=Parent, sipstate=SipState}=State)
  when Pid == Parent, SipState == completed; SipState == confirmed; SipState == terminated ->
    %% Parent exits normally when we are done, just ignore signal.
    check_quit({noreply, State, ?TIMEOUT});

%%
%% non-normal exit, SipState = completed | confirmed | terminated
%%
handle_info({'EXIT', Pid, Reason}, #state{parent=Parent, sipstate=SipState}=State)
  when Pid == Parent, SipState == completed; SipState == confirmed; SipState == terminated ->
    %% Parent exits abnormally when we are done, just log the event.
    logger:log(error, "~s: Parent ~p exited ABNORMALLY, my SIP-state is ~p so I'll ignore it.",
	       [State#state.logtag, Parent, SipState]),
    logger:log(debug, "~s: Parent ~p exit reason : ~p", [State#state.logtag, Parent, Reason]),
    check_quit({noreply, State, ?TIMEOUT});

%%
%% normal or non-normal exit, SipState = something-other-than-finished, generate 500 response
%%
handle_info({'EXIT', Pid, Reason}, #state{parent=Parent}=State) when Pid == Parent ->
    LogTag = State#state.logtag,
    SipState = State#state.sipstate,
    logger:log(error, "~s: Caught exit signal from parent ~p when in SIP-state ~p, "
	       "generating '500 Server Internal Error'", [LogTag, Parent, SipState]),
    logger:log(debug, "~s: Parent ~p exit reason : ~p", [LogTag, Parent, Reason]),
    Response = make_response(500, "Server Internal Error", <<>>, [], [], State),
    {ok, NewState} = do_response(created, Response, State),
    check_quit({noreply, NewState, ?TIMEOUT});

handle_info(Info, State) ->
    logger:log(error, "Server transaction: Received unknown gen_server info :~n~p",
	       [Info]),
    check_quit({noreply, State, ?TIMEOUT}).

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
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
	    logger:log(debug, "~s: Server transaction orphaned ('~p' not alive) - can't inform parent "
		       "that I am terminating now", [LogTag, R])
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

check_quit(Res, From) ->
    case Res of
	{reply, _Reply, State, _Timeout} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	{stop, _Reason, State} when is_record(State, state) ->
	    check_quit2(Res, From, State);
	{noreply, State, _Timeout} when is_record(State, state) ->
	    check_quit2(Res, From, State)
    end.

check_quit2(Res, From, #state{sipstate=terminated}=State) ->
    %% State#state.terminated is terminated, do some logging and (perhaps) any requested replying,
    %% then terminate.
    RStr = case State#state.response of
	       Re when is_record(Re, response) ->
		   io_lib:format("last sent response was '~p ~s'", [Re#response.status, Re#response.reason]);
	       undefined ->
		   "no responses sent"
	   end,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Server transaction (~s ~s) terminating in state '~p', ~s",
	       [LogTag, Method, sipurl:print(URI), State#state.sipstate, RStr]),
    %% If there was a reply to be sent, we must send that before changing Res into
    %% a stop. Only include clauses for variants we actually use.
    NewReply = case Res of
		   {reply, Reply, _, _} ->
		       send_reply(Reply, From, State),
		       {stop, normal, State};
		   {stop, _Reason, _State} ->
		       Res;
		   _ ->
		       %% {noreply, ...} or something
		       {stop, normal, State}
	       end,
    NewReply;
check_quit2(Res, _From, State) when is_record(State, state) ->
    %% State#state.sipstate was NOT 'terminated'
    Res.

send_reply(none, Reply, State) when is_record(State, state) ->
    logger:log(error, "~s: Can't send gen_server reply ~p to 'none'", [State#state.logtag, Reply]),
    error;
send_reply(To, Reply, State) when is_record(State, state)   ->
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
%% Function: process_timer2({resendresponse}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: We should resend a response, and arrange for it to be
%%           resent once again (unless it gets ACKed).
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({resendresponse}, Timer, State) when is_record(State, state) ->
    [Timeout] = siptimer:extract([timeout], Timer),
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    case State#state.response of
	Response when is_record(Response, response) ->
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
		    logger:log(debug, "~s: Ignoring signal to resend response '~p ~s' (response to ~s ~s) since "
			       "we are in state '~p'", [LogTag, Status, Reason, Method, sipurl:print(URI), State]),
		    State
	    end;
	_ ->
	    logger:log(error, "~s: Resend after ~p (response to ~s ~s, state '~p'): no response sent!",
		       [LogTag, Timeout, Method, sipurl:print(URI), State]),
	    State
    end;

%%--------------------------------------------------------------------
%% Function: process_timer2({resendresponse_timeout}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: We have been sending and sending our response. Enough is
%%           enough, terminate transaction.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({resendresponse_timeout}, Timer, State) when is_record(State, state) ->
    [Timeout] = siptimer:extract([timeout], Timer),
    LogTag = State#state.logtag,
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    Response = State#state.response,
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(normal, "~s: Sending of '~p ~s' (response to ~s ~s) timed out after ~s seconds, terminating "
	       "transaction.", [LogTag, Status, Reason, Method, sipurl:print(URI), siptimer:timeout2str(Timeout)]),
    %% XXX report to TU?
    enter_sip_state(terminated, State);

%%--------------------------------------------------------------------
%% Function: process_timer2({terminate_transaction}, Timer, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: Signal that it is time to terminate this transaction.
%% Returns : NewState = state record()
%%--------------------------------------------------------------------
process_timer2({terminate_transaction}, _Timer, State) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Received timer signal to terminate transaction", [LogTag]),
    enter_sip_state(terminated, State);

process_timer2(Signal, Timer, State) when is_record(State, state) ->
    [TRef] = siptimer:extract([ref], Timer),
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received unknown signal from timer ~p: ~p", [LogTag, TRef, Signal]),
    State.

%%--------------------------------------------------------------------
%% Function: do_response(Created, Response, State)
%%           Timer = siptimer record()
%%           State = state record()
%% Descrip.: We have a response to send. It is either created by our
%%           TU (Transaction User (Yxa application)) or it is a
%%           response we have received and should proxy. Regardless of
%%           which, we need to run it through our state machine to
%%           see if we should change state. Do that, and pass this
%%           response to send_response/3.
%% Returns : {ok, NewState}
%%           NewState = state record()
%%--------------------------------------------------------------------
do_response(Created, Response, State) when is_record(State, state), is_record(Response, response),
					   Created == created; Created == forwarded ->
    LogTag = State#state.logtag,
    Request = State#state.request,
    {ResponseToMethod, ResponseToURI} = {Request#request.method, Request#request.uri},
    OldSipState = State#state.sipstate,
    {Status, Reason} = {Response#response.status, Response#response.reason},
    NewState =
	case catch send_response_statemachine(ResponseToMethod, Status, OldSipState) of
	    {ignore, _SendReliably, NewSipState} ->
		enter_sip_state(NewSipState, State);
	    {send, SendReliably, NewSipState} ->
		LogLevel = if
			       Status >= 200 -> normal;
			       true -> debug
			   end,
		What = case Created of
			   created -> "Responding";
			   forwarded -> "Forwarding response"
		       end,
		logger:log(LogLevel, "~s: ~s '~p ~s'",
			   [LogTag, What, Status, Reason]),
		store_transaction_result(NewSipState, Request, Status, Reason, LogTag),
		{ok, NewState2} = send_response(Response, SendReliably, State),
		%% Make event out of the fact that we are sending a response
		ToTagUsed = sipheader:get_tag(keylist:fetch('to', Response#response.header)),
		L = [{request_method, ResponseToMethod}, {request_uri, sipurl:print(ResponseToURI)},
		     {to_tag, ToTagUsed}],
		event_final_response(State#state.branch, Created, Status, Reason, L),
		enter_sip_state(NewSipState, NewState2);
	    E ->
		logger:log(error, "~s: State machine does not allow us to send '~p ~s' in response to '~s ~s' "
			   "when my SIP-state is '~p'", [LogTag, Status, Reason, ResponseToMethod,
							 sipurl:print(ResponseToURI), OldSipState]),
		logger:log(debug, "~s: State machine returned :~n~p", [LogTag, E]),
		State
	end,
    {ok, NewState}.

%% generate a uas_result event with information about the original request
%% and the response we are now sending. part of do_response().
event_final_response(Created, Branch, Status, Reason, L) when Status >= 200 ->
    event_handler:uas_result(Branch, Created, Status, Reason, L),
    ok;
event_final_response(_Created, _Method, _Reason, _Branch, _L) ->
    %% Don't make events out of every non-final response we send
    ok.

%%--------------------------------------------------------------------
%% Function: store_transaction_result(SipState, Request, Status,
%%                                    Reason)
%% Descrip.: If SipState == completed, store this response with the
%%           transaction layer. This information is only used for
%%           debugging/informational purposes.
%% Returns : ok | error
%%--------------------------------------------------------------------
store_transaction_result(completed, Request, Status, Reason, LogTag) ->
    RStr = lists:flatten(lists:concat([Status, " ", Reason])),
    case transactionlayer:set_result(Request, RStr) of
	ok -> ok;
	E ->
	    logger:log(error, "~s: Failed storing my result (~s) with the transaction layer : ~p",
		       [LogTag, RStr, E]),
	    error
    end;
store_transaction_result(_SipState, _Request, _Status, _Reason, _LogTag) ->
    ok.

%%--------------------------------------------------------------------
%% Function: send_response_statemachine(Method, Status, State)
%%           Method = string(), SIP request method
%%           Status = integer(), SIP status code (received response)
%%           State  = atom(), trying | proceeding | completed |
%%                            terminated
%% Descrip.: State machine to decide what to do with a response we are
%%           going to send. Might choose not to send the response, and
%%           tells whether or not we should set up retransmission
%%           timers. Also says what we should set our SIP state to.
%% Returns : {Action, Reliably, State}
%%           Action   = atom(), ignore | tell_parent
%%           Reliably = atom(), true | false (send response reliably
%%                                            or not)
%%           State    = trying | proceeding | completed | terminated
%%--------------------------------------------------------------------
send_response_statemachine(Method, Status, trying) when Status == 100 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'trying' - " ++
	       "doing so (unreliably) and entering state 'proceeding'", [Status, Method]),
    {send, false, proceeding};

send_response_statemachine(Method, Status, State) when Status == 100 ->
    logger:log(debug, "UAS decision: Requested to send 100 Trying to ~s when in state '~p' - " ++
	       "ignoring", [Method, State]),
    {ignore, false, State};

send_response_statemachine(Method, Status, trying) when Status >= 100, Status =< 199 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'trying' - " ++
	       "doing so (unreliably) and entering state 'proceeding'", [Status, Method]),
    {send, false, proceeding};

send_response_statemachine(Method, Status, proceeding) when Status >= 100, Status =< 199 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'proceeding' - " ++
	       "doing so (unreliably)", [Status, Method]),
    {send, false, proceeding};


send_response_statemachine("INVITE", Status, proceeding) when Status >= 200, Status =< 299 ->
    logger:log(debug, "UAS decision: Requested to send 2xx response ~p to INVITE when in state 'proceeding' - " ++
	       "doing so (unreliably) and entering state 'terminated'", [Status]),
    {send, false, terminated};


send_response_statemachine("INVITE", Status, proceeding) when Status >= 300, Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 3/4/5/6xx response ~p to INVITE when in " ++
	       "state 'proceeding' - doing so (reliably) and entering state 'completed'", [Status]),
    {send, true, completed};

send_response_statemachine(Method, Status, trying) when Status >= 200, Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2/3/4/5/6xx response ~p to ~s when in " ++
	       "state 'trying' - doing so (unreliably) and entering state 'completed'", [Status, Method]),
    {send, false, completed};

send_response_statemachine(Method, Status, proceeding) when Status >= 200, Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2/3/4/5/6xx response ~p to ~s when in " ++
	       "state 'proceeding' - doing so (unreliably) and entering state 'completed'", [Status, Method]),
    {send, false, completed};

send_response_statemachine(Method, Status, completed) when Status >= 101, Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send 2/3/4/5/6xx response ~p to ~s when already in " ++
	       "state 'completed' - ignoring", [Status, Method]),
    {ignore, false, completed};

send_response_statemachine(Method, Status, terminated) when Status >= 100, Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send response ~p to ~s when in state 'terminated' - " ++
	       "ignoring", [Status, Method]),
    {ignore, false, terminated}.


%%--------------------------------------------------------------------
%% Function: send_response(Response, SendReliably, State)
%% Descrip.: This server transaction should answer something. If it is
%%           a final, but non-2xx, response to INVITE we also must
%%           store the To-tag we used with the transaction layer so
%%           that it can correctly match the ACK to this server
%%           transaction.
%% Returns : {ok, NewState}
%%           NewState = state record()
%%--------------------------------------------------------------------
send_response(Response, SendReliably, State) when is_record(Response, response), is_record(State, state) ->
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
	    case sipheader:get_tag(keylist:fetch('to', RHeader)) of
		none ->
		    logger:log(debug, "~s: Warning: No To-tag in response received (~p ~s) - transaction"
			       "layer might not be able to route the ACK to us!", [LogTag, Status, Reason]),
		    true;
		ToTag when is_list(ToTag) ->
		    case transactionlayer:store_to_tag(Request, ToTag) of
			ok -> true;
			R ->
			    %% XXX abort sending?
			    logger:log(error, "~s: Failed storing To-tag with transactionlayer : ~p", [LogTag, R])
		    end
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
				GDesc = "resendresponse " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++
				    sipurl:print(URI) ++ " (Timer G)",
				HDesc = "stopresend of " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++
				    sipurl:print(URI) ++ " (Timer H)",
				NewState2 = add_timer(TimerG, GDesc, {resendresponse}, State),
				NewState3 = add_timer(TimerH, HDesc, {resendresponse_timeout}, NewState2),
				NewState3;
			    _ ->
				TimerJ = 64 * T1,
				JDesc = "terminate server transaction after response " ++ integer_to_list(Status) ++
				    " " ++ Reason ++ " has been sent to " ++ sipurl:print(URI) ++ " (Timer J)",
				NewState2 = add_timer(TimerJ, JDesc, {terminate_transaction}, State),
				NewState2
			end;
		    false ->
			State
		end,
    NewState = NewState1#state{response=Response},
    {ok, NewState}.

add_timer(Timeout, Description, AppSignal, State) when is_record(State, state) ->
    NewTimerList = siptimer:add_timer(Timeout, Description, AppSignal, State#state.timerlist),
    State#state{timerlist=NewTimerList}.

%%--------------------------------------------------------------------
%% Function: enter_sip_state(SipState, State)
%%           SipState = atom(), trying | proceeding | ...
%%           State    = state record()
%% Descrip.: We are already in SIP state SipState, just return.
%% Returns : State = state record()
%%--------------------------------------------------------------------
enter_sip_state(SipState, #state{sipstate=SipState}=State) ->
    State;

%%--------------------------------------------------------------------
%% Function: enter_sip_state(completed, State)
%%           State    = state record()
%% Descrip.: Check if we are a non-INVITE transaction. If so, set up
%%           Timer J to terminate this transaction in 64 * T1 ms.
%% Returns : State = state record()
%%--------------------------------------------------------------------
enter_sip_state(completed, State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    NewState1 = State#state{sipstate=completed},
    Request = State#state.request,
    {ResponseToMethod, ResponseToURI} = {Request#request.method, Request#request.uri},
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
	    JDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++
		" (Timer J)",
	    add_timer(TimerJ, JDesc, {terminate_transaction}, NewState1)
    end;

%%--------------------------------------------------------------------
%% Function: enter_sip_state(confirmed, State)
%%           State    = state record()
%% Descrip.: We are an INVITE transaction that has sent a reply which
%%           has been ACK:ed. Stay alive for a few seconds (default 5)
%%           to absorb any additional ACK:s that might arrive if we've
%%           sent our response more than once.
%% Returns : State = state record()
%%--------------------------------------------------------------------
enter_sip_state(confirmed, State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    NewState1 = State#state{sipstate=confirmed},
    Request = State#state.request,
    {ResponseToMethod, ResponseToURI} = {Request#request.method, Request#request.uri},
    case ResponseToMethod of
	"INVITE" ->
	    TimerI = sipserver:get_env(timerT4, 5000),
	    logger:log(debug, "~s: Entered state 'confirmed'. Original request was an INVITE, starting " ++
		       "Timer I with a timeout of ~s seconds.",  [LogTag, siptimer:timeout2str(TimerI)]),
	    %% Install TimerI (T4, default 5 seconds) RFC 3261 17.2.1. Until TimerI fires we
	    %% absorb any additional ACK requests that might arrive.
	    IDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++ 
		" (Timer I)",
	    add_timer(TimerI, IDesc, {terminate_transaction}, NewState1);
	_ ->
	    logger:log(error, "~s: Entered state 'confirmed'. Original request was NOT an INVITE (it was ~s ~s). " ++
		       "How could this be?", [LogTag, ResponseToMethod, sipurl:print(ResponseToURI)]),
	    NewState1
    end;
enter_sip_state(NewSipState, State) when is_record(State, state), NewSipState == trying;
					 NewSipState == proceeding; NewSipState == terminated ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Entering state '~p'", [LogTag, NewSipState]),
    State#state{sipstate=NewSipState}.

%%--------------------------------------------------------------------
%% Function: process_received_ack(State)
%%           State    = state record()
%% Descrip.: We are an INVITE transaction that has just received an
%%           ACK. Check that we have actually sent a final response,
%%           and then cancel any response resend siptimers and go into
%%           SIP state 'confirmed'.
%% Returns : State = state record()
%%--------------------------------------------------------------------
process_received_ack(State) when is_record(State, state), is_record(State#state.response, response) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    Response = State#state.response,
    {Status, Reason} = {Response#response.status, Response#response.reason},
    LogTag = State#state.logtag,
    logger:log(normal, "~s: Response '~p ~s' to request ~s ~s ACK-ed",
	       [LogTag, Status, Reason, Method, sipurl:print(URI)]),
    case Method of
	"INVITE" ->
	    SipState = State#state.sipstate,
	    case lists:member(SipState, [trying, proceeding]) of
		true ->
		    logger:log(error, "~s: Received ACK when in state '~p' - ignoring", [LogTag, SipState]),
		    State;
		false ->
		    logger:log(debug, "~s: Received ACK, cancelling resend timers for response '~p ~s' "
			       "(to request ~s ~s) and entering state 'confirmed'",
			       [LogTag, Status, Reason, Method, sipurl:print(URI)]),
		    TimerList = State#state.timerlist,
		    NewTimerList = siptimer:cancel_timers_with_appsignal({resendresponse}, TimerList),
		    NewState1 = State#state{timerlist=NewTimerList},
		    NewState = enter_sip_state(confirmed, NewState1),
		    NewState
	    end;
	_ ->
	    logger:log(debug, "~s: Received ACK to non-INVITE request ~s ~s (response being ACKed is '~p ~s') "
		       "- ignoring", [LogTag, Method, sipurl:print(URI), Status, Reason]),
	    State
    end;
process_received_ack(State) when is_record(State, state) ->
    %% State#state.response is not a response record()
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received an ACK before I sent any responses, ignoring", [LogTag]),
    State.

%%--------------------------------------------------------------------
%% Function: make_response(Status, Reason, RBody, ExtraHeaders,
%%                         ViaParameters, State)
%%           Status        = integer(), SIP status code
%%           Reason        = string(), SIP reason phrase
%%           RBody         = integer(), body of response
%%           ExtraHeaders  = list() of {Key, ValueList} typles
%%           ViaParameters = term()
%%           State         = state record()
%% Descrip.: If there is no To-tag in our requests header, put ours
%%           there. Then use siprequest:make_response/7 to create a
%%           response from our requests headers.
%% Returns : Response = response record()
%%--------------------------------------------------------------------
make_response(100, Reason, RBody, ExtraHeaders, ViaParameters, State)
  when is_record(State, state), is_list(Reason), is_binary(RBody), is_list(ExtraHeaders) ->
    %% We don't need to set To-tag for 100 Trying. 100 Trying are not supposed
    %% to have a To-Tag.
    siprequest:make_response(100, Reason, RBody, ExtraHeaders, ViaParameters,
			     State#state.socket, State#state.request);

make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State)
  when is_record(State, state), is_integer(Status), is_list(Reason), is_binary(RBody), is_list(ExtraHeaders) ->
    Request = State#state.request,
    Header = Request#request.header,
    To = keylist:fetch('to', Header),
    Req = case sipheader:get_tag(To) of
	      none ->
		  {DisplayName, ToURI} = sipheader:to(To),
		  [NewTo] = sipheader:contact_print(
			      [ contact:new(DisplayName, ToURI, [{"tag", State#state.my_to_tag}]) ]),
		  NewHeader = keylist:set("To", [NewTo], Header),
		  Request#request{header=NewHeader};
	      _ ->
		  Request
	  end,
    siprequest:make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State#state.socket, Req).

%%--------------------------------------------------------------------
%% Function: generate_tag()
%% Descrip.: Generate a string that might be used as To: tag in
%%           responses we create. This means it includes at least 32
%%           bits of randomness (specified by RFC3261 #19.3).
%%           there. Then use siprequest:make_response/7 to create a
%%           response from our requests headers.
%% Returns : Tag = string()
%%--------------------------------------------------------------------
generate_tag() ->
    %% Erlang guarantees that subsequent calls to now() generate increasing values (on the same node).
    {Megasec, Sec, Microsec} = now(),
    In = lists:concat([node(), Megasec * 1000000 + Sec, 8, $., Microsec]),
    Out = siprequest:make_base64_md5_token(In),
    %% don't make the tag longer than it has to be.
    "yxa-" ++ string:substr(Out, 1, 9).


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
    %% test send_response_statemachine/3
    %%--------------------------------------------------------------------

    %% 17.2.1 INVITE Server Transaction

    io:format("test: send_response_statemachine/3 INVITE - 1~n"),
    %% When a server transaction is constructed for a request, it enters the
    %% "Proceeding" state.
    %% Send 100 Trying only in 'trying'. 'trying' for INVITE is not in the RFC3261
    %% spec, it is our internal way of saying "we haven't sent a 100 Trying yet".
    {send, false, proceeding} = send_response_statemachine("INVITE", 100, trying),

    io:format("test: send_response_statemachine/3 INVITE - 2~n"),
    %% The TU passes any number of provisional responses to the server
    %% transaction.  So long as the server transaction is in the
    %% "Proceeding" state, each of these MUST be passed to the transport
    %% layer for transmission.  They are not sent reliably by the
    %% transaction layer (they are not retransmitted by it) and do not cause
    %% a change in the state of the server transaction.
    {send, false, proceeding} = send_response_statemachine("INVITE", 101, proceeding),
    {send, false, proceeding} = send_response_statemachine("INVITE", 183, proceeding),
    {send, false, proceeding} = send_response_statemachine("INVITE", 199, proceeding),

    io:format("test: send_response_statemachine/3 INVITE - 3~n"),
    %% If, while in the "Proceeding" state, the TU passes a 2xx response to
    %% the server transaction, the server transaction MUST pass this
    %% response to the transport layer for transmission.  It is not
    %% retransmitted by the server transaction; retransmissions of 2xx
    %% responses are handled by the TU.  The server transaction MUST then
    %% transition to the "Terminated" state.
    {send, false, terminated} = send_response_statemachine("INVITE", 200, proceeding),

    io:format("test: send_response_statemachine/3 INVITE - 4~n"),
    %% While in the "Proceeding" state, if the TU passes a response with
    %% status code from 300 to 699 to the server transaction, the response
    %% MUST be passed to the transport layer for transmission, and the state
    %% machine MUST enter the "Completed" state.  For unreliable transports,
    %% timer G is set to fire in T1 seconds, and is not set to fire for
    %% reliable transports.
    {send, true, completed} = send_response_statemachine("INVITE", 300, proceeding),
    {send, true, completed} = send_response_statemachine("INVITE", 400, proceeding),
    {send, true, completed} = send_response_statemachine("INVITE", 500, proceeding),
    {send, true, completed} = send_response_statemachine("INVITE", 600, proceeding),
    {send, true, completed} = send_response_statemachine("INVITE", 699, proceeding),

    %% own conclusions

    io:format("test: send_response_statemachine/3 INVITE - 5~n"),
    %% Don't send 100 Trying in any other state than 'trying'. This means that the TU
    %% will be ignored if it asks us to send out a 100 Trying, since we did that ourselves
    %% when in our internal mode (for INVITE) 'trying'.
    {ignore, false, proceeding} = send_response_statemachine("INVITE", 100, proceeding),
    {ignore, false, completed} = send_response_statemachine("INVITE", 100, completed),
    {ignore, false, terminated} = send_response_statemachine("INVITE", 100, terminated),

    io:format("test: send_response_statemachine/3 INVITE - 6~n"),
    %% don't send any more responses once we have sent a final response
    {ignore, false, completed} = send_response_statemachine("INVITE", 100, completed),
    {ignore, false, completed} = send_response_statemachine("INVITE", 200, completed),
    {ignore, false, completed} = send_response_statemachine("INVITE", 300, completed),
    {ignore, false, completed} = send_response_statemachine("INVITE", 400, completed),
    {ignore, false, completed} = send_response_statemachine("INVITE", 500, completed),
    {ignore, false, completed} = send_response_statemachine("INVITE", 600, completed),

    io:format("test: send_response_statemachine/3 INVITE - 7~n"),
    %% don't send any more responses if we should happen to end up here even if we are
    %% really supposed to have terminated (like if we are emptying our mailbox before
    %% _really_ terminating)
    {ignore, false, terminated} = send_response_statemachine("INVITE", 100, terminated),
    {ignore, false, terminated} = send_response_statemachine("INVITE", 200, terminated),
    {ignore, false, terminated} = send_response_statemachine("INVITE", 300, terminated),
    {ignore, false, terminated} = send_response_statemachine("INVITE", 400, terminated),
    {ignore, false, terminated} = send_response_statemachine("INVITE", 500, terminated),
    {ignore, false, terminated} = send_response_statemachine("INVITE", 600, terminated),

    %% 17.2.2 Non-INVITE Server Transaction

    %% The state machine is initialized in the "Trying" state ...

    io:format("test: send_response_statemachine/3 non-INVITE - 1~n"),
    %% While in the "Trying" state, if the TU passes a provisional response
    %% to the server transaction, the server transaction MUST enter the
    %% "Proceeding" state.  The response MUST be passed to the transport
    %% layer for transmission.
    {send, false, proceeding} = send_response_statemachine("OPTIONS", 100, trying),
    {send, false, proceeding} = send_response_statemachine("OPTIONS", 199, trying),

    io:format("test: send_response_statemachine/3 non-INVITE - 2~n"),
    %% Any further provisional responses that are received from the TU while in the
    %% "Proceeding" state MUST be passed to the transport layer for transmission.
    %% Yxa note: we filter out 100 Trying
    {ignore, false, proceeding} = send_response_statemachine("OPTIONS", 100, proceeding),
    {send, false, proceeding} = send_response_statemachine("OPTIONS", 199, proceeding),

    io:format("test: send_response_statemachine/3 non-INVITE - 3~n"),
    %% If the TU passes a final response (status codes 200-699) to the server while
    %% in the "Proceeding" state, the transaction MUST enter the "Completed" state,
    %% and the response MUST be passed to the transport layer for transmission.
    {send, false, completed} = send_response_statemachine("OPTIONS", 200, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 300, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 400, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 500, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 600, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 699, proceeding),

    io:format("test: send_response_statemachine/3 non-INVITE - 4~n"),
    %% Any other final responses passed by the TU to the server transaction MUST
    %% be discarded while in the "Completed" state.
    {ignore, false, completed} = send_response_statemachine("OPTIONS", 200, completed),
    {ignore, false, completed} = send_response_statemachine("OPTIONS", 300, completed),
    {ignore, false, completed} = send_response_statemachine("OPTIONS", 400, completed),
    {ignore, false, completed} = send_response_statemachine("OPTIONS", 500, completed),
    {ignore, false, completed} = send_response_statemachine("OPTIONS", 600, completed),
    {ignore, false, completed} = send_response_statemachine("OPTIONS", 699, completed),

    %% own conclusions

    io:format("test: send_response_statemachine/3 non-INVITE - 5~n"),
    %% provisional responses received from the TU when already completed is ignored
    {ignore, false, completed} = send_response_statemachine("OPTIONS", 100, completed),
    {ignore, false, completed} = send_response_statemachine("OPTIONS", 199, completed),

    ok.
