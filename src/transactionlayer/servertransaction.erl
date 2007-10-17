%%%-------------------------------------------------------------------
%%% File    : servertransaction.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      A server transaction gen_server.
%%%
%%% @since    05 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%
%%% Note    : Perhaps we should generate a 500 if we don't get adopted
%%%           in a few seconds from when we start?
%%%
%%% Server transaction processes are spawned automatically by the
%%% transaction layer when new requests arrive.
%%%
%%% At the same time, the transaction layer also starts the request/3
%%% function of the YXA application running (for example
%%% incomingproxy:request/3). The application is what RFC3261 calls a
%%% 'Transaction User' or 'proxy core'.
%%%
%%% The application should 'adopt' the server transaction process, and
%%% then tell it what response(s) to send.
%%%
%%% After adopting a server transaction, the application can get the
%%% following signals from the server transaction :
%%%
%%%    {servertransaction_cancelled, Pid, ExtraHeaders} - the
%%%       transaction layer has received a CANCEL matching this server
%%%       transaction.
%%%         Pid          = pid() of server transaction
%%%         ExtraHeaders = list() of {Key, ValueList} tuples - headers
%%%                        from the CANCEL request that should be
%%%                        passed on to any downstream entitys
%%%
%%%    {servertransaction_terminating, Pid} - the server transaction
%%%       is terminating.
%%%         Pid = pid() of server transaction
%%%
%%%-------------------------------------------------------------------
-module(servertransaction).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/2,
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
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  branch,		%% string(), our branch identifier
	  logtag,		%% string(), prefix to use when logging
	  socket,		%% sipsocket record(), the socket the request was received on -
	  			%% RFC3261 requires us to send responses using the very same socket
	  is_rel_sock,		%% true | false, are the sockets transport reliable?
	  report_to,		%% undefined | pid(), to whom we should report if we are cancelled
	  parent,		%% pid(), our parent process - to handle trapped EXITs from it
	  request,		%% request record(), the request we are handling
	  response,		%% undefined | response record(), the last response we sent
	  sipstate,		%% atom(), trying|proceeding|completed|confirmed|terminated
	  cancelled = false,	%% bool(), have we been cancelled?
	  timerlist,		%% siptimerlist record(), our current set of timers
	  my_to_tag,		%% string(), To-tag to use if we generate a response (as opposed to
	  			%% if we forward a response)
	  nit_100 = false,	%% non-INVITE '100 Trying' time reached?
	  testing = false	%% true | false, are we just testing this modules functions?
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(TIMEOUT, 300 * 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) -> term()
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Request, YxaCtx) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    %% It is intentional to call gen_server:start(...) here even though
    %% this function is called start_link. That is because of a 'problem'
    %% with gen_servers in Erlang/OTP (at least R10B-2). If you use
    %% gen_server:start_link(...) to start your gen_server, you won't be
    %% able to trap 'EXIT' signals from the parent process, even if you
    %% set process_flag(trap_exit, true)! We set up a link to this process
    %% in the init/1 callback to achieve the same effect (although with
    %% a bitter taste).
    gen_server:start(?MODULE, {Request, YxaCtx, self()}, []).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({Request, YxaCtx, Parent}) ->
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%% @doc     Initiates the server transaction gen_server
%% @hidden
%% @end
%%--------------------------------------------------------------------

%%
%% ACK - no go
%%
init({#request{method="ACK"} = Request, _YxaCtx, _Parent}) ->
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
init({Request, YxaCtx, Parent}) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    Branch = siprequest:generate_branch() ++ "-UAS",
    Desc = lists:concat([Branch, ": ", Method, " ", sipurl:print(URI)]),
    %% Get ourselves into the transaction state list first of all
    case transactionstatelist:add_server_transaction(Request, self(), Desc) of
	ok ->
	    %% Link to transactionlayer immediately (so that it removes this
	    %% transaction from the transactionstatelist when we exit/crash).
	    TPid = erlang:whereis(transactionlayer),
	    true = link(TPid),
	    %% Link to parent from init/1 instead of using gen_server:start_link to
	    %% be able to trap EXIT signals from parent process. See comment in start_link
	    %% above for more details.
	    true = link(Parent),
	    process_flag(trap_exit, true),
	    case init2({Request, YxaCtx, Branch, Parent}) of
		{ok, State, Timeout} when is_record(State, state) ->
		    {ok, State, Timeout};
		Reply ->
		    Reply
	    end;
	{duplicate, TState} when is_record(TState, transactionstate) ->
	    %% We are the losing party of a race. Notify the winner that there was
	    %% a resend (us) and then exit. XXX implement the notifying, for now
	    %% just log.
	    logger:log(normal, "~s: Early resend, exiting.", [YxaCtx#yxa_ctx.logstr]),
	    {stop, normal}
    end.

init2({Request, YxaCtx, Branch, Parent}) when is_record(Request, request) ->
    {Method, URI} = {Request#request.method, Request#request.uri},

    %% LogTag is essentially Branch + Method, LogStr is a string that
    %% describes this request (METHOD URI [client=x, from=y, to=z])
    LogTag = Branch ++ " " ++ Method,

    MyToTag =
	case sipheader:get_tag(keylist:fetch('to', Request#request.header)) of
	    none ->
		siputil:generate_tag();
	    Tag when is_list(Tag) ->
		Tag
	end,

    Socket = (YxaCtx#yxa_ctx.origin)#siporigin.sipsocket,
    IsRel = sipsocket:is_reliable_transport(Socket),

    State = #state{branch	= Branch,
		   logtag	= LogTag,
		   socket	= Socket,
		   is_rel_sock	= IsRel,
		   request	= Request,
		   sipstate	= trying,
		   timerlist	= siptimer:empty(),
		   my_to_tag	= MyToTag,
		   parent	= Parent
		  },

    logger:log(debug, "~s: Started new server transaction for request ~s ~s.",
	       [LogTag, Method, sipurl:print(URI)]),
    logger:log(normal, "~s: ~s", [LogTag, YxaCtx#yxa_ctx.logstr]),

    %% Create event about new request received
    {CallId, FromTag, ToTag} = sipheader:dialogid(Request#request.header),
    DId = lists:concat(["c=", CallId, "; ft=", FromTag, "; tt=", ToTag]),
    [From] = keylist:fetch('from', Request#request.header),
    [To] = keylist:fetch('to', Request#request.header),
    event_handler:new_request(Method, URI, Branch, DId, From, To),

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
		%% set up a timer to send a 100 Trying for non-INVITE transactions
		%% after T2 seconds, RFC4320 #4.1 (Action 1)
		{ok, T2} = yxa_config:get_env(timerT2),
		Desc = "send non-INVITE 100",
		add_timer(T2, Desc, {send_nit_100}, State)
	end,

    {ok, NewState, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({get_branch}, From, State) ->
%%            {reply, Reply, State, Timeout::integer()} |
%%            {stop, Reason, Reply, State}
%%
%%            Reply  = {ok, Branch}
%%            Branch = string()
%%
%% @doc     This is a request to get our generated branch.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({get_branch}, From, State) ->
    check_quit({reply, {ok, State#state.branch}, State, ?TIMEOUT}, From);

%%--------------------------------------------------------------------
%% @spec    ({set_report_to, Pid}, From, State) ->
%%            {reply, Reply, State, Timeout::integer()} |
%%            {stop, Reason, Reply, State}    
%%
%%            Pid = pid()
%%
%%            Reply  = ok               |
%%                     {error, Error}   |
%%                     {ignore, Reason}
%%            Error  = string()
%%            Reason = cancelled | completed
%%
%% @doc     This is a request to set our report_to. This is typically
%%          done by a TU (Transaction User (YXA application)) that
%%          wants to receive notice if we terminate etc. We can only
%%          report to one process, so this function will fail if our
%%          report_to is already set. It will also fail if we have
%%          already been cancelled. Note : If we already have a
%%          report_to, we will reject the call even if report_to is
%%          the same pid as the call asks us to set it to. This might
%%          be relaxed later, but for now you should not try to adopt
%%          a server transcation twice.
%% @hidden
%% @end
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

%%--------------------------------------------------------------------
%% @spec    (change_parent, FromPid, ToPid}, From, State) ->
%%            {reply, Reply, State, Timeout::integer()} |
%%            {stop, Reason, Reply, State}    
%%
%%            FromPid = pid()
%%            ToPid   = pid()
%%
%%            Reply = {ok, ToTag}
%%            ToTag = string()
%%
%% @doc     Change our parent. Internal to the transaction layer -
%%          used when a dialog controller is handed the request
%%          instead of the YXA application's request/3 function being
%%          invoked.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({change_parent, FromPid, ToPid}, From, #state{parent = FromPid} = State) when is_pid(ToPid) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Changing parent from ~p to ~p", [LogTag, FromPid, ToPid]),
    true = link(ToPid),
    true = unlink(FromPid),
    Reply = {reply, ok, State#state{parent = ToPid}, ?TIMEOUT},
    check_quit(Reply, From);

%%--------------------------------------------------------------------
%% @spec    (get_my_to_tag, From, State) ->
%%            {reply, Reply, State, Timeout::integer()} |
%%            {stop, Reason, Reply, State}    
%%
%%            Reply = {ok, ToTag}
%%            ToTag = string()
%%
%% @doc     Return our To-tag. Functionality required for making
%%          UAC/UAS applications, which must be able to send other
%%          requests with the same to-tag (requests inside the same
%%          dialog).
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(get_my_to_tag, _From, State) ->
    Reply = {reply, {ok, State#state.my_to_tag}, State, ?TIMEOUT},
    check_quit(Reply);

handle_call(Request, From, State) ->
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received unknown gen_server call from ~p :~n~p",
	       [LogTag, From, Request]),
    Reply = {reply, error, State, ?TIMEOUT},
    check_quit(Reply, From).


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
%% @spec    ({siprequest, Request, Origin}, State) ->
%%            {noreply, NewState, Timeout::integer()}
%%
%%            Request = #request{}
%%            Origin  = #siporigin{}
%%
%% @doc     The transaction layer has received a request that matches
%%          this server transaction. It should be either a resend of
%%          the previous request, or an ACK that matches a non-2xx
%%          response to INVITE that we are sending reliably.
%% @hidden
%% @end
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
		logger:log(normal, "~s: Server transaction: Received request is not particularly alike "
			   "the first one (~p ~s ~s /= ~p ~s ~s). Dropping it on the floor.",
			   [LogTag, CSeqNum, Method, sipurl:print(URI),
			    OrigCSeqNum, OrigMethod, sipurl:print(OrigURI)]),
		State
	end,
    check_quit({noreply, NewState, ?TIMEOUT});

%%--------------------------------------------------------------------
%% @spec    ({{create_response, Status, Reason, ExtraHeaders, RBody},
%%          State) -> {noreply, NewState, Timeout::integer()}
%%
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = [{Key, ValueList} tuples]
%%            RBody        = binary() "body to use in response"
%%
%% @doc     The TU (transaction user) instructs us to create a
%%          response to our request (and send it).
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({create_response, Status, Reason, ExtraHeaders, RBody}, State)
  when is_integer(Status), is_list(Reason), is_list(ExtraHeaders), is_binary(RBody) ->
    Response = make_response(Status, Reason, RBody, ExtraHeaders, [], State),
    {ok, NewState1} = do_response(created, Response, State),
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% @spec    ({forwardresponse, Response}, State) ->
%%            {noreply, NewState, Timeout::integer()}
%%
%%            Response = #response{}
%%
%% @doc     The TU (transaction user) instructs us to proxy a response
%%          we have received from some other transaction as a
%%          response to this transaction.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({forwardresponse, Response}, State) when is_record(Response, response) ->
    {ok, NewState1} = do_response(forwarded, Response, State),
    check_quit({noreply, NewState1, ?TIMEOUT});

%%--------------------------------------------------------------------
%% @spec    ({expired}, State) ->
%%            {stop, Reason, State}
%%
%%            Reason = string()
%%
%% @doc     The transaction layer tells us that we are overdue and
%%          should exit now.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({expired}, State) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Received signal that I am expired, exiting.", [LogTag]),
    check_quit({stop, "Server transaction expired", State});

%%--------------------------------------------------------------------
%% @spec    ({cancelled, ExtraHeaders}, State) ->
%%            {noreply, NewState, Timeout::integer()}
%%
%%            ExtraHeaders = [{Key, ValueList} tuples]
%%
%% @doc     The transaction layer tells us that it has received a
%%          CANCEL matching our request. If our report_to is set, we
%%          inform the TU and let it do whatever it wants before
%%          instructing us to send a 487 response. If report_to is
%%          NOT set, we answer 487 right here and remember the fact
%%          that we have been cancelled so that we can inform the TU
%%          when it tries to adopt us.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({cancelled, ExtraHeaders}, State) ->
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
			ReportTo ! {servertransaction_cancelled, self(), ExtraHeaders},
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
%% @spec    ({quit}, State) -> {stop, normal, State}
%%
%% @doc     Someone tells us it is time to quit now.
%% @hidden
%% @end
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
%% @spec    (timeout, State) ->
%%            {noreply, State, Timeout::integer()} |
%%            {stop, Reason, State}
%%
%%            Reason = string()
%%
%% @doc     Timeout received. Check if we have report_to set, and if
%%          so check if that process is still alive. Try to find a
%%          reason to terminate (better than having processes linger
%%          forever).
%% @hidden
%% @end
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
    logger:log(debug, "~s: Server transaction (~s ~s) still alive after 5 minutes! SIP-state is ~p, ~s",
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
    case State#state.report_to of
	Parent -> ok;
	ReportTo when is_pid(ReportTo) ->
	    (catch exit(ReportTo, servertransaction_timed_out));
	undefined ->
	    ok
    end,

    case (Method == "INVITE") of
	true ->
	    %% Generate a '500 Server Internal Error' for INVITE transactions
	    logger:log(error, "~s: INVITE server transaction timed out (after ~ps of inactivity), "
		       "answering '500 Server Internal Error'", [LogTag, ?TIMEOUT div 1000]),
	    SendResponse = make_response(500, "Server Internal Error", <<>>, [], [], State),
	    {ok, NewState} = do_response(created, SendResponse, State),
	    check_quit({noreply, NewState, ?TIMEOUT});
	false ->
	    %% when we timeout, we shouldn't send any response at all to non-INVITE (RFC4320 #4.2)
	    logger:log(error, "~s: non-INVITE server transaction timed out (after ~ps of inactivity), "
		       "exiting withouth having sent a final response (RFC4320)", [LogTag, ?TIMEOUT div 1000]),
	    check_quit({stop, normal, State})
    end;


%%--------------------------------------------------------------------
%% @spec    ({siptimer, TRef, TDesc}, State) ->
%%            {noreply, NewState, Timeout::integer()}
%%
%%            TRef  = term() "siptimer reference"
%%            TDesc = string() "description of timer event"
%%
%% @doc     One of our siptimers has fired. Find it in our list and
%%          invoke process_timer/2.
%% @hidden
%% @end
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
%% @spec    ({'EXIT', Parent, Reason}, State) ->
%%            {noreply, NewState, Timeout::integer()}
%%
%%            Parent = pid()
%%            Reason = term()
%%
%% @doc     Handle trapped EXIT signal from Parent. If we haven't sent
%%          a final response yet, we will generate a '500 Server
%%          Internal Error'.
%% @hidden
%% @end
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
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
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
%%            Res = term() "gen_server:call/cast/info() return value"
%%
%% @equiv    check_quit(Res, none)
%% @end
%%--------------------------------------------------------------------
check_quit(Res) ->
    check_quit(Res, none).

%%--------------------------------------------------------------------
%% @spec    (Res, From) ->
%%            {noreply, State}          |
%%            {stop, Reason, State}
%%
%%            Res  = term() "gen_server:call/cast/info() return value"
%%            From = term() "gen_server from-value | none"
%%
%% @doc     Extract the state record() from Res, and check if it's
%%          sipstate is 'terminated'. If it is then turn Res into a
%%          stop signal, but if Res was {reply, ...} execute a
%%          gen_server:reply() first. Note : Not all variants of
%%          gen_server call/cast/info return values are covered in
%%          these functions - only the ones we actually use!
%% @end
%%--------------------------------------------------------------------
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

send_reply(Reply, none, State) when is_record(State, state) ->
    logger:log(error, "~s: Can't send gen_server reply ~p to 'none'", [State#state.logtag, Reply]),
    error;
send_reply(Reply, To, State) when is_record(State, state)   ->
    gen_server:reply(To, Reply).

%%--------------------------------------------------------------------
%% @spec    (Timer, State) ->
%%            NewState
%%
%%            Timer = #siptimer{}
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Process fired siptimer events.
%% @end
%%--------------------------------------------------------------------
process_timer(Timer, State) when is_record(State, state) ->
    [TRef, Signal, Description] = siptimer:extract([ref, appsignal, description], Timer),
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Timer ~p:~p fired", [LogTag, TRef, Description]),
    process_timer2(Signal, Timer, State).

%%--------------------------------------------------------------------
%% @spec    ({resendresponse}, Timer, State) ->
%%            NewState
%%
%%            Timer = #siptimer{}
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     We should resend a response, and arrange for it to be
%%          resent once again (unless it gets ACKed).
%% @end
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
		    logger:log(debug, "~s: Resend after ~s seconds (response to ~s ~s): ~p ~s",
			       [LogTag, siptimer:timeout2str(Timeout), Method, sipurl:print(URI), Status, Reason]),
		    transportlayer:send_proxy_response(State#state.socket, State#state.response),
		    NewTimeout = case Method of
				     "INVITE" ->
					 %% Use Timer Value T2 for INVITE responses
					 {ok, T2} = yxa_config:get_env(timerT2),
					 lists:min([Timeout * 2, T2]);
				     _ ->
					 Timeout * 2
				 end,
		    NewTimerList = siptimer:revive_timer(Timer, NewTimeout, State#state.timerlist),
		    State#state{timerlist=NewTimerList};
		SipState ->
		    logger:log(debug, "~s: Ignoring signal to resend response '~p ~s' (response to ~s ~s) since "
			       "we are in state '~p'", [LogTag, Status, Reason, Method, sipurl:print(URI), SipState]),
		    State
	    end;
	_ ->
	    logger:log(error, "~s: Resend after ~s seconds (response to ~s ~s): no response sent!",
		       [LogTag, siptimer:timeout2str(Timeout), Method, sipurl:print(URI)]),
	    logger:log(debug, "~s: Full internal state dump :~n~p", [LogTag, State]),
	    State
    end;

%%--------------------------------------------------------------------
%% @spec    ({resendresponse_timeout}, Timer, State) ->
%%            NewState
%%
%%            Timer = #siptimer{}
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     We have been sending and sending our response. Enough is
%%          enough, terminate transaction.
%% @end
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
    %% XXX report to TU in some special way? The TU will get a general
    %% 'servertransaction_terminated' from enter_sip_state/2.
    enter_sip_state(terminated, State);

%%--------------------------------------------------------------------
%% @spec    ({terminate_transaction}, Timer, State) ->
%%            NewState
%%
%%            Timer = #siptimer{}
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Signal that it is time to terminate this transaction.
%% @end
%%--------------------------------------------------------------------
process_timer2({terminate_transaction}, _Timer, State) ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Received timer signal to terminate transaction", [LogTag]),
    enter_sip_state(terminated, State);

process_timer2({send_nit_100}, _Timer, State) ->
    LogTag = State#state.logtag,
    case State#state.sipstate of
	trying ->
	    %% RFC4320 #4.1 (Action 1)
	    %% Without regard to transport, an SIP element MUST respond to a non-
	    %% INVITE request with a Status-Code of 100 if it has not otherwise
	    %% responded after the amount of time it takes a client transaction's
	    %% Timer E to be reset to T2.
	    logger:log(debug, "~s: Sending '100 Trying' in response to non-INVITE", [LogTag]),
	    Response = make_response(100, "Trying", <<>>, [], [], State),
	    {ok, NewState1} = do_response(created, Response, State#state{nit_100 = true}),
	    NewState1;
	SipState ->
	    logger:log(debug, "~s: Not sending '100 Trying' response to non-INVITE when in SIP state ~p",
		       [LogTag, SipState]),
	    State
    end;

process_timer2(Signal, Timer, State) when is_record(State, state) ->
    [TRef] = siptimer:extract([ref], Timer),
    LogTag = State#state.logtag,
    logger:log(error, "~s: Received unknown signal from timer ~p: ~p", [LogTag, TRef, Signal]),
    State.

%%--------------------------------------------------------------------
%% @spec    (Created, Response, State) ->
%%            {ok, NewState}
%%
%%            Created = created | forwarded
%%            Timer   = #siptimer{}
%%            State   = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     We have a response to send. It is either created by our TU
%%          (Transaction User (YXA application)) or it is a response
%%          we have received and should proxy. Regardless of which,
%%          we need to run it through our state machine to see if we
%%          should change state. Do that, and pass this response to
%%          send_response/3.
%% @end
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
	    ignore ->
		State;
	    {send, SendReliably, NewSipState} ->
		case catch send_response_non_invite_check(ResponseToMethod, Status, State) of
		    ignore ->
			enter_sip_state(NewSipState, State);
		    send ->
			LogLevel = if
				       Status >= 200 -> normal;
				       true -> debug
				   end,
			What = case Created of
				   created -> "Responding";
				   forwarded -> "Forwarding response"
			       end,
			logger:log(LogLevel, "~s: ~s '~p ~s'", [LogTag, What, Status, Reason]),
			case State#state.testing of
			    true -> ok;
			    false ->
				store_transaction_result(NewSipState, Request, Status, Reason, LogTag)
			end,
			{ok, NewState2} = send_response(Response, SendReliably, State),
			%% Make event out of the fact that we are sending a response
			event_final_response(Created, NewState2, Status),
			enter_sip_state(NewSipState, NewState2)
		end;
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
event_final_response(Created, State, Status) when Status >= 200 ->
    Branch = State#state.branch,
    Response = State#state.response,
    {Status, Reason} = {Response#response.status, Response#response.reason},
    ToTagUsed = sipheader:get_tag(keylist:fetch('to', Response#response.header)),
    L = [{to_tag, ToTagUsed}],
    case State#state.testing of
	true -> ok;
	false ->
	    event_handler:uas_result(Branch, Created, Status, Reason, L)
    end;
event_final_response(_Created, _State, _Status) ->
    %% Don't make events out of every non-final response we send
    ok.

%%--------------------------------------------------------------------
%% @spec    (SipState, Request, Status, Reason, LogTag) -> ok | error
%%
%%            SipState = completed | atom()
%%            Request  = #requets{}
%%            Status   = integer() "SIP status code"
%%            Reason   = string() "SIP reason phrase"
%%            LogTag   = string() "log prefix if we fail"
%%
%% @doc     If SipState == completed, store this response with the
%%          transaction layer. This information is only used for
%%          debugging/informational purposes.
%% @end
%%--------------------------------------------------------------------
store_transaction_result(completed, Request, Status, Reason, LogTag) ->
    RStr = lists:concat([Status, " ", Reason]),
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
%% @spec    (Method, Status, SipState) ->
%%            {send, Reliably, SipState} | ignore
%%
%%            Method   = string() "SIP request method"
%%            Status   = integer() "SIP status code (received response)"
%%            SipState = trying | proceeding | completed | terminated
%%
%%            Reliably = true | false "send response reliably or not"
%%            State    = trying | proceeding | completed | terminated
%%
%% @doc     State machine to decide what to do with a response we are
%%          going to send. Might choose not to send the response, and
%%          tells whether or not we should set up retransmission
%%          timers. Also says what we should set our SIP state to.
%% @end
%%--------------------------------------------------------------------
send_response_statemachine(Method, Status, trying) when Status == 100 ->
    logger:log(debug, "UAS decision: Requested to send 1xx response ~p to ~s when in state 'trying' - " ++
	       "doing so (unreliably) and entering state 'proceeding'", [Status, Method]),
    {send, false, proceeding};

send_response_statemachine(Method, Status, State) when Status == 100 ->
    logger:log(debug, "UAS decision: Requested to send 100 Trying to ~s when in state '~p' - " ++
	       "ignoring", [Method, State]),
    ignore;

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
    ignore;

send_response_statemachine(Method, Status, terminated) when Status >= 100, Status =< 699 ->
    logger:log(debug, "UAS decision: Requested to send response ~p to ~s when in state 'terminated' - " ++
	       "ignoring", [Status, Method]),
    ignore.

%%--------------------------------------------------------------------
%% @spec    (Method, Status, State) -> ignore | send
%%
%% @doc     Check the plan to send a response against the rules in
%%          RFC4320 (SIP Non-INVITE Actions) which updates RFC3261.
%% @end
%%--------------------------------------------------------------------
send_response_non_invite_check(Method, 100, State) when Method /= "INVITE" ->
    T2hasFired = (State#state.nit_100 == true),
    IsReliable = State#state.is_rel_sock,

    if
	IsReliable == true ->
	    %% An SIP element MAY respond to a non-INVITE request with a Status-Code
	    %% of 100 over a reliable transport at any time.
	    send;
	T2hasFired == true, State#state.sipstate == trying ->
	    send;
	true ->
	    %% Either Timer T2 hadn't fired yet, or we are not in SIP-state 'trying'
	    %% anymore (meaning that we have sent some other response already)
	    LogTag = State#state.logtag,
	    logger:log(normal, "~s: Ignoring request to send '100 Trying' response to non-INVITE request "
		       "over non-reliable transport based on the rules in RFC4320 (which updates RFC3261)", [LogTag]),
	    ignore
    end;

send_response_non_invite_check(Method, Status, State) when Method /= "INVITE", Status >= 101, Status =< 199 ->
    %% "An SIP element MUST NOT send any provisional response with a Status-
    %%  Code other than 100 to a non-INVITE request." RFC4320 #4.1 (Action 1)
    LogTag = State#state.logtag,
    logger:log(normal, "~s: NOT sending non-100 provisional response to non-INVITE request (RFC4320)", [LogTag]),
    ignore;

send_response_non_invite_check(Method, 408, State) when Method /= "INVITE" ->
    %% RFC4320 #4.2 :
    %% A transaction-stateful SIP element MUST NOT send a response with
    %% Status-Code of 408 to a non-INVITE request.  As a consequence, an
    %% element that cannot respond before the transaction expires will not
    %% send a final response at all.
    LogTag = State#state.logtag,
    logger:log(normal, "~s: NOT sending 408 response to non-INVITE request (RFC4320)", [LogTag]),
    ignore;

send_response_non_invite_check(_Method, _Status, _State) ->
    send.

%%--------------------------------------------------------------------
%% @spec    (Response, SendReliably, State) ->
%%            {ok, NewState}
%%
%%            NewState = #state{}
%%
%% @doc     This server transaction should answer something. If it is
%%          a final, but non-2xx, response to INVITE we also must
%%          store the To-tag we used with the transaction layer so
%%          that it can correctly match the ACK to this server
%%          transaction.
%% @end
%%--------------------------------------------------------------------
send_response(Response, SendReliably, State) when is_record(Response, response), is_record(State, state) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    Socket = State#state.socket,
    LogTag = State#state.logtag,
    SendResponse =
	if
	    Method == "INVITE", Status >= 300 ->
		send_response_process_to_tag(Response, State);
	    true ->
		Response
	end,
    logger:log(debug, "~s: Sending response to '~s ~s' : ~p ~s",
	       [LogTag, Method, sipurl:print(URI), Status, Reason]),
    transportlayer:send_proxy_response(Socket, SendResponse),
    NewState1 =
	case {SendReliably, Method} of
	    {true, "INVITE"} ->
		%% Checking of Method here is just a safety net
		{ok, T1} = yxa_config:get_env(timerT1),
		%% "For unreliable transports, timer G is set to fire in T1 seconds,
		%%  and is not set to fire for reliable transports." RFC3261 #17.2.1
		NewState2 =
		    case State#state.is_rel_sock of
			true ->
			    State;
			false ->
			    TimerG = T1,
			    GDesc = "resendresponse " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++
				sipurl:print(URI) ++ " (Timer G)",
			    add_timer(TimerG, GDesc, {resendresponse}, State)
		    end,
		TimerH = 64 * T1,
		HDesc = "stopresend of " ++ integer_to_list(Status) ++ " " ++ Reason ++ " to " ++
		    sipurl:print(URI) ++ " (Timer H)",
		NewState3 = add_timer(TimerH, HDesc, {resendresponse_timeout}, NewState2),
		NewState3;
	    {false, _} ->
		State
	end,
    NewState = NewState1#state{response=Response},
    {ok, NewState}.

send_response_process_to_tag(Response, State) ->
    {Status, Reason, RHeader} = {Response#response.status, Response#response.reason, Response#response.header},
    {LogTag, Request} = {State#state.logtag, State#state.request},
    %% Tell transactionlayer pid about this response to INVITE - it needs to know
    %% what To-tag we have used in order to match any ACK:s received for this
    %% response back to this server transaction.
    {StoreToTag, NewResponse} =
	case sipheader:get_tag(keylist:fetch('to', RHeader)) of
	    none ->
		%% RFC3261 #16.7 (Response Processing) says a proxy SHOULD preserve To-tags,
		%% MUST NOT modify if a To-tag is set, but also notes that it makes no
		%% difference to upstreams if a proxy should modify To-tag on 3-6xx responses
		%% to INVITE. However, this is a response without a To-tag and we can be more
		%% confident that an ACK is for this response if we set a To-tag in it, so we do.
		logger:log(debug, "~s: No To-tag in 3/4/5/6xx response to INVITE received (~p ~s), "
			   "using mine (~p)", [LogTag, Status, Reason, State#state.my_to_tag]),
		{DisplayName, ToURI} = sipheader:to(keylist:fetch('to', Response#response.header)),
		[NewTo] = sipheader:contact_print([ contact:new(DisplayName, ToURI,
								[{"tag", State#state.my_to_tag}])
						   ]),
		NewHeader = keylist:set("To", [NewTo], Response#response.header),
		SendResponse1_1 = Response#response{header = NewHeader},
		{State#state.my_to_tag, SendResponse1_1};
	    ToTag when is_list(ToTag) ->
		{ToTag, Response}
	end,
    case State#state.testing of
	true -> ok;
	false ->
	    case transactionlayer:store_to_tag(Request, StoreToTag) of
		ok -> true;
		R ->
		    %% Abort sending? Nah, this way we get the response to the caller, even
		    %% though we might resend it a couple of times because the transaction
		    %% layer might fail to match the ACK to this server transaction
		    logger:log(error, "~s: Failed storing To-tag with transactionlayer : ~p",
			       [LogTag, R])
	    end
    end,
    NewResponse.

add_timer(Timeout, Description, AppSignal, State) when is_record(State, state) ->
    NewTimerList = siptimer:add_timer(Timeout, Description, AppSignal, State#state.timerlist),
    State#state{timerlist=NewTimerList}.

del_timer(AppSignal, State) ->
    NewTL = siptimer:cancel_timers_with_appsignal(AppSignal, State#state.timerlist),
    State#state{timerlist = NewTL}.

%%--------------------------------------------------------------------
%% @spec    (SipState, State) ->
%%            State
%%
%%            SipState = trying     |
%%                       proceeding |
%%                       completed  |
%%                       confirmed  |
%%                       terminated
%%            State    = #state{}
%%
%%            State = #state{}
%%
%% @doc     We are already in SIP state SipState, just return.
%% @end
%%--------------------------------------------------------------------
enter_sip_state(SipState, #state{sipstate=SipState}=State) ->
    State;

%%--------------------------------------------------------------------
%% @spec    (proceeding, State) ->
%%            State
%%
%%            State = #state{}
%%
%%            State = #state{}
%%
%% @doc     Check if we are a non-INVITE transaction. If so, we might
%%          need to cancel the {send_nit_100} timer because it is no
%%          longer needed.
%% @end
%%--------------------------------------------------------------------
enter_sip_state(proceeding, State) ->
    Method = (State#state.request)#request.method,
    NewState1 = State#state{sipstate = proceeding},
    case Method /= "INVITE" andalso
	State#state.is_rel_sock == true andalso
	State#state.nit_100 == false of
	true ->
	    %% cancel the {send_nit_100} timer since we have apparently sent a '100 Trying'
	    %% on behalf of our Transaction User, instead of because of the timer
	    del_timer({send_nit_100}, NewState1);
	false ->
	    NewState1
    end;

%%--------------------------------------------------------------------
%% @spec    (completed, State) ->
%%            State
%%
%%            State = #state{}
%%
%%            State = #state{}
%%
%% @doc     Check if we are a non-INVITE transaction. If so, set up
%%          Timer J to terminate this transaction in 64 * T1 ms.
%% @end
%%--------------------------------------------------------------------
enter_sip_state(completed, State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    NewState1 = State#state{sipstate = completed},
    Request = State#state.request,
    {ResponseToMethod, ResponseToURI} = {Request#request.method, Request#request.uri},
    case ResponseToMethod == "INVITE" of
	true ->
	    NewState1;
	false ->
	    {ok, T1} = yxa_config:get_env(timerT1),
	    TimerJ = 64 * T1,
	    logger:log(debug, "~s: Server transaction: Entered state 'completed'. Original request was non-INVITE, "
		       "starting Timer J with a timeout of ~s seconds.",
		       [LogTag, siptimer:timeout2str(TimerJ)]),
	    %% Install TimerJ (default 32 seconds) RFC 3261 17.2.2. Until TimerJ fires we
	    %% resend our response whenever we receive a request resend.
	    JDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++
		" (Timer J)",
	    NewState2 = add_timer(TimerJ, JDesc, {terminate_transaction}, NewState1),
	    %% cancel the {send_nit_100} timer since we have apparently produced a final response now
	    del_timer({send_nit_100}, NewState2)
    end;

%%--------------------------------------------------------------------
%% @spec    (confirmed, State) ->
%%            State
%%
%%            State = #state{}
%%
%%            State = #state{}
%%
%% @doc     We are an INVITE transaction that has sent a reply which
%%          has been ACK:ed. Stay alive for a few seconds (default 5)
%%          to absorb any additional ACK:s that might arrive if we've
%%          sent our response more than once.
%% @end
%%--------------------------------------------------------------------
enter_sip_state(confirmed, State) when is_record(State, state) ->
    LogTag = State#state.logtag,
    NewState1 = State#state{sipstate=confirmed},
    Request = State#state.request,
    {ResponseToMethod, ResponseToURI} = {Request#request.method, Request#request.uri},
    {ok, TimerI} = yxa_config:get_env(timerT4),
    logger:log(debug, "~s: Entered state 'confirmed'. Original request was an INVITE, starting " ++
	       "Timer I with a timeout of ~s seconds.",  [LogTag, siptimer:timeout2str(TimerI)]),
    %% Install TimerI (T4, default 5 seconds) RFC 3261 17.2.1. Until TimerI fires we
    %% absorb any additional ACK requests that might arrive.
    IDesc = "terminate server transaction " ++ ResponseToMethod ++ " " ++ sipurl:print(ResponseToURI) ++
	" (Timer I)",
    add_timer(TimerI, IDesc, {terminate_transaction}, NewState1);
enter_sip_state(NewSipState, State) when is_record(State, state), NewSipState == trying;
					 NewSipState == proceeding; NewSipState == terminated ->
    LogTag = State#state.logtag,
    logger:log(debug, "~s: Entering state '~p'", [LogTag, NewSipState]),
    State#state{sipstate=NewSipState}.

%%--------------------------------------------------------------------
%% @spec    (State) ->
%%            State
%%
%%            State = #state{}
%%
%%            State = #state{}
%%
%% @doc     We are an INVITE transaction that has just received an
%%          ACK. Check that we have actually sent a final response,
%%          and then cancel any response resend siptimers and go into
%%          SIP state 'confirmed'.
%% @end
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
		    NewState1 = del_timer({resendresponse}, State),
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
%% @spec    (Status, Reason, RBody, ExtraHeaders, ViaParameters,
%%          State) ->
%%            Response
%%
%%            Status        = integer() "SIP status code"
%%            Reason        = string() "SIP reason phrase"
%%            RBody         = integer() "body of response"
%%            ExtraHeaders  = [{Key, ValueList}]
%%            ViaParameters = term()
%%            State         = #state{}
%%
%%            Response = #response{}
%%
%% @doc     If there is no To-tag in our requests header, put ours
%%          there. Then use siprequest:make_response/7 to create a
%%          response from our requests headers.
%% @end
%%--------------------------------------------------------------------
make_response(100, Reason, RBody, ExtraHeaders, ViaParameters, State)
  when is_record(State, state), is_list(Reason), is_binary(RBody), is_list(ExtraHeaders) ->
    %% For 100 Trying, we use the To-header verbatim from the Request. If it has a To-tag
    %% we leave it there, if it has no To-tag we don't add one.
    %% RFC3261 #8.2.6.2 (Headers and Tags) says a 100 Trying response MAY contain a To-tag.
    siprequest:make_response(100, Reason, RBody, ExtraHeaders, ViaParameters,
			     State#state.socket, State#state.request);

make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State)
  when is_record(State, state), is_integer(Status), is_list(Reason), is_binary(RBody), is_list(ExtraHeaders) ->
    Header = (State#state.request)#request.header,
    To = keylist:fetch('to', Header),
    Request =
	case sipheader:get_tag(To) of
	    none ->
		{DisplayName, ToURI} = sipheader:to(To),
		[NewTo] = sipheader:contact_print(
			    [ contact:new(DisplayName, ToURI, [{"tag", State#state.my_to_tag}]) ]),
		NewHeader = keylist:set("To", [NewTo], Header),
		(State#state.request)#request{header=NewHeader};
	    _ ->
		%% If the request is part of an existing dialog, it will have a To-tag
		State#state.request
	  end,
    siprequest:make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State#state.socket, Request).


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback Note : Not much is tested in this module
%%          at the moment because almost every function includes
%%          communication with the outside world (receiving or
%%          sending signals/SIP-messages) which the current test
%%          framework does not allow testing of.
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    Test_OrigRequest =
	#request{method = "INVITE",
		 uri    = sipurl:parse("sip:user@example.org"),
		 header = keylist:from_list([{"Via",     ["SIP/2.0/YXA-TEST 192.0.2.27"]},
					     {"From",    ["Test <sip:test@example.org>;tag=f-tag"]},
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


    %% test send_response_statemachine(Method, Status, SipState)
    %%--------------------------------------------------------------------

    %% 17.2.1 INVITE Server Transaction

    autotest:mark(?LINE, "send_response_statemachine/3 INVITE - 1"),
    %% When a server transaction is constructed for a request, it enters the
    %% "Proceeding" state.
    %% Send 100 Trying only in 'trying'. 'trying' for INVITE is not in the RFC3261
    %% spec, it is our internal way of saying "we haven't sent a 100 Trying yet".
    {send, false, proceeding} = send_response_statemachine("INVITE", 100, trying),

    autotest:mark(?LINE, "send_response_statemachine/3 INVITE - 2"),
    %% The TU passes any number of provisional responses to the server
    %% transaction.  So long as the server transaction is in the
    %% "Proceeding" state, each of these MUST be passed to the transport
    %% layer for transmission.  They are not sent reliably by the
    %% transaction layer (they are not retransmitted by it) and do not cause
    %% a change in the state of the server transaction.
    {send, false, proceeding} = send_response_statemachine("INVITE", 101, proceeding),
    {send, false, proceeding} = send_response_statemachine("INVITE", 183, proceeding),
    {send, false, proceeding} = send_response_statemachine("INVITE", 199, proceeding),

    autotest:mark(?LINE, "send_response_statemachine/3 INVITE - 3"),
    %% If, while in the "Proceeding" state, the TU passes a 2xx response to
    %% the server transaction, the server transaction MUST pass this
    %% response to the transport layer for transmission.  It is not
    %% retransmitted by the server transaction; retransmissions of 2xx
    %% responses are handled by the TU.  The server transaction MUST then
    %% transition to the "Terminated" state.
    {send, false, terminated} = send_response_statemachine("INVITE", 200, proceeding),

    autotest:mark(?LINE, "send_response_statemachine/3 INVITE - 4"),
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

    autotest:mark(?LINE, "send_response_statemachine/3 INVITE - 5"),
    %% Don't send 100 Trying in any other state than 'trying'. This means that the TU
    %% will be ignored if it asks us to send out a 100 Trying, since we did that ourselves
    %% when in our internal mode (for INVITE) 'trying'.
    ignore = send_response_statemachine("INVITE", 100, proceeding),
    ignore = send_response_statemachine("INVITE", 100, completed),
    ignore = send_response_statemachine("INVITE", 100, terminated),

    autotest:mark(?LINE, "send_response_statemachine/3 INVITE - 6"),
    %% don't send any more responses once we have sent a final response
    ignore = send_response_statemachine("INVITE", 100, completed),
    ignore = send_response_statemachine("INVITE", 200, completed),
    ignore = send_response_statemachine("INVITE", 300, completed),
    ignore = send_response_statemachine("INVITE", 400, completed),
    ignore = send_response_statemachine("INVITE", 500, completed),
    ignore = send_response_statemachine("INVITE", 600, completed),

    autotest:mark(?LINE, "send_response_statemachine/3 INVITE - 7"),
    %% don't send any more responses if we should happen to end up here even if we are
    %% really supposed to have terminated (like if we are emptying our mailbox before
    %% _really_ terminating)
    ignore = send_response_statemachine("INVITE", 100, terminated),
    ignore = send_response_statemachine("INVITE", 200, terminated),
    ignore = send_response_statemachine("INVITE", 300, terminated),
    ignore = send_response_statemachine("INVITE", 400, terminated),
    ignore = send_response_statemachine("INVITE", 500, terminated),
    ignore = send_response_statemachine("INVITE", 600, terminated),

    %% 17.2.2 Non-INVITE Server Transaction

    %% The state machine is initialized in the "Trying" state ...

    autotest:mark(?LINE, "send_response_statemachine/3 non-INVITE - 1"),
    %% While in the "Trying" state, if the TU passes a provisional response
    %% to the server transaction, the server transaction MUST enter the
    %% "Proceeding" state.  The response MUST be passed to the transport
    %% layer for transmission.
    {send, false, proceeding} = send_response_statemachine("OPTIONS", 100, trying),
    {send, false, proceeding} = send_response_statemachine("OPTIONS", 199, trying),

    autotest:mark(?LINE, "send_response_statemachine/3 non-INVITE - 2"),
    %% Any further provisional responses that are received from the TU while in the
    %% "Proceeding" state MUST be passed to the transport layer for transmission.
    %% YXA note: we filter out 100 Trying
    ignore = send_response_statemachine("OPTIONS", 100, proceeding),
    {send, false, proceeding} = send_response_statemachine("OPTIONS", 199, proceeding),

    autotest:mark(?LINE, "send_response_statemachine/3 non-INVITE - 3"),
    %% If the TU passes a final response (status codes 200-699) to the server while
    %% in the "Proceeding" state, the transaction MUST enter the "Completed" state,
    %% and the response MUST be passed to the transport layer for transmission.
    {send, false, completed} = send_response_statemachine("OPTIONS", 200, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 300, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 400, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 500, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 600, proceeding),
    {send, false, completed} = send_response_statemachine("OPTIONS", 699, proceeding),

    autotest:mark(?LINE, "send_response_statemachine/3 non-INVITE - 4"),
    %% Any other final responses passed by the TU to the server transaction MUST
    %% be discarded while in the "Completed" state.
    ignore = send_response_statemachine("OPTIONS", 200, completed),
    ignore = send_response_statemachine("OPTIONS", 300, completed),
    ignore = send_response_statemachine("OPTIONS", 400, completed),
    ignore = send_response_statemachine("OPTIONS", 500, completed),
    ignore = send_response_statemachine("OPTIONS", 600, completed),
    ignore = send_response_statemachine("OPTIONS", 699, completed),

    %% own conclusions

    autotest:mark(?LINE, "send_response_statemachine/3 non-INVITE - 5"),
    %% provisional responses received from the TU when already completed is ignored
    ignore = send_response_statemachine("OPTIONS", 100, completed),
    ignore = send_response_statemachine("OPTIONS", 199, completed),

    autotest:mark(?LINE, "send_response_statemachine/3 non-INVITE - 6"),
    %% handle 2/3/4/5/6xx responses when in state 'trying' as we would in state 'proceeding'
    {send, false, completed} = send_response_statemachine("OPTIONS", 200, trying),
    {send, false, completed} = send_response_statemachine("OPTIONS", 300, trying),
    {send, false, completed} = send_response_statemachine("OPTIONS", 400, trying),
    {send, false, completed} = send_response_statemachine("OPTIONS", 500, trying),
    {send, false, completed} = send_response_statemachine("OPTIONS", 600, trying),
    {send, false, completed} = send_response_statemachine("OPTIONS", 699, trying),


    %% handle_call({get_branch}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:call() {get_branch} - 1"),
    %% is just normal case
    GetBranchState = #state{branch = "Branch"},
    {reply, {ok, "Branch"}, GetBranchState, ?TIMEOUT} =
	handle_call({get_branch}, undefined, GetBranchState),


    %% handle_call({set_report_to, Pid}, ...)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "gen_server:call() {set_report_to, Pid} - 0"),
    SetReportTo_DeadPid = spawn(fun() -> ok end),
    erlang:yield(),	%% make sure SetReportTo_DeadPid finishes
    SetReportTo_S = #state{logtag = "test",
			   sipstate = trying,
			   cancelled = false
			  },

    autotest:mark(?LINE, "gen_server:call() {set_report_to, Pid} - 1"),
    %% test already set
    SetReportTo_S1 = SetReportTo_S#state{report_to = SetReportTo_DeadPid},
    {reply, {error, "Server transaction already adopted"}, SetReportTo_S1, ?TIMEOUT} =
	handle_call({set_report_to, self()}, undefined, SetReportTo_S1),

    autotest:mark(?LINE, "gen_server:call() {set_report_to, Pid} - 2"),
    %% test cancelled
    SetReportTo_S2 = SetReportTo_S#state{cancelled = true},
    {reply, {ignore, cancelled}, SetReportTo_S2, ?TIMEOUT} =
	handle_call({set_report_to, self()}, undefined, SetReportTo_S2),

    autotest:mark(?LINE, "gen_server:call() {set_report_to, Pid} - 3"),
    %% test normal case (sipstate: trying)
    SetReportTo_S3_1 = SetReportTo_S#state{sipstate = trying},
    SetReportTo_S3_2 = SetReportTo_S3_1#state{report_to = self()},
    {reply, ok, SetReportTo_S3_2, ?TIMEOUT} =
	handle_call({set_report_to, self()}, undefined, SetReportTo_S3_1),

    autotest:mark(?LINE, "gen_server:call() {set_report_to, Pid} - 4"),
    %% test normal case (sipstate: proceeding)
    SetReportTo_S4_1 = SetReportTo_S#state{sipstate = proceeding},
    SetReportTo_S4_2 = SetReportTo_S4_1#state{report_to = self()},
    {reply, ok, SetReportTo_S4_2, ?TIMEOUT} =
	handle_call({set_report_to, self()}, undefined, SetReportTo_S4_1),

    autotest:mark(?LINE, "gen_server:call() {set_report_to, Pid} - 5"),
    %% test already completed (sipstate: completed)
    SetReportTo_S5 = SetReportTo_S#state{sipstate = completed},
    {reply, {ignore, completed}, SetReportTo_S5, ?TIMEOUT} =
	handle_call({set_report_to, self()}, undefined, SetReportTo_S5),


    %% handle_call(get_my_to_tag, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:call() get_my_to_tag - 1"),
    %% is just normal case
    GetMyToTagState = #state{my_to_tag = "to-tag"},
    {reply, {ok, "to-tag"}, GetMyToTagState, ?TIMEOUT} =
	handle_call(get_my_to_tag, undefined, GetMyToTagState),


    %% handle_cast({siprequest, Request, Origin}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:call() {siprequest, ...} - 0"),
    SipRequest_State =
	#state{logtag    = "testing",
	       request   = Test_OrigRequest,
	       sipstate  = proceeding,
	       socket    = #sipsocket{proto = yxa_test}
	      },

    autotest:mark(?LINE, "gen_server:call() {siprequest, ...} - 1.1"),
    %% test resend scenario
    SipRequest_State1 =
	SipRequest_State#state{response  = Test_Response,
			       my_to_tag = "t-tag"
			      },
    {noreply, SipRequest_State1, ?TIMEOUT} =
	handle_cast({siprequest, Test_OrigRequest, #siporigin{}}, SipRequest_State1),

    test_verify_response_was_sent(100, "Testing", <<>>,
				  "gen_server:call() {siprequest, ...}", 1, 2),

    autotest:mark(?LINE, "gen_server:call() {siprequest, ...} - 2"),
    %% test resend scenario without a response to resend
    SipRequest_State2 =
	SipRequest_State#state{response  = undefined},
    {noreply, SipRequest_State2, ?TIMEOUT} =
	handle_cast({siprequest, Test_OrigRequest, #siporigin{}}, SipRequest_State2),


    autotest:mark(?LINE, "gen_server:call() {siprequest, ...} - 3.1"),
    %% test received ACK
    SipRequest_State3_in = SipRequest_State#state{timerlist = siptimer:empty(),
						  response  = Test_Response,
						  sipstate  = completed
						 },
    SipRequest_Request3 = Test_OrigRequest#request{method = "ACK"},
    {noreply, SipRequest_State3_out, ?TIMEOUT} =
	handle_cast({siprequest, SipRequest_Request3, #siporigin{}}, SipRequest_State3_in),
    autotest:mark(?LINE, "gen_server:call() {siprequest, ...} - 3.2"),
    %% check that a timer was set up
    [{terminate_transaction}] = siptimer:test_get_appsignals(SipRequest_State3_out#state.timerlist),
    autotest:mark(?LINE, "gen_server:call() {siprequest, ...} - 3.3"),
    %% cancel timer
    SipRequest_3_NewTL = siptimer:cancel_all_timers(SipRequest_State3_out#state.timerlist),
    SipRequest_State3_out1 = SipRequest_State3_out#state{timerlist = SipRequest_3_NewTL},
    autotest:mark(?LINE, "gen_server:call() {siprequest, ...} - 3.4"),
    %% and finally verify that SipRequest_State3_out1 is the same as SipRequest_State3_in
    %% with sipstate changed from 'completed' to 'confirmed'
    confirmed = SipRequest_State3_out1#state.sipstate,
    SipRequest_State3_out2 = SipRequest_State3_out1#state{sipstate = completed},
    SipRequest_State3_out2 = SipRequest_State3_in,

    autotest:mark(?LINE, "gen_server:call() {siprequest, ...} - 4"),
    %% test resend scenario with bad new request (different Request-URI)
    SipRequest_State4 = SipRequest_State,
    SipRequest_Request4 = Test_OrigRequest#request{uri = sipurl:parse("sip:changed@example.org")},
    {noreply, SipRequest_State4, ?TIMEOUT} =
	handle_cast({siprequest, SipRequest_Request4, #siporigin{}}, SipRequest_State2),


    %% unknown handle_call
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "unknown gen_server:call() - 1"),
    UnknownCallState = #state{},
    {reply, error, UnknownCallState, ?TIMEOUT} = handle_call(undefined, none, UnknownCallState),


    %% process_received_ack(State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_received_ack/1 - 0"),
    ReceivedAckRequest1 = #request{method = "INVITE",
				   uri    = sipurl:parse("sip:user@example.org")
				  },
    ReceivedAckResponse = #response{status = 400,
				    reason = "Testing"
				   },
    ReceivedAckState = #state{logtag    = "testing",
			      request   = ReceivedAckRequest1,
			      response  = ReceivedAckResponse,
			      timerlist = siptimer:empty()
			     },

    autotest:mark(?LINE, "process_received_ack/1 - 1"),
    %% test ACK when 'trying', no change expected
    ReceivedAckState_1 = ReceivedAckState#state{sipstate = trying},
    ReceivedAckState_1 = process_received_ack(ReceivedAckState_1),

    autotest:mark(?LINE, "process_received_ack/1 - 2"),
    %% test ACK when 'proceeding', no change expected
    ReceivedAckState_2 = ReceivedAckState#state{sipstate = proceeding},
    ReceivedAckState_2 = process_received_ack(ReceivedAckState_2),

    autotest:mark(?LINE, "process_received_ack/1 - 3.1"),
    %% test normal case
    ReceivedAckState_3 = process_received_ack(ReceivedAckState#state{sipstate = completed}),

    autotest:mark(?LINE, "process_received_ack/1 - 3.2"),
    %% check that the expected changes were made
    confirmed = ReceivedAckState_3#state.sipstate,
    [{terminate_transaction}] = siptimer:test_get_appsignals(ReceivedAckState_3#state.timerlist),
    ReceivedAckState_3_1 = ReceivedAckState_3#state{timerlist = undefined, sipstate = undefined},
    ReceivedAckState_3_1 = ReceivedAckState#state{timerlist = undefined},

    autotest:mark(?LINE, "process_received_ack/1 - 3.3"),
    %% cancel timer started
    siptimer:cancel_all_timers(ReceivedAckState_3#state.timerlist),

    autotest:mark(?LINE, "process_received_ack/1 - 4"),
    %% test ACK to non-INVITE, no change expected
    ReceivedAckRequest_4 = ReceivedAckRequest1#request{method = "OPTIONS"},
    ReceivedAckState_4 = ReceivedAckState#state{request = ReceivedAckRequest_4},
    ReceivedAckState_4 = process_received_ack(ReceivedAckState_4),

    autotest:mark(?LINE, "process_received_ack/1 - 5"),
    %% test ACK when no response sent, no change expected
    ReceivedAckState_5 = ReceivedAckState#state{response = undefined},
    ReceivedAckState_5 = process_received_ack(ReceivedAckState_5),


    %% handle_cast({create_response, Status, Reason, ExtraHeaders, RBody}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {create_response, ...} - 0"),
    CreateResponse_State =
	#state{logtag    = "testing",
	       request   = Test_OrigRequest,
	       sipstate  = trying,
	       socket    = #sipsocket{proto = yxa_test},
	       branch    = "test-branch"
	      },

    autotest:mark(?LINE, "gen_server:cast() {create_response, ...} - 1.1"),
    %% test normal case: 100 Trying
    {noreply, CreateResponse_State1, ?TIMEOUT} =
	handle_cast({create_response, 100, "Testing", [], <<>>}, CreateResponse_State),

    autotest:mark(?LINE, "gen_server:cast() {create_response, ...} - 1.2"),
    proceeding = CreateResponse_State1#state.sipstate,
    100 = (CreateResponse_State1#state.response)#response.status,
    "Testing" = (CreateResponse_State1#state.response)#response.reason,

    test_verify_response_was_sent(100, "Testing", <<>>,
				  "gen_server:cast() {create_response, ...}", 1, 3),

    autotest:mark(?LINE, "gen_server:cast() {create_response, ...} - 2"),
    %% test '101 Testing' with invalid SIP-state, should not work (no change expected)
    CreateResponse_State2 = CreateResponse_State#state{sipstate = false,
						       my_to_tag = "to123"
						      },
    {noreply, CreateResponse_State2, ?TIMEOUT} =
	handle_cast({create_response, 101, "Testing", [], <<>>}, CreateResponse_State2),


    autotest:mark(?LINE, "gen_server:cast() {create_response, ...} - 3"),
    %% test '100 Testing' when alreday 'proceeding', no change expected
    CreateResponse_State3 = CreateResponse_State#state{sipstate = proceeding},
    {noreply, CreateResponse_State3, ?TIMEOUT} =
	handle_cast({create_response, 100, "Testing", [], <<>>}, CreateResponse_State3),


    %% handle_cast({forwardresponse, Status, Reason, ExtraHeaders, RBody}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {forwardresponse, ...} - 0"),
    ForwardResponse_State =
	#state{logtag    = "testing",
	       request   = Test_OrigRequest,
	       sipstate  = trying,
	       socket    = #sipsocket{proto = yxa_test},
	       branch    = "test-branch",
	       my_to_tag = "to321",
	       testing   = true
	      },
    ForwardResponse_Response = Test_Response#response{status = 404},

    autotest:mark(?LINE, "gen_server:cast() {forwardresponse, ...} - 1.1"),
    %% test normal case: 404 Testing
    {noreply, ForwardResponse_State1, ?TIMEOUT} =
	handle_cast({forwardresponse, ForwardResponse_Response}, ForwardResponse_State),

    autotest:mark(?LINE, "gen_server:cast() {forwardresponse, ...} - 1.2"),
    %% verify new state
    completed = ForwardResponse_State1#state.sipstate,
    404 = (ForwardResponse_State1#state.response)#response.status,
    "Testing" = (ForwardResponse_State1#state.response)#response.reason,

    autotest:mark(?LINE, "gen_server:cast() {forwardresponse, ...} - 1.3"),
    %% verify that we 'sent' the 404 response
    test_verify_response_was_sent(404, "Testing", <<>>,
				  "gen_server:cast() {forwardresponse, ...}", 1, 3),

    autotest:mark(?LINE, "gen_server:cast() {forwardresponse, ...} - 2.1"),
    %% test forwarding response without To-tag
    ForwardResponse_Header2_1 = Test_Response#response.header,
    ForwardResponse_Header2 = keylist:set("To", ["sip:no-to-tag@example.org"], ForwardResponse_Header2_1),
    ForwardResponse_Response2 = ForwardResponse_Response#response{header = ForwardResponse_Header2},

    {noreply, ForwardResponse_State2, ?TIMEOUT} =
	handle_cast({forwardresponse, ForwardResponse_Response2}, ForwardResponse_State),

    autotest:mark(?LINE, "gen_server:cast() {forwardresponse, ...} - 2.2"),
    %% verify new state
    completed = ForwardResponse_State2#state.sipstate,
    404 = (ForwardResponse_State2#state.response)#response.status,
    "Testing" = (ForwardResponse_State2#state.response)#response.reason,

    %% verify that we 'sent' the 404 response
    test_verify_response_was_sent(404, "Testing", <<>>,
				  "gen_server:cast() {forwardresponse, ...}", 2, 3),


    %% handle_cast({expired}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {expired} - 1"),
    ExpiredState = #state{logtag = "testing"},
    {stop, _, ExpiredState} = handle_cast({expired}, ExpiredState),


    %% handle_cast({cancelled, ExtraHeaders}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {cancelled, ...} - 1.1"),
    %% test normal case
    CancelledState1 = #state{sipstate  = trying,
			     report_to = self(),
			     cancelled = false
			   },
    {noreply, CancelledState1_out, ?TIMEOUT} = handle_cast({cancelled, []}, CancelledState1),

    autotest:mark(?LINE, "gen_server:cast() {cancelled, ...} - 1.2"),
    %% verify new state
    true = CancelledState1_out#state.cancelled,
    CancelledState1_out1 = CancelledState1#state{cancelled = true},
    CancelledState1_out1 = CancelledState1_out,

    autotest:mark(?LINE, "gen_server:cast() {cancelled, ...} - 1.3"),
    %% check that we got a servertransaction_cancelled message (since parent = self())
    CancelledMyself = self(),
    receive
	{servertransaction_cancelled, CancelledMyself, []} ->
	    ok
    after 0 ->
	    throw("test failed, no {servertransaction_cancelled, ...} signal in process mailbox")
    end,

    autotest:mark(?LINE, "gen_server:cast() {cancelled, ...} - 2.1"),
    %% test without report_to, in which case we should generate a 487 response
    CancelledState2 = #state{logtag    = "testing",
			     sipstate  = trying,
			     report_to = undefined,
			     cancelled = false,
			     my_to_tag = "to-tag111",
			     socket    = #sipsocket{proto = yxa_test},
			     request   = Test_OrigRequest,
			     testing   = true
			   },
    {noreply, CancelledState2_out, ?TIMEOUT} = handle_cast({cancelled, []}, CancelledState2),

    autotest:mark(?LINE, "gen_server:cast() {cancelled, ...} - 2.2"),
    %% verify new state
    true = CancelledState2_out#state.cancelled,
    CancelledState2 = CancelledState2_out#state{cancelled = false,
						response  = undefined,
						sipstate  = trying
					       },

    test_verify_response_was_sent(487, "Request Cancelled", <<>>,
				  "gen_server:cast() {cancelled, ...}", 2, 3),

    autotest:mark(?LINE, "gen_server:cast() {cancelled, ...} - 3"),
    %% test states that should return in no action
    lists:map(fun(S) ->
		      CS3 = #state{logtag   = "testing",
				   request  = Test_OrigRequest,
				   sipstate = S
					      },
		      {noreply, CS3, ?TIMEOUT} = handle_cast({cancelled, []}, CS3)
	      end, [completed, confirmed, testing]),	%% can't check terminated with this fun



    %% handle_cast({quit}, ...)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server:cast() {quit} - 1"),
    QuitState = #state{logtag = "testing"},
    {stop, _, QuitState} = handle_cast({quit}, QuitState),


    %% unknown handle_cast
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "unknown gen_server:cast() - 1"),
    UnknownCastState = #state{},
    {noreply, UnknownCastState, ?TIMEOUT} = handle_cast(undefined, UnknownCastState),


    %% handle_info(timeout, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server signal 'timeout' - 0"),
    WaitFun = fun() ->
		      receive
			  _ -> ok
		      end
	      end,
    TimeoutReportTo = spawn(WaitFun),
    TimeoutMonitorRef = erlang:monitor(process, TimeoutReportTo),
    TimeoutState = #state{logtag	= "testing",
			  request	= Test_OrigRequest,
			  sipstate	= proceeding,
			  my_to_tag	= "totag",
			  socket	= #sipsocket{proto = yxa_test},
			  is_rel_sock	= false,
			  timerlist	= siptimer:empty(),
			  response	= Test_Response,
			  report_to	= TimeoutReportTo,
			  parent	= TimeoutReportTo,
			  testing	= true
			 },

    autotest:mark(?LINE, "gen_server signal 'timeout' - 1.1"),
    {noreply, TimeoutState_out, ?TIMEOUT} = handle_info(timeout, TimeoutState),

    autotest:mark(?LINE, "gen_server signal 'timeout' - 1.2"),
    %% verify new state
    completed = TimeoutState_out#state.sipstate,
    [{resendresponse},{resendresponse_timeout}] =
	siptimer:test_get_appsignals(TimeoutState_out#state.timerlist),
    %% cancel timers
    siptimer:cancel_all_timers(TimeoutState_out#state.timerlist),

    test_verify_response_was_sent(500, "Server Internal Error", <<>>,
				  "gen_server signal 'timeout'", 1, 3),

    autotest:mark(?LINE, "gen_server signal 'timeout' - 1.6"),
    %% verify that report_to/parent was killed
    receive
	{'DOWN', TimeoutMonitorRef, process, TimeoutReportTo, servertransaction_timed_out} ->
	    ok
    after 0 ->
	    throw("test failed, no 'DOWN' message received from report_to/parent pid")
    end,

    %% handle_info({siptimer, TRef, TDesc}, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server signal '{siptimer, ...}' - 0"),
    %% set up a timer and let it fire
    SipTimerTest_Timerlist1 = siptimer:add_timer(1, "Testing testing",
						 {terminate_transaction},
						 siptimer:empty()
						),
    TimerTest_State = #state{timerlist = SipTimerTest_Timerlist1,
			     request   = Test_OrigRequest
			    },

    TimerTest_Signal =
	receive
	    {siptimer, TimerTest_Ref1, "Testing testing"} ->
		{siptimer, TimerTest_Ref1, "Testing testing"}
	after 100 ->
		throw("test failed, the timer we set up never fired")
	end,

    autotest:mark(?LINE, "gen_server signal '{siptimer, ...}' - 1"),
    %% test normal case, the transaction should terminate
    {stop, normal, #state{sipstate = terminated}} = handle_info(TimerTest_Signal, TimerTest_State),

    autotest:mark(?LINE, "gen_server signal '{siptimer, ...}' - 1"),
    %% test with unknown timer
    {noreply, TimerTest_State, ?TIMEOUT} =
	handle_info({siptimer, make_ref(), "Testing unknown timer"}, TimerTest_State),


    %% handle_info({'EXIT', ...}, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 0"),
    ExitSignal_State = #state{logtag		= "testing",
			      parent		= self(),
			      request		= Test_OrigRequest,
			      socket		= #sipsocket{proto = yxa_test},
			      is_rel_sock	=  false,
			      my_to_tag		= "ttt",
			      timerlist		= siptimer:empty(),
			      testing		= true
			     },

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 1"),
    %% test parent exit normal when completed, no change expected
    ExitSignal_State1 = ExitSignal_State#state{sipstate = completed},
    {noreply, ExitSignal_State1, ?TIMEOUT} = handle_info({'EXIT', self(), normal}, ExitSignal_State1),

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 2"),
    %% test parent exit abnormally when completed, no change expected
    ExitSignal_State2 = ExitSignal_State#state{sipstate = completed},
    {noreply, ExitSignal_State2, ?TIMEOUT} = handle_info({'EXIT', self(), false}, ExitSignal_State2),

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 3.1"),
    %% test parent exit normally when NOT completed, generates a 500
    ExitSignal_State3 = ExitSignal_State#state{sipstate = proceeding},
    {noreply, ExitSignal_State3_out, ?TIMEOUT} =
	handle_info({'EXIT', self(), normal}, ExitSignal_State3),

    autotest:mark(?LINE, "gen_server signal '{'EXIT', ...}' - 3.2"),
    %% verify new state
    completed = ExitSignal_State3_out#state.sipstate,
    [{resendresponse},{resendresponse_timeout}] =
	siptimer:test_get_appsignals(ExitSignal_State3_out#state.timerlist),
    %% cancel timers
    siptimer:cancel_all_timers(ExitSignal_State3_out#state.timerlist),

    test_verify_response_was_sent(500, "Server Internal Error", <<>>,
				  "gen_server signal '{'EXIT', ...}'", 3, 3),


    %% unknown handle_info
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "unknown gen_server signal - 1"),
    UnknownInfoState = #state{},
    {noreply, UnknownInfoState, ?TIMEOUT} = handle_info(undefined, UnknownInfoState),


    %% terminate(Reason, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "terminate/2 - 1.1"),
    %% test normal case
    TerminateState1 = #state{report_to = self()},
    normal = terminate(normal, TerminateState1),

    autotest:mark(?LINE, "terminate/2 - 1.2"),
    %% verify that report_to (we) got a 'servertransaction_terminating' signal
    TerminateSelf = self(),
    receive
	{servertransaction_terminating, TerminateSelf} ->
	    ok
    after 0 ->
	    throw("test failed, we never got a servertransaction_terminating signal")
    end,

    autotest:mark(?LINE, "terminate/2 - 2"),
    %% test non-normal exit
    "testing" = terminate("testing", #state{}),

    autotest:mark(?LINE, "terminate/2 - 3"),
    %% test with dead report_to
    TerminateDeadPid = spawn(fun() -> ok end),
    erlang:yield(),	%% make sure TerminateDeadPid finishes
    TerminateState3 = #state{report_to = TerminateDeadPid},
    normal = terminate(normal, TerminateState3),


    %% check_quit2(Res, From, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_quit2/3 - 1"),
    %% test Res = {reply, ...} (but From = none)
    CheckQuit_State1 = #state{logtag   = "testing",
			      request  = Test_OrigRequest,
			      sipstate = terminated
			     },
    {stop, normal, CheckQuit_State1} = check_quit2({reply, 1, 2, 3}, none, CheckQuit_State1),

    autotest:mark(?LINE, "check_quit2/3 - 2"),
    %% test Res = {stop, ...}
    CheckQuit_State2 = #state{logtag   = "testing",
			      request  = Test_OrigRequest,
			      response = Test_Response,
			      sipstate = terminated
			     },
    {stop, true, CheckQuit_State2} = check_quit2({stop, true, CheckQuit_State2}, none, CheckQuit_State2),

    autotest:mark(?LINE, "check_quit2/3 - 2"),
    %% test Res = {noreply, ...}
    CheckQuit_State3 = #state{logtag  = "testing",
			      request = Test_OrigRequest,
			      sipstate = terminated
			     },
    {stop, normal, CheckQuit_State3} = check_quit2({noreply, 1, 2}, none, CheckQuit_State3),


    %% process_timer2({resendresponse}, Timer, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_timer2/3 {resendresponse} - 0"),
    %% set up a timer and let it fire
    ResendResponseTimer_Timerlist1 =
	siptimer:add_timer(1, "Testing testing",
			   {resendresponse},
			   siptimer:empty()
			  ),
    ResendResponseTimer_State =
	#state{logtag    = "testing",
	       socket    = #sipsocket{proto = yxa_test},
	       timerlist = ResendResponseTimer_Timerlist1,
	       request   = Test_OrigRequest,
	       response  = Test_Response,
	       sipstate  = completed
	      },

    ResendResponseTimer_Signal =
	receive
	    {siptimer, ResendResponseTimer_Ref1, "Testing testing"} ->
		{siptimer, ResendResponseTimer_Ref1, "Testing testing"}
	after 100 ->
		throw("test failed, the timer we set up never fired")
	end,

    autotest:mark(?LINE, "process_timer2/3 {resendresponse} - 1.1"),
    %% test normal case, the transaction should terminate
    {noreply, ResendResponseTimer_State_out, ?TIMEOUT} =
	handle_info(ResendResponseTimer_Signal, ResendResponseTimer_State),

    autotest:mark(?LINE, "process_timer2/3 {resendresponse} - 1.2"),
    %% cancel the revived timer
    siptimer:cancel_all_timers(ResendResponseTimer_State_out#state.timerlist),
    %% verify new state
    ResendResponseTimer_State = ResendResponseTimer_State_out#state{timerlist = ResendResponseTimer_Timerlist1},
    [{resendresponse}] = siptimer:test_get_appsignals(ResendResponseTimer_State_out#state.timerlist),

    test_verify_response_was_sent(100, "Testing", <<>>,
				  "process_timer2/3 {resendresponse}", 1, 3),

    autotest:mark(?LINE, "process_timer2/3 {resendresponse} - 2"),
    %% test not in sipstate 'completed', no change expected
    %% not really an error but we should not normally end up there either
    ResendResponseTimer_State2 = ResendResponseTimer_State#state{sipstate = proceeding},
    {noreply, ResendResponseTimer_State2, ?TIMEOUT} =
	handle_info(ResendResponseTimer_Signal, ResendResponseTimer_State2),

    autotest:mark(?LINE, "process_timer2/3 {resendresponse} - 3"),
    %% test with no response sent
    ResendResponseTimer_State3 = ResendResponseTimer_State#state{response = undefined},
    {noreply, ResendResponseTimer_State3, ?TIMEOUT} =
	handle_info(ResendResponseTimer_Signal, ResendResponseTimer_State3),


    %% process_timer2({resendresponse_timeout}, Timer, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_timer2/3 {resendresponse_timeout} - 0"),
    %% set up a timer and let it fire
    ResendResponseTimeout_Timerlist1 =
	siptimer:add_timer(1, "Testing testing",
			   {resendresponse_timeout},
			   siptimer:empty()
			  ),
    ResendResponseTimeout_State =
	#state{logtag    = "testing",
	       socket    = #sipsocket{proto = yxa_test},
	       timerlist = ResendResponseTimeout_Timerlist1,
	       request   = Test_OrigRequest,
	       response  = Test_Response,
	       sipstate  = completed
	      },

    ResendResponseTimeout_Signal =
	receive
	    {siptimer, ResendResponseTimeout_Ref1, "Testing testing"} ->
		{siptimer, ResendResponseTimeout_Ref1, "Testing testing"}
	after 100 ->
		throw("test failed, the timer we set up never fired")
	end,

    autotest:mark(?LINE, "process_timer2/3 {resendresponse_timeout} - 1.1"),
    %% test normal case, the transaction should terminate
    {stop, normal, ResendResponseTimeout_State_out} =
	handle_info(ResendResponseTimeout_Signal, ResendResponseTimeout_State),

    autotest:mark(?LINE, "process_timer2/3 {resendresponse_timeout} - 1.2"),
    %% verify new state
    terminated = ResendResponseTimeout_State_out#state.sipstate,
    ResendResponseTimeout_State =
	ResendResponseTimeout_State_out#state{
	  sipstate  = completed
	 },


    %% enter_sip_state(SipState, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "enter_sip_state/2 - 1"),
    %% enter same state, no change expected
    EnterSipState_State1 = #state{sipstate = trying},
    EnterSipState_State1 = enter_sip_state(trying, EnterSipState_State1),

    autotest:mark(?LINE, "enter_sip_state/2 - 2.1"),
    %% enter state 'completed' with non-INVITE transaction, should start Timer I
    EnterSipState_State2 = #state{sipstate = proceeding,
				  logtag = "testing",
				  request = Test_OrigRequest#request{method = "OPTIONS"},
				  timerlist = siptimer:empty()
				 },
    EnterSipState_State2_out = enter_sip_state(completed, EnterSipState_State2),

    autotest:mark(?LINE, "enter_sip_state/2 - 2.2"),
    %% verify new state
    completed = EnterSipState_State2_out#state.sipstate,
    EnterSipState_State2 = EnterSipState_State2_out#state{timerlist = siptimer:empty(),
							  sipstate = proceeding
							 },
    [{terminate_transaction}] = siptimer:test_get_appsignals(EnterSipState_State2_out#state.timerlist),


    %% make_response(Status, Reason, RBody, ExtraHeaders, ViaParameters, State)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "make_response/6 - 0"),
    MakeResponse_Header1 = Test_OrigRequest#request.header,
    MakeResponse_Header1_1 = keylist:set("To", ["sip:with-to-tag@example.org;tag=foo"], MakeResponse_Header1),
    MakeResponse_Request1 = Test_OrigRequest#request{header = MakeResponse_Header1_1},

    autotest:mark(?LINE, "make_response/6 - 1.1"),
    %% test that we use the To-tag from the request, if it has one
    MakeResponse_State1 = #state{request = MakeResponse_Request1,
				 socket  = #sipsocket{proto = yxa_test}
				},
    MakeResponse_Response1 = make_response(400, "Testing", <<>>, [], [], MakeResponse_State1),

    autotest:mark(?LINE, "make_response/6 - 1.2"),
    %% verify response created
    #response{status = 400,
	      reason = "Testing",
	      body   = <<>>
	     } = MakeResponse_Response1,
    ["sip:with-to-tag@example.org;tag=foo"] = keylist:fetch('to', MakeResponse_Response1#response.header),

    autotest:mark(?LINE, "make_response/6 - 2.1"),
    %% test that we don't add my_to_tag to a 100 Trying if it does not contain a To-tag
    MakeResponse_Header2 = Test_OrigRequest#request.header,
    MakeResponse_Header2_1 = keylist:set("To", ["sip:without-to-tag@example.org"], MakeResponse_Header2),
    MakeResponse_Request2 = Test_OrigRequest#request{header = MakeResponse_Header2_1},
    MakeResponse_State2 = #state{my_to_tag = "my_to_tag",
				 socket    = #sipsocket{proto = yxa_test},
				 request   = MakeResponse_Request2
				},
    MakeResponse_Response2 = make_response(100, "Testing", <<>>, [], [], MakeResponse_State2),
    autotest:mark(?LINE, "make_response/6 - 2.2"),
    %% verify response created
    #response{status = 100,
	      reason = "Testing",
	      body   = <<>>
	     } = MakeResponse_Response2,
    ["sip:without-to-tag@example.org"] = keylist:fetch('to', MakeResponse_Response2#response.header),

    autotest:mark(?LINE, "make_response/6 - 3.1"),
    %% test that we leave the To-tag in a 100 Trying if the request had a To-tag
    MakeResponse_State3 = MakeResponse_State1#state{my_to_tag = "my_to_tag"},
    MakeResponse_Response3 = make_response(100, "Testing", <<>>, [], [], MakeResponse_State3),
    autotest:mark(?LINE, "make_response/6 - 3.2"),
    %% verify response created
    #response{status = 100,
	      reason = "Testing",
	      body   = <<>>
	     } = MakeResponse_Response3,
    ["sip:with-to-tag@example.org;tag=foo"] = keylist:fetch('to', MakeResponse_Response3#response.header),

    autotest:mark(?LINE, "make_response/6 - 4.1"),
    %% test that we use my_to_tag from State if request does not have one
    MakeResponse_Header4 = Test_OrigRequest#request.header,
    MakeResponse_Header4_1 = keylist:set("To", ["sip:without-to-tag@example.org"], MakeResponse_Header4),
    MakeResponse_Request4 = Test_OrigRequest#request{header = MakeResponse_Header4_1},
    MakeResponse_State4 = #state{request   = MakeResponse_Request4,
				 my_to_tag = "my_to_tag",
				 socket    = #sipsocket{proto = yxa_test}
				},
    MakeResponse_Response4 = make_response(400, "Testing", <<>>, [], [], MakeResponse_State4),

    autotest:mark(?LINE, "make_response/6 - 4.2"),
    %% verify response created
    #response{status = 400,
	      reason = "Testing",
	      body   = <<>>
	     } = MakeResponse_Response4,
    ["<sip:without-to-tag@example.org>;tag=my_to_tag"] = keylist:fetch('to', MakeResponse_Response4#response.header),

    ok.


test_verify_response_was_sent(Status, Reason, Body, TestLabel, Major, Minor) ->
    autotest:mark(?LINE, "~s - ~p.~p", [TestLabel, Major, Minor]),
    %% check that a xxx response was generated
    receive
	{sipsocket_test, send, {yxa_test, "192.0.2.27", 6050}, SipMsg} ->
	    %% parse message that was 'sent'
	    autotest:mark(?LINE, "~s - ~p.~p", [TestLabel, Major, Minor + 1]),
	    Response = sippacket:parse(SipMsg, none),
	    true = is_record(Response, response),

	    autotest:mark(?LINE, "~s - ~p.~p", [TestLabel, Major, Minor + 2]),
	    %% check that it was the expected response that was resent
	    #response{status = Status,
		      reason = Reason,
		      body   = Body
		     } = Response,
	    ok
    after 0 ->
	    throw("test failed, SIP message 'sent' not found in process mailbox")
    end.
