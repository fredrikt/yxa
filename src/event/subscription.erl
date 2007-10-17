%%%-------------------------------------------------------------------
%%% File    : subscription.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Handle a SUBSCRIBE dialog (RFC3265).
%%%
%%%           These gen_servers are dialog controllers for dialogs
%%%           established by SUBSCRIBE requests sent to the
%%%           YXA application 'eventserver'.
%%%
%%%           Each time they receive a SUBSCRIBE (a request to refresh
%%%           the subscription) they ask the event package module what
%%%           to do.
%%%
%%%           Whenever there is a request to send out NOTIFYs on the
%%%           dialog, these processes ask the event package to supply
%%%           the body of the NOTIFY request etc.
%%%
%%%           Terminology :
%%%
%%%              Presentity: The 'presenter' of presence information.
%%%                          This is either {user, User} when the
%%%                          Request-URI of the SUBSCRIBE was an AOR
%%%                          of one of our users, or
%%%                          {address, AddressStr} when it was not.
%%%
%%%              Subscriber: The entity subscribing to someones
%%%                          presence. Currently, this is always one
%%%                          of our users username, since we only
%%%                          allow subscriptions from people we can
%%%                          authenticate.
%%%
%%%                          If we do bidirectional subscribe, the
%%%                          subscriber will be the presentity for the
%%%                          SUBSCRIBE we send out.
%%%
%%%    TODO :
%%%
%%%           * Implement NOTIFY rate limiting
%%%
%%% @since    27 Apr 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(subscription).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/5,

	 send_notify/1,

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
-include("siprecords.hrl").
-include("event.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%% @type state() = #state{}.
%%                 no description
-record(state, {subscriber,		%% string(), subscriber SIP username
		presentity,		%% {users, UserList} | {address, AddressStr}, presentity SIP username or address
		logtag,			%% string(), log prefix
		package_module,		%% atom(), event package module
		package_string,		%% string(), event package

		branch_base,		%% string(), Via branch parameter base
		branch_seq,		%% integer(), increased for every new request we send out (to generate unique branches)

		dialog,			%% dialog record()
		dialog_id,		%% {CallId, LocalTag, RemoteTag}, dialog identifier
		timerlist,		%% term(), SIP timer list

		expires,		%% integer(), util:timestamp() format of when subscription terminates
		last_accept,		%% list() of string(), Accept: header values from the last request we received
		last_event,		%% list() of string(), Event: header values from the last request we received
		my_allow,		%% list() of string(), list of methods allowed on this dialog
		my_contact,		%% string(), the Contact: header value we use

		notify_pids = [],	%% list() of pid(), active NOTIFY client transactions
		subscription_state,	%% active | pending | deactivated | terminated
		last_notify_content,	%% undefined | {Body, ExtraHeaders} - parameters of the last NOTIFY we sent
		subscr_num,		%% integer(), number of SUBSCRIBEs we have accepted
		notification_rate,	%% integer(), milliseconds required between NOTIFYs for the current event package

		bidirectional_sub,	%% true | false | started
		subscribe_pid,		%% undefined | pid(), the pid of our SUBSCRIBE client transaction if we do bidirectional
		subscribe_interval,	%% undefined | integer(), the Expires value we request for our outgoing SUBSCRIBEs
		subscribe_notifys,	%% undefined | integer(), the number of NOTIFYs we have received since the last SUBSCRIBE we sent
		subscribe_extrah,	%% undefined | list() of {Key, ValueL}
		event_pkg_state		%% term(), event package opaque data
	       }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DEFAULT_EXPIRE_TIME, 3600).		%% RFC3856 #6.4 (Subscription Duration)
-define(DEFAULT_MIN_EXPIRES, 60).		%% The shortest subscription Expires we accept
-define(DIALOG_GRACE_PERIOD, 60).		%% How long into the future, past the Expire time, we set our dialogs to expire

-define(DEFAULT_SUBSCRIBE_INTERVAL, 300).	%% Default Expires for our outgoing SUBSCRIBE, if bidirectional

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, PackageM, PackageS, Subscriber) ->
%%            Pid
%%
%%            Request    = #request{}
%%            YxaCtx     = #yxa_ctx{}
%%            PackageM   = atom() "event package module name"
%%            PackageS   = string() "event package"
%%            Subscriber = undefined | string() "subscribing user (authenticated)"
%%
%%            Pid    = pid()     |
%%            {error, Reason} |
%%            ignore
%%            Reason = need_auth | unacceptable
%%
%% @doc     Starts the server, if the event package in question says
%%          that the preconditions for a subscription are met.
%% @end
%%--------------------------------------------------------------------
start(#request{method = "SUBSCRIBE"} = Request, YxaCtx, PackageM, PackageS, Subscriber) ->
    SubscribeNum = 1,

    URIstr = sipurl:print(Request#request.uri),
    Presentity =
	case lookup:lookup_address_to_users(URIstr) of
	    [] ->
		{address, URIstr};
	    Users ->
		{users, Users}
	end,

    case PackageM:is_allowed_subscribe(PackageS, SubscribeNum, Request, YxaCtx,
				       Subscriber, Presentity, undefined) of
	{ok, SubscrState, Status, Reason, ExtraHeaders, Body, PkgState} when SubscrState == active orelse
									     SubscrState == pending,
									     is_integer(Status), is_list(Reason),
									     is_list(ExtraHeaders),
									     is_binary(Body) orelse is_list(Body) ->
	    case check_subscribe_expires(Request, YxaCtx#yxa_ctx.thandler) of
		{ok, Expires} ->
		    Now = util:timestamp(),

		    NotificationRate = PackageM:package_parameters(PackageS, notification_rate_limit),
		    PackageMethods = PackageM:package_parameters(PackageS, request_methods),
		    %% Should we send a subscribe in the other direction as well? Ask event package.
		    Bidirectional = bidirectional_subscribe(PackageM, PackageS, Request),
		    logger:log(debug, "Supscription: Event package ~p bidirectional subscribe for this peer : ~p",
			       [PackageS, Bidirectional]),
		    SubscribeInterval =
			case PackageM:package_parameters(PackageS, subscribe_interval) of
			    undefined -> ?DEFAULT_SUBSCRIBE_INTERVAL;
			    SInt1 when is_integer(SInt1) -> SInt1
			end,
		    Notify =
			case Bidirectional of
			    true -> ["NOTIFY"];
			    false -> []
			end,
		    %% Get list of methods to put in our responses Allow: header.
		    %% Make sure SUBSCRIBE and NOTIFY, if bidirectional, are included, once.
		    Allow =
			case PackageMethods of
			    undefined -> ["SUBSCRIBE"] ++ Notify;
			    _ when is_list(PackageMethods) ->
				MyList = ["SUBSCRIBE"] ++ Notify,
				PackageMethods1 = PackageMethods -- MyList,
				MyList ++ PackageMethods1
			end,

		    State1 = #state{subscriber    	= Subscriber,
				    presentity		= Presentity,
				    package_module	= PackageM,
				    package_string	= PackageS,
				    expires		= Now + Expires,
				    my_allow		= Allow,
				    last_accept		= keylist:fetch('accept', Request#request.header),
				    last_event		= keylist:fetch('event', Request#request.header),
				    subscription_state	= SubscrState,
				    notification_rate	= NotificationRate,
				    event_pkg_state	= PkgState,
				    timerlist		= siptimer:empty(),

				    bidirectional_sub   = Bidirectional,
				    subscribe_interval	= SubscribeInterval
				   },
		    gen_server:start(?MODULE, {Request, YxaCtx, State1, Status, Reason,
					       ExtraHeaders, Body},
				     []);
		_ ->
		    {error, unacceptable}
	    end;

	{error, need_auth} ->
	    {error, need_auth};

	{siperror, Status, Reason, ExtraHeaders} ->
	    Server = get_useragent_or_server("Server"),
	    THandler = YxaCtx#yxa_ctx.thandler,
	    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders ++ Server),
	    ignore
    end.

%%--------------------------------------------------------------------
%% @spec    (Pid) -> term() "result of gen_server:cast/2"
%%
%%            Pid = pid()
%%
%% @doc     Request that the subscription dialog controller Pid sends
%%          out a NOTIFY. No NOTIFY will be sent out if it matches
%%          the last NOTIFY sent on that subscription.
%% @end
%%--------------------------------------------------------------------
send_notify(Pid) ->
    gen_server:cast(Pid, {notify, undefined}).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({Request, YxaCtx, State1, Status, Reason, ExtraHeaders,
%%          Body}) ->
%%            {ok, State}    |
%%            ignore         |
%%            {stop, Reason}
%%
%%            Request      = #request{} "SUBSCRIBE request"
%%            YxaCtx       = #yxa_ctx{}
%%            State1       = #state{} "state in"
%%            Status       = integer() "(200 or 202), SIP status code to send in response to this SUBSCRIBE"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = [{Key, Value}] "extra headers to include in response"
%%            Body         = binary() | list() "body of response"
%%
%% @doc     Initiates the server when eventserver has received a
%%          SUBSCRIBE request.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({Request, YxaCtx, State1, Status, Reason, ExtraHeaders, Body}) ->
    #yxa_ctx{app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,

    BranchBase = siprequest:generate_branch(),
    BranchSeq = 1,

    {ok, ToTag} = transactionlayer:get_my_to_tag(THandler),
    Contact = generate_contact_str(),
    {ok, Dialog} = local:create_dialog_state_uas(?MODULE, Request, ToTag, Contact),
    DialogId = {Dialog#dialog.callid, Dialog#dialog.local_tag, Dialog#dialog.remote_tag},

    ExpSeconds = State1#state.expires - util:timestamp(),
    NewTimerL1 = siptimer:add_timer(ExpSeconds * 1000, "Subscription timeout",
				    subscription_timeout, State1#state.timerlist),

    SubscribeExtraHeaders =
	case State1#state.bidirectional_sub of
	    true ->
		%% Static extra headers to include in SUBSCRIBEs we send
		PackageS = State1#state.package_string,
		PackageM = State1#state.package_module,
		AcceptL =
		    case PackageM:package_parameters(PackageS, subscribe_accept_content_types) of
			undefined ->
			    logger:log(normal, "Subscription: Warning: Event package module ~p does not specify "
				       "the package_parameters 'subscribe_accept_content_types', sending SUBSCRIBE "
				       "without Accept: header", [PackageM]),
			    [];
			AcceptL1 when is_list(AcceptL1) ->
			    [{"Accept", AcceptL1}]
		    end,
		AllowEvents =
		    case PackageM:package_parameters(PackageS, allow_events) of
			undefined ->
			    [PackageS];
			AllowEvents1 when is_list(AllowEvents1) ->
			    AllowEvents1
		    end,

		UserAgent = get_useragent_or_server("User-Agent"),
		{ok, MaxForwards} = yxa_config:get_env(default_max_forwards),
		[{"Contact",		[Contact]},
		 {"Event",		[PackageS]},
		 {"Max-Forwards",	[integer_to_list(MaxForwards)]},
		 {"Allow",		State1#state.my_allow},
		 {"Allow-Events",	AllowEvents}
		] ++ AcceptL ++ UserAgent;
	    false ->
		[]
	end,

    State =
	State1#state{logtag		= YxaCtx#yxa_ctx.app_logtag,
		     branch_base	= BranchBase,
		     branch_seq		= BranchSeq,
		     my_contact		= Contact,
		     dialog		= Dialog,
		     dialog_id		= DialogId,
		     subscr_num		= 1,
		     subscribe_extrah	= SubscribeExtraHeaders,
		     timerlist		= NewTimerL1
		  },

    ok = sipdialog:register_dialog_controller(Dialog, self(), State#state.expires + ?DIALOG_GRACE_PERIOD),

    case State#state.presentity of
	{users, UserList} ->
	    [ok = notifylist:add({user, E}, State#state.package_string, self()) || E <- UserList];
	{address, AddressStr} ->
	    ok = notifylist:add({address, AddressStr}, State#state.package_string, self())
    end,

    logger:log(debug, "~s: Subscription: Answering SUBSCRIBE with ~p (from user: ~p, to: ~p)",
	       [LogTag, Status, State#state.subscriber, State#state.presentity]),

    ExtraHeaders2 = [{"Expires", [integer_to_list(ExpSeconds)]} | ExtraHeaders],
    ExtraHeaders3 = headers_for_response("SUBSCRIBE", Status, ExtraHeaders2, State),
    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders3, Body),

    NewTimerL2 = siptimer:add_timer(1, "NOTIFY after SUBSCRIBE", send_notify, State#state.timerlist),

    {ok, State#state{timerlist = NewTimerL2}}.


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
handle_call(Msg, _From, State) ->
    logger:log(error, "Subscription: Received unknown gen_server call : ~p", [Msg]),
    {noreply, State}.


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
%% @spec    ({notify, Source}, State) ->
%%            {noreply, State} |
%%            {stop, Reason, State}
%%
%% @doc     Request to send a NOTIFY on our subscription. Do so
%%          (unless it is supressed as a duplicate) and then check if
%%          our Subscription-State is 'terminated'.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({notify, Source}, State) ->
    case Source == State#state.dialog_id of
	true ->
	    logger:log(debug, "Subscription: Not generating NOTIFY for event caused by my own dialog"),
	    {noreply, State};
	false ->
	    process_timer_signal(send_notify, undefined, State)
    end;

%%--------------------------------------------------------------------
%% @spec    ({terminate, Mode}, State) -> {stop, Reason, State}
%%
%%            Mode = shutdown | graceful | atom()
%%
%% @doc     Terminate the subscription since the eventserver is
%%          shutting down.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({terminate, Mode}, State) when is_atom(Mode) ->
    NewState =
	case State#state.subscription_state of
	    S when S == active; S == pending ->
		logger:log(debug, "Subscription: Deactivating subscription"),
		prepare_and_send_notify(State#state{subscription_state = deactivated});
	    _ ->
		State
	end,
    {stop, normal, NewState};

handle_cast(Msg, State) ->
    logger:log(error, "Subscription: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State}.


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
%% @spec    ({siptimer, TRef, TDesc}, State) ->
%%            {noreply, NewState}      |
%%            {stop, Reason, NewState}
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
    case siptimer:get_timer(TRef, State#state.timerlist) of
	none ->
	    logger:log(error, "~s: Subscription: Unknown timer (~p:~p) fired! Ignoring. LIST ~n~p",
		       [LogTag, TRef, TDesc, State#state.timerlist]),
	    {noreply, State};
	Timer ->
	    process_timer(Timer, State)
    end;

%%--------------------------------------------------------------------
%% @spec    ({branch_result, FromPid, Branch, SipState, Response},
%%          State) ->
%%            {noreply, State} |
%%            {stop, Reason, State}
%%
%% @doc     A NOTIFY client transaction we started resulted in a
%%          response. Check if that response was a (received) 481 and
%%          terminate if it was.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({branch_result, FromPid, _Branch, _SipState, Response}, #state{subscribe_pid = FromPid} = State) ->
    Status = case Response of
		 #response{status = Status1} -> Status1;
		 {Status1, _} -> Status1
	     end,

    logger:log(debug, "Subscription: SUBSCRIBE we sent resulted in ~p response", [Status]),

    if
	Status >= 200, Status =< 299 ->
	    %% Get the Expire time the server granted us
	    Expires = get_subscribe_2xx_expires(Response#response.header, ?DEFAULT_SUBSCRIBE_INTERVAL),
	    Refresh = get_subscribe_refresh_timeout(Expires),

	    NewTL = siptimer:add_timer(Refresh, "Refresh outgoing SUBSCRIBE", subscribe_refresh, State#state.timerlist),
	    NewState = State#state{timerlist = NewTL},
	    {noreply, NewState};

	Status >= 400, Status =< 499 ->
	    %% turn bidirectional SUBSCRIBE off if we receive a 4xx response

	    logger:log(normal, "Subscription: Disabling bidirectional SUBSCRIBE after receiving a 4xx response"),
	    NewTL = siptimer:cancel_timers_with_appsignal(subscribe_refresh, State#state.timerlist),
	    NewState =
		State#state{timerlist         = NewTL,
			    bidirectional_sub = false
			   },
	    {noreply, NewState};

	Status >= 500, Status =< 599 ->
	    %% Check for a Retry-After header
	    Default = 60,
	    Refresh =
		case keylist:fetch('retry-after', Response#response.header) of
		    [RAStr] ->
			try list_to_integer(RAStr) of
			    RA when is_integer(RA) -> RA
			catch
			    _: _ -> Default
			end;
		    _ ->
			Default
		end,

	    logger:log(normal, "Subscription: Will retry bidirectional SUBSCRIBE after ~p seconds "
		       "(received a 5xx response)", [Refresh]),

	    NewTL = siptimer:add_timer(Refresh * 1000, "Refresh outgoing SUBSCRIBE", subscribe_refresh,
				       State#state.timerlist),
	    NewState = State#state{timerlist = NewTL},
	    {noreply, NewState};

	true ->
	    {noreply, State}
    end;

handle_info({branch_result, FromPid, Branch, SipState, Response}, State) ->
    Status =
	case Response of
	    #response{status = Status1} -> Status1;
	    {Status1, _Reason} -> Status1
	end,
    case lists:member(FromPid, State#state.notify_pids) of
	true ->
	    if
		SipState == completed, is_record(Response, response), Status >= 200, Status =< 299 ->
		    case State#state.bidirectional_sub of
			true ->
			    NewState = send_bidirectional_subscribe(State),
			    {noreply, NewState};
			_ ->
			    %% false or started
			    {noreply, State}
		    end;
		(SipState == completed orelse SipState == terminated), Status >= 300, Status =< 699 ->
		    %% If a NOTIFY results in a 481, we MUST terminate the subscription.
		    %% If it results in a non-2xx response we MUST terminate the subscription if it
		    %% was installed using a soft-state mechanism (SUBSCRIBE), and we SHOULD terminate
		    %% the subscription if it wasn't. RFC3265 #3.2.2 (Notifier NOTIFY Behavior)
		    [C] = contact:parse([(State#state.dialog)#dialog.remote_target]),
		    logger:log(normal, "Subscription: NOTIFY ~s resulted in ~p response, terminating "
			       "the subscription immediately.", [C#contact.urlstr, Status]),
		    {stop, normal, State};
		true ->
		    %% SipState most probably wasn't completed/terminated
		    {noreply, State}
	    end;
	false ->
	    logger:log(debug, "Subscription: Ignoring branch_result from ~p (~p) : ~p",
		       [FromPid, Branch, Response]),
	    {noreply, State}
    end;

%%--------------------------------------------------------------------
%% @spec
%%    ({clienttransaction_terminating, FromPid, Branch}, State) ->
%%            {noreply, State, Timeout}
%%
%% @doc     A NOTIFY client transaction has terminated. Remove it from
%%          our list of active NOTIFY client transactions.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({clienttransaction_terminating, FromPid, Branch}, State) ->
    PidList =
	case lists:member(FromPid, State#state.notify_pids) of
	    true ->
		%% remove FromPid from list of active NOTIFY client transactions
		State#state.notify_pids -- [FromPid];
	    false ->
		logger:log(debug, "Subscription: Ignoring clienttransaction_terminating from ~p (~p)",
			   [FromPid, Branch]),
		State#state.notify_pids
	end,
    {noreply, State#state{notify_pids = PidList}};

%%--------------------------------------------------------------------
%% @spec    ({servertransaction_terminating, ...}, State) ->
%%            {noreply, State, Timeout}
%%
%% @doc     A SUBSCRIBE server transaction (probably) has terminated,
%%          just ignore.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({servertransaction_terminating, _FromPid}, State) ->
    {noreply, State};

%%--------------------------------------------------------------------
%% @spec    ({new_request, FromPid, Ref, NewRequest, YxaCtx}, ...) ->
%%            {noreply, State, Timeout}      |
%%            {stop, normal, State}
%%
%%            FromPid    = pid() "transaction layer"
%%            Ref        = ref() "unique reference to ack this message with (signal back to transaction layer)"
%%            NewRequest = #request{}
%%            YxaCtx     = #yxa_ctx{}
%%
%% @doc     Handle incoming requests on our dialog.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({new_request, FromPid, Ref, NewRequest, YxaCtx}, State) when is_record(NewRequest, request),
									 is_record(YxaCtx, yxa_ctx) ->
    THandler = YxaCtx#yxa_ctx.thandler,
    transactionlayer:adopt_server_transaction_handler(THandler),
    FromPid ! {ok, self(), Ref},

    NewState_Res =
	case sipdialog:update_dialog_recv_request(NewRequest, State#state.dialog) of
	    {error, old_cseq} ->
		EH = headers_for_response(NewRequest#request.method, 500, [], State),
		transactionlayer:send_response_handler(THandler, 500, "CSeq not higher than last requests", EH),
		{noreply, State};
	    {ok, NewDialog} ->
		received_request(State#state{dialog = NewDialog}, THandler, NewRequest, YxaCtx)
	end,

    case is_record(NewState_Res, state) of
	true ->
	    {noreply, NewState_Res};
	false ->
	    NewState_Res
    end;

%%--------------------------------------------------------------------
%% @spec    ({dialog_expired, ...}, State) -> {stop, normal, State}
%%
%% @doc     This should never happen, but the transaction layer just
%%          signaled us that this dialog has expired. Log an error
%%          message and exit. This almost certainly means that a bug
%%          was encountered, since we should otherwise have noticed
%%          that the subscription expired and exited ourselves.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({dialog_expired, _DialogId}, State) ->
    logger:log(error, "Subscription: Dialog expired, should not happen."),
    {stop, normal, State};

handle_info(Unknown, State) ->
    logger:log(error, "Subscription: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     The server is being shut down. Remove ourselves from the
%%          ets table with subscriptions for presentitys before we
%%          terminate.
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) when is_record(State, state) ->
    case Reason of
        normal ->
	    logger:log(debug, "Subscription: terminating normally");
        shutdown ->
	    logger:log(debug, "Subscription: shutting down");
        _ ->
	    logger:log(error, "Subscription: terminating : ~p", [Reason])
    end,

    case State#state.presentity of
	{users, UserList} ->
	    [ok = notifylist:delete({user, E}, State#state.package_string, self()) || E <- UserList];
	{address, AddressStr} ->
	    ok = notifylist:delete({address, AddressStr}, State#state.package_string, self())
    end,

    Reason;
terminate(Reason, State) ->
    logger:log(error, "Subscription: Terminating (reason: ~p) with invalid State", [Reason]),
    logger:log(debug, "Subscription: The invalid State : ~p", [State]),
    error.


%%--------------------------------------------------------------------
%% @spec    (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    (State, THandler, NewRequest, YxaCtx) ->
%%            NewState
%%
%%            State      = #state{}
%%            THandler   = term() "server transaction handle"
%%            NewRequest = #request{}
%%            YxaCtx     = #yxa_ctx{}
%%
%%            NewState = #state{} |
%%            {stop, Reason, NewState}
%%
%% @doc     We have received a request on our dialog. Figure out what
%%          we should respond to it with and tell the THandler to
%%          send that response. If it is a SUBSCRIBE, we handle it
%%          here. If it is not a SUBSCRIBE, we check our my_allow
%%          list to see if the event package says it has implemented
%%          the particular method, and if so we invoke the event
%%          packages request/7 method on the new request.
%% @end
%%--------------------------------------------------------------------
%%
%% SUBSCRIBE
%%
received_request(State, THandler, #request{method = "SUBSCRIBE"} = NewRequest, YxaCtx) ->
    case check_subscribe_expires(NewRequest, THandler) of
	{ok, Expires} ->
	    Now = util:timestamp(),

	    #state{subscriber      = Subscriber,
		   presentity      = Presentity,
		   package_module  = PackageM,
		   package_string  = PackageS,
		   subscr_num      = SubNum,
		   event_pkg_state = PkgState,
		   logtag          = LogTag
		  } = State,

	    logger:log(debug, "Subscription: Checking with event package ~p module ~p if "
		       "~p is allowed to re-subscribe to presentity ~p",
		       [PackageS, PackageM, Subscriber, Presentity]),
	    case PackageM:is_allowed_subscribe(PackageS, SubNum + 1, NewRequest, YxaCtx,
					       LogTag, THandler, Subscriber, Presentity,
					       PkgState) of
		{ok, SubState1, Status, Reason, ExtraHeaders, Body, NewPkgState}
		when SubState1 == active orelse SubState1 == pending orelse SubState1 == terminated,
		     is_integer(Status), is_list(Reason), is_list(ExtraHeaders) ->
		    case Expires of
			0 ->
			    logger:log(normal, "~s: Subscription: Terminating subscription "
				       "(from user: ~p, to: ~p) on UAC's request",
				       [LogTag, Subscriber, Presentity]);
			_ ->
			    logger:log(normal, "~s: Subscription: Renewing subscription "
				       "(from user: ~p, to: ~p) +~p",
				       [LogTag, Subscriber, Presentity, Expires])
		    end,

		    ok = sipdialog:set_dialog_expires(State#state.dialog, Expires + ?DIALOG_GRACE_PERIOD),
		    NewTimerL1 = siptimer:cancel_timers_with_appsignal(subscription_timeout, State#state.timerlist),
		    NewTimerL2 = siptimer:add_timer(Expires * 1000, "Subscription timeout",
						    subscription_timeout, NewTimerL1),

		    %% Don't let package module override clients request to terminate subscription
		    {SetExpire, NewSubState} =
			case Expires of
			    0 -> {Now, terminated};
			    _ ->
				%% Set Expires Now if package module returned state 'terminated'
				case SubState1 of
				    terminated -> {Now, terminated};
				    _ -> {Now + Expires, SubState1}
				end
			end,

		    ExtraHeaders1 = [{"Expires", [integer_to_list(Expires)]} | ExtraHeaders],
		    ExtraHeaders2 = headers_for_response("SUBSCRIBE", Status, ExtraHeaders1, State),
		    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders2, Body),

		    SetLastAccept =
			case keylist:fetch('accept', NewRequest#request.header) of
			    [] ->
				%% Windows Messenger (RTC/1.3) workaround. They don't include the Accept header
				%% in SUBSCRIBEs except the very first one
				logger:log(debug, "Subscription: Warning: No Accept: in SUBSCRIBE, using previous value (~p)",
					   [State#state.last_accept]),
				State#state.last_accept;
			    AcceptV ->
				AcceptV
			end,

		    NewState1 =
			State#state{last_accept        = SetLastAccept,
				    last_event         = keylist:fetch('event', NewRequest#request.header),
				    expires            = SetExpire,
				    subscr_num         = SubNum + 1,
				    subscription_state = NewSubState,
				    event_pkg_state    = NewPkgState,
				    timerlist	       = NewTimerL2
				   },

		    %% RFC3265 #3.1.4.4 says that each subscription refresh will generate a NOTIFY,
		    %% and that Expires: 0 is a request for immediate fetch of state.
		    %% process_timer_signal/3 will return {stop, _, _} if
		    %% NewState1#state.subscription_state is 'terminated'
		    case process_timer_signal(send_notify, undefined, NewState1) of
			{noreply, NewState2} when is_record(NewState2, state) ->
			    NewState2;
			Res ->
			    Res
		    end;

		{error, need_auth} ->
		    erlang:error("authorization of subsequent SUBSCRIBEs not implemented yet");

		{siperror, Status, Reason, ExtraHeaders} ->
		    ExtraHeaders1 = headers_for_response("SUBSCRIBE", Status, ExtraHeaders, State),
		    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders1),
		    State
	    end;
	error ->
	    State
    end;

%%
%% Anything but SUBSCRIBE
%%
received_request(State, THandler, NewRequest, YxaCtx) ->
    %% Count NOTIFYs we receive when bidirectional
    State1 =
	case State#state.bidirectional_sub == started andalso
	    NewRequest#request.method == "NOFITY" of
	    true ->
		N = State#state.subscribe_notifys + 1,
		State#state{subscribe_notifys = N};
	    false ->
		State
	end,

    case lists:member(NewRequest#request.method, State1#state.my_allow) of
	true ->
	    %% Method is implemented by the event package
	    #state{package_module = PackageM,
		   package_string = PackageS,
		   logtag         = LogTag,
		   subscriber     = Subscriber,
		   presentity     = Presentity,
		   dialog_id      = DialogId
		  } = State1,
	    Ctx =
		#event_ctx{sipuser    = Subscriber,
			   presentity = Presentity,
			   dialog_id  = DialogId
			  },
	    YxaCtx1 =
		YxaCtx#yxa_ctx{app_logtag   = LogTag,
			       thandler = THandler
			      },
	    case PackageM:request(PackageS, NewRequest, YxaCtx1, Ctx) of
		{error, need_auth} ->
		    logger:log(error, "Subscription: Authorization of in-dialog requests not implemented "
			       "(subscriber: ~p), answering '500 Server Internal Error'", [Subscriber]),
		    ExtraHeaders = headers_for_response("SUBSCRIBE", 500, [], State),
		    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error", ExtraHeaders);
		PackageM_Res ->
		    logger:log(debug, "Subscription: Extra debug: Result of ~p:request/4 was : ~p",
			       [PackageM, PackageM_Res])
	    end,
	    State1;
	false ->
	    %% Answer anything not implemented by us or the event package with '501 Not Implemented'.
	    ExtraHeaders = headers_for_response(NewRequest#request.method, 501, [], State),
	    transactionlayer:send_response_handler(THandler, 501, "Not Implemented (inside dialog)",
						   ExtraHeaders),

	    State1
    end.




%%--------------------------------------------------------------------
%% @spec    (Timer, State) ->
%%            {noreply, NewState}      |
%%            {stop, Reason, NewState}
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
    process_timer_signal(Signal, Timer, State).

%%--------------------------------------------------------------------
%% @spec    (subscription_timeout, Timer, State) ->
%%            {stop, Reason, NewState}
%%
%%            Timer = undefined | #siptimer{}
%%            State = #state{}
%%
%% @doc     The subscription has timed out, send a final NOTIFY and
%%          then terminate.
%% @end
%%--------------------------------------------------------------------
process_timer_signal(subscription_timeout, Timer, State) when is_record(State, state) ->
    process_timer_signal(send_notify, Timer, State);

%%--------------------------------------------------------------------
%% @spec    (send_notify, Timer, State) ->
%%            {noreply, NewState}      |
%%            {stop, Reason, NewState}
%%
%%            Timer = undefined | #siptimer{}
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     We should send a NOTIFY. After that, we check if our
%%          subscription state is 'terminated' and if it is we exit.
%% @end
%%--------------------------------------------------------------------
process_timer_signal(send_notify, _Timer, State) when is_record(State, state) ->
    %% Set last_notify_content to 'undefined' to make sure we don't supress sending of this NOTIFY
    %% just because it matches the last one we sent
    NewState = prepare_and_send_notify(State#state{last_notify_content = undefined}),
    case NewState#state.subscription_state of
	terminated ->
	    logger:log(normal, "~s: Subscription for event package ~p expired (subscriber: ~p, presentity ~p)",
		       [State#state.logtag, State#state.package_string, State#state.subscriber,
			State#state.presentity]),
	    logger:log(debug, "Subscription: Exiting after sending NOTIFY when Subscription-State is 'terminated'"),
	    %% XXX should we hang around until the NOTIFY client transaction finishes?
	    {stop, normal, NewState};
	_ ->
	    {noreply, NewState}
    end;

process_timer_signal(subscribe_refresh, _Timer, State) when is_record(State, state) ->
    case State#state.subscribe_notifys of
	0 ->
	    logger:log(normal, "~s: Letting bidirectinal SUBSCRIBE expire since I haven't received any NOTIFYs "
		       "since my last SUBSCRIBE", [State#state.logtag]),
	    {noreply, State};
	_ ->
	    NewState = send_bidirectional_subscribe(State),
	    {noreply, NewState}
    end;

process_timer_signal(Unknown, _Timer, State) when is_record(State, state) ->
    logger:log(error, "~s: Subscription: Unknown timer signal : ~p", [State#state.logtag, Unknown]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec    (State) ->
%%            NewState
%%
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Create a NOTIFY request for this event package, and send
%%          it on our dialog unless it happens to exactly match the
%%          last NOTIFY we sent.
%% @end
%%--------------------------------------------------------------------
prepare_and_send_notify(State) when is_record(State, state) ->
    #state{package_module  = PackageM,
	   package_string  = PackageS,
	   last_accept     = LastAccept,
	   presentity      = Presentity,
	   event_pkg_state = PkgState
	  } = State,
    case PackageM:notify_content(PackageS, Presentity, LastAccept, PkgState) of
	{error, Reason} ->
	    logger:log(error, "Subscription: Failed creating NOTIFY for presentity ~p : ~p",
		       [State#state.presentity, Reason]),
	    %% XXX should we terminate the subscription? Terminate if this was the notify we were
	    %% supposed to send as a result of the initial SUBSCRIBE? Subsequent SUBSCRIBE?
	    State;
	{ok, Body, ExtraHeaders, NewPkgState} when is_list(Body); is_binary(Body), is_list(ExtraHeaders) ->
	    CompareWith = {Body, ExtraHeaders, State#state.subscription_state},
	    case (CompareWith == State#state.last_notify_content) of
		true ->
		    logger:log(debug, "Subscription: Supressed sending of NOTIFY identical (essentially) "
			       "to the last one we sent."),
		    State;
		false ->
		    NewState1 = send_notify_request(State, Body, ExtraHeaders),
		    NewState1#state{last_notify_content = CompareWith,
				    event_pkg_state     = NewPkgState
				   }
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (State, Body, ExtraHeaders) ->
%%            NewState
%%
%%            State        = #state{}
%%            Body         = list() | binary()
%%            ExtraHeaders = [{Key, ValueL}]
%%
%%            NewState = #state{}
%%
%% @doc     Part of prepare_and_send_notify/1.
%% @end
%%--------------------------------------------------------------------
send_notify_request(State, Body, ExtraHeaders) when is_list(Body); is_binary(Body), is_list(ExtraHeaders) ->
    Now = util:timestamp(),

    {NewSubState, SubscriptionState} =
	case State#state.subscription_state of
	    deactivated ->
		{terminated, "terminated;reason=deactivated"};
	    _ ->
		case (State#state.expires =< Now) of
		    true ->
			%% "A subscription is destroyed when a notifier sends a NOTIFY request
			%%  with a "Subscription-State" of "terminated"." RFC3265 #3.3.4
			{terminated, "terminated;reason=timeout"};
		    false ->
			SubStr = lists:concat([State#state.subscription_state, ";expires=", State#state.expires - Now]),
			{State#state.subscription_state, lists:flatten(SubStr)}
		end
	end,

    ExtraHeaders1 =
	case lists:keysearch("Contact", 1, ExtraHeaders) of
	    {value, _} ->
		ExtraHeaders;
	    false ->
		[{"Contact", [State#state.my_contact]} | ExtraHeaders]
	end,

    ExtraHeaders2 = [{"Subscription-State", [SubscriptionState]} | ExtraHeaders1],

    %% "NOTIFY requests are matched to such SUBSCRIBE requests if they ...
    %%  and the same "Event" header field" RFC3265 #3.3.4
    ExtraHeaders3 =
	case lists:keysearch("Event", 1, ExtraHeaders) of
	    {value, _} ->
		ExtraHeaders2;
	    false ->
		[{"Event", State#state.last_event} | ExtraHeaders2]
	end,

    Timeout = 5,
    {ok, NotifyPid, NewState} = start_client_transaction("NOTIFY", ExtraHeaders3, Body, Timeout, State),

    PidList = [NotifyPid | State#state.notify_pids],
    NewState#state{notify_pids        = PidList,
		   subscription_state = NewSubState
		  }.

%% Returns : true | false
bidirectional_subscribe(PackageM, PackageS, Request) when is_atom(PackageM), is_list(PackageS), is_record(Request, request) ->
    MyHostname = siprequest:myhostname(),
    case (Request#request.uri)#sipurl.host == MyHostname of
	true ->
	    logger:log(normal, "Subscription: NOT doing bidirectional subscribe when Request-URI contains my hostname"),
	    false;
	false ->
	    case PackageM:subscription_behaviour(PackageS, bidirectional_subscribe, Request) of
		undefined ->
		    false;
		Res when is_boolean(Res) ->
		    Res
	    end
    end.

%% Returns : NewState
send_bidirectional_subscribe(State) ->
    ExpiresH = {"Expires", [integer_to_list(State#state.subscribe_interval)]},
    ExtraHeaders = [ExpiresH | State#state.subscribe_extrah],
    Body = <<>>,
    Timeout = 5,
    {ok, Pid, NewState} = start_client_transaction("SUBSCRIBE", ExtraHeaders, Body, Timeout, State),
    NewState#state{subscribe_pid     = Pid,
		   bidirectional_sub = started,
		   subscribe_notifys = 0
		  }.


%% Returns : {ok, Pid, NewState}
start_client_transaction(Method, ExtraHeaders, Body, Timeout, State) ->
    {ok, Request, NewDialog, Dst} =
	sipdialog:generate_new_request(Method, ExtraHeaders, Body, State#state.dialog),

    BranchSeq = State#state.branch_seq,
    Branch = lists:concat([State#state.branch_base, "-UAC-", BranchSeq]),

    [FirstDst | _] = Dst,      %% XXX don't just throw away the other destinations!
    Pid = transactionlayer:start_client_transaction(Request, FirstDst, Branch, Timeout, self()),

    NewState =
	State#state{dialog     = NewDialog,
		    branch_seq = BranchSeq + 1
		   },

    {ok, Pid, NewState}.


%%--------------------------------------------------------------------
%% @spec    () ->
%%            Contact
%%
%%            Contact = string() "SIP URL inside ``<'' and ``>''"
%%
%% @doc     Generate a Contact header value for our requests. The
%%          registration as a dialog controller will get all requests
%%          on the dialog sent to us, so the user part of the contact
%%          is not important. We use the Erlang pid, without ``<''
%%          and ``>''.
%% @end
%%--------------------------------------------------------------------
generate_contact_str() ->
    generate_contact_str("sip").

generate_contact_str(Proto) ->
    PidStr = pid_to_list(self()),
    User = lists:reverse(
	     lists:foldl(fun($<, Acc) -> Acc;
			    ($>, Acc) -> Acc;
			    (C, Acc) ->
				 [C | Acc]
			 end, [], PidStr)
	     ),
    MyPort =
	case Proto of
	    "sips" -> sipsocket:get_listenport(tls);
	    _ ->      sipsocket:get_listenport(udp)
	end,

    %% Add {port, MyPort} if we are not listening on the standard port for Proto, for interoperability
    PortL =
	case sipsocket:default_port(Proto, none) of
	    MyPort -> [];
	    _ ->      [{port, MyPort}]
	end,

    Params = [{proto, Proto},
	      {user, User},
	      {host, siprequest:myhostname()}
	     ] ++ PortL,

    URL = sipurl:new(Params),
    "<" ++ sipurl:print(URL) ++ ">".

%%--------------------------------------------------------------------
%% @spec    (Request, THandler) ->
%%            {ok, Expires} |
%%            error
%%
%%            Request  = #request{}
%%            THandler = term() "server transaction handle"
%%
%%            Expires = integer()
%%
%% @doc     Extract and check that the 'expires' value of this
%%          SUBSCRIBE request is acceptable.
%% @end
%%--------------------------------------------------------------------
check_subscribe_expires(Request, THandler) ->
    %% RFC3265 #3.1.6.1 (Initial SUBSCRIBE Transaction Processing)
    MinExpires = lists:min([?DEFAULT_MIN_EXPIRES, 3600]),

    case keylist:fetch('expires', Request#request.header) of
	[] ->
	    {ok, ?DEFAULT_EXPIRE_TIME};
	[ExpiresStr] ->
	    try list_to_integer(ExpiresStr) of
		Res when Res > 0, Res < MinExpires ->
		    ExtraHeaders = [{"Min-Expires", [integer_to_list(MinExpires)]}],
		    transactionlayer:send_response_handler(THandler, 423, "Interval too small", ExtraHeaders),
		    error;
		ExpiresInt when is_integer(ExpiresInt) ->
		    {ok, ExpiresInt}
	    catch
		_ : _ ->
		    ExtraHeaders = [{"Warning", ["Bad Expires header value"]}],
		    transactionlayer:send_response_handler(THandler, 400, "Bad Request", ExtraHeaders),
		    error
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (Method, Status, ExtraHeaders, State) ->
%%            {ok, Expires} |
%%            error
%%
%%            Method       = string() "SIP method"
%%            Status       = integer() "SIP status code"
%%            ExtraHeaders = [{Key, ValueL}]
%%            State        = #state{}
%%
%%            Expires = integer()
%%
%% @doc     Extract and check that the 'expires' value of this
%%          SUBSCRIBE request is acceptable.
%% @end
%%--------------------------------------------------------------------
headers_for_response("SUBSCRIBE", Status, ExtraHeaders, State) when is_integer(Status), is_list(ExtraHeaders),
								    is_record(State, state) ->
    EH1 =
	case lists:keysearch("Allow", 1, ExtraHeaders) of
	    {value, _} ->
		ExtraHeaders;
	    false ->
		[{"Allow", State#state.my_allow} | ExtraHeaders]
	end,
    EH2 =
	case Status >= 200 andalso Status =< 299 of
	    true ->
		case lists:keysearch("Expires", 1, EH1) of
		    {value, _} ->
			ok;
		    false ->
			logger:log(error, "Subscription: 2xx response to SUBSCRIBE MUST contain an Expires header : ~p",
				  [ExtraHeaders]),
			error
		end,

		case lists:keysearch("Contact", 1, EH1) of
		    {value, _} ->
			EH1;
		    false ->
			[{"Contact", [State#state.my_contact]} | EH1]
		end;
	    false ->
		EH1
	end,
    EH3 = EH2 ++ get_useragent_or_server("Server"),
    EH3;
headers_for_response(_Method, 501, ExtraHeaders, State) when is_list(ExtraHeaders), is_record(State, state) ->
    EH1 = ExtraHeaders ++ get_useragent_or_server("Server"),
    case lists:keysearch("Allow", 1, ExtraHeaders) of
	{value, _} ->
	    EH1;
	false ->
	    [{"Allow", State#state.my_allow} | EH1]
    end;
headers_for_response(_Method, _Status, ExtraHeaders, _State) ->
    ExtraHeaders.

%% Descrip.: Get Expires value from 2xx response to SUBSCRIBE we sent
%% Returns : Expires = integer()
get_subscribe_2xx_expires(Header, Default) ->
    case keylist:fetch('expires', Header) of
	[] ->
	    logger:log(debug, "Subscription: Warning: 2xx response to SUBSCRIBE did not contain "
		       "an Expires header, assuming we got what we asked for"),
	    Default;
	[ExpiresStr] ->
	    try list_to_integer(ExpiresStr) of
		Res when is_integer(Res) -> Res
	    catch
		_ : _ ->
		    logger:log(normal, "Subscription: Invalid Expires: header in 2xx response "
			       "to SUBSCRIBE : ~p", [ExpiresStr]),
		    Default
	    end;
	Invalid ->
	    logger:log(normal, "Subscription: Invalid (or more than one) Expires: header in 2xx response "
		       "to SUBSCRIBE : ~p", [Invalid]),
	    Default
    end.

%%--------------------------------------------------------------------
%% @spec    (Key) ->
%%            [{Key, Value}] | []
%%
%%            Key = string() "\"User-Agent\" or \"Server\""
%%
%%            Value = string()
%%
%% @doc     Create a User-Agent or Server ExtraHeaders list, if not
%%          configured not to.
%% @end
%%--------------------------------------------------------------------
get_useragent_or_server(Key) ->
    case yxa_config:get_env(set_useragent_and_server) of
	{ok, true} ->
	    Value =
		lists:concat(["YXA/", version:get_version(),
			      " at ", siprequest:myhostname()]),
	    [{Key, [Value]}];
	{ok, false} ->
	    []
    end.

%%--------------------------------------------------------------------
%% @spec    (Expires) ->
%%            Refresh
%%
%%            Expires = integer()
%%
%%            Refresh = integer()
%%
%% @doc     Figure out what we should use as refresh timer for a
%%          subscription of ours. We must refresh _before_ the other
%%          end times out, and the other end might be 32 seconds
%%          ahead of us if we only received the last 200 OK response
%%          to SUBSCRIBE resend. We must also re-subscribe at least
%%          32 seconds before the remote end might terminate the
%%          subscription in case our (re-)SUBSCRIBE only gets to the
%%          other side at it's last resend. Hence we try to set up a
%%          refresh 64 seconds before the expires-time, unless it is
%%          very low.
%% @end
%%--------------------------------------------------------------------
get_subscribe_refresh_timeout(Expires) when is_integer(Expires) ->
    if
	Expires >= 64 + 60 ->
	    (Expires - 64) * 1000;
	true ->
	    round(Expires * 0.8) * 1000
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
    %% get_subscribe_refresh_timeout(Expires)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_subscribe_refresh_timeout/1 - 1"),
    60 * 1000 = get_subscribe_refresh_timeout(124),

    autotest:mark(?LINE, "get_subscribe_refresh_timeout/1 - 2"),
    (60 + 30) * 1000 = get_subscribe_refresh_timeout(124 + 30),

    autotest:mark(?LINE, "get_subscribe_refresh_timeout/1 - 3"),
    4 * 1000 = get_subscribe_refresh_timeout(5),

    ok.
