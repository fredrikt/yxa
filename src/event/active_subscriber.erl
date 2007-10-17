%%%-------------------------------------------------------------------
%%% File    : active_subscriber.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      SUBSCRIBE to some event package from a presenter, and
%%%           store any data we receive in NOTIFYs as event data. This
%%%           is for use with event packages where the UAs don't use
%%%           PUBLISH, and possibly for user agents that are edge
%%%           presence servers and don't PUBLISH their state.
%%%
%%%           How this works :
%%%
%%%           active_subscriber sends out a SUBSCRIBE. This subscribe
%%%           might result in zero or more subscriptions.
%%%
%%%           One of the established subscriptions will, according to
%%%           the specs (RFC3265), be established by a 2xx response
%%%           to the SUBSCRIBE, and the rest (if the SUBSCRIBE forked)
%%%           will be established by the NOTIFY requests sent from the
%%%           other endpoints towards the Contact: we placed in the
%%%           SUBSCRIBE.
%%%
%%%           To make the dialog establishing code a bit more uniform,
%%%           we don't set up a full dialog on the 2xx response to
%%%           SUBSCRIBE, but instead set up one full dialog for every
%%%           NOTIFY we receive. These dialogs are handled by one
%%%           active_subscription process started for each dialog.
%%%
%%%           Finally, after at least 32 seconds has passed since we
%%%           received the 2xx response to SUBSCRIBE, we unregister
%%%           the half dialog the active_subscriber process had, and
%%%           just hang around until all active_subscription processes
%%%           has terminated. The reason to hang around is to enable
%%%           whoever started us in the first place to call our stop/1
%%%           function. When stop/1 is called we tell all the
%%%           active_subscription processes to stop.
%%%
%%% NOTE    : This module isn't used by the event server for now, it
%%%           was just usable during development, possibly usable as
%%%           an example and possibly also needed for some future
%%%           event package.
%%%
%%% @since    10 May 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(active_subscriber).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/14,
	 stop/1,
	 shared_line/4,

	 ft/1
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
-record(state, {package_module,		%% atom(), event package module
		package_string,		%% string(), event package name
		presenter,		%% {user, SIPuser} | {address, AddressStr}, presenter SIP username or address
		uri,			%% sipurl record(), remote target (or where to send SUBSCRIBE request)
		interval,		%% integer(), number of seconds until our subscription terminates (what we ask for anyways)
		my_accept,		%% list() of string(), Accept: header values we use
		event,			%% [string()], Event: header value we use
		extra_headers,		%% list() of {Key, ValueL} - extra SIP headers to put in SUBSCRIBEs
		logtag,			%% string(), log prefix
		branch_base,		%% string(),
		subscription_pids = [],	%% list() of pid(), list of active_subscription worker processes we have started
		subscribe_pid,		%% undefined | pid() of currently ongoing SUBSCRIBE client transaction
		parent,			%% pid() of our parent process
		my_contact,		%% string(), the Contact: header value we use
		cseq,			%% undefined | integer(), last CSeq we used
		sent_subscr_ts,		%% integer(), util:timestamp() notion of when we last sent a SUBSCRIBE
		mystate = init,		%% init | active | stop | stopped
		callid,
		localtag,
		presentity		%% undefined | {address, AddressStr} | {user, User}
	       }).


%%====================================================================
%% External functions
%%====================================================================

start(PackageM, PackageS, Presenter, RequestURI, From, To, Dst, SInterval, Accept, ExtraH,
      EventSuffix, LogTag, Parent) ->
    start(PackageM, PackageS, Presenter, RequestURI, From, To, Dst,
	  SInterval, Accept, ExtraH, EventSuffix, LogTag, Parent, undefined).

%%--------------------------------------------------------------------
%% @spec    (PackageM, PackageS, Presenter, RequestURI, From, To, Dst,
%%          SInterval, Accept, ExtraH, EventSuffix, LogTag, Parent,
%%          Presentity) ->
%%            Pid
%%
%%            PackageM    = atom() "event package module"
%%            PackageS    = string() "event package name"
%%            Presenter   = {user, SIPuser} | {address, AddressStr}
%%            RequestURI  = #sipurl{} "where to send SUBSCRIBE"
%%            From        = undefined | #contact{} "From: to use"
%%            To          = undefiend | #contact{} "To: to use"
%%            Dst         = #sipdst{} "where to send requests"
%%            SInterval   = integer() "SUBSCRIBE expire time in seconds"
%%            Accept      = [string()] "Accept: header value"
%%            ExtraH      = [{Key, ValueL}] "extra headers to set in our SUBSCRIBEs"
%%            EventSuffix = string() "our Event: will be PackageS plus this suffix"
%%            LogTag      = string() "log prefix"
%%            Parent      = pid()
%%            Presentity  = undefined          |
%%                          {user, User}       |
%%                          {address, Address}
%%
%%            Pid = pid()
%%
%% @doc     Starts the active_subscriber gen_server.
%% @end
%%--------------------------------------------------------------------
start(PackageM, PackageS, Presenter, RequestURI, From, To, Dst, SInterval, Accept, ExtraH, EventSuffix, LogTag, Parent,
      Presentity)
  when is_list(PackageS), is_tuple(Presenter), is_record(RequestURI, sipurl),
       is_record(From, contact) orelse From == undefined, is_record(To, contact) orelse To == undefined,
       is_integer(SInterval), is_list(Accept), is_list(ExtraH), is_list(EventSuffix),
       is_list(LogTag), is_pid(Parent) ->
    %% Now = util:timestamp(),

    Event = [PackageS ++ EventSuffix],

    State1 = #state{package_module	= PackageM,
		    package_string    	= PackageS,
		    presenter		= Presenter,	%% remote party identifier
		    presentity		= Presentity,	%% local resource identifier
		    uri			= RequestURI,
		    event		= Event,
		    interval		= SInterval,
		    my_accept		= Accept,
		    extra_headers	= ExtraH,
		    logtag		= LogTag,
		    parent		= Parent
		   },

    gen_server:start(?MODULE, {State1, From, To, Dst}, []).

%%--------------------------------------------------------------------
%% @spec    (Pid) -> ok | {error, already_stopped}
%%
%%            Pid = pid()
%%
%% @doc     Shuts down an active subscription.
%% @end
%%--------------------------------------------------------------------
stop(Pid) ->
    gen_server:call(Pid, stop).


%%--------------------------------------------------------------------
%% @spec    (Resource, SIPuser, URI, Params) ->
%%            term() "return value of start()"
%%
%%            Resource = string() "session identifier"
%%            SIPuser  = string() "username of presenter"
%%            URI      = #sipurl{} "other side"
%%            Params   = [{Key, Value}]
%%            Key      = term()
%%            Value    = term()
%%
%% @doc     Initiates a shared line (bridged line appearance).
%% @end
%%--------------------------------------------------------------------
shared_line(Resource, SIPuser, URI, Params) when is_list(Resource), is_record(URI, sipurl), is_list(Params) ->
    Contact = generate_contact_str(Resource),
    [ParsedContact] = contact:parse([Contact]),
    {Presenter, To} =
	case is_list(SIPuser) of
	    true ->
		ToC =
		    case sipuserdb:get_addresses_for_user(SIPuser) of
			[FirstAddr | _] when is_list(FirstAddr) ->
			    contact:new(FirstAddr);
			_ ->
			    contact:new(URI)
		    end,
		{{user, SIPuser},
		 ToC
		};
	    false ->
		{{address, ParsedContact#contact.urlstr},
		 contact:new(URI)
		}
	end,

    Presentity = {address, ParsedContact#contact.urlstr},
    From = undefined,

    Interval =
	case lists:keysearch(interval, 1, Params) of
	    {value, {interval, IntervalV}} when is_integer(IntervalV) ->
		IntervalV;
	    false ->
		600
	end,

    %% XXX really figure out message size
    ApproxMsgSize = 500,

    {PathL, Dst} =
	case lists:keysearch(path, 1, Params) of
	    {value, {path, [FirstPath | _] = PathV}} ->
		[PathC] = contact:parse([FirstPath]),
		PathURL = sipurl:parse(PathC#contact.urlstr),
		[Dst1 | _] = sipdst:url_to_dstlist(PathURL, ApproxMsgSize, URI),
		{[{"Route", PathV}], Dst1};
	    false ->
		[Dst1 | _] = sipdst:url_to_dstlist(URI, ApproxMsgSize, URI),
		{[], Dst1}
	end,

    Accept = ["application/dialog-info+xml"],
    ExtraHeaders = [{"Contact", [Contact]}] ++ PathL,

    LogTag = lists:flatten( lists:concat(["Shared line (", Resource, ")"]) ),

    start(dialog_package, "dialog", Presenter, URI, From, To, Dst, Interval, Accept,
	  ExtraHeaders, ";sla;include-session-description", LogTag, self(), Presentity).

%%--------------------------------------------------------------------
%% @spec    (Type) -> term()
%%
%%            Type = atom()
%%
%% @doc     Fredriks development testing code.
%% @hidden
%% @end
%%--------------------------------------------------------------------
ft(sipit) ->
    URI = sipurl:parse("sip:line225@132.177.126.87:5074"),
    From = undefined,
    [To] = contact:parse(["<sip:ft@example.org>"]),

    Presenter = {user, "sharedline"},
    Interval = 600,
    Accept = ["application/dialog-info+xml"],
    Contact = "<sip:shared@eventserver.yxa.sipit.net:5010>",
    ExtraHeaders = [{"Contact", [Contact]}],

    [Dst | _] = sipdst:url_to_dstlist(URI, 500, URI),

    start(dialog_package, "dialog", Presenter, URI, From, To, Dst, Interval, Accept,
	  ExtraHeaders, ";sla", "test logtag", self());

ft(hemma) ->
    URI = sipurl:parse("sip:ft@192.168.123.15:5062;transport=udp"),
    From = undefined,
    [To] = contact:parse(["<sip:ft@nbar.it.su.se>"]),

    Presenter = {user, "footest"},
    Interval = 60,
    Accept = presence_package:package_parameters("presence", subscribe_accept_content_types),
    ExtraHeaders = [],

    [Dst | _] = sipdst:url_to_dstlist(URI, 500, URI),

    start(presence_package, "presence", Presenter, URI, From, To, Dst, Interval, Accept,
	  ExtraHeaders, "", "test logtag", self());


ft(work) ->
    URI = sipurl:parse("sip:ft@193.11.30.96:5062;transport=udp"),
    From = undefined,
    [To] = contact:parse(["<sip:ft@nbar.it.su.se>"]),

    Presenter = {user, "footest"},
    Interval = 60,
    Accept = presence_package:package_parameters("presence", subscribe_accept_content_types),
    ExtraHeaders = [],

    [Dst | _] = sipdst:url_to_dstlist(URI, 500, URI),

    start(presence_package, "presence", Presenter, URI, From, To, Dst, Interval, Accept,
	  ExtraHeaders, "", "test logtag", self());

ft(work2) ->
    URI = sipurl:parse("sip:ft22@193.11.30.10:5062;transport=udp"),
    From = undefined,
    [To] = contact:parse(["<sip:ft22@thulin.net>"]),

    Presenter = {user, "ft22"},
    Interval = 60,
    Accept = presence_package:package_parameters("presence", subscribe_accept_content_types),
    ExtraHeaders = [],

    [Dst | _] = sipdst:url_to_dstlist(URI, 500, URI),

    start(presence_package, "presence", Presenter, URI, From, To, Dst, Interval, Accept,
	  ExtraHeaders, "", "test logtag", self());

ft(shared) ->
    URI = sipurl:parse("sip:ft@130.237.95.166:2391;transport=tcp;line=47c9hufz"),
    From = undefined,
    [To] = contact:parse(["<sip:ft@pappersk.org>"]),

    Presenter = {user, "sharedline"},
    Interval = 60,
    Accept = ["application/dialog-info+xml"],
    ExtraHeaders = [],

    [Dst | _] = sipdst:url_to_dstlist(sipurl:parse("sip:incomingproxy2.devel.sip.su.se"), 500, URI),
   %% [Dst | _] = sipdst:url_to_dstlist(sipurl:parse("sip:outgoingproxy2.devel.sip.su.se:5050"), 500, URI),

    start(dialog_package, "dialog", Presenter, URI, From, To, Dst, Interval, Accept,
	  ExtraHeaders, "", "test logtag", self());

ft(shared2) ->
    LocL = siplocation:get_locations_for_users(["ft.sip2"]),
    [Loc] = [E || E <- LocL, E#siplocationdb_e.instance == "<urn:uuid:ff5fa541-01f7-4c69-8b96-a864f12fdcaf>"],

%    URI = sipurl:set([{user, "shared-line"}, {param, []}], Loc#siplocationdb_e.address),
    URI = sipurl:parse("sip:something@thulin.net"),

    Route =
	case lists:keysearch(path, 1, Loc#siplocationdb_e.flags) of
	    {value, {path, [Path]}} ->
		[Path, "<" ++ sipurl:print(Loc#siplocationdb_e.address) ++ ">"];
	    false ->
		["<" ++ sipurl:print(Loc#siplocationdb_e.address) ++ ">"]
	end,

    From = undefined,
    [To] = contact:parse(["<sip:ft@pappersk.org>"]),

    Presenter = {user, "sharedline"},
    Interval = 180,
    Accept = ["application/dialog-info+xml"],
    ExtraHeaders = [{"Route", Route}],

    [FirstContact] = contact:parse([hd(Route)]),

    [Dst | _] = sipdst:url_to_dstlist(sipurl:parse(FirstContact#contact.urlstr), 500, URI),

    start(dialog_package, "dialog", Presenter, URI, From, To, Dst, Interval, Accept,
	  ExtraHeaders, "", "test logtag", self());

ft(e60) ->
    URI = sipurl:parse("sip:ft@193.11.30.33"),
    From = undefined,
    [To] = contact:parse(["<sip:ft@it.su.se>"]),

    Presenter = {user, "test"},
    Interval = 60,
    Accept = presence_package:package_parameters("presence", subscribe_accept_content_types),
    ExtraHeaders = [],

%%    [Dst | _] = sipdst:url_to_dstlist(sipurl:parse("sip:incomingproxy2.devel.sip.su.se"), 500, URI),
   %% [Dst | _] = sipdst:url_to_dstlist(sipurl:parse("sip:outgoingproxy2.devel.sip.su.se:5050"), 500, URI),
    [Dst | _] = sipdst:url_to_dstlist(URI, 500, URI),

    start(presence_package, "presence", Presenter, URI, From, To, Dst, Interval, Accept,
	  ExtraHeaders, "", "test logtag", self()).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({State1, From, To, Dst}) ->
%%            {ok, State}          |
%%            ignore               |
%%            {stop, Reason}
%%
%%            State1 = #state{} "state in"
%%            From   = undefined | #contact{} "From: to use"
%%            To     = undefiend | #contact{} "To: to use"
%%            Dst    = #sipdst{} "where to send request"
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({State1, From, To, Dst}) ->
    process_flag(trap_exit, true),

    BranchBase = siprequest:generate_branch(),
    Branch = lists:concat([BranchBase, "-UAC"]),

    {Contact, ContactL} =
	case lists:keysearch("Contact", 1, State1#state.extra_headers) of
	    {value, {"Contact", [Contact1]}} -> {Contact1, []};
	    false ->
		Contact1 = generate_contact_str(),
		{Contact1, [{"Contact", [Contact1]}]}
	end,

    UserAgent = get_useragent_or_server("User-Agent"),
    {ok, MaxForwards} = yxa_config:get_env(default_max_forwards),
    ExtraHeaders1 =
	[{"Event",   State1#state.event},
	 {"Accept",  State1#state.my_accept},
	 {"Max-Forwards", [integer_to_list(MaxForwards)]}
	] ++ ContactL ++ UserAgent ++ State1#state.extra_headers,

    State =
	State1#state{branch_base	= BranchBase,
		     my_contact		= Contact,
		     extra_headers	= ExtraHeaders1
		  },

    %% Generate parameters for sending our first SUBSCRIBE

    ExtraHeaders = [{"Expires", [integer_to_list(State#state.interval)]} |
		    ExtraHeaders1],

    From1 =
	case From of
	    _ when is_record(From, contact) ->
		From;
	    undefined ->
		[FromC] = contact:parse([Contact]),
		FromC
	end,

    To1 =
	case To of
	    _ when is_record(To, contact) ->
		To;
	    undefined ->
		contact:new(State#state.uri)
	end,

    {ok, Request, CallId, FromTag, CSeq} =
	start_generate_request(State#state.uri, From1, To1, ExtraHeaders),

    %% register half dialog (no To-tag)
    ok = sipdialog:register_dialog_controller(CallId, FromTag, self()),

    logger:log(debug, "~s: Active subscriber: Starting by sending a SUBSCRIBE ~s (to ~s)",
	       [State#state.logtag, sipurl:print(State#state.uri), sipdst:dst2str(Dst)]),

    Now = util:timestamp(),
    Timeout = 5,

    Pid = transactionlayer:start_client_transaction(Request, Dst, Branch, Timeout, self()),

    {ok, State#state{subscribe_pid	= Pid,
		     sent_subscr_ts	= Now,
		     cseq		= CSeq,
		     callid		= CallId,
		     localtag		= FromTag,
		     uri		= Request#request.uri
		    }}.

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
%% @spec    (stop, From, State) ->
%%            {reply, Reply, NewState}
%%
%%            NewState = #state{}
%%            Reply    = ok | {error, already_stopped}
%%
%% @doc     Shuts down the active subscription.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    logger:log(debug, "Active subscriber: Extra debug: Requested to stop when in state '~p'", [State#state.mystate]),
    case State#state.mystate of
	init ->
	    {reply, {error, not_active_yet}, State};
	active ->
	    %% Signal all our subscription processes to stop, reply OK and then exit
	    %% when we have no subscription processes left
	    [util:safe_signal("Active subscriber: ", SPid, {stop, self()}) || SPid <- State#state.subscription_pids],
	    {reply, ok, State#state{mystate = stop}};
	stopped ->
	    {reply, {error, already_stopped}, State}
    end;

%%--------------------------------------------------------------------
%% @spec    ({set_interval, NewInterval}, From, State) ->
%%            {reply, Reply, NewState}
%%
%%            NewInterval = integer()
%%
%%            NewState = #state{}
%%            Reply    = ok
%%
%% @doc     Change the SUBSCRIBE refresh interval.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({set_interval, NewInterval}, _From, State) when is_integer(NewInterval) ->
    %% XXX send a new SUBSCRIBE? Only do it if interval is decreased?
    {reply, ok, State#state{interval = NewInterval}};

%%--------------------------------------------------------------------
%% @spec    (Unknown, From, State) -> {noreply, State}
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "Active subscriber: Received unknown gen_server call : ~p", [Unknown]),
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
%% @spec    ({received_notify, FromPid, Request, YxaCtx, THandler,
%%          Ctx}, State) -> {noreply, State}
%%
%%            FromPid  = pid()
%%            Request  = #request{}
%%            YxaCtx   = #yxa_ctx{}
%%            THandler = term() "server transaction handler"
%%            Ctx      = #event_ctx{}
%%
%% @doc     One of our active_subscription pids have received a NOTIFY
%%          request with a body.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({received_notify, FromPid, Request, YxaCtx, THandler, Ctx}, State)
  when is_pid(FromPid), is_record(Request, request) ->

    #state{package_module = PackageM,
	   package_string = PackageS,
	   logtag         = LogTag
	  } = State,

    Ctx1_1 =
	case State#state.presenter of
	    {user, SIPuser} ->
		Ctx#event_ctx{sipuser = SIPuser};
	    _ ->
		Ctx
	end,
    Ctx1 =
	Ctx1_1#event_ctx{presentity = State#state.presentity
			},

    YxaCtx1 =
	YxaCtx#yxa_ctx{app_logtag   = LogTag,
		       thandler = THandler
		      },
    case PackageM:request(PackageS, Request, YxaCtx1, Ctx1) of
	{error, need_auth} ->
	    case eventserver:authenticate_subscriber(Request, YxaCtx1) of
		{true, AuthSIPuser} ->
		    Ctx2 = Ctx1#event_ctx{sipuser = AuthSIPuser},
		    PackageM_Res = PackageM:request(PackageS, Request, YxaCtx1, Ctx2),
		    logger:log(debug, "Active subscriber: Extra debug: Result of ~p:request/4 was : ~p",
			       [PackageM, PackageM_Res]);
		false ->
		    logger:log(normal, "~s: Active subscriber: ~s -> '403 Forbidden'",
			       [LogTag, YxaCtx#yxa_ctx.logstr]),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden");
		drop ->
		    ok
	    end;
	PackageM_Res ->
	    logger:log(debug, "Active subscriber: Extra debug: Result of ~p:request/4 was : ~p",
		       [PackageM, PackageM_Res])
    end,

    {noreply, State};

handle_cast(Msg, State) ->
    logger:log(error, "Active subscriber: Received unknown gen_server cast : ~p", [Msg]),
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
%% @spec    ({branch_result, FromPid, Branch, SipState, Response},
%%          State) ->
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     A NOTIFY client transaction we started resulted in a
%%          response. Check if that response was a (received) 481 and
%%          terminate if it was.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({branch_result, FromPid, _Branch, SipState, Response}, #state{subscribe_pid = FromPid} = State) ->
    Status =
	case Response of
	    #response{status = Status1} ->
		Status1;
	    {Status1, _Reason1} ->
		Status1
	end,

    case SipState of
	completed when is_record(Response, response), Status >= 200, Status =< 299 ->
	    logger:log(debug, "Active subscriber: Received a 2xx response to the SUBSCRIBE, "
		       "eagerly awaiting the NOTIFY(s)"),
	    %% A 2xx response to SUBSCRIBE really establishes a dialog, and then NOTIFYs with different To-tag
	    %% establishes more dialogs (if the request forked, and more than one entity responded with 2xx).
	    %% We cut a corner here and only set up dialog state when NOTIFYs are received, to treat all equally.

	    DialogTimeout = 60,
	    ok = sipdialog:set_dialog_expires(State#state.callid, State#state.localtag, undefined,
					      DialogTimeout + 10),
	    timer:send_after(DialogTimeout * 1000, unregister_dialog),
	    {noreply, State#state{mystate        = active,
				  sent_subscr_ts = util:timestamp()
				 }};

	_ when Status >= 300 ->
	    %% Our initial SUBSCRIBE resulted in a non-2xx response. Even if the SUBSCRIBE forked, this means
	    %% that no endpoint answered our SUBSCRIBE with a 2xx response, so we can just terminate here.
	    logger:log(normal, "Active subscriber: SUBSCRIBE ~s resulted in non-2xx response ~p, terminating "
		       "the subscriber immediately.", [sipurl:print(State#state.uri), Status]),
	    {stop, normal, State};

	_ ->
	    %% ignore anything else, probably provisional responses (1xx)
	    logger:log(debug, "Active subscriber: Ignoring ~p response from ~p (SIP state '~p')",
		       [Status, FromPid, SipState]),
	    {noreply, State}
    end;


%%--------------------------------------------------------------------
%% @spec    ({clienttransaction_terminating, ...}, State) ->
%%            {noreply, State, Timeout}
%%
%% @doc     A NOTIFY client transaction has terminated. Remove it from
%%          our list of active NOTIFY client transactions.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({clienttransaction_terminating, FromPid, _Branch}, #state{subscribe_pid = FromPid} = State) ->
    %% clear subscribe_pid
    NewState = State#state{subscribe_pid = undefined},
    {noreply, NewState};

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
    AdoptRes = transactionlayer:adopt_server_transaction_handler(THandler),
    FromPid ! {ok, self(), Ref},

    ResExtraH = get_useragent_or_server("Server"),

    case AdoptRes of
	THandler ->
	    case NewRequest#request.method of
		"NOTIFY" ->
		    ChildBranchBase = lists:flatten(lists:concat([State#state.branch_base, "-UAC-",
								  length(State#state.subscription_pids) + 1, "."])
						   ),
		    case active_subscription:start_link(NewRequest,
							YxaCtx,
							ChildBranchBase,
							self(),
							State#state.mystate,
							State#state.interval,
							State#state.extra_headers,
							State#state.cseq + 1
						       ) of
			{ok, Pid} when is_pid(Pid) ->
			    NewL = [Pid | State#state.subscription_pids],
			    {noreply, State#state{subscription_pids = NewL}};
			ignore ->
			    ok;
			Unknown ->
			    logger:log(error, "Active subscriber: Failed starting active_subscription process : ~p",
				       [Unknown]),
			    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error", ResExtraH),
			    {noreply, State}
		    end;
		_ ->
		    transactionlayer:send_response_handler(THandler, 501, "Not Implemented (inside dialog)", ResExtraH),
		    {noreply, State}
	    end;
	{ignore, _Reason} ->
	    {noreply, State}
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
    logger:log(error, "Active subscriber: Dialog expired, should not happen. Terminating."),
    {stop, normal, State};

%%--------------------------------------------------------------------
%% @spec    (unregister_dialog, State) ->
%%            {noreply, State}      |
%%            {stop, normal, State}
%%
%% @doc     Enough time has passed since we received the final
%%          response to our SUBSCRIBE transaction for us to close the
%%          gate for new subscriptions being established by NOTIFYs.
%%          We unregister the half dialog and exit if we haven't
%%          received any NOTIFYs, otherwise we stay alive to shepherd
%%          those processes until they have all exited.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(unregister_dialog, State) ->
    %% delete our half dialog
    ok = sipdialog:delete_dialog_controller(State#state.callid, State#state.localtag, undefined),
    case State#state.subscription_pids of
	[] ->
	    logger:log(normal, "Active subscriber: We never received any NOTIFYs, terminating."),
	    %% XXX care to send a SUBSCRIBE with Expires: 0?
	    {stop, normal, State};
	_ ->
	    {noreply, State}
    end;

%%--------------------------------------------------------------------
%% @spec    ({'EXIT', Pid, Reason}, State) ->
%%            {noreply, State}      |
%%            {stop, normal, State}
%%
%%            Pid    = pid()
%%            Reason = term()
%%
%% @doc     Handle exit signals from our linked worker processes.
%%          Remove them from our list of subscription_pids and
%%          terminate this process when there are none left.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    Level = case Reason of
		normal -> debug;
		shutdown -> debug;
		_ -> error
	    end,
    case Level of
	error ->
	    logger:log(error, "=== ERROR REPORT === Active subscriber: Process ~p terminated : ~p",
		       [Pid, Reason]);
	_ ->
	    logger:log(Level, "Active subscriber: Process ~p terminated : ~p", [Pid, Reason])
    end,

    case lists:member(Pid, State#state.subscription_pids) of
	true ->
	    NewL = State#state.subscription_pids -- [Pid],
	    case NewL of
		[] ->
		    %% XXX debug level
		    logger:log(normal, "Active subscriber: Last subscription handler exited, I will too."),
		    {stop, normal, State#state{subscription_pids = []}};
		_ ->
		    {noreply, State#state{subscription_pids = NewL}}
	    end;
	false ->
	    {noreply, State}
    end;

handle_info(Unknown, State) ->
    logger:log(error, "Active subscriber: Received unknown gen_server info : ~p", [Unknown]),
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
terminate(Reason, _State) ->
    case Reason of
        normal ->   logger:log(debug, "Active subscriber: terminating normally");
        shutdown -> logger:log(debug, "Active subscriber: shutting down");
        _ ->        logger:log(error, "Active subscriber: terminating : ~p", [Reason])
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


%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    (URI, From, To, ExtraHeaders) ->
%%            {ok, Request, CallId, FromTag, CSeq}
%%
%%            URI          = #sipurl{}
%%            From         = #contact{}
%%            To           = #contact{}
%%            ExtraHeaders = [{Key, ValueL}]
%%
%%            Request = #request{}
%%            CallId  = string() "Call-Id of generated request"
%%            FromTag = string() "From-tag of generated request"
%%            CSeq    = integer() "CSeq of generated request"
%%
%% @doc     Part of the startup functions. Build our initial request
%%          record.
%% @end
%%--------------------------------------------------------------------
start_generate_request(URI, From, To, ExtraHeaders) when is_record(URI, sipurl), is_record(From, contact),
							 is_record(To, contact), is_list(ExtraHeaders) ->
    Method = "SUBSCRIBE",

    {From1, FromTag} =
	case contact_param:find(From#contact.contact_param, "tag") of
	    [] ->
		Tag = siputil:generate_tag(),
		{contact:add_param(From, "tag", Tag), Tag};
	    [Tag] ->
		{From, Tag}
	end,

    {Megasec, Sec, Microsec} = now(),

    CallId = lists:concat([Megasec, "-", Sec, "-", Microsec,
			   "@", siprequest:myhostname()
			  ]),
    CSeq = 1,

    Header = keylist:from_list([{"From",	[contact:print(From1)]},
				{"To",		[contact:print(To)]},
				{"Call-Id",	[CallId]},
				{"CSeq",	[lists:concat([CSeq, " ", Method])]}
			       ] ++ ExtraHeaders),

    Request1 = #request{method = Method,
			uri    = URI,
			header = Header
		       },
    Request = siprequest:set_request_body(Request1, <<>>),

    {ok, Request, CallId, FromTag, CSeq}.


%%--------------------------------------------------------------------
%% @spec    () ->
%%            Contact
%%
%%            Contact = string() "SIP URL within ``<'' and ``>''."
%%
%% @doc     Generate a Contact header value for our requests. The
%%          registration as a dialog controller will get all requests
%%          on the dialog sent to us, so the user part of the contact
%%          is not important. We use the Erlang pid, without ``<''
%%          and ``>''.
%% @end
%%--------------------------------------------------------------------
generate_contact_str() ->
    PidStr = pid_to_list(self()),
    User = lists:reverse(
	     lists:foldl(fun($<, Acc) -> Acc;
			    ($>, Acc) -> Acc;
			    (C, Acc) ->
				 [C | Acc]
			 end, [], PidStr)
	     ),
    generate_contact_str(User).

%%--------------------------------------------------------------------
%% @spec    (User) ->
%%            Contact
%%
%%            User = string() "user part of contact URL produced"
%%
%%            Contact = string() "SIP URL within ``<'' and ``>''."
%%
%% @doc     Generate a Contact header value for our requests, with a
%%          specific user-part.
%% @end
%%--------------------------------------------------------------------
generate_contact_str(User) ->
    %% Figure out if we have to explicitly set the port number in the URL we create -
    %% we do like when we create Record-Route headers and always set it if it is not
    %% the default port for the protocol used, even though myhostname might resolve
    %% to the right port through NAPTR/SRV records in DNS
    Proto = "sip",
    MyPort = sipsocket:get_listenport(udp),
    URLport = sipsocket:default_port(Proto, none),
    Port = case (URLport == MyPort) of
	       true  -> none;
	       false -> MyPort
	   end,

    URL = sipurl:new([{user, User}, {host, siprequest:myhostname()}, {port, Port}]),
    "<" ++ sipurl:print(URL) ++ ">".


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
