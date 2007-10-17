%%%-------------------------------------------------------------------
%%% File    : refer.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      REFER tests.
%%%
%%% @since     8 Feb 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%
%%% If the real_start function is used, and a ProgressPid is supplied,
%%% then that process will receive progress reports from the REFER
%%% worker process (an instance of this module) of the form
%%%
%%%   {refer_progress, Pid, Method, Progress}
%%%
%%%      Pid      = pid(), REFER worker process pid
%%%      Method   = string(), SIP method that received a response
%%%      Progress = string(), progress made
%%%
%%%   Example progress reports :
%%%
%%%      Method = "INVITE" Progress = "200 Ok"
%%%      Method = "REFER"  Progress = "202 Accepted"
%%%      Method = "NOTIFY" Progress = "SIP/2.0 100 Trying"
%%%      Method = "NOTIFY" Progress = "SIP/2.0 200 OK"
%%%
%%% TODO :
%%%   * Handle multiple 2xx responses to INVITE.
%%%   * Handle multiple sipdst entrys when our URI resolves to more
%%%     than one sipdst (currently we just pick the first one).
%%%-------------------------------------------------------------------
-module(refer).

-behaviour(gen_server).

%% API
-export([
	 start_ir/2,
	 start_r/2,
	 real_start/7
	]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%% @type state() = #state{}.
%%                 no description
-record(state, {invite_request,	%% request record()
		invite_pid,	%% pid() | none
		invite_branch,	%% string()
		refer_request,	%% undefined | request record()
		refer_pid,	%% undefined | pid() | none
		refer_branch,	%% undefined | string()
		branch_base,	%% string(),
		branch_seq,	%% integer()
		dialog,		%% dialog record() | undefined
		referer,	%% contact record()
		referee,	%% contact record()
		refer_to,	%% contact record()
		log_fun,	%% undefined | function()
		progress_pid	%% undefined | pid()
	       }).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").

-define(FROM_URI, "<sip:referer@example.net>").
-define(TO_URI, "<sip:referee@example.net>").
-define(DEFAULT_TIMEOUT, 50).
-define(REFERRED_BY, "<sip:referredby@example.net>").	%% Required by Cisco 79xx


%%--------------------------------------------------------------------
%% @spec    (Referee, ReferTo) ->
%%            Does not matter - does not return until we are finished.
%%
%%            URLstr  = string() "parseable with sipurl:parse/1"
%%            ReferTo = string() "parseable with sipurl:parse/1"
%%
%% @doc     Refer Referee to ReferTo by first establishing a dialog
%%          using an INVITE, and then sending a REFER on that dialog.
%%          Note : Intended for invocation from the Erlang console.
%% @end
%%--------------------------------------------------------------------
start_ir(Referee, ReferTo) when is_list(Referee), is_list(ReferTo) ->
    case real_start(contact:parse([Referee]),
		    contact:parse([ReferTo]),
		    contact:parse([?REFERRED_BY]),
		    ?DEFAULT_TIMEOUT, undefined, undefined, []) of
	{ok, Pid, CallId, LocalTag} ->
	    R = wait_for_pids([Pid], undefined),
	    sipdialog:delete_dialog_controller(CallId, LocalTag, undefined),
	    R;
	Other ->
	    Other
    end.


%%--------------------------------------------------------------------
%% @spec    (Referee, ReferTo) ->
%%            Does not matter - does not return until we are finished.
%%
%%            Referee = string() "parseable with sipurl:parse/1"
%%            ReferTo = string() "parseable with sipurl:parse/1"
%%
%% @doc     Refer URLstr to ReferTo by just sending a REFER and
%%          thereafter handle the NOTIFYs that a standards compliant
%%          User-Agent will send us. Note : Intended for invocation
%%          from the Erlang console.
%% @end
%%--------------------------------------------------------------------
start_r(Referee, ReferTo) when is_list(Referee), is_list(ReferTo) ->
    case real_start(contact:parse([Referee]),
		    contact:parse([ReferTo]),
		    contact:parse([?REFERRED_BY]),
		    ?DEFAULT_TIMEOUT, undefined, undefined, [no_invite]) of
	{ok, Pid, CallId, LocalTag} ->
	    R = wait_for_pids([Pid], undefined),
	    sipdialog:delete_dialog_controller(CallId, LocalTag, undefined),
	    R;
	Other ->
	    Other
    end.


%%--------------------------------------------------------------------
%% @spec    (Referee, ReferTo, Referer, Timeout, ProgressPid,
%%          StartOptions) ->
%%            {ok, Pid, CallId, LocalTag} |
%%            error
%%
%%            Referee      = #contact{}
%%            ReferTo      = #contact{}
%%            Referer      = #contact{}
%%            Timeout      = integer()
%%            LogFun       = undefined | fun()
%%            ProgressPid  = undefined | pid() "notify this pid of progress we make"
%%            StartOptions = [no_invite] | []
%%
%%            Pid      = pid() "INVITE/REFER client transaction pid"
%%            CallId   = string() "part of Dialog ID"
%%            LocalTag = string() "part of Dialog ID"
%%
%% @doc     Build the initial request, call start_link and then wait
%%          until the started gen_server worker process exits.
%% @end
%%--------------------------------------------------------------------
real_start([Referee], [ReferTo], [Referer], Timeout, LogFun, ProgressPid, StartOptions) ->
    real_start(Referee, ReferTo, Referer, Timeout, LogFun, ProgressPid, StartOptions);

real_start(Referee, ReferTo, Referer, Timeout, LogFun, ProgressPid, StartOptions)
  when is_record(Referee, contact), is_record(ReferTo, contact), is_record(Referer, contact), is_integer(Timeout),
       is_list(StartOptions) ->
    Contact = generate_contact_str(),
    SDP = sdp:print({siphost:myip(), 12345},
		    [{$a, "recvonly"}]
		   ),
    {ok, Request, CallId, FromTag} =
	start_generate_request("INVITE",
			       Referer,
			       Referee,
			       [{"Contact", [Contact]},
				{"Content-Type", ["application/sdp"]}
			       ],
			       list_to_binary(SDP)
			      ),

    ok = sipdialog:register_dialog_controller(CallId, FromTag, self()),

    case start_link(Request, Referer, Referee, ReferTo, Timeout, LogFun, ProgressPid, StartOptions) of
	{ok, InviteHandlerPid} ->
	    {ok, InviteHandlerPid, CallId, FromTag};
	Res ->
	    log(LogFun, error, "Refer: FAILED starting a refer worker process : ~p", [Res]),
	    sipdialog:delete_dialog_controller(CallId, FromTag, undefined),
	    error
    end.


%% part of my_start/3
start_link(Request, Referer, Referee, ReferTo, Timeout, LogFun, ProgressPid, Options)
  when is_record(Request, request), is_record(Referer, contact), is_record(Referee, contact), is_record(ReferTo, contact),
       is_integer(Timeout) ->
    gen_server:start_link(?MODULE, [Request, Referer, Referee, ReferTo, Timeout, LogFun, ProgressPid, Options], []).


%%====================================================================
%% gen_server callbacks
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    ([Request, Referer, ReferTo, Timeout, ProgressPid,
%%          Options]) -> {ok, State}
%%
%%            Request     = #request{}
%%            Referer     = #contact{}
%%            Referee     = #contact{}
%%            ReferTo     = #contact{}
%%            Timeout     = integer() "timeout of the first request"
%%            LogFun      = undefined | fun()
%%            ProgressPid = undefined | pid()
%%            Options     = [no_invite] | []
%%
%% @doc     Initiates the gen_server worker process.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([Request, Referer, Referee, ReferTo, Timeout, LogFun, ProgressPid, Options])
  when is_record(Request, request), is_record(Referer, contact), is_record(ReferTo, contact), is_integer(Timeout),
       is_list(Options) ->
    URL = Request#request.uri,
    case sipdst:url_to_dstlist(URL, 500, URL) of
	[Dst | _] ->
	    BranchBase = siprequest:generate_branch(),
	    BranchSeq = 1,
	    Branch = lists:concat([BranchBase, "-UAC-", BranchSeq]),

	    State = #state{invite_request = Request,
			   invite_branch  = Branch,
			   branch_base    = BranchBase,
			   branch_seq     = BranchSeq + 1,
			   referer        = Referer,
			   referee        = Referee,
			   refer_to       = ReferTo,
			   log_fun        = LogFun,
			   progress_pid   = ProgressPid
			  },

	    case lists:member(no_invite, Options) of
		true ->
		    NewState = send_refer(State),
		    {ok, NewState};
		false ->
		    Pid = start_client_transaction(Request, Dst, Branch, Timeout, LogFun),
		    {ok, State#state{invite_pid = Pid}}
	    end;
	_ ->
	    log(LogFun, error, "FAILED: No usable destinations found for request"),
	    ignore
    end.


handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @spec    ({branch_request, Pid, Branch, BranchState, Response},
%%          ...) ->
%%            {noreply, State}      |
%%            {stop, normal, State}
%%
%%            Pid         = pid() "client transaction"
%%            Branch      = string() "client transaction branch"
%%            BranchState = atom()
%%            Response    = #response{} | {Status, Reason}
%%
%% @doc     Handle responses received by our client transactions.
%% @hidden
%% @end
%%--------------------------------------------------------------------

%%
%% INVITE transcation resulted in non-2xx received final response
%%
handle_info({branch_result, Pid, Branch, BranchState, #response{status = Status} = Response},
	    #state{invite_pid = Pid, invite_branch = Branch} = State) ->
    if
	BranchState == terminated, Status >= 200, Status =< 299 ->
	    %% dialog creating response
	    NewState = received_invite_2xx(Response, State),
	    report_progress(State, "INVITE", Status, Response#response.reason),
	    {noreply, NewState};
	BranchState == completed, Status >= 300, Status =< 699 ->
	    log(State#state.log_fun, normal, "INVITE ~s : ~p ~s",
		[sipurl:print((State#state.invite_request)#request.uri), Status, Response#response.reason]
	       ),
	    report_progress(State, "INVITE", Status, Response#response.reason),
	    {stop, normal, State};
	true ->
	    log(State#state.log_fun, debug, "IGNORING response '~p ~s' to my INVITE",
		[Status, Response#response.reason]),
	    {noreply, State}
    end;

%%
%% INVITE transcation resulted in non-2xx _created_ final response
%%
handle_info({branch_result, Pid, Branch, _BranchState, {Status, Reason}},
	   #state{invite_pid = Pid, invite_branch = Branch} = State) when Status >= 300, Status =< 699 ->
    log(State#state.log_fun, normal, "INVITE ~s : (created) ~p ~s~n",
	[sipurl:print((State#state.invite_request)#request.uri), Status, Reason]
       ),
    report_progress(State, "INVITE", Status, Reason),
    {stop, normal, State};

%%
%% REFER transaction finished
%%
handle_info({branch_result, Pid, Branch, _BranchState, Response},
	   #state{refer_pid = Pid, refer_branch = Branch} = State) ->
    URI = (State#state.invite_request)#request.uri,
    {Status, Reason} =
	case Response of
	    #response{status = Status1, reason = Reason1} ->
		log(State#state.log_fun, normal, "REFER ~s : ~p ~s", [sipurl:print(URI), Status1, Reason1]),
		{Status1, Reason1};
	    {Status1, Reason1} ->
		log(State#state.log_fun, normal, "REFER ~s : (created) ~p ~s", [sipurl:print(URI), Status1, Reason1]),
		{Status1, Reason1}
	end,

    report_progress(State, "REFER", Status, Reason),

    if
	Status >= 200, Status =< 299 ->
	    {noreply, State};
	Status >= 300, Status =< 699 ->
	    %% Terminate dialog by sending a BYE (if INVITE initiated) if we receive a non-2xx final response
	    NewState = send_bye(State),
	    %% UACs destroy their dialogs as soon as they create a BYE
	    {stop, normal, NewState}
    end;

%%--------------------------------------------------------------------
%% @spec    ({clienttransaction_terminating, Pid, Branch}, ...) ->
%%            {noreply, State}       |
%%            {stop, StopRes, State}
%%
%%            Pid    = pid() "client transaction"
%%            Branch = string() "client transaction branch"
%%
%%            StopRes = normal | string()
%%
%% @doc     Handle client transactions terminating. If we only sent a
%%          REFER, terminate when that transaction terminates. XXX
%%          HANDLE NOTIFYS FIRST?
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({clienttransaction_terminating, Pid, Branch},
	    #state{invite_pid = Pid, invite_branch = Branch} = State) ->
    case State#state.refer_pid of
	undefined ->
	    {stop, "INVITE terminated prematurely", State};
	ReferPid when is_pid(ReferPid) ->
	    log(State#state.log_fun, debug, "INVITE transaction (~p ~p) terminated~n", [Pid, Branch]),
	    {noreply, State#state{invite_pid = none}}
    end;

handle_info({clienttransaction_terminating, Pid, Branch},
	    #state{refer_pid = Pid, refer_branch = Branch} = State) ->
    log(State#state.log_fun, debug, "REFER transaction (~p ~p) terminated~n", [Pid, Branch]),
    NewState = State#state{refer_pid = none},
    case State#state.invite_pid of
	undefined ->
	    {stop, normal, NewState};
	_ ->
	    {noreply, NewState}
    end;

%%--------------------------------------------------------------------
%% @spec    ({servertransaction_terminating, Pid}, ...) ->
%%            {noreply, State}
%%
%%            Pid = pid() "server transaction"
%%
%% @doc     Ignore signals about server transactions terminating.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({servertransaction_terminating, _Pid}, State) ->
    %% ignore all signals about server transactions (all the NOTIFYs for example) terminating, we don't care
    {noreply, State};

%%--------------------------------------------------------------------
%% @spec    ({new_request, FromPid, Ref, NewRequest, YxaCtx}, ...) ->
%%            {noreply, State}      |
%%            {stop, normal, State}
%%
%%            FromPid    = pid() "transaction layer"
%%            Ref        = ref() "unique reference to ack this message with (signal back to transaction layer)"
%%            NewRequest = #request{}
%%            YxaCtx     = #yxa_ctx{}
%%
%% @doc     Handle incoming requests on our dialog. Answer all NOFITYs
%%          with '200 Ok', handle BYE and reject all other methods
%%          with '501 Not Implemented'.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({new_request, FromPid, Ref, NewRequest, _YxaCtx}, State) when is_record(NewRequest, request) ->
    THandler = transactionlayer:get_handler_for_request(NewRequest),
    transactionlayer:adopt_server_transaction_handler(THandler),
    FromPid ! {ok, self(), Ref},

    {Action, NewDialog} =
	case sipdialog:update_dialog_recv_request(NewRequest, State#state.dialog) of
	    {error, old_cseq} ->
		transactionlayer:send_response_handler(THandler, 500, "CSeq not higher than last requests"),
		{noreply, State#state.dialog};
	    {ok, NewDialog1} ->
		case NewRequest#request.method of
		    "NOTIFY" ->
			%% answer all NOTIFY with 200 Ok
			transactionlayer:send_response_handler(THandler, 200, "Ok"),

			{CSeq, "NOTIFY"} = sipheader:cseq(NewRequest#request.header),

			case keylist:fetch('content-type', NewRequest#request.header) of
			    ["message/sipfrag;version=2.0"] ->
				log(State#state.log_fun, normal, "Received NOTIFY ~p, refer progress : ~p",
				    [CSeq, binary_to_list(NewRequest#request.body)]),
				report_progress(State, "NOTIFY", binary_to_list(NewRequest#request.body));
			    ["message/sipfrag"] ->
				log(State#state.log_fun, normal, "Received NOTIFY ~p, refer progress : ~p",
				    [CSeq, binary_to_list(NewRequest#request.body)]),
				report_progress(State, "NOTIFY", binary_to_list(NewRequest#request.body));
			    U ->
				log(State#state.log_fun, debug, "Received NOTIFY ~p with unknown Content-Type ~p",
				    [CSeq, U])
			end,

			case keylist:fetch("Subscription-State", NewRequest#request.header) of
			    ["terminated" ++ _Rest] ->
				%% Ok, the subscription dialog usage is now over. Terminate the dialog.
				log(State#state.log_fun, normal, "NOTIFY has Subscription-State 'terminated'"),
				ByeState = send_bye(State#state{dialog = NewDialog1}),
				{stop, ByeState#state.dialog};
			    _ ->
				{noreply, NewDialog1}
			end;
		    "BYE" ->
			%% answer BYE with 200 Ok
			transactionlayer:send_response_handler(THandler, 200, "Ok"),

			log(State#state.log_fun, normal, "Dialog ended by remote end (using BYE)"),

			{stop, NewDialog1};
		    _ ->
			%% answer all unknown requests with 501 Not Implemented
			transactionlayer:send_response_handler(THandler, 501, "Not Implemented"),

			{noreply, NewDialog1}
		end
	end,

    log(State#state.log_fun, extra_verbose, "~n~nDIALOG DUMP :~n~s~n", [sipdialog:dialog2str(NewDialog)]),
    case Action of
	noreply ->
	    {noreply, State#state{dialog = NewDialog}};
	stop ->
	    {stop, normal, State#state{dialog = NewDialog}}
    end;

%%--------------------------------------------------------------------
%% @spec    ({new_response, Response, YxaCtx}, ...) ->
%%            {noreply, State}      |
%%            {stop, normal, State}
%%
%%            Response = #response{}
%%            YxaCtx   = #yxa_ctx{} ""
%%
%% @doc     Handle incoming responses not matching any client
%%          transaction, but matching our dialog.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({new_response, #response{status = Status} = Response, _YxaCtx},
	    State) when Status >= 200, Status =< 299 ->
    case sipheader:cseq(Response#response.header) of
	{_CSeqNum, "INVITE"} ->
	    NewState = received_invite_2xx(Response, State),
	    {noreply, NewState};
	{_CSeqNum, Method} ->
	    log(State#state.log_fun, debug, "IGNORING response '~p ~s' to ~s",
		[Status, Response#response.reason, Method]),
	    {noreply, State}
    end;

%%--------------------------------------------------------------------
%% @spec    ({dialog_expired, DialogId}, ...) ->
%%            {stop, dialog_expired, State}
%%
%%            DialogId = term() "dialog id for the dialog we have registered as dialog controller for"
%%
%% @doc     Our dialog has expired. Terminate.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({dialog_expired, DialogId}, State) ->
    log(State#state.log_fun, error, "Dialog ~p apparently expired, exiting.", [DialogId]),
    {stop, dialog_expired, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @spec    (Method, Referer, Referee, ExtraHeaders, Body) ->
%%            {ok, Request, CallId, FromTag}
%%
%%            Method       = string() "SIP method"
%%            Referer      = #contact{}
%%            Referee      = #contact{}
%%            ExtraHeaders = [{Key, ValueList} tuple()]
%%            Body         = binary() "request body"
%%
%%            Request = #request{}
%%            CallId  = string() "Call-Id of generated request"
%%            FromTag = string() "From-tag of generated request"
%%
%% @doc     Part of the startup functions. Build our initial request
%%          record.
%% @end
%%--------------------------------------------------------------------
start_generate_request(Method, Referer, Referee, ExtraHeaders, Body) ->
    FromTag = siputil:generate_tag(),
    {Megasec, Sec, Microsec} = now(),

    CallId = lists:concat([Megasec * 1000000 + Sec, "-", Microsec,
			   "@", siprequest:myhostname()
			  ]),
    CSeq = 1,
    {ok, MaxForwards} = yxa_config:get_env(default_max_forwards),

    FromContact = contact:add_param(Referer, "tag", FromTag),
    Header = keylist:from_list([{"From",	[contact:print(FromContact)]},
				{"To",		[contact:print(Referee)]},
				{"Call-Id",	[CallId]},
				{"CSeq",	[lists:concat([CSeq, " ", Method])]},
				{"Max-Forwards", [integer_to_list(MaxForwards)]}
			       ] ++ ExtraHeaders),

    RefereeURL = sipurl:parse(Referee#contact.urlstr),
    Request1 = #request{method = Method, uri = RefereeURL, header = Header},
    Request = siprequest:set_request_body(Request1, Body),

    {ok, Request, CallId, FromTag}.


%%--------------------------------------------------------------------
%% @spec    () ->
%%            Contact
%%
%%            Contact = string() "SIP URL inside \"<\" and \">\""
%%
%% @doc     Generate a Contact header value for our requests. The
%%          registration as a dialog controller will get all requests
%%          on the dialog sent to us, so the user part of the contact
%%          is not important. We use the Erlang pid, without "<" and
%%          ">".
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
    URL = sipurl:new([{user, User}, {host, siprequest:myhostname()}]),
    "<" ++ sipurl:print(URL) ++ ">".


%%--------------------------------------------------------------------
%% @spec    (Response, State) ->
%%            NewState
%%
%%            Response = #response{}
%%            State    = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Handle 2xx responses to INVITE. If we haven't done so yet
%%          we start a REFER when we receive a 2xx.
%% @end
%%--------------------------------------------------------------------
received_invite_2xx(Response, State) when is_record(Response, response), is_record(State, state) ->
    Dialog =
	case State#state.dialog of
	    undefined ->
		{ok, D} = sipdialog:create_dialog_state_uac(State#state.invite_request, Response),
		D;
	    D ->
		D
	end,

    %% relieve the general dialog controller (that started this gen_server) from messages
    %% within the now existing dialog
    ok = sipdialog:register_dialog_controller(Dialog, self()),

    NewState = generate_ack(Response, State#state{dialog = Dialog}),

    case NewState#state.refer_request of
	undefined ->
	    send_refer(NewState);
	_ ->
	    NewState
    end.


%%--------------------------------------------------------------------
%% @spec    (Response, State) ->
%%            NewState
%%
%%            Response = #response{}
%%            State    = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Generate and send ACKs of 2xx responses to INVITE. we
%%          start a REFER when we receive a 2xx.
%% @end
%%--------------------------------------------------------------------
generate_ack(Response, #state{invite_branch = Branch} = State) ->
    Header = Response#response.header,
    {CSeq, _} = sipheader:cseq(Header),
    SendHeader1 = keylist:copy(Header, ['from', 'call-id']),
    SendHeader2 = keylist:set("To", keylist:fetch('to', Header), SendHeader1),
    SendHeader3 = keylist:set("CSeq", [sipheader:cseq_print({CSeq, "ACK"})], SendHeader2),
    SendHeader = case keylist:fetch('record-route', Header) of
		     [] ->
			 SendHeader3;
		     Route ->
			 keylist:set("Route", Route, SendHeader3)
		 end,
    [Contact] = contact:parse(keylist:fetch('contact', Header)),
    AckURI = sipurl:parse(Contact#contact.urlstr),
    ACKRequest1 = #request{method = "ACK",
			   uri    = AckURI,
			   header = SendHeader
			  },
    ACKRequest = siprequest:set_request_body(ACKRequest1, <<>>),

    %% Logging
    BinLine1 = list_to_binary([ACKRequest#request.method, " ", sipurl:print(ACKRequest#request.uri), " SIP/2.0"]),
    BinMsg = siprequest:binary_make_message(BinLine1, ACKRequest#request.header, <<>>),
    log(State#state.log_fun, debug, "Sending ACK (branch ~p) :~n~s~n", [Branch, binary_to_list(BinMsg)]),

    [Dst | _] = sipdst:url_to_dstlist(AckURI, 500, AckURI),
    transportlayer:send_proxy_request(none, ACKRequest, Dst, ["branch=" ++ Branch]),
    State.


%%--------------------------------------------------------------------
%% @spec    (State) ->
%%            NewState
%%
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Generate and send a REFER request.
%% @end
%%--------------------------------------------------------------------
send_refer(State) ->
    {ok, Request1, NewDialog, Dst} = generate_new_request("REFER", State),

    Header1 = Request1#request.header,
    Header2 = keylist:set("Refer-To", [contact:print(State#state.refer_to)], Header1),
    Header3 = keylist:set("Referred-By", [contact:print(State#state.referer)], Header2),

    Request = Request1#request{header = Header3},

    Timeout = 5,
    BranchSeq = State#state.branch_seq,
    Branch = lists:concat([State#state.branch_base, "-UAC-", BranchSeq]),

    Pid = start_client_transaction(Request, Dst, Branch, Timeout, State#state.log_fun),

    State#state{refer_pid     = Pid,
		refer_request = Request,
		refer_branch  = Branch,
		dialog        = NewDialog,
		branch_seq    = BranchSeq + 1
	       }.


%%--------------------------------------------------------------------
%% @spec    (State) ->
%%            NewState
%%
%%            State = #state{}
%%
%%            NewState = #state{}
%%
%% @doc     Generate and send a BYE request.
%% @end
%%--------------------------------------------------------------------
send_bye(State) ->
    {ok, Request1, NewDialog, Dst} = generate_new_request("BYE", State),

    Header1 = keylist:delete('contact', Request1#request.header),
    Request = Request1#request{header = Header1},

    Timeout = 32,
    BranchSeq = State#state.branch_seq,
    Branch = lists:concat([State#state.branch_base, "-UAC-", BranchSeq]),

    _Pid = start_client_transaction(Request, Dst, Branch, Timeout, State#state.log_fun),

    State#state{dialog        = NewDialog,
		branch_seq    = BranchSeq + 1
	       }.


%%--------------------------------------------------------------------
%% @spec    (Method, State) -> {ok, Request, NewDialog}
%%
%%            Method = string() "SIP method"
%%            State  = #state{}
%%
%% @doc     Generate a request template using values from the dialog
%%          state in State#state.dialog, or from the INVITE request
%%          created during startup and stored in
%%          State#state.invite_request (note that the INVITE request
%%          is always created, even though it is not always sent).
%% @end
%%--------------------------------------------------------------------
generate_new_request(Method, State) ->
    %% Figure out a bunch of parameters in ways that vary depending on if we have
    %% an existing dialog to send the REFER in or not
    {NewDialog, CSeqNum, CallId, TargetURI, To, Contact, Route, Dst} =
	case State#state.dialog of
	    undefined ->
		%% No existing dialog
		IHeader = (State#state.invite_request)#request.header,
		CallId1 = sipheader:callid(IHeader),
		{InvCSeq, _} = sipheader:cseq(IHeader),
		CSeqNum1 = list_to_integer(InvCSeq),
		TargetURI1 = (State#state.invite_request)#request.uri,
		[To1] = keylist:fetch('to', IHeader),
		Contact1 = generate_contact_str(),
		Route1 = [],
		[Dst1 | _] = sipdst:url_to_dstlist(TargetURI1, 500, TargetURI1),
		{undefined, CSeqNum1, CallId1, TargetURI1, To1, Contact1, Route1, Dst1};
	    _ ->
		%% We have an existing dialog (created by INVITE we presume)
		IHeader = (State#state.invite_request)#request.header,
		{ok, CSeqNum1, NewDialog1} = sipdialog:get_next_local_cseq(State#state.dialog),
		[C] = contact:parse([NewDialog1#dialog.remote_target]),
		TargetURI1 = sipurl:parse(C#contact.urlstr),
		To1 = set_to_tag(NewDialog1#dialog.remote_tag, sipheader:to(IHeader)),
		[Contact1] = keylist:fetch('contact', IHeader),
		{Route1, Dst1} =
		    case NewDialog1#dialog.route_set of
			[] ->
			    [Dst1_1 | _] = sipdst:url_to_dstlist(TargetURI1, 500, TargetURI1),
			    {[], Dst1_1};
			[FirstRoute | _] = RouteL1 ->
			    [FRC] = contact:parse([FirstRoute]),
			    FRURL = sipurl:parse(FRC#contact.urlstr),
			    [Dst1_1 | _] = sipdst:url_to_dstlist(FRURL, 500, TargetURI1),
			    {[{"Route", RouteL1}], Dst1_1}
		    end,
		{NewDialog1, CSeqNum1, NewDialog1#dialog.callid, TargetURI1, To1, Contact1, Route1, Dst1}
	end,

    [From] = keylist:fetch('from', (State#state.invite_request)#request.header),
    Header = keylist:from_list([{"From",	[From]},
				{"To",		[To]},
				{"Call-Id",	[CallId]},
				{"CSeq",	[lists:concat([CSeqNum, " ", Method])]},
				{"Contact",	[Contact]}
			       ] ++ Route),

    Request1 = #request{method = Method,
			uri    = TargetURI,
			header = Header
		       },
    Request = siprequest:set_request_body(Request1, <<>>),

    {ok, Request, NewDialog, Dst}.


%%--------------------------------------------------------------------
%% @spec    (ToTag, {DisplayName, ToURI}) ->
%%            NewTo
%%
%%            ToTag       = string()
%%            DisplayName = string() | none
%%            ToURI       = #sipurl{}
%%
%%            NewTo = string()
%%
%% @doc     Set tag on a parsed To: header.
%% @end
%%--------------------------------------------------------------------
set_to_tag(ToTag, {DisplayName, ToURI}) when is_list(ToTag) ->
    [NewTo] = sipheader:contact_print(
                [ contact:new(DisplayName, ToURI, [{"tag", ToTag}]) ]),
    NewTo.


%%--------------------------------------------------------------------
%% @spec    (Request, Dst, Branch, Timeout) -> Pid | {error, E}
%%
%%            Request = #request{}
%%            Dst     = #sipdst{}
%%            Branch  = string()
%%            Timeout = integer()
%%
%% @doc     Wrapper for transactionlayer:start_client_transaction/5 to
%%          do some logging first.
%% @end
%%--------------------------------------------------------------------
start_client_transaction(Request, Dst, Branch, Timeout, LogFun) ->
    %% Logging
    BinLine1 = list_to_binary([Request#request.method, " ", sipurl:print(Dst#sipdst.uri), " SIP/2.0"]),
    BinMsg = siprequest:binary_make_message(BinLine1, Request#request.header, <<>>),
    log(LogFun, extra_verbose, "Sending :~n~s", [binary_to_list(BinMsg)]),

    transactionlayer:start_client_transaction(Request, Dst, Branch, Timeout, self()).


%%--------------------------------------------------------------------
%% @spec    (PidList, LogFun) ->
%%            {ok, Msg}
%%
%%            PidList = [pid()]
%%            LogFun  = undefined | function()
%%
%%            Msg = string()
%%
%% @doc     Wait until none of the pids in PidList is alive amymore.
%% @end
%%--------------------------------------------------------------------
wait_for_pids(PidList, LogFun) ->
    case any_alive(PidList) of
	true ->
	    receive
		M ->
		    log(LogFun, error, "DIALOG CONTROLLER ~p GOT SIGNAL ~p", [self(), M])
	    after 1000 ->
		    ok
	    end,
	    wait_for_pids(PidList, LogFun);
	false ->
	    {ok, "All processes terminated"}
    end.

%% part of wait_for_pids/1
%% Returns : true | false
any_alive([H | T]) when is_pid(H) ->
    is_process_alive(H) orelse
	any_alive(T);
any_alive([]) ->
    false.


%%--------------------------------------------------------------------
%% @spec    (LogFun, Level, Format) log(LogFun, Level, Format,
%%          Arguments) -> void()
%%
%%            LogFun    = undefined | function() with arity 3
%%            Level     = debug | normal | error | extra_verbose
%%            Format    = string()
%%            Arguments = [term()]
%%
%% @doc     Either call the function LogFun with the Level, Format and
%%          Arguments as parameters or log it to the console if
%%          LogFun is undefined.
%% @end
%%--------------------------------------------------------------------
log(LogFun, Level, Format) ->
    log(LogFun, Level, Format, []).

log(LogFun, extra_verbose, _Format, _Arguments) when is_function(LogFun) ->
    %% non-standard debug level, only for undefined LogFun
    ok;
log(LogFun, Level, Format, Arguments) when is_function(LogFun) ->
    LogFun(Level, Format, Arguments);
log(undefined, _Level, Format, Arguments) ->
    %% default is to log to console
    io:format(Format, Arguments),
    io:format("~n", []).


%%--------------------------------------------------------------------
%% @spec    (State, Method, Progress) report_progress(State, Method,
%%          Status, Reason) -> void()
%%
%%            State    = #state{}
%%            Method   = string()
%%            Progress = string()
%%            Status   = integer()
%%            Reason   = string()
%%
%% @doc     Report some progress to the State#state.progress_pid if it
%%          is set.
%% @end
%%--------------------------------------------------------------------
report_progress(#state{progress_pid = Pid}, Method, Progress) when is_pid(Pid) ->
    Pid ! {refer_progress, self(), Method, Progress};
report_progress(#state{progress_pid = undefined}, _Method, _Progress) ->
    ok.

report_progress(State, Method, Status, Reason) when is_integer(Status), is_list(Reason) ->
    report_progress(State, Method, lists:concat([Status, " ", Reason])).
