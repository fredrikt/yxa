%%%-------------------------------------------------------------------
%%% File    : register_client.erl
%%% @author   Fredrik Thulin <fredrik@thulin.net>
%%% @doc      REGISTER with some third party registrar.
%%%
%%%           This module was developed under contract with NORDUnet.
%%%           It is not to be considered complete yet - it lacks
%%%           handling of Proxy-Authentication, and does not really
%%%           check that the challenge received is from the domain
%%%           that matches the credentials.
%%%
%%% Usage :
%%%
%%%   register_client:start("local-number", "sip:your-number@your-provider", "user", "secret").
%%%
%%% @since    14 Feb 2013 by Fredrik Thulin <fredrik@thulin.net>
%%% @end
%%% @private
%%%
%%%-------------------------------------------------------------------
-module(register_client).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/3,
	 start/7
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
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {aor,			%% string(), AoR is address-of-record (your SIP URI)
		username,		%% string(), authentication username
		password,		%% string(), authentication password
		dstlist,		%% [sipdst record()], destinations to try
		dst,			%% sipdst record(), the current destination
		timeout,		%% integer(), probe client transaction timeout
		probe_pid,		%% pid(), client transaction pid
		old_pid,		%% pid(), previous client transaction pid when we are challenged
		cseq,			%% integer(), last CSeq used
		request,		%% request record()
		expire,			%% integer(), REGISTER Expire time in seconds
		rereg_interval,		%% integer(), number of seconds before re-registration
		rereg_tref,		%% ref(), re-registration timer reference
		contact			%% contact record(), the REGISTER Contact header value
	       }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DEFAULT_EXPIRE, 1800).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Dst, Timeout) -> term()
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start(LocalPart, Aor, Username, Password) when is_list(Aor), is_list(Username), is_list(Password) ->
    DstList = sipdst:url_to_dstlist(sipurl:parse(Aor), 500),  %% XXX figure out real size
    ContactURL = sipurl:new([{proto, "sip"},
			     {user, LocalPart},
			     {host, siphost:myip()},
			     {port, sipsocket:get_listenport(udp)}]),
    Contact = contact:new(ContactURL),
    start(Aor, Username, Password, DstList, 60, ?DEFAULT_EXPIRE, Contact).

start(Aor, Username, Password, DstList, Timeout, Expire, Contact) ->
    State =
	#state{aor		= Aor,
	       username		= Username,
	       password		= Password,
	       dstlist		= DstList,
	       timeout		= Timeout,
	       expire		= Expire,
	       rereg_interval	= trunc(Expire / 2),
	       cseq		= 0,
	       contact		= Contact
	      },
    gen_server:start({local, ?MODULE}, ?MODULE, State, []).


%%====================================================================
%% Behaviour callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (State) -> {ok, State}
%%
%%            State = state record() "initial gen_server state"
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(State) when is_record(State, state) ->
    %% Send myself a signal to start, and then return immediately to
    %% not block the caller
    self() ! start,
    {ok, State}.

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

handle_call(re_register, From, State) ->
    logger:log(normal, "Register client: Re-register on request from ~p", [From]),
    {reply, ok, update_reregister(0, State)};

handle_call({set_re_register_interval, Interval}, From, State) when is_integer(Interval) ->
    logger:log(normal, "Register client: Changing re-registration interval from ~p to ~p seconds",
	       [State#state.rereg_interval, Interval]),
    %% XXX maybe we ought to verify that Interval < Expires
    {reply, ok, update_reregister(Interval, State)};

%%--------------------------------------------------------------------
%% @spec    (Unknown, From, State) ->
%%            {reply, {error, not_implemented}, State, Timeout::integer()}
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "Register client: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, not_implemented}, State}.


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
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Unknown cast.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "Register client: Received unknown gen_server cast : ~p", [Unknown]),
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

handle_info(start, State) ->
    %% really part of init/1, but done this way to not block the caller
    FirstDst = hd(State#state.dstlist),
    {ok, Request, UseDst} =
	start_make_register_request(FirstDst, sipurl:parse(State#state.aor),
				    State#state.expire,
				    State#state.contact, <<>>, []),

    UpdateHeaders = [],
    NewState = start_register_client(Request, UpdateHeaders, State#state{dst = UseDst}),

    {noreply, NewState};

handle_info({branch_result, Pid, _Branch, _BranchState, #response{status = 401} = Response},
	    #state{probe_pid = Pid, old_pid = undefined} = State) ->
    %% The last REGISTER resulted in an authorization request
    logger:log(debug, "Register client: Destination ~p requested authorization", [sipdst:dst2str(State#state.dst)]),

    [WWW | _] = keylist:fetch('www-authenticate', Response#response.header),
    Auth = sipheader:auth(WWW),
    AuthResponse = get_auth_response('www-authenticate', Auth, State),
    UpdateHeaders = [{"Authorization", [AuthResponse]}],

    NewState = start_register_client(State#state.request, UpdateHeaders, State),
    {noreply, NewState};

handle_info({branch_result, Pid, _Branch, _BranchState, #response{status = 200} = Response},
	    #state{probe_pid = Pid} = State) ->
    logger:log(normal, "Register client: Authentication with ~p successful", [sipdst:dst2str(State#state.dst)]),
    {noreply, update_reregister(State#state.rereg_interval, State)};

handle_info({branch_result, Pid, _Branch, _BranchState, Response}, #state{probe_pid = Pid} = State)
  when is_record(Response, response) ->
    logger:log(normal, "Register client: Authentication with ~p failed", [sipdst:dst2str(State#state.dst)]),
    {noreply, State};

handle_info({branch_result, Pid, _Branch, _BranchState, {_Status, _Response}}, #state{probe_pid = Pid} = State) ->
    %% ignore created responses
    {noreply, State};

handle_info({clienttransaction_terminating, Pid, _Branch}, #state{old_pid = Pid} = State) ->
    logger:log(debug, "Register client: Terminating old pid (dst ~s)", [sipdst:dst2str(State#state.dst)]),
    {noreply, State#state{old_pid = undefined}};

handle_info({clienttransaction_terminating, Pid, _Branch}, #state{probe_pid = Pid} = State) ->
    logger:log(debug, "Register client: Terminating (dst ~s)", [sipdst:dst2str(State#state.dst)]),
    {noreply, State#state{probe_pid = undefined}};

handle_info({re_register, ReRegisterTime}, #state{old_pid = OPid, probe_pid = PPid} = State) when is_pid(OPid) ; is_pid(PPid) ->
    %% safety measure to try and not spin out of control
    logger:log(debug, "Register client: Old pid (~p) or probe pid (~p) is not undefined, postponing re-registration 5 seconds"),
    NewState = update_reregister(5, State),
    {noreply, NewState};
handle_info({re_register, ReRegisterTime}, State) ->
    logger:log(debug, "Register client: Re-registering after ~p seconds", [ReRegisterTime]),

    OldRequest = State#state.request,
    CSeq = State#state.cseq + 1,
    NewHeader1 = OldRequest#request.header,
    NewHeader2 = keylist:set("CSeq", [sipheader:cseq_print({integer_to_list(CSeq), OldRequest#request.method})], NewHeader1),
    NewRequest = OldRequest#request{header = NewHeader2},

    BranchBase = siprequest:generate_branch(),
    Branch = lists:concat([BranchBase, "-YXA-UAC"]),

    Timeout = State#state.timeout,

    Probe = transactionlayer:start_client_transaction(NewRequest, State#state.dst, Branch, Timeout, self()),

    {noreply, State#state{probe_pid	= Probe,
			  request	= NewRequest,
			  cseq		= CSeq
			 }};

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Unknown info.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(Unknown, State) ->
    logger:log(error, "Register client: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ok.

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

start_register_client(Request, UpdateHeaders, State)
  when is_record(Request, request), is_list(UpdateHeaders), is_record(State, state) ->
    CSeq = State#state.cseq + 1,
    CSeqStr = sipheader:cseq_print({integer_to_list(CSeq), Request#request.method}),
    UpdateHeaders1 = [{"CSeq", [CSeqStr]} | UpdateHeaders],
    NewHeaders = update_header(Request#request.header, UpdateHeaders1),
    NewRequest = Request#request{header = NewHeaders},

    BranchBase = siprequest:generate_branch(),
    Branch = lists:concat([BranchBase, "-YXA-UAC"]),

    Timeout = State#state.timeout,

    %% XXX use sippipe and full dstlist?
    Probe = transactionlayer:start_client_transaction(NewRequest, State#state.dst, Branch, Timeout, self()),

    State#state{probe_pid	= Probe,
		old_pid		= State#state.probe_pid,
		request		= NewRequest,
		cseq		= CSeq
	       }.

update_header(Header, []) ->
    Header;
update_header(Header, [{Key, Val} | T]) ->
    update_header(keylist:set(Key, Val, Header), T).

update_reregister(Interval, #state{rereg_tref = undefined} = State) ->
    logger:log(debug, "Register client: Scheduling re-registration in ~p seconds", [Interval]),
    {ok, TRef} = timer:send_after(Interval * 1000, {re_register, Interval}),
    State#state{rereg_interval = Interval,
		rereg_tref = TRef
	       };
update_reregister(New, State) ->
    %% rereg_tref was not undefined, cancel old timer first
    {ok, cancel} = timer:cancel(State#state.rereg_tref),
    update_reregister(New, State#state{rereg_tref = undefined}).

get_auth_response(Type, Auth, State) when is_atom(Type) ->
    Request = State#state.request,
    URIstr = sipurl:print(Request#request.uri),
    Nonce = dict:fetch("nonce", Auth),
    Opaque = case dict:find("opaque", Auth) of
		 error -> "";
		 {ok, Val} -> Val
	     end,
    Realm = dict:fetch("realm", Auth),
    Res = sipauth:get_response(Nonce, Request#request.method, URIstr, State#state.username, State#state.password, Realm),

    sipauth:print_auth_response("Digest", State#state.username, Realm, URIstr, Res, Nonce, Opaque, "MD5").

start_make_register_request(Dst, Aor, Expire, Contact, Body, ExtraHeaders) when is_record(Dst, sipdst) ->
    Method = "REGISTER",
    FromTag = siputil:generate_tag(),
    {Megasec, Sec, Microsec} = now(),

    MyHostname = siprequest:myhostname(),

    CallId = lists:concat(["yxa-client-",
			   Megasec * 1000000 + Sec, "-", Microsec,
			   "@", MyHostname
			  ]),

    FromURI = Aor,
    FromStr = lists:concat(["\"YXA register client\" <", sipurl:print(FromURI), ">;tag=", FromTag]),

    ToURI = Aor,
    ToStr = lists:concat(["<", sipurl:print(ToURI), ">"]),

    Header = keylist:from_list([{"From",		[FromStr]},
				{"To",			[ToStr]},
				{"Call-Id",		[CallId]},
				{"Max-Forwards",	["1"]},
				{"Contact",		[contact:print(Contact)]},
				{"Expires",		[integer_to_list(Expire)]}
			       ] ++ ExtraHeaders),

    URI = sipurl:set([{user, none}, {pass, none}, {param, []}], Dst#sipdst.uri),

    Request1 = #request{method = Method, uri = URI, header = Header},
    Request = siprequest:set_request_body(Request1, Body),
    {ok, Request, Dst#sipdst{uri = URI}}.
