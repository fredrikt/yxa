%%%-------------------------------------------------------------------
%%% File    : sipsocket_blacklist_probe.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Blacklist probe process.
%%%
%%% Created : 23 Feb 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipsocket_blacklist_probe).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/4
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
-record(state, {dst,			%% sipdst record(), destination of our probe
		bl,			%% term(), blacklist ETS table name/reference
		probe_id,		%% term(), id of probe in the ETS table
		timeout,		%% integer(), probe client transaction timeout
		probe_pid		%% pid(), client transaction pid
	       }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start(Dst, EtsRef, ProbeId, Timeout)
%% Descrip.: Starts the server
%%--------------------------------------------------------------------
start(Dst, EtsRef, ProbeId, Timeout) ->
    gen_server:start(?MODULE, [Dst, EtsRef, ProbeId, Timeout], []).


%%====================================================================
%% Behaviour callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([Dst, EtsRef, ProbeId, Timeout])
%%           Dst     = sipdst record()
%%           EtsRef  = term(), ETS table name/reference
%%           ProbeId = term(), id of probe in the ETS table
%%           Timeout = integer(), probe client transaction timeout
%% Descrip.: Initiates the server
%% Returns : {ok, State}
%%--------------------------------------------------------------------
init([Dst, EtsRef, ProbeId, Timeout]) ->
    %% Send myself a signal to start, and then return immediately to
    %% not block the caller
    self() ! start,
    {ok, #state{dst		= Dst,
		bl		= EtsRef,
		probe_id	= ProbeId,
		timeout		= Timeout
	       }
    }.


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

handle_call(Msg, _From, State) ->
    logger:log(error, "Sipsocket blacklist probe: Received unknown gen_server call : ~p", [Msg]),
    {reply, {error, not_implemented}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast(Msg, State) ->
    logger:log(error, "Sipsocket blacklist probe: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_info(start, State) ->
    %% really part of init/1, but done this way to not block the caller
    {ok, Request, UseDst} = start_make_request(State#state.dst, "OPTIONS", <<>>, []),

    BranchBase = siprequest:generate_branch(),
    Branch = lists:concat([BranchBase, "-PROBE-UAC"]),

    Timeout = State#state.timeout,

    Probe = transactionlayer:start_client_transaction(Request, UseDst, Branch, Timeout, self()),

    {noreply, State#state{probe_pid = Probe}};

handle_info({branch_result, Pid, _Branch, _BranchState, Response}, #state{probe_pid = Pid} = State)
  when is_record(Response, response) ->
    %% resulted in an actual response
    logger:log(debug, "Sipsocket blacklist probe: Destination ~p is alive", [sipdst:dst2str(State#state.dst)]),
    sipsocket_blacklist:remove_blacklisting(State#state.dst, State#state.bl),
    {noreply, State};

handle_info({branch_result, Pid, _Branch, _BranchState, {_Status, _Response}}, #state{probe_pid = Pid} = State) ->
    %% ignore created responses
    {noreply, State};

handle_info({clienttransaction_terminating, Pid, _Branch}, #state{probe_pid = Pid} = State) ->
    logger:log(debug, "Sipsocket blacklist probe: Terminating probe for dst ~s", [sipdst:dst2str(State#state.dst)]),
    {stop, normal, State};

handle_info(Msg, State) ->
    logger:log(error, "Sipsocket blacklist probe: Received unknown gen_server info : ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    true = ets:delete(State#state.bl, State#state.probe_id),
    ok.

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

start_make_request(Dst, Method, Body, ExtraHeaders) when is_record(Dst, sipdst) ->
    FromTag = siputil:generate_tag(),
    {Megasec, Sec, Microsec} = now(),

    MyHostname = siprequest:myhostname(),

    CallId = lists:concat(["yxa-blacklist-probe-",
			   Megasec * 1000000 + Sec, "-", Microsec,
			   "@", MyHostname
			  ]),
    CSeq = 1,

    FromURI =
	case Dst#sipdst.proto of
	    tls ->
		sipurl:new([{proto, "sips"}, {host, MyHostname}]);
	    tls6 ->
		sipurl:new([{proto, "sips"}, {host, MyHostname}]);
	    _ ->
		sipurl:new([{host, MyHostname}])
	end,

    FromStr = lists:concat(["\"Reachability probe\" <", sipurl:print(FromURI), ">;tag=", FromTag]),

    ToURI = sipurl:set([{host, Dst#sipdst.addr}, {port, Dst#sipdst.port}], FromURI),
    ToStr = lists:concat(["<", sipurl:print(ToURI), ">"]),

    Header = keylist:from_list([{"From",		[FromStr]},
				{"To",			[ToStr]},
				{"Call-Id",		[CallId]},
				{"CSeq",		[lists:concat([CSeq, " ", Method])]},
				{"Max-Forwards",	["1"]}
			       ] ++ ExtraHeaders),

    URI = sipurl:set([{user, none}, {pass, none}, {param, []}], Dst#sipdst.uri),

    Request1 = #request{method = Method, uri = URI, header = Header},
    Request = siprequest:set_request_body(Request1, Body),
    {ok, Request, Dst#sipdst{uri = URI}}.

