%%%-------------------------------------------------------------------
%%% File    : transactionlayer.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : Transactionlayer 
%%% Created : 05 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(transactionlayer).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("transactionstatelist.hrl").
-include("siprecords.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/2]).

-export([send_response_request/3, send_response_request/4, send_response_request/5,
	 transaction_terminating/1, get_handler_for_request/1,
	 get_branch_from_handler/1, start_client_transaction/6,
	 store_to_tag/2, adopt_server_transaction/1, adopt_server_transaction_handler/1,
	 send_response_handler/3, send_response_handler/4, send_response_handler/5,
	 send_proxy_response_handler/2, get_server_handler_for_stateless_response/1,
	 store_stateless_response_branch/3, is_good_transaction/1,
	 get_pid_from_handler/1, send_challenge_request/4, send_challenge/4,
	 debug_show_transactions/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tstatelist, appmodule, mode}).
-record(thandler, {pid}).

-define(TIMEOUT, 7 * 1000).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/3
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(AppModule, Mode) ->
    gen_server:start_link({local, transaction_layer}, ?MODULE, [AppModule, Mode], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([AppModule, Mode]) ->
    process_flag(trap_exit, true),
    TStateList = transactionstatelist:empty(),
    logger:log(debug, "Transaction layer started"),
    {ok, #state{appmodule=AppModule, mode=Mode, tstatelist=TStateList}}.

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


%% A request has been received by the transaction layer and a SIP message handler is
%% asking us of what to do with it. If we return {continue} the SIP message handler will
%% terminate, but if we return {pass_to_core, Fun} the SIP message handler will pass the
%% request on to that function.
handle_call({sipmessage, Request, Origin, LogStr}, From, State) when record(Request, request), record(Origin, siporigin) ->
    AppModule = State#state.appmodule,
    case get_server_transaction_pid(Request, State#state.tstatelist) of
	none ->
	    received_new_request(Request, Origin#siporigin.sipsocket, LogStr, State);
	TPid when pid(TPid) ->
	    gen_server:cast(TPid, {sipmessage, Request, Origin, From, AppModule}),
	    %% We do noreply here and send AppModule (and gen_server From) on to TPid and let TPid do gen_server:reply()
	    {noreply, State, ?TIMEOUT}
    end;

%% A response has been received by the transaction layer and a SIP message handler is
%% asking us of what to do with it. See comment about requests above.
handle_call({sipmessage, Response, Origin, LogStr}, From, State) when record(Response, response), record(Origin, siporigin) ->
    AppModule = State#state.appmodule,
    case get_client_transaction_pid(Response, State#state.tstatelist) of
	none ->
	    logger:log(debug, "Transaction layer: No state for received response '~p ~s', passing to ~p:response().",
	    		[Response#response.status, Response#response.reason, AppModule]),
	    {reply, {pass_to_core, AppModule}, State, ?TIMEOUT};
	TPid when pid(TPid) ->
	    logger:log(debug, "Transaction layer: Passing response ~p ~s to registered handler ~p",
		       [Response#response.status, Response#response.reason, TPid]),
	    gen_server:cast(TPid, {sipmessage, Response, Origin, LogStr}),
	    {reply, {continue}, State, ?TIMEOUT}
    end;

handle_call({store_stateless_response_branch, Pid, Branch, Method}, From, State) ->
    case transactionstatelist:get_server_transaction_using_stateless_response_branch(Branch, Method, State#state.tstatelist) of
	none ->
	    case transactionstatelist:get_elem_using_pid(Pid, State#state.tstatelist) of
		{error, E} ->
		    logger:log(error, "Transaction layer: Error associating stateless response branch ~p with pid ~p~n: ~p",
			       [Branch, Pid, E]),
		    {reply, {error, E}, State, ?TIMEOUT};
		[] ->
		    logger:log(error, "Transaction layer: Can't associate stateless response branch ~p with unknown transaction handler ~p",
			       [Branch, Pid]),
		    {reply, {error, "server transaction not found"}, State, ?TIMEOUT};
		ThisElem when record(ThisElem, transactionstate) ->
		    NewTElem = transactionstatelist:append_response_branch(ThisElem, Branch, Method),
		    NewL = transactionstatelist:update_transactionstate(NewTElem, State#state.tstatelist),
		    {reply, {ok}, State#state{tstatelist=NewL}, ?TIMEOUT}
	    end;
	ThisElem when record(ThisElem, transactionstate) ->
	    case ThisElem#transactionstate.pid of
		Pid ->
		    %% already associated with that very same server transaction, this is not an error since
		    %% it can happen when a stateless application receives retransmissions of a request
		    {reply, {ok}, State, ?TIMEOUT};
		OtherPid ->
		    logger:log(error, "Transaction layer: Asked to associate branch ~p with transaction with pid ~p, " ++
			       "but branch is already associated with transaction handled by ~p!", [Branch, Pid, OtherPid]),
		    E = "branch already associated with another server transaction",
		    {reply, {error, E}, State, ?TIMEOUT}
		end
    end;	

handle_call({get_server_transaction_handler, Request}, From, State) when record(Request, request) ->
    case get_server_transaction_pid(Request, State#state.tstatelist) of
	none ->
	    {reply, {error, "No state found"}, State, ?TIMEOUT};
	TPid when pid(TPid) ->
	    {reply, {ok, TPid}, State, ?TIMEOUT}
    end;

handle_call({get_server_transaction_handler_for_response, Branch, Method}, From, State) ->
    case transactionstatelist:get_server_transaction_using_stateless_response_branch(Branch, Method, State#state.tstatelist) of
	none ->
	    {reply, {error, nomatch}, State, ?TIMEOUT};
	ThisState when record(ThisState, transactionstate) ->
	    TPid = ThisState#transactionstate.pid,
	    {reply, {ok, TPid}, State, ?TIMEOUT}
    end;

handle_call({start_client_transaction, Request, SocketIn, Dst, Branch, Timeout, Parent}, From, State) when record(Request, request) ->
    Method = Request#request.method,
    case transactionstatelist:get_client_transaction(Method, Branch, State#state.tstatelist) of
	none ->
	    case clienttransaction:start_link(Request, SocketIn, Dst, Branch, Timeout, Parent) of
		{ok, CPid} ->
		    RegBranch = sipheader:remove_loop_cookie(Branch),
		    NewL = transactionstatelist:add_client_transaction(Method, RegBranch, CPid, State#state.tstatelist),
		    {reply, {ok, CPid}, State#state{tstatelist=NewL}, ?TIMEOUT};
		_ ->
		    {reply, {error, "Failed starting client transaction"}, State, ?TIMEOUT}
	    end;
	_ ->
	    logger:log(error, "Transaction layer: Can't start duplicate client transaction"),
	    {reply, {error, "Transaction already exists"}, State, ?TIMEOUT}
    end;

handle_call({store_to_tag, Request, ToTag}, From, State) when record(Request, request) ->
    case get_server_transaction(Request, State#state.tstatelist) of
	none ->
	    {reply, {error, "Transaction not found"}, State, ?TIMEOUT};
	ThisState when record(ThisState, transactionstate) ->
	    NewTState = transactionstatelist:set_response_to_tag(ThisState, ToTag),
	    NewL = transactionstatelist:update_transactionstate(NewTState, State#state.tstatelist),
	    {reply, {ok}, State#state{tstatelist=NewL}, ?TIMEOUT}
	end;

handle_call(Request, From, State) ->
    logger:log(debug, "Transaction layer: Received unknown gen_server call (from ~p) : ~p", [From, Request]),
    {noreply, State, ?TIMEOUT}.



%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({unregister_pid, Pid}, State) ->
    NewL = transactionstatelist:delete_using_pid(Pid, State#state.tstatelist),
    {noreply, State#state{tstatelist=NewL}, ?TIMEOUT};

handle_cast({debug_show_transactions, FromPid}, State) ->
    logger:log(debug, "Transaction layer: Pid ~p asked me to show all ongoing transactions :~n~p",
	       [FromPid, transactionstatelist:debugfriendly(State#state.tstatelist)]),
    {noreply, State, ?TIMEOUT};

handle_cast({quit}, State) ->
    logger:log(debug, "Transaction layer: Received signal to quit"),
    {stop, normal, State};

handle_cast(Msg, State) ->
    logger:log(debug, "Transaction layer: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_info(timeout, State) ->
    NewL = transactionstatelist:delete_expired(State#state.tstatelist),
    {noreply, State#state{tstatelist=NewL}};

handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
	normal -> logger:log(debug, "Transaction layer: Received normal exit-signal from process ~p", [Pid]);
	_ -> logger:log(error, "Transaction layer: =ERROR REPORT==== Received non-normal exit signal from process ~p :~n~p", [Pid, Reason])
    end,
    NewState = case transactionstatelist:get_list_using_pid(Pid, State#state.tstatelist) of
                   none ->
                       logger:log(debug, "Transaction layer: Received exit signal from ~p not in my list. Socketlist is :~n~p",
				  [Pid, transactionstatelist:debugfriendly(State#state.tstatelist)]),
                       State;
                   L when record(L, transactionstatelist) ->
		       NewL = transactionstatelist:delete_using_pid(Pid, State#state.tstatelist),
                       logger:log(debug, "Transaction layer: Deleting ~p entry(s) from transactionlist :~n~p~n(new list is ~p entry(s))",
				  [transactionstatelist:get_length(L), transactionstatelist:debugfriendly(L), transactionstatelist:get_length(NewL)]),
		       %%logger:log(debug, "Transaction layer: Extra debug: Transactionlist is now :~n~p", [transactionstatelist:debugfriendly(NewL)]),
		       State#state{tstatelist=NewL};
		   Unknown ->
		       logger:log(error, "Transaction layer: Unknown result returned from get_list_using_pid :~n~p",
				  [Unknown]),
		       State
               end,
    {noreply, NewState, ?TIMEOUT};

handle_info(Msg, State) ->
    logger:log(error, "Transaction layer: Received unknown gen_server info :~n~p", [Msg]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(normal, State) ->
    logger:log(debug, "Transaction layer: Terminating normally"),
    ok;
terminate(Reason, State) ->
    logger:log(error, "Transaction layer: =ERROR REPORT==== from transaction_layer :~n~p",
	       [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%
%% ACK
%%
received_new_request(Request, Socket, LogStr, State) when record(State, state), record(Request, request), Request#request.method == "ACK" ->
    AppModule = State#state.appmodule,
    logger:log(debug, "Transaction layer: Received ACK ~s that does not match any existing transaction, passing to core.",
		[sipurl:print(Request#request.uri)]),
    {reply, {pass_to_core, AppModule}, State, ?TIMEOUT};

%%
%% CANCEL, our mode == stateless
%%
received_new_request(Request, Socket, LogStr, State) when record(State, state), record(Request, request), State#state.mode == stateless, Request#request.method == "CANCEL" ->
    AppModule = State#state.appmodule,
    logger:log(debug, "Transaction layer: Stateless received CANCEL ~s. Starting transaction but passing to core.",
		[sipurl:print(Request#request.uri)]),
    case servertransaction:start_link(Request, Socket, LogStr, AppModule, stateless) of
	{ok, STPid} when pid(STPid) ->
	    NewL = transactionstatelist:add_server_transaction(Request, STPid, State#state.tstatelist),
	    {reply, {pass_to_core, AppModule}, State#state{tstatelist=NewL}, ?TIMEOUT};
	E ->
	    logger:log(error, "Transaction layer: Failed starting server transaction (~p) - ignoring request", [E]),
	    {reply, {continue}, State, ?TIMEOUT}
    end;

received_new_request(Request, Socket, LogStr, State) when record(State, state), record(Request, request) ->
    AppModule = State#state.appmodule,
    logger:log(debug, "Transaction layer: No state for received request, starting new transaction"),
    case servertransaction:start_link(Request, Socket, LogStr, AppModule, State#state.mode) of
	{ok, STPid} when pid(STPid) ->
	    NewTStateList2 = transactionstatelist:add_server_transaction(Request, STPid, State#state.tstatelist),
	    PassToCore = case Request#request.method of
			     "CANCEL" ->
				 Header = Request#request.header,
				 %% XXX not only INVITE can be cancelled, RFC3261 9.2 says we should find the
				 %% transaction that is being handled by 'assuming the method is anything but
				 %% CANCEL or ACK'.
				 {CSeqNum, _} = sipheader:cseq(keylist:fetch("CSeq", Header)),
				 %% When looking for the corresponding INVITE transaction, we have to change the
				 %% CSeq method of this header to INVITE, in case we received it from a RFC2543 client
				 %% (RFC2543 backwards-compatible transaction matching includes the whole CSeq, this is
				 %% probably an error in RFC3261 #9.2 that refers to #17.2.3 which does not say that
				 %% CANCEL matches a transaction even though the CSeq method differs)
				 IHeader = keylist:set("CSeq", [sipheader:cseq_print({CSeqNum, "INVITE"})], Header),
				 Invite = Request#request{method="INVITE", header=IHeader},
				 case get_server_transaction_pid(Invite, State#state.tstatelist) of
				     InvitePid when pid(InvitePid) ->
					 logger:log(debug, "Transaction layer: CANCEL matches server transaction handled by ~p", [InvitePid]),
					 {Status, Reason} = case util:safe_is_process_alive(InvitePid) of
								{true, _} ->
								    logger:log(debug, "Transaction layer: Cancelling other transaction handled by ~p " ++
									       "and responding 200 Ok", [InvitePid]),
								    gen_server:cast(InvitePid, {cancelled}),
								    {200, "Ok"};
								_ ->
								    logger:log(debug, "Transaction layer: Transaction to be cancelled handled by dead " ++
									       "pid '~p', responding 481 Call/Transaction Does Not Exist", [InvitePid]),
								    {481, "Call/Transaction Does Not Exist"}
							    end,	
					 gen_server:cast(STPid, {create_response, Status, Reason, [], ""}),
					 false;
				     _ ->
					 logger:log(debug, "Transaction layer: Could not find transaction for CANCEL, pass it on to application core."),
					 true
				 end;
			     _ ->
				 true
			 end,
	    case PassToCore of
		true ->
		    logger:log(debug, "Transaction layer: Telling sipserver to apply this applications request function."),
		    {reply, {pass_to_core, AppModule}, State#state{tstatelist=NewTStateList2}, ?TIMEOUT};
		_ ->
		    {reply, {continue}, State#state{tstatelist=NewTStateList2}, ?TIMEOUT}
	    end;
	E ->
	    logger:log(error, "Transaction layer: Failed starting server transaction (~p) - ignoring request", [E]),
	    {reply, {continue}, State, ?TIMEOUT}
    end.

get_server_transaction(Request, TStateList) when record(Request, request) ->
    transactionstatelist:get_server_transaction_using_request(Request, TStateList);
get_server_transaction(Response, TStateList) when record(Response, response) ->
    transactionstatelist:get_server_transaction_using_response(Response, TStateList).

get_server_transaction_pid(Re, TStateList) when record(Re, request); record(Re, response) ->
    case get_server_transaction(Re, TStateList) of
	none ->
	    none;
	TState when record(TState, transactionstate) ->
	    case transactionstatelist:extract([pid], TState) of
		[TPid] when pid(TPid) ->
		    TPid;
		_ ->
		    none
	    end;
	Unknown ->
	    logger:log(error, "Transaction layer: Could not get server transaction - get_server_transaction returned unknown result : ~p",
		       [Unknown]),
	    logger:log(debug, "Transaction layer: Request or response passed to get_server_transaction was :~n~p~n~n",
		       [Re]),
	    none
    end.

get_client_transaction(Response, TStateList) when record(Response, response) ->
    Header = Response#response.header,
    TopVia = sipheader:topvia(Header),
    Branch = sipheader:get_via_branch(TopVia),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    transactionstatelist:get_client_transaction(Method, Branch, TStateList).

get_client_transaction_pid(Response, TStateList) when record(Response, response) ->
    case get_client_transaction(Response, TStateList) of
	none ->
	    none;	
	TState ->
	    case transactionstatelist:extract([pid], TState) of
		[TPid] when pid(TPid) ->
		    TPid;
		_ ->
		    none	    
	    end
    end.


%%--------------------------------------------------------------------
%%% Interface functions
%%--------------------------------------------------------------------

send_response_request(Request, Status, Reason) when record(Request, request) ->
    send_response_request(Request, Status, Reason, []).

send_response_request(Request, Status, Reason, ExtraHeaders) when record(Request, request) ->
    send_response_request(Request, Status, Reason, ExtraHeaders, "").

send_response_request(Request, Status, Reason, ExtraHeaders, RBody) when record(Request, request) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    case get_handler_for_request(Request) of
	{error, E} ->
	    logger:log(error, "Transaction layer: Failed locating server transaction handler for request ~s ~s : ~p",
			[Method, sipurl:print(URI), E]),
	    none;
	none ->
	    logger:log(error, "Transaction layer: No server transaction found for request ~s ~s",
			[Method, sipurl:print(URI)]),
	    none;
	TH when record(TH, thandler) ->
	    logger:log(debug, "Transaction layer: Located server transaction handler ~p", [TH#thandler.pid]),
	    send_response_handler(TH, Status, Reason, ExtraHeaders, RBody)
    end.

send_response_handler(TH, Status, Reason) when record(TH, thandler) ->
    send_response_handler(TH, Status, Reason, []).

send_response_handler(TH, Status, Reason, ExtraHeaders) when record(TH, thandler) ->
    send_response_handler(TH, Status, Reason, ExtraHeaders, "").

send_response_handler(TH, Status, Reason, ExtraHeaders, RBody) when record(TH, thandler) ->
    case catch gen_server:cast(TH#thandler.pid, {create_response, Status, Reason, ExtraHeaders, RBody}) of
	ok ->
	    ok;
	_ ->
	    {error, "Transaction handler failure"}
    end.

send_proxy_response_handler(TH, Response) when record(TH, thandler), record(Response, response) ->
    case gen_server:cast(TH#thandler.pid, {forwardresponse, Response}) of
	ok ->
	    ok;
	_ ->
	    {error, "Transaction handler failure"}
    end.

get_handler_for_request(Request) ->
    case catch gen_server:call(transaction_layer, {get_server_transaction_handler, Request}, 2500) of
        {error, E} ->
	    {error, E};
        {ok, Pid} ->
	    #thandler{pid=Pid};
	_ ->
	    {error, "Transaction layer failed"}
    end.

get_server_handler_for_stateless_response(Response) when record(Response, response) ->
    TopVia = sipheader:topvia(Response#response.header),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Response#response.header)),
    case sipheader:get_via_branch(TopVia) of
	Branch when list(Branch) ->
	    case catch gen_server:call(transaction_layer, {get_server_transaction_handler_for_response, Branch, Method}, 1000) of
		{error, nomatch} ->
		    none;
		{error, E} ->
		    {error, E};
		{ok, Pid} ->
		    #thandler{pid=Pid};
		_ ->
		    {error, "Transaction layer failed"}
	    end;
	_ ->
	    {error, "No branch in top via of response"}
    end.

adopt_server_transaction(Request) when record(Request, request) ->
    case get_handler_for_request(Request) of
	TH when record(TH, thandler) -> 
	    adopt_server_transaction_handler(TH);
	Unknown ->
	   {error, "unknown result from get_handler_for_request"}
    end.

adopt_server_transaction_handler(TH) when record(TH, thandler) ->
    case catch gen_server:call(TH#thandler.pid, {set_report_to, self()}, 1000) of
	{error, E} ->
	    {error, E};
	{ok} ->
	    TH;
	_ ->
	    {error, "Server transaction handler failed"}
    end.

get_branch_from_handler(TH) when record(TH, thandler) ->
    TPid = TH#thandler.pid,
    case catch gen_server:call(TPid, {get_branch}, 500) of
	{ok, Branch} ->
	    Branch;
	_ ->
	    logger:log(error, "Transaction layer: Failed getting branch from pid ~p", [TPid]),
	    error
    end.

transaction_terminating(TransactionPid) ->
    gen_server:cast(transaction_layer, {unregister_pid, TransactionPid}),
    ok.

start_client_transaction(Request, SocketIn, Dst, Branch, Timeout, Parent) ->
    %% XXX we probably want a shorter timeout on this, but since we send out the initial request
    %% in clienttransaction:init() it can take some time (if a TCP connection has to be established
    %% first etc).
    case catch gen_server:call(transaction_layer, {start_client_transaction, Request, SocketIn, Dst, Branch, Timeout, Parent}, 10000) of
	{error, E} ->
	    {error, E};
	{ok, Pid} ->
	    Pid;
	Unknown ->
	    logger:log(debug, "Transaction layer: Failed starting client transaction, gen_server call result :~n~p",
		       [Unknown]),
	    {error, "Transaction layer failure"}
    end.

store_to_tag(Request, ToTag) when record(Request, request) ->
    case catch gen_server:call(transaction_layer, {store_to_tag, Request, ToTag}, 2500) of
	{error, E} ->
	    {error, E};
	{ok} ->
	    ok;
	Unknown ->
	    logger:log(debug, "Transaction layer: Failed storing To: tag, gen_server call result :~n~p",
		       [Unknown]),
	    {error, "Transaction layer failure"}
    end.

store_stateless_response_branch(none, Branch, Method) ->
    ok;
store_stateless_response_branch(TH, Branch, Method) when record(TH, thandler) ->
    HPid = TH#thandler.pid,
    case catch gen_server:call(transaction_layer, {store_stateless_response_branch, HPid, Branch, Method}, 1000) of
	{ok} ->
	    ok;
	{error, E} ->
	    {error, E};
	Unknown ->
	    logger:log(debug, "Transaction layer: Failed storing stateless response branch, gen_server call result :~n~p",
		       [Unknown]),
	    {error, "Transaction layer failure"}
    end.

is_good_transaction(TH) when record(TH, thandler) ->
    case util:safe_is_process_alive(TH#thandler.pid) of
	{true, _} ->
	    true;
	_ ->
	    false
    end;
is_good_transaction(_) ->
    false.

get_pid_from_handler(TH) when record(TH, thandler) ->
    TH#thandler.pid;
get_pid_from_handler(_) ->
    none.

send_challenge_request(Request, Type, Stale, RetryAfter) ->
    TH = transactionlayer:get_handler_for_request(Request),
    send_challenge(TH, Type, Stale, RetryAfter).

send_challenge(TH, www, Stale, RetryAfter) when record(TH, thandler) ->
    AuthHeader = [{"WWW-Authenticate", sipheader:auth_print(sipauth:get_challenge(), Stale)}],
    send_challenge2(TH, 401, "Authentication Required", AuthHeader, RetryAfter);
send_challenge(TH, proxy, Stale, RetryAfter) when record(TH, thandler) ->
    AuthHeader = [{"Proxy-Authenticate", sipheader:auth_print(sipauth:get_challenge(), Stale)}],
    send_challenge2(TH, 407, "Proxy Authentication Required", AuthHeader, RetryAfter).

send_challenge2(TH, Status, Reason, AuthHeader, RetryAfter) when record(TH, thandler)->
    RetryHeader = case RetryAfter of
		      I when integer(I) ->
			  [{"Retry-After", [integer_to_list(RetryAfter)]}];
		      _ ->
			  []
		  end,
    ExtraHeaders = lists:append(AuthHeader, RetryHeader),
    send_response_handler(TH, Status, Reason, ExtraHeaders),
    case sipserver:get_env(stateless_challenges, false) of
	true ->
	    %% Adhere to advice in RFC3261 #26.3.2.4 (DoS Protection) saying we SHOULD terminate
	    %% the server transaction immediately after sending a challenge, to preserve server
	    %% resources but also to not resend these and thus be more usefull to someone using
	    %% us to perform a DoS on a third party.
	    %%
	    %% Default is to NOT do this, since we will have sent a 100 Trying in response to
	    %% an INVITE (and the DoS thingy only applies to INVITE transactions, since we wouldn't
	    %% send a 401/407 response to a non-INVITE reliably), and if it is a legitimate INVITE
	    %% and the client receives the 100 but this 401/407 is lost then we have screwed up
	    %% badly. The client would wait until it times out, instead of resubmitting the request.
	    gen_server:cast(TH#thandler.pid, {quit}),
	    ok;
	_ ->
	    true
    end,
    ok.

debug_show_transactions() ->
    gen_server:cast(transaction_layer, {debug_show_transactions, self()}),
    ok.
