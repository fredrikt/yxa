%%%-------------------------------------------------------------------
%%% File    : transactionlayer.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Transactionlayer
%%% Created : 05 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(transactionlayer).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/2,
	 send_response_request/3,
	 send_response_request/4,
	 send_response_request/5,
	 transaction_terminating/1,
	 get_handler_for_request/1,
	 get_branch_from_handler/1,
	 start_client_transaction/6,
	 adopt_server_transaction/1,
	 adopt_server_transaction_handler/1,
	 send_response_handler/3,
	 send_response_handler/4,
	 send_proxy_response_handler/2,
	 get_server_handler_for_stateless_response/1,
	 is_good_transaction/1,
	 get_pid_from_handler/1,
	 send_challenge_request/4,
	 send_challenge/4,
	 debug_show_transactions/0
	]).

%%--------------------------------------------------------------------
%% Transport layer internal exports (functions that should only be
%% invoked from the transport layer).
%%--------------------------------------------------------------------
-export([
	 store_stateless_response_branch/3
	]).

%%--------------------------------------------------------------------
%% Transaction layer internal exports (functions that should only be
%% invoked from the transaction layer).
%%--------------------------------------------------------------------
-export([
	 store_to_tag/2
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
-include("transactionstatelist.hrl").
-include("siprecords.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% My State
-record(state, {
	  tstatelist,	% The transaction layers list of transactions
	  appmodule,	% Which Yxa application this is
	  mode		% stateful | stateless
	 }).

%% A container record to make it clear that all communication with
%% the transaction processes should go through this module.
-record(thandler, {
	  pid
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
%% Wake up every seven seconds to remove expired transactions from
%% State#state.tstatelist.
-define(TIMEOUT, 7 * 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link(AppModule, Mode)
%%           AppModule = atom(), name of Yxa application
%%           Mode      = stateful | stateless
%% Descrip.: start the transaction_layer gen_server.
%%           The transaction_layer is only registered localy (on the
%%           current node)
%% Returns : gen_server:start_link/4
%%--------------------------------------------------------------------
start_link(AppModule, Mode) ->
    gen_server:start_link({local, transaction_layer}, ?MODULE, [AppModule, Mode], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([AppModule, Mode])
%%           AppModule = atom(), name of Yxa application
%%           Mode      = stateful | stateless
%% Descrip.: Initiates the server
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([AppModule, Mode]) ->
    process_flag(trap_exit, true),
    TStateList = transactionstatelist:empty(),
    logger:log(debug, "Transaction layer started"),
    {ok, #state{appmodule=AppModule, mode=Mode, tstatelist=TStateList}, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages.
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%%           Msg     = {sipmessage, Request, Origin, LogStr}
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogStr  = string()
%% Descrip.: A SIP request has been received by the transport layer.
%%           Check to see if we have a matching transaction (i.e.
%%           this is a resend) or if we need to start a new
%%           transaction.
%% Returns : {reply, Reply, State, ?TIMEOUT}       |
%%           {noreply, State, ?TIMEOUT}
%%           Reply     = {continue}                |
%%                       {pass_to_core, AppModule}
%%           AppModule = atom()
%%
%% Notes   : This handle_call() is a bit special in that it might pass
%%           the From term() on to someone else (an existing server
%%           transaction), and let that someone else do
%%           gen_server:reply().
%%
%%           See documentation of received_new_request() for info
%%           about the meaning of Reply.
%%--------------------------------------------------------------------
handle_call({sipmessage, Request, Origin, LogStr}, From, State)
  when is_record(Request, request), is_record(Origin, siporigin) ->
    AppModule = State#state.appmodule,
    case get_server_transaction_pid(Request, State#state.tstatelist) of
	none ->
	    received_new_request(Request, Origin#siporigin.sipsocket, LogStr, State);
	TPid when is_pid(TPid) ->
	    gen_server:cast(TPid, {sipmessage, Request, Origin, From, AppModule}),
	    %% We do noreply here and send AppModule (and gen_server From) on to TPid and let TPid do gen_server:reply()
	    {noreply, State, ?TIMEOUT}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%%           Msg     = {sipmessage, Request, Origin, LogStr}
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogStr  = string()
%% Descrip.: A SIP response has been received by the transport layer.
%%           Check to see if we have a matching transaction (i.e.
%%           this is a resend) or if we need to start a new
%%           transaction.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply     = {continue}                |
%%                       {pass_to_core, AppModule}
%%           AppModule = atom()
%%
%% Notes   : This handle_call() is a bit special in the same way as
%%           the {sipmessage, Request ...} handle_call() above. See
%%           it's documentation to understand Reply.
%%--------------------------------------------------------------------
handle_call({sipmessage, Response, Origin, LogStr}, _From, State)
  when is_record(Response, response), is_record(Origin, siporigin) ->
    AppModule = State#state.appmodule,
    case get_client_transaction_pid(Response, State#state.tstatelist) of
	none ->
	    logger:log(debug, "Transaction layer: No state for received response '~p ~s', passing to ~p:response().",
		       [Response#response.status, Response#response.reason, AppModule]),
	    {reply, {pass_to_core, AppModule}, State, ?TIMEOUT};
	TPid when is_pid(TPid) ->
	    logger:log(debug, "Transaction layer: Passing response ~p ~s to registered handler ~p",
		       [Response#response.status, Response#response.reason, TPid]),
	    gen_server:cast(TPid, {sipmessage, Response, Origin, LogStr}),
	    {reply, {continue}, State, ?TIMEOUT}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%%           Msg     = {store_stateless_response_branch, Pid, Branch,
%%                      Method}
%%           Pid     = pid()
%%           Branch  = string()
%%           Method  = string()
%% Descrip.: When sending out requests statelessly, the transport
%%           layer (in our implementation) knows what server
%%           transaction the request originally arrived on. To make it
%%           possible to know exactly on which socket we should send
%%           out any responses we receive to this stateless request
%%           we must associate the branch we used in the request we
%%           sent out, with the server transaction of the original
%%           request.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply     = {ok}               |
%%                       {error, E}
%%           E         = string(), description of error
%%--------------------------------------------------------------------
handle_call({store_stateless_response_branch, Pid, Branch, Method}, _From, State)
  when is_pid(Pid), is_list(Branch), is_list(Method) ->
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
		ThisElem when is_record(ThisElem, transactionstate) ->
		    NewTElem = transactionstatelist:append_response_branch(ThisElem, Branch, Method),
		    NewL = transactionstatelist:update_transactionstate(NewTElem, State#state.tstatelist),
		    {reply, {ok}, State#state{tstatelist=NewL}, ?TIMEOUT}
	    end;
	ThisElem when is_record(ThisElem, transactionstate) ->
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

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%%           Msg     = {get_server_transaction_handler, Request}
%%           Request = request record()
%% Descrip.: Get the pid of a server transaction using a Request.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply     = {ok, Pid}          |
%%                       {error, E}
%%           Pid       = pid()
%%           E         = string(), description of error
%%--------------------------------------------------------------------
handle_call({get_server_transaction_handler, Request}, _From, State) when is_record(Request, request) ->
    case get_server_transaction_pid(Request, State#state.tstatelist) of
	none ->
	    {reply, {error, "No state found"}, State, ?TIMEOUT};
	TPid when is_pid(TPid) ->
	    {reply, {ok, TPid}, State, ?TIMEOUT}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%%           Msg     = {get_server_transaction_handler_for_response,
%%                      Branch, Method}
%%           Branch  = string()
%%           Method  = string()
%% Descrip.: Get the pid of a server transaction using properties from
%%           a received response.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply     = {ok, Pid}          |
%%                       {error, nomatch}
%%           Pid       = pid()
%%--------------------------------------------------------------------
handle_call({get_server_transaction_handler_for_response, Branch, Method}, _From, State)
  when is_list(Branch), is_list(Method) ->
    case transactionstatelist:get_server_transaction_using_stateless_response_branch(Branch, Method, State#state.tstatelist) of
	none ->
	    {reply, {error, nomatch}, State, ?TIMEOUT};
	ThisState when is_record(ThisState, transactionstate) ->
	    TPid = ThisState#transactionstate.pid,
	    {reply, {ok, TPid}, State, ?TIMEOUT}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%%           Msg      = {start_client_transaction, Request, SocketIn,
%%                      Dst, Branch, Timeout, Parent}
%%           Request  = request record()
%%           SocketIn = sipsocket record(), the socket the request
%%                                          arrived on
%%           Dst      = sipdst record(), the destination for this
%%                                       client transaction
%%           Branch   = string()
%%           Timeout  =
%%           Parent   = pid(), the process that initiated this client
%% Descrip.: Start a new client transaction.
%% Returns : {reply, Reply, State, Timeout}
%%           Reply     = {ok, Pid}          |
%%                       {error, E}
%%           Pid       = pid()
%%           E         = string(), description of error
%%--------------------------------------------------------------------
handle_call({start_client_transaction, Request, SocketIn, Dst, Branch, Timeout, Parent}, _From, State)
  when is_record(Request, request) ->
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

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%%           Msg      = {store_to_tag, Request, ToTag}
%%           Request  = request record()
%%           ToTag    = string()
%% Descrip.: Store the to-tag we use when sending non-2xx responses in
%%           INVITE server transactions. We need to do this to
%%           correctly match ACK to the server transaction.
%% Returns : {reply, Reply, State, Timeout} |
%%           Reply     = {ok}               |
%%                       {error, E}
%%           E         = string(), description of error
%%--------------------------------------------------------------------
handle_call({store_to_tag, Request, ToTag}, _From, State) when is_record(Request, request) ->
    case get_server_transaction(Request, State#state.tstatelist) of
	none ->
	    {reply, {error, "Transaction not found"}, State, ?TIMEOUT};
	ThisState when is_record(ThisState, transactionstate) ->
	    NewTState = transactionstatelist:set_response_to_tag(ThisState, ToTag),
	    NewL = transactionstatelist:update_transactionstate(NewTState, State#state.tstatelist),
	    {reply, {ok}, State#state{tstatelist=NewL}, ?TIMEOUT}
    end;

handle_call(Request, From, State) ->
    logger:log(debug, "Transaction layer: Received unknown gen_server call (from ~p) : ~p", [From, Request]),
    {noreply, State, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_cast({unregister_pid, Pid}, State)
%%           Pid = pid()
%% Descrip.: Delete all transactions handled by Pid from our list.
%%           Does NOT send the Pid any signals to quit or similar -
%%           just removes the transactions from our list.
%% Returns : {noreply, State, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_cast({unregister_pid, Pid}, State) ->
    NewL = transactionstatelist:delete_using_pid(Pid, State#state.tstatelist),
    {noreply, State#state{tstatelist=NewL}, ?TIMEOUT};

%%--------------------------------------------------------------------
%% Function: handle_cast({debug_show_transactions, FromPid}, State)
%%           Pid = pid()
%% Descrip.: Dump all our current transactions to logger. Just for
%%           debug output.
%% Returns : {noreply, State, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_cast({debug_show_transactions, FromPid}, State) ->
    logger:log(debug, "Transaction layer: Pid ~p asked me to show all ongoing transactions :~n~p",
	       [FromPid, transactionstatelist:debugfriendly(State#state.tstatelist)]),
    {noreply, State, ?TIMEOUT};

%%--------------------------------------------------------------------
%% Function: handle_cast({quit}, State)
%% Descrip.: Terminate the transaction_layer process. Should normally
%%           never be used.
%% Returns : {stop, normal, State}
%%--------------------------------------------------------------------
handle_cast({quit}, State) ->
    logger:log(debug, "Transaction layer: Received signal to quit"),
    {stop, normal, State};

handle_cast(Msg, State) ->
    logger:log(debug, "Transaction layer: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%% Descrip.: Wake up and remove all expired transactions from our
%%           list. DOES tell transaction handlers that it is time to
%%           terminate, if they are found to be expired.
%% Returns : {noreply, State, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    NewL = transactionstatelist:delete_expired(State#state.tstatelist),
    {noreply, State#state{tstatelist=NewL}, ?TIMEOUT};

%%--------------------------------------------------------------------
%% Function: handle_info({'EXIT', Pid, Reason}, State)
%%           Pid    = pid()
%%           Reason = normal | term()
%% Descrip.: Trap exit signals from client/server transaction handlers
%%           and act on them. Log if they exit with an error, and
%%           remove them from our list of ongoing transactions.
%% Returns : {noreply, State, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
	normal -> logger:log(debug, "Transaction layer: Received normal exit-signal from process ~p", [Pid]);
	_ -> logger:log(error, "Transaction layer: =ERROR REPORT==== Received non-normal exit signal from process ~p :~n~p", [Pid, Reason])
    end,
    NewState = case transactionstatelist:get_list_using_pid(Pid, State#state.tstatelist) of
		   none ->
		       logger:log(debug, "Transaction layer: Received exit signal from ~p not in my list. Transactionlist is :~n~p",
				  [Pid, transactionstatelist:debugfriendly(State#state.tstatelist)]),
		       State;
		   L when is_record(L, transactionstatelist) ->
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
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    logger:log(debug, "Transaction layer: Terminating normally"),
    ok;
terminate(Reason, _State) ->
    logger:log(error, "Transaction layer: =ERROR REPORT==== from transaction_layer :~n~p",
	       [Reason]),
    ok.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Purpose : Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: received_new_request(Request, Socket, LogStr, State)
%%           Request = request record()
%%           Socket  = sipsocket record(), the socket this request was
%%                                         received on
%%           LogStr  = string(), describes the request
%%           State   = record()
%% Descrip.: Act on a new request that has just been delivered to the
%%           transaction layer from the transport layer, where the
%%           transaction layer did not have any prior transaction.
%% Returns : {reply, Reply, State, ?TIMEOUT}
%%           Reply   = {pass_to_core, AppModule} |
%%                     {continue}
%%
%% Notes   :
%%           Meaning of Reply :
%%           {continue} means that the transaction layer has taken care
%%           of this Request and no further action should be taken by
%%           the caller of this function.
%%
%%           {pass_to_core, AppModule} means that the caller should
%%           apply() AppModule:request/3 with this Request as argument.
%%           We can't do it here since we can't block the
%%           transacton_layer process, and asking the caller to do it
%%           saves us a spawn(). AppModule is the module name passed to
%%           our init/1.
%%--------------------------------------------------------------------

%%
%% ACK
%%
received_new_request(Request, _Socket, _LogStr, State)
  when is_record(State, state), is_record(Request, request), Request#request.method == "ACK" ->
    AppModule = State#state.appmodule,
    logger:log(debug, "Transaction layer: Received ACK ~s that does not match any existing transaction, passing to core.",
	       [sipurl:print(Request#request.uri)]),
    {reply, {pass_to_core, AppModule}, State, ?TIMEOUT};

%%
%% CANCEL, our mode == stateless
%%
received_new_request(Request, Socket, LogStr, State)
  when is_record(State, state), is_record(Request, request), State#state.mode == stateless, Request#request.method == "CANCEL" ->
    AppModule = State#state.appmodule,
    logger:log(debug, "Transaction layer: Stateless received CANCEL ~s. Starting transaction but passing to core.",
	       [sipurl:print(Request#request.uri)]),
    case servertransaction:start_link(Request, Socket, LogStr, AppModule, stateless) of
	{ok, STPid} when is_pid(STPid) ->
	    NewL = transactionstatelist:add_server_transaction(Request, STPid, State#state.tstatelist),
	    {reply, {pass_to_core, AppModule}, State#state{tstatelist=NewL}, ?TIMEOUT};
	E ->
	    logger:log(error, "Transaction layer: Failed starting server transaction (~p) - ignoring request", [E]),
	    {reply, {continue}, State, ?TIMEOUT}
    end;

received_new_request(Request, Socket, LogStr, State) when is_record(State, state),
							  is_record(Request, request) ->
    AppModule = State#state.appmodule,
    logger:log(debug, "Transaction layer: No state for received request, starting new transaction"),
    case servertransaction:start_link(Request, Socket, LogStr, AppModule, State#state.mode) of
	{ok, STPid} when is_pid(STPid) ->
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
				     InvitePid when is_pid(InvitePid) ->
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

%%--------------------------------------------------------------------
%% Function: get_server_transaction(R, TStateList)
%%           R           = request record() | response record()
%%           TStateList  = list() of transactionstate record()
%% Descrip.: Find a server transaction in our list given either a
%%           request or a response record().
%% Returns : THandler |
%%           error    |
%%           none
%%           THandler    = transactionstate record()
%%--------------------------------------------------------------------
get_server_transaction(Request, TStateList) when is_record(Request, request) ->
    transactionstatelist:get_server_transaction_using_request(Request, TStateList);
get_server_transaction(Response, TStateList) when is_record(Response, response) ->
    transactionstatelist:get_server_transaction_using_response(Response, TStateList).

%%--------------------------------------------------------------------
%% Function: get_server_transaction_pid(R, TStateList)
%%           R           = request record() | response record()
%%           TStateList  = list() of transactionstate record()
%% Descrip.: Find a server transaction in TStateList given either a
%%           request or a response record(). Return it's pid.
%% Returns : Pid |
%%           none
%%           Pid         = pid()
%%--------------------------------------------------------------------
get_server_transaction_pid(Re, TStateList) when is_record(Re, request); is_record(Re, response) ->
    case get_server_transaction(Re, TStateList) of
	none ->
	    none;
	TState when is_record(TState, transactionstate) ->
	    case transactionstatelist:extract([pid], TState) of
		[TPid] when is_pid(TPid) ->
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

%%--------------------------------------------------------------------
%% Function: get_client_transaction(Response, TStateList)
%%           Response    = response record()
%%           TStateList  = list() of transactionstate record()
%% Descrip.: Find a client transaction in TStateList given a response.
%% Returns : TState |
%%           none
%%           TState      = transactionstate record()
%%--------------------------------------------------------------------
get_client_transaction(Response, TStateList) when is_record(Response, response) ->
    Header = Response#response.header,
    TopVia = sipheader:topvia(Header),
    Branch = sipheader:get_via_branch(TopVia),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    transactionstatelist:get_client_transaction(Method, Branch, TStateList).

%%--------------------------------------------------------------------
%% Function: get_client_transaction_pid(Response, TStateList)
%%           Response    = response record()
%%           TStateList  = list() of transactionstate record()
%% Descrip.: Find a client transaction in TStateList given a response.
%%           Return it's pid.
%% Returns : Pid  |
%%           none
%%           Pid         = pid()
%%--------------------------------------------------------------------
get_client_transaction_pid(Response, TStateList) when is_record(Response, response) ->
    case get_client_transaction(Response, TStateList) of
	none ->
	    none;
	TState when is_record(TState, transactionstate) ->
	    case transactionstatelist:extract([pid], TState) of
		[TPid] when is_pid(TPid) ->
		    TPid;
		_ ->
		    none
	    end
    end.


%%--------------------------------------------------------------------
%%% Interface functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: send_response_request(Request, Status, Reason,
%%                                 ExtraHeaders, RBody)
%%           Request      = request record()
%%           Status       = integer(), SIP status code
%%           Reason       = string(), SIP reason phrase
%%           ExtraHeaders = keylist record()
%%           RBody        = string(), response body
%% Descrip.: Locate the server transaction handler using a request,
%%           and then ask the server transaction handler to send a
%%           response.
%% Returns : none  |
%%           ok    |
%%           {error, E}
%%           E = string(), describes the error
%%
%% Note    : If this function returns 'none' it did not find a server
%%           transasction. Should probably be changed to
%%           {error, "No server transaction found"} or similar.
%%--------------------------------------------------------------------
send_response_request(Request, Status, Reason) when is_record(Request, request) ->
    send_response_request(Request, Status, Reason, []).

send_response_request(Request, Status, Reason, ExtraHeaders) when is_record(Request, request) ->
    send_response_request(Request, Status, Reason, ExtraHeaders, "").

send_response_request(Request, Status, Reason, ExtraHeaders, RBody) when is_record(Request, request) ->
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
	TH when is_record(TH, thandler) ->
	    logger:log(debug, "Transaction layer: Located server transaction handler ~p", [TH#thandler.pid]),
	    send_response_handler(TH, Status, Reason, ExtraHeaders, RBody)
    end.

%%--------------------------------------------------------------------
%% Function: send_response_handler(TH, Status, Reason, ExtraHeaders,
%%                                 RBody)
%%           TH           = thandler record(), transaction handler
%%           Request      = request record()
%%           Status       = integer(), SIP status code
%%           Reason       = string(), SIP reason phrase
%%           ExtraHeaders = keylist record()
%%           RBody        = string(), response body
%% Descrip.: Ask a server transaction handler to send a response.
%% Returns : ok    |
%%           {error, E}
%%           E = string(), describes the error
%%--------------------------------------------------------------------
send_response_handler(TH, Status, Reason) when is_record(TH, thandler) ->
    send_response_handler(TH, Status, Reason, []).

send_response_handler(TH, Status, Reason, ExtraHeaders) when is_record(TH, thandler) ->
    send_response_handler(TH, Status, Reason, ExtraHeaders, "").

send_response_handler(TH, Status, Reason, ExtraHeaders, RBody) when is_record(TH, thandler) ->
    case catch gen_server:cast(TH#thandler.pid, {create_response, Status, Reason, ExtraHeaders, RBody}) of
	ok ->
	    ok;
	_ ->
	    {error, "Transaction handler failure"}
    end.

%%--------------------------------------------------------------------
%% Function: send_proxy_response_handler(TH, Response)
%%           TH           = thandler record(), transaction handler
%%           Response     = response record()
%% Descrip.: Ask a server transaction handler to proxy a response.
%%           When we proxy (as opposed to send) a response, we do some
%%           additional processing like removing the top Via header.
%% Returns : ok    |
%%           {error, E}
%%           E = string(), describes the error
%%--------------------------------------------------------------------
send_proxy_response_handler(TH, Response) when is_record(TH, thandler), is_record(Response, response) ->
    case gen_server:cast(TH#thandler.pid, {forwardresponse, Response}) of
	ok ->
	    ok;
	_ ->
	    {error, "Transaction handler failure"}
    end.

%%--------------------------------------------------------------------
%% Function: get_handler_for_request(Request)
%%           Request = request record()
%% Descrip.: Return the server transaction handler using a request.
%%           This is the ordinary way for an application to locate the
%%           server transaction for a request it gets passed, in order
%%           to send a response or get it's branch or whatever.
%% Returns : THandler   |
%%           {error, E}
%%           THandler = thandler record()
%%           E = string(), describes the error
%%--------------------------------------------------------------------
get_handler_for_request(Request) ->
    case catch gen_server:call(transaction_layer, {get_server_transaction_handler, Request}, 2500) of
        {error, E} ->
	    {error, E};
        {ok, Pid} ->
	    #thandler{pid=Pid};
	_ ->
	    {error, "Transaction layer failed"}
    end.

%%--------------------------------------------------------------------
%% Function: get_server_handler_for_stateless_response(Response)
%%           Response = response record()
%% Descrip.: Return the server transaction handler using a response.
%%           If we operate without keeping track of which server
%%           transaction to send a received response to in order to
%%           forward it upstreams, we can use this function to find
%%           out which server transaction to use by looking for the
%%           association, between server transaction and forwarded
%%           requests branch, created by the transport layer with
%%           store_to_tag(). This does only apply to stateless
%%           operation, which is NOT the default operating mode for
%%           any current Yxa applications.
%% Returns : THandler   |
%%           none       |
%%           {error, E}
%%           THandler = thandler record()
%%           E = string(), describes the error
%%--------------------------------------------------------------------
get_server_handler_for_stateless_response(Response) when is_record(Response, response) ->
    TopVia = sipheader:topvia(Response#response.header),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Response#response.header)),
    case sipheader:get_via_branch(TopVia) of
	Branch when is_list(Branch) ->
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

%%--------------------------------------------------------------------
%% Function: adopt_server_transaction(Request)
%%           Request = request record()
%% Descrip.: Adopt a server transaction. Adoption means that the
%%           server transaction will inform whoever is adopting it
%%           if the server transaction gets cancelled, and when it
%%           terminates.
%% Returns : THandler   |
%%           {error, E}
%%           THandler = thandler record()
%%           E = string(), describes the error
%%--------------------------------------------------------------------
adopt_server_transaction(Request) when is_record(Request, request) ->
    case get_handler_for_request(Request) of
	TH when is_record(TH, thandler) ->
	    adopt_server_transaction_handler(TH);
	_ ->
	    {error, "unknown result from get_handler_for_request"}
    end.

%% See adopt_server_transaction/1 above
adopt_server_transaction_handler(TH) when is_record(TH, thandler) ->
    case catch gen_server:call(TH#thandler.pid, {set_report_to, self()}, 1000) of
	{error, E} ->
	    {error, E};
	{ok} ->
	    TH;
	_ ->
	    {error, "Server transaction handler failed"}
    end.

%%--------------------------------------------------------------------
%% Function: get_branch_from_handler(TH)
%%           TH = thandler record(), the transaction handler to query
%% Descrip.: Get the branch from a server transaction. It is useful to
%%           get the branch from a server transaction for use in
%%           logging, and when generating branches for client
%%           transactions related to the server transaction.
%% Returns : Branch |
%%           error
%%           Branch = string()
%%--------------------------------------------------------------------
get_branch_from_handler(TH) when is_record(TH, thandler) ->
    TPid = TH#thandler.pid,
    case catch gen_server:call(TPid, {get_branch}, 500) of
	{ok, Branch} ->
	    Branch;
	_ ->
	    logger:log(error, "Transaction layer: Failed getting branch from pid ~p", [TPid]),
	    error
    end.

%% unused
transaction_terminating(TransactionPid) ->
    gen_server:cast(transaction_layer, {unregister_pid, TransactionPid}),
    ok.

%%--------------------------------------------------------------------
%% Function: start_client_transaction(Request, SocketIn, Dst, Branch,
%%                                    Timeout, Parent)
%%           Request  = request record()
%%           SocketIn = sipsocket record(), XXX which socket is this?
%%           Dst      = sipdst record(), the destination for this
%%                      client transaction
%%           Branch   = string(), branch parameter to use
%%           Timeout  = integer(), how long to live (seconds)
%%           Parent   = pid(), who the client transaction should
%%                      report to
%% Descrip.: Start a new client transaction.
%% Returns : Pid        |
%%           {error, E}
%%           Pid = pid()
%%           E   = string()
%%--------------------------------------------------------------------
start_client_transaction(Request, SocketIn, Dst, Branch, Timeout, Parent) ->
    %% XXX we probably want a shorter timeout on this, but since we send out the initial request
    %% in clienttransaction:init() it can take some time (if a TCP connection has to be established
    %% first etc).
    case catch gen_server:call(transaction_layer, {start_client_transaction, Request, SocketIn,
						   Dst, Branch, Timeout, Parent}, 10000) of
	{error, E} ->
	    {error, E};
	{ok, Pid} ->
	    Pid;
	Unknown ->
	    logger:log(debug, "Transaction layer: Failed starting client transaction, gen_server call result :~n~p",
		       [Unknown]),
	    {error, "Transaction layer failure"}
    end.

%%--------------------------------------------------------------------
%% Function: store_to_tag(Request, ToTag)
%%           Request  = request record()
%%           ToTag    = string()
%% Descrip.: Store the to-tag we use when sending non-2xx responses in
%%           INVITE server transactions. We need to do this to
%%           correctly match ACK to the server transaction.
%% Returns : ok         |
%%           {error, E}
%%           E = string(), description of error
%%--------------------------------------------------------------------
store_to_tag(Request, ToTag) when is_record(Request, request) ->
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

%%--------------------------------------------------------------------
%% Function: store_stateless_response_branch(TH, Branch, Method)
%%           TH      = thandler record() | none
%%           Branch  = string()
%%           Method  = string()
%% Descrip.: When sending out requests statelessly, the transport
%%           layer (in our implementation) knows what server
%%           transaction the request originally arrived on. To make it
%%           possible to know exactly on which socket we should send
%%           out any responses we receive to this stateless request
%%           we must associate the branch we used in the request we
%%           sent out, with the server transaction of the original
%%           request.
%% Returns : ok         |
%%           {error, E}
%%           E = string(), description of error
%%--------------------------------------------------------------------
store_stateless_response_branch(none, _Branch, _Method) ->
    ok;
store_stateless_response_branch(TH, Branch, Method) when is_record(TH, thandler) ->
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

%%--------------------------------------------------------------------
%% Function: is_good_transaction(TH)
%%           TH      = thandler record() | term()
%% Descrip.: Check if a given argument is a thandler record() that
%%           refers to a transaction handler that is still alive.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
is_good_transaction(TH) when is_record(TH, thandler) ->
    case util:safe_is_process_alive(TH#thandler.pid) of
	{true, _} ->
	    true;
	_ ->
	    false
    end;
is_good_transaction(_) ->
    false.

%%--------------------------------------------------------------------
%% Function: get_pid_from_handler(TH)
%%           TH      = thandler record() | term()
%% Descrip.: Sometimes it is actually necessary for something besides
%%           the transaction layer to know the pid handling a
%%           transaction. In those cases, this function lets a caller
%%           extract the pid of a transaction handler. Note though
%%           that you should NEVER communicate with that pid directly.
%% Returns : Pid  |
%%           none
%%           Pid = pid()
%%--------------------------------------------------------------------
get_pid_from_handler(TH) when is_record(TH, thandler) ->
    TH#thandler.pid;
get_pid_from_handler(_) ->
    none.

%%--------------------------------------------------------------------
%% Function: send_challenge_request(Request, Type, Stale, RetryAfter)
%%           Request    = request record()
%%           Type       = www | proxy
%%           Stale      = true | false
%%           RetryAfter = integer()
%% Descrip.: Locate a server transaction handler using Request, then
%%           invoke send_challenge() with the rest of our parameters.
%% Returns : ok
%%--------------------------------------------------------------------
send_challenge_request(Request, Type, Stale, RetryAfter) ->
    TH = transactionlayer:get_handler_for_request(Request),
    %% XXX check that we really got a thandler record()
    send_challenge(TH, Type, Stale, RetryAfter).

%%--------------------------------------------------------------------
%% Function: send_challenge(TH, Type, Stale, RetryAfter)
%%           TH         = thandler record()
%%           Type       = www | proxy
%%           Stale      = true | false
%%           RetryAfter = integer()
%% Descrip.: Generate a '407 Proxy-Authenticate' or
%%           '401 WWW-Authenticate' response and hand this to a
%%           server transaction handler. If given a request record()
%%           as In, first locate the real server transaction handler.
%% Returns : ok
%%--------------------------------------------------------------------
send_challenge(TH, www, Stale, RetryAfter) when is_record(TH, thandler) ->
    AuthHeader = [{"WWW-Authenticate", sipheader:auth_print(sipauth:get_challenge(), Stale)}],
    send_challenge2(TH, 401, "Authentication Required", AuthHeader, RetryAfter);
send_challenge(TH, proxy, Stale, RetryAfter) when is_record(TH, thandler) ->
    AuthHeader = [{"Proxy-Authenticate", sipheader:auth_print(sipauth:get_challenge(), Stale)}],
    send_challenge2(TH, 407, "Proxy Authentication Required", AuthHeader, RetryAfter).

send_challenge2(TH, Status, Reason, AuthHeader, RetryAfter) when is_record(TH, thandler)->
    RetryHeader = case RetryAfter of
		      I when is_integer(I) ->
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

%%--------------------------------------------------------------------
%% Function: debug_show_transactions()
%% Descrip.: Make the transaction layer log info about all it's
%%           transactions to the debug log.
%% Returns : ok
%%--------------------------------------------------------------------
debug_show_transactions() ->
    gen_server:cast(transaction_layer, {debug_show_transactions, self()}),
    ok.
