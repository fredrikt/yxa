%%%-------------------------------------------------------------------
%%% File    : transactionlayer.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Transactionlayer
%%% @since    05 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(transactionlayer).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 send_response_request/3,
	 send_response_request/4,
	 send_response_request/5,
	 transaction_terminating/1,
	 get_handler_for_request/1,
	 get_branch_from_handler/1,
	 get_branchbase_from_handler/1,
	 start_client_transaction/5,
	 start_client_transaction/6,
	 cancel_client_transaction/3,
	 adopt_server_transaction/1,
	 adopt_server_transaction_handler/1,
	 adopt_st_and_get_branchbase/1,
	 send_response_handler/3,
	 send_response_handler/4,
	 send_response_handler/5,
	 send_proxy_response_handler/2,
	 is_good_transaction/1,
	 get_pid_from_handler/1,
	 send_challenge_request/4,
	 send_challenge/4,
	 store_appdata/2,
	 get_my_to_tag/1,
	 change_transaction_parent/3,
	 debug_show_transactions/0
	]).

-export([test_get_thandler_self/0,
	 test/0
	]).

-deprecated([
             {start_client_transaction, 6}
            ]).

%%--------------------------------------------------------------------
%% Transport layer internal exports (functions that should only be
%% invoked from the transport layer).
%%--------------------------------------------------------------------
-export([
	 from_transportlayer/2
	]).

%%--------------------------------------------------------------------
%% Transaction layer internal exports (functions that should only be
%% invoked from the transaction layer).
%%--------------------------------------------------------------------
-export([
	 store_to_tag/2,
	 set_result/2
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
%% @type state() = #state{}.
%%                 My State
-record(state, {
	 }).

%% @type thandler() = #thandler{}.
%%                    A container record to make it clear that all communication
%%                    with the transaction processes should go through this
%%                    module.
-record(thandler, {
	  pid
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
%% Wake up every seven seconds to tell expired transactions that it is
%% time to exit
-define(TIMEOUT, 7 * 1000).
-define(STORE_TIMEOUT, 2500).
-define(STATELESS_STORE_TIMEOUT, 1000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     start the transactionlayer gen_server. The
%%          transactionlayer is only registered localy (on the
%%          current node)
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, transactionlayer}, ?MODULE, [], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ([]) ->
%%            {ok, State}          |
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),

    transactionstatelist:empty(), %% create ets tables

    %% create statistics ets-entrys
    lists:map(fun(Key) ->
		      true = ets:insert_new(yxa_statistics, {Key, 0})
	      end, [{transactionlayer, transactions}
		   ]),

    logger:log(debug, "Transaction layer started"),
    {ok, #state{}, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% @spec    ({monitor_get_transactionlist}, From, State) ->
%%            {reply, {ok, List}, State, Timeout::integer()}
%%
%%            List = #transactionstatelist{}
%%
%% @doc     The stack monitor is requesting our list of transactions.
%%          Note : Returning all entrys from an ets table is a
%%          relatively expensive operation. You might not want to run
%%          the stack monitor on a busy system...
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({monitor_get_transactionlist}, _From, State) ->
    {reply, {ok, transactionstatelist:get_all_entries()}, State, ?TIMEOUT};

handle_call(Request, From, State) ->
    logger:log(debug, "Transaction layer: Received unknown gen_server call (from ~p) : ~p", [From, Request]),
    {noreply, State, ?TIMEOUT}.


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
%% @spec    ({unregister_pid, Pid}, State) ->
%%            {noreply, State, Timeout::integer()}
%%
%%            Pid = pid()
%%
%% @doc     Delete all transactions handled by Pid from our list. Does
%%          NOT send the Pid any signals to quit or similar - just
%%          removes the transactions from our list.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({unregister_pid, Pid}, State) ->
    L = transactionstatelist:get_entrylist_using_pid(Pid),
    {ok, Deleted} = transactionstatelist:delete_using_entrylist(L),
    logger:log(debug, "Transaction layer: Deleted ~p entry(s) from transactionlist upon "
	       "receiving an {unregister_pid, ~p} :~n~p~n(new list is ~p entry(s))",
	       [Deleted, Pid, transactionstatelist:debugfriendly(L),
		transactionstatelist:get_length()]),
    {noreply, State, ?TIMEOUT};

%%--------------------------------------------------------------------
%% @spec    ({debug_show_transactions, FromPid}, State) ->
%%            {noreply, State, Timeout::integer()}
%%
%%            Pid = pid()
%%
%% @doc     Dump all our current transactions to logger. Just for
%%          debug output.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({debug_show_transactions, FromPid}, State) ->
    logger:log(debug, "Transaction layer: Pid ~p asked me to show all ongoing transactions :~n~p",
	       [FromPid, transactionstatelist:debugfriendly()]),
    {noreply, State, ?TIMEOUT};

%%--------------------------------------------------------------------
%% @spec    ({quit}, State) -> {stop, normal, State}
%%
%% @doc     Terminate the transactionlayer process. Should normally
%%          never be used.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({quit}, State) ->
    logger:log(debug, "Transaction layer: Received signal to quit"),
    {stop, normal, State};

handle_cast(Msg, State) ->
    logger:log(debug, "Transaction layer: Received unknown gen_server cast : ~p", [Msg]),
    {noreply, State, ?TIMEOUT}.


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
%% @spec    (timeout, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Wake up and remove all expired transactions from our list.
%%          DOES tell transaction handlers that it is time to
%%          terminate, if they are found to be expired.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    case transactionstatelist:get_expired() of
	none ->
	    ok;
	{ok, Expired} when is_list(Expired) ->
	    %% Signal all Expired entrys that they are expired
	    lists:map(fun(#transactionstate{pid = Pid}) when is_pid(Pid) ->
			      logger:log(debug, "Transaction layer: Telling transaction with pid ~p that "
					 "it is expired", [Pid]),
			      gen_server:cast(Pid, {expired})
		      end, Expired)
    end,
    {noreply, State, ?TIMEOUT};

%%--------------------------------------------------------------------
%% @spec    ({'EXIT', Pid, Reason}, State) ->
%%            {noreply, State, Timeout::integer()}
%%
%%            Pid    = pid()
%%            Reason = normal | term()
%%
%% @doc     Trap exit signals from client/server transaction handlers
%%          and act on them. Log if they exit with an error, and
%%          remove them from our list of ongoing transactions.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
	normal -> logger:log(debug, "Transaction layer: Received normal exit-signal from process ~p", [Pid]);
	_ -> logger:log(error, "Transaction layer: =ERROR REPORT==== Received non-normal exit signal from process ~p :~n~p", [Pid, Reason])
    end,
    case transactionstatelist:get_entrylist_using_pid(Pid) of
	none ->
	    logger:log(debug, "Transaction layer: Received exit signal from ~p not in my list.", [Pid]),
	    true;
	L when is_list(L) ->
	    {ok, Deleted} = transactionstatelist:delete_using_entrylist(L),
	    logger:log(debug, "Transaction layer: Deleted ~p entry(s) from transactionlist :~n~p~n"
		       "(new list is ~p entry(s))", [Deleted, transactionstatelist:debugfriendly(L),
						     transactionstatelist:get_length()]),
	    true
    end,
    {noreply, State, ?TIMEOUT};

handle_info(Msg, State) ->
    logger:log(error, "Transaction layer: Received unknown gen_server info :~n~p", [Msg]),
    {noreply, State, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @end
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    logger:log(debug, "Transaction layer: Terminating normally."),
    ok;
terminate(shutdown, _State) ->
    logger:log(debug, "Transaction layer: Shutting down."),
    shutdown;
terminate(Reason, _State) ->
    logger:log(error, "Transaction layer: =ERROR REPORT==== from transactionlayer :~n~p",
	       [Reason]),
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

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, AppModule) ->
%%            {pass_to_core, AppModule, STPid} |
%%            continue
%%
%%            Request   = #request{}
%%            YxaCtx    = #yxa_ctx{}
%%            AppModule = atom() "YXA application module"
%%
%%            STPid = pid() | undefined
%%
%% @doc     Act on a new request that has just been delivered to the
%%          transaction layer from the transport layer, where the
%%          transaction layer did not have any prior transaction.
%%          This code executes in the request handler process - NOT
%%          in the transactionlayer. This is to achieve better
%%          concurrency by not doing alot of work in the
%%          transactionlayer process. Notes : Meaning of Reply :
%%          continue means that the transaction layer has taken care
%%          of this Request and no further action should be taken by
%%          the caller of this function.
%%          {pass_to_core, AppModule, STPid} means that the caller
%%          should apply() AppModule:request/2 with this Request as
%%          argument. We can't do it here since we can't block the
%%          transacton_layer process, and asking the caller to do it
%%          saves us a spawn(). AppModule is the module name passed
%%          to our init/1.
%%
%%          It might be worthwile to insert a dummy-entry in the
%%          transaction layers list of transactions for this new
%%          transaction, before spawning a servertransaction. That
%%          would reduce the workload of a really loaded system that
%%          receives a big percentage of resends before it has had
%%          time to spawn servertransactions that have registered
%%          themselves in the transaction layers list.
%% @end
%%--------------------------------------------------------------------

%%
%% ACK
%%
received_new_request(#request{method = "ACK"} = Request, _YxaCtx, AppModule) ->
    logger:log(debug, "Transaction layer: Received ACK ~s that does not match any existing transaction, "
	       "passing to core.", [sipurl:print(Request#request.uri)]),
    {pass_to_core, AppModule, undefined};

received_new_request(Request, YxaCtx, AppModule) when is_record(Request, request) ->
    logger:log(debug, "Transaction layer: No state for received request, starting new transaction"),
    case servertransaction:start_link(Request, YxaCtx) of
	{ok, STPid} when is_pid(STPid) ->
	    %% returns true if it is a CANCEL and we find a transaction to cancel, otherwise false
	    case cancel_corresponding_transaction(Request, STPid) of
		true ->
		    logger:log(debug, "Transaction layer: Telling SIP message handler to start this applications "
			       "request function."),
		    {pass_to_core, AppModule, STPid};
		false ->
		    continue
	    end;
	{error, resend} ->
	    logger:log(debug, "Transaction layer: Failed starting server transaction (was a resend) - "
		       "ignoring request"),
	    continue;
	E ->
	    %%% XXX erlang:exit() here so that we might return a 500 response (statelessly)?
	    logger:log(error, "Transaction layer: Failed starting server transaction (~p) - ignoring request", [E]),
	    continue
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, STPid) ->
%%            PassToCore
%%
%%            Request = #request{}
%%            STPid   = pid() "CANCEL server transaction."
%%
%%            PassToCore = true | false
%%
%% @doc     Part of received_new_request/4. We have received a CANCEL
%%          and here we try to find the corresponding INVITE and tell
%%          it that it has been cancelled. If we find an INVITE, we
%%          return 'false' to _not_ pass this CANCEL to the
%%          Transaction User (core). If we do not find it however, we
%%          return 'true' to pass the CANCEL to core.
%% @end
%%--------------------------------------------------------------------
cancel_corresponding_transaction(#request{method="CANCEL"}=Request, STPid) when is_pid(STPid) ->
    Header = Request#request.header,
    %% XXX not only INVITE can be cancelled, RFC3261 9.2 says we should find the
    %% transaction that is being handled by 'assuming the method is anything but
    %% CANCEL or ACK'.
    {CSeqNum, _} = sipheader:cseq(Header),
    %% When looking for the corresponding INVITE transaction, we have to change the
    %% CSeq method of this header to INVITE, in case we received it from a RFC2543 client
    %% (RFC2543 backwards-compatible transaction matching includes the whole CSeq, this is
    %% probably an error in RFC3261 #9.2 that refers to #17.2.3 which does not say that
    %% CANCEL matches a transaction even though the CSeq method differs)
    IHeader = keylist:set("CSeq", [sipheader:cseq_print({CSeqNum, "INVITE"})], Header),
    Invite = Request#request{method="INVITE", header=IHeader},
    case get_server_transaction_pid(Invite) of
	InvitePid when is_pid(InvitePid) ->
	    logger:log(debug, "Transaction layer: CANCEL matches server transaction handled by ~p", [InvitePid]),
	    {Status, Reason} =
		case util:safe_is_process_alive(InvitePid) of
		    {true, _} ->
			logger:log(debug, "Transaction layer: Cancelling original request handled by ~p "
				   "and responding 200 Ok", [InvitePid]),
			%% RFC3326 #2 says we SHOULD include any present Reason header in a CANCEL we
			%% receive in the CANCELs we generate based on it
			ExtraHeaders = [{"Reason",
					 keylist:fetch('reason', Header)
					}],
			gen_server:cast(InvitePid, {cancelled, ExtraHeaders}),
			{200, "Ok"};
		    {false, _} ->
			logger:log(debug, "Transaction layer: Transaction to be cancelled handled by dead "
				   "pid '~p', responding 481 Call/Transaction Does Not Exist", [InvitePid]),
			{481, "Call/Transaction Does Not Exist"}
		end,
	    gen_server:cast(STPid, {create_response, Status, Reason, [], <<>>}),
	    %% Don't pass to core
	    false;
	none ->
	    logger:log(debug, "Transaction layer: Could not find transaction to cancel, "
		       "pass it on to TU/proxy core."),
	    %% Pass to core
	    true
    end;
cancel_corresponding_transaction(Request, STPid) when is_record(Request, request), is_pid(STPid) ->
    %% Request method not CANCEL, pass to core
    true.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            Pid |
%%            none
%%
%%            Request = #request{}
%%
%%            Pid = pid()
%%
%% @doc     Find a server transaction given a request. Return it's
%%          pid.
%% @end
%%--------------------------------------------------------------------
get_server_transaction_pid(Request) when is_record(Request, request) ->
    case get_server_transaction(Request) of
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
%% @spec    (Request) ->
%%            THandler |
%%            error    |
%%            none
%%
%%            Request = #request{}
%%
%%            THandler = #transactionstate{}
%%
%% @doc     Find a server transaction in our list given either a
%%          request or a response record().
%% @end
%%--------------------------------------------------------------------
get_server_transaction(Request) when is_record(Request, request) ->
    transactionstatelist:get_server_transaction_using_request(Request).

%%--------------------------------------------------------------------
%% @spec    (Response) ->
%%            TState |
%%            none
%%
%%            Response = #response{}
%%
%%            TState = #transactionstate{}
%%
%% @doc     Find a client transaction given a response.
%% @end
%%--------------------------------------------------------------------
get_client_transaction(Response) when is_record(Response, response) ->
    Header = Response#response.header,
    TopVia = sipheader:topvia(Header),
    Branch = sipheader:get_via_branch(TopVia),
    {_, Method} = sipheader:cseq(Header),
    transactionstatelist:get_client_transaction(Method, Branch).

%%--------------------------------------------------------------------
%% @spec    (Response) ->
%%            Pid  |
%%            none
%%
%%            Response = #response{}
%%
%%            Pid = pid()
%%
%% @doc     Find a client transaction given a response. Return it's
%%          pid.
%% @end
%%--------------------------------------------------------------------
get_client_transaction_pid(Response) when is_record(Response, response) ->
    case get_client_transaction(Response) of
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
%% @spec    (Request, Status, Reason) -> term()
%%
%%            Request = #request{}
%%            Status  = integer() "SIP status code"
%%            Reason  = string() "SIP reason phrase"
%%
%% @doc     Sends a response with an empty body and no extra headers.
%%          @see send_response_request/5.
%% @end
%%--------------------------------------------------------------------
send_response_request(Request, Status, Reason) when is_record(Request, request) ->
    send_response_request(Request, Status, Reason, []).

%%--------------------------------------------------------------------
%% @spec    (Request, Status, Reason, ExtraHeaders) -> term()
%%
%%            Request      = #request{}
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = #keylist{}
%%
%% @doc     Sends a response with an empty body. @see
%%          send_response_request/5.
%% @end
%%--------------------------------------------------------------------
send_response_request(Request, Status, Reason, ExtraHeaders) when is_record(Request, request) ->
    send_response_request(Request, Status, Reason, ExtraHeaders, <<>>).

%%--------------------------------------------------------------------
%% @spec    (Request, Status, Reason, ExtraHeaders, RBody) ->
%%            none  |
%%            ok    |
%%            {error, E}
%%
%%            Request      = #request{}
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = #keylist{}
%%            RBody        = string() "response body"
%%
%%            E = string() "describes the error"
%%
%% @doc     Locate the server transaction handler using a request, and
%%          then ask the server transaction handler to send a
%%          response. Note : If this function returns 'none' it did
%%          not find a server transasction. Should probably be
%%          changed to {error, "No server transaction found"} or
%%          similar.
%% @end
%%--------------------------------------------------------------------
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
%% @spec    (TH, Status, Reason) -> term()
%%
%%            TH     = #thandler{} "transaction handler"
%%            Status = integer() "SIP status code"
%%            Reason = string() "SIP reason phrase"
%%
%% @doc     Send a response with an empty body and no extra headers.
%%          @see send_response_handler/5.
%% @end
%%--------------------------------------------------------------------
send_response_handler(TH, Status, Reason) when is_record(TH, thandler) ->
    send_response_handler(TH, Status, Reason, []).

%%--------------------------------------------------------------------
%% @spec    (TH, Status, Reason, ExtraHeaders) -> term()
%%
%%            TH           = #thandler{} "transaction handler"
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = #keylist{}
%%
%% @doc     Send a response with an empty body. @see
%%          send_response_handler/5.
%% @end
%%--------------------------------------------------------------------
send_response_handler(TH, Status, Reason, ExtraHeaders) when is_record(TH, thandler) ->
    send_response_handler(TH, Status, Reason, ExtraHeaders, <<>>).

%%--------------------------------------------------------------------
%% @spec    (TH, Status, Reason, ExtraHeaders, RBody) ->
%%            ok    |
%%            {error, E}
%%
%%            TH           = #thandler{} "transaction handler"
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = #keylist{}
%%            RBody        = binary() "response body"
%%
%%            E = string() "describes the error"
%%
%% @doc     Ask a server transaction handler to send a response.
%% @end
%%--------------------------------------------------------------------
send_response_handler(TH, Status, Reason, ExtraHeaders, RBody)
  when is_record(TH, thandler), is_integer(Status), is_list(Reason), is_list(ExtraHeaders), is_binary(RBody) ->
    case catch gen_server:cast(TH#thandler.pid, {create_response, Status, Reason, ExtraHeaders, RBody}) of
	ok ->
	    ok;
	_ ->
	    {error, "Transaction handler failure"}
    end.

%%--------------------------------------------------------------------
%% @spec    (TH, Response) ->
%%            ok    |
%%            {error, E}
%%
%%            TH       = #thandler{} "transaction handler"
%%            Response = #response{}
%%
%%            E = string() "describes the error"
%%
%% @doc     Ask a server transaction handler to proxy a response. When
%%          we proxy (as opposed to send) a response, we do some
%%          additional processing like removing the top Via header.
%% @end
%%--------------------------------------------------------------------
send_proxy_response_handler(TH, Response) when is_record(TH, thandler), is_record(Response, response) ->
    case gen_server:cast(TH#thandler.pid, {forwardresponse, Response}) of
	ok ->
	    ok;
	_ ->
	    {error, "Transaction handler failure"}
    end.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            THandler   |
%%            {error, E}
%%
%%            Request = #request{}
%%
%%            THandler = #thandler{}
%%            E        = string() "describes the error"
%%
%% @doc     Return the server transaction handler using a request.
%%          This is the ordinary way for an application to locate the
%%          server transaction for a request it gets passed, in order
%%          to send a response or get it's branch or whatever.
%% @end
%%--------------------------------------------------------------------
get_handler_for_request(Request) when is_record(Request, request) ->
    case get_server_transaction_pid(Request) of
        none ->
            {error, "No state found"};
        TPid when is_pid(TPid) ->
            #thandler{pid = TPid}
    end.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            THandler         |
%%            {ignore, Reason} |
%%            {error, E}
%%
%%            Request = #request{}
%%
%%            THandler = #thandler{}
%%            Reason   = cancelled | completed
%%            E        = string() "describes the error"
%%
%% @doc     Adopt a server transaction. Adoption means that the server
%%          transaction will inform whoever is adopting it if the
%%          server transaction gets cancelled, and when it
%%          terminates.
%% @end
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
    case catch gen_server:call(TH#thandler.pid, {set_report_to, self()}, ?STORE_TIMEOUT) of
	{ignore, Reason} ->
	    {ignore, Reason};
	ok ->
	    TH;
	{error, E} ->
	    {error, E};
	_ ->
	    {error, "Server transaction handler failed"}
    end.

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            {ok, THandler, BranchBase} |
%%            error                      |
%%            ignore
%%
%%            In = #thandler{} | #request{}
%%
%%            THandler   = #thandler{}
%%            BranchBase = string() "branch minus the \"-UAS\" suffix"
%%
%% @doc     Adopt a server transaction, and get it's branch. This is
%%          just a helper function for applications, since this is
%%          typically what they do anyways. If you want to know for
%%          example the real reason it returns 'ignore', use the more
%%          articulate adopt_server_transaction_handler/1.
%% @end
%%--------------------------------------------------------------------
adopt_st_and_get_branchbase(Request) when is_record(Request, request) ->
    case get_handler_for_request(Request) of
	TH when is_record(TH, thandler) ->
	    adopt_st_and_get_branchbase(TH);
	{error, Reason} ->
	    logger:log(error, "Transaction layer: Could not find transaction handler for request : ~p",
		       [Reason]),
	    error
    end;
adopt_st_and_get_branchbase(TH) when is_record(TH, thandler) ->
    case adopt_server_transaction_handler(TH) of
	{ignore, _Reason} ->
	    ignore;
	{error, E} ->
	    logger:log(error, "Transaction layer: Could not adopt server transaction handler ~p : ~p", [TH, E]),
	    error;
	TH ->
	    BranchBase = get_branchbase_from_handler(TH),
	    {ok, TH, BranchBase}
    end.

%%--------------------------------------------------------------------
%% @spec    (TH) ->
%%            BranchBase |
%%            error
%%
%%            TH = #thandler{} "the transaction handler to query"
%%
%%            BranchBase = string() "branch minus the \"-UAS\" suffix"
%%
%% @doc     Get the branch from a server transaction, and then remove
%%          the "-UAS" suffix to get the base of the branch. The base
%%          can for example be used to generate unique branches that
%%          can be visually correlated to each other, by appending a
%%          sequence number.
%% @end
%%--------------------------------------------------------------------
get_branchbase_from_handler(TH) when is_record(TH, thandler) ->
    case get_branch_from_handler(TH) of
	Branch when is_list(Branch) ->
	    case string:rstr(Branch, "-UAS") of
		0 ->
		    %% XXX is this an error condition?
		    Branch;
		Index when is_integer(Index) ->
		    BranchBase = string:substr(Branch, 1, Index - 1),
		    BranchBase
	    end;
	error ->
	    error
    end.

%%--------------------------------------------------------------------
%% @spec    (TH) ->
%%            Branch |
%%            error
%%
%%            TH = #thandler{} "the transaction handler to query"
%%
%%            Branch = string()
%%
%% @doc     Get the branch from a server transaction. It is useful to
%%          get the branch from a server transaction for use in
%%          logging, and when generating branches for client
%%          transactions related to the server transaction.
%% @end
%%--------------------------------------------------------------------
get_branch_from_handler(#thandler{pid = TPid}) when is_pid(TPid), TPid == self() ->
    %% for unit testing
    case autotest:is_unit_testing(transactionlayer, get_branch_from_handler) of
	{true, Res} when (is_list(Res) orelse Res == error)->
	    Res;
	false ->
	    logger:log(error, "Transaction layer: Failed getting branch from self!"),
	    error
    end;
get_branch_from_handler(#thandler{pid = TPid}) when is_pid(TPid) ->
    case catch gen_server:call(TPid, {get_branch}, ?STORE_TIMEOUT) of
	{ok, Branch} ->
	    Branch;
	Unknown ->
	    logger:log(error, "Transaction layer: Failed getting branch from pid ~p : ~p", [TPid, Unknown]),
	    error
    end.

%% unused
transaction_terminating(TransactionPid) ->
    gen_server:cast(transactionlayer, {unregister_pid, TransactionPid}),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Request, SocketIn, Dst, Branch, Timeout, ReportTo) ->
%%            term()
%%
%% @doc     OBSOLETE There is no need to pass in a socket anymore.
%%          @see start_client_transaction/5.
%% @end
%%--------------------------------------------------------------------
start_client_transaction(Request, _SocketIn, Dst, Branch, Timeout, ReportTo) ->
    %% OBSOLETE - there is no reason to pass sockets to client transactions anymore
    logger:log(normal, "Warning: transactionlayer:start_client_transaction/6 is obsolete, "
	       "use transactionlayer:start_client_transaction/5 instead"),
    start_client_transaction(Request, Dst, Branch, Timeout, ReportTo).

%%--------------------------------------------------------------------
%% @spec    (Request, Dst, Branch, Timeout, ReportTo) ->
%%            Pid        |
%%            {error, E}
%%
%%            Request  = #request{}
%%            Dst      = #sipdst{} "the destination for this client transaction"
%%            Branch   = string() "branch parameter to use"
%%            Timeout  = integer() "timeout for INVITE transactions (seconds from now)"
%%            ReportTo = pid() "who the client transaction should report to"
%%
%%            Pid = pid()
%%            E   = string()
%%
%% @doc     Start a new client transaction.
%% @end
%%--------------------------------------------------------------------
start_client_transaction(Request, Dst, Branch, Timeout, ReportTo)
  when is_record(Request, request), is_record(Dst, sipdst), is_list(Branch), is_integer(Timeout),
       is_pid(ReportTo); ReportTo == none ->
    case clienttransaction:start_link(Request, Dst, Branch, Timeout, ReportTo) of
	{ok, CPid} ->
	    CPid;
	E ->
	    logger:log(error, "Transaction layer: Failed starting client transaction : ~p", [E]),
	    {error, "Failed starting client transaction"}
    end.

%%--------------------------------------------------------------------
%% @spec    (Pid, Reason, ExtraHeaders) ->
%%            ok         |
%%            {error, E}
%%
%%            Pid          = pid() "client transaction pid"
%%            Reason       = string() "will be logged by client transaction"
%%            ExtraHeaders = [{Key, Value}] "headers to put in any CANCELs we send"
%%
%%            E = string() "description of error"
%%
%% @doc     Store the to-tag we use when sending non-2xx responses in
%%          INVITE server transactions. We need to do this to
%%          correctly match ACK to the server transaction.
%% @end
%%--------------------------------------------------------------------
cancel_client_transaction(Pid, Reason, ExtraHeaders) when is_pid(Pid), is_list(Reason), is_list(ExtraHeaders) ->
    gen_server:cast(Pid, {cancel, Reason, ExtraHeaders}).

%%--------------------------------------------------------------------
%% @spec    (Request, ToTag) ->
%%            ok         |
%%            {error, E}
%%
%%            Request = #request{}
%%            ToTag   = string()
%%
%%            E = string() "description of error"
%%
%% @doc     Store the to-tag we use when sending non-2xx responses in
%%          INVITE server transactions. We need to do this to
%%          correctly match ACK to the server transaction.
%% @end
%%--------------------------------------------------------------------
store_to_tag(Request, ToTag) when is_record(Request, request), is_list(ToTag) ->
    case get_server_transaction(Request) of
	none ->
	    {error, "Transaction not found"};
	ThisState when is_record(ThisState, transactionstate) ->
	    NewTState = transactionstatelist:set_response_to_tag(ThisState, ToTag),
	    ok = transactionstatelist:update_transactionstate(NewTState),
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, ToTag) ->
%%            ok         |
%%            {error, E}
%%
%%            Request = #request{}
%%            ToTag   = string()
%%
%%            E = string() "description of error"
%%
%% @doc     Set the informational result parameter of a transaction.
%%          This 'result' value is only used for debugging/
%%          informational purposes.
%% @end
%%--------------------------------------------------------------------
set_result(Request, Value) when is_record(Request, request), is_list(Value) ->
    case get_server_transaction(Request) of
	none ->
	    {error, "Transaction not found"};
	ThisState when is_record(ThisState, transactionstate) ->
	    NewTState = transactionstatelist:set_result(ThisState, Value),
	    ok = transactionstatelist:update_transactionstate(NewTState),
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, ToTag) ->
%%            ok         |
%%            {error, E}
%%
%%            Request = #request{}
%%            Value   = term()
%%
%%            E = string() "description of error"
%%
%% @doc     Store some arbitrary data associated with this transcation
%%          for an application. The YXA stack never uses this data -
%%          it is just provided as convenient storage for application
%%          writers.
%% @end
%%--------------------------------------------------------------------
store_appdata(Request, Value) when is_record(Request, request) ->
    case get_server_transaction(Request) of
	none ->
	    {error, "Transaction not found"};
	ThisState when is_record(ThisState, transactionstate) ->
	    NewTState = transactionstatelist:set_appdata(ThisState, Value),
	    ok = transactionstatelist:update_transactionstate(NewTState),
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (TH) ->
%%            true  |
%%            false
%%
%%            TH = #thandler{} | term()
%%
%% @doc     Check if a given argument is a thandler record() that
%%          refers to a transaction handler that is still alive.
%% @end
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
%% @spec    (TH) ->
%%            Pid  |
%%            none
%%
%%            TH = #thandler{} | term()
%%
%%            Pid = pid()
%%
%% @doc     Sometimes it is actually necessary for something besides
%%          the transaction layer to know the pid handling a
%%          transaction. In those cases, this function lets a caller
%%          extract the pid of a transaction handler. Note though
%%          that you should NEVER communicate with that pid directly.
%% @end
%%--------------------------------------------------------------------
get_pid_from_handler(TH) when is_record(TH, thandler) ->
    TH#thandler.pid;
get_pid_from_handler(_) ->
    none.

%%--------------------------------------------------------------------
%% @spec    (Request, Type, Stale, RetryAfter) ->
%%            ok         |
%%            {error, E}
%%
%%            Request    = #request{}
%%            Type       = www | proxy
%%            Stale      = true | false
%%            RetryAfter = integer()
%%
%%            E = string()
%%
%% @doc     Locate a server transaction handler using Request, then
%%          invoke send_challenge() with the rest of our parameters.
%% @end
%%--------------------------------------------------------------------
send_challenge_request(Request, Type, Stale, RetryAfter) ->
    case get_handler_for_request(Request) of
	TH when is_record(TH, thandler) ->
	    send_challenge(TH, Type, Stale, RetryAfter);
	_ ->
	    {error, "Could not locate handler for request"}
    end.

%%--------------------------------------------------------------------
%% @spec    (TH, Type, Stale, RetryAfter) -> ok
%%
%%            TH         = #thandler{}
%%            Type       = www | proxy
%%            Stale      = true | false
%%            RetryAfter = integer()
%%
%% @doc     Generate a '407 Proxy-Authenticate' or '401
%%          WWW-Authenticate' response and hand this to a server
%%          transaction handler. If given a request record() as In,
%%          first locate the real server transaction handler.
%% @end
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
    case yxa_config:get_env(stateless_challenges) of
	{ok, true} ->
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
	{ok, false} ->
	    true
    end,
    ok.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Make the transaction layer log info about all it's
%%          transactions to the debug log.
%% @end
%%--------------------------------------------------------------------
debug_show_transactions() ->
    gen_server:cast(transactionlayer, {debug_show_transactions, self()}),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) ->
%%            {pass_to_core, AppModule, NewYxaCtx} |
%%            continue
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%%            AppModule = atom() "YXA application module name"
%%            NewYxaCtx = #yxa_ctx{}
%%
%% @doc     The transport layer passes us a request it has just
%%          received.
%% @end
%%--------------------------------------------------------------------
from_transportlayer(Request, YxaCtx) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    case get_server_transaction_pid(Request) of
	none ->
	    {ok, AppModule} = yxa_config:get_env(yxa_appmodule),
	    case received_new_request(Request, YxaCtx, AppModule) of
		{pass_to_core, NewAppModule, STPid} when is_atom(NewAppModule) ->
		    NewYxaCtx =
			case STPid of
			    _ when is_pid(STPid) ->
				YxaCtx#yxa_ctx{thandler = #thandler{pid = STPid}};
			    undefined ->
				YxaCtx#yxa_ctx{thandler = undefined}
			end,
		    case get_dialog_handler(Request) of
			nomatch ->
			    {pass_to_core, NewAppModule, NewYxaCtx};
			DCPid when is_pid(DCPid) ->
			    pass_to_dialog_controller(DCPid, STPid, Request, NewYxaCtx),
			    continue
		    end;
		continue ->
		    continue
	    end;
	STPid when is_pid(STPid) ->
	    Origin = YxaCtx#yxa_ctx.origin,
	    gen_server:cast(STPid, {siprequest, Request, Origin}),
	    continue
    end;

%%--------------------------------------------------------------------
%% @spec    (Response, YxaCtx) ->
%%            {pass_to_core, AppModule, NewYxaCtx} |
%%            continue
%%
%%            Response = #response{}
%%            YxaCtx   = #yxa_ctx{}
%%
%%            AppModule = atom() "YXA application module name"
%%            NewYxaCtx = #yxa_ctx{}
%%
%% @doc     The transport layer passes us a response it has just
%%          received.
%% @end
%%--------------------------------------------------------------------
from_transportlayer(Response, YxaCtx) when is_record(Response, response), is_record(YxaCtx, yxa_ctx) ->
    case get_client_transaction_pid(Response) of
	none ->
	    {_CSeqNum, Method} = sipheader:cseq(Response#response.header),
	    from_transportlayer_response_no_transaction(Method, Response, YxaCtx);
	CTPid when is_pid(CTPid) ->
	    logger:log(debug, "Transaction layer: Passing response '~p ~s' to client transaction (~p)",
		       [Response#response.status, Response#response.reason, CTPid]),
	    #yxa_ctx{origin = Origin,
		     logstr = LogStr
		    } = YxaCtx,
	    gen_server:cast(CTPid, {sipmessage, Response, Origin, LogStr}),
	    continue
    end.

%% part of from_transportlayer/1
%% Returns : continue | {pass_to_core, AppModule, NewYxaCtx}
from_transportlayer_response_no_transaction(Method, Response, _YxaCtx) when is_list(Method), Method /= "INVITE" ->
    %% RFC4320 #4.2 (Action 2)
    %% A transaction-stateful SIP proxy MUST NOT send any response to a
    %% non-INVITE request unless it has a matching server transaction that
    %% is not in the Terminated state.  As a consequence, this proxy will
    %% not forward any "late" non-INVITE responses.
    %%
    %% YXA applications does not have to be proxys, but I can't think of a reason why a
    %% transaction user would want to get responses at all.
    logger:log(debug, "Transaction layer: No transaction for received response to non-INVITE request ~s : "
	       "'~p ~s', dropping response (RFC4320 #4.2)",
	      [Method, Response#response.status, Response#response.reason]),
    continue;
from_transportlayer_response_no_transaction("INVITE", Response, YxaCtx) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    {ok, AppModule} = yxa_config:get_env(yxa_appmodule),
    case get_dialog_handler(Response) of
	nomatch ->
	    logger:log(debug, "Transaction layer: No state for received response '~p ~s', "
		       "passing to ~p:response(...).", [Status, Reason, AppModule]),
	    {pass_to_core, AppModule, YxaCtx};
	DCPid when is_pid(DCPid) ->
	    case is_process_alive(DCPid) of
		true ->
		    logger:log(debug, "Transaction layer: Passing response '~p ~s' to dialog controller ~p",
			       [Status, Reason, DCPid]),
		    DCPid ! {new_response, Response, YxaCtx};
		false ->
		    logger:log(debug, "Transaction layer: Dialog controller ~p dead, ignoring response '~p ~s'",
			       [DCPid, Status, Reason])
	    end,
	    continue
    end.

%% part of from_transportlayer (request).
%% Returns : void()
pass_to_dialog_controller(DCPid, STPid, Request, YxaCtx) when is_record(Request, request) ->
    logger:log(debug, "Transaction layer: Passing request to dialog controller ~p", [DCPid]),
    case is_process_alive(DCPid) of
	true ->
	    %% If a server transaction has been started for this request, change it's
	    %% parent from this process to the dialog controller. Server transactions
	    %% are not started for ACKs since they are not real requests.
	    case is_pid(STPid) of
		true -> ok = change_transaction_parent(STPid, self(), DCPid);
		false -> ok
	    end,

	    Ref = make_ref(),
	    %% create a link bridge between server transaction and dialog controller
	    %% while we are in the process of turning the server transaction ownership
	    %% over to the dialog controller
	    true = link(STPid),
	    true = link(DCPid),
	    DCPid ! {new_request, self(), Ref, Request, YxaCtx},
	    receive
		{ok, DCPid, Ref} ->
		    true = unlink(DCPid),
		    true = unlink(STPid),
		    logger:log(debug, "Transaction layer: Terminating after having turned over "
			       "server transaction ~p to dialog controller ~p", [STPid, DCPid]),
		    ok
	    after
		5 * 1000 ->
		    logger:log(error, "Transaction layer: Dialog handler ~p alive, but not "
			       "responding - throwing '500 Server Internal Error'", [DCPid]),
		    throw({siperror, 500, "Server Internal Error"})
	    end;
	false ->
	    THandler = get_handler_for_request(Request),
	    logger:log(error, "Transaction layer: Dialog handler pid ~p not alive, answering "
		       "'481 Call/Transaction Does Not Exist'", [DCPid]),
	    send_response_handler(THandler, 481, "Call/Transaction Does Not Exist")
    end.

%%--------------------------------------------------------------------
%% @spec    (TH) ->
%%            {ok, ToTag}
%%
%%            TH = #thandler{} "server transaction handle"
%%
%%            ToTag = string()
%%
%% @doc     Get to tag that will we used in server transaction
%%          response
%% @end
%%--------------------------------------------------------------------
get_my_to_tag(TH) when is_record(TH, thandler) ->
    gen_server:call(TH#thandler.pid, get_my_to_tag, ?STORE_TIMEOUT).

%%--------------------------------------------------------------------
%% @spec    (Re) ->
%%            DCPid
%%
%%            Re = #request{} | #response{}
%%
%%            DCPid = pid() |
%%            nomatch       |
%%            error
%%
%% @doc     Get the dialog controller for a request or response.
%% @end
%%--------------------------------------------------------------------
get_dialog_handler(Re) when is_record(Re, request); is_record(Re, response) ->
    case sipdialog:get_dialog_controller(Re) of
	{ok, Pid} -> Pid;
	R -> R
    end.

%%--------------------------------------------------------------------
%% @spec    (Entity, From, To) ->
%%            ok              |
%%            {error, Reason}
%%
%%            Entity = #thandler{} | pid()
%%            From   = pid()
%%            To     = pid() | none
%%
%%            Reason = string()
%%
%% @doc     Change parent of a client or server transaction. Entity
%%          should be a thandler record for a server transaction, and
%%          a pid for a client transaction. A 'To' of 'none' is only
%%          applicable to client transactions.
%% @end
%%--------------------------------------------------------------------
change_transaction_parent(TH, From, To) when is_record(TH, thandler), is_pid(From), is_pid(To) ->
    %% server transaction
    gen_server:call(TH#thandler.pid, {change_parent, From, To}, ?STORE_TIMEOUT);
change_transaction_parent(Pid, From, To) when is_pid(Pid), is_pid(From), is_pid(To) orelse To == none ->
    %% client transaction, or server transaction if called from within the transaction layer
    gen_server:call(Pid, {change_parent, From, To}, ?STORE_TIMEOUT).


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
    ok.

%%--------------------------------------------------------------------
%% @spec    () -> #thandler{}
%%
%% @doc     Get fake transaction handler pointing at the calling
%%          process for use in test cases in other modules.
%% @private
%% @end
%%--------------------------------------------------------------------
test_get_thandler_self() ->
    #thandler{pid = self()}.
