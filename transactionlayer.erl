-module(transactionlayer).
-export([start/3, check_alive/3, send_response_request/3, send_response_request/4,
	 transaction_terminating/1, get_handler_for_request/1,
	 get_branch_from_handler/1, start_client_transaction/6,
	 store_to_tag/2, adopt_server_transaction/1,
	 send_response_handler/3, send_response_handler/4,
	 send_proxy_response_handler/2, get_server_handler_for_stateless_response/1,
	 store_stateless_response_branch/3, is_good_transaction/1,
	 get_pid_from_handler/1]).

-include("transactionstatelist.hrl").

-record(thandler, {pid}).

start(RequestFun, ResponseFun, Mode) ->
    sipserver:safe_spawn(fun spawned/3, [RequestFun, ResponseFun, Mode]).

check_alive(RequestFun, ResponseFun, Mode) ->
    case util:safe_is_process_alive(transaction_layer) of
	{true, Pid} ->
	    true;
	{false, Pid} ->
	    logger:log(error, "Transaction layer: Service pid ~p not alive, attempting restart.",
			[Pid]),
	    start(RequestFun, ResponseFun, Mode)
    end.

spawned(RequestFun, ResponseFun, Mode) ->
    register(transaction_layer, self()),
    TStateList = transactionstatelist:empty(),
    receive_signals(TStateList, RequestFun, ResponseFun, Mode).

receive_signals(TStateList, RequestFun, ResponseFun, Mode) ->
    Res = receive

	{sipmessage, FromPid, MsgRef, request, Request, Socket, LogStr} ->
	    NewTStateList1 = case get_server_transaction_pid(request, Request, TStateList) of
		nohandler ->
		    % XXX is this really needed? Transactions that have no handlers? Handlers are spawned automatically for
		    % server transactions below.
		    logger:log(debug, "Transaction layer: No handler registered for received request, passing to RequestFun ~p.", [RequestFun]),
		    util:safe_signal("Transaction layer: ", FromPid, {pass_to_core, MsgRef, RequestFun}),
		    TStateList;
		nostate ->
		    received_new_request(FromPid, MsgRef, Request, Socket, LogStr, RequestFun, Mode, TStateList);
		{handlerdead, TPid} ->
		    logger:log(error, "Transaction layer: Process ~p registered to receive this request is not alive! Passing to RequestFun ~p.",
				[TPid, RequestFun]),
		    util:safe_signal("Transaction layer: ", FromPid, {pass_to_core, MsgRef, RequestFun}),
		    TStateList;
		TPid when pid(TPid) ->
		    util:safe_signal("Transaction layer: ", TPid, {sipmessage, self(), request, Request, Socket, LogStr, FromPid, MsgRef, RequestFun}),
		    TStateList
	    end,
	    {ok, NewTStateList1};

	{sipmessage, FromPid, MsgRef, response, Response, Socket, LogStr} ->
	    case get_client_transaction_pid(response, Response, TStateList) of
		nohandler ->
		    logger:log(debug, "Transaction layer: No handler registered for received response, passing to ResponseFun."),
		    util:safe_signal("Transaction layer: ", FromPid, {pass_to_core, MsgRef, ResponseFun});
		nostate ->
		    logger:log(debug, "Transaction layer: No state for received response, passing to ResponseFun."),
		    util:safe_signal("Transaction layer: ", FromPid, {pass_to_core, MsgRef, ResponseFun});
		{handlerdead, TPid} ->
		    logger:log(error, "Transaction layer: Process ~p registered to receive this response is not alive! Passing to ResponseFun.",
				[TPid]),
		    util:safe_signal("Transaction layer: ", FromPid, {pass_to_core, MsgRef, ResponseFun});
		TPid when pid(TPid) ->
		    {Status, Reason, _, _} = Response,
		    logger:log(debug, "Transaction layer: Passing response ~p ~s to registered handler ~p",
				[Status, Reason, TPid]),
		    util:safe_signal("Transaction layer: ", TPid, {sipmessage, self(), response, Response, Socket, LogStr}),
		    util:safe_signal("Transaction layer: ", FromPid, {continue, MsgRef})
	    end,
	    {ok, TStateList};

	{store_stateless_response_branch, FromPid, Pid, Branch, Method} ->
	    case transactionstatelist:get_server_transaction_using_stateless_response_branch(Branch, Method, TStateList) of
		none ->
		    case transactionstatelist:get_server_transaction_using_pid(Pid, TStateList) of
			{error, E} ->
			    logger:log(error, "Transaction layer: Error associating stateless response branch ~p with pid ~p~n: ~p",
					[Branch, Pid, E]),
			    util:safe_signal("Transaction layer: ", FromPid,
					{failed_storing_stateless_response_branch, self(), E}),
			    {ok, TStateList};
			{none} ->
			    logger:log(error, "Transaction layer: Can't associate stateless response branch ~p with unknown transaction handler ~p",
					[Branch, Pid]),
			    util:safe_signal("Transaction layer: ", FromPid,
					{failed_storing_stateless_response_branch, self(), "server transaction not found"}),
			    {ok, TStateList};
			ThisState when record(ThisState, transactionstate) ->
			    NewTState = transactionstatelist:append_response_branch(ThisState, Branch, Method),
			    NewTStateList = transactionstatelist:update_transactionstate(NewTState, TStateList),
			    util:safe_signal("Transaction layer: ", FromPid, {stored_stateless_response_branch, self()}),
			    {ok, NewTStateList}
		    end;
		ThisState when record(ThisState, transactionstate) ->
		    case ThisState#transactionstate.pid of
			Pid ->
			    % already associated with that very same server transaction, this is not an error since
			    % it can happen when a stateless application receives retransmissions of a request
			    util:safe_signal("Transaction layer: ", FromPid, {stored_stateless_response_branch, self()});
			OtherPid ->
			    logger:log(error, "Transaction layer: Asked to associate branch ~p with transaction with pid ~p, " ++
					"but branch is already associated with transaction handled by ~p!", [Branch, Pid, OtherPid]),
			    util:safe_signal("Transaction layer: ", FromPid, {failed_storing_stateless_response_branch, self(),
					"branch already associated with another server transaction"})
		    end,
		    {ok, TStateList}
	    end;	

	{unregister_pid, Pid} ->
	    {ok, transactionstatelist:delete_using_pid(Pid, TStateList)};

	    
	{get_server_transaction_handler, FromPid, Request} ->
	    case get_server_transaction_pid(request, Request, TStateList) of
		nohandler ->
		    util:safe_signal("Transaction layer: ", FromPid, {failed_getting_server_transaction_handler, "No handler found"});
		nostate ->
		    util:safe_signal("Transaction layer: ", FromPid, {failed_getting_server_transaction_handler, "No state found"});
		{handlerdead, TPid} ->
		    util:safe_signal("Transaction layer: ", FromPid, {failed_getting_server_transaction_handler, "Server transaction handler dead"});
		TPid when pid(TPid) ->
		    util:safe_signal("Transaction layer: ", FromPid, {got_server_transaction_handler, self(), TPid})
	    end,
	    {ok, TStateList};

	{get_server_transaction_handler_for_response, FromPid, Branch, Method} ->
	    case transactionstatelist:get_server_transaction_using_stateless_response_branch(Branch, Method, TStateList) of
		none ->
		    util:safe_signal("Transaction layer: ", FromPid, {failed_getting_server_transaction_for_response, self(), nomatch});
		ThisState when record(ThisState, transactionstate) ->
		    TPid = ThisState#transactionstate.pid,
		    util:safe_signal("Transaction layer: ", FromPid,
				{got_server_transaction_handler_for_response, self(), TPid})
	    end,
	    {ok, TStateList};

	{start_client_transaction, FromPid, Request, SocketIn, Dst, Branch, Timeout, Parent} ->
	    {Method, _, _, _} = Request,
	    NewTStateList1 = case transactionstatelist:get_client_transaction(Method, Branch, TStateList) of
		none ->
		    CPid = clienttransaction:start(Request, SocketIn, Dst, Branch, Timeout, Parent),
		    util:safe_signal("Transaction layer: ", FromPid, {started_client_transaction, self(), CPid}),
		    transactionstatelist:add_client_transaction(Method, Branch, CPid, TStateList);
		_ ->
		    logger:log(error, "Transaction layer: Can't start duplicate client transaction"),
		    util:safe_signal("Transaction layer: ", FromPid, {failed_starting_client_transaction, self(), "Transaction already exists"}),
		    TStateList	
	    end,
	    {ok, NewTStateList1};

	{store_to_tag, FromPid, Request, ToTag} ->
	    case get_server_transaction(request, Request, TStateList) of
		nostate ->
		    util:safe_signal("Transaction layer: ", FromPid, {failed_storing_totag, self(), "Transaction not found"}),
		    {ok, TStateList};
		ThisState when record(ThisState, transactionstate) ->
		    NewTState = transactionstatelist:set_response_to_tag(ThisState, ToTag),
		    NewTStateList = transactionstatelist:update_transactionstate(NewTState, TStateList),
		    util:safe_signal("Transaction layer: ", FromPid, {stored_to_tag, self()}),
		    {ok, NewTStateList}
	    end;

	%{register_appdata, Pid, Type, RequestOrResponse, AppName, AppData} ->
	%    case get_server_transaction(Type, RequestOrResponse, TStateList) of
	%	nostate ->
	%	    util:safe_signal("Transaction layer: ", Pid, {failed_registering_appdata, 
	%		self(), Type, RequestOrResponse, "No state found for request/response"}),
	%	    {ok, TStateList};		
	%	TState ->
	%	    [OldAppDataList] = transactionstatelist:extract([appdata], TState),
	%	    NewAppDataList = lists:keyreplace(AppName, 1, OldAppDataList, {AppName, AppData}),
	%	    NewTState = transactionstatelist:set_appdata(TState, NewAppDataList),
	%	    NewTStateList = transactionstatelist:update_transactionstate(TState, TStateList),
	%	    util:safe_signal("Transaction layer: ", Pid, {appdata_registered, self()}),
	%	    {ok, NewTStateList}
	%    end;

	{quit} ->
	    logger:log(debug, "Transport layer: Received signal to quit"),
    	    {quit};
    	Unknown ->
	    logger:log(error, "Tranport layer: Received unknown signal in receive_signals() : ~p", [Unknown]),
	    {error, TStateList}
    after
	7 * 1000 ->
	    % Wake up every 7 seconds to do garbage collection (delete_expired() below)
	    {ok, TStateList}
    end,
    case Res of
	{quit} ->
	    ok;
	{_, NewTStateList1_1} when record(NewTStateList1_1, transactionstatelist) ->
	    NewTStateList1_2 = transactionstatelist:delete_expired(NewTStateList1_1),
	    receive_signals(NewTStateList1_2, RequestFun, ResponseFun, Mode);
	UnknownRes ->
	    logger:log(error, "Transaction layer: Unknown result from receive_signals() : ~p", [UnknownRes]),
	    receive_signals(TStateList, RequestFun, ResponseFun, Mode)
    end.

received_new_request(FromPid, MsgRef, {"ACK", URI, _, _}, Socket, LogStr, RequestFun, Mode, TStateList) ->
    logger:log(debug, "Transaction layer: Received ACK ~s that does not match any existing transaction, passing to core.",
		[sipurl:print(URI)]),
    util:safe_signal("Transaction layer: ", FromPid, {pass_to_core, MsgRef, RequestFun}),
    TStateList;
received_new_request(FromPid, MsgRef, {"CANCEL", URI, Header, Body}, Socket, LogStr, RequestFun, stateless, TStateList) ->
    logger:log(debug, "Transaction layer: Stateless received CANCEL ~s. Starting transaction but passing to core.",
		[sipurl:print(URI)]),
    Request = {"CANCEL", URI, Header, Body},
    case servertransaction:start(Request, Socket, LogStr, RequestFun, stateless) of
	STPid when pid(STPid) ->
	    NewTStateList2 = transactionstatelist:add_server_transaction(Request, STPid, TStateList),
	    util:safe_signal("Transaction layer: ", FromPid, {pass_to_core, MsgRef, RequestFun}),
	    NewTStateList2;
	E ->
	    logger:log(error, "Transaction layer: Failed starting server transaction (~p) - ignoring request", [E]),
	    util:safe_signal("Transaction layer: ", FromPid, {continue, MsgRef}),
	    TStateList
    end;
received_new_request(FromPid, MsgRef, Request, Socket, LogStr, RequestFun, Mode, TStateList) ->
    logger:log(debug, "Transaction layer: No state for received request, starting new transaction"),
    case servertransaction:start(Request, Socket, LogStr, RequestFun, Mode) of
	STPid when pid(STPid) ->
	    NewTStateList2 = transactionstatelist:add_server_transaction(Request, STPid, TStateList),
	    PassToCore = case Request of
		{"CANCEL", URI, Header, _} ->
		    % XXX not only INVITE can be cancelled, RFC3261 9.2 says we should find the
		    % transaction that is being handled by 'assuming the method is anything but
		    % CANCEL or ACK'.
		    Invite = {"INVITE", URI, Header, ""},
		    case get_server_transaction_pid(request, Invite, TStateList) of
			InvitePid when pid(InvitePid) ->
			    logger:log(debug, "Transaction layer: CANCEL matches server transaction handled by ~p", [InvitePid]),
			    {Status, Reason} = case util:safe_is_process_alive(InvitePid) of
				{true, _} ->
				    logger:log(debug, "Transaction layer: Cancelling other transaction handled by ~p and responding 200 Ok",
						[InvitePid]),
				    util:safe_signal("Transaction layer: ", InvitePid, {cancelled, self()}),
				    {200, "Ok"};
				_ ->
				    logger:log(debug, "Transaction layer: Transaction to be cancelled handled by dead pid '~p', responding 481 Call/Transaction Does Not Exist",
				    			[InvitePid]),
				    {481, "Call/Transaction Does Not Exist"}
			    end,	
			    util:safe_signal("Transaction layer: ", STPid, {create_response, self(), Status, Reason, []}),
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
		    logger:log(debug, "Transaction layer: Telling sipserver ~p to apply this applications request function.",
				[FromPid]),
		    util:safe_signal("Transaction layer: ", FromPid, {pass_to_core, MsgRef, RequestFun});
		_ ->
		    util:safe_signal("Transaction layer: ", FromPid, {continue, MsgRef})
	    end,
	    NewTStateList2;
	E ->
	    logger:log(error, "Transaction layer: Failed starting server transaction (~p) - ignoring request", [E]),
	    util:safe_signal("Transaction layer: ", FromPid, {continue, MsgRef}),
	    TStateList
    end.

get_server_transaction(request, Request, TStateList) ->
    transactionstatelist:get_server_transaction_using_request(Request, TStateList);
get_server_transaction(response, Response, TStateList) ->
    transactionstatelist:get_server_transaction_using_response(Response, TStateList).

get_server_transaction_pid(Type, RequestOrResponse, TStateList) ->
    case get_server_transaction(Type, RequestOrResponse, TStateList) of
	none ->
	    nostate;	
	TState ->
	    case transactionstatelist:extract([pid], TState) of
		[none] ->
		    nohandler;	    
		[TPid] when pid(TPid) ->
		    case util:safe_is_process_alive(TPid) of
			{true, _} ->
			    TPid;
			{false, _} ->
			    {handlerdead, TPid}
		    end
	    end
    end.

get_client_transaction(response, Response, TStateList) ->
    {_, _, Header, _} = Response,
    TopVia = sipheader:topvia(Header),
    Branch = sipheader:get_via_branch(TopVia),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    transactionstatelist:get_client_transaction(Method, Branch, TStateList);
get_client_transaction(Foo, _, _) ->
    logger:log(error, "Transaction layer: Client transaction locating using '~p' is not implemented", [Foo]),
    none.

get_client_transaction_pid(Type, RequestOrResponse, TStateList) ->
    case get_client_transaction(Type, RequestOrResponse, TStateList) of
	none ->
	    nostate;	
	TState ->
	    case transactionstatelist:extract([pid], TState) of
		[none] ->
		    nohandler;	    
		[TPid] when pid(TPid) ->
		    case util:safe_is_process_alive(TPid) of
			{true, _} ->
			    TPid;
			{false, _} ->
			    {handlerdead, TPid}
		    end
	    end
    end.



% Application functions


send_response_request(Request, Status, Reason) ->
    send_response_request(Request, Status, Reason, []).

send_response_request(Request, Status, Reason, ExtraHeaders) ->
    {Method, URI, _, _} = Request,
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
	    send_response_handler(TH, Status, Reason, ExtraHeaders)
    end.

send_response_handler(TH, Status, Reason) when record(TH, thandler) ->
    send_response_handler(TH, Status, Reason, []).

send_response_handler(TH, Status, Reason, ExtraHeaders) when record(TH, thandler) ->
    case util:safe_is_process_alive(TH#thandler.pid) of
	{true, Pid} ->
	    Pid ! {create_response, self(), Status, Reason, ExtraHeaders},
	    ok;
	_ ->
	    {error, "Transaction handler not alive"}
    end.

send_proxy_response_handler(TH, Response) when record(TH, thandler) ->
    case util:safe_is_process_alive(TH#thandler.pid) of
	{true, Pid} ->
	    Pid ! {forwardresponse, Response},
	    ok;
	_ ->
	    {error, "Transaction handler not alive"}
    end.

get_handler_for_request(Request) ->
    case util:safe_is_process_alive(transaction_layer) of
	{true, TLPid} ->
	    TLPid ! {get_server_transaction_handler, self(), Request},
	    receive
	        {failed_getting_server_transaction_handler, TLPid, E} ->
		    {error, E};
	        {got_server_transaction_handler, TLPid, Pid} ->
		    #thandler{pid=Pid}
	    after
		2500 ->
		    {error, unavailable}
	    end;
	{false, _} ->
	    {error, unavailable}
    end.

get_server_handler_for_stateless_response(Response) ->
    {Status, Reason, Header, _} = Response,
    TopVia = sipheader:topvia(Header),
    {_, Method} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    case sipheader:get_via_branch(TopVia) of
	Branch when list(Branch) ->
	    case util:safe_is_process_alive(transaction_layer) of
		{true, TLPid} ->
		    TLPid ! {get_server_transaction_handler_for_response, self(), Branch, Method},
		    receive
			{failed_getting_server_transaction_for_response, TLPid, nomatch} ->
			    none;
			{failed_getting_server_transaction_for_response, TLPid, E} ->
			    {error, E};
			{got_server_transaction_handler_for_response, TLPid, Pid} ->
			    #thandler{pid=Pid}
		    after
			1000 ->
			    {error, unavailable}
		    end;
		{false, _} ->
		    {error, unavailable}
	    end;
	_ ->
	    {error, "No branch in top via of response"}
    end.

adopt_server_transaction(Request) ->
    case get_handler_for_request(Request) of
	TH when record(TH, thandler) -> 
	    Pid = TH#thandler.pid,
	    util:safe_signal("Transaction layer: ", Pid, {set_report_to, self()}),
	    receive
		{failed_setting_report_to, Pid, E} ->
		    {error, E};
		{set_report_to, Pid} ->
		    TH
	    after
		1000 ->
		    {error, unavailable}
	    end;
	Unknown ->
	   {error, "unknown result from get_handler_for_request"}
    end.

get_branch_from_handler(TH) when record(TH, thandler) ->
    Pid = TH#thandler.pid,
    util:safe_signal("Transaction layer: ", Pid, {get_branch, self()}),
    receive
	{got_branch, Pid, Branch} ->
	    Branch
    after
	500 ->
	    logger:log(error, "Transaction layer: Failed getting branch from pid ~p : timeout", [Pid]),
	    error
    end.

transaction_terminating(TransactionPid) ->
    case util:safe_is_process_alive(transaction_layer) of
	{true, TLPid} ->
	    TLPid ! {unregister_pid, TransactionPid};
	{false, TLPid} ->
	    logger:log(error, "Transaction layer: Transaction layer ~p is dead, no need to signal it about terminating transaction.",
			[TLPid])
    end,
    ok.

start_client_transaction(Request, SocketIn, Dst, Branch, Timeout, Parent) ->
    case util:safe_is_process_alive(transaction_layer) of
	{true, TLPid} ->
	    TLPid ! {start_client_transaction, self(), Request, SocketIn, Dst, Branch, Timeout, Parent},
	    receive
	        {failed_starting_client_transaction, TLPid, E} ->
		    {error, E};
	        {started_client_transaction, TLPid, Pid} ->
		    Pid
	    after
		2500 ->
		    {error, unavailable}
	    end;
	{false, _} ->
	    {error, unavailable}
    end.

store_to_tag(Request, ToTag) ->
    case util:safe_is_process_alive(transaction_layer) of
	{true, TLPid} ->
	    TLPid ! {store_to_tag, self(), Request, ToTag},
	    receive
	        {failed_storing_to_tag, TLPid, E} ->
		    {error, E};
	        {stored_to_tag, TLPid} ->
		    ok
	    after
		2500 ->
		    {error, unavailable}
	    end;
	{false, _} ->
	    {error, unavailable}
    end.

store_stateless_response_branch(none, Branch, Method) ->
    ok;
store_stateless_response_branch(TH, Branch, Method) when record(TH, thandler) ->
    HandlerPid = TH#thandler.pid,
    case util:safe_is_process_alive(transaction_layer) of
	{true, TLPid} ->
	    TLPid ! {store_stateless_response_branch, self(), HandlerPid, Branch, Method},
	    receive
	        {failed_storing_stateless_response_branch, TLPid, E} ->
		    {error, E};
	        {stored_stateless_response_branch, TLPid} ->
		    ok
	    after
		1000 ->
		    {error, unavailable}
	    end;
	{false, _} ->
	    {error, unavailable}
    end.

is_good_transaction(TH) when record(TH, thandler) ->
    util:safe_is_process_alive(TH#thandler.pid);
is_good_transaction(_) ->
    false.

get_pid_from_handler(TH) when record(TH, thandler) ->
    TH#thandler.pid;
get_pid_from_handler(_) ->
    none.
