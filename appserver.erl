-module(appserver).
-export([init/0, request/6, response/6]).

-include("sipproxy.hrl").

-record(state, {request, sipmethod, callhandler, forkpid, cancelled, completed}).

init() ->
    [[fun request/6, fun response/6], [user, numbers, phone], stateful, none].


request(Method, URI, Header, Body, Socket, FromIP) when Method == "REGISTER" ->
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    logger:log(normal, "Appserver: ~s Method not applicable here -> 403 Forbidden", [LogStr]),
    transactionlayer:send_response_request({Method, URI, Header, Body}, 403, "Forbidden");

request(Method, URI, Header, Body, Socket, FromIP) when Method == "ACK" ->
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    case local:get_user_with_contact(URI) of
	none ->
	    logger:log(debug, "Appserver: ~s -> no transaction state, unknown user, ignoring",
	    		[LogStr]),
	    ok;
	SIPuser ->
	    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (SIP user ~p)",
	    		[LogStr, SIPuser]),
	    transportlayer:send_proxy_request(none, {Method, URI, Header, Body}, URI, [])
    end;

request(Method, URI, Header, Body, Socket, FromIP) when Method == "CANCEL" ->
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    case local:get_user_with_contact(URI) of
	none ->
	    logger:log(debug, "Appserver: ~s -> CANCEL not matching any existing transaction received, " ++
	    	"answer 481 Call/Transaction Does Not Exist", [LogStr]),
	    transactionlayer:send_response_request({Method, URI, Header, Body}, 481, "Call/Transaction Does Not Exist");
	SIPuser ->
	    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (SIP user ~p)",
	    		[LogStr, SIPuser]),
	    transportlayer:send_proxy_request(none, {Method, URI, Header, Body}, URI, [])
    end;
	    

request(Method, URI, OrigHeader, Body, Socket, FromIP) ->
    Header = case sipserver:get_env(record_route, false) of
	true -> siprequest:add_record_route(OrigHeader);
	false -> OrigHeader
    end,
    create_session(Method, URI, Header, Body, Socket, FromIP).

create_session(Method, URI, Header, Body, Socket, FromIP) ->
    % create header suitable for answering the incoming request
    Request = {Method, URI, Header, Body},
    case keylist:fetch("Route", Header) of
	[] ->
	    case get_actions(URI) of
		nomatch ->
		    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
		    case local:get_user_with_contact(URI) of
			none ->
			    logger:log(normal, "Appserver: ~s -> 404 Not Found (no actions, unknown user)",
			    		[LogStr]),
			    transactionlayer:send_response_request(Request, 404, "Not Found");
			SIPuser ->
			    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (no actions found, SIP user ~p)",
			    		[LogStr, SIPuser]),
			    THandler = transactionlayer:get_handler_for_request(Request),
			    transportlayer:send_proxy_request(THandler, {Method, URI, Header, Body}, URI, [])
		    end;
		{Users, Actions} ->
		    logger:log(debug, "Appserver: User(s) ~p actions :~n~p", [Users, Actions]),
		    case transactionlayer:adopt_server_transaction(Request) of
			{error, E} ->
			    logger:log(error, "Appserver: Failed to adopt server transaction for request ~s ~s : ~p",
					[Method, sipurl:print(URI), E]),
			    throw({siperror, 500, "Server Internal Error"});
			CallHandler ->
			    CallBranch = transactionlayer:get_branch_from_handler(CallHandler),
			    case string:rstr(CallBranch, "-UAS") of
				0 ->
				    logger:log(error, "Appserver: Could not make branch base from branch ~p", [CallBranch]),
				    throw({siperror, 500, "Server Internal Error"});
				Index when integer(Index) ->
				    BranchBase = string:substr(CallBranch, 1, Index - 1),
				    fork_actions(BranchBase, CallBranch, CallHandler, Request, Socket, FromIP, Actions)
			    end
		    end
	    end;
	_ ->
	    logger:log(debug, "Appserver: Request ~s ~s has Route header. Forwarding statelessly.", [Method, sipurl:print(URI)]),
	    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
	    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (Route-header present)", [LogStr]),
	    THandler = transactionlayer:get_handler_for_request(Request),
	    transportlayer:send_proxy_request(THandler, {Method, URI, Header, Body}, URI, [])
    end.

response(Status, Reason, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    % RFC 3261 16.7 says we MUST act like a stateless proxy when no
    % transaction can be found
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    Response = {Status, Reason, Header, Body},
    case transactionlayer:get_server_handler_for_stateless_response(Response) of
	{error, E} ->
	    logger:log(error, "Failed getting server transaction for stateless response: ~p", [E]),
	    logger:log(normal, "Response to ~s: ~p ~s, failed fetching state - proxying", [LogStr, Status, Reason]),
	    transportlayer:send_proxy_response(none, Response);
	none ->
	    logger:log(normal, "Response to ~s: ~p ~s, found no state - proxying", [LogStr, Status, Reason]),
	    transportlayer:send_proxy_response(none, Response);
	TH ->
	    logger:log(debug, "Response to ~s: ~p ~s, server transaction ~p", [LogStr, Status, Reason, TH]),
	    transactionlayer:send_proxy_response_handler(TH, Response)
    end.

get_actions(URI) ->
    {User, Pass, Host, Port, Parameters} = URI,
    LookupURL = {User, none, Host, none, []},
    case local:get_users_for_url(LookupURL) of
	nomatch ->
	    nomatch;
	Users when list(Users) ->
	    logger:log(debug, "Appserver: Found user(s) ~p for URI ~s", [Users, sipurl:print(LookupURL)]),
	    case fetch_actions_for_users(Users) of
		[] -> nomatch;
		Actions when list(Actions) ->
		    WaitAction = #sipproxy_action{action=wait, timeout=sipserver:get_env(appserver_call_timeout, 40)},
		    {Users, lists:append(Actions, [WaitAction])}
	    end;
	Unknown ->
	    logger:log(error, "Appserver: Unexpected result from lookup_address_to_users(~p) : ~p",
	    		[sipurl:print(LookupURL), Unknown]),
	    throw({siperror, 500, "Server Internal Error"})
    end.

forward_call_actions([{Forwards, Timeout, Localring}], Actions) ->
    FwdTimeout = sipserver:get_env(appserver_forward_timeout, 40),
    Func = fun(Forward) ->
    		   #sipproxy_action{action=call, requri=Forward, timeout=FwdTimeout}
	   end,
    ForwardActions = lists:map(Func, Forwards),
    case Localring of
	true ->
	    WaitAction = #sipproxy_action{action=wait, timeout=Timeout},
	    lists:append(Actions, [WaitAction | ForwardActions]);
	_ ->
	    case Timeout of
		0 ->
		    ForwardActions;
		_ ->
		    WaitAction = #sipproxy_action{action=wait, timeout=Timeout},
		    lists:append(Actions, [WaitAction | ForwardActions])
	    end
    end.

fetch_actions_for_users(Users) ->
    Actions = fetch_users_locations_as_actions(Users),
    case local:get_forwards_for_users(Users) of
	nomatch ->
	    Actions;
	[] ->
	    Actions;
	{aborted, {no_exists, forward}} ->
	    Actions;
	Forwards when list(Forwards) ->
	    forward_call_actions(Forwards, Actions);
	Unknown ->
	    logger:log(error, "Appserver: Unexpected result from get_forwards_for_user(~p) in fetch_actions_for_users",
	    		[Users]),
	    throw({siperror, 500, "Server Internal Error"})
    end.

fetch_users_locations_as_actions(Users) ->
    case local:get_locations_for_users(Users) of
	nomatch ->
	    [];
	Locations when list(Locations) ->
	    locations_to_actions(Locations);
	Unknown ->
	    logger:log(error, "Appserver: Unexpected result from get_locations_for_users(~p) in fetch_users_locations_as_actions",
			[Users]),
	    throw({siperror, 500, "Server Internal Error"})
    end.

locations_to_actions(L) ->
    locations_to_actions2(L, []).
    
locations_to_actions2([], Res) ->
    Res;
locations_to_actions2([{Location, Flags, Class, Expire} | T], Res) ->
    Timeout = sipserver:get_env(appserver_call_timeout, 40),
    CallAction = #sipproxy_action{action=call, requri=Location, timeout=Timeout},
    locations_to_actions2(T, lists:append([CallAction], Res)).

fork_actions(BranchBase, CallBranch, CallHandler, Request, Socket, FromIP, Actions) ->
    {Method, URI, OrigHeader, _} = Request,
    ForkPid = sipserver:safe_spawn(fun start_actions/6, [BranchBase, self(), Request, Socket, FromIP, Actions]),
    logger:log(normal, "~s: Appserver: Forking request, ~p actions",
		[BranchBase, length(Actions)]),
    logger:log(debug, "Appserver: Starting up call, glue process ~p, CallHandler ~p, ForkPid ~p",
    		[self(), CallHandler, ForkPid]),
    InitState = #state{request=Request, sipmethod=Method, callhandler=CallHandler, forkpid=ForkPid, cancelled=false, completed=false},
    process_messages(InitState),
    logger:log(normal, "Appserver glue: Finished with call (~s ~s), exiting.", [Method, sipurl:print(URI)]),
    ok.

start_actions(BranchBase, GluePid, OrigRequest, Socket, FromIP, Actions) ->
    {Method, URI, OrigHeader, _} = OrigRequest,
    Timeout = 32,
    % We don't return from fork() until all Actions are done, and fork() signals GluePid
    % when it is done.
    sipproxy:fork(BranchBase, GluePid, OrigRequest, Actions, Timeout),
    logger:log(debug, "Appserver glue: fork() of request ~s ~s done, start_actions() returning", [Method, sipurl:print(URI)]),
    true.


% Receive messages from branch pids and do whatever is necessary with them
process_messages(State) when record(State, state), State#state.callhandler == none, State#state.forkpid == none -> 
    logger:log(debug, "Appserver glue: Both CallHandler and ForkPid are 'none' - terminating"),
    ok;
process_messages(State) when record(State, state) ->
    CallHandler = State#state.callhandler,
    ForkPid = State#state.forkpid,
    CallHandlerPid = transactionlayer:get_pid_from_handler(CallHandler),

    {Res, NewState} = receive

	{sipproxy_response, ForkPid, Branch, Response} ->
	    NewState1 = handle_sipproxy_response(Response, State),
	    {ok, NewState1};

	{servertransaction_cancelled, CallHandlerPid} ->
	    logger:log(debug, "Appserver glue: Original request has been cancelled, sending 'cancel_pending' to ForkPid ~p and entering state 'cancelled'",
	    			[ForkPid]),
	    util:safe_signal("Appserver: ", ForkPid, {cancel_pending}),
	    NewState1 = State#state{cancelled=true},
	    {ok, NewState1};
	    
	{all_terminated, FinalResponse} ->
	    NewState1 = case State#state.cancelled of
		true ->
		    logger:log(debug, "Appserver glue: received all_terminated - request was cancelled. " ++
		    		      "Ask CallHandler ~p to send 487 Request Terminated and entering state 'completed'",
		    		       [CallHandler]),
		    transactionlayer:send_response_handler(CallHandler, 487, "Request Cancelled"),
		    State#state{completed=true};
		_ ->
		    case FinalResponse of
			{Status, Reason, _, _} ->
			    logger:log(debug, "Appserver glue: received all_terminated with a ~p ~s response (completed: ~p)",
			    		       [Status, Reason, State#state.completed]),
	    		    NewState2 = handle_sipproxy_response(FinalResponse, State),
	    		    NewState2;
	    		{Status, Reason} ->
			    logger:log(debug, "Appserver glue: received all_terminated - asking CallHandler ~p to answer ~p ~s",
			    		       [CallHandler, Status, Reason]),
			    transactionlayer:send_response_handler(CallHandler, Status, Reason),
			    % XXX check that this is really a final response?
			    State#state{completed=true};
			none ->
			    logger:log(debug, "Appserver glue: received all_terminated with no final answer"),
			    State
	    	    end
	    end,
	    {ok, NewState1};

	{no_more_actions} ->
	    NewState1 = case State#state.cancelled of
		true ->
		    logger:log(debug, "Appserver glue: received no_more_actions when cancelled. Ask CallHandler ~p to send 487 Request Terminated",
		    		       [CallHandler]),
		    transactionlayer:send_response_handler(CallHandler, 487, "Request Terminated"),
		    State#state{completed=true};
		_ ->
		    case State#state.completed of
			false ->
			    {Method, URI, _, _} = State#state.request,
			    logger:log(debug, "Appserver glue: received no_more_actions when NOT cancelled (and not completed), " ++
					      "responding 408 Request Timeout to original request ~s ~s",
					      [Method, sipurl:print(URI)]),
			    transactionlayer:send_response_handler(CallHandler, 408, "Request Timeout"),
			    State#state{completed=true};
			_ ->
			    logger:log(debug, "Appserver glue: received no_more_actions when already completed - ignoring"),
			    State
		    end
	    end,
	    {ok, NewState1};

	{callhandler_terminating, ForkPid} ->
	    logger:log(debug, "Appserver glue: received callhandler_terminating from my ForkPid ~p (CallHandlerPid is ~p) - setting ForkPid to 'none'",
			[ForkPid, CallHandlerPid]),
	    NewState1 = State#state{forkpid=none},
	    NewState2 = case util:safe_is_process_alive(CallHandler) of
		{true, _} ->
		    case NewState1#state.completed of
			false ->
			    logger:log(error, "Appserver glue: No answer to original request, answer 500 Server Internal Error"),
			    transactionlayer:send_response_handler(CallHandler, 500, "Server Internal Error"),
			    NewState1#state{completed=true};
			_ ->
			    NewState1
		    end;
		_ ->
		    NewState1
	    end,
	    {ok, NewState2};

	{servertransaction_terminating, CallHandlerPid} ->
	    logger:log(debug, "Appserver glue: received serverbranch_terminating from my CallHandlerPid ~p (ForkPid is ~p) - " ++
			      "setting CallHandler to 'none'", [CallHandlerPid, ForkPid]),
	    NewState1 = State#state{callhandler=none},
	    {ok, NewState1};
	    
	{quit} ->
	    {quit, State};
	    
	Unknown ->
	    logger:log(error, "Appserver: Received unknown signal in process_messages() :~n~p", [Unknown]),
	    {error, State}
	    
    after
	300 * 1000 ->
	    logger:log(debug, "Appserver glue: Still alive after 5 minutes! CallHandler ~p, ForkPid ~p", [CallHandler, ForkPid]),
	    util:safe_signal("Appserver: ", ForkPid, {showtargets}),
	    NewCallHandler = garbage_collect_transaction("CallHandler", CallHandler),
	    NewForkPid = garbage_collect_pid("ForkPid", ForkPid),
	    NewState1 = State#state{callhandler=NewCallHandler, forkpid=NewForkPid},
	    {error, NewState1}
    end,
    case Res of
	quit ->
	    logger:log(debug, "Appserver glue: Quitting process_messages()"),
	    ok;
	_ ->
	    process_messages(NewState)
    end.    

handle_sipproxy_response(Response, State) when record(State, state), State#state.sipmethod == "INVITE", State#state.completed == true ->
    {Method, URI, _, _} = State#state.request,
    {Status, Reason, _, _} = Response,
    if
	Status >= 200, Status =< 299 ->
	    % XXX change to debug level
	    logger:log(normal, "Appserver glue: Forwarding 'late' 2xx response, ~p ~s to INVITE ~s statelessly",
			[Status, Reason, sipurl:print(URI)]),
	    transportlayer:send_proxy_response(none, Response);
	true ->
	    logger:log(error, "Appserver glue: NOT forwarding non-2xx response ~p ~s to INVITE ~s - " ++
			"a final response has already been forwarded (sipproxy should not do this!)",
			[Status, Reason, sipurl:print(URI)])
    end,
    State;
handle_sipproxy_response(Response, State) when record(State, state), State#state.completed == true ->
    {Method, URI, _, _} = State#state.request,
    {Status, Reason, _, _} = Response,
    logger:log(error, "Appserver glue: NOT forwarding response ~p ~s to non-INVITE request ~s ~s - " ++
			"a final response has already been forwarded (sipproxy should not do this!)",
			[Status, Reason, Method, sipurl:print(URI)]),
    State;
handle_sipproxy_response(Response, State) when record(State, state) ->
    {Method, URI, _, _} = State#state.request,
    {Status, Reason, _, _} = Response,
    CallHandler = State#state.callhandler,
    logger:log(debug, "Appserver glue: Forwarding response ~p ~s to ~s ~s to CallHandler ~p",
		[Status, Reason, Method, sipurl:print(URI), CallHandler]),
    transactionlayer:send_proxy_response_handler(CallHandler, Response),
    if
	Status >= 200 ->
	    State#state{completed=true};
	true ->
	    State
    end.

garbage_collect_pid(Descr, none) ->
    none;
garbage_collect_pid(Descr, Pid) ->
    case util:safe_is_process_alive(Pid) of
	{true, _}  ->
	    Pid;
	_ ->
	    logger:log(error, "Appserver glue: ~s ~p is not alive, garbage collected.",
	    		[Descr, Pid]),
	    none
    end.

garbage_collect_transaction(Descr, TH) ->
    case transactionlayer:is_good_transaction(TH) of
	true ->
	    TH;
	_ ->
	    logger:log(error, "Appserver glue: ~s transaction is not alive, garbage collected.",
	    		[Descr]),
	    none
    end.

% XXX Store Route-Set when creating early or completed dialogue
%
% Reject requests with too low Max-Forwards before forking
