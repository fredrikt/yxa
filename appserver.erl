-module(appserver).

%% Standard Yxa SIP-application exports
-export([init/0, request/3, response/3]).

-include("sipproxy.hrl").
-include("siprecords.hrl").
-include("sipsocket.hrl").

-record(state, {request, sipmethod, callhandler, forkpid, cancelled, completed}).

%%--------------------------------------------------------------------
%%% Standard Yxa SIP-application exported functions
%%--------------------------------------------------------------------


%% Function: init/0
%% Description: Yxa applications must export an init/0 function.
%% Returns: See XXX
%%--------------------------------------------------------------------
init() ->
    [[user, numbers, phone], stateful, none].


%% Function: request/3
%% Description: Yxa applications must export an request/3 function.
%% Returns: See XXX
%%--------------------------------------------------------------------

%%
%% REGISTER
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "REGISTER" ->
    logger:log(normal, "Appserver: ~s Method not applicable here -> 403 Forbidden", [LogStr]),
    transactionlayer:send_response_request(Request, 403, "Forbidden");

%%
%% ACK
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "ACK" ->
    case local:get_user_with_contact(Request#request.uri) of
	none ->
	    logger:log(debug, "Appserver: ~s -> no transaction state, unknown user, ignoring",
		       [LogStr]),
	    ok;
	SIPuser ->
	    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (SIP user ~p)",
		       [LogStr, SIPuser]),
	    transportlayer:send_proxy_request(none, Request, Request#request.uri, [])
    end;

%%
%% CANCEL
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "CANCEL" ->
    case local:get_user_with_contact(Request#request.uri) of
	none ->
	    logger:log(debug, "Appserver: ~s -> CANCEL not matching any existing transaction received, " ++
		       "answer 481 Call/Transaction Does Not Exist", [LogStr]),
	    transactionlayer:send_response_request(Request, 481, "Call/Transaction Does Not Exist");
	SIPuser ->
	    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (SIP user ~p)",
		       [LogStr, SIPuser]),
	    transportlayer:send_proxy_request(none, Request, Request#request.uri, [])
    end;

%%
%% Anything but REGISTER, ACK and CANCEL
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin) ->
    Header = case sipserver:get_env(record_route, false) of
		 true -> siprequest:add_record_route(Request#request.header, Origin);
		 false -> Request#request.header
	     end,
    NewRequest = Request#request{header=Header},
    case local:is_request_to_this_proxy(Request) of
	true ->
	    request_to_me(Request, LogStr);
	false ->
	    create_session(NewRequest, Origin, LogStr)
    end.


%% Function: response/3
%% Description: Yxa applications must export an response/3 function.
%% Returns: See XXX
%%--------------------------------------------------------------------
response(Response, Origin, LogStr) when record(Response, response), record(Origin, siporigin) ->
    %% RFC 3261 16.7 says we MUST act like a stateless proxy when no
    %% transaction can be found
    {Status, Reason} = {Response#response.status, Response#response.reason},
    case transactionlayer:get_server_handler_for_stateless_response(Response) of
	{error, E} ->
	    logger:log(error, "Failed getting server transaction for stateless response: ~p", [E]),
	    logger:log(normal, "appserver: Response to ~s: ~p ~s, failed fetching state - proxying", [LogStr, Status, Reason]),
	    transportlayer:send_proxy_response(none, Response);
	none ->
	    logger:log(normal, "appserver: Response to ~s: ~p ~s, found no state - proxying", [LogStr, Status, Reason]),
	    transportlayer:send_proxy_response(none, Response);
	TH ->
	    logger:log(debug, "appserver: Response to ~s: ~p ~s, server transaction ~p", [LogStr, Status, Reason, TH]),
	    transactionlayer:send_proxy_response_handler(TH, Response)
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: request_to_me(Request, LogTag) 
%%           Request = request record()
%%           LogTag  = string()
%% Descrip.: Request is meant for this proxy, if it is OPTIONS we
%%           respond 200 Ok, otherwise we respond 481 Call/
%%           transaction does not exist.
%% Returns : Does not matter.
%%--------------------------------------------------------------------
request_to_me(Request, LogTag) when is_record(Request, request), Request#request.method == "OPTIONS" ->
    logger:log(normal, "~s: appserver: OPTIONS to me -> 200 OK", [LogTag]),
    logger:log(debug, "XXX The OPTIONS response SHOULD include Accept, Accept-Encoding,"
	       " Accept-Language, and Supported headers. RFC 3261 section 11"),
    transactionlayer:send_response_request(Request, 200, "OK");

request_to_me(Request, LogTag) when is_record(Request, request) ->
    logger:log(normal, "~s: appserver: non-OPTIONS request to me -> 481 Call/Transaction Does Not Exist", 
	       [LogTag]),
    transactionlayer:send_response_request(Request, 481, "Call/Transaction Does Not Exist").


%%--------------------------------------------------------------------
%% Function: create_session(Request, Origin, LogStr)
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogStr  = string()
%% Descrip.: Request was not meant for this proxy itself - find out if
%%           this request is for one of our users, and if so find out
%%           what actions to perform for the request.
%% Returns : void() | throw({siperror, ...})
%%--------------------------------------------------------------------
create_session(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin) ->
    %% create header suitable for answering the incoming request
    URI = Request#request.uri,
    case keylist:fetch("Route", Request#request.header) of
	[] ->
	    case get_actions(URI) of
		nomatch ->
		    case local:get_user_with_contact(URI) of
			none ->
			    logger:log(normal, "Appserver: ~s -> 404 Not Found (no actions, unknown user)",
				       [LogStr]),
			    transactionlayer:send_response_request(Request, 404, "Not Found");
			SIPuser ->
			    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (no actions found, SIP user ~p)",
				       [LogStr, SIPuser]),
			    THandler = transactionlayer:get_handler_for_request(Request),
			    transportlayer:send_proxy_request(THandler, Request, URI, [])
		    end;
		{Users, Actions} ->
		    logger:log(debug, "Appserver: User(s) ~p actions :~n~p", [Users, Actions]),
		    case transactionlayer:adopt_server_transaction(Request) of
			{error, cancelled} ->
			    logger:log(normal, "Appserver: Request '~s ~s' has allready been cancelled, aborting.",
				       [Request#request.method, sipurl:print(URI)]),
			    ok;
			{error, E} ->
			    logger:log(error, "Appserver: Failed to adopt server transaction for request ~s ~s : ~p",
				       [Request#request.method, sipurl:print(URI), E]),
			    throw({siperror, 500, "Server Internal Error"});
			CallHandler ->
			    CallBranch = transactionlayer:get_branch_from_handler(CallHandler),
			    case string:rstr(CallBranch, "-UAS") of
				0 ->
				    logger:log(error, "Appserver: Could not make branch base from branch ~p", [CallBranch]),
				    throw({siperror, 500, "Server Internal Error"});
				Index when integer(Index) ->
				    BranchBase = string:substr(CallBranch, 1, Index - 1),
				    fork_actions(BranchBase, CallHandler, Request, Actions)
			    end
		    end
	    end;
	_ ->
	    logger:log(debug, "Appserver: Request ~s ~s has Route header. Forwarding statelessly.",
		       [Request#request.method, sipurl:print(URI)]),
	    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (Route-header present)", [LogStr]),
	    THandler = transactionlayer:get_handler_for_request(Request),
	    transportlayer:send_proxy_request(THandler, Request, URI, [])
    end.


%%--------------------------------------------------------------------
%% Function: get_actions(URI)
%%           URI = sipuri record()
%% Descrip.: Find the SIP user(s) for a URI and make a list() of
%%           sipproxy_action to take for a request destined for that
%%           user(s).
%% Returns : {UserList, ActionsList} | throw()
%%           UserList    = list() of SIP usernames (strings)
%%           ActionsList = list() of sipproxy_action record()
%%--------------------------------------------------------------------
get_actions(URI) when record(URI, sipurl) ->
    LookupURL = URI#sipurl{pass=none, port=none, param=[]},
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

%% forward_call_actions/2 helps fetch_actions_for_users/1 make a list of
%% sipproxy_action out of a list of forwards, a timeout value and
%% "concurrent ringing or not" information
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
	    %% ehh? better let the Unknown thing below handle this?
	    Actions;
	Forwards when list(Forwards) ->
	    forward_call_actions(Forwards, Actions);
	Unknown ->
	    logger:log(error, "Appserver: Unexpected result from get_forwards_for_user(~p) in fetch_actions_for_users : ~p",
		       [Users, Unknown]),
	    throw({siperror, 500, "Server Internal Error"})
    end.

fetch_users_locations_as_actions(Users) ->
    case local:get_locations_for_users(Users) of
	nomatch ->
	    [];
	Locations when list(Locations) ->
	    locations_to_actions(Locations);
	Unknown ->
	    logger:log(error, "Appserver: Unexpected result from get_locations_for_users(~p) in fetch_users_locations_as_actions : ~p",
		       [Users, Unknown]),
	    throw({siperror, 500, "Server Internal Error"})
    end.

locations_to_actions(L) ->
    locations_to_actions2(L, []).

locations_to_actions2([], Res) ->
    Res;
locations_to_actions2([{Location, Flags, Class, Expire} | T], Res) when record(Location, sipurl) ->
    Timeout = sipserver:get_env(appserver_call_timeout, 40),
    CallAction = #sipproxy_action{action=call, requri=Location, timeout=Timeout},
    locations_to_actions2(T, lists:append([CallAction], Res));
locations_to_actions2([H | T], Res) ->
    logger:log(error, "appserver: Illegal location in locations_to_actions2: ~p", [H]),
    locations_to_actions2(T, Res).

%%--------------------------------------------------------------------
%% Function: fork_actions(BranchBase, CallHandler, Request, Actions)
%%           BranchBase = string()
%%           CallHandler = term(), reference to server transaction
%%           Request = request record()
%%           Actions = list() of sipproxy_action record()
%% Descrip.: Start start_actions/4 in a new thread, then enter the
%%           process_messages() loop. Do not return until fork is
%%           done (meaning both server transaction and all client
%%           transactions are finished or dead). Execution process is
%%           the one we refer to as appserver glue process.
%% Returns : ok
%%--------------------------------------------------------------------
fork_actions(BranchBase, CallHandler, Request, Actions) when record(Request, request) ->
    {Method, URI} = {Request#request.method, Request#request.uri},
    ForkPid = sipserver:safe_spawn(fun start_actions/4, [BranchBase, self(), Request, Actions]),
    logger:log(normal, "~s: Appserver: Forking request, ~p actions",
	       [BranchBase, length(Actions)]),
    logger:log(debug, "Appserver: Forking request, glue process ~p, CallHandler ~p, ForkPid ~p",
	       [self(), CallHandler, ForkPid]),
    InitState = #state{request=Request, sipmethod=Method, callhandler=CallHandler, forkpid=ForkPid, cancelled=false, completed=false},
    process_messages(InitState),
    logger:log(normal, "Appserver glue: Finished with fork (~s ~s), exiting.", [Method, sipurl:print(URI)]),
    ok.

%%--------------------------------------------------------------------
%% Function: start_actions(BranchBase, GluePid, OrigRequest, Actions)
%%           BranchBase = string(), the "base" part of the server
%%                        transactions branch - so that we can get
%%                        sipproxy to generate intuitive branches for
%%                        it's corresponding client transactions
%%           GluePid = pid(), the pid to which sipproxy should report
%%           OrigRequest = request record()
%%           Actions = list() of sipproxy_action record()
%% Descrip.: This function is spawned by the appserver glue process
%%           and executes sipproxy:start() in this new thread.
%% Returns : void(), does not matter.
%%--------------------------------------------------------------------
start_actions(BranchBase, GluePid, OrigRequest, Actions) when record(OrigRequest, request) ->
    {Method, URI} = {OrigRequest#request.method, OrigRequest#request.uri},
    Timeout = 32,
    %% We don't return from sipproxy:start() until all Actions are done, and sipproxy signals GluePid
    %% when it is done.
    case sipproxy:start(BranchBase, GluePid, OrigRequest, Actions, Timeout) of
	ok ->
	    logger:log(debug, "Appserver forker : sipproxy fork of request ~s ~s done, start_actions() returning", [Method, sipurl:print(URI)]);
	{error, What} ->
	    logger:log(error, "Appserver forker : sipproxy fork of request ~s ~s failed : ~p", [Method, sipurl:print(URI), What])
    end.

%%--------------------------------------------------------------------
%% Function: process_messages(State)
%%           State = state record()
%% Descrip.: Main loop of the appserver glue process. Waits for
%%           signals from the server transaction (CallHandler) and the
%%           sipproxy process (ForkPid) and passes information on
%%           between these two as appropriately. Does not return until
%%           both the CallHandler and ForkPid processes are gone.
%% Returns : void(), does not matter.
%%--------------------------------------------------------------------
process_messages(State) when record(State, state), State#state.callhandler == none, State#state.forkpid == none ->
    logger:log(debug, "Appserver glue: Both CallHandler and ForkPid are 'none' - terminating"),
    ok;
process_messages(State) when record(State, state) ->
    CallHandler = State#state.callhandler,	%% The transaction layer reference to our server transaction
    ForkPid = State#state.forkpid,		%% The pid of our sipproxy process
    %% Get the actual pid of our server transaction process. We must _never_
    %% send signals directly to this pid (instead we must use the interface
    %% functions in the transactionlayer module) but we need it to verify the
    %% source of messages we receive from it.
    CallHandlerPid = transactionlayer:get_pid_from_handler(CallHandler),

    {Res, NewState} = receive
			  %%
			  %% Signals from our server transaction (CallHandlerPid)
			  %%
			  {servertransaction_cancelled, CallHandlerPid} ->
			      logger:log(debug, "Appserver glue: Original request has been cancelled, sending " ++
					 "'cancel_pending' to ForkPid ~p and entering state 'cancelled'",
					 [ForkPid]),
			      util:safe_signal("Appserver: ", ForkPid, {cancel_pending}),
			      NewState1 = State#state{cancelled=true},
			      {ok, NewState1};

			  {servertransaction_terminating, CallHandlerPid} ->
			      logger:log(debug, "Appserver glue: received servertransaction_terminating from my CallHandlerPid ~p (ForkPid is ~p) - " ++
					 "setting CallHandler to 'none'", [CallHandlerPid, ForkPid]),
			      NewState1 = State#state{callhandler=none},
			      {ok, NewState1};

			  %%
			  %% Signals from our sipproxy process (ForkPid)
			  %%
			  {sipproxy_response, ForkPid, Branch, Response} when record(Response, response) ->
			      NewState1 = case CallHandler of
					      none ->
						  {Status, Reason} = {Response#response.status, Response#response.reason},
						  logger:log(error, "Appserver glue: Received a response (~p ~s) that I can't forward since CallHandler is 'none'",
							     [Status, Reason]),
						  State;
					      _ ->
						  handle_sipproxy_response(Response, State)
					  end,
			      {ok, NewState1};

			  {sipproxy_all_terminated, ForkPid, FinalResponse} ->
			      NewState1 = case State#state.cancelled of
					      true ->
						  logger:log(debug, "Appserver glue: received sipproxy_all_terminated - request was cancelled. " ++
							     "Ask CallHandler ~p to send 487 Request Terminated and entering state 'completed'",
							     [CallHandler]),
						  transactionlayer:send_response_handler(CallHandler, 487, "Request Cancelled"),
						  State#state{completed=true};
					      _ ->
						  case FinalResponse of
						      _ when record(FinalResponse, response) ->
							  logger:log(debug, "Appserver glue: received sipproxy_all_terminated with a ~p ~s response (completed: ~p)",
								     [FinalResponse#response.status, FinalResponse#response.reason, State#state.completed]),
							  NewState2 = handle_sipproxy_response(FinalResponse, State),
							  NewState2;
						      {Status, Reason} ->
							  logger:log(debug, "Appserver glue: received sipproxy_all_terminated - asking CallHandler ~p " ++
								     "to answer ~p ~s", [CallHandler, Status, Reason]),
							  safe_callhandler_send_response(CallHandler, Status, Reason),
							  %% XXX check that this is really a final response?
							  State#state{completed=true};
						      none ->
							  logger:log(debug, "Appserver glue: received sipproxy_all_terminated with no final answer"),
							  State
						  end
					  end,
			      {ok, NewState1};

			  {sipproxy_no_more_actions, ForkPid} ->
			      NewState1 = case State#state.cancelled of
					      true ->
						  logger:log(debug, "Appserver glue: received sipproxy_no_more_actions when cancelled. " ++
							     "Ask CallHandler ~p to send 487 Request Terminated", [CallHandler]),
						  transactionlayer:send_response_handler(CallHandler, 487, "Request Terminated"),
						  State#state{completed=true};
					      _ ->
						  case State#state.completed of
						      false ->
							  Request = State#state.request,
							  {Method, URI} = {Request#request.method, Request#request.uri},
							  logger:log(debug, "Appserver glue: received sipproxy_no_more_actions when NOT cancelled " ++
								     "(and not completed), responding 408 Request Timeout to original request ~s ~s",
								     [Method, sipurl:print(URI)]),
							  safe_callhandler_send_response(CallHandler, 408, "Request Timeout"),
							  State#state{completed=true};
						      _ ->
							  logger:log(debug, "Appserver glue: received sipproxy_no_more_actions when already completed - ignoring"),
							  State
						  end
					  end,
			      {ok, NewState1};

			  {sipproxy_terminating, ForkPid} ->
			      logger:log(debug, "Appserver glue: received sipproxy_terminating from my ForkPid ~p (CallHandlerPid is ~p) " ++
					 "- setting ForkPid to 'none'", [ForkPid, CallHandlerPid]),
			      NewState1 = State#state{forkpid=none},
			      NewState2 = case util:safe_is_process_alive(CallHandlerPid) of
					      {true, _} ->
						  %% Server transaction is still alive, make it send 500 response
						  %% if no final response has been sent already
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

			  %%
			  %% Other signals
			  %%
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

safe_callhandler_send_response(none, Status, Reason) ->
    logger:log(error, "Appserver glue: CallHandler is gone, can't ask it to send '~p ~s' response", [Status, Reason]),
    error;
safe_callhandler_send_response(TH, Status, Reason) ->
    %% XXX do catch here since we are supposed to be safe?
    transactionlayer:send_response_handler(TH, Status, Reason).

%%
%% Method is INVITE and we are already completed
%%
handle_sipproxy_response(Response, State) when record(Response, response), record(State, state), State#state.sipmethod == "INVITE", State#state.completed == true ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    {Status, Reason} = {Response#response.status, Response#response.reason},
    if
	Status >= 200, Status =< 299 ->
	    %% XXX change to debug level
	    logger:log(normal, "Appserver glue: Forwarding 'late' 2xx response, ~p ~s to INVITE ~s statelessly",
		       [Status, Reason, sipurl:print(URI)]),
	    transportlayer:send_proxy_response(none, Response);
	true ->
	    logger:log(error, "Appserver glue: NOT forwarding non-2xx response ~p ~s to INVITE ~s - " ++
		       "a final response has already been forwarded (sipproxy should not do this!)",
		       [Status, Reason, sipurl:print(URI)])
    end,
    State;
%%
%% Method is non-INVITE and we are already completed
%%
handle_sipproxy_response(Response, State) when record(Response, response), record(State, state), State#state.completed == true ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(error, "Appserver glue: NOT forwarding response ~p ~s to non-INVITE request ~s ~s - " ++
	       "a final response has already been forwarded (sipproxy should not do this!)",
	       [Status, Reason, Method, sipurl:print(URI)]),
    State;
%%
%% We are not yet completed
%%
handle_sipproxy_response(Response, State) when record(Response, response), record(State, state) ->
    Request = State#state.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    {Status, Reason} = {Response#response.status, Response#response.reason},
    CallHandler = State#state.callhandler,
    logger:log(debug, "Appserver glue: Forwarding response '~p ~s' to '~s ~s' to CallHandler ~p",
	       [Status, Reason, Method, sipurl:print(URI), CallHandler]),
    case transactionlayer:is_good_transaction(CallHandler) of
	true ->
	    transactionlayer:send_proxy_response_handler(CallHandler, Response);
	false ->
	    logger:log(error, "Appserver glue: Can't forward response '~p ~s' to bad CallHandler '~p'",
		       [Status, Reason, CallHandler])
    end,
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
