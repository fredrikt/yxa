%%%-------------------------------------------------------------------
%%% File    : appserver.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: SIP application server. Handles forking and other more
%%%           advanced message routing for our users.
%%%
%%% Created : 09 Dec 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(appserver).

%%--------------------------------------------------------------------
%%% Standard Yxa SIP-application callback functions
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/3,
	 response/3
	]).

%% Internal exports
-export([start_actions/4]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipproxy.hrl").
-include("siprecords.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  request,
	  sipmethod,
	  callhandler,
	  forkpid,
	  cancelled,
	  completed
	 }).


%%====================================================================
%% Behaviour functions
%% Standard Yxa SIP-application callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init()
%% Descrip.: Yxa applications must export an init/0 function.
%% Returns : [Tables, Mode, SupData]
%%           Tables  = list() of atom(), remote mnesia tables the Yxa
%%                     startup sequence should make sure are available
%%           Mode    = stateful (or 'stateless' but DON'T use that).
%%           SupData = {append, SupSpec} |
%%                     none
%%           SupSpec = OTP supervisor child specification. Extra
%%                     processes this application want the
%%                     sipserver_sup to start and maintain.
%%--------------------------------------------------------------------
init() ->
    [[user, numbers, phone], stateful, none].



%%--------------------------------------------------------------------
%% Function: request(Request, Origin, LogStr)
%%           Request = request record()
%%           Origin  = siporigin record()
%%           LogStr  = string()
%% Descrip.: Yxa applications must export an request/3 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------

%%
%% REGISTER
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "REGISTER" ->
    logger:log(normal, "Appserver: ~s Method not applicable here -> 403 Forbidden", [LogStr]),
    transactionlayer:send_response_request(Request, 403, "Forbidden"),
    ok;

%%
%% ACK
%%
request(Request, Origin, LogStr) when record(Request, request), record(Origin, siporigin), Request#request.method == "ACK" ->
    case local:get_user_with_contact(Request#request.uri) of
	none ->
	    logger:log(debug, "Appserver: ~s -> no transaction state, unknown user, ignoring",
		       [LogStr]);
	SIPuser ->
	    logger:log(normal, "Appserver: ~s -> Forwarding statelessly (SIP user ~p)",
		       [LogStr, SIPuser]),
	    transportlayer:send_proxy_request(none, Request, Request#request.uri, [])
    end, 
    ok;

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
    end,
    ok;

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
    end,
    ok.


%%--------------------------------------------------------------------
%% Function: response(Response, Origin, LogStr)
%%           Response = response record()
%%           Origin   = siporigin record()
%%           LogStr   = string()
%% Descrip.: Yxa applications must export an response/3 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------
response(Response, Origin, LogStr) when record(Response, response), record(Origin, siporigin) ->

    %% RFC 3261 16.7 says we MUST act like a stateless proxy when no
    %% transaction can be found
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(normal, "incomingproxy: Response to ~s: '~p ~s', no matching transaction - proxying statelessly",
	       [LogStr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response),
    ok.


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
    case keylist:fetch('route', Request#request.header) of
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
		    %%fork_actions(BranchBase, CallHandler, Request, Actions)
		    %% XXX do we need to trap the EXIT from this process inside appserver_glue?
		    case appserver_glue:start(Request, Actions) of
			{ok, P} when is_pid(P) ->
			    true;
			_ ->
			    throw({siperror, 500, "Server Internal Error"})
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
    LookupURL = sipurl:set([{pass, none}, {port, none}, {param, []}], URI),
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
locations_to_actions2([{Location, _Flags, _Class, _Expire} | T], Res) when record(Location, sipurl) ->
    Timeout = sipserver:get_env(appserver_call_timeout, 40),
    CallAction = #sipproxy_action{action=call, requri=Location, timeout=Timeout},
    locations_to_actions2(T, lists:append([CallAction], Res));
locations_to_actions2([H | T], Res) ->
    logger:log(error, "appserver: Illegal location in locations_to_actions2: ~p", [H]),
    locations_to_actions2(T, Res).

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
	    logger:log(debug, "Appserver forker: sipproxy fork of request ~s ~s done, start_actions() returning", [Method, sipurl:print(URI)]);
	{error, What} ->
	    logger:log(error, "Appserver forker: sipproxy fork of request ~s ~s failed : ~p", [Method, sipurl:print(URI), What])
    end.
