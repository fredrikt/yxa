%%%-------------------------------------------------------------------
%%% File    : eventserver.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Event package server framework (RFC3265).
%%%
%%% Created : 26 Apr 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(eventserver).

-behaviour(yxa_app).

%%--------------------------------------------------------------------
%%% Standard YXA SIP-application callback functions
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/3,
	 response/3,
	 terminate/1
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("event.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(ALLOW_METHODS, ["SUBSCRIBE", "PUBLISH", "OPTIONS"]).
-define(EVENTSERVER_T, eventserver_t).
-define(SHUTDOWN_TIMEOUT, 10).


%%====================================================================
%% Behaviour functions
%% Standard YXA SIP-application callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init()
%% Descrip.: YXA applications must export an init/0 function.
%% Returns : [Tables, Mode, SupData]
%%           Tables  = list() of atom(), remote mnesia tables the YXA
%%                     startup sequence should make sure are available
%%           Mode    = stateful
%%           SupData = {append, SupSpec} |
%%                     none
%%           SupSpec = OTP supervisor child specification. Extra
%%                     processes this application want the
%%                     sipserver_sup to start and maintain.
%%--------------------------------------------------------------------
init() ->
    %% This is a table listing processes interested in hearing about changes to different
    %% user's state
    notifylist:init(),
    MySpecs = [{event_mnesia_monitor, {event_mnesia_monitor, start_link, []},
		permanent, 2000, worker, [event_mnesia_monitor]}],

    %% Let all the registered package modules initialize and optionally add to
    %% the list of things we request the sipserver_sup supervisor to start and monitor
    Specs =
	lists:foldl(fun(M, Acc) ->
			    case M:init() of
				{append, This} ->
				    Acc ++ This;
				none ->
				    Acc
			    end
		    end, MySpecs, get_event_modules()),

    %% create ets table where we store application specific information
    ets:new(?EVENTSERVER_T, [public, set, named_table]),

    Tables = [user, numbers],
    [Tables, stateful, {append, Specs}].

%%--------------------------------------------------------------------
%% Function: request(Request, _Origin, LogStr)
%%           Request = request record()
%%           _Origin  = siporigin record()
%%           LogStr  = string() describing request
%% Descrip.: YXA applications must export an request/3 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------
request(Request, Origin, LogStr) ->
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branchbase_from_handler(THandler),

    %% check if we are shutting down
    case is_shutting_down(THandler, LogTag) of
	true ->
	    ok;
	false ->
	    %% Check if request has To-tag, if so we reject it since we obviously lost the dialog state
	    case sipheader:get_tag( keylist:fetch('to', Request#request.header) ) of
		none ->
		    request2(Request, Origin, LogStr, THandler, LogTag);
		_ ->
		    logger:log(normal, "~s: event server: Request has To-tag, answering '481 Call/Transaction Does Not Exist'",
			       [LogTag]),
		    ExtraHeaders = make_extraheaders(481, []),
		    transactionlayer:send_response_handler(THandler, 481, "Call/Transaction Does Not Exist", ExtraHeaders),
		    ok
	    end
    end.

is_shutting_down(THandler, LogTag) ->
    case ets:lookup(?EVENTSERVER_T, terminating) of
	[{terminating, Start}] when is_integer(Start) ->
	    logger:log(normal, "~s: event server: Performing shutdown, answering "
		       "'503 Service Unavailable (shutting down)'", [LogTag]),
	    Now = util:timestamp(),
	    RetryAfter = 1 + ?SHUTDOWN_TIMEOUT - (Now - Start),
	    transactionlayer:send_response_handler(THandler, 503, "Service Unavailable (shutting down)",
						   [{"Retry-After", [integer_to_list(RetryAfter)]}]),
	    true;
	[] ->
	    false
    end.

%%
%% PUBLISH or SUBSCRIBE
%%
request2(#request{method = Method} = Request, Origin, LogStr, THandler, LogTag) when Method == "SUBSCRIBE";
										     Method == "PUBLISH" ->
    case sipheader:event_package(Request#request.header) of
	[] ->
	    %% No Event: header in request
	    logger:log(normal, "~s: event server: ~s -> '489 Bad Event'", [LogTag, LogStr]),
	    ExtraHeaders = make_extraheaders(489, []),
	    transactionlayer:send_response_handler(THandler, 489, "Bad Event", ExtraHeaders);
	EventPackage when is_list(EventPackage) ->
	    event(EventPackage, Request, Origin, LogStr, LogTag, THandler)
    end;

%%
%% OPTIONS
%%
request2(#request{method = "OPTIONS"} = Request, _Origin, _LogStr, THandler, LogTag) ->
    case local:is_request_to_this_proxy(Request) of
	true ->
	    AllowMethods = [{"Allow", ?ALLOW_METHODS}],
	    ExtraHeaders = make_extraheaders(200, AllowMethods),
	    logger:log(normal, "~s: event server: OPTIONS ~s (to me) -> '200 OK'",
		       [LogTag, sipurl:print(Request#request.uri)]),
	    transactionlayer:send_response_handler(THandler, 200, "OK", ExtraHeaders);
	false ->
	    logger:log(normal, "~s: event server: OPTIONS ~s (not to me) -> '403 Forbidden'",
		       [LogTag, sipurl:print(Request#request.uri)]),
	    ExtraHeaders = make_extraheaders(403, []),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden", ExtraHeaders)
    end,
    ok;

%%
%% Request other than SUBSCRIBE/PUBLISH/OPTIONS, should not end up on the event server
%%
request2(Request, _Origin, LogStr, THandler, LogTag) when is_record(Request, request) ->
    logger:log(normal, "~s: event server: ~s -> '403 Forbidden'", [LogTag, LogStr]),
    ExtraHeaders = make_extraheaders(403, []),
    transactionlayer:send_response_handler(THandler, 403, "Forbidden", ExtraHeaders),
    ok.

%%--------------------------------------------------------------------
%% Function: response(Response, _Origin, LogStr)
%%           Request = response record()
%%           _Origin  = siporigin record()
%%           LogStr  = string(), description of response
%% Descrip.: YXA applications must export an response/3 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------
response(Response, _Origin, LogStr) when is_record(Response, response) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(normal, "event server: Response to ~s: '~p ~s', no matching transaction - ignoring",
	       [LogStr, Status, Reason]),
    ok.

%%--------------------------------------------------------------------
%% Function: terminate(Mode)
%%           Mode = shutdown | graceful | ...
%% Descrip.: YXA applications must export a terminate/1 function.
%% Returns : Yet to be specified. Return 'ok' for now.
%%--------------------------------------------------------------------
terminate(Mode) when is_atom(Mode) ->
    Now = util:timestamp(),
    true = ets:insert(?EVENTSERVER_T, {terminating, Now}),
    case notifylist:get_all_pids() of
	[] ->
	    ok;
	NotifyL ->
	    logger:log(normal, "event server: Telling ~p ongoing subscriptions to stop", [length(NotifyL)]),
	    _ = [gen_server:cast(Pid, {terminate, Mode}) || Pid <- NotifyL],
	    terminate_wait(?SHUTDOWN_TIMEOUT)
    end,
    ok.

terminate_wait(0) ->
    ok;
terminate_wait(N) ->
    case notifylist:get_all_pids() of
        [] ->
	    ok;
	_ ->
	    timer:sleep(1000),
	    terminate_wait(N - 1)
    end.


%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: event(EventPackage, Request, Origin, LogStr, LogTag,
%%                 THandler)
%%           EventPackage = string()
%%           Request      = request record()
%%           Origin       = siporigin record()
%%           LogStr       = string() describing request
%%           LogTag       = string(), prefix for logging
%%           Thandler     = term(), server transaction handle
%% Descrip.: Eventserver has received a new request from the transact-
%%           ion layer (on a new dialog). It is either a SUBSCRIBE or
%%           a PUBLISH for which we could figure out an event package.
%% Returns : void()
%%--------------------------------------------------------------------
event(EventPackage, Request, Origin, LogStr, LogTag, THandler) ->
    case get_event_package_module(EventPackage, Request, Origin) of
	{ok, Module} ->
	    Res = event2(Request, Origin, LogStr, LogTag, THandler, Module, EventPackage),
	    logger:log(debug, "~s: event server: Terminating after processing a ~p request for package ~p "
		       "(module ~p), result : ~p", [LogTag, Request#request.method, EventPackage, Module, Res]),
	    Res;
	none ->
	    logger:log(normal, "~s: event server: Event package ~p not implemented, "
		       "answering '489 Bad Event'", [LogTag, EventPackage]),
	    ExtraHeaders = make_extraheaders(489, []),
	    transactionlayer:send_response_handler(THandler, 489, "Bad Event", ExtraHeaders)
    end.

%%--------------------------------------------------------------------
%% Function: event2(EventPackage, Request, Origin, LogStr, LogTag,
%%                 THandler)
%%           EventPackage = string()
%%           Request      = request record()
%%           Origin       = siporigin record()
%%           LogStr       = string() describing request
%%           LogTag       = string(), prefix for logging
%%           Thandler     = term(), server transaction handle
%% Descrip.: If the request we received was a SUBSCRIBE, we start an
%%           subscription domain controller process to handle the new
%%           dialog. If it was something else (read: PUBLISH) we pass
%%           it to the package modules request function.
%% Returns : void()
%%--------------------------------------------------------------------
event2(#request{method = "SUBSCRIBE"} = Request, Origin, LogStr, LogTag, THandler, Module, EventPackage) ->
    case subscription:start(Request, Origin, LogStr, LogTag, THandler, Module, EventPackage, undefined) of
	{error, need_auth} ->
	    case authenticate_subscriber(Request, LogTag, LogStr) of
		{true, SIPuser} ->
		    subscription:start(Request, Origin, LogStr, LogTag, THandler, Module, EventPackage, SIPuser);
		false ->
		    logger:log(normal, "~s: event server: ~s -> '403 Forbidden'",
			       [LogTag, LogStr]),
		    ExtraHeaders = make_extraheaders(403, []),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden", ExtraHeaders);
		drop ->
		    ok
	    end;
	Res ->
	    Res
    end;

event2(Request, Origin, LogStr, LogTag, THandler, Module, EventPackage) ->
    Ctx = #event_ctx{sipuser = undefined},
    case Module:request(EventPackage, Request, Origin, LogStr, LogTag, THandler, Ctx) of
	{error, need_auth} ->
	    case authenticate_subscriber(Request, LogTag, LogStr) of
		{true, SIPuser} ->
		    Ctx1 = Ctx#event_ctx{sipuser = SIPuser},
		    Module:request(EventPackage, Request, Origin, LogStr, LogTag, THandler, Ctx1);
		false ->
		    logger:log(normal, "~s: event server: ~s -> '403 Forbidden'",
			       [LogTag, LogStr]),
		    ExtraHeaders = make_extraheaders(403, []),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden", ExtraHeaders);
		drop ->
		    ok
	    end;
	Res ->
	    Res
    end.


%%--------------------------------------------------------------------
%% Function: get_branchbase_from_handler(TH)
%%           TH = term(), server transaction handle
%% Descrip.: Get branch from server transaction handler and then
%%           remove the -UAS suffix. The result is used as a tag
%%           when logging actions.
%% Returns : BranchBase = string()
%%--------------------------------------------------------------------
get_branchbase_from_handler(TH) ->
    CallBranch = transactionlayer:get_branch_from_handler(TH),
    case string:rstr(CallBranch, "-UAS") of
	0 ->
	    CallBranch;
	Index when is_integer(Index) ->
	    BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
    end.

%%--------------------------------------------------------------------
%% Function: authenticate_subscriber(Request, LogTag)
%%           Request = request record()
%%           LogTag  = string(), prefix for logging
%% Descrip.: The event package has requested that we authenticate the
%%           user who sent us this request. Do WWW _OR_ Proxy-
%%           authentication since there has been cases where a client
%%           first got a 407 from 'incomingproxy', sent a new request
%%           with proxy credentials, got a 401 from the eventserver
%%           and sent a new request with www credentials but without
%%           the proxy credentials, which of course caused the
%%           incomingproxy to one again require proxy authentication.
%% Returns : {true, AuthUser} | false | drop
%%           AuthUser = string()
%%--------------------------------------------------------------------
authenticate_subscriber(Request, LogTag, LogStr) when is_record(Request, request), is_list(LogTag), is_list(LogStr) ->
    #request{method = Method,
	     header = Header
	    } = Request,
    {_, FromURI} = sipheader:from(Header),

    %% Verify sending user.
    AuthRes =
	case local:get_user_verified(Header, Method) of
	    {authenticated, WWWSIPuser} ->
		{ok, WWWSIPuser};
	    {stale, WWWSIPuser} ->
		logger:log(normal, "~s: event server: From: address requires authentication (stale, user ~p)",
			   [LogTag, WWWSIPuser]),
		transactionlayer:send_challenge_request(Request, www, true, none),
		drop;
	    false ->
		case local:get_user_verified_proxy(Header, Method) of
		    {authenticated, ProxySIPuser} ->
			{ok, ProxySIPuser};
		    {stale, ProxySIPuser} ->
			logger:log(normal, "~s: event server: From: address requires authentication "
				   "(stale, user ~p)", [LogTag, ProxySIPuser]),
			transactionlayer:send_challenge_request(Request, proxy, true, none),
			drop;
		    false ->
			case keylist:fetch('www-authenticate', Header) of
			    [] ->
				logger:log(normal, "~s: event server: From: address requires authentication", [LogTag]),
				transactionlayer:send_challenge_request(Request, www, false, none),
				drop;
			    _ ->
				false
			end
		end
	end,

    case AuthRes of
	{ok, SIPUser} ->
	    case local:can_use_address(SIPUser, FromURI) of
		true ->
		    logger:log(debug, "~s: event server: User ~p is allowed to use From: address ~p",
			       [LogTag, SIPUser, sipurl:print(FromURI)]),
		    {true, SIPUser};
		false ->
		    logger:log(error, "~s: event server: Authenticated user ~p may NOT use address ~p",
			       [LogTag, SIPUser, sipurl:print(FromURI)]),
		    false
	    end;
	_ ->
	    AuthRes
    end.


%%--------------------------------------------------------------------
%% Function: get_branchbase_from_handler(TH)
%%           TH = term(), server transaction handle
%% Descrip.: Get branch from server transaction handler and then
%%           remove the -UAS suffix. The result is used as a tag
%%           when logging actions.
%% Returns : BranchBase = string()
%%--------------------------------------------------------------------
get_event_package_module(EventPackage, Request, Origin) ->
    case local:get_event_package_module(EventPackage, Request, Origin) of
	undefined ->
	    {ok, L} = yxa_config:get_env(eventserver_package_handlers),
	    case lists:keysearch(EventPackage, 1, L) of
		{value, {EventPackage, Module}} when is_atom(Module) ->
		    {ok, Module};
		false ->
		    none
	    end;
	Res ->
	    Res
    end.

%%--------------------------------------------------------------------
%% Function: get_allowed_events()
%% Descrip.: Get a list of all event packages for which we have a
%%           handler. For use in OPTIONS replys.
%% Returns : Packages = list() of string()
%%--------------------------------------------------------------------
get_allowed_events() ->
    {ok, L} = local:get_all_event_packages(),
    PL = [Package || {Package, _Module} <- L],
    %% duplicate Package is explicitly allowed
    lists:usort(PL).

%%--------------------------------------------------------------------
%% Function: get_event_package_modules()
%% Descrip.: Get a list of all event package modules, to call each of
%%           their's init/0 function from our init/0 function.
%% Returns : Packages = list() of string()
%%--------------------------------------------------------------------
get_event_modules() ->
    {ok, L} = local:get_all_event_packages(),
    [Module || {_Package, Module} <- L].

%%--------------------------------------------------------------------
%% Function: make_extraheaders(Status, ExtraHeaders_In)
%%           Status          = integer(), SIP status code of response
%%                                        we are creating
%%           ExtraHeaders_In = list() of tuple() ({Key, ValueL})
%% Descrip.: Create ExtraHeaders to use when sending responses.
%%           Include Server: and Allow-Event: headers as appropriate.
%% Returns : ExtraHeaders = list() of tuple() ({Key, ValueL})
%%--------------------------------------------------------------------
make_extraheaders(Status, ExtraHeaders_In) ->
    Server =
	case yxa_config:get_env(set_useragent_and_server) of
	    {ok, true} ->
		Value =
		    lists:concat(["YXA/", version:get_version(),
				  " at ", siprequest:myhostname()]),
		[{"Server", [Value]}];
	    {ok, false} ->
		[]
	end,
    AllowEvents =
	case {Status, get_allowed_events()} of
	    {489, Events} when is_list(Events), length(Events) > 0 ->
		[{"Allow-Events", Events}];
	    {_Status, _Events} ->
		[]
	end,
    ExtraHeaders_In ++ AllowEvents ++ Server.
