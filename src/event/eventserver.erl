%%%-------------------------------------------------------------------
%%% File    : eventserver.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Event package server framework (RFC3265).
%%%
%%% @since    26 Apr 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(eventserver).

-behaviour(yxa_app).

%%--------------------------------------------------------------------
%%% Standard YXA SIP-application callback functions
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/2,
	 response/2,
	 terminate/1
	]).

-export([test/0]).

%%--------------------------------------------------------------------
%%% Application specific exports
%%--------------------------------------------------------------------
-export([authenticate_subscriber/2
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
%% @spec    () -> #yxa_app_init{}
%%
%% @doc     YXA applications must export an init/0 function.
%% @hidden
%% @end
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

    #yxa_app_init{sup_spec	= {append, Specs},
		  mnesia_tables	= Tables
		 }.

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%% @doc     YXA applications must export a request/2 function.
%% @end
%%--------------------------------------------------------------------
request(Request, YxaCtx) ->
    THandler = YxaCtx#yxa_ctx.thandler,
    LogTag = transactionlayer:get_branchbase_from_handler(THandler),
    YxaCtx1 =
	YxaCtx#yxa_ctx{app_logtag = LogTag
		      },

    %% check if we are shutting down
    case is_shutting_down(YxaCtx) of
	true ->
	    ok;
	false ->
	    %% Check if request has To-tag, if so we reject it since we obviously lost the dialog state
	    case sipheader:get_tag( keylist:fetch('to', Request#request.header) ) of
		none ->
		    request2(Request, YxaCtx1);
		_ ->
		    logger:log(normal, "~s: event server: Request has To-tag, answering '481 Call/Transaction Does Not Exist'",
			       [LogTag]),
		    ExtraHeaders = make_extraheaders(481, []),
		    transactionlayer:send_response_handler(THandler, 481, "Call/Transaction Does Not Exist", ExtraHeaders),
		    ok
	    end
    end.

is_shutting_down(YxaCtx) ->
    case ets:lookup(?EVENTSERVER_T, terminating) of
	[{terminating, Start}] when is_integer(Start) ->
	    #yxa_ctx{app_logtag   = LogTag,
		     thandler = THandler
		    } = YxaCtx,

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
request2(#request{method = Method} = Request, YxaCtx) when Method == "SUBSCRIBE"; Method == "PUBLISH" ->
    case sipheader:event_package(Request#request.header) of
	[] ->
	    %% No Event: header in request
	    #yxa_ctx{logstr       = LogStr,
		     app_logtag   = LogTag,
		     thandler = THandler
		    } = YxaCtx,

	    logger:log(normal, "~s: event server: ~s -> '489 Bad Event'", [LogTag, LogStr]),
	    ExtraHeaders = make_extraheaders(489, []),
	    transactionlayer:send_response_handler(THandler, 489, "Bad Event", ExtraHeaders);
	EventPackage when is_list(EventPackage) ->
	    event(EventPackage, Request, YxaCtx)
    end;

%%
%% OPTIONS
%%
request2(#request{method = "OPTIONS"} = Request, YxaCtx) ->
    case local:is_request_to_this_proxy(Request) of
	true ->
	    AllowMethods = [{"Allow", ?ALLOW_METHODS}],
	    ExtraHeaders = make_extraheaders(200, AllowMethods),
	    siprequest:request_to_me(Request, YxaCtx, ExtraHeaders);
	false ->
	    #yxa_ctx{app_logtag	= LogTag,
		     thandler	= THandler
		    } = YxaCtx,
	    logger:log(normal, "~s: event server: OPTIONS ~s (not to me) -> '403 Forbidden'",
		       [LogTag, sipurl:print(Request#request.uri)]),
	    ExtraHeaders = make_extraheaders(403, []),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden", ExtraHeaders)
    end,
    ok;

%%
%% Request other than SUBSCRIBE/PUBLISH/OPTIONS, should not end up on the event server
%%
request2(Request, YxaCtx) when is_record(Request, request) ->
    #yxa_ctx{logstr       = LogStr,
	     app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,

    logger:log(normal, "~s: event server: ~s -> '403 Forbidden'", [LogTag, LogStr]),
    ExtraHeaders = make_extraheaders(403, []),
    transactionlayer:send_response_handler(THandler, 403, "Forbidden", ExtraHeaders),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Response, YxaCtx) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Request = #response{}
%%            YxaCtx  = #yxa_ctx{}
%%
%% @doc     YXA applications must export a response/2 function.
%% @end
%%--------------------------------------------------------------------
response(Response, YxaCtx) when is_record(Response, response) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(normal, "event server: Response to ~s: '~p ~s', no matching transaction - ignoring",
	       [YxaCtx#yxa_ctx.logstr, Status, Reason]),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Mode) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Mode = shutdown | graceful | atom()
%%
%% @doc     YXA applications must export a terminate/1 function.
%% @hidden
%% @end
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
%% @spec    (EventPackage, Request, YxaCtx) -> void()
%%
%%            EventPackage = string()
%%            Request      = #request{}
%%            YxaCtx       = #yxa_ctx{}
%%
%% @doc     Eventserver has received a new request from the transact-
%%          ion layer (on a new dialog). It is either a SUBSCRIBE or
%%          a PUBLISH for which we could figure out an event package.
%% @end
%%--------------------------------------------------------------------
event(EventPackage, Request, YxaCtx) ->
    #yxa_ctx{app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,

    case get_event_package_module(EventPackage, Request, YxaCtx) of
	{ok, Module} when is_atom(Module) ->
	    Res = event2(Request, YxaCtx, Module, EventPackage),
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
%% @spec    (Request, YxaCtx, Module, EventPackage) -> void()
%%
%%            Request      = #request{}
%%            YxaCtx       = #yxa_ctx{}
%%            Module       = atom()
%%            EventPackage = string()
%%
%% @doc     If the request we received was a SUBSCRIBE, we start an
%%          subscription domain controller process to handle the new
%%          dialog. If it was something else (read: PUBLISH) we pass
%%          it to the package modules request function.
%% @end
%%--------------------------------------------------------------------
event2(#request{method = "SUBSCRIBE"} = Request, YxaCtx, Module, EventPackage) ->
    case subscription:start(Request, YxaCtx, Module, EventPackage, undefined) of
	{error, need_auth} ->
	    case authenticate_subscriber(Request, YxaCtx) of
		{true, SIPuser} ->
		    subscription:start(Request, YxaCtx, Module, EventPackage, SIPuser);
		false ->
		    #yxa_ctx{logstr       = LogStr,
			     app_logtag   = LogTag,
			     thandler = THandler
			    } = YxaCtx,
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

event2(Request, YxaCtx, Module, EventPackage) ->
    Ctx = #event_ctx{sipuser   = undefined,
		     dialog_id = sipheader:dialogid(Request#request.header)
		    },
    case Module:request(EventPackage, Request, YxaCtx, Ctx) of
	{error, need_auth} ->
	    case authenticate_subscriber(Request, YxaCtx) of
		{true, SIPuser} ->
		    Ctx1 = Ctx#event_ctx{sipuser = SIPuser},
		    PackageM_Res = Module:request(EventPackage, Request, YxaCtx, Ctx1),
		    logger:log(debug, "event server: Extra debug: Result of ~p:request/4 was : ~p",
			       [Module, PackageM_Res]);

		false ->
		    #yxa_ctx{logstr       = LogStr,
			     app_logtag   = LogTag,
			     thandler = THandler
			    } = YxaCtx,
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
%% @spec    (Request, YxaCtx) ->
%%            {true, AuthUser} | false | drop
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%%            AuthUser = string()
%%
%% @doc     The event package has requested that we authenticate the
%%          user who sent us this request. Do WWW _OR_ Proxy-
%%          authentication since there has been cases where a client
%%          first got a 407 from 'incomingproxy', sent a new request
%%          with proxy credentials, got a 401 from the eventserver
%%          and sent a new request with www credentials but without
%%          the proxy credentials, which of course caused the
%%          incomingproxy to one again require proxy authentication.
%% @end
%%--------------------------------------------------------------------
authenticate_subscriber(Request, YxaCtx) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    #yxa_ctx{app_logtag	= LogTag,
	     thandler	= THandler
	    } = YxaCtx,
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
			transactionlayer:send_challenge(THandler, proxy, true, none),
			drop;
		    false ->
			case keylist:fetch('www-authenticate', Header) of
			    [] ->
				logger:log(normal, "~s: event server: From: address requires authentication", [LogTag]),
				transactionlayer:send_challenge(THandler, www, false, none),
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
%% @spec    (EventPackage, Request, YxaCtx) ->
%%            BranchBase
%%
%%            EventPackage = string()
%%            Request      = #request{}
%%            YxaCtx       = #yxa_ctx{}
%%
%%            BranchBase = string()
%%
%% @doc     Get branch from server transaction handler and then remove
%%          the -UAS suffix. The result is used as a tag when logging
%%          actions.
%% @end
%%--------------------------------------------------------------------
get_event_package_module(EventPackage, Request, YxaCtx) ->
    case local:get_event_package_module(EventPackage, Request, YxaCtx) of
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
%% @spec    () ->
%%            Packages
%%
%%            Packages = [string()]
%%
%% @doc     Get a list of all event packages for which we have a
%%          handler. For use in OPTIONS replys.
%% @end
%%--------------------------------------------------------------------
get_allowed_events() ->
    {ok, L} = local:get_all_event_packages(),
    PL = [Package || {Package, _Module} <- L],
    %% duplicate Package is explicitly allowed
    lists:usort(PL).

%%--------------------------------------------------------------------
%% @spec    () ->
%%            Packages
%%
%%            Packages = [string()]
%%
%% @doc     Get a list of all event package modules, to call each of
%%          their's init/0 function from our init/0 function.
%% @end
%%--------------------------------------------------------------------
get_event_modules() ->
    {ok, L} = local:get_all_event_packages(),
    [Module || {_Package, Module} <- L].

%%--------------------------------------------------------------------
%% @spec    (Status, ExtraHeaders_In) ->
%%            ExtraHeaders
%%
%%            Status          = integer() "SIP status code of response we are creating"
%%            ExtraHeaders_In = [{Key, ValueL}]
%%
%%            ExtraHeaders = [{Key, ValueL}]
%%
%% @doc     Create ExtraHeaders to use when sending responses. Include
%%          Server: and Allow-Event: headers as appropriate.
%% @end
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
    ok = eventserver_test:test(),
    ok.
