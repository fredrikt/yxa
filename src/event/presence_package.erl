%%%-------------------------------------------------------------------
%%% File    : presence_package.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      A very basic RFC3856 (SIP Presence) implementation.
%%%
%%%           PUBLISH is described in RFC3903 (SIP Event State
%%%           Publication).
%%%
%%% @since    27 Apr 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(presence_package).

-behaviour(event_package).

%%--------------------------------------------------------------------
%%% Standard YXA Event package exports
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/4,
	 is_allowed_subscribe/7,
	 notify_content/4,
	 package_parameters/2,
	 subscription_behaviour/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("event.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(my_state, {}).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% @type event_pkg() = string().
%%           Event package string - "dialog" for this package.

%%====================================================================
%% Behaviour functions
%% Standard YXA Event package callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () ->
%%            none | {append, SupSpec}
%%
%%            SupSpec = term() "OTP supervisor child specification. Extra processes this event package want the sipserver_sup to start and maintain."
%%
%% @doc     YXA event packages must export an init/0 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    ok = presence_pidf:init(),
    none.

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), Request, YxaCtx, Ctx) ->
%%            void() "but return 'ok' or {error, Reason} for now"
%%
%%            Request  = #request{} "the SUBSCRIBE request"
%%            YxaCtx   = #yxa_ctx{}
%%            LogTag   = string() "log prefix"
%%            THandler = term() "server transaction handler"
%%            Ctx      = #event_ctx{} "context information for request."
%%
%% @doc     YXA event packages must export a request/6 function. See
%%          the eventserver.erl module description for more
%%          information about when this function is invoked.
%% @end
%%--------------------------------------------------------------------
request("presence", _Request, YxaCtx, #event_ctx{sipuser = undefined}) ->
    logger:log(debug, "~s: presence event package: Requesting authorization (only local users allowed)",
	       [YxaCtx#yxa_ctx.app_logtag]),
    {error, need_auth};

request("presence", #request{method = "PUBLISH"}, YxaCtx, #event_ctx{sipuser = []}) ->
    %% empty SIP user
    #yxa_ctx{logstr       = LogStr,
	     app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,
    logger:log(normal, "~s: presence event package: ~s -> '404 Not Found'", [LogTag, LogStr]),
    transactionlayer:send_response_handler(THandler, 404, "Not Found"),
    ok;

request("presence", #request{method = "PUBLISH"} = Request, YxaCtx, Ctx) ->
    %% non-empty SIP user
    #yxa_ctx{app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,

    #event_ctx{sipuser = SIPuser
	      } = Ctx,

    logger:log(normal, "~s: presence event package: Processing PUBLISH ~s (presentity: {user, ~p})",
	       [LogTag, sipurl:print(Request#request.uri), SIPuser]),

    Res =
	case get_publish_etag_expires(Request, SIPuser, THandler) of
	    error ->
		{error, "ETag/Expires problem"};

	    {ok, Action, OldETag, Expires} when (Action == insert orelse Action == update),
						is_integer(Expires) ->
		%% No ETag in request (SIP-If-Match header)
		case keylist:fetch('content-type', Request#request.header) of
		    [ContentType] ->
			XML = binary_to_list(Request#request.body),
			ETag = generate_etag(),
			Res2 =
			    case Action of
				insert ->
				    presence_pidf:set_pidf_for_user(SIPuser, ETag, Expires, ContentType, XML, Ctx);
				update ->
				    presence_pidf:update_pidf_for_user(SIPuser, OldETag, ETag, Expires, ContentType,
								       XML, Ctx)
			    end,

			case Res2 of
			    ok ->
				EH = [{"SIP-ETag", [ETag]},
				      {"Expires", [integer_to_list(Expires)]}
				     ],
				{ok, EH};
			    nomatch when Action == update ->
				%% Entry with this ETag must have expired between get_publish_etag_expires/3 and now
				transactionlayer:send_response_handler(THandler, 412, "Conditional Request Failed");
			    {error, unsupported_content_type} ->
				AcceptL = presence_pidf:get_supported_content_types(set),
				ExtraHeaders1 = [{"Accept", AcceptL}],
				transactionlayer:send_response_handler(THandler, 406, "Not Acceptable", ExtraHeaders1);
			    {error, unknown_content_type} ->
				AcceptL = presence_pidf:get_supported_content_types(set),
				ExtraHeaders1 = [{"Accept", AcceptL}],
				transactionlayer:send_response_handler(THandler, 406, "Not Acceptable", ExtraHeaders1);
			    {error, bad_xml} ->
				logger:log(error, "~s: presence event package: Failed storing presence for user ~p "
					   "(bad XML)", [LogTag, SIPuser]),
				AcceptL = presence_pidf:get_supported_content_types(set),
				ExtraHeaders1 = [{"Accept", AcceptL}],
				transactionlayer:send_response_handler(THandler, 400, "Could not parse XML body",
								       ExtraHeaders1),
				{error, "Could not parse XML body"}
			end;
		    _ ->
			logger:log(error, "~s: presence event package: Failed storing presence for user ~p "
				   "(bad or missing Content-Type)", [LogTag, SIPuser]),
			transactionlayer:send_response_handler(THandler, 400, "Bad or missing Content-Type"),
			{error, "Bad or missing Content-Type"}
		end;
	    {ok, refresh, ETag, Expires} when is_list(ETag), is_integer(Expires) ->
		%% This is a request to refresh the validity period of an existing publication
		NewETag = generate_etag(),
		case presence_pidf:refresh_pidf_user_etag(SIPuser, ETag, Expires, NewETag) of
		    ok ->
			EH = [{"SIP-ETag", [NewETag]},
			      {"Expires", [integer_to_list(Expires)]}
			     ],
			{ok, EH};
		    nomatch ->
			logger:log(debug, "~s: presence event package: No entry with ETag ~p found in the event "
				   "database, answering '412 Conditional Request Failed'", [ETag]),
			transactionlayer:send_response_handler(THandler, 412, "Conditional Request Failed"),
			{error, "Request has invalid SIP-If-Match"}
		end
	end,

    case Res of
	{ok, ExtraHeaders} when is_list(ExtraHeaders) ->
	    transactionlayer:send_response_handler(THandler, 200, "Ok", ExtraHeaders),
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end;

request("presence", #request{method = "NOTIFY"} = Request, YxaCtx, Ctx) ->
    %% non-empty SIP user
    #yxa_ctx{app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,

    #event_ctx{sipuser = Presentity
	      } = Ctx,

    logger:log(normal, "~s: presence event package: Processing NOTIFY ~s (presentity: {user, ~p})",
	       [LogTag, sipurl:print(Request#request.uri), Presentity]),

    [ContentType] = keylist:fetch('content-type', Request#request.header),
    XML = binary_to_list(Request#request.body),
    ETag = erlang:now(),
    Expires = publish_get_expires(Request#request.header, THandler),
    case presence_pidf:set_pidf_for_user(Presentity, ETag, Expires, ContentType, XML, Ctx) of
	ok ->
	    EH = [{"Expires", [integer_to_list(Expires)]}
		 ],
	    transactionlayer:send_response_handler(THandler, 200, "Ok", EH);
	{error, unsupported_content_type} ->
	    AcceptL = presence_pidf:get_supported_content_types(set),
	    ExtraHeaders1 = [{"Accept", AcceptL}],
	    transactionlayer:send_response_handler(THandler, 406, "Not Acceptable", ExtraHeaders1);
	{error, unknown_content_type} ->
	    AcceptL = presence_pidf:get_supported_content_types(set),
	    ExtraHeaders1 = [{"Accept", AcceptL}],
	    transactionlayer:send_response_handler(THandler, 406, "Not Acceptable", ExtraHeaders1);
	{error, bad_xml} ->
	    logger:log(error, "~s: presence event package: Failed storing presence for presentity ~p (bad XML)",
		       [LogTag, Presentity]),
	    AcceptL = presence_pidf:get_supported_content_types(set),
	    ExtraHeaders1 = [{"Accept", AcceptL}],
	    transactionlayer:send_response_handler(THandler, 400, "Could not parse XML body", ExtraHeaders1),
	    {error, "Could not parse XML body"}
    end;


request("presence", _Request, YxaCtx, _Ctx) ->
    #yxa_ctx{logstr       = LogStr,
	     app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,

    logger:log(normal, "~s: presence event package: ~s -> '501 Not Implemented'",
	       [LogTag, LogStr]),
    transactionlayer:send_response_handler(THandler, 501, "Not Implemented"),
    {error, "SIP method not implemented"}.


%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), Num, Request, YxaCtx, SIPuser,
%%          Presentity, PkgState) ->
%%            {error, need_auth} |
%%            {ok, SubState, Status, Reason, ExtraHeaders, NewPkgState}  |
%%            {siperror, Status, Reason, ExtraHeaders}
%%
%%            Num        = integer() "the number of subscribes we have received on this dialog, starts at 1"
%%            Request    = #request{} "the SUBSCRIBE request"
%%            YxaCtx     = #yxa_ctx{}
%%            SIPuser    = undefined | string() "undefined if request originator is not not authenticated, and string() if the user is authenticated (empty string if user could not be authenticated)"
%%            Presentity = undefined          |
%%                         {user, User}       |
%%                         {address, Address}
%%            PkgState   = undefined | #my_state{}
%%
%%            SubState     = active | pending
%%            Status       = integer() "SIP status code to respond with"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = [{Key, ValueList}] "headers to include in the response to the SUBSCRIBE,"
%%            Body         = binary() | list()
%%            PkgState     = #my_state{}
%%
%% @doc     YXA event packages must export an is_allowed_subscribe/6
%%          function. This function is called when the event server
%%          receives a subscription request for this event package,
%%          and is the event packages chance to decide wether the
%%          subscription should be accepted or not. It is also called
%%          for every time the subscription is refreshed by the
%%          subscriber.
%% @end
%%--------------------------------------------------------------------
%%
%% SIPuser = undefined
%%
is_allowed_subscribe("presence", _Num, _Request, _YxaCtx, _SIPuser = undefined,
		     _Presentity, _PkgState) ->
    {error, need_auth};
%%
%% Presentity is {users, UserList}
%%
is_allowed_subscribe("presence", _Num, Request, _YxaCtx, SIPuser, {users, ToUsers} = _Presentity, PkgState)
  when is_list(SIPuser), is_list(ToUsers) ->
    is_allowed_subscribe2(Request#request.header, active, 200, "Ok", [], PkgState);
%%
%% Presentity is {address, AddressStr}
%%
is_allowed_subscribe("presence", _Num, Request, _YxaCtx, SIPuser,
		     {address, AddressStr} = _Presentity, PkgState) when is_list(SIPuser), is_list(AddressStr) ->
    is_allowed_subscribe2(Request#request.header, pending, 202, "Ok", [], PkgState).

is_allowed_subscribe2(Header, SubState, Status, Reason, ExtraHeaders, PkgState) when is_record(PkgState, my_state);
										     PkgState == undefined ->
    Accept = get_accept(Header),
    case presence_pidf:is_compatible_contenttype(subscribe, Accept) of
	true ->
	    NewPkgState =
		case PkgState of
		    #my_state{} ->
			PkgState;
		    undefined ->
			#my_state{}
		end,
	    Body = <<>>,
	    {ok, SubState, Status, Reason, ExtraHeaders, Body, NewPkgState};
	false ->
	    {siperror, 406, "Not Acceptable", []}
    end.

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), Presentity, LastAccept, PkgState) ->
%%            {ok, Body, ExtraHeaders, NewPkgState} |
%%            {error, Reason}
%%
%%            Presentity = {users, UserList} | {address, AddressStr}
%%            UserList   = [string()] "SIP usernames"
%%            AddressStr = string() "parseable with sipurl:parse/1"
%%            LastAccept = [string()] "Accept: header value from last SUBSCRIBE"
%%            PkgState   = #my_state{}
%%
%%            Body         = io_list()
%%            ExtraHeaders = [{Key, ValueList}] "headers to include in the NOTIFY request"
%%            Reason       = string() | atom()
%%            NewPkgState  = #my_state{}
%%
%% @doc     YXA event packages must export a notify_content/3
%%          function. Whenever the subscription requires us to
%%          generate a NOTIFY request, this function is called to
%%          generate the body and extra headers to include in the
%%          NOTIFY request.
%% @end
%%--------------------------------------------------------------------
notify_content("presence", {users, [ToUser]}, LastAccept, PkgState) when is_list(ToUser),
									 is_record(PkgState, my_state) ->
    notify_content2(ToUser, LastAccept, PkgState);
notify_content("presence", {address, AddressStr}, LastAccept, PkgState) when is_list(AddressStr),
									     is_record(PkgState, my_state) ->
    %% XXX check if Presentity is still an 'address' or if that AOR now resolves to one or more users
    notify_content2({fake_offline, AddressStr}, LastAccept, PkgState);
notify_content("presence", Presentity, _LastAccept, PkgState) when is_record(PkgState, my_state) ->
    logger:log(error, "Presence package: Generation of PIDF documents for non-single-user presentitys "
	       "not implemented yet (~p)", [Presentity]),
    {ok, "", [], PkgState}.

%% part of notify_content/4
notify_content2(ToUser, LastAccept, PkgState) ->
    logger:log(debug, "Presence package: Creating NOTIFY content (PIDF document) for user ~p (types accepted: ~p)",
	       [ToUser, LastAccept]),
    case presence_pidf:get_pidf_xml_for_user(ToUser, LastAccept) of
	{ok, _ContentType, ""} ->
	    logger:log(debug, "Presence package: Empty PIDF document produced"),
	    {ok, "", [], PkgState};

	{ok, ContentType, PIDF} ->
	    BinPIDF = list_to_binary(PIDF),
	    logger:log(debug, "Presence package: PIDF document produced, type ~p, ~p bytes",
		       [ContentType, size(BinPIDF)]),
	    ExtraHeaders = [{"Content-Type", [ContentType]}],
	    {ok, BinPIDF, ExtraHeaders, PkgState};

	{error, Reason} ->
	    logger:log(debug, "Presence package: Failed creating PIDF document for user ~p : ~p",
		       [ToUser, Reason]),
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec    package_parameters(PkgS::event_pkg(), Param) ->
%%            Value | undefined
%%
%%            Param = atom()
%%
%%            Value = term()
%%
%% @doc     YXA event packages must export a package_parameters/2
%%          function. 'undefined' MUST be returned for all unknown
%%          parameters.
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), notification_rate_limit) ->
%%            MilliSeconds
%%
%%            MilliSeconds = integer()
%%
%% @doc     The minimum amount of time that should pass between
%%          NOTIFYs we send about this event packages events.
%% @end
%%--------------------------------------------------------------------
package_parameters("presence", notification_rate_limit) ->
    %% The minimum amount of time that the SIP event package RFC in question
    %% says SHOULD pass between NOTIFYs generated by this event package.
    5000;

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), request_methods) ->
%%            Methods
%%
%%            Methods = [string()]
%%
%% @doc     What SIP methods this event packages request/7 function
%%          can handle.
%% @end
%%--------------------------------------------------------------------
package_parameters("presence", request_methods) ->
    ["PUBLISH", "NOTIFY"];

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), subscribe_accept_content_types) ->
%%            ContentTypes
%%
%%            ContentTypes = [string()]
%%
%% @doc     What Content-Type encodings we should list as acceptable
%%          in SUBSCRIBEs we send.
%% @end
%%--------------------------------------------------------------------
package_parameters("presence", subscribe_accept_content_types) ->
    presence_pidf:get_supported_content_types(set);

package_parameters("presence", _Param) ->
    undefined.


%%--------------------------------------------------------------------
%% @spec
%%    subscription_behaviour(PkgS::event_pkg(), Param, Argument) ->
%%            Value | undefined
%%
%%            Param    = atom()
%%            Argument = term() "depending on Param"
%%
%%            Value = term()
%%
%% @doc     YXA event packages must export a sbuscription_behaviour/2
%%          function. 'undefined' MUST be returned for all unknown
%%          parameters.
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (PkgS::event_pkg(), bidirectional_subscribe, Request) ->
%%            true | false
%%
%%            Request = #request{}
%%
%% @doc     When we receive a SUBSCRIBE, should the subscription
%%          handler also SUBSCRIBE to the other side in the same
%%          dialog? For the presence package, this depends on if the
%%          SUBSCRIBE has an Allow-Events header listing "presence".
%% @end
%%--------------------------------------------------------------------
subscription_behaviour("presence", bidirectional_subscribe, Request) when is_record(Request, request) ->
    case keylist:fetch('allow-events', Request#request.header) of
	[] ->
	    case keylist:fetch('user-agent', Request#request.header) of
		["KPhone" ++ _] -> true;	%% KPhone softphone
		["WirelessIP5000"] -> true;	%% Hitachi WIP-5000
		%% disabled, Windows Messenger won't answer SUBSCRIBEs we send
	 	%% ["RTC/1.3"] -> true;		%% Windows Messenger
		_ -> false
	    end;
	Events ->
	    Lowercased = [http_util:to_lower(E) || E <- Events],
	    lists:member("presence", Lowercased)
    end;

subscription_behaviour("presence", _Param, _Argument) ->
    undefined.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Header) -> [string()]
%%
%%            Header = #keylist{}
%%
%% @doc     Get Accept: header value (or default) from a header.
%% @end
%%--------------------------------------------------------------------
get_accept(Header) ->
    case keylist:fetch('accept', Header) of
	[] ->
	    %% RFC3856 #6.7
	    ["application/pidf+xml"];
	AcceptV ->
	    [http_util:to_lower(Elem) || Elem <- AcceptV]
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, SIPuser, THandler) ->
%%            {ok, Action, ETag, Expires} | error
%%
%%            Request  = #request{}
%%            SIPuser  = string()
%%            THandler = term() "server transaction handle"
%%
%%            Action  = insert | update | refresh
%%            ETag    = none | string()
%%            Expires = integer()
%%
%% @doc     Get ETag and Expires values from a PUBLISH request. Note :
%%          Functionality is specified in RFC3903 #6 (Processing
%%          PUBLISH Requests), steps 2-3
%% @end
%%--------------------------------------------------------------------
get_publish_etag_expires(Request, SIPuser, THandler) ->
    {Action, ETag} =
	case keylist:fetch("SIP-If-Match", Request#request.header) of
	    [ETag1] ->
		case presence_pidf:check_if_user_etag_exists(SIPuser, ETag1) of
		    true ->
			case Request#request.body of
			    <<>> ->
				%% Has body, is not a refresh but rather an update
				{update, ETag1};
			    _ ->
				%%% "a PUBLISH request that refreshes event state MUST NOT have a body."
				{refresh, ETag1}
			end;
		    false ->
			transactionlayer:send_response_handler(THandler, 412, "Conditional Request Failed"),
			error
		end;
	    [] ->
		{insert, none};
	    _ ->
		%% more than one value, reject request
		transactionlayer:send_response_handler(THandler, 400, "More than one SIP-If-Match header value"),
		error
	end,

    case ETag of
	error ->
	    error;
	_ ->
	    case publish_get_expires(Request#request.header, THandler) of
		error ->
		    error;
		Expires when is_integer(Expires) ->
		    {ok, Action, ETag, Expires}
	    end
    end.

%% part of get_publish_etag_expires/3
%% Returns : Expires = integer() | throw({error, Reason})
publish_get_expires(Header, THandler) ->
    case sipheader:expires(Header) of
	[E_Str] ->
	    try list_to_integer(E_Str) of
		Expires when is_integer(Expires) ->
		    {ok, Min} = yxa_config:get_env(presence_min_publish_time),
		    case (Expires > 0 andalso Expires < Min) of
			true ->
			    transactionlayer:send_response_handler(THandler, 423, "Interval Too Brief",
								   [{"Min-Expires", [integer_to_list(Min)]}]
								  ),
			    error;
			false ->
			    {ok, Max} = yxa_config:get_env(presence_max_publish_time),
			    lists:min([Expires, Max])
		    end
	    catch
		_ : _ ->
		    transactionlayer:send_response_handler(THandler, 400, "Bad Expires value"),
		    throw({error, bad_expires_value})
	    end;
	[] ->
	    {ok, Default} = yxa_config:get_env(presence_default_publish_time),
	    Default
    end.

%%--------------------------------------------------------------------
%% @spec    () ->
%%            ETag
%%
%%            ETag = string()
%%
%% @doc     Generate an ETag value. ETag values probably only need to
%%          be unique at a given time for package+user or similar,
%%          but this is a very easy solution.
%% @end
%%--------------------------------------------------------------------
generate_etag() ->
    {A, B, C} = erlang:now(),
    ETag1 = lists:concat([siprequest:myhostname(), "-", A, "-", B, "-", C]),
    lists:flatten(ETag1).
