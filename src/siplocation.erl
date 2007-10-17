%%%-------------------------------------------------------------------
%%% File    : siplocation.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Interface module for the location database. Also handles
%%%           processing of REGISTER requests for our domains,
%%%           received by 'incomingproxy' and 'outgoingproxy'.
%%%
%%% @since    29 Jan 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------

%%       ___How REGISTER requests are processed___
%% - - - - - - - - - - - - - - - - - -
%%
%%    UAC     UAC     UAC    ....     (e.g. IP-phones)
%%      \      |      /
%%       \    _|_____/_
%%        \__/         \__
%%        /               \
%%       |    Internet     |
%%        \__           __/
%%           \_________/
%%                |
%% - - - - - - - -|- - - - - - - - - - YXA
%%           \    |    /
%%            \   |   /
%%         -----------------
%%        | Transport layer |    requests are received from the
%%         -----------------     network
%%                |
%%        -------------------
%%       | Transaction layer |   sipserver is used to spawn one
%%        -------------------    process per sip request and process
%%          /       |       \    them asynchronously
%%         /        |        \
%%    sipserver   sipserver   sipserver ...
%%        |
%% - - - -|- - - - - - - - - - - - - - PROCESS PROCESSING REQUEST
%%        |
%%    incomingproxy.erl          entry point in processing REGISTER
%%        |                      request
%%        |
%%    siplocation.erl            the database update calls are done
%%        |                      in this module
%%        |
%% - - - -|- - - - - - - - - - - - - - DATABASE UPDATES
%%   \    |    /
%%    \   |   /
%%     mnesia                    mnesia supplies transaction support,
%%                               so that requests can be processed
%%                               atomically and can also
%%                               (transparently) be distributed on
%%                               several erlang nodes
%%
%%       ___Notes on the current RFC compliance___
%%
%% RFC 3261 chapter 10.3 p62 - "REGISTER requests MUST be processed
%% by a registrar in the order that they are received."
%% RFC 3261 chapter 10.3 p64 - "... , it MUST remove the binding only
%% if the CSeq in the request is higher than the value stored for
%% that binding.  Otherwise, the update MUST be aborted and the
%% request fails." - in regards to request that arrive out of order
%%
%% The first quote is can be interpreted in a number of ways which
%% contradict the actions taken in the second quote:
%%
%% * the "simple way" - process REGISTERs in order
%% - this is hard because the transport layer receives messages from
%%   various sources on different physical connections. It also limits
%%   scalability (parallelism) in the system.
%%
%% * would it be enough to process REGISTERs for the same UAC
%%   (Call-ID) in order ?
%% - this would be doable but would require solutions like delaying
%%   REGISTERs[1] which have CSeq number which are to high
%%   (NewCSeq - OldCSeq >= 2) compared to the last entry in the DB.
%% - looking at quote no.2 we can conclude that we don't need to
%%   reorder requests that were received out of order, when they come
%%   from the network.
%% - If REGISTERs are sent very quickly in succession - something that
%%   is unlikely to occur in practice, the asynchronous nature of
%%   YXAs request processing may indeed result in out of order
%%   processing, but it would be much rarer than out of order
%%   request received from the network - which don't need to be
%%   reordered. The two REGISTERs could of course be received from a
%%   single client pipelined using TCP, but see the conclusions below.
%%
%%  [1]: spawning a "wait & retry" process or resending request
%%       to the process mailbox
%%
%%  Conclusion:
%%  * Quote no.1 appears incorrect in requiring all REGISTERs to be
%%    processed "in order". The requirement appears to be a
%%    "best effort" attempt at processing requests in order.
%%  * Full compliance to quote no. 1 appears both unnecessary,
%%    tedious to implement and to make the system less scalable - so
%%    the current implementation will be retained.
%%
%%--------------------------------------------------------------------

-module(siplocation).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 process_register_request/3,
	 prioritize_locations/1,
	 get_locations_for_users/1,
	 get_user_with_contact/1,
	 get_locations_with_contact/1,
	 to_url/1,
	 sort_most_recent_first/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("phone.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(reg_request, {uri,			%% sipurl record(), REGISTER Request-URI
		      header,			%% keylist record(), REGISTER request header
		      to_url,			%% sipurl record(), To: header URL
		      callid,			%% string(), Call-Id header value
		      cseq,			%% string(), CSeq number
		      expire_h,			%% [ExpireStr], Expire: header value
		      sipuser,			%% string(), authenticated SIP username
		      thandler,			%% term(), server transaction handle
		      priority = 100,		%% integer(), default priority for registrations
		      logtag,			%% string(), prefix for logging
		      logstr,			%% string(), describes request
		      appname,			%% atom(), YXA application name
		      do_gruu,			%% bool(), GRUU enabled or not?
		      path_ignsup,		%% bool(), allow Path without Supported: path?
		      path_vector,		%% list() of string()
		      origin,			%% siporigin record(), info about origin of REGISTER request
		      do_outbound,		%% bool(), SIP Outbound enabled or not?
		      user_agent		%% string(), User-Agent header value
		     }).


%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, AppName) -> not_homedomain | void()
%%
%%            Request = #response{}
%%            YxaCtx  = #yxa_ctx{}
%%            AppName = atom() "application name"
%%
%% @doc     Process a received REGISTER. First check if it is for one
%%          of our domains (homedomain), then check that it contains
%%          proper authentication and that the authenticated user is
%%          allowed to register using this address-of-record.
%%          Finally, let process_updates() process all the Contact
%%          headers and update the location database.
%% @end
%%--------------------------------------------------------------------
process_register_request(Request, YxaCtx, AppName) when is_record(Request, request), is_record(YxaCtx, yxa_ctx),
							is_atom(AppName) ->
    #yxa_ctx{logstr     = LogStr,
	     origin     = Origin,
	     thandler   = THandler,
	     app_logtag = LogTag
	    } = YxaCtx,
    URL = Request#request.uri,
    LogPrefix = lists:concat([LogTag, ": ", AppName]),
    logger:log(debug, "~p: REGISTER ~p", [LogPrefix, sipurl:print(URL)]),
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 1
    %% check if this registrar handles the domain the request wants to register for
    case local:homedomain(URL#sipurl.host) of
	true ->
	    RegReq = #reg_request{uri		= URL,
				  header	= Request#request.header,
				  origin	= Origin,
				  logtag	= LogPrefix,
				  logstr	= LogStr,
				  thandler	= THandler,
				  appname	= AppName
				 },
	    register_require_supported(RegReq, LogTag);
	false ->
	    %% act as proxy and forward message to other domain
	    logger:log(debug, "~p: REGISTER for non-homedomain ~p", [AppName, URL#sipurl.host]),
	    not_homedomain
    end.

%%--------------------------------------------------------------------
%% @spec    (RegReq, OrigLogTag) -> term()
%%
%%            RegReq     = #reg_request{}
%%            OrigLogTag = string() "logtag used for events"
%%
%% @doc     After we have checked that the REGISTER request is for one
%%          of our homedomains, we start checking the validity of the
%%          request. First, we do some checks in this function before
%%          going on to making sure the request is authenticated in
%%          register_authenticate(...).
%% @end
%%--------------------------------------------------------------------
register_require_supported(RegReq, OrigLogTag) when is_record(RegReq, reg_request), is_list(OrigLogTag) ->
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 2
    case is_valid_register_request(RegReq#reg_request.header) of
	true ->
	    register_authenticate(RegReq, OrigLogTag);
	{siperror, Status, Reason, ExtraHeaders} ->
	    THandler = RegReq#reg_request.thandler,
	    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders)
    end.

%% part of process_register/6
register_authenticate(RegReq, OrigLogTag) when is_record(RegReq, reg_request), is_list(OrigLogTag) ->
    #reg_request{logtag		= LogTag,
		 thandler	= THandler,
		 logstr		= LogStr
		} = RegReq,

    logger:log(debug, "~p: ~s -> processing", [LogTag, RegReq#reg_request.logstr]),
    %% delete any present Record-Route header (RFC3261, #10.3) - strictly speaking not necessary
    %% since we won't look at the Record-Route header in the following code anyways
    NewHeader = keylist:delete("Record-Route", RegReq#reg_request.header),
    {_, ToURL} = sipheader:to(NewHeader),
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 3, step 4 and step 5
    %% authenticate UAC
    case local:can_register(NewHeader, ToURL) of
	{{true, _}, SIPuser} ->
	    Contacts = sipheader:contact(NewHeader),
	    logger:log(debug, "~s: user ~p, registering contact(s) : ~p",
		       [LogTag, SIPuser, sipheader:contact_print(Contacts)]),

	    %% Fetch configuration parameters here and pass as parameters, to make process_updates testable
	    {ok, DoGRUU} = yxa_config:get_env(experimental_gruu_enable),
	    {ok, PathIgnSup} = yxa_config:get_env(allow_proxy_inserted_path),
	    {ok, DoOutbound} = yxa_config:get_env(experimental_outbound_enable),

	    NewRegReq =
		RegReq#reg_request{header	= NewHeader,
				   to_url	= ToURL,
				   sipuser	= SIPuser,
				   do_gruu	= DoGRUU,
				   path_ignsup	= PathIgnSup,
				   do_outbound	= DoOutbound
				  },

	    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 6, step 7 and step 8
	    case catch process_updates(NewRegReq, Contacts) of
		{ok, {Status, Reason, ExtraHeaders}} ->
		    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders),
		    %% Make event about user sucessfully registered
		    L = [{register, ok}, {user, SIPuser},
			 {contacts, sipheader:contact_print(Contacts)}],
		    event_handler:generic_event(normal, location, OrigLogTag, L),
		    ok;
		{siperror, Status, Reason} ->
		    transactionlayer:send_response_handler(THandler, Status, Reason);
		{siperror, Status, Reason, ExtraHeaders} ->
		    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders);
		{'EXIT', Reason} ->
		    logger:log(error, "=ERROR REPORT==== siplocation:process_updates() failed :~n~p~n",
			       [Reason]),
		    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
	    end;
	{stale, _} ->
	    logger:log(normal, "~s -> Authentication is STALE, sending new challenge", [LogStr]),
	    transactionlayer:send_challenge(THandler, www, true, none);
	{{false, eperm}, SipUser} when SipUser /= none ->
	    logger:log(normal, "~s: SipUser ~p NOT ALLOWED to REGISTER address ~s",
		       [LogTag, SipUser, sipurl:print(ToURL)]),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden"),
	    %% Make event about users failure to register
	    L = [{register, forbidden}, {user, SipUser}, {address, sipurl:print(ToURL)}],
	    event_handler:generic_event(normal, location, OrigLogTag, L);
	{{false, nomatch}, SipUser} when SipUser /= none ->
	    logger:log(normal, "~s: SipUser ~p tried to REGISTER invalid address ~s",
		       [LogTag, SipUser, sipurl:print(ToURL)]),
	    transactionlayer:send_response_handler(THandler, 404, "Not Found"),
	    %% Make event about users failure to register
	    L = [{register, invalid_address}, {user, SipUser}, {address, sipurl:print(ToURL)}],
	    event_handler:generic_event(normal, location, OrigLogTag, L);
	{false, none} ->
	    Level = case keylist:fetch('authorization', RegReq#reg_request.header) of
			[] -> debug;
			_ -> normal
		    end,
	    %% XXX send new challenge (current behavior) or send 403 Forbidden when authentication fails?
	    logger:log(Level, "~s -> Authentication FAILED, sending challenge", [LogStr]),
	    transactionlayer:send_challenge(THandler, www, false, 3)
    end.

%%--------------------------------------------------------------------
%% @spec    (Header) -> true | SipError
%%
%%            Header = #keylist{}
%%
%% @doc     looks for unsupported extensions.
%% @end
%%--------------------------------------------------------------------
is_valid_register_request(Header) ->
    case keylist:fetch('require', Header) of
	[] ->
	    true;
	Require ->
	    case get_unsupported_extensions(Require) of
		[] ->
		    true;
		L ->
		    logger:log(normal, "Request check: The client requires unsupported extension(s) ~p", [Require]),
		    {siperror, 420, "Bad Extension", [{"Unsupported", L}]}
	    end
    end.

get_unsupported_extensions(In) ->
    {ok, DoGRUU} = yxa_config:get_env(experimental_gruu_enable),
    get_unsupported_extensions2(In, DoGRUU, []).

get_unsupported_extensions2(["gruu" | T], DoGRUU, Res) ->
    %% draft-ietf-sip-gruu-07
    case DoGRUU of
	true ->
	    get_unsupported_extensions2(T, DoGRUU, Res);
	false ->
	    logger:log(debug, "Request check: GRUU requested, but not enabled (experimental)"),
	    get_unsupported_extensions2(T, DoGRUU, ["gruu" | Res])
    end;
get_unsupported_extensions2(["path" | T], DoGRUU, Res) ->
    %% RFC3327
    get_unsupported_extensions2(T, DoGRUU, Res);
get_unsupported_extensions2([H | T], DoGRUU, Res) ->
    get_unsupported_extensions2(T, DoGRUU, [H | Res]);
get_unsupported_extensions2([], _DoGRUU, Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% @spec    (RegReq, Contacts) ->
%%            {ok, {Status, Reason, ExtraHeaders}}     |
%%            {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders}
%%
%%            RegReq   = #reg_request{}
%%            Contacts = [#contact{}]
%%
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = [{Key, NewValueList}]
%%
%% @doc     Update the location database, based on a REGISTER request
%%          we are processing. Either add or remove entrys.
%% @end
%%--------------------------------------------------------------------
%% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 6 and 7
%% remove, add or update contact info in location (phone) database

%% REGISTER request had no contact header
process_updates(RegReq, []) when is_record(RegReq, reg_request) ->
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 8
    {ok, create_process_updates_response(RegReq)};

process_updates(RegReq, Contacts) when is_record(RegReq, reg_request), is_list(Contacts) ->
    #reg_request{sipuser = SipUser,
		 logtag  = LogTag
		} = RegReq,
    %% Processing REGISTER Request - step 6
    %% check for and process wildcard (request contact = *)
    case process_register_wildcard_isauth(RegReq, Contacts) of
	none ->
	    case process_updates_get_path_vector(RegReq) of
		PathVector when is_list(PathVector) ->
		    %% Processing REGISTER Request - step 7
		    %% No wildcard found, register/update/remove entries in Contacts.
		    %% Process registration atomicly - change all or nothing in database.
		    NewRegReq = RegReq#reg_request{path_vector = PathVector},
		    F = fun() ->
				process_non_wildcard_contacts(NewRegReq, Contacts)
			end,
		    case mnesia:transaction(F) of
			{aborted, Reason} ->
			    logger:log(error, "~s: Location database: REGISTER request failed to add/update/remove one "
				       "or more contacts for user ~p, failed due to: ~n~p", [LogTag, SipUser, Reason]),
			    %% Check if it was a siperror, otherwise return '500 Server Internal Error'
			    case Reason of
				{throw, {siperror, Status, Reason2}} ->
				    {siperror, Status, Reason2};
				_ ->
				    {siperror, 500, "Server Internal Error"}
			    end;
			{atomic, _ResultOfFun} ->
			    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 8
			    {ok, create_process_updates_response(RegReq)}
		    end;
		Other ->
		    Other
	    end;
	ok ->
	    %% wildcard found and processed
	    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 8
	    {ok, create_process_updates_response(RegReq)};
	SipError ->
	    SipError
    end.

%%--------------------------------------------------------------------
%% @spec    (RegReq) ->
%%            PathVector |
%%            {siperror, Status, Reason, ExtraHeaders}
%%
%%            RegReq = #reg_request{}
%%
%%            PathVector = [string()]
%%
%% @doc     Examine Path: header in request, and possibly add
%%          ourselves to the path vector. Create a path vector if
%%          there were none.
%% @end
%%--------------------------------------------------------------------
process_updates_get_path_vector(RegReq) when is_record(RegReq, reg_request) ->
    #reg_request{uri		= URL,
		 header		= Header,
		 appname	= AppName,
		 path_ignsup	= IgnoreSupported
		} = RegReq,
    Proto = URL#sipurl.proto,
    process_updates_get_path_vector2(Proto, Header, AppName, IgnoreSupported).

%% part of process_updates_get_path_vector/1, to make things easily testable
%% Returns : PathVector = list() of string() |
%%           {siperror, Status, Reason, ExtraHeaders}
process_updates_get_path_vector2(Proto, Header, AppName, IgnoreSupported) ->
    case keylist:fetch('path', Header) of
	[] ->
	    process_updates_get_path_vector3(Proto, [], AppName);
	Path ->
	    %% Request has Path, check if UA says it supports Path. Reject with 420 Bad Extension
	    %% if Path is present but Supported: does not contain "path", unless configured not to
	    IsSupported = sipheader:is_supported("path", Header),
	    if
		IsSupported ->
		    process_updates_get_path_vector3(Proto, Path, AppName);
		IsSupported /= true, IgnoreSupported ->
		    logger:log(debug, "Location database: Notice: Storing path vector even though Supported: "
			       "does not contain \"path\""),
		    process_updates_get_path_vector3(Proto, Path, AppName);
		true ->
		    logger:log(debug, "Location database: Rejecting REGISTER request since it has Path: but "
			       "the originating UA hasn't indicated support for RFC3327"),
		    {siperror, 421, "Extension Required", [{"Require", ["path"]}]}
	    end
    end.

%% part of process_updates_get_path_vector2/4, this is where we actually add ourselves to the path vector
%% Returns : list() of string()
process_updates_get_path_vector3(Proto, Path, outgoingproxy) ->
    RouteStr1 = siprequest:construct_record_route(Proto),
    %% add an ;ob URI parameter, draft-Outbound 5.1 (Processing Register Requests)
    %% "if it adds a Path header field value, it MUST include the 'ob' parameter in its Path URI"
    [C] = contact:parse([RouteStr1]),
    URL = sipurl:parse(C#contact.urlstr),
    NewParams = url_param:add(URL#sipurl.param_pairs, "ob"),
    NewURL = sipurl:set([{param, NewParams}], URL),
    RouteStr = contact:print( contact:new(NewURL) ),
    [RouteStr | Path];
process_updates_get_path_vector3(_Proto, Path, _AppName) ->
    Path.

%% part of process_updates/6. Returns {200, "OK", ExtraHeaders}
create_process_updates_response(RegReq) ->
    #reg_request{sipuser	= SipUser,
		 header		= Header,
		 do_gruu	= GRUU_enabled,
		 do_outbound	= DoOutbound
		} = RegReq,
    Date = {"Date", [httpd_util:rfc1123_date()]},

    Path =
	case keylist:fetch('path', Header) of
	    [] ->
		[];
	    PathV ->
		%% "The registrar copies the Path header field values into a Path header
		%% field in the successful (200 Class) REGISTER response."
		%% RFC3327 #5.3 (Procedures at the Registrar)
		[{"Path", PathV}]
	end,

    %% "The Registrar MUST include the 'outbound' option-tag in a Supported
    %% header field value in its responses to REGISTER requests."
    %% draft Outbound 03 #6.1 (Processing Register Requests)
    Outbound = case DoOutbound of
		   true -> [{"Supported", ["outbound"]}];
		   false -> []
	       end,

    Do_GRUU = sipheader:is_supported("gruu", Header) andalso GRUU_enabled,
    case fetch_contacts(SipUser, Do_GRUU, DoOutbound, keylist:fetch('to', Header)) of
	{ok, _AddRequireGRUU, []} ->
	    {200, "OK", [Date] ++ Outbound ++ Path};
	{ok, AddRequireGRUU, NewContacts} when is_boolean(AddRequireGRUU), is_list(NewContacts) ->
	    H1 = [{"Contact", NewContacts}, Date] ++ Outbound ++ Path,
	    ExtraHeaders =
		case AddRequireGRUU of
		    true ->
			%% "If the REGISTER response contains a gruu Contact header field
			%% parameter in any of the contacts, the REGISTER response MUST contain
			%% a Require header field with the value "gruu". " GRUU draft 06 #7.1.2.1
			%% (Processing a REGISTER Request)
			%% UPDATE: This is no longer required by the spec (removed in GRUU draft 07)
			%% so we can stop doing it when we anticipate that clients have been updated
			[{"Require", ["gruu"]} | H1];
		    false ->
			H1
		end,
	    {200, "OK", ExtraHeaders}
    end.


%%--------------------------------------------------------------------
%% @spec    (SipUser, Do_GRUU, DoOutbound, To) ->
%%            {ok, GRUUs, Contacts}
%%
%%            SipUser    = string() "SIP authentication user - key in location database"
%%            Do_GRUU    = bool() "add gruu= parameters or not?"
%%            DoOutbound = bool() "is outbound enabled?"
%%            To         = [string()] "REGISTER request To header"
%%
%%            GRUUs    = bool() "true if one or more of the contacts were equipped with 'gruu' contact parameters"
%%            Contacts = [string()] "formated as a contact field-value (see RFC 3261)"
%%
%% @doc     Find all the locations where a specific sipuser can be
%%          located (e.g. all the users phones).
%% @end
%%--------------------------------------------------------------------
fetch_contacts(SipUser, Do_GRUU, DoOutbound, To) ->
    {ok, Locations} = phone:get_sipuser_locations(SipUser),
    {ContainsGRUUs, Contacts} = locations_to_contacts(Locations, SipUser, Do_GRUU,
						      DoOutbound, To, util:timestamp()),
    {ok, ContainsGRUUs, Contacts}.

%% Returns: {GRUUs, Contacts}
%%          GRUUs    = bool()
%%          Contacts = list() of string(), (contact:print/1 of contact record())
locations_to_contacts(Locations, SipUser, Do_GRUU, DoOutbound, To, Timestamp) ->
    locations_to_contacts2(Locations, Timestamp, SipUser, Do_GRUU, DoOutbound, To, false, []).

locations_to_contacts2([], _Now, _SipUser, _Do_GRUU, _DoOutbound, _To, GRUUs, Res) ->
    {GRUUs, Res};
locations_to_contacts2([#siplocationdb_e{expire = never} | T], Now, SipUser, Do_GRUU, DoOutbound, To, GRUUs, Res) ->
    %% Don't include static contacts which never expire
    locations_to_contacts2(T, Now, SipUser, Do_GRUU, DoOutbound, To, GRUUs, Res);
locations_to_contacts2([#siplocationdb_e{expire = Expire} = H | T], Now, SipUser, Do_GRUU, DoOutbound, To, GRUUs, Res)
  when is_integer(Expire) ->
    Location = H#siplocationdb_e.address,

    %% Expires can't be less than 0 so make sure we don't end up with a negative Expires
    NewExpire = lists:max([0, Expire - Now]),

    {NewGRUUs, GRUUparams} = locations_to_contacts2_gruu(Do_GRUU, H, SipUser, To, GRUUs),
    OutboundParams = locations_to_contacts2_outbound(DoOutbound, Do_GRUU, H),
    ExpiresParams = [{"expires", integer_to_list(NewExpire)}],
    Params = lists:append([ExpiresParams, GRUUparams, OutboundParams]),
    Contact = contact:new(Location, Params),
    NewRes = [contact:print(Contact) | Res],
    locations_to_contacts2(T, Now, SipUser, Do_GRUU, DoOutbound, To, NewGRUUs, NewRes).

locations_to_contacts2_gruu(_DoGRUU = false, _E, _SipUser, _To, GRUUs) ->
    {GRUUs, []};
locations_to_contacts2_gruu(_DoGRUU = true, H, SipUser, To, GRUUs) ->
    case H#siplocationdb_e.instance of
	[] ->
	    {GRUUs, []};
	InstanceId when is_list(InstanceId) ->
	    I_ID_param = {"+sip.instance", "\"" ++ InstanceId ++ "\""},
	    case database_gruu:fetch_using_user_instance(SipUser, InstanceId) of
		{ok, [GRUUdbe]} ->
		    GRUU = gruu:extract(gruu, GRUUdbe),
		    GRUU_URL = gruu:make_url(SipUser, InstanceId, GRUU, To),
		    InstanceId = gruu:extract(instance_id, GRUUdbe),
		    %% GRUU draft 06 #7.1.2.1 (Processing a REGISTER Request)
		    %% ...
		    %% Furthermore, for each Contact header field value placed in the
		    %% response, if the registrar has stored an instance ID associated with
		    %% that contact, that instance ID is returned as a Contact header field
		    %% parameter.
		    %% ...
		    %% The value of the gruu parameter is a quoted string containing the URI
		    %% that is the GRUU for the associated instance ID/AOR pair.
		    GRUU_param = {"gruu", "\"" ++ sipurl:print(GRUU_URL) ++ "\""},
		    {true, [GRUU_param, I_ID_param]};
		nomatch ->
		    {GRUUs, [I_ID_param]}
	    end
    end.

locations_to_contacts2_outbound(_DoOutbound = false, _Do_GRUU, _H) ->
    [];
locations_to_contacts2_outbound(_DoOutbound = true, Do_GRUU, H) ->
    case H#siplocationdb_e.instance of
        [] ->
	    [];
	InstanceId when is_list(InstanceId) ->
	    case lists:keysearch(reg_id, 1, H#siplocationdb_e.flags) of
		{value, {reg_id, RegId}} when is_integer(RegId) ->
		    RegId_Param =
			[{"reg-id", integer_to_list(RegId)}],
		    case Do_GRUU of
			true ->
			    %% instance-id already included (by locations_to_contacts2_gruu)
			    RegId_Param;
			false ->
			    %% we have to add instance-id too
			    I_ID_param = {"+sip.instance", "\"" ++ InstanceId ++ "\""},
			    [I_ID_param | RegId_Param]
		    end;
		false ->
		    []
	    end
    end.

%% return = ok       | wildcard processed
%%          none     | no wildcard found
%%          SipError   db error
%% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 6
process_register_wildcard_isauth(RegReq, Contacts) ->
    #reg_request{logtag = LogTag,
		 header = Header,
		 sipuser = SipUser
		} = RegReq,
    case is_valid_wildcard_request(Header, Contacts) of
	true ->
	    logger:log(debug, "Location: Processing valid wildcard un-register"),
	    case phone:get_phone(SipUser) of
		{atomic, PhoneEntrys} ->
		    unregister(LogTag, Header, PhoneEntrys);
		E ->
		    logger:log(error, "Location: Failed fetching registered contacts for user ~p : ~p",
			       [SipUser, E]),
		    {siperror, 500, "Server Internal Error"}
	    end;
	false ->
	    none;
	SipError when is_tuple(SipError), element(1, SipError) == siperror ->
	    SipError
    end.

%% return = ok       | wildcard processed
%%          SipError   db error
%% unregister all location entries for a sipuser
unregister(LogTag, Header, PhoneEntrys) ->
    %% unregister all Locations entries
    F = fun() ->
		unregister_contacts(LogTag, Header, PhoneEntrys)
	end,
    %% process unregistration atomically - change all or nothing in database
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    logger:log(error, "Database: unregister of registrations failed for one or more"
		       " contact entries, due to: ~p",
		       [Reason]),
	    case Reason of
		{throw, {siperror, Status, Reason2}} ->
		    {siperror, Status, Reason2};
		_ ->
		    {siperror, 500, "Server Internal Error"}
	    end;
	{atomic, _ResultOfFun} ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (Header, Contacts) ->
%%            true            |
%%            false           |
%%            {siperror, Status, Reason}
%%
%%            Header   = #keylist{} "the sip request headers"
%%            Contacts = [#contact{}]
%%
%%            Status = integer() "SIP status code"
%%            Reason = string() "SIP reason phrase"
%%
%% @doc     determine if request is a properly formed wildcard request
%%          (see RFC 3261 Chapter 10 page 64 - step 6)
%% @end
%%--------------------------------------------------------------------
%% there is only one Contact and it's a wildcard
is_valid_wildcard_request(Header, [ #contact{urlstr = "*"} ]) ->
    case sipheader:expires(Header) of
	[Expire] ->
	    %% cast to integer so that "0", "00" ... and so on are all considered as 0
	    case catch list_to_integer(Expire) of
		0 ->
		    true;
		_ ->
		    {siperror, 400, "Wildcard with non-zero contact expires parameter"}
	    end;
	[] ->
	    {siperror, 400, "Wildcard without Expires header"};
	_ ->
	    {siperror, 400, "Wildcard with more than one expires parameter"}
    end;

%% There are 2+ elements in Contacts, make sure that none of them are wildcards
%% - there can only be one wildcard
is_valid_wildcard_request(_Header, Contacts) ->
    case wildcard_grep(Contacts) of
	true ->
	    {siperror, 400, "Wildcard present but not alone, invalid (RFC3261 10.3 #6)"};
	false ->
	    false
    end.

%% is there a wildcard in the contacts list ?
%% return = true | false
wildcard_grep([]) ->
    false;
wildcard_grep([ #contact{urlstr = "*"} | _Rest]) ->
    true;
wildcard_grep([_Foo | Rest]) ->
    wildcard_grep(Rest).


%%--------------------------------------------------------------------
%% @spec    (URI) ->
%%            none | SIPuser
%%
%%            URI = #sipurl{}
%%
%%            SIPuser = string() "#phone.user field value"
%%
%% @doc     Checks if any of our users are registered at the location
%%          specified. Used to determine if we should proxy requests
%%          to a URI without authorization. NOTE : If you want to
%%          know all the users (in case there is more than one), you
%%          should use get_locations_with_contact/1 instead.
%% @end
%%--------------------------------------------------------------------
get_user_with_contact(URI) when is_record(URI, sipurl) ->
    case phone:get_sipusers_using_contact(URI) of
	{atomic, [SIPuser | _]} when is_list(SIPuser) ->
	    SIPuser;
	{atomic, []} ->
	    %% no one using URI found
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (URI) ->
%%            {ok, Locations} |
%%            error
%%
%%            URI = #sipurl{}
%%
%%            Locations = [#siplocationdb_e{}]
%%
%% @doc     Checks if any of our users are registered at the location
%%          specified. Return all the location database entrys
%%          matching the URI, so that the caller can look for a
%%          specific socket matching the registration(s) if the
%%          client used Outbound.
%% @end
%%--------------------------------------------------------------------
get_locations_with_contact(URI) when is_record(URI, sipurl) ->
    case phone:get_locations_using_contact(URI) of
	{ok, L} when is_list(L) ->
	    {ok, L};
	Unknown ->
	    logger:log(error, "Location: Unknown result from get_locations_using_contact/1 : ~p", [Unknown]),
	    error
    end.

%%--------------------------------------------------------------------
%% @spec    (SipUserList) -> [#siplocationdb_e{}]
%%
%%            SipUserList = [string()] "usernames"
%%
%% @doc     Looks up all locations for a list of users. Used to find
%%          out where a set of users are to see where we should route
%%          a request.
%% @end
%%--------------------------------------------------------------------
get_locations_for_users(In) when is_list(In) ->
    get_locations_for_users2(In, []).

get_locations_for_users2([], Res) ->
    Res;
get_locations_for_users2([H | T], Res) when is_list(H) ->
    {ok, Locations} = phone:get_sipuser_locations(H),
    get_locations_for_users2(T, Res ++ Locations).

%%--------------------------------------------------------------------
%% @spec    (Locations) -> [#siplocationdb_e{}]
%%
%%            Locations = [#siplocationdb_e{}]
%%
%% @doc     Look through a list of siplocation DB entrys, figure out
%%          what the lowest priority amongst them are and then return
%%          all records which has that priority.
%% @end
%%--------------------------------------------------------------------
prioritize_locations(Locations) when is_list(Locations) ->
    case get_priorities(Locations) of
	[BestPrio | _] ->
	    get_locations_with_prio(BestPrio, Locations);
	_ ->
	    %% No locations or no locations with priority - return input list
	    Locations
    end.

%% Descrip. = examine all Flags entries in Locations and return all
%%            priority values (if any are given) sorted with lowest (best)
%%            priority first.
%% Returns  = list() of integer()
get_priorities(Locations) when is_list(Locations) ->
    get_priorities2(Locations, []).

get_priorities2([#siplocationdb_e{flags=Flags} | T], Res) ->
    case lists:keysearch(priority, 1, Flags) of
	{value, {priority, Prio}} when Prio /= [] ->
	    get_priorities2(T, [Prio | Res]);
	false ->
	    %% no priority
	    get_priorities2(T, Res)
    end;
get_priorities2([], Res) ->
    lists:sort(Res).

%% Descrip.: find the Location/s that has the "best" priority in the Flags part of the tuple.
%%            Note that some may lack a {priority, PrioVal} entry
%% Returns : list() of siplocationdb_e record() (all with same "best" priority - Priority)
get_locations_with_prio(Priority, Locations) ->
    get_locations_with_prio2(Priority, Locations, []).

get_locations_with_prio2(_Priority, [], Res) ->
    lists:reverse(Res);
get_locations_with_prio2(Priority, [#siplocationdb_e{flags = Flags} = H | T], Res) ->
    case lists:keysearch(priority, 1, Flags) of
	{value, {priority, Priority}} ->
	    get_locations_with_prio2(Priority, T, [H | Res]);
	_ ->
	    %% other priority, or no priority
	    get_locations_with_prio2(Priority, T, Res)
    end.

%%--------------------------------------------------------------------
%% @spec    (RegReq, Contacts) ->
%%            void()                 
%%
%%            RegReq   = #reg_request{}
%%            Contacts = [#contact{}]
%%
%%            MnesiaError = {aborted, term()} | term()
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} |
%%            MnesiaError                              
%%
%% @doc     process a SIP Contact entry (thats not a wildcard) and do
%%          the appropriate db add/rm/update, see: RFC 3261 chapter
%%          10.3 - Processing REGISTER Request - step 7 for more
%%          details
%% @end
%%--------------------------------------------------------------------
process_non_wildcard_contacts(RegReq, Contacts) ->
    Header = RegReq#reg_request.header,
    CallId = sipheader:callid(Header),
    {CSeqStr, _CSeqMethod} = sipheader:cseq(Header),
    CSeq = list_to_integer(CSeqStr),

    %% get expire value from request header only once, this will speed up the calls to
    %% parse_register_contact_expire/2 that are done for each Locations entry
    ExpireHeader = sipheader:expires(Header),

    UserAgent = keylist:fetch('user-agent', Header),

    NewRegReq = RegReq#reg_request{callid     = CallId,
				   cseq       = CSeq,
				   expire_h   = ExpireHeader,
				   user_agent = UserAgent
				  },

    process_non_wildcard_contacts2(NewRegReq, Contacts).

%% process_non_wildcard_contacts2 - part of process_non_wildcard_contacts()
process_non_wildcard_contacts2(RegReq, [Contact | T]) ->
    case get_database_phone_entry(RegReq, Contact) of
	{ok, []} ->
	    %% User has no bindings in the location database, register this one
	    Expire = parse_register_contact_expire(RegReq#reg_request.expire_h, Contact),
	    register_contact(RegReq, Contact, Expire);
	{ok, [DBLocation]} ->
	    %% User has exactly one binding in the location database matching this one, do some checking
	    check_same_call_id(RegReq, Contact, DBLocation)
    end,
    %% Remove any entrys byte-by-byte identical to Contact from T
    NewT = T -- [Contact],
    process_non_wildcard_contacts2(RegReq, NewT);
process_non_wildcard_contacts2(_RegReq, []) ->
    ok.



get_database_phone_entry(RegReq, Contact) ->
    %% Check if contact has instance-id and reg-id, if so use that as key. Otherwise
    %% use Contact URI as key.
    {DoOutbound, RegId, Instance} =
	case get_contact_reg_id(Contact) of
	    RegId1 when is_integer(RegId1) ->
		case get_contact_instance_id(Contact) of
		    [] ->
			{false, [], []};
		    InstanceId1 ->
			{true, RegId1, InstanceId1}
		end;
	    none ->
		{false, [], []};
	    {error, _BadValue} ->
		{false, [], []}
	end,

    SipUser = RegReq#reg_request.sipuser,

    Res =
	case DoOutbound of
	    true ->
		{atomic, DbPhones} = phone:get_phone(SipUser),
		get_database_phone_instance_reg_id(Instance, RegId, DbPhones, []);
	    false ->
		%% check if SipUser-Location binding exists in database
		{atomic, Res1} = phone:get_sipuser_location_binding(SipUser, sipurl:parse(Contact#contact.urlstr)),
		Res1
	end,

    {ok, Res}.

get_database_phone_instance_reg_id(Instance, RegId, [#phone{instance = Instance} = H | T], Res) ->
    %% instance match, check reg-id flag too
    case lists:keysearch(reg_id, 1, H#phone.flags) of
	{value, {reg_id, RegId}} ->
	    get_database_phone_instance_reg_id(Instance, RegId, T, [H | Res]);
	_ ->
	    get_database_phone_instance_reg_id(Instance, RegId, T, Res)
    end;
get_database_phone_instance_reg_id(Instance, RegId, [_H | T], Res) ->
    %% no match
    get_database_phone_instance_reg_id(Instance, RegId, T, Res);
get_database_phone_instance_reg_id(_Instance, _RegId, [], Res) ->
    Res.

%%--------------------------------------------------------------------
%% @spec    (RegReq, Contact, DBLocation) ->
%%            void()                 
%%
%%            RegReq      = #reg_request{}
%%            Contact     = #contact{} "sipuser-location binding data in REGISTER request"
%%            DBLocations = #phone{} "currently stored sipuser-location info"
%%
%%            MnesiaError = {aborted, term()} | term()
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} |
%%            MnesiaError                              
%%
%% @doc     Check if the REGISTER we are processing has the same
%%          Call-Id as the currently stored location binding.
%% @end
%%--------------------------------------------------------------------
check_same_call_id(RegReq, Contact, DBLocation) ->
    Expire = parse_register_contact_expire(RegReq#reg_request.expire_h, Contact),

    case RegReq#reg_request.callid == DBLocation#phone.callid of
	true ->
	    %% request has same call-id so a binding already exists
	    check_greater_cseq(RegReq, Contact, DBLocation, Expire);
	false ->
	    %% call-id differs, so the UAC has probably been restarted.
	    case (Expire == 0) of
		true ->
		    LogTag = RegReq#reg_request.logtag,

		    %% zero expire-time, unregister binding
		    Priority = get_flag_value(priority, DBLocation),

		    logger:log(normal, "~s: UN-REGISTER ~s at ~s (priority ~p)",
			       [LogTag, DBLocation#phone.user, DBLocation#phone.requristr, Priority]),
		    phone:delete_record(DBLocation);
		false ->
		    register_contact(RegReq, Contact, Expire)
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (RegReq, Contact, DBLocation, Expire) ->
%%            void()                 
%%
%%            RegReq     = #reg_request{}
%%            Contact    = #contact{} "sipuser-location binding data in REGISTER request"
%%            DBLocation = #phone{} "currently stored sipuser-location info"
%%            Expire     = none | integer() "expire value supplied by UA client - either through an 'expires' contact parameter, or an Expires header"
%%
%%            MnesiaError = {aborted, term()} | term()
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} |
%%            MnesiaError                              
%%
%% @doc     Check if the REGISTER we are processing has the same
%%          Call-Id as the currently stored location binding.
%% @end
%%--------------------------------------------------------------------
check_greater_cseq(RegReq, Contact, DBLocation, Expire) ->
    #reg_request{cseq		= CSeq,
		 logtag		= LogTag
		} = RegReq,

    %% only process request if cseq is > than the last one processed i.e. ignore
    %% old, out of order requests
    case CSeq > DBLocation#phone.cseq of
	true ->
	    case (Expire == 0) of
		true ->
		    Priority = get_flag_value(priority, DBLocation),
		    %% unregister binding
		    logger:log(normal, "~s: UN-REGISTER ~s at ~s (priority ~p)",
			       [LogTag, DBLocation#phone.user, DBLocation#phone.requristr, Priority]),
		    phone:delete_record(DBLocation);
		false ->
		    %% update the binding
		    register_contact(RegReq, Contact, Expire)
	    end;
	false ->
	    #phone{user	     = DBSipUser,
		   requristr = DBContact,
		   cseq      = DBCSeq
		  } = DBLocation,
	    logger:log(debug, "Location: NOT updating binding for user ~p, entry ~p in db has CSeq ~p "
		       "and request has ~p", [DBSipUser, DBContact, DBCSeq, CSeq]),
	    %% RFC 3261 doesn't appear to document the proper error code for this case
	    throw({siperror, 403, "Request out of order, contained old CSeq number"})
    end.

%%--------------------------------------------------------------------
%% @spec    (LDBE) -> #sipurl{}
%%
%%            LDBE = #siplocationdb_e{}
%%
%% @doc     Create a SIP URL from a SIP location db entry.
%% @end
%%--------------------------------------------------------------------
to_url(LDBE) when is_record(LDBE, siplocationdb_e) ->
    LDBE#siplocationdb_e.address.

%%--------------------------------------------------------------------
%% @spec    (Locations) ->
%%            NewLocations
%%
%%            Locations = [#siplocationdb_e{}]
%%
%%            NewLocations = [#siplocationdb_e{}]
%%
%% @doc     Sort the locations based on when they were created, most
%%          recent locations first.
%% @end
%%--------------------------------------------------------------------
sort_most_recent_first(Locations) when is_list(Locations) ->
    F = fun(A, B) when is_record(A, siplocationdb_e), is_record(B, siplocationdb_e) ->
		A_Reg =
		    case lists:keysearch(registration_time, 1, A#siplocationdb_e.flags) of
			{value, {registration_time, ATime}} -> ATime;
			false -> 0
		    end,
		B_Reg =
		    case lists:keysearch(registration_time, 1, B#siplocationdb_e.flags) of
			{value, {registration_time, BTime}} -> BTime;
			false -> 0
		    end,
		(A_Reg >= B_Reg)
	end,
    lists:sort(F, Locations).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (RegReq, Contact, Expire) ->
%%            ok | {atomic, Result}
%%
%%            RegReq  = #reg_request{}
%%            Contact = #contact{}
%%            Expire  = none | integer() "value provided by UA client"
%%
%%            Result = term()
%%
%% @doc     add or update a location database entry
%% @end
%%--------------------------------------------------------------------
register_contact(RegReq, Contact, ExpireIn) when is_record(RegReq, reg_request), is_record(Contact, contact) ->
    #reg_request{logtag		= LogTag,
		 sipuser	= SipUser,
		 priority	= Priority,
		 callid		= CallId,
		 cseq		= CSeq
		} = RegReq,

    Expire = get_register_expire(ExpireIn),

    {ok, Flags, Instance} = register_contact_params(RegReq, Contact),

    logger:log(normal, "~s: REGISTER ~s at ~s (priority ~p, expire in ~p)",
	       [LogTag, SipUser, Contact#contact.urlstr, Priority, Expire]),

    case Instance /= [] of
	true ->
	    %% Always generate a GRUU for an instance id if there isn't any yet
	    gruu:create_if_not_exists(SipUser, Instance);
	false ->
	    ok
    end,

    phone:insert_purge_phone(SipUser, Flags, dynamic,
			     Expire + util:timestamp(),
			     sipurl:parse(Contact#contact.urlstr),
			     CallId, CSeq, Instance).

%%--------------------------------------------------------------------
%% @spec    (RegReq, Contact) ->
%%            {ok, Flags, Instance}
%%
%%            RegReq  = #reg_request{}
%%            Contact = #contact{}
%%
%%            Flags    = [{Key, Value}] "(sorted, to be testable)"
%%            Key      = atom()
%%            Value    = term()
%%            Instance = string() "instance ID (or \"\")"
%%
%% @doc     Create flags to store in the location database for this
%%          registration. For outgoingproxy, this includes an 'addr'
%%          that is the address of the proxy to which the UAC
%%          (presumably) has a persistent TCP connection that must be
%%          used in order to reach this client with SIP messages.
%%          Note : The registration_time is used by the GRUU
%%          mechanism in case there is more than one registered
%%          contact with the same instance id for a user. GRUU draft
%%          06 #8.4.1 (Request Targeting)
%% @end
%%--------------------------------------------------------------------
register_contact_params(RegReq, Contact) ->
    case RegReq#reg_request.path_vector of
	[] ->
	    register_contact_params2(RegReq, Contact);
	Path when is_list(Path) ->
	    {ok, MoreFlags, Instance} = register_contact_params2(RegReq, Contact),
	    {ok, lists:sort( [{path, Path} | MoreFlags] ), Instance}
    end.

%% Returns: {ok, Flags, Instance}
register_contact_params2(RegReq, Contact) ->
    Priority = RegReq#reg_request.priority,

    Instance = get_contact_instance_id(Contact),

    UA =
	case RegReq#reg_request.user_agent of
	    [UAgent] when is_list(UAgent) ->
		[{user_agent, UAgent}];
	    _ ->
		[]
	end,

    OutboundFlags = get_contact_outbound_flags(RegReq, Contact, Instance),

    Flags = lists:sort([{priority, Priority},
			{registration_time, util:timestamp()}
		       ] ++ OutboundFlags ++ UA
		      ),

    {ok, Flags, Instance}.

get_contact_outbound_flags(#reg_request{do_outbound = false}, _Contact, _Instance) ->
    %% outbound disabled
    [];
get_contact_outbound_flags(_RegReq, _Contact, _Instance = []) ->
    %% no Instance ID, so don't store reg-id even if it exists
    [];
get_contact_outbound_flags(RegReq, Contact, Instance) when is_record(RegReq, reg_request), is_list(Instance) ->
    case get_contact_reg_id(Contact) of
	RegId when is_integer(RegId) ->
	    %% draft Outbound 03 #6.1 (Processing Register Requests)
	    Origin = RegReq#reg_request.origin,
	    Id1 = (Origin#siporigin.sipsocket)#sipsocket.id,
	    SocketId = #locationdb_socketid{node  = node(),
					    id    = Id1,
					    proto = Origin#siporigin.proto,
					    addr  = Origin#siporigin.addr,
					    port  = Origin#siporigin.port
					   },
	    [{reg_id, RegId},
	     {socket_id, SocketId}
	    ];
	none ->
	    %% Instance, but no reg-id
	    [];
	{error, BadValue} ->
	    logger:log(debug, "Location: Ignoring bad (non-integer) reg-id/flow-id contact "
		       "parameter value ~p", [BadValue]),
	    []
    end.

%% Returns : RegId             |
%%           none              |
%%           {error, BadValue}
%%           RegId = integer()
%%           BadValue = string(), invalid value encountered
get_contact_reg_id(Contact) when is_record(Contact, contact) ->
    Str =
	case contact_param:find(Contact#contact.contact_param, "reg-id") of
	    [] ->
		%% earlier versions of draft-Outbound called it flow-id
		case contact_param:find(Contact#contact.contact_param, "flow-id") of
		    [FlowIdV] ->
			FlowIdV;
		    [] ->
			[]
		end;
	    [RegIdV] ->
		RegIdV
	end,

    case Str of
	[] ->
	    none;
	_ ->
	    case catch list_to_integer(Str) of
		RegId when is_integer(RegId) ->
		    RegId;
		_ ->
		    {error, Str}
	    end
    end.

%% Returns : Instance = string() ([] if no instance was found)
get_contact_instance_id(Contact) ->
    case contact_param:find(Contact#contact.contact_param, "+sip.instance") of
	[] -> [];
	[Instance1] ->
	    case string:chr(Instance1, 34) of    %% 34 is "
		0 ->
		    logger:log(debug, "Location: Ignoring +sip.instance parameter with non-quouted value ~p",
			       [Instance1]),
		    [];
		LeftQuoteIndex ->
		    TempString = string:substr(Instance1, LeftQuoteIndex + 1),
		    RightQuoteIndex = string:chr(TempString, 34),   %% 34 is "
		    string:substr(TempString, 1, RightQuoteIndex - 1)
	    end
    end.

%% determine expiration time for a specific contact. Use default
%% value if contact/header supplies no expiration period.
%% Returns : integer(), time in seconds
get_register_expire(Expire) ->
    case Expire of
	none ->
	    %% no expire - use default
	    3600;
	Expire when is_integer(Expire) ->
	    %% expire value supplied by request - we can choose to accept,
	    %% change (shorten/increase expire period) or reject too short expire
	    %% times with a 423 (Interval Too Brief) error.
	    %% Currently implementation only limits the max expire period
	    {ok, MaxRegisterTime} = yxa_config:get_env(max_register_time),

	    lists:min([MaxRegisterTime, Expire])
    end.


%%--------------------------------------------------------------------
%% @spec    (LogTag, RequestHeader, PhoneEntrys) -> ok
%%
%%            LogTag        = string() ""
%%            RequestHeader = #keylist{} ""
%%            PhoneEntrys   = [#phone{}] ""
%%
%% @throws  SipError 
%%
%% @doc     handles wildcard based removal (RFC 3261 chapter 10 page
%%          64 - step 6), this function handles a list of Locations
%%          (sipuser-location binding)
%% @end
%%--------------------------------------------------------------------
unregister_contacts(LogTag, RequestHeader, PhoneEntrys) when is_record(RequestHeader, keylist),
							     is_list(PhoneEntrys) ->
    RequestCallId = sipheader:callid(RequestHeader),
    {CSeq, _}  = sipheader:cseq(RequestHeader),
    RequestCSeq = list_to_integer(CSeq),

    F = fun(Phone) when is_record(Phone, phone) ->
		unregister_phone(LogTag, Phone, RequestCallId, RequestCSeq)
	end,
    lists:foreach(F, PhoneEntrys).


%% Descrip.: handles wildcard based removal (RFC 3261 chapter 10
%%           page 64 - step 6), this function handles a single
%%           Phone (phone record/sipuser-location binding)
unregister_phone(LogTag, #phone{class = dynamic}=Location, RequestCallId, RequestCSeq) ->
    SameCallId = (RequestCallId == Location#phone.callid),
    HigherCSeq = (RequestCSeq > Location#phone.cseq),
    %% RFC 3261 chapter 10 page 64 - step 6 check to see if
    %% sipuser-location binding (stored in Location) should be removed
    RemoveLocation = case {SameCallId, HigherCSeq} of
			 {true, true} -> true;
			 {false, _} -> true;
			 _ -> false
		     end,

    case RemoveLocation of
	true ->
	    phone:delete_record(Location),
	    Priority = get_flag_value(priority, Location),

	    SipUser = Location#phone.user,
	    logger:log(normal, "~s: UN-REGISTER ~s at ~s (priority ~p)",
		       [LogTag, SipUser, Location#phone.requristr, Priority]),
	    ok;
	false ->
	    %% Request CSeq value was too old, abort Request
	    %% RFC 3261 doesn't appear to document the proper error code for this case
	    throw({siperror, 403, "Request out of order, contained old CSeq number"})
    end;
unregister_phone(LogTag, Phone, _RequestCallId, _RequestCSeq) when is_record(Phone, phone) ->
    logger:log(debug, "~s: Not un-registering location with class not 'dynamic' : ~p",
	       [LogTag, Phone#phone.requristr]),
    ok.

%%--------------------------------------------------------------------
%% @spec    (ExpireHeader, Contact) ->
%%            integer() |
%%            none
%%
%%            Header       = #keylist{} "the request headers"
%%            ExpireHeader = [string()] "Expire header value"
%%            Contact      = #contact{} "a contact entry from a request"
%%
%% @doc     determine the expire time supplied for a contact in a SIP
%%          REGISTER request Note : Test order may not be changed as
%%          it is specified by RFC 3261 chapter 10 page 64 - step 6
%% @end
%%--------------------------------------------------------------------
parse_register_contact_expire(ExpireHeader, Contact) when is_list(ExpireHeader),
							  is_record(Contact, contact) ->
    %% first check if "Contact" has a expire parameter
    case contact_param:find(Contact#contact.contact_param, "expires") of
	[ContactExpire] ->
	    list_to_integer(ContactExpire);
	[] ->
	    %% then check for a expire header
	    case ExpireHeader of
		[HExpire] ->
		    list_to_integer(HExpire);
		[] ->
		    %% no expire found
		    none
	    end
    end.

get_flag_value(Key, #phone{flags = Flags}) ->
    case lists:keysearch(Key, 1, Flags) of
	{value, {Key, PriorityV}} ->
	    PriorityV;
	false ->
	    undefined
    end.


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

    %% test get_priorities(Locations)
    %%--------------------------------------------------------------------
    Loc0 = #siplocationdb_e{flags = [], expire = 0},
    Loc1 = #siplocationdb_e{flags = [{priority, 1}], expire = 1},
    Loc2 = #siplocationdb_e{flags = [{priority, 2}], expire = 2},
    Loc3 = #siplocationdb_e{flags = [{priority, 2}], expire = 3},

    autotest:mark(?LINE, "get_priorities/1 - 1"),
    %% normal case
    [1, 2, 2] = get_priorities([Loc0, Loc1, Loc2, Loc3]),

    autotest:mark(?LINE, "get_priorities/1 - 2"),
    %% no location with priority
    [] = get_priorities([Loc0]),


    %% test prioritize_locations(Locations)
    %%--------------------------------------------------------------------
    Loc4 = #siplocationdb_e{flags = [{priority, 4}], expire = 4},

    autotest:mark(?LINE, "prioritize_locations/1 - 1"),
    [Loc1] = prioritize_locations([Loc0, Loc1, Loc2, Loc3, Loc4, Loc0]),

    autotest:mark(?LINE, "prioritize_locations/1 - 2"),
    [Loc2, Loc3] = prioritize_locations([Loc2, Loc3, Loc4]),

    autotest:mark(?LINE, "prioritize_locations/1 - 3"),
    [Loc4] = prioritize_locations([Loc4, Loc0]),

    autotest:mark(?LINE, "prioritize_locations/1 - 4"),
    %% test without priority flag
    [Loc0] = prioritize_locations([Loc0]),


    %% is_valid_wildcard_request(Header, ContactList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 1"),
    %% test valid wildcard
    true = is_valid_wildcard_request(keylist:from_list([{"Expires", ["0"]}]), [contact:new("*")]),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 2"),
    %% test non-zero Expires
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["1"]}]), [contact:new("*")])),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 3"),
    %% test non-zero Expires, starting with a zero
    {siperror, 400, "Wildcard with non-zero contact expires parameter"} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["01"]}]), [contact:new("*")])),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 4"),
    %% test without Expires-header
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([]), [contact:new("*")])),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 5"),
    %% test with non-numeric Expires-header
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["test"]}]), [contact:new("*")])),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 6"),
    %% test with invalid Expires-header
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["0 invalid"]}]), [contact:new("*")])),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 7"),
    %% non-wildcard contact
    false = is_valid_wildcard_request(keylist:from_list([]), [contact:new("sip:ft@example.org")]),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 8"),
    %% multiple non-wildcard contact
    false = is_valid_wildcard_request(keylist:from_list([]), [contact:new("sip:ft@example.org"),
							      contact:new("sip:ft@example.net")]),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 9"),
    %% multiple contacts, one is a wildcard
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([]), [contact:new("*"),
								 contact:new("sip:ft@example.org")])),

    autotest:mark(?LINE, "is_valid_wildcard_request/2 - 10"),
    %% more than one Expires header value
    {siperror, 400, "Wildcard with more than one expires parameter"} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["0", "1"]}]), [contact:new("*")])),


    %% parse_register_expire(ExpireHeader, Contact)
    %% get_register_expire(Expire)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_register_expire/1 - 1"),
    %% test default
    3600 = get_register_expire(none),

    autotest:mark(?LINE, "get_register_expire/1 - 2"),
    %% test that contact parameters expire-value is used if present
    1202 = get_register_expire(1202),

    autotest:mark(?LINE, "get_register_expire/1 - 3"),
    %% test that contact can't be larger than maximum - XXX this check depends on configuration!
    43200 = get_register_expire(86400),


    %% locations_to_contacts(Locations, SipUser, Do_GRUU, DoOutbound, To, Timestamp)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "locations_to_contacts/6 - 0"),
    LTC_Now1 = util:timestamp(),
    LTC_L1 = #siplocationdb_e{expire = LTC_Now1 + 1, address = sipurl:parse("sip:ft@one.example.org")},
    LTC_L2 = #siplocationdb_e{expire = LTC_Now1 + 2, address = sipurl:parse("sip:ft@two.example.org")},
    LTC_L3 = #siplocationdb_e{expire = never, address = sipurl:parse("sip:ft@static.example.org")},

    LTC_L4 = #siplocationdb_e{address  = sipurl:parse("sip:ft@uac.example.org"),
				sipuser  = "ft.test",
				instance = "<urn:test:abc>",
				flags    = [],
				expire   = LTC_Now1 + 10
			       },

    autotest:mark(?LINE, "locations_to_contacts/6 - 1"),
    LTC_To = ["<sip:testuser@example.org>"],
    %% test basic case
    {false, ["<sip:ft@one.example.org>;expires=1", "<sip:ft@two.example.org>;expires=2"]} =
	locations_to_contacts([LTC_L2, LTC_L1], "testuser", false, false, LTC_To, LTC_Now1),

    autotest:mark(?LINE, "locations_to_contacts/6 - 2"),
    %% test that we ignore entrys that never expire
    {false, []} = locations_to_contacts([LTC_L3], "testuser", false, false, LTC_To, LTC_Now1),

    autotest:mark(?LINE, "locations_to_contacts/6 - 3"),
    %% test that we ignore entrys that never expire together with other entrys
    {false, ["<sip:ft@one.example.org>;expires=1", "<sip:ft@two.example.org>;expires=2"]} =
	locations_to_contacts([LTC_L2, LTC_L3, LTC_L1], "testuser", false, false, LTC_To, LTC_Now1),

    autotest:mark(?LINE, "locations_to_contacts/6 - 4"),
    %% test simple case, instance id but no reg_id parameter
    {false, ["<sip:ft@uac.example.org>;expires=10"]} =
	locations_to_contacts([LTC_L4], "ft.test", false, false, LTC_To, LTC_Now1),

    autotest:mark(?LINE, "locations_to_contacts/6 - 5"),
    LTC_L5 = LTC_L4#siplocationdb_e{flags = [{reg_id, 1}]
				   },
    %% test simple case, outbound
    {false, ["<sip:ft@uac.example.org>;expires=10;+sip.instance=\"<urn:test:abc>\";reg-id=1"]} =
	locations_to_contacts([LTC_L5], "ft.test", false, true, LTC_To, LTC_Now1),

    autotest:mark(?LINE, "locations_to_contacts/6 - 6"),
    %% test same thing, outbound disabled
    {false, ["<sip:ft@uac.example.org>;expires=10"]} =
	locations_to_contacts([LTC_L5], "ft.test", false, false, LTC_To, LTC_Now1),


    %% to_url(LDBE)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "to_url/1 - 0"),
    ToURL_URL1 = sipurl:parse("sip:ft@192.0.2.111;line=foo"),

    autotest:mark(?LINE, "to_url/1 - 1"),
    %% basic test
    ToURL_URL1 = to_url(#siplocationdb_e{flags = [], address = ToURL_URL1}),


    %% register_contact_params(Priority, Contact, Path)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "register_contact_params/3 - 1"),
    [RCF_Contact1] = contact:parse(["<sip:ft@192.0.2.123>"]),
    RCF_RegReq1 = #reg_request{priority		= 100,
			       path_vector	= []
			      },
    {ok, [{priority, 100}, {registration_time, _}], []} =
	register_contact_params(RCF_RegReq1, RCF_Contact1),

    autotest:mark(?LINE, "register_contact_params/3 - 2"),
    RCF_RegReq2 = #reg_request{priority		= 102,
			       path_vector	= ["one", "two"]
			      },
    {ok, [{path, ["one", "two"]}, {priority, 102}, {registration_time, _}], []} =
	register_contact_params(RCF_RegReq2, RCF_Contact1),

    autotest:mark(?LINE, "register_contact_params/3 - 3"),
    [RCF_Contact3] = contact:parse(["<sip:ft@192.0.2.123>;+sip.instance=\"<test-instance>\""]),
    RCF_RegReq3 = #reg_request{priority		= 100,
			       path_vector	= []
			      },
    {ok, [{priority, 100}, {registration_time, _}], "<test-instance>"} =
	register_contact_params(RCF_RegReq3, RCF_Contact3),

    autotest:mark(?LINE, "register_contact_params/3 - 4"),
    %% test that we ignore non-quoted instance-ids
    [RCF_Contact4] = contact:parse(["<sip:ft@192.0.2.123>;+sip.instance=test"]),
    RCF_RegReq4 = #reg_request{priority		= 100,
			       path_vector	= []
			      },
    {ok, [{priority, 100}, {registration_time, _}], []} =
	register_contact_params(RCF_RegReq4, RCF_Contact4),

    autotest:mark(?LINE, "register_contact_params/3 - 5"),
    %% test that we get reg_id and socket_id if reg-id and instance is present
    [RCF_Contact5] = contact:parse(["<sip:ft@192.0.2.123>;+sip.instance=\"<test-instance>\";reg-id=1"]),
    ThisNode = node(),
    RCF_RegReq5 = #reg_request{priority		= 100,
			       path_vector	= [],
			       origin		= #siporigin{sipsocket = #sipsocket{id = "socketidRCF5"}}
			      },
    {ok, [{priority, 100}, {reg_id, 1}, {registration_time, _},
	  {socket_id, #locationdb_socketid{node = ThisNode, id = "socketidRCF5"}}], "<test-instance>"} =
	register_contact_params(RCF_RegReq5, RCF_Contact5),

    autotest:mark(?LINE, "register_contact_params/3 - 6"),
    %% test same thing but with old draft-Outbound semantics (flow-id instead of reg-id)
    [RCF_Contact6] = contact:parse(["<sip:ft@192.0.2.123>;+sip.instance=\"<test-instance>\";flow-id=12"]),
    {ok, [{priority, 100}, {reg_id, 12}, {registration_time, _},
	  {socket_id, #locationdb_socketid{node = ThisNode, id = "socketidRCF5"}}], "<test-instance>"} =
	register_contact_params(RCF_RegReq5, RCF_Contact6),



    %% is_valid_register_request(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_valid_register_request/1 - 1"),
    %% test without Require
    true = is_valid_register_request(keylist:from_list([])),

    autotest:mark(?LINE, "is_valid_register_request/1 - 2"),
    %% test with "path" required
    true = is_valid_register_request( keylist:from_list([{"Require", ["path"]}]) ),

    autotest:mark(?LINE, "is_valid_register_request/1 - 3"),
    %% test with unknown extensions required
    {siperror, 420, "Bad Extension", [{"Unsupported", ["unknown-ext1", "unknown-ext2"]}]} =
	is_valid_register_request( keylist:from_list([{"Require", ["unknown-ext1", "unknown-ext2"]}]) ),


    %% get_unsupported_extensions2(In, DoGRUU, [])
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_unsupported_extensions2/3 D- 1"),
    [] = get_unsupported_extensions2(["gruu", "path"], true, []),

    autotest:mark(?LINE, "get_unsupported_extensions2/3 - 2"),
    ["gruu"] = get_unsupported_extensions2(["gruu", "path"], false, []),


    %% process_updates_get_path_vector2(Proto, Header, AppName, IgnoreSupported)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 0"),
    PUGPV_RRStr1 = siprequest:construct_record_route("sip"),
    PUGPV_RRStr2 = siprequest:construct_record_route("sips"),
    PUGPV_NoPathHeader = keylist:from_list([]),
    PUGPV_PathHeader = keylist:from_list([ {"Path", ["<sip:edge-proxy.example.com;lr>"]} ]),
    PUGPV_PathSupportedHeader = keylist:set("Supported", ["path"], PUGPV_PathHeader),
    PUGPV_ExtRequired = {siperror, 421, "Extension Required", [{"Require", ["path"]}]},

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 1"),
    %% test with no Path, not outgoingproxy
    [] = process_updates_get_path_vector2("sip", PUGPV_NoPathHeader, incomingproxy, false),

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 2.1"),
    %% test with no Path, outgoingproxy. Means this proxy should add itself.
    [PUGPV_RRStr1_res] = process_updates_get_path_vector2("sip", PUGPV_NoPathHeader, outgoingproxy, false),

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 2.2"),
    %% verify
    PUGPV_RRStr1 = test_remove_outbound_params(PUGPV_RRStr1_res),

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 3"),
    %% test with path, Supported required
    PUGPV_ExtRequired = process_updates_get_path_vector2("sip", PUGPV_PathHeader, incomingproxy, false),

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 4"),
    %% test with path, Supported NOT required, incomingproxy
    ["<sip:edge-proxy.example.com;lr>"] = process_updates_get_path_vector2("sip", PUGPV_PathHeader, incomingproxy, true),

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 5"),
    %% test with path, Supported NOT required, outgoingproxy
    [PUGPV_RRStr1_res, "<sip:edge-proxy.example.com;lr>"] =
	process_updates_get_path_vector2("sip", PUGPV_PathHeader, outgoingproxy, true),

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 6"),
    %% test with path, Supported required, incomingproxy
    ["<sip:edge-proxy.example.com;lr>"] =
	process_updates_get_path_vector2("sip", PUGPV_PathSupportedHeader, incomingproxy, false),

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 7.1"),
    %% test with path, Supported required, outgoingproxy
    [PUGPV_RRStr2_res, "<sip:edge-proxy.example.com;lr>"] =
	process_updates_get_path_vector2("sips", PUGPV_PathSupportedHeader, outgoingproxy, false),

    autotest:mark(?LINE, "process_updates_get_path_vector2/4 - 7.2"),
    %% verify
    PUGPV_RRStr2 = test_remove_outbound_params(PUGPV_RRStr2_res),

    %% process_updates_get_path_vector(RegReq)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_updates_get_path_vector/1 - 1"),
    %% simple test
    ProcUpd_RegReq1 = #reg_request{uri		= sipurl:parse("sip:ft@192.0.2.22"),
				   header	= keylist:from_list([]),
				   appname	= incomingproxy,
				   path_ignsup	= false
				  },
    [] = process_updates_get_path_vector(ProcUpd_RegReq1),


    %% sort_most_recent_first(Locations)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "sort_most_recent_first/1 - 0"),
    MostRecent_Now = util:timestamp(),
    MostRecent_LMinus0 = #siplocationdb_e{flags = [{registration_time, MostRecent_Now - 0}]},
    MostRecent_LMinus5 = #siplocationdb_e{flags = [{registration_time, MostRecent_Now - 5}]},
    MostRecent_LMinus50 = #siplocationdb_e{flags = [{registration_time, MostRecent_Now - 50}]},
    MostRecent_LMissing = #siplocationdb_e{flags = []},

    autotest:mark(?LINE, "sort_most_recent_first/1 - 1"),
    [MostRecent_LMinus0, MostRecent_LMinus5] = sort_most_recent_first([MostRecent_LMinus5, MostRecent_LMinus0]),

    autotest:mark(?LINE, "sort_most_recent_first/1 - 2"),
    [MostRecent_LMinus0, MostRecent_LMinus5, MostRecent_LMinus50, MostRecent_LMissing] =
	sort_most_recent_first([MostRecent_LMissing, MostRecent_LMinus5, MostRecent_LMinus50, MostRecent_LMinus0]),

    autotest:mark(?LINE, "sort_most_recent_first/1 - 3"),
    [MostRecent_LMinus0, MostRecent_LMissing] = sort_most_recent_first([MostRecent_LMissing, MostRecent_LMinus0]),

    autotest:mark(?LINE, "sort_most_recent_first/1 - 4"),
    [MostRecent_LMinus0, MostRecent_LMissing] = sort_most_recent_first([MostRecent_LMinus0, MostRecent_LMissing]),


    %% Mnesia dependant tests
    %%--------------------------------------------------------------------


    autotest:mark(?LINE, "Mnesia setup - 0"),

    phone:test_create_table(),
    database_gruu:test_create_table(),

    case mnesia:transaction(fun test_mnesia_dependant_functions/0) of
	{aborted, ok} ->
	    ok;
	{aborted, Res} ->
	    {error, Res}
    end.

test_mnesia_dependant_functions() ->

    %% register_contact(RegReq, Contact, Expire)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "register_contact/3 - 0"),
    Register_Contact1_Now = util:timestamp(),
    Register_Contact1_Expire_In = 3610,	%% use value not likely to be outside configured bounds
    Register_Contact1_Expire = Register_Contact1_Now + Register_Contact1_Expire_In,
    [Register_Contact1] = contact:parse(["<sip:user@192.0.2.11>;expires=" ++
					 integer_to_list(Register_Contact1_Expire_In)
					]),
    Register_Contact1_URL = sipurl:parse(Register_Contact1#contact.urlstr),

    [Register_Contact2] = contact:parse(["<sips:user@192.0.2.12>;expires=" ++
					 integer_to_list(Register_Contact1_Expire_In)
					]),
    Register_Contact2_URL = sipurl:parse(Register_Contact2#contact.urlstr),

    TestInstanceId1 = "<test:__unit_testing_instance_id_RC3__>",

    [Register_Contact3] = contact:parse(["<sips:user@192.0.2.13>;"
					 "+sip.instance=\"" ++ TestInstanceId1 ++ "\";expires=" ++
					 integer_to_list(Register_Contact1_Expire_In)
					]),
    Register_Contact3_URL = sipurl:parse(Register_Contact3#contact.urlstr),

    TestUser1 = "__unit-test__user1__",
    TestUser2 = "__unit-test__user2__",

    RC_RegReq1 =
	#reg_request{logtag = "testing",
		     sipuser = TestUser1,
		     priority = 100,
		     expire_h = [],
		     callid  = "call-id123",
		     cseq    = 1,
		     path_vector = [],
		     origin = #siporigin{sipsocket = #sipsocket{}}
		    },

    autotest:mark(?LINE, "register_contact/3 - 1.1"),
    %% test regular registration
    {atomic, ok} = register_contact(RC_RegReq1, Register_Contact1, Register_Contact1_Expire_In),

    autotest:mark(?LINE, "register_contact/3 - 1.2"),
    %% verify result
    [#siplocationdb_e{address = Register_Contact1_URL,
		      flags   = Register_Contact1_Flags,
		      class   = dynamic,
		      expire  = Register_Contact1_Expire1
		     }] = get_locations_for_users([TestUser1]),

    {expire, true} = {expire, (Register_Contact1_Expire1 >= Register_Contact1_Expire -1 andalso
			       Register_Contact1_Expire1 =< Register_Contact1_Expire +1)},

    autotest:mark(?LINE, "register_contact/3 - 1.3"),
    [{priority, 100}, {registration_time, _}] = lists:sort(Register_Contact1_Flags),

    autotest:mark(?LINE, "register_contact/3 - 2.1"),
    %% test registration with path vector
    Register_Contact2_Path = ["sip:edge-proxy.example.com;lr", "sip:foo.example.net;lr"],
    RC_RegReq2 = RC_RegReq1#reg_request{path_vector	= Register_Contact2_Path,
					sipuser		= TestUser2
				       },
    {atomic, ok} = register_contact(RC_RegReq2, Register_Contact2, Register_Contact1_Expire_In),

    autotest:mark(?LINE, "register_contact/3 - 2.2"),
    [#siplocationdb_e{address = Register_Contact2_URL,
		      flags   = Register_Contact2_Flags}] = get_locations_for_users([TestUser2]),

    autotest:mark(?LINE, "register_contact/3 - 2.3"),
    %% verify resulting flags
    [{path, Register_Contact2_Path}, {priority, 100}, {registration_time, _}] = Register_Contact2_Flags,

    autotest:mark(?LINE, "register_contact/3 - 3.1"),
    %% test registration with instance ID, same user as in previous test
    RC_RegReq3 = RC_RegReq1#reg_request{callid  = "call-id345",
					sipuser = TestUser2
				       },
    {atomic, ok} = register_contact(RC_RegReq3, Register_Contact3, Register_Contact1_Expire_In),

    autotest:mark(?LINE, "register_contact/3 - 3.2"),
    [#siplocationdb_e{address = Register_Contact2_URL},
     #siplocationdb_e{address = Register_Contact3_URL,
		      flags   = Register_Contact3_Flags,
		      instance = TestInstanceId1
		     }] = lists:sort( get_locations_for_users([TestUser2])),

    autotest:mark(?LINE, "register_contact/3 - 3.3"),
    %% verify resulting flags
    [{priority,			100},
     {registration_time,	_}
    ] = Register_Contact3_Flags,


    %% get_user_with_contact(URI)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "get_user_with_contact/1 - 1"),
    %% fetch entrys from the last test
    TestUser1 = get_user_with_contact(Register_Contact1_URL),

    autotest:mark(?LINE, "get_user_with_contact/1 - 2 (disabled)"),
    %% fetch same user, but this time with SIPS URI (stored as SIP, should match SIPS as well)
    %TestUser1 = get_user_with_contact(sipurl:set([{proto, "sips"}], Register_Contact1_URL)),

    autotest:mark(?LINE, "get_user_with_contact/1 - 3"),
    %% test no matching contact
    none = get_user_with_contact(sipurl:parse("sip:__unit_test__3k4uhtihAKJFEt@example.org")),


    %% fetch_contacts(SipUser, Do_GRUU, DoOutbound, To)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "fetch_contacts/3 - 1.1"),
    %% fetch first user from register_contact tests above, only one location
    {ok, false, FetchContacts1}	= fetch_contacts(TestUser1, true, false, ["<sip:ft@example.net>"]),
    ok = test_verify_contacts(3600, 3610, ["<sip:user@192.0.2.11>"], FetchContacts1),

    autotest:mark(?LINE, "fetch_contacts/3 - 1.2"),
    %% verify resulting contact

    autotest:mark(?LINE, "fetch_contacts/3 - 2.1"),
    FetchContacts2_To = ["<sip:ft@example.net>"],
    %% fetch other users contacts, one should have a GRUU
    {ok, true, FetchContacts2} = fetch_contacts(TestUser2, true, false, FetchContacts2_To),

    autotest:mark(?LINE, "fetch_contacts/3 - 2.2"),
    %% verify the contacts
    {ok, [FetchContacts2_GRUU]} = database_gruu:fetch_using_user_instance(TestUser2, TestInstanceId1),
    FetchContacts2_GRUU_URL = gruu:make_url(TestUser2, TestInstanceId1,
					    FetchContacts2_GRUU, FetchContacts2_To),
    ok = test_verify_contacts(3600, 3610, ["<sips:user@192.0.2.12>",
					  "<sips:user@192.0.2.13>;gruu=\"" ++
					   sipurl:print(FetchContacts2_GRUU_URL) ++ "\""
					   ";+sip.instance=\"" ++ TestInstanceId1 ++ "\""
					  ], FetchContacts2),


    %% process_register_request(Request, THandler, LogTag, LogStr, AppName)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_register_request/3 - 1"),
    Test_THandler = transactionlayer:test_get_thandler_self(),
    %% test non-homedomain
    PRR_Request1 = #request{method = "REGISTER",
			    uri    = sipurl:parse("sip:ft@something.not-local.test.example.org"),
			    header = keylist:from_list([{"To", ["<sip:ft@example.org>"]}])
			   },
    PRR_YxaCtx1 = #yxa_ctx{origin	= #siporigin{sipsocket = #sipsocket{}},
			   thandler	= Test_THandler,
			   logstr	= "test logstring",
			   app_logtag	= "test logtag"
			  },
    not_homedomain = process_register_request(PRR_Request1, PRR_YxaCtx1, test),


    %% register_require_supported(RegReq, OrigLogTag)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "register_require_supported/2 - 1"),
    %% test with unsupported extensions required
    RRSup_RegReq1 = #reg_request{uri		= sipurl:parse("sip:ft@example.org"),
				 header		= keylist:from_list([{"To", ["<sip:ft@example.org>"]},
								     {"Require", ["X-unsupported"]}
								    ]),
				 thandler	= Test_THandler
				},

    register_require_supported(RRSup_RegReq1, "foo"),
    %% verify result
    receive
	{'$gen_cast', {create_response, 420, "Bad Extension", _RRSup_ExtraHeaders, <<>>}} ->
	    ok;
	RRSup_M1 ->
	    RRSup_Msg1 = io_lib:format("Unknown signal received: ~p", [RRSup_M1]),
	    throw({error, lists:flatten(RRSup_Msg1)})
    after 0 ->
	    throw({error, "Test did not result in the expected create_response signal"})
    end,

    %% process_updates(RegReq, Contacts)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_updates/2 - 0"),
    PU_RegReq1 = #reg_request{uri	= sipurl:parse("sip:ft@example.org"),
			      header	= keylist:from_list([{"To",		["<sip:ft@example.org>"]},
							     {"Expires",	["0"]},
							     {"Call-Id",	["call-id1245667"]},
							     {"CSeq",		["101 REGISTER"]}
							    ]),
			      logtag	= "test logtag",
			      sipuser	= TestUser1,
			      appname	= incomingproxy,
			      do_gruu	= false,
			      do_outbound = true,
			      path_ignsup = false,
			      origin	= #siporigin{sipsocket = #sipsocket{}}
			     },
    PU_Contact1Str = "<sip:ft@192.0.2.10;up=foo>",

    autotest:mark(?LINE, "process_updates/2 - 1"),
    %% test unregister with wildcard first, to make sure we don't have any entrys for
    %% our test user in the database
    PU_Contacts1 = contact:parse(["*"]),
    {ok, {200, "OK", [{"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} = process_updates(PU_RegReq1, PU_Contacts1),

    autotest:mark(?LINE, "process_updates/2 - 2.1"),
    %% simple register
    PU_Contacts2 = contact:parse([PU_Contact1Str ++ ";expires=20"]),
    {ok, {200, "OK", [{"Contact", PU_Contacts2_CRes},
		      {"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} = process_updates(PU_RegReq1, PU_Contacts2),

    autotest:mark(?LINE, "process_updates/2 - 2.2"),
    %% verify contact read back from the database
    test_verify_contacts(15, 20, [PU_Contact1Str], PU_Contacts2_CRes),

    autotest:mark(?LINE, "process_updates/2 - 3"),
    %% test register again with the same call-id but not higher CSeq
    PU_Header3 = keylist:set("CSeq", ["50 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq3 = PU_RegReq1#reg_request{header = PU_Header3},
    {siperror, 403, "Request out of order, contained old CSeq number"}
	= (catch process_updates(PU_RegReq3, PU_Contacts2)),

    autotest:mark(?LINE, "process_updates/2 - 3.5"),
    %% test same thing with wildcard contact
    {siperror, 403, "Request out of order, contained old CSeq number"}
	= (catch process_updates(PU_RegReq3, PU_Contacts1)),

    autotest:mark(?LINE, "process_updates/2 - 4"),
    %% and again, with higher CSeq this time
    [PU_Contacts4_1] = PU_Contacts2,
    PU_Contacts4_2 = contact:rm_param(PU_Contacts4_1, "expires"),
    PU_Contacts4 = [contact:add_param(PU_Contacts4_2, "expires", "40")],
    PU_Header4 = keylist:set("CSeq", ["401 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq4 = PU_RegReq1#reg_request{header = PU_Header4},
    {ok, {200, "OK", [{"Contact",	PU_Contacts4_CRes},
		      {"Date",		_},
		      {"Supported",	["outbound"]}
		     ]}} = process_updates(PU_RegReq4, PU_Contacts4),

    %% verify contact read back from the database
    test_verify_contacts(35, 40, [PU_Contact1Str], PU_Contacts4_CRes),

    autotest:mark(?LINE, "process_updates/2 - 5"),
    %% test without contacts, database readback only (should give exactly the same result as the previous test)
    {ok, {200, "OK", [{"Contact", PU_Contacts5_CRes},
		      {"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} = process_updates(PU_RegReq4, []),

    %% verify contact read back from the database
    test_verify_contacts(35, 40, [PU_Contact1Str], PU_Contacts5_CRes),

    autotest:mark(?LINE, "process_updates/2 - 7"),
    %% unregister the only contact for TestUser1 specifically
    PU_Contacts7 = contact:parse([PU_Contact1Str ++ ";expires=0"]),
    PU_Header7 = keylist:set("CSeq", ["701 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq7 = PU_RegReq1#reg_request{header = PU_Header7},
    {ok, {200, "OK", [{"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} = process_updates(PU_RegReq7, PU_Contacts7),


    autotest:mark(?LINE, "process_updates/2 - 8.1"),
    %% register again
    PU_Contacts8_1 = contact:parse([PU_Contact1Str ++ ";expires=10"]),
    {ok, {200, "OK", [{"Contact", PU_Contacts8_1_CRes},
		      {"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} = process_updates(PU_RegReq1, PU_Contacts8_1),

    test_verify_contacts(5, 10, [PU_Contact1Str], PU_Contacts8_1_CRes),

    autotest:mark(?LINE, "process_updates/2 - 8.2"),
    %% test new register with changed Call-Id, simulating a client that has rebooted
    PU_Contacts8_2 = contact:parse([PU_Contact1Str ++ ";expires=20"]),
    PU_Header8_2 = keylist:set("Call-Id", ["other-call-id19237"], PU_RegReq1#reg_request.header),
    PU_RegReq8_2 = PU_RegReq1#reg_request{header = PU_Header8_2},
    {ok, {200, "OK", [{"Contact", PU_Contacts8_2_CRes},
		      {"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} = process_updates(PU_RegReq8_2, PU_Contacts8_2),

    %% verify binding was updated (longer expire now)
    test_verify_contacts(15, 20, [PU_Contact1Str], PU_Contacts8_2_CRes),

    autotest:mark(?LINE, "process_updates/2 - 8.3"),
    %% unregister the only contact for TestUser1 with different Call-Id once again
    PU_Contacts8_3 = contact:parse([PU_Contact1Str ++ ";expires=0"]),
    PU_Header8_3 = keylist:set("Call-Id", ["yet-another-call-id56622"], PU_RegReq1#reg_request.header),
    PU_RegReq8_3 = PU_RegReq1#reg_request{header = PU_Header8_3},
    {ok, {200, "OK", [{"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} = process_updates(PU_RegReq8_3, PU_Contacts8_3),

    %%
    %% GRUU TESTS
    %%

    autotest:mark(?LINE, "process_updates/2 - 10.1"),
    %% Register with GRUU
    PU_Contacts10_Instance = "<test:__unit_testing_instance_id_PU10__>",
    PU_Contacts10 = contact:parse([PU_Contact1Str ++ ";+sip.instance=\"" ++ PU_Contacts10_Instance ++
				  "\";expires=20"]),
    PU_Header10_1 = keylist:set("CSeq", ["1001 REGISTER"], PU_RegReq1#reg_request.header),
    PU_Header10_2 = keylist:set("Supported", ["gruu"], PU_Header10_1),
    PU_RegReq10 = PU_RegReq1#reg_request{header		= PU_Header10_2,
					 do_gruu	= true
					},

    %% Note: The 'Require: gruu' is no longer required by the GRUU draft (removed in -07)
    %% but we will keep doing it until we are sure that clients don't ignore the GRUUs otherwise
    {ok, {200, "OK", [{"Require",	["gruu"]},
		      {"Contact",	PU_Contacts10_CRes},
		      {"Date",		_},
		      {"Supported",	["outbound"]}
		     ]}} =
	process_updates(PU_RegReq10, PU_Contacts10),

    autotest:mark(?LINE, "process_updates/2 - 10.2"),
    %% verify the contacts
    {ok, [PU_Contacts10_GRUU]} = database_gruu:fetch_using_user_instance(TestUser1, PU_Contacts10_Instance),
    PU_Contacts10_GRUU_URL = gruu:make_url(TestUser1, PU_Contacts10_Instance, PU_Contacts10_GRUU,
					   keylist:fetch('to', PU_RegReq10#reg_request.header)),
    ok = test_verify_contacts(15, 20, [PU_Contact1Str ++ ";gruu=\"" ++
				       sipurl:print(PU_Contacts10_GRUU_URL) ++ "\""
				       ";+sip.instance=\"" ++ PU_Contacts10_Instance ++ "\""
				      ], PU_Contacts10_CRes),


    autotest:mark(?LINE, "process_updates/2 - 11.1"),
    %% test that GRUUs are not included if the client does not indicate support for it (through Supported: gruu)
    PU_Header11 = keylist:set("CSeq", ["1101 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq11 = PU_RegReq1#reg_request{header = PU_Header11},
    {ok, {200, "OK", [{"Contact",	PU_Contacts11_CRes},
		      {"Date",		_},
		      {"Supported",	["outbound"]}
		     ]}} = process_updates(PU_RegReq11, PU_Contacts10),

    autotest:mark(?LINE, "process_updates/2 - 11.2"),
    %% verify the contacts
    ok = test_verify_contacts(15, 20, [PU_Contact1Str], PU_Contacts11_CRes),


    autotest:mark(?LINE, "process_updates/2 - 14"),
    %% unregister all contacts for TestUser1 with a wildcard, same Call-Id and increased CSeq
    PU_Contacts14 = contact:parse(["*"]),
    PU_Header14 = keylist:set("CSeq", ["1401 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq14 = PU_RegReq1#reg_request{header = PU_Header14},
    {ok, {200, "OK", [{"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} =
	process_updates(PU_RegReq14, PU_Contacts14),

    %%
    %% Path TESTS
    %%

    autotest:mark(?LINE, "process_updates/2 - 15"),
    %% test with path inserted by a previous proxy, but no UA support for Path
    PU_Header15_1 = keylist:set("CSeq", ["1501 REGISTER"], PU_RegReq1#reg_request.header),
    PU_Header15_2 = keylist:set("Path", ["<sip:edge.example.org>"], PU_Header15_1),
    PU_RegReq15 = PU_RegReq1#reg_request{header = PU_Header15_2},
    PU_Contacts15 = contact:parse([PU_Contact1Str ++ ";expires=20"]),
    {siperror, 421, "Extension Required", [{"Require", ["path"]}]} = process_updates(PU_RegReq15, PU_Contacts15),


    autotest:mark(?LINE, "process_updates/2 - 16.1"),
    %% test with path inserted by a previous proxy, no UA support for Path but configuration that says we
    %% should store such Path anyways
    PU_RegReq16 = PU_RegReq15#reg_request{path_ignsup = true},
    {ok, {200, "OK", [{"Contact",	PU_Contacts16_CRes},
		      {"Date",		_},
		      {"Supported",	["outbound"]},
		      {"Path",		["<sip:edge.example.org>"]}
		     ]}} = process_updates(PU_RegReq16, PU_Contacts15),

    autotest:mark(?LINE, "process_updates/2 - 16.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(15, 20, [PU_Contact1Str], PU_Contacts16_CRes),

    autotest:mark(?LINE, "process_updates/2 - 16.3"),
    %% verify the record in the location database
    [PU_Contacts16_Loc] = get_locations_for_users([TestUser1]),
    {value, {path, ["<sip:edge.example.org>"]}} = lists:keysearch(path, 1, PU_Contacts16_Loc#siplocationdb_e.flags),


    autotest:mark(?LINE, "process_updates/2 - 17.1"),
    %% test with path inserted by a previous proxy, and one inserted by this proxy (appname is outgoingproxy)
    %% (the one we insert should not be visible in the response)
    PU_Header17_1 = keylist:set("CSeq", ["1701 REGISTER"], PU_RegReq15#reg_request.header),
    PU_Header17_2 = keylist:set("Supported", ["path"], PU_Header17_1),
    PU_RegReq17 = PU_RegReq1#reg_request{uri		= sipurl:parse("sips:example.org"),
					 header		= PU_Header17_2,
					 path_ignsup	= false,
					 appname	= outgoingproxy
					},
    {ok, {200, "OK", [{"Contact",	PU_Contacts17_CRes},
		      {"Date",		_},
		      {"Supported",	["outbound"]},
		      {"Path",		["<sip:edge.example.org>"]}
		     ]}} = process_updates(PU_RegReq17, PU_Contacts15),

    autotest:mark(?LINE, "process_updates/2 - 17.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(15, 20, [PU_Contact1Str], PU_Contacts17_CRes),

    autotest:mark(?LINE, "process_updates/2 - 17.3"),
    %% verify the record in the location database
    [PU_17_me] = process_updates_get_path_vector3("sips", [], outgoingproxy),
    [PU_Contacts17_Loc] = get_locations_for_users([TestUser1]),
    {value, {path, [PU_17_me, "<sip:edge.example.org>"]}} =
	lists:keysearch(path, 1, PU_Contacts17_Loc#siplocationdb_e.flags),

    autotest:mark(?LINE, "process_updates/2 - 17.4"),
    %% wipe clean
    PU_Contacts17_4 = contact:parse(["*"]),
    PU_Header17_4 = keylist:set("CSeq", ["1704 REGISTER"], PU_Header17_2),
    PU_RegReq17_4 = PU_RegReq17#reg_request{header = PU_Header17_4},
    {ok, {200, "OK", [{"Date",		_},
		      {"Supported",	["outbound"]},
		      {"Path",		["<sip:edge.example.org>"]}
		     ]}} = process_updates(PU_RegReq17_4, PU_Contacts17_4),

    %%
    %% END Path TESTS
    %%


    autotest:mark(?LINE, "process_updates/2 - 20.0"),
    %% put some unusual records into the location database
    PU_20_CSeq = keylist:fetch('cseq', PU_RegReq1#reg_request.header),
    PU_20_Expire = util:timestamp() + 20,
    %% record with no priority
    {atomic, ok} = phone:insert_purge_phone(TestUser1, [], dynamic, PU_20_Expire,
					    sipurl:parse("sip:dynamic@192.0.2.12"), PU_20_CSeq, 2000, ""),
    %% static registration
    {atomic, ok} = phone:insert_purge_phone(TestUser1, [], static, never,
					    sipurl:parse("sip:static@example.org"), "", 0, ""),

    autotest:mark(?LINE, "process_updates/2 - 20.1"),
    %% verify that the static registration isn't included in REGISTER responses
    PU_Header20 = keylist:set("CSeq", ["2001 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq20 = PU_RegReq1#reg_request{header		= PU_Header20,
					 do_gruu	= true,
					 path_ignsup	= true
					},
    {ok, {200, "OK", [{"Contact",       PU_Contacts20_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq20, []),

    autotest:mark(?LINE, "process_updates/2 - 20.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(15, 20, ["<sip:dynamic@192.0.2.12>"], PU_Contacts20_CRes),

    autotest:mark(?LINE, "process_updates/2 - 21.1"),
    %% test unregistering with wildcard when there are some 'strange' entrys there
    PU_Contacts21 = contact:parse(["*"]),
    PU_Header21 = keylist:set("CSeq", ["2101 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq21 = PU_RegReq1#reg_request{header	= PU_Header21,
					 do_gruu	= true,
					 path_ignsup	= true,
					 do_outbound	= false
					},
    {ok, {200, "OK", [{"Date", _}
		     ]}} =
	process_updates(PU_RegReq21, PU_Contacts21),

    autotest:mark(?LINE, "process_updates/2 - 21.2"),
    %% delete the static entry too, can't be done through REGISTER message
    {atomic, ok} = phone:delete_location(TestUser1, static, "sip:static@example.org"),

    autotest:mark(?LINE, "process_updates/2 - 21.3"),
    %% verify TestUser1 has no active registrations now
    [] = get_locations_for_users([TestUser1]),



    %% Outbound TESTS

    autotest:mark(?LINE, "process_updates/2 - 25.1"),
    %% test simple fetch with outbound disabled
    {ok, {200, "OK", [{"Date", _}
		     ]}} =
	process_updates(PU_RegReq1#reg_request{do_outbound = false}, []),

    autotest:mark(?LINE, "process_updates/2 - 25.2"),
    %% test simple fetch with outbound enabled
    {ok, {200, "OK", [{"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} =
	process_updates(PU_RegReq1#reg_request{do_outbound = true}, []),

    autotest:mark(?LINE, "process_updates/2 - 26.1"),
    %% test registration with both instance id and reg-id
    PU_Contacts26_Instance = "<test:__unit_testing_instance_id_PU26__>",
    PU_Contacts26_OBstr = ";+sip.instance=\"" ++ PU_Contacts26_Instance ++ "\";reg-id=4711",
    PU_Contacts26 = contact:parse([PU_Contact1Str ++ PU_Contacts26_OBstr ++ ";expires=20"]),
    PU_Header26 = keylist:set("CSeq", ["2601 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq26 = PU_RegReq1#reg_request{header		= PU_Header26,
					 do_gruu	= false,
					 path_ignsup	= true,
					 do_outbound	= true,
					 origin		= #siporigin{sipsocket = #sipsocket{id = "PU_test26_socketid"}},
					 sipuser	= TestUser1
					},
    {ok, {200, "OK", [{"Contact",       PU_Contacts26_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq26, PU_Contacts26),

    autotest:mark(?LINE, "process_updates/2 - 26.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(15, 20, [PU_Contact1Str ++ PU_Contacts26_OBstr], PU_Contacts26_CRes),

    autotest:mark(?LINE, "process_updates/2 - 26.3"),
    %% verify the record in the location database
    [PU_Contacts26_Loc] = get_locations_for_users([TestUser1]),
    #siplocationdb_e{instance = PU_Contacts26_Instance} = PU_Contacts26_Loc,
    {value, {reg_id, 4711}} =
	lists:keysearch(reg_id, 1, PU_Contacts26_Loc#siplocationdb_e.flags),
    {value, {socket_id, #locationdb_socketid{node = ThisNode, id = "PU_test26_socketid"}}} =
	lists:keysearch(socket_id, 1, PU_Contacts26_Loc#siplocationdb_e.flags),

    autotest:mark(?LINE, "process_updates/2 - 27.1"),
    %% test client replacing previous registration with new flow
    PU_Contact27Str = "<sip:new@192.0.2.10:1213>",
    PU_Contacts27_OBstr = ";+sip.instance=\"" ++ PU_Contacts26_Instance ++ "\";reg-id=4711",
    PU_Contacts27 = contact:parse([PU_Contact27Str ++ PU_Contacts27_OBstr ++ ";expires=30"]),
    PU_Header27 = keylist:set("CSeq", ["2701 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq27 = PU_RegReq26#reg_request{header	= PU_Header27,
					  origin	= #siporigin{sipsocket = #sipsocket{id = "PU_test27_socketid"}}
					},
    {ok, {200, "OK", [{"Contact",       PU_Contacts27_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq27, PU_Contacts27),

    autotest:mark(?LINE, "process_updates/2 - 27.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(25, 30, [PU_Contact27Str ++ PU_Contacts27_OBstr], PU_Contacts27_CRes),

    autotest:mark(?LINE, "process_updates/2 - 27.3"),
    %% verify the record in the location database
    [PU_Contacts27_Loc] = get_locations_for_users([TestUser1]),
    #siplocationdb_e{instance = PU_Contacts26_Instance} = PU_Contacts27_Loc,
    {value, {reg_id, 4711}} =
	lists:keysearch(reg_id, 1, PU_Contacts27_Loc#siplocationdb_e.flags),
    {value, {socket_id, #locationdb_socketid{node = ThisNode, id = "PU_test27_socketid"}}} =
	lists:keysearch(socket_id, 1, PU_Contacts27_Loc#siplocationdb_e.flags),


    autotest:mark(?LINE, "process_updates/2 - 28.1"),
    %% test client makes second REGISTER to other node
    PU_Contact28Str = PU_Contact27Str,
    PU_Contacts28_OBstr = ";+sip.instance=\"" ++ PU_Contacts26_Instance ++ "\";reg-id=4712",
    PU_Contacts28 = contact:parse([PU_Contact28Str ++ PU_Contacts28_OBstr ++ ";expires=30"]),
    %% same CSeq (sent to another node, right?) but different Call-Id
    PU_Header28_1 = keylist:set("CSeq", ["2701 REGISTER"], PU_RegReq1#reg_request.header),
    PU_Header28 = keylist:set("Call-Id", ["2345-some-other-call-id"], PU_Header28_1),
    PU_RegReq28 = PU_RegReq27#reg_request{header	= PU_Header28,
					  origin	= #siporigin{sipsocket = #sipsocket{id = "PU_test28_socketid_backup"}}
					},

    {ok, {200, "OK", [{"Contact",       PU_Contacts28_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq28, PU_Contacts28),

    autotest:mark(?LINE, "process_updates/2 - 28.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(25, 30, [PU_Contact27Str ++ PU_Contacts27_OBstr,
				       PU_Contact28Str ++ PU_Contacts28_OBstr], PU_Contacts28_CRes),

    autotest:mark(?LINE, "process_updates/2 - 28.3"),
    %% verify the two records in the location database
    [PU_Contacts27_Loc, PU_Contacts28_Loc] = get_locations_for_users([TestUser1]),
    #siplocationdb_e{instance = PU_Contacts26_Instance} = PU_Contacts28_Loc,
    {value, {reg_id, 4712}} =
	lists:keysearch(reg_id, 1, PU_Contacts28_Loc#siplocationdb_e.flags),
    {value, {socket_id, #locationdb_socketid{node = ThisNode, id = "PU_test28_socketid_backup"}}} =
	lists:keysearch(socket_id, 1, PU_Contacts28_Loc#siplocationdb_e.flags),


    autotest:mark(?LINE, "process_updates/2 - 29.1"),
    %% test client making the next subsequent re-register
    PU_Header29 = keylist:set("CSeq", ["2801 REGISTER"], PU_RegReq28#reg_request.header),
    PU_RegReq29 = PU_RegReq28#reg_request{header = PU_Header29},
    {ok, {200, "OK", [{"Contact",       PU_Contacts29_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq29, PU_Contacts28),

    autotest:mark(?LINE, "process_updates/2 - 29.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(25, 30, [PU_Contact27Str ++ PU_Contacts27_OBstr,
                                       PU_Contact28Str ++ PU_Contacts28_OBstr], PU_Contacts29_CRes),

    %% XXX and test non-Outbound client registering when there are Outbound entrys involved -
    %% can't have same Contact URI as the Outbound registrations though. Bug in draft? Naah.







    autotest:mark(?LINE, "process_updates/2 - 30.1"),
    %% test unregistering TestUser2 with wildcard and outbound enabled
    PU_Contacts30 = contact:parse(["*"]),
    PU_Header30 = keylist:set("CSeq", ["3001 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq30 = PU_RegReq1#reg_request{header	= PU_Header30,
					 do_gruu	= true,
					 path_ignsup	= true,
					 do_outbound	= true,
					 sipuser	= TestUser2
					},
    {ok, {200, "OK", [{"Date", _},
		      {"Supported", ["outbound"]}
		     ]}} =
	process_updates(PU_RegReq30, PU_Contacts30),

    autotest:mark(?LINE, "process_updates/2 - 30.2"),
    %% verify TestUser2 has no active registrations now
    [] = get_locations_for_users([TestUser2]),

    autotest:mark(?LINE, "process_updates/2 - 31.1"),
    %% test registration with invalid (non-numeric) reg-id parameter
    PU_Contacts31_Instance = "<test:__unit_testing_instance_id_PU31__>",
    PU_Contacts31 = contact:parse([PU_Contact1Str ++ ";+sip.instance=\"" ++ PU_Contacts31_Instance ++
				   "\";reg-id=fyrtiosju;expires=20"]),
    PU_Header31 = keylist:set("CSeq", ["3101 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq31 = PU_RegReq1#reg_request{header		= PU_Header31,
					 do_gruu	= false,
					 path_ignsup	= true,
					 do_outbound	= true,
					 origin		= #siporigin{sipsocket = #sipsocket{id = "PU_test31_socketid"}},
					 sipuser	= TestUser2
					},
    {ok, {200, "OK", [{"Contact",       PU_Contacts31_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq31, PU_Contacts31),

    autotest:mark(?LINE, "process_updates/2 - 31.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(15, 20, [PU_Contact1Str], PU_Contacts31_CRes),

    autotest:mark(?LINE, "process_updates/2 - 31.2"),
    %% verify the record in the location database, should NOT have reg-id (or socket_id)
    [PU_Contacts31_Loc] = get_locations_for_users([TestUser2]),
    #siplocationdb_e{instance = PU_Contacts31_Instance} = PU_Contacts31_Loc,
    false = lists:keysearch(reg_id, 1, PU_Contacts31_Loc#siplocationdb_e.flags),
    false = lists:keysearch(socket_id, 1, PU_Contacts31_Loc#siplocationdb_e.flags),

    %% clean up
    phone:delete_phone_for_user(TestUser1, dynamic),
    phone:delete_phone_for_user(TestUser2, dynamic),



    autotest:mark(?LINE, "process_updates/2 - 32.1"),
    %% test two user agents (different SIP users) having the same instance-id
    %% SHOULD not happen, but I've seen it happen at it is important that a user
    %% can't hijack another user's registration just because the evil user knows
    %% the other user's instace-id
    PU_Contacts32_Instance = "<test:__unit_testing_instance_id_PU32__>",
    PU_Contacts32_OBstr = ";+sip.instance=\"" ++ PU_Contacts32_Instance ++ "\";reg-id=4711",
    PU_Contacts32 = contact:parse([PU_Contact1Str ++ PU_Contacts32_OBstr ++ ";expires=20"]),
    PU_Header32 = keylist:set("CSeq", ["3201 REGISTER"], PU_RegReq1#reg_request.header),
    PU_RegReq32 = PU_RegReq1#reg_request{header		= PU_Header32,
					 do_gruu	= false,
					 path_ignsup	= true,
					 do_outbound	= true,
					 origin		= #siporigin{sipsocket = #sipsocket{id = "PU_test32_socketid"}},
					 sipuser	= TestUser1
					},
    {ok, {200, "OK", [{"Contact",       PU_Contacts32_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq32, PU_Contacts32),

    autotest:mark(?LINE, "process_updates/2 - 32.2"),
    %% verify the contacts in the response
    ok = test_verify_contacts(15, 20, [PU_Contact1Str ++ PU_Contacts32_OBstr], PU_Contacts32_CRes),

    autotest:mark(?LINE, "process_updates/2 - 32.3"),
    %% verify the record in the location database
    [PU_Contacts32_Loc] = get_locations_for_users([TestUser1]),
    #siplocationdb_e{instance = PU_Contacts32_Instance} = PU_Contacts32_Loc,
    {value, {reg_id, 4711}} =
	lists:keysearch(reg_id, 1, PU_Contacts32_Loc#siplocationdb_e.flags),
    {value, {socket_id, #locationdb_socketid{node = ThisNode, id = "PU_test32_socketid"}}} =
	lists:keysearch(socket_id, 1, PU_Contacts32_Loc#siplocationdb_e.flags),
    TestUser1 = PU_Contacts32_Loc#siplocationdb_e.sipuser,
    PU_Contacts32_URL1 = sipurl:parse((hd(PU_Contacts32))#contact.urlstr),
    PU_Contacts32_URL1 = PU_Contacts32_Loc#siplocationdb_e.address,

    autotest:mark(?LINE, "process_updates/2 - 32.4"),
    %% verify nothing in database for second user at this point
    [] = get_locations_for_users([TestUser2]),

    autotest:mark(?LINE, "process_updates/2 - 32.5"),
    %% second user registers with same instance-id and reg-id
    PU_Contact32bStr = "<sip:ft@192.0.2.32>",
    PU_Contacts32b = contact:parse([PU_Contact32bStr ++ PU_Contacts32_OBstr ++ ";expires=20"]),
    PU_RegReq32b = PU_RegReq32#reg_request{sipuser	= TestUser2},
    {ok, {200, "OK", [{"Contact",       PU_Contacts32b_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq32b, PU_Contacts32b),


    autotest:mark(?LINE, "process_updates/2 - 32.6"),
    %% verify the contacts in the response
    ok = test_verify_contacts(15, 20, [PU_Contact32bStr ++ PU_Contacts32_OBstr], PU_Contacts32b_CRes),

    autotest:mark(?LINE, "process_updates/2 - 32.7"),
    %% verify the record in the location database - first the one present from before
    [PU_Contacts32_Loc] = get_locations_for_users([TestUser1]),
    %% and then this new one
    [PU_Contacts32b_Loc] = get_locations_for_users([TestUser2]),
    #siplocationdb_e{instance = PU_Contacts32_Instance} = PU_Contacts32b_Loc,
    {value, {reg_id, 4711}} =
	lists:keysearch(reg_id, 1, PU_Contacts32b_Loc#siplocationdb_e.flags),
    {value, {socket_id, #locationdb_socketid{node = ThisNode, id = "PU_test32_socketid"}}} =
	lists:keysearch(socket_id, 1, PU_Contacts32b_Loc#siplocationdb_e.flags),
    TestUser2 = PU_Contacts32b_Loc#siplocationdb_e.sipuser,
    PU_Contacts32b_URL1 = sipurl:parse((hd(PU_Contacts32b))#contact.urlstr),
    PU_Contacts32b_URL1 = PU_Contacts32b_Loc#siplocationdb_e.address,



    %% clean up
    phone:delete_phone_for_user(TestUser1, dynamic),
    phone:delete_phone_for_user(TestUser2, dynamic),

    %% END Outbound TESTS

    autotest:mark(?LINE, "process_updates/2 - 50"),
    %% test wildcard with invalid expires, actually testing that siperrors thrown deep
    %% down are handled correctly (we've had bugs in that handling, turning 400 into 500)

    PU_Contacts50 = contact:parse(["*"]),
    PU_Header50_1 = keylist:set("CSeq", ["5001 REGISTER"], PU_RegReq1#reg_request.header),
    PU_Header50 = keylist:set("Expires", ["1"], PU_Header50_1),
    PU_RegReq50 = PU_RegReq1#reg_request{header	= PU_Header50
					},
    {siperror, 400, "Wildcard with non-zero contact expires parameter"} =
	process_updates(PU_RegReq50, PU_Contacts50),

    autotest:mark(?LINE, "process_updates/2 - 51"),
    %% Test REGISTER with multiple identical contacs to make sure we don't fail because the
    %% second one has same Call-Id and CSeq as the first one. Corner case not covered by
    %% RFC3261, but actually happened (because of bug in other implementation) at SIPit 18.
    PU_Contact51Str = "<sip:user@example.net>",
    PU_Contacts51 = contact:parse([PU_Contact51Str, PU_Contact51Str]),
    PU_Header51_1 = keylist:set("CSeq", ["5101 REGISTER"], PU_RegReq1#reg_request.header),
    PU_Header51 = keylist:set("Expires", ["20"], PU_Header51_1),
    PU_RegReq51 = PU_RegReq1#reg_request{header	 = PU_Header51,
					 sipuser = TestUser2
					},
    {ok, {200, "OK", [{"Contact",       PU_Contacts51_CRes},
                      {"Date",          _},
		      {"Supported",	["outbound"]}
                     ]}} = process_updates(PU_RegReq51, PU_Contacts51),

    autotest:mark(?LINE, "process_updates/2 - 51.1"),
    %% verify the contacts in the response
    ok = test_verify_contacts(15, 20, [PU_Contact51Str], PU_Contacts51_CRes),

    mnesia:abort(ok).


test_verify_contacts(ExpiresMin, ExpiresMax, ExpectList, Got) when is_integer(ExpiresMin), is_integer(ExpiresMax) ->
    test_verify_contacts2(ExpiresMin, ExpiresMax, ExpectList, contact:parse( lists:sort(Got) )).

test_verify_contacts2(ExpiresMin, ExpiresMax, [ExpectH | ExpectT], [GotH | GotT])
  when is_integer(ExpiresMin), is_integer(ExpiresMax), is_list(ExpectH), is_record(GotH, contact) ->
    case contact_param:find(GotH#contact.contact_param, "expires") of
	[ExpiresStr] ->
	    Expires = list_to_integer(ExpiresStr),
	    case (Expires >= ExpiresMin andalso Expires =< ExpiresMax) of
		true ->
		    This = contact:rm_param(GotH, "expires"),
		    ExpectH_flat = lists:flatten(ExpectH),
		    case contact:print(This) of
			ExpectH_flat ->
			    %% match, test next
			    test_verify_contacts2(ExpiresMin, ExpiresMax, ExpectT, GotT);
			Other ->
			    io:format("Contact (with expires parameter removed) :~n~p~n"
				      "does not match the expected :~n~p~n~n~n", [Other, ExpectH]),

			    {error, contact_mismatch}
		    end;
		false ->
		    io:format("Contact ~p expires out of bounds (~p..~p)",
			      [contact:print(GotH), ExpiresMin, ExpiresMax]),
		    {error, contact_expires_out_of_bounds}
	    end;
	[] ->
	    Msg = io_lib:format("Contact ~p missing expires parameter", [contact:print(GotH)]),
	    io:format(Msg),
	    {error, lists:flatten(Msg)}
    end;
test_verify_contacts2(_ExpiresMin, _ExpiresMax, [], []) ->
    ok.


%% XXX make test case where we register the _same requristr_ from two different proxys.
%% Might happen with outbound.

test_remove_outbound_params(In) ->
    [C] = contact:parse([In]),
    URL1 = sipurl:parse(C#contact.urlstr),
    case url_param:find(URL1#sipurl.param_pairs, "ob") of
	[none] -> ok;
	[] ->
	    Msg = io_lib:format("No ;ob parameter in Path header value '~s'", [In]),
	    throw(lists:flatten(Msg));
	_ ->
	    Msg = io_lib:format("Unknown ;ob parameter in Path header value '~s'", [In]),
	    throw(lists:flatten(Msg))
    end,
    NewParams = url_param:remove(URL1#sipurl.param_pairs, "ob"),
    NewURL = sipurl:set([{param, NewParams}], URL1),
    contact:print( contact:new(NewURL) ).
