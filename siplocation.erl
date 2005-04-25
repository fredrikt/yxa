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
	 process_register_request/5,
	 prioritize_locations/1,
	 get_locations_for_users/1,
	 get_user_with_contact/1,
	 to_url/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("phone.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: process_register_request(Request, THandler, LogTag,
%%                                    LogStr, AppName)
%%           Request  = response record()
%%           THandler = term(), server transaction handler
%%           LogTag   = string(), tag for log messages
%%           LogStr   = string(), description of request
%%           AppName  = atom(), application name
%% Descrip.: Process a received REGISTER. First check if it is for
%%           one of our domains (homedomain), then check that it
%%           contains proper authentication and that the authenticated
%%           user is allowed to register using this address-of-record.
%%           Finally, let process_updates() process all the
%%           Contact headers and update the location database.
%% Returns : not_homedomain | void()
%%--------------------------------------------------------------------
process_register_request(Request, THandler, LogTag, LogStr, AppName) when is_record(Request, request),
									  is_list(LogStr), is_list(LogTag),
									  is_atom(AppName) ->
    URL = Request#request.uri,
    logger:log(debug, "~p: REGISTER ~p", [AppName, sipurl:print(URL)]),
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 1
    %% check if this registrar handles the domain the request wants to register for
    case local:homedomain(URL#sipurl.host) of
	true ->
	    register_require_supported(Request, LogStr, THandler, LogTag, AppName);
	false ->
	    %% act as proxy and forward message to other domain
	    logger:log(debug, "~p: REGISTER for non-homedomain ~p", [AppName, URL#sipurl.host]),
	    not_homedomain
    end.

%%--------------------------------------------------------------------
%% Function: register_require_supported(Request, LogStr, THandler,
%%                                      LogTag, AppName)
%%           Request  = request record()
%%           LogStr   = string(), describes REGISTER request
%%           THandler = thandler record(), server transaction handler
%%           LogTag   = string(), prefix for logging
%%           AppName  = atom(), incomingproxy | outgoingproxy
%% Descrip.: After we have checked that the REGISTER request is for
%%           one of our homedomains, we start checking the validity of
%%           the request. First, we do some checks in this function
%%           before going on to making sure the request is
%%           authenticated in register_authenticate(...).
%% Returns :
%%--------------------------------------------------------------------
register_require_supported(Request, LogStr, THandler, LogTag, AppName) ->
    Header = Request#request.header,
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 2
    case is_valid_register_request(Header) of
	true ->
	    register_authenticate(Request, LogStr, THandler, LogTag, AppName);
	{siperror, Status, Reason, ExtraHeaders} ->
	    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders)
    end.

%% part of process_register/6
register_authenticate(Request, LogStr, THandler, LogTag, AppName) ->
    Header = Request#request.header,
    logger:log(debug, "~p: ~s -> processing", [AppName, LogStr]),
    %% delete any present Record-Route header (RFC3261, #10.3)
    NewHeader = keylist:delete("Record-Route", Header),
    {_, ToURL} = sipheader:to(Header),
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 3, step 4 and step 5
    %% authenticate UAC
    case local:can_register(NewHeader, ToURL) of
	{{true, _}, SIPuser} ->
	    Contacts = sipheader:contact(Header),
	    logger:log(debug, "~s: user ~p, registering contact(s) : ~p",
		       [LogTag, SIPuser, sipheader:contact_print(Contacts)]),
	    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 6, step 7 and step 8
	    case catch process_updates(lists:concat([LogTag, ": ", AppName]),
				       NewHeader, SIPuser, Contacts, AppName) of
		{ok, {Status, Reason, ExtraHeaders}} ->
		    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders),
		    %% Make event about user sucessfully registered
		    L = [{register, ok}, {user, SIPuser},
			 {contacts, sipheader:contact_print(Contacts)}],
		    event_handler:generic_event(normal, location, LogTag, L),
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
	    logger:log(normal, "~s: ~p: SipUser ~p NOT ALLOWED to REGISTER address ~s",
		       [LogTag, AppName, SipUser, sipurl:print(ToURL)]),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden"),
	    %% Make event about users failure to register
	    L = [{register, forbidden}, {user, SipUser}, {address, sipurl:print(ToURL)}],
	    event_handler:generic_event(normal, location, LogTag, L);
	{{false, nomatch}, SipUser} when SipUser /= none ->
	    logger:log(normal, "~s: ~p: SipUser ~p tried to REGISTER invalid address ~s",
		       [LogTag, AppName, SipUser, sipurl:print(ToURL)]),
	    transactionlayer:send_response_handler(THandler, 404, "Not Found"),
	    %% Make event about users failure to register
	    L = [{register, invalid_address}, {user, SipUser}, {address, sipurl:print(ToURL)}],
	    event_handler:generic_event(normal, location, LogTag, L);
	{false, none} ->
	    Prio = case keylist:fetch('authorization', Header) of
		       [] -> debug;
		       _ -> normal
		   end,
	    %% XXX send new challenge (current behavior) or send 403 Forbidden when authentication fails?
	    logger:log(Prio, "~s -> Authentication FAILED, sending challenge", [LogStr]),
	    transactionlayer:send_challenge(THandler, www, false, 3)
    end.

%%--------------------------------------------------------------------
%% Function: is_valid_register_request(Header)
%%           Header = keylist record()
%% Descrip.: looks for unsupported extensions.
%% Returns : true | SipError
%%--------------------------------------------------------------------
is_valid_register_request(Header) ->
    case keylist:fetch('require', Header) of
	[] ->
	    true;
	Require ->
	    %% we support no extensions, so all are unsupported
	    logger:log(normal, "Request check: The client requires unsupported extension(s) ~p", [Require]),
	    {siperror, 420, "Bad Extension", [{"Unsupported", Require}]}
    end.

%%--------------------------------------------------------------------
%% Function: process_updates(LogTag, Header, SipUser, Contacts,
%%                           AppName)
%%           LogTag   = string(), logging prefix
%%           Header   = keylist record(), REGISTER request header
%%           SipUser  = SIP authentication username (#phone.number
%%                      field entry, when using mnesia userdb)
%%           Contacts = list() of contact record()
%%           AppName  = atom(), incomingproxy | outgoingproxy
%% Descrip.: Update the location database, based on a REGISTER request
%%           we are processing. Either add or remove entrys.
%% Returns : {ok, {Status, Reason, ExtraHeaders}}
%%           {siperror, Status, Reason}
%%           {siperror, Status, Reason, ExtraHeaders}
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           ExtraHeaders = list() of {Key, NewValueList} see
%%           keylist:appendlist/2
%%--------------------------------------------------------------------
%% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 6 and 7
%% remove, add or update contact info in location (phone) database

%% REGISTER request had no contact header
process_updates(_LogTag, _Header, SipUser, [], _AppName) ->
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 8
    {ok, create_process_updates_response(SipUser)};

process_updates(LogTag, Header, SipUser, Contacts, AppName) ->
    %% Processing REGISTER Request - step 6
    %% check for and process wildcard (request contact = *)
    case process_register_wildcard_isauth(LogTag, Header, SipUser, Contacts) of
	none ->
	    %% Processing REGISTER Request - step 7
	    %% No wildcard found, register/update/remove entries in Contacts.
	    %% Process registration atomicly - change all or nothing in database.
	    F = fun() ->
			process_non_wildcard_contacts(LogTag, SipUser, Contacts, Header, AppName)
		end,
	    case mnesia:transaction(F) of
		{aborted, Reason} ->
		    logger:log(error, "Location database: REGISTER request failed to add/update/remove one "
			       "or more contacts for user ~p, failed due to: ~n~p", [SipUser, Reason]),
		    %% Check if it was a siperror, otherwise return '500 Server Internal Error'
		    case Reason of
			{throw, {siperror, Status, Reason2}} ->
			    {siperror, Status, Reason2};
			_ ->
			    {siperror, 500, "Server Internal Error", []}
		    end;
		{atomic, _ResultOfFun} ->
		    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 8
		    {ok, create_process_updates_response(SipUser)}
	    end;
	ok ->
	    %% wildcard found and processed
	    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 8
	    {ok, create_process_updates_response(SipUser)};
	SipError ->
	    SipError
    end.

%% part of process_updates/4. Returns {200, "OK", []} | {200, "OK", ExtraHeaders}
create_process_updates_response(SipUser) ->
    case fetch_contacts(SipUser) of
	[] ->
	    {200, "OK", []};
	NewContacts when is_list(NewContacts) ->
	    {200, "OK", [{"Contact", NewContacts}]}
    end.


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Function: fetch_contacts(SipUser)
%% Descrip.: find all the locations where a specific sipuser can be
%%           located (e.g. all the users phones)
%% Returns : none | list of string()
%%           string() is formated as a contact field-value
%%           (see RFC 3261)
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fetch_contacts(SipUser) ->
    case phone:get_sipuser_locations(SipUser) of
	{ok, Locations} ->
	    locations_to_contacts(Locations);
	_ ->
	    none
    end.

%% return: list() of string() (contact:print/1 of contact record())
locations_to_contacts(Locations) ->
    locations_to_contacts2(Locations, util:timestamp(), []).

locations_to_contacts2([], _Now, Res) ->
    Res;
locations_to_contacts2([#siplocationdb_e{expire = never} | T], Now, Res) ->
    %% Don't include static contacts which never expire
    locations_to_contacts2(T, Now, Res);
locations_to_contacts2([H | T], Now, Res) when is_record(H, siplocationdb_e), is_integer(H#siplocationdb_e.expire) ->
    Location = H#siplocationdb_e.address,
    Expire = H#siplocationdb_e.expire,

    %% Expires can't be less than 0 so make sure we don't end up with a negative Expires
    NewExpire = lists:max([0, Expire - Now]),
    Contact = contact:new(Location, [{"expires", integer_to_list(NewExpire)}]),

    locations_to_contacts2(T, Now, [contact:print(Contact) | Res]).


%% return = ok       | wildcard processed
%%          none     | no wildcard found
%%          SipError   db error
%% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 6
process_register_wildcard_isauth(LogTag, Header, SipUser, Contacts) ->
    case is_valid_wildcard_request(Header, Contacts) of
	true ->
	    logger:log(debug, "Location: Processing valid wildcard un-register"),
	    case phone:get_sipuser_locations(SipUser) of
		{ok, SipUserLocations} ->
		    unregister(LogTag, Header, SipUserLocations);
		_ ->
		    none
	    end;
	_ ->
	    none
    end.

%% return = ok       | wildcard processed
%%          SipError   db error
%% unregister all location entries for a sipuser
unregister(LogTag, Header, Locations) ->
    %% unregister all Locations entries
    F = fun() ->
		unregister_contacts(LogTag, Header, Locations)
	end,
    %% process unregistration atomically - change all or nothing in database
    case mnesia:transaction(F) of
	{aborted, Reason} ->
	    logger:log(error, "Database: unregister of registrations failed for one or more"
		       " contact entries, due to: ~p",
		       [Reason]),
	    {siperror, 500, "Server Internal Error"};
	{atomic, _ResultOfFun} ->
	    ok
    end.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Function: is_valid_wildcard_request(Header, Contacts)
%%           Header      = keylist record(), the sip request headers
%%           Contacts    = list() of contact record()
%% Descrip.: determine if request is a properly formed wildcard
%%           request (see RFC 3261 Chapter 10 page 64 - step 6)
%% Returns : true     |
%%           false    |
%%           throw({siperror, ...})
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% there is only one Contact and it's a wildcard
is_valid_wildcard_request(Header, [ #contact{urlstr = "*"} ]) ->
    case sipheader:expires(Header) of
	[Expire] ->
	    %% cast to integer so that "0", "00" ... and so on are all considered as 0
	    case catch list_to_integer(Expire) of
		0 ->
		    true;
		_ ->
		    {siperror, 400, "Wildcard with non-zero contact expires parameter, invalid (RFC3261 10.2.2)"}
	    end;
	[] ->
	    {siperror, 400, "Wildcard without Expires header, invalid (RFC3261 10.2.2)"};
	_ ->
	    {siperror, 400, "Wildcard with non-zero contact expires parameter, invalid (RFC3261 10.2.2)"}
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
%% Function: get_user_with_contact(URI)
%%           URI = sipurl record()
%% Descrip.: Checks if any of our users are registered at the
%%           location specified. Used to determine if we should
%%           proxy requests to a URI without authorization.
%% Returns : none | SIPuser
%%           SIPuser = #phone.number field value
%%--------------------------------------------------------------------
get_user_with_contact(URI) when is_record(URI, sipurl) ->
    case phone:get_phone_with_requri(URI) of
	%% XXX can there be more than 1 matching SIPUser
	%% - yes, should a list() of SIPuser be returned instead ?
	{atomic, [SIPuser | _]} ->
	    SIPuser;
	%% no one using URI found
	{atomic, []} ->
	    %% XXX test this
	    none
    end.

%%--------------------------------------------------------------------
%% Function: get_locations_for_users(SipUserList)
%%           SipUserList = list() of string(), usernames
%% Descrip.: Looks up all locations for a list of users. Used
%%           to find out where a set of users are to see where
%%           we should route a request.
%% Returns : list() of siplocationdb_e record()
%%--------------------------------------------------------------------
get_locations_for_users(In) when is_list(In) ->
    get_locations_for_users2(In, []).

get_locations_for_users2([], Res) ->
    Res;
get_locations_for_users2([H | T], Res) when is_list(H) ->
    {ok, Locations} = phone:get_sipuser_locations(H),
    get_locations_for_users2(T, Res ++ Locations).

%%--------------------------------------------------------------------
%% Function: prioritize_locations(Locations)
%%           Locations = list of siplocationdb_e record()
%% Descrip.: Look through a list of siplocation DB entrys, figure out
%%           what the lowest priority amongst them are and then
%%           return all records which has that priority.
%% Returns : list of siplocationdb_e record()
%%--------------------------------------------------------------------
prioritize_locations(Locations) when is_list(Locations) ->
    case get_priorities(Locations) of
	[BestPrio | _] ->
	    get_locations_with_prio(BestPrio, Locations);
	_ ->
	    %% No locations or no locations with priority - can't choose
	    []
    end.

%% Descrip. = examine all Flags entries in Locations and return all
%%            priority values (if any are given) sorted with lowest (best)
%%            priority first.
%% Returns  = list of integer()
get_priorities(Locations) when is_list(Locations) ->
    get_priorities2(Locations, []).

get_priorities2([#siplocationdb_e{flags=Flags} | T], Res) ->
    case lists:keysearch(priority, 1, Flags) of
	{value, {priority, Prio}} when Prio /= [] ->
	    get_priorities2(T, [Prio | Res]);
	_ ->
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
get_locations_with_prio2(Priority, [#siplocationdb_e{flags=Flags}=H | T], Res) ->
    Prio = lists:keysearch(priority, 1, Flags),
    case Prio of
	{value, {priority, Priority}} ->
	    get_locations_with_prio2(Priority, T, [H | Res]);
	_ ->
	    get_locations_with_prio2(Priority, T, Res)
    end.

%%--------------------------------------------------------------------
%% Function: process_non_wildcard_contacts(LogTag, SipUser, Location,
%%                                         Header, AppName)
%%           LogTag    = string(),
%%           SipUser   =
%%           Locations = list() of contact record(), binding to add for
%%                       SipUser
%%           Header    = keylist record(), REGISTER request header
%%           AppName   = atom(), incomingproxy | outgoingproxy
%% Descrip.: process a SIP Contact entry (thats not a wildcard)
%%           and do the appropriate db add/rm/update, see:
%%           RFC 3261 chapter 10.3 - Processing REGISTER Request -
%%           step 7 for more details
%% Returns : void() | throw(...) (throw is either a siperror or a
%%                                Mnesia error)
%%--------------------------------------------------------------------
process_non_wildcard_contacts(LogTag, SipUser, Locations, Header, AppName) ->
    CallId = sipheader:callid(Header),
    {CSeqStr, _CSeqMethod} = sipheader:cseq(Header),
    CSeq = list_to_integer(CSeqStr),

    %% get expire value from request header only once, this will speed up the calls to
    %% parse_register_contact_expire/2 that are done for each Locations entry
    ExpireHeader = sipheader:expires(Header),
    process_non_wildcard_contacts2(LogTag, SipUser, CallId, CSeq, ExpireHeader, AppName, Locations).

%% process_non_wildcard_contacts2 - part of process_non_wildcard_contacts()
process_non_wildcard_contacts2(LogTag, SipUser, CallId, CSeq, ExpireHeader, AppName, [Location | T]) ->
    {atomic, R} = phone:get_sipuser_location_binding(SipUser, sipurl:parse(Location#contact.urlstr)),
    Priority = 100,
    %% check if SipUser-Location binding exists in database
    case R of
	[] ->
	    %% User has no bindings in the location database, register this one
	    register_contact(LogTag, SipUser, Location, Priority, ExpireHeader, CallId, CSeq, AppName);
	[SipUserLocation] ->
	    %% User has exactly one binding in the location database matching this one, do some checking
	    check_same_call_id(LogTag, SipUser, Location, SipUserLocation, Priority,
			       CallId, CSeq, ExpireHeader, AppName)
    end,
    process_non_wildcard_contacts2(LogTag, SipUser, CallId, CSeq, ExpireHeader, AppName, T);
process_non_wildcard_contacts2(_LogTag, _SipUser, _CallId, _CSeq, _ExpireHeader, _AppName, []) ->
    ok.

%% DBLocation = phone record(), currently stored sipuser-location info
%% ReqLocation = contact record(), sipuser-location binding data in REGISTER request
check_same_call_id(LogTag, SipUser, ReqLocation, DBLocation, Priority, CallId, CSeq, ExpireHeader, AppName) ->
    case CallId == DBLocation#phone.callid of
	true ->
	    %% request has same call-id so a binding already exists
	    check_greater_cseq(LogTag, SipUser, ReqLocation, DBLocation,
			       Priority, CallId, CSeq, ExpireHeader, AppName);
	false ->
	    %% call-id differs, so the UAC has probably been restarted.
	    case parse_register_contact_expire(ExpireHeader, ReqLocation) == 0 of
		true ->
		    %% zero expire-time, unregister bindning
		    logger:log(normal, "~s: UN-REGISTER ~s at ~s (priority ~p)",
			       [LogTag, SipUser, DBLocation#phone.requristr, Priority]),
		    phone:delete_record(DBLocation);
		false ->
		    %% non-zero expire-time, update the binding
		    register_contact(LogTag, SipUser, ReqLocation, Priority, ExpireHeader, CallId, CSeq, AppName)
	    end
    end.

check_greater_cseq(LogTag, SipUser, ReqLocation, DBLocation, Priority, CallId, CSeq, ExpireHeader, AppName) ->
    %% only process reqest if cseq is > than the last one processed i.e. ignore
    %% old, out of order requests
    case CSeq > DBLocation#phone.cseq of
	true ->
	    case parse_register_contact_expire(ExpireHeader, ReqLocation) == 0 of
		true ->
		    %% unregister bindning
		    logger:log(normal, "~s: UN-REGISTER ~s at ~s (priority ~p)",
			       [LogTag, SipUser, DBLocation#phone.requristr, Priority]),
		    phone:delete_record(DBLocation);
		false ->
		    %% update the binding
		    register_contact(LogTag, SipUser, ReqLocation, Priority, ExpireHeader, CallId, CSeq, AppName)
	    end;
	false ->
	    logger:log(debug, "Location: NOT updating binding for user ~p, entry ~p in db has CSeq ~p "
		       "and request has ~p", [SipUser, DBLocation#phone.requristr, DBLocation#phone.cseq, CSeq]),
	    %% RFC 3261 doesn't appear to document the proper error code for this case
	    throw({siperror, 403, "Request out of order, contained old CSeq number"})
    end.

%%--------------------------------------------------------------------
%% Function: to_url(LDBE)
%%           LDBE = siplocationdb_e record()
%% Descrip.: Create a SIP URL from a SIP location db entry.
%% Returns : sipurl record()
%%--------------------------------------------------------------------
to_url(LDBE) when is_record(LDBE, siplocationdb_e) ->
    case lists:keysearch(outgoingproxy, 1, LDBE#siplocationdb_e.flags) of
	{value, {outgoingproxy, URLstr}} ->
	    %% siplocationdb_e has outgoingproxy specified, create suitable URL
	    OPURL = sipurl:parse(URLstr),
	    %% XXX we should make sure to use a SIPS URI if the registered location was SIPS
	    AddrStr = sipurl:print(LDBE#siplocationdb_e.address),
	    NewParams = url_param:add(OPURL#sipurl.param_pairs, "addr", AddrStr),
	    sipurl:set([{param, NewParams}], OPURL);
	_ when is_record(LDBE#siplocationdb_e.address, sipurl) ->
	    %% No outgoingproxy, just return URL from LDBE
	    LDBE#siplocationdb_e.address
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: register_contact(LogTag, SipUser, Location, Priority,
%%                            ExpireHeader, CallId, CSeq, AppName)
%%           LogTag       = string()
%%           SipUser      = string()
%%           Location     = contact record()
%%           Priority     = integer()
%%           ExpireHeader = list() of string(), Expire header from
%%                          REGISTER request, or [] if not present
%%           CallId       = string(), Call-Id header from REGISTER
%%                          request
%%           CSeq         = integer(), CSeq number from REGISTER
%%                          request
%% Descrip.: add or update a Location entry
%% Returns : -
%%--------------------------------------------------------------------
register_contact(LogTag, SipUser, Location, Priority, ExpireHeader, CallId, CSeq, AppName)
  when is_list(SipUser), is_record(Location, contact), is_integer(Priority), is_list(CallId), is_integer(CSeq) ->
    Expire = parse_register_expire(ExpireHeader, Location),
    logger:log(normal, "~s: REGISTER ~s at ~s (priority ~p, expire in ~p)",
	       [LogTag, SipUser, Location#contact.urlstr, Priority, Expire]),
    Flags = register_contact_flags(Priority, Location, AppName),
    phone:insert_purge_phone(SipUser, Flags, dynamic,
			     Expire + util:timestamp(),
			     sipurl:parse(Location#contact.urlstr),
			     CallId, CSeq).

register_contact_flags(Priority, _Contact, incomingproxy) ->
    [{priority, Priority}];
register_contact_flags(Priority, Contact, outgoingproxy) ->
    ContactURL = sipurl:parse(Contact#contact.urlstr),
    Proto = sipurl:get_port(ContactURL),
    MyURL = sipurl:new([{host, siprequest:myhostname()},
			{port, sipserver:get_listenport(Proto)}
		       ]),
    [{priority, Priority}, {outgoingproxy, sipurl:print(MyURL)}].

%% determine expiration time for a specific contact. Use default
%% value if contact/header supplies no expiration period.
%% Returns : integer(), time in seconds
parse_register_expire(ExpireHeader, Contact) when is_record(Contact, contact) ->
    ContactExpire = parse_register_contact_expire(ExpireHeader, Contact),
    case ContactExpire of
	%% no expire - use default
	none ->
	    3600;
	ContactExpire ->
	    %% expire value supplied by request - we can choose to accept,
	    %% change (shorten/increase expire period) or reject too short expire
	    %% times with a 423 (Interval Too Brief) error.
	    %% Currently implementation only limits the max expire period
	    MaxRegisterTime = sipserver:get_env(max_register_time, 43200),

	    lists:min([MaxRegisterTime, ContactExpire])
    end.


%%--------------------------------------------------------------------
%% Function: unregister_contacts(LogTag, RequestHeader, Location)
%%           LogTag = string(),
%%           RequestHeader = keylist record(),
%%           Locations = list of phone record(),
%% Descrip.: handles wildcard based removal (RFC 3261 chapter 10
%%           page 64 - step 6), this function handles a single
%%           Location (sipuser-location bindning)
%% Returns : ok |
%%           throw(SipError)
%%--------------------------------------------------------------------
unregister_contacts(LogTag, RequestHeader, Locations) when is_record(RequestHeader, keylist),
							   is_list(Locations) ->
    RequestCallId = sipheader:callid(RequestHeader),
    {CSeq, _}  = sipheader:cseq(RequestHeader),
    RequestCSeq = list_to_integer(CSeq),

    F = fun(Location) ->
		unregister_contact(LogTag, Location, RequestCallId, RequestCSeq)
	end,
    lists:foreach(F, Locations).


unregister_contact(LogTag, #phone{class = dynamic}=Location, RequestCallId, RequestCSeq) ->
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

	    Flags = Location#phone.flags,
	    Priority = case lists:keysearch(priority, 1, Flags) of
			   {value, {priority, P}} -> P;
			   _ -> 100
		       end,

	    SipUser = Location#phone.number,
	    logger:log(normal, "~s: UN-REGISTER ~s at ~s (priority ~p)",
		       [LogTag, SipUser, Location#phone.requristr, Priority]),
	    ok;
	false ->
	    %% Request CSeq value was too old, abort Request
	    %% RFC 3261 doesn't appear to document the proper error code for this case
	    throw({siperror, 403, "Request out of order, contained old CSeq number"})
    end;
unregister_contact(LogTag, Location, _RequestCallId, _RequestCSeq) when is_record(Location, phone) ->
    logger:log(debug, "~s: Not un-registering location with class not 'dynamic' : ~p",
	       [LogTag, Location#phone.requristr]),
    ok.

%%--------------------------------------------------------------------
%% Function: parse_register_contact_expire(Header, Contact)
%%           parse_register_contact_expire(ExpireHeader, Contact)
%%           Header  = keylist record(), the request headers
%%           ExpireHeader = sipheader:expires(Header) return value
%%           Contact = contact record(), a contact entry from a request
%% Descrip.: determine the expire time supplied for a contact in a SIP
%%           REGISTER request
%% Returns : integer() |
%%           none        if no expire was supplied
%% Note    : Test order may not be changed as it is specified by
%%           RFC 3261 chapter 10 page 64 - step 6
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
    end;

parse_register_contact_expire(Header, Contact) when is_record(Header, keylist),
						    is_record(Contact, contact) ->
    ExpireHeader = sipheader:expires(Header),
    parse_register_contact_expire(ExpireHeader, Contact).


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->

    %% test get_priorities(Locations)
    %%--------------------------------------------------------------------
    Loc0 = #siplocationdb_e{flags=[], expire=0},
    Loc1 = #siplocationdb_e{flags=[{priority, 1}], expire=1},
    Loc2 = #siplocationdb_e{flags=[{priority, 2}], expire=2},
    Loc3 = #siplocationdb_e{flags=[{priority, 2}], expire=3},

    io:format("test: get_priorities/1 - 1~n"),
    %% normal case
    [1, 2, 2] = get_priorities([Loc0, Loc1, Loc2, Loc3]),

    io:format("test: get_priorities/1 - 2~n"),
    %% no location with priority
    [] = get_priorities([Loc0]),


    %% test prioritize_locations(Locations)
    %%--------------------------------------------------------------------
    Loc4 = #siplocationdb_e{flags=[{priority, 4}], expire=4},

    io:format("test: prioritize_locations/1 - 1~n"),
    [Loc1] = prioritize_locations([Loc0, Loc1, Loc2, Loc3, Loc4]),

    io:format("test: prioritize_locations/1 - 2~n"),
    [Loc2, Loc3] = prioritize_locations([Loc2, Loc3, Loc4]),

    io:format("test: prioritize_locations/1 - 3~n"),
    [Loc4] = prioritize_locations([Loc4, Loc0]),


    %% is_valid_wildcard_request(Header, ContactList)
    %%--------------------------------------------------------------------
    io:format("test: is_valid_wildcard_request/2 - 1~n"),
    %% test valid wildcard
    true = is_valid_wildcard_request(keylist:from_list([{"Expires", ["0"]}]), [contact:new("*")]),

    io:format("test: is_valid_wildcard_request/2 - 2~n"),
    %% test non-zero Expires
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["1"]}]), [contact:new("*")])),

    io:format("test: is_valid_wildcard_request/2 - 3~n"),
    %% test non-zero Expires, starting with a zero
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["01"]}]), [contact:new("*")])),

    io:format("test: is_valid_wildcard_request/2 - 4~n"),
    %% test without Expires-header
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([]), [contact:new("*")])),

    io:format("test: is_valid_wildcard_request/2 - 5~n"),
    %% test with non-numeric Expires-header
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["test"]}]), [contact:new("*")])),

    io:format("test: is_valid_wildcard_request/2 - 6~n"),
    %% test with invalid Expires-header
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([{"Expires", ["0 invalid"]}]), [contact:new("*")])),

    io:format("test: is_valid_wildcard_request/2 - 7~n"),
    %% non-wildcard contact
    false = is_valid_wildcard_request(keylist:from_list([]), [contact:new("sip:ft@example.org")]),

    io:format("test: is_valid_wildcard_request/2 - 8~n"),
    %% multiple non-wildcard contact
    false = is_valid_wildcard_request(keylist:from_list([]), [contact:new("sip:ft@example.org"),
							      contact:new("sip:ft@example.net")]),

    io:format("test: is_valid_wildcard_request/2 - 9~n"),
    %% multiple contacts, one is a wildcard
    {siperror, 400, _} =
	(catch is_valid_wildcard_request(keylist:from_list([]), [contact:new("*"),
								 contact:new("sip:ft@example.org")])),

    %% parse_register_expire(ExpireHeader, Contact)
    %%--------------------------------------------------------------------
    io:format("test: parse_register_expire/2 - 1~n"),
    %% test default
    3600 = parse_register_expire([], contact:new("sip:ft@example.org")),

    io:format("test: parse_register_expire/2 - 2~n"),
    %% test that contact parameter is used if present
    1201 = parse_register_expire(["1202"], contact:new("sip:ft@example.org", [{"expires", "1201"}])),

    io:format("test: parse_register_expire/2 - 3~n"),
    %% test that expires header is used if contact parameter is absent
    1202 = parse_register_expire(["1202"], contact:new("sip:ft@example.org")),

    io:format("test: parse_register_expire/2 - 4~n"),
    %% test that contact can't be larger than maximum
    43200 = parse_register_expire([], contact:new("sip:ft@example.org", [{"expires", "86400"}])),


    %% locations_to_contacts2(Locations, Now, [])
    %%--------------------------------------------------------------------
    io:format("test: locations_to_contacts2/3 - 0~n"),
    LTCNow = util:timestamp(),
    LTC_L1 = #siplocationdb_e{expire = LTCNow + 1, address = sipurl:parse("sip:ft@one.example.org")},
    LTC_L2 = #siplocationdb_e{expire = LTCNow + 2, address = sipurl:parse("sip:ft@two.example.org")},
    LTC_L3 = #siplocationdb_e{expire = never, address = sipurl:parse("sip:ft@static.example.org")},

    io:format("test: locations_to_contacts2/3 - 1~n"),
    %% test basic case
    ["<sip:ft@one.example.org>;expires=1", "<sip:ft@two.example.org>;expires=2"] =
	locations_to_contacts2([LTC_L2, LTC_L1], LTCNow, []),

    io:format("test: locations_to_contacts2/3 - 2~n"),
    %% test that we ignore entrys that never expire
    [] = locations_to_contacts2([LTC_L3], LTCNow, []),

    io:format("test: locations_to_contacts2/3 - 3~n"),
    %% test that we ignore entrys that never expire together with other entrys
    ["<sip:ft@one.example.org>;expires=1", "<sip:ft@two.example.org>;expires=2"] =
	locations_to_contacts2([LTC_L2, LTC_L3, LTC_L1], LTCNow, []),


    %% to_url(LDBE)
    %%--------------------------------------------------------------------
    io:format("test: to_url/1 - 0~n"),
    ToURL_URL1 = sipurl:parse("sip:ft@192.0.2.111;line=foo"),
    ToURL_URL2 = sipurl:parse("sips:ft@192.0.2.111;line=foo"),
    ToURL_URL3 = sipurl:parse("sip:ft@192.0.2.111;line=foo;transport=TLS"),

    io:format("test: to_url/1 - 1~n"),
    %% test without outgoingproxy flag
    ToURL_URL1 = to_url(#siplocationdb_e{flags = [], address = ToURL_URL1}),

    io:format("test: to_url/1 - 2~n"),
    %% test with outgoingproxy flag
    "sip:out.example.org;addr=sip:ft%40192.0.2.111%3Bline%3Dfoo" =
	sipurl:print( to_url(#siplocationdb_e{flags = [{outgoingproxy, "sip:out.example.org"}],
					      address = ToURL_URL1}) ),

    io:format("test: to_url/1 - 3~n"),
    %% test that parameters in outgoingproxy URL are preserved
    "sips:out.example.org;a=b;addr=sip:ft%40192.0.2.111%3Bline%3Dfoo" =
	sipurl:print( to_url(#siplocationdb_e{flags = [{outgoingproxy, "sips:out.example.org;a=b"}],
					      address = ToURL_URL1}) ),

    io:format("test: to_url/1 - 4 (disabled)~n"),
    %% test that outgoingproxy URL is upgraded to SIPS if address is SIPS
    %%    "sips:out.example.org;a=b;addr=sips:ft%40192.0.2.111%3Bline%3Dfoo" =
    sipurl:print( to_url(#siplocationdb_e{flags = [{outgoingproxy, "sip:out.example.org;a=b"}],
					  address = ToURL_URL2}) ),

    io:format("test: to_url/1 - 5 (disabled)~n"),
    %% test that outgoingproxy URL is upgraded to SIPS if address has transport=TLS
    %%    "sips:out.example.org;a=b;addr=sips:ft%40192.0.2.111%3Bline%3Dfoo%3Btransport%3Dtls" =
    sipurl:print( to_url(#siplocationdb_e{flags = [{outgoingproxy, "sip:out.example.org"}],
					  address = ToURL_URL3}) ),

    ok.
