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
	 process_register_isauth/4,
	 prioritize_locations/1,
	 debugfriendly_locations/1,
	 get_locations_for_users/1,
	 get_user_with_contact/1,

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
%% Function: register_request(Request, Origin, LogStr)
%%           Request = response record()
%%           Origin  = siporigin record()
%%           LogStr  = string(), description of request
%% Descrip.: Process a received REGISTER. First check if it is for
%%           one of our domains (homedomain), then check that it
%%           contains proper authentication and that the authenticated
%%           user is allowed to register using this address-of-record.
%%           Finally, let siplocation:process_register_isauth()
%%           process all the Contact headers and update the location
%%           database.
%% Returns : Does not matter.
%%--------------------------------------------------------------------
register_request(Request, Origin, LogStr) when is_record(Request, request), is_record(Origin, siporigin) ->
    URL = Request#request.uri,
    logger:log(debug, "incomingproxy: REGISTER ~p", [sipurl:print(URL)]),
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 1
    %% check if this registrar handles the domain the request wants to register for
    case local:homedomain(URL#sipurl.host) of
	true ->
	    register_require_supported(Request, Origin, LogStr, THandler, LogTag);
	_ ->
	    %% act as proxy and forward message to other domain
	    logger:log(debug, "incomingproxy: REGISTER for non-homedomain ~p", [URL#sipurl.host]),
	    do_request(Request, Origin)
    end.

%% part of register_request/3
register_require_supported(Request, Origin, LogStr, THandler, LogTag) ->
    Header = Request#request.header,
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 2
    case is_valid_register_request(Header) of
	true ->
	    register_authenticate(Request, Origin, LogStr, THandler, LogTag);
	{siperror, Status, Reason, ExtraHeaders} ->
	    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders)
    end.

%% part of register_request/3
register_authenticate(Request, _Origin, LogStr, THandler, LogTag) ->
    {URL, Header} =
	{Request#request.uri, Request#request.header},
    logger:log(debug, "incomingproxy: ~s -> processing", [LogStr]),
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
	    case catch siplocation:process_register_isauth(LogTag ++ ": incomingproxy",
							   NewHeader, SIPuser, Contacts) of
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
		    logger:log(error, "=ERROR REPORT==== siplocation:process_register_isauth() failed :~n~p~n",
			       [Reason]),
		    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error");
		_ ->
		    true
	    end;
	{stale, _} ->
	    logger:log(normal, "~s -> Authentication is STALE, sending new challenge", [LogStr]),
	    transactionlayer:send_challenge(THandler, www, true, none);
	{{false, eperm}, SipUser} when SipUser /= none ->
	    logger:log(normal, "~s: incomingproxy: SipUser ~p NOT ALLOWED to REGISTER address ~s",
		       [LogTag, SipUser, sipurl:print(ToURL)]),
	    transactionlayer:send_response_handler(THandler, 403, "Forbidden"),
	    %% Make event about users failure to register
	    L = [{register, forbidden}, {user, SipUser}, {address, sipurl:print(ToURL)}],
	    event_handler:generic_event(normal, location, LogTag, L);
	{{false, nomatch}, SipUser} when SipUser /= none ->
	    logger:log(normal, "~s: incomingproxy: SipUser ~p tried to REGISTER invalid address ~s",
		       [LogTag, SipUser, sipurl:print(ToURL)]),
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
	    transactionlayer:send_challenge(THandler, www, false, 3);
	Unknown ->
	    logger:log(error, "Register: Unknown result from local:can_register() URL ~p :~n~p",
		       [sipurl:print(URL), Unknown]),
	    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error")
    end.


%%--------------------------------------------------------------------
%% Function: is_valid_register_request(Header)
%%           Header = keylist record()
%% Descrip.: looks for unsupported extensions.
%% Returns : true | SipError
%%--------------------------------------------------------------------
is_valid_register_request(Header) ->
    Require = keylist:fetch('require', Header),
    case Require of
	[] ->
	    true;
	_ ->
	    %% we support no extensions, so all are unsupported
	    logger:log(normal, "Request check: The client requires unsupported extension(s) ~p", [Require]),
	    {siperror, 420, "Bad Extension", [{"Unsupported", Require}]}
    end.


%%--------------------------------------------------------------------
%% Function: process_register_isauth(LogTag, Header, SipUser,
%%           Contacts)
%%           LogTag      = string(),
%%           Header      = keylist record()
%%           SipUser     = SIP authentication username (#phone.number
%%                         field entry, when using mnesia userdb)
%%           Contacts    = list() of contact record()
%% Descrip.:
%% Returns : {ok, {Status, Reason, ExtraHeaders}}
%%           {siperror, Status, Reason}
%%           {siperror, Status, Reason, ExtraHeaders}
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           ExtraHeaders = list() of {Key, NewValueList} see
%%           keylist:appendlist/2
%%--------------------------------------------------------------------
process_register_isauth(LogTag, Header, SipUser, Contacts) ->
    process_updates(LogTag, Header, SipUser, Contacts).

%% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 6 and 7
%% remove, add or update contact info in location (phone) database

%% REGISTER request had no contact header
process_updates(_LogTag, _Header, SipUser, []) ->
    %% RFC 3261 chapter 10.3 - Processing REGISTER Request - step 8
    {ok, create_process_updates_response(SipUser)};

process_updates(LogTag, Header, SipUser, Contacts) ->
    %% Processing REGISTER Request - step 6
    %% check for and process wildcard (request contact = *)
    case process_register_wildcard_isauth(LogTag, Header, SipUser, Contacts) of
	none ->
	    %% Processing REGISTER Request - step 7
	    %% No wildcard found, register/update/remove entries in Contacts.
	    %% Process registration atomicly - change all or nothing in database.
	    F = fun() ->
			process_non_wildcard_contacts(LogTag, SipUser, Contacts, Header)
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
	{atomic, Locations} ->
	    locations_to_contacts(Locations);
	_ ->
	    none
    end.

%% return: list() of string() (contact:print/1 of contact record())
locations_to_contacts(Locations) ->
    locations_to_contacts2(Locations, util:timestamp(), []).

locations_to_contacts2([], _Now, Res) ->
    Res;
locations_to_contacts2([H | T], Now, Res) when is_record(H, phone), H#phone.expire == never ->
    %% Don't include static contacts which never expire
    locations_to_contacts2(T, Now, Res);
locations_to_contacts2([H | T], Now, Res) when is_record(H, phone), is_integer(H#phone.expire) ->
    Location = H#phone.address,
    Expire = H#phone.expire,

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
		{atomic, SipUserLocations} ->
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
%% XXX usage of phone:get_sipuser_locations/1 rather than get_phone/1
%% would be preferable
%%--------------------------------------------------------------------
get_locations_for_users(In) ->
    get_locations_for_users2(In, []).

get_locations_for_users2([], Res) ->
    Res;
get_locations_for_users2([H | T], Res) ->
    {ok, Locations} = phone:get_phone(H),
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

%% Descrip. = find the Location/s that has the "best" priority in the Flags part of the tuple.
%%            Note that some may lack a {priority, PrioVal} entry
%% Returns  = list() of siplocationdb_e record() (all with same "best" priority - Priority)
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
%%           Header)
%%           LogTag    = string(),
%%           SipUser   =
%%           Locations = list() of contact record(), binding to add for
%%                       SipUser
%%           RequestHeader = keylist record()
%% Descrip.: process a SIP Contact entry (thats not a wildcard)
%%           and do the appropriate db add/rm/update, see:
%%           RFC 3261 chapter 10.3 - Processing REGISTER Request -
%%           step 7 for more details
%% Returns : -
%%--------------------------------------------------------------------
process_non_wildcard_contacts(LogTag, SipUser, Locations, RequestHeader) ->
    RequestCallId = sipheader:callid(RequestHeader),
    {CSeq, _CSeqMethod}  = sipheader:cseq(RequestHeader),
    RequestCSeq = list_to_integer(CSeq),

    %% get expire value from request header only once, this will speed up the calls to
    %% parse_register_contact_expire/2 that are done for each Locations entry
    ExpireHeader = sipheader:expires(RequestHeader),
    F = fun(Location) ->
		process_non_wildcard_contact(LogTag, SipUser, Location,
					     RequestCallId, RequestCSeq, ExpireHeader)
	end,
    lists:foreach(F, Locations).



process_non_wildcard_contact(LogTag, SipUser, Location, RequestCallId, RequestCSeq, ExpireHeader) ->
    {atomic, R} = phone:get_sipuser_location_binding(SipUser, sipurl:parse(Location#contact.urlstr)),
    Priority = 100,
    %% check if SipUser-Location binding exists in database
    case R of
	[] ->
	    %% User has no bindings in the location database, register this one
	    register_contact(LogTag, SipUser, Location, Priority, ExpireHeader,RequestCallId, RequestCSeq);
	[SipUserLocation] ->
	    %% User has exactly one binding in the location database matching this one, do some checking
	    check_same_call_id(LogTag, SipUser, Location, SipUserLocation, Priority,
			       RequestCallId, RequestCSeq, ExpireHeader)
    end.

%% DBLocation = phone record(), currently stored sipuser-location info
%% ReqLocation = contact record(), sipuser-location binding data in REGISTER request
check_same_call_id(LogTag, SipUser, ReqLocation, DBLocation, Priority, RequestCallId, RequestCSeq, ExpireHeader) ->
    case RequestCallId == DBLocation#phone.callid of
	true ->
	    %% request has same call-id so a binding already exists
	    check_greater_cseq(LogTag, SipUser, ReqLocation, DBLocation,
			       Priority, RequestCallId, RequestCSeq, ExpireHeader);
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
		    register_contact(LogTag, SipUser, ReqLocation, Priority, ExpireHeader, RequestCallId, RequestCSeq)
	    end
    end.

check_greater_cseq(LogTag, SipUser, ReqLocation, DBLocation, Priority, RequestCallId, RequestCSeq, ExpireHeader) ->
    %% only process reqest if cseq is > than the last one processed i.e. ignore
    %% old, out of order requests
    case RequestCSeq > DBLocation#phone.cseq of
	true ->
	    case parse_register_contact_expire(ExpireHeader, ReqLocation) == 0 of
		true ->
		    %% unregister bindning
		    logger:log(normal, "~s: UN-REGISTER ~s at ~s (priority ~p)",
			       [LogTag, SipUser, DBLocation#phone.requristr, Priority]),
		    phone:delete_record(DBLocation);
		false ->
		    %% update the binding
		    register_contact(LogTag, SipUser, ReqLocation, Priority, ExpireHeader, RequestCallId, RequestCSeq)
	    end;
	false ->
	    logger:log(debug, "Location: NOT updating binding for user ~p, entry ~p in db has CSeq ~p "
		       "and request has ~p", [SipUser, DBLocation#phone.requristr, DBLocation#phone.cseq,
					      RequestCSeq]),
	    %% RFC 3261 doesn't appear to document the proper error code for this case
	    throw({siperror, 403, "Request out of order, contained old CSeq number"})
    end.


%%--------------------------------------------------------------------
%% Function:
%% Descrip.: function intended for debugging purpouses
%% Returns :
%%--------------------------------------------------------------------
debugfriendly_locations([]) ->
    [];
debugfriendly_locations([Location | Rest]) ->
    Prio = case get_priorities([Location]) of
	       [] -> none;
	       [P] -> P
	   end,
    {Contact, _, _, Expire} = Location,
    %% make sure we don't end up with a negative Expires
    NewExpire = lists:max([0, Expire - util:timestamp()]),
    C = contact:print(contact:new(Contact)),
    Res = lists:concat(["priority ", Prio, ": ", C, ";expires=", NewExpire]),
    lists:append([Res], debugfriendly_locations(Rest)).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: register_contact(LogTag, SipUser, Location, Priority,
%%                            ExpireHeader)
%%           LogTag        = string()
%%           SipUser       = string()
%%           Location      = contact record()
%%           Priority      = integer()
%%           ExpireHeader  =
%%           RequestCallId = string()
%%           RequestCSeq   = integer()
%% Descrip.: add or update a Location entry
%% Returns : -
%%--------------------------------------------------------------------
register_contact(LogTag, SipUser, Location, Priority, ExpireHeader,RequestCallId, RequestCSeq)
  when is_record(Location, contact) ->
    Expire = parse_register_expire(ExpireHeader, Location),
    logger:log(normal, "~s: REGISTER ~s at ~s (priority ~p, expire in ~p)",
	       [LogTag, SipUser, Location#contact.urlstr, Priority, Expire]),
    phone:insert_purge_phone(SipUser, [{priority, Priority}],
			     dynamic,
			     Expire + util:timestamp(),
			     sipurl:parse(Location#contact.urlstr),
			     RequestCallId,
			     RequestCSeq).

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
    LTC_L1 = #phone{expire = LTCNow + 1, address = sipurl:parse("sip:ft@one.example.org")},
    LTC_L2 = #phone{expire = LTCNow + 2, address = sipurl:parse("sip:ft@two.example.org")},
    LTC_L3 = #phone{expire = never, address = sipurl:parse("sip:ft@static.example.org")},

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

    ok.
