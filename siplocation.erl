-module(siplocation).
-export([process_register_isauth/4, prioritize_locations/1,
	 debugfriendly_locations/1, get_locations_for_users/1,
	 get_user_with_contact/1, remove_expired_phones/0]).

register_contact(LogTag, Phone, Location, Priority, Header) ->
    {_, Contact} = Location,
    Expire = parse_register_expire(Header, Contact),
    NewContact = remove_expires_parameter(Contact),
    logger:log(normal, "~s: REGISTER ~s at ~s (priority ~p, expire in ~p)",
		[LogTag, Phone, sipurl:print(NewContact), Priority, Expire]),
    phone:insert_purge_phone(Phone, [{priority, Priority}],
			     dynamic,
			     Expire + util:timestamp(),
			     NewContact).

remove_expires_parameter(Contact) ->
    {User, _, Host, Port, _} = Contact,
    Param1 = sipheader:contact_params({none, Contact}),
    Param2 = dict:erase("expires", Param1),
    Parameters = sipheader:dict_to_param(Param2),
    {User, none, Host, Port, Parameters}.

unregister_contact(LogTag, Phone, Contact, Priority) ->
    logger:log(normal, "~s: UN-REGISTER ~s at ~s (priority ~p)",
		[LogTag, Phone, sipheader:contact_print([{none, Contact}]), Priority]),
    phone:insert_purge_phone(Phone, [{priority, Priority}],
			     dynamic,
			     util:timestamp(),
			     Contact).

process_register_isauth(LogTag, Header, Phone, Contacts) ->
    % XXX RFC3261 says to store Call-ID and CSeq and to check these on
    % registers replacing erlier bindings (10.3 #7)

    check_valid_register_request(Header),

    % try to find a wildcard to process
    case process_register_wildcard_isauth(LogTag, Header, Phone, Contacts) of
	none ->
	    % no wildcard, loop through all Contacts
	    lists:map(fun (Location) ->
			register_contact(LogTag, Phone, Location, 100, Header)
		      end, Contacts);
	_ ->
	    none
    end,
    {ok, {200, "OK", [{"Contact", fetch_contacts(Phone)}]}}.

check_valid_register_request(Header) ->
    Require = keylist:fetch("Require", Header),
    case Require of
	[] ->
	    true;
	_ ->
	    logger:log(normal, "Request check: The client requires unsupported extension(s) ~p", [Require]),
	    throw({siperror, 420, "Bad Extension", [{"Unsupported", Require}]})
    end.

fetch_contacts(Phone) ->
    case phone:get_phone(Phone) of
	{atomic, Locations} ->
	    locations_to_contacts(Locations);
	_ ->
	    none
    end.

locations_to_contacts([]) ->
    [];
locations_to_contacts([{Location, Flags, Class, Expire} | Rest]) ->
    lists:append(print_contact(Location, Expire), locations_to_contacts(Rest)).

print_contact(Location, Expire) ->
    [C] = sipheader:contact_print([{none, Location}]),
    case Expire of
	never ->
	    logger:log(debug, "Not adding permanent contact ~p to REGISTER response", [C]),
	    [];
	_ ->
	    % make sure we don't end up with a negative Expires
	    NewExpire = lists:max([0, Expire - util:timestamp()]),
	    [C ++ ";expires=" ++ integer_to_list(NewExpire)]
    end.

process_register_wildcard_isauth(LogTag, Header, Phone, Contacts) ->
    case is_valid_wildcard_request(Header, Contacts) of
	true ->
	    logger:log(debug, "Location: Processing valid wildcard un-register"),
	    case phone:get_phone(Phone) of
		{atomic, Entrys} ->
		    % loop through all Entrys and unregister them
		    lists:map(fun (Entry) ->
			{Location, Flags, Class, Expire} = Entry,
			Prio = case lists:keysearch(priority, 1, Flags) of
			    {value, {priority, P}} -> P;
			    _ -> 100
			end,
			unregister_contact(LogTag, Phone, Location, Prio)
		      end, Entrys);
		_ ->
		    none
	    end;
	_ ->
	    none
    end.

is_valid_wildcard_request(Header, [{_, {wildcard, Parameters}}]) ->
    case keylist:fetch("Expires", Header) of
	["0"] ->
	    % A single wildcard contact with an Expires header of 0, now just check that
	    % Parameters does not contain any expire value except possibly zero
	    case dict:find("expires", sipheader:contact_params({none, {wildcard, Parameters}})) of
		{ok, E} ->
		    case E of
			"0" ->
			    true;
			_ ->
			    %logger:log(debug, "Location: Wildcard with non-zero contact expires parameter (~p), invalid IMO", [Parameters]),
			    throw({siperror, 400, "Wildcard with non-zero contact expires parameter"})
		    end;
		_ ->
		    true
	    end;
	_ ->
	    throw({siperror, 400, "Wildcard without 'Expires: 0', invalid (RFC3261 10.2.2)"})
    end;
is_valid_wildcard_request(Header, Contacts) ->
    % More than one Contacts (or just one non-wildcard), make sure there
    % are no wildcards since that would be invalid
    case wildcard_grep(Contacts) of
	true ->
	    %logger:log(debug, "Location: Wildcard present but not alone, invalid (RFC3261 10.3 #6)"),
	    throw({siperror, 400, "Wildcard present but not alone, invalid (RFC3261 10.3 #6)"});
	_ ->
	    none
    end.

wildcard_grep([]) ->
    nomatch;
wildcard_grep([{_, {wildcard, Parameters}} | Rest]) ->
    true;
wildcard_grep([Foo | Rest]) ->
    wildcard_grep(Rest).
    
parse_register_expire(Header, Contact) ->
    MaxRegisterTime = sipserver:get_env(max_register_time, 43200),
    case dict:find("expires", sipheader:contact_params({none, Contact})) of
	{ok, E1} ->
	    % Contact parameters has an expire value, use that
	    lists:min([MaxRegisterTime, list_to_integer(E1)]);
	error ->
	    case keylist:fetch("Expires", Header) of
		[E2] ->
		    % Request has an Expires header, use that
		    lists:min([MaxRegisterTime, list_to_integer(E2)]);
		[] ->
		    % Default expire
		    3600
	    end
    end.

remove_expired_phones() ->
    case phone:expired_phones() of
	{atomic, Expired} ->
	    remove_phones(Expired);
	{aborted, {no_exists, Table}} ->
	    logger:log(error, "Location: Mnesia says that table '~p' does not exist - did you bootstrap Yxa? (See README file)", [Table]),
	    error;
	E ->
	    logger:log(error, "Location: phone:expired_phones() returned unknown result : ~p", [E]),
	    error
    end.

remove_phones([]) ->
    true;

remove_phones([Phone | Rest]) ->
    {User, Location, Class} = Phone,
    [C] = sipheader:contact_print([{none, Location}]),
    logger:log(normal, "Location: User ~s contact ~s has expired", [User, C]),
    phone:delete_phone(User, Class, Location),
    remove_phones(Rest).


% Checks if any of our users are registered at the
% location specified. Used to determine if we should
% proxy requests to a URI without authorization.
get_user_with_contact(URI) ->
    {User, _, Host, UPort, _} = URI,
    Port = siprequest:default_port(UPort),
    URIstr = sipurl:print({User, none, Host, Port, []}),
    case phone:get_phone_with_requristr(URIstr) of
	{atomic, [SIPuser | _]} ->
	    SIPuser;
	_ ->
	    none
    end.

% Looks up all locations for a list of users. Used
% to find out where a set of users are to see where
% we should route a request.
get_locations_for_users([]) ->
    [];
get_locations_for_users([User | Rest]) ->
    Locations = case phone:get_phone(User) of
	{atomic, L} ->
	    L;
	Unknown ->
	    logger:log(debug, "Location: Unknown result from phone:get_phone() in get_locations_for_users : ~p",
			[Unknown]),
	    []
    end,
    lists:append(Locations, get_locations_for_users(Rest)).

prioritize_locations([]) ->
    [];
prioritize_locations(Locations) when list(Locations) ->
    BestPrio = lists:min(get_prioritys(Locations)),
    get_locations_with_prio(list_to_integer(BestPrio), Locations);
prioritize_locations(Unknown) ->
    logger:log(error, "prioritize_locations() called with non-list argument ~p", [Unknown]),
    [].

get_locations_with_prio(_, []) ->
    [];
get_locations_with_prio(Priority, [Location | Rest]) ->
    {Contact, Flags, Class, Expire} = Location,
    Prio = lists:keysearch(priority, 1, Flags),
    case Prio of
	{value, {priority, Priority}} ->
	    lists:append([{Contact, Flags, Class, Expire}], get_locations_with_prio(Priority, Rest));
	  _ ->
	    get_locations_with_prio(Priority, Rest)
    end.

get_prioritys(Locations) ->
    lists:map(fun ({Location, Flags, Class, Expire}) ->
		      Prio = lists:keysearch(priority, 1, Flags),
		      case Prio of
		          {value, {priority, P}} ->
				integer_to_list(P);
			  _ ->
				none
		      end
	      end, Locations).

debugfriendly_locations([]) ->
    [];
debugfriendly_locations([Location | Rest]) ->
    [Prio] = get_prioritys([Location]),
    {Contact, _, _, Expire} = Location,
    % make sure we don't end up with a negative Expires
    NewExpire = lists:max([0, Expire - util:timestamp()]),
    [C] = sipheader:contact_print([{none, Contact}]),
    Res = lists:concat(["priority ", Prio, ": ", C, ";expires=", NewExpire]),
    lists:append([Res], debugfriendly_locations(Rest)).
    
