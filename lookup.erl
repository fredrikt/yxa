-module(lookup).
-export([lookupregexproute/1,
	 lookupuser/1,
	 lookup_url_to_locations/1,
	 lookup_url_to_addresses/2,
	 lookup_addresses_to_users/1,
	 lookup_address_to_users/1,
	 lookupdefault/1,
	 lookuppotn/1,
	 lookupenum/1,
	 lookuppstn/1,
	 isours/1,
	 homedomain/1,
	 lookupappserver/1,
	 rewrite_potn_to_e164/1,
	 get_remote_party_number/2,
	 format_number_for_remote_party_id/3,
	 get_remote_party_name/2
	]).

-include("database_regexproute.hrl").

lookupregexproute(User) ->
    {atomic, Routes} = database_regexproute:list(),
    Sortedroutes = lists:sort(fun (Elem1, Elem2) -> 
				      Prio1 = lists:keysearch(priority, 1, Elem1#regexproute.flags),
				      Prio2 = lists:keysearch(priority, 1, Elem2#regexproute.flags),
				      case {Prio1, Prio2} of
					  {_, false} ->
					      true;
					  {false, _} ->
					      false;
					  {{value, {priority,P1}}, {value, {priority,P2}}} when P1 >= P2 ->
					      true;
					  _ ->
					      false
				      end
			      end, Routes),
    Rules = lists:map(fun(R) ->
			      {R#regexproute.regexp, sipurl:print(R#regexproute.address)}
		      end, Sortedroutes),
    case util:regexp_rewrite(User, Rules) of
	nomatch ->
	    none;
	Result ->
	    {proxy, sipurl:parse(Result)}
    end.

lookupuser(URL) ->
    case local:lookup_url_to_locations(URL) of
	nomatch ->
	    % User does not exist in any of our databases.
	    nomatch;
	Locations when list(Locations) ->
	    % check if more than one location was found.
	    case Locations of
	        [] ->
		    % User exists but has no currently known locations
		    case local:lookupregexproute(sipurl:print(URL)) of
			none ->
			    logger:log(debug, "Lookup: No locations found for URL ~p, and no matching regexp rules.", [sipurl:print(URL)]),
			    none;
			Loc ->
			    logger:log(debug, "Lookup: Regexp-route lookup of ~p -> ~p", [sipurl:print(URL), Loc]),
		            Loc
		    end;
		[{BestLocation, _, _, _}] ->
		    {proxy, BestLocation};
		_ ->
		    % More than one location registered for this address, check for appserver...
		    % (appserver is the program that handles forking of requests)
		    local:lookupappserver(URL)
	    end;
	Unknown ->
	    logger:log(error, "Lookup: Unknown result from local:lookup_url_to_locations() of URL ~p in lookupuser : ~n~p",
			[sipurl:print(URL), Unknown]),
	    throw({siperror, 500, "Server Internal Error"})
    end.

lookup_url_to_locations(URL) ->
    case local:get_users_for_url(URL) of
	nomatch ->
	    nomatch;
	Users when list(Users) ->
	    Locations = local:get_locations_for_users(Users),
	    local:prioritize_locations([Users], Locations);
	Unknown ->
	    logger:log(error, "Lookup: Unknown result from local:get_users_for_url() in lookup_url_to_locations: ~p",
			[Unknown]),
	    throw({siperror, 500, "Server Internal Error"})
    end.
    
lookup_url_to_addresses(sipuserdb_mnesia, URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    % sipuserdb_mnesia is extra liberal with usernames
    Standard = lookup_url_to_addresses(lookup, URL),
    Addr1 = local:url2mnesia_userlist(URL),
    Addr2 = case util:isnumeric(User) of
	true -> [User];
	_ -> []
    end,	
    lists:append([Standard, Addr1, Addr2]);
lookup_url_to_addresses(Src, URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    % Make a list of all possible addresses we
    % can create out of this URL
    Tel = local:canonify_numberlist([User]),
    lists:append([sipurl:print({User, none, Host, none, []})], Tel).

lookup_addresses_to_users(Addresses) ->
    case local:get_users_for_addresses_of_record(Addresses) of
	[] ->
	    [];
	nomatch ->
	    [];
	Users when list(Users) ->
	    Res = lists:sort(Users),
	    logger:log(debug, "Lookup: Addresses ~p belongs to one or more users : ~p", [Addresses, Res]),
	    Res;
	Unknown ->
	    logger:log(error, "Lookup: lookup_addresses_to_users: unknown result from local:get_users_for_addresses_of_record(~p) : ~p",
	    		[Addresses, Unknown]),
	    []
    end.

lookup_address_to_users(Address) ->
    case local:get_users_for_address_of_record(Address) of
	[] ->
	    [];
	nomatch ->
	    [];
	Users when list(Users) ->
	    Res = lists:sort(Users),
	    logger:log(debug, "Lookup: Address ~p belongs to one or more users : ~p", [Address, Res]),
	    Res;
	Unknown ->
	    logger:log(error, "Lookup: lookup_address_to_users: unknown result from local:get_users_for_address_of_record(~p) : ~p",
	    		[Address, Unknown]),
	    []
    end.

lookupappserver(Key) ->
    AppServer = sipserver:get_env(appserver, none),
    case AppServer of
	none ->
	    logger:log(debug, "Lookup: More than one location is registered for user ~p, but no appserver configured! Returning 480 Temporarily Unavailable.",
			[Key]),
	    {response, 480, "Temporarily Unavailable"};
	_ ->
	    {Host, Port} = sipurl:parse_hostport(AppServer),
	    {forward, Host, Port}
    end.

lookupdefault(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    case homedomain(Host) of
	true ->
	    logger:log(debug, "Lookup: Cannot default-route request to a local domain (~s), aborting", [Host]),
	    none;
        _ ->
	    DefaultRoute = sipserver:get_env(defaultroute, none),
	    case DefaultRoute of
		none ->
		    logger:log(debug, "Lookup: No default route - dropping request"),
		    {response, 500, "Can't route request"};	% XXX rätt error-code?
		Hostname ->
		    {Host, Port} = sipurl:parse_hostport(Hostname),
		    NewURI = {User, none, Host, Port, []},
		    logger:log(debug, "Lookup: Default-routing to ~s", [sipurl:print(NewURI)]),
		    % XXX we should preserve the Request-URI by proxying this as a loose router.
		    % It is almost useless to only preserve the User-info IMO. We can do this
		    % by returning {forward, Host, Port} instead.
		    {proxy, NewURI}
	    end
    end.

lookuppotn("+" ++ E164) ->
    Loc1 = case util:isnumeric(E164) of
	true ->
	    Res1 = lookupenum("+" ++ E164),
    	    logger:log(debug, "Lookup: ENUM lookup on ~s -> ~p", ["+" ++ E164, Res1]),
	    case Res1 of
	    	none ->
    		    Res2 = lookuppstn("+" ++ E164),
    		    logger:log(debug, "Lookup: PSTN lookup on ~s -> ~p", ["+" ++ E164, Res2]),
	    	    Res2;
    		Res1 ->
		    Res1
	    end;
	_ ->
	    none
    end;
lookuppotn(Number) ->
    Res = rewrite_potn_to_e164(Number),
    case Res of
        none ->
            none;
        "+" ++ E164 ->
	    lookuppotn("+" ++ E164);
	_ ->
	    none
    end.	    


lookupenum("+" ++ E164) ->
    case dnsutil:enumlookup("+" ++ E164) of
	none ->
	    none;
	URL ->
	    {E164User, _, E164Host, _, _} = sipurl:parse(URL),
	    IsMe = homedomain(E164Host),
	    NewE164 = rewrite_potn_to_e164(E164User),
	    SameE164 = util:casecompare(NewE164, "+" ++ E164),
	    if
		IsMe /= true ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in remote URL ~p, relaying", [URL]),
		    {relay, sipurl:parse(URL)};
		NewE164 == none ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in a homedomain but not E.164 URL ~p, proxying", [URL]),
		    {proxy, sipurl:parse(URL)};
		SameE164 == true ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in a homedomain (~p) and the same E.164 number (~p == ~p), avoiding loop",
			    		[E164Host, E164User, "+" ++ E164]),
		    none;
		true ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in homedomain E.164 URL ~p, proxying", [URL]),
		    {proxy, sipurl:parse(URL)}
	    end
    end;
lookupenum(Number) ->
    Res = rewrite_potn_to_e164(Number),
    case Res of
        none ->
            none;
        "+" ++ E164 ->
	    lookupenum("+" ++ E164);
	_ ->
	    none
    end.	    

    
lookuppstn("+" ++ E164) ->
    case util:isnumeric(E164) of
	true ->
	    case util:regexp_rewrite("+" ++ E164, sipserver:get_env(e164_to_pstn, [])) of
		nomatch ->
		    none;
		PSTN ->
		    logger:log(debug, "Rewrite: ~s to PSTN URL ~p", ["+" ++ E164, PSTN]),
		    {proxy, sipurl:parse("sip:" ++ PSTN)}
	    end;
	_ ->
	    none
    end;
lookuppstn(Number) ->
    Res = rewrite_potn_to_e164(Number),
    case Res of
        none ->
            none;
        "+" ++ E164 ->
	    lookuppstn("+" ++ E164);
	_ ->
	    none
    end.	    


rewrite_potn_to_e164("+" ++ E164) ->
    case util:isnumeric(E164) of
	true ->
	    "+" ++ E164;
	_ ->
	    none
    end;
rewrite_potn_to_e164(Number) when list(Number) ->
    case util:isnumeric(Number) of
	true ->
	    case util:regexp_rewrite(Number, sipserver:get_env(internal_to_e164, [])) of
		"+" ++ E164 ->
		    rewrite_potn_to_e164("+" ++ E164);
		_ ->
		    none
	    end;
	_ ->
	    none
    end;
rewrite_potn_to_e164(_) ->
    none.

isours(URL) ->
    case local:get_users_for_url(URL) of
	[] ->
	    logger:log(debug, "Lookup: isours ~s -> false", [sipurl:print(URL)]),
	    false;
	nomatch ->
	    logger:log(debug, "Lookup: isours ~s -> false", [sipurl:print(URL)]),
	    false;
	Users when list(Users) ->
	    logger:log(debug, "Lookup: isours ~s -> user(s) ~p", [sipurl:print(URL), Users]),
	    true;
	Unknown ->
	    logger:log(debug, "Lookup: isours ~s -> Unknown result ~p", [sipurl:print(URL), Unknown]),
	    false
    end.

homedomain(Domain) ->
    case util:casegrep(Domain, sipserver:get_env(homedomain, [])) of
	true ->
	    true;
	_ ->
	    HostnameList = lists:append(sipserver:get_env(myhostnames, []), [siphost:myip()]),
	    util:casegrep(Domain, HostnameList)
    end.

get_remote_party_number(URL, DstHost) ->
    case local:get_users_for_url(URL) of
	[User] ->
	    case local:get_telephonenumber_for_user(User) of
		Number when list(Number) ->
		    local:format_number_for_remote_party_id(Number, URL, DstHost);
		nomatch ->
		    logger:log(debug, "Lookup: No telephone number found for user ~p", [User]),
		    none;
		Unknown ->
		    logger:log(error, "Lookup: Unexpected results from local:get_telephonenumber_for_user() for user ~p in get_remote_party_number: ~p",
				[User, Unknown]),
		    none
	    end;
	nomatch ->
	    logger:log(debug, "Lookup: No user(s) match address ~p, can't get telephone number",
			[sipurl:print(URL)]),
	    none;
	[] ->
	    logger:log(debug, "Lookup: No user(s) match address ~p, can't get telephone number",
			[sipurl:print(URL)]),
	    none;
	Users when list(Users) ->
	    logger:log(debug, "Lookup: Multiple users match address ~p, can't get telephone number",
			[sipurl:print(URL)]),
	    none;
	Unknown ->
	    logger:log(error, "Lookup: Unexpected results from local:get_users_for_url() for URL ~p in get_remote_party_number: ~p",
			[sipurl:print(URL), Unknown]),
	    none
    end.

format_number_for_remote_party_id(Number, ToURI, DstHost) ->
    rewrite_potn_to_e164(Number).

get_remote_party_name(Key, URI) when list(Key) ->
    case directory:lookup_tel2name(Key) of
	none ->
	    none;
	DisplayName when list(DisplayName) ->
	    DisplayName;
	Unknown ->
	    logger:log(error, "Lookup: Failed to get name for remote party ~p, unexpected result from lookup_tel2name : ~p",
		    		[Key, Unknown]),
	    none
    end;
get_remote_party_name(Key, URI) ->
    logger:log(error, "Lookup: Could not get remote party name for non-list argument ~p", [Key]),
    none.
