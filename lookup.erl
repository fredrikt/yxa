-module(lookup).
-export([lookupregexproute/1, lookupuser/1, lookupdefault/1, lookuppotn/1,
	 lookupenum/1, lookuppstn/1, isours/1, homedomain/1,
	 prioritize_locations/1, get_locations_with_prio/2,
	 lookupappserver/1, rewrite_potn_to_e164/1,
	 get_remote_party_number/3, format_number_for_remote_party_id/3,
	 get_remote_party_name/2]).

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
    {User, Pass, Host, Port, Parameters} = URL,
    Key = local:sipuser(URL),
    Loc1 = lookupaddress(Key),
    case Loc1 of
	none ->
	    case util:isnumeric(User) of
		true ->
		    lookupaddress(User);
		_ ->
		    none
	    end;
	Loc1 ->
	    Loc1
    end.
    
lookupaddress(Key) ->
    case phone:get_phone(Key) of
	{atomic, []} ->
	    Loc = lookupregexproute(Key),
	    logger:log(debug, "Lookup: Address lookup of ~p -> ~p", [Key, Loc]),
	    Loc;
	{atomic, Locations} ->
	    BestLocations = prioritize_locations(Locations),
	    logger:log(debug, "Lookup: Best location(s) of ~p :~n~p", [Key, BestLocations]),
	    % check if more than one location was found.
	    case BestLocations of
	        [] ->
		    none;
		[{BestLocation, _, _, _}] ->
		    {proxy, BestLocation};
		 _ ->
		    % More than one location registered for this user, check for appserver...
		    % (appserver is the program that handles forking of requests)
		    local:lookupappserver(Key)
	    end
    end.

lookupappserver(Key) ->
    AppServer = sipserver:get_env(appserver, none),
    case AppServer of
	none ->
	    logger:log(debug, "Lookup: More than one location is registered for user ~p, but no appserver configured! Aborting.",
			[Key]),
	    {response, 480, "Temporarily Unavailable"};
	_ ->
	    {Host, Port} = sipurl:parse_hostport(AppServer),
	    {forward, Host, Port}
    end.

prioritize_locations([]) ->
    {none, [], none, never};
prioritize_locations(Locations) ->
    BestPrio = lists:min(get_prioritys(Locations)),
    get_locations_with_prio(BestPrio, Locations).

get_locations_with_prio(Priority, Locations) ->
    PrioInt = list_to_integer(Priority),
    L = lists:map(fun ({Location, Flags, Class, Expire}) ->
		      Prio = lists:keysearch(priority, 1, Flags),
		      case Prio of
		          {value, {priority, PrioInt}} ->
				{Location, Flags, Class, Expire};
			  _ ->
				none
		      end
	      end, Locations),
    lists:filter(fun(A) ->
		case A of
		    none -> false;
		    _ -> true
		end
	     end, L).

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

lookupdefault(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    case homedomain(Host) of
	true ->
	    logger:log(debug, "Lookup: Cannot default-route to a local domain (~s), aborting", [Host]),
	    none;
        _ ->
	    DefaultRoute = sipserver:get_env(defaultroute, none),
	    case DefaultRoute of
		none ->
		    logger:log(debug, "Lookup: No default route - dropping request"),
		    {response, 500, "Can't route request"};	% XXX rätt error-code?
		Hostname ->
		    logger:log(debug, "Lookup: Proxying to ~p @ ~p (my defaultroute)", [User, Hostname]),
		    {proxy, {User, none, Hostname, "5060", []}}
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
	    IsMe = is_me(E164Host),
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
rewrite_potn_to_e164(Number) ->
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
    end.


isours(URL) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Key = local:sipuser(URL),
    Res = in_userdb(Key),
    case Res of
	none ->
	    case util:isnumeric(User) of
		true ->
		    in_userdb(User);
		_ ->
		    none
	    end;
	_ ->
	    Res
    end.

in_userdb(Key) ->
    case phone:get_users_for_number(Key) of
	{atomic, []} ->
	    false;
	{atomic, Foo} ->
	    true;
	{aborted, _} ->
	    false
    end.

is_me(Hostname) ->
    case homedomain(Hostname) of
	true ->
	    true;
	_ ->
	    HostnameList = lists:append(sipserver:get_env(myhostnames, []), [siphost:myip()]),
	    util:casegrep(Hostname, HostnameList)
    end.

homedomain(Domain) ->
    util:casegrep(Domain, sipserver:get_env(homedomain, [])).

get_remote_party_number(Key, URI, DstHost) ->
    {_, _, Host, _, _} = URI,
    case phone:get_numbers_for_user(Key) of
	{atomic, [FirstNumber | _]} ->
	    Number = local:format_number_for_remote_party_id(FirstNumber, URI, DstHost),
	    [Addr] = sipheader:contact_print([{none, {Number, none, Host, none, ["user=phone"]}}]),
	    Addr ++ ";screen=no;privacy=off";
	_ ->
	    none
    end.

format_number_for_remote_party_id(Number, ToURI, DstHost) ->
    rewrite_potn_to_e164(Number).

get_remote_party_name(Key, URI) ->
    case directory:lookup_tel2name(Key) of
	none ->
	    none;
	DisplayName ->
	    [C] = sipheader:contact_print([{DisplayName, URI}]),
	    C ++ ";screen=no;privacy=off"
    end.

