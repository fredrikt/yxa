%%%-------------------------------------------------------------------
%%% File    : lookup.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Varios lookup functions. Mainly routing logic for our
%%%           three applications incomingproxy, pstnproxy and
%%%           appserver. Most of these functions are called through
%%%           functions in local.erl with the same name, so if you
%%%           want to make them return different values than the
%%%           defaults in this file, make a local.erl specific for
%%%           your domain.
%%%
%%% @since    20 Mar 2003 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------
-module(lookup).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([lookupregexproute/1,
	 lookupuser/1,
	 lookupuser_gruu/2,
	 lookupuser_locations/2,
	 lookup_url_to_locations/1,
	 lookup_url_to_addresses/2,
	 lookup_addresses_to_users/1,
	 lookup_address_to_users/1,
	 lookupdefault/1,
	 lookuppotn/1,
	 lookupnumber/1,
	 lookupenum/1,
	 lookuppstn/1,
	 isours/1,
	 is_request_to_this_proxy/1,
	 homedomain/1,
	 lookupappserver/1,
	 rewrite_potn_to_e164/1,
	 get_remote_party_number/4,
	 format_number_for_remote_party_id/3,
	 get_remote_party_name/2,
	 remove_unsuitable_locations/2,
	 lookup_result_to_str/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("database_regexproute.hrl").
-include("siprecords.hrl").
-include("sipsocket.hrl").


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Input) ->
%%            {proxy, URL} |
%%            none
%%
%%            Input = string() "what we will match against the regexps"
%%
%%            URL = #sipurl{}
%%
%% @doc     See if we have a regexp that matches Input in the Mnesia
%%          regexp route table. If we find one, we return a proxy
%%          tuple with the resulting destination. The regexps in the
%%          database have a priority field, where higher priority is
%%          better.
%% @end
%%--------------------------------------------------------------------
lookupregexproute(Input) when is_list(Input) ->
    Routes = database_regexproute:list(),
    lookupregexproute2(Input, Routes).

lookupregexproute2(Input, Routes) ->
    Sortedroutes = lists:sort(fun (Elem1, Elem2) ->
				      Prio1 = lists:keysearch(priority, 1, Elem1#regexproute.flags),
				      Prio2 = lists:keysearch(priority, 1, Elem2#regexproute.flags),
				      case {Prio1, Prio2} of
					  {_, false} ->
					      true;
					  {false, _} ->
					      false;
					  {{value, {priority, P1}}, {value, {priority, P2}}} when P1 >= P2 ->
					      true;
					  _ ->
					      false
				      end
			      end, Routes),

    %% Build {Regexp, Address} tuples of all non-expired regexproutes in Sortedroutes
    Now = util:timestamp(),
    ReversedRules =
	lists:foldl(fun(R, Acc) when R#regexproute.expire == never; R#regexproute.expire > Now ->
			    This = {R#regexproute.regexp, R#regexproute.address},
			    [This | Acc];
		       (_R, Acc) ->
			    %% expired regexproute
			    Acc
		    end, [], Sortedroutes),
    Rules = lists:reverse(ReversedRules),

    case util:regexp_rewrite(Input, Rules) of
	nomatch ->
	    none;
	Result ->
	    {proxy, sipurl:parse(Result)}
    end.

%%--------------------------------------------------------------------
%% @spec    (URL) ->
%%            {ok, Users, Res} | nomatch
%%
%%            URL = #sipurl{}
%%
%%            Users  = [string()] | none "usernames matching URL"
%%            Res    = {proxy, URL}                    |
%%                     {proxy, {with_path, URL, Path}} |
%%                     {relay, URL}                    |
%%                     {forward, URL}                  |
%%                     {response, Status, Reason}      |
%%                     none
%%            URL    = #sipurl{}
%%            Path   = [string()]
%%            Status = integer() "SIP status code"
%%            Reason = string() "SIP reason phrase"
%%
%% @doc     The main 'give me a set of locations for one of our users'
%%          function that incomingproxy uses, when it determines that
%%          a request is for one of it's homedomains. Returns
%%          'nomatch' if no user was found, 'none' if the user(s)
%%          associated with URL has no registered locations.
%% @end
%%--------------------------------------------------------------------
lookupuser(URL) when is_record(URL, sipurl) ->
    case local:is_gruu_url(URL) of
	{true, GRUU} ->
	    %% format lookupuser_gruu for incomingproxy, which is the one calling this function
	    case local:lookupuser_gruu(URL, GRUU) of
		{ok, User, Loc, _Contact} when is_list(User), is_tuple(Loc) ->
		    {ok, [User], Loc};
		{ok, User, Loc} ->
		    {ok, [User], Loc}
	    end;
	false ->
	    lookupuser2(URL)
    end.

lookupuser2(URL) ->
    case local:get_users_for_url(URL) of
	nomatch ->
	    NoParamURL = sipurl:set([{param, []}], URL),
	    case local:lookupregexproute(sipurl:print(NoParamURL)) of
		none ->
		    logger:log(debug, "Lookup: No user matches URL ~p, and no regexp rules match either.",
			       [sipurl:print(NoParamURL)]),
		    nomatch;
		{proxy, Loc} ->
		    logger:log(debug, "Lookup: No matching user, but a matching regexp rule was found : ~p -> ~p",
			       [sipurl:print(NoParamURL), sipurl:print(Loc)]),
		    {ok, [], {proxy, Loc}}
	    end;
	[User] when is_list(User) ->
	    %% single user, look if the user has a CPL script
	    Res =
		case local:user_has_cpl_script(User, incoming) of
		    true ->
			%% let appserver handle requests for users with CPL scripts
			case local:lookupappserver(URL) of
			    {forward, AppS} ->
				logger:log(debug, "Lookup: User ~p has a CPL script, forwarding to appserver : ~p",
					   [User, sipurl:print(AppS)]),
				{forward, AppS};
			    {response, Status, Reason} ->
				logger:log(debug, "Lookup: User ~p has a CPL script, but appserver lookup resulted in "
					   "request to send SIP response '~p ~s'",
					   [User, Status, Reason]),
				{response, Status, Reason};
			    _ ->
				logger:log(error, "Lookup: User ~p has a CPL script, but I could not find an appserver",
					   [User]),
				%% Fallback to just looking in the location database
				lookupuser_get_locations([User], URL)
			end;
		    false ->
			lookupuser_get_locations([User], URL)
		end,
	    {ok, [User], Res};
	Users when is_list(Users) ->
	    %% multiple (or no) users
	    Res = lookupuser_get_locations(Users, URL),
	    {ok, Users, Res}
    end.

%% part of lookupuser()
lookupuser_get_locations(Users, URL) ->
    %% check if more than one location exists for our list of users.
    case local:lookupuser_locations(Users, URL) of
	[] ->
	    %% User exists but has no currently known locations
	    NoParamURL = sipurl:set([{param, []}], URL),
	    case local:lookupregexproute(sipurl:print(NoParamURL)) of
		none ->
		    logger:log(debug, "Lookup: No locations found for users ~p, and no regexp rules match URL ~p.",
			       [Users, sipurl:print(NoParamURL)]),
		    none;
		{proxy, Loc} ->
		    logger:log(debug, "Lookup: Regexp-route rewrite of ~p -> ~p",
			       [sipurl:print(NoParamURL), sipurl:print(Loc)]),
		    {proxy, Loc}
	    end;
	[Location] when is_record(Location, siplocationdb_e) ->
	    lookupuser_single_location(Location, URL);
	[Location | _] = Locations when is_record(Location, siplocationdb_e) ->
	    lookupuser_multiple_locations(Locations, URL)
    end.

%% Returns: {proxy, DstList}                |
%%          {proxy, {with_path, URL, Path}} |
%%          {proxy, URL}                    |
%%          {response, Status, Reason}
lookupuser_single_location(Location, URL) when is_record(Location, siplocationdb_e) ->
    %% A single location was found in the location database (after removing any unsuitable ones)
    ThisNode = node(),
    Dst =
	case lists:keysearch(socket_id, 1, Location#siplocationdb_e.flags) of
	    {value, {socket_id, #locationdb_socketid{node = ThisNode} = SocketId}} ->
		%% We have a stored socket_id, meaning the client did Outbound. We must now
		%% check if that socket is still available.
		case sipsocket:get_specific_socket(SocketId#locationdb_socketid.id) of
		    {error, _Reason} ->
			%% The socket the user registered using is no longer available - reject
			%% request with a '430 Flow Failed' response (draft-Outbound #5.3 (Forwarding Requests))
			%% "For connection-oriented transports, if the flow no longer exists the
			%% proxy SHOULD send a 430 (Flow Failed) response to the request."
			{response, 430, "Flow Failed"};
		    SipSocket ->
			[#sipdst{proto		= SocketId#locationdb_socketid.proto,
				 addr		= SocketId#locationdb_socketid.addr,
				 port		= SocketId#locationdb_socketid.port,
				 uri		= siplocation:to_url(Location),
				 socket		= SipSocket,
				 instance	= Location#siplocationdb_e.instance
				}
			]
		end;
	    {value, {socket_id, #locationdb_socketid{node = OtherNode}}} ->
		logger:log(debug, "Lookup: User has Outbound flow to other node (~p)", [OtherNode]),
		none;
	    false ->
		none
	end,

    case Dst of
	{response, _Status2, _Reason2} ->
	    Dst;
	_ when is_list(Dst) ->
	    {proxy, Dst};
	none ->
	    %% No Outbound socket for this node to use, look for RFC3327 Path
	    case lists:keysearch(path, 1, Location#siplocationdb_e.flags) of
		{value, {path, Path}} ->
		    %% Path found, check if the first element is ours
		    Me = siprequest:construct_record_route(URL#sipurl.proto),
		    case Path of
			[Me] ->
			    {proxy, siplocation:to_url(Location)};
			[Me | PathRest] ->
			    logger:log(debug, "Lookup: Removing myself from Path of location database entry, "
				      "leaving ~p", [PathRest]),
			    {proxy, {with_path, siplocation:to_url(Location), PathRest}};
			_ ->
			    {proxy, {with_path, siplocation:to_url(Location), Path}}
		    end;
		false ->
		    {proxy, siplocation:to_url(Location)}
	    end
    end.


lookupuser_multiple_locations(Locations, URL) when is_list(Locations), is_record(URL, sipurl) ->
    case is_same_outbound_client_without_path(Locations, URL) of
	true ->
	    %% We found more than one entry in the location database for this URL, but
	    %% they all end up at the same User-Agent (they have the same Instance ID).
	    %% Presumably this User-Agent does Outbound and registers more than once for
	    %% redundancy. We should not parallell-fork this but rather go on a sequential
	    %% hunt for a working destination.
	    Sorted = siplocation:sort_most_recent_first(Locations),
	    case make_dstlist(Sorted) of
		[] ->
		    %% All locations were removed by make_dstlist. This means they all used
		    %% Outbound and the flows all terminated at this host, and are gone.
		    {response, 430, "Flow Failed"};
		DstList when is_list(DstList) ->
		    {proxy, DstList}
	    end;
	false ->
	    %% More than one location registered for this address, check for appserver...
	    %% (appserver is the program that handles forking of requests)
	    local:lookupappserver(URL)
    end.

%% part of lookupuser_multiple_locations/2
%% Returns: true | false
is_same_outbound_client_without_path(In, URL) ->
    Me = siprequest:construct_record_route(URL#sipurl.proto),
    is_same_outbound_client_without_path2(In, undefined, undefined, Me).

is_same_outbound_client_without_path2([#siplocationdb_e{instance = []} | _], _PrevInst, _PrevUser, _Me) ->
    %% Binding without instance, this is not an Outbound client
    false;

is_same_outbound_client_without_path2([H | _] = In, undefined, undefined, Me) ->
    %% First one, to get the path checked as well we recurse on all of In
    #siplocationdb_e{instance = Instance,
		     sipuser  = User
		    } = H,
    is_same_outbound_client_without_path2(In, Instance, User, Me);

is_same_outbound_client_without_path2([#siplocationdb_e{instance = PrevInst, sipuser = PrevUser} = H | T],
				      PrevInst, PrevUser, Me) ->
    %% check Path too
    case lists:keysearch(path, 1, H#siplocationdb_e.flags) of
	{value, {path, [Me]}} ->
	    %% This one is OK, check next
	    is_same_outbound_client_without_path2(T, PrevInst, PrevUser, Me);
	false ->
	    %% No Path - that is OK, check next
	    is_same_outbound_client_without_path2(T, PrevInst, PrevUser, Me);
	_ ->
	    %% uh oh, this one has Path. We currently can't return such complex data from lookupuser
	    %% so we'll have to use appserver for this.
	    false
    end;

is_same_outbound_client_without_path2([#siplocationdb_e{} | _T], _PrevInst, _PrevUser, _Me) ->
    %% Not same instance ID or user
    false;

is_same_outbound_client_without_path2([], _Instance, _User, _Me) ->
    %% Instance ID was not empty, and all matched.
    true.

%% part of lookupuser_multiple_locations/2
%% Returns: NewList = list() of siplocationdb_e record()
make_dstlist(In) ->
    ThisNode = node(),
    make_dstlist2(In, ThisNode, []).

make_dstlist2([H | T], ThisNode, Res) when is_record(H, siplocationdb_e) ->
    case lists:keysearch(socket_id, 1, H#siplocationdb_e.flags) of
	{value, {socket_id, #locationdb_socketid{node = ThisNode} = SocketId}} ->
	    case sipsocket:get_specific_socket(SocketId#locationdb_socketid.id) of
		{error, _Reason} ->
		    %% Flow not avaliable anymore, skip this one
		    make_dstlist2(T, ThisNode, Res);
		SipSocket ->
		    This =
			#sipdst{proto		= SocketId#locationdb_socketid.proto,
				addr		= SocketId#locationdb_socketid.addr,
				port		= SocketId#locationdb_socketid.port,
				uri		= siplocation:to_url(H),
				socket		= SipSocket,
				instance	= H#siplocationdb_e.instance
			       },
		    make_dstlist2(T, ThisNode, [This | Res])
	    end;
	_ ->
	    %% Not using Outbound, or connected to some other node - use as is.
	    %% We already know we don't need to care about Path (checked by
	    %% is_same_outbound_client_without_path).
	    This =
		#sipdst{uri = siplocation:to_url(H),
			instance = H#siplocationdb_e.instance
		       },
	    make_dstlist2(T, ThisNode, [This | Res])
    end;
make_dstlist2([], _ThisNode, Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% @spec    (URL, GRUU) ->
%%            {ok, User, Res, Contact}
%%
%%            URL  = #sipurl{} "GRUU Request-URI"
%%            GRUU = string()
%%
%%            Res     = {proxy, URL}                    |
%%                      {proxy, {with_path, URL, Path}} |
%%                      {response, Status, Reason}
%%            User    = none | string() "SIP authentication user of GRUU"
%%            Contact = #siplocationdb_e{} "used by outgoingproxy"
%%
%% @doc     Look up the 'best' contact of a GRUU. Note : used by
%%          incomingproxy and outgoingproxy
%% @end
%%--------------------------------------------------------------------
lookupuser_gruu(URL, GRUU) when is_record(URL, sipurl), is_list(GRUU) ->
    %% XXX if it was an 'opaque=' GRUU, verify somehow that the rest of the URI matches
    %% the user we found when we look up the GRUU? Probably a good idea.
    logger:log(debug, "Lookup: URL ~s contains a GRUU (~p), looking for active contact",
	       [sipurl:print(URL), GRUU]),
    case gruu:get_contact_for_gruu(GRUU) of
	{ok, User, Contact} when is_record(Contact, siplocationdb_e) ->
	    logger:log(debug, "Lookup: GRUU ~p matches user ~p contact ~s",
		       [GRUU, User, sipurl:print(Contact#siplocationdb_e.address)]),

	    %% Copy 'grid' parameter
	    GridURL = gruu:prepare_contact(Contact, URL),

	    Res = lookupuser_single_location(Contact#siplocationdb_e{address = GridURL}, URL),
	    {ok, User, Res};
	{ok, User, none} ->
	    %% GRUU found, but user has no active contacts
	    logger:log(debug, "Lookup: GRUU ~p matches user ~p, but user has no active contacts. "
		       "Responding '480 Temporarily Unavailable'", [GRUU, User]),
	    %% "If the request URI is within the domain of the proxy, and
	    %% the URI has been constructed by the domain such that the proxy is
	    %% able to determine that it has the form of a GRUU for an AOR that is
	    %% known within the domain, but the instance ID is unknown, the proxy
	    %% SHOULD generate a 480 (Temporarily Unavailable)."
	    %% GRUU draft 06 #8.4.1 (Request Targeting)
	    {ok, none, {response, 480, "Temporarily Unavailable"}};
	nomatch ->
	    %% looked like a GRUU, but not found
	    logger:log(debug, "Lookup: Request-URI is a GRUU, but I have no record of it. "
		       "Answering '404 Not Found'."),
	    %% "If the request URI is within the domain of the proxy, and the URI has
	    %% been constructed by the domain such that the proxy is able to
	    %% determine that it has the form of a GRUU for an AOR that is unknown
	    %% within the domain, the proxy rejects the request with a 404 (Not
	    %% Found)." GRUU draft 06 #8.4.1 (Request Targeting)
	    {ok, none, {response, 404, "Not Found"}}
    end.

%%--------------------------------------------------------------------
%% @spec    (Users, URL) ->
%%            Locations
%%
%%            Users = [string()] "SIP users to fetch locations of"
%%            URL   = #sipurl{} "the Request-URI"
%%
%%            Locations = [#siplocationdb_e{}]
%%
%% @doc     Return all locations for a list of users that is suitable
%%          given a Request-URI. By suitable, we mean that we filter
%%          out SIP locations if Request-URI was SIPS, unless this
%%          proxy is configured not to.
%% @end
%%--------------------------------------------------------------------
lookupuser_locations(Users, URL) when is_list(Users), is_record(URL, sipurl) ->
    Locations1 = local:get_locations_for_users(Users),
    Locations = local:prioritize_locations(Users, Locations1),
    local:remove_unsuitable_locations(URL, Locations).

%%--------------------------------------------------------------------
%% @spec    (URL, Locations) -> [#siplocationdb_e{}]
%%
%%            URL      = #sipurl{} "Request-URI of request"
%%            Location = [#siplocationdb_e{}]
%%
%% @doc     Apply local policy for what locations are good to use for
%%          a particular Request-URI. The default action we do here
%%          is to remove non-SIPS locations if the Request-URI is
%%          SIPS, unless we are configured not to.
%% @end
%%--------------------------------------------------------------------
remove_unsuitable_locations(#sipurl{proto="sips"}, Locations) when is_list(Locations) ->
    case yxa_config:get_env(ssl_require_sips_registration) of
	{ok, true} ->
	    remove_non_sips_locations(Locations, []);
	{ok, false} ->
	    Locations
    end;
remove_unsuitable_locations(URL, Locations) when is_record(URL, sipurl), is_list(Locations) ->
    Locations.

%% part of remove_unsuitable_locations/2. Returns : list() of siplocationdb_e record()
remove_non_sips_locations([#siplocationdb_e{address=URL}=H | T], Res)
  when is_record(URL, sipurl), URL#sipurl.proto == "sips" ->
    remove_non_sips_locations(T, [H | Res]);
remove_non_sips_locations([#siplocationdb_e{address=URL}=H | T], Res) ->
    %% XXX do we need to lowercase what we get from url_param:find?
    case url_param:find(URL#sipurl.param_pairs, "transport") of
	["tls"] ->
	    %% Keep this location
	    remove_non_sips_locations(T, [H | Res]);
	_ ->
	    %% Not SIPS protocol or TLS transport parameter, remove this location from result
	    remove_non_sips_locations(T, Res)
    end;
remove_non_sips_locations([], Res) ->
    lists:reverse(Res).

%%--------------------------------------------------------------------
%% @spec    (URL) ->
%%            Locations |
%%            nomatch
%%
%%            URL = #sipurl{}
%%
%%            Locations = [#siplocationdb_e{}]
%%
%% @doc     Turn an URL into a set of locations. The URL might map to
%%          more than one user, in which case the locations for all
%%          matched users are returned. Locations is sorted according
%%          to the priority values they have in the location
%%          database.
%% @end
%%--------------------------------------------------------------------
lookup_url_to_locations(URL) when is_record(URL, sipurl) ->
    case local:get_users_for_url(URL) of
	nomatch ->
	    nomatch;
	Users when is_list(Users) ->
	    Locations = local:get_locations_for_users(Users),
	    local:prioritize_locations(Users, Locations)
    end.

%%--------------------------------------------------------------------
%% @spec    (Src, URL) -> [string()]
%%
%%            Src = atom() "who is asking"
%%            URL = #sipurl{}
%%
%% @doc     Make up a bunch of possible userdb keys from an URL. Since
%%          our userdbs store different addresses implicitly
%%          sometimes, we do this mess to make sure we find one or
%%          more users for requests destined to an URL.
%% @end
%%--------------------------------------------------------------------
lookup_url_to_addresses(Src, #sipurl{proto = "sips"} = URL) ->
    %% When turning address into user, we make no difference on SIP and SIPS URI's
    %% (as any SIP URI may be "upgraded" to SIPS).
    %% RFC3261 #19.1 (SIP and SIPS Uniform Resource Indicators).
    NewURL = sipurl:set([{proto, "sip"}], URL),
    lookup_url_to_addresses(Src, NewURL);
lookup_url_to_addresses(sipuserdb_mnesia, URL) when is_record(URL, sipurl) ->
    User = URL#sipurl.user,
    %% sipuserdb_mnesia is extra liberal with usernames
    Standard = lookup_url_to_addresses(lookup, URL),
    Addr1 = local:url2mnesia_userlist(URL),
    Addr2 = case util:isnumeric(User) of
		true -> [User];
		_ -> []
	    end,
    lists:append([Standard, Addr1, Addr2]);
lookup_url_to_addresses(_Src, URL) when is_record(URL, sipurl) ->
    %% Make a list of all possible addresses we
    %% can create out of this URL
    Addrs =
	case URL#sipurl.user of
	    none ->
		[];
	    _ ->
		local:canonify_addresses([URL#sipurl.user])
	end,
    BareURLstr = sipurl:print(sipurl:set([{pass, none}, {port, none}, {param, []}], URL)),
    lists:append([BareURLstr], Addrs).

%%--------------------------------------------------------------------
%% @spec    (Addresses) ->
%%            [string()] "list of usernames or empty list"
%%
%%            Addresses = [term()]
%%
%% @doc     Get a list of users that match an input list of addresses.
%% @end
%%--------------------------------------------------------------------
lookup_addresses_to_users(Addresses) when is_list(Addresses) ->
    case local:get_users_for_addresses_of_record(Addresses) of
	nomatch ->
	    [];
	Users when is_list(Users) ->
	    Res = lists:sort(Users),
	    logger:log(debug, "Lookup: Addresses ~p belongs to one or more users : ~p", [Addresses, Res]),
	    Res
    end.

%%--------------------------------------------------------------------
%% @spec    (Address) -> [string()] "list of usernames or empty list"
%%
%%            Address = term()
%%
%% @doc     Get a list of users that match a single address.
%% @end
%%--------------------------------------------------------------------
lookup_address_to_users(Address) ->
    case local:get_users_for_address_of_record(Address) of
	nomatch ->
	    [];
	Users when is_list(Users) ->
	    Res = lists:sort(Users),
	    logger:log(debug, "Lookup: Address ~p belongs to one or more users : ~p", [Address, Res]),
	    Res
    end.

%%--------------------------------------------------------------------
%% @spec    (Key) ->
%%            {forward, URL}             |
%%            {response, Status, Reason}
%%
%%            Key = #sipurl{}
%%
%%            URL    = #sipurl{}
%%            Status = integer() "SIP status code"
%%            Reason = string() "SIP reason phrase"
%%
%% @doc     Get the configured appserver to use for Key. Used in
%%          incomingproxy.
%% @end
%%--------------------------------------------------------------------
lookupappserver(Key) when is_record(Key, sipurl) ->
    case yxa_config:get_env(appserver) of
	{ok, URL} when is_record(URL, sipurl), URL#sipurl.user == none, URL#sipurl.pass == none ->
	    case Key#sipurl.proto of
		"sips" ->
		    SIPS_URL = sipurl:set([{proto, "sips"}], URL),
		    {forward, SIPS_URL};
		_ ->
		    {forward, URL}
	    end;
	none ->
	    logger:log(error, "Lookup: Requested to provide appserver for ~p, but no appserver configured! "
		       "Responding '480 Temporarily Unavailable (no appserver configured)'.", [sipurl:print(Key)]),
	    %% XXX perhaps the '(no appserver configured)' should either not be disclosed, or
	    %% included in a Reason: header instead of the reason phrase
	    {response, 480, "Temporarily Unavailable (no appserver configured)"}
    end.

%%--------------------------------------------------------------------
%% @spec    (URL) ->
%%            {proxy, DefaultRoute} |
%%            {response, Status, Reason}
%%
%%            URL = #sipurl{}
%%
%%            DefaultRoute = #sipurl{}
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%
%% @doc     Get the configured default route. Used in incomingproxy.
%% @end
%%--------------------------------------------------------------------
lookupdefault(URL) when is_record(URL, sipurl) ->
    case homedomain(URL#sipurl.host) of
	true ->
	    logger:log(debug, "Lookup: Cannot default-route request to a local domain (~s), aborting",
		       [URL#sipurl.host]),
	    none;
        false ->
	    case yxa_config:get_env(defaultroute) of
		{ok, DefaultURL} when is_record(DefaultURL, sipurl) ->
		    NewURI = sipurl:set([{user, URL#sipurl.user}, {pass, none}], DefaultURL),
		    logger:log(debug, "Lookup: Default-routing to ~s", [sipurl:print(NewURI)]),
		    %% XXX we should preserve the Request-URI by proxying this as a loose router.
		    %% It is almost useless to only preserve the User-info IMO. We can do this
		    %% by returning {forward, Proto, Host, Port} instead.
		    {proxy, NewURI};
		none ->
		    logger:log(debug, "Lookup: No default route - dropping request"),
		    {response, 500, "Can't route request"}	%% XXX is 500 the correct error-code?
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            {proxy, URL} |
%%            {relay, URL} |
%%            none
%%
%%            Number = string()
%%
%%            URL = #sipurl{}
%%
%% @doc     Look Up Plain Old Telephone Number. Figures out where to
%%          route a numerical destination. First we try to rewrite it
%%          to E.164 and do ENUM lookup, and if that fails,
%%          lookuppstn() on it. Then we try our fallback numerical
%%          route matching, lookupnumber(). Used in both
%%          incomingproxy and pstnproxy.
%% @end
%%--------------------------------------------------------------------
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
	   end,
    Loc1;
lookuppotn(Number) ->
    case rewrite_potn_to_e164(Number) of
        "+" ++ E164 ->
	    lookuppotn("+" ++ E164);
	_ ->
            %% Number could not be rewritten to E164, check to see if lookupnumber()
            %% can make anything out of it.
	    case local:lookupnumber(Number) of
		error ->
		    none;
		R ->
		    R
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            {proxy, URL} |
%%            {relay, URL} |
%%            none
%%
%%            Number = string()
%%
%%            URL = #sipurl{}
%%
%% @doc     Does ENUM resolving on an E164 number. If the input number
%%          is not an E164 number, it is converted first.
%% @end
%%--------------------------------------------------------------------
lookupenum("+" ++ E164) ->
    case dnsutil:enumlookup("+" ++ E164) of
	none ->
	    none;
	URLstr ->
	    URL = sipurl:parse(URLstr),
	    E164User = URL#sipurl.user,
	    E164Host = URL#sipurl.host,
	    IsMe = homedomain(E164Host),
	    %% Try to rewrite the userpart of the returned URL into a E164 number
	    %% to make sure it is not a loop back to this proxy. If it is a remote
	    %% domain, the username comparison test does not have any effect, so
	    %% a remote URL with the E164 number as username is OK.
	    {NewE164, SameE164} = case rewrite_potn_to_e164(E164User) of
				      error ->
					  {error, false};
				      Res ->
					  {Res, util:casecompare(Res, "+" ++ E164)}
				  end,
	    if
		IsMe /= true ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in remote URL ~p, relaying",
			       [sipurl:print(URL)]),
		    {relay, URL};
		NewE164 == error ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in a homedomain but not E.164 URL ~p, proxying",
			       [sipurl:print(URL)]),
		    {proxy, URL};
		SameE164 == true ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in a homedomain (~p) and the same E.164 number "
			       "(~p == ~p), avoiding loop", [E164Host, E164User, "+" ++ E164]),
		    none;
		true ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in homedomain E.164 URL ~p, proxying",
			       [sipurl:print(URL)]),
		    {proxy, URL}
	    end
    end;
lookupenum(Number) ->
    case rewrite_potn_to_e164(Number) of
        error ->
            none;
        "+" ++ E164 ->
	    lookupenum("+" ++ E164);
	_ ->
	    error
    end.

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            {proxy, URL}          |
%%            {relay, URL}          |
%%            none                  |
%%            error
%%
%%            Number = string()
%%
%%            URL = #sipurl{}
%%
%% @doc     Rewrites a number to a PSTN URL using the e164_to_pstn
%%          configuration regexp. If the number is not E164, it is
%%          converted using rewrite_potn_to_e164() first.
%% @end
%%--------------------------------------------------------------------
lookuppstn("+" ++ E164) ->
    case util:isnumeric(E164) of
	true ->
	    {ok, Regexps} = yxa_config:get_env(e164_to_pstn, []),
	    case util:regexp_rewrite("+" ++ E164, Regexps) of
		nomatch ->
		    none;
		PSTN when is_list(PSTN) ->
		    logger:log(debug, "Rewrite: ~s to PSTN URL ~p", ["+" ++ E164, PSTN]),
		    case sipurl:parse_url_with_default_protocol("sip", PSTN) of
			error ->
			    logger:log(error, "Lookup: Failed parsing result of rewrite ~p using 'e164_to_pstn'",
				       ["+" ++ E164]),
			    none;
			URL when is_record(URL, sipurl) ->
			    {proxy, URL}
		    end
	    end;
	false ->
	    error
    end;
lookuppstn(Number) ->
    case rewrite_potn_to_e164(Number) of
        "+" ++ E164 ->
	    lookuppstn("+" ++ E164);
        _ ->
	    %% Number could not be rewritten to E164, check to see if lookupnumber()
	    %% can make anything out of it.
	    case local:lookupnumber(Number) of
		error ->
		    none;
		R ->
		    R
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            {proxy, URL}          |
%%            {relay, URL}          |
%%            none                  |
%%            error
%%
%%            Number = string()
%%
%%            URL = #sipurl{}
%%
%% @doc     Check if there are any numerical matching rules that apply
%%          (configured regexp 'number_to_pstn'). Called by
%%          lookuppotn/1 and lookuppstn/1 when the input number is
%%          not rewriteable to a E164 number.
%% @end
%%--------------------------------------------------------------------
lookupnumber(Number) ->
    %% Check if Number is all digits
    case util:isnumeric(Number) of
	true ->
	    {ok, Regexps} = yxa_config:get_env(number_to_pstn, []),
	    lookupnumber2(Number, Regexps);
	false ->
	    %% Number was not numeric
	    error
    end.

%% part of lookupnumber/1
lookupnumber2(Number, Regexps) when is_list(Number), is_list(Regexps) ->
    %% Try to rewrite Number using configured regexp 'number_to_pstn'
    case util:regexp_rewrite(Number, Regexps) of
	Res when is_list(Res) ->
	    %% Check to see if what we got is a parseable URL
	    case sipurl:parse_url_with_default_protocol("sip", Res) of
		URL when is_record(URL, sipurl) ->
		    %% Check if it is a local URL or a remote
		    case homedomain(URL#sipurl.host) of
			true ->
			    {proxy, URL};
			false ->
			    {relay, URL}
		    end;
		_ ->
		    logger:log(error, "Lookup: Rewrite of number ~p using 'number_to_pstn' did not "
			       "result in a parseable URL : ~p", [Number, Res]),
		    error
	    end;
	nomatch ->
	    %% Regexp did not match
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            Result |
%%            error
%%
%%            Number = string()
%%
%%            Result = string() "\"+\" followed by an E.164 number"
%%
%% @doc     Rewrite a number to an E164 number using our local
%%          numbering plan (configured regexp 'internal_to_e164').
%% @end
%%--------------------------------------------------------------------
rewrite_potn_to_e164("+" ++ E164) ->
    case util:isnumeric(E164) of
	true ->
	    "+" ++ E164;
	_ ->
	    error
    end;
rewrite_potn_to_e164(Number) when is_list(Number) ->
    case util:isnumeric(Number) of
	true ->
	    {ok, Regexps} = yxa_config:get_env(internal_to_e164, []),
	    case util:regexp_rewrite(Number, Regexps) of
		"+" ++ E164 ->
		    rewrite_potn_to_e164("+" ++ E164);
		_ ->
		    error
	    end;
	_ ->
	    error
    end;
rewrite_potn_to_e164(_Unknown) ->
    error.

%%--------------------------------------------------------------------
%% @spec    (URL) -> true | false
%%
%%            URL = #sipurl{}
%%
%% @doc     Check if we have a user matching an URL.
%% @end
%%--------------------------------------------------------------------
isours(URL) when is_record(URL, sipurl) ->
    case local:get_users_for_url(URL) of
	nomatch ->
	    logger:log(debug, "Lookup: isours ~s -> false", [sipurl:print(URL)]),
	    false;
	Users when is_list(Users) ->
	    logger:log(debug, "Lookup: isours ~s -> user(s) ~p", [sipurl:print(URL), Users]),
	    true
    end.

%%--------------------------------------------------------------------
%% @spec    (Request) -> true | false
%%
%%            Request = #request{}
%%
%% @doc     Check if a request is destined for this proxy. Not for a
%%          domain handled by this proxy, but for this proxy itself.
%% @end
%%--------------------------------------------------------------------
is_request_to_this_proxy(Request) when is_record(Request, request) ->
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    IsOptionsForMe = is_request_to_this_proxy2(Method, URI, Header),
    IsHomedomain = local:homedomain(URI#sipurl.host),
    NoUserpart = (URI#sipurl.user == none),
    if
	IsOptionsForMe == true ->
	    true;
	IsHomedomain == true, NoUserpart == true ->
	    true;
	true ->
	    false
    end.

%% is_request_to_this_proxy2/3 is a subfunction of is_request_to_this_proxy/1,
%% called to check if this is an OPTIONS request with Max-Forwards =< 1.
%% This procedure is from RFC3261 #11 Querying for Capabilities.
is_request_to_this_proxy2("OPTIONS", URL, Header) when is_record(URL, sipurl) ->
    %% RFC3261 # 11 says a proxy that receives an OPTIONS request with a Max-Forwards less than one
    %% MAY treat it as a request to the proxy.
    case keylist:fetch('max-forwards', Header) of
	[M] ->
	    case list_to_integer(M) of
		N when N =< 1 ->
		    logger:log(debug, "Routing: Request is OPTIONS and Max-Forwards =< 1, "
			       "treating it as a request to me."),
		    true;
		_ ->
		    false
	    end;
	_ ->
	    %% No Max-Forwards, or invalid (more than one list element)
	    false
    end;
is_request_to_this_proxy2(_, URL, _) when is_record(URL, sipurl) ->
    false.

%%--------------------------------------------------------------------
%% @spec    (Domain) -> true | false
%%
%%            Domain = string()
%%
%% @doc     Check if Domain is one of our configured homedomains.
%% @end
%%--------------------------------------------------------------------
homedomain(Domain) when is_list(Domain) ->
    {ok, HomedomainL} = yxa_config:get_env(homedomain, []),
    LCdomain = http_util:to_lower(Domain),
    case lists:member(LCdomain, HomedomainL) of
	true ->
	    true;
	false ->
	    %% Domain did not match configured sets of homedomain, check against list
	    %% of hostnames and also my IP address
	    {ok, MyHostnames} = yxa_config:get_env(myhostnames, []),
	    lists:member(LCdomain, MyHostnames)
		orelse lists:member(LCdomain, siphost:myip_list())
    end.

%%--------------------------------------------------------------------
%% @spec    (User, Header, URI, DstHost) ->
%%            {ok, RPI, Number} |
%%            none
%%
%%            User    = string() "SIP authentication username"
%%            Header  = #keylist{}
%%            URI     = #sipurl{} "outgoing Request-URI"
%%            DstHost = term() "chosen destination for request"
%%
%%            RPI    = #contact{}
%%            Number = string()
%%
%% @doc     This function is used by the pstnproxy to provide a PSTN
%%          gateway with usefull caller-id information. PSTN networks
%%          typically gets upset if the "A-number" (calling party) is
%%          a SIP URL. Different gateways might want the number
%%          formatted differently, thus the DstHost parameter (a TSP
%%          gateway to PSTN might only handle E.164 numbers, while a
%%          PBX might be expecting only a 4-digit extension number).
%% @end
%%--------------------------------------------------------------------
get_remote_party_number(User, _Header, URI, DstHost) when is_list(User), is_list(DstHost), is_record(URI, sipurl) ->
    case local:get_telephonenumber_for_user(User) of
	Number when is_list(Number) ->
	    case local:format_number_for_remote_party_id(Number, URI, DstHost) of
		{ok, FormattedNumber} ->
		    Parameters = [{"party", "calling"}, {"screen", "yes"}, {"privacy", "off"}],
		    RPURI = sipurl:set([{user, FormattedNumber}, {pass, none}, {param, []}], URI),
		    {ok, contact:new(none, RPURI, Parameters), FormattedNumber};
		none ->
		    logger:log(error, "Lookup: Failed to format telephone number '~p' for Remote-Party-Id", [Number]),
		    none
	    end;
	nomatch ->
	    logger:log(debug, "Lookup: No telephone number found for user ~p", [User]),
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (Number, Header, DstHost) ->
%%            {ok, Number} | none
%%
%%            Number  = string() "the number to format"
%%            Header  = #keylist{}
%%            DstHost = term() "destination for request"
%%
%%            Number = string()
%%
%% @doc     Hook for the actual formatting once
%%          get_remote_party_number/2 has found a number to be
%%          formatted. This default function simply tries to rewrite
%%          the number to E.164. If one or more of your PSTN gateways
%%          wants the Caller-ID information in any other format, then
%%          override this function in local.erl.
%% @end
%%--------------------------------------------------------------------
format_number_for_remote_party_id(Number, _Header, _DstHost) when is_list(Number) ->
    case rewrite_potn_to_e164(Number) of
	error ->
	    none;
	Res when is_list(Res) ->
	    {ok, Res}
    end.

%%--------------------------------------------------------------------
%% @spec    (Key, URI) ->
%%            {ok, DisplayName} |
%%            none
%%
%%            Key = string() "number we should turn into a name"
%%            URI = term() "destination for request"
%%
%%            DisplayName = string()
%%
%% @doc     When pstnproxy receives a request from a PSTN gateway,
%%          this function is called to see if we can find a nice
%%          Display Name for the calling party.
%% @end
%%--------------------------------------------------------------------
get_remote_party_name(Key, URI) when is_list(Key), is_record(URI, sipurl) ->
    case directory:lookup_tel2name(Key) of
	none ->
	    none;
	[DisplayName] when is_list(DisplayName) ->
	    {ok, DisplayName};
	L when is_list(L) ->
	    logger:log(debug, "Lookup: Got more than one name back for number ~p. Since I have no way "
		       "of choosing a name from a list, I'm not going to try.", [Key]),
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (In) -> string()
%%
%%            In = term()
%%
%% @doc     Pretty-print our various used lookup result values.
%% @end
%%--------------------------------------------------------------------
lookup_result_to_str(In) ->
    lists:flatten(lookup_result_to_str2(In)).

lookup_result_to_str2({Type, URL}) when is_atom(Type), is_record(URL, sipurl) ->
    URLstr = lists:flatten( io_lib:format("(sipurl) ~s", [sipurl:print(URL)]) ),
    io_lib:format("~p", [{Type, URLstr}]);
lookup_result_to_str2(Unknown) ->
    io_lib:format("~p", [Unknown]).

%%====================================================================
%%% Internal functions
%%====================================================================


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
    %% test homedomain/1
    %% Note: We can't test this function very well because it relies heavily
    %% on configuration that can't be assumed to have any special content
    %% when testing
    %%--------------------------------------------------------------------
    MyHostname = siprequest:myhostname(),

    autotest:mark(?LINE, "homedomain/1 - 1"),
    %% test with my IP address
    %% XXX test fails if we have no interfaces up!
    true = homedomain(MyHostname),

    autotest:mark(?LINE, "homedomain/1 - 1"),
    %% test with something that should definately NOT be our hostname
    false = homedomain("1-2"),


    %% test is_request_to_this_proxy(Request)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_request_to_this_proxy/1 - 1"),
    %% test OPTIONS with Max-Forwards: 1
    true = is_request_to_this_proxy(#request{method="OPTIONS", uri=sipurl:parse("sip:ft@example.org"),
					     header=keylist:from_list([{"Max-Forwards", ["1"]}])}),

    autotest:mark(?LINE, "is_request_to_this_proxy/1 - 2"),
    %% test OPTIONS with Max-Forwards: 10
    false = is_request_to_this_proxy(#request{method="OPTIONS", uri=sipurl:parse("sip:ft@example.org"),
					      header=keylist:from_list([{"Max-Forwards", ["10"]}])}),

    autotest:mark(?LINE, "is_request_to_this_proxy/1 - 3"),
    %% test MESSAGE with Max-Forwards: 10, but with URI pointing at us
    IRTTP_URI1 = sipurl:new([{proto, "sip"}, {host, MyHostname}]),
    true = is_request_to_this_proxy(#request{method="MESSAGE", uri=IRTTP_URI1,
					     header=keylist:from_list([{"Max-Forwards", ["10"]}])}),


    %% test remove_unsuitable_locations(URL, Locations)
    %%--------------------------------------------------------------------
    Unsuitable_URL1 = sipurl:new([{proto, "sip"}, {host, "sip1.example.org"}]),
    Unsuitable_URL2 = sipurl:new([{proto, "sips"}, {host, "sips1.example.org"}]),
    Unsuitable_URL3 = sipurl:new([{proto, "sip"}, {host, "sip2.example.org"}]),
    Unsuitable_URL4 = sipurl:new([{proto, "sip"}, {host, "sip2.example.org"}, {param, ["transport=tls"]}]),

    Unsuitable_LDBE1 = #siplocationdb_e{address=Unsuitable_URL1},
    Unsuitable_LDBE2 = #siplocationdb_e{address=Unsuitable_URL2},
    Unsuitable_LDBE3 = #siplocationdb_e{address=Unsuitable_URL3},
    Unsuitable_LDBE4 = #siplocationdb_e{address=Unsuitable_URL4},

    autotest:mark(?LINE, "remove_unsuitable_locations/2 - 1"),
    %% test with non-SIPS URI, no entrys should be removed
    [Unsuitable_LDBE1, Unsuitable_LDBE2, Unsuitable_LDBE3] =
	remove_unsuitable_locations(Unsuitable_URL1, [Unsuitable_LDBE1, Unsuitable_LDBE2, Unsuitable_LDBE3]),

    autotest:mark(?LINE, "remove_unsuitable_locations/2 - 2"),
    %% test with SIPS URI
    [Unsuitable_LDBE2] =
	remove_unsuitable_locations(Unsuitable_URL2, [Unsuitable_LDBE1, Unsuitable_LDBE2, Unsuitable_LDBE3]),

    autotest:mark(?LINE, "remove_unsuitable_locations/2 - 3"),
    %% test with SIP URI but transport parameter indicating TLS
    [Unsuitable_LDBE4] =
	remove_unsuitable_locations(Unsuitable_URL2, [Unsuitable_LDBE1, Unsuitable_LDBE4]),


    %% test lookupnumber(Number)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "lookupnumber/1 - 1"),

    %% only thing we can test here is non-numeric input
    error = lookupnumber("foo"),


    %% test lookupnumber2(Number, RegExps)
    %%--------------------------------------------------------------------

    yxa_test_config:init([{homedomain, ["homedomain.example.org"]}]),

    autotest:mark(?LINE, "lookupnumber2/2 - 1"),
    %% test empty result
    error = lookupnumber2("123", [{"123", ""}]),

    autotest:mark(?LINE, "lookupnumber2/2 - 2"),
    %% test no matching regexp
    none = lookupnumber2("123", [{"12x", "foo"}]),

    autotest:mark(?LINE, "lookupnumber2/2 - 3"),
    %% test valid rewrite
    LNURL3 = sipurl:parse("sips:ft@example.org"),
    {relay, LNURL3} = lookupnumber2("123", [{"123", "sips:ft@example.org"}]),

    autotest:mark(?LINE, "lookupnumber2/2 - 4"),
    %% test valid rewrite, with default protocol
    LNURL4 = sipurl:parse("sip:ft@example.org"),
    {relay, LNURL4} = lookupnumber2("123", [{"123", "ft@example.org"}]),

    autotest:mark(?LINE, "lookupnumber2/2 - 5"),
    %% test valid rewrite, with invalid result
    error = lookupnumber2("123", [{"123", "unknown::ft@example.org"}]),

    autotest:mark(?LINE, "lookupnumber2/2 - 6"),
    %% test valid rewrite with homedomain result
    LNURL6 = sipurl:parse("sips:ft@" ++ MyHostname),
    {proxy, LNURL6} = lookupnumber2("123", [{"123", "sips:ft@" ++ MyHostname}]),

    autotest:mark(?LINE, "lookupnumber2/2 - 7"),
    %% test valid rewrite, with default protocol and homedomain result
    LNURL7 = sipurl:parse("sip:ft@" ++ MyHostname),
    {proxy, LNURL7} = lookupnumber2("123", [{"123", "ft@" ++ MyHostname}]),

    autotest:mark(?LINE, "lookupnumber2/2 - 8"),
    %% test that we get 'proxy' with homedomain
    LNURL8 = sipurl:parse("sips:ft@homedomain.example.org"),
    {proxy, LNURL8} = lookupnumber2("123", [{"123", "sips:ft@homedomain.example.org"}]),


    %% test lookupregexproute2(Input, Routes)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "lookupregexproute2/2 - 1.0"),

    LRR_SortRoutes = [#regexproute{regexp = "^sip:4000@.*",
				   flags = [{priority, 200}],
				   class = permanent,
				   expire = never,
				   address = "sip:prio200@example.org"},
		      #regexproute{regexp = "^sip:4000@.*",
				   flags = [{priority, 100}],
				   class = permanent,
				   expire = never,
				   address = "sip:prio100@example.org"}
		      ],

    autotest:mark(?LINE, "lookupregexproute2/2 - 1.1"),
    %% test that we use the route with highest priority if we have two matching
    LRR_URL11 = sipurl:parse("sip:prio200@example.org"),
    {proxy, LRR_URL11} = lookupregexproute2("sip:4000@example.org", LRR_SortRoutes),

    autotest:mark(?LINE, "lookupregexproute2/2 - 1.2"),
    %% test non-matching input
    none = lookupregexproute2("foo", LRR_SortRoutes),

    LRR_Now = util:timestamp(),
    autotest:mark(?LINE, "lookupregexproute2/2 - 2.0"),
    LRR_Expire = [#regexproute{regexp = "^sip:testexpire@.*",
			       flags = [{priority, 200}],
			       class = permanent,
			       expire = 0,
			       address = "sip:matches-but-expired@example.org"},
		  #regexproute{regexp = "^sip:testexpire@.*",
			       flags = [{priority, 100}],
			       class = permanent,
			       expire = LRR_Now + 20,
			       address = "sip:matches-not-expired@example.org"}
		 ],

    autotest:mark(?LINE, "lookupregexproute2/2 - 2.1"),
    %% test that we don't pick the one with priority 200 because it is expired
    LRR_URL21 = sipurl:parse("sip:matches-not-expired@example.org"),
    {proxy, LRR_URL21} = lookupregexproute2("sip:testexpire@example.org", LRR_Expire),

    autotest:mark(?LINE, "lookupregexproute2/2 - 3.0"),
    %% test with entrys without priority flag (no priority is worst)
    LRR_RRoute3 = #regexproute{regexp = "^sip:4000@.*",
			       class = permanent,
			       expire = never
			      },
    LRR_SortRoutes3 = [LRR_RRoute3#regexproute{flags = [],
					       address = "sip:noprio@example.org"
					      },
		       LRR_RRoute3#regexproute{flags = [{priority, 100}],
					       address = "sip:prio100@example.org"
					      },
		       LRR_RRoute3#regexproute{flags = [{priority, 200}],
					       address = "sip:prio200@example.org"
					      },
		       LRR_RRoute3#regexproute{flags = [],
					       address = "sip:noprio2@example.org"
					      }
		      ],

    autotest:mark(?LINE, "lookupregexproute2/2 - 2.1"),
    %% test that we use the route with highest priority if we have two matching
    LRR_URL3_1 = sipurl:parse("sip:prio200@example.org"),
    {proxy, LRR_URL3_1} = lookupregexproute2("sip:4000@example.org", LRR_SortRoutes3),


    %% is_same_outbound_client_without_path(Locations, URL)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_same_outbound_client_without_path/2 - 1"),
    %% test with a single location
    IsSame_URL = sipurl:parse("sip:ft@example.org"),
    IsSame_Location1 = #siplocationdb_e{instance = "<urn:test:1>",
					flags = []
				       },
    IsSame_Locations1 = [IsSame_Location1],
    true = is_same_outbound_client_without_path(IsSame_Locations1, IsSame_URL),

    autotest:mark(?LINE, "is_same_outbound_client_without_path/2 - 2"),
    %% test with two simple (equal) locations
    IsSame_Locations2 = [IsSame_Location1, IsSame_Location1],
    true = is_same_outbound_client_without_path(IsSame_Locations2, IsSame_URL),

    autotest:mark(?LINE, "is_same_outbound_client_without_path/2 - 3"),
    %% test with one location without instance
    IsSame_Locations3 = [IsSame_Location1,
			 IsSame_Location1#siplocationdb_e{instance = []}
			],
    false = is_same_outbound_client_without_path(IsSame_Locations3, IsSame_URL),

    autotest:mark(?LINE, "is_same_outbound_client_without_path/2 - 4"),
    %% test with non-me Path
    IsSame_Locations4 = [IsSame_Location1,
                         IsSame_Location1#siplocationdb_e{flags = [{path, ["<notme;lr>"]}]}
                        ],
    false = is_same_outbound_client_without_path(IsSame_Locations4, IsSame_URL),

    autotest:mark(?LINE, "is_same_outbound_client_without_path/2 - 5"),
    %% test with yes-me Path
    IsSame_PathMe = siprequest:construct_record_route(IsSame_URL#sipurl.proto),
    IsSame_Locations5 = [IsSame_Location1,
                         IsSame_Location1#siplocationdb_e{flags = [{path, [IsSame_PathMe]}]}
                        ],
    true = is_same_outbound_client_without_path(IsSame_Locations5, IsSame_URL),


    %% lookupuser_multiple_locations(Locations, URL)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "lookupuser_multiple_locations/2 - 0"),
    LMult_LDBE_SocketId1 = #locationdb_socketid{node  = node(),
						id    = #ob_id{proto = yxa_test,
							       id    = erlang:now()
							      },
						proto = udp,
						addr  = "192.0.2.1",
						port  = 1
					       },
    LMult_LDBE_SocketId2 = #locationdb_socketid{node  = node(),
						id    = #ob_id{proto = yxa_test,
							       id    = erlang:now()
							      },
						proto = tcp,
						addr  = "192.0.2.2",
						port  = 2
					       },
    LMult_Locations1 = [#siplocationdb_e{instance = "<urn:test:17>",
					 flags    = [{socket_id, LMult_LDBE_SocketId1}]
					},
			#siplocationdb_e{instance = "<urn:test:17>",
					 flags    = [{socket_id, LMult_LDBE_SocketId2}]
					}
		       ],
    LMult_URL1 = sipurl:parse("sip:ft@example.org"),

    autotest:mark(?LINE, "lookupuser_multiple_locations/2 - 1.1"),
    %% test with valid local Outbound sockets
    {proxy, [LMult_Dst1, LMult_Dst2]} = lookupuser_multiple_locations(LMult_Locations1, LMult_URL1),

    autotest:mark(?LINE, "lookupuser_multiple_locations/2 - 1.2"),
    %% verify
    #sipdst{proto = udp, addr = "192.0.2.1", port = 1} = LMult_Dst1,
    #sipdst{proto = tcp, addr = "192.0.2.2", port = 2} = LMult_Dst2,

    autotest:mark(?LINE, "lookupuser_multiple_locations/2 - 2"),
    %% test with invalid local Outbound socket
    autotest:store_unit_test_result(?MODULE, {sipsocket_test, get_specific_socket}, {error, "testing"}),
    {response, 430, "Flow Failed"} = lookupuser_multiple_locations(LMult_Locations1, LMult_URL1),


    %% lookup_result_to_str(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "lookup_result_to_str/1 - 1"),
    %% test tuple with URL
    "{relay,\"(sipurl) sip:ft@example.net\"}" = lookup_result_to_str({relay, sipurl:parse("sip:ft@example.net")}),

    autotest:mark(?LINE, "lookup_result_to_str/1 - 2"),
    %% test tuple with atom
    "{proxy,route}" = lookup_result_to_str({proxy, route}),

    autotest:mark(?LINE, "lookup_result_to_str/1 - 3"),
    %% test unknown
    "[test]" = lookup_result_to_str([test]),


    %% Mnesia dependant tests
    %%--------------------------------------------------------------------


    autotest:mark(?LINE, "Mnesia setup - 0"),

    phone:test_create_table(),
    database_gruu:test_create_table(),
    database_regexproute:test_create_table(),

    case mnesia:transaction(fun test_mnesia_dependant_functions/0) of
	{aborted, ok} ->
	    ok;
	{aborted, Res} ->
	    {error, Res}
    end.


test_mnesia_dependant_functions() ->

    %% lookupuser(URL)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "lookupuser/1 - 0"),
    LookupURL1 = sipurl:new([{user, "testuser1"},
			     {host, "__test__.example.org"}
			    ]),

    LookupURL2 = sipurl:new([{user, "testuser2"},
			     {host, "__test__.example.org"}
			    ]),

    autotest:mark(?LINE, "lookupuser/1 - 1"),
    %% look for user that does not exist
    nomatch = lookupuser(LookupURL1),

    %% XXX not testing rest of this function yet because it requires sipuserdb_mnesia
    %% to be used (for us to be able to predict the results that is).

    %% lookupuser_get_locations(Users, URL)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "lookupuser_get_locations/2 - 1"),
    %% test with bad (unknown) user
    none = lookupuser_get_locations(["__test_user_dont_exist__"], LookupURL2),


    autotest:mark(?LINE, "lookupuser_get_locations/2 - 2.0"),
    %% test that lookup_regexproute is called when user has no known locations
    LGL_Rewritten2_Str = "sip:rewritten@test19119.example.com",
    LGL_Rewritten2_URL = sipurl:parse(LGL_Rewritten2_Str),
    {atomic, ok} = database_regexproute:insert(".*@__test__.example.org$", [], dynamic,
					       util:timestamp() + 20, LGL_Rewritten2_Str),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 2.1"),
    {proxy, LGL_Rewritten2_URL} = lookupuser_get_locations(["__test_user_dont_exist__"], LookupURL2),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 2.2"),
    %% clean up
    {atomic, ok} = database_regexproute:delete(".*@__test__.example.org$", [], dynamic,
					       util:timestamp() + 20, LGL_Rewritten2_Str),


    autotest:mark(?LINE, "lookupuser_get_locations/2 - 3.0"),
    %% test with a single registered contact, no Outbound and no Path
    LGL_Contact3_URL = sipurl:parse("sip:ft@192.0.2.133"),
    LGL_Username3 = "__test_user_LGL_3__",

    {atomic, ok} = phone:insert_purge_phone(LGL_Username3, [], static, never,
					    LGL_Contact3_URL, [], 1, []),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 3.1"),
    {proxy, LGL_Contact3_URL} = lookupuser_get_locations([LGL_Username3], LookupURL1),


    autotest:mark(?LINE, "lookupuser_get_locations/2 - 4.0"),
    %% test with a single registered contact, Path matching me
    LGL_Contact4_URL = sipurl:parse("sip:ft@192.0.2.144;foo=bar"),
    LGL_Path4 = siprequest:construct_record_route(LGL_Contact4_URL#sipurl.proto),
    LGL_Username4 = "__test_user_LGL_4__",

    {atomic, ok} = phone:insert_purge_phone(LGL_Username4, [{path, [LGL_Path4]}], static, never,
					    LGL_Contact4_URL, [], 1, []),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 4.1"),
    {proxy, LGL_Contact4_URL} = lookupuser_get_locations([LGL_Username4], LookupURL1),


    autotest:mark(?LINE, "lookupuser_get_locations/2 - 5.0"),
    %% test with a single registered contact, Path matching me and one more entry
    LGL_Contact5_URL = sipurl:parse("sip:ft@192.0.2.155;foo=bar"),
    LGL_Path5 = siprequest:construct_record_route(LGL_Contact5_URL#sipurl.proto),
    LGL_Path5_2 = "<sip:__test__@__test_test__.example.org;lr>",
    LGL_Username5 = "__test_user_LGL_5__",

    {atomic, ok} = phone:insert_purge_phone(LGL_Username5, [{path, [LGL_Path5, LGL_Path5_2]}], static, never,
					    LGL_Contact5_URL, [], 1, []),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 5.1"),
    {proxy, {with_path, LGL_Contact5_URL, [LGL_Path5_2]}} = lookupuser_get_locations([LGL_Username5], LookupURL1),


    autotest:mark(?LINE, "lookupuser_get_locations/2 - 6.0"),
    %% test with a single registered contact, Path pointing at some other host only
    LGL_Contact6_URL = sipurl:parse("sip:ft@192.0.2.154;foo=bar"),
    LGL_Path6 = "<sip:__test__@__test_test__.example.org;lr>",
    LGL_Username6 = "__test_user_LGL_6__",

    {atomic, ok} = phone:insert_purge_phone(LGL_Username6, [{path, [LGL_Path6]}], static, never,
					    LGL_Contact6_URL, [], 1, []),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 6.1"),
    {proxy, {with_path, LGL_Contact6_URL, [LGL_Path6]}} = lookupuser_get_locations([LGL_Username6], LookupURL1),


    autotest:mark(?LINE, "lookupuser_get_locations/2 - 9"),
    %% clean up
    {atomic, ok} = phone:delete_phone_for_user(LGL_Username3, static),
    {atomic, ok} = phone:delete_phone_for_user(LGL_Username4, static),
    {atomic, ok} = phone:delete_phone_for_user(LGL_Username5, static),
    {atomic, ok} = phone:delete_phone_for_user(LGL_Username6, static),


    %% Outbound TESTS

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 10.0"),
    %% test with Outbound socket on other node
    LGL_Contact10_URL = sipurl:parse("sip:ft@192.0.2.212"),
    LGL_Username10 = "__test_user_LGL_10__",
    LGL_LDBSocketId10 = #locationdb_socketid{node = 'othernode@nowhere',
					     id   = 1
					    },

    {atomic, ok} = phone:insert_purge_phone(LGL_Username10, [{socket_id, LGL_LDBSocketId10}],
					    static, never, LGL_Contact10_URL, [], 1, []),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 10.1"),
    {proxy, LGL_Contact10_URL} = lookupuser_get_locations([LGL_Username10], LookupURL1),


    autotest:mark(?LINE, "lookupuser_get_locations/2 - 11.0"),
    %% test with Outbound socket to this node but no longer available
    LGL_Contact11_URL = sipurl:parse("sip:ft@192.0.2.212"),
    LGL_Username11 = "__test_user_LGL_11__",
    LGL_SocketId11 = #ob_id{proto = yxa_test,
			    id = 1
			   },
    LGL_LDBSocketId11 = #locationdb_socketid{node = node(),
					     id   = LGL_SocketId11
					    },
    autotest:store_unit_test_result(?MODULE, {sipsocket_test, get_specific_socket}, {error, "testing"}),

    {atomic, ok} = phone:insert_purge_phone(LGL_Username11, [{socket_id, LGL_LDBSocketId11}],
					    static, never, LGL_Contact11_URL, [], 1, []),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 11.1"),
    {response, 430, _} = lookupuser_get_locations([LGL_Username11], LookupURL1),

    autotest:mark(?LINE, "lookupuser_get_locations/2 - 11.2"),
    %% now test with socket available
    autotest:clear_unit_test_result(?MODULE, {sipsocket_test, get_specific_socket}),
    LGL_SipSocket11 = sipsocket:get_specific_socket(LGL_SocketId11),
    {proxy, [#sipdst{proto = undefined,
		     addr = undefined,
		     port = undefined,
		     uri = LGL_Contact11_URL,
		     socket = LGL_SipSocket11
		    }
	     ]} = lookupuser_get_locations([LGL_Username11], LookupURL1),




    autotest:mark(?LINE, "lookupuser_get_locations/2 - 19"),
    %% clean up
    {atomic, ok} = phone:delete_phone_for_user(LGL_Username10, static),
    {atomic, ok} = phone:delete_phone_for_user(LGL_Username11, static),


    mnesia:abort(ok).
