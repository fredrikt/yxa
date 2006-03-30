%%%-------------------------------------------------------------------
%%% File    : lookup.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: Varios lookup functions. Mainly routing logic for our
%%%           three applications incomingproxy, pstnproxy and
%%%           appserver. Most of these functions are called through
%%%           functions in local.erl with the same name, so if you
%%%           want to make them return different values than the
%%%           defaults in this file, make a local.erl specific for
%%%           your domain.
%%%
%%% Created : 20 Mar 2003 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(lookup).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([lookupregexproute/1,
	 lookupuser/1,
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

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("database_regexproute.hrl").
-include("siprecords.hrl").


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: lookupregexproute(Input)
%%           Input = string(), what we will match against the regexps
%% Descrip.: See if we have a regexp that matches Input in the Mnesia
%%           regexp route table. If we find one, we return a proxy
%%           tuple with the resulting destination. The regexps in the
%%           database have a priority field, where higher priority
%%           is better.
%% Returns : {proxy, URL} |
%%           none
%%           URL = sipurl record()
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
%% Function: lookupuser(URL)
%%           URL = sipurl record()
%% Descrip.: The main 'give me a set of locations for one of our
%%           users' function that incomingproxy uses, when it
%%           determines that a request is for one of it's homedomains.
%% Returns : {ok, Users, Res} |
%%           nomatch              No such user
%%           Users = list() of string(), usernames that matched this URL
%%           Res   = {proxy, URL}               |
%%                   {relay, URL}               |
%%                   {forward, URL}             |
%%                   {response, Status, Reason} |
%%                   none    - The user was found but has no locations
%%                             registered
%%--------------------------------------------------------------------
lookupuser(URL) when is_record(URL, sipurl) ->
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
	    Res = case local:user_has_cpl_script(User, incoming) of
		      true ->
			  %% let appserver handle requests for users with CPL scripts
			  case local:lookupappserver(URL) of
			      {forward, AppS} ->
				  logger:log(debug, "Lookup: User ~p has a CPL script, forwarding to appserver : ~p",
					     [User, sipurl:print(AppS)]),
				  {forward, AppS};
			      {response, Status, Reason} ->
				  {response, Status, Reason};
			      _ ->
				  logger:log(debug, "Lookup: User ~p has a CPL script, but I could not find an appserver",
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
    %% check if more than one location was found.
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
	    %% A single location was found in the location database (after removing any unsuitable ones)
	    BestLocation = siplocation:to_url(Location),
	    case lists:keysearch(path, 1, Location#siplocationdb_e.flags) of
		{value, {path, Path}} ->
		    {proxy, {with_path, BestLocation, Path}};
		false ->
		    {proxy, BestLocation}
	    end;
	[Location | _] when is_record(Location, siplocationdb_e) ->
	    %% More than one location registered for this address, check for appserver...
	    %% (appserver is the program that handles forking of requests)
	    local:lookupappserver(URL)
    end.

%%--------------------------------------------------------------------
%% Function: lookupuser_locations(Users, URL)
%%           Users = list() of string(), SIP users to fetch locations
%%                                       of
%%           URL   = sipurl record(), the Request-URI
%% Descrip.: Return all locations for a list of users that is suitable
%%           given a Request-URI. By suitable, we mean that we filter
%%           out SIP locations if Request-URI was SIPS, unless this
%%           proxy is configured not to.
%% Returns : Locations = list() of siplocationdb_e record()
%%--------------------------------------------------------------------
lookupuser_locations(Users, URL) when is_list(Users), is_record(URL, sipurl) ->
    Locations1 = local:get_locations_for_users(Users),
    Locations = local:prioritize_locations(Users, Locations1),
    local:remove_unsuitable_locations(URL, Locations).
    
%%--------------------------------------------------------------------
%% Function: remove_unsuitable_locations(URL, Locations)
%%           URL      = sipurl record(), Request-URI of request
%%           Location = list() of siplocationdb_e record()
%% Descrip.: Apply local policy for what locations are good to use for
%%           a particular Request-URI. The default action we do here
%%           is to remove non-SIPS locations if the Request-URI is
%%           SIPS, unless we are configured not to.
%% Returns : list() of siplocationdb_e record()
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
%% Function: lookup_url_to_locations(URL)
%%           URL = sipurl record()
%% Descrip.: Turn an URL into a set of locations. The URL might map to
%%           more than one user, in which case the locations for all
%%           matched users are returned. Locations is returned
%%           according to the priority values they have in the
%%           location database.
%% Returns : {proxy, URL} |
%%           nomatch      |
%%           throw({siperror, ...})
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
%% Function: lookup_url_to_addresses(Src, URL)
%%           Src = atom(), who is asking
%%           URL = sipurl record()
%% Descrip.: Make up a bunch of possible userdb keys from an URL.
%%           Since our userdbs store different addresses implicitly
%%           sometimes, we do this mess to make sure we find one or
%%           more users for requests destined to an URL.
%% Returns : list() of string()
%%--------------------------------------------------------------------
lookup_url_to_addresses(Src, #sipurl{proto="sips"}=URL) ->
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
%% Function: lookup_addresses_to_users(Addresses)
%%           Addresses = list() of term()
%% Descrip.: Get a list of users that match an input list of
%%           addresses.
%% Returns : list() of string(), list of usernames or empty list
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
%% Function: lookup_address_to_users(Address)
%%           Address = term()
%% Descrip.: Get a list of users that match a single address.
%% Returns : list() of string(), list of usernames or empty list
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
%% Function: lookupappserver(Key)
%%           Key = sipurl record()
%% Descrip.: Get the configured appserver to use for Key. Used in
%%           incomingproxy.
%% Returns : {forward, URL}             |
%%           {response, Status, Reason}
%%           URL    = sipurl record()
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%% Note    : XXX We need to make sure we don't return a SIP URI if Key
%%           was SIPS. For this we need a general function
%%           ensure_is_tls_protected_uri(ReqURI, URI)' or similar.
%%--------------------------------------------------------------------
lookupappserver(Key) when is_record(Key, sipurl) ->
    case yxa_config:get_env(appserver) of
	{ok, URL} when is_record(URL, sipurl), URL#sipurl.user == none, URL#sipurl.pass == none ->
	    {forward, URL};
	none ->
	    logger:log(error, "Lookup: Requested to provide appserver for ~p, but no appserver configured! "
		       "Responding '480 Temporarily Unavailable (no appserver configured)'.", [sipurl:print(Key)]),
	    %% XXX perhaps the '(no appserver configured)' should either not be disclosed, or
	    %% included in a Reason: header instead of the reason phrase
	    {response, 480, "Temporarily Unavailable (no appserver configured)"}
    end.

%%--------------------------------------------------------------------
%% Function: lookupdefault(URL)
%%           URL = sipurl record()
%% Descrip.: Get the configured default route. Used in incomingproxy.
%% Returns : {proxy, DefaultRoute} |
%%           {response, Status, Reason}
%%           DefaultRoute = sipurl record()
%%           Status       = integer(), SIP status code
%%           Reason       = string(), SIP reason phrase
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
%% Function: lookuppotn(Number)
%%           Number = string()
%% Descrip.: Look Up Plain Old Telephone Number. Figures out where
%%           to route a numerical destination. First we try to
%%           rewrite it to E.164 and do ENUM lookup, and if that
%%           fails, lookuppstn() on it. Then we try our fallback
%%           numerical route matching, lookupnumber(). Used in both
%%           incomingproxy and pstnproxy.
%% Returns : {proxy, URL} |
%%           {relay, URL} |
%%           none
%%           URL = sipurl record()
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
%% Function: lookupenum(Number)
%%           Number = string()
%% Descrip.: Does ENUM resolving on an E164 number. If the input
%%           number is not an E164 number, it is converted first.
%% Returns : {proxy, URL} |
%%           {relay, URL} |
%%           none
%%           URL = sipurl record()
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
	    %% a remote URL with the E164 number as uesrname is OK.
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
%% Function: lookuppstn(Number)
%%           Number = string()
%% Descrip.: Rewrites a number to a PSTN URL using the e164_to_pstn
%%           configuration regexp. If the number is not E164, it
%%           is converted using rewrite_potn_to_e164() first.
%% Returns : {proxy, URL}          |
%%           {relay, URL}          |
%%           none                  |
%%           error
%%           URL = sipurl record()
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
%% Function: lookupnumber(Number)
%%           Number = string()
%% Descrip.: Check if there are any numerical matching rules that
%%           apply (configured regexp 'number_to_pstn'). Called by
%%           lookuppotn/1 and lookuppstn/1 when the input number is
%%           not rewriteable to a E164 number.
%% Returns : {proxy, URL}          |
%%           {relay, URL}          |
%%           none                  |
%%           error
%%           URL = sipurl record()
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
%% Function: rewrite_potn_to_e164(Number)
%%           Number = string()
%% Descrip.: Rewrite a number to an E164 number using our local
%%           numbering plan (configured regexp 'internal_to_e164').
%% Returns : Result |
%%           error
%%           Result = string(), "+" followed by an E.164 number
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
%% Function: isours(URL)
%%           URL = sipurl record()
%% Descrip.: Check if we have a user matching an URL.
%% Returns : true | false
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
%% Function: is_request_to_this_proxy(Request)
%%           Request = request record()
%% Descrip.: Check if a request is destined for this proxy. Not for a
%%           domain handled by this proxy, but for this proxy itself.
%% Returns : true | false
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
%% Function: homedomain(Domain)
%%           Domain = string()
%% Descrip.: Check if Domain is one of our configured homedomains.
%% Returns : true | false
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
%% Function: get_remote_party_number(User, Header, DstHost)
%%           User    = string(), SIP authentication username
%%           Header  = keylist record()
%%           URI     = sipurl record(), outgoing Request-URI
%%           DstHost = term(), chosen destination for request
%% Descrip.: This function is used by the pstnproxy to provide a PSTN
%%           gateway with usefull caller-id information. PSTN networks
%%           typically gets upset if the "A-number" (calling party) is
%%           a SIP URL. Different gateways might want the number
%%           formatted differently, thus the DstHost parameter (a TSP
%%           gateway to PSTN might only handle E.164 numbers, while a
%%           PBX might be expecting only a 4-digit extension number).
%% Returns : {ok, RPI, Number} |
%%           none
%%           RPI    = contact record()
%%           Number = string()
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
		    logger:log(error, "Lookup: Failed to format telephone number for Remote-Party-Id", [Number]),
		    none
	    end;
	nomatch ->
	    logger:log(debug, "Lookup: No telephone number found for user ~p", [User]),
	    none
    end.

%%--------------------------------------------------------------------
%% Function: format_number_for_remote_party_id(Number, Header,
%%                                             DstHost)
%%           Number  = string(), the number to format
%%           Header  = keylist record()
%%           DstHost = term(), destination for request
%% Descrip.: Hook for the actual formatting once
%%           get_remote_party_number/2 has found a number to be
%%           formatted. This default function simply tries to rewrite
%%           the number to E.164. If one or more of your PSTN gateways
%%           wants the Caller-ID information in any other format, then
%%           override this function in local.erl.
%% Returns : {ok, Number} | none
%%           Number = string()
%%--------------------------------------------------------------------
format_number_for_remote_party_id(Number, _Header, _DstHost) when is_list(Number) ->
    case rewrite_potn_to_e164(Number) of
	error ->
	    none;
	Res when is_list(Res) ->
	    {ok, Res}
    end.

%%--------------------------------------------------------------------
%% Function: get_remote_party_name(Key, URI)
%%           Key = string(), number we should turn into a name
%%           URI = term(), destination for request
%% Descrip.: When pstnproxy receives a request from a PSTN gateway,
%%           this function is called to see if we can find a nice
%%           Display Name for the calling party.
%% Returns : {ok, DisplayName} |
%%           none
%%           DisplayName = string()
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


%%====================================================================
%%% Internal functions
%%====================================================================


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
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

    ok.
