-module(lookup).

-export([lookupregexproute/1,
	 lookupuser/1,
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
	 get_remote_party_number/2,
	 format_number_for_remote_party_id/3,
	 get_remote_party_name/2
	]).

-include("database_regexproute.hrl").
-include("siprecords.hrl").

lookupregexproute(User) ->
    Routes = database_regexproute:list(),
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

lookupuser(URL) when record(URL, sipurl) ->
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

lookup_url_to_locations(URL) when record(URL, sipurl) ->
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
    
lookup_url_to_addresses(sipuserdb_mnesia, URL) when record(URL, sipurl) ->
    User = URL#sipurl.user,
    % sipuserdb_mnesia is extra liberal with usernames
    Standard = lookup_url_to_addresses(lookup, URL),
    Addr1 = local:url2mnesia_userlist(URL),
    Addr2 = case util:isnumeric(User) of
	true -> [User];
	_ -> []
    end,	
    lists:append([Standard, Addr1, Addr2]);
lookup_url_to_addresses(_Src, URL) when record(URL, sipurl) ->
    % Make a list of all possible addresses we
    % can create out of this URL
    Tel = local:canonify_numberlist([URL#sipurl.user]),
    lists:append([sipurl:print(sipurl:set([{pass, none}, {port, none}, {param, []}], URL))], Tel).

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
	    logger:log(error, "Lookup: lookup_addresses_to_users: unknown result from"
		       " local:get_users_for_addresses_of_record(~p) : ~p",
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
    case sipserver:get_env(appserver, none) of
	none ->
	    logger:log(debug, "Lookup: Requested to provide appserver for ~p, but no appserver configured! Returning 480 Temporarily Unavailable.",
			[Key]),
	    {response, 480, "Temporarily Unavailable"};
	AppServer ->
	    case sipurl:parse_url_with_default_protocol("sip", AppServer) of
		URL when record(URL, sipurl), URL#sipurl.user == none, URL#sipurl.pass == none ->
		    {forward, URL};
		_ ->
		    logger:log(error, "Failed parsing configured appserver ~p", [AppServer]),
		    {response, 500, "Server Internal Error"}
	    end
    end.

lookupdefault(URL) when record(URL, sipurl) ->
    case homedomain(URL#sipurl.host) of
	true ->
	    logger:log(debug, "Lookup: Cannot default-route request to a local domain (~s), aborting", 
		       [URL#sipurl.host]),
	    none;
        _ ->
	    DefaultRoute = sipserver:get_env(defaultroute, none),
	    case DefaultRoute of
		none ->
		    logger:log(debug, "Lookup: No default route - dropping request"),
		    {response, 500, "Can't route request"};	%% XXX is 500 the correct error-code?
		Hostname ->
		    {Host, Port} = sipparse_util:parse_hostport(Hostname),
		    NewURI = sipurl:new([{user, URL#sipurl.user}, {pass, none}, {host, Host}, {port, Port}]), 
		    

		    logger:log(debug, "Lookup: Default-routing to ~s", [sipurl:print(NewURI)]),
		    %% XXX we should preserve the Request-URI by proxying this as a loose router.
		    %% It is almost useless to only preserve the User-info IMO. We can do this
		    %% by returning {forward, Proto, Host, Port} instead.
		    {proxy, NewURI}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: lookuppotn/1
%% Description: Look Up Plain Old Telephone Number. Figures out where
%%              to route a numerical destination. First we try to
%%              rewrite it to E164 and do ENUM lookup, and if that
%%              fails, lookuppstn() on it. Then we try our fallback
%%              numerical route matching, lookupnumber().
%% Returns: {proxy, URL}          |
%%          {relay, URL}          |
%%          none
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
%% Function: lookupenum/1
%% Description: Does ENUM resolving on an E164 number. If the input
%%              number is not an E164 number, it is converted first.
%% Returns: {proxy, URL}          |
%%          {relay, URL}          |
%%          none
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
		    logger:log(debug, "Lookup: ENUM lookup resulted in remote URL ~p, relaying", [sipurl:print(URL)]),
		    {relay, URL};
		NewE164 == error ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in a homedomain but not E.164 URL ~p, proxying", [sipurl:print(URL)]),
		    {proxy, URL};
		SameE164 == true ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in a homedomain (~p) and the same E.164 number (~p == ~p), avoiding loop",
			       [E164Host, E164User, "+" ++ E164]),
		    none;
		true ->
		    logger:log(debug, "Lookup: ENUM lookup resulted in homedomain E.164 URL ~p, proxying", [sipurl:print(URL)]),
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
%% Function: lookuppstn/1
%% Description: Rewrites a number to a PSTN URL using the e164_to_pstn
%%              configuration regexp. If the number is not E164, it
%%              is converted using rewrite_potn_to_e164() first.
%% Returns: {proxy, URL}          |
%%          {relay, URL}          |
%%          none                  |
%%          error
%%--------------------------------------------------------------------
lookuppstn("+" ++ E164) ->
    case util:isnumeric(E164) of
	true ->
	    case util:regexp_rewrite("+" ++ E164, sipserver:get_env(e164_to_pstn, [])) of
		nomatch ->
		    none;
		PSTN ->
		    logger:log(debug, "Rewrite: ~s to PSTN URL ~p", ["+" ++ E164, PSTN]),
		    case sipurl:parse_url_with_default_protocol("sip", PSTN) of
			error ->
			    logger:log(error, "Lookup: Failed parsing result of rewrite ~p using 'e164_to_pstn'",
				       ["+" ++ E164]),
			    none;
			URL ->
			    {proxy, URL}
		    end
	    end;
	_ ->
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
%% Function: lookupnumber/1
%% Description: Check if there are any numerical matching rules that
%%              apply (configured regexp 'number_to_pstn'). Called by
%%              lookuppotn/1 and lookuppstn/1 when the input number is
%%              not rewriteable to a E164 number.
%% Returns: {proxy, URL}          |
%%          {relay, URL}          |
%%          none                  |
%%          error
%%--------------------------------------------------------------------
lookupnumber(Number) ->
    %% Check if Number is all digits
    case util:isnumeric(Number) of
	true ->
	    %% Try to rewrite Number using configured regexp 'number_to_pstn'
	    case util:regexp_rewrite(Number, sipserver:get_env(number_to_pstn, [])) of
		[] ->
		    logger:log(error, "Lookup: Failed rewriting number ~p using regexp 'number_to_pstn'"),
		    error;
		Res when list(Res) ->
		    %% Check to see if what we got is a parseable URL
		    case sipurl:parse_url_with_default_protocol("sip", Res) of
			URL when record(URL, sipurl) ->
			    %% Check if it is a local URL or a remote
			    case homedomain(URL#sipurl.host) of
				true ->
				    {proxy, URL};
				_ ->
				    {relay, URL}
			    end;
			_ ->
			    logger:log(error, "Lookup: Rewrite of number ~p did not result in a parseable URL : ~p",
				       [Number, Res]),
			    error
		    end;
		nomatch ->
		    %% Regexp did not match
		    none;
		Unknown ->
		    %% Regexp rewrite failed
		    logger:log(error, "Lookup: Failed rewriting number ~p using regexp 'number_to_pstn', result : ~p",
			       [Unknown]),
		    error
	    end;
	_ ->
	    %% Number was not numeric
	    error
    end.
    
%%--------------------------------------------------------------------
%% Function: rewrite_potn_to_e164/1
%% Description: Rewrite a number to an E164 number using our local
%%              numbering plan (configured regexp 'internal_to_e164').
%% Returns: Number                | (Number is a list starting with +)
%%          error
%%--------------------------------------------------------------------
rewrite_potn_to_e164("+" ++ E164) ->
    case util:isnumeric(E164) of
	true ->
	    "+" ++ E164;
	_ ->
	    error
    end;
rewrite_potn_to_e164(Number) when list(Number) ->
    case util:isnumeric(Number) of
	true ->
	    case util:regexp_rewrite(Number, sipserver:get_env(internal_to_e164, [])) of
		"+" ++ E164 ->
		    rewrite_potn_to_e164("+" ++ E164);
		_ ->
		    error
	    end;
	_ ->
	    error
    end;
rewrite_potn_to_e164(_) ->
    error.

isours(URL) when record(URL, sipurl) ->
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

%%--------------------------------------------------------------------
%% Function: is_request_to_this_proxy(Request)
%%           Request = request record()
%% Descrip.: Check if a request is destined for this proxy. Not for a
%%           domain handled by this proxy, but for this proxy itself.
%% Returns : true | false
%%--------------------------------------------------------------------
is_request_to_this_proxy(Request) when record(Request, request) ->
    {Method, URL, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    case local:homedomain(URL#sipurl.host) of
	true ->
	    is_request_to_this_proxy2(Method, URL, Header);
	false ->
	    false
    end.

%% is_request_to_this_proxy2/3 is a subfunction of is_request_to_this_proxy/1,
%% called if the URI host matches one of our hostnames. Return true if there
%% is no userpart in the URI, or if the method is OPTIONS and Max-Forwards is
%% less than one. This procedure is from RFC3261 #11 Querying for Capabilities.
is_request_to_this_proxy2(_Method, #sipurl{user=User}=URL, _Header) when is_record(URL, sipurl), User == none ->
    true;
is_request_to_this_proxy2("OPTIONS", URL, Header) when is_record(URL, sipurl) ->
    %% RFC3261 # 11 says a proxy that receives an OPTIONS request with a Max-Forwards less than one
    %% MAY treat it as a request to the proxy.
    MaxForwards =
        case keylist:fetch("Max-Forwards", Header) of
            [M] ->
                lists:min([255, list_to_integer(M) - 1]);
            [] ->
                70
        end,
    if
        MaxForwards < 1 ->
            logger:log(debug, "Routing: Request is OPTIONS and Max-Forwards < 1, treating it as a request to me."),
            true;
        true ->
            false
    end;
is_request_to_this_proxy2(_, URL, _) when record(URL, sipurl) ->
    false.

homedomain(Domain) ->
    case util:casegrep(Domain, sipserver:get_env(homedomain, [])) of
	true ->
	    true;
	_ ->
	    %% Domain did not match configured sets of homedomain, check against list
	    %% of hostnames and also my IP address
	    HostnameList = lists:append(sipserver:get_env(myhostnames, []), [siphost:myip_list()]),
	    util:casegrep(Domain, HostnameList)
    end.

%%--------------------------------------------------------------------
%% Function: get_remote_party_number(URL, DstHost)
%%           URL     = sipurl record(), From:
%%           DstHost = term(), chosen destination for request
%% Descrip.: This function is used by the pstnproxy to provide a PSTN
%%           gateway with usefull caller-id information. PSTN networks
%%           typically gets upset if the "A-number" (calling party) is
%%           a SIP URL. Different gateways might want the number
%%           formatted differently, thus the DstHost parameter (a TSP
%%           gateway to PSTN might only handle E.164 numbers, while a
%%           PBX might be expecting only a 4-digit extension number).
%% Returns : {ok, Number} |
%%           none
%%           Number = string()
%%--------------------------------------------------------------------
get_remote_party_number(URL, DstHost) when is_record(URL, sipurl), is_list(DstHost) ->
    case local:get_users_for_url(URL) of
	[User] ->
	    case local:get_telephonenumber_for_user(User) of
		Number when is_list(Number) ->
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
	Users when is_list(Users) ->
	    logger:log(debug, "Lookup: Multiple users match address ~p, can't get telephone number",
		       [sipurl:print(URL)]),
	    none;
	Unknown ->
	    logger:log(error, "Lookup: Unexpected results from local:get_users_for_url() for URL ~p in get_remote_party_number: ~p",
		       [sipurl:print(URL), Unknown]),
	    none
    end.

%%--------------------------------------------------------------------
%% Function: format_number_for_remote_party_id(Number, URL, DstHost)
%%           Number  = string(), the number to format
%%           URL     = sipurl record(), From:
%%           DstHost = term(), destination for request
%% Descrip.: Hook for the actual formatting once
%%           get_remote_party_number/2 has found a number to be
%%           formatted.
%% Returns : {ok, Number}
%%           Number = string()
%%--------------------------------------------------------------------
format_number_for_remote_party_id(Number, ToURI, _DstHost) when is_list(Number),
								is_record(ToURI, sipurl) ->
    case rewrite_potn_to_e164(Number) of
	error ->
	    none;
	Res ->
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
	    logger:log(debug, "Lookup: Got more than one name back for number ~p. Since I have no way " ++
		       "of choosing a name from a list, I'm not going to try.", [Key]),
	    none;
	Unknown ->
	    logger:log(error, "Lookup: Failed to get name for remote party ~p, unexpected result from lookup_tel2name : ~p",
		    		[Key, Unknown]),
	    none
    end;
get_remote_party_name(Key, URI) ->
    logger:log(error, "Lookup: Could not get remote party name for non-list argument ~p or non-URL argument ~p", [Key, URI]),
    none.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

