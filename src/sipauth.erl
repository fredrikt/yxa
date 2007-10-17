%%%-------------------------------------------------------------------
%%% File    : sipauth.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      SIP authentication functions.
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sipauth).

%%-compile(export_all).

-export([get_response/5,
	 get_response/6,
	 get_nonce/1,
	 get_user_verified/2,
	 get_user_verified_proxy/2,
	 get_challenge/0,
	 can_register/2,
	 pstn_get_user_verified/2,
	 is_allowed_pstn_dst/4,
	 can_use_address/2,
	 can_use_address_detail/2,
	 realm/0,
	 add_x_yxa_peer_auth/5,
	 add_credentials/7,
	 classify_number/2,

	 test/0
	]).


-include("siprecords.hrl").

%% MD5 digest 'formula'
%%
%% A1 = username ":" realm ":" password
%% A2 = Method ":" digest-uri
%% nonce = H(timestamp ":" privatekey)
%% resp = H(H(A1) ":" nonce ":" H(A2))

%%--------------------------------------------------------------------
%% @spec    () -> string()
%%
%% @doc     Return this proxys configured authentication realm, or the
%%          hostname if no realm has been configured.
%% @end
%%--------------------------------------------------------------------
realm() ->
    case yxa_config:get_env(sipauth_realm) of
	{ok, Realm} ->
	    Realm;
	none ->
	    siprequest:myhostname()
    end.

%%--------------------------------------------------------------------
%% @spec    (Timestamp) -> string()
%%
%%            Timestamp = string() "current time in hex"
%%
%% @doc     Create a nonce. Since we have not located any useful
%%          randomness functions in Erlang, and since all proxys that
%%          share authentication realm should be able to use the
%%          responses to the challenges we create here, we use the
%%          current time plus the configured sipauth_password.
%% @end
%%--------------------------------------------------------------------
get_nonce(Timestamp) when is_list(Timestamp) ->
    {ok, Password} = yxa_config:get_env(sipauth_password, ""),
    hex:to(erlang:md5([Timestamp, ":", Password])).

%%--------------------------------------------------------------------
%% @spec    () ->
%%            Challenge
%%
%%            Challenge = {Realm, Nonce, Timestamp}
%%            Realm     = string()
%%            Nonce     = string()
%%            Timestamp = string()
%%
%% @doc     Create a challenge tuple.
%% @end
%%--------------------------------------------------------------------
get_challenge() ->
    Timestamp = hex:to(util:timestamp(), 8),
    {realm(), get_nonce(Timestamp), Timestamp}.

%%--------------------------------------------------------------------
%% @spec    (Nonce, Method, URIstr, User, Password) -> term()
%%
%% @equiv    get_response(Nonce, Method, URIstr, User, Password, realm())
%% @end
%%--------------------------------------------------------------------
get_response(Nonce, Method, URIstr, User, Password) ->
    Realm = realm(),
    get_response(Nonce, Method, URIstr, User, Password, Realm).

%%--------------------------------------------------------------------
%% @spec    (Nonce, Method, URIstr, User, Password, Realm) ->
%%            Response |
%%            none
%%
%%            Nonce    = string()
%%            Method   = string()
%%            URIstr   = string()
%%            User     = string()
%%            Password = string() | nomatch
%%            Realm    = string()
%%
%%            Response = string()
%%
%% @doc     Get the correct response to a challenge, given a nonce,
%%          method, URI, username and password.
%% @end
%%--------------------------------------------------------------------
get_response(_Nonce, _Method, _URIstr, _User, nomatch, _Realm) ->
    %% Password is nomatch - return 'none'
    none;
get_response(Nonce, Method, URIstr, User, Password, Realm) ->
    A1 = hex:to(erlang:md5([User, ":", Realm, ":", Password])),
    A2 = hex:to(erlang:md5([Method, ":", URIstr])),
    hex:to(erlang:md5([A1, ":", Nonce, ":", A2])).

%%--------------------------------------------------------------------
%% @spec    (Number, Regexps) ->
%%            {ok, Class}   |
%%            {ok, unknown} |
%%            {error, E}
%%
%%            Number  = string() | none
%%            Regexps = [{Regexp, Class}]
%%            Regexp  = string()
%%            Class   = atom()
%%
%%            Class = atom()
%%
%% @doc     Search a list of regexps until Number matches the Regexp
%%          and return the Class.
%% @end
%%--------------------------------------------------------------------
classify_number(none, _Regexps) ->
    {ok, unknown};

classify_number(Number, []) when is_list(Number) ->
    {ok, unknown};

classify_number(Number, [{"^+" ++ Regexp, _Class} | Rest]) when is_list(Number) ->
    logger:log(error, "sipauth:classify_number() Skipping invalid regexp ~p (you probably "
	       "forgot to escape the plus char)", ["^+" ++ Regexp]),
    classify_number(Number, Rest);

classify_number(Number, [{Regexp, Class} | Rest]) when is_list(Number), is_list(Regexp), is_atom(Class) ->
    case regexp:first_match(Number, Regexp) of
	{match, _, _} ->
	    {ok, Class};
	nomatch ->
	    classify_number(Number, Rest);
	{error, E} ->
	    logger:log(normal, "Error in regexp ~p: ~p", [Regexp, E]),
	    {error, E}
    end.

%%--------------------------------------------------------------------
%% @spec    (Header, Method) ->
%%            false                 |
%%            {stale, User}         |
%%            {authenticated, User}
%%
%%            Header = #keylist{}
%%            Method = string()
%%
%%            User = string() "SIP authentication username"
%%
%% @doc     Check if there is an Authorization: header in Header and
%%          check if it contains a valid response of a challenge we
%%          supposedly sent out.
%% @end
%%--------------------------------------------------------------------
get_user_verified(Header, Method) ->
    case keylist:fetch('authorization', Header) of
	[] ->
	    logger:log(debug, "Auth: get_user_verified: No Authorization header, returning false"),
	    false;
	AuthHeaders ->
	    AuthRealmMatches = parse_auth_filter_realm(AuthHeaders, realm(), "Authorization"),
	    get_user_verified2(Method, AuthRealmMatches, Header)
    end.

%%--------------------------------------------------------------------
%% @spec    (Header, Method) ->
%%            false                 |
%%            {stale, User}         |
%%            {authenticated, User} 
%%
%%            Header = #keylist{}
%%            Method = string()
%%
%%            User = string() "SIP authentication username"
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Check if there is an Proxy-Authorization: header in Header
%%          and check if it contains a valid response of a challenge
%%          we supposedly sent out. Might throw an {siperror, ...} if
%%          something is wrong with the authorization header. Notes :
%%          XXX we should verify the URI too
%% @end
%%--------------------------------------------------------------------
get_user_verified_proxy(Header, Method) ->
    case keylist:fetch('proxy-authorization', Header) of
	[] ->
	    logger:log(debug, "Auth: get_user_verified_proxy: No Proxy-Authorization header, returning false"),
	    false;
	AuthHeaders ->
	    AuthRealmMatches = parse_auth_filter_realm(AuthHeaders, realm(), "Proxy-Authorization"),
	    get_user_verified2(Method, AuthRealmMatches, Header)
    end.

%%--------------------------------------------------------------------
%% @spec    (Header, Method) ->
%%            false                 |
%%            {stale, User}         |
%%            {authenticated, User} 
%%
%%            Header = #keylist{}
%%            Method = string()
%%
%%            User = string() "SIP authentication username"
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Check if there is an X-YXA-Peer-Auth: header in Header and
%%          check if it authorizes this request. Might throw an
%%          {siperror, ...} if something is wrong with the
%%          authorization header. Notes : XXX we should verify the
%%          URI too
%% @end
%%--------------------------------------------------------------------
get_user_verified_yxa_peer(Header, Method) ->
    case keylist:fetch('x-yxa-peer-auth', Header) of
	[] ->
	    logger:log(debug, "Auth: get_user_verified_yxa_peer: No X-YXA-Peer-Auth header, returning false"),
	    false;
	AuthHeaders ->
	    Realm = realm(),
	    AuthRealmMatches = parse_auth_filter_realm(AuthHeaders, Realm, "X-YXA-Peer-Auth"),
	    get_user_verified_yxa_peer2(Header, Method, AuthRealmMatches, Realm, false)
    end.

get_user_verified_yxa_peer2(Header, Method, [Authorization | T], Realm, _LastRes) ->
    OrigUser = User = dict:fetch("username", Authorization),
    case yxa_config:get_env(x_yxa_peer_auth_secret) of
	{ok, Password} ->
	    Now = util:timestamp(),
	    case do_get_user_verified2(Method, User, OrigUser, Password, Realm, Now, Authorization) of
		false ->
		    %% Look for another Authheader with our realm
		    get_user_verified_yxa_peer2(Header, Method, T, Realm, false);
		{stale, VResUser} ->
		    %% Look for another Authheader with our realm that might _not_ be stale
		    get_user_verified_yxa_peer2(Header, Method, T, Realm, {stale, VResUser});
		VRes ->
		    VRes
	    end;
	none ->
	    logger:log(debug, "Auth: Request has X-YXA-Peer-Auth header for my realm, but I have no configured secret"),
	    false
    end;
get_user_verified_yxa_peer2(_Header, _Method, [], _Realm, LastRes) ->
    logger:log(debug, "Auth: No more auth headers matching my realm, returning last result : ~p", [LastRes]),
    LastRes.

%%get_user_verified2(_Method, ["GSSAPI" ++ _R] = Authheader, _Header) ->
%%    Authorization = sipheader:auth(Authheader),
%%    Info = dict:fetch("info", Authorization),
%%    {_Response, Username} = gssapi:request(Info),
%%    %% XXX this is definately broken! What does gssapi:request() return anyways?
%%    Username,
%%    erlang:fault({error, "GSSAPI code broken and not yet fixed"});

%%--------------------------------------------------------------------
%% @spec    (Method, AuthDicts, Header) ->
%%            {authenticated, User} |
%%            {stale, User}         |
%%            false
%%
%%            Method    = string()
%%            AuthDicts = [dict()] "the authorization data"
%%            Header    = #keylist{}
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Authenticate a request.
%% @end
%%--------------------------------------------------------------------
get_user_verified2(Method, AuthDicts, Header) ->
    get_user_verified2(Method, AuthDicts, Header, false).

get_user_verified2(Method, [Authorization | T], Header, _LastRes) ->
    %% Remember the username the client used
    OrigUser = dict:fetch("username", Authorization),
    %% Canonify username
    User = case local:canonify_authusername(OrigUser, Header) of
	       undefined ->
		   OrigUser;
	       Res when is_list(Res) ->
		   Res
	   end,
    Password = case local:get_password_for_user(User) of
		   nomatch ->
		       nomatch;
		   PRes when is_list(PRes) ->
		       PRes
	       end,
    Realm = realm(),
    Now = util:timestamp(),
    case do_get_user_verified2(Method, User, OrigUser, Password, Realm, Now, Authorization) of
	false ->
	    %% Look for another Authheader with our realm
	    get_user_verified2(Method, T, Header, false);
	{stale, VResUser} ->
	    %% Look for another Authheader with our realm that might _not_ be stale
	    get_user_verified2(Method, T, Header, {stale, VResUser});
	VRes ->
	    VRes
    end;
get_user_verified2(_Method, [], _Header, LastRes) ->
    logger:log(debug, "Auth: No more credentials, returning last result : ~p", [LastRes]),
    LastRes.

%% do_get_user_verified2/7 - part of get_user_verified2/3 in order to make it testable
do_get_user_verified2(Method, User, OrigUser, Password, Realm, Now, AuthDict) ->
    Opaque = case dict:find("opaque", AuthDict) of
		 error ->
		     throw({siperror, 400, "Authorization should contain opaque"});
		 {ok, Value} ->
		     Value
	     end,
    AuthURI = dict:fetch("uri", AuthDict),
    Response = dict:fetch("response", AuthDict),
    Nonce2 = get_nonce(Opaque),
    Nonce = dict:fetch("nonce", AuthDict),

    Timestamp = hex:from(Opaque),
    logger:log(debug, "Auth: timestamp: ~p now: ~p", [Timestamp, Now]),
    Response2 = get_response(Nonce2, Method, AuthURI,
			     OrigUser, Password, Realm),
    {ok, AuthTimeValid} = yxa_config:get_env(sipauth_challenge_expiration),
    if
	Password == nomatch ->
	    logger:log(normal, "Auth: Authentication failed for non-existing user ~p", [User]),
	    false;
	Response /= Response2 ->
	    logger:log(debug, "Response ~p /= Response2 ~p", [Response, Response2]),
	    logger:log(normal, "Auth: Authentication failed for user ~p", [User]),
	    false;
	Nonce /= Nonce2 ->
	    logger:log(normal, "Auth: Nonce ~p /= ~p, authentication failed for user ~p", [Nonce, Nonce2, User]),
	    false;
	AuthTimeValid > 0 andalso Timestamp < (Now - AuthTimeValid) ->
	    logger:log(normal, "Auth: Timestamp ~p too old. Now: ~p, authentication failed for user ~p",
		       [Timestamp, Now, User]),
	    {stale, User};
	AuthTimeValid < 0 ->
	    %% In unit test cases, we need a way to get 'stale' authentication
	    logger:log(normal, "Auth: 'sipauth_challenge_expiration' less than zero, treating as stale"),
	    {stale, User};
	Timestamp > Now ->
	    logger:log(normal, "Auth: Timestamp ~p too new. Now: ~p, authentication failed for user ~p",
		       [Timestamp, Now, User]),
	    false;
	true ->
	    logger:log(debug, "Auth: User ~p authenticated", [User]),
	    {authenticated, User}
    end.

%%--------------------------------------------------------------------
%% @spec    (In, Realm, Name) -> [dict()]
%%
%%            In    = [string()] "auth header values"
%%            Realm = string() "this proxys realm"
%%            Name  = string() "description of header we are parsing"
%%
%% @doc     Parse a number of auth-header values (auth headers are
%%          Proxy-Authorization, Authorization and X-YXA-Peer-Auth)
%%          with sipheader:auth/1 and return the ones whose realm
%%          matches Realm.
%% @end
%%--------------------------------------------------------------------
parse_auth_filter_realm(In, Realm, Name) when is_list(Name), is_list(In), is_list(Realm) ->
    parse_auth_filter_realm(In, Realm, Name, []).

parse_auth_filter_realm([H | T], Realm, Name, Res) when is_list(H) ->
    Dict = sipheader:auth(H),
    case dict:find("realm", Dict) of
	{ok, Realm} ->
	    parse_auth_filter_realm(T, Realm, Name, [Dict | Res]);
	{ok, OtherRealm} ->
	    logger:log(debug, "Auth: Ignoring ~s header with realm not matching mine (~p /= ~p)",
		       [Name, OtherRealm, Realm]),
	    parse_auth_filter_realm(T, Realm, Name, Res);
	_ ->
	    logger:log(error, "Auth: Ignoring ~s header without realm", [Name]),
	    parse_auth_filter_realm(T, Realm, Name, Res)
    end;
parse_auth_filter_realm([], _Realm, _Name, Res) ->
    lists:reverse(Res).

%%--------------------------------------------------------------------
%% @spec    (Header, Method) ->
%%            false                      |
%%            {stale, User}              |
%%            {authenticated, User}      |
%%            {peer_authenticated, User} 
%%
%%            Header = #keylist{}
%%            Method = string()
%%
%%            User = string() "SIP authentication username"
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Authenticate through X-YXA-Peer-Auth or, if that does not
%%          exist, through Proxy-Authentication.
%% @end
%%--------------------------------------------------------------------
pstn_get_user_verified(Header, Method) when is_record(Header, keylist), is_list(Method) ->
    case get_user_verified_yxa_peer(Header, Method) of
	false ->
	    get_user_verified_proxy(Header, Method);
	{stale, User} ->
	    {stale, User};
	{authenticated, User} ->
	    {peer_authenticated, User}
    end.

%%--------------------------------------------------------------------
%% @spec    (User, ToNumber, Header, Class) ->
%%            true  |
%%            false
%%
%%            User     = string()
%%            ToNumber = string() "destination, local or E.164 number"
%%            Header   = #keylist{}
%%            Class    = atom()
%%
%% @doc     Check if a given User is explicitly allowed to call a
%%          number in a given Class.
%% @end
%%--------------------------------------------------------------------
is_allowed_pstn_dst(User, _ToNumber, _Header, Class) ->
    case local:get_classes_for_user(User) of
	nomatch ->
	    false;
	UserAllowedClasses when is_list(UserAllowedClasses) ->
	    lists:member(Class, UserAllowedClasses)
    end.

%%--------------------------------------------------------------------
%% @spec    (User, URL) ->
%%            true  |
%%            false
%%
%%            User = string()
%%            URL  = #sipurl{}
%%
%% @doc     Check if a given User may use address Address as From: by
%%          using the function can_use_address_detail/2 not caring
%%          about the reason it returns.
%% @end
%%--------------------------------------------------------------------
can_use_address(User, URL) when is_list(User), is_record(URL, sipurl) ->
    case local:can_use_address_detail(User, URL) of
	{true, _} -> true;
	{false, _} -> false
    end.

%%--------------------------------------------------------------------
%% @spec    (User, URL) ->
%%            {Verdict, Reason}
%%
%%            User = string()
%%            URL  = #sipurl{}
%%
%%            Verdict = true | false
%%            Reason  = ok | eperm | nomatch | error
%%
%% @doc     Check if a given User may use address Address as From:
%% @end
%%--------------------------------------------------------------------
can_use_address_detail(User, URL) when is_list(User), is_record(URL, sipurl) ->
    can_use_address_detail2(User, URL, local:get_users_for_url(URL)).

%% can_use_address_detail2 - the testable part of can_use_address_detail/2
can_use_address_detail2(User, URL, URLUsers) when is_list(User), is_record(URL, sipurl), is_list(URLUsers) ->
    case URLUsers of
	[User] ->
	    logger:log(debug, "Auth: User ~p is allowed to use address ~p",
		       [User, sipurl:print(URL)]),
	    {true, ok};
	[OtherUser] ->
	    logger:log(debug, "Auth: User ~p may NOT use use address ~p (belongs to user ~p)",
		       [User, sipurl:print(URL), OtherUser]),
	    {false, eperm};
	[] ->
	    logger:log(debug, "Auth: No users found for address ~p, use by user ~p NOT permitted",
		       [sipurl:print(URL), User]),
	    {false, nomatch};
	_ ->
	    case lists:member(User, URLUsers) of
		true ->
		    {true, ok};
		false ->
		    logger:log(debug, "Auth: Use of address ~p NOT permitted. Address maps to more than one user, but not to ~p (~p)",
			       [sipurl:print(URL), User, URLUsers]),
		    {false, eperm}
	    end
    end;
can_use_address_detail2(User, URL, nomatch) when is_list(User), is_record(URL, sipurl) ->
    logger:log(debug, "Auth: No users found for address ~p, use by user ~p NOT permitted",
	       [sipurl:print(URL), User]),
    {false, nomatch}.

%%--------------------------------------------------------------------
%% @spec    (Header, ToURL) ->
%%            {{Verdict, Reason}, User} |
%%            {stale, User}             |
%%            {false, none}
%%
%%            Header = #keylist{}
%%            ToURL  = #sipurl{}
%%
%%            Verdict = true | false
%%            Reason  = ok | eperm | nomatch | error
%%
%% @doc     Check if a REGISTER message authenticates OK, and check
%%          that the User returned from credentials check actually
%%          may use this To: (NOT From:, so third party registrations
%%          are not denied per se by this check).
%% @end
%%--------------------------------------------------------------------
can_register(Header, ToURL) ->
    case local:get_user_verified(Header, "REGISTER") of
	{authenticated, User} ->
	    {local:can_use_address_detail(User, ToURL), User};
	{stale, User} ->
	    {stale, User};
	_ ->
	    logger:log(debug, "Auth: Registration of address ~p NOT permitted", [sipurl:print(ToURL)]),
	    {false, none}
    end.

%%--------------------------------------------------------------------
%% @spec    (Method, URI, Header, User, Secret) ->
%%            NewHeader
%%
%%            Method = string() "SIP method"
%%            URI    = #sipurl{}
%%            Header = #keylist{}
%%            User   = string()
%%            Secret = string()
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Compute and add an X-YXA-Peer-Auth header to Header.
%% @end
%%--------------------------------------------------------------------
add_x_yxa_peer_auth(Method, URI, Header, User, Secret) when is_list(Method), is_record(URI, sipurl),
							    is_record(Header, keylist), is_list(User),
							    is_list(Secret) ->
    add_credentials(digest, "X-YXA-Peer-Auth", Method, URI, Header, User, Secret).

%%--------------------------------------------------------------------
%% @spec    (Type, HeaderName, Method, URI, Header, User, Secret) ->
%%            NewHeader
%%
%%            Type       = atom() "only 'digest' supported so far!"
%%            HeaderName = string() "SIP header name"
%%            Method     = string() "SIP method"
%%            URI        = #sipurl{}
%%            Header     = #keylist{}
%%            User       = string()
%%            Secret     = string()
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Compute and add a MD5 digest response header (HeaderName)
%%          to a header.
%% @end
%%--------------------------------------------------------------------
add_credentials(digest, HeaderName, Method, URI, Header, User, Secret)
  when is_list(HeaderName), is_list(Method), is_record(URI, sipurl), is_record(Header, keylist), is_list(User),
       is_list(Secret) ->
    {Realm, Nonce, Opaque} = get_challenge(),
    URIstr = sipurl:print(URI),
    Response = get_response(Nonce, Method, URIstr, User, Secret, Realm),
    AuthStr = print_auth_response("Digest", User, Realm, URIstr,
				  Response, Nonce, Opaque, "md5"),
    keylist:set(HeaderName, [AuthStr], Header).

%%--------------------------------------------------------------------
%% @spec    (AuthMethod, User, Realm, URIstr, Response, Nonce, Opaque,
%%          Algorithm) -> string()
%%
%%            AuthMethod = string()
%%            User       = string()
%%            Realm      = string()
%%            URIstr     = string()
%%            Response   = string()
%%            Nonce      = string()
%%            Opaque     = string()
%%            Algorithm  = string()
%%
%% @doc     Construct a challenge response, given a bunch of in-
%%          parameters.
%% @end
%%--------------------------------------------------------------------
print_auth_response(AuthMethod, User, Realm, URIstr, Response, Nonce, Opaque, Algorithm) ->
    Quote = "\"",
    QuoteComma = "\",",

    lists:concat([AuthMethod, " ",
		  "username=",		Quote, User,		QuoteComma,
		  "realm=",		Quote, Realm,		QuoteComma,
		  "uri=",		Quote, URIstr,		QuoteComma,
		  "response=",		Quote, Response,	QuoteComma,
		  "nonce=",		Quote, Nonce,		QuoteComma,
		  "opaque=",		Quote, Opaque,		QuoteComma,
		  "algorithm=",		Algorithm]).


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
    autotest:mark(?LINE, "sipauth - 0"),

    TestConfig = [{sipauth_password, "autotest.secret"}],
    ok = yxa_test_config:init(TestConfig),

    %% test classify_number(Number, RegexpList)
    %%--------------------------------------------------------------------
    ClassifyRegexp1 = [{"^123", internal},
		       {"^00", external}
		      ],
    autotest:mark(?LINE, "classify_number/2 - 1"),
    {ok, unknown} = classify_number(none, []),

    autotest:mark(?LINE, "classify_number/2 - 2"),
    %% test normal case #1
    {ok, internal} = classify_number("1234", ClassifyRegexp1),

    autotest:mark(?LINE, "classify_number/2 - 3"),
    %% test normal case #2
    {ok, external} = classify_number("00234", ClassifyRegexp1),

    autotest:mark(?LINE, "classify_number/2 - 4"),
    %% test unmatched number
    {ok, unknown} = classify_number("9", ClassifyRegexp1),

    autotest:mark(?LINE, "classify_number/2 - 5"),
    %% test invalid regexp (circumflex-plus), should be skipped
    {ok, unknown} = classify_number("+123", [{"^+1", internal}]),

    autotest:mark(?LINE, "classify_number/2 - 6"),
    %% test invalid regexp
    {error, _} = classify_number("+123", [{"unbalanced (", internal}]),


    %% test can_use_address_detail2(User, URL, URLUsers)
    %%--------------------------------------------------------------------
    CanUseURL1 = sipurl:parse("sip:ft@example.org"),

    autotest:mark(?LINE, "can_use_address_detail2/3 - 1"),
    {true, ok} = can_use_address_detail2("ft", CanUseURL1, ["ft"]),

    autotest:mark(?LINE, "can_use_address_detail2/3 - 2"),
    {false, eperm} = can_use_address_detail2("ft", CanUseURL1, ["not-ft"]),

    autotest:mark(?LINE, "can_use_address_detail2/3 - 3"),
    {false, nomatch} = can_use_address_detail2("ft", CanUseURL1, []),

    autotest:mark(?LINE, "can_use_address_detail2/3 - 3"),
    {true, ok} = can_use_address_detail2("ft", CanUseURL1, ["foo", "ft", "bar"]),

    autotest:mark(?LINE, "can_use_address_detail2/3 - 3"),
    {false, eperm} = can_use_address_detail2("ft", CanUseURL1, ["foo", "bar"]),

    autotest:mark(?LINE, "can_use_address_detail2/3 - 1"),
    {false, nomatch} = can_use_address_detail2("ft", CanUseURL1, nomatch),


    %% Auth tests
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "auth - 0"),
    AuthNow1		= 11000000,
    AuthTimestamp1	= hex:to(AuthNow1, 8),
    AuthOpaque1		= AuthTimestamp1,
    AuthMethod1		= "INVITE",
    AuthURI1		= "sip:ft@example.org",
    AuthUser1		= "ft.test",
    AuthPassword1	= "foo",
    AuthRealm1		= "yxa-test",
    AuthNonce1		= get_nonce(AuthTimestamp1),	%% The nonce is MD5 of AuthTimestamp1 colon OurSecret

    AuthCorrectResponse1 = get_response(AuthNonce1, AuthMethod1, AuthURI1, AuthUser1, AuthPassword1, AuthRealm1),
    AuthResponse1 = print_auth_response("Digest", AuthUser1, AuthRealm1, AuthURI1, AuthCorrectResponse1,
					AuthNonce1, AuthOpaque1, "md5"),
    AuthDict1 = sipheader:auth(AuthResponse1),


    %% test get_nonce(Timestamp)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_nonce/1 - 1"),
    "b190a506349b48fc21d42d4c1022295e" = get_nonce(hex:to(0, 8)),

    autotest:mark(?LINE, "get_nonce/1 - 2"),
    "00ade0f6f93a185c19c358c943c6ba5c" = get_nonce(hex:to(11000000, 8)),

    autotest:mark(?LINE, "get_nonce/1 - 3"),
    "d9d49f6dbd47739e9780ed7a267b184f" = get_nonce(hex:to(22000000, 8)),


    %% test do_get_user_verified2(Method, User, UAuser, Password, Realm, Now, AuthDict)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "do_get_user_verified2/7 - 1"),
    %% Correct response (AuthDict1)
    {authenticated, "canon-user"} =
	do_get_user_verified2(AuthMethod1, "canon-user", AuthUser1, AuthPassword1,
			      AuthRealm1, AuthNow1, AuthDict1),

    autotest:mark(?LINE, "do_get_user_verified2/7 - 2"),
    %% Correct response, time in the future
    false =
	do_get_user_verified2(AuthMethod1, "canon-user", AuthUser1, AuthPassword1,
			      AuthRealm1, AuthNow1 - 1, AuthDict1),

    autotest:mark(?LINE, "do_get_user_verified2/7 - 3"),
    %% Correct response, time since challenge: 30 seconds
    {authenticated, "canon-user"} =
	do_get_user_verified2(AuthMethod1, "canon-user", AuthUser1, AuthPassword1,
			      AuthRealm1, AuthNow1 + 30, AuthDict1),

    autotest:mark(?LINE, "do_get_user_verified2/7 - 4"),
    %% Correct response, time since challenge: 31 seconds = stale
    {stale, "canon-user"} =
	do_get_user_verified2(AuthMethod1, "canon-user", AuthUser1, AuthPassword1,
			      AuthRealm1, AuthNow1 + 31, AuthDict1),

    autotest:mark(?LINE, "do_get_user_verified2/7 - 5"),
    %% Invalid password
    false =
	do_get_user_verified2(AuthMethod1, "canon-user", AuthUser1, "incorrect",
			      AuthRealm1, AuthNow1, AuthDict1),

    autotest:mark(?LINE, "do_get_user_verified2/7 - 6"),
    %% Invalid user, indicated by password 'nomatch'
    false =
	do_get_user_verified2(AuthMethod1, "canon-user", AuthUser1, nomatch,
			      AuthRealm1, AuthNow1, AuthDict1),

    autotest:mark(?LINE, "do_get_user_verified2/7 - 7"),
    %% Wrong 'nonce' parameter
    false =
	do_get_user_verified2(AuthMethod1, "canon-user", AuthUser1, AuthPassword1,
			      AuthRealm1, AuthNow1, dict:store("nonce", "0a1b2c", AuthDict1)),

    autotest:mark(?LINE, "do_get_user_verified2/7 - 8"),
    %% Missing 'opaque' parameter
    {siperror, 400, "Authorization should contain opaque"} =
	(catch do_get_user_verified2(AuthMethod1, "canon-user", AuthUser1, nomatch,
			      AuthRealm1, AuthNow1, dict:erase("opaque", AuthDict1))),


    %% test get_challenge()
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_challenge/0 - 1.1"),
    {_Realm, ChallengeNonce1, ChallengeTimestamp1} = get_challenge(),

    autotest:mark(?LINE, "get_challenge/0 - 1.1"),
    %% verify results as good as we can
    ChallengeNonce1 = get_nonce(ChallengeTimestamp1),
    true = (ChallengeTimestamp1 > 11000000),


    %% test get_user_verified(Header, Method)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_user_verified/2 - 1"),
    %% Test without Authorization header - that is the only thing we can test
    %% here. The testable parts of this code is tested above (do_get_user_verified2).
    false = get_user_verified(keylist:from_list([]), "INVITE"),


    %% test get_user_verified_proxy(Header, Method)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_user_verified_proxy/2 - 1"),
    %% Test without Authorization header - that is the only thing we can test
    %% here. The testable parts of this code is tested above (do_get_user_verified2).
    false = get_user_verified(keylist:from_list([]), "INVITE"),


    %% test can_use_address(User, URL)
    %% Not much can be tested in this function, but some is better than nothing
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "can_use_address/2 - 1"),
    false = can_use_address("ft.testuser", sipurl:parse("sip:not-homedomain.example.org")),


    %% test can_register(Header, ToURL)
    %% Not much can be tested in this function, but some is better than nothing
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "can_register/2 - 1"),
    {false, none} = can_register(keylist:from_list([]), sipurl:parse("sip:ft@example.org")),


    %% test parse_auth_filter_realm(In, Realm, Name)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "parse_auth_filter_realm/3 - 1"),
    %% no matching
    [] = parse_auth_filter_realm(["Digest username=\"test\", realm=\"nomatch\""], "test", "Test-Auth"),

    autotest:mark(?LINE, "parse_auth_filter_realm/3 - 2.1"),
    %% two matching, one non-matching
    [RealmFilterDict1, RealmFilterDict2] =
	parse_auth_filter_realm(["Digest username=\"test1\", realm=\"test\"",
				 "Digest username=\"test\", realm=\"nomatch\"",
				 "Digest username=\"test2\", realm=\"test\""],
				"test", "Test-Auth"),

    autotest:mark(?LINE, "parse_auth_filter_realm/3 - 2.2"),
    %% verify the usernames in the dicts
    {ok, "test1"} = dict:find("username", RealmFilterDict1),
    {ok, "test2"} = dict:find("username", RealmFilterDict2),

    ok = yxa_test_config:stop(),

    ok.
