-module(sipauth).
-export([get_response/5,
	 get_nonce/1,
	 get_user_verified/2,
	 get_user_verified_proxy/2,
	 get_challenge/0, 
	 can_register/2,
	 pstn_call_check_auth/5,
	 is_allowed_pstn_dst/4,
	 can_use_address/2,
	 can_use_address_detail/2,
	 realm/0]).

% A1 = username ":" realm ":" password
% A2 = Method ":" digest-uri
% nonce = H(timestamp ":" privatekey)
% resp = H(H(A1) ":" nonce ":" H(A2))

realm() ->
    sipserver:get_env(sipauth_realm, "").

get_nonce(Timestamp) ->
    hex:to(erlang:md5(Timestamp ++ ":" ++ sipserver:get_env(sipauth_password, ""))).

get_challenge() ->
    Timestamp = hex:to(util:timestamp(), 8),
    {realm(), get_nonce(Timestamp), Timestamp}.


get_response(Nonce, Method, URI, User, nomatch) ->
    none;

get_response(Nonce, Method, URI, User, Password) ->
    A1 = hex:to(erlang:md5(User ++ ":" ++ realm() ++ ":" ++ Password)),
    A2 = hex:to(erlang:md5(Method ++ ":" ++ URI)),
    hex:to(erlang:md5(A1 ++ ":" ++ Nonce ++ ":" ++ A2)).

classify_number(none, _) ->
    unknown;
    
classify_number(Number, []) ->
    unknown;

classify_number(Number, [{"^+" ++ Regexp, Class} | Rest]) ->
    logger:log(error, "sipauth:classify_number() Skipping invalid regexp ~p (you probably forgot to escape the plus char)", ["^+" ++ Regexp]),
    classify_number(Number, Rest);

classify_number(Number, [{Regexp, Class} | Rest]) ->
    case regexp:first_match(Number, Regexp) of
	{match, _, _} ->
	    Class;
	nomatch ->
	    classify_number(Number, Rest);
	{error, Error} ->
	    logger:log(normal, "Error in regexp ~p: ~p", [Regexp, Error])
    end.

get_user_verified(Header, Method) ->
    case keylist:fetch("Authorization", Header) of
	[] ->
	    logger:log(debug, "Auth: get_user_verified: No Authorization header, returning false"),
	    false;
	Authheader ->
	    get_user_verified2(Method, Authheader)
    end.

get_user_verified_proxy(Header, Method) ->
    case keylist:fetch("Proxy-Authorization", Header) of
	[] ->
	    logger:log(debug, "Auth: get_user_verified_proxy: No Proxy-Authorization header, returning false"),
	    false;
	Authheader ->
	    get_user_verified2(Method, Authheader)
    end.

get_user_verified2(Method, ["GSSAPI" ++ Authheader]) ->
    Authorization = sipheader:auth(["GSSAPI" ++ Authheader]),
    Info = dict:fetch("info", Authorization),
    {Response, Username} = gssapi:request(Info),
    Username;

get_user_verified2(Method, Authheader) ->
    Authorization = sipheader:auth(Authheader),
    Response = dict:fetch("response", Authorization),
    Nonce = dict:fetch("nonce", Authorization),
    Opaque = case dict:find("opaque", Authorization) of
		 error ->
		     throw({siperror, 400, "Authorization should contain opaque"});
		 {ok, Value} ->
		     Value
	     end,
    Timestamp = hex:from(Opaque),
    Now = util:timestamp(),
    logger:log(debug, "Auth: timestamp: ~p now: ~p", [Timestamp, Now]),
    User = dict:fetch("username", Authorization),
    Password = case local:get_password_for_user(User) of
	nomatch ->
	    nomatch;
	PRes when list(PRes) ->
	    PRes;
	E ->
	    logger:log(error, "Auth: Failed to fetch password for user ~p, result of get_password_for_user was : ~p", [User, E]),
	    throw({siperror, 500, "Server Internal Error"})
    end,
    Nonce2 = get_nonce(Opaque),
    Response2 = get_response(Nonce2, Method,
			     dict:fetch("uri", Authorization),
			     User, Password),
    if
	Password == nomatch ->
	    logger:log(normal, "Auth: Authentication failed for non-existing user ~p", [User]),
	    false;
	Response /= Response2 ->
	    %logger:log(normal, "Response ~p /= Response2 ~p", [Response, Response2]),
	    logger:log(normal, "Auth: Authentication failed for user ~p", [User]),
	    false;
	Nonce /= Nonce2 ->
	    logger:log(normal, "Auth: Nonce ~p /= ~p, authentication failed for user ~p", [Nonce, Nonce2, User]),
	    stale;
	Timestamp < Now - 30 ->
	    logger:log(normal, "Auth: Timestamp ~p too old. Now: ~p, authentication failed for user ~p", [Timestamp, Now, User]),
	    stale;
	Timestamp > Now ->
	    logger:log(normal, "Auth: Timestamp ~p too new. Now: ~p, authentication failed for user ~p", [Timestamp, Now, User]),
	    false;
	true ->
	    logger:log(debug, "Auth: User ~p authenticated", [User]),
	    {authenticated, User}
    end.

pstn_call_check_auth(Method, Header, Address, ToNumberIn, Classdefs) ->
    ToNumber = case local:rewrite_potn_to_e164(ToNumberIn) of
	none -> ToNumberIn;
	N -> N
    end,
    Class = classify_number(ToNumber, Classdefs),
    case lists:member(Class, sipserver:get_env(sipauth_unauth_classlist, [])) of
	true ->
	    % This is a class that anyone should be allowed to call,
	    % just check that if this is one of our SIP users, they
	    % are permitted to use the From: address
	    logger:log(debug, "Auth: ~p is of class ~p which does not require authorization", [ToNumber, Class]),
	    case local:get_user_with_address(Address) of
		nomatch ->
		    logger:log(debug, "Auth: Address ~p does not match any of my users, no need to verify.", [Address]),
		    {true, unknown, Class};
		User when list(User) ->
		    Allowed = local:can_use_address(User, Address),
		    {Allowed, User, Class};
		Unknown ->
		    logger:log(error, "Auth: Unknown result returned from get_user_for_address(~p) : ~p",
				[Address, Unknown]),
		    {false, unknown, Class}
	    end;		    
	_ ->
	    case get_user_verified_proxy(Header, Method) of
		false ->
		    {false, none, Class};
		stale ->
		    {stale, none, Class};
		{authenticated, User} ->
		    UserAllowedToUseAddress = local:can_use_address(User, Address),
		    AllowedCallToNumber = local:is_allowed_pstn_dst(User, ToNumber, Header, Class),
		    if
			UserAllowedToUseAddress /= true ->
			    logger:log(normal, "Auth: User ~p is not allowed to use address ~p (when placing PSTN call to ~s (class ~p))",
			    		[User, Address, ToNumber, Class]),
			    {false, User, Class};
			AllowedCallToNumber /= true ->
			    logger:log(normal, "Auth: User ~p not allowed to call ~p in class ~p",
				       [User, ToNumber, Class]),
			    {false, User, Class};
			true ->
			    {true, User, Class}
		    end;
		Unknown ->
		    logger:log(error, "Auth: Unknown result from get_user_verified_proxy: ~p", [Unknown]),
		    {false, none, Class}
	    end
    end.

is_allowed_pstn_dst(User, ToNumber, Header, Class) ->
    case keylist:fetch("Route", Header) of
	[] ->
	    case local:get_classes_for_user(User) of
		nomatch ->
		    false;
		UserAllowedClasses when list(UserAllowedClasses) ->
		    lists:member(Class, UserAllowedClasses);
		Unknown ->
		    logger:log(error, "Auth: Unknown result from local:get_classes_for_user() user ~p :~n~p",
				[User, Unknown]),
		    false
	    end;
	R when list(R) ->
	    logger:log(debug, "Auth: Authenticated user ~p sends request with Route-header. Allow.", [User]),
	    true
    end.

can_use_address(User, Address) ->
    case local:can_use_address_detail(User, Address) of
	{Verdict, _} -> Verdict;
	R -> R
    end.

can_use_address_detail(User, Address) when list(User), list(Address) ->
    URL = sipurl:parse(Address),
    case local:get_users_for_url(URL) of
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
	nomatch ->
	    logger:log(debug, "Auth: No users found for address ~p, use by user ~p NOT permitted",
	    		[sipurl:print(URL), User]),
	    {false, nomatch};
	Users when list(Users) ->
	    case lists:member(User, Users) of
		true ->
		    {true, ok};
		false ->
		    logger:log(debug, "Auth: Use of address ~p NOT permitted. Address maps to more than one user, but not to ~p (~p)",
		    		[sipurl:print(URL), User, Users]),
		    {false, eperm}
	    end;
	Unknown ->
	    logger:log(debug, "Auth: Use of address ~p NOT permitted. Unknown result from get_users_for_url : ~p",
	    		[sipurl:print(URL), Unknown]),
	    {false, error}
    end;
can_use_address_detail(User, Address) ->
    logger:log(debug, "Auth: can_use_address() called with incorrect arguments, User ~p Address ~p",
		[User, Address]),
    {false, error}.

can_register(Header, Address) ->
    case local:get_user_verified(Header, "REGISTER") of
	{authenticated, User} ->
	    {local:can_use_address_detail(User, Address), User};
	{stale, User} ->
	    {stale, User};
	_ ->
	    logger:log(debug, "Auth: Registration of address ~p NOT permitted", [Address]),
	    {false, none}
    end.
