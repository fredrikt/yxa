%%%-------------------------------------------------------------------
%%% File    : sipauth.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Description : SIP authentication functions.
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(sipauth).

%%-compile(export_all).

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


-include("siprecords.hrl").

%% MD5 digest 'formula'
%%
%% A1 = username ":" realm ":" password
%% A2 = Method ":" digest-uri
%% nonce = H(timestamp ":" privatekey)
%% resp = H(H(A1) ":" nonce ":" H(A2))

%%--------------------------------------------------------------------
%% Function: realm()
%% Descrip.: Return this proxys configured authentication realm.
%% Returns : string()
%%--------------------------------------------------------------------
realm() ->
    sipserver:get_env(sipauth_realm, "").

%%--------------------------------------------------------------------
%% Function: get_nonce()
%% Descrip.: Create a nonce. Since we have not located any useful
%%           randomness functions in Erlang, and since all proxys that
%%           share authentication realm should be able to use the
%%           responses to the challenges we create here, we use the
%%           current time plus the configured sipauth_password.
%% Returns : string()
%%--------------------------------------------------------------------
get_nonce(Timestamp) ->
    hex:to(erlang:md5(Timestamp ++ ":" ++ sipserver:get_env(sipauth_password, ""))).

%%--------------------------------------------------------------------
%% Function: get_challenge()
%% Descrip.: Create a challenge tuple.
%% Returns : Challenge
%%           Challenge = {Realm, Nonce, Timestamp}
%%           Realm     = string()
%%           Nonce     = string()
%%           Timestamp = string()
%%--------------------------------------------------------------------
get_challenge() ->
    Timestamp = hex:to(util:timestamp(), 8),
    {realm(), get_nonce(Timestamp), Timestamp}.

%%--------------------------------------------------------------------
%% Function: get_response(Nonce, Method, URI, User, Password)
%%           Nonce    = string()
%%           Method   = string()
%%           URI      = string()
%%           User     = string()
%%           Password = string() | nomatch
%% Descrip.: Get the correct response to a challenge, given a nonce,
%%           method, URI, username and password.
%% Returns : Response |
%%           none
%%           Response = string()
%%--------------------------------------------------------------------
get_response(_Nonce, _Method, _URI, _User, nomatch) ->
    %% Password is nomatch - return 'none'
    none;
get_response(Nonce, Method, URI, User, Password) ->
    A1 = hex:to(erlang:md5(User ++ ":" ++ realm() ++ ":" ++ Password)),
    A2 = hex:to(erlang:md5(Method ++ ":" ++ URI)),
    hex:to(erlang:md5(A1 ++ ":" ++ Nonce ++ ":" ++ A2)).

%%--------------------------------------------------------------------
%% Function: classify_number(Number, Regexps)
%%           Number  = string(),
%%           Regexps = list() of {Regexp, Class} tuple()
%%           Regexp  = string()
%%           Class   = atom()
%% Descrip.: Search a list of regexps until Number matches the Regexp
%%           and return the Class.
%% Returns : Class   |
%%           unknown
%%           Class = atom()
%%--------------------------------------------------------------------
classify_number(none, _) ->
    unknown;

classify_number(_Number, []) ->
    unknown;

classify_number(Number, [{"^+" ++ Regexp, _Class} | Rest]) ->
    logger:log(error, "sipauth:classify_number() Skipping invalid regexp ~p (you probably "
	       "forgot to escape the plus char)", ["^+" ++ Regexp]),
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

%%--------------------------------------------------------------------
%% Function: get_user_verified(Header, Method)
%%           Header = keylist record()
%%           Method = string()
%% Descrip.: Check if there is an Authorization: header in Header and
%%           check if it contains a valid response of a challenge we
%%           supposedly sent out.
%% Returns : false                 |
%%           {stale, User}         |
%%           {authenticated, User}
%%           User = string(), SIP authentication username
%%--------------------------------------------------------------------
get_user_verified(Header, Method) ->
    case keylist:fetch('authorization', Header) of
	[] ->
	    logger:log(debug, "Auth: get_user_verified: No Authorization header, returning false"),
	    false;
	Authheader ->
	    get_user_verified2(Method, Authheader, Header)
    end.

%%--------------------------------------------------------------------
%% Function: get_user_verified_proxy(Header, Method)
%%           Header = keylist record()
%%           Method = string()
%% Descrip.: Check if there is an Proxy-Authorization: header in
%%           Header and check if it contains a valid response of a
%%           challenge we supposedly sent out. Might throw an
%%           {siperror, ...} if something is wrong with the
%%           authorization header.
%% Returns : false                 |
%%           {stale, User}         |
%%           {authenticated, User} |
%%           throw()
%%           User = string(), SIP authentication username
%%--------------------------------------------------------------------
get_user_verified_proxy(Header, Method) ->
    case keylist:fetch('proxy-authorization', Header) of
	[] ->
	    logger:log(debug, "Auth: get_user_verified_proxy: No Proxy-Authorization header, returning false"),
	    false;
	Authheader ->
	    get_user_verified2(Method, Authheader, Header)
    end.

get_user_verified2(_Method, ["GSSAPI" ++ _R] = Authheader, _Header) ->
    Authorization = sipheader:auth(Authheader),
    Info = dict:fetch("info", Authorization),
    {_Response, Username} = gssapi:request(Info),
    %% XXX this is definately broken! What does gssapi:request() return anyways?
    Username;

%%--------------------------------------------------------------------
%% Function: get_user_verified2(Method, Authheader, Header)
%%           Method     = string()
%%           Authheader = [string()], the auth header in question
%%           Header     = keylist record()
%% Descrip.: Authenticate a request.
%% Returns : {authenticated, User} |
%%           {stale, User}         |
%%           false
%%--------------------------------------------------------------------
get_user_verified2(Method, Authheader, Header) ->
    Authorization = sipheader:auth(Authheader),
    %% Remember the username the client used
    UAuser = dict:fetch("username", Authorization),
    %% Canonify username
    User = case local:canonify_authusername(UAuser, Header) of
	       undefined ->
		   UAuser;
	       Res when is_list(Res) ->
		   Res
	   end,
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
    Password = case local:get_password_for_user(User) of
		   nomatch ->
		       nomatch;
		   PRes when is_list(PRes) ->
		       PRes;
		   E ->
		       logger:log(error, "Auth: Failed to fetch password for user ~p, "
				  "result of get_password_for_user was : ~p", [User, E]),
		       throw({siperror, 500, "Server Internal Error"})
	       end,

    Nonce2 = get_nonce(Opaque),
    Response2 = get_response(Nonce2, Method,
			     dict:fetch("uri", Authorization),
			     UAuser, Password),
    if
	Password == nomatch ->
	    logger:log(normal, "Auth: Authentication failed for non-existing user ~p", [User]),
	    false;
	Response /= Response2 ->
	    %%logger:log(normal, "Response ~p /= Response2 ~p", [Response, Response2]),
	    logger:log(normal, "Auth: Authentication failed for user ~p", [User]),
	    false;
	Nonce /= Nonce2 ->
	    logger:log(normal, "Auth: Nonce ~p /= ~p, authentication failed for user ~p", [Nonce, Nonce2, User]),
	    {stale, User};
	Timestamp < Now - 30 ->
	    logger:log(normal, "Auth: Timestamp ~p too old. Now: ~p, authentication failed for user ~p",
		       [Timestamp, Now, User]),
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
%% Function: pstn_call_check_auth(Method, Header, URL, ToNumberIn,
%%                                Classdefs)
%%           Method     = string()
%%           Header     = keylist record()
%%           URL        = sipurl record(), From-address in request
%%           ToNumberIn = string(), destination, local or E.164 number
%%           Classdefs  = term()
%% Descrip.: Check if the destination is allowed for this user, and
%%           check if this user may use this Address.
%% Returns : {Allowed, User, Class}
%%           Allowed = true | false
%%           User    = string(), SIP authentication username
%%           Class   = atom(), the class that this ToNumberIn matched
%%--------------------------------------------------------------------
pstn_call_check_auth(Method, Header, URL, ToNumberIn, Classdefs)
  when is_list(Method), is_record(Header, keylist), is_record(URL, sipurl), is_list(ToNumberIn) ->
    ToNumber = case local:rewrite_potn_to_e164(ToNumberIn) of
		   error -> ToNumberIn;
		   N -> N
	       end,
    Class = classify_number(ToNumber, Classdefs),
    case lists:member(Class, sipserver:get_env(sipauth_unauth_classlist, [])) of
	true ->
	    %% This is a class that anyone should be allowed to call,
	    %% just check that if this is one of our SIP users, they
	    %% are permitted to use the From: address
	    logger:log(debug, "Auth: ~p is of class ~p which does not require authorization", [ToNumber, Class]),
	    Address = sipurl:print(URL),
	    case local:get_user_with_address(Address) of
		nomatch ->
		    logger:log(debug, "Auth: Address ~p does not match any of my users, no need to verify.", 
			       [Address]),
		    {true, unknown, Class};
		User when list(User) ->
		    Allowed = local:can_use_address(User, URL),
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
		    UserAllowedToUseAddress = local:can_use_address(User, URL),
		    AllowedCallToNumber = local:is_allowed_pstn_dst(User, ToNumber, Header, Class),
		    if
			UserAllowedToUseAddress /= true ->
			    logger:log(normal, "Auth: User ~p is not allowed to use address ~p (when placing PSTN "
				       "call to ~s (class ~p))", [User, sipurl:print(URL), ToNumber, Class]),
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

%%--------------------------------------------------------------------
%% Function: is_allowed_pstn_dst(User, ToNumber, Header, Class)
%%           User     = string()
%%           ToNumber = string(), destination, local or E.164 number
%%           Header   = keylist record()
%%           Class    = atom()
%% Descrip.: Check if a given User is explicitly allowed to call a
%%           number in a given Class, or if there is a Route: header
%%           present in Header.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
is_allowed_pstn_dst(User, _ToNumber, Header, Class) ->
    case keylist:fetch('route', Header) of
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

%%--------------------------------------------------------------------
%% Function: can_use_address(User, URL)
%%           User    = string()
%%           URL     = sipurl record()
%% Descrip.: Check if a given User may use address Address as From:
%%           by using the function can_use_address_detail/2 not caring
%%           about the reason it returns.
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
can_use_address(User, URL) when is_list(User), is_record(URL, sipurl) ->
    case local:can_use_address_detail(User, URL) of
	{true, _} -> true;
	{false, _} -> false
    end.

%%--------------------------------------------------------------------
%% Function: can_use_address_detail(User, URL)
%%           User    = string()
%%           URL     = sipurl record()
%% Descrip.: Check if a given User may use address Address as From:
%% Returns : {Verdict, Reason}
%%           Verdict = true | false
%%           Reason  = ok | eperm | nomatch | error
%%--------------------------------------------------------------------
can_use_address_detail(User, URL) when is_list(User), is_record(URL, sipurl) ->
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

%%--------------------------------------------------------------------
%% Function: can_register(Header, ToURL)
%%           Header = keylist record()
%%           ToURL  = sipurl record()
%% Descrip.: Check if a REGISTER message authenticates OK, and check
%%           that the User returned from credentials check actually
%%           may use this To: (NOT From:, so third party registrations
%%           are not denied per se by this check).
%% Returns : {{Verdict, Reason}, User} |
%%           {stale, User}             |
%%           {false, none}
%%           Verdict = true | false
%%           Reason  = ok | eperm | nomatch | error
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
