-module(sipauth).
-export([check_and_send_auth/9, get_response/5,
	 get_nonce/1, get_user_verified/2, check_and_send_relay/5,
	 get_challenge/0,
	 can_register/2]).

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


get_response(Nonce, Method, URI, User, none) ->
    none;

get_response(Nonce, Method, URI, User, Password) ->
    A1 = hex:to(erlang:md5(User ++ ":" ++ realm() ++ ":" ++ Password)),
    A2 = hex:to(erlang:md5(Method ++ ":" ++ URI)),
    hex:to(erlang:md5(A1 ++ ":" ++ Nonce ++ ":" ++ A2)).

get_passnumber(Usertext) when list(Usertext) ->
    {User, Host} = canon_user(Usertext),
    case {get_passnumber_try(User ++ "@" ++ Host), realm()} of
	{{ok, Res}, _} ->
	    Res;
	{{error}, Host} ->
	    case get_passnumber_try(User) of
		{ok, Res} ->
		    Res;
		{error} ->
		    {none, [], [], []}
	    end;
	_ ->
	    {none, [], [], []}
    end;

get_passnumber(_) ->
    {none, [], [], []}.    

get_passnumber_try(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    {error};
	{atomic, [A]} ->
	    {Password, Flags, Classes} = A,
	    {atomic, Numbers} = phone:get_numbers_for_user(User),
	    {ok, {Password, Numbers, Flags, Classes}};
	{aborted, _} ->
	    {error}
    end.

get_class(Number, []) ->
    unknown;

get_class(Number, [{Regexp, Class} | Rest]) ->
    case regexp:first_match(Number, Regexp) of
	{match, _, _} ->
	    Class;
	nomatch ->
	    get_class(Number, Rest);
	{error, Error} ->
	    logger:log(normal, "Error in regexp ~p: ~p", [Regexp, Error])
    end.

get_user_verified(Header, Method) ->
    Authheader = keylist:fetch("Authorization", Header),
    if Authheader == [] ->
	    false;
       true ->
	    get_user_verified(Header, Method, Authheader)
    end.

get_user_verified_proxy(Header, Method) ->
    Authheader = keylist:fetch("Proxy-Authorization", Header),
    if Authheader == [] ->
	    false;
       true ->
	    get_user_verified(Header, Method, Authheader)
    end.

get_user_verified(Header, Method, ["GSSAPI" ++ Authheader]) ->
    Authorization = sipheader:auth(["GSSAPI" ++ Authheader]),
    Info = dict:fetch("info", Authorization),
    {Response, Username} = gssapi:request(Info),
    Username;

get_user_verified(Header, Method, Authheader) ->
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
    {Password, _, _, _} = get_passnumber(User),
    Nonce2 = get_nonce(Opaque),
    Response2 = get_response(Nonce2, Method,
			     dict:fetch("uri", Authorization),
			     User, Password),
    if
	Password == none ->
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
	    User
    end.

check_auth(Header, Method, Number, Tophone, Classdefs) ->
    User = get_user_verified_proxy(Header, Method),
    {Numberallowed, _} = can_use_name(User, Number),
    {_, _, _, Classes} = get_passnumber(User),
    Class = get_class(Tophone, Classdefs),
    Classallowed = lists:member(Class, Classes),
    if
	User == false ->
	    false;
	User == stale ->
	    stale;
	Numberallowed /= true ->
	    logger:log(normal, "Number ~p not allowed caller id", [Number]),
	    false;
	Classallowed /= true ->
	    logger:log(normal, "Number ~p not allowed to call ~p in class ~p",
		       [Number, Tophone, Class]),
	    false;
	true ->
	    true
    end.

canon_user(Fulluser) ->
    case string:tokens(Fulluser, "@") of
	[User, Host] ->
	    {User, Host};
	[User] ->
	    {User, realm()};
	[] ->
	    {"", realm()}
    end.

canon_list(List) ->
    lists:map(fun canon_user/1, List).

can_use_name(User, Number) ->
    case User of
	false ->
	    {false, []};
	stale ->
	    {stale, []};
	User ->
	    {_, Numberlist, _, _} = get_passnumber(User),
	    CanonUser = canon_user(User),
	    case canon_user(Number) of
		CanonUser ->
		    {true, Numberlist};
		CanonNumber ->
		    Res = {lists:member(CanonNumber, canon_list(Numberlist)), []},
		    case Res of
			true ->
			    true;
			_ ->
			    logger:log(debug, "Auth: User ~p may NOT user number ~p (allowed number(s): ~p)",
					[User, Number, Numberlist])
		    end,
		    Res
	    end
    end.

can_register(Header, Number) ->
    can_use_name(get_user_verified(Header, "REGISTER"), Number).

check_and_send_auth(OrigHeader, Header, Socket, Phone, Tophone, Func, Arg, "ACK", Classdefs) ->
    logger:log(debug, "Auth: Always accepting ACK"),
    apply(Func, [Header, Socket, Arg]);
check_and_send_auth(OrigHeader, Header, Socket, Phone, Tophone, Func, Arg, "CANCEL", Classdefs) ->
    logger:log(debug, "Auth: Always accepting CANCEL"),
    apply(Func, [Header, Socket, Arg]);
check_and_send_auth(OrigHeader, Header, Socket, Phone, Tophone, Func, Arg, Method, Classdefs) ->
    Class = get_class(Tophone, Classdefs),
    Classlist = sipserver:get_env(sipauth_unauth_classlist, []),
    Classallowed = lists:member(Class, Classlist),
    if
	Classallowed == true ->
	    logger:log(debug, "Auth: ~s is allowed to call ~s (class ~s) without challenge", [Phone, Tophone, Class]),
	    apply(Func, [Header, Socket, Arg]);
	true ->
	    case check_auth(Header, Method, Phone, Tophone, Classdefs) of
		true -> 
		    logger:log(debug, "Auth: Authenticated user ~s is allowed dst ~s (class ~s)", [Phone, Tophone, Class]),
		    apply(Func, [Header, Socket, Arg]);
		stale ->
		    logger:log(debug, "Auth: User ~s must authenticate for dst ~s (class ~s)", [Phone, Tophone, Class]),
		    siprequest:send_proxyauth_req(OrigHeader, Socket,
						  get_challenge(), true);
		false ->
		    logger:log(debug, "Auth: User ~s must authenticate for dst ~s (class ~s)", [Phone, Tophone, Class]),
		    siprequest:send_proxyauth_req(OrigHeader, Socket,
						  get_challenge(), false)
	    end
    end.

check_and_send_relay(Header, Socket, Func, Arg, "ACK") ->
    logger:log(debug, "Auth: Always accepting ACK"),
    apply(Func, [Header, Socket, Arg]);
check_and_send_relay(Header, Socket, Func, Arg, "CANCEL") ->
    logger:log(debug, "Auth: Always accepting CANCEL"),
    apply(Func, [Header, Socket, Arg]);
check_and_send_relay(Header, Socket, Func, Arg, Method) ->
    case get_user_verified_proxy(Header, Method) of
	false ->
	    siprequest:send_proxyauth_req(Header, Socket, sipauth:get_challenge(), false);
	stale ->
	    siprequest:send_proxyauth_req(Header, Socket, sipauth:get_challenge(), true);
	User ->
	    apply(Func, [Header, Socket, Arg])
    end.
