-module(sipauth).
-export([check_and_send_auth/8, get_response/5,
	get_nonce/1, get_user_verified/2, get_challenge/0,
	can_register/2]).

% A1 = username ":" realm ":" password
% A2 = Method ":" digest-uri
% nonce = H(timestamp ":" privatekey)
% resp = H(H(A1) ":" nonce ":" H(A2))

realm() ->
    "kth.se".

my_password() ->
    "foobarhemligt".

get_nonce(Timestamp) ->
    hex:to(erlang:md5(Timestamp ++ ":" ++ my_password())).

get_challenge() ->
    Timestamp = hex:to(util:timestamp(), 8),
    {realm(), get_nonce(Timestamp), Timestamp}.


get_response(Nonce, Method, URI, User, none) ->
    none;

get_response(Nonce, Method, URI, User, Password) ->
    A1 = hex:to(erlang:md5(User ++ ":" ++ realm() ++ ":" ++ Password)),
    A2 = hex:to(erlang:md5(Method ++ ":" ++ URI)),
    hex:to(erlang:md5(A1 ++ ":" ++ Nonce ++ ":" ++ A2)).

get_passnumber(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    {none, [], [], []};
	{atomic, [A]} ->
	    A;
	{aborted, _} ->
	    {none, [], [], []}
    end.

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

get_user_verified(Header, Method, Authheader) ->
    Authorization = sipheader:auth(Authheader),
    Response = dict:fetch("response", Authorization),
    Nonce = dict:fetch("nonce", Authorization),
    Opaque = dict:fetch("opaque", Authorization),
    Timestamp = hex:from(Opaque),
    Now = util:timestamp(),
    logger:log(debug, "timestamp: ~p now: ~p", [Timestamp, Now]),
    User = dict:fetch("username", Authorization),
    {Password, _, _, _} = get_passnumber(User),
    Nonce2 = get_nonce(Opaque),
    Response2 = get_response(Nonce2, Method,
			     dict:fetch("uri", Authorization),
			     User, Password),
    if
	Response /= Response2 ->
	    logger:log(normal, "Response ~p /= Response2 ~p", [Response, Response2]),
	    false;
	Nonce /= Nonce2 ->
	    logger:log(normal, "Nonce ~p /= ~p", [Nonce, Nonce2]),
	    stale;
	Timestamp < Now - 5 ->
	    logger:log(normal, "Timestamp ~p too old. Now: ~p", [Timestamp, Now]),
	    stale;
	Timestamp > Now ->
	    logger:log(normal, "Timestamp ~p too new. Now: ~p", [Timestamp, Now]),
	    false;
	true ->
	    User
    end.

check_auth(Header, Method, Number, Tophone, Classdefs) ->
    User = get_user_verified_proxy(Header, Method),
    {_, Numberlist, _, Classes} = get_passnumber(User),
    Numberallowed = lists:member(Number, Numberlist),
    Class = get_class(Tophone, Classdefs),
%    Classlist = [internal, mobile_or_pay, national, local, almostinternal, bogus, international],
    Classallowed = lists:member(Class, Classes),
    if
	User == false ->
	    false;
	User == stale ->
	    stale;
	Numberallowed /= true ->
	    logger:log(normal, "Number ~p not in ~p", [Number, Numberlist]),
	    false;
	Classallowed /= true ->
	    logger:log(normal, "Number ~p not allowed to call ~p in class ~p",
		       [Number, Tophone, Class]),
	    false;
	true ->
	    true
    end.

can_register(Header, Number) ->
    case get_user_verified(Header, "REGISTER") of
	false ->
	    {false, []};
	stale ->
	    {stale, []};
	User ->
	    {_, Numberlist, _, _} = get_passnumber(User),
	    case Number of
		User ->
		    {true, Numberlist};
		_ ->
		    {lists:member(Number, Numberlist), []}
	    end
    end.

check_and_send_auth(Header, Socket, Phone, Tophone, Func, Arg, Method, Classdefs) ->
    Class = get_class(Tophone, Classdefs),
    Classlist = [internal, local, bogus],
    Classallowed = lists:member(Class, Classlist),
    if
	Classallowed == true ->
	    apply(Func, [Header, Socket, Arg]);
	true ->
	    case check_auth(Header, Method, Phone, Tophone, Classdefs) of
		true -> 
		    apply(Func, [Header, Socket, Arg]);
		stale ->
		    siprequest:send_proxyauth_req(Header, Socket,
					     get_challenge(), true);
		false ->
		    siprequest:send_proxyauth_req(Header, Socket,
					     get_challenge(), false)
	    end
    end.
