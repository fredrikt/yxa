-module(admin_www).
-export([start/2, start/1, list_phones/2, list_users/2, add_user/2, change_user_form/2, change_user/2]).

server_node() -> 'incomingproxy@granit.e.kth.se'.

start(normal, Args) ->
    Pid = spawn(admin_www, start, ["/afs/e.kth.se/home/2002/map/sip/erlang/kth.se/httpd.conf"]),
    {ok, Pid}.

start(ConfigFile) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [server_node()]),
    httpd:start(ConfigFile),
    sleep().

sleep() ->
    timer:sleep(1000000),
    sleep().

print_flags([]) ->
    [];
print_flags([Key | Rest]) when atom(Key) ->
    [io_lib:format("~w<br>", [Key]) | print_flags(Rest)];
print_flags([{Key, Value} | Rest]) ->
    [io_lib:format("~w:~w<br>", [Key, Value]) | print_flags(Rest)].

print_phones([]) -> [];

print_phones([{phone, Number, Flags, Class, Expire, Address} | List]) ->
    [case Address of
	 {_, _, _, _, _} when list(Number) ->
	     ["<tr><td>",
	      Number,
	      "</td><td>",
	      print_flags(Flags),
	      "</td><td>",
	      atom_to_list(Class),
	      "</td><td>",
	      util:sec_to_date(Expire),
	      "</td><td>",
	      sipurl:print(Address),
	      "</td></tr>\n"
	     ];
	{mailbox, Status, Number2} ->
	     ["<tr><td>",
	      Number,
	      "</td><td>",
	      print_flags(Flags),
	      "</td><td>",
	      atom_to_list(Class),
	      "</td><td>",
	      util:sec_to_date(Expire),
	      "</td><td>",
	      "mailbox ", Number2,
	      " message ", Status,
	      "</td></tr>\n"
	     ];
	_ when atom(Address) ->
	     ["<tr><td>",
	      Number,
	      "</td><td>",
	      print_flags(Flags),
	      "</td><td>",
	      atom_to_list(Class),
	      "</td><td>",
	      util:sec_to_date(Expire),
	      "</td><td>",
	      atom_to_list(Address),
	      "</td></tr>\n"
	     ];
	 _ ->
	     ["<tr><td>",
	      Number,
	      "</td></tr>\n"]
     end | print_phones(List)].

print_classes([]) ->
    [];
print_classes([Class]) ->
    atom_to_list(Class);
print_classes([Class | List]) ->
    atom_to_list(Class) ++ ", " ++ print_classes(List).

print_users([]) -> [];

print_users([{user, User, Password, Number, Flags, Classes} | List]) ->
    ["<tr><td>",
     User,
     "</td><td>",
     Number,
     "</td><td>",
     print_flags(Flags),
     "</td><td>",
     print_classes(Classes),
     "</td><td>",
     "<form action=\"admin_www%3Achange_user_form\" method=post>\n",
     "<input type=\"hidden\" name=\"user\" value=\"", User,"\" size=\"12\">\n",
     "<input type=\"submit\" value=\"Change\">\n",
     "</form>\n",
     "</td></tr>\n"
     | print_users(List)].


get_passnumber(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    {none, [], [], []};
	{atomic, [A]} ->
	    A;
	{aborted, _} ->
	    {none, [], [], []}
    end.

check_auth(Env, WantAdmin) ->
    case lists:keysearch(http_authorization, 1, Env) of
	false ->
	    {error, [header(unauth, false),
		     "Not authorized\r\n"
		    ]};
	{value, {Key, Value}} ->
	    case check_auth2(Value, Env, WantAdmin) of
		true ->
		    {ok};
		false ->
		    {error, [header(unauth, false),
			     "Not authorized\r\n"
			    ]}
	    end
    end.

check_auth2(Header, Env, WantAdmin) ->
    Authorization = sipheader:auth([Header]),
    {value, {_, Method}} = lists:keysearch(request_method, 1, Env),
    {value, {_, URI}} = lists:keysearch(script_name, 1, Env),
    Response = dict:fetch("response", Authorization),
    Nonce = dict:fetch("nonce", Authorization),
    Opaque = dict:fetch("opaque", Authorization),
    Timestamp = hex:from(Opaque),
    Now = util:timestamp(),
    User = dict:fetch("username", Authorization),
    {Password, Numberlist, Flags, Classes} = get_passnumber(User),
    Nonce2 = sipauth:get_nonce(Opaque),
    IsAdmin = lists:member(admin, Flags),
    Response2 = sipauth:get_response(Nonce2, Method,
				     URI,
				     User, Password),
    if 
	Password == "" ->
	    false;
	Response == Response2 ->
	    if 
		WantAdmin == true ->
		    if
			IsAdmin == true ->
			    true;
					  true ->
			    false
		    end;
		true ->
		    true
	    end;
	true ->
	    false
    end.


header(ok) ->
    ["Content-type: text/html\r\n\r\n"].

header(unauth, Stale) ->
    Auth = sipauth:get_challenge(),
    ["WWW-Authenticate: " ++ sipheader:auth_print(Auth, Stale) ++ "\r\n",
     "Status: 401 Authenticate\r\n",
     "Content-type: text/html\r\n\r\n"];

header(redirect, URL) ->
    ["Location: " ++ URL ++ "\r\n\r\n"].

list_users(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    {atomic, List} = phone:list_users(),
	    [header(ok),
	     "<table cellspacing=0 border=1 cellpadding=4>\n",
	     "<tr><th>Name</th><th>Numbers</th><th>Flags</th>",
	     "<th>Classes</th></tr>\n",
	     print_users(List),
	     "</table>\n",
	     "<form action=\"admin_www%3Aadd_user\" method=post>\n",
	     "<input type=\"text\" name=\"user\" size=\"12\">\n",
	     "<input type=\"text\" name=\"phone\" size=\"4\">\n",
	     "<input type=\"submit\" value=\"Add\">\n",
	     "</form>\n"
	    ]
    end.

list_phones(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    {atomic, List} = phone:list_phones(),
	    [header(ok),
	     "<table cellspacing=0 border=1 cellpadding=4>\n",
	     "<tr><th>Number</th><th>Flags</th><th>Class</th>",
	     "<th>Expire</th><th>Address</th></tr>\n",
	     print_phones(List),
	     "</table>\n"
	    ]
    end.

parse_classes(String) ->
    [internal, mobile_or_pay, national, local, almostinternal].

add_user(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Userfind = dict:find("user", Args),
	    Phonefind = dict:find("phone", Args),
	    Classes = parse_classes(dict:find("classes", Args)),
	    case {Userfind, Phonefind} of
		{error, _} ->
		    [header(ok), "Incorrect user name"];
		{_, error} ->
		    [header(ok), "Incorrect phone"];
		{{ok, User}, {ok, Phone}} ->
		    phone:insert_user(User, none, [Phone], [], Classes),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_users")]
	    end
    end.

change_user_form(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Userfind = dict:find("user", Args),
	    case Userfind of
		{error, _} ->
		    [header(ok), "Incorrect user name"];
		{ok, User} ->
		    [
		     header(ok),
		     "<h1>", User, "</h1>\n",
		     "Password:\n",
		     "<form action=\"admin_www%3Achange_user\" method=post>\n",
		     "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
		     "<input type=\"password\" name=\"password\" size=\"20\">\n",
		     "<input type=\"submit\" value=\"Change password\">\n",
		     "</form>\n",
		     "<form action=\"admin_www%3Achange_user\" method=post>\n",
		     "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
		     "<input type=\"hidden\" name=\"admin\" value=\"true\">\n",
		     "<input type=\"submit\" value=\"Set admin\">\n",
		     "</form>\n",
		     "<form action=\"admin_www%3Achange_user\" method=post>\n",
		     "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
		     "<input type=\"hidden\" name=\"admin\" value=\"false\">\n",
		     "<input type=\"submit\" value=\"Clear admin\">\n",
		     "</form>\n"
		    ]
	    end
    end.
    
set_admin(User, "true") ->
    {Password, Numberlist, Flags, Classes} = get_passnumber(User),
    case lists:member(admin, Flags) of
	false ->
	    phone:set_user_flags(User, [admin | Flags]);
	_ ->
	    true
    end;
set_admin(User, "false") ->
    {Password, Numberlist, Flags, Classes} = get_passnumber(User),
    case lists:member(admin, Flags) of
	false ->
	    true;
	_ ->
	    phone:set_user_flags(User, lists:delete(admin, Flags))
    end.

change_user(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Userfind = dict:find("user", Args),
	    Passwordfind = dict:find("password", Args),
	    Adminfind = dict:find("admin", Args),
	    case {Userfind, Passwordfind, Adminfind} of
		{error, _, _} ->
		    [header(ok), "Incorrect user name"];
		{_, error, error} ->
		    [header(ok), "Must "];
		{{ok, User}, {ok, Password}, _} ->
		    phone:set_user_password(User, Password),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_users")];
		{{ok, User}, _, {ok, Admin}} ->
		    set_admin(User, Admin),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_users")]
	    end
    end.
