-module(admin_www).
-export([start/2, start/1, list_phones/2, list_users/2, add_user/2, change_user_form/2, change_user/2, add_route/2, del_route/2, wml/2, add_user_with_cookie/2]).

-include("phone.hrl").

server_node() -> 'incomingproxy@granit.e.kth.se'.

start(normal, Args) ->
    Pid = spawn(admin_www, start, ["/afs/e.kth.se/home/2002/map/sip/erlang/kth.se/httpd.conf"]),
    {ok, Pid}.

start(ConfigFile) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [server_node()]),
    {Message, Args} = case mnesia:wait_for_tables([phone,user], infinity) of
			  ok ->
			      {"web server started, all tables found~n", []};
			  {timeout, BadTabList} ->
			      {"web server started, tables not reachable right now: ~p~n", BadTabList}
		      end,
    httpd:start(ConfigFile),
    io:format(Message, Args),
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
	      "</td><td>",
	      case Class of
		  permanent ->
		      [
		       "<form action=\"admin_www%3Adel_route\" method=post>\n",
		       "<input type=\"hidden\" name=\"number\" value=\"",
		       Number, "\">\n",
		       "<input type=\"submit\" value=\"Ta bort\">\n",
		       "</form>\n"
		      ];
		  _ ->
		      ""
	      end,
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

print_numbers([]) ->
    [];
print_numbers([Number]) ->
    Number;
print_numbers([Number | List]) ->
    Number ++ "," ++ print_numbers(List).

print_users([]) -> [];

print_users([{user, User, Password, Number, Flags, Classes} | List]) ->
    ["<tr><td>",
     User,
     "</td><td>",
     print_numbers(Number),
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
    io:format("in: ~p~n", [Header]),
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
    io:format("nonce: ~p ~p~n", [Nonce, Nonce2]),
    Response2 = sipauth:get_response(Nonce2, Method,
				     URI,
				     User, Password),
    io:format("response: ~p ~p~n", [Response, Response2]),
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
    Ret = ["WWW-Authenticate: " ++ sipheader:auth_print(Auth, Stale) ++ "\r\n",
	   "Status: 401 Authenticate\r\n",
	   "Content-type: text/html\r\n\r\n"],
    io:format("out: ~p~n", [lists:flatten(Ret)]),
    Ret;

header(redirect, URL) ->
    ["Location: " ++ URL ++ "\r\n\r\n"].

indexurl() ->
    "<a href=\"https://granit.e.kth.se:8080/index.html\">Tillbaka till förstasidan</a>".

userurl() ->
    "<a href=\"https://granit.e.kth.se:8080/erl/admin_www%3Alist_users\">Tillbaka till användarlistan</a>".

list_users(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    {atomic, List} = phone:list_users(),
	    [header(ok),
	     "<table cellspacing=0 border=1 cellpadding=4>\n",
	     "<tr><th>Namn</th><th>Nummer</th><th>Flaggor</th>",
	     "<th>Klasser</th></tr>\n",
	     print_users(List),
	     "</table>\n",
	     "<form action=\"admin_www%3Aadd_user\" method=post>\n",
	     "<input type=\"text\" name=\"user\" size=\"12\">\n",
	     "<input type=\"text\" name=\"phone\" size=\"4\">\n",
	     "<input type=\"submit\" value=\"Add\">\n",
	     "</form>\n",
	     "<ul>\n",
	     "<li>internal: samtal inom KTH\n",
	     "<li>almostinternal: samtal till t.ex. SU, nummer som börjar på 0\n",
	     "<li>local: lokalsamtal inom 08-området\n",
	     "<li>national: samtal inom Sverige\n",
	     "<li>international: utlandssamtal\n",
	     "<li>mobile_or_pay: alla samtal som börjar på 0007\n",
	     "</ul>\n",
	     indexurl()
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
	     "<tr><th>Nummer</th><th>Flaggor</th><th>Klass</th>",
	     "<th>Går ut</th><th>Adress</th></tr>\n",
	     print_phones(lists:sort(fun (Elem1, Elem2) -> 
					     if
						 Elem1#phone.number < Elem2#phone.number ->
						     true;
						 true ->
						     false
					     end
				     end, List)),
	     "</table>\n",
	     "<h1>Lägg till route</h1>\n",
	     "<form action=\"admin_www%3Aadd_route\" method=post>\n",
	     "Nummer: <input type=\"text\" name=\"number\">\n",
	     "Prioritet: <input type=\"text\" name=\"priority\">\n",
	     "Adress: <input type=\"text\" name=\"address\">\n",
	     "<input type=\"submit\" value=\"Lägg till\">\n",
	     "</form>\n",
	     indexurl()
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
		    [header(ok), "Felaktigt användarnamn"];
		{_, error} ->
		    [header(ok), "Felaktigt telefonnummer"];
		{{ok, User}, {ok, Phone}} ->
		    phone:insert_user(User, none, [Phone], [], Classes),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_users")]
	    end
    end.

add_route(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Numberfind = dict:find("number", Args),
	    Priorityfind = dict:find("priority", Args),
	    Addressfind = dict:find("address", Args),
	    case {Numberfind, Priorityfind, Addressfind} of
		{error, _, _} ->
		    [header(ok), "Du måste ange ett nummer"];
		{_, error, _} ->
		    [header(ok), "Du måste ange prioritet"];
		{_, _, error} ->
		    [header(ok), "Du måste ange en adress"];
		{{ok, Number}, {ok, Priority}, {ok, Address}} ->
		    phone:insert_purge_phone(Number,
					     [{priority,
					       list_to_integer(Priority)}],
					     permanent,
					     never,
					     sipurl:parse(Address)),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_phones")]
	    end
    end.

del_route(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Numberfind = dict:find("number", Args),
	    case {Numberfind} of
		{error} ->
		    [header(ok), "Du måste ange ett nummer"];
		{{ok, Number}} ->
		    phone:purge_class_phone(Number, permanent),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_phones")]
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
		    [header(ok), "Felaktigt användarnamn"];
		{ok, User} ->
		    {Password, Numberlist, Flags, Classes} = get_passnumber(User),
		    [
		     header(ok),
		     "<h1>", User, "</h1>\n",
		     "Password:\n",
		     "<form action=\"admin_www%3Achange_user\" method=post>\n",
		     "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
		     "<input type=\"password\" name=\"password\" size=\"20\">\n",
		     "<input type=\"submit\" value=\"Ändra lösenord\">\n",
		     "</form>\n",
		     "<form action=\"admin_www%3Achange_user\" method=post>\n",
		     "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
		     "<input type=\"text\" name=\"numbers\" size=\"40\" value=\"",
		     print_numbers(Numberlist),
		     "\">\n",
		     "<input type=\"submit\" value=\"Ändra nummer\">\n",
		     "</form>\n",
		     "<form action=\"admin_www%3Achange_user\" method=post>\n",
		     "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
		     "<input type=\"hidden\" name=\"admin\" value=\"true\">\n",
		     "<input type=\"submit\" value=\"Slå på administratörsflaggan\">\n",
		     "</form>\n",
		     "<form action=\"admin_www%3Achange_user\" method=post>\n",
		     "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
		     "<input type=\"hidden\" name=\"admin\" value=\"false\">\n",
		     "<input type=\"submit\" value=\"Slå av administratörsflaggan\">\n",
		     "</form>\n",
		     userurl()
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

cookie() ->
    {ok, Data} = file:read_file("www-cookie"),
    binary_to_list(Data).

add_user_with_cookie(Env, Input) ->
    Args = sipheader:httparg(Input),
    Userfind = dict:find("user", Args),
    Passwordfind = dict:find("password", Args),
    Cookiefind = dict:find("cookie", Args),
    Cookie = cookie(),
    ["Content-type: text/plain\r\n\r\n",
     case {Userfind, Passwordfind, Cookiefind} of
	 {error, _, _} ->
	     "FORBIDDEN";
	 {_, error, _} ->
	     "FORBIDDEN";
	 {_, _, error} ->
	     "FORBIDDEN";
	 {{ok, User}, {ok, Password}, {ok, Cookie}} ->
	     Classes = [internal],
	     phone:insert_user(User, Password, [], [], Classes),
	     "CREATED"
     end
    ].

change_user(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Userfind = dict:find("user", Args),
	    Passwordfind = dict:find("password", Args),
	    Adminfind = dict:find("admin", Args),
	    Numbersfind = dict:find("numbers", Args),
	    case {Userfind, Passwordfind, Adminfind, Numbersfind} of
		{error, _, _, _} ->
		    [header(ok), "Du måste ange ett användarnamn"];
		{_, error, error, error} ->
		    [header(ok), "Du måste ange lösenord, admin eller nummer"];
		{{ok, User}, {ok, Password}, _, _} ->
		    phone:set_user_password(User, Password),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_users")];
		{{ok, User}, _, {ok, Admin}, _} ->
		    set_admin(User, Admin),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_users")];
		{{ok, User}, _, _, {ok, Numbers}} ->
		    Numberlist = string:tokens(Numbers, ","),
		    phone:set_user_numbers(User, Numberlist),
		    [header(redirect, "https://granit.e.kth.se:8080/erl/admin_www%3Alist_users")]
	    end
    end.

wml(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    ["Content-type: text/vnd.wap.wml\r\n\r\n",
	     "<wml>\r\n",
	     "<card id=\"test\" title=\"Test\">",
	     "<p>",
	     "Hello, this is WML",
	     "</p>",
	     "</card>",
	     "</wml>"
	    ]
    end.
