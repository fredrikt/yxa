-module(admin_www).
-export([
	 start/2, 
	 start/1, 
	 list_phones/2, 
	 list_users/2, 
	 add_user/2, 
	 change_user_form/2,
	 change_user/2, 
	 add_route/2, 
	 del_route/2, 
	 wml/2, 
	 add_user_with_cookie/2,
	 change_classes/2, 
	 list_numbers/2, 
	 add_regexp/2, 
	 del_regexp/2,
	 show_user/2, 
	 set_forward/2
	]).

-include("phone.hrl").
-include("database_regexproute.hrl").
-include("siprecords.hrl").

start(normal, Args) ->
    Pid = sipserver:safe_spawn(admin_www, start, [sipserver:get_env(httpd_config)]),
    {ok, Pid}.

start(ConfigFile) ->
    mnesia:start(),
    logger:start_link(),
    directory:start_link(),
    mnesia:change_config(extra_db_nodes, sipserver:get_env(databaseservers)),
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

username_to_uid("*") ->
    "&nbsp;";
username_to_uid(Username) ->
    case local:get_users_for_address_of_record(Username) of
	none ->
	    "&nbsp;";
	String when list(String) ->
	    String;
	_ ->
	    "error"
    end.

username_to_cn("*") ->
    "&nbsp;";
username_to_cn(Username) ->
    Res = directory:lookup_mail2cn(Username),
    case Res of
	none ->
	    "&nbsp;";
	String when list(String) ->
	    String;
	E ->
	    "error"
    end.

print_flag(Key) when atom(Key) ->
    io_lib:format("~w<br>", [Key]);
print_flag({Key, Value}) ->
    io_lib:format("~w:~w<br>", [Key, Value]).

print_flags([]) ->
    "&nbsp;";
print_flags([Key]) ->
    [print_flag(Key)];
print_flags([Key | Rest]) ->
    [print_flag(Key) | print_flags(Rest)].

print_one_regexp(Regexp, Flags, Class, Expire, Address) when list(Regexp), record(Address, sipurl) ->
    ["<tr><td>",
     Regexp,
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
	      "<form action=\"admin_www%3Adel_regexp\" method=post>\n",
	      "<input type=\"hidden\" name=\"number\" value=\"",
	      Regexp, "\">\n",
	      "<input type=\"submit\" value=\"Ta bort\">\n",
	      "</form>\n"
	     ];
	 _ ->
	     ""
     end,
     "</td></tr>\n"
    ].

print_one_phone(Number, Flags, Class, Expire, Address) ->
    ["<tr><td>",
     Number,
     "</td><td>",
     print_flags(Flags),
     "</td><td>",
     atom_to_list(Class),
     "</td><td>",
     case util:timestamp() of
	 Time when Time > Expire ->
	     "<strong>";
	 _ ->
	     ""
     end,
     util:sec_to_date(Expire),
     case util:timestamp() of
	 Time when Time > Expire ->
	     "</strong>";
	 _ ->
	     ""
     end,
     "</td><td>",
     case Address of
	 _ when list(Number), record(Address, sipurl) ->
	     sipurl:print(Address);
	 {mailbox, Status, Number2} ->
	     ["mailbox ", Number2,
	      " message ", Status
	     ];
	 {error, Errorcode} ->
	     ["error ", integer_to_list(Errorcode)];
	_ when atom(Address) ->
	     atom_to_list(Address);
	 _ ->
	     Number
     end,
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
     "</td></tr>\n"].

print_one_forward({Forwards, Timeout, Localring}) ->
    Printfunc = fun (Number) ->
			[
			 "<tr><td>",
			 Number,
			 "</td></tr>"
			]
		end,
    ["<td>",
     case Timeout of
	 0 ->
	     "Vidarekoppla alltid till ";
	 _ ->
	     ["Vidarekoppla efter ", integer_to_list(Timeout), " sekunder till "]
     end,
     "</td><td>",
     "<table>",
     lists:map(Printfunc, Forwards),
     "</table>",
     "</td><td>",
     case Localring of
	 true ->
	     "Ring ocks&aring; p&aring; den egna telefonen ";
	 _ ->
	     ""
     end,
     "</td>\n"
    ];
print_one_forward(none) ->
    ["<td>",
     "Vidarekoppla aldrig"
     "</td>\n"
    ].

print_phones([]) -> [];

print_phones([{regexproute, Regexp, Flags, Class, Expire, Address} | List]) ->
    [print_one_regexp(Regexp, Flags, Class, Expire, Address) | print_phones(List)];

print_phones([{phone, Number, Flags, Class, Expire, Address} | List]) ->
    print_one_phone(Number, Flags, Class, Expire, Address) ++ print_phones(List).

print_phones_list([]) -> [];

print_phones_list([{Number, {Address, Flags, Class, Expire}} | List]) ->
    print_one_phone(Number, Flags, Class, Expire, Address) ++ print_phones_list(List).

print_classes([]) ->
    "&nbsp;";
print_classes([Class]) ->
    atom_to_list(Class);
print_classes([Class | List]) ->
    atom_to_list(Class) ++ ", " ++ print_classes(List).

print_numbers([]) ->
    "&nbsp;";
print_numbers([Number]) ->
    Number;
print_numbers([Number | List]) ->
    Number ++ "," ++ print_numbers(List).

print_users([]) -> [];

print_users([Elem | List]) ->
    ["<tr><td>",
     Elem#user.user,
     "</td><td>",
     print_numbers(get_numbers(Elem#user.user)),
     "</td><td>",
     print_flags(Elem#user.flags),
     "</td><td>",
     print_classes(lists:sort(Elem#user.classes)),
     "</td><td>",
     username_to_uid(Elem#user.user),
     "</td><td>",
     username_to_cn(Elem#user.user),
     "</td><td>",
     "<form action=\"admin_www%3Achange_user_form\" method=post>\n",
     "<input type=\"hidden\" name=\"user\" value=\"", Elem#user.user,"\" size=\"12\">\n",
     "<input type=\"submit\" value=\"&Auml;ndra\">\n",
     "</form>\n",
     "</td></tr>\n"
     | print_users(List)].

print_numbers_list([]) -> [];

print_numbers_list([Elem | List]) ->
    ["<tr><td>",
     Elem#numbers.number,
     "</td><td>",
     Elem#numbers.user,
     "</td><td>",
     case username_to_uid(Elem#numbers.user) of
	 String1 when list(String1) ->
	     String1;
	 _ ->
	     ""
     end,
     "</td><td>",
     case username_to_cn(Elem#numbers.user) of
	 String2 when list(String2) ->
	     String2;
	 _ ->
	     ""
     end,
     "</td></tr>\n"
     | print_numbers_list(List)].


get_pass(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    {none, [], []};
	{atomic, [A]} ->
	    A;
	{aborted, _} ->
	    {none, [], []}
    end.

user_exists(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    false;
	{atomic, [A]} ->
	    true;
	{aborted, _} ->
	    false
    end.

get_numbers(User) ->
    case phone:get_numbers_for_user(User) of
	{atomic, Numbers} ->
	    Numbers;
	{aborted, _} ->
	    []
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

check_auth_user(Env, User) ->
    case lists:keysearch(http_authorization, 1, Env) of
	false ->
	    {error, [header(unauth, false),
		     "Not authorized\r\n"
		    ]};
	{value, {Key, Value}} ->
	    case check_auth_user2(Value, Env, User) of
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
    Response = dict:fetch("response", Authorization),
    URI = dict:fetch("uri", Authorization),
    Nonce = dict:fetch("nonce", Authorization),
    Opaque = dict:fetch("opaque", Authorization),
    Timestamp = hex:from(Opaque),
    Now = util:timestamp(),
    User = dict:fetch("username", Authorization),
    {Password, Flags, Classes} = get_pass(User),
    Nonce2 = sipauth:get_nonce(Opaque),
    IsAdmin = lists:member(admin, Flags),
    Response2 = sipauth:get_response(Nonce2, Method,
				     URI,
				     User, Password),
    Response3 = sipauth:get_response(Nonce2, "GET",
				     URI,
				     User, Password),
    if 
	Password == "" ->
	    false;
	Response == Response2 ; Response == Response3 ->
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

check_auth_user2(Header, Env, InUser) ->
    Authorization = sipheader:auth([Header]),
    {value, {_, Method}} = lists:keysearch(request_method, 1, Env),
    Response = dict:fetch("response", Authorization),
    URI = dict:fetch("uri", Authorization),
    Nonce = dict:fetch("nonce", Authorization),
    Opaque = dict:fetch("opaque", Authorization),
    Timestamp = hex:from(Opaque),
    Now = util:timestamp(),
    User = dict:fetch("username", Authorization),
    {Password, Flags, Classes} = get_pass(User),
    Nonce2 = sipauth:get_nonce(Opaque),
    IsAdmin = lists:member(admin, Flags),
    Response2 = sipauth:get_response(Nonce2, Method,
				     URI,
				     User, Password),
    Response3 = sipauth:get_response(Nonce2, "GET",
				     URI,
				     User, Password),
    if 
	Password == "" ->
	    false;
	Response == Response2 ; Response == Response3 ->
	    if 
		User == InUser ->
		    true;
		true ->
		    false
	    end;
	true ->
	    false
    end.


header(ok) ->
    ["Content-type: text/html; charset=utf-8\r\n\r\n"].

header(unauth, Stale) ->
    Auth = sipauth:get_challenge(),
    Ret = ["WWW-Authenticate: " ++ sipheader:auth_print(Auth, Stale) ++ "\r\n",
	   "Status: 401 Authenticate\r\n",
	   "Content-type: text/html\r\n\r\n"],
    Ret;

header(redirect, URL) ->
    ["Location: " ++ URL ++ "\r\n\r\n"].

indexurl() ->
    sipserver:get_env(www_baseurl) ++ "index.html".

userurl() ->
    sipserver:get_env(www_baseurl) ++ "erl/admin_www%3Alist_users".

phonesurl() ->
    sipserver:get_env(www_baseurl) ++ "erl/admin_www%3Alist_phones".

showuserurl() ->
    sipserver:get_env(www_baseurl) ++ "erl/admin_www%3Ashow_user".

indexurl_html() ->
    "<a href=\"" ++ indexurl() ++ "\">Tillbaka till f&ouml;rstasidan</a>".

userurl_html() ->
    "<a href=\"" ++ userurl() ++ "\">Tillbaka till anv&auml;ndarlistan</a>".

list_users(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    List = phone:list_users(),
	    [header(ok),
	     "<table cellspacing=0 border=1 cellpadding=4>\n",
	     "<tr><th>Anv&auml;ndarnamn</th><th>Nummer</th><th>Flaggor</th>",
	     "<th>Klasser</th><th>KTH-ID</th><th>Namn</th></tr>\n",
	     print_users(lists:sort(fun (Elem1, Elem2) -> 
					    if
						Elem1#user.user < Elem2#user.user ->
						    true;
						true ->
						    false
					    end
				    end, List)),
	     "</table>\n",
	     "<p>\n",
	     "<form action=\"admin_www%3Aadd_user\" method=post>\n",
	     "Anv&auml;ndarnamn: <input type=\"text\" name=\"user\" size=\"12\">\n",
	     "<input type=\"submit\" value=\"Skapa anv&auml;ndare\">\n",
	     "</form>\n",
	     "<ul>\n",
	     "<li>internal: samtal inom KTH\n",
	     "<li>national: samtal inom Sverige\n",
	     "<li>international: utlandssamtal\n",
	     "<li>mobile: mobilsamtal\n",
	     "<li>pay: betalsamtal\n",
	     "</ul>\n",
	     indexurl_html()
	    ]
    end.

list_numbers(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    List = phone:list_numbers(),
	    PhoneList = phone:list_phones(),
	    List2 = lists:map(fun (Elem) ->
					  #numbers{number = Elem#phone.number, user="*"}
				  end, PhoneList),
	    [header(ok),
	     "<h1>Alla allokerade nummer</h1>\n",
	     "<table cellspacing=0 border=1 cellpadding=4>\n",
	     "<tr><th>Nummer</th><th>Anv&auml;ndarnamn</th><th>KTH-ID</th><th>Namn</th></tr>\n",
	     print_numbers_list(lists:sort(fun (Elem1, Elem2) -> 
						   if
						       Elem1#numbers.number < Elem2#numbers.number ->
							   true;
						       true ->
							   false
						   end
					   end, lists:append(List, List2))),
	     "</table>\n",
	     indexurl_html()
	    ]
    end.

list_phones(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    List = phone:list_phones(),
	    Regexps = database_regexproute:list(),
	    [header(ok),
	     "<h1>Nummertabell</h1>\n",
	     "<table cellspacing=0 border=1 cellpadding=4>\n",
	     "<tr><th>Nummer</th><th>Flaggor</th><th>Klass</th>",
	     "<th>G&aring;r ut</th><th>Adress</th></tr>\n",
	     print_phones(lists:sort(fun (Elem1, Elem2) -> 
					     if
						 Elem1#phone.number < Elem2#phone.number ->
						     true;
						 true ->
						     false
					     end
				     end, List)),
	     "</table>\n",
	     "<h1>L&auml;gg till medflyttning</h1>\n",
	     "<form action=\"admin_www%3Aadd_route\" method=post>\n",
	     "Nummer: <input type=\"text\" name=\"number\">\n",
	     "Prioritet: <input type=\"text\" name=\"priority\">\n",
	     "Adress: <input type=\"text\" name=\"address\">\n",
	     "<input type=\"submit\" value=\"L&auml;gg till\">\n",
	     "</form>\n",
	     "<h1>Regler</h1>\n",
	     "<table cellspacing=0 border=1 cellpadding=4>\n",
	     "<tr><th>Nummer</th><th>Flaggor</th><th>Klass</th>",
	     "<th>G&aring;r ut</th><th>Adress</th></tr>\n",
	     print_phones(lists:sort(fun (Elem1, Elem2) -> 
					     if
						 Elem1#regexproute.regexp < Elem2#regexproute.regexp ->
						     true;
						 true ->
						     false
					     end
				     end, Regexps)),
	     "</table>\n",
	     "<h1>L&auml;gg till regel</h1>\n",
	     "<form action=\"admin_www%3Aadd_regexp\" method=post>\n",
	     "Regel: <input type=\"text\" name=\"number\">\n",
	     "Prioritet: <input type=\"text\" name=\"priority\">\n",
	     "Adress: <input type=\"text\" name=\"address\">\n",
	     "<input type=\"submit\" value=\"L&auml;gg till\">\n",
	     "</form>\n",
	     indexurl_html()
	    ]
    end.

parse_classes(String) ->
    [internal].

add_user(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Userfind = dict:find("user", Args),
	    Classes = parse_classes(dict:find("classes", Args)),
	    case Userfind of
		error ->
		    [header(ok), "Felaktigt anv&auml;ndarnamn"];
		{ok, User} ->
		    phone:insert_user(User, none, [], Classes),
		    [header(redirect, userurl())]
	    end
    end.

%% XXX appears to be unused
add_route(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Numberfind = dict:find("number", Args),
	    Priorityfind = dict:find("priority", Args),
	    Addressfind = dict:find("address", Args),
	    %% XXX These two lines are untested
	    CallId = dict:find("call-id", Args),
	    CSeq = list_to_integer(dict:find("cseq", Args)),
	    case {Numberfind, Priorityfind, Addressfind} of
		{error, _, _} ->
		    [header(ok), "Du m&aring;ste ange ett nummer"];
		{_, error, _} ->
		    [header(ok), "Du m&aring;ste ange prioritet"];
		{_, _, error} ->
		    [header(ok), "Du m&aring;ste ange en adress"];
		{{ok, Number}, {ok, Priority}, {ok, ">" ++ Errorcode}} ->
		    phone:insert_purge_phone(Number,
					     [{priority,
					       list_to_integer(Priority)}],
					     permanent,
					     never,
					     {error, list_to_integer(Errorcode)},
					     CallId,
					     CSeq),
		    [header(redirect, phonesurl())];
		{{ok, Number}, {ok, Priority}, {ok, Address}} ->
		    phone:insert_purge_phone(Number,
					     [{priority,
					       list_to_integer(Priority)}],
					     permanent,
					     never,
					     sipurl:parse(Address),
					     CallId,
					     CSeq),
		    [header(redirect, phonesurl())]
	    end
    end.

add_regexp(Env, Input) ->
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
		    [header(ok), "Du m&aring;ste ange en regexp"];
		{_, error, _} ->
		    [header(ok), "Du m&aring;ste ange prioritet"];
		{_, _, error} ->
		    [header(ok), "Du m&aring;ste ange en adress"];
		{{ok, Number}, {ok, Priority}, {ok, Address}} ->
		    database_regexproute:insert(Number,
						[{priority,
						  list_to_integer(Priority)}],
						permanent,
						never,
						sipurl:parse(Address)),
		    [header(redirect, phonesurl())]
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
		    [header(ok), "Du m&aring;ste ange ett nummer"];
		{{ok, Number}} ->
		    phone:purge_class_phone(Number, permanent),
		    [header(redirect, phonesurl())]
	    end
    end.

del_regexp(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Numberfind = dict:find("number", Args),
	    case {Numberfind} of
		{error} ->
		    [header(ok), "Du m&aring;ste ange ett nummer"];
		{{ok, Number}} ->
		    database_regexproute:purge_class(Number, permanent),
		    [header(redirect, phonesurl())]
	    end
    end.


print_class_checkboxes(Classes) ->
    lists:map(fun (Class) ->
		      C = atom_to_list(Class),
		      Checked = case lists:member(Class, Classes) of
				    true ->
					"CHECKED";
				    _ ->
					""
				end,
		      ["<input type=\"checkbox\" name=\"class_", C, "\"",
		       Checked, ">", C, "<BR>"]
	      end,
	      lists:usort([internal, national, mobile, pay,
			   international | Classes])).

change_user_form(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Userfind = dict:find("user", Args),
	    case Userfind of
		error ->
		    [header(ok), "Felaktigt anv&auml;ndarnamn"];
		{ok, User} ->
		    {Password, Flags, Classes} = get_pass(User),
		    Numberlist = get_numbers(User),
		    Phonelist = lists:foldl(fun (A, AccIn) ->
						    {atomic, List} = phone:get_phone(A),
						    lists:append(AccIn, lists:map(fun (I) ->
											  {A, I}
										  end, List))
					    end, [], Numberlist),
		    [
		     header(ok),
		     "<h1>", username_to_cn(User), "(", User, ")", "</h1>\n",
		     "<h2>KTH-ID</h2>", username_to_uid(User),
		     case user_exists(User) of
			 true ->
			     ["<h2>Registrerade telefoner</h2>\n",
			      "<table cellspacing=0 border=1 cellpadding=4>\n",
			      "<tr><th>Nummer</th><th>Flaggor</th><th>Klass</th>",
			      "<th>G&aring;r ut</th><th>Adress</th></tr>\n",
			      print_phones_list(Phonelist),
			      "</table>\n",
			      "<h2>L&ouml;senord</h2>\n",
			      "<form action=\"admin_www%3Achange_user\" method=post>\n",
			      "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
			      "<input type=\"password\" name=\"password\" size=\"20\">\n",
			      "<input type=\"submit\" value=\"&Auml;ndra l&ouml;senord\">\n",
			      "</form>\n",
			      "<h2>Nummer</h2>\n",
			      "<form action=\"admin_www%3Achange_user\" method=post>\n",
			      "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
			      "<input type=\"text\" name=\"numbers\" size=\"40\" value=\"",
			      print_numbers(Numberlist),
			      "\">\n",
			      "<input type=\"submit\" value=\"&Auml;ndra nummer\">\n",
			      "</form>\n",
			      "<h2>Klasser</h2>\n",
			      "<form action=\"admin_www%3Achange_classes\" method=post>\n",
			      "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
			      print_class_checkboxes(Classes),
			      "<input type=\"submit\" value=\"&Auml;ndra klasser\">\n",
			      "</form>\n",
			      "<h2>Administrat&ouml;r</h2>\n",
			      "<form action=\"admin_www%3Achange_user\" method=post>\n",
			      "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
			      "<input type=\"hidden\" name=\"admin\" value=\"true\">\n",
			      "<input type=\"submit\" value=\"Sl&aring; p&aring; administrat&ouml;rsflaggan\">\n",
			      "</form>\n",
			      "<form action=\"admin_www%3Achange_user\" method=post>\n",
			      "<input type=\"hidden\" name=\"user\" value=\"", User, "\">\n",
			      "<input type=\"hidden\" name=\"admin\" value=\"false\">\n",
			      "<input type=\"submit\" value=\"Sl&aring; av administrat&ouml;rsflaggan\">\n",
			      "</form>\n"
			     ];
			 _ ->
			     [
			      "<h2>Anv&auml;ndaren existerar inte</h2>\n",
			      "<form action=\"admin_www%3Aadd_user\" method=post>\n",
			      "<input type=\"hidden\" name=\"user\" value=\"", User,"\">\n",
			      "<input type=\"submit\" value=\"Skapa\">\n",
			      "</form>\n"
			     ]
		     end,
		     userurl_html()
		    ]
	    end
    end.

print_disabled(none, never) ->
    "disabled";
print_disabled({Forwards, 0, Localring}, always) ->
    "disabled";
print_disabled({Forwards, Timeout, Localring}, noanswer) when Timeout > 0 ->
    "disabled";
print_disabled(_, _) ->
    "".

show_user_front(User, Numberlist) ->
    Printfunc = fun (Number) ->
			Forward = case database_forward:fetch(Number) of
				      {atomic, [F]} ->
					  F;
				      _ ->
					  none
				  end,
			[
			 "<h2>Vidarekoppling f&ouml;r ", Number, "</h2>\n",
			 "<h3>Nuvarande vidarekopplingsstatus</h3>\n",
			 "<table>",
			 "<tr>",
			 print_one_forward(Forward),
			 case Forward of
			     none ->
				 "";
			     _ ->
				 ["<td>",
				  "<form action=\"admin_www%3Ashow_user\" method=get>\n",
				  "<input type=\"hidden\" name=\"user\" value=\"",
				  User, "\">\n",
				  "<input type=\"hidden\" name=\"number\" value=\"",
				  Number, "\">\n",
				  "<input type=\"hidden\" name=\"type\" value=\"",
				  "change", "\">\n",
				  "<input type=\"submit\" value=\"&Auml;ndra\">\n",
				  "</form>\n",
				  "</td>"]
			 end,
			 "</tr>",
			 "</table>",
			 "<h3>&Auml;ndra vidarekopplingstyp</h3>",
			 "<table><tr><td>",
			 "<form action=\"admin_www%3Ashow_user\" method=get>\n",
			 "<input type=\"hidden\" name=\"user\" value=\"",
			 User, "\">\n",
			 "<input type=\"hidden\" name=\"number\" value=\"",
			 Number, "\">\n",
			 "<input type=\"hidden\" name=\"type\" value=\"",
			 "never", "\">\n",
			 "<input ",
			 print_disabled(Forward, never),
			 " type=\"submit\" value=\"Vidarekoppla aldrig\">\n",
			 "</form>\n",
			 "</td><td>",
			 "<form action=\"admin_www%3Ashow_user\" method=get>\n",
			 "<input type=\"hidden\" name=\"user\" value=\"",
			 User, "\">\n",
			 "<input type=\"hidden\" name=\"number\" value=\"",
			 Number, "\">\n",
			 "<input type=\"hidden\" name=\"type\" value=\"",
			 "always", "\">\n",
			 "<input ",
			 print_disabled(Forward, always),
			 " type=\"submit\" value=\"Vidarekoppla alltid\">\n",
			 "</form>\n",
			 "</td><td>",
			 "<form action=\"admin_www%3Ashow_user\" method=get>\n",
			 "<input type=\"hidden\" name=\"user\" value=\"",
			 User, "\">\n",
			 "<input type=\"hidden\" name=\"number\" value=\"",
			 Number, "\">\n",
			 "<input type=\"hidden\" name=\"type\" value=\"",
			 "noanswer", "\">\n",
			 "<input ",
			 print_disabled(Forward, noanswer),
			 " type=\"submit\" value=\"Vidarekoppla vid ej svar\">\n",
			 "</form>\n"
			 "</td></tr></table>"
			]
		end,
    [
     header(ok),
     "<h1>", username_to_cn(User), "(", User, ")", "</h1>\n",
     lists:map(Printfunc, Numberlist),
     indexurl_html()
    ].

forward_to_typestring(none) ->
    "never";
forward_to_typestring({Forwards, 0, Localring}) ->
    "always";
forward_to_typestring({Forwards, _, Localring}) ->
    "noanswer".

show_user_number(User, Number, "never") ->
    [
     header(ok),
     "<h1>", username_to_cn(User), "(", User, ")", "</h1>\n",
     "<h2>Byt vidarekopplingstyp f&ouml;r ", Number, "</h2>\n",
     "Byt till vidarekoppla aldrig<br>",

     "<form action=\"admin_www%3Aset_forward\" method=post>\n",
     "<input type=\"hidden\" name=\"user\" value=\"",
     User, "\">\n",
     "<input type=\"hidden\" name=\"number\" value=\"",
     Number, "\">\n",
     "<input type=\"hidden\" name=\"type\" value=\"",
     "never", "\">\n",
     "<input type=\"submit\" value=\"OK\">\n",
     "</form>\n",
     "<form action=\"admin_www%3Ashow_user\" method=post>\n",
     "<input type=\"hidden\" name=\"user\" value=\"",
     User, "\">\n",
     "<input type=\"submit\" value=\"Avbryt\">\n",
     "</form>\n",
     indexurl_html()
    ];

show_user_number(User, Number, InType) ->
    Forward = case database_forward:fetch(Number) of
		  {atomic, [F]} ->
		      F;
		  _ ->
		      none
	      end,
    {Forwards, Timeout, Localring} = case Forward of
					 none ->
					     {[""], 20, false};
					 _ ->
					     Forward
				     end,
    Type = case InType of
	       "change" ->
		   forward_to_typestring(Forward);
	       _ ->
		   InType
	   end,
    [
     header(ok),
     "<h1>", username_to_cn(User), "(", User, ")", "</h1>\n",
     case InType of
	 "change" ->
	     ["<h2>&Auml;ndra vidarekoppling f&ouml;r ", Number, "</h2>\n"];
	 _ ->
	     ["<h2>Byt vidarekopplingstyp f&ouml;r ", Number, "</h2>\n"]
     end,

     "<form action=\"admin_www%3Aset_forward\" method=post>\n",
     "Vidarekoppla till ",
     "<input type=\"text\" name=\"forwardnumber\" value=\"",
     hd(Forwards),
     "\">",
     case Type of
	 "noanswer" ->
	     T = case InType of
		     "change" ->
			 integer_to_list(Timeout);
		     _ ->
			 "20"
		 end,
	     [
	      "<br>efter <input type=\"text\" name=\"timeout\" value=\"",
	      T,
	      "\"> sekunder"
	     ];
	 _ ->
	     ""
     end,
     "<br>\n",
     case Type of
	 "noanswer" ->
	     "Forts&auml;tt ring p&aring; min vanliga telefon ";
	 "always" ->
	     "Ring ocks&aring; p&aring; min vanliga telefon "
     end,
     "<input type=\"checkbox\" name=\"localring\"",
     case Localring of
	 true ->
	     "checked ";
	 _ ->
	     ""
     end,
     "value=\"true\"><br>\n",
     "<br>\n",
     "<input type=\"hidden\" name=\"user\" value=\"",
     User, "\">\n",
     "<input type=\"hidden\" name=\"number\" value=\"",
     Number, "\">\n",
     "<input type=\"hidden\" name=\"type\" value=\"",
     Type, "\">\n",
     "<input type=\"submit\" value=\"OK\">\n",
     "</form>\n",
     "<form action=\"admin_www%3Ashow_user\" method=post>\n",
     "<input type=\"hidden\" name=\"user\" value=\"",
     User, "\">\n",
     "<input type=\"submit\" value=\"Avbryt\">\n",
     "</form>\n",
     indexurl_html()
    ].

httparg(Env, Input) ->
    case lists:keysearch(request_method, 1, Env) of
	{value, {_, "GET"}} ->
	    case lists:keysearch(query_string, 1, Env) of
		{value, {_, Query}} ->
		    sipheader:httparg(Query)
	    end;
	{value, {Key, "POST"}} ->
	    sipheader:httparg(Input)
    end.

show_user(Env, Input) ->
    Args = httparg(Env, Input),
    {ok, User} = dict:find("user", Args),

    case check_auth_user(Env, User) of
	{error, Message} ->
	    Message;
	{ok} ->
	    {Password, Flags, Classes} = get_pass(User),
	    Numberlist = get_numbers(User),
	    case dict:find("number", Args) of
		error ->
		    show_user_front(User, Numberlist);
		{ok, Number} ->
		    case lists:member(Number, Numberlist) of
			false ->
			    [header(ok),
			     "<h1>Not allowed to change number</h1>"
			    ];
			_ ->
			    {ok, Type} = dict:find("type", Args),
			    show_user_number(User, Number, Type)
		    end
	    end
    end.

set_forward_type(Number, "never", _, _, _) ->
    database_forward:delete(Number);
set_forward_type(Number, "always", Forwards, _, Localring) ->
    database_forward:insert(Number, Forwards, 0, Localring);
set_forward_type(Number, "noanswer", Forwards, Timeout, Localring) ->
    database_forward:insert(Number, Forwards, Timeout, Localring).

dict_find(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
	{ok, Value} ->
	    Value;
	_ ->
	    Default
    end.

set_forward(Env, Input) ->
    Args = httparg(Env, Input),
    {ok, User} = dict:find("user", Args),
    {ok, Number} = dict:find("number", Args),
    {ok, Type} = dict:find("type", Args),

    case check_auth_user(Env, User) of
	{error, Message} ->
	    Message;
	{ok} ->
	    {Password, Flags, Classes} = get_pass(User),
	    
	    Numberlist = get_numbers(User),
	    
	    true = lists:member(Number, Numberlist),
	    
	    Forwards = [dict_find("forwardnumber", Args, "")],
	    TimeoutText = dict_find("timeout", Args, "20"),
	    Timeout = case util:isnumeric(TimeoutText) of
			  true ->
			      list_to_integer(TimeoutText);
			  _ ->
			      20
		      end,
	    Localring = list_to_atom(dict_find("localring", Args, "false")),
	    
	    set_forward_type(Number, Type, Forwards, Timeout, Localring),
	    header(redirect, showuserurl() ++ "?user=" ++ User)
    end.

set_admin(User, "true") ->
    {Password, Flags, Classes} = get_pass(User),
    case lists:member(admin, Flags) of
	false ->
	    phone:set_user_flags(User, [admin | Flags]);
	_ ->
	    true
    end;
set_admin(User, "false") ->
    {Password, Flags, Classes} = get_pass(User),
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
    Cookie = cookie(), % this makes the match below fail if the cookie is incorrect
    ["Content-type: text/plain\r\n\r\n",
     case {Userfind, Passwordfind, Cookiefind} of
	 {error, _, _} ->
	     "ERROR";
	 {_, error, _} ->
	     "ERROR";
	 {_, _, error} ->
	     "ERROR";
	 {{ok, User}, {ok, Password}, {ok, Cookie}} ->
	     case phone:insert_user_or_password(User, Password) of
		 {atomic, ok} ->
		     "CREATED";
		 _ ->
		     "ERROR"
	     end;
	 _ ->
	     "FORBIDDEN"
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
		    [header(ok), "Du m&aring;ste ange ett anv&auml;ndarnamn"];
		{_, error, error, error} ->
		    [header(ok), "Du m&aring;ste ange l&ouml;senord, admin eller nummer"];
		{{ok, User}, {ok, Password}, _, _} ->
		    phone:set_user_password(User, Password),
		    [header(redirect, userurl())];
		{{ok, User}, _, {ok, Admin}, _} ->
		    set_admin(User, Admin),
		    [header(redirect, userurl())];
		{{ok, User}, _, _, {ok, Numbers}} ->
		    Numberlist = string:tokens(Numbers, ","),
		    phone:set_user_numbers(User, Numberlist),
		    [header(redirect, userurl())]
	    end
    end.

findclasses(Args) ->
    List = lists:filter(fun (A) ->
				case A of
				    {"class_" ++ Class, _} ->
					true;
				    _ ->
					false
				end
			end,
			dict:to_list(Args)),
    lists:map(fun (A) ->
		      case A of
			  {"class_" ++ Class, _} ->
			      list_to_atom(Class)
		      end
	      end,
	      List).

change_classes(Env, Input) ->
    case check_auth(Env, true) of
	{error, Message} ->
	    Message;
	{ok} ->
	    Args = sipheader:httparg(Input),
	    Userfind = dict:find("user", Args),
	    Classes = findclasses(Args),
	    case Userfind of
		error ->
		    [header(ok), "Du m&aring;ste ange ett anv&auml;ndarnamn"];
		{ok, User} ->
		    phone:set_user_classes(User, Classes),
		    [header(redirect, userurl())]
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
