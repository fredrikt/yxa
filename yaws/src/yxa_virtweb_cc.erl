%%%-------------------------------------------------------------------
%%% File    : yxa_virtweb_cc.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Virtweb common code.
%%%
%%% @since    15 Oct 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(yxa_virtweb_cc).

%% General functions
-export([
	 script_output/2,

	 get_logged_in_user/5,
	 get_logged_in_user/6,
	 logout/3,
	 get_user_domains/2,

	 check_is_allowed_write/4,
	 check_is_allowed_write2/2,

	 do_write_user/4,

	 rpc_call/4
	]).

-include("siprecords.hrl").
-include("yxa_virtweb.hrl").

%% @type session() = #session{}.
%%                   cookie data
-record(session, {user,		%% string()
		  password	%% string()
		 }).



%%--------------------------------------------------------------------
%% @spec    (Page, In) ->
%%            EHTML
%%
%%            Page = string()
%%            In   = list() | tuple() "Yaws EHTML"
%%
%%            EHTML = term() "Yaws ehtml data"
%%
%% @doc     Do top level control of what is to be outputted.
%% @end
%%--------------------------------------------------------------------
script_output(_Page, {ehtml, EHTML}) when is_list(EHTML) ->
    EHTML;
script_output(_Page, {redirect, Link}) when is_list(Link) ->
    yaws_api:redirect(Link);
script_output(Page, In) when is_list(Page), is_list(In); is_tuple(In) ->
    Title = case Page of
		[] -> {h2, [], "Self administration :"};
		_ -> {h2, [], ["Self administration - ", Page, " :"]}
	    end,
    LoggedInInfo =
	case get({yxa_virtweb_cc, logged_in_as}) of
	    User when is_list(User) ->
		["Logged in as : ", {strong, [], User}, " | ",
		 {a, [{href, ?STARTPAGE_URL
		       "?" ? VARNAME_VIRTACTION "=logout"
		      }], "Log out"}
		];
	    _ ->
		"Not logged in"
	end,
    TitleTable = [{table, [{border, 0}], [{tr, [], [{td, [], Title},
						    {td, [{align, "right"}], LoggedInInfo}
						   ]}
					 ]}],
    EHTML = [TitleTable,
	     {p, [], []},
	     In
	    ],
    {ehtml, EHTML}.


%%--------------------------------------------------------------------
%% @spec    (Arg, ArgRI, HeadersRI, HTTPReqRI, Node) ->
%%            Result
%%
%%            Result = term() "\"see get_logged_in_user/6\""
%%
%% @equiv get_logged_in_user(Arg, ArgRI, HeadersRI, HTTPReqRI, Node, true)
%% @end
%%--------------------------------------------------------------------
get_logged_in_user(Arg, ArgRI, HeadersRI, HTTPReqRI, Node) ->
    get_logged_in_user(Arg, ArgRI, HeadersRI, HTTPReqRI, Node, true).

%%--------------------------------------------------------------------
%% @spec    (Arg, ArgRI, HeadersRI, HTTPReqRI, Node, Required) ->
%%            {ok, User}             |
%%            {not_logged_in, EHTML} |
%%            {logged_in, EHTML}     |
%%            not_logged_in          
%%
%%            Arg       = #arg{} "Yaws request data structure"
%%            ArgRI     = [atom()] "record_info(fields, arg)"
%%            HeadersRI = [atom()] "record_info(fields, headers)"
%%            HTTPReqRI = [atom()] "record_info(fields, http_request)"
%%            Node      = atom() "Yxa application node"
%%            Required  = true | false
%%
%%            User   = string()
%%            EHTML  = term() "Yaws ehtml data"
%%            Reason = string()
%%
%% @throws  {error, Reason} 
%%
%% @doc     Get the name of the currently logged in user, or create a
%%          login page to show to the user that needs to log in.
%% @end
%%--------------------------------------------------------------------
get_logged_in_user(Arg, ArgRI, HeadersRI, HTTPReqRI, Node, Required) ->
    case get_logged_in_user2(Arg, ArgRI, HeadersRI, Node) of
	{ok, User} ->
	    put({yxa_virtweb_cc, logged_in_as}, User),
	    {ok, User};
	{logged_in, EHTML} ->
	    {logged_in, EHTML};
	not_logged_in ->
	    case Required of
		true ->
		    LoginForm = login_form(Arg, ArgRI, HTTPReqRI),
		    {not_logged_in, LoginForm};
		false ->
		    not_logged_in
	    end
    end.

login_form(Arg, ArgRI, HTTPReqRI) ->
    %% have to extract in two steps as element req is a http_request record()
    {ok, HTReq} = get_yaws_arg_element(Arg, arg, [req], [ArgRI]),
    {ok, {abs_path, Me}} = get_yaws_arg_element(HTReq, http_request, [path], [HTTPReqRI]),

    UserVal = case yxa_yaws_util:get_var(Arg, ?VARNAME_LOGIN_AUTHUSER) of
		  {ok, UserVal1} ->
		      UserVal1;
		  undefined ->
		      ""
	      end,

    [{h1, [], ["Login"]},
     case UserVal of
	 "" -> [];
	 _ ->
	     {font, [{color, "red"}], [{strong, [], "Invalid username and/or password"}]}
     end,
     {form,
      [{method, "post"},
       {action, Me}
      ], [
	  {table, [{border, 0}],
	   [
	    {tr, [], [
		      {td, [], "Username"},
		      {td, [], [{input,
				 [{name, ?VARNAME_LOGIN_AUTHUSER},
				  {value, UserVal}
				 ]}
			       ]}
		     ]},
	    {tr, [], [
		      {td, [], "Password"},
		      {td, [], [{input,
				 [{type, "password"},
				  {name, ?VARNAME_LOGIN_AUTHPASSWORD},
				  {value, ""}
				 ]}
			       ]}
		     ]},
	    {tr, [], [
		      {td, [{colspan, 2}],
		       [{input, [{name, "Login"},
				 {type, "submit"}]
			}]}
		     ]}
	   ]}
	 ]}
    ].

%% part of get_logged_in_user/4
%% Returns : {ok, User} | {logged_in, EHTML} | not_logged_in
get_logged_in_user2(Arg, ArgRI, HeadersRI, Node) ->
    Res =
	case get_session(Arg, ArgRI, HeadersRI) of
	    {ok, Session, _SID} when is_record(Session, session) ->
		case is_valid_login_info(Session, Node) of
		    true ->
			{ok, Session#session.user};
		    false ->
			not_logged_in
		end;
	    _ ->
		not_logged_in
	end,

    case Res of
	{ok, _User} ->
	    Res;
	not_logged_in ->
	    Method = case get({yxa_yaws_util, method}) of
			 undefined ->
			     throw({error, "yxa_yaws_util:get_var/2 could not get method"});
			 Method1 when is_atom(Method1) ->
			     Method1
		     end,

	    case Method of
		'POST' ->
		    %% might be login variables in the post
		    case {yxa_yaws_util:get_var(Arg, ?VARNAME_LOGIN_AUTHUSER),
			  yxa_yaws_util:get_var(Arg, ?VARNAME_LOGIN_AUTHPASSWORD)
			 } of
			{{ok, User}, {ok, Password}} when is_list(User), is_list(Password) ->
			    NewSession = #session{user     = User,
						  password = Password
						 },
			    case is_valid_login_info(NewSession, Node) of
				true ->
				    %% store new SID cookie
				    Cookie = yaws_api:new_cookie_session(NewSession),
				    {logged_in, [{redirect, ?STARTPAGE_URL},
						 yaws_api:setcookie(?COOKIENAME_SID, Cookie)
						]};
				false ->
				    not_logged_in
			    end;
			_ ->
			    not_logged_in
		    end;
		_ ->
		    not_logged_in
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (Session, Node) -> true | false
%%
%%            Session = #session{}
%%            Node    = atom() "node to fetch authuser info from"
%%
%% @doc     Check if a valid username and password is given.
%% @end
%%--------------------------------------------------------------------
is_valid_login_info(#session{user = User, password = Password}, Node) ->
    case rpc_call(Node, yxa_virtweb_db, get_authuser, [User]) of
	{ok, #yxa_virtweb_db{password = Password}} ->
	    true;
	{error, Reason} ->
	    Msg = io_lib:format("Failed fetching authuser information : ~p",
				[Reason]),
	    throw({error, lists:flatten(Msg)});
	_ ->
	    false
    end.


%%--------------------------------------------------------------------
%% @spec    (Arg, ArgRI, HeadersRI) -> ok
%%
%%            Arg       = #arg{} "Yaws request data structure"
%%            ArgRI     = [atom()] "record_info(fields, arg)"
%%            HeadersRI = [atom()] "record_info(fields, headers)"
%%
%% @doc     Log out a user (remove session id cookie).
%% @end
%%--------------------------------------------------------------------
logout(Arg, ArgRI, HeadersRI) ->
    {ok, _Session, SID} = get_session(Arg, ArgRI, HeadersRI),
    yaws_api:delete_cookie_session(SID),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Node, M, F, A) ->
%%            Res
%%
%%            Node = atom()
%%            M    = atom() "module name"
%%            F    = atom() "function name"
%%            A    = [atom()] "arguments to function call"
%%
%%            Res = term()
%%
%% @throws  {error, Reason} 
%%
%% @doc     Do an RPC call and throw explanatory error message if it
%%          fails.
%% @end
%%--------------------------------------------------------------------
rpc_call(Node, M, F, A) when is_atom(Node), is_atom(M), is_atom(F), is_list(A) ->
    case rpc:call(Node, M, F, A) of
	{badrpc, E} ->
	    Msg = io_lib:format("Failed communicating with node ~p (function ~p:~p/~p) : ~p",
				[Node, M, F, length(A), E]),
	    throw({error, lists:flatten(Msg)});
	Res ->
	    Res
    end.


%%====================================================================
%% Virtual user data related functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (User, Node) -> [string()]
%%
%%            User  = string()
%%            ModeL = [read] | write | both
%%
%% @doc     Get all domains that a user has access to.
%% @end
%%--------------------------------------------------------------------
get_user_domains(User, Node) when is_list(User), is_atom(Node) ->
    case rpc_call(Node, yxa_virtweb_db, get_authuser, [User]) of
	{ok, E} when is_record(E, yxa_virtweb_db) ->
	    E#yxa_virtweb_db.domains;
	_ ->
	    []
    end.


%% Descrip. : Executed on the Yxa application node (using RPC) when the
%%            Virtuser web-interface wants to write user info, to do it
%%	      atomically inside a single Mnesia transcation.
%% Returns  : ok | Error
do_write_user(User, Password, CPLXML, NewAddress) ->
    WriteFun =
	fun() ->
		case phone:get_user(User) of
		    {atomic, [UT]} when is_tuple(UT) ->
			%% User exists

			%%
			%% Password
			%%
			case is_list(Password) of
			    true ->
				case phone:set_user_password(User, Password) of
				    {atomic, ok} ->
					ok;
				    _E ->
					%% try to avoid exposing password in error message
					mnesia:abort("Failed setting password")
				end;
			    false ->
				ok
			end,
			%%
			%% CPL
			%%
			case is_list(CPLXML) of
			    true ->
				try cpl_db:set_cpl_for_user(User, CPLXML) of
				    {atomic, ok} -> ok
				catch
				    X: Y ->
					Msg = io_lib:format("Failed setting CPL script : ~p ~p~n", [X, Y]),
					mnesia:abort({error, lists:flatten(Msg)})
				end;
			    false ->
				ok
			end,
			%%
			%% Address
			%%
			case is_list(NewAddress) of
			    true ->
				{atomic, OldAddresses} = phone:get_numbers_for_user(User),
				NewAddresses = OldAddresses ++ [NewAddress],
				phone:set_user_numbers(User, NewAddresses);
			    false ->
				ok
			end;
		    {atomic, []} ->
			%% New user
			if
			    is_list(User), is_list(Password) -> ok;
			    true -> mnesia:abort("Need both user and password when creating a new user")
			end,

			%%
			%% User
			%%
			{atomic, ok} = phone:insert_user(User, Password, [], []),

			%%
			%% CPL
			%%
			case is_list(CPLXML) of
			    true ->
				try cpl_db:set_cpl_for_user(User, CPLXML) of
				    {atomic, ok} -> ok
				catch
				    X: Y ->
					Msg = io_lib:format("Failed setting CPL script : ~p ~p~n", [X, Y]),
					mnesia:abort({error, lists:flatten(Msg)})
				end;
			    false ->
				ok
			end,
			%%
			%% Address (mandatory for new users, checked above)
			%%
			{atomic, ok} = phone:set_user_numbers(User, [NewAddress])
		end,
		ok
	end,
    case mnesia:transaction(WriteFun) of
	{atomic, ok} ->
	    ok;
	E ->
	    E
    end.


%%--------------------------------------------------------------------
%% @spec    (Args, AllowedDomains, User, Node) ->
%%            true 
%%
%%            Args           = #arg{} "Yaws request data structure"
%%            AllowedDomains = [string()] "domains that the currently logged in auth user (NOT User) is allowed to manage"
%%            User           = string() "virtual user (a.k.a SIP user) username"
%%            Node           = atom() "Yxa application node"
%%
%%            Reason = string()
%%
%% @throws  {error, Reason} 
%%
%% @doc     Check if User has an address matching any of the Allowed-
%%          Domains.
%% @end
%%--------------------------------------------------------------------
check_is_allowed_write(Args, AllowedDomains, User, Node) when is_list(AllowedDomains), is_list(User), is_atom(Node) ->
    UserExists = yxa_yaws_util:user_exists(User, Node),

    UserAddresses =
	case UserExists of
	    true ->
		yxa_yaws_util:get_user_addresses(User, Node);
	    false ->
		%% for new users, check permission based on the mandatory new address
		case  yxa_yaws_util:get_var(Args, ?VARNAME_EDIT_NEW_ADDRESS) of
		    {ok, NewAddress} when is_list(NewAddress) ->
			[NewAddress];
		    undefined ->
			[]
		end
	end,

    %% If username has an "@" in it, check that the domain thereafter is in AllowedDomains
    case string:chr(User, $@) of
	0 ->
	    ok;
	AtIndex ->
	    UserDomain = string:substr(User, AtIndex + 1),
	    case lists:member(UserDomain, AllowedDomains) of
		true ->
		    ok;
		false ->
		    Msg = io_lib:format("You are not allowed to administer users with that domain (~p) in "
					"their username", [UserDomain]),
		    throw({error, lists:flatten(Msg)})
	    end
    end,

    case check_is_allowed_write2(AllowedDomains, UserAddresses) of
	true ->
	    true;
	false ->
	    case UserExists of
		true ->
		    throw({error, "You are not allowed to edit this user"});
		false ->
		    throw({error, "You must add an address in one of your domains to a new user"})
	    end
    end.

%% Returns: true | false
check_is_allowed_write2(AllowedDomains, [H | T]) when is_list(H) ->
    case sipurl:parse_url_with_default_protocol("sip", H) of
	URL when is_record(URL, sipurl) ->
	    case lists:member(URL#sipurl.host, AllowedDomains) of
		true ->
		    true;
		false ->
		    check_is_allowed_write2(AllowedDomains, T)
	    end;
	_ ->
	    %% ignore bad addresses
	    check_is_allowed_write2(AllowedDomains, T)
    end;
check_is_allowed_write2(_AllowedDomains, []) ->
    false.




%%====================================================================
%% Internal functions
%%====================================================================

%% Descrip.: Yucky function that decodes records based on their record_info(fields, Foo)
%% Returns : {ok, Value} |
%%           throw({error, Reason})
%%           Reason = atom() | string()
get_yaws_arg_element(In, NowAt, [WantElement | ElemT], [ThisRInfo | RInfoT]) ->
    case tuple_to_list(In) of
	[NowAt | Fields] ->
	    case extract_field(WantElement, Fields, ThisRInfo) of
		nomatch ->
		    Msg = io_lib:format("Error decoding Yaws data structure ('~p' not found, in '~p')",
					[WantElement, NowAt]),
		    throw({error, lists:flatten(Msg)});
		This ->
		    case ElemT of
			[] ->
			    %% This is the element that was ultimately requested
			    {ok, This};
			_ ->
			    %% Recurse down another level
			    get_yaws_arg_element(This, WantElement, ElemT, RInfoT)
		    end
	    end;
	[Other | _Fields] ->
	    Msg = io_lib:format("Error decoding Yaws data structure ('~p' does not match expected value '~p')",
				[Other, NowAt]),
	    throw({error, lists:flatten(Msg)});
	_ ->
	    throw({error, "Error decoding Yaws data structure"})
    end.

%% part of get_yaws_arg_element/4
extract_field(Field, [Value | _], [Field | _]) ->
    Value;
extract_field(Field, [_Value | FieldT], [_Other | RInfoT]) ->
    extract_field(Field, FieldT, RInfoT);
extract_field(_Field, [], []) ->
    nomatch.



%% Returns : {ok, Session, SID} | none
get_session(Arg, ArgRI, HeadersRI) ->
    try get_yaws_arg_element(Arg, arg, [headers, cookie], [ArgRI, HeadersRI]) of
	{ok, Cookies} ->
	    case yaws_api:find_cookie_val(?COOKIENAME_SID, Cookies) of
		SID when is_list(SID), SID /= [] ->
		    case yaws_api:cookieval_to_opaque(SID) of
			{ok, Session} when is_record(Session, session) ->
			    {ok, Session, SID};
			{error, {has_session, Session}} when is_record(Session, session) ->
			    {ok, Session, SID};
			_ ->
			    none
		    end;
		[] ->
		    none
	    end
    catch
	throw:
	  {error, _Reason} ->
	    none
    end.

