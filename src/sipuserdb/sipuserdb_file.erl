%%%-------------------------------------------------------------------
%%% File    : sipuserdb_file.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      A persistent sipuserdb module that reads it's user
%%%           database from a file using file:consult().
%%%
%%% @since    11 Aug 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%
%%% Notes   : If there is unparsable data in the userdb when we start,
%%%           we crash rather ungracefully. Start with empty userdb
%%%           or crash? If the latter then at least crash with style.
%%%           We don't validate data (like addresses) when we load it
%%%           - maybe we should.
%%%-------------------------------------------------------------------
-module(sipuserdb_file).
%%-compile(export_all).

-behaviour(sipuserdb).

%%--------------------------------------------------------------------
%% External exports - sipuserdb callbacks
%%--------------------------------------------------------------------
-export([yxa_init/0,
	 get_user_with_address/1,
	 get_users_for_address_of_record/1,
	 get_users_for_addresses_of_record/1,
	 get_users_for_url/1,
	 get_addresses_for_user/1,
	 get_addresses_for_users/1,
	 get_password_for_user/1,
	 get_classes_for_user/1,
	 get_telephonenumber_for_user/1,
	 get_forwards_for_users/1,
	 get_forward_for_user/1
	]).

%%--------------------------------------------------------------------
%% Extra exports - module specific manual-use functions
%%--------------------------------------------------------------------
-export([reload_userdb/0,
	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipuserdb_file.hrl").

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () ->
%%            Spec |
%%            []
%%
%%            Spec = term() "OTP supervisor child specification"
%%
%% @doc     Perform any necessary startup initialization and return an
%%          OTP supervisor child spec if we want to add to
%%          sipserver_sup's list. If this sipuserdb_module needs to
%%          be persistent, it should be a gen_server and init should
%%          just return a spec so that the gen_server is started by
%%          the supervisor.
%% @private
%% @end
%%--------------------------------------------------------------------
yxa_init() ->
    [{sipuserdb_file_backend, {sipuserdb_file_backend, start_link, []},
      permanent, 2000, worker, [sipuserdb_file_backend]}
    ].

%%--------------------------------------------------------------------
%% @spec    (Address) ->
%%            Username |
%%            nomatch  |
%%            error
%%
%%            Address = string() "an address in string format."
%%
%%            Username = string()
%%
%% @doc     Looks up exactly one user with an Address. Used for
%%          example in REGISTER. If there are multiple users with
%%          this address in our database, this function returns
%%          'error'.
%% @end
%%--------------------------------------------------------------------
get_user_with_address(Address) ->
    case get_users_using_address(Address) of
	[] ->
	    logger:log(debug, "userdb-file: No user with address ~p", [Address]),
	    nomatch;
	[User] ->
	    %% There exists exactly one user with this name
	    User;
	Users when is_list(Users) ->
	    logger:log(debug, "userdb-file: More than one user with address ~p (~p)", [Address, Users]),
	    error
    end.


%%--------------------------------------------------------------------
%% @spec    (Address) ->
%%            Users |
%%            error
%%
%%            Address = string() "an address in string format."
%%
%%            Users = [string()]
%%
%% @doc     Get all usernames of users matching an address. Used to
%%          find out to which users we should send a request.
%% @end
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    get_users_using_address(Address).


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Users
%%
%%            In = [string()] "addresses in string format."
%%
%%            Users = [string()]
%%
%% @doc     Iterate over a list of addresses of record, return all
%%          users matching one or more of the addresses, without
%%          duplicates.
%% @end
%%--------------------------------------------------------------------
get_users_for_addresses_of_record(In) ->
    get_users_for_addresses_of_record2(In, []).

get_users_for_addresses_of_record2([], Res) ->
    lists:usort(Res);
get_users_for_addresses_of_record2([H | T], Res) ->
    case get_users_for_address_of_record(H) of
	Users when is_list(Users) ->
	    get_users_for_addresses_of_record2(T, lists:append(Res, Users));
	_ ->
	    get_users_for_addresses_of_record2(T, Res)
    end.


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Addresses
%%
%%            In = [string()] "usernames"
%%
%%            Addresses = [string()]
%%
%% @doc     Iterate over a list of users, return all their addresses
%%          without duplicates by using the next function,
%%          get_addresses_for_user/1.
%% @end
%%--------------------------------------------------------------------
get_addresses_for_users(In) ->
    get_addresses_for_users2(In, []).

get_addresses_for_users2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_addresses_for_users2([H | T], Res) ->
    case get_addresses_for_user(H) of
	A when is_list(A) ->
	    get_addresses_for_users2(T, lists:append(Res, A));
	_ ->
	    get_addresses_for_users2(T, Res)
    end.


%%--------------------------------------------------------------------
%% @spec    (Username) ->
%%            Addresses |
%%            error
%%
%%            Username = string()
%%
%%            Addresses = [string()]
%%
%% @doc     Get all possible addresses of a user. Both configured
%%          ones, and implicit ones. Used for example to check if a
%%          request from a user has an acceptable From: header.
%% @end
%%--------------------------------------------------------------------
get_addresses_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p", [Username]),
	    [];
	User when is_record(User, user) ->
	    collect_addresses(get_addresses_using_user(User))
    end.


%%--------------------------------------------------------------------
%% @spec    (URL) ->
%%            Usernames
%%
%%            URL = #sipurl{}
%%
%%            Usernames = [string()]
%%
%% @doc     Given an URL that is typically the Request-URI of an
%%          incoming request, make a list of implicit user addresses
%%          and return a list of all users matching any of these
%%          addresses. This is located in here since user database
%%          backends can have their own way of deriving addresses
%%          from a Request-URI.
%% @end
%%--------------------------------------------------------------------
get_users_for_url(URL) when is_record(URL, sipurl) ->
    Addresses = local:lookup_url_to_addresses(sipuserdb_file, URL),
    logger:log(debug, "userdb-file: Looking for users matching address(es) ~p derived from URL ~p",
	       [Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%--------------------------------------------------------------------
%% @spec    (Username) ->
%%            Password |
%%            nomatch  |
%%            error
%%
%%            Username = string()
%%
%%            Password = string()
%%
%% @doc     Returns the password for a user.
%% @end
%%--------------------------------------------------------------------
get_password_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p when fetching password", [Username]),
	    nomatch;
	User when is_record(User, user) ->
	    User#user.password
    end.


%%--------------------------------------------------------------------
%% @spec    (Username) ->
%%            Classes |
%%            nomatch |
%%            error
%%
%%            Username = string()
%%
%%            Classes = [atom()]
%%
%% @doc     Returns a list of classes allowed for a user. Classes are
%%          used by pstnproxy to determine if it should allow a call
%%          to a PSTN number (of a certain class) from a user or not.
%% @end
%%--------------------------------------------------------------------
get_classes_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p when fetching password", [Username]),
	    nomatch;
	User when is_record(User, user) ->
	    case User#user.classes of
		_ when User#user.classes == none ; User#user.classes == undefined ->
		    [];
		_ ->
		    User#user.classes
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (Username) ->
%%            Number  |
%%            nomatch |
%%            error
%%
%%            Username = string()
%%
%%            Number = string()
%%
%% @doc     Return the telephone number for a user. We do this by
%%          fetching all addresses for the user and then examining
%%          them to see if any of them is a tel: URL, or has a user
%%          part which is all numeric or is an E.164 number. The
%%          numbering plan in the number return is not specified.
%% @end
%%--------------------------------------------------------------------
get_telephonenumber_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p when fetching telephone number", [Username]),
	    nomatch;
	User when is_record(User, user) ->
	    %% use get_addresses_using_user2/3 to get addresses with order preserved
	    A = get_addresses_using_user2(Username, fetch_addresses(), []),
	    %% Look through the addresses returned and return the first one
	    %% which has an all numeric user-part.
	    case find_first_telephonenumber(A) of
		nomatch ->
		    %% XXX Should we perhaps turn that into 'error' since we
		    %% apparenlty had a user with this name in our database but
		    %% that user did not have an address that looks like a phone
		    %% number?
		    logger:log(debug, "userdb-file: User ~p has no address that looks like a phone number "
			       "(addresses : ~p)", [Username, collect_addresses(A)]),
		    nomatch;
		Address when is_record(Address, address) ->
		    (Address#address.url)#sipurl.user
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            ForwardList
%%
%%            In = [string()] "list of usernames"
%%
%%            ForwardList = [#sipproxy_forward{}]
%%
%% @doc     Return a list of forward addresses for a list of users.
%%          Uses the next function, get_forward_for_user/1.
%% @end
%%--------------------------------------------------------------------
get_forwards_for_users(In) ->
    get_forwards_for_users2(In, []).

get_forwards_for_users2([], Res) ->
    Res;
get_forwards_for_users2([H | T], Res) ->
    case get_forward_for_user(H) of
	Fwd when is_list(Fwd) ->
	    get_forwards_for_users2(T, lists:append(Res, Fwd));
	nomatch ->
	    get_forwards_for_users2(T, Res)
    end.


%%--------------------------------------------------------------------
%% @spec    (Username) ->
%%            ForwardList |
%%            nomatch     |
%%            error
%%
%%            Username = string()
%%
%%            ForwardList = [#sipproxy_forward{}]
%%
%% @doc     Return the forward address(es) for a user.
%% @end
%%--------------------------------------------------------------------
get_forward_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No forwards found for user ~p", [Username]),
	    nomatch;
	User when is_record(User, user) ->
	    case User#user.forward of
		Foo when Foo == none; Foo == []; Foo == undefined ->
		    nomatch;
		L when is_list(L) ->
		    L
	    end
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @spec    (User) ->
%%            User    |
%%            nomatch
%%
%%            User = string()
%%
%% @doc     Fetch a user record from the user database, given it's
%%          username.
%% @end
%%--------------------------------------------------------------------
get_user(User) when is_list(User) ->
    get_user2(User, fetch_users()).

get_user2(_User, []) ->
    nomatch;
get_user2(User, [H | _T]) when is_record(H, user), H#user.name == User ->
    H;
get_user2(User, [H | T]) when is_record(H, user) ->
    get_user2(User, T).


%%--------------------------------------------------------------------
%% @spec    (Username) ->
%%            Addresses
%%
%%            User = string() | #user{}
%%
%%            Addresses = [#address{}]
%%
%% @doc     Return all addresses for a username or a user record,
%%          sorted alphabetically.
%% @end
%%--------------------------------------------------------------------
get_addresses_using_user(Username) when is_list(Username) ->
    Addresses = get_addresses_using_user2(Username, fetch_addresses(), []),
    %% Make list sorted and remove duplicates
    lists:usort(Addresses);

get_addresses_using_user(User) when is_record(User, user) ->
    get_addresses_using_user(User#user.name).

%% get_addresses_using_user2/3, part of get_addresses_using_user/1
get_addresses_using_user2(_Username, [], Res) ->
    %% it is important that we preserve order here, for get_telephonenumber_for_user/1
    lists:reverse(Res);
get_addresses_using_user2(Username, [H | T], Res) when is_record(H, address), H#address.user == Username ->
    %% Username matches the username for the address record (H)
    get_addresses_using_user2(Username, T, [H | Res]);
get_addresses_using_user2(Username, [H | T], Res) when is_record(H, address) ->
    get_addresses_using_user2(Username, T, Res).


%%--------------------------------------------------------------------
%% @spec    (Address) ->
%%            Usernames
%%
%%            Address = string() | #sipurl{}
%%
%%            Usernames = [string()]
%%
%% @doc     Given an address (list) or URL (sipurl record), locate and
%%          return all address records in the userdb, fetched from
%%          the persistent sipuserdb_file process, that matches using
%%          URI address matching rules.
%% @end
%%--------------------------------------------------------------------
get_users_using_address("tel:+" ++ Num) when is_list(Num) ->
    case util:isnumeric(Num) of
	true ->
	    get_usernames_for_exact_address("tel:+" ++ Num, fetch_addresses());
	false ->
	    %% Invalid TEL URL
	    []
    end;
get_users_using_address(Address) when is_list(Address) ->
    %% Check that Address is a parseable URL. It really does not have to
    %% be - it is often the result of canonization of a Request-URI, URL
    %% or address of some form.
    case sipurl:parse(Address) of
	URL when is_record(URL, sipurl) ->
	    get_usernames_for_url(URL, fetch_addresses(), []);
	_ ->
	    %% unparseable URL
	    []
    end;
get_users_using_address(URL) when is_record(URL, sipurl) ->
    get_usernames_for_url(URL, fetch_addresses(), []).


%%--------------------------------------------------------------------
%% @spec    (URL, Addresses, []) ->
%%            Users
%%
%%            URL       = #sipurl{}
%%            Addresses = [#address{}]
%%
%%            Users = [string()]
%%
%% @doc     Given an URL (sipurl record), locate and return all
%%          usernames for the matching address records, fetched from
%%          the persistent sipuserdb_file process, that matches using
%%          URI address matching rules.
%% @end
%%--------------------------------------------------------------------
get_usernames_for_url(_URL, [], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_usernames_for_url(URL, [H | T], Res) when is_record(URL, sipurl), is_record(H, address) ->
    case H#address.url of
	AddrURL when is_record(AddrURL, sipurl) ->
	    case sipurl:url_is_equal(URL, H#address.url) of
		true ->
		    get_usernames_for_url(URL, T, [H#address.user | Res]);
		false ->
		    %% URL does NOT match this address record, try next (T)
		    get_usernames_for_url(URL, T, Res)
	    end;
	undefined ->
	    get_usernames_for_url(URL, T, Res)
    end.

%%--------------------------------------------------------------------
%% @spec    (Address, Addresses) ->
%%            Users
%%
%%            Address   = string()
%%            Addresses = [#address{}]
%%
%%            Users = [string()]
%%
%% @doc     Get all address records having Addr#address.address
%%          exactly matching Address. Used for example when the input
%%          is a TEL URL.
%% @end
%%--------------------------------------------------------------------
get_usernames_for_exact_address(Address, Addresses) ->
    Match = [H#address.user || H <- Addresses, H#address.address == Address],
    %% Make list sorted and remove duplicates
    lists:usort(Match).

%%--------------------------------------------------------------------
%% @spec    (In) -> [string()] "list of addresses in string format"
%%
%%            In = [#address{}]
%%
%% @doc     Collect and return a list of all addresses from a set of
%%          address records.
%% @end
%%--------------------------------------------------------------------
collect_addresses(In) ->
    collect_addresses2(In, []).

collect_addresses2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
collect_addresses2([H | T], Res) when is_record(H, address) ->
    collect_addresses2(T, [H#address.address | Res]).


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Address |
%%            nomatch
%%
%%            In = [#address{}]
%%
%%            Address = #address{}
%%
%% @doc     Look through a list of address records and return the
%%          first one that has a URL userpart that is all numeric.
%%          Called from get_telephonenumber_for_user.
%% @end
%%--------------------------------------------------------------------
find_first_telephonenumber([]) ->
    nomatch;
find_first_telephonenumber([H | T]) when is_record(H, address), H#address.url == undefined ->
    case H#address.address of
	"tel:+" ++ Num ->
	    "+" ++ Num;
	_ ->
	    %% Not a tel: URL like we would have expected when H#address.url is undefined
	    find_first_telephonenumber(T)
    end;
find_first_telephonenumber([H | T]) when is_record(H, address) ->
    URL = H#address.url,
    IsNumericUser = util:isnumeric(URL#sipurl.user),
    IsTelURL = (URL#sipurl.proto == "tel"),
    IsE164User = case URL#sipurl.user of
		     "+" ++ Rest ->
			 util:isnumeric(Rest);
		     _ ->
			 false
		 end,
    if
	IsNumericUser == true -> H;
	IsTelURL == true -> H;
	IsE164User == true -> H;
	true ->
	    %% The address records (H) address does not appear to
	    %% be a phone number. Check next element (T).
	    find_first_telephonenumber(T)
    end.

fetch_users() ->
    {ok, Res} = gen_server:call(sipuserdb_file_backend, {fetch_users}),
    Res.

fetch_addresses() ->
    {ok, Res} = gen_server:call(sipuserdb_file_backend, {fetch_addresses}),
    Res.

reload_userdb() ->
    gen_server:cast(sipuserdb_file_backend, {reload_userdb}).


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
    autotest:mark(?LINE, "init variables"),

    Test_User1_Addresses = [test_make_address("test", "sip:a@example.org"),
			    test_make_address("test", "sip:b@example.org"),
			    test_make_address("test", "sip:1@example.org"),
			    test_make_address("test", "sip:11@example.org"),
			    test_make_address("test", "sip:both@example.org")
			   ],

    Test_User2_Addresses = [test_make_address("test2", "sip:a2@example.org"),
			    test_make_address("test2", "sip:b2@example.org"),
			    test_make_address("test2", "sip:2@example.org"),
			    test_make_address("test2", "sip:22@example.org"),
			    test_make_address("test2", "sip:both@example.org")
			   ],

    %% mix the two users addresses into a single list
    Test_Addresses_Zipped = lists:zip(Test_User1_Addresses, Test_User2_Addresses),
    Test_Addresses = lists:foldl(fun(T, Acc) when is_tuple(T) ->
					 L = tuple_to_list(T),
					 Acc ++ L
				 end, [], Test_Addresses_Zipped),


    %% get_addresses_using_user2(Username, Addresses, Res)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_addresses_using_user2/3 - 1"),
    %% test normal case, make sure order is preserved
    Test_User1_Addresses = get_addresses_using_user2("test", Test_Addresses, []),


    %% find_first_telephonenumber(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "find_first_telephonenumber/1 - 1"),
    %% test normal case
    #address{user = "test2", address = "sip:2@example.org"} =
	find_first_telephonenumber(Test_User2_Addresses),

    autotest:mark(?LINE, "find_first_telephonenumber/1 - 2"),
    nomatch = find_first_telephonenumber([]),

    autotest:mark(?LINE, "find_first_telephonenumber/1 - 3"),
    FFT_Addressses3 = [test_make_address("foo", "sip:alpha@example.org")],
    nomatch = find_first_telephonenumber(FFT_Addressses3),


    %% get_usernames_for_url(URL, Addresses, [])
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_usernames_for_url/3 - 1"),
    ["test"] = get_usernames_for_url(sipurl:parse("sip:a@example.org"), Test_Addresses, []),

    autotest:mark(?LINE, "get_usernames_for_url/3 - 2"),
    ["test", "test2"] = get_usernames_for_url(sipurl:parse("sip:both@example.org"), Test_Addresses, []),


    %% get_usernames_for_exact_address(Address, Addresses)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_usernames_for_exact_address/2 - 1"),
    %% test simple case
    Test_Addresses2 = Test_Addresses ++ [test_make_address("numberholder", "tel:+46123456789")],
    ["numberholder"] = get_usernames_for_exact_address("tel:+46123456789", Test_Addresses2),

    autotest:mark(?LINE, "get_usernames_for_exact_address/2 - 2"),
    %% test wrong number
    [] = get_usernames_for_exact_address("tel:+46123", Test_Addresses2),


    %% collect_addresses(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "collect_addresses/1 - 1"),
    CollectAddresses_L1 = [test_make_address("foo", "sip:first@example.org"),
			   test_make_address("foo", "sip:second@example.org")],
    ["sip:first@example.org", "sip:second@example.org"] = collect_addresses(CollectAddresses_L1),



    ok.

test_make_address(U, A) ->
    #address{user    = U,
	     address = A,
	     url     = case sipurl:parse(A) of
			   URL when is_record(URL, sipurl) -> URL;
			   {unparseable, _} -> undefined
		       end
	    }.

