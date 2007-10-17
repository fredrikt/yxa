%%%-------------------------------------------------------------------
%%% File    : sipuserdb_test.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      A sipuserdb backend to be used in unit tests. Reads all
%%%           userdata from a per-process ets table.
%%%
%%% @since    28 Nov 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sipuserdb_test).
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
-export([test/0
	]).

%%--------------------------------------------------------------------
%% Extra exports - module specific unit test utility functions
%%--------------------------------------------------------------------
-export([init/1,
	 stop/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipuserdb_file.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SIPUSERDB_TEST_SOURCE, sipuserdb_test_etsref).

%%====================================================================
%% External functions specific for this module
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Data) ->
%%            ok | {error, Reason}
%%
%%            Data = [#user{} | #address{}]
%%
%%            Reason = term()
%%
%% @doc     Initialize a per-process test userdb. Accepts Data in the
%%          same format as sipuserdb_file.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Data) when is_list(Data) ->
    case sipuserdb_file_backend:parse_db([Data]) of
	{ok, Users, Addresses} ->
	    Ets = ets:new(?MODULE, [protected, set]),
	    true = ets:insert(Ets, {users, Users}),
	    true = ets:insert(Ets, {addresses, Addresses}),
	    put(?SIPUSERDB_TEST_SOURCE, Ets),
	    ok;
	E ->
	    E
    end.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Stop using a per-process test userdb.
%% @end
%%--------------------------------------------------------------------
stop() ->
    case erase(?SIPUSERDB_TEST_SOURCE) of
	undefined ->
	    ok;
	EtsRef ->
	    ets:delete(EtsRef),
	    ok
    end.


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
    [].

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
	    logger:log(debug, "userdb-test: No user with address ~p", [Address]),
	    nomatch;
	[User] ->
	    %% There exists exactly one user with this name
	    User;
	Users when is_list(Users) ->
	    logger:log(debug, "userdb-test: More than one user with address ~p (~p)", [Address, Users]),
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
	    logger:log(debug, "userdb-test: No such user ~p", [Username]),
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
    logger:log(debug, "userdb-test: Looking for users matching address(es) ~p derived from URL ~p",
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
	    logger:log(debug, "userdb-test: No such user ~p when fetching password", [Username]),
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
	    logger:log(debug, "userdb-test: No such user ~p when fetching password", [Username]),
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
	    logger:log(debug, "userdb-test: No such user ~p when fetching telephone number", [Username]),
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
		    logger:log(debug, "userdb-test: User ~p has no address that looks like a phone number (addresses : ~p)",
			       [Username, collect_addresses(A)]),
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
	    logger:log(debug, "userdb-test: No forwards found for user ~p", [Username]),
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
    case sipurl:url_is_equal(URL, H#address.url) of
	true ->
	    get_usernames_for_url(URL, T, [H#address.user | Res]);
	false ->
	    %% URL does NOT match this address record, try next (T)
	    get_usernames_for_url(URL, T, Res)
    end.


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
    case get(?SIPUSERDB_TEST_SOURCE) of
	undefined ->
	    erlang:throw({error, "sipuserdb_test used without initialization"});
	Ets ->
	    [{users, Users}] = ets:lookup(Ets, users),
	    Users
    end.

fetch_addresses() ->
    case get(?SIPUSERDB_TEST_SOURCE) of
	undefined ->
	    erlang:throw({error, "sipuserdb_test used without initialization"});
	Ets ->
	    [{addresses, Addresses}] = ets:lookup(Ets, addresses),
	    Addresses
    end.


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
    %% yxa_init()
    %%--------------------------------------------------------------------
    [] = yxa_init(),


    %% init(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "init/1 - 1"),
    ExtraCfg = [],
    ok = yxa_test_config:init(ExtraCfg),

    UserDb =
	[
	 {user, [
		 {name, "ft.sip1"},
		 {password, "secret"},
		 {classes, [internal,national,mobile]},
		 {addresses, ["sip:all@example.org", "sip:2345@example.org"]}
		]},
	 {user, [
		 {name, "foo@example.org"},
		 {password, "secret2"},
		 {classes, [internal]},
		 {addresses, ["sip:info@example.org", "sip:all@example.org"]}
		]},
	 {user, [
		 {name, "notmuch"},
		 {password, "secret3"}
		]},
	 {user, [
		 {name, "e164-user"},
		 {addresses, ["sip:+1234@example.org"]}
		]},

	 {address, [
		    {user, "ft.sip1"},
		    {address, "sip:onemore@example.org"}
		   ]}
	],

    ok = init(UserDb),

    autotest:mark(?LINE, "init/1 - 2"),
    %% test with bad input
    {error, _Reason_Init2} = init([foo]),

    autotest:mark(?LINE, "init/1 - 3"),
    %% test with bad input
    {error, _Reason_Init3} = init([{user, foo, []}]),


    %% get_user_with_address(Address)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_user_with_address/1 - 1"),
    "foo@example.org" = get_user_with_address("sip:info@example.org"),

    autotest:mark(?LINE, "get_user_with_address/1 - 2"),
    %% test with more than one matching user
    error = get_user_with_address("sip:all@example.org"),

    autotest:mark(?LINE, "get_user_with_address/1 - 2"),
    nomatch = get_user_with_address("sip:NOMATCH@example.org"),


    %% get_users_for_address_of_record(Address)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_users_for_address_of_record/1 - 1"),
    ["ft.sip1"] = get_users_for_address_of_record("sip:2345@example.org"),

    autotest:mark(?LINE, "get_users_for_address_of_record/1 - 2"),
    ["foo@example.org", "ft.sip1"] = get_users_for_address_of_record("sip:all@example.org"),

    autotest:mark(?LINE, "get_users_for_address_of_record/1 - 3"),
    [] = get_users_for_address_of_record("sip:NONE@example.org"),


    %% get_users_for_addresses_of_record(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_users_for_addresses_of_record/1 - 1"),
    ["ft.sip1"] = get_users_for_addresses_of_record(["sip:2345@example.org", "sip:NOMATCH@example.org"]),

    autotest:mark(?LINE, "get_users_for_addresses_of_record/1 - 2"),
    ["foo@example.org", "ft.sip1"] =
	get_users_for_addresses_of_record(["sip:info@example.org", "sip:onemore@example.org"]),


    %% get_addresses_for_users(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_addresses_for_users/1 - 1"),
    ["sip:2345@example.org",
     "sip:all@example.org",
     "sip:onemore@example.org"] = get_addresses_for_users(["ft.sip1"]),

    autotest:mark(?LINE, "get_addresses_for_users/1 - 2"),
    [] = get_addresses_for_users(["nouser", "barX"]),

    autotest:mark(?LINE, "get_addresses_for_users/1 - 3"),
    ["sip:2345@example.org",
     "sip:all@example.org",
     "sip:info@example.org",
     "sip:onemore@example.org"] = get_addresses_for_users(["ft.sip1", "foo@example.org"]),


    %% get_addresses_for_user(Username)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_addresses_for_user/1 - 1"),
    ["sip:2345@example.org",
     "sip:all@example.org",
     "sip:onemore@example.org"] = get_addresses_for_user("ft.sip1"),


    %% get_users_for_url(URL)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_users_for_url/1 - 1"),
    [] = get_users_for_url(sipurl:parse("sip:NO")),

    autotest:mark(?LINE, "get_users_for_url/1 - 2"),
    ["foo@example.org"] = get_users_for_url(sipurl:parse("sip:info@example.org")),


    %% get_password_for_user(Username)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_password_for_user/1 - 1"),
    "secret" = get_password_for_user("ft.sip1"),

    autotest:mark(?LINE, "get_password_for_user/1 - 2"),
    nomatch = get_password_for_user("NO"),


    %% get_classes_for_user(Username)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_classes_for_user/1 - 1"),
    [internal, national, mobile] = get_classes_for_user("ft.sip1"),

    autotest:mark(?LINE, "get_classes_for_user/1 - 2"),
    nomatch = get_classes_for_user("NO"),

    autotest:mark(?LINE, "get_classes_for_user/1 - 3"),
    [] = get_classes_for_user("notmuch"),


    %% get_telephonenumber_for_user(Username)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_telephonenumber_for_user/1 - 1"),
    "2345" = get_telephonenumber_for_user("ft.sip1"),

    autotest:mark(?LINE, "get_telephonenumber_for_user/1 - 2"),
    nomatch = get_telephonenumber_for_user("NO"),

    autotest:mark(?LINE, "get_telephonenumber_for_user/1 - 3"),
    nomatch = get_telephonenumber_for_user("notmuch"),

    autotest:mark(?LINE, "get_telephonenumber_for_user/1 - 4"),
    "+1234" = get_telephonenumber_for_user("e164-user"),


    %% get_forwards_for_users(In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_forwards_for_users/1 - 1"),
    %% forwards not implemented in sipuserdb_file_backend yet
    [] = get_forwards_for_users(["ft.sip1"]),

    autotest:mark(?LINE, "get_forwards_for_users/1 - 2"),
    [] = get_forwards_for_users(["NO"]),


    %% get_forward_for_user(Username)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_forward_for_users/1 - 1"),
    %% forwards not implemented in sipuserdb_file_backend yet
    nomatch = get_forward_for_user("ft.sip1"),

    autotest:mark(?LINE, "get_forward_for_user/1 - 2"),
    nomatch = get_forward_for_user("NO"),


    %% stop()
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "stop/1 - 1"),
    ok = stop(),

    autotest:mark(?LINE, "stop/1 - 2"),
    ok = stop(),

    ok.
