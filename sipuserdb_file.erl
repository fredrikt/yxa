%%%-------------------------------------------------------------------
%%% File    : sipuserdb_file.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: A persistent sipuserdb module that reads it's user
%%%           database from a file using file:consult().
%%%
%%% Created : 11 Aug 2004 by Fredrik Thulin <ft@it.su.se>
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
-export([reload_userdb/0
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
%% Function: yxa_init()
%% Descrip.: Perform any necessary startup initialization and
%%           return an OTP supervisor child spec if we want to add
%%           to sipserver_sup's list. If this sipuserdb_module
%%           needs to be persistent, it should be a gen_server and
%%           init should just return a spec so that the gen_server
%%           is started by the supervisor.
%% Returns : Spec |
%%           []
%%           Spec = OTP supervisor child specification
%%--------------------------------------------------------------------
yxa_init() ->
    [{sipuserdb_file_backend, {sipuserdb_file_backend, start_link, []},
      permanent, 2000, worker, [sipuserdb_file_backend]}
    ].

%%--------------------------------------------------------------------
%% Function: get_user_with_address(Address)
%%           Address = string(), an address in string format.
%% Descrip.: Looks up exactly one user with an Address. Used for
%%           example in REGISTER. If there are multiple users with
%%           this address in our database, this function returns
%%           'error'.
%% Returns:  Username |
%%           nomatch  |
%%           error
%%           Username = string()
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
	    error;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_users_using_address(), address ~p result : ~p",
		       [Address, Unknown]),
	    error
    end.


%%--------------------------------------------------------------------
%% Function: get_users_for_address_of_record(Address)
%%           Address = string(), an address in string format.
%% Descrip.: Get all usernames of users matching an address. Used to
%%           find out to which users we should send a request.
%% Returns : Users |
%%           error
%%           Users = list() of string() 
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    case get_users_using_address(Address) of
	L when is_list(L) ->
	    L;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_users_using_address(), address ~p result : ~p",
		       [Address, Unknown]),
	    error
    end.


%%--------------------------------------------------------------------
%% Function: get_users_for_addresses_of_record(In)
%%           In = list() of string(), addresses in string format.
%% Descrip.: Iterate over a list of addresses of record, return
%%           all users matching one or more of the addresses,
%%           without duplicates.
%% Returns : Users, list() of string()
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
%% Function: get_addresses_for_users(In)
%%           In = list() of string(), usernames
%% Descrip.: Iterate over a list of users, return all their
%%           addresses without duplicates by using the next
%%           function, get_addresses_for_user/1.
%% Returns : Addresses, list() of string()
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
%% Function: get_addresses_for_user(Username)
%%           Username = string()
%% Descrip.: Get all possible addresses of a user. Both configured
%%           ones, and implicit ones. Used for example to check if a
%%           request from a user has an acceptable From: header.
%% Returns : Addresses |
%%           error
%%           Addresses = list() of string()
%%--------------------------------------------------------------------
get_addresses_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p", [Username]),
	    [];
	User when is_record(User, user) ->
	    collect_addresses(get_addresses_using_user(User));
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%%--------------------------------------------------------------------
%% Function: get_users_for_url(URL)
%%           URL = sipurl record()
%% Descrip.: Given an URL that is typically the Request-URI of an
%%           incoming request, make a list of implicit user
%%           addresses and return a list of all users matching any
%%           of these addresses. This is located in here since
%%           user database backends can have their own way of
%%           deriving addresses from a Request-URI.
%% Returns : Usernames, list() of string()
%%--------------------------------------------------------------------
get_users_for_url(URL) when is_record(URL, sipurl) ->
    Addresses = local:lookup_url_to_addresses(sipuserdb_file, URL),
    logger:log(debug, "userdb-file: Looking for users matching address(es) ~p derived from URL ~p",
	       [Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%--------------------------------------------------------------------
%% Function: get_password_for_user(Username)
%%           Username = string()
%% Descrip.: Returns the password for a user.
%% Returns : Password |
%%           nomatch  |
%%           error
%%           Password = string()
%%--------------------------------------------------------------------
get_password_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p when fetching password", [Username]),
	    nomatch;
	User when is_record(User, user) ->
	    User#user.password;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%%--------------------------------------------------------------------
%% Function: get_classes_for_user(Username)
%%           Username = string()
%% Descrip.: Returns a list of classes allowed for a user. Classes
%%           are used by pstnproxy to determine if it should allow
%%           a call to a PSTN number (of a certain class) from a
%%           user or not.
%% Returns : Classes |
%%           nomatch |
%%           error
%%           Classes = list() of atom()
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
	    end;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%%--------------------------------------------------------------------
%% Function: get_telephonenumber_for_user(Username)
%%           Username = string()
%% Descrip.: Return the telephone number for a user. We do this by
%%           fetching all addresses for the user and then examining
%%           them to see if any of them is a tel: URL, or has a
%%           user part which is all numeric or is an E.164 number.
%%           The numbering plan in the number return is not specified.
%% Returns : Number  |
%%           nomatch |
%%           error
%%           Number = string()
%%--------------------------------------------------------------------
get_telephonenumber_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p when fetching telephone number", [Username]),
	    nomatch;
	User when is_record(User, user) ->
	    case get_addresses_using_user(User) of
		nomatch ->
		    nomatch;
		error ->
		    error;
		A when is_list(A) ->
		    %% Look through the addresses returned and return the first one
		    %% which has an all numeric user-part.
		    case find_first_telephonenumber(A) of
			nomatch ->
			    %% XXX Should we perhaps turn that into 'error' since we
			    %% apparenlty had a user with this name in our database but
			    %% that user did not have an address that looks like a phone
			    %% number?
			    logger:log(debug, "userdb-file: User ~p has no address that looks like a phone number (addresses : ~p)",
				       [Username, collect_addresses(A)]),
			    nomatch;
			Address when is_record(Address, address) ->
			    URL = sipurl:parse(Address#address.address),
			    URL#sipurl.user
		    end;
		Unknown ->
		    logger:log(error, "userdb-file: Unexpected result from get_addresses_for_user(), user ~p result : ~p",
			       [Username, Unknown]),
		    error
	    end;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%%--------------------------------------------------------------------
%% Function: get_forwards_for_users(In)
%%           In = list() of string(), list of usernames
%% Descrip.: Return a list of forward addresses for a list of users.
%%           Uses the next function, get_forward_for_user/1.
%% Returns : Forwards, list() of string()
%%--------------------------------------------------------------------
get_forwards_for_users(In) ->
    get_forwards_for_users2(In, []).

get_forwards_for_users2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_forwards_for_users2([H | T], Res) ->
    case get_forward_for_user(H) of
	Fwd when is_list(Fwd) ->
	    get_forwards_for_users2(T, lists:append(Res, [Fwd]));
	nomatch ->
	    get_forwards_for_users2(T, Res);
	error ->
	    get_forwards_for_users2(T, Res)
    end.


%%--------------------------------------------------------------------
%% Function: get_forward_for_user(Username)
%%           Username = string()
%% Descrip.: Return the forward address for a user.
%% Returns : Forward |
%%           nomatch |
%%           error
%%           Forward = string()
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
		    L;
		_ ->
		    logger:log(error, "userdb-file: User ~p has invalid forward in database : ~p",
			       [Username, User#user.forward]),
		    error
	    end
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: get_user(User)
%%           User = string()
%% Descrip.: Fetch a user record from the user database, given it's
%%           username.
%% Returns : User    |
%%           nomatch
%%--------------------------------------------------------------------
get_user(User) when is_list(User) ->
    get_user2(User, fetch_users()).

get_user2(_User, []) ->
    nomatch;
get_user2(User, [H | _T]) when is_record(H, user), H#user.name == User ->
    H;
get_user2(User, [H | T]) when is_record(H, user) ->
    get_user2(User, T);
get_user2(User, [H | T]) ->
    logger:log(error, "userdb-file: Malformed user record : ~p", [H]),
    get_user2(User, T).

%%--------------------------------------------------------------------
%% Function: get_addresses_using_user(Username)
%%           User = string() or user record()
%% Descrip.: Return all addresses for a username or a user record.
%% Returns : Addresses, list() of address record()
%%--------------------------------------------------------------------
get_addresses_using_user(Username) when is_list(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(error, "userdb-file: No user named ~p, can't look up addresses", [Username]),
	    [];
	User when is_record(User, user) ->
	    get_addresses_using_user2(Username, fetch_addresses(), [])
    end;
get_addresses_using_user(User) when is_record(User, user) ->
    get_addresses_using_user2(User#user.name, fetch_addresses(), []).

get_addresses_using_user2(_Username, [], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_addresses_using_user2(Username, [H | T], Res) when is_record(H, address), H#address.user == Username ->
    %% Username matches the username for the address record (H)
    get_addresses_using_user2(Username, T, lists:append(Res, [H]));
get_addresses_using_user2(Username, [H | T], Res) when is_record(H, address) ->
    get_addresses_using_user2(Username, T, Res).


%%--------------------------------------------------------------------
%% Function: get_users_using_address(Address)
%%           Address = string() | sipurl record()
%% Descrip.: Given an address (list) or URL (sipurl record), locate
%%           and return all address records in the userdb, fetched
%%           from the persistent sipuserdb_file process, that matches
%%           using URI address matching rules.
%% Returns : Usernames, list() of string()
%%--------------------------------------------------------------------
get_users_using_address(Address) when is_list(Address) ->
    %% Check that Address is a parseable URL. It really does not have to
    %% be - it is often the result of canonization of a Request-URI, URL
    %% or address of some form.
    case sipurl:parse(Address) of
	URL when is_record(URL, sipurl) ->
	    case get_user_records_for_url(URL, fetch_addresses(), []) of
		error ->
		    [];
		R when is_list(R) ->
		    collect_usernames(R)
	    end;
	_ ->
	    %% unparseable URL
	    []
    end;
get_users_using_address(URL) when is_record(URL, sipurl) ->
    R = get_user_records_for_url(URL, fetch_addresses(), []),
    collect_usernames(R).


%%--------------------------------------------------------------------
%% Function: get_user_records_for_url(URL, Addresses, [])
%%           URL       = sipurl record()
%%           Addresses = list() of address record() 
%% Descrip.: Given an URL (sipurl record), locate and return all
%%           address records in the userdb, fetched from the
%%	     persistent sipuserdb_file process, that matches using URI
%%           address matching rules, or the user records matching the
%%           address.
%% Returns : Users |
%%           error
%%           Users = list() of user record()
%%--------------------------------------------------------------------
get_user_records_for_url(_URL, [], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_user_records_for_url(URL, [H | T], Res) when is_record(URL, sipurl), is_record(H, address) ->
    case sipurl:url_is_equal(URL, sipurl:parse(H#address.address)) of
	true ->
	    %% Check that there is actually a user for this address record too
	    case get_user(H#address.user) of
		nomatch ->
		    logger:log(error, "userdb-file: Found address record matching URL ~p " ++
			       "but it does not have a matching user record : ~p",
			       [sipurl:print(URL), H]),
		    get_user_records_for_url(URL, T, Res);
		U when is_record(U, user) ->
		    %% Address record matching URL found, and we also have a user
		    %% matching the address records user variable.
		    get_user_records_for_url(URL, T, lists:append(Res, [U]))
	    end;
	_ ->
	    %% URL does NOT match this address record, try next (T)
	    get_user_records_for_url(URL, T, Res)
    end;
get_user_records_for_url(URL, [H | T], Res) when is_record(URL, sipurl) ->
    logger:log(error, "userdb-file: Malformed address record : ~p", [H]),
    get_user_records_for_url(URL, T, Res).


%%--------------------------------------------------------------------
%% Function: collect_usernames(In)
%%           In = list() of user and/or address record()
%% Descrip.: Collect and return a list of all usernames from a
%%           set of user and/or address records.
%% Returns : Usernames, list() of string()
%%--------------------------------------------------------------------
collect_usernames(In) ->
    collect_usernames2(In, []).

collect_usernames2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
collect_usernames2([H | T], Res) when is_record(H, user) ->
    collect_usernames2(T, lists:append(Res, [H#user.name]));
collect_usernames2([H | T], Res) when is_record(H, address) ->
    collect_usernames2(T, lists:append(Res, [H#address.user]));
collect_usernames2([H | T], Res) ->
    logger:log(error, "userdb-file: Input to collect_usernames2 is neither address nor user record : ~p",
	       [H]),
    collect_usernames2(T, Res).


%%--------------------------------------------------------------------
%% Function: collect_addresses(In)
%%           In = list() of address record()
%% Descrip.: Collect and return a list of all addresses from a
%%           set of address records.
%% Returns : list() of string(), list of addresses in string format
%%--------------------------------------------------------------------
collect_addresses(In) ->
    collect_addresses2(In, []).

collect_addresses2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
collect_addresses2([H | T], Res) when is_record(H, address) ->
    collect_addresses2(T, lists:append(Res, [H#address.address]));
collect_addresses2([H | T], Res) ->
    logger:log(error, "userdb-file: Input to collect_addresses2 is not address record : ~p",
	       [H]),
    collect_addresses2(T, Res).


%%--------------------------------------------------------------------
%% Function: find_first_telephonenumber(In)
%%           In = list() of address record()
%% Description: Look through a list of address records and return the
%%              first one that has a URL userpart that is all numeric.
%%              Called from get_telephonenumber_for_user.
%% Returns: Address |
%%          nomatch
%%          Address = address record()
%%--------------------------------------------------------------------
find_first_telephonenumber([]) ->
    nomatch;
find_first_telephonenumber([H | T]) when is_record(H, address) ->
    URL = sipurl:parse(H#address.address),
    IsNumericUser = util:isnumeric(URL#sipurl.user),
    IsTelURL = case URL#sipurl.proto of
		   "tel" -> true;
		   _ -> false
	       end,
    IsE164User = case util:isnumeric(URL#sipurl.user) of
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
    end;
find_first_telephonenumber([H | T]) ->
    logger:log(error, "userdb-file: Input to find_first_telephonenumber is not address record : ~p",
	       [H]),
    find_first_telephonenumber(T).

fetch_users() ->
    {ok, Res} = gen_server:call(sipuserdb_file_backend, {fetch_users}),
    Res.

fetch_addresses() ->
    {ok, Res} = gen_server:call(sipuserdb_file_backend, {fetch_addresses}),
    Res.

reload_userdb() ->
    gen_server:cast(sipuserdb_file_backend, {reload_userdb}).
