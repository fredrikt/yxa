%%% File    : sipuserdb_static.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Purpose : A static user database module. The user data you want to
%%%           edit is in the file sipuserdb_static.hrl.
%%% Created : 16 Apr 2004 by Fredrik Thulin <ft@it.su.se>

-module(sipuserdb_static).
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

-include("siprecords.hrl").

%%-compile(export_all).

%% Static userdb module

-record(user, {name, password, classes, forward}).
-record(address, {user, address}).

%%
%% The file included below is the user database. You should not edit
%% sipuserdb_static.erl, just fill out the data in sipuserdb_static.hrl
%%
-include("sipuserdb_static.hrl").

%% Function: yxa_init/0
%% Description: Perform any necessary startup initialization and
%%              return an OTP supervisor child spec if we want to add
%%              to sipserver_sup's list. If this sipuserdb_module
%%              needs to be persistent, it should be a gen_server and
%%              init should just return a spec so that the gen_server
%%              is started by the supervisor.
%% Returns: Spec |
%%          []
%%--------------------------------------------------------------------
yxa_init() ->
    io:format("sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate immediately!~n"),
    [].

%% Function: get_user_with_address/1
%% Description: Looks up exactly one user with an Address. Used for
%%              example in REGISTER. If there are multiple users with
%%              an address, this function returns 'error'.
%% Returns: Username |
%%          nomatch  |
%%          error
%%--------------------------------------------------------------------
get_user_with_address(Address) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    case get_users_using_address(Address) of
	[] ->
	    logger:log(debug, "userdb-static: No user with address ~p", [Address]),
	    nomatch;
	[User] ->
	    %% There exists exactly one user with this name
	    User;
	Users when list(Users) ->
	    logger:log(debug, "userdb-static: More than one user with address ~p (~p)", [Address, Users]),
	    error;
	Unknown ->
	    logger:log(error, "userdb-static: Unexpected result from get_users_using_address(), address ~p result : ~p",
		       [Address, Unknown]),
	    error
    end.


%% Function: get_users_for_address_of_record/1
%% Description: Get all usernames of users matching an address. Used
%%              to find out to which users we should send a request.
%% Returns: ListOfUsers |
%%          error
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    case get_users_using_address(Address) of
	L when list(L) ->
	    L;
	Unknown ->
	    logger:log(error, "userdb-static: Unexpected result from get_users_using_address(), address ~p result : ~p",
		       [Address, Unknown]),
	    error
    end.


%% Function: get_users_for_addresses_of_record/1
%% Description: Iterate over a list of addresses of record, return
%%              all users matching one or more of the addresses,
%%              without duplicates.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
get_users_for_addresses_of_record(In) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    get_users_for_addresses_of_record2(In, []).

get_users_for_addresses_of_record2([], Res) ->
    lists:usort(Res);
get_users_for_addresses_of_record2([H | T], Res) ->
    case get_users_for_address_of_record(H) of
	Users when list(Users) ->
	    get_users_for_addresses_of_record2(T, lists:append(Res, Users));
	_ ->
	    get_users_for_addresses_of_record2(T, Res)
    end.


%% Function: get_addresses_for_users/1
%% Description: Iterate over a list of users, return all their
%%              addresses without duplicates by using the next
%%              function, get_addresses_for_user/1.
%% Returns: ListOfAddresses
%%--------------------------------------------------------------------
get_addresses_for_users(In) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    get_addresses_for_users2(In, []).

get_addresses_for_users2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_addresses_for_users2([H | T], Res) ->
    case get_addresses_for_user(H) of
	A when list(A) ->
	    get_addresses_for_users2(T, lists:append(Res, A));
	_ ->
	    get_addresses_for_users2(T, Res)
    end.


%% Function: get_addresses_for_user/1
%% Description: Get all possible addresses of a user. Both configured
%%              ones, and implcit ones. Used for example to check if a
%%              request from a user has an acceptable From: header.
%% Returns: ListOfAddresses |
%%          nomatch         |
%%          error
%%--------------------------------------------------------------------
get_addresses_for_user(Username) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-static: No such user ~p", [Username]),
	    [];
	User when record(User, user) ->
	    collect_addresses(get_addresses_using_user(User));
	Unknown ->
	    logger:log(error, "userdb-static: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%% Function: get_users_for_url/1
%% Description: Given an URL that is typically the Request-URI of an
%%              incoming request, make a list of implicit user
%%              addresses and return a list of all users matching any
%%              of these addresses. This is located in here since
%%              user database backends can have their own way of
%%              deriving addresses from a Request-URI.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
get_users_for_url(URL) when record(URL, sipurl) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    Addresses = local:lookup_url_to_addresses(sipuserdb_static, URL),
    logger:log(debug, "userdb-static: Looking for users matching address(es) ~p derived from URL ~p",
	       [Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Function: get_password_for_user/1
%% Description: Returns the password for a user.
%% Returns: PasswordString |
%%          nomatch        |
%%          error
%%--------------------------------------------------------------------
get_password_for_user(Username) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-static: No such user ~p when fetching password", [Username]),
	    nomatch;
	User when record(User, user) ->
	    User#user.password;
	Unknown ->
	    logger:log(error, "userdb-static: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%% Function: get_classes_for_users/1
%% Description: Returns a list of classes allowed for a user. Classes
%%              are used by pstnproxy to determine if it should allow
%%              a call to a PSTN number (of a certain class) from a
%%              user or not.
%% Returns: ClassesList |
%%          nomatch
%%--------------------------------------------------------------------
get_classes_for_user(Username) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-static: No such user ~p when fetching password", [Username]),
	    nomatch;
	User when record(User, user) ->
	    case User#user.classes of
		_ when User#user.classes == none ; User#user.classes == undefined ->
		    [];
		_ ->
		    User#user.classes
	    end;
	Unknown ->
	    logger:log(error, "userdb-static: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%% Function: get_telephonenumber_for_user/1
%% Description: Return the telephone number for a user. We do this by
%%              fetching all addresses for the user and then examining
%%              them to see if any of them is a tel: URL, or has a
%%              user part which is all numeric or is an E.164 number.
%% Returns: NumberString |
%%          nomatch      |
%%          error
%%--------------------------------------------------------------------
get_telephonenumber_for_user(Username) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-static: No such user ~p when fetching telephone number", [Username]),
	    nomatch;
	User when record(User, user) ->
	    case get_addresses_using_user(User) of
		nomatch ->
		    nomatch;
		error ->
		    error;
		A when list(A) ->
		    %% Look through the addresses returned and return the first one
		    %% which has an all numeric user-part.
		    case find_first_telephonenumber(A) of
			nomatch ->
			    %% XXX Should we perhaps turn that into 'error' since we
			    %% apparenlty had a user with this name in our database but
			    %% that user did not have an address that looks like a phone
			    %% number?
			    logger:log(debug, "userdb-static: User ~p has no address that looks like a phone number (addresses : ~p)",
				       [Username, collect_addresses(A)]),
			    nomatch;
			Address when record(Address, address) ->
			    URL = sipurl:parse(Address#address.address),
			    URL#sipurl.user
		    end;
		Unknown ->
		    logger:log(error, "userdb-static: Unexpected result from get_addresses_for_user(), user ~p result : ~p",
			       [Username, Unknown]),
		    error
	    end;
	Unknown ->
	    logger:log(error, "userdb-static: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%% Function: get_forwards_for_users/1
%% Description: Return a list of all forwards for a list of users.
%%              Uses the next function, get_forward_for_user/1.
%% Returns: ForwardList
%%--------------------------------------------------------------------
get_forwards_for_users(In) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    get_forwards_for_users2(In, []).

get_forwards_for_users2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_forwards_for_users2([H | T], Res) ->
    case get_forward_for_user(H) of
	nomatch ->
	    get_forwards_for_users2(T, Res);
	Fwd ->
	    get_forwards_for_users2(T, lists:append(Res, [Fwd]))
    end.


%% Function: get_forward_for_user/1
%% Description: Return the forward for a user.
%% Returns: Forward |
%%          nomatch |
%%          error
%%--------------------------------------------------------------------
get_forward_for_user(Username) ->
    logger:log(error, "sipuserdb_static is OBSOLETED by sipuserdb_file, you should migrate your user database immediately!"),
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-static: No forwards found for user ~p", [Username]),
	    nomatch;
	User when record(User, user) ->
	    case User#user.forward of
		Foo when Foo == none; Foo == []; Foo == undefined ->
		    nomatch;
		L when list(L) ->
		    L;
		U ->
		    logger:log(error, "userdb-static: User ~p has invalid forward in database : ~p",
			       [Username, User#user.forward]),
		    error
	    end;
	Unknown ->
	    logger:log(error, "userdb-static: Unexpected result from database_forward:fetch(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%% Function: get_user/1
%% Description: Fetch a user record from the static user database,
%%              given it's username.
%% Returns: User    |
%%          nomatch
%%--------------------------------------------------------------------
get_user(User) ->
    get_user2(User, ?USERS).

get_user2(User, []) ->
    nomatch;
get_user2(User, [H | T]) when record(H, user), H#user.name == User ->
    H;
get_user2(User, [H | T]) when record(H, user) ->
    get_user2(User, T);
get_user2(User, [H | T]) ->
    logger:log(error, "userdb-static: Malformed user record : ~p", [H]),
    get_user2(User, T).


%% Function: get_addresses_using_user/1
%% Description: Return all addresses for a username or a user record.
%% Returns: ListOfAddresses
%%--------------------------------------------------------------------
get_addresses_using_user(Username) when list(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(error, "userdb-static: No user named ~p, can't look up addresses", [Username]),
	    [];
	User when record(User, user) ->
	    get_addresses_using_user2(Username, ?ADDRESSES, [])
    end;
get_addresses_using_user(User) when record(User, user) ->
    get_addresses_using_user2(User#user.name, ?ADDRESSES, []).

get_addresses_using_user2(Username, [], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_addresses_using_user2(Username, [H | T], Res) when record(H, address), H#address.user == Username ->
    %% Username matches the username for the address record (H)
    get_addresses_using_user2(Username, T, lists:append(Res, [H]));
get_addresses_using_user2(Username, [H | T], Res) when record(H, address) ->
    get_addresses_using_user2(Username, T, Res).


%% Function: get_users_using_address/1
%% Description: Given an address (list) or URL (sipurl record), locate
%%              and return all address records in the static userdb
%%              that matches using URI address matching rules.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
get_users_using_address(Address) when list(Address) ->
    R = get_user_records_for_url(sipurl:parse(Address), ?ADDRESSES, []),
    collect_usernames(R);
get_users_using_address(URL) when record(URL, sipurl) ->
    R = get_user_records_for_url(URL, ?ADDRESSES, []),
    collect_usernames(R).


%% Function: get_user_records_for_url/4
%% Description: Given an address (list) or URL (sipurl record), locate
%%              and return all address records in the static userdb
%%              that matches using URI address matching rules, or
%%              the user records matching the address.
%% Returns: ListOfUserRecords |
%%          error
%%--------------------------------------------------------------------
get_user_records_for_url(URL, [], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_user_records_for_url(URL, [H | T], Res) when record(URL, sipurl), record(H, address) ->
    case sipurl:url_is_equal(URL, sipurl:parse(H#address.address)) of
	true ->
	    %% Check that there is actually a user for this address record too
	    case get_user(H#address.user) of
		nomatch ->
		    logger:log(error, "userdb-static: Found address record matching URL ~p " ++
			       "but it does not have a matching user record : ~p",
			       [sipurl:print(URL), H]),
		    get_user_records_for_url(URL, T, Res);
		U when record(U, user) ->
		    %% Address record matching URL found, and we also have a user
		    %% matching the address records user variable.
		    get_user_records_for_url(URL, T, lists:append(Res, [U]))
	    end;
	_ ->
	    %% URL does NOT match this address record, try next (T)
	    get_user_records_for_url(URL, T, Res)
    end;
get_user_records_for_url(URL, [H | T], Res) when record(URL, sipurl) ->
    logger:log(error, "userdb-static: Malformed address record : ~p", [H]),
    get_user_records_for_url(URL, T, Res);
get_user_records_for_url(URL, [H | T], Res) ->
    logger:log(error, "userdb-static: Malformed URL in get_user_records_for_url : ~p", [URL]),
    error.


%% Function: collect_usernames/1
%% Description: Collect and return a list of all usernames from a
%%              set of user and/or address records.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
collect_usernames(In) ->
    collect_usernames2(In, []).

collect_usernames2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
collect_usernames2([H | T], Res) when record(H, user) ->
    collect_usernames2(T, lists:append(Res, [H#user.name]));
collect_usernames2([H | T], Res) when record(H, address) ->
    collect_usernames2(T, lists:append(Res, [H#address.user]));
collect_usernames2([H | T], Res) ->
    logger:log(error, "userdb-static: Input to collect_usernames2 is neither address nor user record : ~p",
	       [H]),
    collect_usernames2(T, Res).


%% Function: collect_addresses/1
%% Description: Collect and return a list of all addresses from a
%%              set of address records.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
collect_addresses(In) ->
    collect_addresses2(In, []).

collect_addresses2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
collect_addresses2([H | T], Res) when record(H, address) ->
    collect_addresses2(T, lists:append(Res, [H#address.address]));
collect_addresses2([H | T], Res) ->
    logger:log(error, "userdb-static: Input to collect_addresses2 is not address record : ~p",
	       [H]),
    collect_addresses2(T, Res).


%% Function: find_first_telephonenumber/1
%% Description: Look through a list of address records and return the
%%              first one that has a URL userpart that is all numeric.
%%              Called from get_telephonenumber_for_user.
%% Returns: Address |
%%          nomatch
%%--------------------------------------------------------------------
find_first_telephonenumber([]) ->
    nomatch;
find_first_telephonenumber([H | T]) when record(H, address) ->
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
    logger:log(error, "userdb-static: Input to find_first_telephonenumber is not address record : ~p",
	       [H]),
    find_first_telephonenumber(T).
