-module(sipuserdb_mnesia).
-export([get_user_with_address/1,
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

%% Mnesia userdb module


%% Looks up exactly one user with an Address. Used
%% for example in REGISTER. If there are multiple
%% users with an address, this function returns 'error'.
get_user_with_address(Address) ->
    case phone:get_user(Address) of
	{atomic, []} ->
	    case phone:get_users_for_number(Address) of
		{atomic, []} ->
		    logger:log(debug, "userdb-mnesia: No user with name or number ~p", [Address]),
		    nomatch;
		{atomic, [User]} ->
		    User;
		{atomic, Users} ->
		    logger:log(debug, "userdb-mnesia: More than one user with number ~p (~p)", [Address, Users]),
		    error;
		Unknown ->
		    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_users_for_number(), address ~p result : ~p",
			       [Address, Unknown]),
		    error
	    end;
	{atomic, [User]} ->
	    %% There exists exactly one user with this name. Return the name of the user,
	    %% not the user record returned from phone:get_user().
	    Address;
	{atomic, Users} ->
	    logger:log(debug, "userdb-mnesia: More than one user with username ~p (~p)", [Address, Users]),
	    error;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_user(), address ~p result : ~p",
		       [Address, Unknown]),
	    error
    end.

%% Looks up all users with a given address. Used
%% to find out to which users we should send a request.
%% Other userdb types than mnesia might perform multiple lookups.
get_users_for_address_of_record(Address) ->
    UserList = case phone:get_user(Address) of
		   {atomic, []} ->
		       [];
		   {atomic, User} ->
		       %% There exists a user with this name
		       [Address];
		   Unknown1 ->
		       logger:log(error, "userdb-mnesia: Unexpected result from phone:get_user(), address ~p result : ~p",
				  [Address, Unknown1]),
		       error
	       end,
    case UserList of
	error ->
	    error;
	_ ->
	    case phone:get_users_for_number(Address) of
		{atomic, []} ->
		    UserList;
		{atomic, NumberUsers} ->
		    lists:usort(UserList, lists:sort(NumberUsers));
		Unknown2 ->
		    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_users_for_number(), address ~p result : ~p",
			       [Address, Unknown2]),
		    UserList
	    end
    end.

get_users_for_addresses_of_record(In) ->
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
%%              addresses without duplicates. Uses the next function,
%%              get_addresses_for_user/1.
%% Returns: ListOfAddresses
%%--------------------------------------------------------------------
get_addresses_for_users([]) ->
    [];
get_addresses_for_users([User | Rest]) ->
    lists:append(get_addresses_for_user(User), get_addresses_for_users(Rest)).


%% Gets all addresses for a user. Used for example
%% to check if a request from a user has an acceptable
%% From: header.
get_addresses_for_user(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    logger:log(debug, "userdb-mnesia: No such user ~p", [User]),
	    nomatch;
	{atomic, [UserEntry]} ->
	    case phone:get_numbers_for_user(User) of
		{atomic, []} ->
		    logger:log(debug, "userdb-mnesia: No numbers for user ~p", [User]),
		    [local:canonify_user(User)];
		{atomic, Numbers} ->
		    logger:log(debug, "userdb-mnesia: Found number(s) ~p for user ~p",
			       [Numbers, User]),
		    CanonL = [local:canonify_user(User)],
		    NumberL = local:canonify_numberlist(Numbers),
		    All = lists:append([CanonL, NumberL]),
		    lists:usort(All);
		Unknown ->
		    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_numbers_for_user(), user ~p result : ~p",
			       [User, Unknown]),
		    error
	    end;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_user(), user ~p result : ~p",
		       [User, Unknown]),
	    error
    end.


%% Function: get_users_for_url/1
%% Description: Given an URL that is typically the Request-URI of an
%%              incoming request, make a list of implicit user
%%              addresses and return a list of all users matching any
%%              of these addresses.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
get_users_for_url(URL) when record(URL, sipurl) ->
    Addresses = local:lookup_url_to_addresses(sipuserdb_mnesia, URL),
    logger:log(debug, "userdb-mnesia: Looking for users matching address(es) ~p derived from URL ~p",
	       [Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_password_for_user(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    logger:log(debug, "userdb-mnesia: No such user ~p when fetching password", [User]),
	    nomatch;
	{atomic, [A]} ->
	    {Password, Flags, Classes} = A,
	    Password;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_user(), user ~p result : ~p",
		       [User, Unknown]),
	    error
    end.

get_classes_for_user(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    logger:log(debug, "userdb-mnesia: No such user ~p when fetching password", [User]),
	    nomatch;
	{atomic, [A]} ->
	    {Password, Flags, Classes} = A,
	    Classes;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_user(), user ~p result : ~p",
		       [User, Unknown]),
	    error
    end.

get_telephonenumber_for_user(User) ->
    case phone:get_numbers_for_user(User) of
	{atomic, [FirstNumber | _]} ->
	    FirstNumber;
	_ ->
	    nomatch
    end.

get_forwards_for_users(In) ->
    get_forwards_for_users2(In, []).

get_forwards_for_users2([], Res) ->
    lists:usort(Res);
get_forwards_for_users2([H | T], Res) ->
    case get_forward_for_user(H) of
	error ->
	    error;
	nomatch ->
	    get_forwards_for_users2(T, Res);
	Fwd ->
	    get_forwards_for_users2(T, lists:append(Res, [Fwd]))
    end.

get_forward_for_user(User) ->
    case database_forward:fetch(User) of
	{atomic, []} ->
	    logger:log(debug, "userdb-mnesia: No forwards found for user ~p", [User]),
	    nomatch;
	{atomic, [F]} ->
	    F;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from database_forward:fetch(), user ~p result : ~p",
		       [User, Unknown]),
	    error
    end.
