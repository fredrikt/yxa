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


%% Mnesia userdb module


%% Looks up exactly one user with an Address. Used
%% for example in REGISTER. If there are multiple
%% users with an address, this function returns 'error'.
get_user_with_address(Address) ->
    case phone:get_user(Address) of
	{atomic, []} ->
	    case phone:get_users_for_number(Address) of
		{atomic, []} ->
		    logger:log(debug, "userdb-mnesia: No user with number ~p", [Address]),
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
	    %% User exists with this name
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
		       []
	       end,
    case phone:get_users_for_number(Address) of
	{atomic, []} ->
	    UserList;
	{atomic, NumberUsers} ->
	    lists:umerge(UserList, lists:sort(NumberUsers));
	Unknown2 ->
	    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_users_for_number(), address ~p result : ~p",
		       [Address, Unknown2]),
	    UserList
    end.

get_users_for_addresses_of_record([]) ->
    [];
get_users_for_addresses_of_record([Address | Rest]) ->
    lists:append(get_users_for_address_of_record(Address), get_users_for_addresses_of_record(Rest)).

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
		    lists:append([local:canonify_user(User)], local:canonify_numberlist(Numbers));
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

get_addresses_for_users([]) ->
    [];
get_addresses_for_users([User | Rest]) ->
    lists:append(get_addresses_for_user(User), get_addresses_for_users(Rest)).

get_users_for_url(URL) ->
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

get_forwards_for_users([]) ->
    [];
get_forwards_for_users([User | Rest]) ->
    lists:append(get_forward_for_user(User), get_forwards_for_users(Rest)).

get_forward_for_user(User) ->
    case database_forward:fetch(User) of
	{atomic, []} ->
	    logger:log(debug, "userdb-mnesia: No forwards found for user ~p", [User]),
	    [];
	{atomic, [F]} ->
	    F;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from database_forward:fetch(), user ~p result : ~p",
		       [User, Unknown]),
	    []
    end.
