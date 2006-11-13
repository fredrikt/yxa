%%%-------------------------------------------------------------------
%%% File    : sipuserdb_ldap.erl
%%% Author  : Fredrik Thulin
%%% Descrip.: A sipuserdb module with a Mnesia backend. Uses the ol'
%%%           functions in phone.erl of course.
%%%
%%% Created : 30 Sep 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipuserdb_mnesia).

-behaviour(sipuserdb).

%%--------------------------------------------------------------------
%% External exports
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
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").

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
    [].

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
	{atomic, [_User]} ->
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

%%--------------------------------------------------------------------
%% Function: get_users_for_address_of_record(Address)
%%           Address = string(), an address in string format.
%% Descrip.: Get all usernames of users matching an address. Used to
%%           find out to which users we should send a request.
%% Returns : Users   |
%%           nomatch |
%%           error
%%           Users = list() of string() 
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    UserList = case phone:get_user(Address) of
		   {atomic, []} ->
		       [];
		   {atomic, _User} ->
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
		    lists:umerge(lists:usort(UserList), lists:usort(NumberUsers));
		Unknown2 ->
		    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_users_for_number(), address ~p result : ~p",
			       [Address, Unknown2]),
		    UserList
	    end
    end.

%%--------------------------------------------------------------------
%% Function: get_users_for_addresses_of_record(Addresses)
%%           Addresses = list() of string(), addresses in string
%%                       format.
%% Descrip.: Iterate over a list of addresses of record, return
%%           all users matching one or more of the addresses,
%%           without duplicates.
%% Returns : Users   |
%%           nomatch |
%%           error
%%           Users = list() of string() 
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% Function: get_addresses_for_users(UserList)
%%           UserList = list() of string(), usernames
%% Descrip.: Iterate over a list of users, return all their
%%           addresses without duplicates.
%%           Uses the next function, get_addresses_for_user/1.
%% Returns : Addresses |
%%           nomatch   |
%%           error
%%           Addresses = list() of string()
%%--------------------------------------------------------------------
get_addresses_for_users([]) ->
    [];
get_addresses_for_users([User | Rest]) ->
    lists:append(get_addresses_for_user(User), get_addresses_for_users(Rest)).


%%--------------------------------------------------------------------
%% Function: get_addresses_for_user(User)
%%           User = string()
%% Descrip.: Get all possible addresses of a user. Both configured
%%           ones, and implicit ones. Used for example to check if a
%%           request from a user has an acceptable From: header.
%% Returns : Addresses |
%%           error
%%           Addresses = list() of string()
%%--------------------------------------------------------------------
get_addresses_for_user(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    logger:log(debug, "userdb-mnesia: No such user ~p", [User]),
	    nomatch;
	{atomic, [_UserEntry]} ->
	    %% Ok, user exists
	    case phone:get_numbers_for_user(User) of
		{atomic, []} ->
		    logger:log(debug, "userdb-mnesia: No numbers for user ~p", [User]),
		    [local:canonify_user(User)];
		{atomic, Addresses} ->
		    logger:log(debug, "userdb-mnesia: Found address(es)/number(s) ~p for user ~p",
			       [Addresses, User]),
		    CanonL = [local:canonify_user(User)],
		    AddrL = local:canonify_addresses(Addresses),
		    All = lists:append([CanonL, AddrL]),
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
get_users_for_url(URL) when record(URL, sipurl) ->
    Addresses = local:lookup_url_to_addresses(sipuserdb_mnesia, URL),
    logger:log(debug, "userdb-mnesia: Looking for users matching address(es) ~p derived from URL ~p",
	       [Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: get_password_for_user(Username)
%%           Username = string()
%% Descrip.: Returns the password for a user.
%% Returns : Password |
%%           nomatch  |
%%           error
%%           Password = string()
%%--------------------------------------------------------------------
get_password_for_user(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    logger:log(debug, "userdb-mnesia: No such user ~p when fetching password", [User]),
	    nomatch;
	{atomic, [{Password, _Flags, _Classes}]} ->
	    Password;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_user(), user ~p result : ~p",
		       [User, Unknown]),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: get_classes_for_user(Username)
%%           Username = string()
%% Descrip.: Return a list of classes for this Username. Classes are
%%           'free-form' atoms of types of PSTN destinations this user
%%           is allowed to call to, through a pstnproxy. What class a
%%           PSTN number is in is determined through pstnproxy
%%           configuration.
%% Returns : Classes |
%%           nomatch |
%%           error
%%           Classes = list() of atom()
%%--------------------------------------------------------------------
get_classes_for_user(User) ->
    case phone:get_user(User) of
	{atomic, []} ->
	    logger:log(debug, "userdb-mnesia: No such user ~p when fetching classes", [User]),
	    nomatch;
	{atomic, [{_Password, _Flags, Classes}]} ->
	    Classes;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_user(), user ~p result : ~p",
		       [User, Unknown]),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: get_telephonenumber_for_user(User)
%%           User = string()
%% Descrip.: Return the telephone number for a user.
%% Returns : Number  |
%%           nomatch
%%           Number = string()
%%--------------------------------------------------------------------
get_telephonenumber_for_user(User) ->
    case phone:get_numbers_for_user(User) of
	{atomic, NumberL} ->
	    case find_first_telephonenumber(NumberL) of
		FirstNumber when is_list(FirstNumber) ->
		    FirstNumber;
		nomatch ->
		    logger:log(debug, "userdb-mnesia: No numbers for user ~p", [User]),
		    nomatch
	    end;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from phone:get_numbers_for_user(), user ~p result : ~p",
		       [User, Unknown]),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: get_forwards_for_users(Usernames)
%%           Usernames = list() of string()
%% Descrip.: Return a list of forwards for this user. Forwards are
%%           currently not in the main YXA CVS repository - Magnus has
%%           some CVS merging to do.
%% Returns : ForwardList |
%%           nomatch
%%           ForwardList = list() of sipproxy_forward record()
%%--------------------------------------------------------------------
get_forwards_for_users(In) ->
    get_forwards_for_users2(In, []).

get_forwards_for_users2([], Res) ->
    lists:usort(Res);
get_forwards_for_users2([H | T], Res) ->
    case get_forward_for_user(H) of
	nomatch ->
	    get_forwards_for_users2(T, Res);
	Fwd ->
	    get_forwards_for_users2(T, lists:append(Res, [Fwd]))
    end.

%%--------------------------------------------------------------------
%% Function: get_forward_for_user(Username)
%%           Username = string()
%% Descrip.: Return the forward entry for a user. Forwards are
%%           currently not in the main YXA CVS repository - Magnus has
%%           some CVS merging to do.
%% Returns : ForwardList |
%%           nomatch
%%           ForwardList = list() of sipproxy_forward record()
%%--------------------------------------------------------------------
get_forward_for_user(User) ->
    case database_forward:fetch(User) of
	{ok, []} ->
	    logger:log(debug, "userdb-mnesia: No forwards found for user ~p", [User]),
	    nomatch;
	{ok, Fwds} when is_list(Fwds) ->
	    Fwds;
	Unknown ->
	    logger:log(error, "userdb-mnesia: Unexpected result from database_forward:fetch(), user ~p result : ~p",
		       [User, Unknown]),
	    error
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: find_first_telephonenumber(In)
%%           In = list() of string()
%% Descrip.: Look through a list of addresses (numbers) for a user and
%%           return the first one that is a valid phone number.
%% Returns : Number |
%%           nomatch
%%           Number = string()
%%--------------------------------------------------------------------
find_first_telephonenumber([]) ->
    nomatch;
find_first_telephonenumber(["tel:+" ++ Rest | T]) ->
    case util:isnumeric(Rest) of
	true ->
	    "+" ++ Rest;
	false ->
	    find_first_telephonenumber(T)
    end;
find_first_telephonenumber([H | T]) when is_list(H) ->
    case sipurl:parse(H) of
	#sipurl{user = User} ->
	    IsNumericUser = util:isnumeric(User),
	    IsE164User =
		case User of
		    "+" ++ Rest ->
			util:isnumeric(Rest);
		    _ ->
			false
		end,
	    if
		IsNumericUser == true -> User;
		IsE164User == true -> User;
		true ->
		    find_first_telephonenumber(T)
	    end;
	_ ->
	    %% unparsable URL
	    find_first_telephonenumber(T)
    end.

