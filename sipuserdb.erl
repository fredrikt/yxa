%%%-------------------------------------------------------------------
%%% File    : sipuserdb.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Yxa user database. Implemented to be easy to extend with
%%%           new backend modules. Which backends we use is managed
%%%           through configuration (parameter 'userdb_modules'). We
%%%           query the backends in the specified order and stop when
%%%           a module returns something other than 'nomatch' or
%%%           'error', or there are no more backends to query.
%%% Created : 30 Sep 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------

-module(sipuserdb).

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

-export([behaviour_info/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DEFAULT_MODULES, [sipuserdb_mnesia]).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: behaviour_info(callbacks)
%% Descrip.: Describe all the API functions a module indicating it is
%%           a sipuserdb behaviour module must export. List of tuples
%%           of the function names and their arity.
%% Returns : list() of tuple()
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{yxa_init, 0},
     {get_user_with_address, 1},
     {get_users_for_address_of_record, 1},
     {get_users_for_addresses_of_record, 1},
     {get_users_for_url, 1},
     {get_addresses_for_user, 1},
     {get_addresses_for_users, 1},
     {get_password_for_user, 1},
     {get_classes_for_user, 1},
     {get_telephonenumber_for_user, 1},
     {get_forwards_for_users, 1},
     {get_forward_for_user, 1}
    ];
behaviour_info(_Other) ->
    undefined.

%%--------------------------------------------------------------------
%% Function: yxa_init()
%% Descrip.: Called by sipserver_sip when the application is starting.
%%           Invokes the init/0 function of each configured sipuserdb
%%           module. Returns a list of OTP supervisor child
%%           specifications, or empty list.
%% Returns : Spec
%%--------------------------------------------------------------------
yxa_init() ->
    Modules = sipserver:get_env(userdb_modules, ?DEFAULT_MODULES),
    Res = lists:foldl(fun(M, Acc) ->
			      Acc ++ apply(M, yxa_init, [])
		      end, [], Modules),
    Res.

%%--------------------------------------------------------------------
%% Function: get_user_with_address(Address)
%%           Address = string(), an address in string format.
%% Descrip.: Looks up exactly one user with an Address. Used for
%%           example in REGISTER. If there are multiple users with
%%           this address in our database, this function returns
%%           'error'.
%% Returns:  Username               |
%%           nomatch                |
%%           throw({siperror, ...})
%%           Username = string()
%%--------------------------------------------------------------------
get_user_with_address(Address) ->
    {Src, Res} = module_apply(get_user_with_address, [Address]),
    logger:log(debug, "Userdb: ~p:get_user_with_address ~p -> ~p", [Src, Address, Res]),
    Res.

%%--------------------------------------------------------------------
%% Function: get_users_for_address_of_record(Address)
%%           Address = string(), an address in string format.
%% Descrip.: Get all usernames of users matching an address. Used to
%%           find out to which users we should send a request.
%% Returns : Users                  |
%%           nomatch                |
%%           throw({siperror, ...})
%%           Users = list() of string() 
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    {Src, Res} = module_apply(get_users_for_address_of_record, [Address]),
    logger:log(debug, "Userdb: ~p:get_users_for_address_of_record ~p -> ~p", [Src, Address, Res]),
    Res.

%%--------------------------------------------------------------------
%% Function: get_users_for_addresses_of_record(Addresses)
%%           Addresses = list() of string(), addresses in string
%%                       format.
%% Descrip.: Iterate over a list of addresses of record, return
%%           all users matching one or more of the addresses,
%%           without duplicates.
%% Returns : Users                  |
%%           nomatch                |
%%           throw({siperror, ...})
%%           Users = list() of string() 
%%--------------------------------------------------------------------
get_users_for_addresses_of_record(Addresses) ->
    {Src, Res} = module_apply(get_users_for_addresses_of_record, [Addresses]),
    logger:log(debug, "Userdb: ~p:get_users_for_addresses_of_record ~p -> ~p", [Src, Addresses, Res]),
    Res.

%%--------------------------------------------------------------------
%% Function: get_addresses_for_user(User)
%%           User = string()
%% Descrip.: Get all possible addresses of a user. Both configured
%%           ones, and implicit ones. Used for example to check if a
%%           request from a user has an acceptable From: header.
%% Returns : Addresses              |
%%           nomatch                |
%%           throw({siperror, ...})
%%           Addresses = list() of string()
%%--------------------------------------------------------------------
get_addresses_for_user(User) ->
    {Src, Res} = module_apply(get_addresses_for_user, [User]),
    logger:log(debug, "Userdb: ~p:get_addresses_for_user ~p -> ~p", [Src, User, Res]),
    Res.

%%--------------------------------------------------------------------
%% Function: get_addresses_for_users(UserList)
%%           UserList = list() of string(), usernames
%% Descrip.: Iterate over a list of users, return all their
%%           addresses without duplicates.
%% Returns : Addresses |
%%           nomatch   |
%%           throw({siperror, ...})
%%           error
%%           Addresses = list() of string()
%%--------------------------------------------------------------------
get_addresses_for_users(Users) ->
    {Src, Res} = module_apply(get_addresses_for_users, [Users]),
    logger:log(debug, "Userdb: ~p:get_addresses_for_users ~p -> ~p", [Src, Users, Res]),
    Res.


%%--------------------------------------------------------------------
%% Function: get_users_for_url(URL)
%%           URL = sipurl record()
%% Descrip.: Given an URL that is typically the Request-URI of an
%%           incoming request, make a list of implicit user
%%           addresses and return a list of all users matching any
%%           of these addresses. This is located in here since
%%           user database backends can have their own way of
%%           deriving addresses from a Request-URI.
%% Returns : Usernames              |
%%           nomatch                |
%%           throw({siperror, ...})
%%           Usernames = list() of string()
%%--------------------------------------------------------------------
get_users_for_url(URL) ->
    {Src, Res} = module_apply(get_users_for_url, [URL]),
    logger:log(debug, "Userdb: ~p:get_users_for_url ~p -> ~p", [Src, sipurl:print(URL), Res]),
    Res.


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: get_password_for_user(Username)
%%           Username = string()
%% Descrip.: Returns the password for a user.
%% Returns : Password               |
%%           nomatch                |
%%           throw({siperror, ...})
%%           Password = string()
%%--------------------------------------------------------------------
get_password_for_user(User) ->
    {Src, Res} = module_apply(get_password_for_user, [User]),
    ResStr = case Res of
		 _ when list(Res) -> "password not shown";
		 _ -> atom_to_list(Res)
	     end,
    logger:log(debug, "Userdb: ~p:get_password_for_user ~p -> ~s", [Src, User, ResStr]),
    Res.

%%--------------------------------------------------------------------
%% Function: get_classes_for_user(Username)
%%           Username = string()
%% Descrip.: Return a list of classes for this Username. Classes are
%%           'free-form' atoms of types of PSTN destinations this user
%%           is allowed to call to, through a pstnproxy. What class a
%%           PSTN number is in is determined through pstnproxy
%%           configuration.
%% Returns : Classes                |
%%           nomatch                |
%%           throw({siperror, ...})
%%           Classes = list() of atom()
%%--------------------------------------------------------------------
get_classes_for_user(User) ->
    {Src, Res} = module_apply(get_classes_for_user, [User]),
    logger:log(debug, "Userdb: ~p:get_classes_for_user ~p -> ~p", [Src, User, Res]),
    Res.

%%--------------------------------------------------------------------
%% Function: get_telephonenumber_for_user(User)
%%           User = string()
%% Descrip.: Return the telephone number for a user. We do this by
%%           fetching all addresses for the user and then examining
%%           them to see if any of them is a tel: URL, or has a
%%           user part which is all numeric or is an E.164 number.
%%           The numbering plan in the number return is not specified.
%% Returns : Number                 |
%%           nomatch                |
%%           throw({siperror, ...})
%%           Number = string()
%%--------------------------------------------------------------------
get_telephonenumber_for_user(User) ->
    {Src, Res} = module_apply(get_telephonenumber_for_user, [User]),
    logger:log(debug, "Userdb: ~p:get_telephonenumber_for_user ~p -> ~p", [Src, User, Res]),
    Res.

%%--------------------------------------------------------------------
%% Function: get_forwards_for_users(Usernames)
%%           Usernames = list() of string()
%% Descrip.: Return a list of forwards for this user. Forwards are
%%           currently not in the main Yxa CVS repository - Magnus has
%%           some CVS merging to do.
%% Returns : ForwardList            |
%%           nomatch                |
%%           throw({siperror, ...})
%%           ForwardList = list() of sipproxy_forward record()
%%--------------------------------------------------------------------
get_forwards_for_users(Users) ->
    {Src, Res} = module_apply(get_forwards_for_users, [Users]),
    logger:log(debug, "Userdb: ~p:get_forwards_for_users ~p -> ~p", [Src, Users, Res]),
    Res.

%%--------------------------------------------------------------------
%% Function: get_forward_for_user(Username)
%%           Username = string()
%% Descrip.: Return the forward entry for a user. Forwards are
%%           currently not in the main Yxa CVS repository - Magnus has
%%           some CVS merging to do.
%% Returns : ForwardList            |
%%           nomatch                |
%%           throw({siperror, ...})
%%           ForwardList = list() of sipproxy_forward record()
%%--------------------------------------------------------------------
get_forward_for_user(User) ->
    {Src, Res} = module_apply(get_forward_for_users, [User]),
    logger:log(debug, "Userdb: ~p:get_forward_for_user ~p -> ~p", [Src, User, Res]),
    Res.


%%====================================================================
%% Internal functions
%%====================================================================

module_apply(Function, Args) ->
    Modules = sipserver:get_env(userdb_modules, ?DEFAULT_MODULES),
    module_apply(Modules, Function, Args).

module_apply([], _Function, _Args) ->
    {"*", nomatch};
module_apply([Module | T], Function, Args) ->
    case apply(Module, Function, Args) of
	error ->
	    logger:log(error, "Userdb: Error was returned from ~p:~p (arguments ~p), "
		       "throwing '500 Server Internal Error'",
		       [Module, Function, Args]),
	    throw({siperror, 500, "Server Internal Error"});
	nomatch ->
	    module_apply(T, Function, Args);
	[] ->
	    module_apply(T, Function, Args);
	Res ->
	    {Module, Res}
    end.
