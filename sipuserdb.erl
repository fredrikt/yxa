-module(sipuserdb).
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


% Looks up exactly one user with an Address. Used
% for example in REGISTER. If there are multiple
% users with an address, this function returns {error}.
get_user_with_address(Address) ->
    {Src, Res} = module_apply(get_user_with_address, [Address]),
    logger:log(debug, "Userdb: ~p:get_user_with_address ~p -> ~p", [Src, Address, Res]),
    Res.

% Looks up all users with a given address. Used
% to find out to which users we should send a request.
get_users_for_address_of_record(Address) ->
    {Src, Res} = module_apply(get_users_for_address_of_record, [Address]),
    logger:log(debug, "Userdb: ~p:get_users_for_address_of_record ~p -> ~p", [Src, Address, Res]),
    Res.

get_users_for_addresses_of_record(Addresses) ->
    {Src, Res} = module_apply(get_users_for_addresses_of_record, [Addresses]),
    logger:log(debug, "Userdb: ~p:get_users_for_addresses_of_record ~p -> ~p", [Src, Addresses, Res]),
    Res.

% Gets all addresses for a user. Used for example
% to check if a request from a user has an acceptable
% From: header.
get_addresses_for_user(User) ->
    {Src, Res} = module_apply(get_addresses_for_user, [User]),
    logger:log(debug, "Userdb: ~p:get_addresses_for_user ~p -> ~p", [Src, User, Res]),
    Res.

get_addresses_for_users(Users) ->
    {Src, Res} = module_apply(get_addresses_for_users, [Users]),
    logger:log(debug, "Userdb: ~p:get_addresses_for_users ~p -> ~p", [Src, Users, Res]),
    Res.


% Gets all users for an URL. Used to route incoming requests.
get_users_for_url(URL) ->
    {Src, Res} = module_apply(get_users_for_url, [URL]),
    logger:log(debug, "Userdb: ~p:get_users_for_url ~p -> ~p", [Src, sipurl:print(URL), Res]),
    Res.


% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_password_for_user(User) ->
    {Src, Res} = module_apply(get_password_for_user, [User]),
    ResStr = case Res of
	_ when list(Res) -> "password not shown";
	_ -> atom_to_list(Res)
    end,
    logger:log(debug, "Userdb: ~p:get_password_for_user ~p -> ~s", [Src, User, ResStr]),
    Res.

get_classes_for_user(User) ->
    {Src, Res} = module_apply(get_classes_for_user, [User]),
    logger:log(debug, "Userdb: ~p:get_classes_for_user ~p -> ~p", [Src, User, Res]),
    Res.

get_telephonenumber_for_user(User) ->
    {Src, Res} = module_apply(get_telephonenumber_for_user, [User]),
    logger:log(debug, "Userdb: ~p:get_telephonenumber_for_user ~p -> ~p", [Src, User, Res]),
    Res.

get_forwards_for_users(Users) ->
    {Src, Res} = module_apply(get_forwards_for_users, [Users]),
    logger:log(debug, "Userdb: ~p:get_forwards_for_users ~p -> ~p", [Src, Users, Res]),
    Res.

get_forward_for_user(User) ->
    {Src, Res} = module_apply(get_forward_for_users, [User]),
    logger:log(debug, "Userdb: ~p:get_forward_for_user ~p -> ~p", [Src, User, Res]),
    Res.



module_apply(Function, Args) ->
    Modules = sipserver:get_env(userdb_modules, [sipuserdb_mnesia]),
    module_apply(Modules, Function, Args).

module_apply([], _, _) ->
    {"*", nomatch};
module_apply([Module | NextModule], Function, Args) ->
    case apply(Module, Function, Args) of
	error ->	
	    logger:log(debug, "Userdb: Error was returned from ~p:~p (arguments ~p), throwing 500 Server Internal Error",
	    		[Module, Function, Args]),
	    throw({siperror, 500, "Server Internal Error"});
	nomatch ->
	    module_apply(NextModule, Function, Args);
	[] ->
	    module_apply(NextModule, Function, Args);
	Res ->
	    {Module, Res}
    end.
