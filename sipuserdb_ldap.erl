-module(sipuserdb_ldap).
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


% LDAP userdb module

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
    [].

% Looks up exactly one user with an Address. Used
% for example in REGISTER. If there are multiple
% users with an address, this function returns 'error'.
get_user_with_address(Address) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    nomatch;
	Server ->
	    UserAttribute = sipserver:get_env(ldap_userattribute, "sipAuthenticationUser"),
	    AddressAttribute = sipserver:get_env(ldap_addressattribute, "sipLocalAddress"),
	    Res = directory:ldapsearch(Server, AddressAttribute, Address, [UserAttribute]),
	    case Res of
		error ->
		    error;
		none ->
		    logger:log(debug, "userdb-ldap: No user found for address ~p in LDAP (server ~p)", [Address, Server]),
		    nomatch;
		[{dn, Dn, attributes, EAttributes}] ->
		    case directory:get_value(EAttributes, UserAttribute) of
			QRes when list(QRes) ->
			    logger:log(debug, "userdb-ldap: Found LDAP user for ~p, dn ~p: ~p", [Address, Dn, QRes]),
			    QRes;
			Foo ->
			    logger:log(debug, "userdb-ldap: Query for exactly one user matching address ~p failed (~p)",
					[Address, Foo]),
			    error
		    end;
		[{dn, Dn, attributes, EAttributes} | Rest] ->
		    logger:log(debug, "userdb-ldap: Query for exactly one user matching address ~p failed, multiple users found",
		    		[Address]),
		    error;
		Unknown ->
		    logger:log(debug, "userdb-ldap: Query for exactly one user matching address ~p failed, ldapsearch returned : ~p",
		    		[Address, Unknown]),
		    error
	    end
    end.

% Looks up all users with a given address. Used
% to find out to which users we should send a request.
% Other userdb types than mnesia might perform multiple lookups.
get_users_for_address_of_record(Address) ->
    get_users_for_addresses_of_record([Address]).

get_users_for_addresses_of_record(AddressList) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    nomatch;
	Server ->
	    UserAttribute = sipserver:get_env(ldap_userattribute, "sipAuthenticationUser"),
	    AddressAttribute = sipserver:get_env(ldap_addressattribute, "sipLocalAddress"),
	    case get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, AddressList) of
		Res when list(Res) ->
		    SortedRes = lists:usort(Res),
		    logger:log(debug, "userdb-ldap: Found user(s) ~p for address(es) ~p", [SortedRes, AddressList]),
		    SortedRes;
		Res ->
		    Res
	    end
    end.

get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, []) ->
    [];
get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, [Address | Rest]) ->
    Res = directory:ldapsearch_simple(Server, AddressAttribute, Address, UserAttribute),
    case Res of
	error ->
	    error;
	none ->
	    get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, Rest);
	Users when list(Users) ->
	    lists:append(Users, get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, Rest))
    end.


% Gets all addresses for a user. Used for example
% to check if a request from a user has an acceptable
% From: header.
get_addresses_for_user(User) ->
    get_addresses_for_users([User]).

get_addresses_for_users(UserList) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    nomatch;
	Server ->
	    UserAttribute = sipserver:get_env(ldap_userattribute, "sipAuthenticationUser"),
	    AddressAttribute = sipserver:get_env(ldap_addressattribute, "sipLocalAddress"),
	    case get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, UserList) of
		Res when list(Res) ->
		    SortedRes = lists:usort(Res),
		    logger:log(debug, "userdb-ldap: Found address(es) ~p for user(s) ~p", [SortedRes, UserList]),
		    SortedRes;
		Res ->
		    Res
	    end
    end.

get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, []) ->
    [];
get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, [User | Rest]) ->
    Res = directory:ldapsearch_simple(Server, UserAttribute, User, AddressAttribute),
    case Res of
	error ->
	    error;
	none ->
	    get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, Rest);
	Addresses when list(Addresses) ->
	    lists:append(Addresses, get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, Rest))
    end.

get_users_for_url(URL) ->
    Addresses = local:lookup_url_to_addresses(sipuserdb_ldap, URL),
    logger:log(debug, "userdb-ldap: Looking for users matching address(es) ~p derived from URL ~p",
		[Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).


% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_password_for_user(User) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    nomatch;
	Server ->
	    UserAttribute = sipserver:get_env(ldap_userattribute, "sipAuthenticationUser"),
	    PasswordAttribute = sipserver:get_env(ldap_passwordattribute, "sipPassword"),
	    case directory:ldapsearch_simple(Server, UserAttribute, User, PasswordAttribute) of
		error ->
		    error;
		none ->
		    logger:log(debug, "userdb-ldap: Found no password for user ~p", [User]),
		    nomatch;
		Addresses when list(Addresses) ->
		    Addresses
	    end
    end.

get_classes_for_user(User) ->
    logger:log(debug, "userdb-ldap: Classes are not yet implemented in the LDAP userdb module"),
    [].

get_telephonenumber_for_user(User) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    nomatch;
	Server ->
	    UserAttribute = sipserver:get_env(ldap_userattribute, "sipAuthenticationUser"),
	    TelephoneNumberAttribute = sipserver:get_env(ldap_telephonenumberattribute, "telephoneNumber"),
	    case directory:ldapsearch_simple(Server, UserAttribute, User, TelephoneNumberAttribute) of
		error ->
		    error;
		none ->
		    logger:log(debug, "userdb-ldap: Found no telephone number for user ~p", [User]),
		    nomatch;
		Number when list(Number) ->
		    Number
	    end
    end.

get_forwards_for_users(User) ->
    logger:log(debug, "userdb-ldap: Forwards are not yet implemented in the LDAP userdb module"),
    nomatch.

get_forward_for_user(User) ->
    logger:log(debug, "userdb-ldap: Forwards are not yet implemented in the LDAP userdb module"),
    nomatch.

