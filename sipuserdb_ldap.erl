%%%-------------------------------------------------------------------
%%% File    : sipuserdb_ldap.erl
%%% Author  : Fredrik Thulin
%%% Descrip.: A sipuserdb module with an LDAP backend.
%%%
%%% Created : 09 Oct 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipuserdb_ldap).

-behaviour(sipuserdb).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 yxa_init/0,
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
-include("directory.hrl").

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
		[LDAPres] when is_record(LDAPres, ldapres) ->
		    case directory:get_value(LDAPres, UserAttribute) of
			QRes when is_list(QRes) ->
			    Dn = LDAPres#ldapres.dn,
			    logger:log(debug, "userdb-ldap: Found LDAP user for ~p, dn ~p: ~p", [Address, Dn, QRes]),
			    QRes;
			Foo ->
			    logger:log(debug, "userdb-ldap: Query for exactly one user matching address ~p failed (~p)",
				       [Address, Foo]),
			    error
		    end;
		[LDAPres | _Rest] when is_record(LDAPres, ldapres)->
		    logger:log(debug, "userdb-ldap: Query for exactly one user matching address ~p failed, multiple users found",
			       [Address]),
		    error;
		Unknown ->
		    logger:log(debug, "userdb-ldap: Query for exactly one user matching address ~p failed, ldapsearch returned : ~p",
			       [Address, Unknown]),
		    error
	    end
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
get_users_for_address_of_record(Address) when is_list(Address) ->
    get_users_for_addresses_of_record([Address]).

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
get_users_for_addresses_of_record(AddressList) when is_list(AddressList) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    nomatch;
	Server ->
	    UserAttribute = sipserver:get_env(ldap_userattribute, "sipAuthenticationUser"),
	    AddressAttribute = sipserver:get_env(ldap_addressattribute, "sipLocalAddress"),
	    case get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, AddressList) of
		Res when is_list(Res) ->
		    SortedRes = lists:usort(Res),
		    logger:log(debug, "userdb-ldap: Found user(s) ~p for address(es) ~p", [SortedRes, AddressList]),
		    SortedRes;
		Res ->
		    Res
	    end
    end.

%% Internal function - part of get_users_for_addresses_of_record/1.
get_users_for_AOR_list(_Server, _UserAttribute, _AddressAttribute, []) ->
    [];
get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, [Address | Rest]) when is_list(Address) ->
    Res = directory:ldapsearch_simple(Server, AddressAttribute, Address, UserAttribute),
    case Res of
	error ->
	    error;
	none ->
	    get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, Rest);
	Users when is_list(Users) ->
	    lists:append(Users, get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, Rest))
    end.

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
get_addresses_for_user(User) when is_list(User) ->
    get_addresses_for_users([User]).

%%--------------------------------------------------------------------
%% Function: get_addresses_for_users(UserList)
%%           UserList = list() of string(), usernames
%% Descrip.: Iterate over a list of users, return all their
%%           addresses without duplicates.
%% Returns : Addresses |
%%           nomatch   |
%%           error
%%           Addresses = list() of string()
%%--------------------------------------------------------------------
get_addresses_for_users(UserList) when is_list(UserList) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    nomatch;
	Server ->
	    UserAttribute = sipserver:get_env(ldap_userattribute, "sipAuthenticationUser"),
	    AddressAttribute = sipserver:get_env(ldap_addressattribute, "sipLocalAddress"),
	    case get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, UserList) of
		Res when is_list(Res) ->
		    SortedRes = lists:usort(Res),
		    logger:log(debug, "userdb-ldap: Found address(es) ~p for user(s) ~p", [SortedRes, UserList]),
		    SortedRes;
		error ->
		    error
	    end
    end.

%% internal function - part of get_addresses_for_users/1
get_addresses_for_users_list(_Server, _UserAttribute, _AddressAttribute, []) ->
    [];
get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, [User | Rest]) ->
    Res = directory:ldapsearch_simple(Server, UserAttribute, User, AddressAttribute),
    case Res of
	error ->
	    error;
	none ->
	    get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, Rest);
	Addresses when is_list(Addresses) ->
	    lists:append(Addresses, get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, Rest))
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
    Addresses = local:lookup_url_to_addresses(sipuserdb_ldap, URL),
    logger:log(debug, "userdb-ldap: Looking for users matching address(es) ~p derived from URL ~p",
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
get_password_for_user(User) when is_list(User) ->
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
		[Password] when is_list(Password) ->
		    Password
	    end
    end.

%%--------------------------------------------------------------------
%% Function: get_classes_for_user(Username)
%%           Username = string()
%% Descrip.: Should return a list of classes allowed for a user. This
%%           is not yet implemented in the LDAP userdb. XXX FIX. The
%%           reason it isn't implemented is because we (SU) haven't
%%           decided on how to represent classes in the LDAP schema.
%% Returns : []
%%--------------------------------------------------------------------
get_classes_for_user(_User) ->
    logger:log(debug, "userdb-ldap: Classes are not yet implemented in the LDAP userdb module"),
    [].

%%--------------------------------------------------------------------
%% Function: get_telephonenumber_for_user(User)
%%           User = string()
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
		[Number] when is_list(Number) ->
		    Number
	    end
    end.

%%--------------------------------------------------------------------
%% Function: get_forwards_for_users(Usernames)
%%           Usernames = list() of string()
%% Descrip.: Should return a list of forwards for a list of users.
%%           This is not yet implemented in the LDAP userdb. XXX FIX.
%%           The reason it isn't implemented is because we (SU)
%%           haven't decided on how to represent forwards in the LDAP
%%           schema. XXX we haven't? Isn't this "sipRoutingAddress"?
%% Returns : nomatch
%%--------------------------------------------------------------------
get_forwards_for_users(_Usernames) ->
    logger:log(debug, "userdb-ldap: Forwards are not yet implemented in the LDAP userdb module"),
    nomatch.

%%--------------------------------------------------------------------
%% Function: get_forward_for_user(Username)
%%           Username = string()
%% Descrip.: Should return the forward address for a user.
%%           This is not yet implemented in the LDAP userdb. XXX FIX.
%%           The reason it isn't implemented is because we (SU)
%%           haven't decided on how to represent forwards in the LDAP
%%           schema. XXX we haven't? Isn't this "sipRoutingAddress"?
%% Returns : nomatch
%%--------------------------------------------------------------------
get_forward_for_user(_User) ->
    logger:log(debug, "userdb-ldap: Forwards are not yet implemented in the LDAP userdb module"),
    nomatch.

