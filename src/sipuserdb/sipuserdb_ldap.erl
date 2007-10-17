%%%-------------------------------------------------------------------
%%% File    : sipuserdb_ldap.erl
%%% @author   Fredrik Thulin
%%% @doc      A sipuserdb module with an LDAP backend.
%%%
%%% @since    09 Oct 2003 by Fredrik Thulin <ft@it.su.se>
%%% @end
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
    case yxa_config:get_env(ldap_server) of
	{ok, Server} ->
	    {ok, UserAttribute} = yxa_config:get_env(ldap_userattribute),
	    {ok, AddressAttribute} = yxa_config:get_env(ldap_addressattribute),
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
	    end;
	none ->
	    nomatch
    end.

%%--------------------------------------------------------------------
%% @spec    (Address) ->
%%            Users   |
%%            nomatch |
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
get_users_for_address_of_record(Address) when is_list(Address) ->
    get_users_for_addresses_of_record([Address]).

%%--------------------------------------------------------------------
%% @spec    (Addresses) ->
%%            Users   |
%%            nomatch |
%%            error
%%
%%            Addresses = [string()] "addresses in string format."
%%
%%            Users = [string()]
%%
%% @doc     Iterate over a list of addresses of record, return all
%%          users matching one or more of the addresses, without
%%          duplicates.
%% @end
%%--------------------------------------------------------------------
get_users_for_addresses_of_record(AddressList) when is_list(AddressList) ->
    case yxa_config:get_env(ldap_server) of
	{ok, Server} ->
	    {ok, UserAttribute} = yxa_config:get_env(ldap_userattribute),
	    {ok, AddressAttribute} = yxa_config:get_env(ldap_addressattribute),
	    case get_users_for_AOR_list(Server, UserAttribute, AddressAttribute, AddressList) of
		Res when is_list(Res) ->
		    SortedRes = lists:usort(Res),
		    logger:log(debug, "userdb-ldap: Found user(s) ~p for address(es) ~p", [SortedRes, AddressList]),
		    SortedRes;
		Res ->
		    Res
	    end;
	none ->
	    nomatch
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
%% @spec    (User) ->
%%            Addresses |
%%            error
%%
%%            User = string()
%%
%%            Addresses = [string()]
%%
%% @doc     Get all possible addresses of a user. Both configured
%%          ones, and implicit ones. Used for example to check if a
%%          request from a user has an acceptable From: header.
%% @end
%%--------------------------------------------------------------------
get_addresses_for_user(User) when is_list(User) ->
    get_addresses_for_users([User]).

%%--------------------------------------------------------------------
%% @spec    (UserList) ->
%%            Addresses |
%%            nomatch   |
%%            error
%%
%%            UserList = [string()] "usernames"
%%
%%            Addresses = [string()]
%%
%% @doc     Iterate over a list of users, return all their addresses
%%          without duplicates.
%% @end
%%--------------------------------------------------------------------
get_addresses_for_users(UserList) when is_list(UserList) ->
    case yxa_config:get_env(ldap_server) of
        {ok, Server} ->
	    {ok, UserAttribute} = yxa_config:get_env(ldap_userattribute),
	    {ok, AddressAttribute} = yxa_config:get_env(ldap_addressattribute),
	    case get_addresses_for_users_list(Server, UserAttribute, AddressAttribute, UserList) of
		Res when is_list(Res) ->
		    SortedRes = lists:usort(Res),
		    logger:log(debug, "userdb-ldap: Found address(es) ~p for user(s) ~p", [SortedRes, UserList]),
		    SortedRes;
		error ->
		    error
	    end;
	none ->
	    nomatch
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
    Addresses = local:lookup_url_to_addresses(sipuserdb_ldap, URL),
    logger:log(debug, "userdb-ldap: Looking for users matching address(es) ~p derived from URL ~p",
	       [Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
get_password_for_user(User) when is_list(User) ->
    case yxa_config:get_env(ldap_server) of
        {ok, Server} ->
	    {ok, UserAttribute} = yxa_config:get_env(ldap_userattribute),
	    {ok, PasswordAttribute} = yxa_config:get_env(ldap_passwordattribute),
	    case directory:ldapsearch_simple(Server, UserAttribute, User, PasswordAttribute) of
		error ->
		    error;
		none ->
		    logger:log(debug, "userdb-ldap: Found no password for user ~p", [User]),
		    nomatch;
		[Password] when is_list(Password) ->
		    Password
	    end;
	none ->
	    nomatch
    end.

%%--------------------------------------------------------------------
%% @spec    (Username) -> []
%%
%%            Username = string()
%%
%% @doc     Should return a list of classes allowed for a user. This
%%          is not yet implemented in the LDAP userdb. XXX FIX. The
%%          reason it isn't implemented is because we (SU) haven't
%%          decided on how to represent classes in the LDAP schema.
%% @end
%%--------------------------------------------------------------------
get_classes_for_user(_User) ->
    logger:log(debug, "userdb-ldap: Classes are not yet implemented in the LDAP userdb module"),
    [].

%%--------------------------------------------------------------------
%% @spec    (User) ->
%%            Number  |
%%            nomatch |
%%            error
%%
%%            User = string()
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
get_telephonenumber_for_user(User) ->
    case yxa_config:get_env(ldap_server) of
        {ok, Server} ->
	    {ok, UserAttribute} = yxa_config:get_env(ldap_userattribute),
	    {ok, TelephoneNumberAttribute} = yxa_config:get_env(ldap_telephonenumberattribute, "telephoneNumber"),
	    case directory:ldapsearch_simple(Server, UserAttribute, User, TelephoneNumberAttribute) of
		error ->
		    error;
		none ->
		    logger:log(debug, "userdb-ldap: Found no telephone number for user ~p", [User]),
		    nomatch;
		[Number] when is_list(Number) ->
		    Number
	    end;
	none ->
	    nomatch
    end.

%%--------------------------------------------------------------------
%% @spec    (Usernames) -> nomatch
%%
%%            Usernames = [string()]
%%
%% @doc     Should return a list of forwards for a list of users. This
%%          is not yet implemented in the LDAP userdb. XXX FIX. The
%%          reason it isn't implemented is because we (SU) haven't
%%          decided on how to represent forwards in the LDAP schema.
%%          XXX we haven't? Isn't this "sipRoutingAddress"?
%% @end
%%--------------------------------------------------------------------
get_forwards_for_users(_Usernames) ->
    logger:log(debug, "userdb-ldap: Forwards are not yet implemented in the LDAP userdb module"),
    nomatch.

%%--------------------------------------------------------------------
%% @spec    (Username) -> nomatch
%%
%%            Username = string()
%%
%% @doc     Should return the forward address for a user. This is not
%%          yet implemented in the LDAP userdb. XXX FIX. The reason
%%          it isn't implemented is because we (SU) haven't decided
%%          on how to represent forwards in the LDAP schema. XXX we
%%          haven't? Isn't this "sipRoutingAddress"?
%% @end
%%--------------------------------------------------------------------
get_forward_for_user(_User) ->
    logger:log(debug, "userdb-ldap: Forwards are not yet implemented in the LDAP userdb module"),
    nomatch.

