-module(sipuserdb_mysql).
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
	 get_forward_for_user/1,
	 get_kthid_for_user/1,
	 set_password_for_user/2,
	 shellget/1
	]).

-include("siprecords.hrl").

%% Mysql userdb module

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
    mysql:start(sipserver:get_env(mysql_host),
                sipserver:get_env(mysql_user),
                sipserver:get_env(mysql_password),
                sipserver:get_env(mysql_database)),
    crypto:start(),
    [].

%% Looks up exactly one user with an Address. Used
%% for example in REGISTER. If there are multiple
%% users with an address, this function returns 'error'.
get_user_with_address(Address) ->
    case mysql:fetch("select kthid from usernames where username = " ++ mysql:quote(Address)) of
	{_, []} ->
	    case mysql:fetch("select kthid from numbers where number = " ++ mysql:quote(Address)) of
		{_, []} ->
		    logger:log(debug, "userdb-mysql: No user with name or number ~p", [Address]),
		    nomatch;
		{_, [[User]]} ->
		    User;
		{error, Message} ->
		    logger:log(error, "userdb-mysql: Error for address ~p: ~p", [Address, Message]),
		    error;
		{_, Users} ->
		    logger:log(debug, "userdb-mysql: More than one user with number ~p (~p)", [Address, lists:append(Users)]),
		    error
	    end;
	{_, [[User]]} ->
	    User;
	{error, Message} ->
	    logger:log(error, "userdb-mysql: Error for address ~p: ~p", [Address, Message]),
	    error;
	{_, Users} ->
	    logger:log(debug, "userdb-mysql: More than one user with username ~p (~p)", [Address, lists:append(Users)]),
	    error
    end.

sqlwhere(Field, []) ->
    "0";
sqlwhere(Field, [Value]) ->
    Field ++ " = " ++ mysql:quote(Value);
sqlwhere(Field, [Value | Rest]) ->
    Field ++ " = " ++ mysql:quote(Value) ++ " or " ++ sqlwhere(Field, Rest).

%% Looks up all users with a given address. Used
%% to find out to which users we should send a request.
%% Other userdb types than mnesia might perform multiple lookups.
get_users_for_address_of_record(Address) ->
    get_users_for_addresses_of_record([Address]).

get_users_for_addresses_of_record(AddressList) ->
    AddressListMod =
	lists:foldl(
	  fun (Address, Acc) ->
		  case util:regexp_rewrite(Address,
					   [{"sip:(.*)@test.kth.se", "\\1"}]) of
		      nomatch ->
			  Acc;
		      A ->
			  [A | Acc]
		  end
	  end, [], AddressList),
    case mysql:fetch("select kthid from numbers where " ++ sqlwhere("number", AddressListMod)) of
	{error, Message} ->
	    logger:log(error, "userdb-mysql: Error for numbers ~p: ~p", [AddressListMod, Message]),
	    error;
	{_, Numberlist} ->
	    case mysql:fetch("select kthid from usernames where " ++ sqlwhere("username", AddressListMod)) of
		{error, Message} ->
		    logger:log(error, "userdb-mysql: Error for usernames ~p: ~p", [AddressList, Message]),
		    error;
		{_, Userlist} ->
		    Res = lists:usort(lists:append(lists:append(Numberlist), lists:append(Userlist))),
		    logger:log(debug, "userdb-mysql: Found user(s) ~p for address(es) ~p", [Res, AddressList]),
		    Res
	    end
    end.

get_kthid_for_user(User) ->
    case mysql:fetch("select kthid from usernames where username = " ++ mysql:quote(User)) of
	{_, []} ->
	    logger:log(debug, "userdb-mysql: No such user ~p", [User]),
	    nomatch;
	{_, [[Kthid]]} ->
	    Kthid;
	{error, Message} ->
	    logger:log(error, "userdb-mysql: Error for username ~p: ~p", [User, Message]),
	    error
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
    case get_kthid_for_user(User) of
	nomatch ->
	    nomatch;
	error ->
	    error;
	Kthid ->
	    case mysql:fetch("select number from numbers where kthid = " ++ mysql:quote(Kthid)) of
		{_, []} ->
		    logger:log(debug, "userdb-mysql: No numbers for user ~p", [User]),
		    [local:canonify_user(User)];
		{error, Message} ->
		    logger:log(error, "userdb-mysql: Error for username ~p: ~p", [User, Message]),
		    error;
		{_, Numbers} ->
		    logger:log(debug, "userdb-mysql: Found number(s) ~p for user ~p",
			       [lists:append(Numbers), User]),
		    CanonL = [local:canonify_user(User)],
		    NumberL = local:canonify_numberlist(lists:append(Numbers)),
		    All = lists:append([CanonL, NumberL]),
		    lists:usort(All)
	    end
    end.


%% Function: get_users_for_url/1
%% Description: Given an URL that is typically the Request-URI of an
%%              incoming request, make a list of implicit user
%%              addresses and return a list of all users matching any
%%              of these addresses.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
get_users_for_url(URL) when record(URL, sipurl) ->
    Addresses = local:lookup_url_to_addresses(sipuserdb_mysql, URL),
    logger:log(debug, "userdb-mysql: Looking for users matching address(es) ~p derived from URL ~p",
	       [Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).

pack_password(Password) when binary(Password), size(Password) < 255 ->
    Length = size(Password),
    Padlength = 8 - (((Length + 2) rem 8) - 1),
    Pad = list_to_binary(lists:duplicate(Padlength, 0)),
    <<Length:8,
    Password/binary,
    Pad/binary>>.

unpack_password(Data) when binary(Data) ->
    <<Length:8,
    Rest/binary>> = Data,
    <<Password:Length/binary-unit:8, Rest2/binary>> = Rest,
    binary_to_list(Password).

decrypt_password(IV, Data) ->
    Key1 = sipserver:get_env(password_des3_key1),
    Key2 = sipserver:get_env(password_des3_key2),
    Key3 = sipserver:get_env(password_des3_key3),
    logger:log(debug, "userdb-mysql: IV ~p Data ~p", [IV, Data]),
    Res = crypto:des3_cbc_decrypt(Key1, Key2, Key3, IV, Data),
    logger:log(debug, "userdb-mysql: Res ~p", [unpack_password(Res)]),
    unpack_password(Res).

encrypt_password(IV, Data) ->
    Key1 = sipserver:get_env(password_des3_key1),
    Key2 = sipserver:get_env(password_des3_key2),
    Key3 = sipserver:get_env(password_des3_key3),
    logger:log(debug, "userdb-mysql: IV ~p Data ~p", [IV, Data]),
    Res = binary_to_list(crypto:des3_cbc_encrypt(Key1, Key2, Key3, IV,
						 pack_password(list_to_binary(Data)))),
    logger:log(debug, "userdb-mysql: Res ~p", [Res]),
    Res.

% Salt with kthid

calc_password_hmac(Data, Kthid) ->
    Key = sipserver:get_env(password_shamac_key),
    logger:log(debug, "userdb-mysql: Key ~p Data ~p Kthid ~p", [Key, Data, Kthid]),
    Res = binary_to_list(crypto:sha_mac(Key, Kthid ++ ":" ++ Data)),
    logger:log(debug, "userdb-mysql: Res ~p", [Res]),
    Res.

check_password_hmac(Mac, Data, Kthid) ->
    case calc_password_hmac(Data, Kthid) of
	Mac ->
	    true;
	_ ->
	    false
    end.

set_password_for_user(User, Password) ->
    IV = shellget("openssl rand 8"),
    logger:log(debug, "userdb-mysql: IV: ~p", [IV]),
    Data = encrypt_password(IV, Password),
    case mysql:fetch("insert into password (kthid,iv,mac,data) values (" ++
		     mysql:quote(User) ++ "," ++
		     mysql:quote(binary_to_list(IV)) ++ "," ++
		     mysql:quote(calc_password_hmac(Data, User)) ++ "," ++
		     mysql:quote(Data) ++ ")") of
	{error, Message} ->
	    logger:log(error, "userdb-mysql: Error for kthid ~p: ~p", [User, Message]),
	    error;
	{_, Message} ->
	    logger:log(debug, "userdb-mysql: set_password_for_user: ~p", [Message]),
	    nomatch
    end.    


shellget(Command) ->
    Port = open_port({spawn, Command}, [stream, binary, eof]),
    shellgetloop(Port, <<>>).

shellgetloop(Port, Acc) ->
    receive
	{Port, {data, Data}} ->
	    shellgetloop(Port, <<Data/binary, Acc/binary>>);
	{Port, eof} ->
	    Acc
    end.
    


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_password_for_user(User) ->
%    sipuserdb_mysql:set_password_for_user("u1lhi9k2", "foobar"),
    case mysql:fetch("select iv,mac,data from password where kthid = " ++ mysql:quote(User)) of
	{error, Message} ->
	    logger:log(error, "userdb-mysql: Error for kthid ~p: ~p", [User, Message]),
	    error;
	{_, []} ->
	    logger:log(debug, "userdb-mysql: No such kthid ~p when fetching password", [User]),
	    nomatch;
	{_, [[IV, Mac, Data], _]} ->
	    Password = decrypt_password(IV, Data),
	    case check_password_hmac(Mac, Data, User) of
		true ->
		    logger:log(debug, "userdb-mysql: pwd: ~p", [Password]),
		    Password;
		_ ->
		    nomatch
	    end
    end.

get_classes_for_user(User) ->
    case mysql:fetch("select class from classes where kthid = " ++ mysql:quote(User)) of
	{error, Message} ->
	    logger:log(error, "userdb-mysql: Error for username ~p: ~p", [User, Message]),
	    error;
	{_, Classes} ->
	    lists:map(fun ([Class]) ->
			      list_to_atom(Class)
		      end, Classes)
    end.

get_telephonenumber_for_user(User) ->
    case mysql:fetch("select number from numbers where kthid = " ++ mysql:quote(User)) of
	{_, []} ->
	    logger:log(debug, "userdb-mysql: No numbers for user ~p", [User]),
	    nomatch;
	{error, Message} ->
	    logger:log(error, "userdb-mysql: Error for username ~p: ~p", [User, Message]),
	    error;
	{_, [[FirstNumber] | _]} ->
	    FirstNumber
    end.

get_forwards_for_users(In) ->
    nomatch.

get_forward_for_user(User) ->
    nomatch.
