%%%-------------------------------------------------------------------
%%% File    : sipuserdb_mysql.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: MySQL sipuserdb module.
%%%
%%% Created :  4 Aug 2005 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(sipuserdb_mysql).
-compile(export_all).

-behaviour(sipuserdb).

%%--------------------------------------------------------------------
%% External exports - sipuserdb callbacks
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
%% External exports - module specific exported functions
%%--------------------------------------------------------------------
-export([make_sql_statement/2
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").

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
    {ok, [Host, User, Password, Db]} =
	get_mysql_params([sipuserdb_mysql_host,
			  sipuserdb_mysql_user,
			  sipuserdb_mysql_password,
			  sipuserdb_mysql_database
			 ]),
    %% create a logging function that makes the MySQL subsystem logs
    %% end up in the standard Yxa log system.
    LogFun = fun(Level, Fmt, Args) ->
		     logger:log(Level, Fmt, Args)
	     end,
    %% mysql server port is optional
    MysqlArgs =
	case yxa_config:get_env(sipuserdb_mysql_port) of
	    {ok, Port} ->
		[yxa, Host, Port, User, Password, Db, LogFun];
	    none ->
		[yxa, Host, User, Password, Db, LogFun]
	end,
    [{mysql_dispatcher, {mysql, start_link, MysqlArgs},
      permanent, 2000, worker, [mysql_dispatcher]}
    ].

%% part of yxa_init/0
get_mysql_params(L) ->
    get_mysql_params(L, []).

%% part of yxa_init/0
get_mysql_params([H | T], Res) ->
    case yxa_config:get_env(H) of
	{ok, Value} ->
	    get_mysql_params(T, [Value | Res]);
	none ->
	    Msg = lists:concat(["sipuserdb_mysql requires you to set the configuration parameter ",
				H]),
	    %% throw as atom to get printed readably in the startup error message
	    throw(list_to_atom(Msg))
    end;
get_mysql_params([], Res) ->
    {ok, lists:reverse(Res)}.


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
    Query1 = make_sql_statement(sipuserdb_mysql_get_user,
				Address),
    case mysql:fetch(yxa, Query1) of
	{ok, _, []} ->
	    Query2 = make_sql_statement(sipuserdb_mysql_get_user_for_address,
					     Address),
	    case mysql:fetch(yxa, Query2) of
		{ok, _, []} ->
		    logger:log(debug, "userdb-mysql: No user with name or number ~p", [Address]),
		    nomatch;
		{ok, _, [[User]]} ->
		    User;
		{ok, _, Users} ->
		    logger:log(debug, "userdb-mysql: More than one user with number ~p (~p)", [Address, lists:append(Users)]),
		    error;
		{error, Reason} ->
		    logger:log(error, "userdb-mysql: Error for number ~p: ~p", [Address, Reason]),
		    error
	    end;
	{ok, _, [[User]]} ->
	    User;
	{ok, _, Users} ->
	    logger:log(debug, "userdb-mysql: More than one user with username ~p (~p)", [Address, lists:append(Users)]),
	    error;
	{error, Reason} ->
	    logger:log(error, "userdb-mysql: Error for address ~p: ~p", [Address, Reason]),
	    error
    end.


%%--------------------------------------------------------------------
%% Function: get_users_for_address_of_record(Address)
%%           Address = string(), an address in string format.
%% Descrip.: Get all usernames of users matching an address. Used to
%%           find out to which users we should send a request.
%% Returns : Users |
%%           error
%%           Users = list() of string()
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    get_users_for_addresses_of_record([Address]).


%%--------------------------------------------------------------------
%% Function: get_users_for_addresses_of_record(In)
%%           In = list() of string(), addresses in string format.
%% Descrip.: Iterate over a list of addresses of record, return
%%           all users matching one or more of the addresses,
%%           without duplicates.
%% Returns : Users, list() of string()
%%--------------------------------------------------------------------
get_users_for_addresses_of_record(AddressList) ->
    Query1 = make_sql_statement(sipuserdb_mysql_get_user_for_address,
				AddressList),
    case mysql:fetch(yxa, Query1) of
	{ok, _, Users} ->
	    Res = lists:usort(lists:append(Users)),
	    logger:log(debug, "userdb-mysql: Found user(s) ~p for address(es) ~p", [Res, AddressList]),
	    Res;
	{error, Reason} ->
	    logger:log(error, "userdb-mysql: Error for address(es) ~p: ~p", [AddressList, Reason]),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: get_addresses_for_users(In)
%%           In = list() of string(), usernames
%% Descrip.: Iterate over a list of users, return all their
%%           addresses without duplicates by using the next
%%           function, get_addresses_for_user/1.
%% Returns : Addresses, list() of string()
%%--------------------------------------------------------------------
get_addresses_for_users(In) when is_list(In) ->
    Addresses = 
	lists:foldl(fun(User, Acc) ->
			    case get_addresses_for_user(User) of
				nomatch ->
				    Acc;
				A when is_list(A) ->
				    [A | Acc]
			    end
		    end, [], In),
    lists:usort(lists:append(Addresses)).

%%--------------------------------------------------------------------
%% Function: get_addresses_for_user(Username)
%%           Username = string()
%% Descrip.: Get all possible addresses of a user. Both configured
%%           ones, and implicit ones. Used for example to check if a
%%           request from a user has an acceptable From: header.
%% Returns : Addresses |
%%           error
%%           Addresses = list() of string()
%%--------------------------------------------------------------------
get_addresses_for_user(User) ->
    Query1 = make_sql_statement(sipuserdb_mysql_get_addresses_for_user,
				User),
    case mysql:fetch(yxa, Query1) of
	{ok, _, []} ->
	    logger:log(debug, "userdb-mysql: No numbers for user ~p", [User]),
	    %% Check if there is a user with that name, if so we canonify the username
	    Query2 = make_sql_statement(sipuserdb_mysql_get_user,
					User),
	    case mysql:fetch(yxa, Query2) of
		{ok, _, []} ->
		    logger:log(debug, "userdb-mysql: No such user ~p", [User]),
		    nomatch;
		{ok, _, _} ->
		    [local:canonify_user(User)];
		{error, Reason} ->
		    logger:log(error, "userdb-mysql: Error for username ~p: ~p", [User, Reason]),
		    error
	    end;
	{ok, _, Numbers} ->
	    FlatNumbers = lists:append(Numbers),
	    logger:log(debug, "userdb-mysql: Found number(s) ~p for user ~p",
		       [FlatNumbers, User]),
	    CanonL = [local:canonify_user(User)],
	    NumberL = local:canonify_numberlist(FlatNumbers),
	    All = lists:append([CanonL, NumberL]),
	    lists:usort(All);
	{error, Reason} ->
	    logger:log(error, "userdb-mysql: Error for username ~p: ~p", [User, Reason]),
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
    Addresses = local:lookup_url_to_addresses(sipuserdb_mysql, URL),
    logger:log(debug, "userdb-mysql: Looking for users matching address(es) ~p derived from URL ~p",
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
    Query1 = make_sql_statement(sipuserdb_mysql_get_password_for_user,
				User),
    case mysql:fetch(yxa, Query1) of
	{ok, _, []} ->
	    logger:log(debug, "userdb-mysql: No password found for user ~p", [User]),
	    nomatch;
	{ok, _, [Res]} ->
	    lists:append(Res);
	{error, Reason} ->
	    logger:log(error, "userdb-mysql: Error for user ~p: ~p", [User, Reason]),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: get_classes_for_user(Username)
%%           Username = string()
%% Descrip.: Returns a list of classes allowed for a user. Classes
%%           are used by pstnproxy to determine if it should allow
%%           a call to a PSTN number (of a certain class) from a
%%           user or not.
%% Returns : Classes |
%%           nomatch |
%%           error
%%           Classes = list() of atom()
%%--------------------------------------------------------------------
get_classes_for_user(User) ->
    Query1 = make_sql_statement(sipuserdb_mysql_get_classes_for_user,
				User),
    case mysql:fetch(yxa, Query1) of
	{ok, _, Classes} ->
	    lists:map(fun ([Class]) ->
			      list_to_atom(Class)
		      end, Classes);
	{error, Reason} ->
	    logger:log(error, "userdb-mysql: Error for username ~p: ~p", [User, Reason]),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: get_telephonenumber_for_user(Username)
%%           Username = string()
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
    Query1 = make_sql_statement(sipuserdb_mysql_get_telephonenumber_for_user,
				User),
    case mysql:fetch(yxa, Query1) of
	{ok, _, []} ->
	    logger:log(debug, "userdb-mysql: No numbers for user ~p", [User]),
	    nomatch;
	{ok, _, [[FirstNumber] | _]} ->
	    FirstNumber;
	{error, Reason} ->
	    logger:log(error, "userdb-mysql: Error for username ~p: ~p", [User, Reason]),
	    error
    end.

%%--------------------------------------------------------------------
%% Function: get_forwards_for_users(In)
%%           In = list() of string(), list of usernames
%% Descrip.: Return a list of forward addresses for a list of users.
%%           Uses the next function, get_forward_for_user/1.
%% Returns : ForwardList = list() of sipproxy_forward record()
%%--------------------------------------------------------------------
get_forwards_for_users(_In) ->
    nomatch.

%%--------------------------------------------------------------------
%% Function: get_forward_for_user(Username)
%%           Username = string()
%% Descrip.: Return the forward address(es) for a user.
%% Returns : ForwardList |
%%           nomatch     |
%%           error
%%           ForwardList = list() of sipproxy_forward record()
%%--------------------------------------------------------------------
get_forward_for_user(_User) ->
    nomatch.


%%--------------------------------------------------------------------
%% Function: make_sql_statement(CfgKey, Args)
%%           make_sql_statement(Template, Args)
%%           CfgKey   = atom()
%%           Args     = list() of term(), SQL query key(s)
%%           Template = string()
%% Descrip.: Construct an SQL query statement given a configuration
%%           parameter name or a string template. If Args consists of
%%           more than one element, SQL ' or ' will be used.
%% Returns : Query = string()
%%--------------------------------------------------------------------
make_sql_statement(CfgKey, Args) when is_atom(CfgKey) ->
    case local:sipuserdb_mysql_make_sql_statement(CfgKey, Args) of
	{ok, Res} when is_list(Res) ->
	    logger:log(debug, "userdb-mysql: ~p -> ~p (from local)", [CfgKey, Res]),
	    Res;
	undefined ->
	    {ok, Template} = yxa_config:get_env(CfgKey),
	    Res = make_sql_statement(Template, Args),
	    logger:log(debug, "userdb-mysql: ~p -> ~p", [CfgKey, Res]),
	    Res
    end;
make_sql_statement(Template, Args) when is_list(Template), is_list(Args) ->
    make_sql_statement2(Template, Args, []).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: make_sql_statement(Template, Args)
%%           Template = string()
%%           Args     = list() of term(), SQL query key(s)
%% Descrip.: Construct an SQL query statement given a template,
%%           replacing all occurrences of '?' with either Args (if it
%%           is a single-value argument) or an SQL ' or ' list if
%%           Args is multi-value. The latter of course requires that
%%           we can identify the SQL variable name.
%% Returns : Query = string()
%%--------------------------------------------------------------------
make_sql_statement2([$? | T], Args, Res) ->
    %% ok, ? found
    NewRes = make_sql_statement_insert(Args, Res),
    %% keep looking for more ? until T is empty
    make_sql_statement2(T, Args, NewRes);
make_sql_statement2([H | T], Args, Res) ->
    %% non-?
    make_sql_statement2(T, Args, [H | Res]);
make_sql_statement2([], _Args, Res) ->
    lists:reverse(binary_to_list( list_to_binary(Res) )).

make_sql_statement_insert([H | _T] = Args, Res) when is_list(H) ->
    %% Args is a list, make "foo = bar OR foo = baz" statement

    %% get the 'foo' token, since Res is reversed this is at the
    %% beginning of Res
    {ok, SqlVar, Rest} = make_sql_statement_insert_get_sqlvar(Res),
    OrStrR = lists:reverse( sqlwhere(SqlVar, Args) ),
    [OrStrR | Rest];
make_sql_statement_insert(Arg, Res) when is_list(Arg) ->
    %% Arg is a single element
    This = lists:reverse( mysql:quote(Arg) ),
    [This | Res].

make_sql_statement_insert_get_sqlvar([32 | T]) ->	%% 32 is ' '
    %% space before sql-var starts, ignore
    make_sql_statement_insert_get_sqlvar(T);
make_sql_statement_insert_get_sqlvar([61 | T]) ->	%% 61 is '-'
    %% equal-sign before sql-var starts, ignore
    make_sql_statement_insert_get_sqlvar(T);
make_sql_statement_insert_get_sqlvar([H | T]) ->
    %% sql-var starts here
    make_sql_statement_insert_get_sqlvar2(T, [H]);
make_sql_statement_insert_get_sqlvar([]) ->
    throw({error, "invalid SQL-statement, no distinguishable variable name start"}).

make_sql_statement_insert_get_sqlvar2([32 | T], Res) ->
    %% we are finished
    {ok, Res, [32 | T]};
make_sql_statement_insert_get_sqlvar2([H | T], Res) ->
    %% not finished, keep looking
    make_sql_statement_insert_get_sqlvar2(T, [H | Res]);
make_sql_statement_insert_get_sqlvar2([], _Res) ->
    throw({error, "invalid SQL-statement, no distinguishable variable name end"}).

%% part of make_sql_statement_insert
sqlwhere(Field, [Value]) ->
    Field ++ " = " ++ mysql:quote(Value);
sqlwhere(Field, [Value | Rest]) ->
    Field ++ " = " ++ mysql:quote(Value) ++ " or " ++ sqlwhere(Field, Rest).
