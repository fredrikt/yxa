%%%--------------------------------------------------------------------
%%% File    : phone.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      This function is a mix of location database and Mnesia
%%%           user database functions. It pre-dates the modular
%%%           sipuserdb system (but sipuserdb_mnesia uses these
%%%           functions), and the location database has not yet been
%%%           made modular.
%%%
%%% Note: the #phone.requristr field must always be created with
%%% url_to_requristr/1, otherwise functions that match againts
%%% requristr, like get_sipuser_location_binding/2 will fail.
%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%--------------------------------------------------------------------

-module(phone).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 init/0,
	 create/0,
	 create/1,
	 remove_expired_phones/0,
	 get_sipuser_location_binding/2,
	 get_sipuser_locations/1,
	 get_phone/1,
	 list_phones/0,
	 get_user/1,
	 insert_user/4,
	 list_users/0,
	 insert_purge_phone/8,
	 purge_class_phone/2,
	 delete_location/3,
	 expired_phones/0,
	 delete_record/1,
	 delete_user/1,
	 set_user_password/2,
	 set_user_flags/2,
	 set_user_numbers/2,
	 set_user_classes/2,
	 insert_user_or_password/2,
	 get_numbers_for_user/1,
	 get_users_for_number/1,
	 list_numbers/0,
	 delete_phone/3,
	 delete_phone_for_user/2,
	 get_sipusers_using_contact/1,
	 get_locations_using_contact/1,
	 decode_mnesia_change_event/1,

	 test/0,
	 test_create_table/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("phone.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% @type  transaction_result() = {atomic, Result} | {aborted, Reason}.
%%             The result of a Mnesia transaction. Result is a term().

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term() "result of mnesia:start()"
%%
%% @doc     start mnesia and use local node as disc node
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    mnesia:create_schema([node()]),
    mnesia:start().

%%--------------------------------------------------------------------
%% @spec    () -> term() "result of mnesia:create_table/2."
%%
%% @doc     Invoke create/1 with the list of servers indicated by the
%%          configuration parameter 'databaseservers'.
%% @private
%% @end
%%--------------------------------------------------------------------
create() ->
    {ok, S} = yxa_config:get_env(databaseservers),
    create(S).

%%--------------------------------------------------------------------
%% @spec    (Servers) -> term() "result of mnesia:create_table/2."
%%
%%            Servers = [atom()] "list of nodes"
%%
%% @doc     Put phone, user and numbers tables as disc_copies on
%%          Servers
%% @private
%% @end
%%--------------------------------------------------------------------
create(Servers) ->
    mnesia:create_table(phone, [{attributes, record_info(fields, phone)},
				{disc_copies, Servers},
				{index, [requristr, instance]},
				{type, bag}]),
    mnesia:create_table(user, [{attributes, record_info(fields, user)},
			       {disc_copies, Servers}]),
    mnesia:create_table(numbers, [{attributes, record_info(fields, numbers)},
				  {disc_copies, Servers},
				  {index, [number]},
				  {type, bag}]).

%%--------------------------------------------------------------------
%% @spec    () -> true | error
%%
%% @doc     remove phone entries in DB that should no longer be
%%          present
%% @end
%%--------------------------------------------------------------------
remove_expired_phones() ->
    case expired_phones() of
	{ok, Expired} ->
	    remove_phones(Expired),
	    true;
	{aborted, {no_exists, Table}} ->
	    logger:log(error, "Location: Mnesia says that table '~p' does not exist"
		       " - did you bootstrap YXA? (See README file)", [Table]),
	    error;
	{aborted, Reason} ->
	    logger:log(error, "Location: phone:expired_phones() returned unknown result : ~p", [Reason]),
	    error
    end.


remove_phones(ExpiredPhones) ->
    F = fun({User, Location, Class}) ->
		URL = if
			  is_list(Location) ->
			      sipurl:parse(Location);
			  true ->
			      none
		      end,
		if
		    is_record(URL, sipurl) ->
			logger:log(normal, "Location: User ~p ~p location ~p has expired",
				   [User, Class, sipurl:print(URL)]);
		    true ->
			logger:log(normal, "Location: User ~p ~p (unrecognized) location ~p has expired",
				   [User, Class, Location])
		end,
		delete_phone(User, Location, Class)
	end,
    lists:foreach(F, ExpiredPhones).

%%--------------------------------------------------------------------
%% @spec    (SipUser, Flags, Class, Expire, Address, CallId, CSeq,
%%          Instance) ->
%%            {atomic, term()} "The result of the mnesia:transaction()"
%%
%%            SipUser  = string() "username"
%%            Flags    = [{Name, Value}]
%%            Name     = atom()
%%            Value    =  term() "but typically integer() or string()"
%%            Class    = static | dynamic
%%            Expire   = integer() | never
%%            Address  = #sipurl{}
%%            CallId   = string()
%%            CSeq     = integer()
%%            Instance = string() "Instance ID (or \"\")"
%%
%% @doc     Remove a certain SipUser -> location mapping. Then create
%%          a new entry with a new Expire value
%% @end
%%--------------------------------------------------------------------
insert_purge_phone(SipUser, Flags, Class, Expire, Address, CallId, CSeq, Instance)
  when is_list(SipUser), is_list(Flags), (Class == static orelse Class == dynamic),
       (is_integer(Expire) orelse Expire == never),
       is_record(Address, sipurl), is_list(CallId), is_integer(CSeq), is_list(Instance) ->

    %% We store locations as strings in the location database, since any
    %% datastructure could have to be changed in the future
    LocationStr = sipurl:print(Address),
    %% URIstr is not a valid location for this user, it is used to find the user of
    %% a location, when for example we need to determine if we should proxy a request
    %% without requiring authorization in incomingproxy and appserver.
    URIstr = url_to_requristr(Address),

    This = #phone{user = SipUser,
		  flags = Flags,
		  class = Class,
		  expire = Expire,
		  address = LocationStr,
		  requristr = URIstr,
		  callid = CallId,
		  cseq = CSeq,
		  instance = Instance
		 },

    case {Instance /= [], lists:keysearch(reg_id, 1, Flags)} of
	{true, {value, {reg_id, RegId}}} when is_integer(RegId) ->
	    insert_purge_phone_outbound(Instance, RegId, This);
	_ ->
	    Fun = fun() ->
			  %% query on requristr since that is most likely unique
			  L = mnesia:index_read(phone, URIstr, #phone.requristr),
			  %% apply our other selection criterias as well
			  A = [E || E <- L, E#phone.user == SipUser, E#phone.class == Class],

			  Delete = fun(O) ->
					   mnesia:delete_object(O)
				   end,
			  lists:foreach(Delete, A),
			  mnesia:write(This)
		  end,
	    mnesia:transaction(Fun)
    end.

%% part of insert_purge_phone/8
insert_purge_phone_outbound(Instance, RegId, This) when is_list(Instance), is_integer(RegId), is_record(This, phone) ->
    %% do Outbound registration - meaning we delete any record with the same
    %% SIP user, instance ID and reg-id this one has, not the same contact URI
    %% as we do when not doing Outbound
    Fun = fun() ->
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,

		  %% delete every entry with mathcing user (AOR in the spec), instance id
		  %% and reg_id matching This
		  L2 = mnesia:index_read(phone, Instance, #phone.instance),
		  InvL =
		      lists:foldl(fun(#phone{flags = HF} = H, Acc) ->
					  case H#phone.user == This#phone.user of
					      true ->
						  case lists:member({reg_id, RegId}, HF) of
						      true ->
							  [H | Acc];
						      false ->
							  Acc
						  end;
					      false ->
						  Acc
					  end
				  end, [], L2),
		  lists:foreach(Delete, InvL),

		  mnesia:write(This),

		  {ok, {invalidated, InvL}}
	  end,
    Res = mnesia:transaction(Fun),

    case Res of
	{atomic, {ok, {invalidated, []}}} ->
	    ok;
	{atomic, {ok, {invalidated, L}}} when is_list(L) ->
	    lists:map(fun(IP) when is_record(IP, phone) ->
			      #phone{user = User,
				     address = Address,
				     class = Class
				    } = IP,
			      logger:log(debug, "Location: Registration with Outbound mechanism "
					 "invalidated previously registered contact for user \"~s\" : ~s (~p)",
					 [User, Address, Class])
		      end, L)
    end,

    Res.



%%--------------------------------------------------------------------
%% @spec    (User, Class) -> transaction_result()
%%
%%            User  = string()
%%            Class = atom()
%%
%% @doc     Removes all entrys matching User (SIP user), and class
%%          from the location database.
%% @end
%%--------------------------------------------------------------------
purge_class_phone(User, Class) ->
    Fun = fun() ->
		  A = mnesia:match_object(#phone{user = User,
						 class = Class,
						 _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).


%%--------------------------------------------------------------------
%% @spec    (User, Class, Address) -> transaction_result()
%%
%%            User    = string() "username (phone record 'user' element)"
%%            Class   = static | dynamic
%%            Address = string() "address to remove"
%%
%% @doc     Delete a specific location from the location database.
%% @end
%%--------------------------------------------------------------------
delete_location(User, Class, Address) when is_list(User), (Class == static orelse Class == dynamic),
					   is_list(Address) ->
    Fun = fun() ->
		  A = mnesia:match_object(#phone{user = User,
						 class = Class,
						 address = Address,
						 _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @spec    (User, Password, Flags, Classes) -> transaction_result()
%%
%%            User     = string()
%%            Password = term()
%%            Flags    = [atom()]
%%            Classes  = [atom()]
%%
%% @doc     Create a new user.
%% @end
%%--------------------------------------------------------------------
insert_user(User, Password, Flags, Classes) when is_list(User), is_list(Flags), is_list(Classes) ->
    db_util:insert_record(#user{user = User,
				password = Password,
				flags = Flags,
				classes = Classes
			       }).

%%--------------------------------------------------------------------
%% @spec    (User, Password) -> transaction_result()
%%
%%            User     = string()
%%            Password = term()
%%
%% @doc     Create a user, or set existing user's password.
%% @end
%%--------------------------------------------------------------------
insert_user_or_password(User, Password) when is_list(User) ->
    Fun = fun() ->
		  A = mnesia:read({user, User}),
 		  %% A = mnesia:match_object(#user{user = User,  _ = '_'}),
		  case A of
		      [] ->
			  mnesia:write(#user{user = User, password = Password,
					     flags = [], classes = []});
		      [Entry] ->
			  mnesia:write(Entry#user{password = Password});
		      _ ->
			  mnesia:abort(illegal)
		  end
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @spec    () -> [#phone{}]
%%
%% @doc     Return database contents of the table named 'phone' as a
%%          list. NOTE: this code is mainly intended for debugging.
%% @end
%%--------------------------------------------------------------------
list_phones() ->
    db_util:tab_to_list(phone).

%%--------------------------------------------------------------------
%% @spec    () -> [#user{}]
%%
%% @doc     Return database contents of the table named 'user' as a
%%          list. NOTE: this code is mainly intended for debugging.
%% @end
%%--------------------------------------------------------------------
list_users() ->
    db_util:tab_to_list(user).

%%--------------------------------------------------------------------
%% @spec    () -> [#numbers{}]
%%
%% @doc     Return database contents of the table named 'numbers' as a
%%          list. NOTE: this code is mainly intended for debugging.
%% @end
%%--------------------------------------------------------------------
list_numbers() ->
    db_util:tab_to_list(numbers).

%%--------------------------------------------------------------------
%% @spec    (SipUser, Location) -> transaction_result()
%%
%%            SipUser  = string()
%%            Location = #sipurl{}
%%
%% @doc     Look for a phone record() that maps SipUser to Location.
%%          Returns a list of phone records.
%% @end
%%--------------------------------------------------------------------
get_sipuser_location_binding(SipUser, Location) when is_list(SipUser), is_record(Location, sipurl) ->
    Now = util:timestamp(),
    LocationStr = url_to_requristr(Location),

    %% query on requristr since that is most likely unique
    F = fun() ->
		mnesia:index_read(phone, LocationStr, #phone.requristr)
	end,
    {atomic, L} = mnesia:transaction(F),

    %% remove entrys which are expired, or don't have #phone.user == SipUser
    Res = [E || E <- L, E#phone.expire > Now orelse E#phone.expire == never, E#phone.user == SipUser],

    {atomic, Res}.


%%--------------------------------------------------------------------
%% @spec    (SipUser) ->
%%            {ok, Entries} 
%%
%%            SipUser = string()
%%
%%            Entries = [#siplocationdb_e{}]
%%
%% @doc     Fetches all locations for a given user (SIP user) from the
%%          location database.
%% @end
%%--------------------------------------------------------------------
get_sipuser_locations(SipUser) when is_list(SipUser) ->
    F = fun() ->
		 mnesia:read({phone, SipUser})
	end,
    {atomic, L} = mnesia:transaction(F),

    Rewrite = non_expired_phones_to_ldbe(L),
    {ok, Rewrite}.

%%--------------------------------------------------------------------
%% @spec    (SipUser) -> transaction_result()
%%
%%            SipUser = string()
%%
%% @doc     Fetches all locations for a given user (SIP user) from the
%%          location database. Return raw phone record()s. Note :
%%          Only use this for testing, use get_sipuser_locations/1 in
%%          your applications! (The exception to this rule is
%%          siplocation:process_register_wildcard_isauth/4 which
%%          needs the Call-Id and CSeq information).
%% @end
%%--------------------------------------------------------------------
get_phone(SipUser) when is_list(SipUser) ->
    F = fun() ->
		mnesia:read({phone, SipUser})
	end,
    {atomic, L} = mnesia:transaction(F),

    Now = util:timestamp(),
    Res = [E || E <- L, E#phone.expire > Now orelse E#phone.expire == never],

    {atomic, Res}.

%%--------------------------------------------------------------------
%% @spec    (User) ->
%%            {atomic, Result}
%%
%%            User = string()
%%
%%            Result   = [{Password, Flags, Classes}]
%%            Password = term() "password element of #user{}"
%%            Flags    = [atom()] "flags element of #user{}"
%%            Classes  = [atom()] "classes element of #user{}"
%%
%% @doc     Get password, flags and classes for User.
%% @end
%%--------------------------------------------------------------------
get_user(User) when is_list(User) ->
    F = fun() ->
		[ {E#user.password, E#user.flags, E#user.classes} ||
		    E <- mnesia:read({user, User}) ]
	end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @spec    (User) ->
%%            {atomic, Result}
%%
%%            User = string()
%%
%%            Result = [string()]
%%
%% @doc     Get numbers for User.
%% @end
%%--------------------------------------------------------------------
get_numbers_for_user(User) when is_list(User) ->
    F = fun() ->
		[ E#numbers.number || E <- mnesia:read({numbers, User}) ]
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            {atomic, Users}
%%
%%            Number = string()
%%
%%            Users = [string()] "list of #numbers.user"
%%
%% @doc     Fetch all users having an address matchig Number.
%% @end
%%--------------------------------------------------------------------
get_users_for_number(Number) ->
    F = fun() ->
		[ E#numbers.user || E <- mnesia:index_read(numbers, Number, #numbers.number) ]
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @spec    (URI) ->
%%            {atomic, Users}
%%
%%            URI = #sipurl{}
%%
%%            Users = [string()] "#phone.user values"
%%
%% @doc     Find all users (#phone.user) that have registered a
%%          specific concact (location database binding).
%% @end
%%--------------------------------------------------------------------
get_sipusers_using_contact(URI) when is_record(URI, sipurl) ->
    ReqURIstr = url_to_requristr(URI),
    Now = util:timestamp(),
    F = fun() ->
		L = mnesia:index_read(phone, ReqURIstr, #phone.requristr),
		%% remove expired entrys
		[ E#phone.user || E <- L, E#phone.expire > Now orelse E#phone.expire == never ]
	end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @spec    (URI) ->
%%            {ok, LDBEs}
%%
%%            URI = #sipurl{}
%%
%%            LDBEs = [#siplocationdb_e{}]
%%
%% @doc     Fetch the complete siplocationdb_e records for all
%%          registrations of a specific concact (location database
%%          binding).
%% @end
%%--------------------------------------------------------------------
get_locations_using_contact(URI) when is_record(URI, sipurl) ->
    ReqURIstr = url_to_requristr(URI),
    F = fun() ->
		mnesia:index_read(phone, ReqURIstr, #phone.requristr)
	end,
    {atomic, L} = mnesia:transaction(F),

    Rewrite = non_expired_phones_to_ldbe(L),
    {ok, Rewrite}.

%%--------------------------------------------------------------------
%% @spec    (User, Password) ->
%%            {atomic, ok} |
%%            {aborted, Reason}
%%
%%            User     = string()
%%            Password = term()
%%
%%            Reason = no_such_user | illegal
%%
%% @doc     Set password for User.
%% @end
%%--------------------------------------------------------------------
set_user_password(User, Password) ->
    F = fun() ->
		case mnesia:read({user, User}) of
		    [] ->
			mnesia:abort(no_such_user);
		    L when is_list(L) ->
			Update = fun(Obj) ->
					 mnesia:write(Obj#user{password = Password})
				 end,
			lists:foreach(Update, L);
		    _ ->
			mnesia:abort(illegal)
		end
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @spec    (User, Flags) -> transaction_result()
%%
%%            User  = string()
%%            Flags = [atom()]
%%
%% @doc     Set flags for User.
%% @end
%%--------------------------------------------------------------------
set_user_flags(User, Flags) when is_list(User), is_list(Flags) ->
    F = fun() ->
		case mnesia:read({user, User}) of
		    [] ->
			mnesia:abort(no_such_user);
		    A when is_list(A) ->
			Update = fun(Obj) ->
					 mnesia:write(Obj#user{flags = Flags})
				 end,
			lists:foreach(Update, A);
		    _ ->
			mnesia:abort(illegal)
		end
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @spec    (User, Numbers) -> transaction_result()
%%
%%            User    = string()
%%            Numbers = [string()]
%%
%% @doc     Set numbers for User.
%% @end
%%--------------------------------------------------------------------
set_user_numbers(User, Numbers) when is_list(User), is_list(Numbers) ->
    F = fun() ->
		A = mnesia:read({numbers, User}),
		Delete = fun(Obj) ->
				 mnesia:delete_object(Obj)
			 end,
		lists:foreach(Delete, A),
		Update = fun(Number) ->
				 mnesia:write(#numbers{number = Number,
						       user = User})
			 end,
		lists:foreach(Update, Numbers)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @spec    (User, Classes) -> transaction_result()
%%
%%            User    = string()
%%            Classes = [atom()]
%%
%% @doc     Set classes for User.
%% @end
%%--------------------------------------------------------------------
set_user_classes(User, Classes) when is_list(User), is_list(Classes) ->
    F = fun() ->
		case mnesia:read({user, User}) of
		    [] ->
			mnesia:abort(no_such_user);
		    A when is_list(A) ->
			Update = fun(Obj) ->
					 mnesia:write(Obj#user{classes = Classes})
				 end,
			lists:foreach(Update, A);
		    _ ->
			mnesia:abort(illegal)
		end
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @spec    () ->
%%            {ok, Entries} | transaction_result()
%%
%%            Entries = [{Number, Address, Class}]
%%
%% @doc     Finds all expired entrys from the location database.
%% @end
%%--------------------------------------------------------------------
expired_phones() ->
    Now = util:timestamp(),

    F = fun() ->
		mnesia:select(phone, [{#phone{_ = '_'},
				       [{'<', {element, #phone.expire, '$_'}, Now}],
				       [{{{element, #phone.user, '$_'},
					  {element, #phone.address, '$_'},
					  {element, #phone.class, '$_'}
					 }}]
				      }])
	end,

    case mnesia:transaction(F) of
	{atomic, L} ->
	    {ok, L};
	Unknown ->
	    logger:log(error, "Location: expired_phones got unknown result from Mnesia : ~p", [Unknown]),
	    Unknown
    end.

%%--------------------------------------------------------------------
%% @spec    (SipUser, Address, Class) -> transaction_result()
%%
%%            SipUser = string()
%%            Class   = atom()
%%            Address = term() "should be string"
%%
%% @doc     Removes all entrys matching a user (SIP user), class and
%%          address from the location database. Address can be
%%          whatever - this function should not be depending on being
%%          able to understand Address to remove old records from the
%%          database. Note : used by remove_phones (and
%%          remove_expired_phones) XXX Redundant with
%%          delete_location/3???
%% @end
%%--------------------------------------------------------------------
delete_phone(SipUser, Address, Class) when is_list(SipUser), is_atom(Class) ->
    Fun = fun() ->
		  A = mnesia:match_object(#phone{user = SipUser,
						 class = Class,
						 address = Address,
						 _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @spec    (SipUser, Class) -> transaction_result()
%%
%%            SipUser = string()
%%            Class   = atom()
%%
%% @doc     Removes all registered locations of a specific Class for a
%%          SipUser.
%% @end
%%--------------------------------------------------------------------
delete_phone_for_user(SipUser, Class) when is_list(SipUser), is_atom(Class) ->
    Fun = fun() ->
		  A = mnesia:match_object(#phone{user = SipUser,
						 class = Class,
						 _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @spec    (Obj) -> transaction_result()
%%
%%            Obj = #phone{}
%%
%% @doc     remove a single phone record Obj in a bag table
%% @end
%%--------------------------------------------------------------------
delete_record(Obj) ->
    db_util:delete_record(Obj).

%%--------------------------------------------------------------------
%% @spec    (User) -> transaction_result()
%%
%% @doc     Delete user User and all numbers for that user from the
%%          database.
%% @end
%%--------------------------------------------------------------------
delete_user(User) when is_list(User) ->
    F = fun() ->
		{atomic, ok} = set_user_numbers(User, []),
		{atomic, ok} = cpl_db:rm_cpl_for_user(User),
		{atomic, ok} = delete_with_key(user, User),
		ok
	end,
    mnesia:transaction(F).

delete_with_key(Db, Key) ->
    F = fun() ->
 		mnesia:delete({Db, Key})
 	end,
    Rec = mnesia:transaction(F),
    Rec.

%%--------------------------------------------------------------------
%% @spec    (MnesiaEvent) ->
%%            {ok, Action, User, Locations} |
%%            none
%%
%%            MnesiaEvent = tuple() "Mnesia 'subscribe' event data"
%%
%%            Action    = insert | delete
%%            User      = string()
%%            Locations = [#siploctiondb_e{}]
%%
%% @doc     Return location records from Mnesia write or delete_object
%%          events.
%% @private
%% @end
%%--------------------------------------------------------------------
decode_mnesia_change_event({Type, Data, _TId}) when (Type == write orelse Type == delete_object),
						    is_record(Data, phone) ->
    [Entry] = phones_to_ldbe([Data]),
    Action =
	case Type of
	    write         -> insert;
	    delete_object -> delete
	end,
    {ok, Action, Entry#siplocationdb_e.sipuser, Entry};
decode_mnesia_change_event({_Type, Data, _Tid}) when is_record(Data, phone) ->
    %% not 'write' or 'delete_object' - we don't care
    none;
decode_mnesia_change_event(_Unknown) ->
    error.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (URL) -> string()
%%
%%            URL = #sipurl{}
%%
%% @doc     We need to store the location as a searchable string in
%%          order to be able to look up a location to a user. This
%%          string is not used to look up the location of a user, but
%%          rather to see if we are to relay requests to a
%%          destination without challenging and so on.
%% @end
%%--------------------------------------------------------------------
url_to_requristr(URL) when is_record(URL, sipurl) ->
    Port = sipsocket:default_port(URL#sipurl.proto, sipurl:get_port(URL)),
    Host = URL#sipurl.host,
    User = URL#sipurl.user,
    %% sipurl:print/1 is used to create a consistent and therefore
    %% easily matchable string
    sipurl:print(sipurl:new([{proto, "sip"}, {user, User}, {host, Host}, {port, Port}])).


non_expired_phones_to_ldbe(L) ->
    Now = util:timestamp(),

    %% Remove expired entrys and convert into siplocationdb_e record().
    %% Convert the location strings stored in the database to sipurl record format.
    %% We can't store the locations as sipurl records in the database in case we
    %% have to change something in the sipurl record.
    L1 = [E || E <- L, E#phone.expire > Now orelse E#phone.expire == never],

    phones_to_ldbe(L1).

phones_to_ldbe(L) ->
    [ #siplocationdb_e{address	= sipurl:parse(E#phone.address),
		       sipuser	= E#phone.user,
		       instance	= E#phone.instance,
		       flags	= E#phone.flags,
		       class	= E#phone.class,
		       expire	= E#phone.expire
		      }
      || E <- L].


%%====================================================================
%% Test functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->

    %% test x
    %%--------------------------------------------------------------------
    %%autotest:mark(?LINE, "f/a - 1"),

    ok.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Create a table in RAM only, for use in unit tests.
%% @hidden
%% @end
%%--------------------------------------------------------------------
test_create_table() ->
    case catch mnesia:table_info(phone, attributes) of
	Attrs when is_list(Attrs) ->
	    ok;
	{'EXIT', {aborted, {no_exists, phone, attributes}}}  ->
	    %% Create table 'phone' in RAM for use in the tests here
	    {atomic, ok} =
		mnesia:create_table(phone, [{attributes,	record_info(fields, phone)},
					    {index,		[requristr, instance]},
					    {type,		bag}
					   ]),
	    ok
    end.
