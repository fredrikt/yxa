%%
%%--------------------------------------------------------------------

-module(phone).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 init/0,
	 create/0,
	 create/1,
	 insert_phone/5,
	 get_phone/1,
	 list_phones/0,
	 get_user/1,
	 insert_user/4,
	 list_users/0,
	 insert_purge_phone/5,
	 insert_purge_class_phone/5,
	 purge_class_phone/2,
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
	 get_phone_with_requri/1
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("phone.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init()
%% Descrip.: start mnesia and use local node as disc node
%% Returns :
%%--------------------------------------------------------------------
init() ->
    mnesia:create_schema([node()]),
    mnesia:start().

%%--------------------------------------------------------------------
%% Function: create()
%%           create(Servers)
%% Descrip.: Put phone, user and numbers tables as disc_copies on
%%           Servers
%% Returns :
%%--------------------------------------------------------------------
create() ->
    create(servers()).

create(Servers) ->
    mnesia:create_table(phone, [{attributes, record_info(fields, phone)},
				{disc_copies, Servers},
				{type, bag}]),
    mnesia:create_table(user, [{attributes, record_info(fields, user)},
			       {disc_copies, Servers}]),
    mnesia:create_table(numbers, [{attributes, record_info(fields, numbers)},
				  {disc_copies, Servers},
				  {index, [number]},
				  {type, bag}]).

servers() ->
    sipserver:get_env(databaseservers).

%%--------------------------------------------------------------------
%% Function: insert_phone/5
%% Descrip.: Inserts an entry for a number (SIP user) in the location
%%           database.
%% Returns : The result of the mnesia:transaction()
%%--------------------------------------------------------------------
insert_phone(SipUser, Flags, Class, Expire, Address) when is_record(Address, sipurl) ->
    %% We store locations as strings in the location database, since any
    %% datastructure could have to be changed in the future
    LocationStr = sipurl:print(Address),
    %% URIstr is not a valid location for this user, it is used to find the user of
    %% a location.
    URIstr = url_to_requristr(Address),
    db_util:insert_record(#phone{number = SipUser, flags = Flags, class = Class,
			 expire = Expire, address = LocationStr,
			 requristr = URIstr}).

%%--------------------------------------------------------------------
%% Function: insert_purge_phone/5
%% Descrip.: Removes all entrys matching on the number (SIP user),
%%           class and address, then inserts a new entry in the
%%           location database.
%% Returns : The result of the mnesia:transaction()
%%--------------------------------------------------------------------
insert_purge_phone(SipUser, Flags, Class, Expire, Address) when is_record(Address, sipurl) ->
    %% We store locations as strings in the location database, since any
    %% datastructure could have to be changed in the future
    LocationStr = sipurl:print(Address),
    %% URIstr is not a valid location for this user, it is used to find the user of
    %% a location.
    URIstr = url_to_requristr(Address),
    Fun = fun() ->
		  A = mnesia:match_object(#phone{number = SipUser,
						 class = Class,
						 address = LocationStr,
						 _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A),
		  mnesia:write(#phone{number = SipUser, flags = Flags,
				      class = Class,
				      expire = Expire, address = LocationStr,
				      requristr = URIstr})
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% Function: insert_purge_class_phone/5
%% Descrip.: Removes all entrys matching on the number (SIP user),
%%           and class, then inserts a new entry in the location
%%           database.
%% Returns : The result of the mnesia:transaction()
%%--------------------------------------------------------------------
insert_purge_class_phone(Number, Flags, Class, Expire, Address) when is_record(Address, sipurl) ->
    %% We store locations as strings in the location database, since any
    %% datastructure could have to be changed in the future
    LocationStr = sipurl:print(Address),
    %% URIstr is not a valid location for this user, it is used to find the user of
    %% a location.
    URIstr = url_to_requristr(Address),
    Fun = fun() ->
		  A = mnesia:match_object(#phone{number = Number,
						 class = Class,
						 _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A),
		  mnesia:write(#phone{number = Number, flags = Flags,
				      class = Class,
				      expire = Expire, address = LocationStr,
				      requristr = URIstr})
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% Function: purge_class_phone/2
%% Descrip.: Removes all entrys matching a number (SIP user),
%%           and class from the location database.
%% Returns : The result of the mnesia:transaction()
%%--------------------------------------------------------------------
purge_class_phone(Number, Class) ->
    Fun = fun() ->
		  A = mnesia:match_object(#phone{number = Number,
						 class = Class,
						 _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
insert_user(User, Password, Flags, Classes) ->
    db_util:insert_record(#user{user = User, password = Password,
			flags = Flags, classes = Classes}).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
insert_user_or_password(User, Password) ->
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
%% Function: list_phones/0
%% Descrip.: Fetches all locations stored in the location database.
%% Returns : The result of the mnesia:transaction()
%%--------------------------------------------------------------------
list_phones() ->
    db_util:tab_to_list(phone).

list_users() ->
    db_util:tab_to_list(user).

list_numbers() ->
    db_util:tab_to_list(numbers).

%%--------------------------------------------------------------------
%% Function: get_phones/1
%% Descrip.: Fetches all locations for a given number (SIP user)
%%           from the location database.
%% Returns : {atomic, Entrys} |
%%           the result of the mnesia:transaction()
%%--------------------------------------------------------------------
get_phone(SipUser) ->
    Now = util:timestamp(),
    F = fun() ->
		L = mnesia:select(phone, [{#phone{number = SipUser, _ = '_'},
					   [{'>', {element, #phone.expire, '$_'}, Now}],
					   [{{{element, #phone.address, '$_'},
					      {element, #phone.flags, '$_'},
					      {element, #phone.class, '$_'},
					      {element, #phone.expire, '$_'}
					     }}]
					  }])
	end,
    case mnesia:transaction(F) of
	{atomic, L} ->
	    %% Convert the location strings stored in the database to sipurl record format.
	    %% We can't store the locations as sipurl records in the database in case we
	    %% want have to change something in the sipurl record.
	    Rewrite = fun(Entry) ->
			      {LocationStr, Flags, Class, Expire} = Entry,
                              case LocationStr of
                                  {User, Pass, Host, Port, Parameters} ->
				      %% 2004-04-14, ft@ : This is COMPABILITY code to handle old entrys just enough to
				      %% delete them when they expire. Will be removed.
                                      U = #sipurl{proto="sip", user=User, pass=Pass, host=Host,
						  port=Port, param=Parameters},
                                      {U, Flags, Class, Expire};
				  _ ->
				      {sipurl:parse(LocationStr), Flags, Class, Expire}
			      end
		      end,
	    {atomic, lists:map(Rewrite, L)};
       Unknown ->
	    logger:log(error, "Location: get_phone got unknown result from Mnesia : ~p", [Unknown]),
	    Unknown
    end.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
get_user(User) ->
    F = fun() ->
		[ {E#user.password, E#user.flags, E#user.classes} ||
		    E <- mnesia:read({user, User}) ]
	end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
get_numbers_for_user(User) ->
    F = fun() ->
		[ E#numbers.number || E <- mnesia:read({numbers, User}) ]
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
get_users_for_number(Number) ->
    F = fun() ->
		[ E#numbers.user || E <- mnesia:match_object(#numbers{number = Number,
								      _ = '_'}) ]
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function: get_phone_with_requri/1
%% Descrip.: Find an entry with requristr matching the URI, and
%%           return the number (SIP user) of that entry.
%% Returns : {atomic, User} | result of mnesia:transaction()
%%--------------------------------------------------------------------
get_phone_with_requri(URI) when is_record(URI, sipurl) ->
    ReqURIstr = url_to_requristr(URI),
    F = fun() ->
		[ E#phone.number || E <- mnesia:match_object(#phone{requristr = ReqURIstr,
								    _ = '_'}) ]
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
set_user_password(User, Password) ->
    F = fun() ->
		L = mnesia:read({user, User}),
		Update = fun(Obj) ->
				 mnesia:write(Obj#user{password = Password})
			 end,
		lists:foreach(Update, L)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
set_user_flags(User, Flags) ->
    F = fun() ->
		A = mnessia:read({user, User}),
		Update = fun(Obj) ->
				 mnesia:write(Obj#user{flags = Flags})
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
set_user_numbers(User, Numbers) ->
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
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
set_user_classes(User, Classes) ->
    F = fun() ->
		A = mnesia:read({user, User}),
		Update = fun(Obj) ->
				 mnesia:write(Obj#user{classes = Classes})
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function: expired_phones/0
%% Descrip.: Finds all expired entrys from the location database.
%% Returns : {atomic, Entrys} |
%%           the result of the Mnesia transaction
%%--------------------------------------------------------------------
expired_phones() ->
    Now = util:timestamp(),

    F = fun() ->
		L = mnesia:select(phone, [{#phone{_ = '_'},
					   ['<', {element, #phone.expire, '$_'}, Now],
					   [{{{element, #phone.number, '$_'},
					      {element, #phone.address, '$_'},
					      {element, #phone.class, '$_'}
					     }}]
					  }])
	end,

    case mnesia:transaction(F) of
	{atomic, L} ->
	    %% Convert the location strings stored in the database to sipurl record format.
	    %% We can't store the locations as sipurl records in the database in case we
	    %% want have to change something in the sipurl record.
	    Rewrite = fun(Entry) ->
			      {SipUser, LocationStr, Class} = Entry,
			      case LocationStr of
				  {User, Pass, Host, Port, Parameters} ->
				      %% 2004-04-14, ft@ : This is COMPABILITY code to handle old entrys just enough to
                                      %% delete them when they expire. Will be removed.
				      U = #sipurl{proto="sip", user=User, pass=Pass, host=Host, port=Port,
						  param=Parameters},
				      {SipUser, U, Class};
				  _ ->
				      {SipUser, sipurl:parse(LocationStr), Class}
			      end
		      end,
	    {atomic, lists:map(Rewrite, L)};
	Unknown ->
	    logger:log(error, "Location: expired_phones got unknown result from Mnesia : ~p", [Unknown]),
	    Unknown
    end.

%%--------------------------------------------------------------------
%% Function: delete_phone/3
%% Descrip.: Removes all entrys matching a number (SIP user), class
%%           and address from the location database.
%% Returns : The result of the Mnesia transaction
%%--------------------------------------------------------------------
delete_phone(SipUser, Class, Address) when is_record(Address, sipurl) ->
    AddressStr = sipurl:print(Address),
    %% 2004-04-14, ft@ : This is COMPABILITY code to flush out records of old format
    %% from the location database. Will be removed.
    CompatForm = {Address#sipurl.user, Address#sipurl.pass, Address#sipurl.host, Address#sipurl.port,
		  Address#sipurl.param},
    Fun = fun() ->
		  A1 = mnesia:match_object(#phone{number = SipUser,
						 class = Class,
						 address = AddressStr,
						 _ = '_'}),
		  %% 2004-04-14, ft@ : This is COMPABILITY code to flush out records of old format
		  %% from the location database. Will be removed.
		  A2 = mnesia:match_object(#phone{number = SipUser,
						 class = Class,
						 address = CompatForm,
						 _ = '_'}),
		  A = A1 ++ A2,
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
delete_record(Obj) ->
    db_util:delete_record(Obj).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
delete_user(User) ->
    delete_with_key(user, User).

delete_with_key(Db, Key) ->
    F = fun() ->
		mnesia:delete({Db, Key})
	end,
    Rec = mnesia:transaction(F).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: url_to_requristr/1
%% Descrip.: We need to store the location as a searchable string
%%           in order to be able to look up a location to a user.
%%           This string is not used to look up the location of a
%%           user, but rather to see if we are to relay requests
%%           to a destination without challenging and so on.
%% Returns : URLstring
%%--------------------------------------------------------------------
url_to_requristr(URL) when is_record(URL, sipurl) ->
    Port = siprequest:default_port(URL#sipurl.proto, URL#sipurl.port),
    Host = sipurl:cleanup_hostname(URL#sipurl.host),
    User = sipurl:unescape_user(URL#sipurl.user),
    sipurl:print(#sipurl{proto="sip", user=User, pass=none, host=Host, port=Port, param=[]}).
