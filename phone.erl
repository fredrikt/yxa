-module(phone).
-export([init/0, create/0, create/1, insert_phone/5, get_phone/1, list_phones/0,
	 get_user/1, insert_user/4, list_users/0,
	 insert_purge_phone/5, insert_purge_class_phone/5,
	 purge_class_phone/2, expired_phones/0, delete_record/1,
	 delete_user/1, set_user_password/2, set_user_flags/2,
	 set_user_numbers/2, set_user_classes/2, insert_user_or_password/2,
	 get_numbers_for_user/1, get_users_for_number/1, list_numbers/0,
	 delete_phone/3, get_phone_with_requri/1]).

-include("phone.hrl").
-include("siprecords.hrl").

-include_lib("mnemosyne/include/mnemosyne.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start().

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

insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

%% Function: insert_phone/5
%% Description: Inserts an entry for a number (SIP user) in the
%%              location database.
%% Returns: The result of the mnesia:transaction()
%%--------------------------------------------------------------------
insert_phone(SipUser, Flags, Class, Expire, Address) when record(Address, sipurl) ->
    %% We store locations as strings in the location database, since any
    %% datastructure could have to be changed in the future
    LocationStr = sipurl:print(Address),
    %% URIstr is not a valid location for this user, it is used to find the user of
    %% a location.
    URIstr = url_to_requristr(Address),
    insert_record(#phone{number = SipUser, flags = Flags, class = Class,
			 expire = Expire, address = LocationStr,
			 requristr = URIstr}).

%% Function: insert_purge_phone/5
%% Description: Removes all entrys matching on the number (SIP user),
%%              class and address, then inserts a new entry in the
%%              location database.
%% Returns: The result of the mnesia:transaction()
%%--------------------------------------------------------------------
insert_purge_phone(SipUser, Flags, Class, Expire, Address) when record(Address, sipurl) ->
    %% We store locations as strings in the location database, since any
    %% datastructure could have to be changed in the future
    LocationStr = sipurl:print(Address),
    %% URIstr is not a valid location for this user, it is used to find the user of
    %% a location.
    URIstr = url_to_requristr(Address),
    Fun = fun() ->
		  Q = query
			  [E || E <- table(phone),
				E.number = SipUser,
				E.class = Class,
				E.address = LocationStr]
		      end,
		  A = mnemosyne:eval(Q),
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

%% Function: insert_purge_class_phone/5
%% Description: Removes all entrys matching on the number (SIP user),
%%              and class, then inserts a new entry in the location
%%              database.
%% Returns: The result of the mnesia:transaction()
%%--------------------------------------------------------------------
insert_purge_class_phone(Number, Flags, Class, Expire, Address) when record(Address, sipurl) ->
    %% We store locations as strings in the location database, since any
    %% datastructure could have to be changed in the future
    LocationStr = sipurl:print(Address),
    %% URIstr is not a valid location for this user, it is used to find the user of
    %% a location.
    URIstr = url_to_requristr(Address),
    Fun = fun() ->
		  Q = query
			  [E || E <- table(phone),
				E.number = Number,
				E.class = Class]
		      end,
		  A = mnemosyne:eval(Q),
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

%% Function: purge_class_phone/2
%% Description: Removes all entrys matching a number (SIP user),
%%              and class from the location database.
%% Returns: The result of the mnesia:transaction()
%%--------------------------------------------------------------------
purge_class_phone(Number, Class) ->
    Fun = fun() ->
		  Q = query
			  [E || E <- table(phone),
				E.number = Number,
				E.class = Class]
		      end,
		  A = mnemosyne:eval(Q),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).    

insert_user(User, Password, Flags, Classes) ->
    insert_record(#user{user = User, password = Password,
			flags = Flags, classes = Classes}).

insert_user_or_password(User, Password) ->
    Fun = fun() ->
		  Q = query
			  [E || E <- table(user),
				E.user = User]
		      end,
		  case mnemosyne:eval(Q) of
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

%% Function: list_phones/0
%% Description: Fetches all locations stored in the location database.
%% Returns: The result of the mnesia:transaction()
%%--------------------------------------------------------------------
list_phones() ->
    F = fun() ->
		Q = query
			[E || E <- table(phone)]
		    end,
		mnemosyne:eval(Q)
	end,
    mnesia:transaction(F).

list_users() ->
    F = fun() ->
		Q = query
			[E || E <- table(user)]
		    end,
		mnemosyne:eval(Q)
	end,
    mnesia:transaction(F).

list_numbers() ->
    F = fun() ->
		Q = query
			[E || E <- table(numbers)]
		    end,
		mnemosyne:eval(Q)
	end,
    mnesia:transaction(F).
    
%% Function: get_phones/1
%% Description: Fetches all locations for a given number (SIP user)
%%              from the location database.
%% Returns: {atomic, Entrys} |
%%          the result of the mnesia:transaction()
%%--------------------------------------------------------------------
get_phone(SipUser) ->
    Now = util:timestamp(),
    F = fun() ->
		Q = query
			[{E.address, E.flags, E.class, E.expire} ||
			    E <- table(phone), E.number = SipUser,
			    E.expire > Now]
		    end,
		mnemosyne:eval(Q)
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
                                      U = #sipurl{proto="sip", user=User, pass=Pass, host=Host, port=Port, param=Parameters},
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

get_user(User) ->
    F = fun() ->
		Q = query
			[{E.password, E.flags, E.classes} || E <- table(user),
							     E.user = User]
			end,
		mnemosyne:eval(Q)
	end,
    mnesia:transaction(F).

get_numbers_for_user(User) ->
    F = fun() ->
		Q = query
			[{E.number} || E <- table(numbers),
				       E.user = User]
		    end,
		mnemosyne:eval(Q)
	end,
    Rewrite = fun({Number}) ->
		      Number
	      end,
    case mnesia:transaction(F) of
	{atomic, List} ->
	    {atomic, lists:map(Rewrite, List)};
	Other ->
	    Other
    end.

get_users_for_number(Number) ->
    F = fun() ->
		Q = query
			[{E.user} || E <- table(numbers),
				     E.number = Number]
		    end,
		mnemosyne:eval(Q)
	end,
    Rewrite = fun({User}) ->
		      User
	      end,
    case mnesia:transaction(F) of
	{atomic, List} ->
	    {atomic, lists:map(Rewrite, List)};
	Other ->
	    Other
    end.

%% Function: get_phone_with_requri/1
%% Description: Find an entry with requristr matching the URI, and
%%              return the number (SIP user) of that entry.
%% Returns: {atomic, User} |
%%          the result of the mnesia:transaction()
%%--------------------------------------------------------------------
get_phone_with_requri(URI) when record(URI, sipurl) ->
    ReqURIstr = url_to_requristr(URI),
    F = fun() ->
		Q = query
			[{E.number} || E <- table(phone),
				     E.requristr = ReqURIstr]
		    end,
		mnemosyne:eval(Q)
	end,
    Rewrite = fun({User}) ->
		      User
	      end,
    case mnesia:transaction(F) of
	{atomic, List} ->
	    {atomic, lists:map(Rewrite, List)};
	Other ->
	    Other
    end.

set_user_password(User, Password) ->
    F = fun() ->
		Q = query
			[E || E <- table(user),
			      E.user = User]
		    end,
		A = mnemosyne:eval(Q),
		Update = fun(O) ->
				 mnesia:write(O#user{password = Password})
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

set_user_flags(User, Flags) ->
    F = fun() ->
		Q = query
			[E || E <- table(user),
			      E.user = User]
		    end,
		A = mnemosyne:eval(Q),
		Update = fun(O) ->
				 mnesia:write(O#user{flags = Flags})
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

set_user_numbers(User, Numbers) ->
    F = fun() ->
		Q = query
			[E || E <- table(numbers),
			      E.user = User]
		    end,
		A = mnemosyne:eval(Q),
		Delete = fun(O) ->
				 mnesia:delete_object(O)
			   end,
		lists:foreach(Delete, A),
		Update = fun(Number) ->
				 mnesia:write(#numbers{number = Number,
						       user = User})
			 end,
		lists:foreach(Update, Numbers)
	end,
    mnesia:transaction(F).

set_user_classes(User, Classes) ->
    F = fun() ->
		Q = query
			[E || E <- table(user),
			      E.user = User]
		    end,
		A = mnemosyne:eval(Q),
		Update = fun(O) ->
				 mnesia:write(O#user{classes = Classes})
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

%% Function: expired_phones/0
%% Description: Finds all expired entrys from the location database.
%% Returns: {atomic, Entrys} |
%%          the result of the Mnesia transaction
%%--------------------------------------------------------------------
expired_phones() ->
    Now = util:timestamp(),
    F = fun() ->
		Q = query
			[ {E.number, E.address, E.class} ||
			  E <- table(phone), E.expire < Now]
		    end,
		mnemosyne:eval(Q)
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
				      U = #sipurl{proto="sip", user=User, pass=Pass, host=Host, port=Port, param=Parameters},
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

%% Function: delete_phone/3
%% Description: Removes all entrys matching a number (SIP user), class
%%              and address from the location database.
%% Returns: The result of the Mnesia transaction
%%--------------------------------------------------------------------
delete_phone(SipUser, Class, Address) when record(Address, sipurl) ->
    AddressStr = sipurl:print(Address),
    %% 2004-04-14, ft@ : This is COMPABILITY code to flush out records of old format
    %% from the location database. Will be removed.
    CompatForm = {Address#sipurl.user, Address#sipurl.pass, Address#sipurl.host, Address#sipurl.port, Address#sipurl.param},
    Fun = fun() ->
		  Q = query
			  [E || E <- table(phone),
				E.number = SipUser,
				E.class = Class,
				E.address = AddressStr
				   ]
		      end,
		  %% 2004-04-14, ft@ : This is COMPABILITY code to flush out records of old format
		  %% from the location database. Will be removed.
                  Q2 = query
			   [E || E <- table(phone),
				 E.number = SipUser,
				 E.class = Class,
				 E.address = CompatForm
				    ]
		       end,
		  A = lists:append(mnemosyne:eval(Q), mnemosyne:eval(Q2)),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).    

%% query [E || E <- table(employee),
%%             E <- [#employee{emp_id=312}, #employee{emp_id=400}]]

delete_record(Obj) ->
    F = fun() ->
		mnesia:delete_object(Obj)
	end,
    mnesia:transaction(F).

delete_with_key(Db, Key) ->
    F = fun() ->
		mnesia:delete({Db, Key})
		end,
    Rec = mnesia:transaction(F).

delete_user(User) ->
    delete_with_key(user, User).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Function: url_to_requristr/1
%% Description: We need to store the location as a searchable string
%%              in order to be able to look up a location to a user.
%%              This string is not used to look up the location of a
%%              user, but rather to see if we are to relay requests
%%              to a destination without challenging and so on.
%% Returns: URLstring
%%--------------------------------------------------------------------
url_to_requristr(URL) when record(URL, sipurl) ->
    Port = siprequest:default_port(URL#sipurl.proto, URL#sipurl.port),
    Host = sipurl:cleanup_hostname(URL#sipurl.host),
    User = sipurl:unescape_user(URL#sipurl.user),
    sipurl:print(#sipurl{proto="sip", user=User, pass=none, host=Host, port=Port, param=[]}).
