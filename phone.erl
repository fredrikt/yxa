-module(phone).
-export([init/0, create/0, insert_phone/5, get_phone/1, list_phones/0,
	 get_user/1, insert_user/5, list_users/0,
	 insert_purge_phone/5, insert_purge_class_phone/5,
	 purge_class_phone/2, expired_phones/0, delete_record/1,
	 delete_user/1, set_user_password/2, set_user_flags/2,
	 set_user_numbers/2]).

-include("phone.hrl").

-include_lib("mnemosyne/include/mnemosyne.hrl").

init() ->
    mnesia:create_schema([node()]),
    mnesia:start().

create() ->
    mnesia:create_table(phone, [{attributes, record_info(fields, phone)},
				{disc_copies, [node()]},
				{type, bag}]),
    mnesia:create_table(user, [{attributes, record_info(fields, user)},
			       {disc_copies, [node()]}]).

insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

insert_phone(Number, Flags, Class, Expire, Address) ->
    insert_record(#phone{number = Number, flags = Flags, class = Class,
			 expire = Expire, address = Address}).

insert_purge_phone(Number, Flags, Class, Expire, Address) ->
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
				      expire = Expire, address = Address})
	  end,
    mnesia:transaction(Fun).    

insert_purge_class_phone(Number, Flags, Class, Expire, Address) ->
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
				      expire = Expire, address = Address})
	  end,
    mnesia:transaction(Fun).    

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

insert_user(User, Password, Number, Flags, Classes) ->
    insert_record(#user{user = User, password = Password, number = Number,
			flags = Flags, classes = Classes}).

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
    
get_phone(Number) ->
	F = fun() ->
		    Q = query
			    [{E.address, E.flags, E.class, E.expire} ||
				E <- table(phone), E.number = Number]
			end,
		    mnemosyne:eval(Q)
	    end,
    mnesia:transaction(F).

get_user(User) ->
	F = fun() ->
		    Q = query
			    [{E.password, E.number, E.flags, E.classes} || E <- table(user),
						       E.user = User]
			end,
		    mnemosyne:eval(Q)
	    end,
    mnesia:transaction(F).

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
			[E || E <- table(user),
			      E.user = User]
		    end,
		A = mnemosyne:eval(Q),
		Update = fun(O) ->
				 mnesia:write(O#user{number = Numbers})
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).


expired_phones() ->
    {Meg, Sec, _} = now(),
    Now = Meg * 1000000 + Sec,
    F = fun() ->
		Q = query
			[ E || E <- table(phone), E.expire < Now]
		    end,
		mnemosyne:eval(Q)
	end,
    mnesia:transaction(F).

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
