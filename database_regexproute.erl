-module(database_regexproute).
-export([create/0, insert/5, list/0, purge_class/2]).

-include("database_regexproute.hrl").

-include_lib("mnemosyne/include/mnemosyne.hrl").

insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

create() ->
    mnesia:create_table(regexproute, [{attributes, record_info(fields, regexproute)},
				      {disc_copies, [node()]},
				      {type, bag}]).

insert(Regexp, Flags, Class, Expire, Address) ->
    insert_record(#regexproute{regexp = Regexp, flags = Flags, class = Class,
			       expire = Expire, address = Address}).

list() ->
    F = fun() ->
		Q = query
			[E || E <- table(regexproute)]
		    end,
		mnemosyne:eval(Q)
	end,
    mnesia:transaction(F).

purge_class(Regexp, Class) ->
    Fun = fun() ->
		  Q = query
			  [E || E <- table(regexproute),
				E.regexp = Regexp,
				E.class = Class]
		      end,
		  A = mnemosyne:eval(Q),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).
