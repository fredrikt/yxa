-module(database_regexproute).
-export([create/0, create/1, insert/5, list/0, purge_class/2, convert_urls/0]).

-include("database_regexproute.hrl").
-include("siprecords.hrl").

-include_lib("mnemosyne/include/mnemosyne.hrl").

insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

create() ->
    create(servers()).

create(Servers) ->
    mnesia:create_table(regexproute, [{attributes, record_info(fields, regexproute)},
				      {disc_copies, Servers},
				      {type, bag}]).

servers() ->
    sipserver:get_env(databaseservers).

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

convert_urls() ->
    F = fun() ->
		Q = query
			[E || E <- table(regexproute)]
		    end,
		A = mnemosyne:eval(Q),
		Update = fun(O) ->
				 mnesia:delete_object(O),
				 mnesia:write(rewrite_url(O))
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

rewrite_url(R) when record(R, regexproute) ->
    case R#regexproute.address of
	URL when record(URL, sipurl) ->
	    %% all set
	    R;
	{User, Pass, Host, Port, Parameters} ->
	    %% old format
	    U = #sipurl{proto="sip", user=User, pass=Pass, host=Host, port=Port, param=Parameters},
	    io:format("database_regexproute: Rewrote regexp ~p URL ~p~n", [R#regexproute.regexp, sipurl:print(U)]),
	    R#regexproute{address=U};
	Unknown ->
	    io:format("database_regexproute: Unknown URL format ~p in entry :~n~p~n",
		      [Unknown, R])
    end.

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
