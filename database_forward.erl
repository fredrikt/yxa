-module(database_forward).
-export([create/0, insert/4, fetch/1, list/0, delete/1
	]).

-include("database_forward.hrl").

-include_lib("mnemosyne/include/mnemosyne.hrl").

% -record(forward, {number, forwards, timeout, localring}).

insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

create() ->
    mnesia:create_table(forward, [{attributes, record_info(fields, forward)},
				  {disc_copies, [node()]}
				 ]).

insert(Number, Forwards, Timeout, Localring) ->
    insert_record(#forward{number = Number,
			   forwards = Forwards,
			   timeout = Timeout,
			   localring = Localring}).

list() ->
    F = fun() ->
		Q = query
			[E || E <- table(forward)]
		    end,
		mnemosyne:eval(Q)
	end,
    mnesia:transaction(F).

fetch(Number) ->
	F = fun() ->
		    Q = query
			    [{E.forwards, E.timeout, E.localring} ||
				E <- table(forward),
				E.number = Number]
			end,
		    mnemosyne:eval(Q)
	    end,
    mnesia:transaction(F).

delete_with_key(Db, Key) ->
    F = fun() ->
		mnesia:delete({Db, Key})
		end,
    Rec = mnesia:transaction(F).

delete(Number) ->
    delete_with_key(forward, Number).
