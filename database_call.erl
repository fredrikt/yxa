-module(database_call).
-export([create_call/0, insert_call/4, get_call/1, list_calls/0,
	 insert_call_unique/4, set_data/2]).

-include("database_call.hrl").

-include_lib("mnemosyne/include/mnemosyne.hrl").

insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

create_call() ->
    mnesia:create_table(call, [{attributes, record_info(fields, call)}]).

insert_call(Callid, Type, Headers, Data) ->
    insert_record(#call{callid = Callid, type = Type, headers = Headers,
			data = Data}).

insert_call_unique(Callid, Type, Headers, Data) ->
    Fun = fun() ->
		  Q = query
			  [E || E <- table(call),
				E.callid = Callid]
		      end,
		  case mnemosyne:eval(Q) of
		      [] ->
			  mnesia:write(#call{callid = Callid, type = Type,
					     headers = Headers, data = Data});
		      _ ->
			  mnesia:abort(key_exists)
		  end
	  end,
    mnesia:transaction(Fun).

list_calls() ->
    F = fun() ->
		Q = query
			[E || E <- table(call)]
		    end,
		mnemosyne:eval(Q)
	end,
    mnesia:transaction(F).

get_call(Call) ->
	F = fun() ->
		    Q = query
			    [{E.type, E.headers, E.data} || E <- table(call),
							    E.callid = Call]
			end,
		    mnemosyne:eval(Q)
	    end,
    mnesia:transaction(F).

set_data(CallID, Data) ->
    F = fun() ->
		Q = query
			[E || E <- table(call),
			      E.callid = CallID]
		    end,
		A = mnemosyne:eval(Q),
		Update = fun(O) ->
				 mnesia:write(O#call{data = Data})
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).
