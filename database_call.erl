-module(database_call).
-export([create_call/0, insert_call/3, get_call/1, list_calls/0,
	 insert_call_unique/3]).

-include("database_call.hrl").

-include_lib("mnemosyne/include/mnemosyne.hrl").

create_call() ->
    mnesia:create_table(call, [{attributes, record_info(fields, call)}]).

insert_call(Callid, Headers, Pid) ->
    insert_record(#call{callid = Callid, headers = Headers,
			pid = Pid}).

insert_call_unique(Callid, Headers, Pid) ->
    Fun = fun() ->
		  Q = query
			  [E || E <- table(call),
				E.callid = Callid]
		      end,
		  case mnemosyne:eval(Q) of
		      [] ->
			  mnesia:write(#call{callid = Callid, headers = Headers,
					     pid = Pid});
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
			    [{E.headers, E.pid} || E <- table(call),
						   E.callid = Call]
			end,
		    mnemosyne:eval(Q)
	    end,
    mnesia:transaction(F).

