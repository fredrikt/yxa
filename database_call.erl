-module(database_call).
-export([create_call/0, insert_call/4, get_call/1, list_calls/0,
	 insert_call_unique/4, set_data_type/3, delete_call_type/2, get_call_type/2,
	 fetch_call/2, fetch_dialogue/2, delete_all_calls/0]).

-include("database_call.hrl").

-include_lib("mnemosyne/include/mnemosyne.hrl").

insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

create_call() ->
    mnesia:create_table(call, [{attributes, record_info(fields, call)},
    			       {ram_copies, [node()]}
    			      ]).

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

delete_call_type(Callid, Type) ->
	F = fun() ->
		    Q = query
			    [E || E <- table(call),
				  E.callid = Callid,
				  E.type = Type]
			end,
		    A = mnemosyne:eval(Q),
		    Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		    lists:foreach(Delete, A)
	    end,
    mnesia:transaction(F).

delete_all_calls() ->
    F = fun() ->
		Q = query
			[E || E <- table(call)]
		    end,
		    A = mnemosyne:eval(Q),
		    Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		    lists:foreach(Delete, A)
	end,
    mnesia:transaction(F).

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

get_call_type(Call, Type) ->
	F = fun() ->
		    Q = query
			    [{E.headers, E.data} || E <- table(call),
						    E.callid = Call,
						    E.type = Type]
			end,
		    mnemosyne:eval(Q)
	    end,
    mnesia:transaction(F).

set_data_type(CallID, Data, Type) ->
    F = fun() ->
		Q = query
			[E || E <- table(call),
			      E.callid = CallID,
			      E.type = Type]
		    end,
		A = mnemosyne:eval(Q),
		Update = fun(O) ->
				 mnesia:write(O#call{data = Data})
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

fetch_dialogue({CallID, FromTag, none}, Type) ->
    case get_call_type({CallID, FromTag, none}, Type) of
	{atomic, [Record]} ->
	    {{CallID, FromTag, none}, Record};
	{atomic, []} ->
	     nomatch
    end;
fetch_dialogue(DialogueID, Type) ->
    case database_call:get_call_type(DialogueID, Type) of
	{atomic, [Record]} ->
	    {DialogueID, Record};
	{atomic, []} ->
	    {CallID, FromTag, _} = DialogueID,
	    fetch_dialogue({CallID, FromTag, none}, Type)
    end.
        
fetch_call(Header, Type) ->
    DialogueID = sipheader:dialogueid(Header),
    case fetch_dialogue(DialogueID, Type) of
	{_, Call} ->
	    Call;
	_ ->
	    nomatch
    end.

