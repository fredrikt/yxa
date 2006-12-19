%% this module is used by contact_param.erl and url_param.erl
%% to ensure that their entries are unique and can be accessed
%% efficiently.
%% The current implementation handles any kind of key-value pairs
%% but, the current yxa code mostly uses string() and in some cases
%% 'none' when there is no value to enter for a key-value pair.
%%
%% Note: add is implemented in a simplistic manner, time consumption is
%% O(N) (N is the current size of the DB) - but considering the
%% small size of the parameter DBs created, this may actualy be an
%% advantage because most other solutions are likely to have a greater
%% overhead, making them slower for small DBs.
%%
%% XXX should case sensitivity be handled here ? stored quoted strings
%% are usually supposed to be case sensitive compared to other
%% tokens and parameter values.
%%--------------------------------------------------------------------

-module(key_val_db).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 %% create
	 new/0,
	 new/1,

	 %% retrieve
	 to_key_val/1,
	 find/2,

	 %% update
	 add/2,
	 add/3,
	 rm/2,

	 %% XXX should there be test code ? contact_param.erl and
	 %% url_param.erl current do the testing
	 test/0
	]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% @type key_val_db() = [{Key, Val}].
%%           Primitive format of 'internal' database.

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: new()
%% Descrip.: Create a new key_val_db.
%% Returns : key_val_db()
%%--------------------------------------------------------------------
new() ->
    [].

%%--------------------------------------------------------------------
%% Function: new(KeyValList)
%%           KeyValList = [{Key, Val}]
%%           Key        = term()
%%           Val        = term()
%% Descrip.: Create a new key_val_db with content.
%% Returns : key_val_db()
%%--------------------------------------------------------------------
new(KeyValList) ->
    F = fun({Key, Val}, DB) ->
		add(DB, Key, Val)
	end,
    lists:foldl(F, new(), KeyValList).


%%--------------------------------------------------------------------
%% Function: to_key_val(DB)
%%           DB = key_val_db()
%% Descrip.: Turn one of our internal format databases into a list of
%%           key-value tuples.
%% Returns : KeyValList = list() of {Key, Val}
%%           Key = term()
%%           Val = term()
%%--------------------------------------------------------------------
to_key_val(DB) ->
    DB.

%%--------------------------------------------------------------------
%% Function: add(DB, Key)
%%           DB    = key_val_db()
%%           Key   = term()
%% @equiv   add(DB, Key, none)
%% Returns : DB
%%           throw({error, duplicate_key})
%%--------------------------------------------------------------------
add(DB, Key) ->
    add3(DB, {Key, none}).

%%--------------------------------------------------------------------
%% Function: add(DB, Key, Value)
%%           DB    = key_val_db()
%%           Key   = term()
%%           Value = term()
%% Descrip.: Add new entry to DB.
%% Returns : DB
%%           throw({error, duplicate_key})
%%--------------------------------------------------------------------
add(DB, Key, Value) ->
    add3(DB, {Key, Value}).

add3([], Pair) ->
    [Pair];
add3([{K2, _} | _R], {K2, _V2}) ->
    throw({error, duplicate_key});
add3([Pair1 | R], Pair2) ->
    [Pair1 | add3(R, Pair2)].

%%--------------------------------------------------------------------
%% Function: find(DB, Key)
%%           DB  = key_val_db()
%%           Key = term()
%% Descrip.: Retrive the value of Key if it is contained in DB.
%% Returns : [term()] | []
%%--------------------------------------------------------------------
find(DB, Key) ->
    case lists:keysearch(Key, 1, DB) of
	{value, {Key,Val}} ->
	    [Val];
	false ->
	    []
    end.

%%--------------------------------------------------------------------
%% Function: remove(DB, Key)
%%           DB  = key_val_db()
%%           Key = term()
%% Descrip.: Delete a Key-Val pair from DB.
%% Returns : key_val_db()
%%--------------------------------------------------------------------
rm(DB, Key) ->
    lists:keydelete(Key, 1, DB).


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%%--------------------------------------------------------------------
test() ->
    ok.
