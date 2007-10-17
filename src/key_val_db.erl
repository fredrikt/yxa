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
%% @spec    () -> key_val_db()
%%
%% @doc     Create a new key_val_db.
%% @end
%%--------------------------------------------------------------------
new() ->
    [].

%%--------------------------------------------------------------------
%% @spec    (KeyValList) -> key_val_db()
%%
%%            KeyValList = [{Key, Val}]
%%            Key        = term()
%%            Val        = term()
%%
%% @doc     Create a new key_val_db with content.
%% @end
%%--------------------------------------------------------------------
new(KeyValList) ->
    F = fun({Key, Val}, DB) ->
		add(DB, Key, Val)
	end,
    lists:foldl(F, new(), KeyValList).


%%--------------------------------------------------------------------
%% @spec    (DB) ->
%%            KeyValList
%%
%%            DB = key_val_db()
%%
%%            KeyValList = [{Key, Val}]
%%            Key        = term()
%%            Val        = term()
%%
%% @doc     Turn one of our internal format databases into a list of
%%          key-value tuples.
%% @end
%%--------------------------------------------------------------------
to_key_val(DB) ->
    DB.

%%--------------------------------------------------------------------
%% @spec    (DB, Key) -> DB
%%
%%            DB  = key_val_db()
%%            Key = term()
%%
%% @throws  {error, duplicate_key} 
%%
%% @equiv   add(DB, Key, none)
%% @end
%%--------------------------------------------------------------------
add(DB, Key) ->
    add3(DB, {Key, none}).

%%--------------------------------------------------------------------
%% @spec    (DB, Key, Value) -> DB
%%
%%            DB    = key_val_db()
%%            Key   = term()
%%            Value = term()
%%
%% @throws  {error, duplicate_key} 
%%
%% @doc     Add new entry to DB.
%% @end
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
%% @spec    (DB, Key) -> [term()] | []
%%
%%            DB  = key_val_db()
%%            Key = term()
%%
%% @doc     Retrive the value of Key if it is contained in DB.
%% @end
%%--------------------------------------------------------------------
find(DB, Key) ->
    case lists:keysearch(Key, 1, DB) of
	{value, {Key,Val}} ->
	    [Val];
	false ->
	    []
    end.

%%--------------------------------------------------------------------
%% @spec    (DB, Key) -> key_val_db()
%%
%%            DB  = key_val_db()
%%            Key = term()
%%
%% @doc     Delete a Key-Val pair from DB.
%% @end
%%--------------------------------------------------------------------
rm(DB, Key) ->
    lists:keydelete(Key, 1, DB).


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    ok.
