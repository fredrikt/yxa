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
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%%           KeyValList = list() of {Key, Val}
%%           Key, Val = term()
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
new() ->
    [].

new(KeyValList) ->
    F = fun({Key, Val}, DB) ->
		add(DB, Key, Val)
	end,
    lists:foldl(F, new(), KeyValList).


%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : KeyValList = list() of {Key, Val}
%%           Key, Val = term() 
%%--------------------------------------------------------------------
to_key_val(DB) ->
    DB.

%%--------------------------------------------------------------------
%% Function: add(DB, Key, Value)
%%           add(DB, Key)
%%           DB    = 
%%           Key   = term()
%%           Value = term()
%% Descrip.: add new entry to DB.
%% Returns : DB | throw({error, duplicate_key})
%%--------------------------------------------------------------------
add(DB, Key) ->
    add3(DB, {Key, none}).

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
%%           DB  =
%%           Key = term()
%% Descrip.: retrive the value of Key if it is contained in DB
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
%%           ContactParam = contact_param record()
%%           Key          = term(), is treated as case insensetive
%% Descrip.: find the Key-Val pair to remove from DB
%% Returns : 
%%--------------------------------------------------------------------
rm(DB, Key) ->
    lists:keydelete(Key, 1, DB).
    


test() ->
    
    ok.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------

