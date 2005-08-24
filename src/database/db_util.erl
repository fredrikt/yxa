%% This module contains various usefull functions used to handle
%% mnesia tables
%%--------------------------------------------------------------------

-module(db_util).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 tab_to_list/1,
	 delete_all_entries/1,
	 insert_record/1,
	 delete_record/1,
	 delete_with_key/2,
	 match_object/1,
	 generic_table_info/2
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
%% Function: tab_to_list(MnesiaTab)
%%           MnesiaTab = atom(), the name of a mnesia table
%% Descrip.: return a list with all entries in MnesiaTab
%% Returns : list() of records
%% Note    : This is a time and memory consuming operation if the
%%           table is large
%%--------------------------------------------------------------------
tab_to_list(MnesiaTab) ->
    F = fun() ->
		Fun = fun(Rec, Acc) ->
			      [Rec | Acc]
		      end,
		mnesia:foldl(Fun, [], MnesiaTab)
	end,
    {atomic, L} = mnesia:transaction(F),
    lists:reverse(L).

%%--------------------------------------------------------------------
%% Function: delete_all_entries(MnesiaTab)
%%           MnesiaTab = atom(), the name of a mnesia table
%% Descrip.: delete all entries in the mnesia table MnesiaTab
%% Returns : mnesia:transaction()
%%--------------------------------------------------------------------
delete_all_entries(MnesiaTab) ->
    F = fun() ->
		Fun = fun(Rec, _Acc) ->
			      mnesia:delete_object(Rec)
		      end,
		mnesia:foldl(Fun, dummy_acc, MnesiaTab)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function: insert_record(Record)
%%           Record = record(), the record MUST have the same name as
%%           the table it is inserted into
%% Descrip.: wraps mnesia:write in a transaction context
%% Returns : mnesia:transaction()
%%--------------------------------------------------------------------
insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% Function: delete_record(Rec)
%%           Rec = record(), the record MUST have the same name as
%%           the table it is deleted from
%% Descrip.: wraps mnesia:delete_object in a transaction context
%% Returns : mnesia:transaction()
%%--------------------------------------------------------------------
delete_record(Rec) ->
    F = fun() ->
		mnesia:delete_object(Rec)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function: delete_with_key(Db, Key)
%%           Db = atom(), name of mnesia database
%%           Key = term(), key value for entry/entries to delete
%% Descrip.: wraps mnesia:delete({Db, Key}) in a transaction context
%% Returns : mnesia:transaction()
%%--------------------------------------------------------------------
delete_with_key(Db, Key) ->
    F = fun() ->
		mnesia:delete({Db, Key})
	end,
    Rec = mnesia:transaction(F),
    Rec.

%%--------------------------------------------------------------------
%% Function: match_object(Pattern)
%%           Pattern = a tuple as used by mnesia:match_object/1,
%%           usually something like:
%%           #rec_name{f1 = V1, f2 = V2, ..., _ = '_'} where '_' acts
%%           as a wildcard value
%% Descrip.: alternative match_object based on mnesia:select/2 it
%%           should be faster than mnesia:match_object/1. Finds the
%%           records that match the pattern Pattern.
%% Returns : list() of records
%% Note    : the record name must be = table name. '$<number>'
%%           (e.g. '$1', '$2', .... ) can not be used in Pattern (it
%%           has special meaning in the match specification used by
%%           mnesia:select/1)
%%--------------------------------------------------------------------
match_object(Pattern) ->
    mnesia:select(element(1,Pattern),
		  [{Pattern,
		    [],
		    ['$_']
		   }]).

%%--------------------------------------------------------------------
%% Function: generic_table_info(Tab, Item)
%%           Tab  = atom(), table name
%%           Item = atom(), item we are interested in
%% Descrip.: Get mnesia table information, regardless of where table
%%           resides.
%% Returns : term()
%%--------------------------------------------------------------------
generic_table_info(Tab, Item) ->
    Info = fun(T, I) ->
	      mnesia:table_info(T, I)
	   end,
    mnesia:activity(async_dirty, Info, [Tab, Item], mnesia_frag).
