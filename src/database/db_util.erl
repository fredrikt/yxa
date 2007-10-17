%%%-------------------------------------------------------------------
%%% File    : db_util.erl
%%% @author   Håkan Stenholm <hsten@it.su.se>
%%% @doc      This module contains various usefull functions used to
%%%           handle mnesia tables
%%%
%%% @since    22 Sep 2004 by Håkan Stenholm <hsten@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
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
%% @spec    (MnesiaTab) -> [records]
%%
%%            MnesiaTab = atom() "the name of a mnesia table"
%%
%% @doc     return a list with all entries in MnesiaTab Note : This is
%%          a time and memory consuming operation if the table is
%%          large
%% @end
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
%% @spec    (MnesiaTab) -> term()
%%
%%            MnesiaTab = atom() "the name of a mnesia table"
%%
%% @doc     delete all entries in the mnesia table MnesiaTab
%% @end
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
%% @spec    (Record) -> term()
%%
%%            Record = term()
%%
%% @doc     wraps mnesia:write in a transaction context.
%%          NOTE: Record MUST have the same name as the table it is
%%          inserted into
%%
%% @end
%%--------------------------------------------------------------------
insert_record(Record) ->
    Fun = fun() ->
		  mnesia:write(Record)
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @spec    (Record) -> term()
%%
%%            Record = term()
%%
%% @doc     wraps mnesia:delete_object in a transaction context
%%          NOTE: Record MUST have the same name as the table it is
%%          inserted into
%%          Returns : mnesia:transaction()
%% @end
%%--------------------------------------------------------------------
delete_record(Record) ->
    F = fun() ->
		mnesia:delete_object(Record)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @spec    (Db, Key) -> term()
%%
%%            Db  = atom() "name of mnesia database"
%%            Key = term() "key value for entry/entries to delete"
%%
%% @doc     wraps mnesia:delete({Db, Key}) in a transaction context
%% @end
%%--------------------------------------------------------------------
delete_with_key(Db, Key) ->
    F = fun() ->
		mnesia:delete({Db, Key})
	end,
    Rec = mnesia:transaction(F),
    Rec.

%%--------------------------------------------------------------------
%% @spec    (Pattern) ->
%%            Records
%%
%%            Pattern = term() "a tuple as used by mnesia:match_object/1"
%%
%%            Records = [term()]
%%
%% @doc     alternative match_object based on mnesia:select/2 it
%%          should be faster than mnesia:match_object/1. Finds the
%%          records that match the pattern Pattern.
%%          Pattern is usually something like: #rec_name{f1 = V1, f2
%%          = V2, ..., _ = '_'} where '_' acts as a wildcard value
%%          NOTE : the record name must be = table name. '$number'
%%          (e.g. '$1', '$2', .... ) can not be used in Pattern (it
%%          has special meaning in the match specification used by
%%          mnesia:select/1)
%%
%% @end
%%--------------------------------------------------------------------
match_object(Pattern) ->
    mnesia:select(element(1,Pattern),
		  [{Pattern,
		    [],
		    ['$_']
		   }]).

%%--------------------------------------------------------------------
%% @spec    (Tab, Item) -> term()
%%
%%            Tab  = atom() "table name"
%%            Item = atom() "item we are interested in"
%%
%% @doc     Get mnesia table information, regardless of where table
%%          resides.
%% @end
%%--------------------------------------------------------------------
generic_table_info(Tab, Item) ->
    Info = fun(T, I) ->
	      mnesia:table_info(T, I)
	   end,
    mnesia:activity(async_dirty, Info, [Tab, Item], mnesia_frag).
