%%%--------------------------------------------------------------------
%%% File    : database_regexproute.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Access routines for a Mnesia table holding regular
%%%           expression rewrites of input addresses, used in
%%%           'incomingproxy'.
%%%
%%% @since    02 Jan 2003 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%--------------------------------------------------------------------
-module(database_regexproute).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create/0,
	 create/1,
	 insert/5,
	 list/0,
	 purge_class/2,
	 delete/5,

	 test/0,
	 test_create_table/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("database_regexproute.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% @type  transaction_result() = {atomic, Result} | {aborted, Reason}.
%%             The result of a Mnesia transaction. Result is a term().

%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    () -> term() "result of mnesia:create_table/2."
%%
%% @doc     Invoke create/1 with the list of servers indicated by the
%%          configuration parameter 'databaseservers'.
%% @private
%% @end
%%--------------------------------------------------------------------
create() ->
    {ok, S} = yxa_config:get_env(databaseservers),
    create(S).

%%--------------------------------------------------------------------
%% @spec    (Servers) -> term() "result of mnesia:create_table/2."
%%
%%            Servers = [atom()] "list of nodes"
%%
%% @doc     Create the table 'regexproute' on Servers.
%% @private
%% @end
%%--------------------------------------------------------------------
create(Servers) ->
    mnesia:create_table(regexproute, [{attributes, record_info(fields, regexproute)},
				      {disc_copies, Servers},
				      {type, bag}]).

%%--------------------------------------------------------------------
%% @spec    (Regexp, Flags, Class, Expire, Address) ->
%%            transaction_result()
%%
%%            Regexp  = string()
%%            Flags   = [{Key, Value}]
%%            Class   = atom()
%%            Expire  = integer() | never
%%            Address = string() "must be parseable with sipurl:parse/1"
%%
%% @doc     Insert a new regexproute entry into the database.
%% @end
%%--------------------------------------------------------------------
insert(Regexp, Flags, Class, Expire, Address) when is_list(Regexp), is_list(Flags), is_atom(Class),
						   is_integer(Expire); Expire == never, is_list(Address) ->
    db_util:insert_record(#regexproute{regexp = Regexp, flags = Flags, class = Class,
			       expire = Expire, address = Address}).

%%--------------------------------------------------------------------
%% @spec    (Regexp, Flags, Class, Expire, Address) ->
%%            transaction_result()
%%
%%            Regexp  = string()
%%            Flags   = [{Key, Value}]
%%            Class   = atom()
%%            Expire  = integer() | never
%%            Address = string() "must be parseable with sipurl:parse/1"
%%
%% @doc     Insert a new regexproute entry into the database.
%% @end
%%--------------------------------------------------------------------
delete(Regexp, Flags, Class, Expire, Address) when is_list(Regexp), is_list(Flags), is_atom(Class),
                                                   is_integer(Expire); Expire == never, is_list(Address) ->
    Fun = fun() ->
		  A = mnesia:match_object(#regexproute{regexp = Regexp,
						       flags = Flags,
						       class = Class,
						       expire = Expire,
						       address = Address,
						       _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).


%%--------------------------------------------------------------------
%% @spec    () -> [#regexproute{}]
%%
%% @doc     Lists all regexproutes in the database
%% @end
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(regexproute).

%%--------------------------------------------------------------------
%% @spec    (Regexp, Class) -> transaction_result()
%%
%%            Regexp = string()
%%            Class  = atom()
%%
%% @doc     Delete all regexproutes in database that matches the
%%          regexp and class.
%% @end
%%--------------------------------------------------------------------
purge_class(Regexp, Class) ->
    Fun = fun() ->
		  A = mnesia:match_object(#regexproute{regexp = Regexp,
						       class = Class,
						       _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).



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

    %% test x
    %%--------------------------------------------------------------------
    %%autotest:mark(?LINE, "f/a - 1"),

    ok.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Create a table in RAM only, for use in unit tests.
%% @hidden
%% @end
%%--------------------------------------------------------------------
test_create_table() ->
    case catch mnesia:table_info(regexproute, attributes) of
	Attrs when is_list(Attrs) ->
	    ok;
	{'EXIT', {aborted, {no_exists, regexproute, attributes}}} ->
	    %% Create table 'regexproute' in RAM for use in the tests here
	    {atomic, ok} =
		mnesia:create_table(regexproute, [{attributes, record_info(fields, regexproute)},
						  {type, bag}
						 ])
    end.
