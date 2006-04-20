%%
%%--------------------------------------------------------------------

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
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("database_regexproute.hrl").
-include("siprecords.hrl").

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
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
create() ->
    create(servers()).

create(Servers) ->
    mnesia:create_table(regexproute, [{attributes, record_info(fields, regexproute)},
				      {disc_copies, Servers},
				      {type, bag}]).

servers() ->
    {ok, S} = yxa_config:get_env(databaseservers),
    S.

%%--------------------------------------------------------------------
%% Function: insert(Regexp, Flags, Class, Expire, Address)
%%           Regexp  = string()
%%           Flags   = list() of {Key, Value}
%%           Class   = atom()
%%           Expire  = integer() | never
%%           Address = string(), must be parseable with sipurl:parse/1
%% Descrip.: Insert a new regexproute entry into the database.
%% Returns : result of the Mnesia transaction()
%%--------------------------------------------------------------------
insert(Regexp, Flags, Class, Expire, Address) when is_list(Regexp), is_list(Flags), is_atom(Class),
						   is_integer(Expire); Expire == never, is_list(Address) ->
    db_util:insert_record(#regexproute{regexp = Regexp, flags = Flags, class = Class,
			       expire = Expire, address = Address}).

%%--------------------------------------------------------------------
%% Function: insert(Regexp, Flags, Class, Expire, Address)
%%           Regexp  = string()
%%           Flags   = list() of {Key, Value}
%%           Class   = atom()
%%           Expire  = integer() | never
%%           Address = string(), must be parseable with sipurl:parse/1
%% Descrip.: Insert a new regexproute entry into the database.
%% Returns : result of the Mnesia transaction()
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
%% Function: list()
%% Descrip.: Lists all regexproutes in the database
%% Returns : list() of regexproute record()
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(regexproute).

%%--------------------------------------------------------------------
%% Function: purge_class(Regexp, Class)
%%           Regexp = string()
%%           Class  = atom()
%% Descrip.: Delete all regexproutes in database that matches the
%%           regexp and class.
%% Returns : Result of the Mnesia transaction
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
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->

    %% test x
    %%--------------------------------------------------------------------
    %%autotest:mark(?LINE, "f/a - 1"),

    ok.

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
