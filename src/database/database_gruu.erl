%%%-------------------------------------------------------------------
%%% File    : database_gruu.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      GRUU database functions.
%%%
%%% @since     3 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(database_gruu).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create/0,
	 create/1,

	 insert/4,
	 update_last_registered/1,

	 fetch_using_gruu/1,
	 fetch_using_user_instance/2,
	 fetch_using_instance/1,
	 fetch_all/0,
	 list/0,

	 delete/1,
	 delete_gruus_for_user/1,
	 delete_gruus_for_instance/1,

	 get_transform_fun/0,

	 test/0,
	 test_create_table/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("database_gruu.hrl").
-include("siprecords.hrl").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%% @type gruu() = #gruu{}.
%%                private Mnesia record, the interface record is called gruu_dbe
-record(gruu, {
	  gruu,			%% string()
	  sipuser,		%% string()
	  instance_id,		%% string()
	  created,		%% integer(), util:timestamp() notion
	  last_registered,	%% integer(), util:timestamp() notion
	  flags			%% list() of {Key, Value} tuple(), for future use
	 }).

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
%% @doc     Create the table 'gruu' on Servers.
%% @private
%% @end
%%--------------------------------------------------------------------
create(Servers) when is_list(Servers) ->
    mnesia:create_table(gruu, [{attributes, record_info(fields, gruu)},
			       {disc_copies, Servers},
			       {index, [sipuser, instance_id]}
			       %% key = gruu
			       %% type = set, default for table
			      ]).


%%--------------------------------------------------------------------
%% @spec    (GRUU, SIPuser, InstanceId, Flags) -> transaction_result()
%%
%%            GRUU       = string()
%%            SIPuser    = string() "SIP authentication username (key in our location database)"
%%            InstanceId = string() "+sip.instance from Contact"
%%            Flags      = [{Key, Value}]
%%
%% @doc     Create a new GRUU entry in the database.
%% @end
%%--------------------------------------------------------------------
insert(GRUU, SIPuser, InstanceId, Flags) when is_list(GRUU), is_list(SIPuser), is_list(InstanceId),
					      is_list(Flags) ->
    Now = util:timestamp(),
    case fetch_using_gruu(GRUU) of
	{ok, E} when is_record(E, gruu_dbe) ->
	    erlang:error(non_unique_gruu);
	nomatch ->
	    db_util:insert_record(#gruu{gruu		= GRUU,
					sipuser		= SIPuser,
					instance_id	= InstanceId,
					created		= Now,
					last_registered	= Now,
					flags		= Flags
				       })
    end.


%%--------------------------------------------------------------------
%% @spec    (GRUU) -> transaction_result()
%%
%%            GRUU = string()
%%
%% @doc     Update the 'last_registered' on the GRUU database entry
%%          matching GRUU.
%% @end
%%--------------------------------------------------------------------
update_last_registered(GRUU) when is_list(GRUU) ->
    Now = util:timestamp(),
    F = fun() ->
		case mnesia:read({gruu, GRUU}) of
		    [OldEntry] ->
			ok = mnesia:delete_object(OldEntry),
			mnesia:write(OldEntry#gruu{last_registered = Now});
		    _ ->
			nomatch
		end
	   end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @spec    () -> [#gruu{}]
%%
%% @doc     List all GRUUs in the database.
%% @end
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(gruu).


%%--------------------------------------------------------------------
%% @spec    (GRUU) ->
%%            {ok, GRUUs} |
%%            nomatch
%%
%%            GRUU = string()
%%
%%            GRUUs = [#gruu_dbe{}]
%%
%% @doc     Fetch a GRUU database entry based on the GRUU string.
%% @end
%%--------------------------------------------------------------------
fetch_using_gruu(GRUU) when is_list(GRUU) ->
    F = fun() ->
		mnesia:read({gruu, GRUU})
	end,
    {atomic, Entrys} = mnesia:transaction(F),
    case make_fetch_result(Entrys) of
	{ok, [E]} ->
	    {ok, E};
	Res ->
	    Res
    end.


%%--------------------------------------------------------------------
%% @spec    (SipUser, InstanceId) ->
%%            {ok, GRUUs} |
%%            nomatch
%%
%%            SipUser    = string()
%%            InstanceId = string()
%%
%%            GRUUs = [#gruu_dbe{}]
%%
%% @doc     Fetch a GRUU database entry based on a SipUser and an
%%          Instance ID.
%% @end
%%--------------------------------------------------------------------
fetch_using_user_instance(SipUser, InstanceId) when is_list(SipUser), is_list(InstanceId) ->
    F = fun() ->
		mnesia:index_read(gruu, SipUser, #gruu.sipuser)
	end,
    {atomic, Entrys1} = mnesia:transaction(F),
    Entrys = [E || E <- Entrys1, E#gruu.instance_id == InstanceId],
    make_fetch_result(Entrys).


%%--------------------------------------------------------------------
%% @spec    (InstanceId) ->
%%            {ok, GRUUs} |
%%            nomatch
%%
%%            InstanceId = string()
%%
%%            GRUUs = [#gruu_dbe{}]
%%
%% @doc     Fetch all GRUU database entrys matching an Instance ID.
%%          There might be more than one if a UA uses a single
%%          Instance ID to acquire GRUUs for multiple users (AORs in
%%          the GRUU draft).
%% @end
%%--------------------------------------------------------------------
fetch_using_instance(InstanceId) when is_list(InstanceId) ->
    F = fun() ->
		mnesia:index_read(gruu, InstanceId, #gruu.instance_id)
	end,
    {atomic, Entrys} = mnesia:transaction(F),
    make_fetch_result(Entrys).


%%--------------------------------------------------------------------
%% @spec    () ->
%%            {ok, GRUUs} |
%%            nomatch
%%
%%            GRUUs = [#gruu_dbe{}]
%%
%% @doc     Fetch all GRUU database entrys.
%% @end
%%--------------------------------------------------------------------
fetch_all() ->
    make_fetch_result(db_util:tab_to_list(gruu)).


%%--------------------------------------------------------------------
%% @spec    (GRUU) -> transaction_result()
%%
%%            GRUU = string()
%%
%% @doc     Delete all GRUUs matching a GRUU string.
%% @end
%%--------------------------------------------------------------------
delete(GRUU) when is_list(GRUU) ->
    db_util:delete_with_key(gruu, GRUU).


%%--------------------------------------------------------------------
%% @spec    (SipUser) -> transaction_result()
%%
%%            SipUser = string()
%%
%% @doc     Delete all GRUUs matching a SIP user.
%% @end
%%--------------------------------------------------------------------
delete_gruus_for_user(SipUser) ->
    F = fun() ->
		A = mnesia:index_read(gruu, SipUser, #gruu.sipuser),
		Delete = fun(O) ->
				 mnesia:delete_object(O)
			 end,
		lists:foreach(Delete, A)
	end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @spec    (InstanceId) -> transaction_result()
%%
%%            InstanceId = string()
%%
%% @doc     Delete all GRUUs matching an Instance ID.
%% @end
%%--------------------------------------------------------------------
delete_gruus_for_instance(InstanceId) ->
    F = fun() ->
		A = mnesia:index_read(gruu, InstanceId, #gruu.instance_id),
		Delete = fun(O) ->
				 mnesia:delete_object(O)
			 end,
		lists:foreach(Delete, A)
	end,
    mnesia:transaction(F).


%%--------------------------------------------------------------------
%% @spec    () -> {ok, FieldInfo, Fun}
%%
%% @doc     Return a function to transform the gruu Mnesia table.
%% @private
%% @end
%%--------------------------------------------------------------------
get_transform_fun() ->
    %% Table = gruu,
    %% On update, don't forget : put({Table, update}, true),
    F = fun
	    (G) when is_record(G, gruu) ->
		%% nothing to update
		G
	end,
    {ok, record_info(fields, gruu), F}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Entrys) ->
%%            {ok, GRUUs} |
%%            nomatch
%%
%%            Entrys = [#gruu{}]
%%
%%            GRUUs = [#gruu_dbe{}]
%%
%% @doc     Turn a list of 'gruu' records into a list of 'gruu_dbe'
%%          records. 'gruu' records are our internal data format for
%%          storing GRUUs in Mnesia, 'gruu_dbe' are potentially
%%          different records that outside modules might use.
%% @end
%%--------------------------------------------------------------------
make_fetch_result([]) ->
    nomatch;
make_fetch_result(Entrys) when is_list(Entrys) ->
    %% Transform from this modules private mnesia database record (gruu) to
    %% the interface record type gruu_dbe
    Transform = fun(E) when is_record(E, gruu) ->
			#gruu_dbe{gruu			= E#gruu.gruu,
				  sipuser		= E#gruu.sipuser,
				  instance_id		= E#gruu.instance_id,
				  created		= E#gruu.created,
				  last_registered	= E#gruu.last_registered,
				  flags			= E#gruu.flags
				 }
		end,
    Converted = [Transform(E) || E <- Entrys],
    {ok, Converted}.


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
    case catch mnesia:table_info(gruu, attributes) of
	Attrs when is_list(Attrs) ->
	    ok;
	{'EXIT', {aborted, {no_exists, gruu, attributes}}} ->
	    %% Create table 'gruu' in RAM for use in the tests here
	    {atomic, ok} =
		mnesia:create_table(gruu, [{attributes, record_info(fields, gruu)},
					   {index, [sipuser, instance_id]}
					  ])
    end.
