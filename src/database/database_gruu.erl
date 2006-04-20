%%%-------------------------------------------------------------------
%%% File    : database_gruu.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: GRUU database functions.
%%%
%%% Created :  3 Mar 2006 by Fredrik Thulin <ft@it.su.se>
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
%% Macros
%%--------------------------------------------------------------------

%% private Mnesia record, the interface record is called gruu_dbe
-record(gruu, {
	  gruu,			%% string()
	  sipuser,		%% string()
	  instance_id,		%% string()
	  created,		%% integer(), util:timestamp() notion
	  last_registered,	%% integer(), util:timestamp() notion
	  flags			%% list() of {Key, Value} tuple(), for future use
	 }).


%%====================================================================
%% External functions
%%====================================================================



%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
create() ->
    create([node()]).

create(Servers) ->
    mnesia:create_table(gruu, [{attributes, record_info(fields, gruu)},
			       {disc_copies, Servers},
			       {index, [sipuser, instance_id]}
			       %% key = gruu
			       %% type = set, default for table
			      ]).


%%--------------------------------------------------------------------
%% Function: insert(GRUU, SIPuser, InstanceId, Flags)
%%           GRUU       = string()
%%           SIPuser    = string(), SIP authentication username (key
%%                        in our location database)
%%           InstanceId = string(), +sip.instance from Contact
%%           Flags      = list() of {Key, Value} tuple()
%% Descrip.: Create a new GRUU entry in the database.
%% Returns : mnesia transaction()   |
%%           error(non_unique_gruu)
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
%% Function: update_last_registered(GRUU)
%%           GRUU = string()
%% Descrip.: Update the 'last_registered' on the GRUU database entry
%%           matching GRUU.
%% Returns : mnesia transaction()
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
%% Function: list()
%% Descrip.: List all GRUUs in the database.
%% Returns : list() of gruu record()
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(gruu).


%%--------------------------------------------------------------------
%% Function: fetch_using_gruu(GRUU)
%%           GRUU = string()
%% Descrip.: Fetch a GRUU database entry based on the GRUU string.
%% Returns : {ok, GRUUs} |
%%           nomatch
%%           GRUUs = list() of gruu_dbe record()
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
%% Function: fetch_using_user_instance(SipUser, InstanceId)
%%           SipUser    = string()
%%           InstanceId = string()
%% Descrip.: Fetch a GRUU database entry based on a SipUser and an
%%           Instance ID.
%% Returns : {ok, GRUUs} |
%%           nomatch
%%           GRUUs = list() of gruu_dbe record()
%%--------------------------------------------------------------------
fetch_using_user_instance(SipUser, InstanceId) when is_list(SipUser), is_list(InstanceId) ->
    F = fun() ->
		mnesia:index_read(gruu, SipUser, #gruu.sipuser)
	end,
    {atomic, Entrys1} = mnesia:transaction(F),
    Entrys = [E || E <- Entrys1, E#gruu.instance_id == InstanceId],
    make_fetch_result(Entrys).


%%--------------------------------------------------------------------
%% Function: fetch_using_user_instance(SipUser, InstanceId)
%%           SipUser    = string()
%%           InstanceId = string()
%% Descrip.: Fetch all GRUU database entrys matching an Instance ID.
%%           There might be more than one if a UA uses a single
%%           Instance ID to acquire GRUUs for multiple users (AORs in
%%           the GRUU draft).
%% Returns : {ok, GRUUs} |
%%           nomatch
%%           GRUUs = list() of gruu_dbe record()
%%--------------------------------------------------------------------
fetch_using_instance(InstanceId) when is_list(InstanceId) ->
    F = fun() ->
		mnesia:index_read(gruu, InstanceId, #gruu.instance_id)
	end,
    {atomic, Entrys} = mnesia:transaction(F),
    make_fetch_result(Entrys).


%%--------------------------------------------------------------------
%% Function: fetch_all()
%% Descrip.: Fetch all GRUU database entrys.
%% Returns : {ok, GRUUs} |
%%           nomatch
%%           GRUUs = list() of gruu_dbe record()
%%--------------------------------------------------------------------
fetch_all() ->
    make_fetch_result(db_util:tab_to_list(gruu)).


%%--------------------------------------------------------------------
%% Function: delete(GRUU)
%%           GRUU = string()
%% Descrip.: Delete all GRUUs matching a GRUU string.
%% Returns : mnesia:transaction()
%%--------------------------------------------------------------------
delete(GRUU) when is_list(GRUU) ->
    db_util:delete_with_key(gruu, GRUU).


%%--------------------------------------------------------------------
%% Function: delete_gruus_for_user(SipUser)
%%           SipUser = string()
%% Descrip.: Delete all GRUUs matching a SIP user.
%% Returns : mnesia:transaction()
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
%% Function: delete_gruus_for_instance(InstanceId)
%%           InstanceId = string()
%% Descrip.: Delete all GRUUs matching an Instance ID.
%% Returns : mnesia:transaction()
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
%% Function: get_transform_fun()
%% Descrip.: Return a function to transform the gruu Mnesia table.
%% Returns : {ok, FieldInfo, Fun}
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
%% Function: make_fetch_result(Entrys)
%%           Entrys = list() of gruu record()
%% Descrip.: Turn a list of 'gruu' records into a list of 'gruu_dbe'
%%           records. 'gruu' records are our internal data format for
%%           storing GRUUs in Mnesia, 'gruu_dbe' are potentially
%%           different records that outside modules might use.
%% Returns : {ok, GRUUs} |
%%           nomatch
%%           GRUUs = list() of gruu_dbe record() 
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
