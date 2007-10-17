%%%-------------------------------------------------------------------
%%% File    : database_eventdata.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Eventdata database functions.
%%%
%%% @since     3 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(database_eventdata).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create/0,
	 create/1,

	 insert/6,
	 update/7,
	 refresh_presentity_etag/4,

	 fetch_using_presentity/1,
	 fetch_using_presentity_etag/2,
	 list/0,

	 delete_expired/0,
	 delete_using_presentity/1,
	 delete_using_presentity_etag/2,

	 decode_mnesia_change_event/1,

	 get_transform_fun/0,

	 test/0,
	 test_create_table/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("event.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(MIN_ABSOLUTE_TIME, 1161388800).  %% refuse expiration times that would be before this

%% @type eventdata() = #eventdata{}.
%%                     private Mnesia record
-record(eventdata, {
	  presentity,	%% term(), presentity this data belongs to ({user, User} | {address, AddressStr} | ...)
	  entity_tag,	%% term(), event package record reference
	  package,	%% string(), event package name
	  expires,	%% never | integer(), util:timestamp() time when this record expires
	  flags,	%% list() of {Key, Value} attributes
	  data		%% term(), event package data
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
%% @doc     Create the 'eventdata' table on Servers.
%% @private
%% @end
%%--------------------------------------------------------------------
create(Servers) when is_list(Servers) ->
    mnesia:create_table(eventdata, [{attributes, record_info(fields, eventdata)},
				    {type, bag},
				    {disc_copies, Servers},
				    {index, [entity_tag]}    %% key = presentity
				   ]).


%%--------------------------------------------------------------------
%% @spec    (PackageS, Presentity, ETag, Expires, Flags, Data) ->
%%            transaction_result()
%%
%%            PackageS   = string() "event package"
%%            Presentity = {user, User} | {address, AddrStr}
%%            ETag       = term() "event package record reference"
%%            Expires    = integer() | never
%%            Flags      = [{Key, Value}] "(for future use)"
%%            Data       = term() "event package data"
%%
%% @doc     Create a new entry in the database.
%% @end
%%--------------------------------------------------------------------
insert(Package, Presentity, ETag, Expires, Flags, Data) when is_integer(Expires), Expires < ?MIN_ABSOLUTE_TIME ->
    erlang:error("time supplied to insert/6 should be absolute, not relative",
		 [Package, Presentity, ETag, Expires, Flags, Data]);
insert(Package, Presentity, ETag, Expires, Flags, Data) when is_list(Package), is_tuple(Presentity),
							     is_integer(Expires) orelse Expires == never,
							     is_list(Flags) ->
    db_util:insert_record(#eventdata{presentity	= Presentity,
				     entity_tag	= ETag,
				     package    = Package,
				     expires	= Expires,
				     flags	= Flags,
				     data	= Data
			       }).

%%--------------------------------------------------------------------
%% @spec
%%    (Package, Presentity, ETag, NewETag, Expires, Flags, Data) ->
%%            ok | nomatch | error
%%
%%            PackageS   = string() "event package"
%%            Presentity = {user, User} | {address, AddrStr}
%%            ETag       = term() "event package record reference"
%%            NewETag    = term() "NEW event package record reference"
%%            Expires    = integer() | never
%%            Flags      = [{Key, Value}] "(for future use)"
%%            Data       = term() "event package data"
%%
%% @doc     Update an existing element that matches Package,
%%          Presentity and ETag.
%% @end
%%--------------------------------------------------------------------
update(Package, Presentity, ETag, NewETag, Expires, Flags, Data) when is_integer(Expires),
								      Expires < ?MIN_ABSOLUTE_TIME ->
    erlang:error("time supplied to update/7 should be absolute, not relative",
		 [Package, Presentity, ETag, NewETag, Expires, Flags, Data]);
update(Package, Presentity, ETag, NewETag, Expires, Flags, Data) when is_list(Package), is_tuple(Presentity),
								      is_integer(Expires) orelse
								      Expires == never, is_list(Flags) ->
    Now = util:timestamp(),

    F = fun() ->
		L = mnesia:read({eventdata, Presentity}),
		L2 = [E1 || E1 <- L, E1#eventdata.entity_tag == ETag
				andalso E1#eventdata.package == Package
				andalso (E1#eventdata.expires >= Now orelse
					 E1#eventdata.expires == never)],
		case L2 of
		    [E] ->
			%% Exactly one entry found, delete it and then write a new entry
			ok = mnesia:delete_object(E),
			NewE = E#eventdata{entity_tag = NewETag,
					   expires    = Expires,
					   flags      = Flags,
					   data       = Data
					  },
			ok = mnesia:write(NewE);
		    [] ->
			mnesia:abort(nomatch);
		    _ ->
			mnesia:abort(error)
		end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->   ok;
	{aborted, Res} -> Res
    end.


%%--------------------------------------------------------------------
%% @spec    (Presentity, ETag, NewExpires, NewETag) ->
%%            ok | nomatch | error
%%
%%            Presentity = {user, User} | {address, AddrStr}
%%            ETag       = term() "event package record reference"
%%            Expires    = integer()
%%            Flags      = [{Key, Value}] "(for future use)"
%%            Data       = term() "event package data"
%%
%% @doc     Update the expires (and optionally entity_tag) element(s)
%%          of a database record.
%% @end
%%--------------------------------------------------------------------
refresh_presentity_etag(Presentity, ETag, NewExpires, NewETag) when is_integer(NewExpires),
								    NewExpires < ?MIN_ABSOLUTE_TIME ->
    erlang:error("time supplied to refresh_presentity_etag/4 should be absolute, not relative",
		 [Presentity, ETag, NewExpires, NewETag]);
refresh_presentity_etag(Presentity, ETag, NewExpires, NewETag) when is_tuple(Presentity), (is_integer(NewExpires) orelse
								    NewExpires == never) ->
    Now = util:timestamp(),

    F = fun() ->
		L = mnesia:read({eventdata, Presentity}),
		L2 = [E1 || E1 <- L, E1#eventdata.entity_tag == ETag
				andalso (E1#eventdata.expires >= Now orelse
					 E1#eventdata.expires == never)],
		case L2 of
		    [E] ->
			%% Exactly one entry found, delete it and then write a new entry
			ok = mnesia:delete_object(E),
			NewE = E#eventdata{entity_tag = NewETag,
					   expires    = NewExpires
					  },
			ok = mnesia:write(NewE);
		    [] ->
			mnesia:abort(nomatch)
		end
	end,
    case mnesia:transaction(F) of
	{atomic, ok} ->   ok;
	{aborted, Res} -> Res
    end.

%%--------------------------------------------------------------------
%% @spec    () -> [#eventdata{}]
%%
%% @doc     List all eventdata records in the database.
%% @end
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(eventdata).


%%--------------------------------------------------------------------
%% @spec    (Presentity) ->
%%            {ok, List} |
%%            nomatch
%%
%%            Presentity = {user, User} | {address, AddrStr}
%%
%%            List = [#evendata_dbe{}]
%%
%% @doc     Fetch all entrys for a presentity.
%% @end
%%--------------------------------------------------------------------
fetch_using_presentity(Presentity) when is_tuple(Presentity) ->
    F = fun() ->
		mnesia:read({eventdata, Presentity})
	end,
    {atomic, Entrys} = mnesia:transaction(F),
    case make_fetch_result(Entrys) of
	{ok, E} ->
	    {ok, E};
	Res ->
	    Res
    end.


%%--------------------------------------------------------------------
%% @spec    (Presentity, ETag) ->
%%            {ok, Entry} |
%%            nomatch
%%
%%            Presentity = {user, User} | {address, AddrStr}
%%            ETag       = term()
%%
%%            Entry = #evendata_dbe{}
%%
%% @doc     Fetch a single record, matching both Presentity and ETag.
%% @end
%%--------------------------------------------------------------------
fetch_using_presentity_etag(Presentity, ETag) when is_tuple(Presentity) ->
    F = fun() ->
		mnesia:read({eventdata, Presentity})
	end,
    {atomic, Entrys} = mnesia:transaction(F),
    L1 = [E || E <- Entrys, E#eventdata.entity_tag == ETag],
    case make_fetch_result(L1) of
	{ok, [Entry]} ->
	    {ok, Entry};
	nomatch ->
	    nomatch
    end.


%%--------------------------------------------------------------------
%% @spec    (Presentity) -> transaction_result()
%%
%%            Presentity = {user, User} | {address, AddrStr}
%%
%% @doc     Delete all entrys for a presentity.
%% @end
%%--------------------------------------------------------------------
delete_using_presentity(Presentity) when is_tuple(Presentity) ->
    db_util:delete_with_key(eventdata, Presentity).


%%--------------------------------------------------------------------
%% @spec    (Presentity, ETag) -> transaction_result()
%%
%%            Presentity = {user, User} | {address, AddrStr}
%%            ETag       = term()
%%
%% @doc     Delete all eventdata entrys matching a Presentity and
%%          ETag.
%% @end
%%--------------------------------------------------------------------
delete_using_presentity_etag(Presentity, ETag) when is_tuple(Presentity) ->
    F = fun() ->
		L = mnesia:read({eventdata, Presentity}),
		[mnesia:delete_object(E) || E <- L, E#eventdata.entity_tag == ETag]
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% @spec    () ->
%%            {ok, Num} | error
%%
%%            Num = integer() "deleted entrys"
%%
%% @doc     Delete all expired eventdata entrys.
%% @end
%%--------------------------------------------------------------------
delete_expired() ->
    Now = util:timestamp(),

    F = fun() ->
		L = mnesia:select(eventdata, [{#eventdata{_ = '_'},
					       [{'<', {element, #eventdata.expires, '$_'}, Now}],
					       ['$_']
					      }]
				 ),

		[mnesia:delete_object(E) || E <- L],
		length(L)
	end,

    case mnesia:transaction(F) of
	{atomic, L} ->
	    {ok, L};
	Unknown ->
	    logger:log(error, "Eventdata: Got unknown result from Mnesia when deleting expired entrys : ~p",
		       [Unknown]),
	    error
    end.

%%--------------------------------------------------------------------
%% @spec    (MnesiaEvent) ->
%%            {ok, PackageS, List} |
%%            none                 |
%%            error
%%
%%            MnesiaEvent = tuple() "Mnesia 'subscribe' event data"
%%
%%            PackageS = string()
%%            List     = [#evendata_dbe{}]
%%
%% @doc     Return eventdata_dbe records from Mnesia write or
%%          delete_object events.
%% @private
%% @end
%%--------------------------------------------------------------------
decode_mnesia_change_event({Type, Data, _TId}) when (Type == write orelse Type == delete_object),
						    is_record(Data, eventdata) ->
    %% use make_fetch_result2 to also get Res about expired events (i.e. delete_object)
    case make_fetch_result2([Data], _FakeNow = 0) of
	{ok, Res} ->
	    {ok, Data#eventdata.package, Res};
	nomatch ->
	    none
    end;
decode_mnesia_change_event({_Type, Data, _Tid}) when is_record(Data, eventdata) ->
    %% not 'write' or 'delete_object' - we don't care
    none;
decode_mnesia_change_event(_Unknown) ->
    error.


%%--------------------------------------------------------------------
%% @spec    () ->
%%            {ok, RecordInfo, Fun}
%%
%%            RecordInfo = [atom()] "record field names"
%%            Fun        = function()
%%
%% @doc     Return a function that table_update uses to transform this
%%          table. We do it this way to not have to export the
%%          private record 'eventdata'.
%% @private
%% @end
%%--------------------------------------------------------------------
get_transform_fun() ->
    %% Table = eventdata,
    %% On update, don't forget : put({Table, update}, true),
    F = fun
	    (E) when is_record(E, eventdata) ->
		%% nothing to update
		E
	end,
    {ok, record_info(fields, eventdata), F}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Entrys) ->
%%            {ok, Entrys} |
%%            nomatch
%%
%%            Entrys = [#eventdata{}]
%%
%%            Entrys = [#eventdata_dbe{}]
%%
%% @doc     Turn a list of 'eventdata' records into a list of
%%          'eventdata_dbe' records. 'eventdata' records are our
%%          internal data format for storing event data in Mnesia,
%%          'eventdata_dbe' are potentially different records that
%%          outside modules might use.
%% @end
%%--------------------------------------------------------------------
make_fetch_result(Entrys) ->
    Now = util:timestamp(),
    make_fetch_result2(Entrys, Now).

make_fetch_result2([], _Now) ->
    nomatch;
make_fetch_result2(Entrys, Now) when is_list(Entrys), is_integer(Now) ->
    %% Transform from this modules private mnesia database record (eventdata) to
    %% the interface record type eventdata_dbe
    Transform = fun(E) when is_record(E, eventdata) ->
			#eventdata_dbe{presentity	= E#eventdata.presentity,
				       etag		= E#eventdata.entity_tag,
				       expires		= E#eventdata.expires,
				       flags		= E#eventdata.flags,
				       data		= E#eventdata.data
				      }
		end,
    Converted = [Transform(E) || E <- Entrys, E#eventdata.expires >= Now orelse
				     E#eventdata.expires == never],
    case Converted of
	[] ->
	    nomatch;
	_ ->
	    {ok, Converted}
    end.


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
    case catch mnesia:table_info(eventdata, attributes) of
	Attrs when is_list(Attrs) ->
	    ok;
	{'EXIT', {aborted, {no_exists, eventdata, attributes}}} ->
	    %% Create table 'eventdata' in RAM for use in the tests here
	    {atomic, ok} =
		mnesia:create_table(eventdata, [{attributes, record_info(fields, eventdata)},
						{index, [entity_tag]},
						{type, bag}
					  ])
    end.
