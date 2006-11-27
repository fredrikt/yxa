%%%-------------------------------------------------------------------
%%% File    : database_eventdata.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Eventdata database functions.
%%%
%%% Created :  3 Mar 2006 by Fredrik Thulin <ft@it.su.se>
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

%% private Mnesia record
-record(eventdata, {
	  presentity,	%% term(), presentity this data belongs to ({user, User} | {address, AddressStr} | ...)
	  entity_tag,	%% term(), event package record reference
	  package,	%% string(), event package name
	  expires,	%% never | integer(), util:timestamp() time when this record expires
	  flags,	%% list() of {Key, Value} attributes
	  data		%% term(), event package data
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
    mnesia:create_table(eventdata, [{attributes, record_info(fields, eventdata)},
				    {type, bag},
				    {disc_copies, Servers},
				    {index, [entity_tag]}    %% key = presentity
				   ]).


%%--------------------------------------------------------------------
%% Function: insert(PackageS, Presentity, ETag, Expires, Flags, Data)
%%           PackageS   = string(), event package
%%           Presentity = tuple(), {user, User} | {address, AddrStr}
%%           ETag       = term(), event package record reference
%%           Expires    = integer() | never
%%           Flags      = list() of {Key, Value} (for future use)
%%           Data       = term(), event package data
%% Descrip.: Create a new entry in the database.
%% Returns : mnesia transaction()
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
%% Function: update(Package, Presentity, ETag, NewETag, Expires,
%%                  Flags, Data)
%%           PackageS   = string(), event package
%%           Presentity = tuple(), {user, User} | {address, AddrStr}
%%           ETag       = term(), event package record reference
%%           NewETag    = term(), NEW event package record reference
%%           Expires    = integer() | never
%%           Flags      = list() of {Key, Value} (for future use)
%%           Data       = term(), event package data
%% Descrip.: Update an existing element that matches Package,
%%           Presentity and ETag.
%% Returns : ok | nomatch | error
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
%% Function: refresh_presentity_etag(Presentity, ETag, NewExpires,
%%                                  NewETag)
%%           Presentity = tuple(), {user, User} | {address, AddrStr}
%%           ETag       = term(), event package record reference
%%           Expires    = integer()
%%           Flags      = list() of {Key, Value} (for future use)
%%           Data       = term(), event package data
%% Descrip.: Update the expires (and optionally entity_tag) element(s)
%%           of a database record.
%% Returns : ok | nomatch | error
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
%% Function: list()
%% Descrip.: List all eventdata records in the database.
%% Returns : list() of eventdata record()
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(eventdata).


%%--------------------------------------------------------------------
%% Function: fetch_using_presentity(Presentity)
%%           Presentity = tuple(), {user, User} | {address, AddrStr}
%% Descrip.: Fetch all entrys for a presentity.
%% Returns : {ok, List} |
%%           nomatch
%%           List = list() of evendata_dbe record()
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
%% Function: fetch_using_presentity_etag(Presentity, ETag)
%%           Presentity = tuple(), {user, User} | {address, AddrStr}
%%           ETag       = term()
%% Descrip.: Fetch a single record, matching both Presentity and ETag.
%% Returns : {ok, Entry} |
%%           nomatch
%%           Entry = evendata_dbe record()
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
%% Function: delete_using_presentity(Presentity)
%%           Presentity = tuple(), {user, User} | {address, AddrStr}
%% Descrip.: Delete all entrys for a presentity.
%% Returns : mnesia:transaction()
%%--------------------------------------------------------------------
delete_using_presentity(Presentity) when is_tuple(Presentity) ->
    db_util:delete_with_key(eventdata, Presentity).


%%--------------------------------------------------------------------
%% Function: delete_using_presentity_etag(Presentity, ETag)
%%           Presentity = tuple(), {user, User} | {address, AddrStr}
%%           ETag       = term()
%% Descrip.: Delete all eventdata entrys matching a Presentity and
%%           ETag.
%% Returns : mnesia:transaction()
%%--------------------------------------------------------------------
delete_using_presentity_etag(Presentity, ETag) when is_tuple(Presentity) ->
    F = fun() ->
		L = mnesia:read({eventdata, Presentity}),
		[mnesia:delete_object(E) || E <- L, E#eventdata.entity_tag == ETag]
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function: delete_expired()
%% Descrip.: Delete all expired eventdata entrys.
%% Returns : {ok, Num} | error
%%           Num = integer(), deleted entrys
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
%% Function: decode_mnesia_change_event(MnesiaEvent)
%%           MnesiaEvent = tuple(), Mnesia 'subscribe' event data
%% Descrip.: Return eventdata_dbe records from Mnesia write or
%%           delete_object events.
%% Returns : {ok, PackageS, List} |
%%           none                 |
%%           error
%%           PackageS = string()
%%           List     = list() of evendata_dbe record()
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
%% Function: make_fetch_result(Entrys)
%%           Entrys = list() of eventdata record()
%% Descrip.: Turn a list of 'eventdata' records into a list of
%%           'eventdata_dbe' records. 'eventdata' records are our
%%           internal data format for storing event data in Mnesia,
%%           'eventdata_dbe' are potentially different records that
%%           outside modules might use.
%% Returns : {ok, Entrys} |
%%           nomatch
%%           Entrys = list() of eventdata_dbe record()
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
