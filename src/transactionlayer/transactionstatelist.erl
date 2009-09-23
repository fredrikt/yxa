%%%-------------------------------------------------------------------
%%% File    : transactionstatelist.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Transaction layer's list-module. A mix of code
%%%           manipulating a list of transactions, and code to locate
%%%           transactions in all strange ways the SIP RFC3261
%%%           prescribe.
%%%
%%% @since    05 Feb 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
%%%-------------------------------------------------------------------
-module(transactionstatelist).

%%--------------------------------------------------------------------
%% External exports - note that these functions should _only_be_used_
%% by the transaction layer modules. This module is INTERNAL to the
%% transaction layer.
%%--------------------------------------------------------------------
-export([
	 add_client_transaction/4,
	 add_server_transaction/3,
	 delete_using_entrylist/1,
	 empty/0,
	 extract/2,
	 debugfriendly/0,
	 debugfriendly/1,
	 monitor_format/1,
	 get_client_transaction/2,
	 get_server_transaction_using_request/1,
	 get_elem_using_pid/2,
	 get_entrylist_using_pid/1,
	 get_expired/0,
	 set_pid/2,
	 set_appdata/2,
	 set_response_to_tag/2,
	 set_result/2,
	 update_transactionstate/1,
	 get_length/0,
	 get_all_entries/0,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("transactionstatelist.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type tables() = #tables{}.
%%                  no description
-record(tables, {ref_to_t,
		 pid_to_ref,
		 ack_to_ref,
		 typeid_to_ref,
		 statistics
		}).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
%% Transaction expire is just a safety-net thing. All transactions should
%% terminate by themselves long before this.
-define(TRANSACTION_EXPIRE, 900).

%% we generally use named tables in this module, but to facilitate testing some
%% functions (the ones tested typically) provide the opportunity to supply a #tables
%% record instead.
-define(DEFAULT_TABLES, #tables{ref_to_t      = transactionstate_ref_to_t,
				pid_to_ref    = transactionstate_pid_to_ref,
				ack_to_ref    = transactionstate_ack_to_ref,
				typeid_to_ref = transactionstate_typeid_to_ref,
				statistics    = yxa_statistics
			       }
	).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Method, Branch, Pid, Desc) ->
%%            ok               |
%%            error            |
%%            {duplicate, Dup}
%%
%%            Method = string()
%%            Branch = string()
%%            Pid    = pid()
%%            Desc   = string() "description of transaction"
%%
%%            Dup = #transactionstate{}
%%
%% @doc     Add a new client transaction state entry to TStateList.
%% @end
%%--------------------------------------------------------------------
add_client_transaction(Method, Branch, Pid, Desc)
  when is_list(Method), is_list(Branch), is_pid(Pid), is_list(Desc) ->
    add_client_transaction(?DEFAULT_TABLES, Method, Branch, Pid, Desc).

add_client_transaction(Tables, Method, Branch, Pid, Desc)
  when is_list(Method), is_list(Branch), is_pid(Pid), is_list(Desc) ->
    Id = {Branch, Method},
    add(Tables, client, Id, none, Pid, Desc).

%%--------------------------------------------------------------------
%% @spec    (Request, Pid, Desc) ->
%%            ok               |
%%            error            |
%%            {duplicate, Dup}
%%
%%            Request = #request{}
%%            Pid     = pid()
%%            Desc    = string() "description of transaction"
%%
%%            Dup = #transactionstate{}
%%
%% @doc     Add a new transaction state entry to SocketList.
%% @end
%%--------------------------------------------------------------------
add_server_transaction(Request, Pid, Desc) 
  when is_record(Request, request), is_pid(Pid), is_list(Desc) ->
    add_server_transaction(?DEFAULT_TABLES, Request, Pid, Desc).

add_server_transaction(Tables, Request, Pid, Desc)
  when is_record(Tables, tables), is_record(Request, request), is_pid(Pid), is_list(Desc) ->
    case sipheader:get_server_transaction_id(Request) of
	error ->
	    logger:log(error, "transactionstatelist: Could not get server transaction id for request"),
	    error;
	Id ->
	    AckId = case Request#request.method of
		        "INVITE" ->
			    %% For INVITE, we must store an extra Id to match ACK to the INVITE transaction.
			    %% We must do this even for RFC3261 INVITE since the ACK might arrive through
			    %% another proxy that is not RFC3261 compliant.
			    sipheader:get_server_transaction_ack_id_2543(Request);
			_ ->
			    none
		    end,
	    add(Tables, server, Id, AckId, Pid, Desc)
    end.

%%--------------------------------------------------------------------
%% @spec    () ->
%%            {ok, TStateList}
%%
%%            TStateList = #transactionstatelist{}
%%
%% @doc     Get an empty transactionstatelist.
%% @end
%%--------------------------------------------------------------------
empty() ->
    empty(?DEFAULT_TABLES).

empty(Tables) when is_record(Tables, tables) ->
    Ref2t = ets:new(Tables#tables.ref_to_t, [public, set, named_table]),
    Pid2ref = ets:new(Tables#tables.pid_to_ref, [public, bag, named_table]),
    Ack2ref = ets:new(Tables#tables.ack_to_ref, [public, set, named_table]),
    TypeId2ref = ets:new(Tables#tables.typeid_to_ref, [public, set, named_table]),
    NewTables = Tables#tables{ref_to_t      = Ref2t,
			      pid_to_ref    = Pid2ref,
			      ack_to_ref    = Ack2ref,
			      typeid_to_ref = TypeId2ref
			     },
    {ok, #transactionstatelist{tables = NewTables}}.


%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            Entry |
%%            error |
%%            none
%%
%%            Request = #request{}
%%
%%            Entry = #transactionstate{}
%%
%% @doc     Given a (newly received) request (resend or ACK), try to
%%          locate an already existing server transaction handler.
%% @end
%%--------------------------------------------------------------------
get_server_transaction_using_request(Request) when is_record(Request, request) ->
    get_server_transaction_using_request(?DEFAULT_TABLES, Request).

get_server_transaction_using_request(Tables, Request) when is_record(Tables, tables), is_record(Request, request) ->
    case sipheader:get_server_transaction_id(Request) of
	is_2543_ack ->
	    get_server_transaction_ack_2543(Tables, Request);
	error ->
	    logger:log(error, "Transaction state list: Could not get server transaction for request"),
	    error;
	Id ->
	    case get_elem(Tables, server, Id) of
		none when Request#request.method == "ACK" ->
		    %% If the UAC is 2543 compliant, but there is a 3261 compliant proxy between UAC and us,
		    %% the 3261 proxy will possibly generate another branch for the ACK than the INVITE
		    %% because the ACK might have a To-tag. If this happens, we must use 2543 methods to find
		    %% the transaction even though RFC3261 17.2.3 only says we should do this if the ACK
		    %% received does NOT have the 3261 magic cookie in the branch parameter.
		    %%
		    %% Bug in RFC3261, see http://bugs.sipit.net/sipwg/show_bug.cgi?id=755
		    %%
		    logger:log(debug, "Transaction state list: Found no matching server "
			       "transaction for ACK using RFC3261 methods, trying RFC2543 too"),
		    get_server_transaction_ack_2543(Tables, Request);
		none ->
		    none;
		Res when is_record(Res, transactionstate) ->
		    Res
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Tables, Request) ->
%%            Entry |
%%            error |
%%            none
%%
%%            Tables  = #tables{}
%%            Request = #request{}
%%
%%            Entry = #transactionstate{}
%%
%% @doc     Given an (newly received) ACK that we have already
%%          determined has no RFC3261 Via branch parameter, try to
%%          locate an already existing server transaction handler
%%          using the painful old RFC2543 backwards compatible code.
%% @end
%%--------------------------------------------------------------------
get_server_transaction_ack_2543(Tables, Request) when is_record(Request, request) ->
    %% ACK requests are matched to transactions differently if they are not received from
    %% an RFC3261 compliant device, see RFC3261 17.2.3
    case sipheader:get_server_transaction_ack_id_2543(Request) of
	error ->
	    logger:log(error, "Transaction state list: Could not get server transaction RFC2543 ack-id for request"),
	    error;
	Id ->
	    ToTag = sipheader:get_tag(keylist:fetch('to', Request#request.header)),
	    case get_elem_ackid(Tables, Id, ToTag) of
		none ->
		    logger:log(debug, "Transaction state list: ACK request does not match any existing transaction"),
		    %% If this ever happens, this extra debug output will probably be crucial to
		    %% diagnose why. However, it can't be enabled per default in case there are
		    %% a _lot_ of transactions on your server.
		    %%logger:log(debug, "Transaction state list: Extra debug: Looked for server transaction with ack-id ~p "
		    %%	       "AND to-tag ~p in list :~n~p", [Id, ToTag, debugfriendly()]),
		    none;
		Res when is_record(Res, transactionstate) ->
		    Res
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Method, Branch) ->
%%            Entry |
%%            none
%%
%%            Method = string()
%%            Branch = string()
%%
%%            Entry = #transactionstate{}
%%
%% @doc     Look for a client transaction using a method and branch.
%%          Notes : This code is only used in transcationlayer before
%%          adding a new client transaction. Since we have only a
%%          single transactionlayer process per node, this is not a
%%          race, but it would be better to just look for duplicates
%%          in add_client_transaction().
%% @end
%%--------------------------------------------------------------------
get_client_transaction(Method, Branch) ->
    get_client_transaction(?DEFAULT_TABLES, Method, Branch).

get_client_transaction(Tables, Method, Branch) ->
    Id = {Branch, Method},
    get_elem(Tables, client, Id).

%%--------------------------------------------------------------------
%% @spec    (Pid) ->
%%            EntryList |
%%            none
%%
%%            Pid = pid()
%%
%%            EntryList = [#transactionstate{}]
%%
%% @doc     Find all elements in a transactionstatelist who have a
%%          matching pid. Return a plain list of those. Use with
%%          care.
%% @end
%%--------------------------------------------------------------------
get_entrylist_using_pid(Pid) when is_pid(Pid) ->
    get_entrylist_using_pid(?DEFAULT_TABLES, Pid).

get_entrylist_using_pid(Tables, Pid) when is_pid(Pid) ->
    case get_using_pid2(Tables, Pid) of
	[] ->
	    none;
	L when is_list(L) ->
	    L
    end.

%%--------------------------------------------------------------------
%% @spec    (Pid, TStateList) ->
%%            TransactionState |
%%            {error, Reason}  |
%%            none
%%
%%            Pid        = pid()
%%            TStateList = #transactionstatelist{}
%%
%%            TransactionState = #transactionstate{}
%%            Reason           = string()
%%
%% @doc     The same as get_list_using_pid/2 except this function
%%          returns {error, Reason} if more than one record has a
%%          matching pid.
%% @end
%%--------------------------------------------------------------------
get_elem_using_pid(Pid, TStateList) when is_pid(Pid), is_record(TStateList, transactionstatelist) ->
    get_elem_using_pid(?DEFAULT_TABLES, Pid, TStateList).

get_elem_using_pid(Tables, Pid, TStateList) when is_pid(Pid), is_record(TStateList, transactionstatelist) ->
    %%case get_using_pid2(Pid, TStateList#transactionstatelist.list, []) of
    case get_using_pid2(Tables, Pid) of
	[] ->
	    none;
	[Elem] when is_record(Elem, transactionstate) ->
	    Elem;
	L when is_list(L) ->
	    {error, "more than one transactionstate found"}
    end.

%%--------------------------------------------------------------------
%% @spec    (Pid) ->
%%            Entrys |
%%            []
%%
%%            Pid = pid()
%%
%%            Entrys = [#transactionstate{}]
%%
%% @doc     Internal function that returns a plain list (not a
%%          transactionstatelist) of all the elements of a given
%%          transactionstatelist who have a matching pid.
%% @end
%%--------------------------------------------------------------------
get_using_pid2(Tables, Pid)  ->
    RefList = ets:lookup(Tables#tables.pid_to_ref, Pid),
    fetch_using_ref_tuples(Tables, RefList).

%%--------------------------------------------------------------------
%% @spec    (Type, Id) ->
%%            Entry |
%%            none
%%
%%            Type = client | server
%%            Id   = term()
%%
%%            Entry = #transactionstate{}
%%
%% @doc     Find a single element from a transactionstatelist which
%%          has a matching type and id.
%% @end
%%--------------------------------------------------------------------
get_elem(Tables, Type, Id) when Type == server orelse Type == client ->
    TId = {Type, Id},
    RefList = ets:lookup(Tables#tables.typeid_to_ref, TId),
    case fetch_using_ref_tuples(Tables, RefList) of
	[] -> none;
	[IdMatch] ->
	    IdMatch
    end.


%%--------------------------------------------------------------------
%% @spec    (Tables, AckId, ToTag) ->
%%            Entry |
%%            none
%%
%%            Tables = #tables{}
%%            AckId  = term()
%%            ToTag  = string() | none
%%
%%            Entry = #transactionstate{}
%%
%% @doc     Find a single element from a transactionstatelist which
%%          has a matching ack_id and response_to_tag. Only look at
%%          server transactions (type == server). This is used when
%%          matching RFC2543 ACK's to non-2xx responses to INVITEs.
%% @end
%%--------------------------------------------------------------------
get_elem_ackid(Tables, AckId, ToTag) ->
    RefList = ets:lookup(Tables#tables.ack_to_ref, AckId),
    AckIdMatches = fetch_using_ref_tuples(Tables, RefList),
    filter_server_totag_matches(ToTag, AckIdMatches).

filter_server_totag_matches(_ToTag, []) ->
    none;
filter_server_totag_matches(ToTag, [#transactionstate{type=server, response_to_tag=ToTag}=H | _T]) ->
    %% We have a match
    H;
filter_server_totag_matches(ToTag, [#transactionstate{type=server}=H | T]) ->
    logger:log(debug, "Transaction state list: Found a transaction with matching ACK-Id, "
	       "but the to-tag is not ~p (it is ~p) :~n~p",
	       [ToTag, H#transactionstate.response_to_tag, debugfriendly(H)]),
    filter_server_totag_matches(ToTag, T);
filter_server_totag_matches(ToTag, [H|T]) when is_record(H, transactionstate) ->
    filter_server_totag_matches(ToTag, T).

%%--------------------------------------------------------------------
%% @spec    () ->
%%            none | Expired
%%
%%            Expired = [#transactionstate{}]
%%
%% @doc     Get all transactionstatelist entrys that have expired.
%% @end
%%--------------------------------------------------------------------
get_expired() ->
    Now = util:timestamp(),
    L = get_all_entries(),
    case get_expired2(L, Now, []) of
	[] -> none;
	Res -> {ok, Res}
    end.

%% part of get_expired/0 - filter out all transactionstate entrys with expire =< now
get_expired2([#transactionstate{expire=Expire}=H | T], Now, Res) when Expire =< Now ->
    %% match
    get_expired2(T, Now, [H | Res]);
get_expired2([H | T], Now, Res) when is_record(H, transactionstate) ->
    %% no match
    get_expired2(T, Now, Res);
get_expired2([], _Now, Res) ->
    Res.

%%--------------------------------------------------------------------
%% @spec    (Fields, TState) ->
%%            Values
%%
%%            Fields = [pid|appdata|response_to_tag]
%%            TState = #transactionstate{}
%%
%%            Values = list()
%%
%% @doc     Return one or more values from a transactionstate record.
%% @end
%%--------------------------------------------------------------------
extract(Values, TState) when is_record(TState, transactionstate) ->
    extract(Values, TState, []).

extract([], TState, Res) when is_record(TState, transactionstate) ->
    Res;
extract([pid | T], TState, Res) when is_record(TState, transactionstate) ->
    extract(T, TState, lists:append(Res, [TState#transactionstate.pid]));
extract([appdata | T], TState, Res) when is_record(TState, transactionstate) ->
    extract(T, TState, lists:append(Res, [TState#transactionstate.appdata]));
extract([response_to_tag | T], TState, Res) when is_record(TState, transactionstate) ->
    extract(T, TState, lists:append(Res, [TState#transactionstate.response_to_tag])).

%%--------------------------------------------------------------------
%% @spec    (TState, Value) ->
%%            NewTState
%%
%%            TState = #transactionstate{}
%%            Value  = pid()
%%
%%            NewTState = #transactionstate{}
%%
%% @doc     Set pid in a transactionstate record()
%% @end
%%--------------------------------------------------------------------
set_pid(TState, Value) when is_record(TState, transactionstate), is_pid(Value); Value == none ->
    TState#transactionstate{pid = Value}.

%%--------------------------------------------------------------------
%% @spec    (TState, Value) ->
%%            NewTState
%%
%%            TState = #transactionstate{}
%%            Value  = term()
%%
%%            NewTState = #transactionstate{}
%%
%% @doc     Set appdata in a transactionstate record()
%% @end
%%--------------------------------------------------------------------
set_appdata(TState, Value) when is_record(TState, transactionstate) ->
    TState#transactionstate{appdata = Value}.

%%--------------------------------------------------------------------
%% @spec    (TState, Value) ->
%%            NewTState
%%
%%            TState = #transactionstate{}
%%            Value  = string()
%%
%%            NewTState = #transactionstate{}
%%
%% @doc     Set response_to_tag in a transactionstate record()
%% @end
%%--------------------------------------------------------------------
set_response_to_tag(TState, Value) when is_record(TState, transactionstate) ->
    TState#transactionstate{response_to_tag = Value}.

%%--------------------------------------------------------------------
%% @spec    (TState, Value) ->
%%            NewTState
%%
%%            TState = #transactionstate{}
%%            Value  = string()
%%
%%            NewTState = #transactionstate{}
%%
%% @doc     Set result in a transactionstate record()
%% @end
%%--------------------------------------------------------------------
set_result(TState, Value) when is_record(TState, transactionstate), is_list(Value) ->
    TState#transactionstate{result = Value}.

%%--------------------------------------------------------------------
%% @spec    (EntryList) ->
%%            {ok, NumDeleted}
%%
%%            EntryList = [#transactionstate{}]
%%
%%            NumDeleted = integer() "number of records deleted"
%%
%% @doc     Delete all entrys in EntryList.
%% @end
%%--------------------------------------------------------------------
delete_using_entrylist(EntryList) when is_list(EntryList) ->
    delete_using_entrylist(?DEFAULT_TABLES, EntryList).

delete_using_entrylist(Tables, EntryList) when is_list(EntryList) ->
    ok = del_entrys(Tables, EntryList),
    NumEntrys = length(EntryList),
    ets:update_counter(Tables#tables.statistics, {transactionlayer, transactions}, 0 - NumEntrys),
    {ok, NumEntrys}.

%%--------------------------------------------------------------------
%% @spec    (TState) ->
%%            NewTStateList
%%
%%            TState = #transcationstate{}
%%
%%            NewTStateList = #transactionstatelist{}
%%
%% @doc     Look in TStateList for an entry matching (using the unique
%%          reference) the TState entry, and replace it with the
%%          (presumably) updated TState entry.
%% @end
%%--------------------------------------------------------------------
update_transactionstate(TState) when is_record(TState, transactionstate) ->
    update_transactionstate(?DEFAULT_TABLES, TState).

update_transactionstate(Tables, TState) when is_record(TState, transactionstate) ->
    Ref = TState#transactionstate.ref,
    true = ets:insert(Tables#tables.ref_to_t, {Ref, TState}),
    ok.

%%--------------------------------------------------------------------
%% @spec    () ->
%%            Length
%%
%%            Length = integer()
%%
%% @doc     Returns the length of the list record element of a
%%          transactionstatelist.
%% @end
%%--------------------------------------------------------------------
get_length() ->
    get_length(?DEFAULT_TABLES).

get_length(Tables) ->
    ets:info(Tables#tables.ref_to_t, size).

debugfriendly() ->
    %% no list supplied, request full dump
    L = get_all_entries(),
    debugfriendly2(L, []).

%%--------------------------------------------------------------------
%% @spec    (TStateList) ->
%%            Data
%%
%%            TStateList = #transactionstatelist{}
%%
%%            Data = term()
%%
%% @doc     Return information about the elements in an
%%          transactionstatelist record in a format that is suitable
%%          for logging using ~p.
%% @end
%%--------------------------------------------------------------------
debugfriendly([]) ->
    [];
debugfriendly(TStateList) when is_record(TStateList, transactionstatelist) ->
    debugfriendly2(TStateList#transactionstatelist.list, []);
debugfriendly(TState) when is_record(TState, transactionstate) ->
    debugfriendly2([TState], []);
debugfriendly([H|_] = TStateList) when is_list(TStateList), is_record(H, transactionstate) ->
    debugfriendly2(TStateList, []).

debugfriendly2([], Res) ->
    lists:reverse(Res);
debugfriendly2([H | T], Res) when is_record(H, transactionstate) ->
    %% The In elements are in the reverse order of what they will be
    %% after debugfriendly_non_empty()
    In = [{id, H#transactionstate.id}, {description, H#transactionstate.description},
	  {type, H#transactionstate.type}, {ref, H#transactionstate.ref}],
    %% These are NOT in reverse order
    Out = debugfriendly_non_empty([pid, ack_id, response_to_tag, appdata, branches, result], H, In),
    debugfriendly2(T, [Out | Res]).

debugfriendly_non_empty([], _R, Res) ->
    lists:reverse(Res);
%% result, not empty
debugfriendly_non_empty([result | T], R, Res)
  when R#transactionstate.result /= undefined, R#transactionstate.result /= none ->
    debugfriendly_non_empty(T, R, [{result, R#transactionstate.result} | Res]);
%% pid, not empty
debugfriendly_non_empty([pid | T], R, Res)
  when R#transactionstate.pid /= undefined, R#transactionstate.pid /= none ->
    debugfriendly_non_empty(T, R, [{pid, R#transactionstate.pid} | Res]);
%% response_to_tag, not empty
debugfriendly_non_empty([response_to_tag | T], R, Res)
  when R#transactionstate.response_to_tag /= undefined, R#transactionstate.response_to_tag /= none ->
    debugfriendly_non_empty(T, R, [{response_to_tag, R#transactionstate.response_to_tag} | Res]);
%% ack_id, not empty
debugfriendly_non_empty([ack_id | T], R, Res)
  when R#transactionstate.ack_id /= undefined, R#transactionstate.ack_id /= none ->
    debugfriendly_non_empty(T, R, [{ack_id, R#transactionstate.ack_id} | Res]);
%% appdata, not empty
debugfriendly_non_empty([appdata | T], R, Res)
  when R#transactionstate.appdata /= undefined, R#transactionstate.appdata /= none ->
    debugfriendly_non_empty(T, R, [{appdata, R#transactionstate.appdata} | Res]);
%% branches (length), not zero
debugfriendly_non_empty([branches | T], R, Res)
  when length(R#transactionstate.stateless_response_branches) /= 0 ->
    debugfriendly_non_empty(T, R, [{stateless_response_branches, length(R#transactionstate.stateless_response_branches)} | Res]);
%% empty, or unknown record field
debugfriendly_non_empty([_ | T], R, Res) ->
    debugfriendly_non_empty(T, R, Res).

monitor_format(TStateList) when is_record(TStateList, transactionstatelist) ->
    monitor_format2(TStateList#transactionstatelist.list, []);
monitor_format(TList) when is_list(TList) ->
    monitor_format2(TList, []).

monitor_format2([], Res) ->
    lists:reverse(Res);
monitor_format2([H|T], Res) when record(H, transactionstate) ->
    Str =
	case H#transactionstate.result of
	    none ->
		H#transactionstate.description;
	    _ ->
		lists:flatten(lists:concat([H#transactionstate.description,
					    " (result: ",
					    H#transactionstate.result, ")"]))
	end,
    monitor_format2(T, [Str | Res]).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Tables, Type, Id, AckId, Pid, Desc) ->
%%            ok               |
%%            error            |
%%            {duplicate, Dup}
%%
%%            Tables = #tables{}
%%            Type   = client | server
%%            Id     = term()
%%            AckId  = term() | none
%%            Pid    = pid()
%%            Desc   = string() ""
%%
%%            Dup = #transactionstate{}
%%
%% @doc     Add a new transaction state entry to the ets tables.
%% @end
%%--------------------------------------------------------------------
add(Tables, Type, Id, AckId, Pid, Desc) when is_pid(Pid), is_atom(Type),
					     (Type == client orelse Type == server) ->
    Ref = make_ref(),
    TId = {Type, Id},
    case ets:insert_new(Tables#tables.typeid_to_ref, {TId, Ref}) of
	true ->
	    NewT = #transactionstate{ref = Ref,
				     type = Type,
				     id	= Id,
				     ack_id = AckId,
				     pid = Pid,
				     expire = util:timestamp() + ?TRANSACTION_EXPIRE,
	    			     description = Desc},
	    true = ets_insert_new(Tables#tables.ref_to_t, {Ref, NewT}),
	    true = ets:insert(Tables#tables.pid_to_ref, {Pid, Ref}),
	    case AckId of
		none -> true;
		_ -> true = ets_insert_new(Tables#tables.ack_to_ref, {AckId, Ref})
	    end,
	    %% Statistics
	    ets:update_counter(Tables#tables.statistics, {transactionlayer, transactions}, 1),
	    ok;
	false ->
	    case get_elem(Tables, Type, Id) of
		Dup when is_record(Dup, transactionstate) ->
		    logger:log(debug, "transactionstatelist: Asked to add transaction (handled by ~p) with duplicate Id ~p, "
			       "existing entry :~n~p~nReturning {duplicate, ...}",
			       [Pid, Id, debugfriendly(Dup)]),
		    {duplicate, Dup};
		_ ->
		    logger:log(error, "transactionstatelist: Insert failed, but duplicate entry not found!~nId : ~p", [Id]),
		    error
	    end
    end.

ets_insert_new(TName, Data) when is_atom(TName), is_tuple(Data) ->
    case ets:insert_new(TName, Data) of
	true ->
	    true;
	false ->
	    %% fetch colliding data as quickly as possible
	    {Key, _Value} = Data,
	    X = (catch ets:lookup(TName, Key)),

	    logger:log(error, "Transaction state list: Failed adding entry to ets table '~p'", [TName]),
	    logger:log(debug, "Transaction state list: Data that did not get added to ets table '~p' :~n~p",
		       [TName, Data]),
	    logger:log(debug, "Transaction state list: Colliding data in ets table '~p' :~n~p",
		       [TName, X]),
	    true
    end.

%% Fetch the transaction state and then remove all references to it from all our ets tables
del_entrys(_Tables, []) ->
    ok;
del_entrys(Tables, [H|T]) when is_record(H, transactionstate) ->
    Type = H#transactionstate.type,
    Id = H#transactionstate.id,
    TId = {Type, Id},
    true = ets:delete(Tables#tables.typeid_to_ref, TId),
    case H#transactionstate.ack_id of
	none -> true;
	AckId -> true = ets:delete(Tables#tables.ack_to_ref, AckId)
    end,
    true = ets:delete(Tables#tables.pid_to_ref, H#transactionstate.pid),
    true = ets:delete(Tables#tables.ref_to_t, H#transactionstate.ref),
    del_entrys(Tables, T).

fetch_using_ref_tuples(Tables, In) ->
    fetch_using_ref_tuples2(Tables, In, []).

fetch_using_ref_tuples2(_Tables, [], Res) ->
    Res;
fetch_using_ref_tuples2(Tables, [{_, Ref}|T], Res) ->
    case ets:lookup(Tables#tables.ref_to_t, Ref) of
	[{Ref, Entry}] ->
	    fetch_using_ref_tuples2(Tables, T, [Entry | Res]);
	_ ->
	    fetch_using_ref_tuples2(Tables, T, Res)
    end.

get_all_entries() ->
    get_all_entries(?DEFAULT_TABLES).

get_all_entries(Tables) ->
    F = fun(H, Acc) ->
		case H of
		    {_Ref, Entry} -> [Entry | Acc];
		    _ -> Acc
		end
	end,
    lists:foldl(F, [], ets:tab2list(Tables#tables.ref_to_t)).

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
    %% empty(Tables)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "empty/1 - 1.0"),
    %% create a bunch of tables
    TestTables_in = #tables{ref_to_t      = transactionstate_ref_to_t_TEST,
			    pid_to_ref    = transactionstate_pid_to_ref_TEST,
			    ack_to_ref    = transactionstate_ack_to_ref_TEST,
			    typeid_to_ref = transactionstate_typeid_to_ref_TEST,
			    statistics    = transactionstate_yxa_statistics_TEST
			   },

    {ok, TestTSList} = empty(TestTables_in),
    %% the statistics table is not created by empty/1
    ets:new(TestTables_in#tables.statistics, [public, set, named_table]),

    TestTables = TestTSList#transactionstatelist.tables,

    autotest:mark(?LINE, "empty/1 - 1.1"),
    %% check that ets tables were created, and are empty
    TestTablesList = tl(tuple_to_list(TestTables)),
    [true = test_check_is_empty_ets_table(TableName) || TableName <- TestTablesList],

    autotest:mark(?LINE, "empty/1 - 1.2"),
    %% create statistics ets-entrys
    true = ets:insert_new(TestTables_in#tables.statistics, {{transactionlayer, transactions}, 0}),

    %% add_server_transaction(Request, Pid, Desc)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add_server_transaction/4 - 1.0"),
    Message1Branch = "z9hG4bK-yxa-unittest-add_server_transaction3",
    Message1 =
	"INVITE sip:ft@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org;branch=" ++ Message1Branch ++ "\r\n"
	"From: Test <sip:test@example.org;tag=abc>\r\n"
	"To: Test <sip:test@example.org>\r\n"
	"Call-Id: unittest-add_server_transaction3@yxa.example.org\r\n"
	"CSeq: INVITE 1234\r\n"
	"\r\n",

    Request1 = sippacket:parse(Message1, none),

    autotest:mark(?LINE, "add_server_transaction/4 - 1.1"),
    ok = add_server_transaction(TestTables, Request1, self(), "TEST request 1"),

    autotest:mark(?LINE, "add_server_transaction/4 - 2"),
    %% try to add the same request again, should fail
    {duplicate, _} = add_server_transaction(TestTables, Request1, self(), "TEST request 1"),

    autotest:mark(?LINE, "get_server_transaction_using_request/2 - 1"),
    %% try to fetch the transaction
    #transactionstate{type = server,
		      id   = {Message1Branch, _TopVia1, "INVITE"}
		     } = get_server_transaction_using_request(TestTables, Request1),

    


    %% clean up
    [true = ets:delete(TableName) || TableName <- TestTablesList],

    ok.

test_check_is_empty_ets_table(TableName) when is_atom(TableName) ->
    case catch ets:tab2list(TableName) of
	[] ->
	    true;
	_ ->
	    Msg = io_lib:format("ETS table ~p is not empty", [TableName]),
	    {error, lists:flatten(Msg)}
    end.
