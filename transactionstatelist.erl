%%%-------------------------------------------------------------------
%%% File    : transactionstatelist.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Transaction layer's list-module. A mix of code
%%%           manipulating a list of transactions, and code to locate
%%%           transactions in all strange ways the SIP RFC3261
%%%           prescribe.
%%%
%%% Created : 05 Feb 2004 by Fredrik Thulin <ft@it.su.se>
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
	 delete_expired/0,
	 empty/0,
	 extract/2,
	 debugfriendly/0,
	 debugfriendly/1,
	 monitor_format/1,
	 get_client_transaction/2,
	 get_server_transaction_using_request/1,
	 get_server_transaction_using_response/1,
	 get_elem_using_pid/2,
	 get_entrylist_using_pid/1,
	 set_pid/2,
	 set_appdata/2,
	 set_response_to_tag/2,
	 set_result/2,
	 update_transactionstate/1,
	 get_length/0,
	 get_all_entries/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("transactionstatelist.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(tables, {ref_to_t, pid_to_ref, ack_to_ref, typeid_to_ref}).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add_client_transaction(Method, Branch, Pid, Desc)
%%           Method     = string()
%%           Branch     = string()
%%           Pid        = pid()
%%           Desc       = string(), description of transaction
%% Descrip.: Add a new client transaction state entry to TStateList.
%% Returns : ok               |
%%           error            |
%%           {duplicate, Dup}
%%           Dup = transactionstate record()
%%--------------------------------------------------------------------
add_client_transaction(Method, Branch, Pid, Desc)
  when is_list(Method), is_list(Branch), is_pid(Pid), is_list(Desc) ->
    Id = {Branch, Method},
    add(client, Id, none, Pid, Desc).

%%--------------------------------------------------------------------
%% Function: add_server_transaction(Request, Pid, Desc)
%%           Request    = request record()
%%           Pid        = pid()
%%           Desc       = string(), description of transaction
%% Descrip.: Add a new transaction state entry to SocketList.
%% Returns : ok               |
%%           error            |
%%           {duplicate, Dup}
%%           Dup = transactionstate record()
%%--------------------------------------------------------------------
add_server_transaction(Request, Pid, Desc)
  when is_record(Request, request), is_pid(Pid), is_list(Desc) ->
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
	    add(server, Id, AckId, Pid, Desc)
    end.

%%--------------------------------------------------------------------
%% Function: empty()
%% Descrip.: Get an empty transactionstatelist.
%% Returns : TStateList = transactionstatelist record()
%%--------------------------------------------------------------------
empty() ->
    Ref2t = ets:new(transactionstate_ref_to_t, [protected, set, named_table]),
    Pid2ref = ets:new(transactionstate_pid_to_ref, [protected, bag, named_table]),
    Ack2ref = ets:new(transactionstate_ack_to_ref, [protected, set, named_table]),
    TypeId2ref = ets:new(transactionstate_typeid_to_ref, [protected, set, named_table]),
    Tables = #tables{ref_to_t=Ref2t, pid_to_ref=Pid2ref, ack_to_ref=Ack2ref, typeid_to_ref=TypeId2ref},
    #transactionstatelist{tables=Tables}.


%%--------------------------------------------------------------------
%% Function: get_server_transaction_using_request(Request)
%%           Request    = request record()
%% Descrip.: Given a (newly received) request (resend or ACK), try to
%%           locate an already existing server transaction handler.
%% Returns : Entry |
%%           error |
%%           none
%%           Entry = transactionstate record()
%%--------------------------------------------------------------------
get_server_transaction_using_request(Request) when is_record(Request, request) ->
    case sipheader:get_server_transaction_id(Request) of
	is_2543_ack ->
	    get_server_transaction_ack_2543(Request);
	error ->
	    logger:log(error, "Transaction state list: Could not get server transaction for request"),
	    error;
	Id ->
	    case get_elem(server, Id) of
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
		    get_server_transaction_ack_2543(Request);
		none ->
		    none;
		Res when is_record(Res, transactionstate) ->
		    Res
	    end
    end.

%%--------------------------------------------------------------------
%% Function: get_server_transaction_ack_2543(Request)
%%           Request    = request record()
%% Descrip.: Given an (newly received) ACK that we have already
%%           determined has no RFC3261 Via branch parameter, try to
%%           locate an already existing server transaction handler
%%           using the painful old RFC2543 backwards compatible code.
%% Returns : Entry |
%%           error |
%%           none
%%           Entry = transactionstate record()
%%--------------------------------------------------------------------
get_server_transaction_ack_2543(Request) when is_record(Request, request) ->
    %% ACK requests are matched to transactions differently if they are not received from
    %% an RFC3261 compliant device, see RFC3261 17.2.3
    case sipheader:get_server_transaction_ack_id_2543(Request) of
	error ->
	    logger:log(error, "Transaction state list: Could not get server transaction RFC2543 ack-id for request"),
	    error;
	Id ->
	    ToTag = sipheader:get_tag(keylist:fetch("To", Request#request.header)),
	    case get_elem_ackid(Id, ToTag) of
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
%% Function: get_server_transaction_using_response(Response)
%%           Response   = response record()
%% Descrip.: Look for a server transaction given a (newly received)
%%           response.
%% Returns : Entry |
%%           none
%%           Entry = transactionstate record()
%%--------------------------------------------------------------------
get_server_transaction_using_response(Response) when is_record(Response, response) ->
    case sipheader:get_client_transaction_id(Response) of
	error ->
	    none;
	Id ->
	    get_elem(server, Id)
    end.

%%--------------------------------------------------------------------
%% Function: get_client_transaction(Method, Branch)
%%           Method     = string()
%%           Branch     = string()
%% Descrip.: Look for a client transaction using a method and branch.
%% Returns : Entry |
%%           none
%%           Entry = transactionstate record()
%% Notes   : This code is only used in transcationlayer before adding
%%           a new client transaction. Since we have only a single
%%           transaction_layer process per node, this is not a race,
%%           but it would be better to just look for duplicates in
%%           add_client_transaction().
%%--------------------------------------------------------------------
get_client_transaction(Method, Branch) ->
    Id = {Branch, Method},
    get_elem(client, Id).

%%--------------------------------------------------------------------
%% Function: get_entrylist_using_pid(Pid)
%%           Pid        = pid()
%%           TStateList = transactionstatelist record()
%% Descrip.: Find all elements in a transactionstatelist who have a
%%           matching pid. Return a plain list of those. Use with
%%           care.
%% Returns : EntryList |
%%           none
%%           EntryList = list() of transactionstate record()
%%--------------------------------------------------------------------
get_entrylist_using_pid(Pid) when is_pid(Pid) ->
    case get_using_pid2(Pid) of
	[] ->
	    none;
	L when is_list(L) ->
	    L
    end.

%%--------------------------------------------------------------------
%% Function: get_elem_using_pid(Pid, TStateList)
%%           Pid        = pid()
%%           TStateList = transactionstatelist record()
%% Descrip.: The same as get_list_using_pid/2 except this function
%%           returns {error, Reason} if more than one record has
%%           a matching pid.
%% Returns : TransactionState |
%%           {error, Reason}
%%           none
%%           TransactionState = transactionstate record()
%%           Reason           = string()
%%--------------------------------------------------------------------
get_elem_using_pid(Pid, TStateList) when is_pid(Pid), is_record(TStateList, transactionstatelist) ->
    %%case get_using_pid2(Pid, TStateList#transactionstatelist.list, []) of
    case get_using_pid2(Pid) of
	[] ->
	    none;
	[Elem] when is_record(Elem, transactionstate) ->
	    Elem;
	L when is_list(L) ->
	    {error, "more than one transactionstate found"};
	_ ->
	    {error, "unknown result from get_using_pid2"}
    end.

%%--------------------------------------------------------------------
%% Function: get_using_pid2(Pid)
%%           Pid    = pid()
%% Descrip.: Internal function that returns a plain list (not a
%%           transactionstatelist) of all the elements of a given
%%           transactionstatelist who have a matching pid.
%% Returns : Entrys |
%%           []
%%           Entrys = list() of transactionstate record()
%%--------------------------------------------------------------------
get_using_pid2(Pid) ->
    RefList = ets:lookup(transactionstate_pid_to_ref, Pid),
    fetch_using_ref_tuples(RefList).

%%--------------------------------------------------------------------
%% Function: get_elem(Type, Id)
%%           Type       = atom() (client | server)
%%           Id         = term()
%% Descrip.: Find a single element from a transactionstatelist which
%%           has a matching type and id.
%% Returns : Entry |
%%           none
%%           Entry = transactionstate record()
%%--------------------------------------------------------------------
get_elem(Type, Id) when is_atom(Type); Type == server; Type == client ->
    TId = {Type, Id},
    RefList = ets:lookup(transactionstate_typeid_to_ref, TId),
    case fetch_using_ref_tuples(RefList) of
	[] -> none;
	[IdMatch] ->
	    IdMatch
    end.


%%--------------------------------------------------------------------
%% Function: get_elem_ackid(AckId, ToTag)
%%           AckId      = term()
%%           ToTag      = string() | none
%% Descrip.: Find a single element from a transactionstatelist which
%%           has a matching ack_id and response_to_tag. Only look at
%%           server transactions (type == server). This is used when
%%           matching RFC2543 ACK's to non-2xx responses to INVITEs.
%% Returns : Entry |
%%           none
%%           Entry = transactionstate record()
%%--------------------------------------------------------------------
get_elem_ackid(AckId, ToTag) ->
    RefList = ets:lookup(transactionstate_ack_to_ref, AckId),
    AckIdMatches = fetch_using_ref_tuples(RefList),
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
%% Function: extract(Fields, TState)
%%           Fields = list() of atom(), pid|appdata|response_to_tag
%%           TState = transactionstate record()
%% Descrip.: Return one or more values from a transactionstate record.
%% Returns : Values = list()
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
%% Function: set_pid(TState, Value)
%%           TState = transactionstate record()
%%           Value  = pid()
%% Descrip.: Set pid in a transactionstate record()
%% Returns : NewTState = transactionstate record()
%%--------------------------------------------------------------------
set_pid(TState, Value) when is_record(TState, transactionstate), is_pid(Value); Value == none ->
    TState#transactionstate{pid = Value}.

%%--------------------------------------------------------------------
%% Function: set_appdata(TState, Value)
%%           TState = transactionstate record()
%%           Value  = term()
%% Descrip.: Set appdata in a transactionstate record()
%% Returns : NewTState = transactionstate record()
%%--------------------------------------------------------------------
set_appdata(TState, Value) when is_record(TState, transactionstate) ->
    TState#transactionstate{appdata = Value}.

%%--------------------------------------------------------------------
%% Function: set_response_to_tag(TState, Value)
%%           TState = transactionstate record()
%%           Value  = string() ???
%% Descrip.: Set response_to_tag in a transactionstate record()
%% Returns : NewTState = transactionstate record()
%%--------------------------------------------------------------------
set_response_to_tag(TState, Value) when is_record(TState, transactionstate) ->
    TState#transactionstate{response_to_tag = Value}.

%%--------------------------------------------------------------------
%% Function: set_result(TState, Value)
%%           TState = transactionstate record()
%%           Value  = string()
%% Descrip.: Set result in a transactionstate record()
%% Returns : NewTState = transactionstate record()
%%--------------------------------------------------------------------
set_result(TState, Value) when is_record(TState, transactionstate), is_list(Value) ->
    TState#transactionstate{result = Value}.

%%--------------------------------------------------------------------
%% Function: delete_expired()
%% Descrip.: Delete entrys from a transactionstatelist record() that
%%           has a timestamp older than now. del_time() signals
%%           the transaction handlers for transactions it removes.
%% Returns : NewTStateList = transactionstatelist record()
%%--------------------------------------------------------------------
delete_expired() ->
%%    Now = util:timestamp(),
%%    L = TStateList#transactionstatelist.list,
%%    NewL = del_time(Now, L),
%%    TStateList#transactionstatelist{list=NewL}.
    %% XXX Not implemented
    ok.

%%--------------------------------------------------------------------
%% Function: delete_using_entrylist(Pid, TStateList)
%%           EntryList  = list() of transactionstate record()
%%           TStateList = transactionstatelist record()
%% Descrip.: Delete entrys from a transactionstatelist record() that
%%           has a pid matching the supplied Pid.
%% Returns : {ok, NumDeleted}
%%           NumDeleted = integer(), number of records deleted
%%--------------------------------------------------------------------
delete_using_entrylist(EntryList) when is_list(EntryList) ->
    ok = del_entrys(EntryList),
    {ok, length(EntryList)}.

%%--------------------------------------------------------------------
%% Function: update_transactionstate(TState)
%%           TState     = transcationstate record()
%% Descrip.: Look in TStateList for an entry matching (using the
%%           unique reference) the TState entry, and replace it with
%%           the (presumably) updated TState entry.
%% Returns : NewTStateList = transactionstatelist record()
%%--------------------------------------------------------------------
update_transactionstate(TState) when is_record(TState, transactionstate) ->
    Ref = TState#transactionstate.ref,
    true = ets:insert(transactionstate_ref_to_t, {Ref, TState}),
    ok.

%%--------------------------------------------------------------------
%% Function: get_length()
%% Descrip.: Returns the length of the list record element of a
%%           transactionstatelist.
%% Returns : Length = integer()
%%--------------------------------------------------------------------
get_length() ->
    ets:info(transactionstate_ref_to_t, size).

%%--------------------------------------------------------------------
%% Function: debugfriendly(TStateList)
%%           TStateList = transactionstatelist record()
%% Descrip.: Return information about the elements in an
%%           transactionstatelist record in a format that is suitable
%%           for logging using ~p.
%% Returns : Data = term()
%%--------------------------------------------------------------------
debugfriendly() ->
    %% no list supplied, request full dump
    L = get_all_entries(),
    debugfriendly2(L, []).

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

monitor_format([]) ->
    [];
monitor_format(TStateList) when is_record(TStateList, transactionstatelist) ->
    monitor_format2(TStateList#transactionstatelist.list, []).

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
%% Function: add(Type, Id, AckId, Pid, Desc, TStateList)
%%           Type       = atom(), client | server
%%           Id         = term()
%%           AckId      = term() | none
%%           Pid        = pid() | none
%%           Desc       = string(),
%% Descrip.: Add a new transaction state entry to TStateList.
%% Returns : ok               |
%%           error            |
%%           {duplicate, Dup}
%%           Dup = transactionstate record()
%%--------------------------------------------------------------------
add(Type, Id, AckId, Pid, Desc) when is_pid(Pid); Pid == none, is_atom(Type),
				     Type == client; Type == server ->
    Ref = make_ref(),
    TId = {Type, Id},
    case ets:insert_new(transactionstate_typeid_to_ref, {TId, Ref}) of
	true ->
	    NewT = #transactionstate{ref=Ref, type=Type, id=Id, ack_id=AckId, pid=Pid,
	    			     description=Desc},
	    true = ets_insert_new(transactionstate_ref_to_t, {Ref, NewT}),
	    case Pid of
		none -> true;
		_ ->
		    true = ets:insert(transactionstate_pid_to_ref, {Pid, Ref})
	    end,
	    case AckId of
		none -> true;
		_ -> true = ets_insert_new(transactionstate_ack_to_ref, {AckId, Ref})
	    end,
	    ok;
	false ->
	    case get_elem(Type, Id) of
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
	    logger:log(error, "Transaction state list: Failed adding entry to ets table '~p'", [TName]),
	    logger:log(debig, "Transaction state list: Data that did not get added to ets table '~p' :~n~p",
		       [TName, Data]),
	    true
    end.

%% Fetch the transaction state and then remove all references to it from all our ets tables
del_entrys([]) ->
    ok;
del_entrys([H|T]) when is_record(H, transactionstate) ->
    Type = H#transactionstate.type,
    Id = H#transactionstate.id,
    TId = {Type, Id},
    true = ets:delete(transactionstate_typeid_to_ref, TId),
    case H#transactionstate.ack_id of
	none -> true;
	AckId -> true = ets:delete(transactionstate_ack_to_ref, AckId)
    end,
    true = ets:delete(transactionstate_pid_to_ref, H#transactionstate.pid),
    true = ets:delete(transactionstate_ref_to_t, H#transactionstate.ref),
    del_entrys(T).

fetch_using_ref_tuples(In) ->
    fetch_using_ref_tuples2(In, []).

fetch_using_ref_tuples2([], Res) ->
    Res;
fetch_using_ref_tuples2([{_, Ref}|T], Res) ->
    case ets:lookup(transactionstate_ref_to_t, Ref) of
	[{Ref, Entry}] ->
	    fetch_using_ref_tuples2(T, [Entry | Res]);
	_ ->
	    fetch_using_ref_tuples2(T, Res)
    end.

get_all_entries() ->
    F = fun(H, Acc) ->
		case H of
		    {_Ref, Entry} -> [Entry | Acc];
		    _ -> Acc
		end
	end,
    lists:foldl(F, [], ets:tab2list(transactionstate_ref_to_t)).
