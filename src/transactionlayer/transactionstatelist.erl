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
	 get_client_transaction/1,
	 get_server_transaction_using_request/1,
	 get_server_transaction_to_cancel/1,
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

%% these records are the different variants of keys in the
%% ack_to_ref and typeid_to_ref ETS tables.
-record(rfc3261_client_id,	{branch		:: nonempty_string(),
				 cseq_method	:: nonempty_string()
				}).

-record(rfc3261_server_id,	{branch		:: nonempty_string(),
				 sent_by	:: {Host :: nonempty_string(), Port :: non_neg_integer()},
				 method		:: nonempty_string()
				}).

-record(rfc2543_ack_id,		{uri		:: #sipurl{},
				 from_tag	:: nonempty_string(),
				 call_id	:: nonempty_string(),
				 cseq		:: {nonempty_string(), nonempty_string()},
				 top_via	:: #via{}
				}).
-record(rfc2543_id,		{uri		:: #sipurl{},
				 from_tag	:: nonempty_string(),
				 to_tag		:: nonempty_string(),
				 call_id	:: nonempty_string(),
				 cseq		:: {nonempty_string(), nonempty_string()},
				 top_via	:: #via{}
				}).


-type rfc3261_client_id()	:: #rfc3261_client_id{}.
-type rfc3261_server_id()	:: #rfc3261_server_id{}.
-type rfc2543_ack_id()		:: #rfc2543_ack_id{}.
-type rfc2543_id()		:: #rfc2543_id{}.

%% Transaction layer data about ongoing transactions
-record(transactionstate, {
	  ref			:: reference(),			%% unique reference
	  type			:: server | client,
	  id			:: rfc3261_client_id() |	 %% identification data for matching requests/responses
	  			   rfc3261_server_id() |
	  			   rfc2543_ack_id()    |
	  			   rfc2543_id(),
	  							%% to this transaction
	  ack_id = none		:: none | rfc2543_ack_id(),	%% special id for matching ACK to INVITE
	  pid			:: pid(),			%% transaction handler process
	  appdata		:: term(),			%% data about this transaction stored by an application
	  response_to_tag	:: nonempty_string(),		%% the To-tag used in responses to this transaction that
								%% we have generated ourselves
	  expire		:: non_neg_integer(),		%% when in time this transaction expires
	  description		:: string(),			%% just used for displaying (in stack_monitor
	  							%% and debug logs etc.)
	  result = none		:: none | nonempty_string()	%% just used for displaying (in stack_monitor
	  							%% and debug logs etc.)
	 }).

-opaque transactionstate() :: #transactionstate{}.


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
-spec add_client_transaction(Method :: nonempty_string(),
			     Branch :: nonempty_string(),
			     Pid    :: pid(),
			     Desc   :: nonempty_string()
			    ) -> ok | error |
				     {duplicate, Dup :: transactionstate()}.

add_client_transaction(Method, Branch, Pid, Desc)
  when is_list(Method), is_list(Branch), is_pid(Pid), is_list(Desc) ->
    add_client_transaction(?DEFAULT_TABLES, Method, Branch, Pid, Desc).

add_client_transaction(Tables, Method, Branch, Pid, Desc)
  when is_list(Method), is_list(Branch), is_pid(Pid), is_list(Desc) ->
    Id = #rfc3261_client_id{branch = Branch,
    			    cseq_method = Method
    			   },
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
    case get_server_transaction_id(Request) of
	error ->
	    logger:log(error, "transactionstatelist: Could not get server transaction id for request"),
	    error;
	Id ->
	    AckId = case Request#request.method of
		        "INVITE" ->
			    %% For INVITE, we must store an extra Id to match ACK to the INVITE transaction.
			    %% We must do this even for RFC3261 INVITE since the ACK might arrive through
			    %% another proxy that is not RFC3261 compliant.
			    get_server_transaction_ack_id_2543(Request);
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

get_server_transaction_using_request(Tables, #request{method = "ACK"} = Request) when is_record(Tables, tables) ->
    case get_server_transaction_id(Request) of
	error ->
	    logger:log(error, "Transaction state list: Could not get server transaction for request"),
	    error;
	is_2543_ack ->
	    get_server_transaction_ack_2543(Tables, Request);
	Id ->
	    case get_elem(Tables, server, Id) of
		none ->
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
		Res when is_record(Res, transactionstate) ->
		    Res
	    end
    end;
get_server_transaction_using_request(Tables, Request) when is_record(Tables, tables), is_record(Request, request) ->
    get_server_transaction_using_request2(Tables, Request).

get_server_transaction_using_request2(Tables, Request) ->
    case get_server_transaction_id(Request) of
	error ->
	    logger:log(error, "Transaction state list: Could not get server transaction for request"),
	    error;
	Id ->
	    case get_elem(Tables, server, Id) of
		none ->
		    none;
		Res when is_record(Res, transactionstate) ->
		    Res
	    end
    end.

get_server_transaction_to_cancel(Request) when is_record(Request, request) ->
    get_server_transaction_to_cancel(?DEFAULT_TABLES, Request).

get_server_transaction_to_cancel(Tables, #request{method = "CANCEL"} = Request) when is_record(Tables, tables) ->
    Header = Request#request.header,
    %% XXX not only INVITE can be cancelled, RFC3261 9.2 says we should find the
    %% transaction that is being handled by 'assuming the method is anything but
    %% CANCEL or ACK'.
    {CSeqNum, _} = sipheader:cseq(Header),
    %% When looking for the corresponding INVITE transaction, we have to change the
    %% CSeq method of this header to INVITE, in case we received it from a RFC2543 client
    %% (RFC2543 backwards-compatible transaction matching includes the whole CSeq, this is
    %% probably an error in RFC3261 #9.2 that refers to #17.2.3 which does not say that
    %% CANCEL matches a transaction even though the CSeq method differs)
    IHeader = keylist:set("CSeq", [sipheader:cseq_print({CSeqNum, "INVITE"})], Header),
    Invite = Request#request{method = "INVITE",
			     header = IHeader
			    },
    case get_server_transaction_using_request2(Tables, Invite) of
	none ->
	    none;
	Res ->
	    Res
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
    case get_server_transaction_ack_id_2543(Request) of
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
%% @spec    (Response) ->
%%            Entry |
%%            none
%%
%%            Response = #response{}
%%
%%            Entry = #transactionstate{}
%%
%% @doc     When we receive a response, we use this function to get an
%%          Id which we look up in our transaction state database to
%%          see if we have a client transaction handler that should
%%          get this response. This is specified in RFC3261 #17.1.3
%%          (Matching Responses to Client Transactions).
%% @end
%%--------------------------------------------------------------------
get_client_transaction(Response) when is_record(Response, response) ->
    get_client_transaction(?DEFAULT_TABLES, Response).

get_client_transaction(Tables, Response) ->
    Id = get_client_transaction_id(Response),
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
%% @spec    (Tables, Pid) ->
%%            Entrys |
%%            []
%%
%%            Tables  = #tables{}
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
%% @spec    (Tables, Type, Id) ->
%%            Entry |
%%            none
%%
%%            Tables = #tables{}
%%            Type   = client | server
%%            Id     = term()
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
    extract(T, TState, lists:append(Res, [TState#transactionstate.response_to_tag]));
extract([description | T], TState, Res) when is_record(TState, transactionstate) ->
    extract(T, TState, lists:append(Res, [TState#transactionstate.description]));
extract([result | T], TState, Res) when is_record(TState, transactionstate) ->
    extract(T, TState, lists:append(Res, [TState#transactionstate.result])).


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
    Out = debugfriendly_non_empty([pid, ack_id, response_to_tag, appdata, result], H, In),
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
%% empty, or unknown record field
debugfriendly_non_empty([_ | T], R, Res) ->
    debugfriendly_non_empty(T, R, Res).

monitor_format(TStateList) when is_record(TStateList, transactionstatelist) ->
    monitor_format2(TStateList#transactionstatelist.list, []);
monitor_format(TList) when is_list(TList) ->
    monitor_format2(TList, []).

monitor_format2([], Res) ->
    lists:reverse(Res);
monitor_format2([H|T], Res) when is_record(H, transactionstate) ->
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


%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            Id
%%
%%            Request = #request{}
%%
%%            Id = rfc3261_server_id() | rfc2543_server_id() | is_2543_ack | error
%%
%% @doc     Turn a request into a transaction id, that can be stored
%%          in our transaction state database together with a
%%          reference to the process handling this request (server
%%          transaction handler) if this is a new transaction, or
%%          looked up in the database to find an existing handler if
%%          this is a resend of the same request or an ACK to a
%%          non-2xx response to INVITE. This is specified in RFC3261
%%          #17.2.3 (Matching Requests to Server Transactions).
%% @end
%%--------------------------------------------------------------------
get_server_transaction_id(Request) ->
    %% We do a catch around this since it includes much parsing of the
    %% request, and parsing data received from the network is a fragile thing.
    case catch guarded_get_server_transaction_id(Request) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from get_server_transaction_id(~p) :~n~p", [Request, E]),
	    error;
	Id ->
	    Id
    end.

guarded_get_server_transaction_id(Request) when is_record(Request, request) ->
    TopVia = sipheader:topvia(Request#request.header),
    case sipheader:get_via_branch(TopVia) of
	"z9hG4bK" ++ _RestOfBranch ->
	    M = case Request#request.method of
		    "ACK" ->
			%% RFC3261 #17.2.3, bullet #3 - when looking for server
			%% transaction for ACK, the method of the transaction is INVITE
			"INVITE";
		    Other ->
			Other
		end,
	    guarded_get_server_transaction_id_3261(M, TopVia);
	_ ->
	    guarded_get_server_transaction_id_2543(Request, TopVia)
    end.

%%--------------------------------------------------------------------
%% @spec    (Response) ->
%%            Id
%%
%%            Response = #response{}
%%
%%            Id = rfc3261_client_id() | error
%%
%% @doc     Get the id we use for client transactions.
%% @see     get_client_transaction/1.
%% @end
%%--------------------------------------------------------------------
get_client_transaction_id(Response) ->
    %% We do a catch around this since it includes much parsing of the
    %% request, and parsing data received from the network is a fragile thing.
    case catch guarded_get_client_transaction_id(Response) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from get_client_transaction_id(~p) :~n~p", [Response, E]),
	    error;
	Id ->
	    Id
    end.

guarded_get_client_transaction_id(Response) when is_record(Response, response) ->
    Header = Response#response.header,
    TopVia = sipheader:topvia(Header),
    Branch = sipheader:get_via_branch(TopVia),
    {_, CSeqMethod} = sipheader:cseq(Header),
    #rfc3261_client_id{branch = Branch,
		       cseq_method = CSeqMethod
		      }.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            Id
%%
%%            Request = #request{}
%%
%%            Id = rfc2543_id() | error
%%
%% @doc     When we receive an ACK that has no RFC3261 Via branch
%%          parameter, we use this function to get an Id that we then
%%          look up in our transaction state database to try and find
%%          an existing server transaction that this ACK should be
%%          delivered to. This is specified in RFC3261 #17.2.3
%%          (Matching Requests to Server Transactions). Note : When
%%          using this function, you have to make sure the To-tag of
%%          this ACK matches the To-tag of the response you think
%%          this might be the ACK for!
%%          Note : RFC3261 #17.2.3 relevant text : The ACK request
%%          matches a transaction if the Request- URI, From tag,
%%          Call-ID, CSeq number (not the method), and top Via header
%%          field match those of the INVITE request which created the
%%          transaction, and the To tag of the ACK matches the To tag
%%          of the response sent by the server transaction.
%%          Note : We are supposed to do the comparison of for
%%          example, the URI, according to the matching rules for
%%          URIs but that would require us to do a full table scan
%%          for every ACK. XXX perhaps we should divide the Id into
%%          two parts - one that is byte-by-byte and used as table
%%          index, and another part for elements that require more
%%          exhaustive matching.
%% @end
%%--------------------------------------------------------------------
get_server_transaction_ack_id_2543(Request) ->
    case catch guarded_get_server_transaction_ack_id_2543(Request) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from get_server_transaction_ack_id_2543(~p) :~n~p", [Request, E]),
	    error;
	Id ->
	    Id
    end.

guarded_get_server_transaction_ack_id_2543(Request) when is_record(Request, request) ->
    {URI, Header} = {Request#request.uri, Request#request.header},
    TopVia = remove_branch(sipheader:topvia(Header)),
    CallId = sipheader:callid(Header),
    {CSeqNum, _} = sipheader:cseq(Header),
    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
    %% We are supposed to match only on the CSeq number, but the entry we are
    %% matching against is an INVITE and that INVITE had it's Id generated with
    %% the full CSeq. Make it possible to match the INVITE with this Id.
    FakeCSeq = {CSeqNum, "INVITE"},
    #rfc2543_ack_id{uri		= URI,
		    from_tag	= FromTag,
		    call_id	= CallId,
		    cseq	= FakeCSeq,
		    top_via	= TopVia
		   }.

remove_branch(Via) when is_record(Via, via) ->
    ParamDict = sipheader:param_to_dict(Via#via.param),
    NewDict = dict:erase("branch", ParamDict),
    Via#via{param = sipheader:dict_to_param(NewDict)}.

%%--------------------------------------------------------------------
%% @spec    (Method, TopVia) ->
%%            Id
%%
%%            Method = list()
%%            TopVia = #via{}
%%
%%            Id = rfc3261_server_id()
%%
%% @doc     Part of guarded_get_server_transaction_id(), called when
%%          the top Via header is found to contain an RFC3261 branch
%%          parameter. This is the straight forward case.
%% @end
%%--------------------------------------------------------------------
guarded_get_server_transaction_id_3261(Method, TopVia) when is_list(Method), is_record(TopVia, via) ->
    Branch = sipheader:get_via_branch_full(TopVia),
    SentBy = via_sentby(TopVia),
    #rfc3261_server_id{branch	= Branch,
		       sent_by	= SentBy,
		       method	= Method
		      }.

%%--------------------------------------------------------------------
%% @spec    (Request, TopVia) ->
%%            Id
%%
%%            Request = #request{}
%%            TopVia  = #via{}
%%
%%            Id = term() | is_2543_ack
%%
%% @doc     Part of guarded_get_server_transaction_id(), called when
%%          the top Via header does NOT contain an RFC3261 branch
%%          parameter. Creates an Id based on RFC3261 #17.2.3
%%          (Matching Requests to Server Transactions). Note : We
%%          could very well do the 2543 ack-id computation here, but
%%          since the caller must do the To-tag verification for such
%%          requests we just return is_2543_ack here to make sure the
%%          caller does not miss this. Note : RFC3261 #17.2.3 has
%%          different text for ACK (entirely separate, see previous
%%          note), INVITE and "all other methods". However, it seems
%%          to me that the instructions for INVITE and "all other"
%%          are the same :
%%          The INVITE request matches a transaction if the
%%          Request-URI, To tag, From tag, Call-ID, CSeq, and top Via
%%          header field match those of the INVITE request which
%%          created the transaction. ... For all other request
%%          methods, a request is matched to a transaction if the
%%          Request-URI, To tag, From tag, Call-ID, CSeq (including
%%          the method), and top Via header field match those of the
%%          request that created the transaction.
%%          Therefor, we just have non-ACK below.
%% @end
%%--------------------------------------------------------------------
%%
%% ACK
%%
guarded_get_server_transaction_id_2543(Request, _) when is_record(Request, request), Request#request.method == "ACK" ->
    is_2543_ack;

%%
%% non-ACK
%%
guarded_get_server_transaction_id_2543(Request, TopVia) when is_record(Request, request), is_record(TopVia, via) ->
    {URI, Header} = {Request#request.uri, Request#request.header},
    CallId = sipheader:callid(Header),
    CSeq = sipheader:cseq(Header),
    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
    ToTag = sipheader:get_tag(keylist:fetch('to', Header)),
    #rfc2543_id{uri		= URI,
		from_tag	= FromTag,
		to_tag		= ToTag,
		call_id		= CallId,
		cseq		= CSeq,
		top_via		= TopVia
	       }.

%%--------------------------------------------------------------------
%% @spec    (Via) ->
%%            {Host, Port}
%%
%%            Via = #via{}
%%
%%            Host  = string()
%%            Port  = integer()
%%
%% @doc     Extract sent-by part of a via record()
%% @end
%%--------------------------------------------------------------------
via_sentby(Via) when is_record(Via, via) ->
    %% Before a request is sent, the client transport MUST insert a value of
    %% the "sent-by" field into the Via header field.  This field contains
    %% an IP address or host name, and port.
    {Via#via.host, Via#via.port}.



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
-ifdef( YXA_NO_UNITTEST ).
test() ->
    {error, "Unit test code disabled at compile time"}.

-else.

test() ->
    %% test via_sentby(Via)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "via_sentby/1 - 1"),
    {"host", 1234} = via_sentby(#via{host="host", port=1234}),

    %% test get_server_transaction_id(Request)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_server_transaction_id/1 - 1.1"),
    %% get Id for INVITE with RFC3261 branch tag in top Via
    InviteHeader1 = keylist:from_list([
				       {"Via",	["SIP/2.0/TLS sip.example.org:5061;branch=z9hG4bK-really-unique"]},
				       {"From", ["<sip:alice@example.org>;tag=f-abc"]},
				       {"To",	["<sip:bob@example.org>"]},
				       {"Call-ID", ["3c26722ce234@192.0.2.111"]},
				       {"CSeq",	["2 INVITE"]}
				      ]),
    Invite1 = #request{method="INVITE", uri=sipurl:parse("sip:alice@example.org"),
		       header=InviteHeader1, body = <<>>},
    Invite1Id = get_server_transaction_id(Invite1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 1.2"),
    %% check result
    #rfc3261_server_id{branch = "z9hG4bK-really-unique",
		       sent_by = {"sip.example.org", 5061},
		       method = "INVITE"}
	= Invite1Id,

    autotest:mark(?LINE, "get_server_transaction_id/1 - 2"),
    %% make an ACK for an imagined 3xx-6xx response with to-tag "t-123"
    AckInvite1Header_1 = keylist:set("To", ["<sip:bob@example.org>;tag=t-123"], InviteHeader1),
    AckInvite1Header1  = keylist:set("CSeq", ["2 ACK"], AckInvite1Header_1),
    AckInvite1 = #request{method="ACK", uri=sipurl:parse("sip:alice@example.org"),
			  header=AckInvite1Header1, body = <<>>},

    AckInvite1Id = get_server_transaction_id(AckInvite1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 3"),
    %% Test that the INVITE id matches the ACK id
    Invite1Id = AckInvite1Id,

    autotest:mark(?LINE, "get_server_transaction_id/1 - 4.1"),
    %% get Id for INVITE with RFC2543 branch tag in top Via
    Invite2543_1Header = keylist:from_list([
					    {"Via",	["SIP/2.0/TLS sip.example.org:5061;branch=not-really-unique"]},
					    {"From",	["<sip:alice@example.org>;tag=f-abc"]},
					    {"To",	["<sip:bob@example.org>"]},
					    {"Call-ID", ["3c26722ce234@192.0.2.111"]},
					    {"CSeq",	["2 INVITE"]}
					   ]),
    Invite2543_1 = #request{method="INVITE", uri=sipurl:parse("sip:alice@example.org"),
			    header=Invite2543_1Header, body = <<>>},
    Invite2543_1Id = get_server_transaction_id(Invite2543_1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 4.2"),
    %% check result
    Invite2543_1Expected =
	#rfc2543_id{uri		= Invite2543_1#request.uri,
		    from_tag	= "f-abc",
		    to_tag	= none,
		    call_id	= "3c26722ce234@192.0.2.111",
		    cseq	= {"2", "INVITE"},
		    top_via	= sipheader:topvia(Invite2543_1#request.header)
		   },
    ok = test_compare_records(Invite2543_1Id, Invite2543_1Expected, []),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 5.1"),
    %% for RFC2543 INVITE, we must also get the ACK-id to match future ACKs with this INVITE
    Invite2543_1AckId = get_server_transaction_ack_id_2543(Invite2543_1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 5.2"),
    %% check result
    Invite2543_1Expected_Ack =
	#rfc2543_ack_id{uri		= Invite2543_1#request.uri,
			from_tag	= "f-abc",
			call_id		= "3c26722ce234@192.0.2.111",
			cseq		= {"2", "INVITE"},
			top_via		= #via{proto = "SIP/2.0/TLS",
					       host  = "sip.example.org",
					       port  = 5061,
					       param = []
					      }
		       },
    ok = test_compare_records(Invite2543_1AckId, Invite2543_1Expected_Ack, []),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 6"),
    %% make an ACK for an imagined 3xx-6xx response with to-tag "t-123", check that
    %% get_server_transaction_id refuses and tells us it is an 2543 ACK
    AckInvite2543_1Header_1 = keylist:set("To", ["<sip:bob@example.org>;tag=t-123"], Invite2543_1Header),
    AckInvite2543_1Header   = keylist:set("CSeq", ["2 ACK"], AckInvite2543_1Header_1),
    AckInvite2543_1 = #request{method="ACK", uri=sipurl:parse("sip:alice@example.org"),
			       header=AckInvite2543_1Header, body = <<>>},

    is_2543_ack = get_server_transaction_id(AckInvite2543_1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 7"),
    %% now get the 2543 ACK id from the ACK
    AckInvite2543_1Id = get_server_transaction_ack_id_2543(AckInvite2543_1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 8"),
    %% check that the 2543 ACK id matches the 2543 INVITE id
    AckInvite2543_1Id = Invite2543_1AckId,


    %% test get_client_transaction_id(Response)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_client_transaction_id/1 - 1"),
    Response1 = #response{status=699, reason="foo", header=Invite2543_1Header, body = <<>>},
    #rfc3261_client_id{branch = "not-really-unique",
		       cseq_method = "INVITE"
		      } = get_client_transaction_id(Response1),




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
    MessageCommonHeaders1 =
	"From: Test <sip:test@example.org;tag=abc>\r\n"
	"To: Test <sip:test@example.org>\r\n"
	"Call-Id: unittest-add_server_transaction3@yxa.example.org\r\n"
	"CSeq: INVITE 1234\r\n",

    Message1 =
	"INVITE sip:ft@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org;branch=" ++ Message1Branch ++ "\r\n"
        ++ MessageCommonHeaders1 ++
	"\r\n",

    Request1 = sippacket:parse(Message1, none),

    autotest:mark(?LINE, "add_server_transaction/4 - 1.1"),
    %% add a request
    Request1_Description = "TEST request 1",
    ok = add_server_transaction(TestTables, Request1, self(), Request1_Description),

    autotest:mark(?LINE, "add_server_transaction/4 - 2"),
    %% try to add the same request again, should fail
    {duplicate, _} = add_server_transaction(TestTables, Request1, self(), Request1_Description),

    autotest:mark(?LINE, "add_server_transaction/4 - 3.0"),
    CancelMessage1 =
	"CANCEL sip:ft@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org;branch=" ++ Message1Branch ++ "\r\n"
	++ MessageCommonHeaders1 ++
	"\r\n",

    CancelRequest1 = sippacket:parse(CancelMessage1, none),
    CancelRequest1_Description = "CANCEL of TEST request 1",
    %% add a CANCEL request (CANCEL of the INVITE received in test case above)
    ok = add_server_transaction(TestTables, CancelRequest1, self(), CancelRequest1_Description),

    autotest:mark(?LINE, "get_server_transaction_using_request/2 - 1.1"),
    %% try to fetch the INVITE transaction
    GetUsingReq1_Expected1 =
	#transactionstate{type		= server,
			  id		= get_server_transaction_id(Request1),
			  description	= Request1_Description,
			  pid		= self()
			 },
    GetUsingReq1_Result1 = get_server_transaction_using_request(TestTables, Request1),
    ok = test_compare_records(GetUsingReq1_Expected1, GetUsingReq1_Result1, [ref, ack_id, expire]),

    autotest:mark(?LINE, "get_server_transaction_using_request/2 - 1.2"),
    %% try to fetch the CANCEL transaction
    GetUsingReq1_Expected2 =
	#transactionstate{type		= server,
			  id		= get_server_transaction_id(CancelRequest1),
			  description	= CancelRequest1_Description,
			  pid		= self()
			 },
    GetUsingReq1_Result2 = get_server_transaction_using_request(TestTables, CancelRequest1),
    ok = test_compare_records(GetUsingReq1_Expected2, GetUsingReq1_Result2, [ref, expire]),


    autotest:mark(?LINE, "get_server_transaction_to_cancel/2 - 1"),
    %% now, try finding the INVITE server transaction using the CANCEL request
    GetToCancel2_Result = get_server_transaction_to_cancel(TestTables, CancelRequest1),
    ok = test_compare_records(GetUsingReq1_Result1, GetToCancel2_Result, []),

    autotest:mark(?LINE, "get_server_transaction_to_cancel/2 - 2"),
    %% try the same thing but with the CANCEL being received over another transport
    CancelMessage2 =
	"CANCEL sip:ft@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/UDP one.example.org;branch=" ++ Message1Branch ++ "\r\n"
	++ MessageCommonHeaders1 ++
	"\r\n",

    CancelRequest2 = sippacket:parse(CancelMessage2, none),

    GetToCancel3_Result = get_server_transaction_to_cancel(TestTables, CancelRequest2),
    ok = test_compare_records(GetUsingReq1_Result1, GetToCancel3_Result, []),


    %% add_client_transaction(Method, Branch, Pid, Desc)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add_client_transaction/5 - 1"),
    Message4Method = "TEST",
    Message4Branch = "z9hG4bK-yxa-unittest-add_client_transaction3",
    Request4Description = "TEST client request 4",
    ok = add_client_transaction(TestTables, Message4Method, Message4Branch, self(), Request4Description),

    %% build response
    Response4Header = keylist:from_list([
					    {"Via",	["SIP/2.0/UDP sip.example.org:5060;branch=" ++ Message4Branch]},
					    {"From",	["<sip:alice@example.org>;tag=f-abc"]},
					    {"To",	["<sip:bob@example.org>;tag=t-abc"]},
					    {"Call-ID", ["3c26722ce235@192.0.2.111"]},
					    {"CSeq",	["2 " ++ Message4Method]}
					   ]),

    Response4 = #response{status=200, reason="Foo", header=Response4Header, body = <<>>},

    autotest:mark(?LINE, "add_client_transaction/5 - 2"),
    %% try to add the same request again, should fail
    {duplicate, _} = add_client_transaction(TestTables, Message4Method, Message4Branch, self(), Request4Description),

    autotest:mark(?LINE, "get_client_transaction/2 - 1.0"),
    %% try to fetch the TEST transaction
    GetUsingReq4_Expected1 =
	#transactionstate{type		= client,
			  id		= get_client_transaction_id(Response4),
			  description	= Request4Description,
			  pid		= self()
			 },
    autotest:mark(?LINE, "get_client_transaction/2 - 1.1"),
    GetUsingReq4_Result1 = get_client_transaction(TestTables,
    						  Response4),
    autotest:mark(?LINE, "get_client_transaction/2 - 1.2"),
    ok = test_compare_records(GetUsingReq4_Expected1, GetUsingReq4_Result1, [ref, expire]),




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

%% compare two records element by element and give good information on where they
%% are not equal
test_compare_records(R1, R2, ShouldChange) when is_tuple(R1), is_tuple(R2), is_list(ShouldChange) ->
    RecName = element(1, R1),
    Fields = test_record_info(RecName),
    autotest_util:compare_records(R1, R2, ShouldChange, Fields).

%% add more records here when needed
test_record_info(transactionstate) ->	record_info(fields, transactionstate);
test_record_info(rfc2543_id) ->		record_info(fields, rfc2543_id);
test_record_info(rfc2543_ack_id) ->	record_info(fields, rfc2543_ack_id);
test_record_info(rfc3261_client) ->	record_info(fields, rfc3261_client_id);
test_record_info(rfc3261_server) ->	record_info(fields, rfc3261_server_id).

-endif.
