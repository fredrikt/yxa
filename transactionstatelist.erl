-module(transactionstatelist).
-export([add_client_transaction/4, add_server_transaction/3, 
	 delete_using_pid/2, delete_expired/1,
	 empty/0, extract/2, debugfriendly/1, get_client_transaction/3,
	 get_server_transaction_using_request/2, get_server_transaction_using_response/2,
	 get_elem_using_pid/2, get_list_using_pid/2,
	 set_pid/2, set_appdata/2, set_response_to_tag/2,
	 update_transactionstate/2, append_response_branch/3,
	 get_server_transaction_using_stateless_response_branch/3,
	 get_length/1]).

-include("transactionstatelist.hrl").
-include("siprecords.hrl").

%%-record(transactionstate, {ref, id, ack_id, branch, pid, appdata,
%%	request, response_to_tag, expire}).

add(Type, Id, AckId, Pid, TStateList) when record(TStateList, transactionstatelist), pid(Pid) ->
    case get_elem(Type, Id, TStateList) of
	none ->
	    NewT = #transactionstate{ref=make_ref(), type=Type, id=Id, ack_id=AckId, pid=Pid,
	    			     stateless_response_branches=[]},
	    #transactionstatelist{list=lists:append(TStateList#transactionstatelist.list, [NewT])};
	_ ->
	    logger:log(error, "transactionstatelist: Asked to add transaction with duplicate Id ~p to list :~n~p",
	    	       [Id, debugfriendly(TStateList)]),
	    TStateList
    end.


add_client_transaction(Method, Branch, Pid, TStateList) when record(TStateList, transactionstatelist), pid(Pid) ->
    Id = {Branch, Method},
    add(client, Id, none, Pid, TStateList).

add_server_transaction(Request, Pid, TStateList) when record(Request, request), record(TStateList, transactionstatelist), pid(Pid) ->
    case sipheader:get_server_transaction_id(Request) of
	error ->
	    logger:log(error, "transactionstatelist: Could not get server transaction id for request"),
	    TStateList;
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
	    add(server, Id, AckId, Pid, TStateList)
    end.

empty() ->
    #transactionstatelist{list=[]}.

get_server_transaction_using_request(Request, TransactionList) when record(Request, request), record(TransactionList, transactionstatelist) ->
    case sipheader:get_server_transaction_id(Request) of
	is_2543_ack ->
	    get_server_transaction_ack_2543(Request, TransactionList);
	error ->
	    logger:log(error, "Transaction state list: Could not get server transaction for request"),
	    error;
	Id ->
	    case get_elem(server, Id, TransactionList#transactionstatelist.list) of
		none when Request#request.method == "ACK" ->
		    %% If the UAC is 2543 compliant, but there is a 3261 compliant proxy between UAC and us,
		    %% the 3261 proxy will possibly generate another branch for the ACK than the INVITE
		    %% because the ACK might have a To-tag. If this happens, we must use 2543 methods to find
		    %% the transaction even though RFC3261 17.2.3 only says we should do this if the ACK
		    %% received does NOT have the 3261 magic cookie in the branch parameter.
		    %%
		    %% Bug in RFC3261, see http://bugs.sipit.net/sipwg/show_bug.cgi?id=755
		    %%
		    logger:log(debug, "Transaction state list: Found no matching server transaction for ACK using RFC3261 methods, " ++
			       "trying RFC2543 too"),
		    get_server_transaction_ack_2543(Request, TransactionList);	
		T ->
		    T
	    end    
    end.

%% ACK requests are matched to transactions differently if they are not received from
%% an RFC3261 compliant device, see RFC3261 17.2.3   
get_server_transaction_ack_2543(Request, TransactionList) when record(Request, request), record(TransactionList, transactionstatelist) ->
    case sipheader:get_server_transaction_ack_id_2543(Request) of
	error ->
	    logger:log(error, "Transaction state list: Could not get server transaction RFC2543 ACK id for request"),
	    error;
	Id ->
	    ToTag = sipheader:get_tag(keylist:fetch("To", Request#request.header)),
	    case get_elem_ackid(server, Id, ToTag, TransactionList#transactionstatelist.list) of
		none ->
		    logger:log(debug, "Transaction state list: ACK request does not match any existing transaction"),
		    %% If this ever happens, this extra debug output will probably be crucial to
		    %% diagnose why.
		    logger:log(debug, "Transaction state list: Extra debug: Looked for server transaction with id ~p AND to-tag ~p in list :~n~p", [Id, ToTag, debugfriendly(TransactionList)]),
		    none;
		Res when record(Res, transactionstate) ->
		    Res
	    end
    end.

get_server_transaction_using_response(Response, TransactionList) when record(Response, response), record(TransactionList, transactionstatelist) ->
    case sipheader:get_client_transaction_id(Response) of
	error ->
	    none;
	Id ->
	    get_elem(server, Id, TransactionList#transactionstatelist.list)
    end.

get_client_transaction(Method, Branch, TStateList) when record(TStateList, transactionstatelist) ->
    Id = {Branch, Method},
    get_elem(client, Id, TStateList).

%% Function: get_list_using_pid/2
%% Description: Find all elements in a transactionstatelist who have
%%              a matching pid. Return a new transactionstatelist
%%              containing the transactionstate records.
%% Returns: TransactionStateListRecord |
%%          none
%%--------------------------------------------------------------------
get_list_using_pid(Pid, TStateList) when record(TStateList, transactionstatelist), pid(Pid) ->
    case get_using_pid2(Pid, TStateList#transactionstatelist.list, []) of
	[] ->
	    none;
	L ->
	    #transactionstatelist{list=L}
    end;
get_list_using_pid(_, _) ->
    {error, "Invalid arguments passed to get_list_using_pid()"}.

%% Function: get_elem_using_pid/2
%% Description: The same as get_list_using_pid/2 except this function
%%              returns {error, Reason} if more than one record has
%%              a matching pid.
%% Returns: [TransactionStateRecord] |
%%          none
%%--------------------------------------------------------------------
get_elem_using_pid(Pid, TStateList) when record(TStateList, transactionstatelist), pid(Pid) ->
    case get_using_pid2(Pid, TStateList#transactionstatelist.list, []) of
	[] ->
	    none;
	[Elem] ->
	    [Elem];
	L when list(L) ->
	    {error, "more than one transactionstate found"};
	_ ->
	    {error, "unknown result from get_using_pid2"}
    end.

%% Function: get_using_pid/2
%% Description: Non-exported function that returns a plain list (not
%%              a transactionstatelist) of all the elements of a
%%              given transactionstatelist who have a matching pid.
%% Returns: ListOfTransactionStateRecords |
%%          []
%%--------------------------------------------------------------------
get_using_pid2(Pid, [], Res) ->
    Res;
get_using_pid2(Pid, [H | T], Res) when record(H, transactionstate), H#transactionstate.pid == Pid ->
    get_using_pid2(Pid, T, lists:append(Res, [H]));
get_using_pid2(Pid, [H | T], Res) when record(H, transactionstate) ->
    get_using_pid2(Pid, T, Res).

get_server_transaction_using_stateless_response_branch(Branch, Method, TStateList) when record(TStateList, transactionstatelist) ->
    Id = {Branch, Method},
    get_server_transaction_using_stateless_response_branch2(Id, TStateList#transactionstatelist.list).

get_server_transaction_using_stateless_response_branch2(Id, []) ->
    none;
get_server_transaction_using_stateless_response_branch2(Id, [H | T]) ->
    case lists:member(Id, H#transactionstate.stateless_response_branches) of
	true ->
	    H;
	_ ->
	    get_server_transaction_using_stateless_response_branch2(Id, T)
    end.

%% Function: get_elem/3
%% Description: Find a single element from a transactionstatelist
%%              which has a matching type and id.
%% Returns: TransactionStateRecord |
%%          none
%%--------------------------------------------------------------------
get_elem(Type, Id, TStateList) when record(TStateList, transactionstatelist) ->
    get_elem(Type, Id, TStateList#transactionstatelist.list);

get_elem(Type, Id, []) ->
    none;
get_elem(Type, Id, [H | T]) when record(H, transactionstate), H#transactionstate.type == Type, H#transactionstate.id == Id ->
    H;
get_elem(Type, Id, [H | T]) when record(H, transactionstate) ->
    get_elem(Type, Id, T).


%% Function: get_elem_ackid/4
%% Description: Find a single element from a transactionstatelist
%%              which has a matching ack_id and response_to_tag.
%% Returns: TransactionStateRecord |
%%          none
%%--------------------------------------------------------------------
get_elem_ackid(Type, AckId, ToTag, TStateList) when record(TStateList, transactionstatelist) ->
    get_elem_ackid(Type, AckId, ToTag, TStateList#transactionstatelist.list);

get_elem_ackid(Type, AckId, ToTag, []) ->
    none;
get_elem_ackid(Type, AckId, ToTag, [H | T]) when record(H, transactionstate),
H#transactionstate.type == Type, H#transactionstate.ack_id == AckId, H#transactionstate.response_to_tag == ToTag ->
    H;
get_elem_ackid(Type, AckId, ToTag, [H | T]) when record(H, transactionstate),
H#transactionstate.type == Type, H#transactionstate.ack_id == AckId ->
    logger:log(debug, "Transaction state list: Found a transaction with matching ACK-Id, but the to-tag is not ~p (it is ~p) :~n~p",
	       [ToTag, H#transactionstate.response_to_tag, debugfriendly(H)]),
    get_elem_ackid(Type, AckId, ToTag, T);
get_elem_ackid(Type, AckId, ToTag, [H | T]) when record(H, transactionstate),
H#transactionstate.type == Type, H#transactionstate.ack_id == AckId; H#transactionstate.response_to_tag == ToTag ->
    logger:log(debug, "Transaction state list: Found a transaction with matching ack-id OR matching to-tag~n(ack-id: ~p~n to-tag: ~p)~n~p",
	       [AckId, ToTag, debugfriendly(H)]),
    get_elem_ackid(Type, AckId, ToTag, T);
get_elem_ackid(Type, AckId, ToTag, [H | T]) when record(H, transactionstate) ->
    get_elem_ackid(Type, AckId, ToTag, T).


extract(Values, TState) when record(TState, transactionstate) ->
    extract(Values, TState, []).

extract([], TState, Res) when record(TState, transactionstate) ->
    Res;
extract([pid | T], TState, Res) when record(TState, transactionstate) ->
    extract(T, TState, lists:append(Res, [TState#transactionstate.pid]));
extract([appdata | T], TState, Res) when record(TState, transactionstate) ->
    extract(T, TState, lists:append(Res, [TState#transactionstate.appdata]));
extract([response_to_tag | T], TState, Res) when record(TState, transactionstate) ->
    extract(T, TState, lists:append(Res, [TState#transactionstate.response_to_tag])).

set_pid(TState, Value) when record(TState, transactionstate) ->
    TState#transactionstate{pid = Value}.

set_appdata(TState, Value) when record(TState, transactionstate) ->
    TState#transactionstate{appdata = Value}.

set_response_to_tag(TState, Value) when record(TState, transactionstate) ->
    TState#transactionstate{response_to_tag = Value}.

append_response_branch(TState, Branch, Method) when record(TState, transactionstate) ->
    In = TState#transactionstate.stateless_response_branches,
    Id = {Branch, Method},
    case lists:member(Id, In) of
	true ->
	    logger:log(debug, "Transaction state list: Stateless response id ~p already stored on element :~n~p",
		       [Id, debugfriendly(TState)]),
	    TState;
	_ ->
	    TState#transactionstate{stateless_response_branches=lists:append(In, [Id])}
    end.

delete_expired(TStateList) when record(TStateList, transactionstatelist) ->
    Now = util:timestamp(),
    #transactionstatelist{list=del_time(Now, TStateList#transactionstatelist.list)}.

delete_using_pid(TState, TStateList) when record(TStateList, transactionstatelist) ->
    #transactionstatelist{list=del_pid(TState, TStateList#transactionstatelist.list)}.



del_pid(Pid, []) ->
    [];
del_pid(Pid, [H | T]) when record(H, transactionstate), H#transactionstate.pid == Pid ->
    del_pid(Pid, T);
del_pid(Pid, [H | T]) when record(H, transactionstate) ->
    [H | del_pid(Pid, T)].

del_time(Time, []) ->
    [];
del_time(Time, [H | T]) when record(H, transactionstate), H#transactionstate.expire =< Time, H#transactionstate.expire > 0 ->
    case util:safe_is_process_alive(H#transactionstate.pid) of
	{true, Pid} ->
	    %% Be nice and tell lingering processes it is time to go
	    logger:log(error, "Transaction layer: Had to tell lingering transaction that it is time to terminate :~n~p",
		       [debugfriendly(H)]),
	    gen_server:cast(Pid, {expired});
	_ ->
	    true
    end,
    del_time(Time, T);
del_time(Time, [H | T]) when record(H, transactionstate) ->
    [H | del_time(Time, T)].



update_transactionstate(TState, TStateList) when record(TState, transactionstate), record(TStateList, transactionstatelist) ->
    Ref = TState#transactionstate.ref,
    #transactionstatelist{list=update_transactionstate(Ref, TState, TStateList#transactionstatelist.list, TStateList)}.

update_transactionstate(Ref, _, [], TStateList) ->
    logger:log(error, "Transaction state list: Asked to update a transactionstate with ref=~p, but I can't find it", [Ref]),
    logger:log(debug, "Transaction state list: Asked to update a transactionstate with ref=~p, but I can't find it in list :~n~p",
	       [Ref, debugfriendly(TStateList)]),
    [];
update_transactionstate(Ref, NewT, [H | T], TStateList) when record(H, transactionstate), H#transactionstate.ref == Ref ->
    [NewT | T];
update_transactionstate(Ref, NewT, [H | T], TStateList) when record(H, transactionstate) ->
    lists:append([H], update_transactionstate(Ref, NewT, T, TStateList)).

%% Function: get_length/1
%% Description: Returns the length of the list record element of a
%%              transactionstatelist.
%% Returns: Length
%%--------------------------------------------------------------------
get_length(T) when record(T, transactionstatelist) ->
    length(T#transactionstatelist.list).

%% Function: debugfriendly/1
%% Description: Returns a list, suitable for debug logging (using ~p),
%%              describing the contents of a transactionstatelist.
%% Returns: Blob
%%          []
%%--------------------------------------------------------------------
debugfriendly([]) ->
    [];
debugfriendly(TStateList) when record(TStateList, transactionstatelist) ->
    debugfriendly2(TStateList#transactionstatelist.list, []);
debugfriendly(TState) when record(TState, transactionstate) ->
    debugfriendly2([TState], []).

debugfriendly2([], Res) ->
    lists:reverse(Res);
debugfriendly2([H | T], Res) when record(H, transactionstate) ->
    In = [{id, H#transactionstate.id}, {type, H#transactionstate.type}, {ref, H#transactionstate.ref}],
    Out = debugfriendly_non_empty([pid, ack_id, response_to_tag, appdata, branches], H, In),
    debugfriendly2(T, [Out | Res]).

debugfriendly_non_empty([], R, Res) ->
    lists:reverse(Res);
%% pid, not empty
debugfriendly_non_empty([pid | T], R, Res) when R#transactionstate.pid /= undefined, R#transactionstate.pid /= none ->
    debugfriendly_non_empty(T, R, [{pid, R#transactionstate.pid} | Res]);
%% response_to_tag, not empty
debugfriendly_non_empty([response_to_tag | T], R, Res) when R#transactionstate.response_to_tag /= undefined, R#transactionstate.response_to_tag /= none ->
    debugfriendly_non_empty(T, R, [{response_to_tag, R#transactionstate.response_to_tag} | Res]);
%% ack_id, not empty
debugfriendly_non_empty([ack_id | T], R, Res) when R#transactionstate.ack_id /= undefined, R#transactionstate.ack_id /= none ->
    debugfriendly_non_empty(T, R, [{ack_id, R#transactionstate.ack_id} | Res]);
%% appdata, not empty
debugfriendly_non_empty([appdata | T], R, Res) when R#transactionstate.appdata /= undefined, R#transactionstate.appdata /= none ->
    debugfriendly_non_empty(T, R, [{appdata, R#transactionstate.appdata} | Res]);
%% branches (length), not zero
debugfriendly_non_empty([branches | T], R, Res) when length(R#transactionstate.stateless_response_branches) /= 0 ->
    debugfriendly_non_empty(T, R, [{stateless_response_branches, length(R#transactionstate.stateless_response_branches)} | Res]);
%% empty, or unknown record field
debugfriendly_non_empty([_ | T], R, Res) ->
    debugfriendly_non_empty(T, R, Res).
