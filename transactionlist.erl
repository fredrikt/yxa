-module(transactionlist).
-export([add_transaction/4, get_transaction/2, get_transaction_using_header/2, 
	 get_transaction_using_response/2, get_transaction_using_request/2,
	 extract/2, set_response/2, set_cancelled/2, set_state/2,
	 update_transaction/2, debugfriendly/1, empty/0]).

-record(transactionlist, {list}).
-record(transaction, {ref, id, request, response, cancelled, state}).

% Transaction :
%
%	Id	    = CSeq {num, method}
%	Request	    = {Method, URI, Header, Body}
%	Response    = {Status, Reason, Header, Body}
%	Cancelled   = true or false
%	Acked	    = true or false
%	State	    = calling/trying, proceeding, completed, terminated
 
add_transaction(Request, Response, State, TList) when record(TList, transactionlist) ->
    {Method, URI, Header, Body} = Request,
    Id = sipheader:cseq(keylist:fetch("CSeq", Header)),
    case get_transaction_using_header(Header, TList) of
	none ->
	    NewT = #transaction{ref=make_ref(), id=Id, request=Request, response=Response, state=State},
	    #transactionlist{list=lists:append(TList#transactionlist.list, [NewT])};
	_ ->
	    logger:log(error, "transactionlist: Asked to add ~p request with duplicate Id ~p to list :~n~p",
	    	       [Method, Id, debugfriendly(TList)]),
	    TList
    end.

empty() ->
    #transactionlist{list=[]}.

get_transaction(Id, TransactionList) when record(TransactionList, transactionlist) ->
    %logger:log(normal, "FREDRIK: GET TRANSACTION ~p FROM LIST : ~n~p", [Id, TransactionList]),
    get_transaction_id(Id, TransactionList#transactionlist.list).
   
get_transaction_id(Id, []) ->
    %logger:log(debug, "transactionlist: No match looking for transaction ~p", [Id]),
    none;
get_transaction_id(Id, [H | T]) when record(H, transaction), H#transaction.id == Id ->
    H;
get_transaction_id(Id, [H | T]) when record(H, transaction) ->
    %logger:log(normal, "FREDRIK: NO MATCH ~p~nREST :~n~p", [H, T]),
    get_transaction_id(Id, T).

get_transaction_using_header(Header, TransactionList) when record(TransactionList, transactionlist) ->
    {CSeq, CSeqMethod} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    Id = case CSeqMethod of
	"ACK" -> {CSeq, "INVITE"};
	_ -> {CSeq, CSeqMethod}
    end,
    get_transaction(Id, TransactionList).

get_transaction_using_request(Request, TransactionList) ->
    {_, _, Header, _} = Request,
    get_transaction_using_header(Header, TransactionList).

get_transaction_using_response(Response, TransactionList) ->
    {Status, Reason, Header, _} = Response,
    get_transaction_using_header(Header, TransactionList).

extract(Values, Transaction) when record(Transaction, transaction) ->
    extract(Values, Transaction, []).

extract([], Transaction, Res) when record(Transaction, transaction) ->
    Res;
extract([id | T], Transaction, Res) when record(Transaction, transaction) ->
    extract(T, Transaction, lists:append(Res, [Transaction#transaction.id]));
extract([request | T], Transaction, Res) when record(Transaction, transaction) ->
    extract(T, Transaction, lists:append(Res, [Transaction#transaction.request]));
extract([response | T], Transaction, Res) when record(Transaction, transaction) ->
    extract(T, Transaction, lists:append(Res, [Transaction#transaction.response]));
extract([cancelled | T], Transaction, Res) when record(Transaction, transaction) ->
    extract(T, Transaction, lists:append(Res, [Transaction#transaction.cancelled]));
extract([state | T], Transaction, Res) when record(Transaction, transaction) ->
    extract(T, Transaction, lists:append(Res, [Transaction#transaction.state])).

set_response(Transaction, Value) when record(Transaction, transaction) ->
    Transaction#transaction{response = Value}.

set_cancelled(Transaction, Value) when record(Transaction, transaction) ->
    Transaction#transaction{cancelled = Value}.

set_state(Transaction, Value) when record(Transaction, transaction) ->
    Transaction#transaction{state = Value}.

update_transaction(Transaction, TList) when record(Transaction, transaction), record(TList, transactionlist) ->
    Ref = Transaction#transaction.ref,
    #transactionlist{list=update_transaction(Ref, Transaction, TList#transactionlist.list, TList)}.

update_transaction(Ref, _, [], TList) ->
    logger:log(error, "Transactionlist: Asked to update a transaction, but I can't find it", [Ref]),
    logger:log(error, "Transactionlist: Asked to update a transaction with ref=~p, but I can't find it in list :~n~p",
		[Ref, debugfriendly(TList)]),
    [];
update_transaction(Ref, NewT, [H | T], TList) when record(H, transaction), H#transaction.ref == Ref ->
    [NewT | T];
update_transaction(Ref, NewT, [H | T], TList) when record(H, transaction) ->
    lists:append([H], update_transaction(Ref, NewT, T, TList)).

debugfriendly(TList) when record(TList, transactionlist) ->
    debugfriendly(TList#transactionlist.list);
debugfriendly([]) ->
    [];
debugfriendly([H | Rest]) when record(H, transaction) ->
    {Method, URI, _, _} = H#transaction.request,
    RespStr = case H#transaction.response of
	none -> "no response";
	{Status, Reason, _, _} ->
	    lists:concat(["response=", Status, " ", Reason])
    end,
    {CSeq, _} = H#transaction.id,
    Cancelled = H#transaction.cancelled,
    State = H#transaction.state,
    Str = lists:concat([CSeq, " ", Method, ": ", sipurl:print(URI), ", ", RespStr, ", cancelled=", Cancelled, ", state=", State]), 
    lists:append([Str], debugfriendly(Rest)).
