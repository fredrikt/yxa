-module(transactionlist).
-export([add_transaction/4, get_transaction/2, get_transaction_using_header/2, 
	 get_transaction_using_response/2, get_transaction_using_request/2,
	 extract_id/1, extract_request/1, extract_response/1, extract_cancelled/1, extract_state/1,
	 set_response/2, set_cancelled/2, set_acked/2, set_state/2,
	 update_transaction/2, debugfriendly/1]).

% RequestEntry :
%
%   {Id, Request, Response, Cancelled, Acked, State}
%
%	Id	    = CSeq {num, method}
%	Request	    = {Method, URI, Header, Body}
%	Response    = {Status, Reason, Header, Body}
%	Cancelled   = true or false
%	Acked	    = true or false
%	State	    = calling/trying, proceeding, completed, terminated
 
add_transaction(Request, Response, State, TransactionList) ->
    {Method, URI, Header, Body} = Request,
    Id = sipheader:cseq(keylist:fetch("CSeq", Header)),
    case get_transaction_using_header(Header, TransactionList) of
	none ->
	    Transaction = {Id, Request, Response, false, false, State},
	    lists:append(TransactionList, [Transaction]);
	_ ->
	    logger:log(error, "transactionlist: Asked to add ~p request with duplicate Id ~p to list :~n~p",
	    	       [Method, Id, debugfriendly(TransactionList)]),
	    TransactionList
    end.

get_transaction(Id, TransactionList) when list(TransactionList) ->
    case lists:keysearch(Id, 1, TransactionList) of
	false ->
	    %logger:log(debug, "transactionlist: No match looking for transaction ~p", [Id]),
	    none;
	{value, T} ->
	    T
    end.

get_transaction_using_header(Header, TransactionList) ->
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

extract_id({Id, Request, Response, Cancelled, Acked, State}) ->
    Id.
extract_request({Id, Request, Response, Cancelled, Acked, State}) ->
    Request.
extract_response({Id, Request, Response, Cancelled, Acked, State}) ->
    Response.
extract_cancelled({Id, Request, Response, Cancelled, Acked, State}) ->
    Cancelled.
extract_state({Id, Request, Response, Cancelled, Acked, State}) ->
    State.

set_response({Id, Request, _, Cancelled, Acked, State}, Response) ->
    {Id, Request, Response, Cancelled, Acked, State}.

set_cancelled({Id, Request, Response, _, Acked, State}, Cancelled) ->
    {Id, Request, Response, Cancelled, Acked, State}.

set_acked({Id, Request, Response, Cancelled, _, State}, Acked) ->
    {Id, Request, Response, Cancelled, Acked, State}.

set_state({Id, Request, Response, Cancelled, Acked, _}, State) ->
    {Id, Request, Response, Cancelled, Acked, State}.

update_transaction(Transaction, TransactionList) ->
    {Id, Request, Response, Cancelled, Acked, State} = Transaction,
    lists:keyreplace(Id, 1, TransactionList, Transaction).

debugfriendly([]) ->
    [];
debugfriendly([{Id, Request, Response, Cancelled, Acked, State} | Rest]) ->
    {Method, URI, _, _} = Request,
    RespStr = case Response of
	none -> "no response";
	{Status, Reason, _, _} ->
	    "response=" ++ integer_to_list(Status) ++ " " ++ Reason
    end,
    Str = Method ++ " " ++ sipurl:print(URI) ++ ", " ++ RespStr ++ ", cancelled=" ++ atom_to_list(Cancelled) ++ ", acked=" ++ atom_to_list(Acked) ++ ", state=" ++ atom_to_list(State), 
    lists:append([{Id, Str}], debugfriendly(Rest)).
