-record(transactionstatelist, {list}).
-record(transactionstate, {ref, type, id, ack_id, branch, pid, appdata,
	request, response_to_tag, expire, stateless_response_branches}).

