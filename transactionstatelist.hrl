%%
%%
%% -------------------------------------------------------------------

%% Container record to make sure noone modifies a transaction list
%% without using functions exported by transactionstatelist module
-record(transactionstatelist, {
	  list
	 }).

%% Transaction layer data about ongoing transactions
-record(transactionstate, {
	  ref, 
	  type, 
	  id, 
	  ack_id, 
	  branch, 
	  pid, 
	  appdata,
	  request, 
	  response_to_tag, 
	  expire, 
	  stateless_response_branches
	 }).

