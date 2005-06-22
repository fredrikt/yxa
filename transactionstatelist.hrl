%%
%%
%% -------------------------------------------------------------------

%% Container record to make sure noone modifies a transaction list
%% without using functions exported by transactionstatelist module
-record(transactionstatelist, {
	  list,
	  tables
	 }).

%% Transaction layer data about ongoing transactions
-record(transactionstate, {
	  ref,		%% unique reference 
	  type,		%% server | client 
	  id,		%% identification data for matching requests/responses to this transaction 
	  ack_id,	%% special id for matching ACK to INVITE 
	  pid,		%% pid() of transaction handler process
	  appdata,	%% term(), data about this transaction stored by an application
	  response_to_tag, %% the To-tag used in responses to this transaction that we have generated ourselves
	  expire,	%% integer(), when in time this transaction expires
	  stateless_response_branches=[], %% list() of string(), branches we have used when proxying this request on statelessly - DONT USE
	  description,	%% string(), just used for displaying (in stack_monitor and debug logs etc.)
	  result=none	%% string(), just used for displaying (in stack_monitor and debug logs etc.)
	 }).
