%%
%%
%% -------------------------------------------------------------------

%% Container record to make sure noone modifies a transaction list
%% without using functions exported by transactionstatelist module
-record(transactionstatelist, {
	  list,
	  tables
	 }).
-type transactionstatelist() :: #transactionstatelist{}.
