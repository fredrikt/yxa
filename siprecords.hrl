-record(request, {method, uri, header, body}).

-record(response, {status, reason, header, body}).

-record(via, {proto, host, port, param}).

-record(sipurl, {proto, user, pass, host, port, param}).

%%--------------------------------------------------------------------
%% keylist.erl records 
%%--------------------------------------------------------------------

%% a list wrapped in a record to allow better type checking and 
%% limiting direct access to it (use access functions supplied by
%% module keylist)
-record(keylist, {
	  list     % list() of keyelem record() with unique keys
	 }).
