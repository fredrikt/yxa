%%
%% -------------------------------------------------------------------

-record(sipproxy_action, {
	  action, 
	  timeout, 
	  requri
	 }).

%% sipproxy response record. only used internally when processing responses,
%% and stored in our 'response context' (targetlist that is).
-record(sp_response, {
	  status,	%% integer(), same as in record 'response'
	  reason,	%% string(), same as in record 'response'
	  header,	%% keylist record(), same as in record 'response'
	  body,		%% binary(), same as in record 'response'
	  created	%% true | false, was this a response record or not?
	  }).

