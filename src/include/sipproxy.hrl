%%
%% -------------------------------------------------------------------

-record(sipproxy_action, {
	  action,	%% call | wait
	  timeout,	%% integer()
	  requri,	%% sipurl record(), Request-URI for location db entry
	  path = [],	%% list() of string(), RFC3327 Path for location db entry
	  user,		%% undefined | string(), SIP username if this was a location db entry, or owner of
	                %% CPL script if the location was added through CPL
	  instance	%% undefined | string(), Instance ID if this was a location db entry that had one
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

%% the functions that read forwards from a database returns them in this form
-record(sipproxy_forward, {
	  user,		%% SIP username to make this forward call as (appserver/pstnproxy)
	  forwards,	%% list() of sipurl record()
	  timeout,	%% integer(), number of seconds to call these forwards
	  localring	%% true | false, whether to ring on location database entrys
	  		%% at the same time as the forward destinations or not
	 }).
