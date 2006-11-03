%%--------------------------------------------------------------------
%% database_evendata.erl records
%%--------------------------------------------------------------------
-record(eventdata_dbe, {
	  presentity,	%% term(), presentity this data belongs to ({user, User} | {address, AddressStr} | ...)
	  etag,		%% term(), event package record reference
	  expires,	%% never | integer(), util:timestamp() time when this record expires
	  flags,	%% list() of {Key, Value} for future use
	  data		%% term(), event package data
	 }).

%% Record for passing context information to event packages request/7 function
-record(event_ctx, {
	  sipuser,	%% undefined | string(), authenticated SIP user
	  presentity,	%% term(), presentity for subscription
	  dialog_id	%% undefined | tuple() of arity 3, dialog identifier
	 }).
