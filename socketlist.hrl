%%
%% -------------------------------------------------------------------

-record(socketlist, {
	  list
	 }).

-record(socketlistelem, {
	  ref, 
	  id, 
	  pid, 
	  local, 
	  remote, 
	  sipsocket, 
	  expire
	 }).

