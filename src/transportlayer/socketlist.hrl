%%
%% -------------------------------------------------------------------

-record(socketlist, {
	  list		%% list() of socketlistelem record()
	 }).

-record(socketlistelem, {
	  ref,		%% unique reference
	  id,		%% yxa_socket_ident record()
	  pid,		%% pid() of connection handler process
	  proto,	%% atom()
	  hostport,	%% hp record(), local/remote IP/port info
	  sipsocket,	%% term(), socket
	  expire	%% integer(), when this socket expires or 0 for never expire
	 }).
