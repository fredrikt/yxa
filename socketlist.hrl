%%
%% -------------------------------------------------------------------

-record(socketlist, {
	  list
	 }).

-record(socketlistelem, {
	  ref,		%% unique reference
	  id,		%% {listener, Proto, Port} | {in, Proto, Remote} | {out, Proto, Remote}
	  pid,		%% pid() of connection handler process
	  proto,	%% atom()
	  local,	%% {IP, Port} tuple() 
	  remote,	%% {IP, Port} tuple()
	  sipsocket,	%% term(), socket
	  expire	%% integer(), when this socket expires or 0 for never expire
	 }).
