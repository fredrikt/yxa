%%
%%--------------------------------------------------------------------

-record(sipsocket, {
	  module,	%% atom(), sipsocket module name to use for this socket
	  proto,	%% atom(), tcp | tcp6 | udp | udp6 | tls | tls6
	  pid,		%% pid(), socket connection handler
	  data,		%% term(), information about local and remote endpoints
	  		%%         or just local if this is a listener socket
	  id		%% string(), unique id for this sipsocket instance - used in Outbound
	  %% XXX add SSL valid names here
	 }).

%% sipdst is the Transport Layer destination record.
-record(sipdst, {
	  proto,	%% atom(), tcp | tcp6 | udp | udp6 | tls | tls6
	  addr,		%% string(), typically IP address
	  port,		%% integer()
	  uri,		%% sipurl record() | undefined
	  ssl_names,	%% list() of string(), name(s) to verify SSL certificate against
	  socket	%% undefined | sipsocket record(), when Outbound wants a specific socket
	 }).

%% Information about the origin of a received SIP request/response
-record(siporigin, {
	  proto,    % atom(), tcp | tcp6 | udp | udp6 | tls | tls6
	  addr,     % string()
	  port,     % integer()
	  receiver, % pid(), pid of receiver process
	  sipsocket % sipsocket record()
	 }).

%% information about a listener
-record(yxa_sipsocket_info_e, {
	  proto,	% tcp | tcp6 | udp | udp6 | tls | tls6
	  addr,		% string(), local address we have bound to
	  port		% integer(), port number we are listening on
	 }).
