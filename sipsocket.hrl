%%
%%--------------------------------------------------------------------

-record(sipsocket, {
	  module, % atom(), sipsocket module name to use for this socket
	  proto,  % atom(), tcp|tcp6|udp|udp6|tls|tls6 ...
	  pid,    % pid(), socket connection handler
	  data    % term(), information about local and remote endpoints
	  	  %         or just local if this is a listener socket
	 }).

%% sipdst is the Transport Layer destination record.
-record(sipdst, {
	  proto, % string() lower case, e.g. "sip" | ... , protocol
	  addr,  % string(), typically IP address
	  port,  % integer()
	  uri    % sipurl record() | undefined
	 }).

-record(siporigin, {
	  proto,    % atom(), tcp|tcp6|udp|udp6|tls|tls6 ...
	  addr,     % string()
	  port,     % integer()
	  receiver, % pid(), pid of receiver process
	  sipsocket % sipsocket record()
	 }).

