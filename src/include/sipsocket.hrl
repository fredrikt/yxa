%%
%%--------------------------------------------------------------------

-record(sipsocket, {
	  module,	%% atom(), sipsocket module name to use for this socket
	  proto,	%% atom(), tcp | tcp6 | udp | udp6 | tls | tls6
	  pid,		%% pid(), socket connection handler
	  hostport,	%% hp record(), local/remote IP and port info
	  id		%% string(), unique id for this sipsocket instance - used in Outbound
	  %% XXX add SSL valid names here
	 }).

%% host+port record, used when we need to pass around local+remote IP+port
-record(hp, {
	  l_ip,		%% string(), local IP address
	  l_port,	%% integer(), local port number
	  r_ip,		%% string() | undefined (for listening sockets)
	  r_port	%% integer() | undefined (for listening sockets)
	 }).

%% sipdst is the Transport Layer destination record.
-record(sipdst, {
	  proto,	%% atom(), tcp | tcp6 | udp | udp6 | tls | tls6
	  addr,		%% string(), typically IP address
	  port,		%% integer()
	  uri,		%% sipurl record() | undefined
	  ssl_names,	%% list() of string(), name(s) to verify SSL certificate against
	  socket,	%% undefined | sipsocket record(), when Outbound wants a specific socket
	  instance	%% undefined | string(), set if sipdst is created from location db entry with instance id
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

% decoded SSL information
-record(ssl_conn_subject, {
	  countryName,		%% string()
	  organizationName,	%% string()
	  commonName,		%% string()
	  description		%% string()
	 }).

%% draft-outbound specific socket identifier
-record(ob_id, {
	  proto,	%% udp | ...
	  id		%% term()
	 }).
