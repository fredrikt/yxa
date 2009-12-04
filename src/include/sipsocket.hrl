%%
%%--------------------------------------------------------------------

-type sipsocket_proto() :: tcp | tcp6 | udp | udp6 | tls | tls6 | yxa_test | yxa_test6.
-type sipsocket_addr() :: string().
-type sipsocket_port() :: non_neg_integer().


%% host+port record, used when we need to pass around local+remote IP+port
-record(hp, {
	  l_ip		:: string(),				%% local IP address
	  l_port	:: sipsocket_port(),			%% local port number
	  r_ip		:: string() | undefined, 		%% (undefined is for listening sockets)
	  r_port	:: sipsocket_port() | undefined		%% (undefined is for listening sockets)
	 }).

%% draft-outbound specific socket identifier
-record(ob_id, {
	  proto	:: sipsocket_proto(),
	  id	:: any()
	 }).

-record(sipsocket, {
	  module	:: atom(),		%% sipsocket module name to use for this socket
	  proto		:: sipsocket_proto(),
	  pid		:: pid(),		%% socket connection handler
	  hostport	:: #hp{},		%% local/remote IP and port info
	  id		:: #ob_id{} 		%% unique id for this sipsocket instance - used in Outbound
	  %% XXX add SSL valid names here
	 }).

%% sipdst is the Transport Layer destination record.
-record(sipdst, {
	  proto		 :: sipsocket_proto(),
	  addr		 :: sipsocket_addr(),	%% typically IP address
	  port		 :: sipsocket_port(),
	  uri,					%% #sipurl{}, specified in siprecords.hrl
	  ssl_names = [] :: [string()],		%% name(s) to verify SSL certificate against
	  socket	 :: #sipsocket{},	%% when Outbound wants a specific socket
	  instance	 :: [string()]		%% set if sipdst is created from location db entry with instance id
	 }).

%% Information about the origin of a received SIP request/response
-record(siporigin, {
	  proto		:: sipsocket_proto(),
	  addr		:: sipsocket_addr(),
	  port		:: sipsocket_port(),
	  receiver	:: pid(),		% pid(), pid of receiver process
	  sipsocket	:: #sipsocket{}		% sipsocket record()
	 }).

-type siporigin() :: #siporigin{}.

%% information about a listener
-record(yxa_sipsocket_info_e, {
	  proto	:: sipsocket_proto(),
	  addr	:: sipsocket_addr(),	% local address we have bound to
	  port	:: sipsocket_port()	% port number we are listening on
	 }).

% decoded SSL information
-record(ssl_conn_subject, {
	  countryName		:: string(),
	  organizationName	:: string(),
	  commonName		:: string(),
	  description		:: string()
	 }).
