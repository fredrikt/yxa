%%
%%--------------------------------------------------------------------

-record(sipsocket, {
	  module, % atom(), sipsocket module name to use for this socket
	  proto,  % atom(), tcp|tcp6|udp|udp6|tls|tls6 ...
	  pid,    % pid(), socket connection handler
	  data    % term()
	 }).

%% sipdst is the Transport Layer destination record.
-record(sipdst, {
	  proto, % string() lower case, e.g. "sip" | ... , protocol
	  addr,  % string() | sipurl record() ?
	  port,  % integer() (siprequest.erl - are other formats ok ? 
		 %            they won't work in get_response_socket/4)
	  uri    % sipurl record() ?
	 }).

-record(siporigin, {
	  proto,    % string() lower case, e.g. "sip" | ... , protocol
	  addr,     % string() | sipurl record() ?
	  port,     % string() | integer()  ?
	  receiver, % string() | sipurl record() ?
	  sipsocket % sipsocket record() ?
	 }).

