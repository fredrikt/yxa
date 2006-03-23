%%--------------------------------------------------------------------
%% stun.erl records
%%--------------------------------------------------------------------

%% this file is not in src/include/ since the contents is completely
%% internal to the transport layer

-record(stun_env, {
	  %% Not really STUN request related, more a number of parameters we
	  %% need to know to create answers

	  proto,		%% udp | udp6 | ...
	  local_ip,		%% tuple(), local IP address for socket we received STUN packet on
	  local_port,		%% tuple(), local port number for socket we received STUN packet on
	  
	  remote_ip,		%% tuple(), source IP of STUN packet
	  remote_ip_str,	%% string(), source IP of STUN packet
	  remote_port,		%% integer(), source port of STUN packet
	  
	  alt_ip,		%% undefined | tuple(), alternate IP address available for responding
	  alt_port,		%% undefined | tuple(), alternate port number available for responding

	  allow_no_msg_integr = true	%% should we allow requests with the RFC3489bis magic cookie,
	  				%% without MESSAGE-INTEGRITY? Probably needed for draft-sip-outbound
	 }).


-record(stun_result, {
	  action,		%% send_response
	  type,			%% ok | error
	  change = none,	%% none | ip | port | both
	  response		%% io_list() ([STUN_Header, STUN_Attributes], two binaries)
	 }).
	
