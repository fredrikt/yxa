%%
%%--------------------------------------------------------------------

-record(sipsocket, {
	  module, 
	  proto, 
	  pid, 
	  data
	 }).

%% sipdst is the Transport Layer destination record.
-record(sipdst, {
	  proto, 
	  addr, 
	  port, % integer() (siprequest.erl - are other formats ok ? 
		%            they won't work in get_response_socket/4)
	  uri
	 }).

-record(siporigin, {
	  proto, 
	  addr, 
	  port, 
	  receiver, 
	  sipsocket
	 }).
