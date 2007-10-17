%% YXA application context record, containing information passed from the YXA stack
%% to the application, for example when a new request is received.
-record(yxa_ctx, {
	  origin,	%% siporigin record, information about where a request/response was received from
	  logstr,	%% string(), describes request/response
	  thandler,	%% undefined | term(), server transaction handler when processing requests

	  %% Application responsibilities - MIGHT NOT BE SET :
	  app_logtag	%% undefined | string(), log prefix to use
	 }).

%% record returned by YXA applications init/0 functions
-record(yxa_app_init, {
	  sup_spec	= none,	%% none | {append, SupSpec}
	  mnesia_tables	= []	%% list() of atom(), mnesia tables required online before starting
	 }).
	  
%% XXX can we assume that fields with the same names have the same possible types ?

%% @type     request() = #request{}.
%%              SIP request record, containing 'method', 'uri', 'header' and 'body'.
-record(request, {
	  method, % string(), type of request e.g. "REGISTER", "ACK", "CANCEL", "BYE", "INVITE", "MESSAGE" etc.
	  uri,    % sipurl record()
	  header, % keylist record()
	  body    % binary()
	 }).

-record(response, {
	  status, % integer()
	  reason, % string()
	  header, % keylist record()
	  body    % binary()
	 }).

%% XXX there should be new / set functions to handle param_pairs in via (if we chose to use them)  
-record(via, {
	  proto,         % "SIP/2.0/PROTOCOL"
                         % the protocol used as a string() -
	                 % a consistent use of atom() would be preferable
	  host,          % string(), in lower case, usually a domain name (e.g. "su.se"), 
	                 % but may also be a IPv4 address (e.g. "123.10.23.45") or a
	                 % IPv6 references (e.g. "[1:2:3:4:5:6:7:8]") 
	  port,          % none | string() (a numeric string) | integer()
	                 % destination port at host
	  param          % list() of "name=val" | "name" strings, in lower case        
	  %% XXX should this record also be equipped with a param_pairs field, as the sipurl record() ?
	 }).

%% stores a single SIP Request "Contact" header entry
%% 
-record(contact, {
	  display_name,  % none | string()
	  urlstr,        % "*" | string() (a raw sipurl without padding whitespaces)
	  contact_param  % contact_param record()
	 }).

%%--------------------------------------------------------------------
%% * see RFC 3261 chapter 19 for info on fields
%% * use sipurl:new and set functions to create and modify 
%%   records - data may otherwise become inconsistent
%% * Optional fields that may or may not be part of a sip-url are set 
%%   to 'none' if they are not found during url parsing or if they are
%%   not to be included in a url that is to be sent.
%%--------------------------------------------------------------------
-record(sipurl, {
	  proto,  % "sip" | "sips" | "tel" (not yet supported)
                  % the protocol used  
	          % XXX current sipurl.erl code only handles string() (must be lower case) -
	  	  % "sip" or "sips", a consistent use of atom() would be preferable
	  
	  %% user info - must be case sensitive
	  user,   % none | string(), numeric phone no. or user part of sip url
	  pass,   % none | string(), a password
	  
	  %% host info - must be case insensitive
	  host,   % string(), in lower case, usually a domain name (e.g. "su.se"), 
	          % but may also be a IPv4 address (e.g. "123.10.23.45") or a
	          % IPv6 references (e.g. "[1:2:3:4:5:6:7:8]") 
	  port,   % none | integer()
	          % destination port at host
	          	  
	  %% uri-parameters - must be case insensitive
	  param_pairs    % url_param record()

	  %% headers
	  %% XXX headers are currently no supported
	 }).


%%--------------------------------------------------------------------
%% keylist.erl records 
%%--------------------------------------------------------------------

%% a list wrapped in a record to allow better type checking and 
%% limiting direct access to it (use access functions supplied by
%% module keylist)
-record(keylist, {
	  list     % list() of keyelem record() with unique keys
	 }).

%%--------------------------------------------------------------------
%% url_param.erl records
%%--------------------------------------------------------------------

%% a term() wrapped in a record to allow better type checking and 
%% limiting direct access to it (use access functions supplied by 
%% url_param.erl)
-record(url_param, {
	  pairs
	 }).

%%--------------------------------------------------------------------
%% contact_param.erl records
%%--------------------------------------------------------------------

%% a term() wrapped in a record to allow better type checking and 
%% limiting direct access to it (use access functions supplied by 
%% contact_param.erl)
-record(contact_param, {
	  pairs
	 }).

%%--------------------------------------------------------------------
%% dnsutil.erl records
%%--------------------------------------------------------------------

%% SRV entry lookup result
-record(sipdns_srv, {
	  proto,	%% tcp | udp | tls
	  host,		%% string(), hostname
	  port		%% integer()
	  }).

%% host-port lookup result
-record(sipdns_hostport, {
	  family,	%% inet | inet6
	  addr,		%% string(), IPv4 or IPv6 address
	  port		%% integer() | none
	  }).

%%--------------------------------------------------------------------
%% siplocation.erl records
%%--------------------------------------------------------------------

%% Location DB entry
-record(siplocationdb_e, {
	  address,	%% sipurl record(), the registered contact
	  sipuser,	%% string(), SIP username for this binding
	  instance,	%% string(), instance ID
	  flags,	%% list() of {Name, Value}
	  class,	%% atom(), static | dynamic
	  expire	%% integer(), util:timestamp/0 time when this record expires
	  }).

-record(locationdb_socketid, {
	  node,		%% atom(), node identifier
	  id,		%% term(), sipsocket socket id
	  proto,	%% atom(), origin proto
	  addr,		%% string(), origin address (IP)
	  port		%% integer(), origin port number
	 }).

%%--------------------------------------------------------------------
%% sipdialog.erl records
%%--------------------------------------------------------------------

%% RFC 3261 #12 (Dialogs)
-record(dialog, {callid,
		 local_cseq	= 1,		%% integer()
		 remote_cseq	= 1,		%% integer()
		 local_tag,			%% string()
		 remote_tag	= none,		%% string() | none
		 secure		= false,	%% true | false
		 route_set,			%% list() of string()
		 local_uri,			%% string(), URI from From: or To: header
		 remote_uri,			%% string(), URI from From: or To: header
		 remote_target,			%% string(), URI from Contact: header
		 state,				%% early | confirmed

		 remote_uri_str			%% undefined | string(), the exact From: header value
		 				%% of the request we received that initiated the dialog -
						%% needed when speaking to non-RFC-compliant clients
		}).

