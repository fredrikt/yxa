%%
%% XXX #sipurl.port and #via.port are currently being used confusingly
%% by allowing both string() and integer() as input. YXA code should 
%% use either string() or integer() - preferably integer().
%% - This is currently solved by requiring users to call 
%% sipurl:get_port/1 to get a well defined return value.
%%--------------------------------------------------------------------

%% XXX can we assume that fields with the same names have the same possible types ?

-record(request, {
	  method, % string(), type of request e.g. "REGISTER", "ACK", "CANCEL", "BYE", "INVITE", "MESSAGE" etc.
	  uri,    % sipurl record()
	  header, % keylist record()
	  body    % string()
	 }).

-record(response, {
	  status, % integer()
	  reason, % string()
	  header, % keylist record()
	  body    % string()
	 }).

%% XXX there should be new / set functions to handle param_pairs in via (if we chose to use them)  
-record(via, {
	  proto,         % "sip" | "sips" (not yet supported)
                         % the protocol used as a string() -
	                 % a consistent use of atom() would be preferable
	  host,          % string(), in lower case, usualy a domain name ("." separated text e.g. "su.se"), 
	                 % but may also be a IPv4 address (e.g. "123.10.23.45") or a
	                 % IPv6 references (e.g. "[1:2:3:4:5:6:7:8]") 
	  port,          % none | string() (a numeric string) | integer()
	                 % destination port at host
	  param          % list() of "name=val" | "name" strings, in lower case        
	  %% XXX should this record also be equiped with a param_pairs field, as the phone record() ?
	 }).

%% stores a single SIP Request "Contact" header entry - "Contact" can use "," 
%% to include several entries in a header
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
	  proto,  % "sip" | "sips" (not yet supported) | "tel"
                  % the protocol used  
	          % XXX current sipurl.erl code only handles string() (must be lower case) - "sip"
	          % a consistent use of atom() would be preferable
	  
	  %% user info - must be case sensitive
	  user,   % none | string(), numeric phone no. or user part of sip url
	  pass,   % none | string(), a password
	  
	  %% host info - must be case insensitive
	  host,   % string(), in lower case, usualy a domain name ("." separated text e.g. "su.se"), 
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
	  address,	%% string(), parseable with sipurl:parse(...)
	  flags,	%% list() of {Name, Value}
	  class,	%% atom(), static | dynamic
	  expire	%% integer(), util:timestamp/0 time when this record expires
	  }).
