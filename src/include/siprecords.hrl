%% YXA application context record, containing information passed from the YXA stack
%% to the application, for example when a new request is received.
-record(yxa_ctx, {
	  origin	:: sipsocket:siporigin(),	%% siporigin record, information about where a request/response was received from
	  logstr	:: nonempty_string(),	%% string(), describes request/response
	  thandler	:: undefined | transactionlayer:transaction_handler(),	%% undefined | term(), server transaction handler when processing requests

	  %% Application responsibilities - MIGHT NOT BE SET :
	  app_logtag	:: undefined | string()	%% undefined | string(), log prefix to use
	 }).

%% record returned by YXA applications init/0 functions
-record(yxa_app_init, {
	  sup_spec	= none	:: none | {append, term()},
	  mnesia_tables	= []	:: none | [atom()]			%% mnesia tables required online before starting
	 }).
	  
%% XXX can we assume that fields with the same names have the same possible types ?


%%--------------------------------------------------------------------
%% sipurl.erl records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% * see RFC 3261 chapter 19 for info on fields
%% * use sipurl:new and set functions to create and modify 
%%   records - data may otherwise become inconsistent
%% * Optional fields that may or may not be part of a sip-url are set 
%%   to 'none' if they are not found during url parsing or if they are
%%   not to be included in a url that is to be sent.
%%--------------------------------------------------------------------
-record(sipurl, {
	  proto :: nonempty_string(),  % "sip" | "sips" | "tel" (not yet supported)
                  % the protocol used  
	          % XXX current sipurl.erl code only handles string() (must be lower case) -
	  	  % "sip" or "sips", a consistent use of atom() would be preferable
	  
	  %% user info - must be case sensitive
	  user = none :: none | nonempty_string(),   % numeric phone no. or user part of sip url
	  pass = none :: none | nonempty_string(),   % a password
	  
	  %% host info - must be case insensitive
	  host :: HostName :: nonempty_string(),   % in lower case, usually a domain name (e.g. "su.se"), 
	          % but may also be a IPv4 address (e.g. "123.10.23.45") or a
	          % IPv6 references (e.g. "[1:2:3:4:5:6:7:8]") 
	  port = none :: none | non_neg_integer(),     % destination port at host
	          	  
	  %% uri-parameters - must be case insensitive
	  param_pairs = url_param:url_param()

	  %% headers
	  %% XXX headers are currently no supported
	 }).


%%--------------------------------------------------------------------
%% keylist.erl records 
%%--------------------------------------------------------------------

%% a list wrapped in a record to allow better type checking and 
%% limiting direct access to it (use access functions supplied by
%% module keylist)
-type keylist_keyelem() :: keylist:keyelem().
-record(keylist, {
	  list = [] :: [keylist_keyelem()]     % list() of keyelem record() with unique keys
	 }).


%%--------------------------------------------------------------------
%% commonly used request and response records
%%--------------------------------------------------------------------

-type sip_method()		:: nonempty_string().
-type sip_url()			:: #sipurl{}.
-type sip_headers()		:: #keylist{}.
-type sip_message_body()	:: binary().

%% @type     request() = #request{}.
%%              SIP request record, containing 'method', 'uri', 'header' and 'body'.
-record(request, {
	  method :: sip_method(), % type of request e.g. "REGISTER", "ACK", "CANCEL", "BYE", "INVITE", "MESSAGE" etc.
	  uri    :: sip_url(),
	  header :: sip_headers(),
	  body   :: sip_message_body()
	 }).

-type sip_request() :: #request{}.

-record(response, {
	  status :: non_neg_integer(),
	  reason :: string(),
	  header :: sip_headers(),
	  body   :: sip_message_body()
	 }).

-type sip_response() :: #request{}.

%% XXX there should be new / set functions to handle param_pairs in via (if we choose to use them)  
-record(via, {
	  proto		:: nonempty_string(),         % "SIP/2.0/PROTOCOL"
                         % the protocol used as a string() -
	                 % a consistent use of atom() would be preferable
	  host		:: nonempty_string(),          	% in lower case, usually a domain name (e.g. "su.se"), 
	                			% but may also be a IPv4 address (e.g. "123.10.23.45") or a
	                 % IPv6 references (e.g. "[1:2:3:4:5:6:7:8]") 
	  port		:: none | nonempty_string() | non_neg_integer(),	% destination port at host
	  param = []	:: [nonempty_string()]         % list() of "name=val" | "name" strings, in lower case        
	  %% XXX should this record also be equipped with a param_pairs field, as the sipurl record() ?
	 }).

%% stores a single SIP Request "Contact" header entry
%% 
-record(contact, {
	  display_name	:: none | nonempty_string(),
	  urlstr	:: nonempty_string(),        % "*" | string() (a raw sipurl without padding whitespaces)
	  contact_param	:: contact_param:contact_param()
	 }).


%%--------------------------------------------------------------------
%% dnsutil.erl records
%%--------------------------------------------------------------------

-type yxa_transport_proto() :: tcp | udp | tls | yxa_test.

%% SRV entry lookup result
-record(sipdns_srv, {
	  proto :: yxa_transport_proto(),
	  host  :: nonempty_string(),		%% hostname
	  port  :: non_neg_integer()
	  }).

%% host-port lookup result
-record(sipdns_hostport, {
	  family :: inet | inet6,
	  addr   :: nonempty_string(),		%% IPv4 or IPv6 address
	  port	 :: none | non_neg_integer()
	  }).

%%--------------------------------------------------------------------
%% siplocation.erl records
%%--------------------------------------------------------------------

%% Location DB entry
-record(siplocationdb_e, {
	  address	:: sip_url(),			%% the registered contact
	  sipuser	:: nonempty_string(),			%% SIP username for this binding
	  instance = []	:: string(),			%% instance ID
	  flags = []	:: [{Name :: atom(), Value :: any()}],
	  class		:: static | dynamic,
	  expire	:: never | non_neg_integer()		%% util:timestamp/0 time when this record expires
	  }).

-record(locationdb_socketid, {
	  node :: atom(),			%% node identifier
	  id :: any(),				%% sipsocket socket id
	  proto :: sipsocket:sipsocket_proto(),	%% origin proto
	  addr  :: sipsocket:sipsocket_addr(),	%% origin address (IP)
	  port  :: sipsocket:sipsocket_port()	%% origin port number
	 }).

%%--------------------------------------------------------------------
%% sipdialog.erl records
%%--------------------------------------------------------------------

-type yxa_name_header_uri() :: sip_url().
-type yxa_name_header() :: {DisplayName :: string() | none, URI :: yxa_name_header_uri()}.

%% RFC 3261 #12 (Dialogs)
-record(dialog, {callid			:: nonempty_string(),
		 local_cseq		:: non_neg_integer(),
		 remote_cseq		:: non_neg_integer(),
		 local_tag		:: nonempty_string(),
		 remote_tag	= none	:: none | nonempty_string(),
		 secure		= false	:: bool(),
		 route_set	= []	:: [nonempty_string()],
		 local_uri		:: yxa_name_header_uri(),		%% URI from From: or To: header
		 remote_uri		:: yxa_name_header_uri(),		%% URI from From: or To: header
		 remote_target		:: nonempty_string(),		%% URI from Contact: header
		 state			:: early | confirmed,

		 remote_uri_str		:: nonempty_string()	%% the exact From: header value
		 				%% of the request we received that initiated the dialog -
						%% needed when speaking to non-RFC-compliant clients.
		}).
