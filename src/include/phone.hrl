%%
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% Table 'phone' is the location database. One entry for every phone
%% (or, more correctly, User Agent) that REGISTERs for one of our
%% users.
%% phone record is used to map a user (number) to one UAC (address and
%% requristr). Several UACs can be configured to produce the same
%% user (number) by sending the same "To" field in a REGISTER request,
%% thereby mapping several UACs to the same user.
%% -------------------------------------------------------------------
-record(phone, {	%% 'location' would should be a better record name, 'phone' is legacy
	  user,		%% string(), sipuser authentication name (from sipuserdb:get_user_with_address(...))
	  		%% acts as "adress of record" (AOR) (see RFC 3261 chapter 10 p65)
	  flags,	%% list of {Name,Value}
          		%% Name  = atom()
          		%% Value = term()
	  class,	%% dynamic | static | ...
	  expire,	%% integer(), util:timestap/0 value, stores the time at which the phone database entry
	  		%% is no longer to be used and should be removed
	  		%% Note: this differs from the expire value used in the REGISTER request (see RFC 3261)
	  		%% which is used to indicate how long a registration is to last i.e.
	  		%% "phone.expire = time of registration + request expire"
	  address,	%% string(), sip url with parameters
	  requristr,	%% string(), sip url
	  callid,	%% string(), a sequence of bytes
	  cseq,		%% integer()
	  instance	%% string(), Instance ID, unique identifier of UA. Used for GRUU and Outbound.
	 }).

%% -------------------------------------------------------------------
%% Table 'user' is the user database if you are using
%% sipuserdb_mnesia.
%% -------------------------------------------------------------------
-record(user, {
	  user,		% string(), username
	  password,	% term() (for now, string() is recommended)
	  number,	% unused
	  flags,	% list() of atom()
	  classes	% list() of atom()
	 }).

%% -------------------------------------------------------------------
%% Table 'numbers' maps extra addresses (besides the first one -
%% ('number' in table 'user') to users. Note well that
%% sipuserdb_mnesia also has implicit mappings of addresses to users
%% through lookup:lookup_url_to_addresses.
%% -------------------------------------------------------------------
-record(numbers, {
	  user,		% string(), same as #user.user
	  number	% string(), something that can be parsed by sipurl:parse()
	 }).
