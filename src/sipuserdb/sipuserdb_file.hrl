-record(user, {
	  name,		%% string(), username
	  password,	%% string(), password
	  classes,	%% list() of atom()
	  forward	%% list() of sipproxy_forward record() | undefined - currently
	  		%% not supported by sipuserdb_file, so always 'undefined'
	 }).
-record(address, {
	  user,		%% string(), username
	  address,	%% string(), address
	  url		%% sipurl record() | undefined, address parsed with sipurl:parse()
	 		%%                              or undefined for tel: URL
	 }).
