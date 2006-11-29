-record(pstn_ctx, {tags = [],		%% list() of atom()
		   ip,
		   cert_subject,
		   user,		%% undefined | bool()
		   stale_auth,		%% undefined | bool()
		   orig_uri,		%% original request-URI

		   called_number,	%% string()
		   destination		%% undefined | sip | pstn
		  }).
