-record(pstn_ctx, {tags = [],		%% list() of atom()
		   ip,
		   cert_subject,
		   user,		%% undefined | bool()
		   stale_auth,		%% undefined | bool()
		   orig_uri,		%% original request-URI

		   called_number,	%% undefined | string(), input number
		   dst_number,		%% undefined | string(), rewritten number
		   dst_class,		%% undefined | atom(), class of dst_number
		   destination		%% undefined | sip | pstn
		  }).
