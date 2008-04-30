-define(STARTPAGE_URL, "index.yaws").

% common
-define(VARNAME_WHAT,			"what").
-define(VARNAME_VIRTACTION,		"virtaction").
-define(VARNAME_VIRTUSER,		"virtuser").
-define(VARNAME_VIRTADDRESS,		"virtaddress").

-define(COOKIENAME_SID,			"yxa_virtweb_sid").

% yssi-edit
-define(VARNAME_EDIT_NEW_ADDRESS,	"new_address").
-define(VARNAME_EDIT_PASSWORD,		"edit_password").
-define(VARNAME_EDIT_CPL,		"cpl_xml").

% yssi-dnscheck
-define(VARNAME_DNSCHECK_DOMAIN,	"domain").

%% login
-define(VARNAME_LOGIN_AUTHUSER,		"authuser").
-define(VARNAME_LOGIN_AUTHPASSWORD,	"authpassword").


-record(yxa_virtweb_db, {user,		%% string()
			 password,	%% string()
			 domains,	%% list() of string()
			 flags		%% list() of {Key, Value}
			}).
