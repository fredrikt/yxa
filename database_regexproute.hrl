%% Regexproute allows you to route requests based on regular
%% expression matching in incomingproxy.
%% -------------------------------------------------------------------

-record(regexproute, {
	  regexp,	% string(), the regular expression
	  flags,	% list() of {Key, Value}
	  class,	% atom(), permanent | ...
	  expire,	% integer() | never
	  address	% string(), the target address. must be parseable by sipurl:parse().
	 }).
