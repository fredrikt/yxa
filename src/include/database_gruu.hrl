-record(gruu_dbe, {
	  gruu,			%% string()
	  sipuser,		%% string()
	  instance_id,		%% string()
	  created,		%% integer(), util:timestamp() notion
	  last_registered,	%% integer(), util:timestamp() notion
	  flags			%% list() of {Key, Value} tuple(), for future use
	 }).
