%%
%% -------------------------------------------------------------------

-record(forward, {
	  number,	%% number (or SIP user) that has a forward
	  forwards,	%% list() of sipurl record()
	  timeout,	%% integer(), seconds to try these forwards
	  localring	%% true | false, ring non-forward entrys at
	  		%% the same time or not?
	 }).
