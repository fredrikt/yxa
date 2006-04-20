%% CPL related records
%%--------------------------------------------------------------------

%% macros that give the number of seconds for different time periods

-define(SecInMin, 60).
-define(SecInHour, (60 * 60)).
-define(SecInDay, (60 * 60 * 24)).
-define(SecInWeek, (60 * 60 * 24 * 7)).        
-define(SecInYear, (60 * 60 * 24 * 7 * 365)).        
-define(SecInLeapYear, (60 * 60 * 24 * 7 * 366)).        

%%--------------------------------------------------------------------
%% node_code stores the information needed about how to execute the
%% actions of the node
%%--------------------------------------------------------------------
-record(node_code, {
	  %% atom(), the name of a "top level" tag (any tag not a sub
	  %% member of a switch)
	  type,    
	  
	  %% the "code" of the node
	  statements
	 }).

%%--------------------------------------------------------------------
%% store time zone info form time-switch tag
%%--------------------------------------------------------------------
-record(time_zone, {
	  tzid, % string() | none (if no value is supplied) 
	  tzurl % string() | none (if no value is supplied)
	 }).

%%--------------------------------------------------------------------
%% stores a RFC 2445 DATE-TIME entry, used in the attributes of the 
%% time tag in a time-switch
%% XXX do time ranges match those used by calendar.erl in OTP ?
%%--------------------------------------------------------------------
-record(date_time, {
	  date, % {Year, Month, Day}, values = range [0,9999], [1,12] and [1,31]
	  time, % {Hour, Minute, Second}, values = range [0,23], [0,59] and [0,60] (60 = leap second)
	  type  % floating | utc - floating time is a timevalue 
	        % without any reference point, while utc is time 
	        % according to UTC (successor of Greenwich Mean Time)
	 }).

%%--------------------------------------------------------------------
%% stores a RFC 2445 DURATION entry, used in the attributes of the 
%% time tag in a time-switch
%%--------------------------------------------------------------------
-record(duration, {
	  weeks = 0,   % integer()
	  days = 0,    % integer()
	  hours = 0,   % integer()
	  minutes = 0, % integer()
	  seconds = 0  % integer()
	 }).

%%--------------------------------------------------------------------
%% stores the attributes in a time tag (used by time-switch) in a 
%% format suitable for later graph/fsm processing.
%% There are several versions of the time_switch__cond_X record which
%% hold the various possible combination of attributes - some which 
%% require / exclude each other 
%%--------------------------------------------------------------------
-record(time_switch__cond_8, {
	  dtstart,        % date_time record()
	  dtend_duration, % {dtend, Time} | {duration, Duration}
	                  % Time = date_time record()
	                  % Duration = duration record()
	  freq,           % atom() - secondly | minutely | hourly | daily | weekly | monthly | yearly
	  interval,       % integer() >= 1
	  until_count,    % {until, Time} | {count, Count} | repeat_forever 
                          % Time = date_time record() | {Year, Month, Day} (as in #date_time.date)
	                  % Count = integer() >= 1
	  by_values,      % list() of {Key,Val} entries, containing any of:
	                  % {bysecond, Val}    Val = integer() in range [0,59]
	                  % {byminute, Val}    Val = integer() in range [0,59]
                          % {byhour, Val}      Val = integer() in range [0,23]
	                  % {byday, Val}       Val = {N, Day} 
			  %                        Day = mo | tu | we | th | fr | sa | su |  
	                  %                        N   = -1 or less | 1 or greater | all (default)
	                  % {bymonthday, Val}  Val = integer() in range [1,31] | [-1,-31]
	                  % {byyearday, Val}   Val = integer() in range [1,366] | [-1,-366]
	                  % {byweekno, Val}    Val = integer() in range [1,53] | [-1,-53]
	                  % {bymonth, Val}     Val = integer() in range [1,12]
    	  wkst,           % atom() - mo | tu | we | th | fr | sa | su
	  bysetpos,       % list() of sorted integer(), in range = [1,366] | [-1,-366]

	  time_ranges     % stores the return value of interpret_time:get_count_ranges_8/2
}).

-record(time_switch__cond_7, {
	  dtstart,  
	  dtend_duration,
	  freq,          
	  interval,      
	  until_count,   
	  by_values,     
    	  wkst,

	  time_ranges     % stores the return value of interpret_time:get_count_ranges_7/2
}).

-record(time_switch__cond_5, {
	  dtstart,  
	  dtend_duration,
	  freq,          
	  interval,      
	  until_count,

	  time_ranges     % stores the return value of interpret_time:get_count_ranges_5/2
}).

-record(time_switch__cond_2, {
	  dtstart,  
	  dtend_duration
}).

%%--------------------------------------------------------------------
%% Records used for location modifiers 
%%--------------------------------------------------------------------

-record(location__attrs, {
	  url,            % string(), mandatory 
	  priority = 1.0, % 0.0 - 1.0, default = 1.0
	  clear = no      % yes | no, default = no
}).

-record(lookup__attrs, {
	  source,         % string(), mandatory - may be url | other value
	  timeout = 30,   % integer(), seconds to wait
	  clear = no      % yes | no, default = no           XXX RFC 3880 doesn't say what the default should be but 
	                                                   % the same default as for location seams likely
	 }).

-record(remove_location__attrs, {
	  location = all  % string() - a url | 
			  % all (if no location attribute is supplied in remove-location tag)
	 }).

%%--------------------------------------------------------------------
%% Records used for actions
%%--------------------------------------------------------------------

-record(log__attrs, {
	  name,    % default (refers to the standard log) | string() 
	  comment  % string()
	 }).

-record(proxy__attrs, {
	  timeout, % integer() | server_max (if server determines ring time)
	  recurse, % yes | no
	  ordering % parallel | sequential | 'first-only' 
	 }).

-record(reject__attrs, {
	  status,     % integer(), sip error code
	  reason = "" % string()
	 }).

%%--------------------------------------------------------------------
%% Record used to store information about a location, used by the 
%% implicit CPL location db 
%%--------------------------------------------------------------------
-record(location, {
	  url,            % sipurl record()
	  ldbe,           % undefined | siplocationdb_e record(), original data if from location db
	  priority = 1.0  % float(), 0.0 - 1.0       XXX location tag uses 1.0 as default, so this may be ok ?  
	 }).

%%--------------------------------------------------------------------
%% internal record of interpret_time.erl only exported for use in 
%% interpret_time_test.erl
%%--------------------------------------------------------------------
-record(start_time, {
	  year,
	  month,
	  weekno,   % {Year, WeekNo}
	  yearday,
	  monthday,
	  weekday,  % mo | tu | we | th | fr | sa | su
	  hour,
	  minute,
	  second
	 }).
