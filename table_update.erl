%% this code updates older database tables - code commited before 
%% 25-10-2004
%% 
%%--------------------------------------------------------------------

-module(table_update).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------

-export([
	 update/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("phone.hrl").
-include("siprecords.hrl").
-include("database_regexproute.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: update databases
%% Returns : 
%%--------------------------------------------------------------------
update() ->
    logger:log(debug, "Checking if any mnesia tables needs updating"),
    phone(),
    regexproute().

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: phone record got two new fields, add dummy fields for old
%%           existing database entries
%% Returns : 
%%--------------------------------------------------------------------
phone() ->
    put(update, false),
    F = fun
	    %% check for old record lacking callid and cseq field
	    ({phone, Number, Flags, Class, Expire, Address, ReqUriStr}) -> 
		put(update, true),
		#phone{
		     number = Number,
		     flags = Flags, 
		     class = Class, 
		     expire = Expire,	
		     address = Address,
		     requristr = ReqUriStr, 
		     callid = "", 
		     cseq = 0    
		    };
	    %% debug related patch - to fix when new phone entries got improperly updated - 
	    %% forgot setting CallId and CSeq field values
	    ({phone, Number, Flags, Class, Expire, Address, ReqUriStr, undefined, undefined}) -> 
		put(update, true),
		#phone{
		     number = Number,
		     flags = Flags, 
		     class = Class, 
		     expire = Expire,	
		     address = Address,
		     requristr = ReqUriStr, 
		     callid = "", 
		     cseq = 0
		    };    
	    %% nothing to update
	    ({phone, _Number, _Flags, _Class, _Expire, _Address, _ReqUriStr, _CallId, _CSeq} = Phone) -> 
		Phone
	end,
    {atomic, ok} = mnesia:transform_table(phone, F, record_info(fields, phone)),

    case erase(update) of
	true -> 
	    logger:log(normal, "phone: updated");
	false ->
	    true
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: update the sipurl record() in the regexproute
%% Returns : 
%%--------------------------------------------------------------------
regexproute() ->
    put(update, false),
    F = fun
	    %% check for old sipurl's lacking url_param field
	    ({regexproute, 
	      Regexp, 
	      Flags, 
	      Class, 
	      Expire, 
	      {
		sipurl,
		Proto, 
		User,
		Pass,
		Host,
		Port,
		Param        
	       }
	     }) ->
		put(update, true),
		#regexproute{
		     regexp = Regexp, 
		     flags = Flags, 
		     class = Class, 
		     expire = Expire, 
		     %% fixes so that url_param record is used
		     address = sipurl:new([{proto, Proto}, {user, User}, {pass, Pass},
					   {host, Host}, {port, Port}, {param, Param}])
		    };
	    %% nothing to update
	    (RegExpRoute)  ->
		RegExpRoute
	end,
    {atomic, ok} = mnesia:transform_table(regexproute, F, record_info(fields, regexproute)),

    case erase(update) of
	true -> 
	    logger:log(normal, "regexproute: updated");
	false ->
	    true
    end.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
