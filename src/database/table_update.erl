%%%-------------------------------------------------------------------
%%% File    : table_update.erl
%%% @author   Håkan Stenholm <hsten@it.su.se>
%%% @doc      This code updates older database tables.
%%%           to disk (and erlang shell).
%%%
%%% @since    25 Oct 2004 by Håkan Stenholm <hsten@it.su.se>
%%% @end
%%% @private
%%%-------------------------------------------------------------------
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
%% Returns : ok
%%--------------------------------------------------------------------
update() ->
    logger:log(debug, "Checking if any mnesia tables needs updating"),
    phone(),
    regexproute(),
    cpl_script_graph(),
    gruu(),
    ok.

%%--------------------------------------------------------------------
%% @spec    () -> void()
%%
%% @doc     Phone record got two new fields, add dummy fields for old
%%          existing database entries. Change dated ~2004-12.
%% @end
%%--------------------------------------------------------------------
phone() ->
    Table = phone,
    F = fun({phone, Number, Flags, Class, Expire, Address, ReqUriStr}) ->
		%% check for old record lacking callid and cseq field
		put({Table, update}, true),
		#phone{
		     user = Number,
		     flags = Flags,
		     class = Class,
		     expire = Expire,
		     address = Address,
		     requristr = ReqUriStr,
		     callid = "",
		     cseq = 0,
		     instance = ""
		    };
	   ({phone, User, Flags, Class, Expire, Address, ReqUriStr, CallId, CSeq}) ->
		%% Add instance field with Instance ID, previously stored in Flags.
		%% We don't care to remove it from Flags - no real need to.
		Instance =
		    case lists:keysearch(instance_id, 1, Flags) of
			{value, {instance_id, InstanceId}} ->
			    InstanceId;
			false ->
			    []
		    end,

		put({Table, update}, true),
		#phone{
		     user = User,
		     flags = Flags,
		     class = Class,
		     expire = Expire,
		     address = Address,
		     requristr = ReqUriStr,
		     callid = CallId,
		     cseq = CSeq,
		     instance = Instance
		    };
	   (Phone) when is_record(Phone, phone) ->
		%% nothing to update
		Phone
	end,
    do_transform_table(Table, F, record_info(fields, phone)),

    case lists:member(#phone.requristr, mnesia:table_info(phone, index)) of
	true ->
	    ok;
	false ->
	    logger:log(debug, "Startup: Adding 'requristr' index to location database table 'phone'"),
	    {atomic, ok} = mnesia:add_table_index(phone, #phone.requristr)
    end,

    case lists:member(#phone.instance, mnesia:table_info(phone, index)) of
	true ->
	    ok;
	false ->
	    logger:log(debug, "Startup: Adding 'instance' index to location database table 'phone'"),
	    {atomic, ok} = mnesia:add_table_index(phone, #phone.instance)
    end,

    ok.


%%--------------------------------------------------------------------
%% @spec    () -> void()
%%
%% @doc     Update the sipurl record() in the regexproute, and store
%%          it as a string instead of as a record. Change dated
%%          2005-02.
%% @end
%%--------------------------------------------------------------------
regexproute() ->
    Table = regexproute,
    F = fun({regexproute, Regexp, Flags,  Class,  Expire, {sipurl, Proto, User, Pass, Host, Port, Param}}) ->
		%% old sipurl's lacking url_param field
		put({Table, update}, true),
		%% fixes so that url_param record is used
		URL = sipurl:new([{proto, Proto}, {user, User}, {pass, Pass},
				  {host, Host}, {port, Port}, {param, Param}]),
		%% store as string instead of record, so that we don't have to do
		%% conversions like this in the future when we modify records
		URLstr = sipurl:print(URL),
		#regexproute{
				       regexp = Regexp,
				       flags = Flags,
				       class = Class,
				       expire = Expire,
				       address = URLstr
				      };
	   ({regexproute, Regexp, Flags,  Class,  Expire, {sipurl, Proto, User, Pass, Host, Port, _Param, UrlParam}}) ->
		%% sipurl's with both param and url_param field
		put({Table, update}, true),
		URL = sipurl:new([{proto, Proto}, {user, User}, {pass, Pass},
					{host, Host}, {port, Port}, {param, UrlParam}]),
		%% store as string instead of record, so that we don't have to do
		%% conversions like this in the future when we modify records
		URLstr = sipurl:print(URL),
		#regexproute{
				       regexp = Regexp,
				       flags = Flags,
				       class = Class,
				       expire = Expire,
				       address = URLstr
				      };
	   (RegExpRoute) when is_record(RegExpRoute, regexproute) ->
		%% nothing to update
		RegExpRoute
	end,
    do_transform_table(Table, F, record_info(fields, regexproute)).


%%--------------------------------------------------------------------
%% @spec    () -> void()
%%
%% @doc     Update the cpl_script_graph record() in cpl_db to also
%%          store CPL script as plain text. Change dated 2005-10.
%% @end
%%--------------------------------------------------------------------
cpl_script_graph() ->
    Table = cpl_script_graph,
    {ok, Attrs, Fun} = cpl_db:get_transform_fun(),
    do_transform_table(Table, Fun, Attrs).

%%--------------------------------------------------------------------
%% @spec    () -> void()
%%
%% @doc     Update the gruu record().
%% @end
%%--------------------------------------------------------------------
gruu() ->
    Table = gruu,
    {ok, Attrs, Fun} = database_gruu:get_transform_fun(),
    do_transform_table(Table, Fun, Attrs).

%%====================================================================
%% Internal functions
%%====================================================================

%% Returns: ok
do_transform_table(Table, Fun, Fields) when is_atom(Table), is_function(Fun, 1), is_list(Fields) ->
    put({Table, update}, false),

    case mnesia:transform_table(Table, Fun, Fields) of
	{atomic, ok} ->
	    ok;
	{aborted, {not_active, Reason, Table, NodeList}} ->
	    %% All disc_copies nodes must be online for table transforming, but we can't require
	    %% all those nodes to be alive in order to start the YXA servers.
	    logger:log(normal, "Warning: Failed to update Mnesia table '~p' : ~s~n(node list : ~p)",
		       [Table, Reason, NodeList]);
	{aborted, {"Bad transform function", Table, _BadFun, OtherNode, {{badfun, _BadFun}, _ST}}} ->
	    logger:log(error, "Error: Failed to update Mnesia table '~p' because the local transformation "
		       "function is not the same as the one on node ~p", [Table, OtherNode]),
	    erlang:error('Version inconsistency with other disc_copies nodes - table transform impossible')
    end,

    case erase({Table, update}) of
	true ->
	    logger:log(debug, "~p: updated", [Table]);
	false ->
	    true
    end,

    ok.
