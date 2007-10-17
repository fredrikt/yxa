%%%--------------------------------------------------------------------
%%% File    : database_call.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      RAM based database to hold information about 'a call'.
%%%           This module is currently not used at all.
%%%
%%% @since    09 Dec 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%% @hidden
%%%--------------------------------------------------------------------
-module(database_call).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create/0,
	 create/1,
	 insert_call/4,
	 get_call/1,
	 list_calls/0,
	 insert_call_unique/4,
	 set_data_type/3,
	 delete_call_type/2,
	 get_call_type/2,
	 fetch_call/2,
	 fetch_dialogue/2,
	 delete_all_calls/0
	]).

%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("database_call.hrl").


%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term() "result of mnesia:create_table/2."
%%
%% @doc     Invoke create/1 with the list of servers indicated by the
%%          configuration parameter 'databaseservers'.
%% @private
%% @end
%%--------------------------------------------------------------------
create() ->
    {ok, S} = yxa_config:get_env(databaseservers),
    create(S).

create(Servers) ->
    mnesia:create_table(call, [{attributes, record_info(fields, call)},
    			       {ram_copies, Servers}
			       %% type = set, mnesia default
    			      ]).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
insert_call(Callid, Type, Headers, Data) ->
    db_util:insert_record(#call{callid = Callid,
				type = Type,
				headers = Headers,
				data = Data}).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
insert_call_unique(Callid, Type, Headers, Data) ->
    Fun = fun() ->
		  L = mnesia:read({call,Callid}),
		  case L of
		      [] ->
			  mnesia:write(#call{callid = Callid, type = Type,
					     headers = Headers, data = Data});
		      _ ->
			  mnesia:abort(key_exists)
		  end
	  end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
delete_call_type(Callid, Type) ->
	F = fun() ->
		    A = mnesia:match_object(#call{callid = Callid, type = Type, _ = '_'}),
		    Delete = fun(O) ->
				     mnesia:delete_object(O)
			     end,
		    %% XXX list ops are unnecessary, as call is a set table (each key is unique)
		    %% and callid is the key - so there can only be one match
		    lists:foreach(Delete, A)
	    end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
delete_all_calls() ->
    db_util:delete_all_entries(call).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns : list() of call record()
%%--------------------------------------------------------------------
list_calls() ->
    db_util:tab_to_list(call).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
get_call(Call) ->
	F = fun() ->
		    MatchSpec = [{#call{callid = Call, _ = '_'},
				  [],
				  [{{{element, #call.type, '$_'},
				     {element, #call.headers, '$_'},
				     {element, #call.data, '$_'}
				    }}]
				 }],
		    mnesia:select(call, MatchSpec)
	    end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
get_call_type(Call, Type) ->
	F = fun() ->
		    mnesia:match_object(#call{callid = Call,
					      type = Type,
					      _ = '_'})
	    end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
set_data_type(CallID, Data, Type) ->
    F = fun() ->
		A = mnesia:match_object(#call{callid = CallID,
					      type = Type,
					      _ = '_'}),
		Update = fun(O) ->
				 mnesia:write(O#call{data = Data})
			 end,
		%% XXX list ops are unnecessary, as call is a set table (each key is unique)
		%% and callid is the key - so there can only be one match
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
fetch_dialogue({CallID, FromTag, none}, Type) ->
    case get_call_type({CallID, FromTag, none}, Type) of
	{atomic, [Record]} ->
	    {{CallID, FromTag, none}, Record};
	{atomic, []} ->
	     nomatch
    end;
fetch_dialogue(DialogueID, Type) ->
    case get_call_type(DialogueID, Type) of
	{atomic, [Record]} ->
	    {DialogueID, Record};
	{atomic, []} ->
	    {CallID, FromTag, _} = DialogueID,
	    fetch_dialogue({CallID, FromTag, none}, Type)
    end.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
fetch_call(Header, Type) ->
    DialogueID = sipheader:dialogid(Header),
    case fetch_dialogue(DialogueID, Type) of
	{_, Call} ->
	    Call;
	_ ->
	    nomatch
    end.
