%%
%%--------------------------------------------------------------------

-module(database_regexproute).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create/0,
	 create/1,
	 insert/5,
	 list/0,
	 purge_class/2,
	 convert_urls/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("database_regexproute.hrl").
-include("siprecords.hrl").

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
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
create() ->
    create(servers()).

create(Servers) ->
    mnesia:create_table(regexproute, [{attributes, record_info(fields, regexproute)},
				      {disc_copies, Servers},
				      {type, bag}]).

servers() ->
    sipserver:get_env(databaseservers).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
insert(Regexp, Flags, Class, Expire, Address) ->
    db_util:insert_record(#regexproute{regexp = Regexp, flags = Flags, class = Class,
			       expire = Expire, address = Address}).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(regexproute).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
convert_urls() ->
    F = fun() ->
		%% !!! this may be costly if the table is large
		A = db_util:tab_to_list(regexproute),
		Update = fun(O) ->
				 mnesia:delete_object(O),
				 mnesia:write(rewrite_url(O))
			 end,
		lists:foreach(Update, A)
	end,
    mnesia:transaction(F).

rewrite_url(R) when record(R, regexproute) ->
    case R#regexproute.address of
	URL when record(URL, sipurl) ->
	    %% all set
	    R;
	{User, Pass, Host, Port, Parameters} ->
	    %% old format
	    U = #sipurl{proto="sip", user=User, pass=Pass, host=Host, port=Port, param=Parameters},
	    io:format("database_regexproute: Rewrote regexp ~p URL ~p~n", [R#regexproute.regexp, sipurl:print(U)]),
	    R#regexproute{address=U};
	Unknown ->
	    io:format("database_regexproute: Unknown URL format ~p in entry :~n~p~n",
		      [Unknown, R])
    end.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
purge_class(Regexp, Class) ->
    Fun = fun() ->
		  A = mnesia:match_object(#regexproute{regexp = Regexp,
						       class = Class,
						       _ = '_'}),
		  Delete = fun(O) ->
				   mnesia:delete_object(O)
			   end,
		  lists:foreach(Delete, A)
	  end,
    mnesia:transaction(Fun).

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
