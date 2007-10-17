%%%--------------------------------------------------------------------
%%% File    : database_forward.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Access routines for a Mnesia table holding forwarding
%%%           information for users. Only used at KTH, in
%%%           'appserver' and maybe 'incomingproxy'.
%%%
%%% @since    11 Sep 2003 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%--------------------------------------------------------------------
-module(database_forward).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create/0,
	 create/1,
	 insert/4,
	 fetch/1,
	 list/0,
	 delete/1
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("database_forward.hrl").
-include("siprecords.hrl").
-include("sipproxy.hrl").


%%====================================================================
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

%%--------------------------------------------------------------------
%% @spec    (Servers) -> term() "result of mnesia:create_table/2."
%%
%%            Servers = [atom()] "list of nodes"
%%
%% @doc     Create the table 'forward' on Servers.
%% @private
%% @end
%%--------------------------------------------------------------------
create(Servers) when is_list(Servers) ->
    mnesia:create_table(forward, [{attributes, record_info(fields, forward)},
				  {disc_copies, Servers}
				  %% key = number
				  %% type = set, default for table
				 ]).

%%--------------------------------------------------------------------
%% @spec    (Number, Forwards, Timeout, Localring) -> term()
%%
%%            Number    = string() "arbitrary number or SIP username"
%%            Forwards  = [#sipurl{}]
%%            Timeout   = integer() "wait timeout - timeout for wait sipproxy_action placed after the call sipproxy_actions generated for Forwards"
%%            LocalRing = bool() "whether to ring on location database entry at the same time as the forward destinations or not"
%%
%% @doc     Store Forwards for Number.
%% @end
%%--------------------------------------------------------------------
insert(Number, Forwards, Timeout, Localring) when is_list(Number), is_list(Forwards), is_integer(Timeout),
						  Localring == true; Localring == false ->
    ForwardStrings = sipurls_to_strings(Forwards),
    db_util:insert_record(#forward{number = Number,
				   forwards = ForwardStrings,
				   timeout = Timeout,
				   localring = Localring}).

%%--------------------------------------------------------------------
%% @spec    () -> [#forward{}]
%%
%% @doc     List all forwards in the database.
%% @end
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(forward).

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            {ok, Forwards}
%%
%%            Number = string() "arbitrary number or SIP username"
%%
%%            Forwards = [#sipproxy_forward{}]
%%
%% @doc     Fetch all forwards for Number.
%% @end
%%--------------------------------------------------------------------
fetch(Number) ->
    F = fun() ->
		MatchSpec = [{#forward{number = Number, _ = '_'},
			      [],
			      [{{{element, #forward.forwards, '$_'},
				 {element, #forward.timeout, '$_'},
				 {element, #forward.localring, '$_'}
				}}]
			     }],
		mnesia:select(forward, MatchSpec)
	end,
    {atomic, L} = mnesia:transaction(F),
    Rewrite = fun(Entry) ->
		      {Fwds, Timeout, LRing} = Entry,
		      FwdURIs = strings_to_sipurls(Fwds),
		      #sipproxy_forward{user      = Number,
					forwards  = FwdURIs,
					timeout   = Timeout,
					localring = LRing
				       }
	      end,
    {ok, lists:map(Rewrite, L)}.

%%--------------------------------------------------------------------
%% @spec    (Number) -> term()
%%
%%            Number = string() "arbitrary number or SIP username"
%%
%% @doc     Delete all forwards for Number.
%% @end
%%--------------------------------------------------------------------
delete(Number) when is_list(Number) ->
    db_util:delete_with_key(forward, Number).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================


%% Make strings out of URIs before storing them in the database
sipurls_to_strings(In) ->
    sipurls_to_strings2(In, []).

sipurls_to_strings2([H | T], Res) when is_record(H, sipurl) ->
    sipurls_to_strings2(T, [sipurl:print(H) | Res]);
sipurls_to_strings2([], Res) ->
    lists:reverse(Res).


%% Make URIs out of the forwards we fetch from the database
strings_to_sipurls(In) ->
    strings_to_sipurls2(In, []).

strings_to_sipurls2([H | T], Res) ->
    strings_to_sipurls2(T, [sipurl:parse(H) | Res]);
strings_to_sipurls2([], Res) ->
    lists:reverse(Res).
