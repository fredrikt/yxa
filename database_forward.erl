%%
%%--------------------------------------------------------------------

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
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("database_forward.hrl").
-include("siprecords.hrl").
-include("sipproxy.hrl").

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
    create([node()]).

create(Servers) ->
    mnesia:create_table(forward, [{attributes, record_info(fields, forward)},
				  {disc_copies, Servers}
				  %% key = number
				  %% type = set, default for table
				 ]).

%%--------------------------------------------------------------------
%% Function: insert(Number, Forwards, Timeout, Localring)
%%           Number    = string(), arbitrary number or SIP username
%%           Forwards  = list() of sipurl record()
%%           Timeout   = integer(), wait timeout - timeout for wait
%%                       sipproxy_action placed after the call
%%                       sipproxy_actions generated for Forwards
%%           LocalRing = whether to ring on location database entrys
%%                       at the same time as the forward destinations
%%                       or not
%% Descrip.: Store Forwards for Number.
%% Returns : mnesia:transactions()
%%--------------------------------------------------------------------
insert(Number, Forwards, Timeout, Localring) when is_list(Number), is_list(Forwards), is_integer(Timeout),
						  Localring == true; Localring == false ->
    ForwardStrings = sipurls_to_strings(Forwards),
    db_util:insert_record(#forward{number = Number,
				   forwards = ForwardStrings,
				   timeout = Timeout,
				   localring = Localring}).

%%--------------------------------------------------------------------
%% Function: list()
%% Descrip.: List all forwards in the database.
%% Returns : list() of forward record()
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(forward).

%%--------------------------------------------------------------------
%% Function: fetch(Number)
%%           Number = string(), arbitrary number or SIP username
%% Descrip.: Fetch all forwards for Number.
%% Returns : {ok, Forwards}
%%           Forwards = list() of sipproxy_forward record()
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
%% Function: delete(Number)
%%           Number = string(), arbitrary number or SIP username
%% Descrip.: Delete all forwards for Number.
%% Returns : mnesia:transaction()
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
