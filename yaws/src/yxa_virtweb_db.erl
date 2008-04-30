%%%-------------------------------------------------------------------
%%% File    : yxa_virtweb_db.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Virtweb database code.
%%%
%%% @since    18 Oct 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(yxa_virtweb_db).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create/0,
	 create/1,
	 insert_authuser/4,
	 get_authuser/1,
	 delete_authuser/1,
	 list/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("yxa_virtweb.hrl").


%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     Invike create/1 to create Mnesia table yxa_virtweb_db on
%%          the currently running erlang node.
%% @private
%% @end
%%--------------------------------------------------------------------
create() ->
    create([node()]).

%%--------------------------------------------------------------------
%% @spec    (Servers) -> term()
%%
%%            Servers = [atom()]
%%
%% @doc     Create Mnesia table yxa_virtweb_db on Servers.
%% @private
%% @end
%%--------------------------------------------------------------------
create(Servers) when is_list(Servers) ->
    mnesia:create_table(yxa_virtweb_db, [{attributes, record_info(fields, yxa_virtweb_db)},
					 {disc_copies, Servers}
					]).

%%--------------------------------------------------------------------
%% @spec    (User, Password, Domains, Flags) ->
%%            Res
%%
%%            User     = string()
%%            Password = string()
%%            Domains  = [string()]
%%            Flags    = [{Key, Value}] "for future use"
%%
%%            Res = term() "result of mnesia:transaction()"
%%
%% @doc     Insert a new user into the database.
%% @end
%%--------------------------------------------------------------------
insert_authuser(User, Password, DomainsIn, Flags) when is_list(User), is_list(Password), is_list(DomainsIn),
						       is_list(Flags) ->
    Domains = lists:map(fun(D) when is_list(D) ->
				string:to_lower(D)
			end, DomainsIn),
    db_util:insert_record(#yxa_virtweb_db{user     = User,
					  password = Password,
					  domains  = Domains,
					  flags    = Flags
					 }).

%%--------------------------------------------------------------------
%% @spec    () -> [#forward{}]
%%
%% @doc     List all entrys in the database.
%% @end
%%--------------------------------------------------------------------
list() ->
    db_util:tab_to_list(yxa_virtweb_db).

%%--------------------------------------------------------------------
%% @spec    (User) ->
%%            {ok, User}     |
%%            {error, Error} |
%%            nomatch
%%
%%            User = string() "username"
%%
%%            Forwards = [#sipproxy_forward{}]
%%            Error    = term()
%%
%% @doc     Fetch the authuser entry for User.
%% @end
%%--------------------------------------------------------------------
get_authuser(User) when is_list(User) ->
    F = fun() ->
		MatchSpec = [{#yxa_virtweb_db{user = User, _ = '_'},
			      [],
			      ['$_']
			     }],
		mnesia:select(yxa_virtweb_db, MatchSpec)
	end,
    case mnesia:transaction(F) of
	{atomic, [E]} when is_record(E, yxa_virtweb_db) ->
	    {ok, E};
	{atomic, []} ->
	    nomatch;
        Error ->
	    {error, Error}
    end.

%%--------------------------------------------------------------------
%% @spec    (User) ->
%%            Res
%%
%%            User = string() "arbitrary number or SIP username"
%%
%%            Res = term() "result of mnesia:transaction()"
%%
%% @doc     Delete authuser User.
%% @end
%%-------------------------------------------a-------------------------
delete_authuser(User) when is_list(User) ->
    db_util:delete_with_key(yxa_virtweb_db, User).


