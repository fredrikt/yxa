%%%-------------------------------------------------------------------
%%% File    : notifylist.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      ETS table based list of running processes that want to
%%%           be notified when a presentitys state _might_ have
%%%           changed.
%%%
%%% @since     8 May 2006 by  <ft@nbar.it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(notifylist).

-export([init/0,
	 add/3,
	 delete/3,
	 lookup/2,
	 get_all_pids/0
	]).


%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(ETS_SUBSCRIPTIONS_TABLE, yxa_event_package_subscriptions).


%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Initializes the notifylist. Creates an ETS table.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(?ETS_SUBSCRIPTIONS_TABLE, [public, bag, named_table]),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Presentity, PackageS, Pid) -> ok
%%
%%            Presentity = tuple() "should be {user, User} or {address, Address}"
%%            PackageS   = string()
%%            Pid        = pid()
%%
%% @doc     Adds an entry to the notification list. Add Pid as a
%%          watcher to changes for Presentity in the context of
%%          PackageS.
%% @end
%%--------------------------------------------------------------------
add(Presentity, PackageS, Pid) when is_tuple(Presentity), is_list(PackageS), is_pid(Pid) ->
    Entry = {Presentity, {PackageS, Pid}},
    true = ets:insert(?ETS_SUBSCRIPTIONS_TABLE, Entry),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Presentity, PackageS, Pid) -> ok
%%
%%            Presentity = tuple() "should be {user, User} or {address, Address}"
%%            PackageS   = string()
%%            Pid        = pid()
%%
%% @doc     Deletes an entry from the notification list. Declare Pid
%%          not any longer interested in changes to Presentity in the
%%          context of PackageS.
%% @end
%%--------------------------------------------------------------------
delete(Presentity, PackageS, Pid) when is_tuple(Presentity), is_list(PackageS), is_pid(Pid) ->
    Entry = {Presentity, {PackageS, Pid}},
    true = ets:delete_object(?ETS_SUBSCRIPTIONS_TABLE, Entry),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Presentity, PackageFilter) -> [pid()]
%%
%%            Presentity    = tuple() "should be {user, User} or {address, Address}"
%%            PackageFilter = string()
%%
%% @doc     Get a list of everyone interested in changes to Presentity
%%          matching PackageFilter.
%% @end
%%--------------------------------------------------------------------
lookup(Presentity, PackageFilter) when is_tuple(Presentity), is_list(PackageFilter) ->
    L = ets:lookup(?ETS_SUBSCRIPTIONS_TABLE, Presentity),
    [Pid || {_Id, {PackageS, Pid}} <- L, PackageS == PackageFilter].

%%--------------------------------------------------------------------
%% @spec    () -> [pid()]
%%
%% @doc     Get a list of every pid subscribed to any resource. The
%%          list MAY contain duplicates.
%% @end
%%--------------------------------------------------------------------
get_all_pids() ->
    L = ets:tab2list(?ETS_SUBSCRIPTIONS_TABLE),
    [Pid || {_Id, {_PackageS, Pid}} <- L].
