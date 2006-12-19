%%%-------------------------------------------------------------------
%%% File    : notifylist.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: ETS table based list of running processes that want to
%%%           be notified when a presentitys state _might_ have
%%%           changed.
%%%
%%% Created :  8 May 2006 by  <ft@nbar.it.su.se>
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
%% Function: init()
%% Descrip.: Initializes the notifylist. Creates an ETS table.
%% Returns : ok
%%--------------------------------------------------------------------
init() ->
    ets:new(?ETS_SUBSCRIPTIONS_TABLE, [public, bag, named_table]),
    ok.

%%--------------------------------------------------------------------
%% Function: add(Presentity, PackageS, Pid)
%%           Presentity = tuple(), should be {user, User} or
%%                                           {address, Address}
%%           PackageS   = string()
%%           Pid        = pid()
%% Descrip.: Adds an entry to the notification list. Add Pid as a
%%           watcher to changes for Presentity in the context of
%%           PackageS.
%% Returns : ok
%%--------------------------------------------------------------------
add(Presentity, PackageS, Pid) when is_tuple(Presentity), is_list(PackageS), is_pid(Pid) ->
    Entry = {Presentity, {PackageS, Pid}},
    true = ets:insert(?ETS_SUBSCRIPTIONS_TABLE, Entry),
    ok.

%%--------------------------------------------------------------------
%% Function: delete(Presentity, PackageS, Pid)
%%           Presentity = tuple(), should be {user, User} or
%%                                           {address, Address}
%%           PackageS   = string()
%%           Pid        = pid()
%% Descrip.: Deletes an entry from the notification list. Declare Pid
%%           not any longer interested in changes to Presentity in the
%%           context of PackageS.
%% Returns : ok
%%--------------------------------------------------------------------
delete(Presentity, PackageS, Pid) when is_tuple(Presentity), is_list(PackageS), is_pid(Pid) ->
    Entry = {Presentity, {PackageS, Pid}},
    true = ets:delete_object(?ETS_SUBSCRIPTIONS_TABLE, Entry),
    ok.

%%--------------------------------------------------------------------
%% Function: lookup(Presentity, PackageFilter)
%%           Presentity    = tuple(), should be {user, User} or
%%                                              {address, Address}
%%           PackageFilter = string()
%% Descrip.: Get a list of everyone interested in changes to
%%           Presentity matching PackageFilter.
%% Returns : list() of pid()
%%--------------------------------------------------------------------
lookup(Presentity, PackageFilter) when is_tuple(Presentity), is_list(PackageFilter) ->
    L = ets:lookup(?ETS_SUBSCRIPTIONS_TABLE, Presentity),
    [Pid || {_Id, {PackageS, Pid}} <- L, PackageS == PackageFilter].

%%--------------------------------------------------------------------
%% Function: get_all_pids()
%% Descrip.: Get a list of every pid subscribed to any resource. The
%%           list MAY contain duplicates.
%% Returns : list() of pid()
%%--------------------------------------------------------------------
get_all_pids() ->
    L = ets:tab2list(?ETS_SUBSCRIPTIONS_TABLE),
    [Pid || {_Id, {_PackageS, Pid}} <- L].
