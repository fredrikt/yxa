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



%% Returs : ok
init() ->
    ets:new(?ETS_SUBSCRIPTIONS_TABLE, [public, bag, named_table]),
    ok.

%% Returs : ok
add(Presentity, PackageS, Pid) when is_tuple(Presentity), is_list(PackageS), is_pid(Pid) ->
    Entry = {Presentity, {PackageS, Pid}},
    true = ets:insert(?ETS_SUBSCRIPTIONS_TABLE, Entry),
    ok.

%% Returs : ok
delete(Presentity, PackageS, Pid) when is_tuple(Presentity), is_list(PackageS), is_pid(Pid) ->
    Entry = {Presentity, {PackageS, Pid}},
    true = ets:delete_object(?ETS_SUBSCRIPTIONS_TABLE, Entry),
    ok.

%% Returns : list() of pid()
lookup(Presentity, PackageFilter) when is_tuple(Presentity), is_list(PackageFilter) ->
    L = ets:lookup(?ETS_SUBSCRIPTIONS_TABLE, Presentity),
    [Pid || {_Id, {PackageS, Pid}} <- L, PackageS == PackageFilter].

%% Returns : list() of pid()
get_all_pids() ->
    L = ets:tab2list(?ETS_SUBSCRIPTIONS_TABLE),
    [Pid || {_Id, {PackageS, Pid}} <- L].
