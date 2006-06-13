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
	 lookup/2
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
    true = ets:insert(?ETS_SUBSCRIPTIONS_TABLE, {Presentity, {PackageS, Pid}}),
    ok.

%% Returs : ok
delete(Presentity, PackageS, Pid) when is_tuple(Presentity), is_list(PackageS), is_pid(Pid) ->
    true = ets:delete(?ETS_SUBSCRIPTIONS_TABLE, {Presentity, {PackageS, Pid}}),
    ok.

%% Returns : list() of pid()
lookup(Presentity, PackageFilter) when is_tuple(Presentity), is_list(PackageFilter) ->
    L = ets:lookup(?ETS_SUBSCRIPTIONS_TABLE, Presentity),
    [Pid || {_Id, {PackageS, Pid}} <- L, PackageS == PackageFilter].
