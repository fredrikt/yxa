%%%-------------------------------------------------------------------
%%% File    : yxa_test_config.erl
%%% Author  : Fredrik <ft@it.su.se>
%%% Descrip.: Module to init/modify a per-process configuration, for
%%%           use in unit tests.
%%%
%%% Created : 28 Nov 2006 by Fredrik <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(yxa_test_config).

-export([init/1,
	 init/2,
	 set/1,
	 set/2,
	 stop/0
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("yxa_config.hrl").


%%--------------------------------------------------------------------
%% Function: init(L)
%%           init(App, L)
%%           L   = list() of {Key, Value}
%%                 Key   = atom()
%%                 Value = term()
%%	     App = atom(), YXA application module
%% Descrip.: Initiates a per-process configuration with the defaults
%%           for App
%% Returns : ok | {error, Msg}
%%           Msg = string() | atom()
%%--------------------------------------------------------------------
init(L) when is_list(L) ->
    {ok, App} = yxa_config:get_env(yxa_appmodule),
    init(App, L).

init(App, L) when is_atom(App), is_list(L) ->
    Ets = ets:new(?MODULE, [protected, set]),

    L2 = [{Key, Value, ?MODULE} || {Key, Value} <- L],

    Backends = [yxa_config_default],
    ExtraCfg = #yxa_cfg{entrys = L2},

    try yxa_config:init_config(Backends, App, ExtraCfg, Ets) of
	{ok, _PrivateState} ->
	    Old =
		case get(?YXA_CONFIG_SOURCE_PTR) of
		    undefined -> [];
		    Old1 -> Old1
		end,
	    put(?YXA_CONFIG_SOURCE_PTR, [Ets | Old]),
	    ok
    catch
	throw: E ->
	    {error, E}
    end.

%%--------------------------------------------------------------------
%% Function: set(L)
%%           set(Key, Value)
%%           L     = list() of {Key, Value}
%%           Key   = atom()
%%           Value = term()
%% Descrip.: Update a per-process configuration. Returns an error-
%%           tuple if no per-process config is in use.
%% Returns : ok | {error, Msg}
%%           Msg = atom()
%%--------------------------------------------------------------------
set([{Key, Value} | T]) ->
    case set(Key, Value) of
	ok -> set(T);
	E ->  E
    end;
set([]) ->
    ok.

set(Key, Value) when is_atom(Key) ->
    case get(?YXA_CONFIG_SOURCE_PTR) of
	undefined ->
	    {error, 'No per-process configuration active'};
	[EtsRef | _Rest] ->
	    true = ets:insert(EtsRef, {Key, Value, ?MODULE}),
	    ok
    end.

%%--------------------------------------------------------------------
%% Function: stop()
%% Descrip.: Stop using a per-process configuration.
%% Returns : ok
%%--------------------------------------------------------------------
stop() ->
    case get(?YXA_CONFIG_SOURCE_PTR) of
	undefined ->
	    ok;
	[EtsRef | T] ->
	    ets:delete(EtsRef),
	    %% pop the first element from the process dictionary, erase if none left
	    case T of
		[] -> erase(?YXA_CONFIG_SOURCE_PTR);
		_  -> put(?YXA_CONFIG_SOURCE_PTR, T)
	    end,
	    ok
    end.
