%%%-------------------------------------------------------------------
%%% File    : yxa_test_config.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Module to init/modify a per-process configuration, for
%%%           use in unit tests.
%%%
%%% @since    28 Nov 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
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
%% @spec    (L) ->
%%            ok | {error, Msg}
%%
%%            L     = [{Key, Value}]
%%            Key   = atom()
%%            Value = term()
%%
%%            Msg = string() | atom()
%%
%% @doc     Initiates a per-process configuration with the defaults
%%          for App. Fetches the App name from the configuration.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(L) when is_list(L) ->
    {ok, App} = yxa_config:get_env(yxa_appmodule),
    init(App, L).

%%--------------------------------------------------------------------
%% @spec    (App, L) ->
%%            ok | {error, Msg}
%%
%%            L     = [{Key, Value}]
%%            Key   = atom()
%%            Value = term()
%%            App   = atom() "YXA application module"
%%
%%            Msg = string() | atom()
%%
%% @doc     Initiates a per-process configuration with the defaults
%%          for App
%% @hidden
%% @end
%%--------------------------------------------------------------------
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
%% @spec    (L) ->
%%            ok | {error, Msg}
%%
%%            L     = [{Key, Value}]
%%            Key   = atom()
%%            Value = term()
%%
%%            Msg = atom()
%%
%% @doc     Update a per-process configuration. Returns an error-
%%          tuple if no per-process config is in use.
%% @end
%%--------------------------------------------------------------------
set([{Key, Value} | T]) ->
    case set(Key, Value) of
	ok -> set(T);
	E ->  E
    end;
set([]) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (Key, Value) ->
%%            ok | {error, Msg}
%%
%%            Key   = atom()
%%            Value = term()
%%
%%            Msg = atom()
%%
%% @doc     Update a per-process configuration. Returns an error-
%%          tuple if no per-process config is in use.
%% @end
%%--------------------------------------------------------------------
set(Key, Value) when is_atom(Key) ->
    case get(?YXA_CONFIG_SOURCE_PTR) of
	undefined ->
	    {error, 'No per-process configuration active'};
	[EtsRef | _Rest] ->
	    true = ets:insert(EtsRef, {Key, Value, ?MODULE}),
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Stop using a per-process configuration.
%% @end
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
