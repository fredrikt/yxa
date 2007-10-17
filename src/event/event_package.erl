%%%-------------------------------------------------------------------
%%% File    : event_package.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Behaviour definition for YXA event package modules.
%%%
%%% @since    04 May 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @hidden
%%%-------------------------------------------------------------------
-module(event_package).

-export([behaviour_info/1
	]).


%%--------------------------------------------------------------------
%% @spec    (callbacks) -> [tuple()]
%%
%% @doc     Describe all the API functions a module indicating it is
%%          an YXA event package module must export. List of tuples
%%          of the function names and their arity.
%% @hidden
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{init, 0},
     {request, 4},
     {is_allowed_subscribe, 7},
     {notify_content, 4},
     {package_parameters, 2},
     {subscription_behaviour, 3}
    ];
behaviour_info(_Other) ->
    undefined.
