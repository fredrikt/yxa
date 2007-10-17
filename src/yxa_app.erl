%%%-------------------------------------------------------------------
%%% File    : yxa_app.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Behaviour definition for YXA applications.
%%%
%%% @since    01 Sep 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @hidden
%%%-------------------------------------------------------------------
-module(yxa_app).

-export([behaviour_info/1
	]).


%%--------------------------------------------------------------------
%% @spec    (callbacks) -> [tuple()]
%%
%% @doc     Describe all the API functions a module indicating it is
%%          an YXA application must export. List of tuples of the
%%          function names and their arity.
%% @hidden
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{init, 0},
     {request, 2},
     {response, 2},
     {terminate, 1}
    ];
behaviour_info(_Other) ->
    undefined.
