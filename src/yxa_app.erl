%%%-------------------------------------------------------------------
%%% File    : yxa_app.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Behaviour definition for YXA applications.
%%%
%%% Created : 01 Sep 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(yxa_app).

-export([behaviour_info/1
	]).


%%--------------------------------------------------------------------
%% Function: behaviour_info(callbacks)
%% Descrip.: Describe all the API functions a module indicating it is
%%           an YXA application must export. List of tuples of the
%%           function names and their arity.
%% Returns : list() of tuple()
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{init, 0},
     {request, 3},
     {response, 3},
     {terminate, 1}
    ];
behaviour_info(_Other) ->
    undefined.
