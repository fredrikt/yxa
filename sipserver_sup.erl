%%%-------------------------------------------------------------------
%%% File    : sipserver_sup.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : Yxa general application supervisor. 
%%%
%%% Created : 21 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipserver_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/4]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([init/1]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/4
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Port, AppCallbacks, Mode, AppSupdata) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, AppCallbacks, Mode, AppSupdata]).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([Port, [RequestFun, ResponseFun], Mode, AppSupdata]) ->
    Logger = {logger, {logger, start_link, []},
                 permanent, 2000, worker, [logger]},
    Directory = {directory, {directory, start_link, []},
                 permanent, 2000, worker, [directory]},
    TransactionLayer = {transactionlayer, 
			{transactionlayer, start_link, [RequestFun, ResponseFun, Mode]},
			permanent, 2000, worker, [transactionlayer]},
    MyList = [Logger, Directory, TransactionLayer],
    SupList = case AppSupdata of
		  {append, AppList} when list(AppList) ->
		      lists:append(MyList, AppList);
		  _ ->
		      MyList
	      end,
    {ok,{{one_for_one,20,60}, SupList}}.

%%====================================================================
%% Internal functions
%%====================================================================
