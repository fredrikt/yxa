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
-export([start_link/3]).

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
%% Function: start_link/3
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(AppCallbacks, Mode, AppSupdata) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [AppCallbacks, Mode, AppSupdata]).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([AppModule, Mode, AppSupdata]) ->
    Logger = {logger, {logger, start_link, []},
                 permanent, 2000, worker, [logger]},
    Directory = {directory, {directory, start_link, []},
                 permanent, 2000, worker, [directory]},
    TransactionLayer = {transactionlayer,
			{transactionlayer, start_link, [AppModule, Mode]},
			permanent, 2000, worker, [transactionlayer]},
    UserDb = sipuserdb:yxa_init(),
    MyList = lists:append([Logger, Directory, TransactionLayer], UserDb),
    SupList = case AppSupdata of
		  {append, AppList} when list(AppList) ->
		      lists:append(MyList, AppList);
		  none ->
		      MyList
	      end,
    {ok,{{one_for_one,20,60}, SupList}}.

%%====================================================================
%% Internal functions
%%====================================================================
