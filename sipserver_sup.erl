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
-export([
	 start_link/1,
	 start_extras/2,
	 start_transportlayer/1
	]).

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
start_link(AppCallbacks) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [AppCallbacks]).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init([AppModule])
%%           AppModule = atom(), name of Yxa application module
%% Returns : {ok,  {SupFlags,  [ChildSpec]}} |
%%           ignore                          |
%%           {error, Reason}   
%%--------------------------------------------------------------------
init([AppModule]) ->
    Logger = {logger, {logger, start_link, []},
                 permanent, 2000, worker, [logger]},
    Directory = {directory, {directory, start_link, []},
                 permanent, 2000, worker, [directory]},
    TransactionLayer = {transactionlayer,
			{transactionlayer, start_link, [AppModule]},
			permanent, 2000, worker, [transactionlayer]},
    MyList = [Logger, Directory, TransactionLayer],
    {ok, {{one_for_one, 20, 60}, MyList}}.

start_extras(Supervisor, AppSupdata) ->
    UserDb = sipuserdb:yxa_init(),
    MyList = UserDb,
    SupList = case AppSupdata of
		  {append, AppList} when is_list(AppList) ->
		      lists:append(MyList, AppList);
		  none ->
		      MyList
	      end,
    my_start_children(Supervisor, SupList).

start_transportlayer(Supervisor) ->
    %% Start the transport layer now that we have initialized everything
    TransportLayer = {transportlayer,
		      {transportlayer, start_link, []},
		      permanent, 2000, supervisor, [transportlayer]},
    my_start_children(Supervisor, [TransportLayer]).

%%====================================================================
%% Internal functions
%%====================================================================
my_start_children(Supervisor, []) ->
    {ok, Supervisor};
my_start_children(Supervisor, [H|T]) ->
    case supervisor:start_child(Supervisor, H) of
	{error, E} ->
	    Msg = lists:flatten(
		    io_lib:format("Sipserver supervisor: Failed starting child '~p': ~p",
				  [element(1, H), E])
		    ),
	    logger:log(error, Msg),
	    %% We must sleep a few seconds here, so that the supervisor does not shut down
	    %% the logger process while it is still logging (yes, that was what happened
	    %% with the result being that the logger process crashed and produced a mis-
	    %% guiding error 'ebadf' from io:format()).
	    timer:sleep(3 * 1000),
	    {error, Msg};
	{ok, _} ->
	    my_start_children(Supervisor, T);
	{ok, _, _} ->
	    my_start_children(Supervisor, T)
    end.


