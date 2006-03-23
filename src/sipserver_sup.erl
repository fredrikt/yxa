%%%-------------------------------------------------------------------
%%% File    : sipserver_sup.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: YXA application main supervisor.
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
	 start_link/2,
	 start_extras/3,
	 start_transportlayer/1,
	 get_pids/0
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
start_link(AppModule, MnesiaTables) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [AppModule, MnesiaTables]).

%%--------------------------------------------------------------------
%% Function: get_pids()
%% Descrip.: Try to make a complete list of all pids currently
%%           involved in the running system (excluding ongoing worker
%%           pids like client/server transcations and request
%%           handlers) - for manual use. Intended for getting a list
%%           of pids to tell eprof to profile.
%% Returns : list() of pid()
%%--------------------------------------------------------------------
get_pids() ->
    sup_get_pids(?MODULE).

%% part of get_pids()
sup_get_pids(M) when is_pid(M); is_atom(M) ->
    List = supervisor:which_children(M),
    extract_pids(List, []).

%% part of sup_get_pids()
extract_pids([{_Id, Child, supervisor, _Modules} | T], Res) ->
    %% is a supervisor, ask for it's childrens
    Childs = sup_get_pids(Child),
    extract_pids(T, [[Child | Childs] | Res]);
extract_pids([{_Id, Child, worker, _Modules} | T], Res) ->
    %% is worker
    extract_pids(T, [Child | Res]);
extract_pids([], Res) ->
    %% no more input
    lists:flatten(Res).


%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([AppModule, MnesiaTables])
%%           AppModule = atom(), name of Yxa application module
%% Returns : {ok,  {SupFlags,  [ChildSpec]}} |
%%           ignore                          |
%%           {error, Reason}   
%%--------------------------------------------------------------------
init([AppModule, MnesiaTables]) ->
    CfgServer = {yxa_config, {yxa_config, start_link, [AppModule]},
		 permanent, 2000, worker, [yxa_config]},
    Logger = {logger, {logger, start_link, []},
                 permanent, 2000, worker, [logger]},
    Directory = {directory, {directory, start_link, []},
                 permanent, 2000, worker, [directory]},
    DialogServer = {dialog_server, {dialog_server, start_link, []},
		    permanent, 2000, worker, [dialog_server]},
    TransactionLayer = {transactionlayer,
			{transactionlayer, start_link, [AppModule]},
			permanent, 2000, worker, [transactionlayer]},
    Monitor = {yxa_monitor, {yxa_monitor, start_link, [AppModule, MnesiaTables]},
	       permanent, 2000, worker, [yxa_monitor]},
    MyList = [CfgServer, Logger, Monitor, Directory, DialogServer, TransactionLayer],
    {ok, {{one_for_one, 20, 60}, MyList}}.

start_extras(Supervisor, AppModule, AppSupdata) ->
    UserDb = sipuserdb:yxa_init(),
    EventSup = {event_handler, {event_handler, start_link, [AppModule]},
		permanent, 2000, worker, [event_handler]},
    MyList = lists:append([EventSup], UserDb),
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
		      permanent, infinity, supervisor, [transportlayer]},
    my_start_children(Supervisor, [TransportLayer]).


%%====================================================================
%% Internal functions
%%====================================================================

%% tell Supervisor to start a list of children, one by one.
my_start_children(Supervisor, []) ->
    {ok, Supervisor};
my_start_children(Supervisor, [H|T]) ->
    case supervisor:start_child(Supervisor, H) of
	{error, E} ->
	    logger:log(error, "Sipserver supervisor: Failed starting child '~p': ~p",
		       [element(1, H), E]),
    	    throw('Failed starting subsystem, see logfiles for more details');
	{ok, _Child} ->
	    my_start_children(Supervisor, T);
	{ok, _Child, _Info} ->
	    my_start_children(Supervisor, T)
    end.


