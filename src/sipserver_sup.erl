%%%-------------------------------------------------------------------
%%% File    : sipserver_sup.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      YXA application main supervisor.
%%%
%%% @since    21 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
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
%% @spec    (AppModule, MnesiaTables) ->
%%            term() "result of supervisor:start_link/3."
%%
%%            AppModule    = atom() "name of YXA application module"
%%            MnesiaTables = [atom()]
%%
%% @doc     Starts the supervisor of all supervisors.
%% @end
%%--------------------------------------------------------------------
start_link(AppModule, MnesiaTables) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {AppModule, MnesiaTables}).

%%--------------------------------------------------------------------
%% @spec    () -> [pid()]
%%
%% @doc     Try to make a complete list of all pids currently involved
%%          in the running system (excluding ongoing worker pids like
%%          client/server transcations and request handlers) - for
%%          manual use. Intended for getting a list of pids to tell
%%          eprof to profile.
%% @end
%%--------------------------------------------------------------------
get_pids() ->
    sup_get_pids(?MODULE).

%% part of get_pids()
sup_get_pids(M) when is_atom(M) ->
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
%% @spec    ({AppModule, MnesiaTables}) ->
%%            {ok,  {SupFlags, [ChildSpec]}} |
%%            ignore                         |
%%            {error, Reason}
%%
%%            AppModule    = atom() "name of YXA application module"
%%            MnesiaTables = [atom()]
%%
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({AppModule, MnesiaTables}) ->
    CfgServer = {yxa_config, {yxa_config, start_link, [AppModule]},
		 permanent, 2000, worker, [yxa_config]},
    Logger = {logger, {logger, start_link, []},
                 permanent, 2000, worker, [logger]},
    Directory = {directory, {directory, start_link, []},
                 permanent, 2000, worker, [directory]},
    DialogServer = {dialog_server, {dialog_server, start_link, []},
		    permanent, 2000, worker, [dialog_server]},
    TransactionLayer = {transactionlayer,
			{transactionlayer, start_link, []},
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
my_start_children(Supervisor, [H | T]) ->
    Subsystem = element(1, H),
    %%logger:log(debug, "Sipserver supervisor: Starting subsystem ~p", [Subsystem]),
    case supervisor:start_child(Supervisor, H) of
	{error, E} ->
	    logger:log(error, "Sipserver supervisor: Failed starting subsystem '~p': ~p",
		       [Subsystem, E]),
    	    throw('Failed starting subsystem, see logfiles for more details');
	{ok, Child} ->
	    logger:log(debug, "Sipserver supervisor: Started subsystem ~p (~p)", [Subsystem, Child]),
	    my_start_children(Supervisor, T);
	{ok, Child, _Info} ->
	    logger:log(debug, "Sipserver supervisor: Started subsystem ~p (~p)", [Subsystem, Child]),
	    my_start_children(Supervisor, T)
    end.


