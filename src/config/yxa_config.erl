%%%-------------------------------------------------------------------
%%% File    : yxa_config.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: YXA configuration subsystem.
%%%
%%% Created : 15 Jun 2005 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(yxa_config).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/1,
	 reload/0,
	 get_env/1,
	 get_env/2,
	 list/0
	]).

-export([behaviour_info/1]).

%% internal export for the configuration subsystem (for yxa_test_config)
-export([init_config/4]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-export([test/0]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("yxa_config.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  backends,	%% list() of {Module, Opaque} where Module is an atom and Opaque is state internal to Module
	  appmodule,	%% atom(), YXA application module
	  etsref	%% term(), ets table reference
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, yxa_config).
-define(YXA_CONFIG, yxa_config_t).
-define(BACKENDS, [
		   yxa_config_default,
		   yxa_config_erlang
		  ]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link(AppModule)
%%           AppModule = atom(), YXA application module
%% Descrip.: start the server.
%% Returns : {ok, Pid}
%%           Pid = pid() of yxa_config persistent gen_server
%%--------------------------------------------------------------------
start_link(AppModule) when is_atom(AppModule) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [AppModule], []).

%%--------------------------------------------------------------------
%% Function: init([AppModule])
%%	     AppModule = atom(), YXA application module
%% Descrip.: Initiates the server
%% Returns : {ok, State}
%%           State = state record()
%%--------------------------------------------------------------------
init([AppModule]) when is_atom(AppModule) ->
    yxa_config_util:startup_log(?MODULE, debug, "Starting configuration subsystem for '~p'", [AppModule]),
    EtsRef = ets:new(?YXA_CONFIG, [protected, set, named_table]),
    ExtraCfg = #yxa_cfg{},
    init_config(?BACKENDS, AppModule, ExtraCfg, EtsRef).

%% part of init/1 and exported for yxa_test_config
init_config(Backends, AppModule, ExtraCfg, EtsRef) when is_list(Backends), is_atom(AppModule),
							is_record(ExtraCfg, yxa_cfg) ->
    %% Figure out which backends to activate
    yxa_config_util:startup_log(?MODULE, debug, "Initializing backends : ~p", [Backends]),
    BackendData = init_backends(Backends, AppModule),

    State = #state{backends	= BackendData,
		   appmodule	= AppModule,
		   etsref	= EtsRef
		  },

    case parse(ExtraCfg, State) of
	{ok, Cfg} when is_record(Cfg, yxa_cfg) ->
	    case validate(Cfg, AppModule, hard) of
		{ok, Normalized} when is_record(Normalized, yxa_cfg) ->
		    ok = load(Normalized, hard, State);
		{error, Msg} when is_list(Msg) ->
		    %% output error message to console first, since logger is probably not running
		    io:format("ERROR: Config validation failed : ~p~n", [Msg]),

		    %% Since this is a failure on init, we fail hard
		    logger:log(error, "Config server: Failed validating configuration : ~p",
			       [Msg]),

		    throw('Config validation failed')
	    end;
	{error, Module, E} when is_list(E) ->
	    %% output error message to console first, since logger is probably not running
	    io:format("ERROR: Config parsing failed (parsing module ~p) : ~p~n", [Module, E]),

	    logger:log(error, "Config server: Failed parsing configuration (parsing module ~p) : ~p",
		       [Module, E]),
	    throw('Configuration parsing error')
    end,

    {ok, State}.

%%--------------------------------------------------------------------
%% Function: init([AppModule])
%%	     AppModule = atom(), YXA application module
%% Descrip.: Calls the init/1 function in each backend, and returns
%%           the backends opaque data structures that will later be
%%           passed to their parse/1 functions.
%% Returns : Res = list() of {Backend, Opaque}
%%           Backend = atom()
%%           Opaque  = term()
%%--------------------------------------------------------------------
init_backends(In, AppModule) when is_list(In), is_atom(AppModule) ->
    F = fun(M, Acc) ->
		case M:init(AppModule) of
		    {ok, Opaque} ->
			[{M, Opaque} | Acc];
		    {error, E} ->
			logger:log(error, "Config server: Failed initializing config backend ~p : ~p",
				   [M, E]),
			throw('Config backend initialization failed');
		    ignore ->
			Acc
		end
	end,

    Res = lists:foldl(F, [], In),
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% Function: reload()
%% Descrip.: Parse, validate and load the configuration (again).
%% Returns : ok | {error, Where, Msg}
%%           Where = atom()
%%           Msg   = string()
%%--------------------------------------------------------------------
reload() ->
    gen_server:call(yxa_config, reload).

%%--------------------------------------------------------------------
%% Function: behaviour_info(callbacks)
%% Descrip.: Describe all the API functions a module indicating it is
%%           an yxa_config behaviour module must export. List of
%%           tuples of the function names and their arity.
%% Returns : list() of tuple()
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{init, 1},
     {parse, 1}
    ];
behaviour_info(_Other) ->
    undefined.

%%--------------------------------------------------------------------
%% Function: get_env(Key)
%%           get_env(Key, Default)
%%           Key     = atom()
%%           Default = term(), returned if no value is present
%% Descrip.: Fetch parameter value.
%% Returns : {ok, Value} |
%%           Value       |
%%           none		parameter known, but no value set
%%--------------------------------------------------------------------
get_env(Key) when is_atom(Key) ->
    %% check if the configuration source for this process is overridden -
    %% for example because we are executing unit tests
    case get(?YXA_CONFIG_SOURCE_PTR) of
	undefined ->
	    get_env2(Key, ?YXA_CONFIG);
	[Tab | _Rest] ->
	    get_env2(Key, Tab)
    end.

get_env(Key, Default) when is_atom(Key) ->
    %% check if the configuration source for this process is overridden -
    %% for example because we are executing unit tests
    case get(?YXA_CONFIG_SOURCE_PTR) of
	undefined ->
	    get_env2(Key, Default, ?YXA_CONFIG);
	[Tab | _Rest] ->
	    get_env2(Key, Default, Tab)
    end.

get_env2(Key, Tab) when is_atom(Key) ->
    try ets:lookup(Tab, Key) of
	[{Key, undefined, yxa_config_default}] ->
	    %% Parameter known, but not set.
	    none;
	[{Key, Value, _Source}] ->
	    {ok, Value};
	_ ->
	    Msg = io_lib:format("Unknown configuration parameter '~p' requested", [Key]),
	    erlang:error({error, lists:flatten(Msg)}, [Key])
    catch
	error: badarg ->
	    %% probably table does not exist
	    erlang:error(config_ets_lookup_failed, [Key])
    end.

get_env2(Key, Default, Tab) when is_atom(Key) ->
    case get_env2(Key, Tab) of
	none -> {ok, Default};
	Res -> Res
    end.

list() ->
    case get(?YXA_CONFIG_SOURCE_PTR) of
	undefined ->
	    list(?YXA_CONFIG);
	[Tab | _Rest] ->
	    list(Tab)
    end.

list(Tab) ->
    lists:keysort(1, ets:tab2list(Tab)).

%%====================================================================
%% Behaviour functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages.
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: handle_call(reload, From, State)
%% Descrip.: Reload configuration.
%% Returns : {reply, Reply, State}
%%           Reply = ok                  |
%%                   {error, Where, Msg}
%%                   Where = parse | validation | load
%%--------------------------------------------------------------------
handle_call(reload, _From, State) ->
    logger:log(normal, "Config server: Reloading configuration"),
    Reply =
	case parse(#yxa_cfg{}, State) of
	    {ok, Cfg} when is_record(Cfg, yxa_cfg) ->
		AppModule = State#state.appmodule,
		case validate(Cfg, AppModule, soft) of
		    {ok, Normalized} when is_record(Normalized, yxa_cfg) ->
			%% load() should never fail and if it does then our ets table is
			%% probably half way updated, so we might as well crash so that
			%% the supervisor (sipserver_sup) restarts us.
			%% XXX what about configuration changes activated by such a restart?
			%% Shold we perhaps restart the whole application if this process dies?
			logger:log(debug, "Config server: Loading checked configuration"),
			ok = load(Normalized, soft, State),
			logger:log(normal, "Config server: Finished reloading configuration"),
			ok;
		    {error, Msg} when is_list(Msg) ->
			logger:log(error, "Config server: Failed validating configuration : ~p",
				   [Msg]),
			{error, validation, Msg}
		end;
	    {error, Backend, E} when is_list(E) ->
		logger:log(error, "Config server: Failed parsing configuration (~p) : ~p", [Backend, E]),
		{error, parse, E}
	end,
    {reply, Reply, State};

handle_call(Unknown, _From, State) ->
    logger:log(error, "Config server: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "unknown gen_server call in config server"}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast(Unknown, State) ->
    logger:log(error, "Config server: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_info(Info, State) ->
    logger:log(error, "Config server: Received unknown signal :~n~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change(_OldVsn, State, _Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    %% XXX has to reload/re-validate config in ets table in case
    %% the configuration specification changes
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: parse(ExtraCfg, State)
%%           ExtraCfg = yxa_cfg record()
%%           State    = state record()
%% Descrip.: Let all our backends parse their config and then merge
%%           it together into our final configuration. Merge ExtraCfg
%%           in last of all. The merging is from right to left, so the
%%           order of the backends in State is crucial - last backend
%%           wins if there is a merge conflict.
%% Returns : {ok, NewCfg}          |
%%           {error, Backend, Msg}
%%           NewCfg  = yxa_cfg record()
%%           Backend = atom(), module name
%%           Msg     = term()
%%--------------------------------------------------------------------
parse(ExtraCfg, State) when is_record(ExtraCfg, yxa_cfg), is_record(State, state) ->
    Backends = State#state.backends,
    case parse2(Backends, []) of
	{ok, Parsed} when is_list(Parsed) ->
	    %% now merge in entrys from ExtraCfg
	    ExtraEntrys = ExtraCfg#yxa_cfg.entrys,
	    yxa_config_util:startup_log(?MODULE, debug, "Merging in ~p entrys from ExtraCfg",
					[length(ExtraEntrys)]),
	    NewEntrys = merge_entrys(Parsed, ExtraEntrys),
	    {ok, ExtraCfg#yxa_cfg{entrys = NewEntrys}};
	{error, Module, Msg} ->
	    {error, Module, Msg}
    end.

parse2([{Module, Opaque} | T], Entrys) when is_list(Entrys) ->
    yxa_config_util:startup_log(?MODULE, debug, "Calling the parse/1 function of backend '~p'", [Module]),
    try Module:parse(Opaque) of
	{error, Msg} ->
	    {error, Module, Msg};
	continue ->
	    parse2(T, Entrys);
	{ok, This} when is_record(This, yxa_cfg) ->
	    yxa_config_util:startup_log(?MODULE, debug, "Backend '~p' returned ~p entrys",
					[Module, length(This#yxa_cfg.entrys)]),
	    NewEntrys = merge_entrys(Entrys, This#yxa_cfg.entrys),
	    parse2(T, NewEntrys)
    catch
	throw:
	  {error, Msg} ->
	    {error, Module, Msg}
    end;
parse2([], Entrys) ->
    {ok, lists:keysort(1, Entrys)}.

%%--------------------------------------------------------------------
%% Function: merge_entrys(Left, Right)
%%           Left  = list() of {Key, Value, Src}
%%           Right = list() of {Key, Value, Src}
%% Descrip.: Merge entrys from Right to Left
%% Returns : NewEntrys = list() of {Key, Value, Src}
%%--------------------------------------------------------------------
merge_entrys(Entrys, [{Key, Value, Src} | T]) ->
    NewL =
	case lists:keyreplace(Key, 1, Entrys, {Key, Value, Src}) of
	    Entrys ->
		%% no change, we should add (prepend) our tuple
		[{Key, Value, Src} | Entrys];
	    NewL1 when is_list(NewL1)  ->
		NewL1
	end,
    merge_entrys(NewL, T);
merge_entrys(Entrys, []) ->
    Entrys.

%%--------------------------------------------------------------------
%% Function: validate(Cfg, AppModule, Mode)
%%           Cfg       = yxa_cfg record()
%%           AppModule = atom(), YXA application name
%%           Mode      = soft | hard, fail soft (for reloads) or
%%                       hard (for initial startup)?
%% Descrip.: Validate and normalize (according to the type
%%           declarations) all configuration entrys in Cfg.
%% Returns : {ok, NewCfg} |
%%           {error, Msg}
%%           NewCfg = yxa_cfg record()
%%           Msg    = string()
%%--------------------------------------------------------------------
validate(Cfg, AppModule, Mode) when is_record(Cfg, yxa_cfg), is_atom(AppModule), (Mode == soft orelse Mode == hard) ->
    yxa_config_check:check_config(Cfg, AppModule, Mode).

%%--------------------------------------------------------------------
%% Function: load(Cfg, Mode, State)
%%           Cfg   = yxa_cfg record()
%%           Mode  = soft | hard, fail soft (for reloads) or
%%                   hard (for initial startup)?
%%           State = state record()
%% Descrip.: Load a parsed and validated config into our configuration
%%           storage.
%% Returns : ok
%%--------------------------------------------------------------------
load(Cfg, Mode, State) when is_record(Cfg, yxa_cfg), (Mode == soft orelse Mode == hard) ->
    ok = load_set(Cfg#yxa_cfg.entrys, Mode, State#state.etsref),
    ok = delete_not_present(Cfg, Mode, State#state.etsref),
    ok = insert_implicit(State),
    ok.

%% part of load/3
load_set([{Key, Value, Src} | T], Mode, EtsRef) ->
    case ets:lookup(EtsRef, Key) of
	[{Key, OldValue, _OldSrc}] when OldValue == Value ->
	    %% not changed
	    ok;
	[{Key, OldValue, _OldSrc}] ->
	    %% changed
	    ok = update(Key, Value, Src, OldValue, Mode, EtsRef);
	[] ->
	    %% new value
    	    true = ets:insert_new(EtsRef, {Key, Value, Src}),
	    case Mode of
		soft ->
		    %% don't disclose sensitive information in log files
		    case lists:member(Key, ?NO_DISCLOSURE) of
			true ->
			    logger:log(debug, "Config server: Set configuration parameter '~p' (value not shown)",
				       [Key]);
			false ->
			    logger:log(debug, "Config server: Set configuration parameter '~p' (value : ~p)",
				       [Key, Value])
		    end;
		hard ->
		    ok
	    end,
	    ok
    end,
    load_set(T, Mode, EtsRef);
load_set([], _Mode, _EtsRef) ->
    ok.

%% part of load_set/3, Return  : ok
update(Key, undefined, yxa_config_default, OldValue, soft, EtsRef) ->
    true = ets:insert(EtsRef, {Key, undefined, yxa_config_default}),
    case lists:member(Key, ?NO_DISCLOSURE) of
	true ->
	    logger:log(debug, "Config server: Deleting parameter '~p' (value not shown)",
		       [Key]);
	false ->
	    logger:log(debug, "Config server: Deleting parameter '~p' (old value: ~p)",
		       [Key, OldValue])
    end,
    ok;
update(Key, Value, Src, OldValue, Mode, EtsRef) ->
    true = ets:insert(EtsRef, {Key, Value, Src}),
    case Mode of
	soft ->
	    %% don't disclose sensitive information in log files
	    case lists:member(Key, ?NO_DISCLOSURE) of
		true ->
		    logger:log(debug, "Config server: Updated configuration parameter '~p' (value not shown)",
			       [Key]);
		false ->
		    logger:log(debug, "Config server: Updated configuration parameter '~p' "
			       "(old value : ~p, new value ~p)", [Key, OldValue, Value])
	    end;
	hard ->
	    %% don't log 'changes' on startup
	    ok
    end,
    ok.

%% part of load/3, delete keys from ets table not present in Cfg. Happens if a code change/other
%% makes our config definitions change.
delete_not_present(Cfg, _Mode, EtsRef) ->
    L = list(EtsRef),
    CheckPurge =
	fun({Key, OldValue, _Src}) ->
		case lists:keysearch(Key, 1, Cfg#yxa_cfg.entrys) of
		    {value, {Key, _NewValue, _NewSrc}} ->
			%% Key still valid, do nothing
			ok;
		    false ->
			case lists:member(Key, ?NO_DISCLOSURE) of
			    true ->
				logger:log(debug, "Config server: Purging parameter '~p' (value not shown)",
					   [Key]);
			    false ->
				logger:log(debug, "Config server: Purging parameter '~p' (old value: ~p)",
					   [Key, OldValue])
			end,
			true = ets:delete(EtsRef, Key)
		end
	end,
    lists:map(CheckPurge, L),
    ok.

%% part of load/3 - forces certain values for certain applications
insert_implicit(State) when is_record(State, state) ->
    #state{appmodule = AppModule,
	   etsref    = EtsRef
	  } = State,
    true = ets:insert(EtsRef, {yxa_appmodule, AppModule, yxa_config_default}),
    ok.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->
    %% merge_entrys(Left, Right)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "merge_entrys/2 - 1"),
    [{a,b,c}] = merge_entrys([], [{a,b,c}]),

    autotest:mark(?LINE, "merge_entrys/2 - 2"),
    [{a,b,c}] = merge_entrys([{a,a,a}], [{a,b,c}]),

    autotest:mark(?LINE, "merge_entrys/2 - 3"),
    [{a,b,c}] = merge_entrys([], [{a,b,c}]),

    autotest:mark(?LINE, "merge_entrys/2 - 4"),
    [{a,b,c},
     {b,2,2}] = merge_entrys([{b,2,2}], [{a,b,c}]),

    ok.
