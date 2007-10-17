%%%-------------------------------------------------------------------
%%% File    : yxa_config.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      YXA configuration subsystem.
%%%
%%% @since    15 Jun 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
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
%% @type state() = #state{}.
%%                 no description
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
-define(BACKENDS, [yxa_config_default,
		   yxa_config_erlang
		  ]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (AppModule) ->
%%            {ok, Pid}
%%
%%            AppModule = atom() "YXA application module"
%%
%%            Pid = pid() "yxa_config persistent gen_server pid"
%%
%% @doc     start the server.
%% @end
%%--------------------------------------------------------------------
start_link(AppModule) when is_atom(AppModule) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {AppModule}, []).

%%--------------------------------------------------------------------
%% @spec    ({AppModule}) ->
%%            {ok, State}
%%
%%            AppModule = atom() "YXA application module"
%%
%%            State = #state{}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({AppModule}) when is_atom(AppModule) ->
    yxa_config_util:startup_log(?MODULE, debug, "Starting configuration subsystem for '~p'", [AppModule]),
    EtsRef = ets:new(?YXA_CONFIG, [protected, set, named_table]),
    ExtraCfg = #yxa_cfg{},
    init_config(?BACKENDS, AppModule, ExtraCfg, EtsRef).

%%--------------------------------------------------------------------
%% @spec    (Backends, AppModule, ExtraCfg, EtsRef) ->
%%            {ok, State}
%%
%%            Backends  = [atom()] "backend module names"
%%            AppModule = atom() "YXA application module"
%%            ExtraCfg  = #yxa_cfg{} "config to append"
%%            EtsRef    = term() "ets table to load config into"
%%
%%            State = #state{}
%%
%% @doc     Part of init/1 and exported for yxa_test_config.
%% @private
%% @end
%%--------------------------------------------------------------------
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
%% @spec    (In, AppModule) ->
%%            Res
%%
%%            In        = [atom()] "list of module names"
%%            AppModule = atom() "YXA application module"
%%
%%            Res     = [{Backend, Opaque}]
%%            Backend = atom()
%%            Opaque  = term()
%%
%% @doc     Calls the init/1 function in each backend, and returns the
%%          backends opaque data structures that will later be passed
%%          to their parse/1 functions.
%% @end
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
%% @spec    () ->
%%            ok | {error, Where, Msg}
%%
%%            Where = atom()
%%            Msg   = string()
%%
%% @doc     Parse, validate and load the configuration (again).
%% @end
%%--------------------------------------------------------------------
reload() ->
    gen_server:call(yxa_config, reload).

%%--------------------------------------------------------------------
%% @spec    (callbacks) -> [tuple()]
%%
%% @doc     Describe all the API functions a module indicating it is
%%          an yxa_config behaviour module must export. List of
%%          tuples of the function names and their arity.
%% @hidden
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{init, 1},
     {parse, 1}
    ];
behaviour_info(_Other) ->
    undefined.

%%--------------------------------------------------------------------
%% @spec    (Key) ->
%%            {ok, Value} |
%%            Value       |
%%            none
%%
%%            Key = atom()
%%
%% @doc     Fetch parameter value. Return 'none' if parameter is known
%%          but not set. throw() if unknown parameter is requested.
%% @end
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

%%--------------------------------------------------------------------
%% @spec    (Key, Default) ->
%%            {ok, Value} |
%%            Value       |
%%            none
%%
%%            Key     = atom()
%%            Default = term() "returned if no value is present"
%%
%% @doc     Fetch parameter value, with a default specified by the
%%          caller. throw() if unknown parameter is requested.
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @spec    () ->
%%            [ConfigTuple]
%%
%%            ConfigTuple = {Key, Value, Source}
%%            Key         = atom()
%%            Value       = term()
%%            Source      = atom() "backend module this entry came from"
%%
%% @doc     Return a list of tuples with the current configuration.
%% @end
%%--------------------------------------------------------------------
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
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% @spec    (reload, From, State) ->
%%            {reply, Reply, State}
%%
%%            Reply = ok | {error, Where, Msg}
%%            Where = parse | validation | load
%%
%% @doc     Reload configuration.
%% @hidden
%% @end
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
			try load(Normalized, soft, State) of
			    ok ->
				logger:log(normal, "Config server: Finished reloading configuration"),
				ok
			catch
			    throw:
			      {error, Reason} when is_list(Reason) ->
				{error, load, Reason}
			end;
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
%% @spec    handle_cast(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Log unknown casts we receive.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "Config server: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @spec    handle_info(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling all non call/cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Log unknown signals we receive.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(Unknown, State) ->
    logger:log(error, "Config server: Received unknown signal :~n~p", [Unknown]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (_OldVsn, State, _Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    %% XXX has to reload/re-validate config in ets table in case
    %% the configuration specification changes
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @spec    (ExtraCfg, State) ->
%%            {ok, NewCfg}          |
%%            {error, Backend, Msg}
%%
%%            ExtraCfg = #yxa_cfg{}
%%            State    = #state{}
%%
%%            NewCfg  = #yxa_cfg{}
%%            Backend = atom() "module name"
%%            Msg     = term()
%%
%% @doc     Let all our backends parse their config and then merge it
%%          together into our final configuration. Merge ExtraCfg in
%%          last of all. The merging is from right to left, so the
%%          order of the backends in State is crucial - last backend
%%          wins if there is a merge conflict.
%% @end
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
%% @spec    (Left, Right) ->
%%            NewEntrys
%%
%%            Left  = [{Key, Value, Src}]
%%            Right = [{Key, Value, Src}]
%%
%%            NewEntrys = [{Key, Value, Src}]
%%
%% @doc     Merge entrys from Right to Left
%% @end
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
%% @spec    (Cfg, AppModule, Mode) ->
%%            {ok, NewCfg} |
%%            {error, Msg}
%%
%%            Cfg       = #yxa_cfg{}
%%            AppModule = atom() "YXA application name"
%%            Mode      = soft | hard
%%
%%            NewCfg = #yxa_cfg{}
%%            Msg    = string()
%%
%% @doc     Validate and normalize (according to the type
%%          declarations) all configuration entrys in Cfg. Mode is
%%          our fail mode - soft (for reloads) or hard (for initial
%%          startup).
%% @end
%%--------------------------------------------------------------------
validate(Cfg, AppModule, Mode) when is_record(Cfg, yxa_cfg), is_atom(AppModule), (Mode == soft orelse Mode == hard) ->
    yxa_config_check:check_config(Cfg, AppModule, Mode).

%%--------------------------------------------------------------------
%% @spec    (Cfg, Mode, State) -> ok
%%
%%            Cfg   = #yxa_cfg{}
%%            Mode  = soft | hard
%%            State = #state{}
%%
%% @doc     Load a parsed and validated config into our configuration
%%          storage. Mode is our fail mode - soft (for reloads) or
%%          hard (for initial startup).
%% @end
%%--------------------------------------------------------------------
load(Cfg, Mode, State) when is_record(Cfg, yxa_cfg), (Mode == soft orelse Mode == hard) ->
    ok = load_set(Cfg#yxa_cfg.entrys, Mode, State#state.etsref),
    ok = delete_not_present(Cfg, Mode, State#state.etsref),
    ok = insert_implicit(State),
    ok.

%% part of load/3
load_set([{Key, Value, Src} | T], Mode, EtsRef) ->
    Res =
	case ets:lookup(EtsRef, Key) of
	    [{Key, OldValue, _OldSrc}] when OldValue == Value ->
		%% not changed
		ok;
	    [{Key, OldValue, _OldSrc}] ->
		%% changed
		update(Key, Value, Src, OldValue, Mode, EtsRef);
	    [] ->
		%% new value
		insert_new(Key, Value, Src, Mode, EtsRef)
	end,

    case Res of
	ok ->
	    load_set(T, Mode, EtsRef);
	{error, Reason} when is_list(Reason) ->
	    logger:log(error, "Config server: Failed setting/updating configuration parameter '~p' "
		       "(value: ~s) : ~p", [Key, value_for_logging(Key, Value), Reason]),
	    %% XXX we might have loaded half of a configuration change if we end up here,
	    %% but this is our only option besides shutting the whole node down which
	    %% might be even worse if someone made a trivial error in the configuration
	    Msg = io_lib:format("Failed setting/updating configuration parameter '~p'", [Key]),
	    throw({error, lists:flatten(Msg)})
    end;
load_set([], _Mode, _EtsRef) ->
    ok.

%% part of load_set/3, Return  : ok | {error, Reason}
update(Key, undefined, yxa_config_default, OldValue, soft, EtsRef) ->
    case change_action(Key, undefined, soft) of
	ok ->
	    true = ets:insert(EtsRef, {Key, undefined, yxa_config_default}),
	    logger:log(debug, "Config server: Deleted configuration parameter '~p' (old value: ~s)",
		       [Key, value_for_logging(Key, OldValue)]),
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end;
update(Key, Value, Src, OldValue, Mode, EtsRef) ->
    case change_action(Key, Value, Mode) of
	ok ->
	    true = ets:insert(EtsRef, {Key, Value, Src}),
	    case Mode of
		soft ->
		    logger:log(debug, "Config server: Updated configuration parameter '~p' "
			       "(old value : ~s, new value : ~s)",
			       [Key, value_for_logging(Key, OldValue), value_for_logging(Key, Value)]);
		hard ->
		    %% don't log 'changes' on startup
		    ok
	    end,
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.

%% part of load_set/3, Return  : ok | {error, Reason}
insert_new(Key, Value, Src, Mode, EtsRef) ->
    case change_action(Key, Value, Mode) of
	ok ->
	    true = ets:insert_new(EtsRef, {Key, Value, Src}),
	    case Mode of
		soft ->
		    %% only log for configuration reload requests, not initial startup
		    logger:log(debug, "Config server: Set configuration parameter '~p' (value: ~s)",
			       [Key, value_for_logging(Key, Value)]);
		hard ->
		    ok
	    end,
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.


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
		    false when Key == yxa_appmodule ->
			%% see insert_implicit/1
			ok;
		    false ->
			logger:log(debug, "Config server: Purging configuration parameter '~p' (old value: ~s)",
				   [Key, value_for_logging(Key, OldValue)]),
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

value_for_logging(Key, Value) ->
    case lists:member(Key, ?NO_DISCLOSURE) of
	true ->
	    %% don't log sensitive information
	    "not shown";
	false ->
	    io_lib:format("~p", [Value])
    end.

%%--------------------------------------------------------------------
%% @spec    (Key, Value, Mode) ->
%%            ok | {error, Reason}
%%
%%            Key   = atom()
%%            Value = term()
%%            Mode  = soft | hard
%%
%%            Reason = string()
%%
%% @doc     Perform any necessary actions when a configuration value
%%          changes, like perhaps notifying a gen_server or similar.
%% @end
%%--------------------------------------------------------------------
change_action(sipuserdb_file_refresh_interval, Value, soft) ->
    case sipuserdb_file_backend:change_interval(Value) of
	ok ->
	    ok;
	Error ->
	    {error, io_lib:format("~p", [Error])}
    end;
change_action(Key, Value, Mode) ->
    case atom_to_list(Key) of
	"local_" ++ _ ->
	    local:config_change_action(Key, Value, Mode);
	_ ->
	    ok
    end.

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
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
