%%%-------------------------------------------------------------------
%%% File    : yxa_config_erlang.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Config backend for parsing plain files containing Erlang
%%%           terms.
%%%
%%% Created : 16 Jun 2005 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(yxa_config_erlang).

-behaviour(yxa_config).

-export([
	 init/1,
	 parse/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("yxa_config.hrl").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(yxa_config_erlang_state, {
	  filename,	%% string(), filename
	  appmodule	%% atom(), Yxa application module
	 }).

%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: init([AppModule])
%%	     AppModule = atom(), Yxa application module
%% Descrip.: Initiates the configuration backend.
%% Returns : {ok, State} | ignore | {error, Msg}
%%           State = yxa_config_erlang_state record()
%%           Msg = string()
%%--------------------------------------------------------------------
init(AppModule) ->
    FileName =
	case application:get_env(yxa, config_erlang_fn) of
	    undefined ->
		%% No config file indicated in the OTP application environment,
		%% look in command line we were started with
		InitArgs = init:get_arguments(),
		get_args_filename(InitArgs);
	    {ok, FileName1} when is_list(FileName1) ->
		FileName1
	end,
    State = #yxa_config_erlang_state{
      filename         = FileName,
      appmodule        = AppModule
     },
    case is_readable_file(FileName) of
	true ->
	    {ok, State};
	false ->
	    Msg = io_lib:format("File ~p not readable", [FileName]),
	    {error, Msg};
	ignore ->
	    ignore
    end.

%% part of init/1
get_args_filename(In) ->
    case lists:keysearch(config_erlang_fn, 1, In) of
	{value, {config_erlang_fn, [Fn1]}} when is_list(Fn1) ->
	    Fn1;
	false ->
	    case lists:keysearch(yxa_config, 1, In) of
		{value, {yxa_config, [Fn2]}} when is_list(Fn2) ->
		    Fn2;
		false ->
		    none
	    end
    end.

%% part of init/1
is_readable_file(none) ->
    ignore;
is_readable_file(Fn) when is_list(Fn) ->
    case file:open(Fn, [read, raw]) of
	{ok, D} ->
	    file:close(D),
	    true;
	{error, _Reason} ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: parse(State)
%%	     State = yxa_config_erlang_state record()
%% Descrip.: Initiates the configuration backend.
%% Returns : {ok, State} | continue | {error, Msg}
%%           State = yxa_config_erlang_state record()
%%           Msg = string()
%%--------------------------------------------------------------------
parse(State) when is_record(State, yxa_config_erlang_state) ->
    File = State#yxa_config_erlang_state.filename,
    case file:consult(File) of
	{ok, [TermL]} when is_list(TermL) ->
	    AppModule = State#yxa_config_erlang_state.appmodule,
	    L1 = extract_config(TermL, AppModule),
	    %% turn all {Key, Value} tuples we get from extract_config() into
	    %% the {Key, Value, ?MODULE} tuples that this function should return
	    L2 = lists:map(fun({K, V}) ->
				   {K, V, ?MODULE}
			   end, L1),
	    Cfg = #yxa_cfg{entrys = L2},
	    {ok, Cfg};
	{error, {Line, Mod, Term}} ->
	    Msg = io_lib:format("Failed parsing file ~p, line ~p. Module ~p - term ~p",
				[File, Line, Mod, Term]),
	    {error, lists:flatten(Msg)};
	{error, E} ->
	    Msg = io_lib:format("Failed parsing file ~p : ~p",
				[File, E]),
	    {error, lists:flatten(Msg)}
    end.


%%====================================================================
%% Internal functions
%%====================================================================


extract_config(TermL, AppModule) when is_list(TermL), is_atom(AppModule) ->
    {common, CommonConfig} = get_section(common, TermL),
    {AppModule, AppConfig} = get_section(AppModule, TermL),

    merge_config(CommonConfig, AppConfig).

get_section(Key, TermL) when is_atom(Key), is_list(TermL) ->
    case lists:keysearch(Key, 1, TermL) of
	{value, {Key, Res}} when is_list(Res) ->
	    {Key, Res};
	false ->
	    {Key, []}
    end.

merge_config(Config, [{Key, Value} | T]) ->
    NewL =
	case lists:keyreplace(Key, 1, Config, {Key, Value}) of
	    Config ->
		%% no change, we should add (prepend) our tuple
		[{Key, Value} | Config];
	    NewL1 when is_list(NewL1)  ->
		NewL1
	end,
    merge_config(NewL, T);
merge_config(Config, []) ->
    lists:keysort(1, Config).


%%====================================================================
%% Test functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->

    %% get_section(Key, TermL)
    %%--------------------------------------------------------------------

    io:format("test: get_section/2 - 0~n"),
    GS_Term_Test = {test, [{1, 2},
			   {2, 3}
			  ]},

    GS_Term_Common = {common, [{5, 6},
			       {6, 5}
			      ]},

    GS_Term_Foo = {foo, [{0, 10},
			 {1, 11}
			]},

    io:format("test: get_section/2 - 1~n"),
    GS_Term_Test = get_section(test, [GS_Term_Test]),

    io:format("test: get_section/2 - 2~n"),
    GS_Term_Test = get_section(test, [GS_Term_Foo, GS_Term_Test]),

    io:format("test: get_section/2 - 3~n"),
    GS_Term_Common = get_section(common, [GS_Term_Foo, GS_Term_Common, GS_Term_Test]),


    %% merge_config(Config1, Config2)
    %%--------------------------------------------------------------------

    io:format("test: merge_config/2 - 0~n"),
    MConfig_1 = [{userdb_modules,		[sipuserdb_file]},
		 {sipuserdb_file_filename,	"/etc/yxa-userdb"},
		 {sipauth_realm,		"test.example.org"},
		 {sipauth_password,		"pw"}
		],
    MConfig_1_S = lists:keysort(1, MConfig_1),

    MConfig_2 = [{sipauth_password,		"other-pw"}
		],

    MConfig_3 = [{sipauth_realm,		"example.org"},
		 {record_route,			true}
		],

    io:format("test: merge_config/2 - 1~n"),
    %% sipauth_password #2 should replace #1
    [{sipauth_password,             "other-pw"},
     {sipauth_realm,                "test.example.org"},
     {sipuserdb_file_filename,      "/etc/yxa-userdb"},
     {userdb_modules,               [sipuserdb_file]}
    ] = merge_config(MConfig_1, MConfig_2),

    io:format("test: merge_config/2 - 2~n"),
    %% #1 should be totally overwritten by list #2
    MConfig_1_S = merge_config(MConfig_2, MConfig_1),

    io:format("test: merge_config/2 - 3~n"),
    %% record_route should 'shine through'
    [{record_route,	       true},
     {sipauth_password,        "pw"},
     {sipauth_realm,           "test.example.org"},
     {sipuserdb_file_filename, "/etc/yxa-userdb"},
     {userdb_modules,          [sipuserdb_file]}
    ] = merge_config(MConfig_3, MConfig_1),

    io:format("test: merge_config/2 - 4~n"),
    %% no conflicts at all
    [{record_route,true},
     {sipauth_password,"other-pw"},
     {sipauth_realm,"example.org"}
    ] = merge_config(MConfig_3, MConfig_2),

    ok.
