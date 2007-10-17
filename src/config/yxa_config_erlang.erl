%%%-------------------------------------------------------------------
%%% File    : yxa_config_erlang.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Config backend for parsing plain files containing Erlang
%%%           terms.
%%%
%%% @since    16 Jun 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
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
	  appmodule,	%% atom(), YXA application module
	  recursing	%% bool()
	 }).

%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    ([AppModule]) ->
%%            {ok, State} | ignore | {error, Msg}
%%
%%            AppModule = atom() "YXA application module"
%%
%%            State = #yxa_config_erlang_state{}
%%            Msg   = string()
%%
%% @doc     Initiates the configuration backend.
%% @hidden
%% @end
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
    case is_readable_file(FileName) of
	true ->
	    yxa_config_util:startup_log(?MODULE, debug, "Will read config from file ~p", [FileName]),
	    State = #yxa_config_erlang_state{
	      filename  = FileName,
	      appmodule = AppModule,
	      recursing = false
	     },
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
%% @spec    (State) ->
%%            {ok, State} | continue | {error, Msg}
%%
%%            State = #yxa_config_erlang_state{}
%%
%%            State = #yxa_config_erlang_state{}
%%            Msg   = string()
%%
%% @doc     Initiates the configuration backend.
%% @end
%%--------------------------------------------------------------------
parse(State) when is_record(State, yxa_config_erlang_state) ->
    try begin
	    case parse2(State) of
		{ok, TermL} when is_list(TermL) ->
		    AppModule = State#yxa_config_erlang_state.appmodule,
		    Terms = extract_config(TermL, AppModule),
		    %% turn all {Key, Value} tuples we get from extract_config() into
		    %% the {Key, Value, ?MODULE} tuples that this function should return
		    FormattedTerms =
			lists:map(fun({K, V}) ->
					  {K, V, ?MODULE}
				  end, Terms),
		    Cfg = #yxa_cfg{entrys = FormattedTerms},
		    {ok, Cfg};
		Res1 ->
		    Res1
	    end
	end of
	Res ->
	    Res
    catch
	throw:
	  {error, Msg} ->
	    {error, Msg}
    end.


%%====================================================================
%% Internal functions
%%====================================================================

%% parse2 parses the configuration file and recurses into included files
parse2(State) when is_record(State, yxa_config_erlang_state) ->
    File = State#yxa_config_erlang_state.filename,
    yxa_config_util:startup_log(?MODULE, debug, "Consulting file ~p", [File]),
    case file:consult(File) of
	{ok, [TermL]} when is_list(TermL) ->
	    parse_includes(State, TermL);
	{ok, _Unknown} ->
	    Msg = io_lib:format("Could not parse data in file ~p.",
				[File]),
	    {error, lists:flatten(Msg)};
	{error, {Line, Mod, Term}} ->
	    Msg = io_lib:format("Failed parsing file ~p, line ~p. Module ~p - term ~p",
				[File, Line, Mod, Term]),
	    {error, lists:flatten(Msg)};
	{error, E} ->
	    Msg = io_lib:format("Failed parsing file ~p : ~p",
				[File, E]),
	    throw({error, lists:flatten(Msg)})
    end.

%% parse_includes/2, part of parse2/1
parse_includes(State, TermL) when is_record(State, yxa_config_erlang_state), is_list(TermL) ->
    parse_includes2(State, TermL, []).

parse_includes2(State, [{include, Inc} | T], Res) when is_record(State, yxa_config_erlang_state), is_list(Inc) ->
    CurrentFile = State#yxa_config_erlang_state.filename,
    case State#yxa_config_erlang_state.recursing of
	false ->
	    IncFile =
		case filename:pathtype(Inc) of
		    absolute ->
			Inc;
		    _ ->
			filename:absname_join(filename:dirname(CurrentFile), Inc)
		end,

	    case parse2(State#yxa_config_erlang_state{recursing = true, filename = IncFile}) of
		{ok, This} ->
		    parse_includes2(State, T, This ++ Res);
		{error, Reason} ->
		    {error, Reason}
	    end;
	true ->
	    Msg = io_lib:format("Multiple levels of configuration file includes not permitted (in ~p)",
				[CurrentFile]),
	    throw({error, lists:flatten(Msg)})
    end;
parse_includes2(State, [H | T], Res) when is_record(State, yxa_config_erlang_state), is_list(Res) ->
    %% move all non-include statements to Res
    parse_includes2(State, T, [H | Res]);
parse_includes2(_State, [], Res) when is_list(Res) ->
    {ok, Res}.

%%--------------------------------------------------------------------
%% @spec    (TermL, AppModule) ->
%%            Cfg
%%
%%            TermL     = [term()]
%%            AppModule = atom() "application for which to load config"
%%
%%            Cfg = [{Key, Value}]
%%
%% @doc     Get the 'common' and the application specific sections of
%%          TermL, and merge them. Returns a sorted list.
%% @end
%%--------------------------------------------------------------------
extract_config(TermL, AppModule) when is_list(TermL), is_atom(AppModule) ->
    {common, CommonConfig} = get_section(common, TermL),
    {AppModule, AppConfig} = get_section(AppModule, TermL),

    merge_config(CommonConfig, AppConfig).

get_section(Key, TermL) when is_atom(Key), is_list(TermL) ->
    Res = proplists:append_values(Key, TermL),
    ok = check_for_dups(Key, Res),
    {Key, Res}.

%% There must not be two instances of a parameter inside the same section.
check_for_dups(Section, Data) ->
    lists:foldl(fun({Key, _Value}, Previous) when Key == Previous ->
			Msg = io_lib:format("Duplicate configuration parameter '~p' in section '~p'",
					    [Key, Section]),
			throw({error, lists:flatten(Msg)});
		   ({Key, _Value}, _Previous) ->
			Key
		end, [], lists:keysort(1, Data)),
    ok.

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
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    %% parse_includes(State, TermL)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "parse_includes/2 - 0"),
    ParseIncludesState1 = #yxa_config_erlang_state{filename  = "test",
						   recursing = false},

    ParseIncludesState2 = #yxa_config_erlang_state{filename  = "test",
						   recursing = true},

    autotest:mark(?LINE, "parse_includes/2 - 1"),
    {ok, [{foo, bar}]} = parse_includes(ParseIncludesState1, [{foo, bar}]),

    autotest:mark(?LINE, "parse_includes/2 - 2"),
    {ok, [{foo, bar}]} = parse_includes(ParseIncludesState2, [{foo, bar}]),

    autotest:mark(?LINE, "parse_includes/2 - 3"),
    {error, "Multiple levels" ++ _} = (catch parse_includes(ParseIncludesState2, [{include, "testfile"}])),


    %% get_section(Key, TermL)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "get_section/2 - 0"),
    GS_Term_Test = {test, [{1, 2},
			   {2, 3}
			  ]},

    GS_Term_Common = {common, [{5, 6},
			       {6, 5}
			      ]},

    GS_Term_Foo = {foo, [{0, 10},
			 {1, 11}
			]},

    GS_Term_Dups = {dups_test, [{dup, 10},
				{dup, 11}
			       ]},

    autotest:mark(?LINE, "get_section/2 - 1"),
    GS_Term_Test = get_section(test, [GS_Term_Test]),

    autotest:mark(?LINE, "get_section/2 - 2"),
    GS_Term_Test = get_section(test, [GS_Term_Foo, GS_Term_Test]),

    autotest:mark(?LINE, "get_section/2 - 3"),
    GS_Term_Common = get_section(common, [GS_Term_Foo, GS_Term_Common, GS_Term_Test]),

    autotest:mark(?LINE, "get_section/2 - 4"),
    {error, "Duplicate configuration parameter 'dup' in section 'dups_test'"} =
	(catch get_section(dups_test, [GS_Term_Dups])),


    %% merge_config(Config1, Config2)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "merge_config/2 - 0"),
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

    autotest:mark(?LINE, "merge_config/2 - 1"),
    %% sipauth_password #2 should replace #1
    [{sipauth_password,             "other-pw"},
     {sipauth_realm,                "test.example.org"},
     {sipuserdb_file_filename,      "/etc/yxa-userdb"},
     {userdb_modules,               [sipuserdb_file]}
    ] = merge_config(MConfig_1, MConfig_2),

    autotest:mark(?LINE, "merge_config/2 - 2"),
    %% #1 should be totally overwritten by list #2
    MConfig_1_S = merge_config(MConfig_2, MConfig_1),

    autotest:mark(?LINE, "merge_config/2 - 3"),
    %% record_route should 'shine through'
    [{record_route,	       true},
     {sipauth_password,        "pw"},
     {sipauth_realm,           "test.example.org"},
     {sipuserdb_file_filename, "/etc/yxa-userdb"},
     {userdb_modules,          [sipuserdb_file]}
    ] = merge_config(MConfig_3, MConfig_1),

    autotest:mark(?LINE, "merge_config/2 - 4"),
    %% no conflicts at all
    [{record_route,true},
     {sipauth_password,"other-pw"},
     {sipauth_realm,"example.org"}
    ] = merge_config(MConfig_3, MConfig_2),

    ok.
