%%%-------------------------------------------------------------------
%%% File    : yxa_config_check.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Checking and normalization of config.
%%%
%%% @since    20 Jun 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(yxa_config_check).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 check_config/3,
	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 start_bg_check/2
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("yxa_config.hrl").
-include("siprecords.hrl").

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Cfg, AppModule, Mode) ->
%%            {ok, NewCfg} |
%%            {error, Msg}
%%
%%            Cfg       = #yxa_cfg{}
%%            AppModule = atom() "YXA application module"
%%            Mode      = soft | hard
%%
%%            NewCfg = #yxa_cfg{}
%%            Msg    = string()
%%
%% @doc     Check Cfg and return a new yxa_cfg record with all the
%%          values normalized, or an error. Mode is failure mode -
%%          soft for config reloads and hard for initial startup.
%% @end
%%--------------------------------------------------------------------
check_config(Cfg, AppModule, Mode) when is_record(Cfg, yxa_cfg), is_atom(AppModule), Mode == soft; Mode == hard ->
    Definitions = get_cfg_definitions(AppModule),
    case check_types(Cfg, Definitions) of
	{ok, NewCfg} when is_record(NewCfg, yxa_cfg) ->
	    case check_required(NewCfg, Definitions) of
		ok ->
		    case check_config_dependencys(NewCfg) of
			ok ->
			    case check_application_specific(AppModule, NewCfg) of
				ok ->
				    case check_loadable(NewCfg, Definitions, Mode) of
					ok ->
					    {ok, NewCfg};
					{error, Msg} when is_list(Msg) ->
					    {error, Msg}
				    end;
				{error, Msg} when is_list(Msg) ->
				    {error, Msg}
			    end;
			{error, Msg} when is_list(Msg) ->
			    {error, Msg}
		    end;
		{error, Msg} when is_list(Msg) ->
		    {error, Msg}
	    end;
	{error, Msg} when is_list(Msg) ->
	    {error, Msg}
    end.

%%--------------------------------------------------------------------
%% @spec    (Cfg, AppModule) ->
%%            {ok, Pid}
%%
%%            Cfg       = #yxa_cfg{}
%%            AppModule = atom() "YXA application module"
%%
%%            Pid = pid() "config checker started"
%%
%% @doc     Start sanity checks of parameters in the background to
%%          warn about things. NOTE : Not yet implemented.
%% @end
%%--------------------------------------------------------------------
start_bg_check(Cfg, AppModule) when is_record(Cfg, yxa_cfg), is_atom(AppModule) ->
    %% warn if detect_loops is not 'true'
    %% warn if stateless_challenges is not 'false'
    %% check that ldap_server resolves
    not_implemented.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (AppModule) ->
%%            Entrys
%%
%%            AppModule = atom() "YXA application module"
%%
%%            Entrys = [#cfg_entry{}]
%%
%% @doc     Get common + more specific configuration definitions for
%%          this application.
%% @end
%%--------------------------------------------------------------------
get_cfg_definitions(AppModule) when is_atom(AppModule) ->
    AppConfig = case lists:keysearch(AppModule, 1, ?APPLICATION_DEFAULTS) of
		    {value, {AppModule, AppConfig1}} when is_list(AppConfig1) ->
			AppConfig1;
		    false ->
			[]
		end,
    merge_cfg_entrys(?COMMON_DEFAULTS, AppConfig).

%%--------------------------------------------------------------------
%% @spec    (Entrys, In) ->
%%            Entrys
%%
%%            Entrys = [#cfg_entry{}]
%%            In     = [#cfg_entry{}]
%%
%%            Entrys = [#cfg_entry{}]
%%
%% @doc     Merge all entrys in In into Entrys, overwriting any
%%          duplicates in Entrys with the values from In. Return a
%%          sorted list (so that it is testable).
%% @end
%%--------------------------------------------------------------------
merge_cfg_entrys(Entrys, [H | T]) when is_record(H, cfg_entry) ->
    NewEntrys = replace_or_append(Entrys, H, []),
    merge_cfg_entrys(NewEntrys, T);
merge_cfg_entrys(Entrys, []) ->
    lists:sort(fun cfg_entry_sort/2, Entrys).

%%--------------------------------------------------------------------
%% @spec    (Entrys, This, []) ->
%%            NewEntrys
%%
%%            Entrys = [#cfg_entry{}]
%%            This   = #cfg_entry{}
%%
%%            NewEntrys = [#cfg_entry{}]
%%
%% @doc     Look in Entrys for a record with cfg_entry key matching
%%          the one of This. If it is found, replace that record in
%%          Entrys with This. If it is not found, append This to
%%          Entrys.
%% @end
%%--------------------------------------------------------------------
replace_or_append([#cfg_entry{key = Key} | T], #cfg_entry{key = Key} = This, Seen) ->
    %% match, return Seen (reversed) ++ This ++ T
    lists:reverse([This | Seen]) ++ T;
replace_or_append([], This, Seen) ->
    %% no more input, append This to Seen (reversed)
    lists:reverse([This | Seen]);
replace_or_append([H | T], This, Seen) ->
    %% no match, shift H to Seen
    replace_or_append(T, This, [H | Seen]).

%%--------------------------------------------------------------------
%% @spec    (A, B) ->
%%            NewEntrys
%%
%%            A = #cfg_entry{}
%%            B = #cfg_entry{}
%%
%%            NewEntrys = [#cfg_entry{}]
%%
%% @doc     lists:sort/2 function for sorting a list of cfg_entry
%%          records (sort on cfg_entry.key).
%% @end
%%--------------------------------------------------------------------
cfg_entry_sort(#cfg_entry{key = A}, #cfg_entry{key = B}) when A > B ->
    false;
cfg_entry_sort(_A, _B) ->
    true.


%%--------------------------------------------------------------------
%% @spec    (Cfg, Definitions) ->
%%            {ok, NewCfg} |
%%            {error, Msg}
%%
%%            Cfg         = #yxa_cfg{}
%%            Definitions = [#cfg_entry{}]
%%
%%            NewCfg = #yxa_cfg{}
%%            Msg    = string()
%%
%% @doc     Check that all entrys in Cfg have values matching their
%%          specification (found in Definitions). Return new yxa_cfg
%%          record with normalized values.
%% @end
%%--------------------------------------------------------------------
check_types(Cfg, Definitions) when is_record(Cfg, yxa_cfg), is_list(Definitions) ->
    case check_types2(Cfg#yxa_cfg.entrys, Definitions, []) of
	{ok, NewEntrys} ->
	    {ok, Cfg#yxa_cfg{entrys = NewEntrys}};
	{error, Msg} when is_list(Msg) ->
	    {error, Msg}
    end.

%%--------------------------------------------------------------------
%% @spec    (Entrys, Definitions, []) ->
%%            {ok, NewEntrys} |
%%            {error, Msg}
%%
%%            Entrys      = [{Key, Value, Src}]
%%            Key         = atom()
%%            Value       = term()
%%            Src         = atom() "'source module' of this entry"
%%            Definitions = [#cfg_entry{}]
%%
%%            NewEntrys = [{Key, NewValue, Src}]
%%            Msg       = string()
%%
%% @doc     Check that all keys have values matching their
%%          specification (found in Definitions).
%% @end
%%--------------------------------------------------------------------
check_types2([{Key, Value, Src} | T], Definitions, Res) when is_atom(Key), is_atom(Src) ->
    case get_definition(Key, Definitions) of
	{ok, Def} when is_record(Def, cfg_entry) ->
	    case check_cfg_entry_type(Key, Value, Src, Def) of
		{ok, NewValue} ->
		    check_types2(T, Definitions, [{Key, NewValue, Src} | Res]);
		{error, E} ->
		    {error, E}
	    end;
	nomatch ->
	    case atom_to_list(Key) of
		"local_" ++ _ ->
		    %% don't fail on unknown variables named local_*
		    case check_local_cfg_entry_type(Key, Value, Src) of
			{ok, NewValue} ->
			    check_types2(T, Definitions, [{Key, NewValue, Src} | Res]);
			{error, E} ->
			    {error, E}
		    end;
		_ ->
		    Msg = io_lib:format("Unknown configuration parameter ~p (source: ~p)",
					[Key, Src]),
		    {error, lists:flatten(Msg)}
	    end
    end;
check_types2([], _Definitions, Res) ->
    {ok, lists:reverse(Res)}.

%% part of check_types2/4
get_definition(Key, [#cfg_entry{key = Key} = H | _T]) ->
    %% match
    {ok, H};
get_definition(Key, [H | T]) when is_record(H, cfg_entry) ->
    get_definition(Key, T);
get_definition(_Key, []) ->
    nomatch.


%%--------------------------------------------------------------------
%% @spec    (Key, Value, Src, Def) ->
%%            {ok, NewValue} |
%%            {error, Msg}
%%
%%            Key   = atom()
%%            Value = term()
%%            Src   = atom() "'source module' of this entry"
%%            Def   = #cfg_entry{}
%%
%% @doc     Check a single key-value against it's definition.
%% @end
%%--------------------------------------------------------------------
check_cfg_entry_type(Key, undefined, yxa_config_default, Def) when is_atom(Key), is_record(Def, cfg_entry) ->
    {ok, undefined};
check_cfg_entry_type(Key, Value, Src, Def) when is_atom(Key), is_record(Def, cfg_entry) ->
    try case type_check(Key, Value, Def) of
	    {ok, NewValue} when NewValue /= Value ->
		{ok, NewValue};
	    {ok, NewValue} ->
		%%case lists:member(Key, ?NO_DISCLOSURE) of
		%%    true ->
		%%	logger:log(debug, "Config: Kept ~p (value not shown)", [Key]);
		%%    false ->
		%%	logger:log(debug, "Config: Kept ~p = ~p", [Key, NewValue])
		%%end,
		{ok, NewValue};
	    {error, Msg} when is_list(Msg) ->
		{error, Msg}
	end
    catch
	X:Y ->
	    ST =
		case X of
		    error ->
			io_lib:format(", stacktrace : ~p", [erlang:get_stacktrace()]);
		    _ ->
			""
		end,
	    Expected =
		case Def#cfg_entry.list_of of
		    true ->
			io_lib:format("list of ~p", [Def#cfg_entry.type]);
		    false ->
			atom_to_list(Def#cfg_entry.type)
		end,
	    logger:log(error, "=ERROR REPORT==== from config type checking - ~n"
		       "source ~p, parameter '~p', value '~p', expected ~s :~n~p~s",
		       [Src, Key, Value, Expected, Y, ST]),

	    E = io_lib:format("Could not parse configuration (parameter '~p', caught ~p)", [Key, X]),
	    {error, lists:flatten(E)}
    end.


%%--------------------------------------------------------------------
%% @spec    (Key, Value, Src) ->
%%            {ok, NewValue} |
%%            {error, Msg}
%%
%%            Key   = atom()
%%            Value = term()
%%            Src   = atom() "'source module' of this entry"
%%            Def   = #cfg_entry{}
%%
%% @doc     Part of check_cfg_entry_type/3. Call
%%          local:check_config_type/2 to check an unknown parameter
%%          with key being local_* to validate/normalize the Value.
%%          Note : XXX test this code with actual local_ variables!
%% @end
%%--------------------------------------------------------------------
check_local_cfg_entry_type(Key, Value, Src) ->
    try case local:check_config_type(Key, Value, Src) of
	    {ok, NewValue} when NewValue /= Value ->
		{ok, NewValue};
	    {ok, Value} ->
		{ok, Value};
	    {error, Msg} when is_list(Msg) ->
		{error, Msg}
	end
    catch
	X:Y ->
	    ST =
		case X of
		    error ->
			io_lib:format(", stacktrace : ~p", [erlang:get_stacktrace()]);
		    _ ->
			""
		end,
	    logger:log(error, "=ERROR REPORT==== from LOCAL config type checking - ~n"
		       "source ~p, key ~p, value ~p, :~n~p~s",
		       [Src, Key, Value, Y, ST]),

	    E = lists:flatten("Could not parse configuration (caught " ++ atom_to_list(X) ++ ")"),
	    {error, E}
    end.

%%--------------------------------------------------------------------
%% @spec    (Key, Value, Def) ->
%%            {ok, NewValue} |
%%            {error, Msg}
%%
%%            Key   = atom()
%%            Value = term()
%%            Def   = #cfg_entry{}
%%
%% @doc     Check type of Value. Handle list/non-list issue, and then
%%          call type_check_elements on the value or values.
%% @end
%%--------------------------------------------------------------------
type_check(Key, Value, #cfg_entry{list_of = false} = Def) ->
    try case type_check_elements([Value], Def#cfg_entry.type, Def, []) of
	    {ok, [NewValue]} ->
		{ok, NewValue}
	end
    catch
	throw: {invalid_value, RStr, 1, V} ->
	    Msg = io_lib:format("parameter '~p' has invalid value (~p) - expected ~p : ~s",
			       [Key, V, Def#cfg_entry.type, RStr]),
	    {error, lists:flatten(Msg)}
    end;
type_check(Key, Values, #cfg_entry{list_of = true} = Def) when is_list(Values) ->
    try case type_check_elements(Values, Def#cfg_entry.type, Def, []) of
	    {ok, NewValues} ->
		{ok, NewValues}
	end
    catch
	throw: {invalid_value, RStr, Num, V} ->
	    Msg = io_lib:format("parameter '~p' has invalid value (#~p in list (~p)) - expected ~p : ~s",
			       [Key, Num, V, Def#cfg_entry.type, RStr]),
	    {error, lists:flatten(Msg)}
    end;
type_check(Key, Value, #cfg_entry{list_of = true} = Def) ->
    %% Value is not a list
    Msg = io_lib:format("parameter '~p' has invalid value (~p) - list of ~p expected",
			[Key, Value, Def#cfg_entry.type]),
    {error, lists:flatten(Msg)}.


%%--------------------------------------------------------------------
%% @spec    (Values, Type, Def, []) ->
%%            {ok, NewValue} |
%%            {error, Msg}
%%
%%            Values = [term()]
%%            Type   = atom           |
%%                     integer        |
%%                     bool           |
%%                     term           |
%%                     string         |
%%                     regexp_rewrite |
%%                     regexp_match   |
%%                     sipurl         |
%%                     sip_sipurl     |
%%                     sips_sipurl    |
%%                     tuple          |
%%                     {tuple, Arity}
%%            Arity  = integer()
%%            Def    = #cfg_entry{}
%%
%% @doc     Check Values to make sure they match Type. The tests and
%%          normalization done is depending on Type and Def.
%% @end
%%--------------------------------------------------------------------

%%
%% atom
%%
type_check_elements([H | T], atom, Def, Res) when is_atom(H) ->
    type_check_elements(T, atom, Def, [H | Res]);

%%
%% integer
%%
type_check_elements([H | T], integer, Def, Res) when is_integer(H) ->
    type_check_elements(T, integer, Def, [H | Res]);

%%
%% bool
%%
type_check_elements([H | T], bool, Def, Res) when is_boolean(H) ->
    type_check_elements(T, bool, Def, [H | Res]);

%%
%% tuple
%%
type_check_elements([H | T], tuple, Def, Res) when is_tuple(H) ->
    type_check_elements(T, tuple, Def, [H | Res]);

%%
%% tuple with arity
%%
type_check_elements([H | T], {tuple, Arity}, Def, Res) when is_integer(Arity), is_tuple(H), size(H) == Arity ->
    type_check_elements(T, {tuple, Arity}, Def, [H | Res]);

%%
%% term
%%
type_check_elements(Terms, term, _Def, []) ->
    %% term matches anything, no need to check every element
    {ok, Terms};

%%
%% string
%%
type_check_elements([H | T], string, Def, Res) when is_list(H), length(H) > 1 ->
    %% strings may not be of length 1 or less, to avoid accidents with value "foo" when
    %% we expect list of string (which would then be the list ["f", "o", "o"])
    case Def#cfg_entry.normalize of
	true ->
	    LC = http_util:to_lower(H),
	    type_check_elements(T, string, Def, [LC | Res]);
	false ->
	    type_check_elements(T, string, Def, [H | Res])
    end;
type_check_elements([H | _T], string, _Def, Res) when is_list(H) ->
    throw({invalid_value, "string must be more than one character", length(Res) + 1, H});

%%
%% regexp_rewrite
%%
type_check_elements([{L, R} | T], regexp_rewrite, Def, Res) ->
    if
	is_list(L), length(L) > 1 ->
	    case regexp:parse(L) of
		{ok, _RE} ->
		    %% XXX we could store the RE if normalization is true, but we don't do that yet
		    if
			is_list(R), length(R) > 1 ->
			    type_check_elements(T, regexp_rewrite, Def, [{L, R} | Res]);
			true ->
			    throw({invalid_value, "RHS not string or too short", length(Res) + 1, {L, R}})
		    end;
		{error, _E} ->
		    throw({invalid_value, "unparsable LHS", length(Res) + 1, {L, R}})
	    end;
	true ->
	    throw({invalid_value, "LHS not string or too short", length(Res) + 1, {L, R}})
    end;

%%
%% regexp_match
%%
type_check_elements([{L, R} | T], regexp_match, Def, Res) when is_list(L) ->
    case regexp:parse(L) of
	{ok, _RE} ->
	    %% XXX we could store the RE if normalization is true, but we don't do that yet
	    type_check_elements(T, regexp_match, Def, [{L, R} | Res]);
	{error, _E} ->
	    throw({invalid_value, "unparsable LHS", length(Res) + 1, {L, R}})
    end;
type_check_elements([{L, R} | _T], regexp_match, _Def, Res) ->
    throw({invalid_value, "non-list LHS", length(Res) + 1, {L, R}});

%%
%% sipurl
%%
type_check_elements([H | T], sipurl, Def, Res) when is_list(H) ->
    case sipurl:parse(H) of
	URL when is_record(URL, sipurl) ->
	    case Def#cfg_entry.normalize of
		true ->
		    type_check_elements(T, sipurl, Def, [URL | Res]);
		false ->
		    %% H was parseable, but we are not to normalize it
		    type_check_elements(T, sipurl, Def, [H | Res])
	    end;
	_ ->
	    throw({invalid_value, "unparsable URL", length(Res) + 1, H})
    end;

%%
%% sip_sipurl
%%
type_check_elements([H | T], sip_sipurl, Def, Res) when is_list(H) ->
    case sipurl:parse_url_with_default_protocol("sip", H) of
	URL when is_record(URL, sipurl) ->
	    case Def#cfg_entry.normalize of
		true ->
		    type_check_elements(T, sip_sipurl, Def, [URL | Res]);
		false ->
		    %% H was parseable, but we are not to normalize it
		    type_check_elements(T, sip_sipurl, Def, [H | Res])
	    end;
	_ ->
	    throw({invalid_value, "unparsable default-sip-URL", length(Res) + 1, H})
    end;

%%
%% sips_sipurl
%%
type_check_elements([H | T], sips_sipurl, Def, Res) when is_list(H) ->
    case sipurl:parse_url_with_default_protocol("sips", H) of
	URL when is_record(URL, sipurl) ->
	    case Def#cfg_entry.normalize of
		true ->
		    type_check_elements(T, sips_sipurl, Def, [URL | Res]);
		false ->
		    %% H was parseable, but we are not to normalize it
		    type_check_elements(T, sips_sipurl, Def, [H | Res])
	    end;
	_ ->
	    throw({invalid_value, "unparsable default-sips-URL", length(Res) + 1, H})
    end;

type_check_elements([H | _T], _Type, _Def, Res) ->
    %% something was wrong
    %% XXX distinguish between unknown Type and value-not-of-Type problem?
    throw({invalid_value, "invalid type", length(Res) + 1, H});

type_check_elements([], _Type, _Def, Res) ->
    %% all finished
    {ok, lists:reverse(Res)}.


%%--------------------------------------------------------------------
%% @spec    (Cfg, Definitions) ->
%%            ok | {error, Msg}
%%
%%            Cfg         = #yxa_cfg{}
%%            Definitions = [#cfg_entry{}]
%%
%%            Msg = string()
%%
%% @doc     Check that all required entrys in Definitions is present
%%          in Cfg.
%% @end
%%--------------------------------------------------------------------
check_required(Cfg, Definitions) when is_record(Cfg, yxa_cfg), is_list(Definitions) ->
    check_required2(Definitions, Cfg#yxa_cfg.entrys).

check_required2([#cfg_entry{required = true, key = Key} | T], Entrys) ->
    case lists:keysearch(Key, 1, Entrys) of
	{value, {Key, [], _Src}} ->
	    Msg = io_lib:format("Required parameter '~p' may not have empty value", [Key]),
            {error, lists:flatten(Msg)};
	{value, {Key, _Value, _Src}} ->
	    check_required2(T, Entrys);
	false ->
	    Msg = io_lib:format("Required parameter '~p' not set", [Key]),
	    {error, lists:flatten(Msg)}
    end;
check_required2([H | T], Entrys) when is_record(H, cfg_entry) ->
    %% non-required parameter
    check_required2(T, Entrys);
check_required2([], _Entrys) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (Cfg) ->
%%            ok | {error, Msg}
%%
%%            Cfg = #yxa_cfg{}
%%
%%            Msg = string()
%%
%% @doc     Check parameters that depend on other parameters.
%% @end
%%--------------------------------------------------------------------
check_config_dependencys(Cfg) when is_record(Cfg, yxa_cfg) ->
    %% XXX check dependant configuration parameters too!
    %%
    %% sipuserdb_file_filename is required if userdb_modules contains sipuserdb_file
    %%
    %% ssl_server_ssloptions should not contain {certfile, File} if ssl_server_certfile is set
    %%
    %% ssl_client_ssloptions should not contain {certfile, File} if ssl_client_certfile is set
    %%
    %% check that sipuserdb_mysql_{host, user, password, database} is set if userdb_modules
    %% contains sipuserdb_mysql
    %%
    %% check that record_route_url is a SIP/SIPS URL and (at least) warn if it does not have
    %% 'lr' parameter
    %%
    ok.

%%--------------------------------------------------------------------
%% @spec    (AppModule, Cfg) ->
%%            ok | {error, Msg}
%%
%%            AppModule = atom() "YXA application module"
%%            Cfg       = #yxa_cfg{}
%%
%%            Msg = string()
%%
%% @doc     Check parameters that are application specific.
%% @end
%%--------------------------------------------------------------------

check_application_specific(AppModule, Cfg) when is_atom(AppModule), is_record(Cfg, yxa_cfg) ->
    check_application_specific2(AppModule, Cfg#yxa_cfg.entrys).

%% appserver requires loop detection, since it forks. draft-ietf-sip-fork-loop-fix-01.
check_application_specific2(appserver, [{detect_loops, true, _Source} | T]) ->
    check_application_specific2(appserver, T);
check_application_specific2(appserver, [{detect_loops, false, _Source} | _T]) ->
    {error, "Appserver must have detect_loops set to 'true'"};
check_application_specific2(appserver, [{_Key, _Value, _Source} | T]) ->
    check_application_specific2(appserver, T);

check_application_specific2(_AppModule, _L) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (Cfg, Definitions, Mode) ->
%%            ok | {error, Msg}
%%
%%            Cfg         = #yxa_cfg{}
%%            Definitions = [#cfg_entry{}]
%%            Mode        = soft | hard
%%
%%            Msg = string()
%%
%% @doc     Check if it is possible to do a reload of type Mode of the
%%          configuration parameters in Cfg.
%% @end
%%--------------------------------------------------------------------
check_loadable(Cfg, Definitions, soft) when is_record(Cfg, yxa_cfg), is_list(Definitions) ->
    check_loadable_soft(Cfg#yxa_cfg.entrys, Definitions);
check_loadable(Cfg, Definitions, hard) when is_record(Cfg, yxa_cfg), is_list(Definitions) ->
    ok.

%% part of check_loadable/3, Returns  : ok | {error, Msg}
check_loadable_soft([{Key, Value, Src} | T], Definitions) ->
    case get_definition(Key, Definitions) of
        {ok, #cfg_entry{soft_reload = false}} ->
	    Current = try yxa_config:get_env(Key) of
			  {ok, LRes} -> LRes;
			  none -> undefined
		      catch
			  error: _ ->
			      %% to facilitate testing
			      undefined
		      end,
	    case Value of
		Current ->
		    %% no change
		    check_loadable_soft(T, Definitions);
		_ ->
		    Msg = io_lib:format("Parameter '~p' can't be changed with a soft reconfiguration, "
					"you must restart the whole application - sorry.~n"
					"Source              : ~p~n"
					"Current value       : ~p~n"
					"Requested change to : ~p~n",
					[Key, Src, Current, Value]),
		    {error, lists:flatten(Msg)}
	    end;
	{ok, #cfg_entry{soft_reload = true}} ->
	    check_loadable_soft(T, Definitions);
	nomatch ->
	    case check_loadable_soft_no_definition(Key, Value) of
		ok ->
		    check_loadable_soft(T, Definitions);
		{error, Msg} when is_list(Msg) ->
		    {error, Msg}
	    end
    end;
check_loadable_soft([], _Definitions) ->
    ok.

%% Part of check_loadable_soft/2, Return  : ok | {error, Msg}
check_loadable_soft_no_definition(Key, Value) when is_atom(Key) ->
    case atom_to_list(Key) of
	"local_" ++ _ ->
	    {ok, Current} = try yxa_config:get_env(Key) of
				LRes -> LRes
			    catch
				error: _ ->
				    %% to facilitate testing
				    {ok, undefined}
			    end,
	    case Key of
		Current ->
		    %% no change
		    ok;
		_ ->
		    case local:config_is_soft_reloadable(Key, Value) of
			true ->
			    ok;
			false ->
			    Msg = io_lib:format("Local parameter '~p' can't be changed with a soft "
						"reconfiguration, you must restart the whole application - "
						"sorry.~nCurrent value : ~p~nRequested change to : ~p~n",
						[Key, Current, Value]),
			    {error, lists:flatten(Msg)}
		    end
	    end;
	_ ->
	    %% we should have already checked that all non-local (local_*) parameters have
	    %% a definition, so we should never end up here
	    throw({error, "Config check: definition missing in check_loadable_soft, should not happen!"})
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

    %% type_check_elements(Values, Type, Def, [])
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "type_check_elements/4 - 0"),

    ok = test_type_check_atom(),
    ok = test_type_check_integer(),
    ok = test_type_check_bool(),
    ok = test_type_check_term(),
    ok = test_type_check_string(),
    ok = test_type_check_regexp_rewrite(),
    ok = test_type_check_regexp_match(),
    ok = test_type_check_sipurl(),
    ok = test_type_check_sip_sipurl(),
    ok = test_type_check_sips_sipurl(),


    %% check_cfg_entry_type(Key, Value, Src, Def)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "check_cfg_entry_type/4 - 1"),
    %% check list of atom
    {ok, [true, false]} =
	check_cfg_entry_type(test, [true, false], test_backend,
			     #cfg_entry{list_of = true,
					type	= atom
				       }
			    ),

    autotest:mark(?LINE, "check_cfg_entry_type/4 - 2"),
    %% check list of atom when single value was expected
    {error, "parameter 'test' has invalid value ([true,false]) - expected atom : invalid type"} =
	check_cfg_entry_type(test, [true, false], test_backend,
			     #cfg_entry{list_of = false,
					type	= atom
				       }
			    ),

    autotest:mark(?LINE, "check_cfg_entry_type/4 - 3"),
    %% check single atom (normalize should be a no-op)
    {ok, true} =
	check_cfg_entry_type(test, true, test_backend,
			     #cfg_entry{normalize = true,
					type	= atom
				       }
			    ),

    autotest:mark(?LINE, "check_cfg_entry_type/4 - 4"),
    %% check single atom when list was expected
    {error, "parameter 'test' has invalid value (true) - list of atom expected"} =
	check_cfg_entry_type(test, true, test_backend,
			     #cfg_entry{list_of = true,
					type	= atom
				       }
			    ),

    autotest:mark(?LINE, "check_cfg_entry_type/4 - 5"),
    %% make sure we don't accept a string when we should get an integer (a bit tricky)
    {error,"parameter 'test' has invalid value (\"string\") - expected integer : invalid type"} =
	check_cfg_entry_type(test, "string", test_backend,
                             #cfg_entry{list_of = false,
                                        type    = integer
                                       }
                            ),

    %% get_cfg_definitions(AppModule)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_cfg_definitions/1 - 1"),
    %% test to make sure we get at least 25 configuration entrys back for
    %% our known applications. The tuple-tagging is to make test fail output
    %% indicate which one it was that failed.
    {incomingproxy, true}	= {incomingproxy, length(get_cfg_definitions(incomingproxy)) >= 25},
    {pstnproxy, true}		= {pstnproxy, length(get_cfg_definitions(pstnproxy)) >= 25},
    {appserver, true}		= {appserver, length(get_cfg_definitions(appserver)) >= 25},
    {outgoingproxy, true}	= {outgoingproxy, length(get_cfg_definitions(outgoingproxy)) >= 25},

    autotest:mark(?LINE, "get_cfg_definitions/1 - 2"),
    %% test that unknown application name also gets config (common config)
    {test, true}		= {test, length(get_cfg_definitions(false)) >= 20},

    autotest:mark(?LINE, "get_cfg_definitions/1 - 3"),
    %% test that unknown application does NOT result in the same thing as known application
    GCD_Unknown_Length = length(get_cfg_definitions(false)),
    GCD_Known_Length = length(get_cfg_definitions(incomingproxy)),
    case (GCD_Unknown_Length < GCD_Known_Length) of
	true -> ok;
	false -> throw("Unknown app does not get less definitions than known one!")
    end,


    %% merge_cfg_entrys(Entrys, In)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "merge_cfg_entrys/2 - 1"),
    %% test no conflicts during merge
    [#cfg_entry{key = aaa},
     #cfg_entry{key = abb},
     #cfg_entry{key = abc}] =
	merge_cfg_entrys([#cfg_entry{key = abb}],
			 [#cfg_entry{key = abc},
			  #cfg_entry{key = aaa}
			 ]),

    autotest:mark(?LINE, "merge_cfg_entrys/2 - 2"),
    %% test conflicts
    [#cfg_entry{key = aaa, default = 2},
     #cfg_entry{key = abc}
    ] = merge_cfg_entrys([#cfg_entry{key = aaa, default = 1}],
			 [#cfg_entry{key = abc},
			  #cfg_entry{key = aaa, default = 2}]),


    %% check_types(Cfg, Definitions)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_types/2 - 1"),
    %% test complete set of definitions and no bad parameters
    CT_Def1 =
	[#cfg_entry{key = abc,
		    list_of = true,
		    type = integer
		   },
	 #cfg_entry{key = def,
		    list_of = false,
		    type = string,
		    normalize = true
		   },
	 #cfg_entry{key = gih,
		    list_of = false,
		    type = sipurl,
		    normalize = false
		   }],
    CT_Cfg1 = #yxa_cfg{entrys = [{abc, [9, 8, 7], test},
				 {def, "LowerCASEme", test},
				 {gih, "sip:dontparse.example.org", test}
				]
		      },
    CT_Res1 = CT_Cfg1#yxa_cfg{entrys = [{abc, [9, 8, 7], test},
					{def, "lowercaseme", test},
					{gih, "sip:dontparse.example.org", test}
				       ]},
    {ok, CT_Res1} = check_types(CT_Cfg1, CT_Def1),

    autotest:mark(?LINE, "check_types/2 - 2"),
    %% test with missing definition
    CT_Def2 = CT_Def1,
    CT_Cfg2 = CT_Cfg1#yxa_cfg{entrys = CT_Cfg1#yxa_cfg.entrys ++ [{jkl, "unknown-i-am", test}]},
    {error, "Unknown configuration parameter jkl (source: test)"} = check_types(CT_Cfg2, CT_Def2),

    autotest:mark(?LINE, "check_types/2 - 3"),
    %% test bad parameter type
    CT_Def3 = [#cfg_entry{key = abc,
			  list_of = false,
			  type = bool
			 }],
    CT_Cfg3 = #yxa_cfg{entrys = [{abc, 27, test}]
		      },
    {error, "parameter 'abc' has invalid value (27) - expected bool : invalid type"} =
	check_types(CT_Cfg3, CT_Def3),


    autotest:mark(?LINE, "check_types/2 - 4"),
    %% test list with one bad parameter type
    CT_Def4 = [#cfg_entry{key = abc,
			  list_of = true,
			  type = bool
			 }],
    CT_Cfg4 = #yxa_cfg{entrys = [{abc, [false, 27], test}]
		      },
    {error,"parameter 'abc' has invalid value (#2 in list (27)) - expected bool : invalid type"} =
	check_types(CT_Cfg4, CT_Def4),


    %% check_required(Cfg, Definitions)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_required/2 - 1"),
    %% test everything-ok
    CR_Cfg1 = #yxa_cfg{entrys = [{abc, 123, test},
				 {req, "hi world", test}
				]
		      },
    CR_Def1 = [#cfg_entry{key = abc, type = integer},
	       #cfg_entry{key = req, type = string, required = true}
	      ],

    ok = check_required(CR_Cfg1, CR_Def1),

    autotest:mark(?LINE, "check_required/2 - 2"),
    %% test missing required parameter
    CR_Cfg2 = #yxa_cfg{entrys = [{abc, 123, test}]
		      },
    CR_Def2 = [#cfg_entry{key = abc, type = integer},
	       #cfg_entry{key = req, type = string, required = true}
	      ],

    {error, "Required parameter 'req' not set"} = check_required(CR_Cfg2, CR_Def2),

    autotest:mark(?LINE, "check_required/2 - 3"),
    %% test required parameter set to empty list
    CR_Cfg3 = #yxa_cfg{entrys = [{req, "", test}]
		      },
    CR_Def3 = [#cfg_entry{key = req, type = string, required = true}
	      ],

    {error, "Required parameter 'req' may not have empty value"} = check_required(CR_Cfg3, CR_Def3),


    %% check_loadable(Cfg, Definitions, Mode)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_loadable/3 - 1"),
    %% test hard reload, should _always_ return ok

    CL_Cfg1 = #yxa_cfg{entrys = [{test_hard, 123, test},
				 {test_abc, "foo", test}]
		      },
    CL_Def1 = [#cfg_entry{key = test_hard, type = integer, soft_reload = false},
	       #cfg_entry{key = test_abc, type = string}
	      ],

    ok = check_loadable(CL_Cfg1, CL_Def1, hard),

    autotest:mark(?LINE, "check_loadable/3 - 2"),
    %% test soft reload that should be permitted

    %% use make_ref to make sure the value changes
    CL_Cfg2 = #yxa_cfg{entrys = [{test_soft, make_ref(), test},
				 {test_abc, "foo", test}]
		      },
    CL_Def2 = [#cfg_entry{key = test_soft, type = integer, soft_reload = true},
	       #cfg_entry{key = test_abc, type = string}
	      ],

    ok = check_loadable(CL_Cfg2, CL_Def2, soft),


    autotest:mark(?LINE, "check_loadable/3 - 3"),
    %% test soft reload that should NOT be permitted

    %% use make_ref to make sure the value changes
    CL_Cfg3 = #yxa_cfg{entrys = [{test_hard, 4711, test},
				 {test_abc, "foo", test}]
		      },
    CL_Def3 = [#cfg_entry{key = test_hard, type = integer, soft_reload = false},
	       #cfg_entry{key = test_abc, type = string}
	      ],

    {error, "Parameter 'test_hard' can't be changed with a soft reconfiguration" ++ _} =
	check_loadable(CL_Cfg3, CL_Def3, soft),

    autotest:mark(?LINE, "check_loadable/3 - 4"),
    %% test missing definition
    CL_Cfg4 = #yxa_cfg{entrys = [{test_hard, 4711, test}]
		      },
    try check_loadable(CL_Cfg4, [], soft) of
	_ -> throw(test_failed)
    catch
	throw: {error, "Config check: definition missing in check_loadable_soft, should not happen!"} ->
	    ok
    end,

    %% check_application_specific(AppModule, Cfg)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_application_specific/2 - 1"),
    CAS_Cfg1 = #yxa_cfg{entrys = [{1, true, test},
				  {detect_loops, true, test},
				  {2, true, test}
				 ]
		       },
    ok = check_application_specific(appserver, CAS_Cfg1),

    CAS_Cfg2 = #yxa_cfg{entrys = [{1, true, test},
				  {detect_loops, false, test}
				 ]
		       },
    {error, "Appserver must have" ++ _} = check_application_specific(appserver, CAS_Cfg2),

    ok.


test_type_check_atom() ->
    autotest:mark(?LINE, "type_check_elements/4 - atom 1"),
    %% check list of atoms
    {ok, [true, false, nomatch]} = type_check_elements([true, false, nomatch],
						       atom,
						       #cfg_entry{list_of = true},
						       []),


    autotest:mark(?LINE, "type_check_elements/4 - atom 2"),
    %% check single atom
    {ok, [true]} = type_check_elements([true], atom,
				       #cfg_entry{}, []),

    autotest:mark(?LINE, "type_check_elements/4 - atom 3"),
    %% check integer among atoms
    try type_check_elements([true, "non-atom-value", false], atom,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "invalid type", 2, "non-atom-value"} ->
	    ok
    end,

    ok.

test_type_check_integer() ->
    autotest:mark(?LINE, "type_check_elements/4 - integer 1"),
    %% check integer
    {ok, [3141592]} = type_check_elements([3141592], integer,
					#cfg_entry{}, []),



    autotest:mark(?LINE, "type_check_elements/4 - integer 2"),
    %% check non-integer
    try type_check_elements([9, {foo, 98}], integer,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "invalid type", 2, {foo, 98}} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - integer 3"),
    %% strings are list of integers, and we can't avoid 'accepting' a string
    %% as a list of integers here. We can (and do) in check_cfg_entry_type/4 though.
    {ok, [115, 116, 114, 105, 110, 103]} = type_check_elements("string", integer,
							       #cfg_entry{}, []),

    ok.

test_type_check_bool() ->
    autotest:mark(?LINE, "type_check_elements/4 - bool 1"),
    %% check bools
    {ok, [true, false]} = type_check_elements([true, false], bool,
					      #cfg_entry{}, []),

    autotest:mark(?LINE, "type_check_elements/4 - bool 2"),
    %% check non-bool
    try type_check_elements([error], bool,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "invalid type", 1, error} ->
	    ok
    end,
    ok.

test_type_check_term() ->
    autotest:mark(?LINE, "type_check_elements/4 - term 1"),
    %% check all kinds of weird things
    In = [self(), make_ref(), {1, false}],

    {ok, In} = type_check_elements(In, term, #cfg_entry{}, []),

    %% we can't make check-type-term fail - everything is a term in Erlang

    ok.

test_type_check_string() ->
    autotest:mark(?LINE, "type_check_elements/4 - string 1"),
    %% check single string
    {ok, ["test_string"]} = type_check_elements(["test_string"], string,
						#cfg_entry{}, []),

    autotest:mark(?LINE, "type_check_elements/4 - string 2"),
    %% check list of strings
    {ok, ["test", "string"]} = type_check_elements(["test", "string"], string,
						#cfg_entry{}, []),


    autotest:mark(?LINE, "type_check_elements/4 - string 3"),
    %% check too short string
    try type_check_elements(["why", "am", "i", "so", "short?"], string,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "string must be more than one character", 3, "i"} -> ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - string 4"),
    %% check accidental integer
    try type_check_elements([64], string,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "invalid type", 1, 64} -> ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - string 5"),
    %% check normalization
    {ok, ["foo"]} = type_check_elements(["FoO"], string,
					#cfg_entry{normalize = true}, []),


    autotest:mark(?LINE, "type_check_elements/4 - string 6"),
    %% check without normalization
    {ok, ["FoO"]} = type_check_elements(["FoO"], string,
					#cfg_entry{normalize = false}, []),

    ok.

test_type_check_regexp_rewrite() ->
    autotest:mark(?LINE, "type_check_elements/4 - regexp_rewrite 1"),
    %% check single regexp
    {ok, [{"...", "foo"}]} = type_check_elements([{"...", "foo"}], regexp_rewrite,
						  #cfg_entry{}, []),

    autotest:mark(?LINE, "type_check_elements/4 - regexp_rewrite 2"),
    %% check list of regexps, the last one invalid because it is too short
    try type_check_elements([{"...", "foo"}, {".*", "bar"}, {".", "X"}], regexp_rewrite,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "LHS not string or too short", 3, {".", "X"}} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - regexp_rewrite 3"),
    %% check list of regexps, the last one invalid because it has atom LHS
    try type_check_elements([{"...", "foo"}, {false, "X"}], regexp_rewrite,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "LHS not string or too short", 2, {false, "X"}} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - regexp_rewrite 4"),
    %% check invalid regexp with illegal LHS
    try type_check_elements([{"+test", "foo"}], regexp_rewrite,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "unparsable LHS", 1, {"+test", "foo"}} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - regexp_rewrite 5"),
    %% check invalid RHS
    try type_check_elements([{".*", "x"}], regexp_rewrite,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "RHS not string or too short", 1, {".*", "x"}} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - regexp_rewrite 6"),
    %% check invalid RHS
    try type_check_elements([{".*", false}], regexp_rewrite,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "RHS not string or too short", 1, {".*", false}} ->
	    ok
    end,

    ok.


test_type_check_regexp_match() ->
    autotest:mark(?LINE, "type_check_elements/4 - regexp_match 1"),
    %% check single regexp
    {ok, [{"...", foo}]} = type_check_elements([{"...", foo}], regexp_match,
						  #cfg_entry{}, []),

    autotest:mark(?LINE, "type_check_elements/4 - regexp_match 2"),
    %% check list of regexps, the last one invalid because it has atom LHS
    try type_check_elements([{"...", foo}, {false, true}], regexp_match,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "non-list LHS", 2, {false, true}} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - regexp_match 3"),
    %% check invalid regexp with illegal LHS
    try type_check_elements([{"+test", foo}], regexp_match,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "unparsable LHS", 1, {"+test", foo}} ->
	    ok
    end,

    ok.

test_type_check_sipurl() ->
    autotest:mark(?LINE, "type_check_elements/4 - sipurl 0"),
    TCE_URL_s1 = "sip:ft@example.com:5555",
    TCE_URL_s2 = "sip:example.com:4321",
    TCE_URL1 = sipurl:parse(TCE_URL_s1),
    TCE_URL2 = sipurl:parse(TCE_URL_s2),

    autotest:mark(?LINE, "type_check_elements/4 - sipurl 1"),
    %% check valid URLs with normalization
    {ok, [TCE_URL1, TCE_URL2]} = type_check_elements([TCE_URL_s1, TCE_URL_s2], sipurl,
						     #cfg_entry{normalize = true}, []),

    autotest:mark(?LINE, "type_check_elements/4 - sipurl 2"),
    %% check valid URLs without normalization
    {ok, [TCE_URL_s1, TCE_URL_s2]} = type_check_elements([TCE_URL_s1, TCE_URL_s2], sipurl,
							 #cfg_entry{normalize = false}, []),

    autotest:mark(?LINE, "type_check_elements/4 - sipurl 3"),
    %% check one valid and one invalid URL
    try type_check_elements([TCE_URL_s1, "not an URL"], sipurl,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "unparsable URL", 2, "not an URL"} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - sipurl 4"),
    %% check invalid type
    try type_check_elements([17], sipurl,
			    #cfg_entry{normalize = true}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "invalid type", 1, 17} ->
	    ok
    end,

    ok.


test_type_check_sip_sipurl() ->
    autotest:mark(?LINE, "type_check_elements/4 - sip_sipurl 0"),
    TCE_SIPURL_s1 = "ft@example.com:5555",
    TCE_SIPURL_s2 = "example.com:4321",
    TCE_SIPURL_s3 = "sips:secure.example.com",
    TCE_SIPURL1 = sipurl:parse("sip:" ++ TCE_SIPURL_s1),
    TCE_SIPURL2 = sipurl:parse("sip:" ++ TCE_SIPURL_s2),
    TCE_SIPURL3 = sipurl:parse(TCE_SIPURL_s3),

    autotest:mark(?LINE, "type_check_elements/4 - sip_sipurl 1"),
    %% check valid URLs with normalization
    {ok, [TCE_SIPURL1, TCE_SIPURL2]} = type_check_elements([TCE_SIPURL_s1, TCE_SIPURL_s2], sip_sipurl,
						     #cfg_entry{normalize = true}, []),

    autotest:mark(?LINE, "type_check_elements/4 - sip_sipurl 2"),
    %% check valid URLs without normalization
    {ok, [TCE_SIPURL_s1, TCE_SIPURL_s2]} = type_check_elements([TCE_SIPURL_s1, TCE_SIPURL_s2], sip_sipurl,
							 #cfg_entry{normalize = false}, []),


    autotest:mark(?LINE, "type_check_elements/4 - sip_sipurl 3"),
    %% check valid URL with protocol specified and with normalization
    {ok, [TCE_SIPURL3]} = type_check_elements([TCE_SIPURL_s3], sip_sipurl,
					     #cfg_entry{normalize = true}, []),

    autotest:mark(?LINE, "type_check_elements/4 - sip_sipurl 4"),
    %% check one valid and one invalid URL
    try type_check_elements([TCE_SIPURL_s1, "not an URL"], sip_sipurl,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "unparsable default-sip-URL", 2, "not an URL"} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - sip_sipurl 5"),
    %% check invalid type
    try type_check_elements([17], sip_sipurl,
			    #cfg_entry{normalize = true}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "invalid type", 1, 17} ->
	    ok
    end,

    ok.


test_type_check_sips_sipurl() ->
    autotest:mark(?LINE, "type_check_elements/4 - sips_sipurl 0"),
    TCE_SIPSURL_s1 = "ft@example.com:5555",
    TCE_SIPSURL_s2 = "example.com:4321",
    TCE_SIPSURL1 = sipurl:parse("sips:" ++ TCE_SIPSURL_s1),
    TCE_SIPSURL2 = sipurl:parse("sips:" ++ TCE_SIPSURL_s2),

    autotest:mark(?LINE, "type_check_elements/4 - sips_sipurl 1"),
    %% check valid URLs with normalization
    {ok, [TCE_SIPSURL1, TCE_SIPSURL2]} = type_check_elements([TCE_SIPSURL_s1, TCE_SIPSURL_s2], sips_sipurl,
						     #cfg_entry{normalize = true}, []),

    autotest:mark(?LINE, "type_check_elements/4 - sips_sipurl 2"),
    %% check valid URLs without normalization
    {ok, [TCE_SIPSURL_s1, TCE_SIPSURL_s2]} = type_check_elements([TCE_SIPSURL_s1, TCE_SIPSURL_s2], sips_sipurl,
							 #cfg_entry{normalize = false}, []),

    autotest:mark(?LINE, "type_check_elements/4 - sips_sipurl 3"),
    %% check one valid and one invalid URL
    try type_check_elements([TCE_SIPSURL_s1, "not an URL"], sips_sipurl,
			    #cfg_entry{}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "unparsable default-sips-URL", 2, "not an URL"} ->
	    ok
    end,

    autotest:mark(?LINE, "type_check_elements/4 - sips_sipurl 4"),
    %% check invalid type
    try type_check_elements([17], sips_sipurl,
			    #cfg_entry{normalize = true}, []) of
	_ -> throw(test_failed)
    catch
	throw: {invalid_value, "invalid type", 1, 17} ->
	    ok
    end,

    ok.
