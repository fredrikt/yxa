%% This module handles parameters supplied in sip urls. They  must
%% be unique - i.e. the same key can only occure once.
%%
%% Note: keys and values are currently stored as strings but pattern
%% matching and list:keysearch will be faster if standard values are
%% represented as atoms (but don't turn them all into atoms - as atoms
%% aren't GCed)
%%--------------------------------------------------------------------

-module(url_param).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 %% create url_param
	 to_norm/1,
	 %% form url_param to other format
	 to_list/1,
	 to_string_list/1,
	 to_string/1,
	 %% modify
	 add/2,
	 add/3,
	 %% lookup
	 find/2,
	 %% remove
	 remove/2,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: to_norm(Params)
%%           Params = list() of string(), each string is a "name=val"
%%                    pair or a single "name" value and may contain
%%                    %HH hex escape codes - they are treated as case
%%                    insensitive
%% Descrip.: convert a uri-parameter list to a normalized (a non case
%%           sensitive form) form
%% Returns : url_param record() |
%%           throw({error, duplicate_key}) if a "name"
%%           component in Params is duplicated
%% Note    : URL parameters may not contain quotes, and the = is
%%           literal in the RFC3261 BNF, not EQUAL (which may be
%%           surrounded with linear whitespace)
%%--------------------------------------------------------------------
to_norm(Params) when is_list(Params) ->
    F = fun(E) ->
		case string:tokens(E, "=") of
		    [Name, Val] ->
			{ sipurl:unescape_str(httpd_util:to_lower(Name)),
			  sipurl:unescape_str(httpd_util:to_lower(Val))
			 };
		    [Name] ->
			{ sipurl:unescape_str(httpd_util:to_lower(Name)), none}
		end
	end,
    L = [F(E) || E <- Params],
    #url_param{pairs = key_val_db:new(L)}.

%%--------------------------------------------------------------------
%% Function: to_list(Norm)
%%           Norm = url_param record()
%% Descrip.: returns a normalized form of the parameters
%% Returns : list() of {Key,Val}
%%           Key = string()
%%           Val = string() | none (if a "name" paramter rather than
%%           "name=val")
%%--------------------------------------------------------------------
to_list(Norm) when is_record(Norm, url_param) ->
        key_val_db:to_key_val(Norm#url_param.pairs).


%%--------------------------------------------------------------------
%% Function: to_string_list(Norm)
%%           Norm = url_param record()
%% Descrip.: return parameter data in the same format as input to
%%           to_norm/1
%% Returns : list() of string(), the strings are either "name=val" or
%%           "name"
%%--------------------------------------------------------------------
to_string_list(Norm) when is_record(Norm, url_param) ->
    F = fun(E) ->
		case E of
		    %% XXX compatibility hack - this is not correct according to the specification
		    {"lr", none} ->
			"lr=true";
		    {Name, none} ->
			EscName = sipurl:escape_parameters(Name),
			lists:flatten(io_lib:format("~s",[EscName]));
		    {Name, Val} ->
			EscName = sipurl:escape_parameters(Name),
			EscVal = sipurl:escape_parameters(Val),
			lists:flatten(io_lib:format("~s=~s",[EscName, EscVal]))
		end
	end,
    lists:map(F, to_list(Norm)).

%%--------------------------------------------------------------------
%% Function: to_string(Norm)
%%           Norm = url_param record()
%% Descrip.: return a raw uri-parameter string
%% Returns : string(), on the ";name=val;..." format
%%--------------------------------------------------------------------
to_string(Norm) when is_record(Norm, url_param) ->
    L = to_string_list(Norm),
    ParamStrList = [[$; | Param] || Param <- L],
    lists:append(ParamStrList).


%%--------------------------------------------------------------------
%% Function: add(UrlParam, Key, Value)
%%           add(UrlParam, Key)
%%           UrlParam = url_param record(), the record to update
%%           Key      = string(), treated as case insensitive
%%           Value    = string(), treated as case insensitive
%% Descrip.: add new entry or replace old entry in url_param. Key and
%%           Value are stored in a case insensitive manner
%% Returns : url_param record()
%%--------------------------------------------------------------------
add(UrlParam, Key) ->
    NKey = httpd_util:to_lower(Key),
    add2(UrlParam, {NKey, none}).

add(UrlParam, Key, Value) when is_record(UrlParam, url_param), is_list(Key) ->
    NKey = httpd_util:to_lower(Key),
    NValue = httpd_util:to_lower(Value),
    add2(UrlParam, {NKey, NValue}).


add2(UrlParam, {Key, Value}) when is_record(UrlParam, url_param), is_list(Key) ->
    DB = UrlParam#url_param.pairs,
    NewDB = key_val_db:add(DB, Key, Value),
    #url_param{ pairs = NewDB }.

%--------------------------------------------------------------------
%% Function: find(Param, Key)
%%           Param = url_param record(), the record to update
%%           Key   = string(), is treated as case insensitive
%% Descrip.: retrive the value of Key if it is contained in Param
%% Returns : [string()] | []
%%--------------------------------------------------------------------
find(Param, Key) when is_record(Param, url_param), is_list(Key) ->
    Data = Param#url_param.pairs,
    CKey = httpd_util:to_lower(Key),
    key_val_db:find(Data, CKey).

%%--------------------------------------------------------------------
%% Function: remove(Param, Key)
%%           Param = url_param record()
%%           Key   = string(), is treated as case insensitive
%% Descrip.: find the Key-Val pair to remove from Param
%% Returns : url_param record
%%--------------------------------------------------------------------
remove(Param, Key) when is_record(Param, url_param), is_list(Key) ->
    Data = Param#url_param.pairs,
    CKey = httpd_util:to_lower(Key),
    Res = key_val_db:rm(Data, CKey),
    Param#url_param{pairs = Res}.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test/0
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->
    %% test to_norm
    %%---------------------------------------------------------------
    %% test regular case
    autotest:mark(?LINE, "to_norm/1 - 1"),
    NDB1 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a", "43"}]),
    #url_param{pairs = NDB1 } = to_norm(["foo=bar", "bar=42", "a=43"]),

    %% test empty param list
    autotest:mark(?LINE, "to_norm/1 - 2"),
    NDB2 = key_val_db:new([]),
    #url_param{pairs = NDB2} = to_norm([]),

    %% test that duplicate names are detected
    autotest:mark(?LINE, "to_norm/1 - 3"),
    case catch to_norm(["foo=bar", "bar=42", "foo=43"]) of
	{error, _} -> ok;
	_ -> throw({error, test_failed})
    end,
    %% test that duplicate names in different case are detected
    autotest:mark(?LINE, "to_norm/1 - 4"),
    case catch to_norm(["foo=bar", "bar=42", "FOO=43"]) of
	{error, _} -> ok;
	_ -> throw({error, test_failed})
    end,
    %% test params that only consist of a name part
    autotest:mark(?LINE, "to_norm/1 - 5"),
    NDB3 = key_val_db:new([{"foo","bar"}, {"bar", none}, {"a", "43"}]),
    #url_param{pairs = NDB3 } = to_norm(["foo=bar", "bar", "a=43"]),

    %% test handling of hex encoding (with both upper and lower case hex values)
    autotest:mark(?LINE, "to_norm/1 - 6"),
    NDB4 = key_val_db:new([{"foo=","bar"}, {"=bar", none}, {"a", "43"}]),
    #url_param{pairs = NDB4 } = to_norm(["foo%3d=bar", "%3Dbar", "a=43"]),

    %% test to_string_list
    %%---------------------------------------------------------------
    %% test that case and missing value part are handled properly
    autotest:mark(?LINE, "to_string_list/1 - 1"),
    Urlparam1 = to_norm(["foo=bAr", "BaR", "lr", "a=43"]),
    ["foo=bar", "bar", "lr=true", "a=43"] = to_string_list(Urlparam1),
    %% test empty param
    autotest:mark(?LINE, "to_string_list/1 - 2"),
    Urlparam2 = to_norm([]),
    [] = to_string_list(Urlparam2),
    %% test that hex encoding is used
    autotest:mark(?LINE, "to_string_list/1 - 3"),
    Urlparam3 = to_norm(["foo%3d=bAr", "Ba%3DR", "lr", "a=43"]),
    ["foo%3D=bar", "ba%3Dr", "lr=true", "a=43"] = to_string_list(Urlparam3),


    %% test to_string
    %%---------------------------------------------------------------
    %% test that case and missing value part are handled properly
    autotest:mark(?LINE, "to_string/1 - 1"),
    ";foo=bar;bar;lr=true;a=43" = to_string(Urlparam1),
    %% test empty param
    autotest:mark(?LINE, "to_string/1 - 2"),
    "" = to_string(Urlparam2),

    %% test to_list
    %%---------------------------------------------------------------
    %% regular case
    autotest:mark(?LINE, "to_list/1 - 1"),
    [{"foo","bar"}, {"bar","42"}, {"a", "43"}] = to_list(to_norm(["foo=bar", "bar=42", "a=43"])),
    %% empty list
    autotest:mark(?LINE, "to_list/1 - 2"),
    [] = to_list(to_norm([])),
    %% test case handling
    autotest:mark(?LINE, "to_list/1 - 3"),
    [{"foo","bar"}, {"bar",none}, {"a", "43"}] = to_list(to_norm(["foo=bAr", "bar", "A=43"])),


    %% test add
    %%---------------------------------------------------------------
    %% add Key-Val to empty url_param
    autotest:mark(?LINE, "add/2 - 1"),
    UrlParam1 = to_norm([]),
    ADB1 = key_val_db:new([{"foo","bar"}]),
    #url_param{pairs = ADB1 } = add(UrlParam1, "foo", "bar"),

    %% add Key _only_ to empty url_param
    autotest:mark(?LINE, "add/2 - 2"),
    ADB2 = key_val_db:new([{"foo", none}]),
    #url_param{pairs = ADB2 } = add(UrlParam1, "foo"),


    UrlParam2 = to_norm(["foo=bar", "bar=42", "a=43"]),

    %% add a new key-val
    autotest:mark(?LINE, "add/2 - 5"),
    ADB5 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a", "43"}, {"gazong", "zog"}]),
    #url_param{pairs = ADB5 } = add(UrlParam2, "gazong", "zog"),

    %% add a new key
    autotest:mark(?LINE, "add/2 - 6"),
    ADB6 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a", "43"}, {"gazong", none}]),
    #url_param{pairs = ADB6} = add(UrlParam2, "gazong"),

    ok.
