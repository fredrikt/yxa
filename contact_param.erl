%% This module handles contact-params for a single contact header
%% entry (sip/sips uri).
%%
%% Note: keys and values are currently stored as strings but pattern
%% matching and list:keysearch will be faster if standard values are
%% represented as atoms (but dont turn them all into atoms - as atoms
%% aren't GCed)
%%--------------------------------------------------------------------

-module(contact_param).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 %% create contact_param
	 to_norm/1,
	 %% form url_param to other format
	 to_list/1,
	 to_string/1,
	 %% modify
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
%%           Params = list() of {Name, Val}
%%           Name = string(), treated as case insensitive
%%           Val  = string() | none, treated as case insensitive
%%                                   unless it starts with a quote
%% Descrip.: convert a contact-parameter list to a normalized (a case
%%           insensitive form) form
%% Returns : contact_param record() |
%%           throw({error, duplicate_key}) if Name
%%           component is already present in Params
%%--------------------------------------------------------------------
to_norm(Params) when is_list(Params) ->
    F = fun({Name, ValIn}) ->
		Val =
		    case ValIn of
			none ->
			    none;
			[] ->
			    none;
			[$\" | _] ->
			    ValIn;  %% quoted value, don't lowercase
			_ when is_list(ValIn) ->
			    httpd_util:to_lower(ValIn)
	            end,
		{ httpd_util:to_lower(Name), Val}
	end,
    %% make case insensitive
    L = [F(E) || E <- Params],
    #contact_param{pairs = key_val_db:new(L)}.

%%--------------------------------------------------------------------
%% Function: to_list(Norm)
%%           Norm = contact_param record()
%% Descrip.: returns a normalized form of the parameters
%% Returns : list() of {Key, Val}
%%           Key = string()
%%           Val = string()
%%--------------------------------------------------------------------
to_list(Norm) ->
    key_val_db:to_key_val(Norm#contact_param.pairs).


%%--------------------------------------------------------------------
%% Function: to_string(Norm)
%%           Norm = contact_param record()
%% Descrip.: return a raw contact-parameter string
%% Returns : string(), in the ";name=val;..." format
%%--------------------------------------------------------------------
to_string(Norm) when is_record(Norm, contact_param) ->
    L = to_list(Norm),
    ParamStrList = [format_param(Param) || Param <- L],
    lists:append(ParamStrList).

format_param({Name, Val}) ->
    lists:flatten(io_lib:format(";~s=~s",[Name, Val])).


%%--------------------------------------------------------------------
%% Function: add(ContactParam, Key, Value)
%%           ContactParam = contact_param record(), the record to
%%                          update
%%           Key          = string(), treated as case insensitive
%%           Value        = string(), treated as case insensitive
%% Descrip.: add new entry or replace old entry in contact_param. Key
%%           and Value are stored in a case insensitive manner
%% Returns : contact_param record()
%%--------------------------------------------------------------------
add(ContactParam, Key, Value) ->
    NKey = httpd_util:to_lower(Key),
    NValue = httpd_util:to_lower(Value),
    add2(ContactParam, {NKey, NValue}).

add2(ContactParam, {Key, Value}) ->
    L = ContactParam#contact_param.pairs,
    #contact_param{ pairs = key_val_db:add(L, Key, Value)}.


%%--------------------------------------------------------------------
%% Function: find(ContactParam, Key)
%%           ContactParam = contact_param record(), the record to
%%                          update
%%           Key          = string(), is treated as case insensitive
%% Descrip.: retrieve the value of Key if it is present in
%%           ContactParam
%% Returns : [string()] | []
%%--------------------------------------------------------------------
find(ContactParam, Key) ->
    Data = ContactParam#contact_param.pairs,
    CKey = httpd_util:to_lower(Key),
    key_val_db:find(Data, CKey).

%%--------------------------------------------------------------------
%% Function: remove(ContactParam, Key)
%%           ContactParam = contact_param record()
%%           Key          = string(), is treated as case insensitive
%% Descrip.: find the Key-Val pair to remove from ContactParam
%% Returns : contact_param record
%%--------------------------------------------------------------------
remove(ContactParam, Key) ->
    Data = ContactParam#contact_param.pairs,
    CKey = httpd_util:to_lower(Key),
    Res = key_val_db:rm(Data, CKey),
    ContactParam#contact_param{pairs = Res}.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.: autotest callback
%% Returns :
%%--------------------------------------------------------------------
test() ->
    %% test to_norm
    %%---------------------------------------------------------------
    %% test regular case, with case insensitivity
    io:format("test: to_norm/1 - 1~n"),
    DB1 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a", "43"}]),
    #contact_param{pairs = DB1 } = to_norm([{"foo","bar"}, {"bar","42"}, {"a", "43"}]),

    %% test empty param list
    io:format("test: to_norm/1 - 2~n"),
    DB2 = key_val_db:new([]),
    #contact_param{pairs = DB2 } = to_norm([]),

    %% test that duplicate names are detected
    io:format("test: to_norm/1 - 3~n"),
    case catch to_norm([{"foo","bar"}, {"bar","42"}, {"foo", "43"}]) of
 	{error, _} -> ok;
 	_ -> throw({error, test_failed})
    end,

    %% test that duplicate names in different case are detected
    io:format("test: to_norm/1 - 4~n"),
    case catch to_norm([{"foo","bar"}, {"bar","42"}, {"FOO", "43"}]) of
 	{error, _} -> ok;
 	_ -> throw({error, test_failed})
    end,

    %% test to_string
    %%---------------------------------------------------------------
    %% test that case and missing value part are handled properly
    io:format("test: to_string/1 - 1~n"),
    ";foo=bar;lr=true;a=43" = to_string(to_norm([{"foo","bar"}, {"lr","true"}, {"a","43"}])),
    %% test empty param
    io:format("test: to_string/1 - 2~n"),
    "" = to_string(to_norm([])),

    %% test to_list
    %%---------------------------------------------------------------
    %% regular case
    io:format("test: to_list/1 - 1~n"),
    [{"foo","bar"}, {"bar","42"}, {"a", "43"}] = to_list(to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}])),

    %% empty list
    io:format("test: to_list/1 - 2~n"),
    [] = to_list(to_norm([])),

    %% test case handling
    io:format("test: to_list/1 - 3~n"),
    [{"foo","bar"}, {"bar","42"}, {"a", "43"}] = to_list(to_norm([{"foo","bAr"}, {"BAr","42"}, {"A","43"}])),

    %% test add
    %%---------------------------------------------------------------
    %% add Key-Val to empty url_param
    io:format("test: add/3 - 1~n"),
    ContactParam1 = to_norm([]),
    AddDB1 = key_val_db:new([{"foo","bar"}]),
    #contact_param{pairs = AddDB1 } = add(ContactParam1, "foo", "bar"),


    ContactParam2 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),

    %% add a new key-val
    io:format("test: add/3 - 5~n"),
    AddDB3 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a", "43"}, {"gazong", "zog"}]),
    #contact_param{pairs =  AddDB3 } =
	add(ContactParam2, "gazong", "zog"),


    %% test find
    %%---------------------------------------------------------------
    %% test find with existing value
    ContactParam4 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    io:format("test: find/2 - 1~n"),
    ["42"] = find(ContactParam4, "bar"),

    %% test find with missing value
    ContactParam5 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    io:format("test: find/2 - 2~n"),
    [] = find(ContactParam5, "zog"),

    %% test that find handles Key in a case insensitive manner
    ContactParam4_2 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    io:format("test: find/2 - 3~n"),
    ["42"] = find(ContactParam4_2, "bAr"),

    %% test remove
    %%---------------------------------------------------------------
    %% test remove with existing value
    ContactParam6 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    io:format("test: remove/2 - 1~n"),
    RMDB1 = key_val_db:new([{"foo","bar"}, {"a","43"}]),
    #contact_param{pairs = RMDB1 } = remove(ContactParam6, "bar"),

    %% test remove with missing value
    ContactParam7 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    io:format("test: remove/2 - 2~n"),
    RMDB2 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    #contact_param{pairs = RMDB2 } = remove(ContactParam7, "zog"),

    %% test remove, ensure that Key is used in a case insensitive manner
    ContactParam8 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    io:format("test: remove/2 - 3~n"),
    RMDB3 = key_val_db:new([{"foo","bar"}, {"a","43"}]),
    #contact_param{pairs = RMDB3 } = remove(ContactParam8, "BaR"),

    %% test remove, from empty #contact_param.pair
    ContactParam9 = to_norm([]),
    io:format("test: remove/2 - 4~n"),
    RMDB4 = key_val_db:new([]),
    #contact_param{pairs = RMDB4 } = remove(ContactParam9, "BaR"),

    ok.


%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================
