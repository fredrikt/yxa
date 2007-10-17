%%%-------------------------------------------------------------------
%%% File    : contact.erl
%%% @author   Håkan Stenholm <hsten@it.su.se>
%%% @doc      This module handles contact-params for a single contact
%%%           header entry (sip/sips uri).
%%%
%%%           Note: keys and values are currently stored as strings
%%%           but pattern matching and list:keysearch will be faster
%%%           if standard values are represented as atoms (but don't
%%%           turn them all into atoms - as atoms aren't GCed)
%%%
%%% @since    09 Sep 2004 by Håkan Stenholm <hsten@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(contact_param).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 %% create contact_param
	 to_norm/1,
	 %% form contact_param to other format
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
%% @spec    (Params) -> #contact_param{}
%%
%%            Params = [{Name, Val}]
%%            Name   = string() "treated as case insensitive"
%%            Val    = string() | none "treated as case insensitive unless it starts with a quote"
%%
%% @throws  {error, duplicate_key} 
%%
%% @doc     Convert a contact-parameter list to a normalized (a case
%%          insensitive form) form. Throws an error if Name component
%%          is already present in Params.
%% @end
%%--------------------------------------------------------------------
to_norm(Params) when is_list(Params) ->
    F = fun({Name, ValIn}) when is_list(Name), (is_list(ValIn) orelse ValIn == none) ->
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
%% @spec    (Norm) ->
%%            [{Key, Val}]
%%
%%            Norm = #contact_param{}
%%
%%            Key = string()
%%            Val = string()
%%
%% @doc     Returns a normalized form of the parameters.
%% @end
%%--------------------------------------------------------------------
to_list(Norm) when is_record(Norm, contact_param) ->
    key_val_db:to_key_val(Norm#contact_param.pairs).


%%--------------------------------------------------------------------
%% @spec    (Norm) -> string() "in the \";name=val;...\" format"
%%
%%            Norm = #contact_param{}
%%
%% @doc     Return a raw contact-parameter string.
%% @end
%%--------------------------------------------------------------------
to_string(Norm) when is_record(Norm, contact_param) ->
    L = to_list(Norm),
    ParamStrList = [format_param(Param) || Param <- L],
    lists:append(ParamStrList).

format_param({Name, Val}) when is_list(Name), is_list(Val) ->
    lists:flatten(io_lib:format(";~s=~s", [Name, Val]));
format_param({Name, none}) when is_list(Name) ->
    lists:flatten(io_lib:format(";~s", [Name])).


%%--------------------------------------------------------------------
%% @spec    (ContactParam, Key, Value) -> #contact_param{}
%%
%%            ContactParam = #contact_param{} "the record to update"
%%            Key          = string() "treated as case insensitive"
%%            Value        = string() "treated as case insensitive"
%%
%% @doc     Add new entry or replace old entry in contact_param. Key
%%          and Value are stored in a case insensitive manner.
%% @end
%%--------------------------------------------------------------------
add(ContactParam, Key, Value) when is_record(ContactParam, contact_param), is_list(Key), is_list(Value) ->
    NKey = httpd_util:to_lower(Key),
    NValue = httpd_util:to_lower(Value),
    add2(ContactParam, {NKey, NValue}).

add2(ContactParam, {Key, Value}) ->
    L = ContactParam#contact_param.pairs,    #contact_param{ pairs = key_val_db:add(L, Key, Value)}.


%%--------------------------------------------------------------------
%% @spec    (ContactParam, Key) -> [string()] | []
%%
%%            ContactParam = #contact_param{} "the record to update"
%%            Key          = string() "is treated as case insensitive"
%%
%% @doc     Retrieve the value of Key if it is present in
%%          ContactParam.
%% @end
%%--------------------------------------------------------------------
find(ContactParam, Key) when is_record(ContactParam, contact_param), is_list(Key) ->
    Data = ContactParam#contact_param.pairs,
    CKey = httpd_util:to_lower(Key),
    key_val_db:find(Data, CKey).

%%--------------------------------------------------------------------
%% @spec    (ContactParam, Key) -> #contact_param{}
%%
%%            ContactParam = #contact_param{}
%%            Key          = string() "is treated as case insensitive"
%%
%% @doc     Find the Key-Val pair to remove from ContactParam.
%% @end
%%--------------------------------------------------------------------
remove(ContactParam, Key) ->
    Data = ContactParam#contact_param.pairs,
    CKey = httpd_util:to_lower(Key),
    Res = key_val_db:rm(Data, CKey),
    ContactParam#contact_param{pairs = Res}.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    %% test to_norm(Params)
    %%---------------------------------------------------------------
    %% test regular case, with case insensitivity
    autotest:mark(?LINE, "to_norm/1 - 1"),
    DB1 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a", "43"}]),
    #contact_param{pairs = DB1 } = to_norm([{"foo","bar"}, {"bar","42"}, {"a", "43"}]),

    %% test empty param list
    autotest:mark(?LINE, "to_norm/1 - 2"),
    DB2 = key_val_db:new([]),
    #contact_param{pairs = DB2 } = to_norm([]),

    %% test that duplicate names are detected
    autotest:mark(?LINE, "to_norm/1 - 3"),
    case catch to_norm([{"foo","bar"}, {"bar","42"}, {"foo", "43"}]) of
 	{error, _} -> ok;
 	_ -> throw({error, test_failed})
    end,

    %% test that duplicate names in different case are detected
    autotest:mark(?LINE, "to_norm/1 - 4"),
    case catch to_norm([{"foo","bar"}, {"bar","42"}, {"FOO", "43"}]) of
 	{error, _} -> ok;
 	_ -> throw({error, test_failed})
    end,

    %% test that quoted value isn't lowercased
    autotest:mark(?LINE, "to_norm/1 - 5"),
    #contact_param{pairs = [{"key","\"Value\""}]} = to_norm([{"Key", "\"Value\""}]),


    %% test to_string(Norm)
    %%---------------------------------------------------------------
    %% test that case and missing value part are handled properly
    autotest:mark(?LINE, "to_string/1 - 1"),
    ";foo=bar;lr=true;a=43" = to_string(to_norm([{"foo","bar"}, {"lr","true"}, {"a","43"}])),

    %% test empty param
    autotest:mark(?LINE, "to_string/1 - 2"),
    "" = to_string(to_norm([])),

    %% test empty value
    autotest:mark(?LINE, "to_string/1 - 3"),
    ";lr" = to_string( to_norm([{"lr", none}]) ),

    %% test empty value #2
    autotest:mark(?LINE, "to_string/1 - 4"),
    ";lr" = to_string( to_norm([{"lr", []}]) ),


    %% test to_list(Norm)
    %%---------------------------------------------------------------
    %% regular case
    autotest:mark(?LINE, "to_list/1 - 1"),
    [{"foo","bar"}, {"bar","42"}, {"a", "43"}] = to_list(to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}])),

    %% empty list
    autotest:mark(?LINE, "to_list/1 - 2"),
    [] = to_list(to_norm([])),

    %% test case handling
    autotest:mark(?LINE, "to_list/1 - 3"),
    [{"foo","bar"}, {"bar","42"}, {"a", "43"}] = to_list(to_norm([{"foo","bAr"}, {"BAr","42"}, {"A","43"}])),


    %% test add(ContactParam, Key, Value)
    %%---------------------------------------------------------------
    %% add Key-Val to empty url_param
    autotest:mark(?LINE, "add/3 - 1"),
    ContactParam1 = to_norm([]),
    AddDB1 = key_val_db:new([{"foo","bar"}]),
    #contact_param{pairs = AddDB1 } = add(ContactParam1, "foo", "bar"),


    ContactParam2 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),

    %% add a new key-val
    autotest:mark(?LINE, "add/3 - 5"),
    AddDB3 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a", "43"}, {"gazong", "zog"}]),
    #contact_param{pairs =  AddDB3 } =
	add(ContactParam2, "gazong", "zog"),


    %% test find(ContactParam, Key)
    %%---------------------------------------------------------------
    %% test find with existing value
    ContactParam4 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    autotest:mark(?LINE, "find/2 - 1"),
    ["42"] = find(ContactParam4, "bar"),

    %% test find with missing value
    ContactParam5 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    autotest:mark(?LINE, "find/2 - 2"),
    [] = find(ContactParam5, "zog"),

    %% test that find handles Key in a case insensitive manner
    ContactParam4_2 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    autotest:mark(?LINE, "find/2 - 3"),
    ["42"] = find(ContactParam4_2, "bAr"),


    %% test remove(ContactParam, Key)
    %%---------------------------------------------------------------
    %% test remove with existing value
    ContactParam6 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    autotest:mark(?LINE, "remove/2 - 1"),
    RMDB1 = key_val_db:new([{"foo","bar"}, {"a","43"}]),
    #contact_param{pairs = RMDB1 } = remove(ContactParam6, "bar"),

    %% test remove with missing value
    ContactParam7 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    autotest:mark(?LINE, "remove/2 - 2"),
    RMDB2 = key_val_db:new([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    #contact_param{pairs = RMDB2 } = remove(ContactParam7, "zog"),

    %% test remove, ensure that Key is used in a case insensitive manner
    ContactParam8 = to_norm([{"foo","bar"}, {"bar","42"}, {"a","43"}]),
    autotest:mark(?LINE, "remove/2 - 3"),
    RMDB3 = key_val_db:new([{"foo","bar"}, {"a","43"}]),
    #contact_param{pairs = RMDB3 } = remove(ContactParam8, "BaR"),

    %% test remove, from empty #contact_param.pair
    ContactParam9 = to_norm([]),
    autotest:mark(?LINE, "remove/2 - 4"),
    RMDB4 = key_val_db:new([]),
    #contact_param{pairs = RMDB4 } = remove(ContactParam9, "BaR"),

    ok.


%%====================================================================
%% Behaviour functions
%%====================================================================

