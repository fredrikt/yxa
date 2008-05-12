%%%-------------------------------------------------------------------
%%% File    : autotest_util.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Utility functions to use with YXA's autotest unit
%%%           testing framework.
%%%
%%% @since    30 Apr 2008 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(autotest_util).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([fail/1,

	 is_unit_testing/2,
	 store_unit_test_result/3,
	 clear_unit_test_result/2,

	 compare_records/3,
	 compare_records/4
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").


%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    (Fun) -> ok
%%
%%            Fun = function()
%%
%% @throws  {error, no_exception_thrown_by_test}
%%
%% @doc     test case support function, used to check if call Fun()
%%          fails - as expected
%% @end
%%--------------------------------------------------------------------
fail(Fun) ->
    try Fun() of
	_  -> throw({error, no_exception_thrown_by_test})
    catch
	_ -> ok %% catch user throw()
    end.

%%--------------------------------------------------------------------
%% @spec    (Module, Key) ->
%%            {true, Result} |
%%            false
%%
%%            Module = atom() "calling module (currently unused)"
%%            Key    = term()
%%
%%            Result = term()
%%
%% @doc     Check if we are currently unit testing and have a result
%%          stored for the user of this specific Key.
%% @end
%%--------------------------------------------------------------------
is_unit_testing(Module, Key) when is_atom(Module) ->
    case get({autotest, Key}) of
	undefined ->
	    false;
	Res ->
	    {true, Res}
    end.

%%--------------------------------------------------------------------
%% @spec    (Module, Key, Value) -> term()
%%
%%            Module = atom() "calling module (currently unused)"
%%            Key    = term()
%%            Value  = term()
%%
%% @doc     Store a value to be returned for this Key.
%% @end
%%--------------------------------------------------------------------
store_unit_test_result(Module, Key, Value) when is_atom(Module) ->
    put({autotest, Key}, Value).

%%--------------------------------------------------------------------
%% @spec    (Module, Key) -> term()
%%
%%            Module = atom() "calling module (currently unused)"
%%            Key    = term()
%%
%% @doc     Clear any stored value for this Key.
%% @end
%%--------------------------------------------------------------------
clear_unit_test_result(Module, Key) when is_atom(Module) ->
    erase({autotest, Key}).

%%--------------------------------------------------------------------
%% @spec    (T1, T2, ShouldChange) -> ok | {error, Reason}
%%
%%            T1           = tuple() "Record #1"
%%            T2           = tuple() "Record #2"
%%            ShouldChange = [atom()]
%%
%%            Reason       = string()
%%
%% @see     compare_records/4.
%%
%% @doc     Same as compare_records/4 but will use numeric names of
%%          fields in error messages, if you don't care to use
%%          record_info in the calling module to get the real names.
%%
%% @end
%%--------------------------------------------------------------------
compare_records(T1, T2, ShouldChange) when is_tuple(T1), is_tuple(T2), is_list(ShouldChange) ->
    %% Two records as input, see if they are standard ones (from siprecords.hrl), and
    %% otherwise use numbers istead of field names
    Fields =
	case test_record_info(element(1, T1)) of
	    undefined ->
		lists:seq(1, size(T1) - 1);
	    T1_Fields ->
		T1_Fields
	end,
    compare_records(T1, T2, ShouldChange, Fields).

%%--------------------------------------------------------------------
%% @spec    (T1, T2, ShouldChange, Fields) -> ok | {error, Reason}
%%
%%            T1           = tuple() "Record #1"
%%            T2           = tuple() "Record #2"
%%            ShouldChange = [atom()]
%%            Fields       = [atom()] "Record fields, as given by record_info/2"
%%
%%            Reason       = string()
%%
%% @doc     Compare two records, typically before- and after-versions
%%          of some kind of state in a test case. Fail if any field
%%          that is NOT listed in ShouldChange differs, or if a field
%%          that IS listed in ShouldChange has NOT changed.
%%
%% @end
%%--------------------------------------------------------------------
compare_records(T1, T2, ShouldChange, Fields) when is_tuple(T1), is_tuple(T2), is_list(Fields),
                                                   is_list(ShouldChange) ->
    compare_records(tuple_to_list(T1), tuple_to_list(T2), ShouldChange, Fields);
compare_records(L1, L2, ShouldChange, Fields) when is_list(L1), is_list(L2), is_list(Fields),
						   is_list(ShouldChange) ->
    if
	hd(L1) /= hd(L2) ->
	    Msg = io_lib:format("Records are not of the same kind! : ~p /= ~p", [hd(L1), hd(L2)]),
	    {error, lists:flatten(Msg)};
	length(L1) /= length(L2) ->
	    Msg = io_lib:format("These are not records, they have different length! ~p", [hd(L1)]),
	    {error, lists:flatten(Msg)};
	true ->
	    RecName = hd(L1),
	    case ShouldChange -- Fields of
		[] ->
		    compare_records2(tl(L1), tl(L2), RecName, Fields, ShouldChange);
		Unknown ->
		    Msg = io_lib:format("ShouldChange field(s) ~p not valid for record ~p (fields : ~w)",
					[Unknown, RecName, Fields]),
		    {error, lists:flatten(Msg)}
	    end
    end.

compare_records2([Elem | L1], [Elem | L2], RecName, [ThisField | Fields], ShouldChange) ->
    %% element at first position matches
    %%io:format("Record ~p#~p matches~n", [RecName, ThisField]),
    case lists:member(ThisField, ShouldChange) of
	true ->
	    Msg = io_lib:format("Record ~p#~p NOT changed", [RecName, ThisField]),
	    {error, lists:flatten(Msg), Elem};
	false ->
	    compare_records2(L1, L2, RecName, Fields, ShouldChange)
    end;
compare_records2([Elem1 | L1], [Elem2 | L2], RecName, [ThisField | Fields], ShouldChange) ->
    case lists:member(ThisField, ShouldChange) of
	true ->
	    %%io:format("Record ~p#~p does NOT match, but we ignore that : ~p /= ~p~n",
	    %%	      [RecName, ThisField, Elem1, Elem2]),
	    compare_records2(L1, L2, RecName, Fields, ShouldChange);
	false ->
	    Msg = io_lib:format("Record ~p#~p does NOT match", [RecName, ThisField]),
	    {error, lists:flatten(Msg), Elem1, Elem2}
    end;
compare_records2([], [], _RecName, [], _ShouldChange) ->
    ok.


%% add records found in siprecords.hrl here
test_record_info(yxa_ctx) ->			record_info(fields, yxa_ctx);
test_record_info(yxa_app_init) ->		record_info(fields, yxa_app_init);
test_record_info(request) ->			record_info(fields, request);
test_record_info(response) ->			record_info(fields, response);
test_record_info(via) ->			record_info(fields, via);
test_record_info(contact) ->			record_info(fields, contact);
test_record_info(sipurl) ->			record_info(fields, sipurl);
test_record_info(keylist) ->			record_info(fields, keylist);
test_record_info(url_param) ->			record_info(fields, url_param);
test_record_info(contact_param) ->		record_info(fields, contact_param);
test_record_info(sipdns_srv) ->			record_info(fields, sipdns_srv);
test_record_info(sipdns_hostport) ->		record_info(fields, sipdns_hostport);
test_record_info(siplocationdb_e) ->		record_info(fields, siplocationdb_e);
test_record_info(dialog) ->			record_info(fields, dialog);
test_record_info(_) ->
    undefined.
