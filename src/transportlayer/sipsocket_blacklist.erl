%%%-------------------------------------------------------------------
%%% File    : sipsocket_blacklist.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Sipsocket blacklist gen_server and interface functions.
%%%           Keeps track of destinations that recently has failed to
%%%           respond to SIP messages. Read more in RFC3263 and 4321.
%%%
%%% @since    19 Feb 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
%%%-------------------------------------------------------------------
-module(sipsocket_blacklist).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0
	]).

%%--------------------------------------------------------------------
%% External interface exports
%%--------------------------------------------------------------------
-export([report_unreachable/3,
	 report_unreachable/4,
	 remove_blacklisting/1,
	 remove_blacklisting/2,

	 is_blacklisted/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports - for local.erl use only
%%--------------------------------------------------------------------
-export([lookup_sipsocket_blacklist/1
	]).

%%--------------------------------------------------------------------
%% Internal exports - for sipsocket supervisor only
%%--------------------------------------------------------------------
-export([get_blacklist_name/0
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {bl			%% blacklist ETS table name/reference
	       }).

-record(blacklist_entry, {pid,		%% pid(), who reported this destination unreachable?
			  reason,	%% string(), reason behind blacklisting
			  ts,		%% integer(), util:timestamp() time of blacklisting
			  probe_t,	%% integer(), util:timestamp() of when to probe this destination
			  duration	%% integer(), suggested penalty duration
			 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, sipsocket_blacklist).
-define(SIPSOCKET_BLACKLIST, yxa_sipsocket_blacklist).

%% Our standard wakeup interval - how often we should look for expired
%% entrys in the blacklist ETS table
-define(TIMEOUT, 89 * 1000).

-define(PROBE_TIMEOUT, 32).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?SIPSOCKET_BLACKLIST], []).


%%====================================================================
%% External interface functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (SipDst, SipSocket, Msg) -> ok
%%
%%            SipDst    = #sipdst{}
%%            SipSocket = #sipsocket{} | none
%%            Msg       = string() "reason for blacklisting"
%%
%% @doc     Blacklist a destination.
%% @equiv    report_unreachable(SipDst, SipSocket, Msg, undefined)
%% @end
%%--------------------------------------------------------------------
report_unreachable(SipDst, SipSocket, Msg) when is_record(SipDst, sipdst),
						(is_record(SipSocket, sipsocket) orelse SipSocket == none),
						is_list(Msg) ->
    report_unreachable(SipDst, SipSocket, Msg, undefined).

%%--------------------------------------------------------------------
%% @spec    (SipDst, SipSocket, Msg, RetryAfter) -> ok
%%
%%            SipDst     = #sipdst{}
%%            SipSocket  = #sipsocket{} | none
%%            Msg        = string() "reason for blacklisting"
%%            RetryAfter = undefined | integer() "seconds to blacklist"
%%
%% @doc     Blacklist a destination.
%% @end
%%--------------------------------------------------------------------
report_unreachable(SipDst, SipSocket, Msg, RetryAfter) when is_record(SipDst, sipdst), is_list(Msg),
							    (is_integer(RetryAfter) orelse RetryAfter == undefined) ->
    case yxa_config:get_env(sipsocket_blacklisting) of
	{ok, true} ->
	    %% Figure out how long time to blacklist this destination this time
	    {ok, StdDuration} = yxa_config:get_env(sipsocket_blacklist_duration),
	    {ok, MaxPenalty} = yxa_config:get_env(sipsocket_blacklist_max),
	    Now = util:timestamp(),
	    ProbeTS =
		case RetryAfter of
		    undefined ->
			{ok, ProbeDelay} = yxa_config:get_env(sipsocket_blacklist_probe_delay),
			Now + ProbeDelay;
		    _ ->
			undefined
		end,
	    do_report_unreachable(SipDst, SipSocket, lists:flatten(Msg), RetryAfter,
				  StdDuration, MaxPenalty, ProbeTS, Now, ?SIPSOCKET_BLACKLIST);
	{ok, false} ->
	    ok
    end.


%%--------------------------------------------------------------------
%% @spec    (SipDst) -> ok
%%
%%            SipDst = #sipdst{}
%%
%% @doc     Remove a destination from the blacklist, if it is present
%%          there.
%% @end
%%--------------------------------------------------------------------
remove_blacklisting(SipDst) when is_record(SipDst, sipdst) ->
    do_remove_blacklisting(SipDst, ?SIPSOCKET_BLACKLIST).

remove_blacklisting(SipDst, EtsRef) when is_record(SipDst, sipdst) ->
    do_remove_blacklisting(SipDst, EtsRef).


%%--------------------------------------------------------------------
%% @spec    (SipDst) -> true | false
%%
%%            SipDst = #sipdst{}
%%
%% @doc     Check if a destination is currently blacklisted.
%% @end
%%--------------------------------------------------------------------
is_blacklisted(SipDst) when is_record(SipDst, sipdst) ->
    case yxa_config:get_env(sipsocket_blacklisting) of
	{ok, true} ->
	    do_is_blacklisted(SipDst, util:timestamp(), ?SIPSOCKET_BLACKLIST);
	{ok, false} ->
	    false
    end.


%%--------------------------------------------------------------------
%% @spec    (Dst) ->
%%            {ok, Entry}       |
%%            {ok, whitelisted} |
%%            {ok, blacklisted} |
%%            undefined
%%
%%            Dst   = {Proto, Addr, Port} "ets table lookup key"
%%            Proto = term()
%%            Addr  = term()
%%            Port  = term()
%%
%% @doc     Part of lookup_dst, unless 'local' had an opinion about a
%%          destination. NOTE: Not implemented yet.
%% @end
%%--------------------------------------------------------------------
lookup_sipsocket_blacklist({_Proto, _Addr, _Port} = _Dst) ->
    %% XXX check for configured lists of black/whitelisted destinations here
    undefined.

%%====================================================================
%% External functions - for sipsocket supervisor only
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () ->
%%            EtsRef
%%
%%            EtsRef = term()
%%
%% @doc     Return the name of the blacklisting ets table. For use
%%          from sipsocket supervisor only!
%% @hidden
%% @end
%%--------------------------------------------------------------------
get_blacklist_name() ->
    ?SIPSOCKET_BLACKLIST.


%%====================================================================
%% Behaviour callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ([TableName]) ->
%%            {ok, State}          |
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%
%%            TableName = term() "ETS table name/reference"
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([TableName]) ->
    {ok, #state{bl = TableName}, ?TIMEOUT}.


%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Unknown, From, State) ->
%%            {reply, {error, not_implemented}, State, Timeout::integer()}
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "Sipsocket blacklist: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, not_implemented}, State, ?TIMEOUT}.


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
%% @spec    (Unknown, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Unknown cast.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "Sipsocket blacklist: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State, ?TIMEOUT}.


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
%% @spec    (timeout, State) -> {noreply, State, Timeout::integer()}
%%
%% @doc     Wake up and delete expired sockets from our list.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    AllEntrys = ets:tab2list(State#state.bl),
    case AllEntrys of
	[] -> ok;
	_ ->
	    logger:log(debug, "Sipsocket blacklist: Extra debug: Contents of blacklist table is :~n~p",
		       [AllEntrys]),
	    delete_expired_entrys(AllEntrys, util:timestamp(), State#state.bl)
    end,
    {noreply, State, ?TIMEOUT}.

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
%% @spec    (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @spec    (SipDst, SipSocket, Msg, SecondsIn, StdDuration,
%%          MaxPenalty, ProbeTS, Now, EtsRef) -> ok
%%
%%            SipDst      = #sipdst{}
%%            SipSocket   = #sipsocket{}
%%            Msg         = string() "reason for blacklisting (only for debug/diagnostic use)"
%%            SecondsIn   = integer() | undefined "seconds requested by upper layers (through an Retry-After header in a 503 response for example)"
%%            StdDuration = integer() "configured default duration - used if SecondsIn is 'undefined'"
%%            MaxPenalty  = integer() "configured max duration"
%%            ProbeTS     = undefined() | integer() "util:timestamp() notion of when we should start probing this destination, if it is requested again"
%%            Now         = integer() "util:timestamp() notion of present time"
%%            EtsRef      = term() "ETS table reference"
%%
%% @doc     Part of the exported report_unreachable function. Made
%%          this way in order to be testable.
%% @end
%%--------------------------------------------------------------------
do_report_unreachable(SipDst, SipSocket, Msg, SecondsIn, StdDuration, MaxPenalty, ProbeTS, Now, EtsRef) ->
    Dst = make_dst(SipDst),
    case get_blacklist_time(Dst, SecondsIn, EtsRef, StdDuration, MaxPenalty) of
	Seconds when is_integer(Seconds) ->
	    logger:log(debug, "Sipsocket blacklist: Blacklisting destination ~s for ~p seconds",
		       [sipdst:dst2str(SipDst), Seconds]),
	    Entry =
		#blacklist_entry{pid      = self(),
				 reason   = Msg,
				 ts       = Now,
				 probe_t  = ProbeTS,
				 duration = Seconds
				},

	    ets:insert(EtsRef, {Dst, Entry}),
	    case is_record(SipSocket, sipsocket) of
		true ->
		    case sipsocket:close_socket(SipSocket) of
			ok -> ok;
			{error, not_applicable} -> ok;
			{error, Reason} ->
			    logger:log(error, "Sipsocket blacklist: Failed closing sipsocket ~p : ~p",
				       [SipSocket, Reason])
		    end,
		    ok;
		false ->
		    ok
	    end;
	ignore ->
	    ok
    end,
    ok.


%%--------------------------------------------------------------------
%% @spec    (Dst, SecondsIn, EtsRef, StdDuration, MaxDuration) ->
%%            Seconds |
%%            ignore
%%
%%            Dst         = term() "ETS table key"
%%            SecondsIn   = integer() | undefined "seconds requested by upper layers (through an Retry-After header in a 503 response for example)"
%%            EtsRef      = term() "ETS table reference"
%%            StdDuration = integer() "configured default duration - used if SecondsIn is 'undefined'"
%%            MaxDuration = integer() "configured max duration"
%%
%%            Seconds = integer()
%%
%% @doc     Decide how long a host should be blacklisted, or if the
%%          blacklisting request should be ignored completely
%%          (because the destination is black/whitelisted through
%%          configuration).
%% @end
%%--------------------------------------------------------------------
get_blacklist_time(Dst, SecondsIn, EtsRef, StdDuration, MaxDuration) ->
    case lookup_dst(Dst, EtsRef) of
	{ok, blacklisted} ->
	    ignore;
	{ok, whitelisted} ->
	    ignore;
	{ok, OldEntry} when is_record(OldEntry, blacklist_entry) ->
	    get_blacklist_time_res(SecondsIn, StdDuration, MaxDuration);
	none ->
	    get_blacklist_time_res(SecondsIn, StdDuration, MaxDuration)
    end.

%% part of get_blacklist_time/5
get_blacklist_time_res(undefined, StdDur, Max) ->
    lists:min([StdDur, Max]);
get_blacklist_time_res(N, _StdDur, Max) when is_integer(N) ->
    lists:min([N, Max]).


%%--------------------------------------------------------------------
%% @spec    (SipDst, EtsRef) -> ok
%%
%%            SipDst = #sipdst{}
%%            EtsRef = term() "ETS table reference"
%%
%% @doc     Part of the exported remove_blacklisting function. Made
%%          this way in order to be testable. NOTE : Currently, the
%%          exported function is not used by the YXA stack or
%%          applications. Things get blacklisted, and the only part
%%          removing blacklists are this modules gen_server or probes
%%          that notice the destination is now reachable again.
%%          Having an API function to do it seems reasonable though.
%% @end
%%--------------------------------------------------------------------
do_remove_blacklisting(SipDst, EtsRef) when is_record(SipDst, sipdst) ->
    Dst = make_dst(SipDst),
    case lookup_dst(Dst, EtsRef) of
	{ok, Entry} when is_record(Entry, blacklist_entry) ->
	    logger:log(debug, "Sipsocket blacklist: Removing blacklisting of ~s", [sipdst:dst2str(SipDst)]),
	    ets:delete(EtsRef, Dst);
	{ok, blacklisted} ->
	    logger:log(debug, "Sipsocket blacklist: NOT removing (static) blacklisting of ~s",
		       [sipdst:dst2str(SipDst)]),
	    ok;
	{ok, whitelisted} ->
	    ok;
	none ->
	    ok
    end,
    ok.


%%--------------------------------------------------------------------
%% @spec    (SipDst, Now, EtsRef) ->
%%            NewDstList
%%
%%            SipDst = #sipdst{}
%%            Now    = integer() "util:timestamp() notion of present time"
%%            EtsRef = term() "ETS table reference"
%%
%%            NewDstList = [#sipdst{}]
%%
%% @doc     Filter out all currently blacklisted destinations from a
%%          list of sipdst records. Starts background probes where
%%          appropriate. Part of the exported
%%          blacklist_filter_dstlist/1 function - written this way to
%%          be testable.
%% @end
%%--------------------------------------------------------------------
do_is_blacklisted(SipDst, Now, EtsRef) ->
    Dst = make_dst(SipDst),
    case lookup_dst(Dst, EtsRef) of
	{ok, #blacklist_entry{duration = Duration} = Entry} when is_integer(Duration) ->
	    case (Entry#blacklist_entry.ts + Duration) - Now of
		Remaining when Remaining > 0 ->
		    DstStr = sipdst:dst2str(SipDst),
		    logger:log(debug, "Sipsocket blacklist: Skipping destination ~s, blacklisted for ~p more seconds",
			       [DstStr, Remaining]),

		    start_background_probe(SipDst, Dst, DstStr, Entry#blacklist_entry.probe_t, Now, EtsRef),

		    true;
		_ ->
		    %% Entry is expired, treat like no entry was present
		    false
	    end;
	{ok, blacklisted} ->
	    logger:log(debug, "Sipsocket blacklist: Skipping destination ~s, blacklisted statically",
		       [sipdst:dst2str(SipDst)]),
	    true;
	{ok, whitelisted} ->
	    false;
	none ->
	    false
    end.


start_background_probe(SipDst, Dst, DstStr, ProbeT, Now, EtsRef) when is_integer(ProbeT), Now >= ProbeT ->
    %% check that we don't have a probe for this destination running already
    ProbeId = {probe, Dst},
    case ets:insert_new(EtsRef, {ProbeId, self()}) of
	true ->
	    case (SipDst#sipdst.proto == yxa_test) of
		true ->
		    self() ! {start_background_probe, ProbeId};
		false ->
		    {ok, ProbePid} = sipsocket_blacklist_probe:start(SipDst, EtsRef, ProbeId, ?PROBE_TIMEOUT),
		    logger:log(debug, "Sipsocket blacklist: Started background probe for destination ~s : ~p",
			       [DstStr, ProbePid])
	    end;
	false ->
	    logger:log(debug, "Sipsocket blacklist: Extra debug: NOT starting background probe for "
		       "destination ~s since one is already running", [DstStr])
    end,
    ok;
start_background_probe(_SipDst, _Dst, DstStr, ProbeT, Now, _EtsRef) when is_integer(ProbeT) ->
    logger:log(debug, "Sipsocket blacklist: Extra debug: NOT starting background probe for "
	       "destination ~s for another ~p seconds",
	       [DstStr, ProbeT - Now]),
    ok;
start_background_probe(_SipDst, _Dst, DstStr, undefined, _Now, _EtsRef) ->
    logger:log(debug, "Sipsocket blacklist: Extra debug: NOT starting background probe for "
	       "destination ~s",
	       [DstStr]),
    ok.


%%--------------------------------------------------------------------
%% @spec    (Dst, EtsRef) ->
%%            {ok, Entry} |
%%            none
%%
%%            Dst    = term() "database lookup key"
%%            EtsRef = term() "ETS table reference"
%%
%%            Entry = blacklist_entry() |
%%            whitelisted               |
%%            blacklisted
%%
%% @doc     Returns the latest entry for Dst. Note well that the entry
%%          might be expired!
%% @end
%%--------------------------------------------------------------------
lookup_dst({_Proto, _Addr, _Port} = Dst, EtsRef) ->
    case local:lookup_sipsocket_blacklist(Dst) of
        {ok, Res} ->
	    {ok, Res};
	undefined ->
	    case ets:lookup(EtsRef, Dst) of
		[] ->
		    none;
		Entrys when is_list(Entrys) ->
		    {Dst, LatestEntry} = hd(lists:reverse(Entrys)),
		    {ok, LatestEntry}
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (In, Now, EtsRef) -> ok
%%
%%            In     = [{Dst, Entry}]
%%            Dst    = term() "lookup key ({Proto, Addr, Port})"
%%            Entry  = #blacklist_entry{}
%%            Now    = integer() "util:timestamp() value of present time"
%%            EtsRef = term() "ETS table reference"
%%
%% @doc     Delete all entrys that have a ts+duration less than Now.
%%          Called periodically by the sipsocket_blacklist
%%          gen_server.
%% @end
%%--------------------------------------------------------------------
delete_expired_entrys([{Dst, H} | T], Now, EtsRef) when is_record(H, blacklist_entry) ->
    case (H#blacklist_entry.ts + H#blacklist_entry.duration) - Now of
	Remaining when Remaining =< 0 ->
	    logger:log(debug, "Sipsocket blacklist: Extra debug: Removing expired entry for ~p : ~p",
		       [Dst, H]),
	    ets:delete_object(EtsRef, {Dst, H});
	_ ->
	    ok
    end,
    delete_expired_entrys(T, Now, EtsRef);
delete_expired_entrys([{{probe, _Dst}, _ProbePid} | T], Now, EtsRef) ->
    delete_expired_entrys(T, Now, EtsRef);
delete_expired_entrys([], _Now, _EtsRef) ->
    ok.


%%--------------------------------------------------------------------
%% @spec    (SipDst) ->
%%            Dst
%%
%%            SipDst = #sipdst{}
%%
%%            Dst = term()
%%
%% @doc     Make database key from a sipdst record.
%% @end
%%--------------------------------------------------------------------
make_dst(SipDst) when is_record(SipDst, sipdst) ->
    {SipDst#sipdst.proto, SipDst#sipdst.addr, SipDst#sipdst.port}.


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
    Self = self(),

    TestEtsRef = ets:new(yxa_blacklist_test, [bag]),

    TestSipDst1 = #sipdst{proto = yxa_test,
			  addr  = "192.0.2.1",
			  port  = 6050
			 },
    TestDst1 = make_dst(TestSipDst1),
    TestSipSocket1 = #sipsocket{proto  = yxa_test,
				module = sipsocket_test
			       },

    TestSipDst2 = #sipdst{proto = yxa_test,
			  addr  = "192.0.2.2",
			  port  = 6050
			 },
    %%TestDst2 = make_dst(TestSipDst2),

    %% test #1, add, read and delete
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "blacklisting test 1 - 1"),
    TestNow1 = 10000000,
    ok = do_report_unreachable(TestSipDst1, TestSipSocket1, "test", undefined, 90, 120, 1234, TestNow1, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 2"),
    %% verify result of insert
    TestBLEntry1 = #blacklist_entry{pid		= Self,
				    reason	= "test",
				    ts		= TestNow1,
				    probe_t	= 1234,
				    duration	= 90
				   },

    [{TestDst1, TestBLEntry1}] = ets:tab2list(TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 3"),
    {ok, TestBLEntry1} = lookup_dst(TestDst1, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 4"),
    %% insert again, with specified blacklist time
    ok = do_report_unreachable(TestSipDst1, TestSipSocket1, "test", 100, 90, 120, 1234, TestNow1, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 5"),
    %% verify result of insert, both entrys should be there
    TestBLEntry5 = TestBLEntry1#blacklist_entry{duration = 100},
    [{TestDst1, TestBLEntry1}, {TestDst1, TestBLEntry5}] = ets:tab2list(TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 6"),
    %% verify that we only get the most recent entry back
    {ok, TestBLEntry5} = lookup_dst(TestDst1, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 7"),
    %% verify result of insert, both entrys should be there
    TableContents8 = [{TestDst1, TestBLEntry1}, {TestDst1, TestBLEntry5}],
    TableContents8 = ets:tab2list(TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 8"),
    %% verify that we only get the most recent entry back
    {ok, TestBLEntry5} = lookup_dst(TestDst1, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 9"),
    %% delete _wrong_ destination
    ok = do_remove_blacklisting(TestSipDst2, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 10"),
    %% verify that the table still looks the same
    TableContents8 = ets:tab2list(TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 11"),
    %% delete right destination
    ok = do_remove_blacklisting(TestSipDst1, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 1 - 12"),
    %% verify that the table is now empty again
    [] = ets:tab2list(TestEtsRef),


    %% test #2, two expired entrys
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "blacklisting test 2 - 1"),
    TestNow2 = 10000000,
    %% simulate inserting entry 100 seconds ago, with expire time 60 seconds
    ok = do_report_unreachable(TestSipDst1, TestSipSocket1, "test 2", 60, 30, 120,
			       undefined, TestNow2 - 100, TestEtsRef),
    %% simulate inserting entry 90 seconds ago, with expire time 30 seconds
    ok = do_report_unreachable(TestSipDst1, TestSipSocket1, "test 2", undefined, 30, 120,
			       undefined, TestNow2 - 90, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 2 - 2"),
    [{_, _}, {_, _}] = ets:tab2list(TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 2 - 3"),
    TestBLEntry2 = #blacklist_entry{pid		= Self,
				    reason	= "test 2",
				    ts		= TestNow2 - 90,
				    probe_t	= undefined,
				    duration	= 30
				   },
    {ok, TestBLEntry2} = lookup_dst(TestDst1, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 2 - 4"),
    %% add non-expired entry
    ok = do_report_unreachable(TestSipDst1, TestSipSocket1, "test 2", 60, 30, 120,
			       undefined, TestNow2, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 2 - 5"),
    %% delete expired entrys
    ok = delete_expired_entrys(ets:tab2list(TestEtsRef), TestNow2, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 2 - 6"),
    %% verify table now only contains the non-expired entry
    TestBLEntry2_1 = TestBLEntry2#blacklist_entry{ts       = TestNow2,
						  duration = 60
						 },
    [{TestDst1, TestBLEntry2_1}] = ets:tab2list(TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 2 - 7"),
    %% delete destination
    ok = do_remove_blacklisting(TestSipDst1, TestEtsRef),

    autotest:mark(?LINE, "blacklisting test 2 - 8"),
    %% verify that the table is now empty again
    [] = ets:tab2list(TestEtsRef),


    %% do_is_blacklisted(SipDst, Now, EtsRef)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "do_is_blacklisted/3 - 1.0"),
    TestISBL_Now = util:timestamp(),
    TestISBL_Penalty = 100,
    ok = do_report_unreachable(TestSipDst1, TestSipSocket1, "test", TestISBL_Penalty, 90, 120,
			       undefined, TestISBL_Now, TestEtsRef),

    autotest:mark(?LINE, "do_is_blacklisted/3 - 1.1"),
    %% other SipDst
    false = do_is_blacklisted(TestSipDst2, TestISBL_Now, TestEtsRef),

    autotest:mark(?LINE, "do_is_blacklisted/3 - 1.2"),
    %% blacklisted for one more second
    true = do_is_blacklisted(TestSipDst1, TestISBL_Now + TestISBL_Penalty - 1, TestEtsRef),

    autotest:mark(?LINE, "do_is_blacklisted/3 - 1.3"),
    %% blacklisting expires this very second
    false = do_is_blacklisted(TestSipDst1, TestISBL_Now + TestISBL_Penalty, TestEtsRef),

    autotest:mark(?LINE, "do_is_blacklisted/3 - 2.0"),
    TestISBL_ProbeT = TestISBL_Now + 5,
    TestISBL_Expire = 120,
    ok = do_report_unreachable(TestSipDst2, TestSipSocket1, "test", TestISBL_Penalty,
			       90, 120, TestISBL_ProbeT, TestISBL_Now, TestEtsRef),
    %% make sure there are no signals in our process mailbox
    receive
	TestISBL_M1 ->
	    throw({error, test_received_unknown_signal, TestISBL_M1})
    after 0 ->
	    ok
    end,

    autotest:mark(?LINE, "do_is_blacklisted/3 - 2.1"),
    %% blacklisted, but no probe for one more second
    true = do_is_blacklisted(TestSipDst2, TestISBL_ProbeT - 1, TestEtsRef),

    autotest:mark(?LINE, "do_is_blacklisted/3 - 2.2"),
    %% make sure there are no signals in our process mailbox
    receive
	TestISBL_M2 ->
	    throw({error, test_received_unknown_signal, TestISBL_M2})
    after 0 ->
	    ok
    end,

    autotest:mark(?LINE, "do_is_blacklisted/3 - 2.3"),
    %% expired entry, don't start probe
    false = do_is_blacklisted(TestSipDst2, TestISBL_Now + TestISBL_Expire, TestEtsRef),

    autotest:mark(?LINE, "do_is_blacklisted/3 - 2.4"),
    %% blacklisted, and time to probe
    true = do_is_blacklisted(TestSipDst2, TestISBL_ProbeT, TestEtsRef),
    %% make sure the expected signal is in our process mailbox
    receive
	{start_background_probe, {probe, TestDst2}} ->
	    ets:delete(TestEtsRef, {probe, TestDst2}),
	    ok
    after 0 ->
	    throw({error, "background probe did not start"})
    end,

    autotest:mark(?LINE, "do_is_blacklisted/3 - 3.0"),
    %% delete destinations
    ok = do_remove_blacklisting(TestSipDst1, TestEtsRef),
    ok = do_remove_blacklisting(TestSipDst2, TestEtsRef),

    autotest:mark(?LINE, "do_is_blacklisted/3 - 3.1"),
    %% verify that the table is now empty again
    [] = ets:tab2list(TestEtsRef),


    ok.
