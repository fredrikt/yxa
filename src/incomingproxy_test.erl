%%%-------------------------------------------------------------------
%%% File    : incomingproxy_test.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Test cases for the complete incomingproxy application.
%%%
%%% @since    29 Nov 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @hidden
%%%-------------------------------------------------------------------
-module(incomingproxy_test).

-export([test/0]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").

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
-ifdef( YXA_NO_UNITTEST ).
test() ->
    {error, "Unit test code disabled at compile time"}.

-else.

test() ->
    autotest:mark(?LINE, "incomingproxy_test setup - 0"),

    UserDb =
	[{user, [
		 {name, "autotest1"},
		 {password, "secret"},
		 {classes, [internal,national,mobile]},
		 {addresses, ["sip:autotest1@example.org", "sip:234599@example.org"]}
		]},
	 {user, [
		 {name, "autotest2"},
		 {addresses, ["sip:autotest2@example.org"]}
		]}

	],
    ok = sipuserdb_test:init(UserDb),

    ExtraCfg = [
		{userdb_modules,		[sipuserdb_test]},
		{myhostnames,			["autotest.example.org"]}

	       ],
    ok = yxa_test_config:init(incomingproxy, ExtraCfg),

    ok = test_request(),

    ok.

test_request() ->
    autotest_util:store_unit_test_result(incomingproxy, testing_sippipe, {ok, self()}),
    autotest_util:store_unit_test_result(transactionlayer, get_branch_from_handler, "test-branch"),

    phone:test_create_table(),
    database_gruu:test_create_table(),
    database_regexproute:test_create_table(),
    cpl_db:test_create_table(),

    ok = test_route(),
    ok = test_pubsub_routing(),
    ok = test_regexp_routing(),

    ok.


test_route() ->
    autotest:mark(?LINE, "request/2 - Route - 0.0"),

    case mnesia:transaction(fun test_route2/0) of
	{aborted, ok} ->
	    ok;
	{aborted, Res} ->
	    {error, Res}
    end.

test_route2() ->
    autotest:mark(?LINE, "request/2 - Route - 0.1"),
    Location = sipurl:parse("sip:ua@192.0.2.1"),
    RouteURL = sipurl:parse("sip:ua@192.0.2.66"),

    {atomic, ok} = phone:insert_purge_phone("autotest1", [], static, util:timestamp() + 10,
					    Location, "test callid", 1, ""),

    YxaCtx1 = #yxa_ctx{thandler = transactionlayer:test_get_thandler_self(),
		       origin   = #siporigin{proto = yxa_test,
					     addr  = "192.0.2.11",
					     port  = 50000
					    }
		      },

    %% request(Request, YxaCtx)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "request/2 - Route - 1.0"),
    %% test request to registered location of one of our users, with Route header

    Message1 =
	"INVITE " ++ sipurl:print(Location) ++ " SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org\r\n"
	"From: Test <sip:test@example.org;tag=abc>\r\n"
	"To: Test <sip:test@example.org>\r\n"
	"Route: <" ++ sipurl:print(RouteURL) ++ ">\r\n"
	"\r\n",

    Request1 = sippacket:parse(Message1, none),

    autotest:mark(?LINE, "request/2 - Route - 1.1"),
    ok = incomingproxy:request(Request1, YxaCtx1),

    {Request1_Res, _YxaCtx1_Res11, Dst1_Res, []} = autotest_util:get_sippipe_result(),

    autotest:mark(?LINE, "request/2 - Route - 1.2"),
    %% verify results
    #request{uri = Location} = Request1_Res,
    ExpectedRoute1 = ["<" ++ sipurl:print(RouteURL) ++ ">"],
    ExpectedRoute1 = keylist:fetch('route', Request1_Res#request.header),
    route = Dst1_Res,


    autotest:mark(?LINE, "request/2 - Route - 2.0"),
    %% test BYE request with Route, can't be challenged (no ;lr)

    Message2 =
	"BYE sip:remote.example.net SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org\r\n"
	"From: Test <sip:test@example.org;tab=bcd>\r\n"
	"To: Test <sip:test@example.org>\r\n"
	"Route: <sip:192.0.2.67>\r\n"
	"\r\n",

    Request2 = sippacket:parse(Message2, none),

    autotest:mark(?LINE, "request/2 - Route - 2.1"),
    ok = incomingproxy:request(Request2, YxaCtx1),

    {Request2_Res, _YxaCtx1_Res21, Dst2_Res, []} = autotest_util:get_sippipe_result(),

    autotest:mark(?LINE, "request/2 - Route - 2.2"),
    %% verify results. the push-requri-to-Route for Route header without ;lr
    %% is done in sippipe, so it isn't visible here
    "sip:remote.example.net" = sipurl:print(Request2_Res#request.uri),
    route = Dst2_Res,
    ["<sip:192.0.2.67>"] = keylist:fetch('route', Request2_Res#request.header),


    autotest:mark(?LINE, "request/2 - Route - 3.0"),
    %% test non-BYE with remote Route destination, and remote Request-URI - should be challenged

    Message3 =
	"TEST sip:remote.example.net SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org\r\n"
	"From: Test <sip:test@example.org;tab=bcd>\r\n"
	"To: Test <sip:test@example.org>\r\n"
	"Route: <sip:192.0.2.67>\r\n"
	"\r\n",

    Request3 = sippacket:parse(Message3, none),

    autotest:mark(?LINE, "request/2 - Route - 3.1"),
    ok = incomingproxy:request(Request3, YxaCtx1),

    autotest:mark(?LINE, "request/2 - Route - 3.2"),
    %% verify results
    {407, "Proxy Authentication Required", [{"Proxy-Authenticate", _}], <<>>} =
	autotest_util:get_created_response(),

    mnesia:abort(ok).


test_pubsub_routing() ->
    autotest:mark(?LINE, "request/2 - Route - 0.0"),

    case mnesia:transaction(fun test_pubsub_routing2/0) of
	{aborted, ok} ->
	    ok;
	{aborted, Res} ->
	    {error, Res}
    end.

test_pubsub_routing2() ->
    autotest:mark(?LINE, "request/2 - PubSub routing - 0.1"),

    EventServerURL = sipurl:parse("sip:192.0.2.155"),
    HomedomainStr = "example.org",

    ExtraCfg = [
		{userdb_modules,		[sipuserdb_test]},
		{myhostnames,			["autotest.example.org"]},
		{homedomain,			[HomedomainStr]},
		{eventserver_for_package,	[{"unittest", sipurl:print(EventServerURL)}
						]}
	       ],
    %% must use init, can't use set - set won't normalize values and we expect
    %% URLs in eventserver_for_package to be normalized
    ok = yxa_test_config:init(incomingproxy, ExtraCfg),

    YxaCtx1 = #yxa_ctx{thandler = transactionlayer:test_get_thandler_self(),
		       origin   = #siporigin{proto = yxa_test,
					     addr  = "192.0.2.11",
					     port  = 50000
					    }
		      },

    %% request(Request, YxaCtx)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "request/2 - PubSub routing - 1.0"),
    %% test SUBSCRIBE request to a user in one of our homedomains

    HomedomainURL = sipurl:parse("sip:" ++ HomedomainStr),
    HomeUser = sipurl:set([{user, "autotest2"}], HomedomainURL),

    Message1 =
	"SUBSCRIBE " ++ sipurl:print(HomeUser) ++ " SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org\r\n"
	"From: Remote User <sip:test@remote.example.org;tag=abc>\r\n"
	"To: Test <sip:test@example.org>\r\n"
	"Event: unittest;foo\r\n"
	"\r\n",

    Request1 = sippacket:parse(Message1, none),
    autotest:mark(?LINE, "request/2 - PubSub routing - 1.1"),
    ok = incomingproxy:request(Request1, YxaCtx1),
    {Request1_Res, _YxaCtx1_Res11, Dst1_Res, []} = autotest_util:get_sippipe_result(),

    autotest:mark(?LINE, "request/2 - PubSub routing - 1.2"),
    %% verify results

    Request1_Expected =
	#request{method = "SUBSCRIBE",
		 uri    = HomeUser,
		 body   = <<>>
		},
    ok = test_compare_records(Request1_Res, Request1_Expected, [header]),

    [FirstDst1_Res] = Dst1_Res,
    SipDst1_Expected =
	#sipdst{proto = udp,
		addr  = "192.0.2.155",
		port  = 5060,
		uri = HomeUser
	       },
    ok = test_compare_records(FirstDst1_Res, SipDst1_Expected, []),


    ok = yxa_test_config:stop(),
    mnesia:abort(ok).


test_regexp_routing() ->
    autotest:mark(?LINE, "request/2 - Regexp routing - 0.0"),

    case mnesia:transaction(fun test_regexp_routing2/0) of
	{aborted, ok} ->
	    ok;
	{aborted, Res} ->
	    {error, Res}
    end.

test_regexp_routing2() ->
    autotest:mark(?LINE, "request/2 - regexp routing - 0.1"),
    %% test a request to an address that is present in our regexp rewriting database

    HomedomainStr = "example.org",

    ExtraCfg = [
		{userdb_modules,		[sipuserdb_test]},
		{myhostnames,			["autotest.example.org"]},
		{homedomain,			[HomedomainStr]}
	       ],
    ok = yxa_test_config:init(incomingproxy, ExtraCfg),

    YxaCtx1 = #yxa_ctx{thandler = transactionlayer:test_get_thandler_self(),
		       origin   = #siporigin{proto = yxa_test,
					     addr  = "192.0.2.11",
					     port  = 60000
					    }
		      },

    %% set up a regexp route
    Flags = [],
    Class = test,
    Expire = util:timestamp() + 99,
    Regexp1_Output_URL = sipurl:parse("sip:regexp2@" ++ HomedomainStr),
    {atomic, ok} = database_regexproute:insert("^sip:regexp1@", Flags, Class, Expire,
					       sipurl:print(Regexp1_Output_URL)
					      ),

    %% request(Request, YxaCtx)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "request/2 - regexp routing - 1.0"),

    Message1 =
	"MESSAGE sip:regexp1@" ++ HomedomainStr ++ " SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org\r\n"
	"From: Remote User <sip:test@remote.example.org;tag=abc>\r\n"
	"To: Test <sip:regexp1@example.org>\r\n"
	"CSeq: 31000 MESSAGE\r\n"
	"\r\n",

    Request1 = sippacket:parse(Message1, none),
    autotest:mark(?LINE, "request/2 - regexp routing routing - 1.1"),
    ok = incomingproxy:request(Request1, YxaCtx1),
    {Request1_Res, _YxaCtx1_Res11, Dst1_Res, []} = autotest_util:get_sippipe_result(),

    autotest:mark(?LINE, "request/2 - regexp routing - 1.2"),
    %% verify results

    ok = test_compare_records(Request1_Res, Request1, []),
    Dst1_Expected = Regexp1_Output_URL,
    ok = test_compare_records(Dst1_Res, Dst1_Expected, []),

    %% Here, we pretend that sippipe was started and that it started a client transaction
    %% that handed the request over to the transport layer.

    TestSipSocket = #sipsocket{proto = yxa_test},
    TestSipDst1 =
	#sipdst{addr  = "192.0.2.99",
		port  = 5999,
		proto = yxa_test,
		uri   = Regexp1_Output_URL
	       },
    {ok, _SipSocket_Res1, Branch_Res1} =
	transportlayer:send_proxy_request(TestSipSocket, Request1, TestSipDst1, []),

    autotest:mark(?LINE, "request/2 - regexp routing - 1.3"),
    {ok, SentMessage1} = test_get_sipsocket_sent_message(TestSipDst1),

    SentRequest1 = sippacket:parse(SentMessage1, none),
    Regexp1_Output_URL = SentRequest1#request.uri,

    ok = yxa_test_config:stop(),
    mnesia:abort(ok).

test_get_sipsocket_sent_message(Dst) when is_record(Dst, sipdst) ->
    #sipdst{addr = Addr,
	    port = Port,
	    proto = Proto
	   } = Dst,
    receive
	{sipsocket_test, send, {Proto, Addr, Port}, Message} ->
	    {ok, Message}
    after
	1000 ->
	    {error, "Did not find expected sent message in mailbox"}
    end.


%% compare two records element by element and give good information on where they
%% are not equal
test_compare_records(R1, R2, ShouldChange) when is_tuple(R1), is_tuple(R2), is_list(ShouldChange) ->
    RecName = element(1, R1),
    Fields = test_record_info(RecName),
    autotest_util:compare_records(R1, R2, ShouldChange, Fields).

%% add more records here when needed
test_record_info(request) ->	record_info(fields, request);
test_record_info(sipdst) ->	record_info(fields, sipdst);
test_record_info(sipurl) ->	record_info(fields, sipurl).

-endif.
