%%%-------------------------------------------------------------------
%%% File    : outgoingproxy_test.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Test cases for the complete outgoingproxy application.
%%%
%%% @since    29 Nov 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @hidden
%%%-------------------------------------------------------------------
-module(outgoingproxy_test).

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
test() ->
    autotest:mark(?LINE, "outgoingproxy_test setup - 0"),

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
		{userdb_modules,	[sipuserdb_test]},
		{myhostnames,		["autotest.example.org"]}
	       ],
    yxa_test_config:init(outgoinproxy, ExtraCfg),

    ok = test_request(),

    ok.

test_request() ->
    autotest:store_unit_test_result(outgoingproxy, testing_sippipe, {ok, self()}),
    autotest:store_unit_test_result(transactionlayer, get_branch_from_handler, "test-branch"),

    ok = test_route(),

    ok.


test_route() ->
    autotest:mark(?LINE, "request/2 - Route - 0.0"),

    phone:test_create_table(),
    database_gruu:test_create_table(),

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
    ok = outgoingproxy:request(Request1, YxaCtx1),

    {Request1_Res, _YxaCtx1_Res11, Dst1_Res, []} = get_sippipe_result(),

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
    ok = outgoingproxy:request(Request2, YxaCtx1),

    {Request2_Res, _YxaCtx1_Res21, Dst2_Res, []} = get_sippipe_result(),

    autotest:mark(?LINE, "request/2 - Route - 2.2"),
    %% verify results. the push-requri-to-Route for Route header without ;lr
    %% is done in sippipe, so it isn't visible here
    "sip:remote.example.net" = sipurl:print(Request2_Res#request.uri),
    route = Dst2_Res,
    ["<sip:192.0.2.67>"] = keylist:fetch('route', Request2_Res#request.header),


    autotest:mark(?LINE, "request/2 - Route - 3.0"),
    %% test non-BYE with remote Route destination, and remote Reqyest-URI - should be challenged

    Message3 =
	"TEST sip:remote.example.net SIP/2.0\r\n"
	"Via: SIP/2.0/YXA-TEST one.example.org\r\n"
	"From: Test <sip:test@example.org;tab=bcd>\r\n"
	"To: Test <sip:test@example.org>\r\n"
	"Route: <sip:192.0.2.67>\r\n"
	"\r\n",

    Request3 = sippacket:parse(Message3, none),

    autotest:mark(?LINE, "request/2 - Route - 3.1"),
    ok = outgoingproxy:request(Request3, YxaCtx1),

    autotest:mark(?LINE, "request/2 - Route - 3.2"),
    %% verify results
    {407, "Proxy Authentication Required", [{"Proxy-Authenticate", _}], <<>>} = get_created_response(),

    mnesia:abort(ok).



%%====================================================================
%% Helper functions
%%====================================================================

get_created_response() ->
    receive
	{'$gen_cast', {create_response, Status, Reason, EH, Body}} ->
	    {Status, Reason, EH, Body};
	M ->
	    Msg = io_lib:format("Test: Unknown signal found in process mailbox :~n~p~n~n", [M]),
	    {error, lists:flatten(Msg)}
    after 0 ->
	    {error, "no created response in my mailbox"}
    end.

get_sippipe_result() ->
    receive
	{start_sippipe, Res} ->
	    Res;
	M ->
	    Msg = io_lib:format("Test: Unknown signal found in process mailbox :~n~p~n~n", [M]),
	    {error, lists:flatten(Msg)}
    after 0 ->
	    {error, "no sippipe data in my mailbox"}
    end.


add_valid_credentials(MethodName, Request, User) ->
    Password = sipuserdb:get_password_for_user(User),
    add_valid_credentials(MethodName, Request, User, Password).

add_valid_credentials(MethodName, Request, User, Password) ->
    true = is_list(Password),
    NewHeader =
	sipauth:add_credentials(digest, MethodName,
				Request#request.method, Request#request.uri,
				Request#request.header, User, Password),
    Request#request{header = NewHeader}.
