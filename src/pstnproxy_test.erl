%%%-------------------------------------------------------------------
%%% File    : pstnproxy_test.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Test cases for the complete pstnproxy application.
%%%
%%% Created : 29 Nov 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(pstnproxy_test).

-export([test/0]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("pstnproxy.hrl").

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->
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
		{myhostnames,		["autotest.example.org"]},
		{internal_to_e164,	[{"234599", "+46234599"}]}
	       ],
    yxa_test_config:init(pstnproxy, ExtraCfg),

    test_request(),

    ok.


test_request() ->
    put({transactionlayer, get_branch_from_handler}, "test-branch"),
    put({pstnproxy, testing_sippipe}, {true, ok, self()}),


    ok = test_OPTIONS(),
    ok = test_INVITE_from_gw(),
    ok = test_INVITE_to_pstn(),
    ok.

test_OPTIONS() ->
    Cfg1 = [{myhostnames,	["test.example.org"]},
	    {pstngatewaynames,	["gw.example.org"]},
	    {userdb_modules,	[sipuserdb_test]}
	   ],

    ok = yxa_test_config:init(pstnproxy, Cfg1),

    %% request(Request, YxaCtx)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "request/2 - OPTIONS 1.0"),
    Message1 =
	"OPTIONS sip:test-NOT-FOUND@test.example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TEST one.example.org\r\n"
	"From: Test <sip:test@example.org>\r\n"
	"\r\n",

    Request1 = sippacket:parse(Message1, none),
    YxaCtx1 = #yxa_ctx{thandler = transactionlayer:test_get_thandler_self(),
		       origin   = #siporigin{proto = yxa_test,
					     addr  = "192.0.2.11",
					     port  = 50000
					    }
		      },

    autotest:mark(?LINE, "request/2 - OPTIONS 1.1"),
    %% test OPTIONS request to unknown user at this proxy
    ok = pstnproxy:request(Request1, YxaCtx1),

    {404, "Not Found", [], <<>>} = get_created_response(),

    autotest:mark(?LINE, "request/2 - OPTIONS 2.0"),
    Message2 =
	"OPTIONS sip:test.example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TEST one.example.org\r\n"
	"From: Test <sip:test@example.org>\r\n"
	"\r\n",

    Request2 = sippacket:parse(Message2, none),

    autotest:mark(?LINE, "request/2 - OPTIONS 2.2"),
    %% test OPTIONS request to unknown user at this proxy
    ok = pstnproxy:request(Request2, YxaCtx1),

    {200, "OK", [], <<>>} = get_created_response(),

    yxa_test_config:stop(),
    ok.


test_INVITE_from_gw() ->
    Cfg1 = [{myhostnames,	["test.example.org"]},
	    {pstngatewaynames,	["gw.example.org", "192.0.2.33"]},
	    {userdb_modules,	[sipuserdb_test]}
	   ],

    ok = yxa_test_config:init(pstnproxy, Cfg1),

    %% request(Request, YxaCtx)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "request/2 - INVITE from gw 1.0"),
    Message1 =
	"INVITE sip:contact@somewhere-else.example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TEST gw.example.org\r\n"
	"From: Test <sip:test@example.org>\r\n"
	"Route: <sip:192.0.2.11;lr>\r\n"
	"\r\nbody",

    Request1 = sippacket:parse(Message1, none),
    YxaCtx1 = #yxa_ctx{thandler = transactionlayer:test_get_thandler_self(),
		       origin   = #siporigin{proto = yxa_test,
					     addr  = "192.0.2.33",
					     port  = 50000
					    }
		      },

    autotest:mark(?LINE, "request/2 - INVITE from gw 1.1"),
    %% test INVITE with Route received from gateway, should be passed on
    ok = pstnproxy:request(Request1, YxaCtx1),

    autotest:mark(?LINE, "request/2 - INVITE from gw 1.2"),
    %% verify result
    {Request1, _SentCtx1, route, AppData1} = get_sippipe_result(),
    [#pstn_ctx{} = PstnCtx1] = AppData1,
    [from_gateway, has_route] = lists:sort(PstnCtx1#pstn_ctx.tags),
    "192.0.2.33" = PstnCtx1#pstn_ctx.ip,

    yxa_test_config:stop(),
    ok.


test_INVITE_to_pstn() ->
    Cfg1 = [{myhostnames,	["test.example.org"]},
	    {pstngatewaynames,	["gw.example.org", "192.0.2.33"]},
	    {userdb_modules,	[sipuserdb_test]},
	    {classdefs,		[{"^\\+123456$",	national},
				 {"^\\+1111$",		free}
				]},
	    {sipauth_unauth_classlist,	[free]},
	    {e164_to_pstn,	[{"(.+)",	"\\1@gw.example.org"}]}
	   ],

    ok = yxa_test_config:init(pstnproxy, Cfg1),

    %% request(Request, YxaCtx)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "request/2 - INVITE to pstn 1.0"),
    Message1 =
	"INVITE sip:+123456@test.example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TEST gw.example.org\r\n"
	"From: Test <sip:test@remote.example.org>\r\n"
	"To: E164-national <sip:e164-national@example.org>\r\n"
	"\r\nbody",

    Request1 = sippacket:parse(Message1, none),
    YxaCtx1 = #yxa_ctx{thandler = transactionlayer:test_get_thandler_self(),
		       origin   = #siporigin{proto = yxa_test,
					     addr  = "192.0.2.9",
					     port  = 50000
					    }
		      },

    autotest:mark(?LINE, "request/2 - INVITE to pstn 1.1"),
    %% test INVITE to PSTN without credentials
    ok = pstnproxy:request(Request1, YxaCtx1),

    {407, "Proxy Authentication Required", [{"Proxy-Authenticate", [_]}], <<>>} = get_created_response(),


    autotest:mark(?LINE, "request/2 - INVITE to pstn 2.0"),
    Message2 =
	"INVITE sip:+1111@test.example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TEST gw.example.org\r\n"
	"From: Test <sip:test@remote.example.org>\r\n"
	"To: E264-national <sip:e264-national@example.org>\r\n"
	"\r\nbody",

    Request2 = sippacket:parse(Message2, none),
    YxaCtx2 = #yxa_ctx{thandler = transactionlayer:test_get_thandler_self(),
		       origin   = #siporigin{proto = yxa_test,
					     addr  = "192.0.2.9",
					     port  = 50000
					    }
		      },

    autotest:mark(?LINE, "request/2 - INVITE to pstn 2.2"),
    %% test INVITE to free PSTN destination
    ok = pstnproxy:request(Request2, YxaCtx2),

    autotest:mark(?LINE, "request/2 - INVITE to pstn 2.3"),
    DstUrl2 = sipurl:parse("sip:+1111@gw.example.org"),
    {Request2, _YxaCtx2_res, DstUrl2, AppData2} = get_sippipe_result(),
    [#pstn_ctx{} = PstnCtx2] = AppData2,
    [] = lists:sort(PstnCtx2#pstn_ctx.tags),
    "192.0.2.9" = PstnCtx2#pstn_ctx.ip,


    yxa_test_config:stop(),
    ok.

get_created_response() ->
    receive
	{'$gen_cast', {create_response, Status, Reason, EH, Body}} ->
	    {Status, Reason, EH, Body}
    after 0 ->
	    {error, "no created response in my mailbox"}
    end.

get_sippipe_result() ->
    receive
	{start_sippipe, Res} ->
	    Res
    after 0 ->
	    {error, "no created response in my mailbox"}
    end.

