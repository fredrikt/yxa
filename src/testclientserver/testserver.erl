-module(testserver).

%% Standard YXA SIP-application exports
-export([init/0,
	 request/2,
	 response/2
	]).

-include("siprecords.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%%% Standard YXA SIP-application exported functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     YXA applications must export an init/0 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    database_call:create([node()]),
    #yxa_app_init{}.


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) -> ok
%%
%% @doc     YXA applications must export a request/2 function.
%% @end
%%--------------------------------------------------------------------
request(Request, YxaCtx) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    THandler = YxaCtx#yxa_ctx.thandler,
    LogTag = transactionlayer:get_branchbase_from_handler(THandler),
    case Request#request.method of
        "REGISTER" ->
            process_request(Request, LogTag);
	_ when Request#request.method == "INVITE"; Request#request.method == "MESSAGE" ->
	    packet_check_ok(Request#request.header, LogTag),
	    process_request(Request, LogTag);
	"ACK" ->
	    process_request(Request, LogTag);
	"CANCEL" ->
	    process_request(Request, LogTag);
	"BYE" ->
	    process_request(Request, LogTag);
	_ ->
	    logger:log(normal, "~s -- NOT IMPLEMENTED", [LogTag]),
	    transactionlayer:send_response_handler(THandler, 501, "Not Implemented")
    end,
    ok.

%%--------------------------------------------------------------------
%% @spec    (Response, YxaCtx) -> ok
%%
%% @doc     YXA applications must export a response/2 function.
%% @end
%%--------------------------------------------------------------------
response(Response, YxaCtx) when is_record(Response, response), is_record(YxaCtx, yxa_ctx) ->
    logger:log(normal, "~p ~p - dropping", [Response#response.status, Response#response.reason]),
    ok.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%
%% REGISTER
%%
process_request(Request, LogTag) when record(Request, request), Request#request.method == "REGISTER" ->
    URI = Request#request.uri,
    case localhostname(URI#sipurl.host) of
	true ->
	    Contacts = sipheader:contact(Request#request.header),
	    logger:log(debug, "Register: Contact(s) ~p", [sipheader:contact_print(Contacts)]),
	    transactionlayer:send_response_request(Request, 200, "OK",
						   [{"Expires", ["0"]},
						    {"Contacts", sipheader:contact_print(Contacts)}]
						  );
	_ ->
	    logger:log(normal, "~s: REGISTER for non-homedomain ~p", [LogTag, URI#sipurl.host]),
	    transactionlayer:send_response_request(Request, 501, "Not Implemented")
    end;

%%
%% INVITE or MESSAGE
%%
process_request(Request, LogTag) when record(Request, request), Request#request.method == "INVITE";
Request#request.method == "MESSAGE" ->
    case get_user(Request#request.uri) of
	{404, Reason} ->
	    logger:log(normal, "~s: Testserver classic response: '404 ~p'", [LogTag, Reason]),
	    transactionlayer:send_response_request(Request, 404, Reason);
	{Status, Reason} ->
	    logger:log(normal, "~s: Testserver response: '~p ~s'", [LogTag, Status, Reason]),
	    transactionlayer:send_response_request(Request, Status, Reason);
	nomatch ->
	    {User, _, _, _, _} = Request#request.uri,
	    S = lists:flatten(io_lib:format("Busy Here (~s)", [User])),
	    logger:log(normal, "~s: Testserver built-in response: '486 ~s'", [LogTag, S]),
	    transactionlayer:send_response_request(Request, 486, S)
    end;

%%
%% Anything but REGISTER, INVITE or MESSAGE
%%
process_request(Request, LogTag) when record(Request, request) ->
    logger:log(normal, "~s: testserver: ~s ~s dropped",
	       [LogTag, Request#request.method, sipurl:print(Request#request.uri)]),
    true.


get_user(URI) ->
    Key = sipurl:print(URI),
    UserDb = yxa_config:get_env(testserver_userdb),
    Res = regexp_locate_user(Key, UserDb),
    logger:log(debug, "Locate user: ~s -> ~p", [Key, Res]),
    Res.


regexp_locate_user(_Input, []) ->
    nomatch;
regexp_locate_user(Input, [{Regexp, Code, Text} | Rest]) ->
    case regexp:match(Input, Regexp) of
	{match, _, _} ->
	    {Code, Text};
	nomatch ->
	    regexp_locate_user(Input, Rest);
	{error, Error} ->
	    logger:log(normal, "Error in regexp ~p: ~p", [Regexp, Error]),
	    []
    end.


packet_check_ok(Header, LogTag) ->
    check_no_unsupported_extension(Header, LogTag).


check_no_unsupported_extension(Header, LogTag) ->
    Require = keylist:fetch('require', Header),
    case Require of
	[] ->
	    true;
	_ ->
	    logger:log(normal, "~s: UAS Request check: The client requires unsupported extension(s) ~p",
		       [LogTag, Require]),
	    throw({siperror, 420, "Bad Extension", [{"Unsupported", Require}]})
    end.

localhostname(Hostname) ->
    LC = http_util:to_lower(Hostname),
    {ok, MyHostnames} = yxa_config:get_env(myhostnames),
    lists:member(LC, MyHostnames).
