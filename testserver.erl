-module(testserver).
-export([init/0, request/6, response/6]).

init() ->
    database_call:create([node()]),
    [[fun request/6, fun response/6], none, stateful, {append, []}].

localhostname(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(myhostnames)).

request(Method, URL, Header, Body, Socket, FromIP) ->
    Request = {Method, URL, Header, Body},
    THandler = transactionlayer:get_handler_for_request(Request),
    LogTag = get_branch_from_handler(THandler),
    case Method of
        "REGISTER" ->
            process_request(Method, URL, Header, Body, Socket, LogTag);
	"INVITE" ->
	    packet_check_ok(Header, LogTag),
	    process_request(Method, URL, Header, Body, Socket, LogTag);
	"ACK" ->
	    process_request(Method, URL, Header, Body, Socket, LogTag);
	"CANCEL" ->
	    process_request(Method, URL, Header, Body, Socket, LogTag);
	"BYE" ->
	    process_request(Method, URL, Header, Body, Socket, LogTag);
	_ ->
	    logger:log(normal, "~s -- NOT IMPLEMENTED", [LogTag]),
	    transactionlayer:send_response_handler(THandler, 501, "Not Implemented")
    end.

process_request("REGISTER", URI, Header, Body, Socket, LogTag) ->
    {User, Pass, Host, Port, Parameters} = URI,
    Request = {"REGISTER", URI, Header, Body},
    case localhostname(Host) of
	true ->
	    %% delete any present Record-Route header (RFC3261, #10.3)
	    NewHeader = keylist:delete("Record-Route", Header),
	    Contacts = sipheader:contact(keylist:fetch("Contact", NewHeader)),
	    logger:log(debug, "Register: Contact(s) ~p", [sipheader:contact_print(Contacts)]),
	    transactionlayer:send_response_request(Request, 200, "OK",
						   [{"Expires", ["0"]},
						    {"Contacts", sipheader:contact_print(Contacts)}]
						  );
	_ ->
	    logger:log(normal, "~s: REGISTER for non-homedomain ~p", [LogTag, Host]),
	    transactionlayer:send_response_request(Request, 501, "Not Implemented")
    end;

process_request("INVITE", URI, Header, Body, Socket, LogTag) ->
    siprequest:send_result(Header, Socket, "", 100, "Trying"),
    Request = {"INVITE", URI, Header, Body},
    case get_user(URI) of
	{404, Reason} ->
	    logger:log(normal, "~s: Testserver classic response: '404 ~p'", [LogTag, Reason]),
	    transactionlayer:send_response_request(Request, 404, Reason);
	{Status, Reason} ->
	    logger:log(normal, "~s: Testserver response: '~p ~s'", [LogTag, Status, Reason]),
	    transactionlayer:send_response_request(Request, Status, Reason)
    end;

process_request(Method, URI, Header, Body, Socket, LogTag) ->
    logger:log(normal, "~s: testserver: ~s ~s dropped", [LogTag, Method, sipurl:print(URI)]),
    true.

get_user(URI) ->
    Key = sipurl:print(URI),
    Res = regexp_locate_user(Key, sipserver:get_env(user_db, "")),
    logger:log(debug, "Locate user: ~s -> ~p", [Key, Res]),
    case Res of
        nomatch ->
	    {404, "Not Found"};
        Res ->
	    Res
    end.

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

response(Status, Reason, Header, Body, Socket, FromIP) ->
    logger:log(normal, "~p ~p - dropping", [Status, Reason]),
    true.

packet_check_ok(Header, LogTag) ->
    check_no_unsupported_extension(Header, LogTag).

check_no_unsupported_extension(Header, LogTag) ->
    Require = keylist:fetch("Require", Header),
    case Require of
	[] ->
	    true;
	_ ->
	    logger:log(normal, "~s: UAS Request check: The client requires unsupported extension(s) ~p",
		       [LogTag, Require]),
	    throw({siperror, 420, "Bad Extension", [{"Unsupported", Require}]})
    end.

get_branch_from_handler(TH) ->
    CallBranch = transactionlayer:get_branch_from_handler(TH),
    case string:rstr(CallBranch, "-UAS") of
	0 ->
	    CallBranch;
	Index when integer(Index) ->
            BranchBase = string:substr(CallBranch, 1, Index - 1),
	    BranchBase
    end.
