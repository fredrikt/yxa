-module(testserver).
-export([start/2, list_calls_in_database/0]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, none, true]),
    {ok, Pid}.

init() ->
    timer:apply_interval(10000, ?MODULE, list_calls_in_database, []),
    database_call:create_call().

localhostname(Hostname) ->
    util:casegrep(Hostname, sipserver:get_env(myhostnames)).

request(Method, URL, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({request, Method, URL, Header, Body}, FromIP),
    case Method of
        "REGISTER" ->
            process_request(Method, URL, Header, Body, Socket, LogStr);
	"INVITE" ->
	    packet_check_ok(Header),
	    process_request(Method, URL, Header, Body, Socket, LogStr);
	"ACK" ->
	    process_request(Method, URL, Header, Body, Socket, LogStr);
	"CANCEL" ->
	    process_request(Method, URL, Header, Body, Socket, LogStr);
	"BYE" ->
	    process_request(Method, URL, Header, Body, Socket, LogStr);
	_ ->
	    logger:log(normal, "~s -- NOT IMPLEMENTED", [LogStr]),
	    siprequest:send_result(Header, Socket, "", 501, "Not Implemented")
    end.

process_request("REGISTER", URI, Header, Body, Socket, LogStr) ->
    {User, Pass, Host, Port, Parameters} = URI,
    case localhostname(Host) of
	true ->
	    % delete any present Record-Route header (RFC3261, #10.3)
	    NewHeader = keylist:delete("Record-Route", Header),
	    Contacts = sipheader:contact(keylist:fetch("Contact", NewHeader)),
	    logger:log(debug, "Register: Contact(s) ~p", [sipheader:contact_print(Contacts)]),
	    siprequest:send_result(NewHeader, Socket, "", 200, "OK",
	    			   [{"Expires", ["0"]},
				    {"Contacts", sipheader:contact_print(Contacts)}
				   ]
				  );
	_ ->
	    logger:log(debug, "REGISTER for non-homedomain ~p", [Host]),
	    siprequest:send_result(Header, Socket, "", 501, "Not Implemented")
    end;

process_request("INVITE", URI, Header, Body, Socket, LogStr) ->
    siprequest:send_result(Header, Socket, "", 100, "Trying"),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:insert_call_unique(CallID, track, Header, self()) of
	{atomic, ok} ->
	    logger:log(debug, "State: Created call ~p", [CallID]),
	    case get_user(URI) of
		{404, Text} ->
		    logger:log(normal, "~s -> -- NO SUCH USER -- 404 ~p",
		    	       [LogStr, Text]),
		    siprequest:send_result(Header, Socket, "", 404, Text);
		{Code, Text} ->
		    logger:log(normal, "~s -> ~p ~s",
		    	       [LogStr, Code, Text]),
		    siprequest:send_result(Header, Socket, "", Code, Text)
	    end;
	{aborted, key_exists} ->
	    logger:log(debug, "State: Received another INVITE for call ~p - ignoring", [CallID]),
	    true
    end;

process_request("ACK", URI, Header, Body, Socket, LogStr) ->
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [{track, Origheaders, Pid}]} ->
	    case get_user(URI) of
		{404, Text} ->
		    logger:log(normal, "ACK ~s -- NO SUCH USER -- 404 ~s",
    		               [sipurl:print(URI), Text]),
		    siprequest:send_result(Header, Socket, "", 404, Text);
		{_, _} ->
		    logger:log(debug, "ACK ~s - dropping", [sipurl:print(URI)])
	    end;	
	{atomic, []} ->
	    logger:log(normal, "Call ~p not found", [CallID]),
	    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist")
    end;

process_request(Method, URI, Header, Body, Socket, LogStr) ->
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [{track, Origheaders, Pid}]} ->
	    case get_user(URI) of
		{404, Text} ->
		    logger:log(normal, "~s ~s -- NO SUCH USER -- 404 ~s",
    		               [Method, sipurl:print(URI), Text]),
		    siprequest:send_result(Header, Socket, "", 404, "Not Found");
		{_, _} ->
		    case Method of
			"CANCEL" ->
			    logger:log(debug, "~s ~s - DELETING call ~s", [Method, sipurl:print(URI), CallID]);
			"BYE" ->
			    logger:log(debug, "~s ~s - DELETING call ~s", [Method, sipurl:print(URI), CallID]);
			_ ->
			    logger:log(debug, "~s ~s - dropping", [Method, sipurl:print(URI)])
		    end
	    end;
	{atomic, []} ->
	    logger:log(normal, "Call ~p not found", [CallID]),
	    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist")
    end.
    
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

list_calls_in_database() ->
    Res = database_call:list_calls(),
    case Res of
    	{atomic, []} ->
	    logger:log(debug, "No calls in database");
	{atomic, Calls} ->
	    logger:log(debug, "Calls in database:"),
	    print_calls(Calls);
	{error, What} ->
	    logger:log(normal, "FREDRIK 2"),
	    logger:log(debug, "Could not list calls in database (error: ~p)", [What]);
	Res ->
	    logger:log(normal, "FREDRIK 1"),
	    logger:log(debug, "Could not list calls in database (~p)")
    end.

print_calls([]) ->
    [];
print_calls([{Type, CallID, Action, _, _}]) ->
    logger:log(debug, "  ~p ~p ~p", [Type, CallID, Action]);
print_calls([{Type, CallID, Action, _, _} | Rest]) ->
    logger:log(debug, "  ~p ~p ~p", [Type, CallID, Action]),
    print_calls(Rest).

packet_check_ok(Header) ->
    check_no_unsupported_extension(Header).

check_no_unsupported_extension(Header) ->
    Require = keylist:fetch("Require", Header),
    case Require of
	[] ->
	    true;
	_ ->
	    logger:log(normal, "UAS Request check: The client requires unsupported extension(s) ~p", [Require]),
	    throw({siperror, 420, "Bad Extension", [{"Unsupported", Require}]})
    end.
