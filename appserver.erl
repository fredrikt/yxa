-module(appserver).
-export([start/2]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/6,
				   fun response/6, [phone], false]),
    {ok, Pid}.

init() ->
    true. %database_call:create_call().

request("BYE", URI, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({request, "BYE", URI, Header, Body}, FromIP),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call_type(CallID, proxy) of
	{atomic, [{Origheaders, Pid}]} ->
	    logger:log(normal, "~s -> PID ~p", [LogStr, Pid]),
	    Pid ! {siprequest, bye},
	    siprequest:send_result(Header, Socket, "", 200, "OK");
	{atomic, []} ->
	    logger:log(normal, "~s -> 481 Call/Transaction Does Not Exist", [LogStr]),
	    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist")
    end;

request("ACK", {User, Pass, Host, Port, Parameters}, Header, Body, Socket, FromIP) ->
    true;

request("CANCEL", URI, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({request, "CANCEL", URI, Header, Body}, FromIP),
    logger:log(debug, "CANCEL", []),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call_type(CallID, proxy) of
	{atomic, [{Origheaders, Pid}]} ->
	    logger:log(normal, "~s -> PID ~p", [LogStr, Pid]),
	    Pid ! {cancel, {URI, Header, Body}};
	{atomic, []} ->
	    logger:log(normal, "~s -> 481 Call/Transaction Does Not Exist", [LogStr]),
	    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist")
    end;

request("INVITE", URI, Header, Body, Socket, FromIP) ->
    % create header suitable for answering the incoming request
    AnswerHeader = siprequest:make_answerheader(Header),
    logger:log(normal, "Forking: Sending 100 Trying", []),
    siprequest:send_result(AnswerHeader, Socket, "", 100, "Trying"),
    Key = local:sipuser(URI),
    Actions = get_actions(URI),
    case Actions of
	nomatch ->
	    logger:log(normal, "Appserver: No actions found for ~p, returning 404 Not Found", [Key]),
	    siprequest:send_notfound(AnswerHeader, Socket);
	_ ->
	    logger:log(debug, "Appserver: User ~p actions :~n~p", [Key, Actions]),
	    sipproxy:start("INVITE", URI, Header, AnswerHeader, Body, Socket, FromIP, Actions)
    end;

request(Method, URI, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({request, Method, URI, Header, Body}, FromIP),
    logger:log(normal, "~s -> 501 Not Implemented", [LogStr]),
    siprequest:send_result(Header, Socket, "", 501, "Not Implemented").


response(Status, Reason, Header, Body, Socket, FromIP) ->
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call_type(CallID, proxy) of
	{atomic, [{Origheaders, Pid}]} ->
	    logger:log(normal, "Response to ~s: ~p ~s -> PID ~p", [LogStr, Status, Reason, Pid]),
	    Pid ! {response, {Status, Reason, Header, Body}};
	{atomic, []} ->
	    logger:log(normal, "Response to ~s: ~p ~s -> Call not found (ignoring)", [LogStr, Status, Reason])
    end.

get_actions(URI) ->
    Key = local:sipuser(URI),
    case fetch_actions(Key) of
	none ->
	    {User, Pass, Host, Port, Parameters} = URI,
	    case util:isnumeric(User) of
		true ->
		    case fetch_actions(User) of
			none -> nomatch;
			Actions -> lists:append(Actions, [{wait, 40}])
		    end;
		_ ->
		    nomatch
	    end;
	Actions ->
	    lists:append(Actions, [{wait, 40}])
    end.

fetch_actions(Key) ->
    case phone:get_phone(Key) of
    	{atomic, []} ->
	    none;
	{atomic, Locations} ->
	    BestLocations = local:prioritize_locations(Key, Locations),
	    locations_to_actions(BestLocations)
    end.

locations_to_actions([]) ->
    [];
locations_to_actions([{Location, Flags, Class, Expire}]) ->
    [{call, 30, Location}];
locations_to_actions([{Location, Flags, Class, Expire} | Rest]) ->
    Actions = locations_to_actions(Rest),
    lists:append([{call, 30, Location}], Actions).

