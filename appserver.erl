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
    case database_call:get_call(CallID) of
	{atomic, [Call]} ->
	    {Origheaders, Pid} = Call,
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
    case database_call:get_call(CallID) of
	{atomic, [{proxy, Origheaders, Pid}]} ->
	    logger:log(normal, "~s -> PID ~p", [LogStr, Pid]),
	    Pid ! {cancel, {URI, Header, Body}};
	{atomic, []} ->
	    logger:log(normal, "~s -> 481 Call/Transaction Does Not Exist", [LogStr]),
	    siprequest:send_result(Header, Socket, "", 481, "Call/Transaction Does Not Exist")
    end;

request("INVITE", URI, Header, Body, Socket, FromIP) ->
    sipproxy:start("INVITE", URI, Header, Body, Socket, FromIP).

response(Status, Reason, Header, Body, Socket, FromIP) ->
%    logger:log(normal, "status:~p", [Status]),
    LogStr = sipserver:make_logstr({response, Status, Reason, Header, Body}, FromIP),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [{answer, Origheaders, [Pid]}]} ->
	    logger:log(normal, "Response to ~s: ~p ~s -> PID ~p", [LogStr, Status, Reason, Pid]),
	    Pid ! {siprequest, status, Status, Header, Socket, Body, Origheaders};
	{atomic, [{proxy, Origheaders, Pid}]} ->
	    logger:log(normal, "Response to ~s: ~p ~s -> PID ~p", [LogStr, Status, Reason, Pid]),
	    Pid ! {response, {Status, Reason, Header, Body}};
	{atomic, []} ->
	    logger:log(normal, "Response to ~s: ~p ~s -> Call not found", [LogStr, Status, Reason])
    end.
