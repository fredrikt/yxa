-module(appserver).
-export([start/2]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/5,
				   fun response/5, none, true]),
    {ok, Pid}.

init() ->
    database_call:create_call().

request("BYE", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [Call]} ->
	    {Origheaders, Pid} = Call,
	    Pid ! {siprequest, bye},
	    siprequest:send_result(Header, Socket, "", 200, "OK")
    end;

request("ACK", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    true;

request("CANCEL", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    true;

request("INVITE", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    case sipanswer:start(Header, Body, bounce, none, none) of
	{ok, Replybody} ->
	    logger:log(debug, "body:~p", [Replybody]),
	    siprequest:send_answer(Header, Socket, Replybody);
	{error, _} ->
	    true
    end.

response(Status, Reason, Header, Body, Socket) ->
    logger:log(normal, "status:~p", [Status]),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [Call]} ->
	    {Origheaders, Pid} = Call,
	    Pid ! {siprequest, status, Status, Header, Socket, Body, Origheaders}
    end.
