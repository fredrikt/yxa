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
	    siprequest:send_result(Header, Socket, "", 200, "OK");
	{atomic, []} ->
	    logger:log(debug, "Call not found", [])
    end;

request("ACK", {User, Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    true;

request("CANCEL", URI, Header, Body, Socket) ->
    logger:log(normal, "CANCEL", []),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [{proxy, Origheaders, Pid}]} ->
	    Pid ! {cancel, {URI, Header, Body}};
	{atomic, []} ->
	    logger:log(debug, "Call not found", [])
    end;

request("INVITE", {"messages", Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    case sipanswer:start(Header, Body, start, none, none) of
	{ok, Replybody} ->
	    logger:log(debug, "body:~p", [Replybody]),
	    siprequest:send_answer(Header, Socket, Replybody);
	{error, _} ->
	    true
    end;

request("INVITE", {"fork", Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    sipproxy:start("INVITE", {"fork", Pass, Host, Port, Parameters},
		   Header, Body, Socket);

request("INVITE", {"bounce", Pass, Host, Port, Parameters}, Header, Body, Socket) ->
    case sipanswer:bounce(Header, Body, start, none, none) of
	{ok, Replybody} ->
	    logger:log(debug, "body:~p", [Replybody]),
	    siprequest:send_answer(Header, Socket, Replybody);
	{error, _} ->
	    true
    end.

response(Status, Reason, Header, Body, Socket) ->
%    logger:log(normal, "status:~p", [Status]),
    [CallID] = keylist:fetch("Call-ID", Header),
    case database_call:get_call(CallID) of
	{atomic, [{answer, Origheaders, [Pid]}]} ->
	    Pid ! {siprequest, status, Status, Header, Socket, Body, Origheaders};
	{atomic, [{proxy, Origheaders, Pid}]} ->
	    Pid ! {response, {Status, Header, Body}};
	{atomic, []} ->
	    logger:log(debug, "Call not found", [])
    end.
