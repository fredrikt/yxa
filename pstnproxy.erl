-module(pstnproxy).
-export([start/2]).

start(normal, Args) ->
    Pid = spawn(sipserver, start, [fun init/0, fun request/5,
				   fun response/5, [user, numbers], false]),
    {ok, Pid}.

init() ->
    true.

request(Method, {User, Pass, "sip-pstn.kth.se", Port, Parameters}, Header, Body, Socket) ->
    logger:log(normal, Method),
    case Method of
	"INVITE" ->
	    request2(Method, User, Header, Body, Socket);
	"ACK" ->
	    request2(Method, User, Header, Body, Socket);
	"CANCEL" ->
	    request2(Method, User, Header, Body, Socket);
	"BYE" ->
	    request2(Method, User, Header, Body, Socket);
	_ ->
	    siprequest:send_result(Header, Socket, "", 501, "Not Implemented")
    end;

request(Method, URL, Header, Body, Socket) ->
    {User, Pass, Host, Port, Parameters} = URL,
    Newlocation = {User, none, "kth.se", none, []},
    siprequest:send_proxy_request(Header, Socket, {Method, Newlocation, Body}).

request2(Method, Phone, Header, Body, Socket) ->
    Newlocation = {Phone, none, sipserver:get_env(proxyaddr), "5060", []},
    {_, FromURI} = sipheader:to(keylist:fetch("From", Header)),
    {Fromphone, _, _, _, _} = FromURI,
    Classdefs = sipserver:get_env(classdefs, [{"", unknown}]),
    sipauth:check_and_send_auth(Header, Socket, Fromphone, Phone,
				{siprequest, send_proxy_request},
				{Method, Newlocation, Body},
				Method, Classdefs).

response(Status, Reason, Header, Body, Socket) ->
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).
