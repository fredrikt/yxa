-module(transportlayer).
-export([start/1, send_proxy_request/4, send_proxy_response/2, send_result/5,
	 send_result/6]).

start(Port) when integer(Port) ->
    sipsocket:start_link(Port).    

send_proxy_response(Socket, Response) ->
    {Status, Reason, Header, Body} = Response,
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).

send_proxy_request(Socket, OrigRequest, DstURI, Parameters) ->
    siprequest:send_proxy_request(Socket, OrigRequest, DstURI, Parameters).

send_result(Header, Socket, Body, Code, Description) ->
    siprequest:send_result(Header, Socket, Body, Code, Description).

send_result(Header, Socket, Body, Code, Description, ExtraHeaders) ->
    siprequest:send_result(Header, Socket, Body, Code, Description, ExtraHeaders).
