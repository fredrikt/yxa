-module(transportlayer).
-export([start/0, send_proxy_request/4, send_proxy_response/2, send_result/5,
	 send_result/6]).

-include("siprecords.hrl").

start() ->
    sipsocket:start_link().

send_proxy_response(Socket, Response) ->
    {Status, Reason, Header, Body} = Response,
    siprequest:send_proxy_response(Socket, Status, Reason, Header, Body).

send_proxy_request(Socket, Request, DstURI, Parameters) when record(Request, request) ->
    CompatRequest = {Request#request.method, Request#request.uri, Request#request.header, Request#request.body},
    send_proxy_request(Socket, CompatRequest, DstURI, Parameters);

send_proxy_request(Socket, Request, DstURI, Parameters) ->
    siprequest:send_proxy_request(Socket, Request, DstURI, Parameters).

send_result(Header, Socket, Body, Code, Description) ->
    siprequest:send_result(Header, Socket, Body, Code, Description).

send_result(Header, Socket, Body, Code, Description, ExtraHeaders) ->
    siprequest:send_result(Header, Socket, Body, Code, Description, ExtraHeaders).
