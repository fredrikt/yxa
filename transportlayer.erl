-module(transportlayer).
-export([start/0, send_proxy_request/4, send_proxy_response/2, send_result/5,
	 send_result/6]).

-include("siprecords.hrl").

start() ->
    sipsocket:start_link().

send_proxy_response(Socket, Response) when record(Response, response) ->
    siprequest:send_proxy_response(Socket, Response).

send_proxy_request(Socket, Request, DstURI, Parameters) when record(Request, request) ->
    siprequest:send_proxy_request(Socket, Request, DstURI, Parameters).

send_result(RequestHeader, Socket, Body, Code, Description) ->
    siprequest:send_result(RequestHeader, Socket, Body, Code, Description).

send_result(RequestHeader, Socket, Body, Code, Description, ExtraHeaders) ->
    siprequest:send_result(RequestHeader, Socket, Body, Code, Description, ExtraHeaders).
