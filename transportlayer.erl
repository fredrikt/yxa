%%%-------------------------------------------------------------------
%%% File    : transportlayer
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Transport layer functions. Because of legacy reasons,
%%%           the actual code is in siprequest.erl but the sending
%%%           code will be moved here RSN.
%%%
%%% Created : 01 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------

-module(transportlayer).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/0,
	 send_proxy_request/4,
	 send_proxy_response/2,
	 send_result/5,
	 send_result/6
	]).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").

%%====================================================================
%% External functions
%%====================================================================

start() ->
    sipsocket:start_link().

send_proxy_response(Socket, Response)
  when is_record(Socket, sipsocket); Socket == none, is_record(Response, response) ->
    siprequest:send_proxy_response(Socket, Response).

send_proxy_request(Socket, Request, DstList, Parameters)
  when is_record(Socket, sipsocket); Socket == none, is_record(Request, request) ->
    siprequest:send_proxy_request(Socket, Request, DstList, Parameters).

send_result(RequestHeader, Socket, Body, Status, Reason)
  when is_record(RequestHeader, keylist), is_record(Socket, sipsocket); Socket == none,
       is_list(Body), is_integer(Status), is_list(Reason) ->
    siprequest:send_result(RequestHeader, Socket, Body, Status, Reason).

send_result(RequestHeader, Socket, Body, Status, Reason, ExtraHeaders)
  when is_record(RequestHeader, keylist), is_record(Socket, sipsocket); Socket == none,
       is_list(Body), is_integer(Status), is_list(Reason), is_record(ExtraHeaders, keylist) ->
    siprequest:send_result(RequestHeader, Socket, Body, Status, Reason, ExtraHeaders).
