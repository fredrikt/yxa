%%%-------------------------------------------------------------------
%%% File    : sipsocket_test.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Sipsocket test module, fakes network communication to
%%%           make it possible to test other modules.
%%%
%%% @since    18 Jul 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sipsocket_test).
%%-compile(export_all).

-behaviour(sipsocket).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 send/5,
	 is_reliable_transport/1,
	 get_socket/1,
	 get_specific_socket/1,
	 get_raw_socket/1,
	 get_remote_peer/1,
	 close_socket/1
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ignore
%%
%% @doc     Would've done some useful initialization if this was not
%%          merely a test module.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    ignore.


%%====================================================================
%% Interface functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (SipSocket, Proto, Host, Port, Message) ->
%%            ok              |
%%            {error, Reason}
%%
%%            SipSocket = #sipsocket{}
%%            Proto     = atom() "yxa_test"
%%            Host      = string()
%%            Port      = integer()
%%            Message   = term()
%%
%%            Reason = string()
%%
%% @doc     Fake sending Message to Host:Port. Return failure or
%%          success based on process dictionary.
%% @end
%%--------------------------------------------------------------------
send(SipSocket, Proto, _Host, _Port, _Message)
  when is_record(SipSocket, sipsocket), SipSocket#sipsocket.proto /= Proto ->
    {error, "Protocol mismatch"};
send(SipSocket, yxa_test, Host, Port, Message) when is_record(SipSocket, sipsocket) ->
    Proto = SipSocket#sipsocket.proto,
    self() ! {sipsocket_test, send, {Proto, Host, Port}, Message},
    case autotest:is_unit_testing(?MODULE, {sipsocket_test, send_result}) of
	{true, {error, Reason}} ->
	    {error, Reason};
	false ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (Dst) -> #sipsocket{} | term()
%%
%%            Dst = #sipdst{}
%%
%% @doc     Return a fake socket or a term based on process dict.
%% @end
%%--------------------------------------------------------------------
get_socket(#sipdst{proto = yxa_test}) ->
    case autotest:is_unit_testing(?MODULE, {sipsocket_test, get_socket}) of
	{true, Res} ->
	    Res;
	false ->
	    #sipsocket{module = ?MODULE,
		       proto  = yxa_test,
		       pid    = self()
		      }
    end.

%%--------------------------------------------------------------------
%% @spec    (Id) -> #sipsocket{} | term()
%%
%%            Id = #ob_id{}
%%
%% @doc     Return a fake socket or a term based on process dict.
%% @end
%%--------------------------------------------------------------------
get_specific_socket(#ob_id{proto = yxa_test}) ->
    case autotest:is_unit_testing(?MODULE, {sipsocket_test, get_specific_socket}) of
	{true, Res} ->
	    Res;
	false ->
	    #sipsocket{module = ?MODULE,
		       proto  = yxa_test,
		       pid    = self()
		      }
    end.

%%--------------------------------------------------------------------
%% @spec    (SipSocket) -> #sipsocket{} | term()
%%
%%            SipSocket = #sipsocket{}
%%
%% @doc     Return a fake raw socket or a term based on process dict.
%% @end
%%--------------------------------------------------------------------
get_raw_socket(#sipsocket{proto = yxa_test}) ->
    case autotest:is_unit_testing(?MODULE, {sipsocket_test, get_raw_socket}) of
	{true, Res} ->
	    Res;
	false ->
	    {sipsocket_test, fake_raw_socket}
    end.

%%--------------------------------------------------------------------
%% @spec    (SipSocket) ->
%%            {ok, Proto, Addr, Port} | term()
%%
%%            SipSocket = #sipsocket{}
%%
%%            Proto = yxa_test
%%            Addr  = string() "\"192.0.2.242\""
%%            Port  = integer()
%%
%% @doc     Return fake remote peer info based on process dictionary.
%% @end
%%--------------------------------------------------------------------
get_remote_peer(#sipsocket{proto = yxa_test}) ->
    case autotest:is_unit_testing(?MODULE, {sipsocket_test, get_remote_peer}) of
	{true, Res} ->
	    Res;
	false ->
	    {ok, yxa_test, "192.0.2.242", sipsocket:get_listenport(yxa_test)}
    end.

%%--------------------------------------------------------------------
%% @spec    (SipSocket) -> true | false
%%
%%            SipSocket = #sipsocket{}
%%
%% @doc     Fake response based on process dictionary.
%% @end
%%--------------------------------------------------------------------
is_reliable_transport(#sipsocket{proto = yxa_test}) ->
    case autotest:is_unit_testing(?MODULE, {sipsocket_test, is_reliable_transport}) of
	{true, Res} ->
	    Res;
	false ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (SipSocket) ->
%%            ok              |
%%            {error, Reason}
%%
%%            SipSocket = #sipsocket{}
%%
%%            Reason = not_applicable | term()
%%
%% @doc     Fake response based on process dictionary.
%% @end
%%--------------------------------------------------------------------
close_socket(#sipsocket{proto = yxa_test}) ->
    case autotest:is_unit_testing(?MODULE, {sipsocket_test, close_socket}) of
	{true, Res} ->
	    Res;
	false ->
	    ok
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
