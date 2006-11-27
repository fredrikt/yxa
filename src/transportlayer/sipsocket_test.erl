%%%-------------------------------------------------------------------
%%% File    : sipsocket_test.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Sipsocket test module, fakes network communication to
%%%           make it possible to test other modules.
%%%
%%% Created : 18 Jul 2005 by Fredrik Thulin <ft@it.su.se>
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
	 get_remote_peer/1
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Descrip.: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    ignore.


%%====================================================================
%% Interface functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: send(SipSocket, Proto, Host, Port, Message)
%%           SipSocket = sipsocket record()
%%           Proto     = atom(), yxa_test
%%           Host      = string()
%%           Port      = integer()
%%           Message   = term()
%% Descrip.: Fake sending Message to Host:Port. Return failure or
%%           success based on process dictionary.
%% Returns : ok              |
%%           {error, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
send(SipSocket, Proto, _Host, _Port, _Message)
  when is_record(SipSocket, sipsocket), SipSocket#sipsocket.proto /= Proto ->
    {error, "Protocol mismatch"};
send(SipSocket, yxa_test, Host, Port, Message) when is_record(SipSocket, sipsocket) ->
    Proto = SipSocket#sipsocket.proto,
    self() ! {sipsocket_test, send, {Proto, Host, Port}, Message},
    case get({sipsocket_test, send_result}) of
	undefined ->
	    ok;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: get_socket(Dst)
%%           Dst = sipdst record()
%% Descrip.: Return a fake socket or a term based on process dict.
%% Returns : sipsocket record() | term()
%%--------------------------------------------------------------------
get_socket(#sipdst{proto = yxa_test}) ->
    case get({sipsocket_test, get_socket}) of
	undefined ->
	    #sipsocket{module = ?MODULE,
		       proto  = yxa_test,
		       pid    = self()
		      };
	Res ->
	    Res
    end.

%%--------------------------------------------------------------------
%% Function: get_specific_socket(Id)
%%           Id = tuple() ({Proto, Id})
%% Descrip.: Return a fake socket or a term based on process dict.
%% Returns : sipsocket record() | term()
%%--------------------------------------------------------------------
get_specific_socket({yxa_test, _Id}) ->
    case get({sipsocket_test, get_specific_socket}) of
	undefined ->
	    #sipsocket{module = ?MODULE,
		       proto  = yxa_test,
		       pid    = self()
		      };
	Res ->
	    Res
    end.

%%--------------------------------------------------------------------
%% Function: get_raw_socket(SipSocket)
%%           Dst = sipdst record()
%% Descrip.: Return a fake raw socket or a term based on process dict.
%% Returns : sipsocket record() | term()
%%--------------------------------------------------------------------
get_raw_socket(#sipsocket{proto = yxa_test}) ->
    case get({sipsocket_test, get_raw_socket}) of
	undefined ->
	    {sipsocket_test, fake_raw_socket};
	Res ->
	    Res
    end.

%%--------------------------------------------------------------------
%% Function: get_remote_peer(SipSocket)
%%           Dst = sipdst record()
%% Descrip.: Return fake remote peer info based on process dictionary.
%% Returns : {ok, Proto, Addr, Port} | term()
%%--------------------------------------------------------------------
get_remote_peer(#sipsocket{proto = yxa_test}) ->
    case get({sipsocket_test, get_remote_peer}) of
	undefined ->
	    {ok, yxa_test, "192.0.2.242", sipsocket:get_listenport(yxa_test)};
	Res ->
	    Res
    end.

%%--------------------------------------------------------------------
%% Function: is_reliable_transport(_)
%% Descrip.: Fake response based on process dictionary.
%% Returns : true | false
%%--------------------------------------------------------------------
is_reliable_transport(_) ->
    case get({sipsocket_test, is_reliable_transport}) of
	undefined ->
	    false;
	X ->
	    X
    end.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
