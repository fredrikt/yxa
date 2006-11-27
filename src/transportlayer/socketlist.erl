%%%-------------------------------------------------------------------
%%% File    : socketlist.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Transport layer modules list-module.
%%%
%%% Created : 15 Dec 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(socketlist).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 add/4,
	 add/5,
	 delete_using_pid/2,
	 delete_expired/1,
	 empty/0,
	 get_using_id/2,
	 get_using_pid/2,
	 get_using_remote/4,
	 get_using_socketid/2,
	 extract/2,
	 get_length/1,
	 monitor_format/1,
	 debugfriendly/1
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("socketlist.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(yxa_socket_ident, {
	  type,		%% listener | from | to
	  proto,	%% udp | udp6 | tcp | tcp6 | tls | tls6 | yxa_test | yxa_test6
	  hostport	%% hp record(),
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DEFAULT_SOCKET_EXPIRE, 300).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add(Type, Pid, SipSocket, SocketList)
%%           Type       = listener | from | to
%%           Pid        = pid(), pid of connection handler
%%           SipSocket  = sipsocket record()
%%           Expire     = integer() (0 for never expire)
%%           SocketList = socketlist record()
%% Descrip.: Add a new sipsocket entry to SocketList.
%% Returns : {ok, NewSocketList} |
%%           {error, Reason}
%%           NewSocketList = socketlist record()
%%           Reason        = string()
%%--------------------------------------------------------------------
add(Type, Pid, SipSocket, SocketList) when is_atom(Type), is_pid(Pid), is_record(SipSocket, sipsocket),
					   is_record(SocketList, socketlist) ->
    %% Add without expire, use default of Now + DEFAULT_SOCKET_EXPIRE seconds
    Expire = util:timestamp() + ?DEFAULT_SOCKET_EXPIRE,
    add(Type, Pid, SipSocket, Expire, SocketList).

add(Type, Pid, SipSocket, Expire, SocketList) when is_pid(Pid), is_record(SipSocket, sipsocket), is_integer(Expire),
						   is_record(SocketList, socketlist) ->
    Id = make_yxa_socket_ident(Type, SipSocket),
    case get_using_id(Id, SocketList) of
	[] ->
	    NewElem = #socketlistelem{ref	= make_ref(),
				      id	= Id,
				      pid	= Pid,
				      proto	= SipSocket#sipsocket.proto,
				      hostport  = SipSocket#sipsocket.hostport,
				      sipsocket	= SipSocket,
				      expire	= Expire
				     },
	    OldList = SocketList#socketlist.list,
	    {ok, #socketlist{list = [NewElem | OldList]}};
	Elem when is_record(Elem, socketlistelem), Elem#socketlistelem.pid == Pid ->
	    %% delete and add to get fresh expires
	    SList1 = delete_using_ref(Elem#socketlistelem.ref, SocketList),
	    %% XXX should we keep old ref perhaps?
	    NewElem = #socketlistelem{ref	= make_ref(),
				      id	= Id,
				      pid	= Pid,
				      proto	= SipSocket#sipsocket.proto,
				      hostport  = SipSocket#sipsocket.hostport,
				      sipsocket	= SipSocket,
				      expire	= Expire
				     },
	    OldList = SList1#socketlist.list,
	    {ok, #socketlist{list = [NewElem | OldList]}};
	Elem when is_record(Elem, socketlistelem) ->
	    [StoredPid] = extract([pid], Elem),
	    logger:log(error, "socketlist: Failed adding duplicate item with id ~p", [Id]),
	    logger:log(debug, "socketlist: Asked to add duplicate Id ~p (to OTHER pid, "
		       "~p - stored pid ~p) to list :~n~p", [Id, Pid, StoredPid, SocketList]),
	    {error, "Duplicate Id, new Pid"}
    end.

%%--------------------------------------------------------------------
%% Function: empty()
%% Descrip.: Get an empty socketlist.
%% Returns : SocketList = socketlist record()
%%--------------------------------------------------------------------
empty() ->
    #socketlist{list = []}.

%%--------------------------------------------------------------------
%% Function: extract(Fields, SListElem)
%%           Fields = list() of atom(), id | pid | proto | local |
%%                                      remote | sipsocket | expire
%%           SListElem = socketlistelem record()
%% Descrip.: Return one or more values from a socketlistelem record.
%% Returns : Values = list()
%%--------------------------------------------------------------------
extract(Values, []) ->
    erlang:fault(Values, []);
extract(Values, SListElem) when is_record(SListElem, socketlistelem) ->
    extract(Values, SListElem, []).

extract([], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    lists:reverse(Res);
extract([id | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, [SListElem#socketlistelem.id | Res]);
extract([pid | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, [SListElem#socketlistelem.pid | Res]);
extract([proto | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, [SListElem#socketlistelem.proto | Res]);
extract([hostport | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, [SListElem#socketlistelem.hostport | Res]);
extract([sipsocket | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, [SListElem#socketlistelem.sipsocket | Res]);
extract([expire | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, [SListElem#socketlistelem.expire | Res]).

%%--------------------------------------------------------------------
%% Function: delete_using_pid(Pid, SocketList)
%%           Pid        = pid()
%%           SocketList = socketlist record()
%% Descrip.: Delete an element from SocketList identified by a pid.
%% Returns : NewSocketList = socketlist record()
%%--------------------------------------------------------------------
delete_using_pid(Pid, SocketList) when is_pid(Pid), is_record(SocketList, socketlist) ->
    #socketlist{list = del_pid(Pid, SocketList#socketlist.list)}.

%%--------------------------------------------------------------------
%% Function: delete_expired(SocketList)
%%           SocketList = socketlist record()
%% Descrip.: Delete all elements from SocketList that has a timestamp
%%           less than the current time.
%% Returns : NewSocketList = socketlist record()
%%--------------------------------------------------------------------
delete_expired(SocketList) when is_record(SocketList, socketlist) ->
    Now = util:timestamp(),
    #socketlist{list = del_time(Now, SocketList#socketlist.list)}.

%%--------------------------------------------------------------------
%% Function: get_using_id(Id, SocketList)
%%           Id         = term()
%%           SocketList = socketlist record()
%% Descrip.: Find the first element of SocketList that has an id
%%           matching the supplied Id.
%% Returns : Elem |
%%           []
%%           Elem = socketlistelem record()
%%--------------------------------------------------------------------
get_using_id(Id, SocketList) when is_record(SocketList, socketlist) ->
    get_using_id1(Id, SocketList#socketlist.list).

get_using_id1(_Id, []) ->
    [];
get_using_id1(Id, [H | _]) when is_record(H, socketlistelem), H#socketlistelem.id == Id ->
    %% We have a match
    H;
get_using_id1(Id, [H | T]) when is_record(H, socketlistelem) ->
    get_using_id1(Id, T).

%%--------------------------------------------------------------------
%% Function: get_using_pid(Id, SocketList)
%%           Pid        = pid()
%%           SocketList = socketlist record()
%% Descrip.: Return all elements of SocketList that has a pid matching
%%           the supplied Pid.
%% Returns : NewSocketList |
%%           none
%%           NewSocketList = socketlist record()
%%--------------------------------------------------------------------
get_using_pid(Pid, SocketList) when is_record(SocketList, socketlist), is_pid(Pid) ->
    case get_using_pid1(Pid, SocketList#socketlist.list, []) of
	[] ->
	    none;
	L ->
	    #socketlist{list = L}
    end.

get_using_pid1(Pid, [H | T], Res) when is_record(H, socketlistelem), H#socketlistelem.pid == Pid ->
    get_using_pid1(Pid, T, [H | Res]);
get_using_pid1(Pid, [H | T], Res) when is_record(H, socketlistelem) ->
    get_using_pid1(Pid, T, Res);
get_using_pid1(_Pid, [], Res) ->
    lists:reverse(Res).

%%--------------------------------------------------------------------
%% Function: get_using_remote(Proto, IP, Port, SocketList)
%%           Proto      = atom()
%%           IP         = string()
%%           Port       = integer()
%%           SocketList = socketlist record()
%% Descrip.: Find the first element of SocketList that has a remote
%%           matching the supplied Remote, and protocol matches Proto.
%% Returns : Elem |
%%           none
%%           Elem = socketlistelem record()
%%--------------------------------------------------------------------
get_using_remote(Proto, IP, Port, SocketList) when is_atom(Proto), is_record(SocketList, socketlist),
						     is_list(IP), is_integer(Port) ->
    get_using_remote1(Proto, IP, Port, SocketList#socketlist.list).

get_using_remote1(Proto, IP, Port, [#socketlistelem{sipsocket =
						    #sipsocket{proto = Proto,
							       hostport = #hp{r_ip = IP,
									      r_port = Port
									     }
							      }
						   } = H | _]) ->
    H;
get_using_remote1(Proto, IP, Port, [H | T]) when is_record(H, socketlistelem) ->
    get_using_remote1(Proto, IP, Port, T);
get_using_remote1(_Proto, _IP, _Port, []) ->
    none.


get_using_socketid(Id, SocketList) when is_record(SocketList, socketlist), is_record(Id, ob_id) ->
    case get_using_socketid1(Id, SocketList#socketlist.list, []) of
	[] ->
	    none;
	[Elem] ->
	    Elem
    end.

get_using_socketid1(_Id, [], Res) ->
    Res;
get_using_socketid1(Id, [#socketlistelem{sipsocket = #sipsocket{id = Id}} = H | T], Res) ->
    get_using_socketid1(Id, T, [H | Res]);
get_using_socketid1(Id, [H | T], Res) when is_record(H, socketlistelem) ->
    get_using_socketid1(Id, T, Res).


%%--------------------------------------------------------------------
%% Function: get_length(SList)
%%           SList = socketlist record()
%% Descrip.: Return the number of elements in an socketlist record().
%% Returns : Length = integer()
%%--------------------------------------------------------------------
get_length(SList) when is_record(SList, socketlist) ->
    length(SList#socketlist.list).

%%--------------------------------------------------------------------
%% Function: debugfriendly(SList)
%%           SList = socketlist record()
%% Descrip.: Return information about the elements in an socketlist
%%           record in a format that is suitable for logging using ~p.
%% Returns : Data = term()
%%--------------------------------------------------------------------
debugfriendly(SList) when is_record(SList, socketlist) ->
    debugfriendly2(debug, SList#socketlist.list).

monitor_format(SList) when is_record(SList, socketlist) ->
    debugfriendly2(monitor, SList#socketlist.list).

debugfriendly2(_, []) ->
    [];
debugfriendly2(Output, [H | Rest]) when is_record(H, socketlistelem) ->
    Id = H#socketlistelem.id,
    Pid = pid_to_list(H#socketlistelem.pid),
    Proto = H#socketlistelem.proto,
    HP    = H#socketlistelem.hostport,
    ExpireIn = case H#socketlistelem.expire of
		   0 -> "never";
		   Expire when integer(Expire) ->
		       Expire - util:timestamp()
	       end,
    IdStr = io_lib:format("~p", [Id]),
    LocalStr =
	case HP#hp.l_ip of
	    undefined ->
		"none";
	    _ ->
		lists:concat([HP#hp.l_ip, ":", HP#hp.l_port])
	end,
    RemoteStr =
	case HP#hp.r_ip of
	    undefined ->
		"none";
	    _ ->
		lists:concat([HP#hp.r_ip, ":", HP#hp.r_port])
	end,
    Str =
	case Output of
	    debug ->
		lists:concat(["id=", IdStr, ", Pid=", Pid, ", L=", LocalStr,
			      ", R=", RemoteStr, ", Expire=", ExpireIn]);
	    monitor ->
		case Id of
		    {listener, LProto, LPort} ->
			lists:concat(["Listening on: ", LProto, " port ", LPort]);
		    {from, _Host, _Port} ->
			lists:concat(["From: ", RemoteStr, " to ", LocalStr,
				      " (", Proto, ") (expires: ", ExpireIn, ")"]);
		    {to, _Host, _Port} ->
			lists:concat(["To: ", RemoteStr, " from ", LocalStr,
				      " (", Proto, ") (expires: ", ExpireIn, ")"])
		end
	end,
    lists:append([lists:flatten(Str)], debugfriendly2(Output, Rest)).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: delete_using_ref(Id, SocketList)
%%           Id         = term()
%%           SocketList = socketlist record()
%% Descrip.: Delete an element from SocketList identified by a
%%           reference.
%% Returns : NewSocketList = socketlist record()
%%--------------------------------------------------------------------
delete_using_ref(Ref, SocketList) when is_record(SocketList, socketlist) ->
    #socketlist{list=del_ref(Ref, SocketList#socketlist.list)}.

del_ref(_Ref, []) ->
    [];
del_ref(Ref, [H | T]) when is_record(H, socketlistelem), H#socketlistelem.ref == Ref ->
    del_ref(Ref, T);
del_ref(Ref, [H | T]) when is_record(H, socketlistelem) ->
    [H | del_ref(Ref, T)].

del_pid(Pid, []) when is_pid(Pid) ->
    [];
del_pid(Pid, [H | T]) when is_pid(Pid), is_record(H, socketlistelem), H#socketlistelem.pid == Pid ->
    del_pid(Pid, T);
del_pid(Pid, [H | T]) when is_pid(Pid), is_record(H, socketlistelem) ->
    [H | del_pid(Pid, T)].

del_time(Time, []) when is_integer(Time) ->
    [];
del_time(Time, [H | T]) when is_record(H, socketlistelem), is_integer(Time), H#socketlistelem.expire < Time, H#socketlistelem.expire > 0 ->
    %% XXX signal expired socket pid so that it can exit?
    logger:log(debug, "socketlist: Extra debug : Record expired :~n~p", [debugfriendly([H])]),
    del_time(Time, T);
del_time(Time, [H | T]) when is_record(H, socketlistelem), is_integer(Time) ->
    [H | del_time(Time, T)].



%% Returns : yxa_socket_ident record()
make_yxa_socket_ident(Type, SipSocket) when is_atom(Type), is_record(SipSocket, sipsocket) ->
    make_yxa_socket_ident(Type, SipSocket#sipsocket.proto, SipSocket#sipsocket.hostport).

make_yxa_socket_ident(listener, Proto, HP) when is_atom(Proto), is_record(HP, hp) ->
    #yxa_socket_ident{type		= listener,
		      proto		= Proto,
		      hostport		= HP#hp{r_ip	= undefined,
						r_port	= undefined
					       }
		     };
make_yxa_socket_ident(in, Proto, HP) when is_atom(Proto), is_record(HP, hp) ->
    #yxa_socket_ident{type		= from,
		      proto		= Proto,
		      hostport		= HP#hp{l_ip	= undefined,
						l_port	= undefined
					       }
		     };
make_yxa_socket_ident(out, Proto, HP) when is_atom(Proto), is_record(HP, hp) ->
    #yxa_socket_ident{type		= from,
		      proto		= Proto,
		      hostport		= HP#hp{l_ip	= undefined,
						l_port	= undefined
					       }
		     }.
