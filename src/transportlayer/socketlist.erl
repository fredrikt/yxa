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
	 add/7,
	 add/8,
	 delete_using_pid/2,
	 delete_expired/1,
	 empty/0,
	 get_using_id/2,
	 get_using_pid/2,
	 get_using_remote/3,
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

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DEFAULT_SOCKET_EXPIRE, 300).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add(Id, Pid, Proto, Local, Remote, SipSocket, SocketList)
%%           Id         = {listener, Proto, IP, Port} |
%%                        {from, Proto, Remote}       |
%%                        {to, Proto, Remote}
%%                        Proto = atom(), tcp|tcp6|udp|udp6|tls|tls6
%%                        Remote = {IP, Port}
%%                        IP   = string()
%%                        Port = integer()
%%           Pid        = pid(), pid of connection handler
%%           Proto      = atom(), protocol of this socket (tcp|...)
%%           Local      = {IP, Port}
%%                        IP   = string()
%%                        Port = integer()
%%           Remote     = {IP, Port} | none
%%                        IP   = string()
%%                        Port = integer()
%%           SipSocket  = sipsocket record()
%%           Expire     = integer() (0 for never expire)
%%           SocketList = socketlist record()
%% Descrip.: Add a new sipsocket entry to SocketList.
%% Returns : NewSocketList   |
%%           {error, Reason}
%%           NewSocketList = socketlist record()
%%           Reason        = string()
%%--------------------------------------------------------------------
add(Id, Pid, Proto, {_LIP, _LPort} = Local, Remote, SipSocket, SocketList)
  when is_pid(Pid), is_atom(Proto), is_record(SipSocket, sipsocket), is_record(SocketList, socketlist) ->
    %% Add without expire, use default of Now + DEFAULT_SOCKET_EXPIRE seconds
    Expire = util:timestamp() + ?DEFAULT_SOCKET_EXPIRE,
    add(Id, Pid, Proto, Local, Remote, SipSocket, Expire, SocketList).

add(Id, Pid, Proto, Local, Remote, SipSocket, Expire, SocketList)
  when is_pid(Pid), is_atom(Proto), is_record(SipSocket, sipsocket), is_integer(Expire),
       is_record(SocketList, socketlist),
       element(1, Id) == listener; element(1, Id) == from; element(1, Id) == to ->
    case get_using_id(Id, SocketList) of
	[] ->
	    NewElem = #socketlistelem{ref	= make_ref(),
				      id	= Id,
				      pid	= Pid,
				      proto	= Proto,
				      local	= Local,
				      remote	= Remote,
				      sipsocket	= SipSocket,
				      expire	= Expire
				     },
	    #socketlist{list = lists:append(SocketList#socketlist.list, [NewElem])};
	Elem when is_record(Elem, socketlistelem), Elem#socketlistelem.pid == Pid ->
	    %% delete and add to get fresh expires
	    SList1 = delete_using_ref(Elem#socketlistelem.ref, SocketList),
	    %% XXX should we keep old ref perhaps?
	    NewElem = #socketlistelem{ref	= make_ref(),
				      id	= Id,
				      pid	= Pid,
				      local	= Local,
				      remote	= Remote,
				      sipsocket	= SipSocket,
				      expire	= Expire
				     },
	    #socketlist{list = lists:append(SList1#socketlist.list, [NewElem])};
	Elem when is_record(Elem, socketlistelem) ->
	    [StoredPid] = extract([pid], Elem),
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
    Res;
extract([id | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.id]));
extract([pid | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.pid]));
extract([proto | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.proto]));
extract([local | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.local]));
extract([remote | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.remote]));
extract([sipsocket | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.sipsocket]));
extract([expire | T], SListElem, Res) when is_record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.expire])).

%%--------------------------------------------------------------------
%% Function: delete_using_pid(Pid, SocketList)
%%           Pid        = pid()
%%           SocketList = socketlist record()
%% Descrip.: Delete an element from SocketList identified by a pid.
%% Returns : NewSocketList = socketlist record()
%%--------------------------------------------------------------------
delete_using_pid(Pid, SocketList) when is_pid(Pid), is_record(SocketList, socketlist) ->
    #socketlist{list=del_pid(Pid, SocketList#socketlist.list)}.

%%--------------------------------------------------------------------
%% Function: delete_expired(SocketList)
%%           SocketList = socketlist record()
%% Descrip.: Delete all elements from SocketList that has a timestamp
%%           less than the current time.
%% Returns : NewSocketList = socketlist record()
%%--------------------------------------------------------------------
delete_expired(SocketList) when is_record(SocketList, socketlist) ->
    Now = util:timestamp(),
    #socketlist{list=del_time(Now, SocketList#socketlist.list)}.

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
	    #socketlist{list=L}
    end.

get_using_pid1(_Pid, [], Res) ->
    Res;
get_using_pid1(Pid, [H | T], Res) when is_record(H, socketlistelem), H#socketlistelem.pid == Pid ->
    get_using_pid1(Pid, T, lists:append(Res, [H]));
get_using_pid1(Pid, [H | T], Res) when is_record(H, socketlistelem) ->
    get_using_pid1(Pid, T, Res).

%%--------------------------------------------------------------------
%% Function: get_using_remote(Proto, Remote, SocketList)
%%           Proto      = atom()
%%           Remote     = {IP, Port} tuple()
%%           SocketList = socketlist record()
%% Descrip.: Find the first element of SocketList that has a remote
%%           matching the supplied Remote, and protocol matches Proto.
%% Returns : Elem |
%%           []
%%           Elem = socketlistelem record()
%%--------------------------------------------------------------------
get_using_remote(Proto, {IP, Port}=Remote, SocketList) when is_atom(Proto), is_record(SocketList, socketlist),
							    is_list(IP), is_integer(Port) ->
    get_using_remote1(Proto, Remote, SocketList#socketlist.list).

get_using_remote1(_Proto, _Remote, []) ->
    [];
get_using_remote1(Proto, Remote, [#socketlistelem{proto=Proto, remote=Remote}=H | _]) ->
    H;
get_using_remote1(Proto, Remote, [H | T]) when is_record(H, socketlistelem) ->
    get_using_remote1(Proto, Remote, T).


get_using_socketid(Id, SocketList) when is_record(SocketList, socketlist), is_tuple(Id), size(Id) == 2 ->
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
    Local = H#socketlistelem.local,
    Remote = H#socketlistelem.remote,
    ExpireIn = case H#socketlistelem.expire of
		   0 -> "never";
		   Expire when integer(Expire) ->
		       Expire - util:timestamp()
	       end,
    IdStr = io_lib:format("~p", [Id]),
    LocalStr = case Local of
		   {IP, Port} ->
		       lists:concat([IP, ":", Port]);
		   none ->
		       "none"
	       end,
    RemoteStr = case Remote of
		    {IP2, Port2} ->
			lists:concat([IP2, ":", Port2]);
		    none ->
			"none"
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
