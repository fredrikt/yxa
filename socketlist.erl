-module(socketlist).
-export([add/5, add/6, delete_using_pid/2, delete_expired/1,
	 empty/0, get_using_id/2, get_using_remote/2,
	 extract/2, debugfriendly/1]).

-include("socketlist.hrl").

add(Id, Pid, Local, Remote, SocketList) when record(SocketList, socketlist) ->
    Now = util:timestamp(),
    add(Id, Pid, Local, Remote, Now + 300, SocketList).

add(Id, Pid, Local, Remote, Expire, SocketList) when record(SocketList, socketlist) ->
    case get_using_id(Id, SocketList) of
	[] ->
	    NewElem = #socketlistelem{ref=make_ref(), id=Id, pid=Pid, local=Local, remote=Remote, expire=Expire},
	    #socketlist{list=lists:append(SocketList#socketlist.list, [NewElem])};
	Elem when record(Elem, socketlistelem), Elem#socketlistelem.pid == Pid ->
	    % delete and add to get fresh expires
	    SList1 = delete_using_ref(Elem#socketlistelem.ref, SocketList),
	    % XXX should we keep old ref perhaps?
	    NewElem = #socketlistelem{ref=make_ref(), id=Id, pid=Pid, local=Local, remote=Remote, expire=Expire},
	    #socketlist{list=lists:append(SList1#socketlist.list, [NewElem])};
	Elem when record(Elem, socketlistelem) ->
	    [StoredPid] = extract([pid], Elem),
	    logger:log(error, "socketlist: Asked to add duplicate Id ~p (to OTHER pid, ~p - stored pid ~p) to list :~n~p",
	    	       [Id, Pid, StoredPid, SocketList]),
	    {error, "Duplicate Id, new Pid"}
    end.

empty() ->
    #socketlist{list=[]}.

extract(Values, []) ->
    erlang:fault(Values, []);
extract(Values, SListElem) when record(SListElem, socketlistelem) ->
    extract(Values, SListElem, []).

extract([], SListElem, Res) when record(SListElem, socketlistelem) ->
    Res;
extract([id | T], SListElem, Res) when record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.id]));
extract([pid | T], SListElem, Res) when record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.pid]));
extract([local | T], SListElem, Res) when record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.local]));
extract([remote | T], SListElem, Res) when record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.remote]));
extract([expire | T], SListElem, Res) when record(SListElem, socketlistelem) ->
    extract(T, SListElem, lists:append(Res, [SListElem#socketlistelem.expire])).

delete_using_ref(Id, SocketList) when record(SocketList, socketlist) ->
    #socketlist{list=del_ref(Id, SocketList#socketlist.list)}.

delete_using_pid(Socket, SocketList) when record(SocketList, socketlist) ->
    #socketlist{list=del_pid(Socket, SocketList#socketlist.list)}.

delete_expired(SocketList) when record(SocketList, socketlist) ->
    Now = util:timestamp(),
    #socketlist{list=del_time(Now, SocketList#socketlist.list)}.

get_using_id(Id, SocketList) when record(SocketList, socketlist) ->
    get_using_id1(Id, SocketList#socketlist.list).

get_using_id1(Id, []) ->
    [];
get_using_id1(Id, [H | _]) when record(H, socketlistelem), H#socketlistelem.id == Id ->
    H;
get_using_id1(Id, [H | T]) when record(H, socketlistelem) ->
    get_using_id1(Id, T);
get_using_id1(Id, [H | _]) ->
    throw({'EXIT', {"SocketList element not wellformed", H}}).

get_using_remote(Remote, SocketList) when record(SocketList, socketlist) ->
    get_using_remote1(Remote, SocketList#socketlist.list).

get_using_remote1(Remote, []) ->
    [];
get_using_remote1(Remote, [H | _]) when record(H, socketlistelem), H#socketlistelem.remote == Remote ->
    H;
get_using_remote1(Remote, [H | T]) when record(H, socketlistelem) ->
    get_using_remote1(Remote, T);
get_using_remote1(Remote, [H | _]) ->
    throw({'EXIT', {"SocketList element not wellformed", H}}).


del_ref(Ref, []) ->
    [];
del_ref(Ref, [H | T]) when record(H, socketlistelem), H#socketlistelem.ref == Ref ->
    del_ref(Ref, T);
del_ref(Ref, [H | T]) when record(H, socketlistelem) ->
    [H | del_ref(Ref, T)].

del_pid(Pid, []) ->
    [];
del_pid(Pid, [H | T]) when record(H, socketlistelem), H#socketlistelem.pid == Pid ->
    del_pid(Pid, T);
del_pid(Pid, [H | T]) when record(H, socketlistelem) ->
    [H | del_pid(Pid, T)].

del_time(Time, []) ->
    [];
del_time(Time, [H | T]) when record(H, socketlistelem), H#socketlistelem.expire < Time, H#socketlistelem.expire > 0 ->
    logger:log(debug, "FREDRIK: RECORD EXPIRED : ~n~p", [debugfriendly([H])]),
    del_time(Time, T);
del_time(Time, [H | T]) when record(H, socketlistelem) ->
    [H | del_time(Time, T)].

debugfriendly(SList) when record(SList, socketlist) ->
    debugfriendly(SList#socketlist.list);
debugfriendly([]) ->
    [];
debugfriendly([H | Rest]) when record(H, socketlistelem) ->
    Id = H#socketlistelem.id,
    Pid = pid_to_list(H#socketlistelem.pid),
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
    Str = lists:concat(["id=", IdStr, ", Pid=", Pid, ", ", LocalStr, ", ", RemoteStr, ", Expire=", ExpireIn]), 
    lists:append([lists:flatten(Str)], debugfriendly(Rest));
debugfriendly(Unknown) ->
    logger:log(debug, "MALFORMED socketlist or socketlistelem :~n~p", [Unknown]).
