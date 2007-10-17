%%%-------------------------------------------------------------------
%%% File    : socketlist.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Transport layer modules list-module.
%%%
%%% @since    15 Dec 2003 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
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

-export([test/0]).

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

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% @type  slist_field() = id | pid | proto | local | remote | sipsocket | expire.
%%           Extractable #socketlistelem{} fields.


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Type, Pid, SipSocket, SocketList) -> term()
%%
%%            Type       = listener | in | out
%%            Pid        = pid() "pid of connection handler"
%%            SipSocket  = #sipsocket{}
%%            SocketList = #socketlist{}
%%
%% @doc     Add a new sipsocket entry to SocketList with a default
%%          expiration time. The DefaultExpire is current time plus
%%          300 seconds.
%% @equiv    add(Type, Pid, SipSocket, DefaultExpire, SocketList)
%% @end
%%--------------------------------------------------------------------
add(Type, Pid, SipSocket, SocketList) when is_atom(Type), is_pid(Pid), is_record(SipSocket, sipsocket),
					   is_record(SocketList, socketlist) ->
    %% Add without expire, use default of Now + DEFAULT_SOCKET_EXPIRE seconds
    Expire = util:timestamp() + ?DEFAULT_SOCKET_EXPIRE,
    add(Type, Pid, SipSocket, Expire, SocketList).

%%--------------------------------------------------------------------
%% @spec    (Type, Pid, SipSocket, Expire, SocketList) ->
%%            {ok, NewSocketList} |
%%            {error, Reason}
%%
%%            Type       = listener | in | out
%%            Pid        = pid() "pid of connection handler"
%%            SipSocket  = #sipsocket{}
%%            Expire     = integer() "absolute expiration time in util:timestamp() format - 0 for never expire."
%%            SocketList = #socketlist{}
%%
%%            NewSocketList = #socketlist{}
%%            Reason        = string()
%%
%% @doc     Add a new sipsocket entry to SocketList.
%% @end
%%--------------------------------------------------------------------
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
%% @spec    () ->
%%            SocketList
%%
%%            SocketList = #socketlist{}
%%
%% @doc     Get an empty socketlist.
%% @end
%%--------------------------------------------------------------------
empty() ->
    #socketlist{list = []}.

%%--------------------------------------------------------------------
%% @spec    (Fields, SListElem) ->
%%            Values
%%
%%            Fields    = [slist_field()]
%%            SListElem = #socketlistelem{}
%%
%%            Values = list()
%%
%% @doc     Return one or more values from a socketlistelem record.
%% @end
%%--------------------------------------------------------------------
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
%% @spec    (Pid, SocketList) ->
%%            NewSocketList
%%
%%            Pid        = pid()
%%            SocketList = #socketlist{}
%%
%%            NewSocketList = #socketlist{}
%%
%% @doc     Delete an element from SocketList identified by a pid.
%% @end
%%--------------------------------------------------------------------
delete_using_pid(Pid, SocketList) when is_pid(Pid), is_record(SocketList, socketlist) ->
    #socketlist{list = del_pid(Pid, SocketList#socketlist.list)}.

%%--------------------------------------------------------------------
%% @spec    (SocketList) ->
%%            NewSocketList
%%
%%            SocketList = #socketlist{}
%%
%%            NewSocketList = #socketlist{}
%%
%% @doc     Delete all elements from SocketList that has a timestamp
%%          less than the current time.
%% @end
%%--------------------------------------------------------------------
delete_expired(SocketList) when is_record(SocketList, socketlist) ->
    Now = util:timestamp(),
    #socketlist{list = del_time(Now, SocketList#socketlist.list)}.

%%--------------------------------------------------------------------
%% @spec    (Id, SocketList) ->
%%            Elem |
%%            []
%%
%%            Id         = term()
%%            SocketList = #socketlist{}
%%
%%            Elem = #socketlistelem{}
%%
%% @doc     Find the first element of SocketList that has an id
%%          matching the supplied Id.
%% @end
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
%% @spec    (Pid, SocketList) ->
%%            NewSocketList |
%%            none
%%
%%            Pid        = pid()
%%            SocketList = #socketlist{}
%%
%%            NewSocketList = #socketlist{}
%%
%% @doc     Return all elements of SocketList that has a pid matching
%%          the supplied Pid.
%% @end
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
%% @spec    (Proto, IP, Port, SocketList) ->
%%            Elem |
%%            none
%%
%%            Proto      = atom()
%%            IP         = string()
%%            Port       = integer()
%%            SocketList = #socketlist{}
%%
%%            Elem = #socketlistelem{}
%%
%% @doc     Find the first element of SocketList that has a remote
%%          matching the supplied Remote, and protocol matches Proto.
%% @end
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


%%--------------------------------------------------------------------
%% @spec    (Id, SocketList) ->
%%            Elem |
%%            none
%%
%%            Id         = #ob_id{}
%%            SocketList = #socketlist{}
%%
%%            Elem = #socketlistelem{}
%%
%% @doc     Return the first element of SocketList that has a socket
%%          with id matching the supplied Id.
%% @end
%%--------------------------------------------------------------------
get_using_socketid(Id, SocketList) when is_record(SocketList, socketlist), is_record(Id, ob_id) ->
    get_using_socketid1(Id, SocketList#socketlist.list).

get_using_socketid1(_Id, []) ->
    none;
get_using_socketid1(Id, [#socketlistelem{sipsocket = #sipsocket{id = Id}} = H | _T]) ->
    %% match
    H;
get_using_socketid1(Id, [H | T]) when is_record(H, socketlistelem) ->
    get_using_socketid1(Id, T).


%%--------------------------------------------------------------------
%% @spec    (SList) ->
%%            Length
%%
%%            SList = #socketlist{}
%%
%%            Length = integer()
%%
%% @doc     Return the number of elements in an socketlist record().
%% @end
%%--------------------------------------------------------------------
get_length(SList) when is_record(SList, socketlist) ->
    length(SList#socketlist.list).

%%--------------------------------------------------------------------
%% @spec    (SList) ->
%%            Data
%%
%%            SList = #socketlist{}
%%
%%            Data = term()
%%
%% @doc     Return information about the elements in an socketlist
%%          record in a format that is suitable for logging using ~p.
%% @end
%%--------------------------------------------------------------------
debugfriendly(SList) when is_record(SList, socketlist) ->
    debugfriendly2(debug, SList#socketlist.list).

%%--------------------------------------------------------------------
%% @spec    (SList) ->
%%            Data
%%
%%            SList = #socketlist{}
%%
%%            Data = term()
%%
%% @doc     Return information about the elements in a socketlist
%%          record in a format that is usable by the monitor program
%%          once written. The monitor program is NOT maintained.
%% @hidden
%% @end
%%--------------------------------------------------------------------
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
		    #yxa_socket_ident{type	= listener,
				      proto	= LProto,
				      hostport	= #hp{l_port = LPort}
				     } ->
			lists:concat(["Listening on: ", LProto, " port ", LPort]);
		    #yxa_socket_ident{type = in} ->
			lists:concat(["From: ", RemoteStr, " to ", LocalStr,
				      " (", Proto, ") (expires: ", ExpireIn, ")"]);
		    #yxa_socket_ident{type = out} ->
			lists:concat(["To: ", RemoteStr, " from ", LocalStr,
				      " (", Proto, ") (expires: ", ExpireIn, ")"])
		end
	end,
    lists:append([lists:flatten(Str)], debugfriendly2(Output, Rest)).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Id, SocketList) ->
%%            NewSocketList
%%
%%            Id         = term()
%%            SocketList = #socketlist{}
%%
%%            NewSocketList = #socketlist{}
%%
%% @doc     Delete an element from SocketList identified by a
%%          reference.
%% @end
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
    logger:log(debug, "socketlist: Extra debug : Record expired :~n~p", [debugfriendly2(debug, [H])]),
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
    #yxa_socket_ident{type		= to,
		      proto		= Proto,
		      hostport		= HP#hp{l_ip	= undefined,
						l_port	= undefined
					       }
		     }.

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    DeadPid1 = spawn(fun() -> ok end),
    Empty = empty(),

    %% add(Type, Pid, SipSocket, Expire, SocketList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add/5 - 1.1"),

    {ok, Add_L1} =
	add(listener, self(), #sipsocket{proto		= yxa_test,
					 hostport	= #hp{l_ip = "192.0.2.1",
							      l_port = 1
							     },
					 id		= #ob_id{id = 1}
					}, 0, Empty),

    autotest:mark(?LINE, "add/5 - 1.2"),
    #socketlist{list = [Add_L1_1]} = Add_L1,

    autotest:mark(?LINE, "add/5 - 1.3"),
    #socketlistelem{id = #yxa_socket_ident{type		= listener,
					   proto	= yxa_test,
					   hostport	= #hp{l_ip = "192.0.2.1",
							      l_port = 1
							     }
					  }
		    } = Add_L1_1,

    autotest:mark(?LINE, "add/5 - 2.1"),
    %% add an expired entry
    Add_L2_2_SipSocket = #sipsocket{proto        = yxa_test,
				    hostport     = #hp{l_ip = "192.0.2.1",
						       l_port = 1,
						       r_ip = "192.0.2.2",
						       r_port = 2
						      },
				    id           = #ob_id{id = 2}
				   },
    {ok, Add_L2} =
	add(in, self(), Add_L2_2_SipSocket, util:timestamp() - 10, Add_L1),

    autotest:mark(?LINE, "add/5 - 2.2"),
    #socketlist{list = [Add_L1_2, Add_L1_1]} = Add_L2,

    autotest:mark(?LINE, "add/5 - 2.3"),
    #yxa_socket_ident{type	= from,
		      proto	= yxa_test,
		      hostport	= #hp{l_ip = undefined,
				      l_port = undefined,
				      r_ip = "192.0.2.2",
				      r_port = 2
				    }
		      } = Add_L1_2_Id = Add_L1_2#socketlistelem.id,


    autotest:mark(?LINE, "add/5 - 3.1"),
    %% test adding newer entry (should replace old one)
    {ok, Add_L3_L1} = add(in, self(), Add_L2_2_SipSocket, 10, Empty),
    {ok, Add_L3_L2} = add(in, self(), Add_L2_2_SipSocket, 20, Add_L3_L1),

    autotest:mark(?LINE, "add/5 - 3.2"),
    %% check result
    #socketlist{list = [Add_L3_L1_Elem1]} = Add_L3_L1,
    #socketlist{list = [Add_L3_L2_Elem1]} = Add_L3_L2,

    10 = Add_L3_L1_Elem1#socketlistelem.expire,
    20 = Add_L3_L2_Elem1#socketlistelem.expire,

    false = (Add_L3_L1_Elem1#socketlistelem.ref == Add_L3_L2_Elem1#socketlistelem.ref),

    autotest:mark(?LINE, "add/5 - 3.2"),
    %% verify our new element
    #socketlistelem{proto	= yxa_test,
		    id		= #yxa_socket_ident{type	= from,
						    proto	= yxa_test,
						    hostport	= #hp{l_ip = undefined,
								      l_port = undefined,
								      r_ip = "192.0.2.2",
								      r_port = 2
								     }
						   },
		    sipsocket	= Add_L2_2_SipSocket
		   } = Add_L3_L2_Elem1,


    autotest:mark(?LINE, "add/5 - 4"),
    %% test duplicate id, other pid
    {error, "Duplicate Id, new Pid"} = add(in, DeadPid1, Add_L2_2_SipSocket, 30, Add_L3_L2),


    %% add(Type, Pid, SipSocket, SocketList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add/4 - 1.1"),
    Add4_L1_1_HP = #hp{l_ip = "192.0.2.1",
		       l_port = 1,
		       r_ip = "192.0.2.2",
		       r_port = 2
		      },
    {ok, Add4_L1} =
	add(out, self(), #sipsocket{proto	= yxa_test,
				   hostport	= Add4_L1_1_HP,
				   id		= #ob_id{id = 2}
				  }, Empty),

    autotest:mark(?LINE, "add/4 - 1.2"),
    #socketlist{list = [#socketlistelem{id		= #yxa_socket_ident{type = to},
					hostport	= Add4_L1_1_HP
				       }
		       ]} = Add4_L1,


    %% delete_expired(SocketList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "delete_expired/1 - 1"),
    Add_L1 = delete_expired(Add_L2),


    %% delete_using_pid(Pid, SocketList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "delete_using_pid/2 - 1"),
    Empty = delete_using_pid(self(), Add_L2),

    autotest:mark(?LINE, "delete_using_pid/2 - 2"),
    Add_L2 = delete_using_pid(DeadPid1, Add_L2),


    %% get_length(SList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_length/1 - 1"),
    0 = get_length(Empty),

    autotest:mark(?LINE, "get_length/1 - 2"),
    2 = get_length(Add_L2),


    %% get_using_id(Id, SocketList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_using_id/2 - 1"),
    GetUsingId1 = get_using_id(Add_L1_2_Id, Add_L2),
    Add_L1_2_Id = GetUsingId1#socketlistelem.id,

    autotest:mark(?LINE, "get_using_id/2 - 2"),
    [] = get_using_id(1, Add_L2),


    %% get_using_pid(Pid, SocketList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_using_pid/2 - 1"),
    Add_L2 = get_using_pid(self(), Add_L2),

    autotest:mark(?LINE, "get_using_pid/2 - 2"),
    none = get_using_pid(DeadPid1, Add_L2),


    %% get_using_remote(Proto, IP, Port, SocketList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_using_remote/4 - 1"),
    Add_L1_2 = get_using_remote(yxa_test, "192.0.2.2", 2, Add_L2),

    autotest:mark(?LINE, "get_using_remote/2 - 2"),
    none = get_using_remote(yxa_test, "192.0.2.123", 5, Add_L2),


    %% get_using_socketid(Id, SocketList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_using_socketid/2 - 1"),
    Add_L1_2 = get_using_socketid(#ob_id{id = 2}, Add_L2),

    autotest:mark(?LINE, "get_using_socketid/2 - 2"),
    none = get_using_socketid(#ob_id{id = foo}, Add_L2),


    %% extract(Fields, SListElem)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "extract/2 - 1"),
    Extract_Id1 = Add_L1_2#socketlistelem.id,
    [Extract_Id1] = extract([id], Add_L1_2),

    autotest:mark(?LINE, "extract/2 - 2"),
    Extract_Pid1 = Add_L1_2#socketlistelem.pid,
    [Extract_Pid1] = extract([pid], Add_L1_2),

    autotest:mark(?LINE, "extract/2 - 3"),
    Extract_Proto1 = Add_L1_2#socketlistelem.proto,
    [Extract_Proto1] = extract([proto], Add_L1_2),

    autotest:mark(?LINE, "extract/2 - 4"),
    Extract_HostPort1 = Add_L1_2#socketlistelem.hostport,
    [Extract_HostPort1] = extract([hostport], Add_L1_2),

    autotest:mark(?LINE, "extract/2 - 5"),
    Extract_Sipsocket1 = Add_L1_2#socketlistelem.sipsocket,
    [Extract_Sipsocket1] = extract([sipsocket], Add_L1_2),

    autotest:mark(?LINE, "extract/2 - 6"),
    Extract_Expire1 = Add_L1_2#socketlistelem.expire,
    [Extract_Expire1] = extract([expire], Add_L1_2),

    autotest:mark(?LINE, "extract/2 - 7"),
    [Extract_Id1, Extract_Pid1] = extract([id, pid], Add_L1_2),


    %% debugfriendly(SList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "debugfriendly/1 - 1"),
    [] = debugfriendly(Empty),

    autotest:mark(?LINE, "debugfriendly/1 - 2"),
    ["id=" ++ _] = debugfriendly(Add_L1),


    %% monitor_format(SList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "monitor_format/1 - 1"),
    ["Listening on" ++ _] = monitor_format(Add_L1),

    ok.
