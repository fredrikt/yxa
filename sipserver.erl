-module(sipserver).
-export([start/5, process/4, get_env/1, get_env/2]).

start(InitFun, RequestFun, ResponseFun, RemoteMnesiaTables, LocalTablesP) ->
    case LocalTablesP of
	true ->
	    mnesia:create_schema([node()]);
	_ ->
	    true
    end,
    mnesia:start(),
    apply(InitFun, []),
    logger:start(),
    case RemoteMnesiaTables of
	none ->
	    logger:log(normal, "proxy started");
	_ ->
	    mnesia:change_config(extra_db_nodes,
				 sipserver:get_env(databaseservers)),
	    {Message, Args} = case mnesia:wait_for_tables(RemoteMnesiaTables, infinity) of
				  ok ->
				      {"proxy started, all tables found", []};
				  {timeout, BadTabList} ->
				      {"proxy started, tables not reachable right now: ~p", BadTabList}
			      end,
	    logger:log(normal, Message, Args)
    end,
    {ok, Socket} = gen_udp:open(5060, [{reuseaddr, true}]),
    recvloop(Socket, RequestFun, ResponseFun).

recvloop(Socket, RequestFun, ResponseFun) ->
    receive
	{udp, Socket, IP, InPortNo, Packet} ->
	    spawn(?MODULE, process, [Packet, Socket, RequestFun, ResponseFun]),
	    recvloop(Socket, RequestFun, ResponseFun)
    end.

process(Packet, Socket, RequestFun, ResponseFun) ->
    case sippacket:parse(Packet) of
	{request, Method, URL, Header, Body} ->
	    apply(RequestFun, [Method, URL, Header, Body, Socket]);
	{response, Status, Reason, Header, Body} ->
	    apply(ResponseFun, [Status, Reason, Header, Body, Socket])
    end.

get_env(Name) ->
    {ok, Value} = application:get_env(Name),
    Value.

get_env(Name, Default) ->
    case application:get_env(Name) of
	{ok, Value} ->
	    Value;
	undefined ->
	    Default
    end.
