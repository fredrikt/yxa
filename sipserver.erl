-module(sipserver).
-export([start/5, process/6, get_env/1, get_env/2, make_logstr/2]).

start(InitFun, RequestFun, ResponseFun, RemoteMnesiaTables, _LocalTablesP) ->
    mnesia:start(),
    apply(InitFun, []),
    logger:start(),
    case RemoteMnesiaTables of
	none ->
	    logger:log(normal, "proxy started");
	_ ->
	    case mnesia:change_config(extra_db_nodes,
				 sipserver:get_env(databaseservers)) of
		{error, Reason} ->
		    logger:log(error, "Startup: Could not add configured databaseservers: ~p", [mnesia:error_description(Reason)]);
		_ ->
		    true
	    end,
	    {Message, Args} = case mnesia:wait_for_tables(RemoteMnesiaTables, infinity) of
				  ok ->
				      {"proxy started, all tables found", []};
				  {timeout, BadTabList} ->
				      {"proxy started, tables not reachable right now: ~p", BadTabList}
			      end,
	    logger:log(normal, Message, Args)
    end,
    {ok, Socket} = gen_udp:open(sipserver:get_env(listenport, 5060), [{reuseaddr, true}]),
    recvloop(Socket, RequestFun, ResponseFun).

recvloop(Socket, RequestFun, ResponseFun) ->
    receive
	{udp, Socket, IPlist, InPortNo, Packet} ->
	    spawn(?MODULE, process, [Packet, Socket, IPlist, InPortNo, RequestFun, ResponseFun]),
	    recvloop(Socket, RequestFun, ResponseFun)
    end.

process(Packet, Socket, IPlist, InPortNo, RequestFun, ResponseFun) ->
    case catch do_process(Packet, Socket, IPlist, InPortNo,
			  RequestFun, ResponseFun) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from do_process()~n~p", [E]),
	    IP = siphost:makeip(IPlist),
	    logger:log(error, "CRASHED processing packet [client=~s]", [IP]),
	    true;
	_ ->
	    true
    end.

internal_error(Header, Socket) ->
    siprequest:send_result(Header, Socket, "", 500, "Server Internal Error").

internal_error(Header, Socket, Code, Text) ->
    siprequest:send_result(Header, Socket, "", Code, Text).

internal_error(Header, Socket, Code, Text, ExtraHeaders) ->
    siprequest:send_result(Header, Socket, "", Code, Text, ExtraHeaders).

do_process(Packet, Socket, IPlist, InPortNo, RequestFun, ResponseFun) ->
    IP = siphost:makeip(IPlist),
    case parse_packet(Socket, Packet, IP, InPortNo) of
	{{request, Method, URL, Header, Body}, LogStr} ->
%	    logger:log(debug, "~s from ~s:~p", [Method, IP, InPortNo]),
	    case catch apply(RequestFun, [Method, URL, Header, Body, Socket, IP]) of
		{'EXIT', E} ->
		    logger:log(error, "=ERROR REPORT==== from RequestFun~n~p", [E]),
		    internal_error(Header, Socket);
		{siperror, Code, Text} ->
		    logger:log(error, "INVALID request: ~s -> ~p ~s", [LogStr, Code, Text]),
		    internal_error(Header, Socket, Code, Text);
		{siperror, Code, Text, ExtraHeaders} ->
		    logger:log(error, "INVALID request: ~s -> ~p ~s", [LogStr, Code, Text]),
		    internal_error(Header, Socket, Code, Text, ExtraHeaders);
		_ ->
		    true
	    end;
	{{response, Status, Reason, Header, Body}, LogStr} ->
	    apply(ResponseFun, [Status, Reason, Header, Body, Socket, IP]);
	_ ->
	    true
    end.

parse_packet(Socket, Packet, IP, InPortNo) ->
    case catch sippacket:parse(Packet, IP, InPortNo) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from sippacket:parse()~n~p", [E]),
	    logger:log(error, "CRASHED parsing packet [client=~s]", [IP]),
	    false;
	{siperror, Code, Text} ->
	    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE", [IP, Code, Text]),
	    false;
	{siperror, Code, Text, ExtraHeaders} ->
	    logger:log(error, "INVALID packet [client=~s] ~p ~s, CAN'T SEND RESPONSE", [IP, Code, Text]),
	    false;
	Parsed ->
	    % From here on, we can generate responses to the UAC on error
	    {Type, Header} = case Parsed of
		{request, Method, _, Header2, _} ->
		    {Method, Header2};
		{response, Status, Reason, Header2, _} ->
		    {integer_to_list(Status) ++ " " ++ Reason, Header2}
	    end,
	    case catch make_logstr(Parsed, IP) of
		{'EXIT', E} ->
		    logger:log(error, "=ERROR REPORT==== from sipserver:make_logstr()~n~p", [E]),
		    internal_error(Header, Socket),
		    logger:log(error, "CRASHED parsing packet [client=~s]", [IP]);
		{siperror, Code, Text} ->
		    logger:log(error, "INVALID packet ~s [client=~s] -> ~p ~s", [Type, IP, Code, Text]),
		    internal_error(Header, Socket, Code, Text);
		{siperror, Code, Text, ExtraHeaders} ->
		    logger:log(error, "INVALID packet ~s [client=~s] -> ~p ~s", [Type, IP, Code, Text]),
		    internal_error(Header, Socket, Code, Text, ExtraHeaders);
		LogStr ->
		    {Parsed, LogStr}
	    end
    end.

make_logstr({request, Method, URL, Header, Body}, IP) ->
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
    io_lib:format("~s ~s [client=~s, from=<~s>, to=<~s>]", 
		[Method, sipurl:print(URL), IP, sipurl:print(FromURI), sipurl:print(ToURI)]);
make_logstr({response, Status, Reason, Header, Body}, IP) ->
    {_, CSeqMethod} = sipheader:cseq(keylist:fetch("CSeq", Header)),
    {_, FromURI} = sipheader:from(keylist:fetch("From", Header)),
    {_, ToURI} = sipheader:to(keylist:fetch("To", Header)),
    case keylist:fetch("Warning", Header) of
	[] ->
	    io_lib:format("~s [client=~s, from=<~s>, to=<~s>]", 
			[CSeqMethod, IP, sipurl:print(FromURI), sipurl:print(ToURI)]);
	[Warning] ->
	    io_lib:format("~s [client=~s, from=<~s>, to=<~s>, warning=~p]", 
			[CSeqMethod, IP, sipurl:print(FromURI), sipurl:print(ToURI), Warning])
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
