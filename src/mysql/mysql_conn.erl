%%%-------------------------------------------------------------------
%%% File    : mysql_conn.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      MySQL connection handler, handles de-framing of messages
%%%           received by the MySQL receiver process.
%%% @since     5 Aug 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%
%%% Note    : All MySQL code was written by Magnus Ahltorp, originally
%%%           in the file mysql.erl - I just moved it here.
%%%
%%% Copyright (c) 2001-2004 Kungliga Tekniska Högskolan
%%% See the file COPYING
%%%
%%%
%%% This module handles a single connection to a single MySQL server.
%%% You can use it stand-alone, or through the 'mysql' module if you
%%% want to have more than one connection to the server, or
%%% connections to different servers.
%%%
%%% To use it stand-alone, set up the connection with
%%%
%%%   {ok, Pid} = mysql_conn:start(Host, Port, User, Password,
%%%                                Database, LogFun)
%%%
%%%         Host     = string()
%%%         Port     = integer()
%%%         User     = string()
%%%         Password = string()
%%%         Database = string()
%%%         LogFun   = undefined | (gives logging to console)
%%%                    function() of arity 3 (Level, Fmt, Args)
%%%
%%% and then make MySQL querys with
%%%
%%%   Result = mysql_conn:fetch(Pid, Query, self())
%%%
%%%         Result = {ok, Fields, Rows} |
%%%                  {error, Reason}
%%%
%%%-------------------------------------------------------------------

-module(mysql_conn).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start/6,
	 fetch/3,
	 fetch/4
	]).

%%--------------------------------------------------------------------
%% External exports (should only be used by the 'mysql_auth' module)
%%--------------------------------------------------------------------
-export([do_recv/3
	]).

%% @type state() = #state{}.
%%                 no description
-record(state, {
	  log_fun,
	  recv_pid,
	  socket,
	  data
	 }).

-define(SECURE_CONNECTION, 32768).
-define(MYSQL_QUERY_OP, 3).
-define(DEFAULT_STANDALONE_TIMEOUT, 5000).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Host, Port, User, Password, Database, LogFun) ->
%%            {ok, Pid} | {error, Reason}
%%
%%            Host     = string()
%%            Port     = integer()
%%            User     = string()
%%            Password = string()
%%            Database = string()
%%            LogFun   = undefined | function() of arity 3
%%
%%            Pid    = pid()
%%            Reason = string()
%%
%% @doc     Starts a mysql_conn process that connects to a MySQL
%%          server, logs in and chooses a database.
%% @end
%%--------------------------------------------------------------------
start(Host, Port, User, Password, Database, LogFun) when is_list(Host), is_integer(Port), is_list(User),
							 is_list(Password), is_list(Database) ->
    ConnPid = self(),
    Pid = spawn(fun () ->
			init(Host, Port, User, Password, Database, LogFun, ConnPid)
		end),
    receive
	{mysql_conn, Pid, ok} ->
	    {ok, Pid};
	{mysql_conn, Pid, {error, Reason}} ->
	    {error, Reason};
	Unknown ->
	    mysql:log(LogFun, error, "mysql_conn: Received unknown signal, exiting"),
	    mysql:log(LogFun, debug, "mysql_conn: Unknown signal : ~p", [Unknown]),
	    {error, "unknown signal received"}
    after 5000 ->
	    {error, "timed out"}
    end.

%%--------------------------------------------------------------------
%% @spec    (Pid, Query, From) fetch(Pid, Query, From, Timeout) ->
%%            ok                    |
%%            {ok, FieldInfo, Rows} |
%%            {error, Reason}         (stand-alone mode)
%%
%%            Pid     = pid() "mysql_conn to send fetch-request to"
%%            Query   = string() "MySQL query in verbatim"
%%            From    = pid() or term() "use a From of self() when using this module for a single connection, or pass the gen_server:call/3 From argument if using a gen_server to do the querys (e.g. the mysql_dispatcher)"
%%            Timeout = integer() | infinity "gen_server timeout value"
%%
%%            FieldInfo = term()
%%            Rows      = [[string()]]
%%            Reason    = term()
%%
%% @doc     Send a query and wait for the result if running stand-
%%          alone (From = self()), but don't block the caller if we
%%          are not running stand-alone (From = gen_server From).
%% @end
%%--------------------------------------------------------------------
fetch(Pid, Query, From) ->
    fetch(Pid, Query, From, ?DEFAULT_STANDALONE_TIMEOUT).

fetch(Pid, Query, From, Timeout) when is_pid(Pid), is_list(Query), is_integer(Timeout) ->
    Self = self(),
    Pid ! {fetch, Query, From},
    case From of
	Self ->
	    %% We are not using a mysql_dispatcher, await the response
	    receive
		{fetch_result, Pid, Result} ->
		    Result
	    after Timeout ->
		    {error, "query timed out"}
	    end;
	_ ->
	    %% From is gen_server From, Pid will do gen_server:reply() when it has an answer
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (LogFun, RecvPid, SeqNum) ->
%%            {ok, Packet, Num} |
%%            {error, Reason}
%%
%%            LogFun  = undefined | function() with arity 3
%%            RecvPid = pid() "mysql_recv process"
%%            SeqNum  = undefined | integer()
%%
%%            Reason = term()
%%
%% @doc     Wait for a frame decoded and sent to us by RecvPid. Either
%%          wait for a specific frame if SeqNum is an integer, or
%%          just any frame if SeqNum is undefined. Note : Only to be
%%          used externally by the 'mysql_auth' module.
%% @end
%%--------------------------------------------------------------------
do_recv(LogFun, RecvPid, SeqNum) when is_function(LogFun); LogFun == undefined, SeqNum == undefined ->
    receive
        {mysql_recv, RecvPid, data, Packet, Num} ->
            %%mysql:log(LogFun, debug, "mysql_conn: recv packet ~p: ~p", [Num, Packet]),
	    {ok, Packet, Num};
	{mysql_recv, RecvPid, closed, _E} ->
	    {error, "mysql_recv: socket was closed"}
    end;
do_recv(LogFun, RecvPid, SeqNum) when is_function(LogFun); LogFun == undefined, is_integer(SeqNum) ->
    ResponseNum = SeqNum + 1,
    receive
        {mysql_recv, RecvPid, data, Packet, ResponseNum} ->
            %%mysql:log(LogFun, debug, "mysql_conn: recv packet ~p: ~p", [ResponseNum, Packet]),
	    {ok, Packet, ResponseNum};
	{mysql_recv, RecvPid, closed, _E} ->
	    {error, "mysql_recv: socket was closed"}
        end.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Host, Port, User, Password, Database, LogFun, Parent) ->
%%            void() | does not return
%%
%%            Host     = string()
%%            Port     = integer()
%%            User     = string()
%%            Password = string()
%%            Database = string()
%%            LogFun   = undefined | function() of arity 3
%%            Parent   = pid() of process starting this mysql_conn
%%
%% @doc     Connect to a MySQL server, log in and chooses a database.
%%          Report result of this to Parent, and then enter loop() if
%%          we were successfull.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init(Host, Port, User, Password, Database, LogFun, Parent) ->
    case mysql_recv:start_link(Host, Port, LogFun, self()) of
	{ok, RecvPid, Sock} ->
	    case mysql_init(Sock, RecvPid, User, Password, LogFun) of
		ok ->
		    case do_query(Sock, RecvPid, LogFun, "use " ++ Database) of
			{ok, _Fields, _Rows} ->
			    Parent ! {mysql_conn, self(), ok},
			    State = #state{recv_pid = RecvPid,
					   socket   = Sock,
					   log_fun  = LogFun,
					   data     = <<>>
					  },
			    loop(State);
			{error, Reason} ->
			    mysql:log(LogFun, error, "mysql_conn: Failed changing to database ~p : ~p",
				      [Database, Reason]),
			    Parent ! {mysql_conn, self(), {error, failed_changing_database}}
		    end;
		{error, _Reason} ->
		    Parent ! {mysql_conn, self(), {error, login_failed}}
	    end;
	E ->
	    mysql:log(LogFun, error, "mysql_conn: Failed connecting to ~p:~p : ~p",
		      [Host, Port, E]),
	    Parent ! {mysql_conn, self(), {error, connect_failed}}
    end.

%%--------------------------------------------------------------------
%% @spec    (State) -> error | does not return
%%
%%            State = #state{}
%%
%% @doc     Wait for signals asking us to perform a MySQL query, or
%%          signals that the socket was closed.
%% @end
%%--------------------------------------------------------------------
loop(State) ->
    RecvPid = State#state.recv_pid,
    receive
	{fetch, Query, GenSrvFrom} ->
	    %% GenSrvFrom is either a gen_server:call/3 From term(), or a pid if no
	    %% gen_server was used to make the query
	    Res = do_query(State, Query),
	    case is_pid(GenSrvFrom) of
		true ->
		    %% The query was not sent using gen_server mechanisms
		    GenSrvFrom ! {fetch_result, self(), Res};
		false ->
		    gen_server:reply(GenSrvFrom, Res)
	    end,
	    loop(State);
	{mysql_recv, RecvPid, data, Packet, Num} ->
	    mysql:log(State#state.log_fun, error, "mysql_conn: Received MySQL data when not expecting any "
		      "(num ~p) - ignoring it", [Num]),
	    mysql:log(State#state.log_fun, error, "mysql_conn: Unexpected MySQL data (num ~p) :~n~p",
		      [Num, Packet]),
	    loop(State);
        Unknown ->
	    mysql:log(State#state.log_fun, error, "mysql_conn: Received unknown signal, exiting"),
	    mysql:log(State#state.log_fun, debug, "mysql_conn: Unknown signal : ~p", [Unknown]),
	    error
    end.

%%--------------------------------------------------------------------
%% @spec    (Sock, RecvPid, User, Password, LogFun) ->
%%            ok | {error, Reason}
%%
%%            Sock     = term() "gen_tcp socket"
%%            RecvPid  = pid() "mysql_recv process"
%%            User     = string()
%%            Password = string()
%%            LogFun   = undefined | function() with arity 3
%%
%%            Reason = string()
%%
%% @doc     Try to authenticate on our new socket.
%% @end
%%--------------------------------------------------------------------
mysql_init(Sock, RecvPid, User, Password, LogFun) ->
    case do_recv(LogFun, RecvPid, undefined) of
	{ok, Packet, InitSeqNum} ->
	    {Salt1, Salt2, Caps} = greeting(Packet, LogFun),
	    AuthRes =
		case Caps band ?SECURE_CONNECTION of
		    ?SECURE_CONNECTION ->
			mysql_auth:do_new_auth(Sock, RecvPid, InitSeqNum + 1, User, Password, Salt1, Salt2, LogFun);
		    _ ->
			mysql_auth:do_old_auth(Sock, RecvPid, InitSeqNum + 1, User, Password, Salt1, LogFun)
		end,
	    case AuthRes of
		{ok, <<0:8, _Rest/binary>>, _RecvNum} ->
		    ok;
		{ok, <<255:8, Code:16/little, Message/binary>>, _RecvNum} ->
		    mysql:log(LogFun, error, "mysql_conn: init error ~p: ~p~n", [Code, binary_to_list(Message)]),
		    {error, binary_to_list(Message)};
		{ok, RecvPacket, _RecvNum} ->
		    mysql:log(LogFun, error, "mysql_conn: init unknown error ~p~n", [binary_to_list(RecvPacket)]),
		    {error, binary_to_list(RecvPacket)};
		{error, Reason} ->
		    mysql:log(LogFun, error, "mysql_conn: init failed receiving data : ~p~n", [Reason]),
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%% part of mysql_init/4
greeting(Packet, LogFun) ->
    <<_Protocol:8, Rest/binary>> = Packet,
    {Version, Rest2} = asciz(Rest),
    <<_TreadID:32/little, Rest3/binary>> = Rest2,
    {Salt, Rest4} = asciz(Rest3),
    <<Caps:16/little, Rest5/binary>> = Rest4,
    <<ServerChar:16/binary-unit:8, Rest6/binary>> = Rest5,
    {Salt2, _Rest7} = asciz(Rest6),
    mysql:log(LogFun, debug, "mysql_conn: greeting version ~p salt ~p caps ~p serverchar ~p salt2 ~p",
	      [Version, Salt, Caps, ServerChar, Salt2]),
    {Salt, Salt2, Caps}.

%% part of greeting/2
asciz(Data) when binary(Data) ->
    mysql:asciz_binary(Data, []);
asciz(Data) when list(Data) ->
    {String, [0 | Rest]} = lists:splitwith(fun (C) ->
						   C /= 0
					   end, Data),
    {String, Rest}.

%%--------------------------------------------------------------------
%% @spec    (LogFun, RecvPid) ->
%%            {ok, FieldInfo, Rows} |
%%            {error, Reason}
%%
%%            LogFun  = undefined | function() with arity 3
%%            RecvPid = pid() "mysql_recv process"
%%
%%            FieldInfo = [term()]
%%            Rows      = [[string()]]
%%            Reason    = term()
%%
%% @doc     Wait for frames until we have a complete query response.
%% @end
%%--------------------------------------------------------------------
get_query_response(LogFun, RecvPid) ->
    case do_recv(LogFun, RecvPid, undefined) of
	{ok, <<Fieldcount:8, Rest/binary>>, _} ->
	    case Fieldcount of
		0 ->
		    {ok, [], []};
		255 ->
		    <<_Code:16/little, Message/binary>>  = Rest,
		    {error, binary_to_list(Message)};
		_ ->
		    case get_fields(LogFun, RecvPid, []) of
			{ok, Fields} ->
			    case get_rows(Fieldcount, LogFun, RecvPid, []) of
				{ok, Rows} ->
				    {ok, Fields, Rows};
				{error, Reason} ->
				    {error, Reason}
			    end;
			{error, Reason} ->
			    {error, Reason}
		    end
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec    (LogFun, RecvPid, []) ->
%%            {ok, FieldInfo} |
%%            {error, Reason}
%%
%%            LogFun  = undefined | function() with arity 3
%%            RecvPid = pid() "mysql_recv process"
%%
%%            FieldInfo = [term()]
%%            Reason    = term()
%%
%% @doc     Received and decode field information.
%% @end
%%--------------------------------------------------------------------
get_fields(LogFun, RecvPid, Res) ->
    case do_recv(LogFun, RecvPid, undefined) of
	{ok, Packet, _Num} ->
	    case Packet of
		<<254:8>> ->
		    {ok, lists:reverse(Res)};
		<<254:8, Rest/binary>> when size(Rest) < 8 ->
		    {ok, lists:reverse(Res)};
		_ ->
		    {Table, Rest} = get_with_length(Packet),
		    {Field, Rest2} = get_with_length(Rest),
		    {LengthB, Rest3} = get_with_length(Rest2),
		    LengthL = size(LengthB) * 8,
		    <<Length:LengthL/little>> = LengthB,
		    {Type, Rest4} = get_with_length(Rest3),
		    {_Flags, _Rest5} = get_with_length(Rest4),
		    This = {binary_to_list(Table),
			    binary_to_list(Field),
			    Length,
			    binary_to_list(Type)},
		    get_fields(LogFun, RecvPid, [This | Res])
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec    (N, LogFun, RecvPid, []) ->
%%            {ok, Rows} |
%%            {error, Reason}
%%
%%            N       = integer() "number of rows to get"
%%            LogFun  = undefined | function() with arity 3
%%            RecvPid = pid() "mysql_recv process"
%%
%%            Rows = [[string()]]
%%
%% @doc     Receive and decode a number of rows.
%% @end
%%--------------------------------------------------------------------
get_rows(N, LogFun, RecvPid, Res) ->
    case do_recv(LogFun, RecvPid, undefined) of
	{ok, Packet, _Num} ->
	    case Packet of
		<<254:8, Rest/binary>> when size(Rest) < 8 ->
		    {ok, lists:reverse(Res)};
		_ ->
		    {ok, This} = get_row(N, Packet, []),
		    get_rows(N, LogFun, RecvPid, [This | Res])
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%% part of get_rows/4
get_row(0, _Data, Res) ->
    {ok, lists:reverse(Res)};
get_row(N, Data, Res) ->
    {Col, Rest} = get_with_length(Data),
    This = case Col of
	       null ->
		   null;
	       _ ->
		   binary_to_list(Col)
	   end,
    get_row(N - 1, Rest, [This | Res]).


get_with_length(<<251:8, Rest/binary>>) ->
    {null, Rest};
get_with_length(<<252:8, Length:16/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<253:8, Length:24/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<254:8, Length:64/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<Length:8, Rest/binary>>) when Length < 251 ->
    split_binary(Rest, Length).

%%--------------------------------------------------------------------
%% @spec    (State, Query) do_query(Sock, RecvPid, LogFun, Query) ->
%%            result of get_query_response/2 | {error, Reason}
%%
%%            Sock    = term() "gen_tcp socket"
%%            RecvPid = pid() "mysql_recv process"
%%            LogFun  = undefined | function() with arity 3
%%            Query   = string()
%%
%% @doc     Send a MySQL query and block awaiting it's response.
%% @end
%%--------------------------------------------------------------------
do_query(State, Query) when is_record(State, state) ->
    do_query(State#state.socket,
	     State#state.recv_pid,
	     State#state.log_fun,
	     Query
	    ).

do_query(Sock, RecvPid, LogFun, Query) when is_pid(RecvPid), is_list(Query) ->
    Packet = list_to_binary([?MYSQL_QUERY_OP, Query]),
    case do_send(Sock, Packet, 0, LogFun) of
	ok ->
	    get_query_response(LogFun, RecvPid);
	{error, Reason} ->
	    Msg = io_lib:format("Failed sending data on socket : ~p", [Reason]),
	    {error, Msg}
    end.

%%--------------------------------------------------------------------
%% @spec    (Sock, Packet, SeqNum, LogFun) -> result of gen_tcp:send/2
%%
%%            Sock   = term() "gen_tcp socket"
%%            Packet = binary()
%%            SeqNum = integer() "packet sequence number"
%%            LogFun = undefined | function() with arity 3
%%
%% @doc     Send a packet to the MySQL server.
%% @end
%%--------------------------------------------------------------------
do_send(Sock, Packet, SeqNum, _LogFun) when is_binary(Packet), is_integer(SeqNum) ->
    Data = <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>,
    %%mysql:log(LogFun, debug, "mysql_conn: send packet ~p: ~p", [SeqNum, Data]),
    gen_tcp:send(Sock, Data).
