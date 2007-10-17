%%%-------------------------------------------------------------------
%%% File    : mysql.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      MySQL client.
%%%
%%% @since     4 Aug 2005 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%
%%% Copyright (c) 2001-2004 Kungliga Tekniska Högskolan
%%% See the file COPYING
%%%
%%% Usage:
%%%
%%%
%%% Call one of the start-functions before any call to fetch/2
%%%
%%%   start_link(Id, Host, User, Password, Database)
%%%   start_link(Id, Host, Port, User, Password, Database)
%%%   start_link(Id, Host, User, Password, Database, LogFun)
%%%   start_link(Id, Host, Port, User, Password, Database, LogFun)
%%%
%%% Id is a connection group identifier. If you want to have more
%%% than one connection to a server (or a set of MySQL replicas),
%%% add more with
%%%
%%%   connect(Id, Host, Port, User, Password, Database, Reconnect)
%%%
%%% use 'undefined' as Port to get default MySQL port number (3306).
%%% MySQL querys will be sent in a per-Id round-robin fashion.
%%% Set Reconnect to 'true' if you want the dispatcher to try and
%%% open a new connection, should this one die.
%%%
%%% When you have a mysql_dispatcher running, this is how you make a
%%% query :
%%%
%%%   fetch(Id, "select * from hello") -> Result
%%%     Result = {ok, Fieldinfo, Rows}
%%%     Rows   = list() of [Row]
%%%     Row    = [string()]
%%%
%%% If you just want a single MySQL connection, or want to manage your
%%% connections yourself, you can use the mysql_conn module as a
%%% stand-alone single MySQL connection. See the comment at the top of
%%% mysql_conn.erl.
%%%
%%%-------------------------------------------------------------------
-module(mysql).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/5,
	 start_link/6,
	 start_link/7,

	 fetch/2,
	 fetch/3,

	 quote/1,
	 asciz_binary/2,

	 connect/7
	]).

%%--------------------------------------------------------------------
%% Internal exports - just for mysql_* modules
%%--------------------------------------------------------------------
-export([log/3,
	 log/4
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  conn_list,	%% list() of mysql_connection record()
	  log_fun	%% undefined | function for logging,
	 }).

-record(mysql_connection, {
	  id,		%% term(), user of 'mysql' modules id of this socket group
	  conn_pid,	%% pid(), mysql_conn process
	  reconnect,	%% true | false, should mysql_dispatcher try to reconnect if this connection dies?
	  host,		%% undefined | string()
	  port,		%% undefined | integer()
	  user,		%% undefined | string()
	  password,	%% undefined | string()
	  database	%% undefined | string()
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, mysql_dispatcher).
-define(CONNECT_TIMEOUT, 5000).
-define(LOCAL_FILES, 128).

-define(PORT, 3306).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Id, Host, User, Password, Database) start_link(Id, Host,
%%          Port, User, Password, Database) start_link(Id, Host,
%%          User, Password, Database, LogFun) start_link(Id, Host,
%%          Port, User, Password, Database, LogFun) ->
%%            {ok, Pid} | ignore | {error, Error}
%%
%%            Id       = term() "first connection-group Id"
%%            Host     = string()
%%            Port     = integer()
%%            User     = string()
%%            Password = string()
%%            Database = string()
%%            LogFun   = undefined | function() of arity 3
%%
%% @doc     Starts the MySQL client gen_server process.
%% @end
%%--------------------------------------------------------------------
start_link(Id, Host, User, Password, Database) when is_list(Host), is_list(User), is_list(Password),
						    is_list(Database) ->
    start_link(Id, Host, ?PORT, User, Password, Database, undefined).

start_link(Id, Host, Port, User, Password, Database) when is_list(Host), is_integer(Port), is_list(User),
							  is_list(Password), is_list(Database) ->
    start_link(Id, Host, Port, User, Password, Database, undefined);

start_link(Id, Host, User, Password, Database, LogFun) when is_list(Host), is_list(User), is_list(Password),
							    is_list(Database) ->
    start_link(Id, Host, ?PORT, User, Password, Database, LogFun).

start_link(Id, Host, Port, User, Password, Database, LogFun) when is_list(Host), is_integer(Port), is_list(User),
								  is_list(Password), is_list(Database) ->
    crypto:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Id, Host, Port, User, Password, Database, LogFun], []).

%%--------------------------------------------------------------------
%% @spec    (Id, Query) fetch(Id, Query, Timeout) ->
%%            {ok, FieldInfo, Rows} |
%%            {error, Reason}
%%
%%            Id      = term() "connection-group Id"
%%            Query   = string() "MySQL query in verbatim"
%%            Timeout = integer() | infinity "gen_server timeout value"
%%
%%            FieldInfo = term()
%%            Rows      = [[string()]]
%%            Reason    = term()
%%
%% @doc     Send a query and wait for the result.
%% @end
%%--------------------------------------------------------------------
fetch(Id, Query) when is_list(Query) ->
    gen_server:call(?SERVER, {fetch, Id, Query}).
fetch(Id, Query, Timeout) when is_list(Query) ->
    gen_server:call(?SERVER, {fetch, Id, Query}, Timeout).

%%--------------------------------------------------------------------
%% @spec    (String) ->
%%            Quoted
%%
%%            String = string()
%%
%%            Quoted = string()
%%
%% @doc     Quote a string so that it can be included safely in a
%%          MySQL query.
%% @end
%%--------------------------------------------------------------------
quote(String) when is_list(String) ->
    [34 | lists:reverse([34 | quote(String, [])])].	%% 34 is $"

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->		%% 39 is $'
    quote(Rest, [39, $\\ | Acc]);	%% 39 is $'
quote([34 | Rest], Acc) ->		%% 34 is $"
    quote(Rest, [34, $\\ | Acc]);	%% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).

%%--------------------------------------------------------------------
%% @spec    (Data, Acc) ->
%%            {NewList, Rest}
%%
%%            Data = binary()
%%            Acc  = list() "input accumulator"
%%
%%            NewList = list() "Acc plus what we extracted from Data"
%%            Rest    = binary() "whatever was left of Data, not including the zero-byte"
%%
%% @doc     Find the first zero-byte in Data and add everything before
%%          it to Acc, as a string.
%% @end
%%--------------------------------------------------------------------
asciz_binary(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
asciz_binary(<<0:8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) ->
    asciz_binary(Rest, [C | Acc]).

%%--------------------------------------------------------------------
%% @spec    (Id, Host, Port, User, Password, Database, Reconnect) ->
%%            {ok, ConnPid} | {error, Reason}
%%
%%            Id        = term() "connection-group Id"
%%            Host      = string()
%%            Port      = undefined | integer()
%%            User      = string()
%%            Password  = string()
%%            Database  = string()
%%            Reconnect = true | false
%%
%% @doc     Starts a MySQL connection and, if successfull, registers
%%          it with the mysql_dispatcher.
%% @end
%%--------------------------------------------------------------------
connect(Id, Host, undefined, User, Password, Database, Reconnect) ->
    connect(Id, Host, ?PORT, User, Password, Database, Reconnect);
connect(Id, Host, Port, User, Password, Database, Reconnect) ->
    {ok, LogFun} = gen_server:call(?SERVER, get_logfun),
    case mysql_conn:start(Host, Port, User, Password, Database, LogFun) of
	{ok, ConnPid} ->
	    MysqlConn =
		case Reconnect of
		    true ->
			#mysql_connection{id        = Id,
					  conn_pid  = ConnPid,
					  reconnect = true,
					  host      = Host,
					  port      = Port,
					  user      = User,
					  password  = Password,
					  database  = Database
					 };
		    false ->
			#mysql_connection{id        = Id,
					  conn_pid  = ConnPid,
					  reconnect = false
					 }
		end,
	    case gen_server:call(?SERVER, {add_mysql_connection, MysqlConn}) of
		ok ->
		    {ok, ConnPid};
		Res ->
		    Res
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec    (LogFun, Level, Format) log(LogFun, Level, Format,
%%          Arguments) -> void()
%%
%%            LogFun    = undefined | function() with arity 3
%%            Level     = debug | normal | error
%%            Format    = string()
%%            Arguments = [term()]
%%
%% @doc     Either call the function LogFun with the Level, Format and
%%          Arguments as parameters or log it to the console if
%%          LogFun is undefined. Note : Exported only for use by the
%%          mysql_* modules.
%%
%% @end
%%--------------------------------------------------------------------
log(LogFun, Level, Format) ->
    log(LogFun, Level, Format, []).

log(LogFun, Level, Format, Arguments) when is_function(LogFun) ->
    LogFun(Level, Format, Arguments);
log(undefined, _Level, Format, Arguments) ->
    %% default is to log to console
    io:format(Format, Arguments),
    io:format("~n", []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Args) -> {ok, State} | {ok, State, Timeout} | ignore |
%%          {stop, Reason} -> term()
%%
%%            Args     = [Id, Host, Port, User, Password, Database, LogFun]
%%            Id       = term() "connection-group Id"
%%            Host     = string()
%%            Port     = integer()
%%            User     = string()
%%            Password = string()
%%            Database = string()
%%            LogFun   = undefined | function() with arity 3
%%
%% @doc     Initiates the gen_server (MySQL dispatcher).
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([Id, Host, Port, User, Password, Database, LogFun]) ->
    case mysql_conn:start(Host, Port, User, Password, Database, LogFun) of
	{ok, ConnPid} ->
	    MysqlConn = #mysql_connection{id        = Id,
					  conn_pid  = ConnPid,
					  reconnect = true,
					  host	    = Host,
					  port      = Port,
					  user      = User,
					  password  = Password,
					  database  = Database
					 },
	    case add_mysql_conn(MysqlConn, []) of
		{ok, ConnList} ->
		    {ok, #state{log_fun    = LogFun,
				conn_list = ConnList
			       }};
		error ->
		    Msg = "mysql: Failed adding first MySQL connection handler to my list, exiting",
		    log(LogFun, error, Msg),
		    {error, Msg}
	    end;
	{error, Reason} ->
	    log(LogFun, error, "mysql: Failed starting first MySQL connection handler, exiting"),
	    {stop, {error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% @spec    ({fetch, Id, Query}, From, State) ->
%%            {noreply, NewState}             |
%%            {reply, {error, Reason}, State}
%%
%%            Id    = term() "connection-group id"
%%            Query = string() "MySQL query"
%%
%%            NewState = #state{}
%%            Reason   = atom() | string()
%%
%% @doc     Make a MySQL query. Use the first connection matching Id
%%          in our connection-list. Don't block the mysql_dispatcher
%%          by returning {noreply, ...} here and let the mysql_conn
%%          do gen_server:reply(...) when it has an answer.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({fetch, Id, Query}, From, State) ->
    log(State#state.log_fun, debug, "mysql: fetch ~p (id ~p)", [Query, Id]),
    case get_next_mysql_connection_for_id(Id, State#state.conn_list) of
	{ok, MysqlConn, RestOfConnList} when is_record(MysqlConn, mysql_connection) ->
	    mysql_conn:fetch(MysqlConn#mysql_connection.conn_pid, Query, From),
	    %% move this mysql socket to the back of the list
	    NewConnList = RestOfConnList ++ [MysqlConn],
	    %% The ConnPid process does a gen_server:reply() when it has an answer
	    {noreply, State#state{conn_list = NewConnList}};
	nomatch ->
	    %% we have no active connection matching Id
	    {reply, {error, no_connection}, State}
    end;

%%--------------------------------------------------------------------
%% @spec    ({add_mysql_connection, Conn}, From, State) ->
%%            {reply, Reply, NewState}
%%
%%            Conn = #mysql_connection{}
%%
%%            Reply    = ok | {error, Reason}
%%            NewState = #state{}
%%            Reason   = string()
%%
%% @doc     Add Conn to our list of connections.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({add_mysql_connection, Conn}, _From, State) when is_record(Conn, mysql_connection) ->
    case add_mysql_conn(Conn, State#state.conn_list) of
	{ok, NewConnList} ->
	    {Id, ConnPid} = {Conn#mysql_connection.id, Conn#mysql_connection.conn_pid},
	    log(State#state.log_fun, normal, "mysql: Added connection with id '~p' (pid ~p) to my list",
		[Id, ConnPid]),
	    {reply, ok, State#state{conn_list = NewConnList}};
	error ->
	    {reply, {error, "failed adding MySQL connection to my list"}, State}
    end;

%%--------------------------------------------------------------------
%% @spec    (get_logfun, From, State) ->
%%            {reply, {ok, LogFun}, State}
%%
%%            LogFun = undefined | function() with arity 3
%%
%% @doc     Fetch our logfun.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(get_logfun, _From, State) ->
    {reply, {ok, State#state.log_fun}, State};

handle_call(Unknown, _From, State) ->
    log(State#state.log_fun, error, "mysql: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "unknown gen_server call in mysql client"}, State}.


%%--------------------------------------------------------------------
%% @spec    handle_cast(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

handle_cast(Unknown, State) ->
    log(State#state.log_fun, error, "mysql: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @spec    handle_info(Msg, State) ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @doc     Handling all non call/cast messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({'DOWN', ...}, State) ->
%%            {noreply, NewState}   |
%%            {stop, normal, State}
%%
%%            NewState = #state{}
%%
%% @doc     Handle a message that one of our monitored processes
%%          (mysql_conn processes in our connection list) has exited.
%%          Remove the entry from our list. Note : For now, we stop
%%          if our connection list becomes empty. We should try to
%%          reconnect for a while first, to not eventually stop the
%%          whole OTP application if the MySQL- server is shut down
%%          and the mysql_dispatcher was super- vised by an OTP
%%          supervisor.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _MonitorRef, process, Pid, Info}, State) ->
    LogFun = State#state.log_fun,
    case remove_mysql_connection_using_pid(Pid, State#state.conn_list, []) of
	{ok, Conn, NewConnList} ->
	    LogLevel = case Info of
			   normal -> normal;
			   _ -> error
		       end,
	    log(LogFun, LogLevel, "mysql: MySQL connection pid ~p exited : ~p", [Pid, Info]),
	    log(LogFun, normal, "mysql: Removed MySQL connection with pid ~p from list",
		[Pid]),
	    case Conn#mysql_connection.reconnect of
		true ->
		    start_reconnect(Conn, LogFun);
		false ->
		    ok
	    end,
	    {noreply, State#state{conn_list = NewConnList}};
	nomatch ->
	    log(LogFun, error, "mysql: Received 'DOWN' signal from pid ~p not in my list", [Pid]),
	    {noreply, State}
    end;

handle_info(Info, State) ->
    log(State#state.log_fun, error, "mysql: Received unknown signal : ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> Reason
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    LogFun = State#state.log_fun,
    LogLevel = case Reason of
		   normal -> debug;
		   _ -> error
	       end,
    log(LogFun, LogLevel, "mysql: Terminating with reason : ~p", [Reason]),
    Reason.

%%--------------------------------------------------------------------
%% @spec    (_OldVsn, State, _Extra) -> {ok, State}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Conn, ConnList) ->
%%            NewConnList
%%
%%            Conn     = #mysql_connection{}
%%            ConnList = [#mysql_connection{}]
%%
%%            NewConnList = [#mysql_connection{}]
%%
%% @doc     Set up process monitoring of the mysql_conn process and
%%          then add it (first) to ConnList.
%% @end
%%--------------------------------------------------------------------
add_mysql_conn(Conn, ConnList) when is_record(Conn, mysql_connection), is_list(ConnList) ->
    erlang:monitor(process, Conn#mysql_connection.conn_pid),
    {ok, [Conn | ConnList]}.

%%--------------------------------------------------------------------
%% @spec    (Pid, ConnList) ->
%%            {ok, Conn, NewConnList} | nomatch
%%
%%            Pid      = pid()
%%            ConnList = [#mysql_connection{}]
%%
%%            Conn        = #mysql_connection{}
%%            NewConnList = [#mysql_connection{}]
%%
%% @doc     Removes the first mysql_connection in ConnList that has a
%%          pid matching Pid.
%% @end
%%--------------------------------------------------------------------
remove_mysql_connection_using_pid(Pid, [#mysql_connection{conn_pid = Pid} = H | T], Res) ->
    {ok, H, lists:reverse(Res) ++ T};
remove_mysql_connection_using_pid(Pid, [H | T], Res) when is_record(H, mysql_connection) ->
    remove_mysql_connection_using_pid(Pid, T, [H | Res]);
remove_mysql_connection_using_pid(_Pid, [], _Res) ->
    nomatch.

%%--------------------------------------------------------------------
%% @spec    (Id, ConnList) ->
%%            {ok, Conn, NewConnList} | nomatch
%%
%%            Id       = term() "connection-group id"
%%            ConnList = [#mysql_connection{}]
%%
%%            Conn        = #mysql_connection{}
%%            NewConnList = [#mysql_connection{}] "same as ConnList but without Conn"
%%
%% @doc     Find the first mysql_connection in ConnList that has an id
%%          matching Id.
%% @end
%%--------------------------------------------------------------------
get_next_mysql_connection_for_id(Id, ConnList) ->
    get_next_mysql_connection_for_id(Id, ConnList, []).

get_next_mysql_connection_for_id(Id, [#mysql_connection{id = Id} = H | T], Res) ->
    {ok, H, lists:reverse(Res) ++ T};
get_next_mysql_connection_for_id(Id, [H | T], Res) when is_record(H, mysql_connection) ->
    get_next_mysql_connection_for_id(Id, T, [H | Res]);
get_next_mysql_connection_for_id(_Id, [], _Res) ->
    nomatch.

%%--------------------------------------------------------------------
%% @spec    (Conn, LogFun) -> ok
%%
%%            Conn   = #mysql_connection{}
%%            LogFun = undefined | function() with arity 3
%%
%% @doc     Spawns a process that will try to re-establish a new
%%          connection instead of the one in Conn which has just
%%          died.
%% @end
%%--------------------------------------------------------------------
start_reconnect(Conn, LogFun) when is_record(Conn, mysql_connection) ->
    Pid = spawn(fun () ->
			reconnect_loop(Conn#mysql_connection{conn_pid = undefined}, LogFun, 0)
		end),
    {Id, Host, Port} = {Conn#mysql_connection.id, Conn#mysql_connection.host, Conn#mysql_connection.port},
    log(LogFun, debug, "mysql: Started pid ~p to try and reconnect to ~p:~s:~p (replacing "
	"connection with pid ~p)", [Pid, Id, Host, Port, Conn#mysql_connection.conn_pid]),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Conn, LogFun, 0) -> ok
%%
%%            Conn   = #mysql_connection{}
%%            LogFun = undefined | function() with arity 3
%%
%% @doc     Loop indefinately until we are able to reconnect to the
%%          server specified in the now dead connection Conn.
%% @end
%%--------------------------------------------------------------------
reconnect_loop(Conn, LogFun, N) when is_record(Conn, mysql_connection) ->
    {Id, Host, Port} = {Conn#mysql_connection.id, Conn#mysql_connection.host, Conn#mysql_connection.port},
    case connect(Id,
		 Host,
		 Port,
		 Conn#mysql_connection.user,
		 Conn#mysql_connection.password,
		 Conn#mysql_connection.database,
		 Conn#mysql_connection.reconnect) of
	{ok, ConnPid} ->
	    log(LogFun, debug, "mysql_reconnect: Managed to reconnect to ~p:~s:~p (connection pid ~p)",
		[Id, Host, Port, ConnPid]),
	    ok;
	{error, Reason} ->
	    %% log every once in a while
	    NewN = case N of
		       10 ->
			   log(LogFun, debug, "mysql_reconnect: Still unable to connect to ~p:~s:~p (~p)",
			       [Id, Host, Port, Reason]),
			   0;
		       _ ->
			   N + 1
		   end,
	    %% sleep between every unsuccessfull attempt
	    timer:sleep(20 * 1000),
	    reconnect_loop(Conn, LogFun, NewN)
    end.
