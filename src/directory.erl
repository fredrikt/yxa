%%%-------------------------------------------------------------------
%%% File    : directory.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      gen_server that caches connection to LDAP-server
%%%           and do all the querying.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%
%%% Note    : You have to give the server name to every query to make
%%%           it possible for the server process to have cached
%%%           connections to multiple servers in the future.
%%%
%%% Note    : If a server name is given, but we can't connect to it,
%%%           or we loose our connection to a server, we will try to
%%%           reconnect to it once for every query someone asks us to
%%%           send, and also using an exponential backoff timer. The
%%%           reason for the timer is to always try and have a
%%%           connection ready to use when we need it, since SSL
%%%           connects can take a long time (for a SIP transaction).
%%%-------------------------------------------------------------------
-module(directory).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0,
	 start_link/1,
	 ldapsearch_simple/4,
	 ldapsearch/4,
	 get_value/2,
	 lookup_mail2tel/1,
	 lookup_mail2uid/1,
	 lookup_mail2cn/1,
	 lookup_tel2name/1
	]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("directory.hrl").
-include("eldap.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  handle,	%% not_connected | none | ldaphandle record()
	  server,	%% string(), server name
	  querycount,	%% integer(), number of queries we have used the current handle for
	  timeout       %% integer(), exponential backoff for reconnect attempts
	 }).
%% @type ldaphandle() = #ldaphandle{}.
%%                      no description
-record(ldaphandle, {
	  ref		%% term(), eldap handle
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(TIMEOUT, 2 * 1000).		%% check connection timeout
-define(TIMEOUT_MAX, 60 * 1000).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    case yxa_config:get_env(ldap_server) of
	{ok, Server} ->
	    start_link(Server);
	none ->
	    logger:log(debug, "Directory: No LDAP server configured"),
	    start_link(none)
    end.

start_link(Server) ->
    gen_server:start_link({local, ldap_client}, ?MODULE, [Server], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ([Server]) ->
%%            {ok, State}           |
%%            {ok, State, Timeout::integer()}
%%
%%            Server = string()
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([Server]) ->
    Handle =
	case Server of
	    none ->
		none;
	    H when is_record(H, ldaphandle) ->
		H;
	    _ when is_list(Server) ->
		case ldap_connect(Server) of
		    H2 when is_record(H2, ldaphandle) ->
			H2;
		    {error, E} ->
			logger:log(error, "LDAP client: Could not connect to LDAP server ~p "
				   "at the moment : ~p", [Server, E]),
			not_connected
		end
	end,
    State =
	#state{handle     = Handle,
	       server     = Server,
	       querycount = 0,
	       timeout    = ?TIMEOUT
	      },

    case Handle of
	none -> {ok, State};
	_ ->    {ok, State, ?TIMEOUT}
    end.


%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) Description: Handling call
%%          messages ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear


%%--------------------------------------------------------------------
%% @spec    ({simple_search, Server, Type, In, Attribute}, From,
%%          State) ->
%%            {reply, Reply, State, Timeout::integer()}
%%
%%            Server    = string()
%%            Type      = string()
%%            In        = string()
%%            Attribute = string()
%%
%%            Reply = {ok, Res} | {error, Reason}
%%            Res   = string() | none     | error
%%
%% @doc     Perform an LDAP search for a single attribute. Type is the
%%          attribute name of In.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({simple_search, Server, Type, In, Attribute}, _From, State) ->
    case State#state.server of
	Server ->
	    {ok, NewState, Res} = ldapsearch_wrapper(simple, {Type, In, Attribute}, State),
	    {reply, Res, NewState, ?TIMEOUT};
	_ ->
	    {reply, {error, "Different LDAP server"}, State, ?TIMEOUT}
    end;

%%--------------------------------------------------------------------
%% @spec    ({search, Server, Type, In, Attribute}, From, State) ->
%%            {reply, Reply, State, Timeout::integer()}
%%
%%            Server    = string()
%%            Type      = string()
%%            In        = string()
%%            Attribute = string()
%%
%%            Reply     = {ok, Res} | {error, Reason}
%%            Res       = [SearchRes] | none                | error
%%            SearchRes = #ldapres{}
%%
%% @doc     Perform an LDAP search for a single attribute. Type is the
%%          attribute name of In.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({search, Server, Type, In, Attributes}, _From, State) ->
    case State#state.server of
	Server ->
	    {ok, NewState, Res} = ldapsearch_wrapper(full, {Type, In, Attributes}, State),
	    {reply, Res, NewState, ?TIMEOUT};
	_ ->
	    {reply, {error, "Different LDAP server"}, State, ?TIMEOUT}
    end.

%%--------------------------------------------------------------------
%% @spec    handle_cast(Msg, State) Description: Handling cast
%%          messages ->
%%            {noreply, State}          |
%%            {noreply, State, Timeout} |
%%            {stop, Reason, State}
%%
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({ping_of_death, Pid}, State) -> {stop, normal, State}
%%
%% @doc     Log a debug message, then exit. We will get restarted by
%%          the sipserver_sup OTP supervisor.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast({ping_of_death, Pid}, State) ->
    logger:log(debug, "LDAP client: Pinged with death by ~p, handle ~p server ~p query count ~p",
	       [Pid, State#state.handle, State#state.server, State#state.querycount]),
    {stop, normal, State}.

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
%% @spec    (timeout, State) -> {noreply, NewState, Timeout::integer()}
%%
%% @doc     Check if we should try to re-open our LDAP handle every
%%          time we have been idle for 2 seconds
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{handle = none} = State) ->
    %% short-circuit the reconnection-on-timeout when no LDAP server has been configured
    {noreply, State};
handle_info(timeout, State) ->
    Server = State#state.server,
    case should_reopen_connection(State) of
	true ->
	    case catch ldap_connect(Server) of
		NewHandle when is_record(NewHandle, ldaphandle) ->
		    logger:log(debug, "LDAP client: Opened new connection to server ~s, closing old", [Server]),
		    ldap_close(State#state.handle),
		    NewState =
			State#state{handle     = NewHandle,
				    querycount = 0,
				    timeout    = ?TIMEOUT
				   },
		    {noreply, NewState, ?TIMEOUT};
		E ->
		    OldTimeout = State#state.timeout,
		    NewTimeout = lists:min([?TIMEOUT_MAX, OldTimeout * 2]),	%% exponential backoff
		    logger:log(error, "LDAP client: Could not reconnect to LDAP server ~p : ~p",
			       [Server, E]),
		    %% Keep using old handle in hope that it is still working.
		    NewState = State#state{timeout = NewTimeout},
		    {noreply, NewState, NewTimeout}
	    end;
	false ->
	    {noreply, State, ?TIMEOUT}
    end.


%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    case Reason of
	normal -> true;
	_ -> logger:log(error, "LDAP client terminating : ~p", [Reason])
    end,
    ldap_close(State#state.handle),
    Reason.


%%--------------------------------------------------------------------
%% @spec    (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec    (State) -> true | false
%%
%% @doc     Determine if it is time to try and re-open our connection
%%          to the LDAP server.
%% @end
%%--------------------------------------------------------------------
should_reopen_connection(State) when is_record(State, state) ->
    QueryCount = State#state.querycount,
    Server = State#state.server,
    case State#state.handle of
	H when is_record(H, ldaphandle) ->
	    case util:safe_is_process_alive(H#ldaphandle.ref) of
		{false, _} ->
		    true;
		{true, _} ->
		    QueryCount = State#state.querycount,
		    {ok, QueryLimit} = yxa_config:get_env(ldap_connection_query_limit),
		    if
			QueryCount >= QueryLimit ->
			    logger:log(debug, "LDAP client: This connections query limit (~p) is exceeded (~p).",
				       [QueryLimit, QueryCount]),
			    true;
			true -> false
		    end
	    end;
	H ->
	    logger:log(debug, "LDAP client: Trying to open new connection to server ~p since current "
		       "handle is broken (handle ~p, query count ~p)",
		       [Server, H, QueryCount]),
	    true
    end.

%%--------------------------------------------------------------------
%% @spec    (Mode, Args, State) ->
%%            {ok, NewState, Res}
%%
%%            Mode  = simple | full
%%            Args  = term()
%%            State = #state{}
%%
%%            NewHandle = #ldaphandle{}
%%            Res       = {ok, SearchRes} | {error, Reason}
%%
%% @doc     Perform an LDAP search (using exec_ldapsearch()) and, if
%%          the search results in 'error', close the connection, try
%%          to open a new one and repeat the search.
%% @end
%%--------------------------------------------------------------------
ldapsearch_wrapper(Mode, Args, State) when Mode == simple orelse Mode == full, is_record(State, state) ->
    case ensure_connected(State) of
	{ok, NewState1} ->
	    case exec_ldapsearch(Mode, NewState1#state.handle, Args) of
		error ->
		    ldap_close(NewState1#state.handle),
		    NewState2 = NewState1#state{handle = not_connected},
		    ldapsearch_wrapper_retry(Mode, Args, NewState2);
		Res ->
		    QC = NewState1#state.querycount,
		    NewState = NewState1#state{querycount = QC + 1},
		    {ok, NewState, {ok, Res}}
	    end;
	{error, Reason} ->
	    {ok, State, {error, Reason}}
    end.

%% part of ldapsearch_wrapper/3, retry a failed query once
%% Returns : same as ldapsearch_wrapper/3
ldapsearch_wrapper_retry(Mode, Args, State) ->
    case ensure_connected(State) of
	{ok, NewState1} ->
	    logger:log(debug, "LDAP client: Retrying query using new LDAP handle ~p",
		       [NewState1#state.handle]),
	    Res = exec_ldapsearch(Mode, NewState1#state.handle, Args),
	    NewState = NewState1#state{querycount = 1},
	    case Res of
		error ->
		    %% if the same query fails twice, it might be a problem with the
		    %% query and not the server
		    {ok, NewState, {error, "query failed"}};
		_ ->
		    {ok, NewState, {ok, Res}}
	    end;
	{error, Reason} ->
	    {ok, State, {error, Reason}}
    end.

%% part of ldapsearch_wrapper/3
%% Returns : {ok, NewState} | {error, Reason}
ensure_connected(State) when is_record(State, state) ->
    case State#state.handle of
	H when is_record(H, ldaphandle) ->
	    {ok, State};
	none ->
	    {error, "LDAP client disabled"};
	not_connected ->
	    case catch ldap_connect(State#state.server) of
		NewHandle when is_record(NewHandle, ldaphandle) ->
		    NewState =
			State#state{handle     = NewHandle,
				    querycount = 0
				   },
		    {ok, NewState};
		{error, "eldap:open failed"} ->
		    {error, "server unavailable"};
		Unknown ->
		    logger:log(debug, "LDAP client: Could not connect to LDAP server ~p, ldap_connect() returned ~p",
			       [State#state.server, Unknown]),
		    {error, "server unavailable"}
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Mode, Handle, {Type, In, AttrIn}) ->
%%            [SearchRes] | none | error
%%
%%            Mode   = simple | full
%%            Handle = #ldaphandle{}
%%            Type   = string()
%%            In     = string()
%%            AttrIn = [string()] | string()
%%            State  = #state{}
%%
%%            SearchRes = #ldapres{} | string()
%%
%% @doc     Perform an LDAP search, and depending on Mode return
%%          either just the result values or the whole set of
%%          results, complete with DN information and all. Note : If
%%          you make a simple query with AttrIn being a [string()],
%%          then the subsequent query will fail mysteriously. Seems
%%          to be a bug in eldap module.
%% @end
%%--------------------------------------------------------------------
exec_ldapsearch(Mode, Handle, {Type, In, AttrIn}) when is_record(Handle, ldaphandle) ->
    Attr = case {Mode, AttrIn} of
	       {simple, [Attr1]} when is_list(Attr1) ->
		   [Attr1];
	       {simple, Attr1} when is_list(Attr1) ->
		   [Attr1];
	       {full, Attr1} when is_list(Attr1) ->
		   Attr1
	   end,
    case catch exec_ldapsearch_unsafe(Handle, Type, In, Attr) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from ldapsearch_unsafe(~p, ~p, ~p, ~p) :~n~p",
		       [Handle, Type, In, Attr, E]),
	    error;
	none ->
	    none;
	error ->
	    error;
	[H | _] = SearchRes when is_record(H, ldapres) ->
	    case Mode of
		simple ->
		    [FindAttr] = Attr,
		    case lists:sort(get_valuelist(SearchRes, FindAttr)) of
			[] ->
			    none;
			Res when is_list(Res) ->
			    Res
		    end;
		full ->
		    SearchRes
	    end;
	Unknown ->
	    logger:log(error, "LDAP client: Unknown result from ldapsearch_unsafe (handle ~p, query ~p ~p) :~n~p",
		       [Handle, Type, In, Unknown]),
	    error
    end;
exec_ldapsearch(_Mode, Handle, {Type, In, _AttrIn}) ->
    logger:log(error, "LDAP client: Returning 'error' on query ~p ~p because handle '~p' is not valid", [Type, In, Handle]),
    error.

%%--------------------------------------------------------------------
%% @spec    (L, Attribute) -> [string()]
%%
%%            L         = [{Key, Value}]
%%            Attribute = string()
%%            Key       = string()
%%            Value     = string()
%%
%% @doc     Find the interesting attributes from an LDAP search result
%%          list. L is typically the attribute that was requested
%%          with a 'simple' LDAP search, but since we share code for
%%          'simple' and 'full' searches the result is formatted as
%%          if there might have been more than one result.
%% @end
%%--------------------------------------------------------------------
get_valuelist(L, Attribute) ->
    get_valuelist2(L, Attribute, []).

get_valuelist2([], _Attribute, Res) ->
    Res;
get_valuelist2([H | T], Attribute, Res) ->
    case get_value(H, Attribute) of
    	none ->
	    get_valuelist2(T, Attribute, Res);
	V ->
	    get_valuelist2(T, Attribute, lists:append(Res, [V]))
    end.

%%--------------------------------------------------------------------
%% @spec    (LHandle, Type, In, Attributes) ->
%%            [#ldapres{}] | none | error
%%
%%            LHandle    = #ldaphandle{}
%%            Type       = string()
%%            In         = string()
%%            Attributes = [string()]
%%
%% @doc     Perform an LDAP search in a catch context, since we might
%%          crash.
%% @end
%%--------------------------------------------------------------------
exec_ldapsearch_unsafe(LHandle, Type, In, Attributes) when is_record(LHandle, ldaphandle), is_list(Type),
							   is_list(In), is_list(Attributes) ->
    Handle = LHandle#ldaphandle.ref,
    Filter = eldap:equalityMatch(Type, In),
    SearchBase = case yxa_config:get_env(ldap_searchbase) of
		     {ok, SearchBase1} -> SearchBase1;
		     none -> ""
		 end,
    case eldap:search(Handle, [{base, SearchBase},
			       {filter, Filter},
			       {attributes, Attributes}]) of
	{ok, #eldap_search_result{entries = List}} ->
	    case List of
		[] ->
		    none;
		[{error, Msg}] ->
		    logger:log(normal, "LDAP client: Search using handle ~p failed: ~p", [Handle, Msg]),
		    error;
		[#eldap_entry{} | _] = Results ->
		    get_ldapres(Results);
		Unknown ->
		    logger:log(error, "LDAP client: Search using handle ~p returned unknown data (2) : ~p",
			       [Handle, Unknown]),
		    error
	    end;
	{error, {gen_tcp_error, closed}} ->
	    logger:log(debug, "LDAP client: Handle ~p could not be used (socket closed)", [Handle]),
	    error;
	Unknown ->
	    logger:log(error, "LDAP client: Search using handle ~p returned unknown data (1) : ~p",
		       [Handle, Unknown]),
	    error
    end;
exec_ldapsearch_unsafe(H, Type, In, Arguments) when is_list(Type), is_list(In), is_list(Arguments) ->
    logger:log(debug, "LDAP client: exec_ldapsearch_unsafe() called with broken handle ~p - returning error", [H]),
    error;
exec_ldapsearch_unsafe(Handle, Type, In, Arguments) ->
    logger:log(error, "LDAP client: exec_ldapsearch_unsafe() called with illegal arguments :~n" ++
	       "Handle ~p, Type ~p, In ~p, Arguments", [Handle, Type, In, Arguments]),
    error.

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Res
%%
%%            In = [#eldap_entry{}]
%%
%%            Res = [#ldapres{}]
%%
%% @doc     Turn a list of eldap module query results into our own
%%          result format (ldapres records).
%% @end
%%--------------------------------------------------------------------
get_ldapres(In) ->
    get_ldapres(In, []).

get_ldapres([], Res) ->
    lists:reverse(Res);
get_ldapres([#eldap_entry{object_name = Dn, attributes = RAttributes} | T], Res) ->
    This = #ldapres{dn         = Dn,
		    attributes = RAttributes
		   },
    get_ldapres(T, [This | Res]).

%%--------------------------------------------------------------------
%% @spec    (Server) ->
%%            Handle          |
%%            {error, Reason}
%%
%%            Server = string()
%%
%%            Handle = #ldaphandle{}
%%
%% @doc     Connect to LDAP server Server.
%% @end
%%--------------------------------------------------------------------
ldap_connect(Server) when is_list(Server) ->
    {Host, Port} = sipparse_util:parse_hostport(Server),
    Options =
	case yxa_config:get_env(ldap_use_ssl) of
	    {ok, true} ->
		case Port of
		    none -> [{ssl, true}, {port, 636}];
		    _ -> [{ssl, true}, {port, Port}]
		end;
	    {ok, false} ->
		case Port of
		    none -> [];
		    _ -> [{port, Port}]
		end
	end,

    case eldap:open([Host], Options) of
	{ok, Handle} ->
	    Username = case yxa_config:get_env(ldap_username) of
			   {ok, Username1} -> Username1;
			   none -> ""
		       end,
	    Password = case yxa_config:get_env(ldap_password) of
			   {ok, Password1} -> Password1;
			   none -> ""
		       end,
	    case eldap:simple_bind(Handle, Username, Password) of
		ok ->
		    logger:log(debug, "LDAP client: Opened LDAP handle ~p (server ~s)", [Handle, Server]),
		    #ldaphandle{ref = Handle};
		E ->
		    logger:log(debug, "LDAP client: ldap_connect: eldap:simple_bind failed : ~p", [E]),
		    {error, "eldap:simple_bind failed"}
	    end;
	{error, E} ->
	    logger:log(debug, "LDAP client: ldap_connect: eldap:open() failed. Server ~p, options ~p : ~p",
		       [Server, Options, E]),
	    {error, "eldap:open failed"};
	Unknown ->
	    logger:log(debug, "LDAP client: ldap_connect: eldap:open() Server ~p, options ~p failed - unknown data returned: ~p",
		       [Server, Options, Unknown]),
	    {error, "eldap:open failed"}
    end.

%%--------------------------------------------------------------------
%% @spec    (H) -> ok
%%
%%            H = #ldaphandle{}
%%
%% @doc     Close connection to LDAP server.
%% @end
%%--------------------------------------------------------------------
ldap_close(H) when is_record(H, ldaphandle) ->
    logger:log(debug, "LDAP client: Closing LDAP handle '~p'", [H#ldaphandle.ref]),
    eldap:close(H#ldaphandle.ref),
    ok;
ldap_close(_) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (Query) ->
%%            Result |
%%            error
%%
%%            Query = term()
%%
%%            Result = term() "result returned by persistent server"
%%
%% @doc     Send a query to the persistent directory gen_server, and
%%          return the results. If the persistent process fails in
%%          any way, we kill it so that it gets restarted and
%%          hopefully works better for the next query. Note :
%%          query_ldapclient is executed by the interface functions
%%          in the calling process, not in the persistent ldap_client
%%          gen_server.
%% @end
%%--------------------------------------------------------------------
query_ldapclient(Query) ->
    case util:safe_is_process_alive(ldap_client) of
	{true, Pid} ->
	    %% We must remember which pid we send this query to, so that if the ldap_client
	    %% process dies while processing our query and the sipserver_sup starts a new
	    %% ldap_client, we don't kill the new ldap_client with our ping_of_death
	    case catch gen_server:call(Pid, Query, 1500) of
		{ok, Res} ->
		    Res;
		{error, E} ->
		    logger:log(error, "Directory: LDAP client ~p returned error : ~p", [Pid, E]),
		    error;
		{'EXIT', {timeout, _}} ->
		    logger:log(error, "Directory: LDAP client ~p timed out, killing it", [Pid]),
		    exit(Pid, "You failed to answer a query"),
		    error;
		Unknown ->
		    logger:log(error, "Directory: LDAP client ~p returned unknown result : ~p", [Pid, Unknown]),
		    catch gen_server:cast(Pid, {ping_of_death, self()}),
		    error
	    end;
	{false, Pid} ->
	    logger:log(error, "Directory: LDAP client '~p' not alive", [Pid]),
	    error
    end.


%%--------------------------------------------------------------------
%%% Interface functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec    (LDAPres, Attribute) ->
%%            Value
%%
%%            LDAPres     = #ldapres{}
%%            EAttributes = term()
%%            Attribute   = string()
%%
%%            Value = string() | none
%%
%% @doc     Find the Attribute we are looking for in the result list
%%          EAttributes. Returns the value of the first found
%%          matching attribute. Does not indicate in any way if there
%%          were multiple matches.
%% @end
%%--------------------------------------------------------------------
get_value(#ldapres{attributes=EAttributes}, Attribute) when is_list(Attribute) ->
    get_value2(EAttributes, Attribute).

%% get_value2/2 is not exported. EAttributes is a list() of {Key, Value}
%% tuples - look for the _first one_ where Key equals Attribute.
get_value2(EAttributes, Attribute) when is_list(EAttributes), is_list(Attribute) ->
    case lists:keysearch(Attribute, 1, EAttributes) of
	{value, {Attribute, [Value]}} -> Value;
	_ -> none
    end.

%%--------------------------------------------------------------------
%% @spec    (Server, Type, In, Attribute) ->
%%            Value |
%%            none  |
%%            error
%%
%%            Server    = string()
%%            Type      = string()
%%            In        = string()
%%            Attribute = string()
%%
%%            Value = string()
%%
%% @doc     Query LDAP for a single Attribute and return a single
%%          result.
%% @end
%%--------------------------------------------------------------------
ldapsearch_simple(Server, Type, In, Attribute) when is_list(Server), is_list(Type), is_list(In), is_list(Attribute) ->
    logger:log(debug, "Directory: Extra debug: ldapsearch_simple() Server ~p, Type ~p, In ~p, Attribute ~p",
	       [Server, Type, In, Attribute]),
    query_ldapclient({simple_search, Server, Type, In, Attribute}).

%%--------------------------------------------------------------------
%% @spec    (Server, Type, In, Attributes) ->
%%            Result |
%%            none   |
%%            error
%%
%%            Server     = string()
%%            Type       = string()
%%            In         = string()
%%            Attributes = [string()]
%%
%%            Result = [#ldapres{}]
%%
%% @doc     Query LDAP. Return a list of our result tuples.
%% @end
%%--------------------------------------------------------------------
ldapsearch(Server, Type, In, Attributes) when is_list(Server), is_list(Type), is_list(In), is_list(Attributes) ->
    logger:log(debug, "Directory: Extra debug: ldapsearch() Server ~p, Type ~p, In ~p, Attributes ~p",
	       [Server, Type, In, Attributes]),
    query_ldapclient({search, Server, Type, In, Attributes}).


%% Specific querys

%%--------------------------------------------------------------------
%% @spec    (Mail) ->
%%            Value |
%%            none  |
%%            error
%%
%%            Mail = string() "e-mail address"
%%
%%            Value = string()
%%
%% @doc     Find the telephoneNumber of a user that has Mail as mail-
%%          attribute in LDAP.
%% @end
%%--------------------------------------------------------------------
lookup_mail2tel(Mail) ->
    case yxa_config:get_env(ldap_server) of
	{ok, Server} ->
	    Res = ldapsearch_simple(Server, "mail", Mail, "telephoneNumber"),
	    logger:log(debug, "Directory: LDAP mail -> telephoneNumber lookup on ~p -> ~p", [Mail, Res]),
	    Res;
	none ->
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (Mail) ->
%%            Value |
%%            none  |
%%            error
%%
%%            Mail = string() "e-mail address"
%%
%%            Value = string()
%%
%% @doc     Find the uid of a user that has Mail as mail-attribute in
%%          LDAP.
%% @end
%%--------------------------------------------------------------------
lookup_mail2uid(Mail) ->
    case yxa_config:get_env(ldap_server) of
	{ok, Server} ->
	    Res = ldapsearch_simple(Server, "mail", Mail, "uid"),
	    logger:log(debug, "Directory: LDAP uid lookup on ~p -> ~p", [Mail, Res]),
	    Res;
	none ->
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (Mail) ->
%%            Value |
%%            none  |
%%            error
%%
%%            Mail = string() "e-mail address"
%%
%%            Value = string()
%%
%% @doc     Find the cn (common name) of a user that has Mail as
%%          mail-attribute in LDAP.
%% @end
%%--------------------------------------------------------------------
lookup_mail2cn(Mail) ->
    case yxa_config:get_env(ldap_server) of
	{ok, Server} ->
	    Res = ldapsearch_simple(Server, "mail", Mail, "cn"),
	    logger:log(debug, "Directory: LDAP server ~s cn lookup on ~p -> ~p", [Server, Mail, Res]),
	    Res;
	none ->
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (Number) ->
%%            Value |
%%            none  |
%%            error
%%
%%            Number = string() "telephone number"
%%
%%            Value = string()
%%
%% @doc     Find the displayName of a user that has Number as
%%          telephoneNumber in LDAP.
%% @end
%%--------------------------------------------------------------------
lookup_tel2name(Number) ->
    case yxa_config:get_env(ldap_server) of
	{ok, Server} ->
	    Res = ldapsearch_simple(Server, "telephoneNumber", Number, "displayName"),
	    logger:log(debug, "Directory: LDAP server ~s displayName lookup on ~p -> ~p", [Server, Number, Res]),
	    Res;
	none ->
	    none
    end.

