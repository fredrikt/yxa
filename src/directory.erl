%%%-------------------------------------------------------------------
%%% File    : directory.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: gen_server that caches connection to LDAP-server
%%%           and do all the querying.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%
%%% Note    : You have to give the server name to every query to make
%%%           it possible for the server process to have cached
%%%           connections to multiple servers in the future.
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
-record(state, {
	  handle,
	  server,
	  querycount
	 }).
-record(ldaphandle, {
	  ref
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(TIMEOUT, 2 * 1000).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link()
%% Descrip.: Starts the server
%% Returns : gen_server:start_link() result
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
%% Function: init([Server])
%%           Server = string()
%% Descrip.: Initiates the server
%% Returns : {ok, State}
%% Note    : XXX we don't do timeout here. Accident, or to not produce
%%           work load if noone ever asks us anything?
%%--------------------------------------------------------------------
init([Server]) ->
    Handle = case Server of
		 none -> none;
		 H when is_record(H, ldaphandle) -> H;
		 _ ->
		     case ldap_connect(Server) of
			 H2 when is_record(H2, ldaphandle) -> H2;
			 {error, E} ->
			     logger:log(error, "LDAP client: Could not connect to LDAP server ~p " ++
					"at the moment : ~p", [Server, E]),
			     error
		     end
	     end,
    {ok, #state{handle=Handle, server=Server, querycount=0}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% Function: handle_call({simple_search, Server, Type, In, Attribute},
%%                       From, State)
%%           Server    = string()
%%           Type      = string()
%%           In        = string()
%%           Attribute = string()
%% Descrip.: Perform an LDAP search for a single attribute. Type is
%%           the attribute name of In.
%% Returns:  {reply, Reply, State, ?TIMEOUT}
%%           Reply = {ok, Res}       |
%%                   {error, Reason}
%%           Res = string() |
%%                 none     |
%%                 error
%%--------------------------------------------------------------------
handle_call({simple_search, Server, Type, In, Attribute}, _From, State) ->
    case State#state.server of
	Server ->
	    {NewHandle, Res} = ldapsearch_wrapper(simple, [Type, In, Attribute], State),
	    NewQC = State#state.querycount + 1,
	    NewState = State#state{handle=NewHandle, querycount=NewQC},
	    {reply, {ok, Res}, NewState, ?TIMEOUT};
	_ ->
	    {reply, {error, "Different LDAP server"}, State, ?TIMEOUT}
    end;

%%--------------------------------------------------------------------
%% Function: handle_call({simple_search, Server, Type, In, Attribute},
%%                       From, State)
%%           Server    = string()
%%           Type      = string()
%%           In        = string()
%%           Attribute = string()
%% Descrip.: Perform an LDAP search for a single attribute. Type is
%%           the attribute name of In.
%% Returns:  {reply, Reply, State, ?TIMEOUT}
%%           Reply = {ok, Res}       |
%%                   {error, Reason}
%%           Res = list() of SearchRes |
%%                 none                |
%%                 error
%%                 SearchRes = ldapres record()
%%--------------------------------------------------------------------
handle_call({search, Server, Type, In, Attributes}, _From, State) ->
    case State#state.server of
	Server ->
	    {NewHandle, Res} = ldapsearch_wrapper(full, [Type, In, Attributes], State),
	    NewQC = State#state.querycount + 1,
	    NewState = State#state{handle=NewHandle, querycount=NewQC},
	    {reply, {ok, Res}, NewState, ?TIMEOUT};
	_ ->
	    {reply, {error, "Different LDAP server"}, State, ?TIMEOUT}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_cast({ping_of_death, Pid}, State)
%% Descrip.: Log a debug message, then exit. We will get restarted by
%%           the sipserver_sup OTP supervisor.
%% Returns : {stop, normal, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({ping_of_death, Pid}, State) ->
    logger:log(debug, "LDAP client: Pinged with death by ~p, handle ~p server ~p query count ~p",
	       [Pid, State#state.handle, State#state.server, State#state.querycount]),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info(timeout, State)
%% Descrip.: Check if we should try to re-open our LDAP handle every
%%           time we have been idle for 2 seconds
%% Returns : {noreply, NewState, ?TIMEOUT}
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    Server = State#state.server,
    case should_reopen_connection(State) of
	true ->
	    case catch ldap_connect(Server) of
		H when is_record(H, ldaphandle) ->
		    logger:log(debug, "LDAP client: Opened new connection to server ~s, closing old", [Server]),
		    ldap_close(State#state.handle),
		    {noreply, State#state{handle=H, querycount=0}, ?TIMEOUT};
		E ->
		    logger:log(error, "LDAP client: Could not reconnect to LDAP server ~p : ~p", [Server, E]),
		    %% Keep using old handle in hope that it is still working.
		    {noreply, State, ?TIMEOUT}
	    end;
	_ ->
	    {noreply, State, ?TIMEOUT}
    end.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    case Reason of
	normal -> true;
	_ -> logger:log(error, "LDAP client terminating : ~p", [Reason])
    end,
    ldap_close(State#state.handle),
    Reason.


%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: should_reopen_connection(State)
%% Descrip.: Determine if it is time to try and re-open our connection
%%           to the LDAP server.
%% Returns : true | false
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
	    logger:log(debug, "LDAP client: Trying to open new connection to server ~p since current " ++
		       "handle is broken (handle ~p, query count ~p)",
		       [Server, H, QueryCount]),
	    true
    end.

%%--------------------------------------------------------------------
%% Function: ldapsearch_wrapper(Mode, Args, State)
%%           Mode  = simple | full
%%           Args  = term()
%%           State = state record()
%% Descrip.: Perform an LDAP search (using exec_ldapsearch()) and, if
%%           the search results in 'error', close the connection, try
%%           to open a new one and repeat the search.
%% Returns : {NewHandle, Res}
%%           NewHandle = ldaphandle record()
%%           Res       = term(), result of exec_ldapsearch()
%%--------------------------------------------------------------------
ldapsearch_wrapper(Mode, Args, State) when Mode == simple; Mode == full, is_record(State, state) ->
    Handle = State#state.handle,
    Server = State#state.server,
    case exec_ldapsearch(Mode, Handle, Args) of
	error ->
	    logger:log(error, "LDAP client: Error returned from exec_ldapsearch (~p), " ++
		       "closing LDAP handle '~p', opening new handle and retrying query", [Mode, Handle]),
	    ldap_close(Handle),
	    case catch ldap_connect(Server) of
		NewHandle when is_record(NewHandle, ldaphandle) ->
		    logger:log(debug, "LDAP client: Retrying query using new LDAP handle ~p", [NewHandle]),
		    %% New handle returned, retry query and return new handle plus result of retry
		    Res = exec_ldapsearch(Mode, NewHandle, Args),
		    {NewHandle, Res};
		Unknown ->
		    logger:log(error, "LDAP client: Could not connect to LDAP server ~p, ldap_connect() returned ~p",
			       [Server, Unknown]),
		    %% This is bad. We have closed our old handle and could not get a new one...
		    {error, error}
	    end;
	Res ->
	    {Handle, Res}
    end.

%%--------------------------------------------------------------------
%% Function: exec_ldapsearch(Mode, Handle, [Type, In, AttrIn])
%%           Mode   = simple | full
%%           Handle = ldaphandle record()
%%           Type   = string()
%%           In     = string()
%%           AttrIn = list() of string() | string()
%%           State = state record()
%% Descrip.: Perform an LDAP search, and depending on Mode return
%%           either just the result values or the whole set of
%%           results, complete with DN information and all.
%% Returns : Res = list() of SearchRes |
%%           none                      |
%%           error
%%           SearchRes  = ldapres record() | string()
%% Note    : If you make a simple query with AttrIn being a
%%           [string()], then the subsequent query will fail
%%           mysteriously. Seems to be a bug in eldap module.
%%--------------------------------------------------------------------
exec_ldapsearch(Mode, Handle, [Type, In, AttrIn]) when is_record(Handle, ldaphandle) ->
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
exec_ldapsearch(_Mode, Handle, [Type, In, _AttrIn]) ->
    logger:log(error, "LDAP client: Returning 'error' on query ~p ~p because handle '~p' is not valid", [Type, In, Handle]),
    error.

%%--------------------------------------------------------------------
%% Function: get_valuelist(L, Attribute)
%%           L         = list() of {Key, Value} tuples
%%           Attribute = string()
%%           Key       = string()
%%           Value     = string()
%% Descrip.: Find the interesting attributes from an LDAP search
%%           result list. L is typically the attribute that was
%%           requested with a 'simple' LDAP search, but since we share
%%           code for 'simple' and 'full' searches the result is
%%           formatted as if there might have been more than one
%%           result.
%% Returns : list() of string()
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
%% Function: exec_ldapsearch_unsafe(LHandle, Type, In, Attributes)
%%           LHandle    = ldaphandle record()
%%           Type       = string()
%%           In         = string()
%%           Attributes = list() of string()
%% Descrip.: Perform an LDAP search in a catch context, since we might
%%           crash.
%% Returns : list() of ldapres record() |
%%           none                       |
%%           error
%%--------------------------------------------------------------------
exec_ldapsearch_unsafe(LHandle, Type, In, Attributes) when is_record(LHandle, ldaphandle), is_list(Type),
							   is_list(In), is_list(Attributes) ->
    Handle = LHandle#ldaphandle.ref,
    Filter = eldap:equalityMatch(Type, In),
    SearchBase = case yxa_config:get_env(ldap_searchbase) of
		     {ok, SearchBase1} -> SearchBase1;
		     none -> ""
		 end,
    SearchResult = eldap:search(Handle, [{base, SearchBase},
					 {filter, Filter},
					 {attributes, Attributes}]),
    case SearchResult of
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
%% Function: get_ldapres(In)
%%           In          = list() of eldap_entry record()
%% Descrip.: Turn a list of eldap module query results into our own
%%           result format (ldapres records).
%% Returns : Res = list() of ldapres record()
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
%% Function: ldap_connect(Server)
%%           Server = string()
%% Descrip.: Connect to LDAP server Server.
%% Returns : Handle          |
%%           {error, Reason}
%%           Handle = ldaphandle record()
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
		    #ldaphandle{ref=Handle};
		E ->
		    logger:log(error, "LDAP client: ldap_connect: eldap:simple_bind failed : ~p", [E]),
		    {error, "eldap:simple_bind failed"}
	    end;
	{error, E} ->
	    logger:log(error, "LDAP client: ldap_connect: eldap:open() failed. Server ~p, options ~p : ~p",
		       [Server, Options, E]),
	    {error, "eldap:open failed"};
	Unknown ->
	    logger:log(error, "LDAP client: ldap_connect: eldap:open() Server ~p, options ~p failed - unknown data returned: ~p",
		       [Server, Options, Unknown]),
	    {error, "eldap:open failed"}
    end.

%%--------------------------------------------------------------------
%% Function: ldap_connect(H)
%%           H = ldaphandle record()
%% Descrip.: Close connection to LDAP server.
%% Returns : ok
%%--------------------------------------------------------------------
ldap_close(H) when is_record(H, ldaphandle) ->
    logger:log(debug, "LDAP client: Closing LDAP handle '~p'", [H#ldaphandle.ref]),
    eldap:close(H#ldaphandle.ref),
    ok;
ldap_close(_) ->
    ok.

%%--------------------------------------------------------------------
%% Function: query_ldapclient(Query)
%%           Query = term()
%% Descrip.: Send a query to the persistent directory gen_server, and
%%           return the results. If the persistent process fails in
%%           any way, we kill it so that it gets restarted and
%%           hopefully works better for the next query.
%% Returns : Result |
%%           error
%%           Result = term(), result returned by persistent server
%% Note    : query_ldapclient is executed by the interface functions
%%           in the calling process, not in the persistent ldap_client
%%           gen_server.
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
%% Function: get_value(LDAPres, Attribute)
%%           LDAPres     = ldapres record()
%%           EAttributes = term()
%%           Attribute   = string()
%% Descrip.: Find the Attribute we are looking for in the result list
%%           EAttributes. Returns the value of the first found
%%           matching attribute. Does not indicate in any way if there
%%           were multiple matches.
%% Returns : Value = string() | none
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
%% Function: ldapsearch_simple(Server, Type, In, Attribute)
%%           Server    = string()
%%           Type      = string()
%%           In        = string()
%%           Attribute = string()
%% Descrip.: Query LDAP for a single Attribute and return a single
%%           result.
%% Returns : Value |
%%           none  |
%%           error
%%           Value = string()
%%--------------------------------------------------------------------
ldapsearch_simple(Server, Type, In, Attribute) when is_list(Server), is_list(Type), is_list(In), is_list(Attribute) ->
    logger:log(debug, "Directory: Extra debug: ldapsearch_simple() Server ~p, Type ~p, In ~p, Attribute ~p",
	       [Server, Type, In, Attribute]),
    query_ldapclient({simple_search, Server, Type, In, Attribute}).

%%--------------------------------------------------------------------
%% Function: ldapsearch(Server, Type, In, Attributes)
%%           Server     = string()
%%           Type       = string()
%%           In         = string()
%%           Attributes = list() of string()
%% Descrip.: Query LDAP. Return a list of our result tuples.
%% Returns : Result |
%%           none   |
%%           error
%%           Result = list() of ldapres record()
%%--------------------------------------------------------------------
ldapsearch(Server, Type, In, Attributes) when is_list(Server), is_list(Type), is_list(In), is_list(Attributes) ->
    logger:log(debug, "Directory: Extra debug: ldapsearch() Server ~p, Type ~p, In ~p, Attributes ~p",
	       [Server, Type, In, Attributes]),
    query_ldapclient({search, Server, Type, In, Attributes}).


%% Specific querys

%%--------------------------------------------------------------------
%% Function: lookup_mail2tel(Mail)
%%           Mail = string(), e-mail address
%% Descrip.: Find the telephoneNumber of a user that has Mail as mail-
%%           attribute in LDAP.
%% Returns : Value |
%%           none  |
%%           error
%%           Value = string()
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
%% Function: lookup_mail2uid(Mail)
%%           Mail = string(), e-mail address
%% Descrip.: Find the uid of a user that has Mail as mail-attribute in
%%           LDAP.
%% Returns : Value |
%%           none  |
%%           error
%%           Value = string()
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
%% Function: lookup_mail2cn(Mail)
%%           Mail = string(), e-mail address
%% Descrip.: Find the cn (common name) of a user that has Mail as
%%           mail-attribute in LDAP.
%% Returns : Value |
%%           none  |
%%           error
%%           Value = string()
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
%% Function: lookup_mail2tel(Number)
%%           Number = string(), telephone number
%% Descrip.: Find the displayName of a user that has Number as
%%           telephoneNumber in LDAP.
%% Returns : Value |
%%           none  |
%%           error
%%           Value = string()
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

