%%%-------------------------------------------------------------------
%%% File    : directory.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Description : gen_server that caches connection to LDAP-server
%%% and do all the querying.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(directory).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start_link/1,
	 ldapsearch_simple/4, ldapsearch/4, get_value/2,
	 lookup_mail2tel/1, lookup_mail2uid/1, lookup_mail2cn/1, lookup_tel2name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {handle, server, querycount}).
-record(ldaphandle, {ref}).

-define(TIMEOUT, 2 * 1000).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    logger:log(debug, "Directory: No LDAP server configured"),
	    start_link(none);
	Server ->
	    start_link(Server)
    end.    

start_link(Server) ->
    gen_server:start_link({local, ldap_client}, ?MODULE, [Server], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Server]) ->
    Handle = case Server of
		 none -> none;
		 H when record(H, ldaphandle) -> H;
		 _ ->
		     case ldap_connect(Server) of
			 H2 when record(H2, ldaphandle) -> H2;
			 {error, E} ->
			     logger:log(error, "Directory: Could not connect to LDAP server ~p " ++
					"at the moment : ~p", [Server, E]),
			     error
		     end
	     end,
    {ok, #state{handle=Handle, server=Server, querycount=0}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({simple_search, Server, Type, In, Attribute}, From, State) ->
    case State#state.server of
	Server ->
	    {NewHandle, Res} = ldapsearch_wrapper(simple, [Type, In, Attribute], State),
	    NewQC = State#state.querycount + 1,
	    NewState = State#state{handle=NewHandle, querycount=NewQC},
	    {reply, {ok, Res}, NewState, ?TIMEOUT};
	_ ->
	    {reply, {error, "Different LDAP server"}, State, ?TIMEOUT}
    end;

handle_call({search, Server, Type, In, Attributes}, From, State) ->
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
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast({ping_of_death, Pid}, State) ->
    logger:log(debug, "LDAP client: Pinged with death by ~p, handle ~p server ~p query count ~p",
	       [Pid, State#state.handle, State#state.server, State#state.querycount]),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%% Check if we should try to re-open our LDAP handle
%% every time we have been idle for 2 seconds
handle_info(timeout, State) ->
    Server = State#state.server,
    case should_reopen_connection(State) of
	true ->
	    case catch ldap_connect(Server) of
		H when record(H, ldaphandle) ->
		    logger:log(debug, "LDAP client: Opened new connection to server ~s", [Server]),
		    {noreply, State#state{handle=H, querycount=0}, ?TIMEOUT};
		{'EXIT', E} ->
		    logger:log(error, "LDAP client: Could not reconnect to LDAP server ~p : ~p", [Server, E]),
		    %% Keep using old handle in hope that it is still working.
		    {noreply, State, ?TIMEOUT};
		Unknown ->
		    logger:log(error, "LDAP client: Could not reconnect to LDAP server ~p, unknown " ++
			       "result from ldap_connect :~n~p", [Server, Unknown]),
		    %% Keep using old handle in hope that it is still working.
		    {noreply, State, ?TIMEOUT}
	    end;
	_ ->
	    {noreply, State, ?TIMEOUT}
    end.


%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    case Reason of
	normal -> true;
	_ -> logger:log(error, "LDAP client terminating : ~p", [Reason])
    end,
    ldap_close(State#state.handle),
    ok.


%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

should_reopen_connection(State) when record(State, state) ->
    QueryCount = State#state.querycount,
    Server = State#state.server,
    case State#state.handle of
	H when record(H, ldaphandle) ->
	    case util:safe_is_process_alive(H#ldaphandle.ref) of
		{false, _} ->
		    true;
		{true, _} ->
		    QueryCount = State#state.querycount,
		    QueryLimit = sipserver:get_env(ldap_connection_query_limit, 500),
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

ldapsearch_wrapper(Mode, Args, State) when record(State, state) ->
    Handle = State#state.handle,
    Server = State#state.server,
    case exec_ldapsearch(Mode, Handle, Args) of
	error ->
	    logger:log(error, "LDAP client: Error returned from exec_ldapsearch (~p), " ++
		       "closing LDAP handle ~p and retrying query", [Mode, Handle]),
	    ldap_close(Handle),
	    case catch ldap_connect(Server) of
		{'EXIT', E} ->
		    logger:log(error, "LDAP client: Could not connect to LDAP server ~p : ~p", [Server, E]),
		    %% This is bad. We have closed our old handle and could not get a new one...
		    {error, error};
		NewHandle when record(NewHandle, ldaphandle) ->
		    logger:log(debug, "LDAP client: Retrying query using new LDAP handle ~p", [NewHandle]),
		    %% New handle returned, retry query and return new handle plus result of retry
		    Res = exec_ldapsearch(Mode, NewHandle, Args),
		    {NewHandle, Res}
	    end;
	Res ->
	    {Handle, Res}
    end.

exec_ldapsearch(Mode, Handle, [Type, In, Attribute]) when record(Handle, ldaphandle) ->
    case catch exec_ldapsearch_unsafe(Handle, Type, In, [Attribute]) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from ldapsearch_unsafe(~p, ~p, ~p, ~p) :~n~p",
		       [Handle, Type, In, Attribute, E]),
	    error;
	none ->
	    none;
	error ->
	    error;
	[{dn, Dn, attributes, EAttributes} | Rest] ->
	    case Mode of
		simple ->
		    lists:sort(get_valuelist([{dn, Dn, attributes, EAttributes} | Rest], Attribute));
		full ->
		    [{dn, Dn, attributes, EAttributes} | Rest]
	    end;
	Unknown ->
	    logger:log(error, "LDAP client: Unknown result from ldapsearch_unsafe (handle ~p, query ~p ~p) :~n~p",
		       [Handle, Type, In, Unknown]),
	    error
    end.

get_valuelist([], Attribute) ->
    [];
get_valuelist([H | T], Attribute) ->
    lists:append([get_value(H, Attribute)], get_valuelist(T, Attribute)).

exec_ldapsearch_unsafe(LHandle, Type, In, Attributes) when record(LHandle, ldaphandle), list(Type), list(In), list(Attributes) ->
    Handle = LHandle#ldaphandle.ref,
    Filter = eldap:equalityMatch(Type, In),
    SearchResult = eldap:search(Handle, [{base,
					  sipserver:get_env(ldap_searchbase, "")},
					 {filter, Filter},
					 {attributes, Attributes}]),
    case SearchResult of
	{ok, {eldap_search_result, List, _}} ->
	    case List of
		[] ->
		    none;
		[{error, Msg}] ->
		    logger:log(normal, "LDAP client: Search using handle ~p failed: ~p", [Handle, Msg]),
		    error;
		[{eldap_entry, Dn, RAttributes} | Rest] ->
		    get_ldapres([{eldap_entry, Dn, RAttributes} | Rest]);
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
exec_ldapsearch_unsafe(H, Type, In, Arguments) when list(Type), list(In), list(Arguments) ->
    logger:log(debug, "LDAP client: exec_ldapsearch_unsafe() called with broken handle ~p - returning error", [H]),
    error;
exec_ldapsearch_unsafe(Handle, Type, In, Arguments) ->
    logger:log(error, "LDAP client: exec_ldapsearch_unsafe() called with illegal arguments :~n" ++
	       "Handle ~p, Type ~p, In ~p, Arguments", [Handle, Type, In, Arguments]),
    error.

get_ldapres(In) ->
    get_ldapres(In, []).

get_ldapres([], Res) ->
    Res;
get_ldapres([{eldap_entry, Dn, RAttributes} | T], Res) ->
    get_ldapres(T, lists:append(Res, [{dn, Dn, attributes, RAttributes}])).

ldap_connect(Server) when list(Server) ->
    UseSSL = sipserver:get_env(ldap_use_ssl, false),
    case eldap:open([Server], [{use_ssl, UseSSL}]) of
	{ok, Handle} ->
	    case eldap:simple_bind(Handle,
				   sipserver:get_env(ldap_username, ""),
				   sipserver:get_env(ldap_password, "")) of
		ok ->
		    logger:log(debug, "LDAP client: Opened LDAP handle ~p (server ~s)", [Handle, Server]),
		    #ldaphandle{ref=Handle};
		E ->
		    logger:log(error, "LDAP client: ldap_connect: eldap:simple_bind failed : ~p", [E]),
		    {error, E}
	    end;
	E ->
	    logger:log(error, "LDAP client: ldap_connect: eldap:open() failed. Server ~p, UseSSL ~p : ~p",
		       [Server, UseSSL, E]),
	    {error, E}
    end.

ldap_close(H) when record(H, ldaphandle) ->
    logger:log(debug, "LDAP client: Closing LDAP handle ~p", [H#ldaphandle.ref]),
    eldap:close(H#ldaphandle.ref);
ldap_close(_) ->
    ok.

%% query_ldapclient is executed by the interface functions in the calling process,
%% not in the persistent ldap_client gen_server.
query_ldapclient(Query) ->
    case catch gen_server:call(ldap_client, Query, 1500) of
	{ok, Res} ->
	    Res;
	{error, E} ->
	    logger:log(error, "Directory: LDAP client returned error : ~p", [E]),
	    error;
	Unknown ->
	    logger:log(error, "Directory: LDAP client returned unknown result : ~p", [Unknown]),
	    gen_server:cast(ldap_client, {ping_of_death, self()}),
	    error
    end.


%%--------------------------------------------------------------------
%%% Interface functions
%%--------------------------------------------------------------------

get_value({dn, Dn, attributes, EAttributes}, Attribute) ->
    get_value(EAttributes, Attribute);
get_value(EAttributes, Attribute) ->
    case lists:keysearch(Attribute, 1, EAttributes) of
	{value, {Attribute, [Value]}} -> Value;
	_ -> none
    end.

ldapsearch_simple(Server, Type, In, Attribute) when list(Server), list(Type), list(In), list(Attribute) ->
    logger:log(debug, "Directory: Extra debug: ldapsearch_simple() Server ~p, Type ~p, In ~p, Attribute ~p",
	       [Server, Type, In, Attribute]),
    query_ldapclient({simple_search, Server, Type, In, Attribute});
ldapsearch_simple(Server, Type, In, Attribute) ->
    logger:log(error, "Directory: ldapsearch_simple() called with illegal argument(s) :~nServer ~p, Type ~p, In ~p, Attribute ~p",
	       [Server, Type, In, Attribute]),
    error.

ldapsearch(Server, Type, In, Attribute) when list(Server), list(Type), list(In), list(Attribute) ->
    logger:log(debug, "Directory: Extra debug: ldapsearch() Server ~p, Type ~p, In ~p, Attribute ~p",
	       [Server, Type, In, Attribute]),
    query_ldapclient({search, Server, Type, In, Attribute});
ldapsearch(Server, Type, In, Attribute) ->
    logger:log(error, "Directory: ldapsearch() called with illegal argument(s) :~nServer ~p, Type ~p, In ~p, Attribute ~p",
	       [Server, Type, In, Attribute]),
    error.


%% Specific querys

lookup_mail2tel(Mail) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    none;
	Server ->
	    Res = ldapsearch_simple(Server, "mail", Mail, "telephoneNumber"),
	    logger:log(debug, "Directory: LDAP telephoneNumber lookup on ~p -> ~p", [Mail, Res]),
	    Res
    end.

lookup_mail2uid(Mail) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    none;
	Server ->
	    Res = ldapsearch_simple(Server, "mail", Mail, "uid"),
	    logger:log(debug, "Directory: LDAP uid lookup on ~p -> ~p", [Mail, Res]),
	    Res
    end.

lookup_mail2cn(Mail) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    none;
	Server ->
	    Res = ldapsearch_simple(Server, "mail", Mail, "cn"),
	    logger:log(debug, "Directory: LDAP server ~s cn lookup on ~p -> ~p", [Server, Mail, Res]),
	    Res
    end.

lookup_tel2name(Number) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    none;
	Server ->
	    Res = ldapsearch_simple(Server, "telephoneNumber", Number, "displayName"),
	    logger:log(debug, "Directory: LDAP server ~s displayName lookup on ~p -> ~p", [Server, Number, Res]),
	    Res
    end.

