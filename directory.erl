-module(directory).
-export([start/0, recvloop/3, ldapsearch_simple/4, ldapsearch/4,
	 real_ldapsearch_simple/4, real_ldapsearch/4,
	 get_value/2,
	 lookup_mail2tel/1, lookup_mail2uid/1, lookup_mail2cn/1, lookup_tel2name/1]).

start() ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    logger:log(debug, "Directory: Not starting LDAP client, no LDAP server configured"),
	    none;
	Server ->
	    start(Server)
    end.

start(Server) ->
    Handle = case Server of
	{handle, H} -> H;
	_ ->
	    case ldap_connect(Server) of
		{handle, H} -> H;
		{error, E} ->
		    logger:log(error, "Directory: Could not connect to LDAP server ~p at the moment : ~p", [Server, E]),
		    error
	    end
    end,
    case Handle of
	P when pid(P) ->
	    Pid = spawn(directory, recvloop, [Server, {handle, Handle}, 0]),
	    logger:log(debug, "Directory: Started ldap_client process ~p", [Pid]),
	    register(ldap_client, Pid),
	    ok;
	_ ->
	    error
    end.

recvloop(Server, Handle, QueryCount) ->
    {NewHandle, NewQueryCount} = receive
	{simple_search, Pid, Opaque, {QueryServer, Type, In, Attribute}} ->
	    {NewHandle1, Res} = ldapsearch_wrapper(Server, real_ldapsearch_simple, Handle, [Type, In, Attribute]),
	    util:safe_signal("LDAP client: ", Pid, {ldap_client, Opaque, Res}),
	    {NewHandle1, QueryCount + 1} ;
	{search, Pid, Opaque, {QueryServer, Type, In, Attributes}} ->
	    {NewHandle1, Res} = ldapsearch_wrapper(Server, real_ldapsearch, Handle, [Type, In, Attributes]),
	    util:safe_signal("LDAP client: ", Pid, {ldap_client, Opaque, Res}),
	    {NewHandle1, QueryCount + 1};
	{ping} ->
	    logger:log(debug, "LDAP client: Pong, handle ~p server ~p query count ~p",
	    		[Handle, Server, QueryCount]),
	    {Handle, QueryCount};
	Unknown ->
	    logger:log(debug, "Directory: ldap_client received unknown signal ~p", [Unknown]),
	    {Handle, QueryCount}
    after
	2 * 1000 ->
	    % Check if we should try to re-open our LDAP handle
	    % every time we have been idle for 5 seconds
	    case should_reopen_connection(Handle, QueryCount, Server) of
		true ->
		    case catch ldap_connect(Server) of
			{'EXIT', E} ->
			    logger:log(error, "LDAP client: Could not connect to LDAP server ~p : ~p", [Server, E]),
			    % Return old handle in hope that it is still working.
			    {Handle, QueryCount};
			{handle, H} ->
			    logger:log(debug, "LDAP client: Opened new connection to server ~s", [Server]),
			    {{handle, H}, 0}
		    end;
		_ ->
		    {Handle, QueryCount}
	    end
    end,
    recvloop(Server, NewHandle, NewQueryCount).

should_reopen_connection({handle, H}, QueryCount, Server) when pid(H) ->
    QueryLimit = sipserver:get_env(ldap_connection_query_limit, 500),
    if
	QueryCount >= QueryLimit ->
	    logger:log(debug, "LDAP client: This connections query limit (~p) is exceeded (~p). Will try to open new connection to server ~p.",
			[QueryLimit, QueryCount, Server]),
	    true;
	true -> false
    end;
should_reopen_connection(H, QC, Server) ->
    logger:log(debug, "LDAP client: Trying to open new connection to server ~p since current handle is broken (handle ~p, query count ~p)",
		[Server, H, QC]),
    true.

ldapsearch_wrapper(Server, Func, Handle, Args) ->
    case apply(?MODULE, Func, [Handle | Args]) of
	error ->
	    logger:log(error, "LDAP client: Error returned from ~p, closing LDAP handle ~p and retrying query",
			[Func, Handle]),
	    ldap_close(Handle),
	    case catch ldap_connect(Server) of
		{'EXIT', E} ->
		    logger:log(error, "LDAP client: Could not connect to LDAP server ~p : ~p", [Server, E]),
		    {{handle, error}, error};
		{handle, H} ->
		    NewHandle = {handle, H},
		    logger:log(debug, "LDAP client: Retrying query using new LDAP handle ~p", [NewHandle]),
		    % New handle returned, retry query and return new handle plus result of retry
		    Res = apply(?MODULE, Func, [NewHandle | Args]),
		    {NewHandle, Res}
	    end;
	Res ->
	    {Handle, Res}
    end.

real_ldapsearch_simple(Handle, Type, In, Attribute) ->
    case catch ldapsearch_unsafe(Handle, Type, In, [Attribute]) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from ldapsearch_unsafe(~p, ~p, ~p, ~p) :~n~p",
	    		[Handle, Type, In, Attribute, E]),
	    error;
	[{dn, Dn, attributes, EAttributes} | Rest] ->
	    lists:sort(get_valuelist([{dn, Dn, attributes, EAttributes} | Rest], Attribute));
	none ->
	    none;
	error ->
	    error;
	Unknown ->
	    logger:log(error, "LDAP client: Unknown result from ldapsearch_unsafe (handle ~p, query ~p ~p) :~n~p",
	    		[Handle, Type, In, Unknown]),
	    error
    end.

real_ldapsearch(Handle, Type, In, Attributes) when list(Attributes) ->
    case catch ldapsearch_unsafe(Handle, Type, In, Attributes) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from ldapsearch_unsafe(~p, ~p, ~p, ~p) :~n~p",
	    		[Handle, Type, In, Attributes, E]),
	    error;
	[{dn, Dn, attributes, EAttributes} | Rest] ->
	    [{dn, Dn, attributes, EAttributes} | Rest];
	none ->
	    none;
	error ->
	    error;
	Unknown ->
	    logger:log(error, "LDAP client: Unknown result from ldapsearch_unsafe (handle ~p, query ~p ~p) :~n~p",
	    		[Handle, Type, In, Unknown]),
	    error
    end.

get_valuelist([], Attribute) ->
    [];
get_valuelist([H | T], Attribute) ->
    lists:append([get_value(H, Attribute)], get_valuelist(T, Attribute)).
    
get_value({dn, Dn, attributes, EAttributes}, Attribute) ->
    get_value(EAttributes, Attribute);
get_value(EAttributes, Attribute) ->
    case lists:keysearch(Attribute, 1, EAttributes) of
	{value, {Attribute, [Value]}} -> Value;
	_ -> none
    end.

ldapsearch_unsafe({handle, Handle}, Type, In, Attributes) when pid(Handle) ->
    Filter = eldap:equalityMatch(Type, In),
    SearchResult = eldap:search(Handle, [{base,
					  sipserver:get_env(ldap_searchbase, "")},
					 {filter, Filter},
					 {attributes, Attributes}]),
    case SearchResult of
	{ok, Result} ->
	    {eldap_search_result, List, _} = Result,
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
ldapsearch_unsafe({handle, H}, Type, In, Arguments) ->
    logger:log(debug, "LDAP client: ldapsearch_unsafe() called with broken handle ~p - returning error", [{handle, H}]),
    error;
ldapsearch_unsafe(Handle, Type, In, Arguments) ->
    logger:log(debug, "LDAP client: ldapsearch_unsafe() called with illegal arguments :~nHandle ~p, Type ~p, In ~p, Arguments",
		[Handle, Type, In, Arguments]),
    error.

get_ldapres([]) ->
    [];
get_ldapres([{eldap_entry, Dn, RAttributes} | Rest]) ->
    lists:append([{dn, Dn, attributes, RAttributes}], get_ldapres(Rest)).

ldap_connect(Server) ->
    OpenRes = eldap:open([Server], [{use_ssl, sipserver:get_env(ldap_use_ssl, false)}]),
    case OpenRes of
	{ok, Handle} ->
	    case eldap:simple_bind(Handle,
			   sipserver:get_env(ldap_username, ""),
			   sipserver:get_env(ldap_password, "")) of
		ok ->
		    logger:log(debug, "LDAP client: Opened LDAP handle ~p (server ~s)", [Handle, Server]),
		    {handle, Handle};
		E ->
		    logger:log(error, "LDAP client: ldap_connect: eldap:simple_bind failed : ~p", [E]),
		    {error, E}
	    end;
	E ->
	    logger:log(error, "LDAP client: ldap_connect: eldap:open failed : ~p", [E]),
	    {error, E}
    end.
	
ldap_close({handle, Handle}) ->
    logger:log(debug, "LDAP client: Closing LDAP handle ~p", [Handle]),
    eldap:close(Handle).



% Search functions :

ldapsearch_simple(Server, Type, In, Attribute) when list(Server), list(Type), list(In), list(Attribute) ->
    logger:log(debug, "Directory: Extra debug: ldapsearch_simple() Server ~p, Type ~p, In ~p, Attribute ~p",
		[Server, Type, In, Attribute]),
    send_to_ldap_client(simple_search, {Server, Type, In, Attribute});
ldapsearch_simple(Server, Type, In, Attribute) ->
    logger:log(error, "Directory: ldapsearch_simple called with illegal argument(s) :~nServer ~p, Type ~p, In ~p, Attribute ~p",
		[Server, Type, In, Attribute]),
    error.

ldapsearch(Server, Type, In, Attributes) when list(Server), list(Type), list(In), list(Attributes) ->
    logger:log(debug, "Directory: Extra debug: ldapsearch() Server ~p, Type ~p, In ~p, Attribute ~p",
		[Server, Type, In, Attributes]),
    send_to_ldap_client(search, {Server, Type, In, Attributes});
ldapsearch(Server, Type, In, Attributes) ->
    logger:log(error, "Directory: ldapsearch called with illegal argument(s) :~nServer ~p, Type ~p, In ~p, Attributes ~p",
		[Server, Type, In, Attributes]),
    error.
    
send_to_ldap_client(Function, Arguments) ->
    case erlang:whereis(ldap_client) of
	undefined ->
	    logger:log(error, "Directory: Spawned LDAP client process is not around, trying to start a new one"),
	    Res = start(),
	    logger:log(debug, "Directory: Extra debug: Starting ldap_client -> ~p", [Res]),
	    case Res of
		none ->
		    none;
		ok ->
		    send_to_ldap_client1(Function, Arguments);
		_ ->
		    error		    
	    end;
	Pid when pid(Pid) ->
	    send_to_ldap_client1(Function, Arguments)
    end.
    
send_to_ldap_client1(Function, Arguments) ->
    util:safe_signal("LDAP client: ", ldap_client, {Function, self(), opaque, Arguments}),
    receive     
	{ldap_client, opaque, Reply} ->
	    Reply;
	Unknown ->
	    logger:log(error, "Directory: Received unknown signal ~p when waiting for answer from ldap_client, aborting",
			[Unknown]),
	    error
    after
	1500 ->
	    case util:safe_is_process_alive(ldap_client) of
		{true, Pid} ->
		    logger:log(error, "Directory: Communication with ldap_client timed out, killing ldap_client ~p and answering 'error' to query : ~p ~p",
				[Pid, Function, Arguments]),
		    Pid ! {ping},
		    exit(Pid, "You were not fast enough"),
		    error;
		_ ->
		    logger:log(error, "Directory: Communication with ldap_client timed out and now there are no ldap_client. Answering 'error' to query : ~p ~p",
				[Function, Arguments]),
		    error
	    end
    end.


% Specific querys

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

