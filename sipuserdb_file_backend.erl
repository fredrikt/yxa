%%%-------------------------------------------------------------------
%%% File    : sipuserdb_file_backend.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: gen_server backend for the sipuserdb file module that
%%%           reads it's user database from a file using
%%%           file:consult().
%%%
%%% Created : 28 Dec 2004 by Fredrik Thulin <ft@it.su.se>
%%%
%%% Notes   : If there is unparsable data in the userdb when we start,
%%%           we crash rather ungracefully. Start with empty userdb
%%%           or crash? If the latter then at least crash with style.
%%%           We don't validate data (like addresses) when we load it
%%%           - maybe we should.
%%%-------------------------------------------------------------------
-module(sipuserdb_file_backend).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0]).

%%--------------------------------------------------------------------
%% Internal exports - gen_server callbacks
%%--------------------------------------------------------------------
-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipuserdb_file.hrl").
%% needed for file:read_file_info
-include_lib("kernel/include/file.hrl").


%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(state, {
	  fn,		%% string(), filename
	  file_mtime,	%% integer(), mtime of fn
	  last_fail,	%% integer(), timestamp when we last warned about failure to interpret fn
	  userlist,	%% list() of user record()
	  addresslist	%% list() of address record()
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
%% how often we will log errors with our periodical reading of the
%% user database file
-define(LOG_THROTTLE_SECONDS, 300).
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link()
%% Descrip.: Starts the persistent sipuserdb_file gen_server.
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([])
%% Descrip.: Initiates the server
%% Returns : {ok, State}          |
%%           {ok, State, Timeout} |
%%           ignore               |
%%           {stop, Reason}
%%--------------------------------------------------------------------
init([]) ->
    case yxa_config:get_env(sipuserdb_file_filename) of
	{ok, Fn} when is_list(Fn) ->
	    %% Set up periodic checking for changes in the userdb file, default every 15 seconds
	    case yxa_config:get_env(sipuserdb_file_refresh_interval) of
		{ok, 0} ->
		    %% periodic checking for new data disabled through configuration
		    ok;
		{ok, Interval} when is_integer(Interval) ->
		    {ok, _T} = timer:send_interval(Interval * 1000, ?SERVER, {check_file})
	    end,

	    case get_mtime(Fn) of
		{ok, MTime} ->
		    case read_userdb(#state{fn=Fn}, MTime, init) of
			NewState when is_record(NewState, state) ->
			    {ok, NewState};
			{error, Reason} ->
			    logger:log(error, Reason, []),
			    {stop, Reason}
		    end;
		Unknown ->
		    logger:log(error, "sipuserdb_file: could not get mtime for file ~p : ~p", [Fn, Unknown]),
		    {stop, "sipuserdb_file: get_mtime of userdb failed"}
	    end;
	none ->
	    E = "sipuserdb_file module activated, but sipuserdb_file_filename not set - module disabled",
	    logger:log(error, E),
	    ignore
    end.


%%--------------------------------------------------------------------
%% Function: handle_call(Msg, From, State)
%% Descrip.: Handling call messages
%% Returns : {reply, Reply, State}          |
%%           {reply, Reply, State, Timeout} |
%%           {noreply, State}               |
%%           {noreply, State, Timeout}      |
%%           {stop, Reason, Reply, State}   | (terminate/2 is called)
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_call({fetch_users}, From, State)
%% Descrip.: Fetch our user database.
%% Returns : {reply, {ok, Users}, State}
%%           Users = list() of user record()
%%--------------------------------------------------------------------
handle_call({fetch_users}, _From, State) ->
    {reply, {ok, State#state.userlist}, State};

%%--------------------------------------------------------------------
%% Function: handle_call({fetch_addresses}, From, State)
%% Descrip.: Fetch our address database.
%% Returns : {reply, {ok, Addresses}, State}
%%           Addresses = list() of address record()
%%--------------------------------------------------------------------
handle_call({fetch_addresses}, _From, State) ->
    {reply, {ok, State#state.addresslist}, State};

handle_call(Unknown, _From, State) ->
    logger:log(error, "sipuserdb_file: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "unknown gen_server call in sipuserdb_file"}, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State)
%% Descrip.: Handling cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_cast({reload_userdb}, State)
%% Descrip.: Signal to reload our user database, even if mtime has
%%           not changed.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_cast({reload_userdb}, State) ->
    logger:log(debug, "sipuserdb_file: Forcing reload of userdb upon request"),
    case get_mtime(State#state.fn) of
	{ok, MTime} ->
	    case read_userdb(State, MTime, cast) of
		NewState when is_record(NewState, state) ->
		    {noreply, NewState};
		{error, _} ->
		    {noreply, State}
	    end;
	Unknown ->
	    logger:log(error, "sipuserdb_file: Could not get mtime of file ~p : ~p",
		       [State#state.fn, Unknown]),
	    {noreply, State}
    end;

handle_cast(Unknown, State) ->
    logger:log(error, "sipuserdb_file: Received unknown gen_server cast : ~p", [Unknown]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Msg, State)
%% Descrip.: Handling all non call/cast messages
%% Returns : {noreply, State}          |
%%           {noreply, State, Timeout} |
%%           {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: handle_info({check_file}, State)
%% Descrip.: Periodically check if our files mtime has changed, and if
%%           so reload it.
%% Returns : {noreply, State}
%%--------------------------------------------------------------------
handle_info({check_file}, State) ->
    case get_mtime(State#state.fn) of
	{ok, MTime} when MTime /= State#state.file_mtime ->
	    case read_userdb(State, MTime, info) of
		NewState when is_record(NewState, state) ->
		    {noreply, NewState};
		{error, _} ->
		    {noreply, State}
	    end;
	_ ->
	    %% Either error returned from read_file_info, or file mtime has not changed.
	    %% We don't check (care) which one...
	    {noreply, State}
    end;

handle_info(Info, State) ->
    logger:log(error, "sipuserdb_file: Received unknown signal : ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State)
%%           Reason = term()
%% Descrip.: Shutdown the server
%% Returns : any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    Reason.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra)
%% Descrip.: Convert process state when code is changed
%% Returns : {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: read_userdb(State, MTime, Caller)
%%           State  = state record()
%%           MTime  = integer(), file modification time
%%           Caller = atom(), who is calling us (init | cast | info)
%% Descrip.: (Re-)load the user database.
%% Returns : State           |
%%           {error, Reason}
%%--------------------------------------------------------------------
read_userdb(State, MTime, Caller) when is_record(State, state) ->
    logger:log(debug, "sipuserdb_file: (Re-)Loading userdb from file ~p", [State#state.fn]),
    case file:consult(State#state.fn) of
	{ok, TermList} ->
	    case parse_db(TermList) of
		{ok, Users, Addresses} ->
		    logger:log(debug, "sipuserdb_file: Read ~p users with ~p addresses from file ~p",
			       [length(Users), length(Addresses), State#state.fn]),
		    %%logger:log(debug, "sipuserdb_file: Extra debug:~nUsers : ~p~nAddresses : ~p", [Users, Addresses]),
		    %% on every successfull read, we unset last_fail since it is
		    %% nothing more than a throttle for how often we will log
		    %% failures
		    State#state{userlist=Users, addresslist=Addresses, last_fail=undefined, file_mtime=MTime};
		{error, Reason} ->
		    read_userdb_error(Reason, State, Caller)
	    end;
	{error, {Line, Mod, Term}} ->
	    E = io_lib:format("sipuserdb_file: Parsing of userdb file ~p failed : ~p",
			      [State#state.fn, file:format_error({Line, Mod, Term})]),
	    read_userdb_error(E, State, Caller);
	{error, Reason} ->
	    E = io_lib:format("sipuserdb_file: Could not read userdb file ~p : ~p", [State#state.fn, Reason]),
	    read_userdb_error(E, State, Caller)
    end.

%%--------------------------------------------------------------------
%% Function: read_userdb_error(E, State, Caller)
%%           E      = string(), the error reason
%%           State  = state record()
%%           Caller = atom(), who we are formatting the error for
%%                    (init | cast | info)
%% Descrip.: Create error return-value for read_userdb/3. Different
%%           depending on who it is that called read_userdb/3 - for
%%           'init' (startup), we have no good database in memory to
%%           fall back on, so we return {error, E} while for others
%%           we log the error and return the old state which contains
%%           the last good user database.
%% Returns : {error, Reason} |
%%           NewState
%%           Reason = string()
%%           NewState = state record()
%%--------------------------------------------------------------------
read_userdb_error(E, State, init) when is_record(State, state) ->
    %% On init, always return error tuple
    {error, lists:flatten(E)};
read_userdb_error(E, State, cast) when is_record(State, state) ->
    %% On cast, always log errors
    logger:log(error, E, []),
    State;
read_userdb_error(E, State, info) when is_record(State, state) ->
    %% On info (i.e. interval timer), only log errors every LOG_THROTTLE_SECONDS seconds
    Now = util:timestamp(),
    case State#state.last_fail of
	L when is_integer(L), L >= Now - ?LOG_THROTTLE_SECONDS ->
	    %% We have logged an error recently, skip
	    State;
	L when is_integer(L) ->
	    logger:log(error, E, []),
	    State#state{last_fail=Now}
    end.

%%--------------------------------------------------------------------
%% Function: get_mtime(Fn)
%%           Fn = string(), the filename
%% Descrip.: Get file modification time of a file.
%% Returns : {ok, MTime} |
%%           error
%%           MTime = integer()
%%--------------------------------------------------------------------
get_mtime(Fn) ->
    case file:read_file_info(Fn) of
	{ok, FileInfo} when is_record(FileInfo, file_info) ->
	    %% file_info record returned
	    {ok, FileInfo#file_info.mtime};
	_Unknown ->
	    error
    end.

%%--------------------------------------------------------------------
%% Function: parse_db([TermList])
%%           TermList = list() of term(), result of file:consult()
%% Descrip.: Parse our userdb file.
%% Returns : {ok, UserList, AddrList} |
%%           {error, Reason}
%%           UserList = list() of user record()
%%           AddrList = list() of addr record()
%%           Reason   = string()
%%--------------------------------------------------------------------
parse_db([TermList]) ->
    case parse_term(TermList, [], []) of
	{ok, U, A} ->
	    verify_consistency(U, A),
	    {ok, U, A};
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: parse_term(In, UserList, AddrList)
%%           In       = list() of {user, Params} | {address, Params}
%%                      record()
%%           UserList = list() of user record()
%%           AddrList = list() of addr record()
%% Descrip.: Parse terms read from the userdb file.
%% Returns : {ok, UserList, AddrList} |
%%           {error, Reason}
%%           UserList = list() of user record()
%%           AddrList = list() of addr record()
%%           Reason   = string()
%%--------------------------------------------------------------------
parse_term([], UserList, AddrList) ->
    {ok, lists:reverse(UserList), lists:reverse(AddrList)};
parse_term([{user, Params} | T], UserList, AddrList) ->
    case parse_user(Params, #user{}, []) of
	{ok, R, []} when is_record(R, user) ->
	    parse_term(T, [R | UserList], AddrList);
	{ok, R, Addrs} when is_record(R, user), is_list(Addrs) ->
	    case parse_addresses(R#user.name, Addrs, AddrList) of
		L when is_list(L) ->
		    %% Parse addresses listed together with user
		    parse_term(T, [R | UserList], lists:reverse(L));
		E ->
		    E
	    end;
	E ->
	    E
    end;
parse_term([{address, Params} | T], UserList, AddrList) ->
    case parse_address(Params, #address{}) of
	R when is_record(R, address) ->
	    parse_term(T, UserList, [R | AddrList]);
	E ->
	    E
    end;
parse_term([H | _T], _UserList, _AddrList) ->
    E = io_lib:format("sipuserdb_file: Unknown data : ~p", [H]),
    {error, lists:flatten(E)}.


%% part of parse_term/3, parse addresses listed together with user
parse_addresses(Username, [H | T], Res) ->
    case parse_address([{address, H}], #address{user=Username}) of
	A when is_record(A, address) ->
	    parse_addresses(Username, T, [A | Res]);
	E ->
	    E
    end;
parse_addresses(_Username, [], Res) ->
    lists:reverse(Res).

%%--------------------------------------------------------------------
%% Function: parse_user(Params, U, Addrs)
%%           Params = list() of {Key, Value} tuple()
%%           U      = user record()
%%           Addrs  = list() of string() - just accumulator
%% Descrip.: Parse a user entry.
%% Returns : {ok, User, AddrList} |
%%           {error, Reason}
%%           User     = user record()
%%           AddrList = list() of string()
%%           Reason   = string()
%%--------------------------------------------------------------------
parse_user([], U, Addrs) when is_record(U, user), U#user.name == undefined ->
    E = io_lib:format("user record incomplete (no user, raw user ~p, addresses ~p)", [U, Addrs]),
    {error, lists:flatten(E)};
parse_user([], U, Addrs) when is_record(U, user) ->
    {ok, U, Addrs};

parse_user([{name, V} | T], U, Addrs) when is_record(U, user), is_list(V) ->
    parse_user(T, U#user{name=V}, Addrs);
parse_user([{password, V} | T], U, Addrs) when is_record(U, user), is_list(V) ->
    parse_user(T, U#user{password=V}, Addrs);
parse_user([{classes, V} | T], U, Addrs) when is_record(U, user), is_list(V) ->
    parse_user(T, U#user{classes=V}, Addrs);
parse_user([{forward, V} | _T], U, _Addrs) when is_record(U, user), is_list(V) ->
    E = io_lib:format("forward not implemented in sipuserdb_file yet (name ~p) : ~p",
		      [U#user.name, {forward, V}]),
    {error, lists:flatten(E)};
parse_user([{addresses, V} | T], U, Addrs) when is_record(U, user), is_list(V) ->
    parse_user(T, U, Addrs ++ V);
parse_user([H | _T], U, _Addrs) when is_record(U, user) ->
    E = io_lib:format("bad data in user record (name ~p) : ~p", [U#user.name, H]),
    {error, lists:flatten(E)}.


%%--------------------------------------------------------------------
%% Function: parse_address(Params, A)
%%           Params = list() of {Key, Value} tuple()
%%           A      = address record()
%% Descrip.: Parse an address entry.
%% Returns : Address         |
%%           {error, Reason}
%%           Address = address record()
%%           Reason  = string()
%%--------------------------------------------------------------------
parse_address([], A) when is_record(A, address), A#address.user == undefined ->
    E = io_lib:format("address record incomplete (no user, address ~p)", [A#address.address]),
    {error, E};
parse_address([], A) when is_record(A, address), A#address.address == undefined ->
    E = io_lib:format("address record incomplete (no address, user ~p)", [A#address.user]),
    {error, lists:flatten(E)};
parse_address([], A) when is_record(A, address) ->
    A;

parse_address([{user, V} | T], A) when is_record(A, address), is_list(V) ->
    parse_address(T, A#address{user=V});
parse_address([{address, V} | T], A) when is_record(A, address), is_list(V) ->
    case sipurl:parse(V) of
	URL when is_record(URL, sipurl) ->
	    parse_address(T, A#address{address=V, url=URL});
	Unknown ->
	    E = io_lib:format("unparsable URL in address record (user ~p) : ~p (~p)", [A#address.user, V, Unknown]),
	    {error, lists:flatten(E)}
    end;
parse_address([H | _T], A) when is_record(A, address) ->
    E = io_lib:format("bad data in address record (user ~p) : ~p", [A#address.user, H]),
    {error, lists:flatten(E)}.


%%--------------------------------------------------------------------
%% Function: verify_consistency(Users, Addresses)
%%           Users    = list() of user record()
%%           Adresses = list() of address record()
%% Descrip.: Log warnings if there are any users that has no addresses
%%           or any addresses that has no users.
%% Returns : ok
%%--------------------------------------------------------------------
verify_consistency(Users, Addresses) ->
    {ok, NoAddressUsers, NoUserAddresses} = verify_consistency2(Users, Addresses),
    case NoAddressUsers of
	[] -> ok;
	_ ->
	    logger:log(normal, "sipuserdb_file: Warning: The following user(s) has no address(es) : ~p",
		       [NoAddressUsers])
    end,
    case NoUserAddresses of
	[] -> ok;
	_ ->
	    logger:log(normal, "sipuserdb_file: Warning: The following address(es) has no user(s) : ~p",
		       [NoUserAddresses])
    end,
    ok.

%% verify_consistency2/2 - part of verify_consistency/2
%% Returns : {ok, NoAddressUsers, NoUserAddresses}
verify_consistency2(Users, Addresses) ->	     
    NoAddressUsers = get_no_address_users(Users, Addresses, []),
    NoUserAddresses = get_no_user_addresses(Addresses, Users, []),
    {ok, NoAddressUsers, NoUserAddresses}.

%% get_no_address_users/3 - part of verify_consistency2/2
%% Returns : NoAddressUsers = list() of string() - users who have no addresses
get_no_address_users([H | T], Addresses, Res) when is_record(H, user) ->
    case get_no_address_users2(H#user.name, Addresses) of
	true ->
	    %% there was at least one address for this user
	    get_no_address_users(T, Addresses, Res);
	false ->
	    %% there was no addresses for this user
	    get_no_address_users(T, Addresses, [H#user.name | Res])
    end;
get_no_address_users([], _Addresses, Res) ->
    lists:reverse(Res).

%% get_no_address_users2/2 - part of get_no_address_users/3
get_no_address_users2(Username, [#address{user=Username} | _T]) ->
    true;
get_no_address_users2(Username, [_H | T]) ->
    get_no_address_users2(Username, T);
get_no_address_users2(_Username, []) ->
    false.

%% get_no_user_addresses/3 - part of verify_consistency2/2
%% Returns : NoUserAddresses = list() of string() - addresses who have no user
get_no_user_addresses([H | T], Users, Res) when is_record(H, address) ->
    case get_no_user_addresses2(H#address.user, Users) of
	true ->
	    %% there was at least one user for this address
	    get_no_user_addresses(T, Users, Res);
	false ->
	    %% there was no user for this addresses
	    get_no_user_addresses(T, Users, [H#address.address | Res])
    end;
get_no_user_addresses([], _Users, Res) ->
    lists:reverse(Res).

%% get_no_user_addresses2/2 - part of get_no_user_addresses/3
get_no_user_addresses2(Username, [#user{name=Username} | _T]) ->
    true;
get_no_user_addresses2(Username, [_H | T]) ->
    get_no_user_addresses2(Username, T);
get_no_user_addresses2(_Username, []) ->
    false.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->

    %% test parse_address/1
    %%--------------------------------------------------------------------
    ParseAddress_URL1 = sipurl:parse("sip:ft@example.org"),

    io:format("test: parse_address/2 - 1~n"),
    %% valid entry
    #address{user    = "test",
	     address = "sip:ft@example.org",
	     url     = ParseAddress_URL1} =
	parse_address([{user, "test"}, {address, "sip:ft@example.org"}], #address{}),

    io:format("test: parse_address/2 - 2~n"),
    %% unparsable URL
    {error, "unparsable URL in address record " ++ _} =
	parse_address([{address, "unsupported:ft@example.org"}], #address{}),

    io:format("test: parse_address/2 - 3~n"),
    %% no user
    {error, "address record incomplete (no user, " ++ _} =
	parse_address([{address, "sip:ft@example.org"}], #address{}),

    io:format("test: parse_address/2 - 4~n"),
    %% no address
    {error, "address record incomplete (no address, " ++ _} =
	parse_address([{user, "test"}], #address{}),

    io:format("test: parse_address/2 - 5~n"),
    %% non-list user
    {error, "bad data in address record " ++ _} = parse_address([{user, none}], #address{}),

    io:format("test: parse_address/2 - 6~n"),
    %% non-list address
    {error, "bad data in address record " ++ _} = parse_address([{address, none}], #address{}),

    io:format("test: parse_address/2 - 7~n"),
    %% unknown parameter
    {error, "bad data in address record " ++ _} = parse_address([{true, "x"}], #address{}),


    %% test parse_user/3
    %%--------------------------------------------------------------------
    io:format("test: parse_user/3 - 1~n"),
    %% minimalistic case
    {ok, #user{name="foo"}, []} = parse_user([{name, "foo"}], #user{}, []),

    io:format("test: parse_user/3 - 2~n"),
    %% all settings possible
    {ok, #user{name     = "foo",
	       password = "secret",
	       classes  = [none],
	       forward  = undefined
	      }, ["test"]} =
	parse_user([{name, "foo"}, {password, "secret"}, {classes, [none]}, {addresses, ["test"]}], #user{}, []),

    io:format("test: parse_user/3 - 3~n"),
    %% test not-implemented forward
    {error, "forward not implemented in sipuserdb_file yet" ++ _} = parse_user([{forward, "foo"}], #user{}, []),

    io:format("test: parse_user/3 - 4~n"),
    %% non-list username
    {error, "bad data in user record" ++ _} = parse_user([{name, none}], #user{}, []),

    io:format("test: parse_user/3 - 5~n"),
    %% without username
    {error, "user record incomplete (no user, " ++ _} = parse_user([{password, "secret"}], #user{}, []),


    %% test parse_term/3
    %%--------------------------------------------------------------------
    ParseTermUserL1 = [#user{name = "t1"},
		       #user{name = "t2"}],
    ParseTermAddrL1 = [#address{user = "t1", address = "sip:1@example.net", url = sipurl:parse("sip:1@example.net")},
		       #address{user = "t2", address = "sip:2@example.net", url = sipurl:parse("sip:2@example.net")}],

    ParseTermAddrL2 = [#address{user = "t1", address = "sip:1.1@example.net", url = sipurl:parse("sip:1.1@example.net")},
		       #address{user = "t1", address = "sip:1.2@example.net", url = sipurl:parse("sip:1.2@example.net")},
		       #address{user = "t2", address = "sip:2.1@example.net", url = sipurl:parse("sip:2.1@example.net")},
		       #address{user = "t2", address = "sip:2.2@example.net", url = sipurl:parse("sip:2.2@example.net")}],

    io:format("test: parse_term/3 - 1~n"),
    %% uncomplicated case
    {ok, ParseTermUserL1, ParseTermAddrL1} = parse_term([{user, [{name, "t1"}]},
							 {address, [{user, "t1"},
								    {address, "sip:1@example.net"}]},
							 
							 {user, [{name, "t2"}]},
							 {address, [{user, "t2"},
								    {address, "sip:2@example.net"}]}
							 ], [], []),

    io:format("test: parse_term/3 - 2~n"),
    %% addresses grouped with user
    {ok, ParseTermUserL1, ParseTermAddrL2} = parse_term([{user, [{name, "t1"}, 
								 {addresses, ["sip:1.1@example.net",
									      "sip:1.2@example.net"]}]},
							 {user, [{name, "t2"},
								 {addresses, ["sip:2.1@example.net",
									      "sip:2.2@example.net"]}]}
							], [], []),

    io:format("test: parse_term/3 - 3~n"),
    %% unknown term
    {error, "sipuserdb_file: Unknown data :" ++ _} = parse_term([{none, []}], [], []),

    io:format("test: parse_term/3 - 4~n"),
    %% user without name
    {error, "bad data in user record" ++ _} = parse_term([{user, [{password, secret}]}], [], []),

    io:format("test: parse_term/3 - 5~n"),
    %% invalid address listed with user
    {error, "unparsable URL in address record " ++ _} =
	parse_term([{user, [{name, "t1"},
			    {addresses, ["unsupported:ft@example.org"]}
			   ]}], [], []),

    io:format("test: parse_term/3 - 6~n"),
    %% invalid address
    {error, "unparsable URL in address record " ++ _} =
	parse_term([{address, [{user, "t1"},
			       {address, "unsupported:ft@example.org"}
			      ]}], [], []),


    %% test verify_consistency2(Users, Addresses)
    %%--------------------------------------------------------------------
    io:format("test: verify_consistency2/2 - 1~n"),
    %% no warnings
    {ok, VC_U1, VC_A1} = parse_term([{user, [{name, "test1"},
					     {addresses, ["sip:test@example.org"]}
					    ]},
				     {user, [{name, "test2"},
					     {addresses, ["sip:test@example.org"]}
					    ]}
				     ], [], []),
    {ok, [], []} = verify_consistency2(VC_U1, VC_A1),

    io:format("test: verify_consistency2/2 - 2~n"),
    %% one address warning
    {ok, VC_U2, VC_A2} = parse_term([{user, [{name, "test1"},
					     {addresses, ["sip:test@example.org"]}
					    ]},
				     {user, [{name, "test2"},
					     {addresses, ["sip:test@example.org"]}
					    ]},
				     {address, [{user, "x"},
						{address, "sip:x@example.org"}]}
				     ], [], []),
    {ok, [], ["sip:x@example.org"]} = verify_consistency2(VC_U2, VC_A2),

    io:format("test: verify_consistency2/2 - 3~n"),
    %% one user warning
    {ok, VC_U3, VC_A3} = parse_term([{user, [{name, "test1"}]},
				     {user, [{name, "test2"},
					     {addresses, ["sip:test@example.org"]}
					    ]}
				     ], [], []),
    {ok, ["test1"], []} = verify_consistency2(VC_U3, VC_A3),


    
    ok.
