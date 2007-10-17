%%%-------------------------------------------------------------------
%%% File    : sipuserdb_file_backend.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      gen_server backend for the sipuserdb file module that
%%%           reads it's user database from a file using
%%%           file:consult().
%%%
%%% @since    28 Dec 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
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
-export([start_link/0,
	 change_interval/1
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
	 code_change/3,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Export for use from sipuserdb_test
%%--------------------------------------------------------------------
-export([parse_db/1]).

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
%% @type state() = #state{}.
%%                 no description
-record(state, {
	  fn,		%% string(), filename
	  file_mtime,	%% integer(), mtime of fn
	  last_fail,	%% integer() | undefined, timestamp when we last warned about failure to interpret fn
	  userlist,	%% list() of user record()
	  addresslist,	%% list() of address record()
	  check_tref	%% undefined | term(), timer reference to check_file timer
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
%% @spec    () -> term()
%%
%% @doc     Starts the persistent sipuserdb_file gen_server.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec    (Interval) -> ok | {error, Reason}
%%
%%            Interval = integer() "number of seconds"
%%
%% @doc     Change the time between checks for a changed userdb file.
%% @end
%%--------------------------------------------------------------------
change_interval(Interval) when is_integer(Interval) ->
    gen_server:call(?SERVER, {change_check_file_timeout, Interval}).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ([]) ->
%%            {ok, State}          |
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init([]) ->
    case yxa_config:get_env(sipuserdb_file_filename) of
	{ok, Fn} when is_list(Fn) ->
	    %% Set up periodic checking for changes in the userdb file, default every 15 seconds
	    TRef =
		case yxa_config:get_env(sipuserdb_file_refresh_interval) of
		    {ok, 0} ->
			%% periodic checking for new data disabled through configuration
			ok;
		    {ok, Interval} when is_integer(Interval) ->
			{ok, TRef1} = timer:send_interval(Interval * 1000, ?SERVER, {check_file}),
			TRef1
		end,

	    case get_mtime(Fn) of
		{ok, MTime} ->
		    NewState1 = #state{fn = Fn,
				       check_tref = case TRef of
							ok -> undefined;
							_ -> TRef
						    end
				      },
		    case read_userdb(NewState1, MTime, init) of
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
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({fetch_users}, From, State) ->
%%            {reply, {ok, Users}, State}
%%
%%            Users = [#user{}]
%%
%% @doc     Fetch our user database.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({fetch_users}, _From, State) ->
    {reply, {ok, State#state.userlist}, State};

%%--------------------------------------------------------------------
%% @spec    ({fetch_addresses}, From, State) ->
%%            {reply, {ok, Addresses}, State}
%%
%%            Addresses = [#address{}]
%%
%% @doc     Fetch our address database.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({fetch_addresses}, _From, State) ->
    {reply, {ok, State#state.addresslist}, State};

%%--------------------------------------------------------------------
%% @spec    ({change_check_file_timeout, Interval}, From, State) ->
%%            {reply, ok, State}
%%
%%            Interval = integer() "seconds between checks"
%%
%% @doc     Set up a new timer for when to check if the userdb file
%%          has changed.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call({change_check_file_timeout, Interval}, _From, State) when is_integer(Interval) ->
    case State#state.check_tref of
	undefined -> ok;
	OldTRef -> {ok, cancel} = timer:cancel(OldTRef)
    end,
    TRef =
	case Interval of
	    0 ->
		logger:log(debug, "sipuserdb_file: Disabled check for changed file"),
		undefined;
	    _ ->
		{ok, TRef1} = timer:send_interval(Interval * 1000, ?SERVER, {check_file}),
		logger:log(debug, "sipuserdb_file: New file check interval : ~p", [Interval]),
		TRef1
	end,
    {reply, ok, State#state{check_tref = TRef}};

handle_call(Unknown, _From, State) ->
    logger:log(error, "sipuserdb_file: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "unknown gen_server call in sipuserdb_file"}, State}.


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

%%--------------------------------------------------------------------
%% @spec    ({reload_userdb}, State) -> {noreply, State}
%%
%% @doc     Signal to reload our user database, even if mtime has not
%%          changed.
%% @hidden
%% @end
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
%% @spec    ({check_file}, State) -> {noreply, State}
%%
%% @doc     Periodically check if our files mtime has changed, and if
%%          so reload it.
%% @hidden
%% @end
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
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%%            Reason = term()
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
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


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (State, MTime, Caller) ->
%%            State           |
%%            {error, Reason}
%%
%%            State  = #state{}
%%            MTime  = integer() "file modification time"
%%            Caller = init | cast | info
%%
%% @doc     (Re-)load the user database.
%% @end
%%--------------------------------------------------------------------
read_userdb(State, MTime, Caller) when is_record(State, state) ->
    logger:log(debug, "sipuserdb_file: (Re-)Loading userdb from file ~p", [State#state.fn]),
    case file:consult(State#state.fn) of
	{ok, TermList} ->
	    case parse_db(TermList) of
		{ok, Users, Addresses} ->
		    logger:log(debug, "sipuserdb_file: Read ~p users with ~p addresses from file ~p",
			       [length(Users), length(Addresses), State#state.fn]),
		    %% on every successfull read, we unset last_fail since it is
		    %% nothing more than a throttle for how often we will log
		    %% failures
		    State#state{userlist=Users, addresslist=Addresses, last_fail=undefined, file_mtime=MTime};
		{error, Reason} ->
		    read_userdb_error(Reason, State, Caller)
	    end;
	{error, {Line, Mod, Term}} ->
	    E = io_lib:format("sipuserdb_file: Parsing of userdb file ~p failed, line ~p : ~p",
			      [State#state.fn, Line, file:format_error({Line, Mod, Term})]),
	    read_userdb_error(E, State, Caller);
	{error, Reason} ->
	    E = io_lib:format("sipuserdb_file: Could not read userdb file ~p : ~p", [State#state.fn, Reason]),
	    read_userdb_error(E, State, Caller)
    end.

%%--------------------------------------------------------------------
%% @spec    (E, State, Caller) ->
%%            {error, Reason} |
%%            NewState
%%
%%            E      = string() "the error reason"
%%            State  = #state{}
%%            Caller = init | cast | info
%%
%%            Reason   = string()
%%            NewState = #state{}
%%
%% @doc     Create error return-value for read_userdb/3. Different
%%          depending on who it is that called read_userdb/3 - for
%%          'init' (startup), we have no good database in memory to
%%          fall back on, so we return {error, E} while for others we
%%          log the error and return the old state which contains the
%%          last good user database.
%% @end
%%--------------------------------------------------------------------
read_userdb_error(E, State, init) when is_list(E), is_record(State, state) ->
    %% On init, always return error tuple
    {error, lists:flatten(E)};
read_userdb_error(E, State, cast) when is_list(E), is_record(State, state) ->
    %% On cast, always log errors
    logger:log(error, E, []),
    State;
read_userdb_error(E, State, info) when is_list(E), is_record(State, state) ->
    %% On info (i.e. interval timer), only log errors every LOG_THROTTLE_SECONDS seconds
    Now = util:timestamp(),
    case State#state.last_fail of
	L when is_integer(L), L >= Now - ?LOG_THROTTLE_SECONDS ->
	    %% We have logged an error recently, skip
	    State;
	L when is_integer(L); L == undefined ->
	    logger:log(error, E, []),
	    State#state{last_fail=Now}
    end.

%%--------------------------------------------------------------------
%% @spec    (Fn) ->
%%            {ok, MTime} |
%%            error
%%
%%            Fn = string() "the filename"
%%
%%            MTime = integer()
%%
%% @doc     Get file modification time of a file.
%% @end
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
%% @spec    ([TermList]) ->
%%            {ok, UserList, AddrList} |
%%            {error, Reason}
%%
%%            TermList = [term()] "result of file:consult()"
%%
%%            UserList = [#user{}]
%%            AddrList = [#addr{}]
%%            Reason   = string()
%%
%% @doc     Parse our userdb file.
%% @end
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
%% @spec    (In, UserList, AddrList) ->
%%            {ok, UserList, AddrList} |
%%            {error, Reason}
%%
%%            In       = [{user, Params} | {address, Params}]
%%            UserList = [#user{}]
%%            AddrList = [#addr{}]
%%
%%            UserList = [#user{}]
%%            AddrList = [#addr{}]
%%            Reason   = string()
%%
%% @doc     Parse terms read from the userdb file.
%% @end
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
%% @spec    (Params, U, Addrs) ->
%%            {ok, User, AddrList} |
%%            {error, Reason}
%%
%%            Params = [{Key, Value}]
%%            U      = #user{}
%%            Addrs  = [string()] "just accumulator"
%%
%%            User     = #user{}
%%            AddrList = [string()]
%%            Reason   = string()
%%
%% @doc     Parse a user entry.
%% @end
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
%% @spec    (Params, A) ->
%%            Address         |
%%            {error, Reason}
%%
%%            Params = [{Key, Value}]
%%            A      = #address{}
%%
%%            Address = #address{}
%%            Reason  = string()
%%
%% @doc     Parse an address entry.
%% @end
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
parse_address([{address, "tel:+" ++ Num} | T], A) when is_record(A, address), is_list(Num) ->
    case util:isnumeric(Num) of
	true ->
	    parse_address(T, A#address{address = "tel:+" ++ Num});
	false ->
	    E = io_lib:format("unparsable tel: URL in address record (user ~p) : ~p",
			      [A#address.user, "tel:+" ++ Num]),
	    {error, lists:flatten(E)}
    end;
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
%% @spec    (Users, Addresses) -> ok
%%
%%            Users    = [#user{}]
%%            Adresses = [#address{}]
%%
%% @doc     Log warnings if there are any users that has no addresses
%%          or any addresses that has no users.
%% @end
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
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->

    %% test parse_address(Params, A)
    %%--------------------------------------------------------------------
    ParseAddress_URL1 = sipurl:parse("sip:ft@example.org"),

    autotest:mark(?LINE, "parse_address/2 - 1"),
    %% valid entry
    #address{user    = "test",
	     address = "sip:ft@example.org",
	     url     = ParseAddress_URL1} =
	parse_address([{user, "test"}, {address, "sip:ft@example.org"}], #address{}),

    autotest:mark(?LINE, "parse_address/2 - 2"),
    %% unparsable URL
    {error, "unparsable URL in address record " ++ _} =
	parse_address([{address, "unsupported:ft@example.org"}], #address{}),

    autotest:mark(?LINE, "parse_address/2 - 3"),
    %% no user
    {error, "address record incomplete (no user, " ++ _} =
	parse_address([{address, "sip:ft@example.org"}], #address{}),

    autotest:mark(?LINE, "parse_address/2 - 4"),
    %% no address
    {error, "address record incomplete (no address, " ++ _} =
	parse_address([{user, "test"}], #address{}),

    autotest:mark(?LINE, "parse_address/2 - 5"),
    %% non-list user
    {error, "bad data in address record " ++ _} = parse_address([{user, none}], #address{}),

    autotest:mark(?LINE, "parse_address/2 - 6"),
    %% non-list address
    {error, "bad data in address record " ++ _} = parse_address([{address, none}], #address{}),

    autotest:mark(?LINE, "parse_address/2 - 7"),
    %% unknown parameter
    {error, "bad data in address record " ++ _} = parse_address([{true, "x"}], #address{}),

    autotest:mark(?LINE, "parse_address/2 - 8"),
    %% tel: URL
    #address{user    = "test",
             address = "tel:+468123456789",
             url     = undefined} =
	parse_address([{user, "test"}, {address, "tel:+468123456789"}], #address{}),


    %% test parse_user(Params, U, Addrs)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "parse_user/3 - 1"),
    %% minimalistic case
    {ok, #user{name="foo"}, []} = parse_user([{name, "foo"}], #user{}, []),

    autotest:mark(?LINE, "parse_user/3 - 2"),
    %% all settings possible
    {ok, #user{name     = "foo",
	       password = "secret",
	       classes  = [none],
	       forward  = undefined
	      }, ["test"]} =
	parse_user([{name, "foo"}, {password, "secret"}, {classes, [none]}, {addresses, ["test"]}], #user{}, []),

    autotest:mark(?LINE, "parse_user/3 - 3"),
    %% test not-implemented forward
    {error, "forward not implemented in sipuserdb_file yet" ++ _} = parse_user([{forward, "foo"}], #user{}, []),

    autotest:mark(?LINE, "parse_user/3 - 4"),
    %% non-list username
    {error, "bad data in user record" ++ _} = parse_user([{name, none}], #user{}, []),

    autotest:mark(?LINE, "parse_user/3 - 5"),
    %% without username
    {error, "user record incomplete (no user, " ++ _} = parse_user([{password, "secret"}], #user{}, []),


    %% parse_term(In, UserList, AddrList)
    %%--------------------------------------------------------------------
    ParseTermUserL1 = [#user{name = "t1"},
		       #user{name = "t2"}],
    ParseTermAddrL1 = [#address{user = "t1", address = "sip:1@example.net", url = sipurl:parse("sip:1@example.net")},
		       #address{user = "t2", address = "sip:2@example.net", url = sipurl:parse("sip:2@example.net")}],

    ParseTermAddrL2 = [#address{user = "t1", address = "sip:1.1@example.net", url = sipurl:parse("sip:1.1@example.net")},
		       #address{user = "t1", address = "sip:1.2@example.net", url = sipurl:parse("sip:1.2@example.net")},
		       #address{user = "t2", address = "sip:2.1@example.net", url = sipurl:parse("sip:2.1@example.net")},
		       #address{user = "t2", address = "sip:2.2@example.net", url = sipurl:parse("sip:2.2@example.net")},
		       #address{user = "t2", address = "tel:+468123456789",   url = undefined}
		      ],

    autotest:mark(?LINE, "parse_term/3 - 1"),
    %% uncomplicated case
    {ok, ParseTermUserL1, ParseTermAddrL1} = parse_term([{user, [{name, "t1"}]},
							 {address, [{user, "t1"},
								    {address, "sip:1@example.net"}]},

							 {user, [{name, "t2"}]},
							 {address, [{user, "t2"},
								    {address, "sip:2@example.net"}]}
							 ], [], []),

    autotest:mark(?LINE, "parse_term/3 - 2"),
    %% addresses grouped with user
    {ok, ParseTermUserL1, ParseTermAddrL2} = parse_term([{user, [{name, "t1"},
								 {addresses, ["sip:1.1@example.net",
									      "sip:1.2@example.net"
									     ]}
								]},
							 {user, [{name, "t2"},
								 {addresses, ["sip:2.1@example.net",
									      "sip:2.2@example.net",
									      "tel:+468123456789"
									     ]}
								]}
							], [], []),

    autotest:mark(?LINE, "parse_term/3 - 3"),
    %% unknown term
    {error, "sipuserdb_file: Unknown data :" ++ _} = parse_term([{none, []}], [], []),

    autotest:mark(?LINE, "parse_term/3 - 4"),
    %% user without name
    {error, "bad data in user record" ++ _} = parse_term([{user, [{password, secret}]}], [], []),

    autotest:mark(?LINE, "parse_term/3 - 5"),
    %% invalid address listed with user
    {error, "unparsable URL in address record " ++ _} =
	parse_term([{user, [{name, "t1"},
			    {addresses, ["unsupported:ft@example.org"]}
			   ]}], [], []),

    autotest:mark(?LINE, "parse_term/3 - 6"),
    %% invalid address
    {error, "unparsable URL in address record " ++ _} =
	parse_term([{address, [{user, "t1"},
			       {address, "unsupported:ft@example.org"}
			      ]}], [], []),


    %% verify_consistency2(Users, Addresses)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "verify_consistency2/2 - 1"),
    %% no warnings
    {ok, VC_U1, VC_A1} = parse_term([{user, [{name, "test1"},
					     {addresses, ["sip:test@example.org"]}
					    ]},
				     {user, [{name, "test2"},
					     {addresses, ["sip:test@example.org"]}
					    ]}
				     ], [], []),
    {ok, [], []} = verify_consistency2(VC_U1, VC_A1),

    autotest:mark(?LINE, "verify_consistency2/2 - 2"),
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

    autotest:mark(?LINE, "verify_consistency2/2 - 3"),
    %% one user warning
    {ok, VC_U3, VC_A3} = parse_term([{user, [{name, "test1"}]},
				     {user, [{name, "test2"},
					     {addresses, ["sip:test@example.org"]}
					    ]}
				     ], [], []),
    {ok, ["test1"], []} = verify_consistency2(VC_U3, VC_A3),


    %% read_userdb_error(E, State, Caller)
    %%--------------------------------------------------------------------
    RUE_Now = util:timestamp(),

    autotest:mark(?LINE, "read_userdb_error/3 - 1"),
    %% Caller = init
    {error, "test"} = read_userdb_error(["te", "st"], #state{}, init),

    autotest:mark(?LINE, "read_userdb_error/3 - 2"),
    %% Caller = cast
    #state{} = read_userdb_error("test", #state{}, cast),

    autotest:mark(?LINE, "read_userdb_error/3 - 3.1"),
    %% Caller = info, first error (last_fail = undefined)
    RUE_State_out3 = read_userdb_error("test", #state{last_fail = undefined}, info),

    autotest:mark(?LINE, "read_userdb_error/3 - 3.2"),
    %% verify new state
    if
	is_integer(RUE_State_out3#state.last_fail), RUE_State_out3#state.last_fail >= RUE_Now ->
	    ok;
	is_integer(RUE_State_out3#state.last_fail), RUE_State_out3#state.last_fail < RUE_Now + 5 ->
	    ok;
	true ->
	    RUE_Msg3 = io_lib:format("test failed, last_failed set to something strange (~p, now is ~p)",
				     [RUE_State_out3#state.last_fail, RUE_Now]),
	    throw(RUE_Msg3)
    end,

    autotest:mark(?LINE, "read_userdb_error/3 - 4"),
    %% Caller = info, error that should not be logged
    RUE_State4 = #state{last_fail = RUE_Now - ?LOG_THROTTLE_SECONDS + 5},

    RUE_State4 = read_userdb_error("test", RUE_State4, info),

    autotest:mark(?LINE, "read_userdb_error/3 - 5.1"),
    %% Caller = info, error that should be logged
    RUE_State5 = #state{last_fail = RUE_Now - ?LOG_THROTTLE_SECONDS - 5},
    RUE_State_out5 = read_userdb_error("test", RUE_State5, info),

    autotest:mark(?LINE, "read_userdb_error/3 - 5.2"),
    %% verify new state
    if
	is_integer(RUE_State_out5#state.last_fail), RUE_State_out5#state.last_fail >= RUE_Now ->
	    ok;
	is_integer(RUE_State_out5#state.last_fail), RUE_State_out5#state.last_fail < RUE_Now + 5 ->
	    ok;
	true ->
	    RUE_Msg5 = io_lib:format("test failed, last_failed set to something strange (~p, now is ~p)",
				     [RUE_State_out5#state.last_fail, RUE_Now]),
	    throw(RUE_Msg5)
    end,

    ok.
