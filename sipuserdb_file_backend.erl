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
	 code_change/3
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
	  fn,
	  file_mtime,
	  last_fail,
	  userlist,
	  addresslist
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
    case sipserver:get_env(sipuserdb_file_filename, none) of
	none ->
	    E = "sipuserdb_file module activated, but sipuserdb_file_filename not set - module disabled",
	    logger:log(error, E),
	    ignore;
	Fn when is_list(Fn) ->
	    %% Set up periodic checking for changes in the userdb file, default every 15 seconds
	    case sipserver:get_env(sipuserdb_file_refresh_interval, 15) of
		X when X == 0; X == none ->
		    %% periodic checking for new data disabled through configuration
		    ok;
		Interval when is_integer(Interval) ->
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
	    end
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

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
    parse_term(TermList, [], []).

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
    case parse_user(Params, #user{}) of
	R when is_record(R, user) ->
	    parse_term(T, [R | UserList], AddrList);
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
    {error, io_lib:format("sipuserdb_file: Unknown data : ~p", [H])}.

%%--------------------------------------------------------------------
%% Function: parse_user(Params, U)
%%           Params = list() of {Key, Value} tuple()
%%           U      = user record()
%% Descrip.: Parse a user entry.
%% Returns : User            |
%%           {error, Reason}
%%           User   = user record()
%%           Reason = string()
%%--------------------------------------------------------------------
parse_user([], U) when is_record(U, user) ->
    U;
parse_user([{name, V} | T], U) when is_record(U, user) ->
    parse_user(T, U#user{name=V});
parse_user([{password, V} | T], U) when is_record(U, user) ->
    parse_user(T, U#user{password=V});
parse_user([{classes, V} | T], U) when is_record(U, user) ->
    parse_user(T, U#user{classes=V});
parse_user([{forward, V} | T], U) when is_record(U, user) ->
    parse_user(T, U#user{forward=V});
parse_user([H | _T], U) when is_record(U, user) ->
    E = io_lib:format("bad data in user record (name ~p) : ~p", [U#user.name, H]),
    {error, E}.

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
parse_address([], A) when is_record(A, address)->
    A;
parse_address([{user, V} | T], A) when is_record(A, address) ->
    parse_address(T, A#address{user=V});
parse_address([{address, V} | T], A) when is_record(A, address) ->
    parse_address(T, A#address{address=V});
parse_address([H | _T], A) when is_record(A, address) ->
    E = io_lib:format("bad data in address record (user ~p) : ~p", [A#address.user, H]),
    {error, E}.
