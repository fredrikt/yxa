%%%-------------------------------------------------------------------
%%% File    : sipuserdb_file.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Description : A persistent sipuserdb module that reads it's user
%%%               database from a file using file:consult.
%%%
%%% Created : 11 Aug 2004 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(sipuserdb_file).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% standard Yxa userdb module exports
-export([yxa_init/0,
	 get_user_with_address/1,
	 get_users_for_address_of_record/1,
	 get_users_for_addresses_of_record/1,
	 get_users_for_url/1,
	 get_addresses_for_user/1,
	 get_addresses_for_users/1,
	 get_password_for_user/1,
	 get_classes_for_user/1,
	 get_telephonenumber_for_user/1,
	 get_forwards_for_users/1,
	 get_forward_for_user/1
	]).

-include("siprecords.hrl").

%%-compile(export_all).

%% File userdb module

-record(user, {name, password, classes, forward}).
-record(address, {user, address}).

-record(state, {fn, file_mtime, last_fail, userlist, addresslist}).

%% needed for file:read_file_info
-include_lib("kernel/include/file.hrl").

%% how often we will log errors with our periodical reading of the
%% user database file
-define(LOG_THROTTLE_SECONDS, 300).

%%====================================================================
%% External functions
%%====================================================================

%% Function: yxa_init/0
%% Description: Perform any necessary startup initialization and
%%              return an OTP supervisor child spec if we want to add
%%              to sipserver_sup's list. If this sipuserdb_module
%%              needs to be persistent, it should be a gen_server and
%%              init should just return a spec so that the gen_server
%%              is started by the supervisor.
%% Returns: Spec |
%%          []
%%--------------------------------------------------------------------
yxa_init() ->
    [{sipuserdb_file, {sipuserdb_file, start_link, []},
                 permanent, 2000, worker, [sipuserdb_file]}
    ].

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, sipuserdb_file}, ?MODULE, [], []).

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
init([]) ->
    case sipserver:get_env(sipuserdb_file_filename, none) of
	none ->
	    E = "sipuserdb_file module activated, but sipuserdb_file_filename not set - module disabled",
	    logger:log(error, E),
	    ignore;
	Fn when list(Fn) ->
	    case sipserver:get_env(sipuserdb_file_refresh_interval, 60) of
		X when X == 0; X == none ->
		    ok;
		Interval when integer(Interval) ->
		    {ok, T} = timer:send_interval(Interval * 1000, sipuserdb_file, {check_file})
	    end,
	    case get_mtime(Fn) of
		{ok, MTime} ->
		    case read_userdb(#state{fn=Fn}, MTime, init) of
			NewState when record(NewState, state) ->
			    {ok, NewState};
			{error, Reason} ->
			    logger:log(error, Reason, []),
			    {stop, Reason}
		    end;
		Unknown ->
		    logger:log(error, "sipuserdb_file: could not get mtime for file ~p : ~p", [Fn, Unknown]),
		    {ok, #state{fn=Fn}}
	    end
    end.

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
handle_call({fetch_users}, From, State) ->
    {reply, {ok, State#state.userlist}, State};

handle_call({fetch_addresses}, From, State) ->
    {reply, {ok, State#state.addresslist}, State};

handle_call(Unknown, From, State) ->
    logger:log(error, "sipuserdb_file: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "unknown gen_server call in sipuserdb_file"}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({reload_userdb}, State) ->
    logger:log(debug, "sipuserdb_file: Forcing reload of userdb upon request"),
    case get_mtime(State#state.fn) of
	{ok, MTime} ->
	    case read_userdb(State, MTime, cast) of
		NewState when record(NewState, state) ->
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
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({check_file}, State) ->
    case get_mtime(State#state.fn) of
	{ok, MTime} when MTime /= State#state.file_mtime ->
	    case read_userdb(State, MTime, info) of
		NewState when record(NewState, state) ->
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
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Interface functions
%%--------------------------------------------------------------------


%% Function: get_user_with_address/1
%% Description: Looks up exactly one user with an Address. Used for
%%              example in REGISTER. If there are multiple users with
%%              an address, this function returns 'error'.
%% Returns: Username |
%%          nomatch  |
%%          error
%%--------------------------------------------------------------------
get_user_with_address(Address) ->
    case get_users_using_address(Address) of
	[] ->
	    logger:log(debug, "userdb-file: No user with address ~p", [Address]),
	    nomatch;
	[User] ->
	    %% There exists exactly one user with this name
	    User;
	Users when list(Users) ->
	    logger:log(debug, "userdb-file: More than one user with address ~p (~p)", [Address, Users]),
	    error;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_users_using_address(), address ~p result : ~p",
		       [Address, Unknown]),
	    error
    end.


%% Function: get_users_for_address_of_record/1
%% Description: Get all usernames of users matching an address. Used
%%              to find out to which users we should send a request.
%% Returns: ListOfUsers |
%%          error
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    case get_users_using_address(Address) of
	L when list(L) ->
	    L;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_users_using_address(), address ~p result : ~p",
		       [Address, Unknown]),
	    error
    end.


%% Function: get_users_for_addresses_of_record/1
%% Description: Iterate over a list of addresses of record, return
%%              all users matching one or more of the addresses,
%%              without duplicates.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
get_users_for_addresses_of_record(In) ->
    get_users_for_addresses_of_record2(In, []).

get_users_for_addresses_of_record2([], Res) ->
    lists:usort(Res);
get_users_for_addresses_of_record2([H | T], Res) ->
    case get_users_for_address_of_record(H) of
	Users when list(Users) ->
	    get_users_for_addresses_of_record2(T, lists:append(Res, Users));
	_ ->
	    get_users_for_addresses_of_record2(T, Res)
    end.


%% Function: get_addresses_for_users/1
%% Description: Iterate over a list of users, return all their
%%              addresses without duplicates by using the next
%%              function, get_addresses_for_user/1.
%% Returns: ListOfAddresses
%%--------------------------------------------------------------------
get_addresses_for_users(In) ->
    get_addresses_for_users2(In, []).

get_addresses_for_users2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_addresses_for_users2([H | T], Res) ->
    case get_addresses_for_user(H) of
	A when list(A) ->
	    get_addresses_for_users2(T, lists:append(Res, A));
	_ ->
	    get_addresses_for_users2(T, Res)
    end.


%% Function: get_addresses_for_user/1
%% Description: Get all possible addresses of a user. Both configured
%%              ones, and implcit ones. Used for example to check if a
%%              request from a user has an acceptable From: header.
%% Returns: ListOfAddresses |
%%          nomatch         |
%%          error
%%--------------------------------------------------------------------
get_addresses_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p", [Username]),
	    [];
	User when record(User, user) ->
	    collect_addresses(get_addresses_using_user(User));
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%% Function: get_users_for_url/1
%% Description: Given an URL that is typically the Request-URI of an
%%              incoming request, make a list of implicit user
%%              addresses and return a list of all users matching any
%%              of these addresses. This is located in here since
%%              user database backends can have their own way of
%%              deriving addresses from a Request-URI.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
get_users_for_url(URL) when record(URL, sipurl) ->
    Addresses = local:lookup_url_to_addresses(sipuserdb_file, URL),
    logger:log(debug, "userdb-file: Looking for users matching address(es) ~p derived from URL ~p",
	       [Addresses, sipurl:print(URL)]),
    get_users_for_addresses_of_record(Addresses).


%% Attribute fetching functions :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Function: get_password_for_user/1
%% Description: Returns the password for a user.
%% Returns: PasswordString |
%%          nomatch        |
%%          error
%%--------------------------------------------------------------------
get_password_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p when fetching password", [Username]),
	    nomatch;
	User when record(User, user) ->
	    User#user.password;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%% Function: get_classes_for_users/1
%% Description: Returns a list of classes allowed for a user. Classes
%%              are used by pstnproxy to determine if it should allow
%%              a call to a PSTN number (of a certain class) from a
%%              user or not.
%% Returns: ClassesList |
%%          nomatch
%%--------------------------------------------------------------------
get_classes_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p when fetching password", [Username]),
	    nomatch;
	User when record(User, user) ->
	    case User#user.classes of
		_ when User#user.classes == none ; User#user.classes == undefined ->
		    [];
		_ ->
		    User#user.classes
	    end;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%% Function: get_telephonenumber_for_user/1
%% Description: Return the telephone number for a user. We do this by
%%              fetching all addresses for the user and then examining
%%              them to see if any of them is a tel: URL, or has a
%%              user part which is all numeric or is an E.164 number.
%% Returns: NumberString |
%%          nomatch      |
%%          error
%%--------------------------------------------------------------------
get_telephonenumber_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No such user ~p when fetching telephone number", [Username]),
	    nomatch;
	User when record(User, user) ->
	    case get_addresses_using_user(User) of
		nomatch ->
		    nomatch;
		error ->
		    error;
		A when list(A) ->
		    %% Look through the addresses returned and return the first one
		    %% which has an all numeric user-part.
		    case find_first_telephonenumber(A) of
			nomatch ->
			    %% XXX Should we perhaps turn that into 'error' since we
			    %% apparenlty had a user with this name in our database but
			    %% that user did not have an address that looks like a phone
			    %% number?
			    logger:log(debug, "userdb-file: User ~p has no address that looks like a phone number (addresses : ~p)",
				       [Username, collect_addresses(A)]),
			    nomatch;
			Address when record(Address, address) ->
			    URL = sipurl:parse(Address#address.address),
			    URL#sipurl.user
		    end;
		Unknown ->
		    logger:log(error, "userdb-file: Unexpected result from get_addresses_for_user(), user ~p result : ~p",
			       [Username, Unknown]),
		    error
	    end;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from get_user(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.


%% Function: get_forwards_for_users/1
%% Description: Return a list of all forwards for a list of users.
%%              Uses the next function, get_forward_for_user/1.
%% Returns: ForwardList
%%--------------------------------------------------------------------
get_forwards_for_users(In) ->
    get_forwards_for_users2(In, []).

get_forwards_for_users2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_forwards_for_users2([H | T], Res) ->
    case get_forward_for_user(H) of
	nomatch ->
	    get_forwards_for_users2(T, Res);
	Fwd ->
	    get_forwards_for_users2(T, lists:append(Res, [Fwd]))
    end.


%% Function: get_forward_for_user/1
%% Description: Return the forward for a user.
%% Returns: Forward |
%%          nomatch |
%%          error
%%--------------------------------------------------------------------
get_forward_for_user(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(debug, "userdb-file: No forwards found for user ~p", [Username]),
	    nomatch;
	User when record(User, user) ->
	    case User#user.forward of
		Foo when Foo == none; Foo == []; Foo == undefined ->
		    nomatch;
		L when list(L) ->
		    L;
		U ->
		    logger:log(error, "userdb-file: User ~p has invalid forward in database : ~p",
			       [Username, User#user.forward]),
		    error
	    end;
	Unknown ->
	    logger:log(error, "userdb-file: Unexpected result from database_forward:fetch(), user ~p result : ~p",
		       [Username, Unknown]),
	    error
    end.



%%--------------------------------------------------------------------
%%% Client side Internal functions
%%--------------------------------------------------------------------


%% Function: get_user/1
%% Description: Fetch a user record from the user database,
%%              given it's username.
%% Returns: User    |
%%          nomatch
%%--------------------------------------------------------------------
get_user(User) ->
    get_user2(User, fetch_users()).

get_user2(User, []) ->
    nomatch;
get_user2(User, [H | T]) when record(H, user), H#user.name == User ->
    H;
get_user2(User, [H | T]) when record(H, user) ->
    get_user2(User, T);
get_user2(User, [H | T]) ->
    logger:log(error, "userdb-file: Malformed user record : ~p", [H]),
    get_user2(User, T).


%% Function: get_addresses_using_user/1
%% Description: Return all addresses for a username or a user record.
%% Returns: ListOfAddresses
%%--------------------------------------------------------------------
get_addresses_using_user(Username) when list(Username) ->
    case get_user(Username) of
	nomatch ->
	    logger:log(error, "userdb-file: No user named ~p, can't look up addresses", [Username]),
	    [];
	User when record(User, user) ->
	    get_addresses_using_user2(Username, fetch_addresses(), [])
    end;
get_addresses_using_user(User) when record(User, user) ->
    get_addresses_using_user2(User#user.name, fetch_addresses(), []).

get_addresses_using_user2(Username, [], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_addresses_using_user2(Username, [H | T], Res) when record(H, address), H#address.user == Username ->
    %% Username matches the username for the address record (H)
    get_addresses_using_user2(Username, T, lists:append(Res, [H]));
get_addresses_using_user2(Username, [H | T], Res) when record(H, address) ->
    get_addresses_using_user2(Username, T, Res).


%% Function: get_users_using_address/1
%% Description: Given an address (list) or URL (sipurl record), locate
%%              and return all address records in the userdb, fetched
%%              from the persistent sipuserdb_file process,
%%              that matches using URI address matching rules.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
get_users_using_address(Address) when list(Address) ->
    %% Check that Address is a parseable URL. It really does not have to
    %% be - it is often the result of canonization of a Request-URI, URL
    %% or address of some form.
    case sipurl:parse(Address) of
	URL when record(URL, sipurl) ->
	    case get_user_records_for_url(URL, fetch_addresses(), []) of
		error ->
		    [];
		R when list(R) ->
		    collect_usernames(R)
	    end;
	_ ->
	    %% unparseable URL
	    []
    end;
get_users_using_address(URL) when record(URL, sipurl) ->
    R = get_user_records_for_url(URL, fetch_addresses(), []),
    collect_usernames(R).


%% Function: get_user_records_for_url/4
%% Description: Given an URL (sipurl record), locate and return all
%%              address records in the userdb, fetched from the
%%	        persistent sipuserdb_file process, that matches using
%%              URI address matching rules, or the user records
%%              matching the address.
%% Returns: ListOfUserRecords |
%%          error
%%--------------------------------------------------------------------
get_user_records_for_url(URL, [], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
get_user_records_for_url(URL, [H | T], Res) when record(URL, sipurl), record(H, address) ->
    case sipurl:url_is_equal(URL, sipurl:parse(H#address.address)) of
	true ->
	    %% Check that there is actually a user for this address record too
	    case get_user(H#address.user) of
		nomatch ->
		    logger:log(error, "userdb-file: Found address record matching URL ~p " ++
			       "but it does not have a matching user record : ~p",
			       [sipurl:print(URL), H]),
		    get_user_records_for_url(URL, T, Res);
		U when record(U, user) ->
		    %% Address record matching URL found, and we also have a user
		    %% matching the address records user variable.
		    get_user_records_for_url(URL, T, lists:append(Res, [U]))
	    end;
	_ ->
	    %% URL does NOT match this address record, try next (T)
	    get_user_records_for_url(URL, T, Res)
    end;
get_user_records_for_url(URL, [H | T], Res) when record(URL, sipurl) ->
    logger:log(error, "userdb-file: Malformed address record : ~p", [H]),
    get_user_records_for_url(URL, T, Res);
get_user_records_for_url(URL, [H | T], Res) ->
    logger:log(error, "userdb-file: Malformed URL in get_user_records_for_url : ~p", [URL]),
    error.


%% Function: collect_usernames/1
%% Description: Collect and return a list of all usernames from a
%%              set of user and/or address records.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
collect_usernames(In) ->
    collect_usernames2(In, []).

collect_usernames2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
collect_usernames2([H | T], Res) when record(H, user) ->
    collect_usernames2(T, lists:append(Res, [H#user.name]));
collect_usernames2([H | T], Res) when record(H, address) ->
    collect_usernames2(T, lists:append(Res, [H#address.user]));
collect_usernames2([H | T], Res) ->
    logger:log(error, "userdb-file: Input to collect_usernames2 is neither address nor user record : ~p",
	       [H]),
    collect_usernames2(T, Res).


%% Function: collect_addresses/1
%% Description: Collect and return a list of all addresses from a
%%              set of address records.
%% Returns: ListOfUsernames
%%--------------------------------------------------------------------
collect_addresses(In) ->
    collect_addresses2(In, []).

collect_addresses2([], Res) ->
    %% Make list sorted and remove duplicates
    lists:usort(Res);
collect_addresses2([H | T], Res) when record(H, address) ->
    collect_addresses2(T, lists:append(Res, [H#address.address]));
collect_addresses2([H | T], Res) ->
    logger:log(error, "userdb-file: Input to collect_addresses2 is not address record : ~p",
	       [H]),
    collect_addresses2(T, Res).


%% Function: find_first_telephonenumber/1
%% Description: Look through a list of address records and return the
%%              first one that has a URL userpart that is all numeric.
%%              Called from get_telephonenumber_for_user.
%% Returns: Address |
%%          nomatch
%%--------------------------------------------------------------------
find_first_telephonenumber([]) ->
    nomatch;
find_first_telephonenumber([H | T]) when record(H, address) ->
    URL = sipurl:parse(H#address.address),
    IsNumericUser = util:isnumeric(URL#sipurl.user),
    IsTelURL = case URL#sipurl.proto of
		   "tel" -> true;
		   _ -> false
	       end,
    IsE164User = case util:isnumeric(URL#sipurl.user) of
		     "+" ++ Rest ->
			 util:isnumeric(Rest);
		     _ ->
			 false
		 end,
    if
	IsNumericUser == true -> H;
	IsTelURL == true -> H;
	IsE164User == true -> H;
	true ->
	    %% The address records (H) address does not appear to
	    %% be a phone number. Check next element (T).
	    find_first_telephonenumber(T)
    end;
find_first_telephonenumber([H | T]) ->
    logger:log(error, "userdb-file: Input to find_first_telephonenumber is not address record : ~p",
	       [H]),
    find_first_telephonenumber(T).

fetch_users() ->
    {ok, Res} = gen_server:call(sipuserdb_file, {fetch_users}),
    Res.
    
fetch_addresses() ->
    {ok, Res} = gen_server:call(sipuserdb_file, {fetch_addresses}),
    Res.

%%--------------------------------------------------------------------
%%% Server side Internal functions
%%--------------------------------------------------------------------
read_userdb(State, MTime, Caller) when record(State, state) ->
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

read_userdb_error(E, State, init) when record(State, state) ->
    %% On init, always return error tuple
    {error, E};
read_userdb_error(E, State, cast) when record(State, state) ->
    %% On cast, always log errors
    logger:log(error, E, []),
    State;
read_userdb_error(E, State, info) when record(State, state) ->
    %% On info (i.e. interval timer), only log errors every LOG_THROTTLE_SECONDS seconds
    Now = util:timestamp(),
    case State#state.last_fail of
	L when integer(L), L >= Now - ?LOG_THROTTLE_SECONDS ->
	    %% We have logged an error recently, skip
	    State;
	L when integer(L) ->
	    logger:log(error, E, []),
	    State#state{last_fail=Now}
    end.
	        
get_mtime(Fn) ->
    case file:read_file_info(Fn) of
	{ok, FileInfo} when record(FileInfo, file_info) ->
	    %% file_info record returned
	    {ok, FileInfo#file_info.mtime};
	Unknown ->
	    error
    end.

parse_db([TermList]) ->
    parse_term(TermList, [], []).

parse_term([], U, A) ->
    {ok, lists:reverse(U), lists:reverse(A)};
parse_term([{user, Params} | T], U, A) ->
    case parse_user(Params, #user{}) of
	R when record(R, user) ->
	    parse_term(T, [R | U], A);
	E ->
	    E
    end;
parse_term([{address, Params} | T], U, A) ->
    case parse_address(Params, #address{}) of
	R when record(R, address) ->
	    parse_term(T, U, [R | A]);
	E ->
	    E
    end;
parse_term([H | T], U, A) ->
    {error, io_lib:format("sipuserdb_file: Unknown data : ~p", [H])}.

parse_user([], U) ->
    U;
parse_user([{name, V} | T], U) when record(U, user) ->
    parse_user(T, U#user{name=V});
parse_user([{password, V} | T], U) when record(U, user) ->
    parse_user(T, U#user{password=V});
parse_user([{classes, V} | T], U) when record(U, user) ->
    parse_user(T, U#user{classes=V});
parse_user([{forward, V} | T], U) when record(U, user) ->
    parse_user(T, U#user{forward=V});
parse_user([H | T], U) when record(U, user) ->
    E = io_lib:format("bad data in user record (name ~p) : ~p", [U#user.name, H]),
    {error, E}.

parse_address([], A) ->
    A;
parse_address([{user, V} | T], A) when record(A, address) ->
    parse_address(T, A#address{user=V});
parse_address([{address, V} | T], A) when record(A, address) ->
    parse_address(T, A#address{address=V});
parse_address([H | T], A) when record(A, address) ->
    E = io_lib:format("bad data in address record (user ~p) : ~p", [A#address.user, H]),
    {error, E}.
