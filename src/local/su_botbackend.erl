%%%-------------------------------------------------------------------
%%% File    : su_botbackend.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      JEvent bot Erlang backend from Stockholm university.
%%%
%%% @since     1 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(su_botbackend).
%%-compile(export_all).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0
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
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {port
	       }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, su_botbackend).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


%%====================================================================
%% Behaviour callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({}) -> {ok, State}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({}) ->
    case yxa_config:get_env(local_su_bot_jevent_port) of
	{ok, Cmd} when is_list(Cmd) ->
	    Port = open_port({spawn, Cmd}, [{packet, 2}, exit_status]),
	    {ok, #state{port = Port}};
	_ ->
	    {error, "No 'local_su_bot_jevent_port' configuration parameter present"}
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

handle_call(Msg, _From, State) ->
    logger:log(error, "SU bot backend: Received unknown gen_server call : ~p", [Msg]),
    {reply, {error, not_implemented}, State}.


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

handle_cast({send_to_port, Pid, Cmd, Opaque, Data}, State) when is_list(Cmd) ->
    Msg = lists:append([Cmd, " : ", Opaque, " : ", Data]),
    logger:log(debug, "SU bot backend: Sending message to our port on behalf of ~p : ~p", [Pid, Msg]),
    true = send_to_port(State#state.port, Msg),
    {noreply, State};

handle_cast(Msg, State) ->
    logger:log(error, "SU bot backend: Received unknown gen_server cast : ~p", [Msg]),
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

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    logger:log(normal, "SU bot backend: JEvent port ~p terminated (exit status ~p), exiting.", [Port, Status]),
    {stop, normal, State};

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    logger:log(debug, "SU bot backend: Extra debug: Data received from port : ~p", [Data]),
    NewState = port_request(Data, State),
    {noreply, NewState};

handle_info(Msg, State) ->
    logger:log(error, "SU bot backend: Received unknown gen_server info (XXX ~p) : ~p", [State, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

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


port_request("REFER-REQUEST :" ++ Rest, State) ->
    {Opaque1, Rest2} = sipparse_util:split_fields(Rest, $:),
    Opaque = sipparse_util:strip(Opaque1, both, " "),

    {ok, Params} = decode_comma(Rest2),

    logger:log(normal, "SU bot backend: Refer request (id ~p) : ~p",
	       [Opaque, Params]),

    NewState = do_refer(Opaque, Params, State),
    NewState;

port_request("LOCATIONS :" ++ Rest, State) ->
    {Opaque1, Rest2} = sipparse_util:split_fields(Rest, $:),
    Opaque = sipparse_util:strip(Opaque1, both, " "),

    {ok, Params} = decode_comma(Rest2),

    logger:log(normal, "SU bot backend: Locations request (id ~p) : ~p",
	       [Opaque, Params]),

    NewState = do_locations(Opaque, Params, State),
    NewState;

port_request("PING :" ++ Rest, State) ->
    {Opaque, _Rest} = extract_token(Rest, $:),

    true = send_to_port(State#state.port, ["PONG: ", Opaque, " :"]),

    State;

port_request(Data, State) ->
    logger:log(error, "SU bot backend: Received unknown data from port ~p : ~p",
	       [State#state.port, Data]),
    State.

extract_token(In, Sep) ->
    extract_token2(In, Sep, []).

extract_token2([Sep | T], Sep, Res) ->
    Token = sipparse_util:strip(lists:reverse(Res), both, " "),
    {Token, T};
extract_token2([Sep], Sep, Res) ->
    %% last char is separator
    {sipparse_util:strip(lists:reverse(Res), both, " "), ""};
extract_token2([H | T], Sep, Res) ->
    %% non-matching character
    extract_token2(T, Sep, [H | Res]);
extract_token2([], _Sep, _Res) ->
    erlang:error(separator_not_found).

decode_comma("") ->
    [];
decode_comma(In) ->
    try sipheader:comma(In) of
	L when is_list(L) ->
	    make_proplist(L)
    catch
	_: _ ->
	    error
    end.

make_proplist(In) ->
    make_proplist2(In, []).

make_proplist2([H | T], Res) ->
    {Key1, Value1} = sipparse_util:split_fields(H, $=),
    Key = sipparse_util:strip(Key1, both, " "),

    %% strip quotes from Value1
    LeftQuoteIndex = string:chr(Value1, 34),	%% 34 is "
    Value =
	case LeftQuoteIndex of
	    0 ->
		throw({error, "bad value quotation"});
	    _ ->
		TempString = string:substr(Value1, LeftQuoteIndex + 1),
		RightQuoteIndex = string:chr(TempString, 34),	%% 34 is "
		string:substr(TempString, 1, RightQuoteIndex - 1)
	end,

    This = {Key, Value},
    make_proplist2(T, [This | Res]);
make_proplist2([], Res) ->
    {ok, lists:reverse(Res)}.


send_to_port(Port, Cmd) when is_port(Port) ->
    logger:log(debug, "SU bot backend: Extra debug: Send to port ~p : ~p", [Port, Cmd]),
    erlang:port_command(Port, Cmd).

do_refer(Opaque, Params, State) when is_list(Params), is_record(State, state) ->
    true = send_to_port(State#state.port,
			["REFER-REQUEST continued: ",
			 Opaque,
			 " : Trying to decode parameters..."]),

    Referer = get_refer_url("referer", Params),
    Referee = get_refer_url("referee", Params),
    Refer_to = get_refer_url("refer_to", Params),

    Res =
	case {Referer, Referee, Refer_to} of
	    {error, _, _} ->
		"Invalid referer";
	    {_, error, _} ->
		"Invalid referee";
	    {_, _, error} ->
		"Invalid refer_to";
	    _ ->
		true = send_to_port(State#state.port,
				    ["REFER-REQUEST continued: ",
				     Opaque,
				     " : Starting refer..."]),
		su_bot_refer:start_link(Opaque, Referer, Referee, Refer_to),
		ok
    end,

    case Res of
	ok ->
	    ok;
	_ ->
	    true = send_to_port(State#state.port, ["REFER-REQUEST : ", Opaque, " : ", Res])
    end,
    State.

get_refer_url(Key, Params) ->
    case lists:keysearch(Key, 1, Params) of
	{value, {Key, Value}} ->
	    case sipurl:parse(Value) of
		URL when is_record(URL, sipurl) ->
		    URL;
		_ ->
		    logger:log(debug, "SU bot backend: Refer requests has bad ~s URL : ~p", [Key, Value]),
		    error
	    end;
	_ ->
	    error
    end.

do_locations(Opaque, Params, State) when is_list(Params), is_record(State, state) ->
    Res =
	case lists:keysearch("user", 1, Params) of
	    {value, {"user", User}} when is_list(User) ->
		LocL = get_locations_for_user(User),
		[true = send_to_port(State#state.port,
				     ["LOCATIONS continued: ",
				      Opaque,
				      lists:concat([" : ", This])
				     ]
				    ) || This <- LocL],
		io_lib:format("OK, ~p location(s)", [length(LocL)]);
	    _ ->
		"Invalid user"
    end,

    true = send_to_port(State#state.port, ["LOCATIONS : ", Opaque, " : ", Res]),

    State.

get_locations_for_user(User) ->
    Locations = siplocation:sort_most_recent_first( siplocation:get_locations_for_users([User]) ),
    Now = util:timestamp(),
    F = fun(LDBE) when is_record(LDBE, siplocationdb_e) ->
		URL = io_lib:format("url=\"~s\"", [sipurl:print( siplocation:to_url(LDBE) )]),
		UserAgent =
		    case lists:keysearch(user_agent, 1, LDBE#siplocationdb_e.flags) of
			{value, {user_agent, UserAgent1}} ->
			    io_lib:format("user_agent=\"~s\"", [UserAgent1]);
			false ->
			    ""
		    end,
		Expires =
		    case LDBE#siplocationdb_e.expire of
			Ex when is_integer(Ex) ->
			    io_lib:format("expires=\"~p\"", [Ex - Now]);
			never ->
			    "expires=\"never\"";
			_ ->
			    ""
		    end,

		lists:flatten( ["LDBE ", util:join([URL, Expires, UserAgent], " ")] )
	end,

    [F(Loc) || Loc <- Locations].
