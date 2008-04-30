%%%-------------------------------------------------------------------
%%% File    : su_pstnproxy_policy_server.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      pstnproxy SPOCP port program
%%%
%%% @since     4 Apr 2007 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(su_pstnproxy_policy_server).
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
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {port
	       }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, su_pstnproxy_policy_server).

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
    case yxa_config:get_env(local_su_pstnproxy_policy_server_command) of
	{ok, Cmd} when is_list(Cmd) ->
	    gen_server:start_link({local, ?SERVER}, ?MODULE, {Cmd}, []);
	_ ->
	    {error, "No 'local_su_pstnproxy_policy_server_command' configuration parameter present"}
    end.

%%====================================================================
%% Behaviour callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({Cmd}) -> {ok, State}
%%
%%            Cmd = string() "path to port program"
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({Cmd}) when is_list(Cmd) ->
    Port = open_port({spawn, Cmd}, [{line, 5000},
				    exit_status
				   ]),
    {ok, #state{port = Port}}.

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
handle_call({is_allowed, User, ToNumber, Class}, _From, State) when is_list(User), is_list(ToNumber), is_atom(Class) ->
    Request = lists:flatten( io_lib:format("v1:is_allowed:~s:~s:~p~n", [User, ToNumber, Class]) ),
    Port = State#state.port,
    true = send_to_port(Port, Request),
    Reply =
	receive
	    {Port, {data, {eol, Response}}} ->
		case Response of
		    "v1:true" ->
			true;
		    "v1:false" ->
			false;
		    "v1:undefined" ->
			%% fall back to old auth mechanism (sipauth:is_allowed_pstn_dst/4)
			undefined;
		    _ ->
			logger:log(error, "SU pstnpolicy: Received unknown response : ~p (request ~p)",
				   [Response, Request]),
			error
		end;
	    X ->
		logger:log(error, "SU pstnpolicy: Received unknown ~p", [X]),
		error
	    after 4000 ->
		    logger:log(error, "SU pstnpolicy: Did not receive a response (request ~p)", [Request]),
		    error
	    end,
    {reply, Reply, State};

handle_call(Msg, _From, State) ->
    logger:log(error, "SU pstnpolicy: Received unknown gen_server call : ~p", [Msg]),
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
    logger:log(debug, "SU pstnpolicy: Sending message to our port on behalf of ~p : ~p", [Pid, Msg]),
    true = send_to_port(State#state.port, Msg),
    {noreply, State};

handle_cast(Msg, State) ->
    logger:log(error, "SU pstnpolicy: Received unknown gen_server cast : ~p", [Msg]),
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
    logger:log(normal, "SU pstnpolicy: port ~p terminated (exit status ~p), exiting.", [Port, Status]),
    {stop, normal, State};

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    logger:log(error, "SU pstnpolicy: Extra debug: Unexpected data received from port : ~p", [Data]),
    {noreply, State};

handle_info(Msg, State) ->
    logger:log(error, "SU pstnpolicy: Received unknown gen_server info (XXX ~p) : ~p", [State, Msg]),
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

send_to_port(Port, Cmd) when is_port(Port) ->
    logger:log(debug, "SU pstnpolicy: Extra debug: Send to port ~p : ~p", [Port, Cmd]),
    erlang:port_command(Port, Cmd).
