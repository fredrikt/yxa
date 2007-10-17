%%%-------------------------------------------------------------------
%%% File    : registrar.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Process that handles expiration of expired entrys in the
%%%           Mnesia based location database.
%%%
%%% @since    21 Mar 2004 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(registrar).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/0
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
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {}).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term() "result of gen_server:start_link/4"
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, registrar}, ?MODULE, {}, []).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    ({}) ->
%%            {ok, State}          |
%%            {ok, State, Timeout} |
%%            ignore               |
%%            {stop, Reason}
%%
%% @doc     Initiates the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
init({}) ->
    timer:apply_interval(60000, phone, remove_expired_phones, []),
    logger:log(debug, "Registrar started"),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec    handle_call(Msg, From, State) ->
%%            {reply, Reply, State}          |
%%            {reply, Reply, State, Timeout} |
%%            {noreply, State}               |
%%            {noreply, State, Timeout}      |
%%            {stop, Reason, Reply, State}   |
%%            {stop, Reason, State}
%%
%% @doc     Handling call messages.
%% @hidden
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Unknown, From, State) ->
%%            {reply, {error, Reason}, State}
%%
%%            Reason = string()
%%
%% @doc     Unknown call.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_call(Unknown, _From, State) ->
    logger:log(error, "Registrar: Received unknown gen_server call : ~p", [Unknown]),
    {reply, {error, "Unknown gen_server call", State}}.

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
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Unknown cast.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_cast(Unknown, State) ->
    logger:log(error, "Registrar: Received unknown gen_server cast : ~p", [Unknown]),
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
%% @spec    (Unknown, State) -> {noreply, State}
%%
%% @doc     Unknown info.
%% @hidden
%% @end
%%--------------------------------------------------------------------
handle_info(Unknown, State) ->
    logger:log(error, "Registrar: Received unknown gen_server info : ~p", [Unknown]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec    (Reason, State) -> term() "ignored by gen_server"
%%
%% @doc     Shutdown the server
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    case Reason of
	normal -> true;
	_ -> logger:log(error, "Registrar terminating : ~p", [Reason])
    end,
    ok.

%%--------------------------------------------------------------------
%% @spec    (_OldVsn, State, _Extra) -> {ok, NewState}
%%
%% @doc     Convert process state when code is changed
%% @hidden
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
