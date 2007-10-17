%%%-------------------------------------------------------------------
%%% File    : sipserver.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Main OTP application startup function, and per-request
%%%           start processing function.
%%%
%%% @since    12 Dec 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sipserver).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start/2,
	 stop/0,
	 restart/0,
	 process/2,

	 %% XXX should move these to some utility module
	 safe_spawn/3,
	 origin2str/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (normal, [AppModule]) ->
%%            {ok, Pid}
%%
%%            AppModule = atom() "name of this YXA application"
%%
%%            Pid = pid() "the YXA OTP supervisor (module sipserver_sup)"
%%
%% @doc     The big start-function for the YXA stack. Invoke this
%%          function to make the sun go up, and tell it the name of
%%          your YXA application (AppModule) to have the stack invoke
%%          the correct init/0, request/3 and response/3 methods.
%% @end
%%--------------------------------------------------------------------
start(normal, [AppModule]) ->
    %% First of all, we add a custom error_logger module. This is the only
    %% way to really get all information about why supervised subsystems fails
    %% to start.
    sup_error_logger:start(),
    catch ssl:start(),
    %% We seed SSL better after starting our configuration subsystem
    ssl:seed([
	      io_lib:format("~p", [now()])
	     ]),
    mnesia:start(),
    ok = init_statistics(),
    case sipserver_sup:start_link(AppModule, []) of
	{ok, Supervisor} ->
	    local:init(),
	    #yxa_app_init{sup_spec	= AppSupdata,
			  mnesia_tables	= MnesiaTables
			 } = AppModule:init(),
	    logger:log(debug, "starting, supervisor is ~p", [Supervisor]),
	    case siphost:myip() of
		"127.0.0.1" ->
		    logger:log(normal, "NOTICE: siphost:myip() returns 127.0.0.1, it is either "
			       "broken on your platform or you have no interfaces (except loopback) up");
		_ ->
		    true
	    end,
	    ok = init_mnesia(MnesiaTables),
	    ok = gen_server:call(yxa_monitor, {add_mnesia_tables, MnesiaTables}),
	    {ok, Supervisor} = sipserver_sup:start_extras(Supervisor, AppModule, AppSupdata),
	    %% now that everything is started, seed ssl with more stuff
	    ssl:seed([
		      io_lib:format("~p ~p",
				    [now(),
				     yxa_config:list()
				    ])
		     ]),
	    {ok, Supervisor} = sipserver_sup:start_transportlayer(Supervisor),
	    logger:log(normal, "proxy started (YXA version ~s)", [version:get_version()]),
	    {ok, Supervisor};
	Unknown ->
	    E = lists:flatten(io_lib:format("Failed starting supervisor : ~p", [Unknown])),
	    {error, E}
    end.

%%--------------------------------------------------------------------
%% @spec    () -> term() "does not return"
%%
%% @doc     Log and then shut down application. Shuts down the whole
%%          Erlang virtual machine, so never really returns.
%% @end
%%--------------------------------------------------------------------
stop() ->
    logger:log(normal, "Sipserver: shutting down"),
    {ok, AppModule} = yxa_config:get_env(yxa_appmodule),
    case (catch AppModule:terminate(shutdown)) of
	ok ->
	    ok;
	Res ->
	    logger:log(debug, "Sipserver: ~p:terminate/1 terminated with reason other than 'ok' : ~p", [AppModule, Res])
    end,
    init:stop().

%%--------------------------------------------------------------------
%% @spec    () -> term() "does not return"
%%
%% @doc     Log and then restart application. Will never really
%%          return.
%% @end
%%--------------------------------------------------------------------
restart() ->
    logger:log(normal, "Sipserver: restarting"),
    init:restart().

%%--------------------------------------------------------------------
%% @spec    (Tables) ->
%%            ok 
%%
%%            Tables = [atom()] "names of Mnesia tables needed by this YXA application."
%%
%%            DescriptiveAtom = atom() "reason converted to atom since atoms are displayed best when an application fails to start"
%%
%% @throws  DescriptiveAtom 
%%
%% @doc     Initiate Mnesia on this node. If there are no remote
%%          mnesia-tables, we conclude that we are a mnesia master
%%          and check if any of the tables needs to be updated.
%% @end
%%--------------------------------------------------------------------
init_mnesia(none) ->
    init_mnesia_update();

init_mnesia(Tables) when is_list(Tables) ->
    case get_remote_tables(Tables) of
	{ok, []} ->
	    %% no remote tables, we are master
	    find_mnesia_tables("local", Tables),
	    init_mnesia_update();
	{ok, RemoteTables} ->
	    case yxa_config:get_env(databaseservers) of
		{ok, DbNodes} ->
		    logger:log(debug, "Mnesia extra db nodes : ~p (needed for tables ~p)", [DbNodes, RemoteTables]),
		    case mnesia:change_config(extra_db_nodes, DbNodes) of
			{error, Reason} ->
			    logger:log(error, "Startup problem: Could not add configured databaseservers: ~p",
				       [mnesia:error_description(Reason)]),
			    throw('Could not add configured databaseservers');
			{ok, _Ret} ->
			    ensure_mnesia_nodes_running(DbNodes),
			    check_for_tables(Tables),
			    ok
		    end,
		    find_mnesia_tables("remote", RemoteTables);
		none ->
		    logger:log(error, "Startup: This application needs remote tables ~p but you "
			       "haven't configured any databaseservers, exiting.",
			       [RemoteTables]),
		    throw('No databaseservers configured (Did you bootstrap YXA? See the README file)')
	    end
    end.

%% part of init_mnesia/1 - make sure that we have a connection to at least one of DbNodes
%% Returns : true | throw()
ensure_mnesia_nodes_running([]) ->
    true;
ensure_mnesia_nodes_running(DbNodes) when is_list(DbNodes) ->
    Running = mnesia:system_info(running_db_nodes),
    ensure_mnesia_nodes_running(DbNodes, Running).

ensure_mnesia_nodes_running([H | T], RunningNodes) when is_atom(H), is_list(RunningNodes) ->
    case lists:member(H, RunningNodes) of
	true ->
	    true;
	false ->
	    ensure_mnesia_nodes_running(T, RunningNodes)
    end;
ensure_mnesia_nodes_running([], _RunningNodes) ->
    logger:log(error, "Startup problem: Could not establish a connection to any of the configured databaseservers"),
    throw('Could not establish a connection to any of the configured databaseservers').

%% init_mnesia_update/0, part of init_mnesia/1. Do table update if we are Mnesia db-master node.
init_mnesia_update() ->
    %% update old database versions
    try table_update:update() of
	ok -> ok
    catch
	EType: Reason ->
	    logger:log(error, "Startup problem: Mnesia table updating failed with '~p' reason :~n~p",
		       [EType, Reason]),
	    timer:sleep(3 * 1000),

	    %% Although very verbose (and only outputted to the console), the output of
	    %% mnesia:info() can be really helpfull
	    io:format("~n~nMnesia info :~n"),
	    mnesia:info(),
	    throw('Mnesia table update error')
    end,
    ok.

get_remote_tables(Tables) ->
    get_remote_tables(Tables, []).

get_remote_tables([H | T], Res) when is_atom(H) ->
    try mnesia:table_info(H, storage_type) of
	unknown ->
	    get_remote_tables(T, [H | Res]);
	_ ->
	    get_remote_tables(T, Res)
    catch
	exit: {aborted, {no_exists, _, storage_type}} ->
	    get_remote_tables(T, [H | Res])
    end;
get_remote_tables([], Res) ->
    {ok, Res}.

%%--------------------------------------------------------------------
%% @spec    (Tables) ->
%%            ok 
%%
%%            Tables = [atom()] "list of table names"
%%
%%            DescriptiveAtom = atom() "reason converted to atom since atoms are displayed best when an application fails to start"
%%
%% @throws  DescriptiveAtom 
%%
%% @doc     Make sure the tables listed in Tables exist, otherwise
%%          halt the Erlang runtime system.
%% @end
%%--------------------------------------------------------------------
check_for_tables([H | T]) ->
    %% check if table exists
    Type = record_name,
    try mnesia:table_info(H, Type) of
	H ->
	    check_for_tables(T)
    catch
	_E: {aborted, {no_exists, H, Type}} ->
	    logger:log(error, "Startup problem: Mnesia table '~p' does not exist - did you bootstrap YXA? "
		       "(see README file)~n", [H]),
	    throw('Missing Mnesia table - YXA probably not bootstrapped')
    end;
check_for_tables([]) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (Descr, Tables) ->
%%            ok 
%%
%%            Descr  = string() "\"local\" or \"remote\""
%%            Tables = [atom()] "names of local/remote Mnesia tables needed by this YXA application."
%%
%%            DescriptiveAtom = atom() "reason converted to atom since atoms are displayed best when an application fails to start"
%%
%% @throws  DescriptiveAtom 
%%
%% @doc     Do mnesia:wait_for_tables() for RemoteTables, with a
%%          timeout since Mnesia doesn't always start correctly due
%%          to network issues, fast restarts or other reasons.
%% @end
%%--------------------------------------------------------------------
find_mnesia_tables(Descr, Tables) ->
    logger:log(debug, "Initializing ~s Mnesia tables ~p", [Descr, Tables]),
    find_mnesia_tables2(Descr, Tables, Tables, 0).

find_mnesia_tables2(Descr, OrigTableList, Tables, Count) ->
    case mnesia:wait_for_tables(Tables, 10000) of
	ok ->
	    ok;
	{timeout, BadTabList} ->
	    case Count of
		3 ->
		    logger:log(normal, "Attempting a Mnesia restart because I'm still waiting for ~s tables ~p",
			       [Descr, BadTabList]),
		    StopRes = mnesia:stop(),
		    StartRes = mnesia:start(),
		    logger:log(debug, "Mnesia stop() -> ~p, start() -> ~p", [StopRes, StartRes]),
		    find_mnesia_tables2(Descr, OrigTableList, OrigTableList, Count + 1);
		6 ->
		    logger:log(error, "Could not initiate ~s Mnesia tables ~p, exiting.", [Descr, BadTabList]),
		    logger:quit(none),
		    Msg = io_lib:format("Mnesia ~s table init error", [Descr]),
		    throw(list_to_atom(lists:flatten(Msg)));
		_ ->
		    logger:log(debug, "Still waiting for ~s tables ~p", [Descr, BadTabList]),
		    find_mnesia_tables2(Descr, OrigTableList, BadTabList, Count + 1)
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Create ETS tables used by YXA.
%% @end
%%--------------------------------------------------------------------
init_statistics() ->
    ets:new(yxa_statistics, [public, set, named_table]),
    true = ets:insert(yxa_statistics, {starttime, util:timestamp()}),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Module, Function, Arguments) ->
%%            Pid
%%
%%            Module    = atom()
%%            Function  = atom()
%%            Arguments = list()
%%
%%            Pid = pid() "spawned process pid"
%%
%% @doc     Run Module:Function(Arguments), in a separate process. Log
%%          errors, but otherwise just ignore them. Relies on Erlang
%%          process links elsewhere to 'fix' errors.
%% @end
%%--------------------------------------------------------------------
safe_spawn(Module, Function, Arguments) ->
    spawn(fun() -> safe_spawn_child(Module, Function, Arguments) end).

safe_spawn_child(Module, Function, Arguments) ->
    try apply(Module, Function, Arguments) of
	Res -> Res
    catch
	exit: E ->
	    logger:log(error, "=ERROR REPORT==== from ~p:~p :~n~p", [Module, Function, E]),
	    erlang:exit(E);
	throw:
	  {siperror, Status, Reason} ->
	    logger:log(error, "Spawned function ~p:~p generated a SIP-error (ignoring) : ~p ~s",
		       [Module, Function, Status, Reason]),
	    throw({siperror, Status, Reason});
	  {siperror, Status, Reason, _} ->
	    logger:log(error, "Spawned function ~p:~p generated a SIP-error (ignoring) : ~p ~s",
		       [Module, Function, Status, Reason]),
	    throw({siperror, Status, Reason})
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, Socket, Status, Reason, ExtraHeaders) ->
%%            ok  |
%%            Res
%%
%%            Request      = #request{}
%%            Socket       = #sipsocket{}
%%            Status       = integer() "SIP response code"
%%            Reason       = string() "error description"
%%            ExtraHeaders = #keylist{}
%%
%%            Res = term() "result of transportlayer:send_result()"
%%
%% @doc     In sipserver we do lots of checking in the dark areas of
%%          transport layer, transaction layer or somewhere in
%%          between. When we detect unparseable requests for example,
%%          we generate an error response in sipserver but special
%%          care must be taken so that we do not generate responses
%%          to malformed ACK's. This function checks that.
%% @end
%%--------------------------------------------------------------------
my_send_result(Request, Socket, Status, Reason, ExtraHeaders) when is_record(Request, request) ->
    case Request#request.method of
	"ACK" ->
	    %% Empirical evidence says that it is a really bad idea to send responses to ACK
	    %% (since the response may trigger yet another ACK). Although not very clearly,
	    %% RFC3261 section 17 (Transactions) do say that responses to ACK is not permitted :
	    %% ' The client transaction is also responsible for receiving responses
	    %%   and delivering them to the TU, filtering out any response
	    %%   retransmissions or disallowed responses (such as a response to ACK).'
	    logger:log(normal, "Sipserver: Suppressing application error response '~p ~s' in response to ACK ~s",
		       [Status, Reason, sipurl:print(Request#request.uri)]);
	_ ->
	    case transactionlayer:send_response_request(Request, Status, Reason, ExtraHeaders) of
		ok -> ok;
		_ ->
		    logger:log(error, "Sipserver: Failed sending caught error ~p ~s (in response to ~s ~s) " ++
			       "using transaction layer - sending directly on the socket we received the request on",
			       [Status, Reason, Request#request.method, sipurl:print(Request#request.uri)]),
		    transportlayer:send_result(Request#request.header, Socket, <<>>, Status, Reason, ExtraHeaders)
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, Socket) ->
%%            ok  |
%%            Res
%%
%%            Request = #request{}
%%            Socket  = #sipsocket{}
%%
%%            Res = term() "result of transportlayer:send_result()"
%%
%% @doc     Send a 500 Server Internal Error, or some other given
%%          error, in response to a request (Request) received on a
%%          specific socket (Socket).
%% @end
%%--------------------------------------------------------------------
internal_error(Request, Socket) when is_record(Request, request), is_record(Socket, sipsocket) ->
    my_send_result(Request, Socket, 500, "Server Internal Error", []).

internal_error(Request, Socket, Status, Reason) when is_record(Request, request), is_record(Socket, sipsocket) ->
    my_send_result(Request, Socket, Status, Reason, []).

internal_error(Request, Socket, Status, Reason, ExtraHeaders) when is_record(Request, request),
								   is_record(Socket, sipsocket) ->
    my_send_result(Request, Socket, Status, Reason, ExtraHeaders).

%%--------------------------------------------------------------------
%% @spec    (Packet, Origin) -> void() "does not matter."
%%
%%            Packet = string()
%%            Origin = #siporigin{}
%%
%% @doc     Check if something we received from a socket (Packet) is a
%%          valid SIP request/response by calling parse_packet() on
%%          it. Then, use my_apply to either send it on to the
%%          transaction layer, or invoke a modules request/3 or
%%          response/3 function on it - depending on the contents of
%%          Dst.
%% @end
%%--------------------------------------------------------------------
process(Packet, Origin) when is_record(Origin, siporigin) ->
    SipSocket = Origin#siporigin.sipsocket,
    case parse_packet(Packet, Origin) of
	{ok, Request, YxaCtx} when is_record(Request, request) ->
	    %% Ok, the parsing and checking of the request is done now
	    try	my_apply(transactionlayer, Request, YxaCtx) of
		_ -> true
	    catch
		error:
		  E ->
		    ST = erlang:get_stacktrace(),
		    logger:log(error, "=ERROR REPORT==== from SIP message handler (in sipserver:process()) :~n"
			       "~p, stacktrace : ~p", [E, ST]),
		    internal_error(Request, SipSocket),
		    %% pass the error on
		    erlang:error({caught_error, E, ST});
		exit:
		  E ->
		    logger:log(error, "=ERROR REPORT==== from SIP message handler (in sipserver:process()) :~n~p", [E]),
		    internal_error(Request, SipSocket),
		    %% pass the error on
		    erlang:exit(E);
		throw:
		  {siperror, Status, Reason} ->
		    logger:log(error, "FAILED processing request: ~s -> ~p ~s",
			       [YxaCtx#yxa_ctx.logstr, Status, Reason]),
		    internal_error(Request, SipSocket, Status, Reason),
		    %% throw a new error, but not the same since we have handled the SIP error sending
		    throw({error, application_failed_processing_request});
		  {siperror, Status, Reason, ExtraHeaders} ->
		    logger:log(error, "FAILED processing request: ~s -> ~p ~s",
			       [YxaCtx#yxa_ctx.logstr, Status, Reason]),
		    internal_error(Request, SipSocket, Status, Reason, ExtraHeaders),
		    %% throw a new error, but not the same since we have handled the SIP error sending
		    throw({error, application_failed_processing_request})
	    end;
	{ok, Response, YxaCtx} when is_record(Response, response) ->
	    my_apply(transactionlayer, Response, YxaCtx);
	Unspecified ->
	    Unspecified
    end.

%%--------------------------------------------------------------------
%% @spec    (Dst, Request, YxaCtx) ->
%%            ignore      |
%%            SIPerror    |
%%            ApplyResult
%%
%%            Dst     = transactionlayer | Module
%%            Module  = atom() "YXA application module name"
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%%            SIPerror    = {siperror, Status, Reason}
%%            Status      = integer()
%%            Reason      = string()
%%            ApplyResult = term() "result of applications request/3 or response/3 function."
%%
%% @doc     If Dst is transactionlayer, gen_server call the
%%          transaction layer and let it decide our next action. If
%%          Dst is the name of a module, apply() that modules
%%          request/2 or response/2 function.
%% @end
%%--------------------------------------------------------------------
my_apply(transactionlayer, R, YxaCtx) when is_record(R, request) orelse is_record(R, response),
					   is_record(YxaCtx, yxa_ctx) ->
    %% Dst is the transaction layer.
    case transactionlayer:from_transportlayer(R, YxaCtx) of
	continue ->
	    %% terminate silently, the transaction layer found an existing transaction
	    %% for this request/response
	    ignore;
	{pass_to_core, AppModule, NewYxaCtx1} ->
	    %% Dst (the transaction layer presumably) wants us to apply a function with this
	    %% request/response as argument. This is common when the transaction layer has started
	    %% a new server transaction for this request and wants it passed to the core (or TU)
	    %% but can't do it itself because that would block the transactionlayer process.
	    Action =
		if
		    is_record(R, request) ->
			local:new_request(AppModule, R, NewYxaCtx1);
		    is_record(R, response) ->
			local:new_response(AppModule, R, NewYxaCtx1)
		end,
	    case Action of
		undefined ->
		    my_apply(AppModule, R, NewYxaCtx1);
		{modified, NewAppModule, NewR, NewYxaCtx} ->
		    logger:log(debug, "Sipserver: Passing possibly modified request/response to application"),
		    my_apply(NewAppModule, NewR, NewYxaCtx);
		ignore ->
		    ignore
	    end
    end;
my_apply(AppModule, Request, YxaCtx) when is_atom(AppModule), is_record(Request, request),
					  is_record(YxaCtx, yxa_ctx) ->
    AppModule:request(Request, YxaCtx);
my_apply(AppModule, Response, YxaCtx) when is_atom(AppModule), is_record(Response, response),
					   is_record(YxaCtx, yxa_ctx) ->
    AppModule:response(Response, YxaCtx).

%%--------------------------------------------------------------------
%% @spec    (Packet, Origin) ->
%%            {ok, R, YxaCtx}       |
%%            void() "unspecified"
%%
%%            Packet = string() | binary()
%%            Origin = #siporigin{}
%%
%%            R      = #request{}  |
%%            #response{}
%%            YxaCtx = #yxa_ctx{}
%%
%% @doc     Check if something we received from a socket (Packet) is a
%%          valid SIP request/response. What we return is a parsed
%%          request/response that has been checked for loops, correct
%%          top-Via etc. together with a logging string that descr-
%%          ibes this request/response.
%% @end
%%--------------------------------------------------------------------
parse_packet(Packet, Origin) when is_record(Origin, siporigin) ->
    Socket = Origin#siporigin.sipsocket,
    case parse_packet2(Packet, Origin) of
	{ok, Parsed} when is_record(Parsed, request); is_record(Parsed, response) ->
	    %% Ok, we have done the elementary parsing of the request/response. Now check it
	    %% for bad things, like loops, wrong IP in top Via (for responses) etc. etc.
	    %%
	    %% From here on, we can generate responses to the UAC on error since we have parsed
	    %% enough of the packet to have a SIP request or response with headers.
	    try process_parsed_packet(Parsed, Origin) of
		{ok, R, YxaCtx} when is_record(R, request) orelse is_record(R, response),
				     is_record(YxaCtx, yxa_ctx) ->
		    {ok, R, YxaCtx};
		invalid ->
		    invalid
	    catch
		exit:
		  E ->
		    logger:log(error, "=ERROR REPORT==== from sipserver:process_parsed_packet() :~n~p", [E]),
		    erlang:exit(E);
		throw:
		  {sipparseerror, request, Header, Status, Reason} ->
		    logger:log(error, "INVALID request [client=~s]: -> ~p ~s",
			       [origin2str(Origin), Status, Reason]),
		    parse_do_internal_error(Header, Socket, Status, Reason, []);
		  {sipparseerror, request, Header, Status, Reason, ExtraHeaders} ->
		    logger:log(error, "INVALID request [client=~s]: -> ~p ~s",
			       [origin2str(Origin), Status, Reason]),
		    parse_do_internal_error(Header, Socket, Status, Reason, ExtraHeaders);
		  {sipparseerror, response, _Header, Status, Reason} ->
		    logger:log(error, "INVALID response [client=~s] -> '~p ~s' (dropping)",
			       [origin2str(Origin), Status, Reason]),
		    false;
		  {sipparseerror, response, _Header, Status, Reason, _ExtraHeaders} ->
		    logger:log(error, "INVALID response [client=~s] -> '~p ~s' (dropping)",
			       [origin2str(Origin), Status, Reason]),
		    false;
		  {siperror, Status, Reason} ->
		    logger:log(error, "INVALID packet [client=~s] -> '~p ~s', CAN'T SEND RESPONSE",
			       [origin2str(Origin), Status, Reason]),
		    false;
		  {siperror, Status, Reason, _ExtraHeaders} ->
		    logger:log(error, "INVALID packet [client=~s] -> '~p ~s', CAN'T SEND RESPONSE",
			       [origin2str(Origin), Status, Reason]),
		    false;
		error:
		  E ->
		    ST = erlang:get_stacktrace(),
		    logger:log(error, "=ERROR REPORT==== from SIP message parser (stage 2) "
			       "(in sipserver:parse_packet()) -> CAN'T SEND RESPONSE :~n~p, "
			       "stacktrace : ~p", [E, ST]),
		    %% pass the error on (will probably go unnoticed)
		    erlang:error({caught_error, E, ST})
	    end;
	ignore -> ignore;
	error -> error
    end.

%% parse_packet2/2 - part of parse_packet/2. Parse the data if it is not in fact already parsed.
parse_packet2(Msg, Origin) when is_record(Msg, request); is_record(Msg, response), is_record(Origin, siporigin) ->
    %% is already parsed
    {ok, Msg};
parse_packet2(Packet, Origin) when is_binary(Packet), is_record(Origin, siporigin) ->
    try sippacket:parse(Packet, Origin) of
	keepalive ->
	    ignore;
	Parsed when is_record(Parsed, request); is_record(Parsed, response) ->
	    {ok, Parsed}
    catch
	exit:
	  E ->
	    logger:log(error, "=ERROR REPORT==== from sippacket:parse() [client=~s]~n~p", [origin2str(Origin), E]),
	    %% awful amount of debug output here, but a parsing crash is serious and it might be
	    %% needed to track the bug down
	    logger:log(debug, "CRASHED parsing packet (binary version):~n~p", [Packet]),
	    logger:log(debug, "CRASHED parsing packet (ASCII version):~n~p", [binary_to_list(Packet)]),
	    error;
	throw:
	  {siperror, Status, Reason} ->
	    logger:log(error, "INVALID packet [client=~s] -> '~p ~s', CAN'T SEND RESPONSE",
		       [origin2str(Origin), Status, Reason]),
	    error;
	  {siperror, Status, Reason, _ExtraHeaders} ->
	    logger:log(error, "INVALID packet [client=~s] -> '~p ~s', CAN'T SEND RESPONSE",
		       [origin2str(Origin), Status, Reason]),
	    error;
	  Error ->
	    logger:log(error, "INVALID packet [client=~s], probably not SIP-message at all (reason: ~p) ",
		       [origin2str(Origin), Error]),
	    error
    end.

%%--------------------------------------------------------------------
%% @spec    (Header, Socket, Status, Reason, ExtraHeaders) -> ok
%%
%%            Header       = term() "opaque #(keylist{})"
%%            Socket       = term() "opaque #(sipsocket{})"
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = term() "opaque #(keylist{})"
%%
%% @doc     Handle errors returned during initial parsing of a
%%          request. These errors occur before the transaction layer
%%          is notified of the requests, so there are never any
%%          server transactions to handle the errors. Just send them.
%% @end
%%--------------------------------------------------------------------
parse_do_internal_error(Header, Socket, Status, Reason, ExtraHeaders) ->
    {_, Method} = sipheader:cseq(Header),
    case Method of
	"ACK" ->
	    %% Empirical evidence says that it is a really bad idea to send responses to ACK
	    %% (since the response may trigger yet another ACK). Although not very clearly,
	    %% RFC3261 section 17 (Transactions) do say that responses to ACK is not permitted :
	    %% ' The client transaction is also responsible for receiving responses
	    %%   and delivering them to the TU, filtering out any response
	    %%   retransmissions or disallowed responses (such as a response to ACK).'
	    logger:log(normal, "Sipserver: Suppressing parsing error response ~p ~s because CSeq method is ACK",
		       [Status, Reason]);
	_ ->
	    transportlayer:send_result(Header, Socket, <<>>, Status, Reason, ExtraHeaders)
    end,
    ok.

%%--------------------------------------------------------------------
%% @spec    (Request, Origin) -> {ok, NewRequest, YxaCtx}
%%
%%            Request = #request{}
%%            Origin  = #siporigin{} "information about where this Packet was received from"
%%
%% @doc     Do alot of transport/transaction layer checking/work on a
%%          request or response we have received and previously
%%          concluded was parseable. For example, do RFC3581 handling
%%          of rport parameter on top via, check for loops, check if
%%          we received a request from a strict router etc.
%% @end
%%--------------------------------------------------------------------
process_parsed_packet(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    NewHeader2 = fix_topvia_received_rport(Request#request.header, Origin),
    check_packet(Request#request{header = NewHeader2}, Origin),
    {NewURI1, NewHeader3} =
	case received_from_strict_router(Request#request.uri, NewHeader2) of
	    true ->
		%% RFC3261 #16.4 (Route Information Preprocessing)
		logger:log(debug, "Sipserver: Received request with a"
			   " Request-URI I (probably) put in a Record-Route. "
			   "Pop real Request-URI from Route-header."),
		ReverseRoute = lists:reverse(keylist:fetch('route', NewHeader2)),
		[LastRoute | ReverseRouteRest] = ReverseRoute,
		[ParsedLastRoute] = contact:parse([LastRoute]),
		NewReqURI = sipurl:parse(ParsedLastRoute#contact.urlstr),
		NewH =
		    case ReverseRouteRest of
			[] ->
			    keylist:delete("Route", NewHeader2);
			_ ->
			    keylist:set("Route", lists:reverse(ReverseRouteRest), NewHeader2)
		end,
		{NewReqURI, NewH};
	    false ->
		{Request#request.uri, NewHeader2}
	end,
    NewURI = remove_maddr_matching_me(NewURI1, Origin),
    NewHeader4 = remove_route_matching_me(NewHeader3),
    NewRequest = Request#request{uri    = NewURI,
				 header = NewHeader4
				},
    LogStr = make_logstr(NewRequest, Origin),
    YxaCtx = #yxa_ctx{origin = Origin,
		      logstr = LogStr
		     },
    {ok, NewRequest, YxaCtx};

%%--------------------------------------------------------------------
%% @spec    (Response, Origin) ->
%%            {NewResponse, LogStr} |
%%            invalid
%%
%%            Response = #response{}
%%            Origin   = #siporigin{} "information about where this Packet was received from"
%%
%% @doc     Do alot of transport/transaction layer checking/work on a
%%          request or response we have received and previously
%%          concluded was parseable. For example, do RFC3581 handling
%%          of rport parameter on top via, check for loops, check if
%%          we received a request from a strict router etc.
%% @end
%%--------------------------------------------------------------------
process_parsed_packet(Response, Origin) when is_record(Response, response), is_record(Origin, siporigin) ->
    check_packet(Response, Origin),
    TopVia = sipheader:topvia(Response#response.header),
    case check_response_via(Origin, TopVia) of
	ok ->
	    LogStr = make_logstr(Response, Origin),
	    YxaCtx = #yxa_ctx{origin = Origin,
			      logstr = LogStr
			     },
	    {ok, Response, YxaCtx};
	error ->
	    %% Silently drop packet
	    invalid
    end.

%%--------------------------------------------------------------------
%% @spec    (Origin, TopVia) ->
%%            ok    |
%%            error
%%
%%            Origin = #siporigin{} "information about where this Packet was received from"
%%            TopVia = #via{} | none
%%
%% @doc     Check that there actually was a Via header in this
%%          response, and check if it matches us.
%% @end
%%--------------------------------------------------------------------
check_response_via(Origin, none) when is_record(Origin, siporigin) ->
    logger:log(error, "INVALID top-Via in response [client=~s] (no Via found).",
	       [origin2str(Origin)]),
    error;
check_response_via(Origin, TopVia) when is_record(Origin, siporigin), is_record(TopVia, via) ->
    %% Check that top-Via is ours (RFC 3261 18.1.2),
    %% silently drop message if it is not.

    %% Create a Via that looks like the one we would have produced if we sent the request
    %% this is an answer to, but don't include parameters since they might have changed
    Proto = Origin#siporigin.proto,
    %% This is what we expect, considering the protocol in Origin (Proto)
    MyViaNoParam = siprequest:create_via(Proto, []),
    %% But we also accept this, which is the same but with the protocol from this response - in
    %% case we sent the request out using TCP but received the response over UDP for example
    SentByMeNoParam = siprequest:create_via(sipsocket:viastr2proto(TopVia#via.proto), []),
    case sipheader:via_is_equal(TopVia, MyViaNoParam, [proto, host, port]) of
        true ->
	    ok;
	false ->
	    case sipheader:via_is_equal(TopVia, SentByMeNoParam, [proto, host, port]) of
		true ->
		    %% This can happen if we for example send a request out on a TCP socket, but the
		    %% other end responds over UDP.
		    logger:log(debug, "Sipserver: Warning: received response [client=~s]"
			       " matching me, but different protocol ~p (received on: ~p)",
			       [origin2str(Origin),
				TopVia#via.proto, sipsocket:proto2viastr(Origin#siporigin.proto)]),
		    ok;
		false ->
		    logger:log(error, "INVALID top-Via in response [client=~s]."
			       " Top-Via (without parameters) (~s) does not match mine (~s). Discarding.",
			       [origin2str(Origin), sipheader:via_print([TopVia#via{param=[]}]),
				sipheader:via_print([MyViaNoParam])]),
		    error
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Header, Origin) ->
%%            NewHeader
%%
%%            Header = term() "opaque #(keylist{})"
%%            Origin = #siporigin{}
%%
%%            NewHeader = term() "opaque (a new #keylist{})"
%%
%% @doc     Implement handling of rport= top Via parameter upon
%%          receiving a request with an 'rport' parameter. RFC3581.
%%          Even if there is no rport parameter, we check if we must
%%          add a received= parameter (RFC3261 #18.2.1).
%% @end
%%--------------------------------------------------------------------
%% XXX this RFC3581 implementation is not 100% finished. RFC3581 Section 4 says we MUST
%% send the responses to this request back from the same IP and port we received the
%% request to. We should be able to solve this when sending responses if we keep a list
%% of requests and sockets even for requests received over UDP too. XXX make it so.
fix_topvia_received_rport(Header, Origin) when is_record(Origin, siporigin) ->
    IP = Origin#siporigin.addr,
    Port = Origin#siporigin.port,
    PortStr = integer_to_list(Port),
    TopVia = sipheader:topvia(Header),
    ParamDict = sipheader:param_to_dict(TopVia#via.param),
    %% RFC3581 Section 4 says we MUST add a received= parameter when client
    %% requests rport even if the sent-by is set to the IP-address we received
    %% the request from, so if we find an rport below we always add received=
    %% information.
    case dict:find("rport", ParamDict) of
	error ->
	    %% No rport, check "sent-by" in top-Via to see if we still MUST add a
	    %% received= parameter (RFC 3261 #18.2.1)
	    case TopVia#via.host of
		IP ->
		    Header;
		_ ->
		    NewDict = dict:store("received", IP, ParamDict),
		    NewVia = TopVia#via{param=sipheader:dict_to_param(NewDict)},
		    logger:log(debug, "Sipserver: Top Via host ~p does not match IP ~p, appending received=~s parameter",
			       [TopVia#via.host, IP, IP]),
		    replace_top_via(NewVia, Header)
	    end;
	{ok, []} ->
	    %% rport without value, this is like it should be. Add value and received=.
	    logger:log(debug, "Sipserver: Client requests symmetric response routing, setting rport=~p", [Port]),
	    NewDict1 = dict:store("rport", PortStr, ParamDict),
	    NewDict = dict:store("received", IP, NewDict1),
	    NewVia = TopVia#via{param=sipheader:dict_to_param(NewDict)},
	    replace_top_via(NewVia, Header);
	{ok, PortStr} ->
	    %% rport already set to the value we would have set it to - client is not
	    %% RFC-compliant. Just add received= information.
	    logger:log(debug, "Sipserver: Top Via has rport already set to ~p, remote party "
		       "isn't very RFC3581 compliant.", [Port]),
	    NewDict = dict:store("received", IP, ParamDict),
	    NewVia = TopVia#via{param=sipheader:dict_to_param(NewDict)},
	    replace_top_via(NewVia, Header);
	{ok, RPort} ->
	    %% rport already set, and to the WRONG value. Must be a NAT or some other
	    %% evildoer in the path of the request. Fix the rport value, and add received=.
	    logger:log(normal, "Sipserver: Received request with rport already containing a value (~p)! "
		       "Overriding with the right port (~p).", [RPort, Port]),
	    NewDict1 = dict:store("rport", PortStr, ParamDict),
	    NewDict = dict:store("received", IP, NewDict1),
	    NewVia = TopVia#via{param=sipheader:dict_to_param(NewDict)},
	    replace_top_via(NewVia, Header)
    end.

%% Replace top Via header in a keylist record()
replace_top_via(NewVia, Header) when is_record(NewVia, via) ->
    [_FirstVia | Via] = sipheader:via(Header),
    keylist:set("Via", sipheader:via_print(lists:append([NewVia], Via)), Header).

%%--------------------------------------------------------------------
%% @spec    (URI, Header) ->
%%            true |
%%            false
%%
%%            URI    = #sipurl{}
%%            Header = term() "opaque #(keylist{})"
%%
%% @doc     Look at the URI of a request we just received to see if it
%%          is something we (possibly) put in a Record-Route and this
%%          is a request sent from a strict router (RFC2543 compliant
%%          UA).
%% @end
%%--------------------------------------------------------------------
received_from_strict_router(URI, Header) when is_record(URI, sipurl) ->
    MyPorts = sipsocket:get_all_listenports(),
    {ok, MyHostnames} = yxa_config:get_env(myhostnames, []),
    HostnameList = MyHostnames ++ siphost:myip_list(),
    %% If the URI has a username in it, it is not something we've put in a Record-Route
    case URI#sipurl.user of
	T when is_list(T) ->
	    %% short-circuit the rest of the checks for efficiency (most normal Request-URI's
	    %% have the username part set).
	    false;
	none ->
	    LCHost = http_util:to_lower(URI#sipurl.host),
	    HostnameIsMyHostname = lists:member(LCHost, HostnameList),
	    %% In theory, we should not treat an absent port number in this Request-URI as
	    %% if the default port number was specified in there, but in practice that is
	    %% what we have to do since some UAs can remove the port we put into the
	    %% Record-Route
	    Port = sipsocket:default_port(URI#sipurl.proto, sipurl:get_port(URI)),
	    PortMatches = lists:member(Port, MyPorts),
	    HeaderHasRoute = case keylist:fetch('route', Header) of
				 [] -> false;
				 _ -> true
			     end,
	    if
		HostnameIsMyHostname /= true -> false;
		PortMatches /= true -> false;
		HeaderHasRoute /= true ->
		    logger:log(debug, "Sipserver: Warning: Request-URI looks like something"
			       " I put in a Record-Route header, but request has no Route!"),
		    false;
		true -> true
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (URI, Origin) ->
%%            NewURI
%%
%%            URI    = #sipurl{}
%%            Origin = #siporigin{}
%%
%%            NewURI = #sipurl{}
%%
%% @doc     Perform some rather complex processing of a Request-URI if
%%          it has an maddr parameter. RFC3261 #16.4 (Route
%%          Information Preprocessing) requires this as some kind of
%%          backwards compatibility thing for clients that are trying
%%          to do loose routing while being strict routers. The world
%%          would be a better place without this need.
%% @end
%%--------------------------------------------------------------------
remove_maddr_matching_me(URI, Origin) when is_record(URI, sipurl), is_record(Origin, siporigin) ->
    case url_param:find(URI#sipurl.param_pairs, "maddr") of
        [MAddr] ->
	    LCMaddr = http_util:to_lower(MAddr),
	    {ok, MyHostnames} = yxa_config:get_env(myhostnames, []),
	    {ok, Homedomains} = yxa_config:get_env(homedomain, []),
	    case lists:member(LCMaddr, siphost:myip_list())
		orelse lists:member(LCMaddr, MyHostnames)
		orelse lists:member(LCMaddr, Homedomains) of
		true ->
		    %% Ok, maddr matches me or something 'the proxy is configured
		    %% to be responsible for'. Now check if port and transport in URI
		    %% matches what we received the request using, either explicitly
		    %% or by default. Sigh.
		    Port = sipsocket:default_port(URI#sipurl.proto, sipurl:get_port(URI)),
		    MyPort = case URI#sipurl.proto of
				 "sips" -> sipsocket:get_listenport(tls);
				 _ -> sipsocket:get_listenport(udp)
			     end,
		    case (Port == MyPort) of
			true ->
			    IsDefaultPort = (Port == sipsocket:default_port(URI#sipurl.proto, none)),
			    %% lastly check transport parameter too
			    case url_param:find(URI#sipurl.param_pairs, "transport") of
				[] ->
				    NewParam = url_param:remove(URI#sipurl.param_pairs, "maddr"),
				    %% implicit match since transport was not specified,
				    %% now 'strip the maddr and any non-default port'
				    case IsDefaultPort of
					true ->
					    sipurl:set([{param, NewParam}], URI);
					false ->
					    sipurl:set([{param, NewParam}, {port, none}], URI)
				    end;
				[Transport] ->
				    LCTransport = http_util:to_lower(Transport),
				    Matches =
					case {LCTransport, Origin#siporigin.proto} of
					    {"tcp", TCP} when TCP == tcp; TCP == tcp6 -> true;
					    {"udp", UDP} when UDP == udp; UDP == udp6 -> true;
					    {"tls", TLS} when TLS == tls; TLS == tls6 -> true;
					    _ -> false
					end,
				    case Matches of
					true ->
					    %% explicit match on transport, 'strip the maddr and any
					    %% non-default port or transport parameter'
					    NewParam1 = url_param:remove(URI#sipurl.param_pairs, "maddr"),
					    NewParam = url_param:remove(NewParam1, "transport"),
					    case IsDefaultPort of
						true ->
						    sipurl:set([{param, NewParam}], URI);
						false ->
						    sipurl:set([{param, NewParam}, {port, none}], URI)
					    end;
					false ->
					    %% transport parameter did not match
					    URI
				    end
			    end;
			false ->
			    %% port did not match
			    URI
		    end;
		false ->
		    %% maddr does not match me
		    URI
	    end;
	_ ->
	    %% no maddr
	    URI
    end.

%%--------------------------------------------------------------------
%% @spec    (Header) ->
%%            NewHeader
%%
%%            Header = term() "opaque #(keylist{})"
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Look at the first Route header element in Header (if any)
%%          and see if it matches this proxy. If so, remove the first
%%          element and return a new Header.
%% @end
%%--------------------------------------------------------------------
remove_route_matching_me(Header) ->
    case keylist:fetch('route', Header) of
        [FirstRoute | RouteRest] ->
	    [FirstRouteParsed] = contact:parse([FirstRoute]),
	    case route_matches_me(FirstRouteParsed) of
		true ->
		    logger:log(debug, "Sipserver: First Route ~p matches me, removing it.",
			       [FirstRoute]),
		    case RouteRest of
			[] ->
			    keylist:delete('route', Header);
			_ ->
			    keylist:set("Route", RouteRest, Header)
		    end;
		false ->
		    Header
	    end;
	_ ->
	    %% No Route header
	    Header
    end.

%%--------------------------------------------------------------------
%% @spec    (Route) ->
%%            true  |
%%            false
%%
%%            Route = #contact{}
%%
%% @doc     Helper function for remove_route_matching_me/1. Check if
%%          an URL matches this proxys name (or address) and port.
%% @end
%%--------------------------------------------------------------------
route_matches_me(Route) when is_record(Route, contact) ->
    URL = sipurl:parse(Route#contact.urlstr),

    MyPorts = sipsocket:get_all_listenports(),
    Port = sipsocket:default_port(URL#sipurl.proto, sipurl:get_port(URL)),
    PortMatches = lists:member(Port, MyPorts),

    LChost = http_util:to_lower(URL#sipurl.host),
    {ok, MyHostnames} = yxa_config:get_env(myhostnames, []),
    HostnameMatches = (lists:member(LChost, MyHostnames) orelse
		       lists:member(LChost, siphost:myip_list())
		      ),

    case {HostnameMatches, PortMatches} of
	{true, true} ->
	    true;
	{true, false} ->
	    logger:log(debug, "Sipserver: Hostname ~p matches me, but port ~p derived from Route-header does not. "
		       "Concluding that first Route does not match me.", [LChost, Port]),
	    false;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (Packet, Origin) -> ok
%%
%%            Packet = #request{} | #response{}
%%            Origin = #siporigin{} "information about where this Packet was received from"
%%
%% @throws  {sipparseerror, request, Header, Status, Reason} 
%%
%% @doc     Sanity check To: and From: in a received request/response
%%          and, if Packet is a request record(), also check sanity
%%          of CSeq and (unless configured not to) check for a
%%          looping request.
%% @end
%%--------------------------------------------------------------------
%%
%% Packet is request record()
%%
check_packet(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    {Method, Header} = {Request#request.method, Request#request.header},
    check_supported_uri_scheme(Request#request.uri, Header),
    sanity_check_contact(request, "From", Header),
    sanity_check_contact(request, "To", Header),
    case sipheader:cseq(Header) of
	{unparseable, CSeqStr} ->
	    logger:log(error, "INVALID CSeq '~p' in packet from ~s", [CSeqStr, origin2str(Origin)]),
	    throw({sipparseerror, request, Header, 400, "Invalid CSeq"});
	{CSeqNum, CSeqMethod} ->
	    case util:isnumeric(CSeqNum) of
		false ->
		    throw({sipparseerror, request, Header, 400, "CSeq number '" ++
			   CSeqNum ++ "' is not an integer"});
		_ -> true
	    end,
	    if
		CSeqMethod /= Method ->
		    throw({sipparseerror, request, Header, 400, "CSeq Method " ++ CSeqMethod ++
			   " does not match request Method " ++ Method});
		true -> true
	    end;
	_ ->
	    logger:log(error, "INVALID CSeq in packet from ~s", [origin2str(Origin)]),
	    throw({sipparseerror, request, Header, 400, "Invalid CSeq"})
    end,
    case yxa_config:get_env(detect_loops) of
	{ok, true} ->
	    check_for_loop(Header, Request#request.uri, Origin);
	{ok, false} ->
	    ok
    end;
%%
%% Packet is response record()
%%
check_packet(Response, Origin) when is_record(Response, response), is_record(Origin, siporigin) ->
    %% Check that the response code is within range. draft-ietf-sipping-torture-tests-04.txt
    %% #3.1.2.19 suggests that an element that receives a response with an overly large response
    %% code should simply drop it (that is what happens if we throw a sipparseerror when parsing
    %% responses).
    if
	Response#response.status >= 100, Response#response.status =< 699 -> ok;
	true ->
	    throw({sipparseerror, response, Response#response.header, 400, "Response code out of bounds"})
    end,
    sanity_check_contact(response, "From", Response#response.header),
    sanity_check_contact(response, "To", Response#response.header),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Header, URI, Origin) -> ok
%%
%%            Header = term() "opaque #(keylist{})"
%%            URI    = term() "opaque #(sipurl{})"
%%            Origin = #siporigin{} "information about where this Packet was received from"
%%
%% @throws  {sipparseerror, request, Header, Status, Reason} 
%%
%% @doc     Inspect Header's Via: record(s) to make sure this is not a
%%          looping request.
%% @end
%%--------------------------------------------------------------------
check_for_loop(Header, URI, Origin) when is_record(Origin, siporigin) ->
    LoopCookie = siprequest:get_loop_cookie(Header, URI, Origin#siporigin.proto),
    MyHostname = siprequest:myhostname(),
    MyPort = sipsocket:get_listenport(Origin#siporigin.proto),
    CmpVia = #via{host = MyHostname,
		  port = MyPort
		 },

    case via_indicates_loop(LoopCookie, CmpVia, sipheader:via(Header)) of
	true ->
	    logger:log(debug, "Sipserver: Found a loop when inspecting the Via headers, "
		       "throwing SIP-serror '482 Loop Detected'"),
	    throw({sipparseerror, request, Header, 482, "Loop Detected"});
	false ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (LoopCookie, CmpVia, ViaList) ->
%%            true  |
%%            false
%%
%%            LoopCookie = string() "loop cookie as generated by siprequest:get_loop_cookie/3."
%%            CmpVia     = #via{} "what my Via would look like"
%%            ViaList    = [#via{}]
%%
%% @doc     Helper function for check_for_loop/3. See that function.
%% @end
%%--------------------------------------------------------------------
via_indicates_loop(_LoopCookie, _CmpVia, []) ->
    false;
via_indicates_loop(LoopCookie, CmpVia, [TopVia | Rest]) when is_record(TopVia, via)->
    case sipheader:via_is_equal(TopVia, CmpVia, [host, port]) of
	true ->
	    %% Via matches me
	    ParamDict = sipheader:param_to_dict(TopVia#via.param),
	    %% Can't use sipheader:get_via_branch() since it strips the loop cookie
	    case dict:find("branch", ParamDict) of
		error ->
		    %% XXX should broken Via perhaps be considered fatal?
		    logger:log(error, "Sipserver: Request has Via that matches me,"
			       " but no branch parameter. Loop checking broken!"),
		    logger:log(debug, "Sipserver: Via ~p matches me, but has no branch parameter."
			       " Loop checking broken!",
			       sipheader:via_print([TopVia])),
		    via_indicates_loop(LoopCookie, CmpVia, Rest);
		{ok, Branch} ->
		    case extract_loopcookie(Branch, length(LoopCookie)) of
			LoopCookie ->
			    true;
			_ ->
			    %% Loop cookie does not match, check next (request might have passed
			    %% this proxy more than once, loop can be further back the via trail)
			    via_indicates_loop(LoopCookie, CmpVia, Rest)
		    end
	    end;
	false ->
	    %% Via doesn't match me, check next.
	    via_indicates_loop(LoopCookie, CmpVia, Rest)
    end.

%% part of via_indicates_loop/3
%% Returns: LoopCookieString | error
extract_loopcookie(Branch, CookieLen) when CookieLen + 13 >= length(Branch) -> %% 13 is length("z9hG4bK-yxa-X")
    %% XXX should broken Via perhaps be considered fatal?
    logger:log(error, "Sipserver: Request has Via that matches me, but is too short. Loop checking broken!"),
    error;
extract_loopcookie(Branch, CookieLen) ->
    %% We lowercase the branch here because branches are tokens which are case-
    %% insensitive. Detecting loops are important, so we go through the extra
    %% trouble of being compliant by the letter here, in case someone has changed
    %% the casing of our Via header branch (never seen, but rumored to happen).
    RBranch1 = lists:reverse(Branch),
    %% lowercase as little of Branch as possible
    RBranch2 = http_util:to_lower( string:substr(RBranch1, 1, CookieLen + 2) ),	%% 2 is length("-o")
    %% now check for -o indicating a real loop cookie
    case lists:reverse(RBranch2) of
	"-o" ++ LoopCookie ->
	    LoopCookie;
	_ ->
	    %% XXX should broken Via perhaps be considered fatal?
	    logger:log(error, "Sipserver: Request has Via that matches me, has no loop cookie. Loop checking broken!"),
	    error
    end.


%%--------------------------------------------------------------------
%% @spec    (R, Origin) ->
%%            LogStr
%%
%%            R      = #request{} | #response{}
%%            Origin = #siporigin{}
%%
%%            LogStr = string()
%%
%% @doc     Create a textual representation of a request/response, for
%%          use in logging. Note :
%%          draft-ietf-sipping-torture-tests-04.txt argues that a
%%          proxy shouldn't fail processing a packet just because it
%%          has a From: header using an URI scheme that it doesn't
%%          understand - like http. Well, we do - here. The reason
%%          for not just fixing this here is that there might be
%%          other places where we expect the From: to be parsable by
%%          our sipheader:from() - and this hasn't been a problem in
%%          real life.
%% @end
%%--------------------------------------------------------------------
make_logstr(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    {Method, URI, Header} = {Request#request.method, Request#request.uri, Request#request.header},
    {_, FromURI} = sipheader:from(Header),
    {_, ToURI} = sipheader:to(Header),
    ClientStr = origin2str(Origin),
    lists:flatten(io_lib:format("~s ~s [client=~s, from=<~s>, to=<~s>]",
				[Method, sipurl:print(URI), ClientStr, url2str(FromURI), url2str(ToURI)]));
make_logstr(Response, Origin) when is_record(Response, response), is_record(Origin, siporigin) ->
    Header = Response#response.header,
    {_, CSeqMethod} = sipheader:cseq(Header),
    {_, FromURI} = sipheader:from(Header),
    {_, ToURI} = sipheader:to(Header),
    ClientStr = origin2str(Origin),
    case keylist:fetch('warning', Header) of
	[Warning] when is_list(Warning) ->
	    lists:flatten(io_lib:format("~s [client=~s, from=<~s>, to=<~s>, warning=~p]",
					[CSeqMethod, ClientStr, url2str(FromURI), url2str(ToURI), Warning]));
	_ ->
	    %% Zero or more than one Warning-headers
	    lists:flatten(io_lib:format("~s [client=~s, from=<~s>, to=<~s>]",
					[CSeqMethod, ClientStr, url2str(FromURI), url2str(ToURI)]))
    end.

%% part of make_logstr/2
url2str(URL) when is_record(URL, sipurl) ->
    sipurl:print(URL);
url2str({unparseable, URLstr}) when is_list(URLstr) ->
    "unparseable".

%%--------------------------------------------------------------------
%% @spec    (Type, Name, Header) -> term()
%%
%%            Type   = request | response
%%            Name   = string() "\"From\" or \"To\" or similar"
%%            Header = #keylist{}
%%
%% @throws  {sipparseerror, Type, Header, Status, Reason} 
%%
%% @doc     Check if the header Name (from Header) is parsable.
%%          Currently we define parsable as parsable by
%%          sipheader:from().
%% @end
%%--------------------------------------------------------------------
sanity_check_contact(Type, Name, Header) when Type == request; Type == response; is_list(Name),
					      is_record(Header, keylist) ->
    case keylist:fetch(Name, Header) of
	[Str] when is_list(Str) ->
	    try sipheader:from([Str]) of
		{_, URI} when is_record(URI, sipurl) ->
		    sanity_check_uri(Type, Name ++ ":", URI, Header);
		_ ->
		    throw({sipparseerror, Type, Header, 400, "Invalid " ++ Name ++ ": header"})
	    catch
		_X: _Y ->
		    throw({sipparseerror, Type, Header, 400, "Invalid " ++ Name ++ ": header"})
	    end;
	_ ->
	    %% Header is either missing, or there was more than one
	    throw({sipparseerror, Type, Header, 400, "Missing or invalid " ++ Name ++ ": header"})
    end.

%% part of sanity_check_contact/4
sanity_check_uri(Type, Desc, URI, Header)  when is_record(URI, sipurl), URI#sipurl.host == none ->
    throw({sipparseerror, Type, Header, 400, "No host part in " ++ Desc ++ " URL"});
sanity_check_uri(_Type, _Desc, URI, _Header) when is_record(URI, sipurl) ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (URI, Header) -> term()
%%
%%            URI    = #sipurl{} | {unparseable, URIstr}
%%            Header = #keylist{}
%%
%% @throws  {sipparseerror, Type, Header, Status, Reason} 
%%
%% @doc     Check if we supported the URI scheme of a request. If we
%%          didn't support the URI scheme, sipurl:parse(...) will
%%          have failed, and we just format the 416 error response.
%% @end
%%--------------------------------------------------------------------
check_supported_uri_scheme({unparseable, URIstr}, Header) when is_list(URIstr), is_record(Header, keylist) ->
    case string:chr(URIstr, $:) of
	0 ->
	    throw({sipparseerror, request, Header, 416, "Unsupported URI Scheme"});
	Index ->
	    case http_util:to_lower(string:substr(URIstr, 1, Index)) of
		Proto when Proto == "sip:"; Proto == "sips:" ->
		    throw({sipparseerror, request, Header, 400, "Unparsable Request-URI"});
		Scheme ->
		    throw({sipparseerror, request, Header, 416, "Unsupported URI Scheme (" ++ Scheme ++ ")"})
	    end
    end;
check_supported_uri_scheme(URI, Header) when is_record(URI, sipurl), is_record(Header, keylist) ->
    true.

%%--------------------------------------------------------------------
%% @spec    (Origin) ->
%%            OriginStr
%%
%%            Origin  = #siporigin{}
%%            Default = term()
%%
%%            OriginStr = string()
%%
%% @doc     Turn a siporigin record into a string.
%% @end
%%--------------------------------------------------------------------
origin2str(Origin) when is_record(Origin, siporigin) ->
    lists:concat([Origin#siporigin.proto, ":", Origin#siporigin.addr, ":", Origin#siporigin.port]).



%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------


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
    EmptyBody = <<>>,	%% Work around compiler bug in Erlang R10B-2

    %% build request header
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "init variables - 1"),

    MyHostname = siprequest:myhostname(),
    SipPort  = sipsocket:get_listenport(tcp),
    SipsPort = sipsocket:get_listenport(tls),

    ViaMe = siprequest:create_via(tcp, []),
    ViaOrigin1 = #siporigin{proto=tcp},


    %% test check_response_via(Origin, TopVia)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_response_via/2 - 1"),
    %% straight forward, via is what this proxy would use (ViaMe)
    ok = check_response_via(ViaOrigin1, ViaMe),

    autotest:mark(?LINE, "check_response_via/2 - 2"),
    %% received response over unexpected protocol, still ok since this can
    %% happen if we for example send a request out over TCP but the other end
    %% fails to send the response back to us using the same connection so the
    %% response is received over UDP
    ok = check_response_via(ViaOrigin1#siporigin{proto=udp}, ViaMe),

    autotest:mark(?LINE, "check_response_via/2 - 3"),
    %% no top via, invalid
    error = check_response_via(ViaOrigin1#siporigin{proto=udp}, none),


    %% test process_parsed_packet(Response, Origin)
    %% tests sanity_check_contact(Type, Name, Header) indirectly
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "build response - 1"),
    CRHeader1 = keylist:from_list([
				    {"Via",	sipheader:via_print([ViaMe])},
				    {"Via",	["SIP/2.0/FOO 192.0.2.78"]},
				    {"From",	["<sip:user1@example.org>"]},
				    {"To",	["\"Joe\" <sip:user2@example.org>"]},
				    {"CSeq",	["10 MESSAGE"]}
				   ]),
    CheckResponse1 = #response{status=200, reason="Ok", header=CRHeader1, body=EmptyBody},

    autotest:mark(?LINE, "process_parsed_packet/2 response - 1"),
    %% straight forward
    {ok, #response{}, #yxa_ctx{}} = process_parsed_packet(CheckResponse1, #siporigin{proto=tcp}),

    autotest:mark(?LINE, "process_parsed_packet/2 response - 2"),
    CRHeader2 = keylist:delete("Via", CRHeader1),
    CheckResponse2 = #response{status=200, reason="Ok", header=CRHeader2, body=EmptyBody},
    %% without Via-headers
    invalid = process_parsed_packet(CheckResponse2, #siporigin{proto=tcp}),

    autotest:mark(?LINE, "process_parsed_packet/2 response - 3 (disabled)"),
    CRHeader3 = keylist:set("From", ["http://www.example.org/"], CRHeader1),
    CheckResponse3 = #response{status=200, reason="Ok", header=CRHeader3, body=EmptyBody},
    %% http From: URL, draft-ietf-sipping-torture-tests-04 argues that a proxy
    %% should be able to process a request/response with this unless the
    %% proxy really has to understand the From:. We currently don't.
    _ = (catch process_parsed_packet(CheckResponse3, #siporigin{proto=tcp})),

    autotest:mark(?LINE, "process_parsed_packet/2 response - 4"),
    CRHeader4 = keylist:set("From", ["<sip:test@example.org"], CRHeader1),
    CheckResponse4 = #response{status=200, reason="Ok", header=CRHeader4, body=EmptyBody},
    %% check with From: without closing ">"
    {sipparseerror, response, _, 400, "Invalid From: header"} =
	(catch process_parsed_packet(CheckResponse4, #siporigin{proto=tcp})),


    %% build request header for other tests
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "build request header - 0"),
    ReqHeader1 = keylist:from_list([{"Via", ["SIP/2.0/TLS 192.0.2.78"]}]),
    Origin1 = #siporigin{proto=tcp, addr="192.0.2.78", port=1234},
    Origin2 = #siporigin{proto=tcp, addr="192.0.2.200", port=2345},

    autotest:mark(?LINE, "build request header - 1"),
    ReqHeader10 = keylist:from_list([
				     {"Via",	["SIP/2.0/TLS 130.237.90.1:111",
						 "SIP/2.0/TCP 2001:6b0:5:987::1"]},
				     {"From",	["<sip:test@it.su.se>;tag=f-123"]},
				     {"To",	["<sip:test@it.su.se>;tag=t-123"]},
				     {"CSeq",	["4711 INVITE"]},
				     {"Call-ID",	["abc123@test"]},
				     {"Route",	["<sip:p1:1111>", "<sip:p2:2222>"]}
				    ]),
    MyRoute = contact:parse(["<sip:" ++ MyHostname ++ ":" ++ integer_to_list(SipPort) ++ ">"]),
    MyRouteStr = contact:print(MyRoute),

    MyRoute2 = contact:parse(["<sip:" ++ siphost:myip() ++ ":" ++ integer_to_list(SipPort) ++ ">"]),
    MyRouteStr2 = contact:print(MyRoute2),

    %% port should not match me
    MyRoute3 = contact:parse(["<sip:" ++ MyHostname ++ ":4711>"]),
    MyRouteStr3 = contact:print(MyRoute3),


    %% test fix_topvia_received_rport(Header, Origin)
    %%--------------------------------------------------------------------

    %% no rport parameter tests :

    autotest:mark(?LINE, "fix_topvia_received_rport/2 no rport - 1.1"),
    %% check Via that is IP-address (the right one), and no rport parameter
    ReqHeader1_1 = fix_topvia_received_rport(ReqHeader1, Origin1),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 no rport - 1.2"),
    %% check result
    ["SIP/2.0/TLS 192.0.2.78"] = keylist:fetch(via, ReqHeader1_1),


    autotest:mark(?LINE, "fix_topvia_received_rport/2 no rport - 2.1"),
    %% check Via that is IP-address (but not the same as in Origin2), and no rport parameter
    ReqHeader1_2 = fix_topvia_received_rport(ReqHeader1, Origin2),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 no rport - 2.2"),
    %% check result
    ["SIP/2.0/TLS 192.0.2.78;received=192.0.2.200"] = keylist:fetch(via, ReqHeader1_2),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 no rport - 3.1"),
    ReqHeader2 = keylist:from_list([{"Via", ["SIP/2.0/TLS phone.example.org"]}]),
    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 3.2"),
    %% check Via that is hostname, and no rport parameter
    ReqHeader2_1 = fix_topvia_received_rport(ReqHeader2, Origin1),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 no rport - 3.3"),
    %% check result
    ["SIP/2.0/TLS phone.example.org;received=192.0.2.78"] = keylist:fetch(via, ReqHeader2_1),

    %% rport parameter tests :

    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 1.1"),
    ReqHeader3 = keylist:from_list([{"Via", ["SIP/2.0/TLS 192.0.2.78;rport"]}]),
    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 1.2"),
    %% check Via that is IP address, with rport. When rport exists, we MUST add a
    %% received= even if the host-part equals the address we received the request from
    ReqHeader3_1 = fix_topvia_received_rport(ReqHeader3, Origin1),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 1.3"),
    %% check result
    ["SIP/2.0/TLS 192.0.2.78;received=192.0.2.78;rport=1234"] = keylist:fetch(via, ReqHeader3_1),


    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 2.1"),
    %% check Via that is IP address (wrong address), with rport.
    ReqHeader3_2 = fix_topvia_received_rport(ReqHeader3, Origin2),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 2.2"),
    %% check result
    ["SIP/2.0/TLS 192.0.2.78;received=192.0.2.200;rport=2345"] = keylist:fetch(via, ReqHeader3_2),


    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 3.1"),
    ReqHeader4 = keylist:from_list([{"Via", ["SIP/2.0/TCP phone.example.org;rport"]}]),
    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 3.2"),
    %% check Via that is hostname, with rport.
    ReqHeader4_1 = fix_topvia_received_rport(ReqHeader4, Origin2),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 3.3"),
    %% check result
    ["SIP/2.0/TCP phone.example.org;received=192.0.2.200;rport=2345"] = keylist:fetch(via, ReqHeader4_1),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 4.1"),
    RportHeader4_1 = keylist:from_list([{"Via", ["SIP/2.0/TCP phone.example.org;rport=2345"]}]),
    %% rport with a value (right value, but still set - shouldn't be but we should handle it)
    RPortHeader4_2 = fix_topvia_received_rport(RportHeader4_1, Origin2),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 4.2"),
    %% check result
    ["SIP/2.0/TCP phone.example.org;received=192.0.2.200;rport=2345"] = keylist:fetch(via, RPortHeader4_2),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 5.1"),
    RportHeader5_1 = keylist:from_list([{"Via", ["SIP/2.0/TCP phone.example.org;rport=1111"]}]),
    %% rport with a value (WRONG value (1111), should be replaced by right value (2345))
    RPortHeader5_2 = fix_topvia_received_rport(RportHeader5_1, Origin2),

    autotest:mark(?LINE, "fix_topvia_received_rport/2 - 5.2"),
    %% check result
    ["SIP/2.0/TCP phone.example.org;received=192.0.2.200;rport=2345"] = keylist:fetch(via, RPortHeader5_2),


    %% test remove_route_matching_me(Header)
    %% indirectly tests route_matches_me(Route)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "remove_route_matching_me/1 - 1"),
    %% These two Route headers doesn't match me
    ["<sip:p1:1111>", "<sip:p2:2222>"] =
	keylist:fetch(route, remove_route_matching_me(ReqHeader10)),

    autotest:mark(?LINE, "remove_route_matching_me/1 - 2"),
    %% Test a single matching Route, should result in empty route set
    [] = keylist:fetch(route, remove_route_matching_me(
				keylist:set("Route", [MyRouteStr], ReqHeader10)
				)),

    autotest:mark(?LINE, "remove_route_matching_me/1 - 3"),
    %% Test a matching Route, and some non-matching
    ["<sip:example.org>"] = keylist:fetch(route,
					  remove_route_matching_me(
					    keylist:set("Route", [MyRouteStr, "<sip:example.org>"], ReqHeader10)
					   )),


    autotest:mark(?LINE, "remove_route_matching_me/1 - 4"),
    %% Test a double matching Route, should result in the second one still there
    [MyRouteStr] = keylist:fetch(route, remove_route_matching_me(
					  keylist:set("Route", [MyRouteStr, MyRouteStr], ReqHeader10)
					 )),

    autotest:mark(?LINE, "remove_route_matching_me/1 - 5"),
    %% Test Route matching on my IP address, plus one more Route
    ["<sip:example.org>"] = keylist:fetch(route,
					  remove_route_matching_me(
					    keylist:set("Route", [MyRouteStr2, "<sip:example.org>"], ReqHeader10)
					   )),

    autotest:mark(?LINE, "remove_route_matching_me/1 - 6"),
    %% Test Route matching on my IP address, plus one more Route
    ["<sip:example.org>"] = keylist:fetch(route,
					  remove_route_matching_me(
					    keylist:set("Route", [MyRouteStr2, "<sip:example.org>"], ReqHeader10)
					   )),

    autotest:mark(?LINE, "remove_route_matching_me/1 - 7"),
    %% Test Route with my hostname, but wrong port
    [MyRouteStr3] = keylist:fetch(route,
				  remove_route_matching_me(
				    keylist:set("Route", [MyRouteStr3], ReqHeader10)
				   )),

    %% test remove_maddr_matching_me(URI, Origin)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 1"),
    MaddrURL1 = sipurl:parse("sip:ft@example.org:5060;transport=tcp"),
    %% test no maddr
    MaddrURL1 = remove_maddr_matching_me(MaddrURL1, #siporigin{}),

    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 2"),
    MaddrURL2 = sipurl:parse("sip:ft@example.org;maddr=testing"),
    %% test with maddr not matching me
    MaddrURL2 = remove_maddr_matching_me(MaddrURL2, #siporigin{}),

    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 3"),
    MaddrURL3 = sipurl:parse("sip:ft@example.org:1234;maddr=" ++ MyHostname),
    %% test with maddr matching me, but not port
    MaddrURL3 = remove_maddr_matching_me(MaddrURL3, #siporigin{}),

    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 4"),
    MaddrURL4 = sipurl:parse("sip:ft@example.org;transport=udp;maddr=" ++ MyHostname),
    %% test with maddr matching me, port matching me (by default) but not transport
    MaddrURL4 = remove_maddr_matching_me(MaddrURL4, #siporigin{proto = tcp}),

    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 5"),
    MaddrURL5 = sipurl:parse("sip:ft@example.org;maddr=" ++ MyHostname),
    MaddrURL5_expected = sipurl:parse("sip:ft@example.org"),
    %% test with maddr matching me, port matching me (by default) and no transport
    MaddrURL5_expected = remove_maddr_matching_me(MaddrURL5, #siporigin{proto = udp}),

    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 6"),
    MaddrURL6 = sipurl:parse("sips:ft@example.org:" ++ integer_to_list(SipsPort) ++ ";maddr=" ++ MyHostname),
    MaddrURL6_expected = sipurl:parse("sips:ft@example.org:" ++ integer_to_list(SipsPort)),
    %% test with maddr matching me, port matching me (explicitly) and no transport
    MaddrURL6_expected = remove_maddr_matching_me(MaddrURL6, #siporigin{proto = udp}),

    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 7"),
    MaddrURL7 = sipurl:parse("sip:ft@example.org;transport=tcp;maddr=" ++ MyHostname),
    MaddrURL7_expected = sipurl:parse("sip:ft@example.org"),
    %% test with maddr matching me, port matching me (by default) and matching transport
    MaddrURL7_expected = remove_maddr_matching_me(MaddrURL7, #siporigin{proto = tcp}),

    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 8"),
    MaddrURL8 = sipurl:parse("sip:ft@example.org;transport=udp;maddr=" ++ MyHostname),
    MaddrURL8_expected = sipurl:parse("sip:ft@example.org"),
    %% test with maddr matching me, port matching me (by default) and matching transport
    MaddrURL8_expected = remove_maddr_matching_me(MaddrURL8, #siporigin{proto = udp6}),

    autotest:mark(?LINE, "remove_maddr_matching_me/2 - 9"),
    MaddrURL9 = sipurl:parse("sip:ft@example.org;transport=tls;maddr=" ++ MyHostname),
    MaddrURL9_expected = sipurl:parse("sip:ft@example.org"),
    %% test with maddr matching me, port matching me (by default) and matching transport
    MaddrURL9_expected = remove_maddr_matching_me(MaddrURL9, #siporigin{proto = tls6}),


    %% test received_from_strict_router(URI, Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "received_from_strict_router/2 - 0"),
    StrictHeader1 = keylist:from_list([{"Route", ["sip:user@example.org"]}]),

    autotest:mark(?LINE, "received_from_strict_router/2 - 1"),
    %% test with username part of URI, should always return false
    false = received_from_strict_router(sipurl:parse("sip:ft@example.org"), StrictHeader1),

    autotest:mark(?LINE, "received_from_strict_router/2 - 2"),
    %% This is an URL that we could actually have put in a Record-Route header
    RRURL1 = "sip:" ++ MyHostname ++ ":" ++ integer_to_list(SipPort) ++ ";maddr=" ++ siphost:myip(),
    true = received_from_strict_router(sipurl:parse(RRURL1), StrictHeader1),

    autotest:mark(?LINE, "received_from_strict_router/2 - 3"),
    %% This is the same URL, but without the maddr parameter. Some stacks strip RR parameters
    %% so unfortunately we must allow this one too.
    RRURL2 = "sip:" ++ MyHostname ++ ":" ++ integer_to_list(SipPort),
    false = received_from_strict_router(sipurl:parse(RRURL1), keylist:from_list([])),

    autotest:mark(?LINE, "received_from_strict_router/2 - 4"),
    %% RRURL2 is a matching URL but without the maddr parameter. As some stacks strip the
    %% parameters, this one should also work even though it wouldn't by the RFC.
    true = received_from_strict_router(sipurl:parse(RRURL2), StrictHeader1),

    autotest:mark(?LINE, "received_from_strict_router/2 - 5"),
    %% This is an URL that we could actually have put in a Record-Route header, but with the WRONG maddr.
    %% It still matches us though, since we only check on hostname and port.
    RRURL3 = "sip:" ++ MyHostname ++ ":" ++ integer_to_list(SipPort) ++ ";maddr=192.0.2.123",
    true = received_from_strict_router(sipurl:parse(RRURL3), StrictHeader1),

    autotest:mark(?LINE, "received_from_strict_router/2 - 6"),
    %% This is an URL that we could actually have put in a Record-Route header, but without the port
    %% which we would have put in there. Unfortunately, some stacks strip the port if it is the default
    %% port for a protocol (which SipPort should be), so we must allow this too
    RRURL4 = "sip:" ++ MyHostname ++ ";maddr=" ++ siphost:myip(),
    true = received_from_strict_router(sipurl:parse(RRURL4), StrictHeader1),


    %% test check_for_loop(Header, URI, Origin)
    %% indirectly test via_indicates_loop(LoopCookie, CmpVia, ViaList)
    %%--------------------------------------------------------------------
    Me = lists:concat([MyHostname, ":", SipPort]),

    autotest:mark(?LINE, "check_for_loop/2 - 1"),
    LoopHeader1 = keylist:set("Via", ["SIP/2.0/TLS example.org:1234",
				      "SIP/2.0/TCP example.org:2222"
				     ], ReqHeader10),
    LoopURI1 = sipurl:parse("sip:user@example.org"),
    LoopOrigin1 = #siporigin{proto=tcp, addr="192.0.2.123", port=4321},
    %% No loop. No Via matches me at all.
    ok = (catch check_for_loop(LoopHeader1, LoopURI1, LoopOrigin1)),

    autotest:mark(?LINE, "check_for_loop/2 - 2"),
    LoopHeader2 = keylist:set("Via", ["SIP/2.0/TLS example.org:1234",
				      "SIP/2.0/TCP " ++ Me
				     ], ReqHeader10),
    %% No loop. One of the Vias match me but has no branch parameter. Maybe this should
    %% be considered cause to reject the request, but we currently don't.
    ok = (catch check_for_loop(LoopHeader2, LoopURI1, LoopOrigin1)),

    autotest:mark(?LINE, "check_for_loop/2 - 3"),
    LoopHeader3 = keylist:set("Via", ["SIP/2.0/TLS example.org:1234",
				      "SIP/2.0/TCP " ++ Me ++ ";branch=noloop"
				     ], ReqHeader10),
    %% No loop. The Via that matches me clearly does not have a matching loop cookie.
    ok = (catch check_for_loop(LoopHeader3, LoopURI1, LoopOrigin1)),

    autotest:mark(?LINE, "check_for_loop/2 - 4"),
    LoopHeader4 = keylist:set("Via", ["SIP/2.0/TLS example.org:1234",
				      "SIP/2.0/TCP " ++ Me ++ ";branch=z9hG4bK-yxa-foo-oZo99DPyZILaWA73FVsm7Dw"
				     ], ReqHeader10),
    %% Loop. Also check that we don't get fooled if someone changes the casing of our branch.
    {sipparseerror, request, _Keylst, 482, _Reason} = (catch check_for_loop(LoopHeader4, LoopURI1, LoopOrigin1)),

    autotest:mark(?LINE, "check_for_loop/2 - 5"),
    LoopHeader5 = keylist:set("Via", ["SIP/2.0/TLS example.org:1234",
				      "SIP/2.0/UDP " ++ Me ++ ";branch=z9hG4bK-yxa-foo-o4711foo",
				      "SIP/2.0/TLS example.com:5090",
				      "SIP/2.0/TLS " ++ Me ++ ";received=192.0.2.1;branch="
				      "z9hG4bK-yxa-foo-oZo99DPyZILaWA73FVsm7Dw",
				      "SIP/2.0/UDP phone.example.net;received=192.0.2.254"
				     ], ReqHeader10),
    %% Loop, although a wee bit harder to spot since there is one Via matching us (UDP) that does NOT
    %% indicate a loop, and the one that does (TLS) has some unknown-to-us IP address in a received parameter.
    {sipparseerror, request, _Keylist, 482, _Reason} =
	(catch check_for_loop(LoopHeader5, LoopURI1, LoopOrigin1)),


    %% test origin2str(Origin)
    %%--------------------------------------------------------------------
    Origin2Str1 = #siporigin{proto=tcp, addr="192.0.2.123", port=10},

    autotest:mark(?LINE, "origin2str/1 - 1"),
    %% straight forward
    "tcp:192.0.2.123:10" = origin2str(Origin2Str1),


    %% test make_logstr(Request, Origin)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "make_logstr/2 - 1"),
    %% create records
    LogStrH1 = keylist:from_list([
				  {"From",	["<sip:test@it.su.se>;tag=f-123"]},
				  {"To",	["<sip:test@it.su.se>;tag=t-123"]}
				 ]),
    LogStrReq1 = #request{method="INVITE", uri=sipurl:parse("sip:ft@example.org"),
			  header=LogStrH1, body=EmptyBody},

    autotest:mark(?LINE, "make_logstr/2 - 2"),
    %% straight forward
    LogStrResult1 = "INVITE sip:ft@example.org [client=tcp:192.0.2.123:10, from=<sip:test@it.su.se>, to=<sip:test@it.su.se>]",
    LogStrResult1 = make_logstr(LogStrReq1, Origin2Str1),


    %% test check_packet(Request, Origin)
    %% several parts of this is tested separately, focus on the CSeq checks
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_packet/2 request - 1"),
    %% create records
    CPacketH1 = keylist:from_list([
				   {"From",	["<sip:test@it.su.se>;tag=f-123"]},
				   {"To",	["<sip:test@it.su.se>;tag=t-123"]},
				   {"CSeq",	["1 INVITE"]}
				  ]),
    CPacketU1 = sipurl:parse("sip:ft@example.net"),
    CPacketR1 = #request{method="INVITE", uri=CPacketU1, header=CPacketH1, body=EmptyBody},

    autotest:mark(?LINE, "check_packet/2 request - 2"),
    %% valid CSeq
    ok = check_packet(CPacketR1, Origin2Str1),

    autotest:mark(?LINE, "check_packet/2 request - 3"),
    CPacketH2 = keylist:set("CSeq", ["foo"], CPacketH1),
    CPacketR2 = CPacketR1#request{header=CPacketH2},
    %% completely invalid CSeq
    {sipparseerror, request, _, 400, "Invalid CSeq"} = (catch check_packet(CPacketR2, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 request - 4"),
    CPacketH3 = keylist:set("CSeq", ["A INVITE"], CPacketH1),
    CPacketR3 = CPacketR1#request{header=CPacketH3},
    %% non-integer CSeq number
    {sipparseerror, request, _, 400, "CSeq number 'A' is not an integer"} =
							(catch check_packet(CPacketR3, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 request - 5"),
    CPacketH4 = keylist:set("CSeq", ["1 NOMATCH"], CPacketH1),
    CPacketR4 = CPacketR1#request{header=CPacketH4},
    %% wrong method in CSeq
    {sipparseerror, request, _, 400, "CSeq Method NOMATCH does not match request Method INVITE"} =
							 (catch check_packet(CPacketR4, Origin2Str1)),


    %% test check_packet(Response, Origin)
    %%--------------------------------------------------------------------

    autotest:mark(?LINE, "check_packet/2 response - 1"),
    %% create records
    CPacketRH1 = keylist:from_list([
				    {"From",	["<sip:test@it.su.se>;tag=f-123"]},
				    {"To",	["<sip:test@it.su.se>;tag=t-123"]}
				   ]),
    CPacketRR1 = #response{status=100, reason="Trying", header=CPacketRH1, body=""},

    autotest:mark(?LINE, "check_packet/2 response - 1"),
    %% test valid case
    ok = check_packet(CPacketRR1, Origin2Str1),

    autotest:mark(?LINE, "check_packet/2 response - 2.1"),
    %% test invalid status code #1
    {sipparseerror, response, _, 400, "Response code out of bounds"} =
	(catch check_packet(CPacketRR1#response{status=99}, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 response - 2.2"),
    %% test invalid status code #2
    {sipparseerror, response, _, 400, "Response code out of bounds"} =
	(catch check_packet(CPacketRR1#response{status=-1}, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 response - 2.3"),
    %% test invalid status code #3
    {sipparseerror, response, _, 400, "Response code out of bounds"} =
	(catch check_packet(CPacketRR1#response{status=700}, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 response - 2.4"),
    %% test invalid status code #4
    {sipparseerror, response, _, 400, "Response code out of bounds"} =
	(catch check_packet(CPacketRR1#response{status=4294967301}, Origin2Str1)),

    WWWURL = "http:/www.stacken.kth.se/projekt/yxa/",

    autotest:mark(?LINE, "check_packet/2 response - 3"),
    %% Test strange From:. draft-ietf-sipping-torture-tests-04 argues that a
    %% proxy should not break on this, unless it is really required to be able
    %% to understand the From:. That is a good point, but our current behavior
    %% is to break on To: and From: that we don't understand - so we test this.
    CPacketRH3 = keylist:set("From", [WWWURL], CPacketRH1),
    {sipparseerror, response, CPacketRH3, 400, "Invalid From: header"} =
	(catch check_packet(CPacketRR1#response{header=CPacketRH3}, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 response - 4"),
    %% Test response with more than one From: header
    CPacketRH4 = keylist:set("From", ["sip:first@example.com", "sip:second@example.com"], CPacketRH1),
    {sipparseerror, response, CPacketRH4, 400, "Missing or invalid From: header"} =
	(catch check_packet(CPacketRR1#response{header=CPacketRH4}, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 response - 5"),
    %% Test response with no From: header
    CPacketRH5 = keylist:delete("From", CPacketRH1),
    {sipparseerror, response, CPacketRH5, 400, "Missing or invalid From: header"} =
	(catch check_packet(CPacketRR1#response{header=CPacketRH5}, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 response - 6"),
    %% Test response with totally invalid From: header
    CPacketRH6 = keylist:set("From", ["1"], CPacketRH1),
    {sipparseerror, response, CPacketRH6, 400, "Invalid From: header"} =
	(catch check_packet(CPacketRR1#response{header=CPacketRH6}, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 response - 7"),
    %% Test response with parsable From: but URL without closing ">"
    CPacketRH7 = keylist:set("From", ["<sip:test@example.org"], CPacketRH1),
    {sipparseerror, response, CPacketRH7, 400, "Invalid From: header"} =
	(catch check_packet(CPacketRR1#response{header=CPacketRH7}, Origin2Str1)),

    autotest:mark(?LINE, "check_packet/2 response - 8"),
    %% Test strange To:. I think it is enough to test just one To: - that should suffice
    %% to tell that To: is checked the same way as From: (see tests above).
    CPacketRH8 = keylist:set("To", [WWWURL], CPacketRH1),
    {sipparseerror, response, CPacketRH8, 400, "Invalid To: header"} =
	(catch check_packet(CPacketRR1#response{header=CPacketRH8}, Origin2Str1)),


    %% test process_parsed_packet(Request, Origin)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "process_parsed_packet/2 - 1"),
    %% create records
    PPPH1 = keylist:from_list([
			       {"Via",		["SIP/2.0/TLS example.org:1234;rport",
						 "SIP/2.0/TCP foo"]},
			       {"From",		["<sip:test@it.su.se>;tag=f-123"]},
			       {"To",		["<sip:test@it.su.se>;tag=t-123"]},
			       {"Route",	["sip:192.0.2.222"]},
			       {"CSeq",		["1 INVITE"]}
			      ]),
    PPPRequest1 = LogStrReq1#request{header=PPPH1},

    autotest:mark(?LINE, "process_parsed_packet/2 - 2.1"),
    %% test that received= and rport= is set correctly in top Via
    {ok, PPPRequest1_res, #yxa_ctx{logstr = LogStrResult1}} = process_parsed_packet(PPPRequest1, Origin2Str1),
    autotest:mark(?LINE, "process_parsed_packet/2 - 2.2"),
    %% check result
    ["SIP/2.0/TLS example.org:1234;received=192.0.2.123;rport=10", "SIP/2.0/TCP foo"] =
	keylist:fetch('via', PPPRequest1_res#request.header),

    autotest:mark(?LINE, "process_parsed_packet/2 - 3"),
    %% check that process_parsed_packet actually verifys packet using check_packet
    PPPH3 = keylist:set("CSeq", ["bogus-cseq"], PPPH1),
    PPPRequest3 = PPPRequest1#request{header=PPPH3},
    {sipparseerror, request, _, 400, "Invalid CSeq"} = (catch process_parsed_packet(PPPRequest3, Origin2Str1)),

    autotest:mark(?LINE, "process_parsed_packet/2 - 4.1"),
    %% This is an URL that we could actually have put in a Record-Route header
    PPPURL4_str = "sip:" ++ MyHostname ++ ":" ++ integer_to_list(SipPort) ++ ";maddr=" ++ siphost:myip(),
    PPPURL4 = sipurl:parse(PPPURL4_str),
    %% check Request-URI is popped correctly from Route-header if received from strict router
    PPPH4_urlstr = "sip:ft@example.org;foo=bar",
    PPPH4_url = sipurl:parse(PPPH4_urlstr),
    PPPH4_logstr = "INVITE sip:ft@example.org;foo=bar [client=tcp:192.0.2.123:10, "
	"from=<sip:test@it.su.se>, to=<sip:test@it.su.se>]",
    PPPH4 = keylist:append({"Route", ["<" ++ PPPH4_urlstr ++ ">"]}, PPPH1),
    PPPRequest4 = PPPRequest1#request{uri=PPPURL4, header=PPPH4},
    {ok, PPPRequest4_res, #yxa_ctx{logstr = PPPH4_logstr}} = process_parsed_packet(PPPRequest4, Origin2Str1),
    autotest:mark(?LINE, "process_parsed_packet/2 - 4.2"),
    %% check resulting URI
    PPPH4_url = PPPRequest4_res#request.uri,
    autotest:mark(?LINE, "process_parsed_packet/2 - 4.3"),
    %% check resulting Route-header.
    ["sip:192.0.2.222"] = keylist:fetch('route', PPPRequest4_res#request.header),

    autotest:mark(?LINE, "process_parsed_packet/2 - 5.1"),
    %% now check that Route is empty if it contained only the real Request-URI
    PPPH5 = keylist:set("Route", ["<" ++ PPPH4_urlstr ++ ">"], PPPH1),
    PPPRequest5 = PPPRequest1#request{uri=PPPURL4, header=PPPH5},
    {ok, PPPRequest5_res, #yxa_ctx{logstr = PPPH4_logstr}} = process_parsed_packet(PPPRequest5, Origin2Str1),
    %% check resulting URI
    PPPH4_url = PPPRequest5_res#request.uri,
    %% check resulting Route-header (should be empty)
    [] = keylist:fetch('route', PPPRequest5_res#request.header),

    autotest:mark(?LINE, "process_parsed_packet/2 - 6"),
    %% test with non-strict-router Request-URI (should remain the same)
    %% and Route matching me - just to make sure that process_parsed_packet()
    %% includes remove_route_matching_me()
    PPPH6 = keylist:set("Route", ["<" ++ PPPURL4_str ++ ">"], PPPH1),
    PPPRequest6 = PPPRequest1#request{uri=PPPH4_url, header=PPPH6},
    {ok, PPPRequest6_res, #yxa_ctx{logstr = PPPH4_logstr}} = process_parsed_packet(PPPRequest6, Origin2Str1),
    %% check resulting URI, should be the same as input URI
    %% the compiler (R9C-0..R10B-2) barks at this :
    %%     PPPRequest6#request.uri = PPPRequest6_res#request.uri,
    %% (probably because the pre-processor turns them both into calls to element())
    %% so we do it like this instead :
    PPPRequest6_check = PPPRequest6#request.uri,
    PPPRequest6_check = PPPRequest6_res#request.uri,
    %% check resulting Route-header (should be empty)
    [] = keylist:fetch('route', PPPRequest6_res#request.header),


    %% test check_supported_uri_scheme(URI, Header)
    %%--------------------------------------------------------------------
    URISchemeURL1 = sipurl:parse("sip:ft@example.org"),
    URISchemeHeader = keylist:from_list([]),

    autotest:mark(?LINE, "check_supported_uri_scheme/2 - 1"),
    %% valid test
    true = check_supported_uri_scheme(URISchemeURL1, URISchemeHeader),

    autotest:mark(?LINE, "check_supported_uri_scheme/2 - 2"),
    URISchemeURL2 = sipurl:parse("bogus:ft@example.org"),
    %% URL was unparseable
    {sipparseerror, request, URISchemeHeader, 416, "Unsupported URI Scheme (bogus:)"} =
	(catch check_supported_uri_scheme(URISchemeURL2, URISchemeHeader)),

    autotest:mark(?LINE, "check_supported_uri_scheme/2 - 3"),
    URISchemeURL3 = sipurl:parse("SiP:ft@454.247.207.112"),
    %% URL was unparseable, but it wasn't the URI scheme that we could not understand
    {sipparseerror, request, URISchemeHeader, 400, "Unparsable Request-URI"} =
	(catch check_supported_uri_scheme(URISchemeURL3, URISchemeHeader)),

    ok.
