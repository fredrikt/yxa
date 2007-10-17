%%%-------------------------------------------------------------------
%%% File    : appserver.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      SIP application server. Handles forking and other more
%%%           advanced message routing for our users.
%%%
%%% @since    09 Dec 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------
-module(appserver).

-behaviour(yxa_app).

%%--------------------------------------------------------------------
%%% Standard YXA SIP-application callback functions
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/2,
	 response/2,
	 terminate/1
	]).

-export([test/0]).

%%--------------------------------------------------------------------
%%% Application specific exports, exported for CPL subsystem
%%--------------------------------------------------------------------
-export([locations_to_actions/2,
	 location_to_call_action/2
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipproxy.hrl").
-include("siprecords.hrl").
-include("sipsocket.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(APPSERVER_GLUE_TIMEOUT, 1200 * 1000).
-define(SIPPIPE_TIMEOUT, 900).


%%====================================================================
%% Behaviour functions
%% Standard YXA SIP-application callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> #yxa_app_init{}
%%
%% @doc     YXA applications must export an init/0 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    Tables = [user, numbers, phone, cpl_script_graph, gruu],
    #yxa_app_init{mnesia_tables = Tables}.



%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%% @doc     YXA applications must export a request/2 function.
%% @end
%%--------------------------------------------------------------------

%%
%% REGISTER
%%
request(#request{method = "REGISTER"}, YxaCtx) when is_record(YxaCtx, yxa_ctx) ->
    LogStr = YxaCtx#yxa_ctx.logstr,
    logger:log(normal, "Appserver: ~s Method not applicable here -> 403 Forbidden", [LogStr]),
    transactionlayer:send_response_handler(YxaCtx#yxa_ctx.thandler, 403, "Forbidden"),
    ok;

%%
%% ACK
%%
request(#request{method = "ACK"} = Request, YxaCtx) when is_record(YxaCtx, yxa_ctx) ->
    LogStr = YxaCtx#yxa_ctx.logstr,
    case local:get_user_with_contact(Request#request.uri) of
	none ->
	    logger:log(normal, "Appserver: ~s -> Forwarding ACK statelessly (to unknown SIP user)",
		       [LogStr]),
	    transportlayer:stateless_proxy_ack("appserver", Request, YxaCtx);
	SIPuser ->
	    logger:log(normal, "Appserver: ~s -> Forwarding ACK statelessly (to SIP user ~p)",
		       [LogStr, SIPuser]),
	    transportlayer:stateless_proxy_ack("appserver", Request, YxaCtx)
    end,
    ok;

%%
%% CANCEL
%%
request(#request{method = "CANCEL"}, YxaCtx) when is_record(YxaCtx, yxa_ctx) ->
    LogStr = YxaCtx#yxa_ctx.logstr,
    logger:log(debug, "Appserver: ~s -> CANCEL not matching any existing transaction received, "
	       "answer 481 Call/Transaction Does Not Exist", [LogStr]),
    transactionlayer:send_response_handler(YxaCtx#yxa_ctx.thandler, 481, "Call/Transaction Does Not Exist"),
    ok;

%%
%% Anything but REGISTER, ACK and CANCEL
request(Request, YxaCtx) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    case local:is_request_to_this_proxy(Request) of
	true ->
	    siprequest:request_to_me(Request, YxaCtx, _ExtraHeaders = []);
	false ->
	    %% Ok, request was not for this proxy itself - now just make sure we are supposed to fork it.
	    {ShouldFork, NoForkReason, PipeDst} =
		case keylist:fetch('route', Request#request.header) of
		    [] ->
			URI = Request#request.uri,
			case url_param:find(URI#sipurl.param_pairs, "maddr") of
			    [] ->
				{true, "", none};
			    _ ->
				%% RFC3261 #16.5 (Determining Request Targets) says a proxy MUST
				%% use ONLY the maddr in the Request-URI as target, if set.
				ApproxMsgSize = siprequest:get_approximate_msgsize(Request),
				D = sipdst:url_to_dstlist(URI, ApproxMsgSize, URI),
				D2 = lists:map(fun(H) ->
						       sipdst:dst2str(H)
					       end, D),
				logger:log(debug, "Appserver: Turned Request-URI with maddr set (~s) "
					   "into dst-list : ~p", [sipurl:print(URI), D2]),
				{false, "maddr Request-URI parameter present", D}
			end;
		    _ ->
			%% Request has a Route header, this proxy probably added a Record-Route in a previous
			%% fork, and this request should just be proxyed - not forked.
			{false, "Route-header present", route}
		end,
	    case ShouldFork of
		true ->
		    create_session(Request, YxaCtx, true);
		false ->
		    %% XXX check credentials here - our appserver is currently an open relay!
		    #yxa_ctx{thandler = THandler,
			     logstr   = LogStr
			    } = YxaCtx,
		    logger:log(normal, "Appserver: ~s -> Not forking, just forwarding (~s)", [LogStr, NoForkReason]),
		    sippipe:start(THandler, none, Request, PipeDst, ?SIPPIPE_TIMEOUT)
	    end
    end,
    ok.


%%--------------------------------------------------------------------
%% @spec    (Response, YxaCtx) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Response = #response{}
%%            YxaCtx   = #yxa_ctx{}
%%
%% @doc     YXA applications must export a response/2 function.
%% @end
%%--------------------------------------------------------------------
response(Response, YxaCtx) when is_record(Response, response), is_record(YxaCtx, yxa_ctx) ->
    %% RFC 3261 16.7 says we MUST act like a stateless proxy when no
    %% transaction can be found
    #response{status = Status, reason = Reason} = Response,
    logger:log(normal, "appserver: Response to ~s: '~p ~s', no matching transaction - proxying statelessly",
	       [YxaCtx#yxa_ctx.logstr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response),
    ok.


%%--------------------------------------------------------------------
%% @spec    (Mode) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Mode = shutdown | graceful | atom()
%%
%% @doc     YXA applications must export a terminate/1 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Mode) when is_atom(Mode) ->
    ok.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, CPL) -> term()
%%
%%            Request = #request{}
%%            Origin  = #siporigin{}
%%            LogStr  = string()
%%            DoCPL   = true | false "do CPL or not"
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Request was not meant for this proxy itself - find out if
%%          this request is for one of our users, and if so find out
%%          what actions to perform for the request.
%% @end
%%--------------------------------------------------------------------
create_session(Request, YxaCtx, DoCPL) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    case get_actions(Request#request.uri, DoCPL) of
	nomatch ->
	    create_session_nomatch(Request, YxaCtx);
	{cpl, User, Graph} ->
	    create_session_cpl(Request, YxaCtx, User, Graph);
	{ok, Users, Actions, Surplus} ->
	    create_session_actions(Request, Users, Actions, Surplus)
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, Users, Actions, Surplus) -> term()
%%
%%            Request = #request{}
%%            Users   = [string()] "list of SIP usernames"
%%            Actions = [#sipproxy_action{}]
%%            Surplus = [#sipproxy_action{}] "surplus actions for user agents with multiple active bindings (draft-Outbound)"
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     The request was turned into a set of Actions (derived from
%%          it's URI matching a set of Users). Note : When we start
%%          the appserver_glue process, we should ideally not do a
%%          spawn but instead just execute it. Currently this is the
%%          way we do it though (to keep the code clean). Since this
%%          process is the parent of the server transaction, we must
%%          stay alive until the process that does the actual work
%%          exits - so we monitor it.
%% @end
%%--------------------------------------------------------------------
create_session_actions(Request, Users, Actions, Surplus) when is_record(Request, request), is_list(Users),
							      is_list(Actions), is_list(Surplus) ->
    logger:log(debug, "Appserver: User(s) ~p actions :~n~p", [Users, Actions]),
    {ok, Pid} = appserver_glue:start_link(Request, Actions, Surplus),
    MonitorRef = erlang:monitor(process, Pid),
    receive
	{'DOWN', MonitorRef, process, Pid, _Info} ->
	    ok;
	Msg ->
	    %% We don't exit/crash on this since it might be a late reply from a gen_sever:call() that
	    %% timed out. Since we have apparently chosen to ignore the timeout earlier, we don't
	    %% consider this an error.
	    logger:log(error, "Appserver: Received unknown signal after starting appserver_glue worker : ~p",
		       [Msg]),
	    create_session_actions(Request, Users, Actions, Surplus)
    after ?APPSERVER_GLUE_TIMEOUT ->
	    %% We should _really_ never get here, but just as an additional safeguard...
	    logger:log(error, "appserver: ERROR: the appserver_glue process I started (~p) never finished! Exiting.",
		       [Pid]),
	    erlang:error(appserver_glue_never_finished)
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) -> term()
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     No actions found for this request. Check if we should
%%          forward it anyways, or respond '404 Not Found'.
%% @end
%%--------------------------------------------------------------------
create_session_nomatch(Request, YxaCtx) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    %% Check if the Request-URI is the registered location of one of our users. If we added
    %% a Record-Route to an earlier request, this might be an in-dialog request destined
    %% for one of our users. Such in-dialog requests will have the users Contact (registered
    %% location hopefully) as Request-URI.
    #yxa_ctx{thandler = THandler,
	     logstr   = LogStr
	    } = YxaCtx,
    case local:get_user_with_contact(Request#request.uri) of
	none ->
	    logger:log(normal, "Appserver: ~s -> 404 Not Found (no actions, unknown user)",
		       [LogStr]),
	    transactionlayer:send_response_handler(THandler, 404, "Not Found");
	SIPuser when is_list(SIPuser) ->
	    logger:log(normal, "Appserver: ~s -> Not forking, just forwarding (no actions found, SIP user ~p)",
		       [LogStr, SIPuser]),
	    ApproxMsgSize = siprequest:get_approximate_msgsize(Request),
	    DstList = sipdst:url_to_dstlist(Request#request.uri, ApproxMsgSize, Request#request.uri),
	    sippipe:start(THandler, none, Request, DstList, ?SIPPIPE_TIMEOUT)
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, User, Graph) -> term()
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            User    = string() "SIP username of CPL script owner"
%%            Graph   = term() "CPL graph"
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     We found a CPL script that should be applied to this
%%          request (or perhaps have this request applied to it). Do
%%          that and handle any return values. Noteably handle a CPL
%%          return value of '{server_default_action}' by calling
%%          create_session(...) again, but this time with DoCPL set
%%          to 'false' to not end up here again.
%% @end
%%--------------------------------------------------------------------
create_session_cpl(Request, YxaCtx, User, Graph)
  when is_record(Request, request), is_record(YxaCtx, yxa_ctx), is_list(User) ->
    Res = interpret_cpl:process_cpl_script(Request, User, Graph, incoming),
    case Res of
	{server_default_action} ->
	    %% Loop back to create_session, but tell it to not do CPL again
	    create_session(Request, YxaCtx, false);
	ok ->
	    ok;
	Unknown ->
	    logger:log(error, "appserver: Unknown return value from process_cpl_script(...) user ~p : ~p",
		       [User, Unknown]),
	    throw({siperror, 500, "Server Internal Error"})
    end.

%%--------------------------------------------------------------------
%% @spec    (URI, DoCPL) ->
%%            {UserList, ActionsList} |
%%            {cpl, User, Graph}      |
%%            nomatch
%%
%%            URI   = #sipuri{}
%%            DoCPL = true | false "do CPL or not"
%%
%%            UserList    = [string()] "SIP usernames"
%%            ActionsList = [#sipproxy_action{}]
%%
%% @doc     Find the SIP user(s) for a URI and make a list() of
%%          sipproxy_action to take for a request destined for that
%%          user(s).
%% @end
%%--------------------------------------------------------------------
get_actions(URI, DoCPL) when is_record(URI, sipurl) ->
    LookupURL = sipurl:set([{pass, none}, {port, none}, {param, []}], URI),
    case local:get_users_for_url(LookupURL) of
	nomatch ->
	    nomatch;
	Users when is_list(Users) ->
	    logger:log(debug, "Appserver: Found user(s) ~p for URI ~s", [Users, sipurl:print(LookupURL)]),
	    get_actions_users(Users, URI#sipurl.proto, DoCPL)
    end.

%% part of get_actions() - single user, look for a CPL script
get_actions_users([User], Proto, true) when is_list(User), is_list(Proto) ->
    case local:get_cpl_for_user(User) of
	{ok, Graph} ->
	    {cpl, User, Graph};
	nomatch ->
	    get_actions_users2([User], Proto)
    end;
%% part of get_actions() - more than one user, or DoCPL == false
get_actions_users(Users, Proto, _DoCPL) when is_list(Users), is_list(Proto) ->
    get_actions_users2(Users, Proto).

%% part of get_actions(), more than one user or DoCPL was false
get_actions_users2(Users, Proto) when is_list(Users), is_list(Proto) ->
    case fetch_actions_for_users(Users, Proto) of
	{ok, [], _Surplus} -> nomatch;
	{ok, Actions, Surplus} when is_list(Actions) ->
	    {ok, Timeout} = yxa_config:get_env(appserver_call_timeout),
	    WaitAction = #sipproxy_action{action  = wait,
					  timeout = Timeout
					 },
	    NewActions = Actions ++ [WaitAction],
	    {ok, Users, NewActions, Surplus}
    end.

%%--------------------------------------------------------------------
%% @spec    (Users, Proto) ->
%%            {ok, Actions, Surplus}
%%
%%            Users = [string()] "list of SIP usernames"
%%            Proto = string() "OrigURI proto (\"sips\" | \"sip\" | ...)"
%%
%%            Actions = [#sipproxy_action{}]
%%            Surplus = [#sipproxy_action{}] "extra contacts for instances with more than one location binding (draft-Outbound)"
%%
%% @doc     Construct a list of sipproxy_action record()s for a list
%%          of users, based on the contents of the location database
%%          and the KTH-only 'forwards' database. Just ignore the
%%          'forwards' part.
%% @end
%%--------------------------------------------------------------------
fetch_actions_for_users(Users, Proto) ->
    {ok, Actions, Surplus} = fetch_users_locations_as_actions(Users, Proto),
    NewActions =
	case local:get_forwards_for_users(Users) of
	    nomatch ->
		Actions;
	    [] ->
		Actions;
	    Forwards when is_list(Forwards) ->
		%% Append forwards found to Actions
		forward_call_actions(Forwards, Actions, Proto)
	end,
    {ok, NewActions, Surplus}.

%% part of fetch_actions_for_users/1
fetch_users_locations_as_actions(Users, Proto) ->
    URL = sipurl:new([{proto, Proto}]),
    Locations = local:lookupuser_locations(Users, URL),
    locations_to_actions(Locations).

%%--------------------------------------------------------------------
%% @spec    (Locations) ->
%%            {ok, Actions, Surplus}
%%
%%            Locations = [Loc]
%%            Loc       = #siplocationdb_e{} |
%%                        {URL, Timeout}     |
%%                        {wait, Timeout}
%%            URL       = #sipurl{}
%%
%%            Actions = [#sipproxy_action{}]
%%            Surplus = [#sipproxy_action{}] "extra contacts for instances with more than one location binding (draft-Outbound)"
%%
%% @doc     Turn a list of location database entrys/pseudo-actions
%%          into a list of sipproxy_action record()s.
%% @equiv    locations_to_actions(Locations, CallTimeout)
%% @private
%% @end
%%--------------------------------------------------------------------
locations_to_actions(L) when is_list(L) ->
    {ok, CallTimeout} = yxa_config:get_env(appserver_call_timeout),
    locations_to_actions2(L, CallTimeout, [], []).

%%--------------------------------------------------------------------
%% @spec    (Locations, Timeout) ->
%%            {ok, Actions, Surplus}
%%
%%            Locations = [Loc]
%%            Loc       = #siplocationdb_e{} |
%%                        {URL, Timeout}     |
%%                        {wait, Timeout}
%%            URL       = #sipurl{}
%%            Timeout   = integer()
%%
%%            Actions = [#sipproxy_action{}]
%%            Surplus = [#sipproxy_action{}] "extra contacts for instances with more than one location binding (draft-Outbound)"
%%
%% @doc     Turn a list of location database entrys/pseudo-actions
%%          into a list of sipproxy_action record()s.
%%          NOTE : Exported only for the CPL subsystem.
%% @private
%% @end
%%--------------------------------------------------------------------
locations_to_actions(L, Timeout) when is_list(L), is_integer(Timeout) ->
    %% Exported for CPL subsystem
    locations_to_actions2(L, Timeout, [], []).

locations_to_actions2([], _CallTimeout, Res, Surplus) ->
    {ok, lists:reverse(Res), Surplus};

locations_to_actions2([H | T] = In, CallTimeout, Res, Surplus) when is_record(H, siplocationdb_e) ->
    case H#siplocationdb_e.instance of
	[] ->
	    CallAction = location_to_call_action(H, CallTimeout),
	    locations_to_actions2(T, CallTimeout, [CallAction | Res], Surplus);
	Instance when is_list(Instance) ->
	    %% draft-Outbound says we "MUST NOT populate the target set with more than one
	    %% contact with the same AOR and instance-id at a time.". AOR translates to SipUser
	    %% in YXA.
	    [First | Rest] = Group = get_locations_with_instance(Instance, H#siplocationdb_e.sipuser, In),
	    CallAction = location_to_call_action(First, CallTimeout),
	    MoreSurplus = [location_to_call_action(E, CallTimeout) || E <- Rest],
	    %% Remove all entrys for this sipuser and instance from the ones we have left to process
	    NewTail = T -- Group,
	    %% put action made out of First in Res, all locations matching Firsts user and instance
	    %% into our Surplus list and recurse upon the NewTail we created
	    locations_to_actions2(NewTail, CallTimeout, [CallAction | Res], Surplus ++ MoreSurplus)
    end;

locations_to_actions2([{URL, Timeout} | T], CallTimeout, Res, Surplus) when is_record(URL, sipurl),
									    is_integer(Timeout) ->
    CallAction = #sipproxy_action{action  = call,
				  requri  = URL,
				  timeout = Timeout
				 },
    locations_to_actions2(T, CallTimeout, [CallAction | Res], Surplus);

locations_to_actions2([{wait, Timeout} | T], CallTimeout, Res, Surplus) ->
    WaitAction = #sipproxy_action{action  = wait,
				  timeout = Timeout
				 },
    locations_to_actions2(T, CallTimeout, [WaitAction | Res], Surplus).

%%--------------------------------------------------------------------
%% @spec    (H, Timeout) -> #sipproxy_action{}
%%
%%            H       = #sipurl{}
%%            Timeout = term()
%%
%% @doc     Create a sipproxy_action call record out of the input.
%%          NOTE : Exported only for the CPL subsystem.
%% @private
%% @end
%%--------------------------------------------------------------------
location_to_call_action(H, Timeout) ->
    URL = siplocation:to_url(H),
    %% RFC3327
    Path =
	case lists:keysearch(path, 1, H#siplocationdb_e.flags) of
	    {value, {path, Path1}} ->
		Path1;
	    false ->
		[]
	end,
    #sipproxy_action{action	= call,
		     requri	= URL,
		     path	= Path,
		     timeout	= Timeout,
		     user	= H#siplocationdb_e.sipuser,
		     instance	= H#siplocationdb_e.instance
		    }.

get_locations_with_instance(Instance, SipUser, In) ->
    %% filter out all entrys from In which have instance and user matching our parameters
    L = [E || E <- In, E#siplocationdb_e.instance == Instance, E#siplocationdb_e.sipuser == SipUser],
    siplocation:sort_most_recent_first(L).


%%--------------------------------------------------------------------
%% @spec    (ForwardList, Actions, Proto) ->
%%            NewActions
%%
%%            ForwardList = [#sipproxy_forward{}]
%%            Actions     = [#sipproxy_action{}]
%%            Proto       = string() "\"sips\" or other"
%%
%%            NewActions = [#sipproxy_action{}]
%%
%% @doc     This is something Magnus at KTH developed to suit their
%%          needs of forwarding calls. He hasn't to this date
%%          committed all of the implementation - so don't use it.
%%          Use CPL to accomplish forwards instead.
%% @end
%%--------------------------------------------------------------------
%% forward_call_actions/2 helps fetch_actions_for_users/1 make a list of
%% sipproxy_action out of a list of forwards, a timeout value and
%% "concurrent ringing or not" information
forward_call_actions([Fwd], Actions, Proto) when is_record(Fwd, sipproxy_forward), is_list(Actions),
						 is_list(Proto) ->
    #sipproxy_forward{user	= User,
		      forwards	= Forwards,
		      timeout	= Timeout,
		      localring	= Localring
		     } = Fwd,
    ForwardActions = forward_call_actions_create_calls(Forwards, Localring, User, Proto),
    case {Localring, Timeout} of
	{_, 0} ->
	    %% No timeout in between original actions and ForwardActions
	    lists:append(Actions, ForwardActions);
	{true, _} ->
	    WaitAction = #sipproxy_action{action  = wait,
					  timeout = Timeout},
	    lists:append(Actions, [WaitAction | ForwardActions]);
	{false, _} ->
	    WaitAction = #sipproxy_action{action  = wait,
					  timeout = Timeout},
	    lists:append(Actions, [WaitAction | ForwardActions])
    end.

%% part of forward_call_actions/3 - turn a list of forward URIs into a list of sipproxy_action call records
forward_call_actions_create_calls(Forwards, Localring, User, Proto) when is_list(Forwards), is_boolean(Localring),
									 is_list(Proto), is_list(User) ->
    {ok, FwdTimeout} = yxa_config:get_env(appserver_forward_timeout),
    forward_call_actions_create_calls2(Forwards, FwdTimeout, Localring, User, Proto, []).

forward_call_actions_create_calls2([H | T], Timeout, Localring, User, Proto, Res) when is_record(H, sipurl) ->
    %% Preserve SIPS protocol if original request was SIPS
    FwdURI = case {Proto, H#sipurl.proto} of
		 {"sips", "sip"} ->
		     %% Turn SIP URI into SIPS
		     H#sipurl{proto="sips"};
		 {"sips", _NonSip} ->
		     %% ignore this forward since original request was SIPS and
		     %% this forwards URI is not upgradeable to a SIPS URI
		     logger:log(debug, "Appserver: Ignoring forward ~p since original request was a "
				"SIPS request and I can't upgrade protocol ~p to SIPS",
				[sipurl:print(H), H#sipurl.proto]),
		     ignore;
		 {_Proto1, _Proto2} ->
		     H
	     end,
    case FwdURI of
	ignore ->
	    forward_call_actions_create_calls2(T, Timeout, Localring, User, Proto, Res);
	_ ->
	    This = #sipproxy_action{action  = call,
				    requri  = FwdURI,
				    timeout = Timeout,
				    user    = User},
	    forward_call_actions_create_calls2(T, Timeout, Localring, User, Proto, [This | Res])
    end;
forward_call_actions_create_calls2([], _Timeout, _Localring, _User, _Proto, Res) ->
    lists:reverse(Res).




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

    %% locations_to_actions2(L, CallTimeout, [], [])
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "locations_to_actions2/4 - 1"),
    %% test simple case
    LToActions_Locations1 = [#siplocationdb_e{address	= sipurl:parse("sip:ft@1.example.org"),
					      sipuser	= "user1",
					      flags	= [],
					      instance	= []
					     },
			     #siplocationdb_e{address	= sipurl:parse("sip:ft@2.example.org"),
					      sipuser	= "user2",
					      flags	= [],
					      instance	= []
					     }
			    ],
    LToActions_Actions1 = [#sipproxy_action{action	= call,
					    timeout	= 10,
					    requri	= sipurl:parse("sip:ft@1.example.org"),
					    path	= [],
					    user	= "user1",
					    instance	= []
					   },
			   #sipproxy_action{action	= call,
					    timeout	= 10,
					    requri	= sipurl:parse("sip:ft@2.example.org"),
					    path	= [],
					    user	= "user2",
					    instance	= []
					   }
			  ],

    {ok, LToActions_Actions1, []} = locations_to_actions2(LToActions_Locations1, 10, [], []),

    autotest:mark(?LINE, "locations_to_actions2/4 - 2"),
    %% test complex case with same instances and usernames
    LToActions_Locations2 = [#siplocationdb_e{address	= sipurl:parse("sip:ft@1.example.org"),
					      sipuser	= "user",
					      flags	= [{registration_time, 100}],
					      instance	= "<urn:test:match>"
					     },
			     #siplocationdb_e{address	= sipurl:parse("sip:ft@2.example.org"),
					      sipuser	= "user",
					      flags	= [{registration_time, 200},
							   {path, ["<test;lr>"]}
							  ],
					      instance	= "<urn:test:match>"
					     }
			    ],
    LToActions_Actions2 = [#sipproxy_action{action	= call,
					    timeout	= 10,
					    requri	= sipurl:parse("sip:ft@2.example.org"),
					    path	= ["<test;lr>"],
					    user	= "user",
					    instance	= "<urn:test:match>"
					   }
			  ],
    LToActions_Surplus2 = [#sipproxy_action{action	= call,
					    timeout	= 10,
					    requri	= sipurl:parse("sip:ft@1.example.org"),
					    path	= [],
					    user	= "user",
					    instance	= "<urn:test:match>"
					   }
			  ],
    {ok, LToActions_Actions2, LToActions_Surplus2} = locations_to_actions2(LToActions_Locations2, 10, [], []),

    autotest:mark(?LINE, "locations_to_actions2/4 - 3"),
    %% test with non-siplocationdb_e input
    LToActions_Locations3 = [#siplocationdb_e{address	= sipurl:parse("sip:ft@3.example.org"),
					      sipuser	= "user3",
					      flags	= [],
					      instance	= []
					     },
			     {wait, 29},
			     {sipurl:parse("sip:foo@4.example.org"), 31}
			    ],
    LToActions_Actions3 = [#sipproxy_action{action	= call,
					    timeout	= 30,
					    requri	= sipurl:parse("sip:ft@3.example.org"),
					    path	= [],
					    user	= "user3",
					    instance	= []
					   },
			   #sipproxy_action{action	= wait,
					    timeout	= 29
					   },
			   #sipproxy_action{action	= call,
					    timeout	= 31,
					    requri	= sipurl:parse("sip:foo@4.example.org")
					   }
			  ],

    {ok, LToActions_Actions3, []} = locations_to_actions2(LToActions_Locations3, 30, [], []),

    ok = appserver_test:test(),

    ok.
