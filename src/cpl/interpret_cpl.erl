%% This module implements the CPL graph logic and the user supplied
%% Backend module handles the interaction with other Yxa modules -
%% this design choice has been chosen for several reasons:
%%
%% * to keep the size of this module at a manageable level
%% * to separate interpreter implementation, from Yxa configurations,
%%   done due to cpl actions
%% * to allow testing of the interpreter without requiring a fully
%%   working backend (a test backend module can be used to better
%%   control test behavior).
%%
%% The interpreter code is written with the assumption that all
%% script correctness checking that can be done, is done when the
%% script is parsed by xml_parser.erl - this currently means that only
%% runtime errors and type checking of attribute values[1] are done in
%% the interpreter.
%%
%% [1]: this should really be done in the parser, but this would
%% result in excessive code bloat and duplicate code as the interpreter
%% already looks up and examines these values.
%%
%% Nodes are stored as #node_code record(), which contains the tag
%% type and a 'statements' field which contains the "code" of the node.
%%
%% The format of 'statement' is dependent on the tag type but is
%% usually something like {TagAttr, Dest} or {TagAttr, Conds} where
%% Conds = list() of {SubTagAttr, Dest} - see usage of
%% add_current_node(...) calls in xml_parse_graph.erl for more
%% details.
%%--------------------------------------------------------------------

-module(interpret_cpl).

%% -compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 process_cpl_script/4,
	 process_cpl_script/7,
	 get_node/2,
	 get_start_node/1,

	 test/0,
	 test27/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([

        ]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("cpl.hrl").
-include("sipproxy.hrl").
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(state, {
	  branch_base,    % string(), base of a unique id used in sipproxy:start/5
	  request,        % request record(), the incoming or outgoing sip request
	  user,           % string(), the owner of the script
	  graph,          % list() of {Index, node_code record()}, the yxa internal version of a cpl script
	  locations,      % list() of location record(), gathers all "phones" to call

	  %% atom(), a module name - this is the module that is used to call the rest
	  %% of yxa when running the script. Use a simplified version for testing
	  %% where output from the backend module can easily be controlled
	  backend,

	  %% Note: with the current definition of node ids, this path is also
	  %% encoded directly in the node id, so visited_nodes isn't strictly
	  %% necessary, but is still useful for debugging.

	  %% list() of Index elements useful to check if a script is processed
	  %% properly and also to check which nodes are parents, grandparents and so on ...
	  visited_nodes,

	  %%
	  sthandler,
	  %% Result from "{_, _, Response} = Backend:test_proxy_destinations(...)" call or 'none'
	  response = none

	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: process_cpl_script(Request, User, Graph, Direction)
%%           Request   = request record()
%%           User      = string(), the owner of the CPL script
%%           Graph     = term(), cpl script graph
%%           Direction = incoming | outgoing
%% Descrip.: This is just a simpler version of process_cpl_script/7,
%%           which adopts the server transaction handler for Request,
%%           figures out the BranchBase for you and take care of some
%%           general result actions. See documentation of the big
%%           process_cpl_script/7 for more information.
%% Returns : {server_default_action}                     |
%%           {proxy_call_to_location, Location}          |
%%           {proxy_or_redirect_to_locations, Locations} |
%%           ok                                          |
%%           throw({error, ...})                         |
%%           {error, Reason}
%%--------------------------------------------------------------------
process_cpl_script(Request, User, Graph, Direction) ->
    case transactionlayer:adopt_st_and_get_branchbase(Request) of
	{ok, STHandler, BranchBase} ->
	    Res = process_cpl_script(BranchBase, Request, User, Graph, interpret_backend, STHandler, Direction),
	    process_cpl_script_res(Res, STHandler);
	ignore ->
	    %% The server transaction has already been cancelled, completed or something. Just return.
	    ok;
	error ->
	    throw({error, failed_starting_cpl_processing})
    end.

%% part of process_cpl_script/4, process the results of the CPL script interpretation
process_cpl_script_res({redirect, Permanent, Locations}, STHandler) when Permanent == yes; Permanent == no,
									 is_list(Locations) ->
    {Status, Reason} = case Permanent of
			   yes -> {301, "Moved Permanently"};
			   no -> {302, "Moved Temporarily"}
		       end,
    %% Turn sipurl records in Locations into contact records
    RContacts = lists:foldl(fun(URL, Acc) when is_record(URL, sipurl) ->
				   This = contact:new(URL),
				   [This | Acc]
			   end, [], Locations),
    Contacts = lists:reverse(RContacts),
    ExtraHeaders = [{"Contact",
		     sipheader:contact_print(Contacts)}],
    transactionlayer:send_response_handler(STHandler, Status, Reason, ExtraHeaders);

process_cpl_script_res({reject, Status}, STHandler) when is_integer(Status) ->
    process_cpl_script_res({reject, Status, "CPL reject"}, STHandler);

process_cpl_script_res({reject, Status, Reason}, STHandler) when is_integer(Status), is_list(Reason) ->
    transactionlayer:send_response_handler(STHandler, Status, Reason);

process_cpl_script_res({proxy, Response}, STHandler) when is_record(Response, response) ->
    transactionlayer:send_proxy_response_handler(STHandler, Response);

process_cpl_script_res({proxy, {Status, Reason}}, STHandler) when is_integer(Status), is_list(Reason) ->
    transactionlayer:send_response_handler(STHandler, Status, Reason);

process_cpl_script_res({use_last_proxy_result, Response}, STHandler) ->
    transactionlayer:send_proxy_response_handler(STHandler, Response);

process_cpl_script_res(Unmatched, _STHandler) ->
    %% We just pass unmatched results along to the invoker of
    %% process_cpl_script/4
    Unmatched.

%%--------------------------------------------------------------------
%% Function: process_cpl_script(BranchBase, Request, User, Graph,
%%                              Backend, STHandler, Direction)
%%           BranchBase = string(), prefix to use when generating
%%                        branches for client transactions
%%           Request    = request record()
%%           User       = owner of cpl script
%%           Graph      = cpl script graph
%%           Backend    = callback module
%%           STHandler  =
%%           Direction  = incoming | outgoing
%% Descrip.: execute the the cpl script and return a {Action, ....}
%%           tuple to specify what to do with the request, the Action
%%           parameter indicates the action to do
%% Returns : There are a number of possible return values of the
%%           format {ExitType, ....}:
%%
%%           {redirect, Permanent, Locations}
%%           * a redirect cpl tag was processed, Permanent determines
%%             if this redirection should be made permanent. Locations
%%             lists the locations to redirect to, see RFC 3380
%%             chapter 6.2
%%
%%           {reject, Status, Reason}
%%           {reject, Status}
%%           * a reject cpl tag was processed, see RFC 3880 chapter
%%             6.3, Status is a numerical error code, Reason is a
%%             textual description - both are supplied by the cpl
%%             script. Reason is only supplied if the reason sub tag in the 
%%             reject cpl tag is not = "" 
%%
%%           {server_default_action}
%%           * process request the same way as it would be processed
%%             if no cpl script was run, see RFC 3880 chapter 10
%%
%%           {proxy_call_to_location, Location}
%%           * outgoing call, cpl call supplied no additional locations
%%             or signaling operations - see RFC 3880 chapter 10
%%
%%           {proxy_or_redirect_to_locations, Locations}
%%           * locations where accumulated with cpl script but no
%%             signaling operation where executed, so server decides
%%             what is to be done, see RFC 3880 chapter 10
%%
%%           {proxy, Response}
%%           * proxy tag was processed as the last cpl action,
%%             Response contains the results from this action [1]
%%
%%           {use_last_proxy_result, Response}
%%           * proxy tag was the last signaling action, but additional
%%             cpl tags where processed before terminating [1]
%%
%%           throw({error, ...})
%%
%%           Status      = integer()
%%           Response    = see #state.response
%%           Permanent   = yes | no
%%           Locations   = list() of sipurl record()
%%           Location    = sipurl record()
%%           Reason      = string()
%%           RejectAttrs = reject__attr record()
%%
%% [1]     : a {proxy ...} return is unusual in that it indicates that
%%           a proxying action has already been done - so there is no
%%           need to act on the proxy return value, as is needed for
%%           the other return values.
%% Note    : CPL processing should be done in a spawned process - cpl
%%           processing can take a long time (waiting for timeouts)
%%           and many scripts may needed to be run at the same time
%%--------------------------------------------------------------------
process_cpl_script(BranchBase, Request, User, Graph, Backend, STHandler, Direction) ->
    StartNodeIndex = get_start_node(Direction),
    State = #state{branch_base = BranchBase,
		   request = Request,
		   user = User,
		   graph = Graph,
		   locations = [],
		   backend = Backend,
		   visited_nodes = [],
		   sthandler = STHandler
		  },
    execute_node(StartNodeIndex, State).

%% get start index for graph
get_start_node(incoming) ->
    [1];
get_start_node(outgoing) ->
    [2].

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: execute_node(StartIndex, State)
%%           StartIndex = node index
%%           State      = state record()
%% Descrip.: this functions handles walking the node graph
%% Returns : see process_cpl_script/4
%% Note    : There are three ways a node can terminate:
%%           It can be a terminator node, terminator nodes are created
%%           by the parser, for single destination nodes, when the tag
%%           is empty.
%%           The second type are switches, nodes able to branch to
%%           several destinations, they have a "Cond" list of
%%           {Rule, Dest} elements, they terminate if Cond = [],
%%           i.e. when the cpl tag has no sub tags.
%%           The third type are nodes that always terminate and
%%           therefore have 'terminated' as destination, rather than
%%           another node.
%%--------------------------------------------------------------------
execute_node(Index, State) ->
    execute_node(Index, State, 0).

%% Count is used to count the number of visited nodes
execute_node(Index, State, Count) ->
    Node = get_node(State#state.graph, Index), % return node_code record()
    Type = Node#node_code.type,
    Code = Node#node_code.statements,
    %% execute node action / select destination node
    {NextId, NewState} = case Type of
			     terminator -> terminator(Code, State);
			     incoming -> incoming(Code, State);
			     outgoing -> outgoing(Code, State);
			     'address-switch' -> 'address-switch'(Code, State);
			     'language-switch' -> 'language-switch'(Code, State);
			     'priority-switch' -> 'priority-switch'(Code, State);
			     'string-switch' -> 'string-switch'(Code, State);
			     'time-switch' -> 'time-switch'(Code, State);
			     location -> location(Code, State);
			     lookup -> lookup(Code, State);
			     'remove-location' -> 'remove-location'(Code, State);
			     sub -> sub(Code, State);
			     log -> log(Code, State);
			     mail -> mail(Code, State);
			     proxy -> proxy(Code, Count, State);
			     redirect -> redirect(Code, State);
			     reject -> reject(Code, State)
			 end,
    %% add this node to the visited_nodes list
    NewState2 = add_node_to_visited(NewState, Index),
    case NextId of
	terminated ->
	    finish_execution(NewState2);
	{terminated, redirect, Permanent} ->
	    finish_redirect(Permanent, NewState2);
	{terminated, reject, RejectAttrs} ->
	    finish_reject(RejectAttrs, NewState2);
	{terminated, proxy} ->
	    finish_proxy(NewState2);
	_ ->
	    execute_node(NextId, NewState2, Count + 1)
    end.

add_node_to_visited(State, Index) ->
    Visited = State#state.visited_nodes,
    State#state{visited_nodes = [Index | Visited]}.

%%--------------------------------------------------------------------
%% Function: finish(...)
%% Descrip.: create return result - from running a cpl script
%% Returns : {Action, Response}
%%--------------------------------------------------------------------
finish_redirect(Permanent, State) ->
    Locations = State#state.locations,
    {redirect, Permanent, location_to_uri(Locations)}.

finish_reject(#reject__attrs{status = Status, reason = Reason}, _State) ->
    case Reason of
	"" -> {reject, Status};
	_ -> {reject, Status, Reason}
    end.

%% only called if proxy succeded in finding a working location
finish_proxy(State) ->
    {proxy, State#state.response}.


%% Default behavior for termination, as described in chapter 10 in RFC 3880
finish_execution(State) ->
    Visited = State#state.visited_nodes,
    Graph = State#state.graph,
    IsLocMod = location_mod_performed(Graph, Visited),
    IsSignaled = signalling_performed(Graph, Visited),
    IsLocEmpty = case State#state.locations of
		     [] -> true;
		     _ -> false
		 end,
    Locations = State#state.locations,
    IsLastProxy = is_last_node_proxy(Graph, Visited),

    case {IsLocMod, IsSignaled, IsLocEmpty, IsLastProxy} of
	%% server uses default (non-cpl script) behavior
	{false, false, true, _} ->
	    {server_default_action};
	%% (outgoing call) proxy call to address in location set
	{false, false, false, _} ->
	    [Location] = Locations,
	    {proxy_call_to_location, location_to_uri(Location)};
	%% proxy or redirect according to server policy to addresses in location set
	{true, false, false, _} ->
	    {proxy_or_redirect_to_locations, Locations};
	%% reject as 'notfound'
	{true, false, true, _} ->
	    ErrorCode = xml_parse_util:status_code_to_sip_error_code("notfound"),
	    {reject, ErrorCode};

	%% successful proxy calls are handled in finish_proxy/1, they end up
	%% here if there is no proxy tag subaction to handle busy, noanswer ...
	%% Note: proxy is the only signaling op that can end up here, reject
	%% and redirect always terminate the same way in their own finish_xxx(...)
	%% functions
	{_, true, true, true} ->
	    {proxy, State#state.response};
	{_, true, false, true} ->
	    {proxy, State#state.response};

	%% last signaling op was a proxy (reject and reject can't be previous
	%% signaling ops as they always terminate)
	{_, true, _, false} ->
	    {use_last_proxy_result, State#state.response}

    end.

is_last_node_proxy(_Graph, []) ->
    false;
is_last_node_proxy(Graph, [Index | _Visited]) ->
    case (get_node(Graph, Index))#node_code.type of
	proxy ->
	    true;
	_ ->
	    false
    end.

%% remove the cpl specific parts from the "addresses"
location_to_uri(Location) when is_record(Location, location) ->
    Location#location.url;
location_to_uri(Locations) when is_list(Locations) ->
    [location_to_uri(Loc) || Loc <- Locations].

%%--------------------------------------------------------------------
%% Function: location_mod_performed(Graph, VisitedNodeIndexList)
%%           signalling_performed(Graph, VisitedNodeIndexList)
%% Descrip.: determine if a location modifier or signaling node has
%%           been visited during script execution
%% Returns : true | false
%%--------------------------------------------------------------------
location_mod_performed(_Graph, []) ->
    false;
location_mod_performed(Graph, [Index | Visited]) ->
    case (get_node(Graph, Index))#node_code.type of
	location ->
	    true;
	lookup ->
	    true;
	'remove-location' ->
	    true;
	_ ->
	    location_mod_performed(Graph, Visited)
    end.

signalling_performed(_Graph, []) ->
    false;
signalling_performed(Graph, [Index | Visited]) ->
    case (get_node(Graph, Index))#node_code.type of
	proxy ->
	    true;
	redirect ->
	    true;
	reject ->
	    true;
	_ ->
	    signalling_performed(Graph, Visited)
    end.


%%--------------------------------------------------------------------
%% Function: get_node(Graph, Index)
%%           Graph            = list() of {NodeIndex, NodeCode}
%%           Index, NodeIndex = list() of integer(), node id created
%%                              in xml_parse.erl
%%           NodeCode         = node_code record()
%% Descrip.: find node with Index in Graph
%% Returns : node_code reccord() | throw({error, index_not_found})
%% Note    : Graph could be implemented as a gb_tree (see gb_tree
%%           module) which will be faster for large graphs,
%%           O(log N * log N) instead of O(N log N) (number of nodes
%%           processed = log N = path through graph)
%%--------------------------------------------------------------------
get_node([], Index) ->
    throw({error, {index_not_found, Index}});
get_node([{Index, NodeCode} | _R], Index) ->
    NodeCode;
get_node([_N | R], Index) ->
    get_node(R, Index).

%%--------------------------------------------------------------------
%% Function: terminator(_Code, State)
%% Descrip.:
%% Returns : {terminated, NewState}
%%--------------------------------------------------------------------
terminator(_Code, State) ->
    {terminated, State}.

%%--------------------------------------------------------------------
%% Function: incoming(Code, State)
%%           outgoing(Code, State)
%%           Code  = Index
%%           State = state record()
%% Descrip.: process a start node
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
incoming(Index, State) ->
    {Index, State}.

%% "For the outgoing action, it [#state.locations] is initialized to
%%  the destination address of the call." - RFC 3880 chapter 2.3
outgoing(Index, State) ->
    Request = State#state.request,
    Backend = State#state.backend,
    Dest = Backend:get_outgoing_destination(Request),
    State2 = State#state{locations = [#location{url = Dest}]},
    {Index, State2}.

%%--------------------------------------------------------------------
%% Function: 'address-switch'(Code, State)
%%           Code = {{Field, SubField}, Conds}
%%           State = state record()
%% Descrip.: process a address-switch
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
'address-switch'({{Field, SubField}, Conds}, State) ->
    Request = State#state.request,
    Backend = State#state.backend,
    ReqVal = Backend:'get_address-switch_field'(Request, Field, SubField),
    Dest = 'address-switch_cond'(ReqVal, Conds, Backend),
    {Dest, State}.

%% no condition matched
'address-switch_cond'(_ReqVal, [], _Backend) ->
    terminated;

'address-switch_cond'('#no_value', [{'not-present', Dest} | _R], _Backend) ->
    Dest;
'address-switch_cond'(ReqVal, [{'not-present', _Dest} | R], Backend) ->
    'address-switch_cond'(ReqVal, R, Backend);

%% compare address or part of a address
%% xml_parse.erl doesn't check if the addresses are legal so this is done in Backend instead
'address-switch_cond'('#no_value' = ReqVal, [{{address__is, _Val}, _Dest} | R], Backend) ->
    'address-switch_cond'(ReqVal, R, Backend);
'address-switch_cond'(ReqVal, [{{address__is, Val}, Dest} | R], Backend) ->
    case Backend:compare_address_or_address_part(ReqVal, Val) of
	true -> Dest;
	false -> 'address-switch_cond'(ReqVal, R, Backend)
    end;

%% determine if display part of address contains Val
'address-switch_cond'('#no_value' = ReqVal, [{{address__contains, _Val}, _Dest} | R], Backend) ->
    'address-switch_cond'(ReqVal, R, Backend);
'address-switch_cond'(ReqVal, [{{address__contains, Val}, Dest} | R], Backend) ->
    case Backend:address_or_address_part_contains(ReqVal, Val) of
	true -> Dest;
	false -> 'address-switch_cond'(ReqVal, R, Backend)
    end;

'address-switch_cond'('#no_value' = ReqVal, [{{'address__subdomain-of', _Val}, _Dest} | R], Backend) ->
    'address-switch_cond'(ReqVal, R, Backend);
'address-switch_cond'(ReqVal, [{{'address__subdomain-of', Val}, Dest} | R], Backend) ->
    {host, ReqHostVal} = ReqVal,
    case Backend:is_subdomain(ReqHostVal, Val) of
	true -> Dest;
	false -> 'address-switch_cond'(ReqVal, R, Backend)
    end;

'address-switch_cond'(_ReqVal, [{otherwise, Dest} | _R], _Backend) ->
    Dest.


%%--------------------------------------------------------------------
%% Function: 'string-switch'(Code, State)
%%           Code = {Field, Conds}
%%           State = state record()
%% Descrip.: process a address-switch
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
'string-switch'({Field, Conds}, State) ->
    Request = State#state.request,
    Backend = State#state.backend,
    ReqVal = Backend:'get_string-switch_field'(Request, Field),
    Dest = 'string-switch_cond'(ReqVal, Conds, Backend),
    {Dest, State}.

'string-switch_cond'(_ReqVal, [], _Backend) ->
    terminated;

'string-switch_cond'('#no_value', [{'not-present', Dest} | _R], _Backend) ->
    Dest;
'string-switch_cond'(ReqVal, [{'not-present', _Dest} | R], Backend) ->
    'string-switch_cond'(ReqVal, R, Backend);

'string-switch_cond'('#no_value' = ReqVal, [{{string__is, _Val}, _Dest} | R], Backend) ->
    'string-switch_cond'(ReqVal, R, Backend);
'string-switch_cond'(ReqVal, [{{string__is, Val}, Dest} | R], Backend) ->
    case Backend:string_is(ReqVal, Val) of
	true -> Dest;
	false -> 'string-switch_cond'(ReqVal, R, Backend)
    end;

'string-switch_cond'('#no_value' = ReqVal, [{{string__contains, _Val}, _Dest} | R], Backend) ->
    'string-switch_cond'(ReqVal, R, Backend);
'string-switch_cond'(ReqVal, [{{string__contains, Val}, Dest} | R], Backend) ->
    case Backend:string_contains(ReqVal, Val) of
	true -> Dest;
	false -> 'string-switch_cond'(ReqVal, R, Backend)
    end;

'string-switch_cond'(_ReqVal, [{otherwise, Dest} | _R], _Backend) ->
    Dest.

%%--------------------------------------------------------------------
%% Function: 'language-switch'(Code, State)
%%           Code = Conds
%%           State = state record()
%% Descrip.: process a address-switch
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
'language-switch'(Conds, State) ->
    Request = State#state.request,
    Backend = State#state.backend,
    ReqVal = Backend:'get_language-switch_value'(Request),
    Dest = 'language-switch_cond'(ReqVal, Conds, Backend),
    {Dest, State}.

'language-switch_cond'(_ReqVal, [], _Backend) ->
    terminated;

'language-switch_cond'('#no_value', [{'not-present', Dest} | _R], _Backend) ->
    Dest;
'language-switch_cond'(ReqVal, [{'not-present', _Dest} | R], Backend) ->
    'language-switch_cond'(ReqVal, R, Backend);

'language-switch_cond'('#no_value' = ReqVal, [{{language__matches, _Val}, _Dest} | R], Backend) ->
    'language-switch_cond'(ReqVal, R, Backend);
'language-switch_cond'(ReqVal, [{{language__matches, Val}, Dest} | R], Backend) ->
    case Backend:language_matches(ReqVal, Val) of
	true -> Dest;
	false -> 'language-switch_cond'(ReqVal, R, Backend)
    end;

'language-switch_cond'(_ReqVal, [{otherwise, Dest} | _R], _Backend) ->
    Dest.

%%--------------------------------------------------------------------
%% Function: 'priority-switch'(Code, State)
%%           Code = Conds
%%           State = state record()
%% Descrip.: process a address-switch
%% Returns : {NextId, NewState}
%% Note    : "Since every message has a priority, the "not-present"
%%            output is never true for a priority switch." - RFC 3880
%%            chapter 4.5 p21
%%--------------------------------------------------------------------
'priority-switch'(Conds, State) ->
    Request = State#state.request,
    Backend = State#state.backend,
    ReqVal = Backend:'get_priority-switch_value'(Request),
    Dest = 'priority-switch_cond'(ReqVal, Conds, Backend),
    {Dest, State}.

'priority-switch_cond'(_ReqVal, [], _Backend) ->
    terminated;

'priority-switch_cond'(ReqVal, [{{priority__less, Val}, Dest} | R], Backend) ->
    case Backend:priority_less(ReqVal, Val) of
	true -> Dest;
	false -> 'priority-switch_cond'(ReqVal, R, Backend)
    end;

'priority-switch_cond'(ReqVal, [{{priority__greater, Val}, Dest} | R], Backend) ->
    case Backend:priority_greater(ReqVal, Val) of
	true -> Dest;
	false -> 'priority-switch_cond'(ReqVal, R, Backend)
    end;

'priority-switch_cond'(ReqVal, [{{priority__equal, Val}, Dest}  | R], Backend) ->
    case Backend:priority_equal(ReqVal, Val) of
	true -> Dest;
	false -> 'priority-switch_cond'(ReqVal, R, Backend)
    end;

'priority-switch_cond'(_ReqVal, [{otherwise, Dest} | _R], _Backend) ->
    Dest.

%%--------------------------------------------------------------------
%% Function: 'time-switch'(Code, State)
%%           Code = {Timezone, Conds}
%%           State = state record()
%% Descrip.: process a time-switch
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
'time-switch'({Timezone, Conds}, State) when record(Timezone, time_zone) ->
    Backend = State#state.backend,
    Dest = 'time-switch_cond'(Timezone, Conds, Backend),
    {Dest, State}.

'time-switch_cond'(_Timezone, [], _Backend) ->
    terminated;

'time-switch_cond'(_Timezone, [{otherwise, Dest} | _R], _Backend) ->
    Dest;

'time-switch_cond'(Timezone, [{TimeSwitchCond, Dest} | R], Backend) ->
    case Backend:in_time_range(Timezone, TimeSwitchCond) of
	true -> Dest;
	false -> 'time-switch_cond'(Timezone, R, Backend)
    end.

%%--------------------------------------------------------------------
%% Function: location(Location, State)
%%           Location = location__attrs record()
%%           State = state record()
%% Descrip.: process a location tag, this adds the location to the
%%           location set used by cpl
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
location({#location__attrs{url = URI, priority = Prio, clear = Clear}, Dest}, State) ->
    Locations = State#state.locations,
    NewLocations = case Clear of
		       yes -> [];
		       no -> Locations
		   end,
    NewState = State#state{locations =
			   [#location{url = sipurl:parse(URI), priority = Prio} | NewLocations]
			  },
    {Dest, NewState}.

%%--------------------------------------------------------------------
%% Function: lookup({Lookup,Cond}, State)
%%           Lookup = lookup record()
%%           Cond = list of {LookupResult, Dest}
%%           State = state record()
%% Descrip.: process the lookup tag
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
lookup({#lookup__attrs{source = Source, timeout = Timeout, clear = Clear}, Cond}, State) ->
    Backend = State#state.backend,
    Locations = State#state.locations,
    NewLocations = case Clear of
		       yes -> [];
		       no -> Locations
		   end,
    {Res, URIs} = Backend:lookup(Source, State#state.user, (State#state.request)#request.uri, Timeout),
    LookupLocations = [#location{url = URI} || URI <- URIs],
    NewState = case Res of
		   success ->
		       State#state{locations = LookupLocations ++ NewLocations};
		   notfound ->
		       State;
		   failure ->
		       State
	       end,
    {lookup_dest(Res, Cond), NewState}.

%% get the destination for the result of the lookup
%% terminate cpl script if no match is found (i.e. execute default behavior)
lookup_dest(_Result, []) ->
    terminated;
lookup_dest(Result, [{Result, Dest} | _R]) ->
    Dest;
lookup_dest(Result, [_ | R]) ->
    lookup_dest(Result, R).

%%--------------------------------------------------------------------
%% Function: 'remove-location'(Location, State)
%%           Location = {Loc, Dest}
%%           Loc = remove_location__attrs record()
%%           State = state record()
%% Descrip.: process a location tag, this adds the location to the
%%           location set used by cpl
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
'remove-location'({#remove_location__attrs{location = all}, Dest}, State) ->
    NewState = State#state{locations = []},
    {Dest, NewState};
'remove-location'({#remove_location__attrs{location = URIStr}, Dest}, State) ->
    Locations = State#state.locations,
    Backend = State#state.backend,
    NewLocations = Backend:rm_location(Locations, URIStr),
    NewState = State#state{locations = NewLocations},
    {Dest, NewState}.

%%--------------------------------------------------------------------
%% Function: sub(Dest, State)
%%           Dest = node id
%% Descrip.: process sub tag
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
sub(Dest, State) ->
    {Dest, State}.

%%--------------------------------------------------------------------
%% Function: log({Log, Dest}, State)
%%           Log  = log__attrs record()
%%           Dest = node id
%% Descrip.: log the data in Log
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
log({Log, Dest}, State) ->
    Backend = State#state.backend,
    User = State#state.user,
    Request = State#state.request,
    Backend:log(Log, User, Request),
    {Dest, State}.

%%--------------------------------------------------------------------
%% Function: mail({Mail, Dest}, State)
%%           Mail = string(), a mail url
%%           Dest = node id
%% Descrip.: send mail
%% Returns : {NextId, NewState}
%%--------------------------------------------------------------------
mail({Mail, Dest}, State) ->
    User = State#state.user,
    Backend = State#state.backend,
    Backend:mail(Mail, User),    
    {Dest, State}.

%%--------------------------------------------------------------------
%% Function: proxy(Code, State)
%%           Code  = {ProxyAttrs, Conds}
%%           State = state record()
%% Descrip.:
%% Returns : {NextId, NewState}
%% Note    : RFC 3880 is rather vague about how Timeout should be
%%           handled when ordering = sequential, the most plausible
%%           interpretation seams to be that the timeout applies to
%%           the "global" behavior of the proxy, i.e. timeout is the
%%           total max time to wait for any kind of proxy condition.
%%           This interpretation requires that the timeout period is
%%           divided between the different locations (as their
%%           individual timeout periods) when ordering = sequential,
%%           the create_sequential_proxyaction_list/4 function does
%%           this.
%%--------------------------------------------------------------------
proxy({#proxy__attrs{timeout = TimeoutIn, recurse = Recurse, ordering = Ordering}, Conds}, Count, State) ->
    Backend = State#state.backend,
    Locations = State#state.locations,

    %% ProxyActions can be formated in to ways:
    %% action timeout
    %% call   20        each call specifies how long the proxy should try to access the location
    %% call   20
    %% wait   20        wait timeout defines how long to wait until handling new calls so
    %%                  wait timeout must be = call timeout for 'parallel' ordering in the proxy tag
    %% or
    %% call   10        a 'sequential' ordering with a Timeout = 20, must divided this time between
    %% wait   10        each of it's calls - the example tries 2 calls waiting for 10 seconds for
    %% call   10        each one
    %% wait   10


    Timeout =
	case TimeoutIn of
	    server_max -> Backend:get_server_max();
	    _ -> TimeoutIn
	end,

    User = State#state.user,

    {ProxyActions, RemainingLocations}
	= case Ordering of
	      %% check all locations at once
	      parallel ->
		  {[#sipproxy_action{action = call, timeout = Timeout, requri = E#location.url, user = User}
		    || E <- Locations] ++
		   [#sipproxy_action{action = wait, timeout = Timeout}],
		   []};
	      %% wait Timeout second for each location entry
	      sequential ->
		  PrioOrderLocs1 = lists:sort(fun(E1, E2) -> E1#location.priority > E2#location.priority end,
					      Locations),
		  {create_sequential_proxyaction_list(PrioOrderLocs1, Backend, Timeout, User), []};
	      %% only check the highest priority location
	      'first-only' ->
		  PrioOrderLocs2 = lists:sort(fun(E1, E2) -> E1#location.priority > E2#location.priority end,
					      Locations),
		  [First | Rest] = PrioOrderLocs2,
		  {[#sipproxy_action{action = call, timeout = Timeout, requri = First#location.url, user = User},
		    #sipproxy_action{action = wait, timeout = Timeout}],
		   Rest}
	  end,
    Request = State#state.request,
    BranchBase = State#state.branch_base,
    STHandler = State#state.sthandler,
    {Result, BestURI, BestResponse} =
	Backend:test_proxy_destinations(Count, BranchBase, Request, ProxyActions, Timeout, Recurse, STHandler),

    %% remove used locations
    State2 = set_locations(State, RemainingLocations),
    State3 = case Result of
		 %% this indicates that proxy doesn't needed to execute any of it's proxy conditions
		 success -> State2;
		 %% the non-successful proxying results
		 busy -> State2;
		 noanswer -> State2;
		 redirection -> case BestURI of
				    none -> State2;
				    _ -> add_locations(State2, [uri_to_location(BestURI)])
				end;
		 failure -> State2
	     end,
    State4 = State3#state{response = BestResponse},
    case Result of
	%% success terminates the script
	success ->
	    {{terminated, proxy}, State4};
	_ ->
	    {proxy_dest(Result, Conds), State4}
    end.

%% convert sipurl record to location record
uri_to_location(URI) when is_record(URI, sipurl) ->
    #location{url = URI};
uri_to_location(URIs) when is_list(URIs) ->
    [uri_to_location(URI) || URI <- URIs].

add_locations(State, Locations) ->
    set_locations(State, Locations ++ State#state.locations).
set_locations(State, Locations) ->
    State#state{locations = Locations}.

%% Check if result of proxy has a destination node, or if it isn't
%% available, check if there is a 'default' destination node. If no
%% destinations are available the return will simply be 'terminated'
%% which indicates that the default handling should be used when finishing the cpl script
%% return: terminated | node id (for the next node)
proxy_dest(Result, Conds) ->
    case proxy_dest_cond_matches_res(Result, Conds) of
	false ->
	    case proxy_dest_default_defined(Conds) of
		false -> terminated;
		Dest1 -> Dest1
	    end;
	Dest2 -> Dest2
    end.

proxy_dest_cond_matches_res(Result, Conds) ->
    find_result(Result, Conds).
proxy_dest_default_defined(Conds) ->
    find_result(default, Conds).

find_result(_Cond, []) ->
    false;
find_result(Cond, [{Cond,Dest} | _R]) ->
    Dest;
find_result(Cond, [ _ | R]) ->
    find_result(Cond, R).
%%--------------------------------------------------------------------
%% Function: create_sequential_proxyaction_list(PrioOrderLocs,
%%           Backend, Timeout, User)
%%           PrioOrderLos = list of location record(), ordered on
%%                          priority
%%           Backend      = atom(), callback module
%%           Timeout      = integer(), no. of seconds
%%           User         = string(), SIP username of CPL script owner
%% Descrip.: return a list of sipproxy_action record() that defines
%%           how the the sip proxy in yxa is supposed to check the
%%           locations in PrioOrderLocs.
%% Returns : list of sipproxy_action record()
%% Note    : timeout for locations can be assigned in two ways:
%%           * if (Timeout / no. of locations) > minimum ring timeout
%%             the (Timeout / no. of locations) period is used for
%%             each location
%%           * otherwise minimum ring time, will be assigned in
%%             location priority order, until all Timeout seconds have
%%             been consumed (no location gets less than minimum ring
%%             time seconds), this means that some locations will NOT
%%             be checked by the proxy - as there are no seconds left
%%             to assign to them
%%--------------------------------------------------------------------
%% special case to handle TimePerLoc = Timeout div 0
create_sequential_proxyaction_list([], _Backend, _Timeout, _User) ->
    [];
create_sequential_proxyaction_list(PrioOrderLocs, Backend, Timeout, User) ->
    %% get minimum ring time, before a timeout should occure
    MinRing = Backend:get_min_ring(),
    NoOfLoc = length(PrioOrderLocs),
    TimePerLoc = Timeout div NoOfLoc,
    case TimePerLoc > MinRing of
	true ->
	    %% call all
	    call_all_proxyaction_list(PrioOrderLocs, Timeout, User, TimePerLoc);
	false ->
	    %% call as many as possible (during Timeout period) in prio order
	    Count = Timeout div MinRing, % determine number of locations to call
	    call_count_proxyaction_list(PrioOrderLocs, Timeout, User, MinRing, Count)
    end.

call_all_proxyaction_list([], _Timeout, _User, _TimePerLoc) ->
    [];
%% use any remaining Timeout seconds on the last location entry - the
%% value may be slightly larger than TimePerLoc (but is < 2 * TimePerLoc)
call_all_proxyaction_list([Location], Timeout, User, _TimePerLoc) ->
    [#sipproxy_action{action = call, timeout = Timeout, requri = Location#location.url, user = User},
     #sipproxy_action{action = wait, timeout = Timeout} ];
call_all_proxyaction_list([Location | R], Timeout, User, TimePerLoc) ->
    [#sipproxy_action{action = call, timeout = TimePerLoc, requri = Location#location.url, user = User},
     #sipproxy_action{action = wait, timeout = TimePerLoc} ] ++
	call_all_proxyaction_list(R, Timeout - TimePerLoc, User, TimePerLoc).


call_count_proxyaction_list(_, 0, _User, _MinRing, 0) ->
    [];
%% use any remaining Timeout seconds on the last location entry - the
%% value may be slightly larger than MinRing (but is < 2 * MinRing)
call_count_proxyaction_list([Location | _R], Timeout, User, _MinRing, 1) ->
    [#sipproxy_action{action = call, timeout = Timeout, requri = Location#location.url, user = User},
     #sipproxy_action{action = wait, timeout = Timeout} ];
call_count_proxyaction_list([Location | R], Timeout, User, MinRing, Count) ->
    [#sipproxy_action{action = call, timeout = MinRing, requri = Location#location.url, user = User},
     #sipproxy_action{action = wait, timeout = MinRing} ] ++
	call_count_proxyaction_list(R, Timeout - MinRing, User, MinRing, Count - 1).

%%--------------------------------------------------------------------
%% Function: redirect({Permanent, terminated}, State)
%%           Permanent = yes | no
%% Descrip.: redirect tag encountered, terminate cpl script and
%%           redirect call (in finish_execution)
%% Returns : {Termination, NewState}
%%--------------------------------------------------------------------
redirect({Permanent, terminate}, State) ->
    {{terminated, redirect, Permanent}, State}.

%%--------------------------------------------------------------------
%% Function: reject({RejectAttrs, terminate}, State) ->
%%           RejectAttrs = reject__attrs record()
%%           State       = state record()
%% Descrip.: process a reject tag - terminate cpl script whit a reject
%%           termination reason
%% Returns : {Termination, NewState}
%%--------------------------------------------------------------------
reject({RejectAttrs, terminate}, State) ->
    {{terminated, reject, RejectAttrs}, State}.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%%--------------------------------------------------------------------
test() ->

    interpret_cpl_test:test(),

    

    ok.

%% Called from interpret_cpl_test.erl to ensure calling order of tests.
%% The test is done here so that create_sequential_proxyaction_list/4 doesn't
%% need to be exported
test27() ->
    URI1 = sipurl:parse("sip:test1@foo.com"),
    URI2 = sipurl:parse("sip:test2@foo.com"),
    URI3 = sipurl:parse("sip:test3@foo.com"),
    URI4 = sipurl:parse("sip:test4@foo.com"),
    URI5 = sipurl:parse("sip:test5@foo.com"),
    URI6 = sipurl:parse("sip:test6@foo.com"),

    PrioOrderLocs = [#location{url = URI1, priority = 1.0}, 
		     #location{url = URI2, priority = 0.8}, 
		     #location{url = URI3, priority = 0.7}, 
		     #location{url = URI4, priority = 0.6}, 
		     #location{url = URI5, priority = 0.0}, 
		     #location{url = URI6, priority = 0.0} 
		    ],
    Backend = test_backend, 
    Timeout1 = 125,
    %% ensure that min ring time is set to the expected value
    10 = Backend:get_min_ring(),

    Res1 = create_sequential_proxyaction_list(PrioOrderLocs, Backend, Timeout1, "test"),
    %% io:format("Res1 = ~p~n",[Res1]),
    [
     #sipproxy_action{action = call, timeout = 20, requri = URI1, user = "test"},
     #sipproxy_action{action = wait, timeout = 20},
     #sipproxy_action{action = call, timeout = 20, requri = URI2, user = "test"},
     #sipproxy_action{action = wait, timeout = 20},
     #sipproxy_action{action = call, timeout = 20, requri = URI3, user = "test"},
     #sipproxy_action{action = wait, timeout = 20},
     #sipproxy_action{action = call, timeout = 20, requri = URI4, user = "test"},
     #sipproxy_action{action = wait, timeout = 20},
     #sipproxy_action{action = call, timeout = 20, requri = URI5, user = "test"},
     #sipproxy_action{action = wait, timeout = 20},
     #sipproxy_action{action = call, timeout = 25, requri = URI6, user = "test"},
     #sipproxy_action{action = wait, timeout = 25}
    ] = Res1,


    Timeout2 = 35,
    %% ensure that min ring time is set to the expected value
    10 = Backend:get_min_ring(),

    Res2 = create_sequential_proxyaction_list(PrioOrderLocs, Backend, Timeout2, "test"),
    %% io:format("Res2 = ~p~n",[Res2]),
    [
     #sipproxy_action{action = call, timeout = 10, requri = URI1, user = "test"},
     #sipproxy_action{action = wait, timeout = 10},
     #sipproxy_action{action = call, timeout = 10, requri = URI2, user = "test"},
     #sipproxy_action{action = wait, timeout = 10},
     #sipproxy_action{action = call, timeout = 15, requri = URI3, user = "test"},
     #sipproxy_action{action = wait, timeout = 15}
    ] = Res2,
    
    %% test empty list
    Timeout3 = 20,
    Res3 = create_sequential_proxyaction_list([], Backend, Timeout3, "test"),
    %% io:format("Res3 = ~p~n",[Res3]),
    [] = Res3.
