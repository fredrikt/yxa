%% A support module of xml_parse.erl. This module is used to create a
%% graph representation of the CPL script, this allows checking for
%% cycles, unreachable vertexes and the like.
%% Each CPL "expression" - switch and sub conditions or single action
%% is represented by one digraph vertex, where all information about
%% the expression is stored in the vertex label.
%%
%% There are two types of vertexes, switches (switch tags) where
%% several vertexes can be reached and action vertexes - tags that do
%% something and then continue on to the next tag.
%% Both of these kind of vertexes may also have 0 destination tags
%% (no tags in content of current tag), this is represented in a few
%% different ways:
%% * switches: contain a empty Cond in the vertex and has no outgoing
%%             edges.
%% * actions : some are known to always terminate a CPL script and
%%             therefore never have outgoing edges, actions on the
%%             other hand that do usually have a destination, use
%%             add_node(ParseState, terminator) to add a extra
%%             termination vertex - this isn't strictly needed but
%%             simplifies the parser code.
%%--------------------------------------------------------------------

-module(xml_parse_graph).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 new_graph/0,
	 add_node/2,
	 add_node/3,
	 add_node/4
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("cpl.hrl").
-include("xml_parse.hrl").

-include_lib("xmerl/include/xmerl.hrl").

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
%% @spec    () -> term() "digraph"
%%
%% @doc     return a digraph that checks that graph remains acyclical
%%          Note : the digraph graph, returned is implemented (in
%%          digraph) with a ets table so the returned graph acts as a
%%          reference (pointer), rather than a single assignment
%%          variable value.
%% @end
%%--------------------------------------------------------------------
new_graph() ->
    digraph:new([acyclic]).

%%--------------------------------------------------------------------
%% @spec    (ParseState, TagType ) ->
%%            #parse_state{} "containing the new graph (due to the implementation of digraph this will be the same value - therefore ParseState is simply returned)"
%%
%%            ParseState = #parse_state{} "is used by the parser to store the currently accumulated parse data"
%%            TagType    = atom() "the name of CPL vertex to store"
%%
%% @doc     adds a node of TagType to the ParseState.
%% @end
%%--------------------------------------------------------------------
%% Node add algorithm:
%%
%% * add / update current node
%% * add target nodes, not yet visited - will contain dummy labels,
%%   need to be set so that edges can have a destination
%% * add edges from current node to target nodes

%% a kind of dummy state used when cpl doesn't specifiy any destination node
add_node(ParseState, terminator) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    add_current_node(Graph, CurrentNodeId, terminator, no_statements),
    ParseState;

add_node(ParseState, incoming) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, incoming, [1,1]),
    V1 = digraph:add_vertex(Graph, CurrentNodeId ++ [1], lable_must_be_set),
    add_edge(Graph, S, V1),
    ParseState;

add_node(ParseState, outgoing) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, outgoing, [2,1]),
    V1 = digraph:add_vertex(Graph, CurrentNodeId ++ [1], lable_must_be_set),
    add_edge(Graph, S, V1),
    ParseState.

%%--------------------------------------------------------------------
%% @spec    (ParseState, TagType, DestCond ) ->
%%            #parse_state{} "containing the new graph (due to the implementation of digraph this will be the same value - therefore ParseState is simply returned)"
%%
%%            ParseState = #parse_state{} "is used by the parser to store the currently accumulated parse data"
%%            TagType    = atom() "the name of CPL vertex to store"
%%            DestCond   = term() "switches use a list of {Cond, Dest} pairs, this field is used to store action and destination vertex. The content of these fields is TagType dependent"
%%
%% @doc     adds a node of TagType to the ParseState.
%% @end
%%--------------------------------------------------------------------
%% Node add algorithm:
%%
%% * add / update current node
%% * add target nodes, not yet visited - will contain dummy labels,
%%   need to be set so that edges can have a destination
%% * add edges from current node to target nodes

add_node(ParseState, 'language-switch', Conds) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, 'language-switch', Conds),
    add_dests(S, Graph, Conds),
    ParseState;

add_node(ParseState, 'priority-switch', Conds) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, 'priority-switch', Conds),
    add_dests(S, Graph, Conds),
    ParseState;

%% -----------------------------

add_node(ParseState, location, {Location, NextId}) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, location, {Location, NextId}),
    add_single_dest(Graph, S, NextId),
    ParseState;

add_node(ParseState, 'remove-location', {RmLocation, NextId}) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, 'remove-location', {RmLocation, NextId}),
    add_single_dest(Graph, S, NextId),
    ParseState;

add_node(ParseState, sub, NextId) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, sub, NextId),
    %% NextId is a existing subaction node
    add_edge(Graph, S, NextId),
    ParseState;

add_node(ParseState, log, {Log, NextId}) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, log, {Log, NextId}),
    add_single_dest(Graph, S, NextId),
    ParseState;

add_node(ParseState, mail, {Mail, NextId}) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, mail, {Mail, NextId}),
    add_single_dest(Graph, S, NextId),
    ParseState;

add_node(ParseState, redirect, {Permanent, terminate}) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    add_current_node(Graph, CurrentNodeId, redirect, {Permanent, terminate}),
    ParseState;

add_node(ParseState, reject, {StatusReason, terminate}) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    add_current_node(Graph, CurrentNodeId, reject, {StatusReason, terminate}),
    ParseState.

%%--------------------------------------------------------------------
%% @spec    (ParseState, TagType, Index, DestCond ) ->
%%            #parse_state{} "containing the new graph (due to the implementation of digraph this will be the same value - therefore ParseState is simply returned)"
%%
%%            ParseState = #parse_state{} "is used by the parser to store the currently accumulated parse data"
%%            TagType    = atom() "the name of CPL vertex to store"
%%            Index      = term() "a index value to allow lookup in the SIP request. The content is TagType dependent."
%%            DestCond   = term() "switches use a list of {Cond, Dest} pairs, this field is used to store action and destination vertex. The content is TagType dependent"
%%
%% @doc     adds a node of TagType to the ParseState.
%% @end
%%--------------------------------------------------------------------
%% Node add algorithm:
%%
%% * add / update current node
%% * add target nodes, not yet visited - will contain dummy labels,
%%   need to be set so that edges can have a destination
%% * add edges from current node to target nodes

add_node(ParseState, 'address-switch', {Field, SubField}, Conds) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, 'address-switch', {{Field, SubField}, Conds}),
    add_dests(S, Graph, Conds),
    ParseState;

add_node(ParseState, 'time-switch', TimeZone, Conds) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, 'time-switch', {TimeZone, Conds}),
    add_dests(S, Graph, Conds),
    ParseState;

add_node(ParseState, 'string-switch', Field, Conds) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, 'string-switch', {Field, Conds}),
    add_dests(S, Graph, Conds),
    ParseState;

add_node(ParseState, lookup, Lookup ,Conds) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, lookup, {Lookup, Conds}),
    add_dests(S, Graph, Conds),
    ParseState;

add_node(ParseState, proxy, ProxyAttrs ,Conds) ->
    {CurrentNodeId, Graph} = add_init(ParseState),
    S = add_current_node(Graph, CurrentNodeId, proxy, {ProxyAttrs, Conds}),
    add_dests(S, Graph, Conds),
    ParseState.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================


add_init(ParseState) ->
    CurrentNodeId = ParseState#parse_state.current_id,
    Graph = ParseState#parse_state.current_graph,
    {CurrentNodeId, Graph}.

add_dests(CurrentNode, Graph, Cond) ->
    %% created but unlabled
    DestVs = [digraph:add_vertex(Graph, Dest, lable_must_be_set) || {_, Dest} <-Cond],
    [add_edge(Graph, CurrentNode, V) || V <- DestVs].

add_single_dest(Graph, CurrentNode, NextId) ->
    V1 = digraph:add_vertex(Graph, NextId, lable_must_be_set),
    add_edge(Graph, CurrentNode, V1).

%%--------------------------------------------------------------------
%% @spec    (Graph, Source, Dest) ->
%%            ok 
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     call digraph:add_edge/3, throw a error if cycles are
%%          detected
%% @end
%%--------------------------------------------------------------------
add_edge(Graph, Source, Dest) ->
    case digraph:add_edge(Graph, Source, Dest) of
	{error, _Reason} ->
	    throw({error, cycle_detected_in_graph});
	_ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (Graph, CurrentNodeId, TagType, Statements) ->
%%            DiGraph 
%%
%%            DiGraph = term()
%%            Reason  = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     add a new node or replace a dummy node previously created
%% @end
%%--------------------------------------------------------------------
add_current_node(Graph, CurrentNodeId, TagType, Statements) ->
    case digraph:vertex(Graph, CurrentNodeId) of
	{CurrentNodeId, lable_must_be_set} ->
	    %% replace dummy node
	    NodeCode = #node_code{type = TagType, statements = Statements},
	    digraph:add_vertex(Graph, CurrentNodeId, NodeCode);
	{CurrentNodeId, _Label} ->
	    throw({error, trying_to_set_a_existing_fully_initiated_node});
	false ->
	    %% add new node
	    NodeCode = #node_code{type = TagType, statements = Statements},
	    digraph:add_vertex(Graph, CurrentNodeId, NodeCode)
    end.
