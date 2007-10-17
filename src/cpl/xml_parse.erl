%% This module transforms a CPL xml script into a graph
%% representation (during script parsing), that can be used to test
%% various graph properties like; is the graph acyclical, does
%% it contain unreachable states ...
%%
%% The graph can either be interpreted directly as a FSM (finite
%% state machine) when processing SIP request or the graph can be
%% transformed into a FSM implemented in source code (.erl) - each
%% script will then be handled by a specific .beam file, which should
%% execute faster than when runned in a interpreter.
%%
%% Note: all xml CPL tags are represented with atoms ('...' may be
%%       needed in some cases) without any changes - this makes it
%%       easy to match the specification to the code.
%% Note: all graph nodes have a unique id, ids are list() of int()
%%       identifying which branches was selected to reach the
%%       node, for example [1,1], [1,2], [1,1,1] ...
%%       The incoming tag id is always = [1] and outgoing always =
%%       [2], so it's easy to find the start node. Subactions
%%       are numbered as [3], [4], ...
%% Note: subaction don't do anything except to point to another node,
%%       therefor they aren't stored in the graph and the references
%%       to [N] becomes a reference to the action which subaction [N]
%%       contained / pointed to.
%%
%% To do:
%% * add more descriptive error messages, include line number,
%%   tag/attribute name and value encountered
%% * the current implementation doesn't reject all unexpected sub tags
%%   and attributes - they are simply ignored. A error should be
%%   generated when they are encountered so that misspelt and
%%   unsupported tags/attributes can be detected - this is mainly a
%%   problem if scripts are hand coded or if the use extensions to
%%   RFC 3880
%% - code checks if sub tags name is allowed as element of a tag
%% - check for otherwise as last element (if otherwise sub tag is
%%   used) is done
%% - checks are done for sub tags that occur multiple time when they
%%   should only occur 0-1 times
%% * add time zone and summer/winter time support
%% * there is no check for overlapping time intervals - the cost (in
%%   time) of this feature is high compared to gain received from
%%   implementing it
%% * all atoms could be changed into macros so that the compiler
%%   catches misspelt names - "undefined macro used"
%%--------------------------------------------------------------------

-module(xml_parse).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 cpl_script_to_graph/1,

	 test/0
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
%% @spec    (CPLscript) ->
%%            [{VertexId, Label}]
%%
%%            CPLscript = string() "xml data"
%%
%%            VertexId = [integer()] "a unique node id"
%%            Label    = term() "the 'code' of the node"
%%            Error    = bad_xml                    |
%%                       cycle_detected_in_graph    |
%%                       unreachable_nodes_detected |
%%                       atom()
%%
%% @throws  {error, Error} 
%%
%% @doc     parse and validate CPLscript - check ranges and that there
%%          are no cycles, return a graph that models the flow of the
%%          script - the graph will be used as a FSM by a interpreter
%%          (and possibly to generate FSM .erl code) Note : Error =
%%          bad_xml (xmerl_scan:string failed) |
%%          cycle_detected_in_graph (add_edge/3 failed) |
%%          unreachable_nodes_detected | .... and various other parse
%%          error
%% @end
%%--------------------------------------------------------------------
cpl_script_to_graph(CPLscript) ->
    case xmerl_scan:string(CPLscript) of
	%% XXX Rest in {XMLtag, Rest} appears to be empty list, this
	%% is probably so, expect if partial parsing is done.
	{XMLtag , []} ->
	    ParseState = parse_xml_main(XMLtag),
	    G = ParseState#parse_state.current_graph,
	    Vs = digraph:vertices(G),
	    Nodes = [digraph:vertex(G, V) || V <- Vs],
	    check_for_unreachable_nodes(Vs, G),
	    Nodes;
	%% xmerl_scan error
	{error,_Reason} ->
	    throw({error, bad_xml});
	{'EXIT',_Reason} ->
	    throw({error, bad_xml})
    end.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Vs, G) -> term()
%%
%%            Vs = [vertex()] "a list of node ids"
%%            G  = digraph() "the graph for the CPL script"
%%
%% @throws  {error, unreachable_nodes_detected} 
%%
%% @doc     verify that there are no unreachable nodes in the final
%%          graph. Note : this code only checks if all CPL script
%%          nodes (tags) are reachable through graph edges, it
%%          doesn't check if switch conditions may be ordered in such
%%          a way that certain nodes can never be reached.
%% @end
%%--------------------------------------------------------------------
check_for_unreachable_nodes(Vs, G) ->
    %% find all nodes reachable by incoming and outgoing
    R1 = digraph_utils:reachable([[1]], G),
    R2 = digraph_utils:reachable([[2]], G),
    %% reachable includes [1] and [2] in R1 and R2, even if they
    %% aren't members of the graph.
    %% This can be detected as a proper graph of <incoming/> or <outgoing/>
    %% will consist of at least [[1],[1,1]] or [[2],[2,1]] as they require
    %% at least a terminator node
    R = case {R1, R2} of
	    {[_],[_]} -> [];
	    {[_], _} -> R2;
	    {_, [_]} -> R1;
	    {_,_} -> R1 ++ R2 %% append is ok as a ids are unique
	end,

    %% order both vertex sets the same way so they can be compared
    S1 = lists:sort(Vs),
    S2 = lists:sort(R),
    %% check that the reachable vertexes are the same as the full set in the graph
    case S1 == S2 of
	true -> ok;
	false -> throw({error, unreachable_nodes_detected})
    end.

%%--------------------------------------------------------------------
%% @spec    (CPLscript) ->
%%            #parse_state{} 
%%
%%            CPLscript = #xmlElement{} "the toplevel tag of the xml code"
%%
%%            Reason = no_incoming_or_outgoing_tag | atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     the cpl tag can contain subaction tags and a incoming
%%          and/or outgoing tag Note : CPL expects tags in order:
%%          ancillary, subaction, incoming, outgoing - the order of
%%          incoming/outgoing isn't clearly defined in RFC 3880 and
%%          not enforced in the code Note : the 'ancillary' tag is
%%          ignored - it isn't defined to do anything in RFC 3880
%% @end
%%--------------------------------------------------------------------
parse_xml_main(#xmlElement{name = cpl} = CPLscript) ->
    %% there may be 0+ subactions
    %% this code is run first to gather all subaction names
    SubActions = get_elements(CPLscript, subaction),
    ParseState = parse_xml_subactions(SubActions, initial_parse_state()),

    %% look for a incoming and outgoing xml tag, they are the two
    %% possible starting points (incoming / outgoing id = [1] / [2]),
    %% when a CPL script is run
    case {get_elements(CPLscript, incoming),
	  get_elements(CPLscript, outgoing) } of
	{[], []} ->
	    throw({error, no_incoming_or_outgoing_tag});
	{[In], []} ->
	    parse_xml(In, ParseState#parse_state{current_id = [1]});
	{[], [Out]} ->
	    parse_xml(Out, ParseState#parse_state{current_id = [2]});
	{[In], [Out]} ->
	    PState = parse_xml(In, ParseState#parse_state{current_id = [1]}),
	    parse_xml(Out, PState#parse_state{current_id = [2]})
    end.

initial_parse_state() ->
    #parse_state{
		 %% start index used for subaction
		 current_id = [2],
		 current_graph = xml_parse_graph:new_graph(),
		 %% initiate mapping DB
		 subaction_name_id_mapping = []
		 }.

%%--------------------------------------------------------------------
%% @spec    (SubActionXmlParseTree, ParseState) -> #parse_state{}
%%
%% @doc     parse the sub action xml parse tree, ParseState is used to
%%          store the currently accumulated graph Note : On the
%%          subject of allowing 'sub' tags to only reference already
%%          defined subactions (see RFC 3880): - This ensured by two
%%          parser features, the
%%          #parse_state.subaction_name_id_mapping (see below) only
%%          contains the subaction names of currently parsed
%%          subactions, so lookup of later subactions will fail,
%%          while self references in a sub tag will fail du to the
%%          cycle detection (see xml_parse_graph.erl)
%% @end
%%--------------------------------------------------------------------
%% return the accumulated parse state when all tags have been processed
parse_xml_subactions([], ParseState) ->
    ParseState;

%% subactions can only be members of the cpl tag, this means that
%% they will have ids = [3], [4] .... [N]
parse_xml_subactions([#xmlElement{name = subaction, content = Content} = E | R], ParseState) ->
    SubactionName = case get_attribute(E, id) of
			'#no_value' -> throw({error, subaction_tag_is_missing_the_id_attribute});
			ID -> ID
		    end,
    case is_subaction_name_unqiue(SubactionName, ParseState) of
	true ->
	    [Index] = ParseState#parse_state.current_id,
	    NewId = [Index + 1],
	    Mapping = [{SubactionName, NewId} | ParseState#parse_state.subaction_name_id_mapping],
	    %% the subaction doesn't do anything so the node isn't stored in the graph
	    %% parse content of subaction
	    ParseState2 = parse_xml(get_next_element(subaction, Content),
				    ParseState#parse_state{current_id = NewId,
							   subaction_name_id_mapping = Mapping}),
	    %% then parse the remaining subactions
	    parse_xml_subactions(R, ParseState2#parse_state{current_id = NewId});
	false ->
	    throw({error, subaction_tag_id_attribute_value_is_not_unique})
    end.

%%--------------------------------------------------------------------
%% @spec    (TagElement, ParseState) -> #parse_state{}
%%
%%            TagElement = #xmlElement{}
%%            ParseState = #parse_state{}
%%
%% @doc     Processes TagElement tag and it's containing tags. Results
%%          are accumulated in ParseState while descending through
%%          the xml parse tree
%% @end
%%--------------------------------------------------------------------

%% there are cases when a tag may be empty, and some kind of default
%% action is supposed to be taken, the terminator node signifies this.
parse_xml(empty, ParseState) ->
    ParseState2 = xml_parse_graph:add_node(ParseState, terminator),
    %% nothing more to parse in this branch, so return current parse state
    ParseState2;

%% --------------------- top level entry points

parse_xml(#xmlElement{name = incoming, content = Content}, ParseState) ->
    %% add node to graph, this doesn't do anything but it's needed as a start node
    ParseState2 = xml_parse_graph:add_node(ParseState, incoming),
    {ParseState3, _NextId} = next_id(ParseState2),
    parse_xml(get_next_element(incoming, Content), ParseState3);

%% top level action
parse_xml(#xmlElement{name = outgoing, content = Content}, ParseState) ->
    %% add node to graph, this doesn't do anything but it's needed as a start node
    ParseState2 = xml_parse_graph:add_node(ParseState, outgoing),
    {ParseState3, _NextId} = next_id(ParseState2),
    parse_xml(get_next_element(outgoing, Content), ParseState3);

%% --------------------- switches

parse_xml(#xmlElement{name = 'address-switch', content = Content} = E, ParseState) ->
    Field = attribute_to_atom(get_attribute(E, field)),
    %% attribute field is mandatory so it can't be = '#no_value'
    xml_parse_util:legal_value(Field, [origin, destination, 'original-destination']),

    %% 'tel' is a legal value in RFC 3880, but currently unsupported
    SubField = attribute_to_atom(get_attribute(E, subfield)),
    xml_parse_util:legal_value(SubField, ['address-type', user, host, port, display, password, '#no_value']),

    %% get the branching rules and the code for those nodes, so that they can be parsed
    {Conds, Targets} = get_cond(Content, 'address-switch', SubField, ParseState),

    %% node stores: type, request index, cond op + match val
    ParseState2 = xml_parse_graph:add_node(ParseState, 'address-switch', {Field, SubField}, Conds),

    %% process the destination nodes
    process_targets('address-switch', Targets, ParseState2);


parse_xml(#xmlElement{name = 'language-switch', content = Content}, ParseState) ->
    {Conds, Targets} = get_cond(Content, 'language-switch', no_args, ParseState),
    %% node stores: type, request index, cond op + match val
    ParseState2 = xml_parse_graph:add_node(ParseState, 'language-switch', Conds),
    process_targets('language-switch', Targets, ParseState2);


parse_xml(#xmlElement{name = 'priority-switch', content = Content}, ParseState) ->
    {Conds, Targets} = get_cond(Content, 'priority-switch', no_args, ParseState),
    %% node stores: type, request index, cond op + match val
    ParseState2 = xml_parse_graph:add_node(ParseState, 'priority-switch', Conds),
    process_targets('priority-switch', Targets, ParseState2);


parse_xml(#xmlElement{name = 'string-switch', content = Content} = E, ParseState) ->
    %% 'display' is also a legal value in RFC 3880, but it's not supported by SIP
    Field = normalize_string_switch__field(get_attribute(E, field)),
    %% attribute field is mandatory so it can't be = '#no_value'
    xml_parse_util:legal_value(Field, [subject, organization, 'user-agent']),
    {Conds, Targets} = get_cond(Content, 'string-switch', no_args, ParseState),
    %% node stores: type, request index, cond op + match val
    ParseState2 = xml_parse_graph:add_node(ParseState, 'string-switch', Field, Conds),
    process_targets('string-switch', Targets, ParseState2);


parse_xml(#xmlElement{name = 'time-switch', content = Content} = E, ParseState) ->
    %%    If a script is uploaded with a "tzid" and "tzurl" which the CPL
    %%    server does not recognize or cannot resolve, it SHOULD diagnose and
    %%    reject this at script upload time.  If neither "tzid" nor "tzurl" are
    %%    present, all non-UTC times within this time switch should be
    %%    interpreted as being "floating" times, i.e., that they are specified
    %%    in the local time zone of the CPL server.
    %%
    %%       Because of daylight-savings-time changes over the course of a
    %%       year, it is necessary to specify time switches in a given
    %%       time zone.  UTC offsets are not sufficient, or a time-of-day
    %%       routing rule which held between 9 am and 5 pm in the eastern
    %%       United States would start holding between 8 am and 4 pm at the end
    %%       of October.
    %%    - RFC 3880

    %% XXX time zones are currently unsupported
    Tzid = get_attribute(E, tzid),
    Tzurl = get_attribute(E, tzurl),
    case {Tzid, Tzurl} of
	{'#no_value', '#no_value'} ->
	    {InitialConds, Targets} = get_cond(Content, 'time-switch', no_args, ParseState),

	    %% node stores: type, request index, cond op + match val
	    TimeZone = #time_zone{tzid = Tzid, tzurl = Tzurl},

	    %% check that no date-time or time values use leap seconds
	    validate_no_leap_second_in_datetime_values(InitialConds),
	    %% check that all dtstart < dtend values
	    validate_dtstart_dtend(TimeZone, InitialConds),
	    %% check that no intervals in reoccurrences can overlap
	    validate_duration(InitialConds),
	    %% check that dtstart, dtend and until date-time are in floating format
	    validate_usage_of_floating_time_with_byxxx(InitialConds),
	    %% check that bysetpos and lowest level of byxxx / freq can't generate to large selection sets
	    validate_usage_of_bysetpos(InitialConds),
	    %% preprocess "count" attribute in "time" tag
	    Conds = preprocess_count_in_timeswitch(TimeZone, InitialConds),

	    ParseState2 = xml_parse_graph:add_node(ParseState, 'time-switch', TimeZone, Conds),
	    process_targets('time-switch', Targets, ParseState2);
	_ ->
	    throw({error, time_switch_tag_tzid_and_tzurl_currently_unsupported})
    end;

%% --------------------- modifier

parse_xml(#xmlElement{name = location, content = Content} = E, ParseState) ->
    Location = case {get_attribute(E,url), get_attribute(E,priority), get_attribute(E,clear)} of
		   {'#no_value', _, _} ->
		       throw({error, location_tag_url_attribute_is_mandatory});
		   {URL, '#no_value', '#no_value'} ->
		       #location__attrs{url = check_url(URL)};
		   {URL, Prio, '#no_value'} ->
		       #location__attrs{url = check_url(URL), priority = check_prio_value(Prio)};
		   {URL, '#no_value', Clear} ->
		       #location__attrs{url = check_url(URL), clear = get_clear_value(Clear)};
		   {URL, Prio, Clear} ->
		       #location__attrs{url = check_url(URL), priority = check_prio_value(Prio),
					clear = get_clear_value(Clear)}
	       end,
    NextId = ParseState#parse_state.current_id ++ [1],
    ParseState2 = xml_parse_graph:add_node(ParseState, location, {Location, NextId}),
    ParseState3 = ParseState2#parse_state{current_id = NextId},
    parse_xml(get_next_element(location, Content), ParseState3);


parse_xml(#xmlElement{name = lookup, content = Content} = E, ParseState) ->
    Lookup = case {get_attribute(E,source), get_attribute(E,timeout), get_attribute(E,clear)} of
		 {'#no_value', _, _} ->
		     throw({error, lookup_tag_source_attribute_is_mandatory});
		 {Source, '#no_value', '#no_value'} ->
		     #lookup__attrs{source = Source};
		 {Source, Timeout, '#no_value'} ->
		     #lookup__attrs{source = Source, timeout = list_to_integer(Timeout)};
		 {Source, '#no_value', Clear} ->
		     #lookup__attrs{source = Source, clear = get_clear_value(Clear)};
		 {Source, Timeout, Clear} ->
		     #lookup__attrs{source = Source, timeout = list_to_integer(Timeout),
				    clear = get_clear_value(Clear)}
	     end,
    %% XXX currently only "registration" is supported, but URIs (http)
    %% are also supported (RFC 3880 chapter 5.2 p23)
    xml_parse_util:legal_value(Lookup#lookup__attrs.source, ["registration"]),
    {Conds, Targets} = get_cond(Content, lookup, no_args, ParseState),
    %% node stores: type, lookup index, cond ops
    ParseState2 = xml_parse_graph:add_node(ParseState, lookup, Lookup ,Conds),
    process_targets(lookup, Targets, ParseState2);


parse_xml(#xmlElement{name = 'remove-location', content = Content} = E, ParseState) ->
    RmLocation = case get_attribute(E, location) of
		     '#no_value' ->
			 #remove_location__attrs{};
		     URL ->
			 #remove_location__attrs{location = check_url(URL)}
		 end,
    NextId = ParseState#parse_state.current_id ++ [1],
    ParseState2 = xml_parse_graph:add_node(ParseState, 'remove-location', {RmLocation, NextId}),
    ParseState3 = ParseState2#parse_state{current_id = NextId},
    parse_xml(get_next_element('remove-location', Content), ParseState3);

%% --------------------- sub

parse_xml(#xmlElement{name = sub} = E, ParseState) ->
    NextId = case get_attribute(E, ref) of
		 '#no_value' ->
		     throw({error, sub_tag_is_missing_ref_attribute});
		 Ref ->
		     get_subaction_id(ParseState, Ref)
	     end,
    ParseState2 = xml_parse_graph:add_node(ParseState, sub, NextId),
    %% nothing more to parse in this branch, so return current parse state
    ParseState2;

%% --------------------- action

%% empty Comment strings are allowed
parse_xml(#xmlElement{name = log, content = Content} = E, ParseState) ->
    Log = case {get_attribute(E, name), get_attribute(E, comment)} of
	      {'#no_value', '#no_value'} ->
		  throw({error, log_tag_contained_no_name_or_comment_attribute});
	      {'#no_value', Comment} ->
		  #log__attrs{name = is_log_dest(default), comment = Comment};
	      {_Name, '#no_value'} ->
		  throw({error, log_tag_used_without_comment_attribute});
	      {Name, Comment} ->
		  #log__attrs{name = is_log_dest(Name), comment = Comment}
	  end,
    NextId = ParseState#parse_state.current_id ++ [1],
    ParseState2 = xml_parse_graph:add_node(ParseState, log, {Log, NextId}),
    ParseState3 = ParseState2#parse_state{current_id = NextId},
    parse_xml(get_next_element(log, Content), ParseState3);


%% XXX check for empty / incorrect email url (UrlStr) ?
parse_xml(#xmlElement{name = mail, content = Content} = E, ParseState) ->
    Mail = case get_attribute(E, url) of
	       '#no_value' ->
		   throw({error, mail_tag_contained_no_url_attribute});
	       UrlStr ->
		   UrlStr
	   end,
    NextId = ParseState#parse_state.current_id ++ [1],
    ParseState2 = xml_parse_graph:add_node(ParseState, mail, {Mail, NextId}),
    ParseState3 = ParseState2#parse_state{current_id = NextId},
    parse_xml(get_next_element(mail, Content), ParseState3);


parse_xml(#xmlElement{name = proxy, content = Content} = E, ParseState) ->
    Recurse = case get_attribute(E,recurse) of
		  '#no_value' -> yes; % default
		  "yes" -> yes;
		  "no" -> no;
		  _ -> throw({error, proxy_tag_recurse_attribute_set_to_a_non_legal_value})
	      end,
    Ordering = case get_attribute(E,ordering) of
		  '#no_value' -> parallel;
		  "parallel" -> parallel;
		  "sequential" -> sequential;
		  "first-only" -> 'first-only';
		   _ -> throw({error, proxy_tag_ordering_attribute_set_to_a_non_legal_value})
	      end,

    {Conds, Targets} = get_cond(Content, proxy, no_args, ParseState),

    %% Note that if the value of "recurse" is "yes", the "redirection"
    %% output to the script is never taken. -RFC 3880 chapter 6.1 p27
    case {lists:keysearch(redirection, 1, Conds), Recurse} of
	{false, _} -> ok;
	{_, no} -> ok;
	{_, yes} -> throw({error, proxy_tag_recurse_attribute_is_yes_can_not_be_used_with_sub_tag_redirect})
    end,
    %% If this parameter [timeout] is not specified, the default value is 20
    %% seconds if the "proxy" node has a "noanswer" or "default" output
    %% specified; otherwise the server SHOULD allow the call to ring for a
    %% reasonably long period of time (to the maximum extent that server
    %% policy allows).
    %% - RFC 3880 chapter 6.1 p27
    Timeout = case get_attribute(E,timeout) of
		  '#no_value' ->
		      case {lists:keymember(noanswear, 1, Conds), lists:keymember(default, 1, Conds)} of
			  {true, false} -> 20;
			  {false, true} -> 20;
			  {true, true} -> 20;
			  {false, false} -> server_max
		      end;
		  TimeoutStr ->
		      try
			  list_to_integer(TimeoutStr)
		      catch
			  %% run time error
			  error: _ -> throw({error, proxy_tag_timeout_attribute_not_a_number})
		      end
	      end,

    ProxyAttrs = #proxy__attrs{timeout = Timeout, recurse = Recurse, ordering = Ordering},

    %% node stores: type, lookup index, cond ops
    ParseState2 = xml_parse_graph:add_node(ParseState, proxy, ProxyAttrs ,Conds),
    process_targets(proxy, Targets, ParseState2);


parse_xml(#xmlElement{name = redirect} = E, ParseState) ->
    Permanent = case get_attribute(E, permanent) of
		    '#no_value' -> no; % default
		    "yes" -> yes;
		    "no" -> no;
		    _ -> throw({error, redirect_tag_permanent_attribute_set_to_a_non_legal_value})
	     end,
    ParseState2 = xml_parse_graph:add_node(ParseState, redirect, {Permanent, terminate}),
    %% nothing more to parse in this branch, so return current parse state
    ParseState2;


parse_xml(#xmlElement{name = reject} = E, ParseState) ->
    StatusReason =
	case {get_attribute(E, status), get_attribute(E, reason)} of
	    {'#no_value', _} ->
		throw({error, reject_tag_status_attribute_is_mandatory});
	    {Status, '#no_value'} ->
		#reject__attrs{status = xml_parse_util:status_code_to_sip_error_code(Status), reason = ""};
	    {Status, Reason} ->
		#reject__attrs{status = xml_parse_util:status_code_to_sip_error_code(Status), reason = Reason}
	end,
    ParseState2 = xml_parse_graph:add_node(ParseState, reject, {StatusReason, terminate}),
    %% nothing more to parse in this branch, so return current parse state
    ParseState2;

%% redundant, should never occure - as the current code checks its destination tag
parse_xml(_E, _ParseState) ->
    throw({error, unkown_cpl_tag_encountered}).

%%--------------------------------------------------------------------
%% @spec    (Conds) ->
%%            ok 
%%
%%            Conds = [{Cond, Dest}] "from CondVal in {CondVal, Targets} return value of get_cond/4"
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     the DATE-TIME elements used in CPL (RFC 3880)
%%          "time-switch" elements support the usage of leap seconds
%%          (see RFC 2445 chapter 4.3.12) i.e. times like 23:59:60.
%%          This is for various reasons - simplicity of CPL
%%          interpreter code and lack of proper erlang/OTP support,
%%          impractical to support as script input, this function is
%%          therefor used to reject any time values containing leap
%%          seconds.
%% @end
%%--------------------------------------------------------------------
validate_no_leap_second_in_datetime_values(Conds) ->
    F = fun({Cond, _Dest}) ->
		no_leap_sec(Cond)
	end,
    lists:foreach(F, Conds).

no_leap_sec(Cond) ->
    DTStart =
	case Cond of
	    _ when is_record(Cond, time_switch__cond_2) ->
		Cond#time_switch__cond_2.dtstart;
	    _ when is_record(Cond, time_switch__cond_5) ->
		Cond#time_switch__cond_5.dtstart;
	    _ when is_record(Cond, time_switch__cond_7) ->
		Cond#time_switch__cond_7.dtstart;
	    _ when is_record(Cond, time_switch__cond_8) ->
		Cond#time_switch__cond_8.dtstart;
	    _ ->
		not_time_switch_cond
	end,
    no_leap_sec_datetime(DTStart),
    DTEnd_Duration =
	case Cond of
	    _ when is_record(Cond, time_switch__cond_2) ->
		Cond#time_switch__cond_2.dtend_duration;
	    _ when is_record(Cond, time_switch__cond_5) ->
		Cond#time_switch__cond_5.dtend_duration;
	    _ when is_record(Cond, time_switch__cond_7) ->
		Cond#time_switch__cond_7.dtend_duration;
	    _ when is_record(Cond, time_switch__cond_8) ->
		Cond#time_switch__cond_8.dtend_duration;
	    _ ->
		not_time_switch_cond
	end,
    case DTEnd_Duration of
	{dtend, DTEnd} ->
	    no_leap_sec_datetime(DTEnd);
	{duration, _} ->
	    ok;
	not_time_switch_cond ->
	    ok
    end,
    Until_Count =
	case Cond of
	    _ when is_record(Cond, time_switch__cond_2) ->
		undef;
	    _ when is_record(Cond, time_switch__cond_5) ->
		Cond#time_switch__cond_5.until_count;
	    _ when is_record(Cond, time_switch__cond_7) ->
		Cond#time_switch__cond_7.until_count;
	    _ when is_record(Cond, time_switch__cond_8) ->
		Cond#time_switch__cond_8.until_count;
	    _ ->
		not_time_switch_cond
	end,
    case Until_Count of
	{until, Until} -> case Until of
			      _ when is_record(Until, date_time) ->
				  no_leap_sec_datetime(Until);
			      {_Year, _Month, _Day} ->
				  ok
			  end;
	{count, _Count} ->
	    ok;
	repeat_forever ->
	    ok;
	undef ->
	    ok;
	not_time_switch_cond ->
	    ok
    end.

no_leap_sec_datetime(not_time_switch_cond) ->
    ok;
no_leap_sec_datetime(DateTime) when is_record(DateTime, date_time) ->
    #date_time{time = {_H,_M,S}} = DateTime,
    case S of
	60 -> throw({error, leap_second_input_not_supported});
	_ when S >= 0, S =< 59 -> ok
    end.

%%--------------------------------------------------------------------
%% @spec    (TimeZone, Conds) ->
%%            ok 
%%
%%            TimeZone = term() "currently not supported"
%%            Conds    = [{Cond, Dest}] "from CondVal in {CondVal, Targets} return value of get_cond/4"
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     examine all "time" elements in a "time-witch" with
%%          ts_datetime:dtstart_lt_dtend/3, to see if all dtstart -
%%          dtend pairs have the property; dtstart `<' dtend. Throw a
%%          exception if they don't conform. Note : see cpl/README
%%          about date-time format limitations, in regard to floating
%%          date-time values without time zone settings.
%% @end
%%--------------------------------------------------------------------
validate_dtstart_dtend(TimeZone, Conds) ->
    Ranges = get_dtstart_and_dtend(Conds),
    F = fun({S,E}) ->
		ts_datetime:dtstart_lt_dtend(TimeZone, S, E)
	end,
    case Ranges of
	[] -> ok;
	_ ->
	    case lists:all(F, Ranges) of
		true -> ok;
		false -> throw({error, dtstart_not_less_than_dtend_attribute_in_time_switch_tag})
	    end
    end.

get_dtstart_and_dtend(Conds) ->
    F = fun({Cond, _Dest}, Acc) when is_record(Cond, time_switch__cond_2) ->
		DTStart = Cond#time_switch__cond_2.dtstart,
		case Cond#time_switch__cond_2.dtend_duration of
		    {dtend, Time} -> [{DTStart, Time} | Acc];
		    {duration, _} -> Acc
		end;
	   ({Cond, _Dest}, Acc) when is_record(Cond, time_switch__cond_5) ->
		DTStart = Cond#time_switch__cond_5.dtstart,
		case Cond#time_switch__cond_5.dtend_duration of
		    {dtend, Time} -> [{DTStart, Time} | Acc];
		    {duration, _} -> Acc
		end;
	   ({Cond, _Dest}, Acc) when is_record(Cond, time_switch__cond_7) ->
		DTStart = Cond#time_switch__cond_7.dtstart,
		case Cond#time_switch__cond_7.dtend_duration of
		    {dtend, Time} -> [{DTStart, Time} | Acc];
		    {duration, _} -> Acc
		end;
	   ({Cond, _Dest}, Acc) when is_record(Cond, time_switch__cond_8) ->
		DTStart = Cond#time_switch__cond_8.dtstart,
		case Cond#time_switch__cond_8.dtend_duration of
		    {dtend, Time} -> [{DTStart, Time} | Acc];
		    {duration, _} -> Acc
		end;
	   (_, Acc) ->
		Acc
	end,
    lists:foldl(F, [], Conds).

%%--------------------------------------------------------------------
%% @spec    (Conds) ->
%%            ok 
%%
%%            Conds = [{Cond, Dest}] "from CondVal in {CondVal, Targets} return value of get_cond/4"
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     checks that all durations / "dtend - dtstart" periods are
%%          short enough to never overlap
%% @end
%%--------------------------------------------------------------------
validate_duration(Conds) ->
    F = fun({Cond, _Dest}) when is_record(Cond, time_switch__cond_2) ->
		ts_duration:valid_duration(Cond);
	   ({Cond, _Dest}) when is_record(Cond, time_switch__cond_5) ->
		ts_duration:valid_duration(Cond);
	   ({Cond, _Dest}) when is_record(Cond, time_switch__cond_7) ->
		ts_duration:valid_duration(Cond);
	   ({Cond, _Dest}) when is_record(Cond, time_switch__cond_8) ->
		ts_duration:valid_duration(Cond);
	   (_)  ->
		true
	end,
    case lists:all(F, Conds) of
	true ->
	    ok;
	false ->
	    throw({error, duration_too_long})
    end.

%%--------------------------------------------------------------------
%% @spec    (InitialConds) ->
%%            ok 
%%
%%            Conds = [{Cond, Dest}] "from CondVal in {CondVal, Targets} return value of get_cond/4"
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     check that dtstart and date-time values are in floating
%%          format if byxxx parameters are used in a time tag as
%%          specified by iCalendar; "When used with a recurrence
%%          rule, the 'DTSTART' and 'DTEND' properties MUST be
%%          specified in local time ..." - RFC 2445 chapter 4.8.5.4
%%          page 117. The this constraint is mainly to make
%%          calculations of reoccurring periods with dtstart as
%%          offset unambiguous, as the byxxx parameters processes
%%          floating (local) wall clock time and use dtstart
%%          date-time to initiate undefined time values in their
%%          reoccurrence calculations Note : iCalendar require this
%%          for _all_ reoccurrences and for both dtstart and dtend,
%%          but this implementation only requires this for dtstart
%%          used with byxxx parameters
%% @end
%%--------------------------------------------------------------------
validate_usage_of_floating_time_with_byxxx(Conds) ->
    F = fun({Cond, _Dest}) when is_record(Cond, time_switch__cond_7) ->
		all_floating(Cond);
	   ({Cond, _Dest}) when is_record(Cond, time_switch__cond_8) ->
		all_floating(Cond);
	   (_)  ->
		true
	end,
    case lists:all(F, Conds) of
	true ->
	    ok;
	false ->
	    throw({error, time_tag_using_byxxx_can_not_use_utc_date_time_values_in_dtstart})
    end.

is_floating(DateTime) when is_record(DateTime, date_time) ->
    case DateTime#date_time.type of
	floating -> true;
	utc -> false
    end.

all_floating(TimeSwitchCond) ->
    DtStart = time_switch:get_dtstart(TimeSwitchCond),
    is_floating(DtStart).


%%--------------------------------------------------------------------
%% @spec    (InitialConds) ->
%%            ok 
%%
%%            Conds = [{Cond, Dest}] "from CondVal in {CondVal, Targets} return value of get_cond/4"
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
validate_usage_of_bysetpos(Conds) ->
     F = fun({Cond, _Dest}) when is_record(Cond, time_switch__cond_8) ->
		 interpret_time:is_bysetpos_usable(Cond);
	    (_)  ->
		 true
	 end,
    case lists:all(F, Conds) of
	true ->
	    ok;
	false ->
	    throw({error, bysetpos_combined_with_lowest_byxxx_or_freq_yields_to_large_selection_set})
    end.


%%--------------------------------------------------------------------
%% @spec    (TimeZone, Conds) ->
%%            NewList
%%
%%            TimeZone = term() "currently not supported"
%%            Conds    = [{Cond, Dest}] "from CondVal in {CondVal, Targets} return value of get_cond/4"
%%
%%            NewList = [{Cond, Dest}] "an updated version of Conds"
%%
%% @doc     check if a "time-switch" tag contains the "count"
%%          attribute in any of it's "time" conditions, if it does -
%%          interpret_time:get_count_ranges_X/2 will be used to
%%          calculate all the intervals as specified. This would
%%          otherwise need to be done each time a "count" is
%%          processed by the interpreter which is a O(N) procedure -
%%          all possible intervals between dtstart - current need to
%%          be checked.
%% @end
%%--------------------------------------------------------------------
preprocess_count_in_timeswitch(TimeZone, Conds) ->
    F = fun({Cond, Dest}) when is_record(Cond, time_switch__cond_5) ->
		case Cond#time_switch__cond_5.until_count of
		    {count, _} ->
			NewCond = Cond#time_switch__cond_5{time_ranges =
							  interpret_time:get_count_ranges_5(TimeZone, Cond)},
			{NewCond, Dest};
		    _ ->
			{Cond, Dest}
		end;
 	   ({Cond, Dest}) when is_record(Cond, time_switch__cond_7) ->
 		case Cond#time_switch__cond_7.until_count of
 		    {count, _} ->
 			NewCond = Cond#time_switch__cond_7{time_ranges =
 							  interpret_time:get_count_ranges_7(TimeZone, Cond)},
 			{NewCond, Dest};
 		    _ ->
 			{Cond, Dest}
 		end;
 	   ({Cond, Dest}) when is_record(Cond, time_switch__cond_8) ->
		case Cond#time_switch__cond_8.until_count of
		    {count, _} ->
			NewCond = Cond#time_switch__cond_8{time_ranges =
							   interpret_time:get_count_ranges_8(TimeZone, Cond)},
			{NewCond, Dest};
		    _ ->
			{Cond, Dest}
		end;
	   %% ignore non-count time-switch elements
	   ({Cond, Dest})  ->
		{Cond, Dest}
	end,
    lists:map(F, Conds).

%%--------------------------------------------------------------------
%% @spec    (ParentSwitchName, Targets, ParseState) -> #parse_state{}
%%
%%            ParentSwitchName = term()
%%            Targets          = term() "a list of #xmlElement{} contained inside a switch tag"
%%            ParseState       = #parse_state{}
%%
%% @doc     This function takes Targets - the xml code for the
%%          possible action a certain xml rule (graph node) can take,
%%          and parses them into nodes. ParseState contains the
%%          currently parsed graph, the return value will contain
%%          ParseState + nodes found in Targets.
%% @end
%%--------------------------------------------------------------------
process_targets(ParentSwitchName, Targets, ParseState) ->
    Id = ParseState#parse_state.current_id,
    %% process the destination nodes
    F = fun(Target, {PState, Index}) ->
		NewId = Id ++[Index],
		PState2 = PState#parse_state{current_id = NewId},
		NewPState = parse_xml(get_next_element(ParentSwitchName, Target#xmlElement.content), PState2),
		{NewPState, Index + 1}
	end,
    {FinalParseState, _} = lists:foldl(F, {ParseState, 1}, Targets),
    FinalParseState.

%%--------------------------------------------------------------------
%% @spec    (ParentTagName, Content) ->
%%            #xmlElement{} |
%%            empty               
%%
%%            ParentTagName = atom()
%%            Content       = ParentContent | SubTagContent
%%            ParentContent = term() "xml parse tree inside parent"
%%            SubTagContent = term() "xml parse tree inside parents (switch) sub tag"
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     return the next node (tag) for a tag type that has a
%%          single destination - SubTagContent of sub tags of
%%          switches are also handled by this function Note :
%%          ParentTagName are listed one by one, to simplify handling
%%          them individually - there could be cases where extension
%%          tags don't support the same destination node (tag) set
%% @end
%%--------------------------------------------------------------------
get_next_element(Action, Content) ->
    case Action of
	subaction ->
	    get_next_element(Content);
	incoming ->
	    get_next_element(Content);
	outgoing ->
	    get_next_element(Content);
	location ->
	    get_next_element(Content);
	'remove-location' ->
	    get_next_element(Content);
	log ->
	    get_next_element(Content);
	mail ->
	    get_next_element(Content);

    %% check content of a switch sub tag

	'address-switch' ->
	    get_next_switch_element(Content);
	'language-switch' ->
	    get_next_switch_element(Content);
	'priority-switch' ->
	    get_next_switch_element(Content);
	'string-switch' ->
	    get_next_switch_element(Content);
	'time-switch' ->
	    get_next_switch_element(Content);
	lookup ->
	    get_next_switch_element(Content);
	proxy ->
	    get_next_switch_element(Content);
	_ ->
	    throw({error, tag_is_not_a_single_destination_node})
    end.

get_next_element(ParentContent) ->
    %% ignore non xmlElement parse data
    %% XXX xmlText is probably the only parse data that should be ignored
    Elements = [E || E <- ParentContent, is_record(E, xmlElement)],
    case Elements of
	[] ->
	    empty;
	[Element] ->
	    %% check that destination is legal, some nodes like incoming,
	    %% subaction, cpl and outgoing are not allowed as sub tags (destination nodes)
	    NextTagType = Element#xmlElement.name,
	    case lists:member(NextTagType,
			       ['address-switch', 'language-switch', 'priority-switch', 'string-switch',
				'time-switch', location, lookup, 'remove-location', sub, log, mail, proxy,
				redirect, reject]
			      ) of
		true -> Element;
		false -> throw({tag_contains_ilegal_tag})
	    end;
	_ ->
	    throw({error, tag_can_only_contain_a_single_tag})
    end.


get_next_switch_element(SubTagContent) ->
    get_next_element(SubTagContent).

%%--------------------------------------------------------------------
%% @spec    (Element, AttrName) -> '#no_value' | string()
%%
%%            Element  = #xmlElement{}
%%            AttrName = atom()
%%
%% @doc     return the value of a attribute in a xml tag, e.g.
%%          get_attribute(E,bar) in <foo bar="..."> ... </foo> Note :
%%          xmlElement attributes can be = IOlist() (see erlang
%%          module OTP docs in R10B) | atom() | integer()
%% @end
%%--------------------------------------------------------------------
get_attribute(Element, AttrName) when is_record(Element, xmlElement), is_atom(AttrName) ->
    Attrs = Element#xmlElement.attributes,
    case lists:keysearch(AttrName, #xmlAttribute.name, Attrs) of
	{value, Attr} ->
	    Val = Attr#xmlAttribute.value,
	    if
	       is_atom(Val) -> atom_to_list(Val);
	       is_integer(Val) -> integer_to_list(Val);
	       is_list(Val) -> xml_parse_util:iolist_to_str(Val)
	    end;
	false ->
	    '#no_value'
    end.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
attribute_to_atom("address-type") -> 'address-type';
attribute_to_atom("user") -> user;
attribute_to_atom("host") -> host;
attribute_to_atom("port") -> port;
attribute_to_atom("tel") -> tel;
attribute_to_atom("display") -> display;
attribute_to_atom("password") -> password;
attribute_to_atom("origin") -> origin;
attribute_to_atom("destination") -> destination;
attribute_to_atom("original-destination") -> 'original-destination';
attribute_to_atom('#no_value') -> '#no_value'.

%%--------------------------------------------------------------------
%% @spec    (Element, SubElementName) -> [#xmlElement{}]
%%
%%            Element        = #xmlElement{}
%%            SubElementName = atom() "name of the tag/s contained in Element"
%%
%% @doc     retrieve all xml elements named SubElementName from the
%%          contents of Element (for example a switch condition for a
%%          address-switch tag)
%% @end
%%--------------------------------------------------------------------
get_elements(Element, SubElementName) when is_record(Element, xmlElement), is_atom(SubElementName) ->
    Elems = Element#xmlElement.content,
    [E || E <- Elems, is_record(E, xmlElement), E#xmlElement.name == SubElementName].

%%--------------------------------------------------------------------
%% @spec    (ParseState, Ref) ->
%%            NodeId 
%%
%%            ParseState = #parse_state{}
%%            Ref        = string() "the symbolic name of a subaction used in a ``<sub ref ...>'' tag"
%%
%%            NodeId = term()
%%            Reason = integer()
%%
%% @throws  {error, Reason} 
%%
%% @doc     find the node id of the subaction named Ref
%% @end
%%--------------------------------------------------------------------
get_subaction_id(ParseState, Ref) ->
    Mapping = ParseState#parse_state.subaction_name_id_mapping,

    case lists:keysearch(Ref, 1, Mapping) of
	{value, {_, NextId}} ->
	    NextId;
	_ ->
	    throw({error, sub_tag_ref_attribute_referenced_unkown_or_later_defined_subaction})
    end.

%%--------------------------------------------------------------------
%% @spec    (Conditions, SwitchName, ExtraArgs, ParseState) ->
%%            {CondVal, Targets}
%%
%%            Conditions = term() "#xmlElement.content"
%%            SwitchName = 'address-switch'  |
%%                         'language-switch' |
%%                         'priority-switch' |
%%                         'string-switch'   |
%%                         'time-switch'     |
%%                         lookup            |
%%                         proxy
%%            ExtraArgs  = term() "includes any other switch specific arguments"
%%            ParseState = #parse_state{}
%%
%%            Targets = #xmlElement{} "from Conditions - the process_targets/2 call does the checking of"
%%            CondVal = term() "value returned"
%%
%% @doc     retrieve the match operator and value (to compare request
%%          against), as well as the destination of a successful
%%          match. Note : CondVal depends on the switch type
%%          (SwitchName), as seen below:
%%          * address-switch CondVal = list() of {{address__is, Val},
%%          Dest} | {{address__contains, Val}, Dest} |
%%          {{'address__subdomain-of', Val}, Dest} | {'not-present',
%%          Dest} | {otherwise, Dest} * language-switch CondVal =
%%          list() of {{language__matches, Val}, Dest} |
%%          {'not-present', Dest} | {otherwise, Dest} *
%%          priority-switch CondVal = list() of {{priority__less,
%%          Val}, Dest} | {{priority__greater, Val}, Dest} |
%%          {{priority__equal, Val}, Dest} | {otherwise, Dest} *
%%          string-switch CondVal = list() of {{string__is, Val},
%%          Dest} | {{string__contains, Val}, Dest} | {'not-present',
%%          Dest} | {otherwise, Dest} * time-switch CondVal = list()
%%          of {time_switch__cond_8 record(), Dest} |
%%          {time_switch__cond_7 record(), Dest} |
%%          {time_switch__cond_5 record(), Dest} |
%%          {time_switch__cond_4 record(), Dest} |
%%          {time_switch__cond_2 record(), Dest} | {otherwise, Dest}
%%          * lookup CondVal = list() of {success, Dest} | {notfound,
%%          Dest} | {failure, Dest} * proxy CondVal = list() of
%%          {busy, Dest} | {noanswer, Dest} | {redirection, Dest} |
%%          {failure, Dest} | {default, Dest}
%% @end
%%--------------------------------------------------------------------
get_cond(Conditions, SwitchName, ExtraArgs, ParseState) ->
    {Conds, _Targets} = Res = get_cond(Conditions, SwitchName, ExtraArgs, ParseState, 1, {[],[]}),
    is_otherwise_cond_last(Conds),
    check_for_duplicates(SwitchName, Conds),
    Res.

%% "The output "otherwise", which MUST be the last output specified if it
%%  is present" - RFC 3880 chapter 4 p8
%% throw a error if a otherwise tag is found and it isn't the last element in Conds
is_otherwise_cond_last([]) ->
    ok;
is_otherwise_cond_last([{otherwise, _Dest}, _E | _R]) ->
    throw({error, otherwise_sub_tag_is_not_last_in_switch_tag});
is_otherwise_cond_last([_E | R]) ->
    is_otherwise_cond_last(R).

%% throw a exception if a switch tag (multiple destination tag), contains
%% multiple instances of a sub tag, that should only occur once in the
%% switch - e.g. "<proxy> <busy/> <busy/> </proxy>"
check_for_duplicates(SwitchName, Conds) ->
    SubTags = case SwitchName of
		  proxy -> [busy, noanswer, redirection, failure, default];
		  lookup -> [success,notfound, failure];
		  'time-switch' -> [otherwise];
		  'string-switch' -> ['not-present', otherwise];
		  'priority-switch' -> [otherwise];
		  'language-switch' -> ['not-present', otherwise];
		  'address-switch' -> ['not-present', otherwise]
	      end,
    F = fun(SubTag, Acc) ->
		(not is_duplicate(SubTag, Conds)) and Acc
	end,
    Res = lists:foldl(F, true, SubTags),
    case Res of
	true -> ok;
	false -> throw({error, switch_tag_contained_multiple_instances_of_a_tag_that_should_only_occur_once})
    end.

%% is_duplicate(SubTag, Conds)
is_duplicate(SubTag, Conds) ->
    count_subtags(SubTag, Conds) > 1.

%% only implements counting for SubTags that are unique
count_subtags(SubTag, Conds) ->
    count_subtags(SubTag, Conds, 0).

count_subtags(_SubTag, [], Count) ->
    Count;
count_subtags(busy, [{busy,_}|R], Count) ->
    count_subtags(busy, R, Count + 1);
count_subtags(noanswer, [{noanswer,_}|R], Count) ->
    count_subtags(noanswer, R, Count + 1);
count_subtags(redirection, [{redirection,_}|R], Count) ->
    count_subtags(redirection, R, Count + 1);
count_subtags(failure, [{failure,_}|R], Count) ->
    count_subtags(failure, R, Count + 1);
count_subtags(default, [{default,_}|R], Count) ->
    count_subtags(default, R, Count + 1);
count_subtags(success, [{success,_}|R], Count) ->
    count_subtags(success, R, Count + 1);
count_subtags(notfound, [{notfound,_}|R], Count) ->
    count_subtags(notfound, R, Count + 1);
count_subtags(otherwise, [{otherwise,_}|R], Count) ->
    count_subtags(otherwise, R, Count + 1);
count_subtags('not-present', [{'not-present',_}|R], Count) ->
    count_subtags('not-present', R, Count + 1);
%% don't count non-SubTag entries in Conds
count_subtags(SubTag, [_|R], Count) ->
    count_subtags(SubTag, R, Count).

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_cond([], _SwitchName, _ExtraArgs, _ParseState, _Count, {Conds, Targets}) ->
    {lists:reverse(Conds), lists:reverse(Targets)};


get_cond([#xmlElement{name = address} = Cond | R],
	 'address-switch' = SwitchName, ExtraArgs, ParseState, Count, {Conds,Targets})
when is_record(Cond, xmlElement)->
    CondVal = case {get_attribute(Cond, is),
		    get_attribute(Cond, contains),
		    get_attribute(Cond, 'subdomain-of')} of
		  {'#no_value', '#no_value', '#no_value'} ->
		      throw({error, address_tag_without_expected_attribute});
		  {IS, '#no_value', '#no_value'} ->
		      {address__is, IS};
		  {'#no_value', Contains, '#no_value'} when ExtraArgs == display ->
		      {address__contains, Contains};
		  {'#no_value', '#no_value', SubDomainOf} when ExtraArgs == tel; ExtraArgs == host ->
		      {'address__subdomain-of', SubDomainOf};
		  _ ->
		      throw({error, address_tag_subdomain_attribute_can_only_be_used_when_adress_switch_subfield_attribute_is_tel_or_host})
	      end,
    NewCT = {[{CondVal, ParseState#parse_state.current_id ++ [Count]} | Conds],
	     [Cond | Targets]},
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, NewCT);


get_cond([#xmlElement{name = language} = Cond | R],
	 'language-switch' = SwitchName, ExtraArgs, ParseState, Count, {Conds, Targets})
  when is_record(Cond, xmlElement)->
    CondVal = case get_attribute(Cond, matches) of
		  '#no_value' ->
		      throw({error, langauge_tag_without_matches_attribute});
		  Val ->
		      {language__matches, xml_parse_util:is_language_tag(Val)}
	      end,
    NewCT = {[{CondVal, ParseState#parse_state.current_id ++ [Count]} | Conds],
	     [Cond | Targets]},
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, NewCT);


get_cond([#xmlElement{name = priority} = Cond | R],
	 'priority-switch' = SwitchName, ExtraArgs, ParseState, Count, {Conds, Targets})
  when is_record(Cond, xmlElement) ->
    CondVal = case {get_attribute(Cond, less),
		    get_attribute(Cond, greater),
		    get_attribute(Cond, equal) }
		    of
		  {'#no_value', '#no_value', '#no_value'} ->
		      throw({error, priority_tag_without_expected_attribute});
		  {Less, '#no_value', '#no_value'} ->
		      {priority__less, xml_parse_util:normalize_prio(Less)};
		  {'#no_value', Greater, '#no_value'} ->
		      {priority__greater, xml_parse_util:normalize_prio(Greater)};
		  {'#no_value', '#no_value', Equal} ->
		      {priority__equal, xml_parse_util:normalize_prio(Equal)}
	      end,
    NewCT = {[{CondVal, ParseState#parse_state.current_id ++ [Count]} | Conds],
	     [Cond | Targets]},
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, NewCT);


get_cond([#xmlElement{name = string} = Cond | R],
	 'string-switch' = SwitchName, ExtraArgs, ParseState, Count, {Conds, Targets})
  when is_record(Cond, xmlElement)->
    CondVal = case {get_attribute(Cond, is),
		    get_attribute(Cond, contains)} of
		  {'#no_value', '#no_value'} ->
		      throw({error, string_tag_without_expected_attribute});
		  {Is, '#no_value'} ->
		      {string__is, Is};
		  {'#no_value', Contains} ->
		      {string__contains, Contains}
	      end,
    NewCT = {[{CondVal, ParseState#parse_state.current_id ++ [Count]} | Conds],
	     [Cond | Targets]},
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, NewCT);


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_cond([#xmlElement{name = time} = Cond | R],
	 'time-switch' = SwitchName, ExtraArgs, ParseState, IndexCount, {Conds, Targets})
  when is_record(Cond, xmlElement)->

    Dtstart = case get_attribute(Cond, dtstart) of
		  '#no_value' ->
		      throw({error, time_tag_no_dtstart_attribute_supplied});
		  Res ->
		      DateTime = xml_parse_util:time(Res),
		      bound_dtstart(DateTime),
		      DateTime
	     end,

    Dtend_Duration =
	case {get_attribute(Cond, dtend), get_attribute(Cond, duration)} of
	    {'#no_value', '#no_value' } ->
		throw({error, time_tag_no_duration_or_dtend_attribute_supplied});
	    {Dtend, '#no_value' } ->
		{dtend, xml_parse_util:time(Dtend)};
	    {'#no_value', Duration } ->
		{duration, xml_parse_util:duration(Duration)};
	    {_Dtend, _Duration } ->
		throw({error, time_tag_both_duration_and_dtend_attribute_supplied})
	end,

    %% may not need to be calculated here, but simplifies code logic
    Until_Count = case {get_attribute(Cond, until), get_attribute(Cond, count)} of
		      {'#no_value', '#no_value'} ->
			  repeat_forever;
		      {Until, '#no_value'} ->
			  UntilTime = xml_parse_util:parse_until(Until),
			  {until, UntilTime};
		     {'#no_value', Count} ->
			  CountVal = list_to_integer(Count),
			  case CountVal >= 1 of
			      true ->
				  bound_count(CountVal),
				  {count, CountVal};
			      false ->
				  throw({error, time_tag_count_attribute_is_zero_or_less})
			  end;
		     {_Until, _Count} ->
			  throw({error, time_tag_both_until_and_count_attribute_supplied})
		  end,

    Interval = case get_attribute(Cond, interval) of
		   '#no_value' ->
		       1;
		   Val ->
		       list_to_integer(Val)
	       end,

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% Notes on usage of records based on which attributes the time tag
%% includes:
%%
%% dtstart   - required
%% dtend or duration - required (but not both)
%% freq     - all parameters mentioned later require freq to be defined
%% --------------------------------------------
%% interval - always include in record (it has the default value 1)
%% until or count - cant be used at the same time (existence of these
%%            attributes imply a delimited time period, interval alone
%%            result in a infinite repeat)
%% --------------------------------------------
%% byxxx    - optional, record contains this field if there is at least
%%            one byxxx attribute used
%% wkst     - always include this in record (if there is a byxxx attr)
%%            as it has a default value ('mo')
%% bysetpos - only included in record if byxxx values are used - as it
%%            works on byxxx attributes
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    %% "Parameters other than "dtstart", "dtend", and "duration" SHOULD NOT
    %%  be specified unless "freq" is present, though CPL servers SHOULD
    %%  accept scripts with such parameters present, and ignore the other
    %%  parameters." - RFC 3880 chapter 4.4 p16
    CondVal = case get_attribute(Cond, freq) of
		  '#no_value' ->
		      %% {time, Dtstart, Dtend_Duration};
		      #time_switch__cond_2{ dtstart = Dtstart,
					    dtend_duration = Dtend_Duration
					   };
		  FreqStr ->
		      Freq = freq_str_to_atom(FreqStr),

		      case get_by_values(Cond) of
			  [] ->
			      %% {time, Dtstart, Dtend_Duration, Freq, Interval, Until_Count};
			      #time_switch__cond_5{ dtstart = Dtstart,
						    dtend_duration = Dtend_Duration,
						    freq = Freq,
						    interval = Interval,
						    until_count = Until_Count
						   };
			  ByValues ->
			      Wkst = first_work_day(Cond),
			      case get_attribute(Cond, bysetpos) of
				  '#no_value' ->
				      %% {time, Dtstart, Dtend_Duration, Freq,
				      %% Interval, Until_Count, Wkst};
				      #time_switch__cond_7{ dtstart = Dtstart,
							    dtend_duration = Dtend_Duration,
							    freq = Freq,
							    interval = Interval,
							    until_count = Until_Count,
							    by_values = ByValues,
							    wkst  = Wkst
							   };
				  BysetposStr ->
				      Bysetpos = parse_bysetpos(BysetposStr),
				      %% {time, Dtstart, Dtend_Duration, Freq,
				      %% Interval, Until_Count, Wkst, Bysetpos}
				      #time_switch__cond_8{ dtstart = Dtstart,
							    dtend_duration = Dtend_Duration,
							    freq = Freq,
							    interval = Interval,
							    until_count = Until_Count,
							    by_values = ByValues,
							    wkst = Wkst,
							    bysetpos = Bysetpos
							   }
			      end
		      end
	      end,

    NewCT = {[{CondVal, ParseState#parse_state.current_id ++ [IndexCount]} | Conds],
	     [Cond | Targets]},
    get_cond(R, SwitchName, ExtraArgs, ParseState, IndexCount + 1, NewCT);
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_cond([#xmlElement{name = Name} = Cond | R],
	 lookup = SwitchName, ExtraArgs, ParseState, Count, {Conds, Targets})
  when is_record(Cond, xmlElement)->
    case lists:member(Name, [success, notfound, failure]) of
	true ->
	    NewCT = {[{Name, ParseState#parse_state.current_id ++ [Count]} | Conds],
		     [Cond | Targets]},
	    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, NewCT);
	false ->
	    throw({error, lookup_tag_has_ilegal_condition})
    end;

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_cond([#xmlElement{name = Name} = Cond | R],
	 proxy = SwitchName, ExtraArgs, ParseState, Count, {Conds, Targets})
  when is_record(Cond, xmlElement)->
    case lists:member(Name, [busy, noanswer, redirection, failure, default]) of
	true ->
	    NewCT = {[{Name, ParseState#parse_state.current_id ++ [Count]} | Conds],
		     [Cond | Targets]},
	    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, NewCT);
	false ->
	    throw({error, proxy_tag_has_ilegal_condition})
    end;

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%% 'not-present' will never be matched in a 'time-switch', so don't add it to the graph
get_cond([#xmlElement{name = 'not-present'} = Cond | R],
	 'time-switch' = SwitchName, ExtraArgs, ParseState, Count, CT)
  when is_record(Cond, xmlElement)->
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count, CT);

%% 'not-present' will never be matched in a 'priority-switch', so don't add it to the graph
get_cond([#xmlElement{name = 'not-present'} = Cond | R],
	 'priority-switch' = SwitchName, ExtraArgs, ParseState, Count, CT)
  when is_record(Cond, xmlElement)->
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, CT);

%% all other switches use not-present
get_cond([#xmlElement{name = 'not-present'} = Cond | R],
	 SwitchName, ExtraArgs, ParseState, Count, {Conds, Targets})
  when is_record(Cond, xmlElement)->
    NewCT = {[{'not-present', ParseState#parse_state.current_id ++ [Count]} |  Conds],
	     [Cond | Targets]},
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, NewCT);


get_cond([#xmlElement{name = otherwise} = Cond | R],
	 SwitchName, ExtraArgs, ParseState, Count, {Conds, Targets})
  when is_record(Cond, xmlElement)->
    NewCT = {[{otherwise, ParseState#parse_state.current_id ++ [Count]} | Conds],
	     [Cond | Targets]},
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count + 1, NewCT);

%% ignore (Cond) tags that aren't xmlElement record()
%% XXX this filter should propably only ignore xmlText record() and yield a error otherwise
get_cond([_Cond | R], SwitchName, ExtraArgs, ParseState, Count, CT) ->
    get_cond(R, SwitchName, ExtraArgs, ParseState, Count, CT).

%%--------------------------------------------------------------------
%% @spec    (Start) ->
%%            ok 
%%
%%            Start = #date_time{}
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     limit the minimum that dtstart can be set to, this is done
%%          to guard against limitations in the (OTP) calender module
%%          - "Date must refer to a local date after Jan 1, 1970"
%% @end
%%--------------------------------------------------------------------
bound_dtstart(Start) when is_record(Start, date_time)->
    %% 1970-01-03 is used to ensure that Start will always be a legal
    %% utc value handled properly by OTP
    case {Start#date_time.date, Start#date_time.time} >= {{1970,1,3}, {0,0,0}} of
	true -> ok;
	false -> throw({error, dtstart_attribute_in_time_sub_tag_in_time_switch_set_to_a_to_early_date_time})
    end.

%%--------------------------------------------------------------------
%% @spec    (CountVal) ->
%%            ok 
%%
%%            CountVal = integer() ">= 1"
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     to limit storage space used by
%%          #time_switch__cond_x.time_ranges when storing scripts, as
%%          well as to some extent, limit CPU load when processing
%%          sip request with interpret_time.erl - this functions
%%          limits the maximum value for the "count" attribute in the
%%          "time" subtag used with "time-switch"
%% @end
%%--------------------------------------------------------------------
bound_count(CountVal) ->
    {ok, ConfiguredMax} = yxa_config:get_env(cpl_time_switch_count_max),
    case CountVal > ConfiguredMax of
	true ->
	    throw({error, count_in_time_sub_tag_in_time_switch_tag_set_to_high});
	false ->
	    ok
    end.

%%--------------------------------------------------------------------
%% return list of keyed values; {AttrName, Value}
get_by_values(Cond) ->
    AttrList =
	%% remove duplicates
	lists:usort(
	  get_by_attribute(Cond, bysecond, fun(Val) -> parse_by_range(Val, {0,59}) end) ++
	  get_by_attribute(Cond, byminute, fun(Val) -> parse_by_range(Val, {0,59}) end) ++
	  get_by_attribute(Cond, byhour, fun(Val) -> parse_by_range(Val, {0,23}) end) ++
	  get_by_attribute(Cond, byday, fun(Val) -> xml_parse_util:parse_byday(Val) end) ++
	  get_by_attribute(Cond, bymonthday, fun(Val) -> parse_by_range(Val, [{1,31}, {-1,-31}]) end) ++
	  get_by_attribute(Cond, byyearday, fun(Val) -> parse_by_range(Val, [{1,366}, {-1,-366}]) end) ++
	  get_by_attribute(Cond, byweekno, fun(Val) -> parse_by_range(Val, [{1,53}, {-1,-53}]) end) ++
	  get_by_attribute(Cond, bymonth, fun(Val) -> parse_by_range(Val, {1,12}) end)
	 ),
    unfold_by_values(AttrList).

%% convert list of [{Attr, ValList}, ...] to list of [{Attr, Val}, ...]
unfold_by_values(AttrList) ->
    F = fun({Attr,Vals}, Acc) ->
		[{Attr,Val} || Val <- Vals] ++ Acc
	end,
    lists:foldl(F,[],AttrList).

%% return : [] | [{AttrName, Val}]
%%          AttrNamn = atom(), name of a byxxx field in a time tag (used by time-switch)
%%          Val = usually a list of values, see time_switch__cond_8 record() in cpl.hrl
get_by_attribute(Cond, AttrName, PostFun) ->
    case get_attribute(Cond, AttrName) of
	'#no_value' -> [];
	BySecond ->
	    [{AttrName, PostFun(BySecond)}]
    end.

parse_by_range(Str, {Min,Max}) ->
    IntStrs = string:tokens(Str, ","),
    Ints = [list_to_integer(E) || E <- IntStrs],
    F = fun(Int) ->
		case (Int >= Min) and (Int =< Max) of
		    true -> ok;
		    false ->
			throw({error, byxxx_attribute_out_of_range})
		end
	end,
    lists:foreach(F, Ints),
    lists:sort(Ints);

parse_by_range(Str, [{Min,Max}, {MMin, MMax}]) ->
    IntStrs = string:tokens(Str, ","),
    Ints = [list_to_integer(E) || E <- IntStrs],
    F = fun(Int) ->
		case ((Int >= Min) and (Int =< Max)) or
		    ((Int =< MMin) and (Int >= MMax)) of
		    true -> ok;
		    false ->
			throw({error, byxxx_attribute_out_of_range})
		end
	end,
    lists:foreach(F, Ints),
    lists:sort(Ints).

%% return: mo | tu | we | th | fr | sa | su
first_work_day(Cond) ->
    case get_attribute(Cond, wkst) of
	'#no_value' -> mo;
	Wkst ->
	    case httpd_util:to_lower(Wkst) of
		"mo" -> mo;
		"tu" -> tu;
		"we" -> we;
		"th" -> th;
		"fr" -> fr;
		"sa" -> sa;
		"su" -> su;
		_ ->
		     throw({error, wkst_attribute_value_not_a_day})
	    end
    end.

%% convert freq value to a atom
freq_str_to_atom(Str) ->
    freq_str_to_atom2(httpd_util:to_lower(Str)).

freq_str_to_atom2("secondly") -> secondly;
freq_str_to_atom2("minutely") -> minutely;
freq_str_to_atom2("hourly") -> hourly;
freq_str_to_atom2("daily") -> daily;
freq_str_to_atom2("weekly") -> weekly;
freq_str_to_atom2("monthly") -> monthly;
freq_str_to_atom2("yearly") -> yearly;
freq_str_to_atom2(_Freq) -> throw({error, freq_attribute_value_not_legal}).

%%--------------------------------------------------------------------
%% @spec    (BysetposStr) -> [integer()]
%%
%%            BysetposStr = string() "comma separated integers"
%%
%% @doc     ensure that all bysetpos values are in the [1,366] or
%%          [-1,-366] range (days in year)
%% @end
%%--------------------------------------------------------------------
parse_bysetpos(BysetposStr) ->
    TokenList = string:tokens(BysetposStr,","),
    Ints = [list_to_integer(E) || E <- TokenList],
    ValidInts = [xml_parse_util:check_range(E, {[1,366], [-1,-366]}) || E <- Ints],
    lists:sort(ValidInts).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
get_clear_value("yes") ->
    yes;
get_clear_value("no") ->
    no;
get_clear_value(_) ->
    throw({error, clear_attribute_not_set_to_yes_or_no}).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
check_prio_value(PrioStr) ->
    Float = sipparse_util:str_to_float(PrioStr),
    case  xml_parse_util:check_range(Float, [0.0, 1.0]) of
	true -> PrioStr;
	false -> throw({error, prio_value_out_of_range})

    end.

%%--------------------------------------------------------------------
%% @spec    (FieldStr) ->
%%            subject | organization | 'user-agent' | display 
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     convert field value used by string-switch in the attribute
%%          field, to a standard atom() format
%% @end
%%--------------------------------------------------------------------
normalize_string_switch__field(FieldStr) ->
    case FieldStr of
	"subject" -> subject;
	"organization" -> organization;
	"user-agent" -> 'user-agent';
	"display" -> display;
	_ -> throw({error, not_a_legal_string_switch_tag_field_attribute_value})
    end.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.: "If a basic location node specifies a location which
%%           isn't supported by the underlying signaling protocol,
%%           the script server SHOULD detect this and report it to
%%           the user at the time the script is submitted." - RFC 3880
%%           chapter 5.1 p22
%% Returns : URL |
%%           throw({error, Reason})
%%           Reason = atom()
%% Note    : the current yxa implementation only handles sip urls,
%%           when this changes there may be a need to update this
%%           function.
%%--------------------------------------------------------------------
check_url(URL) ->
    case sipurl:parse(URL) of
	{unparseable, _URLStr} -> throw({error, url_attribute_not_set_to_proper_url});
	_ -> URL
    end.

%%--------------------------------------------------------------------
%% @spec    (ParseState) ->
%%            {NewParseState, NextId}
%%
%%            ParseState = #parse_state{}
%%
%%            NewParseState = #parse_state{} "the updated one"
%%            NextId        = term() "the id of the next node"
%%
%% @doc     This function is used to update the current_id in a
%%          parse_state for a CPL rule that has a single possible
%%          destination, e.g. location and remove-location
%% @end
%%--------------------------------------------------------------------
next_id(ParseState) ->
    CurrentId = ParseState#parse_state.current_id,
    NextId = CurrentId ++ [1],
    ParseState2 = ParseState#parse_state{current_id = NextId},
    {ParseState2, NextId}.

%%--------------------------------------------------------------------
%% @spec    (LogName) ->
%%            LogName 
%%
%%            LogName = default | string()
%%
%%            Reason = atom()
%%
%% @throws  {error, Reason} 
%%
%% @doc     check if the name attribute in the log tag, refers to a
%%          log that can be used by cpl.
%% @end
%%--------------------------------------------------------------------
is_log_dest(LogName) ->
    case local:cpl_is_log_dest(LogName) of
	undefined ->
	    is_log_dest2(LogName);
	Res ->
	    Res
    end.

is_log_dest2(_) ->
    throw({error, log_tag_attribute_name_is_not_a_legal_log}).

%%--------------------------------------------------------------------
%% @spec    (SubactionName, ParseState) -> true | false
%%
%% @doc     determine if SubactionName can be used as id attribute by
%%          a subaction tag - all names must be unique
%% @end
%%--------------------------------------------------------------------
is_subaction_name_unqiue(SubactionName, ParseState) ->
    Mapping = ParseState#parse_state.subaction_name_id_mapping,
    case lists:keysearch(SubactionName, 1, Mapping) of
	%% SubactionName not (currently) used as key
	false ->
	     true;
	_ ->
	    false
    end.

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback Note : moved test cases to
%%          xml_parse_test, to keep file size manageable
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->

    xml_parse_test:test(),

    ok.


