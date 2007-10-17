%%%-------------------------------------------------------------------
%%% File    : xml_parse_test.erl
%%% @author   Håkan Stenholm <hsten@it.su.se>
%%% @doc      Test code for xml_parse.erl module.
%%%
%%% Note: the code could be made less verbose, but test code should be
%%%       kept simple to reduce the risk of bugs.
%%%
%%% @since    01 Dec 2004 by Håkan Stenholm <hsten@it.su.se>
%%% @end
%%% @hidden
%%%-------------------------------------------------------------------
-module(xml_parse_test).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("cpl.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

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

    %% cpl_script_to_graph/1
    %%--------------------------------------------------------------------
    %% RFC 3880 - Figure 19
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 1"),
    ScriptStr1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
      <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
        xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
        xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
        <incoming>
	<location url=\"sip:smith@phone.example.com\">
            <redirect/>
	</location>
        </incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr1]),
    Nodes1 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr1)),
    %% io:format("nodes = ~p~n",[Nodes1]),
    Nodes1 =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = location,
				       statements =
				       {#location__attrs{url = "sip:smith@phone.example.com", priority = 1.0,
							 clear = no}, [1,1,1]}}},
		    {[1,1,1], #node_code{type = redirect, statements = {no, terminate}}}
		   ]),


    %% RFC 3880 - Figure 20
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 2"),
    ScriptStr2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <subaction id=\"voicemail\">
       <location url=\"sip:jones@voicemail.example.com\">
         <proxy/>
	</location>
	</subaction>
	<incoming>
	<location url=\"sip:jones@jonespc.example.com\">
         <proxy timeout=\"8\">
           <busy>
	<sub ref=\"voicemail\"/>
           </busy>
	<noanswer>
	<sub ref=\"voicemail\"/>
           </noanswer>
	</proxy>
	</location>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr2]),
    Nodes2 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr2)),
    %% io:format("nodes = ~p~n",[Nodes2]),
    Nodes2 =
	lists:sort([{[3], #node_code{type = location,
				     statements =
				     {#location__attrs{url = "sip:jones@voicemail.example.com", priority = 1.0,
						       clear = no}, [3,1]}}},
		    {[3,1], #node_code{type = proxy, statements = {#proxy__attrs{ timeout = server_max,
										  recurse = yes,
										  ordering = parallel}, []}}},
		    {[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = location,
				       statements =
				       {#location__attrs{url = "sip:jones@jonespc.example.com", priority = 1.0,
							 clear = no}, [1,1,1]}}},
		    {[1,1,1], #node_code{type = proxy, statements = { #proxy__attrs{ timeout = 8,
										     recurse = yes,
										     ordering = parallel},
								      [
								       {busy, [1,1,1,1]},
								       {noanswer, [1,1,1,2]} ]
								     }}},
		    {[1,1,1,1], #node_code{type = sub, statements = [3]}},
		    {[1,1,1,2], #node_code{type = sub, statements = [3]}}
		   ]),


    %% RFC 3880 - Figure 21 - redirection proxy case removed, it's unreachable code
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 3a"),
    ScriptStr3a = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <location url=\"sip:jones@jonespc.example.com\">
          <proxy>
	<default>
	<location url=\"sip:jones@voicemail.example.com\">
                <proxy/>
	</location>
	</default>
	</proxy>
        </location>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr3a]),
    Nodes3a = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr3a)),
    %% io:format("nodes = ~p~n",[Nodes3a]),
    Nodes3a =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = location,
				       statements =
				       {#location__attrs{url = "sip:jones@jonespc.example.com", priority = 1.0,
							 clear = no}, [1,1,1]}}},
		    {[1,1,1], #node_code{type = proxy, statements =
					 {#proxy__attrs{ timeout = 20,
							 recurse = yes,
							 ordering = parallel},
					  [{default, [1,1,1,1]}]
					 }}},
		    {[1,1,1,1], #node_code{type = location,
					   statements =
					   {#location__attrs{url = "sip:jones@voicemail.example.com", priority = 1.0,
							     clear = no}, [1,1,1,1,1]}}},
		    {[1,1,1,1,1], #node_code{type = proxy, statements = { #proxy__attrs{ timeout = server_max,
											 recurse = yes,
											 ordering = parallel},
									  []}}}
		   ]),

    %% RFC 3880 - Figure 21 - this CPL parser is strict and rejects code
    %% that appears meaningless, in this case the creation of unreachable
    %% branches
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 3b"),
    ScriptStr3b = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <location url=\"sip:jones@jonespc.example.com\">
          <proxy>
	<redirection>
	<redirect/>
	</redirection>
	<default>
	<location url=\"sip:jones@voicemail.example.com\">
                <proxy/>
	</location>
	</default>
	</proxy>
        </location>
	</incoming>
	</cpl>",
    autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr3b) end),


    %% RFC 3880 - Figure 22
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 4"),
    ScriptStr4 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <incoming>
	<address-switch field=\"origin\" subfield=\"user\">
         <address is=\"anonymous\">
           <reject status=\"reject\" reason=\"I reject anonymous calls\"/>
         </address>
	</address-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr4]),
    Nodes4 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr4)),
    %% io:format("nodes = ~p~n",[Nodes4]),
    Nodes4 =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = 'address-switch', statements =
				       {{origin, user},
					[{{address__is, "anonymous"}, [1,1,1]} ]}}},
		    {[1,1,1], #node_code{type = reject, statements =
					 {#reject__attrs{status = 603,
							 reason = "I reject anonymous calls"}, terminate}}}
		   ]),

    %% RFC 3880 - Figure 23
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 5"),
    ScriptStr5 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <priority-switch>
	<priority greater=\"urgent\"/>
          <otherwise>
	<language-switch>
	<language matches=\"es\">
                <location url=\"sip:spanish@operator.example.com\">
                  <proxy/>
	</location>
	</language>
	<otherwise>
	<location url=\"sip:english@operator.example.com\">
                  <proxy/>
	</location>
	</otherwise>
	</language-switch>
	</otherwise>
        </priority-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr5]),
    Nodes5 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr5)),
    %% io:format("nodes = ~p~n",[Nodes5]),
    Nodes5 =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = 'priority-switch', statements =
				       [
					  {{priority__greater, urgent}, [1,1,1]},
					  {otherwise, [1,1,2]} ]}},
		    {[1,1,1], #node_code{type = terminator, statements = no_statements}},
		    {[1,1,2], #node_code{type = 'language-switch', statements =
					 [
					  {{language__matches, "es"}, [1,1,2,1]},
					  {otherwise, [1,1,2,2]}
					 ]}},

		    {[1,1,2,1], #node_code{type = location,
					   statements =
					   {#location__attrs{url = "sip:spanish@operator.example.com", priority = 1.0,
							     clear = no}, [1,1,2,1,1]}}},
		    {[1,1,2,1,1], #node_code{type = proxy, statements = { #proxy__attrs{ timeout = server_max,
											 recurse = yes,
											 ordering = parallel},
									  []}}},
		    {[1,1,2,2], #node_code{type = location,
					   statements =
					   {#location__attrs{url = "sip:english@operator.example.com", priority = 1.0,
							     clear = no}, [1,1,2,2,1]}}},
		    {[1,1,2,2,1], #node_code{type = proxy, statements = { #proxy__attrs{ timeout = server_max,
											 recurse = yes,
											 ordering = parallel},
									  []}}}
		   ]),

    %% RFC 3880 - Figure 24, subfield="tel" replaced with subfield="host", as yxa doesn't have tel support
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 6"),
    ScriptStr6 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <outgoing>
	<address-switch field=\"original-destination\" subfield=\"host\">
         <address subdomain-of=\"1900\">
           <reject status=\"reject\"
               reason=\"Not allowed to make 1-900 calls.\"/>
         </address>
	</address-switch>
	</outgoing>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr6]),
    Nodes6 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr6)),
    %% io:format("nodes = ~p~n",[Nodes6]),
    Nodes6 =
	lists:sort([{[2], #node_code{type = outgoing, statements = [2,1]}},
		    {[2,1], #node_code{type = 'address-switch', statements =
				       {{'original-destination', host},
					[{{'address__subdomain-of', "1900"}, [2,1,1]} ]}}},
		    {[2,1,1], #node_code{type = reject, statements =
					 {#reject__attrs{status = 603,
							 reason = "Not allowed to make 1-900 calls."}, terminate}}}
		   ]),

    %% RFC 3880 - Figure 24, test that subfield="tel" is rejected (by yxa due to lack of tel support)
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 6b"),
    ScriptStr6b = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <outgoing>
	<address-switch field=\"original-destination\" subfield=\"tel\">
         <address subdomain-of=\"1900\">
           <reject status=\"reject\"
               reason=\"Not allowed to make 1-900 calls.\"/>
         </address>
	</address-switch>
	</outgoing>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr6b]),
    autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr6b) end),

    %% RFC 3880 - Figure 25 - removed tzid and tzurl attribute
    %% as they are currently unsupported
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7a"),
    ScriptStr7a = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\"
              byday=\"MO,TU,WE,TH,FR\">
            <lookup source=\"registration\">
              <success>
	<proxy/>
	</success>
	</lookup>
	</time>
	<otherwise>
	<location url=\"sip:jones@voicemail.example.com\">
              <proxy/>
	</location>
	</otherwise>
        </time-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr7a]),
    Nodes7a = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr7a)),
    %% io:format("nodes = ~p~n",[Nodes7a]),
    Nodes7a =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{
		       type = 'time-switch', statements =
		       { #time_zone{tzid = '#no_value', tzurl = '#no_value'},
			 [ {#time_switch__cond_7{dtstart =
						 #date_time{date = {2000, 7, 3}, time = {9,0,0},
							    type = floating},
						 dtend_duration = {duration, #duration{hours = 8}},
						 freq = weekly,
						 interval = 1,
						 until_count = repeat_forever,
						 by_values = [{byday, {all,mo}},
							      {byday, {all,tu}},
							      {byday, {all,we}},
							      {byday, {all,th}},
							      {byday, {all,fr}}],
						 wkst = mo
						}, [1,1,1]},
			   {otherwise, [1,1,2]}
			  ]}}},
		    {[1,1,1], #node_code{type = lookup,
					 statements = {#lookup__attrs{source = "registration",
								      timeout = 30,
								      clear = no},
						       [{success, [1,1,1,1]}]
						      }}},
		    {[1,1,1,1], #node_code{type = proxy,
					   statements = { #proxy__attrs{ timeout = server_max,
									 recurse = yes,
									 ordering = parallel},
							  []}}},
		    {[1,1,2], #node_code{type = location,
					 statements =
					 {#location__attrs{url = "sip:jones@voicemail.example.com",
							   priority = 1.0,
							   clear = no},
					  [1,1,2,1]}}},
		    {[1,1,2,1], #node_code{type = proxy,
					   statements = { #proxy__attrs{ timeout = server_max,
									 recurse = yes,
									 ordering = parallel},
							  []}}}
		   ]),

    %% RFC 3880 - Figure 25 - tests that tzid and tzurl usage
    %% is rejected as, they are currently unsupported
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7b"),
    ScriptStr7b = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch tzid=\"America/New_York\"
            tzurl=\"http://zones.example.com/tz/America/New_York\">
          <time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\"
              byday=\"MO,TU,WE,TH,FR\">
            <lookup source=\"registration\">
              <success>
	<proxy/>
	</success>
	</lookup>
	</time>
	<otherwise>
	<location url=\"sip:jones@voicemail.example.com\">
              <proxy/>
	</location>
	</otherwise>
        </time-switch>
	</incoming>
	</cpl>",
    autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr7b) end),

    %% test creation of time_switch__cond_8 record
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7c"),
    ScriptStr7c = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\"
              byday=\"-1MO,TU,+2WE,TH,FR\" bysetpos=\"1,-1,5,-366\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr7c]),
    Nodes7c = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr7c)),
    %% io:format("nodes = ~p~n",[Nodes7c]),
    Nodes7c =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{
		       type = 'time-switch', statements =
		       { #time_zone{tzid = '#no_value', tzurl = '#no_value'},
			 [ {#time_switch__cond_8{dtstart =
						 #date_time{date = {2000, 7, 3}, time = {9,0,0},
							    type = floating},
						 dtend_duration = {duration, #duration{hours = 8}},
						 freq = weekly,
						 interval = 1,
						 until_count = repeat_forever,
						 by_values = [{byday, {-1,mo}},
							      {byday, {all,tu}},
							      {byday, {2,we}},
							      {byday, {all,th}},
							      {byday, {all,fr}}],
						 wkst = mo,
						 bysetpos = lists:sort([1,-1,5,-366])
						}, [1,1,1]}
			  ]}}},
		    {[1,1,1], #node_code{type = terminator, statements = no_statements}}
		   ]),

    %% test creation of time_switch__cond_2 record
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7d"),
    ScriptStr7d = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <cpl
    xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" dtend=\"20041130T090012z\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr7d]),
    Nodes7d = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr7d)),
    %% io:format("nodes = ~p~n",[Nodes7d]),
    Nodes7d =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{
		       type = 'time-switch', statements =
		       { #time_zone{tzid = '#no_value', tzurl = '#no_value'},
			 [ {#time_switch__cond_2{dtstart =
						 #date_time{date = {2000, 7, 3}, time = {9,0,0},
							    type = floating},
						 dtend_duration = {dtend,
								   #date_time{date = {2004,11,30},
									      time = {9,0,12},
									      type = utc}}
						}, [1,1,1]}
			  ]}}},
		    {[1,1,1], #node_code{type = terminator, statements = no_statements}}
		   ]),

    %% test creation of time_switch__cond_5 record
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7e"),
    ScriptStr7e = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr7e]),
    Nodes7e = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr7e)),
    %% io:format("nodes = ~p~n",[Nodes7e]),
    Nodes7e =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{
		       type = 'time-switch', statements =
		       { #time_zone{tzid = '#no_value', tzurl = '#no_value'},
			 [ {#time_switch__cond_5{dtstart =
						 #date_time{date = {2000, 7, 3}, time = {9,0,0},
							    type = floating},
						 dtend_duration = {duration, #duration{hours = 8}},
						 freq = weekly,
						 interval = 1,
						 until_count = repeat_forever
						}, [1,1,1]}
			  ]}}},
		    {[1,1,1], #node_code{type = terminator, statements = no_statements}}
		   ]),

    %% test creation of time_switch__cond_5 record, count = 5
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7f"),
    ScriptStr7f = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\" count=\"5\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr7f]),
    Nodes7f = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr7f)),
    %% io:format("nodes = ~p~n",[Nodes7f]),

    %% this test has been turned into a partial pattern match - the content
    %% of time_ranges, is set by interpret_time.erl which may not yet have
    %% been tested, it is therefore ignored to remove cyclic test dependencies.
    [{[1], #node_code{type = incoming, statements = [1,1]}},
     {[1,1], #node_code{
	type = 'time-switch', statements =
	{ #time_zone{tzid = '#no_value', tzurl = '#no_value'},
	  [{ #time_switch__cond_5{dtstart =
				  #date_time{date = {2000, 7, 3}, time = {9,0,0},
					     type = floating},
				  dtend_duration = {duration, #duration{hours = 8}},
				  freq = weekly,
				  interval = 1,
				  until_count = {count, 5},
				  time_ranges = _
				 }, [1,1,1]}
	  ]}}},
     {[1,1,1], #node_code{type = terminator, statements = no_statements}}
    ] = Nodes7f,

    %% test creation of time_switch__cond_5 record, until = COS DATE
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7g"),
    ScriptStr7g = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\" until=\"20040703\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr7g]),
    Nodes7g = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr7g)),
    %% io:format("nodes = ~p~n",[Nodes7g]),
    Nodes7g =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{
		       type = 'time-switch', statements =
		       { #time_zone{tzid = '#no_value', tzurl = '#no_value'},
			 [ {#time_switch__cond_5{dtstart =
						 #date_time{date = {2000, 7, 3}, time = {9,0,0},
							    type = floating},
						 dtend_duration = {duration, #duration{hours = 8}},
						 freq = weekly,
						 interval = 1,
						 until_count = {until, {2004,7,3}}
						}, [1,1,1]}
			  ]}}},
		    {[1,1,1], #node_code{type = terminator, statements = no_statements}}
		   ]),

    %% test creation of time_switch__cond_5 record, until = COS DATE
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7h"),
    ScriptStr7h = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\" until=\"20040703t193030Z\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr7h]),
    Nodes7h = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr7h)),
    %% io:format("nodes = ~p~n",[Nodes7h]),
    Nodes7h =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{
		       type = 'time-switch', statements =
		       { #time_zone{tzid = '#no_value', tzurl = '#no_value'},
			 [ {#time_switch__cond_5{dtstart =
						 #date_time{date = {2000, 7, 3}, time = {9,0,0},
							    type = floating},
						 dtend_duration = {duration, #duration{hours = 8}},
						 freq = weekly,
						 interval = 1,
						 until_count = {until,
								#date_time{date = {2004,7,3}, time = {19,30,30},
									   type = utc}}
						}, [1,1,1]}
			  ]}}},
		    {[1,1,1], #node_code{type = terminator, statements = no_statements}}
		   ]),

    %% test that dtstart >= dtend is invalid, dtstart < dtend should hold
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7i"),
    ScriptStr7i = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <cpl
    xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" dtend=\"20000703T090000\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
    %% io:format("CPL script = ~s~n",[ScriptStr7i]),
autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr7i) end),

%% test that dtstart can't be set to a to early date not supported by OTP
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7j"),
    ScriptStr7j = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> <cpl
    xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"19691231T235959\" dtend=\"20000703T090000\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr7j]),
autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr7j) end),

%% test that count limit (default = 100) is checked
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 7k"),
    ScriptStr7k = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000\" duration=\"PT8H\" freq=\"weekly\" count=\"101\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr7k]),
autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr7k) end),


%% RFC 3880 - Figure 26
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 8"),
    ScriptStr8 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <incoming>
	<string-switch field=\"user-agent\">
         <string is=\"Inadequate Software SIP User Agent/0.9beta2\">
           <lookup source=\"registration\">
             <success>
	<remove-location location=\"sip:me@mobile.provider.net\">
                 <proxy/>
	</remove-location>
	</success>
	</lookup>
	</string>
	</string-switch>
	</incoming>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr8]),
    Nodes8 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr8)),
%% io:format("nodes = ~p~n",[Nodes8]),
    Nodes8 =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = 'string-switch', statements =
				       {'user-agent',
					[ {{string__is, "Inadequate Software SIP User Agent/0.9beta2"}, [1,1,1]}]
				       }}},
		    {[1,1,1], #node_code{type = lookup,
					 statements = {#lookup__attrs{source = "registration",
								      timeout = 30,
								      clear = no},
						       [{success, [1,1,1,1]}]
						      }}},
		    {[1,1,1,1], #node_code{type = 'remove-location',
					   statements = {#remove_location__attrs{location = "sip:me@mobile.provider.net"},
							 [1,1,1,1,1]} }},
		    {[1,1,1,1,1], #node_code{type = proxy,
					     statements = {#proxy__attrs{ timeout = server_max,
									  recurse = yes,
									  ordering = parallel}, []}}}
		   ]),

%% RFC 3880 - Figure 27, http source currently unsupported,
%% source="http://www.example.com/cgi-bin/locate.cgi?user=mary" currently replaced by source="registration"
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 9"),
    ScriptStr9 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <lookup
	source=\"registration\"
            timeout=\"8\">
          <success>
	<proxy/>
	</success>
	<failure>
	<mail url=\"mailto:mary@example.com?subject=Lookup%20failed\"/>
          </failure>
        </lookup>
	</incoming>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr9]),
    Nodes9 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr9)),
%% io:format("nodes = ~p~n",[Nodes9]),
    Nodes9 =
	lists:sort([{[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = lookup,
				       statements = {
					 #lookup__attrs{source =
							%% "http://www.example.com/cgi-bin/locate.cgi?user=mary",
							"registration",
							timeout = 8,
							clear = no},
					 [{success, [1,1,1]},
					  {failure, [1,1,2]}
					 ]
					}}},
		    {[1,1,1], #node_code{type = proxy,
					 statements = {#proxy__attrs{ timeout = server_max,
								      recurse = yes,
								      ordering = parallel}, []}}},
		    {[1,1,2], #node_code{type = mail,
					 statements = {"mailto:mary@example.com?subject=Lookup%20failed",
						       [1,1,2,1]}}},
		    {[1,1,2,1], #node_code{type = terminator, statements = no_statements}}
		   ]),

%% RFC 3880 - Figure 30 - <location url="tel:+19175551212">
%% replaced by <location url="sip:boss2@foo.com">, as tel is
%% currently not a supported YXA protocol.
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 10a"),
    ScriptStr10a = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <subaction id=\"voicemail\">
        <location url=\"sip:jones@voicemail.example.com\">
          <redirect />
        </location>
	</subaction>
	<incoming>
        <location url=\"sip:jones@phone.example.com\">
          <proxy timeout=\"8\">
            <busy>
	<sub ref=\"voicemail\" />
            </busy>
	<noanswer>
	<address-switch field=\"origin\">
                <address is=\"sip:boss@example.com\">
                  <location url=\"sip:boss2@foo.com\">
                    <proxy />
	</location>
	</address>
	<otherwise>
	<sub ref=\"voicemail\" />
                </otherwise>
	</address-switch>
	</noanswer>
	</proxy>
        </location>
	</incoming>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr10a]),
    Nodes10a = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr10a)),
%% io:format("nodes = ~p~n",[Nodes10a]),
    Nodes10a =
	lists:sort([
		    {[3], #node_code{type = location,
				     statements =
				     {#location__attrs{url = "sip:jones@voicemail.example.com", priority = 1.0,
						       clear = no}, [3,1]}}},
		    {[3,1], #node_code{type = redirect, statements = {no, terminate}}},
		    {[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = location,
				       statements =
				       {#location__attrs{url = "sip:jones@phone.example.com", priority = 1.0,
							 clear = no}, [1,1,1]}}},
		    {[1,1,1], #node_code{type = proxy,
					 statements = {#proxy__attrs{ timeout = 8,
								      recurse = yes,
								      ordering = parallel},
						       [{busy, [1,1,1,1]},
							{noanswer, [1,1,1,2]}
						       ]}}},
		    {[1,1,1,1], #node_code{type = sub, statements = [3]}},
		    {[1,1,1,2], #node_code{type = 'address-switch', statements =
					   {{origin, '#no_value'},
					    [{{address__is, "sip:boss@example.com"}, [1,1,1,2,1]},
					     {otherwise, [1,1,1,2,2]}
					    ]}}},
		    {[1,1,1,2,2], #node_code{type = sub, statements = [3]}},
		    {[1,1,1,2,1], #node_code{type = location,
					     statements =
					     {#location__attrs{url = "sip:boss2@foo.com", priority = 1.0,
							       clear = no}, [1,1,1,2,1,1]}}},
		    {[1,1,1,2,1,1], #node_code{type = proxy,
					       statements = {#proxy__attrs{ timeout = server_max,
									    recurse = yes,
									    ordering = parallel}, []}}}
		   ]),

%% RFC 3880 - Figure 30 - test that script with unsupported
%% protocol tel is rejected
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 10b"),
    ScriptStr10b = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <subaction id=\"voicemail\">
        <location url=\"sip:jones@voicemail.example.com\">
          <redirect />
        </location>
	</subaction>
	<incoming>
        <location url=\"sip:jones@phone.example.com\">
          <proxy timeout=\"8\">
            <busy>
	<sub ref=\"voicemail\" />
            </busy>
	<noanswer>
	<address-switch field=\"origin\">
                <address is=\"sip:boss@example.com\">
                  <location url=\"tel:+19175551212\">
                    <proxy />
	</location>
	</address>
	<otherwise>
	<sub ref=\"voicemail\" />
                </otherwise>
	</address-switch>
	</noanswer>
	</proxy>
        </location>
	</incoming>
	</cpl>",
    autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr10b) end),

%% RFC 3880 - Figure 2
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 11"),
    ScriptStr11 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <subaction id=\"voicemail\">
        <location url=\"sip:jones@voicemail.example.com\">
          <redirect />
        </location>
	</subaction>
	<incoming>
        <address-switch field=\"origin\" subfield=\"host\">
          <address subdomain-of=\"example.com\">
            <location url=\"sip:jones@example.com\">
              <proxy timeout=\"10\">
                <busy> <sub ref=\"voicemail\" /> </busy>
                <noanswer> <sub ref=\"voicemail\" /> </noanswer>
                <failure> <sub ref=\"voicemail\" /> </failure>
              </proxy>
	</location>
	</address>
	<otherwise>
	<sub ref=\"voicemail\" />
          </otherwise>
        </address-switch>
	</incoming>
	</cpl>",
%%io:format("CPL script = ~s~n",[ScriptStr11]),
    Nodes11 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr11)),
%%io:format("nodes = ~p~n",[Nodes11]),
    Nodes11 =
	lists:sort([
		    {[3], #node_code{type = location,
				     statements =
				     {#location__attrs{url = "sip:jones@voicemail.example.com", priority = 1.0,
						       clear = no}, [3,1]}}},
		    {[3,1], #node_code{type = redirect, statements = {no, terminate}}},
		    {[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = 'address-switch', statements =
				       {{origin, host},
					[{{'address__subdomain-of', "example.com"}, [1,1,1]},
					 {otherwise, [1,1,2]}
					]}}},
		    {[1,1,2], #node_code{type = sub, statements = [3]}},
		    {[1,1,1], #node_code{type = location,
					 statements =
					 {#location__attrs{url = "sip:jones@example.com", priority = 1.0,
							   clear = no}, [1,1,1,1]}}},
		    {[1,1,1,1], #node_code{type = proxy,
					   statements = {#proxy__attrs{ timeout = 10,
									recurse = yes,
									ordering = parallel},
							 [{busy, [1,1,1,1,1]},
							  {noanswer, [1,1,1,1,2]},
							  {failure, [1,1,1,1,3]}
							 ]}}},
		    {[1,1,1,1,1], #node_code{type = sub, statements = [3]}},
		    {[1,1,1,1,2], #node_code{type = sub, statements = [3]}},
		    {[1,1,1,1,3], #node_code{type = sub, statements = [3]}}
		   ]),

%% test that self reference fails
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 12"),
    ScriptStr12 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <subaction id=\"voicemail\">
        <sub ref=\"voicemail\"/>
     </subaction>
	<incoming>
        <sub ref=\"voicemail\"/>
     </incoming>
	</cpl>",
    autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr12) end),

%% RFC 3880 - test that reference to later subaction fails
autotest:mark(?LINE, "cpl_script_to_graph/1  - 13"),
    ScriptStr13 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <subaction id=\"voicemail\">
        <sub ref=\"voicemail2\"/>
     </subaction>
	<subaction id=\"voicemail2\">
        <sub ref=\"voicemail\"/>
     </subaction>
	<incoming>
        <sub ref=\"voicemail2\"/>
     </incoming>
	</cpl>",
autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr13) end),

%% RFC 3880 - test that existence of unreachable node results in failure
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 14"),
    ScriptStr14 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <subaction id=\"unreachable\">
        <reject/>
	</subaction>
	<incoming>
        <reject/>
	</incoming>
	</cpl>",
autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr14) end),

%% test log tag (fail as no log destination is currently supported)
autotest:mark(?LINE, "cpl_script_to_graph/1  - 15"),
    ScriptStr15 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <outgoing>
	<log comment=\"log stuff\"/>
     </outgoing>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr15]),
%% Nodes15 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr15)),
%%     %% io:format("nodes = ~p~n",[Nodes15]),
%%     Nodes15 =
%% 	lists:sort([{[2], #node_code{type = outgoing, statements = [2,1]}},
%% 		    {[2,1], #node_code{type = log, statements =
%% 					 { #log__attrs{name = default, comment = "log stuff"}, [2,1,1]}}},
%% 		    {[2,1,1], #node_code{type = terminator, statements = no_statements}}
%% 		   ]),
autotest:fail(fun() -> xml_parse:cpl_script_to_graph(ScriptStr15) end),


%% test that outgoing and incoming tags can be empty
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 16"),
    ScriptStr16 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <outgoing>
	</outgoing>
	<incoming>
	</incoming>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr16]),
    Nodes16 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr16)),
%% io:format("nodes = ~p~n",[Nodes16]),
    Nodes16 =
	lists:sort([{[2], #node_code{type = outgoing, statements = [2,1]}},
		    {[1], #node_code{type = incoming, statements = [1,1]}},
		    {[2,1], #node_code{type = terminator, statements = no_statements}},
		    {[1,1], #node_code{type = terminator, statements = no_statements}}
		   ]),

%% test that subaction tag can be empty
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 17"),
    ScriptStr17 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
   <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
     xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
     xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
     <subaction id=\"voicemail\">
     </subaction>
	<incoming>
	<sub ref=\"voicemail\"/>
      </incoming>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr17]),
    Nodes17 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr17)),
%% io:format("nodes = ~p~n",[Nodes17]),
    Nodes17 =
	lists:sort([{[3], #node_code{type = terminator, statements = no_statements}},
		    {[1], #node_code{type = incoming, statements = [1,1]}},
		    {[1,1], #node_code{type = sub, statements = [3]}}
		   ]),

%% test that duplicate sub tags are not accepted
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 18"),
    ScriptStr18 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <subaction id=\"voicemail\">
          <proxy/>
	</subaction>
	<incoming>
	<proxy timeout=\"10\">
                <busy> <sub ref=\"voicemail\" /> </busy>
                <busy> <sub ref=\"voicemail\" /> </busy>
                <noanswer> <sub ref=\"voicemail\" /> </noanswer>
                <failure> <sub ref=\"voicemail\" /> </failure>
              </proxy>
	</incoming>
	</cpl>",
%%io:format("CPL script = ~s~n",[ScriptStr18]),
    autotest:fail(fun() -> lists:sort(xml_parse:cpl_script_to_graph(ScriptStr18)) end),

%% test that unknown sub tag (foo) can't be used
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 19"),
    ScriptStr19 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <subaction id=\"voicemail\">
          <proxy/>
	</subaction>
	<incoming>
	<proxy timeout=\"10\">
                <busy> <sub ref=\"voicemail\" /> </busy>
                <foo> <sub ref=\"voicemail\" /> </foo>
                <noanswer> <sub ref=\"voicemail\" /> </noanswer>
                <failure> <sub ref=\"voicemail\" /> </failure>
              </proxy>
	</incoming>
	</cpl>",
%%io:format("CPL script = ~s~n",[ScriptStr19]),
    autotest:fail(fun() -> lists:sort(xml_parse:cpl_script_to_graph(ScriptStr19)) end),

%% test that unknown tag (foo) can't be used
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 20"),
    ScriptStr20 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
	<foo> </foo>
	</incoming>
	</cpl>",
%%io:format("CPL script = ~s~n",[ScriptStr20]),
autotest:fail(fun() -> lists:sort(xml_parse:cpl_script_to_graph(ScriptStr20)) end),

%%
%% test dtstart with utc is rejected when byparameters are supplied
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 21a"),
    ScriptStr21a = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000z\" duration=\"PT8H\" freq=\"weekly\"
              byday=\"-1MO,TU,+2WE,TH,FR\" bysetpos=\"1,-1,5,-366\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
autotest:fail(fun() -> lists:sort(xml_parse:cpl_script_to_graph(ScriptStr21a)) end),

%% test dtstart with utc is not rejected when byparameters are not supplied
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 21b"),
    ScriptStr21b = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20000703T090000z\" duration=\"PT8H\" freq=\"weekly\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
_ = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr21b)),


%% test creation of time_switch__cond_8 record, count = 2
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 22"),
    ScriptStr22 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20050704T090000\" duration=\"PT8H\" freq=\"monthly\" byday=\"tu\"
          bysetpos=\"-1\" count=\"2\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
%% io:format("CPL script = ~s~n",[ScriptStr22]),
    Nodes22 = lists:sort(xml_parse:cpl_script_to_graph(ScriptStr22)),
%% io:format("nodes = ~p~n",[Nodes22]),

%% this test has been turned into a partial pattern match - the content
%% of time_ranges, is set by interpret_time.erl which may not yet have
%% been tested, it is therefore ignored to remove cyclic test dependencies.

%% the time_ranges should be:
%% [2005-07-28 09:00:00, 2005-07-28 16:59:59] and
%% [2005-08-30 09:00:00, 2005-07-30 16:59:59]
%% "last thursday of the first two months"
    [{[1], #node_code{type = incoming, statements = [1,1]}},
     {[1,1], #node_code{
	type = 'time-switch', statements =
	{ #time_zone{tzid = '#no_value', tzurl = '#no_value'},
	  [{ #time_switch__cond_8{dtstart =
				  #date_time{date = {2005, 7, 4}, time = {9,0,0},
					     type = floating},
				  dtend_duration = {duration, #duration{hours = 8}},
				  freq = monthly,
				  interval = 1,
				  until_count = {count, 2},
				  by_values = [{byday, {all, tu}}],
				  wkst = mo,
				  bysetpos = [-1],
				  time_ranges = _
				 }, [1,1,1]}
	  ]}}},
     {[1,1,1], #node_code{type = terminator, statements = no_statements}}
    ] = Nodes22,


%% verify that to large search tree are not run to get the count
    autotest:mark(?LINE, "cpl_script_to_graph/1  - 23"),
    ScriptStr23 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
    <cpl xmlns=\"urn:ietf:params:xml:ns:cpl\"
      xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
      xsi:schemaLocation=\"urn:ietf:params:xml:ns:cpl cpl.xsd \">
      <incoming>
        <time-switch>
	<time dtstart=\"20050704T090000\" duration=\"PT8H\" freq=\"secondly\" byday=\"tu\"
          bysetpos=\"-1\" count=\"2\">
          </time>
        </time-switch>
	</incoming>
	</cpl>",
autotest:fail(fun() -> lists:sort(xml_parse:cpl_script_to_graph(ScriptStr23)) end),




    ok.


