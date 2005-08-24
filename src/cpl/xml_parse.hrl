%% records used during parsing
%%--------------------------------------------------------------------

%% internal record used by the xml_parseXXX modules

-record(parse_state, {
	  current_id,               % stores id for node currently being examined in a parse_xml(...)
	                            % clause
	  current_graph,            % store all parsed nodes and edges in this entry
	  subaction_name_id_mapping % used to lookup subaction id (CPL uses symbolic names)
	                            % stored as list() of {SubactionName, NodeId}
	 }).

