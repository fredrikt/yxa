%% This module handles storage and loading of cpl scripts
%%--------------------------------------------------------------------

-module(cpl_db).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create/0,
	 create/1,
	 load_cpl_for_user/2,
	 get_cpl_for_user/1,
	 user_has_cpl_script/1,
	 user_has_cpl_script/2,
	 rm_cpl_for_user/1
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([

        ]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(cpl_script_graph, {
	  user,
	  graph
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Descrip.: 
%% Returns : 
%%--------------------------------------------------------------------
create() ->
    create(servers()).

create(Servers) ->
    mnesia:create_table(cpl_script_graph, [{attributes, record_info(fields, cpl_script_graph)},
					   {disc_copies, Servers}
					   %% type = set, mnesia default
					  ]).

servers() ->
    sipserver:get_env(databaseservers).

%%--------------------------------------------------------------------
%% Function: get_cpl_for_user(User) 
%% Descrip.: get the cpl script graph for a certain user
%% Returns : nomtach | {ok, CPLGraph}
%%           CPLGraph = a cpl graph for use in 
%%           interpret_cpl:process_cpl_script(...)
%%--------------------------------------------------------------------
get_cpl_for_user(User) ->
    case mnesia:dirty_read({cpl_script_graph, User}) of
	[] -> nomatch;
	[Rec] -> {ok, Rec#cpl_script_graph.graph}
    end.


%%--------------------------------------------------------------------
%% Function: load_cpl_for_user(User, FilePath) 
%%           FilePath = string(), a full file path (no .|..|~)
%% Descrip.: store the cpl script file at FilePath in mnesia
%% Returns : -
%%--------------------------------------------------------------------
load_cpl_for_user(User, FilePath) ->
    Str = load_file(FilePath),
    Graph = xml_parse:cpl_script_to_graph(Str),
    store_graph(User, Graph).

store_graph(User, Graph) ->
    F = fun() ->
		mnesia:write(#cpl_script_graph{user = User, graph = Graph})
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function: rm_cpl_for_user(User)
%% Descrip.: remove the cpl script associated with user User
%% Returns : -
%%--------------------------------------------------------------------
rm_cpl_for_user(User) ->
    F = fun() ->
		mnesia:delete({cpl_script_graph, User})
	end,
    mnesia:transaction(F).

%%--------------------------------------------------------------------
%% Function: user_has_cpl_script(User)
%%           user_has_cpl_script(User, Type)
%%           Type = incoming | outgoing
%% Descrip.: determine if a cpl script has been loaded for the user 
%%           User. Type is used to determine if script can handle 
%%           incoming or outgoing traffic - it may be able to do both
%% Returns : true | false
%%--------------------------------------------------------------------
user_has_cpl_script(User) -> 
    case mnesia:dirty_read({cpl_script_graph, User}) of
	[] ->
	     false;
	[_] ->
	    true
    end.

user_has_cpl_script(User, Type) -> 
    case mnesia:dirty_read({cpl_script_graph, User}) of
	[] ->
	     false;
	[Rec] ->
	    Graph = Rec#cpl_script_graph.graph,
	    Index = interpret_cpl:get_start_node(Type),
	    try begin
		    interpret_cpl:get_node(Graph, Index),
		    %% no exception so node exists
		    true
		end
	    catch
		throw: _ -> false
	    end
    end.

	
%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: load_file(FilePath)
%%           FilePath = string(), a full file path (no .|..|~)
%% Descrip.: get data from file a FilePath
%% Returns : string()
%%--------------------------------------------------------------------
load_file(FilePath) ->
    case file:read_file(FilePath) of
	{ok, Binary} ->
	    binary_to_list(Binary);
	{error, Reason} ->
	    throw({error, Reason})
    end.

%%====================================================================
%% Test functions
%%====================================================================



