-module(targetlist).
-export([add_target/6, debugfriendly/1,
	 get_target/2, get_target_by_header/2, get_targets_in_state/2, get_responses/1,
	 extract_branch/1, extract_initialrequest/1, extract_pid/1, extract_state/1, extract_endresult/1,
         set_pid/2, set_state/2, set_endresult/2,
         update_target/2]).

% This module is used by sipproxy to handle a list of actions we are working
% with.
%
% Targets are of the form :
%
%    {Branch, InitialRequest, Pid, State, EndResult}
%
%       InitialRequest = {Method, URI, Header, Body}
%	EndResult = {Status, Reason, Header, Body}

add_target(Branch, InitialRequest, Pid, State, EndResult, TargetList) ->
    case get_target(Branch, TargetList) of
	none ->
	    lists:append(TargetList, [{Branch, InitialRequest, Pid, State, EndResult}]);	
	_ ->
	    logger:log(error, "Targetlist: Asked to add request with duplicate Branch ~p to list :~n~p",
	    	       [Branch, TargetList]),
	    TargetList
    end.

debugfriendly([]) ->
    [];
debugfriendly([{Branch, Request, Pid, State, EndResult} | Rest]) ->
    {Method, URI, _, _} = Request,
    RespStr = case EndResult of
	none -> "no response";
        cancelled -> "cancelled";
	{Status, Reason, _, _} ->
	    lists:concat(["response=", Status, " ", Reason])
    end,
    Str = lists:concat(["branch=", Branch, ", request=", Method, " ", sipurl:print(URI), ", ", RespStr, ", state=" , State]),
    lists:append([{Pid, Str}], debugfriendly(Rest));
debugfriendly([Unknown | Rest]) ->
    lists:append([{"UNKNOWN: ", Unknown}], debugfriendly(Rest)).

get_target(Branch, TargetList) when list(TargetList) ->
    case lists:keysearch(Branch, 1, TargetList) of
	false ->
	    none;
	{value, Target} ->
	    Target
    end.

get_target_by_header(Header, TargetList) ->
    case sipproxy:get_branch(Header) of
        none ->
            logger:log(error, "Targetlist: Could not get branch from header, Vias :~n~p",
            		[sipheader:via(keylist:fetch("Via", Header))]),
            none;
        Branch ->
	    get_target(Branch, TargetList)
    end.

get_targets_in_state(State, []) ->
    [];
get_targets_in_state(State, [{Branch, InitialRequest, Pid, State, EndResult} | Rest]) ->
    [{Branch, InitialRequest, Pid, State, EndResult} | get_targets_in_state(State, Rest)];
get_targets_in_state(State, [_ | Rest]) ->
    get_targets_in_state(State, Rest).

get_responses([]) ->
    [];
get_responses([{_, _, _, _, {Status, Reason, Header, Body}} | Rest]) when integer(Status) ->
    lists:append([{Status, Reason, Header, Body}], get_responses(Rest));
get_responses([{_, _, _, _, Response} | Rest]) ->
    get_responses(Rest).

extract_branch({Branch, InitialRequest, Pid, State, EndResult}) ->
    Branch.
extract_initialrequest({Branch, InitialRequest, Pid, State, EndResult}) ->
    InitialRequest.
extract_pid({Branch, InitialRequest, Pid, State, EndResult}) ->
    Pid.
extract_state({Branch, InitialRequest, Pid, State, EndResult}) ->
    State.
extract_endresult({Branch, InitialRequest, Pid, State, EndResult}) ->
    EndResult.


set_pid(Target, NewPid) ->
    {Branch, InitialRequest, Pid, State, EndResult} = Target,
    {Branch, InitialRequest, NewPid, State, EndResult}.
set_state(Target, NewState) ->
    {Branch, InitialRequest, Pid, State, EndResult} = Target,
    {Branch, InitialRequest, Pid, NewState, EndResult}.
set_endresult(Target, NewEndResult) ->
    {Branch, InitialRequest, Pid, State, EndResult} = Target,
    {Branch, InitialRequest, Pid, State, NewEndResult}.


update_target(NewTarget, TargetList) ->
    {Branch, _, _, _, _} = NewTarget,
    case get_target(Branch, TargetList) of
	none ->
	    logger:log(error, "Targetlist: Can't modify target with unknown branch ~p, targetlist :~n~p", [Branch, TargetList]),
	    TargetList;
	NewTarget ->
	    TargetList;	
	OldTarget ->
	    %logger:log(debug, "Targetlist: Updating target :~nold: ~p~nnew: ~p", [OldTarget, NewTarget]),
	    lists:keyreplace(Branch, 1, TargetList, NewTarget)
    end.

