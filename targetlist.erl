-module(targetlist).
-export([add/7, empty/0, list_length/1, debugfriendly/1,
	 get_using_branch/2, get_targets_in_state/2, get_responses/1,
	 extract/2, set_pid/2, set_state/2, set_endresult/2, set_dstlist/2,
	 set_cancelled/2, update_target/2]).

-record(target, {ref, branch, request, pid, state, timeout, endresult, dstlist, cancelled}).
-record(targetlist, {list}).

% This module is used by sipproxy to handle a list of actions we are working
% with.
%
% Targets are of the form :
%
%    {Branch, InitialRequest, Pid, State, EndResult}
%
%       InitialRequest = {Method, URI, Header, Body}
%	EndResult = {Status, Reason, Header, Body}

add(Branch, Request, Pid, State, Timeout, DstList, TargetList) when record(TargetList, targetlist) ->
    case get_using_branch(Branch, TargetList) of
	none ->
	    NewTarget = #target{ref=make_ref(), branch=Branch, request=Request, pid=Pid, state=State,
		   		endresult=none, dstlist=DstList, timeout=Timeout, cancelled=false},
	    #targetlist{list=lists:append(TargetList#targetlist.list, [NewTarget])};
	_ ->
	    logger:log(error, "targetlist: Asked to add target with duplicate branch ~p to list :~n~p",
	    	       [branch, debugfriendly(TargetList)]),
	    TargetList
    end.

empty() ->
    #targetlist{list=[]}.

list_length(TargetList) when record(TargetList, targetlist) ->
    length(TargetList#targetlist.list).

update_target(Target, TargetList) when record(Target, target), record(TargetList, targetlist) ->
    Ref = Target#target.ref,
    #targetlist{list=update_target(Ref, Target, TargetList#targetlist.list, TargetList)}.

update_target(Ref, _, [], TargetList) ->
    logger:log(error, "Targetlist: Asked to update a target, but I can't find it", [Ref]),
    logger:log(error, "Targetlist: Asked to update a target with ref=~p, but I can't find it in list :~n~p",
		[Ref, debugfriendly(TargetList)]),
    [];
update_target(Ref, NewT, [H | T], TargetList) when record(H, target), H#target.ref == Ref ->
    [NewT | T];
update_target(Ref, NewT, [H | T], TargetList) when record(H, target) ->
    lists:append([H], update_target(Ref, NewT, T, TargetList)).

debugfriendly(TargetList) when record(TargetList, targetlist) ->
    debugfriendly(TargetList#targetlist.list);
debugfriendly([]) ->
    [];
debugfriendly([H | T]) when record(H, target) ->
    {Method, URI, _, _} = H#target.request,
    RespStr = case H#target.endresult of
	none -> "no response";
	{Status, Reason, _, _} ->
	    lists:concat(["response=", Status, " ", Reason])
    end,
    Str = lists:concat(["pid=", pid_to_list(H#target.pid), "branch=", H#target.branch, ", request=", Method, " ",
		sipurl:print(URI), ", ", RespStr, ", cancelled=", H#target.cancelled, ", state=" , H#target.state]),
    lists:append([lists:flatten(Str)], debugfriendly(T)).
get_using_branch(Branch, TargetList) when record(TargetList, targetlist) ->
    get_using_branch(Branch, TargetList#targetlist.list);

get_using_branch(Branch, []) ->
    none;
get_using_branch(Branch, [H | T]) when record(H, target), H#target.branch == Branch ->
    H;
get_using_branch(Branch, [H | T]) when record(H, target) ->
    get_using_branch(Branch, T).


get_targets_in_state(State, TargetList) when record(TargetList, targetlist) ->
    get_targets_in_state(State, TargetList#targetlist.list);

get_targets_in_state(State, []) ->
    [];
get_targets_in_state(State, [H | T]) when record(H, target), H#target.state == State ->
    lists:append(get_targets_in_state(State, T), [H]);
get_targets_in_state(State, [H | T]) when record(H, target) ->
    get_targets_in_state(State, T).


get_responses(TargetList) when record(TargetList, targetlist) ->
    get_responses2(TargetList#targetlist.list, []).

get_responses2([], Res) ->
    Res;
get_responses2([H | T], Res) when record(H, target) ->
    case H#target.endresult of
	{Status, Reason, Header, Body} ->
	    get_responses2(T, lists:append(Res, [H#target.endresult]));
	_ ->
	    get_responses2(T, Res)
    end.


extract(Values, Target) when record(Target, target) ->
    extract(Values, Target, []).

extract([], Target, Res) when record(Target, target) ->
    Res;
extract([pid | T], Target, Res) when record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.pid]));
extract([branch | T], Target, Res) when record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.branch]));
extract([request | T], Target, Res) when record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.request]));
extract([state | T], Target, Res) when record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.state]));
extract([timeout | T], Target, Res) when record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.timeout]));
extract([dstlist | T], Target, Res) when record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.dstlist]));
extract([endresult | T], Target, Res) when record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.endresult]));
extract([cancelled | T], Target, Res) when record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.cancelled])).

set_pid(Target, Value) when record(Target, target) ->
    Target#target{pid=Value}.
set_state(Target, Value) when record(Target, target) ->
    Target#target{state=Value}.
set_endresult(Target, Value) when record(Target, target) ->
    Target#target{endresult=Value}.
set_dstlist(Target, Value) when record(Target, target) ->
    Target#target{dstlist=Value}.
set_cancelled(Target, Value) when record(Target, target) ->
    Target#target{cancelled=Value}.

