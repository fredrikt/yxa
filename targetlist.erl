%%%-------------------------------------------------------------------
%%% File    : targetlist.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Targetlist is a module for managing a list of ongoing
%%%           client transactions for sipproxy.
%%% Created : 28 Jun 2003 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(targetlist).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 add/7,
	 empty/0,
	 list_length/1,
	 debugfriendly/1,
	 get_using_branch/2,
	 get_targets_in_state/2,
	 get_responses/1,
	 extract/2,
	 set_pid/2,
	 set_state/2,
	 set_endresult/2,
	 set_dstlist/2,
	 set_cancelled/2,
	 update_target/2
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% Container record to make sure noone tries to modify our records.
-record(targetlist, {list}).

-record(target, {
	  ref,		%% ref(), unique reference
	  branch,	%% string(), this clients branch
	  request,	%% request record()
	  pid,		%% pid() of client transaction ???
	  state,
	  timeout,
	  endresult,
	  dstlist,	%% list() of sipdst record(), more destinations if this one fail
	  cancelled=false
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

add(Branch, Request, Pid, State, Timeout, DstList, TargetList) when is_record(Request, request),
								    is_record(TargetList, targetlist) ->
    case get_using_branch(Branch, TargetList) of
	none ->
	    NewTarget = #target{ref=make_ref(), branch=Branch, request=Request, pid=Pid, state=State,
		   		endresult=none, dstlist=DstList, timeout=Timeout},
	    #targetlist{list=lists:append(TargetList#targetlist.list, [NewTarget])};
	_ ->
	    logger:log(error, "targetlist: Asked to add target with duplicate branch ~p to list :~n~p",
	    	       [branch, debugfriendly(TargetList)]),
	    TargetList
    end.

empty() ->
    #targetlist{list=[]}.

list_length(TargetList) when is_record(TargetList, targetlist) ->
    length(TargetList#targetlist.list).

update_target(Target, TargetList) when is_record(Target, target), is_record(TargetList, targetlist) ->
    Ref = Target#target.ref,
    #targetlist{list=update_target(Ref, Target, TargetList#targetlist.list, TargetList)}.

update_target(Ref, _, [], TargetList) ->
    logger:log(error, "Targetlist: Asked to update a target, but I can't find it", [Ref]),
    logger:log(error, "Targetlist: Asked to update a target with ref=~p, but I can't find it in list :~n~p",
		[Ref, debugfriendly(TargetList)]),
    [];
update_target(Ref, NewT, [H | T], _TargetList) when is_record(H, target), H#target.ref == Ref ->
    [NewT | T];
update_target(Ref, NewT, [H | T], TargetList) when is_record(H, target) ->
    lists:append([H], update_target(Ref, NewT, T, TargetList)).

debugfriendly(TargetList) when is_record(TargetList, targetlist) ->
    debugfriendly(TargetList#targetlist.list);
debugfriendly([]) ->
    [];
debugfriendly([H | T]) when is_record(H, target) ->
    Request = H#target.request,
    {Method, URI} = {Request#request.method, Request#request.uri},
    RespStr = case H#target.endresult of
	none -> "no response";
	Response when is_record(Response, response) ->
	    lists:concat(["response=", Response#response.status, " ", Response#response.reason]);
	_ -> "INVALID response"
    end,
    Str = lists:concat(["pid=", pid_to_list(H#target.pid), "branch=", H#target.branch, ", request=", Method, " ",
		sipurl:print(URI), ", ", RespStr, ", cancelled=", H#target.cancelled, ", state=" , H#target.state]),
    lists:append([lists:flatten(Str)], debugfriendly(T)).
get_using_branch(Branch, TargetList) when is_record(TargetList, targetlist) ->
    get_using_branch(Branch, TargetList#targetlist.list);

get_using_branch(_Branch, []) ->
    none;
get_using_branch(Branch, [H | _T]) when is_record(H, target), H#target.branch == Branch ->
    H;
get_using_branch(Branch, [H | T]) when is_record(H, target) ->
    get_using_branch(Branch, T).


get_targets_in_state(State, TargetList) when is_record(TargetList, targetlist) ->
    get_targets_in_state(State, TargetList#targetlist.list);

get_targets_in_state(_State, []) ->
    [];
get_targets_in_state(State, [H | T]) when is_record(H, target), H#target.state == State ->
    lists:append(get_targets_in_state(State, T), [H]);
get_targets_in_state(State, [H | T]) when is_record(H, target) ->
    get_targets_in_state(State, T).


get_responses(TargetList) when is_record(TargetList, targetlist) ->
    get_responses2(TargetList#targetlist.list, []).

get_responses2([], Res) ->
    Res;
get_responses2([H | T], Res) when is_record(H, target) ->
    case H#target.endresult of
	Response when is_record(Response, response) ->
	    get_responses2(T, lists:append(Res, [Response]));
	_ ->
	    get_responses2(T, Res)
    end.


extract(Values, Target) when is_record(Target, target) ->
    extract(Values, Target, []).

extract([], Target, Res) when is_record(Target, target) ->
    Res;
extract([pid | T], Target, Res) when is_record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.pid]));
extract([branch | T], Target, Res) when is_record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.branch]));
extract([request | T], Target, Res) when is_record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.request]));
extract([state | T], Target, Res) when is_record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.state]));
extract([timeout | T], Target, Res) when is_record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.timeout]));
extract([dstlist | T], Target, Res) when is_record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.dstlist]));
extract([endresult | T], Target, Res) when is_record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.endresult]));
extract([cancelled | T], Target, Res) when is_record(Target, target) ->
    extract(T, Target, lists:append(Res, [Target#target.cancelled])).

set_pid(Target, Value) when is_record(Target, target) ->
    Target#target{pid=Value}.
set_state(Target, Value) when is_record(Target, target) ->
    Target#target{state=Value}.
set_endresult(Target, Value) when is_record(Target, target) ->
    Target#target{endresult=Value}.
set_dstlist(Target, Value) when is_record(Target, target) ->
    Target#target{dstlist=Value}.
set_cancelled(Target, Value) when is_record(Target, target) ->
    Target#target{cancelled=Value}.

