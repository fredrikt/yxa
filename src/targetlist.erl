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
	 add/8,
	 empty/0,
	 get_length/1,
	 debugfriendly/1,
	 get_using_pid/2,
	 get_using_branch/2,
	 get_targets_in_state/2,
	 get_responses/1,
	 extract/2,
	 set_pid/2,
	 set_state/2,
	 set_endresult/2,
	 set_dstlist/2,
	 set_cancelled/2,
	 update_target/2,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipproxy.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% Container record to make sure noone tries to modify our records.
-record(targetlist, {list}).

-record(target, {
	  ref,			%% ref(), unique reference
	  branch,		%% string(), this clients branch
	  request,		%% request record()
	  pid,			%% pid() of client transaction ???
	  state,		%% atom(), SIP state of this target
	  timeout,		%% integer()
	  endresult = none,	%% none | sp_response record()
	  dstlist,		%% list() of sipdst record(), more destinations if this one fail
	  cancelled = false,	%% true | false, is this branch cancelled?
	  user_instance		%% none | {User, Instance}
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add(Branch, Request, Pid, State, Timeout, DstList,
%%               User, Instance, TargetList)
%%           Branch     = string()
%%           Request    = request record()
%%           Pid        = pid()
%%           State      = atom()
%%           Timeout    = integer()
%%           DstList    = list() of sipdst record()
%%           UserInst   = none | tuple() ({User, Instance})
%%           TargetList = targetlist record()
%% Descrip.: Add a new entry to TargetList, after verifying that a
%%           target with this branch is not already in the list.
%% Returns : NewTargetList = targetlist record()
%%--------------------------------------------------------------------
add(Branch, Request, Pid, State, Timeout, DstList, UserInst, TargetList) 
  when is_list(Branch), is_record(Request, request), is_pid(Pid), is_atom(State), is_integer(Timeout),
       is_list(DstList), is_record(TargetList, targetlist), is_tuple(UserInst); UserInst == none ->
    case get_using_branch(Branch, TargetList) of
	none ->
	    NewTarget = #target{ref		= make_ref(),
				branch		= Branch,
				request		= Request,
				pid		= Pid,
				state		= State,
		   		dstlist		= DstList,
				timeout		= Timeout,
				user_instance	= UserInst
			       },
	    #targetlist{list = lists:append(TargetList#targetlist.list, [NewTarget])};
	T when is_record(T, target) ->
	    logger:log(error, "targetlist: Asked to add target with duplicate branch ~p to list :~n~p",
	    	       [branch, debugfriendly(TargetList)]),
	    TargetList
    end.

%%--------------------------------------------------------------------
%% Function: empty()
%% Descrip.: Return empty targetlist record. For initialization.
%% Returns : TargetList = targetlist record()
%%--------------------------------------------------------------------
empty() ->
    #targetlist{list = []}.

%%--------------------------------------------------------------------
%% Function: get_length(TargetList)
%%           TargetList = targetlist record()
%% Descrip.: Return length of list encapsulated in the TargetList
%%           record.
%% Returns : Length = integer()
%%--------------------------------------------------------------------
get_length(TargetList) when is_record(TargetList, targetlist) ->
    length(TargetList#targetlist.list).

%%--------------------------------------------------------------------
%% Function: update_target(Target, TargetList)
%%           Target     = target record()
%%           TargetList = targetlist record()
%% Descrip.: Locate the old instance of the target Target in
%%           the TargetList and exchange it with Target.
%% Returns : NewTargetList = list() of target record() | throw(...)
%%--------------------------------------------------------------------
update_target(Target, TargetList) when is_record(Target, target), is_record(TargetList, targetlist) ->
    Ref = Target#target.ref,
    NewList = update_target2(Ref, Target, TargetList#targetlist.list, []),
    #targetlist{list = NewList}.

update_target2(Ref, _NewT, [], Res) ->
    logger:log(error, "Targetlist: Asked to update a target, but I can't find it"),
    logger:log(debug, "Targetlist: Asked to update a target with ref=~p, but I can't find it in list :~n~p",
	       [Ref, debugfriendly2( lists:reverse(Res), [])]),
    throw({error, update_of_non_existin_target});
update_target2(Ref, NewT, [#target{ref=Ref} | T], Res) ->
    %% Match
    Head = lists:reverse([NewT | Res]),
    Head ++ T;
update_target2(Ref, NewT, [H | T], Res) when is_record(H, target) ->
    %% No match
    update_target2(Ref, NewT, T, [H | Res]).

%%--------------------------------------------------------------------
%% Function: debugfriendly(TargetList)
%%           TargetList = targetlist record()
%% Descrip.: Format the entrys in TargetList in a way that is suitable
%%           for logging using ~p.
%% Returns : Data = term()
%%--------------------------------------------------------------------
debugfriendly(TargetList) when is_record(TargetList, targetlist) ->
    debugfriendly2(TargetList#targetlist.list, []).

debugfriendly2([], Res) ->
    lists:reverse(Res);
debugfriendly2([H | T], Res) when is_record(H, target) ->
    #request{method = Method, uri = URI} = H#target.request,
    RespStr = case H#target.endresult of
		  none -> "no response";
		  R when is_record(R, sp_response) ->
		      lists:concat(["response=", R#sp_response.status, " ", R#sp_response.reason]);
		  _ -> "INVALID response"
	      end,
    Str = lists:concat(["pid=", pid_to_list(H#target.pid),
			", branch=", H#target.branch,
			", request=", Method, " ",
			sipurl:print(URI), ", ",
			RespStr,
			", cancelled=", H#target.cancelled,
			", state=" , H#target.state]),
    debugfriendly2(T, [binary_to_list(list_to_binary(Str)) | Res]).


%%--------------------------------------------------------------------
%% Function: get_using_pid(Pid, TargetList)
%%           Pid        = pid()
%%           TargetList = targetlist record()
%% Descrip.: Get the target with pid matching Pid from TargetList.
%% Returns : Target = target record() | none
%%--------------------------------------------------------------------
get_using_pid(Pid, TargetList) when is_pid(Pid), is_record(TargetList, targetlist) ->
    get_using_pid2(Pid, TargetList#targetlist.list).

get_using_pid2(_Pid, []) ->
    none;
get_using_pid2(Pid, [H | _T]) when is_record(H, target), H#target.pid == Pid ->
    H;
get_using_pid2(Pid, [H | T]) when is_record(H, target) ->
    get_using_pid2(Pid, T).


%%--------------------------------------------------------------------
%% Function: get_using_branch(Branch, TargetList)
%%           Branch     = string()
%%           TargetList = targetlist record()
%% Descrip.: Get the target with branch matching Branch.
%% Returns : Target = target record() | none
%%--------------------------------------------------------------------
get_using_branch(Branch, TargetList) when is_list(Branch), is_record(TargetList, targetlist) ->
    get_using_branch2(Branch, TargetList#targetlist.list).

get_using_branch2(_Branch, []) ->
    none;
get_using_branch2(Branch, [#target{branch=Branch}=H | _T]) ->
    H;
get_using_branch2(Branch, [H | T]) when is_record(H, target) ->
    get_using_branch2(Branch, T).


%%--------------------------------------------------------------------
%% Function: get_targets_in_state(State, TargetList)
%%           State      = term()
%%           TargetList = targetlist record()
%% Descrip.: Get all targets with state matching State.
%% Returns : TargetList = list() of target record()
%%--------------------------------------------------------------------
get_targets_in_state(State, TargetList) when is_record(TargetList, targetlist) ->
    get_targets_in_state2(State, TargetList#targetlist.list, []).

get_targets_in_state2(_State, [], Res) ->
    lists:reverse(Res);
get_targets_in_state2(State, [H | T], Res) when is_record(H, target), H#target.state == State ->
    get_targets_in_state2(State, T, [H | Res]);
get_targets_in_state2(State, [H | T], Res) when is_record(H, target) ->
    get_targets_in_state2(State, T, Res).


%%--------------------------------------------------------------------
%% Function: get_repsonses(TargetList)
%%           TargetList = targetlist record()
%% Descrip.: Get all responses that has been set (i.e. not undefined).
%% Returns : list() of response record() | {Status, Reason} tuple()
%%--------------------------------------------------------------------
get_responses(TargetList) when is_record(TargetList, targetlist) ->
    get_responses2(TargetList#targetlist.list, []).

get_responses2([], Res) ->
    lists:reverse(Res);
get_responses2([#target{endresult = H} | T], Res) when is_record(H, sp_response) ->
    %% endresult is an sp_response record, add it to Res
    get_responses2(T, [H | Res]);
get_responses2([#target{endresult = none} | T], Res) ->
    %% endresult is 'none'
    get_responses2(T, Res).

%%--------------------------------------------------------------------
%% Function: extract(Keys, Target)
%%           Keys = list() of atom(), pid | branch | request | state |
%%                  timeout | dstlist | endresult | cancelled
%%           Target = target record()
%% Descrip.: Extract one or more values from a target record. Return
%%           the values in a list of the same order as Keys.
%% Returns : list() of term()
%%--------------------------------------------------------------------
extract(Values, Target) when is_record(Target, target) ->
    extract(Values, Target, []).

extract([pid | T],		#target{pid = Value} = Ta, 		Res) ->	extract(T, Ta, [Value | Res]);
extract([branch | T],		#target{branch = Value} = Ta,		Res) ->	extract(T, Ta, [Value | Res]);
extract([request | T],		#target{request = Value} = Ta,		Res) ->	extract(T, Ta, [Value | Res]);
extract([state | T],		#target{state = Value} = Ta,		Res) ->	extract(T, Ta, [Value | Res]);
extract([timeout | T],		#target{timeout = Value} = Ta,		Res) ->	extract(T, Ta, [Value | Res]);
extract([dstlist | T],		#target{dstlist = Value} = Ta,		Res) ->	extract(T, Ta, [Value | Res]);
extract([endresult | T],	#target{endresult = Value} = Ta,	Res) ->	extract(T, Ta, [Value | Res]);
extract([cancelled | T],	#target{cancelled = Value} = Ta,	Res) ->	extract(T, Ta, [Value | Res]);
extract([user_instance | T],	#target{user_instance = Value} = Ta,	Res) ->	extract(T, Ta, [Value | Res]);
extract([],			#target{},				Res) -> lists:reverse(Res).

%%--------------------------------------------------------------------
%% Function: set_pid(Target, Value)
%%           set_state(Target, Value)
%%           set_endresult(Target, Value)
%%           set_dstlist(Target, Value)
%%           set_cancelled(Target, Value)
%%           Target = target record()
%%           Value  = term(), new value
%% Descrip.: Set functions.
%% Returns : NewTarget = target record()
%%--------------------------------------------------------------------
set_pid(Target, Value) when is_record(Target, target), is_pid(Value) ->
    Target#target{pid = Value}.
set_state(Target, Value) when is_record(Target, target), is_atom(Value) ->
    Target#target{state = Value}.
set_endresult(Target, Value) when is_record(Target, target), is_record(Value, sp_response) ->
    Target#target{endresult = Value}.
set_dstlist(Target, Value) when is_record(Target, target), is_list(Value) ->
    Target#target{dstlist = Value}.
set_cancelled(Target, Value) when is_record(Target, target), Value == true; Value == false ->
    Target#target{cancelled = Value}.


%%====================================================================
%% Internal functions
%%====================================================================



%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%%--------------------------------------------------------------------
test() ->
    %% test empty/0
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "emtpy/0 - 1"),
    #targetlist{list = []} = EmptyList = empty(),


    %% test add/7
    %%--------------------------------------------------------------------
    AddReq = #request{method = "TEST",
		      uri    = sipurl:parse("sip:test@example.org")
		     },

    autotest:mark(?LINE, "add/7 - 1"),
    %% just add an element
    List1 = add("branch1", AddReq, self(), trying, 4711, [123], {"user", "instance"}, EmptyList),
    
    autotest:mark(?LINE, "add/7 - 2"),
    %% test that we can't add another element with the same branch
    List1 = add("branch1", AddReq, self(), calling, 123, [234], none, List1),

    autotest:mark(?LINE, "add/7 - 3"),
    %% add another target
    List2 = add("branch2", AddReq, whereis(logger), completed, 123, [], {"user", "instance2"}, List1),

    autotest:mark(?LINE, "add/7 - 4"),
    %% another element
    List3 = add("branch3", AddReq, whereis(init), terminated, 345, [], {"user2", "instance"}, List2),

    autotest:mark(?LINE, "add/7 - 5"),
    %% another element
    List4 = add("branch4", AddReq, self(), trying, 345, [456], {"user2", "instance2"}, List3),
    

    %% test length/1
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_length/1 - 1"),
    %% check length
    1 = get_length(List1),

    autotest:mark(?LINE, "get_length/1 - 2"),
    %% check length
    4 = get_length(List4),


    %% test get_using_branch/2
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_using_branch/2 - 1"),
    %% check that we can get targets using branch
    Target1 = get_using_branch("branch1", List4),

    autotest:mark(?LINE, "get_using_branch/2 - 2"),
    %% check that we can get targets using branch
    none = get_using_branch("branch9", List4),


    %% test extract/2
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "extract/2 - 1"),
    %% check all the elements we added in the first target
    Extract_Me = self(),
    ["branch1", AddReq, Extract_Me, trying, 4711, [123], {"user", "instance"}] =
	extract([branch, request, pid, state, timeout, dstlist, user_instance], Target1),


    %% test get_using_pid/2
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_using_pid/2 - 1"),
    %% check that we can get targets using pid
    Target1 = get_using_pid(self(), List4),

    autotest:mark(?LINE, "get_using_pid/2 - 2"),
    %% check that we can get targets using pid (note: List2 does not have target 3)
    none = get_using_pid(whereis(init), List2),


    %% test get_targets_in_state/2
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_targets_in_state/2 - 1"),
    [#target{branch="branch3"}] = get_targets_in_state(terminated, List4),

    autotest:mark(?LINE, "get_targets_in_state/2 - 2"),
    [#target{branch="branch1"}, #target{branch="branch4"}] = get_targets_in_state(trying, List4),
    
    autotest:mark(?LINE, "get_targets_in_state/2 - 3"),
    [] = get_targets_in_state(none, List4),

    
    %% test debugfriendly/1
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "debugfriendly/1 - 1"),
    Debug1 = debugfriendly(List4),

    autotest:mark(?LINE, "debugfriendly/1 - 2"),
    %% check length of result, nothing more
    4 = length(Debug1),


    %% test update_target/2
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "update_target/2 - 1"),
    %% test update with no change
    List4 = update_target(Target1, List4),

    autotest:mark(?LINE, "update_target/2 - 2.1"),
    %% test update with small change
    Target1Response = #sp_response{status=404, reason="Not Found"},
    UpdatedTarget1 = set_endresult(Target1, Target1Response),
    UpdatedList1 = update_target(UpdatedTarget1, List4),
    
    autotest:mark(?LINE, "update_target/2 - 2.2"),
    %% verify that target was updated
    UpdatedTarget1 = get_using_branch("branch1", UpdatedList1),


    autotest:mark(?LINE, "update_target/2 - 3.1"),
    %% modify last target in the middle of list
    Target3 = get_using_branch("branch3", UpdatedList1),
    Target3Response = #sp_response{status=100, reason="Trying"},
    UpdatedTarget3 = set_endresult(Target3, Target3Response),

    autotest:mark(?LINE, "update_target/2 - 3.2"),
    %% verify that we can update last target in list
    UpdatedList3 = update_target(UpdatedTarget3, UpdatedList1),

    autotest:mark(?LINE, "update_target/2 - 3.3"),
    %% verify that target was updated
    UpdatedTarget3 = get_using_branch("branch3", UpdatedList3),


    autotest:mark(?LINE, "update_target/2 - 4.1"),
    %% modify last target in list
    Target4 = get_using_branch("branch4", UpdatedList3),
    Target4Response = #sp_response{status=400, reason="Bad Request"},
    UpdatedTarget4 = set_endresult(Target4, Target4Response),

    autotest:mark(?LINE, "update_target/2 - 4.2"),
    %% verify that we can update last target in list
    UpdatedList4 = update_target(UpdatedTarget4, UpdatedList3),

    autotest:mark(?LINE, "update_target/2 - 4.3"),
    %% verify that target was updated
    UpdatedTarget4 = get_using_branch("branch4", UpdatedList4),

    autotest:mark(?LINE, "update_target/2 - 5"),
    %% verify that we get an exception if we try to update non-existing target
    {error, update_of_non_existin_target} = (catch update_target(UpdatedTarget4#target{ref="update_target test 5"}, UpdatedList4)),


    %% test get_responses/1
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "update_target/2 - 1"),
    %% check that we get the valid response, but not the invalid one ('123') for target #2
    [Target1Response, Target3Response, Target4Response] = get_responses(UpdatedList4),

    ok.
