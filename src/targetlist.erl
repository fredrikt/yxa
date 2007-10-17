%%%-------------------------------------------------------------------
%%% File    : targetlist.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Targetlist is a module for managing a list of ongoing
%%%           client transactions for sipproxy.
%%% @since    28 Jun 2003 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
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
%% @type targetlist() = #targetlist{}.
%%                      Container record to make sure noone tries to modify our
%%                      records.
-record(targetlist, {list}).

%% @type target() = #target{}.
%%                  no description
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
%% @spec    (Branch, Request, Pid, State, Timeout, DstList, UserInst,
%%          TargetList) ->
%%            NewTargetList
%%
%%            Branch     = string()
%%            Request    = #request{}
%%            Pid        = pid()
%%            State      = atom()
%%            Timeout    = integer()
%%            DstList    = [#sipdst{}]
%%            UserInst   = none | {User, Instance}
%%            TargetList = #targetlist{}
%%
%%            NewTargetList = #targetlist{}
%%
%% @doc     Add a new entry to TargetList, after verifying that a
%%          target with this branch is not already in the list.
%% @end
%%--------------------------------------------------------------------
add(Branch, Request, Pid, State, Timeout, DstList, UserInst, TargetList)
  when is_list(Branch), is_record(Request, request), is_pid(Pid), is_atom(State), is_integer(Timeout),
       is_list(DstList), is_record(TargetList, targetlist), (is_tuple(UserInst) orelse UserInst == none) ->
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
%% @spec    () ->
%%            TargetList
%%
%%            TargetList = #targetlist{}
%%
%% @doc     Return empty targetlist record. For initialization.
%% @end
%%--------------------------------------------------------------------
empty() ->
    #targetlist{list = []}.

%%--------------------------------------------------------------------
%% @spec    (TargetList) ->
%%            Length
%%
%%            TargetList = #targetlist{}
%%
%%            Length = integer()
%%
%% @doc     Return length of list encapsulated in the TargetList
%%          record.
%% @end
%%--------------------------------------------------------------------
get_length(TargetList) when is_record(TargetList, targetlist) ->
    length(TargetList#targetlist.list).

%%--------------------------------------------------------------------
%% @spec    (Target, TargetList) ->
%%            NewTargetList 
%%
%%            Target     = #target{}
%%            TargetList = #targetlist{}
%%
%%            NewTargetList = [#target{}]
%%
%% @throws  {error, update_of_non_existin_target} 
%%
%% @doc     Locate the old instance of the target Target in the
%%          TargetList and exchange it with Target.
%% @end
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
%% @spec    (TargetList) ->
%%            Data
%%
%%            TargetList = #targetlist{}
%%
%%            Data = term()
%%
%% @doc     Format the entrys in TargetList in a way that is suitable
%%          for logging using ~p.
%% @end
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
%% @spec    (Pid, TargetList) ->
%%            Target
%%
%%            Pid        = pid()
%%            TargetList = #targetlist{}
%%
%%            Target = #target{} | none
%%
%% @doc     Get the target with pid matching Pid from TargetList.
%% @end
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
%% @spec    (Branch, TargetList) ->
%%            Target
%%
%%            Branch     = string()
%%            TargetList = #targetlist{}
%%
%%            Target = #target{} | none
%%
%% @doc     Get the target with branch matching Branch.
%% @end
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
%% @spec    (State, TargetList) ->
%%            TargetList
%%
%%            State      = term()
%%            TargetList = #targetlist{}
%%
%%            TargetList = [#target{}]
%%
%% @doc     Get all targets with state matching State.
%% @end
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
%% @spec    (TargetList) ->
%%            [Response]
%%
%%            TargetList = #targetlist{}
%%
%%            Response = #sp_response{} | {Status, Reason}
%%            Status   = integer() "SIP status code"
%%            Reason   = string() "SIP reason phrase"
%%
%% @doc     Get all responses that has been set (i.e. not undefined).
%% @end
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
%% @spec    (Keys, Target) -> [term()]
%%
%%            Keys   = [pid       |
%%                     branch     |
%%                     request    |
%%                     state      |
%%                     timeout    |
%%                     dstlist    |
%%                     endresult  |
%%                     cancelled]
%%            Target = #target{}
%%
%% @doc     Extract one or more values from a target record. Return
%%          the values in a list of the same order as Keys.
%% @end
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
%% @spec    (Target, Value) ->
%%            NewTarget
%%
%%            Target = #target{}
%%            Value  = pid()
%%
%%            NewTarget = #target{}
%%
%% @doc     Update an element in a target.
%% @end
%%--------------------------------------------------------------------
set_pid(Target, Value) when is_record(Target, target), is_pid(Value) ->
    Target#target{pid = Value}.

%%--------------------------------------------------------------------
%% @spec    (Target, Value) ->
%%            NewTarget
%%
%%            Target = #target{}
%%            Value  = atom()
%%
%%            NewTarget = #target{}
%%
%% @doc     Update an element in a target.
%% @end
%%--------------------------------------------------------------------
set_state(Target, Value) when is_record(Target, target), is_atom(Value) ->
    Target#target{state = Value}.

%%--------------------------------------------------------------------
%% @spec    (Target, Value) ->
%%            NewTarget
%%
%%            Target = #target{}
%%            Value  = #sp_response{}
%%
%%            NewTarget = #target{}
%%
%% @doc     Update an element in a target.
%% @end
%%--------------------------------------------------------------------
set_endresult(Target, Value) when is_record(Target, target), is_record(Value, sp_response) ->
    Target#target{endresult = Value}.

%%--------------------------------------------------------------------
%% @spec    (Target, Value) ->
%%            NewTarget
%%
%%            Target = #target{}
%%            Value  = [term()]
%%
%%            NewTarget = #target{}
%%
%% @doc     Update an element in a target.
%% @end
%%--------------------------------------------------------------------
set_dstlist(Target, Value) when is_record(Target, target), is_list(Value) ->
    Target#target{dstlist = Value}.

%%--------------------------------------------------------------------
%% @spec    (Target, Value) ->
%%            NewTarget
%%
%%            Target = #target{}
%%            Value  = true | false
%%
%%            NewTarget = #target{}
%%
%% @doc     Update an element in a target.
%% @end
%%--------------------------------------------------------------------
set_cancelled(Target, Value) when is_record(Target, target), is_boolean(Value) ->
    Target#target{cancelled = Value}.


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
