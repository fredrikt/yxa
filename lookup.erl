-module(lookup).
-export([lookupregexproute/1, lookupuser/1, isours/1, homedomain/1]).

-include("database_regexproute.hrl").

lookupregexproute(User) ->
    {atomic, Routes} = database_regexproute:list(),
    Sortedroutes = lists:sort(fun (Elem1, Elem2) -> 
				      Prio1 = lists:keysearch(priority, 1, Elem1#regexproute.flags),
				      Prio2 = lists:keysearch(priority, 1, Elem2#regexproute.flags),
				      case {Prio1, Prio2} of
					  {_, false} ->
					      true;
					  {false, _} ->
					      false;
					  {{value, {priority,P1}}, {value, {priority,P2}}} when P1 >= P2 ->
					      true;
					  _ ->
					      false
				      end
			      end, Routes),
    Rules = lists:map(fun(R) ->
			      {R#regexproute.regexp, sipurl:print(R#regexproute.address)}
		      end, Sortedroutes),
    case util:regexp_rewrite(User, Rules) of
	nomatch ->
	    none;
	Result ->
	    {proxy, sipurl:parse(Result)}
    end.

lookupuser(User) ->
    case phone:get_phone(User) of
	{atomic, []} ->
	    Loc = lookupregexproute(User),
	    logger:log(debug, "Routing: Phone-lookup of ~p -> ~p", [User, Loc]),
	    Loc;
	{atomic, Locations} ->
	    {Location, _, _, _} = siprequest:location_prio(Locations),
	    logger:log(debug, "Routing: Phone-lookup of ~s -> ~p", [User, Location]),
	    case Location of
		{error, Errorcode} ->
		    {error, Errorcode};
		_ ->
		    {proxy, Location}
	    end
    end.

isours(Number) ->
    case phone:get_users_for_number(Number) of
	{atomic, []} ->
	    false;
	{atomic, _} ->
	    true;
	{aborted, _} ->
	    false
    end.

homedomain(Domain) ->
    util:casegrep(Domain, sipserver:get_env(homedomain)).
