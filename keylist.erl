-module(keylist).
-export([fetch/2, from_list/1, append/2, prepend/2,
	 delete/2, deletefirstvalue/2, set/3, copy/2, appendlist/2, map/2]).

-record(keylist, {list}).
-record(keyelem, {key, casekey, item}).

to_lower(Key) when list(Key) ->
    httpd_util:to_lower(Key);
to_lower(Key) ->
    Key.

to_lower_list(Keys) ->
    lists:map(fun(Key) ->
		      to_lower(Key)
	      end, Keys).

fetch(Key, List) when record(List, keylist) ->
    fetchcase(to_lower(Key), List#keylist.list);
fetch(Key, List) ->
    throw({error, {"Keylist not wellformed", List}}).

fetchcase(Key, []) ->
    [];
fetchcase(Key, [Elem | List]) when record(Elem, keyelem), Elem#keyelem.casekey == Key ->
    Elem#keyelem.item;
fetchcase(Key, [Elem | List]) when record(Elem, keyelem) ->
    fetchcase(Key, List);
fetchcase(Key, [Elem | List]) ->
    throw({error, {"Keylist element not wellformed", Elem}}).

empty() ->
    #keylist{list=[]}.

appendlist(Keylist, List) when record(Keylist, keylist) ->
    Func = fun ({Key, Item}, KeylistAcc) ->
		   append({Key, Item}, KeylistAcc)
	   end,
    lists:foldl(Func, Keylist, List).

from_list(List) ->
    appendlist(empty(), List).

append({Key, NewValueList}, List) when record(List, keylist) ->
    mod(Key, fun (Valuelist) ->
			 lists:append(Valuelist, NewValueList)
		 end, List).

prepend({Key, NewValueList}, List) when record(List, keylist) ->
    mod(Key, fun (Valuelist) ->
			 lists:append(NewValueList, Valuelist)
		 end, List).

delete(Key, List) when record(List, keylist) ->
    Casekey = to_lower(Key),
    #keylist{list=del(Casekey, List#keylist.list)}.

deletefirstvalue(Key, List) when record(List, keylist) ->
    mod(Key, fun (Valuelist) ->
			 case Valuelist of
			     [Item | Rest] ->
				 Rest;
			     [] ->
				 []
			 end
		 end, List).

set(Key, Valuelist, List) when record(List, keylist) ->
    mod(Key, fun (_) ->
			 Valuelist
		 end, List).

mod(Key, Func, List) when record(List, keylist) ->
    Casekey = to_lower(Key),
    #keylist{list=modcase(Key, Casekey, Func, List#keylist.list)}.

modcase(Key, Casekey, Func, []) ->
    [#keyelem{key=Key,
	      casekey=Casekey,
	      item=Func([])}];
modcase(Key, Casekey, Func, [Elem | List]) when record(Elem, keyelem), Elem#keyelem.casekey == Casekey ->
    [Elem#keyelem{item=Func(Elem#keyelem.item)} | List];
modcase(Key, Casekey, Func, [Elem | List]) when record(Elem, keyelem) ->
    [Elem | modcase(Key, Casekey, Func, List)].

del(Key, []) ->
    [];
del(Key, [Elem | List]) when record(Elem, keyelem), Elem#keyelem.casekey == Key ->
    List;
del(Key, [Elem | List]) when record(Elem, keyelem) ->
    [Elem | del(Key, List)].

filter(Func, Keylist) when record(Keylist, keylist) ->
    Pred = fun(Elem) ->
		   Func(Elem#keyelem.casekey, Elem#keyelem.item)
	   end,
    #keylist{list=lists:filter(Pred, Keylist#keylist.list)}.

copy(Keylist, Keys) when record(Keylist, keylist) ->
    Casekeys = to_lower_list(Keys),
    Func = fun(Key, Item) ->
		   lists:member(Key, Casekeys)
	   end,
    filter(Func, Keylist).

map(Func, Keylist) when record(Keylist, keylist) ->
    Pred = fun(Elem) ->
		   Func(Elem#keyelem.key, Elem#keyelem.casekey, Elem#keyelem.item)
	   end,
    lists:map(Pred, Keylist#keylist.list).
