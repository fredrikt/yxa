-module(keylist).
-export([fetch/2, keys/1, from_list/1, append/2, prepend/2,
	 delete/2, deletefirstvalue/2, set/3]).

listfetch(Key, []) ->
    [];
listfetch(Key, [{Key, Item} | List]) ->
    [Item | listfetch(Key, List)];
listfetch(Key, [{_, _} | List]) ->
    listfetch(Key, List).

fetch(Key, List) when list(Key) ->
    fetchcase(httpd_util:to_lower(Key), List);
fetch(Key, List) ->
    fetchcase(Key, List).

fetchcase(Key, []) ->
    [];
fetchcase(Key, [{Key, Item} | List]) ->
    Item;
fetchcase(Key, [{_, _} | List]) ->
    fetchcase(Key, List).


keys([]) ->
    [];
keys([{Key, Item1}, {Key, Item2} | List]) ->
    keys([{Key, Item2} | List]);
keys([{Key, _} | List]) ->
    [Key | keys(List)].

from_list(List) ->
    Keys = keys(List),
    Func = fun (Item) ->
		   {Item, listfetch(Item, List)}
	   end,
    lists:map(Func, Keys).

append({Key, Value}, List) ->
    Casekey = httpd_util:to_lower(Key),
    mod(Casekey, fun (Valuelist) ->
			 lists:append(Valuelist, [Value])
		 end, List).

prepend({Key, Value}, List) ->
    Casekey = httpd_util:to_lower(Key),
    mod(Casekey, fun (Valuelist) ->
			 lists:append([Value], Valuelist)
		 end, List).

delete(Key, List) ->
    Casekey = httpd_util:to_lower(Key),
    del(Casekey, List).

deletefirstvalue(Key, List) ->
    Casekey = httpd_util:to_lower(Key),
    mod(Casekey, fun (Valuelist) ->
			 case Valuelist of
			     [Item | Rest] ->
				 Rest;
			     [] ->
				 []
			 end
		 end, List).

set(Key, Valuelist, List) ->
    Casekey = httpd_util:to_lower(Key),
    mod(Casekey, fun (_) ->
			 Valuelist
		 end, List).

mod(Key, Func, []) ->
    [{Key, Func([])}];
mod(Key, Func, [{Key, Valuelist} | List]) ->
    [{Key, Func(Valuelist)} | List];
mod(Key, Func, [{Key2, Valuelist} | List]) ->
    [{Key2, Valuelist} | mod(Key, Func, List)].

del(Key, []) ->
    [];
del(Key, [{Key, Valuelist} | List]) ->
    List;
del(Key, [{Key2, Valuelist} | List]) ->
    [{Key2, Valuelist} | del(Key, List)].
