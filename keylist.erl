-module(keylist).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 fetch/2,
	 from_list/1,
	 append/2,
	 prepend/2,
	 delete/2,
	 deletefirstvalue/2,
	 set/3,
	 copy/2,
	 appendlist/2,
	 map/2
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(keyelem, {
	  %% string(), stores a sip header field name in a
	  %% non-normalized form, may be in upper or lower case, or
	  %% short form
	  key,
	  %% string(), stores a sip header field name normalized to a
	  %% standard format by to_lower/1
	  casekey,
	  item     % list() of string(), stores field specific entries
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: fetch(Key, List)
%%           Key = string(), the name of a field in a sip message
%%           header - all RFC 3261 formats are accepted
%%           List = keylist record()
%% Descrip.: return the contens that of the header that matches Key
%% Returns : E#keyelem.item
%%--------------------------------------------------------------------
fetch(Key, List) when record(List, keylist) ->
    fetchcase(to_lower(Key), List#keylist.list);
fetch(Key, List) ->
    erlang:fault("Keylist not wellformed", [Key, List]).

fetchcase(Key, []) ->
    [];
fetchcase(Key, [Elem | List]) when record(Elem, keyelem), Elem#keyelem.casekey == Key ->
    Elem#keyelem.item;
fetchcase(Key, [Elem | List]) when record(Elem, keyelem) ->
    fetchcase(Key, List);
fetchcase(Key, [Elem | List]) ->
    erlang:fault("Keylist element not wellformed", [Key, [Elem | List]]).

%%--------------------------------------------------------------------
%% Function: appendlist(Keylist, List)
%%           Keylist = list() of {Key, Item}
%%           List = keylist record()
%% Descrip.: do one append/2 for each entry in Keylist
%% Returns : keylist record()
%%--------------------------------------------------------------------
appendlist(Keylist, List) when record(Keylist, keylist) ->
    Func = fun ({Key, Item}, KeylistAcc) ->
		   append({Key, Item}, KeylistAcc)
	   end,
    lists:foldl(Func, Keylist, List).

%%--------------------------------------------------------------------
%% Function: from_list(List)
%%           List = list() of {Key, Item}
%% Descrip.: create a empty keylist and add the List elements with
%%           append/2
%% Returns : keylist record()
%%--------------------------------------------------------------------
from_list(List) ->
    appendlist(empty(), List).

%% Function: empty()
%% Descrip.: Return an empty keylist.
%% Returns : keylist record()
empty() ->
    #keylist{list=[]}.

%%--------------------------------------------------------------------
%% Function: append({Key, NewValueList}, List)
%%           Key = string(), the name of a field in a sip message
%%           NewValueList = new entries for keyelem identified by Key
%%           List = keylist record()
%% Descrip.: add NewValueList to tail of element (#keyelem.item) in
%%           List (or create new entry if Key is unkown)
%% Returns : keylist record()
%%--------------------------------------------------------------------
append({Key, NewValueList}, List) when record(List, keylist) ->
    mod(Key, fun (Valuelist) ->
			 lists:append(Valuelist, NewValueList)
		 end, List).

%%--------------------------------------------------------------------
%% Function: prepend({Key, NewValueList}, List)
%%           Key = string(), the name of a field in a sip message
%%           NewValueList = new entries for keyelem identified by Key
%%           List = keylist record()
%% Descrip.: add NewValueList to head of element (#keyelem.item) in
%%           List (or create new entry if Key is unkown)
%% Returns : keylist record()
%%--------------------------------------------------------------------
prepend({Key, NewValueList}, List) when record(List, keylist) ->
    mod(Key, fun (Valuelist) ->
			 lists:append(NewValueList, Valuelist)
		 end, List).

%%--------------------------------------------------------------------
%% Function: delete(Key, List)
%%           Key = string(), the name of a field in a sip message
%%           List = keylist record()
%% Descrip.: remove a entry from keylist record() (#keylist.list)
%% Returns : keylist record()
%%--------------------------------------------------------------------
delete(Key, List) when record(List, keylist) ->
    Casekey = to_lower(Key),
    #keylist{list=del(Casekey, List#keylist.list)}.

%% Descrip.: delete first element (E) in List,
%%           where E#keyelem.casekey == to_lower(Key)
%% Returns : list() of keyelem records()
del(Key, []) ->
    [];
del(Key, [Elem | List]) when record(Elem, keyelem), Elem#keyelem.casekey == Key ->
    List;
del(Key, [Elem | List]) when record(Elem, keyelem) ->
    [Elem | del(Key, List)].

%%--------------------------------------------------------------------
%% Function: deletefirstvalue(Key, List)
%%           Key = string(), the name of a field in a sip message
%%           List = keylist record()
%% Descrip.: remove the first entry from a element (#keyelem.item)
%%           in keylist record() (#keylist.list)
%% Returns : keylist record()
%%--------------------------------------------------------------------
deletefirstvalue(Key, List) when record(List, keylist) ->
    mod(Key, fun (Valuelist) ->
			 case Valuelist of
			     [Item | Rest] ->
				 Rest;
			     [] ->
				 []
			 end
		 end, List).

%%--------------------------------------------------------------------
%% Function: set(Key, Valuelist, List)
%%           Key = string(), the name of a field in a sip message
%%           ValueList =
%%           List = keylist record()
%% Descrip.: replace all items in E#keyelem.item where Key matches
%%           key in E
%% Returns : keylist record()
%%--------------------------------------------------------------------
set(Key, Valuelist, List) when record(List, keylist) ->
    mod(Key, fun (_) ->
			 Valuelist
		 end, List).

%%--------------------------------------------------------------------
%% Function: copy(Keylist, Keys)
%%           Keylist = list() of keyelem record()
%%           Keys = list() of string()
%% Descrip.: return Keylist only containing the fields in Keys
%% Returns : keylist record()
%%--------------------------------------------------------------------
copy(Keylist, Keys) when record(Keylist, keylist) ->
    Casekeys = to_lower_list(Keys),
    Func = fun(Key, Item) ->
		   lists:member(Key, Casekeys)
	   end,
    filter(Func, Keylist).

%% Function: filter(Func, Keylist)
%%           Func = fun(E) -> bool(), predicate function
%%           Keylist = list() of keyelem record()
%% Descrip.:
%% Returns : keylist record()
filter(Func, Keylist) when record(Keylist, keylist) ->
    Pred = fun(Elem) ->
		   Func(Elem#keyelem.casekey, Elem#keyelem.item)
	   end,
    #keylist{list=lists:filter(Pred, Keylist#keylist.list)}.

%%--------------------------------------------------------------------
%% Function: map(Func, Keylist)
%%           Func = fun(Key, Casekey, Item) -> term()
%%           Keylist = keylist record()
%% Descrip.: apply Func to all elements in Keylist (#keylist.list)
%% Returns : list() of term()
%%--------------------------------------------------------------------
map(Func, Keylist) when record(Keylist, keylist) ->
    Pred = fun(Elem) ->
		   Func(Elem#keyelem.key, Elem#keyelem.casekey, Elem#keyelem.item)
	   end,
    lists:map(Pred, Keylist#keylist.list).


%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: to_lower(Key)
%%           Key = string(), the name of a field in a sip message
%% Descrip.: convert header key, to a standard format - lowercase and
%%           verbose string()
%% Returns : string()
%% XXX something like "normalize" might be a more descriptive name
%% divulging the true purpouse of "to_lower"
%%--------------------------------------------------------------------

% This is to support the compact headers we are required to support
% by RFC3261 7.3.3.
to_lower("i") -> "call-id";
to_lower("m") -> "contact";
to_lower("e") -> "content-encoding";
to_lower("l") -> "content-length";
to_lower("c") -> "contact-type";
to_lower("f") -> "from";
to_lower("s") -> "subject";
to_lower("k") -> "supported";
to_lower("t") -> "to";
to_lower("v") -> "via";
% RFC3515 #7
to_lower("r") -> "refer-to";
to_lower(Key) when list(Key) ->
    httpd_util:to_lower(Key);
to_lower(Key) ->
    Key.

%%--------------------------------------------------------------------
%% Function: to_lower_list(Keys)
%%           Keys = list() of string(), the name of a field in a sip
%%           message
%% Descrip.: convert header key, to a standard format - lowercase and
%%           verbose string()
%% Returns : list() of string()
%%--------------------------------------------------------------------
to_lower_list(Keys) ->
    lists:map(fun(Key) ->
		      to_lower(Key)
	      end, Keys).

%%--------------------------------------------------------------------
%% Function: mod(Key, Func, List)
%%           Key = string(), the name of a field in a sip message
%%           Func = fun()
%%           List = keylist record()
%% Descrip.: apply Func to all elements E;
%%           E#keyelem{item = F(E#keyelem.item)} - in List, where
%%           E#keyelem.casekey == to_lower(Key)
%%           A new #keyelem{key=Key,
%%                          casekey=to_lower(CaseKey),
%%                          item=Func([])}
%%           is added if Key isn't found
%% Returns : keylist record()
%%--------------------------------------------------------------------
mod(Key, Func, List) when record(List, keylist) ->
    Casekey = to_lower(Key),
    #keylist{list=modcase(Key, Casekey, Func, List#keylist.list)}.

modcase(Key, Casekey, Func, []) ->
    [#keyelem{key=Key,
	      casekey=Casekey,
	      item=Func([])}];
%% stop after first addition
modcase(Key, Casekey, Func, [Elem | List]) when record(Elem, keyelem), Elem#keyelem.casekey == Casekey ->
    [Elem#keyelem{item=Func(Elem#keyelem.item)} | List];
modcase(Key, Casekey, Func, [Elem | List]) when record(Elem, keyelem) ->
    [Elem | modcase(Key, Casekey, Func, List)].


%%--------------------------------------------------------------------
%% Note: It may be preferable to store the #keyelem.casekey field as
%% a atom() rather than a string() - this should speed up
%% patternmatching, but don't store random strings as atom(), as
%% atoms are not GCed. It may be possible to convert the headers tags
%% to atom() when parsed initialy so that yxa can treat them as atom()
%% internaly and only output them as string().
%%
%% Note: this module does some costly list operations, appendning
%% elements to the end of lists.
%% There may be datastructure better suited for this need, of adding
%% elements to both front and end of lists, but note that most
%% #keyelem.item are probably short, so that the overhead of smarter
%% datastructures may still be more costly.
%% It may also be benificial, when several elements are added to the
%% end of a list - to reverse it, append the new elements in revese
%% order and then reverse the new list back to it's proper order.
%% This results in O(3N+2M) rather than O(N * M) (aproximate - M
%% grows for each element in N) list hd/1 calls
%% (N = new list of elements to add, M = the orginal list). If N is
%% small compared to M this results in roughly a O(2M) vs a O(N*M)
%% algorithm.
%%--------------------------------------------------------------------
