%%--------------------------------------------------------------------
%% Note: this module does some costly list operations, such as
%% appendning elements to the end of lists.
%% There may be a datastructure better suited for this need, of adding
%% elements to both front and end of lists, but note that most
%% #keyelem.item are probably short, so that the overhead of smarter
%% datastructures may still be more costly.
%% It may also be beneficial, when several elements are added to the
%% end of a list - to reverse it, append the new elements in revese
%% order and then reverse the new list back to it's proper order.
%% This results in O(3N+2M) rather than O(N * M) (aproximate - M
%% grows for each element in N) list hd/1 calls
%% (N = new list of elements to add, M = the orginal list). If N is
%% small compared to M this results in roughly a O(2M) vs a O(N*M)
%% algorithm.
%%--------------------------------------------------------------------

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
	 map/2,
	 normalize/1,
	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(keyelem, {
	  %% string() | atom(), stores a sip header field name
	  %% normalized to a standard format by normalize/1
	  key,
	  %% string(), stores a sip header field name in a
	  %% non-normalized form, may be in upper or lower case, or
	  %% short form
	  name,
	  %% list() of string(), stores field specific entries
	  item
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: fetch(Key, List)
%%           Key  = string() | atom(), either the header name (string)
%%                  or the internal form of it (atom).
%%           List = keylist record()
%% Descrip.: return the contents of the header that matches Key
%% Returns : list() of string(), the value of Key - Key may have
%%           several values associated with it (e.g. the "Via"
%%           header in sip requests)
%%--------------------------------------------------------------------
fetch(Key, List) when is_list(Key), is_record(List, keylist) ->
    fetchcase(normalize(Key), List#keylist.list);
fetch(Key, List) when is_atom(Key), is_record(List, keylist) ->
    fetchcase(Key, List#keylist.list).

fetchcase(_Key, []) ->
    [];
fetchcase(Key, [#keyelem{key=Key}=Elem | _List]) ->
    %% match
    Elem#keyelem.item;
fetchcase(Key, [Elem | List]) when is_record(Elem, keyelem) ->
    %% no match
    fetchcase(Key, List).

%%--------------------------------------------------------------------
%% Function: appendlist(Keylist, List)
%%           KeyList = keylist record()
%%           List    = list() of {Name, Item} or {Key, Name, Item}
%% Descrip.: do one append/2 for each entry in List
%% Returns : keylist record()
%%--------------------------------------------------------------------
%% {Name, Key, Item} tuples
appendlist(Keylist, [H | _] = List) when is_record(Keylist, keylist), size(H) == 3 ->
    Func = fun ({Key, Name, Item}, KeylistAcc) ->
		   append({Key, Name, Item}, KeylistAcc)
	   end,
    lists:foldl(Func, Keylist, List);
%% {Name, Item} tuples
appendlist(Keylist, [H | _] = List) when is_record(Keylist, keylist), size(H) == 2 ->
    Func = fun ({Name, Item}, KeylistAcc) ->
		   append({Name, Item}, KeylistAcc)
	   end,
    lists:foldl(Func, Keylist, List);
appendlist(Keylist, []) when is_record(Keylist, keylist) ->
    Keylist.

%%--------------------------------------------------------------------
%% Function: from_list(List)
%%           List = list() of {Name, Key, Item}
%% Descrip.: create an empty keylist and add the List elements with
%%           append/2
%% Returns : keylist record()
%%--------------------------------------------------------------------
from_list(List) when is_list(List) ->
    appendlist(empty(), List).

%% Function: empty()
%% Descrip.: Return an empty keylist.
%% Returns : keylist record()
empty() ->
    #keylist{list=[]}.

%%--------------------------------------------------------------------
%% Function: append({Name, NewValueList}, List)
%%           append({Name, Key, NewValueList}, List)
%%           Name = string(), the name of a header in a sip message
%%           Key  = atom() | list(), the key to use internally
%%           NewValueList = new entries for keyelem identified by Key
%%           List = keylist record()
%% Descrip.: add NewValueList to tail of element (#keyelem.item) in
%%           List (or create new entry if Key is unknown)
%% Returns : keylist record()
%%--------------------------------------------------------------------
append({Name, NewValueList}, Keylist) when is_list(Name), is_list(NewValueList),
					   is_record(Keylist, keylist) ->
    mod(Name, fun (Valuelist) ->
		      lists:append(Valuelist, NewValueList)
	      end, Keylist);
%% Key also given, we can call modcase() directly
append({Key, Name, NewValueList}, Keylist) when is_atom(Key); is_list(Key),
						is_list(Name), is_list(NewValueList),
						is_record(Keylist, keylist) ->
    ModifierFun = fun (Valuelist) ->
			  lists:append(Valuelist, NewValueList)
		  end,
    NewL = modcase(Key, Name, ModifierFun, Keylist#keylist.list),
    Keylist#keylist{list=NewL}.

%%--------------------------------------------------------------------
%% Function: prepend({Name, NewValueList}, List)
%%           Name = string(), the name of a header in a sip message
%%           NewValueList = new entries for keyelem identified by Key
%%           List = keylist record()
%% Descrip.: add NewValueList to head of element (#keyelem.item) in
%%           List (or create new entry if Key is unknown)
%% Returns : keylist record()
%% Note    : This function does not accept atom() form because if
%%           there is no header, one will be created and then we have
%%           to have a string() variant of it anyways.
%%--------------------------------------------------------------------
prepend({Name, NewValueList}, List) when is_list(Name), is_list(NewValueList),
					 is_record(List, keylist) ->
    mod(Name, fun (Valuelist) ->
		      lists:append(NewValueList, Valuelist)
	      end, List).

%%--------------------------------------------------------------------
%% Function: delete(Name, Keylist)
%%           Name = string() | atom(), either the name of a header in
%%                  a sip message (string) or the internal form of it
%%                  (atom)
%%           Keylist = keylist record()
%% Descrip.: remove an entry from keylist record() (#keylist.list)
%% Returns : keylist record()
%%--------------------------------------------------------------------
delete(Name, Keylist) when is_list(Name); is_atom(Name), is_record(Keylist, keylist) ->
    Key = normalize(Name),
    #keylist{list=del(Key, Keylist#keylist.list)}.

%% Descrip.: delete first element (E) in List,
%%           where E#keyelem.key == normalize(Key)
%% Returns : list() of keyelem records()
del(_Key, []) ->
    [];
del(Key, [H | T]) when is_record(H, keyelem), H#keyelem.key == Key ->
    %% Match, return remainder of list
    T;
del(Key, [H | T]) when is_record(H, keyelem) ->
    [H | del(Key, T)].

%%--------------------------------------------------------------------
%% Function: deletefirstvalue(Name, List)
%%           Name = string() | atom(), the name of a header in a sip
%%                  message (string), or our internal representation
%%                  of it (atom)
%%           List = keylist record()
%% Descrip.: remove the first entry from a element (#keyelem.item)
%%           in keylist record() (#keylist.list)
%% Returns : keylist record()
%%--------------------------------------------------------------------
deletefirstvalue(Name, Keylist) when is_list(Name); is_atom(Name), is_record(Keylist, keylist) ->
    mod(Name, fun (Valuelist) ->
		      case Valuelist of
			  [_Item | Rest] ->
			      Rest;
			  [] ->
			      []
		      end
	      end, Keylist).

%%--------------------------------------------------------------------
%% Function: set(Name, Valuelist, List)
%%           Nakme = string(), the name of a header in a sip message
%%           ValueList = list() of string()
%%           List = keylist record()
%% Descrip.: replace all items in E#keyelem.item where Key matches
%%           key in E
%% Returns : keylist record()
%%--------------------------------------------------------------------
set(Name, Valuelist, Keylist) when is_list(Name), is_record(Keylist, keylist) ->
    mod(Name, fun (_) ->
		      Valuelist
	      end, Keylist).

%%--------------------------------------------------------------------
%% Function: copy(Keylist, Keys)
%%           Keylist = list() of keyelem record()
%%           Keys = list() of string()
%% Descrip.: return Keylist only containing the fields in Keys
%% Returns : keylist record()
%% Note    : use of atom() as keys (for well known keys) should
%%           improve patternmatch speed. When matching a
%%           E#keyelem.key against Key in the alternative
%%           solution below, it may also be worth using one of the OTP
%%           set modules so that the member check becomes O(log N)
%%           instead of O(N).
%%--------------------------------------------------------------------
%% This code (untested) should do the same thing and should be less
%% confusing
%%
%% copy(Keylist, Names) when record(Keylist, keylist) ->
%%     Keys = normalize_list(Names),
%%     KeyElems = Keylist#keylist.list,
%%     Copies = [E ||
%% 		 %% determine if E from Keylist should be copied
%% 		 lists:member(E#keyelem.key, Keys),
%% 		 E <- KeyElems ],
%%     #keylist{list = Copies}.
%%--------------------------------------------------------------------

%% XXX I can't quite figure out how this works - hsten
%% XXX but it does work ;) - ft

copy(Keylist, Names) when is_record(Keylist, keylist), is_list(Names) ->
    Keys = normalize_list(Names),
    Func = fun(Key, _Name, _Item) ->
		   lists:member(Key, Keys)
	   end,
    %% filter() calls our Func on every element of Keylist. filter()
    %% returns all elements from Keylist where our Func returned
    %% 'true'. Func returns 'true' if the elements key is a member of
    %% Keys.
    filter(Func, Keylist).

%%--------------------------------------------------------------------
%% Function: filter(Func, Keylist)
%%           Func = fun(E) -> bool(), predicate function
%%           Keylist = list() of keyelem record()
%% Descrip.: Return all keyelem elements from Keylist, for which the
%%           predicate function returned 'true'.
%% Returns : keylist record()
%%--------------------------------------------------------------------
filter(Func, Keylist) when is_record(Keylist, keylist) ->
    Pred = fun(Elem) ->
		   Func(Elem#keyelem.key, Elem#keyelem.name, Elem#keyelem.item)
	   end,
    NewL = lists:filter(Pred, Keylist#keylist.list),
    #keylist{list=NewL}.

%%--------------------------------------------------------------------
%% Function: map(Func, Keylist)
%%           Func = fun(Key, Name, Item) -> term()
%%           Keylist = keylist record()
%% Descrip.: apply Func to all elements in Keylist (#keylist.list)
%% Returns : list() of term()
%%--------------------------------------------------------------------
map(Func, Keylist) when is_record(Keylist, keylist) ->
    F = fun(Elem) ->
		Func(Elem#keyelem.key, Elem#keyelem.name, Elem#keyelem.item)
	end,
    lists:map(F, Keylist#keylist.list).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: normalize(Key)
%%           Key = string(), the name of a header in a sip message
%% Descrip.: Convert header key to a standard format - either an atom
%%           for well known headers, or a lowercased string. We don't
%%           do list_to_atom() because that way it would be possible
%%           to exhaust our atom() space by sending us SIP requests
%%           with made-up headers.
%% Returns : string() | atom()
%%--------------------------------------------------------------------

%% This is to support the compact headers we are required to support
%% by RFC3261 #7.3.3.
normalize("i") -> normalize2("call-id");
normalize("m") -> normalize2("contact");
normalize("e") -> normalize2("content-encoding");
normalize("l") -> normalize2("content-length");
normalize("c") -> normalize2("contact-type");
normalize("f") -> normalize2("from");
normalize("s") -> normalize2("subject");
normalize("k") -> normalize2("supported");
normalize("t") -> normalize2("to");
normalize("v") -> normalize2("via");
%% RFC3515 #7
normalize("r") -> "refer-to";
normalize(Name) when is_list(Name) ->
    LC = httpd_util:to_lower(Name),
    normalize2(LC);
normalize(Key) when is_atom(Key) ->
    %% XXX perhaps check that it is one of the atoms we actually use?
    Key.

%% Convert well-known headers to atoms. Measurements shows that ordering
%% of the function clauses is not significant, but the length of the
%% string to match against is. Matching is really fast though - the
%% slowest one (longest string) takes about 72 ms to call 100k times.
normalize2("accept") ->			'accept';
normalize2("allow") ->			'allow';
normalize2("allow-events") ->		'allow-events';
normalize2("authorization") ->		'authorization';
normalize2("call-id") ->		'call-id';
normalize2("contact") ->		'contact';
normalize2("content-length") ->		'content-length';
normalize2("content-type") ->		'content-type';
normalize2("cseq") ->			'cseq';
normalize2("date") ->			'date';
normalize2("expires") ->		'expires';
normalize2("from") ->			'from';
normalize2("max-forwards") ->		'max-forwards';
normalize2("proxy-authenticate") ->	'proxy-authenticate';
normalize2("proxy-authorization") ->	'proxy-authorization';
normalize2("proxy-require") ->		'proxy-require';
normalize2("record-route") ->		'record-route';
normalize2("require") ->		'require';
normalize2("route") ->			'route';
normalize2("rtp-rxstat") ->		'rtp-rxstat';
normalize2("rtp-txstat") ->		'rtp-txstat';
normalize2("supported") ->		'supported';
normalize2("to") ->			'to';
normalize2("via") ->			'via';
normalize2("warning") ->		'warning';
normalize2("www-authenticate") ->	'www-authenticate';
normalize2("x-yxa-peer-auth") ->	'x-yxa-peer-auth';
normalize2(In) when is_list(In) ->	In.

%%--------------------------------------------------------------------
%% Function: normalize_list(Names)
%%           Names = list() of string() | atom(), the name of a header
%%                   in a sip message, or our internal variant of it
%% Descrip.: convert header key, to a standard format - lowercase and
%%           verbose string()
%% Returns : list() of string()
%%--------------------------------------------------------------------
normalize_list(Names) ->
    lists:map(fun(Key) ->
		      normalize(Key)
	      end, Names).

%%--------------------------------------------------------------------
%% Function: mod(Name, Func, List)
%%           Name = string(), the name of a header in a sip message
%%           Func = fun()
%%           List = keylist record()
%% Descrip.: apply Func to all elements E;
%%           E#keyelem{item = F(E#keyelem.item)} - in List, where
%%           E#keyelem.key == normalize(Key)
%%           A new #keyelem{key=normalize(Key),
%%                          name=Name,
%%                          item=Func([])}
%%           is added if Key isn't found
%% Returns : keylist record()
%%--------------------------------------------------------------------
mod(Name, Func, List) when is_atom(Name); is_list(Name), is_record(List, keylist) ->
    Key = normalize(Name),
    #keylist{list=modcase(Key, Name, Func, List#keylist.list)}.

modcase(Key, Name, Func, []) ->
    %% No more input - add a new element
    [#keyelem{key=Key,
	      name=Name,
	      item=Func([])}];
modcase(Key, _Name, Func, [#keyelem{key=Key}=Elem | List]) ->
    %% Match, process item using Func
    [Elem#keyelem{item=Func(Elem#keyelem.item)} | List];
modcase(Key, Name, Func, [Elem | List]) when is_record(Elem, keyelem) ->
    %% No match
    [Elem | modcase(Key, Name, Func, List)].

%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->

    %% normalize/1
    %%--------------------------------------------------------------------
    io:format("test: normalize/1 - 1~n"),
    via = normalize("Via"),

    io:format("test: normalize/1 - 2~n"),
    via = normalize("vIA"),

    io:format("test: normalize/1 - 3~n"),
    via = normalize(via),

    io:format("test: normalize/1 - 4~n"),
    "other" = normalize("OTHER"),

    io:format("test: normalize/1 - 5~n"),
    "other" = normalize("other"),

    io:format("test: normalize/1 - 6~n"),
    "other " = normalize("otheR "),

    io:format("test: normalize/1 - 7~n"),
    %% test short format
    from = normalize("f"),

    %% from_list/1
    %%--------------------------------------------------------------------
    io:format("test: from_list/1 - 1~n"),
    %% empty list
    #keylist{list=[]} = from_list([]),

    io:format("test: from_list/1 - 2~n"),
    %% list() of {Key, Name, Value} tuples
    H1 = from_list([{via, "Via", ["one", "two", "three"]},
		    {"test", "Test", ["foo", "bar", "baz"]}]),

    io:format("test: from_list/1 - 3~n"),
    %% check result
    #keylist{list=[
		   #keyelem{key=via, name="Via", item=["one", "two", "three"]},
		   #keyelem{key="test", name="Test", item=["foo", "bar", "baz"]}
		  ]} = H1,

    io:format("test: from_list/1 - 4~n"),
    %% list() of {Name, Value} tuples
    H2 = from_list([{"ATOM", [atom]}]),

    io:format("test: from_list/1 - 5~n"),
    %% check result
    #keylist{list=[
		   #keyelem{key="atom", name="ATOM", item=[atom]}
		  ]} = H2,
    %% fetch/2
    %%--------------------------------------------------------------------
    io:format("test: fetch/2 - 1~n"),
    %% fetch using name, will match key after normalize()
    ["one", "two", "three"] = fetch("Via", H1),

    io:format("test: fetch/2 - 2~n"),
    %% fetch using name, will match key after normalize()
    ["one", "two", "three"] = fetch("VIA", H1),

    io:format("test: fetch/2 - 3~n"),
    %% fetch using key
    ["one", "two", "three"] = fetch('via', H1),

    io:format("test: fetch/2 - 4~n"),
    %% when giving atom, it must be an exact match
    [] = fetch('Via', H1),

    io:format("test: fetch/2 - 5~n"),
    %% fetch using name
    [atom] = fetch("atom", H2),

    io:format("test: fetch/2 - 6~n"),
    %% fetch using name, will match after normalize()
    [atom] = fetch("ATOM", H2),

    %% append/2
    %%--------------------------------------------------------------------
    io:format("test: append/2 - 1~n"),
    %% {Name, Value} tuple
    AppendH1 = append({"Header", ["one"]}, empty()),

    io:format("test: append/2 - 2~n"),
    %% check result of append
    #keylist{list=[
		   #keyelem{key="header", name="Header", item=["one"]}
		  ]} = AppendH1,

    io:format("test: append/2 - 3~n"),
    %% {Key, Name, Value}
    AppendH2 = append({"header", "Header", ["two"]}, AppendH1),

    io:format("test: append/2 - 4~n"),
    %% check result of append
    #keylist{list=[
		   #keyelem{key="header", name="Header", item=["one", "two"]}
		  ]} = AppendH2,

    %% set/3
    %%--------------------------------------------------------------------
    io:format("test: set/3 - 1~n"),
    SetH1 = set("FOO", ["test"], empty()),

    io:format("test: set/3 - 2~n"),
    %% check results
    #keylist{list=[
		   #keyelem{key="foo", name="FOO", item=["test"]}
		  ]} = SetH1,

    io:format("test: set/3 - 3~n"),
    %% should not change SetH1
    SetH1 = set("foo", ["test"], SetH1),

    io:format("test: set/3 - 4~n"),
    SetH2 = set("bar", ["test2"], SetH1),

    io:format("test: set/3 - 5~n"),
    %% check results
    #keylist{list=[
		   #keyelem{key="foo", name="FOO", item=["test"]},
		   #keyelem{key="bar", name="bar", item=["test2"]}
		  ]} = SetH2,

    %% prepend/2
    %%--------------------------------------------------------------------
    io:format("test: prepend/3 - 1~n"),
    %% have one more keyelem in the test case to make sure prepend() only
    %% affects the element we want
    PrependH0 = keylist:set("Other", ["foo"], empty()),
    PrependH1 = prepend({"Header", ["one"]}, PrependH0),

    io:format("test: prepend/3 - 2~n"),
    %% check result
    #keylist{list=[
		   #keyelem{key="other", name="Other", item=["foo"]},
		   #keyelem{key="header", name="Header", item=["one"]}
		  ]} = PrependH1,

    io:format("test: prepend/3 - 3~n"),
    PrependH2 = prepend({"header", ["zero"]}, PrependH1),

    io:format("test: prepend/3 - 4~n"),
    %% check result
    #keylist{list=[
		   #keyelem{key="other", name="Other", item=["foo"]},
		   #keyelem{key="header", name="Header", item=["zero", "one"]}
		  ]} = PrependH2,

    %% delete/2
    %%--------------------------------------------------------------------
    io:format("test: delete/2 - 1~n"),
    %% add Via to PrependH2 so that we have one header with an atom key
    DeleteH1 = set("Via", ["bar"], PrependH2),

    io:format("test: delete/2 - 2~n"),
    %% delete the "other" element, leaving the 'via' and "header" elements
    DeleteH2 = delete("other", DeleteH1),

    io:format("test: delete/2 - 2~n"),
    %% check results
    #keylist{list=[
		   #keyelem{key="header", name="Header", item=["zero", "one"]},
		   #keyelem{key=via, name="Via", item=["bar"]}
		  ]} = DeleteH2,

    io:format("test: delete/2 - 2~n"),
    %% delete both the 'via' and "header" elements, leaving an empty list
    #keylist{list=[]} = delete("header", delete(via, DeleteH2)),

    %% deletefirstvalue/2
    %%--------------------------------------------------------------------
    io:format("test: deletefirstvalue/2 - 1~n"),
    DFVH1 = from_list([{via, "Via", ["one", "two", "three"]},
		       {"other", "Other", ["foo"]}]),

    io:format("test: deletefirstvalue/2 - 2~n"),
    DFVH2 = deletefirstvalue("Via", DFVH1),

    io:format("test: deletefirstvalue/2 - 3~n"),
    %% check results
    #keylist{list=[
		   #keyelem{key=via, name="Via", item=["two", "three"]},
		   #keyelem{key="other", name="Other", item=["foo"]}
		  ]},

    io:format("test: deletefirstvalue/2 - 3~n"),
    DFVH3 = deletefirstvalue("Other", DFVH2),

    %% check results, current deletefirstvalue doesn't remove the element
    %% but the result of a keylist:fetch() would be the same as if the
    %% element had been deleted (empty list)
    #keylist{list=[
		   #keyelem{key=via, name="Via", item=["two", "three"]},
		   #keyelem{key="other", name="Other", item=[]}
		  ]} = DFVH3,

    %% copy/2
    %%--------------------------------------------------------------------
    io:format("test: copy/2 - 1~n"),
    CopyH1 = from_list([{via, "Via", ["one", "two", "three"]},
			{"other", "Other", ["foo"]}]),

    io:format("test: copy/2 - 2~n"),
    %% should not change CopyH1
    CopyH1 = copy(CopyH1, [via, "other"]),

    io:format("test: copy/2 - 3~n"),
    %% should not change CopyH1
    CopyH1 = copy(CopyH1, [via, "oThEr"]),

    io:format("test: copy/2 - 3~n"),
    %% copy only via
    #keylist{list=[
		   #keyelem{key=via, name="Via", item=["one", "two", "three"]}
		  ]} = copy(CopyH1, [via]),

    io:format("test: copy/2 - 3~n"),
    %% copy headers not in source, result in empty keylist
    #keylist{list=[]} = copy(CopyH1, [warning, 'call-id', "foobar"]),

    %% appendlist/2
    %%--------------------------------------------------------------------
    io:format("test: appendlist/2 - 1~n"),
    %% {Name, Value} tuples
    AppendLH1 = appendlist(empty(), [{"ViA", ["one"]}, {"via", ["two"]}]),

    io:format("test: appendlist/2 - 2~n"),
    %% check result
    #keylist{list=[
		   #keyelem{key=via, name="ViA", item=["one", "two"]}
		  ]} = AppendLH1,

    io:format("test: appendlist/2 - 3~n"),
    %% {Name, Key, Value} tuples
    AppendLH2 = appendlist(AppendLH1, [{via, "Via", ["three"]},
				       {"other", "Other", ["X"]}
				      ]),

    io:format("test: appendlist/2 - 4~n"),
    %% check result
    #keylist{list=[
		   #keyelem{key=via, name="ViA", item=["one", "two", "three"]},
		   #keyelem{key="other", name="Other", item=["X"]}
		  ]} = AppendLH2,

    %% map/2
    %%--------------------------------------------------------------------
    io:format("test: map/2 - 1~n"),
    CheckFun = fun(Key, Name, Item) ->
		       case {Key, Name, Item} of
			   {one, "one", ["one"]} -> ok;
			   {two, "TWO", ["TWO"]} -> ok
		       end
	       end,
    MapH1 = appendlist(empty(), [{one, "one", ["one"]},
				 {two, "TWO", ["TWO"]}
				]),

    io:format("test: map/2 - 2~n"),
    map(CheckFun, MapH1),

    MapH2 = appendlist(MapH1, [{three, "three", ["thr33"]}]),

    io:format("test: map/2 - 3~n"),
    %% make sure CheckFun does not pass MapH2
    {'EXIT', {{case_clause, _}, _}} = (catch map(CheckFun, MapH2)),

    ok.
