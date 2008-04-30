%%%-------------------------------------------------------------------
%%% File    : keylist.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Kind of generic key-value list module, but really only
%%%           meant/used for storing SIP message headers.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%
%%% Note    : This module does some costly list operations, such as
%%%           appending elements to the end of lists.
%%%           There may be a datastructure better suited for this
%%%           need, of adding elements to both front and end of lists,
%%%           but note that most #keyelem.value are probably short, so
%%%           the overhead of smarter datastructures may still be more
%%%           costly.
%%%           It may also be beneficial, when several elements are
%%%           added to the end of a list - to reverse it, append the
%%%           new elements in reverse order and then reverse the new
%%%           list back to it's proper order.
%%%           This results in O(3N+2M) rather than O(N * M)
%%%           (aproximate - M grows for each element in N) list hd/1
%%%           calls (N = new list of elements to add, M = the orginal
%%%           list). If N is small compared to M this results in
%%%           roughly a O(2M) vs a O(N*M) algorithm.
%%%-------------------------------------------------------------------
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
%% @type keyelem() = #keyelem{}.
%%                   no description
-record(keyelem, {
	  %% string() | atom(), stores a sip header field name
	  %% normalized to a standard format by normalize/1
	  key,
	  %% string(), stores a sip header field name in a
	  %% non-normalized form, may be in upper or lower case, or
	  %% short form
	  name,
	  %% list() of string(), stores field specific entries
	  value
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Key, Keylist) ->
%%            [string()]
%%
%%            Key     = string() | atom() "either the header name (string) or the internal form of it (atom)."
%%            Keylist = #keylist{}
%%
%% @doc     Return the contents of the header that matches Key. The
%%          result is a list of values, or the empty list if no header
%%          matching Key was found in Keylist.
%% @end
%%--------------------------------------------------------------------
fetch(Key, #keylist{list = List}) when is_list(Key) ->
    fetchcase(normalize(Key), List);
fetch(Key, #keylist{list = List}) when is_atom(Key) ->
    fetchcase(Key, List).

fetchcase(_Key, []) ->
    [];
fetchcase(Key, [#keyelem{key = Key} = Elem | _List]) ->
    %% match
    Elem#keyelem.value;
fetchcase(Key, [Elem | List]) when is_record(Elem, keyelem) ->
    %% no match
    fetchcase(Key, List).

%%--------------------------------------------------------------------
%% @spec    (Keylist, List) -> #keylist{}
%%
%%            KeyList  = #keylist{}
%%            List     = [NewEntry]
%%            NewEntry = {Name, Value} | {Key, Name, Value}
%%            Name     = string() "the name of a header in a sip message (e.g. \"From\")"
%%            Key      = atom() | string() "the key to use internally (e.g. 'from')"
%%            Value    = [string()] "header value(s)"
%%
%% @doc     Do one append/2 for each entry in List.
%% @end
%%--------------------------------------------------------------------
appendlist(Keylist, []) when is_record(Keylist, keylist) ->
    Keylist;
appendlist(Keylist, List) when is_record(Keylist, keylist) ->
    Func = fun ({Key, Name, Value}, KeylistAcc) ->
		   append({Key, Name, Value}, KeylistAcc);
	       ({Name, Value}, KeylistAcc) ->
		   append({Name, Value}, KeylistAcc)
	   end,
    lists:foldl(Func, Keylist, List).

%%--------------------------------------------------------------------
%% @spec    (List) -> #keylist{}
%%
%%            List     = [NewEntry]
%%            NewEntry = {Name, Value} | {Key, Name, Value}
%%            Key      = atom() | string() "the key to use internally (e.g. 'from')"
%%            Name     = string() "the name of a header in a sip message (e.g. \"From\")"
%%            Value    = [string()] "header value(s)"
%%
%% @doc     Create an empty keylist and add the entrys in List to it.
%% @end
%%--------------------------------------------------------------------
from_list(List) when is_list(List) ->
    appendlist(empty(), List).

%%--------------------------------------------------------------------
%% @spec    () -> #keylist{}
%%
%% @doc     Return an empty keylist.
%% @end
%%--------------------------------------------------------------------
empty() ->
    #keylist{list = []}.

%%--------------------------------------------------------------------
%% @spec    (Data, Keylist) -> #keylist{}
%%
%%            Data         = {Name, NewValueList}      |
%%                           {Key, Name, NewValueList}
%%            Name         = string() "the name of a header in a sip message (e.g. \"From\")"
%%            Key          = atom() | string() "the key to use internally (e.g. 'from')"
%%            NewValueList = list() "new entries for keyelem identified by Key"
%%            Keylist      = #keylist{}
%%
%% @doc     Add NewValueList to tail of existing entry matching
%%          Key/Name in Keylist, or create a new entry in Keylist if no
%%          matching entry was found.
%% @end
%%--------------------------------------------------------------------
append({Name, NewValueList}, Keylist) when is_list(Name), is_list(NewValueList),
					   is_record(Keylist, keylist) ->
    mod(Name, fun (Valuelist) ->
		      lists:append(Valuelist, NewValueList)
	      end, Keylist);
%% Key also given, we can call modcase() directly
append({Key, Name, NewValueList}, Keylist) when (is_atom(Key) orelse is_list(Key)),
						is_list(Name), is_list(NewValueList),
						is_record(Keylist, keylist) ->
    ModifierFun = fun (Valuelist) ->
			  lists:append(Valuelist, NewValueList)
		  end,
    NewL = modcase(Key, Name, ModifierFun, Keylist#keylist.list),
    Keylist#keylist{list = NewL}.

%%--------------------------------------------------------------------
%% @spec    ({Name, NewValueList}, Keylist) -> #keylist{}
%%
%%            Name         = string() "the name of a header in a sip message"
%%            NewValueList = list() "new entries for keyelem identified by Key"
%%            Keylist      = #keylist{}
%%
%% @doc     Add NewValueList to head of an entry in Keylist (or create
%%          new entry if Key is unknown).
%%          Note : This function does not accept atom() form because
%%          if there is no header, one will be created and then we
%%          have to have a string() variant of it anyways.
%% @end
%%--------------------------------------------------------------------
prepend({Name, NewValueList}, Keylist) when is_list(Name), is_list(NewValueList),
					    is_record(Keylist, keylist) ->
    mod(Name, fun (Valuelist) ->
		      lists:append(NewValueList, Valuelist)
	      end, Keylist).

%%--------------------------------------------------------------------
%% @spec    (Name, Keylist) -> #keylist{}
%%
%%            Name    = string() | atom() "either the name of a header in a sip message (string) or the internal form of it (atom)"
%%            Keylist = #keylist{}
%%
%% @doc     remove an entry from keylist record() (#keylist.list)
%% @end
%%--------------------------------------------------------------------
delete(Key, Keylist) when is_atom(Key), is_record(Keylist, keylist) ->
    #keylist{list = del(Key, Keylist#keylist.list)};
delete(Name, Keylist) when is_list(Name), is_record(Keylist, keylist) ->
    Key = normalize(Name),
    #keylist{list = del(Key, Keylist#keylist.list)}.

%% Descrip.: delete first element (E) in List,
%%           where E#keyelem.key == normalize(Key)
%% Returns : list() of keyelem records()
del(_Key, []) ->
    [];
del(Key, [#keyelem{key = Key} | T]) ->
    %% Match, return remainder of list
    T;
del(Key, [H | T]) when is_record(H, keyelem) ->
    %% no match
    [H | del(Key, T)].

%%--------------------------------------------------------------------
%% @spec    (Name, Keylist) -> #keylist{}
%%
%%            Name    = string() | atom()
%%            Keylist = #keylist{}
%%
%% @doc     Remove the first entry from the element matching Name in
%%          Keylist.
%% @end
%%--------------------------------------------------------------------
deletefirstvalue(Name, Keylist) when (is_list(Name) orelse is_atom(Name)), is_record(Keylist, keylist) ->
    F = fun (Valuelist) ->
		case Valuelist of
		    [_Value | Rest] ->
			Rest;
		    [] ->
			[]
		end
	end,
    mod(Name, F, Keylist).

%%--------------------------------------------------------------------
%% @spec    (Name, NewValue, Keylist) -> #keylist{}
%%
%%            Name      = string() | atom()
%%            NewValue  = [string()]
%%            Keylist   = #keylist{}
%%
%% @doc     Replace value of all entrys in Keylist matching Name with
%%          NewValue.
%% @end
%%--------------------------------------------------------------------
set(Name, Valuelist, Keylist) when is_list(Name), is_record(Keylist, keylist) ->
    mod(Name, fun (_OldValue) ->
		      Valuelist
	      end, Keylist).

%%--------------------------------------------------------------------
%% @spec    (Keylist, Keys) -> #keylist{}
%%
%%            Keylist = [#keyelem{}]
%%            Keys    = [string()]
%%
%% @doc     Copy the elements matching Keys from Keylist.
%% @end
%%--------------------------------------------------------------------
copy(Keylist, Names) when record(Keylist, keylist) ->
    Keys = normalize_list(Names),
    KeyElems = Keylist#keylist.list,
    Copies = [E || E <- KeyElems, lists:member(E#keyelem.key, Keys)],
    #keylist{list = Copies}.

%%--------------------------------------------------------------------
%% @spec    (Func, Keylist) -> [term()]
%%
%%            Func    = fun() "fun(Key, Name, Value) -> term()"
%%            Keylist = #keylist{}
%%
%% @doc     Apply Func to all elements in Keylist. Func must be of
%%          arity three, and accept arguments Func(Key, Name, Value)
%%          where Key is an atom() or a string(), Name is always a
%%          string() and Value is [string()].
%% @end
%%--------------------------------------------------------------------
map(Func, Keylist) when is_record(Keylist, keylist) ->
    F = fun(Elem) ->
		Func(Elem#keyelem.key, Elem#keyelem.name, Elem#keyelem.value)
	end,
    lists:map(F, Keylist#keylist.list).

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Key) -> string() | atom()
%%
%%            Key = string() "the name of a header in a sip message"
%%
%% @doc     Convert header key to a standard format - either an atom
%%          for well known headers, or a lowercased string. We don't
%%          do list_to_atom() because that way it would be possible
%%          to exhaust our atom() space by sending us SIP requests
%%          with made-up headers.
%% @end
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
normalize("o") -> normalize2("event");			%% RFC3265
normalize("u") -> normalize2("allow-events");		%% RFC3265
normalize("r") -> normalize2("refer-to");		%% RFC3515 #7
normalize(Name) when is_list(Name) ->
    LC = string:to_lower(Name),
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
normalize2("content-disposition") ->	'content-disposition';
normalize2("content-length") ->		'content-length';
normalize2("content-type") ->		'content-type';
normalize2("cseq") ->			'cseq';
normalize2("date") ->			'date';
normalize2("event") ->			'event';
normalize2("expires") ->		'expires';
normalize2("from") ->			'from';
normalize2("max-forwards") ->		'max-forwards';
normalize2("mime-version") ->		'mime-version';
normalize2("min-expires") ->		'min-expires';
normalize2("organization") ->		'organization';
normalize2("path") ->			'path';
normalize2("priority") ->		'priority';
normalize2("proxy-authenticate") ->	'proxy-authenticate';
normalize2("proxy-authorization") ->	'proxy-authorization';
normalize2("proxy-require") ->		'proxy-require';
normalize2("reason") ->			'reason';
normalize2("record-route") ->		'record-route';
normalize2("reply-to") ->		'reply-to';
normalize2("require") ->		'require';
normalize2("retry-after") ->		'retry-after';
normalize2("route") ->			'route';
normalize2("rtp-rxstat") ->		'rtp-rxstat';
normalize2("rtp-txstat") ->		'rtp-txstat';
normalize2("server") ->			'server';
normalize2("supported") ->		'supported';
normalize2("timestamp") ->		'timestamp';
normalize2("to") ->			'to';
normalize2("user-agent") ->		'user-agent';
normalize2("via") ->			'via';
normalize2("warning") ->		'warning';
normalize2("www-authenticate") ->	'www-authenticate';
normalize2("x-yxa-peer-auth") ->	'x-yxa-peer-auth';
normalize2(In) when is_list(In) ->	In.

%%--------------------------------------------------------------------
%% @spec    (Names) -> [Normalized]
%%          Normalized = string() | atom()
%%
%% @doc     Convert header names to an internal representation. Known
%%          headers are represented with atoms, unknown ones are kept
%%          as strings.
%% @end
%%--------------------------------------------------------------------
normalize_list(Names) when is_list(Names) ->
    [normalize(Name) || Name <- Names].

%%--------------------------------------------------------------------
%% @spec    (Name, Func, Keylist) -> #keylist{}
%%
%%            Name    = string() | atom()
%%            Func    = fun()
%%            Keylist = #keylist{}
%%
%% @doc     Apply Func to all _values_ of the elements in Keylist,
%%          who matches Name. If no elements match Name, Func([])
%%          will be invoked, and the result will be added as a new
%%          element.
%% @end
%%--------------------------------------------------------------------
mod(Name, Func, #keylist{list = List}) when is_function(Func, 1), (is_atom(Name) orelse is_list(Name)) ->
    Key = normalize(Name),
    #keylist{list = modcase(Key, Name, Func, List)}.

modcase(Key, Name, Func, []) ->
    %% No more input - add a new element
    [#keyelem{key  = Key,
	      name = Name,
	      value = Func([])
	     }
    ];
modcase(Key, _Name, Func, [#keyelem{key = Key, value = Value} = Elem | List]) ->
    %% Match, process item using Func
    [Elem#keyelem{value = Func(Value)} | List];
modcase(Key, Name, Func, [Elem | List]) when is_record(Elem, keyelem) ->
    %% No match
    [Elem | modcase(Key, Name, Func, List)].

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

    %% normalize(Key)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "normalize/1 - 1"),
    via = normalize("Via"),

    autotest:mark(?LINE, "normalize/1 - 2"),
    via = normalize("vIA"),

    autotest:mark(?LINE, "normalize/1 - 3"),
    via = normalize(via),

    autotest:mark(?LINE, "normalize/1 - 4"),
    "other" = normalize("OTHER"),

    autotest:mark(?LINE, "normalize/1 - 5"),
    "other" = normalize("other"),

    autotest:mark(?LINE, "normalize/1 - 6"),
    "other " = normalize("otheR "),

    autotest:mark(?LINE, "normalize/1 - 7"),
    %% test short format
    from = normalize("f"),

    autotest:mark(?LINE, "normalize/1 - 8.1"),
    %% test short format - we test all of them to 'double check' the
    %% result when new ones are added
    Normalize_L1 = [normalize([Normalize_Char]) || Normalize_Char <- lists:seq($a, $z)],
    ["a", "b", "contact-type", "d", "content-encoding", 'from', "g", "h",
     'call-id', "j", 'supported', 'content-length', 'contact', "n", 'event',
     "p", "q", "refer-to", "subject", 'to', 'allow-events', 'via', "w", "x",
     "y", "z"] = Normalize_L1,

    %% from_list(List)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "from_list/1 - 1"),
    %% empty list
    #keylist{list=[]} = from_list([]),

    autotest:mark(?LINE, "from_list/1 - 2.1"),
    %% list() of {Key, Name, Value} tuples
    H1 = from_list([{via,    "Via",  ["one", "two", "three"]},
		    {"test", "Test", ["foo", "bar", "baz"]}
		   ]),

    autotest:mark(?LINE, "from_list/1 - 2.2"),
    %% check result
    [{"test", "Test", ["foo", "bar", "baz"]},
     {via, "Via", ["one", "two", "three"]}
    ] = test_to_list(H1),

    autotest:mark(?LINE, "from_list/1 - 3.1"),
    %% list() of {Name, Value} tuples
    H2 = from_list([{"ATOM", ["hi"]}]),

    autotest:mark(?LINE, "from_list/1 - 3.2"),
    %% check result
    [{"atom", "ATOM", ["hi"]}
    ] = test_to_list(H2),

    autotest:mark(?LINE, "from_list/1 - 4.1"),
    %% trickier list of tuples
    FromList_L4 = from_list([{"ATOM", ["hi"]},
			     {"AtOm", ["low"]},
			     {"Via",  ["first-via"]},
			     {'via', "via", ["second-via"]}
			    ]),

    autotest:mark(?LINE, "from_list/1 - 4.2"),
    %% check result
    [{"atom", "ATOM", ["hi", "low"]},
     {'via', "Via", ["first-via","second-via"]}
    ] = test_to_list(FromList_L4),
    

    %% fetch(Key, Keylist)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "fetch/2 - 1"),
    %% fetch using name, will match key after normalize()
    ["one", "two", "three"] = fetch("Via", H1),

    autotest:mark(?LINE, "fetch/2 - 2"),
    %% fetch using name, will match key after normalize()
    ["one", "two", "three"] = fetch("VIA", H1),

    autotest:mark(?LINE, "fetch/2 - 3"),
    %% fetch using key
    ["one", "two", "three"] = fetch('via', H1),

    autotest:mark(?LINE, "fetch/2 - 4"),
    %% when giving atom, it must be an exact match
    [] = fetch('Via', H1),

    autotest:mark(?LINE, "fetch/2 - 5"),
    %% fetch using name
    ["hi"] = fetch("atom", H2),

    autotest:mark(?LINE, "fetch/2 - 6"),
    %% fetch using name, will match after normalize()
    ["hi"] = fetch("ATOM", H2),

    %% append(Data, Keylist)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "append/2 - 1.1"),
    %% {Name, Value} tuple
    AppendH1 = append({"Header", ["one"]}, empty()),

    autotest:mark(?LINE, "append/2 - 1.2"),
    %% check result of append
    [{"header", "Header", ["one"]}
    ] = test_to_list(AppendH1),

    autotest:mark(?LINE, "append/2 - 2.1"),
    %% {Key, Name, Value}
    AppendH2 = append({"header", "Header", ["two"]}, AppendH1),

    autotest:mark(?LINE, "append/2 - 2.2"),
    %% check result of append
    [{"header", "Header", ["one", "two"]}
    ] = test_to_list(AppendH2),

    autotest:mark(?LINE, "append/2 - 3.1"),
    %% test with existing values in list, and also test normalization
    AppendH3_L1 = append({"Route", ["<sip:foo@test>"]}, AppendH1),
    AppendH3_L2 = append({"Route", ["<sip:foo@test2>"]}, AppendH3_L1),

    autotest:mark(?LINE, "append/2 - 3.2"),
    %% check result
    [{"header", "Header", ["one"]},
     {'route', "Route", ["<sip:foo@test>", "<sip:foo@test2>"]}
    ] = test_to_list(AppendH3_L2),


    %% set(Name, NewValue, Keylist)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "set/3 - 1"),
    SetH1 = set("FOO", ["test"], empty()),

    autotest:mark(?LINE, "set/3 - 2"),
    %% check results
    [{"foo", "FOO", ["test"]}
    ] = test_to_list(SetH1),

    autotest:mark(?LINE, "set/3 - 3"),
    %% should not change SetH1
    SetH1 = set("foo", ["test"], SetH1),

    autotest:mark(?LINE, "set/3 - 4"),
    SetH2 = set("bar", ["test2"], SetH1),

    autotest:mark(?LINE, "set/3 - 5"),
    %% check results
    [{"foo", "FOO", ["test"]},
     {"bar", "bar", ["test2"]}
    ] = test_to_list(SetH2),


    %% prepend({Name, NewValueList}, Keylist)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "prepend/3 - 1"),
    %% have one more keyelem in the test case to make sure prepend() only
    %% affects the element we want
    PrependH0 = keylist:set("Other", ["foo"], empty()),
    PrependH1 = prepend({"Header", ["one"]}, PrependH0),

    autotest:mark(?LINE, "prepend/3 - 2"),
    %% check result
    [{"header", "Header", ["one"]},
     {"other", "Other", ["foo"]}
    ] = test_to_list(PrependH1),

    autotest:mark(?LINE, "prepend/3 - 3"),
    PrependH2 = prepend({"header", ["zero"]}, PrependH1),

    autotest:mark(?LINE, "prepend/3 - 4"),
    %% check result
    [{"header", "Header", ["zero", "one"]},
     {"other", "Other", ["foo"]}
    ] = test_to_list(PrependH2),


    %% delete(Name, Keylist)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "delete/2 - 1"),
    %% add Via to PrependH2 so that we have one header with an atom key
    DeleteH1 = set("Via", ["bar"], PrependH2),

    autotest:mark(?LINE, "delete/2 - 2"),
    %% delete the "other" element, leaving the 'via' and "header" elements
    DeleteH2 = delete("other", DeleteH1),

    autotest:mark(?LINE, "delete/2 - 2"),
    %% check results
    [{"header", "Header", ["zero", "one"]},
     {'via', "Via", ["bar"]}
    ] = test_to_list(DeleteH2),

    autotest:mark(?LINE, "delete/2 - 2"),
    %% delete both the 'via' and "header" elements, leaving an empty list
    #keylist{list=[]} = delete("header", delete(via, DeleteH2)),

    %% deletefirstvalue(Name, Keylist)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "deletefirstvalue/2 - 1"),
    DFVH1 = from_list([{via, "Via", ["one", "two", "three"]},
		       {"other", "Other", ["foo"]}]),

    autotest:mark(?LINE, "deletefirstvalue/2 - 2.1"),
    DFVH2 = deletefirstvalue("Via", DFVH1),

    autotest:mark(?LINE, "deletefirstvalue/2 - 2.2"),
    %% check results
    [{"other", "Other", ["foo"]},
     {'via', "Via", ["two", "three"]}
    ] = test_to_list(DFVH2),

    autotest:mark(?LINE, "deletefirstvalue/2 - 3"),
    DFVH3 = deletefirstvalue("Other", DFVH2),

    %% check results, current deletefirstvalue doesn't remove the element
    %% but the result of a keylist:fetch() would be the same as if the
    %% element had been deleted (empty list)
    [{"other", "Other", []},
     {'via', "Via", ["two", "three"]}
    ] = test_to_list(DFVH3),


    %% copy(Keylist, Names)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "copy/2 - 1"),
    CopyH1 = from_list([{via, "Via", ["one", "two", "three"]},
			{"other", "Other", ["foo"]}]),

    autotest:mark(?LINE, "copy/2 - 2"),
    %% should not change CopyH1
    CopyH1 = copy(CopyH1, [via, "other"]),

    autotest:mark(?LINE, "copy/2 - 3"),
    %% should not change CopyH1
    CopyH1 = copy(CopyH1, [via, "oThEr"]),

    autotest:mark(?LINE, "copy/2 - 3"),
    %% copy only via
    [{'via', "Via", ["one", "two", "three"]}
    ] = test_to_list( copy(CopyH1, [via]) ),

    autotest:mark(?LINE, "copy/2 - 3"),
    %% copy headers not in source, result in empty keylist
    [] = test_to_list( copy(CopyH1, [warning, 'call-id', "foobar"]) ),

    %% appendlist(Keylist, List)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "appendlist/2 - 1"),
    %% {Name, Value} tuples
    AppendLH1 = appendlist(empty(), [{"ViA", ["one"]}, {"via", ["two"]}]),

    autotest:mark(?LINE, "appendlist/2 - 2"),
    %% check result
    [{'via', "ViA", ["one", "two"]}
    ] = test_to_list(AppendLH1),

    autotest:mark(?LINE, "appendlist/2 - 3"),
    %% {Name, Key, Value} tuples
    AppendLH2 = appendlist(AppendLH1, [{via, "Via", ["three"]},
				       {"other", "Other", ["X"]}
				      ]),

    autotest:mark(?LINE, "appendlist/2 - 4"),
    %% check result
    [{"other", "Other", ["X"]},
     {'via', "ViA", ["one", "two", "three"]}
    ] = test_to_list(AppendLH2),


    %% map(Func, Keylist)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "map/2 - 1"),
    CheckFun = fun(Key, Name, Value) ->
		       case {Key, Name, Value} of
			   {one, "one", ["one"]} -> ok;
			   {two, "TWO", ["TWO"]} -> ok
		       end
	       end,
    MapH1 = appendlist(empty(), [{one, "one", ["one"]},
				 {two, "TWO", ["TWO"]}
				]),

    autotest:mark(?LINE, "map/2 - 2"),
    [ok, ok] = map(CheckFun, MapH1),

    MapH2 = appendlist(MapH1, [{three, "three", ["thr33"]}]),

    autotest:mark(?LINE, "map/2 - 3"),
    %% make sure CheckFun does not pass MapH2
    {'EXIT', {{case_clause, _}, _}} = (catch map(CheckFun, MapH2)),

    ok.


%%--------------------------------------------------------------------
%% @spec    (Keylist) -> [{Key, Name, Value}]
%%
%% @doc     Turn Keylist into a sorted list of headers with their
%%          values and normalized representation. Used to verify
%%          results of unit test cases without making them overly
%%          dependant of the internal representation of keylists.
%% @hidden
%% @end
%%--------------------------------------------------------------------
test_to_list(Keylist) when is_record(Keylist, keylist) ->
    Names = [E#keyelem.name || E <- Keylist#keylist.list],
    SortedNames = lists:sort(Names),
    [{normalize(Name), Name, fetch(Name, Keylist)} || Name <- SortedNames].
