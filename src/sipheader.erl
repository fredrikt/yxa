%%%-------------------------------------------------------------------
%%% File    : sipheader.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Various functions for parsing headers or formatting
%%%           headers for printing.
%%%
%%% Note: some functions do several passes over the same string() to
%%% parse it, e.g. 1 pass to find start and end delimiter and one pass
%%% for reading the the elements from start to end index, and probably
%%% a additional scan to reach the remainder of the string.
%%% This performance can be improved, by using a accumulator equiped
%%% parser that returns the Match and Rest part of the string, in a
%%% single scan.
%%%
%%% Note: elements are often only partially parsed - this reduces the
%%% amount of parsing done on entries which will only be passed along,
%%% but while this improves performance it increases the need for later
%%% exception handling.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------

-module(sipheader).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 expires/1,
	 to/1,
	 from/1,
	 contact/1,
	 route/1,
	 record_route/1,
	 via/1,
	 via_print/1,
	 contact_print/1,
	 auth_print/1,
	 auth_print/2,
	 auth/1,
	 comma/1,
	 httparg/1,
	 cseq/1,
	 cseq_print/1,
	 callid/1,
	 via_params/1,
	 build_header_binary/1,
	 dict_to_param/1,
	 param_to_dict/1,
	 dialogid/1,
	 get_tag/1,
	 topvia/1,
	 via_sentby/1,
	 get_client_transaction_id/1,
	 get_server_transaction_id/1,
	 get_server_transaction_ack_id_2543/1,
	 get_via_branch/1,
	 get_via_branch_full/1,
	 remove_loop_cookie/1,
	 via_is_equal/2,
	 via_is_equal/3,
	 is_supported/2,
	 is_required/2,
	 event_package/1,

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

%% Used when building binary messages from headers - the headers will be
%% outputed in the order of the fields in this record
-record(header_sort, {via		= [],
		      from		= [],
		      to		= [],
		      'call-id'		= [],
		      cseq		= [],
		      route		= [],
		      'record-route'	= [],
		      rest		= []
		     }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SP, 16#20).
-define(HTAB, 16#09).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (String) -> [string()]
%%
%% @doc     split a comma separated string into separate strings,
%%          along the commas (don't include them). Each substring is
%%          cleared of any preceding or trailing spaces. example:
%%          "foo, bar, zop, \"quoted, hi\"" -> ["foo","bar","zop",
%%          "\"quoted, hi\""]
%%
%% @end
%%--------------------------------------------------------------------
comma(String) ->
    comma([], String, false, false).

%% comma(Parsed, Rest, Inquote, InUriQuote)

%% Parsed     = current segment of string since last comma (,)
%% Rest       = the remainder of the string to split along comma (,)
%% Inquote    = keep track if currently inside a quoted string segment
%%              where special chars like "," should be ignored
%% InUriQuote = keep track if currently inside a quoted sipurl segment
%%              where special chars like "," should be ignored

%% Note: Parsed stores it's chars in reverse order during
%% scanning - to improve performance, "Parsed ++ [C]" which is easier
%% to understand is considerably more costly, code becomes O(N^2)
%% instead of O(N)
%% Note: quote URIs (<...>) can't contain quoted strings.
%% Quoted strings on the other hand, can contain "<" and ">" - this
%% means that comma doesn't have to deal with nested quotes.


%% start of quoted uri
comma(Parsed, [$< | Rest], _InQuote = false, false) ->
    comma([$< | Parsed], Rest, false, true);

%% end of quoted uri
comma(Parsed, [$> | Rest], _InQuote = false, true) ->
    comma([$> | Parsed], Rest, false, false);

%% inside quoted uri - ignore comma or any other special chars
comma(Parsed, [Char | Rest], _InQuote = false, true) ->
    comma([Char | Parsed], Rest, false, true);

%% -------------

%% an escape code e.g. "\," has been found, don't treat it in any special manner
%% only done inside a quoted segment of the string
comma(Parsed, [$\\, Char | Rest], _InQuote = true, false) ->
    comma([Char, $\\ | Parsed], Rest, true, false);

%% Inquote = false, we have now entered inside a quoted ("...") string segment
comma(Parsed, [$\" | Rest], _InQuote = false, false) ->
    comma([$\" | Parsed], Rest, true, false);

%% Inquote = true and a new quote (") found - end of quoted string segment
comma(Parsed, [$\" | Rest], _InQuote = true, false) ->
    comma([$\" | Parsed], Rest, false, false);

%% regular char, store in current string
comma(Parsed, [Char | Rest], _InQuote = true, false) ->
    comma([Char | Parsed], Rest, true, false);

%% -------------

%% Inquote = false, so comma is a comma that splits string
%% start looking for the next one
comma(Parsed, [$, | Rest], _InQuote = false, false) ->
    [lists:reverse(string:strip(Parsed, both)) | comma([], Rest, false, false)];

%% regular char, store in current string
comma(Parsed, [Char | Rest], _InQuote = false, false) ->
    comma([Char | Parsed], Rest, false, false);

%% end of string, clean up last comma separated entry (it's an error
%% if Inquote or InUriQuote = true, the quotes are then unbalanced)
comma(Parsed, [], _InQuote = false, false) ->
    [lists:reverse(string:strip(Parsed, both))].

%%--------------------------------------------------------------------
%% @spec    (Header) ->
%%            Expires
%%
%%            Header = #keylist{}
%%
%%            Expires = [string()]
%%
%% @doc     Get the value of the "Expires" header.
%% @end
%%--------------------------------------------------------------------
expires(Header) when is_record(Header, keylist) ->
    keylist:fetch('expires', Header).

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            {DisplayName, URI}
%%
%%            In = [string()] | #keylist{}
%%
%%            DisplayName = none | string()
%%            URI         = #sipurl{}
%%
%% @doc     Parse To: header data.
%% @see      name_header/1.
%% @end
%%--------------------------------------------------------------------
to(Header) when is_record(Header, keylist) ->
    to(keylist:fetch('to', Header));

to([String]) ->
    name_header(String).

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            {DisplayName, URI}
%%
%%            In = [string()] | #keylist{}
%%
%%            DisplayName = none | string()
%%            URI         = #sipurl{}
%%
%% @doc     Parse From: header data.
%% @see      name_header/1.
%% @end
%%--------------------------------------------------------------------
from(Header) when is_record(Header, keylist) ->
    from(keylist:fetch('from', Header));

from([String]) ->
    name_header(String).

%%--------------------------------------------------------------------
%% @spec    (Header) -> [#contact{}]
%%
%%            Header = #keylist{}
%%
%% @doc     Return the parsed Contact: header from Header.
%% @end
%%--------------------------------------------------------------------
contact(Header) when is_record(Header, keylist) ->
    contact2(Header, 'contact').

%%--------------------------------------------------------------------
%% @spec    (Header) -> [#contact{}]
%%
%%            Header = #keylist{}
%%
%% @doc     Return the parsed Route: header from Header.
%% @end
%%--------------------------------------------------------------------
route(Header) when is_record(Header, keylist) ->
    contact2(Header, 'route').

%%--------------------------------------------------------------------
%% @spec    (Header) -> [#contact{}]
%%
%%            Header = #keylist{}
%%
%% @doc     Return the parsed Record-Route: header from Header.
%% @end
%%--------------------------------------------------------------------
record_route(Header) when is_record(Header, keylist) ->
    contact2(Header, 'record-route').

%% part of contact/1, route/1 and record_route/1
contact2(Header, Name) when is_record(Header, keylist), is_atom(Name) ->
    V = keylist:fetch(Name, Header),
    contact:parse(V).

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            [#via{}] 
%%
%%            In = #keylist{} | [string()]
%%
%%            Reason = unparseable_via | term()
%%
%% @throws  {error, Reason} 
%%
%% @doc     Parse Via: header data.
%% @end
%%--------------------------------------------------------------------
via(Header) when is_record(Header, keylist) ->
    via2(keylist:fetch('via', Header), []);

via(Vias) when is_list(Vias) ->
    via2(Vias, []).

via2([H | T], Res) when is_list(H) ->
    %% Parse SIP version and protocol ("SIP/2.0/FOO" part)
    {SIP1, Rest1} = sipparse_util:split_fields(H, $/),
    SIP = sipparse_util:strip(SIP1, both, [?SP, ?HTAB]),
    {Ver1, Rest2} = sipparse_util:split_fields(Rest1, $/),
    Ver = sipparse_util:strip(Ver1, both, [?SP, ?HTAB]),
    {ProtoHostport1, Parameters} = case catch sipparse_util:split_fields(Rest2, $;) of
				       {error, no_second_part} ->
					   [Rest2_1] = string:tokens(Rest2, ";"),
					   {Rest2_1, ""};
				       {Rest2_1, Rest2_2} ->
					   %% XXX strip the tokenized values?
					   {Rest2_1, string:tokens(Rest2_2, ";")};
				       {Rest2_1} ->
					   {Rest2_1, ""}
				   end,
    %% Strip horizontal whitespace from ProtoHostport
    ProtoHostport = sipparse_util:strip(ProtoHostport1, both, [?SP, ?HTAB]),

    {ok, Proto, Hostport} = via_proto_hostport(ProtoHostport),

    %% Handle sent-by (Hostport)
    case sipparse_util:parse_hostport(Hostport) of
	{Host, Port} when is_list(Host),
			  is_integer(Port); Port == none ->
	    Protocol = lists:concat([SIP, "/", Ver, "/", Proto]),
	    This = #via{proto=Protocol, host=Host, port=Port, param=Parameters},
	    via2(T, [This | Res]);
	_ ->
	    throw({error, unparseable_via})
    end;
via2([], Res) ->
    lists:reverse(Res).

%% via_proto_hostport/1 - part of via/1. Returns : {ok, Proto, Hostport} | throw()
via_proto_hostport(ProtoHostport) ->
    %% Split ProtoHostport - can result in either two, three or four elements
    case string:tokens(ProtoHostport, [?SP, ?HTAB]) of
	[Proto1, Hostport1] ->
	    {ok, Proto1, Hostport1};
	[Proto1, Host1, Port1] ->
	    %% Check if last character in Host1 is a colon
	    case lists:reverse(Host1) of
		":" ++ RHost2 ->
		    %% Vias can have whitespace between host and colon and
		    %% colon and port - crazy
		    Host2 = lists:reverse(
			      sipparse_util:strip(RHost2, left, [?SP, ?HTAB])
			     ),
		    {ok, Proto1, Host2 ++ ":" ++ Port1};
		_ ->
		    %% check if first character in port is a colon
		    case hd(Port1) == $: of
			true ->
			    {ok, Proto1, Host1 ++ Port1};
			false ->
			    throw({error, {invalid_proto_host_port_in_via, ProtoHostport}})
		    end
	    end;
	[Proto, Host, ":", Port] ->
	    %% Vias can have whitespace both between host and colon, and colon and port - crazy
	    {ok, Proto, Host ++ ":" ++ Port}
    end.

%%--------------------------------------------------------------------
%% @spec    (Header) ->
%%            #via{} |
%%            none
%%
%%            Header = #keylist{}
%%
%% @doc     get the first Via entry from Header
%% @end
%%--------------------------------------------------------------------
topvia(Header) when is_record(Header, keylist) ->
    case via(Header) of
	[] -> none;
	[TopVia | _] when is_record(TopVia, via) -> TopVia;
	_ -> error
    end.

%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Res
%%
%%            In = [string()]
%%
%%            Res = string()
%%
%% @doc     print string() separated by ";"
%% @end
%%--------------------------------------------------------------------
print_parameters([]) ->
    "";
print_parameters([A | B]) ->
    ";" ++ A ++ print_parameters(B).

%%--------------------------------------------------------------------
%% @spec    (Via) ->
%%            [ViaStr]
%%
%%            Via = #via{}
%%
%%            ViaStr = string()
%%
%% @doc     Print via record() or list() of via record().
%% @end
%%--------------------------------------------------------------------
via_print(Via) when is_record(Via, via) ->
    via_print2([Via], []);
via_print(Vias) when is_list(Vias) ->
    via_print2(Vias, []).

via_print2([H | T], Res) when is_record(H, via) ->
    {Protocol, Host, Port, Parameters} = {H#via.proto, H#via.host, H#via.port, H#via.param},
    This = Protocol ++ " " ++ sipurl:print_hostport(Host, Port) ++ print_parameters(Parameters),
    via_print2(T, [This | Res]);
via_print2([], Res) ->
    lists:reverse(Res).

%%--------------------------------------------------------------------
%% @spec    (Via) -> dict()
%%
%%            Via = #via{}
%%
%% @doc     convert parameters stored in Via to a dictionary
%% @end
%%--------------------------------------------------------------------
via_params(Via) when is_record(Via, via) ->
    param_to_dict(Via#via.param).

%%--------------------------------------------------------------------
%% @spec    (Contacts) -> [string()]
%%
%%            Contacts = [#contact{}] "containing contact from contact/1"
%%
%% @doc     Take a list of contact records, and return a list of those
%%          contacts as strings
%% @end
%%--------------------------------------------------------------------
contact_print(Contacts) when is_list(Contacts) ->
    contact_print2(Contacts, []).

contact_print2([H | T], Res) when is_record(H, contact) ->
    contact_print2(T, [contact:print(H) | Res]);
contact_print2([], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% @spec    (Auth) -> [string()]
%%
%% @equiv    auth_print(Auth, false)
%% @end
%%--------------------------------------------------------------------
auth_print(Auth) when is_tuple(Auth) ->
    auth_print(Auth, false).

%%--------------------------------------------------------------------
%% @spec    (Auth, Stale) -> [string()]
%%
%%            Auth   = {Realm, Nonce, Opaque}
%%            Realm  = string()
%%            Nonce  = string()
%%            Opaque = string()
%%            Stale  = true | false
%%
%% @doc     Generate the value of an WWW-Authenticate that we need
%%          when challenging a REGISTER, or a Proxy-Authenticate that
%%          we put in other challenges of a request.
%% @end
%%--------------------------------------------------------------------
auth_print(Auth, Stale) when is_tuple(Auth), is_boolean(Stale) ->
    {Realm, Nonce, Opaque} = Auth,
    ["Digest realm=\"" ++ Realm ++ "\", nonce=\"" ++ Nonce ++ "\", opaque=\"" ++ Opaque ++ "\"" ++
     case Stale of
	 true ->
	     ", stale=true";
	 false ->
	     ""
     end
    ].

%%--------------------------------------------------------------------
%% @spec    (In) -> dict()
%%
%%            In = string() "one authentication header value"
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Parse an authorization header. Note : In is a string like
%%          "Digest username=\"test\",realm=\"example.org\" ...", a
%%          SIP message can have multiple of those header values in
%%          it. This function only handles one at a time. XXX We
%%          should preserve the type in the repsonse as well. Define
%%          a new record for parsed authentication data and use that.
%% @end
%%--------------------------------------------------------------------
auth(In) when is_list(In) ->
    %% lowercase first word (to implement case insensitivity)
    case string:chr(In, $\ ) of	%% Find first space ($\ ) is a space
	0 ->
	    %% no space found, really badly formatted data
	    logger:log(error, "sipheader:auth/1 called with unparsable input (~p)", [In]),
	    throw({siperror, 500, "Server Internal Error"});
	Index ->
	    FirstWord = string:substr(In, 1, Index - 1),
	    Rest = string:substr(In, Index + 1),
	    LCfw = httpd_util:to_lower(FirstWord),
	    auth2(LCfw, Rest)
    end.


%% auth2: part of auth/1, Returns : dict() | throw({siperror, ...})
%%
%% Type is "digest" or "gssapi"
%%
auth2(Type, String) when Type == "digest"; Type == "gssapi", is_list(String) ->
    Headers = comma(String),
    L = lists:map(fun get_name_and_value/1, Headers),
    dict:from_list(L);
%%
%% Type is something we don't recognize
%%
auth2(Type, String) ->
    logger:log(error, "sipheader:auth2() called with unrecognized authentication data (~p, ~p)", [Type, String]),
    throw({siperror, 500, "Server Internal Error"}).

%% part of auth2, Returns : {Name, UnquotedValue}
get_name_and_value(Str) ->
    H = string:strip(Str,left),
    Index = string:chr(H, $=),
    Name = string:substr(H, 1, Index - 1),
    Value = string:substr(H, Index + 1),
    {Name, unquote(Value)}.

%% removes single pair of quotes, returns contents in between these first two quotes
unquote([34 | QString]) ->	%% 34 is $"
    Index = string:chr(QString, 34),	%% 34 is $"
    string:substr(QString, 1, Index - 1);
unquote(QString) ->
    QString.


%%--------------------------------------------------------------------
%% @spec    (Param) -> dict()
%%
%%            Param = [string()] "each string is a \"key=value\" pair, that may have preceding or trailing spaces as well as hex encoded values (e.g. chars of the format %hh (where hh is a hex number))"
%%
%% @doc     Convert SIP parameter strings into a dictionary.
%% @end
%%--------------------------------------------------------------------
param_to_dict(Param) ->
    L = lists:map(fun(A) ->
			  H = string:strip(A,left),
			  Index = string:chr(H, $=),
			  case Index of
			      0 ->
			          {httpd_util:to_lower(H), ""};
			      _ ->
				  Name = httpd_util:to_lower(string:substr(H, 1, Index - 1)),
				  Value = string:substr(H, Index + 1),
				  {Name, unescape(Value)}
			  end
		  end, Param),
    dict:from_list(L).

%% convert (2) hex codes to single 8 bit char
unescape([]) ->
    [];
unescape([37, C1, C2 | Rest]) ->	%% 37 is $%
    [hex:from([C1, C2]) | unescape(Rest)];
unescape([C | Rest]) ->
    [C | unescape(Rest)].

%%--------------------------------------------------------------------
%% @spec    (Dict) -> [string()]
%%
%%            Dict = dict() "a dictionary containing parameter entries"
%%
%% @doc     convert a dictionary containing parameters back into a
%%          "name=value" format - the inverse of param_to_dict/1
%% @end
%%--------------------------------------------------------------------
%% XXX should certain chars in the "value" part be hex encoded ?
dict_to_param(Dict) ->
    list_to_parameters2(lists:keysort(1, dict:to_list(Dict)), []).

%% list_to_parameters2/2 - part of dict_to_param/1
list_to_parameters2([{Key, []} | T], Res) ->
    %% Empty value (like "lr" or "rport")
    list_to_parameters2(T, [Key | Res]);
list_to_parameters2([{Key, Value} | T], Res) ->
    %% Non-empty value
    list_to_parameters2(T, [Key ++ "=" ++ Value | Res]);
list_to_parameters2([], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% @spec    (String) -> dict()
%%
%% @doc     Make dict out of parameters separated by ampersand (`&').
%%          Note : Only used in admin_www. Perhaps move there?
%% @end
%%--------------------------------------------------------------------
httparg(String) ->
    Headers = string:tokens(String, "&"),
    param_to_dict(Headers).


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            {Seq, Method} | {unparseable, String}
%%
%%            In = #keylist{} | [string()]
%%
%%            Seq    = integer()
%%            Method = string()
%%
%% @doc     Parse CSeq: header data.
%% @end
%%--------------------------------------------------------------------
cseq(Header) when is_record(Header, keylist) ->
    cseq(keylist:fetch('cseq', Header));

cseq([String]) ->
    case string:tokens(String, " ") of
	[Seq, Method] when is_list(Seq), is_list(Method) ->
	    %% XXX return Seq as integer
	    {Seq, Method};
	_ ->
	    {unparseable, String}
    end.

%%--------------------------------------------------------------------
%% @spec    ({Seq, Method}) -> string()
%%
%%            Seq    = integer()
%%            Method = string()
%%
%% @doc     Print data parsed with cseq/1.
%% @end
%%--------------------------------------------------------------------
cseq_print({Seq, Method}) when is_list(Seq), is_list(Method) ->
    %% XXX Seq should be integer
    Seq ++ " " ++ Method.

%%--------------------------------------------------------------------
%% @spec    (Header) -> string()
%%
%%            Header = #keylist{}
%%
%% @doc     Get Call-Id: from header.
%% @end
%%--------------------------------------------------------------------
%% XXX does not handle non-existing Call-Id, but that means our
%% callers might not either, so the right solution might not be to
%% make us return [] or 'none' if there is no Call-Id.
callid(Header) when is_record(Header, keylist) ->
    [CallId] = keylist:fetch('call-id', Header),
    CallId.

%%--------------------------------------------------------------------
%% @spec    (Header) -> binary()
%%
%%            Header = #keylist{}
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Build a SIP header we can combine with a first line and
%%          body to create a message to send out on the wire. We try
%%          to prioritize speed here, so we don't spend extra cycles
%%          making the resulting data uniformed. We might return a
%%          list of lists of binaries, or just binaries.
%% @end
%%--------------------------------------------------------------------
build_header_binary(Header) when is_record(Header, keylist) ->
    case catch build_header_unsafe_binary(Header) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== failed to build header ~p,~nfrom build_header_unsafe_binary :~n~p",
		       [Header, E]),
	    throw({siperror, 500, "Server Internal Error"});
	Res ->
	    Res
    end.

build_header_unsafe_binary(Header) ->
    %% process each line in Header
    Lines = keylist:map(fun print_one_header_binary/3, Header),
    list_to_binary(sort_headers(Lines)).

%%--------------------------------------------------------------------
%% @spec    (In) -> binary()
%%
%%            In     = [{Key, Binary}]
%%            Key    = atom() | string()
%%            Binary = binary()
%%
%% @doc     Rough sort headers for better readability (and to follow
%%          RFC3261 #7.3.1 (Header Field Format) recommendations
%%          about some sorting to facilitate more rapid parsing by
%%          other nodes
%% @end
%%--------------------------------------------------------------------
sort_headers(In) when is_list(In) ->
    sort_headers2(In, #header_sort{}).

sort_headers2([{via, Val} | T], R) ->			sort_headers2(T, R#header_sort{via = Val});
sort_headers2([{from, Val} | T], R) ->			sort_headers2(T, R#header_sort{from = Val});
sort_headers2([{to, Val} | T], R) ->			sort_headers2(T, R#header_sort{to = Val});
sort_headers2([{'call-id', Val} | T], R) ->		sort_headers2(T, R#header_sort{'call-id' = Val});
sort_headers2([{cseq, Val} | T], R) ->			sort_headers2(T, R#header_sort{cseq = Val});
sort_headers2([{route, Val} | T], R) ->			sort_headers2(T, R#header_sort{route = Val});
sort_headers2([{'record-route', Val} | T], R) ->	sort_headers2(T, R#header_sort{'record-route' = Val});
sort_headers2([{_Other, Val} | T], R) ->
    NewRest = [Val | R#header_sort.rest],
    sort_headers2(T, R#header_sort{rest = NewRest});
sort_headers2([], R) ->
    [_ | Headers] = tuple_to_list(R),
    Headers.

%% Returns : list() of {Key, Binary}
%%           Key = atom() or string(), keylist normalized header name
%%           Binary = binary(), this header as binary (<<"Supported: something\r\n">> for example
print_one_header_binary(Key, Name, []) ->
    %% Header without value.
    {Key, list_to_binary([Name, $:, 32, 13, 10])};
print_one_header_binary(Key, Name, ValueList) ->
    %% certain headers that have multiple values are written on a single line separated by "," -
    %% this is not because any RFC says so but because these are common headers that look
    %% considerably much better when appearing on one line
    OneLine =
	if
	    Key == 'accept' ->		true;
	    Key == 'allow' ->		true;
	    Key == 'allow-events' ->	true;
	    Key == 'supported' ->	true;
	    Key == 'require' ->		true;
	    Key == 'proxy-require' ->	true;
	    Key == 'rtp-rxstat' ->	true;	%% Cisco 79xx
	    Key == 'rtp-txstat' ->	true;	%% Cisco 79xx
	    true -> false
	end,
    BinName = list_to_binary(Name),
    This = print_one_header_binary2(OneLine, BinName, ValueList, []),
    {Key, This}.

%% print_one_header_binary2 - convert all the values to binarys
print_one_header_binary2(OneLine, BinName, [H | T], Res) ->
    print_one_header_binary2(OneLine, BinName, T, [list_to_binary(H) | Res]);
print_one_header_binary2(OneLine, BinName, [], Res) ->
    print_one_header_binary3(OneLine, BinName, lists:reverse(Res)).

%% print_one_header_binary3 - create the binary representation of this header
%% and all it's values
print_one_header_binary3(true, BinName, BinValueList) ->
    %% return as one line
    OneLine = fun(Value, Acc) ->
		      case Acc of
			  [] ->
			      %% No prior elements - return only Value
			      [Value];
			  _ ->
			      %% Accumulated comma space Value
			      list_to_binary([Acc, $\,, 32, Value])
		      end
	      end,
    BinValues = lists:foldl(OneLine, [], BinValueList),
    list_to_binary([BinName, $:, 32, BinValues, 13, 10]);

print_one_header_binary3(false, BinName, [First | Rest]) ->
    %% Return one "Key: Value" for every element in BinValueList
    This = list_to_binary([BinName, $:, 32, First, 13, 10]),	%% Name: First\r\n
    [This | print_one_header_binary3(false, BinName, Rest)];
print_one_header_binary3(false, _BinName, []) ->
    [].

%%--------------------------------------------------------------------
%% @spec    ([String]) ->
%%            Tag
%%
%%            Tag = string() | none
%%
%% @doc     Get From- or To-tag from from- or to-header value. Note :
%%          This function really ought to parse String using
%%          contact:new() in order to not be fooled by $> appearing
%%          more than once, tag= not written in lowercase etc.
%% @end
%%--------------------------------------------------------------------
get_tag([String]) ->
    [Contact] = contact:parse([String]),
    case contact_param:find(Contact#contact.contact_param, "tag") of
	[] ->
	    none;
	[Tag] ->
	    Tag
    end.

%%--------------------------------------------------------------------
%% @spec    (Header) ->
%%            {CallID, FromTag, ToTag}
%%
%%            Header = #keylist{}
%%
%%            CallId  = string()
%%            FromTag = string()
%%            ToTag   = string()
%%
%% @doc     Get what in RFC3261 is referred to as a dialog ID. This
%%          will be the same for all requests in a dialog. Note
%%          though that the ToTag might be 'none' and later get set.
%% @end
%%--------------------------------------------------------------------
dialogid(Header) when is_record(Header, keylist) ->
    CallID = sipheader:callid(Header),
    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
    ToTag = sipheader:get_tag(keylist:fetch('to', Header)),
    {CallID, FromTag, ToTag}.

%%--------------------------------------------------------------------
%% @spec    (Via) ->
%%            {Proto, Host, Port}
%%
%%            Via = #via{}
%%
%%            Proto = string()
%%            Host  = string()
%%            Port  = integer()
%%
%% @doc     Extract sent-by part of a via record()
%% @end
%%--------------------------------------------------------------------
via_sentby(Via) when is_record(Via, via) ->
    {Via#via.proto, Via#via.host, Via#via.port}.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            Id
%%
%%            Request = #request{}
%%
%%            Id = term() | is_2543_ack | error
%%
%% @doc     Turn a request into a transaction id, that can be stored
%%          in our transaction state database together with a
%%          reference to the process handling this request (server
%%          transaction handler) if this is a new transaction, or
%%          looked up in the database to find an existing handler if
%%          this is a resend of the same request or an ACK to a
%%          non-2xx response to INVITE. This is specified in RFC3261
%%          #17.2.3 (Matching Requests to Server Transactions).
%% @end
%%--------------------------------------------------------------------
get_server_transaction_id(Request) ->
    %% We do a catch around this since it includes much parsing of the
    %% request, and parsing data received from the network is a fragile thing.
    case catch guarded_get_server_transaction_id(Request) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from get_server_transaction_id(~p) :~n~p", [Request, E]),
	    error;
	Id ->
	    Id
    end.

guarded_get_server_transaction_id(Request) when is_record(Request, request) ->
    TopVia = sipheader:topvia(Request#request.header),
    %% XXX the branch is actually a token and should apparently be compared case-insensitively
    %% http://bugs.sipit.net/show_bug.cgi?id=661
    Branch = get_via_branch(TopVia),
    case Branch of
	"z9hG4bK" ++ _RestOfBranch ->
	    M = case Request#request.method of
		    "ACK" ->
			%% RFC3261 #17.2.3, bullet #3 - when looking for server
			%% transaction for ACK, the method of the transaction is INVITE
			"INVITE";
		    Other ->
			Other
		end,
	    guarded_get_server_transaction_id_3261(M, TopVia);
	_ ->
	    guarded_get_server_transaction_id_2543(Request, TopVia)
    end.

%%--------------------------------------------------------------------
%% @spec    (Response) ->
%%            Id
%%
%%            Response = #response{}
%%
%%            Id = term() | error
%%
%% @doc     When we receive a response, we use this function to get an
%%          Id which we look up in our transaction state database to
%%          see if we have a client transaction handler that should
%%          get this response. This is specified in RFC3261 #17.1.3
%%          (Matching Responses to Client Transactions).
%% @end
%%--------------------------------------------------------------------
get_client_transaction_id(Response) ->
    %% We do a catch around this since it includes much parsing of the
    %% request, and parsing data received from the network is a fragile thing.
    case catch guarded_get_client_transaction_id(Response) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from get_client_transaction_id(~p) :~n~p", [Response, E]),
	    error;
	Id ->
	    Id
    end.

guarded_get_client_transaction_id(Response) when is_record(Response, response) ->
    Header = Response#response.header,
    TopVia = sipheader:topvia(Header),
    Branch = get_via_branch(TopVia),
    {_, CSeqMethod} = sipheader:cseq(Header),
    {Branch, CSeqMethod}.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            Id
%%
%%            Request = #request{}
%%
%%            Id = term() | error
%%
%% @doc     When we receive an ACK that has no RFC3261 Via branch
%%          parameter, we use this function to get an Id that we then
%%          look up in our transaction state database to try and find
%%          an existing server transaction that this ACK should be
%%          delivered to. This is specified in RFC3261 #17.2.3
%%          (Matching Requests to Server Transactions). Note : When
%%          using this function, you have to make sure the To-tag of
%%          this ACK matches the To-tag of the response you think
%%          this might be the ACK for!
%%          Note : RFC3261 #17.2.3 relevant text : The ACK request
%%          matches a transaction if the Request- URI, From tag,
%%          Call-ID, CSeq number (not the method), and top Via header
%%          field match those of the INVITE request which created the
%%          transaction, and the To tag of the ACK matches the To tag
%%          of the response sent by the server transaction.
%%          Note : We are supposed to do the comparison of for
%%          example, the URI, according to the matching rules for
%%          URIs but that would require us to do a full table scan
%%          for every ACK. XXX perhaps we should divide the Id into
%%          two parts - one that is byte-by-byte and used as table
%%          index, and another part for elements that require more
%%          exhaustive matching.
%% @end
%%--------------------------------------------------------------------
get_server_transaction_ack_id_2543(Request) ->
    case catch guarded_get_server_transaction_ack_id_2543(Request) of
	{'EXIT', E} ->
	    logger:log(error, "=ERROR REPORT==== from get_server_transaction_ack_id_2543(~p) :~n~p", [Request, E]),
	    error;
	Id ->
	    Id
    end.

guarded_get_server_transaction_ack_id_2543(Request) when is_record(Request, request) ->
    {URI, Header} = {Request#request.uri, Request#request.header},
    TopVia = remove_branch(sipheader:topvia(Header)),
    CallID = sipheader:callid(Header),
    {CSeqNum, _} = sipheader:cseq(Header),
    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
    %% We are supposed to match only on the CSeq number, but the entry we are
    %% matching against is an INVITE and that INVITE had it's Id generated with
    %% the full CSeq. Make it possible to match the INVITE with this Id.
    FakeCSeq = {CSeqNum, "INVITE"},
    {URI, FromTag, CallID, FakeCSeq, TopVia}.

remove_branch(Via) when is_record(Via, via) ->
    ParamDict = sipheader:param_to_dict(Via#via.param),
    NewDict = dict:erase("branch", ParamDict),
    Via#via{param=sipheader:dict_to_param(NewDict)}.

%%--------------------------------------------------------------------
%% @spec    (TopVia) ->
%%            Branch
%%
%%            Branch = string() | none
%%
%% @doc     Get the branch from the TopVia parameters, and then remove
%%          any YXA loop cookie from it. This function should
%%          typically only be called on a Via that matches this proxy
%%          so that should be ok - we won't be altering anyone elses
%%          branches.
%% @end
%%--------------------------------------------------------------------
get_via_branch(TopVia) when is_record(TopVia, via) ->
    %% XXX lowercase result since branches are to be compared case insensitively?
    %% http://bugs.sipit.net/show_bug.cgi?id=661
    Branch = get_via_branch_full(TopVia),
    remove_loop_cookie(Branch).

%%--------------------------------------------------------------------
%% @spec    (Branch) ->
%%            Branch | NewBranch
%%
%%            Branch = string() | none
%%
%%            NewBranch = string()
%%
%% @doc     Removes our special YXA loop cookie from a branch, if it
%%          really is an YXA generated branch.
%% @end
%%--------------------------------------------------------------------
remove_loop_cookie(Branch) ->
    case Branch of
	"z9hG4bK-yxa-" ++ RestOfBranch ->
	    case yxa_config:get_env(detect_loops) of
		{ok, true} ->
		    case string:rstr(RestOfBranch, "-o") of
			0 ->
			    Branch;
			Index when is_integer(Index) ->
			    %% Return branch without YXA loop cookie
			    "z9hG4bK-yxa-" ++ string:substr(RestOfBranch, 1, Index - 1)
		    end;
		{ok, false} ->
		    Branch
	    end;
        _ when is_list(Branch) ->
	    Branch;
	none ->
	    none
    end.

%%--------------------------------------------------------------------
%% @spec    (Via) ->
%%            Branch
%%
%%            Via = #via{}
%%
%%            Branch = string() | none
%%
%% @doc     Get the whole Via branch (inclusive any loop cookie) from
%%          Via.
%% @end
%%--------------------------------------------------------------------
get_via_branch_full(Via) when is_record(Via, via) ->
    case dict:find("branch", sipheader:param_to_dict(Via#via.param)) of
	error ->
	    none;
	{ok, Branch} ->
	    Branch
    end.

%%--------------------------------------------------------------------
%% @spec    (A, B) ->
%%            true  |
%%            false
%%
%%            A = #via{}
%%            B = #via{}
%%
%% @doc     Compare two Via records according to the rules in RFC3261
%%          20.42 (Via)
%% @end
%%--------------------------------------------------------------------
via_is_equal(A, B) when is_record(A, via), is_record(B, via) ->
    via_is_equal(A, B, [proto, host, port, parameters]).


%%--------------------------------------------------------------------
%% @spec    (A, B, CmpList) ->
%%            true  |
%%            false
%%
%%            A       = #via{}
%%            B       = #via{}
%%            CmpList = [proto                        |
%%                      host                          |
%%                      port                          |
%%                      parameters] "what to compare"
%%
%% @doc     Compare one or more parts of two Via records according to
%%          RFC3261 20.42.
%% @end
%%--------------------------------------------------------------------

%%
%% Protocol (e.g. "SIP/2.0/TCP"), string compare case sensitive
%%
via_is_equal(A, B, [proto | T]) when is_record(A, via), is_record(B, via), A#via.proto == B#via.proto ->
    via_is_equal(A, B, T);
via_is_equal(A, B, [proto | _T]) when is_record(A, via), is_record(B, via) ->
    false;

%%
%% Host, string compare case insensitive
%%
via_is_equal(A, B, [host | T]) when is_record(A, via), is_record(B, via) ->
    case util:casecompare(A#via.host, B#via.host) of
	true ->
	    via_is_equal(A, B, T);
	_ ->
	    false
    end;

%%
%% Port, specified port does not equal un-specified port
%%
via_is_equal(A, B, [port | T]) when is_record(A, via), is_record(B, via), A#via.port == B#via.port ->
    via_is_equal(A, B, T);
via_is_equal(A, B, [port | _T]) when is_record(A, via), is_record(B, via) ->
    false;

%%
%% Parameters. All parameters must be present and their values must be equal
%% for the vias to be considerered equal.
%%
via_is_equal(A, B, [parameters | T]) when is_record(A, via), is_record(B, via) ->
    %% XXX we should probably do this case insensitive or whatever, but for now
    %% we just compare that the two Via's sorted parameters are identical.
    %% XXX yes, at least the branch parameter is case insensitive
    %% see RFC3261 section 7.3.1. 'When comparing header fields...'
    Alist = lists:sort(A#via.param),
    Blist = lists:sort(B#via.param),
    case Alist of
	Blist ->
	    via_is_equal(A, B, T);
	_ ->
	    false
    end;

%%
%% Nothing to compare left, consider them equal
%%
via_is_equal(A, B, []) when is_record(A, via), is_record(B, via) ->
    true.

%%--------------------------------------------------------------------
%% @spec    (Extension, Header) -> true | false
%%
%%            Extension = string()
%%            Header    = #keylist{}
%%
%% @doc     Check if Extension appears in a Supported: header.
%% @end
%%--------------------------------------------------------------------
is_supported(Extension, Header) when is_list(Extension), is_record(Header, keylist) ->
    Supported = keylist:fetch('supported', Header),
    lists:member(Extension, Supported).

%%--------------------------------------------------------------------
%% @spec    (Extension, Header) -> true | false
%%
%%            Extension = string()
%%            Header    = #keylist{}
%%
%% @doc     Check if Extension appears in a Required: header.
%% @end
%%--------------------------------------------------------------------
is_required(Extension, Header) when is_list(Extension), is_record(Header, keylist) ->
    Supported = keylist:fetch('require', Header),
    lists:member(Extension, Supported).

%%--------------------------------------------------------------------
%% @spec    (Header) ->
%%            EventPackage
%%
%%            Header = #keylist{}
%%
%%            EventPackage = string()
%%
%% @doc     Get the Event package name from Header.
%% @end
%%--------------------------------------------------------------------
event_package(Header) when is_record(Header, keylist) ->
    case keylist:fetch('event', Header) of
	[] ->
	    [];
	[Event] ->
	    EP = string:sub_word(Event, 1, $\;),
	    http_util:to_lower(EP)
    end.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (String) ->
%%            {Displayname, URI} | {unparseable, String}
%%
%%            String = string() "a sip URI string or sip URI inside \"<\" and \">\" quotes, preceded by a displayname"
%%
%%            Displayname = none | string()
%%            URI         = #sipurl{}
%%
%% @doc     used to parse the contents in a To, From or Contact header
%% @end
%%--------------------------------------------------------------------
name_header(String) ->
    Index1 = string:rchr(String, $<),
    case Index1 of
	0 ->
	    %% No "<", just an URI? XXX Check that it is parseable?
	    URI = sipurl:parse(String),
	    {none, URI};
	_ ->
	    Index2 = string:rchr(String, $>),
	    URL = string:substr(String, Index1 + 1, Index2 - Index1 - 1),
	    case sipurl:parse(URL) of
		URI when is_record(URI, sipurl) ->
		    Displayname = parse_displayname(string:substr(String, 1, Index1 - 1)),
		    {Displayname, URI};
		E ->
		    E
	    end
    end.

%% part of name_header/1.
%% Returns : none | string()
parse_displayname(String) ->
    StrippedStr = sipparse_util:strip(String, both, [?SP, ?HTAB]),
    %% look for quoted display-name
    try sipparse_util:split_quoted_string(StrippedStr) of
	{ok, DisplayNameStr, [] = _Rest} ->
	    empty_displayname(DisplayNameStr)
    catch
	error: does_not_start_with_quote ->
	    empty_displayname(StrippedStr)
    end.

empty_displayname([]) ->
    none;
empty_displayname(Name) ->
    Name.

%%--------------------------------------------------------------------
%% @spec    (Method, TopVia) ->
%%            Id
%%
%%            Method = list()
%%            TopVia = #via{}
%%
%%            Id = term()
%%
%% @doc     Part of guarded_get_server_transaction_id(), called when
%%          the top Via header is found to contain an RFC3261 branch
%%          parameter. This is the straight forward case.
%% @end
%%--------------------------------------------------------------------
guarded_get_server_transaction_id_3261(Method, TopVia) when is_list(Method), is_record(TopVia, via) ->
    Branch = get_via_branch_full(TopVia),
    SentBy = via_sentby(TopVia),
    {Branch, SentBy, Method}.

%%--------------------------------------------------------------------
%% @spec    (Request, TopVia) ->
%%            Id
%%
%%            Request = #request{}
%%            TopVia  = #via{}
%%
%%            Id = term() | is_2543_ack
%%
%% @doc     Part of guarded_get_server_transaction_id(), called when
%%          the top Via header does NOT contain an RFC3261 branch
%%          parameter. Creates an Id based on RFC3261 #17.2.3
%%          (Matching Requests to Server Transactions). Note : We
%%          could very well do the 2543 ack-id computation here, but
%%          since the caller must do the To-tag verification for such
%%          requests we just return is_2543_ack here to make sure the
%%          caller does not miss this. Note : RFC3261 #17.2.3 has
%%          different text for ACK (entirely separate, see previous
%%          note), INVITE and "all other methods". However, it seems
%%          to me that the instructions for INVITE and "all other"
%%          are the same :
%%          The INVITE request matches a transaction if the
%%          Request-URI, To tag, From tag, Call-ID, CSeq, and top Via
%%          header field match those of the INVITE request which
%%          created the transaction. ... For all other request
%%          methods, a request is matched to a transaction if the
%%          Request-URI, To tag, From tag, Call-ID, CSeq (including
%%          the method), and top Via header field match those of the
%%          request that created the transaction.
%%          Therefor, we just have non-ACK below.
%% @end
%%--------------------------------------------------------------------
%%
%% ACK
%%
guarded_get_server_transaction_id_2543(Request, _) when is_record(Request, request), Request#request.method == "ACK" ->
    is_2543_ack;

%%
%% non-ACK
%%
guarded_get_server_transaction_id_2543(Request, TopVia) when is_record(Request, request), is_record(TopVia, via) ->
    {URI, Header} = {Request#request.uri, Request#request.header},
    CallID = sipheader:callid(Header),
    CSeq = sipheader:cseq(Header),
    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
    ToTag = sipheader:get_tag(keylist:fetch('to', Header)),
    {URI, ToTag, FromTag, CallID, CSeq, TopVia}.


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

    %% test comma(String)
    %%--------------------------------------------------------------------
    %% test empty string
    autotest:mark(?LINE, "comma/1 - 1"),
    [""] = comma(""),

    %% test string without comma
    autotest:mark(?LINE, "comma/1 - 2"),
    ["foobar"] = comma("foobar"),

    %% test with single comma
    autotest:mark(?LINE, "comma/1 - 3"),
    ["foo","bar"] = comma("foo,bar"),

    %% test with several comma
    autotest:mark(?LINE, "comma/1 - 4"),
    ["fo","ob","ar"] = comma("fo,ob,ar"),

    %% test with quotes inside string
    autotest:mark(?LINE, "comma/1 - 5"),
    ["foobar: \"this is a string\""] = comma("foobar: \"this is a string\""),

    %% test with commas inside quoted string part
    autotest:mark(?LINE, "comma/1 - 6"),
    ["foobar: \"this, is, a, string,\""] = comma("foobar: \"this, is, a, string,\""),

    %% test with commas outside quoutes
    autotest:mark(?LINE, "comma/1 - 7"),
    ["foobar:", "\",this is a string\""] = comma("foobar:, \",this is a string\""),

    %% test with commas outside and inside quoutes
    autotest:mark(?LINE, "comma/1 - 8"),
    ["foobar:", "\",this is a ,string\"", "foo"] = comma("foobar:, \",this is a ,string\",foo"),

    %% test escaped chars inside quotes
    autotest:mark(?LINE, "comma/1 - 9"),
    ["hi","\" world \\\" \"","foo"] = comma("hi, \" world \\\" \", foo"),

    %% trailing comma
    autotest:mark(?LINE, "comma/1 - 10"),
    ["fo","ob","ar",""] = comma("fo,ob,ar,"),

    %% preceding comma
    autotest:mark(?LINE, "comma/1 - 11"),
    ["","fo","ob","ar"] = comma(",fo,ob,ar"),

    %% comma inside <>
    autotest:mark(?LINE, "comma/1 - 12"),
    ["this","is <sip:a,b@example.com>","URI","comma","test"] =
	comma("this, is <sip:a,b@example.com>, URI, comma, test"),


    %% test via(ViaList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "via/1 - 1"),
    [#via{proto="SIP/2.0/TLS", host="192.0.2.123", port=none, param=[]}] =
	via(["SIP/2.0/TLS 192.0.2.123"]),

    autotest:mark(?LINE, "via/1 - 2"),
    [#via{proto="SIP/2.0/TLS", host="192.0.2.123", port=1234, param=[]}] =
	via(["SIP/2.0/TLS 192.0.2.123:1234"]),

    autotest:mark(?LINE, "via/1 - 3"),
    [#via{proto="SIP/2.0/TLS", host="[2001:6b0:5:987::5060]", port=5060, param=[]}] =
	via(["SIP/2.0/TLS [2001:6b0:5:987::5060]:5060"]),

    autotest:mark(?LINE, "via/1 - 4"),
    [#via{proto="SIP/2.0/TLS", host="[2001:6b0:5:987::5060]", port=none, param=[]}] =
	via(["SIP/2.0/TLS [2001:6b0:5:987::5060]"]),

    autotest:mark(?LINE, "via/1 - 5"),
    [#via{proto="SIP/2.0/TLS", host="[2001:6b0:5:987::5060]", port=none, param=[]}] =
	via(["SIP/2.0/TLS 2001:6b0:5:987::5060"]),

    autotest:mark(?LINE, "via/1 - 6"),
    [#via{proto="SIP/2.0/TLS", host="[2001:6b0:5:987::5060]", port=none, param=[]},
     #via{proto="SIP/2.0/TCP", host="phone.example.org", port=none, param=["received=192.0.2.123"]}] =
	via(["SIP/2.0/TLS 2001:6b0:5:987::5060", "SIP/2.0/TCP phone.example.org;received=192.0.2.123"]),

    autotest:mark(?LINE, "via/1 - 7"),
    %% test unparsable via - fail in tokenizing
    {'EXIT', _} = (catch via(["SIP/2.0/TLS2001:6b0:5:987::5060"])),

    autotest:mark(?LINE, "via/1 - 8"),
    %% test unparsable via - fail inside sipparse_util:parse_hostport() since there is an X in the address
    {'EXIT', _} = (catch via(["SIP/2.0/TLS 2001:6b0:X:987::5060"])),

    autotest:mark(?LINE, "via/1 - 9"),
    %% test unparsable via - fail inside sipparse_util:parse_hostport() since there is a 500 in the address
    {'EXIT', _} = (catch via(["SIP/2.0/TLS 192.0.2.500:5060"])),

    autotest:mark(?LINE, "via/1 - 10"),
    %% test with Via containing stupid spaces #1, from the '3.1.1.1  A short tortuous INVITE'
    %% from draft-ietf-sipping-torture-tests-04.txt.
    [#via{proto="SIP/2.0/UDP", host="192.0.2.2", port=none, param=["branch=390skdjuw"]}] =
	via(["SIP  /   2.0/UDP   192.0.2.2;branch=390skdjuw"]),

    autotest:mark(?LINE, "via/1 - 11"),
    %% test with Via containing stupid spaces #2, from the '3.1.1.1  A short tortuous INVITE'
    %% from draft-ietf-sipping-torture-tests-04.txt.
    [#via{proto="SIP/2.0/TCP", host="spindle.example.com", port=none, param=[" branch =  z9hG4bK9ikj8"]}] =
	via(["SIP  / 2.0  / TCP     spindle.example.com   ; branch =  z9hG4bK9ikj8"]),

    autotest:mark(?LINE, "via/1 - 12"),
    %% test with Via containing stupid spaces #3, from the '3.1.1.1  A short tortuous INVITE'
    %% from draft-ietf-sipping-torture-tests-04.txt.
    [#via{proto="SIP/2.0/UDP", host="192.168.255.111", port=none, param=[" branch=z9hG4bK30239"]}] =
	via(["SIP  /    2.0   / UDP  192.168.255.111   ; branch=z9hG4bK30239"]),

    autotest:mark(?LINE, "via/1 - 13"),
    %% test via with multiple parameters and some ill-placed tabs
    %% XXX are Via-param values case sensitive or not?
    [#via{proto="SIP/2.0/UDP", host="192.0.2.1", port=none, param=["foo=BaR", "bar=\"BaZ\""]}] =
	  via(["SIP \t/\t 2.0 \t /\tUDP\t192.0.2.1\t;foo=BaR;bar=\"BaZ\""]),

    autotest:mark(?LINE, "via/1 - 14"),
    %% test via with whitespace after-colon-before-port - valid according to RFC3261 #20.42
    [#via{proto="SIP/2.0/UDP", host="first.example.com", port=4000,
	  param=["ttl=16", "maddr=224.2.0.1 ", "branch=z9hG4bK-foo"]}] =
	via(["SIP / 2.0 / UDP first.example.com: 4000;ttl=16;maddr=224.2.0.1 ;branch=z9hG4bK-foo"]),

    autotest:mark(?LINE, "via/1 - 15"),
    %% test via with whitespace before colon-port - valid according to my understanding of the BNF
    %% COLON   =  SWS ":" SWS ; colon
    [#via{proto="SIP/2.0/UDP", host="first.example.com", port=4000,
	  param=["ttl=16", "maddr=224.2.0.1 ", "branch=z9hG4bK-foo"]}] =
	via(["SIP / 2.0 / UDP first.example.com :4000;ttl=16;maddr=224.2.0.1 ;branch=z9hG4bK-foo"]),

    autotest:mark(?LINE, "via/1 - 16"),
    %% test via with whitespace before and after colon in host-colon-port -
    %% valid according to my understanding of the BNF
    %% COLON   =  SWS ":" SWS ; colon
    [#via{proto="SIP/2.0/UDP", host="first.example.com", port=4000, param=[]}] =
	via(["SIP / 2.0 / UDP first.example.com : 4000"]),

    autotest:mark(?LINE, "via/1 - 17"),
    %% test that we don't accept Vias with space between host and port, but no colon
    {error, {invalid_proto_host_port_in_via, _}} =
     (catch via(["SIP/2.0/UDP first.example.com 4000"])),

    autotest:mark(?LINE, "via/1 - 18"),
    %% test with semi-colon but no parameters
    [#via{proto="SIP/2.0/UDP", host="example.com", port=none, param=[]}] =
	via(["SIP/2.0/UDP example.com;"]),


    %% test topvia(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "topvia/1 - 1"),
    %% test simple case
    [TopVia_1] = via(["SIP/2.0/TLS sip.example.org:5061"]),
    TopVia_1 = topvia( keylist:from_list([{"Via", ["SIP/2.0/TLS sip.example.org:5061",
						   "SIP/2.0/TLS foo.example.org:5060"]}])),

    autotest:mark(?LINE, "topvia/1 - 2"),
    %% test without via
    none = topvia( keylist:from_list([])),


    %% test get_via_branch(TopVia)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_via_branch/1 - 1"),
    %% test YXA branch
    [GetViaBranch_1] = via(["SIP/2.0/TLS sip.example.org:5061;branch=z9hG4bK-yxa-abc123-oloopcookie"]),
    "z9hG4bK-yxa-abc123" = get_via_branch(GetViaBranch_1),

    autotest:mark(?LINE, "get_via_branch/1 - 2"),
    %% test YXA branch without loop cookie
    [GetViaBranch_2] = via(["SIP/2.0/TLS sip.example.org:5061;branch=z9hG4bK-yxa-abc123"]),
    "z9hG4bK-yxa-abc123" = get_via_branch(GetViaBranch_2),

    autotest:mark(?LINE, "get_via_branch/1 - 3"),
    %% test non-YXA branch
    [GetViaBranch_3] = via(["SIP/2.0/TLS sip.example.org:5061;branch=z9hG4bK-abc123-oloopcookie"]),
    "z9hG4bK-abc123-oloopcookie" = get_via_branch(GetViaBranch_3),

    autotest:mark(?LINE, "get_via_branch/1 - 3"),
    %% test Via without branch
    [GetViaBranch_4] = via(["SIP/2.0/TLS sip.example.org:5061"]),
    none = get_via_branch(GetViaBranch_4),


    %% test via_params(Via)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "via_params/1 - 1"),
    %% test simple case
    [ViaParams_Via1] = via(["SIP/2.0/TCP 192.0.2.123;transport=tcp;rport"]),
    ["rport", "transport=tcp"] = dict_to_param( via_params(ViaParams_Via1) ),


    %% test name_header(String)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "name_header/1 - 1"),
    %% test without Display Name, just URI
    NameHeaderURI1 = sipurl:parse("sip:ft@example.org"),
    {none, NameHeaderURI1} = name_header("sip:ft@example.org"),

    autotest:mark(?LINE, "name_header/1 - 1"),
    %% test without Display Name, just URI inside <>
    NameHeaderURI1 = sipurl:parse("sip:ft@example.org"),
    {none, NameHeaderURI1} = name_header("<sip:ft@example.org>"),

    autotest:mark(?LINE, "name_header/1 - 3"),
    %% test with unqouted Display Name
    {"Fredrik", NameHeaderURI1} = name_header("Fredrik    <sip:ft@example.org>"),

    autotest:mark(?LINE, "name_header/1 - 4"),
    %% test with qouted Display Name
    {"testing, Fredrik", NameHeaderURI1} = name_header("\"testing, Fredrik\" <sip:ft@example.org>"),

    autotest:mark(?LINE, "name_header/1 - 5"),
    %% test with URI missing <>
    {none, {unparseable, "Fredrik sip:ft@example.org"}} = name_header("Fredrik sip:ft@example.org"),

    autotest:mark(?LINE, "name_header/1 - 6 (disabled)"),
    %% test with quoted quotes in the display name
    %%{"Quoted \\\" here", NameHeaderURI1} = name_header("\"Quoted \\\" here\" <sip:ft@example.org>"),


    %% test via_is_equal(A, B)
    %%--------------------------------------------------------------------
    [ViaIsEqual1] = via(["SIP/2.0/TLS sip.example.org:5061;branch=z9hG4bK-really-unique"]),
    [ViaIsEqual2] = via(["SIP/2.0/TLS sip.example.org:5061;branch=z9hG4bK-not-the-same"]),	%% different branch
    [ViaIsEqual3] = via(["SIP/2.0/TLS sip.example.org:5062;branch=z9hG4bK-really-unique"]),	%% different port
    [ViaIsEqual4] = via(["SIP/2.0/TLS sip.example.net:5061;branch=z9hG4bK-really-unique"]),	%% different host
    [ViaIsEqual5] = via(["SIP/2.0/TCP sip.example.org:5061;branch=z9hG4bK-really-unique"]),	%% different protocol

    autotest:mark(?LINE, "via_is_equal/2 - 1"),
    %% two of the same
    true = via_is_equal(ViaIsEqual1, ViaIsEqual1),

    autotest:mark(?LINE, "via_is_equal/2 - 2"),
    %% different branch parameter
    false = via_is_equal(ViaIsEqual1, ViaIsEqual2),

    autotest:mark(?LINE, "via_is_equal/2 - 3"),
    %% different port
    false = via_is_equal(ViaIsEqual1, ViaIsEqual3),

    autotest:mark(?LINE, "via_is_equal/2 - 4"),
    %% different host
    false = via_is_equal(ViaIsEqual1, ViaIsEqual4),

    autotest:mark(?LINE, "via_is_equal/2 - 5"),
    %% different protocol
    false = via_is_equal(ViaIsEqual1, ViaIsEqual5),


    %% test via_is_equal(A, B, CmpList)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "via_is_equal/3 - 1"),
    true = via_is_equal(ViaIsEqual1, ViaIsEqual2, [proto, host, port]),

    autotest:mark(?LINE, "via_is_equal/3 - 2"),
    false = via_is_equal(ViaIsEqual1, ViaIsEqual2, [parameters]),

    autotest:mark(?LINE, "via_is_equal/3 - 2"),
    %% test with same parameters, but different order
    true = via_is_equal(ViaIsEqual1#via{param=["a=b", "b=a"]},
			ViaIsEqual2#via{param=["b=a", "a=b"]}, [parameters]),


    %% test get_server_transaction_id(Request)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_server_transaction_id/1 - 1.1"),
    %% get Id for INVITE with RFC3261 branch tag in top Via
    InviteHeader1 = keylist:from_list([
				       {"Via",	["SIP/2.0/TLS sip.example.org:5061;branch=z9hG4bK-really-unique"]},
				       {"From", ["<sip:alice@example.org>;tag=f-abc"]},
				       {"To",	["<sip:bob@example.org>"]},
				       {"Call-ID", ["3c26722ce234@192.0.2.111"]},
				       {"CSeq",	["2 INVITE"]}
				      ]),
    Invite1 = #request{method="INVITE", uri=sipurl:parse("sip:alice@example.org"),
		       header=InviteHeader1, body = <<>>},
    Invite1Id = get_server_transaction_id(Invite1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 1.2"),
    %% check result
    {"z9hG4bK-really-unique", {"SIP/2.0/TLS", "sip.example.org", 5061}, "INVITE"} = Invite1Id,

    autotest:mark(?LINE, "get_server_transaction_id/1 - 2"),
    %% make an ACK for an imagined 3xx-6xx response with to-tag "t-123"
    AckInvite1Header_1 = keylist:set("To", ["<sip:bob@example.org>;tag=t-123"], InviteHeader1),
    AckInvite1Header1  = keylist:set("CSeq", ["2 ACK"], AckInvite1Header_1),
    AckInvite1 = #request{method="ACK", uri=sipurl:parse("sip:alice@example.org"),
			  header=AckInvite1Header1, body = <<>>},

    AckInvite1Id = get_server_transaction_id(AckInvite1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 3"),
    %% Test that the INVITE id matches the ACK id
    Invite1Id = AckInvite1Id,

    autotest:mark(?LINE, "get_server_transaction_id/1 - 4.1"),
    %% get Id for INVITE with RFC2543 branch tag in top Via
    Invite2543_1Header = keylist:from_list([
					    {"Via",	["SIP/2.0/TLS sip.example.org:5061;branch=not-really-unique"]},
					    {"From",	["<sip:alice@example.org>;tag=f-abc"]},
					    {"To",	["<sip:bob@example.org>"]},
					    {"Call-ID", ["3c26722ce234@192.0.2.111"]},
					    {"CSeq",	["2 INVITE"]}
					   ]),
    Invite2543_1 = #request{method="INVITE", uri=sipurl:parse("sip:alice@example.org"),
			    header=Invite2543_1Header, body = <<>>},
    Invite2543_1Id = get_server_transaction_id(Invite2543_1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 4.2"),
    %% check result
    {#sipurl{proto="sip", user="alice", pass=none, host="example.org", port=none, param_pairs={url_param,[]}},
     none,
     "f-abc",
     "3c26722ce234@192.0.2.111", {"2", "INVITE"},
     {via, "SIP/2.0/TLS", "sip.example.org", 5061, ["branch=not-really-unique"]}
    } = Invite2543_1Id,

    autotest:mark(?LINE, "get_server_transaction_id/1 - 5.1"),
    %% for RFC2543 INVITE, we must also get the ACK-id to match future ACKs with this INVITE
    Invite2543_1AckId = get_server_transaction_ack_id_2543(Invite2543_1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 5.2"),
    %% check result
    {#sipurl{proto="sip", user="alice", pass=none, host="example.org", port=none, param_pairs={url_param,[]}},
     "f-abc",
     "3c26722ce234@192.0.2.111",
     {"2","INVITE"},
     {via,"SIP/2.0/TLS","sip.example.org",5061,[]}
    } = Invite2543_1AckId,

    autotest:mark(?LINE, "get_server_transaction_id/1 - 6"),
    %% make an ACK for an imagined 3xx-6xx response with to-tag "t-123", check that
    %% get_server_transaction_id refuses and tells us it is an 2543 ACK
    AckInvite2543_1Header_1 = keylist:set("To", ["<sip:bob@example.org>;tag=t-123"], Invite2543_1Header),
    AckInvite2543_1Header   = keylist:set("CSeq", ["2 ACK"], AckInvite2543_1Header_1),
    AckInvite2543_1 = #request{method="ACK", uri=sipurl:parse("sip:alice@example.org"),
			       header=AckInvite2543_1Header, body = <<>>},

    is_2543_ack = get_server_transaction_id(AckInvite2543_1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 7"),
    %% now get the 2543 ACK id from the ACK
    AckInvite2543_1Id = get_server_transaction_ack_id_2543(AckInvite2543_1),

    autotest:mark(?LINE, "get_server_transaction_id/1 - 8"),
    %% check that the 2543 ACK id matches the 2543 INVITE id
    AckInvite2543_1Id = Invite2543_1AckId,


    %% test get_client_transaction_id(Response)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_client_transaction_id/1 - 1"),
    Response1 = #response{status=699, reason="foo", header=Invite2543_1Header, body = <<>>},
    {"not-really-unique", "INVITE"} = get_client_transaction_id(Response1),


    %% test contact(Header)
    %% No full test of parameters - we don't test the actual contact parsing here.
    %%--------------------------------------------------------------------
    ContactHeader1 = keylist:from_list([
					{"From", ["Test <sip:alice@example.org>;tag=f-abc"]},
					{"Contact", ["<sip:bob@example.org>;lr=true"]}
				       ]),

    autotest:mark(?LINE, "contact/1 - 1"),
    %% test using list key
    [#contact{urlstr="sip:bob@example.org"}] = contact(ContactHeader1),


    %% test contact(Header, Name).
    %% No full test of parameters - we don't test the actual contact parsing here.
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "contact/2 - 1"),
    %% test using atom key
    [#contact{urlstr="sip:alice@example.org"}] = contact2(ContactHeader1, 'from'),


    %% test via_sentby(Via)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "via_sentby/1 - 1"),
    {"proto", "host", 1234} = via_sentby(#via{proto="proto", host="host", port=1234}),


    %% test via_print(Via)
    %%--------------------------------------------------------------------
    ViaPrint1 = #via{proto = "SIP/2.0/UDP",
		     host = "example.org",
		     port = 5060,
		     param = ["foo=bar", "user=ft"]
		    },
    ViaPrint1_Str = "SIP/2.0/UDP example.org:5060;foo=bar;user=ft",

    ViaPrint2 = ViaPrint1#via{port = none},
    ViaPrint2_Str = "SIP/2.0/UDP example.org;foo=bar;user=ft",

    autotest:mark(?LINE, "via_print/1 - 1"),
    %% one Via
    [ViaPrint1_Str] = via_print([ViaPrint1]),

    autotest:mark(?LINE, "via_print/1 - 2"),
    %% one Via, not in list
    [ViaPrint1_Str] = via_print(ViaPrint1),

    autotest:mark(?LINE, "via_print/1 - 3"),
    %% two Vias
    [ViaPrint1_Str, ViaPrint2_Str] = via_print([ViaPrint1, ViaPrint2]),


    %% test auth_print(Auth)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "auth_print/1 - 1"),
    ["Digest realm=\"su.se\", nonce=\"nonce\", opaque=\"opaque\""] =
	auth_print({"su.se", "nonce", "opaque"}),


    %% test auth_print(Auth, Stale)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "auth_print/2 - 1"),
    ["Digest realm=\"su.se\", nonce=\"nonce\", opaque=\"opaque\", stale=true"] =
	auth_print({"su.se", "nonce", "opaque"}, true),

    autotest:mark(?LINE, "auth_print/2 - 2"),
    ["Digest realm=\"su.se\", nonce=\"nonce\", opaque=\"opaque\""] =
	auth_print({"su.se", "nonce", "opaque"}, false),



    %% test auth(In)
    %%--------------------------------------------------------------------
    AuthIn1 = "Digest response=\"1response1\", uri=\"2bar2\"",
    AuthIn2 = "GSSAPI response=\"1response1\", uri=\"2bar2\", noquote=test",
    AuthIn3 = "Unknown response=\"1response1\", uri=\"2bar2\"",

    AuthOut1 = [{"response","1response1"}, {"uri","2bar2"}],
    AuthOut2 = [{"noquote", "test"}, {"response","1response1"}, {"uri","2bar2"}],

    autotest:mark(?LINE, "auth/1 - 1"),
    %% digest
    AuthOut1 = lists:keysort(1, dict:to_list( auth(AuthIn1) )),

    autotest:mark(?LINE, "auth/1 - 2"),
    %% GSSAPI
    AuthOut2 = lists:keysort(1, dict:to_list( auth(AuthIn2) )),

    autotest:mark(?LINE, "auth/1 - 3"),
    %% Unknown
    {siperror, 500, "Server Internal Error"} = (catch auth(AuthIn3)),

    autotest:mark(?LINE, "auth/1 - 4"),
    %% Invalid
    {siperror, 500, "Server Internal Error"} = (catch auth("TestInvalid")),

    autotest:mark(?LINE, "auth/1 - 4"),
    %% Not handled - not sure if we should or not..
    {'EXIT', _} = (catch auth("Digest foo")),


    %% test param_to_dict(Param)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "param_to_dict/1 - 1"),
    %% test simple case
    [{"foo", "bar"}] = dict:to_list(
			 param_to_dict(["foo=bar"])
			),

    autotest:mark(?LINE, "param_to_dict/1 - 2"),
    %% test more complicated case - uppercase in key, escaped characters in value and multiple entrys
    ["foo=bAr", "user=ft"] = dict_to_param( param_to_dict(["Foo=b%41r", "user=ft"]) ),


    %% test cseq(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "cseq/1 - 1"),
    %% test valid CSeq
    {"1", "INVITE"} = cseq( keylist:from_list([{"CSeq", ["1 INVITE"]}]) ),

    autotest:mark(?LINE, "cseq/1 - 2"),
    %% test invalid CSeq
    {unparseable, "INVITE_1"} = cseq( keylist:from_list([{"CSeq", ["INVITE_1"]}]) ),


    %% test cseq_print({Seq, Method})
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "cseq_print/1 - 1"),
    "1 INVITE" = cseq_print({"1", "INVITE"}),


    %% test callid(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "callid/1 - 1"),
    "call-id-test" = callid( keylist:from_list([{"Call-Id", ["call-id-test"]}]) ),


    %% test build_header_binary(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "build_header_binary/1 - 1"),
    %% test single header
    <<"Call-Id: call-id-test\r\n">> = build_header_binary( keylist:from_list([{"Call-Id", ["call-id-test"]}]) ),

    autotest:mark(?LINE, "build_header_binary/1 - 2"),
    %% test multiple values
    <<"Via: via1\r\nVia: via2\r\n">> = build_header_binary( keylist:from_list([{"Via", ["via1", "via2"]}]) ),

    autotest:mark(?LINE, "build_header_binary/1 - 3"),
    %% test more complex case
    BuildHeaderBin_H1 = keylist:from_list([{"Via", ["via1", "via2"]},
					   {"Call-Id", ["call-id-test"]},
					   {"Accept", ["accept1", "accept2"]},
					   {"Date", ["Thu, 10 Feb 2005 14:41:04 GMT"]}
					  ]),

    <<"Via: via1\r\n"
     "Via: via2\r\n"
     "Call-Id: call-id-test\r\n"
     "Date: Thu, 10 Feb 2005 14:41:04 GMT\r\n"
     "Accept: accept1, accept2\r\n">> = build_header_binary(BuildHeaderBin_H1),

    autotest:mark(?LINE, "build_header_binary/1 - 4"),
    %% test Reason-header without value
    <<"Reason: ", 13, 10>> = build_header_binary( keylist:from_list([{"Reason", []}]) ),

    autotest:mark(?LINE, "build_header_binary/1 - 5"),
    %% test the sorting of headers
    BuildHeaderBin_H2 = keylist:from_list([{"Call-Id", ["call-id-test2"]},
					   {"Via", ["via1", "via2"]},
					   {"Route", ["<sip:example.org;lr>"]},
                                           {"Accept", ["accept1", "accept2"]},
                                           {"Date", ["Thu, 10 Feb 2005 14:41:04 GMT"]},
					   {"From", ["Fredrik <sip:testing@example.org>"]}
                                          ]),
    <<"Via: via1\r\n"
     "Via: via2\r\n"
     "From: Fredrik <sip:testing@example.org>\r\n"
     "Call-Id: call-id-test2\r\n"
     "Route: <sip:example.org;lr>\r\n"
     "Date: Thu, 10 Feb 2005 14:41:04 GMT\r\n"
     "Accept: accept1, accept2\r\n">> = build_header_binary(BuildHeaderBin_H2),

    %% test get_tag([String])
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_tag/1 - 1"),
    "foo" = get_tag(["\"Fredrik\" <sip:ft@example.org>;test=auto;tag=foo"]),

    autotest:mark(?LINE, "get_tag/1 - 2"),
    "foo" = get_tag(["\"Fredrik\" <sip:ft@example.org>;tag=foo;test=auto"]),

    autotest:mark(?LINE, "get_tag/1 - 3"),
    none = get_tag(["\"Fredrik\" <sip:ft@example.org>;test=auto"]),

    autotest:mark(?LINE, "get_tag/1 - 4"),
    %% >;tag= appearing more than once
    "foo" = get_tag(["\"Evil <evil>;tag=bar displayname\" <sip:ft@example.org>;tag=foo"]),

    autotest:mark(?LINE, "get_tag/1 - 5"),
    %% token names (tag=) should be case-insensitive
    "foo" = get_tag(["<sip:ft@example.org>;Tag=foo"]),


    %% test dialogid(Header)
    %%--------------------------------------------------------------------
    DialogHeader1 = keylist:from_list([{"Call-Id", ["call-id-test"]},
				       {"From", ["<sip:ft@example.org>;tag=fromtag"]},
				       {"To", ["<sip:ft@example.org>;tag=totag"]}
				      ]),

    autotest:mark(?LINE, "dialogid/1 - 1"),
    {"call-id-test", "fromtag", "totag"} = dialogid(DialogHeader1),

    autotest:mark(?LINE, "dialogid/1 - 2"),
    {"call-id-test", "fromtag", none} = dialogid(keylist:set("To", ["sip:foo@example.org"], DialogHeader1)),

    autotest:mark(?LINE, "dialogid/1 - 3"),
    {"call-id-test", none, "totag"} = dialogid(keylist:set("From", ["sip:foo@example.org"], DialogHeader1)),


    %% test expires(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "expires/1 - 1"),
    ["123"] = expires(keylist:from_list([{"Expires", ["123"]}])),


    %% test from(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "from/1 - 1"),
    FromURL1 = sipurl:parse("sip:ft@example.org"),
    {"Fredrik", FromURL1} = from(keylist:from_list([{"From", ["Fredrik <sip:ft@example.org>"]}])),


    %% test to(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "to/1 - 1"),
    ToURL1 = sipurl:parse("sip:ft@example.org"),
    {"Fredrik", ToURL1} = to(keylist:from_list([{"To", ["\"Fredrik\" <sip:ft@example.org>"]}])),


    %% test route(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "route/1 - 1"),
    [RouteContact1] = route(keylist:from_list([{"Route", ["<sip:example.org>"]}])),
    #contact{urlstr = "sip:example.org"} = RouteContact1,


    %% test record_route(Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "record_route/1 - 1"),
    [RecordRouteContact1] = record_route(keylist:from_list([{"Record-Route", ["<sip:rr.example.org>"]}])),
    #contact{urlstr = "sip:rr.example.org"} = RecordRouteContact1,


    %% test contact_print(Contacts)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "contact_print/1 - 1"),
    ["<sip:example.org>","<sip:rr.example.org>"] = contact_print([RouteContact1, RecordRouteContact1]),


    %% test httparg(String)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "httparg/1 - 1"),
    ["bar","baz=true","foo"] = dict_to_param( httparg("foo&bar&baz=true") ),

    %% is_supported(Extension, Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_supported/2 - 1"),
    true = is_supported("foo", keylist:from_list([{"Supported", ["foo", "bar"]}])),

    autotest:mark(?LINE, "is_supported/2 - 2"),
    false = is_supported("test", keylist:from_list([{"Supported", ["foo", "bar"]}])),

    autotest:mark(?LINE, "is_supported/2 - 3"),
    false = is_supported("test", keylist:from_list([])),


    %% is_required(Extension, Header)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_required/2 - 1"),
    true = is_required("foo", keylist:from_list([{"Require", ["foo", "bar"]}])),

    autotest:mark(?LINE, "is_required/2 - 2"),
    false = is_required("test", keylist:from_list([{"Require", ["foo", "bar"]}])),

    autotest:mark(?LINE, "is_required/2 - 3"),
    false = is_required("test", keylist:from_list([])),


    ok.
