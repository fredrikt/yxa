%%
%% Note: some functions do several passes over the same string() to
%% parse it, e.g. 1 pass to find start and end delimiter and one pass
%% for reading the the elements from start to end index, and probably
%% a additional scan to reach the remainder of the string.
%% This performance can be improved, by using a accumulator equiped
%% parser that returns the Match and Rest part of the string, in a
%% single scan.
%%
%% Note: elements are often only partially parsed - this reduces the
%% amount of parsing done on entries which will only be passed along,
%% but while this improves performance it increases the need for later
%% exception handling.
%%--------------------------------------------------------------------

-module(sipheader).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 expire/1,
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

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: comma(String)
%% Descrip.: split a comma separated string into separate strings,
%%           along the commas (don't include them).
%%           Each substring is cleared of any preceding or trailing
%%           spaces. example: "foo, bar, zop, \"quoted, hi\"" ->
%%           ["foo","bar","zop", "\"quoted, hi\""]
%%
%% Returns : list() of string()
%%--------------------------------------------------------------------
comma(String) ->
    comma([], String, false, false).

% comma(Parsed, Rest, Inquote, InUriQuote)

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
comma(Parsed, [$< | Rest], false, false) ->
    comma([$< | Parsed], Rest, false, true);

%% end of quoted uri
comma(Parsed, [$> | Rest], false, true) ->
    comma([$> | Parsed], Rest, false, false);

%% inside quoted uri - ignore comma or any other special chars
comma(Parsed, [Char | Rest], false, true) ->
    comma([Char | Parsed], Rest, false, true);

%% -------------

%% an escape code e.g. "\," has been found, don't treat it in any special manner
%% only done inside a quoted segment of the string
comma(Parsed, [$\\, Char | Rest], true, false) ->
    comma([Char, $\\ | Parsed], Rest, true, false);

%% Inquote = false, we have now entered inside a quoted ("...") string segment
comma(Parsed, [$\" | Rest], false, false) ->
    comma([$\" | Parsed], Rest, true, false);

%% Inquote = true and a new quote (") found - end of quoted string segment
comma(Parsed, [$\" | Rest], true, false) ->
    comma([$\" | Parsed], Rest, false, false);

%% regular char, store in current string
comma(Parsed, [Char | Rest], true, false) ->
    comma([Char | Parsed], Rest, true, false);

%% -------------

%% Inquote = false, so comma is a comma that splits string
%% start looking for the next one
comma(Parsed, [$, | Rest], false, false) ->
    [lists:reverse(string:strip(Parsed, both)) | comma([], Rest, false, false)];

%% regular char, store in current string
comma(Parsed, [Char | Rest], false, false) ->
    comma([Char | Parsed], Rest, false, false);

%% end of string, clean up last comma separated entry (it's an error
%% if Inquote or InUriQuote = true, the quotes are then unbalanced)
comma(Parsed, [], false, false) ->
    [lists:reverse(string:strip(Parsed, both))].

%--------------------------------------------------------------------
%% Function: expire(Header)
%%           Header = keylist record()
%% Descrip.: get the value of the "Expires" header
%% Returns : [Expire] | []
%%           Expire = numerical string()
%%--------------------------------------------------------------------
expire(Header) when is_record(Header, keylist) ->
    keylist:fetch('expires', Header).

%%--------------------------------------------------------------------
%% Function: to([String])
%%           to(Header)
%%           String = string(), the contents of a TO header (usually
%%           the result of keylist:fetch(to, Header))
%%           Header = keylist record()
%% Descrip.: parse header data
%% Returns : {Displayname, URI} (see name_header/1)
%%--------------------------------------------------------------------
to(Header) when is_record(Header, keylist) ->
    to(keylist:fetch('to', Header));

to([String]) ->
    name_header(String).

%%--------------------------------------------------------------------
%% Function: from([String])
%%           from(Header)
%%           String = string(), the contents of a FROM header (usually
%%           the result of keylist:fetch('from', Header))
%%           Header = keylist record()
%% Descrip.: parse header data
%% Returns : {Displayname, URI} (see name_header/1)
%%--------------------------------------------------------------------
from(Header) when is_record(Header, keylist) ->
    from(keylist:fetch('from', Header));

from([String]) ->
    name_header(String).

%%--------------------------------------------------------------------
%% Function: contact(Header)
%%           route(Header)
%%           record_route(Header)
%%           Header = keylist record()
%% Descrip.: return the contact/route/record-route/... entries
%%           contained in Header
%% Returns : list() of contact record()
%% XXX should we use contact record() for all of these headers ???
%% they may benefit from their own record type.
%%--------------------------------------------------------------------
contact(Header) when is_record(Header, keylist) ->
    contact(Header, contact).

route(Header) when is_record(Header, keylist) ->
    contact(Header, route).

record_route(Header) when is_record(Header, keylist) ->
    contact(Header, 'record-route').

contact(Header, Name) when is_record(Header, keylist), is_atom(Name); is_list(Name) ->
    V = keylist:fetch(Name, Header),
    contact:parse(V).

%%--------------------------------------------------------------------
%% Function: via(ViaList)
%%           via(Header)
%%           ViaList = list() of string(), string() = the contents of
%%                     Via: header. Usually the result of
%%                     keylist:fetch('via', Header)
%%           Header = keylist record()
%% Descrip.: parse header data
%% Returns : list() of via record() | throw()
%%--------------------------------------------------------------------
via(Header) when is_record(Header, keylist) ->
    via(keylist:fetch('via', Header));

via([]) ->
    [];
via([String | Rest]) ->
    Headers = comma(String),
    lists:append(lists:map(fun(H) ->
				   [Protocol, Sentby] = string:tokens(H, " "),
				   [Hostport | Parameters ] = string:tokens(Sentby, ";"),
				   case sipparse_util:parse_hostport(Hostport) of
				       {Host, Port} when is_list(Host),
							 is_integer(Port); Port == none ->
					   #via{proto=Protocol, host=Host, port=Port, param=Parameters};
				       _ ->
					   throw({error, unparseable_via})
				   end
			   end, Headers),
		 via(Rest)).

%%--------------------------------------------------------------------
%% Function: topvia(Header)
%%           Header = keylist record()
%% Descrip.: get the first Via entry from Header
%% Returns : via record() |
%%           none - if not found in Header
%%--------------------------------------------------------------------
topvia(Header) when is_record(Header, keylist) ->
    case via(Header) of
	[] -> none;
	[TopVia | _] when is_record(TopVia, via) -> TopVia;
	_ -> error
    end.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.: print string() separated by ";"
%% Returns :
%%--------------------------------------------------------------------
print_parameters([]) ->
    "";
print_parameters([A | B]) ->
    ";" ++ A ++ print_parameters(B).

%%--------------------------------------------------------------------
%% Function:
%% Descrip.: print via record() or list() of via record()
%% Returns :
%%--------------------------------------------------------------------
via_print(Via) when is_record(Via, via) ->
    via_print([Via]);
via_print(Via) when is_list(Via) ->
    lists:map(fun(H) ->
		      {Protocol, Host, Port, Parameters} = {H#via.proto, H#via.host, H#via.port, H#via.param},
		      Protocol ++ " " ++ sipurl:print_hostport(Host, Port) ++ print_parameters(Parameters)
	      end, Via).

%%--------------------------------------------------------------------
%% Function: via_params(Via)
%%           Via = via record()
%% Descrip.: convert parameters stored in Via to a dictionary
%% Returns : dict()
%%--------------------------------------------------------------------
via_params(Via) when is_record(Via, via) ->
    param_to_dict(Via#via.param).

%%--------------------------------------------------------------------
%% Function: contact_print(Contacts)
%%           Contacts = list() of contact record(), containing contact
%%           from contact/1
%% Descrip.: Take a list of contact records, and return a list of
%%           those contacts as strings
%% Returns : list() of string()
%%--------------------------------------------------------------------
contact_print(ContactList) when is_list(ContactList) ->
    contact_print2(ContactList, []).

contact_print2([Contact | R], Res) when is_record(Contact, contact) ->
    contact_print2(R, [contact:print(Contact) | Res]);
contact_print2([], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
auth_print(Auth) ->
    auth_print(Auth, false).

auth_print(Auth, Stale) ->
    {Realm, Nonce, Opaque} = Auth,
    ["Digest realm=\"" ++ Realm ++ "\", nonce=\"" ++ Nonce ++ "\", opaque=\"" ++ Opaque ++ "\"" ++
     case Stale of
	 true ->
	     ", stale=true";
	 _ ->
	     ""
     end
    ].

%%--------------------------------------------------------------------
%% Function: auth([In])
%%           In = string()
%% Descrip.: parse authrization header
%% Returns : throw() | dict()
%%--------------------------------------------------------------------
auth([In]) when is_list(In) ->
    %% lowercase first word (to implement case insensitivity)
    case string:chr(In, $\ ) of	%% Find first space ($\ ) is a space
	0 ->
	    %% no space found, really badly formatted data
	    logger:log(error, "sipheader:auth() called with unparsable input (~p)", [In]),
	    throw({siperror, 500, "Server Internal Error"});
	Index ->
	    FirstWord = string:substr(In, 1, Index - 1),
	    Rest = string:substr(In, Index + 1),
	    LCfw = httpd_util:to_lower(FirstWord),
	    auth2(LCfw, Rest)
    end.

auth2("gssapi", String) ->
    Headers = comma(String),
    F = fun get_name_and_value/1,
    L = lists:map(F , Headers),
    dict:from_list(L);

auth2("digest", String) ->
    Headers = comma(String),
    F = fun get_name_and_value/1,
    L = lists:map(F, Headers),
    dict:from_list(L);

auth2(Type, String) ->
    logger:log(error, "sipheader:auth2() called with unrecognized authentication data (~p, ~p)", [Type, String]),
    throw({siperror, 500, "Server Internal Error"}).

get_name_and_value(Str) ->
    H = string:strip(Str,left),
    Index = string:chr(H, $=),
    Name = string:substr(H, 1, Index - 1),
    Value = string:substr(H, Index + 1),
    {Name, unquote(Value)}.

%% removes single pair of quotes, returns contents in between these first two quotes
unquote([$" | QString]) ->
    Index = string:chr(QString, $"),
    string:substr(QString, 1, Index - 1);

unquote(QString) ->
    QString.


%%--------------------------------------------------------------------
%% Function: param_to_dict(Param)
%%           Param = list() of string(), each string is a "para=value"
%%           pair, that may have preceding or trailing spaces as well
%%           as hex encoded values (e.g. chars of the format %hh,
%%           h = hex number)
%% Descrip.: convert sip paramter strings into dictionary
%% Returns : dict()
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

%% convert (2) hex codes to singel 8 bit char
unescape([]) ->
    [];
unescape([$%, C1, C2 | Rest]) ->
    [hex:from([C1, C2]) | unescape(Rest)];
unescape([C | Rest]) ->
    [C | unescape(Rest)].

%%--------------------------------------------------------------------
%% Function: dict_to_param(Dict)
%%           Dict = dict(), a dictionary containing parameter entries
%% Descrip.: convert a dictionary containing parameters back into a
%%           "name=value" format - the inverse of param_to_dict/1
%% Returns : list() of string()
%% XXX should certain chars in the "value" part be hex encoded ?
%%--------------------------------------------------------------------
dict_to_param(Dict) ->
    list_to_parameters(lists:keysort(1, dict:to_list(Dict))).

list_to_parameters([]) ->
    [];
list_to_parameters([{Key, Value}]) ->
    [Key ++ "=" ++ Value];
list_to_parameters([{Key, Value} | Rest]) ->
    [Key ++ "=" ++ Value | list_to_parameters(Rest)].


%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
httparg(String) ->
    Headers = string:tokens(String, "&"),
    param_to_dict(Headers).


%%--------------------------------------------------------------------
%% Function: cseq([String])
%%           cseq(Header)
%%           String = string(), the contents of a CSEQ header (usually
%%           the result of keylist:fetch(cseq, Header))
%%           Header = keylist record()
%% Descrip.: parse header data
%% Returns : {Seq, Method} | {unparseable, String}
%%           Seq    = integer()
%%           Method = string()
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
%% Function: cseq_print({Seq, Method})
%%           Seq    = integer()
%%           Method = string()
%% Descrip.: print data parsed with cseq/1
%% Returns : string()
%%--------------------------------------------------------------------
cseq_print({Seq, Method}) when is_list(Seq), is_list(Method) ->
    %% XXX Seq should be integer
    Seq ++ " " ++ Method.

%%--------------------------------------------------------------------
%% Function: callid(Header)
%%           Header = keylist record()
%% Descrip.: get call-id from header
%% Returns : string()
%% XXX does not handle non-existing Call-Id, but that means our
%% callers might not either, so the right solution might not be to
%% make us return [] or 'none' if there is no Call-Id.
%%--------------------------------------------------------------------
callid(Header) when is_record(Header, keylist) ->
    [CallId] = keylist:fetch('call-id', Header),
    CallId.

%%--------------------------------------------------------------------
%% Function: build_header_binary(Header)
%%           Header = keylist record()
%% Descrip.: build a sip header
%% Returns : binary() | throw()
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
    Lines.


print_one_header_binary(_, _, []) ->
    [];
print_one_header_binary(Key, Name, ValueList) ->
    %% certain headers that have multple values are written on a single line separated by "," -
    %% this is not because any RFC says so but because these are common headers that look
    %% considerably much better when appearing on one line
    OneLine =
	if
	    Key == accept ->		true;
	    Key == allow ->		true;
	    Key == 'allow-events' ->	true;
	    Key == supported ->		true;
	    Key == require ->		true;
	    Key == 'proxy-require' ->	true;
	    Key == 'rtp-rxstat' ->	true;	%% Cisco 79xx
	    Key == 'rtp-txstat' ->	true;	%% Cisco 79xx
	    true -> false
	end,
    MakeBin = fun(V) ->                      list_to_binary(V)
              end,
    BinValueList = lists:map(MakeBin, ValueList),
    BinName = list_to_binary(Name),
    print_one_header_binary2(OneLine, BinName, BinValueList).

print_one_header_binary2(true, BinName, BinValueList) ->
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
print_one_header_binary2(false, BinName, [First | Rest]) ->
    %% Return one "Key: Value" for every element in BinValueList
    This = list_to_binary([BinName, $:, 32, First, 13, 10]),	%% Name: First\r\n
    [This | print_one_header_binary2(false, BinName, Rest)];
print_one_header_binary2(false, _BinName, []) ->
    [].

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
get_tag([String]) ->
    Index = string:chr(String, $>),
    ParamStr = string:substr(String, Index + 1),
    ParamList = string:tokens(ParamStr, ";"),
    ParamDict = param_to_dict(ParamList),
    case dict:find("tag", ParamDict) of
	error ->
	    none;
	{ok, Tag} ->
	    Tag
    end.

%%--------------------------------------------------------------------
%% Function: dialogid(Header)
%%           Header = keylist record()
%% Descrip.: Get what in RFC3261 is referred to as a dialog ID. This
%%           will be the same for all requests in a dialog. Note
%%           though that the ToTag might be 'none' and later get set.
%% Returns : {CallID, FromTag, ToTag}
%%           the contents of "Call-ID", "From" and "To"
%%--------------------------------------------------------------------
dialogid(Header) when is_record(Header, keylist) ->
    CallID = sipheader:callid(Header),
    FromTag = sipheader:get_tag(keylist:fetch('from', Header)),
    ToTag = sipheader:get_tag(keylist:fetch('to', Header)),
    {CallID, FromTag, ToTag}.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns : {Proto, Host, Port}
%%--------------------------------------------------------------------
via_sentby(Via) when is_record(Via, via) ->
    {Via#via.proto, Via#via.host, Via#via.port}.

%%--------------------------------------------------------------------
%% Function: get_server_transaction_id(Request)
%%           Request = request record()
%% Descrip.: Turn a request into a transaction id, that can be stored
%%           in our transaction state database together with a
%%           reference to the process handling this request (server
%%           transaction handler) if this is a new transaction, or
%%           looked up in the database to find an existing handler if
%%           this is a resend of the same request or an ACK to a
%%           non-2xx response to INVITE. This is specified in RFC3261
%%           #17.2.3 (Matching Requests to Server Transactions).
%% Returns : Id = term() | is_2543_ack | error
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
%% Function: get_client_transaction_id(Response)
%%           Response = response record()
%% Descrip.: When we receive a response, we use this function to get
%%           an Id which we look up in our transaction state database
%%           to see if we have a client transaction handler that
%%           should get this response. This is specified in RFC3261
%%           #17.1.3 (Matching Responses to Client Transactions).
%% Returns : Id = term() | error
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
%% Function: get_server_transaction_ack_id_2543(Request)
%%           Request = request record()
%% Descrip.: When we receive an ACK that has no RFC3261 Via branch
%%           parameter, we use this function to get an Id that we then
%%           look up in our transaction state database to try and find
%%           an existing server transaction that this ACK should be
%%           delivered to. This is specified in RFC3261 #17.2.3
%%           (Matching Requests to Server Transactions).
%% Returns : Id = term() | error
%% Note    : When using this function, you have to make sure the
%%           To-tag of this ACK matches the To-tag of the response you
%%           think this might be the ACK for!
%%
%% Note    : RFC3261 #17.2.3 relevant text :
%%           The ACK request matches a transaction if the Request-
%%           URI, From tag, Call-ID, CSeq number (not the method),
%%	     and top Via header field match those of the INVITE
%%           request which created the transaction, and the To tag of
%%           the ACK matches the To tag of the response sent by the
%%           server transaction.
%%
%% Note    : We are supposed to do the comparison of for example, the
%%           URI, according to the matching rules for URIs but that
%%           would require us to do a full table scan for every ACK.
%%           XXX perhaps we should divide the Id into two parts - one
%%           that is byte-by-byte and used as table index, and another
%%           part for elements that require more exhaustive matching.
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
%% Function: get_via_branch(TopVia)
%% Descrip.: Get the branch from the TopVia parameters, and then
%%           remove any Yxa loop cookie from it. This function should
%%           typically only be called on a Via that matches this proxy
%%           so that should be ok - we won't be altering anyone elses
%%           branches.
%% Returns : Branch = string() | none
%%--------------------------------------------------------------------
get_via_branch(TopVia) when is_record(TopVia, via) ->
    case get_via_branch_full(TopVia) of
	"z9hG4bK-yxa-" ++ RestOfBranch ->
	    remove_loop_cookie("z9hG4bK-yxa-" ++ RestOfBranch);
	L when is_list(L) ->
	    L;
	none ->
	    none
    end.

%%--------------------------------------------------------------------
%% Function: remove_loop_cookie(Branch)
%% Descrip.:
%% Returns : Branch | NewBranch = string()
%%--------------------------------------------------------------------
remove_loop_cookie(Branch) ->
    case Branch of
	"z9hG4bK-yxa-" ++ RestOfBranch ->
	    case sipserver:get_env(detect_loops, true) of
		true ->
		    case string:rstr(RestOfBranch, "-o") of
			0 ->
			    Branch;
			Index when is_integer(Index) ->
			    %% Return branch without Yxa loop cookie
			    "z9hG4bK-yxa-" ++ string:substr(RestOfBranch, 1, Index - 1)
		    end;
		_ ->
		    Branch
	    end;
        _ ->
	    Branch
    end.

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns : Branch = string() | none
%%--------------------------------------------------------------------
get_via_branch_full(Via) when is_record(Via, via) ->
    case dict:find("branch", sipheader:param_to_dict(Via#via.param)) of
	error ->
	    none;
	{ok, Branch} ->
	    Branch
    end;
get_via_branch_full(_) ->
    none.

%%--------------------------------------------------------------------
%% Function: via_is_equal/2
%% Descrip.: Compare two Via records according to the rules in
%%           RFC3261 20.42 (Via)
%% Returns : true  |
%%           false
%%--------------------------------------------------------------------
via_is_equal(A, B) when is_record(A, via), is_record(B, via) ->
    via_is_equal(A, B, [proto, host, port, param]).


%%--------------------------------------------------------------------
%% Function: via_is_equal/3
%% Descrip.: Compare one or more parts of two Via records according
%%           to RFC3261 20.42.
%% Returns : true  |
%%           false
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
via_is_equal(A, B, [parameters | T]) ->
    Alist = lists:keysort(1, dict:to_list(A#via.param)),
    Blist = lists:keysort(1, dict:to_list(B#via.param)),
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


%%====================================================================
%% Behaviour functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: name_header(String)
%%           String = string(), a sip URI string or sip URI inside "<"
%%           and ">" quotes, preceded by a displayname
%% Descrip.: used to parse the contents in a To, From or Contact header
%% Returns : {Displayname, URI}
%%           Displayname = none | string()
%%           URI = sipurl record()
%%--------------------------------------------------------------------
name_header(String) ->
    Index1 = string:rchr(String, $<),
    case Index1 of
	0 ->
	    % No "<", just an URI? XXX Check that it is parseable?
	    URI = sipurl:parse(String),
	    {none, URI};
	_ ->
	    Index2 = string:rchr(String, $>),
	    URL = string:substr(String, Index1 + 1, Index2 - Index1 - 1),
	    URI = sipurl:parse(URL),
	    Displayname = parse_displayname(string:substr(String, 1, Index1 - 1)),
	    {Displayname, URI}
    end.

parse_displayname(String) ->
    LeftQuoteIndex = string:chr(String, $\"),
    case LeftQuoteIndex of
	0 ->
	    empty_displayname(string:strip(String));
	_ ->
	    TempString = string:substr(String, LeftQuoteIndex + 1),
	    RightQuoteIndex = string:chr(TempString, $\"),
	    empty_displayname(string:substr(TempString, 1, RightQuoteIndex - 1))
    end.

empty_displayname([]) ->
    none;
empty_displayname(Name) ->
    Name.

%%--------------------------------------------------------------------
%% Function: guarded_get_server_transaction_id_3261(Method, TopVia)
%%           Method = list()
%%           TopVia = via record()
%% Descrip.: Part of guarded_get_server_transaction_id(), called when
%%           the top Via header is found to contain an RFC3261 branch
%%           parameter. This is the straight forward case.
%% Returns : Id = term()
%%--------------------------------------------------------------------
guarded_get_server_transaction_id_3261(Method, TopVia) when is_list(Method), is_record(TopVia, via) ->
    Branch = get_via_branch_full(TopVia),
    SentBy = via_sentby(TopVia),
    {Branch, SentBy, Method}.

%%--------------------------------------------------------------------
%% Function: guarded_get_server_transaction_id_2543(Request, TopVia)
%%           Request = request record()
%%           TopVia = via record()
%% Descrip.: Part of guarded_get_server_transaction_id(), called when
%%           the top Via header does NOT contain an RFC3261 branch
%%           parameter. Creates an Id based on RFC3261 #17.2.3
%%           (Matching Requests to Server Transactions).
%% Returns : Id = term() | is_2543_ack
%% Note    : We could very well do the 2543 ack-id computation here,
%%           but since the caller must do the To-tag verification for
%%           such requests we just return is_2543_ack here to make
%%           sure the caller does not miss this.
%% Note    : RFC3261 #17.2.3 has different text for ACK (entirely
%%           separate, see previous note), INVITE and "all other
%%           methods". However, it seems to me that the instructions
%%           for INVITE and "all other" are the same :
%%
%%           The INVITE request matches a transaction if the
%%           Request-URI, To tag, From tag, Call-ID, CSeq, and top Via
%%           header field match those of the INVITE request which
%%           created the transaction.
%%           ...
%%           For all other request methods, a request is matched to a
%%           transaction if the Request-URI, To tag, From tag,
%%           Call-ID, CSeq (including the method), and top Via header
%%           field match those of the request that created the
%%           transaction.
%%
%%           Therefor, we just have non-ACK below.
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
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->

    %% test comma/1
    %%--------------------------------------------------------------------
    %% test empty string
    io:format("test: comma/1 - 1~n"),
    [""] = comma(""),

    %% test string without comma
    io:format("test: comma/1 - 2~n"),
    ["foobar"] = comma("foobar"),

    %% test with single comma
    io:format("test: comma/1 - 3~n"),
    ["foo","bar"] = comma("foo,bar"),

    %% test with several comma
    io:format("test: comma/1 - 4~n"),
    ["fo","ob","ar"] = comma("fo,ob,ar"),

    %% test with quotes inside string
    io:format("test: comma/1 - 5~n"),
    ["foobar: \"this is a string\""] = comma("foobar: \"this is a string\""),

    %% test with commas inside quoted string part
    io:format("test: comma/1 - 6~n"),
    ["foobar: \"this, is, a, string,\""] = comma("foobar: \"this, is, a, string,\""),

    %% test with commas outside quoutes
    io:format("test: comma/1 - 7~n"),
    ["foobar:", "\",this is a string\""] = comma("foobar:, \",this is a string\""),

    %% test with commas outside and inside quoutes
    io:format("test: comma/1 - 8~n"),
    ["foobar:", "\",this is a ,string\"", "foo"] = comma("foobar:, \",this is a ,string\",foo"),

    %% trailing comma
    io:format("test: comma/1 - 9~n"),
    ["fo","ob","ar",""] = comma("fo,ob,ar,"),

    %% preceeding comma
    io:format("test: comma/1 - 10~n"),
    ["","fo","ob","ar"] = comma(",fo,ob,ar"),

    %% test commas inside <>!


    %% test via/1
    %%--------------------------------------------------------------------
    io:format("test: via/1 - 1~n"),
    [#via{proto="SIP/2.0/TLS", host="192.0.2.123", port=none, param=[]}] =
	via(["SIP/2.0/TLS 192.0.2.123"]),

    io:format("test: via/1 - 2~n"),
    [#via{proto="SIP/2.0/TLS", host="192.0.2.123", port=1234, param=[]}] =
	via(["SIP/2.0/TLS 192.0.2.123:1234"]),

    io:format("test: via/1 - 3~n"),
    [#via{proto="SIP/2.0/TLS", host="[2001:6b0:5:987::5060]", port=5060, param=[]}] =
	via(["SIP/2.0/TLS [2001:6b0:5:987::5060]:5060"]),

    io:format("test: via/1 - 4~n"),
    [#via{proto="SIP/2.0/TLS", host="[2001:6b0:5:987::5060]", port=none, param=[]}] =
	via(["SIP/2.0/TLS [2001:6b0:5:987::5060]"]),

    io:format("test: via/1 - 5~n"),
    [#via{proto="SIP/2.0/TLS", host="[2001:6b0:5:987::5060]", port=none, param=[]}] =
	via(["SIP/2.0/TLS 2001:6b0:5:987::5060"]),

    io:format("test: via/1 - 6~n"),
    [#via{proto="SIP/2.0/TLS", host="[2001:6b0:5:987::5060]", port=none, param=[]},
     #via{proto="SIP/2.0/TCP", host="phone.example.org", port=none, param=["received=192.0.2.123"]}] =
	via(["SIP/2.0/TLS 2001:6b0:5:987::5060", "SIP/2.0/TCP phone.example.org;received=192.0.2.123"]),

    io:format("test: via/1 - 7~n"),
    %% test unparsable via - fail in tokenizing
    {'EXIT', _} = (catch via(["SIP/2.0/TLS2001:6b0:5:987::5060"])),

    io:format("test: via/1 - 8~n"),
    %% test unparsable via - fail inside sipparse_util:parse_hostport() since there is an X in the address
    {'EXIT', _} = (catch via(["SIP/2.0/TLS 2001:6b0:X:987::5060"])),

    io:format("test: via/1 - 9~n"),
    %% test unparsable via - fail inside sipparse_util:parse_hostport() since there is a 500 in the address
    {'EXIT', _} = (catch via(["SIP/2.0/TLS 192.0.2.500:5060"])),
    
    
    %% test get_server_transaction_id/1
    %%--------------------------------------------------------------------

    io:format("test: get_server_transaction_id/1 - 1.1~n"),
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

    io:format("test: get_server_transaction_id/1 - 1.2~n"),
    %% check result
    {"z9hG4bK-really-unique", {"SIP/2.0/TLS", "sip.example.org", 5061}, "INVITE"} = Invite1Id,
    
    io:format("test: get_server_transaction_id/1 - 2~n"),
    %% make an ACK for an imagined 3xx-6xx response with to-tag "t-123"
    AckInvite1Header_1 = keylist:set("To", ["<sip:bob@example.org>;tag=t-123"], InviteHeader1),
    AckInvite1Header1  = keylist:set("CSeq", ["2 ACK"], AckInvite1Header_1),
    AckInvite1 = #request{method="ACK", uri=sipurl:parse("sip:alice@example.org"),
			  header=AckInvite1Header1, body = <<>>},

    AckInvite1Id = get_server_transaction_id(AckInvite1),

    io:format("test: get_server_transaction_id/1 - 3~n"),
    %% Test that the INVITE id matches the ACK id
    Invite1Id = AckInvite1Id,

    io:format("test: get_server_transaction_id/1 - 4.1~n"),
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

    io:format("test: get_server_transaction_id/1 - 4.2~n"),
    %% check result
    {#sipurl{proto="sip", user="alice", pass=none, host="example.org", port=none, param_pairs={url_param,[]}},
     none,
     "f-abc",
     "3c26722ce234@192.0.2.111", {"2", "INVITE"},
     {via, "SIP/2.0/TLS", "sip.example.org", 5061, ["branch=not-really-unique"]}
    } = Invite2543_1Id,

    io:format("test: get_server_transaction_id/1 - 5.1~n"),
    %% for RFC2543 INVITE, we must also get the ACK-id to match future ACKs with this INVITE
    Invite2543_1AckId = get_server_transaction_ack_id_2543(Invite2543_1),

    io:format("test: get_server_transaction_id/1 - 5.2~n"),
    %% check result
    {#sipurl{proto="sip", user="alice", pass=none, host="example.org", port=none, param_pairs={url_param,[]}},
     "f-abc",
     "3c26722ce234@192.0.2.111",
     {"2","INVITE"},
     {via,"SIP/2.0/TLS","sip.example.org",5061,[]}
    } = Invite2543_1AckId,
    
    io:format("test: get_server_transaction_id/1 - 6~n"),
    %% make an ACK for an imagined 3xx-6xx response with to-tag "t-123", check that 
    %% get_server_transaction_id refuses and tells us it is an 2543 ACK
    AckInvite2543_1Header_1 = keylist:set("To", ["<sip:bob@example.org>;tag=t-123"], Invite2543_1Header),
    AckInvite2543_1Header   = keylist:set("CSeq", ["2 ACK"], AckInvite2543_1Header_1),
    AckInvite2543_1 = #request{method="ACK", uri=sipurl:parse("sip:alice@example.org"),
			       header=AckInvite2543_1Header, body = <<>>},

    is_2543_ack = get_server_transaction_id(AckInvite2543_1),

    io:format("test: get_server_transaction_id/1 - 7~n"),
    %% now get the 2543 ACK id from the ACK
    AckInvite2543_1Id = get_server_transaction_ack_id_2543(AckInvite2543_1),

    io:format("test: get_server_transaction_id/1 - 8~n"),
    %% check that the 2543 ACK id matches the 2543 INVITE id
    AckInvite2543_1Id = Invite2543_1AckId,


    %% test get_client_transaction_id/1
    %%--------------------------------------------------------------------
    io:format("test: get_client_transaction_id/1 - 1~n"),
    Response1 = #response{status=699, reason="foo", header=Invite2543_1Header, body = <<>>},
    {"not-really-unique", "INVITE"} = get_client_transaction_id(Response1),


    %% test contact/1. No full test of parameters - we don't test the actual contact parsing here.
    %%--------------------------------------------------------------------
    ContactHeader1 = keylist:from_list([
					{"From", ["Test <sip:alice@example.org>;tag=f-abc"]},
					{"Contact", ["<sip:bob@example.org>;lr=true"]}
				       ]),
    
    io:format("test: contact/1 - 1~n"),
    %% test using list key
    [#contact{urlstr="sip:bob@example.org"}] = contact(ContactHeader1),



    %% test contact/2. No full test of parameters - we don't test the actual contact parsing here.
    %%--------------------------------------------------------------------
    io:format("test: contact/2 - 1~n"),
    %% test using atom key
    [#contact{urlstr="sip:alice@example.org"}] = contact(ContactHeader1, 'from'),

    io:format("test: contact/2 - 2~n"),
    %% test using list key
    [#contact{urlstr="sip:alice@example.org"}] = contact(ContactHeader1, "From"),


    ok.
