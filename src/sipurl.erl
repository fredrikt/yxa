%% This module handles sipurl (record) related actions:
%% * parsing data to a sipurl record()
%% * formating sipurl record() to a parseable text string
%% * comparing sipurls with url_is_equal/2
%% * creating and modifying sipurl records with new() and set()
%% * accessing sipurl records with get_port/1
%%
%% Note: parsing functions used by parse/1 often assume correct
%% syntaxt and will throw an exception, rather than return 'false'
%% if matching fails - this reduces the amount of error checking
%% needed.
%%
%% Note: there are various possibilities for optimizations:
%% * the regexp in host name checking is rather complex and _may_ be
%%   faster if implemented with custom checking functions.
%% * There are probably various functions and list operations that
%%   do unnecessay work.
%% * Some checks that ensure that a sip url is a proper sip url
%%   (acording to RFC3261 chapter 25), are not needed to parse legal
%%   sip url strings, but only to reject malformed ones. Some of
%%   these checks like looking for multiple ";", ":" and "="could be
%%   removed as their removal will not insert "bad" data into the
%%   sipurl record(), but will instead "fix" bad urls.
%% - I personaly question if this is a good idea (hsten)
%%
%% Note: While the set and new functions, ensure case insensitivity
%% and reject duplicat parameter names - they do not check the
%% validity of their fields, this is only done in parse/1 which is
%% expected to handle external data, which might be faulty.
%%
%% XXX RFC 3261 is somewhat vague about character encoding and if all
%% 8 bit values can be escaped. The non-escaped chars(e.g. A-Z, a-z,
%% 0-9 ...) are encoded as ASCII. The current implementation escapes
%% all 8 bit values that have no non-escaped version in their current
%% context (the field they appear in).
%% RFC 2396, which RFC 3261 referes to, appears to only use the
%% lower (7 bit) ASCII set, the implication (if any) of this is
%% unclear - a proper check of RFC 2396 is needed !
%% XXX a split_fields(Str, SepStr) - that takes a separtor
%% sequence rather than a single char() would be nice
%% - string:substring/2 or regexp:split/2 could be used for this but
%% would most likely be slower than the current solution with
%% split_fields/2 - altough the use of catch for matching "]:" is kind
%% of ugly and cryptic.
%%--------------------------------------------------------------------

-module(sipurl).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 parse/1,
	 print/1,
	 print_hostport/2,
	 url_is_equal/2,
	 url_is_equal/3,
	 parse_url_with_default_protocol/2,

	 unescape_str/1,
	 escape_parameters/1,

	 get_port/1,

	 new/1,

	 set/2,

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

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% RFC 2234 specifies the BNF format (and some rules) used in
%% RFC 3261.
%% The most important conventions in this BNF format are:
%% R1 / R2 / ...   = one of these rules must be applied
%% (R1 / R2 / ...) = () are used to group rules
%% N*M(R) = rule R must be applied at least N times but not more than
%%          M, default N = 0 and M = infinity
%% N(R)   = N*N(R) i.e. rule R must be applied exactly N time
%% *(R)   = 0*inifinity(R) i.e. rule R may be applied any number of
%%          times
%% [R]    = *1(R), i.e. apply this rule 0-1 times
%% "string" = quoted strings are case insensitive
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%% RFC 2234  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z
%% DIGIT          =  %x30-39             ; 0-9
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%% RFC 3261  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% 25.1 Basic Rules
%%
%%    The following rules are used throughout this specification to
%%    describe basic parsing constructs.  The US-ASCII coded character set
%%    is defined by ANSI X3.4-1986.
%%
%% alphanum  =  ALPHA / DIGIT
%%
%%    Several rules are incorporated from RFC 2396 [5] but are updated to
%%    make them compliant with RFC 2234 [10].  These include:
%%
%%       reserved    =  ";" / "/" / "?" / ":" / "@" / "&" / "=" / "+" / "$" / ","
%%       unreserved  =  alphanum / mark
%%       mark        =  "-" / "_" / "." / "!" / "~" / "*" / "'" / "(" / ")"
%%       escaped     =  "%" HEXDIG HEXDIG
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%% RFC 3261  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%
%% SIP-URI          =  "sip:" [ userinfo ] hostport uri-parameters [ headers ]
%% SIPS-URI         =  "sips:" [ userinfo ] hostport uri-parameters [ headers ]
%% userinfo         =  ( user / telephone-subscriber ) [ ":" password ] "@"
%% user             =  1*( unreserved / escaped / user-unreserved )
%% user-unreserved  =  "&" / "=" / "+" / "$" / "," / ";" / "?" / "/"
%% password         =  *( unreserved / escaped /
%%                     "&" / "=" / "+" / "$" / "," )
%% hostport         =  host [ ":" port ]
%% host             =  hostname / IPv4address / IPv6reference
%% hostname         =  *( domainlabel "." ) toplabel [ "." ]
%% domainlabel      =  alphanum / alphanum *( alphanum / "-" ) alphanum
%% toplabel         =  ALPHA / ALPHA *( alphanum / "-" ) alphanum
%%
%% IPv4address    =  1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT "." 1*3DIGIT
%% IPv6reference  =  "[" IPv6address "]"
%% IPv6address    =  hexpart [ ":" IPv4address ]
%% hexpart        =  hexseq / hexseq "::" [ hexseq ] / "::" [ hexseq ]
%% hexseq         =  hex4 *( ":" hex4)
%% hex4           =  1*4HEXDIG
%% port           =  1*DIGIT
%%
%%    The BNF for telephone-subscriber can be found in RFC 2806 [9].  Note,
%%    however, that any characters allowed there that are not allowed in
%%    the user part of the SIP URI MUST be escaped.
%%
%% uri-parameters    =  *( ";" uri-parameter)
%% uri-parameter     =  transport-param / user-param / method-param / ttl-param / maddr-param / lr-param / other-param
%% transport-param   =  "transport=" ( "udp" / "tcp" / "sctp" / "tls" / other-transport)
%% other-transport   =  token
%% user-param        =  "user=" ( "phone" / "ip" / other-user)
%% other-user        =  token
%% method-param      =  "method=" Method
%% ttl-param         =  "ttl=" ttl
%% maddr-param       =  "maddr=" host
%% lr-param          =  "lr"
%% other-param       =  pname [ "=" pvalue ]
%% pname             =  1*paramchar
%% pvalue            =  1*paramchar
%% paramchar         =  param-unreserved / unreserved / escaped
%% param-unreserved  =  "[" / "]" / "/" / ":" / "&" / "+" / "$"
%%
%% headers         =  "?" header *( "&" header )
%% header          =  hname "=" hvalue
%% hname           =  1*( hnv-unreserved / unreserved / escaped )
%% hvalue          =  *( hnv-unreserved / unreserved / escaped )
%% hnv-unreserved  =  "[" / "]" / "/" / "?" / ":" / "+" / "$"
%%
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

%%--------------------------------------------------------------------
%% @spec    (URLStr) -> #sipurl{} | {unparseable, URLStr}
%%
%%            URLStr = string() "a sip url"
%%
%% @doc     parses a sip url of the following format (see RFC 3261
%%          chapter 19.1.1 for more details):
%%          sip:user:password@host:port;uri-parameters?headers
%%          * uri-parameters = any number of "parameter_name=value"
%%          separated by `;' * headers = any number of "hname=hvalue"
%%          separated by `&'
%%          Phone numbers can also be encoded as sip uris (see
%%          chapter 19.1.6). A tel uri:
%%          "tel:+358-555-1234567;postd=pp22" becomes
%%          "sip:+358-555-1234567;postd=pp22@foo.com;user=phone" i.e.
%%          "user" is replaced by "phone-no;tel-paramters", and a
%%          default domain for the entity doing the conversion is
%%          appended.
%%          Note : only the sip and sips protocols are supported Note
%%          : the headers field is currently not supported Note : tel
%%          URIs encoded as sip URIs are currently not supported
%% @end
%%--------------------------------------------------------------------
parse("sip:" ++ RURL) ->
    case parse2("sip", RURL) of
	SipUrl when is_record(SipUrl, sipurl) -> SipUrl;
	unparseable ->
	    {unparseable, "sip:" ++ RURL}
    end;
parse("sips:" ++ RURL) ->
    case parse2("sips", RURL) of
	SipUrl when is_record(SipUrl, sipurl) -> SipUrl;
	unparseable ->
	    {unparseable, "sips:" ++ RURL}
    end;
parse(URLStr) ->
    case string:chr(URLStr, $:) of
	N when is_integer(N), N > 0 ->
	    In = string:substr(URLStr, 1, N - 1),
	    case httpd_util:to_lower(In) of
		LC when LC == "sip"; LC == "sips" ->
		    RURL = string:substr(URLStr, N + 1),
		    case parse2(LC, RURL) of
			SipUrl when is_record(SipUrl, sipurl) -> SipUrl;
			unparseable ->
			    {unparseable, URLStr}
		    end;
		_ ->
		    {unparseable, URLStr}
	    end;
	_ ->
	    {unparseable, URLStr}
    end.

%% part of parse/1
parse2(Proto, RURL) ->
    case catch parse_url(Proto, RURL) of
	SipUrl when is_record(SipUrl, sipurl) -> SipUrl;
	_Error ->
	    unparseable
    end.

%% URL = URL input without the proto: prefix ("sip:" that is)
%% Returns : sipurl record() | throw() (if parse failed)
parse_url(Proto, URL) ->
    {User, Password, HostportParametersHeaders} =
	case sipparse_util:split_fields(URL, $@) of
	    {Userinfo, HostportParametersHeaderRest} ->
		{Usr, Pass} = parse_userinfo(Userinfo),
		{Usr, Pass, HostportParametersHeaderRest};
	    %% no "@" means no user:password fields
	    {HostportParametersHeaderRest} ->
		{none, none, HostportParametersHeaderRest}
	end,

    %% first ";" (after "@") occurs in uri-parameters
    {Host, Port, ParametersHeaders} =
	case sipparse_util:split_fields(HostportParametersHeaders, $;) of
	    {HostPortStr, ParametersHeadersStr} ->
		{Host2, Port2} = sipparse_util:parse_hostport(HostPortStr),
		{Host2, Port2, ParametersHeadersStr};
	    {HostPortStr} ->
		{Host2, Port2} = sipparse_util:parse_hostport(HostPortStr),
		{Host2,Port2, []}
	end,

    {Parameters, Parameterlist, _Headerslist} =
	case ParametersHeaders of
	    [] ->
		{[], [], []};
	    _ ->
		%% "?" (which isn't part of uri-parameters) acts as stop marker for uri-parameters
		%% {Parameters, Headers} return, in split_fields/2 currently result in a throw()
		%% as we don't support headers
		{Parameters2} = sipparse_util:split_fields(ParametersHeaders, $?),
		ParamList = string:tokens(Parameters2, ";"),
		{Parameters2, ParamList, []}
	end,

    case is_bnf_compliant_url(User, Password, Host, Port, Parameters) of
	true ->
	    new([{proto, Proto}, {user, User}, {pass, Password}, {host, Host},
		 {port, Port}, {param, Parameterlist}]);
	false ->
	    unparseable
    end.

%% The parse functions are somewhat lax compared to the BNF specification so this
%% function is used to check that fields only contain the expected characters and
%% separators.
%% Host and port fields are checked by parse_hostport/1 so there is no need to
%% verify these values separatly
is_bnf_compliant_url(User, Pass, _Host, _Port, Parameters) ->
    is_user(User) andalso
 	is_password(Pass) andalso
     	is_parameters(Parameters).

parse_userinfo(Userinfo) ->
    %% ":" can't be used in the user or password parts, so it
    %% acts as a separator for the two fields
    case sipparse_util:split_fields(Userinfo, $:) of
	{User, Pass} ->
	    {User, Pass};
	{User} ->
	    {User, none}
    end.

%%--------------------------------------------------------------------
%% @spec    (Str) -> none | string()
%%
%%            Str = none | string()
%%
%% @doc     replaces %HH escape codes in a string with its value
%% @end
%%--------------------------------------------------------------------
unescape_str(none) ->
    none;
unescape_str([]) ->
    [];
%% hex value found convert to integer()
unescape_str([$%, H1, H2 | RStr]) ->
	      [hex:to_int([H1, H2]) | unescape_str(RStr)];
	      unescape_str([Char | RStr]) ->
		     [Char | unescape_str(RStr)].

%%--------------------------------------------------------------------
%% @spec    (URL) -> string()
%%
%%            URL = #sipurl{}
%%
%% @doc     create parseable sip url string Note : user, password,
%%          uri-parameters and headers can use char codes that need
%%          to be escaped using the %HH (H = 0-F) notation
%% @end
%%--------------------------------------------------------------------
%% XXX this will break if if the #sipurl.proto field isn't a string()
print(URL) when record(URL, sipurl) ->
    URL#sipurl.proto ++ ":" ++
	print_userinfo(URL#sipurl.user, URL#sipurl.pass) ++
	print_hostport(URL#sipurl.host, get_port(URL)) ++
	print_parameters(URL#sipurl.param_pairs).

print_userinfo(none, none) ->
    "";
print_userinfo(User, none) ->
    escape_user(User) ++ "@";
print_userinfo(User, Pass) ->
    escape_user(User) ++ ":" ++ escape_password(Pass) ++ "@".

%%--------------------------------------------------------------------
%% @spec    (Host, Port) -> string()
%%
%%            Host = string()
%%            Port = integer() | none "(if sipurl contained no port value)"
%%
%% @doc     return a "host:port" string
%% @end
%%--------------------------------------------------------------------
print_hostport(Host, none) ->
    Host;
print_hostport(Host, Port) when is_integer(Port) ->
    lists:concat([Host, ":", Port]).


print_parameters(URLParams) when record(URLParams, url_param) ->
    url_param:to_string(URLParams).


escape_user(Str) ->
    IsNonEscapeChar = fun(Char) ->
			      is_unreserved(Char) orelse is_user_unreserved(Char)
		      end,
    escape_str(Str, IsNonEscapeChar).

escape_password(Str) ->
    IsNonEscapeChar = fun(Char) ->
			      is_password_without_escape(Char)
		      end,
    escape_str(Str, IsNonEscapeChar).

%%--------------------------------------------------------------------
%% @spec    (Str) -> string()
%%
%%            Str = string()
%%
%% @doc     Do SIP URL parameter escaping on Str.
%% @end
%%--------------------------------------------------------------------
escape_parameters(Str) ->
    IsNonEscapeChar = fun(Char) ->
			      is_param_unreserved(Char) or is_unreserved(Char)
		      end,
    escape_str(Str, IsNonEscapeChar).

%% create string with escape char for chars where IsNonEscapeChar(Char) == false
escape_str(Str,IsNonEscapeChar) ->
    F = fun(Char) ->
		case IsNonEscapeChar(Char) of
		    true -> Char;
		    false -> escape(Char)
		end
	end,
    NewStr = lists:map(F, Str),
    %% flatten as escape codes are themselves strings
    lists:flatten(NewStr).

%% generate a escape hex encoding for a char
escape(Char) ->
    [37 | hex:to_hex_string(Char)].	%% 37 is '%'

%%--------------------------------------------------------------------
%% @spec    (A, B, Compare) -> true | false
%%
%%            A       = #sipurl{}
%%            B       = #sipurl{}
%%            Compare = [proto | user | pass | host | port | param]
%%
%% @doc     Return 'true' if A = B according to RFC 3261 chapter
%%          19.1.4, in the aspects you list. Use url_is_equal/2 to do
%%          a full compare. Note : assume that A and B are properly
%%          parsed sipurl records with unencoded chars i.e. no hex
%%          encoded chars
%% @end
%%--------------------------------------------------------------------
url_is_equal(A, B, []) when is_record(A, sipurl), is_record(B, sipurl) ->
    true;
%% proto
url_is_equal(#sipurl{proto=Proto}=A, #sipurl{proto=Proto}=B, [proto | T]) ->
    url_is_equal(A, B, T);
url_is_equal(A, B, [proto | _T]) when is_record(A, sipurl), is_record(B, sipurl) ->
    false;
%% user
url_is_equal(#sipurl{user=User}=A, #sipurl{user=User}=B, [user | T]) ->
    url_is_equal(A, B, T);
url_is_equal(A, B, [user | _T]) when is_record(A, sipurl), is_record(B, sipurl) ->
    false;
%% pass
url_is_equal(#sipurl{user=User}=A, #sipurl{user=User}=B, [pass | T]) ->
    url_is_equal(A, B, T);
url_is_equal(A, B, [pass | _T]) when is_record(A, sipurl), is_record(B, sipurl) ->
    false;
%% host
url_is_equal(#sipurl{host=Host}=A, #sipurl{host=Host}=B, [host | T]) ->
    url_is_equal(A, B, T);
url_is_equal(A, B, [host | _T]) when is_record(A, sipurl), is_record(B, sipurl) ->
    false;
%% port
url_is_equal(A, B, [port | T]) when is_record(A, sipurl), is_record(B, sipurl) ->
    Bp = get_port(B),
    case get_port(A) of
	Bp ->
	    url_is_equal(A, B, T);
	_ ->
	    false
    end;
url_is_equal(A, B, [param | T]) when is_record(A, sipurl), is_record(B, sipurl) ->
    case url_is_equal_uri_parameter(A, B) of
	true ->
	    url_is_equal(A, B, T);
	false ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (A, B) -> true | false
%%
%%            A = #sipurl{}
%%            B = #sipurl{}
%%
%% @doc     return 'true' if A = B according to RFC 3261 chapter
%%          19.1.4 Note : assume that A and B are properly parsed
%%          sipurl records with unencoded chars i.e. no hex encoded
%%          chars
%% @end
%%--------------------------------------------------------------------
url_is_equal(A, B) when record(A, sipurl), record(B, sipurl) ->
    %% RFC 3261  - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% o  A SIP and SIPS URI are never equivalent.
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    case A#sipurl.proto == B#sipurl.proto of
	false ->
	    false;
	true ->
	    url_is_equal_user_pass_host_port(A, B)
    end.

url_is_equal_user_pass_host_port(A, B) ->
    %% RFC 3261  - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% o  Comparison of the userinfo of SIP and SIPS URIs is case-
    %%    sensitive.  This includes userinfo containing passwords or
    %%    formatted as telephone-subscribers.  Comparison of all other
    %%    components of the URI is case-insensitive unless explicitly
    %%    defined otherwise.
    %% o  The ordering of parameters and header fields is not significant
    %%    in comparing SIP and SIPS URIs.
    %% o  Characters other than those in the "reserved" set (see RFC 2396
    %%    [5]) are equivalent to their ""%" HEX HEX" encoding.
    %% o  An IP address that is the result of a DNS lookup of a host name
    %%    does not match that host name.
    %% o  For two URIs to be equal, the user, password, host, and port
    %%    components must match.
    %%
    %%    A URI omitting the user component will not match a URI that
    %%    includes one.  A URI omitting the password component will not
    %%    match a URI that includes one.
    %%
    %%    A URI omitting any component with a default value will not
    %%    match a URI explicitly containing that component with its
    %%    default value.  For instance, a URI omitting the optional port
    %%    component will not match a URI explicitly declaring port 5060.
    %%    The same is true for the transport-parameter, ttl-parameter,
    %%    user-parameter, and method components.
    %%
    %%      Defining sip:user@host to not be equivalent to
    %%      sip:user@host:5060 is a change from RFC 2543.  When deriving
    %%      addresses from URIs, equivalent addresses are expected from
    %%      equivalent URIs.  The URI sip:user@host:5060 will always
    %%      resolve to port 5060.  The URI sip:user@host may resolve to
    %%      other ports through the DNS SRV mechanisms detailed in [4].
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% Undefined fields are handled as shown below:
    %%
    %% A#sipurl.x = B#sipurl.x
    %% xxx          xxx        -> true
    %% none         none       -> true
    %% xxx          yyy        -> false
    %% none         xxx        -> false
    %% xxx          none       -> false

    case
	(A#sipurl.user == B#sipurl.user) andalso
	(A#sipurl.pass == B#sipurl.pass) andalso
	(A#sipurl.host == B#sipurl.host) andalso
	(get_port(A) == get_port(B)) of
	false ->
	    false;
	true ->
	    url_is_equal_uri_parameter(A, B)
    end.

url_is_equal_uri_parameter(A, B) ->
    %% RFC 3261  - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% o  URI uri-parameter components are compared as follows:
    %%    -  Any uri-parameter appearing in both URIs must match.
    %%    -  A user, ttl, or method uri-parameter appearing in only one
    %%       URI never matches, even if it contains the default value.
    %%    -  A URI that includes an maddr parameter will not match a URI
    %%       that contains no maddr parameter.
    %%    -  All other uri-parameters appearing in only one URI are
    %%       ignored when comparing the URIs.
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    LA = url_param:to_list(A#sipurl.param_pairs),
    LB = url_param:to_list(B#sipurl.param_pairs),

    %% set vs ordset:
    %% ordsets is and returns a sorted list, sets does _NOT_.
    %% ordsets should be better for union, subtract and
    %% intersection functions and as we do few insertions (only
    %% lists:usort/1 is run on the input to ordsets:from_list/1) there
    %% should be litle need for sets.
    SetA = ordsets:from_list(LA),
    SetB = ordsets:from_list(LB),

    Intersection = ordsets:intersection(SetA, SetB),
    %% Union contains all {Name,Value} and {Name, none} entries that match
    Union = ordsets:union(SetA,SetB),
    %% all non duplicat {Name,Val}, {Name, none} parameters
    Nonshared = ordsets:subtract(Union,Intersection),

    NonsharedList = ordsets:to_list(Nonshared),

    %% check if transport, user, method, maddr and lr, only ocure in one
    %% of the urls
    OnlyOne = only_one_entry("transport", LA, LB) orelse
	only_one_entry("user", LA, LB) orelse
	only_one_entry("ttl", LA, LB) orelse
	only_one_entry("method", LA, LB) orelse
	only_one_entry("maddr", LA, LB),

    %% check if remaining parameters that appear unique, truely are
    %% so - there may be entries that have the same name but different values
    Mismatch = param_mismatch(NonsharedList),

    case OnlyOne of
	true ->
	    %% transport, user, method, maddr and lr
	    %% in only one url -> never match
	    false;
	false ->
	    case Mismatch of
		%% parameters that occure in both urls must match
		%% i.e. two parameters with different values
		%% result in a mismatch
		%% unique parameters (in only one url) don't result
		%% in mismatches
		true ->
		    false;
		false ->
		    url_is_equal_header(A, B)
	    end
    end.

%% 'true' if only one ParamName field among the URIs
only_one_entry(ParaName, AL, BL) ->
    R1 = lists:keysearch(ParaName, 1, AL),
    R2 = lists:keysearch(ParaName, 1, BL),
    case {R1,R2} of
	{false,false} -> false;
	{false,_} -> true;
	{_,false} -> true;
	_ -> false
    end.

%% input   : sorted list()
%% returns : true if two parameters with the same name don't have the same value
%% note    : list() from ordsets:to_list/1 used is sorted
param_mismatch([]) ->
    false;
param_mismatch([_E]) ->
    false;
param_mismatch([{Name, V1}, {Name, V2} | _R]) when V1 /= V2 ->
    true;
param_mismatch([_E | R]) ->
    param_mismatch(R).

url_is_equal_header(_A, _B) ->
    %% RFC 3261  - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %%  o  URI header components are never ignored.  Any present header
    %%     component MUST be present in both URIs and match for the URIs
    %%     to match.  The matching rules are defined for each header field
    %%     in Section 20.
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    %% XXX we currently don't support headers
    true.

%%--------------------------------------------------------------------
%% In some places, we allow lookups to result in URL strings without
%% protocol. First try to parse them as-is, and if that does not work
%% then make sure there is no protocol specified that we apparently
%% do not handle, and if not then prepend them with Proto: and try again.
%%--------------------------------------------------------------------
%% XXX sip:sip:username@su.se, without protocol (sip:)
%% and would be parsed as user = username, pass = none instead of user = sip and pass = username.
%% I can't see any good way to solve this (hsten)

%%--------------------------------------------------------------------
%% @spec    (Proto, URLstr) ->
%%            URL | error
%%
%%            Proto  = string()
%%            URLstr = string()
%%
%%            URL = #sipurl{}
%%
%% @doc     Try to parse URLstr (using @{link parse/1}), and if that
%%          fails then try to parse Proto + UrlStr.
%% @end
%%--------------------------------------------------------------------
parse_url_with_default_protocol(Proto, URLstr) ->
    case sipurl:parse(URLstr) of
	URL1 when record(URL1, sipurl) ->
	    URL1;
	_ ->
	    case sipurl:parse(Proto ++ ":" ++ URLstr) of
		URL2 when record(URL2, sipurl) ->
		    URL2;
		_ ->
		    error
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Sipurl) -> none | integer()
%%
%%            Sipurl = #sipurl{}
%%
%% @doc     return the port value in a consistent manner
%% @end
%%--------------------------------------------------------------------
get_port(Sipurl) when is_record(Sipurl, sipurl) ->
    case Sipurl#sipurl.port of
	I when is_integer(I) ->
	    I;
	none ->
	    none;
	_ ->
	    erlang:fault("port in URL was not list or 'none'", [Sipurl])
    end.


%%--------------------------------------------------------------------
%% @spec    (AttrList) -> #sipurl{}
%%
%%            AttrList = [{Id, Value}]
%%            Id       = proto | user | pass | host | port | param
%%            Value    = term() "context specific - see set() below"
%%
%% @doc     Create a sipurl record(). Value is context specific, see
%%          @{link set/2} below. Note : sipurl record() is defined in
%%          siprecords.hrl XXX headers are currently no supported
%% @end
%%--------------------------------------------------------------------
new(AttrList) ->
    URLtemplate = #sipurl{proto="sip", user=none, pass=none, port=none,
			  param_pairs=url_param:to_norm([])
			 },
    set(AttrList, URLtemplate).

%%--------------------------------------------------------------------
%% @spec    (AttrList, URL) -> #sipurl{}
%%
%%            AttrList = [{Id, Value}]
%%            Id       = proto | user | pass | host | port | param
%%            Value    = term()
%%
%% @doc     Update one or more attributes of a sipurl record(). Value
%%          is context specific, depending on Id : ``` proto =
%%          string(), may be of any case, is stored in a case
%%          insensitive manner, may be = "sip" | "tel" (not yet
%%          supported) | the protocol to use user = string(), case
%%          sensitive user name pass = string(), case sensitive
%%          password host = string(), usually a domain name, will be
%%          stored in a case insensitive manner port = integer()
%%          param = list() of "name=value" string() | url_param
%%          record() ''' Note : sipurl record() is defined in
%%          siprecords.hrl XXX headers are currently no supported
%% @end
%%--------------------------------------------------------------------

%% PROTO
set([{proto, Val} | T], URL) when is_record(URL, sipurl), Val == "sip"; Val == "sips" ->
    set(T, URL#sipurl{proto=Val});
set([{proto, Val} | T], URL) when is_record(URL, sipurl), is_list(Val) ->
    case httpd_util:to_lower(Val) of
	L when L == "sip"; L == "sips" ->
	    set(T, URL#sipurl{proto=L});
	_ ->
	    erlang:fault("sipurl:set/2 with non-sip or sips URI", [[{proto, Val} | T], URL])
    end;

%% USER
set([{user, Val} | T], URL) when is_record(URL, sipurl), is_list(Val); Val == none ->
    Fixed = unescape_str(Val),
    set(T, URL#sipurl{user=Fixed});

%% PASS
set([{pass, Val} | T], URL) when is_record(URL, sipurl), is_list(Val); Val == none ->
    Fixed = unescape_str(Val),
    set(T, URL#sipurl{pass=Fixed});

%% HOST
set([{host, Val} | T], URL) when is_record(URL, sipurl), is_list(Val) ->
    %% Lowercase and remove trailing dots
    LC = httpd_util:to_lower(Val),
    Hostname = string:strip(LC, right, $.),
    set(T, URL#sipurl{host=Hostname});

%% PORT
set([{port, Val} | T], URL) when is_record(URL, sipurl), is_integer(Val); Val == none ->
    set(T, URL#sipurl{port=Val});

%% PARAM
set([{param, Val} | T], URL) when is_record(URL, sipurl), is_record(Val, url_param); is_list(Val) ->
    SetParamPairs =
	if
	    is_record(Val, url_param) ->
		%% Param already in a normalized form
		Val;
	    is_list(Val) ->
		%% store param and param_pairs in a normalized form
		url_param:to_norm(Val)
	end,
    set(T, URL#sipurl{param_pairs=SetParamPairs});

%% Finished
set([], URL) when is_record(URL, sipurl) ->
    URL.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    %% unescape_str/1
    %%--------------------------------------------------------------------
    %% test unescaped string
    autotest:mark(?LINE, "unescape_str/1 - 1"),
    Str1 = "hello",
    "hello" = unescape_str(Str1),

    %% test escaped string, with all values as hex
    autotest:mark(?LINE, "unescape_str/1 - 2"),
    Str2 = "%68%65%6c%6c%6f",
    "hello" = unescape_str(Str2),

    %% test escaped string, with first char as hex
    autotest:mark(?LINE, "unescape_str/1 - 3"),
    Str3 = "%68ello",
    "hello" = unescape_str(Str3),

    %% test escaped string, with last char as hex
    autotest:mark(?LINE, "unescape_str/1 - 4"),
    Str4 = "hell%6f",
    "hello" = unescape_str(Str4),

    %% test escaped string, with middel char as hex
    autotest:mark(?LINE, "unescape_str/1 - 5"),
    Str5 = "he%6clo",
    "hello" = unescape_str(Str5),

    %% test escaped string, with upper case hex values
    autotest:mark(?LINE, "unescape_str/1 - 6"),
    Str6 = "%68%65%6C%6C%6F",
    "hello" = unescape_str(Str6),

    %% escape_parameters/1
    %%--------------------------------------------------------------------
    %% test non-escaped string
    autotest:mark(?LINE, "escape_parameters/1 - 1"),
    "hello" = escape_parameters("hello"),

    %% test string of only escaped chars
    autotest:mark(?LINE, "escape_parameters/1 - 2"),
    "%3D" = escape_parameters("="),

    %% test partialy escaped string
    autotest:mark(?LINE, "escape_parameters/1 - 3"),
    "hel%3Dlo" = escape_parameters("hel=lo"),


    %% is_parameters/1
    %%--------------------------------------------------------------------
    %% test empty parameter string
    autotest:mark(?LINE, "is_parameters/1 - 1"),
    true = is_parameters(""),
    %% single "name" parameter
    autotest:mark(?LINE, "is_parameters/1 - 2"),
    true = is_parameters("bar"),
    %% test case and "name=val" parameter
    autotest:mark(?LINE, "is_parameters/1 - 3"),
    true = is_parameters("foo=BAr"),
    %% test several "name" parameters
    autotest:mark(?LINE, "is_parameters/1 - 4"),
    true = is_parameters("bar;foo"),
    %% test several "name=val" parameters (and case)
    autotest:mark(?LINE, "is_parameters/1 - 5"),
    true = is_parameters("bar=Zopg;vaF=ghjT;BOF=tOp"),
    %% test escape hex codes
    autotest:mark(?LINE, "is_parameters/1 - 6"),
    true = is_parameters("bar=Zo%3Dp%3dg;vaF=ghjT;BOF=tOp"),
    %% check that "name==val" doesn't work
    autotest:mark(?LINE, "is_parameters/1 - 7"),
    false = is_parameters("bar=Zo%3Dp%3dg;vaF==ghjT;BOF=tOp"),
    %% check that "name=val;;name=val" doesn't work
    autotest:mark(?LINE, "is_parameters/1 - 8"),
    false = is_parameters("bar=Zo%3Dp%3dg;vaF=ghjT;;;BOF=tOp"),
    %% check that quoted parameters DON'T work, sipurl BNF does not allow quoted URI parameters
    autotest:mark(?LINE, "is_parameters/1 - 9"),
    false = is_parameters("foo=\"bar\""),

    %% test new
    %%--------------------------------------------------------------------
    SipUrl = #sipurl{proto = "sip", user = "hokan", pass = "foobar", host = "su.it", port = 42,
		     param_pairs = url_param:to_norm(["foo=bar", "baz"])
		     %% header = []
		    },
    %% test new/1
    autotest:mark(?LINE, "new/1 - 1"),
    SipUrl = new([{proto, "sIp"}, {user, "hokan"}, {pass, "foobar"},
		  {host, "Su.iT"}, {port, 42}, {param, ["foO=bar", "bAz"]}]),

    %% test new/1 - no protocol, should default to sip
    autotest:mark(?LINE, "new/1 - 2"),
    SipUrl = new([{user, "hokan"}, {pass, "foobar"}, {host, "Su.iT"},
		  {port, 42}, {param, ["foO=bar", "bAz"]}]),

    SipUrl2 = #sipurl{proto = "sip", user = "hokan", pass = "foobar", host = "su.it", port = 42,
		      param_pairs = url_param:to_norm([])
		      %% header = []
		     },

    %% test new/1
    autotest:mark(?LINE, "new/1 - 3"),
    SipUrl2 = new([{proto, "sIp"}, {user, "hokan"}, {pass, "foobar"}, {host, "Su.iT"}, {port, 42}]),

    %% test url_param record() as argument
    autotest:mark(?LINE, "new/1 - 4"),
    SipUrl = new([{proto,  "sIp"}, {user, "hokan"}, {pass, "foobar"}, {host, "Su.iT"}, {port, 42},
		  {param, url_param:to_norm(["foo=bar", "baz"])}]),

    %% test integer port
    autotest:mark(?LINE, "new/1 - 5"),
    SipUrl2 = new([{proto, "sIp"}, {user, "hokan"}, {pass, "foobar"}, {host, "Su.iT"}, {port, 42}]),

    SipUrl3 = #sipurl{proto = "sip", user = "test", pass = none, host = "su.it", port = none,
		      param_pairs = url_param:to_norm([])
		      %% header = []
		     },

    %% test default attribute values
    autotest:mark(?LINE, "new/1 - 6"),
    SipUrl3 = new([{user, "test"}, {host, "SU.IT"}]),

    %% test stripping of trailing dots on hostname
    autotest:mark(?LINE, "new/1 - 7"),
    SipUrl3 = new([{user, "test"}, {host, "SU.IT.."}]),

    %% test new sips
    %%--------------------------------------------------------------------
    SipUrl4 = #sipurl{proto = "sips", user = "hokan", pass = "foobar", host = "su.it", port = 42,
		      param_pairs = url_param:to_norm(["foo=bar", "baz"])
		      %% header = []
		     },
    autotest:mark(?LINE, "new/1 sips - 1"),
    SipUrl4 = new([{proto, "sips"}, {user, "hokan"}, {pass, "foobar"}, {host, "Su.iT"},
		   {port, 42}, {param, ["foO=bar", "bAz"]}]),

    %% test set
    %%--------------------------------------------------------------------
    SipUrl5 = #sipurl{proto = "sip", user = "hokan", pass = "foobar", host = "su.it", port = 42,
		      param_pairs = url_param:to_norm(["foo=bar", "baz"])
		      %% header = []
		     },
    SipUrl6 = #sipurl{proto = "sips", user = "hoKan", pass = "fOObar", host = "foo.su.it", port = 43,
		      param_pairs = url_param:to_norm(["foo=barbaz", "baz"])
		      %% header = []
		     },
    %% test set
    autotest:mark(?LINE, "set/8 - 1"),
    SipUrl6 = set([{proto, "SIPS"}, {user, "hoKan"}, {pass, "fOObar"}, {host, "foo.su.it"},
		   {port, 43}, {param, ["foo=BarBaz", "baz"]}], SipUrl5),

    %% test do_not_set part
    autotest:mark(?LINE, "set/8 - 2"),
    SipUrl5 = set([], SipUrl5),

    %% test url_param record() as argument
    autotest:mark(?LINE, "set/8 - 3"),
    SipUrl6 = set([{proto, "SIPS"}, {user, "hoKan"}, {pass, "fOObar"}, {host, "foo.su.it"},
		   {port, 43}, {param, url_param:to_norm(["foo=barbaz", "baz"])}], SipUrl5),

    %% test setting of hostname with trailing dots
    autotest:mark(?LINE, "set/8 - 4"),
    SipUrl6 = set([{host, "foo.su.it.."}], SipUrl6),

    %% get_port
    %%--------------------------------------------------------------------
    %% integer() in sipurl record
    autotest:mark(?LINE, "get_port/1 - 1"),
    43 = get_port(new([{proto, "sIp"}, {user, "hokan"}, {pass, "foobar"}, {host, "Su.iT"}, {port, 43}])),

    %% parse
    %%--------------------------------------------------------------------
    %% parse minimal url
    ParsedUrl = #sipurl{proto = "sip",
			user = none, pass = none,
			host = "atlanta.com", port = none,
			param_pairs = url_param:to_norm([])
		       },
    autotest:mark(?LINE, "parse/1 - 1"),
    ParsedUrl = parse("sip:atlanta.com"),
    %% parse url with host and port
    ParsedUrl2 = #sipurl{proto = "sip",
			 user = none, pass = none,
			 host = "atlanta.com", port = 42,
			 param_pairs = url_param:to_norm([])
			},
    autotest:mark(?LINE, "parse/1 - 2"),
    ParsedUrl2 = parse("sip:atlanta.com:42"),
    %% parse url with user, host and port
    ParsedUrl3 = #sipurl{proto = "sip",
			 user = "alice", pass = none,
			 host = "atlanta.com", port = 42,
			 param_pairs = url_param:to_norm([])
			},
    autotest:mark(?LINE, "parse/1 - 3"),
    ParsedUrl3 = parse("sip:alice@atlanta.com:42"),
    %% parse url with user, password, host and port
    ParsedUrl4 = #sipurl{proto = "sip",
			 user = "alice", pass = "foo",
			 host = "atlanta.com", port = 42,
			 param_pairs = url_param:to_norm([])
			},
    autotest:mark(?LINE, "parse/1 - 4"),
    ParsedUrl4 = parse("sip:alice:foo@atlanta.com:42"),
    %% parse url with user, password, host, port and parameters
    ParsedUrl5 = #sipurl{proto = "sip",
			 user = "alice", pass = "foo",
			 host = "atlanta.com", port = 42,
			 param_pairs = url_param:to_norm(["foo=bar", "zop"])
			},
    autotest:mark(?LINE, "parse/1 - 5"),
    ParsedUrl5 = parse("sip:alice:foo@atlanta.com:42;foo=bar;zop"),

    %% parse url with escape codes
    ParsedUrl6 = #sipurl{proto = "sip",
			 user = "alice", pass = none,
			 host = "atlanta.com", port = none,
			 param_pairs = url_param:to_norm(["transport=tcp"])
			},
    autotest:mark(?LINE, "parse/1 - 6"),
    ParsedUrl6 = parse("sip:%61lice@atlanta.com;transport=TCP"),

    %% parse url with escape codes (which must be escaped chars) in all escapable fields
    ParsedUrl7 = #sipurl{proto = "sip",
			 user = ":lice", pass = ":",
			 host = "atlanta.com", port = none,
			 param_pairs = url_param:to_norm(["transp%3b%3Bort=TCP"])
			},
    autotest:mark(?LINE, "parse/1 - 7"),
    ParsedUrl7 = parse("sip:%3Alice:%3A@atlanta.com;transp%3b%3Bort=TCP"),

    %% Parse url with escape codes (which must be escaped chars) in all escapable fields
    %% Use IPv4 host
    ParsedUrl8 = #sipurl{proto = "sip",
			 user = ":lice", pass = ":",
			 host = "2.2.2.2", port = none,
			 param_pairs = url_param:to_norm(["transp%3b%3Bort=TCP"])
			},
    autotest:mark(?LINE, "parse/1 - 8"),
    ParsedUrl8 = parse("sip:%3Alice:%3A@2.2.2.2;transp%3b%3Bort=TCP"),

    %% Parse url with escape codes (which must be escaped chars) in all escapable fields
    %% Use IPv6 host
    ParsedUrl9 = #sipurl{proto = "sip",
			 user = ":lice", pass = ":",
			 host = "[1:1:1:1:2:2:2:2]", port = none,
			 param_pairs = url_param:to_norm(["transp%3b%3Bort=TCP"])
			},
    autotest:mark(?LINE, "parse/1 - 9"),
    ParsedUrl9 = parse("sip:%3Alice:%3A@[1:1:1:1:2:2:2:2];transp%3b%3Bort=TCP"),

    %% print(URL)
    %%--------------------------------------------------------------------
    %% test host
    autotest:mark(?LINE, "print/1 - 1"),
    URL1 = parse("sip:atlanta.com"),
    "sip:atlanta.com" = print(URL1),

    %% test host, port
    autotest:mark(?LINE, "print/1 - 2"),
    URL2 = parse("sip:atlanta.com:42"),
    "sip:atlanta.com:42" = print(URL2),

    %% test host, port and parameters
    autotest:mark(?LINE, "print/1 - 3"),
    URL3 = parse("sip:atlanta.com:42;TransPort=TcP"),
    "sip:atlanta.com:42;transport=tcp" = print(URL3),

    %% test user, host and parameters
    autotest:mark(?LINE, "print/1 - 4"),
    URL4 = parse("sip:alice@atlanta.com;transport=TCP"),
    "sip:alice@atlanta.com;transport=tcp" = print(URL4),

    %% test user, password, host and parameters
    autotest:mark(?LINE, "print/1 - 5"),
    URL5 = parse("sip:alice:foo@atlanta.com;transport=TCP"),
    "sip:alice:foo@atlanta.com;transport=tcp" = print(URL5),

    %% test escaped char ":" in user field, that need to be escaped in output
    autotest:mark(?LINE, "print/1 - 6"),
    URL6 = parse("sip:%3ali%3Ace@atlanta.com;TransPort=TcP"),
    "sip:%3Ali%3Ace@atlanta.com;transport=tcp" = print(URL6),

    %% test escaped char ":" in user and password field, that need to be escaped in output
    autotest:mark(?LINE, "print/1 - 7"),
    URL7 = parse("sip:%3ali%3Ace:%3a@atlanta.com;LR;FoO"),
    "sip:%3Ali%3Ace:%3A@atlanta.com;lr;foo" = print(URL7),

    %% test escaped char ":" in user and password and ";"
    %% and "=" in paramter field, that need to be escaped in output
    autotest:mark(?LINE, "print/1 - 8"),
    URL8 = parse("sip:%3ali%3Ace:%3a@atlanta.com;LR%3b;F%3doO"),
    "sip:%3Ali%3Ace:%3A@atlanta.com;lr%3B;f%3Doo" = print(URL8),


    %% url_is_equal/2
    %%--------------------------------------------------------------------
    %% compare host
    autotest:mark(?LINE, "url_is_equal/2 - 11"),
    A11 = parse("sip:atlanta.com"),
    B11 = parse("sip:AtLanTa.CoM"),
    true = url_is_equal(A11, B11),

    %% compare host and port
    autotest:mark(?LINE, "url_is_equal/2 - 12"),
    A12 = parse("sip:atlanta.com:42"),
    B12 = parse("sip:AtLanTa.CoM:42"),
    true = url_is_equal(A12, B12),

    %% compare host, port and user
    autotest:mark(?LINE, "url_is_equal/2 - 13"),
    A13 = parse("sip:foo@atlanta.com:42"),
    B13 = parse("sip:foo@AtLanTa.CoM:42"),
    true = url_is_equal(A13, B13),

    %% compare host, port, user and password
    autotest:mark(?LINE, "url_is_equal/2 - 14"),
    A14 = parse("sip:foo:bar@atlanta.com"),
    B14 = parse("sip:foo:bar@AtLanTa.CoM"),
    true = url_is_equal(A14, B14),

    %% the urls below are taken from RFC 3261 chapter 19.1.4 page 154
    %% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    %% equal, only escape codes and case difference in fields that ignore case
    autotest:mark(?LINE, "url_is_equal/2 - 1"),
    A1 = parse("sip:%61lice@atlanta.com;transport=TCP"),
    B1 = parse("sip:alice@AtLanTa.CoM;Transport=tcp"),
    true = url_is_equal(A1, B1),

    %% equal, unique parameters that aren't = transport, user, ttl, maddr or method
    autotest:mark(?LINE, "url_is_equal/2 - 2"),
    A2 = parse("sip:carol@chicago.com"),
    B2 = parse("sip:carol@chicago.com;newparam=5"),
    C2 = parse("sip:carol@chicago.com;security=on"),
    true = url_is_equal(A2, B2),
    true = url_is_equal(B2, C2),
    true = url_is_equal(A2, C2),

    %% equal, only different order on the uri-parameters
    %% XXX header fields are currently not tested
    autotest:mark(?LINE, "url_is_equal/2 - 3"),
    A3 = parse("sip:biloxi.com;transport=tcp;method=REGISTER"),
    B3 = parse("sip:biloxi.com;method=REGISTER;transport=tcp"),
    true = url_is_equal(A3, B3),
    %%     autotest:mark(?LINE, "url_is_equal/2 - 3"),
    %%     A3 = parse("sip:biloxi.com;transport=tcp;method=REGISTER?to=sip:bob%40biloxi.com"),
    %%     B3 = parse("sip:biloxi.com;method=REGISTER;transport=tcp?to=sip:bob%40biloxi.com"),
    %%     true = url_is_equal(A3, B3),

    %% equal, only different order on the header parameters
    %% XXX headers are currently not tested
    %%     autotest:mark(?LINE, "url_is_equal/2 - 4"),
    %%     A4 = parse("sip:alice@atlanta.com?subject=project%20x&priority=urgent"),
    %%     B4 = parse("sip:alice@atlanta.com?priority=urgent&subject=project%20x"),
    %%     true = url_is_equal(A4, B4),

    %% not equal, different usernames
    autotest:mark(?LINE, "url_is_equal/2 - 5"),
    A5 = parse("SIP:ALICE@AtLanTa.CoM;Transport=udp"),
    B5 = parse("sip:alice@AtLanTa.CoM;Transport=UDP"),
    false = url_is_equal(A5, B5),

    %% not equal, can resolve to different ports
    autotest:mark(?LINE, "url_is_equal/2 - 6"),
    A6 = parse("sip:bob@biloxi.com"),
    B6 = parse("sip:bob@biloxi.com:5060"),
    false = url_is_equal(A6, B6),

    %% not equal, can resolve to different transports
    autotest:mark(?LINE, "url_is_equal/2 - 7"),
    A7 = parse("sip:bob@biloxi.com"),
    B7 = parse("sip:bob@biloxi.com;transport=udp"),
    false = url_is_equal(A7, B7),

    %% not equal, can resolve to different port and transports
    autotest:mark(?LINE, "url_is_equal/2 - 8"),
    A8 = parse("sip:bob@biloxi.com"),
    B8 = parse("sip:bob@biloxi.com:6000;transport=tcp"),
    false = url_is_equal(A8, B8),

    %% not equal, different header component
    %% autotest:mark(?LINE, "url_is_equal/2 - 9"),
    %%     A9 = parse("sip:carol@chicago.com"),
    %%     B9 = parse("sip:carol@chicago.com?Subject=next%20meeting"),
    %%     false = url_is_equal(A9, B9),

    %% not equal, different host
    autotest:mark(?LINE, "url_is_equal/2 - 10"),
    A10 = parse("sip:bob@phone21.boxesbybob.com"),
    B10 = parse("sip:bob@192.0.2.4"),
    false = url_is_equal(A10, B10),

    ok.

%%====================================================================
%% Behaviour functions
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% BNF checking functions, based on RFC3261 chapter 25
%%--------------------------------------------------------------------

is_user_unreserved($&) -> true;
is_user_unreserved($=) -> true;
is_user_unreserved($+) -> true;
is_user_unreserved($$) -> true;
is_user_unreserved($,) -> true;
is_user_unreserved($;) -> true;
is_user_unreserved($?) -> true;
is_user_unreserved($/) -> true;
is_user_unreserved(_) -> false.

is_mark($-) -> true;
is_mark($_) -> true;
is_mark($.) -> true;
is_mark($!) -> true;
is_mark($~) -> true;
is_mark($*) -> true;
is_mark($') -> true;
is_mark($() -> true;
is_mark($)) -> true;
is_mark(_) -> false.

is_unreserved(Char) ->
    sipparse_util:is_alphanum(Char) or is_mark(Char).

is_param_unreserved($[) -> true;
is_param_unreserved($]) -> true;
is_param_unreserved($/) -> true;
is_param_unreserved($:) -> true;
is_param_unreserved($&) -> true;
is_param_unreserved($+) -> true;
is_param_unreserved($$) -> true;
is_param_unreserved(_) -> false.

is_hexdig(Char) ->
    if
	(Char >= $0) and (Char =< $9) ->
	    true;
	(Char >= $A) and (Char =< $F) ->
	    true;
	(Char >= $a) and (Char =< $f) ->
	    true;
	true ->
	    false
    end.

is_escaped([$%, H1, H2]) ->
    is_hexdig(H1) or is_hexdig(H2).

%% 'true' if char is any non-escaped password char
is_password_without_escape($&) -> true;
is_password_without_escape($=) -> true;
is_password_without_escape($+) -> true;
is_password_without_escape($$) -> true;
is_password_without_escape($,) -> true;
is_password_without_escape(Char) ->
    is_unreserved(Char).

%%--------------------------------------------------------------------
%% @spec    (Str) -> true | false
%%
%%            Str = string() | none
%%
%% @doc     parse password string, return 'true' if it conforms to the
%%          format specified in RFC3261 chapter 25.1 p218
%% @end
%%--------------------------------------------------------------------
is_password(none) -> true;
is_password([]) -> true;
is_password("&" ++ Str) ->
    is_password(Str);
is_password("=" ++ Str) ->
    is_password(Str);
is_password("+" ++ Str) ->
    is_password(Str);
is_password("\$" ++ Str) ->
    is_password(Str);
is_password("," ++ Str) ->
    is_password(Str);
is_password([$%, C2, C3 | Str]) ->
    is_escaped([$%, C2, C3]) andalso is_password(Str);
is_password([Char | Str]) ->
    is_unreserved(Char) andalso is_password(Str);
is_password(_) ->
    false.

%%--------------------------------------------------------------------
%% @spec    (Str) -> true | false
%%
%%            Str = string() | none
%%
%% @doc     parse user string, return 'true' if it conforms to the
%%          format specified in RFC3261 chapter 25.1 p218
%% @end
%%--------------------------------------------------------------------
is_user(none) -> true;
is_user([]) -> true;
is_user([$%, C1, C2 | Str]) ->
	 is_escaped([$%, C1, C2]) andalso is_user(Str);
		     is_user([Char | Str]) ->
			    (is_unreserved(Char) orelse is_user_unreserved(Char)) andalso is_user(Str);
		     is_user(_) ->
			    false.

%%--------------------------------------------------------------------
%% @spec    (Parameters) -> true | false
%%
%%            Parameters = string()
%%
%% @doc     parse uri-parameter string, return true if there are no
%%          duplicate ";" or "=" and all other chars are correct and
%%          properly escaped
%% @end
%%--------------------------------------------------------------------
%% this is a redundant check but reduces the amount of work done to
%% check for empty Parameters strings
is_parameters([]) -> true;
is_parameters(Parameters) ->
    Match = ";;",
    %% string:tokens in is_uri_parameters/1 below will consume several ";"
    %% in sucession and consider it as one separator, as sequences like
    %% ";;" are illegal, this is checked for here instead
    case regexp:first_match(Parameters, Match) of
	{match, _, _} -> false;
	_ ->
	    is_uri_parameters(Parameters)
    end.

%% return true if Str is a uri-paramter string
%% Str = xxx;yyy;zzz .... (first ; is assumed to have been consumed
%% in previous string:tokens/2 call)
is_uri_parameters(Str) ->
    Parameters = string:tokens(Str, ";"),
    lists:all(fun is_uri_parameter/1, Parameters).

is_uri_parameter(Str) ->
    Match = "==",
    %% string:tokens/2 below will consume several "==" in sucession
    %% and consider it as one separator, as sequences like
    %% "==" are illegal, this is checked for here instead
    case regexp:first_match(Str, Match) of
	{match, _, _} -> false;
	_ ->
	    case string:tokens(Str, "=") of
		[Name, Val] ->
		    is_paramchar_str(Name) and is_paramchar_str(Val);
		[Name] ->
		    is_paramchar_str(Name)
	    end
    end.

%% checks name or value string in uri-parameter for illegal chars
%% returns 'false' if any are found
is_paramchar_str([]) -> true;
is_paramchar_str([$% | Str]) ->
    [C1, C2 | R] = Str,
    is_escaped([$%, C1, C2]) andalso is_paramchar_str(R);
is_paramchar_str([Char | Str]) ->
    (is_param_unreserved(Char) or is_unreserved(Char)) andalso is_paramchar_str(Str).
