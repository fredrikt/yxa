%%%-------------------------------------------------------------------
%%% File    : interpret_backend.erl
%%% @author   Håkan Stenholm <hsten@it.su.se>
%%% @doc     
%%%
%%% @since    03 Dec 2004 by Håkan Stenholm <hsten@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(interpret_backend).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 %% outgoing
	 get_outgoing_destination/1,

	 %% address-switch
	 'get_address-switch_field'/3,
	 compare_address_or_address_part/2,
	 address_or_address_part_contains/2,
	 is_subdomain/2,

	 %% string-switch
	 'get_string-switch_field'/2,
	 string_is/2,
	 string_contains/2,

	 %% language-switch
	 'get_language-switch_value'/1,
	 language_matches/2,

	 %% priority-switch
	 'get_priority-switch_value'/1,
	 priority_less/2,
	 priority_greater/2,
	 priority_equal/2,

	 %% lookup
	 lookup/4,

	 %% location
	 add_location/4,

	 %% remove-location
	 rm_location/2,

	 %% log
	 log/3,

	 %% mail
	 mail/2,

	 %% time-switch
	 in_time_range/2,

	 %% proxy
	 get_min_ring/0,
	 get_server_max/0,
	 test_proxy_destinations/8,


	 %% test ---------------------
	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
        ]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipproxy.hrl").
-include("cpl.hrl").

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
%% Function:
%% Descrip.: determine the destination of a request handled by a
%%           'outgoing' tag
%% Returns : sipurl record()
%% Note    : only SIP URIs are supported
%%--------------------------------------------------------------------
get_outgoing_destination(Request) when is_record(Request, request) ->
    Request#request.uri.


%%--------------------------------------------------------------------
%% @spec    (Request, Field, SubField) ->
%%            #sipurl{} | {Field, Val} | '#no_value'
%%
%%            Request  = #request{}
%%            Field    = atom() "address-switch tag field attribute value"
%%            SubField = atom() "address-switch tag subfield attribute value"
%%
%%            Field = term() "subfield name, attribute name in address-switch tag"
%%
%% @doc     retrieves the value from a request that address-switch
%%          wants to switch on Note : only SIP URIs are supported
%% @end
%%--------------------------------------------------------------------
%% XXX URI can supposedly always be retrieved ?
%% XXX DisplayName handling ?
'get_address-switch_field'(Request, Field, '#no_value') when is_record(Request, request) ->
    {_DisplayName, URI} = get_address_switch_uri(Request, Field),
    URI;

%% return: {Field, Val}
'get_address-switch_field'(Request, Field, SubField) when is_record(Request, request) ->
    {DisplayName, URI} = get_address_switch_uri(Request, Field),
    case SubField of
	'address-type' ->
	    %% string(), lower case
	    none_to_no_value_atom(SubField, URI#sipurl.proto);
	user ->
	    %% none | string()
	    none_to_no_value_atom(SubField, URI#sipurl.user);
	host ->
	    %% string(), lower case
	    none_to_no_value_atom(SubField, URI#sipurl.host);
	port ->
	    %% none | integer()
	    none_to_no_value_atom(SubField, sipurl:get_port(URI));
	tel ->
	    throw({error, tel_unsupported_address_switch_attribute});
	display ->
	    %% none | string()
	    none_to_no_value_atom(SubField, DisplayName);
	password ->
	    %% none | string()
	    none_to_no_value_atom(SubField, URI#sipurl.pass)
    end.

none_to_no_value_atom(_, none) ->
    '#no_value';
none_to_no_value_atom(SubField, Val) ->
    {SubField, Val}.

%%--------------------------------------------------------------------
%% @spec    (ReqVal, Val) -> true | false
%%
%%            ReqVal = #sipurl{} | {SubField, RVal} "both are return values from 'get_address-switch_field'/3"
%%            Val    = string() "cpl script supplied value"
%%
%% @doc     determine if Val from CPL script matches value (ReqVal) in
%%          Request Note : only SIP URIs are supported Note : most
%%          ReqVal's originate from a sipurl record() that ensures
%%          proper case for the attributes, Val's on the other hand
%%          have to be converted as they are taken "as is" from a CPl
%%          script
%% @end
%%--------------------------------------------------------------------

%% compare complete URIs according to protocol (SIP)
compare_address_or_address_part(ReqVal, Val) when is_record(ReqVal, sipurl), is_list(Val) ->
    ValURI = sipurl:parse(Val),
    sipurl:url_is_equal(ReqVal, ValURI);

%% values are case insensitive
compare_address_or_address_part({'address-type', ReqVal}, Val) ->
    ReqVal == httpd_util:to_lower(Val);

%% values are case sensitive
compare_address_or_address_part({user, ReqVal}, Val) ->
    ReqVal == Val;

%% "Host names are compared as strings. IP addresses are compared
%%  numerically.  (In particular, the presence or location of an
%%  IPv6 :: omitted-zero-bits block is not significant for
%%  matching purposes.)  Host names are never equal to IP
%%  addresses -- no DNS resolution is performed.  IPv4 addresses
%%  are never equal to IPv6 addresses, even if the IPv6 address
%%  is a v4-in-v6 embedding." - RFC 3880 chapter 4 p9
compare_address_or_address_part({host, ReqVal}, Val) ->
    %% host name values are case insensitive
    ReqVal == httpd_util:to_lower(Val)
    %% XXX this will probably fail for IPv4 and IPv6
    ;

compare_address_or_address_part({port, ReqVal}, Val) ->
    %% XXX xml_parse.erl doesn't check if Val is a numerical string
    ReqVal == list_to_integer(Val);

%% "It is a Unicode string, and is matched using the
%%  case-insensitive algorithm described in Section 4.2."
%% - RFC 3880 chapter 4 p10
%% "Strings are matched as case-insensitive Unicode strings, in the
%%  following manner.  First, strings are canonicalized to the
%%  "Compatibility Composition" (KC) form, as specified in Unicode
%%  Standard Annex #15 [5].  Then, strings are compared using locale-
%%  insensitive caseless mapping, as specified in Unicode Standard Annex
%%  #21 [6]." - RFC 3880 chapter 4.2 p12
compare_address_or_address_part({display, ReqVal}, Val) ->
    %% XXX erlang only supports Latin-1 (8 bit), so unicode will only
    %%     work correctly when it's in the ASCII range
    httpd_util:to_lower(ReqVal) == httpd_util:to_lower(Val);

%% values are case sensitive
compare_address_or_address_part({password, ReqVal}, Val) ->
    ReqVal == Val.

%%--------------------------------------------------------------------
%% @spec    (ReqVal, Val) -> true | false
%%
%%            ReqVal = {display, RVal} "value from 'get_address-switch_field'/3"
%%            Val    = string()
%%
%% @doc     determine if Val from CPL script was part of value
%%          (ReqVal) in Request
%% @end
%%--------------------------------------------------------------------
%% XXX see compare_address_or_address_part({display, ReqVal}, Val) about lacking unicode support !
address_or_address_part_contains({display, ReqVal}, Val) ->
    %% non-zero index is returned if Val is a part of ReqVal
    case string:str(httpd_util:to_lower(ReqVal), httpd_util:to_lower(Val)) of
	0 -> false;
	_ -> true
    end.

%%--------------------------------------------------------------------
%% @spec    (ReqHostVal, ScriptVal) -> true | false
%%
%%            ReqHostVal = string() "a domain name or IP4/6 value"
%%            ScriptVal  = string() "a domain name or IP4/6 value"
%%
%% @doc     check that ScriptVal matches the tail of ReqHostVal e.g.
%%          that "bar.com" is part of "foo.bar.com". is_subdomain/2
%%          checks that SubStr is well formed i.e.
%%          is_subdomain("foo.bar.com","ar.com") would return false
%%          Note : IP addresses can be passed to this function but
%%          are only compared for equality (see RFC 3880 chapter 4.1
%%          p10), domain names on the other hand support full
%%          subdomain checking.
%% @end
%%--------------------------------------------------------------------
is_subdomain(ReqHostVal, ScriptVal) ->
    case {checktype(ReqHostVal), checktype(ScriptVal)} of
	{domain_name, domain_name} ->
	    is_subdomain2(ReqHostVal, ScriptVal);
	{ip4addr, ip4addr} ->
	    ReqHostVal == ScriptVal;
	{ip6addr, ip6addr} ->
	    ReqHostVal == ScriptVal;
	{_,_} ->
	    false
    end.

%% return: domain_name | ip4addr | ip6addr | throw() (if DomainStr isn't some kind of domain name)
checktype(DomainStr) ->
    case is_true(fun() -> sipparse_util:is_hostname(DomainStr) end) of
	true -> domain_name;
	false ->
	    case is_true(fun() -> sipparse_util:is_IPv4address(DomainStr) end) of
		true -> ip4addr;
		false ->
		    case is_true(fun() -> sipparse_util:is_IPv6reference(DomainStr) end) of
			true -> ip6addr;
			false ->
			     throw({error, not_domain_name_or_ip4addr_or_ip6addr})
		    end
	    end
    end.

%% return: true (if F() returns true) | false (if F() returns anything but true or throws a exception)
is_true(F) ->
    try begin
	    case F() of
		true -> true;
		_ -> false
	    end
	end
    catch
	_ : _ -> false
    end.


is_subdomain2(_, "") ->
    false;
is_subdomain2(Str, SubStr) ->
    is_subdomain3(lists:reverse(Str), lists:reverse(SubStr)).

%% Str = SubStr
is_subdomain3([], []) -> true;
%% Str shorter than SubStr
is_subdomain3([], _) -> false;

%% only "xxxx." part of Str left
is_subdomain3([$. | _], []) -> true;
%% there is no "." in Str
is_subdomain3(_Str, []) -> false;
%% both still match
is_subdomain3([C | R1], [C | R2]) ->
    is_subdomain3(R1,R2);
%% part of SubString doesn't match
is_subdomain3([_C1 | _], [_C2 | _]) ->
    false.

%%--------------------------------------------------------------------
%% @spec    (Request, Field) ->
%%            Res
%%
%%            Request = #request{}
%%            Field   = atom() "'string-switch' tag 'field' attribute value"
%%
%%            Res = '#no_value'                                                          |
%%                  string() "in lower case       XXX this should be lower case unicode"
%%
%% @doc     retrieve the specified field from Request
%% @end
%%--------------------------------------------------------------------
'get_string-switch_field'(Request, Field) when is_record(Request, request) ->
    Header = Request#request.header,
    case Field of
	subject ->
	    case keylist:fetch("subject", Header) of
		[] -> '#no_value';
		[SubjectStr] -> httpd_util:to_lower(SubjectStr)
	    end;
	organization ->
	    case keylist:fetch("organization", Header) of
		[] -> '#no_value';
		[OrganizationStr] -> httpd_util:to_lower(OrganizationStr)
	    end;
	'user-agent' ->
	    case keylist:fetch("user-agent", Header) of
		      [] -> '#no_value';
		[UserAgentStr] -> httpd_util:to_lower(UserAgentStr)
		  end;
	display ->
	    throw({error, string_switch_tag_field_attribute_value_display_unsupported_by_sip})
    end.

%%--------------------------------------------------------------------
%% @spec    (ReqVal, Val) -> true | false
%%
%% @doc     Check if Val is same as ReqVal. Note : "Strings are
%%          matched as case-insensitive Unicode strings, in the
%%          following manner. First, strings are canonicalized to the
%%          "Compatibility Composition" (KC) form, as specified in
%%          Unicode Standard Annex #15 [5]. Then, strings are
%%          compared using locale- insensitive caseless mapping, as
%%          specified in Unicode Standard Annex #21 [6]." - RFC 3880
%%          chapter 4.2 p12 XXX this code only does a byte by byte
%%          compare with latin-1 chars
%% @end
%%--------------------------------------------------------------------
string_is(ReqVal, Val) ->
    httpd_util:to_lower(ReqVal) == httpd_util:to_lower(Val).

%%--------------------------------------------------------------------
%% @spec    (ReqVal, Val) -> true | false
%%
%% @doc     Check if Val is a string of ReqVal. Note : "Strings are
%%          matched as case-insensitive Unicode strings, in the
%%          following manner. First, strings are canonicalized to the
%%          "Compatibility Composition" (KC) form, as specified in
%%          Unicode Standard Annex #15 [5]. Then, strings are
%%          compared using locale- insensitive caseless mapping, as
%%          specified in Unicode Standard Annex #21 [6]." - RFC 3880
%%          chapter 4.2 p12 XXX this code only does a byte by byte
%%          compare with latin-1 chars
%% @end
%%--------------------------------------------------------------------
string_contains(ReqVal, Val) ->
    case string:str(httpd_util:to_lower(ReqVal), httpd_util:to_lower(Val)) of
	0 -> false;
	_ -> true
    end.

%%--------------------------------------------------------------------
%% @spec    (Request) -> '#no_value' | [string()] "the language codes"
%%
%%            Request = #request{}
%%
%% @doc     retrieve the languages supplied in the request sorted in q
%%          order Note : RFC 3066 handles language codes Note :
%%          sorting on q-value is mainly done here to support CPL
%%          extensions that can use q-values in a sensible manner
%%          (similar to RFC 2616 chapter 14.4) Note : "HTTP content
%%          negotiation (section 12) uses short "floating point"
%%          numbers to indicate the relative importance ("weight") of
%%          various negotiable parameters. A weight is normalized to
%%          a real number in the range
%% @end
%%--------------------------------------------------------------------
'get_language-switch_value'(Request) when is_record(Request, request) ->
    Header = Request#request.header,
    case keylist:fetch("accept-language", Header) of
	[] -> '#no_value';
	AcceptLanguageStrs ->
	    sort_languages_in_q_order(AcceptLanguageStrs)
    end.

%% return: list() of string() - the language codes
sort_languages_in_q_order(AcceptLanguageStrs) ->
    %% sort languages in qvaule priority order
    %% only sort on q-value (not on language) - element order should be stable
    QValLangs = lists:sort(fun({Q1,_L1}, {Q2,_L2}) ->
				   Q1 >= Q2
			   end,
			   [parse_accept_language(Str) || Str <- AcceptLanguageStrs]),
    %% "If the caller specified the special language-range "*", it is ignored
    %%  for the purpose of matching.  Languages with a "q" value of 0 are
    %%  also ignored." - RFC 3880 chapter 4.3 p13

    %% strip qvalues - only language strings are needed for later matching in language_matches/2
    Langs2 = [Lang || {QVal, Lang} <- QValLangs, QVal /= 0.0],
    %% remove "*"
    [Lang2 || Lang2 <- Langs2, Lang2 /= "*"].

%%--------------------------------------------------------------------
%% @spec    (ReqLangRanges, ScriptLangTag) -> true | false
%%
%%            ReqLangs      = [LanguageRange]
%%            LanguageRange = string()
%%            ScriptLangTag = term() "a language tag"
%%
%% @doc     implements a single `` <Language matches = "..."> '' match
%%          according to RFC 3880 chapter 4.3 (see cpl/README for
%%          more details) Note : the prefix_match(...) code assumes
%%          that any "*" has been removed from ReqLangRanges
%% @end
%%--------------------------------------------------------------------
language_matches([], _ScriptLangTag) ->
    false;
language_matches([ReqLangRange | R], ScriptLangTag) ->
    case prefix_match(ReqLangRange, ScriptLangTag) of
	true ->
	     true;
	false ->
	    language_matches(R, ScriptLangTag)
    end.


prefix_match(SubString, String) ->
    prefix_match2(httpd_util:to_lower(SubString), httpd_util:to_lower(String)).

%% SubString = String
prefix_match2([], []) ->
    true;
%% SubString is a proper sub string
prefix_match2([], [$-, _ | _]) ->
    true;
%% sub string doesn't end at a language sub tag (a "-")
prefix_match2([], _) ->
    false;
%% chars match - go check the rest
prefix_match2([C | R1], [C | R2]) ->
    prefix_match2(R1, R2);
%% char missmatch
prefix_match2(_, _) ->
    false.

%%--------------------------------------------------------------------
%% RFC 3066
%% Language-Tag = Primary-subtag *( "-" Subtag )
%% Primary-subtag = 1*8ALPHA
%% Subtag = 1*8(ALPHA / DIGIT)
%%
%% language-range  = language-tag / "*"
%%
%% RFC 3261
%% Accept-Language  =  "Accept-Language" HCOLON [ language *(COMMA language) ]
%% language         =  language-range *(SEMI accept-param)
%% language-range   =  ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) / "*" )
%% accept-param     =  ("q" EQUAL qvalue) / generic-param
%% qvalue           =  ( "0" [ "." 0*3DIGIT ] ) / ( "1" [ "." 0*3("0") ] )
%% generic-param    =  token [ EQUAL gen-value ]
%% gen-value        =  token / host / quoted-string
%%--------------------------------------------------------------------
%% @spec    (Str) ->
%%            {QVal, Language} 
%%
%%            Str = string() "a single language entry from a Accept-Language header"
%%
%%            Language = string() | Asterisk
%%            Asterisk = [42] "silly because of edoc limitation"
%%            QVal     = float() "in range = 0-1"
%%
%% @throws  {error, malformed_accept_language_element} 
%%
%% @doc     Parse a language entry with possible q-value. Throws an
%%          error if Str is a malformed member of a Accept-Language
%%          header in a request.
%% @end
%%--------------------------------------------------------------------
parse_accept_language(Str) ->
    %% try is used, so that a consistent exception can be returned for
    %% all errors in parse_accept_language/1, rather than returning
    %% sipparse_util:split_fields/2 for some of them
    try begin
	    case catch sipparse_util:split_fields(Str, $;) of
		{Lang} ->
		    {1.0, xml_parse_util:is_language_range(string:strip(Lang))};
		{Lang, QPart} when is_list(Lang), is_list(QPart) ->
		    case catch sipparse_util:split_fields(QPart, $=) of
			{Q, QValStr} when is_list(Q), is_list(QValStr) ->
			    QVal = sipparse_util:str_to_qval(string:strip(QValStr)),
			    case string:strip(Q) of
				"Q" -> {QVal, xml_parse_util:is_language_range(string:strip(Lang))};
				"q" -> {QVal, xml_parse_util:is_language_range(string:strip(Lang))};
				_ -> throw({error, malformed_accept_language_element})
			    end;
			_ ->
			    throw({error, malformed_accept_language_element})
		    end;
		{error, _} ->
		    throw({error, malformed_accept_language_element})
	    end
	end
    catch
	throw: _ -> throw({error, malformed_accept_language_element})
    end.

%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            normal | emergency | urgent | 'non-urgent' |
%%            {unknown, PrioStr}
%%
%%            Request = #request{}
%%
%%            PrioStr = string()
%%
%% @doc     Get priority from initial INVITE "Priority" header.
%% @end
%%--------------------------------------------------------------------
%% "The priority of a SIP message corresponds to the "Priority" header in
%%  the initial "INVITE" message." - RFC 3880 chapter 4.5.1 p21
'get_priority-switch_value'(Request) when is_record(Request, request) ->
    Method = Request#request.method,
    case Method of
	"INVITE" ->
	    Header = Request#request.header,
	    Prio = keylist:fetch("priority", Header),
	    case Prio of
		[] ->
		    prio(normal);
		[PrioStr] ->
		    prio(xml_parse_util:normalize_prio(PrioStr))
	    end;
	_ ->
	    prio(normal)
    end.

%% compare priority values as specified in RFC 3880 chapter 4.5 p21
%% - only equal checks make any real sense for unknown priority values,
%% unknown values in less and greater are treated as missing values (prio = normal)

priority_less(ReqVal, Val) when is_integer(ReqVal) ->
    ReqVal < prio(Val);
priority_less(Unknown, Val) when is_list(Unknown) ->
    prio(normal) < prio(Val).

priority_greater(ReqVal, Val) when is_integer(ReqVal) ->
    ReqVal > prio(Val);
priority_greater(Unknown, Val) when is_list(Unknown) ->
    prio(normal) > prio(Val).

%% both ReqVal and Val may be integer() (if it's a known priority value)
%% or string() (if the priority is unknown)
priority_equal(ReqVal, Val) ->
    ReqVal == prio(Val).

%% Return numerical priority code to simplify compare operations.
%% Return lowercased string() if the priority value is unknown.
prio(Prio) ->
    case Prio of
	emergency -> 4;
	urgent -> 3;
	normal -> 2;
	'non-urgent' -> 1;
	{unknown, PrioStr} -> httpd_util:to_lower(PrioStr)
    end.

%%--------------------------------------------------------------------
%% @spec    (Source, User, URI, Timeout) ->
%%            {Result, Locs}
%%
%%            Source  = string() "a URI or a non-URI source (only \"registration\" is defined in RFC 3880)"
%%            User    = string() "SIP username of user whose CPL script this is"
%%            URI     = #sipurl{}
%%            Timeout = integer() "seconds to try lookup"
%%
%%            Result = success | notfound | failure
%%            Locs   = [#siplocationdb_e{}]
%%
%% @doc     Note : XXX Timeout isn't used, should result in {failure,
%%          ...}
%% @end
%%--------------------------------------------------------------------
%% XXX http lookup - unsupported
%% lookup(URISource, User, URI, Timeout) ->
%% ;

%% get all (registered) locations used by User
lookup("registration", User, URI, _Timeout) ->
    try local:lookupuser_locations([User], URI) of
	[] ->
	    logger:log(debug, "CPL: Found no registered locations for user ~p", [User]),
	    {notfound, []};
	Locations ->
	    logger:log(debug, "CPL: Found ~p registered locations for user ~p", [length(Locations), User]),
	    {success, Locations}
    catch
	throw: _ ->
	    {failure, []}
    end.

%%--------------------------------------------------------------------
%% @spec    (Locations, URIstr, Prio, Clear) -> [#location{}]
%%
%%            Locations = [#location{}]
%%            URIstr    = string()
%%            Prio      = float()
%%            Clear     = bool()
%%
%% @doc     Add a location, or if Clear is true replace all previous
%%          locations with this one. Note : this version only
%%          supports SIP URIs
%% @end
%%--------------------------------------------------------------------
add_location(Locations, URIstr, Prio, Clear) ->
    logger:log(debug, "CPL: Adding location ~p (prio ~p, clear: ~p)", [URIstr, Prio, Clear]),
    This = #location{url = sipurl:parse(URIstr), priority = Prio},
    case Clear of
	yes -> [This];
	no -> [This | Locations]
    end.

%%--------------------------------------------------------------------
%% @spec    (Locations, URIStr) -> [#location{}]
%%
%%            Locations = [#location{}]
%%            URIStr    = string()
%%
%% @doc     remove (all) entries of URIStr from Locations Note : this
%%          version only supports SIP URIs
%% @end
%%--------------------------------------------------------------------
rm_location(Locations, URIStr) ->
    URI = sipurl:parse(URIStr),
    F = fun(Location, Acc) ->
		LocationURI = Location#location.url,
		case sipurl:url_is_equal(LocationURI, URI) of
		    true ->
			logger:log(debug, "CPL: Removed location ~p", [sipurl:print(LocationURI)]),
			Acc;
		    false -> [Location | Acc]
		end
	end,
    lists:foldl(F,[], Locations).

%%--------------------------------------------------------------------
%% @spec    (LogAttrs, User, Request) -> term()
%%
%%            LogAttrs = #log__attrs{}
%%            User     = string() "user id"
%%            Request  = #request{}
%%
%% @doc     log to some kind of storage (refered to as
%%          #log__attrs.name)
%% @end
%%--------------------------------------------------------------------
%% "Servers SHOULD also include other information in the log, such as
%%  the time of the logged event, information that triggered the call
%%  to be logged, and so forth. Logs are specific to the owner of the
%%  script which logged the event." - RFC 3880 chapter 7.2 p32

log(#log__attrs{name = LogName, comment = Comment}, User, Request) ->
    log(LogName, Comment, User, Request).

%%--------------------------------------------------------------------
%% @spec    (LogName, Comment, User, Request) -> ok
%%
%%            LogName = default | string() "cpl parameter"
%%            Comment = string() "cpl parameter"
%%            User    = string() "user id"
%%            Request = #request{}
%%
%% @doc     log to some kind of storage refereed to as LogName
%% @end
%%--------------------------------------------------------------------
log(LogName, Comment, User, Request) ->
    case local:cpl_log(LogName, Comment, User, Request) of
	undefined ->
	    ok;
        _ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @spec    (Mail, User) -> term()
%%
%%            Mail = string() "a mail url"
%%            User = string() "user id"
%%
%% @doc     send a mail, by using a callback module
%%          'cpl_mail_callback' defined in local.erl, default
%%          behavior (when no callback has been supplied) is to do
%%          nothing. The callback function CallBackModule:mail/2
%%          takes the same arguments as this function
%% @end
%%--------------------------------------------------------------------
%% "The "mail" node takes one argument: a "mailto" URL giving the
%%  address, and any additional desired parameters, of the mail to be
%%  sent.  The server sends the message containing the content to the
%%  given url; it SHOULD also include other status information about the
%%  original call request and the CPL script at the time of the
%%  notification.
%%
%%     Using a full "mailto" URL rather than just an e-mail address
%%     allows additional e-mail headers to be specified, such as
%%     <mail url="mailto:jones@example.com?subject=Lookup%20failed" />."
%% - RC 3880 chapter 7.1 p 30
%%--------------------------------------------------------------------
mail(Mail, User) ->
    case local:cpl_mail(Mail, User) of
	undefined ->
	    ok;
	_ ->
	    ok
    end.


%%--------------------------------------------------------------------
%% @spec    (Timezone, TimeSwitchCond) -> true | false
%%
%% @doc     determine if the time period specified by the
%%          TimeSwitchCond is currently occurring
%% @end
%%--------------------------------------------------------------------
in_time_range(Timezone, TimeSwitchCond) ->
    interpret_time:in_time_range(Timezone, TimeSwitchCond).


%%--------------------------------------------------------------------
%% @spec    () -> integer() "the number of seconds"
%%
%% @doc     the shortest time a "phone" may ring
%% @end
%%--------------------------------------------------------------------
get_min_ring() ->
    {ok, M} = yxa_config:get_env(cpl_minimum_ringtime, 10),
    M.

%%--------------------------------------------------------------------
%% @spec    () -> integer() "the number of seconds"
%%
%% @doc     the maximum ring-time for this application
%% @end
%%--------------------------------------------------------------------
get_server_max() ->
    {ok, M} = yxa_config:get_env(cpl_call_max_timeout),
    M.

%%--------------------------------------------------------------------
%% @spec    (Count, BranchBase, Request, Actions, Surplus, Timeout,
%%          Recurse, STHandler) ->
%%            {Result, BestLocation, BestResponse}
%%
%% @doc
%% @hidden
%% @end
%%--------------------------------------------------------------------
test_proxy_destinations(Count, BranchBase, Request, Actions, Surplus, _Timeout, _Recurse, STHandler) ->
    %% Count is used to add a "-cplN" element to the branch name, this is
    %% needed as there my be more than one sipproxy:start call executed in a cpl script
    ProxyBranchBase = lists:concat([BranchBase, "-cpl", Count]),
    logger:log(debug, "~p: CPL proxy actions :~n~p", [ProxyBranchBase, Actions]),
    {ok, AppGluePid} = appserver_glue:start_link_cpl(self(), ProxyBranchBase, STHandler, Request, Actions, Surplus),
    %% We need the actual pid from STHandler to match signals from it inside _loop()
    STHandlerPid = transactionlayer:get_pid_from_handler(STHandler),
    %% Set up process monitors to both STHandlerPid and AppGluePid. We are linked to them,
    %% but want to terminate if any of them exits - even normally
    STMonitor = erlang:monitor(process, STHandlerPid),
    AGMonitor = erlang:monitor(process, AppGluePid),
    Res = test_proxy_destinations_loop(STHandlerPid, AppGluePid, true),
    erlang:demonitor(STMonitor),
    erlang:demonitor(AGMonitor),
    Res.

%%--------------------------------------------------------------------
%% @spec    (STHandlerPid, AppGluePid, STAlive) ->
%%            {Result, BestLocation, BestResponse}
%%
%%            STHandlerPid = pid() "server transaction handler pid"
%%            AppGluePid   = pid() "appserver_glue process pid"
%%            STAlive      = true | false "server transaction status"
%%
%%            Result       = success | atom() "proxy result"
%%            BestLocation = #sipurl{} | none
%%            BestResponse = #response{} | {Status, Reason}
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
test_proxy_destinations_loop(STHandlerPid, AppGluePid, STAlive) ->
    receive
	%%
	%% Messages from the server transaction handler
	%%
	{servertransaction_cancelled, STHandlerPid, ExtraHeaders} ->
	    AppGluePid ! {servertransaction_cancelled, STHandlerPid, ExtraHeaders},
	    make_proxy_destinations_response(487, {487, "Request Cancelled while interpreting CPL"});


	{servertransaction_terminating, STHandlerPid} ->
	    AppGluePid ! {servertransaction_terminating, STHandlerPid},
	    %%test_proxy_destinations_loop(STHandlerPid, AppGluePid, false);
	    make_proxy_destinations_response(500, {500, "Server transcation exited"});


	{'DOWN', _MonitorRef1, process, STHandlerPid, normal} when STAlive == false ->
	    %% just ignore, we knew the server transaction was terminating
	    test_proxy_destinations_loop(STHandlerPid, AppGluePid, STAlive);

	%%
	%% Messages about our appserver_glue process being finished, this is what makes us exit
	%%
	{appserver_glue_final_response, AppGluePid, Response} when is_record(Response, response) ->
	    make_proxy_destinations_response(Response#response.status, Response);

	{appserver_glue_final_response, AppGluePid, {Status, Reason}} when is_integer(Status), is_list(Reason) ->
	    make_proxy_destinations_response(Status, {Status, Reason});


	%%
	%% Error situations
	%%

	{'DOWN', _MonitorRef2, process, AppGluePid, normal} ->
	    %% Our AppServerPid died (normally, but we wouldn't expect that)
	    logger:log(error, "CPL test_proxy_destinations_loop: My appserver glue process (~p) terminated "
		       "(normally) before I did", [AppGluePid]),
	    erlang:error(appserver_glue_exited, [STHandlerPid, AppGluePid]);

	{'DOWN', _MonitorRef3, process, STHandlerPid, normal} when STAlive == true ->
	    %% Our STHandlerPid died (normally, but we wouldn't expect that)
	    logger:log(error, "CPL test_proxy_destinations_loop: My server transaction (~p) terminated "
		       "(normally) before I did", [STHandlerPid]),
	    erlang:error(servertransaction_exited, [STHandlerPid, AppGluePid]);

	Msg ->
	    %% Unknown message, we'd better exit
	    %% XXX perhaps not? Is late answers to gen_server:calls an error if we have
	    %% earlier chosen to ingore the gen_server call timing out?
	    logger:log(debug, "CPL test_proxy_destinations_loop: AppGluePid ~p - ignoring unknown signal :~n~p",
		       [AppGluePid, Msg]),
	    test_proxy_destinations_loop(STHandlerPid, AppGluePid, STAlive)
    end.

%%--------------------------------------------------------------------
%% @spec    (Status, Response) ->
%%            {Result, BestLocation, BestResponse}
%%
%%            Status   = integer() "SIP status code"
%%            Response = #response{} | {Status, Reason}
%%            Reason   = string() "SIP reason phrase"
%%
%%            Result       = success     |
%%                           busy        |
%%                           noanswer    |
%%                           redirection |
%%                           failure
%%            BestLocation = #sipurl{} | none
%%            BestResponse = #response{} | {Status, Reason}
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%
%% @doc     Make a test_proxy_destinations() return tuple out of what
%%          appserver_glue resulted in.
%% @end
%%--------------------------------------------------------------------
make_proxy_destinations_response(Status, Response)
  when is_integer(Status), Status >= 200, Status =< 299 ->
    {success, none, Response};
make_proxy_destinations_response(Status, Response)
  when is_integer(Status), Status >= 300, Status =< 399, is_record(Response, response) ->
    [Contact | _MoreContacts] = sipheader:contact(Response#response.header),
    {redirection, sipurl:parse(Contact#contact.urlstr), Response};
make_proxy_destinations_response(408, Response) ->
    {noanswer, none, Response};
make_proxy_destinations_response(486, Response) ->
    {busy, none, Response};
make_proxy_destinations_response(487, Response) ->
    %% Request was cancelled, mask as success since there is little point in executing
    %% a fallback action (like voicemail) when caller has already hung up
    {success, none, Response};
make_proxy_destinations_response(Status, Response)
  when is_integer(Status), Status >= 300, Status =< 699 ->
    {failure, none, Response}.

%%====================================================================
%% Behaviour functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Request, Field) ->
%%            {DisplayName, URI}
%%
%%            DisplayName = none | string()
%%            URI         = #sipurl{}
%%
%% @doc     retrieves a address as specified by the field attribute in
%%          the address-switch tag
%% @end
%%--------------------------------------------------------------------
get_address_switch_uri(Request, Field) when is_record(Request, request) ->
    %% "For SIP, the "origin" address corresponds to the address in the
    %%  "From" header, "destination" corresponds to the "Request-URI", and
    %%  "original-destination" corresponds to the "To" header."
    %% - RFC 3880 chapter 4.1.1 p10
    case Field of
	origin -> sipheader:from(Request#request.header);
	destination -> {none, Request#request.uri};
	'original-destination' -> sipheader:to(Request#request.header)
    end.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback Note : moved test cases to
%%          xml_parse_test, to keep file size manageable
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->

    %% is_subdomain2/2
    %%--------------------------------------------------------------------
    %% Str = SubStr
    autotest:mark(?LINE, "is_subdomain2/2  - 1"),
    true = is_subdomain2("foo.bar.com", "foo.bar.com"),

    %% SubStr is proper substring
    autotest:mark(?LINE, "is_subdomain2/2  - 2"),
    true = is_subdomain2("foo.bar.com", "bar.com"),

    %% SubStr is proper substring
    autotest:mark(?LINE, "is_subdomain2/2  - 3"),
    true = is_subdomain2("foo.bar.com", "com"),

    %% SubStr is = "" - i.e. cant be a domain name
    autotest:mark(?LINE, "is_subdomain2/2  - 4"),
    false = is_subdomain2("foo.bar.com", ""),

    %% SubStr is broken domain name
    autotest:mark(?LINE, "is_subdomain2/2  - 5"),
    false = is_subdomain2("foo.bar.com", "o.bar.com"),

    %% SubStr is broken domain name
    autotest:mark(?LINE, "is_subdomain2/2  - 6"),
    false = is_subdomain2("foo.bar.com", ".com"),

    %% is_true/1
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_success/1  - 1"),
    true = is_true(fun() -> true end),

    autotest:mark(?LINE, "is_success/1  - 2"),
    false = is_true(fun() -> throw({error, foobar}) end),

    autotest:mark(?LINE, "is_success/1  - 3"),
    false = is_true(fun() -> "foo" end),


    %% checktype/1
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "checktype/1  - 1"),
    domain_name = checktype("foo.bar.com"),

    autotest:mark(?LINE, "checktype/1  - 2"),
    ip4addr = checktype("1.1.1.1"),

    autotest:mark(?LINE, "checktype/1  - 3"),
    ip6addr = checktype("1:1:1:1:2:2:2:2"),

    autotest:mark(?LINE, "checktype/1  - 4"),
    autotest:fail(fun() -> checktype("1:1:1:1:xxx:2:2:2") end),

    %% is_subdomain/2
    %%--------------------------------------------------------------------
    %% is_subdomain(SubHostVal, ScriptVal)

    %% SubHostVal = ScriptVal
    autotest:mark(?LINE, "is_subdomain/2  - 1"),
    true = is_subdomain("foo.bar.com", "foo.bar.com"),

    %% SubHostVal part of ScriptVal
    autotest:mark(?LINE, "is_subdomain/2  - 2"),
    true = is_subdomain("foo.bar.com", "com"),

    %% SubHostVal NOT suffix part of ScriptVal
    autotest:mark(?LINE, "is_subdomain/2  - 3"),
    false = is_subdomain("foo.bar.com", "foo.bar"),

    %% IPv4 = IPv4
    autotest:mark(?LINE, "is_subdomain/2  - 4"),
    true = is_subdomain("1.1.1.1", "1.1.1.1"),

    %% IPv4 NOT = IPv4
    autotest:mark(?LINE, "is_subdomain/2  - 5"),
    false = is_subdomain("1.2.1.1", "1.1.1.1"),


    %% prefix_match/2
    %%--------------------------------------------------------------------
    %% LangPrefix = Lang
    autotest:mark(?LINE, "prefix_match/2  - 1"),
    true = prefix_match("en", "en"),

    %% prefix matches
    autotest:mark(?LINE, "prefix_match/2  - 2"),
    true = prefix_match("en", "en-gb"),

    %% test multi part language code
    autotest:mark(?LINE, "prefix_match/2  - 3"),
    true = prefix_match("en", "en-gb-LonDon"),

    %% test multi part prefix & pattern (test case insesetive handling)
    autotest:mark(?LINE, "prefix_match/2  - 4"),
    true = prefix_match("en-GB", "en-gb-LonDon"),

    %% test multi part prefix & pattern (test case insesetive handling)
    autotest:mark(?LINE, "prefix_match/2  - 5"),
    true = prefix_match("En-gB-lONdON", "en-gb-LonDon"),

    %% parse_accept_language/1
    %%--------------------------------------------------------------------
    %%
    autotest:mark(?LINE, "parse_accept_language/1  - 1"),
    {1.0, "da"} = parse_accept_language("da"),

    autotest:mark(?LINE, "parse_accept_language/1  - 2"),
    {0.8, "da"} = parse_accept_language("da;q=0.8"),

    autotest:mark(?LINE, "parse_accept_language/1  - 3"),
    {0.8, "da"} = parse_accept_language("   da   ;  q  =  0.8"),

    autotest:mark(?LINE, "parse_accept_language/1  - 4"),
    autotest:fail(fun() -> parse_accept_language("daq=0.8") end),

    autotest:mark(?LINE, "parse_accept_language/1  - 5"),
    autotest:fail(fun() -> parse_accept_language("da;q0.8") end),

    autotest:mark(?LINE, "parse_accept_language/1  - 6"),
    autotest:fail(fun() -> parse_accept_language("da;q=") end),

    autotest:mark(?LINE, "parse_accept_language/1  - 7"),
    autotest:fail(fun() -> parse_accept_language("da;b=0.8") end),

    autotest:mark(?LINE, "parse_accept_language/1  - 8"),
    autotest:fail(fun() -> parse_accept_language("daq0.8") end),

    autotest:mark(?LINE, "parse_accept_language/1  - 9"),
    autotest:fail(fun() -> parse_accept_language(";q=0.8") end),

    autotest:mark(?LINE, "parse_accept_language/1  - 10"),
    autotest:fail(fun() -> parse_accept_language("da;q0.8") end),

    autotest:mark(?LINE, "parse_accept_language/1  - 11"),
    autotest:fail(fun() -> parse_accept_language("da;0.8") end),

    %% sort_languages_in_q_order/1
    %%--------------------------------------------------------------------
    %%
    autotest:mark(?LINE, "sort_languages_in_q_order/1  - 1"),
    Res1 = sort_languages_in_q_order(["da", "en-gb;q=0.8", "en;q=0.7"]),
    %%io:format("Res1 = ~p~n", [Res1]),
    ["da", "en-gb", "en"] = Res1,

    autotest:mark(?LINE, "sort_languages_in_q_order/1  - 2"),
    Res2 = sort_languages_in_q_order(["da", "en-gb", "en"]),
    %%io:format("Res2 = ~p~n", [Res2]),
    ["da", "en-gb", "en"] = Res2,

    autotest:mark(?LINE, "sort_languages_in_q_order/1  - 3"),
    Res3 = sort_languages_in_q_order(["en", "en-gb", "da"]),
    %%io:format("Res3 = ~p~n", [Res3]),
    ["en", "en-gb", "da"] = Res3,

    autotest:mark(?LINE, "sort_languages_in_q_order/1  - 4"),
    Res4 = sort_languages_in_q_order(["da;q=0", "en-gb;q=0.5", "en;q=1"]),
    %%io:format("Res3 = ~p~n", [Res4]),
    ["en", "en-gb"] = Res4,

    autotest:mark(?LINE, "sort_languages_in_q_order/1  - 5"),
    [] = sort_languages_in_q_order([]),

    %% language_matches/2
    %%--------------------------------------------------------------------
    %%
    autotest:mark(?LINE, "language_matches/2  - 1"),
    true = language_matches(sort_languages_in_q_order(["da", "en-gb;q=0.8", "en;q=0.7"]), "da"),

    autotest:mark(?LINE, "language_matches/2  - 2"),
    true = language_matches(sort_languages_in_q_order(["da", "en-gb;q=0.8", "en;q=0.7"]), "en-gb"),

    autotest:mark(?LINE, "language_matches/2  - 3"),
    true = language_matches(sort_languages_in_q_order(["da", "en-gb;q=0.8", "en;q=0.7"]), "en"),

    autotest:mark(?LINE, "language_matches/2  - 4"),
    false = language_matches(sort_languages_in_q_order(["da", "en-gb;q=0.8", "en;q=0.7"]), "se"),


    ok.
