%% A "quick and dirty" parser that should accept _most_ correct input
%% (except for the absoluteURI rule and some odd cases) but which will
%% also parse some malformed strings as if they where correctly
%% written (e.g. handling of CR,LF,SP and HTAB is not as strict, as it
%% should be).
%%
%% Record-Route and Route appear to have almost the same grammer
%% as Contact - they are a subset of the Contact BNF rules:
%%
%% Route         =  "Route" HCOLON rec *(COMMA rec)
%% Record-Route  =  "Record-Route" HCOLON rec *(COMMA rec)
%% rec           =  name-addr *( SEMI generic-param )
%%
%% (the rules above have been modified, to make the similarities
%% more apparent - they still parse the same garmmar)
%% So contact.erl should be able to parse these headers as well,
%% although it will acept some route and record-route entries that
%% aren't legal as if they where.
%%
%% Note: string:tokens/2 is used in some places where sequences of
%%       separator char() are not allowed, but tokens/2 will only "see"
%%       one separator char().
%% Note: contents of quoted-string are not checked
%%       - there is no need to, it should just be passed along
%%         according to RFC 3261
%% Note: SIP/SIPS-URIs are not parsed into sipurl records, this
%%       has been done to reduce the amount of parsing needed to be
%%       done and must therefor be done later - if needed
%% Note: RFC is inconsistent about ";" handling, the BNF and
%%       chapter 20.10 contradict each other.
%%
%% Opinion (hsten): ";" handling is a mess, the BNF is basicly broken,
%% and inconsistent with it's intended usage - so I had to scrap
%% some of my BNF implementing code - sigh :(
%%
%% contact-params properties - when implementing contact_param.erl:
%% * do they use any hex escape codes ?
%% - no.
%% * contact_param.erl requires unique parameter names, should it
%%   do so ?
%% - XXX
%% * contact_param.erl lowercases name-value fields, is this ok for
%%   contact-params ?
%% - yes, this is default according to the BNF chapter, as I could
%%   find no statement to the contrary this presumably applies to
%%   contact-params as well.
%% * should char() ranges used by contact-params be checked for
%%   correctness, before insertion ?
%% - XXX probably not needed, but a parser of a value part in a
%%   name-value pair may need to.
%%--------------------------------------------------------------------

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

%% RFC 2234 and RFC 3261 - - - - - - - - - - - - - - - - - - - - - - -
%% The most important rules, for the "Contact" header:
%%
%% alphanum    =  ALPHA / DIGIT
%%
%% reserved    =  ";" / "/" / "?" / ":" / "@" / "&" / "=" / "+"
%%                / "$" / ","
%% unreserved  =  alphanum / mark
%% mark        =  "-" / "_" / "." / "!" / "~" / "*" / "'"
%%                / "(" / ")"
%% escaped     =  "%" HEXDIG HEXDIG
%%
%% HTAB        =  %x09     ; horizontal tab
%% LF          =  %x0A     ; linefeed
%% CR          =  %x0D     ; carriage return
%% OCTET       =  %x00-FF  ; 8 bits of data
%% SP          =  %x20
%% DQUOTE      =  %x22     ; " (Double Quote)
%%
%% WSP         =  SP / HTAB
%% LWS         =  [*WSP CRLF] 1*WSP ; linear whitespace
%% SWS         =  [LWS] ; sep whitespace
%%
%% HCOLON      =  *( SP / HTAB ) ":" SWS
%%
%% SIP-message =  Request / Response
%% Request     =  Request-Line
%%                *( message-header )
%%                CRLF
%%                [ message-body ]
%% Request-Line =  Method SP Request-URI SP SIP-Version CRLF
%% Request-URI  =  SIP-URI / SIPS-URI / absoluteURI
%%
%% SIP-Version  =  "SIP" "/" 1*DIGIT "." 1*DIGIT
%%
%% message-header = (Accept
%%       ...
%%       /  Contact
%%       ...
%%       /  extension-header) CRLF
%%
%% message-body  =  *OCTET
%%
%% Contact       =  ("Contact" / "m" ) HCOLON
%%                  ( STAR / (contact-param *(COMMA contact-param)))
%% contact-param =  (name-addr / addr-spec) *(SEMI contact-params)
%%
%% --- The rule above is rather ambiguous, in the
%% "addr-spec *(SEMI contact-params)" case.
%% "Contact: sip:foo@bar;foo=42;bar=52" could be interpreted as:
%%
%% "Contact: <sip:foo@bar>;foo=42;bar=52" or
%% "Contact: <sip:foo@bar;foo=42>;bar=52" or
%% "Contact: <sip:foo@bar;foo=42;bar=52>"
%%
%% the basic "consume as much as possible" rule for parsers implies:
%% "Contact: <sip:foo@bar;foo=42;bar=52>"
%%
%% RFC 3261 chapter 20.10 says: "If no "<" and ">" are present, all
%% parameters after the URI are (Contact) header parameters, not
%% URI parameters." so the interpretation should be:
%%
%% "Contact: <sip:foo@bar>;foo=42;bar=52"
%% ---
%%
%% name-addr     =  [ display-name ] LAQUOT addr-spec RAQUOT
%%
%% --- this is the same as:
%% [ display-name ] SWS "<" (SIP-URI / SIPS-URI / absoluteURI) ">" SWS
%% ---
%%
%% addr-spec     =  SIP-URI / SIPS-URI / absoluteURI
%% display-name  =  *(token LWS)/ quoted-string
%%
%% contact-params =  c-p-q /          <-- redundant ?
%%                   c-p-expires /    <-- redundant ?
%%                   contact-extension
%% c-p-q          =  "q" EQUAL qvalue
%% qvalue         =  ( "0" [ "." 0*3DIGIT ] )
%%                   / ( "1" [ "." 0*3("0") ] )
%% c-p-expires    =  "expires" EQUAL delta-seconds
%% delta-seconds  =  1*DIGIT
%% contact-extension  =  generic-param
%% generic-param  =  token [ EQUAL gen-value ]
%% gen-value      =  token / host / quoted-string
%% host           =  hostname / IPv4address / IPv6reference     <-- use sipurl code
%%
%% EQUAL   =  SWS "=" SWS ; equal
%% STAR    =  SWS "*" SWS ; asterisk
%% COMMA   =  SWS "," SWS ; comma
%% SEMI    =  SWS ";" SWS ; semicolon
%% RAQUOT  =  ">" SWS ; right angle quote
%% LAQUOT  =  SWS "<"; left angle quote
%%
%% token  =  1*(alphanum / "-" / "." / "!" / "%" / "*"
%%           / "_" / "+" / "`" / "'" / "~" )
%%
%% quoted-string  =  SWS DQUOTE *(qdtext / quoted-pair ) DQUOTE
%% qdtext         =  LWS / %x21 / %x23-5B / %x5D-7E
%%                   / UTF8-NONASCII
%% quoted-pair    =  "\" (%x00-09 / %x0B-0C / %x0E-7F)
%% UTF8-NONASCII  =  %xC0-DF 1UTF8-CONT       <--- XXX nUTF8-CONT rules ?
%%                /  %xE0-EF 2UTF8-CONT
%%                /  %xF0-F7 3UTF8-CONT
%%                /  %xF8-Fb 4UTF8-CONT
%%                /  %xFC-FD 5UTF8-CONT
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
-module(contact).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 parse/1,
	 print/1,
	 new/1,
	 new/2,
	 new/3,

	 add_param/3,
	 rm_param/2,
	 set_display_name/2,
	 set_urlstr/2,

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

-define(SP, 16#20).
-define(HTAB, 16#09).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: parse(Contacts)
%%           Contacts = string(), "contact1, concact2" or just "c1"
%% Descrip.: Parse header data from a request that uses the same
%%           (or nearly the same) grammar as "Contact". This include
%%           "From", "To", "Route" and "Record-Route".
%% Returns : list() of contact record() | {unparseable, Reason}
%%--------------------------------------------------------------------
parse(Contacts) when is_list(Contacts) ->
    %% throw({unparseable, Str}) if parsing failed
    case catch [parse_star(Contact) || Contact <- Contacts] of
	%% parse error - detected by yxa code or Erlang match operations
	{error, Reason} ->
	    {unparseable, Reason};
	{'EXIT', Reason} ->
	    {unparseable, Reason};

	%% parsed data
	ResLists ->
	    lists:append(ResLists)
    end.

%% Rule: Contact
%% Contact = string without preceding or trailing whitespaces
%% return: list() of contact record()
parse_star(Contact) ->
    %% check for "*" otherwise parse as regular contacts
    case Contact of
	[$*] ->
	    [#contact{display_name = none, urlstr = "*", contact_param = contact_param:to_norm([])}];
	[$*, _ | _] ->
	    throw({error, star_got_contact_params});
	Contact ->
 	    [parse_contact(Contact)]
    end.


%% Rule: contact-param
%% Check for "addr ;param ;..." or "display_name <addr> ;param ;..."
%% (Note that display_name is optional)
%% Code assumes that ";" isn't used as anything but a parameter separator inside the contact-param rule
parse_contact(Str) ->
    StrippedStr = sipparse_util:strip(Str, both, [?SP, ?HTAB]),

    {Set_DisplayName, Set_UrlStr, Set_Params} =
	%% look for quoted display-name
	try sipparse_util:split_quoted_string(StrippedStr) of
	    {ok, DisplayNameStr, Rest} ->
		DisplayName = case DisplayNameStr of
				  "" -> none;
				  _ -> DisplayNameStr
			      end,
		%% Ok, display-name part finished, now look if there are any parameters

		{URLstr1, Params} = parse_addr_and_param(Rest),
		{DisplayName, URLstr1, Params}
	catch
	    error: does_not_start_with_quote ->
		parse_contact_no_quoted_displayname(StrippedStr)
	end,

    %% process the Params and build the final contact record
    #contact{
	  display_name = Set_DisplayName,
	  urlstr = Set_UrlStr,
	  contact_param = parse_params(Set_Params)
	 }.

%% Returns : {DisplayName, URLstr, Params}
%%           DisplayName = string() | none
%%           URLstr      = string(), URL as string, without enclosing <>
%%           Params      = string(), contact parameters
parse_contact_no_quoted_displayname(StrippedStr) ->
    %% If there is a display-name there, it isn't quoted
    case catch sipparse_util:split_fields(StrippedStr, $<) of
	{DisplayNameStr, Rest} when is_list(DisplayNameStr), is_list(Rest) ->
	    DisplayName = sipparse_util:strip(DisplayNameStr, right, [?SP, ?HTAB]),
	    %% Verify that un-quoted DisplayName is a valid token. We are extra lenient
	    %% here, and allow spaces too (since it is a common error to put
	    %% Firstname Lastname in contacts without quotes). What is really important
	    %% is to not be fooled by contacts such as
	    %%   "Foo <sip:bar@example.org> <sip:other@example.org">
	    DisplayNameNoSpaces = lists:append(string:tokens(DisplayName, " ")),
	    case sipparse_util:is_token(DisplayNameNoSpaces) of
		true -> ok;
		false -> throw({error, {unquoted_displayname_is_not_a_valid_token, DisplayName}})
	    end,
	    {URLstr1, Params} = parse_addr_and_param("<" ++ Rest),
	    {DisplayName, URLstr1, Params};
	{_Foo} ->
	    %% No "<" in StrippedStr, look for parameters (everything after ";")
	    {AddrSpec, Params} =
		case sipparse_util:split_fields(StrippedStr, $;) of
		    {AddrSpec2, Params2} ->
			{sipparse_util:strip(AddrSpec2, right, [?SP, ?HTAB]),
			 Params2};
		    {AddrSpec2} ->
			{sipparse_util:strip(AddrSpec2, right, [?SP, ?HTAB]),
			 ""}
		end,
	    %% Since we did not have even a "<" to tell us where the addr-spec
	    %% started, we verify that what we found is a parseable SIP URI
	    case sipurl:parse(AddrSpec) of
		URI when is_record(URI, sipurl) -> ok;
		_ -> throw({error, {unparseable_uri_without_brackets, AddrSpec}})
	    end,
	    {none, AddrSpec, Params};
	{error, no_first_part} ->
	    %% Nothing found before the "<"
	    {URLstr1, Params} = parse_addr_and_param(StrippedStr),
	    {none, URLstr1, Params}
    end.

%% Returns : {URLstr, Params}
%%           URLstr = string()
%%           Params = string()
parse_addr_and_param(In) ->
    Stripped = sipparse_util:strip(In, both, [?SP, ?HTAB]),
    case Stripped of
	"<" ++ Rest ->
	    %% There is a "<" after the display-name, split at the ">" that we
	    %% can expect to be there (or crash on if it isn't)
	    try sipparse_util:split_fields(Rest, 62) of	%% 62 is ">"
		{AddrSpec, Params} ->
		    {sipparse_util:strip(AddrSpec, both, [?SP, ?HTAB]),
		     sipparse_util:strip(Params, both, [?SP, ?HTAB])};
		{AddrSpec} ->
		    {sipparse_util:strip(AddrSpec, both, [?SP, ?HTAB]),
		     ""}
	    catch
		throw: {error, no_second_part} ->
		    %% ">" was the last character of Rest, not an error
		    AddrSpec = string:strip(Rest, right, $>),
		    {sipparse_util:strip(AddrSpec, both, [?SP, ?HTAB]),
		     ""}
	    end;
	_ ->
	    %% No "<", Stripped is an addr-spec
	    case sipparse_util:split_fields(Stripped, $;) of
		{AddrSpec, Params2} ->
		    {sipparse_util:strip(AddrSpec, right, [?SP, ?HTAB]),
		     Params2};
		{AddrSpec} ->
		    {sipparse_util:strip(AddrSpec, right, [?SP, ?HTAB]),
		     ""}
	    end
    end.


%% input = data after first ";"
%% return: contact_param record()
%%
parse_params([]) ->
    contact_param:to_norm([]);
parse_params(ParamsStr) ->
    Params = string:tokens(ParamsStr,";"),
    NameValList = [parse_param(Param) || Param <- Params],
    contact_param:to_norm(NameValList).

%% return: {NameStr, ValueStr} | throw()
parse_param(ParamStr) ->
    {Name, ValueStripped} =
	case sipparse_util:split_fields(ParamStr, $=) of
	    {Name2, Value} ->
		VS = sipparse_util:strip(Value, both, [?SP,?HTAB]),
		{Name2, VS};
	    {Name2} ->
		{Name2, ""}
	end,
    NameStripped = sipparse_util:strip(Name, both, [?SP,?HTAB]),

    ParamName = httpd_util:to_lower(NameStripped),

    case ParamName of
	"expires" ->
	    %% check that delta-seconds is an integer
	    try list_to_integer(ValueStripped) of
		_ -> {NameStripped, ValueStripped}
	    catch
		_: _ ->
		    throw({error, {malformed_expires, ValueStripped}})
	    end;
	"q" ->
	    %% this conversion is done to ensure that the q value
	    %% can be converted with list_to_float/1 ("0", "1",
	    %% "0." and "1." will result in exceptions)
	    QVal = q_value(ValueStripped),
	    {NameStripped, QVal};
	_OtherParam ->
	    case sipparse_util:is_token(NameStripped)
		andalso
		(sipparse_util:is_token(ValueStripped)
		 orelse is_quoted(ValueStripped)
		 %% addr-spec is a valid contact-param according to the BNF
		 orelse is_addr_spec(ValueStripped)
		) of
		true ->
		    {NameStripped, ValueStripped};
		false ->
		    throw({error, {invalid_contact_param, ParamStr}})
	    end
    end.

%% Descrip.: Check if In is either a hostname, an IPv4 or an IPv6 address
%% Returns : true | false
is_addr_spec(In) ->
    try begin
	    sipparse_util:is_hostname(In) orelse
		sipparse_util:is_IPv4address(In) orelse
		sipparse_util:is_IPv6reference(In) orelse
		is_addr_spec2(In)
	end
    catch
	_ : _ ->
	    false
    end.

%% part of is_addr_spec/1 - check if In is "[v6-reference]"
is_addr_spec2(In) ->
    case In of
	[91 | Rest] ->	%% 91 is "["
	    case catch string:tokens(Rest, "]") of
		[Check] ->
		    sipparse_util:is_IPv6reference(Check);
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

%% return: string() (of "x.xxx" format, that can be turned into a float() with list_to_float/1) |
%%         throw()
q_value(Str) ->
    %% qvalue = 0 - 1 | 0.000 - 1.000 | 0. - 1.
    QVal = string:strip(Str, right, $.),
    case sipparse_util:is_qval(QVal) of
	true ->
	    Float = sipparse_util:str_to_qval(QVal),
	    lists:flatten(io_lib:format("~.3f",[Float]));
	false ->
	    throw({error, {malformed_qvalue, Str}})
    end.

%% return: true if first and last char is a $" - strings of length < 2 -> false as well
%% Note: does not check data inside quotes
is_quoted([34 | R]) ->	%% 34 is '"'
    case lists:reverse(R) of
	[34 | _] -> true;	%% 34 is '"'
	_ -> false
    end;
is_quoted(_Str) ->
    false.


%%--------------------------------------------------------------------
%% Function: print(Contact)
%%           print(Contacts)
%%           Contact = contact record()
%%           Contacts = list() of contact record()
%% Descrip.:
%% Returns : string()
%%--------------------------------------------------------------------
print([]) ->
    "";

print([Contact]) when is_record(Contact, contact) ->
    contact:print(Contact);

print([Contact | R]) when is_record(Contact, contact) ->
    contact:print(Contact) ++ ", " ++ contact:print(R);

%% print single contact record()
print(Contact) when is_record(Contact, contact) ->
    DispName = case Contact#contact.display_name of
		   none -> "";
		   Name -> io_lib:format("~p ", [Name]) % this adds "..." around the name
	       end,
    SipURI = case Contact#contact.urlstr of
		 "*" -> "*";
		 %% URI -> "<" ++ sipurl:print(URI) ++ ">"
		 URI -> "<" ++ URI ++ ">"
	     end,
    ContactParams = Contact#contact.contact_param,

    lists:flatten(DispName ++ SipURI ++ contact_param:to_string(ContactParams)).

%%--------------------------------------------------------------------
%% Function: new(DisplayName, SipURI, Params)
%%           DisplayName = none | string()
%%           SipURI = "*" | sipurl string() | sipurl record()
%%           Params = list() of {Name, Val},
%%           Name, Val = string()
%% Descrip.: create a contact record(), should be used to ensure
%%           proper handling of internal data
%% Returns : contact record()
%%--------------------------------------------------------------------
new(SipURI) when is_list(SipURI); is_record(SipURI, sipurl) ->
    new(none, SipURI, []).

new(SipURI, Params) when is_list(SipURI); is_record(SipURI, sipurl), is_list(Params) ->
    new(none, SipURI, Params).

new(DisplayName, URL, Params) when is_record(URL, sipurl) ->
    new(DisplayName, sipurl:print(URL), Params);

new(DisplayName, [$< | Rest], Params) ->
    %% UrlStr has < as first char - it shouldn't
    erlang:fault("contact:new failed, urlstr should be without <>",
		 [DisplayName, [$<, Rest], Params]);

new(DisplayName, UrlStr, Params) when is_list(DisplayName); DisplayName == none,
				      is_list(UrlStr), is_list(Params) ->
    #contact{
								display_name = DisplayName,
								urlstr = UrlStr,
								contact_param = contact_param:to_norm(Params)
							       }.


%%--------------------------------------------------------------------
%% Function: add_param(Contact, Key, Val)
%%           Contact  = contact record()
%%           Key, Val = string()
%% Descrip.: add a contact-parameter entry
%% Returns : contact record()
%%--------------------------------------------------------------------
add_param(Contact, Key, Val) ->
    Param = Contact#contact.contact_param,
    NewParam = contact_param:add(Param, Key, Val),
    Contact#contact{ contact_param = NewParam }.

%%--------------------------------------------------------------------
%% Function: rm_param(Contact, Key)
%%           Contact = contact record()
%%           Key     = string()
%% Descrip.: remove a contact-parameter entry
%% Returns : contact record()
%%--------------------------------------------------------------------
rm_param(Contact, Key) ->
    Param = Contact#contact.contact_param,
    NewParam = contact_param:remove(Param, Key),
    Contact#contact{ contact_param = NewParam }.

%%--------------------------------------------------------------------
%% Function: set_display_name(Contact, DispName)
%%           Contact = contact record()
%%           DispName = string()
%% Descrip.: change the display name of Contact
%% Returns : contact record()
%%--------------------------------------------------------------------
set_display_name(Contact, DispName) ->
    Contact#contact{display_name = DispName}.

%%--------------------------------------------------------------------
%% Function: set_urlstr(Contact, URLstr)
%%           set_urlstr(Contact, URI)
%%           Contact = contact record()
%%           URLstr  = string()
%%           SipURL  = sipurl record()
%% Descrip.: change the sipurl contained in Contact
%% Returns : contact record()
%%--------------------------------------------------------------------
set_urlstr(Contact, SipURL) when is_record(SipURL, sipurl) ->
    Contact#contact{urlstr = sipurl:print(SipURL)};

set_urlstr(Contact, URLstr) when is_list(URLstr) ->
    Contact#contact{urlstr = URLstr}.



%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok | throw()
%%--------------------------------------------------------------------
test() ->

    %% parse
    %%--------------------------------------------------------------------
    %% test "Contact: *"
    io:format("test: parse/1 - 1~n"),
    P1 = [#contact{display_name = none,
		   urlstr = "*",
		   contact_param = contact_param:to_norm([])
		  }],
    P1 = parse(["*"]),

    %% test single name-addr contact entry
    io:format("test: parse/1 - 2~n"),
    P2 = [#contact{display_name = none,
		   urlstr = "sip:alice@pc33.atlanta.com",
		   contact_param = contact_param:to_norm([])
		  }],
    P2 = parse(["<sip:alice@pc33.atlanta.com>"]),

    %% test name_addr with display-name
    io:format("test: parse/1 - 5~n"),
    P5 = [#contact{display_name = "Mr. Watson",
		   urlstr = "sip:watson@worcester.bell-telephone.com",
		   contact_param = contact_param:to_norm([])
		  }],
    P5 = parse(["\"Mr. Watson\" <sip:watson@worcester.bell-telephone.com>"]),

    %% test addr_spec
    io:format("test: parse/1 - 6~n"),
    P6 = [#contact{display_name = none,
		   urlstr = "sip:watson@worcester.bell-telephone.com",
		   contact_param = contact_param:to_norm([])
		  }],
    P6 = parse(["sip:watson@worcester.bell-telephone.com"]),

    %% test addr_spec with contact-params, test generic-param rule
    io:format("test: parse/1 - 8~n"),
    P8 = [#contact{display_name = none,
		   urlstr = "sip:watson@worcester.bell-telephone.com",
		   contact_param = contact_param:to_norm([{"foo","bar"},{"zoo","123"}])
		  }],
    P8 = parse(["sip:watson@worcester.bell-telephone.com; foo = bar;zoo = 123"]),

    %% test addr_spec with contact-params, test "q" and "expires"
    %% as well as IPv4address, hostname and IPv6reference rules
    io:format("test: parse/1 - 9~n"),
    P9 = [#contact{display_name = none,
		   urlstr = "sip:watson@worcester.bell-telephone.com",
		   contact_param = contact_param:to_norm([{"q","1.000"}, {"expires","123456"}, {"host","1.2.3.4"},
							  {"domain","www.com"}, {"ipv6","[1:2:3:4:5:6:7:8]"}])
		  }],
    P9 = parse(["sip:watson@worcester.bell-telephone.com;q = 1.; "
		"expires=123456;host = 1.2.3.4;domain    = www.com;"
		"ipv6=[1:2:3:4:5:6:7:8]"]),

    %% test name_addr with contact-params
    io:format("test: parse/1 - 10~n"),
    P10 = [#contact{display_name = "Mr. Watson",
		    urlstr = "sip:watson@worcester.bell-telephone.com",
		    contact_param = contact_param:to_norm([{"q","0.000"}, {"expires","123456"}, {"host","1.2.3.4"},
							   {"domain","www.com"}, {"ipv6","[1:2:3:4:5:6:7:8]"}])
		   }],
    P10 = parse(["\"Mr. Watson\"<sip:watson@worcester.bell-telephone.com>; q = 0; "
		 "expires=123456    ;host =1.2.3.4   ;   domain=www.com;"
		 "    ipv6=[1:2:3:4:5:6:7:8]"]),

    %% test strange Contact and DisplayName using token rule
    io:format("test: parse/1 - 11~n"),
    P11 = [#contact{display_name = "Watson-.!%*_+`'~",
		    urlstr = "sip:watson@worcester.bell-telephone.com",
		    contact_param = contact_param:to_norm([{"q","0.950"}, {"expires","123456"}, {"host","1.2.3.4"},
							   {"domain","www.com"}, {"ipv6","[1:2:3:4:5:6:7:8]"}])
		   }],
    P11 = parse(["Watson-.!%*_+`'~" ++ [?HTAB] ++
		 "<sip:watson@worcester.bell-telephone.com>; q = 0.95; " ++
		 "expires=123456    ;host =1.2.3.4   ;   domain=www.com;" ++
		 [?HTAB] ++ "    ipv6=[1:2:3:4:5:6:7:8]"]),


    %% validate correct handling of encounterd bug in old version: we LOOSE parameters outside the URI
    io:format("test: parse/1 - 12~n"),
    P12 = [#contact{display_name = none,
		    urlstr = "sip:hotsip1@130.237.252.103:5060;transport=TCP",
		    contact_param = contact_param:to_norm([{"q","1.000"},
							   {"agentid","\"6a017b68-96b1-4c3f-9513-7a7a90ad501d\""},
							   {"expires","0"}])
		   }],
    P12 = parse(["<sip:hotsip1@130.237.252.103:5060;transport=TCP>;q=1.00;"
		 "agentid=\"6a017b68-96b1-4c3f-9513-7a7a90ad501d\";expires=0"]),

    %% test uri-parameters inside a SIP-URI (name-addr)
    io:format("test: parse/1 - 13~n"),
    P13 = [#contact{display_name = "Hokan",
		    urlstr = "sip:hotsip1@130.237.252.103:5060;transport=TCP;foo;bar=42",
		    contact_param = contact_param:to_norm([{"q","1.000"}, {"expires","0"}])
		   }],
    P13 = parse(["Hokan <sip:hotsip1@130.237.252.103:5060;transport=TCP;foo;bar=42>;q=1.00;"
		 ";expires=0"]),

    %% test uri-parameters inside a SIP-URI (addr-spec)
    io:format("test: parse/1 - 14~n"),
    P14 = [#contact{display_name = none,
		    urlstr = "sip:hotsip1@130.237.252.103:5060",
		    contact_param = contact_param:to_norm([{"transport","TCP"}, {"bar","42"}, {"q","1.000"},
							   {"expires","0"}])
		   }],
    P14 = parse(["sip:hotsip1@130.237.252.103:5060;transport=TCP;bar=42"
		 " ;q=1.00;expires=0"]),

    %% test that "Contact: *;foo=bar" throws a exception (* can't have contact-params)
    io:format("test: parse/1 - 15~n"),
    case parse(["*;foo=bar"]) of
	{unparseable, _Reason} -> ok;
	_ -> throw({error, test_failed})
    end,

    %% test contact-parameters without a value
    io:format("test: parse/1 - 16~n"),
    P16 = [#contact{display_name = none,
		    urlstr = "sip:example.org",
		    contact_param = contact_param:to_norm([{"foo", none}, {"lr", "true"}, {"bar", none},
							   {"baz", none}])
		   }],
    P16 = parse(["sip:example.org;foo;lr=true;bar;baz"]),

    %% test display name with an unbalanced escaped quote in it
    io:format("test: parse/1 - 17~n"),
    P17 = [#contact{display_name = "Fredrik \\\"",
		    urlstr = "sip:example.org",
		    contact_param = contact_param:to_norm([])
		   }],
    P17 = parse(["\"Fredrik \\\"\" sip:example.org"]),

    %% test display name with <> and a fake parameter delimeter in it.
    io:format("test: parse/1 - 18~n"),
    P18 = [#contact{display_name = "Fredrik <X>;notparam=foo Y",
		    urlstr = "sip:example.org",
		    contact_param = contact_param:to_norm([])
		   }],
    P18 = parse(["\"Fredrik <X>;notparam=foo Y\" sip:example.org"]),

    %% test display name without quotes, that really should have quotes
    io:format("test: parse/1 - 19~n"),
    P19 = [#contact{display_name = "Foo Bar",
		    urlstr = "sip:example.org",
		    contact_param = contact_param:to_norm([{"test", "foo"}])
		   }],
    P19 = parse(["Foo Bar <sip:example.org>;test=foo"]),

    %% test display name without quotes, that really should have quotes
    io:format("test: parse/1 - 20~n"),
    case catch parse(["Foo Bar sip:example.org"]) of
	{unparseable, {unparseable_uri_without_brackets, _}} -> ok;
	P20 -> throw({error, {test_failed, P20}})
    end,

    %% test display name without quotes, that really should have quotes
    io:format("test: parse/1 - 21~n"),
    case catch parse(["Foo|Bar <sip:example.org>"]) of
	{unparseable, {unquoted_displayname_is_not_a_valid_token, _}} -> ok;
	P21 -> throw({error, {test_failed, P21}})
    end,

    %% test with empty quoted display name
    io:format("test: parse/1 - 22~n"),
    P22 = [#contact{display_name = none,
		    urlstr = "sip:example.org",
		    contact_param = contact_param:to_norm([{"test", "foo"}])
		   }],
    P22 = parse(["\t\"\" sip:example.org;test=foo \t"]),

    %% test no display name and contact parameters
    io:format("test: parse/1 - 23~n"),
    P23 = [#contact{display_name = none,
		    urlstr = "sip:example.org",
		    contact_param = contact_param:to_norm([{"test", "foo"}])
		   }],
    P23 = parse(["sip:example.org;test=foo"]),

    %% test invalid hostname that is not quoted and not a token
    io:format("test: parse/1 - 24~n"),
    case catch parse(["<sip:example.org>;foo=|.example.org"]) of
	{unparseable, {invalid_contact_param, _}} -> ok;
	P24 -> throw({error, {test_failed, P24}})
    end,

    %% test parameters with delimeter-alike characters, interop problem (our fault) encountered
    %% with Cisco 79xx phones firmware > 7.4
    io:format("test: parse/1 - 25~n"),
    P25 = [#contact{display_name = none,
		    urlstr = "sip:ft@192.0.2.12:5060;user=ip",
		    contact_param = contact_param:to_norm([{"+sip.instance", "\"<urn:uuid:foo>\""}])
		   }],
    P25 = parse(["<sip:ft@192.0.2.12:5060;user=ip>;+sip.instance=\"<urn:uuid:foo>\""]),


    %% print/1
    %%--------------------------------------------------------------------

    %% test "Contact: *"
    io:format("test: print/1 - 1~n"),
    "*" = print(hd(parse(["*"]))),

    %% test single name-addr
    io:format("test: print/1 - 2~n"),
    "<sip:alice@pc33.atlanta.com>" = print(hd(parse(["<sip:alice@pc33.atlanta.com>"]))),

    %% test single addr-spec
    io:format("test: print/1 - 3~n"),
    "<sip:bob@192.0.2.4>" = print(hd(parse(["sip:bob@192.0.2.4"]))),

    %% test name_addr with display-name
    io:format("test: print/1 - 4~n"),
    PH4 = hd(parse(["\"Mr. Watson\" <sip:watson@worcester.bell-telephone.com>"])),
    "\"Mr. Watson\" <sip:watson@worcester.bell-telephone.com>" = print(PH4),

    %% test addr-spec with contact-params, test generic-param rule
    io:format("test: print/1 - 5~n"),
    PH5 = hd(parse(["sip:watson@worcester.bell-telephone.com; foo = bar;zoo = 123"])),
    "<sip:watson@worcester.bell-telephone.com>;foo=bar;zoo=123" = print(PH5),

    %% test addr_spec with contact-params, test "q" and "expires"
    %% as well as IPv4address, hostname and IPv6reference rules
    io:format("test: print/1 - 6~n"),
    PH6 = hd(parse(["sip:watson@worcester.bell-telephone.com;q = 1.; "
		    "expires=123456;host = 1.2.3.4;domain    = www.com;"
		    "ipv6=[1:2:3:4:5:6:7:8]"])),
    "<sip:watson@worcester.bell-telephone.com>;q=1.000;expires=123456;host=1.2.3.4;domain=www.com;"
	"ipv6=[1:2:3:4:5:6:7:8]" = print(PH6),

    %% test name_addr with contact-params and display-name
    io:format("test: print/1 - 7~n"),
    PH7 = hd(parse(["\"Mr. Watson\"<sip:watson@worcester.bell-telephone.com>; q = 0; "
		    "expires=123456    ;host =1.2.3.4   ;   domain=www.com;"
		    "    ipv6=[1:2:3:4:5:6:7:8]"])),
    "\"Mr. Watson\" <sip:watson@worcester.bell-telephone.com>;q=0.000;"
	"expires=123456;host=1.2.3.4;domain=www.com;ipv6=[1:2:3:4:5:6:7:8]" = print(PH7),

    %% test strange Contact and DisplayName
    io:format("test: print/1 - 8~n"),
    PH8 = hd(parse(["Watson-.!%*_+`'~" ++ [?HTAB] ++
		    "<sip:watson@worcester.bell-telephone.com>; q = 0.95; " ++
		    "expires=123456    ;host =1.2.3.4   ;   domain=www.com;" ++
		    [?HTAB] ++ "    ipv6=[1:2:3:4:5:6:7:8]"])),
    "\"Watson-.!%*_+`'~\" <sip:watson@worcester.bell-telephone.com>;q=0.950;"
	"expires=123456;host=1.2.3.4;domain=www.com;ipv6=[1:2:3:4:5:6:7:8]" = print(PH8),

    %% test print of encounterd bug: we LOOSE parameters outside the URI
    %% sipheader:parse_contact(["<sip:hotsip1@130.237.252.103:5060;transport=TCP>;q=1.00;"
    %%                          "agentid=\"6a017b68-96b1-4c3f-9513-7a7a90ad501d\";expires=0"]).
    io:format("test: print/1 - 9~n"),
    PH9 = hd(parse(["<sip:hotsip1@130.237.252.103:5060;transport=TCP>;q=1.00;"
		    "agentid=\"6a017b68-96b1-4c3f-9513-7a7a90ad501d\";expires=0"])),
    "<sip:hotsip1@130.237.252.103:5060;transport=TCP>;q=1.000;"
	"agentid=\"6a017b68-96b1-4c3f-9513-7a7a90ad501d\";expires=0" = print(PH9),

    %% test uri-parameters inside a SIP-URI (name-addr)
    io:format("test: print/1 - 10~n"),
    PH10 = hd(parse(["Hokan <sip:hotsip1@130.237.252.103:5060;transport=TCP;foo;bar=42>;q=1.00;"
		     ";expires=0"])),
    "\"Hokan\" <sip:hotsip1@130.237.252.103:5060;transport=TCP;foo;bar=42>;q=1.000;expires=0"
	= print(PH10),

    %% test contact-parameters after a SIP-URI (addr-spec)
    io:format("test: print/1 - 11~n"),
    PH11 = hd(parse(["sip:hotsip1@130.237.252.103:5060;transport=TCP;bar=42"
		     " ;q=1.00;expires=0"])),
    "<sip:hotsip1@130.237.252.103:5060>;transport=tcp;bar=42;q=1.000;expires=0" = print(PH11),

    %% test printing list of contacts - using a function in sipheader
    io:format("test: print/1 - 12~n"),
    PH12 = hd(parse(["<sip:alice@pc33.atlanta.com>"])),
    PH13 = hd(parse(["sip:bob@192.0.2.4"])),
    ["<sip:alice@pc33.atlanta.com>", "<sip:bob@192.0.2.4>"] = sipheader:contact_print([PH12, PH13]),

    %% test printing list of single contact
    io:format("test: print/1 - 13~n"),
    PH14 = hd(parse(["<sip:alice@pc33.atlanta.com>"])),
    "<sip:alice@pc33.atlanta.com>" = print(PH14),

    %% test printing empty list of contacts
    io:format("test: print/1 - 14~n"),
    "" = print([]),

    %% add_param/3
    %%--------------------------------------------------------------------

    Contact1 = contact_param:to_norm([]),
    Contact2 = contact_param:add(Contact1, "foo", "bar"),
    Contact3 = contact_param:add(Contact2, "zog", "42"),
    Contact4 = contact_param:add(Contact3, "boo", "42"),

    C1 = (new(none, "sip:alice@pc33.atlanta.com", []))#contact{ contact_param =  Contact1},
    C2 = (new(none, "sip:alice@pc33.atlanta.com", []))#contact{ contact_param =  Contact2},
    C3 = (new(none, "sip:alice@pc33.atlanta.com", []))#contact{ contact_param =  Contact3},
    C4 = (new(none, "sip:alice@pc33.atlanta.com", []))#contact{ contact_param =  Contact4},

    %% add to empty params
    io:format("test: add_param/3 - 1~n"),
    C2 = add_param(C1, "foo", "bar"),

    %% add to single element params
    io:format("test: add_param/3 - 2~n"),
    C3 = add_param(C2, "zog", "42"),

    %% add to single element params
    io:format("test: add_param/3 - 3~n"),
    C4 = add_param(C3, "boo", "42"),


    %% rm_param/2
    %%--------------------------------------------------------------------
    Contact1b = contact_param:to_norm([]),
    Contact2b = contact_param:add(Contact1, "foo", "bar"),
    Contact3b = contact_param:add(Contact2, "zog", "42"),
    Contact4b = contact_param:add(Contact3, "boo", "42"),

    C1b = (new(none, "sip:alice@pc33.atlanta.com", []))#contact{ contact_param =  Contact1b},
    C2b = (new(none, "sip:alice@pc33.atlanta.com", []))#contact{ contact_param =  Contact2b},
    C3b = (new(none, "sip:alice@pc33.atlanta.com", []))#contact{ contact_param =  Contact3b},
    C4b = (new(none, "sip:alice@pc33.atlanta.com", []))#contact{ contact_param =  Contact4b},

    %% remove (non-existant key) from empty param
    io:format("test: rm_param/3 - 1~n"),
    C1b = rm_param(C1b, "foo"),

    %% remove existing key
    io:format("test: rm_param/3 - 2~n"),
    C3b = rm_param(C4b, "boo"),

    %% remove last param
    io:format("test: rm_param/3 - 3~n"),
    C1b = rm_param(C2b, "foo"),


    %% set_display_name/2
    %%--------------------------------------------------------------------

    %% set_urlstr/2
    %%--------------------------------------------------------------------



    ok.



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
%% Function:
%% Descrip.:
%% Returns :
%%--------------------------------------------------------------------
