-module(sippacket).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 parse/2,
	 parse_packet/1,
	 parse_packet/2,
	 parseheader/1,
	 test/0
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
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
%% Function: parse(Packet, Origin)
%% Descrip.: Parse a packet received from the network into either a
%%           request or a response record()
%% Returns : {request, Request}   |
%%           {response, Response} |
%%           {siperror, Status, Reason}
%%           Request  = request record()
%%           Response = response record()
%%           Status   = integer(), SIP status code
%%           Reason   = string(), SIP reason phrase
%%--------------------------------------------------------------------
parse(Packet, Origin) ->
    case parse_packet(Packet, Origin) of
	keepalive ->
	    keepalive;
	{HeaderStr, BodyStr} ->
	    {FirstLine, Headerkeylist} = parseheader(HeaderStr),
	    case parse_firstline(FirstLine) of
		{request, Parsed} ->
		    %% XXX move data from SIP URI:s such as
		    %%   sips:alice@atlanta.com?subject=project%20x&priority=urgent
		    %% into Headerkeylist and strip them from the URI. See
		    %% RFC 3261 19.1.5 Forming Requests from a URI
		    request(Parsed, Headerkeylist, BodyStr);
		{response, Parsed} ->
		    response(Parsed, Headerkeylist, BodyStr);
		none ->
		    {siperror, 400, "Completely unparseable request/response"}
	    end
    end.

%%--------------------------------------------------------------------
%% Function: parse_packet(Packet, Origin) ->
%%           parse_packet(Packet)
%%           Packet = string()
%%           Origin = siporigin record() | none
%% Descrip.: 'Fix' line breaks in Packet and then find the header-
%%           body separator.
%% Returns : {HeaderStr, BodyStr} |
%%           {keepalive}
%%           HeaderStr = string()
%%           BodyStr   = string()
%%--------------------------------------------------------------------
parse_packet(Packet) ->
    parse_packet(Packet, none).

parse_packet("\r\n", Origin) ->
    case sipserver:origin2str(Origin, none) of
	none -> true;
	S ->
	    logger:log(debug, "Keep-alive packet from ~s", [S])
    end,
    keepalive;
parse_packet(Packet, Origin) ->
    case sipserver:origin2str(Origin, none) of
	none -> true;
	S ->
	    %% Extract receiver pid if present
	    RStr = case Origin of
		       _ when record(Origin, siporigin) ->
			   "(connection handler: " ++ pid_to_list(Origin#siporigin.receiver) ++ ") ";
		       _ ->
			   ""
		   end,
	    logger:log(debug, "Packet from ~s ~s:~n~s", [S, RStr, Packet])
    end,
    Packetfixed = siputil:linefix(Packet),
    case string:str(Packetfixed, "\n\n") of
	0 ->
	    {Packetfixed, ""};
	Headerlen ->
	    HeaderStr = string:substr(Packetfixed, 1, Headerlen),
	    BodyStr = string:substr(Packetfixed, Headerlen + 2),
	    {HeaderStr, BodyStr}
    end.

%%--------------------------------------------------------------------
%% Function: parseheader(Message)
%%           Message = string()
%% Descrip.: Parse from the start of a SIP message up to the header-
%%           body separator. Return the first line as it was given to
%%           us, and a created keylist of all the headers.
%% Returns : {FirstLine, Header}
%%           FirstLine = string()
%%           Header    = keylist record()
%%--------------------------------------------------------------------
parseheader(Message) ->
    [FirstLine | Rest] = string:tokens(Message, "\n"),
    Parseheader = fun(Line) ->
			  Index = string:chr(Line, $:),
			  Name = string:strip(string:substr(Line, 1, Index - 1), right),
			  Value = string:strip(string:substr(Line, Index + 1),
					       left),
			  Key = keylist:normalize(Name),
			  {Key, Name, split_header_value(Key, Value)}
		  end,
    Headerlist = lists:map(Parseheader, Rest),
    Header = keylist:from_list(Headerlist),
    {FirstLine, Header}.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: split_header_value(Key, Value)
%%           Key   = string()
%%           Value = string()
%% Descrip.: Look at Key and decide whether Value is a header that
%%           should be splitted on comma, or if it is in fact just a
%%           single value that should be returned as a single list
%%           element.
%% Returns : ValueList, list() of string() or just [Value]
%%--------------------------------------------------------------------
split_header_value(_Key, []) ->
    [];
split_header_value(Key, Value) ->
    case lists:member(Key, ['www-authenticate', authorization,
			    'proxy-authenticate', 'proxy-authorization',
			    date]) of
	true ->
	    %% Except the headers listed in RFC3261 7.3.1 and some other that needs
	    %% to be excepted from standard header comma splitting
	    [Value];
	_ ->
	    Res = sipheader:comma(Value),
	    %% Remove leading and trailing white space
	    lists:map(fun(V) ->
			      string:strip(V)
		      end, Res)
    end.

%%--------------------------------------------------------------------
%% Function: parse_firstline(FirstLine)
%% Descrip.: Figure out if this is a request or response.
%% Returns : {request, {Method, URIstr}} |
%%           {response, {Status, Reason}
%%           Method = string(), e.g. "INVITE"
%%           URIstr = string(), e.g. "sip:user@example.org"
%%           Status = string(), e.g. "100"
%%           Reason = string(), e.g. "Trying"
%%--------------------------------------------------------------------
parse_firstline(FirstLine) ->
    Index1 = string:chr(FirstLine, 32),
    F1 = string:substr(FirstLine, 1, Index1 - 1),
    Rest = string:strip(string:substr(FirstLine, Index1 + 1),
			left),
    case F1 of
	"SIP/2.0" ->
	    Index2 = string:chr(Rest, 32),
	    Status = string:substr(Rest, 1, Index2 - 1),
	    Reason = string:strip(string:substr(Rest, Index2 + 1),
				  left),
	    {response, {Status, Reason}};
	[] ->
	    none;
	Method ->
	    Index2 = string:rchr(Rest, 32),
	    URIstr = string:substr(Rest, 1, Index2 - 1),
	    "SIP/2.0" = string:strip(string:substr(Rest, Index2 + 1),
				     left),
	    {request, {Method, URIstr}}
    end.

%%--------------------------------------------------------------------
%% Function: request({Method, URI}, Header, Body)
%%           Method = string()
%%           URI    = string(), URI as string - gets parsed here
%%           Header = keylist record()
%%           Body   = string()
%% Descrip.: Put all the pieces of a request together.
%% Returns : request record()
%%--------------------------------------------------------------------
request({Method, URI}, Header, Body) ->
    URL = sipurl:parse(URI),
    #request{method=Method, uri=URL, header=Header, body=Body}.

%%--------------------------------------------------------------------
%% Function: response({Status, Reason}, Header, Body)
%%           Status = string(), gets converted to integer here
%%           Reason = string()
%%           Header = keylist record()
%%           Body   = string()
%% Descrip.: Put all the pieces of a response together.
%% Returns : response record()
%%--------------------------------------------------------------------
response({Status, Reason}, Header, Body) ->
    #response{status=list_to_integer(Status), reason=Reason, header=Header, body=Body}.


%%====================================================================
%% Test functions - belong at the bottom of the module
%%====================================================================

%%--------------------------------------------------------------------
%% Function: test()
%% Descrip.: autotest callback
%% Returns : ok
%%--------------------------------------------------------------------
test() ->
    %% parse/2
    %%--------------------------------------------------------------------
    %% init
    io:format("test: parse/1 request - 1~n"),
    Message1 = 
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TCP one.example.org\r\n"
	"\r\nbody",
    URL1 = sipurl:parse("sip:test@example.org"),

    %% REQUESTS

    %% basic parse
    io:format("test: parse/1 request - 1.1~n"),
    {request, "INVITE", URL1, Header1, "body"} = parse(Message1, none),

    %% verify single Via
    io:format("test: parse/1 request - 1.2~n"),
    ["SIP/2.0/TCP one.example.org"] = keylist:fetch('via', Header1),

    Message2 = 
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TCP two.example.org\r\n"
	"From: \"Test Test\" <sip:test@example.org>;tag=abc\r\n"
	"To: <sip:to@example.org>   \n"
	"via: SIP/2.0/UDP one.example.org\r\n"
	"\r\nbody\r\n",

    io:format("test: parse/1 request - 2.1~n"),
    %% More complex header, one faulty \r\n (end of To: line) and
    %% two Vias not located together. Also some extra spaces after To: value
    %% and extra \r\n at the end of body.
    {request, "INVITE" , _, Header2, "body\r\n"} = parse(Message2, none),

    io:format("test: parse/1 request - 2.2~n"),
    %% Verify To:
    ["<sip:to@example.org>"] = keylist:fetch('to', Header2),

    io:format("test: parse/1 request - 2.3~n"),
    %% Verify Via's
    ["SIP/2.0/TCP two.example.org", "SIP/2.0/UDP one.example.org"] = keylist:fetch('via', Header2),

    _Message3 = "INVITE sip:test@example.org   SIP/2.0\r\n",
    _URL3 = sipurl:parse("sip:test@example.org"),
    
    %% Extra spaces in request-line
    io:format("test: parse/1 request - 3 (disabled)~n"),
%%    {request, "INVITE" , URL3, _, ""} = parse(Message3, none),

    Message4 =
	"INVITE sip:test@example.org   SIP/2.0\r\n"
	"Via:SIP/2.0/TLS four.example.org\r\n"
	"Via     :     SIP/2.0/TCP three.example.org\r\n"
	"Via     :SIP/2.0/TCP two.example.org,SIP/2.0/UDP one.example.org  \r\n",

    %% Extra spaces and missing spaces
    io:format("test: parse/1 request - 4.1~n"),
    {request, "INVITE" , _, Header4, ""} = parse(Message4, none),
    
    %% verify the Via's
    io:format("test: parse/1 request - 4.2~n"),
    ["SIP/2.0/TLS four.example.org", "SIP/2.0/TCP three.example.org", "SIP/2.0/TCP two.example.org",
     "SIP/2.0/UDP one.example.org"] = keylist:fetch('via', Header4),

    _Message5 = 
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TLS three.example.org,\r\n"
	"	SIP/2.0/TCP two.example.org\r\n"
	"v: SIP/2.0/UDP one.example.org\r\n"
	"\r\n",
    io:format("test: parse/1 request - 5.1 (disabled)~n"),
    %% multiline Via
%%    {request, "INVITE" , _, Header5, ""} = parse(Message5, none),
    
    io:format("test: parse/1 request - 5.2 (disabled)~n"),
    %% verify the three Via's
%%    ["SIP/2.0/TLS three.example.org", "SIP/2.0/TCP two.example.org", "SIP/2.0/UDP one.example.org"] =
%%    keylist:fetch('via', Header5),
    
    %% This is the '3.1.1.1  A short tortuous INVITE' from draft-ietf-sipping-torture-tests-04.txt,
    %% except the body which wasn't very special
    _Message6 =
	"INVITE sip:vivekg@chair-dnrc.example.com;unknownparam SIP/2.0"
	"TO :"
	" sip:vivekg@chair-dnrc.example.com ;   tag    = 1918181833n"
	"from   : \"J Rosenberg \\\"\"       <sip:jdrosen@example.com>"
	"  ;"
	"  tag = 98asjd8"
	"MaX-fOrWaRdS: 0068"
	"Call-ID: wsinv.ndaksdj@192.0.2.1"
	"Content-Length   : 4"
	"cseq: 0009"
	"  INVITE"
	"Via  : SIP  /   2.0"
	" /UDP"
	"    192.0.2.2;branch=390skdjuw"
	"s :"
	"NewFangledHeader:   newfangled value"
	" continued newfangled value"
	"UnknownHeaderWithUnusualValue: ;;,,;;,;"
	"Content-Type: application/sdp"
	"Route:"
	" <sip:services.example.com;lr;unknownwith=value;unknown-no-value>"
	"v:  SIP  / 2.0  / TCP     spindle.example.com   ;"
	"  branch  =   z9hG4bK9ikj8  ,"
	" SIP  /    2.0   / UDP  192.168.255.111   ; branch="
	" z9hG4bK30239"
	"m:\"Quoted string \"\"\" <sip:jdrosen@example.com> ; newparam ="
	"      newvalue ;"
	"  secondparam ; q = 0.33"
	""
	"test",
    _URL6 = sipurl:parse("sip:vivekg@chair-dnrc.example.com;unknownparam"),

    io:format("test: parse/1 request - 6.1 (disabled)~n"),
    %% parse tortuous INVITE
%%    {request, "INVITE" , URL6, Header6, "test"} = parse(Message6, none),
    
    io:format("test: parse/1 request - 6.2 (disabled)~n"),
    %% verify contents


    _Message7 =
        "REGISTER sip:example.org SIP/2.0\r\n"
        "Via: SIP/2.0/TLS 192.0.2.78\r\n"
        "Content-Length: 0\r\n"
        "\r\n"
	"body"
        "INVITE sip:foo@example.com SIP/2.0\r\n"
        "this is just garbage received in the same packet\r\n",

    io:format("test: parse/1 request - 7 (disabled)~n"),
    %% extra data after request - should be ignored (Content-Length: 4)
%%    {request, "REGISTER" , _, _, "body"} = parse(Message7, none),
    
    Message8 =
         "INVITE <sip:user@example.com> SIP/2.0\r\n"
         "Via: SIP/2.0/TLS 192.0.2.1, SIP/2.0/FOO 192.0.2.78\r\n",
    io:format("test: parse/1 request - 8.1~n"),
    %% Request-URI enclosed in <> - invalid, but should not be rejected here
    %% (it isn't THAT broken and should be responded to with a 400 Bad Request) 
    {request, "INVITE" , {unparseable, "<sip:user@example.com>"}, Header8, ""} = parse(Message8, none),

    io:format("test: parse/1 request - 8.2~n"),
    %% verify the Via header so we know keylist is created as it should
    ["SIP/2.0/TLS 192.0.2.1", "SIP/2.0/FOO 192.0.2.78"] = keylist:fetch('via', Header8),

    %% RESPONSES

    RMessage1 = "SIP/2.0 100 Trying\r\n"
	"Via: SIP/2.0/TLS 192.0.2.78   \r\n",
    io:format("test: parse/1 response - 1.1~n"),
    {response, 100, "Trying", RHeader1, ""} = parse(RMessage1, none),

    %% verify the Via header so we know keylist is created as it should
    io:format("test: parse/1 response - 1.2~n"),
    ["SIP/2.0/TLS 192.0.2.78"] = keylist:fetch('via', RHeader1),
    
    RMessage2 =       
        "SIP/2.0 100 \r\n"
        "Via: SIP/2.0/TLS 192.0.2.78\r\n",
    io:format("test: parse/1 response - 2.1~n"),
    {response, 100, "", RHeader2, ""} = parse(RMessage2, none),

    %% verify the Via header so we know keylist is created as it should
    io:format("test: parse/1 response - 2.2~n"),
    ["SIP/2.0/TLS 192.0.2.78"] = keylist:fetch('via', RHeader2),
    
    %% '3.1.2.19  Overlarge response code' from draft-ietf-sipping-torture-tests-04.txt
    %% (in a shorter form) - the draft says you should just ignore responses with
    %% too large response codes, so we can do that here already.
    _RMessage3 =
	"SIP/2.0 4294967301 better not break the receiver\r\n"
        "Via: SIP/2.0/TLS 192.0.2.78\r\n",
    io:format("test: parse/1 response - 3~n"),
%%    invalid = parse(RMessage3, none),



    ok.
