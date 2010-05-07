%%%-------------------------------------------------------------------
%%% File    : sippacket.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Parses a SIP message received from the network.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sippacket).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 parse/2,

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
%% @type ptr() = #ptr{}.
%%               no description
-record(ptr, {
	  offset,	%% integer(), start offset of element
	  length	%% integer(), length of element
	 }).
%% @type header() = #header{}.
%%                  no description
-record(header, {
	  key,		%% ptr record() with data about header key
	  valueptrs,	%% list() of ptr record(), value element(s)
	  comma		%% true | false, comma present in value(s)?
	 }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(CR, 16#0D).
-define(LF, 16#0A).
-define(SP, 16#20).
-define(HTAB, 16#09).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Packet, Origin) ->
%%            Request   |
%%            Response  |
%%            keepalive 
%%
%%            Packet = binary() | string()
%%            Origin = #siporigin{} | none
%%
%%            Request  = #request{}
%%            Response = #response{}
%%            Status   = integer() "SIP status code"
%%            Reason   = string() "SIP reason phrase"
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Parse a packet received from the network into either a
%%          request or a response record()
%% @end
%%--------------------------------------------------------------------
parse(Packet, Origin) when is_binary(Packet), is_record(Origin, siporigin) orelse Origin == none ->
    case parse_packet(Packet, Origin) of
	keepalive ->
	    keepalive;
	{ok, FirstLine, Header, Rest} ->
	    CL = keylist:fetch('content-length', Header),
	    case extract_body(Rest, CL) of
		{ok, Body} ->
		    case FirstLine of
			{request, Parsed} ->
			    %% XXX move data from SIP URI:s such as
			    %%   sips:alice@atlanta.com?subject=project%20x&priority=urgent
			    %% into Header and strip them from the URI. See
			    %% RFC 3261 19.1.5 Forming Requests from a URI
			    request(Parsed, Header, Body);
			{response, Parsed} ->
			    response(Parsed, Header, Body)
		    end;
		{error, "invalid Content-Length"} ->
		    logger:log(debug, "Packet has invalid Content-Length header : ~p", [CL]),
		    throw({siperror, 400, "Invalid (or duplicate) Content-Length header"})
	    end
    end;
parse(PacketL, Origin) when is_list(PacketL) ->
    parse(list_to_binary(PacketL), Origin).

%% part of parse() - extract body using an existing Content-Length
%% or assuming that it is the rest of Bin. If we get a datagram with
%% excess data, we are to just ignore the extra data.
extract_body(Bin, [LLen]) ->
    try list_to_integer(LLen) of
	CLen ->
	    extract_body2(Bin, CLen)
    catch
	_ : _ ->
	    {error, "invalid Content-Length"}
    end;
extract_body(Bin, []) ->
    %% No Content-Length header supplied, assume the body is the rest of the packet.
    {ok, Bin};
extract_body(_Bin, CL) ->
    %% More than one Content-Length header or something else wrong with it
    logger:log(debug, "Packet has invalid Content-Length header : ~p", [CL]),
    {error, "invalid Content-Length"}.

extract_body2(Bin, CLen) when size(Bin) > CLen ->
    <<Body:CLen/binary-unit:8, Rest/binary>> = Bin,
    %% XXX should we really log the data? If so, perhaps log using
    %% special logger:log_io_list.
    logger:log(debug, "Ignoring ~p bytes of excess data (for now) : ~p",
	       [size(Bin) - CLen, binary_to_list(Rest)]),
    {ok, Body};
extract_body2(Bin, CLen) when size(Bin) < CLen ->
    %% We haven't got the whole body. For stream-oriented transport (like TCP)
    %% this is not an error - we just haven't received the complete message yet
    {ok, Bin};
extract_body2(Bin, CLen) when size(Bin) == CLen ->
    %% CLen matches size of Bin
    {ok, Bin}.

%%--------------------------------------------------------------------
%% @spec    (Packet, Origin) ->
%%            {ok, FirstLine, Header, Rest} | keepalive 
%%
%%            Packet = binary()
%%            Origin = #siporigin{} | none
%%
%%            FirstLine = {request, {Method, URIstr}}  |
%%                        {response, {Status, Reason}}
%%            Header    = #keylist{}
%%            Rest      = binary()
%%
%% @throws  {siperror, Status, Reason}               |
%%            {siperror, Status, Reason, ExtraHeaders} 
%%
%% @doc     Parse a request/response. Extract the first line contents,
%%          build a Header keylist, and return the offset to the
%%          body.
%% @end
%%--------------------------------------------------------------------
parse_packet(<<?CR, ?LF>>, Origin) ->
    if
	is_record(Origin, siporigin) ->
	    logger:log(debug, "Keep-alive packet from ~s", [transportlayer:origin2str(Origin)]);
	true ->
	    %% This is not our final parse attempt, don't log
	    true
    end,
    keepalive;
parse_packet(Packet, Origin) ->
    if
	is_record(Origin, siporigin) ->
	    OriginStr = transportlayer:origin2str(Origin),
	    %% Extract receiver pid if present
	    RStr = if
		       is_record(Origin, siporigin) ->
			   ["(connection handler: ", pid_to_list(Origin#siporigin.receiver), ") "];
		       true ->
			   ""
		   end,
	    logger:log_iolist(debug, [<<"Packet from ">>, OriginStr, 32, RStr, $:, 10, Packet, 10]);
	true ->
	    %% This is not our final parse attempt, don't log
	    true
    end,
    {FirstLine, HeaderBody} = parse_firstline(Packet),
    {Header, Body} = parse_headers(HeaderBody),
    {ok, FirstLine, Header, Body}.

%%--------------------------------------------------------------------
%% @spec    (Bin, Offset) ->
%%            {Headers, Body}
%%
%%            Bin     = binary() "SIP request without first line"
%%
%%            Headers = #keylist{}
%%            Body    = binary()
%%
%% @doc     Get all headers, and a new offset pointing at whatever is
%%          after the header-body separator (body or nothing).
%% @end
%%--------------------------------------------------------------------
parse_headers(Bin) ->
    parse_headers(Bin, []).

parse_headers(<<?CR, ?LF, Rest/binary>>, Acc) ->
    %% CRLF, must be header-body separator. We are finished.
    {keylist:from_list(lists:reverse(Acc)), Rest};
parse_headers(<<N, Rest/binary>>, Acc) when N == ?CR orelse N == ?LF ->
    %% CRLF, must be header-body separator. We are finished.
    {keylist:from_list(lists:reverse(Acc)), Rest};
parse_headers(Bin, Acc) when size(Bin) > 0 ->
    {ok, This, Rest} = parse_one_header(Bin),
    parse_headers(Rest, [This | Acc]);
parse_headers(<<>>, _Acc) ->
    throw({error, no_header_body_separator_found}).

%%--------------------------------------------------------------------
%% @spec    (Bin) ->
%%            {ok, HData, Rest}
%%
%%            Bin    = binary() "SIP header and probably more"
%%
%%            HData  = {Key, Name, Values}
%%            Key    = atom() | string() "Normalized header name"
%%            Name   = string() "Header name"
%%            Values = [string()]
%%            Rest   = binary()
%%
%% @doc     Parse the SIP header line at the start of Bin, and return
%%          the parsed #header{} plus anything left of Bin.
%% @end
%%--------------------------------------------------------------------
parse_one_header(Bin) ->
    {Key, Name, Rest} = parse_one_header_key(Bin),
    {Comma, ValueStr, Rest2} = parse_one_header_values(Rest),
    %% Extract values
    Values = split_header_value(Key, ValueStr, Comma),
    {ok, {Key, Name, Values}, Rest2}.

%%--------------------------------------------------------------------
%% @spec    (Bin, Offset) ->
%%            {Length, AfterColonOffset}
%%
%%            Bin    = binary() "the complete SIP message"
%%            Offset = integer() "this headers start offset"
%%
%%            Length           = integer()
%%            AfterColonOffset = integer()
%%
%% @doc     Get the offset and length of the key for this header.
%%          Return an offset to the first byte after the colon
%%          separating key and values.
%% @end
%%--------------------------------------------------------------------
parse_one_header_key(Bin) ->
    {Str, Rest} = parse_one_header_key2(Bin, false, []),
    {keylist:normalize(Str), Str, Rest}.

parse_one_header_key2(<<N, Rest/binary>>, RequireColon, Acc) when N == ?SP orelse N == ?HTAB ->
    %% RFC3261 BNF for headers :
    %% To separate the header name from the rest of value,
    %% a colon is used, which, by the above rule, allows whitespace before,
    %% but no line break, and whitespace after, including a linebreak.
    KeyLen = length(Acc),
    case (KeyLen >= 0) of
	false ->
	    %% XXX should be (KeyLen > 0) ?
	    throw({error, whitespace_where_header_name_was_expected});
	true ->
	    %% KeyLen is non-zero, so tab and space are OK - don't accumulate
	    %% spaces though, and set RequireColon to true
	    parse_one_header_key2(Rest, true, Acc)
    end;
parse_one_header_key2(<<$:, Rest/binary>>, _RequireColon, Acc) ->
    %% Colon spotted
    case (length(Acc) >= 0) of
	true ->
	    %% We are finished.
	    {lists:reverse(Acc), Rest};
	false ->
	    throw({error, colon_where_header_name_was_expected})
    end;
parse_one_header_key2(<<H, Rest/binary>>, RequireColon, Acc) ->
    %% anything else, make sure we don't have RequireColon set
    case RequireColon of
	true ->
	    throw({error, non_colon_when_colon_was_required});
	false ->
	    parse_one_header_key2(Rest, false, [H | Acc])
    end;
parse_one_header_key2(<<>>, _RequireColon, _Acc) ->
    throw({error, no_end_of_key_or_no_header_body_separator}).

%%--------------------------------------------------------------------
%% @spec    (Bin, Offset) ->
%%            {Comma, ValuePtrs, NextOffset}
%%
%%            Bin    = binary() "the complete SIP message"
%%            Offset = integer() "this headers start offset"
%%
%%            Comma      = true | false "comma seen or not?"
%%            ValuePtrs  = #ptr{}
%%            NextOffset = integer() "offset of whatever is after this header's value(s)"
%%
%% @doc     Locate all value elements for this header. This is a list
%%          of tuples consisting of start offset and length, since
%%          header values can span over multiple lines.
%% @end
%%--------------------------------------------------------------------
parse_one_header_values(Bin) ->
    parse_one_header_values2(Bin, false, []).

parse_one_header_values2(<<N, Rest/binary>>, Comma, []) when N == ?SP orelse N == ?HTAB ->
    %% space / tab after colon, just ignore
    parse_one_header_values2(Rest, Comma, []);
parse_one_header_values2(<<?CR, ?LF, N:8, Rest/binary>>, Comma, Acc) when N == ?SP orelse N == ?HTAB ->
    %% CRLF followed by space / tab, this header continues on the next line.
    parse_one_header_values2(Rest, Comma, Acc);
parse_one_header_values2(<<?LF, N:8, Rest/binary>>, Comma, Acc) when N == ?SP orelse N == ?HTAB ->
    %% LF followed by space / tab, this header continues on the next line.
    parse_one_header_values2(Rest, Comma, Acc);
parse_one_header_values2(<<?CR, ?LF, Rest/binary>>, Comma, Acc) ->
    %% CRLF (not followed by space or tab (checked above)), we are done
    {Comma, lists:reverse(Acc), Rest};
parse_one_header_values2(<<?LF, Rest/binary>>, Comma, Acc) ->
    %% LF (not followed by space or tab (checked above)), we are done
    {Comma, lists:reverse(Acc), Rest};
parse_one_header_values2(<<N, Rest/binary>>, Comma, Acc) ->
    %% Part of value. If it is a comma we remember we saw one, and have to do more
    %% expensive header parsing later on.
    NewComma =
	case N of
	    44 -> true;
	    _ ->  Comma
	end,
    parse_one_header_values2(Rest, NewComma, [N | Acc]);
parse_one_header_values2(<<>>, _Comma, _Acc) ->
    throw({error, no_end_of_header}).


%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    (Key, ValueStr, Comma) ->
%%            ValueList
%%
%%            Key       = atom() | string()
%%            ValueStr  = string()
%%            Bin       = binary() "the complete SIP message"
%%            Comma     = true | false "whether there is a comma in the values or not"
%%
%%            ValueList = [string()]
%%
%% @doc     Look at Key and decide whether Value is a header that
%%          should be splitted on comma or not. Then look at Comma to
%%          see if there is a comma in the values at all. If it is,
%%          then do the expensive sipheader:comma() on the values,
%%          otherwise just return it as [Value].
%% @end
%%--------------------------------------------------------------------
%% Except the headers listed in RFC3261 7.3.1 and in the RFC3261 BNF
%% to be excepted from standard header comma splitting
split_header_value('www-authenticate', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('authorization', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('proxy-authenticate', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('proxy-authorization', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('call-id', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('content-disposition', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('content-length', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('content-type', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('cseq', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('date', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('expires', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('from', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('max-forwards', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('mime-version', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('min-expires', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('organization', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('priority', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('reply-to', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('retry-after', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('server', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('timestamp', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('to', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value('user-agent', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
%% Non-RFC3261 ones that should not be splitted
split_header_value('x-yxa-peer-auth', ValueStr, _Comma) ->
    extract_value_s(ValueStr);
split_header_value(_Key, ValueStr, false) ->
    %% An ordinary header, but we haven't seen a comma in it so we don't
    %% have to try and split on comma
    extract_value_s(ValueStr);
split_header_value(_Key, ValueStr, true) ->
    Splitted = sipheader:comma(ValueStr),
    %% Remove leading and trailing white space from all the values
    [string:strip(V, both) || V <- Splitted].

%% part of split_header_value(), extract all value parts, strip leading
%% and trailing whitespace and return it as a list containing a string
%% Returns : [string()]
extract_value_s(ValueStr) ->
    [string:strip(ValueStr, both)].

%%--------------------------------------------------------------------
%% @spec    (Bin) ->
%%            {Parsed, Rest}
%%            Parsed = {request, {Method, URIstr}} |
%%                     {response, {Status, Reason}}
%%            Rest   = binary()
%%
%%            Bin    = binary()
%%            Offset = integer() "where in Bin the first line starts"
%%
%%            Method = string() "e.g. \"INVITE\""
%%            URIstr = string() "e.g. \"sip:user@example.org\""
%%            Status = integer() "e.g. 100"
%%            Reason = string() "e.g. \"Trying\""
%%
%% @doc     Figure out if this is a request or response.
%% @end
%%--------------------------------------------------------------------
parse_firstline(Bin) ->
    case Bin of
	<<"SIP/2.0 ", Bin2/binary>> ->
	    %% is response
	    {ok, Parsed, Bin3} = parse_firstline_response(Bin2),
	    {{response, Parsed}, Bin3};
	_ ->
	    %% is (hopefully) request
	    {ok, Parsed, Bin3} = parse_firstline_request(Bin),
	    {{request, Parsed}, Bin3}
    end.

%% part of parse_firstline(), try to parse data at offset MethodOffset
%% as a Request-Line.
parse_firstline_request(Bin) ->
    {Method, Bin2} = extract_token(Bin, ?SP),
    {URI, Bin3} = extract_token(Bin2, ?SP),
    %% Verify what is after the URI
    case Bin3 of
	<<"SIP/2.0\r\n", Rest/binary>> ->
	    %% Ok, the URI had " SIP/2.0"
	    {ok, {Method, URI}, Rest};
	<<"SIP/2.0\n", Rest/binary>> ->
	    %% Ok, the URI had " SIP/2.0". Missing a CR, but we allow that.
	    {ok, {Method, URI}, Rest};
	<<"SIP/2.0", _Rest/binary>> ->
	    %% uh oh, there is something other than a linefeed there
	    %% (or nothing more at all)
	    throw({error, garbage_after_sip_version});
	_ ->
	    throw({error, invalid_sip_version})
    end.

%% Get everything up until Sep (Sep is a byte)
extract_token(Bin, Sep) ->
    extract_token(Bin, Sep, []).

extract_token(<<Sep, Rest/binary>>, Sep, Acc) ->
    %% Separator found
    {lists:reverse(Acc), Rest};
extract_token(<<H, Rest/binary>>, Sep, Acc) ->
    extract_token(Rest, Sep, [H | Acc]);
extract_token(<<>>, _Sep, Acc) ->
    %% end of binary
    {lists:reverse(Acc), <<>>}.

%% part of parse_firstline(), this should be a Status-Line
parse_firstline_response(Bin) ->
    {Status, Bin2} = extract_integer(Bin),
    {Reason, Bin3} = extract_eol(Bin2),
    {ok, {Status, Reason}, Bin3}.

%% Get an integer
extract_integer(Bin) ->
    {L, Rest} = extract_integer(Bin, []),
    io:format("EXTR INT : ~p  (Rest ~p)~n", [L, Rest]),
    Int =
	try list_to_integer(L) of
	    N -> N
	catch
	    error: _ ->
		throw({error, non_integer_where_integer_expected})
	end,
    {Int, Rest}.
    
extract_integer(<<H, Rest/binary>>, Acc) when H >= $0, H =< $9 ->
    %% a digit
    extract_integer(Rest, [H | Acc]);
extract_integer(<<Rest/binary>>, Acc) ->
    %% not a digit, we are finished
    {lists:reverse(Acc), Rest}.

%% extract up to the next end-of-line (CR or LF)
extract_eol(Bin) ->
    extract_eol(Bin, false, []).

extract_eol(<<?CR, ?LF, Rest/binary>>, _CRLFseen, Acc) ->
    %% CRLF, advance to next position
    extract_eol(Rest, true, Acc);
extract_eol(<<?LF, Rest/binary>>, _CRLFseen, Acc) ->
    %% LF, advance to next position
    extract_eol(Rest, true, Acc);
extract_eol(<<H, Rest/binary>>, false, Acc) ->
    %% non-CR/LF, add to Acc
    extract_eol(Rest, false, [H | Acc]);
extract_eol(Rest, _CRLFseen, Acc) ->
    %% we are finished, peel of spaces and then return
    Str = string:strip(lists:reverse(Acc), left),
    {Str, Rest}.

%%--------------------------------------------------------------------
%% @spec    ({Method, URI}, Header, Body) -> #request{}
%%
%%            Method = string()
%%            URI    = string() "URI as string - gets parsed here"
%%            Header = #keylist{}
%%            Body   = binary()
%%
%% @doc     Put all the pieces of a request together. Note that we
%%          cover up for unparsable URIs here!
%% @end
%%--------------------------------------------------------------------
request({Method, URIstr}, Header, Body) when is_list(Method), is_list(URIstr),
					     is_record(Header, keylist), is_binary(Body) ->
    URI =
	try sipurl:parse(URIstr) of
	    Parsed -> Parsed
	catch
	    throw: {yxa_unparsable, url, {Error, Str}} ->
		{yxa_unparsable, url, {Error, Str}}
	end,
    #request{method	= Method,
	     uri	= URI,
	     header	= Header,
	     body	= Body
	    }.

%%--------------------------------------------------------------------
%% @spec    ({Status, Reason}, Header, Body) -> #response{}
%%
%%            Status = string() "gets converted to integer here"
%%            Reason = string()
%%            Header = #keylist{}
%%            Body   = binary()
%%
%% @doc     Put all the pieces of a response together.
%% @end
%%--------------------------------------------------------------------
response({Status, Reason}, Header, Body) when is_integer(Status), is_list(Reason),
					      is_record(Header, keylist), is_binary(Body) ->
    #response{status = Status,
	      reason = Reason,
	      header = Header,
	      body   = Body
	     }.


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
-ifdef( YXA_NO_UNITTEST ).
test() ->
    {error, "Unit test code disabled at compile time"}.

-else.

test() ->
    %% parse/2
    %%--------------------------------------------------------------------
    %% init
    autotest:mark(?LINE, "parse/2 request - 1"),
    Message1 =
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TCP one.example.org\r\n"
	"\r\nbody",
    URL1 = sipurl:parse("sip:test@example.org"),

    %% REQUESTS

    %% basic parse
    autotest:mark(?LINE, "parse/2 request - 1.1"),
    #request{method = "INVITE",
	     uri    = URL1,
	     header = Header1,
	     body   = <<"body">>} = parse(Message1, none),

    %% verify single Via
    autotest:mark(?LINE, "parse/2 request - 1.2"),
    ["SIP/2.0/TCP one.example.org"] = keylist:fetch('via', Header1),

    Message2 =
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TCP two.example.org\r\n"
	"From: \"Test Test\" <sip:test@example.org>;tag=abc\r\n"
	"To: <sip:to@example.org>   \n"
	"via: SIP/2.0/UDP one.example.org\r\n"
	"\r\nbody\r\n",

    autotest:mark(?LINE, "parse/2 request - 2.1"),
    %% More complex header, one faulty \r\n (end of To: line) and
    %% two Vias not located together. Also some extra spaces after To: value
    %% and extra \r\n at the end of body.
    #request{method = "INVITE",
	     header = Header2,
	     body   = <<"body\r\n">>
	    } = parse(Message2, none),

    autotest:mark(?LINE, "parse/2 request - 2.2"),
    %% Verify To:
    ["<sip:to@example.org>"] = keylist:fetch('to', Header2),

    autotest:mark(?LINE, "parse/2 request - 2.3"),
    %% Verify Via's
    ["SIP/2.0/TCP two.example.org", "SIP/2.0/UDP one.example.org"] = keylist:fetch('via', Header2),

    Message3 = "INVITE sip:test@example.org   SIP/2.0\r\n",

    autotest:mark(?LINE, "parse/2 request - 3"),
    %% Make sure we throw the right exception on extra spaces in Request-Line
    {error, invalid_sip_version} = (catch parse(Message3, none)),

    Message4 =
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via:SIP/2.0/TLS four.example.org\r\n"
	"Via     :     SIP/2.0/TCP three.example.org\r\n"
	"Via     :SIP/2.0/TCP two.example.org,SIP/2.0/UDP one.example.org  \r\n"
	"\r\n",

    %% Extra spaces and missing spaces
    autotest:mark(?LINE, "parse/2 request - 4.1"),
    #request{method = "INVITE",
	     header = Header4,
	     body   = <<>>
	    } = parse(Message4, none),

    %% verify the Via's
    autotest:mark(?LINE, "parse/2 request - 4.2"),
    ["SIP/2.0/TLS four.example.org", "SIP/2.0/TCP three.example.org", "SIP/2.0/TCP two.example.org",
     "SIP/2.0/UDP one.example.org"] = keylist:fetch('via', Header4),

    Message5 =
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TLS three.example.org,\r\n"
	"	SIP/2.0/TCP two.example.org\r\n"
	"v: SIP/2.0/UDP one.example.org\r\n"
	"\r\n",
    autotest:mark(?LINE, "parse/2 request - 5.1"),
    %% multiline Via
    #request{method = "INVITE",
	     header = Header5,
	     body   = <<>>} = parse(Message5, none),

    autotest:mark(?LINE, "parse/2 request - 5.2"),
    %% verify the three Via's
    ["SIP/2.0/TLS three.example.org", "SIP/2.0/TCP two.example.org", "SIP/2.0/UDP one.example.org"] =
	keylist:fetch('via', Header5),

    %% This is the '3.1.1.1  A short tortuous INVITE' from draft-ietf-sipping-torture-tests-04.txt,
    %% except the body which wasn't very special
    Message6 =
	"INVITE sip:vivekg@chair-dnrc.example.com;unknownparam SIP/2.0\r\n"
	"TO :\r\n"
	" sip:vivekg@chair-dnrc.example.com ;   tag    = 1918181833n\r\n"
	"from   : \"J Rosenberg \\\"\"       <sip:jdrosen@example.com>\r\n"
	"  ;\r\n"
	"  tag = 98asjd8\r\n"
	"MaX-fOrWaRdS: 0068\r\n"
	"Call-ID: wsinv.ndaksdj@192.0.2.1\r\n"
	"Content-Length   : 4\r\n"
	"cseq: 0009\r\n"
	"  INVITE\r\n"
	"Via  : SIP  /   2.0\r\n"
	" /UDP\r\n"
	"    192.0.2.2;branch=390skdjuw\r\n"
	"s :\r\n"
	"NewFangledHeader:   newfangled value\r\n"
	" continued newfangled value\r\n"
	"UnknownHeaderWithUnusualValue: ;;,,;;,;\r\n"
	"Content-Type: application/sdp\r\n"
	"Route:\r\n"
	" <sip:services.example.com;lr;unknownwith=value;unknown-no-value>\r\n"
	"v:  SIP  / 2.0  / TCP     spindle.example.com   ;\r\n"
	"  branch  =   z9hG4bK9ikj8  ,\r\n"
	" SIP  /    2.0   / UDP  192.168.255.111   ; branch=\r\n"
	" z9hG4bK30239\r\n"
	"m:\"Quoted string \\\"\\\"\" <sip:jdrosen@example.com> ; newparam =\r\n"
	"      newvalue ;\r\n"
	"  secondparam ; q = 0.33\r\n"
	"\r\n"
	"test",
    URL6 = sipurl:parse("sip:vivekg@chair-dnrc.example.com;unknownparam"),

    autotest:mark(?LINE, "parse/2 request - 6.1"),
    %% parse tortuous INVITE
    {request, "INVITE" , URL6, Header6, <<"test">>} = parse(Message6, none),

    %% verify contents

    autotest:mark(?LINE, "parse/2 request - 6.2.1.1"),
    %% To: raw data extraction test
    ["sip:vivekg@chair-dnrc.example.com ;   tag    = 1918181833n"] = keylist:fetch('to', Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.1.2 (To: full parsing, disabled)"),
    %% To: - parsing check disabled since we fail to parse the URI
    %%{none, Test6_ToURL} = sipheader:to(Header6),
    %%"sip:vivekg@chair-dnrc.example.com;tag=1918181833n" = sipurl:print(Test6_ToURL),

    autotest:mark(?LINE, "parse/2 request - 6.2.2.1"),
    %% From: raw data extraction test
    ["\"J Rosenberg \\\"\"       <sip:jdrosen@example.com> ; tag = 98asjd8"]
	= keylist:fetch('from', Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.2.2"),
    %% From: header with trailing spaces and quoted quotes
    {"J Rosenberg \\\"", Test6_FromURL} = sipheader:from(Header6),
    "sip:jdrosen@example.com" = sipurl:print(Test6_FromURL),

    autotest:mark(?LINE, "parse/2 request - 6.2.3"),
    %% Max-Forwards:
    ["0068"] = keylist:fetch('max-forwards', Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.4"),
    %% Call-Id:
    "wsinv.ndaksdj@192.0.2.1" = sipheader:callid(Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.5"),
    %% Content-Length:
    ["4"] = keylist:fetch('content-length', Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.6"),
    %% CSeq:
    {"0009", "INVITE"} = sipheader:cseq(Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.7.1"),
    ["SIP  /   2.0/UDP   192.0.2.2;branch=390skdjuw",
     "SIP  / 2.0  / TCP     spindle.example.com   ; branch  =   z9hG4bK9ikj8",
     "SIP  /    2.0   / UDP  192.168.255.111   ; branch=z9hG4bK30239"] = keylist:fetch('via', Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.8"),
    %% Subject:
    [] = keylist:fetch('subject', Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.9"),
    %% NewFangledHeader:
    ["newfangled valuecontinued newfangled value"] = keylist:fetch("NewFangledHeader", Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.10"),
    [";;", "", ";;", ";"] = keylist:fetch("UnknownHeaderWithUnusualValue", Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.11"),
    %% Content-Type:
    ["application/sdp"] = keylist:fetch("Content-Type", Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.12"),
    %% Route:
    ["<sip:services.example.com;lr;unknownwith=value;unknown-no-value>"] = keylist:fetch('route', Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.13.1"),
    %% Contact:
    ["\"Quoted string \\\"\\\"\" <sip:jdrosen@example.com> ; newparam =     newvalue ; secondparam ; q = 0.33"]
	= keylist:fetch('contact', Header6),

    autotest:mark(?LINE, "parse/2 request - 6.2.13.2 (Contact: full parse)"),
    %% Contact:
    Parse13C = #contact{display_name = "Quoted string \\\"\\\"",
			urlstr = "sip:jdrosen@example.com",
			contact_param = contact_param:to_norm([{"newparam", "newvalue"},
							       {"secondparam", none},
							       {"q", "0.330"}])
			},
    [Parse13C] = contact:parse( keylist:fetch('contact', Header6) ),


    Message7 =
        "REGISTER sip:example.org SIP/2.0\r\n"
        "Via: SIP/2.0/TLS 192.0.2.78\r\n"
        "Content-Length: 0004\r\n"
        "\r\n"
	"body"
        "INVITE sip:foo@example.com SIP/2.0\r\n"
        "this is just garbage received in the same packet\r\n",

    autotest:mark(?LINE, "parse/2 request - 7"),
    %% extra data after request - should be ignored (Content-Length: 4)
    {request, "REGISTER" , _, _, <<"body">>} = parse(Message7, none),

    Message8 =
	"INVITE <sip:user@example.com> SIP/2.0\r\n"
	"Via: SIP/2.0/TLS 192.0.2.1, SIP/2.0/FOO 192.0.2.78\r\n"
	"\r\n",
    autotest:mark(?LINE, "parse/2 request - 8.1"),
    %% Request-URI enclosed in <> - invalid, but should not be rejected here
    %% (it isn't THAT broken and should be responded to with a 400 Bad Request)
    {request, "INVITE" , {yxa_unparsable, url, {_Error, "<sip:user@example.com>"}}, Header8, <<>>} = parse(Message8, none),

    autotest:mark(?LINE, "parse/2 request - 8.2"),
    %% verify the Via header so we know keylist is created as it should
    ["SIP/2.0/TLS 192.0.2.1", "SIP/2.0/FOO 192.0.2.78"] = keylist:fetch('via', Header8),

    Body9 = "This\nis\rbody\n\r\n\r\n_and should have it's line breaks preserved\r\n",
    Message9 =
	"INVITE sip:a@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TLS 192.0.2.1, SIP/2.0/FOO 192.0.2.78\r\n"
	"\r\n" ++ Body9,

    autotest:mark(?LINE, "parse/2 request - 9"),
    %% verify that we don't mess with line breaks in body
    BinBody9 = list_to_binary(Body9),
    {request, "INVITE", _, _, BinBody9} = parse(Message9, none),

    Message10 =
	"INVITE sip:a@example.org SIP/2.0\n"
	"Via: SIP/2.0/TLS 192.0.2.1\n"
	"Content-Length: 20\n"
	"\n"
	"partial",

    autotest:mark(?LINE, "parse/2 request - 10.1"),
    %% make sure we can handle message where body is not yet fully received
    %% (also test \n instead of \r\n throughout the message)
    {request, "INVITE", _, Header10, <<"partial">>} = parse(Message10, none),

    autotest:mark(?LINE, "parse/2 request - 10.2"),
    %% verify the Content-Length is untouched (to not re-introduce bug)
    ["20"] = keylist:fetch('content-length', Header10),

    Str11 = "INVITE sip:a@example.org SIP/2.0",

    autotest:mark(?LINE, "parse/2 request - 11.1"),
    %% Make sure we throw the right exception on extra characters after SIP version
    {error, garbage_after_sip_version} = (catch parse(Str11 ++ "X\r\n", none)),

    autotest:mark(?LINE, "parse/2 request - 11.2"),
    %% Make sure we throw the right exception on extra characters after SIP version
    {error, garbage_after_sip_version} = (catch parse(Str11 ++ "1\r\nTest: foo\r\n\r\n", none)),

    autotest:mark(?LINE, "parse/2 request - 11.3"),
    %% Make sure we throw the right exception on extra characters after SIP version
    {error, garbage_after_sip_version} = (catch parse(Str11 ++ "\r\r\nTest: foo\r\n\r\n", none)),

    Message12 =
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TCP two.example.org\r\n"
	"body\r\n",

    autotest:mark(?LINE, "parse/2 request - 12"),
    %% Only single CRLF between header and body - illegal. 'body' Can't be distinguished from another header.
    {error, no_end_of_key_or_no_header_body_separator} = (catch parse(Message12, none)),

    Message13 =
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"Via: SIP/2.0/TCP two.example.org\r\n"
	"Content-Length: 0, 4\r\n"
	"\r\n"
	"body",

    autotest:mark(?LINE, "parse/2 request - 13"),
    %% Duplicate Content-Length
    try parse(Message13, none) of
	_ -> throw(test_failed)
    catch
	throw: {siperror, 400, "Invalid (or duplicate) Content-Length header"} -> ok
    end,

    DisplayName14 = "First\t Last",
    Message14 =
	"INVITE sip:test@example.org SIP/2.0\r\n"
	"From: \"" ++ DisplayName14 ++ "\" <sip:test@example.org>;tag=abc\r\n"
	"\r\n"
	"body",

    autotest:mark(?LINE, "parse/2 request - 14"),
    %% <TAB> in the quoted display name
    #request{method = "INVITE", header = Header14} = parse(Message14, none),
    Parse14 = contact:parse(keylist:fetch('from', Header14)),
    [#contact{display_name = DisplayName14}] = Parse14,

    %% RESPONSES

    RMessage1 = "SIP/2.0 100 Trying\r\n"
	"Via: SIP/2.0/TLS 192.0.2.78   \r\n"
	"Content-Length: 3\r\n"
	"\r\n"
	"foo",
    autotest:mark(?LINE, "parse/2 response - 1.1"),
    {response, 100, "Trying", RHeader1, RMessage1_body} = parse(RMessage1, none),

    %% verify the Via header so we know keylist is created as it should
    autotest:mark(?LINE, "parse/2 response - 1.2"),
    ["SIP/2.0/TLS 192.0.2.78"] = keylist:fetch('via', RHeader1),

    %% verify the body
    autotest:mark(?LINE, "parse/2 response - 1.3"),
    <<"foo">> = RMessage1_body,

    RMessage2 =
        "SIP/2.0 100 \r\n"
        "Via: SIP/2.0/TLS 192.0.2.78\r\n"
	"\r\n",
    autotest:mark(?LINE, "parse/2 response - 2.1"),
    {response, 100, "", RHeader2, <<>>} = parse(RMessage2, none),

    %% verify the Via header so we know keylist is created as it should
    autotest:mark(?LINE, "parse/2 response - 2.2"),
    ["SIP/2.0/TLS 192.0.2.78"] = keylist:fetch('via', RHeader2),

    ok.

-endif.
