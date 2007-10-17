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
parse(Packet, Origin) when is_binary(Packet), Origin == none; is_record(Origin, siporigin) ->
    case parse_packet(Packet, Origin) of
	keepalive ->
	    keepalive;
	{FirstLine, Header, BodyOffset} ->
	    CL = keylist:fetch('content-length', Header),
	    case extract_body(Packet, BodyOffset, CL) of
		{ok, Body} ->
		    case FirstLine of
			{request, Parsed} ->
			    %% XXX move data from SIP URI:s such as
			    %%   sips:alice@atlanta.com?subject=project%20x&priority=urgent
			    %% into Header and strip them from the URI. See
			    %% RFC 3261 19.1.5 Forming Requests from a URI
			    request(Parsed, Header, Body);
			{response, Parsed} ->
			    response(Parsed, Header, Body);
			none ->
			    throw({siperror, 400, "Completely unparseable request/response"})
		    end;
		{error, "invalid Content-Length"} ->
		    logger:log(debug, "Packet has invalid Content-Length header : ~p", [CL]),
		    throw({siperror, 400, "Invalid (or duplicate) Content-Length header"})
	    end
    end;
parse(PacketL, Origin) when is_list(PacketL) ->
    parse(list_to_binary(PacketL), Origin).

%% part of parse() - extract body using an existing Content-Length
%% or assuming that it is the rest of Bin
extract_body(Bin, BodyOffset, [LLen]) ->
    %% Extract BodyLen from Content-Length header if one exists. If we get
    %% a datagram with excess data, we are to just ignore the extra data.
    BodyLen = size(Bin) - BodyOffset,
    try list_to_integer(LLen) of
	CLen when CLen < BodyLen ->
	    <<_:BodyOffset/binary, Body:CLen/binary-unit:8, Rest/binary>> = Bin,
	    logger:log(debug, "Ignoring ~p bytes of excess data (for now) : ~p", [size(Rest), binary_to_list(Rest)]),
	    {ok, Body};
	CLen when CLen > BodyLen ->
	    %% We haven't got the whole body. For stream-oriented transport (like TCP)
	    %% this is not an error - we just haven't received the complete message yet
	    <<_:BodyOffset/binary, Body:BodyLen/binary-unit:8>> = Bin,
	    {ok, Body};
	BodyLen ->
	    %% CLen matches BodyLen
	    <<_:BodyOffset/binary, Body:BodyLen/binary-unit:8>> = Bin,
	    {ok, Body}
    catch
	_ : _ ->
	    {error, "invalid Content-Length"}
    end;
extract_body(Bin, BodyOffset, []) ->
    %% No Content-Length header supplied, assume the body is the rest of the packet.
    BodyLen = size(Bin) - BodyOffset,
    <<_:BodyOffset/binary, Body:BodyLen/binary-unit:8>> = Bin,
    {ok, Body};
extract_body(_Bin, _BodyOffset, CL) ->
    %% More than one Content-Length header or something else wrong with it
    logger:log(debug, "Packet has invalid Content-Length header : ~p", [CL]),
    {error, "invalid Content-Length"}.


%%--------------------------------------------------------------------
%% @spec    (Packet, Origin) ->
%%            {FirstLine, Header, BodyOffset} | keepalive 
%%
%%            Packet = binary()
%%            Origin = #siporigin{} | none
%%
%%            FirstLine  = {request, {Method, URIstr}}  |
%%                         {response, {Status, Reason}}
%%            Header     = #keylist{}
%%            BodyOffset = integer()
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
	    logger:log(debug, "Keep-alive packet from ~s", [sipserver:origin2str(Origin)]);
	true ->
	    %% This is not our final parse attempt, don't log
	    true
    end,
    keepalive;
parse_packet(Packet, Origin) when is_binary(Packet) ->
    if
	is_record(Origin, siporigin) ->
	    OriginStr = sipserver:origin2str(Origin),
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
    {FirstLine, HeaderOffset} = parse_firstline(Packet, 0),
    {Header, BodyOffset} = parse_headers(Packet, HeaderOffset),
    {FirstLine, Header, BodyOffset}.

%%--------------------------------------------------------------------
%% @spec    (Bin, Offset) ->
%%            {Headers, BodyOffset}
%%
%%            Bin    = binary() "the complete SIP message"
%%            Offset = integer() "headers start offset"
%%
%%            Headers    = #keylist{}
%%            BodyOffset = integer()
%%
%% @doc     Get all headers, and a new offset pointing at whatever is
%%          after the header-body separator (body or nothing).
%% @end
%%--------------------------------------------------------------------
parse_headers(Bin, Offset) ->
    parse_headers(Bin, Offset, []).

parse_headers(Bin, Offset, Res) ->
    case Bin of
	<<_:Offset/binary, ?CR, ?LF, _/binary>> ->
	    %% CRLF, must be header-body separator. We are finished.
	    {make_keylist(Bin, lists:reverse(Res)), Offset + 2};
	<<_:Offset/binary, N:8, _/binary>> when N == ?CR; N == ?LF ->
	    %% CR or LF, must be header-body separator (albeit broken). We are finished.
	    {make_keylist(Bin, lists:reverse(Res)), Offset + 1};
	<<_:Offset/binary, _, _/binary>> ->
	    {ok, This, NextOffset} = parse_one_header(Bin, Offset),
	    parse_headers(Bin, NextOffset, [This | Res]);
	_ ->
	    throw({error, no_header_body_separator_found})
    end.

%%--------------------------------------------------------------------
%% @spec    (Bin, Offset) ->
%%            {ok, HData, NextLineOffset}
%%
%%            Bin    = binary() "the complete SIP message"
%%            Offset = integer() "this headers start offset"
%%
%%            HData          = #header{}
%%            NextLineOffset = integer()
%%
%% @doc     Get the offsets for all elements in the header starting at
%%          Offset. This is a tuple containing the offset and length
%%          of the header name, and one or more tuples with start
%%          offset and length of value elements. This is a list since
%%          header values can span over multiple lines.
%% @end
%%--------------------------------------------------------------------
parse_one_header(Bin, Offset) ->
    {KeyLength, ColonOffset} = parse_one_header_key(Bin, Offset),
    Key = #ptr{offset=Offset, length=KeyLength},
    {Comma, ValuePtrs, NextLineOffset} = parse_one_header_values(Bin, ColonOffset + 1),
    This = #header{key=Key, valueptrs=ValuePtrs, comma=Comma},
    {ok, This, NextLineOffset}.

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
parse_one_header_key(Bin, Offset) ->
    parse_one_header_key2(Bin, Offset, 0, false).

parse_one_header_key2(Bin, Offset, KeyLen, RequireColon) ->
    case Bin of
	<<_:Offset/binary, N, _/binary>> when N == ?SP; N == ?HTAB ->
	    %% RFC3261 BNF for headers :
	    %% To separate the header name from the rest of value,
	    %% a colon is used, which, by the above rule, allows whitespace before,
	    %% but no line break, and whitespace after, including a linebreak.
	    case (KeyLen >= 0) of
		false ->
		    throw({error, whitespace_where_header_name_was_expected});
		true ->
		    %% KeyLen is non-zero, so tab and space are OK - don't increase
		    %% KeyLen for spaces though, and set RequireColon to true
		    parse_one_header_key2(Bin, Offset + 1, KeyLen, true)
	    end;
	<<_:Offset/binary, $:, _/binary>> ->
	    %% Colon spotted
	    case (KeyLen >= 0) of
		true ->
		    %% KeyLen >= 0. We are finished.
		    {KeyLen, Offset};
		false ->
		    throw({error, colon_where_header_name_was_expected})
	    end;
	<<_:Offset/binary, _:8, _/binary>> ->
	    %% anything else, make sure we don't have RequireColon set
	    case RequireColon of
		true ->
		    throw({error, non_colon_when_colon_was_required});
		false ->
		    parse_one_header_key2(Bin, Offset + 1, KeyLen + 1, false)
	    end;
	_ ->
	    throw({error, no_end_of_key_or_no_header_body_separator})
    end.

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
parse_one_header_values(Bin, Offset) ->
    parse_one_header_values2(Bin, false, Offset, Offset, 0, []).

parse_one_header_values2(Bin, Comma, Offset, StartOffset, Len, Res) ->
    case Bin of
	<<_:Offset/binary, N:8, _/binary>> when Len == 0, Res == [], N == ?SP; N == ?HTAB ->
	    %% space / tab after colon, just ignore
	    parse_one_header_values2(Bin, Comma, Offset + 1, StartOffset + 1, Len, Res);
	<<_:Offset/binary, ?CR, ?LF, N:8, _/binary>> when N == ?SP; N == ?HTAB ->
	    %% CRLF followed by space / tab, this header continues on the next line.
	    %% Make a ptr record() for what we have this far, and then start over (set
	    %% a new StartOffset and set Len to 0)
	    This = #ptr{offset=StartOffset, length=Len},
	    NextOffset = Offset + 3,
	    parse_one_header_values2(Bin, Comma, NextOffset, NextOffset, 0, [This | Res]);
	<<_:Offset/binary, ?LF, N:8, _/binary>> when N == ?SP; N == ?HTAB ->
	    %% LF followed by space / tab, this header continues on the next line.
	    %% Make a ptr record() for what we have this far, and then start over (set
	    %% a new StartOffset and set Len to 0)
	    This = #ptr{offset=StartOffset, length=Len},
	    NextOffset = Offset + 2,
	    parse_one_header_values2(Bin, Comma, NextOffset, NextOffset, 0, [This | Res]);
	<<_:Offset/binary, ?CR, ?LF, _/binary>> ->
	    %% CRLF (not followed by space or tab (checked above)), we are done
	    This = #ptr{offset=StartOffset, length=Len},
	    {Comma, lists:reverse([This | Res]), Offset + 2};
	<<_:Offset/binary, ?LF, _/binary>> ->
	    %% LF (not followed by space or tab (checked above)), we are done
	    This = #ptr{offset=StartOffset, length=Len},
	    {Comma, lists:reverse([This | Res]), Offset + 1};
	<<_:Offset/binary, 44, _/binary>> ->	%% 44 is comma
	    %% comma, part of value but remember we saw a comma (by setting Comma to true)
	    parse_one_header_values2(Bin, true, Offset + 1, StartOffset, Len + 1, Res);
	<<_:Offset/binary, _:8, _/binary>> ->
	    %% something else, must be part of value
	    parse_one_header_values2(Bin, Comma, Offset + 1, StartOffset, Len + 1, Res);
	_ ->
	    throw({error, no_end_of_header})
    end.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Bin, Headers) ->
%%            Keylist
%%
%%            Bin     = binary() "the complete SIP message"
%%            Headers = [#header{}]
%%
%%            Keylist = #keylist{}
%%
%% @doc     Turn every element of Offsets into a {Key, Name, Values}
%%          tuple and then hand all those tuples to
%%          keylist:from_list() to get a keylist record.
%% @end
%%--------------------------------------------------------------------
make_keylist(Bin, Headers) ->
    Headerlist = make_keylist2(Bin, Headers, []),
    Keylist = keylist:from_list(Headerlist),
    Keylist.

make_keylist2(Bin, [#header{key=HKey, valueptrs=ValuePtrs, comma=Comma} | T], Res) ->
    #ptr{offset=KeyOffset, length=KeyLen} = HKey,

    %% Extract key
    <<_:KeyOffset/binary, NameBin:KeyLen/binary-unit:8, _/binary>> = Bin,
    Name = binary_to_list(NameBin),
    Key = keylist:normalize(Name),

    %% Extract values
    Values = split_header_value(Key, ValuePtrs, Bin, Comma),
    This = {Key, Name, Values},

    make_keylist2(Bin, T, [This | Res]);
make_keylist2(_Bin, [], Res) ->
    %% No more input
    lists:reverse(Res).

%%--------------------------------------------------------------------
%% @spec    (Key, ValuePtrs, Bin, Comma) ->
%%            ValueList
%%
%%            Key       = atom() | string()
%%            ValuePtrs = [#ptr{}]
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
split_header_value('www-authenticate', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('authorization', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('proxy-authenticate', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('proxy-authorization', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('call-id', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('content-disposition', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('content-length', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('content-type', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('cseq', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('date', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('expires', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('from', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('max-forwards', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('mime-version', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('min-expires', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('organization', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('priority', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('reply-to', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('retry-after', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('server', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('timestamp', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('to', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value('user-agent', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
%% Non-RFC3261 ones that should not be splitted
split_header_value('x-yxa-peer-auth', ValuePtrs, Bin, _Comma) ->
    extract_value_s(ValuePtrs, Bin);
split_header_value(_Key, ValuePtrs, Bin, false) ->
    %% An ordinary header, but we haven't seen a comma in it so we don't
    %% have to try and split on comma
    extract_value_s(ValuePtrs, Bin);
split_header_value(_Key, ValuePtrs, Bin, true) ->
    Value = extract_value(ValuePtrs, Bin),
    Splitted = sipheader:comma(Value),
    %% Remove leading and trailing white space from all the values
    Res = lists:map(fun(V) ->
			    string:strip(V, both)
		    end, Splitted),
    Res.

%% part of split_header_value(), extract all value parts from the
%% original SIP message and turn them into a list()
%% Returns : string()
extract_value(ValuePtrs, Bin) ->
    extract_value2(ValuePtrs, Bin, []).

%% part of split_header_value(), extract all value parts, strip leading
%% and trailing whitespace and return it as a list containing a string
%% Returns : [string()]
extract_value_s(ValuePtrs, Bin) ->
    V = extract_value2(ValuePtrs, Bin, []),
    [string:strip(V, both)].

extract_value2([H | T], Bin, Res) ->
    #ptr{offset=VOffset, length=VLen} = H,
    <<_:VOffset/binary, ValueBin:VLen/binary-unit:8, _/binary>> = Bin,
    extract_value2(T, Bin, [ValueBin | Res]);
extract_value2([], _Bin, Res) ->
    %% No more input
    All = list_to_binary(lists:reverse(Res)),
    binary_to_list(All).

%%--------------------------------------------------------------------
%% @spec    (Bin, Offset) ->
%%            {request, {Method, URIstr}} | {response, {Status, Reason}}
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
parse_firstline(Bin, Offset) ->
    case Bin of
	<<"SIP/2.0 ", _/binary>> ->
	    %% is response
	    {Parsed, NextLineOffset} = parse_firstline_response(Bin, Offset + 8),
	    {{response, Parsed}, NextLineOffset};
	_ ->
	    %% is (hopefully) request
	    {Parsed, NextLineOffset} = parse_firstline_request(Bin, Offset),
	    {{request, Parsed}, NextLineOffset}
    end.

%% part of parse_firstline(), try to parse data at offset MethodOffset
%% as a Request-Line.
parse_firstline_request(Bin, MethodOffset) ->
    {Method, MethodEOffset} = extract_token(Bin, MethodOffset, ?SP),
    {URI, URIEOffset} = extract_token(Bin, MethodEOffset + 1, ?SP),
    VerOffset = URIEOffset + 1,
    case Bin of
	<<_:VerOffset/binary, "SIP/2.0", _/binary>> ->
	    %% Ok, the URI had " SIP/2.0" after it - now verify that
	    %% the " SIP/2.0" is the last thing on this line
	    VerEOffset = URIEOffset + 8,
	    case Bin of
		<<_:VerEOffset/binary, ?CR, ?LF, _/binary>> ->
		    {{Method, URI}, VerEOffset + 2};
		<<_:VerEOffset/binary, ?LF, _/binary>> ->
		    {{Method, URI}, VerEOffset + 1};
		_ ->
		%% uh oh, there is something other than a linefeed there
		%% (or nothing more at all)
		throw({error, garbage_after_sip_version})
	    end;
	_ ->
	    throw({error, invalid_sip_version})
    end.

%% Get everything up until Sep (Sep is a byte)
extract_token(Bin, Offset, Sep) ->
    extract_token(Bin, Offset, Sep, []).

extract_token(Bin, Offset, Sep, Res) ->
    case Bin of
	<<_:Offset/binary, Sep:8, _/binary>> ->
	    %% Separator found
	    {lists:reverse(Res), Offset};
	<<_:Offset/binary, N:8, _/binary>> ->
	    extract_token(Bin, Offset + 1, Sep, [N | Res]);
	_ ->
	    %% end of binary
	    {lists:reverse(Res), Offset}
    end.

%% part of parse_firstline(), this should be a Status-Line
parse_firstline_response(Bin, StatusOffset) ->
    {Status, StatusEOffset} = extract_integer(Bin, StatusOffset),
    {Reason, NextLineOffset} = extract_eol(Bin, StatusEOffset + 1),
    {{Status, Reason}, NextLineOffset}.

%% Get an integer starting at Offset
extract_integer(Bin, Offset) ->
    extract_integer(Bin, Offset, []).

extract_integer(Bin, Offset, Res) ->
    case Bin of
	<<_:Offset/binary, N:8, _/binary>> when N >= $0, N =< $9 ->
	    %% a digit
	    extract_integer(Bin, Offset + 1, [N | Res]);
	_ ->
	    %% not a digit, we are finished
	    L = lists:reverse(Res),
	    Int = try list_to_integer(L) of
		      N -> N
		  catch
		      error: _ ->
			  throw({error, non_integer_where_integer_expected})
		  end,
	    {Int, Offset}
    end.

%% extract up to the next end-of-line (CR or LF)
extract_eol(Bin, Offset) ->
    extract_eol(Bin, Offset, false, []).

extract_eol(Bin, Offset, CRLFseen, Res) ->
    case Bin of
	<<_:Offset/binary, N, _/binary>> when N == ?CR; N == ?LF ->
	    %% CR or LF, advance to next position
	    extract_eol(Bin, Offset + 1, true, Res);
	<<_:Offset/binary, N, _/binary>> when CRLFseen == false ->
	    %% non-CR/LF, add to front of Res
	    extract_eol(Bin, Offset + 1, false, [N | Res]);
	_ ->
	    %% we are finished, peel of spaces and then return
	    Str = lists:reverse(string:strip(Res, left)),
	    {Str, Offset}
    end.

%%--------------------------------------------------------------------
%% @spec    ({Method, URI}, Header, Body) -> #request{}
%%
%%            Method = string()
%%            URI    = string() "URI as string - gets parsed here"
%%            Header = #keylist{}
%%            Body   = binary()
%%
%% @doc     Put all the pieces of a request together.
%% @end
%%--------------------------------------------------------------------
request({Method, URIstr}, Header, Body) when is_list(Method), is_list(URIstr),
					     is_record(Header, keylist), is_binary(Body) ->
    URI = sipurl:parse(URIstr),
    #request{method=Method, uri=URI, header=Header, body=Body}.

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
    #response{status=Status, reason=Reason, header=Header, body=Body}.


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
    {request, "INVITE" , {unparseable, "<sip:user@example.com>"}, Header8, <<>>} = parse(Message8, none),

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
