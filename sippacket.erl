-module(sippacket).
-export([parse/3, parse_packet/1, parse_packet/3, parseheader/1, parserequest/1]).

parse_packet(Packet) ->
    parse_packet(Packet, "", none).

parse_packet(Packet, SrcIP, SrcPort) ->
    logger:log(debug, "Packet from ~s:~p :~n~s~n", [SrcIP, SrcPort, Packet]),
    Packetfixed = siputil:linefix(Packet),
    case string:str(Packetfixed, "\n\n") of
	0 ->
	    {Packetfixed, ""};
	Headerlen ->
%	    io:format("Headerlen:~p~n", [Headerlen]),
	    Header = string:substr(Packetfixed, 1, Headerlen),
	    Body = string:substr(Packetfixed, Headerlen + 2),
%	    io:format("Header:~p ~p~n", [Header, Body]),
	    {Header, Body}
    end.

parse(Packet, SrcIP, SrcPort) ->
    {Header, Body} = parse_packet(Packet, SrcIP, SrcPort),
    {Request, Headerkeylist} = parseheader(Header),
    case parserequest(Request) of
	{request, Parsed} ->
	    request(Parsed, Headerkeylist, Body);
	{response, Parsed} ->
	    response(Parsed, Headerkeylist, Body)
    end.

parseheader(Header) ->
    [Request | Lines] = string:tokens(Header, "\n"),
    Parseheader = fun(Line) ->
			  Index = string:chr(Line, $:),
			  Name = string:substr(Line, 1, Index - 1),
			  Value = string:strip(string:substr(Line, Index + 1),
					       left),
			  {httpd_util:to_lower(Name), Value}
		  end,
    Headerlist = lists:map(Parseheader, Lines),
    Headerkeylist = keylist:from_list(Headerlist),
    {Request, Headerkeylist}.

parserequest(Request) ->
    Index1 = string:chr(Request, 32),
    F1 = string:substr(Request, 1, Index1 - 1),
    Rest = string:strip(string:substr(Request, Index1 + 1),
			left),
    case F1 of
	"SIP/2.0" ->
	    Index2 = string:chr(Rest, 32),
	    Status = string:substr(Rest, 1, Index2 - 1),
	    Reason = string:strip(string:substr(Rest, Index2 + 1),
				  left),
	    {response, {Status, Reason}};
	Method ->
	    Index2 = string:rchr(Rest, 32),
	    URI = string:substr(Rest, 1, Index2 - 1),
	    "SIP/2.0" = string:strip(string:substr(Rest, Index2 + 1),
				     left),
	    {request, {Method, URI}}
    end.

request({Method, URI}, Header, Body) ->
%    io:format("Request:~p~n", [{Method, URI}]),
    URL = sipurl:parse(URI),
    {request, Method, URL, Header, Body}.

response({Status, Reason}, Header, Body) ->
%    io:format("Response:~p~n", [Status]),
    {response, list_to_integer(Status), Reason, Header, Body}.
