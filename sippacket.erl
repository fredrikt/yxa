-module(sippacket).
-export([parse/2, parse_packet/1, parse_packet/2, parseheader/1, parserequest/1]).

-include("sipsocket.hrl").
-include("siprecords.hrl").

parse_packet(Packet) ->
    parse_packet(Packet, none).

parse_packet("\r\n", Origin) ->
    case sipserver:origin2str(Origin, none) of
	none -> true;
	S ->
	    logger:log(debug, "Keep-alive packet from ~s", [S])
    end,
    {keepalive};
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
	    Header = string:substr(Packetfixed, 1, Headerlen),
	    Body = string:substr(Packetfixed, Headerlen + 2),
	    {Header, Body}
    end.

parse(Packet, Origin) ->
    case parse_packet(Packet, Origin) of
	{keepalive} ->
	    keepalive;
	{Header, Body} ->
	    {Request, Headerkeylist} = parseheader(Header),
	    case parserequest(Request) of
		{request, Parsed} ->
		    % XXX move data from SIP URI:s such as
		    %   sips:alice@atlanta.com?subject=project%20x&priority=urgent
		    % into Headerkeylist and strip them from the URI. See
		    % RFC 3261 19.1.5 Forming Requests from a URI
		    request(Parsed, Headerkeylist, Body);
		{response, Parsed} ->
		    response(Parsed, Headerkeylist, Body);
		none ->
		    {siperror, 400, "Completely unparseable request/response"}
	    end
    end.

parseheader(Header) ->
    [Request | Lines] = string:tokens(Header, "\n"),
    Parseheader = fun(Line) ->
			  Index = string:chr(Line, $:),
			  Name = string:strip(string:substr(Line, 1, Index - 1), right), 
			  Value = string:strip(string:substr(Line, Index + 1),
					       left),
			  {Name, split_header_value(httpd_util:to_lower(Name), Value)}
		  end,
    Headerlist = lists:map(Parseheader, Lines),
    Headerkeylist = keylist:from_list(Headerlist),
    {Request, Headerkeylist}.

split_header_value(_, []) ->
    [];
split_header_value(LCname, Value) ->
    %% Luckily, none of these headers have a compact form.
    %% all values are lower case, so lists:member can be used
    case lists:member(LCname, ["www-authenticate", "authorization",
			       "proxy-authenticate", "proxy-authorization",
			       "date"]) of
	true ->
	    % Except the headers listed in RFC3261 7.3.1 and some other that needs
	    % to be excepted from standard header comma splitting
	    [Value];
	_ ->
	    Res = sipheader:comma(Value),
	    % Remove leading and trailing white space
	    lists:map(fun(V) ->
			string:strip(V)
		      end, Res)
    end.
    
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
	[] ->
	    none;
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
    #request{method=Method, uri=URL, header=Header, body=Body}.

response({Status, Reason}, Header, Body) ->
%    io:format("Response:~p~n", [Status]),
    #response{status=list_to_integer(Status), reason=Reason, header=Header, body=Body}.
