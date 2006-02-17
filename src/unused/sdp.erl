-module(sdp).
-export([parse/1,
	 print/1,
	 print/2
	]).

parseline([C, $= | Value]) ->
    {C, Value};

parseline(_) ->
    none.

parse(String) ->
    Packetfixed = siputil:linefix(String),
    Lines = string:tokens(Packetfixed, "\n"),
    Parseheader = fun(Line) ->
			  parseline(Line)
		  end,
    Headerlist = lists:map(Parseheader, Lines),
    Headerdict = dict:from_list(Headerlist),
    Conn = dict:fetch($c, Headerdict),
    Connsplit = string:tokens(Conn, " "),
    Media = dict:fetch($m, Headerdict),
    Mediasplit = string:tokens(Media, " "),
    [_,_,Address] = Connsplit,
    [_,Port | _] = Mediasplit,
    {Address, list_to_integer(Port)}.

print(AP) ->
    print(AP, []).

print({Address, Port}, Extra) when is_list(Extra) ->
    Headerlist = [{$v, "0"},
		  {$o, "- 1 2 IN IP4 " ++ Address},
		  {$s, "SIP Call"},
		  {$c, "IN IP4 " ++ Address},
		  {$t, "0 0"},
		  {$m, "audio " ++ integer_to_list(Port) ++ " RTP/AVP 8"}
		 ] ++ Extra,
    Buildheader = fun({C, Value}) ->
			  [C, $= | Value]
		  end,
    Lines = lists:map(Buildheader, Headerlist),
    siputil:concat_strings(Lines).
