-module(gssapi).
-export([request/1]).

request(String) ->
    Port = open_port({spawn, "/afs/e.kth.se/home/2002/map/sip/auth/gssapi_server -s sipvxl --keytab=/nobackup/home/sipvxl/keytab"}, [{line, 10000}]),
    Port ! {self(), {command, String ++ [10]}},
    {eol, Data1} = receive
		       {Port, {data, Data}} ->
			   Data
		   end,
    {eol, Data2} = receive
		       {Port, {data, Data3}} ->
			   Data3
		   end,
    case {Data1, Data2} of
	{Response, "User:" ++ Username} ->
	    {Response, httpd_util:to_lower(Username)};
	{_, _} ->
	    {"", false}
    end.
