-module(directory).
-export([lookupmail/1, ldapmailsearch/2]).

ldapmailsearch(Mail, Attribute) ->
    {ok, Handle} = eldap:open([sipserver:get_env(ldap_server)], []),
    ok = eldap:simple_bind(Handle,
			   sipserver:get_env(ldap_username, ""),
			   sipserver:get_env(ldap_password, "")),
    Filter = eldap:equalityMatch("mail", Mail),
    {ok, Result} = eldap:search(Handle, [{base,
					  sipserver:get_env(ldap_searchbase,
							    "")},
					 {filter, Filter},
					 {attributes,[Attribute]}]),
    eldap:close(Handle),
    {eldap_search_result, List, _} = Result,
    case List of
	[] ->
	    none;
	[{eldap_entry, _, Attributes} | _] ->
	    {value, {Attribute, [Value | _]}} = lists:keysearch(Attribute, 1, Attributes),
	    Value
    end.

do_recv(Sock, Text) ->
    receive
	{tcp, Sock, Data} ->
	    do_recv(Sock, Text ++ Data);
	{tcp_closed, Sock} ->
	    Text
    end.

lookupkthid(KTHid) ->
    Server = "yorick.admin.kth.se",
    {ok, Sock} = gen_tcp:connect(Server, 80, 
				 [list, {packet, 0}]),
    ok = gen_tcp:send(Sock, "GET /service/personsokning/kthid2tele.asp?kthid=" ++ KTHid ++ " HTTP/1.0\r\n\r\n"),
    
    Text = do_recv(Sock, ""),
    {Header, Body} = sippacket:parse_packet(Text),
    {Request, Headerkeylist} = sippacket:parseheader(Header),
    ok = gen_tcp:close(Sock),
    Numbers = string:tokens(Body, "\r\n"),
    case Numbers of
	[] ->
	    none;
	[Number | _] ->
	    Number
    end.

lookupmail(Mail) ->
    case ldapmailsearch(Mail ++ "@kth.se", "uid") of
	none ->
	    lookupkthid(Mail);
	KTHid ->
	    lookupkthid(KTHid)
    end.
