-module(sipurl).
-export([parse/1, parse_hostport/1, print/1, print_hostport/2, unescape_user/1, cleanup_hostname/1,
	url_is_equal/2, url_is_equal/3, parse_url_with_default_protocol/2]).

-include("siprecords.hrl").

parse("sip:" ++ URL) ->
%    logger:log(debug, "url: ~p", [URL]),
    [Rest | Parameters ] = string:tokens(URL, ";"),
    case string:tokens(Rest, "@") of
	[Userinfo, Hostport] ->
	    {User, Pass} = parse_userinfo(Userinfo),
	    {Host, Port} = parse_hostport(Hostport),
	    #sipurl{proto="sip", user=User, pass=Pass, host=Host, port=Port, param=Parameters};
	[Hostport] ->
	    {Host, Port} = parse_hostport(Hostport),
	    #sipurl{proto="sip", user=none, pass=none, host=Host, port=Port, param=Parameters}
    end;
parse(URL) ->
    {unparseable, URL}.

print_parameters([]) ->
    "";

print_parameters([A | B]) ->
    ";" ++ A ++ print_parameters(B).

print({wildcard, Parameters}) ->
    "*" ++ print_parameters(Parameters);

print(URL) when record(URL, sipurl), URL#sipurl.proto == undefined ->
    print(URL#sipurl{proto="sip"});

print(URL) when record(URL, sipurl), URL#sipurl.user == none, URL#sipurl.pass == none ->
    URL#sipurl.proto ++ ":" ++ print_hostport(URL#sipurl.host, URL#sipurl.port) ++ print_parameters(URL#sipurl.param);

print(URL) when record(URL, sipurl) ->
    URL#sipurl.proto ++ ":" ++ print_userinfo(URL#sipurl.user, URL#sipurl.pass) ++ "@" ++
	print_hostport(URL#sipurl.host, URL#sipurl.port) ++
	print_parameters(URL#sipurl.param).

print_userinfo(User, none) ->
    escape_user(User);

print_userinfo(User, Pass) ->
    escape_user(User) ++ ":" ++ Pass.

print_hostport(Host, none) ->
    Host;

print_hostport(Host, Port) when integer(Port) ->
    lists:flatten(Host ++ ":" ++ integer_to_list(Port));

print_hostport(Host, Port) when list(Port) ->
    lists:flatten(Host ++ ":" ++ Port).

parse_userinfo(Userinfo) ->
    case string:tokens(Userinfo, ":") of
	[User, Pass] ->
	    {unescape_user(User), Pass};
	[User] ->
	    {unescape_user(User), none};
	[] ->
	    {none, none}
    end.

parse_hostport([$[ | IPv6Hostport]) ->
    HostpartEnd = string:chr(IPv6Hostport, $]),
    IPv6Host = "[" ++ string:substr(IPv6Hostport, 1, HostpartEnd),
    case string:substr(IPv6Hostport, HostpartEnd) of
	"]:" ++ Port ->
	    {cleanup_hostname(IPv6Host), Port};
	_ ->
	    {cleanup_hostname(IPv6Host), none}
    end;
parse_hostport(Hostport) ->
    case string:tokens(Hostport, ":") of
	[Host, Port] ->
	    {cleanup_hostname(Host), Port};
	[Host] ->
	    {cleanup_hostname(Host), none}
    end.

%% remove trailing dot(s) and finally lowercase the hostname
cleanup_hostname(H) ->
    case string:substr(H, length(H), 1) of
        "." ->
	    cleanup_hostname(string:substr(H, 1, length(H) - 1));
	_ ->
	    httpd_util:to_lower(H)
    end.

%% XXX implement this
unescape_user(In) ->
    In.

escape_user(In) ->
    In.

url_is_equal(A, B) when record(A, sipurl), record(B, sipurl) ->
    url_is_equal(A, B, [proto, user, pass, host, port, parameters]).

%% XXX finish url_is_equal
url_is_equal(A, A, _) ->
    true;
url_is_equal(A, B, _) ->
    false.

%% In some places, we allow lookups to result in URL strings without
%% protocol. First try to parse them as-is, and if that does not work
%% then make sure there is no protocol specified that we apparently
%% do not handle, and if not then prepend them with Proto: and try again.
parse_url_with_default_protocol(Proto, URLstr) ->
    case sipurl:parse(URLstr) of
	URL1 when record(URL1, sipurl) ->
	    URL1;
	_ ->
	    UserPart = case string:chr(URLstr, $@) of
			   0 ->
			       [];
			   AtIndex ->
			       %% There is an at-sign in there
			       string:substr(URLstr, 1, AtIndex - 1)
		       end,
	    case string:chr(UserPart, $:) of
		0 ->
		    %% There is no colon in the userpart of URLstr, try with our default protocol
		    case sipurl:parse(Proto ++ ":" ++ URLstr) of
			URL2 when record(URL2, sipurl) ->
			    URL2;
			_ ->
			    error
		    end;
		_ ->
		    %% There is already a protocol in URLstr, but apparently not one
		    %% that sipurl:parse() can handle.
		    error
	    end
    end.

