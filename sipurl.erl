-module(sipurl).
-export([parse/1, parse_hostport/1, print/1, print_hostport/2]).

parse("sip:" ++ URL) ->
%    logger:log(debug, "url: ~p", [URL]),
    [Rest | Parameters ] = string:tokens(URL, ";"),
    case string:tokens(Rest, "@") of
	[Userinfo, Hostport] ->
	    {User, Pass} = parse_userinfo(Userinfo),
	    {Host, Port} = parse_hostport(Hostport),
	    {User, Pass, Host, Port, Parameters};
	[Hostport] ->
	    {Host, Port} = parse_hostport(Hostport),
	    {none, none, Host, Port, Parameters}
    end;
parse(URL) ->
    {unparseable, URL}.

print_parameters([]) ->
    "";

print_parameters([A | B]) ->
    ";" ++ A ++ print_parameters(B).

print({wildcard, Parameters}) ->
    "*" ++ print_parameters(Parameters);

print({none, none, Host, Port, Parameters}) ->
    "sip:" ++ print_hostport(Host, Port) ++ print_parameters(Parameters);

print({User, Pass, Host, Port, Parameters}) ->
    "sip:" ++ print_userinfo(User, Pass) ++ "@" ++
	print_hostport(Host, Port) ++
	print_parameters(Parameters).

print_userinfo(User, none) ->
    User;

print_userinfo(User, Pass) ->
    User ++ ":" ++ Pass.

print_hostport(Host, none) ->
    Host;

print_hostport(Host, Port) ->
    Host ++ ":" ++ Port.

parse_userinfo(Userinfo) ->
    case string:tokens(Userinfo, ":") of
	[User, Pass] ->
	    {User, Pass};
	[User] ->
	    {User, none};
	[] ->
	    {none, none}
    end.

parse_hostport(Hostport) ->
    case string:tokens(Hostport, ":") of
	[Host, Port] ->
	    {Host, Port};
	[Host] ->
	    {Host, none}
    end.
