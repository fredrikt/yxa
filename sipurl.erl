-module(sipurl).
-export([parse/1, parse_hostport/1, print/1]).

parse("sip:" ++ URL) ->
%    logger:log(debug, "url: ~p", [URL]),
    case string:tokens(URL, "@") of
	[Userinfo, Rest] ->
	    [Hostport | Parameters ] = string:tokens(Rest, ";"),
	    {User, Pass} = parse_userinfo(Userinfo),
	    {Host, Port} = parse_hostport(Hostport),
	    {User, Pass, Host, Port, Parameters};
	[Rest] ->
	    [Hostport | Parameters ] = string:tokens(Rest, ";"),
	    {Host, Port} = parse_hostport(Hostport),
	    {none, none, Host, Port, Parameters}
    end.

print_parameters([]) ->
    "";

print_parameters([A | B]) ->
    ";" ++ A ++ print_parameters(B).

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
