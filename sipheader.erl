-module(sipheader).
-export([to/1, from/1, contact/1, via/1, via_print/1, to_print/1,
	 contact_print/1, auth_print/1, auth_print/2, auth/1, comma/1,
	 httparg/1, cseq/1, cseq_print/1, via_params/1, contact_params/1,
	 build_header/1, dict_to_param/1]).

comma(String) ->
    comma([], String, false).

% comma(Parsed, Rest, Inquote)

comma(Parsed, [$\\, Char | Rest], true) ->
    comma(Parsed ++ [$\\, Char], Rest, true);
comma(Parsed, [$" | Rest], false) ->
    comma(Parsed ++ [$"], Rest, true);
comma(Parsed, [$" | Rest], true) ->
    comma(Parsed ++ [$"], Rest, false);
comma(Parsed, [$, | Rest], false) ->
    [string:strip(Parsed, both) | comma([], Rest, false)];
comma(Parsed, [Char | Rest], Inquote) ->
    comma(Parsed ++ [Char], Rest, Inquote);
comma(Parsed, [], false) ->
    [string:strip(Parsed, both)].

% name-addr = [ display-name ] "<" addr-spec ">"
% display-name = *token | quoted-string


% {Displayname, URL}

to([String]) ->
    name_header(String).

from([String]) ->
    name_header(String).

contact([]) ->
    [];
    
contact([String | Rest]) ->
    Headers = comma(String),
    lists:append(lists:map(fun(H) ->
				   parse_contact(H)
			   end, Headers),
		 contact(Rest)).

parse_contact("'*'") ->
    {none, {wildcard, []}};

parse_contact("*") ->
    {none, {wildcard, []}};

parse_contact("'*';" ++ ParamStr) ->
    Parameters = string:tokens(ParamStr, ";"),
    {none, {wildcard, Parameters}};

parse_contact("*;" ++ ParamStr) ->
    Parameters = string:tokens(ParamStr, ";"),
    {none, {wildcard, Parameters}};

parse_contact(String) ->
    name_header(String).

contact_params({_, {wildcard, Parameters}}) ->
    param_to_dict(Parameters);
contact_params({_, _, _, _, Parameters}) ->
    param_to_dict(Parameters).

via([]) ->
    [];
via([String | Rest]) ->
    Headers = comma(String),
    lists:append(lists:map(fun(H) ->
				   [Protocol, Sentby] = string:tokens(H, " "),
				   [Hostport | Parameters ] = string:tokens(Sentby, ";"),
				   {Protocol, sipurl:parse_hostport(Hostport), Parameters}
			   end, Headers),
		 via(Rest)).

print_parameters([]) ->
    "";
print_parameters([A | B]) ->
    ";" ++ A ++ print_parameters(B).

via_print(Via) ->
    lists:map(fun(H) ->
		      {Protocol, {Host, Port}, Parameters} = H,
		      Protocol ++ " " ++ sipurl:print_hostport(Host, Port) ++ print_parameters(Parameters)
	      end, Via).

via_params({Protocol, Hostport, Parameters}) ->
    param_to_dict(Parameters).

contact_print(Contact) ->
    lists:map(fun(H) ->
		      name_print(H)
	      end, Contact).

to_print(To) ->
    name_print(To).

name_print({_, wildcard, Parameters}) ->
    sipurl:print({wildcard, Parameters});
    
name_print({none, URI}) ->
    sipurl:print(URI);

name_print({Name, URI}) ->
    "\"" ++ Name ++ "\" <" ++ sipurl:print(URI) ++ ">".

unquote([$" | QString]) ->
    Index = string:chr(QString, $"),
    string:substr(QString, 1, Index - 1);

unquote(QString) ->
    QString.

name_header([$" | String]) ->
    Index1 = string:chr(String, $"),
    QString = string:substr(String, Index1),
    Index2 = string:chr(QString, $"),
    Displayname = string:substr(QString, 1, Index2 - 1),
    Rest = string:strip(string:substr(QString, Index2 + 1), left),
    case Rest of
	[$< | Rest2] ->
	    Index3 = string:chr(Rest2, $>),
	    URL = string:substr(Rest2, 1, Index3 - 1),
	    URI = sipurl:parse(URL),
	    {Displayname, URI}
    end;

name_header(String) ->
    %logger:log(debug, "n: ~p", [String]),
    case String of
	[$< | Rest2] ->
	    Index2 = string:chr(Rest2, $>),
	    URL = string:substr(Rest2, 1, Index2 - 1),
	    URI = sipurl:parse(URL),
	    {none, URI};
	URL ->
	    URI = sipurl:parse(URL),
	    {none, URI}
    end.

auth_print(Auth) ->
    auth_print(Auth, false).

auth_print(Auth, Stale) ->
    {Realm, Nonce, Opaque} = Auth,
    ["Digest realm=\"" ++ Realm ++ "\", nonce=\"" ++ Nonce ++ "\", opaque=\"" ++ Opaque ++ "\"" ++
     case Stale of
	 true ->
	     ", stale=true";
	 _ ->
	     ""
     end
    ].

auth(["GSSAPI " ++ String]) ->
    Headers = comma(String),
    L = lists:map(fun(A) ->
			  H = string:strip(A,left),
			  Index = string:chr(H, $=),
			  Name = string:substr(H, 1, Index - 1),
			  Value = string:substr(H, Index + 1),
			  
			  {Name, unquote(Value)}
		  end, Headers),
    dict:from_list(L);

auth(["Digest " ++ String]) ->
    Headers = comma(String),
    L = lists:map(fun(A) ->
			  H = string:strip(A,left),
			  Index = string:chr(H, $=),
			  Name = string:substr(H, 1, Index - 1),
			  Value = string:substr(H, Index + 1),
			  
			  {Name, unquote(Value)}
		  end, Headers),
    dict:from_list(L).

unescape([]) ->
    [];
unescape([$%, C1, C2 | Rest]) ->
    [hex:from([C1, C2]) | unescape(Rest)];
unescape([C | Rest]) ->
    [C | unescape(Rest)].

param_to_dict(Param) ->
    L = lists:map(fun(A) ->
			  H = string:strip(A,left),
			  Index = string:chr(H, $=),
			  Name = httpd_util:to_lower(string:substr(H, 1, Index - 1)),
			  Value = string:substr(H, Index + 1),
			  {Name, unescape(Value)}
		  end, Param),
    dict:from_list(L).    

dict_to_param(Dict) ->
    list_to_parameters(dict:to_list(Dict)).
    
list_to_parameters([]) ->
    [];
list_to_parameters([{Key, Value}]) ->
    [Key ++ "=" ++ Value];
list_to_parameters([{Key, Value} | Rest]) ->
    [Key ++ "=" ++ Value | list_to_parameters(Rest)].
    

httparg(String) ->
    Headers = string:tokens(String, "&"),
    param_to_dict(Headers).

cseq([String]) ->
    [Seq, Method] = string:tokens(String, " "),
    {Seq, Method}.

cseq_print({Seq, Method}) ->
    Seq ++ " " ++ Method.

print_one_header({Name, Value}) ->
    Name ++ ": " ++ util:join(Value, ",").

build_header(Header) ->
    Lines = lists:map(fun print_one_header/1, Header),
    util:concat(Lines, "\r\n").
