-module(directory).
-export([ldapsearch/4, lookup_mail2tel/1, lookup_mail2uid/1, lookup_mail2cn/1]).

ldapsearch(Server, Type, In, Attribute) ->
    {ok, Handle} = eldap:open([Server], []),
    ok = eldap:simple_bind(Handle,
			   sipserver:get_env(ldap_username, ""),
			   sipserver:get_env(ldap_password, "")),
    Filter = eldap:equalityMatch(Type, In),
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
	[{error, Msg}] ->
	    logger:log(normal, "LDAP search failed: ~p", [Msg]),
	    none;
	[{eldap_entry, _, Attributes} | _] ->
	    case Attributes of
		[] ->
		    none;
		Attributes ->
		    {value, {Attribute, [Value | _]}} = lists:keysearch(Attribute, 1, Attributes),
		    Value
	    end;
	_ ->
	    none
    end.

lookup_mail2tel(Mail) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    none;
	Server ->
	    Res = ldapsearch(Server, "mail", Mail, "telephoneNumber"),
	    logger:log(debug, "Directory: LDAP telephoneNumber lookup on ~p -> ~p", [Mail, Res]),
	    Res
    end.

lookup_mail2uid(Mail) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    none;
	Server ->
	    Res = ldapsearch(Server, "mail", Mail, "uid"),
	    logger:log(debug, "Directory: LDAP uid lookup on ~p -> ~p", [Mail, Res]),
	    Res
    end.

lookup_mail2cn(Mail) ->
    case sipserver:get_env(ldap_server, none) of
	none ->
	    none;
	Server ->
	    Res = ldapsearch(Server, "mail", Mail, "cn"),
	    logger:log(debug, "Directory: LDAP server ~s cn lookup on ~p -> ~p", [Server, Mail, Res]),
	    Res
    end.
