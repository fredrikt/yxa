-module(directory).
-export([ldapmailsearch/2]).

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
