%%%-------------------------------------------------------------------
%%% File    : local_su_se.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Local functionality for Stockholm university (su.se).
%%%
%%% @since    10 May 2003 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @hidden
%%%-------------------------------------------------------------------
-module(local_su_se).

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
-export([
	 canonify_user/1,
	 lookup_homedomain_url/1,
	 format_number_for_remote_party_id/3,
	 get_classes_for_user/1,
	 get_telephonenumber_for_user/1,
	 can_use_address/2,
	 can_use_address_detail/2,
	 canonify_authusername/2,
	 create_dialog_state_uas/4
	]).

-include("siprecords.hrl").
-include("directory.hrl").

% Turn a SIP username into an address which can be reached from anywhere.
% Used for example from the Mnesia userdb-module. It should be possible
% to call Mnesia users based on their username, but the username might
% need sip: prepended to it, or a default domain name appended to it.
canonify_user("sip:" ++ User) ->
    "sip:" ++ User;
canonify_user(Fulluser) ->
    case string:tokens(Fulluser, "@") of
        [_User, _Host] ->
            "sip:" ++ Fulluser;
        [User] ->
	    "sip:" ++ User ++ "@sip.su.se"
    end.


%% Routing hooks
%%%%%%%%%%%%%%%%%

%% This function looks up the telephone number of the user in LDAP and
%% if a telephone number is found, looks it up using lookuppotn() which
%% first looks in ENUM and secondly looks for a PSTN gateway for the number
lookup_homedomain_url(URL) when is_record(URL, sipurl) ->
    {User, Host} = {URL#sipurl.user, URL#sipurl.host},
    Mail = User ++ "@" ++ Host,
    Loc1 = mail2tel(Mail),
    logger:log(debug, "Local routing: mail2tel ~p -> ~p", [Mail, Loc1]),
    case Loc1 of
	["+" ++ E164] ->
	    Loc2 = local:lookuppotn("+" ++ E164),
	    case Loc2 of
	        {proxy, Dst1} when is_record(Dst1, sipurl) ->
		    case sipurl:url_is_equal(URL, Dst1, [user, host]) of
			true ->
			    logger:log(debug, "Routing: POTN lookup resulted in what I started with, going for PSTN instead"),
			    local:lookuppstn("+" ++ E164);
			false ->
			    {proxy, Dst1}
		    end;
		Loc2 ->
		    Loc2
	    end;
	_ ->
	    Loc1
    end.

mail2tel(Mail) ->
    Loc1 = directory:lookup_mail2tel(Mail),
    case Loc1 of
	none ->
	    case yxa_config:get_env(ldap_server) of
		{ok, LDAPserver} ->
		    %% same as directory:lookup_mail2tel() but looks for the mail address in the
		    %% mailLocalAddress attribute
		    Res = directory:ldapsearch_simple(LDAPserver, "mailLocalAddress", Mail, "telephoneNumber"),
		    logger:log(debug, "Local: LDAP mailLocalAddress -> telephoneNumber lookup on ~p -> ~p",
			       [Mail, Res]),
		    Res;
		none ->
		    none
	    end;
	Loc1 ->
	    Loc1
    end.

-define(E164toInternal, [{"^\\+46816([0-9][0-9][0-9][0-9])\$",    "\\1"},
			 {"^\\+468674(7[0-9][0-9][0-9])\$",       "\\1"},
			 {"^\\+4685537(8[0-9][0-9][0-9]+)\$",   "\\1"}]).

%% Returns: {ok, Number} | none
format_number_for_remote_party_id(Number, _Header, "sip1.telia.com") ->
    {ok, local:rewrite_potn_to_e164(Number)};
format_number_for_remote_party_id(Number, _Header, "sip2.telia.com") ->
    {ok, local:rewrite_potn_to_e164(Number)};
format_number_for_remote_party_id(Number, _Header, _DstHost) ->
    %% Rewrite number to an internal number if the gateway is not one of Telias
    case util:regexp_rewrite(Number, ?E164toInternal) of
	nomatch ->
	    {ok, Number};
	Match ->
	    {ok, Match}
    end.


get_classes_for_user(User) ->
    Res = sipuserdb:get_classes_for_user(User),
    case Res of
	nomatch ->
	    case yxa_config:get_env(ldap_server) of
		{ok, Server} ->
		    case get_classes_for_user_in_domain(Server, User) of
			{ok, Classes} when is_list(Classes) ->
			    %% Override with default classes for domain user since sipuserdb returned 'nomatch'
			    Classes;
			nomatch ->
			    Res
		    end;
		none ->
		    none
	    end;
	_ ->
	    Res
    end.

%% get_classes_for_user_in_domain/2, part of get_classes_for_user/2.
%% Determine if User belongs to "it.su.se" or not. Returns : {ok, Classes} (list() of atom()) | nomatch
get_classes_for_user_in_domain(Server, User) when is_list(Server), is_list(User) ->
    case directory:ldapsearch(Server, "sipAuthenticationUser", User, ["sipAuthenticationUser"]) of
	[LR] when is_record(LR, ldapres) ->
	    Dn = LR#ldapres.dn,
	    {ok, Regexps} = yxa_config:get_env(local_su_domain_classes, []),
	    get_classes_for_user_in_domain2(User, Dn, Regexps);
	_ ->
	    nomatch
    end.

get_classes_for_user_in_domain2(User, Dn, [{Regexp, ClassL} | T]) when is_list(User), is_list(Dn),
								       is_list(Regexp), is_list(ClassL) ->
    case regexp:first_match(Dn, Regexp) of
	{match, _, _} ->
	    logger:log(debug, "local: User ~p (dn ~p) matches dn-regexp ~p -> ~p",
		       [User, Dn, Regexp, ClassL]),
	    {ok, ClassL};
	nomatch ->
	    get_classes_for_user_in_domain2(User, Dn, T);
	{error, E} ->
	    logger:log(normal, "Error in local_su_domain_classes regexp ~p: ~p", [Regexp, E]),
	    get_classes_for_user_in_domain2(User, Dn, T)
    end;
get_classes_for_user_in_domain2(User, Dn, []) ->
    logger:log(debug, "local: User ~p (dn ~p) does not match any of my domains with default classes",
	      [User, Dn]),
    nomatch.


get_telephonenumber_for_user("ft.sip1") ->
    "1225";
get_telephonenumber_for_user("ft.sip2") ->
    "1225";
get_telephonenumber_for_user(User) ->
    sipuserdb:get_telephonenumber_for_user(User).


%% AAA hooks
%%%%%%%%%%%%%

%% Returns: true | false
can_use_address("ata-hage10", URL) when is_record(URL, sipurl) ->
    true;
can_use_address(User, URL) when is_list(User), is_record(URL, sipurl) ->
    sipauth:can_use_address(User, URL).

%% Returns: {Verdict, Reason}
can_use_address_detail("ata-hage10", URL) when is_record(URL, sipurl) ->
    {true, ok};
can_use_address_detail(User, URL) when is_list(User), is_record(URL, sipurl) ->
    sipauth:can_use_address_detail(User, URL).


%% Returns : NewUsername | undefined
canonify_authusername("ft", Header) when is_record(Header, keylist) ->
    {_, FromURL} = sipheader:from(Header),
    canonify_authusername2("ft", FromURL);
canonify_authusername(Username, Header) when is_list(Username), is_record(Header, keylist) ->
    undefined.

canonify_authusername2("ft", #sipurl{user="ft", host="it.su.se"}=FromURL) when is_record(FromURL, sipurl) ->
    logger:log(debug, "Auth: Using authentication username of 'ft.sip1' for this request"),
    "ft.sip1";
canonify_authusername2("ft", #sipurl{user="ft", host="pappersk.org"}=FromURL) when is_record(FromURL, sipurl) ->
    logger:log(debug, "Auth: Using authentication username of 'ft.sip2' for this request"),
    "ft.sip2";
canonify_authusername2("ft", _URL) ->
    undefined.


create_dialog_state_uas(subscription, Request, ToTag, MyContact) ->
    {ok, Dialog} = sipdialog:create_dialog_state_uas(Request, ToTag, MyContact),
    case keylist:fetch('user-agent', Request#request.header) of
	["Hotsip-CallTron/4.3.1.1212"] ->
	    [Contact] = contact:parse([Dialog#dialog.remote_target]),
	    logger:log(debug, "local: Hotsip UA detected, verifying Contact (~s)", [Contact#contact.urlstr]),
	    URL = sipurl:parse(Contact#contact.urlstr),
	    case local:get_locations_with_contact(URL) of
		{ok, []} ->
		    %% Contact is not the registered contact, have to figure out the
		    %% registered one in order to get incomingproxy/outgoingproxy to
		    %% relay the request without requiring authentication
		    {_DisplayName, FromURL} = sipheader:from(Request#request.header),
		    case find_real_contact(FromURL, URL, Dialog#dialog.remote_target) of
			{ok, RealContact} ->
			    {ok, Dialog#dialog{remote_target = RealContact}};
			none ->
			    logger:log(debug, "local: Found no substitute Contact for ~s (bad thing!)",
				       [Dialog#dialog.remote_target]),
			    {ok, Dialog}
		    end;
		{ok, Res} ->
		    logger:log(debug, "local: Found location(s) for contact : ~p", [Res]),
		    {ok, Dialog}
	    end;
	_ ->
	    {ok, Dialog}
    end;
create_dialog_state_uas(_Caller, Request, ToTag, Contact) ->
    sipdialog:create_dialog_state_uas(Request, ToTag, Contact).

find_real_contact(FromURL, URL, RemoteTarget) ->
    case local:lookup_url_to_locations(FromURL) of
	Locations when is_list(Locations) ->
	    logger:log(debug, "local: From: resolved to a number of locations : ~p", [Locations]),
	    case find_matching_location(URL, Locations) of
		{ok, Address} ->
		    NewContactStr = "<" ++ sipurl:print(Address) ++ ">",
		    logger:log(debug, "local: Using Contact ~s instead of ~s", [NewContactStr, RemoteTarget]),
		    {ok, NewContactStr};
		none ->
		    none
	    end;
	nomatch ->
	    %% nothing we can do
	    logger:log(debug, "local: Found no locations for From: URL : (~s) - nothing I can do (bad thing!)",
		       [sipurl:print(FromURL)]),
	    none
    end.

find_matching_location(URL, [#siplocationdb_e{address = URL2} = H | T]) when is_record(URL2, sipurl) ->
    case sipurl:url_is_equal(URL, URL2, [proto, host, port]) of
	true ->
	    {ok, siplocation:to_url(H)};
	false ->
	    find_matching_location(URL, T)
    end;
find_matching_location(_URL, []) ->
    none.
