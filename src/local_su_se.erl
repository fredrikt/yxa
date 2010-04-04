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
%%-compile(export_all).

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
-export([
	 init/0,
	 canonify_user/1,
	 lookup_homedomain_url/1,
	 format_number_for_remote_party_id/3,
	 get_classes_for_user/1,
	 get_telephonenumber_for_user/1,
	 can_use_address/2,
	 can_use_address_detail/2,
	 canonify_authusername/2,
	 create_dialog_state_uas/4,
	 pstnproxy_lookup_action/2,
	 pstnproxy_allowed_proxy_request/2,
	 eventserver_locationdb_action/3,
	 is_allowed_pstn_dst/4
	]).

-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("directory.hrl").
-include("pstnproxy.hrl").

init() ->
    ok = su_pstnproxy_policy:init(),
    ok.

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
format_number_for_remote_party_id(Number, Header, DstHost) ->
    case re:run(DstHost, "\\.su\\.se$", [{capture, none}]) of
	match ->
	    %% Rewrite number to an internal number if the gateway is not one of Telias
	    case util:regexp_rewrite(Number, ?E164toInternal) of
		nomatch ->
		    {ok, Number};
		Match ->
		    {ok, Match}
	    end;
	nomatch ->
	    %% not su.se gateway (for example unit test), perform default action
	    lookup:format_number_for_remote_party_id(Number, Header, DstHost)
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
    try re:run(Dn, Regexp, [{capture, none}]) of
	match ->
	    logger:log(debug, "local: User ~p (dn ~p) matches dn-regexp ~p -> ~p",
		       [User, Dn, Regexp, ClassL]),
	    {ok, ClassL};
	nomatch ->
	    get_classes_for_user_in_domain2(User, Dn, T)
    catch
	error:
	  badarg ->
	    logger:log(normal, "Error in local_su_domain_classes regexp ~p", [Regexp]),
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
canonify_authusername2("ft", #sipurl{user="ft", host="thulin.net"}=FromURL) when is_record(FromURL, sipurl) ->
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


pstnproxy_lookup_action(Request, PstnCtx) when is_record(Request, request),
					       is_record(PstnCtx, pstn_ctx) ->
    case catch yxa_config:get_env(local_internal_enum_domains) of
	{ok, ENUMDomains} ->
	    Number = (Request#request.uri)#sipurl.user,
	    logger:log(debug, "local: Resolving number ~p in internal ENUM routing domain(s) : ~p",
		       [Number, ENUMDomains]),
	    case lookup:rewrite_potn_to_e164(Number) of
		error ->
		    logger:log(debug, "local: Number not found in internal ENUM domains"),
		    undefined;
		"+" ++ E164 ->
		    case dnsutil:enumlookup("+" ++ E164, ENUMDomains) of
			none ->
			    undefined;
			Res when is_list(Res) ->
			    case sipurl:parse(Res) of
				URL when is_record(URL, sipurl) ->
				    NewPstnCtx =
					PstnCtx#pstn_ctx{destination   = pstn,
							 called_number = Number
							},
				    {ok, {proxy, URL}, NewPstnCtx};
				Bad ->
				    logger:log(error, "local: Can't parse internal ENUM result "
					       "(number ~p, domains ~p, string ~p) : ~p",
					       [Number, ENUMDomains, Res, Bad]),
				    undefined
			    end
		    end
	    end;
	_ ->
	    undefined
    end.

pstnproxy_allowed_proxy_request(#request{method = "REFER"} = Request, PstnCtx) when is_record(PstnCtx, pstn_ctx) ->
    case do_local_su_se_allow_icepeak_refer(Request, PstnCtx) of
	undefined ->
	    do_local_su_se_allow_internal_refer(Request, PstnCtx);
	Res when is_boolean(Res) ->
	    Res
    end;

pstnproxy_allowed_proxy_request(_Request, _PstnCtx) ->
    %% non-REFER
    undefined.

do_local_su_se_allow_icepeak_refer(Request, PstnCtx) when is_record(Request, request), is_record(PstnCtx, pstn_ctx) ->
    case catch yxa_config:get_env(local_su_se_allow_icepeak_refer_from) of
	{ok, Icepeaks} when is_list(Icepeaks) ->
	    IP = PstnCtx#pstn_ctx.ip,
	    IP_match = lists:member(IP, Icepeaks),
	    FromTag = (catch sipheader:get_tag(keylist:fetch('from', Request#request.header))),
	    ToTag = (catch sipheader:get_tag(keylist:fetch('to', Request#request.header))),
	    if
		IP_match, is_list(FromTag), is_list(ToTag) ->
		    logger:log(debug, "local: Allowing Icepeak REFER (from: ~p, has both from- and to-tags)", [IP]),
		    true;
		true ->
		    logger:log(debug, "local: Icepeak refer allow: NO (IP match ~p, ft ~p, tt ~p)",
			       [IP_match, FromTag, ToTag]),
		    undefined
	    end;
	_ ->
	    logger:log(debug, "local: local_su_se_allow_icepeak_refer_from not set, not checking if REFER "
		       "should be allowed"),
	    undefined
    end.

do_local_su_se_allow_internal_refer(Request, _PstnCtx) when is_record(Request, request) ->
    case catch yxa_config:get_env(local_su_se_allow_internal_refer) of
	{ok, true} ->
	    case keylist:fetch("Refer-To", Request#request.header) of
		[ReferTo] ->
		    case sipurl:parse(ReferTo) of
			ReferURL when is_record(ReferURL, sipurl) ->
			    case allow_refer_replaces(ReferURL) of
				Res when is_boolean(Res) -> Res;
				undefined ->
				    allow_refer_userpart(ReferURL)
			    end;
			_ ->
			    logger:log(debug, "local: Invalid Refer-To header in REFER"),
			    undefined
		    end;
		_ ->
		    logger:log(debug, "local: No Refer-To header in REFER"),
		    undefined
	    end;
	_ ->
	    logger:log(debug, "local: local_su_se_allow_internal_refer not set, not checking if REFER "
		       "should be allowed"),
	    undefined
    end.



allow_refer_replaces(URL) when is_record(URL, sipurl) ->
    case url_param:find(URL#sipurl.param_pairs, "Replaces") of
	[Replaces] ->
	    logger:log(debug, "local: Allowing REFER with Replaces (~s)", [Replaces]),
	    true;
	[] ->
	    undefined
    end.

allow_refer_userpart(#sipurl{user = User}) when is_list(User) ->
    DstNumber =
	case local:rewrite_potn_to_e164(User) of
	    error -> User;
	    N when is_list(N) -> N
	end,

    {ok, Classdefs} = yxa_config:get_env(classdefs),
    {ok, Class} = sipauth:classify_number(DstNumber, Classdefs),
    logger:log(normal, "local: Checking if we should allow REFER to ~s (class: ~p)",
	       [DstNumber, Class]),
    case Class of
	internal ->
	    logger:log(normal, "local: Allowing REFER to internal number ~p",
		       [DstNumber]),
	    true;
	_ ->
	    undefined
    end;
allow_refer_userpart(_URL) ->
    undefined.


eventserver_locationdb_action(insert, "ft", Location) ->
    Params =
	case lists:keysearch(path, 1, Location#siplocationdb_e.flags) of
	    {value, {path, Path1}} ->
		[{path, Path1}];
	    false ->
		[]
	end,
    [{shared_line,
      "resource",
      "ft",
      siplocation:to_url(Location),
      Params
     }];
eventserver_locationdb_action(insert, "ft2", Location) ->
    Params =
	case lists:keysearch(path, 1, Location#siplocationdb_e.flags) of
	    {value, {path, Path1}} ->
		[{path, Path1}];
	    false ->
		[]
	end,
    [{shared_line,
      "resource",
      "ft2",
      siplocation:to_url(Location),
      Params
     }];
eventserver_locationdb_action(_, _, _) ->
    undefined.


%%--------------------------------------------------------------------
%% @spec    (User, ToNumber, Header, Class) -> bool()
%%
%%            User     = string() "authenticated SIP username"
%%            ToNumber = string() "phone number - E.164 if conversion was possible, otherwise it is the number as entered by the caller"
%%            Header   = #keylist{} "SIP header of request"
%%            Class    = undefined | atom()
%%
%% @doc
%% @see      sipauth:is_allowed_pstn_dst/4.
%% @end
%%--------------------------------------------------------------------
is_allowed_pstn_dst(User, ToNumber, Header, Class) when is_list(User), is_list(ToNumber),
							is_record(Header, keylist), is_atom(Class) ->
    try su_pstnproxy_policy:is_allowed_pstn_dst(User, ToNumber, Header, Class) of
	Res when is_boolean(Res) ->
	    logger:log(debug, "Local: su_pstnproxy_policy:is_allowed_pstn_dst/4 -> ~p", [Res]),
	    Res;
	undefined ->
	    logger:log(debug, "Local: su_pstnproxy_policy:is_allowed_pstn_dst/4 -> undefined"),
	    %% fallback
	    sipauth:is_allowed_pstn_dst(User, ToNumber, Header, Class);
	Unknown ->
	    logger:log(error, "Local: su_pstnproxy_policy:is_allowed_pstn_dst/4 BAD RESULT : ~p", [Unknown]),
	    %% fallback
	    sipauth:is_allowed_pstn_dst(User, ToNumber, Header, Class)
    catch
	error:
	  E ->
	    ST = erlang:get_stacktrace(),
	    logger:log(error, "Local: su_pstnproxy_policy:is_allowed_pstn_dst/4 FAILED: ~p, stacktrace : ~p", [E, ST]),
	    %% fallback
	    sipauth:is_allowed_pstn_dst(User, ToNumber, Header, Class);
	  X: Y ->
	    logger:log(error, "Local: su_pstnproxy_policy:is_allowed_pstn_dst/4 FAILED: ~p ~p", [X, Y]),
	    %% fallback
	    sipauth:is_allowed_pstn_dst(User, ToNumber, Header, Class)
    end;

is_allowed_pstn_dst(User, ToNumber, Header, Class) ->
    %% fallback for unrecognized input
    logger:log(debug, "Local: Unrecognized input to is_allowed_pstn_dst/4 : ~p", [[User, ToNumber, Header, Class]]),
    sipauth:is_allowed_pstn_dst(User, ToNumber, Header, Class).
