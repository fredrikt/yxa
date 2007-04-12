%%%-------------------------------------------------------------------
%%% File    : local.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Interface to local functions hooking into lots of
%%%           different parts of the various YXA applications.
%%%
%%% Created : 03 Jan 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(local).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([init/0
	]).

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
-export([
	 url2mnesia_userlist/1,
	 canonify_user/1,
	 canonify_addresses/1
	]).

%% lookup
-export([
	 lookup_homedomain_request/2,
	 lookup_remote_request/2,
	 lookupregexproute/1,
	 lookupuser/1,
	 lookupuser_gruu/2,
	 lookupuser_locations/2,
	 lookup_url_to_locations/1,
	 lookup_url_to_addresses/2,
	 lookup_addresses_to_users/1,
	 lookup_address_to_users/1,
	 lookupappserver/1,
	 lookupdefault/1,
	 lookuppotn/1,
	 lookupnumber/1,
	 lookupenum/1,
	 lookuppstn/1,
	 isours/1,
	 format_number_for_remote_party_id/3,
	 get_remote_party_name/2,
	 get_remote_party_number/4,
	 rewrite_potn_to_e164/1,
	 is_request_to_this_proxy/1,
	 remove_unsuitable_locations/2
	]).

%% siplocation
-export([
	 prioritize_locations/2,
	 homedomain/1,
	 get_locations_for_users/1,
	 get_user_with_contact/1,
	 get_locations_with_contact/1,
	 gruu_make_url/4,
	 is_gruu_url/1
	]).

%% sipauth
-export([
	 get_user_verified/2,
	 get_user_verified_proxy/2,
	 can_use_address/2,
	 can_use_address_detail/2,
	 can_register/2,
	 is_allowed_pstn_dst/4,
	 canonify_authusername/2
	]).

%% sipuserdb
-export([
	 get_addresses_for_user/1,
	 get_addresses_for_users/1,
	 get_users_for_address_of_record/1,
	 get_users_for_addresses_of_record/1,
	 get_users_for_url/1,
	 get_user_with_address/1,
	 get_classes_for_user/1,
	 get_password_for_user/1,
	 get_telephonenumber_for_user/1,
	 get_forwards_for_users/1,
	 sipuserdb_backend_override/3,
	 sipuserdb_mysql_make_sql_statement/2
	]).

%% incomginproxy
-export([
	 incomingproxy_challenge_before_relay/3,
	 incomingproxy_request_homedomain_event/2
	]).

%% pstnproxy
-export([
	 pstnproxy_route_pstn_not_e164/3,
	 pstnproxy_auth_and_tag/4,
	 pstnproxy_allowed_methods/2,
	 pstnproxy_allowed_proxy_request/2,
	 pstnproxy_verify_from/4,
	 pstnproxy_number_based_routing/4,
	 pstnproxy_lookup_action/2
	]).

%% outgoingproxy
-export([
	 outgoingproxy_challenge_before_relay/3
	]).

%% eventserver
-export([
	 get_event_package_module/3,
	 get_all_event_packages/0,
	 eventserver_locationdb_action/3
	]).

%% sippipe
-export([
	 start_sippipe/4,
	 sippipe_received_response/3
	]).

%% cpl_db
-export([
	 user_has_cpl_script/1,
	 user_has_cpl_script/2,
	 get_cpl_for_user/1
	]).

%% transaction layer
-export([
	 start_client_transaction/4,
	 new_request/3,
	 new_response/3
	]).

%% transport layer
-export([
	 is_acceptable_socket/7,
	 is_tls_equivalent/3,
	 get_valid_altnames/3,
	 lookup_sipsocket_blacklist/1
	]).

%% custom log and mail cpl functions
-export([
	 cpl_mail/2,
	 cpl_log/4,
	 cpl_is_log_dest/1
	]).

%% configuration
-export([
	 check_config_type/3,
	 config_is_soft_reloadable/2,
	 config_change_action/3
	]).

%% sipdialog
-export([
	 create_dialog_state_uas/4
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("pstnproxy.hrl").

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(LOCAL_ETS_TABLE_NAME, yxa_hooks).

-define(CHECK_EXPORTED(LocalMacroKey, LocalMacroIfSo, LocalMacroOtherwise),
	case ets:member(?LOCAL_ETS_TABLE_NAME, LocalMacroKey) of
	    true ->
		LocalMacroIfSo;
	    false ->
		LocalMacroOtherwise
	end).


-define(SIPPIPE_TIMEOUT, 900).


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init()
%% Descrip.: Look at the list of exported functions from the module
%%           specified as ?LOCAL_MODULE, and make a cache of which
%%           of _this_ modules functions are overridden in the
%%           ?LOCAL_MODULE module.
%% Returns : ok
%%--------------------------------------------------------------------
init() ->
    ets:new(?LOCAL_ETS_TABLE_NAME, [named_table, set]),
    Exports = ?MODULE:module_info(exports),
    MyLocalExports = ?LOCAL_MODULE:module_info(exports),
    %% Check which of this ('local') modules exported functions are also exported
    %% by the ?LOCAL_MODULE function. The LOCAL_MODULE define is provided at compile
    %% time and can be affected by supplying a --with-local=modulename argument to
    %% the YXA 'configure' script.
    Fun = fun({init, 0}) ->
		  ?LOCAL_MODULE:init(),
		  [];
	     ({module_info, _A}) ->
		  [];
	     ({F, A}) ->
		  case lists:member({F, A}, Exports) of
		      true ->
			  ets:insert(?LOCAL_ETS_TABLE_NAME, {{F, A}, 1}),
			  F;
		      false ->
			  []
		  end
	  end,
    [Fun(V) || V <- MyLocalExports],
    {ok, Count, Descr} = init_get_overridden(),
    logger:log(debug, "Local: Found ~p overriding functions in module '~p' : ~s",
	       [Count, ?LOCAL_MODULE, Descr]),
    ok.

init_get_overridden() ->
    Overridden = ets:tab2list(?LOCAL_ETS_TABLE_NAME),
    {ok, length(Overridden), format_overridden(Overridden, [])}.

format_overridden([{{F, A}, _Foo} | T], Res) ->
    This = lists:concat([F, "/", A]),
    format_overridden(T, [This | Res]);
format_overridden([], Res) ->
    util:join(lists:reverse(Res), ", ").


%%====================================================================
%% Hooks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: url2mnesia_userlist(URL)
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
url2mnesia_userlist(URL) when is_record(URL, sipurl) ->
    ?CHECK_EXPORTED({url2mnesia_userlist, 1},
		    ?LOCAL_MODULE:url2mnesia_userlist(URL),
		    default_url2mnesia_userlist(URL)
		   ).

%%--------------------------------------------------------------------
%% Function: default_url2mnesia_userlist(URL)
%%           URL = sipurl record()
%% Descrip.: Return "user@host" from URL.
%% Returns : term()
%%--------------------------------------------------------------------
default_url2mnesia_userlist(URL) when is_list(URL#sipurl.user) ->
    [URL#sipurl.user ++ "@" ++ URL#sipurl.host];
default_url2mnesia_userlist(_URL) ->
    [].

%%--------------------------------------------------------------------
%% Function: canonify_user(User)
%%           User = string()
%% Descrip.: Turn a SIP username into an address which can be reached
%%           from anywhere. Used for example from the Mnesia
%%           userdb-module. It should be possible to call Mnesia users
%%           based on their username, but the username might need sip:
%%           prepended to it, or a default domain name appended to it.
%% Returns : string()
%%--------------------------------------------------------------------
canonify_user(User) when is_list(User) ->
    ?CHECK_EXPORTED({canonify_user, 1},
		    ?LOCAL_MODULE:canonfiy_user(User),
		    default_canonify_user(User)
		   ).

%%--------------------------------------------------------------------
%% Function: default_canonify_user(User)
%%           User = string()
%% Descrip.:
%% Returns : string()
%%--------------------------------------------------------------------
default_canonify_user("sip:" ++ User) ->
    "sip:" ++ User;
default_canonify_user(Fulluser) ->
    case string:tokens(Fulluser, "@") of
        [_User, _Host] ->
            "sip:" ++ Fulluser;
        [User] ->
	    "sip:" ++ User ++ "@" ++ sipauth:realm()
    end.

%%--------------------------------------------------------------------
%% Function: canonify_addresses(In)
%%           In = list() of string()
%% Descrip.: Canonify a list of addresses. Turn anything numeric into
%%           it's E.164 canonical representation. Used from some
%%           userdb-modules which potentially get non-fully qualified
%%           phone numbers (like local extension numbers) back from
%%           the database.
%% Returns : list() of string()
%%--------------------------------------------------------------------
canonify_addresses(In) when is_list(In) ->
    ?CHECK_EXPORTED({canonify_addresses, 1},
		    ?LOCAL_MODULE:canonfiy_addresses(In),
		    default_canonify_addresses(In)
		   ).

%%--------------------------------------------------------------------
%% Function: default_canonify_addresses(In)
%%           In = list() of string()
%% Descrip.:
%% Returns : list() of string()
%%--------------------------------------------------------------------
default_canonify_addresses(In) when is_list(In) ->
    default_canonify_addresses2(In, []).

default_canonify_addresses2(["tel:+" ++ _ = H | T], Res) ->
    default_canonify_addresses2(T, [H | Res]);
default_canonify_addresses2(["+" ++ Num = H | T], Res) ->
    case util:isnumeric(Num) of
	true ->
	    This = "tel:+" ++ Num,
	    default_canonify_addresses2(T, [This | Res]);
	false ->
	    %% non-numeric part after '+' - leave unaltered
	    default_canonify_addresses2(T, [H | Res])
    end;
default_canonify_addresses2([H | T], Res) ->
    %% outgoingproxy don't have internal_to_e164 configuration parameter, so rewrite_potn_to_e164 might fail
    case (catch rewrite_potn_to_e164(H)) of
	"+" ++ _ = E164->
	    This = "tel:" ++ E164,
	    default_canonify_addresses2(T, [This | Res]);
	_ ->
	    %% could not rewrite using rewrite_potn_to_e164/1 - leave unaltered
	    default_canonify_addresses2(T, [H | Res])
    end;
default_canonify_addresses2([], Res) ->
    lists:reverse(Res).


% Routing hooks
%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: lookup_homedomain_request(Request, Origin)
%%           Request = request record()
%%           Origin  = request record()
%% Descrip.: Determine where to route a request that arrived to the
%%           'incomingproxy' application, destined for a local domain
%%           when it has been determined that the request was not
%%           addressed to one of our users (see local:lookupuser/1).
%%           Return 'none' for default routing.
%% Returns : {proxy, PDst}    | (proxy unauthenticated)
%%           {relay, RDst}    | (relay requiring Proxy-Authentication)
%%           {error, S}       | (reject request with SIP status S)
%%           {response, S, R} | (reject request with 'S R')
%%           {forward, Fwd}   | (forward request to another server)
%%           none
%%           PDst = sipurl record() | list() of sipdst record() | route
%%           RDst = sipurl record() | list() of sipdst record() | route
%%           S    = integer(), SIP status code
%%           R    = string(), SIP reason phrase
%%           Fwd  = sipurl record(), MUST have 'user' and 'pass' set
%%                  to 'none'
%%--------------------------------------------------------------------
lookup_homedomain_request(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    ?CHECK_EXPORTED({lookup_homedomain_request, 2},
		    ?LOCAL_MODULE:lookup_homedomain_request(Request, Origin),
		    none
		   ).

%%--------------------------------------------------------------------
%% Function: lookup_remote_request(Request, Origin)
%%           Request = request record()
%%           Origin  = request record()
%% Descrip.: Determine where to route a request that arrived to the
%%           'incomingproxy' application, destined for a remote
%%           domain. Return 'none' to perform default routing.
%% Returns : {proxy, PDst}	| (proxy unauthenticated)
%%           {relay, RDst}	| (relay requiring Proxy-Authentication)
%%           {error, S}		| (reject request with SIP status S)
%%           {response, S, R}	| (reject request with 'S R')
%%           {forward, Fwd}	| (forward request to another server)
%%           none
%%           PDst = sipurl record() | list() of sipdst record() | route
%%           RDst = sipurl record() | list() of sipdst record() | route
%%           S    = integer(), SIP status code
%%           R    = string(), SIP reason phrase
%%           Fwd  = sipurl record(), MUST have 'user' and 'pass' set
%%                  to 'none'
%%--------------------------------------------------------------------
lookup_remote_request(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    ?CHECK_EXPORTED({lookup_remote_request, 2},
		    ?LOCAL_MODULE:lookup_remote_request(Request, Origin),
		    none
		   ).

%%--------------------------------------------------------------------
%% Function: is_request_to_this_proxy(Request)
%%           Request = request record()
%% Descrip.: Determine if a request is meant for this proxy itself, as
%%           opposed to say a user of the system.
%% @see      lookup:is_request_to_this_proxy/1.
%% Returns : true | false
%%--------------------------------------------------------------------
is_request_to_this_proxy(Request) when is_record(Request, request) ->
    ?CHECK_EXPORTED({is_request_to_this_proxy, 1},
		    ?LOCAL_MODULE:is_request_to_this_proxy(Request),
		    lookup:is_request_to_this_proxy(Request)
		   ).


% lookup.erl hooks
%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: lookupregexproute(User)
%% Descrip.:
%% @see      lookup:lookupregexproute/1
%% Returns : term()
%%--------------------------------------------------------------------
lookupregexproute(User) ->
    ?CHECK_EXPORTED({lookupregexproute, 1},
		    ?LOCAL_MODULE:lookupregexproute(User),
		    lookup:lookupregexproute(User)
		   ).

%%--------------------------------------------------------------------
%% Function: lookupuser(URL)
%%           URL = sipurl record()
%% Descrip.: The main 'give me a set of locations for one of our
%%           users' function that incomingproxy uses, when it
%%           determines that a request is for one of it's homedomains.
%% @see      lookup:lookupuser/1.
%% Returns : {proxy, URL}               |
%%           {relay, URL}               |
%%           {forward, URL}             |
%%           {response, Status, Reason} |
%%           none    | (The user was found but has no locations registered)
%%           nomatch
%%--------------------------------------------------------------------
lookupuser(URL) ->
    ?CHECK_EXPORTED({lookupuser, 1},
		    ?LOCAL_MODULE:lookupuser(URL),
		    lookup:lookupuser(URL)
		   ).

%%--------------------------------------------------------------------
%% Function: lookupuser_gruu(URL, GRUU)
%%           URL  = sipurl record(), Request-URI
%%           GRUU = string()
%% Descrip.: Look up a GRUU. Used by incomingproxy and outgouingproxy.
%% @see      lookup:lookupuser_gruu/2.
%% Returns : {proxy, URL}               |
%%           {relay, URL}               |
%%           {forward, URL}             |
%%           {response, Status, Reason} |
%%           none    | (The user was found but has no locations registered)
%%           nomatch
%%--------------------------------------------------------------------
lookupuser_gruu(URL, GRUU) ->
    ?CHECK_EXPORTED({lookupuser_gruu, 2},
		    ?LOCAL_MODULE:lookupuser_gruu(URL, GRUU),
		    lookup:lookupuser_gruu(URL, GRUU)
		   ).

%%--------------------------------------------------------------------
%% Function: lookupuser_locations(Users, URL)
%%           Users = list() of string(), SIP users to fetch locations
%%                                       of
%%           URL   = sipurl record(), the Request-URI
%% Descrip.: Return all locations for a list of users that is suitable
%%           given a Request-URI. By suitable, we mean that we filter
%%           out SIP locations if Request-URI was SIPS, unless this
%%           proxy is configured not to.
%% @see      lookup:lookupuser_locations/2.
%% Returns : Locations = list() of siplocationdb_e record()
%%--------------------------------------------------------------------
lookupuser_locations(Users, URL) ->
    ?CHECK_EXPORTED({lookupuser_locations, 2},
		    ?LOCAL_MODULE:lookupuser_locations(Users, URL),
		    lookup:lookupuser_locations(Users, URL)
		   ).

%%--------------------------------------------------------------------
%% Function: remove_unsuitable_locations(URL, Locations)
%%           URL      = sipurl record(), Request-URI of request
%%           Location = list() of sipurl record()
%% Descrip.: Apply local policy for what locations are good to use for
%%           a particular Request-URI.
%% @see      lookup:remove_unsuitable_locations/2.
%% Returns : list() of sipurl record()
%%--------------------------------------------------------------------
remove_unsuitable_locations(URL, Locations) when is_record(URL, sipurl), is_list(Locations) ->
    ?CHECK_EXPORTED({remove_unsuitable_locations, 2},
		    ?LOCAL_MODULE:remove_unsuitable_locations(URL, Locations),
		    lookup:remove_unsuitable_locations(URL, Locations)
		   ).

%%--------------------------------------------------------------------
%% Function: lookup_url_to_locations(URL)
%% Descrip.:
%% @see      lookup:lookup_url_to_locations/1.
%% Returns : term()
%%--------------------------------------------------------------------
lookup_url_to_locations(URL) ->
    ?CHECK_EXPORTED({lookup_url_to_locations, 1},
		    ?LOCAL_MODULE:lookup_url_to_locations(URL),
		    lookup:lookup_url_to_locations(URL)
		   ).

%%--------------------------------------------------------------------
%% Function: lookup_url_to_addresses(Src, URL)
%% Descrip.:
%% @see      lookup:lookup_url_to_addresses/2.
%% Returns : term()
%%--------------------------------------------------------------------
lookup_url_to_addresses(Src, URL) ->
    ?CHECK_EXPORTED({lookup_url_to_addresses, 2},
		    ?LOCAL_MODULE:lookup_url_to_addresses(Src, URL),
		    lookup:lookup_url_to_addresses(Src, URL)
		   ).

%%--------------------------------------------------------------------
%% Function: lookup_addresses_to_users(Addresses)
%% Descrip.:
%% @see      lookup:lookup_addresses_to_users/1
%% Returns : term()
%%--------------------------------------------------------------------
lookup_addresses_to_users(Addresses) ->
    ?CHECK_EXPORTED({lookup_addresses_to_users, 1},
		    ?LOCAL_MODULE:lookup_addresses_to_users(Addresses),
		    lookup:lookup_addresses_to_users(Addresses)
		   ).

%%--------------------------------------------------------------------
%% Function: lookup_address_to_users(Address)
%% Descrip.:
%% @see      lookup:lookup_address_to_users/1
%% Returns : term()
%%--------------------------------------------------------------------
lookup_address_to_users(Address) ->
    ?CHECK_EXPORTED({lookup_address_to_users, 1},
		    ?LOCAL_MODULE:lookup_address_to_users(Address),
		    lookup:lookup_address_to_users(Address)
		   ).

%%--------------------------------------------------------------------
%% Function: lookupappserver(Key)
%% Descrip.:
%% @see      lookup:lookupappserver/1
%% Returns : term()
%%--------------------------------------------------------------------
lookupappserver(Key) ->
    ?CHECK_EXPORTED({lookupappserver, 1},
		    ?LOCAL_MODULE:lookupappserver(Key),
		    lookup:lookupappserver(Key)
		   ).

%%--------------------------------------------------------------------
%% Function: prioritize_locations(Key, Locations)
%% Descrip.:
%% @see      siplocation:prioritize_locations/1.
%% Returns : term()
%%--------------------------------------------------------------------
prioritize_locations(Key, Locations) ->
    ?CHECK_EXPORTED({prioritize_locations, 2},
		    ?LOCAL_MODULE:prioritize_locations(Key, Locations),
		    siplocation:prioritize_locations(Locations)
		   ).

%%--------------------------------------------------------------------
%% Function: lookupdefault(URL)
%% Descrip.:
%% @see      lookup:lookupdefault/1.
%% Returns : term()
%%--------------------------------------------------------------------
lookupdefault(URL) ->
    ?CHECK_EXPORTED({lookupdefault, 1},
		    ?LOCAL_MODULE:lookupdefault(URL),
		    lookup:lookupdefault(URL)
		   ).

%%--------------------------------------------------------------------
%% Function: lookuppotn(Number)
%% Descrip.:
%% @see      lookup:lookuppotn/1.
%% Returns : term()
%%--------------------------------------------------------------------
lookuppotn(Number) ->
    ?CHECK_EXPORTED({lookuppotn, 1},
		    ?LOCAL_MODULE:lookuppotn(Number),
		    lookup:lookuppotn(Number)
		   ).

%%--------------------------------------------------------------------
%% Function: lookupnumber(Number)
%% Descrip.:
%% @see      lookup:lookupnumber/1.
%% Returns : term()
%%--------------------------------------------------------------------
lookupnumber(Number) ->
    ?CHECK_EXPORTED({lookupnumber, 1},
		    ?LOCAL_MODULE:lookupnumber(Number),
		    lookup:lookupnumber(Number)
		   ).

%%--------------------------------------------------------------------
%% Function: lookupenum(Number)
%% Descrip.:
%% @see      lookup:lookupenum/1.
%% Returns : term()
%%--------------------------------------------------------------------
lookupenum(Number) ->
    ?CHECK_EXPORTED({lookupenum, 1},
		    ?LOCAL_MODULE:lookupenum(Number),
		    lookup:lookupenum(Number)
		   ).

%%--------------------------------------------------------------------
%% Function: lookuppstn(Number)
%% Descrip.:
%% @see      lookup:lookuppstn/1.
%% Returns : term()
%%--------------------------------------------------------------------
lookuppstn(Number) ->
    ?CHECK_EXPORTED({lookuppstn, 1},
		    ?LOCAL_MODULE:lookuppstn(Number),
		    lookup:lookuppstn(Number)
		   ).

%%--------------------------------------------------------------------
%% Function: isours(URL)
%% Descrip.: lookup:isours/1.
%% @see      foo
%% Returns : term()
%%--------------------------------------------------------------------
isours(URL) ->
    ?CHECK_EXPORTED({isours, 1},
		    ?LOCAL_MODULE:isours(URL),
		    lookup:isours(URL)
		   ).

%%--------------------------------------------------------------------
%% Function: homedomain(Domain)
%%           Domain = string()
%% Descrip.: Check if something is one of our 'homedomains' - a domain
%%           we are the final destination for.
%% @see      lookup:homedomain/1.
%% Returns : true | false
%%--------------------------------------------------------------------
homedomain(Domain) ->
    ?CHECK_EXPORTED({homedomain, 1},
		    ?LOCAL_MODULE:homedomain(Domain),
		    lookup:homedomain(Domain)
		   ).

%%--------------------------------------------------------------------
%% Function: get_remote_party_number(User, Header, URI, DstHost)
%%           User    = string(), SIP authentication username
%%           Header  = keylist record()
%%           URI     = sipurl record(), outgoing Request-URI
%%           DstHost = term(), chosen destination for request
%% Descrip.: This function is used by the pstnproxy to provide a PSTN
%%           gateway with usefull caller-id information. PSTN networks
%%           typically gets upset if the "A-number" (calling party) is
%%           a SIP URL. Different gateways might want the number
%%           formatted differently, thus the DstHost parameter (a TSP
%%           gateway to PSTN might only handle E.164 numbers, while a
%%           PBX might be expecting only a 4-digit extension number).
%% @see      lookup:get_remote_party_number/4.
%% Returns : {ok, Number} |
%%           none
%%           Number = string()
%%--------------------------------------------------------------------
get_remote_party_number(User, Header, URI, DstHost) when is_list(User) ->
    ?CHECK_EXPORTED({get_remote_party_number, 4},
		    ?LOCAL_MODULE:get_remote_party_number(User, Header, URI, DstHost),
		    lookup:get_remote_party_number(User, Header, URI, DstHost)
		   ).

%%--------------------------------------------------------------------
%% Function: format_number_for_remote_party_id(Number, Header, DstHost)
%%           Number  = string(), the number to format
%%           Header  = keylist record()
%%           DstHost = term(), destination for request
%% Descrip.: Hook for the actual formatting once
%%           get_remote_party_number/2 has found a number to be
%%           formatted.
%% @see      lookup:format_number_for_remote_party_id/3.
%% Returns : {ok, Number}
%%           Number = string()
%%--------------------------------------------------------------------
format_number_for_remote_party_id(Number, Header, DstHost) when is_list(Number) ->
    ?CHECK_EXPORTED({format_number_for_remote_party_id, 3},
		    ?LOCAL_MODULE:format_number_for_remote_party_id(Number, Header, DstHost),
		    lookup:format_number_for_remote_party_id(Number, Header, DstHost)
		   ).

%%--------------------------------------------------------------------
%% Function: get_remote_party_name(Key, DstHost)
%%           Key     = string(), number we should turn into a name
%%           DstHost = term(), destination for request
%% Descrip.: When pstnproxy receives a request from a PSTN gateway,
%%           this function is called to see if we can find a nice
%%           Display Name for the calling party. By default, we only
%%           do the actual lookup if we can rewrite Key into a E.164
%%           number.
%% @see      lookup:get_remote_party_name/2.
%% Returns : {ok, DisplayName} |
%%           none
%%           DisplayName = string()
%%--------------------------------------------------------------------
get_remote_party_name(Key, DstHost) ->
    ?CHECK_EXPORTED({get_remote_party_name, 2},
		    ?LOCAL_MODULE:get_remote_party_name(Key, DstHost),
		    case rewrite_potn_to_e164(Key) of
			"+" ++ E164 ->
			    lookup:get_remote_party_name("+" ++ E164, DstHost);
			_ -> none
		    end
		   ).

%%--------------------------------------------------------------------
%% Function: rewrite_potn_to_e164(Key)
%% Descrip.:
%% @see      lookup:rewrite_potn_to_e164/1.
%% Returns : term()
%%--------------------------------------------------------------------
rewrite_potn_to_e164(Key) ->
    ?CHECK_EXPORTED({rewrite_potn_to_e164, 1},
		    ?LOCAL_MODULE:rewrite_potn_to_e164(Key),
		    lookup:rewrite_potn_to_e164(Key)
		   ).


% userdb hooks
%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: get_user_with_address(Address)
%% Descrip.: Looks up exactly one user with an Address. Used for
%%           example in REGISTER. If there are multiple users with an
%%           address, this function returns {error}.
%% @see      sipuserdb:get_user_with_address/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_user_with_address(Address) ->
    ?CHECK_EXPORTED({get_user_with_address, 1},
		    ?LOCAL_MODULE:get_user_with_address(Address),
		    sipuserdb:get_user_with_address(Address)
		   ).

%%--------------------------------------------------------------------
%% Function: get_user_with_address(Address)
%% Descrip.: Looks up all users with a given address. Used to find out
%%           to which users we should send a request.
%% @see      sipuserdb:get_users_for_address_of_record/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    ?CHECK_EXPORTED({get_users_for_address_of_record, 1},
		    ?LOCAL_MODULE:get_users_for_address_of_record(Address),
		    sipuserdb:get_users_for_address_of_record(Address)
		   ).

%%--------------------------------------------------------------------
%% Function: get_users_for_addresses_of_record(Addresses)
%% Descrip.:
%% @see      sipuserdb:get_users_for_addresses_of_record/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_users_for_addresses_of_record(Addresses) ->
    ?CHECK_EXPORTED({get_users_for_addresses_of_record, 1},
		    ?LOCAL_MODULE:get_users_for_addresses_of_record(Addresses),
		    sipuserdb:get_users_for_addresses_of_record(Addresses)
		   ).

%%--------------------------------------------------------------------
%% Function: get_addresses_for_user(User)
%% Descrip.: Gets all addresses for a user. Used for example to check
%%           if a request from a user has an acceptable From: header.
%% @see      sipuserdb:get_addresses_for_user/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_addresses_for_user(User) ->
    ?CHECK_EXPORTED({get_addresses_for_user, 1},
		    ?LOCAL_MODULE:get_addresses_for_user(User),
		    sipuserdb:get_addresses_for_user(User)
		   ).

%%--------------------------------------------------------------------
%% Function: get_addresses_for_users(Users)
%% Descrip.:
%% @see      sipuserdb:get_addresses_for_users/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_addresses_for_users(Users) ->
    ?CHECK_EXPORTED({get_addresses_for_users, 1},
		    ?LOCAL_MODULE:get_addresses_for_users(Users),
		    sipuserdb:get_addresses_for_users(Users)
		   ).

%%--------------------------------------------------------------------
%% Function: get_users_for_url(URL)
%% Descrip.:
%% @see      sipuserdb:get_users_for_url/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_users_for_url(URL) ->
    ?CHECK_EXPORTED({get_users_for_url, 1},
		    ?LOCAL_MODULE:get_users_for_url(URL),
		    sipuserdb:get_users_for_url(URL)
		   ).

%%--------------------------------------------------------------------
%% Function: get_password_for_user(User)
%% Descrip.:
%% @see      sipuserdb:get_password_for_user/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_password_for_user(User) ->
    ?CHECK_EXPORTED({get_password_for_user, 1},
		    ?LOCAL_MODULE:get_password_for_user(User),
		    sipuserdb:get_password_for_user(User)
		   ).

%%--------------------------------------------------------------------
%% Function: get_classes_for_user(User)
%% Descrip.:
%% @see      sipuserdb:get_classes_for_user/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_classes_for_user(User) ->
    ?CHECK_EXPORTED({get_classes_for_user, 1},
		    ?LOCAL_MODULE:get_classes_for_user(User),
		    sipuserdb:get_classes_for_user(User)
		   ).

%%--------------------------------------------------------------------
%% Function: get_telephonenumber_for_user(User)
%% Descrip.:
%% @see      sipuserdb:get_telephonenumber_for_user/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_telephonenumber_for_user(User) ->
    ?CHECK_EXPORTED({get_telephonenumber_for_user, 1},
		    ?LOCAL_MODULE:get_telephonenumber_for_user(User),
		    sipuserdb:get_telephonenumber_for_user(User)
		   ).

%%--------------------------------------------------------------------
%% Function: get_forwards_for_users(Users)
%% Descrip.:
%% @see      sipuserdb:get_forwards_for_users/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_forwards_for_users(Users) ->
    ?CHECK_EXPORTED({get_forwards_for_user, 1},
		    ?LOCAL_MODULE:get_forwards_for_users(Users),
		    sipuserdb:get_forwards_for_users(Users)
		   ).

%%--------------------------------------------------------------------
%% Function: sipuserdb_backend_override(Module, Function, Args)
%%           Module   = atom(), sipuserdb module
%%           Function = atom(), function in Module
%%           Args     = term(), arguments to function
%% Descrip.: Hook to override a specific sipuserdb backend function.
%%           If 'undefined' is returned, the real backend function
%%           will be called
%% Returns : {ok, Res} | undefined
%%--------------------------------------------------------------------
sipuserdb_backend_override(Module, Function, Args) ->
    ?CHECK_EXPORTED({sipuserdb_backend_override, 3},
		    ?LOCAL_MODULE:sipuserdb_backend_override(Module, Function, Args),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: sipuserdb_backend_override(CfgKey, Args)
%%           CfgKey = atom(), sipuserdb_mysql_get_sipuser            |
%%                            sipuserdb_mysql_get_user_for_address   |
%%                            sipuserdb_mysql_get_addresses_for_user |
%%                            sipuserdb_mysql_get_classes_for_user   |
%%	                      sipuserdb_mysql_get_password_for_user  |
%%                            sipuserdb_mysql_get_telephonenumber_for_user
%%           Args   = term(), key(s) to use in SQL query
%% Descrip.: If you need to make SQL statements other than what is
%%           possible using the template-based configuration parameter
%%           possibilitys, do it here. Return 'undefined' to let
%%           sipuserdb_mysql do it's default query construction.
%% Returns : {ok, Res} | undefined
%%           Res = string(), SQL query
%%
%% Note    : You have to mysql:quote() everything you use from Args!
%%--------------------------------------------------------------------
sipuserdb_mysql_make_sql_statement(CfgKey, Args) ->
    ?CHECK_EXPORTED({sipuserdb_mysql_make_sql_statement, 2},
		    ?LOCAL_MODULE:sipuserdb_mysql_make_sql_statement(CfgKey, Args),
		    undefined
		   ).

% Location lookup hooks
%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: get_locations_for_users(Users)
%% Descrip.: Looks up all contacts for a list of users. Used to find
%%           out where a set of users are to see where we should route
%%           a request.
%% @see      siplocation:get_locations_for_users/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_locations_for_users(Users) ->
    ?CHECK_EXPORTED({get_locations_for_users, 1},
		    ?LOCAL_MODULE:get_locations_for_users(Users),
		    siplocation:get_locations_for_users(Users)
		   ).

%%--------------------------------------------------------------------
%% Function: get_user_with_contact(URI)
%% Descrip.: Checks if any of our users are registered at the location
%%           specified. Used to determine if we should proxy requests
%%           to a URI without authorization.
%% @see      siplocation:get_user_with_contact/1.
%% Returns : term()
%%--------------------------------------------------------------------
get_user_with_contact(URI) ->
    ?CHECK_EXPORTED({get_user_with_contact, 1},
		    ?LOCAL_MODULE:get_user_with_contact(URI),
		    siplocation:get_user_with_contact(URI)
		   ).

%%--------------------------------------------------------------------
%% Function: get_locations_with_contact(URI)
%% Descrip.: like get_user_with_contact but returns a list of
%%           siplocationdb_e records instead
%% @see      siplocation:get_locations_with_contact/1.
%% Returns : term()
%%--------------------------------------------------------------------
%% like get_user_with_contact but returns a list of siplocationdb_e records instead
get_locations_with_contact(URI) ->
    ?CHECK_EXPORTED({get_locations_with_contact, 1},
		    ?LOCAL_MODULE:get_locations_with_contact(URI),
		    siplocation:get_locations_with_contact(URI)
		   ).

%%--------------------------------------------------------------------
%% Function: gruu_make_url(User, InstanceId, GRUU, To)
%% Descrip.: Make an URL out of a GRUU. Return 'undefined' for default
%%           algorithm.
%% @see      foo
%% Returns : URL | undefined
%%           URL = sipurl record()
%%--------------------------------------------------------------------
gruu_make_url(User, InstanceId, GRUU, To) ->
    ?CHECK_EXPORTED({gruu_make_url, 4},
		    ?LOCAL_MODULE:gruu_make_url(User, InstanceId, GRUU, To),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: is_gruu_url(URL)
%% Descrip.: Check if an URL possibly is a GRUU we've created.
%% @see      gruu:is_gruu_url/1.
%% Returns : {true, GRUU} | false
%%           GRUU = string()
%%--------------------------------------------------------------------
is_gruu_url(URL) ->
    ?CHECK_EXPORTED({is_gruu_url, 1},
                    ?LOCAL_MODULE:is_gruu_url(URL),
                    gruu:is_gruu_url(URL)
                   ).


% AAA hooks
%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: get_user_verified(Header, Method)
%% Descrip.:
%% @see      sipauth:get_user_verified/2.
%% Returns : term()
%%--------------------------------------------------------------------
get_user_verified(Header, Method) ->
    ?CHECK_EXPORTED({get_user_verified, 2},
		    ?LOCAL_MODULE:get_user_verified(Header, Method),
		    sipauth:get_user_verified(Header, Method)
		   ).

%%--------------------------------------------------------------------
%% Function: get_user_verified_proxy(Header, Method)
%% Descrip.:
%% @see      sipauth:get_user_verified_proxy/2.
%% Returns : term()
%%--------------------------------------------------------------------
get_user_verified_proxy(Header, Method) ->
    ?CHECK_EXPORTED({get_user_verified_proxy, 2},
		    ?LOCAL_MODULE:get_user_verified_proxy(Header, Method),
		    sipauth:get_user_verified_proxy(Header, Method)
		   ).

%%--------------------------------------------------------------------
%% Function: can_use_address(User, URL)
%%           User = string(), SIP authentication username
%%           URL  = sipurl record()
%% Descrip.: Check if a user (authenticated elsewhere) may use an
%%           address. See sipauth module for more information.
%% @see      sipauth:can_use_address/2.
%% Returns : true | false
%%--------------------------------------------------------------------
can_use_address(User, URL) when is_list(User), is_record(URL, sipurl) ->
    ?CHECK_EXPORTED({can_use_address, 2},
		    ?LOCAL_MODULE:can_use_address(User, URL),
		    sipauth:can_use_address(User, URL)
		   ).

%%--------------------------------------------------------------------
%% Function: can_use_address_detail(User, URL)
%%           User = string(), SIP authentication username
%%           URL  = sipurl record()
%% Descrip.: Check if a user (authenticated elsewhere) may use an
%%           address. See sipauth module for more information.
%% @see      sipauth:can_use_address_detail/2.
%% Returns : {Verdict, Reason}
%%           Verdict = true | false
%%           Reason  = ok | eperm | nomatch | error
%%--------------------------------------------------------------------
can_use_address_detail(User, URL) when is_list(User), is_record(URL, sipurl) ->
    ?CHECK_EXPORTED({can_use_address_detail, 2},
		    ?LOCAL_MODULE:can_use_address_detail(User, URL),
		    sipauth:can_use_address_detail(User, URL)
		   ).

%%--------------------------------------------------------------------
%% Function: can_register(Header, ToURL)
%%           Header  = keylist record()
%%           ToURL   = sipurl record()
%% Descrip.: Check if a REGISTER message authenticates OK etc. See
%%           sipauth module for more information.
%% @see      sipauth:can_register/2.
%% Returns : {{Verdict, Reason}, User} |
%%           {stale, User}             |
%%           {false, none}
%%           Verdict = true | false
%%           Reason  = ok | eperm | nomatch | error
%%--------------------------------------------------------------------
can_register(Header, ToURL) when is_record(Header, keylist), is_record(ToURL, sipurl) ->
    ?CHECK_EXPORTED({can_register, 2},
		    ?LOCAL_MODULE:can_register(Header, ToURL),
		    sipauth:can_register(Header, ToURL)
		   ).

%%--------------------------------------------------------------------
%% Function: is_allowed_pstn_dst(User, ToNumber, Header, Class)
%%           User     = string(), authenticated SIP username
%%           ToNumber = string(), phone number - E.164 if conversion
%%                      was possible, otherwise it is the number as
%%                      entered by the caller
%%           Header   = keylist record(), SIP header of request
%%           Class    = undefined | atom()
%% Descrip.:
%% @see      sipauth:is_allowed_pstn_dst/4.
%% Returns : bool()
%%--------------------------------------------------------------------
is_allowed_pstn_dst(User, ToNumber, Header, Class) ->
    ?CHECK_EXPORTED({is_allowed_pstn_dst, 4},
		    ?LOCAL_MODULE:is_allowed_pstn_dst(User, ToNumber, Header, Class),
		    sipauth:is_allowed_pstn_dst(User, ToNumber, Header, Class)
		   ).

%%--------------------------------------------------------------------
%% Function: canonify_authusername(Username, Header)
%%           Username = string()
%%	     Header   = keylist record()
%% Descrip.: Possibly make us use another username for this request.
%%           This is needed if your user database allows more than one
%%           username per user, or if you have clients that does not
%%           allow you to set authorization username explicitly and
%%           the username they assume you have is incorrect.
%% Returns : NewUsername |
%%           undefined
%%           NewUsername = string()
%%--------------------------------------------------------------------
canonify_authusername(Username, Header) when is_list(Username), is_record(Header, keylist) ->
    ?CHECK_EXPORTED({canonify_authusername, 2},
		    ?LOCAL_MODULE:canonify_authusername(Username, Header),
		    undefined
		   ).

% incomingproxy hooks
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: incomingproxy_challenge_before_relay(Origin, Request,
%%                                                Dst)
%% Descrip.: Check if 'incomingproxy' should challenge a request that
%%           it has determined it should relay, or if it should proxy
%%           the request without authorization instead.
%% Returns : term()
%%--------------------------------------------------------------------
incomingproxy_challenge_before_relay(Origin, Request, Dst) when is_record(Origin, siporigin),
								is_record(Request, request) ->
    ?CHECK_EXPORTED({incomingproxy_challenge_before_relay, 3},
		    ?LOCAL_MODULE:incomingproxy_challenge_before_relay(Origin, Request, Dst),
		    true
		   ).

%%--------------------------------------------------------------------
%% Function: incomingproxy_request_homedomain_event(Request, Origin)
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
incomingproxy_request_homedomain_event(Request, Origin) when is_record(Request, request),
							     is_record(Origin, siporigin) ->
    ?CHECK_EXPORTED({incomingproxy_request_homedomain_event, 2},
		    ?LOCAL_MODULE:incomingproxy_request_homedomain_event(Request, Origin),
		    undefined
		   ).


%% pstnproxy hooks
%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: pstnproxy_route_pstn_not_e164(DstNumber, Request,
%%                                         PstnCtx)
%%           DstNumber = string(), typically user-part of Request-URI
%%           Request   = request record()
%%           Origin    = siporigin record()
%%           THandler  = term(), server transaction handler
%% Descrip.: When a request destined for PSTN is received by the
%%           pstnproxy, and no destination is found using
%%           local:lookuppstn(), this function is called. The return
%%           values have the following meaning :
%%
%%             undefined - proceed with default behavior
%%             nomatch   - there is no destination for DstNumber,
%%                         reject request with a '404 Not Found'
%%             ignore    - pstnproxy should do nothing further (this
%%                         function must generate a final response)
%%             Response  - send a response
%%             Relay     - send Request to DstURI
%%
%% Returns : undefined | nomatch | ignore | Relay
%%           Relay = {relay, DstURI}
%%           Response = {response, Status, Reason, ExtraHeaders}
%%--------------------------------------------------------------------
pstnproxy_route_pstn_not_e164(DstNumber, Request, PstnCtx) ->
    ?CHECK_EXPORTED({pstnproxy_route_pstn_not_e164, 3},
		    ?LOCAL_MODULE:pstnproxy_route_pstn_not_e164(DstNumber, Request, PstnCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: pstnproxy_auth_and_tag(Request, Origin, THandler,
%%                                  PstnCtx)
%%           Request  = request record()
%%           Origin   = siporigin record()
%%           THandler = term(), server transaction handle
%%           PstnCtx  = pstn_ctx record()
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
%% Returns: pstn_ctx record()
pstnproxy_auth_and_tag(Request, Origin, THandler, PstnCtx) when is_record(Request, request),
								is_record(Origin, siporigin),
								is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_auth_and_tag, 4},
		    ?LOCAL_MODULE:pstnproxy_auth_and_tag(Request, Origin, THandler, PstnCtx),
		    PstnCtx
		   ).

%%--------------------------------------------------------------------
%% Function: pstnproxy_allowed_methods(Request, PstnCtx)
%%           Request = request record()
%%           PstnCtx = pstn_ctx record()
%% Descrip.: Return list of allowed SIP methods. Must be upper-cased.
%% Returns : {ok, AllowedMethods}
%%           AllowedMethods = list() of string()
%%--------------------------------------------------------------------
pstnproxy_allowed_methods(Request, PstnCtx) when is_record(Request, request),
						 is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_allowed_methods, 2},
		    ?LOCAL_MODULE:pstnproxy_allowed_methods(Request, PstnCtx),
		    yxa_config:get_env(allowed_request_methods)
		   ).

%%--------------------------------------------------------------------
%% Function: pstnproxy_allowed_proxy_request(Request, PstnCtx)
%%           Request = request record()
%%           PstnCtx = pstn_ctx record()
%% Descrip.: Decide if pstnproxy should proxy a request, or reject it
%%           with a '403 Forbidden'. Return 'undefined' for default
%%           processing.
%% Returns : true | false | undefined
%%--------------------------------------------------------------------
pstnproxy_allowed_proxy_request(Request, PstnCtx) when is_record(Request, request),
						       is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_allowed_proxy_request, 2},
		    ?LOCAL_MODULE:pstnproxy_allowed_proxy_request(Request, PstnCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: pstnproxy_verify_from(Request, THandler, YXAPeerAuth,
%%                                 PstnCtx)
%%           Request     = request record()
%%           THandler    = term(), server transaction handle
%%           YxaPeerAuth = true | false
%%           PstnCtx     = pstn_ctx record()
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
pstnproxy_verify_from(Request, THandler, YXAPeerAuth, PstnCtx) when is_record(Request, request),
								    is_boolean(YXAPeerAuth),
								    is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_verify_from, 4},
		    ?LOCAL_MODULE:pstnproxy_verify_from(Request, THandler, YXAPeerAuth, PstnCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: pstnproxy_number_based_routing(Request, THandler, LogTag,
%%                                          PstnCtx)
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
pstnproxy_number_based_routing(Request, THandler, LogTag, PstnCtx) ->
    ?CHECK_EXPORTED({pstnproxy_number_based_routing, 4},
		    ?LOCAL_MODULE:pstnproxy_number_based_routing(Request, THandler, LogTag, PstnCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: pstnproxy_lookup_action(Request, PstnCtx)
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
pstnproxy_lookup_action(Request, PstnCtx) when is_record(Request, request), is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_lookup_action, 2},
		    ?LOCAL_MODULE:pstnproxy_lookup_action(Request, PstnCtx),
		    undefined
		   ).


% outgoingproxy hooks
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: outgoingproxy_challenge_before_relay(Origin, Request,
%%                                                Dst)
%% Descrip.: Check if 'outgoingproxy' should challenge a request that
%%           it has determined it should relay, or if it should proxy
%%           the request without authorization instead.
%% Returns : term()
%%--------------------------------------------------------------------
outgoingproxy_challenge_before_relay(Origin, Request, Dst) when is_record(Origin, siporigin),
								is_record(Request, request) ->
    ?CHECK_EXPORTED({outgoingproxy_challenge_before_relay, 3},
		    ?LOCAL_MODULE:outgoingproxy_challenge_before_relay(Origin, Request, Dst),
		    true
		   ).

% eventserver hooks
%%%%%%%%%%%%%%%%%%%%%%


%%--------------------------------------------------------------------
%% Function: get_event_package_module(EventPackage, Request, YxaCtx)
%%           EventPackage = string(), "presence" or "ua-config" etc.
%%           Request      = request record()
%%           YxaCtx       = yxa_ctx record()
%% Descrip.: Decide which event package should handle a request
%%           (SUBSCRIBE or PUBLISH) in the eventserver. You can use
%%           this to make only certain SUBSCRIBE/PUBLISH requests go
%%           to a custom event package. Remember to make
%%           get_all_event_packages return any additions too.
%% Returns : {ok, PackageModule} |
%%           undefined
%%           PackageModule = atom()
%%--------------------------------------------------------------------
get_event_package_module(EventPackage, Request, YxaCtx) when is_list(EventPackage), is_record(Request, request),
							     is_record(YxaCtx, yxa_ctx) ->
    ?CHECK_EXPORTED({get_event_package_module, 3},
		    ?LOCAL_MODULE:get_event_package_module(EventPackage, Request, YxaCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: get_all_event_packages()
%% Descrip.: Get list of all event packages. Duplicate Package is
%%           allowed (and has a purpose, if you want to have more than
%%           one possible Module for a Package (decided using
%%           get_event_package_module/3 above).
%% Returns : {ok, PackageDefs}
%%           PackageDefs = list() of {Package, Module}
%%           Package     = string()
%%           Module      = atom()
%%--------------------------------------------------------------------
get_all_event_packages() ->
    ?CHECK_EXPORTED({get_all_event_packages, 0},
		    ?LOCAL_MODULE:get_all_event_packages(),
		    yxa_config:get_env(eventserver_package_handlers)
		   ).


%%--------------------------------------------------------------------
%% Function: eventserver_locationdb_action(Type, User, Location)
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
eventserver_locationdb_action(Type, User, Location) when is_atom(Type), is_list(User) ->
    ?CHECK_EXPORTED({eventserver_locationdb_action, 3},
		    ?LOCAL_MODULE:eventserver_locationdb_action(Type, User, Location),
		    undefined
		   ).

% sippipe hooks
%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: start_sippipe(Request, YxaCtx, Dst, AppData)
%%           Request  = request record()
%%           YxaCtx   = yxa_ctx record()
%%           Dst      = sipurl record() | route | list() of sipdst record()
%%           AppData  = list() of term(), application specific data
%%                      passed to the start_sippipe/4 function in your
%%                      'local' module.
%% Descrip.: Start a sippipe for one of the YXA applications
%%           incomingproxy, outgoingproxy or pstnproxy. This is a very
%%           suitable place to for example add/delete headers.
%% @see      sippipe:start/5.
%% Returns : term(), result of sipppipe:start/5
%%--------------------------------------------------------------------
start_sippipe(Request, YxaCtx, Dst, AppData) when is_record(Request, request), is_record(YxaCtx, yxa_ctx),
						  is_list(AppData) ->
    ?CHECK_EXPORTED({start_sippipe, 4},
		    ?LOCAL_MODULE:start_sippipe(Request, YxaCtx, Dst, AppData),
		    begin
			THandler = YxaCtx#yxa_ctx.thandler,
			ClientTransaction = none,
			sippipe:start(THandler, ClientTransaction, Request, Dst, ?SIPPIPE_TIMEOUT)
		    end
		   ).

%%--------------------------------------------------------------------
%% Function: sippipe_received_response(Request, Response, DstList)
%%           Request  = request record()
%%           Response = response record() | {Status, Reason}
%%           DstList  = list() of sipdst record()
%% Descrip.: When sippipe receives a final response, this function is
%%           called. Depending on the return value of this function,
%%           sippipe will behave differently.
%%           'undefined' means sippipe will fall back to it's default
%            action.
%%           'huntstop' will make sippipe stop processing and instruct
%%           the server transaction to send a response (Status,
%%           Reason).
%%           'next' will tell sippipe to try the next destination in
%%           NewDstList (possibly altered version of DstList).
%% Returns : undefined                  |
%%           {huntstop, Status, Reason} |
%%           {next, NewDstList}
%%           Status = integer(), SIP status code
%%           Reason = string(), SIP reason phrase
%%           NewDstList = list() of sipdst record()
%%--------------------------------------------------------------------
sippipe_received_response(Request, Response, DstList) when is_record(Request, request),
							   is_record(Response, response) ->
    ?CHECK_EXPORTED({sippipe_received_response, 3},
		    ?LOCAL_MODULE:sippipe_received_response(Request, Response, DstList),
		    undefined
		   );
sippipe_received_response(Request, {Status, Reason}, DstList) when is_record(Request, request),
								   is_integer(Status), is_list(Reason) ->
    ?CHECK_EXPORTED({sippipe_received_response, 3},
		    ?LOCAL_MODULE:sippipe_received_response(Request, {Status, Reason}, DstList),
		    undefined
		   ).

% cpl_db hooks
%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: user_has_cpl_script(User)
%%           User = string()
%% Descrip.: determine if a cpl script has been loaded for the user
%%           User
%% @see      cpl_db:user_has_cpl_script/1.
%% Returns : true | false
%%--------------------------------------------------------------------
user_has_cpl_script(User) ->
    ?CHECK_EXPORTED({user_has_cpl_script, 1},
		    ?LOCAL_MODULE:user_has_cpl_script(User),
		    cpl_db:user_has_cpl_script(User)
		   ).

%%--------------------------------------------------------------------
%% Function: user_has_cpl_script(User, Direction)
%%           User      = string()
%%           Direction = incoming | outgoing
%% Descrip.: determine if a cpl script has been loaded for the user
%%           User
%% @see      cpl_db:user_has_cpl_script/1.
%% Returns : true | false
%%--------------------------------------------------------------------
user_has_cpl_script(User, Direction) ->
    ?CHECK_EXPORTED({user_has_cpl_script, 2},
		    ?LOCAL_MODULE:user_has_cpl_script(User, Direction),
		    cpl_db:user_has_cpl_script(User, Direction)
		   ).

%%--------------------------------------------------------------------
%% Function: get_cpl_for_user(User)
%%           User = string()
%% Descrip.: get the cpl script graph for a certain user
%% @see      cpl_db:get_cpl_for_user/1.
%% Returns : nomatch | {ok, CPLGraph}
%%           CPLGraph = term(), a cpl graph for use in
%%                      interpret_cpl:process_cpl_script(...)
%%--------------------------------------------------------------------
get_cpl_for_user(User) ->
    ?CHECK_EXPORTED({get_cpl_for_user, 1},
		    ?LOCAL_MODULE:get_cpl_for_user(User),
		    cpl_db:get_cpl_for_user(User)
		   ).


%%--------------------------------------------------------------------
%% See cpl/README
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Function: cpl_log(LogName, Comment, User, Request)
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
cpl_log(LogName, Comment, User, Request) ->
    ?CHECK_EXPORTED({cpl_log, 4},
		    ?LOCAL_MODULE:cpl_log(LogName, Comment, User, Request),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: cpl_is_log_dest(LogName)
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
cpl_is_log_dest(LogName) ->
    ?CHECK_EXPORTED({cpl_is_log_dest, 1},
		    ?LOCAL_MODULE:cpl_is_log_dest(LogName),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: cpl_mail(Mail, User)
%% Descrip.:
%% Returns : term()
%%--------------------------------------------------------------------
cpl_mail(Mail, User) ->
    ?CHECK_EXPORTED({cpl_mail, 2},
		    ?LOCAL_MODULE:cpl_mail(Mail, User),
		    undefined
		   ).


%% transaction layer hooks
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: start_client_transaction(Request, Dst, Branch, Timeout)
%%           Request  = request record()
%%           Dst      = sipdst record(), the destination for this
%%                                       client transaction
%%           Branch   = string()
%%           Timeout  = integer(), timeout for INVITE transactions
%% Descrip.: Start a client transaction, possibly after altering the
%%           request to be sent.
%% @see      transactionlayer:start_client_transaction/5.
%% Returns : Pid |
%%           {error, Reason}
%%           Pid    = pid(), started client transaction handler
%%           Reason = string()
%%--------------------------------------------------------------------
start_client_transaction(Request, Dst, Branch, Timeout) when is_record(Request, request), is_record(Dst, sipdst),
							     is_list(Branch), is_integer(Timeout) ->
    ?CHECK_EXPORTED({start_client_transaction, 4},
		    ?LOCAL_MODULE:start_client_transaction(Request, Dst, Branch, Timeout),
		    transactionlayer:start_client_transaction(Request, Dst, Branch, Timeout, self())
		   ).

%%--------------------------------------------------------------------
%% Function: new_request(AppModule, Request, YxaCtx)
%%           AppModule = atom(), YXA application module the
%%                       transaction layer thought this request should
%%                       be passed to
%%           Request   = request record()
%%           YxaCtx    = yxa_ctx record()
%% Descrip.: This function gets called when the transaction layer has
%%           decided that a new request has arrived, and figured it
%%           should be passed to the YXA application (proxy core/
%%           transaction user). Depending on what this function
%%           returns, the AppModule:request/2 function will either not
%%           be called at all, called with the parameters unchanged or
%%           called with a modified set of parameters.
%% Returns : undefined | (Continue processing with default arguments)
%%           ignore    | (Don't continue at all - your code assumes responsibility to handle the request)
%%           {modified, NewAppModule, NewRequest, NewOrigin, NewLogStr}
%% Note    : DON'T ALTER THE URI OF INVITE REQUESTS HERE! If you do,
%%           the ACKs of non-2xx responses will be disqualified by the
%%           server transaction since the URI of the ACK doesn't match
%%           the URI of the original INVITE (since you changed it).
%%--------------------------------------------------------------------
new_request(AppModule, Request, YxaCtx) ->
    ?CHECK_EXPORTED({new_request, 3},
		    ?LOCAL_MODULE:new_request(AppModule, Request, YxaCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: new_response(AppModule, Response, YxaCtx)
%%           AppModule = atom(), YXA application module the
%%                       transaction layer thought this request should
%%                       be passed to
%%           Response  = response record()
%%           YxaCtx    = yxa_ctx record()
%% Descrip.: This function gets called when the transaction layer has
%%           decided that a response not assoicated with a running
%%           client transaction has arrived. Such responses should be
%%           passed to the YXA application (proxy core/transaction
%%           user). Depending on what this function returns, the
%%           AppModule:response/2 function will either not be called
%%           at all, called with the parameters unchanged or called
%%           with a modified set of parameters.
%% Returns : undefined | (Continue processing with default arguments)
%%           ignore    | (Don't continue at all - your code assumes responsibility to handle the response)
%%           {modified, NewAppModule, NewResponse, NewOrigin, NewLogStr}
%%--------------------------------------------------------------------
new_response(AppModule, Response, YxaCtx) ->
    ?CHECK_EXPORTED({new_response, 3},
		    ?LOCAL_MODULE:new_response(AppModule, Response, YxaCtx),
		    undefined
		   ).

%% transport layer hooks
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: is_acceptable_socket(Socket, Dir, Proto, Host, Port,
%%                                Module, Subject)
%%           Socket  = term(), the socket
%%           Dir     = in | out, direction of connection
%%           Proto   = tcp | tcp6 | tls | tls6
%%           Host    = string(), IP address or hostname of remote end
%%           Port    = integer()
%%           Module  = atom(), SIP-socket module name (sipsocket_tcp)
%%           Subject = term() | undefined, SSL socket Subject
%%                     information (if SSL socket)
%% Descrip.: Verify a socket. Return 'true' for acceptable, 'false'
%%           for NOT acceptable and 'undefined' to do default checks.
%% Returns : true | false | undefined
%%--------------------------------------------------------------------
is_acceptable_socket(Socket, Dir, Proto, Host, Port, Module, Subject) ->
    ?CHECK_EXPORTED({is_acceptable_socket, 7},
		    ?LOCAL_MODULE:is_acceptable_socket(Socket, Dir, Proto, Host, Port, Module, Subject),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: is_tls_equivalent(Proto, Host, Port)
%%           Proto = atom(), tcp | tcp6 | udp | udp6
%% Descrip.: If a destination (proto:host:port) is not TLS it might
%%           still be protected by an equivalence to TLS (like IPsec).
%%           When we require a TLS-protected destination, this hook
%%           lets you indicate that a particular destination is to be
%%           considered secure at the transport layer.
%% Returns : true | false | undefined
%%--------------------------------------------------------------------
is_tls_equivalent(Proto, Host, Port) ->
    ?CHECK_EXPORTED({is_tls_equivalent, 3},
		    ?LOCAL_MODULE:is_tls_equivalent(Proto, Host, Port),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: get_valid_altnames(Names, Subject, AltNames)
%%           Names    = list() of string(), list of names for the
%%                      certificate that the upper layer is willing to
%%                      accept
%%           Subject  = term(), ssl:peercert() subject data
%%           AltNames = list() of string(), subjectAltName:s in cert
%% Descrip.: Hook that lets you manipulate what names are considered
%%           valid for a SSL certificate presented by a host. If, for
%%           example, the host p1.example.org returns a certificate
%%           with the subjectAltNames, Names might be ["example.org"]
%%           since a user tried to reach sip:user@example.org, and
%%           AltNames might be ["p1.example.org"]. In this case, you
%%           must add "example.org" to AltNames, to allow the
%%           certificate.
%% Returns : NewAltNames = list() of string()
%%--------------------------------------------------------------------
get_valid_altnames(Names, Subject, AltNames) ->
    ?CHECK_EXPORTED({get_valid_altnames, 3},
		    ?LOCAL_MODULE:get_valid_altnames(Names, Subject, AltNames),
		    AltNames
		   ).

%%--------------------------------------------------------------------
%% Function: lookup_sipsocket_blacklist(Dst)
%%           Dst = {Proto, Addr, Port}
%%             Proto = tcp | tcp6 | udp | udp6 | tls | tls6 | atom()
%%             Addr  = string(), typically IPv4/IPv6 address
%%             Port  = integer()
%% Descrip.: Check if a destination is blacklisted/whitelisted.
%%           Return 'undefined' for default processing.
%% @see      sipsocket_blacklist:lookup_sipsocket_blacklist/1.
%% Returns : {ok, Entry}       |
%%           {ok, blacklisted} |
%%           {ok, whitelisted} |
%%           undefined
%%           Entry = blacklist_entry record()
%%--------------------------------------------------------------------
lookup_sipsocket_blacklist(Dst) ->
    ?CHECK_EXPORTED({lookup_sipsocket_blacklist, 1},
		    ?LOCAL_MODULE:lookup_sipsocket_blacklist(Dst),
		    sipsocket_blacklist:lookup_sipsocket_blacklist(Dst)
		   ).

%% configuration hooks
%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: check_config_type(Key, Value, Src)
%%           Key   = atom()
%%           Value = term()
%%           Src   = atom(), config backend module that found this
%%                           configuration parameter
%% Descrip.: Check a local configuration parameter. Local parameters
%%           are local_*.
%% Returns : {ok, NewValue} |
%%           {error, Msg}
%%           NewValue = term()
%%           Msg      = string()
%%--------------------------------------------------------------------
check_config_type(Key, Value, Src) ->
    %% We have to do this with try/catch instead of ?CHECK_EXPORTED since
    %% this function is needed before 'local' has been initialized
    try	?LOCAL_MODULE:check_config_type(Key, Value, Src) of
	Res ->
	    Res
    catch
	error: undef ->
	    %% the local module did not export check_config_type/3
	    {ok, Value}
    end.

%%--------------------------------------------------------------------
%% Function: config_is_soft_reloadable(Key, Value)
%%           Key   = atom()
%%           Value = term()
%% Descrip.: Check if it is possible to change a local configuration
%%           parameter with a soft reconfiguration (true), or if a
%%           complete restart of the application is necessary (false).
%% Returns : true | false
%%--------------------------------------------------------------------
config_is_soft_reloadable(Key, Value) ->
    ?CHECK_EXPORTED({config_is_soft_reloadable, 2},
		    ?LOCAL_MODULE:config_is_soft_reloadable(Key, Value),
		    true
		   ).

%%--------------------------------------------------------------------
%% Function: config_change_action(Key, Value, Mode)
%%           Key   = atom()
%%           Value = term()
%%           Mode  = soft | hard
%% Descrip.: Perform any necessary actions when a configuration value
%%           changes, like perhaps notifying a gen_server or similar.
%% Returns : ok | {error, Reason}
%%           Reason = string()
%%--------------------------------------------------------------------
config_change_action(Key, Value, Mode) ->
    %% We have to do this with try/catch instead of ?CHECK_EXPORTED since
    %% this function is needed before 'local' has been initialized
    try	?LOCAL_MODULE:config_change_action(Key, Value, Mode) of
	Res ->
	    Res
    catch
	error: undef ->
	    %% the local module did not export config_change_action/3
	    ok
    end.

%% sipdialog hooks
%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: create_dialog_state_uas(Caller, Request, ToTag, Contact)
%%           Caller  = term(), who is calling us?
%%           Request = request record(), received request that causes
%%                                       us to create a dialog
%%           ToTag   = string(), the To-tag our server transaction for
%%                               this request has generated
%%           Contact = string(), our Contact header value
%% Descrip.: Create a dialog record out of a received request and some
%%           other parameters.
%% @see      sipdialog:create_dialog_state_uas/3.
%% Returns : {ok, Dialog}
%%           Dialog = dialog record()
%%--------------------------------------------------------------------
create_dialog_state_uas(Caller, Request, ToTag, Contact) ->
    ?CHECK_EXPORTED({create_dialog_state_uas, 4},
		    ?LOCAL_MODULE:create_dialog_state_uas(Caller, Request, ToTag, Contact),
		    sipdialog:create_dialog_state_uas(Request, ToTag, Contact)
		   ).
