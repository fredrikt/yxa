%%%-------------------------------------------------------------------
%%% File    : local.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Interface to local functions hooking into lots of
%%%           different parts of the various YXA applications.
%%%
%%% @since    03 Jan 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
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
%% @spec    () -> ok
%%
%% @doc     Look at the list of exported functions from the module
%%          specified as ?LOCAL_MODULE, and make a cache of which of
%%          _this_ modules functions are overridden in the
%%          ?LOCAL_MODULE module.
%% @hidden
%% @end
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
%% @spec    (URL) -> term()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
url2mnesia_userlist(URL) when is_record(URL, sipurl) ->
    ?CHECK_EXPORTED({url2mnesia_userlist, 1},
		    ?LOCAL_MODULE:url2mnesia_userlist(URL),
		    default_url2mnesia_userlist(URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL) -> term()
%%
%%            URL = #sipurl{}
%%
%% @doc     Return "user@host" from URL.
%% @private
%% @end
%%--------------------------------------------------------------------
default_url2mnesia_userlist(URL) when is_list(URL#sipurl.user) ->
    [URL#sipurl.user ++ "@" ++ URL#sipurl.host];
default_url2mnesia_userlist(_URL) ->
    [].

%%--------------------------------------------------------------------
%% @spec    (User) -> string()
%%
%%            User = string()
%%
%% @doc     Turn a SIP username into an address which can be reached
%%          from anywhere. Used for example from the Mnesia
%%          userdb-module. It should be possible to call Mnesia users
%%          based on their username, but the username might need sip:
%%          prepended to it, or a default domain name appended to it.
%% @end
%%--------------------------------------------------------------------
canonify_user(User) when is_list(User) ->
    ?CHECK_EXPORTED({canonify_user, 1},
		    ?LOCAL_MODULE:canonify_user(User),
		    default_canonify_user(User)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User) -> string()
%%
%%            User = string()
%%
%% @doc
%% @private
%% @end
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
%% @spec    (In) -> [string()]
%%
%%            In = [string()]
%%
%% @doc     Canonify a list of addresses. Turn anything numeric into
%%          it's E.164 canonical representation. Used from some
%%          userdb-modules which potentially get non-fully qualified
%%          phone numbers (like local extension numbers) back from
%%          the database.
%% @end
%%--------------------------------------------------------------------
canonify_addresses(In) when is_list(In) ->
    ?CHECK_EXPORTED({canonify_addresses, 1},
		    ?LOCAL_MODULE:canonify_addresses(In),
		    default_canonify_addresses(In)
		   ).

%%--------------------------------------------------------------------
%% @spec    (In) -> [string()]
%%
%%            In = [string()]
%%
%% @doc
%% @private
%% @end
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
%% @spec    (Request, Origin) ->
%%            {proxy, PDst}    |
%%            {relay, RDst}    |
%%            {error, S}       |
%%            {response, S, R} |
%%            {forward, Fwd}   |
%%            none
%%
%%            Request = #request{}
%%            Origin  = #request{}
%%
%%            PDst = #sipurl{} | [#sipdst{}] | route
%%            RDst = #sipurl{} | [#sipdst{}] | route
%%            S    = integer() "SIP status code"
%%            R    = string() "SIP reason phrase"
%%            Fwd  = #sipurl{} "MUST have 'user' and 'pass' set to 'none'"
%%
%% @doc     Determine where to route a request that arrived to the
%%          'incomingproxy' application, destined for a local domain
%%          when it has been determined that the request was not
%%          addressed to one of our users (see local:lookupuser/1).
%%          Return 'none' for default routing.
%% @end
%%--------------------------------------------------------------------
lookup_homedomain_request(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    ?CHECK_EXPORTED({lookup_homedomain_request, 2},
		    ?LOCAL_MODULE:lookup_homedomain_request(Request, Origin),
		    none
		   ).

%%--------------------------------------------------------------------
%% @spec    (Request, Origin) ->
%%            {proxy, PDst}	|
%%            {relay, RDst}	|
%%            {error, S}		|
%%            {response, S, R}	|
%%            {forward, Fwd}	|
%%            none
%%
%%            Request = #request{}
%%            Origin  = #request{}
%%
%%            PDst = #sipurl{} | [#sipdst{}] | route
%%            RDst = #sipurl{} | [#sipdst{}] | route
%%            S    = integer() "SIP status code"
%%            R    = string() "SIP reason phrase"
%%            Fwd  = #sipurl{} "MUST have 'user' and 'pass' set to 'none'"
%%
%% @doc     Determine where to route a request that arrived to the
%%          'incomingproxy' application, destined for a remote
%%          domain. Return 'none' to perform default routing.
%% @end
%%--------------------------------------------------------------------
lookup_remote_request(Request, Origin) when is_record(Request, request), is_record(Origin, siporigin) ->
    ?CHECK_EXPORTED({lookup_remote_request, 2},
		    ?LOCAL_MODULE:lookup_remote_request(Request, Origin),
		    none
		   ).

%%--------------------------------------------------------------------
%% @spec    (Request) -> true | false
%%
%%            Request = #request{}
%%
%% @doc     Determine if a request is meant for this proxy itself, as
%%          opposed to say a user of the system.
%% @see      lookup:is_request_to_this_proxy/1.
%% @end
%%--------------------------------------------------------------------
is_request_to_this_proxy(Request) when is_record(Request, request) ->
    ?CHECK_EXPORTED({is_request_to_this_proxy, 1},
		    ?LOCAL_MODULE:is_request_to_this_proxy(Request),
		    lookup:is_request_to_this_proxy(Request)
		   ).


% lookup.erl hooks
%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (User) -> term()
%%
%% @doc
%% @see      lookup:lookupregexproute/1
%% @end
%%--------------------------------------------------------------------
lookupregexproute(User) ->
    ?CHECK_EXPORTED({lookupregexproute, 1},
		    ?LOCAL_MODULE:lookupregexproute(User),
		    lookup:lookupregexproute(User)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL) ->
%%            {proxy, URL}               |
%%            {relay, URL}               |
%%            {forward, URL}             |
%%            {response, Status, Reason} |
%%            none    |
%%            nomatch
%%
%%            URL = #sipurl{}
%%
%% @doc     The main 'give me a set of locations for one of our users'
%%          function that incomingproxy uses, when it determines that
%%          a request is for one of it's homedomains.
%% @see      lookup:lookupuser/1.
%% @end
%%--------------------------------------------------------------------
lookupuser(URL) ->
    ?CHECK_EXPORTED({lookupuser, 1},
		    ?LOCAL_MODULE:lookupuser(URL),
		    lookup:lookupuser(URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL, GRUU) ->
%%            {proxy, URL}               |
%%            {relay, URL}               |
%%            {forward, URL}             |
%%            {response, Status, Reason} |
%%            none    |
%%            nomatch
%%
%%            URL  = #sipurl{} "Request-URI"
%%            GRUU = string()
%%
%% @doc     Look up a GRUU. Used by incomingproxy and outgouingproxy.
%% @see      lookup:lookupuser_gruu/2.
%% @end
%%--------------------------------------------------------------------
lookupuser_gruu(URL, GRUU) ->
    ?CHECK_EXPORTED({lookupuser_gruu, 2},
		    ?LOCAL_MODULE:lookupuser_gruu(URL, GRUU),
		    lookup:lookupuser_gruu(URL, GRUU)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Users, URL) ->
%%            Locations
%%
%%            Users = [string()] "SIP users to fetch locations of"
%%            URL   = #sipurl{} "the Request-URI"
%%
%%            Locations = [#siplocationdb_e{}]
%%
%% @doc     Return all locations for a list of users that is suitable
%%          given a Request-URI. By suitable, we mean that we filter
%%          out SIP locations if Request-URI was SIPS, unless this
%%          proxy is configured not to.
%% @see      lookup:lookupuser_locations/2.
%% @end
%%--------------------------------------------------------------------
lookupuser_locations(Users, URL) ->
    ?CHECK_EXPORTED({lookupuser_locations, 2},
		    ?LOCAL_MODULE:lookupuser_locations(Users, URL),
		    lookup:lookupuser_locations(Users, URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL, Locations) -> [#sipurl{}]
%%
%%            URL      = #sipurl{} "Request-URI of request"
%%            Location = [#sipurl{}]
%%
%% @doc     Apply local policy for what locations are good to use for
%%          a particular Request-URI.
%% @see      lookup:remove_unsuitable_locations/2.
%% @end
%%--------------------------------------------------------------------
remove_unsuitable_locations(URL, Locations) when is_record(URL, sipurl), is_list(Locations) ->
    ?CHECK_EXPORTED({remove_unsuitable_locations, 2},
		    ?LOCAL_MODULE:remove_unsuitable_locations(URL, Locations),
		    lookup:remove_unsuitable_locations(URL, Locations)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL) -> term()
%%
%% @doc
%% @see      lookup:lookup_url_to_locations/1.
%% @end
%%--------------------------------------------------------------------
lookup_url_to_locations(URL) ->
    ?CHECK_EXPORTED({lookup_url_to_locations, 1},
		    ?LOCAL_MODULE:lookup_url_to_locations(URL),
		    lookup:lookup_url_to_locations(URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Src, URL) -> term()
%%
%% @doc
%% @see      lookup:lookup_url_to_addresses/2.
%% @end
%%--------------------------------------------------------------------
lookup_url_to_addresses(Src, URL) ->
    ?CHECK_EXPORTED({lookup_url_to_addresses, 2},
		    ?LOCAL_MODULE:lookup_url_to_addresses(Src, URL),
		    lookup:lookup_url_to_addresses(Src, URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Addresses) -> term()
%%
%% @doc
%% @see      lookup:lookup_addresses_to_users/1
%% @end
%%--------------------------------------------------------------------
lookup_addresses_to_users(Addresses) ->
    ?CHECK_EXPORTED({lookup_addresses_to_users, 1},
		    ?LOCAL_MODULE:lookup_addresses_to_users(Addresses),
		    lookup:lookup_addresses_to_users(Addresses)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Address) -> term()
%%
%% @doc
%% @see      lookup:lookup_address_to_users/1
%% @end
%%--------------------------------------------------------------------
lookup_address_to_users(Address) ->
    ?CHECK_EXPORTED({lookup_address_to_users, 1},
		    ?LOCAL_MODULE:lookup_address_to_users(Address),
		    lookup:lookup_address_to_users(Address)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Key) -> term()
%%
%% @doc
%% @see      lookup:lookupappserver/1
%% @end
%%--------------------------------------------------------------------
lookupappserver(Key) ->
    ?CHECK_EXPORTED({lookupappserver, 1},
		    ?LOCAL_MODULE:lookupappserver(Key),
		    lookup:lookupappserver(Key)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Key, Locations) -> term()
%%
%% @doc
%% @see      siplocation:prioritize_locations/1.
%% @end
%%--------------------------------------------------------------------
prioritize_locations(Key, Locations) ->
    ?CHECK_EXPORTED({prioritize_locations, 2},
		    ?LOCAL_MODULE:prioritize_locations(Key, Locations),
		    siplocation:prioritize_locations(Locations)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL) -> term()
%%
%% @doc
%% @see      lookup:lookupdefault/1.
%% @end
%%--------------------------------------------------------------------
lookupdefault(URL) ->
    ?CHECK_EXPORTED({lookupdefault, 1},
		    ?LOCAL_MODULE:lookupdefault(URL),
		    lookup:lookupdefault(URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Number) -> term()
%%
%% @doc
%% @see      lookup:lookuppotn/1.
%% @end
%%--------------------------------------------------------------------
lookuppotn(Number) ->
    ?CHECK_EXPORTED({lookuppotn, 1},
		    ?LOCAL_MODULE:lookuppotn(Number),
		    lookup:lookuppotn(Number)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Number) -> term()
%%
%% @doc
%% @see      lookup:lookupnumber/1.
%% @end
%%--------------------------------------------------------------------
lookupnumber(Number) ->
    ?CHECK_EXPORTED({lookupnumber, 1},
		    ?LOCAL_MODULE:lookupnumber(Number),
		    lookup:lookupnumber(Number)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Number) -> term()
%%
%% @doc
%% @see      lookup:lookupenum/1.
%% @end
%%--------------------------------------------------------------------
lookupenum(Number) ->
    ?CHECK_EXPORTED({lookupenum, 1},
		    ?LOCAL_MODULE:lookupenum(Number),
		    lookup:lookupenum(Number)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Number) -> term()
%%
%% @doc
%% @see      lookup:lookuppstn/1.
%% @end
%%--------------------------------------------------------------------
lookuppstn(Number) ->
    ?CHECK_EXPORTED({lookuppstn, 1},
		    ?LOCAL_MODULE:lookuppstn(Number),
		    lookup:lookuppstn(Number)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL) -> term()
%%
%% @doc     lookup:isours/1.
%% @see      foo
%% @end
%%--------------------------------------------------------------------
isours(URL) ->
    ?CHECK_EXPORTED({isours, 1},
		    ?LOCAL_MODULE:isours(URL),
		    lookup:isours(URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Domain) -> true | false
%%
%%            Domain = string()
%%
%% @doc     Check if something is one of our 'homedomains' - a domain
%%          we are the final destination for.
%% @see      lookup:homedomain/1.
%% @end
%%--------------------------------------------------------------------
homedomain(Domain) ->
    ?CHECK_EXPORTED({homedomain, 1},
		    ?LOCAL_MODULE:homedomain(Domain),
		    lookup:homedomain(Domain)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User, Header, URI, DstHost) ->
%%            {ok, Number} |
%%            none
%%
%%            User    = string() "SIP authentication username"
%%            Header  = #keylist{}
%%            URI     = #sipurl{} "outgoing Request-URI"
%%            DstHost = term() "chosen destination for request"
%%
%%            Number = string()
%%
%% @doc     This function is used by the pstnproxy to provide a PSTN
%%          gateway with usefull caller-id information. PSTN networks
%%          typically gets upset if the "A-number" (calling party) is
%%          a SIP URL. Different gateways might want the number
%%          formatted differently, thus the DstHost parameter (a TSP
%%          gateway to PSTN might only handle E.164 numbers, while a
%%          PBX might be expecting only a 4-digit extension number).
%% @see      lookup:get_remote_party_number/4.
%% @end
%%--------------------------------------------------------------------
get_remote_party_number(User, Header, URI, DstHost) when is_list(User) ->
    ?CHECK_EXPORTED({get_remote_party_number, 4},
		    ?LOCAL_MODULE:get_remote_party_number(User, Header, URI, DstHost),
		    lookup:get_remote_party_number(User, Header, URI, DstHost)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Number, Header, DstHost) ->
%%            {ok, Number}
%%
%%            Number  = string() "the number to format"
%%            Header  = #keylist{}
%%            DstHost = term() "destination for request"
%%
%%            Number = string()
%%
%% @doc     Hook for the actual formatting once
%%          get_remote_party_number/2 has found a number to be
%%          formatted.
%% @see      lookup:format_number_for_remote_party_id/3.
%% @end
%%--------------------------------------------------------------------
format_number_for_remote_party_id(Number, Header, DstHost) when is_list(Number) ->
    ?CHECK_EXPORTED({format_number_for_remote_party_id, 3},
		    ?LOCAL_MODULE:format_number_for_remote_party_id(Number, Header, DstHost),
		    lookup:format_number_for_remote_party_id(Number, Header, DstHost)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Key, DstHost) ->
%%            {ok, DisplayName} |
%%            none
%%
%%            Key     = string() "number we should turn into a name"
%%            DstHost = term() "destination for request"
%%
%%            DisplayName = string()
%%
%% @doc     When pstnproxy receives a request from a PSTN gateway,
%%          this function is called to see if we can find a nice
%%          Display Name for the calling party. By default, we only
%%          do the actual lookup if we can rewrite Key into a E.164
%%          number.
%% @see      lookup:get_remote_party_name/2.
%% @end
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
%% @spec    (Key) -> term()
%%
%% @doc
%% @see      lookup:rewrite_potn_to_e164/1.
%% @end
%%--------------------------------------------------------------------
rewrite_potn_to_e164(Key) ->
    ?CHECK_EXPORTED({rewrite_potn_to_e164, 1},
		    ?LOCAL_MODULE:rewrite_potn_to_e164(Key),
		    lookup:rewrite_potn_to_e164(Key)
		   ).


% userdb hooks
%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Address) -> term()
%%
%% @doc     Looks up exactly one user with an Address. Used for
%%          example in REGISTER. If there are multiple users with an
%%          address, this function returns {error}.
%% @see      sipuserdb:get_user_with_address/1.
%% @end
%%--------------------------------------------------------------------
get_user_with_address(Address) ->
    ?CHECK_EXPORTED({get_user_with_address, 1},
		    ?LOCAL_MODULE:get_user_with_address(Address),
		    sipuserdb:get_user_with_address(Address)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Address) -> term()
%%
%% @doc     Looks up all users with a given address. Used to find out
%%          to which users we should send a request.
%% @see      sipuserdb:get_users_for_address_of_record/1.
%% @end
%%--------------------------------------------------------------------
get_users_for_address_of_record(Address) ->
    ?CHECK_EXPORTED({get_users_for_address_of_record, 1},
		    ?LOCAL_MODULE:get_users_for_address_of_record(Address),
		    sipuserdb:get_users_for_address_of_record(Address)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Addresses) -> term()
%%
%% @doc
%% @see      sipuserdb:get_users_for_addresses_of_record/1.
%% @end
%%--------------------------------------------------------------------
get_users_for_addresses_of_record(Addresses) ->
    ?CHECK_EXPORTED({get_users_for_addresses_of_record, 1},
		    ?LOCAL_MODULE:get_users_for_addresses_of_record(Addresses),
		    sipuserdb:get_users_for_addresses_of_record(Addresses)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User) -> term()
%%
%% @doc     Gets all addresses for a user. Used for example to check
%%          if a request from a user has an acceptable From: header.
%% @see      sipuserdb:get_addresses_for_user/1.
%% @end
%%--------------------------------------------------------------------
get_addresses_for_user(User) ->
    ?CHECK_EXPORTED({get_addresses_for_user, 1},
		    ?LOCAL_MODULE:get_addresses_for_user(User),
		    sipuserdb:get_addresses_for_user(User)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Users) -> term()
%%
%% @doc
%% @see      sipuserdb:get_addresses_for_users/1.
%% @end
%%--------------------------------------------------------------------
get_addresses_for_users(Users) ->
    ?CHECK_EXPORTED({get_addresses_for_users, 1},
		    ?LOCAL_MODULE:get_addresses_for_users(Users),
		    sipuserdb:get_addresses_for_users(Users)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL) -> term()
%%
%% @doc
%% @see      sipuserdb:get_users_for_url/1.
%% @end
%%--------------------------------------------------------------------
get_users_for_url(URL) ->
    ?CHECK_EXPORTED({get_users_for_url, 1},
		    ?LOCAL_MODULE:get_users_for_url(URL),
		    sipuserdb:get_users_for_url(URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User) -> term()
%%
%% @doc
%% @see      sipuserdb:get_password_for_user/1.
%% @end
%%--------------------------------------------------------------------
get_password_for_user(User) ->
    ?CHECK_EXPORTED({get_password_for_user, 1},
		    ?LOCAL_MODULE:get_password_for_user(User),
		    sipuserdb:get_password_for_user(User)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User) -> term()
%%
%% @doc
%% @see      sipuserdb:get_classes_for_user/1.
%% @end
%%--------------------------------------------------------------------
get_classes_for_user(User) ->
    ?CHECK_EXPORTED({get_classes_for_user, 1},
		    ?LOCAL_MODULE:get_classes_for_user(User),
		    sipuserdb:get_classes_for_user(User)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User) -> term()
%%
%% @doc
%% @see      sipuserdb:get_telephonenumber_for_user/1.
%% @end
%%--------------------------------------------------------------------
get_telephonenumber_for_user(User) ->
    ?CHECK_EXPORTED({get_telephonenumber_for_user, 1},
		    ?LOCAL_MODULE:get_telephonenumber_for_user(User),
		    sipuserdb:get_telephonenumber_for_user(User)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Users) -> term()
%%
%% @doc
%% @see      sipuserdb:get_forwards_for_users/1.
%% @end
%%--------------------------------------------------------------------
get_forwards_for_users(Users) ->
    ?CHECK_EXPORTED({get_forwards_for_user, 1},
		    ?LOCAL_MODULE:get_forwards_for_users(Users),
		    sipuserdb:get_forwards_for_users(Users)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Module, Function, Args) -> {ok, Res} | undefined
%%
%%            Module   = atom() "sipuserdb module"
%%            Function = atom() "function in Module"
%%            Args     = term() "arguments to function"
%%
%% @doc     Hook to override a specific sipuserdb backend function. If
%%          'undefined' is returned, the real backend function will
%%          be called
%% @end
%%--------------------------------------------------------------------
sipuserdb_backend_override(Module, Function, Args) ->
    ?CHECK_EXPORTED({sipuserdb_backend_override, 3},
		    ?LOCAL_MODULE:sipuserdb_backend_override(Module, Function, Args),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (CfgKey, Args) ->
%%            {ok, Res} | undefined
%%
%%            CfgKey = sipuserdb_mysql_get_sipuser                  |
%%                     sipuserdb_mysql_get_user_for_address         |
%%                     sipuserdb_mysql_get_addresses_for_user       |
%%                     sipuserdb_mysql_get_classes_for_user         |
%%                     sipuserdb_mysql_get_password_for_user        |
%%                     sipuserdb_mysql_get_telephonenumber_for_user
%%            Args   = term() "key(s) to use in SQL query"
%%
%%            Res = string() "SQL query"
%%
%% @doc     If you need to make SQL statements other than what is
%%          possible using the template-based configuration parameter
%%          possibilitys, do it here. Return 'undefined' to let
%%          sipuserdb_mysql do it's default query construction. Note
%%          : You have to mysql:quote() everything you use from Args!
%% @end
%%--------------------------------------------------------------------
sipuserdb_mysql_make_sql_statement(CfgKey, Args) ->
    ?CHECK_EXPORTED({sipuserdb_mysql_make_sql_statement, 2},
		    ?LOCAL_MODULE:sipuserdb_mysql_make_sql_statement(CfgKey, Args),
		    undefined
		   ).

% Location lookup hooks
%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Users) -> term()
%%
%% @doc     Looks up all contacts for a list of users. Used to find
%%          out where a set of users are to see where we should route
%%          a request.
%% @see      siplocation:get_locations_for_users/1.
%% @end
%%--------------------------------------------------------------------
get_locations_for_users(Users) ->
    ?CHECK_EXPORTED({get_locations_for_users, 1},
		    ?LOCAL_MODULE:get_locations_for_users(Users),
		    siplocation:get_locations_for_users(Users)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URI) -> term()
%%
%% @doc     Checks if any of our users are registered at the location
%%          specified. Used to determine if we should proxy requests
%%          to a URI without authorization.
%% @see      siplocation:get_user_with_contact/1.
%% @end
%%--------------------------------------------------------------------
get_user_with_contact(URI) ->
    ?CHECK_EXPORTED({get_user_with_contact, 1},
		    ?LOCAL_MODULE:get_user_with_contact(URI),
		    siplocation:get_user_with_contact(URI)
		   ).

%%--------------------------------------------------------------------
%% @spec    (URI) -> term()
%%
%% @doc     like get_user_with_contact but returns a list of
%%          siplocationdb_e records instead
%% @see      siplocation:get_locations_with_contact/1.
%% @end
%%--------------------------------------------------------------------
%% like get_user_with_contact but returns a list of siplocationdb_e records instead
get_locations_with_contact(URI) ->
    ?CHECK_EXPORTED({get_locations_with_contact, 1},
		    ?LOCAL_MODULE:get_locations_with_contact(URI),
		    siplocation:get_locations_with_contact(URI)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User, InstanceId, GRUU, To) ->
%%            URL | undefined
%%
%%            URL = #sipurl{}
%%
%% @doc     Make an URL out of a GRUU. Return 'undefined' for default
%%          algorithm.
%% @see      foo
%% @end
%%--------------------------------------------------------------------
gruu_make_url(User, InstanceId, GRUU, To) ->
    ?CHECK_EXPORTED({gruu_make_url, 4},
		    ?LOCAL_MODULE:gruu_make_url(User, InstanceId, GRUU, To),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (URL) ->
%%            {true, GRUU} | false
%%
%%            GRUU = string()
%%
%% @doc     Check if an URL possibly is a GRUU we've created.
%% @see      gruu:is_gruu_url/1.
%% @end
%%--------------------------------------------------------------------
is_gruu_url(URL) ->
    ?CHECK_EXPORTED({is_gruu_url, 1},
                    ?LOCAL_MODULE:is_gruu_url(URL),
                    gruu:is_gruu_url(URL)
                   ).


% AAA hooks
%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Header, Method) -> term()
%%
%% @doc
%% @see      sipauth:get_user_verified/2.
%% @end
%%--------------------------------------------------------------------
get_user_verified(Header, Method) ->
    ?CHECK_EXPORTED({get_user_verified, 2},
		    ?LOCAL_MODULE:get_user_verified(Header, Method),
		    sipauth:get_user_verified(Header, Method)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Header, Method) -> term()
%%
%% @doc
%% @see      sipauth:get_user_verified_proxy/2.
%% @end
%%--------------------------------------------------------------------
get_user_verified_proxy(Header, Method) ->
    ?CHECK_EXPORTED({get_user_verified_proxy, 2},
		    ?LOCAL_MODULE:get_user_verified_proxy(Header, Method),
		    sipauth:get_user_verified_proxy(Header, Method)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User, URL) -> true | false
%%
%%            User = string() "SIP authentication username"
%%            URL  = #sipurl{}
%%
%% @doc     Check if a user (authenticated elsewhere) may use an
%%          address. See sipauth module for more information.
%% @see      sipauth:can_use_address/2.
%% @end
%%--------------------------------------------------------------------
can_use_address(User, URL) when is_list(User), is_record(URL, sipurl) ->
    ?CHECK_EXPORTED({can_use_address, 2},
		    ?LOCAL_MODULE:can_use_address(User, URL),
		    sipauth:can_use_address(User, URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User, URL) ->
%%            {Verdict, Reason}
%%
%%            User = string() "SIP authentication username"
%%            URL  = #sipurl{}
%%
%%            Verdict = true | false
%%            Reason  = ok | eperm | nomatch | error
%%
%% @doc     Check if a user (authenticated elsewhere) may use an
%%          address. See sipauth module for more information.
%% @see      sipauth:can_use_address_detail/2.
%% @end
%%--------------------------------------------------------------------
can_use_address_detail(User, URL) when is_list(User), is_record(URL, sipurl) ->
    ?CHECK_EXPORTED({can_use_address_detail, 2},
		    ?LOCAL_MODULE:can_use_address_detail(User, URL),
		    sipauth:can_use_address_detail(User, URL)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Header, ToURL) ->
%%            {{Verdict, Reason}, User} |
%%            {stale, User}             |
%%            {false, none}
%%
%%            Header = #keylist{}
%%            ToURL  = #sipurl{}
%%
%%            Verdict = true | false
%%            Reason  = ok | eperm | nomatch | error
%%
%% @doc     Check if a REGISTER message authenticates OK etc. See
%%          sipauth module for more information.
%% @see      sipauth:can_register/2.
%% @end
%%--------------------------------------------------------------------
can_register(Header, ToURL) when is_record(Header, keylist), is_record(ToURL, sipurl) ->
    ?CHECK_EXPORTED({can_register, 2},
		    ?LOCAL_MODULE:can_register(Header, ToURL),
		    sipauth:can_register(Header, ToURL)
		   ).

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
is_allowed_pstn_dst(User, ToNumber, Header, Class) ->
    ?CHECK_EXPORTED({is_allowed_pstn_dst, 4},
		    ?LOCAL_MODULE:is_allowed_pstn_dst(User, ToNumber, Header, Class),
		    sipauth:is_allowed_pstn_dst(User, ToNumber, Header, Class)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Username, Header) ->
%%            NewUsername |
%%            undefined
%%
%%            Username = string()
%%            Header   = #keylist{}
%%
%%            NewUsername = string()
%%
%% @doc     Possibly make us use another username for this request.
%%          This is needed if your user database allows more than one
%%          username per user, or if you have clients that does not
%%          allow you to set authorization username explicitly and
%%          the username they assume you have is incorrect.
%% @end
%%--------------------------------------------------------------------
canonify_authusername(Username, Header) when is_list(Username), is_record(Header, keylist) ->
    ?CHECK_EXPORTED({canonify_authusername, 2},
		    ?LOCAL_MODULE:canonify_authusername(Username, Header),
		    undefined
		   ).

% incomingproxy hooks
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Origin, Request, Dst) -> term()
%%
%% @doc     Check if 'incomingproxy' should challenge a request that
%%          it has determined it should relay, or if it should proxy
%%          the request without authorization instead.
%% @end
%%--------------------------------------------------------------------
incomingproxy_challenge_before_relay(Origin, Request, Dst) when is_record(Origin, siporigin),
								is_record(Request, request) ->
    ?CHECK_EXPORTED({incomingproxy_challenge_before_relay, 3},
		    ?LOCAL_MODULE:incomingproxy_challenge_before_relay(Origin, Request, Dst),
		    true
		   ).

%%--------------------------------------------------------------------
%% @spec    (Request, Origin) -> term()
%%
%% @doc
%% @end
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
%% @spec    (DstNumber, Request, PstnCtx) ->
%%            undefined | nomatch | ignore | Relay
%%
%%            DstNumber = string() "typically user-part of Request-URI"
%%            Request   = #request{}
%%            Origin    = #siporigin{}
%%            THandler  = term() "server transaction handler"
%%
%%            Relay    = {relay, DstURI}
%%            Response = {response, Status, Reason, ExtraHeaders}
%%
%% @doc     When a request destined for PSTN is received by the
%%          pstnproxy, and no destination is found using
%%          local:lookuppstn(), this function is called. The return
%%          values have the following meaning :
%%          undefined - proceed with default behavior nomatch - there
%%          is no destination for DstNumber, reject request with a
%%          '404 Not Found' ignore - pstnproxy should do nothing
%%          further (this function must generate a final response)
%%          Response - send a response Relay - send Request to DstURI
%%
%% @end
%%--------------------------------------------------------------------
pstnproxy_route_pstn_not_e164(DstNumber, Request, PstnCtx) ->
    ?CHECK_EXPORTED({pstnproxy_route_pstn_not_e164, 3},
		    ?LOCAL_MODULE:pstnproxy_route_pstn_not_e164(DstNumber, Request, PstnCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (Request, Origin, THandler, PstnCtx) -> term()
%%
%%            Request  = #request{}
%%            Origin   = #siporigin{}
%%            THandler = term() "server transaction handle"
%%            PstnCtx  = #pstn_ctx{}
%%
%% @doc
%% @end
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
%% @spec    (Request, PstnCtx) ->
%%            {ok, AllowedMethods}
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            AllowedMethods = [string()]
%%
%% @doc     Return list of allowed SIP methods. Must be upper-cased.
%% @end
%%--------------------------------------------------------------------
pstnproxy_allowed_methods(Request, PstnCtx) when is_record(Request, request),
						 is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_allowed_methods, 2},
		    ?LOCAL_MODULE:pstnproxy_allowed_methods(Request, PstnCtx),
		    yxa_config:get_env(allowed_request_methods)
		   ).

%%--------------------------------------------------------------------
%% @spec    (Request, PstnCtx) -> true | false | undefined
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%% @doc     Decide if pstnproxy should proxy a request, or reject it
%%          with a '403 Forbidden'. Return 'undefined' for default
%%          processing.
%% @end
%%--------------------------------------------------------------------
pstnproxy_allowed_proxy_request(Request, PstnCtx) when is_record(Request, request),
						       is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_allowed_proxy_request, 2},
		    ?LOCAL_MODULE:pstnproxy_allowed_proxy_request(Request, PstnCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (Request, THandler, YXAPeerAuth, PstnCtx) -> term()
%%
%%            Request     = #request{}
%%            THandler    = term() "server transaction handle"
%%            YxaPeerAuth = true | false
%%            PstnCtx     = #pstn_ctx{}
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
pstnproxy_verify_from(Request, THandler, YXAPeerAuth, PstnCtx) when is_record(Request, request),
								    is_boolean(YXAPeerAuth),
								    is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_verify_from, 4},
		    ?LOCAL_MODULE:pstnproxy_verify_from(Request, THandler, YXAPeerAuth, PstnCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (Request, THandler, LogTag, PstnCtx) -> term()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
pstnproxy_number_based_routing(Request, THandler, LogTag, PstnCtx) ->
    ?CHECK_EXPORTED({pstnproxy_number_based_routing, 4},
		    ?LOCAL_MODULE:pstnproxy_number_based_routing(Request, THandler, LogTag, PstnCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (Request, PstnCtx) -> term()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
pstnproxy_lookup_action(Request, PstnCtx) when is_record(Request, request), is_record(PstnCtx, pstn_ctx) ->
    ?CHECK_EXPORTED({pstnproxy_lookup_action, 2},
		    ?LOCAL_MODULE:pstnproxy_lookup_action(Request, PstnCtx),
		    undefined
		   ).


% outgoingproxy hooks
%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Origin, Request, Dst) -> term()
%%
%% @doc     Check if 'outgoingproxy' should challenge a request that
%%          it has determined it should relay, or if it should proxy
%%          the request without authorization instead.
%% @end
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
%% @spec    (EventPackage, Request, YxaCtx) ->
%%            {ok, PackageModule} |
%%            undefined
%%
%%            EventPackage = string() "\"presence\" or \"ua-config\" etc."
%%            Request      = #request{}
%%            YxaCtx       = #yxa_ctx{}
%%
%%            PackageModule = atom()
%%
%% @doc     Decide which event package should handle a request
%%          (SUBSCRIBE or PUBLISH) in the eventserver. You can use
%%          this to make only certain SUBSCRIBE/PUBLISH requests go
%%          to a custom event package. Remember to make
%%          get_all_event_packages return any additions too.
%% @end
%%--------------------------------------------------------------------
get_event_package_module(EventPackage, Request, YxaCtx) when is_list(EventPackage), is_record(Request, request),
							     is_record(YxaCtx, yxa_ctx) ->
    ?CHECK_EXPORTED({get_event_package_module, 3},
		    ?LOCAL_MODULE:get_event_package_module(EventPackage, Request, YxaCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    () ->
%%            {ok, PackageDefs}
%%
%%            PackageDefs = [{Package, Module}]
%%            Package     = string()
%%            Module      = atom()
%%
%% @doc     Get list of all event packages. Duplicate Package is
%%          allowed (and has a purpose, if you want to have more than
%%          one possible Module for a Package (decided using
%%          get_event_package_module/3 above).
%% @end
%%--------------------------------------------------------------------
get_all_event_packages() ->
    ?CHECK_EXPORTED({get_all_event_packages, 0},
		    ?LOCAL_MODULE:get_all_event_packages(),
		    yxa_config:get_env(eventserver_package_handlers)
		   ).


%%--------------------------------------------------------------------
%% @spec    (Type, User, Location) -> term()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
eventserver_locationdb_action(Type, User, Location) when is_atom(Type), is_list(User) ->
    ?CHECK_EXPORTED({eventserver_locationdb_action, 3},
		    ?LOCAL_MODULE:eventserver_locationdb_action(Type, User, Location),
		    undefined
		   ).

% sippipe hooks
%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, Dst, AppData) ->
%%            term() "result of sipppipe:start/5"
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            Dst     = #sipurl{} | route | [#sipdst{}]
%%            AppData = [term()] "application specific data passed to the start_sippipe/4 function in your 'local' module."
%%
%% @doc     Start a sippipe for one of the YXA applications
%%          incomingproxy, outgoingproxy or pstnproxy. This is a very
%%          suitable place to for example add/delete headers.
%% @see      sippipe:start/5.
%% @end
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
%            action.
%% @spec    (Request, Response, DstList) ->
%%            undefined                  |
%%            {huntstop, Status, Reason} |
%%            {next, NewDstList}
%%
%%            Request  = #request{}
%%            Response = #response{} | {Status, Reason}
%%            DstList  = [#sipdst{}]
%%
%%            Status     = integer() "SIP status code"
%%            Reason     = string() "SIP reason phrase"
%%            NewDstList = [#sipdst{}]
%%
%% @doc     When sippipe receives a final response, this function is
%%          called. Depending on the return value of this function,
%%          sippipe will behave differently. 'undefined' means
%%          sippipe will fall back to it's default 'huntstop' will
%%          make sippipe stop processing and instruct the server
%%          transaction to send a response (Status, Reason). 'next'
%%          will tell sippipe to try the next destination in
%%          NewDstList (possibly altered version of DstList).
%% @end
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
%% @spec    (User) -> true | false
%%
%%            User = string()
%%
%% @doc     determine if a cpl script has been loaded for the user
%%          User
%% @see      cpl_db:user_has_cpl_script/1.
%% @end
%%--------------------------------------------------------------------
user_has_cpl_script(User) ->
    ?CHECK_EXPORTED({user_has_cpl_script, 1},
		    ?LOCAL_MODULE:user_has_cpl_script(User),
		    cpl_db:user_has_cpl_script(User)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User, Direction) -> true | false
%%
%%            User      = string()
%%            Direction = incoming | outgoing
%%
%% @doc     determine if a cpl script has been loaded for the user
%%          User
%% @see      cpl_db:user_has_cpl_script/1.
%% @end
%%--------------------------------------------------------------------
user_has_cpl_script(User, Direction) ->
    ?CHECK_EXPORTED({user_has_cpl_script, 2},
		    ?LOCAL_MODULE:user_has_cpl_script(User, Direction),
		    cpl_db:user_has_cpl_script(User, Direction)
		   ).

%%--------------------------------------------------------------------
%% @spec    (User) ->
%%            nomatch | {ok, CPLGraph}
%%
%%            User = string()
%%
%%            CPLGraph = term() "a cpl graph for use in interpret_cpl:process_cpl_script(...)"
%%
%% @doc     get the cpl script graph for a certain user
%% @see      cpl_db:get_cpl_for_user/1.
%% @end
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
%% @spec    (LogName, Comment, User, Request) -> term()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
cpl_log(LogName, Comment, User, Request) ->
    ?CHECK_EXPORTED({cpl_log, 4},
		    ?LOCAL_MODULE:cpl_log(LogName, Comment, User, Request),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (LogName) -> term()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
cpl_is_log_dest(LogName) ->
    ?CHECK_EXPORTED({cpl_is_log_dest, 1},
		    ?LOCAL_MODULE:cpl_is_log_dest(LogName),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (Mail, User) -> term()
%%
%% @doc
%% @end
%%--------------------------------------------------------------------
cpl_mail(Mail, User) ->
    ?CHECK_EXPORTED({cpl_mail, 2},
		    ?LOCAL_MODULE:cpl_mail(Mail, User),
		    undefined
		   ).


%% transaction layer hooks
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Request, Dst, Branch, Timeout) ->
%%            Pid |
%%            {error, Reason}
%%
%%            Request = #request{}
%%            Dst     = #sipdst{} "the destination for this client transaction"
%%            Branch  = string()
%%            Timeout = integer() "timeout for INVITE transactions"
%%
%%            Pid    = pid() "started client transaction handler"
%%            Reason = string()
%%
%% @doc     Start a client transaction, possibly after altering the
%%          request to be sent.
%% @see      transactionlayer:start_client_transaction/5.
%% @end
%%--------------------------------------------------------------------
start_client_transaction(Request, Dst, Branch, Timeout) when is_record(Request, request), is_record(Dst, sipdst),
							     is_list(Branch), is_integer(Timeout) ->
    ?CHECK_EXPORTED({start_client_transaction, 4},
		    ?LOCAL_MODULE:start_client_transaction(Request, Dst, Branch, Timeout),
		    transactionlayer:start_client_transaction(Request, Dst, Branch, Timeout, self())
		   ).

%%--------------------------------------------------------------------
%% @spec    (AppModule, Request, YxaCtx) ->
%%            undefined |
%%            ignore    |
%%            {modified, NewAppModule, NewRequest, NewOrigin, NewLogStr}
%%
%%            AppModule = atom() "YXA application module the transaction layer thought this request should be passed to"
%%            Request   = #request{}
%%            YxaCtx    = #yxa_ctx{}
%%
%% @doc     This function gets called when the transaction layer has
%%          decided that a new request has arrived, and figured it
%%          should be passed to the YXA application (proxy core/
%%          transaction user). Depending on what this function
%%          returns, the AppModule:request/2 function will either not
%%          be called at all, called with the parameters unchanged or
%%          called with a modified set of parameters. Note : DON'T
%%          ALTER THE URI OF INVITE REQUESTS HERE! If you do, the
%%          ACKs of non-2xx responses will be disqualified by the
%%          server transaction since the URI of the ACK doesn't match
%%          the URI of the original INVITE (since you changed it).
%% @end
%%--------------------------------------------------------------------
new_request(AppModule, Request, YxaCtx) ->
    ?CHECK_EXPORTED({new_request, 3},
		    ?LOCAL_MODULE:new_request(AppModule, Request, YxaCtx),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (AppModule, Response, YxaCtx) ->
%%            undefined |
%%            ignore    |
%%            {modified, NewAppModule, NewResponse, NewOrigin, NewLogStr}
%%
%%            AppModule = atom() "YXA application module the transaction layer thought this request should be passed to"
%%            Response  = #response{}
%%            YxaCtx    = #yxa_ctx{}
%%
%% @doc     This function gets called when the transaction layer has
%%          decided that a response not assoicated with a running
%%          client transaction has arrived. Such responses should be
%%          passed to the YXA application (proxy core/transaction
%%          user). Depending on what this function returns, the
%%          AppModule:response/2 function will either not be called
%%          at all, called with the parameters unchanged or called
%%          with a modified set of parameters.
%% @end
%%--------------------------------------------------------------------
new_response(AppModule, Response, YxaCtx) ->
    ?CHECK_EXPORTED({new_response, 3},
		    ?LOCAL_MODULE:new_response(AppModule, Response, YxaCtx),
		    undefined
		   ).

%% transport layer hooks
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Socket, Dir, Proto, Host, Port, Module, Subject) ->
%%            true | false | undefined
%%
%%            Socket  = term() "the socket"
%%            Dir     = in | out "direction of connection"
%%            Proto   = tcp | tcp6 | tls | tls6
%%            Host    = string() "IP address or hostname of remote end"
%%            Port    = integer()
%%            Module  = atom() "SIP-socket module name (sipsocket_tcp)"
%%            Subject = term() | undefined "SSL socket Subject information (if SSL socket)"
%%
%% @doc     Verify a socket. Return 'true' for acceptable, 'false' for
%%          NOT acceptable and 'undefined' to do default checks.
%% @end
%%--------------------------------------------------------------------
is_acceptable_socket(Socket, Dir, Proto, Host, Port, Module, Subject) ->
    ?CHECK_EXPORTED({is_acceptable_socket, 7},
		    ?LOCAL_MODULE:is_acceptable_socket(Socket, Dir, Proto, Host, Port, Module, Subject),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (Proto, Host, Port) -> true | false | undefined
%%
%%            Proto = tcp | tcp6 | udp | udp6
%%
%% @doc     If a destination (proto:host:port) is not TLS it might
%%          still be protected by an equivalence to TLS (like IPsec).
%%          When we require a TLS-protected destination, this hook
%%          lets you indicate that a particular destination is to be
%%          considered secure at the transport layer.
%% @end
%%--------------------------------------------------------------------
is_tls_equivalent(Proto, Host, Port) ->
    ?CHECK_EXPORTED({is_tls_equivalent, 3},
		    ?LOCAL_MODULE:is_tls_equivalent(Proto, Host, Port),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% @spec    (Names, Subject, AltNames) ->
%%            NewAltNames
%%
%%            Names    = [string()] "list of names for the certificate that the upper layer is willing to accept"
%%            Subject  = term() "ssl:peercert() subject data"
%%            AltNames = [string()] "subjectAltName:s in cert"
%%
%%            NewAltNames = [string()]
%%
%% @doc     Hook that lets you manipulate what names are considered
%%          valid for a SSL certificate presented by a host. If, for
%%          example, the host p1.example.org returns a certificate
%%          with the subjectAltNames, Names might be ["example.org"]
%%          since a user tried to reach sip:user@example.org, and
%%          AltNames might be ["p1.example.org"]. In this case, you
%%          must add "example.org" to AltNames, to allow the
%%          certificate.
%% @end
%%--------------------------------------------------------------------
get_valid_altnames(Names, Subject, AltNames) ->
    ?CHECK_EXPORTED({get_valid_altnames, 3},
		    ?LOCAL_MODULE:get_valid_altnames(Names, Subject, AltNames),
		    AltNames
		   ).

%%--------------------------------------------------------------------
%% @spec    (Dst) ->
%%            {ok, Entry}       |
%%            {ok, blacklisted} |
%%            {ok, whitelisted} |
%%            undefined
%%
%%            Dst   = {Proto, Addr, Port}
%%            Proto = tcp | tcp6 | udp | udp6 | tls | tls6 | atom()
%%            Addr  = string() "typically IPv4/IPv6 address"
%%            Port  = integer()
%%
%%            Entry = #blacklist_entry{}
%%
%% @doc     Check if a destination is blacklisted/whitelisted. Return
%%          'undefined' for default processing.
%% @see      sipsocket_blacklist:lookup_sipsocket_blacklist/1.
%% @end
%%--------------------------------------------------------------------
lookup_sipsocket_blacklist(Dst) ->
    ?CHECK_EXPORTED({lookup_sipsocket_blacklist, 1},
		    ?LOCAL_MODULE:lookup_sipsocket_blacklist(Dst),
		    sipsocket_blacklist:lookup_sipsocket_blacklist(Dst)
		   ).

%% configuration hooks
%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @spec    (Key, Value, Src) ->
%%            {ok, NewValue} |
%%            {error, Msg}
%%
%%            Key   = atom()
%%            Value = term()
%%            Src   = atom() "config backend module that found this configuration parameter"
%%
%%            NewValue = term()
%%            Msg      = string()
%%
%% @doc     Check a local configuration parameter. Local parameters
%%          are local_*.
%% @end
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
%% @spec    (Key, Value) -> true | false
%%
%%            Key   = atom()
%%            Value = term()
%%
%% @doc     Check if it is possible to change a local configuration
%%          parameter with a soft reconfiguration (true), or if a
%%          complete restart of the application is necessary (false).
%% @end
%%--------------------------------------------------------------------
config_is_soft_reloadable(Key, Value) ->
    ?CHECK_EXPORTED({config_is_soft_reloadable, 2},
		    ?LOCAL_MODULE:config_is_soft_reloadable(Key, Value),
		    true
		   ).

%%--------------------------------------------------------------------
%% @spec    (Key, Value, Mode) ->
%%            ok | {error, Reason}
%%
%%            Key   = atom()
%%            Value = term()
%%            Mode  = soft | hard
%%
%%            Reason = string()
%%
%% @doc     Perform any necessary actions when a configuration value
%%          changes, like perhaps notifying a gen_server or similar.
%% @end
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
%% @spec    (Caller, Request, ToTag, Contact) ->
%%            {ok, Dialog}
%%
%%            Caller  = term() "who is calling us?"
%%            Request = #request{} "received request that causes us to create a dialog"
%%            ToTag   = string() "the To-tag our server transaction for this request has generated"
%%            Contact = string() "our Contact header value"
%%
%%            Dialog = #dialog{}
%%
%% @doc     Create a dialog record out of a received request and some
%%          other parameters.
%% @see      sipdialog:create_dialog_state_uas/3.
%% @end
%%--------------------------------------------------------------------
create_dialog_state_uas(Caller, Request, ToTag, Contact) ->
    ?CHECK_EXPORTED({create_dialog_state_uas, 4},
		    ?LOCAL_MODULE:create_dialog_state_uas(Caller, Request, ToTag, Contact),
		    sipdialog:create_dialog_state_uas(Request, ToTag, Contact)
		   ).
