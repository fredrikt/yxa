%%%-------------------------------------------------------------------
%%% File    : local.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Interface to local functions hooking into lots of
%%%           different parts of the various Yxa applications.
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
	 get_user_with_contact/1
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
	 incomingproxy_challenge_before_relay/3
	]).

%% pstnproxy
-export([
	 pstnproxy_route_pstn_not_e164/4
	]).
%% outgoingproxy
-export([
	 outgoingproxy_challenge_before_relay/3
	]).

%% sippipe
-export([
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
	 new_request/4,
	 new_response/4
	]).

%% transport layer
-export([
	 is_acceptable_socket/7,
	 is_tls_equivalent/3
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
	 config_is_soft_reloadable/2
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").

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
    %% the Yxa 'configure' script.
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

url2mnesia_userlist(URL) when is_record(URL, sipurl) ->
    ?CHECK_EXPORTED({url2mnesia_userlist, 1},
		    ?LOCAL_MODULE:url2mnesia_userlist(URL),
		    default_url2mnesia_userlist(URL)
		   ).

default_url2mnesia_userlist(URL) when is_list(URL#sipurl.user) ->
    [URL#sipurl.user ++ "@" ++ URL#sipurl.host];
default_url2mnesia_userlist(_URL) ->
    [].

% Turn a SIP username into an address which can be reached from anywhere.
% Used for example from the Mnesia userdb-module. It should be possible
% to call Mnesia users based on their username, but the username might
% need sip: prepended to it, or a default domain name appended to it.
canonify_user(User) when is_list(User) ->
    ?CHECK_EXPORTED({canonify_user, 1},
		    ?LOCAL_MODULE:canonfiy_user(User),
		    default_canonify_user(User)
		   ).

default_canonify_user("sip:" ++ User) ->
    "sip:" ++ User;
default_canonify_user(Fulluser) ->
    case string:tokens(Fulluser, "@") of
        [_User, _Host] ->
            "sip:" ++ Fulluser;
        [User] ->
	    "sip:" ++ User ++ "@" ++ sipauth:realm()
    end.

%% Canonify a list of addresses. Turn anything numeric into it's E.164
%% canonical representation. Used from some userdb-modules which potentially
%% get non-fully qualified phone numbers (like local extension numbers)
%% back from the database.
canonify_addresses(In) when is_list(In) ->
    ?CHECK_EXPORTED({canonify_addresses, 1},
		    ?LOCAL_MODULE:canonfiy_addresses(In),
		    default_canonify_addresses(In)
		   ).

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
    case rewrite_potn_to_e164(H) of
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
%% Returns : {proxy, PDst}	| proxy unauthenticated
%%           {relay, RDst}	| relay requiring Proxy-Authentication
%%           {error, S}		| reject request with SIP status S
%%           {response, S, R}	| reject request with 'S R'
%%           {forward, Fwd}	| forward request to another server
%%           none		  perform default routing
%%           PDst = sipurl record() | list() of sipdst record() |
%%                  route
%%           RDst = sipurl record() | list() of sipdst record() |
%%                  route
%%           S    = integer(), SIP status code
%%           R    = string(), SIP reason phrase
%%           Fwd  = sipurl record() that MUST have 'user' and 'pass'
%%                  set to 'none'
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
%%           domain.
%% Returns : {proxy, PDst}	| proxy unauthenticated
%%           {relay, RDst}	| relay requiring Proxy-Authentication
%%           {error, S}		| reject request with SIP status S
%%           {response, S, R}	| reject request with 'S R'
%%           {forward, Fwd}	| forward request to another server
%%           none		  perform default routing
%%           PDst = sipurl record() | list() of sipdst record() |
%%                  route
%%           RDst = sipurl record() | list() of sipdst record() |
%%                  route
%%           S    = integer(), SIP status code
%%           R    = string(), SIP reason phrase
%%           Fwd  = sipurl record() that MUST have 'user' and 'pass'
%%                  set to 'none'
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
%% Returns : true | false
%%--------------------------------------------------------------------
is_request_to_this_proxy(Request) when is_record(Request, request) ->
    ?CHECK_EXPORTED({is_request_to_this_proxy, 1},
		    ?LOCAL_MODULE:is_request_to_this_proxy(Request),
		    lookup:is_request_to_this_proxy(Request)
		   ).


% lookup.erl hooks
%%%%%%%%%%%%%%%%%%%

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
%% Returns : {proxy, URL}               |
%%           {relay, URL}               |
%%           {forward, URL}             |
%%           {response, Status, Reason} |
%%           none    |   The user was found but has no locations registered
%%           nomatch     No such user
%%--------------------------------------------------------------------
lookupuser(URL) ->
    ?CHECK_EXPORTED({lookupuser, 1},
		    ?LOCAL_MODULE:lookupuser(URL),
		    lookup:lookupuser(URL)
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
%% Returns : list() of sipurl()
%%--------------------------------------------------------------------
remove_unsuitable_locations(URL, Locations) when is_record(URL, sipurl), is_list(Locations) ->
    ?CHECK_EXPORTED({remove_unsuitable_locations, 2},
		    ?LOCAL_MODULE:remove_unsuitable_locations(URL, Locations),
		    lookup:remove_unsuitable_locations(URL, Locations)
		   ).

lookup_url_to_locations(URL) ->
    ?CHECK_EXPORTED({lookup_url_to_locations, 1},
		    ?LOCAL_MODULE:lookup_url_to_locations(URL),
		    lookup:lookup_url_to_locations(URL)
		   ).

lookup_url_to_addresses(Src, URL) ->
    ?CHECK_EXPORTED({lookup_url_to_addresses, 2},
		    ?LOCAL_MODULE:lookup_url_to_addresses(Src, URL),
		    lookup:lookup_url_to_addresses(Src, URL)
		   ).

lookup_addresses_to_users(Addresses) ->
    ?CHECK_EXPORTED({lookup_addresses_to_users, 1},
		    ?LOCAL_MODULE:lookup_addresses_to_users(Addresses),
		    lookup:lookup_addresses_to_users(Addresses)
		   ).

lookup_address_to_users(Address) ->
    ?CHECK_EXPORTED({lookup_address_to_users, 1},
		    ?LOCAL_MODULE:lookup_address_to_users(Address),
		    lookup:lookup_address_to_users(Address)
		   ).

lookupappserver(Key) ->
    ?CHECK_EXPORTED({lookupappserver, 1},
		    ?LOCAL_MODULE:lookupappserver(Key),
		    lookup:lookupappserver(Key)
		   ).

prioritize_locations(Key, Locations) ->
    ?CHECK_EXPORTED({prioritize_locations, 2},
		    ?LOCAL_MODULE:prioritize_locations(Key, Locations),
		    siplocation:prioritize_locations(Locations)
		   ).

lookupdefault(URL) ->
    ?CHECK_EXPORTED({lookupdefault, 1},
		    ?LOCAL_MODULE:lookupdefault(URL),
		    lookup:lookupdefault(URL)
		   ).

lookuppotn(Number) ->
    ?CHECK_EXPORTED({lookuppotn, 1},
		    ?LOCAL_MODULE:lookuppotn(Number),
		    lookup:lookuppotn(Number)
		   ).

lookupnumber(Number) ->
    ?CHECK_EXPORTED({lookupnumber, 1},
		    ?LOCAL_MODULE:lookupnumber(Number),
		    lookup:lookupnumber(Number)
		   ).

lookupenum(Number) ->
    ?CHECK_EXPORTED({lookupenum, 1},
		    ?LOCAL_MODULE:lookupenum(Number),
		    lookup:lookupenum(Number)
		   ).

lookuppstn(Number) ->
    ?CHECK_EXPORTED({lookuppstn, 1},
		    ?LOCAL_MODULE:lookuppstn(Number),
		    lookup:lookuppstn(Number)
		   ).

isours(URL) ->
    ?CHECK_EXPORTED({isours, 1},
		    ?LOCAL_MODULE:isours(URL),
		    lookup:isours(URL)
		   ).

homedomain(Domain) ->
    ?CHECK_EXPORTED({homedomain, 1},
		    ?LOCAL_MODULE:homedomain(Domain),
		    lookup:homedomain(Domain)
		   ).

%%--------------------------------------------------------------------
%% Function: get_remote_party_number(User, Header, DstHost)
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

rewrite_potn_to_e164(Key) ->
    ?CHECK_EXPORTED({rewrite_potn_to_e164, 1},
		    ?LOCAL_MODULE:rewrite_potn_to_e164(Key),
		    lookup:rewrite_potn_to_e164(Key)
		   ).


% userdb hooks
%%%%%%%%%%%%%%%

% Looks up exactly one user with an Address. Used
% for example in REGISTER. If there are multiple
% users with an address, this function returns {error}.
get_user_with_address(Address) ->
    ?CHECK_EXPORTED({get_user_with_address, 1},
		    ?LOCAL_MODULE:get_user_with_address(Address),
		    sipuserdb:get_user_with_address(Address)
		   ).

% Looks up all users with a given address. Used
% to find out to which users we should send a request.
get_users_for_address_of_record(Address) ->
    ?CHECK_EXPORTED({get_users_for_address_of_record, 1},
		    ?LOCAL_MODULE:get_users_for_address_of_record(Address),
		    sipuserdb:get_users_for_address_of_record(Address)
		   ).

get_users_for_addresses_of_record(Addresses) ->
    ?CHECK_EXPORTED({get_users_for_addresses_of_record, 1},
		    ?LOCAL_MODULE:get_users_for_addresses_of_record(Addresses),
		    sipuserdb:get_users_for_addresses_of_record(Addresses)
		   ).

% Gets all addresses for a user. Used for example
% to check if a request from a user has an acceptable
% From: header.
get_addresses_for_user(User) ->
    ?CHECK_EXPORTED({get_addresses_for_user, 1},
		    ?LOCAL_MODULE:get_addresses_for_user(User),
		    sipuserdb:get_addresses_for_user(User)
		   ).

get_addresses_for_users(Users) ->
    ?CHECK_EXPORTED({get_addresses_for_users, 1},
		    ?LOCAL_MODULE:get_addresses_for_users(Users),
		    sipuserdb:get_addresses_for_users(Users)
		   ).

get_users_for_url(URL) ->
    ?CHECK_EXPORTED({get_users_for_url, 1},
		    ?LOCAL_MODULE:get_users_for_url(URL),
		    sipuserdb:get_users_for_url(URL)
		   ).

get_password_for_user(User) ->
    ?CHECK_EXPORTED({get_password_for_user, 1},
		    ?LOCAL_MODULE:get_password_for_user(User),
		    sipuserdb:get_password_for_user(User)
		   ).

get_classes_for_user(User) ->
    ?CHECK_EXPORTED({get_classes_for_user, 1},
		    ?LOCAL_MODULE:get_classes_for_user(User),
		    sipuserdb:get_classes_for_user(User)
		   ).

get_telephonenumber_for_user(User) ->
    ?CHECK_EXPORTED({get_telephonenumber_for_user, 1},
		    ?LOCAL_MODULE:get_telephonenumber_for_user(User),
		    sipuserdb:get_telephonenumber_for_user(User)
		   ).

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

% Looks up all contacts for a list of users. Used
% to find out where a set of users are to see where
% we should route a request.
get_locations_for_users(Users) ->
    ?CHECK_EXPORTED({get_locations_for_users, 1},
		    ?LOCAL_MODULE:get_locations_for_users(Users),
		    siplocation:get_locations_for_users(Users)
		   ).

% Checks if any of our users are registered at the
% location specified. Used to determine if we should
% proxy requests to a URI without authorization.
get_user_with_contact(URI) ->
    ?CHECK_EXPORTED({get_user_with_contact, 1},
		    ?LOCAL_MODULE:get_user_with_contact(URI),
		    siplocation:get_user_with_contact(URI)
		   ).


% AAA hooks
%%%%%%%%%%%%

get_user_verified(Header, Method) ->
    ?CHECK_EXPORTED({get_user_verified, 2},
		    ?LOCAL_MODULE:get_user_verified(Header, Method),
		    sipauth:get_user_verified(Header, Method)
		   ).

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
%% Returns : true  |
%%           false
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

incomingproxy_challenge_before_relay(Origin, Request, Dst) when is_record(Origin, siporigin),
								is_record(Request, request) ->
    ?CHECK_EXPORTED({incomingproxy_challenge_before_relay, 3},
		    ?LOCAL_MODULE:incomingproxy_challenge_before_relay(Origin, Request, Dst),
		    true
		   ).

%% pstnproxy hooks
%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: pstnproxy_route_pstn_not_e164(DstNumber, Request, Origin,
%%                                         THandler)
%%           DstNumber = string(), typically user-part of Request-URI
%%           Request   = request record()
%%           Origin    = siporigin record()
%%           THandler  = term(), server transaction handler
%% Descrip.: When a request destined for PSTN is received by the
%%           pstnproxy, and no destination is found using
%%           local:lookuppstn(), this function is called. The return
%%           values have the following meaning :
%%
%%             undefined - proceed with default behavior (currently
%%                         to try and use a destination indicated by
%%                         the configuration parameter
%%                         default_pstngateway)
%%             nomatch   - there is no destination for DstNumber,
%%                         reject request with a '404 Not Found'
%%             ignore    - pstnproxy should do nothing further (this
%%                         function should have told THandler to send
%%                         a final response)
%%             Relay     - send NewRequest to DstURI
%%
%% Returns : undefined | nomatch | ignore | Relay
%%           Relay = {relay, DstURI, NewRequest}
%%--------------------------------------------------------------------
pstnproxy_route_pstn_not_e164(DstNumber, Request, Origin, THandler) ->
    ?CHECK_EXPORTED({pstnproxy_route_pstn_not_e164, 4},
		    ?LOCAL_MODULE:pstnproxy_route_pstn_not_e164(DstNumber, Request, Origin, THandler),
		    undefined
		   ).

% outgoingproxy hooks
%%%%%%%%%%%%%%%%%%%%%%

outgoingproxy_challenge_before_relay(Origin, Request, Dst) when is_record(Origin, siporigin),
								 is_record(Request, request) ->
    ?CHECK_EXPORTED({outgoingproxy_challenge_before_relay, 3},
		    ?LOCAL_MODULE:outgoingproxy_challenge_before_relay(Origin, Request, Dst),
		    true
		   ).

% sippipe hooks
%%%%%%%%%%%%%%%%

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
%% Returns : true | false
%%--------------------------------------------------------------------
user_has_cpl_script(User) ->
    ?CHECK_EXPORTED({user_has_cpl_script, 1},
		    ?LOCAL_MODULE:user_has_cpl_script(User),
		    cpl_db:user_has_cpl_script(User)
		   ).

%%--------------------------------------------------------------------
%% Function: user_has_cpl_script(User)
%%           User      = string()
%%           Direction = incoming | outgoing
%% Descrip.: determine if a cpl script has been loaded for the user
%%           User
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
%% Returns : nomatch | {ok, CPLGraph}
%%           CPLGraph = a cpl graph for use in
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
cpl_log(LogName, Comment, User, Request) ->
    ?CHECK_EXPORTED({cpl_log, 4},
		    ?LOCAL_MODULE:cpl_log(LogName, Comment, User, Request),
		    undefined
		   ).

cpl_is_log_dest(LogName) ->
    ?CHECK_EXPORTED({cpl_is_log_dest, 1},
		    ?LOCAL_MODULE:cpl_is_log_dest(LogName),
		    undefined
		   ).

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
%% Returns : Pid |
%%           {error, Reason}
%%           Pid    = pid() of started client transaction handler
%%           Reason = string()
%%--------------------------------------------------------------------
start_client_transaction(Request, Dst, Branch, Timeout) when is_record(Request, request), is_record(Dst, sipdst),
							     is_list(Branch), is_integer(Timeout) ->
    ?CHECK_EXPORTED({start_client_transaction, 4},
		    ?LOCAL_MODULE:start_client_transaction(Request, Dst, Branch, Timeout),
		    transactionlayer:start_client_transaction(Request, Dst, Branch, Timeout, self())
		   ).

%%--------------------------------------------------------------------
%% Function: new_request(AppModule, Request, Origin, LogStr)
%%           AppModule = atom(), Yxa application module the
%%                       transaction layer thought this request should
%%                       be passed to
%%           Request   = request record()
%%           Origin    = siporigin record()
%%           LogStr    = string(), textual description of request
%% Descrip.: This function gets called when the transaction layer has
%%           decided that a new request has arrived, and figured it
%%           should be passed to the Yxa application (proxy core/
%%           transaction user). Depending on what this function
%%           returns, the AppModule:request/3 function will either not
%%           be called at all, called with the parameters unchanged or
%%           called with a modified set of parameters.
%% Returns : undefined | Continue processing with default arguments
%%           ignore    | Don't continue at all (your code assumes
%%                       responsibility to handle the request)
%%           {modified, NewAppModule, NewRequest,
%%                      NewOrigin, NewLogStr}
%% Note    : DON'T ALTER THE URI OF INVITE REQUESTS HERE! If you do,
%%           the ACKs of non-2xx responses will be disqualified by the
%%           server transaction since the URI of the ACK doesn't match
%%           the URI of the original INVITE (since you changed it).
%%--------------------------------------------------------------------
new_request(AppModule, Request, Origin, LogStr) ->
    ?CHECK_EXPORTED({new_request, 4},
		    ?LOCAL_MODULE:new_request(AppModule, Request, Origin, LogStr),
		    undefined
		   ).

%%--------------------------------------------------------------------
%% Function: new_response(AppModule, Response, Origin, LogStr)
%%           AppModule = atom(), Yxa application module the
%%                       transaction layer thought this request should
%%                       be passed to
%%           Response  = response record()
%%           Origin    = siporigin record()
%%           LogStr    = string(), textual description of response
%% Descrip.: This function gets called when the transaction layer has
%%           decided that a response not assoicated with a running
%%           client transaction has arrived. Such responses should be
%%           passed to the Yxa application (proxy core/transaction
%%           user). Depending on what this function returns, the
%%           AppModule:response/3 function will either not be called
%%           at all, called with the parameters unchanged or called
%%           with a modified set of parameters.
%% Returns : undefined | Continue processing with default arguments
%%           ignore    | Don't continue at all (your code assumes
%%                       responsibility to handle the response)
%%           {modified, NewAppModule, NewResponse,
%%                      NewOrigin, NewLogStr}
%%--------------------------------------------------------------------
new_response(AppModule, Response, Origin, LogStr) ->
    ?CHECK_EXPORTED({new_response, 4},
		    ?LOCAL_MODULE:new_response(AppModule, Response, Origin, LogStr),
		    undefined
		   ).

%% transport layer hooks
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: is_acceptable_socket(Socket, Dir, Proto, Host, Port,
%%                                Module, Subject)
%%           Socket  = term(), the socket
%%           Dir     = atom(), in | out - direction of connection
%%           Proto   = atom(), tcp | tcp6 | tls | tls6
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

%% configuration hooks
%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Function: check_config_type(Key, Value, Src)
%%           Key     = atom()
%%           Value   = term()
%%           Src     = atom(), config backend module that found this
%%                             configuration parameter
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
%%           Key     = atom()
%%           Value   = term()
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
