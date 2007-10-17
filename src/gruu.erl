%%%-------------------------------------------------------------------
%%% File    : gruu.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Code for Globally Routable User Agent (UA) URIs (GRUU),
%%%           based on draft-ietf-sip-gruu-06 and then updated with
%%%           regards to the changes in draft-ietf-sip-gruu-07.
%%% @since     3 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(gruu).

-export([
	 generate_gruu/2,
	 create_if_not_exists/2,

	 get_contact_for_gruu/1,

	 make_url/4,
	 is_gruu_url/1,
	 prepare_contact/2,

	 extract/2
	]).

%% debug functions
-export([show_all/0]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("database_gruu.hrl").

-define(GRUU_LENGTH, 10).

%%--------------------------------------------------------------------
%% @spec    (SipUser, InstanceId) ->
%%            GRUU
%%
%%            SipUser    = string() "username"
%%            InstanceId = string() "Instance ID the users device uses."
%%
%%            GRUU = string()
%%
%% @doc     Generate a new GRUU.
%% @end
%%--------------------------------------------------------------------
generate_gruu(SipUser, InstanceId) when is_list(SipUser), is_list(InstanceId) ->
    {MegaSec, Sec, MicroSec} = now(),
    Token = siprequest:make_base64_md5_token([hex:to_hex_string(MicroSec),
					      hex:to_hex_string(Sec),
					      hex:to_hex_string(MegaSec),
					      SipUser,
					      InstanceId
					     ]),
    %% Are URL parameters (opaque) case sensitive if quoted? If so, we should implement
    %% that and not lowercase all GRUUs
    http_util:to_lower( string:substr(Token, 1, ?GRUU_LENGTH) ).

%%--------------------------------------------------------------------
%% @spec    (SipUser, InstanceId) ->
%%            {ok, GRUU}
%%
%%            SipUser    = string() "username"
%%            InstanceId = string() "Instance ID the users device uses."
%%
%%            GRUU = string()
%%
%% @doc     Update last registration time on an existing User+Instance
%%          combination, or create a new GRUU for the User+Instance
%%          combination if it hasn't been registered before.
%% @end
%%--------------------------------------------------------------------
create_if_not_exists(SipUser, InstanceId) ->
    case database_gruu:fetch_using_user_instance(SipUser, InstanceId) of
	nomatch ->
	    GRUU = generate_unique_gruu(SipUser, InstanceId),
	    {atomic, ok} = database_gruu:insert(GRUU, SipUser, InstanceId, []),
	    logger:log(debug, "Registrar: Stored new GRUU ~p for SIP user ~p, instance id ~p",
		       [GRUU, SipUser, InstanceId]),
	    {ok, GRUU};
	{ok, [#gruu_dbe{gruu = GRUU}]} ->
	    %% Update gruu last_registered, so that we have something to go on if someone ever
	    %% wants to clean away old GRUUs
	    database_gruu:update_last_registered(GRUU),
	    logger:log(debug, "Registrar: SIP user ~p instance ~p already has a GRUU : ~p (updated)",
		       [SipUser, InstanceId, GRUU]),
	    {ok, GRUU}
    end.

%%--------------------------------------------------------------------
%% @spec    (SipUser, InstanceId) ->
%%            UniqueGRUU
%%
%%            SipUser    = string() "username"
%%            InstanceId = string() "Instance ID the users device uses."
%%
%%            UniqueGRUU = string()
%%
%% @doc     Generate a new GRUU, and make sure it is unique. If it
%%          wasn't, we generate a new GRUU and check again and so on.
%% @end
%%--------------------------------------------------------------------
generate_unique_gruu(SipUser, InstanceId) ->
    GRUU = generate_gruu(SipUser, InstanceId),
    case database_gruu:fetch_using_gruu(GRUU) of
	nomatch ->
	    %% ok, was unique
	    GRUU;
	_ ->
	    %% already existed, as unlikely as that is
	    generate_unique_gruu(SipUser, InstanceId)
    end.

%%--------------------------------------------------------------------
%% @spec    (GRUUstr) ->
%%            nomatch          |
%%            {ok, User, none} |
%%            {ok, User, SingleContact}
%%
%%            GRUUstr = string()
%%
%%            SingleContact = #siplocationdb_e{}
%%
%% @doc     Fetch the contact for a GRUU from the location database
%% @end
%%--------------------------------------------------------------------
get_contact_for_gruu(GRUUstr) when is_list(GRUUstr) ->
    case database_gruu:fetch_using_gruu(GRUUstr) of
	{ok, GRUU} when is_record(GRUU, gruu_dbe) ->
	    AllContacts = siplocation:get_locations_for_users([GRUU#gruu_dbe.sipuser]),

	    InstanceId = GRUU#gruu_dbe.instance_id,

	    %% Get location database entrys whose instance id matches our GRUU
	    Contacts = [E || E <- AllContacts, E#siplocationdb_e.instance == InstanceId],

	    %% GRUU draft 06 #8.4.1 (Request Targeting)
	    %% the proxy MUST populate the target set so
	    %% that there is never more than one contact at a time with a given
	    %% instance ID.  It is RECOMMENDED that the most recently registered
	    %% contact be used.
	    case Contacts of
		[] ->
		    {ok, GRUU#gruu_dbe.sipuser, none};
		_ ->
		    Sorted = lists:sort(fun newest_contact_first/2, Contacts),
		    [Newest | _] = Sorted,

		    {ok, GRUU#gruu_dbe.sipuser, Newest}
	    end;
	nomatch ->
	    nomatch
    end.

%% Sort on registration_time or expire or argh
newest_contact_first(#siplocationdb_e{flags = AFlags} = A,
		     #siplocationdb_e{flags = BFlags} = B) ->
    ATime =
	case lists:keysearch(registration_time, 1, AFlags) of
	    {value, {registration_time, ATime1}} when is_integer(ATime1) ->
		ATime1;
	    _ ->
		false
	end,

    BTime =
	case lists:keysearch(registration_time, 1, BFlags) of
	    {value, {registration_time, BTime1}} when is_integer(BTime1) ->
		BTime1;
	    _ ->
		false
	end,

    if
	ATime == BTime ->
	    %% sort on expire of ATime equals BTime
	    AExpire = A#siplocationdb_e.expire,
	    BExpire = B#siplocationdb_e.expire,
	    if
		AExpire == BExpire ->
		    %% argh, last resort
		    (A > B);
		AExpire > BExpire ->
		    %% later expire is considered more valuable
		    true;
		true ->
		    false
	    end;
	ATime >= BTime ->
	    %% later registration wins
	    true;
	true ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (SipUser, InstanceId, GRUU, ToHeader) ->
%%            URL
%%
%%            SipUser    = string() "username"
%%            InstanceId = string()
%%            GRUU       = string() | #gruu{}
%%            ToHeader   = string()
%%
%%            URL = #sipurl{}
%%
%% @doc     Make a SIP/SIPS URL out of a GRUU.
%% @end
%%--------------------------------------------------------------------
make_url(SipUser, InstanceId, GRUU, ToHeader) when is_record(GRUU, gruu_dbe) ->
    make_url(SipUser, InstanceId, GRUU#gruu_dbe.gruu, ToHeader);

make_url(SipUser, InstanceId, GRUU, ToHeader) when is_list(SipUser), is_list(InstanceId), is_list(GRUU),
						   is_list(ToHeader) ->
    {_DisplayName, To} = sipheader:to(ToHeader),
    case local:gruu_make_url(SipUser, InstanceId, GRUU, To) of
	{ok, URL} ->
	    URL;
	undefined ->
	    %% GRUU draft 07 #6 (Creation of a GRUU) "Unless the GRUU is meant to also possess the anonymity
	    %% property, it is RECOMMENDED that GRUUs be constructed using this parameter (opaque)"
	    case yxa_config:get_env(experimental_gruu_use_domain) of
		{ok, Domain} ->
		    %% An indicator prefix is needed to make sure we don't get username/gruu clashes
		    %% (even if that would not likely happen)
		    {ok, GRUUindicator} = yxa_config:get_env(experimental_gruu_gruu_indicator),
		    UserPart = GRUUindicator ++ GRUU,
		    %% It is important to make a SIPS URI if the To: header contained a SIPS URI.
		    %%
		    %% GRUU draft 06 #7.1.1.2 (Processing the REGISTER Response)
		    %% "The URI will be a SIP URI if the To header field in the
		    %% REGISTER request contained a SIP URI, else (if the To header field in
		    %% the REGISTER request contained a SIPS URI) it will be a SIPS URI."
		    sipurl:new([{proto,	To#sipurl.proto},
				{user,	UserPart},
				{host,	Domain}
			       ]);
		none ->
		    %% Create GRUU by using the To header (from the REGISTER request presumably)
		    %% with an 'opaque' parameter attached to it
		    sipurl:set([{pass,	none},
				{param,	["opaque=" ++ GRUU, "gruu"]}
			       ], To)
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (URL) ->
%%            {true, GRUU} | false
%%
%%            URL = #sipurl{}
%%
%%            GRUU = string()
%%
%% @doc     Check if a URL could be a GRUU we have generated. Does NOT
%%          check in the database to see if there is such a GRUU.
%%          NOTE : Only call this on a URL matching 'homedomain'.
%% @end
%%--------------------------------------------------------------------
is_gruu_url(URL) when is_record(URL, sipurl) ->
    case url_param:find(URL#sipurl.param_pairs, "gruu") of
	[_GRUUvalue] ->
	    case url_param:find(URL#sipurl.param_pairs, "opaque") of
		[Opaque] ->
		    {true, Opaque};
		[] ->
		    false
	    end;
	[] ->
	    {ok, GRUUindicator} = yxa_config:get_env(experimental_gruu_gruu_indicator),
	    is_gruu_prefix(GRUUindicator, URL#sipurl.user)
    end.

is_gruu_prefix([H | T1], [H | T2]) ->
    %% one more char match
    is_gruu_prefix(T1, T2);
is_gruu_prefix([], Rest) ->
    {true, Rest};
is_gruu_prefix(_In, _NoMatch) ->
    false.

%%--------------------------------------------------------------------
%% @spec    (Field, GRUU_DBE) -> term()
%%
%%            Field    = gruu            |
%%                       sipuser         |
%%                       instance_id     |
%%                       created         |
%%                       last_registered |
%%                       flags
%%            GRUU_DBE = #gruu_dbe{}
%%
%% @doc     Extract data from our private gruu_dbe record.
%% @end
%%--------------------------------------------------------------------
extract(gruu,		E) when is_record(E, gruu_dbe) -> E#gruu_dbe.gruu;
extract(sipuser,	E) when is_record(E, gruu_dbe) -> E#gruu_dbe.sipuser;
extract(instance_id,	E) when is_record(E, gruu_dbe) -> E#gruu_dbe.instance_id;
extract(created,	E) when is_record(E, gruu_dbe) -> E#gruu_dbe.created;
extract(last_registered,E) when is_record(E, gruu_dbe) -> E#gruu_dbe.last_registered;
extract(flags,		E) when is_record(E, gruu_dbe) -> E#gruu_dbe.flags.


%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Displays all currently registered GRUUs on the system
%%          console. For debugging use only.
%% @hidden
%% @end
%%--------------------------------------------------------------------
show_all() ->
    {ok, GRUUs} = database_gruu:fetch_all(),
    Fmt = "~15s   ~10s   ~40s   ~s~n",
    io:format(Fmt, ["GRUU", "User", "Instance", "Where"]),
    lists:map(fun(GRUU) when is_record(GRUU, gruu_dbe) ->
		      io:format(Fmt, [GRUU#gruu_dbe.gruu, GRUU#gruu_dbe.sipuser, GRUU#gruu_dbe.instance_id,
				      dump_loc(GRUU#gruu_dbe.gruu)])
	      end, GRUUs),
    ok.

%% part of show_all/0
dump_loc(GRUU) when is_list(GRUU) ->
    case get_contact_for_gruu(GRUU) of
	{ok, _User, Contact} when is_record(Contact, siplocationdb_e) ->
	    URLstr = sipurl:print(Contact#siplocationdb_e.address),
	    Outgoing =
		case lists:keysearch(outgoingproxy, 1, Contact#siplocationdb_e.flags) of
		    {value, {outgoingproxy, Out}} ->
			io_lib:format(" (behind ~s)", [Out]);
		    _ ->
			""
		end,
	    lists:append([URLstr, Outgoing]);
	{ok, _User, none} ->
	    "no active registration"
    end.

%%--------------------------------------------------------------------
%% @spec    (Contact, URI) ->
%%            NewURI
%%
%%            Contact = #contact{}
%%            URI     = #sipurl{}
%%
%%            NewURI = #sipurl{}
%%
%% @doc     Copy any 'grid' parameter from Request-URI to contact URI
%% @end
%%--------------------------------------------------------------------
prepare_contact(Contact, URI) ->
    %% "The server MUST copy the "grid" parameter from the Request URI (if
    %% present) into the new target URI obtained from the registered
    %% contact.  If the grid was already present in the contact bound to the
    %% GRUU, it is overwritten in this process." GRUU draft 06 #8.4.1 (Request Targeting)
    ContactURL = Contact#siplocationdb_e.address,
    URL_WithGrid =
	case url_param:find(URI#sipurl.param_pairs, "grid") of
	    [GRID] when is_list(GRID) ->
		NewParams = url_param:add(ContactURL#sipurl.param_pairs, "grid", GRID),
		sipurl:set([{param, NewParams}], ContactURL);
	    [] ->
		ContactURL
	end,
    URL_WithGrid.
