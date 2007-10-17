%%%-------------------------------------------------------------------
%%% File    : presence_pidf.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Presence PIDF document store. Input and output for this
%%%           module is always XML PIDF documents.
%%%           PIDF is Presence Information Data Format, described in
%%%           RFC3863.
%%%
%%% @since    30 Apr 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(presence_pidf).

-export([init/0,

	 set_pidf_for_user/6,
	 update_pidf_for_user/7,
	 refresh_pidf_user_etag/4,

	 get_pidf_xml_for_user/2,
	 get_pidfs_for_user/1,

	 check_if_user_etag_exists/2,
	 get_supported_content_types/0,
	 get_supported_content_types/1,
	 is_compatible_contenttype/2,

	 delete_pidf_for_user/1,
	 delete_expired/0,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("event.hrl").
-include("siprecords.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(pidf_data, {type,		%% pidf | cpim_pidf | xpidf | ...
		    xml,		%% string(), input XML
		    data		%% pidf_doc record()
		   }).

-record(pidf_doc, {'PRESENTITY_URL',	%% string(), specifies the "pres" URL of the PRESENTITY
		   'PRESENCE_TUPLES',	%% list() of 'PRESENCE_TUPLE' record()
		   'PRESENTITY_COMMENT'	%% undefined | string(), human readable comment about the PRESENTITY (optional)
		  }).

-record(pidf_type, {name,		%% string(), Content-Type header value syntax (lowercased)
		    type		%% atom(), pidf | xpidf
		   }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%% The different Content-Types we support, must be listed in order of our preference

-define(PIDF_TYPES, [#pidf_type{name = "application/pidf+xml",		%% RFC3863
				type = pidf
			       },
		     #pidf_type{name = "application/cpim-pidf+xml",	%% alias for pidf+xml (?)
				type = pidf
			       },
		     #pidf_type{name = "application/xpidf+xml",		%% draft-rosenberg-impp-pidf-00
				type = xpidf
			       }
		    ]).

-define(EXPIRES_LOWER_LIMIT, -1).
-define(EXPIRES_UPPER_LIMIT, 1150000000).


%%====================================================================
%% External functions
%%====================================================================



%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Initialization code for presence_pidf.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    ok.

%%--------------------------------------------------------------------
%% @spec    (User, ETag, Expires, ContentType, XML, Ctx) ->
%%            ok | {error, Reason}
%%
%%            User        = string() "presentity username"
%%            ETag        = string() "presence ETag"
%%            Expires     = integer() "how many seconds this data is valid"
%%            ContentType = string() "\"application/pidf+xml\" | ..."
%%            XML         = string() "XML data"
%%            Ctx         = #event_ctx{}
%%
%% @doc     Parse and store an PIDF XML associated with a presentity.
%% @end
%%--------------------------------------------------------------------
set_pidf_for_user(_User, _ETag, Expires, _ContentType, _XML, _Ctx) when is_integer(Expires) andalso
								          (Expires < ?EXPIRES_LOWER_LIMIT orelse
								           Expires > ?EXPIRES_UPPER_LIMIT) ->
    {error, expires_out_of_bounds};
set_pidf_for_user(User, ETag, Expires, ContentType, XML, Ctx) when is_list(User), is_list(XML),
								   is_record(Ctx, event_ctx) ->
    case parse_pidf_xml(ContentType, XML) of
	{ok, Data1} when is_record(Data1, pidf_data) ->
	    UseExpires =
		if
		    is_integer(Expires) ->
			Now = util:timestamp(),
			Now + Expires;
		    Expires == never ->
			never
		end,

	    case check_if_user_etag_exists(User, ETag) of
		false ->
		    Flags = [{source, Ctx#event_ctx.dialog_id}],
		    {atomic, ok} = database_eventdata:insert("presence", {user, User}, ETag, UseExpires, Flags, Data1),
		    ok;
		true ->
		    {error, etag_already_exists}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec    (User, ETag, NewEtag, Expires, ContentType, XML, Ctx) ->
%%            ok | nomatch | {error, Reason}
%%
%%            User        = string() "presentity username"
%%            ETag        = string() "current ETag"
%%            NewETag     = string() "new ETag"
%%            Expires     = integer() "how many seconds this data is valid"
%%            ContentType = string() "\"application/pidf+xml\" | ..."
%%            XML         = string() "XML data"
%%            Ctx         = #event_ctx{}
%%
%% @doc     Parse and update an PIDF XML associated with a presentity.
%% @end
%%--------------------------------------------------------------------
update_pidf_for_user(_User, _ETag, _NewEtag, Expires, _ContentType, _XML, _Ctx) when is_integer(Expires) andalso
								                (Expires < ?EXPIRES_LOWER_LIMIT orelse
								                 Expires > ?EXPIRES_UPPER_LIMIT) ->
    {error, expires_out_of_bounds};
update_pidf_for_user(User, ETag, NewETag, Expires, ContentType, XML, Ctx) when is_list(User), is_list(XML),
									       is_record(Ctx, event_ctx) ->
    case parse_pidf_xml(ContentType, XML) of
	{ok, Data1} when is_record(Data1, pidf_data) ->
	    UseExpires =
		if
		    is_integer(Expires) ->
			Now = util:timestamp(),
			Now + Expires;
		    Expires == never ->
			never
		end,

	    Flags = [{source, Ctx#event_ctx.dialog_id}],
	    case database_eventdata:update("presence", {user, User}, ETag, NewETag, UseExpires, Flags, Data1) of
		nomatch ->
		    nomatch;
		ok ->
		    ok
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @spec    (User, ETag, NewExpires, NewETag) ->
%%            ok | nomatch | {error, Reason}
%%
%%            User       = string() "presentity username"
%%            ETag       = string() "presence ETag"
%%            NewExpires = integer() "how many more seconds this record is now valid (NOT util:timestamp/0 format)"
%%            NewETag    = string() "new ETag for this record"
%%
%%            Reason = atom
%%
%% @doc     Update the expiration time of the record for User with
%%          etag ETag. Also changes the ETag to a new value, because
%%          of how the RFC is written. I don't know why the ETag has
%%          to change. As a validiator of sequentiality perhaps.
%% @end
%%--------------------------------------------------------------------
refresh_pidf_user_etag(_User, _ETag, NewExpires, _NewETag) when is_integer(NewExpires) andalso
                                                                (NewExpires < ?EXPIRES_LOWER_LIMIT orelse
                                                                 NewExpires > ?EXPIRES_UPPER_LIMIT) ->
    {error, expires_out_of_bounds};
refresh_pidf_user_etag(User, ETag, NewExpires, NewETag) when is_list(User) ->
    UseExpires =
	if
	    is_integer(NewExpires) ->
		Now = util:timestamp(),
		Now + NewExpires;
	    NewExpires == never ->
		never
	end,
    database_eventdata:refresh_presentity_etag({user, User}, ETag, UseExpires, NewETag).

%%--------------------------------------------------------------------
%% @spec    (User, AcceptL) ->
%%            {ok, ContentType, PIDF_document} |
%%            {error, Reason}
%%
%%            User    = {fake_offline, AddrStr} | string() "presentity username"
%%            AddrStr = string() "presentity address string"
%%            Accept  = [string()] "content types the UA we are going to send the PIDF to accepts"
%%
%%            ContentType   = string()
%%            PIDF_document = io_list()
%%
%% @doc     Return a Presence PIDF document for User. If User is
%%          'none' we generate a fake offline presence document.
%% @end
%%--------------------------------------------------------------------
get_pidf_xml_for_user({fake_offline, AddrStr}, AcceptL) when is_list(AddrStr), is_list(AcceptL) ->
    case get_best_accepted_content_type(AcceptL) of
        {error, Reason} ->
            {error, Reason};
        ContentType when is_list(ContentType) ->
	    Presentity = normalize_entity(AddrStr),
	    Type = content_type(ContentType),
	    case Type == pidf orelse Type == xpidf of
		true ->
		    TupleXML =
			"<tuple id=\"1\">\n"
			"    <status><basic>closed</basic></status>\n"
			"</tuple>\n",
		    try output_pidf_xml(Type, Presentity, "1", [TupleXML]) of
			PIDF_XML when is_list(PIDF_XML) ->
			    {ok, ContentType, PIDF_XML}
		    catch
			_:_ ->
			    {error, failed_generating_xml}
		    end;
		false ->
		    {error, "Unable to produce PIDF document for Content-Type"}
	    end
    end;

get_pidf_xml_for_user(User, AcceptL) when is_list(User), is_list(AcceptL) ->
    %% get list of eventdata_dbe records
    L = get_pidfs_for_user(User),

    case get_best_accepted_content_type(AcceptL) of
	{error, Reason} ->
	    {error, Reason};
	ContentType when is_list(ContentType) ->
	    Tuples_as_XML1 = [((E#eventdata_dbe.data)#pidf_data.data)#pidf_doc.'PRESENCE_TUPLES' || E <- L],

	    %% flatten one level
	    Tuples_as_XML = lists:append(Tuples_as_XML1),

	    XML =
		case L of
		    [] ->
			"";
		    [#eventdata_dbe{data = First} | _] ->
			Presentity = (First#pidf_data.data)#pidf_doc.'PRESENTITY_URL',

			try output_pidf_xml(content_type(ContentType), Presentity, User, Tuples_as_XML) of
			    Res ->
				Res
			catch
			    error : Y ->
				ST = erlang:get_stacktrace(),
				logger:log(error, "Presence PIDF: Failed generating XML document,~ncaught error : "
					   "~p ~p", [Y, ST]),
				error;
			    X : Y ->
				logger:log(error, "Presence PIDF: Failed generating XML document, caught ~p : ~p",
					   [X, Y]),
				error
			end
		end,

	    case is_list(XML) of
		true ->
		    {ok, ContentType, XML};
		false ->
		    {error, failed_generating_xml}
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (Type, Presentity, User, Tuples) ->
%%            PIDF_XML
%%
%%            Type       = pidf | xpidf
%%            Presentity = string() "\"pres:\" URL of presentity"
%%            User       = none | string() "presentity username"
%%            Tuples     = [string()]
%%
%%            PIDF_XML = string() "PIDF document"
%%
%% @doc     Merge a bunch of XML tuples into a PIDF document of the
%%          requested type. Tuples should be a list of XML presence
%%          tuples, e.g. : [ ``` "<tuple id=\"foo\">\n" " <status>\n"
%%          " <basic>open</basic>\n" " </status>\n" "</tuple>\n" '''
%%          ]
%% @end
%%--------------------------------------------------------------------
%%
%% application/pidf+xml or application/cpim-pidf+xml
%%
output_pidf_xml(pidf, Presentity, _User, Tuples) when is_list(Presentity), is_list(Tuples) ->
    UniqueTuples = get_unique_tuples(Tuples),

    Res = [
	   "<?xml version=\"1.0\"?>\n"
	   "<presence xmlns=\"urn:ietf:params:xml:ns:pidf\" entity=\"" ++ Presentity ++ "\">\n",
	   [["\t", E, "\n"] || E <- UniqueTuples],
	   "</presence>\n"
	  ],

    binary_to_list( list_to_binary(Res) );

%%
%% application/xpidf+xml - We really don't handle xpidf (outdated),
%% just try to make it look like we do
output_pidf_xml(xpidf, Presentity, User, TuplesIn) when is_list(Presentity), is_list(TuplesIn) ->
    Scanned = [xmerl_scan:string(lists:flatten(E)) || E <- TuplesIn],
    Tuples = lists:map(fun({E, []}) ->
			       %% No LC since we want to make sure the second element really is []
			       E
		       end, Scanned),

    case priority_sort_tuples(Tuples) of
	[Best | _] ->
	    "pres:" ++ PresRest = Presentity,
	    Address = "sip:" ++ PresRest,

	    Status =
		case get_xml_elements(status, Best#xmlElement.content) of
		    [Status1] when is_record(Status1, xmlElement) ->
			case get_xml_elements(basic, Status1#xmlElement.content) of
			    [#xmlElement{content = [#xmlText{value = StatusV1}]
					}] when is_list(StatusV1) ->
				%% yuck yuck
				StatusV1;
			    _ ->
				"unknown"
			end;
		    _ ->
			"unknown"
		end,

	    Res = [
		   "<?xml version=\"1.0\"?>\n"
		   "<!DOCTYPE presence PUBLIC \"-//IETF//DTD RFCxxxx XPIDF 1.0//EN\" \"xpidf.dtd\">\n"
		   "<presence>\n"
		   "    <presentity uri=\"", Presentity, ";method=SUBSCRIBE\"/>\n"
		   "    <atom id=\"", User, "\">\n"
		   "        <address uri=\"", Address, "\" priority=\"0.0\">\n"
		   "            <status status=\"", Status, "\"/>\n",
		   case Status of
		       "open" ->
			   "            <msnsubstatus substatus=\"online\"/>\n";
		       "closed" ->
			   "            <msnsubstatus substatus=\"offline\"/>\n";
		       _ ->
			   ""
		   end,
		   "        </address>\n"
		   "    </atom>\n"
		   "</presence>\n"
		  ],

	    binary_to_list( list_to_binary(Res) );
	[] ->
	    ""
    end.


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            Out
%%
%%            In = [#xmlElement{}]
%%
%%            Out = [#xmlElement{}]
%%
%% @doc     Sort a number of PIDF tuples according to their 'priority'
%%          XML value.
%% @end
%%--------------------------------------------------------------------
priority_sort_tuples(In) when is_list(In) ->
    lists:sort(fun sort_xml_presence_tuples/2, In).

%% part of priority_sort_tuples/1
%% Returns : true | false
sort_xml_presence_tuples(A, B) ->
    APrio = sort_xml_presence_tuples_get_prio(A),
    BPrio = sort_xml_presence_tuples_get_prio(B),

    (APrio < BPrio).

%% part of sort_xml_presence_tuples/2
%% Returns : float()
sort_xml_presence_tuples_get_prio(#xmlElement{content = Content}) ->
    case get_xml_elements(contact, Content) of
	[] ->
	    0.000;
	Contact ->
	    case get_xml_attributes(priority, Contact) of
		[Prio1] ->
		    %% XXX check if valid value
		    list_to_float(Prio1);
		_ ->
		    0.000
	    end
    end.

%%--------------------------------------------------------------------
%% @spec    (User, ETag) ->
%%            {ok, Entry} |
%%            nomatch
%%
%%            User = list() "SIP username of presentity"
%%            ETag = string() "ETag header value"
%%
%%            Entry = #evendata_dbe{}
%%
%% @doc     Fetch an eventdata_dbe record from the event database,
%%          given a User and ETag.
%% @end
%%--------------------------------------------------------------------
get_user_etag(User, ETag) when is_list(User), is_list(ETag) ->
    database_eventdata:fetch_using_presentity_etag({user, User}, ETag).

%%--------------------------------------------------------------------
%% @spec    (User, ETag) -> true | false
%%
%%            User = list() "SIP username of presentity"
%%            ETag = string() "ETag header value"
%%
%% @doc     Check if an entry exists in the event database for a User
%%          and ETag.
%% @end
%%--------------------------------------------------------------------
check_if_user_etag_exists(User, ETag) when is_list(User), is_list(ETag) ->
    case get_user_etag(User, ETag) of
	{ok, Entry} when is_record(Entry, eventdata_dbe) ->
	    true;
	nomatch ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (User) -> ok
%%
%%            User = list() "SIP username of presentity"
%%
%% @doc     Delete an entry from the event data Mnesia database.
%% @end
%%--------------------------------------------------------------------
delete_pidf_for_user(User) when is_list(User) ->
    {atomic, ok} = database_eventdata:delete_using_presentity({user, User}),
    ok.

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Purge expired event data from the Mnesia database.
%% @end
%%--------------------------------------------------------------------
delete_expired() ->
    {ok, _} = database_eventdata:delete_expired(),
    ok.

%%--------------------------------------------------------------------
%% @spec    () -> [string()]
%%
%% @doc     Return a list of our supported content types as strings,
%%          in the order of our preference (best first).
%% @end
%%--------------------------------------------------------------------
get_supported_content_types() ->
    [E#pidf_type.name || E <- ?PIDF_TYPES].

%%--------------------------------------------------------------------
%% @spec    (set) -> [string()]
%%
%% @doc     Return a list of the content types we allow a client to
%%          PUBLISH/NOTIFY a PIDF document using.
%% @end
%%--------------------------------------------------------------------
get_supported_content_types(set) ->
    %% We currently can't parse application/xpidf+xml, only output something that looks like it
    [E#pidf_type.name || E <- ?PIDF_TYPES] -- ["application/xpidf+xml"].

%%--------------------------------------------------------------------
%% @spec    (PubOrSub, AcceptL) -> true | false
%%
%%            PubOrSub = publish | subscribe
%%            AcceptL  = [string()] "Content-Type values"
%%
%% @doc     Check if our peers list of content types are acceptable
%%          for us, for this kind of operation (publish or
%%          subscribe).
%% @end
%%--------------------------------------------------------------------
is_compatible_contenttype(PubOrSub, AcceptL) when is_atom(PubOrSub), is_list(AcceptL) ->
    case get_best_accepted_content_type(AcceptL) of
	{error, _} ->
	    false;
	Res when is_list(Res) ->
	    true
    end.


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (ContentType) ->
%%            Type
%%
%%            ContentType = string() "must be lower cased"
%%
%%            Type = atom()
%%
%% @doc     Turn Content-Type into atom representation. ContentType
%%          must be lower cased.
%% @end
%%--------------------------------------------------------------------
content_type(Name) ->
    Res = [E#pidf_type.type || E <- ?PIDF_TYPES, E#pidf_type.name == Name],
    case Res of
	[Type] when is_atom(Type) -> Type;
	[] -> error
    end.

%%--------------------------------------------------------------------
%% @spec    (Accept) ->
%%            ContentType                         |
%%            {error, no_acceptable_content_type}
%%
%%            Accept = [string()] "our peers list of it's accepted content types"
%%
%%            ContentType = string()
%%
%% @doc     Find the best Content-Type match between our preferred
%%          ones, and the list our SIP peer says it supports.
%% @end
%%--------------------------------------------------------------------
get_best_accepted_content_type(Accept) ->
    Preference = get_supported_content_types(),
    case get_best_accepted_content_type2(Preference, Accept) of
	nomatch ->
	    {error, no_acceptable_content_type};
	Res ->
	    Res
    end.

get_best_accepted_content_type2([H | T], Accept) ->
    case lists:member(http_util:to_lower(H), Accept) of
	true ->
	    H;
	false ->
	    get_best_accepted_content_type2(T, Accept)
    end;
get_best_accepted_content_type2([], _Accept) ->
    nomatch.

%%--------------------------------------------------------------------
%% @spec    (ContentType, XML) ->
%%            {ok, PIDF_Doc}  |
%%            {error, Reason}
%%
%%            ContentType = string() "(\"application/pidf+xml\" | ...)"
%%            XML         = string() "PIDF document"
%%
%%            PIDF_Doc = #pidf_doc{}
%%            Reason   = atom()
%%
%% @doc     Parse an XML document into our internal PIDF
%%          representation (pidf_doc record()).
%% @end
%%--------------------------------------------------------------------
parse_pidf_xml(ContentType, XML) ->
    case content_type(ContentType) of
	error ->
	    {error, unknown_content_type};
	pidf ->
	    parse_pidf(pidf, XML);
	_ ->
	    {error, unsupported_content_type}
    end.

%% Returns : {ok, PIDF} | {error, Reason}
%%           PIDF_Doc = pidf_doc record()
%%           Reason   = atom()
parse_pidf(pidf, XML) when is_list(XML) ->
    try xmerl_scan:string(XML, [{namespace_conformant, true}]) of
	{XMLtag, []} ->
	    try parse_pidf_xml(XMLtag) of
		{ok, PIDF_Doc} ->
		    {ok, #pidf_data{type = pidf,
				    xml  = XML,
				    data = PIDF_Doc
				   }}
	    catch
		error : Y ->
		    ST = erlang:get_stacktrace(),
		    logger:log(error, "Presence PIDF: Could not parse pidf-xml data,~ncaught error : "
			       "~p ~p", [Y, ST]),
		    {error, bad_xml};
		X : Y ->
		    logger:log(error, "Presence PIDF: Could not parse pidf-xml data, caught ~p ~p", [X, Y]),
		    {error, bad_xml}
	    end;
	Unknown ->
	    logger:log(error, "Presence PIDF: Could not parse presence XML document : ~p", [Unknown]),
	    {error, bad_xml}
    catch
	X: Y ->
	    logger:log(error, "Presence PIDF: Could not parse presence XML document, caught ~p ~p",
		       [X, Y]),
	    {error, bad_xml}
    end.

%% Returns : {ok, PIDF_Doc} | {error, Reason}
%%           PIDF_Doc = pidf_doc record()
%%           Reason   = atom()
parse_pidf_xml(#xmlElement{name = presence} = XML) ->
    parse_pidf_xml2(XML);
parse_pidf_xml(#xmlElement{expanded_name = {_URI, presence}} = XML) ->
    parse_pidf_xml2(XML).

parse_pidf_xml2(XML) ->
    [Entity] = get_xml_attributes(entity, XML#xmlElement.attributes),

    Comment =
	case get_xml_attributes(comment, XML#xmlElement.attributes) of
	    [Comment1] -> Comment1;
	    _ -> undefined
	end,

    Tuples = get_xml_elements(tuple, XML#xmlElement.content),

    XMLTuples = [lists:flatten(E) || E <- xmerl:export_simple_content(Tuples, presence_xmerl_xml)],

    This = #pidf_doc{'PRESENTITY_URL'     = normalize_entity(Entity),
		     'PRESENCE_TUPLES'    = XMLTuples,
		     'PRESENTITY_COMMENT' = Comment
		    },

    {ok, This}.


%%--------------------------------------------------------------------
%% @spec    (Entity) ->
%%            Presentity
%%
%%            Entity = string() "from presence entity attr in PIDFs"
%%
%%            Presentity = string()
%%
%% @doc     Change sip: into pres: in presence entitys and normalize
%%          some things people have gotten wrong at SIPits.
%% @end
%%--------------------------------------------------------------------
normalize_entity(Entity) ->
    Presentity1 =
	case Entity of
	    "sip:" ++ PresRest ->
		"pres:" ++ PresRest;
	    "pres:" ++ _ ->
		Entity;
	    "<" ++ _ ->
		%% use concact-parse to normalize "<sip:...>" into "sip:..."
		[C] = contact:parse([Entity]),
		case C#contact.urlstr of
		    "sip:" ++ PresRest2 ->
			"pres:" ++ PresRest2;
		    "pres:" ++ _ ->
			C#contact.urlstr
		end
	end,
    lists:flatten(Presentity1).


%%--------------------------------------------------------------------
%% @spec    (Name, In) ->
%%            Values
%%
%%            Name = atom()
%%            In   = [term()]
%%
%%            Values = [string()]
%%
%% @doc     Look for xmlAttribute record() with name matching Name.
%%          Extract the value elements of the xmlAttribute records
%%          matching.
%% @end
%%--------------------------------------------------------------------
get_xml_attributes(Name, In) when is_atom(Name), is_list(In) ->
    get_xml_attributes2(Name, In, []).

get_xml_attributes2(Name, [#xmlAttribute{name = Name} = H | T], Res) ->
    This = H#xmlAttribute.value,
    get_xml_attributes2(Name, T, [This | Res]);
get_xml_attributes2(Name, [_H | T], Res) ->
    get_xml_attributes2(Name, T, Res);
get_xml_attributes2(_Name, [], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% @spec    (Name, In) ->
%%            Elements
%%
%%            Name = atom()
%%            In   = [term()]
%%
%%            Elements = [#xmlElement{}]
%%
%% @doc     Look for xmlElement record() with name matching Name.
%%          Return all matching xmlElement records.
%% @end
%%--------------------------------------------------------------------
get_xml_elements(Name, In) when is_atom(Name), is_list(In) ->
    get_xml_elements2(Name, In, []).

get_xml_elements2(Name, [#xmlElement{name = Name} = H | T], Res) ->
    get_xml_elements2(Name, T, [H | Res]);
get_xml_elements2(Name, [#xmlElement{expanded_name = {_URI, Name}} = H | T], Res) ->
    get_xml_elements2(Name, T, [H | Res]);
get_xml_elements2(Name, [_H | T], Res) ->
    get_xml_elements2(Name, T, Res);
get_xml_elements2(_Name, [], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% @spec    (User) -> [#eventdata_dbe{}]
%%
%%            User = string() "presentity username"
%%
%% @doc     Fetch all eventdata entrys matching our presentity.
%% @end
%%--------------------------------------------------------------------
get_pidfs_for_user(User) when is_list(User) ->
    case database_eventdata:fetch_using_presentity({user, User}) of
	{ok, L} when is_list(L)->
	    L;
	nomatch ->
	    []
    end.


%%--------------------------------------------------------------------
%% @spec    (Tuples) ->
%%            NewTuples
%%
%%            Tuples = [string()] "list of presence tuples in XML format"
%%
%%            NewTuples = [string()]
%%
%% @doc     Make a list of tuples with unique id's from a list that
%%          might contain tuples with duplicate id's. NOTE : We just
%%          ignore all but the first one for every Id. Which one is
%%          the first is rather arbitrary. We should probably keep
%%          the most recent tuple, or make unique ids based on the
%%          tuple and where it came from or something.
%% @end
%%--------------------------------------------------------------------
get_unique_tuples(Tuples) when is_list(Tuples) ->
    Parsed = [{get_tuple_id(E), E} || E <- Tuples],
    get_unique_tuples2(Parsed, [], []).

%% part of get_unique_tuples/1
get_unique_tuples2([{Id, Tuple} | T], Seen, Res) ->
    case lists:member(Id, Seen) of
	true ->
	    logger:log(debug, "Presence PIDF: IGNORING tuple with duplicate Id ~p : ~p~nIncluded tuples : ~p",
		       [Id, Tuple, Res]),
	    get_unique_tuples2(T, Seen, Res);
	false ->
	    get_unique_tuples2(T, [Id | Seen], [Tuple | Res])
    end;
get_unique_tuples2([], _Seen, Res) ->
    lists:reverse(Res).

%% part of get_unique_tuples/1
%% Returns : Id = string()
get_tuple_id(Tuple) ->
    {XML, []} = xmerl_scan:string(lists:flatten(Tuple)),
    [Id] = get_xml_attributes(id, XML#xmlElement.attributes),
    Id.




%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->

    %% normalize_entity(Entity)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "normalize_entity/1 - 1"),
    "pres:test@foo" = normalize_entity("sip:test@foo"),

    autotest:mark(?LINE, "normalize_entity/1 - 2"),
    "pres:test@foo" = normalize_entity("pres:test@foo"),

    autotest:mark(?LINE, "normalize_entity/1 - 3"),
    "pres:test@foo" = normalize_entity("<sip:test@foo>"),

    autotest:mark(?LINE, "normalize_entity/1 - 4"),
    "pres:test@foo" = normalize_entity("<pres:test@foo>"),


    %% Mnesia dependant tests
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "Mnesia setup - 0"),

    database_eventdata:test_create_table(),

    case mnesia:transaction(fun test_mnesia_dependant_functions/0) of
	{aborted, ok} ->
	    ok;
	{aborted, Res} ->
	    {error, Res}
    end.


test_mnesia_dependant_functions() ->
    autotest:mark(?LINE, "presence_pidf - 0"),

    Now = util:timestamp(),

    PIDF_XML1_Tuple1 =
        "        <tuple id=\"1-2-3-4\">\n"
        "                <status>\n"
        "                        <basic>closed</basic>\n"
        "                </status>\n"
        "                <contact priority=\"0.000\">sip:user@example.org</contact>\n"
        "        </tuple>\n",

    PIDF_XML1 =
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"     <presence xmlns=\"urn:ietf:params:xml:ns:cpim-pidf\" entity=\"pres:user@example.org\">\n" ++
	PIDF_XML1_Tuple1 ++
	"     </presence>\n",

    %% parse_xml(Type, XML)
    %%--------------------------------------------------------------------
    {ok, ParseCT_Tuples1} = test_parse_xml(PIDF_XML1, PIDF_XML1_Tuple1),

    %% set_pidf_for_user(User, ETag, Expires, {ContentType, XML})
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "set_pidf_for_user/4 - 0"),
    SPFU_User1 = "__test_user1__",
    SPFU_ETag1 = "test-" ++ "-" ++ integer_to_list(Now) ++ "::" ++ integer_to_list(?LINE),
    %% make sure we have a clean sheat
    delete_pidf_for_user(SPFU_User1),
    {ok, "application/pidf+xml", []} = get_pidf_xml_for_user(SPFU_User1, ["application/pidf+xml"]),

    autotest:mark(?LINE, "set_pidf_for_user/4 - 1.1"),
    ok = set_pidf_for_user(SPFU_User1, SPFU_ETag1, 5, "application/pidf+xml", PIDF_XML1, #event_ctx{}),

    autotest:mark(?LINE, "set_pidf_for_user/4 - 1.2"),
    %% verify single entry
    [#eventdata_dbe{expires = SFPU_Expires1, data = SPFU_Entry1}] = get_pidfs_for_user(SPFU_User1),
    #pidf_data{type    = pidf,
	       xml     = PIDF_XML1,
	       data    = #pidf_doc{'PRESENTITY_URL'  = "pres:user@example.org",
				   'PRESENCE_TUPLES' = ParseCT_Tuples1
				  }
	      } = SPFU_Entry1,

    %% check that expires is between now and in five seconds (six, Mnesia transactions are playful)
    true = (SFPU_Expires1 =< Now + 6 andalso SFPU_Expires1 >= Now),

    autotest:mark(?LINE, "set_pidf_for_user/4 - 2.1"),
    SPFU_ETag2 = "test-" ++ "-" ++ integer_to_list(Now) ++ "::" ++ integer_to_list(?LINE),

    PIDF_XML2_Tuple1 =
        "        <tuple id=\"2\">\n"
        "                <status>\n"
        "                        <basic>closed</basic>\n"
        "                </status>\n"
        "                <contact priority=\"1.000\">sip:user@example.org</contact>\n"
        "        </tuple>\n",

    PIDF_XML2 =
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"     <presence xmlns=\"urn:ietf:params:xml:ns:cpim-pidf\" entity=\"pres:user@example.org\">\n" ++
	PIDF_XML2_Tuple1 ++
	"     </presence>\n",

    ok = set_pidf_for_user(SPFU_User1, SPFU_ETag2, 5, "application/pidf+xml", PIDF_XML2, #event_ctx{}),

    autotest:mark(?LINE, "set_pidf_for_user/4 - 2.2"),
    [#eventdata_dbe{data = SPFU_Entry1},
     #eventdata_dbe{expires = SFPU_Expires2,
		    data    = SPFU_Entry2
		   }] = get_pidfs_for_user(SPFU_User1),
    %% verify second entry
    #pidf_data{type    = pidf,
	       xml     = PIDF_XML2,
	       data    = #pidf_doc{'PRESENTITY_URL'  = "pres:user@example.org",
				   'PRESENCE_TUPLES' = ParseCT_Tuples2
				  }
	      } = SPFU_Entry2,

    %% check that expires is between now and in five seconds (six, Mnesia transactions are playful)
    true = (SFPU_Expires2 =< Now + 6 andalso SFPU_Expires2 >= Now),

    ok = test_verify_tuples([PIDF_XML2_Tuple1], ParseCT_Tuples2),

    autotest:mark(?LINE, "set_pidf_for_user/4 - 3"),
    %% test same etag, and also test max Expires value
    {error, etag_already_exists} =
	set_pidf_for_user(SPFU_User1, SPFU_ETag2, ?EXPIRES_UPPER_LIMIT, "application/pidf+xml",
			  PIDF_XML2, #event_ctx{}),

    autotest:mark(?LINE, "set_pidf_for_user/4 - 4.1"),
    %% test out of bounds Expires
    {error, expires_out_of_bounds} =
	set_pidf_for_user(SPFU_User1, SPFU_ETag2, ?EXPIRES_LOWER_LIMIT - 1, "application/pidf+xml",
			  PIDF_XML2, #event_ctx{}),

    autotest:mark(?LINE, "set_pidf_for_user/4 - 4.2"),
    %% test out of bounds Expires
    {error, expires_out_of_bounds} =
	set_pidf_for_user(SPFU_User1, SPFU_ETag2, ?EXPIRES_UPPER_LIMIT + 1, "application/pidf+xml",
			  PIDF_XML2, #event_ctx{}),


    %% refresh_pidf_user_etag(User, ETag, NewExpires, NewETag)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 1.1"),
    RPUE_Now = util:timestamp(),
    %% update first entry from previous test with new expires-time
    RPUE_ETag1 = "test-" ++ "-" ++ integer_to_list(RPUE_Now) ++ "::" ++ integer_to_list(?LINE),
    ok = refresh_pidf_user_etag(SPFU_User1, SPFU_ETag1, 10, RPUE_ETag1),

    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 1.2"),
    %% verify
    nomatch = get_user_etag(SPFU_User1, SPFU_ETag1),
    {ok, #eventdata_dbe{expires = RPUE_Expires1, data = RPUE_Entry1}} = get_user_etag(SPFU_User1, RPUE_ETag1),

    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 1.3"),
    %% verify that our record matches SPFU_Entry1
    RPUE_Entry1 = SPFU_Entry1,

    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 1.4"),
    %% check that expires is between six and ten seconds (eleven, Mnesia transactions are playful), not five or lower
    RPUE_Expires1_verdict = (RPUE_Expires1 =< RPUE_Now + 11 andalso RPUE_Expires1 >= RPUE_Now + 6),
    {expires, true} = {expires, RPUE_Expires1_verdict},

    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 2.1"),
    %% expire record by setting expires = -1
    RPUE_ETag2 = "test-" ++ "-" ++ integer_to_list(RPUE_Now) ++ "::" ++ integer_to_list(?LINE),
    ok = refresh_pidf_user_etag(SPFU_User1, RPUE_ETag1, -1, RPUE_ETag2),

    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 2.2"),
    %% verify that we don't get it from get_pidfs_for_user
    [#eventdata_dbe{data = SPFU_Entry2}] = get_pidfs_for_user(SPFU_User1),


    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 3"),
    %% verify that we can't refresh non-existing entrys, and also test max NewExpires value
    RPUE_ETag3 = "test-" ++ "-" ++ integer_to_list(RPUE_Now) ++ "::" ++ integer_to_list(?LINE),
    nomatch = refresh_pidf_user_etag(SPFU_User1, RPUE_ETag3, ?EXPIRES_UPPER_LIMIT, RPUE_ETag3),

    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 4.1"),
    %% test out of bounds NewExpires
    {error, expires_out_of_bounds} =
	refresh_pidf_user_etag(SPFU_User1, RPUE_ETag3, ?EXPIRES_LOWER_LIMIT - 1, RPUE_ETag3),

    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 4.2"),
    %% test out of bounds NewExpires
    {error, expires_out_of_bounds} =
	refresh_pidf_user_etag(SPFU_User1, RPUE_ETag3, ?EXPIRES_UPPER_LIMIT + 10, RPUE_ETag3),

    autotest:mark(?LINE, "refresh_pidf_user_etag/4 - 5"),
    %% clean up
    ok = delete_pidf_for_user(SPFU_User1),


    %% delete_expired()
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "delete_expired/0 - 0"),
    ExpiredNow = util:timestamp(),

    DE_User1 = "__test_expired_user1__",
    DE_ETag1 = "test-" ++ "-" ++ integer_to_list(ExpiredNow) ++ "::" ++ integer_to_list(?LINE),
    %% not-expired entry
    ok = set_pidf_for_user(DE_User1, DE_ETag1, 5, "application/pidf+xml", PIDF_XML2, #event_ctx{}),

    DE_ETag2 = "test-" ++ "-" ++ integer_to_list(ExpiredNow) ++ "::" ++ integer_to_list(?LINE),
    %% expired entry
    ok = set_pidf_for_user(DE_User1, DE_ETag2, -1, "application/pidf+xml", PIDF_XML2, #event_ctx{}),

    autotest:mark(?LINE, "delete_expired/0 - 1"),
    ok = delete_expired(),

    autotest:mark(?LINE, "delete_expired/0 - 2"),
    %% verify the contents in the database after deletion of expired entrys
    [#eventdata_dbe{etag = DE_ETag1}] = get_pidfs_for_user(DE_User1),


    %% check_if_user_etag_exists(User, ETag)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "check_if_user_etag_exists/2 - 1"),
    true = check_if_user_etag_exists(DE_User1, DE_ETag1),

    autotest:mark(?LINE, "check_if_user_etag_exists/2 - 1"),
    false = check_if_user_etag_exists("__test_foo_other_user9__", DE_ETag1),

    autotest:mark(?LINE, "check_if_user_etag_exists/2 - 1"),
    false = check_if_user_etag_exists(DE_User1, DE_ETag1 ++ "_xxx99435"),


    %% delete_pidf_for_user(User)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "delete_pidf_for_user/1 - 1.0"),
    %% verify entry from last test still exists
    [#eventdata_dbe{etag = DE_ETag1}] = get_pidfs_for_user(DE_User1),

    autotest:mark(?LINE, "delete_pidf_for_user/1 - 1.1"),
    ok = delete_pidf_for_user(DE_User1),

    autotest:mark(?LINE, "delete_pidf_for_user/1 - 1.2"),
    [] = get_pidfs_for_user(DE_User1),


    %% get_pidf_xml_for_user(User, AcceptL)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 0"),

    GP_PIDF_XLM1_Tuple1 =
        "        <tuple id=\"1-2-3-4\">\n"
        "                <status>\n"
        "                        <basic>closed</basic>\n"
        "                </status>\n"
        "                <contact priority=\"0.000\">sip:user@example.org</contact>\n"
        "        </tuple>\n",

    GP_PIDF_XLM1_Tuple2 =
        "        <tuple id=\"adfkh4\">\n"
        "                <status>\n"
        "                        <basic>open</basic>\n"
        "                </status>\n"
        "                <contact priority=\"1.000\">sip:user@example.org;line=2</contact>\n"
        "        </tuple>\n",

    GP_PIDF_XML1 =
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"     <presence xmlns=\"urn:ietf:params:xml:ns:cpim-pidf\" entity=\"pres:user@example.org\">\n" ++
	GP_PIDF_XLM1_Tuple1 ++
	GP_PIDF_XLM1_Tuple2 ++
	"        <comment>presence is fun, XML is not</comment>\n"
	"     </presence>\n",


    GP_PIDF_XLM2_Tuple1 =
        "        <tuple id=\"asdfg32\">\n"
        "                <status>\n"
        "                        <basic>open</basic>\n"
        "                        <hs:detected-state xmlns:hs=\"http://www.hotsip.com/presence-1.0\">"
	"active</hs:detected-state>\n"
        "                </status>\n"
        "                <contact priority=\"0.5\">sip:user@example.org</contact>\n"
        "        </tuple>\n",

    GP_PIDF_XML2 =
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	"     <presence xmlns=\"urn:ietf:params:xml:ns:cpim-pidf\" entity=\"pres:user@example.org\">\n" ++
	GP_PIDF_XLM2_Tuple1 ++
	"        <comment>This is a comment in the second presence document</comment>\n"
	"     </presence>\n",



    GP_User1 = "__test_getpidfxml_user1__",

    ok = delete_pidf_for_user(GP_User1),
    {ok, "application/pidf+xml", []} = get_pidf_xml_for_user(GP_User1, ["application/pidf+xml"]),

    ok = set_pidf_for_user(GP_User1, SPFU_ETag1, 15, "application/pidf+xml", GP_PIDF_XML1, #event_ctx{}),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 1.1"),
    {ok, "application/pidf+xml", GP_PIDF_XML1_Out1} =
	get_pidf_xml_for_user(GP_User1, ["application/made-up-pidf+xml", "application/pidf+xml"]),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 1.2"),
    %% verify entry
    {ok, #pidf_data{type   = pidf,
		    xml    = GP_PIDF_XML1_Out1,
		    data   = #pidf_doc{'PRESENTITY_URL' = "pres:user@example.org",
				       'PRESENCE_TUPLES' = GP_PIDF_XML1_Out1_Tuples
				      }
		   }} = parse_pidf_xml("application/pidf+xml", GP_PIDF_XML1_Out1),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 1.3"),
    %% verify tuples
    ok = test_verify_tuples(GP_PIDF_XML1_Out1_Tuples, [GP_PIDF_XLM1_Tuple1, GP_PIDF_XLM1_Tuple2]),


    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 2.0"),
    %% add another presence document
    ok = set_pidf_for_user(GP_User1, SPFU_ETag2, 20, "application/pidf+xml", GP_PIDF_XML2, #event_ctx{}),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 2.1"),
    {ok, "application/pidf+xml", GP_PIDF_XML2_Out1} =
	get_pidf_xml_for_user(GP_User1, ["application/pidf+xml"]),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 2.2"),
    %% verify the merged entry
    {ok, #pidf_data{type   = pidf,
		    xml    = GP_PIDF_XML2_Out1,
		    data   = #pidf_doc{'PRESENTITY_URL' = "pres:user@example.org",
				       'PRESENCE_TUPLES' = GP_PIDF_XML2_Out1_Tuples
				      }
		   }} = parse_pidf_xml("application/pidf+xml", GP_PIDF_XML2_Out1),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 2.3"),
    %% verify tuples
    ok = test_verify_tuples(GP_PIDF_XML2_Out1_Tuples, [GP_PIDF_XLM1_Tuple1, GP_PIDF_XLM1_Tuple2,
							GP_PIDF_XLM2_Tuple1 ]),







    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 3.0"),
    %% add the same document again, to make sure we don't output duplicate tuples
    SPFU_ETag3 = "test-" ++ "-" ++ integer_to_list(Now) ++ "::" ++ integer_to_list(?LINE),
    ok = set_pidf_for_user(GP_User1, SPFU_ETag3, 20, "application/pidf+xml", GP_PIDF_XML2, #event_ctx{}),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 3.1"),
    {ok, "application/pidf+xml", GP_PIDF_XML3_Out1} =
	get_pidf_xml_for_user(GP_User1, ["application/pidf+xml"]),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 3.2"),
    %% verify the merged entry (should be the same as before this test)
    {ok, #pidf_data{type   = pidf,
		    xml    = GP_PIDF_XML3_Out1,
		    data   = #pidf_doc{'PRESENTITY_URL' = "pres:user@example.org",
				       'PRESENCE_TUPLES' = GP_PIDF_XML2_Out1_Tuples
				      }
		   }} = parse_pidf_xml("application/pidf+xml", GP_PIDF_XML3_Out1),






    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 10.1"),
    %% verify xpidf output
    {ok, "application/xpidf+xml", GP_PIDF_XML10_Out1} =
        get_pidf_xml_for_user(GP_User1, ["application/xpidf+xml"]),

    autotest:mark(?LINE, "get_pidf_xml_for_user/2 - 10.2"),
    %% we don't do very much checking of this old unsupported format
    true = (length(GP_PIDF_XML10_Out1) > 200),

    mnesia:abort(ok).

test_parse_xml(PIDF_XML1, PIDF_XML1_Tuple1) ->
    %% parse_xml(Type, XML)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "parse_pidf_xml/2 - 1.0"),
    ParseCT_Str = "application/pidf+xml",
    ParseCT1 = content_type(ParseCT_Str),
    {ok, #pidf_data{type = ParseCT1,
		    xml  = PIDF_XML1,
		    data = #pidf_doc{'PRESENTITY_URL' = "pres:user@example.org",
				     'PRESENCE_TUPLES' = ParseCT_Tuples1
				    }
		   }}
	= parse_pidf_xml(ParseCT_Str, PIDF_XML1),

    autotest:mark(?LINE, "parse_pidf_xml/2 - 1.1"),
    %% verify tuples
    test_verify_tuples([PIDF_XML1_Tuple1], ParseCT_Tuples1),


    autotest:mark(?LINE, "parse_pidf_xml/2 - 2.0"),
    %% test XML like the one produced by X-Lite 3.0

    PIDF_XML2_Tuple1 =
	"  <pr:tuple id=\"sd04cf079\">"
	"    <pr:status><pr:basic>open</pr:basic></pr:status>"
	"    <pr:note>Idle</pr:note>"
	"    <rpid:user-input last-input=\"2006-06-13T21:40:17Z\">idle</rpid:user-input>"
	"    <pr:timestamp>2006-06-13T21:40:17Z</pr:timestamp>"
	"  </pr:tuple>",

    PIDF_XML2_Tuple1_with_NS =
	"  <pr:tuple xmlns:pr=\"urn:ietf:params:xml:ns:pidf\" "
	"xmlns:caps=\"urn:ietf:params:xml:ns:pidf:caps\" "
	"xmlns:cipid=\"urn:ietf:params:xml:ns:pidf:cipid\" "
	"xmlns:counterpath=\"www.counterpath.com/presence/ext\" "
	"xmlns:dm=\"urn:ietf:params:xml:ns:pidf:data-model\" "
	"xmlns:rpid=\"urn:ietf:params:xml:ns:pidf:rpid\" id=\"sd04cf079\" >"
	"    <pr:status><pr:basic>open</pr:basic></pr:status>"
	"    <pr:note>Idle</pr:note>"
	"    <rpid:user-input last-input=\"2006-06-13T21:40:17Z\">idle</rpid:user-input>"
	"    <pr:timestamp>2006-06-13T21:40:17Z</pr:timestamp>"
	"  </pr:tuple>",

    PIDF_XML2 =
	"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>"
	"<pr:presence xmlns:pr=\"urn:ietf:params:xml:ns:pidf\" entity=\"sip:ft@example.net\""
	"	xmlns:caps=\"urn:ietf:params:xml:ns:pidf:caps\""
	"	xmlns:cipid=\"urn:ietf:params:xml:ns:pidf:cipid\""
	"	xmlns:counterpath=\"www.counterpath.com/presence/ext\""
	"	xmlns:dm=\"urn:ietf:params:xml:ns:pidf:data-model\""
	"	xmlns:rpid=\"urn:ietf:params:xml:ns:pidf:rpid\">" ++
	PIDF_XML2_Tuple1 ++
	"  <dm:person id=\"p8652f666\">"
	"    <dm:note>Idle</dm:note>"
	"  </dm:person>"
	"</pr:presence>",

    autotest:mark(?LINE, "parse_pidf_xml/2 - 2.1"),
    {ok, #pidf_data{type = ParseCT1,
		    xml  = PIDF_XML2,
		    data = #pidf_doc{'PRESENTITY_URL' = "pres:ft@example.net",
				     'PRESENCE_TUPLES' = ParseCT_Tuples2
				    }
		   }}
	= parse_pidf_xml(ParseCT_Str, PIDF_XML2),

    autotest:mark(?LINE, "parse_pidf_xml/2 - 2.2"),
    %% verify tuples
    test_verify_tuples([PIDF_XML2_Tuple1_with_NS], ParseCT_Tuples2),

    {ok, ParseCT_Tuples1}.


test_verify_tuples(L1, L2) when length(L1) /= length(L2) ->
    Msg = io_lib:format("Wrong number of XML presence tuples, expected ~p got ~p", [length(L1), length(L2)]),
    throw({error, lists:flatten(Msg)});
test_verify_tuples(L1, L2) ->
    test_verify_tuples2(L1, L2, 1).

test_verify_tuples2([H1 | T1], [H2 | T2], Pos) ->
    {X1, []} = xmerl_scan:string(H1),
    {X2, []} = xmerl_scan:string(H2),
    XC1 = lists:flatten( xmerl:export_simple_content([X1], presence_xmerl_xml) ),
    XC2 = lists:flatten( xmerl:export_simple_content([X2], presence_xmerl_xml) ),
    case (XC1 == XC2) of
	true ->
	    test_verify_tuples2(T1, T2, Pos + 1);
	false ->
	    Msg = io_lib:format("XML presence tuple #~p mismatch~n"
				"Got :~n"
				"~s~n"
				"Expected :~n"
				"~s~n",
				[Pos, XC2, XC1]),
	    throw({error, lists:flatten(Msg)})
    end;
test_verify_tuples2([], [], _Pos) ->
    ok.
