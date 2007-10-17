%%%-------------------------------------------------------------------
%%% File    : sipdialog.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Dialog related functions. Half is for the management of
%%%           the dialog ETS table (yxa_dialogs), and half is helper
%%%           functions for applications making use of dialogs
%%%           (through 'dialog' records).
%%%
%%% @since     7 Feb 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(sipdialog).

-export([
	 init/0,

	 %% dialog ETS table manipulators
	 register_dialog_controller/2,
	 register_dialog_controller/3,
	 register_dialog_controller/4,
	 get_dialog_controller/1,
	 unregister_dialog_controller/1,
	 delete_using_pid/1,
	 delete_dialog_controller/3,
	 handle_expired_dialogs/1,
	 set_dialog_expires/2,
	 set_dialog_expires/4,

	 %% dialog record users helper functions
	 create_dialog_state_uac/2,
	 create_dialog_state_uas/2,
	 create_dialog_state_uas/3,

	 update_dialog_recv_request/2,

	 get_next_local_cseq/1,

	 generate_new_request/4,

	 dialog2str/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type dialogid() = #dialogid{}.
%%                    no description
-record(dialogid, {callid,		%% string()
		   local_tag,		%% string()
		   remote_tag		%% string() | undefined
		  }).

-record(dialog_attrs, {pid,		%% pid() of dialog controller
		       expires		%% integer(), second since epoch when this transaction expires
		      }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(DEFAULT_EXPIRE, 60).
-define(ETS_DIALOG_TABLE, yxa_dialogs).
-define(DEFAULT_FIRST_LOCAL_CSEQ, 1).


%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     Called when the YXA node starts up. Create any ETS tables
%%          we need.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    ets:new(?ETS_DIALOG_TABLE, [public, set, named_table]),
    ok.


%%--------------------------------------------------------------------
%% @spec    (Dialog, Pid) -> ok
%%
%%            Dialog = #dialog{}
%%            Pid    = pid() "dialog controller to be"
%%
%% @throws  {error, Reason} 
%%
%% @doc     Register a dialog controller for a dialog record.
%% @end
%%--------------------------------------------------------------------
register_dialog_controller(Dialog, Pid) when is_record(Dialog, dialog), is_pid(Pid) ->
    register_dialog_controller(Dialog#dialog.callid,
			       Dialog#dialog.local_tag,
			       Dialog#dialog.remote_tag,
			       Pid).

%%--------------------------------------------------------------------
%% @spec    (Dialog, Pid, Expire) -> ok
%%
%%            Dialog = #dialog{}
%%            Pid    = pid() "dialog controller to be"
%%            Expire = integer() "expire time in seconds"
%%
%% @throws  {error, Reason} 
%%
%% @doc     Register a dialog controller for a dialog record.
%% @end
%%--------------------------------------------------------------------
register_dialog_controller(Dialog, Pid, Expire) when is_record(Dialog, dialog), is_pid(Pid), is_integer(Expire) ->
    register_dialog_controller(Dialog#dialog.callid,
			       Dialog#dialog.local_tag,
			       Dialog#dialog.remote_tag,
			       Pid,
			       Expire);

%%--------------------------------------------------------------------
%% @spec    (CallId, LocalTag, Pid) -> ok
%%
%%            CallId   = string()
%%            LocalTag = string()
%%            Pid      = pid() "dialog controller to be"
%%
%% @throws  {error, Reason} 
%%
%% @doc     Register 'half a dialog'. A half dialog is one where we do
%%          not yet know the remote tag, like when we send out a
%%          request that, if answered, will establish one or more
%%          dialogs.
%% @end
%%--------------------------------------------------------------------
register_dialog_controller(CallId, LocalTag, Pid) when is_list(CallId), is_list(LocalTag), is_pid(Pid) ->
    register_dialog_controller(CallId, LocalTag, undefined, Pid).

%%--------------------------------------------------------------------
%% @spec    (CallId, LocalTag, Pid, Expire) -> ok
%%
%%            CallId   = string()
%%            LocalTag = string()
%%            Pid      = pid() "dialog controller to be"
%%            Expire   = integer() "expire time in seconds"
%%
%% @throws  {error, Reason} 
%%
%% @doc     Register 'half a dialog'. A half dialog is one where we do
%%          not yet know the remote tag, like when we send out a
%%          request that, if answered, will establish one or more
%%          dialogs.
%% @end
%%--------------------------------------------------------------------
register_dialog_controller(CallId, LocalTag, Pid, Expire) when is_list(CallId), is_list(LocalTag), is_pid(Pid),
							       is_integer(Expire) ->
    register_dialog_controller(CallId, LocalTag, undefined, Pid, Expire);

%%--------------------------------------------------------------------
%% @spec    (CallId, LocalTag, RemoteTag, Pid) -> ok
%%
%%            CallId    = string()
%%            LocalTag  = string()
%%            RemoteTag = string() | undefined
%%            Pid       = pid() "dialog controller to be"
%%            Expire    = integer() "expire time in seconds"
%%
%% @throws  {error, Reason} 
%%
%% @doc     Register a dialog ('half dialog' if RemoteTag is
%%          undefined).
%% @end
%%--------------------------------------------------------------------
register_dialog_controller(CallId, LocalTag, RemoteTag, Pid) when is_list(CallId), is_list(LocalTag), is_pid(Pid) ->
    register_dialog_controller(CallId, LocalTag, RemoteTag, Pid, ?DEFAULT_EXPIRE).

%%--------------------------------------------------------------------
%% @spec    (CallId, LocalTag, RemoteTag, Pid, Expire) -> ok
%%
%%            CallId    = string()
%%            LocalTag  = string()
%%            RemoteTag = string() | undefined
%%            Pid       = pid() "dialog controller to be"
%%            Expire    = integer() "expire time in seconds"
%%
%% @throws  {error, Reason} 
%%
%% @doc     Register a dialog ('half dialog' if RemoteTag is
%%          undefined).
%% @end
%%--------------------------------------------------------------------
register_dialog_controller(CallId, LocalTag, RemoteTag, Pid, Expire) when is_list(CallId), is_list(LocalTag),
									  is_pid(Pid), is_integer(Expire),
									  Expire >= 1 ->
    Id = #dialogid{callid     = CallId,
		   local_tag  = LocalTag,
		   remote_tag = RemoteTag
		  },

    case RemoteTag of
	undefined ->
	    logger:log(debug, "Sipdialog: Extra debug: Register mapping of half dialog id ~p to pid ~p", [Id, Pid]);
	_ ->
	    logger:log(debug, "Sipdialog: Extra debug: Register mapping of dialog id ~p to pid ~p", [Id, Pid])
    end,

    Attrs = #dialog_attrs{pid = Pid,
			  expires = util:timestamp() + Expire
			 },
    try
	begin
	    DS = whereis(dialog_server),
	    case DS of
		undefined ->
		    erlang:error("No dialog server found");
		_ ->
		    ok
	    end,
	    true = link(DS),
	    true = ets:insert_new(?ETS_DIALOG_TABLE, {Id, Attrs}),
	    true = ets:insert(?ETS_DIALOG_TABLE, {Pid, Id})
	end of
	true ->
	    ok
    catch
	X: Y ->
	    logger:log(debug, "Sipdialog: Failed registering dialog ~p : ~p ~p", [Id, X, Y]),
	    throw({error, "Could not register dialog"})
    end.


%%--------------------------------------------------------------------
%% @spec    (In) ->
%%            {ok, DCPid} |
%%            nomatch     |
%%            error
%%
%%            In = #request{} | #response{}
%%
%%            DCPid = pid() "dialog controller process"
%%
%% @doc     Find the dialog controller for a received request or
%%          response.
%% @end
%%--------------------------------------------------------------------
get_dialog_controller(Request) when is_record(Request, request) ->
    %% When looking for dialog matching a header from a request we received,
    %% the local and remote tags are reversed
    case sipheader:dialogid(Request#request.header) of
	{_CallId, _RemoteTag, none} ->
	    nomatch;
        {CallId, RemoteTag, LocalTag} ->
	    get_dialog_controller2(CallId, LocalTag, RemoteTag);
	_ ->
	    error
    end;
get_dialog_controller(Response) when is_record(Response, response) ->
    case sipheader:dialogid(Response#response.header) of
        {CallId, LocalTag, RemoteTag} ->
	    get_dialog_controller2(CallId, LocalTag, RemoteTag);
	_ ->
	    error
    end.

%% part of get_dialog_controller/1
%% Returns : {ok, Pid} | nomatch
get_dialog_controller2(CallId, LocalTag, RemoteTag) when is_list(CallId), is_list(LocalTag) ->
    Id = #dialogid{callid     = CallId,
		   local_tag  = LocalTag,
		   remote_tag = RemoteTag
		  },
    case ets:lookup(?ETS_DIALOG_TABLE, Id) of
	[{Id, #dialog_attrs{pid = Pid}}] ->
	    logger:log(debug, "Sipdialog: Extra debug: Found dialog controller ~p for dialog ~p",
		       [Pid, Id]),
	    {ok, Pid};
	_ ->
	    HalfId = Id#dialogid{remote_tag = undefined},
	    case ets:lookup(?ETS_DIALOG_TABLE, HalfId) of
		[{HalfId, #dialog_attrs{pid = Pid}}] ->
		    logger:log(debug, "Sipdialog: Extra debug: Found dialog controller ~p for half dialog ~p",
			       [Pid, Id]),
		    {ok, Pid};
		_ ->
		    logger:log(debug, "Sipdialog: Extra debug: Found NO dialog controller for dialog ~p",
			       [Id]),
		    nomatch
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (Dialog) -> ok
%%
%%            Dialog = #dialog{}
%%
%% @doc     Unregister a dialog. Really just a shortcut for
%%          delete_dialog_controller/3.
%% @end
%%--------------------------------------------------------------------
unregister_dialog_controller(Dialog) when is_record(Dialog, dialog) ->
    delete_dialog_controller(Dialog#dialog.callid,
			     Dialog#dialog.local_tag,
			     Dialog#dialog.remote_tag).


%%--------------------------------------------------------------------
%% @spec    (Pid) ->
%%            {ok, Id} |
%%            nomatch
%%
%%            Pid = pid()
%%
%%            Id = #dialogid{}
%%
%% @doc     Delete any records of dialogs controlled by Pid.
%% @end
%%--------------------------------------------------------------------
delete_using_pid(Pid) when is_pid(Pid) ->
    case ets:lookup(?ETS_DIALOG_TABLE, Pid) of
	[{Pid, Id}] when is_record(Id, dialogid) ->
	    HalfId = Id#dialogid{remote_tag = undefined},

	    true = ets:delete(?ETS_DIALOG_TABLE, Id),
	    true = ets:delete(?ETS_DIALOG_TABLE, HalfId),
	    true = ets:delete(?ETS_DIALOG_TABLE, Pid),
	    {ok, Id};
	_ ->
	    nomatch
    end.

%%--------------------------------------------------------------------
%% @spec    (CallId, LocalTag, RemoteTag) -> ok
%%
%%            Pid = pid()
%%
%% @doc     Delete a dialog from the ETS table. See also
%%          unregister_dialog_controller/1 above.
%% @end
%%--------------------------------------------------------------------
delete_dialog_controller(CallId, LocalTag, RemoteTag) when is_list(CallId), is_list(LocalTag),
							   is_list(RemoteTag); RemoteTag == undefined ->
    Id = #dialogid{callid     = CallId,
		   local_tag  = LocalTag,
		   remote_tag = RemoteTag
		  },
    case ets:lookup(?ETS_DIALOG_TABLE, Id) of
	[{Id, Attrs}] ->
	    true = ets:delete_object(?ETS_DIALOG_TABLE, {Id, Attrs}),
	    true = ets:delete_object(?ETS_DIALOG_TABLE, {Attrs#dialog_attrs.pid, Id});
	_ ->
	    ok
    end,
    ok.


%%--------------------------------------------------------------------
%% @spec    (Interval) -> ok
%%
%%            Interval = integer()
%%
%% @doc     Look for expired dialogs and signal their dialog contr-
%%          ollers that they are expired. If they don't exit until
%%          this function is called again (after Interval seconds),
%%          we kill them.
%% @end
%%--------------------------------------------------------------------
handle_expired_dialogs(Interval) when is_integer(Interval) ->
    handle_expired_dialogs2(Interval, ets:tab2list(?ETS_DIALOG_TABLE)).

%% part of handle_expired_dialogs/1, just to make things testable
handle_expired_dialogs2(Interval, Entrys) when is_integer(Interval), is_list(Entrys) ->
    Now = util:timestamp(),
    F = fun({Id, #dialog_attrs{expires = Expire, pid = DCPid}}) when is_record(Id, dialogid), Expire =< Now ->
		case (Now - Expire) of
		    Diff when Diff >= Interval ->
			%% ok, Expire + Interval has passed - kill the dialog controller
			logger:log(debug, "Sipdialog: Killing dialog controller ~p since dialog ~p has expired",
				   [DCPid, Id]),
			erlang:exit(DCPid, kill);
		    _Diff ->
			%% send first signal to expired dialog controller when 'soft' expired
			DialogId = {Id#dialogid.callid, Id#dialogid.local_tag, Id#dialogid.remote_tag},
			logger:log(debug, "Sipdialog: Notifying dialog controller ~p that dialog ~p has expired",
				   [DCPid, DialogId]),
			util:safe_signal("Sipdialog: ", DCPid, {dialog_expired, DialogId})
		end;
	   (_) ->
		%% ignore all other content than expired dialogs
		ok
	end,
    [F(X) || X <- Entrys],
    ok.

%%--------------------------------------------------------------------
%% @spec    (Dialog, Expires) -> ok | nomatch
%%
%%            Dialog  = #dialog{}
%%            Expires = integer() "seconds from now the dialog should expire"
%%
%% @doc     Set a new expiration time on an existing dialog.
%% @end
%%--------------------------------------------------------------------
set_dialog_expires(Dialog, Expires) when is_record(Dialog, dialog), is_integer(Expires) ->
    #dialog{callid     = CallId,
	    local_tag  = LocalTag,
	    remote_tag = RemoteTag
	   } = Dialog,

    set_dialog_expires(CallId, LocalTag, RemoteTag, Expires).

%%--------------------------------------------------------------------
%% @spec    (CallId, LocalTag, RemoteTag, Expires) -> ok | nomatch
%%
%%            CallId    = string()
%%            LocalTag  = string()
%%            RemoteTag = string() | undefined
%%            Expires   = integer() "seconds from now the dialog should expire"
%%
%% @doc     Set a new expiration time on an existing dialog.
%% @end
%%--------------------------------------------------------------------
set_dialog_expires(CallId, LocalTag, RemoteTag, Expires) when is_list(CallId), is_list(LocalTag),
							      is_list(RemoteTag); RemoteTag == undefined,
							      is_integer(Expires) ->
    Id = #dialogid{callid     = CallId,
		   local_tag  = LocalTag,
		   remote_tag = RemoteTag
		  },

    Now = util:timestamp(),
    NewExpires = Now + Expires,

    case ets:lookup(?ETS_DIALOG_TABLE, Id) of
	[{Id, DialogAttrs}] when is_record(DialogAttrs, dialog_attrs) ->
	    logger:log(debug, "Sipdialog: Setting new 'expires' on dialog ~p (expire in ~p seconds)",
		       [Id, Expires]),
	    true = ets:insert(?ETS_DIALOG_TABLE, {Id, DialogAttrs#dialog_attrs{expires = NewExpires}}),
	    ok;
	_ ->
	    HalfId = Id#dialogid{remote_tag = undefined},
	    case ets:lookup(?ETS_DIALOG_TABLE, HalfId) of
		[{HalfId, DialogAttrs}] ->
		    logger:log(debug, "Sipdialog: Setting new 'expires' on half dialog ~p (expire in ~p seconds)",
			       [HalfId, Expires]),
		    true = ets:insert(?ETS_DIALOG_TABLE, {HalfId, DialogAttrs#dialog_attrs{expires = NewExpires}}),
		    ok;
		_ ->
		    logger:log(debug, "Sipdialog: Extra debug: Found NO dialog controller for dialog ~p",
			       [Id]),
		    nomatch
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (Request, Response) ->
%%            {ok, Dialog}
%%
%%            Request  = #request{}
%%            Response = #response{}
%%
%%            Dialog = #dialog{}
%%
%% @doc     Call this when a UAC receives a response which establishes
%%          a dialog, if you want a dialog record() with all the
%%          information about the dialog filled in. NOTE : all the
%%          comments in the code below are quotes from RFC3261
%%          #12.1.2 (UAC Behavior).
%% @end
%%--------------------------------------------------------------------
create_dialog_state_uac(Request, Response) when is_record(Request, request), is_record(Response, response) ->
    Header = Request#request.header,
    RHeader = Response#response.header,

    %% If the request was sent over TLS, and the Request-URI contained a
    %% SIPS URI, the "secure" flag is set to TRUE.
    IsSecure =
	case (Request#request.uri)#sipurl.proto of
	    "sips" -> true;
	    _ -> false
	end,

    %% The route set MUST be set to the list of URIs in the Record-Route
    %% header field from the response, taken in reverse order and preserving
    %% all URI parameters.
    Route = lists:reverse(keylist:fetch('record-route', RHeader)),

    %% The remote target MUST be set to the URI from the Contact header field of the response
    RemoteTarget =
	case keylist:fetch('contact', RHeader) of
	    [] -> undefined;
	    [Contact1] -> Contact1
	end,

    %% The local sequence number MUST be set to the value of the sequence
    %% number in the CSeq header field of the request
    {CSeqStr, _Method} = sipheader:cseq(Header),
    LocalCSeq = list_to_integer(CSeqStr),

    %% The remote sequence number MUST be empty (it is established when the
    %% remote UA sends a request within the dialog)
    RemoteCSeq = undefined,

    %% The call identifier component of the dialog ID MUST be set to the value of
    %% the Call-ID in the request.
    CallId = sipheader:callid(Header),

    %% The local tag component of the dialog ID MUST be set to the tag in
    %% the From field in the request
    LocalTag =
	case sipheader:get_tag(keylist:fetch('from', Header)) of
	    none ->
		throw({error, "No From: tag in request"});
	    FromTag1 ->
		FromTag1
	end,

    %% the remote tag component of the dialog ID MUST be set to the tag in the To
    %% field of the response.
    RemoteTag =
	case sipheader:get_tag(keylist:fetch('to', RHeader)) of
	    none ->
		%% A UAC MUST be prepared to receive a response without a tag in the To
		%% field, in which case the tag is considered to have a value of null.
		undefined;
	    ToTag1 ->
		ToTag1
	end,

    %% The remote URI MUST be set to the URI in the To field, and the local
    %% URI MUST be set to the URI in the From field.
    {_, LocalURI} = sipheader:from(Header),
    {_, RemoteURI} = sipheader:to(Header),

    Dialog =
	#dialog{callid        = CallId,

		local_tag     = LocalTag,
		remote_tag    = RemoteTag,

		local_cseq    = LocalCSeq,
		remote_cseq   = RemoteCSeq,

		local_uri     = LocalURI,
		remote_uri    = RemoteURI,

		remote_target = RemoteTarget,
		secure        = IsSecure,
		route_set     = Route,
		state         = undefined
	       },

    {ok, Dialog}.


%%--------------------------------------------------------------------
%% @spec    (Request, Response) ->
%%            {ok, Dialog} 
%%
%%            Request  = #request{}
%%            Response = #response{}
%%
%%            Dialog = #dialog{}
%%            Reason = string()
%%
%% @throws  {error, Reason} 
%%
%% @doc     Call this when a local UAS prepares to answer a request
%%          and thereby establishes a dialog, if you want a dialog
%%          record() with all the information about the dialog filled
%%          in.
%% @end
%%--------------------------------------------------------------------
create_dialog_state_uas(Request, Response) when is_record(Request, request), is_record(Response, response) ->
    [ResponseContact] = keylist:fetch('contact', Response#response.header),
    ResponseToTag =
	case sipheader:get_tag(keylist:fetch('to', Response#response.header)) of
	    none ->
		throw({error, "No To: tag in response"});
	    ToTag1 ->
		ToTag1
	end,
    create_dialog_state_uas(Request, ResponseToTag, ResponseContact).

%%--------------------------------------------------------------------
%% @spec    (Request, ResponseToTag, ResponseContact) ->
%%            {ok, Dialog}
%%
%%            Request         = #request{}
%%            Response        = #response{}
%%            ResponseToTag   = string() "To-tag of server transcation"
%%            ResponseContact = string() "Contact that will be used"
%%
%%            Dialog = #dialog{}
%%
%% @doc     Call this when a local UAS prepares to answer a request
%%          and thereby establishes a dialog, if you want a dialog
%%          record() with all the information about the dialog filled
%%          in. NOTE : all the comments in the code below are quotes
%%          from RFC3261 #12.1.1 (UAS behavior).
%% @end
%%--------------------------------------------------------------------
create_dialog_state_uas(Request, ResponseToTag, ResponseContact)
  when is_record(Request, request), is_list(ResponseToTag), is_list(ResponseContact) ->
    Header = Request#request.header,

    %% If the request arrived over TLS, and the Request-URI contained a SIPS
    %% URI, the "secure" flag is set to TRUE.
    Secure = create_dialog_state_uas_is_secure(Request, [ResponseContact]),

    %% The route set MUST be set to the list of URIs in the Record-Route
    %% header field from the request, taken in order and preserving all URI
    %% parameters.
    Route = keylist:fetch('record-route', Header),

    %% The remote target MUST be set to the URI from the Contact header field of the request.
    RemoteTarget =
	case keylist:fetch('contact', Header) of
	    [] -> throw({error, "Dialog creating requests must contain Contact header"});
	    [Contact1] -> Contact1
	end,

    %% The remote sequence number MUST be set to the value of the sequence
    %% number in the CSeq header field of the request
    {RemoteCSeqStr, _Method} = sipheader:cseq(Header),
    RemoteCSeq = list_to_integer(RemoteCSeqStr),

    %% The local sequence number MUST be empty.
    LocalCSeq = undefined,

    %% The call identifier component of the dialog ID MUST be set to the value of
    %% the Call-ID in the request.
    CallId = sipheader:callid(Header),

    %% The local tag component of the dialog ID MUST be set to the tag in the To field
    %% in the response to the request (which always includes a tag)
    LocalTag = ResponseToTag,

    %% the remote tag component of the dialog ID MUST be set to the tag from the
    %% From field in the request
    From = keylist:fetch('from', Header),
    RemoteTag =
	case sipheader:get_tag(From) of
	    none ->
		%% A UAS MUST be prepared to receive a request without a tag in the
		%% From field, in which case the tag is considered to have a value of null.
		undefined;
	    FromTag1 ->
		FromTag1
	end,

    %% The remote URI MUST be set to the URI in the From field, and the local
    %% URI MUST be set to the URI in the To field.
    {_, RemoteURI} = sipheader:from(Header),
    {_, LocalURI} = sipheader:to(Header),

    Dialog =
	#dialog{callid        = CallId,

		local_tag     = LocalTag,
		remote_tag    = RemoteTag,

		local_cseq    = LocalCSeq,
		remote_cseq   = RemoteCSeq,

		local_uri     = LocalURI,
		remote_uri    = RemoteURI,

		remote_target = RemoteTarget,
		secure        = Secure,
		route_set     = Route,
		state         = undefined,	%% XXX figure out dialog state here?

		remote_uri_str	= From		%% In the name of interoperability
	       },

    {ok, Dialog}.


%%--------------------------------------------------------------------
%% @spec    (Request, ResponseContact) ->
%%            true | false 
%%
%%            Request         = #request{}
%%            ResponseContact = [string()]
%%
%%            Reason = response_contact_must_be_secure |
%%                     response_contact_missing
%%
%% @throws  {error, Reason} 
%%
%% @doc     Figure out if the 'secure' flag should be set on a dialog,
%%          and assert if the current request+contact combination
%%          violates this rule.
%% @end
%%--------------------------------------------------------------------
create_dialog_state_uas_is_secure(Request, []) when is_record(Request, request) ->
    throw({error, response_contact_missing});
create_dialog_state_uas_is_secure(Request, ResponseContact) when is_record(Request, request),
								 is_list(ResponseContact) ->
    MustBeSecure =
	case is_sips_uri(Request#request.uri) of
	    true ->
		true;
	    false ->
		case keylist:fetch('record-route', Request#request.header) of
		    [] ->
			is_sips_uri(keylist:fetch('contact', Request#request.header));
		    [FirstRR | _] ->
			is_sips_uri([FirstRR])
		end
	end,
    ResponseContactIsSecure = is_sips_uri(ResponseContact),

    %% RFC3261 #12.1.1 (UAS behavior) :
    %% If the request that initiated the dialog contained a
    %% SIPS URI in the Request-URI or in the top Record-Route header field
    %% value, if there was any, or the Contact header field if there was no
    %% Record-Route header field, the Contact header field in the response
    %% MUST be a SIPS URI.
    if
	MustBeSecure == true, ResponseContactIsSecure /= true ->
	    throw({error, response_contact_must_be_secure});
	true ->
	    true
    end,

    %% If the request arrived over TLS, and the Request-URI contained a SIPS
    %% URI, the "secure" flag is set to TRUE.
    is_sips_uri(Request#request.uri).

%% part of create_dialog_state_uas_is_secure/2
%% Returns : true | false
is_sips_uri(URI) when is_record(URI, sipurl) ->
    (URI#sipurl.proto == "sips");
is_sips_uri([Str]) when is_list(Str) ->
    case contact:parse([Str]) of
	[C] when is_record(C, contact) ->
	    is_sips_uri(sipurl:parse(C#contact.urlstr));
	_ ->
	    throw({error, "Unparsable Contact/Record-Route/Request-URI when creating dialog state"})
    end.


%%--------------------------------------------------------------------
%% @spec    (Request, Dialog) ->
%%            {ok, NewDialog} |
%%            {error, old_cseq}
%%
%%            Request = #request{}
%%            Dialog  = #dialog{}
%%
%%            NewDialog = #dialog{}
%%
%% @doc     Update a dialog when a new request was received on the
%%          dialog.
%% @end
%%--------------------------------------------------------------------
update_dialog_recv_request(Request, Dialog) when is_record(Request, request), is_record(Dialog, dialog) ->
    {CSeqStr, _} = sipheader:cseq(Request#request.header),
    CSeq = list_to_integer(CSeqStr),
    case Dialog#dialog.remote_cseq of
	undefined ->
	    {ok, Dialog#dialog{remote_cseq = CSeq}};
	OldCSeq when CSeq > OldCSeq ->
	    %% "It is possible for the CSeq sequence number to be higher than the remote sequence
	    %% number by more than one.  This is not an error condition, and a UAS SHOULD be
	    %% prepared to receive and process requests with CSeq values more than one higher than
	    %% the previous received request."
	    {ok, Dialog#dialog{remote_cseq = CSeq}};
	_ ->
	    {error, old_cseq}
    end.


%%--------------------------------------------------------------------
%% @spec    (Method, ExtraHeaders, Body, Dialog) ->
%%            {ok, Request, NewDialog, DstList}
%%
%%            Method       = string() "SIP method"
%%            ExtraHeaders = [{Key, Value}] "extra headers to include in response"
%%            Body         = binary() | list() "request body"
%%            Dialog       = #dialog{}
%%
%%            Request   = #request{}
%%            NewDialog = #dialog{}
%%            DstList   = [#sipdst{}]
%%
%% @doc     Generate a new request based on Method, supplied Extra-
%%          Headers, body and dialog info found in Dialog.
%% @end
%%--------------------------------------------------------------------
generate_new_request(Method, ExtraHeaders, Body, Dialog) when is_list(Method), is_list(ExtraHeaders),
							      is_binary(Body); is_list(Body),
							      is_record(Dialog, dialog) ->
    {CSeqL, NewDialog} =
	case lists:keysearch("CSeq", 1, ExtraHeaders) of
	    {value, _} ->
		%% CSeq tuple present in ExtraHeaders, don't add another one
		{[], Dialog};
	    false ->
		{ok, CSeqNum, CSeqDialog} = get_next_local_cseq(Dialog),
		CSeqVal = lists:concat([CSeqNum, " ", Method]),
		{[{"CSeq", [CSeqVal]}], CSeqDialog}
	end,

    CallId = NewDialog#dialog.callid,

    [C] = contact:parse([NewDialog#dialog.remote_target]),
    TargetURI = sipurl:parse(C#contact.urlstr),
    From = set_tag(NewDialog#dialog.local_tag, NewDialog#dialog.local_uri),
    To =
	case NewDialog#dialog.remote_uri_str of
	    undefined ->
		%% This really should be enough, but many clients expect To: byte-by-byte matching their From:
		set_tag(NewDialog#dialog.remote_tag, NewDialog#dialog.remote_uri);
	    [RemoteURI_str] when is_list(RemoteURI_str) ->
		RemoteURI_str
	end,

    Route =
	case NewDialog#dialog.route_set of
	    [] ->
		[];
	    RouteL1 when is_list(RouteL1) ->
		[{"Route", RouteL1}]
	end,

    Header = keylist:from_list([{"From",	[From]},
				{"To",		[To]},
				{"Call-Id",	[CallId]}
			       ] ++ CSeqL ++ Route ++ ExtraHeaders),

    Request1 = #request{method = Method,
			uri    = TargetURI,
			header = Header
		       },
    Request = siprequest:set_request_body(Request1, Body),

    ApproxMsgSize = siprequest:get_approximate_msgsize(Request),

    DstList =
	case NewDialog#dialog.route_set of
	    [] ->
		sipdst:url_to_dstlist(TargetURI, ApproxMsgSize, TargetURI);
	    [FirstRoute | _] ->
		[FRC] = contact:parse([FirstRoute]),
		FRURL = sipurl:parse(FRC#contact.urlstr),
		sipdst:url_to_dstlist(FRURL, ApproxMsgSize, TargetURI)
	end,

    {ok, Request, NewDialog, DstList}.


%%--------------------------------------------------------------------
%% @spec    (ToTag, {DisplayName, ToURI}) ->
%%            NewTo
%%
%%            ToTag       = string()
%%            DisplayName = string() | none
%%            ToURI       = #sipurl{}
%%
%%            NewTo = string()
%%
%% @doc     Set tag on a parsed To: header.
%% @end
%%--------------------------------------------------------------------
set_tag(ToTag, {DisplayName, ToURI}) when is_list(ToTag) ->
    [NewTo] = sipheader:contact_print(
                [ contact:new(DisplayName, ToURI, [{"tag", ToTag}]) ]),
    NewTo;

set_tag(ToTag, URI) when is_list(ToTag), is_record(URI, sipurl) ->
    [NewURL] = sipheader:contact_print(
                [ contact:new(none, URI, [{"tag", ToTag}]) ]),
    NewURL.


%%--------------------------------------------------------------------
%% @spec    (Dialog) ->
%%            DialogString
%%
%%            Dialog = #dialog{}
%%
%%            DialogString = string()
%%
%% @doc     Format a dialog for debug logging.
%% @end
%%--------------------------------------------------------------------
dialog2str(Dialog) when is_record(Dialog, dialog) ->
    lists:flatten(["#dialog{\n",
		   dialog2str2(record_info(fields, dialog), tl(tuple_to_list(Dialog)), []),
		   "}"
		  ]).

%% part of dialog2str/1
dialog2str2([H1], [H2], Res) ->
    This = io_lib:format("        ~p = ~p~n", [H1, H2]),
    lists:reverse([This | Res]);
dialog2str2([H1 | T1], [H2 | T2], Res) ->
    This = io_lib:format("        ~p = ~p,~n", [H1, H2]),
    dialog2str2(T1, T2, [This | Res]).


%%--------------------------------------------------------------------
%% @spec    (Dialog) ->
%%            {ok, NextCSeq, NewDialog}
%%
%%            Dialog = #dialog{}
%%
%%            NewDialog = #dialog{}
%%            NextCSeq  = integer()
%%
%% @doc     Get the next local CSeq number for a dialog.
%% @end
%%--------------------------------------------------------------------
get_next_local_cseq(Dialog) when is_record(Dialog, dialog) ->
    Num =
	case Dialog#dialog.local_cseq of
	    undefined ->
		?DEFAULT_FIRST_LOCAL_CSEQ;
	    N when is_integer(N) ->
		N + 1
	end,
    {ok, Num, Dialog#dialog{local_cseq = Num}}.



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
    Now = util:timestamp(),

    %% handle_expired_dialogs2(Interval, Entrys)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "handle_expired_dialogs2/2 - 1.1"),
    ok = handle_expired_dialogs2(60, [test_make_dialog_e(foo, self(), Now + 50)
				     ]),
    autotest:mark(?LINE, "handle_expired_dialogs2/2 - 1.2"),
    %% check that we didn't get an 'dialog_expired' signal
    receive
	{dialog_expired, {foo, _}} ->
	    throw({error, "Non-expired dialog received 'dialog_expired' signal"});
	M1 ->
	    throw({error, {test_unknown_signal, M1}})
    after 0 ->
	    ok
    end,


    autotest:mark(?LINE, "handle_expired_dialogs2/2 - 2.0"),
    DExpParent = self(),
    DExpPid2 = spawn(fun() ->
			     test_dialog_expire_process(DExpParent)
		     end),

    ok = handle_expired_dialogs2(60, [test_make_dialog_e(1, self(), Now + 50),
				      test_make_dialog_e(2, self(), Now - 10),
				      test_make_dialog_e(3, DExpPid2, Now - 70)
				     ]),
    autotest:mark(?LINE, "handle_expired_dialogs2/2 - 2.1"),
    %% check that we got an 'dialog_expired' signal for dialog 2, but not dialog 1 or 3
    receive
	{dialog_expired, {2, _, _}} ->
	    ok;
	M2 ->
	    throw({error, {test_unknown_signal, M2}})
    after 0 ->
	    throw({error, "expired dialog #2 did not receive a 'soft' signal"})
    end,

    autotest:mark(?LINE, "handle_expired_dialogs2/2 - 2.2"),
    %% check that we got an 'dialog_expired' signal for dialog 2, but not dialog 1 or 3
    receive
	{dialog_expired, {N, _}} ->
	    Msg = io_lib:format("received unexpected soft-expire signal about dialog '~p'", [N]),
	    throw({error, lists:flatten(Msg)});
	M3 ->
	    throw({error, {test_unknown_signal, M3}})
    after 0 ->
	    ok
    end,

    autotest:mark(?LINE, "handle_expired_dialogs2/2 - 2.3"),
    %% check that the process handling dialog 3 got killed
    false = is_process_alive(DExpPid2),


    %% create_dialog_state_uac(Request, Response)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "create_dialog_state_uac/2 - 1.0"),
    CDS_UAC_Req1Msg =
	"TEST sip:ft@example.net SIP/2.0\r\n"
	"Via: SIP/2.0/UDP p1.example.net:5060;branch=z9hG4bK-test\r\n"
	"From: <sip:ft@f.example.net>;tag=yxa-testfromtag\r\n"
	"To: <sip:ft@t.example.net>\r\n"
	"Call-Id: 1140081219-380326@foo\r\n"
	"CSeq: 2 TEST\r\n"
	"\r\n",
    CDS_UAC_Req1 = sippacket:parse(CDS_UAC_Req1Msg, none),

    CDS_UAC_Res1Msg =
	"SIP/2.0 200 Ok\r\n"
	"Via: SIP/2.0/TCP p1.example.net:5060;branch=z9hG4bK-test\r\n"
	"From: <sip:ft@f.example.net>;tag=yxa-testfromtag\r\n"
	"To: <sip:ft@t.example.net>;tag=yxa-testtotag\r\n"
	"Call-Id: 1140081219-380326@foo\r\n"
	"CSeq: 2 TEST\r\n"
	"Contact: <sip:0.157.0@example.net>\r\n"
	"\r\n",
    CDS_UAC_Res1 = sippacket:parse(CDS_UAC_Res1Msg, none),

    CDC_UAC_Dialog1 =
	#dialog{callid        = "1140081219-380326@foo",
		local_cseq    = 2,
		remote_cseq   = undefined,
		local_tag     = "yxa-testfromtag",
		remote_tag    = "yxa-testtotag",
		secure        = false,
		route_set     = [],
		local_uri     = sipurl:parse("sip:ft@f.example.net"),
		remote_uri    = sipurl:parse("sip:ft@t.example.net"),
		remote_target = "<sip:0.157.0@example.net>",
		state         = undefined
	       },

    autotest:mark(?LINE, "create_dialog_state_uac/2 - 1.1"),
    {ok, CDC_UAC_Dialog1} = create_dialog_state_uac(CDS_UAC_Req1, CDS_UAC_Res1),

    autotest:mark(?LINE, "create_dialog_state_uac/2 - 2.0"),
    CDS_UAC_Req2Msg =
	"TEST sips:ft@example.net SIP/2.0\r\n"
	"Via: SIP/2.0/TLS p1.example.net:5060;branch=z9hG4bK-test\r\n"
	"From: <sip:ft@f.example.net>;tag=yxa-testfromtag\r\n"
	"To: <sip:ft@t.example.net>\r\n"
	"Call-Id: 1140081219-380326@foo\r\n"
	"CSeq: 2 TEST\r\n"
	"\r\n",
    CDS_UAC_Req2 = sippacket:parse(CDS_UAC_Req2Msg, none),

    CDS_UAC_Res2Msg =
	"SIP/2.0 200 Ok\r\n"
	"Via: SIP/2.0/TCP p1.example.net:5060;branch=z9hG4bK-test\r\n"
	"From: <sip:ft@f.example.net>;tag=yxa-testfromtag\r\n"
	"To: <sip:ft@t.example.net>;tag=yxa-testtotag\r\n"
	"Call-Id: 1140081219-380326@foo\r\n"
	"CSeq: 2 TEST\r\n"
        "Record-Route: <sips:p1.example.net>;lr\r\n"
	"\r\n",
    CDS_UAC_Res2 = sippacket:parse(CDS_UAC_Res2Msg, none),

    CDC_UAC_Dialog2 =
	CDC_UAC_Dialog1#dialog{secure        = true,
			       route_set     = ["<sips:p1.example.net>;lr"],
			       remote_target = undefined
			      },

    autotest:mark(?LINE, "create_dialog_state_uac/2 - 2.1"),
    {ok, CDC_UAC_Dialog2} = create_dialog_state_uac(CDS_UAC_Req2, CDS_UAC_Res2),

    autotest:mark(?LINE, "create_dialog_state_uac/2 - 3"),
    %% test without From-tag in request
    CDS_UAC_Req3 = CDS_UAC_Req1#request{header = keylist:set("From", ["<sip:ft@f.example.net>;no-tag"],
							     CDS_UAC_Req1#request.header)},
    {error, "No From: tag in request"} = (catch create_dialog_state_uac(CDS_UAC_Req3, CDS_UAC_Res2)),

    autotest:mark(?LINE, "create_dialog_state_uac/2 - 4"),
    %% test without To-tag in response
    CDS_UAC_Res4 = CDS_UAC_Res1#response{header = keylist:set("To", ["<sip:ft@t.example.net>;no-tag"],
							      CDS_UAC_Res1#response.header)},
    CDC_UAC_Dialog4 = CDC_UAC_Dialog1#dialog{remote_tag = undefined},
    {ok, CDC_UAC_Dialog4} = create_dialog_state_uac(CDS_UAC_Req1, CDS_UAC_Res4),


    %% create_dialog_state_uas(Request, Response)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "create_dialog_state_uas/2 - 1.0"),
    CDS_UAS_Req1Msg =
	"TEST sips:ft@example.net SIP/2.0\r\n"
	"Via: SIP/2.0/UDP p1.example.net:5060;branch=z9hG4bK-test\r\n"
	"From: <sip:ft@f.example.net>;tag=yxa-testfromtag\r\n"
	"To: <sip:ft@t.example.net>;tag=yxa-testtotag\r\n"
	"Call-Id: 1140081219-380326@foo\r\n"
	"CSeq: 2 TEST\r\n"
	"Contact: <sips:contact@f.example.net>\r\n"
	"\r\n",
    CDS_UAS_Req1 = sippacket:parse(CDS_UAS_Req1Msg, none),

    CDS_UAS_Res1Msg =
	"SIP/2.0 200 Ok\r\n"
	"Via: SIP/2.0/TCP p1.example.net:5060;branch=z9hG4bK-test\r\n"
	"From: <sip:ft@f.example.net>;tag=yxa-testfromtag\r\n"
	"To: <sip:ft@t.example.net>;tag=yxa-testtotag\r\n"
	"Call-Id: 1140081219-380326@foo\r\n"
	"CSeq: 2 TEST\r\n"
	"Contact: <sips:0.157.0@example.net>\r\n"
	"\r\n",
    CDS_UAS_Res1 = sippacket:parse(CDS_UAS_Res1Msg, none),

    CDC_UAS_Dialog1 =
	#dialog{callid         = "1140081219-380326@foo",
		local_cseq     = undefined,
		remote_cseq    = 2,
		local_tag      = "yxa-testtotag",
		remote_tag     = "yxa-testfromtag",
		secure         = true,
		route_set      = [],
		local_uri      = sipurl:parse("sip:ft@t.example.net"),
		remote_uri     = sipurl:parse("sip:ft@f.example.net"),
		remote_target  = "<sips:contact@f.example.net>",
		state          = undefined,
		remote_uri_str = ["<sip:ft@f.example.net>;tag=yxa-testfromtag"]
	       },

    autotest:mark(?LINE, "create_dialog_state_uas/2 - 1.1"),
    {ok, CDC_UAS_Dialog1} = create_dialog_state_uas(CDS_UAS_Req1, CDS_UAS_Res1),

    autotest:mark(?LINE, "create_dialog_state_uas/2 - 2"),
    %% test without To-tag in request
    CDS_UAS_Res2 = CDS_UAS_Res1#response{header = keylist:set("To", ["<sip:ft@t.example.net>;no-to-tag"],
							      CDS_UAS_Res1#response.header)},
    {error, "No To: tag in response"} = (catch create_dialog_state_uas(CDS_UAS_Req1, CDS_UAS_Res2)),

    autotest:mark(?LINE, "create_dialog_state_uas/2 - 3"),
    %% test without Contact in request
    CDS_UAS_Req3 = CDS_UAS_Req1#request{header = keylist:delete('contact', CDS_UAS_Req1#request.header)},
    {error, "Dialog creating requests must contain Contact header"} =
	(catch create_dialog_state_uas(CDS_UAS_Req3, CDS_UAS_Res1)),

    autotest:mark(?LINE, "create_dialog_state_uas/2 - 4"),
    %% test without From-tag in request
    CDS_UAS_Req4 = CDS_UAS_Req1#request{header = keylist:set("From", ["<sip:ft@f.example.net>;no-tag"],
							     CDS_UAS_Req1#request.header)},
    CDC_UAS_Dialog4 = CDC_UAS_Dialog1#dialog{remote_tag = undefined,
					     remote_uri_str = ["<sip:ft@f.example.net>;no-tag"]
					    },
    {ok, CDC_UAS_Dialog4} = create_dialog_state_uas(CDS_UAS_Req4, CDS_UAS_Res1),


    %% create_dialog_state_uas_is_secure(Request, ResponseContact)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "create_dialog_state_uas_is_secure/2 - 0"),
    IsSecReq1_Msg =
	"TEST sip:ft@example.net SIP/2.0\r\n"
	"Contact: <sips:contact@f.example.net>\r\n"
	"\r\n",
    IsSecReq1 = sippacket:parse(IsSecReq1_Msg, none),

    autotest:mark(?LINE, "create_dialog_state_uas_is_secure/2 - 1"),
    {error, response_contact_missing} = (catch create_dialog_state_uas_is_secure(IsSecReq1, [])),

    autotest:mark(?LINE, "create_dialog_state_uas_is_secure/2 - 2"),
    {error, response_contact_must_be_secure} =
	(catch create_dialog_state_uas_is_secure(IsSecReq1, ["<sip:unsecure@example.com>"])),

    autotest:mark(?LINE, "create_dialog_state_uas_is_secure/2 - 3"),
    %% test with SIPS URI
    IsSecReq3 = IsSecReq1#request{uri = sipurl:parse("sips:ft@example.net")},
    {error, response_contact_must_be_secure} =
	(catch create_dialog_state_uas_is_secure(IsSecReq3, ["<sip:unsecure@example.com>"])),

    autotest:mark(?LINE, "create_dialog_state_uas_is_secure/2 - 4"),
    %% test with non-SIPS URI and Record-Route, but SIPS Contact
    IsSecReq4 = IsSecReq1#request{
		  header = keylist:from_list([{"Record-Route", ["<sip:p1.example.net>"]},
					      {"Contact",      ["<sips:ua.example.net>"]}
					     ])
		 },
    false = create_dialog_state_uas_is_secure(IsSecReq4, ["<sip:unsecure@example.com>"]),

    autotest:mark(?LINE, "create_dialog_state_uas_is_secure/2 - 5"),
    %% test valid case, SIPS URI and SIPS RsponseContact
    IsSecReq5 = IsSecReq1#request{uri = sipurl:parse("sips:ft@example.net")},
    true = create_dialog_state_uas_is_secure(IsSecReq5, ["<sips:secure@example.com>"]),


    %% update_dialog_recv_request(Request, Dialog)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "update_dialog_recv_request/2 - 1"),
    {ok, #dialog{remote_cseq = 4}} =
	update_dialog_recv_request(#request{header = keylist:from_list([{"CSeq", ["4 TEST"]}])},
				   #dialog{remote_cseq = undefined}
				  ),

    autotest:mark(?LINE, "update_dialog_recv_request/2 - 2"),
    {ok, #dialog{remote_cseq = 5}} =
	update_dialog_recv_request(#request{header = keylist:from_list([{"CSeq", ["5 TEST"]}])},
				   #dialog{remote_cseq = 4}
				  ),

    autotest:mark(?LINE, "update_dialog_recv_request/2 - 3"),
    {error, old_cseq} =
	update_dialog_recv_request(#request{header = keylist:from_list([{"CSeq", ["3 TEST"]}])},
				   #dialog{remote_cseq = 4}
				  ),

    autotest:mark(?LINE, "update_dialog_recv_request/2 - 4"),
    {error, old_cseq} =
	update_dialog_recv_request(#request{header = keylist:from_list([{"CSeq", ["4 TEST"]}])},
				   #dialog{remote_cseq = 4}
				  ),

    %% dialog2str(Dialog)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "dialog2str/1 - 1"),
    true = is_list(dialog2str(#dialog{})),

    %% get_next_local_cseq(Dialog)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_next_local_cseq/1 - 1"),
    {ok, 1001, #dialog{local_cseq = 1001}} = get_next_local_cseq(#dialog{local_cseq = 1000}),

    %% generate_new_request(Method, ExtraHeaders, Body, Dialog)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "generate_new_request/4 - 1.0"),
    GNR_Dialog1_From = sipurl:parse("sip:ft@f.example.net"),
    GNR_Dialog1_To = sipurl:parse("sip:ft@t.example.net"),
    GNR_Dialog1_CallId = lists:concat([?LINE, "-", util:timestamp(), "@autotest.example.com"]),
    GNR_Dialog1 =
	#dialog{callid        = GNR_Dialog1_CallId,
		local_cseq    = 122,
		remote_cseq   = undefined,
		local_tag     = "yxa-testfromtag",
		remote_tag    = "yxa-testtotag",
		secure        = false,
		route_set     = [],
		local_uri     = GNR_Dialog1_From,
		remote_uri    = GNR_Dialog1_To,
		remote_target = "<sip:gnr-0@192.0.2.233>",
		state         = undefined
	       },

    autotest:mark(?LINE, "generate_new_request/4 - 1.1"),
    {ok, GNR_Request1, GNR_Dialog1_1, [GNR_Dst1 | _]} = generate_new_request("TEST", [], <<>>, GNR_Dialog1),

    autotest:mark(?LINE, "generate_new_request/4 - 1.2"),
    %% verify the request record
    GNR_Request1_URI = sipurl:parse("sip:gnr-0@192.0.2.233"),
    #request{method = "TEST", uri = GNR_Request1_URI, body = <<>>} = GNR_Request1,

    {none, GNR_Dialog1_From} = sipheader:from(GNR_Request1#request.header),
    {none, GNR_Dialog1_To} = sipheader:to(GNR_Request1#request.header),
    {"123", "TEST"} = sipheader:cseq(GNR_Request1#request.header),
    GNR_Dialog1_CallId = sipheader:callid(GNR_Request1#request.header),
    ["0"] = keylist:fetch('content-length', GNR_Request1#request.header),

    autotest:mark(?LINE, "generate_new_request/4 - 1.3"),
    %% verify dialog got updated like it should
    GNR_Dialog1_1 = GNR_Dialog1#dialog{local_cseq = 123},

    autotest:mark(?LINE, "generate_new_request/4 - 1.4"),
    %% verify dst-list
    "udp:192.0.2.233:5060 (sip:gnr-0@192.0.2.233)" = sipdst:dst2str(GNR_Dst1),

    autotest:mark(?LINE, "generate_new_request/4 - 2.0"),
    GNR_Dialog2_CallId = lists:concat([?LINE, "-", util:timestamp(), "@autotest.example.com"]),
    GNR_Dialog2 =
	GNR_Dialog1#dialog{callid         = GNR_Dialog2_CallId,
			   route_set      = ["<sip:192.0.2.111>"],
			   remote_uri_str = ["Test <" ++ sipurl:print(GNR_Dialog1_To) ++ ">"]
			  },

    autotest:mark(?LINE, "generate_new_request/4 - 2.1"),
    GNR_ExtraHeaders2 = [{"CSeq", ["122 ACK"]}],
    {ok, GNR_Request2, GNR_Dialog2_1, [GNR_Dst2 | _]} =
	generate_new_request("ACK", GNR_ExtraHeaders2, <<"test">>, GNR_Dialog2),

    autotest:mark(?LINE, "generate_new_request/4 - 2.2"),
    %% verify the request record
    #request{method = "ACK", uri = GNR_Request1_URI, body = <<"test">>} = GNR_Request2,

    {none, GNR_Dialog1_From} = sipheader:from(GNR_Request2#request.header),
    {"Test", GNR_Dialog1_To} = sipheader:to(GNR_Request2#request.header),
    {"122", "ACK"} = sipheader:cseq(GNR_Request2#request.header),
    GNR_Dialog2_CallId = sipheader:callid(GNR_Request2#request.header),
    ["4"] = keylist:fetch('content-length', GNR_Request2#request.header),

    autotest:mark(?LINE, "generate_new_request/4 - 2.3"),
    %% verify dialog did NOT get updated since we provided the CSeq in ExtraHeaders
    GNR_Dialog2_1 = GNR_Dialog2,

    autotest:mark(?LINE, "generate_new_request/4 - 2.4"),
    %% verify dst-list (this dialog has a route set)
    "udp:192.0.2.111:5060 (sip:gnr-0@192.0.2.233)" = sipdst:dst2str(GNR_Dst2),

    ok.


test_make_dialog_e(Id, Pid, Expires) ->
    {#dialogid{callid = Id},
     #dialog_attrs{expires = Expires,
		   pid     = Pid
		  }
    }.

test_dialog_expire_process(Parent) ->
    receive
	{quit, Parent} ->
	    ok;
	Msg ->
	    Parent ! {self(), Msg},
	    test_dialog_expire_process(Parent)
    end.
