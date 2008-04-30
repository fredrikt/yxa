%%%-------------------------------------------------------------------
%%% File    : uaprofile_package.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Config framework (based on Snom phones and
%%%           draft-ietf-sipping-config-framework-08.txt)
%%%
%%% @since    18 Aug 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(uaprofile_package).

-behaviour(event_package).

%%--------------------------------------------------------------------
%%% Standard YXA Event package exports
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/4,
	 is_allowed_subscribe/7,
	 notify_content/4,
	 package_parameters/2,
	 subscription_behaviour/3
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("event.hrl").
-include_lib("kernel/include/file.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(my_state, {filename,	%% string(), the filename we monitor
		   size,	%% integer(), file size
		   mtime,	%% integer(), file modification time
		   content_md5  %% string(), content MD5 sum
		  }).


%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

%% @type event_pkg() = string().
%%           Event package string - "ua-profile" | "sip-config" for this package.

%%====================================================================
%% Behaviour functions
%% Standard YXA Event package callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () ->
%%            none | {append, SupSpec}
%%
%%            SupSpec = term() "OTP supervisor child specification. Extra processes this event package want the sipserver_sup to start and maintain."
%%
%% @doc     YXA event packages export an init/0 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    none.

%%--------------------------------------------------------------------
%% @spec    (Package, Request, YxaCtx, Ctx) ->
%%            void() "but return 'ok' or {error, Reason} for now"
%%
%%            Package = event_pkg()
%%            Request = #request{} "the SUBSCRIBE request"
%%            YxaCtx  = #yxa_ctx{}
%%            Ctx     = #event_ctx{} "context information for request."
%%
%% @doc     YXA event packages must export a request/6 function. See
%%          the eventserver.erl module description for more
%%          information about when this function is invoked.
%% @end
%%--------------------------------------------------------------------
request(Package, _Request, YxaCtx, #event_ctx{sipuser = undefined})
  when Package == "ua-profile"; Package == "sip-config" ->
    logger:log(debug, "~s: ua-profile event package: Requesting authorization (only local users allowed)",
	       [YxaCtx#yxa_ctx.app_logtag]),
    {error, need_auth};

request(Package, _Request, YxaCtx, _Ctx) when Package == "ua-profile"; Package == "sip-config" ->
    #yxa_ctx{app_logtag   = LogTag,
	     thandler = THandler
	    } = YxaCtx,
    logger:log(normal, "~s: ua-profile event package: ~s -> '501 Not Implemented'",
	       [LogTag, YxaCtx#yxa_ctx.logstr]),
    transactionlayer:send_response_handler(THandler, 501, "Not Implemented"),
    ok.


%%--------------------------------------------------------------------
%% @spec    (Package, Num, Request, YxaCtx, SIPuser, Presentity,
%%          PkgState) ->
%%            {error, need_auth} |
%%            {ok, SubState, Status, Reason, ExtraHeaders, Body, NewPkgState}  |
%%            {siperror, Status, Reason, ExtraHeaders}
%%
%%            Package    = event_pkg()
%%            Num        = integer() "the number of subscribes we have received on this dialog, starts at 1"
%%            Request    = #request{} "the SUBSCRIBE request"
%%            YxaCtx     = #yxa_ctx{}
%%            SIPuser    = undefined | string() "undefined if request originator is not not authenticated, and string() if the user is authenticated (empty string if user could not be authenticated)"
%%            Presentity = undefined          |
%%                         {user, User}       |
%%                         {address, Address}
%%            PkgState   = undefined | #my_state{}
%%
%%            SubState     = active | pending
%%            Status       = integer() "SIP status code to respond with"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = [{Key, ValueList}] "headers to include in the response to the SUBSCRIBE"
%%            Body         = binary() | list()
%%            NewPkgState  = #my_state{}
%%
%% @doc     YXA event packages must export an is_allowed_subscribe/8
%%          function. This function is called when the event server
%%          receives a subscription request for this event package,
%%          and is the event packages chance to decide wether the
%%          subscription should be accepted or not. It is also called
%%          for every time the subscription is refreshed by the
%%          subscriber.
%% @end
%%--------------------------------------------------------------------
%%
%% SIPuser = undefined
%%
is_allowed_subscribe(Package, _Num, _Request, _YxaCtx, _SIPuser = undefined, _Presentity,
		     _PkgState) when Package == "ua-profile" orelse Package == "sip-config" ->
    {error, need_auth};
%%
%% Presentity is {users, UserList}
%%
is_allowed_subscribe(Package, _Num, _Request, YxaCtx, SIPuser, {users, ToUsers} = _Presentity, _PkgState)
  when Package == "ua-profile" orelse Package == "sip-config", is_list(SIPuser), is_list(ToUsers) ->
    logger:log(normal, "~s: ua-profile event package: User presentitys not supported, answering '403 Forbidden'",
	       [YxaCtx#yxa_ctx.app_logtag]),
    {siperror, 403, "Forbidden", []};
%%
%% Presentity is {address, AddressStr}
%%
is_allowed_subscribe(Package, _Num, Request, YxaCtx, SIPuser, {address, AddressStr} = _Presentity, _PkgState)
  when is_list(SIPuser), is_list(AddressStr), Package == "ua-profile" orelse Package == "sip-config" ->
    LogTag = YxaCtx#yxa_ctx.app_logtag,
    case yxa_config:get_env(uaprofile_base_path) of
	{ok, Base} when is_list(Base) ->
	    case get_config_filename(Package, Request, SIPuser, AddressStr, LogTag, Base) of
		{ok, Filename} ->
		    subscribe_to_file(Filename, LogTag);
		Res ->
		    Res
	    end;
	none ->
	    logger:log(error, "~s: ua-profile event package: Base path to device configurations not set "
		       "(uaprofile_base_path)", [LogTag]),
	    {siperror, 404, "Not Found", []}
    end.



%%--------------------------------------------------------------------
%% @spec    (Package, Presentity, LastAccept, PkgState) ->
%%            {ok, Body, ExtraHeaders, NewPkgState} |
%%            {error, Reason}
%%
%%            Package    = event_pkg()
%%            Presentity = {users, UserList} | {address, AddressStr}
%%            UserList   = [string()] "SIP usernames"
%%            AddressStr = string() "parseable with sipurl:parse/1"
%%            LastAccept = [string()] "Accept: header value from last SUBSCRIBE"
%%            PkgState   = #my_state{}
%%
%%            Body         = io_list()
%%            ExtraHeaders = [{Key, ValueList}] "headers to include in the NOTIFY request"
%%            Reason       = string() | atom()
%%            NewPkgState  = #my_state{}
%%
%% @doc     YXA event packages must export a notify_content/3
%%          function. Whenever the subscription requires us to
%%          generate a NOTIFY request, this function is called to
%%          generate the body and extra headers to include in the
%%          NOTIFY request.
%% @end
%%--------------------------------------------------------------------
notify_content(Package, Presentity, LastAccept, PkgState1) when is_record(PkgState1, my_state) ->
    {Size, MTime} = get_file_info(PkgState1#my_state.filename),
    PkgState = PkgState1#my_state{size  = Size,
				  mtime = MTime
				 },
    case make_external_body(Package, Presentity, LastAccept, PkgState) of
	{ok, Body, ExtraHeaders, PkgState} ->
	    {ok, Body, ExtraHeaders, PkgState};
	false ->
	    case get_application_content_type(LastAccept) of
		{ok, AppContent} ->
		    ExtraHeaders = [{"Content-Type", [AppContent]},
				    {"Event", [Package ++ ";effective-by=3600"]}
				   ],
		    {ok, Body1} = file:read_file(PkgState#my_state.filename),
		    Body = fix_body(Body1, AppContent),
		    {ok, Body, ExtraHeaders, PkgState};
		nomatch ->
		    {error, "Can't understand content type"}
	    end
    end.

make_external_body(Package, Presentity, LastAccept, PkgState) ->
    case lists:member("message/external-body", LastAccept) of
	true ->
	    case get_application_content_type(LastAccept) of
		{ok, AppContent} ->
		    DateStr = "XXX FIX THIS",
		    URL = "XXX FIX THIS",
		    ExtraHeaders = [{"Content-Type", ["multipart/mixed; boundary=boundary42"]}],
		    Body =
			["--boundary42\n",
			 "Content-Type: message/external-body;\n",
			 "    access-type=\"URL\";\n",
			 "    expiration=\"", DateStr, "\"\n",
			 "    URL=\"", URL, "\";\n",
			 "    size=", integer_to_list(PkgState#my_state.size), "\n",
			 "\n",
			 "Content-Type: ", AppContent, "\n",
			 "Content-ID: <", PkgState#my_state.content_md5, "@", sipauth:realm(), ">\n",
			 "\n",
			 "--boundary42--\n"
			],
		    {ok, Body, ExtraHeaders, PkgState};
		_ ->
		    false
	    end;
	false ->
	    false
    end.

%% Descrip.: Look for the first entry starting with 'application/' in a
%%            list of Accept header values.
%% Returns : {ok, String} | nomatch
get_application_content_type(["application/" ++ _ = H | _T]) ->
    {ok, H};
get_application_content_type([_H | T]) ->
    get_application_content_type(T);
get_application_content_type([]) ->
    nomatch.

fix_body(Body, "application/x-snom-config") ->
    S1 = binary_to_list(Body),
    case string:str(S1, "<pre>") of
	0 ->
	    S1;
	Index1 ->
	    S2 = string:substr(S1, Index1 + 5),
	    case string:str(S2, "</pre>") of
		0 ->
		    S1;
		Index2 ->
		    string:substr(S2, 1, Index2 - 1)
	    end
    end;
fix_body(Body, _AppContent) ->
    Body.


%%--------------------------------------------------------------------
%% @spec    package_parameters(PkgS::event_pkg(), Param) ->
%%            Value | undefined
%%
%%            Param = atom()
%%
%%            Value = term()
%%
%% @doc     YXA event packages must export a package_parameters/2
%%          function. 'undefined' MUST be returned for all unknown
%%          parameters.
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Package, notification_rate_limit) ->
%%            MilliSeconds
%%
%%            MilliSeconds = integer()
%%
%% @doc     The minimum amount of time that should pass between
%%          NOTIFYs we send about this event packages events.
%% @end
%%--------------------------------------------------------------------
package_parameters(Package, notification_rate_limit) when Package == "ua-profile" orelse Package == "sip-config" ->
    %% RFC4235 #3.10 (Rate of Notifications)
    1000;  %% 1000 milliseconds, 1 second

%%--------------------------------------------------------------------
%% @spec    (Package, request_methods) ->
%%            Methods
%%
%%            Methods = [string()]
%%
%% @doc     What SIP methods this event packages request/6 function
%%          can handle.
%% @end
%%--------------------------------------------------------------------
package_parameters(Package, request_methods) when Package == "ua-profile" orelse Package == "sip-config" ->
    [];

package_parameters(Package, _Param) when Package == "ua-profile" orelse Package == "sip-config" ->
    undefined.


%%--------------------------------------------------------------------
%% @spec
%%    subscription_behaviour(PkgS::event_pkg(), Param, Argument) ->
%%            Value | undefined
%%
%%            Param    = atom()
%%            Argument = term() "depending on Param"
%%
%%            Value = term()
%%
%% @doc     YXA event packages must export a sbuscription_behaviour/2
%%          function. 'undefined' MUST be returned for all unknown
%%          parameters.
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (Package, bidirectional_subscribe, Request) -> false
%%
%%            Package = event_pkg()
%%            Request = #request{}
%%
%% @doc     When we receive a SUBSCRIBE, should the subscription
%%          handler also SUBSCRIBE to the other side in the same
%%          dialog? For the ua-profile package, this is always false.
%% @end
%%--------------------------------------------------------------------
subscription_behaviour(Package, bidirectional_subscribe, Request) when Package == "ua-profile" orelse
								       Package == "sip-config",
								       is_record(Request, request) ->
    false;

subscription_behaviour(Package, _Param, _Argument) when Package == "ua-profile"; Package == "sip-config" ->
    undefined.

%%====================================================================
%% Internal functions
%%====================================================================

%% Returns : {ok, Filename} | {siperror, Status, Reason}
get_config_filename(Package, Request, SIPuser, _AddressStr, LogTag, Base) ->
    case string:to_lower((Request#request.uri)#sipurl.user) of
	"mac%3a" ++ DeviceName ->
	    safe_join(Base, DeviceName);
	_ ->
	    Vendor = url_param:find((Request#request.uri)#sipurl.param_pairs, "vendor"),
	    case (Package == "sip-config") andalso
		(Vendor == ["snom"]) of
		true ->
		    Product = url_param:find((Request#request.uri)#sipurl.param_pairs, "product"),
		    Str = lists:concat([SIPuser, "_", Vendor, "_", Product]),
		    safe_join(Base, Str);
		false ->
		    logger:log(error, "~s: ua-profile event package: Can't get device name from URI ~s",
			       [LogTag, sipurl:print(Request#request.uri)]),
		    {siperror, 404, "Not Found", []}
	    end
    end.

%% Returns : {error, need_auth} |       Request authentication
%%           {ok, SubState, Status, Reason, ExtraHeaders, Body, NewPkgState}  |
%%           {siperror, Status, Reason, ExtraHeaders}
%%           SubState     = active | pending
%%           Status       = integer(), SIP status code to respond with
%%           Reason       = string(), SIP reason phrase
%%           ExtraHeaders = list() of {Key, ValueList} to include in the response to the SUBSCRIBE
%%           Body         = binary() | list()
%%           NewPkgState  = my_state record()
subscribe_to_file(Filename, LogTag) ->
    case file:read_file(Filename) of
	{ok, Bin} ->
	    case get_file_info(Filename) of
		{Size, MTime} ->
		    MyState =
			#my_state{filename    = Filename,
				  size        = Size,
				  mtime       = MTime,
				  content_md5 = hex:to(erlang:md5(Bin))
				 },
		    {ok, active, 200, "Ok", [], <<>>, MyState};
		error ->
		    {siperror, 500, "Server Internal Error", []}
	    end;
	{error, enoent} ->
	    logger:log(error, "~s: ua-profile event package: No such file : ~s",
		       [LogTag, Filename]),
	    {siperror, 404, "Not Found", []};
	{error, Reason} ->
	    logger:log(error, "~s: ua-profile event package: Failed reading file ~s : ~p",
		       [LogTag, Filename, Reason]),
	    {siperror, 500, "Server Internal Error", []}
    end.


get_file_info(Filename) ->
    case file:read_file_info(Filename) of
	{ok, FI} when is_record(FI, file_info) ->
	    {FI#file_info.size, FI#file_info.mtime};
	{error, Reason} ->
	    logger:log(error, "ua-profile event package: Failed reading file info of file ~p : ~p", [Filename, Reason]),
	    error
    end.

safe_join(Base, Str) when is_list(Base), is_list(Str) ->
    Joined = filename:join(Base, Str),
    case string:str(Joined, "..") of
	0 ->
	    case length(Joined) of
		N when N =< 200 ->
		    {ok, Joined};
		Len ->
		    logger:log(error, "ua-profile event package: Filename ~p too long (~p >= 200)", [Joined, Len]),
		    {siperror, 403, "Forbidden", []}
	    end;
	_ ->
	    logger:log(error, "ua-profile event package: Possible relative filename : ~p", [Joined]),
	    {siperror, 403, "Forbidden", []}
    end.
