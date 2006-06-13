%%%-------------------------------------------------------------------
%%% File    : dialog_package.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Basic RFC4235 implementation.
%%%
%%% Created :  8 May 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(dialog_package).

-behaviour(event_package).

%%--------------------------------------------------------------------
%%% Standard YXA Event package exports
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/7,
	 is_allowed_subscribe/10,
	 notify_content/4,
	 package_parameters/2,
	 subscription_behaviour/3
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("event.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(my_state, {entity,	%% string(), dialog-info entity
		   version = 1	%% integer(), dialog-info version
		  }).

-record(dialog_data, {xml	%% string(), XML data from a dialog XML element in a NOTIFY we have received
		     }).



%%====================================================================
%% Behaviour functions
%% Standard YXA Event package callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init()
%% Descrip.: YXA event packages export an init/0 function.
%% Returns : none | {append, SupSpec}
%%           SupSpec = OTP supervisor child specification. Extra
%%                     processes this event package want the
%%                     sipserver_sup to start and maintain.
%%--------------------------------------------------------------------
init() ->
    none.

%%--------------------------------------------------------------------
%% Function: request("dialog", Request, Origin, LogStr, LogTag,
%%                   THandler, SIPuser)
%%           Request  = request record(), the SUBSCRIBE request
%%           Origin   = siporigin record()
%%           LogStr   = string(), describes the request
%%           LogTag   = string(), log prefix
%%           THandler = term(), server transaction handler
%%           Ctx      = event_ctx record(), context information for
%%                      request.
%% Descrip.: YXA event packages must export a request/7 function.
%%           See the eventserver.erl module description for more
%%           information about when this function is invoked.
%% Returns : void(), but return 'ok' or {error, Reason} for now
%%--------------------------------------------------------------------
request("dialog", _Request, _Origin, _LogStr, LogTag, _THandler, #event_ctx{sipuser = undefined}) ->
    logger:log(debug, "~s: dialog event package: Requesting authorization (only local users allowed)",
	       [LogTag]),
    {error, need_auth};

request("dialog", _Request, _Origin, LogStr, LogTag, THandler, _Ctx) ->
    logger:log(normal, "~s: dialog event package: ~s -> '501 Not Implemented'",
	       [LogTag, LogStr]),
    transactionlayer:send_response_handler(THandler, 501, "Not Implemented"),
    ok.


%%--------------------------------------------------------------------
%% Function: is_allowed_subscribe("dialog", Num, Request, Origin,
%%                                LogStr, LogTag, THandler, SIPuser,
%%                                PkgState)
%%           Num      = integer(), the number of subscribes we have
%%                      received on this dialog, starts at 1
%%           Request  = request record(), the SUBSCRIBE request
%%           Origin   = siporigin record()
%%           LogStr   = string(), describes the request
%%           LogTag   = string(), log prefix
%%           THandler = term(), server transaction handler
%%           SIPuser  = undefined | string(), undefined if request
%%                      originator is not not authenticated, and
%%                      string() if the user is authenticated (empty
%%                      string if user could not be authenticated)
%%           PkgState = undefined | my_state record()
%% Descrip.: YXA event packages must export an is_allowed_subscribe/8
%%           function. This function is called when the event server
%%           receives a subscription request for this event package,
%%           and is the event packages chance to decide wether the
%%           subscription should be accepted or not. It is also called
%%           for every time the subscription is refreshed by the
%%           subscriber.
%% Returns : {error, need_auth} |       Request authentication
%%           {ok, SubState, Status, Reason, ExtraHeaders,
%%                NewPkgState}  |
%%           {siperror, Status, Reason, ExtraHeaders}
%%           SubState     = active | pending
%%           Status       = integer(), SIP status code to respond with
%%           Reason       = string(), SIP reason phrase
%%           ExtraHeaders = list() of {Key, ValueList} to include in
%%                          the response to the SUBSCRIBE
%%           NewPkgState  = my_state record()
%%--------------------------------------------------------------------
%%
%% SIPuser = undefined
%%
is_allowed_subscribe("dialog", _Num, _Request, _Origin, _LogStr, _LogTag, _THandler, _SIPuser = undefined, _Presentity,
		     _PkgState) ->
    {error, need_auth};
%%
%% Presentity is {users, UserList}
%%
is_allowed_subscribe("dialog", _Num, _Request, _Origin, _LogStr, LogTag, _THandler, SIPuser,
		     {users, ToUsers} = _Presentity, _PkgState) when is_list(LogTag), is_list(SIPuser),
								     is_list(ToUsers) ->
    %% For the dialog package to work when the presentity is one or more users,
    %% we have to implement the following :
    %%
    %%   Subscribe to every registered contact for the user(s) using the 'dialog' event package
    %%   Monitor the location database for changes to the user(s), and monitor all new contacts registered
    %%
    logger:log(normal, "~s: dialog event package: User presentitys not supported (yet), answering '403 Forbidden'",
	       [LogTag]),
    {siperror, 403, "Forbidden", []};
%%
%% Presentity is {address, AddressStr}
%%
is_allowed_subscribe("dialog", _Num, Request, _Origin, _LogStr, _LogTag, _THandler, SIPuser,
		     {address, AddressStr} = _Presentity, PkgState) when is_list(SIPuser), is_list(AddressStr) ->
    is_allowed_subscribe2(Request, pending, 202, "Ok", [], PkgState).

is_allowed_subscribe2(Request, SubState, Status, Reason, ExtraHeaders, PkgState) when is_record(PkgState, my_state);
										      PkgState == undefined ->
    Header = Request#request.header,
    Accept = get_accept(Header),
    case lists:member("application/dialog-info+xml", Accept) of
	true ->
	    NewPkgState =
		case PkgState of
		    #my_state{} ->
			PkgState;
		    undefined ->
			#my_state{entity = sipurl:print(Request#request.uri)}
		end,
	    {ok, SubState, Status, Reason, ExtraHeaders, NewPkgState};
	false ->
	    {siperror, 406, "Not Acceptable", []}
    end.


%%--------------------------------------------------------------------
%% Function: notify_content("dialog", Presentity, LastAccept,
%%                          PkgState)
%%           Presentity = {users, UserList} | {address, AddressStr}
%%               UserList = list() of string(), SIP usernames
%%             AddressStr = string(), parseable with sipurl:parse/1
%%           LastAccept = list() of string(), Accept: header value
%%                        from last SUBSCRIBE
%%           PkgState   = my_state record()
%% Descrip.: YXA event packages must export a notify_content/3
%%           function. Whenever the subscription requires us to
%%           generate a NOTIFY request, this function is called to
%%           generate the body and extra headers to include in the
%%           NOTIFY request.
%% Returns : {ok, Body, ExtraHeaders, NewPkgState} |
%%           {error, Reason}
%%           Body         = io_list()
%%           ExtraHeaders = list() of {Key, ValueList} to include in
%%                          the NOTIFY request
%%           Reason       = string() | atom()
%%           NewPkgState  = my_state record()
%%--------------------------------------------------------------------
notify_content("dialog", Presentity, _LastAccept, PkgState) when is_record(PkgState, my_state) ->
    #my_state{entity  = Entity,
	      version = Version
	     } = PkgState,

    DialogsXML =
	case database_eventdata:fetch_using_presentity(Presentity) of
	    {ok, Dialogs} ->
		[(E#eventdata_dbe.data)#dialog_data.xml || E <- Dialogs];
	    nomatch ->
		""
	end,

    XML =
	"<?xml version=\"1.0\"?>\n"
	"<dialog-info xmlns=\"urn:ietf:params:xml:ns:dialog-info\"\n"
	"             version=\"" ++ integer_to_list(Version) ++ "\" state=\"full\"\n"
	"             entity=\"" ++ Entity ++ "\">\n" ++
	DialogsXML ++
	"</dialog-info>\n",

    ExtraHeaders = [{"Content-Type", ["application/dialog-info+xml"]}],

    NewPkgState = PkgState#my_state{version = Version + 1},
    {ok, XML, ExtraHeaders, NewPkgState}.


%%--------------------------------------------------------------------
%% Function: package_parameters("dialog", Param)
%%           Param = atom()
%% Descrip.: YXA event packages must export a package_parameters/2
%%           function. 'undefined' MUST be returned for all unknown
%%           parameters.
%% Returns : Value | undefined
%%           Value = term()
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: package_parameters("presence", notification_rate_limit)
%% Descrip.: The minimum amount of time that should pass between
%%           NOTIFYs we send about this event packages events.
%% Returns : MilliSeconds = integer()
%%--------------------------------------------------------------------
package_parameters("dialog", notification_rate_limit) ->
    %% RFC4235 #3.10 (Rate of Notifications)
    1000;  %% 1000 milliseconds, 1 second

%%--------------------------------------------------------------------
%% Function: package_parameters("presence", request_methods)
%% Descrip.: What SIP methods this event packages request/7 function
%%           can handle.
%% Returns : Methods = list() of string()
%%--------------------------------------------------------------------
package_parameters("dialog", request_methods) ->
    ["PUBLISH"];

%%--------------------------------------------------------------------
%% Function: package_parameters("presence",
%%                              subscribe_accept_content_types)
%% Descrip.: What Content-Type encodings we should list as acceptable
%%           in the SUBSCRIBEs we send.
%% Returns : ContentTypes = list() of string()
%%--------------------------------------------------------------------
package_parameters("dialog", subscribe_accept_content_types) ->
    ["application/dialog-info+xml"];

package_parameters("dialog", _Param) ->
    undefined.


%%--------------------------------------------------------------------
%% Function: subscription_behaviour("dialog", Param, Argument)
%%           Param = atom()
%%           Argument = term(), depending on Param
%% Descrip.: YXA event packages must export a sbuscription_behaviour/2
%%           function. 'undefined' MUST be returned for all unknown
%%           parameters.
%% Returns : Value | undefined
%%           Value = term()
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: subscription_behaviour("dialog", bidirectional_subscribe,
%%                                  Request)
%%           Request = request record()
%% Descrip.: When we receive a SUBSCRIBE, should the subscription
%%           handler also SUBSCRIBE to the other side in the same
%%           dialog? For the dialog package, this is always true.
%% Returns : true
%%--------------------------------------------------------------------
subscription_behaviour("dialog", bidirectional_subscribe, Request) when is_record(Request, request) ->
    true;

subscription_behaviour("dialog", _Param, _Argument) ->
    undefined.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: get_accept(Header)
%%           Header = keylist record()
%% Descrip.: Get Accept: header value (or default) from a header.
%% Returns : list() of string()
%%--------------------------------------------------------------------
get_accept(Header) ->
    case keylist:fetch('accept', Header) of
	[] ->
	    %% RFC4235 #3.5
	    ["application/dialog-info+xml"];
	AcceptV ->
	    [http_util:to_lower(Elem) || Elem <- AcceptV]
    end.

