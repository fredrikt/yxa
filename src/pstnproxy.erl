%%%-------------------------------------------------------------------
%%% File    : pstnproxy.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      An SIP application level 'firewall' to defend a PSTN
%%%           gateway from misuse and to make sure users are
%%%           authorized to make calls to different 'classes' of
%%%           numbers (configured through regular expressions). Also
%%%           perform ENUM lookups on requests _from_ the PSTN gw.
%%%           See the README file for more information.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------
-module(pstnproxy).

-behaviour(yxa_app).

%%--------------------------------------------------------------------
%%% Standard YXA SIP-application callback functions
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/2,
	 response/2,
	 terminate/1
	]).

-export([test/0]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").
-include("pstnproxy.hrl").


%%====================================================================
%% Behaviour functions
%% Standard YXA SIP-application callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> #yxa_app_init{}
%%
%% @doc     YXA applications must export an init/0 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    #yxa_app_init{mnesia_tables = [user, numbers]
		 }.


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Request = #request{}
%%            YxaCtl  = #yxa_ctl{}
%%
%% @doc     YXA applications must export a request/2 function.
%% @end
%%--------------------------------------------------------------------

%%
%% ACK
%%
request(#request{method = "ACK"} = Request, YxaCtx) when is_record(YxaCtx, yxa_ctx) ->
    %% ACK requests that end up here could not be matched to a server transaction,
    %% most probably they are ACK to 2xx of INVITE (or we have crashed) - proxy
    %% statelessly.
    transportlayer:stateless_proxy_ack("pstnproxy", Request, YxaCtx),
    ok;

%%
%% Anything except ACK
%%
request(Request, YxaCtx) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    LogTag = transactionlayer:get_branchbase_from_handler(YxaCtx#yxa_ctx.thandler),
    YxaCtx1 = YxaCtx#yxa_ctx{app_logtag = LogTag},
    case auth_and_tag(Request, YxaCtx1) of
	{ok, PstnCtx} ->
	    handle_request(Request, YxaCtx1, PstnCtx);
	ignore ->
	    ok
    end.


%%--------------------------------------------------------------------
%% @spec    (Response, YxaCtx) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Response = #response{}
%%            YxaCtx   = #yxa_ctx{}
%%
%% @doc     YXA applications must export a response/2 function.
%% @end
%%--------------------------------------------------------------------
response(Response, YxaCtx) when is_record(Response, response), is_record(YxaCtx, yxa_ctx) ->
    {Status, Reason} = {Response#response.status, Response#response.reason},
    logger:log(normal, "pstnproxy: Response to ~s: ~p ~s, no matching transaction - proxying statelessly",
	       [YxaCtx#yxa_ctx.logstr, Status, Reason]),
    transportlayer:send_proxy_response(none, Response),
    ok.


%%--------------------------------------------------------------------
%% @spec    (Mode) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Mode = shutdown | graceful | atom()
%%
%% @doc     YXA applications must export a terminate/1 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
terminate(Mode) when is_atom(Mode) ->
    ok.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) ->
%%            {ok, PstnCtx} |
%%            ignore
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%%            PstnCtx = #pstn_ctx{}
%%
%% @doc     Create a pstn_ctx record for this request. The pstn_ctx
%%          record holds information that we can conclude right away
%%          about the request :
%%          tags : list of atom() : from_gateway : Request was
%%          received from one of our gateways. has_route : Request
%%          has a Route header. route_to_gw : Included if the Route
%%          header points at one of our gateways.
%%          ip : string(), the IP address we received the request
%%          from.
%%          cert_subject : ssl_conn_subject record() describing TLS
%%          certificate used by client to send us this request.
%%          user : undefined | string(), the SIP user for which valid
%%          credentials has been provided
%%          stale_auth : true - authentication provided, but it is
%%          stale false - authentication not stale, but not necessary
%%          provided
%%          orig_uri : sipurl record(), the Request-URI of the
%%          request we received.
%%          If the From: address of the request is listed for one of
%%          our users, we know that we will require confirmation of
%%          this so then we challenge the request if valid authenti-
%%          cation is not already provided. Gateways are excepted
%%          from this, they are allowed to provide us with any name.
%% @end
%%--------------------------------------------------------------------
auth_and_tag(Request, YxaCtx) ->
    #yxa_ctx{thandler	= THandler,
	     origin	= Origin,
	     app_logtag	= LogTag
	    } = YxaCtx,
    IP = Origin#siporigin.addr,
    CertSubject = auth_and_tag_get_cert(Origin, LogTag),
    {ok, Stale, YXAPeerAuth, User} = auth_and_tag_get_user(Request),

    PstnCtx1 =
	#pstn_ctx{ip		= IP,
		  cert_subject	= CertSubject,
		  user		= User,
		  stale_auth	= Stale,
		  orig_uri	= Request#request.uri
		 },

    Tags = auth_and_tag_get_tags(Request, YXAPeerAuth, PstnCtx1),
    PstnCtx2 = PstnCtx1#pstn_ctx{tags = Tags},

    PstnCtx = local:pstnproxy_auth_and_tag(Request, Origin, THandler, PstnCtx2),

    case auth_and_tag_verify_from(Request, YxaCtx, PstnCtx) of
	ok ->
	    {ok, PstnCtx};
	_ ->
	    ignore
    end.


%%--------------------------------------------------------------------
%% @spec    (Origin, LogTag) ->
%%            Subject   |
%%            undefined
%%
%%            Origin = #siporigin{}
%%            LogTag = string() "log prefix"
%%
%%            Subject = string()
%%
%% @doc     If the request was received over TLS, try to construct a
%%          string describing the certificate. Returns 'undefined' if
%%          such a string could not be created, for example because
%%          the connecting party did not use a client certificate.
%% @end
%%--------------------------------------------------------------------
auth_and_tag_get_cert(#siporigin{proto = Proto} = Origin, LogTag) when Proto == tls; Proto == tls6 ->
    #siporigin{sipsocket = SipSocket,
	       addr      = IP,
	       port      = Port
	      } = Origin,
    case sipsocket:get_raw_socket(SipSocket) of
	{ok, Socket} ->
	    case ssl_util:get_ssl_peer_info(Socket, Proto, IP, Port) of
		{ok, Subject, _AltNames} when is_record(Subject, ssl_conn_subject) ->
		    Subject;
		_ ->
		    undefined
	    end;
	{error, Reason} ->
	    logger:log(error, "~s: pstnproxy: Failed fetching SSL certificate info : ~p",
		       [LogTag, Reason]),
	    undefined
    end;
auth_and_tag_get_cert(_Origin, _LogTag) ->
    undefined.


%%--------------------------------------------------------------------
%% @spec    (Request) ->
%%            {ok, Stale, YXAPeerAuth, User}
%%
%%            Request = #request{}
%%
%%            Stale       = bool() "was the provided authentication information stale? false if none provided."
%%            YXAPeerAuth = bool() "user information vouched for by a peer of ours, through X-Yxa-Peer-Auth?"
%%            User        = undefined | string() "username"
%%
%% @doc     Try to get a username for this request, and also check to
%%          see if there are authentication information present.
%% @end
%%--------------------------------------------------------------------
auth_and_tag_get_user(Request) ->
    {Stale, YXAPeerAuth, UserRes} =
	case sipauth:pstn_get_user_verified(Request#request.header, Request#request.method) of
	    false ->
		{false, false, undefined};
	    {stale, User} ->
		{true, false, User};
	    {authenticated, User} ->
		{false, false, User};
	    {peer_authenticated, User} ->
		%% this implies that the peer authentication provided wasn't stale
		{false, true, User}
	end,
    {ok, Stale, YXAPeerAuth, UserRes}.


%%--------------------------------------------------------------------
%% @spec    (Request, IsPeerAuth, PstnCtx) ->
%%            Tags
%%
%%            Request    = #request{}
%%            IsPeerAuth = boolean() "X-Yxa-Peer-Auth authenticated?"
%%            PstnCtx    = #pstn_ctx{}
%%
%%            Tags = [term()]
%%
%% @doc     Create a list of tags for this request.
%% @end
%%--------------------------------------------------------------------
auth_and_tag_get_tags(Request, IsPeerAuth, PstnCtx) when is_record(Request, request), is_boolean(IsPeerAuth),
							 is_record(PstnCtx, pstn_ctx) ->
    FromGw =
	case is_pstngateway(PstnCtx#pstn_ctx.ip) of
	    true ->  [from_gateway];
	    false -> []
	end,

    HasRoute =
	case keylist:fetch('route', Request#request.header) of
	    [FirstRoute | _] ->
		[FirstRouteParsed] = contact:parse([FirstRoute]),
		RouteURL = sipurl:parse(FirstRouteParsed#contact.urlstr),
		case is_pstngateway(RouteURL#sipurl.host) of
		    true ->
			[has_route, route_to_gw];
		    false ->
			[has_route]
		end;
	    _ ->
		[]
	end,

    AuthFlags =
	case IsPeerAuth of
	    true ->  [peer_auth];
	    false -> []
	end,

    FromGw ++ HasRoute ++ AuthFlags.


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, PstnCtx) ->
%%            ignore |
%%            ok
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            PstnCtx = #pstn_ctx{}
%%
%% @doc     If the From: has an address belonging to one of our users,
%%          verify that the sender is authorized to use it. This
%%          requires digest-authenticating a user, or determin- ing
%%          that the request was received from one of our gateways or
%%          was vouched for by a peer of ours.
%% @end
%%--------------------------------------------------------------------
auth_and_tag_verify_from(Request, YxaCtx, PstnCtx) ->
    #yxa_ctx{thandler	= THandler,
	     app_logtag	= LogTag
	    } = YxaCtx,
    YXAPeerAuth = is_tagged(peer_auth, PstnCtx),
    case local:pstnproxy_verify_from(Request, THandler, YXAPeerAuth, PstnCtx) of
	ignore    -> ignore;
	ok        -> ok;
	undefined ->
	    FromGateway = is_tagged(from_gateway, PstnCtx),
	    if
		YXAPeerAuth ->
		    logger:log(debug, "~s: pstnproxy: Request was received from one of our peers "
                               "(X-Yxa-Peer-Auth), allowing any From: address", [LogTag]),
		    ok;
		FromGateway ->
		    logger:log(debug, "~s: pstnproxy: Request was received from one of our gateways, "
			       "allowing any From: address", [LogTag]),
		    ok;
		true ->
		    auth_and_tag_verify_from2(Request, YxaCtx, PstnCtx)
	    end
    end.

%% part of auth_and_tag_verify_from/5
%% Returns : ignore | ok
auth_and_tag_verify_from2(Request, YxaCtx, PstnCtx) ->
    THandler = YxaCtx#yxa_ctx.thandler,
    {_DisplayName, FromURL} = sipheader:from(Request#request.header),
    Address = sipurl:print(FromURL),
    case local:get_user_with_address(Address) of
	nomatch ->
	    logger:log(debug, "Auth: Address ~p does not match any of my users, no need to verify.",
		       [Address]),
	    ok;

	User when is_list(User), is_list(PstnCtx#pstn_ctx.user) ->
	    case local:can_use_address(PstnCtx#pstn_ctx.user, FromURL) of
		true ->
		    %% don't allow stale authentication for this check
		    case PstnCtx#pstn_ctx.stale_auth of
			true ->
			    transactionlayer:send_challenge(THandler, proxy, _Stale = true, _RetryAfter = none),
			    ignore;
			false ->
			    ok
		    end;
		false ->
		    logger:log(normal, "Auth: User ~p is not allowed to use From: address ~p",
			       [User, Address]),
		    transactionlayer:send_response_handler(THandler, 403, "Forbidden"),
		    ignore
	    end;

	User when is_list(User) ->
	    %% PstnCtx did not contain a SIP username, or the authentication was stale. Send challenge.
	    Stale = PstnCtx#pstn_ctx.stale_auth,
	    transactionlayer:send_challenge(THandler, proxy, Stale, _RetryAfter = none),
	    ignore
    end.


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, PstnCtx) -> ok
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%% @doc     Handle any non-ACK requests we receive, after the request
%%          has been tagged, and potentially authenticated.
%% @end
%%--------------------------------------------------------------------
handle_request(Request, YxaCtx, PstnCtx) ->
    Actions =
	case is_tagged(has_route, PstnCtx) of
	    true ->
		route_request_with_route(Request, YxaCtx, PstnCtx);
	    false ->
		%% no Route header
		case local:is_request_to_this_proxy(Request) of
		    true ->
			%% XXX it would be nice to include an Allow: header with the allowed methods, but
			%% that is complicated since it can vary depending on PstnCtx
			siprequest:request_to_me(Request, YxaCtx, _ExtraHeaders = []),
			[ignore];
		    false ->
			%% no Route, and not destined for the proxy itself
			ToHost = (Request#request.uri)#sipurl.host,
			case is_localhostname(ToHost) orelse
			    is_pstngateway(ToHost) of
			    true ->
				number_based_routing(Request, YxaCtx, PstnCtx);
			    false ->
				handle_request_unknown_destination(Request, YxaCtx, PstnCtx)
			end
		end
	end,

    logger:log(debug, "~s: pstnproxy: Actions for this request : ~p", [YxaCtx#yxa_ctx.app_logtag, Actions]),

    perform_actions(Actions, Request, YxaCtx, PstnCtx),
    ok.

%% part of handle_request/3
handle_request_unknown_destination(Request, YxaCtx, PstnCtx) ->
    LogTag = YxaCtx#yxa_ctx.app_logtag,
    case is_tagged(from_gateway, PstnCtx) andalso
	Request#request.method == "BYE" of
	true ->
	    %% BYE from gateways can't be challenged, and can be addresses at
	    %% arbitrary URI's
	    logger:log(debug, "~s: pstnproxy: Proxying BYE request from gateway", [LogTag]),
	    [{proxy, Request#request.uri}];
	false ->
	    logger:log(normal, "~s: pstnproxy: Denied request to ~s - not my problem "
		       "(no Route header, and not addressed to me)",
		       [LogTag, sipurl:print(Request#request.uri)]),
	    [{response, 403, "Forbidden", []}]
    end.


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, PstnCtx) ->
%%            [{proxy, route}]                 |
%%            [{response, Status, Reason, EH}] |
%%            [{challenge, proxy, IsStale}]
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%% @doc     Figure out what to do with a request with a Route header.
%% @end
%%--------------------------------------------------------------------
route_request_with_route(Request, YxaCtx, PstnCtx) ->
    %% we relay any request with a Route header, as long as it is authenticated
    %% or is from one of our gateways, and it is not an INVITE
    case Request#request.method of
	"INVITE" ->
	    case is_tagged(from_gateway, PstnCtx) of
		true ->
		    %% XXX double-check that INVITE is an allowed method?
		    [{proxy, route}];
		false ->
		    [{response, 403, "Forbidden (INVITE with Route)", []}]
	    end;
	_ ->
	    %% non-INVITE, allow if authenticated (not stale), or from one of our gateways
	    case is_list(PstnCtx#pstn_ctx.user) andalso
		(PstnCtx#pstn_ctx.stale_auth /= false) of
		true ->
		    logger:log(debug, "~s: pstnproxy: Proxying authenticated request with Route header",
			       [YxaCtx#yxa_ctx.app_logtag]),
		    [{proxy, route}];
		false ->
		    [{challenge, proxy, PstnCtx#pstn_ctx.stale_auth}]
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, PstnCtx) ->
%%            ActionList
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%%            ActionList = [term()]
%%
%% @doc     Figure out what to do with a request that is destined for
%%          one of our hostnames, or one of our gateways names.
%% @end
%%--------------------------------------------------------------------
number_based_routing(Request, YxaCtx, PstnCtx) ->
    #yxa_ctx{thandler	= THandler,
	     app_logtag	= LogTag
	    } = YxaCtx,
    case local:pstnproxy_number_based_routing(Request, THandler, LogTag, PstnCtx) of
	undefined ->
	    This =
		case is_tagged(from_gateway, PstnCtx) of
		    true ->
			[{lookup, enum},
			 {lookup, sipproxy}
			];
		    false ->
			%% not from one of our PSTN gateways
			[{lookup, to_pstngw},
			 {lookup, pstn},
			 {lookup, not_e164},
			 {lookup, default_pstngateway}
			]
		end,
	    [{lookup, local}] ++ This ++ [{response, 404, "Not Found", []}];
	Res when is_list(Res) ->
	    Res
    end.


%%--------------------------------------------------------------------
%% @spec    (Actions, Request, YxaCtx, PstnCtx) -> void()
%%
%%            Actions = [tuple() | atom()]
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%% @doc     Process the Request through performing a set of actions,
%%          that include trying to find a destination, proxying the
%%          request to a found destination, challenging the sender of
%%          the request or sending a response of some sort.
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    ({response, Status, Reason, ExtraHeaders}, Request,
%%          YxaCtx, PstnCtx) -> void()
%%
%%            Status       = integer() "SIP status code"
%%            Reason       = string() "SIP reason phrase"
%%            ExtraHeaders = [{Key, Value}] "extra headers to put in the response"
%%            YxaCtx       = #yxa_ctx{}
%%
%% @doc     Send a response.
%% @end
%%--------------------------------------------------------------------
perform_actions([{response, Status, Reason, ExtraHeaders} | _], _Request, YxaCtx, _PstnCtx) ->
    #yxa_ctx{thandler	= THandler,
	     app_logtag	= LogTag
	    } = YxaCtx,
    logger:log(debug, "~s: pstnproxy: Action 'response' : '~p ~s'", [LogTag, Status, Reason]),
    transactionlayer:send_response_handler(THandler, Status, Reason, ExtraHeaders);

%%--------------------------------------------------------------------
%% @spec    ({proxy, Dst}, Request, YxaCtx, PstnCtx) -> void()
%%
%%            Dst     = #sipurl{} | route
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            PstnCtx = #pstn_ctx{}
%%
%% @doc     Proxy the request to a destination, if permitted.
%% @end
%%--------------------------------------------------------------------
perform_actions([{proxy, Dst} | _], Request, YxaCtx, PstnCtx) when is_record(Dst, sipurl); Dst == route ->
    #yxa_ctx{thandler	= THandler,
	     app_logtag	= LogTag
	    } = YxaCtx,
    logger:log(debug, "~s: pstnproxy: Action 'proxy' : ~p", [LogTag, Dst]),

    {ok, AllowedMethods} = local:pstnproxy_allowed_methods(Request, PstnCtx),

    {IsAllowed, NewPstnCtx} =
	case local:pstnproxy_allowed_proxy_request(Request, PstnCtx) of
	    true -> {true, PstnCtx};
	    false -> {false, PstnCtx};
	    undefined ->
		case authorize_proxying(Request, YxaCtx, PstnCtx) of
		    {ok, true, NewPstnCtx1} ->
			case lists:member(Request#request.method, AllowedMethods) of
			    true ->
				{true, NewPstnCtx1};
			    false ->
				logger:log(normal, "~s: pstnproxy: Method ~s not allowed for this destination",
					   [LogTag, Request#request.method]),
				ExtraHeaders = [{"Allow", AllowedMethods}],
				transactionlayer:send_response_handler(THandler, 405, "Method Not Allowed",
								       ExtraHeaders),
				{false, NewPstnCtx1}
			end;
		    {ok, false, NewPstnCtx1} ->
			{false, NewPstnCtx1}
		end
	end,

    case IsAllowed of
	true ->
	    NewHeader = add_caller_identity(PstnCtx#pstn_ctx.destination, Request#request.method,
					    Request#request.header, Dst, NewPstnCtx),

	    %% make sure we don't downgrade SIPS to SIP
	    NewURI = restore_sips_proto(NewPstnCtx#pstn_ctx.orig_uri, Request#request.uri),
	    NewRequest1 =
		Request#request{uri = NewURI,
				header = NewHeader
			       },
	    {NewDst, NewRequest} =
		case Dst of
		    _ when is_record(Dst, sipurl) ->
			NewDst1 = restore_sips_proto(NewPstnCtx#pstn_ctx.orig_uri, Dst),
			{NewDst1, NewRequest1};
		    route ->
			%% XXX do we need to ensure first Route header is SIPS, if Request-URI was?
			{Dst, NewRequest1}
		end,

	    start_sippipe(NewRequest, YxaCtx, NewDst, [NewPstnCtx]);
	false ->
	    ok
    end;

%%--------------------------------------------------------------------
%% @spec    ({challenge, Type, Stale}, ...) -> void()
%%
%%            Type  = proxy | www
%%            Stale = bool()
%%
%% @doc     Send a challenge to the sender of a request.
%% @end
%%--------------------------------------------------------------------
perform_actions([{challenge, Type, Stale} | _], _Request, YxaCtx, _PstnCtx) when is_atom(Type),
										 is_boolean(Stale) ->
    #yxa_ctx{thandler	= THandler,
	     app_logtag	= LogTag
	    } = YxaCtx,
    logger:log(debug, "~s: pstnproxy: Action 'challenge' : type ~p, stale : ~p", [LogTag, Type, Stale]),
    transactionlayer:send_challenge(THandler, Type, Stale, _RetryAfter = none);

%%--------------------------------------------------------------------
%% @spec    ({lookup, Type}, ...) -> void()
%%
%%            Type = enum | pstn | not_e164 | ...
%%
%% @doc     Perform a lookup of some sort, and then either continue to
%%          the next action (if nomatch was returned), or go process
%%          the result of the lookup as the next action.
%% @end
%%--------------------------------------------------------------------
perform_actions([{lookup, Type} | T], Request, YxaCtx, PstnCtx) ->
    LogTag = YxaCtx#yxa_ctx.app_logtag,
    logger:log(debug, "~s: pstnproxy: Action 'lookup' type '~p'", [LogTag, Type]),
    case perform_lookup(Type, Request, PstnCtx) of
	{ok, Substitute, NewPstnCtx} when is_tuple(Substitute) ->
	    logger:log(debug, "~s: pstnproxy: Lookup result : ~p", [LogTag, Substitute]),
	    perform_actions([Substitute | T], Request, YxaCtx, NewPstnCtx);
	{ok, Substitute, NewPstnCtx} when is_list(Substitute) ->
	    logger:log(debug, "~s: pstnproxy: Lookup result : ~p", [LogTag, Substitute]),
	    perform_actions(Substitute ++ T, Request, YxaCtx, NewPstnCtx);
	nomatch ->
	    perform_actions(T, Request, YxaCtx, PstnCtx)
    end;

%%--------------------------------------------------------------------
%% @spec    (ignore, ...) -> void()
%%
%%            Type = enum | pstn | not_e164 | ...
%%
%% @doc     Perform a lookup of some sort, and then either continue to
%%          the next action (if nomatch was returned), or go process
%%          the result of the lookup as the next action.
%% @end
%%--------------------------------------------------------------------
perform_actions([ignore | _], _Request, YxaCtx, _PstnCtx) ->
    logger:log(debug, "~s: pstnproxy: Action 'ignore'", [YxaCtx#yxa_ctx.app_logtag]),
    ok;

%%--------------------------------------------------------------------
%% @spec    ([], ...) -> void()
%%
%% @doc     No actions left, send a '500 Server Internal Error'.
%% @end
%%--------------------------------------------------------------------
perform_actions([], _Request, YxaCtx, _PstnCtx) ->
    #yxa_ctx{thandler	= THandler,
	     app_logtag	= LogTag
	    } = YxaCtx,
    logger:log(normal, "~s: pstnproxy: No more actions, responding '500 Server Internal Error'", [LogTag]),
    transactionlayer:send_response_handler(THandler, 500, "Server Internal Error", []),
    ok.


%%--------------------------------------------------------------------
%% @spec    (Type, Request, PstnCtx) ->
%%            {ok, LookupSubstitute, NewPstnCtx} |
%%            nomatch
%%
%%            Type    = atom()
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            LookupSubstitute = tuple() | atom()
%%            NewPstnCtx       = #pstn_ctx{}
%%
%% @doc     Perform a lookup of some sort, which may or may not result
%%          in a new action of the type that perform_actions process.
%% @end
%%--------------------------------------------------------------------

%% @clear

%%--------------------------------------------------------------------
%% @spec    (enum, Request, PstnCtx) ->
%%            {ok, {proxy, Dst}, NewPstnCtx} |
%%            nomatch
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            Dst        = #sipurl{}
%%            NewPstnCtx = #pstn_ctx{}
%%
%% @doc     Perform an ENUM lookup. ENUM resolves E.164 numbers
%%          through DNS (RFC3761).
%% @end
%%--------------------------------------------------------------------
perform_lookup(enum, Request, PstnCtx) ->
    User = (Request#request.uri)#sipurl.user,
    logger:log(debug, "pstnproxy: ENUM lookup : user-part ~p", [User]),
    case local:lookupenum(User) of
	{relay, Loc} when is_record(Loc, sipurl) ->
	    NewH =
		case yxa_config:get_env(pstnproxy_redirect_on_enum) of
		    {ok, true} ->
			%% Redirect caller to the destination we found in ENUM instead of
			%% proxying the request.
			Contact = contact:new(none, Loc, []),
			ExtraHeaders = [{"Contact", sipheader:contact_print([Contact])}],
			{response, 302, "Moved Temporarily", ExtraHeaders};
		{ok, false} ->
			%% proxy request (need for authentication is performed later)
			{proxy, Loc}
		end,
	    NewPstnCtx =
		PstnCtx#pstn_ctx{called_number = User,
				 destination   = sip
				},
	    {ok, NewH, NewPstnCtx};
	{proxy, ProxyURL} when is_record(ProxyURL, sipurl) ->
	    %% Proxy instead of relay means that the ENUM result points at a 'homedomain'.
	    %% Since pstnproxy can't be configured with homedomains, it means the result
	    %% matched one of our hostnames. This is probably not a good thing, but should
	    %% be investigated. What if a PSTN gateway sends a request to us, and we have
	    %% 'forwarding' information for that number in ENUM? XXX
	    logger:log(normal, "pstnproxy: ENUM for number ~p points back at me (~p) - ignoring",
		       [User, sipurl:print(ProxyURL)]),
	    nomatch;
	none ->
	    nomatch
    end;

%%--------------------------------------------------------------------
%% @spec    (sipproxy, Request, PstnCtx) ->
%%            {ok, {proxy, Dst}, NewPstnCtx} |
%%            nomatch
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            Dst        = #sipurl{}
%%            NewPstnCtx = #pstn_ctx{}
%%
%% @doc     Look up our default SIP proxy.
%% @end
%%--------------------------------------------------------------------
perform_lookup(sipproxy, Request, PstnCtx) ->
    case yxa_config:get_env(sipproxy) of
	{ok, ProxyURL1} when is_record(ProxyURL1, sipurl) ->
	    %% Use configured sipproxy but exchange user with user from Request-URI
	    User = (Request#request.uri)#sipurl.user,
	    ProxyURL = sipurl:set([{user, User},
				   {pass, none}
				  ], ProxyURL1),
	    NewPstnCtx =
		PstnCtx#pstn_ctx{called_number = User,
				 destination   = sip
				},
	    {ok, {proxy, ProxyURL}, NewPstnCtx};
	none ->
	    logger:log(debug, "pstnproxy: No sipproxy configured"),
	    nomatch
    end;

%%--------------------------------------------------------------------
%% @spec    (pstn, Request, PstnCtx) ->
%%            {ok, {proxy, Dst}, NewPstnCtx} |
%%            nomatch
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            Dst        = #sipurl{}
%%            NewPstnCtx = #pstn_ctx{}
%%
%% @doc     Look for a PSTN destination based on the user part of the
%%          Request-URI of our Request. Only works if the user part
%%          can be turned into an E.164 number.
%% @end
%%--------------------------------------------------------------------
perform_lookup(pstn, Request, PstnCtx) ->
    User = (Request#request.uri)#sipurl.user,
    case local:lookuppstn(User) of
	{Type, Dst} when Type == proxy orelse Type == relay, is_record(Dst, sipurl) ->
	    NewPstnCtx =
		PstnCtx#pstn_ctx{called_number = User,
				 destination   = pstn
				},
	    {ok, {proxy, Dst}, NewPstnCtx};
	none ->
	    nomatch
    end;

%%--------------------------------------------------------------------
%% @spec    (not_e164, Request, PstnCtx) ->
%%            {ok, Res, NewPstnCtx} |
%%            nomatch
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            Res = term()
%%
%% @doc     Look for a PSTN destination for a number that could not be
%%          resolved into an E.164 number.
%% @end
%%--------------------------------------------------------------------
perform_lookup(not_e164, Request, PstnCtx) ->
    User = (Request#request.uri)#sipurl.user,
    case local:pstnproxy_route_pstn_not_e164(User, Request, PstnCtx) of
	undefined ->
	    nomatch;
	nomatch ->
	    logger:log(debug, "pstnproxy: local:pstnproxy_route_pstn_not_e164(...) says there is no "
		       "destination for request to PSTN, number ~p.", [User]),
	    nomatch;
	Res ->
	    NewPstnCtx =
		PstnCtx#pstn_ctx{called_number = User,
				 destination   = pstn
				},
	    {ok, Res, NewPstnCtx}
    end;

%%--------------------------------------------------------------------
%% @spec    (default_pstngateway, Request, PstnCtx) ->
%%            {ok, {proxy, Dst}, NewPstnCtx} |
%%            nomatch
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            Dst        = #sipurl{}
%%            NewPstnCtx = #pstn_ctx{}
%%
%% @doc     Look up our default PSTN gateway.
%% @end
%%--------------------------------------------------------------------
perform_lookup(default_pstngateway, Request, PstnCtx) ->
    %% Route to default PSTN gateway
    case yxa_config:get_env(default_pstngateway) of
	{ok, GwURL} when is_record(GwURL, sipurl) ->
	    logger:log(debug, "pstnproxy: Routing request to default PSTN gateway ~p", [sipurl:print(GwURL)]),
	    User = (Request#request.uri)#sipurl.user,
	    NewURI = sipurl:set([{user, User},
				 {pass, none}
				], GwURL),
	    NewPstnCtx =
		PstnCtx#pstn_ctx{called_number = User,
				 destination   = pstn
				},
	    {ok, {proxy, NewURI}, NewPstnCtx};
	none ->
	    nomatch
    end;

%%--------------------------------------------------------------------
%% @spec    (local, Request, PstnCtx) ->
%%            {ok, Action, NewPstnCtx} |
%%            nomatch
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            Action     = atom() | tuple()
%%            NewPstnCtx = #pstn_ctx{}
%%
%% @doc     Call a local.erl hook.
%% @end
%%--------------------------------------------------------------------
perform_lookup(local, Request, PstnCtx) ->
    case local:pstnproxy_lookup_action(Request, PstnCtx) of
	{ok, Action, NewPstnCtx} when is_tuple(Action) orelse is_atom(Action), is_record(NewPstnCtx, pstn_ctx) ->
	    {ok, Action, NewPstnCtx};
	undefined ->
	    nomatch
    end;

%%--------------------------------------------------------------------
%% @spec    (to_pstngw, Request, PstnCtx) ->
%%            {ok, {proxy, Dst}, NewPstnCtx} |
%%            nomatch
%%
%%            Request = #request{}
%%            PstnCtx = #pstn_ctx{}
%%
%%            Dst        = #sipurl{}
%%            NewPstnCtx = #pstn_ctx{}
%%
%% @doc     Check if request is addressed directly to one of our PSTN
%%          gateways, and if we should allow it based solely on that.
%% @end
%%--------------------------------------------------------------------
perform_lookup(to_pstngw, Request, PstnCtx) ->
    ToHost = (Request#request.uri)#sipurl.host,
    case is_pstngateway(ToHost) of
	true ->
	    logger:log(debug, "pstnproxy: Routing request to specified PSTN gateway : ~s",
		       [ToHost]),
	    NewPstnCtx =
		PstnCtx#pstn_ctx{destination   = pstn
				},
	    {ok, {proxy, Request#request.uri}, NewPstnCtx};
	false ->
	    nomatch
    end.

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, PstnCtx) ->
%%            {ok, Verdict, NewPstnCtx}
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%%            Verdict = true | false
%%
%% @doc     Before we proxy a request somewhere, make sure that it is
%%          not against our policy.
%% @end
%%--------------------------------------------------------------------
authorize_proxying(Request, YxaCtx, PstnCtx) ->
    case is_tagged(from_gateway, PstnCtx) of
	true ->
	    logger:log(normal, "~s: pstnproxy: Allowing request to destination '~p' from PSTN gateway",
		       [YxaCtx#yxa_ctx.app_logtag, PstnCtx#pstn_ctx.destination]),
	    {ok, true, PstnCtx};
	false ->
	    case PstnCtx#pstn_ctx.destination of
		pstn ->
		    authorize_user_call_to_pstn(Request, YxaCtx, PstnCtx);
		sip ->
		    case PstnCtx#pstn_ctx.stale_auth of
			true ->
			    THandler = YxaCtx#yxa_ctx.thandler,
			    transactionlayer:send_challenge(THandler, proxy, _Stale = true, _RetryAfter = none),
			    {ok, false, PstnCtx};
			false ->
			    {ok, true, PstnCtx}
		    end
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, PstnCtx) ->
%%            {ok, Verdict, NewPstnCtx}
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%%            Verdict = true | false
%%
%% @doc     Part of authorize_proxying/4 - the destination is PSTN and
%%          we need to make sure the number is permissible for the
%%          originating user.
%% @end
%%--------------------------------------------------------------------
authorize_user_call_to_pstn(Request, YxaCtx, PstnCtx) ->
    DstNumber =
	case local:rewrite_potn_to_e164(PstnCtx#pstn_ctx.called_number) of
	    error -> PstnCtx#pstn_ctx.called_number;
	    N when is_list(N) -> N
	end,
    Class =
	case DstNumber of
	    undefined ->
		undefined;
	    _ ->
		{ok, Classdefs} = yxa_config:get_env(classdefs),
		{ok, Class1} = sipauth:classify_number(DstNumber, Classdefs),
		Class1
	end,
    {ok, UnauthClasses} = yxa_config:get_env(sipauth_unauth_classlist),
    User = PstnCtx#pstn_ctx.user,
    PstnCtx1 =
	PstnCtx#pstn_ctx{dst_number = DstNumber,
			 dst_class  = Class
			},

    case DstNumber of
	undefined ->
	    FromTag = (catch sipheader:get_tag(keylist:fetch('from', Request#request.header))),
	    ToTag = (catch sipheader:get_tag(keylist:fetch('to', Request#request.header))),
	    case Request#request.method of
                "BYE" when is_list(FromTag), is_list(ToTag) ->
                    logger:log(normal, "~s: pstnproxy: Allowing BYE request to PSTN gateway without trying to "
			       "determine PSTN number destination", [YxaCtx#yxa_ctx.app_logtag]),
		    %% BYEs from IP to PSTN are sent to the Contact used by the gateway. The Contact may
		    %% include the PSTN number, or it may not - it depends completely on the gateway.
		    %% Better not make any assumptions. XXX check that there is a to-tag too?
		    {ok, true, PstnCtx1};
		"INVITE" when is_list(FromTag), is_list(ToTag) ->
		    {ok, AllowReINVITE} = yxa_config:get_env(pstnproxy_allow_reinvite_to_pstn_dst),
		    case AllowReINVITE of
			true ->
			    logger:log(normal, "~s: pstnproxy: Allowing re-INVITE to PSTN gateway",
				      [YxaCtx#yxa_ctx.app_logtag]),
			    {ok, true, PstnCtx1};
			false ->
			    logger:log(normal, "~s: pstnproxy: Disallowing re-INVITE to PSTN gateway",
				      [YxaCtx#yxa_ctx.app_logtag]),
			    {ok, false, PstnCtx1}
		    end;
		_ ->
		    logger:log(normal, "~s: pstnproxy: Disallowing request to PSTN gateway because "
			       "no PSTN number destination could be determined", [YxaCtx#yxa_ctx.app_logtag]),
		    transactionlayer:send_response_handler(YxaCtx#yxa_ctx.thandler, 403, "Forbidden", []),
		    {ok, false, PstnCtx1}
	    end;
	_ when is_list(DstNumber) ->
	    case lists:member(Class, UnauthClasses) of
		true ->
		    logger:log(normal, "~s: pstnproxy: Allowing request from user ~p to number available to anyone "
			       "(~s, class '~p')", [YxaCtx#yxa_ctx.app_logtag, User, DstNumber, Class]),
		    {ok, true, PstnCtx1};
		false when is_list(User) ->
		    case local:is_allowed_pstn_dst(User, DstNumber, Request#request.header, Class) of
			true ->
			    %% Ok, now just check that the authorization isn't stale (or is allowed to be stale)
			    Verdict = check_auth_for_allowed_pstn_dst(Request, YxaCtx, PstnCtx1),
			    {ok, Verdict, PstnCtx1};
			false ->
			    logger:log(normal, "~s: pstnproxy: User ~p NOT allowed request to number ~s, class '~p'",
				       [YxaCtx#yxa_ctx.app_logtag, User, DstNumber, Class]),
			    transactionlayer:send_response_handler(YxaCtx#yxa_ctx.thandler, 403, "Forbidden", []),
			    {ok, false, PstnCtx1}
		    end;
		false when User == undefined ->
		    Verdict = check_unauth_request_to_pstn_dst(Request, YxaCtx, PstnCtx1),
		    {ok, Verdict, PstnCtx1}
	    end
    end.


%% part of authorize_user_call_to_pstn/3
%% Returns: true | false
check_auth_for_allowed_pstn_dst(Request, YxaCtx, PstnCtx) ->
    Method = Request#request.method,

    StaleOK = (Method /= "INVITE"),
    #pstn_ctx{user       = User,
	      stale_auth = IsStale,
	      dst_number = DstNumber,
	      dst_class  = DstClass
	     } = PstnCtx,
    if
	IsStale, StaleOK ->
	    logger:log(normal, "~s: pstnproxy: Allowing ~s request from user ~p to number ~s, class '~p' "
		       "(stale auth)", [YxaCtx#yxa_ctx.app_logtag, Method, User, DstNumber, DstClass]),
	    true;
	IsStale ->
	    logger:log(normal, "~s: pstnproxy: Challenging ~s request from user ~p to number ~s, class '~p' "
		       "(stale auth)", [YxaCtx#yxa_ctx.app_logtag, Method, User, DstNumber, DstClass]),
	    transactionlayer:send_challenge(YxaCtx#yxa_ctx.thandler, proxy, _Stale = true, _RetryAfter = none),
	    false;
	true ->
	    logger:log(normal, "~s: pstnproxy: Allowing request from user ~p to number ~s, class '~p'",
		       [YxaCtx#yxa_ctx.app_logtag, User, DstNumber, DstClass]),
	    true
    end.

%% part of authorize_user_call_to_pstn/3
%% Returns: true | false
check_unauth_request_to_pstn_dst(Request, YxaCtx, PstnCtx) ->
    #pstn_ctx{dst_number = DstNumber,
	      dst_class  = DstClass
	     } = PstnCtx,

    Method = Request#request.method,
    {ok, ChallengeBYE} = yxa_config:get_env(pstnproxy_challenge_bye_to_pstn_dst),
    Allow = (Method == "BYE") andalso (ChallengeBYE == false),

    case Allow of
	true ->
	    logger:log(normal, "~s: pstnproxy: Allowing unauthenticated ~s request to number ~p, class '~p'",
		       [YxaCtx#yxa_ctx.app_logtag, Method, DstNumber, DstClass]),
	    true;
	false ->
	    logger:log(normal, "~s: pstnproxy: Requesting authorization for destination number ~p, class '~p'",
		       [YxaCtx#yxa_ctx.app_logtag, DstNumber, DstClass]),
	    Stale = PstnCtx#pstn_ctx.stale_auth,
	    transactionlayer:send_challenge(YxaCtx#yxa_ctx.thandler, proxy, Stale, _RetryAfter = none),
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    (Hostname) ->
%%            true  |
%%            false
%%
%%            Hostname = string()
%%
%% @doc     Check if given hostname matches one of ours, or one of our
%%          IP addresses.
%% @end
%%--------------------------------------------------------------------
is_localhostname(Hostname) ->
    LChost = http_util:to_lower(Hostname),
    {ok, MyHostnames} = yxa_config:get_env(myhostnames),
    case lists:member(LChost, MyHostnames) of
	true ->
	    true;
	false ->
	    lists:member(LChost, siphost:myip_list())
    end.


%%--------------------------------------------------------------------
%% @spec    (Hostname) ->
%%            true  |
%%            false
%%
%%            Hostname = string()
%%
%% @doc     Check if given hostname matches one of our PSTN gateways
%%          hostnames.
%% @end
%%--------------------------------------------------------------------
is_pstngateway(Hostname) ->
    LChost = http_util:to_lower(Hostname),
    {ok, MyHostnames} = yxa_config:get_env(pstngatewaynames),
    lists:member(LChost, MyHostnames).


%%--------------------------------------------------------------------
%% @spec    (Type, Method, Header, Dst, PstnCtx) ->
%%            NewHeader
%%
%%            Type    = sip | pstn
%%            Method  = string() "SIP method of Request (for easy matching)"
%%            Header  = #keylist{}
%%            Dst     = #sipurl{}
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%%            NewHeader = #keylist{}
%%
%% @doc     If configured to, add Remote-Party-Id information about
%%          caller to this request before it is sent to a PSTN
%%          gateway. Useful to get proper caller-id.
%% @end
%%--------------------------------------------------------------------
add_caller_identity(pstn, "INVITE", Header, Dst, PstnCtx) when is_record(Dst, sipurl) ->
    case yxa_config:get_env(remote_party_id) of
	{ok, true} ->
	    case is_list(PstnCtx#pstn_ctx.user) of
		true ->
		    case local:get_remote_party_number(PstnCtx#pstn_ctx.user, Header,
						       Dst, Dst#sipurl.host) of
			{ok, RPI, Number} when is_record(RPI, contact), is_list(Number) ->
			    RemotePartyId = contact:print(RPI),
			    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
			    NewHeader1 = keylist:set("Remote-Party-Id", [RemotePartyId], Header),
			    logger:log(debug, "P-Preferred-Identity: ~s", ["tel:" ++ Number]),
			    keylist:set("P-Preferred-Identity", ["tel:" ++ Number], NewHeader1);
			none ->
			    %% Add RPI information saying to not show any caller id, in case the From:
			    %% contains something the gateway interprets as a phone number when it shouldn't
			    logger:log(debug, "Remote-Party-Id: Blocking Caller-Id for third party user "
				       "or user without number to avoid incorrect/spoofed A-number in PSTN"),
			    block_remote_party_id(Header)
		    end;
		false ->
		    case is_tagged(from_gateway, PstnCtx) of
			true ->
			    %% trust our gateways, XXX should be local hook?
			    Header;
			false ->
			    block_remote_party_id(Header)
		    end
	    end;
	{ok, false} ->
	    Header
    end;

add_caller_identity(sip, "INVITE", Header, _Dst, PstnCtx) ->
    case yxa_config:get_env(remote_party_id) of
	{ok, true} ->
	    case is_tagged(from_gateway, PstnCtx) of
		true ->
		    {_, FromURL} = sipheader:from(Header),
		    case local:get_remote_party_name(FromURL#sipurl.user, FromURL) of
			{ok, DisplayName} when is_list(DisplayName) ->
			    Parameters = [{"party", "calling"}, {"screen", "yes"}, {"privacy", "off"}],
			    RPI = contact:new(DisplayName, FromURL, Parameters),
			    RemotePartyId = contact:print(RPI),
			    logger:log(debug, "Remote-Party-Id: ~s", [RemotePartyId]),
			    keylist:set("Remote-Party-Id", [RemotePartyId], Header);
			none ->
			    Header
		    end;
		false ->
		    %% XXX perhaps it would be wise to remove any RPI headers for requests
		    %% going from SIP to SIP?
		    Header
	    end;
	{ok, false} ->
	    Header
    end;

add_caller_identity(_Type, _Method, Header, _Dst, _PstnCtx) when is_record(Header, keylist) ->
    %% non-INVITE probably
    Header.

%%--------------------------------------------------------------------
%% @spec    (Header) ->
%%            NewHeader
%%
%%            Header = #keylist{}
%%
%%            NewHeader = #keylist{}
%%
%% @doc     Remove any present P-Preferred-Identity header, and add an
%%          Anonymous Remote-Party-Id.
%% @end
%%--------------------------------------------------------------------
block_remote_party_id(Header) ->
    Parameters = [{"party", "calling"}, {"screen", "yes"}, {"privacy", "on"}],
    RPURI = sipurl:new([{host, siprequest:myhostname()}, {param, []}]),
    RPI = contact:print( contact:new("Anonymous", RPURI, Parameters) ),
    Header1 = keylist:set("Remote-Party-Id", [RPI], Header),
    keylist:delete("P-Preferred-Identity", Header1).


%%--------------------------------------------------------------------
%% @spec    (OldURL, NewURL) ->
%%            URL
%%
%%            OldURL = sipurl()
%%            NewURL = sipurl()
%%
%%            URL = #sipurl{}
%%
%% @doc     Whenever we have constructed a brand new URL to use as a
%%          destination, we can use this function to easily make sure
%%          we didn't downgrade SIPS to SIP (which RFC3261 forbids).
%% @end
%%--------------------------------------------------------------------
restore_sips_proto(OldURL, NewURL) when is_record(OldURL, sipurl), is_record(NewURL, sipurl) ->
    case {OldURL#sipurl.proto, NewURL#sipurl.proto} of
	{"sips", "sip"} ->
	    sipurl:set([{proto, "sips"}], NewURL);
	_ ->
	    NewURL
    end.


%%--------------------------------------------------------------------
%% @spec    (Label, PstnCtx) -> true | false
%%
%%            Label   = term()
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%% @doc     Check if Label is one of the tags in PstnCtx.
%% @end
%%--------------------------------------------------------------------
is_tagged(Label, PstnCtx) when is_atom(Label), is_record(PstnCtx, pstn_ctx) ->
    lists:member(Label, PstnCtx#pstn_ctx.tags).


%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx, Dst, AppData) ->
%%            term() "result of local:start_sippipe/4"
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%            Dst     = [#sipdst{}] | route | #sipurl{}
%%            PstnCtx = #pstn_ctx{} "context for this request"
%%
%% @doc     Start a sippipe unless we are currently unit testing.
%% @end
%%--------------------------------------------------------------------
start_sippipe(Request, YxaCtx, Dst, AppData) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    case autotest:is_unit_testing(?MODULE, testing_sippipe) of
	{true, {Res, Pid}} when is_pid(Pid) ->
	    Pid ! {start_sippipe, {Request, YxaCtx, Dst, AppData}},
	    Res;
	{true, Res} ->
	    Res;
	false ->
	    local:start_sippipe(Request, YxaCtx, Dst, AppData)
    end.


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
    UserDb =
	[{user, [
		 {name, "autotest1"},
		 {password, "secret"},
		 {classes, [internal,national,mobile]},
		 {addresses, ["sip:autotest1@example.org", "sip:234599@example.org"]}
		]},
	 {user, [
		 {name, "autotest2"},
		 {addresses, ["sip:autotest2@example.org"]}
		]}

	],
    ok = sipuserdb_test:init(UserDb),

    ExtraCfg = [
		{userdb_modules,	[sipuserdb_test]},
		{myhostnames,		["autotest.example.org"]},
		{internal_to_e164,	[{"234599", "+46234599"}]}
	       ],
    yxa_test_config:init(pstnproxy, ExtraCfg),


    %% add_caller_identity(pstn, Method, Request, Dst, PstnCtx)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add_caller_identity/5 - pstn 1.0"),
    ACI_Dst1 = sipurl:parse("sip:some.gw.test.example.org"),
    yxa_test_config:set(remote_party_id, true),
    ACI_Ctx1 = #pstn_ctx{user = "autotest1"},
    ACI_Header1 = keylist:from_list([{"Remote-Party-Id", ["untrusted"]}]),

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 1.1"),
    %% test normal case
    ACI_Header1_Res = add_caller_identity(pstn, "INVITE", ACI_Header1, ACI_Dst1, ACI_Ctx1),

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 1.2"),
    %% verify
    ["<sip:+46234599@some.gw.test.example.org>;party=calling;screen=yes;privacy=off"] =
	keylist:fetch("Remote-Party-Id", ACI_Header1_Res),


    autotest:mark(?LINE, "add_caller_identity/5 - pstn 2.0"),
    ACI_Ctx2 = #pstn_ctx{user = "autotest2"},

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 2.1"),
    %% test with user that has no number
    ACI_Header2_Res = add_caller_identity(pstn, "INVITE", ACI_Header1, ACI_Dst1, ACI_Ctx2),

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 2.2"),
    %% verify
    ["\"Anonymous\" <sip:autotest.example.org>;party=calling;screen=yes;privacy=on"] =
	keylist:fetch("Remote-Party-Id", ACI_Header2_Res),


    autotest:mark(?LINE, "add_caller_identity/5 - pstn 3.0"),
    ACI_Ctx3 = #pstn_ctx{user = undefined,
			 tags = [from_gateway]
			},
    ACI_Header3 = keylist:from_list([{"Remote-Party-Id", ["already-set"]},
				     {"P-Preferred-Identity", ["already-set-too"]}
				    ]),
    autotest:mark(?LINE, "add_caller_identity/5 - pstn 3.1"),
    %% test with INVITE from gateway
    ACI_Header3_Res = add_caller_identity(pstn, "INVITE", ACI_Header3, ACI_Dst1, ACI_Ctx3),

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 3.2"),
    %% verify
    ["already-set"] = keylist:fetch("Remote-Party-Id", ACI_Header3_Res),


    autotest:mark(?LINE, "add_caller_identity/5 - pstn 4.0"),
    ACI_Ctx4 = #pstn_ctx{user = unknown},

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 4.1"),
    %% test not from gateway, no username
    ACI_Header4_Res = add_caller_identity(pstn, "INVITE", ACI_Header3, ACI_Dst1, ACI_Ctx4),

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 4.2"),
    %% verify
    ["\"Anonymous\" <sip:autotest.example.org>;party=calling;screen=yes;privacy=on"] =
	keylist:fetch("Remote-Party-Id", ACI_Header4_Res),
    [] = keylist:fetch("P-Preferred-Identity", ACI_Header4_Res),

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 5.1"),
    %% test with Remote-Party-Id disabled
    yxa_test_config:set(remote_party_id, false),
    ACI_Header5 = keylist:from_list([]),
    ACI_Header5_Res = add_caller_identity(pstn, "INVITE", ACI_Header5, ACI_Dst1, ACI_Ctx4),

    autotest:mark(?LINE, "add_caller_identity/5 - pstn 5.2"),
    %% verify
    [] = keylist:fetch("Remote-Party-Id", ACI_Header5_Res),


    %% add_caller_identity(sip, Method, Request, Dst, PstnCtx)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "add_caller_identity/5 - sip 1.0"),
    ACI_SIP_Dst1 = sipurl:parse(siprequest:myhostname()),
    yxa_test_config:set(remote_party_id, true),
    ACI_SIP_Ctx1 = #pstn_ctx{user = undefined,
			     tags = [from_gateway]},
    ACI_SIP_Header1 = keylist:from_list([{"Remote-Party-Id", ["from-gateway"]},
					 {"From", ["Gateway <sip:foo@pstn.example.org>"]}
					]),

    autotest:mark(?LINE, "add_caller_identity/5 - sip 1.1"),
    %% test normal case
    ACI_SIP_Header1_Res = add_caller_identity(sip, "INVITE", ACI_SIP_Header1, ACI_SIP_Dst1, ACI_SIP_Ctx1),

    autotest:mark(?LINE, "add_caller_identity/5 - sip 1.2"),
    %% verify
    ["from-gateway"] = keylist:fetch("Remote-Party-Id", ACI_SIP_Header1_Res),


    autotest:mark(?LINE, "add_caller_identity/5 - sip 2.0"),
    ACI_SIP_Dst2 = sipurl:parse(siprequest:myhostname()),
    yxa_test_config:set(remote_party_id, true),
    ACI_SIP_Ctx2 = #pstn_ctx{user = undefined,
			     tags = []},
    ACI_SIP_Header2 = keylist:from_list([{"Remote-Party-Id", ["from-SIP-to-SIP"]},
					 {"From", ["Sipper <sip:foo@example.org>"]}
					]),

    autotest:mark(?LINE, "add_caller_identity/5 - sip 2.2"),
    %% test to SIP not from PSTN gateway
    ACI_SIP_Header2_Res = add_caller_identity(sip, "INVITE", ACI_SIP_Header2, ACI_SIP_Dst2, ACI_SIP_Ctx2),

    autotest:mark(?LINE, "add_caller_identity/5 - sip 2.2"),
    %% verify
    ["from-SIP-to-SIP"] = keylist:fetch("Remote-Party-Id", ACI_SIP_Header2_Res),

    autotest:mark(?LINE, "add_caller_identity/3 - sip 3"),
    %% test with Remote-Party-Id disabled, header should be untouched
    yxa_test_config:set(remote_party_id, false),
    ACI_SIP_Header3 = keylist:from_list([{"Remote-Party-Id", ["set"]},
					 {"Autotest", ["true"]}
					]),
    ACI_SIP_Header3 = add_caller_identity(sip, "INVITE", ACI_SIP_Header3, ACI_SIP_Dst1, ACI_SIP_Ctx2),



    autotest:mark(?LINE, "add_caller_identity/3 - other 1"),
    %% test with Remote-Party-Id disabled, header should be untouched
    yxa_test_config:set(remote_party_id, true),
    ACI_Other_Header3 = keylist:from_list([{"Remote-Party-Id", ["set"]},
					 {"Autotest", ["true"]}
					]),
    ACI_Other_Header3 = add_caller_identity(sip, "non-INVITE", ACI_Other_Header3, ACI_SIP_Dst1, ACI_SIP_Ctx2),



    pstnproxy_test:test(),

    ok.
