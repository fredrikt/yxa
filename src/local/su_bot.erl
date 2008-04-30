%%%-------------------------------------------------------------------
%%% File    : su_bot.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      SU-specific (so far) things with a JEvent frontend.
%%% @since     1 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(su_bot).

-behaviour(yxa_app).

%%--------------------------------------------------------------------
%%% Standard YXA SIP-application callback functions
%%--------------------------------------------------------------------
-export([
	 init/0,
	 request/2,
	 response/2
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").
-include("sipsocket.hrl").


%%====================================================================
%% Behaviour functions
%% Standard YXA SIP-application callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () ->
%%            [Tables, Mode, SupData]
%%
%%            Tables  = [atom()] "remote mnesia tables the YXA startup sequence should make sure are available"
%%            Mode    = stateful
%%            SupData = {append, SupSpec} | none
%%            SupSpec = OTP supervisor child specification. Extra processes this application want the sipserver_sup to start and maintain.
%%
%% @doc     YXA applications must export an init/0 function.
%% @hidden
%% @end
%%--------------------------------------------------------------------
init() ->
    BotBackend = {su_botbackend, {su_botbackend, start_link, []},
		  permanent, 2000, worker, [su_botbackend]},
    Tables = [user, numbers],
    #yxa_app_init{mnesia_tables	= Tables,
		  sup_spec	= {append, [BotBackend]}
		 }.

%%--------------------------------------------------------------------
%% @spec    (Request, YxaCtx) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Request = #request{}
%%            YxaCtx  = #yxa_ctx{}
%%
%% @doc     YXA applications must export a request/2 function.
%% @end
%%--------------------------------------------------------------------
request(Request, YxaCtx) when is_record(Request, request), is_record(YxaCtx, yxa_ctx) ->
    THandler = YxaCtx#yxa_ctx.thandler,
    transactionlayer:send_response_handler(THandler, 481, "Call/Transaction Does Not Exist").

%%--------------------------------------------------------------------
%% @spec    (Response, YxaCtx) ->
%%            term() "Yet to be specified. Return 'ok' for now."
%%
%%            Request = #response{}
%%            YxaCtx  = #yxa_ctx{}
%%
%% @doc     YXA applications must export a response/2 function.
%% @end
%%--------------------------------------------------------------------
response(Response, YxaCtx) when is_record(Response, response), is_record(YxaCtx, yxa_ctx) ->
    logger:log(normal, "Botserver: Dropping out-of-dialog response '~p ~p'",
	       [Response#response.status, Response#response.reason]),
    true.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
