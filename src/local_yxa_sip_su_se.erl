%%%-------------------------------------------------------------------
%%% File    : local_yxa_sip_su_se.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Local functions used in yxa.sip.su.se YXA virual SIP
%%%	      hotel.
%%%
%%% @since    29 Jun 2008 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @hidden
%%%-------------------------------------------------------------------
-module(local_yxa_sip_su_se).

-export([
	 canonify_authusername/2
	]).

-include("siprecords.hrl").


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
canonify_authusername(Username, Header) when is_list(Username) ->
    case local:get_password_for_user(Username) of
	nomatch ->
	    case canonify_authusername_get_newusername(Username, Header) of
		{ok, NewUsername} ->
		    case local:get_password_for_user(NewUsername) of
			nomatch ->
			    Username;
			_Pw when is_list(_Pw) ->
			    logger:log(debug, "Local: Canonified username ~p -> ~p",
				       [Username, NewUsername]),
			    NewUsername
		    end;
		none ->
		    Username
	    end;
	_Pw when is_list(_Pw) ->
	    Username
    end;
canonify_authusername(Username, _Header) ->
    logger:log(normal, "Local: Could not canonify non-list Username : ~p", [Username]),
    undefined.

%% Part of canonify_authusername/2. Construct a new possible
%% username using the domain name from the From: header
canonify_authusername_get_newusername(Username, Header) ->
    case sipheader:from(Header) of
	{_DN, FromURL} when is_record(FromURL, sipurl) ->
	    Host = FromURL#sipurl.host,
	    NewUsername = Username ++ "@" ++ Host,
	    {ok, NewUsername};
	_ ->
	    none
    end.
