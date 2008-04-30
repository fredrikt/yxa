%%%-------------------------------------------------------------------
%%% File    : su_pstnproxy_policy.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Local functionality for Stockholm university (su.se).
%%%
%%% @since     4 Apr 2007 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(su_pstnproxy_policy).
%%-compile(export_all).

-export([
	 init/0,
	 is_allowed_pstn_dst/4
	]).

init() ->
    case yxa_config:get_env(yxa_appmodule) of
	{ok, pstnproxy} ->
	    %%su_pstnproxy_policy_server:start_link(),
	    Spec = {su_pstnproxy_policy_server,
		    {su_pstnproxy_policy_server, start_link, []},
		    permanent, infinity, supervisor, [su_pstnproxy_policy_server]},

	    case supervisor:start_child(whereis(sipserver_sup), Spec) of
		{ok, _} -> ok;
		{ok, _, _} -> ok;
		_ ->
		    throw('Failed starting su_pstnproxy_policy_server')
	    end;
	_ ->
	    ok
    end.


is_allowed_pstn_dst(User, ToNumber, _Header, Class) when is_list(User), is_list(ToNumber),
							 is_atom(Class) ->
    gen_server:call(su_pstnproxy_policy_server, {is_allowed, User, ToNumber, Class}).
