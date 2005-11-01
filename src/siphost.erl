%%%-------------------------------------------------------------------
%%% File    : siphost.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: Network interface status/address retreival functions.
%%%
%%% Created : 15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%-------------------------------------------------------------------
-module(siphost).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([myip/0,
	 myip_list/0,

	 makeip/1
	]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: myip()
%% Descrip.: Get one IP address for this host. Currently, we default
%%           to the first one returned by get_iplist().
%% Returns : Addr = string()
%%--------------------------------------------------------------------
myip() ->
    case get_iplist() of
	[A | _] ->
	    A;
	[] ->
	    %% XXX look for loopback interface and use that address,
	    %% if found. Don't assume loopback is 127.0.0.1.
	    "127.0.0.1"
    end.

%%--------------------------------------------------------------------
%% Function: myip_list()
%% Descrip.: Get all IP addresses of this host. Exclude loopback and
%%           addresses of interfaces that are down.
%% Returns : Addresses = list() of string()
%% Note    : XXX make this return all addresses, currently IPv6
%%           addresses are not returned!
%%--------------------------------------------------------------------
myip_list() ->
    case get_iplist() of
	[] ->
	    ["127.0.0.1"];
	L ->
	    L
    end.

%%--------------------------------------------------------------------
%% Function: makeip(IPTuple)
%%           IPTuple = tuple(), IPv4 or IPv6 address as tuple (e.g.
%%                     {192, 0, 2, 45} or {2001, ..., 1}).
%% Descrip.: Turn a v4 or v6 address represented as a tuple into
%%           a string representation.
%% Returns : Addr = string()
%%--------------------------------------------------------------------
makeip({A1, A2, A3, A4}) ->
    integer_to_list(A1) ++ "." ++
	integer_to_list(A2) ++ "." ++
	integer_to_list(A3) ++ "." ++
	integer_to_list(A4);
makeip({A1, A2, A3, A4, A5, A6, A7, A8}) ->
    A = inet_parse:ntoa({A1, A2, A3, A4, A5, A6, A7, A8}),
    "[" ++ httpd_util:to_lower(A) ++ "]".


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: get_iplist()
%% Descrip.: Get the addresses of all interfaces that are not loopback
%%           interfaces, and which are 'up'.
%% Returns : Addresses = list() of string()
%%--------------------------------------------------------------------
get_iplist() ->
    {ok, If} = inet:getiflist(),
    get_if(If).

%%--------------------------------------------------------------------
%% Function: get_if(IfList)
%%           IfList = list() of string(), list of interface names
%% Descrip.: Get the addresses of all interfaces named in IfList that
%%           are not loopback interfaces, and which are 'up'.
%% Returns : Addresses = list() of string()
%%--------------------------------------------------------------------
get_if(L) ->
    get_if(L, []).

get_if([], Res) ->
    Res;

get_if([H | T], Res) ->
    {ok, B} = inet:ifget(H, [addr, flags]),
    {value, {flags, Flags}} = lists:keysearch(flags, 1, B),
    case usable_if(Flags) of
	false ->
	    %% Ignore unusable interfaces
	    get_if(T, Res);
	true ->
	    case lists:keysearch(addr, 1, B) of
		{value, {addr, Addr}} ->
		    get_if(T, [makeip(Addr) | Res]);
		_ ->
		    %% Interface has no address, might happen on BSD
		    get_if(T, Res)
	    end
    end.

%%--------------------------------------------------------------------
%% Function: usable_if(Flags)
%%           Flags = list() of atom()
%% Descrip.: Interface must be up and not loopback to be considered
%%           usable.
%% Returns : true | false
%%--------------------------------------------------------------------
usable_if(Flags) ->
    not lists:member(loopback, Flags)
	and lists:member(up, Flags).
