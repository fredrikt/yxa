%%%-------------------------------------------------------------------
%%% File    : siphost.erl
%%% @author   Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @doc      Network interface status/address retreival functions.
%%%
%%% @since    15 Nov 2002 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%% @end
%%%-------------------------------------------------------------------
-module(siphost).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([myip/0,
	 myip_list/0,

	 makeip/1,

	 test/0
	]).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () ->
%%            Addr
%%
%%            Addr = string()
%%
%% @doc     Get one IP address for this host. Currently, we default to
%%          the first one returned by get_iplist().
%% @end
%%--------------------------------------------------------------------
myip() ->
    case get_iplist() of
	[A | _] ->
	    A;
	[] ->
	    get_defaultaddr()
    end.

%%--------------------------------------------------------------------
%% @spec    () ->
%%            Addresses
%%
%%            Addresses = [string()]
%%
%% @doc     Get all IP addresses of this host. Exclude loopback and
%%          addresses of interfaces that are down. Note : XXX make
%%          this return all addresses, currently IPv6 addresses are
%%          not returned!
%% @end
%%--------------------------------------------------------------------
myip_list() ->
    case get_iplist() of
	[] ->
	    [get_defaultaddr()];
	L ->
	    lists:usort(L)
    end.

%%--------------------------------------------------------------------
%% @spec    (IPTuple) ->
%%            Addr
%%
%%            IPTuple = tuple() "IPv4 or IPv6 address as tuple (e.g. {192, 0, 2, 45} or {2001, ..., 1})."
%%
%%            Addr = string()
%%
%% @doc     Turn a v4 or v6 address represented as a tuple into a
%%          string representation.
%% @end
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
%% @spec    () ->
%%            Addresses
%%
%%            Addresses = [string()]
%%
%% @doc     Get the addresses of all interfaces that have global
%%          addresses, and which are 'up'.
%% @end
%%--------------------------------------------------------------------
get_iplist() ->
    case yxa_config:get_env(myips) of
	{ok, List} ->
	    List;
	none ->
	    {ok, IfList} = inet:getiflist(),
	    F = fun(If) ->
			{ok, B} = inet:ifget(If, [addr, flags]),
			B
		end,
	    IfData = [F(If) || If <- IfList],
	    get_ifaddrs(IfData)
    end.

%%--------------------------------------------------------------------
%% @spec    (IfData) ->
%%            Addresses
%%
%%            IfList = [List]
%%            List   = [term()] "interface data"
%%
%%            Addresses = [string()]
%%
%% @doc     Get the addresses of all interfaces for which we have been
%%          given data that have global addressses, and which are
%%          'up'.
%% @end
%%--------------------------------------------------------------------
get_ifaddrs(IfData) when is_list(IfData) ->
    get_ifaddrs(IfData, []).

get_ifaddrs([H | T], Res) when is_list(H) ->
    case get_ifaddrs2(H) of
	ignore ->
	    %% Ignore interface
	    get_ifaddrs(T, Res);
	This ->
	    get_ifaddrs(T, [This | Res])
    end;
get_ifaddrs([], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% @spec    (If) ->
%%            ignore | Address
%%
%%            If = [tuple()] "result of inet:ifget/2"
%%
%%            Address = string()
%%
%% @doc     Get IP address of an interface, unless the interface is to
%%          be ignored.
%% @end
%%--------------------------------------------------------------------
get_ifaddrs2(IfData) when is_list(IfData) ->
    {value, {flags, Flags}} = lists:keysearch(flags, 1, IfData),
    AddrT = case lists:keysearch(addr, 1, IfData) of
		{value, {addr, AddrTuple1}} -> AddrTuple1;
		_ -> none
	    end,
    case usable_if(Flags, AddrT) of
	true ->
	    makeip(AddrT);
	false ->
	    ignore
    end.

%%--------------------------------------------------------------------
%% @spec    (Flags, AddrT) -> true | false
%%
%%            Flags = [atom()]
%%            AddrT = tuple() | none
%%
%% @doc     Interface must be up and not have an address known to only
%%          work locally to be considered usable.
%% @end
%%--------------------------------------------------------------------
usable_if(_Flags, none) ->
    %% Interface has no address, might happen on BSD
    false;
usable_if(Flags, AddrT) ->
    case lists:member(up, Flags) of
	true ->
	    %% Check address
	    case AddrT of
		{127, _, _, _} ->
		    %% IPv4 real loopback address - ignore
		    false;
		{0, 0, 0, 0, 0, 0, 0, 1} ->
		    %% IPv6 localhost address - ignore
		    false;
		_ ->
		    true
	    end;
	false ->
	    false
    end.

%%--------------------------------------------------------------------
%% @spec    () ->
%%            DefaultAddr
%%
%%            DefaultAddr = string()
%%
%% @doc     Return host default address. Note : XXX look for loopback
%%          interface and use that address, if found. Don't assume
%%          loopback is 127.0.0.1.
%% @end
%%--------------------------------------------------------------------
get_defaultaddr() ->
    "127.0.0.1".


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

    %% test myip()
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "myip/0 - 1.1"),
    %% test that we get exactly one string back
    MyIP = myip(),
    autotest:mark(?LINE, "myip/0 - 1.2"),
    true = is_list(MyIP),
    autotest:mark(?LINE, "myip/0 - 1.3"),
    true = is_integer(hd(MyIP)),

    autotest:mark(?LINE, "myip_list/0 - 1.1"),
    %% test that we get a list of strings back
    MyIPList = myip_list(),
    autotest:mark(?LINE, "myip_list/0 - 1.2"),
    [MyIP | _] = MyIPList,

    %% test makeip(IPtuple)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "makeip/1 - 1"),
    %% test IPv4 tuple
    "192.0.2.1" = makeip({192, 0, 2, 1}),

    autotest:mark(?LINE, "makeip/1 - 2"),
    %% test IPv6 tuple
    "[2001:6b0:5:987::1]" = makeip({8193, 1712, 5, 2439, 0, 0, 0, 1}),


    %% test get_ifaddrs2(IfData)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ifaddrs2/1 - 1.1"),
    %% make sure we ignore loopback
    ignore = get_ifaddrs2([{addr, {127, 0, 0, 1}},
			   {flags, [up, loopback, running]}]),

    autotest:mark(?LINE, "get_ifaddrs2/1 - 1.2"),
    %% make sure we ignore strange loopback (not on loopback interface)
    ignore = get_ifaddrs2([{addr, {127, 1, 0, 0}},
			   {flags, [up, broadcast, multicast, running]}]),

    autotest:mark(?LINE, "get_ifaddrs2/1 - 1.3"),
    %% make sure we ignore IPv6 localhost
    ignore = get_ifaddrs2([{addr, {0, 0, 0, 0, 0, 0, 0, 1}},
			   {flags, [up, running]}]),

    autotest:mark(?LINE, "get_ifaddrs2/1 - 2"),
    %% make sure we don't ignore a "global loopback"
    "192.0.2.11" = get_ifaddrs2([{addr, {192, 0, 2, 11}},
				 {flags, [up, loopback, running]}]),


    autotest:mark(?LINE, "get_ifaddrs2/1 - 3"),
    %% make sure we ignore interface that is not 'up'
    ignore = get_ifaddrs2([{addr, {192, 0, 2, 12}},
			   {flags, [broadcast, multicast]}]),

    autotest:mark(?LINE, "get_ifaddrs2/1 - 4"),
    %% test normal case
    "192.0.2.12" = get_ifaddrs2([{addr, {192, 0, 2, 12}},
				 {flags, [broadcast, multicast, up]}]),

    autotest:mark(?LINE, "get_ifaddrs2/1 - 5"),
    %% test normal case IPv6
    "[2001:6b0:5:987::1]" = get_ifaddrs2([{addr, {8193, 1712, 5, 2439, 0, 0, 0, 1}},
					  {flags, [broadcast, multicast, up]}]),


    %% test get_ifaddrs(IfData)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ifaddrs/1 - 1"),
    %% test mixed list with all kinds of interface data structures in it
    ["192.0.2.11", "192.0.2.12"] = get_ifaddrs([[{addr, {192, 0, 2, 11}},		%% valid
						 {flags, [broadcast, multicast, up]}],
						[{addr, {127, 0, 0, 1}},		%% invalid (127.0.0.1)
						 {flags, [broadcast, multicast, up]}],
						[{addr, {192, 0, 2, 10}},		%% invalid (not 'up')
						 {flags, [broadcast, multicast]}],
						[{addr, {192, 0, 2, 12}},		%% valid (loopback interface)
						 {flags, [loopback, up]}],
						[{flags, [loopback, up]}]		%% invalid (no address)
					       ]),

    ok.
