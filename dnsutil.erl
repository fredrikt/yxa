-module(dnsutil).
-export([siplookup/1]).

-include("inet_dns.hrl").

srvlookup(Name) ->
    case inet_res:nslookup(Name, in, srv) of
	{ok, Rec} ->
	    ParseSRV = fun(Entry) ->
			       Entry#dns_rr.data
			  end,
	    lists:map(ParseSRV, Rec#dns_rec.anlist);
	{error, nxdomain} ->
	    []
    end.

siplookup(Domain) ->
    case srvlookup("_sip._udp." ++ Domain) of
	[] ->
	    none;
	[{_, _, Port, Host} | _] ->
	    {Host, Port}
    end.
