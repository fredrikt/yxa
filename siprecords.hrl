-record(request, {method, uri, header, body}).

-record(response, {status, reason, header, body}).

-record(via, {proto, host, port, param}).

-record(sipurl, {proto, user, pass, host, port, param}).
