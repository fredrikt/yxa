-record(sipsocket, {module, proto, pid, data}).

%% sipdst is the Transport Layer destination record.
-record(sipdst, {proto, addr, port, uri}).
-record(siporigin, {proto, addr, port, receiver, sipsocket}).
