-module(sipclient).
-export([dial/2]).

dial(Number, Dest) ->
    {ok, Socket} = gen_udp:open(0),
    Body = sdp:print(Dest),
    From = "\"9532\" <sip:9532@nada.kth.se>",
    To = "sip:" ++ Number ++ "@kth.se",
    {Megasec, Sec, Microsec} = now(),
    CallID = "2b6fb0320-" ++
	integer_to_list(Megasec) ++ "-" ++
	integer_to_list(Sec) ++ "-" ++
	integer_to_list(Microsec) ++ "-" ++
	"-23b6b0-2e3303331@" ++ siphost:myip(),
    Sendheader = [{"via", []},
		  {"From", [From]},
		  {"To", [To]},
		  {"Call-ID", [CallID]},
		  {"Content-Type", ["application/sdp"]},
		  {"Content-Length", [integer_to_list(length(Body))]},
		  {"CSeq", ["101 INVITE"]},
		  {"Expires", ["3600"]}],
    Url = sipurl:parse("sip:" ++ Number ++ "@kth.se"),
    {atomic, ok} =  database_call:insert_call_unique(CallID, answer, Sendheader, [self()]),
    siprequest:send_proxy_request(Socket,
				  {"INVITE",
				   Url,
				   Sendheader,
				   Body},
				   Url, []),
    true.
