%% module to test speed of keylist optimizations
-module(speedtest_keylist).

-compile(export_all).

-include("siprecords.hrl").

response() ->
    response(keylist, 25, 1000).

fprof_response() ->
    fprof:trace(start),
    response(keylist, 5, 100),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse([{dest, "parse-prof.txt"}]).


response(KeylistMod, N, M) ->
    io:format("Measuring time to create ~p*~p responses using module : ~p~n~n", [N, M, KeylistMod]),

    Times = [response_run(KeylistMod, M) || _ <- lists:seq(1, N)],

    io:format("~nBest result : ~.2f ms~n~n", [lists:min(Times)]).

response_run(KeylistMod, Iterations) ->
    StartT = erlang:now(),
    {ok, ParseMS, RespondMS} = response_run2(KeylistMod, Iterations, 0, 0),
    StopT = erlang:now(),

    MS = timer:now_diff(StopT, StartT) / 1000,
    io:format("  response_run : parse ~10.2f, respond ~10.2f, total ~10.2f~n", [ParseMS, RespondMS, MS]),
    MS.


response_run2(_KeylistMod, 0, ParseMS, RespondMS) ->
    {ok, ParseMS, RespondMS};
response_run2(_KeylistMod, Iterations, ParseMS, RespondMS) ->

    StartT1 = erlang:now(),
    Msg =
	<<"REGISTER sip:t1.example.org SIP/2.0\r\n"
	 "Via: SIP/2.0/YXA-TEST 192.0.2.2:2050;branch=z9hG4bK-voklcjagcnab;rport\r\n"
	 "From: \"Test\" <sip:ft@t1.example.org>;tag=f7t0gdev57\r\n"
	 "To: \"Test\" <sip:ft@t1.example.org>\r\n"
	 "Call-ID: 3c2670090f86-ys08fygz4fu2@aa-bb-ccc-dde\r\n"
	 "CSeq: 10 REGISTER\r\n"
	 "Max-Forwards: 70\r\n"
	 "Contact: <sip:192.0.2.2:2050;transport=tcp;line=almbmxia>;q=1.0\r\n"
	 "Authorization: Digest username=\"ft\",realm=\"yxa.sip.su.se\",\r\n"
	 "  nonce=\"a000000000091027bed1b8b503c0c22f\",uri=\"sip:t1.example.org\",response=\"0026b0000000000b5de15997\r\n"
	 "WWW-Contact: <https://192.0.2.2:443>\r\n"
	 "WWW-Contact: <http://192.0.2.2:80>\r\n"
	 "X-Real-IP: 192.0.2.2\r\n"
	 "Allow-Events: dialog\r\n"
	 "Supported: gruu\r\n"
	 "P-NAT-Refresh: 15\r\n"
	 "User-Agent: snom105-3.56p\r\n"
	 "Expires: 86400\r\n"
	 "Content-Length: 0\r\n"
	 "\r\n"
	 >>,

    Request = sippacket:parse(Msg, none),
    StopT1 = erlang:now(),
    This_ParseMS = timer:now_diff(StopT1, StartT1) / 1000,

%%    io:format("REQUEST : ~n~p~n~n", [Request]),
%%    throw(foo),

    StartT2 = erlang:now(),
    ExpectedResponse =
	<<"SIP/2.0 480 Temporarily unavailable\r\n"
	 "Via: SIP/2.0/YXA-TEST 192.0.2.2:2050;branch=z9hG4bK-voklcjagcnab;rport\r\n"
	 "From: \"Test\" <sip:ft@t1.example.org>;tag=f7t0gdev57\r\n"
	 "To: \"Test\" <sip:ft@t1.example.org>\r\n"
	 "Call-ID: 3c2670090f86-ys08fygz4fu2@aa-bb-ccc-dde\r\n"
	 "CSeq: 10 REGISTER\r\n"
	 "Content-Length: 0\r\n"
	 "Retry-After: 180\r\n"
	 "\r\n">>,
    ExpectedLength = size(ExpectedResponse),
    BinMsg = construct_notavail(Request#request.header),
    ExpectedLength = size(BinMsg),
    StopT2 = erlang:now(),
    This_RespondMS = timer:now_diff(StopT2, StartT2) / 1000,

    response_run2(_KeylistMod, Iterations - 1, ParseMS + This_ParseMS, RespondMS + This_RespondMS).

construct_notavail(Header) ->
%%    io:format("HEADER IN : ~p~n~n", [Header]),
    ExtraHeaders = [{"Retry-After", ["180"]}],
    Response1 = #response{status = 480,
			  reason = "Temporarily unavailable",
			  header = siprequest:standardcopy(Header, ExtraHeaders)
			 },
%%    io:format("RESPONSE : ~p~n~n", [Response1]),
%%    throw(foo),

    Body = <<>>,
    Response = siprequest:set_response_body(Response1, Body),

    Header2 = siprequest:fix_content_length(Response#response.header, Body),
    BinLine1 = list_to_binary(["SIP/2.0 ", integer_to_list(Response#response.status), " ",
			       Response#response.reason]),
    siprequest:binary_make_message(BinLine1, Header2, Body).
