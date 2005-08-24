-module(sipanswer).

-export([
	 start/5,
	 bounce/5,
	 control/3,
	 alaw_encode/1,
	 sine/1,
	 sine_gen/1,
	 testsine/0,
	 alaw_decode/1
	]).

printlist([]) ->
    true;
printlist([E | R]) ->
    io:format("~p~n", [alaw_decode(E)]),
    printlist(R).

testsine() ->
    List = sine(160),
    printlist(List).

alaw_decode(InByte) ->
    Byte = InByte bxor 16#55,
    T = (Byte band 15) bsl 4,
    Seg = (Byte band 16#70) bsr 4,
    T2 = case Seg of
	     0 ->
		 T + 8;
	     1 ->
		 T + 16#108;
	     _ ->
		 (T + 16#108) bsl (Seg -1)
	 end,
    T3 = T2 / 16384.0,
    case Byte band 16#80 of
	0 ->
	    T3;
	128 ->
	    -T3
    end.

alaw_search(Linear) when Linear =< 16#FF -> 0;
alaw_search(Linear) when Linear =< 16#1FF -> 1;
alaw_search(Linear) when Linear =< 16#3FF -> 2;
alaw_search(Linear) when Linear =< 16#7FF -> 3;
alaw_search(Linear) when Linear =< 16#FFF -> 4;
alaw_search(Linear) when Linear =< 16#1FFF -> 5;
alaw_search(Linear) when Linear =< 16#3FFF -> 6;
alaw_search(Linear) when Linear =< 16#7FFF -> 7;
alaw_search(_Linear) -> 8.

alaw_encode(Float) ->
    Sample1 = trunc(Float * 16384.0),
    {Mask, Sample} = if
		      Float >= 0 ->
			  {16#D5, Sample1};
		      true ->
			  {16#55, -Sample1}
		  end,
    Seg = alaw_search(Sample),
    Aval = Seg bsl 4,
    case Seg of
	8 ->
	    16#7F bxor Mask;
	_ when Seg < 2 ->
	    (Aval bor ((Sample bsr 4) band 15)) bxor Mask;
	_ ->
	    (Aval bor ((Sample bsr (Seg + 3)) band 15)) bxor Mask
    end.

sine(Num) ->
    lists:map(fun alaw_encode/1, sine_gen(160)).

sine_gen(0) ->
    [];
sine_gen(Num) ->
    [math:sin(Num/(160.0/9.0)*3.1416*2)|sine_gen(Num-1)].

sendack(Socket, Header, Url) ->
    {CSeqID, _CSeqMethod} = sipheader:cseq(Header),
    Sendheader = [{"Via", []},
		  {"From", keylist:fetch('from', Header)},
		  {"To", keylist:fetch('to', Header)},
		  {"Call-ID", keylist:fetch('call-id', Header)},
		  {"CSeq", [sipheader:cseq_print({CSeqID, "ACK"})]}],
    siprequest:send_proxy_request(Socket,
				  {"ACK", Url, Sendheader, ""}, Url, []).

recvresponse(Socket, Header, Url, Body, Status) ->
    io:format("~p ~p~n", [Body, Status]),
    NumStatus = Status,
    case NumStatus of
	_ when NumStatus >= 100, NumStatus < 200 ->
	    case Body of
		"" ->
		    none;
		_ ->
		    sdp:parse(Body)
	    end;
	_ when NumStatus >= 400, NumStatus < 500 ->
	    sendack(Socket, Header, Url),
	    io:format("Call terminated~n", []),
	    final;
	_ when NumStatus >= 200, NumStatus < 300 ->
	    sendack(Socket, Header, Url),
	    final;
	_ ->
	    final
    end.

control(Dest, Callid, Parent) ->
    InputPid = sound:input_sound(self(), Callid),
    sound:input_sound_dtmf(InputPid, true),
    control(Dest, Callid, Parent, none, InputPid).

control(Dest, Callid, Parent, GenPid, RecvPid) ->
    receive
	{sound, input_sound, port, Listenport} ->
	    {ok, Socket} = gen_udp:open(0),
	    NewGenPid = sound:play_sound_gen(self(), Callid, Socket, Dest,
					     fun (Num) ->
						     {ok, list_to_binary(sine(160)), 0}
					     end),
	    Parent ! {sipanswer, port, Listenport},
	    control(Dest, Callid, Parent, NewGenPid, RecvPid);
	{sound, input_sound, dtmf, _, Digit} ->
	    io:format("sipanswer digit: ~p~n", [Digit]),
	    case GenPid of
		none ->
		    none;
		_ ->
		    sound:play_sound_stop(GenPid)
	    end,
	    sipclient:dial("006000600", Dest),
	    control_1(Dest, Callid, Parent, RecvPid, sipurl:parse("sip:006000600@kth.se"));
	{siprequest, bye} ->
	    RecvPid ! {sipanswer, quit}
    end.

control_1(Dest, Callid, Parent, RecvPid, Url) ->
    receive
	{siprequest, bye} ->
	    RecvPid ! {sipanswer, quit};
	{siprequest, status, Status, Header, Socket, Body, _Origheaders} ->
	    case recvresponse(Socket, Header, Url, Body, Status) of
		{Address, Port} ->
		    sound:input_sound_dest(RecvPid, {Address, Port}),
		    control_1(Dest, Callid, Parent, RecvPid, Url);
		final ->
		    control_1(Dest, Callid, Parent, RecvPid, Url);
		none ->
		    control_1(Dest, Callid, Parent, RecvPid, Url)
	    end
    end.

start(Header, Body, _Mode, _Status, _Number) ->
    Dest = sdp:parse(Body),
    CallID = sipheader:callid(Header),
    Pid = spawn(sipanswer, control, [Dest, CallID, self()]),
    case database_call:insert_call_unique(CallID, answer, Header, [Pid]) of
	{atomic, ok} ->
	    receive
		{sipanswer, port, Listenport} ->
		    {ok, sdp:print({siphost:myip(), Listenport})}
	    end;
	{aborted, key_exists} ->
	    Pid ! {siprequest, bye},
	    {error, duplicate}
    end.

bounce(_Header, Body, _Mode, _Status, _Number) ->
    {ok, Body}.
