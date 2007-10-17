-module(sound).
-export([input_sound/2, input_sound_stop/1, input_sound_capture/2, input_sound_endcapture/1,
	input_sound_dtmf/2, input_sound_start/2, input_sound_dest/2,
	 play_sound/5, play_sound_start/5,
	 play_sound_stop/1, play_sound_gen/5, play_sound_genstart/5]).

input_sound(Parent, Handle) ->
    spawn(sound, input_sound_start, [Parent, Handle]).

input_sound_start(Parent, Handle) ->
    {ok, Listensocket} = gen_udp:open(0),
    {ok, Listenport} = inet:port(Listensocket),
    Parent ! {sound, input_sound, port, Listenport},
    inputloop(Handle, Parent, Listensocket).

inputloop(Handle, Parent, Socket) ->
    inputloop(Handle, Parent, Socket, none, none, none).

inputloop(Handle, Parent, Socket, Dtmf, Capture, Dest) ->
    receive
	{udp, Socket, _IP, _InPortNo, Packet} ->
	    {_, _, _, _, _, _, _, Sound} = rtp:parse(list_to_binary(Packet)),
	    case Dtmf of
		none ->
		    true;
		Dtmf ->
		    Dtmf ! {dtmf, data, Sound}
	    end,
	    case Capture of
		none ->
		    true;
		Capture ->
		    io:put_chars(Capture, binary_to_list(Sound))
	    end,
	    case Dest of
		none ->
		    true;
		{Address, Port} ->
		    gen_udp:send(Socket, Address, Port, Packet)
	    end,
	    inputloop(Handle, Parent, Socket, Dtmf, Capture, Dest);
	{sound, inputloop, quit} ->
	    true;
	{sound, inputloop, capture, Device} ->
	    case Capture of
		none ->
		    true;
		Capture ->
		    file:close(Capture)
	    end,
	    inputloop(Handle, Parent, Socket, Dtmf, Device, Dest);
	{sound, inputloop, endcapture} ->
	    case Capture of
		none ->
		    true;
		Capture ->
		    file:close(Capture)
	    end,
	    inputloop(Handle, Parent, Socket, Dtmf, none, Dest);
	{sound, inputloop, dtmf, true} ->
	    case Dtmf of
		none ->
		    inputloop(Handle, Parent, Socket, spawn(dtmf, init, [self()]), Capture, Dest);
		Dtmf ->
		    inputloop(Handle, Parent, Socket, Dtmf, Capture, Dest)
	    end;
	{sound, inputloop, dtmf, false} ->
	    case Dtmf of
		none ->
		    inputloop(Handle, Parent, Socket, Dtmf, Capture, Dest);
		Dtmf ->
		    Dtmf ! {dtmf, quit},
		    inputloop(Handle, Parent, Socket, none, Capture, Dest)
	    end;
	{sound, inputloop, dest, NewDest} ->
	    inputloop(Handle, Parent, Socket, Dtmf, Capture, NewDest);
	{dtmf, digit, Data} ->
	    Parent ! {sound, input_sound, dtmf, Handle, Data},
	    inputloop(Handle, Parent, Socket, Dtmf, Capture, Dest)
    end.

input_sound_stop(Pid) ->
    Pid ! {sound, inputloop, quit}.

input_sound_capture(Pid, Device) ->
    Pid ! {sound, inputloop, capture, Device}.

input_sound_endcapture(Pid) ->
    Pid ! {sound, inputloop, endcapture}.

input_sound_dtmf(Pid, Flag) ->
    Pid ! {sound, inputloop, dtmf, Flag}.

input_sound_dest(Pid, Dest) ->
    Pid ! {sound, inputloop, dest, Dest}.

play_sound(Parent, Handle, Socket, Dest, Data) ->
    spawn(sound, play_sound_start, [Parent, Handle, Socket, Dest, Data]).

play_sound_start(Parent, Handle, Socket, {Address, Port}, Data) ->
    timer:send_interval(20, {sound, play_sound, send}),
    sendloop(Handle, Parent, Socket, Address, Port, Data).

play_sound_stop(Pid) ->
    Pid ! {sound, sendloop, quit}.

sendloop(Handle, Parent, _Socket, _Address, _Port, []) ->
    Parent ! {sound, play_sound, Handle, finished},
    true;

sendloop(Handle, Parent, Socket, Address, Port, Data) ->
    receive
	{sound, play_sound, send} ->
	    [Packet | NewData] = Data,
	    gen_udp:send(Socket, Address, Port, Packet),
	    sendloop(Handle, Parent, Socket, Address, Port, NewData);
	{sound, sendloop, quit} ->
	    true
    end.


play_sound_gen(Parent, Handle, Socket, Dest, Fun) ->
    spawn(sound, play_sound_genstart, [Parent, Handle, Socket, Dest, Fun]).

play_sound_genstart(Parent, Handle, Socket, {Address, Port}, Fun) ->
    timer:send_interval(20, {sound, play_sound, send}),
    sendloop_gen(Handle, Parent, Socket, Address, Port, Fun, 0, 0, 0).

sendloop_gen(Handle, Parent, Socket, Address, Port, Fun, Arg, Seq, Timestamp) ->
    receive
	{sound, play_sound, send} ->
	    case apply(Fun, [Arg]) of
		{stop} ->
		    Parent ! {sound, play_sound, Handle, finished};
		{ok, Data, NewArg} ->
		    Packet = rtp:build({none, 0, 8, Seq, Timestamp, 0, [], Data}),
		    gen_udp:send(Socket, Address, Port, Packet),
		    sendloop_gen(Handle, Parent, Socket, Address, Port, Fun,
				 NewArg, Seq + 1, Timestamp + size(Data))
	    end;
	{sound, sendloop, quit} ->
	    true
    end.
