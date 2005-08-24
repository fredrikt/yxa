-module(dtmf).
-export([init/1]).

init(Ref) ->
    Port = open_port({spawn, "./dtmfserver"}, [{packet, 2}]),
    Ref ! {dtmf, process, self()},
    loop(Port, Ref).

loop(Port, Ref) ->
    receive
	{dtmf, data, Data} ->
	    Port ! {self(), {command, Data}},
	    loop(Port, Ref);
	{Port, {data, Data}} ->
	    Ref ! {dtmf, digit, Data},
	    loop(Port, Ref);
	{dtmf, quit} ->
	    true
    end.
