-module(rtp).
-export([parse/1, build/1]).

% RFC 1889

parse(Packet) ->
    <<2:2, Padding:1, Extension:1, CSRCLength:4,
    Marker:1, PayloadType:7, Sequence:16,
    Timestamp:32, SSRC:32, Rest/binary>> = Packet,
    <<CSRCbinary:CSRCLength/binary-unit:8, Rest2/binary>> = Rest,
    {ExtTuple, Data} = case Extension of
			   0 ->
			       {none, Rest2};
			   1 ->
			       <<ExtType:16, ExtLength:16, Rest3/binary>> = Rest2,
			    <<ExtData:ExtLength/binary-unit:32, Rest4/binary>> = Rest3,
			       {{ExtType, ExtData}, Rest4}
		       end,
    Padding = 0, % XXX should handle padding instead
    {ExtTuple, Marker, PayloadType, Sequence, Timestamp, SSRC, [], Data}.

build({ExtTuple, Marker, PayloadType, Sequence, Timestamp, SSRC, CSRC, Data}) ->
    ExtTuple = none, % XXX should handle extensions instead
    Padding = 0,
    Extension = 0,
    CSRCLength = 0,
    <<2:2, Padding:1, Extension:1, CSRCLength:4,
    Marker:1, PayloadType:7, Sequence:16,
    Timestamp:32, SSRC:32, Data/binary>>.
