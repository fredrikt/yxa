% Copyright (c) 2001-2004 Kungliga Tekniska Högskolan
% See the file COPYING

% Usage:
%
%
% Call this before any call to fetch/1
%
% start(Host, User, Password, Database)
%
%
% Makes a database query
%
% fetch("select * from hello") -> Result
% Result = {Fieldinfo, Rows}
% Rows = [Row]
% Row = [string()]

-module(mysql).
-export([start/4, fetch/1, fetch/2, quote/1]).
-export([asciz_binary/2]).

-define(LONG_PASSWORD, 1).
-define(LONG_FLAG, 4).
-define(CONNECT_WITH_DB, 8).
-define(LOCAL_FILES, 128).
-define(PROTOCOL_41, 512).
-define(INTERACTIVE, 1024).
-define(TRANSACTIONS, 8192).
-define(SECURE_CONNECTION, 32768).

-define(MAX_PACKET_SIZE, 1000000).

log(Level, Format, Arguments) ->
    io:format(Format, Arguments),
    io:format("~n", []).

startrecv(Host, Port) ->
    Self = self(),
    Pid = spawn(fun () ->
			recvprocess(Host, Port, Self)
		end),
    receive
	{mysql, sendsocket, Socket} ->
	    Socket
    end.

recvprocess(Host, Port, Parent) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
    Parent ! {mysql, sendsocket, Sock},
    recvloop(Sock, Parent, <<>>),
    Parent ! {mysql, closed}.

sendpacket(Parent, Data) ->
    case Data of
	<<Length:24/little, Num:8, D/binary>> ->
	    if
		Length =< size(D) ->
		    {Packet, Rest} = split_binary(D, Length),
		    Parent ! {mysql, response, Packet, Num},
		    sendpacket(Parent, Rest);
		true ->
		    Data
	    end;
	_ ->
	    Data
    end.

recvloop(Sock, Parent, Data) ->
    Rest = sendpacket(Parent, Data),
    receive
	{tcp, Sock, InData} ->
	    NewData = concat_binary([Rest, InData]),
	    recvloop(Sock, Parent, NewData);
	{tcp_error, Sock, Reason} ->
	    true;
	{tcp_closed, Sock} ->
	    true
    end.

do_recv() ->
    receive
	{mysql, response, Packet, Num} ->
	    log(debug, "mysql recv packet ~p: ~p", [Num, Packet]),
	    {Packet, Num}
    end.

do_send(Sock, Packet, Num) ->
    log(debug, "mysql send packet ~p: ~p", [Num, Packet]),
    Data = <<(size(Packet)):24/little, Num:8, Packet/binary>>,
    gen_tcp:send(Sock, Data).

make_auth(User, Password) ->
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS,
    Maxsize = 0,
    UserB = list_to_binary(User),
    PasswordB = Password,
    <<Caps:16/little, Maxsize:24/little, UserB/binary, 0:8,
    PasswordB/binary>>.

make_new_auth(User, Password, Database) ->
    DBCaps = case Database of
		 none ->
		     0;
		 _ ->
		     ?CONNECT_WITH_DB
	     end,
    Caps = ?LONG_PASSWORD bor ?LONG_FLAG bor ?TRANSACTIONS bor
	?PROTOCOL_41 bor ?SECURE_CONNECTION bor DBCaps,
    Maxsize = ?MAX_PACKET_SIZE,
    UserB = list_to_binary(User),
    PasswordL = size(Password),
    DatabaseB = case Database of
		    none ->
			<<>>;
		    _ ->
			list_to_binary(Database)
		end,
    <<Caps:32/little, Maxsize:32/little, 8:8, 0:23/integer-unit:8,
    UserB/binary, 0:8, PasswordL:8, Password/binary, DatabaseB/binary>>.

hash(S) ->
    hash(S, 1345345333, 305419889, 7).

hash([C | S], N1, N2, Add) ->
    N1_1 = N1 bxor (((N1 band 63) + Add) * C + N1 * 256),
    N2_1 = N2 + ((N2 * 256) bxor N1_1),
    Add_1 = Add + C,
    hash(S, N1_1, N2_1, Add_1);
hash([], N1, N2, Add) ->
    Mask = (1 bsl 31) - 1,
    {N1 band Mask , N2 band Mask}.

rnd(N, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    rnd(N, [], Seed1 rem Mod, Seed2 rem Mod).

rnd(0, List, _, _) ->
    lists:reverse(List);
rnd(N, List, Seed1, Seed2) ->
    Mod = (1 bsl 30) - 1,
    NSeed1 = (Seed1 * 3 + Seed2) rem Mod,
    NSeed2 = (NSeed1 + Seed2 + 33) rem Mod,
    Float = (float(NSeed1) / float(Mod))*31,
    Val = trunc(Float)+64,
    rnd(N - 1, [Val | List], NSeed1, NSeed2).

password_old(Password, Salt) ->
    {P1, P2} = hash(Password),
    {S1, S2} = hash(Salt),
    Seed1 = P1 bxor S1,
    Seed2 = P2 bxor S2,
    List = rnd(9, Seed1, Seed2),
    {L, [Extra]} = lists:split(8, List),
    list_to_binary(lists:map(fun (E) ->
				     E bxor (Extra - 64)
			     end, L)).

dualmap(F, [], []) ->
    [];
dualmap(F, [E1 | R1], [E2 | R2]) ->
    [F(E1, E2) | dualmap(F, R1, R2)].

bxor_binary(B1, B2) ->
    list_to_binary(dualmap(fun (E1, E2) ->
				   E1 bxor E2
			   end, binary_to_list(B1), binary_to_list(B2))).

password_new(Password, Salt) ->
    Stage1 = crypto:sha(Password),
    Stage2 = crypto:sha(Stage1),
    Res=crypto:sha_final(crypto:sha_update(crypto:sha_update(crypto:sha_init(),
							     Salt),
					   Stage2)),
    bxor_binary(Res, Stage1).
    

asciz_binary(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
asciz_binary(<<0:8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) ->
    asciz_binary(Rest, [C | Acc]).

asciz(Data) when binary(Data) ->
    asciz_binary(Data, []);
asciz(Data) when list(Data) ->
    {String, [0 | Rest]} = lists:splitwith(fun (C) ->
						   C /= 0
					   end, Data),
    {String, Rest}.

greeting(Packet) ->
    <<Protocol:8, Rest/binary>> = Packet,
    {Version, Rest2} = asciz(Rest),
    <<TreadID:32/little, Rest3/binary>> = Rest2,    
    {Salt, Rest4} = asciz(Rest3),
    <<Caps:16/little, Rest5/binary>> = Rest4,
    <<ServerChar:16/binary-unit:8, Rest6/binary>> = Rest5,
    {Salt2, Rest7} = asciz(Rest6),
    log(debug, "mysql greeting version ~p salt ~p caps ~p serverchar ~p salt2 ~p", [Version, Salt, Caps, ServerChar, Salt2]),
    {Salt, Salt2, Caps}.

get_with_length(<<251:8, Rest/binary>>) ->
    {null, Rest};
get_with_length(<<252:8, Length:16/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<253:8, Length:24/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<254:8, Length:64/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<Length:8, Rest/binary>>) when Length < 251 ->
    split_binary(Rest, Length).

get_fields() ->
    {Packet, Num} = do_recv(),
    case Packet of
	<<254:8>> ->
	    [];
	_ ->
	    {Table, Rest} = get_with_length(Packet),
	    {Field, Rest2} = get_with_length(Rest),
	    {LengthB, Rest3} = get_with_length(Rest2),
	    LengthL = size(LengthB) * 8,
	    <<Length:LengthL/little>> = LengthB,
	    {Type, Rest4} = get_with_length(Rest3),
	    {Flags, Rest5} = get_with_length(Rest4),
	    [{binary_to_list(Table),
	      binary_to_list(Field),
	      Length,
	      binary_to_list(Type)} | get_fields()]
    end.

get_row(0, Data) ->
    [];
get_row(N, Data) ->
    {Col, Rest} = get_with_length(Data),
    [case Col of
	 null ->
	     null;
	 _ ->
	     binary_to_list(Col)
     end | get_row(N - 1, Rest)].

get_rows(N) ->
    {Packet, Num} = do_recv(),
    case {Packet, size(Packet) > 8} of
	{<<254:8, Rest/binary>>,  false} ->
	    [];
	_ ->
	    [get_row(N, Packet) | get_rows(N)]
    end.

get_query_response() ->
    {<<Fieldcount:8, Rest/binary>>, _} = do_recv(),
    case Fieldcount of
	0 ->
	    {[], []};
	255 ->
	    <<Code:16/little, Message/binary>>  = Rest,
	    {error, binary_to_list(Message)};
	_ ->
	    Fields = get_fields(),
	    Rows = get_rows(Fieldcount),
	    {Fields, Rows}
    end.

do_query(Sock, Query) ->
    Q = list_to_binary(Query),
    Packet = <<3, Q/binary>>,
    do_send(Sock, Packet, 0),
    get_query_response().

do_init(Sock, User, Password) ->
    {Packet, Num} = do_recv(),
    {Salt1, Salt2, Caps} = greeting(Packet),
    {RecvPacket, RecvNum} = case Caps band ?SECURE_CONNECTION of
				?SECURE_CONNECTION ->
				    do_new_auth(Sock, User, Password, Salt1, Salt2);
				_ ->
				    do_old_auth(Sock, User, Password, Salt1)
			    end,
    case RecvPacket of
	<<0:8, Rest/binary>> ->
	    ok;
	<<255:8, Code:16/little, Message/binary>> ->
	    io:format("Error ~p: ~p~n", [Code, binary_to_list(Message)]),
	    error;
	_ ->
	    io:format("Unknown error ~p~n", [binary_to_list(RecvPacket)]),
	    error
    end.

do_old_auth(Sock, User, Password, Salt1) ->
    Auth = password_old(Password, Salt1),
    Packet2 = make_auth(User, Auth),
    do_send(Sock, Packet2, 1),    
    do_recv().

do_new_auth(Sock, User, Password, Salt1, Salt2) ->
    Auth = password_new(Password, Salt1 ++ Salt2),
    Packet2 = make_new_auth(User, Auth, none),
    do_send(Sock, Packet2, 1),
    {Packet3, Num3} = do_recv(),
    case Packet3 of
	<<254:8>> ->
	    AuthOld = password_old(Password, Salt1),
	    do_send(Sock, <<AuthOld/binary, 0:8>>, 3),
	    do_recv();
	_ ->
	    {Packet3, Num3}
    end.

connect(Host, User, Password, Database) ->
    connect(Host, User, Password, Database, closed).

connect(Host, User, Password, Database, closed) ->
    receive
	{mysql, fetchquery, Ref, Query, Pid} ->
	    Sock = startrecv(Host, 3306),
	    case do_init(Sock, User, Password) of
		ok ->
		    do_query(Sock, "use " ++ Database),
		    Result = do_query(Sock, Query),
		    Pid ! {mysql, result, Ref, Result},
		    connect(Host, User, Password, Database, {open, Sock});
		_ ->
		    gen_tcp:close(Sock),
		    connect(Host, User, Password, Database, closed)
	    end
    end;
connect(Host, User, Password, Database, {open, Sock}) ->
    receive
	{mysql, fetchquery, Ref, Query, Pid} ->
	    Result = do_query(Sock, Query),
	    Pid ! {mysql, result, Ref, Result},
	    connect(Host, User, Password, Database, {open, Sock});
	{mysql, closed} ->
	    connect(Host, User, Password, Database, closed)
    end.

start(Host, User, Password, Database) ->
    Pid = spawn(fun () ->
			connect(Host, User, Password, Database)
		end),
    register(mysql, Pid).

fetchloop(Ref, Query, 0) ->
    log(error, "mysql giving up(~p): ~p", [Ref, Query]),
    {error, "Too many retries, giving up"};
fetchloop(Ref, Query, Times) ->
    mysql ! {mysql, fetchquery, Ref, Query, self()},
    receive
	{mysql, result, Ref, Result} ->
	    Result
    after
	3000 ->
	    log(debug, "mysql not responding(~p): ~p", [Ref, Query]),
	    fetchloop(Ref, Query, Times - 1)
    end.
 
fetch(Query) ->
    fetch(Query, 1).

fetch(Query, Times) when is_list(Query) ->
    Ref = make_ref(),
    log(debug, "mysql: fetch ~p", [Query]),
    fetchloop(Ref, Query, Times);
fetch(Query, Times) ->
    log(error, "mysql: fetch ~p", [Query]),
    {error, "Invalid query string"}.

quote(String) ->
    [$" | lists:reverse([$" | quote(String, [])])].

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([$' | Rest], Acc) ->
    quote(Rest, [$', $\\ | Acc]);
quote([$" | Rest], Acc) ->
    quote(Rest, [$", $\\ | Acc]);
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).
