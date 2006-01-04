-module(local_kth_se).

%%--------------------------------------------------------------------
%% Hooks
%%--------------------------------------------------------------------
-export([
	 url2mnesia_userlist/1,
	 lookup_homedomain_url/1,
	 get_forwards_for_users/1,
	 sipuserdb_backend_override/3,
	 sipuserdb_mysql_make_sql_statement/2
	]).


%% local_kth_se specifics
-export([
	 shellget/1,
	 mysql_set_password_for_user/2
	]).

-include("siprecords.hrl").

url2mnesia_userlist(URL) when record(URL, sipurl), is_list(URL#sipurl.user) ->
    [URL#sipurl.user ++ "@" ++ URL#sipurl.host, URL#sipurl.user];
url2mnesia_userlist(URL) when record(URL, sipurl) ->
    [].


% Routing hooks
%%%%%%%%%%%%%%%%

lookup_homedomain_url(URL) when record(URL, sipurl) ->
    Mail = URL#sipurl.user ++ "@" ++ URL#sipurl.host,
    Loc1 = mail2tel(Mail),
    logger:log(debug, "Local routing: mail2tel ~p -> ~p", [Mail, Loc1]),
    case Loc1 of
	none ->
	    none;
	Loc1 ->
	    lookup:lookuppotn(Loc1)
    end.

mail2tel(Mail) ->
    case directory:lookup_mail2uid(Mail) of
        none ->
            lookupkthid_address(Mail);
        KTHid ->
            lookupkthid_address(KTHid ++ "@kth.se")
    end.

do_recv(Sock, Text) ->
    receive
	{tcp, Sock, Data} ->
	    do_recv(Sock, Text ++ Data);
	{tcp_closed, Sock} ->
	    Text
    end.

parse_packet(Packet) ->
    Packetfixed = siputil:linefix(Packet),
    case string:str(Packetfixed, "\n\n") of
	0 ->
	    {Packetfixed, ""};
	Headerlen ->
	    HeaderStr = string:substr(Packetfixed, 1, Headerlen),
	    BodyStr = string:substr(Packetfixed, Headerlen + 2),
	    {HeaderStr, BodyStr}
    end.

lookupkthid(KTHid) ->
    Server = "yorick.admin.kth.se",
    {ok, Sock} = gen_tcp:connect(Server, 80,
				 [list, {packet, 0}]),
    ok = gen_tcp:send(Sock, "GET /service/personsokning/kthid2tele.asp?kthid=" ++ KTHid ++ " HTTP/1.0\r\n\r\n"),

    Text = do_recv(Sock, ""),
    {_Header, Body} = parse_packet(Text),
    ok = gen_tcp:close(Sock),
    Numbers = string:tokens(Body, "\r\n"),
    case Numbers of
	[] ->
	    none;
	[Number | _] ->
	    Number
    end.

lookupkthid_address(Address) ->
    case group_regexp:groups(Address, "(u[0-9]......)@kth.se") of
	{match, [KTHid]} ->
	    lookupkthid(KTHid);
	nomatch ->
	    none;
	{error, _Error} ->
	    none
    end.



% userdb hooks
%%%%%%%%%%%%%%%

get_forwards_for_users(Users) ->
    case sipuserdb:get_forwards_for_users(Users) of
	{Forwards, Timeout, Localring} when list(Forwards) ->
	    Func = fun(Forward) ->
			   sipurl:new([{proto, "sip"}, {user, Forward}, {host, "kth.se"}])
		   end,
	    NewForwards = lists:map(Func, Forwards),
	    {NewForwards, Timeout, Localring};
	Res ->
	    Res
    end.

%% Module   = atom(), sipuserdb module
%% Function = atom(), function in Module
%% Args     = term(), arguments to function
%% Returns : {ok, Res} | undefined
%%           If 'undefined' is returned, the real backend function will be called
sipuserdb_backend_override(sipuserdb_mysql, get_password_for_user, Args) ->
    mysql_get_password_for_user(yxa, Args);
sipuserdb_backend_override(_Module, _Function, _Args) ->
    undefined.

%% Returns : {ok, Res} | undefined
sipuserdb_mysql_make_sql_statement(sipuserdb_mysql_get_user_for_address, Args) ->
    [AddressList] = Args,
    %% rewrite args slightly before calling original version
    AddressListMod =
	lists:foldl(
	  fun (Address, Acc) ->
		  case util:regexp_rewrite(Address,
					   [{"sip:(.*)@test.kth.se", "\\1"}]) of
		      nomatch ->
			  Acc;
		      A ->
			  [A | Acc]
		  end
	  end, [], AddressList),
    sipuserdb_mysql:make_sql_statement(sipuserdb_mysql_get_user_for_address, AddressListMod);
sipuserdb_mysql_make_sql_statement(_CfgKey, _Args) ->
    undefined.

mysql_set_password_for_user(User, Password) ->
    IV = shellget("openssl rand 8"),
    logger:log(debug, "local_kth_se:set_password_for_user : IV: ~p", [IV]),
    Data = encrypt_password(IV, Password),
    case mysql:fetch(yxa,
		     "insert into password (kthid,iv,mac,data) values (" ++
                     mysql:quote(User) ++ "," ++
                     mysql:quote(binary_to_list(IV)) ++ "," ++
                     mysql:quote(calc_password_hmac(Data, User)) ++ "," ++
                     mysql:quote(Data) ++ ")") of
	{ok, _, Message} ->
	    logger:log(debug, "local_kth_se:set_password_for_user : ~p", [Message]),
	    nomatch;
	{error, Reason} ->
	    logger:log(error, "local_kth_se:set_password_for_user : Error for kthid ~p: ~p", [User, Reason]),
	    error
    end.

mysql_get_password_for_user(Id, User) ->
    %% sipuserdb_mysql:set_password_for_user("u1lhi9k2", "foobar"),
    Query1 = sipuserdb_mysql:make_sql_statement(sipuserdb_mysql_get_password_for_user,
						User),
    case mysql:fetch(Id, Query1) of
        {error, Message} ->
            logger:log(error, "local_kth_se:get_password_for_user : Error for kthid ~p: ~p", [User, Message]),
            error;
        {_, []} ->
            logger:log(debug, "local_kth_se:set_password_for_user : No such kthid ~p when fetching password",
		       [User]),
            nomatch;
        {_, [[IV, Mac, Data], _]} ->
            Password = decrypt_password(IV, Data),
            case check_password_hmac(Mac, Data, User) of
                true ->
                    logger:log(debug, "local_kth_se:set_password_for_user : pwd: ~p", [Password]),
                    Password;
                _ ->
                    nomatch
            end
    end.

pack_password(Password) when binary(Password), size(Password) < 255 ->
    Length = size(Password),
    Padlength = 8 - (((Length + 2) rem 8) - 1),
    Pad = list_to_binary(lists:duplicate(Padlength, 0)),
    <<Length:8,
    Password/binary,
    Pad/binary>>.

unpack_password(Data) when binary(Data) ->
    <<Length:8,
    Rest/binary>> = Data,
    <<Password:Length/binary-unit:8, _Rest2/binary>> = Rest,
    binary_to_list(Password).

decrypt_password(IV, Data) ->
    {ok, Key1} = yxa_config:get_env(local_password_des3_key1),
    {ok, Key2} = yxa_config:get_env(local_password_des3_key2),
    {ok, Key3} = yxa_config:get_env(local_password_des3_key3),
    logger:log(debug, "local_kth_se:decrypt_password : IV ~p Data ~p", [IV, Data]),
    Res = crypto:des3_cbc_decrypt(Key1, Key2, Key3, IV, Data),
    logger:log(debug, "local_kth_se:decrypt_password : Res ~p", [unpack_password(Res)]),
    unpack_password(Res).

encrypt_password(IV, Data) ->
    {ok, Key1} = yxa_config:get_env(local_password_des3_key1),
    {ok, Key2} = yxa_config:get_env(local_password_des3_key2),
    {ok, Key3} = yxa_config:get_env(local_password_des3_key3),
    logger:log(debug, "local_kth_se:encrypt_password: IV ~p Data ~p", [IV, Data]),
    Res = binary_to_list(crypto:des3_cbc_encrypt(Key1, Key2, Key3, IV,
						 pack_password(list_to_binary(Data)))),
    logger:log(debug, "local_kth_se:encrypt_password: Res ~p", [Res]),
    Res.

% Salt with kthid

calc_password_hmac(Data, Kthid) ->
    {ok, Key} = yxa_config:get_env(local_password_shamac_key),
    logger:log(debug, "local_kth_se:calc_password_hmac: Key ~p Data ~p Kthid ~p", [Key, Data, Kthid]),
    Res = binary_to_list(crypto:sha_mac(Key, Kthid ++ ":" ++ Data)),
    logger:log(debug, "local_kth_se:calc_password_hmac: Res ~p", [Res]),
    Res.

check_password_hmac(Mac, Data, Kthid) ->
    case calc_password_hmac(Data, Kthid) of
	Mac ->
	    true;
	_ ->
	    false
    end.


shellget(Command) ->
    Port = open_port({spawn, Command}, [stream, binary, eof]),
    shellgetloop(Port, <<>>).

shellgetloop(Port, Acc) ->
    receive
	{Port, {data, Data}} ->
	    shellgetloop(Port, <<Data/binary, Acc/binary>>);
	{Port, eof} ->
	    Acc
    end.
