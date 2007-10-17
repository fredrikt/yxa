%%%-------------------------------------------------------------------
%%% File    : ssl_util.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Utility functions for SSL socket validation/certificate
%%            information parsing.
%%% @since    30 Sep 2005 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(ssl_util).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 get_ssl_peer_info/4,
	 is_acceptable_ssl_socket/5,

	 decode_ssl_rdnseq/1,

	 test/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("ssl/include/SSL-PKIX.hrl").
-include("sipsocket.hrl").


%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (Socket, Proto, IP, Port) ->
%%            {ok, Subject, AltNames} |
%%            {error, Reason}
%%
%%            Socket = term()
%%            Proto  = tls | tls6
%%            IP     = string()
%%            Port   = integer()
%%
%%            Subject  = term() "ssl:peercert() subject data"
%%            AltNames = [string()] "subjectAltName:s in cert"
%%            Reason   = string()
%%
%% @doc     Try to get the SSL peer certificate using a socket. If
%%          that fails, we check if it was a client that connected to
%%          us and if clients are required to present a certificate.
%%          Returns either {ok, Subject}, or true/false saying if the
%%          socket should be considered valid or not.
%% @end
%%--------------------------------------------------------------------
get_ssl_peer_info(Socket, Proto, IP, Port) when is_atom(Proto), is_list(IP), is_integer(Port) ->
    %% ssl:peercert/3 needs to be wrapped in a try/catch - it fails badly on some certificates
    %% NOTE: the above should not be true anymore since we are not trying to get erlang-decoded
    %% certificates (option 'ssl') anymore, and the problem was with extensions or other
    %% attributes that was not recognized by the erlang-decoding code in the ssl application.
    %% We keep it for unforseen events though.
    PeerCertRes =
	try ssl:peercert(Socket, [pkix]) of
	    PCRes -> PCRes
	catch
	    error: E ->
		ST = erlang:get_stacktrace(),
		logger:log(error, "=ERROR REPORT==== from ssl:peercert/2 on certificate presented by ~p:~s:~p :~n"
			   "~p~nstacktrace : ~p", [Proto, IP, Port, E, ST]),
		{error, "Certificate decoding error"};
	    X: Y ->
		logger:log(error, "=ERROR REPORT==== from ssl:peercert/2 on certificate presented by ~p:~s:~p :~n"
			   "~p ~p", [Proto, IP, Port, X, Y]),
		{error, "Certificate decoding error"}
	end,
    get_ssl_peer_info2(Proto, IP, Port, PeerCertRes).

%% get_ssl_peer_info2/4 - part of get_ssl_peer_info/4, just to make things testable
get_ssl_peer_info2(Proto, IP, Port, PeerCertRes) ->
    case PeerCertRes of
	{ok, PKIXCert} ->
	    %% Now, we need to extract the Subject information, and the subjectAltNames
	    {ok, Subject} = get_ssl_peer_info_subject(PKIXCert),
	    {ok, AltNames} = get_ssl_peer_info_host_altnames(PKIXCert),
	    {ok, Subject, AltNames};
	{error, Reason} ->
	    logger:log(error, "TCP connection: Could not decode certificate presented by ~p:~s:~p : ~p",
		       [Proto, IP, Port, Reason]),
	    {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @spec    (Socket, Dir, Proto, Remote, Names) -> true | false
%%
%%            Socket = term() "(sslsocket, NOT sipsocket)"
%%            Dir    = in | out
%%            Proto  = tls | tcp
%%            Remote = {IP, Port}
%%            IP     = string()
%%            Port   = integer()
%%            Names  = [string()] "list of names for the certificate that the upper layer is willing to accept"
%%
%% @doc     Check if a socket is 'acceptable'. For SSL, this means
%%          verify that the subjectAltName/CN is included in Names.
%% @end
%%--------------------------------------------------------------------
is_acceptable_ssl_socket(Socket, Dir, Proto, Remote, Names) when Proto == tls orelse Proto == tls6, is_list(Names) ->
    {IP, Port} = Remote,
    case get_ssl_peer_info(Socket, Proto, IP, Port) of
	{ok, Subject, AltNames} when is_record(Subject, ssl_conn_subject), is_list(AltNames) ->
	    DirStr = case Dir of
			 in -> "from";
			 out -> "to"
		     end,
	    logger:log(debug, "TCP connection: SSL connection ~s ~s:~p (~s)",
		       [DirStr, IP, Port, Subject#ssl_conn_subject.description]),

	    %% See if Local has an opinion about the validity of this socket...
	    case local:is_acceptable_socket(Socket, Dir, Proto, IP, Port, ?MODULE, Subject) of
		true ->
		    true;
		false ->
		    false;
		undefined ->
		    %% Do our own default checking of the subjectAltName/CN, if this is an outbound
		    %% connection. Per default, we accept any incoming TLS connections as valid,
		    %% although we don't trust those connections in any special way.
		    case Dir of
			in -> true;
			out ->
			    {ok, Reject} = yxa_config:get_env(ssl_check_subject_altname),
			    NewAltNames = local:get_valid_altnames(Names, Subject, AltNames),
			    is_valid_ssl_certname(Names, Subject, NewAltNames, Reject)
		    end
	    end;
	{error, Reason} ->
	    {ok, RequireClientCert} = yxa_config:get_env(ssl_require_client_has_cert),
	    case {Dir, RequireClientCert} of
		{in, true} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information, "
			       "considering SSL connection from ~s:~p invalid : ~p", [IP, Port, Reason]),
		    false;
		{in, false} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information for connection "
			       "from ~s:~p : ~p (ignoring)", [IP, Port, Reason]),
		    true;
		{out, _RequireClientCert} ->
		    logger:log(debug, "TCP connection: Could not get SSL subject information, "
			       "considering SSL connection to ~s:~p invalid : ~p", [IP, Port, Reason]),
		    false
	    end
    end.


%%--------------------------------------------------------------------
%% @spec    (RdnSequence) ->
%%            {ok, Tuples}    |
%%            {error, Reason}
%%
%%            RdnSequence = term() "({rdnSequence, AttrList}). SSL PKIX formatted data from a certificate parsed using ssl:peercert or ssl_pkix:decode_cert*."
%%
%%            Tuples = {Key, Value}
%%            Key    = integer()
%%            Value  = string()
%%            Reason = string()
%%
%% @doc     Turn a rdnSequence into a list of {Key, Value} where Key
%%          is either the oid (integer() or tuple()) or, if
%%          ssl_pkix_oid could turn it into an atom, then an atom
%%          (like countryName).
%% @end
%%--------------------------------------------------------------------
decode_ssl_rdnseq({rdnSequence, AttrList}) when is_list(AttrList) ->
    decode_ssl_rdnseq2(AttrList, []);
decode_ssl_rdnseq(_Other) ->
    {error, "input to decode_ssl_rdnseq is not an rdnSequence tuple"}.

decode_ssl_rdnseq2([[H] | T], Res) when is_record(H, 'AttributeTypeAndValue') ->
    %% get type
    Type =
	try ssl_pkix_oid:id2atom(H#'AttributeTypeAndValue'.type) of
	    A ->
		A
	catch
	    _: _ -> H#'AttributeTypeAndValue'.type
	end,

    %% get value - copy-pasted from ssl_pkix:transform/1
    try
	begin
	    {ok, ATAVEnc} = 'PKIX1Explicit88':encode('AttributeTypeAndValue', H),
	    'SSL-PKIX':decode('AttributeTypeAndValue', list_to_binary(ATAVEnc))
	end of
	{ok, ATAVDec} ->
	    case ATAVDec#'AttributeTypeAndValue'.value of
		{printableString, Value} ->
		    decode_ssl_rdnseq2(T, [{Type, Value} | Res]);
		Str when is_list(Str) ->
		    decode_ssl_rdnseq2(T, [{Type, Str}]);
		_ ->
		    logger:log(debug, "SSL util: Could not decode ~p ~p", [Type, H]),
		    {error, "could not decode rdnSequence value"}
	    end
    catch
	error:
	  E ->
	    ST = erlang:get_stacktrace(),
	    logger:log(error, "SSL util: Failed decoding rdnSequence entry (crashed in ASN.1 encoding/decoding)"),
	    logger:log(debug, "SSL util: Crashed parsing ~p~n~p, stacktrace ~p", [H, E, ST]),
	    {error, "failed parsing rdnSequence entry"};
	  X: Y ->
	    logger:log(error, "SSL util: Failed decoding rdnSequence entry : ~p ~p", [X, Y]),
	    logger:log(debug, "SSL util: Crashed parsing ~p : ~p ~p", [H, X, Y]),
	    {error, "failed parsing rdnSequence entry"}
    end;
decode_ssl_rdnseq2([], Res) ->
    {ok, Res}.


%%====================================================================
%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec    (Cert) ->
%%            {ok, Subject} |
%%            error
%%
%%            Cert = #'Certificate'{} "SSL PKIX parsed"
%%
%%            Subject = #ssl_conn_subject{}
%%
%% @doc     Extracts subject information from an SSL PKIX certificate
%% @end
%%--------------------------------------------------------------------
get_ssl_peer_info_subject(Cert) when is_record(Cert, 'Certificate') ->
    Subject = (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.subject,
    case decode_ssl_rdnseq(Subject) of
	{ok, Decoded} ->
	    C = ssl_decoded_rdn_get('at-countryName', Decoded),
	    O = ssl_decoded_rdn_get('at-organizationName', Decoded),
	    CN = ssl_decoded_rdn_get('at-commonName', Decoded),
	    Descr = lists:append(["C=", C, "/O=", O, "/CN=", CN]),
	    This = #ssl_conn_subject{countryName	= C,
				     organizationName	= O,
				     commonName		= http_util:to_lower(CN),
				     description	= Descr
				    },
	    {ok, This};
	_ ->
	    logger:log(debug, "TCP connection: Don't recognize the format of the SSL subject : ~p",
		       [Subject]),
	    error
    end.

%% ssl_decoded_rdn_get/2, part of get_ssl_peer_info_subject/1. Returns : String
ssl_decoded_rdn_get(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	{value, {Key, Value}} ->
	    Value;
	false ->
	    ""
    end.

%%--------------------------------------------------------------------
%% @spec    (Cert) ->
%%            {ok, AltNames}
%%
%%            Cert = #'Certificate'{} "SSL PKIX parsed"
%%
%%            AltNames = [string()]
%%
%% @doc     Extracts subjectAltName's of type dNSName or iPAddress
%%          from an SSL PKIX certificate.
%% @end
%%--------------------------------------------------------------------
get_ssl_peer_info_host_altnames(Cert) when is_record(Cert, 'Certificate') ->
    Extensions = (Cert#'Certificate'.tbsCertificate)#'TBSCertificate'.extensions,
    AltNameExtensions = get_tbs_extensions(ssl_pkix_oid:atom2id('ce-subjectAltName'), Extensions),
    {ok, DNS_altNames} = get_host_altnames('SubjectAltName', AltNameExtensions),
    {ok, DNS_altNames}.

%% get_tbs_extensions/2 - part of get_ssl_peer_info_host_altnames/1
%% Returns : list() of 'Extension' record
get_tbs_extensions(Key, Extensions) ->
    get_tbs_extensions(Key, Extensions, []).

get_tbs_extensions(Key, [#'Extension'{extnID = Key} = H | T], Res) ->
    get_tbs_extensions(Key, T, [H | Res]);
get_tbs_extensions(Key, [H | T], Res) when is_record(H, 'Extension') ->
    get_tbs_extensions(Key, T, Res);
get_tbs_extensions(_Key, [], Res) ->
    lists:reverse(Res).

%% get_host_altnames(Type, Extensions) - part of get_ssl_peer_info_host_altnames/1
%% Returns : {ok, Strings}
get_host_altnames(Type, Extensions) ->
    get_host_altnames(Type, Extensions, []).

get_host_altnames(Type, [#'Extension'{extnValue = Value} | T], Res) ->
    {ok, Decoded} = 'PKIX1Implicit88':decode(Type, list_to_binary(Value)),
    %% Decoded is a list of tuples, for example :
    %%   [{rfc822Name, "ft@example.org"},
    %%    {dNSName,    "sip.example.org"}]
    This = lists:foldl(fun({dNSName, Name}, Acc) when is_list(Name) ->
			       [http_util:to_lower(Name) | Acc];
			  ({iPAddress, IP}, Acc) when is_list(IP) ->
			       IPstr = siphost:makeip(list_to_tuple(IP)),
			       [IPstr | Acc];
			  (_NonDNSname, Acc) ->
			       Acc
		       end, [], Decoded),
    case This of
	[] ->
	    %% we found no dNSName or iPAddress tuples in Decoded
	    get_host_altnames(Type, T, Res);
	_ ->
	    get_host_altnames(Type, T, Res ++ This)
    end;
get_host_altnames(_Type, [], Res) ->
    {ok, Res}.


%%--------------------------------------------------------------------
%% @spec    (ValidNames, Subject, AltNames, Reject) -> true | false
%%
%%            ValidNames = [string()] "the hostname(s) we want to make sure the subjectAltName matches."
%%            Subject    = #ssl_conn_subject{}
%%            AltNames   = [string()]
%%            Reject     = true | false "reject if name does not match or just log?"
%%
%% @doc     Check if a socket is 'acceptable'. For SSL, this means
%%          verify that the subjectAltName/commonName is in the list
%%          of names we expect.
%% @end
%%--------------------------------------------------------------------
is_valid_ssl_certname(ValidNames, Subject, AltNames, Reject) when is_list(ValidNames),
								  is_record(Subject, ssl_conn_subject),
								  is_list(AltNames) ->
    CommonName = Subject#ssl_conn_subject.commonName,
    {ok, DoesMatch, MatchingName} = get_matching_altname(ValidNames, CommonName, AltNames),
    case {DoesMatch, Reject} of
	{true, _Reject} ->
	    logger:log(debug, "Transport layer: Verified that subjectAltName/CN (~p) is in list of "
		       "valid names for this certificate (~p)", [MatchingName, ValidNames]),
	    true;
	{false, true} ->
	    logger:log(normal, "Transport layer: Rejecting SSL certificate with commonName ~p and "
		       "subjectAltName ~p since it does not match list of valid names for this connection : ~p",
		       [CommonName, AltNames, ValidNames]),
	    false;
	{false, false} ->
	    logger:log(debug, "Transport layer: Warning: SSL certificate with commonName ~p and "
		       "subjectAltName ~p does not match list of valid names for this connection : ~p",
		       [CommonName, AltNames, ValidNames]),
	    true
    end;
is_valid_ssl_certname(undefined, Subject, AltNames, true) when is_record(Subject, ssl_conn_subject),
							       is_list(AltNames) ->
    logger:log(debug, "Transport layer: Rejecting connection with SSL commonName ~p and subjectAltName ~p "
	       "since list of valid names is undefined", [Subject#ssl_conn_subject.commonName, AltNames]),
    false;
is_valid_ssl_certname(undefined, Subject, AltNames, false) when is_record(Subject, ssl_conn_subject),
								is_list(AltNames) ->
    logger:log(debug, "Transport layer: Warning: Not matching commonName ~p and subjectAltName ~p against "
	       "a list of valid names, since list is undefined",
	       [Subject#ssl_conn_subject.commonName, AltNames]),
    true.

%% part of is_valid_ssl_certname/4
%% Returns : {ok, true, MatchingName} |
%%           {ok, false, undefined}
get_matching_altname(ValidNames, CommonName, [H | T]) when is_list(ValidNames), is_list(CommonName) ->
    case util:casegrep(H, ValidNames) of
	true ->
	    {ok, true, H};
	false ->
	    get_matching_altname(ValidNames, CommonName, T)
    end;
get_matching_altname(ValidNames, CommonName, []) when is_list(ValidNames), is_list(CommonName) ->
    %% no matching subjectAltName, check commonName
    case util:casegrep(CommonName, ValidNames) of
	true ->
	    {ok, true, CommonName};
	false ->
	    {ok, false, undefined}
    end.


%%====================================================================
%% Test functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> ok
%%
%% @doc     autotest callback
%% @hidden
%% @end
%%--------------------------------------------------------------------
test() ->
    autotest:mark(?LINE, "SSL certificates - 0"),

    %% To test our SSL certificate parsing and validation functions, we need a test certificate.
    %% The binary TestCert1 that we define below is a certificate with Subject information, and
    %% also a number of (Netscape) extensions that causes Erlangs built-in certificate-to-erlang-term
    %% parser to crash (because it does not recognize those old extensions). We want to be liberal
    %% in what we accept at the application layer (the SSL socket driver must have already accepted
    %% the certificate since it passes it to us), so we have our own SSL subject formatting and
    %% extension locating code.
    %%
    %% Certificate:
    %%     Data:
    %%         Version: 3 (0x2)
    %%         Serial Number: 1 (0x1)
    %%         Signature Algorithm: sha1WithRSAEncryption
    %%         Issuer: CN=Yxa test CA
    %%         Validity
    %%             Not Before: Sep 28 07:45:54 2005 GMT
    %%             Not After : Sep 28 07:45:54 2006 GMT
    %%         Subject: C=SE, O=Stockholms universitet, CN=yxa-test-cert1.example.org
    %%         Subject Public Key Info:
    %%             Public Key Algorithm: rsaEncryption
    %%             RSA Public Key: (1024 bit)
    %%                 Modulus (1024 bit):
    %%                     ...
    %%                 Exponent: 65537 (0x10001)
    %%         X509v3 extensions:
    %%             Netscape Cert Type:
    %%                 SSL Client, SSL Server
    %%             X509v3 Key Usage:
    %%                 Digital Signature, Non Repudiation, Key Encipherment
    %%             X509v3 Extended Key Usage:
    %%                 TLS Web Client Authentication, TLS Web Server Authentication
    %%             Netscape CA Revocation Url:
    %%                 http://ca.example.com/crl-v1.crl
    %%             X509v3 Subject Key Identifier:
    %%                 BC:E7:26:71:52:68:1E:C6:13:2D:F1:4C:6D:2A:B6:47:30:7D:7B:D3
    %%             X509v3 Authority Key Identifier:
    %%                 keyid:26:F7:64:BE:4A:5F:75:52:BC:CB:32:89:09:95:45:02:F4:A4:58:2A
    %%                 DirName:/CN=Yxa test CA
    %%                 serial:D7:E9:41:4A:33:5E:7C:E8
    %%
    %%             Authority Information Access:
    %%                 CA Issuers - URI:http://ca.example.com/ca.crt
    %%
    %%             X509v3 CRL Distribution Points:
    %%                 URI:http://ca.example.com/crl-v2.crl
    %%
    %%             X509v3 Certificate Policies:
    %%                 Policy: 1.1.1.1.1
    %%                   CPS: http://ca.example.com/CPS
    %%                   User Notice:
    %%                     Explicit Text: Limited Liability, see http://ca.example.com/CP
    %%
    %%             X509v3 Issuer Alternative Name:
    %%                 email:ca@example.com, URI:http://ca.example.com
    %%             X509v3 Subject Alternative Name:
    %%                 email:test@yxa-test-cert1.example.org,
    %%                 DNS:yxa-test-cert1.example.org,
    %%                 DNS:example.org,
    %%                 DNS:sip.example.org,
    %%                 DNS:localhost,
    %%                 IP Address:192.0.2.78,
    %%                 IP Address:127.0.0.1
    %%     Signature Algorithm: sha1WithRSAEncryption
    %%     ...
    TestCert1_der =
	<<48,130,4,18,48,130,3,188,160,3,2,1,2,2,1,1,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,48,22,49,20,48,18,6,
	 3,85,4,3,19,11,89,120,97,32,116,101,115,116,32,67,65,48,30,23,13,48,53,48,57,50,56,48,55,52,53,53,52,90,23,
	 13,48,54,48,57,50,56,48,55,52,53,53,52,90,48,83,49,11,48,9,6,3,85,4,6,19,2,83,69,49,31,48,29,6,3,85,4,10,19,
	 22,83,116,111,99,107,104,111,108,109,115,32,117,110,105,118,101,114,115,105,116,101,116,49,35,48,33,6,3,85,
	 4,3,19,26,121,120,97,45,116,101,115,116,45,99,101,114,116,49,46,101,120,97,109,112,108,101,46,111,114,103,
	 48,129,159,48,13,6,9,42,134,72,134,247,13,1,1,1,5,0,3,129,141,0,48,129,137,2,129,129,0,222,85,71,54,9,144,
	 139,255,251,249,67,39,37,170,201,90,2,11,146,126,240,112,98,246,86,127,56,0,210,231,29,84,6,170,148,158,162,
	 107,9,10,172,26,148,115,82,95,136,131,28,17,246,57,156,91,56,69,8,117,206,184,171,119,193,64,236,93,227,185,
	 38,73,239,235,190,95,91,113,28,127,146,172,48,204,214,119,20,189,72,207,119,159,134,192,155,102,220,235,116,
	 243,254,88,155,40,116,28,193,36,9,76,179,242,225,201,6,193,164,54,147,209,241,164,185,255,173,161,117,199,
	 234,125,2,3,1,0,1,163,130,2,114,48,130,2,110,48,17,6,9,96,134,72,1,134,248,66,1,1,4,4,3,2,6,192,48,11,6,3,
	 85,29,15,4,4,3,2,5,224,48,29,6,3,85,29,37,4,22,48,20,6,8,43,6,1,5,5,7,3,2,6,8,43,6,1,5,5,7,3,1,48,47,6,9,96,
	 134,72,1,134,248,66,1,4,4,34,22,32,104,116,116,112,58,47,47,99,97,46,101,120,97,109,112,108,101,46,99,111,
	 109,47,99,114,108,45,118,49,46,99,114,108,48,29,6,3,85,29,14,4,22,4,20,188,231,38,113,82,104,30,198,19,45,
	 241,76,109,42,182,71,48,125,123,211,48,70,6,3,85,29,35,4,63,48,61,128,20,38,247,100,190,74,95,117,82,188,203,
	 50,137,9,149,69,2,244,164,88,42,161,26,164,24,48,22,49,20,48,18,6,3,85,4,3,19,11,89,120,97,32,116,101,115,
	 116,32,67,65,130,9,0,215,233,65,74,51,94,124,232,48,56,6,8,43,6,1,5,5,7,1,1,4,44,48,42,48,40,6,8,43,6,1,5,
	 5,7,48,2,134,28,104,116,116,112,58,47,47,99,97,46,101,120,97,109,112,108,101,46,99,111,109,47,99,97,46,99,
	 114,116,48,49,6,3,85,29,31,4,42,48,40,48,38,160,36,160,34,134,32,104,116,116,112,58,47,47,99,97,46,101,120,
	 97,109,112,108,101,46,99,111,109,47,99,114,108,45,118,50,46,99,114,108,48,121,6,3,85,29,32,4,114,48,112,48,
	 110,6,4,41,1,1,1,48,102,48,37,6,8,43,6,1,5,5,7,2,1,22,25,104,116,116,112,58,47,47,99,97,46,101,120,97,109,
	 112,108,101,46,99,111,109,47,67,80,83,48,61,6,8,43,6,1,5,5,7,2,2,48,49,26,47,76,105,109,105,116,101,100,32,
	 76,105,97,98,105,108,105,116,121,44,32,115,101,101,32,104,116,116,112,58,47,47,99,97,46,101,120,97,109,112,
	 108,101,46,99,111,109,47,67,80,48,48,6,3,85,29,18,4,41,48,39,129,14,99,97,64,101,120,97,109,112,108,101,46,
	 99,111,109,134,21,104,116,116,112,58,47,47,99,97,46,101,120,97,109,112,108,101,46,99,111,109,48,123,6,3,85,
	 29,17,4,116,48,114,129,31,116,101,115,116,64,121,120,97,45,116,101,115,116,45,99,101,114,116,49,46,101,120,
	 97,109,112,108,101,46,111,114,103,130,26,121,120,97,45,116,101,115,116,45,99,101,114,116,49,46,101,120,97,
	 109,112,108,101,46,111,114,103,130,11,101,120,97,109,112,108,101,46,111,114,103,130,15,115,105,112,46,101,
	 120,97,109,112,108,101,46,111,114,103,130,9,108,111,99,97,108,104,111,115,116,135,4,192,0,2,78,135,4,127,0,
	 0,1,48,13,6,9,42,134,72,134,247,13,1,1,5,5,0,3,65,0,144,103,62,16,155,150,76,155,192,124,95,177,48,81,223,
	 18,241,73,68,30,183,225,22,14,172,193,251,254,99,104,222,249,240,41,28,33,81,4,155,105,29,165,75,214,231,150,
	 100,157,176,135,199,38,70,213,107,101,39,244,183,149,103,223,131,137>>,

    {ok, TestCert1} = ssl_pkix:decode_cert(TestCert1_der, [pkix]),


    %% test decode_ssl_rdnseq(RdnSequence)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "decode_ssl_rdnseq/1 - 1"),
    {ok, [{'at-commonName',       "yxa-test-cert1.example.org"},
	  {'at-organizationName', "Stockholms universitet"},
	  {'at-countryName',      "SE"}
	 ]} = decode_ssl_rdnseq((TestCert1#'Certificate'.tbsCertificate)#'TBSCertificate'.subject),

    autotest:mark(?LINE, "decode_ssl_rdnseq/1 - 2"),
    {ok, [{'at-commonName', "Yxa test CA"}]} =
	decode_ssl_rdnseq((TestCert1#'Certificate'.tbsCertificate)#'TBSCertificate'.issuer),

    autotest:mark(?LINE, "decode_ssl_rdnseq/1 - 3"),
    {error, "input to decode_ssl_rdnseq is not an rdnSequence tuple"} = decode_ssl_rdnseq(foo),

    autotest:mark(?LINE, "decode_ssl_rdnseq/1 - 4"),
    {'EXIT', {function_clause, _}} = (catch decode_ssl_rdnseq({rdnSequence, [foo]})),

    autotest:mark(?LINE, "decode_ssl_rdnseq/1 - 5"),
    DecodeInvalidAttrList1 = [[#'AttributeTypeAndValue'{type  = test,
							value = foo
						       }
			     ]],
    {error, "failed parsing rdnSequence entry"} = decode_ssl_rdnseq({rdnSequence, DecodeInvalidAttrList1}),


    %% test get_ssl_peer_info_subject(Cert)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ssl_peer_info_subject/1 - 1.0"),
    {ok, TestCert1_Subject} = get_ssl_peer_info_subject(TestCert1),

    autotest:mark(?LINE, "get_ssl_peer_info_subject/1 - 1.1"),
    %% verify results
    #ssl_conn_subject{countryName	= "SE",
		      organizationName	= "Stockholms universitet",
		      commonName	= "yxa-test-cert1.example.org",
		      description	= "C=SE/O=Stockholms universitet/CN=yxa-test-cert1.example.org"
		     } = TestCert1_Subject,


    %% test get_ssl_peer_info_host_altnames(Cert)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ssl_peer_info_host_altnames/1 - 1.0"),
    {ok, TestCert1_AltNames} = get_ssl_peer_info_host_altnames(TestCert1),

    autotest:mark(?LINE, "get_ssl_peer_info_host_altnames/1 - 1.1"),
    %% verify results
    ["127.0.0.1",
     "192.0.2.78",
     "localhost",
     "sip.example.org",
     "example.org",
     "yxa-test-cert1.example.org"] = TestCert1_AltNames,


    %% test get_ssl_peer_info(Socket, Proto, IP, Port)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ssl_peer_info2/4 - 1"),
    %% test error handling
    {error, "Certificate decoding error"} = get_ssl_peer_info(testing, tls6, "[2001:6b0:5:9876::6789]", 50000),


    %% test get_ssl_peer_info2(Proto, IP, Port, PeerCertRes)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "get_ssl_peer_info2/4 - 1"),
    %% test normal case
    {ok, TestCert1_Subject, TestCert1_AltNames} =
	get_ssl_peer_info2(tls, "192.0.2.11", 50000, {ok, TestCert1}),

    autotest:mark(?LINE, "get_ssl_peer_info2/4 - 2"),
    %% test error handling
    {error, "Testing"} = get_ssl_peer_info2(tls6, "192.0.2.11", 50000, {error, "Testing"}),


    %% test is_valid_ssl_certname(ValidNames, Subject, AltNames, Reject)
    %%--------------------------------------------------------------------
    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 0"),
    IsValidSubject1 = #ssl_conn_subject{commonName = "commonname.example.org"},

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 1"),
    %% test non-matching name #1 (reject: true)
    false = is_valid_ssl_certname(["example.org"], IsValidSubject1, [], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 2"),
    %% test non-matching name #2 (reject: true)
    false = is_valid_ssl_certname(["example.org"], IsValidSubject1, ["altname.testing"], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 3"),
    %% test non-matching name #1 (reject: false)
    true = is_valid_ssl_certname(["example.org"], IsValidSubject1, [], false),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 4"),
    %% test non-matching name #2 (reject: false)
    true = is_valid_ssl_certname(["example.org"], IsValidSubject1, ["altname.testing"], false),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 5"),
    %% test matching commonName
    true = is_valid_ssl_certname(["ComMonName.example.org"], IsValidSubject1, ["example.org", "192.0.2.12"], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 6"),
    %% test matching subjectAltName
    true = is_valid_ssl_certname(["example.org"], IsValidSubject1,
				 ["example.org", "commonName.example.org", "192.0.2.12"], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 7"),
    %% test without list of valid names (reject: true)
    false = is_valid_ssl_certname(undefined, IsValidSubject1, [], true),

    autotest:mark(?LINE, "is_valid_ssl_certname/4 - 8"),
    %% test without list of valid names (reject: false)
    true = is_valid_ssl_certname(undefined, IsValidSubject1, [], false),

    ok.
