#!/usr/bin/env perl
#
# testclient.pl is a minimalistic SIP user agent used in testing
# YXA - the Erlang SIP server/stack
#
# Author : Fredrik Thulin <ft@it.su.se>
#

use strict;
use Digest::MD5 qw(md5 md5_hex md5_base64);;
#use IO::Socket;
use Socket;
use Sys::Hostname;
use FileHandle;
use Getopt::Std;
use vars qw ($opt_h $opt_q $opt_u $opt_2 $opt_3);

getopts ('hqu23');

if (defined ($opt_h)) {
    die (<<EOT);
Syntax: $0 [options] [test name ...]

    Options :

        -h   this text
	-q   quiet mode, less verbose
	-u   use UDP
	-2   do not generate RFC3261 branches
	-3   always generate RFC3261 branches

    Special test names :

        ALL-STANDARD\t All the standard tests defined in testclient.pl
        ALL-LOCAL\t All the tests you have configured in your local configuration file
	ALL\t\t All tests, both standard and local. This is the default

EOT
}

my $hostname = hostname();

# Set all these in your local configuration file
my $testserver = "testserver variable not set";
my $incomingproxy = "incomingproxy variable not set";
my $pstnproxy = "pstnproxy variable not set";
my $mydomain = "mydomain variable not set";
my $default_from = "\"SIP testing client\" <sip:testclient.pl\@$hostname>";
my $testuser_address = "testuser_address variable not set";
my $testuser_user = "testuser_user variable not set";
my $testuser_password = "testuser_password variable not set";

my %local_tests;

read_local_config();

my %standard_tests = (
	# incomingproxy tests

	     "INVALID FROM URL" =>
		{
		    # From: is missing brackets and sip:
				Method	=> "INVITE",
		     		From	=> "testclient.pl\@$hostname",
		     		To	=> "invalid_from_url\@$mydomain",
		     		sendto	=> $incomingproxy,
				expect	=> 404
		},
	     "INVALID TO URL" =>
		{
				Method	=> "INVITE",
				RequestURI => "sip:invalid_to_url\@$mydomain",
		     		From	=> $default_from,
		     		To	=> "foo:invalid_to_url\@$mydomain",
		     		sendto	=> $incomingproxy,
				expect	=> 404
		},
	     "INVALID URI-SCHEME" =>
		{
				Method	=> "INVITE",
		     		From	=> $default_from,
		     		To	=> "foo:invalid_requesturischeme\@$mydomain",
		     		sendto	=> $incomingproxy,
				expect	=> 416,
		},

	     "INVALID CSEQ-METHOD" =>
		{
				Method	=> "OPTIONS",
		     		From	=> $default_from,
		     		To	=> "invalid_cseq\@$mydomain",
		     		sendto	=> $incomingproxy,
				expect	=> 400,
				CSeqMethod => "SUBSCRIBE",
		},

	     "INVALID CSEQ" =>
		{
				Method	=> "OPTIONS",
		     		From	=> $default_from,
		     		To	=> "invalid_cseq\@$mydomain",
		     		sendto	=> $incomingproxy,
				expect	=> 400,
				CSeqMethod => "OPTIONS foo",
		},

	     "PROXY REQUIRE TEST" =>
		{
				Method => "INVITE",
		     		From	=> $default_from,
		     		To	=> "test\@$mydomain",
				Header  => "Proxy-Require: YouDontSupportThis\n",
		     		sendto	=> $incomingproxy,
				expect	=> 420,
				expecth => '^Unsupported:.*\sYouDontSupportThis',
		},

	     "REGISTER" =>
		{
				Method	=> "REGISTER",
		     		From	=> $default_from,
		     		To	=> $testuser_address,
				Header  => "Expires: 5\n",
		     		sendto	=> $incomingproxy,
				expect	=> 200,
				user	=> $testuser_user,
				pw	=> $testuser_password
		},

	     "MULTI-REGISTER" =>
		{
				Method	=> "REGISTER",
		     		From	=> $default_from,
		     		To	=> $testuser_address,
				Header  => "Contact: <sip:multiregister\@$testserver:12345;eXpires=10;user=phone;foo=bar>\nExpires: 5\n",
		     		sendto	=> $incomingproxy,
				expect	=> 200,
				user	=> $testuser_user,
				pw	=> $testuser_password
		},

	     "PROXY REGISTER" =>
		{
				Method	=> "REGISTER",
		     		From	=> $default_from,
		     		To	=> "test\@$testserver",
				Header  => "Expires: 0\n",
		     		sendto	=> $incomingproxy,
				expect	=> 200,
				user	=> $testuser_user,
				pw	=> $testuser_password
		},


	     "REGISTER WILDCARD" =>
		{
				Method	=> "REGISTER",
		     		From	=> $default_from,
		     		To	=> $testuser_address,
		     		Contact	=> "*",
				Header  => "Expires: 0\n",
		     		sendto	=> $incomingproxy,
				expect	=> 200,
				user	=> $testuser_user,
				pw	=> $testuser_password
		},

	     "REGISTER WILDCARD WITHOUT EXPIRES" =>
		{
				Method	=> "REGISTER",
		     		From	=> $default_from,
		     		To	=> $testuser_address,
		     		Contact	=> "*",
		     		sendto	=> $incomingproxy,
				expect	=> 400,
				user	=> $testuser_user,
				pw	=> $testuser_password
		},

	     "REGISTER WILDCARD WITHOUT EXPIRES, WITH EXPIRES PARAMETER" =>
		{
				Method	=> "REGISTER",
		     		From	=> $default_from,
		     		To	=> $testuser_address,
		     		Contact	=> "*;expires=0",
		     		sendto	=> $incomingproxy,
				expect	=> 400,
				user	=> $testuser_user,
				pw	=> $testuser_password
		},

	     "REGISTER WILDCARD WITH INVALID EXPIRES PARAMETER" =>
		{
				Method	=> "REGISTER",
		     		From	=> $default_from,
		     		To	=> $testuser_address,
		     		Contact	=> "*;expires=1",
				Header	=> "Expires: 0\n",
		     		sendto	=> $incomingproxy,
				expect	=> 400,
				user	=> $testuser_user,
				pw	=> $testuser_password
		},

	     "REGISTER WILDCARD AND OTHER" =>
		{
				Method	=> "REGISTER",
		     		From	=> $default_from,
		     		To	=> $testuser_address,
				Header  => "Contact: *;expires=0\nExpires: 1\n",
		     		sendto	=> $incomingproxy,
				expect	=> 400,
				user	=> $testuser_user,
				pw	=> $testuser_password
		},

	     "MALFORMED ACK" =>
		{
				Method	=> "ACK",
		     		From	=> $default_from,
		     		To	=> $testuser_address,
				RequestURI => "sip:$incomingproxy;lr=true",
		     		sendto	=> $incomingproxy,
				expect	=> 400
		},


	# testserver tests

	     "TESTSERVER REGULAR INVITE" =>
		{
				Method	=> "INVITE",
				From	=> $default_from,
				To	=> "test\@$testserver",
				sendto	=> $testserver,
				expect  => '^486 Busy Here \(test\)$'
		},

	     "TESTSERVER REQUIRE TEST" =>
		{
				Method => "INVITE",
		     		From	=> $default_from,
		     		To	=> "test\@$testserver",
				Header  => "Require: YouDontSupportThis\n",
		     		sendto	=> $testserver,
				expect	=> 420,
				expecth => '^Unsupported:.*\sYouDontSupportThis'
		},


	# combined tests



             "ROUTE PROCESSING" =>
		{
				Method	=> "MESSAGE",
				From	=> $default_from,
				To	=> "relay-test\@$testserver",
                                Header  => "Route: <sip:$incomingproxy>\n",
				sendto	=> $incomingproxy,
				user	=> $testuser_user,
				pw	=> $testuser_password,
				expect  => '^486 Busy Here \(relay-test\)$'
		},


             "STRICT ROUTER TRAVERSAL" =>
		{
				Method	=> "INVITE",
                                RequestURI => "sip:$incomingproxy;lr=true",
				From	=> $default_from,
				To	=> "relay-test\@$testserver",
                                Header  => "Route: <sip:relay-test\@$testserver>\n",
				sendto	=> $incomingproxy,
				user	=> $testuser_user,
				pw	=> $testuser_password,
				expect  => '^486 Busy Here \(relay-test\)$'
		},


	     "RELAY, AUTH" =>
		{
				Method	=> "INVITE",
				From	=> $default_from,
				To	=> "relay-test\@$testserver",
				sendto	=> $incomingproxy,
				user	=> $testuser_user,
				pw	=> $testuser_password,
				expect  => '^486 Busy Here \(relay-test\)$'
		}


	    );

my $quiet = defined ($opt_q);
my $use_udp = defined ($opt_u);
my $rfc3261_branch = 'yes';

if ($opt_2 and $opt_3) {
    die ("$0: Specifying both -2 and -3 is not valid\n");
}

$rfc3261_branch = 'no' if ($opt_2);
$rfc3261_branch = 'must' if ($opt_3);

my @testlist;
foreach my $t (@ARGV) {
    $t =~ s/,$//g; # remove trailing commas to make copy-paste work
    if ($t eq 'ALL' or $t eq 'ALL-STANDARD' or $t eq 'ALL-LOCAL') {
	@testlist = ($t);
	last;
    }
    push (@testlist, $t);
}
push (@testlist, 'ALL') if (0 == @testlist);

my $callid_starttime = time();

my ($name, $params, @standard_failed, @local_failed);
my ($standard_okcount, $standard_testcount) = (0, 0);
my ($local_okcount, $local_testcount) = (0, 0);
my $callid_seq = 0;

# Standard tests
foreach $name (keys %standard_tests) {
    if ($testlist[0] ne 'ALL' and $testlist[0] ne 'ALL-STANDARD') {
	next if (! grep (/^$name$/, @testlist));
    }

    my $skip = 0;
    # Check that none of the variables for this test ends with " not set", which
    # means the user has not configured that variable and the test cannot be executed
    foreach my $t (keys %{$standard_tests{$name}}) {
	my $v = $standard_tests{$name}{$t};
	if ($v =~ /([_A-Za-z]+) variable not set$/) {
	    warn ("Skipped standard test '$name' : \$${1} not set\n");
	    $skip = 1;
	}
    }

    if ($standard_tests{$name}{branch} =~ /.*3261$/) {
	if ($rfc3261_branch eq 'no') {
	    warn ("Skipped standard test '$name' because it requires RFC3261 branch\n");
	    $skip = 1;
	}
    } elsif ($standard_tests{$name}{branch} =~ /.*2543$/) {
	if ($rfc3261_branch eq 'must') {
	    warn ("Skipped standard test '$name' because it requires RFC2543 branch\n");
	    $skip = 1;
	}
    }

    next if ($skip);

    $standard_testcount++;

    # Don't restart testclient.pl so many times that you get the same PID twice the same second, please ;)
    my $CallID = "time${callid_starttime}-pid$$-seq${callid_seq}\@$hostname";

    my $branch = 'z9hG4bK-yxa-testclient-' . md5_hex ($CallID);
    my $fromtag = ';tag=ft-' . substr (md5_hex ("ft-${CallID}"), 1, 8);

    if ($standard_tests{$name}{branch} =~ /.*2543$/ or
	$rfc3261_branch eq 'no') {
	$branch = '';
    }

    $callid_seq++;

    my %params = (testname	=> $name,
		  callid_seq	=> $callid_seq,
		  testparamsref	=> $standard_tests{$name},
		  use_udp	=> $use_udp,
		  hostname	=> $hostname,
		  cseq		=> '1',
		  callid	=> $CallID,
		  branch	=> $branch,
		  authresponse	=> undef,
		  from_tag	=> $fromtag
		  );

    my $res += perform_test ($quiet, \%params);
    $standard_okcount += $res;

    push (@standard_failed, "\"$name\"") if (! $res);
}

# Local tests
foreach $name (keys %local_tests) {
    if ($testlist[0] ne 'ALL' and $testlist[0] ne 'ALL-LOCAL') {
	next if (! grep (/^$name$/, @testlist));
    }

    my $skip = 0;
    # Check that none of the variables for this test ends with " not set", which
    # means the user has not configured that variable and the test cannot be executed
    foreach my $t (keys %{$local_tests{$name}}) {
	my $v = $local_tests{$name}{$t};
	if ($v =~ /([_A-Za-z]+) variable not set$/) {
	    warn ("Skipped local test '$name' : \$${1} not set\n");
	    $skip = 1;
	}
    }

    if ($local_tests{$name}{branch} =~ /.*3261$/) {
	if ($rfc3261_branch eq 'no') {
	    warn ("Skipped local test '$name' because it requires RFC3261 branch\n");
	    $skip = 1;
	}
    } elsif ($local_tests{$name}{branch} =~ /.*2543$/) {
	if ($rfc3261_branch eq 'must') {
	    warn ("Skipped local test '$name' because it requires RFC2543 branch\n");
	    $skip = 1;
	}
    }

    next if ($skip);

    $local_testcount++;

    # Don't restart testclient.pl so many times that you get the same PID twice the same second, please ;)
    my $CallID = "time${callid_starttime}-pid$$-seq${callid_seq}\@$hostname";

    my $branch = 'z9hG4bK-yxa-testclient-' . md5_hex ($CallID);
    my $fromtag = ';tag=ft-' . substr (md5_hex ("ft-${CallID}"), 1, 8);

    if ($local_tests{$name}{branch} =~ /.*2543$/ or
	$rfc3261_branch eq 'no') {
	$branch = '';
    }

    $callid_seq++;

    my %params = (testname	=> $name,
		  callid_seq	=> $callid_seq,
		  testparamsref	=> $local_tests{$name},
		  use_udp	=> $use_udp,
		  hostname	=> $hostname,
		  cseq		=> '1',
		  callid	=> $CallID,
		  branch	=> $branch,
		  authresponse	=> undef,
		  from_tag	=> $fromtag
		  );

    my $res += perform_test ($quiet, \%params);

    $local_okcount += $res;

    push (@local_failed, "\"$name\"") if (! $res);
}


my $standard_failstr = '';
$standard_failstr = " (" . ($standard_testcount - $standard_okcount) . " FAILED)" if ($standard_testcount != $standard_okcount);

print ("\n---\n\n$standard_okcount out of $standard_testcount standard test succeeded$standard_failstr\n");

print ("FAILED standard tests: ", join (", ", @standard_failed), "\n\n") if (0 != @standard_failed);

my $local_failstr = '';
$local_failstr = " (" . ($local_testcount - $local_okcount) . " FAILED)" if ($local_testcount != $local_okcount);

print ("\n---\n\n$local_okcount out of $local_testcount local test succeeded$local_failstr\n");

print ("FAILED local tests: ", join (", ", @local_failed), "\n\n") if (0 != @local_failed);

exit ($standard_failstr eq '' and $local_failstr eq '');


sub perform_test
{
    my $quiet = shift;
    my $params_ref = shift;

    my $testname		= $$params_ref{testname};
    my $callid_seq		= $$params_ref{callid_seq};
    my $testparamsref		= $$params_ref{testparamsref};
    my $use_udp			= $$params_ref{use_udp};
    my $hostname		= $$params_ref{hostname};
    my $CSeq			= $$params_ref{cseq};
    my $CallID			= $$params_ref{callid};
    my $branch			= $$params_ref{branch};
    my $authrequestresponse	= $$params_ref{authresponse};
    my $FromTag			= $$params_ref{from_tag};

    my $proto;
    my %testparams = %{$testparamsref};

    my ($msg, $response, $code, $text);

    my $dst = $testparams{sendto} || $testserver;

    my $Socket = new FileHandle;

    if ($use_udp) {
	my $iaddr = gethostbyname(hostname());
	my $paddr = sockaddr_in(0, $iaddr);	# 0 means let kernel pick
	socket($Socket, PF_INET, SOCK_DGRAM, getprotobyname("udp")) or die "socket: $!";
	bind($Socket, $paddr) || die "bind: $!";
	$proto = "UDP";
    } else {
	my ($sendto, $sendto_ip, $sendto_port) = resolve_destination ($dst);
	warn ("Destination '$dst' resolved to $sendto_ip:$sendto_port\n") unless ($quiet);

	my $dst = sockaddr_in($sendto_port, inet_aton($sendto_ip));

	my $iaddr = inet_aton($sendto_ip);
	#my $paddr = sockaddr_in(0, $dst); # 0 means let kernel pick local port
	my $paddr = sockaddr_in($sendto_port, $iaddr);

	socket($Socket, PF_INET, SOCK_STREAM, getprotobyname("tcp")) or die "socket: $!";
	connect($Socket, $paddr) or die ("Failed connecting to tcp:$sendto_ip:$sendto_port: $!\n"), return undef;
	warn ("Connected to $sendto_ip:$sendto_port\n");
	$proto = "TCP";
    }

    my $mysockaddr = getsockname($Socket);
    my ($family, $localport, $myaddr) = unpack('S n a4 x8', $mysockaddr);

    my $Method = check_method ($testparams{Method} || 'INVITE');
    my $CSeqMethod = $testparams{CSeqMethod} || $Method;
    my $To = make_url($testparams{To}) || "\"Testclient\" <sip:test\@$testserver>";
    my $From = $testparams{From} || "\"SIP testing program\" <sip:testclient\@$hostname:$localport>";
    my $Contact = $testparams{Contact} || "<sip:testclient\@$hostname:$localport>";
    my $MaxForwards = defined ($testparams{MaxForwards})?$testparams{MaxForwards}:'10';  # RFC3261 says 70 but that is not suitable for testing
    my $testheader = $testparams{Header} || '';
    my $sipuri = $testparams{RequestURI} || parse_sipurl ($To) or warn ("Could not create a Request URI!\n"), return 0;
    my $body = $testparams{Body} || '';

    my $proxyauthheader = '';
    if ($authrequestresponse) {
	my $username = $testparams{user};
	my $password = $testparams{pw};

	my $response_headername = 'Proxy-Authorization';
	my $pa = fetch_header ('Proxy-Authenticate', $authrequestresponse);
	if (! $pa) {
	    $pa = fetch_header ('WWW-Authenticate', $authrequestresponse);
	    $response_headername = 'Authorization';
	}
	warn ("FAILED, got '$code $text' but no Proxy-Authenticate header\n"), return 0 if (! $pa);
	my $mda = auth_parse($pa);

	if (defined ($username) and
		    defined ($password)) {

	    my $realm = $mda->{realm};
	    my $nonce = $mda->{nonce};
	    my $opaque = $mda->{opaque};

	    my ($hostname, $ip, $port) = resolve_destination ($dst);
	    my $uri = "sip:$ip";

	    my $A1 = md5_hex ("$username:$realm:$password");
	    my $A2 = md5_hex ("$Method:$uri");
	    my $response = md5_hex ("$A1:$nonce:$A2");

	    $proxyauthheader = "$response_headername: Digest username=\"$username\",realm=\"$realm\",uri=\"$uri\",response=\"$response\",nonce=\"$nonce\",opaque=\"$opaque\",algorithm=md5\n";

	    # must make branch different since the new INVITE is a separate transaction
	    $branch .= ".1" if ($branch);
	} else {
	    warn ("FAILED, Could not find/parse Proxy-Authenticate/WWW-Authenticate in response requiring authentication\n");
	    return 0;
	}
    }

    my $my_via = "SIP/2.0/$proto $hostname:$localport";

    $my_via .= ";branch=$branch" if ($branch);
    my $content_length = length($body);
    my $msgbody = "$body";

    $msg = "$Method $sipuri SIP/2.0
Via: ${my_via}
From: ${From}${FromTag}
To: $To
Call-ID: $CallID
CSeq: $CSeq $CSeqMethod
Contact: $Contact
Content-Length: $content_length
Max-Forwards: $MaxForwards
User-Agent: testclient.pl
$testheader$proxyauthheader";

    warn ("  Retrying with authentication :\n") if ($authrequestresponse);
    warn ("---\nTest '$testname' (send to $dst) :\n") if (! $authrequestresponse);

    warn ("	SEND: $Method $sipuri\n") if ($quiet);
    send_message ($Socket, $use_udp, $testname, $msg, $msgbody, $dst, $quiet) or close ($Socket), return 0;
    if ($testparams{cancel_immediately} and $Method eq 'INVITE') {
	my $msg2 = $msg;
	$msg2 =~ s/^INVITE /CANCEL /o;
	$msg2 =~ s/^CSeq: $CSeq INVITE$/CSeq: $CSeq CANCEL/m;
	send_message ($Socket, $use_udp, $testname, $msg2, "", $dst, $quiet) or close ($Socket), return 0;
    }

    $response = read_response ($Socket, $use_udp, $testname, $CallID, $quiet) or close ($Socket), return 0;

    close ($Socket), return 1 if (! defined ($testparams{expect}));

    ($code, $text) = get_response ($response);
    if (! $code) {
	# show response if not already shown
	warn ("FAILED, Can't parse response:\t\n" .  join ("\n\t", split ("\n", $response)) . "\n") if ($quiet);
	warn ("FAILED, Can't parse response\n") if (! $quiet);
	return 0;
    }

    if ($Method eq 'INVITE' and $code >= 200) {
	send_ack($response, $my_via, $sipuri, $Socket, $use_udp, $testname, $testheader, $dst, $quiet) or close ($Socket), return 0;
    }

    my $match = 0;
    if (int ($testparams{expect}) > 0) {
	$match = 1 if ($code == $testparams{expect});
    } else {
	$match = regexp_match ("$code $text", $testparams{expect});
    }

    if (! $match) {
	if ($code eq '407' or $code eq '401') {
	    # we got a 407 Proxy Authorization Required or
	    # 401 Authentication Required - just make sure
	    # that was not what the test was expected to result in
	    close ($Socket);

	    if ($authrequestresponse) {
		warn ("FAILED - could not authenticate with the credentials I have for this test\n");
		return 0;
	    }
	    warn ("\tNeeds authentication...\n") if (! $quiet);

	    $CSeq++;

	    if (defined ($testparams{user}) and defined ($testparams{pw})) {
		$$params_ref{authresponse} = $response;
		#return perform_test ($testname, $callid_seq, $paramref, $use_udp, $quiet, $hostname, $CSeq, $CallID, $branch, $response);
		return perform_test ($quiet, $params_ref);
	    } else {
		warn ("FAILED - got '$code $text' but I have no credentials for this test\n");
		close ($Socket);
		return 0;
	    }
	}

	if (int ($testparams{expect}) > 0) {
	    warn ("FAILED (expected $testparams{expect}, got $code $text)\n");
	} else {
	    warn ("FAILED ('$code $text' does not match /$testparams{expect}/)\n");
	}
	return 0;
    }

    if (int ($testparams{expect}) > 0) {
	warn ("OK (got $code $text)\n");
    } else {
	warn ("OK (match /$testparams{expect}/)\n");
    }

    close ($Socket);
    return 1;
}

sub regexp_match
{
    my $a = shift;
    my $b = shift;

    return ($a =~ /$b/);
}

sub send_ack
{
    my $response = shift;
    my $my_via = shift;
    my $sipuri = shift;
    my $Socket = shift;
    my $is_udp = shift;
    my $testname = shift;
    my $testheader = shift;
    my $dst = shift;
    my $quiet = shift;
    my $MaxForwards;

    my $cseq = fetch_header("CSeq", $response);
    $cseq =~ s/INVITE/ACK/g;

    my $extra_headers = "";
    if ($testheader) {
	$extra_headers = "$testheader\n";
    }

    my $ack = join ("\n", "ACK $sipuri SIP/2.0",
		    "Via: $my_via",
		    "From: " . fetch_header("From", $response),
		    "To: " . fetch_header("To", $response),
		    "Call-Id: " . fetch_header("Call-Id", $response),
		    "CSeq: $cseq",
		    "Content-Length: 0",
		    "Max-Forwards: $MaxForwards",
		    "User-Agent: testclient.pl",
		    $extra_headers);

    send_message($Socket, $is_udp, $testname, $ack, "", $dst, $quiet);
}

sub send_message
{
    my $Socket = shift;
    my $is_udp = shift;
    my $testname = shift;
    my $msg_in = shift;
    my $body_in = shift;
    my $dst_in = shift;
    my $quiet = shift;

    my $proto = $is_udp?"UDP":"TCP";

    my ($sendto, $sendto_ip, $sendto_port) = resolve_destination ($dst_in);

    my $msg = join("\r\n", split("\n", $msg_in)) . "\r\n\r\n" . $body_in;

    if ($is_udp) {
	my $dst = sockaddr_in($sendto_port, inet_aton($sendto_ip));
	send($Socket, $msg, 0, $dst) == length($msg) or warn ("FAILED, Can't send to $sendto ($sendto_ip) port $sendto_port ($proto) : $!"), return undef;
    } else {
	send($Socket, $msg, 0) == length($msg) or warn ("FAILED, Can't send to $sendto ($sendto_ip) port $sendto_port ($proto) : $!"), return undef;
    }

    warn ("  *** SENT to $sendto ($sendto_ip) port $sendto_port ($proto) :\n\t" . join ("\n\t", split ("\n", $msg)) . "\n\n") unless ($quiet);

    return 1;
}

sub read_response
{
    my $Socket = shift;
    my $is_udp = shift;
    my $testname = shift;
    my $CallID = shift;
    my $quiet = shift;

    my $timeout = 5;

    my $proto = $is_udp?"UDP":"TCP";

    while (1) {
	if ($is_udp) {
	    my $rin = '';
	    vec ($rin, fileno ($Socket), 1) = 1;

	    if (select (my $rout = $rin, undef, undef, $timeout)) {
		my $msg;
		(my $hispaddr = recv ($Socket, $msg, 8192, 0))        || die "recv: $!";
		my ($port, $hisiaddr) = sockaddr_in ($hispaddr);
		my $host = gethostbyaddr ($hisiaddr, AF_INET);
		my $ip = inet_ntoa ($hisiaddr);

		warn ("  *** RECV from $host ($ip) port $port (UDP) :\n\t" . join ("\n\t", split ("\n", $msg)) . "\n\n") unless ($quiet);

		if (callid_match ($CallID, $msg)) {
		    my ($code, $text) = get_response ($msg);
		    if (is_provisional_response ($msg)) {
			warn ("	RECV: $code $text\n") if ($quiet);
			# extend timeout since we got a provisional response
			my $timeout = 60;
		    } else {
			warn ("	RECV: $code $text\n") if ($quiet);
			return $msg;
		    }
		} else {
		    warn ("Received response with wrong Call-ID (not $CallID)\n");
		}
	    } else {
		warn ("FAILED, timeout waiting for response\n");
		return 0;
	    }
	} else {
	    my @headers;
	  READMSG: while (my $rad = <$Socket>) {
	      if ($rad eq "\r\n") {
		  next unless @headers;  # ignore extra crlf between messages
		  push(@headers, $rad);
		  my $clen = int (fetch_header("Content-Length", join("", @headers)));

		  while ($clen > 0) {
		      # XXX do timeout
		      $rad = <$Socket>;
		      push (@headers, $rad);
		      $clen -= length($rad);
		  }

		  my $msg = join("", @headers);


		  if ($quiet) {
		      my ($code, $text) = get_response ($msg);
		      warn (" RECV: $code $text\n");
		  } else {
		      #warn ("  *** RECV from $host ($ip) port $port (TCP) :\n " . join ("\n   ", split ("\n", $msg)) . "\n\n");
		      warn ("  *** RECV (TCP) :\n\t" . join ("\n\t", split ("\n", $msg)) . "\n\n");
		  }

		  if (is_provisional_response ($msg)) {
		      # extend timeout since we got a provisional response
		      my $timeout = 60;
		      @headers = undef;
		      redo READMSG;
		  } else {
		      return $msg;
		  }
	      }
	      push(@headers, $rad);
	  }
	    warn ("Did not receive complete response : " . join("", @headers) . "\n");
	    return undef;
	}
    }

    return 1;
}

sub resolve_destination
{
    my $hostname = shift;
    my $port = "5060";

    if ($hostname =~ /^(.+):(\d+)$/) {
	$port = $2;
	$hostname = $1;
    }

    my $ip = gethostbyname ($hostname);

    die ("Can't resolve '$hostname'") unless ($ip);

    return ($hostname, inet_ntoa($ip), $port);
}

sub is_provisional_response
{
    my $msg = shift;

    my ($code, $text) = get_response ($msg);

    return ($code >= 100 and $code <= 199);
}

sub get_response
{
    my $in = (split ("\n", shift))[0];

    if ($in =~ /^SIP\/2\.0 (\d+)\s(.+?)[\r\n]*$/) {
	return (int ($1), $2);
    }

    return (0, '');
}

sub callid_match
{
    my $CallID = shift;
    my $msg = shift;

    return (fetch_header ('Call-ID', $msg) eq $CallID);
}

sub fetch_first_header
{
    my $name = shift;
    my $headers = shift;

    my @m = (grep /^$name:\s*.+/i, split ("\n", $headers));

    my $rad = $m[0];

    if ($rad =~ /^$name:\s*(.+?)[\r\n]*$/i) {
	return $1;
    }

    return undef;
}

sub fetch_header
{
    my $name = shift;
    my $headers = shift;

    my @m = (grep /^$name:\s*.+/i, split ("\n", $headers));

    return undef if (1 != @m);

    my $rad = $m[0];

    if ($rad =~ /^$name:\s*(.+?)[\r\n]*$/i) {
	return $1;
    }

    return undef;
}

sub check_method
{
    my $in = uc (shift);

    return undef if ($in !~ /^[A-Z]+$/);

    return $in;
}

sub parse_sipurl
{
    my $in = shift;

    return $1 if ($in =~ /<(sip:[a-zA-Z0-9\._-]+\@[a-zA-Z0-9\.:_-]+)>/);
    return $1 if ($in =~ /<(foo:[a-zA-Z0-9\._-]+\@[a-zA-Z0-9\.:_-]+)>/);

    return $in if ($in =~ /^sip:[a-zA-Z0-9\._-]+\@[a-zA-Z0-9\.:_-]+$/);
    return $in if ($in =~ /^foo:[a-zA-Z0-9\._-]+\@[a-zA-Z0-9\.:_-]+$/);

    warn ("parse_sipurl: '$in' did not parse\n");

    return undef;
}

sub make_url
{
    my $in = shift;

    return $in if ($in =~ /^sip:[a-zA-Z0-9\._-]+\@[a-zA-Z0-9\.:_-]+$/);
    return $in if ($in =~ /^foo:[a-zA-Z0-9\._-]+\@[a-zA-Z0-9\.:_-]+$/);

    return "sip:$in" if ($in =~ /^[a-zA-Z0-9\._-]+\@[a-zA-Z0-9\.:_-]+$/);

    return undef;
}

sub auth_parse
{
    # stolen from HTTPD-Tools (v. 0.55)
    my($string) = shift;
    $string =~ s/^Digest\s+//;
    $string =~ s/"//g; #"
    my(@pairs) = split(/,?\s+/, $string);
    my(%pairs) = map { split(/=/) } @pairs;
    #print STDERR "Digest::parse -> @pairs{qw(username realm response)}\n" if $Debug;
    return \%pairs;
}

sub base64_decode
{
    my $in = shift;
    # stolen from http://www.rocketaware.com/perl/perlfaq9/How_do_I_decode_a_MIME_BASE64_st.htm
    $in =~ tr#A-Za-z0-9+/##cd;			# remove non-base64 chars
    $in =~ tr#A-Za-z0-9+/# -_#;			# convert to uuencoded format
    my $len = pack ("c", 32 + 0.75 * length ($in));	# compute length byte
    return unpack ("u", $len . $in);		# uudecode and print
}

sub read_local_config
{
    my @f_locs = ("$ENV{HOME}/testclient.conf", "/etc/testclient.conf", "/usr/local/etc/testclient.conf", "./testclient.conf");
    my $cfg_loaded = 0;

    foreach my $fn (@f_locs) {
	if (-r $fn) {
	    warn ("$0: Loading local configuration from '$fn'\n") unless ($quiet);

	    open (CF, "< $fn") or die ("$0: Could not open file '$0' for reading : $!\n");
	    my @cf = <CF>;
	    eval ("@cf");
	    die ("$0: Error in configuration file '$fn': $@\n") if ($@ ne '');

	    $cfg_loaded = 1;
	    last;
	}
    }

    if (! $cfg_loaded) {
	warn ("$0: No local configuration found, looked for it in the following locations :\n\t",
	      join ("\n\t", @f_locs), "\n") unless $quiet;
    }
}
