#!/pkg/perl/5.8.6/bin/perl -w
#
# $Id$
#

use strict;
use Getopt::Std;
use JEvent;
use XML::Simple;
use Data::Dumper;
use POSIX;

use vars qw ($opt_c $opt_d $opt_h);

getopts('c:dh');

sub usage
{
    my $msg = shift;
    die (<<EOU);
${msg}Usage: $0 [-h] -c agent.ini

$0 Sample JEvent agent

SYNOPSIS

\t-c\tINI-style configuration file for this agent.
\t-h\tShow this text
\t-d\tEnable debugging

EOU
}

usage () if ($opt_h);

my $ini_fn = $opt_c || '/local/yxa/config/yxa-su_bot-jevent.ini';

usage ("Could not find config file '$ini_fn'\n\n") if (! -f $ini_fn);

my $debug = defined ($opt_d);
my %monitor;
$monitor{"ft\@su.se"}{"chat"} = 1;

my %ongoing_requests;
my $current_id = 0;


make_non_blocking (*STDIN);

my $ini = Config::IniFiles->new(-file => $ini_fn)
    or die "Unable to read configuration from $ini_fn: $!\n";

my $host = Sys::Hostname::hostname();
my $je = JEvent->new(Config => $ini,

		     Description => "Hello, I am the YXA JEvent bot running on $host.",

		     CommandInfo => {
			 ping		=> [ 'ping: check if I am really there' ],
			 monitor	=> [ 'monitor <on or off>: enable/disable monitoring' ],
			 refer		=> [ 'refer referer referee refer_to: yes, refer someone' ],
			 locations	=> [ 'locations SIPuser: get a list of current locations for a SIP user' ],
			 quit		=> [ 'quit: cause agent to terminate' ]
		     },

		     Commands => {

			 ping => sub {
			     my ($self, $from, $type, $cmd, @args) = @_;
			     
			     my $id = start_request ("$from/$type");

			     send_to_port("PING", $id, undef);
 
			     return ("Sent ping to YXA-node...");
			 },

			 monitor => sub {
			     my ($self, $from, $type, $cmd, @args) = @_;
			     if (defined ($args[0])) {
				 if (uc ($args[0]) eq 'ON') {
				     notify_monitors ($self, "monitor cmd (from $from/$type): enabled", \%monitor);
				     $monitor{$from}{$type} = 1;
				 } elsif (uc ($args[0]) eq 'OFF') {
				     notify_monitors ($self, "monitor cmd (from $from/$type): disabled", \%monitor);
				     delete ($monitor{$from}{$type});
				 } else {
				     return ("ERROR: Invalid argument");
				 }
			     }
			     
			     my $is = defined ($monitor{$from}{$type})?'ON':'OFF';
			     return ("OK: Your ($from/$type) monitor status is: $is");
			 },

			 refer => sub {
			     my ($self, $from, $type, $cmd, @args) = @_;
			     if ($#args == 2) {
				 my %h;
				 $h{'referer'} = shift (@args);
				 $h{'referee'} = shift (@args);
				 $h{'refer_to'} = shift (@args);

				 refer (\%h, "$from/$type", \%monitor);

				 return ("Passed REFER-REQUEST to YXA\n");
			     }
			 },

			 locations => sub {
			     my ($self, $from, $type, $cmd, @args) = @_;
			     if (scalar @args == 1) {
				 my $user = shift (@args);

				 locations ($user, "$from/$type", \%monitor);

				 return ("Passed LOCATIONS request to YXA\n");
			     }
			 },

			 quit => sub {
			     my ($self, $from, $type, $cmd, @args) = @_;

			     notify_monitors ($self, "Quitting (on behalf of $from)", \%monitor);
			     exit (0);
			 }
		     },

		     EventCB => sub {
			 my ($self, $sid, $msg) = @_;
			 my $items = $msg->GetItems ();
			 
			 return unless $items;
			 
			 foreach my $item ($items->GetItems()) {
			     # LEIF: dekoda prylar här
			     my $event = XMLin($item->GetContent(), ForceArray => 1);
			     
			     my $type = $event->{'type'};
			     if ($type eq 'refer') {
				 next unless ref $event->{'items'} eq 'ARRAY';
				 foreach my $t (@{$event->{'items'}}) {
				     refer ($t, 'event', \%monitor);
				 }
			     }
			 }
		     },

		     ProcessCB => sub {
			 my $self = shift;
			 my $buf = "";

			 my $nread;
			 do {
			     $nread = sysread (STDIN, $buf, 2);

			     if (defined ($nread) and $nread == 2) {
				 my $len = unpack ("n", $buf);
				 
				 make_blocking (*STDIN);
				 
				 my $nread2 = sysread (STDIN, $buf, $len);
				 
				 if ($nread2 == $len) {
				     process_response ($self, $buf, \%monitor);
				 } 
				 make_non_blocking (*STDIN);
			     } elsif (defined ($nread) and $nread == 0) {
				 notify_monitors ($self, "Exiting since YXA-node disappeared", \%monitor);
				 exit (1);
			     }
			 } while (defined ($nread));
		     }
		     
		     );
$je->Run();

sub refer
{
    my $items_ref = shift;
    my $source = shift;
    my $monitor_ref = shift;
    
    print ("REFER-REQUEST : " . Dumper($items_ref) . "\n") if ($debug);
    
    my $referer = $$items_ref{'referer'};
    my $referee = $$items_ref{'referee'};
    my $refer_to = $$items_ref{'refer_to'};

    print ("REFER-REQUEST '$referee' TO '$refer_to' ON BEHALF OF '$referer'\n") if ($debug);

    notify_monitors ($je, "REFER-REQUEST (from $source) : refer '$referee' to '$refer_to' on behalf of '$referer'", $monitor_ref);

    my $id = start_request ($source);
    send_to_port("REFER-REQUEST", $id, "referee=\"$referee\", refer_to=\"$refer_to\", referer=\"$referer\"");
}

sub locations
{
    my $user = shift;
    my $source = shift;
    my $monitor_ref = shift;
    
    print ("LOCATIONS : $user\n") if ($debug);
    
    notify_monitors ($je, "LOCATIONS (from $source) : get current locations for '$user'", $monitor_ref);

    my $id = start_request ($source);
    send_to_port("LOCATIONS", $id, "user=\"$user\"");
}

# handle commands read from the YXA node
sub process_response
{
    my $je = shift;
    my $in = shift;
    my $monitor_ref = shift;

    if ($in =~ /^\s*(\S+)\s*(\S*)\s*:\s*(\S+)\s*:\s*([\S\s]*)\s*$/) {
	my $request = $1;
	my $tag = $2;
	my $id = $3;
	my $data = $4;

	if ($id and $ongoing_requests{$id}) {
	    my $send_response_to = $ongoing_requests{$id};
	    if ($tag ne 'continued') {
		$ongoing_requests{$id} = undef ();
	    }

	    my ($to, $type) = split ('/', $send_response_to);

	    $je->Client->MessageSend(to   => $to,
				     type => $type,
				     body => "YXA: $request : $data"
				     );
	} elsif ($id eq 'event') {
	    # LEIF: PRODUCERA EVENTS HÄR
	    warn ("Not implemented");
	} else {
	    notify_monitors ($je, "ERROR: Message from YXA-node with bad ID ($id) : $in", $monitor_ref);
	}
    } else {
	notify_monitors ($je, "ERROR: Undecodable message from YXA-node : $in", $monitor_ref);
    }

    return 1;
}

sub send_to_port
{
    my $request = shift;
    my $id = shift;
    my $data = shift;

    my $msg;
    if (defined ($data)) {
	$msg = "$request : $id : $data";
    } else {
	$msg = "$request : $id :";
    }

    my $header = pack ("n", length($msg));
    syswrite (STDOUT, $header . $msg);
}

sub notify_monitors
{
    my $je = shift;
    my $msg = shift;
    my $monitor_ref = shift;

    foreach my $to (keys %{$monitor_ref}) {
	foreach my $type (keys %{$$monitor_ref{$to}}) {
	    $je->Client->MessageSend(to   => $to,
				     type => $type,
				     body => $msg
				     );
	}
    }
}

sub make_non_blocking
{
    my $fd = shift;
    my $t = fcntl ($fd, F_GETFL, 0) || die ("$0: Failed fetching flags for file descriptor : $!\n");
    fcntl ($fd, F_SETFL, $t | O_NONBLOCK) || die ("$0: Failed changing flags for file descriptor : $!\n");
    return 1;
}

sub make_blocking
{
    my $fd = shift;
    my $t = fcntl ($fd, F_GETFL, 0) || die ("$0: Failed fetching flags for file descriptor : $!\n");
    fcntl ($fd, F_SETFL, $t & O_NONBLOCK) || die ("$0: Failed changing flags for file descriptor : $!\n");
    return 1;
}

sub start_request
{
    my $source = shift;

    $current_id++;

    $ongoing_requests{$current_id} = $source;

    return ($current_id);
}
