#!/usr/bin/env perl
#
# codestats.pl makes statistics of numer of codes (in Yxa) that can be fed to gnuplot
#
# Author : Fredrik Thulin <ft@it.su.se>
#

use strict;
use Time::ParseDate;
use Getopt::Std;
use vars qw ($opt_h $opt_F $opt_T $opt_v);

getopts ('hF:T:v');

my $repo = shift;
my $tempdir = shift;

if (defined ($opt_h) || ! $repo || ! $tempdir) {
    die (<<EOT);
Syntax: $0 [options] repo-path temp-dir

  Options:
    -F date	from date, what range to calculate statistics for
    -T date	to date, what range to calculate statistics for
    -v		verbose

EOT
}

my %stats;

if ($repo !~ /^svn/) {
    die ("$0: Unknown repo syntax '$repo'\n");
}

if (! -d $tempdir) {
    die ("$0: '$tempdir' is not a directory\n");
}

my ($f_date, $t_date);
$f_date = check_date ($opt_F);
$t_date = check_date ($opt_T);

my $from = parsedate ($f_date);
my $to = parsedate ($t_date);
my $interval = 86400;
if ($to < $from) {
    $interval = -86400;
}
show_stats ("# Date");
print ("#\n");

process_interval ($from, $interval, $to, defined ($opt_v), $repo, $tempdir);

exit (0);


sub check_date
{
    my $in = shift;
    if (! defined ($in)) {
	return "now";
    } elsif ($in =~ /^(\d{4})-(\d{1,2})-(\d{1,2})$/o) {
	return ("$1/$2/$3");
    } else {
	die ("$0: Invalid date '$in' - use YYYY-MM-DD format\n");
    }
}

sub process_interval
{
    my $from = shift;
    my $interval = shift;
    my $to = shift;
    my $verbose = shift;
    my $repo = shift;
    my $tempdir = shift;

    if ($interval > 0) {
	# start time should be before endtime
	if ($from > $to) {
	    die ("$0: process_interval: Start time ($from) not before end time ($to) ".
		 "with positive interval ($interval)\n");
	}
    } elsif ($interval < 0) {
	if ($from < $to) {
	    die ("$0: process_interval: Start time ($from) after end time ($to) ".
		 "with negative interval ($interval)\n");
	}
    } else {
	die ("$0: process_cvs: We will never finish with inverval 0\n");
    }

    my $finished = 0;
    my $current_time = $from;
    while (! $finished) {
	my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime ($current_time);
	$year += 1900;
	$mon++;

	my $this_day = sprintf ("%.4i-%.2i-%.2i", $year, $mon, $mday);
	#print ("$this_day\n");

	my %this_stats;
	process_day ($this_day, \%this_stats, $verbose, $repo, $tempdir);
	show_stats("total", $this_day, \%this_stats);

	$current_time += $interval;
	if ($interval < 0) {
	    # stepping down towards $starttime
	    $finished = ($current_time <= $to);
	} else {
	    # going up to $endtime
	    $finished = ($current_time >= $to);
	}
    }

    return 1;
}

sub process_day
{
    my $date = shift;
    my $stats_ref = shift;
    my $verbose = shift;

    my $daydir = "$tempdir/codestats.$date";

    if (-d $daydir) {
	die ("$0: Directory '$daydir' already exists!\n");
    }

    my @cmd = ("svn", "export", "-q", "-r", "{$date}", $repo, $daydir);

    my $rc = 0xffff & system (@cmd);
    if ($rc == 0) {
	process_directory ($daydir, $stats_ref, $verbose);
    } else {
        die ("$0: Failed executing '", join (' ', @cmd), "'\n");
    }

    return 1;
}

sub process_directory
{
    my $dir = shift;
    my $stats_ref = shift;
    my $verbose = shift;

    opendir (DIR, "$dir") or die ("$0: Could not open directory '$dir' : $!\n");

    warn ("Processing directory '$dir'\n") if ($verbose);

    my @dirents = readdir (DIR);
    closedir (DIR);

    foreach my $f (@dirents) {
	next if ($f eq '.' or $f eq '..');
	if (-d "$dir/$f") {
	    warn ("Recursing into '$f'\n") if ($verbose);
	    process_directory ("$dir/$f", $stats_ref, $verbose);
	    next;
	}
	if (! file_is_interesting ($f)) {
	    warn ("Skipping '$f'\n") if ($verbose);
	    next;
	}

	warn ("Processing file '$f'\n") if ($verbose);

	open (FILE, "< $dir/$f") or die ("$0: Could not open file '$dir/$f' for reading : $!\n");
	my @l = <FILE>;
	close (FILE);

	do_stats ($f, $stats_ref, \@l);
	show_stats ($f, $f, $stats_ref) if ($verbose);
    }

    closedir (DIR);
}

sub show_stats
{
    my $key = shift;
    my $label = shift;
    my $stats_ref = shift;

    my ($total_lines, $code, $comment, $test_code, $num_tests);

    if (defined ($stats_ref) and defined ($key) and defined ($label) and $key !~ /^\#/o) {
	$code = $$stats_ref{$key}{"code"} || 0;
	$comment = $$stats_ref{$key}{"comment"} || 0;
	$test_code = $$stats_ref{$key}{"test"} || 0;
	$num_tests = $$stats_ref{$key}{"num_tests"} || 0;

	$total_lines = $code + $comment + $test_code;
    } else {
	if ($key =~ /^\#/o) {
	    $label = $key;
	} else {
	    $label       = "File";
	}
	$total_lines = "Total";
	$code        = "Code";
	$comment     = "Comment";
	$test_code   = "Test code";
	$num_tests   = "Tests";
    }

    printf ("%-45s %-10s %-10s %-15s %-10s %-10s\n",
	    $label, $code, $comment, $test_code, $num_tests, $total_lines);

    return 1;
}

sub do_stats
{
    my $file = shift;
    my $stats_ref = shift;
    my $data_ref = shift;

    my $is_test = 0;
    my $rest_of_file_is_test = 0;

    foreach my $l (@{$data_ref}) {
	my $is_comment = 0;
	chomp ($l);
	
	next if ($l =~ /^\s*$/);	# skip empty lines or lines with just spaces

	if ($l =~ /^\s*%/o) {
	    # optional whitespace, then % - comment
	    $is_comment = 1;
	}

	if (! $rest_of_file_is_test) {
	    if ($l =~ /^test\d*\s*\(\s*\)\s+->\s*/o) {
		# start of test function (function named test\d)
		$is_test = 1;
	    } elsif ($is_test and $l =~ /^\s*ok\.\s*$/) {
		# end of test function
		$is_test = 0;
	    }
	} else {
	    if ($l =~ /^\s*%%\s*Test functions\s*$/) {
		# rest of file is test
		$rest_of_file_is_test = 1;
		$is_test = 1;
	    }
	}

	if ($is_test) {
	    # count everything in test functions as test, don't
	    # differ between comment and code
	    $$stats_ref{$file}{"test"}++;
	    $$stats_ref{"total"}{"test"}++;

	    if ($l =~ /^\s*io\:format\s*\(/o ||
		$l =~ /^\s*autotest\:mark\s*\(/o) {
		# autotest:mark() when $is_test, also increase num_tests counter
		# io:format() when $is_test, also increase num_tests counter (old test code did this)
		$$stats_ref{$file}{"num_tests"}++;
		$$stats_ref{"total"}{"num_tests"}++;
	    }
	} else {
	    if ($is_comment) {
		$$stats_ref{$file}{"comment"}++;
		$$stats_ref{"total"}{"comment"}++;
	    } else {
		# count as code line if we get this far
		$$stats_ref{$file}{"code"}++;
		$$stats_ref{"total"}{"code"}++;
	    }
	}
    }

    return 1;
}

sub file_is_interesting
{
    my $in = shift;

    # skip imported sources
    return 0 if ($in =~ /LDAPv3\.(erl|hrl)$/o);
    return 0 if ($in =~ /eldap\.(erl|hrl)$/o);
    return 0 if ($in =~ /inet_dns\.hrl$/o);

    # only care about *.erl and *.hrl
    return 0 if ($in !~ /\.(erl|hrl)$/o);

    return 1;
}
