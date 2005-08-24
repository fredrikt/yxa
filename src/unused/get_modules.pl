#!/usr/bin/perl -w

use strict;

my %modules;

while (<>) {
    $modules{$1} = 1 if /([a-z_]+):[a-z_]+/;
}

for my $i (keys %modules) {
    unless (-f "$i.erl") {
	print "$i\n";
    }
}
