#!/usr/bin/env perl
use strict;
use warnings;

my $month = int(rand(12));

if (
   ($month == 0)
|| ($month == 2)
|| ($month == 4)
|| ($month == 6)
|| ($month == 7)
|| ($month == 9)
|| ($month == 11)
) {
    print "Long month!\n";
}

1;
