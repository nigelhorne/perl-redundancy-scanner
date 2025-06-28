#!/usr/bin/env perl
use strict;
use warnings;
use Test::More;

# 1. run the scanner
my $cmd    = './scanner.pl t/data/within.pl 2>&1';
my $output = `$cmd`;

# 2. sanity: scanner actually ran on the right file
like(
    $output,
    qr/Scanning t\/data\/within\.pl/,
    'scanner invoked on within.pl'
);

# 3. regression: ensure no false "redundant in AND" warning
unlike(
    $output,
    qr/redundant in AND/,
    'no false AND-redundancy on $number > 1900 && $number <= 2000'
);

done_testing;
